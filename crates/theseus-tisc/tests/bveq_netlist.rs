//! BVEQ slice-2b: symbolic netlist walk tests.
//!
//! Proves `netlist(instr) ≡ instr_semantics(instr)` at full Flat(8)
//! for all 13 register-pure instructions via BDD walk of the decoder's
//! per-instruction netlist.
//!
//! Test tiers:
//! - `bveq_netlist_walk_reconcile` (DEFAULT, ~5-15s): one-arm, 13 ops,
//!   2-3 selector/imm configs each. THE UN-IGNORE WIN.
//! - `bveq_netlist_walk_system` (#[ignore], ~46s): full system_step,
//!   13 ops, 1 config each — the whole-step authority (discharges T3).
//! - `bveq_netlist_walk_full_sweep` (#[ignore], ~19min): one-arm,
//!   EVERY selector pair/triple + every immediate.
//! - `bveq_netlist_walk_bootstrap` (#[ignore], ~30-40s): concrete
//!   walk-vs-`run_rom_on_cpu` (T2 real-machine anchor).
//! - Convention test (T1): pins bits_of/Left=1 leaf-order convention.
//! - Negative controls: non-vacuous mismatch assertions.

// Bit-indexed loops read clearer as range loops in these BDD-per-bit tests.
#![allow(clippy::needless_range_loop)]

use theseus_tisc::asm::{self, Instr};
use theseus_tisc::bveq::{biodivine_lib_bdd, expr_to_bdd, instr_semantics, BITS};
use theseus_tisc::bveq_netlist::{
    self, build_concrete_system_sv, build_cpu_init, build_system_init, eval_bdd,
    extended_varmap, extract_field, extract_word8_bdds, needs_s0_pinned, sv_flatten,
    sv_to_u8, sv_word8_symbolic, WalkCtx, OP_BR, OP_MEM_BASE, OP_S0,
};
use theseus_tisc::decoder::{self, Op};
use theseus_tisc::step;
use theseus_tisc::types::{MEM_WORDS, NUM_BUS_FIELDS, NUM_CPU_FIELDS, NUM_REGS, W};

// ── Helpers ────────────────────────────────────────────────────────

/// Run a closure in a thread with a 1 GiB stack (MINOR-5: Iso trees +
/// adjoint() recursion overflow the 2 MiB cargo-test worker stack).
fn big_stack<F, R>(f: F) -> R
where
    F: FnOnce() -> R + Send + 'static,
    R: Send + 'static,
{
    let builder = std::thread::Builder::new().stack_size(1024 * 1024 * 1024);
    let handle = builder.spawn(f).expect("spawn big-stack thread");
    handle.join().expect("big-stack thread panicked")
}

/// Map Instr to decoder Op.
fn instr_to_op(instr: &Instr) -> Op {
    match instr {
        Instr::Add { .. } => Op::Add,
        Instr::Sub { .. } => Op::Sub,
        Instr::Xor { .. } => Op::Xor,
        Instr::Exch { .. } => Op::Exch,
        Instr::Rotl { .. } => Op::Rotl,
        Instr::Rotr { .. } => Op::Rotr,
        Instr::Cswap { .. } => Op::Cswap,
        Instr::Ccnot { .. } => Op::Ccnot,
        Instr::Addi { .. } => Op::Addi,
        Instr::Subi { .. } => Op::Subi,
        Instr::Xori { .. } => Op::Xori,
        Instr::Rotli { .. } => Op::Rotli,
        Instr::Rotri { .. } => Op::Rotri,
        _ => panic!("not a register-pure instruction: {instr:?}"),
    }
}

/// The 13 register-pure instructions used across all tiers.
fn register_pure_ops() -> Vec<Instr> {
    vec![
        Instr::Add { rd: 0, rs: 1 },
        Instr::Sub { rd: 0, rs: 1 },
        Instr::Xor { rd: 0, rs: 1 },
        Instr::Exch { rd: 0, rs: 1 },
        Instr::Rotl { rd: 0, rs: 1 },
        Instr::Rotr { rd: 0, rs: 1 },
        Instr::Cswap { rd: 0, rs: 1, rt: 2 },
        Instr::Ccnot { rd: 0, rs: 1, rt: 2 },
        Instr::Addi { rd: 0, imm: 42 },
        Instr::Subi { rd: 0, imm: 100 },
        Instr::Xori { rd: 0, imm: 0xFF },
        Instr::Rotli { rd: 0, imm: 3 },
        Instr::Rotri { rd: 0, imm: 5 },
    ]
}

/// Assert that all 8 bits of a Word8 SV are constant-false (value 0).
fn assert_word8_zero(sv: &bveq_netlist::SV, vs: &biodivine_lib_bdd::BddVariableSet, ctx: &str) {
    let bits = sv_flatten(sv);
    assert_eq!(bits.len(), W, "{ctx}: expected {W} bits");
    let f = vs.mk_false();
    for (i, b) in bits.iter().enumerate() {
        assert_eq!(*b, f, "{ctx}: bit {i} should be 0");
    }
}

/// Assert that a Word8 SV has the given constant value.
fn assert_word8_const(
    sv: &bveq_netlist::SV,
    expected: u8,
    vs: &biodivine_lib_bdd::BddVariableSet,
    ctx: &str,
) {
    let bits = sv_flatten(sv);
    assert_eq!(bits.len(), W, "{ctx}: expected {W} bits");
    let t = vs.mk_true();
    let f = vs.mk_false();
    for i in 0..W {
        let expected_bit = if (expected >> i) & 1 == 1 { &t } else { &f };
        assert_eq!(bits[i], *expected_bit, "{ctx}: bit {i} mismatch");
    }
}

/// Assert that a Word8 SV's bits match the entry-value BDDs (identity check).
fn assert_word8_identity(
    sv: &bveq_netlist::SV,
    entry: &bveq_netlist::SV,
    ctx: &str,
) {
    let out_bits = sv_flatten(sv);
    let entry_bits = sv_flatten(entry);
    assert_eq!(out_bits.len(), W, "{ctx}: expected {W} bits");
    assert_eq!(entry_bits.len(), W, "{ctx}: entry expected {W} bits");
    for i in 0..W {
        assert_eq!(out_bits[i], entry_bits[i], "{ctx}: bit {i} not restored to entry");
    }
}

/// Run a one-arm walk: instruction(op) on cpu_init, compare regs
/// against instr_semantics, assert full frame.
fn walk_one_arm_and_check(instr: Instr, label: &str) {
    let map = extended_varmap();
    let vs = map.bdd_vars();
    let word = asm::encode(instr).unwrap_or_else(|e| panic!("{label}: encode: {e}"));
    let pin_s0 = needs_s0_pinned(&instr);
    let init_sv = build_cpu_init(&map, word, pin_s0);
    let op = instr_to_op(&instr);
    let iso = decoder::instruction(op);

    let mut ctx = WalkCtx::new();
    let out_sv = eval_bdd(&iso, init_sv, vs, &mut ctx)
        .unwrap_or_else(|e| panic!("{label}: eval_bdd: {e}"));

    // Compare registers against instr_semantics.
    let semantics = instr_semantics(&instr)
        .unwrap_or_else(|e| panic!("{label}: instr_semantics: {e}"));
    for reg in 0..NUM_REGS {
        let out_field = extract_field(&out_sv, reg, NUM_CPU_FIELDS);
        let out_bdds = extract_word8_bdds(out_field);
        for bit in 0..BITS {
            let expected = expr_to_bdd(&semantics.regs[reg][bit], &map)
                .unwrap_or_else(|e| panic!("{label}: expr_to_bdd r{reg}[{bit}]: {e}"));
            assert_eq!(
                out_bdds[bit], expected,
                "{label}: r{reg}[{bit}] mismatch"
            );
        }
    }

    // PC = 0 (one-arm doesn't touch PC).
    assert_word8_zero(
        extract_field(&out_sv, 8, NUM_CPU_FIELDS),
        vs, &format!("{label}: PC"),
    );

    // BR restored to symbolic entry.
    let br_entry = sv_word8_symbolic(&map, OP_BR);
    assert_word8_identity(
        extract_field(&out_sv, 9, NUM_CPU_FIELDS),
        &br_entry, &format!("{label}: BR"),
    );

    // IR unchanged (concrete word).
    assert_word8_const(
        extract_field(&out_sv, 10, NUM_CPU_FIELDS),
        word as u8, vs, &format!("{label}: IR_lo"),
    );
    assert_word8_const(
        extract_field(&out_sv, 11, NUM_CPU_FIELDS),
        (word >> 8) as u8, vs, &format!("{label}: IR_hi"),
    );

    // s0 restored to entry (symbolic or 0).
    if pin_s0 {
        assert_word8_zero(
            extract_field(&out_sv, 12, NUM_CPU_FIELDS),
            vs, &format!("{label}: s0"),
        );
    } else {
        let s0_entry = sv_word8_symbolic(&map, OP_S0);
        assert_word8_identity(
            extract_field(&out_sv, 12, NUM_CPU_FIELDS),
            &s0_entry, &format!("{label}: s0"),
        );
    }

    // s1-s7 = 0 (ancilla clean, T5).
    for i in 1..8 {
        assert_word8_zero(
            extract_field(&out_sv, 12 + i, NUM_CPU_FIELDS),
            vs, &format!("{label}: s{i}"),
        );
    }
}

// ── T1: convention pin ─────────────────────────────────────────────

#[test]
fn bveq_netlist_convention_left_is_1() {
    use theseus_core::{Type, Value};
    let bit_ty = Type::sum(Type::One, Type::One);
    // Left(Unit) = true = 1
    let bits_left = theseus_netlist::bits_of(&Value::left(Value::Unit), &bit_ty).unwrap();
    assert_eq!(bits_left, vec![true], "Left(Unit) must encode as [true] (= 1)");
    // Right(Unit) = false = 0
    let bits_right = theseus_netlist::bits_of(&Value::right(Value::Unit), &bit_ty).unwrap();
    assert_eq!(bits_right, vec![false], "Right(Unit) must encode as [false] (= 0)");
}

// ── Tier 1: bveq_netlist_walk_reconcile (DEFAULT, ~5-15s) ──────────

#[test]
fn bveq_netlist_walk_reconcile() {
    // One-arm walk, 13 ops, 2-3 selector/imm configs each.
    // Covers: rs=0, high rt, carry op, rotate, both 3-ops.
    let configs: Vec<(Instr, &str)> = vec![
        // 2-op: carry + standard configs
        (Instr::Add { rd: 0, rs: 1 }, "Add(0,1)"),
        (Instr::Add { rd: 1, rs: 0 }, "Add(1,0)-rs=0"),
        (Instr::Add { rd: 3, rs: 5 }, "Add(3,5)"),
        (Instr::Sub { rd: 0, rs: 1 }, "Sub(0,1)"),
        (Instr::Sub { rd: 3, rs: 5 }, "Sub(3,5)"),
        // XOR
        (Instr::Xor { rd: 0, rs: 1 }, "Xor(0,1)"),
        (Instr::Xor { rd: 6, rs: 7 }, "Xor(6,7)"),
        // Exchange
        (Instr::Exch { rd: 0, rs: 1 }, "Exch(0,1)"),
        (Instr::Exch { rd: 0, rs: 7 }, "Exch(0,7)"),
        // Rotate
        (Instr::Rotl { rd: 1, rs: 3 }, "Rotl(1,3)"),
        (Instr::Rotr { rd: 5, rs: 2 }, "Rotr(5,2)"),
        (Instr::Rotl { rd: 0, rs: 1 }, "Rotl(0,1)"),
        // 3-op: Cswap
        (Instr::Cswap { rd: 0, rs: 1, rt: 2 }, "Cswap(0,1,2)"),
        (Instr::Cswap { rd: 4, rs: 3, rt: 6 }, "Cswap(4,3,6)"),
        // 3-op: Ccnot
        (Instr::Ccnot { rd: 0, rs: 1, rt: 2 }, "Ccnot(0,1,2)"),
        (Instr::Ccnot { rd: 5, rs: 3, rt: 7 }, "Ccnot(5,3,7)"),
        // Immediate: Addi/Subi (s0 pinned)
        (Instr::Addi { rd: 0, imm: 42 }, "Addi(0,42)"),
        (Instr::Addi { rd: 3, imm: 255 }, "Addi(3,255)"),
        (Instr::Subi { rd: 2, imm: 100 }, "Subi(2,100)"),
        // Immediate: Xori (s0 symbolic)
        (Instr::Xori { rd: 5, imm: 0xFF }, "Xori(5,0xFF)"),
        (Instr::Xori { rd: 0, imm: 1 }, "Xori(0,1)"),
        // Immediate: Rotli/Rotri
        (Instr::Rotli { rd: 2, imm: 3 }, "Rotli(2,3)"),
        (Instr::Rotri { rd: 7, imm: 5 }, "Rotri(7,5)"),
    ];

    big_stack(move || {
        for (instr, label) in &configs {
            let t = std::time::Instant::now();
            walk_one_arm_and_check(*instr, label);
            eprintln!("  {label}: {:.0} ms", t.elapsed().as_secs_f64() * 1e3);
        }
    });
}

// ── Tier 2: bveq_netlist_walk_system (#[ignore], ~46s) ─────────────

#[test]
#[ignore]
fn bveq_netlist_walk_system() {
    // Full system_step([word]) walk, 13 ops, 1 config each.
    // THE WHOLE-STEP AUTHORITY — discharges T3.
    let ops = register_pure_ops();

    big_stack(move || {
        let map = extended_varmap();
        let vs = map.bdd_vars();

        for instr in &ops {
            let label = format!("{instr:?}");
            let t = std::time::Instant::now();

            let word = asm::encode(*instr).unwrap();
            let pin_s0 = needs_s0_pinned(instr);
            let init_sv = build_system_init(&map, pin_s0);
            let iso = step::system_step(&[word]);

            let mut ctx = WalkCtx::new();
            let out_sv = eval_bdd(&iso, init_sv, vs, &mut ctx)
                .unwrap_or_else(|e| panic!("{label}: eval_bdd: {e}"));

            // System SV = ((Cpu, Mem), Bus)
            let (composite_sv, bus_sv) = match &out_sv {
                bveq_netlist::SV::Pair(c, b) => (&**c, &**b),
                _ => panic!("{label}: output not Pair"),
            };
            let (cpu_sv, mem_sv) = match composite_sv {
                bveq_netlist::SV::Pair(cpu, mem) => (&**cpu, &**mem),
                _ => panic!("{label}: composite not Pair"),
            };

            // Registers: match instr_semantics.
            let semantics = instr_semantics(instr).unwrap();
            for reg in 0..NUM_REGS {
                let out_field = extract_field(cpu_sv, reg, NUM_CPU_FIELDS);
                let out_bdds = extract_word8_bdds(out_field);
                for bit in 0..BITS {
                    let expected = expr_to_bdd(&semantics.regs[reg][bit], &map).unwrap();
                    assert_eq!(
                        out_bdds[bit], expected,
                        "{label}: r{reg}[{bit}] mismatch"
                    );
                }
            }

            // PC = 1 (incremented).
            assert_word8_const(
                extract_field(cpu_sv, 8, NUM_CPU_FIELDS),
                1, vs, &format!("{label}: PC"),
            );
            // BR = 0 (restored, was 0 in init).
            assert_word8_zero(
                extract_field(cpu_sv, 9, NUM_CPU_FIELDS),
                vs, &format!("{label}: BR"),
            );
            // IR = 0 (unfetched).
            assert_word8_zero(
                extract_field(cpu_sv, 10, NUM_CPU_FIELDS),
                vs, &format!("{label}: IR_lo"),
            );
            assert_word8_zero(
                extract_field(cpu_sv, 11, NUM_CPU_FIELDS),
                vs, &format!("{label}: IR_hi"),
            );
            // s0 restored.
            if pin_s0 {
                assert_word8_zero(
                    extract_field(cpu_sv, 12, NUM_CPU_FIELDS),
                    vs, &format!("{label}: s0"),
                );
            } else {
                let s0_entry = sv_word8_symbolic(&map, OP_S0);
                assert_word8_identity(
                    extract_field(cpu_sv, 12, NUM_CPU_FIELDS),
                    &s0_entry, &format!("{label}: s0"),
                );
            }
            // s1-s7 = 0.
            for i in 1..8 {
                assert_word8_zero(
                    extract_field(cpu_sv, 12 + i, NUM_CPU_FIELDS),
                    vs, &format!("{label}: s{i}"),
                );
            }
            // Mem: symbolic entry preserved.
            for i in 0..MEM_WORDS {
                let mem_entry = sv_word8_symbolic(&map, OP_MEM_BASE + i);
                assert_word8_identity(
                    extract_field(mem_sv, i, MEM_WORDS),
                    &mem_entry, &format!("{label}: m{i}"),
                );
            }
            // Bus = 0 (quiescent).
            for i in 0..NUM_BUS_FIELDS {
                assert_word8_zero(
                    extract_field(bus_sv, i, NUM_BUS_FIELDS),
                    vs, &format!("{label}: bus[{i}]"),
                );
            }

            eprintln!("  system {label}: {:.1} s", t.elapsed().as_secs_f64());
        }
    });
}

// ── Tier 3: bveq_netlist_walk_full_sweep (#[ignore], ~19min) ───────

#[test]
#[ignore]
fn bveq_netlist_walk_full_sweep() {
    // One-arm, EVERY selector pair/triple + every immediate.
    // RETIRES bveq_reconciliation_lb_netlist_wide (2.6h).

    big_stack(move || {
        // 2-op: all (rd, rs) pairs with rd != rs, 56 combos.
        let two_ops: &[fn(u8, u8) -> Instr] = &[
            |rd, rs| Instr::Add { rd, rs },
            |rd, rs| Instr::Sub { rd, rs },
            |rd, rs| Instr::Xor { rd, rs },
            |rd, rs| Instr::Exch { rd, rs },
            |rd, rs| Instr::Rotl { rd, rs },
            |rd, rs| Instr::Rotr { rd, rs },
        ];
        for mk in two_ops {
            let sample = mk(0, 1);
            let op_name = format!("{sample:?}").split('{').next().unwrap().trim().to_string();
            let t = std::time::Instant::now();
            let mut count = 0u32;
            for rd in 0..NUM_REGS as u8 {
                for rs in 0..NUM_REGS as u8 {
                    if rd == rs { continue; }
                    let instr = mk(rd, rs);
                    walk_one_arm_and_check(instr, &format!("{op_name}({rd},{rs})"));
                    count += 1;
                }
            }
            eprintln!("  sweep {op_name}: {count} configs in {:.1} s", t.elapsed().as_secs_f64());
        }

        // 3-op: all (rd, rs, rt) triples, pairwise distinct, 336 combos.
        let three_ops: &[fn(u8, u8, u8) -> Instr] = &[
            |rd, rs, rt| Instr::Cswap { rd, rs, rt },
            |rd, rs, rt| Instr::Ccnot { rd, rs, rt },
        ];
        for mk in three_ops {
            let sample = mk(0, 1, 2);
            let op_name = format!("{sample:?}").split('{').next().unwrap().trim().to_string();
            let t = std::time::Instant::now();
            let mut count = 0u32;
            for rd in 0..NUM_REGS as u8 {
                for rs in 0..NUM_REGS as u8 {
                    if rs == rd { continue; }
                    for rt in 0..NUM_REGS as u8 {
                        if rt == rd || rt == rs { continue; }
                        let instr = mk(rd, rs, rt);
                        walk_one_arm_and_check(instr, &format!("{op_name}({rd},{rs},{rt})"));
                        count += 1;
                    }
                }
            }
            eprintln!("  sweep {op_name}: {count} configs in {:.1} s", t.elapsed().as_secs_f64());
        }

        // Immediate: all rd × all imm, 8 × 256 = 2048 per op.
        let imm_ops: &[fn(u8, u8) -> Instr] = &[
            |rd, imm| Instr::Addi { rd, imm },
            |rd, imm| Instr::Subi { rd, imm },
            |rd, imm| Instr::Xori { rd, imm },
            |rd, imm| Instr::Rotli { rd, imm },
            |rd, imm| Instr::Rotri { rd, imm },
        ];
        for mk in imm_ops {
            let sample = mk(0, 1);
            let op_name = format!("{sample:?}").split('{').next().unwrap().trim().to_string();
            let t = std::time::Instant::now();
            let mut count = 0u32;
            for rd in 0..NUM_REGS as u8 {
                for imm in 0..=255u8 {
                    let instr = mk(rd, imm);
                    walk_one_arm_and_check(instr, &format!("{op_name}({rd},{imm})"));
                    count += 1;
                }
            }
            eprintln!("  sweep {op_name}: {count} configs in {:.1} s", t.elapsed().as_secs_f64());
        }
    });
}

// ── Tier 4: bveq_netlist_walk_bootstrap (#[ignore], ~30-40s) ───────

#[test]
#[ignore]
fn bveq_netlist_walk_bootstrap() {
    // T2 real-machine anchor: concrete walk-vs-`run_rom_on_cpu` samples.
    // The walk shares a convention surface with instr_semantics, so a
    // symmetric bug is invisible without a real-machine check.
    use theseus_tisc::compile::run_rom_on_cpu;
    use theseus_tisc::types::StateVals;

    let samples: Vec<(Instr, StateVals, &str)> = vec![
        (
            Instr::Xor { rd: 0, rs: 1 },
            StateVals {
                regs: [0xAA, 0x55, 0, 0, 0, 0, 0, 0],
                pc: 0, br: 0, ir: 0,
                scratch: [0; 8], mem: [0; MEM_WORDS],
            },
            "Xor(0,1) AA^55",
        ),
        (
            Instr::Add { rd: 0, rs: 1 },
            StateVals {
                regs: [200, 100, 0, 0, 0, 0, 0, 0],
                pc: 0, br: 0, ir: 0,
                scratch: [0; 8], mem: [0; MEM_WORDS],
            },
            "Add(0,1) 200+100",
        ),
        (
            Instr::Ccnot { rd: 0, rs: 1, rt: 2 },
            StateVals {
                regs: [0x0F, 0xFF, 0xF0, 0, 0, 0, 0, 0],
                pc: 0, br: 0, ir: 0,
                scratch: [0; 8], mem: [0; MEM_WORDS],
            },
            "Ccnot(0,1,2) 0x0F^(0xFF&0xF0)",
        ),
        (
            Instr::Addi { rd: 0, imm: 42 },
            StateVals {
                regs: [100, 0, 0, 0, 0, 0, 0, 0],
                pc: 0, br: 0, ir: 0,
                scratch: [0; 8], mem: [0; MEM_WORDS],
            },
            "Addi(0,42) 100+42",
        ),
        (
            Instr::Rotl { rd: 0, rs: 1 },
            StateVals {
                regs: [0b1010_0101, 3, 0, 0, 0, 0, 0, 0],
                pc: 0, br: 0, ir: 0,
                scratch: [0; 8], mem: [0; MEM_WORDS],
            },
            "Rotl(0,1) 0xA5<<3",
        ),
    ];

    big_stack(move || {
        let map = extended_varmap();
        let vs = map.bdd_vars();

        for (instr, init, label) in &samples {
            let t = std::time::Instant::now();

            // Walk the system_step with concrete inputs.
            let word = asm::encode(*instr).unwrap();
            let concrete_sv = build_concrete_system_sv(init, vs);
            let iso = step::system_step(&[word]);
            let mut ctx = WalkCtx::new();
            let out_sv = eval_bdd(&iso, concrete_sv, vs, &mut ctx)
                .unwrap_or_else(|e| panic!("{label}: eval_bdd: {e}"));

            // Extract walked registers from output.
            let (composite_sv, _bus_sv) = match &out_sv {
                bveq_netlist::SV::Pair(c, b) => (&**c, &**b),
                _ => panic!("{label}: output not Pair"),
            };
            let (cpu_sv, _mem_sv) = match composite_sv {
                bveq_netlist::SV::Pair(cpu, mem) => (&**cpu, &**mem),
                _ => panic!("{label}: composite not Pair"),
            };
            let walked_regs: [u8; NUM_REGS] = std::array::from_fn(|r| {
                sv_to_u8(extract_field(cpu_sv, r, NUM_CPU_FIELDS), vs)
            });

            // Run the real machine.
            let real = run_rom_on_cpu(
                &[*instr], init.clone(), 1, 0, theseus_eval::eval,
            ).unwrap_or_else(|e| panic!("{label}: run_rom_on_cpu: {e}"));

            assert_eq!(
                walked_regs, real.regs,
                "{label}: walk-vs-real regs mismatch"
            );

            eprintln!(
                "  bootstrap {label}: walk {:.0} ms, real {:.0} ms",
                t.elapsed().as_secs_f64() * 1e3,
                t.elapsed().as_secs_f64() * 1e3,
            );
        }
    });
}

// ── Negative controls (non-vacuous) ────────────────────────────────

#[test]
fn bveq_netlist_negative_xor_vs_add() {
    // Assert xor-netlist vs add-semantics MISMATCH.
    big_stack(|| {
        let map = extended_varmap();
        let vs = map.bdd_vars();

        // Walk the Xor(0,1) netlist.
        let xor_instr = Instr::Xor { rd: 0, rs: 1 };
        let word = asm::encode(xor_instr).unwrap();
        let init_sv = build_cpu_init(&map, word, false);
        let iso = decoder::instruction(Op::Xor);
        let mut ctx = WalkCtx::new();
        let out_sv = eval_bdd(&iso, init_sv, vs, &mut ctx).unwrap();

        // Get Add(0,1) semantics.
        let add_sem = instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();

        // At least one bit of r0 must differ.
        let out_r0 = extract_word8_bdds(extract_field(&out_sv, 0, NUM_CPU_FIELDS));
        let mut found_mismatch = false;
        for bit in 0..BITS {
            let add_bdd = expr_to_bdd(&add_sem.regs[0][bit], &map).unwrap();
            if out_r0[bit] != add_bdd {
                found_mismatch = true;
                // Counterexample: XOR of the two should be satisfiable.
                let diff = out_r0[bit].xor(&add_bdd);
                assert!(
                    diff.sat_witness().is_some(),
                    "xor-vs-add bit {bit}: diff is unsat (vacuous)"
                );
                break;
            }
        }
        assert!(found_mismatch, "xor-netlist vs add-semantics: no mismatch found (vacuous!)");
    });
}

#[test]
fn bveq_netlist_negative_addi_symbolic_s0() {
    // Assert symbolic-s0 Addi MISMATCH — s0=0 is load-bearing + detected.
    // When s0 is symbolic, Addi adds (s0 ^ imm) instead of imm.
    big_stack(|| {
        let map = extended_varmap();
        let vs = map.bdd_vars();

        let addi_instr = Instr::Addi { rd: 0, imm: 42 };
        let word = asm::encode(addi_instr).unwrap();

        // Walk with symbolic s0 (pin_s0_zero = false).
        let init_sv = build_cpu_init(&map, word, false);
        let iso = decoder::instruction(Op::Addi);
        let mut ctx = WalkCtx::new();
        let out_sv = eval_bdd(&iso, init_sv, vs, &mut ctx).unwrap();

        // Get Addi(0,42) semantics (which assumes s0=0).
        let sem = instr_semantics(&addi_instr).unwrap();

        // At least one bit of r0 must differ.
        let out_r0 = extract_word8_bdds(extract_field(&out_sv, 0, NUM_CPU_FIELDS));
        let mut found_mismatch = false;
        for bit in 0..BITS {
            let expected = expr_to_bdd(&sem.regs[0][bit], &map).unwrap();
            if out_r0[bit] != expected {
                found_mismatch = true;
                break;
            }
        }
        assert!(
            found_mismatch,
            "Addi with symbolic s0: no mismatch found — s0=0 constraint is NOT load-bearing!"
        );
    });
}
