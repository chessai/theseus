#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1.
//! GO-ROLLOUT: the DECODER AUTHORITY, retired from brute-force simulation to a
//! SYMBOLIC BVEQ whole-domain proof.
//!
//! ── What this retires ──────────────────────────────────────────────
//! `tests/decoder.rs`'s whole-domain equivalence sweeps proved the WIRED
//! decoder (`decode_execute` / `shared_alu_block`) ≡ the per-op reference
//! `instruction(op)` by SIMULATION -- evaluating hundreds/thousands of
//! (rd, 2nd-operand) points per opcode through `theseus_eval::eval`
//! (~0.1-0.3 s/eval → ~53 min for the full decoder-class sweep):
//!   * b12_whole_domain_equiv_arith / _logic_rot  (wd_equiv_op, ~576 pts/op)
//!   * b13_whole_domain_equiv_ccnot / _cswap        (wd_equiv_3op, 40 pts/op)
//!   * b13_ccnot_full_rs_sweep                       (4096 pts)
//!
//! ── The symbolic replacement (strictly STRONGER) ────────────────────
//! For EVERY opcode, we IR-CONCRETE the instruction word (opcode + concrete
//! operand register/imm INDICES) but leave every operand VALUE SYMBOLIC, and
//! walk the FULL WIRED `decode_execute` netlist ONCE with `eval_bdd`. Keeping
//! the opcode/IR CONCRETE per walk means exactly one instruction decodes
//! (small, flat BDD -- the tractable regime); leaving operands symbolic covers
//! the WHOLE 2^8-per-operand datapath domain in that single walk -- a superset
//! of any finite (rd, op2) sample grid.
//!
//! CRITICAL for soundness (the opcode-DISPATCH obligation): we compare the
//! wired decoder's output against `bveq::instr_semantics` -- an INDEPENDENT
//! per-op semantics source (symbolic_exec over the ISA, NOT `instruction(op)`
//! and NOT a hand-written spec that assumes the right op). This is what makes
//! retiring the sim sound: a MIS-DISPATCH (wrong opcode predicate firing, a
//! config co-fire, an fmt mis-gate) shows up as the wired output ≠ the
//! independent semantics of the op the IR word actually names. The
//! `decode.rs ≡ instruction(op)` sim proved only the datapath; proving
//! `decode.rs ≡ instr_semantics` proves the datapath AND the dispatch against
//! an independent oracle.
//!
//! ── Authority tiers (the GO-rollout convention) ─────────────────────
//! 1. BVEQ WHOLE-DOMAIN (primary): `decoder_bveq_whole_domain` -- every op,
//!    several selector configs, symbolic operands, compared to instr_semantics.
//! 2. REALITY-ANCHOR BOOTSTRAP (sub-second): `decoder_bveq_bootstrap` --
//!    the SAME `decode_execute` iso run through concrete `theseus_eval::eval`
//!    on a handful of points equals the ISA reference model.
//! 3. NEGATIVE CONTROL (teeth): `decoder_bveq_negative_control` -- a WRONG
//!    semantics (Add for an Xor word) MUST be detected as inequivalent, with a
//!    non-vacuous (satisfiable) counterexample.
//!
//! ── Coverage boundary (honest) ──────────────────────────────────────
//! `instr_semantics` covers the 13 REGISTER-PURE ops. Xchg / Swapbr touch
//! scratch / BR beyond the register frame and have NO `instr_semantics`
//! source, so they are NOT proved against an independent semantics here -- the
//! wired decoder is still exercised on them by decoder.rs's kept
//! `per_instruction_lowering_and_semantics` + `full_decoder_moves` (against the
//! `reference` model) and the fib run_on_cpu==eval authority. Reserved opcodes
//! (identity) are checked structurally in the bootstrap.

#![allow(clippy::needless_range_loop)]

use std::time::Instant;

use theseus_tisc::asm::{self, Instr};
use theseus_tisc::bveq::{expr_to_bdd, instr_semantics, VarMap, BITS};
use theseus_tisc::bveq_netlist::{
    build_cpu_init, eval_bdd, extended_varmap, extract_field, extract_word8_bdds, needs_s0_pinned,
    sv_flatten, sv_word8_const, sv_word8_symbolic, WalkCtx, EXTENDED_NUM_OPERANDS, OP_BR, OP_S0,
};
use theseus_tisc::decoder::{self, Op};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_CPU_FIELDS, NUM_REGS, NUM_SCRATCH, W};

// ── big-stack: Iso trees + adjoint() recursion overflow the 2 MiB worker ──
fn big_stack<F, R>(f: F) -> R
where
    F: FnOnce() -> R + Send + 'static,
    R: Send + 'static,
{
    let builder = std::thread::Builder::new().stack_size(1024 * 1024 * 1024);
    builder.spawn(f).expect("spawn").join().expect("panicked")
}

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

fn assert_word8_zero(sv: &theseus_tisc::bveq_netlist::SV, vs: &biodivine_lib_bdd::BddVariableSet, ctx: &str) {
    let bits = sv_flatten(sv);
    let f = vs.mk_false();
    for (i, b) in bits.iter().enumerate() {
        assert_eq!(*b, f, "{ctx}: bit {i} should be 0");
    }
}

fn assert_word8_const(
    sv: &theseus_tisc::bveq_netlist::SV,
    expected: u8,
    vs: &biodivine_lib_bdd::BddVariableSet,
    ctx: &str,
) {
    let bits = sv_flatten(sv);
    let t = vs.mk_true();
    let f = vs.mk_false();
    for i in 0..W {
        let want = if (expected >> i) & 1 == 1 { &t } else { &f };
        assert_eq!(bits[i], *want, "{ctx}: bit {i} mismatch");
    }
}

fn assert_word8_identity(sv: &theseus_tisc::bveq_netlist::SV, entry: &theseus_tisc::bveq_netlist::SV, ctx: &str) {
    let out = sv_flatten(sv);
    let ent = sv_flatten(entry);
    for i in 0..W {
        assert_eq!(out[i], ent[i], "{ctx}: bit {i} not restored to entry");
    }
}

/// VarMap operand base for the s2..s7 SYMBOLIC scratch words (MINOR: catch a
/// spurious scratch read/write). Appended AFTER the 14 standard extended
/// operands (regs 0-7, s0 8, BR 9, mem 10-13). `RegTransfer`/`instr_semantics`
/// only reference register operands (0-7), so these extra operands never appear
/// in the expected BDDs -- but they DO appear in the wired-decoder output if the
/// decode path spuriously reads/writes s2.., which the identity assertion catches.
const OP_S2_BASE: usize = EXTENDED_NUM_OPERANDS; // 14
/// Number of DATA scratch words s2..s7 made symbolic.
const N_SYM_SCRATCH: usize = NUM_SCRATCH - 2; // 6 (s2..s7)
/// Wider VarMap: 14 standard operands + 6 for s2..s7.
fn scratch_symbolic_varmap() -> VarMap {
    VarMap::new(EXTENDED_NUM_OPERANDS + N_SYM_SCRATCH).expect("scratch-symbolic VarMap")
}

/// Right-nested product SV from a slice (mirrors bveq_netlist::sv_right_nested,
/// which is private).
fn sv_right_nested(svs: &[theseus_tisc::bveq_netlist::SV]) -> theseus_tisc::bveq_netlist::SV {
    let mut sv = svs[svs.len() - 1].clone();
    for head in svs[..svs.len() - 1].iter().rev() {
        sv = theseus_tisc::bveq_netlist::SV::pair(head.clone(), sv);
    }
    sv
}

/// Like `build_cpu_init`, but s2..s7 are SYMBOLIC (from operands OP_S2_BASE..)
/// instead of pinned to 0 -- so a spurious scratch read (output depends on s2..)
/// or write (output != symbolic input) is caught by the identity assertion.
/// s0/s1 stay CONTROL (pinned per `pin_s0`/convention).
fn build_cpu_init_scratch_symbolic(map: &VarMap, word: u32, pin_s0: bool) -> theseus_tisc::bveq_netlist::SV {
    use theseus_tisc::bveq_netlist::SV;
    let vs = map.bdd_vars();
    let mut fields: Vec<SV> = Vec::with_capacity(NUM_CPU_FIELDS);
    // r0-r7 symbolic
    for i in 0..NUM_REGS {
        fields.push(sv_word8_symbolic(map, i));
    }
    fields.push(sv_word8_const(0, vs)); // PC_lo = 0
    fields.push(sv_word8_symbolic(map, OP_BR)); // BR symbolic
    for byte in 0..4 {
        fields.push(sv_word8_const((word >> (8 * byte)) as u8, vs)); // IR bytes
    }
    // s0 (control) -- pinned or symbolic
    if pin_s0 {
        fields.push(sv_word8_const(0, vs));
    } else {
        fields.push(sv_word8_symbolic(map, OP_S0));
    }
    fields.push(sv_word8_const(0, vs)); // s1 (control) = 0
    // s2..s7 (DATA scratch) SYMBOLIC.
    for j in 0..N_SYM_SCRATCH {
        fields.push(sv_word8_symbolic(map, OP_S2_BASE + j));
    }
    fields.push(sv_word8_const(0, vs)); // PC_hi (field 22) = 0
    assert_eq!(fields.len(), NUM_CPU_FIELDS, "cpu init field count");
    sv_right_nested(&fields)
}

/// The heart of the authority: walk the WIRED `decode_execute` on the concrete
/// IR word for `instr` (operand VALUES symbolic), and prove the register
/// outputs equal the INDEPENDENT `instr_semantics(instr)` over the whole
/// symbolic domain, plus the full frame (PC_lo / PC_hi / BR / IR / s0..s7) is
/// preserved -- s2..s7 SYMBOLIC and asserted IDENTITY.
fn prove_wired_decoder(instr: Instr, label: &str) -> f64 {
    // Wider map: 14 standard operands + 6 for symbolic s2..s7. Both the walk
    // input AND `expr_to_bdd` use THIS map, so the comparison stays sound (the
    // register expressions reference only operands 0-7, unchanged in meaning).
    let map = scratch_symbolic_varmap();
    let vs = map.bdd_vars();
    let word = asm::encode(instr).unwrap_or_else(|e| panic!("{label}: encode: {e}"));
    // s0 = 0 on entry is the shared-decoder CONVENTION: unlike the per-op
    // `instruction(op)` isos (each of which routes+restores its own symbolic
    // operand), the SHARED `decode_execute` uses s0 as the persistent operand
    // transit slot for the whole ALU group, so a symbolic s0 leaks into the
    // result. The CPU maintains s0=s1=0 between steps (the same convention every
    // decoder.rs sim + the bootstrap below use); we pin it and assert it is
    // restored to 0 on exit.
    let pin_s0 = true;
    let _ = needs_s0_pinned(&instr);
    let init_sv = build_cpu_init_scratch_symbolic(&map, word, pin_s0);

    // THE WIRED DECODER -- full shared-crossbar dispatch, not the per-op iso.
    let iso = decoder::decode_execute();

    let t0 = Instant::now();
    let mut ctx = WalkCtx::new();
    let out_sv = eval_bdd(&iso, init_sv, vs, &mut ctx)
        .unwrap_or_else(|e| panic!("{label}: eval_bdd on decode_execute: {e}"));
    let walk_s = t0.elapsed().as_secs_f64();

    // Registers ≡ INDEPENDENT instr_semantics (the dispatch obligation).
    let semantics = instr_semantics(&instr).unwrap_or_else(|e| panic!("{label}: instr_semantics: {e}"));
    for reg in 0..NUM_REGS {
        let out_bdds = extract_word8_bdds(extract_field(&out_sv, reg, NUM_CPU_FIELDS));
        for bit in 0..BITS {
            let expected = expr_to_bdd(&semantics.regs[reg][bit], &map)
                .unwrap_or_else(|e| panic!("{label}: expr_to_bdd r{reg}[{bit}]: {e}"));
            assert_eq!(
                out_bdds[bit], expected,
                "{label}: wired decode_execute r{reg}[{bit}] != instr_semantics (mis-dispatch or datapath bug)"
            );
        }
    }

    // Frame: PC_lo=0 (decode_execute does not touch PC), BR/s0 restored,
    // IR preserved (concrete word), s1 = 0, s2..s7 = identity (symbolic in),
    // PC_hi (field 22) = 0 (MAJOR-3: a stray write into PC_hi was invisible).
    assert_word8_zero(extract_field(&out_sv, 8, NUM_CPU_FIELDS), vs, &format!("{label}: PC_lo"));
    let br_entry = sv_word8_symbolic(&map, OP_BR);
    assert_word8_identity(extract_field(&out_sv, 9, NUM_CPU_FIELDS), &br_entry, &format!("{label}: BR"));
    for byte in 0..4 {
        assert_word8_const(
            extract_field(&out_sv, 10 + byte, NUM_CPU_FIELDS),
            (word >> (8 * byte)) as u8,
            vs,
            &format!("{label}: IR_b{byte}"),
        );
    }
    if pin_s0 {
        assert_word8_zero(extract_field(&out_sv, 14, NUM_CPU_FIELDS), vs, &format!("{label}: s0"));
    } else {
        let s0_entry = sv_word8_symbolic(&map, OP_S0);
        assert_word8_identity(extract_field(&out_sv, 14, NUM_CPU_FIELDS), &s0_entry, &format!("{label}: s0"));
    }
    // s1 (control) = 0.
    assert_word8_zero(extract_field(&out_sv, 15, NUM_CPU_FIELDS), vs, &format!("{label}: s1"));
    // s2..s7: SYMBOLIC input must be restored to IDENTITY (a spurious scratch
    // read/write -- invisible when s2.. was pinned to 0 -- is caught here).
    for j in 0..N_SYM_SCRATCH {
        let s_entry = sv_word8_symbolic(&map, OP_S2_BASE + j);
        assert_word8_identity(
            extract_field(&out_sv, 16 + j, NUM_CPU_FIELDS),
            &s_entry,
            &format!("{label}: s{} (symbolic scratch identity)", j + 2),
        );
    }
    // MAJOR-3: PC_hi is field 22 (NUM_CPU_FIELDS=23, build_cpu_init puts PC_hi
    // there); a stray write into it was previously unasserted (invisible).
    assert_eq!(NUM_CPU_FIELDS, 23, "field layout: NUM_CPU_FIELDS must be 23 (PC_hi at 22)");
    assert_word8_zero(extract_field(&out_sv, 22, NUM_CPU_FIELDS), vs, &format!("{label}: PC_hi"));
    walk_s
}

// ══════════════════════════════════════════════════════════════════════
// 1. BVEQ WHOLE-DOMAIN authority -- retires the decoder.rs sim sweeps.
// ══════════════════════════════════════════════════════════════════════

/// Every register-pure op × several selector/imm configs, symbolic operands,
/// wired `decode_execute` ≡ `instr_semantics` over the WHOLE datapath domain.
/// This one test subsumes b12_whole_domain_equiv_{arith,logic_rot} and
/// b13_whole_domain_equiv_{ccnot,cswap} + b13_ccnot_full_rs_sweep: each walk
/// covers all 2^8 operand values (a superset of their sample grids), and it is
/// checked against an INDEPENDENT semantics (stronger than `≡ instruction(op)`).
#[test]
fn decoder_bveq_whole_domain() {
    // Selector configs per op: distinct rd/rs/rt indices exercising crossbar
    // routing (low / mid / high; rs=0 carry corner; both 3-op arrangements).
    let mut configs: Vec<(Instr, String)> = vec![
        (Instr::Add { rd: 0, rs: 1 }, "Add(0,1)".into()),
        (Instr::Add { rd: 1, rs: 0 }, "Add(1,0)-rs=0".into()),
        (Instr::Add { rd: 3, rs: 5 }, "Add(3,5)".into()),
        (Instr::Sub { rd: 0, rs: 1 }, "Sub(0,1)".into()),
        (Instr::Sub { rd: 3, rs: 5 }, "Sub(3,5)".into()),
        (Instr::Xor { rd: 0, rs: 1 }, "Xor(0,1)".into()),
        (Instr::Xor { rd: 6, rs: 7 }, "Xor(6,7)".into()),
        (Instr::Exch { rd: 0, rs: 1 }, "Exch(0,1)".into()),
        (Instr::Exch { rd: 2, rs: 7 }, "Exch(2,7)".into()),
        (Instr::Rotl { rd: 1, rs: 3 }, "Rotl(1,3)".into()),
        (Instr::Rotl { rd: 0, rs: 1 }, "Rotl(0,1)".into()),
        (Instr::Rotr { rd: 5, rs: 2 }, "Rotr(5,2)".into()),
        (Instr::Cswap { rd: 0, rs: 1, rt: 2 }, "Cswap(0,1,2)".into()),
        (Instr::Cswap { rd: 4, rs: 3, rt: 6 }, "Cswap(4,3,6)".into()),
        (Instr::Ccnot { rd: 0, rs: 1, rt: 2 }, "Ccnot(0,1,2)".into()),
        (Instr::Ccnot { rd: 5, rs: 3, rt: 7 }, "Ccnot(5,3,7)".into()),
    ];

    // ── IMMEDIATE bit-lane coverage (MAJOR-2) ───────────────────────────
    // The retired sims swept the FULL immediate byte (256 values at w8). The
    // original BVEQ configs pinned 1-2 arbitrary imms (Addi{42,255}, Subi{100})
    // that do NOT distinguish imm bit-lanes -- a bit1↔bit3 imm-lane cross is
    // invisible. Restore lane coverage: the one-hots {1,2,4,8,16,32,64,128}
    // exercise every imm bit-lane INDEPENDENTLY, plus two binary-signature
    // values (0xAA=10101010, 0xCC=11001100) cross lanes. The operand VALUES are
    // already symbolic, so this single axis (the immediate) is what needs the
    // sweep; each (op, imm) walk still covers the WHOLE 2^8 operand domain.
    let imm_lane_set: [u8; 10] = [1, 2, 4, 8, 16, 32, 64, 128, 0xAA, 0xCC];
    for &imm in &imm_lane_set {
        configs.push((Instr::Addi { rd: 0, imm }, format!("Addi(0,{imm:#04x})")));
        configs.push((Instr::Subi { rd: 2, imm }, format!("Subi(2,{imm:#04x})")));
        configs.push((Instr::Xori { rd: 5, imm }, format!("Xori(5,{imm:#04x})")));
    }
    // Rotli/Rotri imm is a rotate AMOUNT masked to `& 7` (3 effective bits), so
    // amounts {0..=7} are EXHAUSTIVE over the whole effective-amount domain --
    // every amount lane exercised, nothing hidden.
    for imm in 0u8..=7 {
        configs.push((Instr::Rotli { rd: 2, imm }, format!("Rotli(2,{imm})")));
        configs.push((Instr::Rotri { rd: 7, imm }, format!("Rotri(7,{imm})")));
    }

    big_stack(move || {
        let t_all = Instant::now();
        let mut total_walk = 0.0f64;
        for (instr, label) in &configs {
            let _ = instr_to_op(instr); // pin the op mapping is total
            let w = prove_wired_decoder(*instr, label);
            total_walk += w;
            eprintln!("  {label}: walk {:.0} ms", w * 1e3);
        }
        eprintln!(
            "DECODER-BVEQ whole-domain: {} configs, wired decode_execute ≡ instr_semantics -- \
             SUPERSET on the operand-value axis (WHOLE 2^8-per-operand domain vs the retired sim \
             grids) and LANE-COMPLETE on the immediate axis (one-hots {{1,2,4,8,16,32,64,128}} + \
             signatures {{0xAA,0xCC}}, rotate amounts 0..=7 exhaustive); total {:.2}s \
             (decoder.rs sim baseline: ~53 min).",
            configs.len(),
            t_all.elapsed().as_secs_f64()
        );
        eprintln!("  (sum of eval_bdd walks: {total_walk:.2}s)");
    });
}

// ══════════════════════════════════════════════════════════════════════
// 2. REALITY-ANCHOR BOOTSTRAP -- same iso, concrete eval == ISA reference.
// ══════════════════════════════════════════════════════════════════════

const EVAL_FUEL: u64 = 4_000_000_000;

/// The ISA reference model (a-format), matching decoder.rs's `reference`.
fn reference_regs(instr: Instr, regs: &[u8; 8], scratch2: u8) -> [u8; 8] {
    let mut r = *regs;
    match instr {
        Instr::Add { rd, rs } => r[rd as usize] = r[rd as usize].wrapping_add(r[rs as usize]),
        Instr::Sub { rd, rs } => r[rd as usize] = r[rd as usize].wrapping_sub(r[rs as usize]),
        Instr::Xor { rd, rs } => r[rd as usize] ^= r[rs as usize],
        Instr::Exch { rd, rs } => r.swap(rd as usize, rs as usize),
        Instr::Rotl { rd, rs } => r[rd as usize] = r[rd as usize].rotate_left(u32::from(r[rs as usize] & 7)),
        Instr::Rotr { rd, rs } => r[rd as usize] = r[rd as usize].rotate_right(u32::from(r[rs as usize] & 7)),
        Instr::Cswap { rd, rs, rt } => {
            if r[rd as usize] & 1 == 1 {
                r.swap(rs as usize, rt as usize);
            }
        }
        Instr::Ccnot { rd, rs, rt } => r[rd as usize] ^= r[rs as usize] & r[rt as usize],
        Instr::Addi { rd, imm } => r[rd as usize] = r[rd as usize].wrapping_add(imm),
        Instr::Subi { rd, imm } => r[rd as usize] = r[rd as usize].wrapping_sub(imm),
        Instr::Xori { rd, imm } => r[rd as usize] ^= imm,
        Instr::Rotli { rd, imm } => r[rd as usize] = r[rd as usize].rotate_left(u32::from(imm & 7)),
        Instr::Rotri { rd, imm } => r[rd as usize] = r[rd as usize].rotate_right(u32::from(imm & 7)),
        _ => {}
    }
    let _ = scratch2;
    r
}

/// The SAME `decode_execute` iso, run through concrete `theseus_eval::eval` on a
/// few register points, equals the ISA reference -- the reality anchor that a
/// symmetric convention bug (shared by walk + instr_semantics) cannot hide.
#[test]
fn decoder_bveq_bootstrap() {
    let iso = decoder::decode_execute();
    let pts: [[u8; 8]; 4] = [
        [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07],
        [0xAA, 0x55, 0xFF, 0x00, 0x0F, 0xF0, 0x80, 0x7F],
        [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF],
        [0x2A, 0xC3, 0x11, 0x5A, 0x91, 0x3C, 0xE1, 0x08],
    ];
    let instrs: Vec<Instr> = vec![
        Instr::Add { rd: 0, rs: 1 },
        Instr::Sub { rd: 3, rs: 5 },
        Instr::Xor { rd: 6, rs: 7 },
        Instr::Exch { rd: 2, rs: 7 },
        Instr::Rotl { rd: 1, rs: 3 },
        Instr::Rotr { rd: 5, rs: 2 },
        Instr::Cswap { rd: 0, rs: 1, rt: 2 },
        Instr::Ccnot { rd: 5, rs: 3, rt: 7 },
        Instr::Addi { rd: 3, imm: 255 },
        Instr::Subi { rd: 2, imm: 100 },
        Instr::Xori { rd: 5, imm: 0xFF },
        Instr::Rotli { rd: 2, imm: 3 },
        Instr::Rotri { rd: 7, imm: 5 },
    ];
    let t0 = Instant::now();
    for instr in &instrs {
        let word = asm::encode(*instr).unwrap();
        for regs in &pts {
            let s = StateVals {
                regs: *regs,
                pc: 0,
                br: 0x99,
                ir: word,
                // s0 = s1 = 0 (decoder convention); s2.. arbitrary background.
                scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
                mem: [0; MEM_WORDS],
            };
            let mut fuel = EVAL_FUEL;
            let ev = theseus_eval::eval(&iso, s.encode(), &mut fuel)
                .unwrap_or_else(|e| panic!("bootstrap {instr:?}: eval: {e:?}"));
            let got = StateVals::decode(&ev);
            let exp = reference_regs(*instr, regs, 0);
            assert_eq!(
                got.regs, exp,
                "bootstrap {instr:?}: wired decode_execute eval != ISA reference (regs {regs:?})"
            );
            assert_eq!(got.ir, word, "bootstrap {instr:?}: IR must be preserved");
            assert_eq!(got.scratch[0], 0, "bootstrap {instr:?}: s0 restored");
            assert_eq!(got.scratch[1], 0, "bootstrap {instr:?}: s1 restored");
        }
    }
    // Reserved opcode → identity (structural coverage of the dispatch's default arm).
    {
        let reserved_ir = (15u32 << 25) | (1 << 20) | (2 << 15) | (3 << 10); // R fmt, opcode 15
        let s = StateVals {
            regs: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88],
            pc: 0,
            br: 0x99,
            ir: reserved_ir,
            scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
            mem: [0; MEM_WORDS],
        };
        let mut fuel = EVAL_FUEL;
        let ev = theseus_eval::eval(&iso, s.encode(), &mut fuel).expect("reserved eval");
        let got = StateVals::decode(&ev);
        assert_eq!(got.regs, s.regs, "reserved opcode must be identity on regs");
        assert_eq!(got.ir, reserved_ir, "reserved: IR preserved");
    }
    eprintln!(
        "DECODER-BVEQ bootstrap: SAME decode_execute iso, concrete theseus_eval == ISA reference \
         across 13 ops × 4 points + reserved-identity, {:.2}s (the reality anchor).",
        t0.elapsed().as_secs_f64()
    );
}

// ══════════════════════════════════════════════════════════════════════
// 3. NEGATIVE CONTROL -- a wrong semantics must be caught (teeth).
// ══════════════════════════════════════════════════════════════════════

#[test]
fn decoder_bveq_negative_control() {
    big_stack(|| {
        let map = extended_varmap();
        let vs = map.bdd_vars();

        // Walk the WIRED decoder on an Xor(0,1) word.
        let xor = Instr::Xor { rd: 0, rs: 1 };
        let word = asm::encode(xor).unwrap();
        let init_sv = build_cpu_init(&map, word, false);
        let iso = decoder::decode_execute();
        let mut ctx = WalkCtx::new();
        let out_sv = eval_bdd(&iso, init_sv, vs, &mut ctx).unwrap();
        let out_r0 = extract_word8_bdds(extract_field(&out_sv, 0, NUM_CPU_FIELDS));

        // WRONG-1: compare Xor's decode against ADD semantics -- must DIFFER,
        // with a satisfiable (non-vacuous) counterexample.
        let add_sem = instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();
        let mut found = false;
        for bit in 0..BITS {
            let add_bdd = expr_to_bdd(&add_sem.regs[0][bit], &map).unwrap();
            if out_r0[bit] != add_bdd {
                found = true;
                let diff = out_r0[bit].xor(&add_bdd);
                assert!(diff.sat_witness().is_some(), "xor-vs-add bit {bit}: vacuous diff");
                break;
            }
        }
        assert!(found, "NEG: Xor decode vs Add semantics: no mismatch (vacuous!)");

        // WRONG-2: a DIFFERENT IR word (Add(0,1)) must produce a DIFFERENT
        // wired-decode output on r0 -- proves the walk tracks the opcode.
        let add_word = asm::encode(Instr::Add { rd: 0, rs: 1 }).unwrap();
        let init2 = build_cpu_init(&map, add_word, false);
        let mut ctx2 = WalkCtx::new();
        let out2 = eval_bdd(&decoder::decode_execute(), init2, vs, &mut ctx2).unwrap();
        let out2_r0 = extract_word8_bdds(extract_field(&out2, 0, NUM_CPU_FIELDS));
        let differs = (0..BITS).any(|b| out2_r0[b] != out_r0[b]);
        assert!(differs, "NEG: Add word must decode differently from Xor word (teeth)");

        // WRONG-3 (MAJOR-3 teeth): the real decode leaves PC_hi (field 22) = 0.
        // A stray write into PC_hi (e.g. a copy of r0) would leave it != 0. Prove
        // the new PC_hi assertion has TEETH: the true PC_hi output (const 0)
        // DIFFERS from a stray-copy-of-r0 candidate on the domain where r0 != 0.
        let pc_hi_out = extract_word8_bdds(extract_field(&out_sv, 22, NUM_CPU_FIELDS));
        let f = vs.mk_false();
        for (i, b) in pc_hi_out.iter().enumerate() {
            assert!(*b == f, "sanity: real PC_hi bit {i} must be 0");
        }
        let stray_r0 = extract_word8_bdds(&sv_word8_symbolic(&map, 0)); // r0 symbolic in
        let pc_hi_differs = (0..BITS).any(|b| pc_hi_out[b] != stray_r0[b]);
        assert!(
            pc_hi_differs,
            "NEG PC_hi: a stray PC_hi=r0 write must DIFFER from the real PC_hi=0 (teeth)"
        );
        let r0_nonzero = stray_r0.iter().fold(f.clone(), |acc, b| acc.or(b));
        assert!(
            r0_nonzero.sat_witness().is_some(),
            "NEG PC_hi: stray-copy witness (r0 != 0) must be satisfiable"
        );

        eprintln!("DECODER-BVEQ negative control: wired decode ≠ {{Add-semantics, Add-word}}; \
                   real PC_hi=0 ≠ a stray PC_hi=r0 write -- teeth (incl. MAJOR-3 PC_hi).");
    });
}
