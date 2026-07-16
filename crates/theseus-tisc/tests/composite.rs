//! M-C2 Stage 1a-B acceptance tests: composite (CpuState, MemState) carrier.
//!
//! (a) COMPOSITE ORACLE: run_on_cpu(system_step) == eval(surface iso)
//!     on spill-to-memory programs and store→reload.
//! (b) C1 REAL-TREE RE-VERIFY: the real re-targeted select_mem_composite
//!     is ancilla-clean at N ∈ {2, 4, 8} over a BOUNDED domain.
//! (c) SHARED-ISA GUARD: decode_execute (ring, no MEXCH) and
//!     proj_CpuState(system_decode_execute) agree on non-MEXCH opcodes.
//! (d) CEILING: MAX_LEAVES = 14 + MEM_WORDS unchanged.

use theseus_core::{enumerate_values, type_of, Iso, Type, Value};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{rail_width, PhasedCellNetlist, RailPair};
use theseus_tisc::compile::{compile, run_on_cpu, run_on_cpu_fast};
use theseus_tisc::decoder;
use theseus_tisc::types::{
    decode_composite, encode_composite, MEM_WORDS, NUM_CPU_FIELDS, StateVals, W,
};

// ── Helpers ─────────────────────────────────────────────────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn bit_prod(n: usize) -> Type {
    if n == 1 {
        bit()
    } else {
        Type::prod(bit(), bit_prod(n - 1))
    }
}

fn cpu_run(iso: &Iso, input: &Value) -> Result<Value, theseus_tisc::compile::CompileError> {
    run_on_cpu(iso, input, theseus_eval::eval)
}

/// Fast oracle: byte-level run_on_cpu_fast == eval.
fn assert_oracle_fast(name: &str, iso: &Iso) {
    let (in_ty, _) = type_of(iso).expect("iso types");
    let vals = enumerate_values(&in_ty);
    assert!(!vals.is_empty(), "{name}: domain is empty");
    let mut checked = 0usize;
    for v in &vals {
        let cpu_out = run_on_cpu_fast(iso, v).unwrap_or_else(|e| {
            panic!("{name} (fast-sim): run failed on {v:?}: {e}")
        });
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("{name}: eval failed on {v:?}: {e:?}"));
        assert_eq!(
            cpu_out, eval_out,
            "{name} (fast-sim): CPU disagrees with eval on input {v:?}"
        );
        checked += 1;
    }
    eprintln!(
        "{name} (fast-sim): oracle agrees on all {checked} inputs ({} instructions)",
        compile(iso).unwrap().len()
    );
}

/// Authoritative oracle on a representative sample.
fn assert_oracle_sample(name: &str, iso: &Iso, count: usize) {
    let (in_ty, _) = type_of(iso).expect("iso types");
    let vals = enumerate_values(&in_ty);
    let n = vals.len();
    let mut indices: Vec<usize> = (0..count)
        .map(|i| {
            if count <= 1 { 0 } else { i * (n - 1) / (count - 1) }
        })
        .collect();
    indices.push(0);
    if n > 0 {
        indices.push(n - 1);
    }
    indices.sort();
    indices.dedup();

    let mut checked = 0usize;
    for &i in &indices {
        let v = &vals[i];
        let cpu_out = cpu_run(iso, v).unwrap_or_else(|e| {
            panic!("{name} (real-CPU sample): failed on #{i}: {e}")
        });
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("{name}: eval failed on #{i}: {e:?}"));
        assert_eq!(
            cpu_out, eval_out,
            "{name} (real-CPU sample): CPU disagrees with eval on input #{i} {v:?}"
        );
        checked += 1;
    }
    eprintln!(
        "{name} (real-CPU sample): oracle agrees on {checked}/{n} inputs"
    );
}

// ══════════════════════════════════════════════════════════════════════
// (a) COMPOSITE ORACLE — spill-to-memory programs via system_step
// ══════════════════════════════════════════════════════════════════════

#[test]
fn composite_oracle_spill_15_leaves() {
    // 15 leaves = 8 reg + 6 scratch + 1 mem. Exercises MEXCH through
    // system_step on the composite (CpuState, MemState) carrier.
    let iso = Iso::SwapProd(bit_prod(7), bit_prod(8));
    assert_oracle_fast("composite_spill_15", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test composite -- --ignored`"]
fn composite_oracle_spill_15_leaves_real_cpu() {
    assert_oracle_sample(
        "composite_spill_15",
        &Iso::SwapProd(bit_prod(7), bit_prod(8)),
        12,
    );
}

#[test]
fn composite_oracle_store_reload_round_trip() {
    // Swap first bit with last in a 15-leaf product: forces data through
    // memory and back (reg → mem → reg), exercising MEXCH in both
    // directions via system_step.
    let n = 15.min(14 + MEM_WORDS);
    let iso = Iso::SwapProd(bit_prod(1), bit_prod(n - 1));
    assert_oracle_fast("composite_store_reload", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test composite -- --ignored`"]
fn composite_oracle_store_reload_real_cpu() {
    let n = 15.min(14 + MEM_WORDS);
    assert_oracle_sample(
        "composite_store_reload",
        &Iso::SwapProd(bit_prod(1), bit_prod(n - 1)),
        12,
    );
}

#[test]
fn composite_oracle_max_ceiling() {
    // 14 + MEM_WORDS leaves: the straight-line ceiling.
    let n = 14 + MEM_WORDS;
    let half = n / 2;
    let iso = Iso::SwapProd(bit_prod(half), bit_prod(n - half));
    assert_oracle_fast("composite_max_ceiling", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// (b) C1 REAL-TREE RE-VERIFY — ancilla-clean on the real select_mem_composite
// ══════════════════════════════════════════════════════════════════════
//
// Bounded domain: for each N ∈ {2,4,8}, enumerate all addr ∈ 0..N
// and ctrl ∈ {set, clear}, with mem-word contents drawn from a small
// representative set {0x00, 0xFF, 0xAA, 0x55}. Total ≤ N*4 samples.
// Uses select_mem_composite_n(sel, NUM_CPU_FIELDS, N) to test the REAL
// construction at each N, not a standalone synthetic prim.

/// Untimed functional evaluation of the cell netlist.
fn eval_cells(net: &PhasedCellNetlist, input_bits: &[bool]) -> Vec<bool> {
    let mut line: Vec<Option<bool>> = vec![None; net.num_lines as usize];
    fn set_pair(line: &mut [Option<bool>], p: RailPair, v: bool) {
        line[p.line1.0 as usize] = Some(v);
        line[p.line0.0 as usize] = Some(!v);
    }
    fn get_pair(line: &[Option<bool>], p: RailPair) -> bool {
        let v1 = line[p.line1.0 as usize].expect("1-line driven");
        let v0 = line[p.line0.0 as usize].expect("0-line driven");
        assert_ne!(v0, v1, "INV-TOKEN violated: pair not 1-of-2");
        v1
    }
    assert_eq!(input_bits.len(), net.inputs.len());
    for (&p, &v) in net.inputs.iter().zip(input_bits) {
        set_pair(&mut line, p, v);
    }
    for cell in &net.cells {
        let vals: Vec<bool> = cell.inputs.iter().map(|&p| get_pair(&line, p)).collect();
        let outs = cell.kind.eval(&vals);
        for (&p, &v) in cell.outputs.iter().zip(&outs) {
            set_pair(&mut line, p, v);
        }
    }
    net.outputs.iter().map(|&p| get_pair(&line, p)).collect()
}

/// Assert ancilla-cleanliness and data correctness of an iso over
/// the given input domain (a list of bit vectors).
fn assert_ancilla_clean_on_samples(
    name: &str,
    iso: &Iso,
    reference: impl Fn(&[bool]) -> Vec<bool>,
    samples: &[Vec<bool>],
) -> usize {
    let (_in_ty, out_ty) = type_of(iso).expect("iso types");
    let (net, inits) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    let w_out = rail_width(&out_ty).unwrap();
    let n_anc = inits.len();

    for (idx, input_bits) in samples.iter().enumerate() {
        let expected_data = reference(input_bits);
        let mut full_input = input_bits.clone();
        full_input.extend_from_slice(&inits);
        let out = eval_cells(&net, &full_input);

        assert_eq!(
            &out[..w_out], &expected_data[..],
            "{name}: data mismatch on sample {idx}"
        );

        for (i, (got, expected)) in out[w_out..].iter().zip(&inits).enumerate() {
            assert_eq!(
                got, expected,
                "{name}: ancilla {i} dirty on sample {idx}"
            );
        }
    }

    eprintln!(
        "{name}: ancilla-clean on {} samples, {n_anc} ancilla lines",
        samples.len()
    );
    n_anc
}

/// Build the bounded sample set for select_mem_composite_n at N fields.
///
/// The iso type is (fields(NUM_CPU_FIELDS), fields(n_mem)).
/// We vary: addr ∈ 0..N with the addr bits set in IR_lo (R_RT field),
/// and the addressed mem word takes values from {0x00, 0xFF, 0xAA, 0x55}.
/// Other mem words get a fixed test pattern (index as byte).
fn select_mem_samples(n_mem: usize) -> Vec<Vec<bool>> {
    let n_addr_bits = match n_mem {
        2 => 1usize,
        4 => 2,
        8 => 3,
        _ => panic!("unsupported N={n_mem}"),
    };

    let cpu_bits = NUM_CPU_FIELDS * W;
    let total_bits = cpu_bits + n_mem * W;

    // R_RT: IR_lo bits [2,3,4] — IR_lo is field 10 of CpuState.
    let ir_lo_offset = 10 * W;
    let addr_bit_positions: [usize; 3] = [
        ir_lo_offset + 2, // R_RT.bits[0]
        ir_lo_offset + 3, // R_RT.bits[1]
        ir_lo_offset + 4, // R_RT.bits[2]
    ];

    let word_patterns: [u8; 4] = [0x00, 0xFF, 0xAA, 0x55];
    let mut samples = Vec::new();

    for addr in 0..n_mem {
        for &fill in &word_patterns {
            let mut bits = vec![false; total_bits];

            // Set the address bits in IR_lo (R_RT field)
            for (i, &pos) in addr_bit_positions.iter().take(n_addr_bits).enumerate() {
                if (addr >> i) & 1 == 1 {
                    bits[pos] = true;
                }
            }

            // Fill mem words: addressed word gets `fill`, others get index
            for m in 0..n_mem {
                let val = if m == addr { fill } else { m as u8 };
                let offset = cpu_bits + m * W;
                for b in 0..W {
                    bits[offset + b] = (val >> b) & 1 == 1;
                }
            }

            samples.push(bits);
        }
    }

    samples
}

/// Reference permutation for select_mem_composite: routes mem[addr] to
/// mem head using the same swap-cascade as the iso.
fn select_mem_reference(n_mem: usize, n_addr_bits: usize) -> impl Fn(&[bool]) -> Vec<bool> {
    move |bits: &[bool]| {
        let mut out = bits.to_vec();
        let cpu_bits = NUM_CPU_FIELDS * W;
        let ir_lo_offset = 10 * W;
        let addr_bit_positions = [ir_lo_offset + 2, ir_lo_offset + 3, ir_lo_offset + 4];

        // Apply the same swap cascade as select_fields: MSB down to LSB.
        for level in (0..n_addr_bits).rev() {
            let stride = 1usize << level;
            let ctrl_bit_set = out[addr_bit_positions[level]];
            if ctrl_bit_set {
                for i in 0..stride {
                    if i + stride < n_mem {
                        let off_a = cpu_bits + i * W;
                        let off_b = cpu_bits + (i + stride) * W;
                        for b in 0..W {
                            out.swap(off_a + b, off_b + b);
                        }
                    }
                }
            }
        }

        out
    }
}

/// C1 re-verify at a specific N using the REAL select_mem_composite_n.
fn c1_re_verify_at_n(n_mem: usize) {
    let n_addr_bits = match n_mem {
        2 => 1usize,
        4 => 2,
        8 => 3,
        _ => panic!("unsupported N={n_mem}"),
    };
    let iso = decoder::select_mem_composite_n(decoder::R_RT, NUM_CPU_FIELDS, n_mem);
    let samples = select_mem_samples(n_mem);
    assert!(!samples.is_empty());
    let n_anc = assert_ancilla_clean_on_samples(
        &format!("select_mem_composite_n{n_mem}"),
        &iso,
        select_mem_reference(n_mem, n_addr_bits),
        &samples,
    );
    eprintln!(
        "N={n_mem}: {n_anc} ancilla lines, {} samples",
        samples.len()
    );
}

#[test]
fn c1_real_select_mem_composite_ancilla_clean_n2() {
    c1_re_verify_at_n(2);
}

#[test]
fn c1_real_select_mem_composite_ancilla_clean_n4() {
    c1_re_verify_at_n(4);
}

#[test]
fn c1_real_select_mem_composite_ancilla_clean_n8() {
    c1_re_verify_at_n(8);
}

// ══════════════════════════════════════════════════════════════════════
// (c) SHARED-ISA GUARD — decode_execute (ring) vs system_decode_execute
// ══════════════════════════════════════════════════════════════════════
//
// Contract: on non-MEXCH opcodes, decode_execute'(v) ==
// proj_CpuState(system_decode_execute(v, mem=0)).

fn r_ins(opcode: u16, rd: u16, rs: u16, rt: u16) -> u16 {
    (1 << 15) | (opcode << 11) | (rd << 8) | (rs << 5) | (rt << 2)
}

fn i_ins(opc: u16, rd: u16, imm: u8) -> u16 {
    (opc << 12) | (rd << 9) | (u16::from(imm) << 1)
}

/// Sample IR words covering all 14 non-MEXCH opcodes + reserved.
fn non_mexch_ir_samples() -> Vec<u16> {
    vec![
        r_ins(0, 2, 1, 0),  // ADD
        r_ins(1, 3, 4, 0),  // SUB
        r_ins(2, 1, 2, 0),  // XOR
        r_ins(3, 0, 7, 0),  // EXCH
        r_ins(4, 3, 1, 0),  // ROTL
        r_ins(5, 3, 1, 0),  // ROTR
        r_ins(6, 4, 1, 2),  // CSWAP
        r_ins(7, 0, 0, 2),  // XCHG
        r_ins(8, 2, 0, 0),  // SWAPBR
        i_ins(0, 0, 0x05),  // ADDI
        i_ins(1, 1, 0x80),  // SUBI
        i_ins(2, 2, 0xAA),  // XORI
        i_ins(3, 3, 0x03),  // ROTLI
        i_ins(4, 4, 0x05),  // ROTRI
        r_ins(10, 1, 2, 3), // reserved R-format
        i_ins(5, 3, 0x7C),  // reserved I-format
        0,                  // IR=0 (degenerates to ADDI r0, #0)
    ]
}

#[test]
fn shared_isa_guard_ring_vs_system() {
    // For each non-MEXCH IR word, with a fixed register state,
    // verify decode_execute(v) == proj_CpuState(system_decode_execute(v, mem=0)).
    let ring_decoder = decoder::decode_execute();
    let sys_decoder = decoder::system_decode_execute();

    let regs = [0x10u8, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76, 0x87];
    let fuel_budget = 4_000_000_000u64;

    for &ir_word in &non_mexch_ir_samples() {
        let state = StateVals {
            regs,
            pc: 0x42,
            br: 0x99,
            ir: ir_word,
            scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
            mem: [0; MEM_WORDS],
        };

        // Ring path: decode_execute on CpuState
        let mut fuel = fuel_budget;
        let ring_out = theseus_eval::eval(&ring_decoder, state.encode(), &mut fuel)
            .unwrap_or_else(|e| panic!("ring eval failed on IR={ir_word:#06x}: {e:?}"));
        let ring_state = StateVals::decode(&ring_out);

        // System path: system_decode_execute on (CpuState, MemState)
        let mut fuel = fuel_budget;
        let sys_out = theseus_eval::eval(
            &sys_decoder,
            encode_composite(&state),
            &mut fuel,
        )
        .unwrap_or_else(|e| panic!("system eval failed on IR={ir_word:#06x}: {e:?}"));
        let sys_state = decode_composite(&sys_out);

        // Compare CpuState fields (ring_state.mem is [0], sys_state.mem may differ for MEXCH)
        assert_eq!(
            ring_state.regs, sys_state.regs,
            "ISA guard: regs disagree on IR={ir_word:#06x}"
        );
        assert_eq!(ring_state.pc, sys_state.pc, "ISA guard: PC disagree");
        assert_eq!(ring_state.br, sys_state.br, "ISA guard: BR disagree");
        assert_eq!(ring_state.ir, sys_state.ir, "ISA guard: IR disagree");
        assert_eq!(
            ring_state.scratch, sys_state.scratch,
            "ISA guard: scratch disagree on IR={ir_word:#06x}"
        );
    }
    eprintln!(
        "shared-ISA guard: ring and system decoders agree on {} non-MEXCH samples",
        non_mexch_ir_samples().len()
    );
}

// ══════════════════════════════════════════════════════════════════════
// (d) CEILING — MAX_LEAVES = 14 + MEM_WORDS unchanged
// ══════════════════════════════════════════════════════════════════════
// These re-confirm the existing ceiling; the existing tests in
// tests/compile.rs already cover this. We add an explicit guard.

#[test]
fn ceiling_straight_line_unchanged() {
    let max = 14 + MEM_WORDS;
    let iso = Iso::Id(bit_prod(max));
    assert!(compile(&iso).is_ok(), "ceiling {max} must compile");

    let over = max + 1;
    let iso_over = Iso::Id(bit_prod(over));
    assert!(compile(&iso_over).is_err(), "above ceiling {over} must reject");
}

#[test]
fn ceiling_branching_unchanged() {
    let max = 12 + MEM_WORDS;
    // Build a controlled NOT on max leaves (1 ctrl + (max-1) target).
    let tgt_ty = bit_prod(max - 1);
    let g_body = Iso::Times(
        Box::new(Iso::SwapSum(Type::One, Type::One)),
        Box::new(Iso::Id(bit_prod(max - 2))),
    );
    let iso = Iso::seq(
        Iso::SwapProd(bit(), tgt_ty.clone()),
        Iso::seq(
            Iso::Distrib(tgt_ty.clone(), Type::One, Type::One),
            Iso::seq(
                Iso::Plus(
                    Box::new(Iso::Id(Type::prod(tgt_ty.clone(), Type::One))),
                    Box::new(Iso::Times(Box::new(g_body), Box::new(Iso::Id(Type::One)))),
                ),
                Iso::seq(
                    Iso::Factor(tgt_ty.clone(), Type::One, Type::One),
                    Iso::SwapProd(tgt_ty, bit()),
                ),
            ),
        ),
    );
    assert!(compile(&iso).is_ok(), "branching ceiling {max} must compile");
}

// ══════════════════════════════════════════════════════════════════════
// Composite encode/decode round-trip sanity
// ══════════════════════════════════════════════════════════════════════

#[test]
fn encode_decode_composite_round_trip() {
    let state = StateVals {
        regs: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
        pc: 0x42,
        br: 0x99,
        ir: 0xABCD,
        scratch: [0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80],
        mem: [0xA0, 0xB0, 0xC0, 0xD0],
    };
    let composite = encode_composite(&state);
    let decoded = decode_composite(&composite);
    assert_eq!(state, decoded, "composite encode/decode round-trip failed");
}

#[test]
fn encode_decode_cpu_round_trip() {
    // CPU encode/decode drops mem (sets to [0]).
    let state = StateVals {
        regs: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
        pc: 0x42,
        br: 0x99,
        ir: 0xABCD,
        scratch: [0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80],
        mem: [0xA0, 0xB0, 0xC0, 0xD0],
    };
    let cpu_val = state.encode();
    let decoded = StateVals::decode(&cpu_val);
    assert_eq!(decoded.regs, state.regs);
    assert_eq!(decoded.pc, state.pc);
    assert_eq!(decoded.br, state.br);
    assert_eq!(decoded.ir, state.ir);
    assert_eq!(decoded.scratch, state.scratch);
    assert_eq!(decoded.mem, [0; MEM_WORDS], "CPU decode must zero mem");
}
