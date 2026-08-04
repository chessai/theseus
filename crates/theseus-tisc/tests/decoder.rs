#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! Differential-oracle tests for the TISC-1 decoder
//! (`decode_execute : State <-> State`).
//!
//! Since 2^160+ inputs are not enumerable, we SAMPLE: for each live
//! opcode (9 R-format + 5 I-format), several (IR, State) pairs varying
//! register-field / immediate choices and register contents (including
//! edge values 0x00/0xFF/0xAA), plus reserved-opcode samples that must
//! act as the identity. Checks per sample:
//!
//!   1. lowering succeeds with max cell kind <= K3 (asserted once);
//!   2. faithfulness: balance-then-simulate == bits of core eval;
//!   3. semantics: decode(eval(iso, encode(sample))) == the Rust
//!      reference model of the ISA (IR preserved, everything else
//!      identity).
//!
//! Sampling constraints follow the decoder's documented conventions:
//! s0 = s1 = 0 on entry, pad bits zero, no register aliasing (the
//! source-level legality checker's job, P1 -- see decoder.rs docs).

use std::sync::OnceLock;

use theseus_core::{type_of, Iso, Type};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{
    balance, bits_of, lower_iso, rail_width, simulate, CellKind, PhasedCellNetlist,
};
use theseus_tisc::decoder::{self, Op};
use theseus_tisc::types::{StateVals, MEM_WORDS};

mod common;
use common::{eval_state, i_ins, r_ins};

const EVAL_FUEL: u64 = 4_000_000_000;

// ── Instruction encoders (F1 spec §3) ──────────────────────────────

// ── Reference model of the ISA ─────────────────────────────────────

fn reference(s: &StateVals) -> StateVals {
    let mut out = s.clone();
    let ir = s.ir;
    if ir & (1 << 31) != 0 {
        let opcode = (ir >> 25) & 0x3F;
        let rd = ((ir >> 20) & 0x1F) as usize;
        let rs = ((ir >> 15) & 0x1F) as usize;
        let rt = ((ir >> 10) & 0x1F) as usize;
        match opcode {
            0b0000 => out.regs[rd] = out.regs[rd].wrapping_add(out.regs[rs]),
            0b0001 => out.regs[rd] = out.regs[rd].wrapping_sub(out.regs[rs]),
            0b0010 => out.regs[rd] ^= out.regs[rs],
            0b0011 => out.regs.swap(rd, rs),
            0b0100 => out.regs[rd] = out.regs[rd].rotate_left(u32::from(out.regs[rs] & 7)),
            0b0101 => out.regs[rd] = out.regs[rd].rotate_right(u32::from(out.regs[rs] & 7)),
            0b0110 => {
                if out.regs[rd] & 1 == 1 {
                    out.regs.swap(rs, rt);
                }
            }
            0b0111 => std::mem::swap(&mut out.regs[rd], &mut out.scratch[rt]),
            0b1000 => {
                let t = out.regs[rd];
                out.regs[rd] = out.br as u8;
                out.br = u16::from(t);
            }
            // 0b1001 = MEXCH, 0b1010 = MEXCHR: reserved at CpuState level (identity).
            0b1011 => {
                // CCNOT: r[rd] ^= r[rs] & r[rt] (byte-parallel)
                out.regs[rd] ^= out.regs[rs] & out.regs[rt];
            }
            _ => {} // reserved: identity
        }
    } else {
        let opc = (ir >> 25) & 0x3F;
        let rd = ((ir >> 20) & 0x1F) as usize;
        let imm = (ir & 0xFF) as u8;
        match opc {
            0b000 => out.regs[rd] = out.regs[rd].wrapping_add(imm),
            0b001 => out.regs[rd] = out.regs[rd].wrapping_sub(imm),
            0b010 => out.regs[rd] ^= imm,
            0b011 => out.regs[rd] = out.regs[rd].rotate_left(u32::from(imm & 7)),
            0b100 => out.regs[rd] = out.regs[rd].rotate_right(u32::from(imm & 7)),
            _ => {} // reserved: identity
        }
    }
    out
}

// ── Sample states (s0 = s1 = 0 per decoder convention) ─────────────

fn regs_a() -> [u8; 8] {
    [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3]
}

fn regs_b() -> [u8; 8] {
    [0x10, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76, 0x87]
}

fn st(regs: [u8; 8], ir: u32) -> StateVals {
    // mem is zero: CpuState (state_ty()) doesn't include memory fields;
    // encode/decode drops mem. MEXCH tests are on the composite carrier.
    StateVals {
        regs,
        pc: 0x42,
        br: 0x99,
        ir,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
        mem: [0; MEM_WORDS],
    }
}

/// Per-opcode sample sets. The first sample of each entry is also used
/// by the per-instruction dispatch tests.
fn op_samples(op: Op) -> Vec<StateVals> {
    match op {
        Op::Add => vec![
            st(regs_a(), r_ins(0, 0, 1, 0)),
            st(regs_b(), r_ins(0, 7, 6, 0)),
            st(regs_a(), r_ins(0, 3, 5, 0)),
        ],
        Op::Sub => vec![
            st(regs_a(), r_ins(1, 2, 4, 0)),
            st(regs_b(), r_ins(1, 5, 0, 0)),
        ],
        Op::Xor => vec![
            st(regs_a(), r_ins(2, 1, 2, 0)),
            st(regs_b(), r_ins(2, 6, 3, 0)),
        ],
        Op::Exch => vec![
            st(regs_a(), r_ins(3, 0, 7, 0)),
            st(regs_b(), r_ins(3, 4, 2, 0)),
        ],
        Op::Rotl => vec![
            st(regs_a(), r_ins(4, 3, 1, 0)), // amount = 0xFF & 7 = 7
            st(regs_b(), r_ins(4, 5, 6, 0)), // amount = 0x76 & 7 = 6
            st(regs_a(), r_ins(4, 2, 0, 0)), // amount = 0x00 & 7 = 0
        ],
        Op::Rotr => vec![
            st(regs_a(), r_ins(5, 3, 1, 0)),
            st(regs_b(), r_ins(5, 2, 0, 0)), // amount = 0x10 & 7 = 0
        ],
        Op::Cswap => vec![
            st(regs_a(), r_ins(6, 4, 1, 2)), // r4 = 0x01: taken
            st(regs_a(), r_ins(6, 0, 1, 2)), // r0 = 0x00: not taken
            st(regs_b(), r_ins(6, 1, 3, 5)), // r1 = 0x21: taken
        ],
        Op::Ccnot => vec![
            // rd=4(0x01), rs=1(0xFF), rt=2(0xAA): rd ^= FF&AA = rd^AA = 0xAB
            st(regs_a(), r_ins(0b1011, 4, 1, 2)),
            // rd=0(0x00), rs=2(0xAA), rt=3(0x55): rd ^= AA&55 = rd^00 = 0x00
            st(regs_a(), r_ins(0b1011, 0, 2, 3)),
            // rd=7(0xC3), rs=4(0x01), rt=5(0x80): rd ^= 01&80 = rd^00 = 0xC3
            st(regs_a(), r_ins(0b1011, 7, 4, 5)),
            // dense-byte: rd=0(0x10), rs=1(0x21), rt=2(0x32): rd ^= 21&32 = rd^20 = 0x30
            st(regs_b(), r_ins(0b1011, 0, 1, 2)),
            // rd=0(0x00), rs=3(0x55), rt=4(0x01): rd ^= 55&01 = rd^01 = 0x01
            st(regs_a(), r_ins(0b1011, 0, 3, 4)),
        ],
        Op::Xchg => vec![
            st(regs_a(), r_ins(7, 0, 0, 2)),
            st(regs_b(), r_ins(7, 5, 0, 7)),
            st(regs_a(), r_ins(7, 3, 0, 0)), // sx = 0: swap with (zero) s0
        ],
        Op::Swapbr => vec![
            st(regs_a(), r_ins(8, 0, 0, 0)), // SWAPBR r0
            st(regs_b(), r_ins(8, 3, 0, 0)), // SWAPBR r3
            st(regs_a(), r_ins(8, 7, 0, 0)), // SWAPBR r7
        ],
        Op::Mexch => panic!("MEXCH operates on composite carrier; not in CpuState decoder"),
        Op::MexchReg => {
            panic!("MEXCHR operates on composite carrier; not in CpuState decoder")
        }
        Op::Addi => vec![
            st(regs_a(), i_ins(0, 0, 0xFF)),
            st(regs_b(), i_ins(0, 7, 0x01)),
            st(regs_a(), i_ins(0, 2, 0x00)),
        ],
        Op::Subi => vec![
            st(regs_a(), i_ins(1, 1, 0x80)),
            st(regs_b(), i_ins(1, 6, 0xFF)),
        ],
        Op::Xori => vec![
            st(regs_a(), i_ins(2, 2, 0xAA)),
            st(regs_b(), i_ins(2, 0, 0x00)),
        ],
        Op::Rotli => vec![
            st(regs_a(), i_ins(3, 3, 0x03)),
            st(regs_b(), i_ins(3, 1, 0x08)), // amount = 8 & 7 = 0
            st(regs_a(), i_ins(3, 5, 0xFF)), // amount = 7
        ],
        Op::Rotri => vec![
            st(regs_a(), i_ins(4, 4, 0x05)),
            st(regs_b(), i_ins(4, 0, 0x01)),
        ],
    }
}

fn reserved_samples() -> Vec<StateVals> {
    vec![
        st(regs_a(), r_ins(9, 0, 0, 0)), // MEXCH opcode (reserved at CpuState level)
        st(regs_a(), r_ins(10, 1, 2, 3)),
        st(regs_b(), r_ins(15, 7, 6, 5)),
        st(regs_a(), i_ins(5, 3, 0x7C)),
        st(regs_b(), i_ins(7, 0, 0xFF)),
    ]
}

// ── Eval helper ────────────────────────────────────────────────────

// ══════════════════════════════════════════════════════════════════════
// 1. Register-select crossbar in isolation
// ══════════════════════════════════════════════════════════════════════

#[test]
fn select_reg_crossbar_isolated() {
    let iso = decoder::select_reg(decoder::R_RD);

    // Lowering: pure K1 routing (7 controlled word-swaps × 8 Fredkins).
    let net = lower_iso(&iso).expect("select_reg lowers");
    assert_eq!(net.validate(), Ok(()));
    let counts = net.cell_counts();
    eprintln!("select_reg(R_RD): cell counts {counts:?}");
    let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
    assert_eq!(max_kind, CellKind::K1, "crossbar must be pure K1");
    assert_eq!(counts.get(&CellKind::K1).copied().unwrap_or(0), 56);

    // Semantics for every field value: r[rd] lands at slot 0, the rest
    // are permuted per the documented model, all other fields preserved.
    for rd in 0..8u32 {
        let s = st(regs_b(), r_ins(0, rd, 0, 0)); // R_RD reads IR bits 10-8
        let got = eval_state("select_reg", &iso, &s, EVAL_FUEL);
        let mut expected = s.clone();
        decoder::crossbar_model(rd as usize, &mut expected.regs);
        assert_eq!(got, expected, "select_reg: rd={rd}");
        assert_eq!(
            got.regs[0], s.regs[rd as usize],
            "select_reg: selected register must land at slot 0 (rd={rd})"
        );
    }

    // Round trip: select ; unselect == identity.
    let rt = Iso::seq(iso.clone(), Iso::sym(iso));
    let s = st(regs_a(), r_ins(0, 5, 0, 0));
    assert_eq!(eval_state("select_reg roundtrip", &rt, &s, EVAL_FUEL), s);
}

#[test]
fn select_scratch_crossbar_isolated() {
    let iso = decoder::select_scratch(decoder::R_RT);
    for sx in 0..8u32 {
        let s = st(regs_b(), r_ins(0, 0, 0, sx)); // R_RT reads IR bits 4-2
        let got = eval_state("select_scratch", &iso, &s, EVAL_FUEL);
        let mut expected = s.clone();
        decoder::crossbar_model(sx as usize, &mut expected.scratch);
        assert_eq!(got, expected, "select_scratch: sx={sx}");
        assert_eq!(got.scratch[0], s.scratch[sx as usize]);
    }
}

// ══════════════════════════════════════════════════════════════════════
// 2. Per-instruction dispatch: lowering budget + semantics + exclusivity
// ══════════════════════════════════════════════════════════════════════

#[test]
fn per_instruction_lowering_and_semantics() {
    for op in Op::ALL {
        let iso = decoder::instruction(op);
        let net = lower_iso(&iso).unwrap_or_else(|e| panic!("{op:?}: lowering failed: {e}"));
        assert_eq!(net.validate(), Ok(()), "{op:?}: netlist invalid");
        let counts = net.cell_counts();
        let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
        let total: usize = counts.values().sum();
        eprintln!("{op:?}: {total} cells, max kind {max_kind:?}, counts {counts:?}");
        assert!(
            (max_kind <= CellKind::K3 || max_kind == CellKind::R1),
            "{op:?}: max kind {max_kind:?} > K3"
        );

        // Semantics on this op's samples (eval-level).
        for (i, s) in op_samples(op).iter().enumerate() {
            let got = eval_state(&format!("{op:?}[{i}]"), &iso, s, EVAL_FUEL);
            assert_eq!(
                got,
                reference(s),
                "{op:?}[{i}]: single-instruction semantics mismatch"
            );
        }
    }
}

#[test]
fn dispatch_exclusivity() {
    // A predicated instruction must be the identity on encodings of
    // OTHER opcodes (this is what makes the sequential dispatch a
    // Branch tree at the value level).
    let add = decoder::instruction(Op::Add);
    let sub_sample = &op_samples(Op::Sub)[0];
    assert_eq!(
        eval_state("add-on-sub", &add, sub_sample, EVAL_FUEL),
        *sub_sample
    );
    let addi_sample = &op_samples(Op::Addi)[0];
    assert_eq!(
        eval_state("add-on-addi", &add, addi_sample, EVAL_FUEL),
        *addi_sample
    );

    let swapbr = decoder::instruction(Op::Swapbr);
    let add_sample = &op_samples(Op::Add)[0];
    assert_eq!(
        eval_state("swapbr-on-add", &swapbr, add_sample, EVAL_FUEL),
        *add_sample
    );

    let rotli = decoder::instruction(Op::Rotli);
    let rotri_sample = &op_samples(Op::Rotri)[0];
    assert_eq!(
        eval_state("rotli-on-rotri", &rotli, rotri_sample, EVAL_FUEL),
        *rotri_sample
    );
}

// ══════════════════════════════════════════════════════════════════════
// 3. The full decoder: lower once, verify all samples
// ══════════════════════════════════════════════════════════════════════

/// Light context: just the decoder iso (no lowering or balancing).
/// Used by semantics-only tests to avoid the expensive balance() call.
fn light_ctx() -> &'static Iso {
    static CTX: OnceLock<Iso> = OnceLock::new();
    CTX.get_or_init(decoder::decode_execute)
}

/// Heavy context: iso + lowered + balanced netlist. Only triggered by
/// faithfulness tests that need simulate().
struct Ctx {
    iso: Iso,
    in_ty: Type,
    out_ty: Type,
    n_data_out: usize,
    balanced: PhasedCellNetlist,
    ancilla: Vec<bool>,
}

fn ctx() -> &'static Ctx {
    static CTX: OnceLock<Ctx> = OnceLock::new();
    CTX.get_or_init(|| {
        let t0 = std::time::Instant::now();
        let iso = decoder::decode_execute();
        let (in_ty, out_ty) = type_of(&iso).expect("decode_execute types");
        eprintln!("decode_execute: built + typed in {:?}", t0.elapsed());
        let t1 = std::time::Instant::now();
        let (net, ancilla) = lower_iso_with_ancillas(&iso).expect("decode_execute lowers");
        eprintln!("decode_execute: lowered in {:?}", t1.elapsed());
        assert_eq!(net.validate(), Ok(()), "decode_execute netlist invalid");

        let counts = net.cell_counts();
        let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
        let total: usize = counts.values().sum();
        eprintln!("decode_execute: {total} cells, max kind {max_kind:?}, counts {counts:?}");
        assert!(
            (max_kind <= CellKind::K3 || max_kind == CellKind::R1),
            "decode_execute: max cell kind {max_kind:?} exceeds K3"
        );

        let n_data = rail_width(&in_ty).unwrap();
        assert_eq!(net.inputs.len(), n_data + ancilla.len());

        let t2 = std::time::Instant::now();
        let balanced = balance(&net).expect("decode_execute balances");
        eprintln!("decode_execute: balanced in {:?}", t2.elapsed());
        assert_eq!(balanced.validate(), Ok(()), "balanced netlist invalid");
        assert!(
            balanced.tick_imbalances().unwrap().is_empty(),
            "balanced netlist has tick imbalances"
        );
        eprintln!(
            "decode_execute: balanced counts {:?}",
            balanced.cell_counts()
        );

        let n_data_out = rail_width(&out_ty).unwrap();
        Ctx {
            iso,
            in_ty,
            out_ty,
            n_data_out,
            balanced,
            ancilla,
        }
    })
}

/// Semantics-only check: core eval vs the Rust reference.
/// Uses light_ctx() to avoid triggering expensive balance().
fn check_semantics(name: &str, s: &StateVals) {
    let iso = light_ctx();
    let val = s.encode();
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, val.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));

    let got = StateVals::decode(&ev);
    let expected = reference(s);
    assert_eq!(
        got, expected,
        "{name}: decoder semantics mismatch\n  got:      {got:?}\n  expected: {expected:?}"
    );
    assert_eq!(got.ir, s.ir, "{name}: IR must be preserved");
}

/// Full three-way check on one sample: core eval vs the Rust reference
/// (semantics), and balanced-net simulate vs core eval (faithfulness).
fn check_full(name: &str, s: &StateVals) {
    let c = ctx();
    let val = s.encode();
    let mut fuel = EVAL_FUEL;
    let te = std::time::Instant::now();
    let ev = theseus_eval::eval(&c.iso, val.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    eprintln!("{name}: eval {:?}", te.elapsed());

    // Semantics.
    let got = StateVals::decode(&ev);
    let expected = reference(s);
    assert_eq!(
        got, expected,
        "{name}: decoder semantics mismatch\n  got:      {got:?}\n  expected: {expected:?}"
    );
    assert_eq!(got.ir, s.ir, "{name}: IR must be preserved");

    // Faithfulness: balanced simulate == core eval.
    let mut input_bits = bits_of(&val, &c.in_ty).unwrap();
    input_bits.extend_from_slice(&c.ancilla);
    let ts = std::time::Instant::now();
    let sim = simulate(&c.balanced, &input_bits)
        .unwrap_or_else(|e| panic!("{name}: simulate failed: {e:?}"));
    eprintln!("{name}: simulate {:?}", ts.elapsed());
    let expected_bits = bits_of(&ev, &c.out_ty).unwrap();
    assert_eq!(
        &sim.outputs[..c.n_data_out],
        &expected_bits[..],
        "{name}: balanced simulate disagrees with core eval"
    );
}

fn check_ops_semantics(ops: &[Op]) {
    for &op in ops {
        for (i, s) in op_samples(op).iter().enumerate() {
            check_semantics(&format!("full/{op:?}[{i}]"), s);
        }
    }
}

#[test]
fn full_decoder_arithmetic() {
    check_ops_semantics(&[Op::Add, Op::Sub, Op::Addi, Op::Subi]);
}

#[test]
fn full_decoder_logic() {
    check_ops_semantics(&[Op::Xor, Op::Xori]);
}

#[test]
fn full_decoder_rotates() {
    check_ops_semantics(&[Op::Rotl, Op::Rotr, Op::Rotli, Op::Rotri]);
}

#[test]
fn full_decoder_moves() {
    check_ops_semantics(&[Op::Exch, Op::Cswap, Op::Ccnot, Op::Xchg, Op::Swapbr]);
}

#[test]
fn full_decoder_reserved_opcodes_are_identity() {
    for (i, s) in reserved_samples().iter().enumerate() {
        assert_eq!(reference(s), *s, "reserved sample {i} sanity");
        check_semantics(&format!("reserved[{i}]"), s);
    }
}

// ══════════════════════════════════════════════════════════════════════
// A-0 BLOCKER regression tests -- MUST run THROUGH decode_execute (L20).
// These would have caught the two shipped BLOCKERs; the pre-A-0 fast gate
// never exercised decode_execute, which is why they slipped through.
// ══════════════════════════════════════════════════════════════════════

/// BLOCKER-2: a scratch operand (`Ccnot r0, r1, s2`) must execute against
/// s2 (scratch[2]), NOT r2 -- proving the reg∪scratch crossbar is wired
/// into the instruction, not just the orphaned `select_operand` iso.
/// Runs through the full `decode_execute` iso (`light_ctx`).
#[test]
fn blocker2_ccnot_uses_scratch_operand_through_decode_execute() {
    use theseus_tisc::asm::{encode, Instr};
    // Ccnot rd=r0, rs=r1, rt=s2 (operand index 10). r0 ^= r1 & s2.
    let ir = encode(Instr::Ccnot {
        rd: 0,
        rs: 1,
        rt: 10,
    })
    .expect("encode");
    // Distinct values so routing errors are visible: r2 != s2, and the
    // AND with r2 would give a DIFFERENT result than with s2.
    //   r0=0x00, r1=0xFF, r2=0x0F (the wrong operand), s2=0xAA (right).
    // Correct:  r0 ^= 0xFF & 0xAA = 0xAA.
    // If it wrongly used r2: r0 ^= 0xFF & 0x0F = 0x0F.
    let mut regs = [0u8; 8];
    regs[1] = 0xFF;
    regs[2] = 0x0F; // the WRONG operand -- must be ignored
    let s = StateVals {
        regs,
        pc: 0,
        br: 0,
        ir,
        // s0=s1=0 (decoder convention); s2 = 0xAA is the intended operand.
        scratch: [0x00, 0x00, 0xAA, 0, 0, 0, 0, 0],
        mem: [0; MEM_WORDS],
    };
    let got = eval_state("blocker2/ccnot-s2", light_ctx(), &s, EVAL_FUEL);
    assert_eq!(
        got.regs[0], 0xAA,
        "Ccnot r0,r1,s2 must use s2 (0xAA) → r0=0xAA; got {:#04x} \
         (0x0F would mean it wrongly used r2)",
        got.regs[0]
    );
    // Operands and IR otherwise preserved (Ccnot leaves rs, rt intact).
    assert_eq!(got.regs[1], 0xFF, "r1 (rs) preserved");
    assert_eq!(got.scratch[2], 0xAA, "s2 (rt) preserved");
    assert_eq!(got.ir, ir, "IR preserved");
    // s0/s1 restored to 0.
    assert_eq!(got.scratch[0], 0, "s0 restored");
    assert_eq!(got.scratch[1], 0, "s1 restored");
}

/// BLOCKER-1: an odd-immediate I-op (`Addi r7, #1`) must NOT fire XCHG's
/// gated swap. Pre-fix, XCHG keyed off IR bit 0 (= the imm LSB post-A-0),
/// so any odd imm spuriously swapped r[rd&7] ↔ s0 before the add. Runs
/// through the full `decode_execute` iso.
#[test]
fn blocker1_odd_imm_iop_does_not_fire_xchg_through_decode_execute() {
    use theseus_tisc::asm::{encode, Instr};
    let ir = encode(Instr::Addi { rd: 7, imm: 0x01 }).expect("encode");
    let mut regs = [0u8; 8];
    regs[7] = 0x87; // arbitrary; expect +1 = 0x88
                    // s0/s1 are the decoder-reserved operand buffer + predicate word; they
                    // MUST be 0 on decode_execute entry (the CPU maintains this between
                    // steps -- the adder uses s0 as its operand slot). Put the anti-XCHG
                    // sentinels in the NON-reserved words s2..s7: a spurious XCHG (the
                    // BLOCKER-1 bug) would swap r7 into scratch, dirtying one / leaving r7
                    // wrong; a clean Addi preserves all scratch.
    let scratch0 = [0x00, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66];
    let s = StateVals {
        regs,
        pc: 0,
        br: 0,
        ir,
        scratch: scratch0,
        mem: [0; MEM_WORDS],
    };
    let got = eval_state("blocker1/addi-odd", light_ctx(), &s, EVAL_FUEL);
    assert_eq!(got.regs[7], 0x88, "Addi r7,#1: r7 must be 0x87+1=0x88");
    assert_eq!(
        got.scratch, scratch0,
        "scratch must be untouched -- a change means XCHG spuriously fired"
    );
    // Every other reg unchanged.
    for i in 0..7 {
        assert_eq!(got.regs[i], 0, "r{i} must be untouched by Addi r7");
    }
    assert_eq!(got.ir, ir, "IR preserved");
}

/// BLOCKER-1 (broader): an odd-imm op on a HIGH register (rd&7 collision)
/// must still not swap. `Xori r5, #0x0B` (odd) -- the pre-fix swap would
/// have hit r5 ↔ s0.
#[test]
fn blocker1_odd_imm_xori_clean_through_decode_execute() {
    use theseus_tisc::asm::{encode, Instr};
    let ir = encode(Instr::Xori { rd: 5, imm: 0x0B }).expect("encode");
    let mut regs = [0u8; 8];
    regs[5] = 0xF0;
    // s0/s1 reserved = 0 on entry; sentinels in non-reserved s2..s7.
    let scratch0 = [0x00, 0x00, 0x2A, 0x3B, 0x4C, 0x5D, 0x6E, 0x7F];
    let s = StateVals {
        regs,
        pc: 0,
        br: 0,
        ir,
        scratch: scratch0,
        mem: [0; MEM_WORDS],
    };
    let got = eval_state("blocker1/xori-odd", light_ctx(), &s, EVAL_FUEL);
    assert_eq!(got.regs[5], 0xF0 ^ 0x0B, "Xori r5,#0x0B");
    assert_eq!(
        got.scratch, scratch0,
        "scratch untouched (XCHG did not fire)"
    );
    assert_eq!(got.ir, ir, "IR preserved");
}

// ══════════════════════════════════════════════════════════════════════
// B1.2 -- whole-domain per-opcode equivalence: the UNIFIED reg+imm block
// (`decode_execute`) == the replicated per-op `instruction(op)`, for EVERY
// (rd-value, 2nd-operand-value) pair, for all 11 reg+imm ops. This is the
// §2.3 B-1 gate obligation: the shared-ALU output must match the current
// per-op output over the whole byte datapath domain (256×256 per op = the
// full range of the adder/rotator/xor cores, both operands). Both isos are
// evaluated through `theseus_eval::eval`; the unified block is the wired
// `decode_execute`, the reference is the standalone `instruction(op)`.
//
// This test would FAIL if: the fmt-select A-source routed the wrong slot
// (R rs-fetch not inert for I, or imm not landing in A), a config line
// co-fired with another, the adder/rotator adjoint (Subi/Rotri) was wired
// to the wrong config line, or an off-opcode block perturbed the state.

/// Fixed register indices for the whole-domain sweep. rd and rs are
/// DISTINCT (no aliasing) and both are mid-file registers so the crossbar
/// exercises non-trivial routing (rd=r3, rs=r5 for R-format; rd=r3 for I).
const WD_RD: u32 = 3;
const WD_RS: u32 = 5;

/// Build the base state with rd=`rd_val`, rs=`rs_val`, everything else the
/// standard sample (s0=s1=0). The `ir` selects the op under test.
fn wd_state(ir: u32, rd_val: u8, rs_val: u8) -> StateVals {
    // Non-trivial, distinct background so a mis-route is visible.
    let mut regs = [0x13, 0x24, 0x35, 0x46, 0x57, 0x68, 0x79, 0x8A];
    regs[WD_RD as usize] = rd_val;
    regs[WD_RS as usize] = rs_val;
    StateVals {
        regs,
        pc: 0x42,
        br: 0x99,
        ir,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
        mem: [0; MEM_WORDS],
    }
}

/// Hoisted isolated UNIVERSAL shared block (built once). B1.3: this is the
/// `shared_alu_block` (reg+imm + Ccnot + Cswap); the other decode blocks
/// (xchg/swapbr) are the identity for these opcodes, so the isolated block
/// == `instruction(op)` is a valid whole-domain equivalence.
#[cfg(feature = "measurement")]
fn wd_unified_ctx() -> &'static Iso {
    static CTX: OnceLock<Iso> = OnceLock::new();
    CTX.get_or_init(decoder::shared_alu_block_for_measurement)
}

// ── RETIRED (GO-rollout) → tests/decoder_bveq.rs ────────────────────
// `b12_whole_domain_equiv_arith` / `b12_whole_domain_equiv_logic_rot`
// (via `wd_equiv_op`, ~576 simulated (rd, op2) pairs/op ≈ 1 min/op) proved the
// shared reg+imm ALU block ≡ `instruction(op)` by SIMULATION. RETIRED into
// `decoder_bveq::decoder_bveq_whole_domain`, which walks the WIRED
// `decode_execute` SYMBOLICALLY (concrete IR word, symbolic operands) and
// proves each opcode ≡ the INDEPENDENT `bveq::instr_semantics` over the WHOLE
// 2^8-per-operand domain in ~0.5s/op -- a strict superset of the sample grid
// (whole domain, not 576 pts) AND checked against an independent oracle rather
// than `instruction(op)` (so a mis-dispatch cannot hide). ~53 min → ~24 s.

/// Config mutual-exclusion + fmt A-source discrimination: for every opcode
/// word (R and I families, all 4/3 low opcode bits, both fmt values), the
/// unified block must equal the standalone `instruction` of whatever op
/// that word denotes -- and reserved words must be the identity. This
/// catches config co-fire (two lines set) and fmt mis-gating (R rs-fetch
/// firing under I-format, or imm→A firing under R-format) as a state
/// perturbation, using ONE representative (rd,operand) per opcode word.
#[test]
fn b12_config_exclusion_and_fmt_gate() {
    let unified = light_ctx();
    // R-format: opcodes 0..16 (only 0..9 + 0b1011 are live; rest reserved).
    for opc in 0u32..16 {
        let ir = r_ins(opc, WD_RD, WD_RS, 2);
        let s = wd_state(ir, 0xC7, 0x3E);
        let got = eval_state("cfg/R", unified, &s, EVAL_FUEL);
        assert_eq!(
            got,
            reference(&s),
            "R opcode {opc:#06b}: unified != reference (config co-fire / fmt mis-gate?)"
        );
    }
    // I-format: opcodes 0..8 (0..5 live, rest reserved) × a couple imm values.
    for opc in 0u32..8 {
        for imm in [0x00u8, 0x9D, 0xFF] {
            let ir = i_ins(opc, WD_RD, imm);
            let s = wd_state(ir, 0xC7, 0x3E);
            let got = eval_state("cfg/I", unified, &s, EVAL_FUEL);
            assert_eq!(
                got,
                reference(&s),
                "I opcode {opc:#05b} imm={imm:#04x}: unified != reference"
            );
        }
    }
}

/// MAJOR-2 tooth (B1.2 new claim): the sel_rs crossbar is INERT for
/// I-format even when the immediate's "steer" bits (IR bits 15..19, which
/// under fmt=1 drive the R-format rs crossbar) are NONZERO. Under B1.2 the
/// rs swap-fetch is fmt-gated (`fetch_rs_fmt_gated`): for I-format (fmt=0)
/// the r0-slot↔s0 word swap carries fmt as a control, so it is the
/// identity -- the crossbar STILL permutes registers on the way in and back
/// out (steered by whatever bits sit in the rs field, here the immediate's
/// high bits), but the bracket is a pure self-inverse permutation and
/// NOTHING is fetched. This test drives those steer bits to nonzero
/// patterns against a nonzero register/scratch background so that any
/// spurious (non-cancelling) crossbar action would corrupt an observable
/// reg or scratch word. Expected: only r[rd] changes per the immediate op;
/// every other reg, all scratch, BR, PC, IR bit-identical to entry.
#[test]
fn b12_iformat_nonzero_immediate_steer_inert() {
    let unified = light_ctx();

    // Distinct sentinels in every reg and in s2..s7 (s0=s1=0 per
    // decoder convention) so any spurious crossbar swap is observable.
    let bg = |ir: u32| StateVals {
        regs: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88],
        pc: 0x42,
        br: 0x99,
        ir,
        scratch: [0x00, 0x00, 0xC2, 0xD3, 0xE4, 0xF5, 0x1A, 0x2B],
        mem: [0; MEM_WORDS],
    };

    // Case 1 (the review's exact word): Addi rd=r3, imm low = 0x34, with the
    // rs-field steer bits 15..18 = 0xF (all crossbar levels incl.
    // reg↔scratch would fire if the fetch were not fmt-gated).
    // fmt bit 31 = 0 (I-format); opcode 0b000 = ADDI.
    let ir1: u32 = (0b000 << 25) | (3 << 20) | (0xF << 15) | 0x34;
    let s1 = bg(ir1);
    let got1 = eval_state("iformat-steer/addi", unified, &s1, EVAL_FUEL);
    let mut exp1 = s1.clone();
    exp1.regs[3] = exp1.regs[3].wrapping_add(0x34); // r3 += 0x34 (mod 256)
    assert_eq!(
        got1, exp1,
        "I-format Addi with nonzero rs-steer bits (0xF<<15): only r3 += 0x34; \
         all other regs / scratch / BR / PC / IR must be bit-identical\n \
         got={got1:?}\n exp={exp1:?}"
    );
    // Belt-and-suspenders: the reference model agrees (imm = ir & 0xFF).
    assert_eq!(
        got1,
        reference(&s1),
        "iformat-steer/addi: != reference model"
    );

    // Case 2: Xori rd=r3, imm low = 0x6C, different steer pattern
    // bits 15..19 = 0b10101 (0x15<<15) to exercise the xor path with a
    // distinct crossbar-steer bit pattern.
    let ir2: u32 = (0b010 << 25) | (3 << 20) | (0b10101 << 15) | 0x6C;
    let s2 = bg(ir2);
    let got2 = eval_state("iformat-steer/xori", unified, &s2, EVAL_FUEL);
    let mut exp2 = s2.clone();
    exp2.regs[3] ^= 0x6C; // r3 ^= 0x6C
    assert_eq!(
        got2, exp2,
        "I-format Xori with nonzero rs-steer bits (0b10101<<15): only r3 ^= 0x6C; \
         everything else bit-identical\n got={got2:?}\n exp={exp2:?}"
    );
    assert_eq!(
        got2,
        reference(&s2),
        "iformat-steer/xori: != reference model"
    );
}

// ══════════════════════════════════════════════════════════════════════
// B1.3 -- the universal 3-operand bracket folds Ccnot + Cswap into the
// shared block. These teeth prove: (a) the re-bracketed reg+imm route is
// still exact (covered by the b12 whole-domain equiv above, now evaluated
// through `shared_alu_block`); (b) Ccnot (byte-parallel rd^=rs&rt) and
// Cswap (conditional swap gated rd[0], both polarities) match the reference
// `instruction(op)` over the operand domain via the SAME bracket; (c) s0 is
// restored to 0 after decode (the L15 imm_to_a precondition); (d) the rt→B
// fetch is inert for reg+imm ops (config-gated off).
// ══════════════════════════════════════════════════════════════════════

// ── RETIRED (GO-rollout) → tests/decoder_bveq.rs ────────────────────
// `b13_whole_domain_equiv_ccnot` / `_cswap` (via `wd_equiv_3op`, 40 simulated
// register-value cases/op) proved the 3-operand shared block ≡
// `instruction(op)` by SIMULATION. RETIRED into
// `decoder_bveq::decoder_bveq_whole_domain`: Ccnot / Cswap are walked with
// concrete IR words (multiple selector triples) and symbolic operand VALUES,
// proved ≡ the INDEPENDENT `bveq::instr_semantics` over the WHOLE operand
// domain -- a strict superset of the 40-case grid (Cswap both polarities +
// Ccnot's full AND product are all inside a single symbolic walk). The
// SCRATCH-operand routing tooth (`b13_cswap_scratch_operand_equiv`, below) and
// `blocker2_ccnot_uses_scratch_operand` remain, since instr_semantics has no
// scratch-frame; they stay as fast concrete teeth.

/// B1.3 (review Unit-2 MINOR-3): Cswap with a SCRATCH operand (rt=s2, idx 10)
/// through the universal bracket == `instruction(Op::Cswap)` -- closes the
/// coverage gap (Ccnot-scratch is covered by `blocker2_ccnot_uses_scratch_
/// operand`, but Cswap routing an operand into the shared B-slot via the
/// reg∪scratch crossbar had no direct tooth). Both control polarities.
#[cfg(feature = "measurement")]
#[test]
fn b13_cswap_scratch_operand_equiv() {
    let unified = wd_unified_ctx();
    let refer = decoder::instruction(Op::Cswap);
    // Cswap rd=r3, rs=r5, rt=s2 (operand idx 10): swap r5 ↔ s2 iff r3[0].
    let ir = r_ins(0b0110, 3, 5, 10);
    for (rd_val, note) in [
        (0x01u8, "taken"),
        (0x00u8, "not-taken"),
        (0xFFu8, "taken"),
        (0x02u8, "not-taken"),
    ] {
        let mut regs = [0x13, 0x24, 0x35, 0x46, 0x57, 0x68, 0x79, 0x8A];
        regs[3] = rd_val;
        regs[5] = 0xC3; // rs value
        let s = StateVals {
            regs,
            pc: 0x42,
            br: 0x99,
            ir,
            // s0=s1=0 (decoder convention); s2=0x3C is the routed operand.
            scratch: [0x00, 0x00, 0x3C, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
            mem: [0; MEM_WORDS],
        };
        let g_uni = eval_state("cswap-s2/unified", unified, &s, EVAL_FUEL);
        let g_ref = eval_state("cswap-s2/ref", &refer, &s, EVAL_FUEL);
        assert_eq!(
            g_uni, g_ref,
            "Cswap rd=r3({rd_val:#04x} {note}) rs=r5 rt=s2 mismatch\n uni={g_uni:?}\n ref={g_ref:?}"
        );
    }
}

// ── RETIRED (GO-rollout) → tests/decoder_bveq.rs ────────────────────
// `b13_ccnot_full_rs_sweep` simulated 4096 pts (256 rs-values × 4 rd × 4 rt)
// of the Ccnot lane against `instruction(op)` -- the single biggest decoder-sim
// hog. RETIRED: `decoder_bveq::decoder_bveq_whole_domain` proves Ccnot's
// r[rd]^=r[rs]&r[rt] over ALL 2^8 operand values symbolically in one walk
// (every AND lane + xor target simultaneously), against instr_semantics.

/// B1.3 s0-restoration tooth: after a full decode+execute of an op that
/// uses the s0-workspace decode, s0 (the decoder-reserved operand buffer)
/// must be exactly 0 -- the L15-critical invariant `imm_to_a` requires
/// (a dirty s0 would corrupt the NEXT instruction's immediate). Checked for
/// every op class (reg+imm + Ccnot + Cswap) through the wired decoder.
#[test]
fn b13_s0_restored_to_zero() {
    let dec = light_ctx();
    // (name, IR word) -- one per op class, all through the universal bracket.
    let cases: &[(&str, u32)] = &[
        ("Add", r_ins(0b0000, 3, 5, 0)),
        ("Sub", r_ins(0b0001, 2, 4, 0)),
        ("Xor", r_ins(0b0010, 1, 6, 0)),
        ("Exch", r_ins(0b0011, 0, 7, 0)),
        ("Rotl", r_ins(0b0100, 3, 1, 0)),
        ("Cswap", r_ins(0b0110, 4, 1, 2)),
        ("Ccnot", r_ins(0b1011, 4, 1, 2)),
        ("Addi", i_ins(0b000, 3, 0x9D)),
        ("Xori", i_ins(0b010, 2, 0xAA)),
        ("Rotli", i_ins(0b011, 5, 0x03)),
    ];
    for &(name, ir) in cases {
        // Nonzero background everywhere EXCEPT s0/s1 (=0 per convention).
        let s = StateVals {
            regs: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88],
            pc: 0x42,
            br: 0x99,
            ir,
            scratch: [0x00, 0x00, 0xC2, 0xD3, 0xE4, 0xF5, 0x1A, 0x2B],
            mem: [0; MEM_WORDS],
        };
        let got = eval_state(&format!("s0-restore/{name}"), dec, &s, EVAL_FUEL);
        assert_eq!(
            got.scratch[0], 0,
            "{name}: s0 must be restored to 0 after decode (L15 imm_to_a precondition); got {:#04x}",
            got.scratch[0]
        );
        assert_eq!(got.scratch[1], 0, "{name}: s1 must be restored to 0");
        // The whole state must also match the reference (belt-and-suspenders).
        assert_eq!(got, reference(&s), "{name}: != reference model");
    }
}

/// B1.3 rt→B inert-for-reg+imm tooth (extends the b12 inert-crossbar tooth
/// to the rt→B fetch): an Addi / Ccnot word with a NONZERO rt-field must be
/// bit-identical to the same word with rt-field zeroed EXCEPT the intended
/// target. For reg+imm the rt→B fetch is config-gated OFF, so a nonzero rt
/// field must not perturb any register/BR (the classic "rt-field=0 aliases
/// r0 into BR" corruption this gating fixes).
#[test]
fn b13_rt_field_inert_for_reg_imm() {
    let dec = light_ctx();
    // Addi rd=r3, imm=0x34, with the rt operand field (bits 10..14) = 0x1F
    // (all ones) -- under an ungated rt→B fetch this would route r7-ish into
    // BR and corrupt state. Config-gated OFF for Addi ⇒ must be inert.
    let ir_rt: u32 = (0b000 << 25) | (3 << 20) | (0x1F << 10) | 0x34; // I-format (fmt=0)
    let s = StateVals {
        regs: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88],
        pc: 0x42,
        br: 0x99,
        ir: ir_rt,
        scratch: [0x00, 0x00, 0xC2, 0xD3, 0xE4, 0xF5, 0x1A, 0x2B],
        mem: [0; MEM_WORDS],
    };
    let got = eval_state("rt-inert/addi", dec, &s, EVAL_FUEL);
    let mut exp = s.clone();
    exp.regs[3] = exp.regs[3].wrapping_add(0x34);
    assert_eq!(
        got, exp,
        "Addi with nonzero rt-field must be inert on BR/regs (rt→B config-gated off): \
         only r3 += 0x34; BR/other regs/scratch unchanged\n got={got:?}\n exp={exp:?}"
    );

    // R-format Add rd=r2, rs=r4, with nonzero rt-field = 0x1F. Add's rt→B is
    // gated off; BR must be untouched.
    let ir_r: u32 = (1 << 31) | (0b0000 << 25) | (2 << 20) | (4 << 15) | (0x1F << 10);
    let s2 = StateVals {
        ir: ir_r,
        ..s.clone()
    };
    let got2 = eval_state("rt-inert/add", dec, &s2, EVAL_FUEL);
    let mut exp2 = s2.clone();
    exp2.regs[2] = exp2.regs[2].wrapping_add(exp2.regs[4]);
    assert_eq!(
        got2, exp2,
        "Add with nonzero rt-field must leave BR/other regs untouched (rt→B gated off)"
    );
    assert_eq!(
        got2.br, 0x99,
        "BR must be untouched by an Add with nonzero rt-field"
    );
}

/// MINOR-4 pin: op4/op5 (the two HIGH opcode bits) are DON'T-CARE at the
/// datapath (the predicates test only the low 4 R / low 3 I opcode bits --
/// see decoder.rs module header, "Reserved opcodes"). This pins that
/// documented blind-spot behavior literal-for-literal against the OLD
/// per-op decode (`instruction(op)`): an opcode with a high bit set aliases
/// the live op with the same low bits, and must decode/execute identically.
/// A future change (e.g. widening the predicate to test the high bits)
/// cannot silently alter this without failing this pin.
#[test]
fn b12_high_opcode_bits_dontcare_matches_reference() {
    let unified = light_ctx();

    // R-family: opcode 0b110000 (high bits 5,4 set) low bits = 0b0000 = Add.
    // Must execute exactly as instruction(Op::Add) on a nonzero state.
    let ir_r = r_ins(0b110000, WD_RD, WD_RS, 0);
    let s_r = wd_state(ir_r, 0xC7, 0x3E);
    let got_r = eval_state("hi-opc/R-add-alias", unified, &s_r, EVAL_FUEL);
    let ref_add = decoder::instruction(Op::Add);
    let exp_r = eval_state("hi-opc/R-add-ref", &ref_add, &s_r, EVAL_FUEL);
    assert_eq!(
        got_r, exp_r,
        "R opcode 0b110000 (high bits set, low=Add) must decode/execute as \
         instruction(Op::Add) literal-for-literal"
    );

    // I-family: opcode 0b111000 (high bits set) low 3 bits = 0b000 = Addi.
    // fmt=0 (I-format); i_ins sets fmt=0 and shifts opc<<25.
    let ir_i = i_ins(0b111000, WD_RD, 0x5B);
    let s_i = wd_state(ir_i, 0xC7, 0x3E);
    let got_i = eval_state("hi-opc/I-addi-alias", unified, &s_i, EVAL_FUEL);
    let ref_addi = decoder::instruction(Op::Addi);
    let exp_i = eval_state("hi-opc/I-addi-ref", &ref_addi, &s_i, EVAL_FUEL);
    assert_eq!(
        got_i, exp_i,
        "I opcode 0b111000 (high bits set, low=Addi) must decode/execute as \
         instruction(Op::Addi) literal-for-literal"
    );
}

/// Smoke faithfulness: one sample per opcode class to verify the
/// balance path. Ignored by default -- balance() alone takes ~50s.
/// Per-instruction lowering (test 2) covers structural correctness;
/// this adds simulate-level faithfulness.
#[test]
#[ignore = "authoritative balanced-simulate faithfulness; run via --ignored"]
fn full_decoder_faithfulness_smoke() {
    // Op::Add[0] + one reserved -- covers the balance path.
    check_full("faithfulness_smoke/Add[0]", &op_samples(Op::Add)[0]);
    check_full("faithfulness_smoke/reserved[0]", &reserved_samples()[0]);
}

#[test]
#[ignore = "authoritative balanced-simulate faithfulness; run via --ignored"]
fn full_decoder_faithfulness_all() {
    for &op in &Op::ALL {
        for (i, s) in op_samples(op).iter().enumerate() {
            check_full(&format!("full/{op:?}[{i}]"), s);
        }
    }
    for (i, s) in reserved_samples().iter().enumerate() {
        check_full(&format!("reserved[{i}]"), s);
    }
}
