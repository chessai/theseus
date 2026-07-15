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
//! source-level legality checker's job, P1 — see decoder.rs docs).

use std::sync::OnceLock;

use theseus_core::{type_of, Iso, Type};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{
    balance, bits_of, lower_iso, rail_width, simulate, CellKind, PhasedCellNetlist,
};
use theseus_tisc::decoder::{self, Op};
use theseus_tisc::types::{MEM_WORDS, StateVals};

const EVAL_FUEL: u64 = 4_000_000_000;

// ── Instruction encoders (F1 spec §3) ──────────────────────────────

fn r_ins(opcode: u16, rd: u16, rs: u16, rt: u16) -> u16 {
    assert!(opcode < 16 && rd < 8 && rs < 8 && rt < 8);
    (1 << 15) | (opcode << 11) | (rd << 8) | (rs << 5) | (rt << 2)
}

fn i_ins(opc: u16, rd: u16, imm: u8) -> u16 {
    assert!(opc < 8 && rd < 8);
    (opc << 12) | (rd << 9) | (u16::from(imm) << 1)
}

// ── Reference model of the ISA ─────────────────────────────────────

fn reference(s: &StateVals) -> StateVals {
    let mut out = s.clone();
    let ir = s.ir;
    if ir & 0x8000 != 0 {
        let opcode = (ir >> 11) & 0xF;
        let rd = ((ir >> 8) & 7) as usize;
        let rs = ((ir >> 5) & 7) as usize;
        let rt = ((ir >> 2) & 7) as usize;
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
            0b1000 => std::mem::swap(&mut out.regs[rd], &mut out.br),
            0b1001 if rt < MEM_WORDS => std::mem::swap(&mut out.regs[rd], &mut out.mem[rt]),
            _ => {} // reserved: identity
        }
    } else {
        let opc = (ir >> 12) & 7;
        let rd = ((ir >> 9) & 7) as usize;
        let imm = ((ir >> 1) & 0xFF) as u8;
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

fn st(regs: [u8; 8], ir: u16) -> StateVals {
    let mut mem = [0u8; MEM_WORDS];
    // Fill memory with non-zero test values so exchange is visible.
    for (i, m) in mem.iter_mut().enumerate() {
        *m = 0xA0 + i as u8;
    }
    StateVals {
        regs,
        pc: 0x42,
        br: 0x99,
        ir,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
        mem,
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
        Op::Mexch => {
            let mut samples = vec![
                st(regs_a(), r_ins(9, 0, 0, 0)), // MEXCH r0, m0
                st(regs_b(), r_ins(9, 5, 0, 1)), // MEXCH r5, m1
            ];
            if MEM_WORDS > 2 {
                samples.push(st(regs_a(), r_ins(9, 3, 0, 3))); // MEXCH r3, m3
            }
            samples
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
        st(regs_a(), r_ins(10, 1, 2, 3)), // opcode 9 is now MEXCH
        st(regs_b(), r_ins(15, 7, 6, 5)),
        st(regs_a(), i_ins(5, 3, 0x7C)),
        st(regs_b(), i_ins(7, 0, 0xFF)),
    ]
}

// ── Eval helper ────────────────────────────────────────────────────

fn eval_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

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
    for rd in 0..8u16 {
        let s = st(regs_b(), r_ins(0, rd, 0, 0)); // R_RD reads IR bits 10-8
        let got = eval_state("select_reg", &iso, &s);
        let mut expected = s.clone();
        decoder::crossbar_model(rd as usize, &mut expected.regs);
        assert_eq!(got, expected, "select_reg: rd={rd}");
        assert_eq!(
            got.regs[0],
            s.regs[rd as usize],
            "select_reg: selected register must land at slot 0 (rd={rd})"
        );
    }

    // Round trip: select ; unselect == identity.
    let rt = Iso::seq(iso.clone(), Iso::sym(iso));
    let s = st(regs_a(), r_ins(0, 5, 0, 0));
    assert_eq!(eval_state("select_reg roundtrip", &rt, &s), s);
}

#[test]
fn select_scratch_crossbar_isolated() {
    let iso = decoder::select_scratch(decoder::R_RT);
    for sx in 0..8u16 {
        let s = st(regs_b(), r_ins(0, 0, 0, sx)); // R_RT reads IR bits 4-2
        let got = eval_state("select_scratch", &iso, &s);
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
        assert!((max_kind <= CellKind::K3 || max_kind == CellKind::R1), "{op:?}: max kind {max_kind:?} > K3");

        // Semantics on this op's samples (eval-level).
        for (i, s) in op_samples(op).iter().enumerate() {
            let got = eval_state(&format!("{op:?}[{i}]"), &iso, s);
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
    assert_eq!(eval_state("add-on-sub", &add, sub_sample), *sub_sample);
    let addi_sample = &op_samples(Op::Addi)[0];
    assert_eq!(eval_state("add-on-addi", &add, addi_sample), *addi_sample);

    let swapbr = decoder::instruction(Op::Swapbr);
    let add_sample = &op_samples(Op::Add)[0];
    assert_eq!(eval_state("swapbr-on-add", &swapbr, add_sample), *add_sample);

    let rotli = decoder::instruction(Op::Rotli);
    let rotri_sample = &op_samples(Op::Rotri)[0];
    assert_eq!(eval_state("rotli-on-rotri", &rotli, rotri_sample), *rotri_sample);
}

// ══════════════════════════════════════════════════════════════════════
// 3. The full decoder: lower once, verify all samples
// ══════════════════════════════════════════════════════════════════════

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
        Ctx { iso, in_ty, out_ty, n_data_out, balanced, ancilla }
    })
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

fn check_ops(ops: &[Op]) {
    for &op in ops {
        for (i, s) in op_samples(op).iter().enumerate() {
            check_full(&format!("full/{op:?}[{i}]"), s);
        }
    }
}

#[test]
fn full_decoder_arithmetic() {
    check_ops(&[Op::Add, Op::Sub, Op::Addi, Op::Subi]);
}

#[test]
fn full_decoder_logic() {
    check_ops(&[Op::Xor, Op::Xori]);
}

#[test]
fn full_decoder_rotates() {
    check_ops(&[Op::Rotl, Op::Rotr, Op::Rotli, Op::Rotri]);
}

#[test]
fn full_decoder_moves() {
    check_ops(&[Op::Exch, Op::Cswap, Op::Xchg, Op::Swapbr]);
}

#[test]
fn full_decoder_reserved_opcodes_are_identity() {
    for (i, s) in reserved_samples().iter().enumerate() {
        // reference() is the identity on reserved opcodes, so
        // check_full both asserts identity and sim-faithfulness.
        assert_eq!(reference(s), *s, "reserved sample {i} sanity");
        check_full(&format!("reserved[{i}]"), s);
    }
}
