//! TISC-1 decoder: `decode_execute : State <-> State`.
//!
//! Reads the instruction word held in IR and applies the encoded
//! instruction to the registers it names, leaving IR intact. Both the
//! opcode dispatch and the register selection are *runtime data* (IR
//! bits), so the construction uses two data-dependent mechanisms:
//!
//! 1. **Register selection (the crossbar).** The 3-bit rd/rs/rt fields
//!    of IR act as controls on a 3-level tree of controlled word-swaps
//!    (K1 Fredkin cascades, the control threading through each cell with
//!    no fanout — the same pattern as the CSWAP cascade / barrel rotate
//!    in `tisc_controlled.rs`). Level 2 (field bit 2) swaps r_i ↔ r_{i+4},
//!    level 1 swaps r0↔r2, r1↔r3, level 0 swaps r0↔r1; afterwards
//!    `r[field]` sits at the fixed slot r0. The op is applied at the
//!    fixed slot and the adjoint crossbar routes everything back.
//!
//! 2. **Opcode dispatch (the Branch tree, P1).** Rather than nesting
//!    `Plus` arms (which promotes every cell once per branching level
//!    and would blow the K3 budget five levels deep), each instruction
//!    computes a one-bit *predicate* — the AND of its opcode-bit
//!    literals, including IR bit 15 (R/I format) — into an aux scratch
//!    bit via a Toffoli chain, and only the instruction's core
//!    state-changing gates carry that predicate as an extra control.
//!    All routing brackets (crossbars, operand fetch, immediate masks)
//!    are unconditional and self-cancelling, so when the predicate is
//!    low the whole instruction is the identity. The 14 predicated
//!    instructions run in sequence; opcodes are mutually exclusive, so
//!    at most one fires.
//!
//!    **Reserved opcodes** (R 1001–1111 except none live there, and
//!    I 101–111) match no predicate, so `decode_execute` is the
//!    identity on them at the iso level. Source-level ISA totality
//!    (rejecting reserved opcodes as compile errors) is the coverage
//!    checker's job (P1) and is out of scope here.
//!
//! ## Conventions and caveats (documented, enforced by the P1 checker
//! at source level, not by this iso)
//!
//! - **s0 (scratch 0) is the operand/immediate buffer.** Register
//!   second operands are *swap-fetched* into s0 (a zero-cell field swap
//!   bracketed by the rs crossbar), which is exact for any s0 content.
//!   ADDI/SUBI additionally require **s0 = 0 on entry** (the immediate
//!   is CNOT-masked onto s0; the TK adder adds `s0 ⊕ imm`). Same
//!   convention as `instr::addi`.
//! - **s1 (scratch 1) holds the predicate bit (bit 0), the Toffoli
//!   chain intermediates (bits 1–3), and CSWAP's control-bit copy
//!   (bit 4); it must be 0 on entry** and is restored to 0 on exit.
//! - **XCHG** routes the scratch file through a second crossbar, which
//!   scrambles s1's position, so its gate control is first copied onto
//!   the (architecturally zero) R-format pad bit IR[0] and removed
//!   afterwards; IR is preserved. XCHG naming s1 (`sx = 1`) exchanges
//!   into the live predicate word: the result is still a bijection but
//!   its semantics are unspecified — the P1 checker should reject
//!   programs that name decoder-reserved scratch.
//! - **Register aliasing** (`rd == rs`, or CSWAP/XCHG operand overlap)
//!   is not implementable as specified (e.g. `ADD rd, rd` is `rd ← 2·rd`,
//!   which is not injective). Every construction here remains a
//!   bijection on the full State under aliasing, but the semantics
//!   deviate (typically the target is preserved and s0 is trashed).
//!   The P1/legality checker must forbid aliased operands at source
//!   level.
//!
//! Everything lowers to cells of kind ≤ K2/K3: the crossbars are K1
//! Fredkins, predicated swaps are native K2 (4-wire fast path), and
//! predicated NOT-style gates (Toffoli / CCC-NOT) synthesize to K1/K2
//! cells via the B4.2 AND-reduction.

use theseus_core::{Iso, Type};
use theseus_netlist::{prim_from_bits, CellKind};

use crate::addressing::at_fields;
use crate::plumbing::{
    at_wire, flatten, fredkin_prim, gate_at, not_iso, rot_transpositions, seq_balanced, seq_list,
};
use crate::types::{
    flat, reg_field, scratch_field, word8, BR_FIELD, IR_HI_FIELD, IR_LO_FIELD, W,
};

// ── Aux-field conventions ──────────────────────────────────────────

/// State field used as the operand/immediate buffer (s0).
pub const AUX_OPERAND_FIELD: usize = 12;
/// State field holding the predicate bit + chain intermediates (s1).
pub const AUX_PRED_FIELD: usize = 13;

// Bit-level plumbing (not_iso, flatten, gate_at, seq_balanced, seq_list,
// at_wire, fredkin_prim, rot_transpositions, …) lives in `crate::plumbing`
// and is imported above. The convention-bearing prims below stay local.

// ══════════════════════════════════════════════════════════════════════
// Prims (all ≤ 4 wires; lower to K1/K2 cells)
// ══════════════════════════════════════════════════════════════════════

/// Gated Fredkin `(c1, c2, a, b)`: swap a,b iff c1∧c2. K2 fast path.
fn gated_fredkin_prim() -> Iso {
    let t4 = flat(4);
    Iso::Prim(Box::new(
        prim_from_bits("gfredkin", t4.clone(), t4, |b| CellKind::K2.eval(b)).expect("gfredkin"),
    ))
}

/// CNOT `(c, t)`: t ^= c.
pub(crate) fn cnot_prim() -> Iso {
    let t2 = flat(2);
    Iso::Prim(Box::new(
        prim_from_bits("dec_cnot", t2.clone(), t2, |b| vec![b[0], b[1] ^ b[0]]).expect("cnot"),
    ))
}

/// Toffoli `(c1, c2, t)`: t ^= c1∧c2.
pub(crate) fn toffoli_prim() -> Iso {
    let t3 = flat(3);
    Iso::Prim(Box::new(
        prim_from_bits("dec_tof", t3.clone(), t3, |b| vec![b[0], b[1], b[2] ^ (b[0] & b[1])])
            .expect("tof"),
    ))
}

/// CCC-NOT `(c1, c2, c3, t)`: t ^= c1∧c2∧c3.
fn cccnot_prim() -> Iso {
    let t4 = flat(4);
    Iso::Prim(Box::new(
        prim_from_bits("dec_ccctof", t4.clone(), t4, |b| {
            vec![b[0], b[1], b[2], b[3] ^ (b[0] & b[1] & b[2])]
        })
        .expect("cccnot"),
    ))
}

// ══════════════════════════════════════════════════════════════════════
// Field-group cores
// ══════════════════════════════════════════════════════════════════════

/// Right-nested product of `k` Word8 fields (matches `addressing::fields`).
fn fields_ty(k: usize) -> Type {
    assert!(k >= 1);
    if k == 1 {
        word8()
    } else {
        Type::prod(word8(), fields_ty(k - 1))
    }
}

/// Lift a list of flat-wire gates over the given State fields: within
/// the core, field-list slot `j`, bit `i` is wire `8*j + i`.
pub(crate) fn on_fields(field_idxs: &[usize], gates: Vec<Iso>) -> Iso {
    let k = field_idxs.len();
    let n = k * W;
    let fl = flatten(&fields_ty(k));
    let core = Iso::seq(fl.clone(), Iso::seq(seq_list(n, gates), Iso::sym(fl)));
    at_fields(core, field_idxs)
}

// ══════════════════════════════════════════════════════════════════════
// Register selection: the crossbar
// ══════════════════════════════════════════════════════════════════════

/// A 3-bit register-index field inside one IR byte: `bits[i]` is the
/// bit position (within that byte) of field bit `i` (LSB-first).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegSel {
    pub ir_field: usize,
    pub bits: [usize; 3],
}

/// R-format rd: IR bits 10–8 → IR_hi bits 2..0.
pub const R_RD: RegSel = RegSel { ir_field: IR_HI_FIELD, bits: [0, 1, 2] };
/// R-format rs: IR bits 7–5 → IR_lo bits 7..5.
pub const R_RS: RegSel = RegSel { ir_field: IR_LO_FIELD, bits: [5, 6, 7] };
/// R-format rt: IR bits 4–2 → IR_lo bits 4..2.
pub const R_RT: RegSel = RegSel { ir_field: IR_LO_FIELD, bits: [2, 3, 4] };
/// I-format rd: IR bits 11–9 → IR_hi bits 3..1.
pub const I_RD: RegSel = RegSel { ir_field: IR_HI_FIELD, bits: [1, 2, 3] };

/// Controlled word-swap: swap the Word8 fields `fa` and `fb` iff bit
/// `ctrl_bit` of `ir_field` is set. 8 K1 Fredkins; the control threads
/// through each cell (no fanout).
fn ctrl_word_swap(ir_field: usize, ctrl_bit: usize, fa: usize, fb: usize) -> Iso {
    let n = 3 * W;
    let gates = (0..W)
        .map(|i| gate_at(n, &[ctrl_bit, W + i, 2 * W + i], fredkin_prim()))
        .collect();
    on_fields(&[ir_field, fa, fb], gates)
}

/// The 1-of-8 select tree over the fields `base(0)..base(7)`.
fn select_fields(sel: RegSel, base: impl Fn(usize) -> usize) -> Iso {
    let mut steps = Vec::new();
    for i in 0..4 {
        steps.push(ctrl_word_swap(sel.ir_field, sel.bits[2], base(i), base(i + 4)));
    }
    for i in 0..2 {
        steps.push(ctrl_word_swap(sel.ir_field, sel.bits[1], base(i), base(i + 2)));
    }
    steps.push(ctrl_word_swap(sel.ir_field, sel.bits[0], base(0), base(1)));
    seq_balanced(steps)
}

/// Bring general register `r[sel]` to the fixed slot r0 (field 0).
/// The other registers are permuted data-dependently; composing with
/// `Iso::sym(select_reg(sel))` restores them exactly.
pub fn select_reg(sel: RegSel) -> Iso {
    select_fields(sel, reg_field)
}

/// Bring scratch word `s[sel]` to the fixed slot s0 (field 12).
pub fn select_scratch(sel: RegSel) -> Iso {
    select_fields(sel, scratch_field)
}

/// Reference model of the crossbar's register permutation (for tests):
/// the exact permutation `select_reg` applies for a given field value.
pub fn crossbar_model(field: usize, regs: &mut [u8; 8]) {
    if field & 4 != 0 {
        for i in 0..4 {
            regs.swap(i, i + 4);
        }
    }
    if field & 2 != 0 {
        regs.swap(0, 2);
        regs.swap(1, 3);
    }
    if field & 1 != 0 {
        regs.swap(0, 1);
    }
}

// ══════════════════════════════════════════════════════════════════════
// Predicates
// ══════════════════════════════════════════════════════════════════════

// Wire layout of the predicate core [IR_lo, IR_hi, s1]:
// IR bit b (0..16) = wire b; s1 bit j = wire 16 + j.
const PRED_WIRE: usize = 16; // s1 bit 0
const CHAIN0_WIRE: usize = 17; // s1 bits 1..3: Toffoli-chain intermediates
const CSWAP_CTRL_WIRE_IN_S1: usize = 4; // s1 bit 4: CSWAP rd[0] copy

/// Compute the AND of the given IR-bit literals into s1 bit 0.
/// `controls` are `(ir_bit, expected)` pairs; chain intermediates land
/// in s1 bits 1..3 and stay set until the adjoint uncomputes them.
fn pred_compute(controls: &[(usize, bool)]) -> Iso {
    let n = 3 * W;
    let k = controls.len();
    assert!((2..=5).contains(&k));
    let nots: Vec<Iso> = controls
        .iter()
        .filter(|c| !c.1)
        .map(|c| at_wire(n, c.0, not_iso()))
        .collect();
    let mut gates: Vec<Iso> = nots.clone();
    let mut acc = controls[0].0;
    for (j, c) in controls.iter().enumerate().skip(1) {
        let tgt = if j == k - 1 { PRED_WIRE } else { CHAIN0_WIRE + (j - 1) };
        gates.push(gate_at(n, &[acc, c.0, tgt], toffoli_prim()));
        acc = tgt;
    }
    gates.extend(nots);
    on_fields(&[IR_LO_FIELD, IR_HI_FIELD, AUX_PRED_FIELD], gates)
}

/// Predicate literals for R-format opcode `op` (bit 15 = 1, bits 14–11).
fn r_pred(op: u8) -> Vec<(usize, bool)> {
    vec![
        (15, true),
        (14, (op >> 3) & 1 == 1),
        (13, (op >> 2) & 1 == 1),
        (12, (op >> 1) & 1 == 1),
        (11, op & 1 == 1),
    ]
}

/// Predicate literals for I-format opc `op` (bit 15 = 0, bits 14–12).
fn i_pred(op: u8) -> Vec<(usize, bool)> {
    vec![
        (15, false),
        (14, (op >> 2) & 1 == 1),
        (13, (op >> 1) & 1 == 1),
        (12, op & 1 == 1),
    ]
}

// ══════════════════════════════════════════════════════════════════════
// Gated operation cores
// ══════════════════════════════════════════════════════════════════════

// Standard 3-field core [target=r0-slot, s0, s1]: target wires 0..8,
// operand wires 8..16, predicate wire 16.
const TGT0: usize = 0;
const OPD0: usize = W;
const PRED3: usize = 2 * W;

/// The Takahashi–Kunihiro zero-ancilla in-place adder gate list on
/// target wires `b0..b0+8` and operand wires `a0..a0+8`, with every
/// gate carrying `pred` as an additional control.
fn gated_tk_gates(n: usize, pred: usize, b0: usize, a0: usize) -> Vec<Iso> {
    enum Tk {
        C { c: usize, t: usize },
        T { c1: usize, c2: usize, t: usize },
    }
    let a = |i: usize| a0 + i;
    let b = |i: usize| b0 + i;
    let mut g: Vec<Tk> = Vec::new();
    for i in 1..W - 1 {
        g.push(Tk::C { c: a(i), t: b(i) });
    }
    for i in (1..W - 1).rev() {
        g.push(Tk::C { c: a(i), t: a(i + 1) });
    }
    for i in 0..W - 1 {
        g.push(Tk::T { c1: a(i), c2: b(i), t: a(i + 1) });
    }
    g.push(Tk::C { c: a(W - 1), t: b(W - 1) });
    g.push(Tk::T { c1: a(W - 2), c2: b(W - 2), t: a(W - 1) });
    for i in (1..W - 1).rev() {
        g.push(Tk::C { c: a(i), t: b(i) });
        g.push(Tk::T { c1: a(i - 1), c2: b(i - 1), t: a(i) });
    }
    for i in 1..W - 1 {
        g.push(Tk::C { c: a(i), t: a(i + 1) });
    }
    for i in 0..W - 1 {
        g.push(Tk::C { c: a(i), t: b(i) });
    }
    g.into_iter()
        .map(|gate| match gate {
            Tk::C { c, t } => gate_at(n, &[pred, c, t], toffoli_prim()),
            Tk::T { c1, c2, t } => gate_at(n, &[pred, c1, c2, t], cccnot_prim()),
        })
        .collect()
}

/// Gated XOR merge: target wires `t0..t0+8` ^= operand wires `s0..s0+8`
/// iff `pred`.
fn gated_xor_gates(n: usize, pred: usize, t0: usize, s0: usize) -> Vec<Iso> {
    (0..W)
        .map(|i| gate_at(n, &[pred, s0 + i, t0 + i], toffoli_prim()))
        .collect()
}

/// Gated barrel rotate-left of target wires `t0..t0+8` by the 3-bit
/// amount whose bit `k` is at wire `ctrl(k)`, all gated on `pred`.
fn gated_rotl_gates(n: usize, pred: usize, t0: usize, ctrl: impl Fn(usize) -> usize) -> Vec<Iso> {
    let mut gates = Vec::new();
    for k in 0..3 {
        for (i, j) in rot_transpositions(1 << k) {
            gates.push(gate_at(n, &[pred, ctrl(k), t0 + i, t0 + j], gated_fredkin_prim()));
        }
    }
    gates
}

// ══════════════════════════════════════════════════════════════════════
// Instruction assembly
// ══════════════════════════════════════════════════════════════════════

/// Swap-fetch: exchange `r[src]` with s0 (bring the operand to the
/// fixed s0 slot). Exact inverse restores both; zero cells beyond the
/// two crossbar traversals.
fn fetch_operand(src: RegSel) -> Iso {
    let sel = select_reg(src);
    let swap = at_fields(
        Iso::SwapProd(word8(), word8()),
        &[reg_field(0), AUX_OPERAND_FIELD],
    );
    Iso::seq(sel.clone(), Iso::seq(swap, Iso::sym(sel)))
}

/// Route `r[sel]` to the fixed slot, apply `core`, route back.
fn with_target(sel: RegSel, core: Iso) -> Iso {
    let s = select_reg(sel);
    Iso::seq(s.clone(), Iso::seq(core, Iso::sym(s)))
}

/// Bracket `body` with predicate compute/uncompute.
fn predicated(controls: &[(usize, bool)], body: Iso) -> Iso {
    let p = pred_compute(controls);
    Iso::seq(p.clone(), Iso::seq(body, Iso::sym(p)))
}

/// Standard core lift onto [r0-slot, s0, s1].
fn std_core(gates: Vec<Iso>) -> Iso {
    on_fields(&[reg_field(0), AUX_OPERAND_FIELD, AUX_PRED_FIELD], gates)
}

/// A gated reg-reg instruction: fetch rs, route rd, run `core`.
fn reg_reg(opcode: u8, core: Iso) -> Iso {
    predicated(&r_pred(opcode), with_two_regs(R_RD, R_RS, core))
}

/// Fetch `r[src]` into s0 and route `r[dst]` to the r0 slot, run
/// `core` on [r0-slot, s0, s1], then restore. Because swap-fetch
/// leaves the *value* of `r[src]` in s0 (and writes s0's final content
/// back into `r[src]` on exit), a core may read AND WRITE both
/// operands locally: writes to the s0 wires become writes to `r[src]`.
fn with_two_regs(dst: RegSel, src: RegSel, core: Iso) -> Iso {
    let f = fetch_operand(src);
    Iso::seq(f.clone(), Iso::seq(with_target(dst, core), Iso::sym(f)))
}

fn add_core() -> Iso {
    std_core(gated_tk_gates(3 * W, PRED3, TGT0, OPD0))
}

fn rotl_core() -> Iso {
    std_core(gated_rotl_gates(3 * W, PRED3, TGT0, |k| OPD0 + k))
}

fn add_instr() -> Iso {
    reg_reg(0b0000, add_core())
}

fn sub_instr() -> Iso {
    reg_reg(0b0001, Iso::sym(add_core()))
}

fn xor_instr() -> Iso {
    reg_reg(0b0010, std_core(gated_xor_gates(3 * W, PRED3, TGT0, OPD0)))
}

/// EXCH rd, rs: with r[rd] at the r0 slot and the VALUE of r[rs] in s0
/// (swap-fetch), a predicated swap of the two words is 8 native
/// 1-controlled swaps; the swapped s0 content is written back into
/// r[rs] by the unfetch.
fn exch_instr() -> Iso {
    let core = std_core(
        (0..W)
            .map(|i| gate_at(3 * W, &[PRED3, TGT0 + i, OPD0 + i], fredkin_prim()))
            .collect(),
    );
    predicated(&r_pred(0b0011), with_two_regs(R_RD, R_RS, core))
}

fn rotl_instr() -> Iso {
    reg_reg(0b0100, rotl_core())
}

fn rotr_instr() -> Iso {
    reg_reg(0b0101, Iso::sym(rotl_core()))
}

/// CSWAP rd, rs, rt: copy the control bit r[rd][0] into s1 bit 4, then
/// with the VALUE of r[rs] in s0 (swap-fetch) and r[rt] at the r0 slot,
/// the conditional exchange is 8 native 2-controlled swaps
/// (controls = predicate ∧ rd[0]-copy); the swapped s0 content is
/// written back into r[rs] by the unfetch, and the control copy is
/// removed afterwards (r[rd] itself is never modified).
fn cswap_instr() -> Iso {
    let copy_rd0 = with_target(
        R_RD,
        on_fields(
            &[reg_field(0), AUX_PRED_FIELD],
            vec![gate_at(2 * W, &[0, W + CSWAP_CTRL_WIRE_IN_S1], cnot_prim())],
        ),
    );
    let core = std_core(
        (0..W)
            .map(|i| {
                gate_at(
                    3 * W,
                    &[PRED3, PRED3 + CSWAP_CTRL_WIRE_IN_S1, OPD0 + i, TGT0 + i],
                    gated_fredkin_prim(),
                )
            })
            .collect(),
    );
    let body = Iso::seq(
        copy_rd0.clone(),
        Iso::seq(with_two_regs(R_RT, R_RS, core), Iso::sym(copy_rd0)),
    );
    predicated(&r_pred(0b0110), body)
}

/// XCHG rd, sx: gated swap of the routed register and scratch slots.
/// The scratch crossbar scrambles s1's position, so the predicate is
/// first copied onto the R-format pad bit IR[0] (architecturally zero)
/// which stays fixed during routing; the copy is removed afterwards,
/// preserving IR.
fn xchg_instr() -> Iso {
    // [IR_lo, s1]: pad = wire 0, pred = wire 8.
    let pad_copy = on_fields(
        &[IR_LO_FIELD, AUX_PRED_FIELD],
        vec![gate_at(2 * W, &[W, 0], cnot_prim())],
    );
    let selr = select_reg(R_RD);
    let sels = select_scratch(R_RT);
    // [IR_lo, r0-slot, s0-slot]: pad = wire 0.
    let gsw = on_fields(
        &[IR_LO_FIELD, reg_field(0), scratch_field(0)],
        (0..W)
            .map(|i| gate_at(3 * W, &[0, W + i, 2 * W + i], fredkin_prim()))
            .collect(),
    );
    let body = Iso::seq(
        pad_copy.clone(),
        Iso::seq(
            selr.clone(),
            Iso::seq(
                sels.clone(),
                Iso::seq(
                    gsw,
                    Iso::seq(Iso::sym(sels), Iso::seq(Iso::sym(selr), Iso::sym(pad_copy))),
                ),
            ),
        ),
    );
    predicated(&r_pred(0b0111), body)
}

/// SWAPBR rd: gated swap of BR ↔ r[rd] (ADR-0034 D1, Pendulum
/// semantics). EXCH-shaped: route r[rd] to the r0 slot via the R_RD
/// crossbar, perform a predicated swap of r0-slot and BR, route back.
/// No instruction writes PC; the branch offset in BR is consumed by
/// the end-of-step `pc_add_br ; pc_inc`.
fn swapbr_instr() -> Iso {
    let sel = select_reg(R_RD);
    let core = on_fields(
        &[reg_field(0), BR_FIELD, AUX_PRED_FIELD],
        (0..W)
            .map(|i| gate_at(3 * W, &[2 * W, i, W + i], fredkin_prim()))
            .collect(),
    );
    let body = Iso::seq(sel.clone(), Iso::seq(core, Iso::sym(sel)));
    predicated(&r_pred(0b1000), body)
}

/// ADDI core on [r0-slot, s0, IR_lo, IR_hi, s1]: CNOT-mask the imm8
/// (IR bits 8..1) onto s0, gated TK add, unmask. Requires s0 = 0.
fn addi_core() -> Iso {
    let n = 5 * W;
    let pred = 4 * W;
    // imm bit j = IR bit j+1: j<7 → IR_lo bit j+1 (wire 2W+1+j);
    // j=7 → IR_hi bit 0 (wire 3W).
    let imm_wire = |j: usize| if j < 7 { 2 * W + 1 + j } else { 3 * W };
    let mask: Vec<Iso> = (0..W)
        .map(|j| gate_at(n, &[imm_wire(j), W + j], cnot_prim()))
        .collect();
    let mut gates = mask.clone();
    gates.extend(gated_tk_gates(n, pred, 0, W));
    gates.extend(mask);
    on_fields(
        &[
            reg_field(0),
            AUX_OPERAND_FIELD,
            IR_LO_FIELD,
            IR_HI_FIELD,
            AUX_PRED_FIELD,
        ],
        gates,
    )
}

fn addi_instr() -> Iso {
    predicated(&i_pred(0b000), with_target(I_RD, addi_core()))
}

fn subi_instr() -> Iso {
    predicated(&i_pred(0b001), with_target(I_RD, Iso::sym(addi_core())))
}

/// XORI core on [r0-slot, IR_lo, IR_hi, s1]: gated CNOTs straight from
/// the IR immediate bits (IR bits act as controls — the legal fanout).
fn xori_instr() -> Iso {
    let n = 4 * W;
    let pred = 3 * W;
    let imm_wire = |j: usize| if j < 7 { W + 1 + j } else { 2 * W };
    let gates: Vec<Iso> = (0..W)
        .map(|j| gate_at(n, &[pred, imm_wire(j), j], toffoli_prim()))
        .collect();
    let core = on_fields(
        &[reg_field(0), IR_LO_FIELD, IR_HI_FIELD, AUX_PRED_FIELD],
        gates,
    );
    predicated(&i_pred(0b010), with_target(I_RD, core))
}

/// ROTLI core on [r0-slot, IR_lo, s1]: barrel rotate controlled
/// directly by IR imm bits 0..2 (IR bits 3..1).
fn rotli_core() -> Iso {
    let n = 3 * W;
    let pred = 2 * W;
    on_fields(
        &[reg_field(0), IR_LO_FIELD, AUX_PRED_FIELD],
        gated_rotl_gates(n, pred, 0, |k| W + 1 + k),
    )
}

fn rotli_instr() -> Iso {
    predicated(&i_pred(0b011), with_target(I_RD, rotli_core()))
}

fn rotri_instr() -> Iso {
    predicated(&i_pred(0b100), with_target(I_RD, Iso::sym(rotli_core())))
}

// ══════════════════════════════════════════════════════════════════════
// Public dispatch
// ══════════════════════════════════════════════════════════════════════

/// The 14 live TISC-1 instructions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Xor,
    Exch,
    Rotl,
    Rotr,
    Cswap,
    Xchg,
    Swapbr,
    Addi,
    Subi,
    Xori,
    Rotli,
    Rotri,
}

impl Op {
    pub const ALL: [Op; 14] = [
        Op::Add,
        Op::Sub,
        Op::Xor,
        Op::Exch,
        Op::Rotl,
        Op::Rotr,
        Op::Cswap,
        Op::Xchg,
        Op::Swapbr,
        Op::Addi,
        Op::Subi,
        Op::Xori,
        Op::Rotli,
        Op::Rotri,
    ];
}

/// The predicated State-level iso for one instruction: identity unless
/// IR's format/opcode bits match `op`, in which case the instruction is
/// applied to the registers IR names.
pub fn instruction(op: Op) -> Iso {
    match op {
        Op::Add => add_instr(),
        Op::Sub => sub_instr(),
        Op::Xor => xor_instr(),
        Op::Exch => exch_instr(),
        Op::Rotl => rotl_instr(),
        Op::Rotr => rotr_instr(),
        Op::Cswap => cswap_instr(),
        Op::Xchg => xchg_instr(),
        Op::Swapbr => swapbr_instr(),
        Op::Addi => addi_instr(),
        Op::Subi => subi_instr(),
        Op::Xori => xori_instr(),
        Op::Rotli => rotli_instr(),
        Op::Rotri => rotri_instr(),
    }
}

/// Decode+execute over a subset of the instruction set (useful for
/// incremental verification and cost accounting).
pub fn decode_execute_ops(ops: &[Op]) -> Iso {
    assert!(!ops.is_empty(), "nonempty op list");
    seq_balanced(ops.iter().map(|&o| instruction(o)).collect())
}

/// `decode_execute : State <-> State` — the full TISC-1 decoder.
///
/// Applies the instruction encoded in IR to the registers it names,
/// preserving IR. Reserved opcodes are the identity. See the module
/// docs for the s0/s1/pad-bit conventions and the aliasing caveat.
pub fn decode_execute() -> Iso {
    decode_execute_ops(&Op::ALL)
}
