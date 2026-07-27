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
//!    low the whole instruction is the identity. The 15 predicated
//!    instructions run in sequence; opcodes are mutually exclusive, so
//!    at most one fires.
//!
//!    **Reserved opcodes.** The predicates test only the **low 4** opcode
//!    bits (R) / **low 3** (I) — the two high opcode bits are NOT tested
//!    (MAJOR-3, widening deferred: `pred_compute` caps at 5 controls and
//!    s1 bit 4 is CSWAP's control copy, so fmt + 6 opcode bits = 7 controls
//!    do not fit without the s1-convention rework). Consequently a word
//!    whose HIGH opcode bits are set ALIASES the live op with the same low
//!    bits (e.g. R opcode 25 = 0b011001 executes as MEXCH 0b1001). This is
//!    SAFE for every emitted ROM: `asm::encode` only ever emits opcodes
//!    that fit the low bits, and `assemble` is the sole ROM source, so no
//!    ROM word can have a high opcode bit set. `asm::decode` masks the
//!    opcode to the same low bits the datapath tests, so decode and the
//!    datapath AGREE on hand-crafted words too (no silent divergence).
//!    Low-bit patterns that match no live op (R low 1100–1111, I low
//!    101–111) ARE the identity at the iso level. MEXCH 1001 and MEXCHR
//!    1010 are handled by emit/mem/absorb. Source-level ISA totality is
//!    the coverage checker's job (P1) and is out of scope here.
//!
//! ## Conventions and caveats (documented, enforced by the P1 checker
//! at source level, not by this iso)
//!
//! - **s0 (scratch 0) is the operand/immediate buffer.** Register
//!   second operands are *swap-fetched* into s0 (a zero-cell field swap
//!   bracketed by the rs crossbar), which is exact for any s0 content.
//!   After B1.2 (unified reg+imm block), **all I-format ALU ops route the
//!   immediate through s0** via `imm_to_a` — Addi/Subi (adder), Xori (xor
//!   sub-block), AND Rotli/Rotri (rotator), not just Addi/Subi — so every
//!   I-format ALU op **requires s0 = 0 at unified-block entry** (the
//!   immediate is CNOT-masked onto s0; the shared core then consumes
//!   `s0 ⊕ imm`). Same convention as `instr::addi`.
//! - **s1 (scratch 1) holds the predicate bit (bit 0), the Toffoli
//!   chain intermediates (bits 1–3), and CSWAP's control-bit copy
//!   (bit 4); it must be 0 on entry** and is restored to 0 on exit.
//! - **BR (field 9) is used by CCNOT as a 2nd operand buffer**
//!   (convention B): r[rt] is swap-fetched into BR before the core
//!   and restored afterwards. The swap is content-agnostic — BR's
//!   entry value rides in r[rt]'s cell for the duration of the core
//!   and is written back on exit (no BR=0 assumption). Aliased
//!   CCNOT operand/BR/s0 residues vary by alias case (rd==rt exits
//!   with BR corrupted; rd==rs leaves an s0 residue) — all bijective,
//!   unspecified-semantics; `encode` rejects via CcnotNotDistinct.
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
    flat, reg_field, scratch_field, word8, MemWidth, BR_FIELD, BUS_ADDR_FIELD, BUS_ENABLE_FIELD,
    BUS_WORD_FIELD, IR_B0_FIELD, IR_B1_FIELD, IR_B2_FIELD, IR_B3_FIELD, IR_LO_FIELD, MEM_WIDTH,
    MEM_WORDS, NUM_BUS_FIELDS, NUM_CPU_FIELDS, NUM_FIELDS, NUM_REGS, W,
};

// ── Aux-field conventions ──────────────────────────────────────────

/// State field used as the operand/immediate buffer (s0).
/// A-0: scratch base shifted 12 → 14 (32-bit IR added two byte fields).
pub const AUX_OPERAND_FIELD: usize = 14;
/// State field holding the predicate bit + chain intermediates (s1).
pub const AUX_PRED_FIELD: usize = 15;

// ── A-0 32-bit IR bit layout (global IR bit → (byte-field, bit)) ────
//
// fmt = bit 31; opcode = bits 25..30 (6 bits); rd = bits 20..24;
// rs = bits 15..19; rt = bits 10..14 (all 5-bit); I-imm = bits 0..19.

/// Global IR-bit index → the (byte-field, bit-within-byte) it lives at.
/// IR byte `k` (k=0..4) is field `IR_B0_FIELD + k`, holding bits 8k..8k+8.
pub const fn ir_bit(b: usize) -> (usize, usize) {
    (IR_B0_FIELD + b / W, b % W)
}

/// IR bit index of the format bit (1 = R-format).
const FMT_BIT: usize = 31;
/// IR bit index of opcode bit `k` (k = 0..6); opcode occupies bits 25..30.
const fn opcode_bit(k: usize) -> usize {
    25 + k
}
// Operand fields (rd/rs/rt at IR bits 20..24 / 15..19 / 10..14) and the
// I-format immediate (bits 0..19) are addressed directly by the RegSel
// constants and the immediate-mask cores below.

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
        prim_from_bits("dec_tof", t3.clone(), t3, |b| {
            vec![b[0], b[1], b[2] ^ (b[0] & b[1])]
        })
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

/// Lift a list of flat-wire gates over the given fields within a product
/// of `n_fields` Word8 fields: within the core, field-list slot `j`,
/// bit `i` is wire `8*j + i`.
pub fn on_fields_n(field_idxs: &[usize], gates: Vec<Iso>, n_fields: usize) -> Iso {
    use crate::addressing::at_fields_n;
    let k = field_idxs.len();
    let n = k * W;
    let fl = flatten(&fields_ty(k));
    let core = Iso::seq(fl.clone(), Iso::seq(seq_list(n, gates), Iso::sym(fl)));
    at_fields_n(core, field_idxs, n_fields)
}

/// Lift a list of flat-wire gates over the given State fields: within
/// the core, field-list slot `j`, bit `i` is wire `8*j + i`.
/// Equivalent to `on_fields_n(field_idxs, gates, NUM_FIELDS)`.
pub(crate) fn on_fields(field_idxs: &[usize], gates: Vec<Iso>) -> Iso {
    on_fields_n(field_idxs, gates, NUM_FIELDS)
}

// ══════════════════════════════════════════════════════════════════════
// Register selection: the crossbar
// ══════════════════════════════════════════════════════════════════════

/// A 5-bit operand-index field of the A-0 word. `bits[i]` is the
/// (byte-field, bit-within-byte) of operand-field bit `i`, LSB-first.
///
/// The operand crossbar routes over `reg ∪ scratch` = 16 locations, so
/// only the low **4** field bits are used to steer the 4-level select
/// tree; the 5th operand bit (selecting reserved locations 16..32) is
/// never set by a live instruction and is carried only for reference.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegSel {
    /// (byte-field, bit) of the 5 operand-field bits, LSB-first.
    pub bits: [(usize, usize); 5],
}

/// Build a `RegSel` from the five global IR-bit indices of a 5-bit
/// operand field (LSB-first).
const fn regsel(b: [usize; 5]) -> RegSel {
    RegSel {
        bits: [
            ir_bit(b[0]),
            ir_bit(b[1]),
            ir_bit(b[2]),
            ir_bit(b[3]),
            ir_bit(b[4]),
        ],
    }
}

/// R-format / I-format rd: IR bits 20..24 (5-bit operand).
pub const R_RD: RegSel = regsel([20, 21, 22, 23, 24]);
/// R-format rs: IR bits 15..19 (5-bit operand).
pub const R_RS: RegSel = regsel([15, 16, 17, 18, 19]);
/// R-format rt: IR bits 10..14 (5-bit operand).
pub const R_RT: RegSel = regsel([10, 11, 12, 13, 14]);
/// I-format rd: same field as R-format rd (IR bits 20..24).
pub const I_RD: RegSel = regsel([20, 21, 22, 23, 24]);

/// Controlled word-swap: swap the Word8 fields `fa` and `fb` iff the
/// control bit at `(ctrl_field, ctrl_bit)` is set. 8 K1 Fredkins; the
/// control threads through each cell (no fanout).
fn ctrl_word_swap(ctrl_field: usize, ctrl_bit: usize, fa: usize, fb: usize) -> Iso {
    let n = 3 * W;
    let gates = (0..W)
        .map(|i| gate_at(n, &[ctrl_bit, W + i, 2 * W + i], fredkin_prim()))
        .collect();
    on_fields(&[ctrl_field, fa, fb], gates)
}

/// Controlled swap of two W-bit words across a 2-factor product boundary.
///
/// Carrier: `fields(n_left) × fields(n_right)`.
/// Swaps `word_a` and `word_b` (field indices within the right factor)
/// iff bit `ctrl_bit` of `ctrl_field` (a field index within the left
/// factor) is set.
///
/// Internally: flatten the composite to `fields(n_left + n_right)`,
/// apply the 3-field controlled swap via `on_fields_n`, then re-split.
/// The routing unit is the field (8 wires), not the bit.
pub fn cross_ctrl_fredkin(
    ctrl_field: usize,
    ctrl_bit: usize,
    word_a: usize,
    word_b: usize,
    n_left: usize,
    n_right: usize,
) -> Iso {
    use crate::addressing::split_fields;
    assert!(
        ctrl_field < n_left,
        "ctrl_field out of range in left factor"
    );
    assert!(ctrl_bit < W, "ctrl_bit out of range");
    assert!(word_a < n_right, "word_a out of range in right factor");
    assert!(word_b < n_right, "word_b out of range in right factor");
    assert_ne!(word_a, word_b, "word_a == word_b");

    let n_total = n_left + n_right;
    // Map indices into the flat fields(n_total) product.
    let ctrl_flat = ctrl_field;
    let wa_flat = n_left + word_a;
    let wb_flat = n_left + word_b;

    // Build the 3-field controlled swap on fields(n_total).
    let n3 = 3 * W;
    let gates: Vec<Iso> = (0..W)
        .map(|i| gate_at(n3, &[ctrl_bit, W + i, 2 * W + i], fredkin_prim()))
        .collect();
    let core = on_fields_n(&[ctrl_flat, wa_flat, wb_flat], gates, n_total);

    // Bracket with the composite ↔ flat iso.
    let split = split_fields(n_left, n_right);
    // split : fields(n_total) ↔ fields(n_left) × fields(n_right)
    // We want: composite → flat → core → flat → composite
    Iso::seq(Iso::sym(split.clone()), Iso::seq(core, split))
}

/// The 1-of-8 select tree over the fields `base(0)..base(7)`, steered
/// by the low 3 bits of the operand field (`sel.bits[0..3]`).
fn select_fields(sel: RegSel, base: impl Fn(usize) -> usize) -> Iso {
    let (f2, b2) = sel.bits[2];
    let (f1, b1) = sel.bits[1];
    let (f0, b0) = sel.bits[0];
    let mut steps = Vec::new();
    for i in 0..4 {
        steps.push(ctrl_word_swap(f2, b2, base(i), base(i + 4)));
    }
    for i in 0..2 {
        steps.push(ctrl_word_swap(f1, b1, base(i), base(i + 2)));
    }
    steps.push(ctrl_word_swap(f0, b0, base(0), base(1)));
    seq_balanced(steps)
}

/// Map an operand index `0..16` to its State field: `i<8 → r[i]`,
/// `8 ≤ i < 16 → s[i−8]` (the A-0 scratch-addressable operand space).
pub fn operand_field(i: usize) -> usize {
    if i < NUM_REGS {
        reg_field(i)
    } else {
        scratch_field(i - NUM_REGS)
    }
}

/// The 1-of-16 operand select tree over `reg ∪ scratch` (A-0), steered
/// by the low **4** bits of the operand field. `base(i)` maps operand
/// index `i` (0..16) to a State field; the selected location lands at
/// `base(0)`. Composing with `Iso::sym(select_operand(...))` restores
/// the permutation exactly.
///
/// This is the coverage-critical generalization of `select_reg`
/// (`select_reg` = the low-8 sub-tree). The extra top level (bit 3)
/// swaps `base(i) ↔ base(i+8)` for i = 2..8, bringing a scratch word
/// into the register half when the operand names scratch.
///
/// A-0 BLOCKER-1 fix (scratch-rd operand crossbar): the level-3 swap
/// range starts at **i = 2**, NOT i = 0. `base(8)` and `base(9)` are
/// `s0`/`s1` — the fixed operand buffer (`AUX_OPERAND_FIELD`) and the
/// predicate word (`AUX_PRED_FIELD`), which are decoder-reserved and
/// rejected as operands (`asm::check_operand`). Idx 8/9 can therefore
/// never be a legal operand, so the tree never needs to route those
/// positions to `base(0)` — and swapping `base(0)↔base(8)` /
/// `base(1)↔base(9)` (i.e. `r0↔s0`, `r1↔s1`) would DISPLACE the operand
/// buffer and CORRUPT the predicate word mid-instruction (wrong cores
/// fire, BR clobbered, s0 left dirty). Every legal scratch operand
/// s2..s7 (idx 10..15) lives at position 10..15 = `base(2+k)+8`, reached
/// purely via the i∈2..8 swaps; register operands (bit3=0) are inert
/// here regardless. So s0/s1 (fields 14/15) are left UNTOUCHED.
pub fn select_operand(sel: RegSel, base: impl Fn(usize) -> usize) -> Iso {
    let (f3, b3) = sel.bits[3];
    let (f2, b2) = sel.bits[2];
    let (f1, b1) = sel.bits[1];
    let (f0, b0) = sel.bits[0];
    let mut steps = Vec::new();
    // Level 3 (bit 3): reg half ↔ scratch half, EXCLUDING positions 0/1
    // (whose scratch partners base(8)/base(9) = s0/s1 are the reserved
    // operand-buffer + predicate word — never legal operands).
    for i in 2..8 {
        steps.push(ctrl_word_swap(f3, b3, base(i), base(i + 8)));
    }
    // Level 2 (bit 2): quarter swaps within the low 8.
    for i in 0..4 {
        steps.push(ctrl_word_swap(f2, b2, base(i), base(i + 4)));
    }
    // Level 1 (bit 1).
    for i in 0..2 {
        steps.push(ctrl_word_swap(f1, b1, base(i), base(i + 2)));
    }
    // Level 0 (bit 0).
    steps.push(ctrl_word_swap(f0, b0, base(0), base(1)));
    seq_balanced(steps)
}

/// Bring general register `r[sel]` to the fixed slot r0 (field 0).
/// The other registers are permuted data-dependently; composing with
/// `Iso::sym(select_reg(sel))` restores them exactly.
pub fn select_reg(sel: RegSel) -> Iso {
    select_fields(sel, reg_field)
}

/// Bring scratch word `s[sel]` to the fixed slot s0 (field 14).
pub fn select_scratch(sel: RegSel) -> Iso {
    select_fields(sel, scratch_field)
}

/// The 1-of-N select tree over a right factor of `n_mem` Word8 fields,
/// controlled by IR bits in a left factor of `n_cpu` Word8 fields.
///
/// Carrier: `(fields(n_cpu), fields(n_mem))`.
/// Routes `RightFactor[addr] → RightFactor head`, where `addr` bits
/// come from `sel` in the left factor. Each level uses
/// [`cross_ctrl_fredkin`]: control in left, swap targets in right.
///
/// THIS IS THE C1 TREE re-targeted onto the composite carrier (Stage 1a-B,
/// ADR-0046 §4.5.0 Decision 3). It MUST be ancilla-clean.
pub fn select_mem_composite_n(sel: RegSel, n_cpu: usize, mem: MemWidth) -> Iso {
    select_mem_composite_n_inner(sel, n_cpu, mem.words())
}

/// Inner helper: operates on raw word count. The recursion structure and
/// gate construction are VERBATIM from the original `select_mem_composite_n`.
fn select_mem_composite_n_inner(sel: RegSel, n_cpu: usize, n_mem: usize) -> Iso {
    let n_bits = n_mem.trailing_zeros() as usize; // addr_bits for powers of two
    let mut steps = Vec::new();
    for level in (0..n_bits).rev() {
        let stride = 1usize << level;
        for i in 0..stride {
            if i + stride < n_mem {
                let (ctrl_field, ctrl_bit) = sel.bits[level];
                steps.push(cross_ctrl_fredkin(
                    ctrl_field,
                    ctrl_bit,
                    i,
                    i + stride,
                    n_cpu,
                    n_mem,
                ));
            }
        }
    }
    if steps.is_empty() {
        use crate::addressing::fields;
        Iso::Id(Type::prod(fields(n_cpu), fields(n_mem)))
    } else {
        seq_balanced(steps)
    }
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

// Wire layout of the predicate core [IR_b0, IR_b1, IR_b2, IR_b3, s1]
// (A-0, 32-bit IR): IR bit b (0..32) = wire b; s1 bit j = wire 32 + j.
const PRED_WIRE: usize = 32; // s1 bit 0
const CHAIN0_WIRE: usize = 33; // s1 bits 1..3: Toffoli-chain intermediates
const CSWAP_CTRL_WIRE_IN_S1: usize = 4; // s1 bit 4: CSWAP rd[0] copy

/// The five fields the predicate core lifts over (four IR bytes + s1).
fn pred_fields() -> [usize; 5] {
    [
        IR_B0_FIELD,
        IR_B1_FIELD,
        IR_B2_FIELD,
        IR_B3_FIELD,
        AUX_PRED_FIELD,
    ]
}

/// Compute the AND of the given IR-bit literals into s1 bit 0.
/// `controls` are `(ir_bit, expected)` pairs; chain intermediates land
/// in s1 bits 1..3 and stay set until the adjoint uncomputes them.
///
/// A-0: the core spans four IR byte-fields (32 wires) + s1 (wires
/// 32..40); IR-bit literals index the flat 32-wire IR directly.
fn pred_compute(controls: &[(usize, bool)]) -> Iso {
    let n = 5 * W;
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
        let tgt = if j == k - 1 {
            PRED_WIRE
        } else {
            CHAIN0_WIRE + (j - 1)
        };
        gates.push(gate_at(n, &[acc, c.0, tgt], toffoli_prim()));
        acc = tgt;
    }
    gates.extend(nots);
    on_fields(&pred_fields(), gates)
}

/// Predicate literals for R-format opcode `op` (fmt bit = 1; the four
/// low opcode bits discriminate the live R-ops — the two high opcode
/// bits are 0 for every architected op and are not tested, exactly as
/// the pre-A-0 4-bit opcode).
fn r_pred(op: u8) -> Vec<(usize, bool)> {
    vec![
        (FMT_BIT, true),
        (opcode_bit(3), (op >> 3) & 1 == 1),
        (opcode_bit(2), (op >> 2) & 1 == 1),
        (opcode_bit(1), (op >> 1) & 1 == 1),
        (opcode_bit(0), op & 1 == 1),
    ]
}

/// Predicate literals for I-format opc `op` (fmt bit = 0; the three low
/// opcode bits discriminate the live I-ops).
fn i_pred(op: u8) -> Vec<(usize, bool)> {
    vec![
        (FMT_BIT, false),
        (opcode_bit(2), (op >> 2) & 1 == 1),
        (opcode_bit(1), (op >> 1) & 1 == 1),
        (opcode_bit(0), op & 1 == 1),
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
        g.push(Tk::C {
            c: a(i),
            t: a(i + 1),
        });
    }
    for i in 0..W - 1 {
        g.push(Tk::T {
            c1: a(i),
            c2: b(i),
            t: a(i + 1),
        });
    }
    g.push(Tk::C {
        c: a(W - 1),
        t: b(W - 1),
    });
    g.push(Tk::T {
        c1: a(W - 2),
        c2: b(W - 2),
        t: a(W - 1),
    });
    for i in (1..W - 1).rev() {
        g.push(Tk::C { c: a(i), t: b(i) });
        g.push(Tk::T {
            c1: a(i - 1),
            c2: b(i - 1),
            t: a(i),
        });
    }
    for i in 1..W - 1 {
        g.push(Tk::C {
            c: a(i),
            t: a(i + 1),
        });
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
            gates.push(gate_at(
                n,
                &[pred, ctrl(k), t0 + i, t0 + j],
                gated_fredkin_prim(),
            ));
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
    // A-0 (BLOCKER-2 fix): route over reg∪scratch (the full 16-way
    // crossbar), not the 8-way register-only tree. A register operand
    // (idx 0..8) has operand-bit 3 = 0, so the top crossbar level is
    // inert and behaviour is identical to the old `select_reg`; a scratch
    // operand (idx 8..16) brings s[idx-8] to the fixed slot `operand_field(0)`.
    let sel = select_operand(src, operand_field);
    let swap = at_fields(
        Iso::SwapProd(word8(), word8()),
        &[operand_field(0), AUX_OPERAND_FIELD],
    );
    Iso::seq(sel.clone(), Iso::seq(swap, Iso::sym(sel)))
}

/// Route operand `sel` (over reg∪scratch) to the fixed slot, apply
/// `core`, route back. A-0 (BLOCKER-2 fix): the crossbar spans reg∪scratch.
fn with_target(sel: RegSel, core: Iso) -> Iso {
    let s = select_operand(sel, operand_field);
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

// ── B1.1: config-wire-parameterized cores ──────────────────────────────
//
// Identical to the `*_core()` builders above, but each gate carries s1 bit
// `cfg` (PRED3 + cfg) as its control instead of the fixed predicate bit 0.
// This lets the shared ALU gate each sub-block on its own decoded config
// line while all sub-blocks share ONE fixed netlist. `cfg == 0` reproduces
// the original `*_core()` exactly.

fn add_core_at(cfg: usize) -> Iso {
    std_core(gated_tk_gates(3 * W, PRED3 + cfg, TGT0, OPD0))
}

fn xor_core_at(cfg: usize) -> Iso {
    std_core(gated_xor_gates(3 * W, PRED3 + cfg, TGT0, OPD0))
}

fn rotl_core_at(cfg: usize) -> Iso {
    std_core(gated_rotl_gates(3 * W, PRED3 + cfg, TGT0, |k| OPD0 + k))
}

fn exch_core_at(cfg: usize) -> Iso {
    std_core(
        (0..W)
            .map(|i| gate_at(3 * W, &[PRED3 + cfg, TGT0 + i, OPD0 + i], fredkin_prim()))
            .collect(),
    )
}

fn add_instr() -> Iso {
    reg_reg(0b0000, add_core())
}

fn sub_instr() -> Iso {
    reg_reg(0b0001, Iso::sym(add_core()))
}

fn xor_core() -> Iso {
    std_core(gated_xor_gates(3 * W, PRED3, TGT0, OPD0))
}

fn exch_core() -> Iso {
    std_core(
        (0..W)
            .map(|i| gate_at(3 * W, &[PRED3, TGT0 + i, OPD0 + i], fredkin_prim()))
            .collect(),
    )
}

fn xor_instr() -> Iso {
    reg_reg(0b0010, xor_core())
}

/// EXCH rd, rs: with r[rd] at the r0 slot and the VALUE of r[rs] in s0
/// (swap-fetch), a predicated swap of the two words is 8 native
/// 1-controlled swaps; the swapped s0 content is written back into
/// r[rs] by the unfetch.
fn exch_instr() -> Iso {
    predicated(&r_pred(0b0011), with_two_regs(R_RD, R_RS, exch_core()))
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

/// CCNOT rd, rs, rt: byte-parallel — `r[rd] ^= r[rs] & r[rt]` over all 8 bits.
///
/// Convention B (BR as 2nd operand buffer): swap-fetch r[rt] into BR
/// (field 9) via a content-agnostic swap (BR's entry value rides in
/// r[rt]'s cell for the core and is written back on exit — no BR=0
/// assumption). The fetch is sequenced OUTSIDE `with_two_regs` so the
/// core sees [r0-slot(rd), s0(rs), BR(rt), s1(pred)] as four fields.
///
/// Each bit lane `i` applies a predicate-gated CCC-NOT:
///   r[rd][i] ^= pred ∧ r[rs][i] ∧ r[rt][i]
///
/// Value-level aliasing caveat: rs==rt gives rd^=BR_entry∧rt (bijective
/// but unspecified). The P1 checker / `encode` already rejects via
/// CcnotNotDistinct.
/// CCNOT's rt→BR swap-fetch bracket (self-inverse). select_operand(R_RT)
/// routes the (reg∪scratch) operand rt to the fixed r0-slot, swaps it ↔
/// BR, then routes back. A-0 (BLOCKER-2 fix): reg∪scratch crossbar.
/// Content-agnostic: whatever was in BR rides in operand rt's cell.
///
/// Split out (A-0 shared-crossbar) so it can be hoisted as a shared
/// bracket in `decode_execute_shared`: the fetch operates on the
/// UN-permuted register file, so it must sit outside the shared `sel_rd`
/// crossbar (but inside/beside `fetch_rs`, disjoint slots).
fn ccnot_fetch_rt_br() -> Iso {
    let sel_rt = select_operand(R_RT, operand_field);
    let swap_br = at_fields(Iso::SwapProd(word8(), word8()), &[reg_field(0), BR_FIELD]);
    Iso::seq(sel_rt.clone(), Iso::seq(swap_br, Iso::sym(sel_rt)))
}

/// B1.3 — CONFIG-GATED rt→B(BR) fetch for the universal shared bracket.
/// Routes rt to the r0-slot (content-agnostic, always), swaps r0-slot↔BR
/// ONLY when a controlled op (Ccnot or Cswap) is decoded — gated on the
/// held config one-hots `CFG_CCNOT ∨ CFG_CSWAP`, then routes back.
///
/// This gating is the fix for the reg+imm rt-field-aliasing hazard: for
/// reg+imm ops the rt field is 0 (don't-care), so an UNGATED rt→BR fetch
/// would move r[0]↔BR and — when rd names r0 — collide with the rd routing
/// and corrupt r[0]/BR. Gating the swap on the controlled-op lines makes it
/// the exact identity for every reg+imm word (and Swapbr) while delivering
/// rt→B for Ccnot/Cswap. Since the two config lines are mutually exclusive,
/// the two gated swaps compose to `swap iff (ccnot ∨ cswap)`.
///
/// The config one-hots live in s1; this fetch must therefore run INSIDE the
/// `decode_config` bracket (one-hots computed) but OUTSIDE fetch_rs/sel_rd
/// (it fetches over the un-permuted file). The swap targets are r0-slot and
/// BR, both position-stable at this nesting depth. Wire layout of the gated
/// swap core [r0-slot, BR, s1]: r0-slot = 0..W, BR = W..2W, s1 = 2W..3W.
fn shared_fetch_rt_br_gated() -> Iso {
    let sel_rt = select_operand(R_RT, operand_field);
    let gated_swap = |cfg: usize| {
        on_fields(
            &[reg_field(0), BR_FIELD, AUX_PRED_FIELD],
            (0..W)
                .map(|i| gate_at(3 * W, &[2 * W + cfg, i, W + i], fredkin_prim()))
                .collect(),
        )
    };
    // swap iff CFG_CCNOT, then swap iff CFG_CSWAP (mutually exclusive ⇒ OR).
    let swap = Iso::seq(gated_swap(CFG_CCNOT), gated_swap(CFG_CSWAP));
    Iso::seq(sel_rt.clone(), Iso::seq(swap, Iso::sym(sel_rt)))
}

/// CCNOT's state-changing core on the 4 fixed slots
/// [r0-slot(rd), s0(rs), BR(rt-value), s1(pred)] — gated on the pred bit.
/// Wire layout: r0-slot = 0..W, s0 = W..2W, BR = 2W..3W, s1 = 3W..4W.
/// Per-bit CCC-NOT: target = bit i, controls = pred(3W), rs-bit(W+i), rt-bit(2W+i).
fn ccnot_core() -> Iso {
    let gates = (0..W)
        .map(|i| gate_at(4 * W, &[3 * W, W + i, 2 * W + i, i], cccnot_prim()))
        .collect();
    on_fields(
        &[reg_field(0), AUX_OPERAND_FIELD, BR_FIELD, AUX_PRED_FIELD],
        gates,
    )
}

fn ccnot_instr() -> Iso {
    let fetch_rt_br = ccnot_fetch_rt_br();
    // Body: fetch_rt_br ; with_two_regs(R_RD, R_RS, core) ; sym(fetch_rt_br)
    let body = Iso::seq(
        fetch_rt_br.clone(),
        Iso::seq(
            with_two_regs(R_RD, R_RS, ccnot_core()),
            Iso::sym(fetch_rt_br),
        ),
    );
    predicated(&r_pred(0b1011), body)
}

/// XCHG rd, sx: gated swap of the routed register and scratch slots.
///
/// A-0 (BLOCKER-1 fix): the scratch crossbar scrambles s1's position, so
/// the predicate cannot be read directly at the swap. It is instead copied
/// onto IR[0] (a routing-stable IR wire), which the reg/scratch crossbars
/// never touch. But A-0 turned IR[0] into the I-format immediate LSB, so
/// IR[0] is NO LONGER architecturally zero — an odd-imm I-op leaves IR[0]=1.
/// A single-control swap on IR[0] would therefore fire spuriously on every
/// odd-imm I-op. The swap is now **2-controlled** on `fmt (IR bit 31) ∧
/// IR[0]-copy`:
///   - fmt is routing-stable (bit 31 lives in IR_b3, untouched by the reg/
///     scratch crossbars) and is 0 for every I-format word ⇒ swap inert for
///     all I-ops regardless of the imm LSB;
///   - for an R-format XCHG word IR[0] is a spare bit = 0, so the CNOT
///     copies the predicate onto it (and removes it on exit), giving
///     `fmt(=1) ∧ pred`.
/// The copy is self-inverse and restores IR in every case.
fn xchg_instr() -> Iso {
    // [IR_lo, s1]: pad = wire 0, pred = wire 8.
    let pad_copy = on_fields(
        &[IR_LO_FIELD, AUX_PRED_FIELD],
        vec![gate_at(2 * W, &[W, 0], cnot_prim())],
    );
    let selr = select_operand(R_RD, operand_field);
    let sels = select_scratch(R_RT);
    // [IR_b3, IR_lo, r0-slot, s0-slot]: fmt = wire 7 (IR bit 31),
    // pred-copy = wire 8 (IR bit 0). 2-controlled swap on fmt ∧ IR[0].
    let fmt_wire = FMT_BIT % W; // bit 7 within IR_b3
    let gsw = on_fields(
        &[IR_B3_FIELD, IR_LO_FIELD, reg_field(0), scratch_field(0)],
        (0..W)
            .map(|i| {
                gate_at(
                    4 * W,
                    &[fmt_wire, W, 2 * W + i, 3 * W + i],
                    gated_fredkin_prim(),
                )
            })
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

/// Controlled swap of one CpuState field with one MemState field,
/// gated on a bit in a CpuState control field.
///
/// Carrier: `(fields(n_left), fields(n_right))` = composite.
/// Flattens the composite to `fields(n_left + n_right)`, routes the
/// 3 fields (ctrl, left_word, right_word) to the head, applies the
/// gated Fredkin swap, and un-routes.
fn cross_factor_gated_swap(
    ctrl_field: usize,
    ctrl_bit: usize,
    left_word: usize,
    right_word: usize,
    n_left: usize,
    n_right: usize,
) -> Iso {
    use crate::addressing::split_fields;
    assert!(ctrl_field < n_left, "ctrl_field out of range");
    assert!(left_word < n_left, "left_word out of range");
    assert!(ctrl_bit < W, "ctrl_bit out of range");
    assert!(right_word < n_right, "right_word out of range");
    assert_ne!(ctrl_field, left_word, "ctrl_field == left_word");

    let n_total = n_left + n_right;
    let ctrl_flat = ctrl_field;
    let lw_flat = left_word;
    let rw_flat = n_left + right_word;

    let n3 = 3 * W;
    let gates: Vec<Iso> = (0..W)
        .map(|i| gate_at(n3, &[ctrl_bit, W + i, 2 * W + i], fredkin_prim()))
        .collect();
    let core = on_fields_n(&[ctrl_flat, lw_flat, rw_flat], gates, n_total);

    let split = split_fields(n_left, n_right);
    Iso::seq(Iso::sym(split.clone()), Iso::seq(core, split))
}

/// SWAPBR rd: gated swap of BR ↔ r[rd] (ADR-0034 D1, Pendulum
/// semantics). EXCH-shaped: route r[rd] to the r0 slot via the R_RD
/// crossbar, perform a predicated swap of r0-slot and BR, route back.
/// No instruction writes PC; the branch offset in BR is consumed by
/// the end-of-step `pc_add_br ; pc_inc`.
/// SWAPBR's state-changing core on [r0-slot(rd), BR, s1] — gated swap of
/// r0-slot ↔ BR. Split out (A-0 shared crossbar) so SWAPBR can join the
/// shared Group A bracket: it needs only rd→r0-slot (which Group A's
/// sel_rd provides) and reads BR (untouched by Group A's fetch_rs/sel_rd
/// over registers). Group A's fetch_rs harmlessly routes r[rs=0]→s0 for a
/// SWAPBR word (SWAPBR ignores s0; restored on exit).
fn swapbr_core() -> Iso {
    on_fields(
        &[reg_field(0), BR_FIELD, AUX_PRED_FIELD],
        (0..W)
            .map(|i| gate_at(3 * W, &[2 * W, i, W + i], fredkin_prim()))
            .collect(),
    )
}

fn swapbr_instr() -> Iso {
    // A-0 (BLOCKER-2 fix): route rd over reg∪scratch.
    predicated(&r_pred(0b1000), with_target(R_RD, swapbr_core()))
}

/// ADDI core on [r0-slot, s0, IR_b0, s1] (A-0): CNOT-mask the imm8
/// (IR bits 0..8, all in byte 0) onto s0, gated TK add, unmask.
/// Requires s0 = 0.
fn addi_core() -> Iso {
    let n = 4 * W;
    let pred = 3 * W;
    // imm bit j = IR bit j = IR_b0 bit j = wire 2W + j.
    let imm_wire = |j: usize| 2 * W + j;
    let mask: Vec<Iso> = (0..W)
        .map(|j| gate_at(n, &[imm_wire(j), W + j], cnot_prim()))
        .collect();
    let mut gates = mask.clone();
    gates.extend(gated_tk_gates(n, pred, 0, W));
    gates.extend(mask);
    on_fields(
        &[reg_field(0), AUX_OPERAND_FIELD, IR_B0_FIELD, AUX_PRED_FIELD],
        gates,
    )
}

fn addi_instr() -> Iso {
    predicated(&i_pred(0b000), with_target(I_RD, addi_core()))
}

fn subi_instr() -> Iso {
    predicated(&i_pred(0b001), with_target(I_RD, Iso::sym(addi_core())))
}

/// XORI core on [r0-slot, IR_b0, s1] (A-0): gated CNOTs straight from
/// the IR immediate bits (IR bits 0..8, all in byte 0, act as controls
/// — the legal fanout).
fn xori_core() -> Iso {
    let n = 3 * W;
    let pred = 2 * W;
    // imm bit j = IR bit j = IR_b0 bit j = wire W + j.
    let imm_wire = |j: usize| W + j;
    let gates: Vec<Iso> = (0..W)
        .map(|j| gate_at(n, &[pred, imm_wire(j), j], toffoli_prim()))
        .collect();
    on_fields(&[reg_field(0), IR_B0_FIELD, AUX_PRED_FIELD], gates)
}

fn xori_instr() -> Iso {
    predicated(&i_pred(0b010), with_target(I_RD, xori_core()))
}

/// ROTLI core on [r0-slot, IR_b0, s1] (A-0): barrel rotate controlled
/// directly by IR imm bits 0..2 (IR bits 0..2, byte 0).
fn rotli_core() -> Iso {
    let n = 3 * W;
    let pred = 2 * W;
    // rotate-amount bit k = IR bit k = IR_b0 bit k = wire W + k.
    on_fields(
        &[reg_field(0), IR_B0_FIELD, AUX_PRED_FIELD],
        gated_rotl_gates(n, pred, 0, |k| W + k),
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

/// The 17 live TISC-1 instructions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Xor,
    Exch,
    Rotl,
    Rotr,
    Cswap,
    Ccnot,
    Xchg,
    Swapbr,
    Mexch,
    MexchReg,
    Addi,
    Subi,
    Xori,
    Rotli,
    Rotri,
}

impl Op {
    /// The 15 CpuState-only instructions (all except MEXCH/MEXCHR).
    /// Used by `decode_execute` (the ring step's decoder).
    pub const ALL: [Op; 15] = [
        Op::Add,
        Op::Sub,
        Op::Xor,
        Op::Exch,
        Op::Rotl,
        Op::Rotr,
        Op::Cswap,
        Op::Ccnot,
        Op::Xchg,
        Op::Swapbr,
        Op::Addi,
        Op::Subi,
        Op::Xori,
        Op::Rotli,
        Op::Rotri,
    ];
}

/// The predicated CpuState-level iso for one instruction: identity unless
/// IR's format/opcode bits match `op`, in which case the instruction is
/// applied to the registers IR names.
///
/// Panics on `Op::Mexch` / `Op::MexchReg` — these operate on the
/// composite carrier via emit/mem/absorb; use [`system_decode_execute`]
/// for the oracle path.
pub fn instruction(op: Op) -> Iso {
    match op {
        Op::Add => add_instr(),
        Op::Sub => sub_instr(),
        Op::Xor => xor_instr(),
        Op::Exch => exch_instr(),
        Op::Rotl => rotl_instr(),
        Op::Rotr => rotr_instr(),
        Op::Cswap => cswap_instr(),
        Op::Ccnot => ccnot_instr(),
        Op::Xchg => xchg_instr(),
        Op::Swapbr => swapbr_instr(),
        Op::Mexch | Op::MexchReg => {
            panic!("Op::{op:?} operates on (CpuState, MemState); use system_decode_execute")
        }
        Op::Addi => addi_instr(),
        Op::Subi => subi_instr(),
        Op::Xori => xori_instr(),
        Op::Rotli => rotli_instr(),
        Op::Rotri => rotri_instr(),
    }
}

/// Decode+execute over a subset of the instruction set (useful for
/// incremental verification and cost accounting).
///
/// Each op re-routes its own operands through the reg∪scratch crossbar
/// (~40 crossbar instances across the full set). This REPLICATED form is
/// kept for the per-instruction tests and incremental subsets; the full
/// ring decoder ([`decode_execute`]) uses the SHARED-crossbar form below.
pub fn decode_execute_ops(ops: &[Op]) -> Iso {
    assert!(!ops.is_empty(), "nonempty op list");
    seq_balanced(ops.iter().map(|&o| instruction(o)).collect())
}

// ══════════════════════════════════════════════════════════════════════
// A-0 shared operand crossbar (decode_execute_shared)
// ══════════════════════════════════════════════════════════════════════
//
// The replicated form routes rd/rs through the 16-way crossbar ONCE PER
// OP → ~2 crossbars/op × ~11 common ops + the 3-operand/scratch ops →
// the netlist bloat that OOMs the energy measurement (93.5 GB) and
// ~doubles step_j. The SHARED form routes operands ONCE per group into
// the fixed working slots [r0-slot(rd), s0(rs), s1(pred)], runs every
// op-core in the group gated on its opcode predicate against those
// slots, then routes back once. Empirically this halves the
// decode_execute lowered netlist (9730 → 4930 cells) and its lowering
// peak (~77 → ~39 GB), undoing the ~2× step_j regression.
//
// Correctness (why sharing is behaviour-identical to the sequential form):
// every op in a group carries the SAME routing bracket B (= a self-inverse
// composition of crossbar/fetch isos). The sequential form is
//   Σ_i  B ; predicated_i(core_i) ; B⁻¹
// and between consecutive ops `B⁻¹ ; B = id` cancels EXACTLY (adjoint
// composition), so it equals
//   B ; ( Σ_i predicated_i(core_i) ) ; B⁻¹
// = the shared form. This is pure iso algebra — it holds regardless of
// what the crossbar permutes, provided the bracket is identical and the
// per-op predicate/core touch only the fixed slots + IR/s1 (disjoint from
// the routed transit). Ops whose routing bracket DIFFERS (Cswap routes rt
// not rd to r0-slot + copies rd[0]; Xchg drives a 2nd scratch crossbar;
// Swapbr routes rd↔BR) are kept as their own replicated `instruction(op)`
// — they are a single crossbar each, not the dominant cost.

/// B1.2 — the UNIFIED reg+imm shared block. One bracket + one config
/// decode + one gated ALU serves ALL 11 reg+imm ops (Add/Sub/Xor/Exch/
/// Rotl/Rotr + Addi/Subi/Xori/Rotli/Rotri). rd→D(r0-slot) always; the
/// A-slot(s0) source is fmt-selected: R-format swap-fetches rs into s0
/// (fmt-gated, INERT for I-format); I-format masks the IR immediate into
/// s0 (¬fmt-gated). Both A-sources are self-inverse brackets that restore
/// s0, and are mutually exclusive by fmt, so exactly one feeds A.
///
/// The immediates thus reuse B1.1's SAME adder (Addi/Subi via CFG_ADD_*),
/// xor sub-block (Xori via CFG_XOR), and rotator (Rotli/Rotri via
/// CFG_ROT_*) — the 4→1 adder + 4→1 rotator core-collapse that removes the
/// separate `group_b_shared` block entirely (B1.2 §6).
///
/// Structure (all self-inverse-bracketed):
///   fetch_rs_fmt                      -- rs value → s0 iff fmt (OUTSIDE sel_rd)
///     sel_rd                          -- rd → r0-slot
///       imm_to_a                      -- IR imm → s0 iff ¬fmt
///         decode_config               -- opcode → one-hot config lines (s1)
///           ALU(sub-blocks gated)     -- one config'd core on [D, A, s1]
///         decode_config⁻¹
///       imm_to_a⁻¹
///     sel_rd⁻¹
///   fetch_rs_fmt⁻¹
///
/// Swapbr / Ccnot / Cswap / Xchg stay separate (their brackets differ —
/// see `decode_execute_shared`).
fn reg_imm_shared() -> Iso {
    let fetch_rs = fetch_rs_fmt_gated();
    let inner = with_target(R_RD, reg_imm_alu());
    Iso::seq(fetch_rs.clone(), Iso::seq(inner, Iso::sym(fetch_rs)))
}

/// B1.3 — the UNIVERSAL 3-operand shared block. Extends `reg_imm_shared`
/// to also serve Ccnot (R 0b1011) and Cswap (R 0b0110) via ONE bracket
/// that fetches rd→D(r0-slot), rs→A(s0), rt→B(BR) once, then config-gates
/// EVERY core (reg+imm + ccnot + cswap) on the fixed [D, A, B, s1] slots.
///
/// The config decode moved its 2 transient workspace bits (prefix/chain)
/// out of s1 into **s0** (`config_decode_fields` / `CFG_*_S0`), freeing 2
/// s1 bits for CFG_CCNOT + CFG_CSWAP one-hot lines. This is the s0-workspace
/// resolution of the B1.3 config-scratch wall (design §8/§9): s0 is 0 at
/// config-decode time and is restored to 0 by the decode forward pass
/// BEFORE the A-operand deposit (fetch_rs/imm_to_a) writes it.
///
/// Bracket nesting (all self-inverse):
///   decode_config             -- opcode → one-hots (s1); s0=0 workspace
///     fetch_rt_br_gated       -- rt → BR iff (cfg_ccnot ∨ cfg_cswap)
///       fetch_rs_fmt          -- rs → s0 iff fmt (OUTSIDE sel_rd)
///         sel_rd              -- rd → r0-slot(D)
///           imm_to_a          -- IR imm → s0 iff ¬fmt
///             ALU cores       -- all config-gated on [D, A=s0, B=BR, s1]
///           imm_to_a⁻¹
///         sel_rd⁻¹
///       fetch_rs_fmt⁻¹
///     fetch_rt_br_gated⁻¹
///   decode_config⁻¹
///
/// `decode_config` is OUTERMOST so it runs with s0 = 0 (its transient
/// prefix/chain workspace lives in s0). It writes the held one-hots into s1
/// and restores s0 to 0 on its forward pass, BEFORE fetch_rs/imm_to_a
/// deposit the A-operand into s0. The rt→BR fetch is CONFIG-GATED (fires
/// only for Ccnot/Cswap) so it is the exact identity for reg+imm ops — this
/// avoids the rt-field=0 aliasing corruption of r[0]/BR when rd names r0.
/// Cswap reads its control D[0] directly (no rd[0] copy). Swapbr/Xchg keep
/// their own separate blocks (their brackets differ).
fn shared_alu_block() -> Iso {
    let fetch_rt_br = shared_fetch_rt_br_gated();
    let fetch_rs = fetch_rs_fmt_gated();
    let decode = alu_config_decode();
    let imm = imm_to_a();

    // The gated ALU core (reg+imm + ccnot + cswap), all on [D, A, B, s1].
    let alu = seq_balanced(vec![
        add_core_at(CFG_ADD_FWD),            // Add / Addi
        Iso::sym(add_core_at(CFG_ADD_REV)),  // Sub / Subi  = adder adjoint
        xor_core_at(CFG_XOR),                // Xor / Xori
        exch_core_at(CFG_EXCH),              // Exch (R-only)
        rotl_core_at(CFG_ROT_FWD),           // Rotl / Rotli
        Iso::sym(rotl_core_at(CFG_ROT_REV)), // Rotr / Rotri = rotator adjoint
        ccnot_core_at(CFG_CCNOT),            // Ccnot (R-only)
        cswap_core_at(CFG_CSWAP),            // Cswap (R-only)
    ]);

    // Innermost: imm_to_a ; ALU ; imm_to_a⁻¹ on the routed slots.
    let with_imm = Iso::seq(imm.clone(), Iso::seq(alu, Iso::sym(imm)));
    // sel_rd routes rd → r0-slot around the imm+ALU.
    let with_rd = with_target(R_RD, with_imm);
    // fetch_rs deposits rs → s0 (fmt-gated) around sel_rd.
    let with_rs = Iso::seq(fetch_rs.clone(), Iso::seq(with_rd, Iso::sym(fetch_rs)));
    // fetch_rt_br delivers rt → BR (config-gated) around the A-deposit.
    let with_rt = Iso::seq(
        fetch_rt_br.clone(),
        Iso::seq(with_rs, Iso::sym(fetch_rt_br)),
    );
    // decode_config computes the one-hots (s0=0 here) around everything.
    Iso::seq(decode.clone(), Iso::seq(with_rt, Iso::sym(decode)))
}

/// fmt-gated swap-fetch of `r[rs]` into s0: `sel_rs ; (swap iff fmt) ;
/// sel_rs⁻¹`. Identical to `fetch_operand(R_RS)` EXCEPT the r0-slot↔s0
/// word swap carries fmt (IR bit 31) as an additional control, so for
/// I-format (fmt=0) the swap is the identity and `sel_rs ; id ; sel_rs⁻¹
/// = id` — the "rs field" bits (which are immediate bits under I-format)
/// still steer the crossbar, but it is a pure self-inverse permutation and
/// nothing is fetched. For R-format (fmt=1) it is exactly `fetch_operand`.
fn fetch_rs_fmt_gated() -> Iso {
    let (fmt_field, fmt_bit) = ir_bit(FMT_BIT);
    let sel = select_operand(R_RS, operand_field);
    // fmt-gated word swap operand_field(0) ↔ AUX_OPERAND_FIELD.
    let gated_swap = ctrl_word_swap(fmt_field, fmt_bit, operand_field(0), AUX_OPERAND_FIELD);
    Iso::seq(sel.clone(), Iso::seq(gated_swap, Iso::sym(sel)))
}

/// ¬fmt-gated immediate→A mask: for j in 0..8, `s0[j] ^= ¬fmt ∧ imm[j]`
/// where `imm[j]` = IR bit j (= IR_B0 bit j). Requires s0 = 0 at entry
/// (decoder scratch is quiescent; the fmt-gated rs-fetch is inert for
/// I-format so it leaves s0 = 0). Self-inverse: the adjoint clears s0
/// back to 0. For R-format (fmt=1) this is the identity, so s0 = the
/// fetched rs value is untouched. The core lifts over [s0, IR_b0, IR_b3]
/// (IR_b3 holds fmt bit 31 = local bit 7).
fn imm_to_a() -> Iso {
    let n = 3 * W;
    // local wires: s0 = 0..W, IR_b0 = W..2W, IR_b3 = 2W..3W.
    let s0 = |j: usize| j;
    let imm = |j: usize| W + j;
    let fmt_local = 2 * W + (FMT_BIT % W); // fmt bit within IR_b3
    let mut gates: Vec<Iso> = Vec::new();
    gates.push(at_wire(n, fmt_local, not_iso())); // ¬fmt
    for j in 0..W {
        gates.push(gate_at(n, &[fmt_local, imm(j), s0(j)], toffoli_prim()));
    }
    gates.push(at_wire(n, fmt_local, not_iso())); // restore fmt
    on_fields(&[AUX_OPERAND_FIELD, IR_B0_FIELD, IR_B3_FIELD], gates)
}

/// B1.1 measurement hook: the PRE-B1.1 Group-A block (one shared route,
/// six per-op `predicated(core)` blocks — six opcode AND-chains + six
/// cores). Kept ONLY so `poc_b11_shared_alu_measure` can lower it and
/// report the Group-A-portion delta vs the shared configurable ALU
/// (`group_a_shared`). NOT wired into the decoder.
pub fn group_a_pre_b11_for_measurement() -> Iso {
    let cores = seq_balanced(vec![
        predicated(&r_pred(0b0000), add_core()),            // Add
        predicated(&r_pred(0b0001), Iso::sym(add_core())),  // Sub
        predicated(&r_pred(0b0010), xor_core()),            // Xor
        predicated(&r_pred(0b0011), exch_core()),           // Exch
        predicated(&r_pred(0b0100), rotl_core()),           // Rotl
        predicated(&r_pred(0b0101), Iso::sym(rotl_core())), // Rotr
    ]);
    with_two_regs(R_RD, R_RS, cores)
}

/// B1.1 measurement hook: the shared configurable ALU Group-A block.
/// B1.2: the Group-A-only measurement view is `with_two_regs(R_RD, R_RS,
/// group_a_alu_view())` — the register-only fold (no imm A-source, no
/// I-format config lines), kept so the B1.1 Group-A-portion Δ is still
/// reproducible after the unification.
pub fn group_a_shared_for_measurement() -> Iso {
    with_two_regs(R_RD, R_RS, group_a_alu_view())
}

/// B1.2 measurement hook: the UNIFIED reg+imm block (was group_a_shared +
/// group_b_shared). Its balanced cell mass vs (B1.1 group_a + group_b) is
/// the apples-to-apples "did we remove the group_b block" number.
pub fn reg_imm_shared_for_measurement() -> Iso {
    reg_imm_shared()
}

/// B1.3 measurement/equiv hook: the UNIVERSAL 3-operand shared block
/// (reg+imm + Ccnot + Cswap). Its balanced mass vs the pre-B1.3 (reg_imm
/// 185864 + ccnot 75520 + cswap 63770) is the "did the two private
/// crossbars collapse" number; it is also the isolated block the B1.3
/// whole-domain equiv test evaluates (all other blocks — xchg/swapbr — are
/// the identity for reg+imm/ccnot/cswap opcodes).
pub fn shared_alu_block_for_measurement() -> Iso {
    shared_alu_block()
}

/// B1.2 measurement hook: the pre-B1.2 group_b (5 per-op predicated
/// immediate cores under one shared sel_rd), so its balanced mass can be
/// summed with B1.1's group_a for the removal comparison. NOT wired.
pub fn group_b_pre_b12_for_measurement() -> Iso {
    let sel_rd = select_operand(R_RD, operand_field);
    let cores = seq_balanced(vec![
        predicated(&i_pred(0b000), addi_core()),            // Addi
        predicated(&i_pred(0b001), Iso::sym(addi_core())),  // Subi
        predicated(&i_pred(0b010), xori_core()),            // Xori
        predicated(&i_pred(0b011), rotli_core()),           // Rotli
        predicated(&i_pred(0b100), Iso::sym(rotli_core())), // Rotri
    ]);
    Iso::seq(sel_rd.clone(), Iso::seq(cores, Iso::sym(sel_rd)))
}

/// B1 measurement hook: the top-level blocks of `decode_execute_shared`, so
/// each block's BALANCED cell mass can be measured individually. This
/// grounds the B-1 re-sequencing decision — where the un-shared *routing*
/// mass actually lives (the controlled ops each carry a PRIVATE crossbar).
pub fn decode_blocks_for_measurement() -> Vec<(&'static str, Iso)> {
    vec![
        // B1.3: the universal block absorbs reg+imm + ccnot + cswap.
        ("shared_alu", shared_alu_block()),
        ("xchg", xchg_instr()),
        ("swapbr", swapbr_instr()),
    ]
}

// ── B1.1/B1.3 shared configurable ALU config layout ─────────────────────
//
// The HELD one-hot config lines live in s1 (`AUX_PRED_FIELD`, decoder-
// reserved — never a legal operand, so safe as config scratch, and
// position-stable: `select_operand` excludes s0/s1 from the level-3
// crossbar). B1.3 fills all 8 s1 bits with HELD one-hot lines (add_fwd/rev,
// xor, exch, rot_fwd/rev, ccnot, cswap) — NO transient decode-workspace
// bits remain in s1.
//
// The 2 TRANSIENT decode-workspace bits (prefix + chain) live in **s0**
// (`AUX_OPERAND_FIELD`), the OTHER decoder-reserved, position-stable word.
// s0 is guaranteed 0 at config-decode time (the A-operand deposit —
// fetch_rs/imm_to_a — runs INSIDE the decode bracket, AFTER decode's
// forward pass restores its s0 workspace to 0; see `reg_imm_alu`). The
// compute/uncompute of prefix+chain restores s0 to 0 by the end of the
// decode forward pass — the L15-critical invariant `imm_to_a` requires.
const CFG_ADD_FWD: usize = 0; // Add  (R 0b0000): forward adder
const CFG_ADD_REV: usize = 1; // Sub  (R 0b0001): adjoint adder
const CFG_XOR: usize = 2; //     Xor  (R 0b0010)
const CFG_EXCH: usize = 3; //    Exch (R 0b0011)
const CFG_ROT_FWD: usize = 4; // Rotl (R 0b0100): forward rotator
const CFG_ROT_REV: usize = 5; // Rotr (R 0b0101): adjoint rotator
const CFG_CCNOT: usize = 6; //   Ccnot (R 0b1011): byte-parallel rd^=rs&rt
const CFG_CSWAP: usize = 7; //   Cswap (R 0b0110): conditional swap rd[0]

// Transient decode-workspace bits, relocated into s0 (B1.3). Addressed
// relative to the s0 field-slot in the extended config-decode carrier.
const CFG_PREFIX_S0: usize = 0; // s0 bit 0: prefix `g` (fmt ∧ ¬op3, ¬fmt, …)
const CFG_CHAIN_S0: usize = 1; //  s0 bit 1: transient op-body AND

/// s1 wire for a HELD config one-hot bit `k` within the config-decode core,
/// which lifts over `config_decode_fields()` = [IR_b0..b3, s1, s0]; IR bits
/// are wires 0..31, s1 bit `k` is wire 32+`k`, s0 bit `k` is wire 40+`k`.
const fn cfg_wire(k: usize) -> usize {
    32 + k
}

/// s0 wire for a TRANSIENT decode-workspace bit `k` (prefix/chain).
const fn cfg_ws_wire(k: usize) -> usize {
    5 * W + k // s0 is field-slot 5 in config_decode_fields()
}

/// The six fields the config decode lifts over: four IR bytes + s1 (held
/// one-hots) + s0 (transient prefix/chain workspace). Extends the old
/// `pred_fields()` [IR_b0..b3, s1] with s0 as the transient workspace so
/// s1's 8 bits can all hold one-hot lines (B1.3).
fn config_decode_fields() -> [usize; 6] {
    [
        IR_B0_FIELD,
        IR_B1_FIELD,
        IR_B2_FIELD,
        IR_B3_FIELD,
        AUX_PRED_FIELD,   // s1 (slot 4): held one-hots
        AUX_OPERAND_FIELD, // s0 (slot 5): transient workspace
    ]
}

/// Decode BOTH the R-format Group-A opcode AND the I-format immediate
/// opcode into the SIX shared one-hot config lines (B1.2 — the immediates
/// reuse the SAME adder/xor/rotator sub-blocks as their register siblings,
/// so they fire the SAME config lines).
///
/// Each *R*-format Group-A op shares the prefix `g_r = fmt ∧ ¬op3` (fmt=
/// bit31, op3=opcode_bit(3)); the three low opcode bits (op2,op1,op0) then
/// select exactly one of Add/Sub/Xor/Exch/Rotl/Rotr. Each *I*-format op
/// shares the prefix `g_i = ¬fmt` (i_pred tests fmt=0 + the three low
/// opcode bits ONLY — op3/op4/op5 are don't-cares, exactly as the pre-B1.2
/// `i_pred`); the low-3 opcode bits select Addi/Subi/Xori/Rotli/Rotri.
///
/// The two families land on the SAME config lines by function, NOT by
/// low-3 pattern (a register op and an immediate op with the same low-3
/// bits fire DIFFERENT lines because the prefix — fmt — differs):
///   Add /Addi  → CFG_ADD_FWD    Sub /Subi  → CFG_ADD_REV
///   Xor /Xori  → CFG_XOR        Rotl/Rotli → CFG_ROT_FWD
///   Rotr/Rotri → CFG_ROT_REV    Exch (R-only) → CFG_EXCH
///
/// We compute each prefix once into `CFG_PREFIX`, then for each opcode land
/// `final ^= g ∧ op2' ∧ op1' ∧ op0'` (primes = the opcode literal, NOT-
/// adjusted for 0-bits) using `CFG_CHAIN` as a single transient:
/// `chain ^= op2'∧op1'∧op0'` (CCC-NOT), then `final ^= g ∧ chain`
/// (Toffoli), then uncompute `chain`. Only one chain bit is ever live, so
/// all six one-hots + prefix fit s1's 8 bits. CFG_PREFIX is reused between
/// the R and I families (computed, consumed, and uncomputed within each
/// family before the next prefix is computed).
///
/// Self-inverse-bracketed by the caller (`decode ; ALU ; decode⁻¹`): the
/// config bits start and end at 0. Opcodes are mutually exclusive (fmt +
/// disjoint low-3 patterns), so at most one one-hot fires — the mutual-
/// exclusion the ALU Branch relies on.
fn alu_config_decode() -> Iso {
    alu_config_decode_families(true)
}

/// R-format opcode literal bits `[op2, op1, op0]` for `op` (the low three
/// opcode bits). The prefix (fmt / op3) is landed separately per family.
const fn low3(op: u8) -> [bool; 3] {
    [(op >> 2) & 1 == 1, (op >> 1) & 1 == 1, op & 1 == 1]
}

/// Config decode over the R family (`include_i = false`) or BOTH families
/// (`include_i = true`, the wired form). The R-only form reproduces the
/// exact B1.1 decode for the `group_a_shared_for_measurement` hook.
fn alu_config_decode_families(include_i: bool) -> Iso {
    let n = 6 * W; // config_decode core: 4 IR bytes + s1 + s0
    let op = |k: usize| opcode_bit(k); // global IR wire of opcode bit k
    let prefix = cfg_ws_wire(CFG_PREFIX_S0); // s0 bit 0 (transient)
    let chain = cfg_ws_wire(CFG_CHAIN_S0); //  s0 bit 1 (transient)
    let mut gates: Vec<Iso> = Vec::new();

    // Land one op: `final ^= g ∧ (op2==l2 ∧ op1==l1 ∧ op0==l0)`, with the
    // prefix `g` already in CFG_PREFIX_S0; CFG_CHAIN_S0 is the transient AND.
    let land_op = |final_bit: usize, lits: [bool; 3]| -> Vec<Iso> {
        let lit_wires = [op(2), op(1), op(0)];
        let nots: Vec<Iso> = (0..3)
            .filter(|&i| !lits[i])
            .map(|i| at_wire(n, lit_wires[i], not_iso()))
            .collect();
        let mut g: Vec<Iso> = Vec::new();
        // chain ^= op2' ∧ op1' ∧ op0'  (CCC-NOT over the three literals)
        g.extend(nots.clone());
        g.push(gate_at(
            n,
            &[lit_wires[0], lit_wires[1], lit_wires[2], chain],
            cccnot_prim(),
        ));
        g.extend(nots.clone()); // un-NOT the literals (restore IR)
        // final ^= g ∧ chain
        g.push(gate_at(
            n,
            &[prefix, chain, cfg_wire(final_bit)],
            toffoli_prim(),
        ));
        // uncompute chain (re-NOT, CCC-NOT, un-NOT) — restores chain to 0.
        g.extend(nots.clone());
        g.push(gate_at(
            n,
            &[lit_wires[0], lit_wires[1], lit_wires[2], chain],
            cccnot_prim(),
        ));
        g.extend(nots);
        g
    };

    // ── R-format family, ¬op3 branch: prefix g_r = fmt ∧ ¬op3 → prefix ──
    // Serves the six Group-A/B register ops (op3=0) AND Cswap (op3=0,
    // low3=110). All seven fire their DISTINCT low-3 pattern; each pattern
    // is unique so no two config lines co-fire.
    gates.push(at_wire(n, op(3), not_iso()));
    gates.push(gate_at(n, &[FMT_BIT, op(3), prefix], toffoli_prim()));
    gates.push(at_wire(n, op(3), not_iso()));
    // (final_bit, [op2,op1,op0] literal). Bit b = 1 → literal true.
    for (final_bit, lits) in [
        (CFG_ADD_FWD, [false, false, false]), // Add   R 0b_000
        (CFG_ADD_REV, [false, false, true]),  // Sub   R 0b_001
        (CFG_XOR, [false, true, false]),      // Xor   R 0b_010
        (CFG_EXCH, [false, true, true]),      // Exch  R 0b_011
        (CFG_ROT_FWD, [true, false, false]),  // Rotl  R 0b_100
        (CFG_ROT_REV, [true, false, true]),   // Rotr  R 0b_101
        (CFG_CSWAP, low3(0b0110)),            // Cswap R 0b_110
    ] {
        gates.extend(land_op(final_bit, lits));
    }
    // Uncompute g_r (¬op3 branch) → prefix returns to 0.
    gates.push(at_wire(n, op(3), not_iso()));
    gates.push(gate_at(n, &[FMT_BIT, op(3), prefix], toffoli_prim()));
    gates.push(at_wire(n, op(3), not_iso()));

    // ── R-format family, op3 branch: prefix g_c = fmt ∧ op3 → prefix ────
    // Serves Ccnot (R 0b1011: op3=1, low3=011). The op3=1 prefix isolates
    // it from every ¬op3 op (Group A/B + Cswap), and low3=011 is landed on
    // CFG_CCNOT. No other architected op has op3=1 with fmt=1, so ONLY
    // Ccnot's low-3 pattern can fire this line.
    gates.push(gate_at(n, &[FMT_BIT, op(3), prefix], toffoli_prim()));
    gates.extend(land_op(CFG_CCNOT, low3(0b1011)));
    gates.push(gate_at(n, &[FMT_BIT, op(3), prefix], toffoli_prim()));

    if include_i {
        // ── I-format family: prefix g_i = ¬fmt → prefix ───────────────
        // i_pred tests fmt=0 + the three low opcode bits ONLY; op3 is a
        // don't-care, so the prefix is a single negated-fmt CNOT.
        gates.push(at_wire(n, FMT_BIT, not_iso()));
        gates.push(gate_at(n, &[FMT_BIT, prefix], cnot_prim()));
        gates.push(at_wire(n, FMT_BIT, not_iso()));
        for (final_bit, lits) in [
            (CFG_ADD_FWD, [false, false, false]), // Addi  I 0b_000
            (CFG_ADD_REV, [false, false, true]),  // Subi  I 0b_001
            (CFG_XOR, [false, true, false]),      // Xori  I 0b_010
            (CFG_ROT_FWD, [false, true, true]),   // Rotli I 0b_011
            (CFG_ROT_REV, [true, false, false]),  // Rotri I 0b_100
        ] {
            gates.extend(land_op(final_bit, lits));
        }
        // Uncompute g_i (leave prefix = 0; the caller's decode⁻¹ is a
        // full mirror so all config bits — held + workspace — return to 0).
        gates.push(at_wire(n, FMT_BIT, not_iso()));
        gates.push(gate_at(n, &[FMT_BIT, prefix], cnot_prim()));
        gates.push(at_wire(n, FMT_BIT, not_iso()));
    }

    on_fields(&config_decode_fields(), gates)
}

/// The UNIFIED reg+imm configurable ALU core (B1.2), run on the shared
/// slots [r0-slot, s0, s1] with rd already routed to r0-slot. Structure:
///   imm_to_a ; decode_config ; ALU(sub-blocks gated) ; decode⁻¹ ; imm_to_a⁻¹
///
/// `imm_to_a` sits inside the sel_rd bracket (so the rd byte is at the
/// r0-slot) and, for I-format, deposits the IR immediate into s0 (the
/// A-slot). For R-format it is the identity (s0 = the fetched rs value).
/// After that, the A-slot holds the 2nd operand regardless of format, and
/// the SAME six sub-blocks serve both families:
///
/// Every sub-block is instantiated and gated on its own config line
/// (single fixed iso, §2.3). At most one config line is set (mutual
/// exclusion, `alu_config_decode` across BOTH fmt families), so at most
/// one sub-block is non-identity — a value-level Branch. Sub/Subi = the
/// adder's adjoint gated on `CFG_ADD_REV`; Rotr/Rotri = the rotator's
/// adjoint gated on `CFG_ROT_REV` (`Iso::sym` of the config-wired forward
/// block, so the whole reverse block still carries the config line as its
/// per-gate control).
fn reg_imm_alu() -> Iso {
    let imm = imm_to_a();
    let decode = alu_config_decode();
    let core = seq_balanced(vec![
        add_core_at(CFG_ADD_FWD),            // Add / Addi
        Iso::sym(add_core_at(CFG_ADD_REV)),  // Sub / Subi  = adder adjoint
        xor_core_at(CFG_XOR),                // Xor / Xori
        exch_core_at(CFG_EXCH),              // Exch (R-only)
        rotl_core_at(CFG_ROT_FWD),           // Rotl / Rotli
        Iso::sym(rotl_core_at(CFG_ROT_REV)), // Rotr / Rotri = rotator adjoint
    ]);
    let alu = Iso::seq(decode.clone(), Iso::seq(core, Iso::sym(decode)));
    Iso::seq(imm.clone(), Iso::seq(alu, Iso::sym(imm)))
}

// ── B1.3: config-wired controlled cores on the 4-slot [D, A, B, s1] ─────
//
// Slot layout: D = r0-slot(rd) = wires 0..W, A = s0(rs) = W..2W,
// B = BR(rt) = 2W..3W, s1 = 3W..4W (config one-hot k at wire 3W+k).

/// Ccnot core: `D[i] ^= cfg_ccnot ∧ A[i] ∧ B[i]` per bit (byte-parallel
/// `rd ^= rs & rt`, config-gated). Verbatim `ccnot_core`'s CCC-NOT pattern
/// but the 4th control is the CFG_CCNOT one-hot instead of the raw pred bit.
fn ccnot_core_at(cfg: usize) -> Iso {
    let gates = (0..W)
        .map(|i| gate_at(4 * W, &[3 * W + cfg, W + i, 2 * W + i, i], cccnot_prim()))
        .collect();
    on_fields(
        &[reg_field(0), AUX_OPERAND_FIELD, BR_FIELD, AUX_PRED_FIELD],
        gates,
    )
}

/// Cswap core: swap `A[i] ↔ B[i]` (= s0 ↔ BR) gated on `cfg_cswap ∧ D[0]`
/// per bit. The control reads D[0] (rd[0]) directly from the r0-slot — no
/// s1-bit-4 copy (cleaner than the pre-B1.3 `cswap_instr`, which routed rt
/// and copied rd[0] into s1). 3 controls + 2 swap = K3 (in vocabulary).
///
/// Reproduces `r[rs] ↔ r[rt] iff r[rd][0]`: with rd→D, rs→A(s0), rt→B(BR),
/// swapping A↔B under D[0] and unwinding the fetch brackets writes the
/// swapped values back into r[rs]/r[rt]; D (rd) itself is never modified.
fn cswap_core_at(cfg: usize) -> Iso {
    // Wires: D[0] = 0, A[i] = W+i, B[i] = 2W+i, cfg one-hot = 3W+cfg.
    // Gated Fredkin control pair (cfg_cswap, D[0]) swaps A[i], B[i].
    let gates = (0..W)
        .map(|i| gate_at(4 * W, &[3 * W + cfg, 0, W + i, 2 * W + i], gated_fredkin_prim()))
        .collect();
    on_fields(
        &[reg_field(0), AUX_OPERAND_FIELD, BR_FIELD, AUX_PRED_FIELD],
        gates,
    )
}

/// B1.1 Group-A-only ALU view (R-format decode + six R cores, NO imm
/// A-source) — reproduces the exact pre-B1.2 `group_a_alu` so the B1.1
/// Group-A-portion measurement remains comparable. Not wired into the
/// decoder (the wired path is `reg_imm_alu`).
fn group_a_alu_view() -> Iso {
    let decode = alu_config_decode_families(false);
    let core = seq_balanced(vec![
        add_core_at(CFG_ADD_FWD),
        Iso::sym(add_core_at(CFG_ADD_REV)),
        xor_core_at(CFG_XOR),
        exch_core_at(CFG_EXCH),
        rotl_core_at(CFG_ROT_FWD),
        Iso::sym(rotl_core_at(CFG_ROT_REV)),
    ]);
    Iso::seq(decode.clone(), Iso::seq(core, Iso::sym(decode)))
}

/// `decode_execute_shared : CpuState <-> CpuState` — the SHARED-crossbar
/// form of the full ring decoder. Semantically identical to
/// `decode_execute_ops(&Op::ALL)` (every op is the identity off its
/// opcode; opcodes mutually exclusive) but collapses the replicated
/// crossbar instances down.
///
/// B1.3: the UNIVERSAL 3-operand `shared_alu_block` now serves reg+imm
/// (11 ops) PLUS Ccnot and Cswap through ONE bracket (rd→D, rs→A, rt→B),
/// so their two private crossbars are gone and the top-level block count
/// drops 5→3. Only Xchg (2nd scratch crossbar) and Swapbr (rs=0 aliases
/// rd) keep their own separate small blocks — their brackets differ.
fn decode_execute_shared() -> Iso {
    seq_balanced(vec![
        shared_alu_block(), // B1.3: reg+imm + Ccnot + Cswap (universal bracket)
        xchg_instr(),
        swapbr_instr(),
    ])
}

/// `decode_execute : CpuState <-> CpuState` — the ring decoder
/// (decode_execute′, Stage 1a-B).
///
/// All 15 non-MEXCH/MEXCHR instructions. MEXCH and MEXCHR are handled
/// by the emit/mem/absorb cycle in [`system_decode_execute`]. Reserved
/// opcodes (including MEXCH 0b1001 and MEXCHR 0b1010) are the identity.
///
/// A-0 shared crossbar: uses [`decode_execute_shared`] (operands routed
/// once per step) instead of the per-op replicated form.
pub fn decode_execute() -> Iso {
    decode_execute_shared()
}

/// `system_decode_execute : (CpuState, MemState) <-> (CpuState, MemState)`
/// — the composite decoder, non-MEXCH/MEXCHR instructions only (Stage 1b-A).
///
/// The decoder acts only on the CpuState factor; MemState is identity.
/// A-0 shared crossbar: tensor the SHARED CpuState decoder with id_Mem
/// ONCE (not per-op) — this is what the ring/energy path lowers, so the
/// sharing is what keeps `me_a3` off the OOM cliff.
/// MEXCH and MEXCHR are handled by the emit/mem/absorb 3-cycle in
/// `system_step` (Stage 1b-A).
pub fn system_decode_execute() -> Iso {
    let mem_id = Iso::Id(crate::types::mem_state_ty());
    Iso::times(decode_execute_shared(), mem_id)
}

// ══════════════════════════════════════════════════════════════════════
// 3-factor reshape views (Stage 1b-A, Decision 2)
// ══════════════════════════════════════════════════════════════════════
//
// The 3-factor carrier is `((CpuState, MemState), BusMsg)`.
// Two views expose the 2-factor sub-products needed by mem and emit/absorb:
//
//   view_mem_bus:  ((Cpu,Mem),Bus) <-> (Cpu, (Mem,Bus))
//   view_cpu_bus:  ((Cpu,Mem),Bus) <-> ((Cpu,Bus), Mem)

/// `((Cpu,Mem),Bus) ↔ (Cpu, (Mem,Bus))` — expose the (Mem,Bus) sub-product
/// for the mem phase, with Cpu as identity.
pub fn view_mem_bus() -> Iso {
    let cpu_ty = crate::types::state_ty();
    let mem_ty = crate::types::mem_state_ty();
    let bus_ty = crate::types::bus_state_ty();
    Iso::AssocRProd(cpu_ty, mem_ty, bus_ty)
}

/// `((Cpu,Mem),Bus) ↔ ((Cpu,Bus), Mem)` — expose the (Cpu,Bus) sub-product
/// for emit/absorb, with Mem as identity.
///
/// Constructed as: `AssocR(Cpu,Mem,Bus) ; (Id(Cpu) × Swap(Mem,Bus)) ; AssocL(Cpu,Bus,Mem)`.
pub fn view_cpu_bus() -> Iso {
    let cpu_ty = crate::types::state_ty();
    let mem_ty = crate::types::mem_state_ty();
    let bus_ty = crate::types::bus_state_ty();
    Iso::seq(
        Iso::AssocRProd(cpu_ty.clone(), mem_ty.clone(), bus_ty.clone()),
        Iso::seq(
            Iso::times(
                Iso::Id(cpu_ty.clone()),
                Iso::SwapProd(mem_ty.clone(), bus_ty.clone()),
            ),
            Iso::AssocLProd(cpu_ty, bus_ty, mem_ty),
        ),
    )
}

// ══════════════════════════════════════════════════════════════════════
// Cross-factor CNOT (Stage 1b-A helper)
// ══════════════════════════════════════════════════════════════════════

/// CNOT across a 2-factor product boundary: `target_bit ^= source_bit`.
///
/// Carrier: `(fields(n_left), fields(n_right))`.
/// Source is bit `src_bit` of field `src_field` in the left factor;
/// target is bit `tgt_bit` of field `tgt_field` in the right factor.
fn cross_cnot(
    src_field: usize,
    src_bit: usize,
    tgt_field: usize,
    tgt_bit: usize,
    n_left: usize,
    n_right: usize,
) -> Iso {
    use crate::addressing::split_fields;
    assert!(src_field < n_left, "src_field out of range in left factor");
    assert!(src_bit < W, "src_bit out of range");
    assert!(
        tgt_field < n_right,
        "tgt_field out of range in right factor"
    );
    assert!(tgt_bit < W, "tgt_bit out of range");

    let n_total = n_left + n_right;
    let src_flat = src_field;
    let tgt_flat = n_left + tgt_field;

    let n2 = 2 * W;
    let gate = gate_at(n2, &[src_bit, W + tgt_bit], cnot_prim());
    let core = on_fields_n(&[src_flat, tgt_flat], vec![gate], n_total);

    let split = split_fields(n_left, n_right);
    Iso::seq(Iso::sym(split.clone()), Iso::seq(core, split))
}

/// Toffoli across a 2-factor product boundary: `tgt_bit ^= ctrl1_bit AND ctrl2_bit`.
///
/// Carrier: `(fields(n_left), fields(n_right))`.
/// Both controls are in the left factor (fields `ctrl1_field` and `ctrl2_field`,
/// at bit positions `ctrl1_bit` and `ctrl2_bit` respectively);
/// target is bit `tgt_bit` of field `tgt_field` in the right factor.
///
/// Used for AUX_PRED-gated address copies in the MEXCHR emit block:
/// `BUS_ADDR[i] ^= AUX_PRED[0] AND reg[0][i]`.
#[allow(clippy::too_many_arguments)]
fn cross_toffoli(
    ctrl1_field: usize,
    ctrl1_bit: usize,
    ctrl2_field: usize,
    ctrl2_bit: usize,
    tgt_field: usize,
    tgt_bit: usize,
    n_left: usize,
    n_right: usize,
) -> Iso {
    use crate::addressing::split_fields;
    assert!(
        ctrl1_field < n_left,
        "ctrl1_field out of range in left factor"
    );
    assert!(
        ctrl2_field < n_left,
        "ctrl2_field out of range in left factor"
    );
    assert!(ctrl1_bit < W, "ctrl1_bit out of range");
    assert!(ctrl2_bit < W, "ctrl2_bit out of range");
    assert!(
        tgt_field < n_right,
        "tgt_field out of range in right factor"
    );
    assert!(tgt_bit < W, "tgt_bit out of range");

    let n_total = n_left + n_right;
    let ctrl1_flat = ctrl1_field;
    let ctrl2_flat = ctrl2_field;
    let tgt_flat = n_left + tgt_field;

    if ctrl1_field == ctrl2_field {
        // Both controls in the same field — 2-field core.
        let n2 = 2 * W;
        let gate = gate_at(n2, &[ctrl1_bit, ctrl2_bit, W + tgt_bit], toffoli_prim());
        let core = on_fields_n(&[ctrl1_flat, tgt_flat], vec![gate], n_total);
        let split = split_fields(n_left, n_right);
        Iso::seq(Iso::sym(split.clone()), Iso::seq(core, split))
    } else {
        // Controls in different fields — 3-field core.
        let n3 = 3 * W;
        let gate = gate_at(
            n3,
            &[ctrl1_bit, W + ctrl2_bit, 2 * W + tgt_bit],
            toffoli_prim(),
        );
        let core = on_fields_n(&[ctrl1_flat, ctrl2_flat, tgt_flat], vec![gate], n_total);
        let split = split_fields(n_left, n_right);
        Iso::seq(Iso::sym(split.clone()), Iso::seq(core, split))
    }
}

// ══════════════════════════════════════════════════════════════════════
// select_mem_on_bus (Stage 1b-A, Decision 4)
// ══════════════════════════════════════════════════════════════════════

/// The 1-of-N select tree over memory words, keyed on `BusMsg.addr`.
///
/// Carrier: `(fields(n_bus), fields(n_mem))` = `(Bus, Mem)`.
/// Routes `Mem[addr] → Mem head`, where `addr` bits come from the
/// `BUS_ADDR_FIELD` byte (bits 0..ceil(log2 N)) in the Bus (left) factor.
/// Each level uses [`cross_ctrl_fredkin`]: control in left, swap targets in right.
///
/// This is the C1 tree re-keyed from IR onto BusMsg.addr (Stage 1b-A,
/// Decision 4). Gate-level cleanliness transfers because the Fredkin
/// control is pass-through regardless of source.
pub fn select_mem_on_bus_n(n_bus: usize, mem: MemWidth) -> Iso {
    select_mem_on_bus_n_inner(n_bus, mem.words())
}

/// Inner helper: operates on raw word count. The recursion structure and
/// gate construction are VERBATIM from the original `select_mem_on_bus_n`.
fn select_mem_on_bus_n_inner(n_bus: usize, n_mem: usize) -> Iso {
    let n_bits = n_mem.trailing_zeros() as usize; // addr_bits for powers of two
    let mut steps = Vec::new();
    for level in (0..n_bits).rev() {
        let stride = 1usize << level;
        for i in 0..stride {
            if i + stride < n_mem {
                steps.push(cross_ctrl_fredkin(
                    BUS_ADDR_FIELD,
                    level,
                    i,
                    i + stride,
                    n_bus,
                    n_mem,
                ));
            }
        }
    }
    if steps.is_empty() {
        use crate::addressing::fields;
        Iso::Id(Type::prod(fields(n_bus), fields(n_mem)))
    } else {
        seq_balanced(steps)
    }
}

// ══════════════════════════════════════════════════════════════════════
// emit / mem / absorb (Stage 1b-A, Decision 3)
// ══════════════════════════════════════════════════════════════════════

/// `emit : system_state_ty <-> system_state_ty` — the first phase of the
/// MEXCH/MEXCHR 3-cycle (Stage 1b-A; M-D.b).
///
/// Two sequenced predicated blocks (MEXCH immediate + MEXCHR register-
/// addressed), lifted to system_state_ty via `view_cpu_bus`.
/// `absorb = sym(emit)` reverses both blocks.
pub fn emit() -> Iso {
    let mem_ty = crate::types::mem_state_ty();
    let view = view_cpu_bus();

    // Build emit_sub on (Cpu, Bus) = (fields(NUM_CPU_FIELDS), fields(NUM_BUS_FIELDS))
    let emit_sub = emit_on_cpu_bus();

    // Lift to system: view ; (emit_sub × Id(Mem)) ; sym(view)
    let lifted = Iso::times(emit_sub, Iso::Id(mem_ty));
    Iso::seq(view.clone(), Iso::seq(lifted, Iso::sym(view)))
}

/// The emit sub-iso on `(CpuState, BusMsg)` = `(fields(n_cpu), fields(n_bus))`.
///
/// Two sequenced, mutually-exclusive predicated blocks (opcodes 0b1001, 0b1010):
///
/// **MEXCH block** (immediate addressing, opcode 0b1001):
///   `pred ; selr ; [enable^=p ; addr^=IR.rt ; gated_swap(reg0↔bus.word)] ; sym(selr) ; sym(pred)`
///
/// **MEXCHR block** (register addressing, opcode 0b1010, M-D.b):
///   `pred ; [ sela ; <AUX_PRED-gated addr-copies reg[0]→BUS_ADDR> ; sym(sela) ] ;`
///   `       [ selr ; enable^=p ; gated_swap(reg0↔bus.word) ; sym(selr) ] ; sym(pred)`
///   The ra-bracket (sela) and rd-bracket (selr) are SEQUENTIAL, NEVER NESTED
///   (nesting reads the wrong register AND passes the bus-quiescence certificate).
///   MEXCHR encodes rt=0 so the MEXCH block's unpredicated IR.rt addr-copies
///   contribute 0 — harmless.
///
/// Public for Stage 2a: `cpu_pre`/`cpu_post` compose this directly on the
/// `(Cpu, Bus)` carrier without the `view_cpu_bus` lift.
pub fn emit_on_cpu_bus() -> Iso {
    let bus_ty = crate::types::bus_state_ty();
    let bus_id = Iso::Id(bus_ty);
    let n_cpu = NUM_CPU_FIELDS;
    let n_bus = NUM_BUS_FIELDS;
    let n_addr = MEM_WIDTH.addr_bits();

    // ── MEXCH block (opcode 0b1001, immediate addressing) ─────────
    let mexch_block = {
        let pred_cpu = pred_compute(&r_pred(0b1001));
        let pred = Iso::times(pred_cpu, bus_id.clone());

        // A-0 (BLOCKER-2 fix): route rd over reg∪scratch (bit3=0 for a
        // register operand ⇒ inert top level, behaviour unchanged).
        let selr = Iso::times(select_operand(R_RD, operand_field), bus_id.clone());

        // 1. enable ^= predicate bit
        let enable_cnot = cross_cnot(AUX_PRED_FIELD, 0, BUS_ENABLE_FIELD, 0, n_cpu, n_bus);

        // 2. addr ^= IR.rt bits (unpredicated — contributes 0 for MEXCHR since rt=0)
        let mut addr_cnots: Vec<Iso> = Vec::new();
        for i in 0..n_addr {
            let (rt_field, rt_b) = R_RT.bits[i];
            addr_cnots.push(cross_cnot(rt_field, rt_b, BUS_ADDR_FIELD, i, n_cpu, n_bus));
        }

        // 3. Predicate-gated swap: reg[0] ↔ BusMsg.word
        let gsw = cross_factor_gated_swap(
            AUX_PRED_FIELD,
            0,
            reg_field(0),
            BUS_WORD_FIELD,
            n_cpu,
            n_bus,
        );

        let mut inner_steps = vec![enable_cnot];
        inner_steps.extend(addr_cnots);
        inner_steps.push(gsw);
        let inner = seq_balanced(inner_steps);

        // pred ; selr ; inner ; sym(selr) ; sym(pred)
        Iso::seq(
            pred.clone(),
            Iso::seq(
                selr.clone(),
                Iso::seq(inner, Iso::seq(Iso::sym(selr), Iso::sym(pred))),
            ),
        )
    };

    // ── MEXCHR block (opcode 0b1010, register addressing, M-D.b) ──
    let mexchr_block = {
        let pred_cpu = pred_compute(&r_pred(0b1010));
        let pred = Iso::times(pred_cpu, bus_id.clone());

        // Ra bracket: route reg[ra] to r0 slot via R_RS crossbar,
        // copy addr bits (AUX_PRED-gated), then un-route.
        let sela = Iso::times(select_operand(R_RS, operand_field), bus_id.clone());
        let mut gated_addr_copies: Vec<Iso> = Vec::new();
        for i in 0..n_addr {
            gated_addr_copies.push(cross_toffoli(
                AUX_PRED_FIELD,
                0, // predicate control
                reg_field(0),
                i, // source: reg[0] bit i (= reg[ra] after sela)
                BUS_ADDR_FIELD,
                i, // target: BUS_ADDR bit i
                n_cpu,
                n_bus,
            ));
        }
        let ra_bracket = Iso::seq(
            sela.clone(),
            Iso::seq(seq_balanced(gated_addr_copies), Iso::sym(sela)),
        );

        // Rd bracket: route reg[rd] to r0 slot via R_RD crossbar,
        // enable + gated-swap, then un-route.
        let selr = Iso::times(select_operand(R_RD, operand_field), bus_id.clone());
        let enable_cnot = cross_cnot(AUX_PRED_FIELD, 0, BUS_ENABLE_FIELD, 0, n_cpu, n_bus);
        let gsw = cross_factor_gated_swap(
            AUX_PRED_FIELD,
            0,
            reg_field(0),
            BUS_WORD_FIELD,
            n_cpu,
            n_bus,
        );
        let rd_bracket = Iso::seq(
            selr.clone(),
            Iso::seq(enable_cnot, Iso::seq(gsw, Iso::sym(selr))),
        );

        // pred ; ra_bracket ; rd_bracket ; sym(pred)
        Iso::seq(
            pred.clone(),
            Iso::seq(ra_bracket, Iso::seq(rd_bracket, Iso::sym(pred))),
        )
    };

    // Sequence the two mutually-exclusive blocks.
    Iso::seq(mexch_block, mexchr_block)
}

/// `absorb : system_state_ty <-> system_state_ty` — the third phase of the
/// MEXCH 3-cycle (Stage 1b-A). Defined as `Iso::sym(emit())`.
///
/// Because emit's swap is predicate-gated and selr is internal, the
/// adjoint correctly re-selects reg[rd], un-swaps, and zeros the bus fields.
pub fn absorb() -> Iso {
    Iso::sym(emit())
}

/// `mem_body : (MemState, BusMsg) <-> (MemState, BusMsg)` — the standalone
/// memory exchange sub-iso (Stage 2a).
///
/// Routes `MemState[addr]→head` keyed on `BusMsg.addr`, gated-swaps
/// `BusMsg.word ↔ MemState[0]` on `BusMsg.enable`, then un-routes.
/// Extracted from `mem_phase` so it can be lowered as a separate body
/// in the two-body ring; `mem_phase` is re-expressed as
/// `view_mem_bus ; (Id(Cpu) × mem_body()) ; sym(view_mem_bus)`.
pub fn mem_body() -> Iso {
    let mem_ty = crate::types::mem_state_ty();
    let bus_ty = crate::types::bus_state_ty();

    // swap inside (Mem,Bus) to get (Bus,Mem) for 2-factor helpers
    let swap_mb = Iso::SwapProd(mem_ty, bus_ty);

    // select_mem_on_bus on (Bus, Mem)
    let select = select_mem_on_bus_n(NUM_BUS_FIELDS, MEM_WIDTH);

    // gated_swap on (Bus, Mem): Bus.word ↔ Mem[0] gated on Bus.enable
    let gsw = cross_factor_gated_swap(
        BUS_ENABLE_FIELD,
        0,
        BUS_WORD_FIELD,
        0, // MemState head = field 0
        NUM_BUS_FIELDS,
        MEM_WORDS,
    );

    // mem_sub on (Bus, Mem): select ; gated_swap ; sym(select)
    let mem_sub_bm = Iso::seq(select.clone(), Iso::seq(gsw, Iso::sym(select)));

    // mem_sub on (Mem, Bus): swap ; mem_sub_bm ; swap^{-1}
    Iso::seq(swap_mb.clone(), Iso::seq(mem_sub_bm, Iso::sym(swap_mb)))
}

/// `mem_phase : system_state_ty <-> system_state_ty` — the second phase
/// of the MEXCH 3-cycle (Stage 1b-A).
///
/// Built over `view_mem_bus`: routes `MemState[addr]→head` keyed on
/// `BusMsg.addr`, then gated-swaps `BusMsg.word ↔ MemState[0]` on
/// `BusMsg.enable`, then un-routes. Controls = BusMsg, targets = Mem+Bus
/// → zero CpuState fields (the line-audit witness).
///
/// Re-expressed as `view_mem_bus ; (Id(Cpu) × mem_body()) ; sym(view_mem_bus)`
/// (Stage 2a refactor — bit-identical to the original).
pub fn mem_phase() -> Iso {
    let cpu_ty = crate::types::state_ty();
    let view = view_mem_bus();
    let lifted = Iso::times(Iso::Id(cpu_ty), mem_body());
    Iso::seq(view.clone(), Iso::seq(lifted, Iso::sym(view)))
}
