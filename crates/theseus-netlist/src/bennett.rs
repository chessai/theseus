//! Bennett clean-up wrap for lowered netlists (ADR-0033 ring closure).
//!
//! # Why lowering leaves dirty ancillas — and what is fixable
//!
//! Every cell in the vocabulary (K0 buffer, K1/K2/K3 conditional
//! swaps) **permutes the values on its rail pairs** — a controlled
//! swap only routes tokens. Free NOTs (rail-role swaps) are *fixed*
//! per edge. Consequently, for any lowered netlist, the parity of all
//! output pair-values equals the parity of all input pair-values plus
//! a circuit-constant (the number of rail-swapped edges, mod 2):
//!
//! ```text
//! parity(data_out) ⊕ parity(anc_out) = parity(data_in) ⊕ parity(anc_in) ⊕ c
//! ```
//!
//! So a lowering of `f` can restore **all** ancillas to their inits
//! only if `parity(bits(f(x))) ⊕ parity(bits(x))` is constant over the
//! domain. A single conditional-NOT (`t ↦ t ⊕ AND(controls)`) already
//! violates this — its parity deviation *is* the AND — which is why
//! [`crate::lower`]'s splitter/selector construction must leave two
//! data-dependent garbage pairs per multi-controlled NOT, and why no
//! per-gate fix can do better (per-instance, two dirty pairs is the
//! floor; see `tests/ancilla_clean.rs`).
//!
//! # What this module does about it
//!
//! The garbage of a *composition* can still cancel: [`bennett_wrap`]
//! applies the classic compute–copy–uncompute discipline **to the
//! whole netlist**:
//!
//! 1. **compute** — the forward net runs unchanged; its garbage
//!    outputs are intercepted instead of exported;
//! 2. **copy** — each data output `y_j` is fanned out with one K1
//!    (`K1(y, 1, 0) → (y, ¬y, y)` — Fredkin fan-out, consuming a
//!    fresh `(1, 0)` ancilla pair per bit);
//! 3. **uncompute** — the forward cells are replayed in reverse (every
//!    K-cell is an involution), consuming the forward garbage and the
//!    fanned-through result register. This *provably* returns every
//!    forward ancilla to its init value and regenerates the input
//!    register.
//!
//! The wrapped net computes the same function (data outputs are the
//! fan-out copies), and its ancilla outputs are, positionally:
//!
//! - one output per forward ancilla input, **always equal to its
//!   init** (data-independently — the mirror restores it exactly);
//! - per data bit `j`, the fan-out slot pair: a spare copy (equal to
//!   its init `1` iff `y_j = 1`) and the regenerated input bit (equal
//!   to its init `0` iff `x_j = 0`).
//!
//! I.e. the *unbounded* per-construct garbage (2 pairs per synthesized
//! multi-controlled NOT, per Branch conditional-NOT, per TraceLoop tag
//! duplication, …) collapses to exactly `2·w` boundary pairs whose
//! residue is the input word and the output word — the honest Bennett
//! residue. By the parity law above, a nonzero residue is unavoidable
//! for any `f` that does not preserve encoded-bit parity up to a fixed
//! mask; the wrap concentrates it at the boundary instead of smearing
//! it across every construct.
//!
//! Cost: cell count ≈ 2× forward + one K1 per data bit; depth ≈ 2×.
//! The wrap preserves the differential oracle (data outputs unchanged)
//! and structural validity; tick-balance is re-established downstream
//! by `balance`/`balance_compact` as usual.
//!
//! # Measured on the TISC-1 step (M4-v2, 3-instruction ROM)
//!
//! Plain `lower_iso_with_ancillas(step)`: 4654 cells, 160 data pairs,
//! 1138 ancillas — **945 dirty** at the ring test's pc = 0 start state
//! (809/832 at pc = 1/2), all attributed to prim synthesis
//! (`prim_synth` 903/1082 + `prim_synth_chain` 42/56 at pc = 0; the
//! step lowers to CNOT/Toffoli mcnots, each stuck at its 2-pair
//! per-instance floor). After `bennett_wrap`: 9468 cells, 1458
//! ancillas — **forward dirty 0/1138** at every sampled state; the
//! whole residue is the 320 boundary slots (166/156/161 dirty at
//! pc = 0/1/2 — the encoded input word + output-word complement).
//! True recirculation (`ancillas_restored == true`) remains
//! **impossible** for `step` in this cell vocabulary: its encoded-bit
//! parity deviation is non-constant, so by the parity law some
//! data-dependent residue must exit every revolution. A physical ring
//! must either re-supply the 2·w boundary pairs per revolution
//! (constant-size, data-independent *ports*, unlike the plain
//! lowering's 1138 scattered lines) or extend the cell vocabulary
//! with a non-value-conserving cell (a controlled rail-role swap —
//! physically a CSWAP whose two swapped conductors are the two rails
//! of one pair, which would make CNOT garbage-free and void the
//! parity law).

use core::fmt;
use std::collections::HashMap;

use crate::ir::cell::{
    Cell, CellKind, CellNetError, LineId, PhasedCellNetlist, RailPair, PHASE_COUNT,
};
use crate::lower::{rail_width, LowerError};

/// Why a netlist cannot be Bennett-wrapped.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BennettError {
    /// The body is structurally invalid.
    Body(CellNetError),
    /// A boundary type has no dual-rail encoding (cannot size the data
    /// sections).
    Encoding(LowerError),
    /// Data input/output widths differ: the wrap needs the regenerated
    /// input register and the spare output copies to fill the fan-out
    /// ancilla slots pairwise (`w_in = w_out`, the iso case).
    Shape { w_in: usize, w_out: usize },
    /// `ancilla_init` length disagrees with the netlist's input ports.
    AncillaCount { want: usize, got: usize },
}

impl fmt::Display for BennettError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BennettError::Body(e) => write!(f, "bennett: invalid body: {e}"),
            BennettError::Encoding(e) => write!(f, "bennett: unencodable boundary: {e}"),
            BennettError::Shape { w_in, w_out } => {
                write!(f, "bennett: data widths differ ({w_in} in vs {w_out} out)")
            }
            BennettError::AncillaCount { want, got } => {
                write!(f, "bennett: ancilla_init has {got} entries, netlist wants {want}")
            }
        }
    }
}

/// Emission state for the wrap: continues the forward net's line
/// numbering and arrival bookkeeping.
struct Wrap {
    next_line: u32,
    /// Arrival tick per line id (dense, extended as lines are created).
    arr: Vec<u32>,
    cells: Vec<Cell>,
}

impl Wrap {
    fn fresh_input_pair(&mut self) -> RailPair {
        let pair = RailPair { line0: LineId(self.next_line), line1: LineId(self.next_line + 1) };
        self.next_line += 2;
        self.arr.push(0);
        self.arr.push(0);
        pair
    }

    fn arrival(&self, p: RailPair) -> u32 {
        self.arr[p.line0.0 as usize].max(self.arr[p.line1.0 as usize])
    }

    fn emit(&mut self, kind: CellKind, ins: &[RailPair]) -> Vec<RailPair> {
        debug_assert_eq!(ins.len(), kind.arity());
        let depth = ins.iter().map(|&p| self.arrival(p)).max().expect("cell has inputs");
        let mut outs = Vec::with_capacity(ins.len());
        for _ in ins {
            let pair =
                RailPair { line0: LineId(self.next_line), line1: LineId(self.next_line + 1) };
            self.next_line += 2;
            self.arr.push(depth + 1);
            self.arr.push(depth + 1);
            outs.push(pair);
        }
        self.cells.push(Cell {
            kind,
            depth,
            phase: (depth % u32::from(PHASE_COUNT)) as u8,
            inputs: ins.to_vec(),
            outputs: outs.clone(),
        });
        outs
    }
}

/// Mirror-image lookup: pairs may be referenced in either rail
/// orientation (a free NOT); the mirror image inherits the flip.
fn mirror_of(m: &HashMap<RailPair, RailPair>, p: RailPair) -> RailPair {
    if let Some(&q) = m.get(&p) {
        q
    } else if let Some(&q) = m.get(&p.swapped()) {
        q.swapped()
    } else {
        panic!("bennett: no mirror image for pair {p:?} (body not single-consumer?)")
    }
}

/// Wrap a lowered netlist in the Bennett compute–copy–uncompute
/// discipline. Returns the wrapped netlist and its extended ancilla
/// init vector (forward inits ++ `[true, false]` per data bit).
///
/// Guarantees, for every input (`tests/ancilla_clean.rs` verifies them
/// over whole domains):
///
/// - **oracle preserved**: data outputs equal the forward net's;
/// - **forward ancillas clean**: ancilla output `i` (for `i` below the
///   forward ancilla count) equals `ancilla_init[i]`;
/// - the only possibly-dirty ancilla outputs are the `2·w` fan-out
///   slots, carrying exactly `(y_j, x_j)` against inits `(1, 0)`.
pub fn bennett_wrap(
    net: &PhasedCellNetlist,
    ancilla_init: &[bool],
) -> Result<(PhasedCellNetlist, Vec<bool>), BennettError> {
    net.validate().map_err(BennettError::Body)?;
    let w_in = rail_width(&net.input_ty).map_err(BennettError::Encoding)?;
    let w_out = rail_width(&net.output_ty).map_err(BennettError::Encoding)?;
    if w_in != w_out {
        return Err(BennettError::Shape { w_in, w_out });
    }
    let w = w_in;
    let n_f = ancilla_init.len();
    if net.inputs.len() != w + n_f {
        return Err(BennettError::AncillaCount {
            want: net.inputs.len().saturating_sub(w),
            got: n_f,
        });
    }

    let arrivals = net.line_arrivals().map_err(BennettError::Body)?;
    let mut wrap = Wrap { next_line: net.num_lines, arr: arrivals, cells: net.cells.clone() };

    // Mirror map: forward pair (as referenced) → the pair carrying the
    // same value on the uncompute side.
    let mut m: HashMap<RailPair, RailPair> = HashMap::new();

    // ── copy: fan out each data output with one K1 ───────────────────
    let mut new_inputs = net.inputs.clone();
    let mut init2 = ancilla_init.to_vec();
    let mut data_copies = Vec::with_capacity(w); // value y_j → data outputs
    let mut spares = Vec::with_capacity(w); // value ¬y_j
    for j in 0..w {
        let p = net.outputs[j];
        let one = wrap.fresh_input_pair();
        let zero = wrap.fresh_input_pair();
        new_inputs.push(one);
        new_inputs.push(zero);
        init2.push(true);
        init2.push(false);
        // K1(y, 1, 0) → (y, ¬y, y): pass-through feeds the mirror,
        // third port is the exported copy, second the spare.
        let outs = wrap.emit(CellKind::K1, &[p, one, zero]);
        m.insert(p, outs[0]);
        spares.push(outs[1]);
        data_copies.push(outs[2]);
    }

    // ── splice: forward garbage outputs feed the mirror directly ─────
    for &g in &net.outputs[w..] {
        m.insert(g, g);
    }

    // ── uncompute: replay every forward cell in reverse ──────────────
    // Every K-cell is an involution, so a cell fed the mirror images of
    // its outputs produces the mirror images of its inputs.
    for cell in net.cells.iter().rev() {
        let ins_m: Vec<RailPair> = cell.outputs.iter().map(|&o| mirror_of(&m, o)).collect();
        let outs_m = wrap.emit(cell.kind, &ins_m);
        for (&ip, &om) in cell.inputs.iter().zip(&outs_m) {
            m.insert(ip, om);
        }
    }

    // ── boundary: assemble outputs positionally against `init2` ──────
    // Data section: the fan-out copies (value y_j).
    let mut outputs = data_copies;
    // Forward ancilla slots: mirror images of the ancilla inputs —
    // these carry exactly their init values, data-independently.
    for i in 0..n_f {
        outputs.push(mirror_of(&m, net.inputs[w + i]));
    }
    // Fan-out slots, per data bit j: the spare copy rail-swapped to
    // carry y_j (against init 1: clean iff y_j = 1), then the
    // regenerated input bit x_j (against init 0: clean iff x_j = 0) —
    // the concentrated Bennett residue.
    for (j, &spare) in spares.iter().enumerate() {
        outputs.push(spare.swapped());
        outputs.push(mirror_of(&m, net.inputs[j]));
    }

    let output_depths: Vec<u32> = outputs.iter().map(|&p| wrap.arrival(p)).collect();

    let wrapped = PhasedCellNetlist {
        input_ty: net.input_ty.clone(),
        output_ty: net.output_ty.clone(),
        inputs: new_inputs,
        outputs,
        output_depths,
        cells: wrap.cells,
        num_lines: wrap.next_line,
    };
    debug_assert_eq!(wrapped.validate(), Ok(()));
    Ok((wrapped, init2))
}

/// Lower a Core Π iso and Bennett-wrap the result: the ancilla-clean
/// (up to the boundary residue) lowering path for ring bodies.
pub fn lower_iso_bennett(
    iso: &theseus_core::Iso,
) -> Result<(PhasedCellNetlist, Vec<bool>), BennettWrapError> {
    let (net, inits) =
        crate::lower::lower_iso_with_ancillas(iso).map_err(BennettWrapError::Lower)?;
    bennett_wrap(&net, &inits).map_err(BennettWrapError::Wrap)
}

/// Error from [`lower_iso_bennett`]: lowering or wrapping failed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BennettWrapError {
    Lower(LowerError),
    Wrap(BennettError),
}

impl fmt::Display for BennettWrapError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BennettWrapError::Lower(e) => write!(f, "{e}"),
            BennettWrapError::Wrap(e) => write!(f, "{e}"),
        }
    }
}
