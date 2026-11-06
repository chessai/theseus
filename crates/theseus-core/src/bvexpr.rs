//! Symbolic bit-vector expression AST and concrete evaluator.
//!
//! Moved from `theseus-tisc/src/bveq.rs` so that `PrimSpec` (a per-bit
//! `BvExpr` register-transfer) can live in theseus-core without pulling
//! in the BDD crate. All BDD machinery (`expr_to_bdd`, `VarMap`, `equiv`)
//! stays in theseus-tisc.
//!
//! [`eval_expr`] is the scalar reference; [`eval_expr_lanes`] is its
//! bit-sliced twin for whole-domain sweeps (ADR-0049).

/// Per-bit boolean expression over w-bit operand lanes (w ≤ 8).
///
/// No `Mul` variant -- kept unrepresentable so BDDs stay polynomial (r2 I3).
/// The enum makes multiply impossible by omission.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BvExpr {
    /// Input variable: bit `bit` of operand (register) `operand`.
    Var { operand: usize, bit: usize },
    /// Boolean constant.
    Const(bool),
    /// Bitwise NOT.
    Not(Box<BvExpr>),
    /// Bitwise XOR.
    Xor(Box<BvExpr>, Box<BvExpr>),
    /// Bitwise AND.
    And(Box<BvExpr>, Box<BvExpr>),
    /// Bitwise OR (needed for ripple-carry and barrel mux).
    Or(Box<BvExpr>, Box<BvExpr>),
    /// 2:1 multiplexer: if `sel` then `then_` else `else_`.
    /// Used by Cswap (bit-0 mux) and variable-rotate (barrel network).
    Mux {
        sel: Box<BvExpr>,
        then_: Box<BvExpr>,
        else_: Box<BvExpr>,
    },
}

impl BvExpr {
    pub fn var(operand: usize, bit: usize) -> Self {
        BvExpr::Var { operand, bit }
    }

    pub fn constant(b: bool) -> Self {
        BvExpr::Const(b)
    }

    // Fluent builder consistent with `.xor()/.and()/.or()`; not the std trait.
    #[allow(clippy::should_implement_trait)]
    pub fn not(self) -> Self {
        BvExpr::Not(Box::new(self))
    }

    pub fn xor(self, other: Self) -> Self {
        BvExpr::Xor(Box::new(self), Box::new(other))
    }

    pub fn and(self, other: Self) -> Self {
        BvExpr::And(Box::new(self), Box::new(other))
    }

    pub fn or(self, other: Self) -> Self {
        BvExpr::Or(Box::new(self), Box::new(other))
    }

    pub fn mux(sel: Self, then_: Self, else_: Self) -> Self {
        BvExpr::Mux {
            sel: Box::new(sel),
            then_: Box::new(then_),
            else_: Box::new(else_),
        }
    }
}

/// Evaluate a `BvExpr` at concrete operand byte values.
///
/// The scalar reference; whole-domain sweeps use [`eval_expr_lanes`].
pub fn eval_expr(expr: &BvExpr, operands: &[u8]) -> bool {
    match expr {
        BvExpr::Var { operand, bit } => (operands[*operand] >> *bit) & 1 == 1,
        BvExpr::Const(b) => *b,
        BvExpr::Not(a) => !eval_expr(a, operands),
        BvExpr::Xor(a, b) => eval_expr(a, operands) ^ eval_expr(b, operands),
        BvExpr::And(a, b) => eval_expr(a, operands) & eval_expr(b, operands),
        BvExpr::Or(a, b) => eval_expr(a, operands) | eval_expr(b, operands),
        BvExpr::Mux { sel, then_, else_ } => {
            if eval_expr(sel, operands) {
                eval_expr(then_, operands)
            } else {
                eval_expr(else_, operands)
            }
        }
    }
}

// ── Lane-parallel (bit-sliced) evaluation, ADR-0049 ─────────────────
//
// Whole-domain sweeps evaluate the same tree on many inputs. Bit-slicing
// walks the tree once per `LANES` inputs: each node yields one `Lane`
// (bit `i` = input `i`) via plain word ops.

/// Inputs evaluated per lane-parallel call.
pub const LANES: usize = 256;

/// Bit planes per operand (operands are at most a byte wide).
pub const OPERAND_BITS: usize = 8;

/// One boolean across [`LANES`] inputs: input `i` is bit `i % 64` of word
/// `i / 64`. Deliberately concrete, not generic: the evaluator is then
/// compiled in theseus-core (dev opt-level 2) whichever crate calls it.
pub type Lane = [u64; 4];

/// Every lane false.
pub const LANE_ZERO: Lane = [0; 4];
/// Every lane true.
pub const LANE_ONES: Lane = [u64::MAX; 4];

/// Lane `i` of `lane`. Panics if `i >= LANES`.
#[inline]
pub fn lane_get(lane: &Lane, i: usize) -> bool {
    (lane[i / 64] >> (i % 64)) & 1 == 1
}

/// Transpose one input's operand bytes into lane `i` of `vars`
/// (`vars[op][bit]`); operands past `operands.len()` read as zero in lane
/// `i`. Other lanes are untouched. Panics if `i >= LANES` or
/// `operands.len() > vars.len()`.
pub fn pack_lane(vars: &mut [[Lane; OPERAND_BITS]], i: usize, operands: &[u8]) {
    let (w, s) = (i / 64, i % 64);
    let (given, rest) = vars.split_at_mut(operands.len());
    for (planes, &byte) in given.iter_mut().zip(operands) {
        for (bit, plane) in planes.iter_mut().enumerate() {
            plane[w] = (plane[w] & !(1 << s)) | ((((byte >> bit) & 1) as u64) << s);
        }
    }
    for planes in rest {
        for plane in planes.iter_mut() {
            plane[w] &= !(1 << s);
        }
    }
}

/// Pack up to [`LANES`] operand vectors (input `i` → lane `i`) into
/// `num_operands` plane sets. Lanes past `inputs.len()` are zero, and so
/// are operands past an input's length.
///
/// One byte column per operand, transposed 64 lanes at a time by
/// [`crate::simd::bytes_to_planes`] (ADR-0052). Panics, like
/// [`pack_lane`], if an input has more than `num_operands` operands.
pub fn pack_lanes(inputs: &[&[u8]], num_operands: usize) -> Vec<[Lane; OPERAND_BITS]> {
    assert!(
        inputs.len() <= LANES,
        "pack_lanes: {} inputs > LANES",
        inputs.len()
    );
    if let Some(i) = inputs.iter().position(|o| o.len() > num_operands) {
        panic!(
            "pack_lanes: input {i} has {} operands > {num_operands}",
            inputs[i].len()
        );
    }
    let mut vars = vec![[LANE_ZERO; OPERAND_BITS]; num_operands];
    let mut col = [0u8; LANES];
    for (op, planes) in vars.iter_mut().enumerate() {
        for (c, operands) in col.iter_mut().zip(inputs) {
            *c = operands.get(op).copied().unwrap_or(0);
        }
        planes_from_column(&col, planes);
    }
    vars
}

/// Transpose one operand's byte column (lane `i` = `col[i]`) into its
/// eight planes.
#[inline]
fn planes_from_column(col: &[u8; LANES], planes: &mut [Lane; OPERAND_BITS]) {
    crate::simd::bytes_to_planes(col, planes.as_flattened_mut(), 4, OPERAND_BITS);
}

/// Pack up to [`LANES`] fixed-width records at once: `rows` is row-major,
/// `planes.len()` bytes per input (input `i` → lane `i`). Every plane set
/// is overwritten; lanes past `rows.len() / planes.len()` are zero.
///
/// # Panics
/// If `rows.len()` is not a multiple of `planes.len()` or holds more than
/// [`LANES`] records.
pub fn pack_rows(rows: &[u8], planes: &mut [[Lane; OPERAND_BITS]]) {
    let k = planes.len();
    if k == 0 {
        assert!(
            rows.is_empty(),
            "pack_rows: {} bytes for 0 operands",
            rows.len()
        );
        return;
    }
    assert!(
        rows.len().is_multiple_of(k) && rows.len() / k <= LANES,
        "pack_rows: {} bytes is not <= {LANES} records of {k}",
        rows.len()
    );
    let mut col = [0u8; LANES];
    for (op, set) in planes.iter_mut().enumerate() {
        for (c, row) in col.iter_mut().zip(rows.chunks_exact(k)) {
            *c = row[op];
        }
        planes_from_column(&col, set);
    }
}

/// Inverse of [`pack_rows`] for the first `rows.len() / planes.len()`
/// lanes: `rows[i * k + op]` = the byte in lane `i` of `planes[op]`.
///
/// # Panics
/// If `rows.len()` is not a multiple of `planes.len()` or holds more than
/// [`LANES`] records.
pub fn unpack_rows(planes: &[[Lane; OPERAND_BITS]], rows: &mut [u8]) {
    let k = planes.len();
    if k == 0 {
        assert!(
            rows.is_empty(),
            "unpack_rows: {} bytes for 0 operands",
            rows.len()
        );
        return;
    }
    assert!(
        rows.len().is_multiple_of(k) && rows.len() / k <= LANES,
        "unpack_rows: {} bytes is not <= {LANES} records of {k}",
        rows.len()
    );
    let mut col = [0u8; LANES];
    for (op, set) in planes.iter().enumerate() {
        crate::simd::planes_to_bytes(set.as_flattened(), 4, OPERAND_BITS, &mut col);
        for (row, &c) in rows.chunks_exact_mut(k).zip(&col) {
            row[op] = c;
        }
    }
}

/// Inverse of [`pack_lane`]: `out[op]` = the byte in lane `i` of
/// `planes[op]`. Panics if `out.len() != planes.len()` or `i >= LANES`.
pub fn unpack_lane(planes: &[[Lane; OPERAND_BITS]], i: usize, out: &mut [u8]) {
    assert_eq!(out.len(), planes.len(), "unpack_lane: length mismatch");
    let (w, s) = (i / 64, i % 64);
    for (byte, set) in out.iter_mut().zip(planes) {
        let mut b = 0u8;
        for (bit, plane) in set.iter().enumerate() {
            b |= (((plane[w] >> s) & 1) as u8) << bit;
        }
        *byte = b;
    }
}

#[inline(always)]
fn zip_lanes(a: Lane, b: Lane, f: impl Fn(u64, u64) -> u64) -> Lane {
    std::array::from_fn(|k| f(a[k], b[k]))
}

/// Lane-parallel [`eval_expr`]: lane `i` of the result is `eval_expr(expr,
/// input_i)`, where `vars[op][bit]` holds bit `bit` of operand `op` across
/// the inputs (see [`pack_lane`]). One tree walk per [`LANES`] inputs, no
/// heap allocation. Panics on a `Var` outside `vars`, as `eval_expr` does.
pub fn eval_expr_lanes(expr: &BvExpr, vars: &[[Lane; OPERAND_BITS]]) -> Lane {
    match expr {
        BvExpr::Var { operand, bit } => vars[*operand][*bit],
        BvExpr::Const(true) => LANE_ONES,
        BvExpr::Const(false) => LANE_ZERO,
        BvExpr::Not(a) => eval_expr_lanes(a, vars).map(|x| !x),
        BvExpr::Xor(a, b) => zip_lanes(
            eval_expr_lanes(a, vars),
            eval_expr_lanes(b, vars),
            |x, y| x ^ y,
        ),
        BvExpr::And(a, b) => zip_lanes(
            eval_expr_lanes(a, vars),
            eval_expr_lanes(b, vars),
            |x, y| x & y,
        ),
        BvExpr::Or(a, b) => zip_lanes(
            eval_expr_lanes(a, vars),
            eval_expr_lanes(b, vars),
            |x, y| x | y,
        ),
        BvExpr::Mux { sel, then_, else_ } => {
            let s = eval_expr_lanes(sel, vars);
            let t = eval_expr_lanes(then_, vars);
            let e = eval_expr_lanes(else_, vars);
            std::array::from_fn(|k| (s[k] & t[k]) | (!s[k] & e[k]))
        }
    }
}

// ── Flattened lane programs (ADR-0052) ──────────────────────────────
//
// `eval_expr_lanes` re-walks the `Box` tree for every 256-input chunk.
// A sweep evaluates the same trees on every chunk, so `LaneProgram`
// compiles them once into a branch-free single-assignment program of
// uniform mux cells, which the SIMD kernels in `crate::simd` evaluate.

use crate::simd::bvprog::{MuxCell, MuxProgram, NEG};
use crate::simd::{self, SimdLevel};
use std::collections::BTreeMap;

/// The all-zeros slot (read negated: all ones).
const ZERO: u32 = 0;
/// [`ZERO`] read negated.
const ONES: u32 = ZERO | NEG;
/// Marks a compile-time ref to cell `i` (renumbered after the var slots
/// are laid out).
const CELL: u32 = 1 << 30;

/// A set of `BvExpr`s compiled once into a single-assignment program of
/// uniform *mux cells* (ADR-0052), then evaluated per 256-input chunk of
/// a domain sweep by an SSE2/AVX2/scalar kernel. Lane `i` of output `o`
/// equals [`eval_expr_lanes`] on output `o`'s tree.
///
/// Every node becomes `slot[d] = e ^ (s & (t ^ e))` (`mux(s, t, e)`) over
/// slot refs carrying a "read negated" bit: `And(a, b) = mux(a, b, 0)`,
/// `Or(a, b) = mux(a, 1, b)`, `Xor(a, b) = mux(a, !b, b)`, and `Not` only
/// flips a polarity bit. The kernels then run one straight-line cell body
/// per node, with no dispatch. Structurally equal subtrees are shared
/// (hash-consing, commutative operands sorted), constant and degenerate
/// muxes fold away -- so a ripple-carry adder, whose `BvExpr` clones its
/// carry chain into every sum bit, compiles to a linear-size DAG.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LaneProgram {
    cells: MuxProgram,
    n_outputs: usize,
    /// `(output, ref)` for every named output.
    outputs: Vec<(usize, u32)>,
    /// Operands whose planes the program reads (slots `1..=8 * operands`).
    operands: usize,
    /// Some `Var` names a bit past the operand planes.
    bad_bit: bool,
}

impl LaneProgram {
    /// Compile `outputs` (`(o, tree)`: output `o` is `tree`) for a program
    /// with `n_outputs` outputs; outputs never named evaluate to zero.
    ///
    /// # Panics
    /// If an output index is `>= n_outputs`.
    pub fn new<'a>(
        n_outputs: usize,
        outputs: impl IntoIterator<Item = (usize, &'a BvExpr)>,
    ) -> Self {
        let mut c = Compiler::default();
        let mut named = Vec::new();
        for (o, expr) in outputs {
            assert!(o < n_outputs, "LaneProgram: output {o} >= {n_outputs}");
            let r = c.emit(expr);
            named.push((o, r));
        }
        // Lay out the slots: ZERO, then every operand's 8 planes, then the
        // cells in ASAP level order (1 + the deepest cell they read; stable
        // within a level). Emission order is depth-first, so a cell usually
        // reads the one just before it; level order puts independent cells
        // side by side and their store-to-load chains overlap.
        let var_end = 1 + OPERAND_BITS * c.operands;
        let n_slots = var_end + c.cells.len();
        let cell_of = |r: u32| (r & CELL != 0).then_some((r & !(CELL | NEG)) as usize);
        let mut level = vec![0u32; c.cells.len()];
        for i in 0..c.cells.len() {
            let deepest = c.cells[i]
                .iter()
                .filter_map(|&r| cell_of(r))
                .map(|j| level[j]);
            level[i] = 1 + deepest.max().unwrap_or(0);
        }
        let mut order: Vec<usize> = (0..c.cells.len()).collect();
        order.sort_by_key(|&i| level[i]);
        let mut slot = vec![0u32; c.cells.len()];
        for (pos, &i) in order.iter().enumerate() {
            slot[i] = u32::try_from(var_end + pos).expect("slot fits u32");
        }
        let fix = |r: u32| match cell_of(r) {
            Some(i) => (r & NEG) | slot[i],
            None => r,
        };
        let cells = order
            .iter()
            .map(|&i| MuxCell {
                refs: c.cells[i].map(fix),
                dst: slot[i],
            })
            .collect();
        LaneProgram {
            cells: MuxProgram::new(cells, n_slots),
            n_outputs,
            outputs: named.into_iter().map(|(o, r)| (o, fix(r))).collect(),
            operands: c.operands,
            bad_bit: c.bad_bit,
        }
    }

    /// Number of outputs (the length `eval` expects for `out`).
    pub fn n_outputs(&self) -> usize {
        self.n_outputs
    }

    /// Number of mux cells (after sharing and folding).
    pub fn len(&self) -> usize {
        self.cells.len()
    }

    /// No cells (every output is a constant or a bare variable).
    pub fn is_empty(&self) -> bool {
        self.cells.len() == 0
    }

    /// Evaluate on one chunk at the active [`simd::level`]: `out[o]` is
    /// output `o` over the inputs packed in `vars` (see [`pack_lane`]).
    ///
    /// # Panics
    /// If `out.len() != self.n_outputs()`, or a `Var` reads outside `vars`
    /// (where [`eval_expr_lanes`] panics too).
    pub fn eval(&self, vars: &[[Lane; OPERAND_BITS]], out: &mut [Lane]) {
        self.eval_with(simd::level(), vars, out);
    }

    /// [`Self::eval`] at a given level (capped at what the CPU has).
    pub fn eval_with(&self, level: SimdLevel, vars: &[[Lane; OPERAND_BITS]], out: &mut [Lane]) {
        assert_eq!(
            out.len(),
            self.n_outputs,
            "LaneProgram::eval: {} outputs, want {}",
            out.len(),
            self.n_outputs
        );
        assert!(
            !self.bad_bit && self.operands <= vars.len(),
            "LaneProgram::eval: a Var reads outside the {} operand plane sets",
            vars.len()
        );
        let mut slots = vec![LANE_ZERO; self.cells.n_slots()];
        let planes = &vars.as_flattened()[..OPERAND_BITS * self.operands];
        slots[1..=planes.len()].copy_from_slice(planes);
        self.cells.run(level, &mut slots);
        out.fill(LANE_ZERO);
        for &(o, r) in &self.outputs {
            let neg = 0u64.wrapping_sub(u64::from(r >> 31));
            out[o] = slots[(r & !NEG) as usize].map(|w| w ^ neg);
        }
    }
}

/// [`LaneProgram`] code generation: a hash-consed mux-cell DAG.
#[derive(Default)]
struct Compiler {
    /// Cell `i`'s `[s, t, e]` refs (compile-time numbering).
    cells: Vec<[u32; 3]>,
    /// Structural sharing: `[s, t, e]` -> the cell's ref.
    memo: BTreeMap<[u32; 3], u32>,
    operands: usize,
    bad_bit: bool,
}

impl Compiler {
    /// The ref computing `e` (polarity bit included).
    fn emit(&mut self, e: &BvExpr) -> u32 {
        match e {
            BvExpr::Var { operand, bit } => {
                if *bit >= OPERAND_BITS {
                    self.bad_bit = true;
                    return ZERO;
                }
                let slot = operand
                    .checked_mul(OPERAND_BITS)
                    .and_then(|s| s.checked_add(1 + bit))
                    .and_then(|s| u32::try_from(s).ok())
                    .filter(|&s| s < CELL)
                    .expect("var slot fits the ref space");
                self.operands = self.operands.max(operand + 1);
                slot
            }
            BvExpr::Const(false) => ZERO,
            BvExpr::Const(true) => ONES,
            BvExpr::Not(a) => self.emit(a) ^ NEG,
            BvExpr::And(a, b) => {
                let (x, y) = self.pair(a, b);
                self.mux(x, y, ZERO)
            }
            BvExpr::Or(a, b) => {
                let (x, y) = self.pair(a, b);
                self.mux(x, ONES, y)
            }
            BvExpr::Xor(a, b) => {
                let (x, y) = self.pair(a, b);
                self.mux(x, y ^ NEG, y)
            }
            BvExpr::Mux { sel, then_, else_ } => {
                let s = self.emit(sel);
                let t = self.emit(then_);
                let e = self.emit(else_);
                self.mux(s, t, e)
            }
        }
    }

    /// A commutative operator's operands, in canonical (sorted) order.
    fn pair(&mut self, a: &BvExpr, b: &BvExpr) -> (u32, u32) {
        let (x, y) = (self.emit(a), self.emit(b));
        (x.min(y), x.max(y))
    }

    /// The ref of `mux(s, t, e)`, folding constant and degenerate muxes
    /// and sharing structurally equal cells.
    fn mux(&mut self, s: u32, t: u32, e: u32) -> u32 {
        match (s, t, e) {
            (ZERO, _, e) => return e,
            (ONES, t, _) => return t,
            (_, t, e) if t == e => return t,
            (s, ONES, ZERO) => return s,
            (s, ZERO, ONES) => return s ^ NEG,
            _ => {}
        }
        // mux(!s, t, e) = mux(s, e, t); mux(s, !t, !e) = !mux(s, t, e).
        let (s, t, e) = if s & NEG != 0 {
            (s ^ NEG, e, t)
        } else {
            (s, t, e)
        };
        let flip = t & NEG;
        let key = [s, t ^ flip, e ^ flip];
        let r = match self.memo.get(&key) {
            Some(&r) => r,
            None => {
                let r = CELL | u32::try_from(self.cells.len()).expect("cell count fits u32");
                assert!(r & NEG == 0, "lane program exceeds 2^30 cells");
                self.cells.push(key);
                self.memo.insert(key, r);
                r
            }
        };
        r ^ flip
    }
}

/// Fixed-seed random `BvExpr`s for property tests (no `rand` dependency).
#[cfg(test)]
pub(crate) mod testgen {
    use super::BvExpr;

    /// splitmix64 (Steele, Lea & Flood 2014).
    pub(crate) struct SplitMix64(pub(crate) u64);

    impl SplitMix64 {
        pub(crate) fn next_u64(&mut self) -> u64 {
            self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
            z ^ (z >> 31)
        }

        /// Roughly uniform in `0..n` (`n > 0`).
        pub(crate) fn below(&mut self, n: usize) -> usize {
            (self.next_u64() % n as u64) as usize
        }
    }

    /// A random tree of depth ≤ `depth` over `operands` × `bits` variables.
    pub(crate) fn random_expr(
        rng: &mut SplitMix64,
        depth: usize,
        operands: usize,
        bits: usize,
    ) -> BvExpr {
        if depth == 0 || rng.below(6) == 0 {
            return if rng.below(8) == 0 {
                BvExpr::constant(rng.below(2) == 1)
            } else {
                BvExpr::var(rng.below(operands), rng.below(bits))
            };
        }
        let sub = |rng: &mut SplitMix64| random_expr(rng, depth - 1, operands, bits);
        match rng.below(5) {
            0 => sub(rng).not(),
            1 => sub(rng).xor(sub(rng)),
            2 => sub(rng).and(sub(rng)),
            3 => sub(rng).or(sub(rng)),
            _ => BvExpr::mux(sub(rng), sub(rng), sub(rng)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testgen::{random_expr, SplitMix64};
    use super::*;

    /// Chunk sizes covering a lone lane, every word boundary, partial
    /// chunks, and the full chunk.
    const SIZES: [usize; 13] = [1, 2, 63, 64, 65, 127, 128, 129, 191, 192, 193, 255, 256];

    fn random_inputs(rng: &mut SplitMix64, n: usize, operands: usize) -> Vec<Vec<u8>> {
        (0..n)
            .map(|_| (0..operands).map(|_| rng.next_u64() as u8).collect())
            .collect()
    }

    #[test]
    fn pack_unpack_round_trip() {
        let mut rng = SplitMix64(0x5EED_0001);
        for &n in &SIZES {
            let inputs = random_inputs(&mut rng, n, 5);
            let refs: Vec<&[u8]> = inputs.iter().map(Vec::as_slice).collect();
            let vars = pack_lanes(&refs, 5);
            let mut out = [0u8; 5];
            for (i, input) in inputs.iter().enumerate() {
                unpack_lane(&vars, i, &mut out);
                assert_eq!(&out[..], &input[..], "lane {i} of {n}");
                for (op, &byte) in input.iter().enumerate() {
                    for (bit, plane) in vars[op].iter().enumerate() {
                        assert_eq!(lane_get(plane, i), (byte >> bit) & 1 == 1);
                    }
                }
            }
            for i in n..LANES {
                unpack_lane(&vars, i, &mut out);
                assert_eq!(out, [0u8; 5], "unused lane {i} of {n} must be zero");
            }
        }
    }

    #[test]
    fn pack_lane_overwrites_and_zero_extends() {
        let mut vars = vec![[LANE_ONES; OPERAND_BITS]; 3];
        pack_lane(&mut vars, 70, &[0xA5]);
        let mut out = [0u8; 3];
        unpack_lane(&vars, 70, &mut out);
        assert_eq!(out, [0xA5, 0, 0]);
        unpack_lane(&vars, 69, &mut out);
        assert_eq!(out, [0xFF; 3], "neighbouring lanes untouched");
    }

    #[test]
    fn eval_expr_lanes_node_semantics() {
        // Lanes 0..4 hold bytes 0b01, 0b10, 0b11, 0b00 (a = bit 0, b = bit 1).
        let vars = pack_lanes(&[&[0b01], &[0b10], &[0b11], &[0b00]], 1);
        let (a, b) = (BvExpr::var(0, 0), BvExpr::var(0, 1));
        let low4 = |e: &BvExpr| (eval_expr_lanes(e, &vars)[0] & 0xF) as u8;
        assert_eq!(low4(&a), 0b0101);
        assert_eq!(low4(&b), 0b0110);
        assert_eq!(low4(&a.clone().not()), 0b1010);
        assert_eq!(low4(&a.clone().xor(b.clone())), 0b0011);
        assert_eq!(low4(&a.clone().and(b.clone())), 0b0100);
        assert_eq!(low4(&a.clone().or(b.clone())), 0b0111);
        assert_eq!(low4(&BvExpr::mux(a, b, BvExpr::constant(true))), 0b1110);
        assert_eq!(eval_expr_lanes(&BvExpr::constant(true), &vars), LANE_ONES);
        assert_eq!(eval_expr_lanes(&BvExpr::constant(false), &vars), LANE_ZERO);
    }

    /// Lane `i` of `eval_expr_lanes` == scalar `eval_expr` on input `i`, for
    /// random trees (depth ≤ 8, ≤ 8 operands, ≤ 8 bits) and random inputs.
    #[test]
    fn eval_expr_lanes_matches_scalar() {
        let mut rng = SplitMix64(0x1A9E_5EED);
        for case in 0..400 {
            let operands = 1 + rng.below(8);
            let bits = 1 + rng.below(OPERAND_BITS);
            let depth = rng.below(9);
            let expr = random_expr(&mut rng, depth, operands, bits);
            let n = match SIZES.get(case) {
                Some(&n) => n,
                None => 1 + rng.below(LANES),
            };
            let inputs = random_inputs(&mut rng, n, operands);
            let refs: Vec<&[u8]> = inputs.iter().map(Vec::as_slice).collect();
            let lanes = eval_expr_lanes(&expr, &pack_lanes(&refs, operands));
            for (i, input) in inputs.iter().enumerate() {
                assert_eq!(
                    lane_get(&lanes, i),
                    eval_expr(&expr, input),
                    "case {case}, lane {i} of {n}: {expr:?} on {input:?}"
                );
            }
        }
    }
}

/// ADR-0052: the row transposes and the flattened [`LaneProgram`] against
/// the per-lane and recursive references, at every SIMD level the host has.
#[cfg(test)]
mod program_tests {
    use super::testgen::{random_expr, SplitMix64};
    use super::*;

    const SIZES: [usize; 9] = [0, 1, 63, 64, 65, 128, 191, 255, 256];

    #[test]
    fn pack_rows_matches_per_lane_packing() {
        let mut rng = SplitMix64(0x5EED_0002);
        for k in [0usize, 1, 3, 8] {
            for &n in &SIZES {
                let rows: Vec<u8> = (0..n * k).map(|_| rng.next_u64() as u8).collect();
                let mut want = vec![[LANE_ZERO; OPERAND_BITS]; k];
                for (i, row) in rows.chunks_exact(k.max(1)).enumerate().take(n) {
                    pack_lane(&mut want, i, row);
                }
                let mut got = vec![[LANE_ONES; OPERAND_BITS]; k];
                pack_rows(&rows, &mut got);
                assert_eq!(got, want, "k = {k}, n = {n}");
                let refs: Vec<&[u8]> = rows.chunks_exact(k.max(1)).take(n).collect();
                if k > 0 {
                    assert_eq!(pack_lanes(&refs, k), want, "pack_lanes, k = {k}, n = {n}");
                }
                let mut back = vec![0xA5u8; n * k];
                unpack_rows(&got, &mut back);
                assert_eq!(back, rows, "unpack_rows, k = {k}, n = {n}");
            }
        }
        // Short inputs zero-extend; long ones are rejected, as pack_lane.
        let vars = pack_lanes(&[&[0xFF], &[]], 2);
        let mut out = [0u8; 2];
        unpack_lane(&vars, 0, &mut out);
        assert_eq!(out, [0xFF, 0]);
        assert!(std::panic::catch_unwind(|| pack_lanes(&[&[1, 2, 3]], 2)).is_err());
    }

    /// A program over `exprs` against `eval_expr_lanes`, every level.
    fn check_program(exprs: &[BvExpr], vars: &[[Lane; OPERAND_BITS]], what: &str) {
        // Interleave unnamed outputs: they must come back zero.
        let prog = LaneProgram::new(
            2 * exprs.len(),
            exprs.iter().enumerate().map(|(o, e)| (2 * o, e)),
        );
        for level in SimdLevel::available() {
            let mut out = vec![LANE_ONES; 2 * exprs.len()];
            prog.eval_with(level, vars, &mut out);
            for (o, e) in exprs.iter().enumerate() {
                assert_eq!(
                    out[2 * o],
                    eval_expr_lanes(e, vars),
                    "{what}, {level}: {e:?}"
                );
                assert_eq!(out[2 * o + 1], LANE_ZERO, "{what}, {level}: unnamed output");
            }
        }
    }

    #[test]
    fn lane_program_matches_recursive_walk() {
        let mut rng = SplitMix64(0x9E0_6EA4);
        let cases = if cfg!(miri) { 20 } else { 300 };
        for case in 0..cases {
            let operands = 1 + rng.below(8);
            let bits = 1 + rng.below(OPERAND_BITS);
            let exprs: Vec<BvExpr> = (0..1 + rng.below(6))
                .map(|_| {
                    let depth = rng.below(10);
                    random_expr(&mut rng, depth, operands, bits)
                })
                .collect();
            let n = SIZES[case % SIZES.len()];
            let rows: Vec<u8> = (0..n * operands).map(|_| rng.next_u64() as u8).collect();
            let mut vars = vec![[LANE_ZERO; OPERAND_BITS]; operands];
            pack_rows(&rows, &mut vars);
            check_program(&exprs, &vars, &format!("case {case}"));
        }
    }

    #[test]
    fn lane_program_deep_trees_sharing_and_edge_shapes() {
        let mut rng = SplitMix64(0xDEE9);
        let vars: Vec<[Lane; OPERAND_BITS]> = (0..3)
            .map(|_| std::array::from_fn(|_| std::array::from_fn(|_| rng.next_u64())))
            .collect();
        let v = BvExpr::var;
        // A deep right-leaning chain mixing every operator.
        let mut deep = v(0, 0).and(v(1, 1));
        for i in 0..40 {
            let leaf = v(i % 3, i % 8).and(v((i + 1) % 3, (i + 5) % 8));
            deep = match i % 3 {
                0 => leaf.xor(deep),
                1 => leaf.or(deep),
                _ => BvExpr::mux(v(2, i % 8).not(), leaf, deep),
            };
        }
        check_program(&[deep], &vars, "deep");
        // Sharing: a ripple-carry adder clones its carry into every sum bit
        // (the tree doubles per bit); the compiled DAG stays linear.
        let mut carry = BvExpr::constant(false);
        let mut sum = Vec::new();
        for i in 0..8 {
            let (a, b) = (v(0, i), v(1, i));
            sum.push(a.clone().xor(b.clone()).xor(carry.clone()));
            carry = a.clone().and(b.clone()).or(carry.and(a.xor(b)));
        }
        let prog = LaneProgram::new(8, sum.iter().enumerate());
        assert!(
            prog.len() <= 8 * 5,
            "{} cells for an 8-bit adder",
            prog.len()
        );
        check_program(&sum, &vars, "adder");
        // Repeated and commuted subtrees share one cell.
        let x = v(0, 1).and(v(2, 3));
        let shared = LaneProgram::new(2, [(0, &x), (1, &v(2, 3).and(v(0, 1)))]);
        assert_eq!(shared.len(), 1);
        // Constants in every position, vars on either side, bare leaves.
        let shapes = [
            v(0, 3),
            BvExpr::constant(true),
            BvExpr::constant(false),
            v(1, 2).not(),
            BvExpr::constant(true).xor(v(0, 0)),
            v(0, 0).and(BvExpr::constant(false)),
            BvExpr::constant(false).or(BvExpr::constant(true)),
            v(2, 7).xor(v(1, 6).and(v(0, 5))),
            BvExpr::mux(BvExpr::constant(true), v(0, 1), BvExpr::constant(false)),
            BvExpr::mux(v(0, 1), BvExpr::constant(true), v(2, 2)),
        ];
        check_program(&shapes, &vars, "shapes");
        // No outputs at all.
        let empty = LaneProgram::new(3, []);
        assert!(empty.is_empty());
        let mut out = [LANE_ONES; 3];
        empty.eval(&vars, &mut out);
        assert_eq!(out, [LANE_ZERO; 3]);
    }

    #[test]
    fn lane_program_rejects_vars_outside_the_planes() {
        let vars = vec![[LANE_ZERO; OPERAND_BITS]; 2];
        let far = BvExpr::var(2, 0);
        let prog = LaneProgram::new(1, [(0, &far)]);
        let r = std::panic::catch_unwind(|| prog.eval(&vars, &mut [LANE_ZERO]));
        assert!(r.is_err(), "operand 2 of 2");
        let wide = BvExpr::var(0, 8);
        let prog = LaneProgram::new(1, [(0, &wide)]);
        let r = std::panic::catch_unwind(|| prog.eval(&vars, &mut [LANE_ZERO]));
        assert!(r.is_err(), "bit 8");
        assert!(std::panic::catch_unwind(|| LaneProgram::new(1, [(1, &far)])).is_err());
    }
}
