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
/// `num_operands` plane sets. Lanes past `inputs.len()` are zero.
pub fn pack_lanes(inputs: &[&[u8]], num_operands: usize) -> Vec<[Lane; OPERAND_BITS]> {
    assert!(
        inputs.len() <= LANES,
        "pack_lanes: {} inputs > LANES",
        inputs.len()
    );
    let mut vars = vec![[LANE_ZERO; OPERAND_BITS]; num_operands];
    for (i, operands) in inputs.iter().enumerate() {
        pack_lane(&mut vars, i, operands);
    }
    vars
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
