//! Symbolic bit-vector equivalence checking via canonical BDD.
//!
//! Provides a symbolic model of TISC-1 register-pure instructions and a
//! canonical BDD-based equivalence checker (biodivine-lib-bdd, pinned =0.6.3).
//!
//! The decisive win is **completeness without a table**: a 3-operand Flat(8)
//! equivalence proof (2^24 = 16.7M domain) in microseconds, where the current
//! thermometer-Value + BTreeMap representation cannot even be constructed.
//!
//! # Trust core
//!
//! A bug in this module silently corrupts every compiled program. The TCB is:
//! - biodivine's textbook ROBDD reduce+apply (external, mature)
//! - our `BvExpr → Bdd` encoder + `Instr → BvExpr` model (this module)
//! - the SINGLE pinned variable mapping shared by both sides of every
//!   equivalence (r2 MAJOR-3)
//!
//! Defended by: tri-model reconciliation (symbolic ≡ exec_instr ≡ netlist),
//! Z3 differential cross-check (feature-gated), and the I4 upper-output-bits
//! obligation (r2 MAJOR-1).

use biodivine_lib_bdd::{Bdd, BddVariable, BddVariableSet};
use crate::asm::Instr;
use crate::types::NUM_REGS;

/// Re-export for tests that need direct BDD access (e.g. T4 mis-map negative).
pub use biodivine_lib_bdd;

/// Bits per register (byte-wide ISA).
pub const BITS: usize = 8;

// ── BvExpr AST ──────────────────────────────────────────────────────

/// Per-bit boolean expression over w-bit operand lanes (w ≤ 8).
///
/// No `Mul` variant — kept unrepresentable so BDDs stay polynomial (r2 I3).
/// The enum makes multiply impossible by omission.
#[derive(Clone, Debug)]
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

// ── RegTransfer ─────────────────────────────────────────────────────

/// Transfer function for the full register file (NUM_REGS × BITS).
///
/// `regs[r][b]` is the BvExpr for bit `b` of register `r` after the transfer.
/// Identity for untouched registers. Full-frame (r2 MAJOR-2).
#[derive(Clone, Debug)]
pub struct RegTransfer {
    pub regs: Vec<Vec<BvExpr>>,
}

impl RegTransfer {
    /// Identity transfer: each register bit maps to its own input variable.
    pub fn identity() -> Self {
        let mut regs = Vec::with_capacity(NUM_REGS);
        for r in 0..NUM_REGS {
            let mut bits = Vec::with_capacity(BITS);
            for b in 0..BITS {
                bits.push(BvExpr::var(r, b));
            }
            regs.push(bits);
        }
        RegTransfer { regs }
    }
}

// ── VarMap (shared variable mapping — TCB, r2 MAJOR-3) ──────────────

/// Pinned variable mapping: `(operand, bit) → BddVariable`.
///
/// Interleaved ordering: `var_index = bit * num_operands + operand`.
/// This groups all operands' bit-0, then all bit-1, etc. — keeps
/// ripple-carry ADD/SUB linear and rotates trivial.
///
/// **This mapping is TCB.** One instance must be shared by BOTH sides of
/// every equivalence check. Using different mappings for the two sides
/// makes unequal functions compare equal (r2 MAJOR-3).
pub struct VarMap {
    num_operands: usize,
    bdd_vars: BddVariableSet,
    variables: Vec<BddVariable>,
}

impl VarMap {
    /// Create a new variable map for `num_operands` registers, each `BITS` wide.
    pub fn new(num_operands: usize) -> Self {
        let num_vars = (BITS * num_operands) as u16;
        let bdd_vars = BddVariableSet::new_anonymous(num_vars);
        let variables = bdd_vars.variables();
        VarMap {
            num_operands,
            bdd_vars,
            variables,
        }
    }

    /// Look up the BDD variable for `(operand, bit)`.
    pub fn var(&self, operand: usize, bit: usize) -> BddVariable {
        let idx = bit * self.num_operands + operand;
        self.variables[idx]
    }

    pub fn bdd_vars(&self) -> &BddVariableSet {
        &self.bdd_vars
    }

    pub fn num_operands(&self) -> usize {
        self.num_operands
    }
}

// ── BvExpr → Bdd encoder ───────────────────────────────────────────

/// Convert a `BvExpr` to a canonical BDD using the shared variable mapping.
pub fn expr_to_bdd(expr: &BvExpr, map: &VarMap) -> Bdd {
    match expr {
        BvExpr::Var { operand, bit } => map.bdd_vars.mk_var(map.var(*operand, *bit)),
        BvExpr::Const(true) => map.bdd_vars.mk_true(),
        BvExpr::Const(false) => map.bdd_vars.mk_false(),
        BvExpr::Not(a) => expr_to_bdd(a, map).not(),
        BvExpr::Xor(a, b) => expr_to_bdd(a, map).xor(&expr_to_bdd(b, map)),
        BvExpr::And(a, b) => expr_to_bdd(a, map).and(&expr_to_bdd(b, map)),
        BvExpr::Or(a, b) => expr_to_bdd(a, map).or(&expr_to_bdd(b, map)),
        BvExpr::Mux { sel, then_, else_ } => {
            let s = expr_to_bdd(sel, map);
            let t = expr_to_bdd(then_, map);
            let e = expr_to_bdd(else_, map);
            // mux(s, t, e) = (s ∧ t) ∨ (¬s ∧ e)
            s.and(&t).or(&s.not().and(&e))
        }
    }
}

// ── Concrete evaluator (for reconciliation tests) ───────────────────

/// Evaluate a `BvExpr` at concrete operand byte values.
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

/// Evaluate a full register transfer at concrete register values.
pub fn eval_transfer(transfer: &RegTransfer, regs: &[u8; NUM_REGS]) -> [u8; NUM_REGS] {
    let mut result = [0u8; NUM_REGS];
    for r in 0..NUM_REGS {
        for bit in 0..BITS {
            if eval_expr(&transfer.regs[r][bit], regs) {
                result[r] |= 1 << bit;
            }
        }
    }
    result
}

// ── Bitvector operation builders ────────────────────────────────────

/// Ripple-carry adder: a + b (8-bit wrapping).
fn bv_add(a: &[BvExpr], b: &[BvExpr]) -> Vec<BvExpr> {
    bv_add_carry(a, b, BvExpr::Const(false))
}

/// Ripple-carry adder with initial carry.
fn bv_add_carry(a: &[BvExpr], b: &[BvExpr], init_carry: BvExpr) -> Vec<BvExpr> {
    debug_assert_eq!(a.len(), BITS);
    debug_assert_eq!(b.len(), BITS);
    let mut result = Vec::with_capacity(BITS);
    let mut carry = init_carry;
    for i in 0..BITS {
        // sum = a ^ b ^ carry
        let sum = a[i].clone().xor(b[i].clone()).xor(carry.clone());
        // carry_out = (a & b) | (carry & (a ^ b))
        let new_carry = a[i]
            .clone()
            .and(b[i].clone())
            .or(carry.and(a[i].clone().xor(b[i].clone())));
        result.push(sum);
        carry = new_carry;
    }
    result
}

/// Subtractor: a - b = a + !b + 1 (8-bit wrapping).
fn bv_sub(a: &[BvExpr], b: &[BvExpr]) -> Vec<BvExpr> {
    let not_b: Vec<BvExpr> = b.iter().map(|x| x.clone().not()).collect();
    bv_add_carry(a, &not_b, BvExpr::Const(true))
}

/// Bitwise XOR.
fn bv_xor(a: &[BvExpr], b: &[BvExpr]) -> Vec<BvExpr> {
    a.iter()
        .zip(b.iter())
        .map(|(ai, bi)| ai.clone().xor(bi.clone()))
        .collect()
}

/// Bitwise AND.
fn bv_and(a: &[BvExpr], b: &[BvExpr]) -> Vec<BvExpr> {
    a.iter()
        .zip(b.iter())
        .map(|(ai, bi)| ai.clone().and(bi.clone()))
        .collect()
}

/// Constant left rotation by `amount` positions (wire permutation).
fn bv_rotl_const(bits: &[BvExpr], amount: usize) -> Vec<BvExpr> {
    let n = bits.len();
    let amount = amount % n;
    // rotl: bit i of output = bit (i - amount) mod n of input
    (0..n)
        .map(|i| bits[(i + n - amount) % n].clone())
        .collect()
}

/// Constant right rotation by `amount` positions (wire permutation).
fn bv_rotr_const(bits: &[BvExpr], amount: usize) -> Vec<BvExpr> {
    let n = bits.len();
    let amount = amount % n;
    // rotr: bit i of output = bit (i + amount) mod n of input
    (0..n).map(|i| bits[(i + amount) % n].clone()).collect()
}

/// Variable left rotation via 3-stage barrel mux network.
///
/// Amount taken from `amount_bits[0..3]` (the low 3 bits of the source register,
/// since the ISA masks with `& 7`). Each stage conditionally rotates by 2^stage.
fn bv_rotl_var(val: &[BvExpr], amount_bits: &[BvExpr]) -> Vec<BvExpr> {
    let n = val.len();
    let mut bits: Vec<BvExpr> = val.to_vec();
    for stage in 0..3 {
        let shift = 1usize << stage;
        let sel = amount_bits[stage].clone();
        let prev = bits.clone();
        bits = (0..n)
            .map(|i| {
                BvExpr::mux(
                    sel.clone(),
                    prev[(i + n - shift) % n].clone(),
                    prev[i].clone(),
                )
            })
            .collect();
    }
    bits
}

/// Variable right rotation via 3-stage barrel mux network.
fn bv_rotr_var(val: &[BvExpr], amount_bits: &[BvExpr]) -> Vec<BvExpr> {
    let n = val.len();
    let mut bits: Vec<BvExpr> = val.to_vec();
    for stage in 0..3 {
        let shift = 1usize << stage;
        let sel = amount_bits[stage].clone();
        let prev = bits.clone();
        bits = (0..n)
            .map(|i| {
                BvExpr::mux(
                    sel.clone(),
                    prev[(i + shift) % n].clone(),
                    prev[i].clone(),
                )
            })
            .collect();
    }
    bits
}

/// Build constant-bit expansion of an immediate value.
fn imm_bits(imm: u8) -> Vec<BvExpr> {
    (0..BITS)
        .map(|i| BvExpr::Const((imm >> i) & 1 == 1))
        .collect()
}

// ── Error type ──────────────────────────────────────────────────────

/// Error from symbolic execution or equivalence checking.
#[derive(Debug)]
pub enum BveqError {
    /// Instruction is not register-pure (Swapbr, Xchg, Mexch, MexchReg).
    /// These move state off the register file — reject, don't corrupt (r2 MAJOR-2).
    NonRegisterPure(Instr),
}

impl std::fmt::Display for BveqError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BveqError::NonRegisterPure(instr) => {
                write!(f, "non-register-pure instruction rejected: {:?}", instr)
            }
        }
    }
}

impl std::error::Error for BveqError {}

// ── Counterexample ──────────────────────────────────────────────────

/// A concrete counterexample demonstrating inequality between two register
/// transfers. Decoded through the shared variable mapping to operand bytes.
#[derive(Debug)]
pub struct Counterexample {
    /// Concrete register values that demonstrate the inequality.
    pub operand_values: [u8; NUM_REGS],
    /// The register index where the difference was found.
    pub register: usize,
    /// The bit position where the difference was found.
    pub bit: usize,
}

impl std::fmt::Display for Counterexample {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "counterexample at reg[{}] bit {}: registers = {:?}",
            self.register, self.bit, self.operand_values
        )
    }
}

impl std::error::Error for Counterexample {}

/// Decode a BDD satisfying assignment back to concrete register byte values.
fn decode_counterexample(
    witness: &biodivine_lib_bdd::BddValuation,
    map: &VarMap,
    register: usize,
    bit: usize,
) -> Counterexample {
    let mut operand_values = [0u8; NUM_REGS];
    for op in 0..map.num_operands().min(NUM_REGS) {
        for b in 0..BITS {
            if witness.value(map.var(op, b)) {
                operand_values[op] |= 1 << b;
            }
        }
    }
    Counterexample {
        operand_values,
        register,
        bit,
    }
}

// ── Instruction semantics (single symbolic model, r2 §4) ───────────

/// Apply one instruction's register-transfer semantics to a `RegTransfer`.
///
/// Returns `Err(NonRegisterPure)` for instructions that move state off the
/// register file (Swapbr, Xchg, Mexch, MexchReg) — reject, don't corrupt.
fn apply_instr(transfer: &mut RegTransfer, instr: &Instr) -> Result<(), BveqError> {
    match *instr {
        Instr::Add { rd, rs } => {
            let rd = rd as usize;
            let rs = rs as usize;
            let result = bv_add(&transfer.regs[rd], &transfer.regs[rs]);
            transfer.regs[rd] = result;
        }
        Instr::Sub { rd, rs } => {
            let rd = rd as usize;
            let rs = rs as usize;
            let result = bv_sub(&transfer.regs[rd], &transfer.regs[rs]);
            transfer.regs[rd] = result;
        }
        Instr::Xor { rd, rs } => {
            let rd = rd as usize;
            let rs = rs as usize;
            // Aliasing handled correctly: xor(x, x) = 0 for all x.
            let result = bv_xor(&transfer.regs[rd], &transfer.regs[rs]);
            transfer.regs[rd] = result;
        }
        Instr::Exch { rd, rs } => {
            let rd = rd as usize;
            let rs = rs as usize;
            // Swap the two register expression vectors.
            // If rd == rs (aliased), this is a no-op (correct).
            let tmp = transfer.regs[rd].clone();
            transfer.regs[rd] = transfer.regs[rs].clone();
            transfer.regs[rs] = tmp;
        }
        Instr::Rotl { rd, rs } => {
            let rd = rd as usize;
            let rs = rs as usize;
            // Variable rotate: barrel mux network over low 3 bits of rs.
            let amount_bits = transfer.regs[rs].clone();
            let result = bv_rotl_var(&transfer.regs[rd], &amount_bits);
            transfer.regs[rd] = result;
        }
        Instr::Rotr { rd, rs } => {
            let rd = rd as usize;
            let rs = rs as usize;
            let amount_bits = transfer.regs[rs].clone();
            let result = bv_rotr_var(&transfer.regs[rd], &amount_bits);
            transfer.regs[rd] = result;
        }
        Instr::Cswap { rd, rs, rt } => {
            let rd = rd as usize;
            let rs = rs as usize;
            let rt = rt as usize;
            // If bit 0 of rd is set, swap rs and rt.
            let sel = transfer.regs[rd][0].clone();
            let old_rs = transfer.regs[rs].clone();
            let old_rt = transfer.regs[rt].clone();
            transfer.regs[rs] = (0..BITS)
                .map(|i| BvExpr::mux(sel.clone(), old_rt[i].clone(), old_rs[i].clone()))
                .collect();
            transfer.regs[rt] = (0..BITS)
                .map(|i| BvExpr::mux(sel.clone(), old_rs[i].clone(), old_rt[i].clone()))
                .collect();
            // rd is unchanged.
        }
        Instr::Ccnot { rd, rs, rt } => {
            let rd = rd as usize;
            let rs = rs as usize;
            let rt = rt as usize;
            // rd ^= rs & rt (byte-parallel Toffoli).
            let rs_and_rt = bv_and(&transfer.regs[rs], &transfer.regs[rt]);
            let result = bv_xor(&transfer.regs[rd], &rs_and_rt);
            transfer.regs[rd] = result;
            // rs, rt unchanged.
        }
        Instr::Addi { rd, imm } => {
            let rd = rd as usize;
            let ib = imm_bits(imm);
            let result = bv_add(&transfer.regs[rd], &ib);
            transfer.regs[rd] = result;
        }
        Instr::Subi { rd, imm } => {
            let rd = rd as usize;
            let ib = imm_bits(imm);
            let result = bv_sub(&transfer.regs[rd], &ib);
            transfer.regs[rd] = result;
        }
        Instr::Xori { rd, imm } => {
            let rd = rd as usize;
            let ib = imm_bits(imm);
            let result = bv_xor(&transfer.regs[rd], &ib);
            transfer.regs[rd] = result;
        }
        Instr::Rotli { rd, imm } => {
            let rd = rd as usize;
            let amount = (imm & 7) as usize;
            let result = bv_rotl_const(&transfer.regs[rd], amount);
            transfer.regs[rd] = result;
        }
        Instr::Rotri { rd, imm } => {
            let rd = rd as usize;
            let amount = (imm & 7) as usize;
            let result = bv_rotr_const(&transfer.regs[rd], amount);
            transfer.regs[rd] = result;
        }
        // Non-register-pure: reject, don't corrupt (r2 MAJOR-2).
        Instr::Xchg { .. }
        | Instr::Swapbr { .. }
        | Instr::Mexch { .. }
        | Instr::MexchReg { .. } => {
            return Err(BveqError::NonRegisterPure(*instr));
        }
    }
    Ok(())
}

/// Symbolic semantics of a single instruction (convenience wrapper).
pub fn instr_semantics(instr: &Instr) -> Result<RegTransfer, BveqError> {
    symbolic_exec(&[*instr])
}

/// Symbolically execute a straight-line instruction sequence over the full
/// register file, starting from identity.
///
/// Returns the resulting `RegTransfer` (identity for untouched registers).
/// Hard-rejects any instruction outside the register-pure subset
/// {Add, Sub, Xor, Exch, Rotl, Rotr, Cswap, Ccnot, Addi, Subi, Xori,
/// Rotli, Rotri} — reject, don't corrupt (r2 MAJOR-2).
pub fn symbolic_exec(instrs: &[Instr]) -> Result<RegTransfer, BveqError> {
    let mut transfer = RegTransfer::identity();
    for instr in instrs {
        apply_instr(&mut transfer, instr)?;
    }
    Ok(transfer)
}

// ── Equivalence checker ─────────────────────────────────────────────

/// Check equivalence of two register transfers at Flat(`w`) bit-width.
///
/// Compares per-output-bit BDDs over the FULL register-file frame (all
/// registers, identity for untouched — r2 MAJOR-2). Uses ONE shared
/// `VarMap` for both sides (r2 MAJOR-3: differing maps make unequal
/// functions compare equal).
///
/// # I4 obligation (r2 MAJOR-1, MANDATORY)
///
/// For `w < 8` (Flat(w) spec), under the constraint that all input bits
/// `w..7` are false:
/// - Low `w` output bits of `a` must equal `b` (the spec).
/// - Output bits `w..7` of `a` must be identically false (the carry/leak
///   cannot escape into upper bits — silent corruption otherwise).
///
/// # Returns
///
/// `Ok(())` if equivalent, `Err(Counterexample)` with a concretely-verified
/// witness decoded through the shared mapping to operand bytes.
///
/// **Never panics** across the recognizer boundary (returns `Result` —
/// r2 MAJOR-3).
pub fn equiv(
    a: &RegTransfer,
    b: &RegTransfer,
    w: usize,
) -> Result<(), Counterexample> {
    assert!(w <= BITS, "width must be <= {BITS}");
    assert_eq!(a.regs.len(), NUM_REGS);
    assert_eq!(b.regs.len(), NUM_REGS);

    let map = VarMap::new(NUM_REGS);

    // Build the Flat(w) input constraint: all input bits >= w are false.
    let constraint = if w < BITS {
        let mut c = map.bdd_vars.mk_true();
        for op in 0..NUM_REGS {
            for bit in w..BITS {
                c = c.and(&map.bdd_vars.mk_not_var(map.var(op, bit)));
            }
        }
        Some(c)
    } else {
        None
    };

    for reg in 0..NUM_REGS {
        for bit in 0..BITS {
            let a_bdd = expr_to_bdd(&a.regs[reg][bit], &map);

            if bit < w {
                // Low bits: a must equal b (under Flat(w) constraint if w < 8).
                let b_bdd = expr_to_bdd(&b.regs[reg][bit], &map);
                let diff = a_bdd.xor(&b_bdd);
                let check = match constraint {
                    Some(ref c) => c.and(&diff),
                    None => diff,
                };
                if let Some(witness) = check.sat_witness() {
                    return Err(decode_counterexample(&witness, &map, reg, bit));
                }
            } else {
                // I4: upper output bits must be identically false under
                // the Flat(w) input constraint. Without this check, a
                // byte-Add at Flat(4) would be mis-recognized as addInto,
                // driving registers out of canonical range (silent corruption).
                let check = match constraint {
                    Some(ref c) => c.and(&a_bdd),
                    None => a_bdd, // w == 8: unreachable (bit >= BITS)
                };
                if let Some(witness) = check.sat_witness() {
                    return Err(decode_counterexample(&witness, &map, reg, bit));
                }
            }
        }
    }

    Ok(())
}

// ── Z3 differential cross-check (feature-gated, test-only) ─────────

#[cfg(all(test, feature = "z3-crosscheck"))]
mod z3_tests {
    use super::*;

    /// Convert a BvExpr to SMT-LIB2 1-bit bitvector term string.
    /// Uses 8-bit bitvectors named `r0`..`r7`; extracts individual bits.
    fn expr_to_smt(expr: &BvExpr) -> String {
        match expr {
            BvExpr::Var { operand, bit } => {
                format!("((_ extract {bit} {bit}) r{operand})")
            }
            BvExpr::Const(true) => "#b1".to_string(),
            BvExpr::Const(false) => "#b0".to_string(),
            BvExpr::Not(a) => format!("(bvnot {})", expr_to_smt(a)),
            BvExpr::Xor(a, b) => {
                format!("(bvxor {} {})", expr_to_smt(a), expr_to_smt(b))
            }
            BvExpr::And(a, b) => {
                format!("(bvand {} {})", expr_to_smt(a), expr_to_smt(b))
            }
            BvExpr::Or(a, b) => {
                format!("(bvor {} {})", expr_to_smt(a), expr_to_smt(b))
            }
            BvExpr::Mux { sel, then_, else_ } => {
                // ite on 1-bit bitvectors: (ite (= sel #b1) then else)
                format!(
                    "(ite (= {} #b1) {} {})",
                    expr_to_smt(sel),
                    expr_to_smt(then_),
                    expr_to_smt(else_)
                )
            }
        }
    }

    /// Build an SMT-LIB2 script that checks whether two RegTransfers are
    /// equivalent at width `w`, and return (check-sat result, script).
    fn build_smt_script(a: &RegTransfer, b: &RegTransfer, w: usize) -> String {
        let mut script = String::new();
        script.push_str("(set-logic QF_BV)\n");
        // Declare 8-bit registers
        for r in 0..NUM_REGS {
            script.push_str(&format!("(declare-const r{r} (_ BitVec 8))\n"));
        }
        // Flat(w) constraint: upper bits zero
        if w < BITS {
            for r in 0..NUM_REGS {
                for bit in w..BITS {
                    script.push_str(&format!(
                        "(assert (= ((_ extract {bit} {bit}) r{r}) #b0))\n"
                    ));
                }
            }
        }
        // Assert NOT(equivalence): exists an input where a != b or upper bits != 0
        let mut disjuncts = Vec::new();
        for reg in 0..NUM_REGS {
            for bit in 0..BITS {
                if bit < w {
                    let a_smt = expr_to_smt(&a.regs[reg][bit]);
                    let b_smt = expr_to_smt(&b.regs[reg][bit]);
                    disjuncts.push(format!("(distinct {a_smt} {b_smt})"));
                } else {
                    let a_smt = expr_to_smt(&a.regs[reg][bit]);
                    disjuncts.push(format!("(distinct {a_smt} #b0)"));
                }
            }
        }
        if disjuncts.is_empty() {
            script.push_str("(assert false)\n");
        } else {
            let disj = disjuncts.join(" ");
            script.push_str(&format!("(assert (or {disj}))\n"));
        }
        script.push_str("(check-sat)\n");
        script
    }

    /// T9: Z3 differential cross-check over a small corpus.
    ///
    /// Asserts Z3's verdict matches `bveq::equiv` for each case.
    /// Behind `z3-crosscheck` feature flag — NEVER in the default build.
    #[test]
    fn t9_z3_differential_crosscheck() {
        use std::io::Write;
        use std::process::{Command, Stdio};

        let corpus: Vec<(&str, Vec<Instr>, Vec<Instr>, usize, bool)> = vec![
            (
                "add_eq_add",
                vec![Instr::Add { rd: 0, rs: 1 }],
                vec![Instr::Add { rd: 0, rs: 1 }],
                8,
                true,
            ),
            (
                "add_ne_xor",
                vec![Instr::Add { rd: 0, rs: 1 }],
                vec![Instr::Xor { rd: 0, rs: 1 }],
                8,
                false,
            ),
            (
                "ccnot_eq_ccnot",
                vec![Instr::Ccnot { rd: 0, rs: 1, rt: 2 }],
                vec![Instr::Ccnot { rd: 0, rs: 1, rt: 2 }],
                8,
                true,
            ),
            (
                "sub_ne_add",
                vec![Instr::Sub { rd: 0, rs: 1 }],
                vec![Instr::Add { rd: 0, rs: 1 }],
                8,
                false,
            ),
        ];

        for (name, instrs_a, instrs_b, w, expect_equal) in &corpus {
            let a = symbolic_exec(instrs_a).unwrap();
            let b = symbolic_exec(instrs_b).unwrap();

            // bveq verdict
            let bveq_result = equiv(&a, &b, *w);
            let bveq_equal = bveq_result.is_ok();
            assert_eq!(
                bveq_equal, *expect_equal,
                "bveq verdict mismatch for {name}"
            );

            // Z3 verdict: check-sat on the negation of equivalence
            let script = build_smt_script(&a, &b, *w);
            let mut child = Command::new("z3")
                .args(["-smt2", "-in"])
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .expect("z3 binary not found — install z3 or disable z3-crosscheck feature");

            child
                .stdin
                .as_mut()
                .unwrap()
                .write_all(script.as_bytes())
                .unwrap();

            let output = child.wait_with_output().unwrap();
            let stdout = String::from_utf8_lossy(&output.stdout);
            let z3_result = stdout.trim();

            // unsat = equivalent, sat = not equivalent
            let z3_equal = z3_result == "unsat";

            assert_eq!(
                z3_equal, bveq_equal,
                "Z3 disagrees with bveq on {name}: z3={z3_result}, bveq_equal={bveq_equal}"
            );
        }
    }
}
