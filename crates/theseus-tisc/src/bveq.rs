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
    ///
    /// Returns `Err` if `BITS * num_operands` overflows `u16` (NIT-1).
    pub fn new(num_operands: usize) -> Result<Self, BveqError> {
        let total = BITS.checked_mul(num_operands).ok_or(BveqError::VarMapOverflow {
            num_operands,
        })?;
        if total > u16::MAX as usize {
            return Err(BveqError::VarMapOverflow { num_operands });
        }
        let num_vars = total as u16;
        let bdd_vars = BddVariableSet::new_anonymous(num_vars);
        let variables = bdd_vars.variables();
        Ok(VarMap {
            num_operands,
            bdd_vars,
            variables,
        })
    }

    /// Look up the BDD variable for `(operand, bit)`.
    ///
    /// # Panics
    ///
    /// Panics if `operand >= num_operands` or `bit >= BITS`. Callers in the
    /// TCB path (expr_to_bdd) use `checked_var` instead to get `Result`.
    pub fn var(&self, operand: usize, bit: usize) -> BddVariable {
        assert!(
            operand < self.num_operands && bit < BITS,
            "VarMap::var out of range: operand={operand} (max {}), bit={bit} (max {})",
            self.num_operands - 1,
            BITS - 1,
        );
        let idx = bit * self.num_operands + operand;
        self.variables[idx]
    }

    /// Checked lookup: returns `Err` on out-of-range `(operand, bit)` instead
    /// of panicking. Used by `expr_to_bdd` to reject adversarial `Var` nodes
    /// (MAJOR-3: silent aliasing fix).
    fn checked_var(&self, operand: usize, bit: usize) -> Result<BddVariable, BveqError> {
        if operand >= self.num_operands || bit >= BITS {
            return Err(BveqError::VarOutOfRange {
                operand,
                bit,
                num_operands: self.num_operands,
            });
        }
        let idx = bit * self.num_operands + operand;
        Ok(self.variables[idx])
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
///
/// Returns `Err(VarOutOfRange)` if any `Var` node references an out-of-range
/// `(operand, bit)` — no silent aliasing, no panic (MAJOR-3).
pub fn expr_to_bdd(expr: &BvExpr, map: &VarMap) -> Result<Bdd, BveqError> {
    match expr {
        BvExpr::Var { operand, bit } => {
            let v = map.checked_var(*operand, *bit)?;
            Ok(map.bdd_vars.mk_var(v))
        }
        BvExpr::Const(true) => Ok(map.bdd_vars.mk_true()),
        BvExpr::Const(false) => Ok(map.bdd_vars.mk_false()),
        BvExpr::Not(a) => Ok(expr_to_bdd(a, map)?.not()),
        BvExpr::Xor(a, b) => Ok(expr_to_bdd(a, map)?.xor(&expr_to_bdd(b, map)?)),
        BvExpr::And(a, b) => Ok(expr_to_bdd(a, map)?.and(&expr_to_bdd(b, map)?)),
        BvExpr::Or(a, b) => Ok(expr_to_bdd(a, map)?.or(&expr_to_bdd(b, map)?)),
        BvExpr::Mux { sel, then_, else_ } => {
            let s = expr_to_bdd(sel, map)?;
            let t = expr_to_bdd(then_, map)?;
            let e = expr_to_bdd(else_, map)?;
            // mux(s, t, e) = (s ∧ t) ∨ (¬s ∧ e)
            Ok(s.and(&t).or(&s.not().and(&e)))
        }
    }
}

/// Encode a `BvExpr` to BDD with operand indices shifted by
/// `operand_shift` (mod `num_operands`). Used by the MAJOR-4 canary test to
/// prove the shared-mapping discipline is load-bearing: under a shifted
/// mapping, genuinely-equal transfers produce different BDDs.
#[doc(hidden)]
pub fn expr_to_bdd_shifted(
    expr: &BvExpr,
    map: &VarMap,
    operand_shift: usize,
) -> Result<Bdd, BveqError> {
    match expr {
        BvExpr::Var { operand, bit } => {
            let shifted = (*operand + operand_shift) % map.num_operands();
            let v = map.checked_var(shifted, *bit)?;
            Ok(map.bdd_vars.mk_var(v))
        }
        BvExpr::Const(true) => Ok(map.bdd_vars.mk_true()),
        BvExpr::Const(false) => Ok(map.bdd_vars.mk_false()),
        BvExpr::Not(a) => Ok(expr_to_bdd_shifted(a, map, operand_shift)?.not()),
        BvExpr::Xor(a, b) => Ok(expr_to_bdd_shifted(a, map, operand_shift)?
            .xor(&expr_to_bdd_shifted(b, map, operand_shift)?)),
        BvExpr::And(a, b) => Ok(expr_to_bdd_shifted(a, map, operand_shift)?
            .and(&expr_to_bdd_shifted(b, map, operand_shift)?)),
        BvExpr::Or(a, b) => Ok(expr_to_bdd_shifted(a, map, operand_shift)?
            .or(&expr_to_bdd_shifted(b, map, operand_shift)?)),
        BvExpr::Mux { sel, then_, else_ } => {
            let s = expr_to_bdd_shifted(sel, map, operand_shift)?;
            let t = expr_to_bdd_shifted(then_, map, operand_shift)?;
            let e = expr_to_bdd_shifted(else_, map, operand_shift)?;
            Ok(s.and(&t).or(&s.not().and(&e)))
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
    for (r, out_reg) in result.iter_mut().enumerate() {
        for bit in 0..BITS {
            if eval_expr(&transfer.regs[r][bit], regs) {
                *out_reg |= 1 << bit;
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

// NIT-2: tie barrel-network stage count to BITS.
const _: () = assert!(BITS == 8, "barrel rotate take(3) assumes BITS == 8");

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
    // 3 stages for 8-bit; tied to BITS by const assert above.
    for (stage, sel_src) in amount_bits.iter().enumerate().take(3) {
        let shift = 1usize << stage;
        let sel = sel_src.clone();
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
    for (stage, sel_src) in amount_bits.iter().enumerate().take(3) {
        let shift = 1usize << stage;
        let sel = sel_src.clone();
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
    /// Register index out of range (rd/rs/rt ≥ NUM_REGS) in an instruction.
    /// MAJOR-3: apply_instr must reject, not panic.
    RegisterOutOfRange { instr: Instr, which: &'static str, val: u8 },
    /// Two-operand instruction has rd == rs — the ISA rejects these
    /// (non-injective datapath) and the hardware deviates. MAJOR-5.
    AliasedOperands { instr: Instr },
    /// Three-operand instruction has non-distinct operands — the ISA rejects
    /// these (Cswap/Ccnot pairwise-distinct rule). MAJOR-5.
    NonDistinctOperands { instr: Instr },
    /// BvExpr Var node references an out-of-range (operand, bit).
    /// MAJOR-3: silent aliasing fix — Var(20,0) no longer maps onto Var(4,2).
    VarOutOfRange { operand: usize, bit: usize, num_operands: usize },
    /// Width parameter out of range (w > BITS or w == 0).
    WidthOutOfRange { w: usize },
    /// RegTransfer frame size mismatch (must be NUM_REGS).
    FrameSizeMismatch { side: &'static str, len: usize },
    /// VarMap::new overflow: BITS * num_operands exceeds u16.
    VarMapOverflow { num_operands: usize },
    /// The two transfers are not equivalent; includes a concrete counterexample.
    NotEquivalent(Counterexample),
}

impl BveqError {
    /// Extract the counterexample if this is a `NotEquivalent` error.
    pub fn unwrap_counterexample(self) -> Counterexample {
        match self {
            BveqError::NotEquivalent(cex) => cex,
            other => panic!("expected NotEquivalent, got: {other}"),
        }
    }
}

impl std::fmt::Display for BveqError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BveqError::NonRegisterPure(instr) => {
                write!(f, "non-register-pure instruction rejected: {instr:?}")
            }
            BveqError::RegisterOutOfRange { instr, which, val } => {
                write!(f, "register index out of range in {instr:?}: {which}={val} (max {})", NUM_REGS - 1)
            }
            BveqError::AliasedOperands { instr } => {
                write!(f, "aliased operands rejected (ISA-illegal): {instr:?}")
            }
            BveqError::NonDistinctOperands { instr } => {
                write!(f, "non-distinct operands rejected (ISA-illegal): {instr:?}")
            }
            BveqError::VarOutOfRange { operand, bit, num_operands } => {
                write!(f, "BvExpr Var out of range: operand={operand} (max {num_operands}), bit={bit} (max {})", BITS)
            }
            BveqError::WidthOutOfRange { w } => {
                write!(f, "width out of range: w={w} (must be 1..={BITS})")
            }
            BveqError::FrameSizeMismatch { side, len } => {
                write!(f, "RegTransfer frame size mismatch on {side}: len={len} (expected {NUM_REGS})")
            }
            BveqError::VarMapOverflow { num_operands } => {
                write!(f, "VarMap overflow: BITS({BITS}) * num_operands({num_operands}) exceeds u16")
            }
            BveqError::NotEquivalent(cex) => {
                write!(f, "transfers not equivalent: {cex}")
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
    let n_op = map.num_operands().min(NUM_REGS);
    for (op, ov) in operand_values.iter_mut().enumerate().take(n_op) {
        for b in 0..BITS {
            // Safe: op < n_op <= num_operands, b < BITS.
            if witness.value(map.var(op, b)) {
                *ov |= 1 << b;
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

/// Check that a register index is in range (< NUM_REGS).
fn check_reg(instr: &Instr, which: &'static str, val: u8) -> Result<(), BveqError> {
    if (val as usize) >= NUM_REGS {
        return Err(BveqError::RegisterOutOfRange {
            instr: *instr,
            which,
            val,
        });
    }
    Ok(())
}

/// Check that a 2-operand instruction has distinct rd and rs (MAJOR-5).
/// Mirrors `asm::encode`'s `check_distinct`.
fn check_two_op_distinct(instr: &Instr, rd: u8, rs: u8) -> Result<(), BveqError> {
    if rd == rs {
        return Err(BveqError::AliasedOperands { instr: *instr });
    }
    Ok(())
}

/// Check that a 3-operand instruction has pairwise distinct operands (MAJOR-5).
/// Mirrors `asm::encode`'s Cswap/Ccnot distinctness check.
fn check_three_op_distinct(instr: &Instr, rd: u8, rs: u8, rt: u8) -> Result<(), BveqError> {
    if rd == rs || rd == rt || rs == rt {
        return Err(BveqError::NonDistinctOperands { instr: *instr });
    }
    Ok(())
}

/// Apply one instruction's register-transfer semantics to a `RegTransfer`.
///
/// Returns `Err(NonRegisterPure)` for instructions that move state off the
/// register file (Swapbr, Xchg, Mexch, MexchReg) — reject, don't corrupt.
///
/// Returns `Err(RegisterOutOfRange)` if any register index ≥ NUM_REGS (MAJOR-3).
///
/// Returns `Err(AliasedOperands)` / `Err(NonDistinctOperands)` for instructions
/// the ISA rejects due to aliasing (MAJOR-5). Mirrors `asm::encode`.
fn apply_instr(transfer: &mut RegTransfer, instr: &Instr) -> Result<(), BveqError> {
    match *instr {
        Instr::Add { rd, rs } => {
            check_reg(instr, "rd", rd)?;
            check_reg(instr, "rs", rs)?;
            check_two_op_distinct(instr, rd, rs)?;
            let rd = rd as usize;
            let rs = rs as usize;
            let result = bv_add(&transfer.regs[rd], &transfer.regs[rs]);
            transfer.regs[rd] = result;
        }
        Instr::Sub { rd, rs } => {
            check_reg(instr, "rd", rd)?;
            check_reg(instr, "rs", rs)?;
            check_two_op_distinct(instr, rd, rs)?;
            let rd = rd as usize;
            let rs = rs as usize;
            let result = bv_sub(&transfer.regs[rd], &transfer.regs[rs]);
            transfer.regs[rd] = result;
        }
        Instr::Xor { rd, rs } => {
            check_reg(instr, "rd", rd)?;
            check_reg(instr, "rs", rs)?;
            check_two_op_distinct(instr, rd, rs)?;
            let rd = rd as usize;
            let rs = rs as usize;
            let result = bv_xor(&transfer.regs[rd], &transfer.regs[rs]);
            transfer.regs[rd] = result;
        }
        Instr::Exch { rd, rs } => {
            check_reg(instr, "rd", rd)?;
            check_reg(instr, "rs", rs)?;
            check_two_op_distinct(instr, rd, rs)?;
            let rd = rd as usize;
            let rs = rs as usize;
            let tmp = transfer.regs[rd].clone();
            transfer.regs[rd] = transfer.regs[rs].clone();
            transfer.regs[rs] = tmp;
        }
        Instr::Rotl { rd, rs } => {
            check_reg(instr, "rd", rd)?;
            check_reg(instr, "rs", rs)?;
            check_two_op_distinct(instr, rd, rs)?;
            let rd = rd as usize;
            let rs = rs as usize;
            let amount_bits = transfer.regs[rs].clone();
            let result = bv_rotl_var(&transfer.regs[rd], &amount_bits);
            transfer.regs[rd] = result;
        }
        Instr::Rotr { rd, rs } => {
            check_reg(instr, "rd", rd)?;
            check_reg(instr, "rs", rs)?;
            check_two_op_distinct(instr, rd, rs)?;
            let rd = rd as usize;
            let rs = rs as usize;
            let amount_bits = transfer.regs[rs].clone();
            let result = bv_rotr_var(&transfer.regs[rd], &amount_bits);
            transfer.regs[rd] = result;
        }
        Instr::Cswap { rd, rs, rt } => {
            check_reg(instr, "rd", rd)?;
            check_reg(instr, "rs", rs)?;
            check_reg(instr, "rt", rt)?;
            check_three_op_distinct(instr, rd, rs, rt)?;
            let rd = rd as usize;
            let rs = rs as usize;
            let rt = rt as usize;
            let sel = transfer.regs[rd][0].clone();
            let old_rs = transfer.regs[rs].clone();
            let old_rt = transfer.regs[rt].clone();
            transfer.regs[rs] = (0..BITS)
                .map(|i| BvExpr::mux(sel.clone(), old_rt[i].clone(), old_rs[i].clone()))
                .collect();
            transfer.regs[rt] = (0..BITS)
                .map(|i| BvExpr::mux(sel.clone(), old_rs[i].clone(), old_rt[i].clone()))
                .collect();
        }
        Instr::Ccnot { rd, rs, rt } => {
            check_reg(instr, "rd", rd)?;
            check_reg(instr, "rs", rs)?;
            check_reg(instr, "rt", rt)?;
            check_three_op_distinct(instr, rd, rs, rt)?;
            let rd = rd as usize;
            let rs = rs as usize;
            let rt = rt as usize;
            let rs_and_rt = bv_and(&transfer.regs[rs], &transfer.regs[rt]);
            let result = bv_xor(&transfer.regs[rd], &rs_and_rt);
            transfer.regs[rd] = result;
        }
        Instr::Addi { rd, imm } => {
            check_reg(instr, "rd", rd)?;
            let rd = rd as usize;
            let ib = imm_bits(imm);
            let result = bv_add(&transfer.regs[rd], &ib);
            transfer.regs[rd] = result;
        }
        Instr::Subi { rd, imm } => {
            check_reg(instr, "rd", rd)?;
            let rd = rd as usize;
            let ib = imm_bits(imm);
            let result = bv_sub(&transfer.regs[rd], &ib);
            transfer.regs[rd] = result;
        }
        Instr::Xori { rd, imm } => {
            check_reg(instr, "rd", rd)?;
            let rd = rd as usize;
            let ib = imm_bits(imm);
            let result = bv_xor(&transfer.regs[rd], &ib);
            transfer.regs[rd] = result;
        }
        Instr::Rotli { rd, imm } => {
            check_reg(instr, "rd", rd)?;
            let rd = rd as usize;
            let amount = (imm & 7) as usize;
            let result = bv_rotl_const(&transfer.regs[rd], amount);
            transfer.regs[rd] = result;
        }
        Instr::Rotri { rd, imm } => {
            check_reg(instr, "rd", rd)?;
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
///
/// Also rejects instructions with out-of-range register indices (MAJOR-3)
/// and ISA-illegal aliased operands (MAJOR-5).
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
/// # `operand_regs` (MAJOR-2)
///
/// The caller-supplied set of registers that are constrained to Flat(`w`).
/// Only these registers have their input bits `w..7` forced false and their
/// output bits `w..7` checked under the I4 obligation. All other registers'
/// bits are fully free (Flat(8)), preventing a global constraint from
/// certifying a corrupting sequence as identity.
///
/// # I4 obligation (r2 MAJOR-1, MANDATORY)
///
/// For `w < 8` (Flat(w) spec), under the constraint that operand registers'
/// input bits `w..7` are false:
/// - Low `w` output bits of `a` must equal `b` (the spec) for all registers.
/// - Output bits `w..7` of BOTH `a` AND `b` must be identically false for
///   operand registers (MAJOR-1: applied to BOTH sides, not just `a`).
///
/// # Returns
///
/// `Ok(())` if equivalent, `Err(NotEquivalent(Counterexample))` with a
/// concretely-verified witness decoded through the shared mapping.
/// `Err(...)` for validation failures (out-of-range, etc.).
///
/// **Never panics** across the recognizer boundary (returns `Result` —
/// r2 MAJOR-3).
pub fn equiv(
    a: &RegTransfer,
    b: &RegTransfer,
    w: usize,
    operand_regs: &[usize],
) -> Result<(), BveqError> {
    // MAJOR-3: convert asserts to Err.
    if w == 0 || w > BITS {
        return Err(BveqError::WidthOutOfRange { w });
    }
    if a.regs.len() != NUM_REGS {
        return Err(BveqError::FrameSizeMismatch {
            side: "a",
            len: a.regs.len(),
        });
    }
    if b.regs.len() != NUM_REGS {
        return Err(BveqError::FrameSizeMismatch {
            side: "b",
            len: b.regs.len(),
        });
    }

    let map = VarMap::new(NUM_REGS)?;

    // MAJOR-2: Build the Flat(w) input constraint ONLY for operand_regs.
    // All other registers' bits are fully free (Flat(8)).
    let constraint = if w < BITS {
        let mut c = map.bdd_vars.mk_true();
        for &op in operand_regs {
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
            let is_operand_reg = operand_regs.contains(&reg);

            if bit < w || !is_operand_reg {
                // Low bits of all registers, OR any bit of non-operand registers:
                // a must equal b (under Flat(w) constraint if w < 8).
                let a_bdd = expr_to_bdd(&a.regs[reg][bit], &map)?;
                let b_bdd = expr_to_bdd(&b.regs[reg][bit], &map)?;
                let diff = a_bdd.xor(&b_bdd);
                let check = match constraint {
                    Some(ref c) => c.and(&diff),
                    None => diff,
                };
                if let Some(witness) = check.sat_witness() {
                    return Err(BveqError::NotEquivalent(decode_counterexample(
                        &witness, &map, reg, bit,
                    )));
                }
            } else {
                // I4: upper output bits of operand registers must be
                // identically false under the Flat(w) input constraint.
                // MAJOR-1: check BOTH a AND b (not just a).
                let a_bdd = expr_to_bdd(&a.regs[reg][bit], &map)?;
                let b_bdd = expr_to_bdd(&b.regs[reg][bit], &map)?;
                for side_bdd in [&a_bdd, &b_bdd] {
                    let check = match constraint {
                        Some(ref c) => c.and(side_bdd),
                        None => side_bdd.clone(),
                    };
                    if let Some(witness) = check.sat_witness() {
                        return Err(BveqError::NotEquivalent(decode_counterexample(
                            &witness, &map, reg, bit,
                        )));
                    }
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
    /// equivalent at width `w` with the given operand registers, and return
    /// the script string.
    ///
    /// Updated for MAJOR-1 (I4 both sides) and MAJOR-2 (per-operand constraint).
    fn build_smt_script(
        a: &RegTransfer,
        b: &RegTransfer,
        w: usize,
        operand_regs: &[usize],
    ) -> String {
        let mut script = String::new();
        script.push_str("(set-logic QF_BV)\n");
        for r in 0..NUM_REGS {
            script.push_str(&format!("(declare-const r{r} (_ BitVec 8))\n"));
        }
        // MAJOR-2: Flat(w) constraint only on operand_regs
        if w < BITS {
            for &r in operand_regs {
                for bit in w..BITS {
                    script.push_str(&format!(
                        "(assert (= ((_ extract {bit} {bit}) r{r}) #b0))\n"
                    ));
                }
            }
        }
        // Assert NOT(equivalence)
        let mut disjuncts = Vec::new();
        for reg in 0..NUM_REGS {
            let is_operand = operand_regs.contains(&reg);
            for bit in 0..BITS {
                if bit < w || !is_operand {
                    // a must equal b
                    let a_smt = expr_to_smt(&a.regs[reg][bit]);
                    let b_smt = expr_to_smt(&b.regs[reg][bit]);
                    disjuncts.push(format!("(distinct {a_smt} {b_smt})"));
                } else {
                    // MAJOR-1: I4 on BOTH sides for operand regs
                    let a_smt = expr_to_smt(&a.regs[reg][bit]);
                    let b_smt = expr_to_smt(&b.regs[reg][bit]);
                    disjuncts.push(format!("(distinct {a_smt} #b0)"));
                    disjuncts.push(format!("(distinct {b_smt} #b0)"));
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

        // (name, instrs_a, instrs_b, w, operand_regs, expect_equal)
        let corpus: Vec<(&str, Vec<Instr>, Vec<Instr>, usize, Vec<usize>, bool)> = vec![
            (
                "add_eq_add",
                vec![Instr::Add { rd: 0, rs: 1 }],
                vec![Instr::Add { rd: 0, rs: 1 }],
                8,
                vec![0, 1],
                true,
            ),
            (
                "add_ne_xor",
                vec![Instr::Add { rd: 0, rs: 1 }],
                vec![Instr::Xor { rd: 0, rs: 1 }],
                8,
                vec![0, 1],
                false,
            ),
            (
                "ccnot_eq_ccnot",
                vec![Instr::Ccnot { rd: 0, rs: 1, rt: 2 }],
                vec![Instr::Ccnot { rd: 0, rs: 1, rt: 2 }],
                8,
                vec![0, 1, 2],
                true,
            ),
            (
                "sub_ne_add",
                vec![Instr::Sub { rd: 0, rs: 1 }],
                vec![Instr::Add { rd: 0, rs: 1 }],
                8,
                vec![0, 1],
                false,
            ),
            // MINOR-4: w<8 / I4 case — byte-Add rejected against Flat(4) addInto spec
            // This exercises the Flat(w) constraint and upper-bit SMT path.
            (
                "add_ne_flat4_add",
                vec![Instr::Add { rd: 0, rs: 1 }],
                // spec side: we'll build this below since it needs hand-construction
                vec![Instr::Add { rd: 0, rs: 1 }],
                4,
                vec![0, 1],
                // byte-Add at Flat(4) has carry in upper bits, so it's not self-equal
                // under I4 — this is the exact T3 scenario.
                false,
            ),
        ];

        for (name, instrs_a, instrs_b, w, operand_regs, expect_equal) in &corpus {
            let a = symbolic_exec(instrs_a).unwrap();
            let b = symbolic_exec(instrs_b).unwrap();

            // bveq verdict
            let bveq_result = equiv(&a, &b, *w, operand_regs);
            let bveq_equal = bveq_result.is_ok();
            assert_eq!(
                bveq_equal, *expect_equal,
                "bveq verdict mismatch for {name}"
            );

            // Z3 verdict
            let script = build_smt_script(&a, &b, *w, operand_regs);
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

            let z3_equal = z3_result == "unsat";

            assert_eq!(
                z3_equal, bveq_equal,
                "Z3 disagrees with bveq on {name}: z3={z3_result}, bveq_equal={bveq_equal}"
            );
        }
    }
}
