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
//!   equivalence
//!
//! Defended by: tri-model reconciliation (symbolic ≡ exec_instr ≡ netlist),
//! Z3 differential cross-check (feature-gated), and the I4 upper-output-bits
//! obligation.

use crate::asm::Instr;
use crate::types::NUM_REGS;
use biodivine_lib_bdd::{Bdd, BddVariable, BddVariableSet};

/// Re-export for tests that need direct BDD access (e.g. T4 mis-map negative).
pub use biodivine_lib_bdd;

/// Bits per register (byte-wide ISA).
pub const BITS: usize = 8;

// ── BvExpr AST (canonical home: theseus-core/src/bvexpr.rs) ─────────

pub use theseus_core::bvexpr::{eval_expr, BvExpr};

// ── RegTransfer ─────────────────────────────────────────────────────

/// Transfer function for the full register file (NUM_REGS × BITS).
///
/// `regs[r][b]` is the BvExpr for bit `b` of register `r` after the transfer.
/// Identity for untouched registers. Full-frame.
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

// ── VarMap (shared variable mapping -- TCB) ──────────────

/// Pinned variable mapping: `(operand, bit) → BddVariable`.
///
/// Interleaved ordering: `var_index = bit * num_operands + operand`.
/// This groups all operands' bit-0, then all bit-1, etc. -- keeps
/// ripple-carry ADD/SUB linear and rotates trivial.
///
/// **This mapping is TCB.** One instance must be shared by BOTH sides of
/// every equivalence check. Using different mappings for the two sides
/// makes unequal functions compare equal.
pub struct VarMap {
    num_operands: usize,
    bdd_vars: BddVariableSet,
    variables: Vec<BddVariable>,
}

impl VarMap {
    /// Create a new variable map for `num_operands` registers, each `BITS` wide.
    ///
    /// Returns `Err` if `BITS * num_operands` overflows `u16`.
    pub fn new(num_operands: usize) -> Result<Self, BveqError> {
        let total = BITS
            .checked_mul(num_operands)
            .ok_or(BveqError::VarMapOverflow { num_operands })?;
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
    /// (silent aliasing fix).
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
/// `(operand, bit)` -- no silent aliasing, no panic.
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
/// `operand_shift` (mod `num_operands`). Used by the mismap canary test to
/// prove the shared-mapping discipline is load-bearing: under a shifted
/// mapping, genuinely-equal transfers produce different BDDs.
///
/// MEASUREMENT-ONLY: gated behind the `measurement` feature; it exists solely
/// for the canary test and is not part of the default public API.
#[cfg(feature = "measurement")]
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

// ── Concrete evaluator (re-exported from theseus-core::bvexpr) ──────

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

// tie barrel-network stage count to BITS.
const _: () = assert!(BITS == 8, "barrel rotate take(3) assumes BITS == 8");

/// Constant left rotation by `amount` positions (wire permutation).
fn bv_rotl_const(bits: &[BvExpr], amount: usize) -> Vec<BvExpr> {
    let n = bits.len();
    let amount = amount % n;
    // rotl: bit i of output = bit (i - amount) mod n of input
    (0..n).map(|i| bits[(i + n - amount) % n].clone()).collect()
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
            .map(|i| BvExpr::mux(sel.clone(), prev[(i + shift) % n].clone(), prev[i].clone()))
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
#[derive(Debug, thiserror::Error)]
pub enum BveqError {
    /// Instruction is not register-pure (Swapbr, Xchg, Mexch, MexchReg).
    /// These move state off the register file -- reject, don't corrupt.
    #[error("non-register-pure instruction rejected: {0:?}")]
    NonRegisterPure(Instr),
    /// Register index out of range (rd/rs/rt >= NUM_REGS) in an instruction.
    /// apply_instr must reject, not panic.
    #[error("register index out of range in {instr:?}: {which}={val} (max {})", NUM_REGS - 1)]
    RegisterOutOfRange {
        instr: Instr,
        which: &'static str,
        val: u8,
    },
    /// Two-operand instruction has rd == rs -- the ISA rejects these
    /// (non-injective datapath) and the hardware deviates.
    #[error("aliased operands rejected (ISA-illegal): {instr:?}")]
    AliasedOperands { instr: Instr },
    /// Three-operand instruction has non-distinct operands -- the ISA rejects
    /// these (Cswap/Ccnot pairwise-distinct rule).
    #[error("non-distinct operands rejected (ISA-illegal): {instr:?}")]
    NonDistinctOperands { instr: Instr },
    /// BvExpr Var node references an out-of-range (operand, bit).
    /// Guards against silent aliasing -- Var(20,0) no longer maps onto Var(4,2).
    #[error(
        "BvExpr Var out of range: operand={operand} (max {num_operands}), bit={bit} (max {})",
        BITS
    )]
    VarOutOfRange {
        operand: usize,
        bit: usize,
        num_operands: usize,
    },
    /// Width parameter out of range (w > BITS or w == 0).
    #[error("width out of range: w={w} (must be 1..={BITS})")]
    WidthOutOfRange { w: usize },
    /// RegTransfer frame size mismatch (must be NUM_REGS).
    #[error("RegTransfer frame size mismatch on {side}: len={len} (expected {NUM_REGS})")]
    FrameSizeMismatch { side: &'static str, len: usize },
    /// A register bit-row has the wrong length (must be BITS). A short row
    /// must return Err, not panic on index.
    #[error("RegTransfer bit-row size mismatch on {side} reg {reg}: len={len} (expected {BITS})")]
    RowSizeMismatch {
        side: &'static str,
        reg: usize,
        len: usize,
    },
    /// An `operand_regs` entry is out of range (>= NUM_REGS). The
    /// operand-register parameter must be validated, not fed to VarMap::var (which panics).
    #[error("operand_regs entry out of range: {reg} (max {})", NUM_REGS - 1)]
    OperandRegOutOfRange { reg: usize },
    /// VarMap::new overflow: BITS * num_operands exceeds u16.
    #[error("VarMap overflow: BITS({BITS}) * num_operands({num_operands}) exceeds u16")]
    VarMapOverflow { num_operands: usize },
    /// The two transfers are not equivalent; includes a concrete counterexample.
    #[error("transfers not equivalent: {0}")]
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

// ── Instruction semantics (single symbolic model) ───────────

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

/// Check that a 2-operand instruction has distinct rd and rs.
/// Mirrors `asm::encode`'s `check_distinct`.
fn check_two_op_distinct(instr: &Instr, rd: u8, rs: u8) -> Result<(), BveqError> {
    if rd == rs {
        return Err(BveqError::AliasedOperands { instr: *instr });
    }
    Ok(())
}

/// Check that a 3-operand instruction has pairwise distinct operands.
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
/// register file (Swapbr, Xchg, Mexch, MexchReg) -- reject, don't corrupt.
///
/// Returns `Err(RegisterOutOfRange)` if any register index ≥ NUM_REGS.
///
/// Returns `Err(AliasedOperands)` / `Err(NonDistinctOperands)` for instructions
/// the ISA rejects due to aliasing. Mirrors `asm::encode`.
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
        // Non-register-pure: reject, don't corrupt.
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
/// Rotli, Rotri} -- reject, don't corrupt.
///
/// Also rejects instructions with out-of-range register indices
/// and ISA-illegal aliased operands.
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
/// registers, identity for untouched). Uses ONE shared
/// `VarMap` for both sides (differing maps make unequal
/// functions compare equal).
///
/// # `operand_regs`
///
/// The caller-supplied set of registers that hold Flat(`w`) values. Only these
/// registers have their input bits `w..7` forced false and their output bits
/// `w..7` checked under the I4 obligation. All other registers' bits are fully
/// free (Flat(8)), preventing a global constraint from certifying a corrupting
/// sequence as identity.
///
/// **CALLER OBLIGATION:** `operand_regs` must be derived from the prim's
/// TYPE, never from the candidate instruction sequence. `equiv` trusts it and
/// cannot validate it:
/// - **Over-declaring** a register that is not actually Flat(`w`) forces its
///   upper bits to zero and thereby HIDES any difference conditioned on them --
///   returning `Ok` on a corrupting sequence (unsound *by declaration*). See
///   `bveq_major2_overdeclaration_hides_corruption`.
/// - **Empty `operand_regs`** at `w < 8` makes the constraint vacuously true and
///   degrades `equiv` to full unconstrained Flat(8) equality (ignoring `w`) --
///   strictly stricter (fail-safe), but surprising.
///
/// # I4 obligation (MANDATORY)
///
/// For `w < 8` (Flat(w) spec), **under the Flat(w) input constraint** on the
/// operand registers (invariant preservation: Flat(w) in ⇒ Flat(w) out):
/// - Low `w` output bits of `a` must equal `b` (the spec) for all registers.
/// - Output bits `w..7` of BOTH `a` AND `b` must be false *under the input
///   constraint* for operand registers (BOTH sides; UNDER the
///   constraint, not unconditionally -- an unconditional check rejects every
///   reversible model, since a bijection's output bit is never identically
///   false).
///
/// # Returns
///
/// `Ok(())` if equivalent, `Err(NotEquivalent(Counterexample))` with a
/// concretely-verified witness decoded through the shared mapping.
/// `Err(...)` for validation failures (out-of-range, etc.).
///
/// **Never panics** across the recognizer boundary on any register/width/frame
/// input (returns `Result`). The ONE remaining abort is a stack
/// overflow from a pathologically deep `BvExpr` (unbounded recursion in
/// `expr_to_bdd`/`eval_expr`); this is unreachable from `symbolic_exec`, which
/// only builds straight-line-bounded expressions.
pub fn equiv(
    a: &RegTransfer,
    b: &RegTransfer,
    w: usize,
    operand_regs: &[usize],
) -> Result<(), BveqError> {
    // convert asserts to Err.
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
    // validate every bit-row length, else a short row panics on index
    // below (a slice-2 spec-builder emitting only `w` bits is the obvious bug).
    for (side, t) in [("a", a), ("b", b)] {
        for (reg, row) in t.regs.iter().enumerate() {
            if row.len() != BITS {
                return Err(BveqError::RowSizeMismatch {
                    side,
                    reg,
                    len: row.len(),
                });
            }
        }
    }
    // validate operand_regs entries (the operand_regs parameter), else the
    // constraint builder feeds an OOB index to VarMap::var → panic (which would
    // itself violate the never-panic contract).
    for &op in operand_regs {
        if op >= NUM_REGS {
            return Err(BveqError::OperandRegOutOfRange { reg: op });
        }
    }

    let map = VarMap::new(NUM_REGS)?;

    // Build the Flat(w) input constraint ONLY for operand_regs.
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
                // I4: upper output bits w..7 of operand registers must
                // be identically false UNDER the Flat(w) input constraint -- i.e.
                // Flat(w) inputs produce Flat(w) outputs (invariant preservation,
                // NOT circular). Pass-through upper bits (`Var(op, b≥w)`) are
                // legitimately false once the constraint zeroes those inputs, so
                // identity/reversible models are ACCEPTED; a real width-escape
                // (e.g. byte-Add's carry into bit w -- a function of the LOW,
                // unconstrained bits) stays satisfiable and is REJECTED.
                // The prior UNCONDITIONAL check rejected every reversible model at
                // w<8 (identity ≡ identity included), since an output bit of a
                // bijection can never be identically false. BOTH sides.
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
    /// Updated for I4 both sides and the per-operand constraint.
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
        // Flat(w) constraint only on operand_regs
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
                    // I4 on BOTH sides for operand regs
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
    /// Behind `z3-crosscheck` feature flag -- NEVER in the default build.
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
                vec![Instr::Ccnot {
                    rd: 0,
                    rs: 1,
                    rt: 2,
                }],
                vec![Instr::Ccnot {
                    rd: 0,
                    rs: 1,
                    rt: 2,
                }],
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
            // w<8 / I4 case -- byte-Add rejected against Flat(4) addInto spec
            // This exercises the Flat(w) constraint and upper-bit SMT path.
            (
                "add_ne_flat4_add",
                vec![Instr::Add { rd: 0, rs: 1 }],
                // spec side: we'll build this below since it needs hand-construction
                vec![Instr::Add { rd: 0, rs: 1 }],
                4,
                vec![0, 1],
                // byte-Add at Flat(4) has carry in upper bits, so it's not self-equal
                // under I4 -- this is the exact T3 scenario.
                false,
            ),
            // w<8 pass-through ACCEPT case -- the case that distinguishes the
            // correct UNDER-constraint I4 (accept) from the old unconditional bug
            // (which rejected every reversible model at w<8). Exch's Flat(4)
            // upper output bits are pass-throughs of constrained-zero inputs.
            (
                "exch_eq_flat4",
                vec![Instr::Exch { rd: 0, rs: 1 }],
                vec![Instr::Exch { rd: 0, rs: 1 }],
                4,
                vec![0, 1],
                true,
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
                .expect("z3 binary not found -- install z3 or disable z3-crosscheck feature");

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

// ── Spec → RegTransfer shim (3b selection-by-equiv) ───────────────────

/// Convert a `SpecTransfer` into a full-frame `RegTransfer` given the
/// operand-to-register mapping (the leaf homes from `with_registers`).
///
/// `operand_regs[i]` is the register index for spec operand (leaf) `i`.
/// Non-operand registers get identity; operand registers' bits `0..w` get
/// the spec's expressions (remapped from leaf-space to register-space),
/// and bits `w..BITS` get `Const(false)` (Flat(w) zero-extension).
///
/// Uses the same operand→register remapping pattern as `expr_to_bdd_shifted`
/// but with an arbitrary mapping (not just a cyclic shift).
pub fn spec_to_reg_transfer(
    spec: &theseus_core::prim::SpecTransfer,
    operand_regs: &[usize],
) -> RegTransfer {
    let w = spec.width as usize;
    let mut transfer = RegTransfer::identity();

    for (leaf_idx, &reg) in operand_regs.iter().enumerate() {
        // Low bits: from the spec, with operand indices remapped to registers.
        for bit in 0..w {
            transfer.regs[reg][bit] =
                remap_expr_operands(&spec.leaves[leaf_idx][bit], operand_regs);
        }
        // High bits: zero (Flat(w) invariant preservation).
        for bit in w..BITS {
            transfer.regs[reg][bit] = BvExpr::Const(false);
        }
    }

    transfer
}

/// Remap a `BvExpr`'s operand indices from leaf-space to register-space.
/// `operand_regs[leaf_idx]` → register index.
fn remap_expr_operands(expr: &BvExpr, operand_regs: &[usize]) -> BvExpr {
    match expr {
        BvExpr::Var { operand, bit } => BvExpr::Var {
            operand: operand_regs[*operand],
            bit: *bit,
        },
        BvExpr::Const(b) => BvExpr::Const(*b),
        BvExpr::Not(a) => BvExpr::Not(Box::new(remap_expr_operands(a, operand_regs))),
        BvExpr::Xor(a, b) => BvExpr::Xor(
            Box::new(remap_expr_operands(a, operand_regs)),
            Box::new(remap_expr_operands(b, operand_regs)),
        ),
        BvExpr::And(a, b) => BvExpr::And(
            Box::new(remap_expr_operands(a, operand_regs)),
            Box::new(remap_expr_operands(b, operand_regs)),
        ),
        BvExpr::Or(a, b) => BvExpr::Or(
            Box::new(remap_expr_operands(a, operand_regs)),
            Box::new(remap_expr_operands(b, operand_regs)),
        ),
        BvExpr::Mux { sel, then_, else_ } => BvExpr::Mux {
            sel: Box::new(remap_expr_operands(sel, operand_regs)),
            then_: Box::new(remap_expr_operands(then_, operand_regs)),
            else_: Box::new(remap_expr_operands(else_, operand_regs)),
        },
    }
}

// ── Symbolic admission (BDD mutual-inverse certificate) ─────────────

/// Certify that a `PrimSpec`'s forward and backward are mutual inverses
/// via BDD composition: `backward ∘ forward == id` AND `forward ∘ backward == id`,
/// checked per output bit.
///
/// This is the symbolic bijectivity certificate for prims that have no table.
/// Core stays BDD-free; this function lives in tisc.
///
/// The leaf count (e.g. 3 for a 3-op prim) and the per-leaf bit-width are read
/// from `spec.forward` (`leaves.len()` and `width`).
pub fn admit_symbolic(spec: &theseus_core::prim::PrimSpec) -> Result<(), BveqError> {
    let width = spec.forward.width as usize;
    let num_operands = spec.forward.leaves.len();

    if width == 0 || width > BITS {
        return Err(BveqError::WidthOutOfRange { w: width });
    }

    // Validate backward shape matches forward.
    if spec.backward.width as usize != width {
        return Err(BveqError::WidthOutOfRange {
            w: spec.backward.width as usize,
        });
    }
    if spec.backward.leaves.len() != num_operands {
        return Err(BveqError::FrameSizeMismatch {
            side: "backward spec",
            len: spec.backward.leaves.len(),
        });
    }

    let map = VarMap::new(num_operands)?;

    // Check backward ∘ forward == id:
    // For each operand `op` and bit `b`:
    //   substitute forward's outputs into backward's expression and check == Var(op, b).
    check_composition_is_identity(
        &spec.forward,
        &spec.backward,
        &map,
        width,
        num_operands,
        "backward∘forward",
    )?;

    // Check forward ∘ backward == id:
    check_composition_is_identity(
        &spec.backward,
        &spec.forward,
        &map,
        width,
        num_operands,
        "forward∘backward",
    )?;

    Ok(())
}

/// Check that `second ∘ first == id` via BDD: for each output operand/bit,
/// substitute first's output expressions into second's input variables and
/// verify the result equals the original input variable.
fn check_composition_is_identity(
    first: &theseus_core::prim::SpecTransfer,
    second: &theseus_core::prim::SpecTransfer,
    map: &VarMap,
    width: usize,
    num_operands: usize,
    _direction: &str,
) -> Result<(), BveqError> {
    // Build BDDs for first's output expressions.
    let mut first_bdds: Vec<Vec<Bdd>> = Vec::with_capacity(num_operands);
    for leaf in 0..num_operands {
        let mut bits = Vec::with_capacity(width);
        for b in 0..width {
            bits.push(expr_to_bdd(&first.leaves[leaf][b], map)?);
        }
        first_bdds.push(bits);
    }

    // For each output (operand, bit) of second: substitute first's BDDs
    // into second's expression and check == identity BDD for that variable.
    for op in 0..num_operands {
        for b in 0..width {
            let composed = substitute_bdd(&second.leaves[op][b], &first_bdds, map, width)?;
            let identity = map.bdd_vars().mk_var(map.var(op, b));
            if composed != identity {
                // Build a counterexample from the BDD disagreement.
                let diff = composed.xor(&identity);
                let witness = diff.sat_witness();
                if let Some(valuation) = witness {
                    let mut operand_values = [0u8; NUM_REGS];
                    let n_op = num_operands.min(NUM_REGS);
                    for (oper, ov) in operand_values.iter_mut().enumerate().take(n_op) {
                        for bit in 0..width {
                            if valuation.value(map.var(oper, bit)) {
                                *ov |= 1 << bit;
                            }
                        }
                    }
                    return Err(BveqError::NotEquivalent(Counterexample {
                        operand_values,
                        register: op,
                        bit: b,
                    }));
                }
                // Fallback: should be unreachable since diff is non-false.
                return Err(BveqError::NotEquivalent(Counterexample {
                    operand_values: [0u8; NUM_REGS],
                    register: op,
                    bit: b,
                }));
            }
        }
    }
    Ok(())
}

/// Substitute BDDs for input variables in a `BvExpr`, producing a composed BDD.
/// `first_bdds[operand][bit]` replaces `Var { operand, bit }`.
fn substitute_bdd(
    expr: &BvExpr,
    first_bdds: &[Vec<Bdd>],
    map: &VarMap,
    width: usize,
) -> Result<Bdd, BveqError> {
    match expr {
        BvExpr::Var { operand, bit } => {
            if *operand < first_bdds.len() && *bit < width {
                Ok(first_bdds[*operand][*bit].clone())
            } else {
                Err(BveqError::VarOutOfRange {
                    operand: *operand,
                    bit: *bit,
                    num_operands: first_bdds.len(),
                })
            }
        }
        BvExpr::Const(true) => Ok(map.bdd_vars().mk_true()),
        BvExpr::Const(false) => Ok(map.bdd_vars().mk_false()),
        BvExpr::Not(a) => Ok(substitute_bdd(a, first_bdds, map, width)?.not()),
        BvExpr::Xor(a, b) => Ok(substitute_bdd(a, first_bdds, map, width)?
            .xor(&substitute_bdd(b, first_bdds, map, width)?)),
        BvExpr::And(a, b) => Ok(substitute_bdd(a, first_bdds, map, width)?
            .and(&substitute_bdd(b, first_bdds, map, width)?)),
        BvExpr::Or(a, b) => Ok(substitute_bdd(a, first_bdds, map, width)?
            .or(&substitute_bdd(b, first_bdds, map, width)?)),
        BvExpr::Mux { sel, then_, else_ } => {
            let s = substitute_bdd(sel, first_bdds, map, width)?;
            let t = substitute_bdd(then_, first_bdds, map, width)?;
            let e = substitute_bdd(else_, first_bdds, map, width)?;
            Ok(s.and(&t).or(&s.not().and(&e)))
        }
    }
}
