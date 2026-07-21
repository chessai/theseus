//! Symbolic bit-vector expression AST and concrete evaluator.
//!
//! Moved from `theseus-tisc/src/bveq.rs` so that `PrimSpec` (a per-bit
//! `BvExpr` register-transfer) can live in theseus-core without pulling
//! in the BDD crate. All BDD machinery (`expr_to_bdd`, `VarMap`, `equiv`)
//! stays in theseus-tisc.

/// Per-bit boolean expression over w-bit operand lanes (w ≤ 8).
///
/// No `Mul` variant — kept unrepresentable so BDDs stay polynomial (r2 I3).
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
