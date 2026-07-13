//! Types: `0 | 1 | a + b | a × b`. Finite in the synthesizable dialect
//! (ADR-0006); sizes are exact and drive exhaustive-mode testing.

use core::fmt;

/// Core Π type: the finite type algebra.
///
/// `0` (empty), `1` (unit), `a + b` (sum / coproduct), `a × b` (product).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    /// The empty type (`0`). Has no inhabitants.
    Zero,
    /// The unit type (`1`). Has exactly one inhabitant.
    One,
    /// Sum (coproduct) type: `a + b`.
    Sum(Box<Type>, Box<Type>),
    /// Product type: `a × b`.
    Prod(Box<Type>, Box<Type>),
}

impl Type {
    /// Convenience constructor for `a + b`.
    pub fn sum(a: Type, b: Type) -> Self {
        Type::Sum(Box::new(a), Box::new(b))
    }

    /// Convenience constructor for `a × b`.
    pub fn prod(a: Type, b: Type) -> Self {
        Type::Prod(Box::new(a), Box::new(b))
    }

    /// The unary numeral type with `count` inhabitants:
    /// `numeral_ty(k) = 1 + (1 + (… + 0))` with `k` units. Its values
    /// are exactly the numerals `0 ..= k−1`, numeral `i` being
    /// `inrⁱ(inl ())` — the same encoding bounded μ-type unrolling
    /// produces for `Nat` (ADR-0035). This is the admissible counter
    /// type for `iter` (ADR-0036).
    pub fn numeral_ty(count: usize) -> Self {
        let mut ty = Type::Zero;
        for _ in 0..count {
            ty = Type::sum(Type::One, ty);
        }
        ty
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Zero => write!(f, "0"),
            Type::One => write!(f, "1"),
            Type::Sum(a, b) => write!(f, "({a} + {b})"),
            Type::Prod(a, b) => write!(f, "({a} × {b})"),
        }
    }
}
