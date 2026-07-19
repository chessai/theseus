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
    /// Width-parameterized single-leaf word: cardinality `2^n`, `num_leaves = 1`.
    /// Values reuse the existing thermometer encoding `Right^k(Left(Unit))`.
    Flat(u8),
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

    /// Convenience constructor for `Flat(n)` — a single-leaf word of width
    /// `n` bits (cardinality `2^n`). Panics if `n` is 0 or > 8.
    pub fn flat(n: u8) -> Self {
        assert!((1..=8).contains(&n), "Flat width must be 1..=8, got {n}");
        Type::Flat(n)
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

    /// The list-spine type of unroll depth `depth` over element type
    /// `elem`: `list_spine_ty(b, d) = 1 + (b × list_spine_ty(b, d−1))`,
    /// `list_spine_ty(b, 0) = 0`. This is exactly the shape bounded
    /// μ-unrolling (ADR-0035) produces for `List b = Nil | Cons b List`;
    /// its values are the lists of length `0 ..= d−1`, encoded
    /// `nil = inl ()`, `cons h t = inr ⟨h, t⟩`. This is the admissible
    /// spine type for `fold`/`unfold` (ADR-0037).
    pub fn list_spine_ty(elem: Type, depth: usize) -> Self {
        let mut ty = Type::Zero;
        for _ in 0..depth {
            ty = Type::sum(Type::One, Type::prod(elem.clone(), ty));
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
            Type::Flat(n) => write!(f, "Flat({n})"),
        }
    }
}
