//! Saturating type cardinality for fuel-bound computation.

use crate::ty::Type;

/// Compute `|ty|` (number of inhabitants) structurally, saturating at
/// `u64::MAX` so that enormous carriers never overflow or panic.
/// O(term size), not O(|ty|).
pub fn cardinality_u64(ty: &Type) -> u64 {
    match ty {
        Type::Zero => 0,
        Type::One => 1,
        Type::Sum(a, b) => cardinality_u64(a).saturating_add(cardinality_u64(b)),
        Type::Prod(a, b) => cardinality_u64(a).saturating_mul(cardinality_u64(b)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cardinality_zero() {
        assert_eq!(cardinality_u64(&Type::Zero), 0);
    }

    #[test]
    fn cardinality_one() {
        assert_eq!(cardinality_u64(&Type::One), 1);
    }

    #[test]
    fn cardinality_sum() {
        // 1 + 1 = 2
        assert_eq!(cardinality_u64(&Type::sum(Type::One, Type::One)), 2);
    }

    #[test]
    fn cardinality_prod() {
        // (1+1) × (1+1+1) = 2 × 3 = 6
        let two = Type::sum(Type::One, Type::One);
        let three = Type::sum(Type::One, Type::sum(Type::One, Type::One));
        assert_eq!(cardinality_u64(&Type::prod(two, three)), 6);
    }

    #[test]
    fn cardinality_saturates() {
        // Build a product type whose cardinality exceeds u64::MAX.
        // 2^64 overflows, so 2^65 should saturate at u64::MAX.
        let mut ty = Type::sum(Type::One, Type::One); // 2
        for _ in 0..64 {
            ty = Type::prod(ty.clone(), Type::sum(Type::One, Type::One)); // *2 each time
        }
        assert_eq!(cardinality_u64(&ty), u64::MAX);
    }
}
