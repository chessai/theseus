//! Exhaustive value enumeration for finite types.
//!
//! Below the size threshold, "proven on all inputs" is an achievable test
//! verdict (ADR-0018). The enumerator produces every value of a finite type
//! in a deterministic canonical order (left-biased, depth-first).

use theseus_core::{Type, Value};

/// Enumerate all inhabitants of a finite type in deterministic canonical order.
///
/// - `Zero` → empty
/// - `One` → `[Unit]`
/// - `Sum(a, b)` → all `Left(v)` for `v ∈ a`, then all `Right(w)` for `w ∈ b`
/// - `Prod(a, b)` → cartesian product, `a`-major order
pub fn enumerate(ty: &Type) -> Vec<Value> {
    theseus_core::enumerate_values(ty)
}

/// The cardinality of a finite type (number of inhabitants).
pub fn cardinality(ty: &Type) -> usize {
    match ty {
        Type::Zero => 0,
        Type::One => 1,
        Type::Sum(a, b) => cardinality(a) + cardinality(b),
        Type::Prod(a, b) => cardinality(a) * cardinality(b),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn bool_ty() -> Type {
        Type::sum(Type::One, Type::One)
    }

    fn three_ty() -> Type {
        Type::sum(Type::One, Type::sum(Type::One, Type::One))
    }

    #[test]
    fn zero_empty() {
        assert!(enumerate(&Type::Zero).is_empty());
        assert_eq!(cardinality(&Type::Zero), 0);
    }

    #[test]
    fn one_singleton() {
        let vals = enumerate(&Type::One);
        assert_eq!(vals, vec![Value::Unit]);
        assert_eq!(cardinality(&Type::One), 1);
    }

    #[test]
    fn bool_two_values() {
        let vals = enumerate(&bool_ty());
        assert_eq!(vals.len(), 2);
        assert_eq!(cardinality(&bool_ty()), 2);
        let set: HashSet<_> = vals.iter().collect();
        assert_eq!(set.len(), 2, "no duplicates");
    }

    #[test]
    fn three_values() {
        let vals = enumerate(&three_ty());
        assert_eq!(vals.len(), 3);
        assert_eq!(cardinality(&three_ty()), 3);
        let set: HashSet<_> = vals.iter().collect();
        assert_eq!(set.len(), 3, "no duplicates");
    }

    #[test]
    fn prod_bool_bool() {
        let ty = Type::prod(bool_ty(), bool_ty());
        let vals = enumerate(&ty);
        assert_eq!(vals.len(), 4);
        assert_eq!(cardinality(&ty), 4);
        let set: HashSet<_> = vals.iter().collect();
        assert_eq!(set.len(), 4, "no duplicates");
    }

    #[test]
    fn prod_three_bool() {
        let ty = Type::prod(three_ty(), bool_ty());
        let vals = enumerate(&ty);
        assert_eq!(vals.len(), 6);
        assert_eq!(cardinality(&ty), 6);
        let set: HashSet<_> = vals.iter().collect();
        assert_eq!(set.len(), 6, "no duplicates");
    }

    #[test]
    fn zero_product_empty() {
        let ty = Type::prod(Type::Zero, Type::One);
        assert!(enumerate(&ty).is_empty());
        assert_eq!(cardinality(&ty), 0);
    }

    #[test]
    fn zero_sum_one() {
        let ty = Type::sum(Type::Zero, Type::One);
        let vals = enumerate(&ty);
        assert_eq!(vals.len(), 1);
        assert_eq!(cardinality(&ty), 1);
    }

    #[test]
    fn deterministic_order() {
        let ty = Type::prod(bool_ty(), three_ty());
        let v1 = enumerate(&ty);
        let v2 = enumerate(&ty);
        assert_eq!(v1, v2, "enumeration must be deterministic");
    }

    #[test]
    fn sum_of_sums() {
        // (1 + 1) + (1 + 1) should have 4 values
        let ty = Type::sum(bool_ty(), bool_ty());
        let vals = enumerate(&ty);
        assert_eq!(vals.len(), 4);
        let set: HashSet<_> = vals.iter().collect();
        assert_eq!(set.len(), 4, "no duplicates");
    }

    #[test]
    fn nested_prod() {
        // (1 × 1) × (1 + 1) = 1 × 2 = 2
        let ty = Type::prod(Type::prod(Type::One, Type::One), bool_ty());
        let vals = enumerate(&ty);
        assert_eq!(vals.len(), 2);
        assert_eq!(cardinality(&ty), 2);
    }

    #[test]
    fn cardinality_matches_enumerate_len() {
        let types = vec![
            Type::Zero,
            Type::One,
            bool_ty(),
            three_ty(),
            Type::prod(bool_ty(), bool_ty()),
            Type::sum(three_ty(), bool_ty()),
            Type::prod(three_ty(), Type::prod(bool_ty(), bool_ty())),
        ];
        for ty in &types {
            assert_eq!(
                enumerate(ty).len(),
                cardinality(ty),
                "mismatch for type {ty:?}"
            );
        }
    }
}
