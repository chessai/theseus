//! Typed value domain for Core Π.
//!
//! Re-exports [`Value`] from `theseus_core` and provides type-membership
//! checking (`inhabits`).

use theseus_core::Type;
pub use theseus_core::Value;

/// Returns `true` if `value` is a valid inhabitant of `ty`.
pub fn inhabits(value: &Value, ty: &Type) -> bool {
    match (value, ty) {
        (Value::Unit, Type::One) => true,
        (Value::Left(v), Type::Sum(a, _)) => inhabits(v, a),
        (Value::Right(v), Type::Sum(_, b)) => inhabits(v, b),
        (Value::Pair(a, b), Type::Prod(ta, tb)) => inhabits(a, ta) && inhabits(b, tb),
        (_, Type::Flat(n)) => {
            // A Flat(n) value is a thermometer Right^k(Left(Unit)) with k < 2^n.
            let mut count = 0u64;
            let mut cur = value;
            loop {
                match cur {
                    Value::Left(u) if u.as_ref() == &Value::Unit => {
                        return count < (1u64 << n);
                    }
                    Value::Right(rest) => {
                        count += 1;
                        cur = rest;
                    }
                    _ => return false,
                }
            }
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unit_inhabits_one() {
        assert!(inhabits(&Value::Unit, &Type::One));
    }

    #[test]
    fn unit_not_inhabits_zero() {
        assert!(!inhabits(&Value::Unit, &Type::Zero));
    }

    #[test]
    fn left_unit_inhabits_sum() {
        let ty = Type::sum(Type::One, Type::One);
        assert!(inhabits(&Value::left(Value::Unit), &ty));
    }

    #[test]
    fn right_unit_inhabits_sum() {
        let ty = Type::sum(Type::One, Type::One);
        assert!(inhabits(&Value::right(Value::Unit), &ty));
    }

    #[test]
    fn pair_inhabits_prod() {
        let ty = Type::prod(Type::One, Type::One);
        assert!(inhabits(&Value::pair(Value::Unit, Value::Unit), &ty));
    }

    #[test]
    fn wrong_type_fails() {
        assert!(!inhabits(&Value::Unit, &Type::sum(Type::One, Type::One)));
        assert!(!inhabits(&Value::left(Value::Unit), &Type::One));
        assert!(!inhabits(
            &Value::pair(Value::Unit, Value::Unit),
            &Type::One
        ));
    }

    #[test]
    fn nested_sum() {
        let ty = Type::sum(Type::One, Type::sum(Type::One, Type::One));
        assert!(inhabits(&Value::left(Value::Unit), &ty));
        assert!(inhabits(&Value::right(Value::left(Value::Unit)), &ty));
        assert!(inhabits(&Value::right(Value::right(Value::Unit)), &ty));
        assert!(!inhabits(&Value::right(Value::Unit), &ty));
    }
}
