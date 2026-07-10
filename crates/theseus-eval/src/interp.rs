//! Fuel-based interpreter over Core Π (plain fragment).
//!
//! Every call to [`eval`] decrements the fuel budget by one. Running out of
//! fuel is a distinct error (`OutOfFuel`), never conflated with a domain
//! error — per ADR-0005, fuel is the join-inverse-category partiality model
//! in software.

use theseus_core::{adjoint, Iso, Value};

/// Errors during evaluation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalError {
    /// Fuel budget exhausted before evaluation completed.
    OutOfFuel,
    /// The input value does not match the combinator's expected domain shape.
    DomainError(String),
    /// Trace evaluation is deferred to the label slice (not in the plain fragment).
    TraceNotImplemented,
}

/// Evaluate iso `f` on `value`, consuming fuel.
///
/// Each combinator application costs one fuel unit. Composition and
/// bifunctors recurse, paying fuel at each node.
///
/// # Errors
/// - [`EvalError::OutOfFuel`] if the fuel budget reaches zero.
/// - [`EvalError::DomainError`] if the value shape doesn't match the iso.
/// - [`EvalError::TraceNotImplemented`] if a `Trace` node is encountered.
pub fn eval(iso: &Iso, value: Value, fuel: &mut u64) -> Result<Value, EvalError> {
    if *fuel == 0 {
        return Err(EvalError::OutOfFuel);
    }
    *fuel -= 1;

    match iso {
        Iso::Id(_) => Ok(value),

        Iso::Sym(f) => {
            let adj = adjoint(f);
            eval(&adj, value, fuel)
        }

        Iso::Seq(f, g) => {
            let mid = eval(f, value, fuel)?;
            eval(g, mid, fuel)
        }

        Iso::Plus(f, g) => match value {
            Value::Left(v) => Ok(Value::left(eval(f, *v, fuel)?)),
            Value::Right(v) => Ok(Value::right(eval(g, *v, fuel)?)),
            other => Err(domain_err("Plus", &other)),
        },

        Iso::Times(f, g) => match value {
            Value::Pair(a, b) => {
                let ra = eval(f, *a, fuel)?;
                let rb = eval(g, *b, fuel)?;
                Ok(Value::pair(ra, rb))
            }
            other => Err(domain_err("Times", &other)),
        },

        Iso::SwapSum(_, _) => match value {
            Value::Left(v) => Ok(Value::right(*v)),
            Value::Right(v) => Ok(Value::left(*v)),
            other => Err(domain_err("SwapSum", &other)),
        },

        Iso::SwapProd(_, _) => match value {
            Value::Pair(a, b) => Ok(Value::pair(*b, *a)),
            other => Err(domain_err("SwapProd", &other)),
        },

        // assocl⁺ : a + (b + c) ↔ (a + b) + c
        Iso::AssocLSum(_, _, _) => match value {
            Value::Left(v) => Ok(Value::left(Value::left(*v))),
            Value::Right(bc) => match *bc {
                Value::Left(w) => Ok(Value::left(Value::right(*w))),
                Value::Right(x) => Ok(Value::right(*x)),
                other => Err(domain_err("AssocLSum/inner", &other)),
            },
            other => Err(domain_err("AssocLSum", &other)),
        },

        // assocr⁺ : (a + b) + c ↔ a + (b + c)
        Iso::AssocRSum(_, _, _) => match value {
            Value::Left(ab) => match *ab {
                Value::Left(v) => Ok(Value::left(*v)),
                Value::Right(w) => Ok(Value::right(Value::left(*w))),
                other => Err(domain_err("AssocRSum/inner", &other)),
            },
            Value::Right(x) => Ok(Value::right(Value::right(*x))),
            other => Err(domain_err("AssocRSum", &other)),
        },

        // assoclˣ : a × (b × c) ↔ (a × b) × c
        Iso::AssocLProd(_, _, _) => match value {
            Value::Pair(a, bc) => match *bc {
                Value::Pair(b, c) => Ok(Value::pair(Value::pair(*a, *b), *c)),
                other => Err(domain_err("AssocLProd/inner", &other)),
            },
            other => Err(domain_err("AssocLProd", &other)),
        },

        // assocrˣ : (a × b) × c ↔ a × (b × c)
        Iso::AssocRProd(_, _, _) => match value {
            Value::Pair(ab, c) => match *ab {
                Value::Pair(a, b) => Ok(Value::pair(*a, Value::pair(*b, *c))),
                other => Err(domain_err("AssocRProd/inner", &other)),
            },
            other => Err(domain_err("AssocRProd", &other)),
        },

        // uniteˣ : 1 × a ↔ a
        Iso::UniteProd(_) => match value {
            Value::Pair(u, v) => match *u {
                Value::Unit => Ok(*v),
                other => Err(domain_err("UniteProd/unit", &other)),
            },
            other => Err(domain_err("UniteProd", &other)),
        },

        // unitiˣ : a ↔ 1 × a
        Iso::UnitiProd(_) => Ok(Value::pair(Value::Unit, value)),

        // unite⁺ : 0 + a ↔ a  (inl is unreachable since 0 is empty)
        Iso::UniteSum(_) => match value {
            Value::Right(v) => Ok(*v),
            other => Err(domain_err("UniteSum", &other)),
        },

        // uniti⁺ : a ↔ 0 + a
        Iso::UnitiSum(_) => Ok(Value::right(value)),

        // distrib : a × (b + c) ↔ (a × b) + (a × c)
        Iso::Distrib(_, _, _) => match value {
            Value::Pair(v, bc) => match *bc {
                Value::Left(w) => Ok(Value::left(Value::pair(*v, *w))),
                Value::Right(x) => Ok(Value::right(Value::pair(*v, *x))),
                other => Err(domain_err("Distrib/inner", &other)),
            },
            other => Err(domain_err("Distrib", &other)),
        },

        // factor : (a × b) + (a × c) ↔ a × (b + c)
        Iso::Factor(_, _, _) => match value {
            Value::Left(vw) => match *vw {
                Value::Pair(v, w) => Ok(Value::pair(*v, Value::left(*w))),
                other => Err(domain_err("Factor/left", &other)),
            },
            Value::Right(vx) => match *vx {
                Value::Pair(v, x) => Ok(Value::pair(*v, Value::right(*x))),
                other => Err(domain_err("Factor/right", &other)),
            },
            other => Err(domain_err("Factor", &other)),
        },

        Iso::Trace(_, _) => Err(EvalError::TraceNotImplemented),

        Iso::Prim(def) => def
            .forward
            .get(&value)
            .cloned()
            .ok_or_else(|| {
                EvalError::DomainError(format!(
                    "prim '{}': value not in forward map: {:?}",
                    def.forward_name, value
                ))
            }),
    }
}

fn domain_err(iso: &str, value: &Value) -> EvalError {
    EvalError::DomainError(format!("{iso}: unexpected value shape: {value:?}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use theseus_core::Type;

    const FUEL: u64 = 1000;

    fn run(iso: &Iso, v: Value) -> Result<Value, EvalError> {
        let mut fuel = FUEL;
        eval(iso, v, &mut fuel)
    }

    #[test]
    fn eval_id() {
        assert_eq!(run(&Iso::Id(Type::One), Value::Unit).unwrap(), Value::Unit);
    }

    #[test]
    fn eval_swap_sum_left() {
        let iso = Iso::SwapSum(Type::One, Type::One);
        assert_eq!(
            run(&iso, Value::left(Value::Unit)).unwrap(),
            Value::right(Value::Unit)
        );
    }

    #[test]
    fn eval_swap_sum_right() {
        let iso = Iso::SwapSum(Type::One, Type::One);
        assert_eq!(
            run(&iso, Value::right(Value::Unit)).unwrap(),
            Value::left(Value::Unit)
        );
    }

    #[test]
    fn eval_swap_prod() {
        let iso = Iso::SwapProd(Type::One, Type::One);
        let v = Value::pair(Value::Unit, Value::Unit);
        assert_eq!(run(&iso, v.clone()).unwrap(), v);
    }

    #[test]
    fn eval_assocl_sum() {
        let iso = Iso::AssocLSum(Type::One, Type::One, Type::One);
        // inl () -> inl (inl ())
        assert_eq!(
            run(&iso, Value::left(Value::Unit)).unwrap(),
            Value::left(Value::left(Value::Unit))
        );
        // inr (inl ()) -> inl (inr ())
        assert_eq!(
            run(&iso, Value::right(Value::left(Value::Unit))).unwrap(),
            Value::left(Value::right(Value::Unit))
        );
        // inr (inr ()) -> inr ()
        assert_eq!(
            run(&iso, Value::right(Value::right(Value::Unit))).unwrap(),
            Value::right(Value::Unit)
        );
    }

    #[test]
    fn eval_assocr_sum() {
        let iso = Iso::AssocRSum(Type::One, Type::One, Type::One);
        // inl (inl ()) -> inl ()
        assert_eq!(
            run(&iso, Value::left(Value::left(Value::Unit))).unwrap(),
            Value::left(Value::Unit)
        );
        // inl (inr ()) -> inr (inl ())
        assert_eq!(
            run(&iso, Value::left(Value::right(Value::Unit))).unwrap(),
            Value::right(Value::left(Value::Unit))
        );
        // inr () -> inr (inr ())
        assert_eq!(
            run(&iso, Value::right(Value::Unit)).unwrap(),
            Value::right(Value::right(Value::Unit))
        );
    }

    #[test]
    fn eval_unite_prod() {
        let iso = Iso::UniteProd(Type::One);
        let v = Value::pair(Value::Unit, Value::Unit);
        assert_eq!(run(&iso, v).unwrap(), Value::Unit);
    }

    #[test]
    fn eval_uniti_prod() {
        let iso = Iso::UnitiProd(Type::One);
        assert_eq!(
            run(&iso, Value::Unit).unwrap(),
            Value::pair(Value::Unit, Value::Unit)
        );
    }

    #[test]
    fn eval_unite_sum() {
        let iso = Iso::UniteSum(Type::One);
        assert_eq!(
            run(&iso, Value::right(Value::Unit)).unwrap(),
            Value::Unit
        );
    }

    #[test]
    fn eval_uniti_sum() {
        let iso = Iso::UnitiSum(Type::One);
        assert_eq!(
            run(&iso, Value::Unit).unwrap(),
            Value::right(Value::Unit)
        );
    }

    #[test]
    fn eval_distrib() {
        let iso = Iso::Distrib(Type::One, Type::One, Type::One);
        // ((), inl ()) -> inl ((), ())
        assert_eq!(
            run(&iso, Value::pair(Value::Unit, Value::left(Value::Unit))).unwrap(),
            Value::left(Value::pair(Value::Unit, Value::Unit))
        );
        // ((), inr ()) -> inr ((), ())
        assert_eq!(
            run(&iso, Value::pair(Value::Unit, Value::right(Value::Unit))).unwrap(),
            Value::right(Value::pair(Value::Unit, Value::Unit))
        );
    }

    #[test]
    fn eval_factor() {
        let iso = Iso::Factor(Type::One, Type::One, Type::One);
        // inl ((), ()) -> ((), inl ())
        assert_eq!(
            run(&iso, Value::left(Value::pair(Value::Unit, Value::Unit))).unwrap(),
            Value::pair(Value::Unit, Value::left(Value::Unit))
        );
        // inr ((), ()) -> ((), inr ())
        assert_eq!(
            run(&iso, Value::right(Value::pair(Value::Unit, Value::Unit))).unwrap(),
            Value::pair(Value::Unit, Value::right(Value::Unit))
        );
    }

    #[test]
    fn eval_seq() {
        // id ; swap⁺ should behave like swap⁺
        let bool_ty = Type::sum(Type::One, Type::One);
        let iso = Iso::seq(Iso::Id(bool_ty), Iso::SwapSum(Type::One, Type::One));
        assert_eq!(
            run(&iso, Value::left(Value::Unit)).unwrap(),
            Value::right(Value::Unit)
        );
    }

    #[test]
    fn eval_plus() {
        // (swap⁺ + id) on (1+1) + 1
        let f = Iso::SwapSum(Type::One, Type::One);
        let g = Iso::Id(Type::One);
        let iso = Iso::plus(f, g);
        // inl (inl ()) -> inl (inr ())
        assert_eq!(
            run(&iso, Value::left(Value::left(Value::Unit))).unwrap(),
            Value::left(Value::right(Value::Unit))
        );
        // inr () -> inr ()
        assert_eq!(
            run(&iso, Value::right(Value::Unit)).unwrap(),
            Value::right(Value::Unit)
        );
    }

    #[test]
    fn eval_times() {
        // (id × swap⁺) on 1 × (1+1)
        let f = Iso::Id(Type::One);
        let g = Iso::SwapSum(Type::One, Type::One);
        let iso = Iso::times(f, g);
        let v = Value::pair(Value::Unit, Value::left(Value::Unit));
        let expected = Value::pair(Value::Unit, Value::right(Value::Unit));
        assert_eq!(run(&iso, v).unwrap(), expected);
    }

    #[test]
    fn fuel_exhaustion_returns_out_of_fuel() {
        let mut fuel = 0;
        assert_eq!(
            eval(&Iso::Id(Type::One), Value::Unit, &mut fuel),
            Err(EvalError::OutOfFuel)
        );
    }

    #[test]
    fn fuel_one_suffices_for_id() {
        let mut fuel = 1;
        assert_eq!(
            eval(&Iso::Id(Type::One), Value::Unit, &mut fuel).unwrap(),
            Value::Unit
        );
        assert_eq!(fuel, 0);
    }

    #[test]
    fn fuel_seq_costs_three() {
        // Seq costs 1 (for Seq node) + 1 (for f) + 1 (for g) = 3
        let iso = Iso::seq(Iso::Id(Type::One), Iso::Id(Type::One));
        let mut fuel = 3;
        eval(&iso, Value::Unit, &mut fuel).unwrap();
        assert_eq!(fuel, 0);
    }

    #[test]
    fn fuel_seq_insufficient() {
        let iso = Iso::seq(Iso::Id(Type::One), Iso::Id(Type::One));
        let mut fuel = 2;
        assert_eq!(
            eval(&iso, Value::Unit, &mut fuel),
            Err(EvalError::OutOfFuel)
        );
    }

    #[test]
    fn trace_returns_not_implemented() {
        let body = Iso::Id(Type::sum(Type::One, Type::One));
        let iso = Iso::Trace(Type::One, Box::new(body));
        assert_eq!(run(&iso, Value::Unit), Err(EvalError::TraceNotImplemented));
    }

    #[test]
    fn domain_error_on_bad_input() {
        let iso = Iso::SwapSum(Type::One, Type::One);
        assert!(matches!(
            run(&iso, Value::Unit),
            Err(EvalError::DomainError(_))
        ));
    }
}
