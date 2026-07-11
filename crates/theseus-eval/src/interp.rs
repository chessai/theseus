//! Fuel-based interpreter over Core Π.
//!
//! **Fuel unit = one `trace` body application.** Structural formers
//! (`id`, `sym`, `;`, `+`, `×`, swap/assoc/unite/uniti/distrib/factor)
//! and a single `prim` application are inductively total over finite
//! values and cost ZERO fuel. Only the `trace` loop charges fuel (one
//! unit per body evaluation). See `docs/src/spec/dynamic-semantics.md`.

use theseus_core::{adjoint, Iso, Type, Value};

/// Errors during evaluation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalError {
    /// Fuel budget exhausted before evaluation completed.
    /// `steps_taken` equals the initial fuel granted (the budget hit zero only
    /// after that many body applications).
    OutOfFuel { steps_taken: u64 },
    /// The input value does not match the combinator's expected domain shape.
    DomainError(String),
}

/// Evaluate iso `f` on `value`, consuming fuel.
///
/// Structural formers and `prim` cost zero fuel. Only `trace` body
/// applications charge fuel (one unit per iteration).
///
/// # Errors
/// - [`EvalError::OutOfFuel`] if the fuel budget reaches zero mid-trace.
/// - [`EvalError::DomainError`] if the value shape doesn't match the iso.
pub fn eval(iso: &Iso, value: Value, fuel: &mut u64) -> Result<Value, EvalError> {
    let initial = *fuel;
    eval_inner(iso, value, fuel).map_err(|e| match e {
        EvalError::OutOfFuel { .. } => EvalError::OutOfFuel { steps_taken: initial },
        other => other,
    })
}

fn eval_inner(iso: &Iso, value: Value, fuel: &mut u64) -> Result<Value, EvalError> {
    match iso {
        Iso::Id(_) => Ok(value),

        Iso::Sym(f) => {
            let adj = adjoint(f);
            eval_inner(&adj, value, fuel)
        }

        Iso::Seq(f, g) => {
            let mid = eval_inner(f, value, fuel)?;
            eval_inner(g, mid, fuel)
        }

        Iso::Plus(f, g) => match value {
            Value::Left(v) => Ok(Value::left(eval_inner(f, *v, fuel)?)),
            Value::Right(v) => Ok(Value::right(eval_inner(g, *v, fuel)?)),
            other => Err(domain_err("Plus", &other)),
        },

        Iso::Times(f, g) => match value {
            Value::Pair(a, b) => {
                let ra = eval_inner(f, *a, fuel)?;
                let rb = eval_inner(g, *b, fuel)?;
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

        Iso::Trace(_u, body) => {
            // Enter on the a-side: a is the RIGHT summand.
            let mut s = Value::right(value);
            loop {
                // Charge 1 fuel for this body application.
                if *fuel == 0 {
                    return Err(EvalError::OutOfFuel { steps_taken: 0 });
                }
                *fuel -= 1;
                let t = eval_inner(body, s, fuel)?;
                match t {
                    Value::Right(y) => return Ok(*y), // exit on b-side
                    Value::Left(w) => s = Value::Left(w), // feed u-state back
                    _ => {
                        return Err(EvalError::DomainError(
                            "Trace: body returned non-sum value".into(),
                        ))
                    }
                }
            }
        }

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

/// Compute `|ty|` (number of inhabitants) structurally, saturating at
/// `u64::MAX` so that enormous carriers never overflow or panic.
/// O(term size), not O(|ty|).
fn cardinality_u64(ty: &Type) -> u64 {
    match ty {
        Type::Zero => 0,
        Type::One => 1,
        Type::Sum(a, b) => cardinality_u64(a).saturating_add(cardinality_u64(b)),
        Type::Prod(a, b) => cardinality_u64(a).saturating_mul(cardinality_u64(b)),
    }
}

/// Compute the static bound `B(iso)` — the maximum fuel an evaluation of
/// `iso` can consume. See `docs/src/spec/dynamic-semantics.md`.
pub fn static_bound(iso: &Iso) -> u64 {
    match iso {
        // Structural formers: free.
        Iso::Id(_)
        | Iso::SwapSum(_, _)
        | Iso::SwapProd(_, _)
        | Iso::AssocLSum(_, _, _)
        | Iso::AssocRSum(_, _, _)
        | Iso::AssocLProd(_, _, _)
        | Iso::AssocRProd(_, _, _)
        | Iso::UniteSum(_)
        | Iso::UnitiSum(_)
        | Iso::UniteProd(_)
        | Iso::UnitiProd(_)
        | Iso::Distrib(_, _, _)
        | Iso::Factor(_, _, _)
        | Iso::Prim(_) => 0,

        Iso::Seq(f, g) | Iso::Times(f, g) => {
            static_bound(f).saturating_add(static_bound(g))
        }

        Iso::Plus(f, g) => static_bound(f).max(static_bound(g)),

        Iso::Sym(f) => static_bound(f),

        Iso::Trace(u, body) => {
            let carrier_size = cardinality_u64(u);
            // (|u| + 1) * (1 + B(Φ))
            carrier_size
                .saturating_add(1)
                .saturating_mul(1u64.saturating_add(static_bound(body)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn structural_iso_costs_zero_fuel() {
        // Structural formers are free — fuel unchanged.
        let mut fuel = 0;
        assert_eq!(
            eval(&Iso::Id(Type::One), Value::Unit, &mut fuel).unwrap(),
            Value::Unit,
        );
        assert_eq!(fuel, 0);
    }

    #[test]
    fn structural_seq_costs_zero_fuel() {
        // Seq of structural isos is still free.
        let iso = Iso::seq(Iso::Id(Type::One), Iso::Id(Type::One));
        let mut fuel = 0;
        eval(&iso, Value::Unit, &mut fuel).unwrap();
        assert_eq!(fuel, 0);
    }

    #[test]
    fn domain_error_on_bad_input() {
        let iso = Iso::SwapSum(Type::One, Type::One);
        assert!(matches!(
            run(&iso, Value::Unit),
            Err(EvalError::DomainError(_))
        ));
    }

    // ─── Trace tests ───────────────────────────────────────────────

    fn bit_ty() -> Type {
        Type::sum(Type::One, Type::One)
    }

    /// Build a counter body Φ : (Bit + 1) ↔ (Bit + 1) as a Prim.
    ///
    /// Mapping (Bit = Left(Unit)|Right(Unit), a = b = Unit):
    ///   Right(Unit)       → Left(Left(Unit))     -- entry: a-side → state O
    ///   Left(Left(Unit))  → Left(Right(Unit))    -- state O → state I
    ///   Left(Right(Unit)) → Right(Unit)           -- state I → b-side exit
    ///
    /// This cycles through both Bit states before exiting, costing 3 body
    /// applications.
    fn counter_body() -> Iso {
        use std::collections::BTreeMap;
        use theseus_core::PrimDef;

        // Values in Bit + 1:
        let r_unit = Value::right(Value::Unit);       // Right(Unit) — boundary
        let l_left = Value::left(Value::left(Value::Unit));  // Left(Left(Unit)) — O
        let l_right = Value::left(Value::right(Value::Unit)); // Left(Right(Unit)) — I

        let mut fwd = BTreeMap::new();
        fwd.insert(r_unit.clone(), l_left.clone());   // Right(()) → Left(O)
        fwd.insert(l_left.clone(), l_right.clone());  // Left(O) → Left(I)
        fwd.insert(l_right.clone(), r_unit.clone());  // Left(I) → Right(())

        // Backward is the inverse permutation.
        let mut bwd = BTreeMap::new();
        bwd.insert(l_left.clone(), r_unit.clone());
        bwd.insert(l_right.clone(), l_left.clone());
        bwd.insert(r_unit.clone(), l_right.clone());

        let ty = Type::sum(bit_ty(), Type::One);
        Iso::Prim(Box::new(PrimDef {
            forward_name: "counter".into(),
            backward_name: "counter_inv".into(),
            input_ty: ty.clone(),
            output_ty: ty,
            forward: fwd,
            backward: bwd,
        }))
    }

    #[test]
    fn trace_counter_forward() {
        let iso = Iso::trace(bit_ty(), counter_body());
        let mut fuel = 100;
        let result = eval(&iso, Value::Unit, &mut fuel).unwrap();
        assert_eq!(result, Value::Unit);
        // Started with 100, should have consumed exactly 3.
        assert_eq!(fuel, 97);
    }

    #[test]
    fn trace_counter_fuel_delta() {
        let iso = Iso::trace(bit_ty(), counter_body());
        let mut fuel = 3;
        let result = eval(&iso, Value::Unit, &mut fuel).unwrap();
        assert_eq!(result, Value::Unit);
        assert_eq!(fuel, 0);
    }

    #[test]
    fn trace_counter_out_of_fuel() {
        let iso = Iso::trace(bit_ty(), counter_body());
        let mut fuel = 2;
        assert_eq!(
            eval(&iso, Value::Unit, &mut fuel),
            Err(EvalError::OutOfFuel { steps_taken: 2 }),
        );
    }

    #[test]
    fn out_of_fuel_steps_taken_equals_initial_fuel() {
        // The counter body needs 3 body applications. Granting k < 3 fuel
        // should return OutOfFuel { steps_taken: k }.
        let iso = Iso::trace(bit_ty(), counter_body());
        for k in 0..3 {
            let mut fuel = k;
            assert_eq!(
                eval(&iso, Value::Unit, &mut fuel),
                Err(EvalError::OutOfFuel { steps_taken: k }),
                "granting {k} fuel should report steps_taken={k}",
            );
        }
    }

    #[test]
    fn structural_tree_costs_zero_fuel() {
        // A big structural tree: seq(times(swap⁺, swap⁺), seq(id, swap×))
        let bt = bit_ty();
        let s = Iso::SwapSum(Type::One, Type::One);
        let iso = Iso::seq(
            Iso::times(s.clone(), s),
            Iso::seq(
                Iso::Id(Type::prod(bt.clone(), bt.clone())),
                Iso::SwapProd(bt.clone(), bt),
            ),
        );
        let v = Value::pair(
            Value::left(Value::Unit),
            Value::right(Value::Unit),
        );
        let mut fuel = 0;
        eval(&iso, v, &mut fuel).unwrap();
        assert_eq!(fuel, 0, "structural tree should consume zero fuel");
    }

    // ─── static_bound tests ────────────────────────────────────────

    #[test]
    fn static_bound_structural_is_zero() {
        let iso = Iso::seq(
            Iso::SwapSum(Type::One, Type::One),
            Iso::Id(Type::sum(Type::One, Type::One)),
        );
        assert_eq!(static_bound(&iso), 0);
    }

    #[test]
    fn static_bound_trace_with_structural_body() {
        // B(Trace(Bit, structural)) = (|Bit|+1)*(1+0) = 3*1 = 3
        let body = Iso::SwapSum(bit_ty(), Type::One);
        let iso = Iso::trace(bit_ty(), body);
        assert_eq!(static_bound(&iso), 3);
    }

    #[test]
    fn static_bound_seq_of_traces() {
        // B(Trace ; Trace) = B(Trace) + B(Trace) = 3 + 3 = 6
        let body = Iso::SwapSum(bit_ty(), Type::One);
        let t = Iso::trace(bit_ty(), body);
        let iso = Iso::seq(t.clone(), t);
        assert_eq!(static_bound(&iso), 6);
    }

    #[test]
    fn static_bound_plus_of_traces() {
        // B(Trace + Trace) = max(3, 3) = 3
        let body = Iso::SwapSum(bit_ty(), Type::One);
        let t = Iso::trace(bit_ty(), body);
        let iso = Iso::plus(t.clone(), t);
        assert_eq!(static_bound(&iso), 3);
    }

    #[test]
    fn static_bound_plain_structural_zero() {
        assert_eq!(static_bound(&Iso::Id(Type::One)), 0);
        assert_eq!(
            static_bound(&Iso::Distrib(Type::One, Type::One, Type::One)),
            0,
        );
    }

    #[test]
    fn static_bound_nested_trace() {
        // u = Bit (|u| = 2), u' = 1+1+1 (|u'| = 3)
        // inner body ψ is structural => B(ψ) = 0
        // B(inner) = B(Trace(u', ψ)) = (|u'|+1)*(1+0) = 4
        // B(outer) = B(Trace(u, inner)) = (|u|+1)*(1+B(inner)) = 3*(1+4) = 15
        // i.e. (|u|+1)*(1 + (|u'|+1))
        let u = bit_ty(); // |u| = 2
        let u_prime = Type::sum(Type::One, Type::sum(Type::One, Type::One)); // |u'| = 3
        let inner_body = Iso::SwapSum(u_prime.clone(), Type::One);
        let inner = Iso::trace(u_prime, inner_body);
        let outer = Iso::trace(u, inner);
        // (|u|+1)*(1 + (|u'|+1)) = 3 * (1 + 4) = 15
        assert_eq!(static_bound(&outer), 15);
    }

    #[test]
    fn static_bound_sym_trace() {
        // B(Sym(Trace(...))) == B(Trace(...))
        let body = Iso::SwapSum(bit_ty(), Type::One);
        let t = Iso::trace(bit_ty(), body);
        let sym_t = Iso::Sym(Box::new(t.clone()));
        assert_eq!(static_bound(&sym_t), static_bound(&t));
    }
}
