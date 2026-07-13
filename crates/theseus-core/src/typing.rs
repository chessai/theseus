//! Type checker: `type_of(term) → Result<(Type, Type), TypeError>`.
//!
//! Implements the typing rules from `core-pi.md`. Every well-typed iso
//! `f` gets a type `a ↔ b`; ill-typed terms are rejected.

use crate::iso::Iso;
use crate::ty::Type;

/// Error from the type checker.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
    /// In `f ; g`, the output type of `f` doesn't match the input type of `g`.
    SeqMismatch { left_output: Type, right_input: Type },
    /// In `trace(u, f)`, `f`'s domain is not a sum type.
    TraceInputNotSum(Type),
    /// In `trace(u, f)`, `f`'s codomain is not a sum type.
    TraceOutputNotSum(Type),
    /// In `trace(u, f)`, the carrier in `f`'s domain ≠ declared `u`.
    TraceInputCarrier { expected: Type, got: Type },
    /// In `trace(u, f)`, the carrier in `f`'s codomain ≠ declared `u`.
    TraceOutputCarrier { expected: Type, got: Type },
    /// In `iter(n, f)`, the counter type `n` is not a unary numeral
    /// type `1 + (1 + (… + 0))`.
    IterCounterNotNumeral(Type),
    /// In `iter(n, f)`, the body is not an endo-iso (`f : a ↔ a`).
    IterBodyNotEndo { input: Type, output: Type },
}

/// Is `ty` a unary numeral type `1 + (1 + (… + 0))`? (`0` itself is the
/// degenerate, uninhabited numeral type.) These are exactly the types
/// bounded μ-unrolling (ADR-0035) produces for `Nat`, and the only
/// admissible `iter` counters: their inhabitants have a canonical
/// reading as counts `k = ` number of `inr` wrappers.
pub fn is_numeral_ty(ty: &Type) -> bool {
    let mut cur = ty;
    loop {
        match cur {
            Type::Zero => return true,
            Type::Sum(l, r) if l.as_ref() == &Type::One => cur = r,
            _ => return false,
        }
    }
}

/// Compute the type `a ↔ b` of a well-typed iso term, or return a
/// [`TypeError`] for ill-typed terms.
pub fn type_of(iso: &Iso) -> Result<(Type, Type), TypeError> {
    match iso {
        Iso::Id(a) => Ok((a.clone(), a.clone())),

        Iso::Sym(f) => {
            let (a, b) = type_of(f)?;
            Ok((b, a))
        }

        Iso::Seq(f, g) => {
            let (a, b1) = type_of(f)?;
            let (b2, c) = type_of(g)?;
            if b1 != b2 {
                return Err(TypeError::SeqMismatch {
                    left_output: b1,
                    right_input: b2,
                });
            }
            Ok((a, c))
        }

        Iso::Plus(f, g) => {
            let (a, b) = type_of(f)?;
            let (c, d) = type_of(g)?;
            Ok((Type::sum(a, c), Type::sum(b, d)))
        }

        Iso::Times(f, g) => {
            let (a, b) = type_of(f)?;
            let (c, d) = type_of(g)?;
            Ok((Type::prod(a, c), Type::prod(b, d)))
        }

        // ── Sum monoid ───────────────────────────────────────────
        Iso::SwapSum(a, b) => Ok((
            Type::sum(a.clone(), b.clone()),
            Type::sum(b.clone(), a.clone()),
        )),

        Iso::AssocLSum(a, b, c) => Ok((
            Type::sum(a.clone(), Type::sum(b.clone(), c.clone())),
            Type::sum(Type::sum(a.clone(), b.clone()), c.clone()),
        )),

        Iso::AssocRSum(a, b, c) => Ok((
            Type::sum(Type::sum(a.clone(), b.clone()), c.clone()),
            Type::sum(a.clone(), Type::sum(b.clone(), c.clone())),
        )),

        Iso::UniteSum(a) => Ok((Type::sum(Type::Zero, a.clone()), a.clone())),

        Iso::UnitiSum(a) => Ok((a.clone(), Type::sum(Type::Zero, a.clone()))),

        // ── Product monoid ───────────────────────────────────────
        Iso::SwapProd(a, b) => Ok((
            Type::prod(a.clone(), b.clone()),
            Type::prod(b.clone(), a.clone()),
        )),

        Iso::AssocLProd(a, b, c) => Ok((
            Type::prod(a.clone(), Type::prod(b.clone(), c.clone())),
            Type::prod(Type::prod(a.clone(), b.clone()), c.clone()),
        )),

        Iso::AssocRProd(a, b, c) => Ok((
            Type::prod(Type::prod(a.clone(), b.clone()), c.clone()),
            Type::prod(a.clone(), Type::prod(b.clone(), c.clone())),
        )),

        Iso::UniteProd(a) => Ok((Type::prod(Type::One, a.clone()), a.clone())),

        Iso::UnitiProd(a) => Ok((a.clone(), Type::prod(Type::One, a.clone()))),

        // ── Distributivity ───────────────────────────────────────
        Iso::Distrib(a, b, c) => Ok((
            Type::prod(a.clone(), Type::sum(b.clone(), c.clone())),
            Type::sum(
                Type::prod(a.clone(), b.clone()),
                Type::prod(a.clone(), c.clone()),
            ),
        )),

        Iso::Factor(a, b, c) => Ok((
            Type::sum(
                Type::prod(a.clone(), b.clone()),
                Type::prod(a.clone(), c.clone()),
            ),
            Type::prod(a.clone(), Type::sum(b.clone(), c.clone())),
        )),

        // ── Trace ────────────────────────────────────────────────
        Iso::Trace(u, f) => {
            let (input, output) = type_of(f)?;

            // input must be u + a
            let a = match &input {
                Type::Sum(u1, a) if u1.as_ref() == u => a.as_ref().clone(),
                Type::Sum(u1, _) => {
                    return Err(TypeError::TraceInputCarrier {
                        expected: u.clone(),
                        got: u1.as_ref().clone(),
                    });
                }
                _ => return Err(TypeError::TraceInputNotSum(input)),
            };

            // output must be u + b
            let b = match &output {
                Type::Sum(u2, b) if u2.as_ref() == u => b.as_ref().clone(),
                Type::Sum(u2, _) => {
                    return Err(TypeError::TraceOutputCarrier {
                        expected: u.clone(),
                        got: u2.as_ref().clone(),
                    });
                }
                _ => return Err(TypeError::TraceOutputNotSum(output)),
            };

            Ok((a, b))
        }

        // ── Iter ─────────────────────────────────────────────────
        // n numeral, f : a ↔ a  ⊢  iter n f : n × a ↔ n × a
        Iso::Iter(n, f) => {
            if !is_numeral_ty(n) {
                return Err(TypeError::IterCounterNotNumeral(n.clone()));
            }
            let (a, b) = type_of(f)?;
            if a != b {
                return Err(TypeError::IterBodyNotEndo { input: a, output: b });
            }
            let side = Type::prod(n.clone(), a);
            Ok((side.clone(), side))
        }

        // ── Prim ─────────────────────────────────────────────────
        Iso::Prim(def) => Ok((def.input_ty.clone(), def.output_ty.clone())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::iso::Iso;

    // Convenience type aliases
    fn one() -> Type {
        Type::One
    }
    fn bool_ty() -> Type {
        Type::sum(Type::One, Type::One)
    }

    #[test]
    fn type_swap_prod() {
        let a = one();
        let b = bool_ty();
        let (input, output) = type_of(&Iso::SwapProd(a.clone(), b.clone())).unwrap();
        assert_eq!(input, Type::prod(a.clone(), b.clone()));
        assert_eq!(output, Type::prod(b, a));
    }

    #[test]
    fn type_distrib() {
        let a = one();
        let b = one();
        let c = bool_ty();
        let (input, output) =
            type_of(&Iso::Distrib(a.clone(), b.clone(), c.clone())).unwrap();
        assert_eq!(input, Type::prod(a.clone(), Type::sum(b.clone(), c.clone())));
        assert_eq!(
            output,
            Type::sum(Type::prod(a.clone(), b), Type::prod(a, c))
        );
    }

    #[test]
    fn type_id_seq_swap() {
        // id ; swap⁺ : (1 + Bool) ↔ (Bool + 1)
        let a = one();
        let b = bool_ty();
        let sum_ab = Type::sum(a.clone(), b.clone());
        let term = Iso::seq(Iso::Id(sum_ab.clone()), Iso::SwapSum(a.clone(), b.clone()));
        let (input, output) = type_of(&term).unwrap();
        assert_eq!(input, sum_ab);
        assert_eq!(output, Type::sum(b, a));
    }

    #[test]
    fn reject_seq_mismatch() {
        // id(1) ; swap⁺(1, Bool) — output of id is 1, input of swap is 1+Bool
        let term = Iso::seq(Iso::Id(one()), Iso::SwapSum(one(), bool_ty()));
        let err = type_of(&term).unwrap_err();
        assert!(matches!(err, TypeError::SeqMismatch { .. }));
    }

    #[test]
    fn type_trace() {
        // trace(u, swap⁺(u, a)) : a ↔ u  where swap⁺ : u+a ↔ a+u
        // Actually we need f : u+a ↔ u+b. Let's use id(u+a): trace carrier u.
        let u = one();
        let a = bool_ty();
        let body = Iso::Id(Type::sum(u.clone(), a.clone()));
        let term = Iso::trace(u, body);
        let (input, output) = type_of(&term).unwrap();
        assert_eq!(input, a.clone());
        assert_eq!(output, a);
    }

    #[test]
    fn reject_trace_bad_carrier() {
        // trace(u=1, f) where f : Bool+a ↔ ...  — carrier mismatch
        let u = one();
        let a = one();
        // f : Bool + a ↔ Bool + a  (carrier is Bool, not 1)
        let body = Iso::Id(Type::sum(bool_ty(), a));
        let term = Iso::trace(u, body);
        assert!(type_of(&term).is_err());
    }

    #[test]
    fn type_unite_sum() {
        let a = bool_ty();
        let (input, output) = type_of(&Iso::UniteSum(a.clone())).unwrap();
        assert_eq!(input, Type::sum(Type::Zero, a.clone()));
        assert_eq!(output, a);
    }

    #[test]
    fn type_unite_prod() {
        let a = bool_ty();
        let (input, output) = type_of(&Iso::UniteProd(a.clone())).unwrap();
        assert_eq!(input, Type::prod(Type::One, a.clone()));
        assert_eq!(output, a);
    }

    #[test]
    fn type_factor() {
        let a = one();
        let b = one();
        let c = bool_ty();
        let (input, output) =
            type_of(&Iso::Factor(a.clone(), b.clone(), c.clone())).unwrap();
        assert_eq!(
            input,
            Type::sum(Type::prod(a.clone(), b.clone()), Type::prod(a.clone(), c.clone()))
        );
        assert_eq!(output, Type::prod(a, Type::sum(b, c)));
    }

    #[test]
    fn type_sym() {
        let a = one();
        let b = bool_ty();
        let swap = Iso::SwapSum(a.clone(), b.clone());
        let sym_swap = Iso::sym(swap);
        let (input, output) = type_of(&sym_swap).unwrap();
        // sym(swap⁺(1, 1+1)) : (1+1)+1 ↔ 1+(1+1)
        assert_eq!(input, Type::sum(b, a.clone()));
        assert_eq!(output, Type::sum(a, bool_ty()));
    }

    #[test]
    fn numeral_ty_shape() {
        assert_eq!(Type::numeral_ty(0), Type::Zero);
        assert_eq!(Type::numeral_ty(1), Type::sum(Type::One, Type::Zero));
        assert_eq!(
            Type::numeral_ty(2),
            Type::sum(Type::One, Type::sum(Type::One, Type::Zero)),
        );
    }

    #[test]
    fn is_numeral_ty_accepts_numerals_only() {
        for k in 0..6 {
            assert!(is_numeral_ty(&Type::numeral_ty(k)), "numeral_ty({k})");
        }
        // Bool = 1 + 1 ends in One, not Zero: not a numeral type.
        assert!(!is_numeral_ty(&bool_ty()));
        assert!(!is_numeral_ty(&Type::One));
        assert!(!is_numeral_ty(&Type::prod(Type::One, Type::Zero)));
        // Left summand must be exactly One.
        assert!(!is_numeral_ty(&Type::sum(bool_ty(), Type::Zero)));
    }

    #[test]
    fn type_iter() {
        // iter n (swap⁺ 1 1) : n × Bool ↔ n × Bool
        let n = Type::numeral_ty(4);
        let body = Iso::SwapSum(Type::One, Type::One); // Bool ↔ Bool
        let (input, output) = type_of(&Iso::iter(n.clone(), body)).unwrap();
        let expected = Type::prod(n, bool_ty());
        assert_eq!(input, expected);
        assert_eq!(output, expected);
    }

    #[test]
    fn reject_iter_counter_not_numeral() {
        // Bool is not a numeral type.
        let body = Iso::Id(Type::One);
        let err = type_of(&Iso::iter(bool_ty(), body)).unwrap_err();
        assert!(matches!(err, TypeError::IterCounterNotNumeral(_)));
    }

    #[test]
    fn reject_iter_body_not_endo() {
        // swap⁺(1, Bool) : 1 + Bool ↔ Bool + 1 — not an endo-iso.
        let n = Type::numeral_ty(2);
        let body = Iso::SwapSum(one(), bool_ty());
        let err = type_of(&Iso::iter(n, body)).unwrap_err();
        assert!(matches!(err, TypeError::IterBodyNotEndo { .. }));
    }

    #[test]
    fn type_sym_iter() {
        // sym (iter n f) has the same (endo) type as iter n f.
        let n = Type::numeral_ty(3);
        let body = Iso::SwapSum(Type::One, Type::One);
        let it = Iso::iter(n.clone(), body);
        let (input, output) = type_of(&Iso::sym(it)).unwrap();
        let expected = Type::prod(n, bool_ty());
        assert_eq!(input, expected);
        assert_eq!(output, expected);
    }

    #[test]
    fn type_plus() {
        // id(1) + id(Bool) : (1 + Bool) ↔ (1 + Bool)
        let f = Iso::Id(one());
        let g = Iso::Id(bool_ty());
        let (input, output) = type_of(&Iso::plus(f, g)).unwrap();
        assert_eq!(input, Type::sum(one(), bool_ty()));
        assert_eq!(output, Type::sum(one(), bool_ty()));
    }

    #[test]
    fn type_times() {
        // id(1) × swap⁺(1,1) : (1 × (1+1)) ↔ (1 × (1+1))
        let f = Iso::Id(one());
        let g = Iso::SwapSum(one(), one());
        let (input, output) = type_of(&Iso::times(f, g)).unwrap();
        assert_eq!(input, Type::prod(one(), bool_ty()));
        assert_eq!(output, Type::prod(one(), bool_ty()));
    }
}
