//! Type checker: `type_of(term) → Result<(Type, Type), TypeError>`.
//!
//! Implements the typing rules from `core-pi.md`. Every well-typed iso
//! `f` gets a type `a ↔ b`; ill-typed terms are rejected.

use crate::iso::Iso;
use crate::ty::Type;

/// Error from the type checker.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum TypeError {
    /// In `f ; g`, the output type of `f` doesn't match the input type of `g`.
    #[error(
        "seq: output {left_output} of the left iso does not match input {right_input} of the right"
    )]
    SeqMismatch {
        left_output: Type,
        right_input: Type,
    },
    /// In `trace(u, f)`, `f`'s domain is not a sum type.
    #[error("trace: body domain {0} is not a sum type")]
    TraceInputNotSum(Type),
    /// In `trace(u, f)`, `f`'s codomain is not a sum type.
    #[error("trace: body codomain {0} is not a sum type")]
    TraceOutputNotSum(Type),
    /// In `trace(u, f)`, the carrier in `f`'s domain ≠ declared `u`.
    #[error("trace: body domain carrier {got} does not match declared carrier {expected}")]
    TraceInputCarrier { expected: Type, got: Type },
    /// In `trace(u, f)`, the carrier in `f`'s codomain ≠ declared `u`.
    #[error("trace: body codomain carrier {got} does not match declared carrier {expected}")]
    TraceOutputCarrier { expected: Type, got: Type },
    /// In `iter(n, f)`, the counter type `n` is not a unary numeral
    /// type `1 + (1 + (... + 0))`.
    #[error("iter: counter type {0} is not a unary numeral type")]
    IterCounterNotNumeral(Type),
    /// In `iter(n, f)`, the body is not an endo-iso (`f : a ↔ a`).
    #[error("iter: body is not an endo-iso (input {input}, output {output})")]
    IterBodyNotEndo { input: Type, output: Type },
    /// In `fold(l, f)` / `unfold(l, f)`, the spine type `l` is not a
    /// list-spine type `1 + (b × (1 + (b × (... + 0))))`.
    #[error("fold: spine type {0} is not a list-spine type")]
    FoldSpineNotList(Type),
    /// In `fold(l, f)` / `unfold(l, f)`, the body is not an endo-iso
    /// (`f : b × c ↔ b × c`).
    #[error("fold: body is not an endo-iso (input {input}, output {output})")]
    FoldBodyNotEndo { input: Type, output: Type },
    /// In `fold(l, f)` / `unfold(l, f)`, the body's (endo) type is not
    /// a product `b × c`.
    #[error("fold: body type {0} is not a product")]
    FoldBodyNotElemProd(Type),
    /// In `fold(l, f)` / `unfold(l, f)`, the body's element component
    /// doesn't match the spine's element type.
    #[error("fold: body element type {got} does not match the spine element type {expected}")]
    FoldBodyElemMismatch { expected: Type, got: Type },
}

/// Is `ty` a unary numeral type `1 + (1 + (... + 0))`? (`0` itself is the
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

/// Is `ty` a list-spine type `1 + (b × (1 + (b × (... + 0))))` with a
/// UNIFORM element type `b`? (`0` itself is the degenerate, uninhabited
/// spine.) These are exactly the types bounded μ-unrolling (ADR-0035)
/// produces for `List b = Nil | Cons b List`, and the only admissible
/// `fold`/`unfold` spines (ADR-0037).
///
/// Returns:
/// - `None` -- not a list-spine type;
/// - `Some(None)` -- the degenerate spine `0` (no layers, so the element
///   type is unconstrained);
/// - `Some(Some(b))` -- a spine with ≥ 1 layer and element type `b`.
pub fn list_spine_elem_ty(ty: &Type) -> Option<Option<&Type>> {
    let mut elem: Option<&Type> = None;
    let mut cur = ty;
    loop {
        match cur {
            Type::Zero => return Some(elem),
            Type::Sum(l, r) if l.as_ref() == &Type::One => match r.as_ref() {
                Type::Prod(b, rest) => {
                    match elem {
                        Some(e) if e != b.as_ref() => return None,
                        _ => elem = Some(b.as_ref()),
                    }
                    cur = rest;
                }
                _ => return None,
            },
            _ => return None,
        }
    }
}

/// Shared typing rule for `fold`/`unfold` (ADR-0037):
/// `l` list-spine over `b`, `f : b × c ↔ b × c`  ⊢  `fold l f : l × c ↔ l × c`
/// (and identically for `unfold`).
fn fold_type_of(l: &Type, f: &Iso) -> Result<(Type, Type), TypeError> {
    let Some(elem) = list_spine_elem_ty(l) else {
        return Err(TypeError::FoldSpineNotList(l.clone()));
    };
    let (a, b) = type_of(f)?;
    if a != b {
        return Err(TypeError::FoldBodyNotEndo {
            input: a,
            output: b,
        });
    }
    let (body_elem, acc) = match &a {
        Type::Prod(x, c) => (x.as_ref(), c.as_ref().clone()),
        _ => return Err(TypeError::FoldBodyNotElemProd(a.clone())),
    };
    if let Some(e) = elem {
        if e != body_elem {
            return Err(TypeError::FoldBodyElemMismatch {
                expected: e.clone(),
                got: body_elem.clone(),
            });
        }
    }
    let side = Type::prod(l.clone(), acc);
    Ok((side.clone(), side))
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
                return Err(TypeError::IterBodyNotEndo {
                    input: a,
                    output: b,
                });
            }
            let side = Type::prod(n.clone(), a);
            Ok((side.clone(), side))
        }

        // ── Fold/Unfold ──────────────────────────────────────────
        // l list-spine over b, f : b × c ↔ b × c
        //   ⊢  fold l f : l × c ↔ l × c   (and unfold likewise)
        Iso::Fold(l, f) | Iso::Unfold(l, f) => fold_type_of(l, f),

        // ── Prim ─────────────────────────────────────────────────
        Iso::Prim(def) => Ok((def.input_ty.clone(), def.output_ty.clone())),
    }
}

/// The widest `Flat(n)` leaf appearing anywhere inside a [`Type`] tree.
///
/// Returns `Some(n)` for the maximum `Flat(n)` width reached by descending
/// `Sum`/`Prod`; `None` if the type contains no `Flat` leaf at all
/// (`Zero`/`One`/unary-numeral/list-spine-of-`One` shapes carry no data word).
fn type_max_flat_width(ty: &Type) -> Option<u8> {
    match ty {
        Type::Flat(n) => Some(*n),
        Type::Sum(a, b) | Type::Prod(a, b) => {
            match (type_max_flat_width(a), type_max_flat_width(b)) {
                (Some(x), Some(y)) => Some(x.max(y)),
                (Some(x), None) | (None, Some(x)) => Some(x),
                (None, None) => None,
            }
        }
        Type::Zero | Type::One => None,
    }
}

/// The widest data-word (`Type::Flat(n)`) the program manipulates -- the
/// per-program `w_data` derivation input (B-2.1c auto-derive).
///
/// The compiled `&[Instr]` stream is byte-oriented and has lost the source
/// data width, so the width must be read off the SOURCE `Iso`'s `Type`s. This
/// recurses the ENTIRE iso: the explicit `Type` parameters on every structural
/// combinator (`Id`/`SwapSum`/`AssocL/RSum`/`Unite/UnitiSum`/`SwapProd`/
/// `AssocL/RProd`/`Unite/UnitiProd`/`Distrib`/`Factor`/`Trace`/`Iter`/`Fold`/
/// `Unfold`), the input/output types of every `Prim`, and every sub-iso of
/// `Sym`/`Seq`/`Plus`/`Times`/`Trace`/`Iter`/`Fold`/`Unfold`. For each embedded
/// `Type` it takes the widest `Flat(n)` leaf ([`type_max_flat_width`]).
///
/// Returns the max `n` seen, clamped to `[1, 8]`. **Defaults to `8` when no
/// `Flat` leaf is found** -- this is the SAFE direction: it never under-reports
/// (a spuriously narrow `w_data` would misroute a wide data word -- a silent
/// corruption). A program whose widest value is `Flat(4)` returns `4`; a
/// byte (`Flat(8)`) program returns `8`; a mixed `Flat(4)` + `Flat(8)` program
/// returns `8`.
///
/// Coverage note: every `Type`-carrying constructor is explicitly matched (no
/// `_` fallthrough), so adding a new width-bearing `Iso`/`Type` variant is a
/// compile error here rather than a silent under-report.
pub fn max_flat_width(iso: &Iso) -> usize {
    /// Accumulate the widest `Flat` leaf over the whole iso, folding each
    /// embedded `Type` and recursing into sub-isos. `None` = no `Flat` seen.
    fn go(iso: &Iso, acc: Option<u8>) -> Option<u8> {
        // Fold in one embedded type's widest Flat leaf.
        fn fold_ty(acc: Option<u8>, ty: &Type) -> Option<u8> {
            match (acc, type_max_flat_width(ty)) {
                (Some(x), Some(y)) => Some(x.max(y)),
                (Some(x), None) | (None, Some(x)) => Some(x),
                (None, None) => None,
            }
        }
        match iso {
            // Structural combinators: fold in their explicit Type params.
            Iso::Id(a) => fold_ty(acc, a),
            Iso::SwapSum(a, b) | Iso::SwapProd(a, b) => fold_ty(fold_ty(acc, a), b),
            Iso::AssocLSum(a, b, c)
            | Iso::AssocRSum(a, b, c)
            | Iso::AssocLProd(a, b, c)
            | Iso::AssocRProd(a, b, c)
            | Iso::Distrib(a, b, c)
            | Iso::Factor(a, b, c) => fold_ty(fold_ty(fold_ty(acc, a), b), c),
            Iso::UniteSum(a) | Iso::UnitiSum(a) | Iso::UniteProd(a) | Iso::UnitiProd(a) => {
                fold_ty(acc, a)
            }

            // Composition: recurse into sub-isos.
            Iso::Sym(f) => go(f, acc),
            Iso::Seq(f, g) | Iso::Plus(f, g) | Iso::Times(f, g) => go(g, go(f, acc)),

            // Combinators carrying BOTH a Type param and a body iso.
            Iso::Trace(u, f) | Iso::Iter(u, f) | Iso::Fold(u, f) | Iso::Unfold(u, f) => {
                go(f, fold_ty(acc, u))
            }

            // Prim: fold in both its input and output types (authoritative;
            // any PrimSpec.width mirrors these).
            Iso::Prim(def) => fold_ty(fold_ty(acc, &def.input_ty), &def.output_ty),
        }
    }
    match go(iso, None) {
        Some(n) => (n as usize).clamp(1, 8),
        // No Flat leaf anywhere -- default to the full byte width (never
        // under-report; a narrow w_data would misroute wide data).
        None => 8,
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

    // ── max_flat_width pins (B-2.1c auto-derive) ─────────────────────

    #[test]
    fn max_flat_width_flat4_is_4() {
        // A program whose only data word is Flat(4) → 4.
        assert_eq!(max_flat_width(&Iso::Id(Type::Flat(4))), 4);
        assert_eq!(
            max_flat_width(&Iso::SwapProd(Type::Flat(4), Type::Flat(4))),
            4
        );
    }

    #[test]
    fn max_flat_width_flat8_is_8() {
        assert_eq!(max_flat_width(&Iso::Id(Type::Flat(8))), 8);
    }

    #[test]
    fn max_flat_width_mixed_is_max() {
        // Times(Flat(4), Flat(8)) → the wider leaf (8), NEVER under-report.
        let mixed = Iso::times(Iso::Id(Type::Flat(4)), Iso::Id(Type::Flat(8)));
        assert_eq!(max_flat_width(&mixed), 8);
        // Order-independent.
        let mixed2 = Iso::times(Iso::Id(Type::Flat(8)), Iso::Id(Type::Flat(4)));
        assert_eq!(max_flat_width(&mixed2), 8);
    }

    #[test]
    fn max_flat_width_wrapped_reaches_inner_flat() {
        // The Flat(4) is buried inside Times / Plus / Trace / Iter / Sym / Seq --
        // each wrapper must be descended, else the width is under-reported.
        let inner = || Iso::Id(Type::Flat(4));
        // Times: sub-iso descent.
        assert_eq!(max_flat_width(&Iso::times(inner(), Iso::Id(Type::One))), 4);
        // Plus: sub-iso descent.
        assert_eq!(max_flat_width(&Iso::plus(inner(), Iso::Id(Type::One))), 4);
        // Seq: both sub-isos (Id(Flat4);Id(Flat4)).
        assert_eq!(max_flat_width(&Iso::seq(inner(), inner())), 4);
        // Sym: sub-iso descent.
        assert_eq!(max_flat_width(&Iso::sym(inner())), 4);
        // Trace: carrier is One, but the body reaches Flat(4).
        //   body : One + Flat(4) ↔ One + Flat(4)  → trace over carrier One.
        let body = Iso::plus(Iso::Id(Type::One), inner());
        assert_eq!(max_flat_width(&Iso::trace(Type::One, body)), 4);
        // Iter: numeral counter (no Flat), body reaches Flat(4).
        assert_eq!(max_flat_width(&Iso::iter(Type::numeral_ty(3), inner())), 4);
        // Trace whose CARRIER itself is the wide word.
        assert_eq!(
            max_flat_width(&Iso::trace(
                Type::Flat(4),
                Iso::Id(Type::sum(Type::Flat(4), Type::One))
            )),
            4
        );
    }

    #[test]
    fn max_flat_width_no_flat_defaults_to_8() {
        // No Flat leaf anywhere → default 8 (never under-report).
        assert_eq!(max_flat_width(&Iso::Id(Type::One)), 8);
        assert_eq!(max_flat_width(&Iso::Id(bool_ty())), 8);
        assert_eq!(max_flat_width(&Iso::Id(Type::numeral_ty(5))), 8);
    }

    #[test]
    fn max_flat_width_reaches_prim_types() {
        // A Prim carries its data width in input_ty/output_ty. Build a trivial
        // Flat(4) → Flat(4) identity prim and confirm the width is read off it.
        use crate::prim::PrimDef;
        use std::collections::BTreeMap;
        let flat4 = Type::Flat(4);
        let vals = crate::prim::enumerate_values(&flat4);
        let mut fwd = BTreeMap::new();
        let mut bwd = BTreeMap::new();
        for v in &vals {
            fwd.insert(v.clone(), v.clone());
            bwd.insert(v.clone(), v.clone());
        }
        let def = PrimDef {
            forward_name: "id4".into(),
            backward_name: "id4".into(),
            input_ty: flat4.clone(),
            output_ty: flat4,
            forward: fwd,
            backward: bwd,
            symbolic: None,
        };
        assert_eq!(max_flat_width(&Iso::Prim(Box::new(def))), 4);
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
        let (input, output) = type_of(&Iso::Distrib(a.clone(), b.clone(), c.clone())).unwrap();
        assert_eq!(
            input,
            Type::prod(a.clone(), Type::sum(b.clone(), c.clone()))
        );
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
        // id(1) ; swap⁺(1, Bool) -- output of id is 1, input of swap is 1+Bool
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
        // trace(u=1, f) where f : Bool+a ↔ ...  -- carrier mismatch
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
        let (input, output) = type_of(&Iso::Factor(a.clone(), b.clone(), c.clone())).unwrap();
        assert_eq!(
            input,
            Type::sum(
                Type::prod(a.clone(), b.clone()),
                Type::prod(a.clone(), c.clone())
            )
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
        // swap⁺(1, Bool) : 1 + Bool ↔ Bool + 1 -- not an endo-iso.
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
    fn list_spine_ty_shape() {
        assert_eq!(Type::list_spine_ty(bool_ty(), 0), Type::Zero);
        assert_eq!(
            Type::list_spine_ty(bool_ty(), 1),
            Type::sum(Type::One, Type::prod(bool_ty(), Type::Zero)),
        );
        assert_eq!(
            Type::list_spine_ty(bool_ty(), 2),
            Type::sum(
                Type::One,
                Type::prod(
                    bool_ty(),
                    Type::sum(Type::One, Type::prod(bool_ty(), Type::Zero)),
                ),
            ),
        );
    }

    #[test]
    fn list_spine_elem_ty_accepts_uniform_spines_only() {
        // Degenerate spine: valid, no element constraint.
        assert_eq!(list_spine_elem_ty(&Type::Zero), Some(None));
        let b = bool_ty();
        for d in 1..5 {
            let l = Type::list_spine_ty(bool_ty(), d);
            assert_eq!(list_spine_elem_ty(&l), Some(Some(&b)), "depth {d}");
        }
        // Numeral types are NOT list spines (no element payload).
        assert_eq!(list_spine_elem_ty(&Type::numeral_ty(3)), None);
        // Bool is not a spine.
        assert_eq!(list_spine_elem_ty(&bool_ty()), None);
        assert_eq!(list_spine_elem_ty(&Type::One), None);
        // Non-uniform element types along the spine: rejected.
        let mixed = Type::sum(
            Type::One,
            Type::prod(
                bool_ty(),
                Type::sum(Type::One, Type::prod(Type::One, Type::Zero)),
            ),
        );
        assert_eq!(list_spine_elem_ty(&mixed), None);
    }

    #[test]
    fn list_spine_elem_ty_reports_elem() {
        let l = Type::list_spine_ty(bool_ty(), 3);
        let b = bool_ty();
        assert_eq!(list_spine_elem_ty(&l), Some(Some(&b)));
    }

    #[test]
    fn type_fold_and_unfold() {
        // fold l cx : l × Bool ↔ l × Bool for a body Bool × Bool ↔ Bool × Bool
        let l = Type::list_spine_ty(bool_ty(), 3);
        let body = Iso::SwapProd(bool_ty(), bool_ty());
        let expected = Type::prod(l.clone(), bool_ty());
        for iso in [Iso::fold(l.clone(), body.clone()), Iso::unfold(l, body)] {
            let (input, output) = type_of(&iso).unwrap();
            assert_eq!(input, expected);
            assert_eq!(output, expected);
        }
    }

    #[test]
    fn type_fold_degenerate_zero_spine() {
        // l = 0: vacuous domain, any b × c endo body is admissible.
        let body = Iso::SwapProd(bool_ty(), bool_ty());
        let (input, output) = type_of(&Iso::fold(Type::Zero, body)).unwrap();
        let expected = Type::prod(Type::Zero, bool_ty());
        assert_eq!(input, expected);
        assert_eq!(output, expected);
    }

    #[test]
    fn reject_fold_spine_not_list() {
        // A numeral type carries no element payload: not a fold spine.
        let body = Iso::SwapProd(bool_ty(), bool_ty());
        let err = type_of(&Iso::fold(Type::numeral_ty(3), body.clone())).unwrap_err();
        assert!(matches!(err, TypeError::FoldSpineNotList(_)));
        let err = type_of(&Iso::unfold(bool_ty(), body)).unwrap_err();
        assert!(matches!(err, TypeError::FoldSpineNotList(_)));
    }

    #[test]
    fn reject_fold_body_not_endo() {
        // swap⁺(1, Bool) : 1 + Bool ↔ Bool + 1 -- not an endo-iso.
        let l = Type::list_spine_ty(bool_ty(), 2);
        let body = Iso::SwapSum(one(), bool_ty());
        let err = type_of(&Iso::fold(l, body)).unwrap_err();
        assert!(matches!(err, TypeError::FoldBodyNotEndo { .. }));
    }

    #[test]
    fn reject_fold_body_not_elem_prod() {
        // id(Bool) is endo but its type is not a product b × c.
        let l = Type::list_spine_ty(bool_ty(), 2);
        let body = Iso::Id(bool_ty());
        let err = type_of(&Iso::fold(l, body)).unwrap_err();
        assert!(matches!(err, TypeError::FoldBodyNotElemProd(_)));
    }

    #[test]
    fn reject_fold_body_elem_mismatch() {
        // Spine over Bool, but the body consumes 1 × c.
        let l = Type::list_spine_ty(bool_ty(), 2);
        let body = Iso::Id(Type::prod(one(), bool_ty()));
        let err = type_of(&Iso::fold(l, body)).unwrap_err();
        assert!(matches!(err, TypeError::FoldBodyElemMismatch { .. }));
    }

    #[test]
    fn type_sym_fold() {
        // sym (fold l f) has the same (endo) type as fold l f.
        let l = Type::list_spine_ty(bool_ty(), 3);
        let body = Iso::SwapProd(bool_ty(), bool_ty());
        let (input, output) = type_of(&Iso::sym(Iso::fold(l.clone(), body))).unwrap();
        let expected = Type::prod(l, bool_ty());
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
