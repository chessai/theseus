//! Adjoint (`sym`, a syntactic involution).
//!
//! `sym f` denotes `f`'s inverse and carries no runtime cost: it is
//! computed by pushing `sym` to the leaves via the adjoint table from
//! `core-pi.md`. Write `f†` for the normal form of `sym f`.

use crate::iso::Iso;

/// Compute the adjoint `f†` of an iso term, following the table in
/// `core-pi.md`. This is a syntactic involution: `adjoint(adjoint(f)) ≡ f`
/// for every `Sym`-free term.
///
/// The key rules:
/// - `id† = id`
/// - `(sym f)† = f`                    (involution)
/// - `(f ; g)† = g† ; f†`              (reverses diagram order)
/// - `(f + g)† = f† + g†`
/// - `(f × g)† = f† × g†`
/// - `(trace f)† = trace (f†)`
/// - `(iter n f)† = iter n (f†)`      (counter-in-data, ADR-0036)
/// - `(fold l f)† = unfold l (f†)`    (mutually adjoint pair, ADR-0037)
/// - `(unfold l f)† = fold l (f†)`
/// - `(prim p)† = prim p̄`              (swap forward/backward)
/// - structural isos flip to their partner (see table)
pub fn adjoint(iso: &Iso) -> Iso {
    match iso {
        // ── Composition ──────────────────────────────────────────
        Iso::Id(a) => Iso::Id(a.clone()),

        // Involution: (sym f)† = f
        Iso::Sym(f) => f.as_ref().clone(),

        // (f ; g)† = g† ; f†
        Iso::Seq(f, g) => Iso::seq(adjoint(g), adjoint(f)),

        // (f + g)† = f† + g†
        Iso::Plus(f, g) => Iso::plus(adjoint(f), adjoint(g)),

        // (f × g)† = f† × g†
        Iso::Times(f, g) => Iso::times(adjoint(f), adjoint(g)),

        // ── Sum monoid: self-inverse swaps, partner pairs ────────
        // swap⁺† = swap⁺ (self-inverse, types flip)
        Iso::SwapSum(a, b) => Iso::SwapSum(b.clone(), a.clone()),

        // assocl⁺† = assocr⁺
        Iso::AssocLSum(a, b, c) => {
            Iso::AssocRSum(a.clone(), b.clone(), c.clone())
        }
        // assocr⁺† = assocl⁺
        Iso::AssocRSum(a, b, c) => {
            Iso::AssocLSum(a.clone(), b.clone(), c.clone())
        }

        // unite⁺† = uniti⁺
        Iso::UniteSum(a) => Iso::UnitiSum(a.clone()),
        // uniti⁺† = unite⁺
        Iso::UnitiSum(a) => Iso::UniteSum(a.clone()),

        // ── Product monoid ───────────────────────────────────────
        // swapˣ† = swapˣ (self-inverse, types flip)
        Iso::SwapProd(a, b) => Iso::SwapProd(b.clone(), a.clone()),

        // assoclˣ† = assocrˣ
        Iso::AssocLProd(a, b, c) => {
            Iso::AssocRProd(a.clone(), b.clone(), c.clone())
        }
        // assocrˣ† = assoclˣ
        Iso::AssocRProd(a, b, c) => {
            Iso::AssocLProd(a.clone(), b.clone(), c.clone())
        }

        // uniteˣ† = unitiˣ
        Iso::UniteProd(a) => Iso::UnitiProd(a.clone()),
        // unitiˣ† = uniteˣ
        Iso::UnitiProd(a) => Iso::UniteProd(a.clone()),

        // ── Distributivity ───────────────────────────────────────
        // distrib† = factor
        Iso::Distrib(a, b, c) => {
            Iso::Factor(a.clone(), b.clone(), c.clone())
        }
        // factor† = distrib
        Iso::Factor(a, b, c) => {
            Iso::Distrib(a.clone(), b.clone(), c.clone())
        }

        // ── Trace ────────────────────────────────────────────────
        // (trace f)† = trace (f†)
        Iso::Trace(u, f) => Iso::trace(u.clone(), adjoint(f)),

        // ── Iter ─────────────────────────────────────────────────
        // (iter n f)† = iter n (f†): the count k is preserved in the
        // data, so the inverse un-applies the body exactly k times.
        Iso::Iter(n, f) => Iso::iter(n.clone(), adjoint(f)),

        // ── Fold/Unfold ──────────────────────────────────────────
        // (fold l f)† = unfold l (f†): the spine is preserved in the
        // data, so the inverse un-threads the accumulator tail-to-head
        // with the inverted body. (unfold l f)† = fold l (f†) dually —
        // a mutually adjoint pair; adjoint∘adjoint is still identity.
        Iso::Fold(l, f) => Iso::unfold(l.clone(), adjoint(f)),
        Iso::Unfold(l, f) => Iso::fold(l.clone(), adjoint(f)),

        // ── Prim ─────────────────────────────────────────────────
        // (prim p)† = prim p̄
        Iso::Prim(def) => Iso::Prim(Box::new(def.adjoint())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prim::PrimDef;
    use crate::ty::Type;
    use std::collections::BTreeMap;

    fn one() -> Type {
        Type::One
    }
    fn bool_ty() -> Type {
        Type::sum(Type::One, Type::One)
    }

    fn make_test_prim() -> PrimDef {
        use crate::prim::Value;
        let tt = Value::left(Value::Unit);
        let ff = Value::right(Value::Unit);
        let mut fwd = BTreeMap::new();
        fwd.insert(tt.clone(), ff.clone());
        fwd.insert(ff.clone(), tt.clone());
        PrimDef {
            forward_name: "not".into(),
            backward_name: "not_inv".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd.clone(),
            backward: fwd,
            symbolic: None,
        }
    }

    /// Test that `adjoint(adjoint(f)) == f` for every constructor kind.
    #[test]
    fn involution_all_constructors() {
        let a = one();
        let b = bool_ty();
        let c = one();

        let terms: Vec<Iso> = vec![
            // Id
            Iso::Id(a.clone()),
            // Structural: sum monoid
            Iso::SwapSum(a.clone(), b.clone()),
            Iso::AssocLSum(a.clone(), b.clone(), c.clone()),
            Iso::AssocRSum(a.clone(), b.clone(), c.clone()),
            Iso::UniteSum(a.clone()),
            Iso::UnitiSum(a.clone()),
            // Structural: product monoid
            Iso::SwapProd(a.clone(), b.clone()),
            Iso::AssocLProd(a.clone(), b.clone(), c.clone()),
            Iso::AssocRProd(a.clone(), b.clone(), c.clone()),
            Iso::UniteProd(a.clone()),
            Iso::UnitiProd(a.clone()),
            // Distributivity
            Iso::Distrib(a.clone(), b.clone(), c.clone()),
            Iso::Factor(a.clone(), b.clone(), c.clone()),
            // Compositional: Seq
            Iso::seq(
                Iso::SwapSum(a.clone(), b.clone()),
                Iso::Id(Type::sum(b.clone(), a.clone())),
            ),
            // Compositional: Plus
            Iso::plus(Iso::Id(a.clone()), Iso::Id(b.clone())),
            // Compositional: Times
            Iso::times(Iso::Id(a.clone()), Iso::Id(b.clone())),
            // Trace
            Iso::trace(a.clone(), Iso::Id(Type::sum(a.clone(), b.clone()))),
            // Iter
            Iso::iter(Type::numeral_ty(3), Iso::SwapSum(a.clone(), a.clone())),
            // Fold / Unfold (bodies with nontrivial adjoints)
            Iso::fold(
                Type::list_spine_ty(b.clone(), 3),
                Iso::times(Iso::Id(b.clone()), Iso::UnitiProd(a.clone())),
            ),
            Iso::unfold(
                Type::list_spine_ty(b.clone(), 3),
                Iso::times(Iso::Id(b.clone()), Iso::UnitiProd(a.clone())),
            ),
            // Prim
            Iso::Prim(Box::new(make_test_prim())),
        ];

        for term in &terms {
            let double = adjoint(&adjoint(term));
            assert_eq!(
                &double, term,
                "involution failed for {term:?}"
            );
        }
    }

    /// `(sym f)† = f` — the involution cancellation rule.
    #[test]
    fn sym_cancellation() {
        let f = Iso::SwapSum(one(), bool_ty());
        let sym_f = Iso::sym(f.clone());
        assert_eq!(adjoint(&sym_f), f);
    }

    /// `(f ; g)† = g† ; f†` — composition reversal.
    #[test]
    fn seq_reversal() {
        let a = one();
        let b = bool_ty();
        let f = Iso::SwapSum(a.clone(), b.clone());
        let g = Iso::Id(Type::sum(b.clone(), a.clone()));

        let lhs = adjoint(&Iso::seq(f.clone(), g.clone()));
        let rhs = Iso::seq(adjoint(&g), adjoint(&f));
        assert_eq!(lhs, rhs);
    }

    /// `(f + g)† = f† + g†`
    #[test]
    fn plus_adjoint() {
        let f = Iso::SwapSum(one(), bool_ty());
        let g = Iso::Id(one());

        let lhs = adjoint(&Iso::plus(f.clone(), g.clone()));
        let rhs = Iso::plus(adjoint(&f), adjoint(&g));
        assert_eq!(lhs, rhs);
    }

    /// `(f × g)† = f† × g†`
    #[test]
    fn times_adjoint() {
        let f = Iso::SwapProd(one(), bool_ty());
        let g = Iso::Id(one());

        let lhs = adjoint(&Iso::times(f.clone(), g.clone()));
        let rhs = Iso::times(adjoint(&f), adjoint(&g));
        assert_eq!(lhs, rhs);
    }

    /// `(trace f)† = trace (f†)`
    #[test]
    fn trace_adjoint() {
        let u = one();
        let body = Iso::SwapSum(u.clone(), bool_ty());

        let lhs = adjoint(&Iso::trace(u.clone(), body.clone()));
        let rhs = Iso::trace(u, adjoint(&body));
        assert_eq!(lhs, rhs);
    }

    /// `(iter n f)† = iter n (f†)` — the counter type is preserved, the
    /// body is inverted.
    #[test]
    fn iter_adjoint() {
        let n = Type::numeral_ty(4);
        // Body with a nontrivial adjoint: assocl⁺.
        let body = Iso::AssocLSum(one(), one(), one());

        let lhs = adjoint(&Iso::iter(n.clone(), body.clone()));
        let rhs = Iso::iter(n, adjoint(&body));
        assert_eq!(lhs, rhs);
    }

    /// `(fold l f)† = unfold l (f†)` and `(unfold l f)† = fold l (f†)` —
    /// the mutually adjoint pair; the spine type is preserved, the body
    /// is inverted, and the direction flips.
    #[test]
    fn fold_unfold_adjoint_pair() {
        let l = Type::list_spine_ty(bool_ty(), 4);
        // Body with a nontrivial adjoint: Bool × a ↔ Bool × (1 × a).
        let body = Iso::times(Iso::Id(bool_ty()), Iso::UnitiProd(one()));

        assert_eq!(
            adjoint(&Iso::fold(l.clone(), body.clone())),
            Iso::unfold(l.clone(), adjoint(&body)),
        );
        assert_eq!(
            adjoint(&Iso::unfold(l.clone(), body.clone())),
            Iso::fold(l, adjoint(&body)),
        );
    }

    /// Each structural adjoint partner pair.
    #[test]
    fn adjoint_table_partners() {
        let a = one();
        let b = bool_ty();
        let c = one();

        // assocl⁺† = assocr⁺
        assert_eq!(
            adjoint(&Iso::AssocLSum(a.clone(), b.clone(), c.clone())),
            Iso::AssocRSum(a.clone(), b.clone(), c.clone()),
        );
        // assocr⁺† = assocl⁺
        assert_eq!(
            adjoint(&Iso::AssocRSum(a.clone(), b.clone(), c.clone())),
            Iso::AssocLSum(a.clone(), b.clone(), c.clone()),
        );

        // assoclˣ† = assocrˣ
        assert_eq!(
            adjoint(&Iso::AssocLProd(a.clone(), b.clone(), c.clone())),
            Iso::AssocRProd(a.clone(), b.clone(), c.clone()),
        );
        // assocrˣ† = assoclˣ
        assert_eq!(
            adjoint(&Iso::AssocRProd(a.clone(), b.clone(), c.clone())),
            Iso::AssocLProd(a.clone(), b.clone(), c.clone()),
        );

        // unite⁺† = uniti⁺
        assert_eq!(
            adjoint(&Iso::UniteSum(a.clone())),
            Iso::UnitiSum(a.clone()),
        );
        // uniti⁺† = unite⁺
        assert_eq!(
            adjoint(&Iso::UnitiSum(a.clone())),
            Iso::UniteSum(a.clone()),
        );

        // uniteˣ† = unitiˣ
        assert_eq!(
            adjoint(&Iso::UniteProd(a.clone())),
            Iso::UnitiProd(a.clone()),
        );
        // unitiˣ† = uniteˣ
        assert_eq!(
            adjoint(&Iso::UnitiProd(a.clone())),
            Iso::UniteProd(a.clone()),
        );

        // distrib† = factor
        assert_eq!(
            adjoint(&Iso::Distrib(a.clone(), b.clone(), c.clone())),
            Iso::Factor(a.clone(), b.clone(), c.clone()),
        );
        // factor† = distrib
        assert_eq!(
            adjoint(&Iso::Factor(a.clone(), b.clone(), c.clone())),
            Iso::Distrib(a.clone(), b.clone(), c.clone()),
        );
    }

    /// swap⁺ and swapˣ are self-inverse (types flip).
    #[test]
    fn swap_self_inverse() {
        let a = one();
        let b = bool_ty();

        // swap⁺(a,b)† = swap⁺(b,a)
        assert_eq!(
            adjoint(&Iso::SwapSum(a.clone(), b.clone())),
            Iso::SwapSum(b.clone(), a.clone()),
        );
        // swapˣ(a,b)† = swapˣ(b,a)
        assert_eq!(
            adjoint(&Iso::SwapProd(a.clone(), b.clone())),
            Iso::SwapProd(b, a),
        );
    }

    /// Prim adjoint swaps forward/backward.
    #[test]
    fn prim_adjoint() {
        let def = make_test_prim();
        let adj = adjoint(&Iso::Prim(Box::new(def.clone())));
        assert_eq!(adj, Iso::Prim(Box::new(def.adjoint())));
    }
}
