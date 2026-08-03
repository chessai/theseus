//! Fuel-based interpreter over core Π -- stage one of the oracle chain.
//!
//! - Partiality per join-inverse-category semantics (ADR-0005): fuel in
//!   software, valid/done handshakes in hardware.
//! - The interpreter is the semantic root: typed-sim, bit-sim, and
//!   Verilator are all differentially tested against it (ADR-0008/0018).
//! - Powers the REPL's `:step`/`:unstep` -- the software preview of the
//!   FPGA time-travel demo (workspace plan, ADR-0016 provenance).

pub mod enumerate;
pub mod interp;
pub mod value;

pub use enumerate::{cardinality, enumerate};
pub use interp::{eval, static_bound, EvalError};
pub use value::{inhabits, Value};

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use theseus_core::{type_of, Iso, PrimDef, Type};

    const FUEL: u64 = 10_000;

    fn run(iso: &Iso, v: Value) -> Result<Value, EvalError> {
        let mut fuel = FUEL;
        eval(iso, v, &mut fuel)
    }

    /// Assert the adjoint-commutation / round-trip property:
    /// for every `v` in `⟦input⟧`, `eval(f†, eval(f, v)) == v`
    /// for every `w` in `⟦output⟧`, `eval(f, eval(f†, w)) == w`
    fn assert_round_trip(iso: &Iso) {
        let (in_ty, out_ty) = type_of(iso).expect("iso should type-check");
        let adj = Iso::sym(iso.clone());

        // Forward then backward
        for v in enumerate(&in_ty) {
            let fwd = run(iso, v.clone())
                .unwrap_or_else(|e| panic!("forward eval failed on {v:?}: {e:?}"));
            assert!(
                inhabits(&fwd, &out_ty),
                "forward result {fwd:?} does not inhabit output type {out_ty:?}"
            );
            let round = run(&adj, fwd.clone())
                .unwrap_or_else(|e| panic!("backward eval failed on {fwd:?}: {e:?}"));
            assert_eq!(round, v, "round-trip f†(f(v)) != v");
        }

        // Backward then forward
        for w in enumerate(&out_ty) {
            let back = run(&adj, w.clone())
                .unwrap_or_else(|e| panic!("backward eval failed on {w:?}: {e:?}"));
            assert!(
                inhabits(&back, &in_ty),
                "backward result {back:?} does not inhabit input type {in_ty:?}"
            );
            let round = run(iso, back.clone())
                .unwrap_or_else(|e| panic!("forward eval failed on {back:?}: {e:?}"));
            assert_eq!(round, w, "round-trip f(f†(w)) != w");
        }
    }

    fn bool_ty() -> Type {
        Type::sum(Type::One, Type::One)
    }

    fn three_ty() -> Type {
        Type::sum(Type::One, Type::sum(Type::One, Type::One))
    }

    // ─── Adjoint-commutation round-trip tests ───────────────────────

    #[test]
    fn round_trip_id() {
        assert_round_trip(&Iso::Id(bool_ty()));
    }

    #[test]
    fn round_trip_swap_sum_bool() {
        assert_round_trip(&Iso::SwapSum(Type::One, Type::One));
    }

    #[test]
    fn round_trip_swap_sum_asymmetric() {
        assert_round_trip(&Iso::SwapSum(Type::One, bool_ty()));
    }

    #[test]
    fn round_trip_swap_prod() {
        assert_round_trip(&Iso::SwapProd(Type::One, bool_ty()));
    }

    #[test]
    fn round_trip_swap_prod_larger() {
        assert_round_trip(&Iso::SwapProd(bool_ty(), three_ty()));
    }

    #[test]
    fn round_trip_assocl_sum() {
        assert_round_trip(&Iso::AssocLSum(Type::One, Type::One, Type::One));
    }

    #[test]
    fn round_trip_assocr_sum() {
        assert_round_trip(&Iso::AssocRSum(Type::One, Type::One, Type::One));
    }

    #[test]
    fn round_trip_assocl_sum_larger() {
        assert_round_trip(&Iso::AssocLSum(Type::One, Type::One, bool_ty()));
    }

    #[test]
    fn round_trip_assocl_prod() {
        assert_round_trip(&Iso::AssocLProd(Type::One, Type::One, Type::One));
    }

    #[test]
    fn round_trip_assocr_prod() {
        assert_round_trip(&Iso::AssocRProd(Type::One, Type::One, Type::One));
    }

    #[test]
    fn round_trip_assocr_prod_larger() {
        assert_round_trip(&Iso::AssocRProd(bool_ty(), Type::One, Type::One));
    }

    #[test]
    fn round_trip_unite_sum() {
        assert_round_trip(&Iso::UniteSum(Type::One));
    }

    #[test]
    fn round_trip_unite_sum_bool() {
        assert_round_trip(&Iso::UniteSum(bool_ty()));
    }

    #[test]
    fn round_trip_uniti_sum() {
        assert_round_trip(&Iso::UnitiSum(Type::One));
    }

    #[test]
    fn round_trip_uniti_sum_bool() {
        assert_round_trip(&Iso::UnitiSum(bool_ty()));
    }

    #[test]
    fn round_trip_unite_prod() {
        assert_round_trip(&Iso::UniteProd(Type::One));
    }

    #[test]
    fn round_trip_unite_prod_bool() {
        assert_round_trip(&Iso::UniteProd(bool_ty()));
    }

    #[test]
    fn round_trip_uniti_prod() {
        assert_round_trip(&Iso::UnitiProd(Type::One));
    }

    #[test]
    fn round_trip_uniti_prod_bool() {
        assert_round_trip(&Iso::UnitiProd(bool_ty()));
    }

    #[test]
    fn round_trip_distrib() {
        assert_round_trip(&Iso::Distrib(Type::One, Type::One, Type::One));
    }

    #[test]
    fn round_trip_distrib_larger() {
        assert_round_trip(&Iso::Distrib(bool_ty(), Type::One, Type::One));
    }

    #[test]
    fn round_trip_distrib_full() {
        assert_round_trip(&Iso::Distrib(bool_ty(), bool_ty(), Type::One));
    }

    #[test]
    fn round_trip_factor() {
        assert_round_trip(&Iso::Factor(Type::One, Type::One, Type::One));
    }

    #[test]
    fn round_trip_factor_larger() {
        assert_round_trip(&Iso::Factor(bool_ty(), Type::One, Type::One));
    }

    #[test]
    fn round_trip_seq_swap_swap() {
        // swap ; swap = id (self-inverse)
        let iso = Iso::seq(
            Iso::SwapSum(Type::One, Type::One),
            Iso::SwapSum(Type::One, Type::One),
        );
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_seq_id_swap() {
        let iso = Iso::seq(
            Iso::Id(bool_ty()),
            Iso::SwapSum(Type::One, Type::One),
        );
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_plus() {
        let f = Iso::SwapProd(Type::One, Type::One);
        let g = Iso::Id(Type::One);
        let iso = Iso::plus(f, g);
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_times() {
        let f = Iso::SwapSum(Type::One, Type::One);
        let g = Iso::Id(Type::One);
        let iso = Iso::times(f, g);
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_sym_swap() {
        let iso = Iso::sym(Iso::SwapSum(Type::One, bool_ty()));
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_sym_assocl() {
        let iso = Iso::sym(Iso::AssocLSum(Type::One, Type::One, Type::One));
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_prim_not() {
        let bool_t = bool_ty();
        let tt = Value::left(Value::Unit);
        let ff = Value::right(Value::Unit);
        let mut fwd = BTreeMap::new();
        fwd.insert(tt.clone(), ff.clone());
        fwd.insert(ff.clone(), tt.clone());
        let mut bwd = BTreeMap::new();
        bwd.insert(ff.clone(), tt.clone());
        bwd.insert(tt.clone(), ff.clone());
        let def = PrimDef {
            forward_name: "not".into(),
            backward_name: "not_inv".into(),
            input_ty: bool_t.clone(),
            output_ty: bool_t,
            forward: fwd,
            backward: bwd,
            symbolic: None,
        };
        let iso = Iso::Prim(Box::new(def));
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_complex_distrib_swap() {
        // distrib ; (swapˣ + swapˣ) on 1 × (1 + 1)
        let a = Type::One;
        let b = Type::One;
        let c = Type::One;
        let dist = Iso::Distrib(a.clone(), b.clone(), c.clone());
        let sw1 = Iso::SwapProd(a.clone(), b);
        let sw2 = Iso::SwapProd(a, c);
        let plus = Iso::plus(sw1, sw2);
        let iso = Iso::seq(dist, plus);
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_nested_composition() {
        // unitiˣ ; uniteˣ : Bool ↔ Bool (round-trip through 1×Bool)
        let iso = Iso::seq(Iso::UnitiProd(bool_ty()), Iso::UniteProd(bool_ty()));
        assert_round_trip(&iso);
    }

    #[test]
    fn round_trip_deep_seq() {
        // id ; id ; id ; swap⁺
        let iso = Iso::seq(
            Iso::seq(
                Iso::seq(Iso::Id(bool_ty()), Iso::Id(bool_ty())),
                Iso::Id(bool_ty()),
            ),
            Iso::SwapSum(Type::One, Type::One),
        );
        assert_round_trip(&iso);
    }

    // ─── Iter: differential oracle over the full swept domain ───────

    /// The numeral value `inrⁿ(inl ())` (encoding per ADR-0035/0036).
    fn numeral(n: usize) -> Value {
        let mut v = Value::left(Value::Unit);
        for _ in 0..n {
            v = Value::right(v);
        }
        v
    }

    /// `rot3 : Three ↔ Three`, the successor mod 3 -- a successor-like,
    /// NOT self-inverse body, so `iterᵏ` distinguishes every k mod 3.
    fn rot3() -> Iso {
        let t0 = Value::left(Value::Unit);
        let t1 = Value::right(Value::left(Value::Unit));
        let t2 = Value::right(Value::right(Value::Unit));

        let mut fwd = BTreeMap::new();
        fwd.insert(t0.clone(), t1.clone());
        fwd.insert(t1.clone(), t2.clone());
        fwd.insert(t2.clone(), t0.clone());
        let mut bwd = BTreeMap::new();
        bwd.insert(t1.clone(), t0.clone());
        bwd.insert(t2.clone(), t1);
        bwd.insert(t0, t2);

        Iso::Prim(Box::new(PrimDef {
            forward_name: "rot3".into(),
            backward_name: "rot3_inv".into(),
            input_ty: three_ty(),
            output_ty: three_ty(),
            forward: fwd,
            backward: bwd,
            symbolic: None,
        }))
    }

    /// Round-trip oracle for `iter`: sweeps the ENTIRE domain
    /// `N₆ × Three` (via `enumerate`) forward-then-backward and
    /// backward-then-forward: `eval(sym f, eval(f, v)) == v`.
    #[test]
    fn round_trip_iter_rot3() {
        assert_round_trip(&Iso::iter(Type::numeral_ty(6), rot3()));
    }

    /// Same oracle with a self-inverse structural body.
    #[test]
    fn round_trip_iter_swap_sum() {
        let body = Iso::SwapSum(Type::One, Type::One); // not : Bool ↔ Bool
        assert_round_trip(&Iso::iter(Type::numeral_ty(5), body));
    }

    /// Depth-generic law: `iter n f ⟨k, x⟩ == ⟨k, fᵏ(x)⟩` -- the n-fold
    /// composition oracle, swept over every `k` in `N₆` and every `x`
    /// in `Three`.
    #[test]
    fn iter_equals_nfold_body_composition() {
        let body = rot3();
        let iso = Iso::iter(Type::numeral_ty(6), body.clone());
        for k in 0..6usize {
            for x in enumerate(&three_ty()) {
                // n-fold composition computed by k separate body evals.
                let mut expected = x.clone();
                for _ in 0..k {
                    expected = run(&body, expected).unwrap();
                }
                let got = run(&iso, Value::pair(numeral(k), x.clone())).unwrap();
                assert_eq!(
                    got,
                    Value::pair(numeral(k), expected),
                    "iter^{k} != {k}-fold composition at x={x:?}",
                );
            }
        }
    }

    /// Backward n-fold law: `sym(iter n f) ⟨k, y⟩ == ⟨k, (f†)ᵏ(y)⟩`.
    #[test]
    fn sym_iter_equals_nfold_inverse_composition() {
        let body = rot3();
        let inv_body = Iso::sym(body.clone());
        let iso = Iso::sym(Iso::iter(Type::numeral_ty(6), body));
        for k in 0..6usize {
            for y in enumerate(&three_ty()) {
                let mut expected = y.clone();
                for _ in 0..k {
                    expected = run(&inv_body, expected).unwrap();
                }
                let got = run(&iso, Value::pair(numeral(k), y.clone())).unwrap();
                assert_eq!(
                    got,
                    Value::pair(numeral(k), expected),
                    "sym(iter)^{k} != {k}-fold inverse composition at y={y:?}",
                );
            }
        }
    }

    /// `iter` composes with the rest of the calculus: round-trip a
    /// sequence that plumbs the pair through swapˣ on both sides.
    #[test]
    fn round_trip_iter_inside_composition() {
        let n = Type::numeral_ty(4);
        let iso = Iso::seq(
            Iso::seq(
                Iso::SwapProd(three_ty(), n.clone()),
                Iso::iter(n.clone(), rot3()),
            ),
            Iso::SwapProd(n, three_ty()),
        );
        assert_round_trip(&iso);
    }

    // ─── Fold/Unfold: differential oracle over the full swept domain ─

    fn bit_list_ty(depth: usize) -> Type {
        Type::list_spine_ty(bool_ty(), depth)
    }

    /// Build a list-spine value: `nil = inl ()`, `cons h t = inr ⟨h, t⟩`.
    fn spine(elems: &[Value]) -> Value {
        let mut v = Value::left(Value::Unit);
        for h in elems.iter().rev() {
            v = Value::right(Value::pair(h.clone(), v));
        }
        v
    }

    /// Decompose a list-spine value into its elements (test-local dual
    /// of `spine`).
    fn spine_elems(v: &Value) -> Vec<Value> {
        let mut elems = Vec::new();
        let mut cur = v;
        loop {
            match cur {
                Value::Left(u) if u.as_ref() == &Value::Unit => return elems,
                Value::Right(cons) => match cons.as_ref() {
                    Value::Pair(h, t) => {
                        elems.push(h.as_ref().clone());
                        cur = t;
                    }
                    other => panic!("not a spine value: {other:?}"),
                },
                other => panic!("not a spine value: {other:?}"),
            }
        }
    }

    /// `cx : Bit × Bit ↔ Bit × Bit` (CNOT, self-inverse): ⟨x, a⟩ ↦
    /// ⟨x, a⊕x⟩. Folding it is the reversible "sum": parity of the list
    /// accumulates while the list itself is preserved.
    fn cx() -> Iso {
        let o = Value::left(Value::Unit);
        let i = Value::right(Value::Unit);
        let mut fwd = BTreeMap::new();
        for x in [o.clone(), i.clone()] {
            for a in [o.clone(), i.clone()] {
                let a2 = if x == i {
                    if a == o { i.clone() } else { o.clone() }
                } else {
                    a.clone()
                };
                fwd.insert(Value::pair(x.clone(), a), Value::pair(x.clone(), a2));
            }
        }
        let ty = Type::prod(bool_ty(), bool_ty());
        Iso::Prim(Box::new(PrimDef {
            forward_name: "cx".into(),
            backward_name: "cx".into(),
            input_ty: ty.clone(),
            output_ty: ty,
            forward: fwd.clone(),
            backward: fwd,
            symbolic: None,
        }))
    }

    /// A NON-self-inverse body: `cx ; swapˣ : Bit × Bit ↔ Bit × Bit`,
    /// ⟨x, a⟩ ↦ ⟨a⊕x, x⟩ -- order- and direction-sensitive, so it
    /// distinguishes head-to-tail from tail-to-head threading.
    fn cx_swap() -> Iso {
        Iso::seq(cx(), Iso::SwapProd(bool_ty(), bool_ty()))
    }

    /// Round-trip oracle for `fold`: sweeps the ENTIRE domain
    /// `L₄(Bit) × Bit` (via `enumerate`) forward-then-backward and
    /// backward-then-forward: `eval(sym f, eval(f, v)) == v`.
    #[test]
    fn round_trip_fold_parity_cx() {
        assert_round_trip(&Iso::fold(bit_list_ty(4), cx()));
    }

    /// Same oracle for `unfold`.
    #[test]
    fn round_trip_unfold_parity_cx() {
        assert_round_trip(&Iso::unfold(bit_list_ty(4), cx()));
    }

    /// Round-trip with the non-self-inverse, order-sensitive body --
    /// the strongest exerciser of the fold/unfold adjoint pairing.
    #[test]
    fn round_trip_fold_nonselfinverse_body() {
        assert_round_trip(&Iso::fold(bit_list_ty(4), cx_swap()));
        assert_round_trip(&Iso::unfold(bit_list_ty(4), cx_swap()));
    }

    /// `fold(f) ; unfold(f†) = id` over the WHOLE domain (and hence
    /// `sym(fold f) = unfold f†` pointwise on the forward image).
    #[test]
    fn fold_then_unfold_of_adjoint_is_identity() {
        let l = bit_list_ty(4);
        let f = cx_swap();
        let iso = Iso::seq(
            Iso::fold(l.clone(), f.clone()),
            Iso::unfold(l.clone(), theseus_core::adjoint(&f)),
        );
        for v in enumerate(&Type::prod(l.clone(), bool_ty())) {
            assert_eq!(run(&iso, v.clone()).unwrap(), v, "fold;unfold† != id at {v:?}");
        }
    }

    /// `unfold(f) ; fold(f†) = id` over the WHOLE domain.
    #[test]
    fn unfold_then_fold_of_adjoint_is_identity() {
        let l = bit_list_ty(4);
        let f = cx_swap();
        let iso = Iso::seq(
            Iso::unfold(l.clone(), f.clone()),
            Iso::fold(l.clone(), theseus_core::adjoint(&f)),
        );
        for v in enumerate(&Type::prod(l.clone(), bool_ty())) {
            assert_eq!(run(&iso, v.clone()).unwrap(), v, "unfold;fold† != id at {v:?}");
        }
    }

    /// Semantic adjoint-pair law: `eval(sym(fold l f), v) ==
    /// eval(unfold l (f†), v)` pointwise over the whole domain (and the
    /// symmetric law for unfold).
    #[test]
    fn sym_fold_equals_unfold_of_adjoint_body() {
        let l = bit_list_ty(4);
        let f = cx_swap();
        let dom = Type::prod(l.clone(), bool_ty());
        let sym_fold = Iso::sym(Iso::fold(l.clone(), f.clone()));
        let unfold_adj = Iso::unfold(l.clone(), theseus_core::adjoint(&f));
        let sym_unfold = Iso::sym(Iso::unfold(l.clone(), f.clone()));
        let fold_adj = Iso::fold(l.clone(), theseus_core::adjoint(&f));
        for v in enumerate(&dom) {
            assert_eq!(
                run(&sym_fold, v.clone()).unwrap(),
                run(&unfold_adj, v.clone()).unwrap(),
                "sym(fold) != unfold(f†) at {v:?}",
            );
            assert_eq!(
                run(&sym_unfold, v.clone()).unwrap(),
                run(&fold_adj, v.clone()).unwrap(),
                "sym(unfold) != fold(f†) at {v:?}",
            );
        }
    }

    /// Structure-driven law: `fold l f` equals the manual head-to-tail
    /// accumulation (reversible mapAccumL), swept over every list in
    /// `L₄(Bit)` and every accumulator in `Bit`.
    #[test]
    fn fold_equals_manual_left_accumulation() {
        let l = bit_list_ty(4);
        let f = cx_swap();
        let iso = Iso::fold(l.clone(), f.clone());
        for xs in enumerate(&l) {
            for c in enumerate(&bool_ty()) {
                let mut acc = c.clone();
                let mut out = Vec::new();
                for h in spine_elems(&xs) {
                    match run(&f, Value::pair(h, acc)).unwrap() {
                        Value::Pair(y, a2) => {
                            out.push(*y);
                            acc = *a2;
                        }
                        other => panic!("body returned non-pair: {other:?}"),
                    }
                }
                let got = run(&iso, Value::pair(xs.clone(), c.clone())).unwrap();
                assert_eq!(
                    got,
                    Value::pair(spine(&out), acc),
                    "fold != manual mapAccumL at xs={xs:?}, c={c:?}",
                );
            }
        }
    }

    /// Dual law: `unfold l f` equals the manual tail-to-head
    /// accumulation over the whole domain.
    #[test]
    fn unfold_equals_manual_right_accumulation() {
        let l = bit_list_ty(4);
        let f = cx_swap();
        let iso = Iso::unfold(l.clone(), f.clone());
        for xs in enumerate(&l) {
            for c in enumerate(&bool_ty()) {
                let mut acc = c.clone();
                let mut out = Vec::new();
                for h in spine_elems(&xs).into_iter().rev() {
                    match run(&f, Value::pair(h, acc)).unwrap() {
                        Value::Pair(y, a2) => {
                            out.push(*y);
                            acc = *a2;
                        }
                        other => panic!("body returned non-pair: {other:?}"),
                    }
                }
                out.reverse();
                let got = run(&iso, Value::pair(xs.clone(), c.clone())).unwrap();
                assert_eq!(
                    got,
                    Value::pair(spine(&out), acc),
                    "unfold != manual reverse mapAccum at xs={xs:?}, c={c:?}",
                );
            }
        }
    }

    /// Length-style structural consumption: fold `id × rot3` counts the
    /// list length (mod 3) into a `Three` accumulator, preserving the
    /// list -- swept over the whole domain and round-tripped.
    #[test]
    fn fold_length_mod3_into_accumulator() {
        let l = bit_list_ty(4);
        let body = Iso::times(Iso::Id(bool_ty()), rot3());
        let iso = Iso::fold(l.clone(), body);
        for xs in enumerate(&l) {
            let len = spine_elems(&xs).len();
            for c in enumerate(&three_ty()) {
                // Expected: rot3 applied `len` times to c.
                let mut expected = c.clone();
                for _ in 0..len {
                    expected = run(&rot3(), expected).unwrap();
                }
                let got = run(&iso, Value::pair(xs.clone(), c.clone())).unwrap();
                assert_eq!(got, Value::pair(xs.clone(), expected), "length-fold at {xs:?}");
            }
        }
        assert_round_trip(&iso);
    }

    /// `fold` composes with the rest of the calculus: round-trip a
    /// sequence that plumbs the pair through swapˣ on both sides.
    #[test]
    fn round_trip_fold_inside_composition() {
        let l = bit_list_ty(3);
        let iso = Iso::seq(
            Iso::seq(
                Iso::SwapProd(bool_ty(), l.clone()),
                Iso::fold(l.clone(), cx_swap()),
            ),
            Iso::SwapProd(l, bool_ty()),
        );
        assert_round_trip(&iso);
    }

    // ─── Enumerator integration: count and completeness ─────────────

    #[test]
    fn enumerator_covers_all_assocl_inputs() {
        // Verify that enumerate produces values the interpreter accepts
        let ty = Type::sum(Type::One, Type::sum(Type::One, Type::One));
        let iso = Iso::AssocLSum(Type::One, Type::One, Type::One);
        for v in enumerate(&ty) {
            run(&iso, v).expect("AssocLSum should accept all enumerated values");
        }
    }

    #[test]
    fn enumerator_covers_all_distrib_inputs() {
        let ty = Type::prod(bool_ty(), bool_ty());
        let iso = Iso::Distrib(bool_ty(), Type::One, Type::One);
        for v in enumerate(&ty) {
            run(&iso, v).expect("Distrib should accept all enumerated values");
        }
    }

    // ─── Flat type: inhabits + round-trip tests ────────────────────────

    #[test]
    fn inhabits_flat_values() {
        for n in [1u8, 4, 8] {
            let ty = Type::Flat(n);
            let card = 1usize << n;
            // Every thermometer Right^k(Left(Unit)) for k < 2^n inhabits Flat(n).
            for k in 0..card {
                let mut v = Value::left(Value::Unit);
                for _ in 0..k {
                    v = Value::right(v);
                }
                assert!(
                    inhabits(&v, &ty),
                    "thermo({k}) should inhabit Flat({n})"
                );
            }
            // One past the end does NOT inhabit.
            let mut v = Value::left(Value::Unit);
            for _ in 0..card {
                v = Value::right(v);
            }
            assert!(
                !inhabits(&v, &ty),
                "thermo({card}) should NOT inhabit Flat({n})"
            );
        }
    }

    #[test]
    fn inhabits_flat_rejects_non_thermometer() {
        let ty = Type::Flat(4);
        assert!(!inhabits(&Value::Unit, &ty));
        assert!(!inhabits(&Value::pair(Value::Unit, Value::Unit), &ty));
    }

    #[test]
    fn round_trip_flat_xor_prim() {
        // Build a byte-Xor prim over Flat(4): (a, b) ↦ (a, a^b), self-inverse.
        let flat4 = Type::Flat(4);
        let ty = Type::prod(flat4.clone(), flat4.clone());
        let vals = enumerate(&ty);

        let mut fwd = BTreeMap::new();
        for v in &vals {
            let Value::Pair(a, b) = v else { panic!("expected pair") };
            let a_idx = thermo_idx(a);
            let b_idx = thermo_idx(b);
            let out_b = a_idx ^ b_idx;
            let out = Value::pair(thermo_val(a_idx), thermo_val(out_b));
            fwd.insert(v.clone(), out);
        }

        let def = PrimDef {
            forward_name: "xor".into(),
            backward_name: "xor".into(),
            input_ty: ty.clone(),
            output_ty: ty,
            forward: fwd.clone(),
            backward: fwd,
            symbolic: None,
        };
        let iso = Iso::Prim(Box::new(def));
        assert_round_trip(&iso);
    }

    fn thermo_val(k: usize) -> Value {
        let mut v = Value::left(Value::Unit);
        for _ in 0..k {
            v = Value::right(v);
        }
        v
    }

    fn thermo_idx(v: &Value) -> usize {
        let mut count = 0usize;
        let mut cur = v;
        loop {
            match cur {
                Value::Left(_) => return count,
                Value::Right(rest) => { count += 1; cur = rest; }
                _ => return count,
            }
        }
    }
}
