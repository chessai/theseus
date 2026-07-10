//! Fuel-based interpreter over core Π — stage one of the oracle chain.
//!
//! - Partiality per join-inverse-category semantics (ADR-0005): fuel in
//!   software, valid/done handshakes in hardware.
//! - The interpreter is the semantic root: typed-sim, bit-sim, and
//!   Verilator are all differentially tested against it (ADR-0008/0018).
//! - Powers the REPL's `:step`/`:unstep` — the software preview of the
//!   FPGA time-travel demo (workspace plan, ADR-0016 provenance).

pub mod enumerate;
pub mod interp;
pub mod value;

pub use enumerate::{cardinality, enumerate};
pub use interp::{eval, EvalError};
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
}
