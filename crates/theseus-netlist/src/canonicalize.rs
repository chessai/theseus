//! Core Π → typed netlist canonicalization (ADR-0014).
//!
//! Flattens monoid layers, absorbs units (`1` under ×, `0` under +), and
//! turns every structural combinator into `Permute(monoid, σ)` — the
//! netlist IR never contains a raw swap or assoc. `sym` is eliminated up
//! front via the syntactic adjoint (it denotes the inverse and carries no
//! runtime cost).
//!
//! TODO(ADR-0014): fuse *adjacent* Permutes by permutation composition
//! (O(n), Mac Lane coherence makes the result canonical). Not needed for
//! correctness of the dual-rail lowering — back-to-back Permutes each
//! lower to zero cells — so it waits for the QoR pass.

use theseus_core::{adjoint, type_of, Iso, Type, TypeError};

use crate::ir::typed::{flatten_layer, identity_sigma, DistribDir, Monoid, NodeKind, TypedNode};

/// σ for swapping two flattened blocks: output layer `[B, A]` from input
/// layer `[A, B]`, where `A` has `la` slots and `B` has `lb`.
fn block_swap_sigma(la: usize, lb: usize) -> Vec<usize> {
    (la..la + lb).chain(0..la).collect()
}

/// Canonicalize a Core Π iso term into the typed netlist IR. Fails only
/// on ill-typed terms.
///
/// The produced node always satisfies [`TypedNode::validate`], and its
/// boundary types are exactly `type_of(iso)` — checked by tests over the
/// combinator vocabulary.
pub fn canonicalize(iso: &Iso) -> Result<TypedNode, TypeError> {
    // `sym` is a syntactic involution: push it to the leaves first.
    if let Iso::Sym(f) = iso {
        return canonicalize(&adjoint(f));
    }

    let (input, output) = type_of(iso)?;

    let kind = match iso {
        Iso::Sym(_) => unreachable!("handled above"),

        // Identity: the empty plumbing permutation over the ×-layer (a
        // non-product type is a single-leaf layer).
        Iso::Id(_) => NodeKind::Permute {
            monoid: Monoid::Prod,
            sigma: identity_sigma(flatten_layer(Monoid::Prod, &input).len()),
        },

        Iso::Seq(f, g) => NodeKind::Seq(vec![canonicalize(f)?, canonicalize(g)?]),
        Iso::Times(f, g) => {
            NodeKind::Par(Box::new(canonicalize(f)?), Box::new(canonicalize(g)?))
        }
        Iso::Plus(f, g) => {
            NodeKind::Branch(Box::new(canonicalize(f)?), Box::new(canonicalize(g)?))
        }

        // ── Product-monoid plumbing → Permute(×, σ) ──────────────────
        Iso::SwapProd(a, b) => NodeKind::Permute {
            monoid: Monoid::Prod,
            sigma: block_swap_sigma(
                flatten_layer(Monoid::Prod, a).len(),
                flatten_layer(Monoid::Prod, b).len(),
            ),
        },
        // Assoc and unit isos are invisible after flattening: same leaf
        // sequence on both sides, σ = id.
        Iso::AssocLProd(..) | Iso::AssocRProd(..) | Iso::UniteProd(_) | Iso::UnitiProd(_) => {
            NodeKind::Permute {
                monoid: Monoid::Prod,
                sigma: identity_sigma(flatten_layer(Monoid::Prod, &input).len()),
            }
        }

        // ── Sum-monoid plumbing → Permute(+, σ) ──────────────────────
        Iso::SwapSum(a, b) => NodeKind::Permute {
            monoid: Monoid::Sum,
            sigma: block_swap_sigma(
                flatten_layer(Monoid::Sum, a).len(),
                flatten_layer(Monoid::Sum, b).len(),
            ),
        },
        Iso::AssocLSum(..) | Iso::AssocRSum(..) | Iso::UniteSum(_) | Iso::UnitiSum(_) => {
            NodeKind::Permute {
                monoid: Monoid::Sum,
                sigma: identity_sigma(flatten_layer(Monoid::Sum, &input).len()),
            }
        }

        // ── Distinct nodes ───────────────────────────────────────────
        Iso::Distrib(..) => NodeKind::Distrib(DistribDir::Distrib),
        Iso::Factor(..) => NodeKind::Distrib(DistribDir::Factor),
        Iso::Trace(u, f) => NodeKind::TraceLoop {
            carrier: u.clone(),
            body: Box::new(canonicalize(f)?),
        },
        Iso::Prim(def) => NodeKind::Prim(def.clone()),
    };

    Ok(TypedNode { input, output, kind })
}

/// Convenience: the type vocabulary's bit, `1 + 1`.
pub fn bit_type() -> Type {
    Type::sum(Type::One, Type::One)
}

#[cfg(test)]
mod tests {
    use super::*;
    use theseus_core::Iso;

    fn bit() -> Type {
        bit_type()
    }

    fn assert_canonical(iso: &Iso) -> TypedNode {
        let node = canonicalize(iso).expect("canonicalize");
        node.validate().expect("canonical node validates");
        let (a, b) = type_of(iso).unwrap();
        assert_eq!((node.input.clone(), node.output.clone()), (a, b));
        node
    }

    #[test]
    fn swap_prod_becomes_block_permutation() {
        // swapˣ : Bit × (Bit × Bit) ↔ (Bit × Bit) × Bit over the flattened
        // layer [b0, b1, b2] is σ = [1, 2, 0].
        let node = assert_canonical(&Iso::SwapProd(bit(), Type::prod(bit(), bit())));
        assert_eq!(
            node.kind,
            NodeKind::Permute { monoid: Monoid::Prod, sigma: vec![1, 2, 0] }
        );
    }

    #[test]
    fn not_is_a_sum_permutation() {
        // swap⁺ : 1 + 1 ↔ 1 + 1 is NOT: σ = [1, 0] on the tag layer.
        let node = assert_canonical(&Iso::SwapSum(Type::One, Type::One));
        assert_eq!(
            node.kind,
            NodeKind::Permute { monoid: Monoid::Sum, sigma: vec![1, 0] }
        );
    }

    #[test]
    fn assoc_and_units_flatten_to_identity() {
        for iso in [
            Iso::AssocRProd(bit(), bit(), bit()),
            Iso::AssocLProd(bit(), bit(), bit()),
            Iso::UniteProd(bit()),
            Iso::UnitiProd(bit()),
        ] {
            let node = assert_canonical(&iso);
            match node.kind {
                NodeKind::Permute { monoid: Monoid::Prod, sigma } => {
                    assert_eq!(sigma, identity_sigma(sigma.len()));
                }
                other => panic!("expected Permute, got {other:?}"),
            }
        }
    }

    #[test]
    fn sym_is_eliminated() {
        // sym swapˣ canonicalizes with no Sym anywhere: it *is* a Permute.
        let node = assert_canonical(&Iso::sym(Iso::SwapProd(bit(), bit())));
        assert!(matches!(node.kind, NodeKind::Permute { .. }));
    }

    #[test]
    fn full_vocabulary_canonicalizes_and_validates() {
        let isos = vec![
            Iso::Id(bit()),
            Iso::seq(Iso::Id(bit()), Iso::SwapSum(Type::One, Type::One)),
            Iso::times(Iso::Id(bit()), Iso::SwapProd(bit(), bit())),
            Iso::plus(Iso::Id(bit()), Iso::Id(Type::One)),
            Iso::AssocLSum(Type::One, Type::One, bit()),
            Iso::UniteSum(bit()),
            Iso::UnitiSum(bit()),
            Iso::Distrib(bit(), Type::One, Type::One),
            Iso::Factor(bit(), Type::One, Type::One),
            Iso::trace(Type::One, Iso::SwapSum(Type::One, Type::One)),
        ];
        for iso in &isos {
            assert_canonical(iso);
        }
    }

    #[test]
    fn ill_typed_seq_is_rejected() {
        let bad = Iso::seq(Iso::Id(bit()), Iso::Id(Type::One));
        assert!(canonicalize(&bad).is_err());
    }
}
