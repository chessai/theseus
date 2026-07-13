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

use theseus_core::{adjoint, Iso, Type, TypeError};

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
///
/// Complexity: O(n) in the iso size. Boundary types are derived
/// bottom-up from leaf constructors — `type_of` is never called
/// recursively at interior nodes, avoiding the O(n²) re-derivation
/// that the naive per-node `type_of` approach produces on deep chains.
pub fn canonicalize(iso: &Iso) -> Result<TypedNode, TypeError> {
    // `sym` is a syntactic involution: push it to the leaves first.
    if let Iso::Sym(f) = iso {
        return canonicalize(&adjoint(f));
    }

    match iso {
        Iso::Sym(_) => unreachable!("handled above"),

        // ── Identity ────────────────────────────────────────────────
        // type_of(Id(a)) is (a, a) — O(1), no subtree walk.
        Iso::Id(a) => {
            let input = a.clone();
            let output = a.clone();
            Ok(TypedNode {
                input: input.clone(),
                output: output.clone(),
                kind: NodeKind::Permute {
                    monoid: Monoid::Prod,
                    sigma: identity_sigma(flatten_layer(Monoid::Prod, &input).len()),
                },
            })
        }

        // ── Seq: recurse into children, derive boundary types from
        // their TypedNodes — no type_of on the whole subtree. ────────
        Iso::Seq(f, g) => {
            let fn_ = canonicalize(f)?;
            let gn = canonicalize(g)?;
            // Type check: f's output must match g's input (same check
            // as type_of, but using already-computed child types).
            if fn_.output != gn.input {
                return Err(TypeError::SeqMismatch {
                    left_output: fn_.output.clone(),
                    right_input: gn.input.clone(),
                });
            }
            let input = fn_.input.clone();
            let output = gn.output.clone();
            Ok(TypedNode {
                input,
                output,
                kind: NodeKind::Seq(vec![fn_, gn]),
            })
        }

        // ── Times (×): boundary = Prod of children's boundaries ────
        Iso::Times(f, g) => {
            let fn_ = canonicalize(f)?;
            let gn = canonicalize(g)?;
            let input = Type::prod(fn_.input.clone(), gn.input.clone());
            let output = Type::prod(fn_.output.clone(), gn.output.clone());
            Ok(TypedNode {
                input,
                output,
                kind: NodeKind::Par(Box::new(fn_), Box::new(gn)),
            })
        }

        // ── Plus (+): boundary = Sum of children's boundaries ───────
        Iso::Plus(f, g) => {
            let fn_ = canonicalize(f)?;
            let gn = canonicalize(g)?;
            let input = Type::sum(fn_.input.clone(), gn.input.clone());
            let output = Type::sum(fn_.output.clone(), gn.output.clone());
            Ok(TypedNode {
                input,
                output,
                kind: NodeKind::Branch(Box::new(fn_), Box::new(gn)),
            })
        }

        // ── Product-monoid plumbing → Permute(×, σ) ──────────────────
        Iso::SwapProd(a, b) => {
            let input = Type::prod(a.clone(), b.clone());
            let output = Type::prod(b.clone(), a.clone());
            Ok(TypedNode {
                input,
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Prod,
                    sigma: block_swap_sigma(
                        flatten_layer(Monoid::Prod, a).len(),
                        flatten_layer(Monoid::Prod, b).len(),
                    ),
                },
            })
        }
        Iso::AssocLProd(a, b, c) => {
            let input = Type::prod(a.clone(), Type::prod(b.clone(), c.clone()));
            let output = Type::prod(Type::prod(a.clone(), b.clone()), c.clone());
            Ok(TypedNode {
                input: input.clone(),
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Prod,
                    sigma: identity_sigma(flatten_layer(Monoid::Prod, &input).len()),
                },
            })
        }
        Iso::AssocRProd(a, b, c) => {
            let input = Type::prod(Type::prod(a.clone(), b.clone()), c.clone());
            let output = Type::prod(a.clone(), Type::prod(b.clone(), c.clone()));
            Ok(TypedNode {
                input: input.clone(),
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Prod,
                    sigma: identity_sigma(flatten_layer(Monoid::Prod, &input).len()),
                },
            })
        }
        Iso::UniteProd(a) => {
            let input = Type::prod(Type::One, a.clone());
            let output = a.clone();
            Ok(TypedNode {
                input: input.clone(),
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Prod,
                    sigma: identity_sigma(flatten_layer(Monoid::Prod, &input).len()),
                },
            })
        }
        Iso::UnitiProd(a) => {
            let input = a.clone();
            let output = Type::prod(Type::One, a.clone());
            Ok(TypedNode {
                input: input.clone(),
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Prod,
                    sigma: identity_sigma(flatten_layer(Monoid::Prod, &input).len()),
                },
            })
        }

        // ── Sum-monoid plumbing → Permute(+, σ) ──────────────────────
        Iso::SwapSum(a, b) => {
            let input = Type::sum(a.clone(), b.clone());
            let output = Type::sum(b.clone(), a.clone());
            Ok(TypedNode {
                input,
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Sum,
                    sigma: block_swap_sigma(
                        flatten_layer(Monoid::Sum, a).len(),
                        flatten_layer(Monoid::Sum, b).len(),
                    ),
                },
            })
        }
        Iso::AssocLSum(a, b, c) => {
            let input = Type::sum(a.clone(), Type::sum(b.clone(), c.clone()));
            let output = Type::sum(Type::sum(a.clone(), b.clone()), c.clone());
            Ok(TypedNode {
                input: input.clone(),
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Sum,
                    sigma: identity_sigma(flatten_layer(Monoid::Sum, &input).len()),
                },
            })
        }
        Iso::AssocRSum(a, b, c) => {
            let input = Type::sum(Type::sum(a.clone(), b.clone()), c.clone());
            let output = Type::sum(a.clone(), Type::sum(b.clone(), c.clone()));
            Ok(TypedNode {
                input: input.clone(),
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Sum,
                    sigma: identity_sigma(flatten_layer(Monoid::Sum, &input).len()),
                },
            })
        }
        Iso::UniteSum(a) => {
            let input = Type::sum(Type::Zero, a.clone());
            let output = a.clone();
            Ok(TypedNode {
                input: input.clone(),
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Sum,
                    sigma: identity_sigma(flatten_layer(Monoid::Sum, &input).len()),
                },
            })
        }
        Iso::UnitiSum(a) => {
            let input = a.clone();
            let output = Type::sum(Type::Zero, a.clone());
            Ok(TypedNode {
                input: input.clone(),
                output,
                kind: NodeKind::Permute {
                    monoid: Monoid::Sum,
                    sigma: identity_sigma(flatten_layer(Monoid::Sum, &input).len()),
                },
            })
        }

        // ── Distinct nodes ───────────────────────────────────────────
        Iso::Distrib(a, b, c) => {
            let input = Type::prod(a.clone(), Type::sum(b.clone(), c.clone()));
            let output = Type::sum(
                Type::prod(a.clone(), b.clone()),
                Type::prod(a.clone(), c.clone()),
            );
            Ok(TypedNode {
                input,
                output,
                kind: NodeKind::Distrib(DistribDir::Distrib),
            })
        }
        Iso::Factor(a, b, c) => {
            let input = Type::sum(
                Type::prod(a.clone(), b.clone()),
                Type::prod(a.clone(), c.clone()),
            );
            let output = Type::prod(a.clone(), Type::sum(b.clone(), c.clone()));
            Ok(TypedNode {
                input,
                output,
                kind: NodeKind::Distrib(DistribDir::Factor),
            })
        }
        Iso::Trace(u, f) => {
            let body = canonicalize(f)?;
            // Derive trace boundary types from body's types — same
            // validation as type_of but using already-computed types.
            let a = match &body.input {
                Type::Sum(u1, a) if u1.as_ref() == u => a.as_ref().clone(),
                Type::Sum(u1, _) => {
                    return Err(TypeError::TraceInputCarrier {
                        expected: u.clone(),
                        got: u1.as_ref().clone(),
                    });
                }
                _ => return Err(TypeError::TraceInputNotSum(body.input.clone())),
            };
            let b = match &body.output {
                Type::Sum(u2, b) if u2.as_ref() == u => b.as_ref().clone(),
                Type::Sum(u2, _) => {
                    return Err(TypeError::TraceOutputCarrier {
                        expected: u.clone(),
                        got: u2.as_ref().clone(),
                    });
                }
                _ => return Err(TypeError::TraceOutputNotSum(body.output.clone())),
            };
            Ok(TypedNode {
                input: a,
                output: b,
                kind: NodeKind::TraceLoop {
                    carrier: u.clone(),
                    body: Box::new(body),
                },
            })
        }
        Iso::Prim(def) => Ok(TypedNode {
            input: def.input_ty.clone(),
            output: def.output_ty.clone(),
            kind: NodeKind::Prim(def.clone()),
        }),
    }
}

/// Convenience: the type vocabulary's bit, `1 + 1`.
pub fn bit_type() -> Type {
    Type::sum(Type::One, Type::One)
}

#[cfg(test)]
mod tests {
    use super::*;
    use theseus_core::{type_of, Iso};

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
