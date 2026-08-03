//! The typed reversible netlist IR (ADR-0013).
//!
//! Hierarchical, typed, direction-symmetric. Sums stay **typed** here --
//! how a sum is encoded in rails is a backend lowering decision
//! (ADR-0011). Structural plumbing appears only as `Permute(monoid, σ)`
//! (ADR-0014): the canonicalizer never emits a raw swap or assoc, and the
//! two monoids never mix in one node.


use theseus_core::{PrimDef, Type};

/// The monoid a [`NodeKind::Permute`] acts in (ADR-0014).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Monoid {
    /// The sum monoid `(+, 0)`.
    Sum,
    /// The product monoid `(×, 1)`.
    Prod,
}

/// Direction of the distributivity pair (`distrib` / `factor`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DistribDir {
    /// `distrib : a × (b + c) ↔ (a × b) + (a × c)`.
    Distrib,
    /// `factor : (a × b) + (a × c) ↔ a × (b + c)` (the adjoint direction).
    Factor,
}

/// Direction of the structural-recursion pair (`fold` / `unfold`,
/// ADR-0037): both share one node shape; only the accumulator
/// threading order differs (head-to-tail vs tail-to-head).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FoldDir {
    /// `fold`: reversible mapAccumL, head-to-tail.
    Fold,
    /// `unfold`: the tail-to-head adjoint partner.
    Unfold,
}

/// A typed netlist node: a region carrying its boundary types.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedNode {
    pub input: Type,
    pub output: Type,
    pub kind: NodeKind,
}

/// The ADR-0013 node vocabulary (v0.1 subset; `Iter` reserved for v0.2,
/// `HandshakeLoop` for v0.2+, `Absurd` pruning is a later pass).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NodeKind {
    /// Sequential region, diagram order. An empty sequence is identity.
    Seq(Vec<TypedNode>),
    /// Product region `f × g`.
    Par(Box<TypedNode>, Box<TypedNode>),
    /// Canonical structural plumbing (ADR-0014): σ permutes one flattened
    /// monoid layer, `flatten(output)[j] = flatten(input)[σ[j]]`. Units of
    /// the monoid (`1` under ×, `0` under +) are absorbed by flattening.
    Permute { monoid: Monoid, sigma: Vec<usize> },
    /// Sum region `f + g`. All sum-soundness machinery lives in this one
    /// node's lowering (demux/merge; masking manifests as token routing
    /// under dual-rail, ADR-0031).
    Branch(Box<TypedNode>, Box<TypedNode>),
    /// `distrib`/`factor`: the only leaf-duplicating structural pair;
    /// quarantined per backend (ADR-0013/0014).
    Distrib(DistribDir),
    /// Finite trace with the given carrier `u`; WCET ≤ |u| (ADR-0007,
    /// ADR-0009). Body has type `u + input ↔ u + output`.
    TraceLoop { carrier: Type, body: Box<TypedNode> },
    /// Bounded counter-driven iteration (`iter n f`, ADR-0036): body
    /// `f : a ↔ a` is applied `k` times where `k` is the unary numeral
    /// carried in the DATA (first component) and preserved on output.
    /// Node type: `counter × a ↔ counter × a`; `counter` is a numeral
    /// type `1 + (1 + (... + 0))`, so `k ≤ |counter| − 1` bounds the
    /// unroll (ADR-0039).
    IterLoop { counter: Type, body: Box<TypedNode> },
    /// Bounded structural recursion over a list spine (`fold`/`unfold
    /// l f`, ADR-0037): body `f : b × c ↔ b × c` is applied once per
    /// Cons node of the spine value, threading the accumulator
    /// head-to-tail (`Fold`) or tail-to-head (`Unfold`); the spine is
    /// preserved on output. Node type: `spine × c ↔ spine × c`;
    /// `spine = 1 + (b × (1 + (b × (... + 0))))`, so its unroll depth − 1
    /// bounds the unroll (ADR-0039).
    FoldLoop { spine: Type, dir: FoldDir, body: Box<TypedNode> },
    /// Registry leaf: an admitted `(forward, backward)` mutual-inverse
    /// pair (ADR-0013 amended).
    Prim(Box<PrimDef>),
    /// `n`-stage semantic identity. Retiming across it is always sound;
    /// under the adiabatic backend it lowers to `n` buffer stages and
    /// holding costs motion (ADR-0026 C2.1).
    Reg(usize),
}

/// Flatten the top-level `monoid` spine of `ty` into one layer, absorbing
/// the monoid's unit (`1` under ×, `0` under +). Leaves are the maximal
/// subterms not headed by the monoid's constructor (ADR-0014: Permute
/// acts on one flattened layer; layers of the *other* monoid are leaves).
pub fn flatten_layer(monoid: Monoid, ty: &Type) -> Vec<Type> {
    match (monoid, ty) {
        (Monoid::Prod, Type::Prod(a, b)) | (Monoid::Sum, Type::Sum(a, b)) => {
            let mut layer = flatten_layer(monoid, a);
            layer.extend(flatten_layer(monoid, b));
            layer
        }
        (Monoid::Prod, Type::One) | (Monoid::Sum, Type::Zero) => vec![],
        _ => vec![ty.clone()],
    }
}

/// The identity permutation on `n` slots.
pub fn identity_sigma(n: usize) -> Vec<usize> {
    (0..n).collect()
}

/// Well-formedness error for a [`TypedNode`].
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum TypedError {
    /// Two types that must agree don't.
    #[error("{context}: expected type {expected}, got {got}")]
    TypeMismatch {
        context: &'static str,
        expected: Type,
        got: Type,
    },
    /// σ is not a permutation of the flattened layer (wrong length,
    /// out-of-range index, or repeated index).
    #[error("{context}: sigma is not a layer permutation")]
    BadSigma { context: &'static str },
    /// A node's boundary type doesn't have the shape the node requires.
    #[error("{context}: type {ty} has the wrong shape")]
    BadShape { context: &'static str, ty: Type },
}

fn expect(context: &'static str, expected: &Type, got: &Type) -> Result<(), TypedError> {
    if expected == got {
        Ok(())
    } else {
        Err(TypedError::TypeMismatch {
            context,
            expected: expected.clone(),
            got: got.clone(),
        })
    }
}

impl TypedNode {
    /// Check structural well-formedness: boundary types line up through
    /// every region, σ is a coherent layer permutation, prim boundaries
    /// match their definitions. `Ok(())` is the precondition every pass
    /// over this IR assumes (ADR-0012 spirit: validity is re-established
    /// locally, so it must be checkable locally).
    pub fn validate(&self) -> Result<(), TypedError> {
        match &self.kind {
            NodeKind::Seq(nodes) => {
                let mut cur = &self.input;
                for n in nodes {
                    expect("seq link", cur, &n.input)?;
                    n.validate()?;
                    cur = &n.output;
                }
                expect("seq output", cur, &self.output)
            }
            NodeKind::Par(f, g) => {
                let want_in = Type::prod(f.input.clone(), g.input.clone());
                let want_out = Type::prod(f.output.clone(), g.output.clone());
                expect("par input", &want_in, &self.input)?;
                expect("par output", &want_out, &self.output)?;
                f.validate()?;
                g.validate()
            }
            NodeKind::Permute { monoid, sigma } => {
                let lin = flatten_layer(*monoid, &self.input);
                let lout = flatten_layer(*monoid, &self.output);
                if sigma.len() != lin.len() || lout.len() != lin.len() {
                    return Err(TypedError::BadSigma { context: "permute arity" });
                }
                let mut seen = vec![false; lin.len()];
                for (j, &s) in sigma.iter().enumerate() {
                    if s >= lin.len() || seen[s] {
                        return Err(TypedError::BadSigma { context: "permute sigma" });
                    }
                    seen[s] = true;
                    expect("permute layer", &lin[s], &lout[j])?;
                }
                Ok(())
            }
            NodeKind::Branch(f, g) => {
                let want_in = Type::sum(f.input.clone(), g.input.clone());
                let want_out = Type::sum(f.output.clone(), g.output.clone());
                expect("branch input", &want_in, &self.input)?;
                expect("branch output", &want_out, &self.output)?;
                f.validate()?;
                g.validate()
            }
            NodeKind::Distrib(dir) => {
                // distrib : a × (b + c) ↔ (a × b) + (a × c); factor is the
                // reverse reading.
                let (prod_side, sum_side) = match dir {
                    DistribDir::Distrib => (&self.input, &self.output),
                    DistribDir::Factor => (&self.output, &self.input),
                };
                let Type::Prod(a, bc) = prod_side else {
                    return Err(TypedError::BadShape { context: "distrib product side", ty: prod_side.clone() });
                };
                let Type::Sum(b, c) = bc.as_ref() else {
                    return Err(TypedError::BadShape { context: "distrib sum operand", ty: bc.as_ref().clone() });
                };
                let want = Type::sum(
                    Type::prod(a.as_ref().clone(), b.as_ref().clone()),
                    Type::prod(a.as_ref().clone(), c.as_ref().clone()),
                );
                expect("distrib expansion", &want, sum_side)
            }
            NodeKind::TraceLoop { carrier, body } => {
                let want_in = Type::sum(carrier.clone(), self.input.clone());
                let want_out = Type::sum(carrier.clone(), self.output.clone());
                expect("trace body input", &want_in, &body.input)?;
                expect("trace body output", &want_out, &body.output)?;
                body.validate()
            }
            NodeKind::IterLoop { counter, body } => {
                if !theseus_core::is_numeral_ty(counter) {
                    return Err(TypedError::BadShape {
                        context: "iter counter",
                        ty: counter.clone(),
                    });
                }
                expect("iter body endo", &body.input, &body.output)?;
                let side = Type::prod(counter.clone(), body.input.clone());
                expect("iter input", &side, &self.input)?;
                expect("iter output", &side, &self.output)?;
                body.validate()
            }
            NodeKind::FoldLoop { spine, dir: _, body } => {
                expect("fold body endo", &body.input, &body.output)?;
                let Type::Prod(elem, acc) = &body.input else {
                    return Err(TypedError::BadShape {
                        context: "fold body input",
                        ty: body.input.clone(),
                    });
                };
                match theseus_core::list_spine_elem_ty(spine) {
                    None => {
                        return Err(TypedError::BadShape {
                            context: "fold spine",
                            ty: spine.clone(),
                        });
                    }
                    Some(Some(e)) if e != elem.as_ref() => {
                        return Err(TypedError::TypeMismatch {
                            context: "fold spine element",
                            expected: e.clone(),
                            got: elem.as_ref().clone(),
                        });
                    }
                    _ => {}
                }
                let side = Type::prod(spine.clone(), acc.as_ref().clone());
                expect("fold input", &side, &self.input)?;
                expect("fold output", &side, &self.output)?;
                body.validate()
            }
            NodeKind::Prim(def) => {
                expect("prim input", &def.input_ty, &self.input)?;
                expect("prim output", &def.output_ty, &self.output)
            }
            NodeKind::Reg(_) => expect("reg", &self.input, &self.output),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bit() -> Type {
        Type::sum(Type::One, Type::One)
    }

    #[test]
    fn flatten_prod_absorbs_unit_and_spine() {
        // 1 × (Bit × (Bit × 1)) flattens to [Bit, Bit]
        let ty = Type::prod(Type::One, Type::prod(bit(), Type::prod(bit(), Type::One)));
        assert_eq!(flatten_layer(Monoid::Prod, &ty), vec![bit(), bit()]);
    }

    #[test]
    fn flatten_sum_stops_at_other_monoid() {
        // Under ×, Bit = 1 + 1 is a leaf (a + layer), not absorbed.
        assert_eq!(flatten_layer(Monoid::Prod, &bit()), vec![bit()]);
        // Under +, Bit flattens to its two unit arms.
        assert_eq!(flatten_layer(Monoid::Sum, &bit()), vec![Type::One, Type::One]);
        // 0 is absorbed under +.
        let ty = Type::sum(Type::Zero, bit());
        assert_eq!(flatten_layer(Monoid::Sum, &ty), vec![Type::One, Type::One]);
    }

    #[test]
    fn validate_permute_coherence() {
        // swap× on Bit × Bit: σ = [1, 0], coherent.
        let ok = TypedNode {
            input: Type::prod(bit(), bit()),
            output: Type::prod(bit(), bit()),
            kind: NodeKind::Permute { monoid: Monoid::Prod, sigma: vec![1, 0] },
        };
        assert_eq!(ok.validate(), Ok(()));

        // Repeated index is rejected.
        let bad = TypedNode {
            input: Type::prod(bit(), bit()),
            output: Type::prod(bit(), bit()),
            kind: NodeKind::Permute { monoid: Monoid::Prod, sigma: vec![0, 0] },
        };
        assert!(matches!(bad.validate(), Err(TypedError::BadSigma { .. })));

        // Layer type mismatch is rejected: Bit × 1-leaf vs swapped output
        // that claims the wrong leaf order.
        let three = Type::sum(bit(), Type::One);
        let bad_ty = TypedNode {
            input: Type::prod(bit(), three.clone()),
            output: Type::prod(bit(), three),
            kind: NodeKind::Permute { monoid: Monoid::Prod, sigma: vec![1, 0] },
        };
        assert!(matches!(bad_ty.validate(), Err(TypedError::TypeMismatch { .. })));
    }

    #[test]
    fn validate_seq_links() {
        let id_bit = TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Permute { monoid: Monoid::Prod, sigma: vec![0] },
        };
        let ok = TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Seq(vec![id_bit.clone(), id_bit.clone()]),
        };
        assert_eq!(ok.validate(), Ok(()));

        let bad = TypedNode {
            input: Type::One,
            output: bit(),
            kind: NodeKind::Seq(vec![id_bit]),
        };
        assert!(matches!(bad.validate(), Err(TypedError::TypeMismatch { .. })));
    }

    #[test]
    fn validate_reg_and_distrib() {
        let reg = TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(3) };
        assert_eq!(reg.validate(), Ok(()));

        let distrib = TypedNode {
            input: Type::prod(bit(), Type::sum(Type::One, bit())),
            output: Type::sum(Type::prod(bit(), Type::One), Type::prod(bit(), bit())),
            kind: NodeKind::Distrib(DistribDir::Distrib),
        };
        assert_eq!(distrib.validate(), Ok(()));
    }
}
