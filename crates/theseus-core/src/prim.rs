//! Prim registry: opaque `(forward, backward)` pairs with mutual-inverse
//! admission (ADR-0013, amended).
//!
//! A `prim` supplies a pair of finite functions. It is admitted **only if
//! the pair are mutual inverses**: `backward ∘ forward = id_A` and
//! `forward ∘ backward = id_B` over the finite domains — not merely that
//! each is individually bijective.

use crate::ty::Type;
use std::collections::BTreeMap;

/// A typed value in the Core Π value domain (initial algebra of the type
/// formers). Minimal representation for prim admission checking;
/// `theseus-eval` owns the full evaluation semantics.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Value {
    /// The sole inhabitant of `1`.
    Unit,
    /// Left injection into `a + b`.
    Left(Box<Value>),
    /// Right injection into `a + b`.
    Right(Box<Value>),
    /// Pair in `a × b`.
    Pair(Box<Value>, Box<Value>),
}

impl Value {
    pub fn left(v: Value) -> Self {
        Value::Left(Box::new(v))
    }

    pub fn right(v: Value) -> Self {
        Value::Right(Box::new(v))
    }

    pub fn pair(a: Value, b: Value) -> Self {
        Value::Pair(Box::new(a), Box::new(b))
    }
}

/// Enumerate all inhabitants of a finite type.
pub fn enumerate_values(ty: &Type) -> Vec<Value> {
    match ty {
        Type::Zero => vec![],
        Type::One => vec![Value::Unit],
        Type::Sum(a, b) => {
            let mut vals: Vec<Value> =
                enumerate_values(a).into_iter().map(Value::left).collect();
            vals.extend(enumerate_values(b).into_iter().map(Value::right));
            vals
        }
        Type::Prod(a, b) => {
            let bs = enumerate_values(b);
            let mut vals = Vec::new();
            for va in enumerate_values(a) {
                for vb in &bs {
                    vals.push(Value::pair(va.clone(), vb.clone()));
                }
            }
            vals
        }
        Type::Flat(n) => {
            debug_assert!(*n <= 8, "Flat({n}): width must be <= 8");
            let card = 1usize << n;
            (0..card)
                .map(|k| {
                    // Right^k(Left(Unit)) — same thermometer encoding as numeral_ty
                    let mut v = Value::left(Value::Unit);
                    for _ in 0..k {
                        v = Value::right(v);
                    }
                    v
                })
                .collect()
        }
    }
}

/// Definition of a primitive iso: a named `(forward, backward)` pair of
/// finite functions together with their domain and codomain types.
///
/// The adjoint swaps both names and maps:
/// `def.adjoint().adjoint() == def` (involution).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimDef {
    pub forward_name: String,
    pub backward_name: String,
    pub input_ty: Type,
    pub output_ty: Type,
    pub forward: BTreeMap<Value, Value>,
    pub backward: BTreeMap<Value, Value>,
}

impl PrimDef {
    /// Return the adjoint prim: swap forward/backward and input/output types.
    pub fn adjoint(&self) -> Self {
        PrimDef {
            forward_name: self.backward_name.clone(),
            backward_name: self.forward_name.clone(),
            input_ty: self.output_ty.clone(),
            output_ty: self.input_ty.clone(),
            forward: self.backward.clone(),
            backward: self.forward.clone(),
        }
    }
}

/// Error from prim admission.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AdmitError {
    /// `forward` is not defined for a value in the input type's domain.
    ForwardNotTotal(Value),
    /// `backward` is not defined for a value in the output type's domain.
    BackwardNotTotal(Value),
    /// `backward(forward(v)) ≠ v`.
    BackwardOfForward { input: Value },
    /// `forward(backward(w)) ≠ w`.
    ForwardOfBackward { input: Value },
}

/// Check that a [`PrimDef`] is admissible: the `(forward, backward)` pair
/// are mutual inverses over the finite domain.
///
/// Checks both `backward ∘ forward = id_A` and `forward ∘ backward = id_B`.
pub fn admit(def: &PrimDef) -> Result<(), AdmitError> {
    let domain = enumerate_values(&def.input_ty);
    let codomain = enumerate_values(&def.output_ty);

    // forward must be total over domain
    for v in &domain {
        if !def.forward.contains_key(v) {
            return Err(AdmitError::ForwardNotTotal(v.clone()));
        }
    }

    // backward must be total over codomain
    for w in &codomain {
        if !def.backward.contains_key(w) {
            return Err(AdmitError::BackwardNotTotal(w.clone()));
        }
    }

    // backward ∘ forward = id_A
    for v in &domain {
        let w = &def.forward[v];
        match def.backward.get(w) {
            Some(v2) if v2 == v => {}
            _ => return Err(AdmitError::BackwardOfForward { input: v.clone() }),
        }
    }

    // forward ∘ backward = id_B
    for w in &codomain {
        let v = &def.backward[w];
        match def.forward.get(v) {
            Some(w2) if w2 == w => {}
            _ => return Err(AdmitError::ForwardOfBackward { input: w.clone() }),
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bool_ty() -> Type {
        Type::sum(Type::One, Type::One)
    }

    fn tt() -> Value {
        Value::left(Value::Unit)
    }

    fn ff() -> Value {
        Value::right(Value::Unit)
    }

    #[test]
    fn admit_self_inverse() {
        // `not : Bool → Bool`, self-inverse
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), ff());
        fwd.insert(ff(), tt());

        let def = PrimDef {
            forward_name: "not".into(),
            backward_name: "not".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd.clone(),
            backward: fwd,
        };
        assert!(admit(&def).is_ok());
    }

    #[test]
    fn admit_identity() {
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), tt());
        fwd.insert(ff(), ff());

        let def = PrimDef {
            forward_name: "id".into(),
            backward_name: "id".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd.clone(),
            backward: fwd,
        };
        assert!(admit(&def).is_ok());
    }

    #[test]
    fn reject_non_mutual_inverse() {
        // forward = identity, backward = not
        // backward(forward(tt)) = backward(tt) = ff ≠ tt
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), tt());
        fwd.insert(ff(), ff());

        let mut bwd = BTreeMap::new();
        bwd.insert(tt(), ff());
        bwd.insert(ff(), tt());

        let def = PrimDef {
            forward_name: "id".into(),
            backward_name: "not".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd,
            backward: bwd,
        };
        assert!(admit(&def).is_err());
    }

    #[test]
    fn reject_non_total_forward() {
        // forward only defined on tt, not ff
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), tt());
        let mut bwd = BTreeMap::new();
        bwd.insert(tt(), tt());
        bwd.insert(ff(), ff());

        let def = PrimDef {
            forward_name: "partial".into(),
            backward_name: "partial".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd,
            backward: bwd,
        };
        let err = admit(&def).unwrap_err();
        assert!(matches!(err, AdmitError::ForwardNotTotal(_)));
    }

    #[test]
    fn enumerate_zero() {
        assert!(enumerate_values(&Type::Zero).is_empty());
    }

    #[test]
    fn enumerate_one() {
        assert_eq!(enumerate_values(&Type::One), vec![Value::Unit]);
    }

    #[test]
    fn enumerate_bool() {
        let vals = enumerate_values(&bool_ty());
        assert_eq!(vals.len(), 2);
        assert_eq!(vals[0], tt());
        assert_eq!(vals[1], ff());
    }

    #[test]
    fn enumerate_product() {
        // 1 × Bool has 2 inhabitants
        let ty = Type::prod(Type::One, bool_ty());
        assert_eq!(enumerate_values(&ty).len(), 2);
    }

    #[test]
    fn enumerate_zero_product() {
        // 0 × 1 has 0 inhabitants
        let ty = Type::prod(Type::Zero, Type::One);
        assert!(enumerate_values(&ty).is_empty());
    }

    #[test]
    fn prim_def_adjoint_involution() {
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), ff());
        fwd.insert(ff(), tt());

        let def = PrimDef {
            forward_name: "not".into(),
            backward_name: "not_inv".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd.clone(),
            backward: fwd,
        };
        assert_eq!(def.adjoint().adjoint(), def);
    }
}
