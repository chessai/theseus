//! T3: shared XOR primitive builders for the valwidth / bveq-invariant tests.
//!
//! `xorInto : (a, b) ↦ (a, a⊕b)` over a `Flat(w)` pair, materialized as a
//! table-backed `PrimDef` (self-inverse: forward == backward). Only the four
//! byte-identical copies fold in here; the `xor_reduce` / `general_lowering`
//! variants (different return types / backward semantics) are intentionally
//! NOT shared.

#![allow(dead_code)]

use std::collections::BTreeMap;
use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type};
use theseus_tisc::compile::{value_from_leaf_bytes_pub, value_to_leaf_bytes_pub};

/// `xorInto` over `Flat(w) x Flat(w)`: `(a, b) ↦ (a, a⊕b)`. Self-inverse,
/// so forward and backward tables are identical.
pub fn xor_prim_def(w: u8) -> PrimDef {
    let flat = Type::Flat(w);
    let ty = Type::prod(flat.clone(), flat.clone());
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let a = bytes[0];
        let b = bytes[1];
        let out_b = a ^ b;
        let out_bytes = vec![a, out_b];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        forward.insert(v.clone(), out_val.clone());
        // Self-inverse: backward is the same function.
        backward.insert(v.clone(), out_val);
    }

    PrimDef {
        forward_name: "xorInto".into(),
        backward_name: "xorInto".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

/// The XOR prim as an `Iso`, moving the `PrimDef` into the box (no clone).
pub fn xor_prim_iso(w: u8) -> Iso {
    Iso::Prim(Box::new(xor_prim_def(w)))
}
