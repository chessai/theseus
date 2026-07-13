//! Register addressing: lift operand-level isos to State-level isos.
//!
//! The strategy: given an operand-level iso `op : T_operands <-> T_operands`
//! and the field indices within State that the instruction addresses,
//! produce a State-level iso `State <-> State` that:
//!
//! 1. Permutes the addressed fields to the head of the State product
//!    (via adjacent-swap plumbing on the 20-field right-nested product),
//! 2. Applies `(op x Id(rest))`,
//! 3. Applies the inverse permutation to restore field order.
//!
//! All routing is zero-cell (pure wire permutation at the field level).

use theseus_core::{Iso, Type};
use crate::types::{word8, NUM_FIELDS};

// ── Right-nested product helpers (field-level, not bit-level) ──────

/// Right-nested product of `n` Word8 fields.
fn fields(n: usize) -> Type {
    assert!(n >= 1);
    let w = word8();
    if n == 1 {
        w
    } else {
        Type::prod(w, fields(n - 1))
    }
}

/// Swap adjacent fields at positions `k` and `k+1` in `fields(n)`.
/// Zero cells: pure product-structure plumbing.
fn swap_field_adj(n: usize, k: usize) -> Iso {
    assert!(k + 1 < n);
    let w = word8();
    if k > 0 {
        return Iso::times(Iso::Id(w), swap_field_adj(n - 1, k - 1));
    }
    // k == 0: swap the first two fields
    if n == 2 {
        Iso::SwapProd(w.clone(), w)
    } else {
        let rest = fields(n - 2);
        // (A x (B x R)) -> ((A x B) x R) -> ((B x A) x R) -> (B x (A x R))
        Iso::seq(
            Iso::seq(
                Iso::AssocLProd(w.clone(), w.clone(), rest.clone()),
                Iso::times(Iso::SwapProd(w.clone(), w.clone()), Iso::Id(rest.clone())),
            ),
            Iso::AssocRProd(w.clone(), w, rest),
        )
    }
}

/// Build the routing iso that moves fields at `indices` (in order) to
/// positions 0, 1, 2, ... at the head of the product, using adjacent swaps.
/// Returns the routing iso and the inverse is just `Iso::sym(route)`.
fn route_to_head(indices: &[usize]) -> Iso {
    let n = NUM_FIELDS;
    let mut arr: Vec<usize> = (0..n).collect();
    let mut swaps: Vec<Iso> = Vec::new();

    for (slot, &orig) in indices.iter().enumerate() {
        let p = arr.iter().position(|&x| x == orig).expect("field index in range");
        assert!(p >= slot, "routing: field already consumed or out of order");
        for k in (slot..p).rev() {
            swaps.push(swap_field_adj(n, k));
            arr.swap(k, k + 1);
        }
    }

    if swaps.is_empty() {
        Iso::Id(fields(n))
    } else {
        swaps.into_iter().reduce(Iso::seq).unwrap()
    }
}

/// `fields(k+r) <-> fields(k) x fields(r)`: split the first k fields
/// out of the right-nested product as a separate group.
///
/// fields(n) is right-nested: `W x (W x (... x W))`. To apply an
/// operand iso on the first k fields, we need them grouped as `fields(k)`
/// on the left of a product with `fields(r)` on the right.
fn split_fields(k: usize, r: usize) -> Iso {
    assert!(k >= 1 && r >= 1);
    let w = word8();
    if k == 1 {
        // fields(1+r) = W x fields(r) = fields(1) x fields(r). Done.
        Iso::Id(fields(1 + r))
    } else {
        // fields(k+r) = W x fields((k-1)+r)
        // -> W x (fields(k-1) x fields(r))     [recursive split]
        // -> (W x fields(k-1)) x fields(r)     [AssocL]
        // = fields(k) x fields(r)
        Iso::seq(
            Iso::times(Iso::Id(w.clone()), split_fields(k - 1, r)),
            Iso::AssocLProd(w, fields(k - 1), fields(r)),
        )
    }
}

/// Lift an operand-level iso to State level at given field indices.
///
/// `op` operates on a right-nested product of `indices.len()` Word8 fields.
/// The result is a State-level iso that:
/// - routes the addressed fields to the head,
/// - reshapes to `fields(k) x fields(rest)`,
/// - applies `op x Id(rest)`,
/// - reshapes back,
/// - routes back.
///
/// All unaddressed fields pass through as identity.
pub fn at_fields(op: Iso, indices: &[usize]) -> Iso {
    let n = NUM_FIELDS;
    let k = indices.len();
    assert!(k >= 1 && k <= n);

    // Validate indices are distinct and in range
    for &idx in indices {
        assert!(idx < n, "field index {idx} out of range");
    }
    for i in 0..k {
        for j in i + 1..k {
            assert_ne!(indices[i], indices[j], "duplicate field index");
        }
    }

    let route = route_to_head(indices);
    let rest_count = n - k;

    if rest_count == 0 {
        // op covers all fields, no rest
        Iso::seq(Iso::seq(route.clone(), op), Iso::sym(route))
    } else if k == 1 {
        // No reshape needed: fields(n) = W x fields(n-1), op : W <-> W
        let lifted = Iso::times(op, Iso::Id(fields(rest_count)));
        Iso::seq(Iso::seq(route.clone(), lifted), Iso::sym(route))
    } else {
        // Reshape: fields(n) <-> fields(k) x fields(rest)
        let reshape = split_fields(k, rest_count);
        let apply = Iso::times(op, Iso::Id(fields(rest_count)));
        let inner = Iso::seq(
            Iso::seq(reshape.clone(), apply),
            Iso::sym(reshape),
        );
        Iso::seq(Iso::seq(route.clone(), inner), Iso::sym(route))
    }
}

// ── Convenience wrappers ───────────────────────────────────────────

/// Lift a 1-register operand iso (Word8 <-> Word8) to State.
pub fn at_reg1(op: Iso, rd: usize) -> Iso {
    at_fields(op, &[rd])
}

/// Lift a 2-register operand iso ((Word8, Word8) <-> (Word8, Word8)) to State.
pub fn at_reg2(op: Iso, rd: usize, rs: usize) -> Iso {
    at_fields(op, &[rd, rs])
}

/// Lift a 3-register operand iso ((Word8, (Word8, Word8)) <-> ...) to State.
pub fn at_reg3(op: Iso, rd: usize, rs: usize, rt: usize) -> Iso {
    at_fields(op, &[rd, rs, rt])
}
