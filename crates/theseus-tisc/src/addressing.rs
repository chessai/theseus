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
//!
//! ## Mixed-width fields (B-2.1c, Option A)
//!
//! Under the per-program data-word width `w_data`, CpuState is a **mixed-width**
//! product: DATA fields (registers, `s2..`, memory) are `flat(w_data)` while
//! CONTROL fields (`s0`, `s1`, PC, BR, IR) are `flat(w_ctl = 8)`. Every field
//! router therefore carries a **per-field width vector** so its `SwapProd` /
//! `Assoc*Prod` nodes are typed on the field's real width, not a uniform
//! `word8()`. The uniform `fields`/`swap_field_adj`/`split_fields`/
//! `route_to_head_n`/`at_fields_n` entry points are thin wrappers that pass a
//! `[8; n]` width vector, so at the default layout the produced iso is exactly
//! the shipped `word8()`-typed one — **byte-identical**.

use crate::plumbing::seq_balanced;
use crate::types::{flat, word8, NUM_FIELDS};
use theseus_core::{Iso, Type};

// ── Right-nested product helpers (field-level, not bit-level) ──────

/// Right-nested product of fields with the given per-field bit-widths.
/// `fields(n) == fields_typed(&[8; n])` (byte-identical).
pub fn fields_typed(widths: &[usize]) -> Type {
    assert!(!widths.is_empty());
    if widths.len() == 1 {
        flat(widths[0])
    } else {
        Type::prod(flat(widths[0]), fields_typed(&widths[1..]))
    }
}

/// Right-nested product of `n` Word8 fields (uniform-width shorthand).
pub fn fields(n: usize) -> Type {
    assert!(n >= 1);
    let w = word8();
    if n == 1 {
        w
    } else {
        Type::prod(w, fields(n - 1))
    }
}

/// Swap adjacent fields at positions `k` and `k+1` in `fields_typed(widths)`.
/// Zero cells: pure product-structure plumbing on the two fields' real types.
fn swap_field_adj_typed(widths: &[usize], k: usize) -> Iso {
    let n = widths.len();
    assert!(k + 1 < n);
    if k > 0 {
        // Peel the head field (width widths[0]) as identity, recurse on the tail.
        return Iso::times(
            Iso::Id(flat(widths[0])),
            swap_field_adj_typed(&widths[1..], k - 1),
        );
    }
    // k == 0: swap the first two fields (types flat(widths[0]), flat(widths[1])).
    let a = flat(widths[0]);
    let b = flat(widths[1]);
    if n == 2 {
        Iso::SwapProd(a, b)
    } else {
        let rest = fields_typed(&widths[2..]);
        // (A x (B x R)) -> ((A x B) x R) -> ((B x A) x R) -> (B x (A x R))
        Iso::seq(
            Iso::seq(
                Iso::AssocLProd(a.clone(), b.clone(), rest.clone()),
                Iso::times(Iso::SwapProd(a.clone(), b.clone()), Iso::Id(rest.clone())),
            ),
            Iso::AssocRProd(b, a, rest),
        )
    }
}

/// Build the routing iso that moves fields at `indices` (in order) to
/// positions 0, 1, 2, ... at the head of a product whose per-field widths are
/// `widths`, using adjacent swaps. Returns the routing iso; its inverse is
/// `Iso::sym(route)`. Also returns the **permuted width vector** after routing
/// (so callers can reshape/split against the post-routing field order).
pub fn route_to_head_typed(indices: &[usize], widths: &[usize]) -> (Iso, Vec<usize>) {
    let n = widths.len();
    let mut arr: Vec<usize> = (0..n).collect();
    // `cur` tracks the live per-field width vector as swaps are applied.
    let mut cur: Vec<usize> = widths.to_vec();
    let mut swaps: Vec<Iso> = Vec::new();

    for (slot, &orig) in indices.iter().enumerate() {
        let p = arr
            .iter()
            .position(|&x| x == orig)
            .expect("field index in range");
        assert!(p >= slot, "routing: field already consumed or out of order");
        for k in (slot..p).rev() {
            swaps.push(swap_field_adj_typed(&cur, k));
            arr.swap(k, k + 1);
            cur.swap(k, k + 1);
        }
    }

    let route = if swaps.is_empty() {
        Iso::Id(fields_typed(widths))
    } else {
        seq_balanced(swaps)
    };
    (route, cur)
}

/// Uniform-width `route_to_head` (all fields Word8).
pub fn route_to_head_n(indices: &[usize], n_fields: usize) -> Iso {
    route_to_head_typed(indices, &vec![8usize; n_fields]).0
}

/// `fields_typed(left ++ right) <-> fields_typed(left) x fields_typed(right)`:
/// split the first `left.len()` fields out of the right-nested product as a
/// separate group. Widths are honored so the produced `Assoc*Prod` nodes are
/// typed on the real field types.
pub fn split_fields_typed(left: &[usize], right: &[usize]) -> Iso {
    assert!(!left.is_empty() && !right.is_empty());
    let head = flat(left[0]);
    if left.len() == 1 {
        // fields(head ++ right) already begins with `head`; done.
        Iso::Id(fields_typed(&[left, right].concat()))
    } else {
        // fields(k+r) = head x fields((k-1) ++ r)
        //  -> head x (fields(k-1) x fields(r))   [recursive split]
        //  -> (head x fields(k-1)) x fields(r)   [AssocL]
        //  = fields(k) x fields(r)
        Iso::seq(
            Iso::times(Iso::Id(head.clone()), split_fields_typed(&left[1..], right)),
            Iso::AssocLProd(head, fields_typed(&left[1..]), fields_typed(right)),
        )
    }
}

/// Uniform-width `split_fields` (all fields Word8).
pub fn split_fields(k: usize, r: usize) -> Iso {
    split_fields_typed(&vec![8usize; k], &vec![8usize; r])
}

/// Lift an operand-level iso at given field indices within a product whose
/// per-field widths are `widths`.
///
/// `op` operates on `fields_typed(addressed-widths)` (the addressed fields in
/// `indices` order). The result is an iso on `fields_typed(widths)` that
/// routes the addressed fields to the head, reshapes to
/// `fields_typed(addressed) x fields_typed(rest)`, applies `op x Id(rest)`,
/// and reshapes/routes back. All unaddressed fields pass through as identity.
pub fn at_fields_typed(op: Iso, indices: &[usize], widths: &[usize]) -> Iso {
    let n = widths.len();
    let k = indices.len();
    assert!(k >= 1 && k <= n);

    for &idx in indices {
        assert!(idx < n, "field index {idx} out of range");
    }
    for i in 0..k {
        for j in i + 1..k {
            assert_ne!(indices[i], indices[j], "duplicate field index");
        }
    }

    let (route, routed_widths) = route_to_head_typed(indices, widths);
    // After routing, the first `k` fields are the addressed ones (in order),
    // the remaining are the rest (their relative order preserved).
    let addressed = &routed_widths[..k];
    let rest = &routed_widths[k..];
    let rest_count = rest.len();

    if rest_count == 0 {
        Iso::seq(Iso::seq(route.clone(), op), Iso::sym(route))
    } else if k == 1 {
        let lifted = Iso::times(op, Iso::Id(fields_typed(rest)));
        Iso::seq(Iso::seq(route.clone(), lifted), Iso::sym(route))
    } else {
        let reshape = split_fields_typed(addressed, rest);
        let apply = Iso::times(op, Iso::Id(fields_typed(rest)));
        let inner = Iso::seq(Iso::seq(reshape.clone(), apply), Iso::sym(reshape));
        Iso::seq(Iso::seq(route.clone(), inner), Iso::sym(route))
    }
}

/// Uniform-width `at_fields_n` (all `n_fields` fields Word8).
pub fn at_fields_n(op: Iso, indices: &[usize], n_fields: usize) -> Iso {
    at_fields_typed(op, indices, &vec![8usize; n_fields])
}

/// Lift an operand-level iso to State level at given field indices.
/// Equivalent to `at_fields_n(op, indices, NUM_FIELDS)`.
pub fn at_fields(op: Iso, indices: &[usize]) -> Iso {
    at_fields_n(op, indices, NUM_FIELDS)
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
