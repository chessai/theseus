//! Shared bit-level plumbing for the TISC-1 datapath.
//!
//! These helpers are pure product-structure routing over `flat(n)` (a
//! right-nested product of `n` Bits) plus the two conservative prims
//! (`fredkin_prim`, `K1`) that every controlled construction reuses.
//! They lower to **zero cells** except the prims. The datapath modules
//! ([`crate::addressing`], [`crate::instr`], [`crate::decoder`],
//! [`crate::step`]) all build on this one vocabulary so the routing
//! logic lives in exactly one place and cannot drift between copies.
//!
//! Field-level routing (over Word8 fields rather than bits) lives in
//! [`crate::addressing`]; the convention-bearing prims (control-first
//! vs target-first CNOT, gated Fredkins, Toffoli/CCC-NOT) stay local to
//! the module whose encoding they belong to.

use theseus_core::{Iso, Type};

use crate::types::{bit, flat};

/// NOT on a single Bit = `SwapSum(1, 1)` — zero cells, pure rail swap.
pub(crate) fn not_iso() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// Number of Bit leaves in a product tree over Bit.
pub(crate) fn leaf_count(ty: &Type) -> usize {
    match ty {
        Type::Prod(a, b) => leaf_count(a) + leaf_count(b),
        _ => 1,
    }
}

/// `flat(a) x flat(b) <-> flat(a+b)`, pure assoc plumbing.
pub(crate) fn append(a: usize, b: usize) -> Iso {
    assert!(a >= 1 && b >= 1);
    if a == 1 {
        Iso::Id(flat(1 + b))
    } else {
        Iso::seq(
            Iso::AssocRProd(bit(), flat(a - 1), flat(b)),
            Iso::times(Iso::Id(bit()), append(a - 1, b)),
        )
    }
}

/// `ty <-> flat(leaf_count(ty))`, leaves in depth-first order.
pub(crate) fn flatten(ty: &Type) -> Iso {
    match ty {
        Type::Prod(a, b) => {
            let (na, nb) = (leaf_count(a), leaf_count(b));
            Iso::seq(Iso::times(flatten(a), flatten(b)), append(na, nb))
        }
        _ => Iso::Id(ty.clone()),
    }
}

/// Balanced sequential composition. `canonicalize` re-derives types at
/// every node (cost = Σ subtree sizes) and `eval` recurses per node, so
/// a left-leaning `reduce(Iso::seq)` chain over thousands of gates is
/// quadratic to lower and deep enough to overflow the stack; a balanced
/// seq tree keeps both O(n log n) and shallow. Seq is associative, so
/// the meaning is unchanged.
pub(crate) fn seq_balanced(mut isos: Vec<Iso>) -> Iso {
    assert!(!isos.is_empty());
    if isos.len() == 1 {
        return isos.pop().expect("len 1");
    }
    let right = isos.split_off(isos.len() / 2);
    Iso::seq(seq_balanced(isos), seq_balanced(right))
}

/// Sequential composition; empty = identity on `flat(n)`.
pub(crate) fn seq_list(n: usize, isos: Vec<Iso>) -> Iso {
    if isos.is_empty() {
        Iso::Id(flat(n))
    } else {
        seq_balanced(isos)
    }
}

/// Swap adjacent leaves `k` and `k+1` of `flat(n)`.
pub(crate) fn swap_adj(n: usize, k: usize) -> Iso {
    assert!(k + 1 < n);
    if k > 0 {
        return Iso::times(Iso::Id(bit()), swap_adj(n - 1, k - 1));
    }
    if n == 2 {
        Iso::SwapProd(bit(), bit())
    } else {
        let r = flat(n - 2);
        Iso::seq(
            Iso::seq(
                Iso::AssocLProd(bit(), bit(), r.clone()),
                Iso::times(Iso::SwapProd(bit(), bit()), Iso::Id(r.clone())),
            ),
            Iso::AssocRProd(bit(), bit(), r),
        )
    }
}

/// Apply a 1-wire iso at leaf `k` of `flat(n)`.
pub(crate) fn at_wire(n: usize, k: usize, iso1: Iso) -> Iso {
    assert!(k < n);
    if k > 0 {
        Iso::times(Iso::Id(bit()), at_wire(n - 1, k - 1, iso1))
    } else if n == 1 {
        iso1
    } else {
        Iso::times(iso1, Iso::Id(flat(n - 1)))
    }
}

/// `flat(k+r) <-> flat(k) x flat(r)`: group the first k leaves.
pub(crate) fn split_bits(k: usize, r: usize) -> Iso {
    assert!(k >= 1 && r >= 1);
    if k == 1 {
        Iso::Id(flat(1 + r))
    } else {
        Iso::seq(
            Iso::times(Iso::Id(bit()), split_bits(k - 1, r)),
            Iso::AssocLProd(bit(), flat(k - 1), flat(r)),
        )
    }
}

/// Apply a k-wire prim (`flat(k) <-> flat(k)`) at the head of `flat(n)`.
pub(crate) fn head_k(n: usize, k: usize, prim: Iso) -> Iso {
    assert!(k >= 1 && k <= n);
    if n == k {
        return prim;
    }
    let reshape = split_bits(k, n - k);
    Iso::seq(
        Iso::seq(reshape.clone(), Iso::times(prim, Iso::Id(flat(n - k)))),
        Iso::sym(reshape),
    )
}

/// Apply a k-wire prim at the given leaves of `flat(n)` (bubble-route
/// the leaves to the head, apply, route back — all routing zero cells).
pub(crate) fn gate_at(n: usize, wires: &[usize], prim: Iso) -> Iso {
    let k = wires.len();
    assert!((1..=5).contains(&k));
    let mut arr: Vec<usize> = (0..n).collect();
    let mut swaps: Vec<Iso> = Vec::new();
    for (slot, &orig) in wires.iter().enumerate() {
        let p = arr.iter().position(|&x| x == orig).expect("leaf present");
        assert!(p >= slot, "bubble-routing invariant");
        for j in (slot..p).rev() {
            swaps.push(swap_adj(n, j));
            arr.swap(j, j + 1);
        }
    }
    let route = seq_list(n, swaps);
    Iso::seq(Iso::seq(route.clone(), head_k(n, k, prim)), Iso::sym(route))
}

/// Transpositions realizing a fixed left-rotation-by-`s` of 8 wire slots.
pub(crate) fn rot_transpositions(s: usize) -> Vec<(usize, usize)> {
    let mut arr: Vec<usize> = (0..8).collect();
    let mut out = Vec::new();
    for j in 0..8 {
        let want = (j + 8 - s) % 8;
        let p = arr.iter().position(|&x| x == want).expect("bit present");
        if p != j {
            out.push((j, p));
            arr.swap(j, p);
        }
    }
    out
}

/// The 3-wide Fredkin prim `(c, a, b)`: swap a,b iff c. K1 fast path.
pub(crate) fn fredkin_prim() -> Iso {
    use theseus_netlist::{prim_from_bits, CellKind};
    let t3 = flat(3);
    Iso::Prim(Box::new(
        prim_from_bits("fredkin", t3.clone(), t3, |b| CellKind::K1.eval(b)).expect("fredkin"),
    ))
}
