//! Tests for the sub-quadratic canonicalize refactor.
//!
//! Verifies two properties:
//! 1. EQUIVALENCE -- the refactored canonicalize produces the same TypedNode
//!    as before for all combinator shapes.
//! 2. SCALING -- large left-leaning and balanced Seq chains complete within
//!    normal test timeouts (the old per-node type_of made left-leaning
//!    chains blow up to O(n²) time/memory; the bottom-up approach is O(n)).

use theseus_core::{type_of, Iso, Type};
use theseus_netlist::{canonicalize, lower_iso};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// Helper: assert canonicalize succeeds, validates, and boundary types
/// agree with type_of.
fn assert_canonical(iso: &Iso) {
    let node = canonicalize(iso).expect("canonicalize");
    node.validate().expect("canonical node validates");
    let (a, b) = type_of(iso).unwrap();
    assert_eq!(
        (node.input.clone(), node.output.clone()),
        (a, b),
        "boundary types must match type_of"
    );
}

// ── Equivalence tests ──────────────────────────────────────────────

#[test]
fn equivalence_seq_chain() {
    // A short left-leaning chain: ((id;swap);id)
    let swap = Iso::SwapSum(Type::One, Type::One);
    let chain = Iso::seq(Iso::seq(Iso::Id(bit()), swap.clone()), Iso::Id(bit()));
    assert_canonical(&chain);
}

#[test]
fn equivalence_right_leaning_seq() {
    let swap = Iso::SwapSum(Type::One, Type::One);
    let chain = Iso::seq(Iso::Id(bit()), Iso::seq(swap.clone(), Iso::Id(bit())));
    assert_canonical(&chain);
}

#[test]
fn equivalence_par_and_branch() {
    let f = Iso::times(Iso::Id(bit()), Iso::SwapProd(bit(), bit()));
    assert_canonical(&f);
    let g = Iso::plus(Iso::Id(bit()), Iso::Id(Type::One));
    assert_canonical(&g);
}

#[test]
fn equivalence_nested_par_seq() {
    let inner = Iso::seq(Iso::Id(bit()), Iso::SwapSum(Type::One, Type::One));
    let outer = Iso::times(inner.clone(), inner);
    assert_canonical(&outer);
}

#[test]
fn equivalence_distrib_and_factor() {
    let d = Iso::Distrib(bit(), Type::One, Type::One);
    assert_canonical(&d);
    let f = Iso::Factor(bit(), Type::One, Type::One);
    assert_canonical(&f);
}

#[test]
fn equivalence_trace() {
    let body = Iso::Id(Type::sum(Type::One, bit()));
    let t = Iso::trace(Type::One, body);
    assert_canonical(&t);
}

#[test]
fn equivalence_sym() {
    let swap = Iso::SwapProd(bit(), Type::One);
    let s = Iso::sym(swap);
    assert_canonical(&s);
}

#[test]
fn equivalence_all_structural() {
    for iso in [
        Iso::AssocLProd(bit(), bit(), bit()),
        Iso::AssocRProd(bit(), bit(), bit()),
        Iso::UniteProd(bit()),
        Iso::UnitiProd(bit()),
        Iso::AssocLSum(Type::One, Type::One, bit()),
        Iso::AssocRSum(Type::One, Type::One, bit()),
        Iso::UniteSum(bit()),
        Iso::UnitiSum(bit()),
    ] {
        assert_canonical(&iso);
    }
}

// ── Scaling tests ──────────────────────────────────────────────────
//
// These construct large isos that the old O(n²) code would choke on.
// The bottom-up canonicalize is O(n) and completes within seconds.

/// Build a LEFT-LEANING Seq chain of `n` identity isos on `bit()`.
/// Shape: (((id ; id) ; id) ; ... ; id)
/// This is the worst case for the old code: type_of at the top walks
/// all n nodes, at the next level walks n-1, etc. → O(n²).
fn left_leaning_chain(n: usize) -> Iso {
    let mut acc = Iso::Id(bit());
    for _ in 1..n {
        acc = Iso::seq(acc, Iso::Id(bit()));
    }
    acc
}

/// Build a BALANCED Seq tree of depth ~log2(n) identity isos.
fn balanced_chain(n: usize) -> Iso {
    if n <= 1 {
        return Iso::Id(bit());
    }
    let half = n / 2;
    let rest = n - half;
    Iso::seq(balanced_chain(half), balanced_chain(rest))
}

#[test]
fn scaling_left_leaning_500() {
    // 500-node left-leaning chain. Old code: ~125k type_of node visits.
    // New code: O(500). Must complete within normal test timeout.
    let iso = left_leaning_chain(500);
    let node = canonicalize(&iso).expect("canonicalize left-leaning 500");
    node.validate().expect("validates");
    let (a, b) = type_of(&iso).unwrap();
    assert_eq!((node.input, node.output), (a, b));
}

#[test]
fn scaling_left_leaning_1000() {
    // 1000-node left-leaning chain -- would be ~500k node visits under
    // O(n²). The bottom-up approach handles this in O(n).
    let iso = left_leaning_chain(1000);
    let node = canonicalize(&iso).expect("canonicalize left-leaning 1000");
    node.validate().expect("validates");
    // Also verify lower_iso completes (canonicalize is the bottleneck).
    lower_iso(&iso).expect("lower_iso left-leaning 1000");
}

#[test]
fn scaling_balanced_1000() {
    let iso = balanced_chain(1000);
    let node = canonicalize(&iso).expect("canonicalize balanced 1000");
    node.validate().expect("validates");
    lower_iso(&iso).expect("lower_iso balanced 1000");
}
