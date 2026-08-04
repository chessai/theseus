//! Regression tests for controlled-op lowering correctness (M3b-A2).
//!
//! The bug: zero-cell arms (Id / NOT-as-rail-swap) in Branch lowering
//! are applied unconditionally, so the per-arm transformation is lost.
//! These tests exercise the differential oracle over the WHOLE finite
//! domain for elaborated controlled-NOT, Toffoli, and swapMaybe.

use theseus_core::{Iso, Type};

mod common;
use common::*;

// ── Elaborated conformance programs ─────────────────────────────────

const CNOT_SRC: &str = "\
type Bit = O | I

iso cnot : (Bit, Bit) <-> (Bit, Bit)
| (O, b) <-> (O, b)
| (I, O) <-> (I, I)
| (I, I) <-> (I, O)
";

#[test]
fn elaborated_cnot_agrees_with_eval() {
    let iso = elaborate(CNOT_SRC, "cnot");
    assert_lowering_agrees(&iso, 10_000, None);
}

#[test]
fn elaborated_toffoli_agrees_with_eval() {
    let src = include_str!("../../../conformance/programs/0006-toffoli.thse");
    let iso = elaborate(src, "tof");
    assert_lowering_agrees(&iso, 10_000, None);
}

#[test]
fn elaborated_swap_maybe_agrees_with_eval() {
    let src = include_str!("../../../conformance/programs/0001-swap-maybe.thse");
    let iso = elaborate(src, "swapMaybe");
    assert_lowering_agrees(&iso, 10_000, None);
}

// ── Branch with zero-cell NOT arms (direct Iso construction) ────────

/// Build a NOT iso on Bit (= SwapSum(1,1)).
fn not_iso() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

#[test]
fn branch_not_id_on_bit_sum() {
    // Plus(NOT, Id) on Bit + Bit: when tag=true, NOT; when tag=false, Id.
    let iso = Iso::plus(not_iso(), Iso::Id(bit()));
    assert_lowering_agrees(&iso, 10_000, None);
}

#[test]
fn branch_id_not_on_bit_sum() {
    // Plus(Id, NOT) on Bit + Bit: when tag=false, NOT; when tag=true, Id.
    let iso = Iso::plus(Iso::Id(bit()), not_iso());
    assert_lowering_agrees(&iso, 10_000, None);
}

#[test]
fn branch_not_not_on_bit_sum() {
    // Plus(NOT, NOT) on Bit + Bit: always NOT (both arms).
    let iso = Iso::plus(not_iso(), not_iso());
    assert_lowering_agrees(&iso, 10_000, None);
}

#[test]
fn branch_id_not_unequal_widths() {
    // Plus(Id(One), NOT) on (One + Bit): swapMaybe-like.
    // wa=0, wb=1.
    let iso = Iso::plus(Iso::Id(Type::One), not_iso());
    assert_lowering_agrees(&iso, 10_000, None);
}

#[test]
fn branch_not_id_unequal_widths() {
    // Plus(NOT, Id(One)) on (Bit + One): NOT on left arm.
    // wa=1, wb=0.
    let iso = Iso::plus(not_iso(), Iso::Id(Type::One));
    assert_lowering_agrees(&iso, 10_000, None);
}
