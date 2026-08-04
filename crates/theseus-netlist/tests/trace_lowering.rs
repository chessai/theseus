//! Differential oracle tests for TraceLoop unroll lowering (ADR-0026 C2.4).
//!
//! Each test asserts that for EVERY input value in the finite domain,
//! the lowered cell netlist produces the same output as the core
//! interpreter (`theseus_eval::eval`).  Additionally, `validate()` must
//! pass and the untimed simulation must never violate INV-TOKEN.

use theseus_core::{enumerate_values, type_of, Iso, Type};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{bits_of, lower_iso, rail_width};

mod common;
use common::*;

// ═══════════════════════════════════════════════════════════════════════
// (a) Hand-built simple trace with small carrier (Bit)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn trace_trivial_swap_carrier_one() {
    // Trace(One, SwapSum(One, One)): carrier = One (|u| = 1).
    // Body swaps Left↔Right on (1+1).
    // Semantics: input Unit → Right(Unit) → body → Left(Unit) → body → Right(Unit) → exit Unit.
    // Net effect: identity on One.
    let iso = Iso::trace(Type::One, Iso::SwapSum(Type::One, Type::One));
    assert_lowering_agrees(&iso, 100_000, None);
}

#[test]
fn trace_identity_bit_carrier_one() {
    // Trace(One, Id(One + Bit)): carrier = One, body = identity.
    // Semantics: input b → Right(b) → body(Right(b)) = Right(b) → exit b.
    // The loop never iterates (first body application already exits).
    // Net effect: identity on Bit.
    let iso = Iso::trace(Type::One, Iso::Id(Type::sum(Type::One, bit())));
    assert_lowering_agrees(&iso, 100_000, None);
}

#[test]
fn trace_bit_carrier_swap_body() {
    // Trace(Bit, SwapSum(Bit, Bit)): carrier = Bit (|u| = 2).
    // Body : (Bit + Bit) <-> (Bit + Bit) swaps Left↔Right.
    // Semantics: input b → Right(b) → body → Left(b) → body → Right(b) → exit b.
    // Always 2 body applications. Net effect: identity on Bit.
    let iso = Iso::trace(bit(), Iso::SwapSum(bit(), bit()));
    assert_lowering_agrees(&iso, 100_000, None);
}

// ═══════════════════════════════════════════════════════════════════════
// (b) Conformance: label-loop program (0011-label-loop.thse)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn conformance_label_loop_spin() {
    // The "spin" iso from the conformance suite: a 2-bit counter walk
    // with carrier = (Bit, (Bit, Bit)).  It uses a trace/label-loop
    // that walks through 4 counter states before exiting.
    let src = include_str!("../../../conformance/programs/0011-label-loop.thse");
    let iso = elaborate(src, "spin");
    assert_lowering_agrees(&iso, 100_000, None);
}

// ═══════════════════════════════════════════════════════════════════════
// (c) validate() Ok and INV-TOKEN on every produced netlist
// ═══════════════════════════════════════════════════════════════════════
// (These are already checked inside assert_lowering_agrees: validate()
// is called, and eval_cells asserts INV-TOKEN at every pair read.)

#[test]
fn trace_validate_and_inv_token_trivial() {
    // Redundant explicit check: validate returns Ok.
    let iso = Iso::trace(Type::One, Iso::SwapSum(Type::One, Type::One));
    let net = lower_iso(&iso).unwrap();
    assert_eq!(net.validate(), Ok(()));
}

#[test]
fn trace_validate_and_inv_token_label_loop() {
    let src = include_str!("../../../conformance/programs/0011-label-loop.thse");
    let iso = elaborate(src, "spin");
    let net = lower_iso(&iso).unwrap();
    assert_eq!(net.validate(), Ok(()));
}

#[test]
fn spin_balances_and_sims_with_inv_token() {
    // End-to-end for the synthesized trace body: lower spin (whose body
    // is a 4-bit prim handled by B4.2 general synthesis), balance, and
    // run the TIMED phased sim -- which asserts dynamic INV-TOKEN for
    // every rail pair on every tick -- against core eval on both inputs.
    let src = include_str!("../../../conformance/programs/0011-label-loop.thse");
    let iso = elaborate(src, "spin");
    let (net, ancilla_values) = lower_iso_with_ancillas(&iso).expect("lowers");
    let balanced = theseus_netlist::balance(&net).expect("balances");
    let (in_ty, out_ty) = type_of(&iso).unwrap();
    let n_data_out = rail_width(&out_ty).unwrap();
    for v in enumerate_values(&in_ty) {
        let mut bits = bits_of(&v, &in_ty).unwrap();
        bits.extend_from_slice(&ancilla_values);
        let res = theseus_netlist::simulate(&balanced, &bits).expect("timed sim");
        let mut fuel = 100_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(
            &res.outputs[..n_data_out],
            &bits_of(&expected, &out_ty).unwrap()[..],
            "timed sim disagrees with core eval on {v:?}"
        );
    }
}
