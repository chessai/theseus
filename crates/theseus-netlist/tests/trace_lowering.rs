//! Differential oracle tests for TraceLoop unroll lowering (ADR-0026 C2.4).
//!
//! Each test asserts that for EVERY input value in the finite domain,
//! the lowered cell netlist produces the same output as the core
//! interpreter (`theseus_eval::eval`).  Additionally, `validate()` must
//! pass and the untimed simulation must never violate INV-TOKEN.

use theseus_core::{enumerate_values, type_of, Iso, Type};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{bits_of, lower_iso, rail_width, PhasedCellNetlist, RailPair};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// Untimed functional evaluation of the cell netlist.  All input pairs
/// (data + ancilla) must be provided in `input_bits`.
fn eval_cells(net: &PhasedCellNetlist, input_bits: &[bool]) -> Vec<bool> {
    let mut line: Vec<Option<bool>> = vec![None; net.num_lines as usize];
    fn set_pair(line: &mut [Option<bool>], p: RailPair, v: bool) {
        line[p.line1.0 as usize] = Some(v);
        line[p.line0.0 as usize] = Some(!v);
    }
    fn get_pair(line: &[Option<bool>], p: RailPair) -> bool {
        let v1 = line[p.line1.0 as usize].expect("1-line driven");
        let v0 = line[p.line0.0 as usize].expect("0-line driven");
        assert_ne!(v0, v1, "INV-TOKEN violated: pair not 1-of-2");
        v1
    }

    assert_eq!(input_bits.len(), net.inputs.len());
    for (&p, &v) in net.inputs.iter().zip(input_bits) {
        set_pair(&mut line, p, v);
    }
    for cell in &net.cells {
        let vals: Vec<bool> = cell.inputs.iter().map(|&p| get_pair(&line, p)).collect();
        let outs = cell.kind.eval(&vals);
        for (&p, &v) in cell.outputs.iter().zip(&outs) {
            set_pair(&mut line, p, v);
        }
    }
    net.outputs.iter().map(|&p| get_pair(&line, p)).collect()
}

/// Provide data bits plus the correct ancilla initial values.
fn extend_with_ancilla(data_bits: &[bool], ancilla_values: &[bool]) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    bits.extend_from_slice(ancilla_values);
    bits
}

/// Differential oracle: for every input value, the lowered netlist's
/// untimed evaluation must agree with `theseus_eval::eval`.  Handles
/// ancilla inputs/outputs transparently using the actual ancilla values
/// from the lowering.
fn assert_lowering_agrees(iso: &Iso) -> PhasedCellNetlist {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (netlist, ancilla_values) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    assert_eq!(netlist.validate(), Ok(()), "lowered netlist validates");
    let n_data_out = rail_width(&out_ty).unwrap();
    for v in enumerate_values(&in_ty) {
        let data_bits = bits_of(&v, &in_ty).unwrap();
        let full_input = extend_with_ancilla(&data_bits, &ancilla_values);
        let got_full = eval_cells(&netlist, &full_input);
        let got = &got_full[..n_data_out];
        let mut fuel = 100_000u64;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        assert_eq!(
            got,
            &bits_of(&expected, &out_ty).unwrap()[..],
            "netlist disagrees with core eval on {v:?}"
        );
    }
    netlist
}

// ── Helper: elaborate a Theseus source program to an Iso ────────────

fn elaborate(src: &str, iso_name: &str) -> Iso {
    let program = theseus_syntax::parse(src).expect("parse");
    let diags = theseus_check::check(&program);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = theseus_check::env::resolve(&program);
    let isos = theseus_check::elaborate::elaborate_program(&program, &env);
    isos.get(iso_name)
        .unwrap_or_else(|| panic!("iso {iso_name:?} not found in elaboration"))
        .clone()
        .unwrap_or_else(|e| panic!("elaboration of {iso_name:?} failed: {e:?}"))
}

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
    assert_lowering_agrees(&iso);
}

#[test]
fn trace_identity_bit_carrier_one() {
    // Trace(One, Id(One + Bit)): carrier = One, body = identity.
    // Semantics: input b → Right(b) → body(Right(b)) = Right(b) → exit b.
    // The loop never iterates (first body application already exits).
    // Net effect: identity on Bit.
    let iso = Iso::trace(Type::One, Iso::Id(Type::sum(Type::One, bit())));
    assert_lowering_agrees(&iso);
}

#[test]
fn trace_bit_carrier_swap_body() {
    // Trace(Bit, SwapSum(Bit, Bit)): carrier = Bit (|u| = 2).
    // Body : (Bit + Bit) <-> (Bit + Bit) swaps Left↔Right.
    // Semantics: input b → Right(b) → body → Left(b) → body → Right(b) → exit b.
    // Always 2 body applications. Net effect: identity on Bit.
    let iso = Iso::trace(bit(), Iso::SwapSum(bit(), bit()));
    assert_lowering_agrees(&iso);
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
    assert_lowering_agrees(&iso);
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
