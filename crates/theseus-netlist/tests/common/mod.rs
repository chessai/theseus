//! Shared test helpers for the `theseus-netlist` integration tests.
//!
//! These consolidate the copy-pasted per-file helpers (untimed dual-rail
//! evaluation, source elaboration, ancilla plumbing, and the differential
//! oracle) into one place. Everything here uses only the crate's PUBLIC
//! API -- this is an external test crate.
//!
//! The dual-rail 1-of-2 INV-TOKEN encode/decode is NOT reimplemented here:
//! [`eval_cells`] delegates to [`RailPair::set`] / [`RailPair::get`], the
//! same authoritative methods the production evaluators (`sim`, `balance`,
//! `lower_iter`) use, so the invariant lives in exactly one place.

// Each test binary pulls in the whole module but uses only a subset of
// these helpers; without this the unused ones would trip
// `clippy --deny warnings` in that binary.
#![allow(dead_code)]

use theseus_core::{enumerate_values, type_of, Iso, Type};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{bits_of, rail_width, PhasedCellNetlist};

/// The `Bit` type: `1 + 1`.
pub fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// The three-bit product `Bit x (Bit x Bit)`.
pub fn three_bits() -> Type {
    Type::prod(bit(), Type::prod(bit(), bit()))
}

/// Untimed functional evaluation of a cell netlist: propagate token
/// values line by line under the 1-of-2 dual-rail discipline, asserting
/// INV-TOKEN at every pair read. Handles ancilla inputs transparently
/// (seed them via [`extend_with_ancilla`]).
///
/// Encode/decode go through [`RailPair::set`] / [`RailPair::get`] so the
/// test evaluator and the production evaluators share one INV-TOKEN
/// definition.
pub fn eval_cells(net: &PhasedCellNetlist, input_bits: &[bool]) -> Vec<bool> {
    let mut line: Vec<Option<bool>> = vec![None; net.num_lines as usize];
    assert_eq!(input_bits.len(), net.inputs.len());
    for (&p, &v) in net.inputs.iter().zip(input_bits) {
        p.set(&mut line, v);
    }
    for cell in &net.cells {
        let vals: Vec<bool> = cell.inputs.iter().map(|&p| p.get(&line)).collect();
        let outs = cell.kind.eval(&vals);
        for (&p, &v) in cell.outputs.iter().zip(&outs) {
            p.set(&mut line, v);
        }
    }
    net.outputs.iter().map(|&p| p.get(&line)).collect()
}

/// Append a lowering's real ancilla init values to the data bits
/// (positional, as returned by `lower_iso_with_ancillas`).
pub fn extend_with_ancilla(data_bits: &[bool], ancillas: &[bool]) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    bits.extend_from_slice(ancillas);
    bits
}

/// Elaborate a Theseus source program to the named iso (parse -> check ->
/// resolve -> elaborate), without the implicit prelude.
pub fn elaborate(src: &str, iso_name: &str) -> Iso {
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

/// Like [`elaborate`], but merges the implicit prelude (`with_prelude`)
/// before checking -- for programs that use prelude combinators.
pub fn elaborate_with_prelude(src: &str, iso_name: &str) -> Iso {
    let program = theseus_syntax::parse(src).expect("parse");
    let merged = theseus_check::prelude::with_prelude(&program);
    let diags = theseus_check::check(&merged);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = theseus_check::env::resolve(&merged);
    let isos = theseus_check::elaborate::elaborate_program(&merged, &env);
    isos.get(iso_name)
        .unwrap_or_else(|| panic!("iso {iso_name:?} not found in elaboration"))
        .clone()
        .unwrap_or_else(|e| panic!("elaboration of {iso_name:?} failed: {e:?}"))
}

/// Differential oracle over the WHOLE finite input domain: for every
/// input value, the lowered netlist's untimed evaluation (data outputs)
/// must equal `theseus_eval::eval`. Returns the lowered netlist.
///
/// `fuel` is the per-value evaluation budget. `extra_check`, if given, is
/// invoked once on the lowered netlist before the sweep -- use it for
/// vocabulary/balance assertions specific to a call site.
pub fn assert_lowering_agrees(
    iso: &Iso,
    fuel: u64,
    extra_check: Option<&dyn Fn(&PhasedCellNetlist)>,
) -> PhasedCellNetlist {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (net, ancillas) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    assert_eq!(net.validate(), Ok(()), "lowered netlist validates");
    if let Some(check) = extra_check {
        check(&net);
    }
    let n_data_out = rail_width(&out_ty).unwrap();
    let mut checked = 0usize;
    for v in enumerate_values(&in_ty) {
        let full_input = extend_with_ancilla(&bits_of(&v, &in_ty).unwrap(), &ancillas);
        let got_full = eval_cells(&net, &full_input);
        let got = &got_full[..n_data_out];
        let mut fuel = fuel;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        assert_eq!(
            got,
            &bits_of(&expected, &out_ty).unwrap()[..],
            "netlist disagrees with core eval on {v:?}"
        );
        checked += 1;
    }
    assert!(checked > 0, "domain was empty");
    net
}
