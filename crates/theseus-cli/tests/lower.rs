//! Integration tests for the surface → cells lowering pipeline.

use theseus_cli::lower::{self, LowerError};

const CNOT_SRC: &str = include_str!("../../../conformance/programs/0004-cnot.thse");
const FIB_SRC: &str = include_str!("../../../conformance/programs/0022-fib.thse");

#[test]
fn cnot_surface_to_cells_validates_and_oracle_agrees() {
    let lowered = lower::surface_to_cells(CNOT_SRC, "cnot")
        .expect("surface_to_cells should succeed for cnot");
    assert_eq!(lowered.net.validate(), Ok(()), "netlist must validate");
    let n = lower::oracle_check(&lowered.iso, &lowered)
        .expect("oracle_check should pass for cnot");
    assert!(n > 0, "should have checked at least one input");
}

#[test]
fn fib_surface_to_cells_validates_and_oracle_agrees() {
    let lowered = lower::surface_to_cells(FIB_SRC, "fib")
        .expect("surface_to_cells should succeed for fib");
    assert_eq!(lowered.net.validate(), Ok(()), "netlist must validate");
    let n = lower::oracle_check(&lowered.iso, &lowered)
        .expect("oracle_check should pass for fib");
    assert_eq!(n, 16, "fib domain should have exactly 16 inputs");
}

/// `Bit` and `cnot` come from the implicit prelude -- no local declarations.
#[test]
fn prelude_cnot_lowers_and_oracle_agrees() {
    // Neither Bit nor cnot are declared; both come from the prelude.
    let src = "iso uncnot : Bit * Bit <-> Bit * Bit = ~cnot\n";
    let lowered = lower::surface_to_cells(src, "uncnot")
        .expect("surface_to_cells should succeed with prelude-provided cnot/Bit");
    assert_eq!(lowered.net.validate(), Ok(()), "netlist must validate");
    let n = lower::oracle_check(&lowered.iso, &lowered)
        .expect("oracle_check should pass for prelude-cnot adjoint");
    assert_eq!(n, 4, "Bit*Bit domain should have exactly 4 inputs");
}

#[test]
fn unknown_iso_name_returns_error() {
    let result = lower::surface_to_cells(CNOT_SRC, "nonexistent");
    match result {
        Err(LowerError::UnknownIso(_)) => {}
        Err(other) => panic!("expected UnknownIso, got {other:?}"),
        Ok(_) => panic!("expected UnknownIso error, got Ok"),
    }
}
