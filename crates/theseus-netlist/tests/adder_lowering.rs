//! Differential-oracle test for the compositional reversible half-adder
//! (`conformance/programs/0024-adder.thse`).
//!
//! Proves the full surface→cells chain: the half-adder composed from prelude
//! cnot + toffoli gates parses, checks, elaborates (via the implicit prelude),
//! and lowers to a cell netlist whose untimed evaluation agrees with the
//! interpreter over the ENTIRE finite domain (2^4 = 16 inputs).

mod common;
use common::*;

const ADDER_SRC: &str = include_str!("../../../conformance/programs/0024-adder.thse");

#[test]
fn surface_ha4_lowers_and_agrees_with_eval() {
    let iso = elaborate_with_prelude(ADDER_SRC, "ha4");
    assert_lowering_agrees(&iso, 100_000, None);
}
