//! Acceptance tests for the M3b energy artifact: compile a 5-wire
//! full adder and price it in joules from the real rung-0 ngspice
//! characterization (measured once, loaded from `data/rung0-chardb.ron`).

use theseus_core::{enumerate_values, type_of, Iso};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{balance, bits_of, rail_width, simulate, CellKind, PhasedCellNetlist};
use theseus_spice::annotate::compiled_energy;
use theseus_spice::chardb;

// ── Full adder source generation ────────────────────────────────────

fn full_adder_source() -> String {
    let mut src = String::from(
        "type Bit = O | I\n\niso fa5 : (Bit, Bit, Bit, Bit, Bit) <-> (Bit, Bit, Bit, Bit, Bit)\n",
    );
    for v in 0u32..32 {
        let a = v & 1 == 1;
        let b = (v >> 1) & 1 == 1;
        let cin = (v >> 2) & 1 == 1;
        let s = (v >> 3) & 1 == 1;
        let c = (v >> 4) & 1 == 1;
        let sum = a ^ b ^ cin;
        let maj = (a & b) | (a & cin) | (b & cin);
        let bs = |x: bool| if x { "I" } else { "O" };
        src.push_str(&format!(
            "| ({}, {}, {}, {}, {}) <-> ({}, {}, {}, {}, {})\n",
            bs(a),
            bs(b),
            bs(cin),
            bs(s),
            bs(c),
            bs(a),
            bs(b),
            bs(cin),
            bs(s ^ sum),
            bs(c ^ maj)
        ));
    }
    src
}

// ── Elaborate helper ────────────────────────────────────────────────

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

// ── Extend input with ancilla bits ──────────────────────────────────

// Append the netlist's REQUIRED ancilla initial values to the data bits.
// The R1/K1 garbage-free construction (ADR-0040) is only ancilla-clean --
// and only computes the right function -- when every ancilla starts at the
// value the lowering allocated it with (K1 AND-chains need 0; a k=0 R1
// control needs 1). Feeding a guessed pattern corrupts those reductions
// and silently produces a wrong bit, so the differential oracle must use
// the real values from `lower_iso_with_ancillas`, not invent them.
fn extend_with_ancilla(data_bits: &[bool], ancilla_values: &[bool]) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    bits.extend_from_slice(ancilla_values);
    bits
}

// ── Compile pipeline (shared by tests) ──────────────────────────────

fn compile_fa5() -> (Iso, PhasedCellNetlist, Vec<bool>) {
    let src = full_adder_source();
    let iso = elaborate(&src, "fa5");
    let (net, ancilla_values) = lower_iso_with_ancillas(&iso).expect("fa5 lowering succeeds");
    let balanced = balance(&net).expect("balance succeeds");
    // balance() maps primary inputs 1:1 in order, so the ancilla init
    // vector still lines up with balanced.inputs[n_data..].
    (iso, balanced, ancilla_values)
}

// ── Test (a): energy report has total_j > 0 with K0 and R1 breakdown ──

#[test]
fn energy_report_has_k0_and_r1() {
    let (_iso, balanced, _ancilla_values) = compile_fa5();
    let db = chardb::rung0_measured();
    let t_ramp = 1e-7;

    let report = compiled_energy(&balanced, &db, t_ramp).expect("energy annotation succeeds");

    assert!(
        report.total_j > 0.0,
        "total_j must be positive, got {}",
        report.total_j
    );
    assert!(
        report.by_kind.contains_key("cswap-k0"),
        "breakdown must contain cswap-k0; got keys: {:?}",
        report.by_kind.keys().collect::<Vec<_>>()
    );
    // The full adder now lowers its conditional NOTs to the R1 rail-role
    // swap (ADR-0040), so the garbage-free construction uses the K1-chain
    // + R1 vocabulary rather than a K3 swap. Assert the R1 path that ACTUALLY
    // fires (do NOT force the retired k3 path back).
    assert!(
        report.by_kind.contains_key("cswap-r1"),
        "breakdown must contain cswap-r1; got keys: {:?}",
        report.by_kind.keys().collect::<Vec<_>>()
    );

    // Verify counts match netlist cell_counts.
    let counts = balanced.cell_counts();
    assert_eq!(
        report.by_kind["cswap-k0"].count,
        counts[&CellKind::K0],
        "K0 count mismatch"
    );
    assert_eq!(
        report.by_kind["cswap-r1"].count,
        counts[&CellKind::R1],
        "R1 count mismatch"
    );

    eprintln!("energy report:\n{report}");
}

// ── Test (b): differential oracle over all 32 inputs ────────────────

#[test]
fn differential_oracle_all_32_inputs() {
    let (iso, balanced, ancilla_values) = compile_fa5();
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let n_data_out = rail_width(&out_ty).unwrap();

    for v in enumerate_values(&in_ty) {
        let data_bits = bits_of(&v, &in_ty).unwrap();
        let full_input = extend_with_ancilla(&data_bits, &ancilla_values);
        let sim = simulate(&balanced, &full_input)
            .unwrap_or_else(|e| panic!("timed sim failed on {v:?}: {e:?}"));
        let sim_data = &sim.outputs[..n_data_out];

        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        let expected_bits = bits_of(&expected, &out_ty).unwrap();
        assert_eq!(
            sim_data,
            &expected_bits[..],
            "timed sim disagrees with core eval on {v:?}"
        );
    }
}

// ── Test (c): determinism -- compiled_energy stable across two runs ───

#[test]
fn energy_is_deterministic() {
    let db = chardb::rung0_measured();
    let t_ramp = 1e-7;

    let (_iso1, balanced1, _anc1) = compile_fa5();
    let report1 = compiled_energy(&balanced1, &db, t_ramp).expect("run 1");

    let (_iso2, balanced2, _anc2) = compile_fa5();
    let report2 = compiled_energy(&balanced2, &db, t_ramp).expect("run 2");

    assert_eq!(
        report1, report2,
        "energy reports must be identical across two runs"
    );
}
