//! Acceptance tests for the M3b energy artifact: compile a 5-wire
//! full adder and price it in joules from the real rung-0 ngspice
//! characterization (measured once, loaded from `data/rung0-chardb.ron`).

use theseus_core::{enumerate_values, type_of, Iso};
use theseus_netlist::{balance, bits_of, lower_iso, rail_width, simulate, CellKind, PhasedCellNetlist};
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

fn extend_with_ancilla(data_bits: &[bool], net: &PhasedCellNetlist) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    let n_ancilla = net.inputs.len() - data_bits.len();
    for i in 0..n_ancilla {
        bits.push(i % 2 == 0);
    }
    bits
}

// ── Compile pipeline (shared by tests) ──────────────────────────────

fn compile_fa5() -> (Iso, PhasedCellNetlist) {
    let src = full_adder_source();
    let iso = elaborate(&src, "fa5");
    let net = lower_iso(&iso).expect("fa5 lowering succeeds");
    let balanced = balance(&net).expect("balance succeeds");
    (iso, balanced)
}

// ── Test (a): energy report has total_j > 0 with K0 and K3 breakdown ──

#[test]
fn energy_report_has_k0_and_k3() {
    let (_iso, balanced) = compile_fa5();
    let db = chardb::rung0_measured();
    let t_ramp = 1e-7;

    let report = compiled_energy(&balanced, &db, t_ramp).expect("energy annotation succeeds");

    assert!(report.total_j > 0.0, "total_j must be positive, got {}", report.total_j);
    assert!(
        report.by_kind.contains_key("cswap-k0"),
        "breakdown must contain cswap-k0; got keys: {:?}",
        report.by_kind.keys().collect::<Vec<_>>()
    );
    assert!(
        report.by_kind.contains_key("cswap-k3"),
        "breakdown must contain cswap-k3; got keys: {:?}",
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
        report.by_kind["cswap-k3"].count,
        counts[&CellKind::K3],
        "K3 count mismatch"
    );

    eprintln!("energy report:\n{report}");
}

// ── Test (b): differential oracle over all 32 inputs ────────────────

#[test]
fn differential_oracle_all_32_inputs() {
    let (iso, balanced) = compile_fa5();
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let n_data_out = rail_width(&out_ty).unwrap();

    for v in enumerate_values(&in_ty) {
        let data_bits = bits_of(&v, &in_ty).unwrap();
        let full_input = extend_with_ancilla(&data_bits, &balanced);
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

    let (_iso1, balanced1) = compile_fa5();
    let report1 = compiled_energy(&balanced1, &db, t_ramp).expect("run 1");

    let (_iso2, balanced2) = compile_fa5();
    let report2 = compiled_energy(&balanced2, &db, t_ramp).expect("run 2");

    assert_eq!(report1, report2, "energy reports must be identical across two runs");
}
