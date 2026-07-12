//! M3b headline energy artifact (ADR-0022 'CPU-as-program'): compile a
//! reversible 5-wire full adder from Theseus source through the real
//! pipeline and price it in joules from the characterization DB.
//!
//!     cargo run -p theseus-spice --example full_adder_energy

use theseus_core::{enumerate_values, type_of};
use theseus_netlist::{balance, bits_of, lower_iso, rail_width, simulate, CellKind};
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

fn elaborate(src: &str, iso_name: &str) -> theseus_core::Iso {
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

fn extend_with_ancilla(
    data_bits: &[bool],
    net: &theseus_netlist::PhasedCellNetlist,
) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    let n_ancilla = net.inputs.len() - data_bits.len();
    for i in 0..n_ancilla {
        bits.push(i % 2 == 0);
    }
    bits
}

fn main() {
    let t_ramp = 1e-7;
    let db = chardb::rung0_measured();

    println!("char DB: real rung-0 ngspice characterization (measured once, loaded from data/rung0-chardb.ron)\n");

    // ── Compile pipeline ────────────────────────────────────────────
    let src = full_adder_source();
    let iso = elaborate(&src, "fa5");

    let net = lower_iso(&iso).expect("fa5 lowering succeeds");
    let balanced = balance(&net).expect("balance succeeds");

    // ── Cell breakdown ──────────────────────────────────────────────
    let counts = balanced.cell_counts();
    println!("== 5-wire full adder (fa5) ==");
    println!(
        "  netlist: {} cell(s), depth {} tick(s), {} line(s)",
        balanced.cells.len(),
        balanced.depth(),
        balanced.num_lines
    );
    for (kind, count) in &counts {
        println!("    {kind:?}: {count} cell(s)");
    }

    // ── Energy report ───────────────────────────────────────────────
    let report = compiled_energy(&balanced, &db, t_ramp).expect("energy annotation succeeds");
    println!("\n{report}");

    // ── One decoded input→output line ───────────────────────────────
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let n_data_out = rail_width(&out_ty).unwrap();
    let vals = enumerate_values(&in_ty);
    // Pick (a=1, b=1, cin=0, s=0, c=0) = index 3 (0b00011)
    let v = &vals[3];
    let data_bits = bits_of(v, &in_ty).unwrap();
    let full_input = extend_with_ancilla(&data_bits, &balanced);
    let sim = simulate(&balanced, &full_input).expect("sim runs");
    let out_bits = &sim.outputs[..n_data_out];

    let mut fuel = 10_000u64;
    let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("core eval");
    let expected_bits = bits_of(&expected, &out_ty).unwrap();
    println!(
        "\nsample: input {:?} -> netlist output {:?}, core eval {:?}  [match={}]",
        data_bits.iter().map(|&b| u8::from(b)).collect::<Vec<_>>(),
        out_bits.iter().map(|&b| u8::from(b)).collect::<Vec<_>>(),
        expected_bits.iter().map(|&b| u8::from(b)).collect::<Vec<_>>(),
        out_bits == &expected_bits[..],
    );

    // ── Summary line ────────────────────────────────────────────────
    let k0 = counts.get(&CellKind::K0).copied().unwrap_or(0);
    let k3 = counts.get(&CellKind::K3).copied().unwrap_or(0);
    println!(
        "\nHEADLINE: 5-wire reversible full adder compiles to {k0} K0 + {k3} K3 cells, \
         total energy = {:.4e} J/op @ t_ramp = {:.0e} s",
        report.total_j, t_ramp
    );
}
