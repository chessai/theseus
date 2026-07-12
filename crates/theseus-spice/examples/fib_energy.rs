//! Reversible Fibonacci energy artifact: compile a minimal-width fib(5)
//! machine (14 wires, registers sized 1,1,2,3,4 bits) from generated
//! Theseus source through the full pipeline and price it in joules.
//!
//!     cargo run -p theseus-spice --example fib_energy
//!
//! Construction (see crates/theseus-spice/tests/fib_reversible.rs for the
//! oracle-verified version of exactly this program):
//! * ha4 / fa5: preserved-input clause-defined half/full adders (4/5 wires)
//! * routing: adjacent transpositions from SwapProd + AssocLProd + Id only
//! * fib = unrolled Bennett-style chain r3 = r1+r2, r4 = r2+r3, r5 = r3+r4,
//!   every term written to a FRESH register.

use std::collections::BTreeMap;

use theseus_core::prim::Value;
use theseus_core::type_of;
use theseus_netlist::{balance, bits_of, rail_width, simulate, lower::lower_iso_with_ancillas};
use theseus_spice::annotate::compiled_energy;
use theseus_spice::chardb::{CellChar, CharDb, CharPoint};
use theseus_spice::energy::EnergyFit;

// ── Shared generator (same construction the oracle test verifies) ──────

#[path = "../tests/fib_gen/mod.rs"]
#[allow(dead_code)]
mod gen;

// ── Fixture char DB with K0–K3 ──────────────────────────────────────────

fn fixture_db() -> CharDb {
    let cell = |key: &str, e: f64| {
        (
            key.to_string(),
            CellChar {
                key: key.to_string(),
                points: vec![
                    CharPoint { t_ramp: 1e-8, energy: e },
                    CharPoint { t_ramp: 1e-5, energy: e },
                ],
                fit: EnergyFit { k_adia: 0.0, k_leak: 0.0, k_res: e, r_squared: 1.0 },
            },
        )
    };
    CharDb {
        cells: BTreeMap::from([
            cell("cswap-k0", 2.0e-15),
            cell("cswap-k1", 7.0e-15),
            cell("cswap-k2", 1.1e-14),
            cell("cswap-k3", 1.6e-14),
        ]),
    }
}

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

fn main() {
    let t_ramp = 1e-7;
    let db = fixture_db();
    println!("char DB: FIXTURE constants (K0-K3; representative, not measured)\n");

    let prog = gen::build_fib5_minwidth("fib5");
    let iso = elaborate(&prog.source, &prog.top_name);

    let (net, ancilla_values) = lower_iso_with_ancillas(&iso).expect("lowering succeeds");
    let balanced = balance(&net).expect("balance succeeds");
    assert_eq!(balanced.validate(), Ok(()));

    // ── Cell breakdown ──────────────────────────────────────────────
    let counts = balanced.cell_counts();
    println!("== reversible Fibonacci fib(1..=5), 14 wires, minimal-width registers ==");
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

    // ── Run the machine on the Fibonacci input ──────────────────────
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let n = prog.initial_layout.len();
    let pos = |name: &str| prog.initial_layout.iter().position(|s| s == name).unwrap();
    let mut wires = vec![false; n];
    wires[pos("x0")] = true; // r1 = fib(1) = 1
    wires[pos("y0")] = true; // r2 = fib(2) = 1
    let mk = |b: bool| if b { Value::right(Value::Unit) } else { Value::left(Value::Unit) };
    let mut it = wires.iter().rev();
    let mut v = mk(*it.next().unwrap());
    for &b in it {
        v = Value::pair(mk(b), v);
    }

    let data_bits = bits_of(&v, &in_ty).unwrap();
    let mut full = data_bits.clone();
    full.extend_from_slice(&ancilla_values);
    let sim = simulate(&balanced, &full).expect("sim runs");
    let n_data_out = rail_width(&out_ty).unwrap();
    let out_wires: Vec<bool> = sim.outputs[..n_data_out].iter().map(|&b| !b).collect();

    let read_reg = |names: &[String]| -> u64 {
        let mut val = 0u64;
        for (i, name) in names.iter().enumerate() {
            let p = prog.final_layout.iter().position(|x| x == name).unwrap();
            if out_wires[p] {
                val |= 1 << i;
            }
        }
        val
    };
    let vals: Vec<u64> = (0..5).map(|j| read_reg(&prog.regs[j])).collect();
    println!(
        "\ntimed sim on fib input: r1={} r2={} r3={} r4={} r5={}  (expected 1 1 2 3 5)",
        vals[0], vals[1], vals[2], vals[3], vals[4]
    );
    assert_eq!(vals, vec![1, 1, 2, 3, 5]);

    let k3 = counts.get(&theseus_netlist::CellKind::K3).copied().unwrap_or(0);
    println!(
        "\nHEADLINE: reversible Fibonacci fib(1..=5) compiles to {} cells (max kind K3, {k3} K3s), \
         total energy = {:.4e} J/op @ t_ramp = {:.0e} s",
        balanced.cells.len(),
        report.total_j,
        t_ramp
    );
}
