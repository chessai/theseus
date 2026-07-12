//! The energy-ascent artifact (M3b slice 1b): compile a reversible
//! program, tick-simulate it, and price it in joules from the char DB.
//!
//!     cargo run -p theseus-spice --example energy_ascent
//!
//! With ngspice on PATH this characterizes the real rung-0 cells (a few
//! seconds of sweeps); without it, a clearly-labeled fixture DB with
//! representative constants is used so the pipeline still demonstrates
//! end to end.

use std::collections::BTreeMap;

use theseus_core::{Iso, Type};
use theseus_netlist::{
    lower, lower_iso, prim_from_bits, simulate, CellKind, NodeKind, PhasedCellNetlist,
    TypedNode,
};
use theseus_spice::adiabatic::DeviceRung;
use theseus_spice::annotate::compiled_energy;
use theseus_spice::bench::ngspice_available;
use theseus_spice::chardb::{build_char_db, CellChar, CharDb, CharPoint};
use theseus_spice::energy::EnergyFit;

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn three_bits() -> Type {
    Type::prod(bit(), Type::prod(bit(), bit()))
}

fn fredkin_iso() -> Iso {
    let def = prim_from_bits("fredkin", three_bits(), three_bits(), |b| {
        CellKind::K1.eval(b)
    })
    .expect("fredkin is encodable");
    Iso::Prim(Box::new(def))
}

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
        ]),
    }
}

fn show(name: &str, net: &PhasedCellNetlist, inputs: &[bool], db: &CharDb, t_ramp: f64) {
    println!("== {name} ==");
    println!(
        "  netlist: {} cell(s), depth {} tick(s), {} line(s)",
        net.cells.len(),
        net.depth(),
        net.num_lines
    );
    for ((kind, phase), count) in net.occupancy() {
        println!("    {kind:?} @ phase {phase}: {count}");
    }
    let sim = simulate(net, inputs).expect("phased sim runs");
    println!(
        "  phased-sim: inputs {:?} -> outputs {:?} over {} tick(s); {} INV-TOKEN checks, all held",
        inputs.iter().map(|&b| u8::from(b)).collect::<Vec<_>>(),
        sim.outputs.iter().map(|&b| u8::from(b)).collect::<Vec<_>>(),
        sim.ticks,
        sim.inv_token_checks
    );
    match compiled_energy(net, db, t_ramp) {
        Ok(report) => println!("{report}\n"),
        Err(e) => println!("  energy annotation failed: {e}\n"),
    }
}

fn main() {
    let t_ramp = 1e-7;
    let db = if ngspice_available() {
        println!("char DB: real rung-0 cell sweeps via ngspice (building...)\n");
        build_char_db(DeviceRung::Rung0Level1, &[1e-8, 1e-7, 1e-6, 1e-5])
    } else {
        println!("char DB: FIXTURE constants (ngspice unavailable; numbers are representative, not measured)\n");
        fixture_db()
    };

    // 1. A Fredkin gate: one K1 cell.
    let fredkin = lower_iso(&fredkin_iso()).expect("fredkin lowers");
    show("fredkin (c,a,b), c=1 swaps", &fredkin, &[true, true, false], &db, t_ramp);

    // 2. A Reg(3) buffer on one bit: three K0 stages (holding costs
    //    motion — storage has per-tick energy, C2.1).
    let reg3 = lower(&TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(3) })
        .expect("reg3 lowers");
    show("reg(3) buffer on one bit", &reg3, &[true], &db, t_ramp);

    // 3. A composite: (Reg(1) on 3 bits) ; fredkin — mixed breakdown.
    let composite = lower(&TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Seq(vec![
            TypedNode { input: three_bits(), output: three_bits(), kind: NodeKind::Reg(1) },
            TypedNode {
                input: three_bits(),
                output: three_bits(),
                kind: NodeKind::Prim(Box::new(
                    prim_from_bits("fredkin", three_bits(), three_bits(), |b| {
                        CellKind::K1.eval(b)
                    })
                    .unwrap(),
                )),
            },
        ]),
    })
    .expect("composite lowers");
    show("(reg1 x 3 bits) ; fredkin", &composite, &[true, false, true], &db, t_ramp);
}
