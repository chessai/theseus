//! The energy ascent, end to end: a compiled reversible program → a
//! measured energy number (M3b slice 1b).
//!
//! Pure-logic tests exercise `compiled_energy` against fixture char DBs
//! (exact expected sums, no ngspice). The measured-DB test loads the
//! real rung-0 ngspice characterization from `data/rung0-chardb.ron`,
//! lowers a Fredkin, runs the timed phased-sim, and reports a real
//! energy number with the per-kind breakdown.

use std::collections::BTreeMap;

use theseus_core::{Iso, Type};
use theseus_netlist::{lower, lower_iso, prim_from_bits, simulate, CellKind, NodeKind, TypedNode};
use theseus_spice::annotate::{compiled_energy, KindEnergy};
use theseus_spice::chardb::{self, CellChar, CharDb, CharPoint, QueryError};
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

/// `(Reg(1) on 3 bits) ; fredkin` -- 3 K0 buffers then 1 K1: the smallest
/// composite with a mixed by-kind breakdown, tick-balanced by
/// construction (all three bits buffer in lockstep).
fn composite_net() -> theseus_netlist::PhasedCellNetlist {
    let reg1 = TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Reg(1),
    };
    let fredkin = TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Prim(Box::new(
            prim_from_bits("fredkin", three_bits(), three_bits(), |b| {
                CellKind::K1.eval(b)
            })
            .unwrap(),
        )),
    };
    let net = lower(&TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Seq(vec![reg1, fredkin]),
    })
    .expect("composite lowers");
    assert_eq!(
        net.cell_counts(),
        BTreeMap::from([(CellKind::K0, 3), (CellKind::K1, 1)])
    );
    net
}

/// Fixture cell with a constant fitted energy over [1e-8, 1e-5] s.
fn const_cell(key: &str, e: f64) -> (String, CellChar) {
    (
        key.to_string(),
        CellChar {
            key: key.to_string(),
            points: vec![
                CharPoint {
                    t_ramp: 1e-8,
                    energy: e,
                },
                CharPoint {
                    t_ramp: 1e-5,
                    energy: e,
                },
            ],
            fit: EnergyFit {
                k_adia: 0.0,
                k_leak: 0.0,
                k_res: e,
                r_squared: 1.0,
            },
        },
    )
}

// ── (a) Pure logic: annotation vs a fixture DB, exact sums ─────────────

#[test]
fn composite_energy_is_3_ek0_plus_1_ek1() {
    let (e_k0, e_k1) = (2.0e-15, 7.0e-15);
    let db = CharDb {
        cells: BTreeMap::from([const_cell("cswap-k0", e_k0), const_cell("cswap-k1", e_k1)]),
    };
    let net = composite_net();

    let report = compiled_energy(&net, &db, 1e-7).unwrap();
    assert_eq!(report.t_ramp, 1e-7);
    assert_eq!(report.total_j, 3.0 * e_k0 + e_k1);
    assert_eq!(
        report.by_kind,
        BTreeMap::from([
            (
                "cswap-k0".to_string(),
                KindEnergy {
                    count: 3,
                    per_cell_j: e_k0,
                    subtotal_j: 3.0 * e_k0
                }
            ),
            (
                "cswap-k1".to_string(),
                KindEnergy {
                    count: 1,
                    per_cell_j: e_k1,
                    subtotal_j: e_k1
                }
            ),
        ])
    );
}

#[test]
fn annotation_inherits_the_interpolation_contract() {
    let db = CharDb {
        cells: BTreeMap::from([const_cell("cswap-k0", 2e-15), const_cell("cswap-k1", 7e-15)]),
    };
    let net = composite_net();
    // Out-of-range ramp: hard error, no extrapolation (C3.1).
    assert!(matches!(
        compiled_energy(&net, &db, 1e-2),
        Err(QueryError::OutOfRange { .. })
    ));
    // A DB missing a placed kind: hard error.
    let db_k0_only = CharDb {
        cells: BTreeMap::from([const_cell("cswap-k0", 2e-15)]),
    };
    assert_eq!(
        compiled_energy(&net, &db_k0_only, 1e-7),
        Err(QueryError::NoSuchCell("cswap-k1".to_string()))
    );
}

#[test]
fn fixture_energy_tracks_the_fitted_model_not_the_raw_points() {
    // A 1/t fit: doubling t_ramp halves the per-cell energy.
    let cell = CellChar {
        key: "cswap-k1".to_string(),
        points: vec![
            CharPoint {
                t_ramp: 1e-8,
                energy: 1e-13,
            },
            CharPoint {
                t_ramp: 1e-6,
                energy: 1e-15,
            },
        ],
        fit: EnergyFit {
            k_adia: 1e-21,
            k_leak: 0.0,
            k_res: 0.0,
            r_squared: 1.0,
        },
    };
    let db = CharDb {
        cells: BTreeMap::from([("cswap-k1".to_string(), cell)]),
    };
    let net = lower_iso(&fredkin_iso()).unwrap();
    let e1 = compiled_energy(&net, &db, 1e-7).unwrap().total_j;
    let e2 = compiled_energy(&net, &db, 2e-7).unwrap().total_j;
    assert!((e1 / e2 - 2.0).abs() < 1e-9, "1/t scaling: {e1} vs {e2}");
}

// ── (b) The energy ascent with the real measured char DB ──────────────

#[test]
fn energy_ascent_fredkin_end_to_end() {
    // Load the real rung-0 ngspice characterization (no live ngspice needed).
    let db = chardb::rung0_measured();

    // 1. Compile: Core Π Fredkin → phased cell netlist (slice 1a).
    let iso = fredkin_iso();
    let net = lower_iso(&iso).expect("fredkin lowers");
    assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K1, 1)]));

    // 2. Phased-sim the compiled netlist (timed, INV-TOKEN asserted
    //    per tick): control=1 swaps a and b.
    let sim = simulate(&net, &[true, true, false]).expect("phased sim runs");
    assert_eq!(sim.outputs, vec![true, false, true]);
    assert!(sim.inv_token_checks > 0);

    // 3. Annotate: the measured energy number.
    let t_ramp = 1e-7;
    let report = compiled_energy(&net, &db, t_ramp).expect("in-range annotation");
    assert!(report.total_j.is_finite() && report.total_j > 0.0);
    assert_eq!(report.by_kind.len(), 1);
    let k1 = &report.by_kind["cswap-k1"];
    assert_eq!(k1.count, 1);
    assert_eq!(report.total_j, k1.subtotal_j);
    eprintln!("── energy ascent: fredkin @ rung0 ──\n{report}");

    // The composite (3 K0 + 1 K1) costs the fredkin plus exactly three
    // fitted K0s -- additivity is definitional in the static report
    // (C3.2's *empirical* validation vs whole-block SPICE is TH-E1,
    // a later slice).
    let comp = composite_net();
    let comp_report = compiled_energy(&comp, &db, t_ramp).unwrap();
    let e_k0 = db.query("cswap-k0", t_ramp).unwrap();
    assert!((comp_report.total_j - (report.total_j + 3.0 * e_k0)).abs() <= 1e-24);
    eprintln!("── energy ascent: (reg1 x3 ; fredkin) @ rung0 ──\n{comp_report}");
}
