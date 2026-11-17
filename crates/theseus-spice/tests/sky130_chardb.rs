//! The committed sky130 characterization database
//! (`data/sky130-chardb.ron`, [`sky130_measured`]). Offline: these tests read
//! the committed measurements; they do not run ngspice.

use theseus_spice::chardb::{rung0_measured, sky130_measured, CharDb, QueryError, QueryMode};
use theseus_spice::cswap::WIRE_LOAD_F;

/// The reversible logic cells programs are priced from.
const LOGIC_CELLS: [&str; 5] = ["cswap-k0", "cswap-k1", "cswap-k2", "cswap-k3", "cswap-r1"];

fn energies(db: &CharDb, key: &str) -> Vec<(f64, f64)> {
    db.cells[key]
        .points
        .iter()
        .map(|p| (p.t_ramp, p.energy))
        .collect()
}

#[test]
fn has_every_cell_over_the_same_ten_periods() {
    let db = sky130_measured();
    let mut keys: Vec<&str> = db.cells.keys().map(String::as_str).collect();
    keys.sort_unstable();
    assert_eq!(
        keys,
        [
            "adiabatic-tg",
            "cmos-static",
            "cswap-k0",
            "cswap-k1",
            "cswap-k2",
            "cswap-k3",
            "cswap-r1"
        ]
    );
    let axis: Vec<f64> = energies(&db, "cswap-k0").iter().map(|p| p.0).collect();
    assert_eq!(axis.len(), 10);
    assert_eq!(axis[0], 1e-8);
    assert!((axis[9] / 1e-5 - 1.0).abs() < 1e-9);
    for cell in db.cells.values() {
        let ts: Vec<f64> = cell.points.iter().map(|p| p.t_ramp).collect();
        assert_eq!(ts, axis, "{} is on a different period axis", cell.key);
        assert!(cell.points.iter().all(|p| p.energy > 0.0), "{}", cell.key);
    }
}

/// Queries return the measurement at a measured period, interpolate between
/// neighbours, and refuse to extrapolate.
#[test]
fn queries_interpolate_the_measurements() {
    let db = sky130_measured();
    assert_eq!(db.query_mode, QueryMode::LogLogInterp);
    for (key, cell) in &db.cells {
        for p in &cell.points {
            assert_eq!(
                db.query(key, p.t_ramp),
                Ok(p.energy),
                "{key} at {}",
                p.t_ramp
            );
        }
        for w in cell.points.windows(2) {
            // Geometric midpoint in t -> geometric mean in E (log-log linear).
            let mid = (w[0].t_ramp * w[1].t_ramp).sqrt();
            let got = db.query(key, mid).unwrap();
            let want = (w[0].energy * w[1].energy).sqrt();
            assert!(
                (got / want - 1.0).abs() < 1e-9,
                "{key} at {mid}: {got} vs {want}"
            );
        }
        assert!(matches!(
            db.query(key, 5e-9),
            Err(QueryError::OutOfRange { .. })
        ));
        assert!(matches!(
            db.query(key, 2e-5),
            Err(QueryError::OutOfRange { .. })
        ));
    }
    assert_eq!(
        db.query("no-such-cell", 1e-6),
        Err(QueryError::NoSuchCell("no-such-cell".to_string()))
    );
}

/// The leakage floor is real: on sky130 every logic cell's energy falls with
/// the period, bottoms out strictly inside the swept range, and rises again.
/// On the rung-0 ideal devices it only ever falls.
#[test]
fn logic_cells_have_an_interior_energy_minimum() {
    let db = sky130_measured();
    for key in LOGIC_CELLS {
        let pts = energies(&db, key);
        let (imin, &(tmin, emin)) = pts
            .iter()
            .enumerate()
            .min_by(|a, b| a.1 .1.total_cmp(&b.1 .1))
            .unwrap();
        assert!(
            imin > 0 && imin < pts.len() - 1,
            "{key}: minimum {emin:e} J at T = {tmin:e} s is at the edge of the sweep"
        );
        // Falling into the minimum from the fast side, rising out of it.
        assert!(
            pts[..=imin].windows(2).all(|w| w[1].1 < w[0].1),
            "{key} fast side"
        );
        assert!(
            pts[imin..].windows(2).all(|w| w[1].1 > w[0].1),
            "{key} slow side"
        );
        // The fast end is adiabatic: a decade slower costs well under a third.
        assert!(pts[3].1 < pts[0].1 / 3.0, "{key}: not ~1/T at the fast end");
    }

    let ideal = rung0_measured();
    for key in LOGIC_CELLS {
        let pts = energies(&ideal, key);
        assert!(
            pts.windows(2).all(|w| w[1].1 < w[0].1),
            "rung0 {key} must only fall"
        );
    }
}

/// The irreversible reference is a static gate driving the cells' wire load:
/// flat in the period, and about `C * VDD^2` per cycle.
#[test]
fn cmos_reference_is_flat_at_c_vdd_squared() {
    let db = sky130_measured();
    let pts = energies(&db, "cmos-static");
    let lo = pts.iter().map(|p| p.1).fold(f64::INFINITY, f64::min);
    let hi = pts.iter().map(|p| p.1).fold(0.0, f64::max);
    let cv2 = WIRE_LOAD_F * 1.8 * 1.8;
    // Above C*VDD^2 by the gate's own capacitance and short-circuit current;
    // leakage adds a little at the slowest periods.
    assert!(
        lo > cv2 && hi < 1.5 * cv2,
        "CMOS {lo:e}..{hi:e} J vs CV^2 = {cv2:e} J"
    );
    assert!(
        hi / lo < 1.25,
        "CMOS reference must be flat, spans {lo:e}..{hi:e}"
    );
}

/// The rung-0 database is untouched by the query-mode addition: it still
/// evaluates its fit, and serializes without the new field.
#[test]
fn rung0_database_is_unchanged() {
    let db = rung0_measured();
    assert_eq!(db.query_mode, QueryMode::Fit);
    assert!(!db.to_ron().contains("query_mode"));
    assert_eq!(
        db.query("cswap-k0", 1e-7).unwrap(),
        db.cells["cswap-k0"].fit.eval(1e-7)
    );
}
