//! CharDB + energy-model integration tests (pure logic, no ngspice).

use theseus_spice::chardb::{self, CharDb, CharPoint, QueryError};
use theseus_spice::energy;

/// Helper: generate synthetic data from E(t) = k_adia/t + k_leak*t + k_res.
fn synthetic_points(k_adia: f64, k_leak: f64, k_res: f64, ts: &[f64]) -> Vec<(f64, f64)> {
    ts.iter().map(|&t| (t, k_adia / t + k_leak * t + k_res)).collect()
}

// ── (a) Fit recovers known coefficients within 1% relative ──────────────

#[test]
fn fit_recovers_coefficients_within_tolerance() {
    let (ka, kl, kr) = (3.0, 2.0, 1.0);
    let ts: Vec<f64> = (1..=20).map(|i| i as f64 * 0.5).collect();
    let points = synthetic_points(ka, kl, kr, &ts);

    let fit = energy::fit(&points);

    let rel = |actual: f64, expected: f64| ((actual - expected) / expected).abs();
    assert!(rel(fit.k_adia, ka) < 0.01, "k_adia: got {} expected {}", fit.k_adia, ka);
    assert!(rel(fit.k_leak, kl) < 0.01, "k_leak: got {} expected {}", fit.k_leak, kl);
    assert!(rel(fit.k_res, kr) < 0.01, "k_res: got {} expected {}", fit.k_res, kr);
    assert!(fit.r_squared > 0.999, "R² should be ~1 for exact data, got {}", fit.r_squared);
    assert!(fit.adequate(0.99));
}

#[test]
fn fit_eval_matches_model() {
    let (ka, kl, kr) = (5.0, 0.1, 0.5);
    let ts: Vec<f64> = (1..=10).map(|i| i as f64).collect();
    let points = synthetic_points(ka, kl, kr, &ts);
    let fit = energy::fit(&points);

    for &(t, expected) in &points {
        let got = fit.eval(t);
        let rel_err = ((got - expected) / expected).abs();
        assert!(rel_err < 0.01, "eval({t}): got {got}, expected {expected}");
    }
}

// ── (b) Query contract: in-range, out-of-range, unknown key ─────────────

fn test_db() -> CharDb {
    let (ka, kl, kr) = (3.0, 2.0, 1.0);
    let ts: Vec<f64> = (1..=10).map(|i| i as f64).collect();
    let pts: Vec<CharPoint> = ts
        .iter()
        .map(|&t| CharPoint { t_ramp: t, energy: ka / t + kl * t + kr })
        .collect();
    chardb::rebuild(&[("inv_std".to_string(), pts)])
}

#[test]
fn query_in_range_succeeds() {
    let db = test_db();
    // Endpoints and mid-range should all succeed.
    assert!(db.query("inv_std", 1.0).is_ok());
    assert!(db.query("inv_std", 5.5).is_ok());
    assert!(db.query("inv_std", 10.0).is_ok());
}

#[test]
fn query_below_min_is_out_of_range() {
    let db = test_db();
    let err = db.query("inv_std", 0.999).unwrap_err();
    match err {
        QueryError::OutOfRange { ref key, t_ramp, min, max } => {
            assert_eq!(key, "inv_std");
            assert!((t_ramp - 0.999).abs() < f64::EPSILON);
            assert!((min - 1.0).abs() < f64::EPSILON);
            assert!((max - 10.0).abs() < f64::EPSILON);
        }
        _ => panic!("expected OutOfRange, got {err:?}"),
    }
    // Display impl works
    let msg = format!("{err}");
    assert!(msg.contains("out of range"));
}

#[test]
fn query_above_max_is_out_of_range() {
    let db = test_db();
    let err = db.query("inv_std", 10.001).unwrap_err();
    assert!(matches!(err, QueryError::OutOfRange { .. }));
}

#[test]
fn query_unknown_key_is_no_such_cell() {
    let db = test_db();
    let err = db.query("nand3_xl", 5.0).unwrap_err();
    assert_eq!(err, QueryError::NoSuchCell("nand3_xl".to_string()));
    let msg = format!("{err}");
    assert!(msg.contains("nand3_xl"));
}

// ── (c) RON round-trip is byte-identical ─────────────────────────────────

#[test]
fn ron_round_trip_byte_identical() {
    let db = test_db();
    let ron_text = db.to_ron();
    let restored = CharDb::from_ron(&ron_text).expect("from_ron should succeed");
    assert_eq!(db, restored, "round-trip must be lossless");

    // Byte-identical re-serialization
    let ron_text2 = restored.to_ron();
    assert_eq!(ron_text, ron_text2, "re-serialization must be byte-identical");
}

// ── (d) Rebuild is deterministic ─────────────────────────────────────────

#[test]
fn rebuild_deterministic() {
    let (ka, kl, kr) = (3.0, 2.0, 1.0);
    let ts: Vec<f64> = (1..=10).map(|i| i as f64).collect();
    let pts: Vec<CharPoint> = ts
        .iter()
        .map(|&t| CharPoint { t_ramp: t, energy: ka / t + kl * t + kr })
        .collect();

    let raw = vec![
        ("cell_b".to_string(), pts.clone()),
        ("cell_a".to_string(), pts),
    ];

    let db1 = chardb::rebuild(&raw);
    let db2 = chardb::rebuild(&raw);
    assert_eq!(db1.to_ron(), db2.to_ron(), "two rebuilds must serialize byte-identically");
}
