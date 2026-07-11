//! Integration test for the money-plot artifact (M3a-4).
//!
//! Validates: (a) all three DUT names present, (b) mandatory stamp keys,
//! (c) byte-determinism across two generations, (d) slope invariants.

use theseus_spice::adiabatic::money_plot;
use theseus_spice::artifact::{
    default_stamps, stamped_money_plot, to_stamped_text, Rung,
};
use theseus_spice::bench::{log_log_slope, ngspice_available};

/// Fixed period axis matching the example binary.
fn regime_periods() -> Vec<f64> {
    vec![1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5]
}

/// Wide sweep that exposes PFAL's adiabatic regime AND its floor.
fn wide_periods() -> Vec<f64> {
    vec![1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4]
}

/// Generate stamped records for the three-family money plot.
fn generate_records(
    periods: &[f64],
) -> Vec<theseus_spice::artifact::MoneyPlotRecord> {
    let curves = money_plot(periods);
    stamped_money_plot(&curves, Rung::Rung0IdealModels, default_stamps())
        .expect("INV-STAMP violation")
}

#[test]
fn money_plot_artifact_contract() {
    if !ngspice_available() {
        eprintln!("[skip] ngspice unavailable");
        return;
    }

    let records = generate_records(&regime_periods());
    let text = to_stamped_text(&records);

    // (a) All three DUT names present.
    for dut in ["adiabatic-tg", "pfal", "cmos-static"] {
        assert!(
            text.contains(dut),
            "stamped text missing DUT name '{dut}'"
        );
    }

    // (b) Mandatory stamp keys present.
    for key in ["pdk:", "simulator:", "harness:", "rung:"] {
        assert!(
            text.contains(key),
            "stamped text missing mandatory stamp key '{key}'"
        );
    }

    // (c) Byte-identical across two generations (determinism).
    let records2 = generate_records(&regime_periods());
    let text2 = to_stamped_text(&records2);
    assert_eq!(
        text.as_bytes(),
        text2.as_bytes(),
        "stamped text is not byte-deterministic across two generations"
    );

    // (d) Slope invariants on the recorded records.

    // adiabatic-tg: slope −1 ± 0.1
    let adia = records
        .iter()
        .find(|r| r.dut == "adiabatic-tg")
        .expect("missing adiabatic-tg record");
    assert!(
        (-1.1..=-0.9).contains(&adia.slope),
        "adiabatic-tg slope must be −1 ± 0.1, got {:.4}",
        adia.slope
    );

    // cmos-static: slope ≈ 0 (flat)
    let cmos = records
        .iter()
        .find(|r| r.dut == "cmos-static")
        .expect("missing cmos-static record");
    assert!(
        (-0.1..=0.1).contains(&cmos.slope),
        "cmos-static slope must be ≈ 0, got {:.4}",
        cmos.slope
    );
}

/// PFAL needs a wider sweep to expose the floor; test separately.
#[test]
fn pfal_fast_slope_and_floor() {
    if !ngspice_available() {
        eprintln!("[skip] ngspice unavailable");
        return;
    }

    let curves = money_plot(&wide_periods());
    let pfal = curves
        .iter()
        .find(|c| c.dut == "pfal")
        .expect("missing pfal curve");

    // Fast-end (first 3 points): slope ≈ −1.
    let fast_slope = log_log_slope(&pfal.points[..3]);
    assert!(
        (-1.2..=-0.8).contains(&fast_slope),
        "PFAL fast-end slope must be ≈ −1, got {fast_slope:.3}"
    );

    // Floor: the minimum energy is NOT at the slowest period.
    let min_i = pfal
        .points
        .iter()
        .enumerate()
        .min_by(|a, b| a.1 .1.partial_cmp(&b.1 .1).unwrap())
        .unwrap()
        .0;
    assert!(
        min_i < pfal.points.len() - 1,
        "PFAL must floor (min energy before the slowest period), got min at index {min_i}"
    );
}
