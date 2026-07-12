//! Money-plot artifact generator (M3a-3): stamped RON records, deterministic
//! serialization, and ASCII rendering of energy-per-op vs period curves.
//!
//! Implements the money-plot artifact contract from `test-spec-m3a.md`:
//! each curve becomes a `MoneyPlotRecord` with mandatory stamps and rung
//! (INV-STAMP enforced), serialized to a byte-deterministic RON-like text
//! format for acceptance artifacts.

use crate::adiabatic::MoneyPlotCurve;

/// Idealization rung for the simulation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Rung {
    /// Ideal MOSFET models (level-1, no PDK).
    Rung0IdealModels,
    /// sky130A foundry models, tt corner, 27 °C, ideal trapezoidal clocks,
    /// leakage broken out (ADR-0032 rung 1). The PDK build hash is carried in
    /// the `pdk` stamp; this string names the rung/corner.
    Rung1Sky130Tt,
}

impl Rung {
    fn as_str(&self) -> &'static str {
        match self {
            Rung::Rung0IdealModels => "rung0-ideal-models",
            Rung::Rung1Sky130Tt => "rung1-sky130-tt",
        }
    }
}

/// Provenance stamps for a money-plot record (INV-STAMP).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Stamps {
    pub pdk: String,
    pub simulator: String,
    pub harness: String,
    pub db: String,
}

/// A stamped money-plot record: the artifact unit per the test-spec contract.
#[derive(Clone, Debug, PartialEq)]
pub struct MoneyPlotRecord {
    pub dut: String,
    pub family: String,
    pub axes: &'static str,
    pub points: Vec<(f64, f64)>,
    pub slope: f64,
    pub rung: Rung,
    pub stamps: Stamps,
}

/// Error for INV-STAMP violations.
#[derive(Debug)]
pub enum StampError {
    MissingPdk,
    MissingSimulator,
    MissingHarness,
    MissingRung,
}

impl std::fmt::Display for StampError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StampError::MissingPdk => write!(f, "INV-STAMP: missing pdk stamp"),
            StampError::MissingSimulator => write!(f, "INV-STAMP: missing simulator stamp"),
            StampError::MissingHarness => write!(f, "INV-STAMP: missing harness stamp"),
            StampError::MissingRung => write!(f, "INV-STAMP: missing rung"),
        }
    }
}

/// Map a DUT name to its curve family. Both the rung-0 dut names and the
/// rung-1 `-sky130`-suffixed names collapse to the same three families.
fn family_for_dut(dut: &str) -> &'static str {
    match dut {
        "adiabatic-tg" | "adiabatic-tg-sky130" => "adiabatic-tg",
        "pfal" | "pfal-sky130" => "pfal",
        "cmos-static" | "cmos-static-sky130" => "cmos-static",
        _ => "unknown",
    }
}

/// Build a stamped record from a raw curve, enforcing INV-STAMP.
///
/// Returns `Err` if any stamp field is empty or rung is missing.
pub fn stamp_curve(
    curve: &MoneyPlotCurve,
    rung: Option<Rung>,
    stamps: Stamps,
) -> Result<MoneyPlotRecord, StampError> {
    if stamps.pdk.is_empty() {
        return Err(StampError::MissingPdk);
    }
    if stamps.simulator.is_empty() {
        return Err(StampError::MissingSimulator);
    }
    if stamps.harness.is_empty() {
        return Err(StampError::MissingHarness);
    }
    let rung = rung.ok_or(StampError::MissingRung)?;
    Ok(MoneyPlotRecord {
        dut: curve.dut.clone(),
        family: family_for_dut(&curve.dut).to_string(),
        axes: "EnergyPerOpVsPeriod",
        points: curve.points.clone(),
        slope: curve.slope(),
        rung,
        stamps,
    })
}

/// Default stamps for the current M3a harness (generic models, no PDK).
pub fn default_stamps() -> Stamps {
    Stamps {
        pdk: "generic-level1".to_string(),
        simulator: "ngspice".to_string(),
        harness: "theseus-spice-m3a-v0.1".to_string(),
        db: "none".to_string(),
    }
}

/// Generate stamped records for the full money-plot family, enforcing INV-STAMP
/// on every curve.
pub fn stamped_money_plot(
    curves: &[MoneyPlotCurve],
    rung: Rung,
    stamps: Stamps,
) -> Result<Vec<MoneyPlotRecord>, StampError> {
    curves
        .iter()
        .map(|c| stamp_curve(c, Some(rung.clone()), stamps.clone()))
        .collect()
}

// ---------------------------------------------------------------------------
// Deterministic RON-like serialization
// ---------------------------------------------------------------------------

/// Format an f64 in a canonical, deterministic way: scientific notation with
/// exactly 15 significant digits. This avoids platform-dependent formatting.
fn fmt_f64(v: f64) -> String {
    format!("{:.15e}", v)
}

/// Serialize a single record to a deterministic RON-like text block.
fn record_to_text(r: &MoneyPlotRecord) -> String {
    let mut s = String::new();
    s.push_str("MoneyPlotRecord(\n");
    s.push_str(&format!("    dut: \"{}\",\n", r.dut));
    s.push_str(&format!("    family: \"{}\",\n", r.family));
    s.push_str(&format!("    axes: \"{}\",\n", r.axes));
    s.push_str(&format!("    slope: {},\n", fmt_f64(r.slope)));
    s.push_str(&format!("    rung: \"{}\",\n", r.rung.as_str()));
    s.push_str("    stamps: (\n");
    s.push_str(&format!("        pdk: \"{}\",\n", r.stamps.pdk));
    s.push_str(&format!("        simulator: \"{}\",\n", r.stamps.simulator));
    s.push_str(&format!("        harness: \"{}\",\n", r.stamps.harness));
    s.push_str(&format!("        db: \"{}\",\n", r.stamps.db));
    s.push_str("    ),\n");
    s.push_str("    points: [\n");
    for &(period, energy) in &r.points {
        s.push_str(&format!(
            "        ({}, {}),\n",
            fmt_f64(period),
            fmt_f64(energy)
        ));
    }
    s.push_str("    ],\n");
    s.push_str(")\n");
    s
}

/// Serialize a slice of records to a deterministic text artifact.
pub fn to_stamped_text(records: &[MoneyPlotRecord]) -> String {
    let mut s = String::new();
    s.push_str("// theseus money-plot artifact (M3a)\n");
    s.push_str(&format!("// records: {}\n", records.len()));
    s.push('\n');
    for (i, r) in records.iter().enumerate() {
        if i > 0 {
            s.push('\n');
        }
        s.push_str(&record_to_text(r));
    }
    s
}

// ---------------------------------------------------------------------------
// ASCII money-plot renderer
// ---------------------------------------------------------------------------

/// Render human-readable ASCII money-plot output: per-curve header with slope,
/// then a table of (period, energy, scaled bar).
pub fn render_ascii(records: &[MoneyPlotRecord]) -> String {
    let mut out = String::new();
    out.push_str("=== Money Plot (energy per op vs period, log-log) ===\n\n");

    for r in records {
        out.push_str(&format!(
            "--- {} [family: {}, slope: {:.4}] ---\n",
            r.dut, r.family, r.slope
        ));
        out.push_str(&format!(
            "    rung: {}, pdk: {}, sim: {}\n",
            r.rung.as_str(),
            r.stamps.pdk,
            r.stamps.simulator
        ));
        out.push_str(&format!(
            "    {:>14}  {:>14}  bar\n",
            "period (s)", "E/op (J)"
        ));
        out.push_str(&format!("    {:->14}  {:->14}  {:->40}\n", "", "", ""));

        if r.points.is_empty() {
            out.push_str("    (no data)\n");
            continue;
        }

        // Scale bars relative to the max log-energy in this curve.
        let log_energies: Vec<f64> = r.points.iter().map(|&(_, e)| e.log10()).collect();
        let log_max = log_energies
            .iter()
            .cloned()
            .fold(f64::NEG_INFINITY, f64::max);
        let log_min = log_energies
            .iter()
            .cloned()
            .fold(f64::INFINITY, f64::min);
        let log_range = (log_max - log_min).max(1.0);

        let bar_width = 40;
        for (&(period, energy), &log_e) in r.points.iter().zip(log_energies.iter()) {
            let frac = (log_e - log_min) / log_range;
            let bars = (frac * bar_width as f64).round() as usize;
            let bar: String = "#".repeat(bars);
            out.push_str(&format!(
                "    {:>14.6e}  {:>14.6e}  {}\n",
                period, energy, bar
            ));
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adiabatic::{money_plot, money_plot_rung, sky130_available, DeviceRung, MoneyPlotCurve};
    use crate::bench::{log_log_slope, ngspice_available};

    fn regime_periods() -> Vec<f64> {
        vec![1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5]
    }

    /// Full artifact generation: both families present, slopes correct in the
    /// serialized record, serialization is stable, ASCII render works.
    #[test]
    fn artifact_round_trip() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }

        let curves = money_plot(&regime_periods());
        let records =
            stamped_money_plot(&curves, Rung::Rung0IdealModels, default_stamps()).unwrap();

        // (a) Both families present.
        assert!(
            records.iter().any(|r| r.family == "adiabatic-tg"),
            "missing adiabatic-tg family"
        );
        assert!(
            records.iter().any(|r| r.family == "cmos-static"),
            "missing cmos-static family"
        );

        // (b) Slopes in the serialized records.
        let adia = records.iter().find(|r| r.dut == "adiabatic-tg").unwrap();
        assert!(
            (-1.1..=-0.9).contains(&adia.slope),
            "adiabatic slope {:.4} not in [-1.1, -0.9]",
            adia.slope
        );
        let cmos = records.iter().find(|r| r.dut == "cmos-static").unwrap();
        assert!(
            (-0.1..=0.1).contains(&cmos.slope),
            "cmos slope {:.4} not in [-0.1, 0.1]",
            cmos.slope
        );

        // (c) Serialization is stable (serialize twice → identical).
        let text1 = to_stamped_text(&records);
        let text2 = to_stamped_text(&records);
        assert_eq!(text1, text2, "serialization is not deterministic");

        // Verify round-trip: the serialized text is non-empty and contains
        // expected markers.
        assert!(text1.contains("MoneyPlotRecord("));
        assert!(text1.contains("adiabatic-tg"));
        assert!(text1.contains("cmos-static"));
        assert!(text1.contains("EnergyPerOpVsPeriod"));
        assert!(text1.contains("rung0-ideal-models"));
        assert!(text1.contains("generic-level1"));

        // ASCII render.
        let ascii = render_ascii(&records);
        assert!(ascii.contains("Money Plot"));
        assert!(ascii.contains("adiabatic-tg"));
        assert!(ascii.contains("cmos-static"));

        eprintln!("--- Serialized artifact ---\n{}", text1);
        eprintln!("--- ASCII money plot ---\n{}", ascii);
    }

    /// Rung-1 (sky130) money-plot artifact (W6): the stamped record carries all
    /// three dut names, the `rung1-sky130-tt` rung + the sky130 PDK version in
    /// the pdk stamp, is byte-deterministic across two generations, and the
    /// recorded slopes match the three-family shape. Gated on ngspice + the PDK
    /// (the `.#char` devShell); skipped otherwise. Modest point count (sims slow).
    #[test]
    fn rung1_money_plot_artifact_shape_and_determinism() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        if !sky130_available() {
            eprintln!("[skip] sky130 PDK unavailable (THESEUS_SKY130_LIB unset/missing)");
            return;
        }
        // Modest log-spaced axis over the pinned 1e-8..1e-5 s rung-1 window.
        let axis = [1e-8, 1e-7, 1e-6, 1e-5];
        let pdk = std::env::var("THESEUS_SKY130_VERSION")
            .unwrap_or_else(|_| "sky130A-unknown".to_string());
        let stamps = Stamps {
            pdk: pdk.clone(),
            simulator: "ngspice".to_string(),
            harness: "theseus-spice-w6-rung1-v0.1".to_string(),
            db: "chardb-rung1-sky130".to_string(),
        };

        let curves = money_plot_rung(&axis, DeviceRung::Rung1Sky130);
        let records =
            stamped_money_plot(&curves, Rung::Rung1Sky130Tt, stamps.clone()).unwrap();

        // (a) All three rung-1 dut names present.
        for dut in ["adiabatic-tg-sky130", "pfal-sky130", "cmos-static-sky130"] {
            assert!(
                records.iter().any(|r| r.dut == dut),
                "missing rung-1 dut {dut} in {:?}",
                records.iter().map(|r| &r.dut).collect::<Vec<_>>()
            );
        }

        // (b) Rung + PDK version carried in the stamp/rung.
        let text = to_stamped_text(&records);
        assert!(text.contains("rung1-sky130-tt"), "artifact missing rung1-sky130-tt");
        assert!(
            text.contains(&pdk),
            "artifact pdk stamp must carry the sky130 version {pdk}"
        );
        for r in &records {
            assert_eq!(r.rung, Rung::Rung1Sky130Tt);
            assert_eq!(r.stamps.pdk, pdk);
        }

        // (c) Byte-deterministic across two independent generations.
        let curves2 = money_plot_rung(&axis, DeviceRung::Rung1Sky130);
        let records2 =
            stamped_money_plot(&curves2, Rung::Rung1Sky130Tt, stamps).unwrap();
        let text2 = to_stamped_text(&records2);
        assert_eq!(
            text.as_bytes(),
            text2.as_bytes(),
            "rung-1 money-plot artifact is not byte-deterministic"
        );

        // (d) Recorded slopes match the three-family shape.
        let adia = records.iter().find(|r| r.dut == "adiabatic-tg-sky130").unwrap();
        assert!(
            (-1.15..=-0.7).contains(&adia.slope),
            "adiabatic-TG slope {:.4} not in [-1.15, -0.7]",
            adia.slope
        );
        let cmos = records.iter().find(|r| r.dut == "cmos-static-sky130").unwrap();
        assert!(
            cmos.slope.abs() < 0.15,
            "CMOS slope {:.4} must be ≈ 0 (|slope| < 0.15)",
            cmos.slope
        );
        // PFAL falls at the fast end then floors at the slow end: fast-half
        // slope is clearly negative, slow-half slope is not still falling
        // (floored/risen).
        let pfal = records.iter().find(|r| r.dut == "pfal-sky130").unwrap();
        let n = pfal.points.len();
        let fast_slope = log_log_slope(&pfal.points[..n / 2 + 1]);
        let slow_slope = log_log_slope(&pfal.points[n / 2..]);
        eprintln!(
            "rung1 slopes: adia={:.3} cmos={:.3} pfal(fast={:.3},slow={:.3})",
            adia.slope, cmos.slope, fast_slope, slow_slope
        );
        assert!(
            fast_slope < -0.4,
            "PFAL fast-end must fall (slope {fast_slope:.3} should be < -0.4): {:?}",
            pfal.points
        );
        assert!(
            slow_slope > fast_slope + 0.3,
            "PFAL slow-end must floor (slow slope {slow_slope:.3} should be well \
             above fast slope {fast_slope:.3}): {:?}",
            pfal.points
        );
    }

    /// INV-STAMP: missing pdk is refused.
    #[test]
    fn inv_stamp_missing_pdk() {
        let curve = MoneyPlotCurve {
            dut: "test".to_string(),
            points: vec![(1e-8, 1e-12), (1e-7, 1e-13)],
        };
        let stamps = Stamps {
            pdk: "".to_string(),
            simulator: "ngspice".to_string(),
            harness: "v0.1".to_string(),
            db: "none".to_string(),
        };
        let err = stamp_curve(&curve, Some(Rung::Rung0IdealModels), stamps).unwrap_err();
        assert!(
            matches!(err, StampError::MissingPdk),
            "expected MissingPdk, got: {err}"
        );
    }

    /// INV-STAMP: missing simulator is refused.
    #[test]
    fn inv_stamp_missing_simulator() {
        let curve = MoneyPlotCurve {
            dut: "test".to_string(),
            points: vec![(1e-8, 1e-12), (1e-7, 1e-13)],
        };
        let stamps = Stamps {
            pdk: "generic-level1".to_string(),
            simulator: "".to_string(),
            harness: "v0.1".to_string(),
            db: "none".to_string(),
        };
        let err = stamp_curve(&curve, Some(Rung::Rung0IdealModels), stamps).unwrap_err();
        assert!(
            matches!(err, StampError::MissingSimulator),
            "expected MissingSimulator, got: {err}"
        );
    }

    /// INV-STAMP: missing harness stamp is refused.
    #[test]
    fn inv_stamp_missing_harness() {
        let curve = MoneyPlotCurve {
            dut: "test".to_string(),
            points: vec![(1e-8, 1e-12), (1e-7, 1e-13)],
        };
        let stamps = Stamps {
            pdk: "generic-level1".to_string(),
            simulator: "ngspice".to_string(),
            harness: "".to_string(),
            db: "none".to_string(),
        };
        let err = stamp_curve(&curve, Some(Rung::Rung0IdealModels), stamps).unwrap_err();
        assert!(
            matches!(err, StampError::MissingHarness),
            "expected MissingHarness, got: {err}"
        );
    }

    /// INV-STAMP: missing rung is refused.
    #[test]
    fn inv_stamp_missing_rung() {
        let curve = MoneyPlotCurve {
            dut: "test".to_string(),
            points: vec![(1e-8, 1e-12), (1e-7, 1e-13)],
        };
        let stamps = default_stamps();
        let err = stamp_curve(&curve, None, stamps).unwrap_err();
        assert!(
            matches!(err, StampError::MissingRung),
            "expected MissingRung, got: {err}"
        );
    }

    /// Serialization is byte-deterministic for the same input data.
    #[test]
    fn serialization_deterministic() {
        let record = MoneyPlotRecord {
            dut: "test-dut".to_string(),
            family: "test-family".to_string(),
            axes: "EnergyPerOpVsPeriod",
            points: vec![(1e-8, 1.5e-12), (1e-7, 1.5e-13), (1e-6, 1.5e-14)],
            slope: -1.0,
            rung: Rung::Rung0IdealModels,
            stamps: default_stamps(),
        };
        let records = vec![record];
        let a = to_stamped_text(&records);
        let b = to_stamped_text(&records);
        assert_eq!(a.as_bytes(), b.as_bytes(), "not byte-identical");
    }
}
