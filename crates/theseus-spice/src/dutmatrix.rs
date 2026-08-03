//! Bench **DUT-matrix machinery** (M3a W5; test-spec-m3a "Bench DUT matrix",
//! ADR-0024 B2.3): the (Vt × body × load) variant axes as a driver that
//! characterizes a cell across every variant into the [`CharDb`] and runs
//! the per-variant admission checklist.
//!
//! The axes (see [`crate::adiabatic`] for the device-level definitions and
//! the discovered PDK mapping):
//! * **Vt** -- [`Sky130Vt`] `{std, lvt, hvt}` (hvt is PMOS-only on sky130's
//!   1.8 V core family; lvt PMOS is L ≥ 0.35 µm -- both recorded, not hidden);
//! * **body** -- [`BodyConfig`] `{body-to-rail, deep-n-well}` (DNW = NMOS in
//!   an isolated, source-tied p-well plus the PDK's parasitic isolation
//!   junction stack);
//! * **load** -- [`tg_load_axis`] `{min, 4×, 16×}` with min = 62.5 fF so the
//!   16× point is the canonical 1 pF the pre-matrix cell used.
//!
//! Per (variant, load) the driver sweeps energy-per-op vs power-clock period
//! (every source integrated over the settled cycle -- [`TgVariantPoint`]),
//! keys the sweep into the DB as `adiabatic-tg@{vt}-{body}+c{load}f`, fits
//! the ADR-0028 model per key, and runs the five-gate admission on the
//! variant's own devices -- reusing the just-measured sweep as the gate-5
//! scaling evidence so nothing is simulated twice.
//!
//! Scope note: this module is the *machinery* plus a bounded demonstration
//! on the adiabatic TG buffer (the simplest cell). The spec's full grid
//! (k-CSWAP family × ≥5 points/decade × ≥3 decades) is a W5/W6 batch run
//! over this same driver, not more code.

use std::io;

use crate::adiabatic::{adiabatic_tg_sky130_curve, Sky130Variant, TgVariantPoint};
use crate::admission::{admit_tg_sky130_variant, AdmissionReport};
use crate::chardb::{rebuild, CharDb, CharPoint};

/// The deterministic char-DB key for one (variant, load) cell of the TG
/// matrix, e.g. `"adiabatic-tg@lvt-dnw+c250.0f"`.
pub fn tg_matrix_key(variant: Sky130Variant, c_load_f: f64) -> String {
    format!("adiabatic-tg@{}+c{:.1}f", variant.key(), c_load_f * 1e15)
}

/// One characterized cell of the (Vt × body × load) matrix: the raw sweep
/// (with the per-source energy decomposition), and the five-gate admission
/// report for exactly this variant's devices at this load.
#[derive(Clone, Debug)]
pub struct TgMatrixEntry {
    /// The char-DB key ([`tg_matrix_key`]).
    pub key: String,
    /// The (Vt, body) variant.
    pub variant: Sky130Variant,
    /// Load capacitance, farads.
    pub c_load_f: f64,
    /// The measured sweep, ascending period.
    pub points: Vec<TgVariantPoint>,
    /// The per-variant admission verdict (gate 5 evaluated over `points`).
    pub admission: AdmissionReport,
}

impl TgMatrixEntry {
    /// The point at `period_s` (exact match against the swept axis).
    pub fn at(&self, period_s: f64) -> Option<&TgVariantPoint> {
        self.points.iter().find(|p| p.period_s == period_s)
    }
}

/// The full matrix result: the characterization DB (one keyed cell per
/// (variant, load), each with an ADR-0028 fit) plus the per-variant entries
/// in deterministic (variant, load) axis order.
#[derive(Clone, Debug)]
pub struct TgMatrixResult {
    pub db: CharDb,
    pub entries: Vec<TgMatrixEntry>,
}

impl TgMatrixResult {
    /// The entry for a (variant, load) cell.
    pub fn entry(&self, variant: Sky130Variant, c_load_f: f64) -> Option<&TgMatrixEntry> {
        self.entries
            .iter()
            .find(|e| e.variant == variant && e.c_load_f == c_load_f)
    }
}

/// Characterize the adiabatic TG buffer across `variants` × `loads_f` over
/// the ramp (`periods`) sweep: one energy sweep per (variant, load), rebuilt
/// into a [`CharDb`] keyed by [`tg_matrix_key`] (sorted points + ADR-0028
/// fit per key, deterministic), and one [`admit_tg_sky130_variant`] run per
/// (variant, load) with the sweep reused as gate-5 scaling evidence.
///
/// The (variant, load) cells run **concurrently, one worker thread per
/// cell** (each simulation is its own `ngspice -b` subprocess, and ~35 s of
/// every rung-1 sim is ngspice parsing the sky130 lib -- the matrix is
/// embarrassingly parallel, and running it serially would turn the bounded
/// demo into an hour). Results are assembled back in deterministic
/// (variant, load) axis order, so the DB build stays byte-deterministic.
///
/// Rung-1 sky130 only -- gate callers on [`crate::adiabatic::sky130_available`]
/// (and ngspice). `periods` must be ascending with ≥ 5 points (the gate-5
/// fit minimum; 3 points fit the 3-parameter model exactly and prove
/// nothing).
pub fn characterize_tg_matrix(
    variants: &[Sky130Variant],
    loads_f: &[f64],
    periods: &[f64],
) -> io::Result<TgMatrixResult> {
    assert!(periods.len() >= 5, "matrix sweep needs >= 5 periods (gate-5 fit minimum)");
    assert!(
        periods.windows(2).all(|w| w[0] < w[1]),
        "periods must be strictly ascending"
    );
    let cells: Vec<(Sky130Variant, f64)> = variants
        .iter()
        .flat_map(|&v| loads_f.iter().map(move |&c| (v, c)))
        .collect();
    let results: Vec<io::Result<TgMatrixEntry>> = std::thread::scope(|scope| {
        let handles: Vec<_> = cells
            .iter()
            .map(|&(variant, c_load_f)| {
                scope.spawn(move || -> io::Result<TgMatrixEntry> {
                    let points = adiabatic_tg_sky130_curve(periods, variant, c_load_f)?;
                    let scaling: Vec<(f64, f64)> =
                        points.iter().map(|p| (p.period_s, p.etot.abs())).collect();
                    let admission = admit_tg_sky130_variant(variant, c_load_f, scaling)?;
                    Ok(TgMatrixEntry {
                        key: tg_matrix_key(variant, c_load_f),
                        variant,
                        c_load_f,
                        points,
                        admission,
                    })
                })
            })
            .collect();
        handles
            .into_iter()
            .map(|h| h.join().expect("matrix worker thread panicked"))
            .collect()
    });
    let mut raw: Vec<(String, Vec<CharPoint>)> = Vec::new();
    let mut entries: Vec<TgMatrixEntry> = Vec::new();
    for result in results {
        let entry = result?;
        raw.push((
            entry.key.clone(),
            entry
                .points
                .iter()
                .map(|p| CharPoint { t_ramp: p.period_s, energy: p.etot.abs() })
                .collect(),
        ));
        entries.push(entry);
    }
    Ok(TgMatrixResult { db: rebuild(&raw), entries })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adiabatic::{
        sky130_available, tg_load_axis, BodyConfig, Sky130Variant, Sky130Vt, TG_LOAD_MIN_F,
    };
    use crate::bench::ngspice_available;

    /// Key format is deterministic and readable.
    #[test]
    fn matrix_key_format() {
        let v = Sky130Variant { vt: Sky130Vt::Lvt, body: BodyConfig::DeepNWell };
        assert_eq!(tg_matrix_key(v, 250e-15), "adiabatic-tg@lvt-dnw+c250.0f");
        assert_eq!(
            tg_matrix_key(Sky130Variant::default(), TG_LOAD_MIN_F),
            "adiabatic-tg@std-b2r+c62.5f"
        );
    }

    /// The bounded W5 demonstration: the full 6-variant (Vt × body) grid ×
    /// the 3-point load axis on the adiabatic TG buffer, over a modest
    /// 5-point ramp sweep (the gate-5 minimum -- this is the demo, not the
    /// spec's ≥5-points/decade production grid).
    ///
    /// Asserts, from the REAL measurements (values recorded in the report
    /// for this change; probe references at T=1e-7 s, 250 fF):
    /// * the matrix builds: 18 keyed cells, each with a finite ADR-0028 fit
    ///   and a five-gate admission verdict;
    /// * every variant actually does work (full-swing output);
    /// * load monotonicity: E(min) < E(4×) < E(16×) at fixed variant/period;
    /// * Vt ordering (body-to-rail, fixed load/period): lvt < std < hvt
    ///   (measured 8.2e-15 < 1.22e-14 < 1.82e-14 J);
    /// * body effect cost (std and hvt): deep-n-well (source-tied NMOS body)
    ///   dissipates LESS than body-to-rail (measured std 1.04e-14 vs
    ///   1.22e-14 J) -- the ADR-0024 "what does body effect cost a sky130
    ///   T-gate" number. lvt is NOT asserted directionally: its body-effect
    ///   gain is small enough that the DNW well parasitics roughly cancel it
    ///   (measured +4% -- recorded, not asserted);
    /// * the DNW well shift is visible in the VDD-rail term: evdd(dnw) >
    ///   evdd(b2r) at every Vt (the isolation stack's junction bookkeeping
    ///   flows through the DNW tap; measured ≈ +3.5e-18 J/cycle).
    #[test]
    fn tg_matrix_demo_full_grid() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        if !sky130_available() {
            eprintln!("[skip] sky130 PDK unavailable (THESEUS_SKY130_LIB unset/missing)");
            return;
        }
        // Modest demo sweep: 5 points (gate-5 minimum), 2 decades, fully
        // inside the rung-1 adiabatic window at EVERY load. (A first run
        // used the canonical-cell admission sweep starting at 3.16e-9 s;
        // at the 16× = 1 pF load that fastest point sits on the finite-ramp
        // knee -- the output reached only 1.61 V = 89% of VDD -- so the demo
        // sweep starts a step slower and full swing is asserted at every
        // point instead of special-casing the knee.)
        let periods = [1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6];
        let t_ref = 1e-7; // reference period for cross-variant comparisons
        let variants = Sky130Variant::all();
        let loads = tg_load_axis();
        let m = characterize_tg_matrix(&variants, &loads, &periods).expect("matrix build failed");

        // (a) Complete: 6 × 3 = 18 cells, each with points, a fit, and a
        // five-gate verdict.
        assert_eq!(m.entries.len(), 18);
        assert_eq!(m.db.cells.len(), 18);
        for e in &m.entries {
            let cell = m.db.cells.get(&e.key).unwrap_or_else(|| panic!("{} missing", e.key));
            assert_eq!(cell.points.len(), periods.len());
            assert!(cell.fit.r_squared.is_finite(), "{}: fit did not converge", e.key);
            eprintln!(
                "{}: E({t_ref:.0e})={:.4e} J  k_adia={:.3e}  R²={:.5}  admitted={}",
                e.key,
                e.at(t_ref).unwrap().etot.abs(),
                cell.fit.k_adia,
                cell.fit.r_squared,
                e.admission.admitted()
            );
            eprintln!("{}", e.admission.summary());
            // Every variant must actually do work at every swept period --
            // no vacuous curves in the DB.
            for p in &e.points {
                assert!(
                    p.vmax > 0.9 * 1.8,
                    "{} @ T={:.2e}: output only reached {:.3} V",
                    e.key,
                    p.period_s,
                    p.vmax
                );
                assert!(p.etot.abs() > 0.0, "{}: zero energy", e.key);
            }
        }

        let etot = |vt, body, load: f64| -> f64 {
            m.entry(Sky130Variant { vt, body }, load)
                .unwrap()
                .at(t_ref)
                .unwrap()
                .etot
                .abs()
        };
        let evdd = |vt, body, load: f64| -> f64 {
            m.entry(Sky130Variant { vt, body }, load).unwrap().at(t_ref).unwrap().evdd
        };

        // (b) Load monotonicity at every (Vt, body).
        for &v in &variants {
            let (e1, e4, e16) = (
                etot(v.vt, v.body, loads[0]),
                etot(v.vt, v.body, loads[1]),
                etot(v.vt, v.body, loads[2]),
            );
            assert!(
                e1 < e4 && e4 < e16,
                "{}: load axis not monotone: {e1:.3e}, {e4:.3e}, {e16:.3e}",
                v.key()
            );
        }

        // (c) Vt ordering, body-to-rail, mid load: lvt < std < hvt.
        let mid = loads[1];
        let (e_lvt, e_std, e_hvt) = (
            etot(Sky130Vt::Lvt, BodyConfig::BodyToRail, mid),
            etot(Sky130Vt::Std, BodyConfig::BodyToRail, mid),
            etot(Sky130Vt::Hvt, BodyConfig::BodyToRail, mid),
        );
        eprintln!("Vt ordering @ T={t_ref:.0e}, C={mid:.2e}: lvt={e_lvt:.4e} std={e_std:.4e} hvt={e_hvt:.4e}");
        assert!(
            e_lvt < e_std && e_std < e_hvt,
            "Vt R_on ordering must show in the energy: lvt={e_lvt:.3e}, std={e_std:.3e}, hvt={e_hvt:.3e}"
        );

        // (d) Body-effect cost (std, hvt): DNW source-tied body beats
        // body-to-rail. (lvt: recorded only -- see doc comment.)
        for vt in [Sky130Vt::Std, Sky130Vt::Hvt] {
            let (b2r, dnw) =
                (etot(vt, BodyConfig::BodyToRail, mid), etot(vt, BodyConfig::DeepNWell, mid));
            eprintln!(
                "body effect, {}: b2r={b2r:.4e} J, dnw={dnw:.4e} J ({:+.1}%)",
                vt.key(),
                100.0 * (dnw - b2r) / b2r
            );
            assert!(
                dnw < b2r,
                "{}: removing the body effect must reduce dissipation: dnw={dnw:.3e} >= b2r={b2r:.3e}",
                vt.key()
            );
        }
        {
            let vt = Sky130Vt::Lvt;
            let (b2r, dnw) =
                (etot(vt, BodyConfig::BodyToRail, mid), etot(vt, BodyConfig::DeepNWell, mid));
            eprintln!(
                "body effect, lvt (recorded, not asserted): b2r={b2r:.4e} J, dnw={dnw:.4e} J ({:+.1}%)",
                100.0 * (dnw - b2r) / b2r
            );
        }

        // (e) DNW well shift on the VDD rail at every Vt: the isolation
        // stack draws net energy through the DNW tap.
        for vt in Sky130Vt::ALL {
            let (b2r, dnw) = (
                evdd(vt, BodyConfig::BodyToRail, mid),
                evdd(vt, BodyConfig::DeepNWell, mid),
            );
            eprintln!("evdd shift, {}: b2r={b2r:.3e} J, dnw={dnw:.3e} J", vt.key());
            assert!(
                dnw > b2r,
                "{}: DNW isolation stack must shift the VDD-rail energy up: dnw={dnw:.3e} <= b2r={b2r:.3e}",
                vt.key()
            );
        }

        // (f) Admission verdicts: every cell has all five gates evaluated,
        // and the canonical corner (std, b2r, 16× = the pre-matrix cell)
        // is admitted.
        for e in &m.entries {
            for (gate, r) in [
                ("functional", &e.admission.functional),
                ("conservation", &e.admission.conservation),
                ("inv-token", &e.admission.token_invariant),
                ("reverse", &e.admission.reverse_drive),
                ("scaling", &e.admission.scaling),
            ] {
                assert!(!r.detail.is_empty(), "{}: gate {gate} has no evidence", e.key);
            }
        }
        let canonical = m
            .entry(Sky130Variant::default(), loads[2])
            .expect("canonical corner missing");
        assert!(
            canonical.admission.admitted(),
            "the canonical (std, b2r, 1 pF) corner must be admitted:\n{}",
            canonical.admission.summary()
        );
    }
}
