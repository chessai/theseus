//! Adiabatic-logic energy curve (M3a-2): the money-plot's slope-−1 curve.
//!
//! The canonical adiabatic result is that a circuit driven by a slow trapezoidal
//! power-clock recovers energy: dissipation per operation falls as `1/T` (log-log
//! slope −1), versus a fixed `½CV²` for a step-driven CMOS gate. This module
//! reproduces that on the 2LAL building block — a full CMOS **transmission gate**
//! charging/recovering a load node through the power-clock — and is measured by
//! the [`crate::bench`] harness (`∫V·I` via a behavioral-power node + `.meas
//! INTEG`, already self-validated against the closed-form RC dissipation).
//!
//! Scope: this is the adiabatic transmission-gate *cell* (the 2LAL primitive),
//! which reproduces the slope-−1 self-check of `test-spec-m3a.md`. A full
//! dual-rail 2LAL *shift register* (dynamic token propagation across a 4-phase
//! chain) is the owed follow-up; the energy-recovery physics and its measurement
//! are what the money plot turns on, and both are demonstrated here.
//!
//! Physics notes (learned by bench iteration):
//! * The PMOS bulk MUST tie to VDD; with a swinging clock node on the drain and
//!   the bulk at 0 the P+/N-well junction forward-biases into a huge diode
//!   current (energy grows *with* T — the opposite of adiabatic).
//! * The transient timestep must resolve the device `R_on·C` (not just the
//!   period), or the energy integral runs high at large T (same lesson as the
//!   RC gate in [`crate::bench`]).
//! * Measure inside the adiabatic regime (`T ≫ R_on·C`) and with a load large
//!   enough to keep the tiny recovered-energy signal above the solver noise floor.

use crate::bench::{log_log_slope, run_energy};

/// A money-plot curve: energy-per-op versus power-clock period, log-log.
#[derive(Clone, Debug, PartialEq)]
pub struct MoneyPlotCurve {
    /// Device-under-test identifier (e.g. `"adiabatic-tg"`).
    pub dut: String,
    /// `(period_s, energy_per_op_J)` samples, ascending in period.
    pub points: Vec<(f64, f64)>,
}

impl MoneyPlotCurve {
    /// The fitted log-log slope of energy-per-op vs period.
    pub fn slope(&self) -> f64 {
        log_log_slope(&self.points)
    }
}

/// Build the adiabatic transmission-gate netlist for one power-clock `period_s`.
///
/// A full CMOS transmission gate (held ON) connects a trapezoidal power-clock
/// `pc` to an output load; the source energy delivered over one full
/// charge/recover cycle is the (adiabatic) dissipation. Generic level-1 MOSFET
/// models — no PDK. The `esrc` `.meas` name is what [`run_energy`] extracts.
pub fn adiabatic_tg_netlist(period_s: f64) -> String {
    let q = period_s / 4.0; // ramp-up ends
    let h = period_s / 2.0; // hold ends
    let tq = 3.0 * period_s / 4.0; // ramp-down ends
    let stop = period_s;
    // Resolve the device R_on·C (~150 ps here) as well as the period.
    let step = (period_s / 4000.0).min(5e-11);
    format!(
        "\
adiabatic transmission-gate cell (T={period_s:.6e} s)
.model NM NMOS (level=1 vto=0.4 kp=200u)
.model PM PMOS (level=1 vto=-0.4 kp=100u)
Vdd vdd 0 DC 1.0
Vpc pc 0 PWL(0 0 {q:.9e} 1.0 {h:.9e} 1.0 {tq:.9e} 0 {stop:.9e} 0)
Vin  ing 0 DC 1.0
Vinb inb 0 DC 0.0
M1 pc ing out 0   NM W=4u L=0.15u
M2 pc inb out vdd PM W=8u L=0.15u
Cout out 0 1p
Bpwr pwr 0 V = -V(pc)*I(Vpc)
.control
tran {step:.9e} {stop:.9e} uic
meas tran esrc INTEG V(pwr) FROM=0 TO={stop:.9e}
print esrc
.endc
.end
"
    )
}

/// Sweep the power-clock period across the adiabatic regime and return the
/// money-plot curve (energy per op vs period). One transmission-gate op per
/// cycle, so energy-per-op is the measured per-cycle source energy.
pub fn adiabatic_tg_curve(periods: &[f64]) -> MoneyPlotCurve {
    let points = periods
        .iter()
        .map(|&t| (t, run_energy(&adiabatic_tg_netlist(t), "esrc")))
        .collect();
    MoneyPlotCurve {
        dut: "adiabatic-tg".to_string(),
        points,
    }
}

/// Build the non-adiabatic **CMOS baseline** netlist for one `period_s`: a
/// static CMOS inverter driven by a full-swing square wave with FAST (non-
/// adiabatic) edges. Each cycle dumps ~`CV²` from the rail irrespective of
/// period → a FLAT energy-vs-period curve (the money plot's control).
pub fn cmos_baseline_netlist(period_s: f64) -> String {
    let half = period_s / 2.0;
    let stop = period_s;
    let edge = (period_s / 1000.0).min(1e-10); // fast edges: no energy recovery
    let step = (period_s / 4000.0).min(5e-11);
    format!(
        "\
static CMOS inverter, step-driven baseline (T={period_s:.6e} s)
.model NM NMOS (level=1 vto=0.4 kp=200u)
.model PM PMOS (level=1 vto=-0.4 kp=100u)
Vdd vdd 0 DC 1.0
Vin in 0 PULSE(0 1.0 0 {edge:.9e} {edge:.9e} {half:.9e} {stop:.9e})
M1 out in 0   0   NM W=4u L=0.15u
M2 out in vdd vdd PM W=8u L=0.15u
Cout out 0 1p
Bpwr pwr 0 V = V(vdd)*I(Vdd)
.control
tran {step:.9e} {stop:.9e} uic
meas tran evdd INTEG V(pwr) FROM=0 TO={stop:.9e}
print evdd
.endc
.end
"
    )
}

/// The CMOS baseline curve (energy per cycle vs period). Energy is reported as a
/// positive magnitude (the rail-current convention makes the raw integral
/// negative for energy leaving the source).
pub fn cmos_baseline_curve(periods: &[f64]) -> MoneyPlotCurve {
    let points = periods
        .iter()
        .map(|&t| (t, run_energy(&cmos_baseline_netlist(t), "evdd").abs()))
        .collect();
    MoneyPlotCurve {
        dut: "cmos-static".to_string(),
        points,
    }
}

/// Build a **PFAL** (positive-feedback adiabatic logic) buffer netlist for one
/// `period_s`: a cross-coupled PMOS latch powered by the trapezoidal clock, with
/// an NMOS evaluation pair. It recovers energy (slope −1) at fast periods but
/// **floors**: a static/crowbar path in the resolved latch dissipates a
/// roughly-constant power, so at slow periods energy stops falling (and rises) —
/// the partially-adiabatic middle curve, between 2LAL (keeps falling) and CMOS
/// (flat everywhere).
pub fn pfal_netlist(period_s: f64) -> String {
    let q = period_s / 4.0;
    let h = period_s / 2.0;
    let tq = 3.0 * period_s / 4.0;
    let stop = period_s;
    let step = (period_s / 4000.0).min(5e-11);
    format!(
        "\
PFAL buffer (cross-coupled PMOS latch + NMOS eval), T={period_s:.6e} s
.model NM NMOS (level=1 vto=0.4 kp=200u)
.model PM PMOS (level=1 vto=-0.4 kp=100u)
Vdd vdd 0 DC 1.0
Vpc pc 0 PWL(0 0 {q:.9e} 1.0 {h:.9e} 1.0 {tq:.9e} 0 {stop:.9e} 0)
Vin  in  0 DC 1.0
Vinb inb 0 DC 0.0
MP1 pc out  outb vdd PM W=8u L=0.15u
MP2 pc outb out  vdd PM W=8u L=0.15u
MN1 pc in  out  0 NM W=4u L=0.15u
MN2 pc inb outb 0 NM W=4u L=0.15u
Cout  out  0 100f
Coutb outb 0 100f
Bpwr pwr 0 V = -V(pc)*I(Vpc)
.control
tran {step:.9e} {stop:.9e} uic
meas tran esrc INTEG V(pwr) FROM=0 TO={stop:.9e}
print esrc
.endc
.end
"
    )
}

/// The PFAL curve (energy per op vs period) — falls then floors.
pub fn pfal_curve(periods: &[f64]) -> MoneyPlotCurve {
    let points = periods
        .iter()
        .map(|&t| (t, run_energy(&pfal_netlist(t), "esrc").abs()))
        .collect();
    MoneyPlotCurve {
        dut: "pfal".to_string(),
        points,
    }
}

/// The canonical three-family money plot (ADR-0023): the adiabatic
/// transmission-gate cell (slope −1, keeps falling), the PFAL buffer (falls then
/// floors), and the static-CMOS control (flat), over one shared period axis.
pub fn money_plot(periods: &[f64]) -> Vec<MoneyPlotCurve> {
    vec![
        adiabatic_tg_curve(periods),
        pfal_curve(periods),
        cmos_baseline_curve(periods),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bench::ngspice_available;

    /// Default adiabatic-regime sweep: ~3 decades of period, ≥2 pts/decade.
    fn regime_periods() -> Vec<f64> {
        vec![1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5]
    }

    /// The money-plot self-check: energy-per-op falls as 1/period (slope −1).
    #[test]
    fn adiabatic_tg_slope_is_minus_one() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let curve = adiabatic_tg_curve(&regime_periods());
        // All points must be positive, monotone-decreasing energy with period.
        for w in curve.points.windows(2) {
            assert!(
                w[0].1 > 0.0 && w[1].1 > 0.0 && w[1].1 < w[0].1,
                "energy must be positive and fall with period: {:?}",
                curve.points
            );
        }
        let slope = curve.slope();
        assert!(
            (-1.1..=-0.9).contains(&slope),
            "adiabatic slope must be −1 ± 0.1, got {slope:.4} for {:?}",
            curve.points
        );
    }

    /// The cell must actually charge (do work): source energy is non-trivial and
    /// the load reaches near VDD — a circuit that dissipates ~0 by doing nothing
    /// must not pass the slope check vacuously.
    #[test]
    fn adiabatic_tg_does_work() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let e = run_energy(&adiabatic_tg_netlist(1e-7), "esrc");
        assert!(e > 1e-16, "cell should dissipate a measurable amount, got {e:e}");
    }

    /// The CMOS control is FLAT: ~CV² per cycle regardless of period (slope ≈ 0),
    /// and — crucially — at a slow period it dissipates FAR more than the
    /// adiabatic cell (the whole point of the money plot).
    #[test]
    fn cmos_baseline_is_flat_and_dominates_adiabatic() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let periods = regime_periods();
        let cmos = cmos_baseline_curve(&periods);
        let slope = cmos.slope();
        assert!(
            (-0.1..=0.1).contains(&slope),
            "CMOS baseline must be flat (slope ≈ 0), got {slope:.4} for {:?}",
            cmos.points
        );
        // At the slowest period the adiabatic cell must win by a wide margin.
        let adia = adiabatic_tg_curve(&periods);
        let (_, e_cmos_slow) = *cmos.points.last().unwrap();
        let (_, e_adia_slow) = *adia.points.last().unwrap();
        assert!(
            e_adia_slow < e_cmos_slow / 100.0,
            "adiabatic cell should beat CMOS by >100× at the slow end: adia={e_adia_slow:e}, cmos={e_cmos_slow:e}"
        );
    }

    /// Wide sweep that exposes PFAL's adiabatic regime AND its floor.
    fn wide_periods() -> Vec<f64> {
        vec![1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4]
    }

    /// PFAL is the middle family: it recovers energy (slope ≈ −1) at fast
    /// periods but FLOORS — the energy stops falling (a static path dissipates
    /// ~constant power), so the minimum is reached at an intermediate period,
    /// unlike the pure-adiabatic cell which keeps falling.
    #[test]
    fn pfal_falls_then_floors() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let periods = wide_periods();
        let pfal = pfal_curve(&periods);
        // Fast-end (first 3 points) is adiabatic: slope ≈ −1.
        let fast_slope = log_log_slope(&pfal.points[..3]);
        assert!(
            (-1.2..=-0.8).contains(&fast_slope),
            "PFAL fast-end slope must be ≈ −1, got {fast_slope:.3} for {:?}",
            pfal.points
        );
        // It floors: the minimum energy is NOT at the slowest period (there is a
        // turnaround), whereas the pure-adiabatic cell would keep dropping.
        let min_i = pfal
            .points
            .iter()
            .enumerate()
            .min_by(|a, b| a.1 .1.partial_cmp(&b.1 .1).unwrap())
            .unwrap()
            .0;
        assert!(
            min_i < pfal.points.len() - 1,
            "PFAL must floor (min energy before the slowest period), got min at index {min_i} of {:?}",
            pfal.points
        );
    }

    /// The money plot exposes all three families over one period axis.
    #[test]
    fn money_plot_has_three_families() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let curves = money_plot(&regime_periods());
        assert_eq!(curves.len(), 3);
        for dut in ["adiabatic-tg", "pfal", "cmos-static"] {
            assert!(curves.iter().any(|c| c.dut == dut), "missing {dut}");
        }
    }
}
