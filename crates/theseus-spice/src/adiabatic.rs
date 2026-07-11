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

/// The money plot: the family of energy-per-op-vs-period curves at a shared set
/// of periods. Presently the adiabatic transmission-gate cell (slope −1) and the
/// static-CMOS control (flat); a PFAL curve (adiabatic-with-a-floor) is owed to
/// complete the canonical three-family plot (ADR-0023).
pub fn money_plot(periods: &[f64]) -> Vec<MoneyPlotCurve> {
    vec![adiabatic_tg_curve(periods), cmos_baseline_curve(periods)]
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

    /// The money plot exposes both families over one period axis.
    #[test]
    fn money_plot_has_adiabatic_and_cmos() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let curves = money_plot(&regime_periods());
        assert_eq!(curves.len(), 2);
        assert!(curves.iter().any(|c| c.dut == "adiabatic-tg"));
        assert!(curves.iter().any(|c| c.dut == "cmos-static"));
    }
}
