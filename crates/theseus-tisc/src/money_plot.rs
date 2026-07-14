//! CPU "money plot" (ADR-0028/0030): energy per TISC-1 step vs the
//! power-clock ramp time — the machine-level analogue of the M3a
//! cell-level money plot.
//!
//! # What it shows
//!
//! Every cell in this adiabatic fabric dissipates `E(T) = k_adia/T +
//! k_leak·T + k_res` (ADR-0028), so the whole compiled step, summed over
//! its cell inventory, inherits the same shape: energy per cycle falls as
//! ~`1/T` (log-log slope ≈ −1) as the power-clock is slowed, until the
//! tiny leakage/residual floor. An *irreversible* CMOS implementation of
//! the same logic pays a flat `½CV²` per gate per cycle, **independent of
//! T** — it cannot be driven below that floor. The plot is that contrast
//! at the machine level: the reversible CPU's curve sliding below the
//! flat CMOS line.
//!
//! # Reproducibility
//!
//! The adiabatic curves come entirely from the committed rung-0
//! characterization DB fits summed over the **zero-garbage** step's cell
//! inventory (ADR-0040 phase C: the R1-based lowering, priced as
//! `cswap-r1`) — no ngspice needed. The flat CMOS reference is a single
//! ngspice-derived constant ([`CMOS_STATIC_E_PER_GATE_J`]), committed so
//! the whole artifact reproduces offline.
//!
//! Two adiabatic curves are reported (ADR-0028 attribution):
//! * **full step** — active logic + the K0 tick-alignment balance
//!   buffers: the honest as-built machine energy;
//! * **active logic** — K1/K2/R1 only, excluding the balance buffers
//!   (which have no irreversible counterpart — CMOS needs no tick
//!   alignment): the logic-proper energy.
//!
//! The CMOS reference counts one `½CV²` gate per *active* reversible cell
//! (1:1, a deliberately conservative gate-count proxy — a Fredkin or an
//! adder's Toffoli is worth more than one CMOS gate, so this understates
//! the reversible win). The balance buffers are excluded from the CMOS
//! gate count on purpose.

use theseus_netlist::PhasedCellNetlist;
use theseus_spice::chardb::{CharDb, QueryError};

use crate::energy::balanced_energy;

/// Rung-0 CMOS static `½CV²` energy per gate, joules — the flat
/// irreversible reference.
///
/// Measured once via ngspice from the `cmos-static` deck (theseus-spice
/// `adiabatic::cmos_baseline_curve_rung`, generic level-1 models,
/// VDD = 1.0 V): the settled per-op energy is T-independent at
/// ≈ 1.019e-12 J across the whole [1e-8, 1e-5] s ramp range (fitted
/// log-log slope ≈ +0.0025, i.e. flat). Committed here so the CPU money
/// plot reproduces without ngspice; regenerate the source figure via
/// `cargo run -p theseus-spice --example money_plot` (read the
/// `cmos-static` curve's asymptotic point).
pub const CMOS_STATIC_E_PER_GATE_J: f64 = 1.01922e-12;

/// One `(t_ramp, energy)` point of a swept curve.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SweepPoint {
    /// Power-clock ramp time, seconds.
    pub t_ramp: f64,
    /// Energy per step at that ramp, joules.
    pub energy_j: f64,
}

/// A CPU money plot: two swept adiabatic curves plus the flat CMOS line.
#[derive(Clone, Debug)]
pub struct CpuMoneyPlot {
    /// Full-step energy vs ramp (active logic + K0 balance buffers).
    pub full_step: Vec<SweepPoint>,
    /// Active-logic-only energy vs ramp (K1/K2/R1, no balance buffers).
    pub active_logic: Vec<SweepPoint>,
    /// Flat CMOS reference energy for the same active-logic gate count.
    pub cmos_ref_j: f64,
    /// Active-logic cell count (the 1:1 CMOS-equivalent gate count).
    pub active_cells: usize,
    /// K0 balance-buffer count (no irreversible counterpart).
    pub buffer_cells: usize,
    /// R1 cell count — the zero-garbage cells (ADR-0040 phase C).
    pub r1_cells: usize,
}

/// Least-squares slope of `log10(energy)` vs `log10(t_ramp)` over a
/// swept curve. An ideal adiabatic curve gives −1.
pub fn log_log_slope(points: &[SweepPoint]) -> f64 {
    let n = points.len() as f64;
    let xs: Vec<f64> = points.iter().map(|p| p.t_ramp.log10()).collect();
    let ys: Vec<f64> = points.iter().map(|p| p.energy_j.log10()).collect();
    let sx: f64 = xs.iter().sum();
    let sy: f64 = ys.iter().sum();
    let sxx: f64 = xs.iter().map(|x| x * x).sum();
    let sxy: f64 = xs.iter().zip(&ys).map(|(x, y)| x * y).sum();
    (n * sxy - sx * sy) / (n * sxx - sx * sx)
}

/// The ramp time at which an adiabatic curve crosses the flat CMOS
/// reference (`Some` when a crossing exists), from the dominant `1/T`
/// term: `t_cross = (E·T)|_fastest / E_cmos`. For `t_ramp > t_cross` the
/// reversible curve is below CMOS.
pub fn cmos_crossing_t_ramp(curve: &[SweepPoint], cmos_ref_j: f64) -> Option<f64> {
    let p = curve.first()?;
    if cmos_ref_j <= 0.0 || p.energy_j <= 0.0 {
        return None;
    }
    // k_adia_eff = E(t)·t at the fastest sampled ramp (adiabatic term).
    Some((p.energy_j * p.t_ramp) / cmos_ref_j)
}

/// Build the CPU money plot for a pre-lowered (unbalanced) step netlist,
/// sweeping `t_ramps` against the characterization DB.
///
/// `step_net` should be a lowered TISC-1 step (via `lower_iso`);
/// `t_ramps` must lie within the DB's characterized range (in-range
/// interpolation only — [`CharDb::query`] errors otherwise, never
/// extrapolates).
pub fn cpu_money_plot(
    step_net: &PhasedCellNetlist,
    db: &CharDb,
    t_ramps: &[f64],
) -> Result<CpuMoneyPlot, QueryError> {
    let mut full_step = Vec::with_capacity(t_ramps.len());
    let mut active_logic = Vec::with_capacity(t_ramps.len());
    let mut active_cells = 0usize;
    let mut buffer_cells = 0usize;
    let mut r1_cells = 0usize;

    for &t in t_ramps {
        let bd = balanced_energy(step_net, db, t)?;
        full_step.push(SweepPoint {
            t_ramp: t,
            energy_j: bd.total_j,
        });
        active_logic.push(SweepPoint {
            t_ramp: t,
            energy_j: bd.active_j,
        });
        // Cell-inventory counts are ramp-independent; capture once.
        active_cells = bd
            .by_kind
            .iter()
            .filter(|(k, _)| k.as_str() != "cswap-k0-balance")
            .map(|(_, v)| v.count)
            .sum();
        buffer_cells = bd.buffer_count;
        r1_cells = bd
            .by_kind
            .get("cswap-r1")
            .map(|v| v.count)
            .unwrap_or(0);
    }

    let cmos_ref_j = active_cells as f64 * CMOS_STATIC_E_PER_GATE_J;

    Ok(CpuMoneyPlot {
        full_step,
        active_logic,
        cmos_ref_j,
        active_cells,
        buffer_cells,
        r1_cells,
    })
}

impl CpuMoneyPlot {
    /// Slope of the full-step adiabatic curve (≈ −1 for ideal adiabatic).
    pub fn full_step_slope(&self) -> f64 {
        log_log_slope(&self.full_step)
    }

    /// Slope of the active-logic adiabatic curve (≈ −1).
    pub fn active_logic_slope(&self) -> f64 {
        log_log_slope(&self.active_logic)
    }

    /// True iff the active-logic energy is below the CMOS floor at every
    /// sampled ramp — the strong claim (reversible logic proper wins
    /// across the whole characterized clock range).
    pub fn active_logic_below_cmos_everywhere(&self) -> bool {
        self.active_logic
            .iter()
            .all(|p| p.energy_j < self.cmos_ref_j)
    }
}

impl std::fmt::Display for CpuMoneyPlot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "TISC-1 CPU money plot — energy/step vs power-clock ramp")?;
        writeln!(f, "{:-<64}", "")?;
        writeln!(
            f,
            "  zero-garbage step inventory: {} active cells \
             ({} R1), {} K0 balance buffers",
            self.active_cells, self.r1_cells, self.buffer_cells
        )?;
        writeln!(
            f,
            "  CMOS reference (flat): {} gates x {:.4e} J = {:.4e} J/step",
            self.active_cells, CMOS_STATIC_E_PER_GATE_J, self.cmos_ref_j
        )?;
        writeln!(f, "{:-<64}", "")?;
        writeln!(
            f,
            "  {:>10}  {:>14}  {:>14}",
            "t_ramp (s)", "full step (J)", "active (J)"
        )?;
        for (fs, al) in self.full_step.iter().zip(&self.active_logic) {
            writeln!(
                f,
                "  {:>10.3e}  {:>14.4e}  {:>14.4e}",
                fs.t_ramp, fs.energy_j, al.energy_j
            )?;
        }
        writeln!(f, "{:-<64}", "")?;
        writeln!(
            f,
            "  full-step slope:    {:+.4}  (ideal adiabatic = -1)",
            self.full_step_slope()
        )?;
        writeln!(
            f,
            "  active-logic slope: {:+.4}",
            self.active_logic_slope()
        )?;
        if let Some(tc) = cmos_crossing_t_ramp(&self.full_step, self.cmos_ref_j) {
            writeln!(
                f,
                "  full-step crosses below CMOS at t_ramp ~ {tc:.3e} s"
            )?;
        }
        writeln!(
            f,
            "  active logic below CMOS across the whole range: {}",
            self.active_logic_below_cmos_everywhere()
        )?;
        write!(
            f,
            "  [claims boundary (ADR-0028 C3.3): logic dissipation under \
             ideal power-clocks; clock-gen/resonator losses and I/O excluded]"
        )
    }
}
