#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! CPU money-plot mechanics (ADR-0028/0030).
//!
//! The *headline* numbers (the full TISC-1 step: active logic below the
//! CMOS floor across the whole clock range, full-step crossing ~2e-8 s)
//! are emitted by `examples/cpu_money_plot.rs`, which lowers the ~2-min
//! full step. These tests instead pin the money-plot *machinery* on a
//! fast, R1-bearing net (a single ADD, whose adder carries lower to
//! Toffoli→R1 sandwiches): the adiabatic slope is −1, the zero-garbage
//! R1 cells are priced, and the CMOS reference is a sane flat line.

use std::sync::OnceLock;

use theseus_netlist::{lower_iso, PhasedCellNetlist};
use theseus_spice::chardb::{rung0_measured, CharDb};

use theseus_tisc::money_plot::{cmos_crossing_t_ramp, cpu_money_plot};

/// ~2 points/decade across the characterized [1e-8, 1e-5] s range.
const AXIS: [f64; 7] = [1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5];

fn db() -> &'static CharDb {
    static D: OnceLock<CharDb> = OnceLock::new();
    D.get_or_init(rung0_measured)
}

/// A single ADD, lowered once. Its ripple-carry adder emits Toffolis,
/// which the ADR-0040 phase-C `emit_mcnot` lowers via R1 sandwiches -- so
/// this net exercises the zero-garbage cell in the energy path.
fn add_net() -> &'static PhasedCellNetlist {
    static N: OnceLock<PhasedCellNetlist> = OnceLock::new();
    N.get_or_init(|| lower_iso(&theseus_tisc::instr::add(0, 1)).expect("lower ADD"))
}

#[test]
fn adiabatic_curves_have_slope_minus_one() {
    let plot = cpu_money_plot(add_net(), db(), &AXIS).unwrap();
    let fs = plot.full_step_slope();
    let al = plot.active_logic_slope();
    assert!(
        (fs + 1.0).abs() < 0.05,
        "full-step slope {fs} is not ≈ -1 (ideal adiabatic)"
    );
    assert!(
        (al + 1.0).abs() < 0.05,
        "active-logic slope {al} is not ≈ -1"
    );
    eprintln!("ADD-net money plot:\n{plot}");
}

#[test]
fn zero_garbage_r1_cells_are_priced() {
    let plot = cpu_money_plot(add_net(), db(), &AXIS).unwrap();
    assert!(
        plot.r1_cells > 0,
        "ADD lowering must contain R1 cells (Toffoli via R1 sandwich, \
         ADR-0040 phase C); got {}",
        plot.r1_cells
    );
}

#[test]
fn cmos_reference_flat_and_crossing_finite() {
    let plot = cpu_money_plot(add_net(), db(), &AXIS).unwrap();
    assert!(plot.cmos_ref_j > 0.0, "CMOS reference must be positive");
    // CMOS = active_cells × ½CV²/gate, a flat line.
    let expected = plot.active_cells as f64 * theseus_tisc::money_plot::CMOS_STATIC_E_PER_GATE_J;
    assert!((plot.cmos_ref_j - expected).abs() < 1e-30);
    // The 1/T adiabatic curve must cross the flat CMOS line at a finite,
    // positive ramp time.
    let tc = cmos_crossing_t_ramp(&plot.full_step, plot.cmos_ref_j).expect("finite crossing");
    assert!(
        tc > 0.0 && tc.is_finite(),
        "CMOS crossing must be finite positive, got {tc}"
    );
    // As the clock slows, the adiabatic curve drops below CMOS: the
    // slowest sampled ramp beats the flat floor.
    let slowest = plot.full_step.last().unwrap();
    assert!(
        slowest.energy_j < plot.cmos_ref_j,
        "at the slowest ramp the reversible step ({:.3e} J) must be \
         below the CMOS floor ({:.3e} J)",
        slowest.energy_j,
        plot.cmos_ref_j
    );
}
