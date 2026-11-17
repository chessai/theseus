//! Pricing compiled logic from the **sky130** characterization database
//! (`theseus_spice::chardb::sky130_measured`): our dual-rail cells built from
//! sky130 primitive transistors, measured in ngspice, committed as data.
//!
//! Offline: these tests read the committed measurements. The fast tests use a
//! single ADD (a small net that still contains R1 cells and balance buffers);
//! the `heavy-tests` one prices the whole TISC step and pins its numbers.

use std::sync::OnceLock;

use theseus_netlist::{lower_iso, PhasedCellNetlist};
use theseus_spice::chardb::{rung0_measured, sky130_measured, CharDb};

use theseus_tisc::money_plot::{
    cmos_energy_per_gate_j, cpu_money_plot, CMOS_STATIC_E_PER_GATE_J, CMOS_STATIC_KEY,
};

/// The ten periods the sky130 database was measured at.
fn periods() -> Vec<f64> {
    (0..=9).map(|i| 10f64.powf(-8.0 + i as f64 / 3.0)).collect()
}

fn sky130() -> &'static CharDb {
    static D: OnceLock<CharDb> = OnceLock::new();
    D.get_or_init(sky130_measured)
}

fn add_net() -> &'static PhasedCellNetlist {
    static N: OnceLock<PhasedCellNetlist> = OnceLock::new();
    N.get_or_init(|| lower_iso(&theseus_tisc::instr::add(0, 1)).expect("lower ADD"))
}

/// On real devices the energy of compiled logic has a best clock period:
/// it falls as the clock slows, then leakage turns it back up. On the ideal
/// rung-0 devices it only falls.
#[test]
fn sky130_energy_has_an_interior_minimum_and_rung0_does_not() {
    let ts = periods();

    let real = cpu_money_plot(add_net(), sky130(), &ts).unwrap();
    let min = real.full_step_minimum().expect("non-empty sweep");
    assert!(
        min.t_ramp > ts[0] && min.t_ramp < ts[ts.len() - 1],
        "sky130 minimum must be interior, got T = {:e}",
        min.t_ramp
    );
    let first = real.full_step[0].energy_j;
    let last = real.full_step[ts.len() - 1].energy_j;
    assert!(
        min.energy_j < first / 10.0,
        "the fast end must be far above the minimum"
    );
    assert!(
        min.energy_j < last / 2.0,
        "the slow end must be above the minimum"
    );

    // rung 0 was measured on a coarser axis; sample it at its own points.
    let ideal_axis = [1e-8, 1e-7, 1e-6, 1e-5];
    let ideal = cpu_money_plot(add_net(), &rung0_measured(), &ideal_axis).unwrap();
    assert_eq!(ideal.full_step_minimum().unwrap().t_ramp, 1e-5);
    assert!(ideal
        .full_step
        .windows(2)
        .all(|w| w[1].energy_j < w[0].energy_j));
}

/// The irreversible reference comes from the same database as the cells when
/// it has one (sky130: same transistors, same wire load), and only otherwise
/// from the rung-0 constant.
#[test]
fn cmos_reference_is_the_databases_own_measurement() {
    let t = 1e-6;
    let measured = sky130()
        .query(CMOS_STATIC_KEY, t)
        .expect("sky130 has cmos-static");
    assert_eq!(cmos_energy_per_gate_j(sky130(), t), measured);
    assert!((3.0e-13..4.0e-13).contains(&measured), "got {measured:e} J");

    let plot = cpu_money_plot(add_net(), sky130(), &[t]).unwrap();
    assert_eq!(plot.cmos_per_gate_j, measured);
    assert_eq!(plot.cmos_ref_j, plot.active_cells as f64 * measured);

    // rung 0 has no measured reference: behaviour is unchanged.
    let ideal = rung0_measured();
    assert_eq!(
        cmos_energy_per_gate_j(&ideal, 1e-7),
        CMOS_STATIC_E_PER_GATE_J
    );
    let plot0 = cpu_money_plot(add_net(), &ideal, &[1e-7]).unwrap();
    assert_eq!(
        plot0.cmos_ref_j,
        plot0.active_cells as f64 * CMOS_STATIC_E_PER_GATE_J
    );
}

/// The whole default-layout TISC step priced on sky130 (pinned). Slow: lowers
/// the full step netlist.
///
/// What the numbers say: per step the machine is best at a 1 µs period, where
/// it dissipates about 0.67 nJ -- 1.7x less than charging its 3290 active
/// cells as static-CMOS gates at the same wire load. Its active logic alone
/// is about 54x below that reference; the difference is the ~589k
/// tick-alignment buffers, which carry ~97% of the energy. Away from the
/// minimum (faster than ~0.3 µs or slower than ~3 µs) the reference wins.
#[cfg(feature = "heavy-tests")]
#[test]
fn full_step_on_sky130_golden() {
    // The two-word reference ROM of the CPU money plot.
    let rom: Vec<u32> = vec![(0b011u32 << 25) | 1, (0b101u32 << 25) | (1 << 20) | 0xFF];
    let net = lower_iso(&theseus_tisc::step::step(&rom)).expect("lower step");
    let plot = cpu_money_plot(&net, sky130(), &periods()).unwrap();

    assert_eq!(plot.active_cells, 3290);
    assert_eq!(plot.r1_cells, 620);
    assert_eq!(plot.buffer_cells, 589_119);

    let close = |got: f64, want: f64| (got / want - 1.0).abs() < 1e-3;
    let min = plot.full_step_minimum().unwrap();
    assert!(
        (min.t_ramp / 1e-6 - 1.0).abs() < 1e-9,
        "minimum at T = {:e}",
        min.t_ramp
    );
    assert!(
        close(min.energy_j, 6.6619e-10),
        "full step {:e} J",
        min.energy_j
    );

    let at = |t: f64| {
        let i = periods()
            .iter()
            .position(|p| (p / t - 1.0).abs() < 1e-9)
            .unwrap();
        (plot.full_step[i].energy_j, plot.active_logic[i].energy_j)
    };
    let (full, active) = at(1e-6);
    let cmos = plot.active_cells as f64 * sky130().query(CMOS_STATIC_KEY, 1e-6).unwrap();
    assert!(close(cmos, 1.1491e-9), "CMOS-equivalent {cmos:e} J");
    assert!(close(cmos / full, 1.725), "CMOS/reversible {}", cmos / full);
    assert!(close(active, 2.1442e-11), "active logic {active:e} J");
    assert!(cmos / active > 50.0);
    // Buffers dominate: active logic is a few percent of the step.
    assert!(active / full < 0.05);

    // The window is narrow: the reference wins at both ends of the sweep.
    assert!(at(1e-7).0 > cmos && at(1e-5).0 > cmos);
}
