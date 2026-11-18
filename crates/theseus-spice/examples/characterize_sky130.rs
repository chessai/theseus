//! Generate the sky130 (rung-1) characterization DB
//! (`data/sky130-chardb.ron`).
//!
//! Characterizes every admitted reversible cell -- the adiabatic
//! transmission-gate buffer, the k-CSWAP family (k = 0..3) and the R1
//! rail-role swap -- on the real SkyWater sky130 `tt` device models
//! ([`DeviceRung::Rung1Sky130`]), over ten power-clock periods spaced three
//! per decade from 1e-8 s to 1e-5 s. That span covers both ends of each
//! cell's adiabatic window: the R_on*C knee at the fast end and the
//! sub-threshold leakage floor at the slow end.
//!
//! It also records the irreversible reference, `cmos-static`: a static CMOS
//! inverter in the same devices, driving the same wire load as the cells
//! (`WIRE_LOAD_F`, 100 fF), switched once per period. Its energy is flat in
//! the period (~`C*VDD^2`), and it lives in the same database so the
//! comparison is like for like.
//!
//! The cells are our own dual-rail transmission-gate circuits built from
//! sky130 *primitive transistors* (`sky130_fd_pr`); the sky130 standard-cell
//! libraries are irreversible static CMOS and are not used.
//!
//! Needs ngspice and the PDK, i.e. the `char` dev shell:
//!
//! ```sh
//! nix develop .#char --command \
//!   cargo run --release -p theseus-spice --example characterize_sky130 \
//!   > crates/theseus-spice/data/sky130-chardb.ron
//! ```
//!
//! The sweeps run on one thread each (each ngspice deck is an
//! independent subprocess). Per-point source breakdowns go to stderr; the
//! RON database goes to stdout.

use std::thread;

use theseus_spice::adiabatic::{
    adiabatic_tg_curve_rung, cmos_baseline_curve_load, sky130_available, DeviceRung,
};
use theseus_spice::chardb::{rebuild_interpolated, CharPoint};
use theseus_spice::cswap::{cell_energy_curve, CellTopo, WIRE_LOAD_F};

/// Three points per decade, 1e-8 s ..= 1e-5 s.
fn periods() -> Vec<f64> {
    (0..=9).map(|i| 10f64.powf(-8.0 + i as f64 / 3.0)).collect()
}

fn sweep_cell(key: &'static str, topo: CellTopo, periods: &[f64]) -> (String, Vec<CharPoint>) {
    let rung = DeviceRung::Rung1Sky130;
    let vdd = rung.vdd();
    let curve = cell_energy_curve(topo, rung, periods)
        .unwrap_or_else(|e| panic!("{key} energy sweep failed: {e}"));
    let mut points = Vec::new();
    for p in &curve {
        eprintln!(
            "{key} T={:.3e} etot={:.4e} epc={:.4e} evdd={:.4e} ein={:.4e} eack={:.4e} vmax={:.3}",
            p.period_s, p.etot, p.epc, p.evdd, p.ein, p.eack, p.vmax
        );
        // Non-vacuity: a cell whose output never reaches the rail is not
        // doing the operation being priced.
        assert!(
            p.vmax > 0.9 * vdd,
            "{key} at T={:.3e}: output only reaches {:.3} V (< 0.9*VDD)",
            p.period_s,
            p.vmax
        );
        points.push(CharPoint {
            t_ramp: p.period_s,
            energy: p.etot.abs(),
        });
    }
    (key.to_string(), points)
}

fn main() {
    assert!(
        sky130_available(),
        "sky130 PDK not found: run inside `nix develop .#char` (THESEUS_SKY130_LIB)"
    );
    let periods = periods();

    let cells: [(&'static str, CellTopo); 5] = [
        ("cswap-k0", CellTopo::K(0)),
        ("cswap-k1", CellTopo::K(1)),
        ("cswap-k2", CellTopo::K(2)),
        ("cswap-k3", CellTopo::K(3)),
        ("cswap-r1", CellTopo::R1),
    ];

    let mut raw: Vec<(String, Vec<CharPoint>)> = thread::scope(|s| {
        let tg = s.spawn(|| {
            let curve = adiabatic_tg_curve_rung(&periods, DeviceRung::Rung1Sky130);
            let points: Vec<CharPoint> = curve
                .points
                .iter()
                .map(|&(t_ramp, energy)| {
                    eprintln!("adiabatic-tg T={t_ramp:.3e} etot={energy:.4e}");
                    CharPoint { t_ramp, energy }
                })
                .collect();
            ("adiabatic-tg".to_string(), points)
        });
        let cmos = s.spawn(|| {
            let curve = cmos_baseline_curve_load(&periods, DeviceRung::Rung1Sky130, WIRE_LOAD_F);
            let points: Vec<CharPoint> = curve
                .points
                .iter()
                .map(|&(t_ramp, energy)| {
                    eprintln!("cmos-static T={t_ramp:.3e} etot={energy:.4e}");
                    CharPoint { t_ramp, energy }
                })
                .collect();
            ("cmos-static".to_string(), points)
        });
        let handles: Vec<_> = cells
            .iter()
            .map(|&(key, topo)| {
                let periods = &periods;
                s.spawn(move || sweep_cell(key, topo, periods))
            })
            .collect();
        let mut out = vec![tg.join().expect("adiabatic-tg sweep panicked")];
        out.push(cmos.join().expect("cmos-static sweep panicked"));
        for h in handles {
            out.push(h.join().expect("cell sweep panicked"));
        }
        out
    });
    raw.sort_by(|a, b| a.0.cmp(&b.0));

    print!("{}", rebuild_interpolated(&raw).to_ron());
}
