//! Runnable money-plot artifact emitter (M3a-4).
//!
//! Builds the three-family money plot (adiabatic-tg, pfal, cmos-static),
//! wraps curves into stamped records, and prints both the deterministic
//! RON text artifact and an ASCII plot to stdout.
//!
//! Run: `cargo run -p theseus-spice --example money_plot`

use theseus_spice::adiabatic::money_plot;
use theseus_spice::artifact::{
    default_stamps, render_ascii, stamped_money_plot, to_stamped_text, Rung,
};
use theseus_spice::bench::ngspice_available;

fn main() {
    if !ngspice_available() {
        eprintln!("note: ngspice is not installed or not on PATH -- cannot generate money plot.");
        eprintln!("Install ngspice and re-run to produce the artifact.");
        return;
    }

    // Fixed period axis: ~3 decades of period, ≥2 pts/decade.
    let periods: Vec<f64> = vec![1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5];

    let curves = money_plot(&periods);
    let records =
        stamped_money_plot(&curves, Rung::Rung0IdealModels, default_stamps())
            .expect("INV-STAMP violation in stamped_money_plot");

    // Stamped RON text artifact.
    let text = to_stamped_text(&records);
    println!("{text}");

    // ASCII plot.
    let ascii = render_ascii(&records);
    println!("{ascii}");
}
