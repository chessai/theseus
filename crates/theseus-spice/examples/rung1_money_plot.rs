//! Runnable rung-1 (sky130) money-plot artifact emitter (W6).
//!
//! Mirrors `examples/money_plot.rs`, but generates the three-family money plot
//! on the real SkyWater sky130 tt models via [`money_plot_rung`], and stamps it
//! as a `rung1-sky130-tt` artifact carrying the pinned sky130 PDK version.
//!
//! The period axis is the pinned rung-1 characterization window
//! (1e-8 .. 1e-5 s, ~7 log-spaced points): over it, adiabatic-TG is adiabatic
//! (slope ≈ −0.96), PFAL falls-then-floors (floor near ~1e-6 s), and static
//! CMOS is flat (~3.3e-12 J) -- the three-family money-plot shape on sky130.
//!
//! Run: `cargo run -p theseus-spice --example rung1_money_plot`
//! (needs the `.#char` devShell for ngspice + the sky130 PDK). Without the PDK
//! it prints a note and exits 0, so a PDK-less CI never hangs.

use theseus_spice::adiabatic::{money_plot_rung, sky130_available, DeviceRung};
use theseus_spice::artifact::{
    render_ascii, stamped_money_plot, to_stamped_text, Rung, Stamps,
};
use theseus_spice::bench::ngspice_available;

fn main() {
    if !ngspice_available() {
        eprintln!("note: ngspice is not installed or not on PATH -- cannot generate money plot.");
        eprintln!("Install ngspice (the .#char devShell provides it) and re-run.");
        return;
    }
    if !sky130_available() {
        eprintln!(
            "note: sky130 PDK unavailable (THESEUS_SKY130_LIB unset/missing) -- \
             rung-1 money plot needs the .#char devShell. Skipping (exit 0)."
        );
        return;
    }

    // Pinned rung-1 period axis: ~7 log-spaced points over 1e-8 .. 1e-5 s.
    let axis: Vec<f64> = vec![1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5];

    let curves = money_plot_rung(&axis, DeviceRung::Rung1Sky130);

    // The pdk stamp carries the real sky130 PDK build hash (from the .#char
    // devShell); fall back to a labelled unknown so the artifact is still
    // stamped if the var is unset.
    let pdk = std::env::var("THESEUS_SKY130_VERSION")
        .unwrap_or_else(|_| "sky130A-unknown".to_string());
    let stamps = Stamps {
        pdk,
        simulator: "ngspice".to_string(),
        harness: "theseus-spice-w6-rung1-v0.1".to_string(),
        db: "chardb-rung1-sky130".to_string(),
    };

    let records = stamped_money_plot(&curves, Rung::Rung1Sky130Tt, stamps)
        .expect("INV-STAMP violation in stamped_money_plot");

    // Stamped RON text artifact.
    let text = to_stamped_text(&records);
    println!("{text}");

    // ASCII plot.
    let ascii = render_ascii(&records);
    println!("{ascii}");
}
