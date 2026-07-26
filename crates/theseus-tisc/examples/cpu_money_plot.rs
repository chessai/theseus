//! CPU money-plot artifact emitter: energy per TISC-1 **step** vs the
//! power-clock ramp time, on the zero-garbage ring body (ADR-0040
//! phase C — R1-based, ancilla-clean lowering).
//!
//! Run: `cargo run -p theseus-tisc --example cpu_money_plot`
//!
//! No ngspice required — the adiabatic curves come from the committed
//! rung-0 characterization DB fits summed over the step's cell
//! inventory; the flat CMOS reference is the committed
//! `CMOS_STATIC_E_PER_GATE_J` constant. Lowering the full step takes a
//! couple of minutes and some GB, but is never *balanced* (the K0 buffer
//! energy is priced analytically).

use theseus_netlist::lower_iso;
use theseus_spice::chardb::rung0_measured;
use theseus_tisc::money_plot::cpu_money_plot;
use theseus_tisc::step::step;

fn main() {
    // A tiny ROM: `decode_execute` (all 14 predicated instructions,
    // lowered every step) dominates the step, so the exact program
    // barely moves the machine-level energy.
    // A-0 32-bit words: I-format = fmt(31)=0 | opcode[30:25] | rd[24:20] | imm[19:0].
    let rom: Vec<u32> = vec![
        (0b011u32 << 25) | 1,                // ROTLI r0, #1 (opc 0b011 = ROTLI)
        (0b101u32 << 25) | (1 << 20) | 0xFF, // reserved I-format opc 0b101 (decodes None)
    ];

    eprintln!("lowering the TISC-1 step (~2 min, unbalanced)...");
    let net = lower_iso(&step(&rom)).expect("lower step");
    let db = rung0_measured();

    // ~2 points/decade across the characterized [1e-8, 1e-5] s range.
    let axis = [1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5];
    let plot = cpu_money_plot(&net, &db, &axis).expect("cpu money plot");

    println!("{plot}");
}
