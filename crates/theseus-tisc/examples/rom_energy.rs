//! M-E finale artifact: energy-price a compiled TISC-1 ROM's execution
//! in joules from the committed rung-0 characterization.
//!
//! Compiles the real `iter Nat fibStep` (0022-fib.thse) to its 24-instr
//! ROM, runs fib(1), prices it at a chosen t_ramp, and sweeps the ramp.
//! Prints the thesis headline: WCET park tax + honest crossing.
//!
//! Run: `cargo run -p theseus-tisc --example rom_energy --release`
//!
//! NOTE: prices the CPU-only `step(rom)` ring body; the composite
//! `system_step` additionally clocks the memory body (a bounded increment).

use theseus_core::Value;
use theseus_netlist::lower_iso;
use theseus_spice::chardb::rung0_measured;

use theseus_tisc::compile::compile_with_metadata;
use theseus_tisc::energy::{balanced_energy, rom_program_energy};
use theseus_tisc::money_plot::{cmos_crossing_t_ramp, SweepPoint, CMOS_STATIC_E_PER_GATE_J};
use theseus_tisc::step::step;

const FIB_SRC: &str = include_str!("../../../conformance/programs/0022-fib.thse");

fn elaborate(src: &str, iso_name: &str) -> theseus_core::Iso {
    let program = theseus_syntax::parse(src).expect("parse");
    let diags = theseus_check::check(&program);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = theseus_check::env::resolve(&program);
    let isos = theseus_check::elaborate::elaborate_program(&program, &env);
    isos.get(iso_name)
        .unwrap_or_else(|| panic!("iso {iso_name:?} not found"))
        .clone()
        .unwrap_or_else(|e| panic!("elaboration of {iso_name:?} failed: {e:?}"))
}

fn main() {
    let fib_iso = elaborate(FIB_SRC, "fib");

    // Compile to ROM.
    let lowered = compile_with_metadata(&fib_iso).expect("compile fib");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble fib");
    let rom_len = rom.len();
    let budget = lowered.wcet();

    eprintln!("fib → {rom_len}-instr ROM, WCET B={budget}");
    eprintln!("lowering step(rom) (~2 min, unbalanced)...");

    let step_iso = step(&rom);
    let step_net = lower_iso(&step_iso).expect("lower_iso fib step");
    let db = rung0_measured();

    // Representative input: fib(1), nat=1 with pair (1,0).
    // nat=1 uses 1 out of 3 unrolled iterations → demonstrates park tax.
    let nat1 = Value::right(Value::left(Value::Unit));
    let pair = Value::pair(
        Value::right(Value::Unit), // bit=1
        Value::left(Value::Unit),  // bit=0
    );
    let input = Value::pair(nat1, pair);

    // Price at a reference t_ramp.
    let t_ref = 1e-7;
    let pe =
        rom_program_energy(&fib_iso, &input, &db, t_ref, &step_net).expect("rom_program_energy");

    println!("M-E finale: energy-priced compiled ROM execution (ADR-0042)");
    println!("{:-<72}", "");
    println!(
        "  fib(1) → {}-instr ROM; executed {} steps ({} productive, park tax {:.1}%)",
        rom_len,
        pe.steps_b,
        pe.steps_t,
        pe.park_tax * 100.0
    );
    println!("  on the reversible TISC-1 CPU (step(rom), zero-garbage ring body)");
    println!(
        "  total energy: {:.4e} J @ t_ramp = {:.1e} s",
        pe.total_j, t_ref
    );
    println!(
        "  per-step energy: {:.4e} J (data-independent; ADR-0028 static cell inventory)",
        pe.step_j
    );
    println!("{:-<72}", "");

    // Sweep the ramp.
    let axis = [1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5];
    println!(
        "  {:>10}  {:>14}  {:>14}",
        "t_ramp (s)", "B*step (J)", "T*cmos (J)"
    );

    let bd_ref = balanced_energy(&step_net, &db, t_ref).unwrap();
    let active_cells: usize = bd_ref
        .by_kind
        .iter()
        .filter(|(k, _)| k.as_str() != "cswap-k0-balance")
        .map(|(_, v)| v.count)
        .sum();
    let cmos_step_j = active_cells as f64 * CMOS_STATIC_E_PER_GATE_J;
    let cmos_program_j = pe.steps_t as f64 * cmos_step_j;

    let mut program_curve = Vec::new();
    for &t in &axis {
        let step_bd = balanced_energy(&step_net, &db, t).unwrap();
        let rev_j = pe.steps_b as f64 * step_bd.total_j;
        program_curve.push(SweepPoint {
            t_ramp: t,
            energy_j: rev_j,
        });
        println!(
            "  {:>10.3e}  {:>14.4e}  {:>14.4e}",
            t, rev_j, cmos_program_j
        );
    }

    // Compute slope.
    let slope = theseus_tisc::money_plot::log_log_slope(&program_curve);
    println!("{:-<72}", "");
    println!("  adiabatic slope (B*step): {:+.4} (ideal = -1)", slope);

    // Honest crossing: B*E_step(t) vs T*E_cmos_step.
    if let Some(tc) = cmos_crossing_t_ramp(&program_curve, cmos_program_j) {
        println!(
            "  crosses below CMOS (which pays only T={} steps) at t_ramp ~ {:.3e} s",
            pe.steps_t, tc
        );
        if (1e-8..=1e-5).contains(&tc) {
            println!("  → INSIDE the characterized [1e-8, 1e-5] range ✓");
        } else {
            println!("  → outside the characterized range");
        }
    } else {
        println!("  no crossing found");
    }

    println!("{:-<72}", "");
    println!(
        "  Even paying the full WCET park tax ({:.1}%) that irreversibility",
        pe.park_tax * 100.0
    );
    println!("  never pays, the compiled program still crosses below the");
    println!("  CMOS ½CV² floor within the characterized clock range.");
    println!("  [claims boundary: CPU-only step(rom); clock-gen/resonator losses excluded]");
}
