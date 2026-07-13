//! Tests for the analytic per-clause energy model (M4-v1).
//!
//! (a) analytic_buffer_count matches balance()'s actual K0 count on >=2
//!     small nets (exact).
//! (b) balanced_energy on a single instruction and on the step is
//!     positive with a sensible kind-breakdown, using the REAL char DB.
//! (c) Print the per-clause energy table so the numbers are visible.
//!
//! The full-step lowering is memoized via `OnceLock` so it happens at
//! most once across all tests. The balanced step netlist is NEVER
//! constructed (that's the whole point of the analytic buffer count).

use std::sync::OnceLock;

use theseus_netlist::{balance, lower_iso, CellKind, PhasedCellNetlist};

use theseus_spice::chardb::{rung0_measured, CharDb};

use theseus_tisc::decoder::Op;
use theseus_tisc::energy::{
    analytic_buffer_count, balanced_energy, clause_report, instruction_energy, step_energy,
};

const T_RAMP: f64 = 1e-7;

fn char_db() -> &'static CharDb {
    static DB: OnceLock<CharDb> = OnceLock::new();
    DB.get_or_init(rung0_measured)
}

/// Small ROM used for step-level tests.
fn test_rom() -> Vec<u16> {
    vec![
        // addr 0: ADDI r0, #1  (I-format: opc=3, rd=0, imm=1)
        (3u16 << 12) | (0 << 9) | (1 << 1),
        // addr 1: XORI r1, #0xFF
        (5u16 << 12) | (1 << 9) | (0xFF << 1),
    ]
}

/// Memoized unbalanced step netlist (lowered ONCE, never balanced).
fn step_net() -> &'static PhasedCellNetlist {
    static NET: OnceLock<PhasedCellNetlist> = OnceLock::new();
    NET.get_or_init(|| {
        let rom = test_rom();
        let step_iso = theseus_tisc::step::step(&rom);
        lower_iso(&step_iso).expect("lower_iso step")
    })
}

// ── (a) analytic_buffer_count matches balance() ──────────────────────

fn lower_and_check_buffer_count(iso: &theseus_core::Iso, label: &str) {
    let net = lower_iso(iso).expect("lower_iso");
    let analytic = analytic_buffer_count(&net);
    let balanced = balance(&net).expect("balance");
    let actual_k0 = balanced.cell_counts().get(&CellKind::K0).copied().unwrap_or(0);
    // The original net may already have active K0 cells; the balanced
    // net's K0 count = original_active_k0 + inserted_buffers.
    let original_k0 = net.cell_counts().get(&CellKind::K0).copied().unwrap_or(0);
    let inserted = actual_k0 - original_k0;
    assert_eq!(
        analytic, inserted,
        "{label}: analytic={analytic}, balance inserted={inserted} \
         (balanced K0={actual_k0}, original K0={original_k0})"
    );
    eprintln!("{label}: analytic_buffer_count={analytic}, balance inserted={inserted} ✓");
}

#[test]
fn analytic_matches_balance_xori() {
    let iso = theseus_tisc::instr::xori(0, 1);
    lower_and_check_buffer_count(&iso, "XORI r0,#1");
}

#[test]
fn analytic_matches_balance_swapbr() {
    let iso = theseus_tisc::instr::swapbr();
    lower_and_check_buffer_count(&iso, "SWAPBR");
}

#[test]
fn analytic_matches_balance_pc_inc() {
    let iso = theseus_tisc::step::pc_inc();
    lower_and_check_buffer_count(&iso, "pc_inc");
}

#[test]
fn analytic_matches_balance_exch() {
    let iso = theseus_tisc::instr::exch(0, 1);
    lower_and_check_buffer_count(&iso, "EXCH r0,r1");
}

// ── (b) balanced_energy positive with sensible breakdown ─────────────

#[test]
fn instruction_energy_positive_and_has_breakdown() {
    let db = char_db();
    let iso = theseus_tisc::instr::add(0, 1);
    let bd = instruction_energy(&iso, db, T_RAMP).unwrap();
    assert!(bd.total_j > 0.0, "total energy must be positive");
    assert!(bd.active_j > 0.0, "active energy must be positive");
    assert!(bd.buffer_j >= 0.0, "buffer energy must be non-negative");
    let diff = (bd.total_j - (bd.active_j + bd.buffer_j)).abs();
    assert!(diff < 1e-30, "total = active + buffer");
    assert!(!bd.by_kind.is_empty(), "breakdown must have entries");
    eprintln!("ADD r0,r1 energy:\n{bd}");
}

#[test]
fn step_balanced_energy_positive() {
    let db = char_db();
    let net = step_net();
    let bd = balanced_energy(net, db, T_RAMP).unwrap();
    assert!(bd.total_j > 0.0, "step total energy must be positive");
    assert!(bd.active_j > 0.0, "step active energy must be positive");
    assert!(
        bd.buffer_count > 0,
        "step must need balance buffers (it has deep skew)"
    );
    assert!(bd.buffer_j > 0.0, "step buffer energy must be positive");
    eprintln!("Step energy:\n{bd}");
}

// ── (c) per-clause energy table ──────────────────────────────────────

#[test]
fn print_clause_energy_table() {
    let db = char_db();
    let rom = test_rom();
    let net = step_net();
    // A tiny trace: 2 steps.
    let trace = vec![Op::Addi, Op::Xori];
    let report = clause_report(&rom, db, T_RAMP, Some(net), Some(&trace)).unwrap();
    eprintln!("\n{report}");

    // Sanity: program total should be 2x the step energy.
    let program_total = report.program_total_j.unwrap();
    assert!(program_total > 0.0, "program total must be positive");

    let step_bd = step_energy(net, db, T_RAMP).unwrap();
    let expected = step_bd.total_j * 2.0;
    assert!(
        (program_total - expected).abs() < 1e-30,
        "program total ({program_total:.4e}) should equal 2 * step ({expected:.4e})"
    );
}
