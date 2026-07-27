#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! Tests for the analytic per-clause energy model (M4-v1).
//!
//! (a) analytic_buffer_count matches balance()'s actual K0 count on >=2
//!     small nets (exact).
//! (b) balanced_energy on a single instruction and on the step is
//!     positive with a sensible kind-breakdown, using the REAL char DB.
//! (c) Print the per-clause energy table so the numbers are visible.
//! (d) M-E: ROM program energy (ADR-0042) — fib priced in joules.
//!
//! The full-step lowering is memoized via `OnceLock` so it happens at
//! most once across all tests. The balanced step netlist is NEVER
//! constructed (that's the whole point of the analytic buffer count).
//!
//! **Run with `--test-threads=1`**: this file has two independent step-net
//! lowerings (2-word ROM + 24-word fib ROM), each ~GB; concurrent lowering
//! exhausts memory.

use std::sync::{Mutex, OnceLock};

use theseus_netlist::{balance, lower_iso, CellKind, PhasedCellNetlist};

use theseus_spice::chardb::{rung0_measured, CharDb};

use theseus_tisc::compile::{compile_with_metadata, marshal};
use theseus_tisc::decoder::Op;
use theseus_tisc::energy::{
    analytic_buffer_count, balanced_energy, clause_report, instruction_energy, rom_program_energy,
    rom_trace, step_energy, ProgramEnergy,
};
use theseus_tisc::money_plot::{cmos_crossing_t_ramp, CMOS_STATIC_E_PER_GATE_J};

const T_RAMP: f64 = 1e-7;

/// Gate to serialize expensive step-net lowerings — concurrent lowerings
/// exhaust memory and trigger SIGKILL.
static LOWER_GATE: Mutex<()> = Mutex::new(());

fn char_db() -> &'static CharDb {
    static DB: OnceLock<CharDb> = OnceLock::new();
    DB.get_or_init(rung0_measured)
}

/// Small ROM used for step-level tests.
#[allow(clippy::identity_op)]
fn test_rom() -> Vec<u32> {
    vec![
        // addr 0: ROTLI r0, #1  (A-0 I-format: opc=0b011=ROTLI, rd=0, imm=1)
        (3u32 << 25) | (0 << 20) | 1,
        // addr 1: reserved I-format opc 0b101 (decodes None; not a valid instruction)
        (5u32 << 25) | (1 << 20) | 0xFF,
    ]
}

/// Memoized unbalanced step netlist (lowered ONCE, never balanced).
fn step_net() -> &'static PhasedCellNetlist {
    static NET: OnceLock<PhasedCellNetlist> = OnceLock::new();
    NET.get_or_init(|| {
        let _guard = LOWER_GATE.lock().unwrap();
        let rom = test_rom();
        let step_iso = theseus_tisc::step::step(&rom);
        lower_iso(&step_iso).expect("lower_iso step")
    })
}

// ── POC: bit-identical netlist across the Type repr change ───────────
//
// The `Box<Type>` → `Arc<Type>` repr change (lean-lower_iso) MUST produce a
// bit-identical `PhasedCellNetlist`: it changes only how `Type` subtrees are
// held in memory (deep copy vs O(1) shared refcount), never the produced net.
// This POC lowers `step(test_rom)` and asserts the net matches a fingerprint
// captured from the pre-change (Box) code on base `9b12f7e`. The fingerprint is
// a full-net digest: `Debug` of the whole `PhasedCellNetlist` (every cell's
// kind/depth/phase/rails, both boundary Types, num_lines) hashed — it changes
// if ANY produced bit changes. Reproduces the finding: identical old-vs-new.
//
// How the golden was captured: check out base `9b12f7e` (Box<Type>), copy this
// test in, run it printing (cells.len, num_lines, fp); the printed values ARE
// the golden below.
//
// NOTE (gate tier): this whole file is `#![cfg(feature = "heavy-tests")]`, so
// this POC runs in the HEAVY tier (`--features heavy-tests`), not the fast gate
// — it must lower a full step net (seconds + GBs), which is heavy by nature.
// The advisor's pre-merge heavy run is the gate; a fast-only run won't exercise
// it.
fn net_fingerprint(net: &PhasedCellNetlist) -> u64 {
    // Debug is a deterministic, total serialization of every field of the net
    // (both boundary Types, all cells with kind/depth/phase/inputs/outputs,
    // output_depths, num_lines). Any produced-bit change perturbs this string.
    //
    // Hashed with FNV-1a (inline, dependency-free) rather than the stdlib
    // `DefaultHasher`: `DefaultHasher`'s algorithm is explicitly NOT guaranteed
    // stable across Rust versions, so its output could drift on a toolchain bump
    // and spuriously fail this golden even on a bit-identical net. FNV-1a is a
    // fixed, version-stable algorithm, so `GOLDEN_FP` stays meaningful.
    let s = format!("{net:?}");
    let mut hash: u64 = 0xcbf29ce484222325; // FNV offset basis
    for b in s.as_bytes() {
        hash ^= *b as u64;
        hash = hash.wrapping_mul(0x100000001b3); // FNV prime
    }
    hash
}

#[test]
fn poc_step_net_bit_identical_across_type_repr() {
    let net = step_net();
    let cells = net.cells.len();
    let num_lines = net.num_lines;
    let fp = net_fingerprint(net);
    eprintln!(
        "POC step(test_rom) net: cells={cells}, num_lines={num_lines}, fingerprint={fp:#018x}"
    );

    // Golden captured from base `9b12f7e` (Box<Type> repr) — the pre-change net.
    // A pure representation change MUST reproduce these exactly.
    // Captured from base `9b12f7e` (Box<Type>) by running the identical
    // FNV-1a-over-Debug fingerprint on the pre-change net: cells=5406,
    // num_lines=32612, fingerprint=0xf155e931576a44b7, peak RSS 57.4 GB (vs
    // Arc: identical net, ~4 GB peak). Byte-for-byte identical ⇒ pure repr
    // change, net bit-for-bit equal old-vs-new.
    const GOLDEN_CELLS: usize = 5406;
    const GOLDEN_NUM_LINES: u32 = 32612;
    const GOLDEN_FP: u64 = 0xf155_e931_576a_44b7;

    assert_eq!(
        cells, GOLDEN_CELLS,
        "cell count changed under the Type repr change — NOT bit-identical"
    );
    assert_eq!(
        num_lines, GOLDEN_NUM_LINES,
        "num_lines changed under the Type repr change — NOT bit-identical"
    );
    assert_eq!(
        fp, GOLDEN_FP,
        "net fingerprint changed under the Type repr change — NOT bit-identical"
    );
}

// ── (a) analytic_buffer_count matches balance() ──────────────────────

fn lower_and_check_buffer_count(iso: &theseus_core::Iso, label: &str) {
    let net = lower_iso(iso).expect("lower_iso");
    let analytic = analytic_buffer_count(&net);
    let balanced = balance(&net).expect("balance");
    let actual_k0 = balanced
        .cell_counts()
        .get(&CellKind::K0)
        .copied()
        .unwrap_or(0);
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
    let iso = theseus_tisc::instr::swapbr(0);
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

// ── M-E: ROM program energy tests (ADR-0042) ────────────────────────

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

fn fib_iso() -> &'static theseus_core::Iso {
    static ISO: OnceLock<theseus_core::Iso> = OnceLock::new();
    ISO.get_or_init(|| elaborate(FIB_SRC, "fib"))
}

/// Memoized fib step net (lowered from fib's OWN 24-word ROM — F2).
fn fib_step_net() -> &'static PhasedCellNetlist {
    static NET: OnceLock<PhasedCellNetlist> = OnceLock::new();
    NET.get_or_init(|| {
        let _guard = LOWER_GATE.lock().unwrap();
        let iso = fib_iso();
        let lowered = compile_with_metadata(iso).expect("compile fib");
        let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble fib");
        eprintln!("fib ROM: {} instructions, lowering step(rom)...", rom.len());
        let step_iso = theseus_tisc::step::step(&rom);
        lower_iso(&step_iso).expect("lower_iso fib step")
    })
}

/// A representative fib input: Nat=1 (the 2nd domain value, k=1),
/// with initial pair (1,0) — computes fib(1). This gives T < B
/// (one iteration used out of 3 unrolled), demonstrating the park tax.
fn fib_input() -> theseus_core::Value {
    use theseus_core::Value;
    // Nat encoding for k=1: Right(Left(Unit)) = second variant of Sum type
    // The fib iso type: Nat * (Bit * Bit)
    let nat1 = Value::right(Value::left(Value::Unit));
    let pair = Value::pair(
        Value::right(Value::Unit), // bit=1
        Value::left(Value::Unit),  // bit=0
    );
    Value::pair(nat1, pair)
}

/// Get fib program energy at the default T_RAMP.
fn fib_program_energy() -> ProgramEnergy {
    let iso = fib_iso();
    let db = char_db();
    let net = fib_step_net();
    let input = fib_input();
    rom_program_energy(iso, &input, db, T_RAMP, net).expect("rom_program_energy fib")
}

// ── me_a1: total_j == B × step_j (identity) ────────────────────────

#[test]
fn me_a1_total_is_b_times_step() {
    let pe = fib_program_energy();
    assert!(pe.total_j > 0.0, "total_j must be positive");
    assert!(pe.step_j > 0.0, "step_j must be positive");
    let expected = pe.steps_b as f64 * pe.step_j;
    // This is an identity (exact model value), not an approximation.
    // Allow only float-accumulation tolerance (the multiply is a single op).
    let rel_err = ((pe.total_j - expected) / expected).abs();
    assert!(
        rel_err < 1e-15,
        "total_j = B*step_j identity violated: total={:.6e}, B*step={:.6e}, rel_err={:.2e}",
        pe.total_j,
        expected,
        rel_err
    );
    eprintln!(
        "me_a1: total_j={:.6e} == {}*{:.6e} ✓",
        pe.total_j, pe.steps_b, pe.step_j
    );
}

// ── me_a2: B vs T ───────────────────────────────────────────────────

#[test]
fn me_a2_b_vs_t() {
    let iso = fib_iso();
    let pe = fib_program_energy();

    // B == wcet(iso)
    let lowered = compile_with_metadata(iso).expect("compile fib");
    let expected_b = lowered.wcet();
    assert_eq!(
        pe.steps_b, expected_b,
        "steps_b must equal wcet(iso): got {}, expected {}",
        pe.steps_b, expected_b
    );

    // T: verify by independently running rom_trace
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let (in_ty, _) = theseus_core::type_of(iso).expect("type_of");
    let input = fib_input();
    let init = marshal(&input, &in_ty);
    let trace = rom_trace(&rom, init, expected_b);
    assert_eq!(pe.steps_t, trace.steps_t, "steps_t mismatch");

    // T < B (there IS a park phase for this input)
    assert!(
        pe.steps_t < pe.steps_b,
        "fib(1) must halt before WCET (guarded static unroll skips 2 of 3 iters): T={}, B={}",
        pe.steps_t,
        pe.steps_b
    );

    // productive_j == T * step_j
    let expected_prod = pe.steps_t as f64 * pe.step_j;
    let rel_err = ((pe.productive_j - expected_prod) / expected_prod).abs();
    assert!(rel_err < 1e-15, "productive_j identity: {rel_err:.2e}");

    // park_tax == (B-T)/B
    let expected_tax = (pe.steps_b - pe.steps_t) as f64 / pe.steps_b as f64;
    assert!(
        (pe.park_tax - expected_tax).abs() < 1e-15,
        "park_tax: got {}, expected {}",
        pe.park_tax,
        expected_tax
    );
    assert!(pe.park_tax >= 0.0, "park_tax must be non-negative");

    // idle energy = total - productive = (B-T)*step_j
    let idle_j = pe.total_j - pe.productive_j;
    let expected_idle = (pe.steps_b - pe.steps_t) as f64 * pe.step_j;
    let rel_err = if expected_idle > 0.0 {
        ((idle_j - expected_idle) / expected_idle).abs()
    } else {
        idle_j.abs()
    };
    assert!(rel_err < 1e-15, "idle energy identity: {rel_err:.2e}");

    eprintln!(
        "me_a2: B={}, T={}, park_tax={:.2}% ✓",
        pe.steps_b,
        pe.steps_t,
        pe.park_tax * 100.0
    );
}

// ── me_a3: golden (F-teeth) ─────────────────────────────────────────

#[test]
fn me_a3_golden() {
    let pe = fib_program_energy();

    // Pinned golden values (deterministic: committed char-DB fits + fixed
    // lowering of fib's own 24-word ROM step net at T_RAMP=1e-7).
    // RE-PINNED for A-6.4 widen-PC (16-bit PC: appends the PC_hi CpuState field,
    // widening the state to STATE_BITS=184 / NUM_FIELDS=23). Measured under the
    // lean-lower_iso Arc<Type> repr (slice-1) which makes this wide-state
    // lowering fit host RAM (peak ~7 GB, was ~86 GB → OOM). The widened
    // pc_add_br/pc_inc + narrow fetch shift step energy DOWN relative to the
    // pre-widen A-0 baseline: step_j 2.7054170445e-9 → 2.5259595646e-9 (−6.6%),
    // total_j 6.4930009067e-8 → 6.0623029551e-8, total_j = 24·step_j. B=24/T=20
    // codegen unchanged. The NET is bit-identical across the repr change (POC
    // poc_step_net_bit_identical_across_type_repr), so this golden reflects ONLY
    // the widen-PC state change, never the repr. Deterministic (per-cell sum).
    const GOLDEN_TOTAL_J: f64 = 6.0623029551e-8;
    const GOLDEN_STEP_J: f64 = 2.5259595646e-9;
    const GOLDEN_B: usize = 24;
    const GOLDEN_T: usize = 20;

    assert_eq!(pe.steps_b, GOLDEN_B, "B changed (codegen regression)");
    assert_eq!(pe.steps_t, GOLDEN_T, "T changed (trace regression)");

    // Tight tolerance: deterministic path, only float arithmetic order
    // can vary (should be bit-exact in practice).
    let tol = 1e-6; // relative tolerance
    let rel_total = ((pe.total_j - GOLDEN_TOTAL_J) / GOLDEN_TOTAL_J).abs();
    assert!(
        rel_total < tol,
        "total_j golden regression: got {:.10e}, expected {:.10e}, rel_err={:.2e}",
        pe.total_j,
        GOLDEN_TOTAL_J,
        rel_total
    );
    let rel_step = ((pe.step_j - GOLDEN_STEP_J) / GOLDEN_STEP_J).abs();
    assert!(
        rel_step < tol,
        "step_j golden regression: got {:.10e}, expected {:.10e}, rel_err={:.2e}",
        pe.step_j,
        GOLDEN_STEP_J,
        rel_step
    );

    eprintln!(
        "me_a3 GOLDEN: total_j={:.10e}, B={}, T={}, step_j={:.10e}, park_tax={:.2}% ✓",
        pe.total_j,
        pe.steps_b,
        pe.steps_t,
        pe.step_j,
        pe.park_tax * 100.0
    );
}

// ── me_a4: honest crossing ──────────────────────────────────────────

#[test]
fn me_a4_honest_crossing() {
    let iso = fib_iso();
    let db = char_db();
    let net = fib_step_net();
    let input = fib_input();

    let pe = rom_program_energy(iso, &input, db, T_RAMP, net).expect("rom_program_energy");
    let b = pe.steps_b;
    let t = pe.steps_t;

    // Active cell count for the CMOS gate count.
    let bd = balanced_energy(net, db, T_RAMP).unwrap();
    let active_cells: usize = bd
        .by_kind
        .iter()
        .filter(|(k, _)| k.as_str() != "cswap-k0-balance")
        .map(|(_, v)| v.count)
        .sum();
    let cmos_step_j = active_cells as f64 * CMOS_STATIC_E_PER_GATE_J;

    // Program-level CMOS baseline: irreversible pays T steps only (F3).
    let cmos_program_j = t as f64 * cmos_step_j;

    // Sweep to find the program-level crossing: B*E_step(t) vs T*E_cmos_step.
    // Use the same approach as cmos_crossing_t_ramp but for program level.
    let axis = [1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5];
    let mut program_curve = Vec::new();
    for &t_r in &axis {
        let step_bd = balanced_energy(net, db, t_r).unwrap();
        program_curve.push(theseus_tisc::money_plot::SweepPoint {
            t_ramp: t_r,
            energy_j: b as f64 * step_bd.total_j,
        });
    }

    // The program crossing: where B*E_step(t) = T*E_cmos_step
    let program_crossing = cmos_crossing_t_ramp(&program_curve, cmos_program_j);

    let tc = program_crossing.expect("program crossing must exist");
    eprintln!("me_a4: program crossing at t_ramp ~ {tc:.3e} s");

    // Must be inside the characterized range [1e-8, 1e-5].
    assert!(
        (1e-8..=1e-5).contains(&tc),
        "program crossing {tc:.3e} must be in [1e-8, 1e-5]"
    );

    // The program crossing must be >= the per-step crossing (park tax shifts right).
    let per_step_curve: Vec<_> = axis
        .iter()
        .map(|&t_r| {
            let step_bd = balanced_energy(net, db, t_r).unwrap();
            theseus_tisc::money_plot::SweepPoint {
                t_ramp: t_r,
                energy_j: step_bd.total_j,
            }
        })
        .collect();
    let per_step_crossing =
        cmos_crossing_t_ramp(&per_step_curve, cmos_step_j).expect("per-step crossing must exist");
    // The program crossing must be the per-step crossing shifted right by
    // EXACTLY B/T (cmos_crossing_t_ramp is linear in the energy ratio; F3:
    // reversible pays B steps, CMOS pays only T). This FALSIFIES a B-vs-B
    // regression — if CMOS were charged B steps, the B would cancel and
    // tc == per_step_crossing (no shift), which this assertion rejects.
    let expected = per_step_crossing * (b as f64 / t as f64);
    let rel_err = ((tc - expected) / expected).abs();
    assert!(
        rel_err < 1e-3,
        "program crossing ({tc:.3e}) must equal per-step ({per_step_crossing:.3e}) × B/T ({:.3}) \
         = {expected:.3e}; rel_err={rel_err:.2e} — a B-vs-B regression gives no shift and fails here",
        b as f64 / t as f64
    );

    eprintln!(
        "me_a4: per-step crossing={:.3e}, program crossing={:.3e} (ratio B/T={:.2}) ✓",
        per_step_crossing,
        tc,
        b as f64 / t as f64
    );
}

// ── B1.1: shared configurable ALU measurement (design open-Q1) ──────
//
// The PRIMARY deliverable of B1.1: does folding Group-A's six per-op
// predicated cores into ONE configurable ALU core (route once, decode the
// opcode once into config lines, run one gated core) actually SHRINK the
// lowered decode netlist, or does route fan-out create balance-buffer skew
// that eats the win? Reports decode_execute cells + fib step_j vs the
// committed baseline. Prints only — no golden assertion here (me_a3_golden
// is re-pinned during finish, after adversarial review).
//
// Baseline (design doc §1.1, pre-B1.1): decode_execute = 874 510 cells;
// step_j = 2.5259595646e-9 J; B=24 / T=20.
#[test]
fn poc_b11_shared_alu_measure() {
    const BASE_DECODE_CELLS: usize = 874_510;
    const BASE_STEP_J: f64 = 2.5259595646e-9;

    // (1) decode_execute BALANCED cell count — the same metric as the
    // design baseline (`clause_report` / `instruction_energy` = lower THEN
    // balance, counting the K0 balance buffers). This is what step_j is
    // built from, so it is the apples-to-apples Q1 number. (The raw
    // `lower_iso` count is ~187× smaller but is a different, pre-balance
    // metric — not comparable to 874 510.)
    let de_iso = theseus_tisc::decoder::decode_execute();
    let de_bd = {
        let _guard = LOWER_GATE.lock().unwrap();
        instruction_energy(&de_iso, char_db(), T_RAMP).expect("decode_execute energy")
    };
    let de_cells: usize = de_bd.by_kind.values().map(|k| k.count).sum();
    let cell_ratio = BASE_DECODE_CELLS as f64 / de_cells as f64;
    eprintln!(
        "B1.1 decode_execute BALANCED cells: {} → {} (Δ {:+}, {:.3}× smaller); \
         active_j={:.4e} buffer_j={:.4e} total_j={:.4e}",
        BASE_DECODE_CELLS,
        de_cells,
        de_cells as isize - BASE_DECODE_CELLS as isize,
        cell_ratio,
        de_bd.active_j,
        de_bd.buffer_j,
        de_bd.total_j,
    );

    // (1b) Group-A PORTION shrink (the actual Q1 shrink-bar target): lower
    // the pre-B1.1 Group-A block (6 per-op predicated cores) vs the shared
    // configurable ALU, both BALANCED. Group A is the only part of decode
    // B1.1 touched, so this isolates the fold's win from the untouched
    // Ccnot/Cswap/Xchg/Swapbr/Group-B mass that dilutes the whole-decode Δ.
    let (ga_pre_cells, ga_new_cells) = {
        let _guard = LOWER_GATE.lock().unwrap();
        let pre = instruction_energy(
            &theseus_tisc::decoder::group_a_pre_b11_for_measurement(),
            char_db(),
            T_RAMP,
        )
        .expect("group_a pre energy");
        let new = instruction_energy(
            &theseus_tisc::decoder::group_a_shared_for_measurement(),
            char_db(),
            T_RAMP,
        )
        .expect("group_a new energy");
        let pc: usize = pre.by_kind.values().map(|k| k.count).sum();
        let nc: usize = new.by_kind.values().map(|k| k.count).sum();
        (pc, nc)
    };
    let ga_ratio = ga_pre_cells as f64 / ga_new_cells as f64;
    eprintln!(
        "B1.1 Group-A PORTION balanced cells: {} → {} (Δ {:+}, {:.3}× smaller)",
        ga_pre_cells,
        ga_new_cells,
        ga_new_cells as isize - ga_pre_cells as isize,
        ga_ratio
    );
    eprintln!(
        "B1.1 Q1 shrink bar (~1.3× on Group-A portion): {}",
        if ga_ratio >= 1.3 { "CLEARED" } else { "NOT CLEARED" }
    );

    // (2) fib step_j via the me_a3 path (same ROM/trace/input).
    let pe = fib_program_energy();
    eprintln!(
        "B1.1 step_j: {:.10e} → {:.10e} (Δ {:+.3}%)  B={} T={}",
        BASE_STEP_J,
        pe.step_j,
        (pe.step_j - BASE_STEP_J) / BASE_STEP_J * 100.0,
        pe.steps_b,
        pe.steps_t
    );
    eprintln!(
        "B1.1 total_j: {:.10e}  (= B·step_j = {}·{:.6e})",
        pe.total_j, pe.steps_b, pe.step_j
    );

    // (3) B/T must be invariant (step_j-only change — same ROM, same trace).
    assert_eq!(pe.steps_b, 24, "B changed (must be step_j-only)");
    assert_eq!(pe.steps_t, 20, "T changed (must be step_j-only)");

    // NEW golden values to re-pin during finish (report, do not assert):
    eprintln!(
        "B1.1 RE-PIN CANDIDATES: step_j={:.10e} total_j={:.10e} decode_cells={}",
        pe.step_j, pe.total_j, de_cells
    );
}

// ── me_a5: range (negative) ─────────────────────────────────────────

#[test]
fn me_a5_range() {
    let iso = fib_iso();
    let db = char_db();
    let net = fib_step_net();
    let input = fib_input();

    // t_ramp outside [1e-8, 1e-5] must error.
    let too_fast = rom_program_energy(iso, &input, db, 1e-9, net);
    assert!(
        too_fast.is_err(),
        "t_ramp=1e-9 (below range) must return error"
    );

    let too_slow = rom_program_energy(iso, &input, db, 1e-4, net);
    assert!(
        too_slow.is_err(),
        "t_ramp=1e-4 (above range) must return error"
    );
    eprintln!("me_a5: out-of-range t_ramp correctly rejected ✓");
}

// ── me_a6: method consistency ───────────────────────────────────────

#[test]
fn me_a6_method_consistency() {
    let db = char_db();
    let net = fib_step_net();
    let pe = fib_program_energy();

    // step_j from rom_program_energy must equal balanced_energy on the
    // same step net at the same t_ramp.
    let direct_bd = balanced_energy(net, db, T_RAMP).unwrap();
    let rel_err = ((pe.step_j - direct_bd.total_j) / direct_bd.total_j).abs();
    assert!(
        rel_err < 1e-15,
        "step_j from rom_program_energy ({:.6e}) != balanced_energy ({:.6e}), rel_err={:.2e}",
        pe.step_j,
        direct_bd.total_j,
        rel_err
    );
    eprintln!("me_a6: step_j={:.6e} == balanced_energy ✓", pe.step_j);
}
