//! B-2.0b — per-program minimal-layout derivation + MEM-drop + reduced-reg
//! non-default-layout probe.
//!
//! FAST tier (no heavy lowering): derivation unit tests, CPU-only-routing
//! `run_on_cpu == eval` correctness on a small program, `run_on_cpu_fast ==
//! eval` regression, and the FIRST exercise of a non-default `Layout` (a
//! reduced `num_regs` / `mem_words = 0` `state_ty_with` lowering) — the
//! probe that surfaces any hardcoded-8 in 0a's parameterization.
//!
//! The heavy MEM-drop energy delta (`step` vs `system_step` balanced
//! energy) is measured out-of-band by the advisor's authority run; here we
//! prove the derivation + routing are correct and that the reduced layout
//! actually lowers.

use theseus_core::{enumerate_values, type_of, Iso, Value};
use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{compile, derive_layout, is_cpu_only, run_on_cpu, run_on_cpu_fast};
use theseus_tisc::types::{state_ty_with, Layout};

// ── Program elaboration (same pattern as compile.rs test) ────────────

const CNOT_SRC: &str = include_str!("../../../conformance/programs/0004-cnot.thse");

fn elaborate(src: &str, iso_name: &str) -> Iso {
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

// ── 1. Derivation unit tests ─────────────────────────────────────────

#[test]
fn derive_cpu_only_drops_mem() {
    // A pure-register program: mem_words must collapse to 0.
    let prog = vec![
        Instr::Xor { rd: 0, rs: 1 },
        Instr::Addi { rd: 2, imm: 5 },
    ];
    assert!(is_cpu_only(&prog));
    let l = derive_layout(&prog);
    assert_eq!(l.mem_words, 0, "CPU-only program must derive mem_words = 0");
    // max named reg = 2 → next_pow2(3) = 4.
    assert_eq!(l.num_regs, 4, "max named reg 2 → num_regs 4");
    // Unchanged axes (B-2.1/B-2.2 own these).
    assert_eq!(l.w_data, 8);
    assert_eq!(l.w_ctl, 8);
    assert_eq!(l.w_pc, 16);
    assert_eq!(l.num_scratch, 8);
}

#[test]
fn derive_mem_program_keeps_bank() {
    let prog = vec![Instr::Xor { rd: 0, rs: 1 }, Instr::Mexch { rd: 0, addr: 0 }];
    assert!(!is_cpu_only(&prog));
    let l = derive_layout(&prog);
    assert_eq!(l.mem_words, Layout::default().mem_words, "mem program keeps bank");
}

#[test]
fn derive_reg_rounding_and_clamp() {
    // 0 regs named (empty) → num_regs 1.
    assert_eq!(derive_layout(&[]).num_regs, 1);
    // max reg 0 → next_pow2(1) = 1.
    assert_eq!(
        derive_layout(&[Instr::Addi { rd: 0, imm: 1 }]).num_regs,
        1
    );
    // max reg 3 → next_pow2(4) = 4.
    assert_eq!(
        derive_layout(&[Instr::Add { rd: 3, rs: 0 }]).num_regs,
        4
    );
    // max reg 4 → next_pow2(5) = 8 (clamped at 8).
    assert_eq!(
        derive_layout(&[Instr::Add { rd: 4, rs: 0 }]).num_regs,
        8
    );
    // max reg 7 → 8.
    assert_eq!(
        derive_layout(&[Instr::Add { rd: 7, rs: 0 }]).num_regs,
        8
    );
    // Scratch operand (idx 10 = s2) is NOT a register — excluded from the
    // reg bound (num_scratch is not shrunk this slice).
    assert_eq!(
        derive_layout(&[Instr::Add { rd: 0, rs: 10 }]).num_regs,
        1
    );
}

// ── 2. FIRST non-default-layout lowering probe (0a completeness) ─────
//
// This is the KEY risk check: build `state_ty_with` at a REDUCED layout
// (num_regs = 4, mem_words = 0) and lower it. If 0a left a hardcoded 8
// anywhere in the *type* construction, the reduced type is malformed and
// this surfaces it deterministically (no heavy step lowering needed).

#[test]
fn reduced_layout_state_ty_is_well_formed() {
    use theseus_netlist::bits_of;
    use theseus_core::enumerate::cardinality_u64;

    let reduced = Layout {
        num_regs: 4,
        mem_words: 0,
        ..Layout::default()
    };
    let ty = state_ty_with(&reduced);

    // Field count: 4 regs + PC_lo + BR + 4 IR + 8 scratch + PC_hi = 19.
    let expected_fields = reduced.num_cpu_fields();
    assert_eq!(expected_fields, 4 + 1 + 1 + 4 + 8 + 1);

    // The type must have exactly num_fields * w_data leaves (bits). We
    // round-trip a zero value through bits_of to confirm the type is a
    // well-formed right-nested product of the right width.
    let card = cardinality_u64(&ty);
    // cardinality = 2^(state_bits) — but that overflows u64 for 152 bits, so
    // instead check the bit-length via a concrete zero inhabitant.
    let _ = card; // (cardinality is astronomically large; not asserted)
    let zero = theseus_netlist::value_from_bits(&ty, &vec![false; reduced.state_bits()])
        .expect("reduced state_ty_with must accept a zero inhabitant of state_bits length");
    let bits = bits_of(&zero, &ty).expect("bits_of reduced state");
    assert_eq!(
        bits.len(),
        reduced.state_bits(),
        "reduced state_ty_with bit-length must equal num_fields*w_data"
    );
    assert_eq!(reduced.state_bits(), 19 * 8);
}

// ── 3. MEM-drop routing correctness (run_on_cpu == eval) ─────────────
//
// cnot is a 2-register, no-memory program → is_cpu_only. After B-2.0b
// `run_on_cpu` routes it through the CPU-only `step(rom)` ring (dropping
// mem/bus). Assert the routed run still equals eval over the whole domain.

fn cpu_run(iso: &Iso, input: &Value) -> Result<Value, theseus_tisc::compile::CompileError> {
    run_on_cpu(iso, input, theseus_eval::eval)
}

#[test]
fn cnot_is_cpu_only_and_run_on_cpu_matches_eval() {
    let iso = elaborate(CNOT_SRC, "cnot");
    let instrs = compile(&iso).expect("compile cnot");
    assert!(
        is_cpu_only(&instrs),
        "cnot must be CPU-only (no Mexch); instrs = {instrs:?}"
    );
    let l = derive_layout(&instrs);
    assert_eq!(l.mem_words, 0, "cnot derives mem_words = 0");

    let (in_ty, _out_ty) = type_of(&iso).expect("type_of cnot");
    for v in enumerate_values(&in_ty) {
        let mut fuel = u64::MAX;
        let want = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        let got = cpu_run(&iso, &v).expect("run_on_cpu (CPU-only path)");
        assert_eq!(
            got, want,
            "run_on_cpu (MEM-dropped step path) != eval on input {v:?}"
        );
    }
}

#[test]
fn cnot_run_on_cpu_fast_matches_eval() {
    let iso = elaborate(CNOT_SRC, "cnot");
    let (in_ty, _out_ty) = type_of(&iso).expect("type_of cnot");
    for v in enumerate_values(&in_ty) {
        let mut fuel = u64::MAX;
        let want = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        let got = run_on_cpu_fast(&iso, &v).expect("run_on_cpu_fast");
        assert_eq!(got, want, "run_on_cpu_fast != eval on input {v:?}");
    }
}

// ── 4. MEM-drop energy measurement (ignored: heavy lowering) ─────────
//
// The B-2.0b win, measured as balanced cell count (step_j ∝ balanced cell
// inventory, design §framing). For the SAME CPU-only ROM, lower the CPU-only
// `step(rom)` ring vs the composite `system_step(rom)` (which additionally
// clocks the MemState + BusMsg emit/mem/absorb machinery). The delta is what
// dropping mem/bus for a CPU-only program saves per step. Run explicitly:
//   cargo test -p theseus-tisc --test b2_layout -- --ignored --nocapture measure
#[test]
#[ignore = "heavy: lowers full step + system_step nets (minutes, GBs)"]
fn measure_mem_drop_cell_delta() {
    use theseus_netlist::{balance, lower_iso};

    // A tiny CPU-only ROM (register XOR + a reserved no-op), same shape as the
    // energy-test rom: no Mexch, so is_cpu_only.
    let rom: Vec<u32> = {
        let iso = elaborate(CNOT_SRC, "cnot");
        let instrs = compile(&iso).expect("compile cnot");
        assert!(is_cpu_only(&instrs));
        theseus_tisc::asm::assemble(&instrs).expect("assemble")
    };

    let step_net = lower_iso(&theseus_tisc::step::step(&rom)).expect("lower step");
    let sys_net = lower_iso(&theseus_tisc::step::system_step(&rom)).expect("lower system_step");

    let step_cells = step_net.cells.len();
    let sys_cells = sys_net.cells.len();

    // Balanced counts (the actual energy driver): cells + K0 balance buffers.
    let step_bal = balance(&step_net).map(|b| b.cells.len()).unwrap_or(step_cells);
    let sys_bal = balance(&sys_net).map(|b| b.cells.len()).unwrap_or(sys_cells);

    eprintln!(
        "MEM-DROP MEASUREMENT (cnot ROM, CPU-only):\n  \
         step(rom)        unbalanced cells = {step_cells}, balanced = {step_bal}\n  \
         system_step(rom) unbalanced cells = {sys_cells}, balanced = {sys_bal}\n  \
         mem-drop saving  = {} unbalanced ({:.1}%), {} balanced ({:.1}%)",
        sys_cells - step_cells,
        100.0 * (sys_cells - step_cells) as f64 / sys_cells as f64,
        sys_bal.saturating_sub(step_bal),
        100.0 * (sys_bal.saturating_sub(step_bal)) as f64 / sys_bal.max(1) as f64,
    );
    assert!(step_cells < sys_cells, "CPU-only step must be smaller than system_step");
}
