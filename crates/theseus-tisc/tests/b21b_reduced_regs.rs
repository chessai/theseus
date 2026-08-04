#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! B-2.1b -- per-program register-count shrink.
//!
//! This slice makes `select_operand_l`'s crossbar topology `num_regs`-parametric
//! and plumbs a reduced `Layout` end to end (`step_with` + `encode_with`/
//! `decode_with` + `run_rom_on_cpu_with`). The tests here are the FAST
//! correctness authority for the reduced crossbar plus the register-shrink
//! measurement.
//!
//! (Fast-correctness authority -- the advisor runs the full heavy suite +
//! the byte-identity `poc_step_net_bit_identical` golden separately.)

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_netlist::{balance, lower_iso, CellKind};
use theseus_spice::chardb::{rung0_measured, CharDb};

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::{derive_layout, exec_instr, run_rom_on_cpu, run_rom_on_cpu_with};
use theseus_tisc::decoder::{operand_field_l, select_operand_l, R_RD};
use theseus_tisc::energy::balanced_energy;
use theseus_tisc::types::{Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

const T_RAMP: f64 = 1e-7;
const FUEL: u64 = 4_000_000_000;

fn char_db() -> &'static CharDb {
    static DB: OnceLock<CharDb> = OnceLock::new();
    DB.get_or_init(rung0_measured)
}

// ── The few-register measurement program ────────────────────────────
//
// A 3-instruction add/xor kernel over r0..r3 only. derive_layout must pick
// num_regs=4 (next_pow2(max_named_reg=3 + 1)). CPU-only (no Mexch), so it
// runs on the reduced CPU-only `step(rom)` ring.
fn few_reg_program() -> Vec<Instr> {
    vec![
        Instr::Add { rd: 0, rs: 1 }, // r0 += r1
        Instr::Xor { rd: 2, rs: 3 }, // r2 ^= r3
        Instr::Add { rd: 0, rs: 2 }, // r0 += r2
    ]
}

// ══════════════════════════════════════════════════════════════════════
// 1. derive_layout picks num_regs=4 for the few-register program
// ══════════════════════════════════════════════════════════════════════

#[test]
fn few_reg_program_derives_num_regs_4() {
    let prog = few_reg_program();
    let layout = derive_layout(&prog);
    assert_eq!(layout.num_regs, 4, "max named reg = 3 → next_pow2(4) = 4");
    assert_eq!(layout.mem_words, 0, "CPU-only program → mem_words 0");
    assert_eq!(layout.w_data, 8, "B-2.1b does not touch w_data");
    assert_eq!(layout.num_scratch, 8, "B-2.1b does not shrink scratch");
    eprintln!(
        "few-reg layout: num_regs={} num_scratch={} mem_words={} num_cpu_fields={}",
        layout.num_regs,
        layout.num_scratch,
        layout.mem_words,
        layout.num_cpu_fields()
    );
}

// ══════════════════════════════════════════════════════════════════════
// 2. Reduced crossbar routes registers correctly (whole reg domain)
// ══════════════════════════════════════════════════════════════════════
//
// The primary risk: at num_regs=4 the butterfly must route r0..r3 to the
// fixed slot base(0). We check the crossbar iso directly against a distinct
// per-register marker, for EVERY register index 0..num_regs, at num_regs=4.

fn distinct_state_reduced(num_regs: usize, operand_index: u32) -> StateVals {
    let mut regs = [0u8; NUM_REGS];
    for (i, r) in regs.iter_mut().enumerate().take(num_regs) {
        *r = 0x10 + i as u8;
    }
    StateVals {
        regs,
        pc: 0,
        br: 0,
        ir: operand_index << 20, // rd field = IR bits 20..24
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    }
}

fn eval_reduced(layout: &Layout, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = FUEL;
    let ev = theseus_eval::eval(iso, s.encode_with(layout), &mut fuel)
        .unwrap_or_else(|e| panic!("eval failed: {e:?}"));
    StateVals::decode_with(layout, &ev)
}

#[test]
fn reduced_crossbar_routes_registers() {
    let layout = Layout {
        num_regs: 4,
        ..Layout::default()
    };
    // The 1-of-N operand crossbar keyed on rd, at num_regs=4.
    let iso = select_operand_l(&layout, R_RD, |i| operand_field_l(&layout, i));

    for idx in 0..layout.num_regs as u32 {
        let s = distinct_state_reduced(layout.num_regs, idx);
        let got = eval_reduced(&layout, &iso, &s);
        let expected = 0x10 + idx as u8; // r[idx]'s marker
        assert_eq!(
            got.regs[0], expected,
            "reduced crossbar (num_regs=4): operand r{idx} must land at slot r0 \
             (expected {expected:#04x}, got {:#04x})",
            got.regs[0]
        );
        // Non-routed fields preserved.
        assert_eq!(got.ir, s.ir, "IR preserved for r{idx}");
        assert_eq!(got.br, s.br, "BR preserved for r{idx}");
    }
    eprintln!("reduced crossbar (num_regs=4): all r0..r3 route to slot r0 ✓");
}

#[test]
fn reduced_crossbar_roundtrip_identity() {
    // select ; unselect == identity for every register operand at num_regs=4.
    let layout = Layout {
        num_regs: 4,
        ..Layout::default()
    };
    let iso = select_operand_l(&layout, R_RD, |i| operand_field_l(&layout, i));
    let rt = Iso::seq(iso.clone(), Iso::sym(iso));
    for idx in 0..layout.num_regs as u32 {
        let s = distinct_state_reduced(layout.num_regs, idx);
        assert_eq!(
            eval_reduced(&layout, &rt, &s),
            s,
            "select ; unselect must be identity at reduced operand r{idx}"
        );
    }
    eprintln!("reduced crossbar (num_regs=4): select;unselect == id ✓");
}

// ══════════════════════════════════════════════════════════════════════
// 3. run_on_cpu == eval at the reduced layout (the real authority)
// ══════════════════════════════════════════════════════════════════════
//
// The few-register program run on the REDUCED (num_regs=4) CPU-only ring
// must agree with (a) the fast-sim reference `exec_instr` and (b) the
// DEFAULT-layout netlist run. This is the end-to-end proof that the reduced
// state netlist executes correctly.

fn exec_reference(prog: &[Instr], init: &StateVals) -> StateVals {
    let mut s = init.clone();
    for instr in prog {
        exec_instr(*instr, &mut s);
    }
    s
}

#[test]
fn reduced_run_matches_exec_and_default() {
    let prog = few_reg_program();
    let layout = derive_layout(&prog);
    assert_eq!(layout.num_regs, 4);

    // A few representative inputs, including edge bytes.
    let inputs = [
        &[0x01, 0x02, 0x04, 0x08][..],
        &[0xFF, 0x01, 0xAA, 0x55][..],
        &[0x00, 0x00, 0x00, 0x00][..],
        &[0x80, 0x7F, 0xFF, 0x01][..],
    ];

    let budget = prog.len(); // straight-line: B = ROM length
    for bytes in inputs {
        let init = StateVals::state_with_regs(bytes);

        let want = exec_reference(&prog, &init);

        let reduced =
            run_rom_on_cpu_with(&layout, &prog, init.clone(), budget, 0, theseus_eval::eval)
                .unwrap_or_else(|e| panic!("reduced run failed for {bytes:?}: {e}"));

        let default = run_rom_on_cpu(&prog, init.clone(), budget, 0, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("default run failed for {bytes:?}: {e}"));

        // Reduced netlist == fast-sim reference (on the named registers).
        assert_eq!(
            &reduced.regs[..layout.num_regs],
            &want.regs[..layout.num_regs],
            "reduced run_on_cpu != exec_instr for input {bytes:?}"
        );
        // Reduced netlist == default-layout netlist (full register agreement
        // on the named registers; dropped registers are un-named zeros).
        assert_eq!(
            &reduced.regs[..layout.num_regs],
            &default.regs[..layout.num_regs],
            "reduced run != default run for input {bytes:?}"
        );
        // Whole default-register agreement too (dropped regs stay 0).
        assert_eq!(
            reduced.regs, default.regs,
            "reduced run full-reg != default run for input {bytes:?}"
        );
        eprintln!(
            "input {bytes:?}: reduced==exec==default regs[0..4]={:?} ✓",
            &reduced.regs[..4]
        );
    }
}

// ══════════════════════════════════════════════════════════════════════
// 4. MEASUREMENT -- the register-shrink win (balanced cells + step_j)
// ══════════════════════════════════════════════════════════════════════

fn measure(layout: &Layout, rom: &[u32], label: &str) -> (usize, f64) {
    let step_iso = theseus_tisc::step::step_with(layout, rom);
    let net = lower_iso(&step_iso).expect("lower_iso step");
    let balanced = balance(&net).expect("balance");
    let cells: usize = balanced.cell_counts().values().copied().sum::<usize>();
    let k0 = balanced
        .cell_counts()
        .get(&CellKind::K0)
        .copied()
        .unwrap_or(0);
    let step_j = balanced_energy(&net, char_db(), T_RAMP)
        .expect("balanced_energy")
        .total_j;
    eprintln!(
        "{label}: num_regs={} balanced_cells={cells} (K0={k0}) step_j={step_j:.6e}",
        layout.num_regs
    );
    (cells, step_j)
}

#[test]
fn register_shrink_win_measurement() {
    let prog = few_reg_program();
    let rom = assemble(&prog).expect("assemble few-reg");
    let reduced = derive_layout(&prog);
    assert_eq!(reduced.num_regs, 4);

    let (cells8, j8) = measure(&Layout::default(), &rom, "default(num_regs=8)");
    let (cells4, j4) = measure(&reduced, &rom, "reduced(num_regs=4)");

    let cell_delta = cells8 as f64 - cells4 as f64;
    let j_delta = j8 - j4;
    eprintln!(
        "REGISTER-SHRINK WIN: cells {cells8} -> {cells4} (Δ={cell_delta}, {:.2}%), \
         step_j {j8:.6e} -> {j4:.6e} (Δ={j_delta:.6e}, {:.2}%)",
        100.0 * cell_delta / cells8 as f64,
        100.0 * j_delta / j8
    );

    // The reduced layout must be a strict win (fewer plumbed register fields
    // → fewer balanced cells, lower step_j). If it is not, the shrink is not
    // being applied and the measurement is meaningless.
    assert!(
        cells4 < cells8,
        "reduced num_regs must shrink the step net (got {cells4} >= {cells8})"
    );
    assert!(
        j4 < j8,
        "reduced num_regs must lower step_j (got {j4:.6e} >= {j8:.6e})"
    );
}

/// B-2.1b correctness (advisor): `derive_layout` must NOT shrink `num_regs`
/// when the program names a SCRATCH operand. Scratch operand indices are
/// absolute (`NUM_REGS + s`) and decode via `s[idx − num_regs]`, so a reduced
/// `num_regs` would misroute them (idx 10 = s2 at num_regs=8 → s6 at num_regs=4
/// = silent corruption). A program using only r0..r3 but ALSO scratch s2 must
/// therefore keep `num_regs = 8`, forgoing the register-shrink win rather than
/// misroute. (Reproduces the latent hazard: without the fix, derive_layout
/// ignores the scratch operand and returns num_regs=4.)
#[test]
fn derive_layout_pins_num_regs_when_scratch_operand_used() {
    // Register-only r0..r3 → shrinks to num_regs = 4 (the win applies).
    let reg_only = vec![Instr::Add { rd: 0, rs: 1 }, Instr::Xor { rd: 2, rs: 3 }];
    assert_eq!(
        derive_layout(&reg_only).num_regs,
        4,
        "register-only r0..r3 must shrink to num_regs=4"
    );

    // Same registers, but ALSO a scratch operand s2 (Ccnot rt = idx 10) →
    // num_regs must stay 8 (else idx 10 misroutes to s6).
    let with_scratch = vec![
        Instr::Add { rd: 0, rs: 1 },
        Instr::Xor { rd: 2, rs: 3 },
        Instr::Ccnot {
            rd: 0,
            rs: 1,
            rt: 10,
        },
    ];
    assert_eq!(
        derive_layout(&with_scratch).num_regs,
        8,
        "scratch operand (idx 10=s2) present → num_regs must stay 8, not misroute"
    );
}

/// B-2.1b review F2 (regression): the s0/s1 immovability invariant -- NO operand
/// crossbar swap may touch the reserved operand-buffer s0 / config word s1 (the
/// latter holds B-1's config one-hots) -- must hold at EVERY `num_regs`. At
/// `num_regs=2` the OLD split-level-only exclusion let a DESCENT level (operand
/// bit 2, stride 4) fire swaps `(pos2,pos6)`/`(pos3,pos7)` = s0↔s4 / s1↔s5,
/// displacing s0/s1 mid-instruction. The position-based exclusion skips any swap
/// touching positions `num_regs`/`num_regs+1` at every level. Assert: routing
/// ANY operand selector at num_regs=2 leaves s0/s1 UNTOUCHED. (Fails on the old
/// code for any selector with bit 2 set, e.g. 4..8.)
#[test]
fn reduced_crossbar_preserves_s0_s1_at_num_regs_2() {
    let layout = Layout {
        num_regs: 2,
        ..Layout::default()
    };
    let iso = select_operand_l(&layout, R_RD, |i| operand_field_l(&layout, i));
    for sel_idx in 0u32..16 {
        let mut s = distinct_state_reduced(layout.num_regs, sel_idx);
        s.scratch[0] = 0xC0; // s0 sentinel (reserved operand buffer)
        s.scratch[1] = 0xC1; // s1 sentinel (reserved config/predicate word)
        let got = eval_reduced(&layout, &iso, &s);
        assert_eq!(
            got.scratch[0], 0xC0,
            "num_regs=2, selector {sel_idx}: s0 displaced -- F2 invariant violated"
        );
        assert_eq!(
            got.scratch[1], 0xC1,
            "num_regs=2, selector {sel_idx}: s1 (config word) corrupted -- F2 invariant violated"
        );
    }
    eprintln!("num_regs=2: s0/s1 immovable across all operand selectors ✓");
}

/// B-2.1b review F2 (correctness at the newly-reachable width): the position-
/// based exclusion keeps `num_regs ∈ {1,2}` reachable, so verify a 2-register
/// program actually RUNS correctly on the reduced-layout netlist (reduced ==
/// default == exec), not just that the crossbar preserves s0/s1.
#[test]
fn num_regs_2_program_runs_correctly() {
    let prog = vec![Instr::Add { rd: 0, rs: 1 }, Instr::Xor { rd: 1, rs: 0 }];
    let layout = derive_layout(&prog);
    assert_eq!(layout.num_regs, 2, "r0,r1-only program → num_regs=2");
    let budget = prog.len();
    for bytes in [
        &[0x12u8, 0x34][..],
        &[0xFF, 0x0F][..],
        &[0x00, 0x00][..],
        &[0x80, 0x7F][..],
    ] {
        let mut regs = [0u8; NUM_REGS];
        regs[0] = bytes[0];
        regs[1] = bytes[1];
        let init = StateVals {
            regs,
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0; NUM_SCRATCH],
            mem: [0; MEM_WORDS],
        };
        let want = exec_reference(&prog, &init);
        let reduced =
            run_rom_on_cpu_with(&layout, &prog, init.clone(), budget, 0, theseus_eval::eval)
                .unwrap_or_else(|e| panic!("num_regs=2 reduced run failed for {bytes:?}: {e}"));
        let default = run_rom_on_cpu(&prog, init.clone(), budget, 0, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("default run failed for {bytes:?}: {e}"));
        assert_eq!(
            &reduced.regs[..2],
            &want.regs[..2],
            "num_regs=2 reduced run != exec for {bytes:?}"
        );
        assert_eq!(
            reduced.regs, default.regs,
            "num_regs=2 reduced run != default run for {bytes:?}"
        );
    }
    eprintln!("num_regs=2 program: reduced == default == exec ✓");
}
