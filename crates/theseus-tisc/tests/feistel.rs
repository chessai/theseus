//! M4-v1 ENERGY-WORKLOAD HEADLINE (ADR-0030): the TISC-1 CPU runs a
//! Feistel cipher round forward AND backward, and reports its energy.
//!
//! A Feistel network is the canonical *intrinsically reversible* cipher
//! structure: one round maps (L, R) -> (R, L ^ F(R)) and is invertible
//! for ANY round function F, because given (R, L') the input is
//! recovered as L = L' ^ F(R). On a reversible machine this means the
//! SAME hardware encrypts running forward and decrypts running the
//! adjoint -- no separate decryption datapath.
//!
//! Round function here: F(R) = ROTL(R, C) ^ key (key held in r3).
//!
//! Register map: r1 = L, r2 = R, r3 = key, r4 = t (temp, starts 0).
//! One round is 8 straight-line TISC-1 instructions, garbage-free
//! (t is uncomputed back to 0 before the half-swap):
//!
//!   XOR   r4, r2    ; t = R                 (t was 0)
//!   ROTLI r4, C     ; t = ROTL(R, C)
//!   XOR   r4, r3    ; t = ROTL(R, C) ^ key = F(R)
//!   XOR   r1, r4    ; L ^= F(R)
//!   XOR   r4, r3    ; t = ROTL(R, C)        (uncompute F ...)
//!   ROTRI r4, C     ; t = R
//!   XOR   r4, r2    ; t = 0                 (... t clean again)
//!   EXCH  r1, r2    ; swap halves: (L, R) <- (R, L ^ F(R))
//!
//! No aliased operands anywhere; R and key are read-only within a
//! round; ROUNDS rounds are unrolled in the ROM (T = 8 * ROUNDS cycles).
//!
//! Verified here (run harness mirrors tests/fib_cpu.rs -- eval-only,
//! no lowering in the run):
//!   1. FORWARD: after every round the registers match a Rust reference
//!      Feistel round with the same F; temp = 0, IR = 0 (P4), scratch
//!      zero, key intact, PC tracks the cycle count.
//!   2. BACKWARD: T forward cycles then T adjoint cycles
//!      (`Iso::sym(step(rom))`) return EXACTLY to the input block + key
//!      + PC -- encryption forward, decryption backward, same hardware.
//!   3. ENERGY: per-step and whole-round energy of the Feistel workload
//!      from the REAL rung-0 char DB via the analytic balanced-energy
//!      model (lower_iso only, buffers counted not materialized), plus
//!      the per-clause cost of the round's own ops (XOR / ROTLI / ROTRI
//!      / EXCH -- the XOR/ROT-heavy mix is the designated energy
//!      workload).

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_netlist::{lower_iso, PhasedCellNetlist};
use theseus_spice::chardb::{rung0_measured, CharDb};
use theseus_tisc::asm::{assemble, decode, Instr};
use theseus_tisc::energy::{balanced_energy, instruction_energy};
use theseus_tisc::step::step;
use theseus_tisc::types::{StateVals, MEM_WORDS};

mod common;
use common::eval_state;

/// Generous per-eval fuel (same order as fib_cpu.rs).
const EVAL_FUEL: u64 = 8_000_000_000;

/// Feistel rounds unrolled in the ROM.
const ROUNDS: usize = 2;

/// Instructions per round (see module docs).
const ROUND_LEN: usize = 8;

/// Rotation constant in F(R) = ROTL(R, C) ^ key.
const ROT_C: u8 = 3;

/// Operating ramp time for the energy report, seconds.
const T_RAMP: f64 = 1e-7;

// ── Run harness (mirrors tests/fib_cpu.rs) ─────────────────────────

/// Apply `step(rom)` `t` times: run the machine `t` cycles forward.
fn run(rom: &[u32], start: &StateVals, t: usize) -> StateVals {
    let iso = step(rom);
    let mut cur = start.clone();
    for k in 0..t {
        cur = eval_state(&format!("run/fwd[{k}]"), &iso, &cur, EVAL_FUEL);
    }
    cur
}

/// Run `t` cycles forward, then `t` cycles of the adjoint
/// `Iso::sym(step(rom))`. Returns (forward endpoint, state after the
/// backward run); the latter must equal `start` (reversibility).
fn run_and_reverse(rom: &[u32], start: &StateVals, t: usize) -> (StateVals, StateVals) {
    let iso = step(rom);
    let back = Iso::sym(iso.clone());
    let mut cur = start.clone();
    for k in 0..t {
        cur = eval_state(&format!("rr/fwd[{k}]"), &iso, &cur, EVAL_FUEL);
    }
    let fwd = cur.clone();
    for k in (0..t).rev() {
        cur = eval_state(&format!("rr/back[{k}]"), &back, &cur, EVAL_FUEL);
    }
    (fwd, cur)
}

// ── The Feistel program ────────────────────────────────────────────

/// One garbage-free Feistel round (see module docs). r1 = L, r2 = R,
/// r3 = key, r4 = t (must be 0 on entry; is 0 again on exit).
fn feistel_round_instrs() -> [Instr; ROUND_LEN] {
    [
        Instr::Xor { rd: 4, rs: 2 },        // t = R
        Instr::Rotli { rd: 4, imm: ROT_C }, // t = ROTL(R, C)
        Instr::Xor { rd: 4, rs: 3 },        // t = F(R)
        Instr::Xor { rd: 1, rs: 4 },        // L ^= F(R)
        Instr::Xor { rd: 4, rs: 3 },        // t = ROTL(R, C)   (uncompute)
        Instr::Rotri { rd: 4, imm: ROT_C }, // t = R
        Instr::Xor { rd: 4, rs: 2 },        // t = 0
        Instr::Exch { rd: 1, rs: 2 },       // (L, R) <- (R, L ^ F(R))
    ]
}

/// ROUNDS Feistel rounds unrolled (T = ROUND_LEN * ROUNDS cycles).
fn feistel_prog() -> Vec<Instr> {
    let mut prog = Vec::with_capacity(ROUND_LEN * ROUNDS);
    for _ in 0..ROUNDS {
        prog.extend_from_slice(&feistel_round_instrs());
    }
    prog
}

/// Machine state holding the plaintext block (L, R) and the key, with
/// the temp register and everything else zero.
fn block_state(l: u8, r: u8, key: u8) -> StateVals {
    let mut s = StateVals {
        regs: [0; 8],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    s.regs[1] = l;
    s.regs[2] = r;
    s.regs[3] = key;
    s
}

// ── Rust reference (same F, same round) ────────────────────────────

/// Round function: F(R) = ROTL(R, C) ^ key.
fn f_ref(r: u8, key: u8) -> u8 {
    r.rotate_left(u32::from(ROT_C)) ^ key
}

/// One reference Feistel round: (L, R) -> (R, L ^ F(R)).
fn round_ref(l: u8, r: u8, key: u8) -> (u8, u8) {
    (r, l ^ f_ref(r, key))
}

/// Inverse reference round: (L', R') -> (R' ^ F(L'), L'). Sanity that
/// the STRUCTURE is invertible independent of the machine.
fn round_ref_inv(lp: u8, rp: u8, key: u8) -> (u8, u8) {
    (rp ^ f_ref(lp, key), lp)
}

/// Sample blocks: (L, R, key).
const SAMPLES: [(u8, u8, u8); 3] = [(0xAB, 0xCD, 0x5E), (0x00, 0x00, 0x00), (0xFF, 0x01, 0xA7)];

// ══════════════════════════════════════════════════════════════════════
// 1. FORWARD: encrypt on the CPU, check every round against the reference
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "M-C2 L1: slow at wider State (resident-memory area cost); green pre-merge via --ignored"]
fn feistel_forward() {
    let prog = feistel_prog();
    let rom = assemble(&prog).expect("valid program");
    assert_eq!(rom.len(), ROUND_LEN * ROUNDS);
    let iso = step(&rom);

    eprintln!(
        "== TISC-1 Feistel ROM ({} words = {ROUNDS} rounds x {ROUND_LEN}; \
         F(R) = ROTL(R, {ROT_C}) ^ key) ==",
        rom.len()
    );
    for (p, &w) in rom.iter().enumerate() {
        eprintln!("  {p:3}: {w:#06X}  {:?}", decode(w).unwrap());
    }

    for &(l0, r0, key) in &SAMPLES {
        let mut cur = block_state(l0, r0, key);
        let (mut lr, mut rr) = (l0, r0);
        eprintln!("CPU Feistel run: (L, R, key) = ({l0:#04X}, {r0:#04X}, {key:#04X})");
        for round in 0..ROUNDS {
            for c in 0..ROUND_LEN {
                cur = eval_state(
                    &format!("feistel/round{round}/cycle{c}"),
                    &iso,
                    &cur,
                    EVAL_FUEL,
                );
            }
            (lr, rr) = round_ref(lr, rr, key);
            eprintln!(
                "  after round {}: (r1, r2) = ({:#04X}, {:#04X})  [ref ({lr:#04X}, {rr:#04X})]",
                round + 1,
                cur.regs[1],
                cur.regs[2]
            );
            assert_eq!(
                (cur.regs[1], cur.regs[2]),
                (lr, rr),
                "round {round}: CPU diverged from the reference Feistel round"
            );
            assert_eq!(cur.regs[3], key, "key register must be untouched");
            assert_eq!(
                cur.regs[4], 0,
                "temp r4 must be uncomputed back to 0 (garbage-free)"
            );
            assert_eq!(
                cur.pc,
                (ROUND_LEN * (round + 1)) as u16,
                "PC must track the cycle count"
            );
            assert_eq!(cur.ir, 0, "P4: IR must be zero between steps");
            assert_eq!(cur.scratch, [0; 8], "scratch must stay zero");
        }
        // Untouched registers stay zero.
        for i in [0usize, 5, 6, 7] {
            assert_eq!(cur.regs[i], 0, "r{i} must be untouched");
        }
        // Reference structure sanity: the inverse round undoes the round.
        let (l1, r1) = round_ref(l0, r0, key);
        assert_eq!(round_ref_inv(l1, r1, key), (l0, r0));
    }
}

// ══════════════════════════════════════════════════════════════════════
// 2. BACKWARD: decrypt = the adjoint of the same hardware
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "M-C2 L1: slow at wider State (resident-memory area cost); green pre-merge via --ignored"]
fn feistel_forward_and_backward() {
    let rom = assemble(&feistel_prog()).expect("valid program");
    let (l0, r0, key) = SAMPLES[0];
    let start = block_state(l0, r0, key);
    let t = ROUND_LEN * ROUNDS;

    let (fwd, back) = run_and_reverse(&rom, &start, t);

    // Forward endpoint = the reference ciphertext after ROUNDS rounds.
    let (mut lr, mut rr) = (l0, r0);
    for _ in 0..ROUNDS {
        (lr, rr) = round_ref(lr, rr, key);
    }
    assert_eq!((fwd.regs[1], fwd.regs[2]), (lr, rr), "ciphertext mismatch");
    assert_ne!(
        (fwd.regs[1], fwd.regs[2]),
        (l0, r0),
        "cipher must change the block"
    );
    assert_eq!(fwd.regs[3], key);
    assert_eq!(fwd.regs[4], 0, "no garbage left in the temp register");
    assert_eq!(fwd.pc, t as u16);
    assert_eq!(fwd.ir, 0);
    assert_eq!(fwd.scratch, [0; 8]);

    // Backward: T adjoint cycles recover the EXACT plaintext state.
    assert_eq!(
        back, start,
        "decryption (adjoint run) must restore the exact plaintext block + key + PC"
    );
    eprintln!(
        "FEISTEL REVERSIBILITY CONFIRMED: {t} cycles forward encrypt \
         ({l0:#04X}, {r0:#04X}) -> ({lr:#04X}, {rr:#04X}) under key {key:#04X}; \
         {t} adjoint cycles decrypt back to the exact plaintext state \
         (same hardware, run backward)"
    );

    // run() agrees with the forward leg of run_and_reverse().
    assert_eq!(run(&rom, &start, t), fwd);
}

// ══════════════════════════════════════════════════════════════════════
// 3. ENERGY: the Feistel round as the ADR-0030 energy workload
// ══════════════════════════════════════════════════════════════════════

fn char_db() -> &'static CharDb {
    static DB: OnceLock<CharDb> = OnceLock::new();
    DB.get_or_init(rung0_measured)
}

/// Memoized unbalanced step netlist for the Feistel ROM (lowered ONCE;
/// never balanced -- buffers are counted analytically).
fn feistel_step_net() -> &'static PhasedCellNetlist {
    static NET: OnceLock<PhasedCellNetlist> = OnceLock::new();
    NET.get_or_init(|| {
        let rom = assemble(&feistel_prog()).expect("valid program");
        lower_iso(&step(&rom)).expect("lower_iso step(feistel rom)")
    })
}

#[test]
#[ignore = "M-C2 L1: slow at wider State (resident-memory area cost); green pre-merge via --ignored"]
fn feistel_energy() {
    let db = char_db();
    let t = ROUND_LEN * ROUNDS;

    // Per-clause: the round's own ops at the registers the ROM uses.
    // (These are the instruction isos in isolation; in the machine each
    // executes inside the shared fetch/decode_execute/unfetch step.)
    let round_ops: Vec<(&str, Iso)> = vec![
        ("XOR r4,r2 (t=R / t=0)", theseus_tisc::instr::xor(4, 2)),
        ("ROTLI r4,#3 (rotate)", theseus_tisc::instr::rotli(4, ROT_C)),
        ("XOR r4,r3 (key mix)", theseus_tisc::instr::xor(4, 3)),
        ("XOR r1,r4 (L^=F(R))", theseus_tisc::instr::xor(1, 4)),
        ("ROTRI r4,#3 (unrot)", theseus_tisc::instr::rotri(4, ROT_C)),
        ("EXCH r1,r2 (swap)", theseus_tisc::instr::exch(1, 2)),
    ];
    eprintln!("== Feistel round op energies (instruction isos, analytic buffers) ==");
    let mut xor_seen_positive = false;
    for (label, iso) in &round_ops {
        let bd = instruction_energy(iso, db, T_RAMP).unwrap();
        // XOR burns real energy; ROTLI/ROTRI (rotate by an immediate)
        // and EXCH lower to pure wire permutations -- zero active cells,
        // zero energy. That asymmetry is itself a workload finding.
        assert!(bd.total_j >= 0.0, "{label}: energy must be non-negative");
        if label.starts_with("XOR") {
            assert!(bd.total_j > 0.0, "{label}: XOR must burn energy");
            xor_seen_positive = true;
        }
        let note = if bd.total_j == 0.0 {
            "  [free: pure wiring]"
        } else {
            ""
        };
        eprintln!(
            "  {label:<24} total {:.4e} J (active {:.4e} J + {} K0 buffers {:.4e} J){note}",
            bd.total_j, bd.active_j, bd.buffer_count, bd.buffer_j
        );
    }
    assert!(xor_seen_positive);

    // Per-step and whole-round energy of the actual machine running the
    // Feistel ROM (fetch + decode_execute + unfetch + pc_inc each cycle).
    let step_bd = balanced_energy(feistel_step_net(), db, T_RAMP).unwrap();
    assert!(step_bd.total_j > 0.0, "step energy must be positive");
    assert!(
        step_bd.buffer_count > 0,
        "the step has deep skew; buffers expected"
    );
    let round_j = step_bd.total_j * ROUND_LEN as f64;
    let program_j = step_bd.total_j * t as f64;

    eprintln!("\n== Feistel step energy (real rung-0 char DB, t_ramp = {T_RAMP:.1e} s) ==");
    eprintln!("{step_bd}");
    eprintln!(
        "\nFEISTEL WORKLOAD ENERGY: per machine cycle {:.4e} J; \
         one round ({ROUND_LEN} cycles) {round_j:.4e} J; \
         full program ({ROUNDS} rounds, {t} cycles) {program_j:.4e} J",
        step_bd.total_j
    );

    // total = active + buffer, exactly.
    let diff = (step_bd.total_j - (step_bd.active_j + step_bd.buffer_j)).abs();
    assert!(diff < 1e-30, "total = active + buffer");
}
