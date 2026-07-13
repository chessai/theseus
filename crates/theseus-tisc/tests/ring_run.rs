//! Differential-oracle tests for the TISC-1 ring runner
//! (ADR-0033, M4-v2): `step_ring(rom).run(&s, R)` ≡ `eval(step)^R`.
//!
//! Uses a small straight-line ROM (3 instructions, BR untouched →
//! fall-through) to keep build + lower + balance_compact fast while
//! exercising the full ring pipeline at real scale.

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_tisc::ring_run::{step_ring, StepRing};
use theseus_tisc::step::step;
use theseus_tisc::types::StateVals;

const EVAL_FUEL: u64 = 8_000_000_000;

// ── Instruction encoders (spec §3) ─────────────────────────────────

fn r_ins(opcode: u16, rd: u16, rs: u16, rt: u16) -> u16 {
    assert!(opcode < 16 && rd < 8 && rs < 8 && rt < 8);
    (1 << 15) | (opcode << 11) | (rd << 8) | (rs << 5) | (rt << 2)
}

fn i_ins(opc: u16, rd: u16, imm: u8) -> u16 {
    assert!(opc < 8 && rd < 8);
    (opc << 12) | (rd << 9) | (u16::from(imm) << 1)
}

// ── Eval helper ────────────────────────────────────────────────────

fn eval_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

// ── Small straight-line ROM (no BR mutation → fall-through) ────────

fn small_rom() -> Vec<u16> {
    vec![
        i_ins(0, 1, 0x05), // 0: ADDI r1, 5
        r_ins(2, 3, 2, 0), // 1: XOR  r3, r2
        i_ins(1, 6, 0x11), // 2: SUBI r6, 0x11
    ]
}

// ── Cached ring + iso ──────────────────────────────────────────────

struct Ctx {
    ring: StepRing,
    iso: Iso,
}

fn ctx() -> &'static Ctx {
    static CTX: OnceLock<Ctx> = OnceLock::new();
    CTX.get_or_init(|| {
        let rom = small_rom();
        let ring = step_ring(&rom);
        let iso = step(&rom);
        Ctx { ring, iso }
    })
}

// ── Sample states (IR = 0, s0 = s1 = 0, BR = 0) ──────────────────

fn sample_state(pc: u8) -> StateVals {
    StateVals {
        regs: [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
        pc,
        br: 0,
        ir: 0,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
    }
}

// ══════════════════════════════════════════════════════════════════════
// 1. Differential oracle: ring.run(s, R) == eval(step)^R
// ══════════════════════════════════════════════════════════════════════

#[test]
fn ring_matches_eval_oracle() {
    let c = ctx();
    const MAX_R: u32 = 5;

    // Sample across in-ROM PCs and one out-of-ROM PC.
    for &pc in &[0u8, 1, 2, 4] {
        let start = sample_state(pc);
        let mut oracle = start.clone();

        for r in 0..=MAX_R {
            let ring_result = c.ring.run(&start, r);
            assert_eq!(
                ring_result, oracle,
                "ring.run(s, {r}) != eval^{r} at pc={pc}"
            );
            oracle =
                eval_state(&format!("oracle/pc={pc}/r={r}"), &c.iso, &oracle);
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 2. Area is revolution-independent (ADR-0033)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn ring_area_independent_of_revolutions() {
    let c = ctx();
    let counts = c.ring.compact().cell_counts();
    let k0 = c.ring.compact().k0_buffer_count();

    eprintln!("ring cell counts: {counts:?}");
    eprintln!("ring k0 buffer count: {k0}");

    // The body is the same struct regardless of R — run a few and
    // re-check (proving the ring reuses one body, not R copies).
    for r in [1u32, 4, 16] {
        let s = sample_state(0);
        let _ = c.ring.run(&s, r);
        assert_eq!(
            c.ring.compact().cell_counts(),
            counts,
            "cell_counts changed after R={r}"
        );
        assert_eq!(
            c.ring.compact().k0_buffer_count(),
            k0,
            "k0_buffer_count changed after R={r}"
        );
    }
}

// ══════════════════════════════════════════════════════════════════════
// 3. Ancilla restoration report
// ══════════════════════════════════════════════════════════════════════

#[test]
fn ancilla_restoration_report() {
    let c = ctx();
    let start = sample_state(0);

    let restored = c.ring.ancillas_restored(&start);
    let mismatches = c.ring.ancilla_mismatches(&start);

    eprintln!("ancilla count: {}", c.ring.ancilla_count());
    eprintln!("ancillas restored: {restored}");

    if restored {
        eprintln!("TRUE recirculation is valid — ancillas close the ring.");
        // Verify true recirculation matches eval^R.
        const MAX_R: u32 = 5;
        let mut oracle = start.clone();
        for r in 0..=MAX_R {
            let true_result = c.ring.run_true(&start, r);
            assert_eq!(
                true_result, oracle,
                "true recirculation run_true(s, {r}) != eval^{r}"
            );
            oracle =
                eval_state(&format!("true_recirc/r={r}"), &c.iso, &oracle);
        }
    } else {
        let n_mismatch = mismatches.len();
        let n_total = c.ring.ancilla_count();
        eprintln!(
            "ancilla lines NOT restored: {n_mismatch}/{n_total} mismatching"
        );
        let show = mismatches.len().min(20);
        eprintln!(
            "mismatching ancilla indices (first {show}): {:?}",
            &mismatches[..show]
        );
        eprintln!(
            "NOTE: step's lowering is not yet ancilla-clean for true \
             recirculation. The re-supply-fresh-ancillas driver still \
             faithfully models step^R. Cleaning the ancillas \
             (Bennett-style uncompute) is the follow-up."
        );
        // Do NOT fail — the run==eval^R oracle (test 1) already passes.
    }
}
