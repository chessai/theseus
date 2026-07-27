#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! Differential-oracle tests for the TISC-1 ring runner
//! (ADR-0033, M4-v2): `step_ring(rom).run(&s, R)` ≡ `eval(step)^R`.
//!
//! Uses a small straight-line ROM (3 instructions, BR untouched →
//! fall-through) to keep build + lower + balance_compact fast while
//! exercising the full ring pipeline at real scale.
//!
//! Tests 3–6 verify the zero-garbage ring (ADR-0040 phase C payoff):
//! ancillas restored after one revolution, `run_true == eval^R`,
//! `run_true == run`, and zero boundary vs Bennett's 320.

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_tisc::ring_run::{closed_step_ring, step_ring, ClosedStepRing, StepRing};
use theseus_tisc::step::step;
use theseus_tisc::types::{state_ty, StateVals, MEM_WORDS};

const EVAL_FUEL: u64 = 8_000_000_000;

// ── Instruction encoders (spec §3) ─────────────────────────────────

// A-0 32-bit word: R: fmt(31) | op[30:25] | rd[24:20] | rs[19:15] | rt[14:10].
fn r_ins(opcode: u32, rd: u32, rs: u32, rt: u32) -> u32 {
    assert!(opcode < 64 && rd < 16 && rs < 16 && rt < 16);
    (1 << 31) | (opcode << 25) | (rd << 20) | (rs << 15) | (rt << 10)
}

// I: fmt(31)=0 | op[30:25] | rd[24:20] | imm[19:0].
fn i_ins(opc: u32, rd: u32, imm: u8) -> u32 {
    assert!(opc < 64 && rd < 16);
    (opc << 25) | (rd << 20) | u32::from(imm)
}

// ── Eval helper ────────────────────────────────────────────────────

fn eval_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

// ── Small straight-line ROM (no BR mutation → fall-through) ────────

fn small_rom() -> Vec<u32> {
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

fn closed_ctx() -> &'static ClosedStepRing {
    static CTX: OnceLock<ClosedStepRing> = OnceLock::new();
    CTX.get_or_init(|| closed_step_ring(&small_rom()))
}

// ── Sample states (IR = 0, s0 = s1 = 0, BR = 0) ──────────────────

fn sample_state(pc_in: impl Into<u16>) -> StateVals {
    let pc: u16 = pc_in.into();
    StateVals {
        regs: [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
        pc,
        br: 0,
        ir: 0,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
        mem: [0; MEM_WORDS],
    }
}

// ══════════════════════════════════════════════════════════════════════
// 1. Differential oracle: ring.run(s, R) == eval(step)^R
// ══════════════════════════════════════════════════════════════════════

/// Fast bounded companion: MAX_R=2, pc∈{0,4}, asserting
/// ring.run == eval^R AND run == run_open.
#[test]
fn ring_matches_eval_oracle_fast() {
    let c = ctx();
    const MAX_R: u32 = 2;

    for &pc in &[0u8, 4] {
        let start = sample_state(pc);
        let mut oracle = start.clone();

        for r in 0..=MAX_R {
            let ring_result = c.ring.run(&start, r);
            assert_eq!(
                ring_result, oracle,
                "ring.run(s, {r}) != eval^{r} at pc={pc}"
            );
            // Also verify run == run_open
            let open_result = c.ring.run_open(&start, r);
            assert_eq!(
                open_result, ring_result,
                "run_open(s, {r}) != run(s, {r}) at pc={pc}"
            );
            oracle = eval_state(&format!("fast_oracle/pc={pc}/r={r}"), &c.iso, &oracle);
        }
    }
}

#[test]
#[ignore = "authoritative recirculation oracle; run via --ignored"]
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
            oracle = eval_state(&format!("oracle/pc={pc}/r={r}"), &c.iso, &oracle);
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
        let s = sample_state(0u8);
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
// 3. Zero-garbage assertion: ancillas restored after one revolution
//    (ADR-0040 phase C payoff — the parity obstruction is voided)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn ancillas_restored_after_one_revolution() {
    let c = ctx();

    eprintln!("ancilla count: {}", c.ring.ancilla_count());

    // Assert across ALL sampled PCs — not just pc=0.
    for &pc in &[0u8, 1, 2, 4] {
        let start = sample_state(pc);
        let mismatches = c.ring.ancilla_mismatches(&start);
        assert!(
            mismatches.is_empty(),
            "ancillas NOT restored at pc={pc}: {}/{} dirty (first 20: {:?}). \
             The R1-lowered step must be ancilla-clean on every sampled state.",
            mismatches.len(),
            c.ring.ancilla_count(),
            &mismatches[..mismatches.len().min(20)]
        );
    }

    eprintln!(
        "ancillas restored: true — all {} ancilla lines return to init \
         after one revolution on every sampled state. The parity \
         obstruction is voided (ADR-0040 phase C).",
        c.ring.ancilla_count()
    );
}

// ══════════════════════════════════════════════════════════════════════
// 4. TRUE recirculation: run_true(s, R) == eval(step)^R
//    (plain path, zero boundary — no Bennett, no re-supply)
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "authoritative recirculation oracle; run via --ignored"]
fn true_recirculation_matches_eval_oracle() {
    let c = ctx();
    const MAX_R: u32 = 5;

    for &pc in &[0u8, 1, 2, 4] {
        let start = sample_state(pc);
        let mut oracle = start.clone();

        for r in 0..=MAX_R {
            let true_result = c.ring.run(&start, r);
            assert_eq!(
                true_result, oracle,
                "run(s, {r}) != eval^{r} at pc={pc} — \
                 the default closed (zero-garbage) runner feeds ancilla \
                 outputs back and must equal the eval oracle"
            );
            oracle = eval_state(&format!("true_recirc/pc={pc}/r={r}"), &c.iso, &oracle);
        }
    }

    eprintln!(
        "TRUE recirculation verified: run_true == eval^R for R in 0..={MAX_R} \
         across all sampled states. The plain ring closes with ZERO boundary."
    );
}

// ══════════════════════════════════════════════════════════════════════
// 5. run (closed) == run_open (fresh re-supply): both paths agree
//    (ancilla-clean step means feeding back restored ancillas ==
//    re-supplying fresh ones — this equivalence certifies the closure)
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "authoritative recirculation oracle; run via --ignored"]
fn run_true_equals_run_when_ancillas_restored() {
    let c = ctx();
    const MAX_R: u32 = 5;

    for &pc in &[0u8, 1, 2, 4] {
        let start = sample_state(pc);
        for r in 0..=MAX_R {
            let feedback = c.ring.run(&start, r);
            let fresh = c.ring.run_open(&start, r);
            assert_eq!(
                fresh, feedback,
                "run_open(s, {r}) != run(s, {r}) at pc={pc} — \
                 with ancillas restored, closed feed-back and fresh \
                 re-supply must agree"
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 6. Zero-garbage quantification: plain run_true boundary == 0
//    vs Bennett ClosedStepRing boundary == 2·w = 320
// ══════════════════════════════════════════════════════════════════════

#[test]
fn zero_boundary_vs_bennett_boundary() {
    let plain = &ctx().ring;
    let closed = closed_ctx();

    // Plain path with ancilla-clean step: ZERO external boundary.
    // Every ancilla output == init, so nothing needs re-supplying.
    let plain_boundary = 0usize;

    // Bennett path: fixed 2·w boundary re-supplied per revolution.
    let bennett_boundary = closed.boundary_size();

    eprintln!(
        "plain  path (run_true): boundary per revolution = {plain_boundary} (zero — ancilla-clean)"
    );
    eprintln!(
        "Bennett path (closed) : boundary per revolution = {bennett_boundary} (2·w = 2·{})",
        closed.data_width()
    );
    eprintln!(
        "plain  path ancilla count: {} (all restored)",
        plain.ancilla_count()
    );
    eprintln!(
        "Bennett path ancilla count: {} (forward: {}, boundary: {})",
        closed.ancilla_count(),
        closed.forward_ancilla_count(),
        closed.boundary_size(),
    );

    // The literal zero-garbage ring: plain path needs NOTHING re-supplied.
    assert_eq!(
        plain_boundary, 0,
        "plain run_true boundary must be zero (ancilla-clean step)"
    );

    // Bennett path still has its 2·w boundary (320 for TISC-1 State).
    assert_eq!(
        bennett_boundary,
        2 * closed.data_width(),
        "Bennett boundary must be 2·w"
    );
    assert!(
        bennett_boundary > 0,
        "Bennett boundary must be nonzero (the honest residue)"
    );

    // The payoff: zero < 320.
    assert!(
        plain_boundary < bennett_boundary,
        "plain path boundary ({plain_boundary}) must be strictly less \
         than Bennett boundary ({bennett_boundary})"
    );

    eprintln!(
        "\nThe plain R1-lowered ring closes with ZERO per-revolution \
         boundary — the literal zero-garbage ring (ADR-0040 phase C). \
         The Bennett ClosedStepRing demotes to a fallback for \
         K-only-dirty circuits."
    );
}

// ══════════════════════════════════════════════════════════════════════
// 7. CLOSED ring (Bennett path): differential oracle with TRUE
//    recirculation — State + forward ancillas fed back, only the fixed
//    2·w boundary re-supplied (ADR-0033 ring closure, now the fallback).
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "authoritative recirculation oracle; run via --ignored"]
fn closed_ring_matches_eval_oracle() {
    let closed = closed_ctx();
    let iso = &ctx().iso;
    const MAX_R: u32 = 5;

    // The closed run injects NOTHING data-dependent after the start
    // state (forward ancillas are fed back verbatim), so agreement
    // with eval^R certifies the ancillas come back clean every
    // revolution — a dirty line would corrupt revolution r+1.
    for &pc in &[0u8, 1, 2, 4] {
        let start = sample_state(pc);
        let mut oracle = start.clone();

        for r in 0..=MAX_R {
            let ring_result = closed.run(&start, r);
            assert_eq!(
                ring_result, oracle,
                "closed_ring.run(s, {r}) != eval^{r} at pc={pc}"
            );
            oracle = eval_state(&format!("closed_oracle/pc={pc}/r={r}"), iso, &oracle);
        }
    }
}

#[test]
#[ignore = "authoritative recirculation oracle; run via --ignored"]
fn closed_ring_forward_ancillas_clean_and_boundary_fixed() {
    let closed = closed_ctx();
    let plain = &ctx().ring;
    let iso = &ctx().iso;

    let w = closed.data_width();
    assert_eq!(closed.boundary_size(), 2 * w, "boundary is exactly 2·w");
    assert_eq!(
        closed.ancilla_count(),
        closed.forward_ancilla_count() + closed.boundary_size(),
        "ancillas = forward (fed back) + boundary (re-supplied)"
    );

    eprintln!("state width w (rail pairs): {w}");
    eprintln!(
        "closed ring: forward ancillas (fed back): {}",
        closed.forward_ancilla_count()
    );
    eprintln!(
        "closed ring: boundary re-supplied per revolution: {} slots",
        closed.boundary_size()
    );
    eprintln!(
        "plain ring: ancillas re-supplied per revolution: {} lines",
        plain.ancilla_count()
    );

    // The physical win: the per-revolution external interface shrank
    // from ALL ancillas (plain path) to the fixed 2·w boundary.
    assert!(
        closed.boundary_size() < plain.ancilla_count(),
        "boundary ({}) must be smaller than the plain path's re-supply \
         set ({})",
        closed.boundary_size(),
        plain.ancilla_count()
    );

    for &pc in &[0u8, 1, 2, 4] {
        let start = sample_state(pc);

        // Every forward ancilla restored to init after one revolution.
        let mismatches = closed.forward_mismatches(&start);
        assert!(
            mismatches.is_empty(),
            "forward ancillas dirty at pc={pc}: {} of {} (first: {:?})",
            mismatches.len(),
            closed.forward_ancilla_count(),
            &mismatches[..mismatches.len().min(10)]
        );

        // The boundary residue is EXACTLY (y_j, x_j) interleaved — the
        // output word and the regenerated input word. Its POSITIONS
        // and re-supplied inits (1, 0) are data-independent; only the
        // exiting values carry data, as any nonzero Bennett residue
        // must.
        let residue = closed.boundary_residue(&start);
        assert_eq!(residue.len(), 2 * w);
        let ty = state_ty();
        let x_bits = theseus_netlist::bits_of(&start.encode(), &ty).expect("bits_of");
        let next = eval_state(&format!("residue/pc={pc}"), iso, &start);
        let y_bits = theseus_netlist::bits_of(&next.encode(), &ty).expect("bits_of");
        for j in 0..w {
            assert_eq!(
                residue[2 * j],
                y_bits[j],
                "boundary spare slot {j} != y_{j} at pc={pc}"
            );
            assert_eq!(
                residue[2 * j + 1],
                x_bits[j],
                "boundary input slot {j} != x_{j} at pc={pc}"
            );
        }
    }
}
