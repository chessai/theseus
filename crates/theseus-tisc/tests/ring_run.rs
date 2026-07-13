//! Differential-oracle tests for the TISC-1 ring runner
//! (ADR-0033, M4-v2): `step_ring(rom).run(&s, R)` ≡ `eval(step)^R`.
//!
//! Uses a small straight-line ROM (3 instructions, BR untouched →
//! fall-through) to keep build + lower + balance_compact fast while
//! exercising the full ring pipeline at real scale.

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_tisc::ring_run::{
    closed_step_ring, step_ring, ClosedStepRing, StepRing,
};
use theseus_tisc::step::step;
use theseus_tisc::types::{state_ty, StateVals};

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

fn closed_ctx() -> &'static ClosedStepRing {
    static CTX: OnceLock<ClosedStepRing> = OnceLock::new();
    CTX.get_or_init(|| closed_step_ring(&small_rom()))
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

// ══════════════════════════════════════════════════════════════════════
// 4. CLOSED ring (Bennett path): differential oracle with TRUE
//    recirculation — State + forward ancillas fed back, only the fixed
//    2·w boundary re-supplied (ADR-0033 ring closure).
// ══════════════════════════════════════════════════════════════════════

#[test]
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
            oracle = eval_state(
                &format!("closed_oracle/pc={pc}/r={r}"),
                iso,
                &oracle,
            );
        }
    }
}

#[test]
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
        let x_bits =
            theseus_netlist::bits_of(&start.encode(), &ty).expect("bits_of");
        let next = eval_state(&format!("residue/pc={pc}"), iso, &start);
        let y_bits =
            theseus_netlist::bits_of(&next.encode(), &ty).expect("bits_of");
        for j in 0..w {
            assert_eq!(
                residue[2 * j], y_bits[j],
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
