#![cfg(feature = "heavy-tests")] // HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! SystemRing closure certificate (M-C2 Stage 1b-B).
//!
//! Recirculates `system_step` through a [`SystemRing`] and proves the
//! composite closure certificate at N = `MEM_WORDS`:
//!   `restored_ok ∧ run == run_open ∧ run == eval(system_step)^R ∧ bus_quiescent`
//!
//! Uses a ROM that exercises both MEXCH (mem path recirculates) and
//! non-MEXCH instructions. Domain is BOUNDED: 4 sample states, R ≤ 4.
//!
//! # Compile-time N and closure scope
//!
//! `MEM_WORDS` is a compile-time `const usize` (defined in `types.rs`,
//! currently 4 — the shipped configuration).  The entire carrier
//! (`system_state_ty`, `mem_state_ty`, `system_step`) is built from it, so
//! this certificate runs at N = 4.  Runtime N-parameterization would require
//! a pervasive refactor that is not warranted.
//!
//! The composite-ring **closure** property proved here — `run == run_open ==
//! eval(system_step)^R`, system_step ancilla-cleanliness, and
//! bus-quiescence — is **N-independent**.  Ancilla-cleanliness is
//! per-instance: the only ancilla source is the CCC-NOT chain inside `step`,
//! whose structure does not depend on N.  Therefore N = 4 is representative
//! for closure.
//!
//! The memory-select tree's ancilla-cleanliness **across** widths
//! N ∈ {2, 4, 8} (where tree depth = ⌈log₂ N⌉ varies) is separately
//! discharged in Stage 1b-A — see the tests in
//! `crates/theseus-tisc/tests/composite.rs`:
//!   - `c1_real_select_mem_composite_ancilla_clean_n2`
//!   - `c1_real_select_mem_composite_ancilla_clean_n4`
//!   - `c1_real_select_mem_composite_ancilla_clean_n8`
//!   - `c1_select_mem_on_bus_ancilla_clean_n2`
//!   - `c1_select_mem_on_bus_ancilla_clean_n4`
//!   - `c1_select_mem_on_bus_ancilla_clean_n8`
//!
//! **Conclusion:** The N = 4 ring closure certificate here, combined with
//! Stage 1b-A's N ∈ {2, 4, 8} tree cleanliness, together establish the
//! composite closure at all supported memory widths.
//!
//! To re-run this certificate at a different N, change `MEM_WORDS` in
//! `types.rs` and recompile.

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_tisc::ring_run::{system_ring, SystemRing};
use theseus_tisc::step::system_step;
use theseus_tisc::types::{decode_system, encode_system, MEM_WORDS, StateVals};

const EVAL_FUEL: u64 = 8_000_000_000;

// ── Instruction encoders (spec §3) ────────────────────────────────

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

// ── Eval helper ───────────────────────────────────────────────────

fn eval_system_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, encode_system(s), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    decode_system(&ev)
}

// ── ROM with MEXCH + non-MEXCH ────────────────────────────────────

fn mexch_rom() -> Vec<u32> {
    vec![
        i_ins(0, 1, 0x05),      // 0: ADDI r1, 5
        r_ins(0b1001, 1, 0, 0), // 1: MEXCH r1, mem[0]
        i_ins(1, 6, 0x11),      // 2: SUBI r6, 0x11
    ]
}

// ── Cached ring + iso ─────────────────────────────────────────────

struct Ctx {
    ring: SystemRing,
    iso: Iso,
}

fn ctx() -> &'static Ctx {
    static CTX: OnceLock<Ctx> = OnceLock::new();
    CTX.get_or_init(|| {
        let rom = mexch_rom();
        let ring = system_ring(&rom);
        let iso = system_step(&rom);
        Ctx { ring, iso }
    })
}

// ── Sample states (IR = 0, s0 = s1 = 0, BR = 0, nonzero mem) ────

fn sample_state(pc: u8) -> StateVals {
    let mut mem = [0u8; MEM_WORDS];
    for (i, m) in mem.iter_mut().enumerate() {
        *m = 0x42u8.wrapping_add((i as u8).wrapping_mul(0x31));
    }
    StateVals {
        regs: [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
        pc,
        br: 0,
        ir: 0,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
        mem,
    }
}

/// PCs to sample: three in-ROM addresses plus one out-of-ROM.
const SAMPLE_PCS: [u8; 4] = [0, 1, 2, 4];

// ══════════════════════════════════════════════════════════════════════
// 1. Ancilla restoration — system_step lowers ancilla-clean
// ══════════════════════════════════════════════════════════════════════

#[test]
fn ancillas_restored_after_one_revolution() {
    let c = ctx();

    eprintln!(
        "SystemRing ancilla count: {}, N (MEM_WORDS) = {}",
        c.ring.ancilla_count(),
        MEM_WORDS,
    );

    for &pc in &SAMPLE_PCS {
        let start = sample_state(pc);
        let mismatches = c.ring.ancilla_mismatches(&start);
        assert!(
            mismatches.is_empty(),
            "system_step ancillas NOT restored at pc={pc}: {}/{} dirty \
             (first 20: {:?}). \
             Use lower_iso_with_ancillas_traced for diagnosis.",
            mismatches.len(),
            c.ring.ancilla_count(),
            &mismatches[..mismatches.len().min(20)]
        );
    }

    eprintln!(
        "SystemRing ancillas restored: true — all {} ancilla lines \
         return to init after one revolution on every sampled state.",
        c.ring.ancilla_count()
    );
}

// ══════════════════════════════════════════════════════════════════════
// 2. Differential oracle: ring.run(s, R) == eval(system_step)^R
// ══════════════════════════════════════════════════════════════════════

/// Fast bounded companion: MAX_R=2, pc∈{0,4}, asserting
/// run == run_open == eval(system_step)^R AND bus_quiescent.
#[test]
fn system_ring_matches_eval_oracle_fast() {
    let c = ctx();
    const MAX_R: u32 = 2;

    for &pc in &[0u8, 4] {
        let start = sample_state(pc);
        let mut oracle = start.clone();

        for r in 0..=MAX_R {
            let ring_result = c.ring.run(&start, r);
            assert_eq!(
                ring_result, oracle,
                "SystemRing.run(s, {r}) != eval^{r} at pc={pc}"
            );
            // run == run_open
            let open_result = c.ring.run_open(&start, r);
            assert_eq!(
                open_result, ring_result,
                "SystemRing: run_open(s, {r}) != run(s, {r}) at pc={pc}"
            );
            oracle = eval_system_state(
                &format!("fast_oracle/pc={pc}/r={r}"),
                &c.iso,
                &oracle,
            );
        }

        // bus_quiescent
        assert!(
            c.ring.bus_quiescent(&start),
            "SystemRing: bus NOT quiescent at pc={pc}"
        );
    }
}

#[test]
#[ignore = "authoritative recirculation oracle; run via --ignored"]
fn ring_matches_eval_oracle() {
    let c = ctx();
    const MAX_R: u32 = 4;

    for &pc in &SAMPLE_PCS {
        let start = sample_state(pc);
        let mut oracle = start.clone();

        for r in 0..=MAX_R {
            let ring_result = c.ring.run(&start, r);
            assert_eq!(
                ring_result, oracle,
                "SystemRing.run(s, {r}) != eval^{r} at pc={pc}"
            );
            oracle = eval_system_state(
                &format!("oracle/pc={pc}/r={r}"),
                &c.iso,
                &oracle,
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 3. TRUE recirculation matches eval oracle
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "authoritative recirculation oracle; run via --ignored"]
fn true_recirculation_matches_eval_oracle() {
    let c = ctx();
    const MAX_R: u32 = 4;

    for &pc in &SAMPLE_PCS {
        let start = sample_state(pc);
        let mut oracle = start.clone();

        for r in 0..=MAX_R {
            let result = c.ring.run(&start, r);
            assert_eq!(
                result, oracle,
                "SystemRing: run(s, {r}) != eval^{r} at pc={pc} — \
                 the closed (zero-garbage) runner must equal the eval oracle"
            );
            oracle = eval_system_state(
                &format!("true_recirc/pc={pc}/r={r}"),
                &c.iso,
                &oracle,
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 4. Open vs closed equivalence: run == run_open
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "authoritative recirculation oracle; run via --ignored"]
fn open_vs_closed_equivalence() {
    let c = ctx();
    const MAX_R: u32 = 4;

    for &pc in &SAMPLE_PCS {
        let start = sample_state(pc);
        for r in 0..=MAX_R {
            let closed = c.ring.run(&start, r);
            let open = c.ring.run_open(&start, r);
            assert_eq!(
                closed, open,
                "SystemRing: run(s, {r}) != run_open(s, {r}) at pc={pc}"
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 5. Bus quiescence after one revolution
// ══════════════════════════════════════════════════════════════════════

#[test]
fn bus_quiescent_after_one_revolution() {
    let c = ctx();

    for &pc in &SAMPLE_PCS {
        let start = sample_state(pc);
        assert!(
            c.ring.bus_quiescent(&start),
            "SystemRing: bus NOT quiescent after one revolution at pc={pc}"
        );
    }

    eprintln!(
        "SystemRing bus quiescence: verified on all sampled states at N={}",
        MEM_WORDS,
    );
}
