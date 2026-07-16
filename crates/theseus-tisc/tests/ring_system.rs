//! SystemRing closure certificate (M-C2 Stage 1b-B).
//!
//! Recirculates `system_step` through a [`SystemRing`] and proves the
//! composite closure certificate at N=MEM_WORDS:
//!   `restored_ok ∧ run == run_open ∧ run == eval(system_step)^R ∧ bus_quiescent`
//!
//! Uses a ROM that exercises both MEXCH (mem path recirculates) and
//! non-MEXCH instructions. Domain is BOUNDED: 4 sample states, R ≤ 4.

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_tisc::ring_run::{system_ring, SystemRing};
use theseus_tisc::step::system_step;
use theseus_tisc::types::{decode_system, encode_system, MEM_WORDS, StateVals};

const EVAL_FUEL: u64 = 8_000_000_000;

// ── Instruction encoders (spec §3) ────────────────────────────────

fn r_ins(opcode: u16, rd: u16, rs: u16, rt: u16) -> u16 {
    assert!(opcode < 16 && rd < 8 && rs < 8 && rt < 8);
    (1 << 15) | (opcode << 11) | (rd << 8) | (rs << 5) | (rt << 2)
}

fn i_ins(opc: u16, rd: u16, imm: u8) -> u16 {
    assert!(opc < 8 && rd < 8);
    (opc << 12) | (rd << 9) | (u16::from(imm) << 1)
}

// ── Eval helper ───────────────────────────────────────────────────

fn eval_system_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, encode_system(s), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    decode_system(&ev)
}

// ── ROM with MEXCH + non-MEXCH ────────────────────────────────────

fn mexch_rom() -> Vec<u16> {
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

#[test]
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
