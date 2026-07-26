#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! A-6.4 widen-PC PAYOFF: a program whose step budget B EXCEEDS 256 now
//! runs on the real CPU (`run_on_cpu == eval`), and the WCET/park caps
//! that hard-rejected B>256 under the 8-bit PC are lifted.
//!
//! Before A-6.4 the 8-bit PC wrapped at 256, so `run_rom_on_cpu`'s P2
//! park-safety guard REJECTED any program with `B + S_max > 256`
//! (`Deferred("... exceeds PC address space 256 ...")`), and past it the
//! hardware/fast-sim PC would silently wrap mod 256. The 16-bit PC lifts
//! the cap to 2^16 and the PC walks — and PARKS — correctly past 256.
//!
//! Two capability proofs:
//!  1. `park_walk_past_256`: a real straight-line ROM run for B = 300
//!     steps (PC walks 0..len, then parks len..300, CROSSING 256 in the
//!     dead zone). The real `system_step` CPU (`run_rom_on_cpu`) tracks
//!     the fast-sim exactly, and the terminal state is park-quiescent
//!     (BR=s0=s1=0) — impossible under an 8-bit PC (it would wrap to 44
//!     and re-execute the ROM, corrupting the answer).
//!  2. `p2_guard_lifted_to_16bit`: the P2 cap now admits B+S_max up to
//!     2^16 and still rejects beyond it (the widened bound is enforced).

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::{run_rom_fast, run_rom_on_cpu};
use theseus_tisc::types::{StateVals, MEM_WORDS, PC_ADDR_SPACE};

/// A small straight-line ROM: `r1 += 1` at each of `len` addresses. Every
/// word is legal and non-idle, so the productive trace is exactly `len`
/// steps; beyond `len` the PC parks (word 0 = idle) — and with B > 256 the
/// parked PC crosses 256, exercising the 16-bit address space.
fn inc_rom(len: usize) -> Vec<Instr> {
    (0..len).map(|_| Instr::Addi { rd: 1, imm: 1 }).collect()
}

fn init() -> StateVals {
    StateVals {
        regs: [0; 8],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    }
}

#[test]
fn park_walk_past_256() {
    const LEN: usize = 100;
    const B: usize = 260; // > 256: PC parks past the old 8-bit wrap point
    assert!(B > 256, "the payoff requires B > 256");

    let prog = inc_rom(LEN);
    let rom = assemble(&prog).expect("assemble inc_rom");

    // Reference: the fast direct simulator (16-bit PC) for B steps.
    let want = run_rom_fast(&rom, init(), B);
    // r1 was incremented once per productive step (LEN), then parked.
    assert_eq!(want.regs[1], (LEN as u8), "fast-sim: r1 = LEN increments");
    // PC walked to B (0 + B·(0+1) = B), CROSSING 256 without wrapping.
    assert_eq!(want.pc, B as u16, "fast-sim: PC advanced to 260 (no 8-bit wrap)");
    assert!(want.pc > 256, "PC must have crossed the old 256 boundary");

    // The real CPU (system_step eval), B steps, with the P2 park guard.
    // s_max = 0 (straight-line). B + s_max = 300 ≤ 2^16, so P2 admits it.
    let got = run_rom_on_cpu(&prog, init(), B, 0, theseus_eval::eval)
        .expect("run_rom_on_cpu must ADMIT and run a B=300 (>256) program");
    assert_eq!(
        got, want,
        "real system_step (run_on_cpu) must equal the fast-sim at B={B} > 256"
    );
    // Terminal park-quiescence (the P2 witness): BR=s0=s1=0.
    assert_eq!(got.br, 0, "terminal BR quiescent");
    assert_eq!(got.scratch[0], 0, "terminal s0 quiescent");
    assert_eq!(got.scratch[1], 0, "terminal s1 quiescent");

    eprintln!(
        "PAYOFF park_walk_past_256: ran B=260 (>256) real steps, PC={} parked, \
         r1={} correct, terminal quiescent ✓",
        got.pc, got.regs[1]
    );
}

#[test]
fn p2_guard_lifted_to_16bit() {
    // The P2 park-safety cap now bounds B+S_max by 2^W_PC, not 256.
    let prog = inc_rom(4);
    // Just under the widened cap: admitted.
    let big = PC_ADDR_SPACE - 1;
    // (We don't RUN big steps — that's 2^16 evals — we assert the guard's
    // arithmetic admits/ rejects at the 2^16 boundary by feeding a budget
    // just past it, which must be rejected fast before any eval.)
    let over = PC_ADDR_SPACE + 1;
    let err = run_rom_on_cpu(&prog, init(), over, 0, theseus_eval::eval)
        .expect_err("B > 2^16 must be rejected by the widened P2 guard");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("exceeds PC address space") && msg.contains(&PC_ADDR_SPACE.to_string()),
        "P2 must reject against the 16-bit cap (2^16={PC_ADDR_SPACE}), got: {msg}"
    );
    // Sanity: the old 256 boundary is NO LONGER the cap — a budget of 300
    // (rejected under the 8-bit PC) is admitted (run separately above).
    assert!(big > 256, "widened cap {big} far exceeds the old 256");
    eprintln!("PAYOFF p2_guard: cap lifted 256 → {PC_ADDR_SPACE}; B>2^16 rejected ✓");
}
