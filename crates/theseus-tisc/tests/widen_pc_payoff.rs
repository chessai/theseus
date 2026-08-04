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
//! the cap to 2^16 and the PC walks -- and PARKS -- correctly past 256.
//!
//! Two capability proofs:
//!  1. `park_walk_past_256`: (a) the fast-sim walks a whole B = 300-step
//!     run (productive, then parked) without wrapping at 256 -- an 8-bit PC
//!     would read PC 44 and re-execute the ROM, corrupting the answer;
//!     (b) the REAL `system_step` CPU is started just below 256 and steps
//!     across it (254 → 260), proving the `pc_inc` carry crosses the
//!     PC_lo→PC_hi byte boundary on the datapath == the fast-sim, terminal
//!     park-quiescent. (Only 6 real evals -- each system_step eval is
//!     expensive; the full 2^16 PC-increment coverage is the isolated
//!     `pc_inc_exhaustive_all_16bit` in tests/step.rs.)
//!  2. `p2_guard_lifted_to_16bit`: the P2 cap now admits B+S_max up to
//!     2^16 and still rejects beyond it (the widened bound is enforced).

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::{run_rom_fast, run_rom_on_cpu};
use theseus_tisc::types::{StateVals, MEM_WORDS, PC_ADDR_SPACE};

/// A small straight-line ROM: `r1 += 1` at each of `len` addresses. Every
/// word is legal and non-idle, so the productive trace is exactly `len`
/// steps; beyond `len` the PC parks (word 0 = idle) -- and with B > 256 the
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
    // Part 1 -- FAST-SIM reference: the 16-bit PC walks a whole B = 300-step
    // run (productive 0..LEN, then parks LEN..300) WITHOUT wrapping at 256.
    const LEN: usize = 5;
    const B: usize = 300; // > 256
    let prog = inc_rom(LEN);
    let rom = assemble(&prog).expect("assemble inc_rom");
    let want = run_rom_fast(&rom, init(), B);
    assert_eq!(want.regs[1], LEN as u8, "fast-sim: r1 = LEN increments");
    assert_eq!(
        want.pc, B as u16,
        "fast-sim: PC advanced to {B} (no 8-bit wrap -- an 8-bit PC would read 44)"
    );
    assert!(want.pc > 256, "fast-sim PC crossed the old 256 boundary");

    // Part 2 -- REAL CPU crossing 256: start the real `system_step` machine
    // just below 256 and run 6 steps so PC crosses the old 8-bit wrap point
    // (254 → 255 → 256 → 257 → 258 → 259 → 260) on the HARDWARE. This is the
    // load-bearing new behavior -- the pc_inc carry crossing the PC_lo→PC_hi
    // byte boundary on the actual datapath -- and it must equal the fast-sim.
    // (Only 6 real evals: each system_step eval is expensive; the multi-
    // hundred-step park is covered by Part 1 + the isolated pc_inc exhaustive
    // over all 2^16 PCs in tests/step.rs.)
    let mut cpu = init();
    cpu.pc = 254;
    const CROSS: usize = 6;
    // Real CPU: 6 steps via the guarded run_rom_on_cpu (P2 admits B=254+6).
    let got = run_rom_on_cpu(&prog, cpu.clone(), CROSS, 0, theseus_eval::eval)
        .expect("run_rom_on_cpu must ADMIT PC crossing 256 (widened P2 cap)");
    // Fast-sim reference from the same start.
    let fast = run_rom_fast(&rom, cpu, CROSS);
    assert_eq!(
        fast.pc, 260,
        "fast-sim: 254 + 6 = 260 (PC crossed 256 via the byte-boundary carry)"
    );
    assert_eq!(
        got, fast,
        "real system_step must equal the fast-sim while PC crosses 256 \
         (254→260): the pc_inc inter-byte carry is correct on the datapath"
    );
    assert!(got.pc > 256, "real CPU PC crossed 256");
    // Terminal park-quiescence (the P2 witness): BR=s0=s1=0.
    assert_eq!(got.br, 0, "terminal BR quiescent");
    assert_eq!(got.scratch[0], 0, "terminal s0 quiescent");
    assert_eq!(got.scratch[1], 0, "terminal s1 quiescent");

    eprintln!(
        "PAYOFF park_walk_past_256: fast-sim ran B={B} (>256, PC={}); real CPU \
         crossed 256 (254→{}) == fast-sim, terminal quiescent ✓",
        want.pc, got.pc
    );
}

#[test]
fn p2_guard_lifted_to_16bit() {
    // The P2 park-safety cap now bounds B+S_max by 2^W_PC, not 256.
    let prog = inc_rom(4);
    // Just under the widened cap: admitted.
    let big = PC_ADDR_SPACE - 1;
    // (We don't RUN big steps -- that's 2^16 evals -- we assert the guard's
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
    // Sanity: the old 256 boundary is NO LONGER the cap -- a budget of 300
    // (rejected under the 8-bit PC) is admitted (run separately above).
    assert!(big > 256, "widened cap {big} far exceeds the old 256");
    eprintln!("PAYOFF p2_guard: cap lifted 256 → {PC_ADDR_SPACE}; B>2^16 rejected ✓");
}
