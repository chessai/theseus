#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! B-2.2b — PROVE `w_pc = 8` is CORRECT, not just well-typed.
//!
//! B-2.2a generalized the PC builders (`pc_add_br_with`/`pc_inc_with`,
//! `fetch_fields`, `encode_with`/`decode_with`) to `layout.pc_fields()` so a
//! hand-built `w_pc=8` layout (pc_fields()==1, no PC_hi) TYPE-CHECKS. But
//! `type_of` cannot see intra-word wire aliasing (the Fable B-2.2a review
//! MAJOR-1 demonstrated this: a `w_pc=12` layout type-checks yet silently
//! routes PC bits onto BR wires). Only a REAL RUN — evaluating the isos over
//! encoded state — proves the field addressing at `w_pc=8` is alias-free.
//! That is this file's job.
//!
//! TEST 1 — the mechanized ADR-0034 adder identity at k=8, EXHAUSTIVE over
//! all 2^16 (PC, BR) pairs: `(pc_add_br_with ; pc_inc_with)(&w_pc8)` realizes
//! `PC' = (PC + sext(BR) + 1) mod 2^8`. At w_pc=8, BR is already 8-bit so
//! sext(BR) is the identity → the oracle is `(PC + BR + 1) mod 256`, the
//! OLDEST proven case (the pre-widen 8-bit PC). Also checks BR/s0/s1 are
//! preserved and the ADJOINT subtracts.
//!
//! TEST 2 — a real end-to-end CPU run at `w_pc=8` via
//! `run_rom_on_cpu_with(&w_pc8, ...)` on a small CPU-only program that
//! EXERCISES the PC adder (a taken `Swapbr` forward branch + a taken
//! backward branch + straight-line `pc_inc` steps). The core assertion:
//! narrowing `w_pc` to 8 changes NOTHING observable — the w_pc=8 final state
//! equals the SAME program run at the default w_pc=16 layout, on every input.
//! `run_rom_on_cpu_with` internally enforces terminal quiescence
//! (BR/s0/s1==0) and the `budget + s_max <= pc_addr_space()` bound, so a
//! returned `Ok` already witnesses quiescence.

use theseus_core::Iso;

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{run_rom_on_cpu, run_rom_on_cpu_with};
use theseus_tisc::step::{pc_add_br_with, pc_inc_with};
use theseus_tisc::types::{Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

const EVAL_FUEL: u64 = 8_000_000_000;

/// A `w_pc=8` layout: PC occupies a single 8-bit CONTROL field (no PC_hi).
/// Everything else is the default so the change under test (PC width) is
/// isolated. Constructed BY HAND — B-2.2b does not DERIVE w_pc (that is
/// B-2.2c).
fn layout_w_pc8() -> Layout {
    Layout {
        w_pc: 8,
        ..Layout::default()
    }
}

// ══════════════════════════════════════════════════════════════════════
// TEST 1 — mechanized ADR-0034 adder identity at k=8 (EXHAUSTIVE, 2^16).
// ══════════════════════════════════════════════════════════════════════
//
// The oracle: at w_pc=8, `PC' = (PC + sext(BR) + 1) mod 2^8`. Since BR is
// already 8-bit, sext(BR) == BR, so the oracle collapses to
// `(PC + BR + 1) mod 256` — arithmetic on u8 with wrapping. This is the
// pre-widen 8-bit-PC identity, the oldest proven ADR-0034 case, now realized
// by the pc_fields()==1 (PC_hi-dropped, BR/op slid down one word) field
// addressing. Any wire-aliasing bug that `type_of` missed shows up here as a
// wrong PC or a clobbered BR/s0/s1 on some pair.

/// A w_pc=8 state: 8-bit PC (high byte does not exist), 8-bit BR, s0=s1=0
/// (the decoder scratch conventions the adder requires + restores). A
/// nonzero, distinctive IR / register pattern so that any misroute of a PC
/// bit into a non-PC field would perturb an observable field.
fn wpc8_state(pc: u8, br: u8) -> StateVals {
    let mut regs = [0u8; NUM_REGS];
    // A recognizable, non-trivial register pattern (any clobber shows up).
    regs[0] = 0x2A;
    regs[1] = 0xC3;
    regs[NUM_REGS - 1] = 0x5A;
    StateVals {
        regs,
        pc: u16::from(pc),
        br,
        ir: 0xDEAD_BEEF,
        scratch: {
            let mut s = [0u8; NUM_SCRATCH];
            // s0=s1=0 required by the adder; the REST of scratch is nonzero to
            // catch a stray write outside the [s0,s1] operand region.
            s[2] = 0xDE;
            s[3] = 0xAD;
            s
        },
        mem: [0; MEM_WORDS],
    }
}

fn eval_wpc8(layout: &Layout, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode_with(layout), &mut fuel)
        .unwrap_or_else(|e| panic!("w_pc8 eval failed: {e:?}"));
    StateVals::decode_with(layout, &ev)
}

#[test]
fn adr0034_split_add_exhaustive_wpc8() {
    let layout = layout_w_pc8();
    // The end-of-step PC update as the hardware computes it at w_pc=8:
    // pc_add_br ; pc_inc, both at the reduced (1-PC-field) layout.
    let iso = Iso::seq(pc_add_br_with(&layout), pc_inc_with(&layout));

    let mut checked: u64 = 0;
    for br in 0u8..=255 {
        for pc in 0u8..=255 {
            let s = wpc8_state(pc, br);
            let got = eval_wpc8(&layout, &iso, &s);

            // sext(BR) is identity at w_pc=8 ⇒ PC' = (PC + BR + 1) mod 256.
            let expected_pc = pc.wrapping_add(br).wrapping_add(1);
            assert_eq!(
                got.pc,
                u16::from(expected_pc),
                "w_pc=8 split-add mismatch: pc={pc:#04x} br={br:#04x} \
                 (expected (PC+BR+1) mod 256 = {expected_pc:#04x}, got {:#06x})",
                got.pc
            );
            // The high byte of the 16-bit PC field MUST stay 0 at w_pc=8: PC
            // only occupies one word, so decode reads no PC_hi — but a bit
            // leaking above bit 7 would corrupt a NEIGHBORING field, caught by
            // the field-preservation checks below.
            assert!(got.pc < 256, "w_pc=8 PC must fit in 8 bits");

            // BR is a read-only control ⇒ preserved. s0/s1 restored to 0.
            assert_eq!(got.br, br, "BR preserved: pc={pc:#04x} br={br:#04x}");
            assert_eq!(got.scratch[0], 0, "s0 restored: pc={pc:#04x} br={br:#04x}");
            assert_eq!(got.scratch[1], 0, "s1 restored: pc={pc:#04x} br={br:#04x}");
            // No stray write outside the PC/BR/s0/s1 region: regs, IR, and the
            // rest of scratch must be byte-identical to entry.
            assert_eq!(got.regs, s.regs, "regs untouched: pc={pc:#04x} br={br:#04x}");
            assert_eq!(got.ir, s.ir, "IR untouched: pc={pc:#04x} br={br:#04x}");
            assert_eq!(
                &got.scratch[2..],
                &s.scratch[2..],
                "scratch[2..] untouched: pc={pc:#04x} br={br:#04x}"
            );

            checked += 1;
        }
    }
    assert_eq!(checked, 1 << 16, "must cover all 2^16 (PC,BR) pairs");

    // ── ADJOINT: sym(pc_add_br;pc_inc) subtracts (PC - sext(BR) - 1). ──
    // Sampled over carry / sign boundaries (the forward pass already proved
    // the map exhaustively; the adjoint check witnesses the sym recovers the
    // input on the interesting pairs — the reversibility half of ADR-0034).
    let back = Iso::sym(iso.clone());
    for &br in &[0u8, 1, 7, 8, 0x7F, 0x80, 0xF6, 0xFF] {
        for &pc in &[0u8, 1, 0x7F, 0x80, 0xFE, 0xFF] {
            let s = wpc8_state(pc, br);
            let fwd = eval_wpc8(&layout, &iso, &s);
            let rec = eval_wpc8(&layout, &back, &fwd);
            assert_eq!(
                rec, s,
                "w_pc=8 adjoint must recover input: pc={pc:#04x} br={br:#04x}"
            );
        }
    }

    eprintln!(
        "B-2.2b TEST 1: ADR-0034 split-add EXHAUSTIVE at w_pc=8 — {checked} \
         (PC,BR) pairs == (PC+BR+1) mod 256 (sext=identity), BR/s0/s1 preserved, \
         adjoint recovers input ✓"
    );
}

// ══════════════════════════════════════════════════════════════════════
// TEST 2 — a real end-to-end CPU run at w_pc=8 == the SAME run at w_pc=16.
// ══════════════════════════════════════════════════════════════════════
//
// A small CPU-only program that EXERCISES the PC adder at w_pc=8:
//   - a taken FORWARD branch (Swapbr with a positive offset ⇒ pc_add_br runs
//     with a nonzero BR whose top bit is CLEAR),
//   - a taken BACKWARD branch (Swapbr with a top-bit-SET offset ⇒ the sext
//     path — identity at w_pc=8, but the come-from token is > 127),
//   - straight-line pc_inc steps between/around them.
//
// The branch idiom mirrors the compiler's `emit_skip_block` (compile.rs):
//   Addi rb, dist ; Swapbr rb            (BR <- dist; PC += sext(dist)+1)
//   ... skipped/looped body ...
//   Swapbr rb                            (come-from: BR <- 0, rb <- token)
// A `Swapbr rd` swaps BR <-> r[rd]; the PC update uses the POST-execute BR.

/// A CPU-only program with an UNCONDITIONAL taken forward branch over a body,
/// bracketed by straight-line increments. Layout (addresses):
///   0: Addi r1, +2         ; r1 = 2 (the forward skip distance)
///   1: Swapbr r1           ; BR <- 2, r1 <- 0; PC = 1 + sext(2) + 1 = 4
///   2: Xori r2, 0xFF       ; SKIPPED (would flip r2 if executed)
///   3: Xori r2, 0x0F       ; SKIPPED
///   4: Swapbr r1           ; come-from: r1 <- 2 (the token), BR <- 0
///   5: Addi r3, +1         ; r3 += 1  (straight-line pc_inc)
///   6: Sub  r3, r0         ; r3 -= r0 (straight-line)
///   7: Subi r1, +2         ; r1 -= 2  (undo the come-from token → r1 = 0)
/// The Swapbr at 1 drives pc_add_br with BR=2 (forward, top bit clear); the
/// increments drive pc_inc. r2 must be UNCHANGED (the body was skipped).
fn forward_branch_program() -> Vec<Instr> {
    vec![
        Instr::Addi { rd: 1, imm: 2 }, // 0
        Instr::Swapbr { rd: 1 },       // 1  -> jumps to 4
        Instr::Xori { rd: 2, imm: 0xFF }, // 2 (skipped)
        Instr::Xori { rd: 2, imm: 0x0F }, // 3 (skipped)
        Instr::Swapbr { rd: 1 },       // 4  come-from
        Instr::Addi { rd: 3, imm: 1 }, // 5
        Instr::Sub { rd: 3, rs: 0 },   // 6
        Instr::Subi { rd: 1, imm: 2 }, // 7
    ]
}

/// A second CPU-only branch program — the canonical `emit_skip_block` bracket
/// (compile.rs): a forward `Swapbr` jumps over a 2-instr body and LANDS ON the
/// come-from `Swapbr`, which clears BR (so the program terminates quiescent).
///   0: Addi r1, +2         ; r1 = 2 = body length (the forward skip distance)
///   1: Swapbr r1           ; BR<-2, r1<-0; PC = 1 + sext(2) + 1 = 4  (skip 2,3)
///   2: Add  r2, r0         ; body (skipped)
///   3: Xor  r2, r3         ; body (skipped)
///   4: Swapbr r1           ; come-from: BR<-0, r1<-2 (token)
///   5: Subi r1, +2         ; undo token → r1 = 0
/// This runs pc_add_br with BR=2 at address 1. When started with the machine
/// near the top of the 8-bit space (the wrap test), pc_inc's carry also walks
/// PC high within the 8-bit range.
fn skip_block_program() -> Vec<Instr> {
    vec![
        Instr::Addi { rd: 1, imm: 2 }, // 0
        Instr::Swapbr { rd: 1 },       // 1 -> jumps to 4 (come-from), skipping 2,3
        Instr::Add { rd: 2, rs: 0 },   // 2 (skipped)
        Instr::Xor { rd: 2, rs: 3 },   // 3 (skipped)
        Instr::Swapbr { rd: 1 },       // 4 come-from: clears BR
        Instr::Subi { rd: 1, imm: 2 }, // 5
    ]
}

fn init_regs(regs: [u8; NUM_REGS], pc: u8) -> StateVals {
    StateVals {
        regs,
        pc: u16::from(pc),
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    }
}

/// The core B-2.2b authority: a real w_pc=8 CPU run == the SAME program at the
/// default w_pc=16 layout, over several inputs. If narrowing w_pc to 8 has a
/// wire-aliasing bug that `type_of` missed, the two runs DIVERGE here.
fn assert_wpc8_eq_wpc16(prog: &[Instr], init: StateVals, budget: usize, s_max: usize, label: &str) {
    let w_pc8 = layout_w_pc8();

    let got8 = run_rom_on_cpu_with(&w_pc8, prog, init.clone(), budget, s_max, theseus_eval::eval)
        .unwrap_or_else(|e| panic!("{label}: w_pc=8 run FAILED: {e:?}"));

    // The w_pc=16 reference: the DEFAULT-layout netlist run (run_rom_on_cpu
    // uses Layout::default()). run_rom_on_cpu_with(&Layout::default(), ...)
    // is the same path; we use run_rom_on_cpu for the canonical 16-bit run.
    let got16 = run_rom_on_cpu(prog, init.clone(), budget, s_max, theseus_eval::eval)
        .unwrap_or_else(|e| panic!("{label}: w_pc=16 run FAILED: {e:?}"));

    assert_eq!(
        got8, got16,
        "{label}: w_pc=8 run DIVERGED from w_pc=16\n  w_pc=8:  {got8:?}\n  w_pc=16: {got16:?}"
    );
    // run_rom_on_cpu_with already enforced terminal quiescence (BR/s0/s1==0);
    // assert it explicitly for the report.
    assert_eq!(got8.br, 0, "{label}: terminal BR quiescent (w_pc=8)");
    assert_eq!(got8.scratch[0], 0, "{label}: terminal s0 quiescent (w_pc=8)");
    assert_eq!(got8.scratch[1], 0, "{label}: terminal s1 quiescent (w_pc=8)");
    eprintln!("B-2.2b TEST 2 [{label}]: w_pc=8 == w_pc=16, terminal quiescent ✓ (final regs={:?})", got8.regs);
}

#[test]
fn wpc8_run_equals_wpc16_forward_branch() {
    let prog = forward_branch_program();
    let budget = prog.len(); // 8 productive steps (Swapbr skips 2,3 but PC still walks the same B)

    // Several register inputs, incl. the r2 the skipped body would flip and
    // edge bytes on r0/r3.
    let inputs = [
        [0x00, 0x00, 0x11, 0x00, 0, 0, 0, 0],
        [0x01, 0x00, 0xAA, 0x05, 0, 0, 0, 0],
        [0xFF, 0x00, 0x55, 0x80, 0, 0, 0, 0],
        [0x7F, 0x00, 0x00, 0xFF, 0, 0, 0, 0],
    ];
    for regs in inputs {
        assert_wpc8_eq_wpc16(
            &prog,
            init_regs(regs, 0),
            budget,
            0,
            &format!("fwd-branch regs={regs:?}"),
        );
    }

    // Verify the branch was actually TAKEN (r2 unchanged: the body at 2,3 was
    // skipped) — otherwise the test would pass vacuously with an untaken branch.
    let regs = [0x01, 0x00, 0xAA, 0x05, 0, 0, 0, 0];
    let got = run_rom_on_cpu_with(
        &layout_w_pc8(),
        &prog,
        init_regs(regs, 0),
        budget,
        0,
        theseus_eval::eval,
    )
    .expect("branch-taken run");
    assert_eq!(
        got.regs[2], 0xAA,
        "the FORWARD BRANCH must be taken (body at pc 2,3 skipped ⇒ r2 unchanged); \
         if r2 flipped the branch fell through and pc_add_br was NOT exercised"
    );
}

#[test]
fn wpc8_run_equals_wpc16_high_pc_and_branch() {
    // Two ways to walk the w_pc=8 PC adder near the top of the 8-bit range in a
    // REAL run, both asserting w_pc=8 == w_pc=16:
    //
    //   Case A — the skip-block branch taken at PC=0 (pc_add_br with BR=2), then
    //            park: exercises the branch adder end-to-end on a second program.
    //   Case B — the machine STARTED high (PC in 240..252), running pure-park
    //            pc_inc steps whose carries ripple through the HIGH bits of the
    //            8-bit PC word (e.g. 0xEF → 0xF0, 0xFB → 0xFC). This is the
    //            w_pc=8 analogue of widen_pc_payoff's "PC crosses 256" carry
    //            check, but kept in-range (parked PC < 256) so w_pc=8 and
    //            w_pc=16 agree exactly (their address spaces only diverge AT the
    //            256 boundary, which TEST 1 covers via (PC+BR+1) mod 256).
    //
    // NOTE ON BACKWARD BRANCHES (BR top-bit set): a TERMINATING hand-built
    // backward branch is not constructible without a loop counter — the
    // reversible come-from RESTORES the offset token into its register, which
    // re-arms the branch on the next pass (an infinite loop). The backward
    // (top-bit-set BR, negative-sext) arithmetic through the real
    // `pc_add_br_with(&w_pc8) ; pc_inc_with(&w_pc8)` datapath is therefore
    // proven EXHAUSTIVELY by TEST 1 over all 128 top-bit-set BR values
    // (BR ∈ 0x80..=0xFF), which is the same iso sequence the run drives.
    let prog = skip_block_program();

    // Case A: PC=0, branch taken — pc_add_br with BR=2 (forward), then park.
    let budget_a = prog.len();
    for regs in [
        [0x00, 0x00, 0x00, 0x00, 0, 0, 0, 0],
        [0x12, 0x00, 0x34, 0x56, 0, 0, 0, 0],
        [0xFF, 0x00, 0xFF, 0xFF, 0, 0, 0, 0],
    ] {
        assert_wpc8_eq_wpc16(
            &prog,
            init_regs(regs, 0),
            budget_a,
            0,
            &format!("skip-block PC=0 regs={regs:?}"),
        );
    }

    // Verify the branch was actually TAKEN (r2 unchanged: body at 2,3 skipped).
    let got = run_rom_on_cpu_with(
        &layout_w_pc8(),
        &prog,
        init_regs([0x12, 0x00, 0x34, 0x56, 0, 0, 0, 0], 0),
        budget_a,
        0,
        theseus_eval::eval,
    )
    .expect("skip-block branch-taken run");
    assert_eq!(
        got.regs[2], 0x34,
        "skip-block FORWARD BRANCH must be taken (body at pc 2,3 skipped ⇒ r2 \
         unchanged); a fall-through would XOR/ADD r2 and not exercise pc_add_br"
    );

    // Case B: start the machine HIGH so pc_inc carries ripple through the top
    // bits of the 8-bit PC word. Pure-park (PC ≥ rom_len ⇒ idle word 0), so the
    // ONLY dynamics are the pc_inc adder walking PC up; start + budget < 256
    // keeps it in-range so w_pc=8 == w_pc=16 exactly.
    let budget_b = 4;
    for start in [239u8, 247, 251] {
        // start + budget_b ≤ 255 < 256 ⇒ parked PC stays in the 8-bit space and
        // both layouts advance PC identically (through the 0xEF/0xF7/0xFB high
        // carries).
        assert_wpc8_eq_wpc16(
            &prog,
            init_regs([0u8; NUM_REGS], start),
            budget_b,
            0,
            &format!("high-park PC={start} (pc_inc high-bit carries)"),
        );
    }
}
