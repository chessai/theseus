#![cfg(feature = "heavy-tests")]
//! A-0 BLOCKER-1 regression (L20): the scratch-`rd` operand datapath.
//!
//! Adopted from the adversarial-review probe
//! `docs/reviews/a0-shared-crossbar-review.md`. Naming a scratch word
//! (s2..s7, operand idx 10..15 — legal per `asm::check_operand`) as the
//! `rd` operand routes it through the operand crossbar `select_operand`.
//! Before the fix, the crossbar's level-3 stage swapped `base(0)↔base(8)`
//! (r0↔s0 = `AUX_OPERAND_FIELD`, the fixed operand buffer) and
//! `base(1)↔base(9)` (r1↔s1 = `AUX_PRED_FIELD`, the predicate word),
//! DISPLACING the operand buffer and CORRUPTING the predicate
//! mid-instruction → wrong cores fire, BR clobbered, s0 left dirty
//! (breaks the inter-step s0=s1=0 invariant → unbounded corruption).
//!
//! Each op is run through the REAL evaluator in BOTH decoder forms —
//! `decode_execute()` (shared cores) and `decode_execute_ops(&Op::ALL)`
//! (replicated per-op) — with a scratch word as `rd`, and asserted to:
//!   1. equal the ISA semantics (`compile::exec_instr`),
//!   2. leave s0 (operand buffer) and s1 (predicate word) == 0, and
//!   3. agree shared ≡ replicated (proves the fix is at the crossbar,
//!      not merely in the sharing restructure).

use theseus_core::Iso;
use theseus_tisc::asm::{encode, Instr};
use theseus_tisc::compile::exec_instr;
use theseus_tisc::decoder::{self, Op};
use theseus_tisc::types::{StateVals, MEM_WORDS};

const EVAL_FUEL: u64 = 4_000_000_000;

fn eval_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

/// Build a probe input. s0=s1=0 (the decoder invariant); s2..s7 seeded
/// with live data so a scratch-`rd` write is observable and any stray
/// permutation of s0/s1 shows up.
fn st(regs: [u8; 8], ir: u32) -> StateVals {
    StateVals {
        regs,
        pc: 0x42,
        br: 0x99,
        ir,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
        mem: [0; MEM_WORDS],
    }
}

/// The ISA-semantics oracle for one instruction on one state.
fn reference(instr: Instr, s: &StateVals) -> StateVals {
    let mut out = s.clone();
    exec_instr(instr, &mut out);
    out
}

/// Every op whose `rd` operand is routed through the shared operand
/// crossbar, instantiated with a scratch-`rd` target (s2..s7). `rs`/`rt`
/// stay registers so we isolate the rd-routing path. Vary r1 across the
/// samples so that, under the pre-fix bug, r1 (which the level-3 swap
/// moved into the s1/predicate field) poisons the predicate visibly.
fn scratch_rd_samples() -> Vec<(&'static str, Instr, StateVals)> {
    // regs: r1 varied per-op to exercise predicate-field poisoning.
    let mk = |r1: u8| [0x10u8, r1, 0x20, 0x55, 0x30, 0x40, 0x50, 0x60];
    let mut v = Vec::new();
    // Group A (reg-reg): rd = s2 (idx 10), rs = r3.
    v.push(("add", Instr::Add { rd: 10, rs: 3 }, st(mk(0x01), 0)));
    v.push(("sub", Instr::Sub { rd: 11, rs: 3 }, st(mk(0xFF), 0)));
    v.push(("xor", Instr::Xor { rd: 12, rs: 5 }, st(mk(0xAA), 0)));
    v.push(("rotl", Instr::Rotl { rd: 13, rs: 4 }, st(mk(0x01), 0)));
    v.push(("rotr", Instr::Rotr { rd: 14, rs: 6 }, st(mk(0xFF), 0)));
    v.push(("exch", Instr::Exch { rd: 15, rs: 2 }, st(mk(0xAA), 0)));
    // Cswap with scratch rd (predicate source): rd=s2, rs=r4, rt=r6.
    v.push(("cswap_taken", Instr::Cswap { rd: 10, rs: 4, rt: 6 }, st(mk(0x01), 0)));
    // Ccnot with scratch rd: rd=s3, rs=r1, rt=r2.
    v.push(("ccnot", Instr::Ccnot { rd: 11, rs: 1, rt: 2 }, st(mk(0xFF), 0)));
    // NB: Swapbr is register-only by ISA (`rd < NUM_REGS`), so it has no
    // scratch-rd form. Xchg is EXCLUDED here: it is the sole op that
    // composes `select_operand(rd)` with `select_scratch(sx)` over the
    // OVERLAPPING scratch field range — for a scratch `rd`, select_operand
    // parks a displaced register value into a scratch home field that
    // select_scratch then reads, so the two crossbars alias. That is a
    // SEPARATE pre-existing datapath issue (independent of BLOCKER-1: it
    // reproduces shared ≡ replicated, so it is not a sharing divergence
    // and not the operand-buffer/predicate corruption fixed here).
    // Group B (immediate): rd = s2..s6.
    v.push(("addi", Instr::Addi { rd: 10, imm: 0x07 }, st(mk(0xFF), 0)));
    v.push(("subi", Instr::Subi { rd: 11, imm: 0x03 }, st(mk(0x01), 0)));
    v.push(("xori", Instr::Xori { rd: 12, imm: 0x0F }, st(mk(0xFF), 0)));
    v.push(("rotli", Instr::Rotli { rd: 13, imm: 0x02 }, st(mk(0xAA), 0)));
    v.push(("rotri", Instr::Rotri { rd: 14, imm: 0x03 }, st(mk(0x55), 0)));
    // fill the IR from the instruction
    v.into_iter()
        .map(|(name, instr, mut s)| {
            s.ir = encode(instr).unwrap_or_else(|e| panic!("{name}: encode {instr:?}: {e:?}"));
            (name, instr, s)
        })
        .collect()
}

#[test]
fn scratch_rd_shared_and_replicated_match_isa() {
    let shared = decoder::decode_execute();
    let replicated = decoder::decode_execute_ops(&Op::ALL);

    for (name, instr, s) in scratch_rd_samples() {
        let expected = reference(instr, &s);
        let got_shared = eval_state(name, &shared, &s);
        let got_repl = eval_state(name, &replicated, &s);

        eprintln!("op={name} instr={instr:?}");
        eprintln!("  in        : regs={:02x?} scratch={:02x?} br={:#04x}", s.regs, s.scratch, s.br);
        eprintln!("  expected  : regs={:02x?} scratch={:02x?} br={:#04x}", expected.regs, expected.scratch, expected.br);
        eprintln!("  shared    : regs={:02x?} scratch={:02x?} br={:#04x}", got_shared.regs, got_shared.scratch, got_shared.br);
        eprintln!("  replicated: regs={:02x?} scratch={:02x?} br={:#04x}", got_repl.regs, got_repl.scratch, got_repl.br);

        assert_eq!(got_shared, expected, "SHARED wrong for {name} ({instr:?})");
        assert_eq!(got_repl, expected, "REPLICATED wrong for {name} ({instr:?})");
        assert_eq!(got_shared, got_repl, "shared != replicated for {name} ({instr:?})");
        // Inter-step invariant: operand buffer (s0) and predicate (s1)
        // must be left at zero after any instruction.
        assert_eq!(got_shared.scratch[0], 0, "{name}: s0 (operand buffer) dirty");
        assert_eq!(got_shared.scratch[1], 0, "{name}: s1 (predicate) dirty");
    }
}

/// Control: register-only `rd` must remain sound in the shared form
/// (guards against a regression that fixes scratch by breaking registers).
#[test]
fn reg_rd_shared_control() {
    let shared = decoder::decode_execute();
    let ir = encode(Instr::Add { rd: 2, rs: 3 }).expect("encode");
    let s = st([0x10, 0xFF, 0x20, 0x55, 0x30, 0x40, 0x50, 0x60], ir);
    let expected = reference(Instr::Add { rd: 2, rs: 3 }, &s);
    let got = eval_state("shared-reg", &shared, &s);
    assert_eq!(got, expected, "SHARED wrong for ADD r2,r3 (register-only)");
    assert_eq!(got.scratch[0], 0);
    assert_eq!(got.scratch[1], 0);
}
