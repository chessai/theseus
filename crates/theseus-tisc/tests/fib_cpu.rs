//! M4 FUNCTIONAL HEADLINE: the TISC-1 CPU runs a real program —
//! Fibonacci — forward AND backward.
//!
//! The program is assembled from typed mnemonics ([`theseus_tisc::asm`])
//! into a ROM, and executed by iterating the machine-cycle iso
//! `step(rom) : State <-> State` via core eval (no lowering — this is
//! the CPU-executes-a-program demo, kept cheap on purpose).
//!
//! The reversible Fibonacci iteration is the two-instruction pair
//!
//!   ADD  r1, r2     ; r1 = a + b   (r2 = b unchanged)
//!   EXCH r1, r2     ; r1 = b, r2 = a + b
//!
//! so with seed (r1, r2) = (1, 1), after k pairs the registers hold
//! the consecutive Fibonacci numbers (fib(k), fib(k+1)) mod 256. The
//! ROM unrolls N = 10 pairs (20 words, T = 20 machine cycles):
//! fib stays under 256 through fib(11) = 144.
//!
//! Verified here:
//!   1. encode/decode round-trip + exact bit patterns (assembler sanity);
//!   2. FORWARD: after every pair the registers match a Rust reference
//!      Fibonacci sequence; final regs = (fib(N), fib(N+1)) = (89, 144);
//!   3. one forward step then one adjoint step = identity (step_adjoint
//!      sanity);
//!   4. BACKWARD: T adjoint steps from the final state return the
//!      machine EXACTLY to the seed (r1 = r2 = 1, PC = 0, IR/scratch 0)
//!      — the CPU is reversible end to end.

use theseus_core::Iso;
use theseus_tisc::asm::{assemble, decode, encode, Instr};
use theseus_tisc::step::step;
use theseus_tisc::types::{MEM_WORDS, StateVals};

/// Generous per-eval fuel (same order as the step tests).
const EVAL_FUEL: u64 = 8_000_000_000;

/// Fibonacci pairs unrolled in the ROM (T = 2N machine cycles).
const N: usize = 10;

// ── Run harness ────────────────────────────────────────────────────

fn eval_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

/// Apply `step(rom)` `t` times: run the machine `t` cycles forward.
fn run(rom: &[u16], start: &StateVals, t: usize) -> StateVals {
    let iso = step(rom);
    let mut cur = start.clone();
    for k in 0..t {
        cur = eval_state(&format!("run/fwd[{k}]"), &iso, &cur);
    }
    cur
}

/// Run `t` cycles forward, then `t` cycles of the adjoint
/// `Iso::sym(step(rom))`. Returns (forward endpoint, state after the
/// backward run); the latter must equal `start` (reversibility).
fn run_and_reverse(rom: &[u16], start: &StateVals, t: usize) -> (StateVals, StateVals) {
    let iso = step(rom);
    let back = Iso::sym(iso.clone());
    let mut cur = start.clone();
    for k in 0..t {
        cur = eval_state(&format!("rr/fwd[{k}]"), &iso, &cur);
    }
    let fwd = cur.clone();
    for k in (0..t).rev() {
        cur = eval_state(&format!("rr/back[{k}]"), &back, &cur);
    }
    (fwd, cur)
}

// ── The Fibonacci program ──────────────────────────────────────────

/// N repetitions of the reversible fib pair: ADD r1, r2 ; EXCH r1, r2.
fn fib_prog() -> Vec<Instr> {
    let mut prog = Vec::with_capacity(2 * N);
    for _ in 0..N {
        prog.push(Instr::Add { rd: 1, rs: 2 });
        prog.push(Instr::Exch { rd: 1, rs: 2 });
    }
    prog
}

/// Seed: r1 = r2 = 1, everything else (regs, PC, BR, IR, scratch) zero.
fn seed() -> StateVals {
    let mut s = StateVals {
        regs: [0; 8],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    s.regs[1] = 1;
    s.regs[2] = 1;
    s
}

/// Reference: consecutive-Fibonacci pairs mod 256 starting from (1, 1).
/// `fib_pairs()[k] = (fib(k), fib(k+1))` with fib(0) = fib(1) = 1.
fn fib_pairs() -> Vec<(u8, u8)> {
    let mut pairs = vec![(1u8, 1u8)];
    for k in 0..N {
        let (a, b) = pairs[k];
        pairs.push((b, a.wrapping_add(b)));
    }
    pairs
}

// ══════════════════════════════════════════════════════════════════════
// 1. Assembler sanity: exact encodings + round-trip
// ══════════════════════════════════════════════════════════════════════

#[test]
fn assembler_encode_roundtrip() {
    // Exact bit patterns (spec §3).
    // ADD r1, r2: 1 | 0000 | 001 | 010 | 000 | 00 = 0x8140.
    assert_eq!(encode(Instr::Add { rd: 1, rs: 2 }), 0x8140);
    // EXCH r1, r2: 1 | 0011 | 001 | 010 | 000 | 00 = 0x9940.
    assert_eq!(encode(Instr::Exch { rd: 1, rs: 2 }), 0x9940);
    // ADDI r3, 0x5A: 0 | 000 | 011 | 0101_1010 | 0 = 0x06B4.
    assert_eq!(encode(Instr::Addi { rd: 3, imm: 0x5A }), 0x06B4);
    // SWAPBR r0: 1 | 1000 | 000 | 000 | 000 | 00 = 0xC000.
    assert_eq!(encode(Instr::Swapbr { rd: 0 }), 0xC000);

    // Round-trip across the whole instruction set.
    let samples = [
        Instr::Add { rd: 1, rs: 2 },
        Instr::Sub { rd: 3, rs: 4 },
        Instr::Xor { rd: 5, rs: 6 },
        Instr::Exch { rd: 1, rs: 2 },
        Instr::Rotl { rd: 7, rs: 0 },
        Instr::Rotr { rd: 0, rs: 7 },
        Instr::Cswap { rd: 1, rs: 2, rt: 3 },
        Instr::Xchg { rd: 4, sx: 5 },
        Instr::Swapbr { rd: 0 },
        Instr::Addi { rd: 0, imm: 0xFF },
        Instr::Subi { rd: 1, imm: 0x01 },
        Instr::Xori { rd: 2, imm: 0xA5 },
        Instr::Rotli { rd: 3, imm: 3 },
        Instr::Rotri { rd: 4, imm: 7 },
    ];
    for ins in samples {
        let word = encode(ins);
        assert_eq!(decode(word), Some(ins), "round-trip failed for {ins:?}");
    }

    // Pad-bit and reserved-opcode words don't decode.
    assert_eq!(decode(0x8141), None, "R-format pad bit set");
    assert_eq!(decode(0x06B5), None, "I-format pad bit set");
    // R opcode 0b1001 is now MEXCH (M-C2 Layer 1).
    assert_eq!(decode(0xC800), Some(Instr::Mexch { rd: 0, addr: 0 }), "MEXCH r0, m0");
    assert_eq!(decode(0xD100), None, "reserved R opcode 1010");
    assert_eq!(decode(0x5000), None, "reserved I opc 101");

    // assemble = per-word encode.
    let prog = fib_prog();
    let rom = assemble(&prog);
    assert_eq!(rom.len(), 2 * N);
    for (p, &ins) in prog.iter().enumerate() {
        assert_eq!(rom[p], encode(ins));
    }
}

#[test]
#[should_panic(expected = "aliased")]
fn assembler_rejects_aliased_add() {
    let _ = encode(Instr::Add { rd: 3, rs: 3 });
}

#[test]
#[should_panic(expected = "decoder-reserved")]
fn assembler_rejects_xchg_s0() {
    let _ = encode(Instr::Xchg { rd: 3, sx: 0 });
}

// ══════════════════════════════════════════════════════════════════════
// 2–4. The headline: Fibonacci forward, adjoint sanity, and backward
// ══════════════════════════════════════════════════════════════════════

#[test]
fn fib_forward() {
    let rom = assemble(&fib_prog());
    let iso = step(&rom);
    let pairs = fib_pairs();

    eprintln!("== TISC-1 Fibonacci ROM ({} words, N = {N} pairs) ==", rom.len());
    for (p, &w) in rom.iter().enumerate() {
        eprintln!("  {p:3}: {w:#06X}  {:?}", decode(w).unwrap());
    }

    let mut cur = seed();
    let mut computed = vec![cur.regs[1]];
    eprintln!("CPU fib run: (r1, r2) = ({}, {})", cur.regs[1], cur.regs[2]);
    for k in 0..N {
        // One fib pair = 2 machine cycles (ADD, then EXCH).
        cur = eval_state(&format!("fib/pair{k}/add"), &iso, &cur);
        cur = eval_state(&format!("fib/pair{k}/exch"), &iso, &cur);
        eprintln!("  after pair {:2}: (r1, r2) = ({:3}, {:3})", k + 1, cur.regs[1], cur.regs[2]);
        assert_eq!(
            (cur.regs[1], cur.regs[2]),
            pairs[k + 1],
            "pair {k}: CPU diverged from the reference Fibonacci sequence"
        );
        assert_eq!(cur.pc, (2 * (k + 1)) as u8, "PC must track the cycle count");
        assert_eq!(cur.ir, 0, "P4: IR must be zero between steps");
        assert_eq!(cur.scratch, [0; 8], "scratch must stay zero");
        computed.push(cur.regs[1]);
    }
    computed.push(cur.regs[2]);

    assert_eq!((cur.regs[1], cur.regs[2]), (89, 144), "final regs = (fib(10), fib(11))");
    eprintln!("Fibonacci sequence computed by the CPU: {computed:?}");
    assert_eq!(computed, vec![1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]);
}

#[test]
fn fib_step_adjoint_identity() {
    let rom = assemble(&fib_prog());
    let iso = step(&rom);
    let back = Iso::sym(iso.clone());
    let s = seed();
    let fwd = eval_state("adj/fwd", &iso, &s);
    assert_ne!(fwd, s, "one step must change the state");
    let rec = eval_state("adj/back", &back, &fwd);
    assert_eq!(rec, s, "one forward step then one adjoint step must be the identity");
}

#[test]
fn fib_forward_and_backward() {
    let rom = assemble(&fib_prog());
    let start = seed();
    let t = 2 * N;

    let (fwd, back) = run_and_reverse(&rom, &start, t);

    // Forward endpoint: the CPU computed fib(N), fib(N+1) in T cycles.
    assert_eq!((fwd.regs[1], fwd.regs[2]), (89, 144));
    assert_eq!(fwd.pc, t as u8);
    assert_eq!(fwd.ir, 0);
    assert_eq!(fwd.scratch, [0; 8]);

    // Backward: the machine returns EXACTLY to the seed.
    assert_eq!(
        back, start,
        "T forward + T adjoint cycles must restore the exact seed state"
    );
    eprintln!(
        "REVERSIBILITY CONFIRMED: {t} cycles forward -> (r1, r2) = ({}, {}), \
         {t} adjoint cycles -> exact seed (r1 = r2 = 1, PC = 0, IR/scratch = 0)",
        fwd.regs[1], fwd.regs[2]
    );

    // run() agrees with the forward leg of run_and_reverse().
    assert_eq!(run(&rom, &start, t), fwd);
}
