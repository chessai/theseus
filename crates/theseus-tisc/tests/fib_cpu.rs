//! M4 FUNCTIONAL HEADLINE: the TISC-1 CPU runs a real program --
//! Fibonacci -- forward AND backward.
//!
//! The program is assembled from typed mnemonics ([`theseus_tisc::asm`])
//! into a ROM, and executed by iterating the machine-cycle iso
//! `step(rom) : State <-> State` via core eval (no lowering -- this is
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
//!      -- the CPU is reversible end to end.

use theseus_core::Iso;
use theseus_tisc::asm::{assemble, decode, encode, AsmError, Instr};
use theseus_tisc::step::step;
use theseus_tisc::types::{StateVals, MEM_WORDS};

mod common;
use common::eval_state;

/// Generous per-eval fuel (same order as the step tests).
const EVAL_FUEL: u64 = 8_000_000_000;

/// Fibonacci pairs unrolled in the ROM (T = 2N machine cycles).
const N: usize = 10;

// ── Run harness ────────────────────────────────────────────────────

/// Apply `step(rom)` `t` times: run the machine `t` cycles forward.
fn run(rom: &[u32], start: &StateVals, t: usize) -> StateVals {
    let iso = step(rom);
    let mut cur = start.clone();
    for k in 0..t {
        cur = eval_state(&format!("run/fwd[{k}]"), &iso, &cur, EVAL_FUEL);
    }
    cur
}

/// Run `t` cycles forward, then `t` cycles of the adjoint
/// `Iso::sym(step(rom))`. Returns (forward endpoint, state after the
/// backward run); the latter must equal `start` (reversibility).
fn run_and_reverse(rom: &[u32], start: &StateVals, t: usize) -> (StateVals, StateVals) {
    let iso = step(rom);
    let back = Iso::sym(iso.clone());
    let mut cur = start.clone();
    for k in 0..t {
        cur = eval_state(&format!("rr/fwd[{k}]"), &iso, &cur, EVAL_FUEL);
    }
    let fwd = cur.clone();
    for k in (0..t).rev() {
        cur = eval_state(&format!("rr/back[{k}]"), &back, &cur, EVAL_FUEL);
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
    // Exact bit patterns (A-0 32-bit word):
    //   R: fmt(31) | opcode[30:25] | rd[24:20] | rs[19:15] | rt[14:10] | spare[9:0]
    //   I: fmt(31) | opcode[30:25] | rd[24:20] | imm[19:0]
    // ADD r1, r2: fmt=1, op=0, rd=1, rs=2 = 0x8011_0000.
    assert_eq!(encode(Instr::Add { rd: 1, rs: 2 }).unwrap(), 0x8011_0000);
    // EXCH r1, r2: fmt=1, op=3, rd=1, rs=2 = 0x8611_0000.
    assert_eq!(encode(Instr::Exch { rd: 1, rs: 2 }).unwrap(), 0x8611_0000);
    // ADDI r3, 0x5A: fmt=0, op=0, rd=3, imm=0x5A = 0x0030_005A.
    assert_eq!(
        encode(Instr::Addi { rd: 3, imm: 0x5A }).unwrap(),
        0x0030_005A
    );
    // SWAPBR r0: fmt=1, op=8, rd=0 = 0x9000_0000.
    assert_eq!(encode(Instr::Swapbr { rd: 0, rs: 0 }).unwrap(), 0x9000_0000);

    // Round-trip across the whole instruction set.
    let samples = [
        Instr::Add { rd: 1, rs: 2 },
        Instr::Sub { rd: 3, rs: 4 },
        Instr::Xor { rd: 5, rs: 6 },
        Instr::Exch { rd: 1, rs: 2 },
        Instr::Rotl { rd: 7, rs: 0 },
        Instr::Rotr { rd: 0, rs: 7 },
        Instr::Cswap {
            rd: 1,
            rs: 2,
            rt: 3,
        },
        Instr::Xchg { rd: 4, sx: 5 },
        Instr::Swapbr { rd: 0, rs: 0 },
        Instr::Addi { rd: 0, imm: 0xFF },
        Instr::Subi { rd: 1, imm: 0x01 },
        Instr::Xori { rd: 2, imm: 0xA5 },
        Instr::Rotli { rd: 3, imm: 3 },
        Instr::Rotri { rd: 4, imm: 7 },
    ];
    for ins in samples {
        let word = encode(ins).unwrap();
        assert_eq!(decode(word), Some(ins), "round-trip failed for {ins:?}");
    }

    // R-format spare bits (bits 9:0) must be zero, else decode → None.
    // ADD r1,r2 with spare bit 0 set = 0x8011_0001.
    assert_eq!(decode(0x8011_0001), None, "R-format spare bit set");
    // R opcode 0b1001 is now MEXCH (M-C2 Layer 1): fmt=1, op=9, rd=0 = 0x9200_0000.
    assert_eq!(
        decode(0x9200_0000),
        Some(Instr::Mexch { rd: 0, addr: 0 }),
        "MEXCH r0, m0"
    );
    // R opcode 0b1010 is MEXCHR (M-D.b): fmt=1, op=10, rd=1, rs=0 = 0x9410_0000.
    assert_eq!(
        decode(0x9410_0000),
        Some(Instr::MexchReg { rd: 1, ra: 0 }),
        "R opcode 1010 is now MEXCHR (M-D.b)"
    );
    // Reserved I-format opcode 0b101 (=5): fmt=0, op=5 = 0x0A00_0000.
    assert_eq!(decode(0x0A00_0000), None, "reserved I opc 101");

    // assemble = per-word encode.
    let prog = fib_prog();
    let rom = assemble(&prog).expect("valid program");
    assert_eq!(rom.len(), 2 * N);
    for (p, &ins) in prog.iter().enumerate() {
        assert_eq!(rom[p], encode(ins).unwrap());
    }
}

#[test]
fn assembler_rejects_aliased_add() {
    assert!(matches!(
        encode(Instr::Add { rd: 3, rs: 3 }),
        Err(AsmError::AliasedOperands {
            mnemonic: "ADD",
            rd: 3,
            rs: 3
        })
    ));
}

#[test]
fn assembler_rejects_xchg_s0() {
    assert!(matches!(
        encode(Instr::Xchg { rd: 3, sx: 0 }),
        Err(AsmError::XchgScratchReserved { sx: 0 })
    ));
}

// ══════════════════════════════════════════════════════════════════════
// 2-4. The headline: Fibonacci forward, adjoint sanity, and backward
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "M-C2 L1: slow at wider State (resident-memory area cost); green pre-merge via --ignored"]
fn fib_forward() {
    let rom = assemble(&fib_prog()).expect("valid program");
    let iso = step(&rom);
    let pairs = fib_pairs();

    eprintln!(
        "== TISC-1 Fibonacci ROM ({} words, N = {N} pairs) ==",
        rom.len()
    );
    for (p, &w) in rom.iter().enumerate() {
        eprintln!("  {p:3}: {w:#06X}  {:?}", decode(w).unwrap());
    }

    let mut cur = seed();
    let mut computed = vec![cur.regs[1]];
    eprintln!("CPU fib run: (r1, r2) = ({}, {})", cur.regs[1], cur.regs[2]);
    for k in 0..N {
        // One fib pair = 2 machine cycles (ADD, then EXCH).
        cur = eval_state(&format!("fib/pair{k}/add"), &iso, &cur, EVAL_FUEL);
        cur = eval_state(&format!("fib/pair{k}/exch"), &iso, &cur, EVAL_FUEL);
        eprintln!(
            "  after pair {:2}: (r1, r2) = ({:3}, {:3})",
            k + 1,
            cur.regs[1],
            cur.regs[2]
        );
        assert_eq!(
            (cur.regs[1], cur.regs[2]),
            pairs[k + 1],
            "pair {k}: CPU diverged from the reference Fibonacci sequence"
        );
        assert_eq!(
            cur.pc,
            (2 * (k + 1)) as u16,
            "PC must track the cycle count"
        );
        assert_eq!(cur.ir, 0, "P4: IR must be zero between steps");
        assert_eq!(cur.scratch, [0; 8], "scratch must stay zero");
        computed.push(cur.regs[1]);
    }
    computed.push(cur.regs[2]);

    assert_eq!(
        (cur.regs[1], cur.regs[2]),
        (89, 144),
        "final regs = (fib(10), fib(11))"
    );
    eprintln!("Fibonacci sequence computed by the CPU: {computed:?}");
    assert_eq!(computed, vec![1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]);
}

#[test]
#[ignore = "M-C2 L1: slow at wider State (resident-memory area cost); green pre-merge via --ignored"]
fn fib_step_adjoint_identity() {
    let rom = assemble(&fib_prog()).expect("valid program");
    let iso = step(&rom);
    let back = Iso::sym(iso.clone());
    let s = seed();
    let fwd = eval_state("adj/fwd", &iso, &s, EVAL_FUEL);
    assert_ne!(fwd, s, "one step must change the state");
    let rec = eval_state("adj/back", &back, &fwd, EVAL_FUEL);
    assert_eq!(
        rec, s,
        "one forward step then one adjoint step must be the identity"
    );
}

#[test]
#[ignore = "M-C2 L1: slow at wider State (resident-memory area cost); green pre-merge via --ignored"]
fn fib_forward_and_backward() {
    let rom = assemble(&fib_prog()).expect("valid program");
    let start = seed();
    let t = 2 * N;

    let (fwd, back) = run_and_reverse(&rom, &start, t);

    // Forward endpoint: the CPU computed fib(N), fib(N+1) in T cycles.
    assert_eq!((fwd.regs[1], fwd.regs[2]), (89, 144));
    assert_eq!(fwd.pc, t as u16);
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
