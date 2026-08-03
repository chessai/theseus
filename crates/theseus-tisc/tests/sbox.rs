//! TP-5 slice-2: PRESENT 4-bit S-box, bit-sliced over Flat(8).
//!
//! The PRESENT S-box is the 4→4-bit substitution
//! `S = [C,5,6,B,9,0,A,D,3,E,F,8,4,7,1,2]`, computed as a bit-sliced ANF
//! circuit over the 4 input bits (each a Flat(8) register = 8 parallel S-box
//! evaluations). Nonlinear gates = byte-parallel `Ccnot` (masked-AND); linear
//! = `Xor`; constant/NOT = `Xori`; output parking = `Xchg` to scratch.
//!
//! WHY PRESENT-4 and NOT AES-8 (settled by the register-pressure analysis in
//! `docs/reviews/sbox-aes-design-review.md`): a bit-sliced Boyar-Peralta AES
//! S-box has a PEAK of 30 simultaneously-live signals -- this machine has 8
//! registers + 6 usable Scratch slots + 4 Mem words = 18 addressable live
//! slots. 30 >> 18 → AES-8 does NOT fit garbage-free. PRESENT-4 fits in 6
//! registers with a single product temp and no paging.
//!
//! Rust-composed instruction ROM (`Vec<Instr>` assembled directly -- NEVER
//! enters the lowering/recognition path; no source Iso, no recognize_prim,
//! no Deferred risk).
//!
//! ## STANDALONE garbage-free framing
//!
//! The 47-instruction PRESENT S-box is **STANDALONE garbage-free** -- strictly
//! stronger than χ's garbage-free-under-adjoint-only (Rev-A).
//!
//! Because inputs r0..r3 are **never mutated**, every product temp is
//! uncomputable within the forward pass: a degree-3 term does
//! `Ccnot{r5,xi,xj}; Ccnot{acc,r5,xk}; Ccnot{r5,xi,xj}` and the second
//! `Ccnot` returns r5 to 0. After a forward-only run:
//!   - inputs r0..r3 = original values (preserved)
//!   - accumulator r4 = 0 (Xchg'd out to scratch after each output)
//!   - temp r5 = 0 (uncomputed after every cubic product)
//!   - outputs y0..y3 in scratch s2..s5
//!
//! No dirty-ancilla caveat. The `sbox_whole_domain_packed` test asserts
//! r4=0, r5=0, r0..r3=original after every run -- the L20 garbage-free
//! regression guard.
//!
//! Register map:
//!   r0..r3 = inputs x0..x3 (PRESERVED throughout)
//!   r4     = per-output accumulator (Xchg'd to scratch after each y_i)
//!   r5     = cubic-product temp (zeroed → used → uncomputed each time)
//!   s2..s5 = outputs y0..y3 (parked via Xchg)

#![allow(clippy::needless_range_loop)]

use theseus_core::Iso;
use theseus_netlist::lower_iso;
use theseus_spice::chardb::rung0_measured;
use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::run_rom_fast;
use theseus_tisc::energy::balanced_energy;
use theseus_tisc::ring_run::system_ring;
use theseus_tisc::step::step;
use theseus_tisc::types::{StateVals, MEM_WORDS};

// ── The independent golden: the PRESENT S-box TABLE (16 entries) ──────
// This constant IS the oracle. The ROM must reproduce it; we NEVER check
// the ROM against a copy of its own gate logic (L19 self-checker class).
const PRESENT_SBOX: [u8; 16] = [
    0xC, 0x5, 0x6, 0xB, 0x9, 0x0, 0xA, 0xD, 0x3, 0xE, 0xF, 0x8, 0x4, 0x7, 0x1, 0x2,
];

// Register roles.
const ACC: u8 = 4;
const TMP: u8 = 5;

// ── The S-box program (STANDALONE garbage-free, one product temp) ─────

/// One bit-sliced PRESENT S-box over Flat(8). r0..r3 = input bits x0..x3
/// (preserved); outputs y0..y3 end in scratch s2..s5; r4/r5 end at 0.
fn sbox_instrs() -> Vec<Instr> {
    let mut p = Vec::new();
    let lin = |p: &mut Vec<Instr>, i: u8| p.push(Instr::Xor { rd: ACC, rs: i });
    let quad = |p: &mut Vec<Instr>, i: u8, j: u8| {
        p.push(Instr::Ccnot {
            rd: ACC,
            rs: i,
            rt: j,
        })
    };
    // acc ^= x_i x_j x_k, uncomputing TMP back to 0 (inputs still resident).
    let cube = |p: &mut Vec<Instr>, i: u8, j: u8, k: u8| {
        p.push(Instr::Ccnot {
            rd: TMP,
            rs: i,
            rt: j,
        });
        p.push(Instr::Ccnot {
            rd: ACC,
            rs: TMP,
            rt: k,
        });
        p.push(Instr::Ccnot {
            rd: TMP,
            rs: i,
            rt: j,
        });
    };
    let const1 = |p: &mut Vec<Instr>| p.push(Instr::Xori { rd: ACC, imm: 0xFF });
    let park = |p: &mut Vec<Instr>, sx: u8| p.push(Instr::Xchg { rd: ACC, sx });

    // y0 = x0 + x2 + x1x2 + x3            → s2
    lin(&mut p, 0);
    lin(&mut p, 2);
    quad(&mut p, 1, 2);
    lin(&mut p, 3);
    park(&mut p, 2);

    // y1 = x1 + x0x1x2 + x3 + x1x3 + x0x1x3 + x2x3 + x0x2x3   → s3
    lin(&mut p, 1);
    cube(&mut p, 0, 1, 2);
    lin(&mut p, 3);
    quad(&mut p, 1, 3);
    cube(&mut p, 0, 1, 3);
    quad(&mut p, 2, 3);
    cube(&mut p, 0, 2, 3);
    park(&mut p, 3);

    // y2 = 1 + x0x1 + x2 + x3 + x0x3 + x1x3 + x0x1x3 + x0x2x3 → s4
    const1(&mut p);
    quad(&mut p, 0, 1);
    lin(&mut p, 2);
    lin(&mut p, 3);
    quad(&mut p, 0, 3);
    quad(&mut p, 1, 3);
    cube(&mut p, 0, 1, 3);
    cube(&mut p, 0, 2, 3);
    park(&mut p, 4);

    // y3 = 1 + x0 + x1 + x1x2 + x0x1x2 + x3 + x0x1x3 + x0x2x3 → s5
    const1(&mut p);
    lin(&mut p, 0);
    lin(&mut p, 1);
    quad(&mut p, 1, 2);
    cube(&mut p, 0, 1, 2);
    lin(&mut p, 3);
    cube(&mut p, 0, 1, 3);
    cube(&mut p, 0, 2, 3);
    park(&mut p, 5);

    p
}

// ── State helpers ────────────────────────────────────────────────────

fn empty_state() -> StateVals {
    StateVals {
        regs: [0; 8],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    }
}

/// Pack the SAME 4-bit input identically into all 8 bits of each of r0..r3.
fn state_packed(x: u8) -> StateVals {
    let mut s = empty_state();
    for i in 0..4 {
        s.regs[i] = if (x >> i) & 1 == 1 { 0xFF } else { 0x00 };
    }
    s
}

/// Reconstruct the 4-bit output from bit `b` of the parked scratch bytes.
fn out_nibble(s: &StateVals, b: u8) -> u8 {
    let mut y = 0u8;
    for i in 0..4 {
        if (s.scratch[2 + i] >> b) & 1 == 1 {
            y |= 1 << i;
        }
    }
    y
}

// ══════════════════════════════════════════════════════════════════════
// FAST TIER (5 tests -- default gate: `cargo test -p theseus-tisc --test sbox`)
// ══════════════════════════════════════════════════════════════════════

// ── 1. Whole per-byte domain: all 16 inputs packed ×8, run_rom == TABLE ──

#[test]
fn sbox_whole_domain_packed() {
    let rom = assemble(&sbox_instrs()).expect("assemble S-box");
    let budget = sbox_instrs().len();

    for x in 0u8..16 {
        let out = run_rom_fast(&rom, state_packed(x), budget);

        // Same input in all 8 lanes → all 8 nibbles equal the table entry.
        for b in 0..8 {
            assert_eq!(
                out_nibble(&out, b),
                PRESENT_SBOX[x as usize],
                "x={x:#04X} lane {b}: ROM {:#04X} != table {:#04X}",
                out_nibble(&out, b),
                PRESENT_SBOX[x as usize]
            );
        }
        // Whole parked bytes are 0x00/0xFF (uniform packing).
        for i in 0..4 {
            let want = if (PRESENT_SBOX[x as usize] >> i) & 1 == 1 {
                0xFF
            } else {
                0x00
            };
            assert_eq!(out.scratch[2 + i], want, "x={x:#04X} output byte y{i}");
        }
        // STANDALONE garbage-free: inputs preserved, acc + temp back to 0.
        for i in 0..4 {
            let inp = if (x >> i) & 1 == 1 { 0xFF } else { 0x00 };
            assert_eq!(out.regs[i], inp, "x={x:#04X}: input r{i} must be preserved");
        }
        assert_eq!(out.regs[ACC as usize], 0, "x={x:#04X}: acc r4 not clean");
        assert_eq!(out.regs[TMP as usize], 0, "x={x:#04X}: temp r5 not clean");
    }
    eprintln!(
        "PRESENT S-box correct on all 16 inputs (packed x8); inputs preserved, \
         acc+temp clean → STANDALONE garbage-free."
    );
}

// ── 2. Discriminating packing: 8 DIFFERENT inputs across the 8 bit positions ──

#[test]
fn sbox_discriminating_packing() {
    let rom = assemble(&sbox_instrs()).expect("assemble S-box");
    let budget = sbox_instrs().len();

    // 8 distinct 4-bit inputs, one per bit position.
    let inputs: [u8; 8] = [0x0, 0xF, 0xA, 0x5, 0x3, 0xC, 0x9, 0x6];

    // lane i, bit b = bit i of inputs[b].
    let mut s = empty_state();
    for i in 0..4 {
        for b in 0..8 {
            if (inputs[b] >> i) & 1 == 1 {
                s.regs[i] |= 1 << b;
            }
        }
    }
    let out = run_rom_fast(&rom, s, budget);

    // Each bit position independently equals its own table entry.
    for b in 0..8u8 {
        let inp = inputs[b as usize];
        assert_eq!(
            out_nibble(&out, b),
            PRESENT_SBOX[inp as usize],
            "discriminating: lane {b} input {inp:#04X} → got {:#04X}, table {:#04X}",
            out_nibble(&out, b),
            PRESENT_SBOX[inp as usize]
        );
    }
    eprintln!(
        "PRESENT discriminating packing: 8 distinct inputs computed independently in one ROM run."
    );
}

// ── 3. Golden vectors (independent, pinned from the published table) ──

#[test]
fn sbox_golden_vectors() {
    // Pinned PRESENT golden entries (published S-box), not derived from the ROM.
    const GOLDENS: [(u8, u8); 4] = [(0x0, 0xC), (0x7, 0xD), (0xA, 0xF), (0xF, 0x2)];
    for (x, y) in GOLDENS {
        assert_eq!(
            PRESENT_SBOX[x as usize], y,
            "table must match published golden S({x:#X})={y:#X}"
        );
    }
    let rom = assemble(&sbox_instrs()).expect("assemble S-box");
    let budget = sbox_instrs().len();
    for (x, y) in GOLDENS {
        let out = run_rom_fast(&rom, state_packed(x), budget);
        assert_eq!(
            out_nibble(&out, 0),
            y,
            "golden ROM S({x:#X}) expected {y:#X}"
        );
    }
    eprintln!("PRESENT golden vectors S(0)=C, S(7)=D, S(A)=F, S(F)=2: ROM matches.");
}

// ── 4. Bijection: the S-box is a permutation of 0..16 ──

#[test]
fn sbox_forward_bijective() {
    let rom = assemble(&sbox_instrs()).expect("assemble S-box");
    let budget = sbox_instrs().len();
    use std::collections::HashSet;
    let mut seen = HashSet::new();
    for x in 0u8..16 {
        let out = run_rom_fast(&rom, state_packed(x), budget);
        let y = out_nibble(&out, 0);
        assert!(
            seen.insert(y),
            "collision: S maps two inputs to {y:#X} -- not a permutation"
        );
    }
    assert_eq!(
        seen.len(),
        16,
        "S-box must be a bijection on all 16 nibbles"
    );
    eprintln!("PRESENT S-box is a bijection over all 16 nibbles (permutation).");
}

// ── 5. Instruction / gate-inventory pin (perf accounting) ──

#[test]
fn sbox_instruction_inventory() {
    let instrs = sbox_instrs();
    let ccnots = instrs
        .iter()
        .filter(|i| matches!(i, Instr::Ccnot { .. }))
        .count();
    let xors = instrs
        .iter()
        .filter(|i| matches!(i, Instr::Xor { .. }))
        .count();
    let xoris = instrs
        .iter()
        .filter(|i| matches!(i, Instr::Xori { .. }))
        .count();
    let xchgs = instrs
        .iter()
        .filter(|i| matches!(i, Instr::Xchg { .. }))
        .count();
    assert_eq!(instrs.len(), 47, "PRESENT S-box schedule length");
    assert_eq!(
        ccnots, 31,
        "31 byte-parallel AND-gates (quadratic + cubic products)"
    );
    assert_eq!(xors, 10, "10 linear XOR terms");
    assert_eq!(xoris, 2, "2 constant-1 (NOT) terms (y2, y3)");
    assert_eq!(xchgs, 4, "4 output parks to scratch");
    eprintln!(
        "PRESENT inventory: {} instrs = {ccnots} Ccnot + {xors} Xor + {xoris} Xori + {xchgs} Xchg. \
         (A factored circuit sharing subproducts could cut the Ccnot count; not needed for this slice.)",
        instrs.len()
    );
}

// ══════════════════════════════════════════════════════════════════════
// HEAVY TIER (3 tests -- `cargo test -p theseus-tisc --test sbox -- --ignored --test-threads=1`)
// ══════════════════════════════════════════════════════════════════════

// ── 6. Reversibility: forward ROM then adjoint restores input + temps ──

#[test]
#[ignore = "HEAVY: eval-based reversibility (step-iso); run via --ignored / advisor pre-merge"]
fn sbox_reversible_roundtrip_eval() {
    let rom = assemble(&sbox_instrs()).expect("assemble S-box");
    let iso = step(&rom);
    let back = Iso::sym(iso.clone());
    let t = sbox_instrs().len();

    // Discriminating packed byte across r0..r3.
    let mut start = empty_state();
    start.regs[0] = 0xA5;
    start.regs[1] = 0x3C;
    start.regs[2] = 0xF0;
    start.regs[3] = 0x69;

    let mut cur = start.encode();
    let mut fuel = 8_000_000_000u64;
    for _ in 0..t {
        cur = theseus_eval::eval(&iso, cur, &mut fuel).expect("fwd");
    }
    let fwd = StateVals::decode(&cur);
    // Transform correct per bit-lane vs the table.
    for b in 0..8 {
        let x = ((start.regs[0] >> b) & 1)
            | (((start.regs[1] >> b) & 1) << 1)
            | (((start.regs[2] >> b) & 1) << 2)
            | (((start.regs[3] >> b) & 1) << 3);
        assert_eq!(
            out_nibble(&fwd, b),
            PRESENT_SBOX[x as usize],
            "fwd lane {b}"
        );
    }
    // Backward t cycles restores EVERYTHING (inputs + temps + parked scratch).
    for _ in 0..t {
        cur = theseus_eval::eval(&back, cur, &mut fuel).expect("back");
    }
    let restored = StateVals::decode(&cur);
    assert_eq!(
        restored, start,
        "adjoint must restore the exact input state"
    );
    eprintln!(
        "PRESENT ROM reversible: {t} fwd then {t} adjoint cycles restore input + temps + scratch."
    );
}

// ── 7. Energy / perf-win: price the S-box ROM; 8× bit-slicing ──

#[test]
#[ignore = "HEAVY: energy lowering (lower_iso step net) is slow; run via --ignored / advisor pre-merge"]
fn sbox_energy_and_bitslicing() {
    let instrs = sbox_instrs();
    let rom = assemble(&instrs).expect("assemble S-box");
    let db = rung0_measured();
    let t_ramp = 1e-7;

    let step_net = lower_iso(&step(&rom)).expect("lower step(S-box rom)");
    let step_bd = balanced_energy(&step_net, &db, t_ramp).expect("energy");
    let step_j = step_bd.total_j;
    let b = rom.len(); // straight-line: B = ROM length (PC += 1 each cycle)
    let total_j = step_j * b as f64;

    // Data-independent model: the SAME ROM/energy computes 8 S-boxes at once.
    let j_per_sbox_sliced = total_j / 8.0;
    let j_per_sbox_unsliced = total_j / 1.0;

    let ccnots = instrs
        .iter()
        .filter(|i| matches!(i, Instr::Ccnot { .. }))
        .count();
    eprintln!(
        "== PRESENT S-box ROM energy ({} instrs, {ccnots} Ccnots) ==",
        instrs.len()
    );
    eprintln!("  step_j = {step_j:.4e} J/cycle;  B = {b} cycles;  total = {total_j:.4e} J");
    eprintln!("  bit-sliced (Flat8, 8 evals): {j_per_sbox_sliced:.4e} J / S-box");
    eprintln!("  un-sliced  (Flat1, 1 eval):  {j_per_sbox_unsliced:.4e} J / S-box");
    eprintln!("  BIT-SLICING WIN: 8x throughput for the same ROM energy.");
    assert!(step_j > 0.0);
}

// ── 8. System-ring L19 authority: real closed-ring CPU == table ──

#[test]
#[ignore = "HEAVY: system-ring authority (L19 real-run); run via --ignored / advisor pre-merge"]
fn sbox_system_ring_authority() {
    let rom = assemble(&sbox_instrs()).expect("assemble S-box");
    let revolutions = rom.len() as u32; // one revolution == one instruction cycle
    let ring = system_ring(&rom);

    // Per-instruction closure certificate on representative start states:
    // the FIRST step of the ring genuinely closes (ancillas restored, bus quiescent).
    for &x in &[0x0u8, 0x5, 0xA, 0xF] {
        let start = state_packed(x);
        assert!(
            ring.ancillas_restored(&start),
            "x={x:#04X}: ring ancillas not restored"
        );
        assert!(ring.bus_quiescent(&start), "x={x:#04X}: bus not quiescent");
    }

    // Whole-program authority: run all 47 revolutions, output == published table.
    // A few discriminating inputs (whole-domain is proven fast-tier by run_rom_fast).
    for &x in &[0x0u8, 0x3, 0x7, 0xA, 0xC, 0xF] {
        let out = ring.run(&state_packed(x), revolutions);
        assert_eq!(
            out_nibble(&out, 0),
            PRESENT_SBOX[x as usize],
            "system-ring: S({x:#04X}) real-CPU {:#04X} != table {:#04X}",
            out_nibble(&out, 0),
            PRESENT_SBOX[x as usize]
        );
        // Inputs preserved on the real ring too.
        for i in 0..4 {
            let inp = if (x >> i) & 1 == 1 { 0xFF } else { 0x00 };
            assert_eq!(
                out.regs[i], inp,
                "system-ring x={x:#04X}: input r{i} not preserved"
            );
        }
    }
    eprintln!(
        "PRESENT system-ring authority: closed real CPU reproduces the table over \
         6 inputs ({revolutions}-revolution program); ancillas restored + bus quiescent."
    );
}
