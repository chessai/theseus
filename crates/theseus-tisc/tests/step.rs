//! Differential-oracle tests for the TISC-1 `step` iso
//! (`step(rom) : State <-> State`, tisc-1-spec §4).
//!
//! A Rust reference emulator `ref_step` (fetch rom[pc] → apply the
//! instruction's semantics → pc = pc + 1 mod 256) is the oracle. For
//! small straight-line test ROMs and sampled States we check:
//!
//!   1. lowering succeeds, netlist valid, max cell kind ≤ K3 — and the
//!      per-step cell counts are PRINTED (the M4 unroll-planning number);
//!   2. faithfulness: balance-then-simulate == bits of core eval;
//!   3. semantics: decode(eval(step, encode(s))) == ref_step(rom, s);
//!   4. reversibility: sym(step) recovers the input, and a T-step
//!      forward run followed by T adjoint steps returns to the start
//!      (the M4 headline capability at small scale).
//!
//! Sampling constraints (decoder/step conventions): ir = 0, s0 = 0,
//! s1 = 0 on entry; ROM words avoid register aliasing and the
//! decoder-reserved scratch (s0/s1) in XCHG.
//!
//! SWAPBR: per the documented step-level limitation (see step.rs module
//! docs — the spec semantics are non-injective on the full State space)
//! a SWAPBR step is PC-advance-only; a dedicated test pins that down.

use std::sync::OnceLock;

use theseus_core::{type_of, Iso, Type};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{
    balance, bits_of, lower_iso, rail_width, simulate, CellKind, PhasedCellNetlist,
};
use theseus_tisc::step::{fetch, pc_inc, step};
use theseus_tisc::types::StateVals;

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

// ── Reference emulator ─────────────────────────────────────────────

/// Apply the semantics of one instruction word to the register state.
/// SWAPBR is a no-op at step level (documented limitation). Reserved
/// opcodes and word 0 are no-ops given s0 = 0 (asserted in ref_step).
fn ref_instr(word: u16, s: &mut StateVals) {
    if word & 0x8000 != 0 {
        let opcode = (word >> 11) & 0xF;
        let rd = ((word >> 8) & 7) as usize;
        let rs = ((word >> 5) & 7) as usize;
        let rt = ((word >> 2) & 7) as usize;
        match opcode {
            0b0000 => s.regs[rd] = s.regs[rd].wrapping_add(s.regs[rs]),
            0b0001 => s.regs[rd] = s.regs[rd].wrapping_sub(s.regs[rs]),
            0b0010 => s.regs[rd] ^= s.regs[rs],
            0b0011 => s.regs.swap(rd, rs),
            0b0100 => s.regs[rd] = s.regs[rd].rotate_left(u32::from(s.regs[rs] & 7)),
            0b0101 => s.regs[rd] = s.regs[rd].rotate_right(u32::from(s.regs[rs] & 7)),
            0b0110 => {
                if s.regs[rd] & 1 == 1 {
                    s.regs.swap(rs, rt);
                }
            }
            0b0111 => std::mem::swap(&mut s.regs[rd], &mut s.scratch[rt]),
            0b1000 => {} // SWAPBR: step-level no-op (see step.rs docs)
            _ => {}      // reserved
        }
    } else {
        let opc = (word >> 12) & 7;
        let rd = ((word >> 9) & 7) as usize;
        let imm = ((word >> 1) & 0xFF) as u8;
        match opc {
            0b000 => s.regs[rd] = s.regs[rd].wrapping_add(imm),
            0b001 => s.regs[rd] = s.regs[rd].wrapping_sub(imm),
            0b010 => s.regs[rd] ^= imm,
            0b011 => s.regs[rd] = s.regs[rd].rotate_left(u32::from(imm & 7)),
            0b100 => s.regs[rd] = s.regs[rd].rotate_right(u32::from(imm & 7)),
            _ => {} // reserved
        }
    }
}

/// One reference machine cycle: fetch rom[pc] (0 out of range), apply
/// it, advance PC. IR stays 0 across the step.
fn ref_step(rom: &[u16], s: &StateVals) -> StateVals {
    assert_eq!(s.ir, 0, "P4: IR must be 0 between steps");
    assert_eq!(s.scratch[0], 0, "s0 convention (ADDI / IR=0 degeneracy)");
    assert_eq!(s.scratch[1], 0, "s1 reserved-zero convention");
    let mut out = s.clone();
    let word = rom.get(s.pc as usize).copied().unwrap_or(0);
    ref_instr(word, &mut out);
    out.pc = s.pc.wrapping_add(1);
    out
}

// ── Test ROMs (straight-line, no aliasing, no s0/s1 in XCHG) ───────

/// Primary ROM: 8 instructions covering ADDI, ADD, XOR, ROTL, EXCH,
/// SUBI, CSWAP, ROTRI at addresses 0..8. This is the ROM the lowered /
/// balanced netlist is built for.
fn rom1() -> Vec<u16> {
    vec![
        i_ins(0, 1, 0x05),  // 0: ADDI r1, 5
        r_ins(0, 2, 1, 0),  // 1: ADD  r2, r1
        r_ins(2, 3, 2, 0),  // 2: XOR  r3, r2
        r_ins(4, 4, 1, 0),  // 3: ROTL r4, r1   (amount = r1 & 7)
        r_ins(3, 5, 0, 0),  // 4: EXCH r5, r0
        i_ins(1, 6, 0x11),  // 5: SUBI r6, 0x11
        r_ins(6, 7, 2, 3),  // 6: CSWAP r7, r2, r3  (cond on r7[0])
        i_ins(4, 0, 0x02),  // 7: ROTRI r0, 2
    ]
}

/// Secondary ROM (eval-only, no lowering): SUB, ROTR, XORI, XCHG.
fn rom2() -> Vec<u16> {
    vec![
        r_ins(1, 1, 4, 0),  // 0: SUB  r1, r4
        r_ins(5, 2, 5, 0),  // 1: ROTR r2, r5
        i_ins(2, 3, 0x5A),  // 2: XORI r3, 0x5A
        r_ins(7, 6, 0, 3),  // 3: XCHG r6, s3
    ]
}

// ── Sample states (ir = 0, s0 = s1 = 0) ────────────────────────────

fn st(regs: [u8; 8], pc: u8) -> StateVals {
    StateVals {
        regs,
        pc,
        br: 0x99,
        ir: 0,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
    }
}

fn regs_a() -> [u8; 8] {
    [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3]
}

fn regs_b() -> [u8; 8] {
    [0x10, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76, 0x87]
}

// ── Eval helper ────────────────────────────────────────────────────

fn eval_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

// ══════════════════════════════════════════════════════════════════════
// 1. Phase isolation: fetch and pc_inc
// ══════════════════════════════════════════════════════════════════════

#[test]
fn fetch_isolated() {
    let rom = rom1();
    let iso = fetch(&rom);

    let net = lower_iso(&iso).expect("fetch lowers");
    assert_eq!(net.validate(), Ok(()));
    let counts = net.cell_counts();
    let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
    let total: usize = counts.values().sum();
    eprintln!("fetch(rom1, len 8): {total} cells, max kind {max_kind:?}, counts {counts:?}");
    assert!(max_kind <= CellKind::K3);

    // Semantics: IR <- rom[pc] at every in-range pc, IR untouched out
    // of range; everything else preserved.
    for pc in [0u8, 1, 2, 3, 4, 5, 6, 7, 8, 42, 255] {
        let s = st(regs_a(), pc);
        let got = eval_state("fetch", &iso, &s);
        let mut expected = s.clone();
        expected.ir = rom.get(pc as usize).copied().unwrap_or(0);
        assert_eq!(got, expected, "fetch: pc={pc}");
    }

    // Involution: fetch ; fetch == identity (unfetch is the same iso).
    let twice = Iso::seq(iso.clone(), iso);
    let s = st(regs_b(), 3);
    assert_eq!(eval_state("fetch twice", &twice, &s), s);
}

#[test]
fn pc_inc_isolated() {
    let iso = pc_inc();

    let net = lower_iso(&iso).expect("pc_inc lowers");
    assert_eq!(net.validate(), Ok(()));
    let counts = net.cell_counts();
    let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
    let total: usize = counts.values().sum();
    eprintln!("pc_inc: {total} cells, max kind {max_kind:?}, counts {counts:?}");
    assert!(max_kind <= CellKind::K3);

    for pc in [0u8, 1, 2, 7, 8, 0x0F, 0x7F, 0x80, 0xFE, 0xFF] {
        let s = st(regs_a(), pc);
        let got = eval_state("pc_inc", &iso, &s);
        let mut expected = s.clone();
        expected.pc = pc.wrapping_add(1);
        assert_eq!(got, expected, "pc_inc: pc={pc}");

        // Adjoint = decrement.
        let back = eval_state("pc_dec", &Iso::sym(iso.clone()), &got);
        assert_eq!(back, s, "pc_inc adjoint: pc={pc}");
    }
}

// ══════════════════════════════════════════════════════════════════════
// 2. The full step: lower once (rom1), verify samples
// ══════════════════════════════════════════════════════════════════════

/// Light context: the rom1 step iso only (for eval-only tests, which
/// must not pay the multi-minute lowering of the heavy ctx).
fn step1() -> &'static (Vec<u16>, Iso) {
    static S: OnceLock<(Vec<u16>, Iso)> = OnceLock::new();
    S.get_or_init(|| {
        let rom = rom1();
        let iso = step(&rom);
        (rom, iso)
    })
}

struct Ctx {
    rom: Vec<u16>,
    iso: Iso,
    in_ty: Type,
    out_ty: Type,
    n_data_out: usize,
    balanced: PhasedCellNetlist,
    ancilla: Vec<bool>,
}

fn ctx() -> &'static Ctx {
    static CTX: OnceLock<Ctx> = OnceLock::new();
    CTX.get_or_init(|| {
        let rom = rom1();
        let t0 = std::time::Instant::now();
        let iso = step(&rom);
        let (in_ty, out_ty) = type_of(&iso).expect("step types");
        eprintln!("step(rom1): built + typed in {:?}", t0.elapsed());

        let t1 = std::time::Instant::now();
        let (net, ancilla) = lower_iso_with_ancillas(&iso).expect("step lowers");
        eprintln!("step(rom1): lowered in {:?}", t1.elapsed());
        assert_eq!(net.validate(), Ok(()), "step netlist invalid");

        let counts = net.cell_counts();
        let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
        let total: usize = counts.values().sum();
        eprintln!("== PER-STEP COST (rom1, 8 words) ==");
        eprintln!("step: {total} cells, max kind {max_kind:?}, counts {counts:?}");
        assert!(max_kind <= CellKind::K3, "step: max kind {max_kind:?} > K3");

        let n_data = rail_width(&in_ty).unwrap();
        assert_eq!(net.inputs.len(), n_data + ancilla.len());

        let t2 = std::time::Instant::now();
        let balanced = balance(&net).expect("step balances");
        eprintln!("step(rom1): balanced in {:?}", t2.elapsed());
        assert_eq!(balanced.validate(), Ok(()), "balanced netlist invalid");
        assert!(
            balanced.tick_imbalances().unwrap().is_empty(),
            "balanced netlist has tick imbalances"
        );
        eprintln!("step balanced counts {:?}", balanced.cell_counts());

        let n_data_out = rail_width(&out_ty).unwrap();
        Ctx { rom, iso, in_ty, out_ty, n_data_out, balanced, ancilla }
    })
}

/// Semantics (eval vs ref_step) + faithfulness (balanced simulate vs
/// eval) on one sample.
fn check_full(name: &str, s: &StateVals) {
    let c = ctx();
    let val = s.encode();
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(&c.iso, val.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));

    let got = StateVals::decode(&ev);
    let expected = ref_step(&c.rom, s);
    assert_eq!(
        got, expected,
        "{name}: step semantics mismatch\n  got:      {got:?}\n  expected: {expected:?}"
    );
    assert_eq!(got.ir, 0, "{name}: P4 violated — IR not re-zeroed");
    assert_eq!(got.scratch[1], 0, "{name}: s1 not restored");

    let mut input_bits = bits_of(&val, &c.in_ty).unwrap();
    input_bits.extend_from_slice(&c.ancilla);
    let sim = simulate(&c.balanced, &input_bits)
        .unwrap_or_else(|e| panic!("{name}: simulate failed: {e:?}"));
    let expected_bits = bits_of(&ev, &c.out_ty).unwrap();
    assert_eq!(
        &sim.outputs[..c.n_data_out],
        &expected_bits[..],
        "{name}: balanced simulate disagrees with core eval"
    );
}

#[test]
fn step_lowering_within_budget() {
    // Building ctx() performs the lowering, K3 assertion, balancing,
    // and prints the per-step cell counts (the unroll-planning number).
    let _ = ctx();
}

#[test]
fn step_semantics_and_faithfulness() {
    // Each in-ROM pc exercises a different instruction of rom1.
    for pc in 0..8u8 {
        check_full(&format!("rom1/pc={pc}/a"), &st(regs_a(), pc));
    }
    // Second register file on a subset (simulate is the slow part).
    for pc in [1u8, 3, 6] {
        check_full(&format!("rom1/pc={pc}/b"), &st(regs_b(), pc));
    }
    // Out-of-ROM pc: no-op + PC advance (word 0, s0 = 0). PC wrap too.
    check_full("rom1/pc=200", &st(regs_a(), 200));
    check_full("rom1/pc=255-wrap", &st(regs_b(), 255));
}

// ══════════════════════════════════════════════════════════════════════
// 3. Second ROM, eval-only (no lowering): SUB/ROTR/XORI/XCHG
// ══════════════════════════════════════════════════════════════════════

#[test]
fn step_semantics_rom2_eval_only() {
    let rom = rom2();
    let iso = step(&rom);
    for pc in 0..4u8 {
        for regs in [regs_a(), regs_b()] {
            let s = st(regs, pc);
            let got = eval_state(&format!("rom2/pc={pc}"), &iso, &s);
            assert_eq!(got, ref_step(&rom, &s), "rom2: pc={pc}");
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 4. Reversibility: adjoint recovery + forward-then-backward run
// ══════════════════════════════════════════════════════════════════════

#[test]
fn step_adjoint_recovers_input() {
    let (_, iso) = step1();
    let back = Iso::sym(iso.clone());
    for pc in [0u8, 3, 6] {
        let s = st(regs_a(), pc);
        let fwd = eval_state("adjoint/fwd", iso, &s);
        let rec = eval_state("adjoint/back", &back, &fwd);
        assert_eq!(rec, s, "sym(step) must recover the input (pc={pc})");
    }
}

#[test]
fn forward_then_backward_run() {
    // The M4 headline capability at small scale: run the whole rom1
    // program (T = 8 steps) forward, tracking the reference emulator at
    // every step, then T adjoint steps back to the exact start state.
    let (rom, iso) = step1();
    let back = Iso::sym(iso.clone());
    const T: usize = 8;

    let start = st(regs_b(), 0);
    let mut cur = start.clone();
    let mut refs = vec![start.clone()];
    for t in 0..T {
        let next = eval_state(&format!("run/fwd[{t}]"), iso, &cur);
        let expected = ref_step(rom, &cur);
        assert_eq!(next, expected, "forward run diverged from reference at step {t}");
        refs.push(next.clone());
        cur = next;
    }
    assert_eq!(cur.pc, T as u8, "PC must have advanced T times");

    for t in (0..T).rev() {
        cur = eval_state(&format!("run/back[{t}]"), &back, &cur);
        assert_eq!(cur, refs[t], "backward run diverged at step {t}");
    }
    assert_eq!(cur, start, "T forward + T backward must return to start");
}

// ══════════════════════════════════════════════════════════════════════
// 5. SWAPBR: documented step-level degeneracy + bijectivity
// ══════════════════════════════════════════════════════════════════════

#[test]
fn swapbr_step_is_pc_advance_only() {
    // See step.rs module docs: the spec's SWAPBR step semantics are
    // non-injective on the full State space, so step() keeps SWAPBR a
    // net no-op (execute swaps PC<->BR, the SWAPBR-keyed unswap undoes
    // it before the unfetch). This test pins that behavior down — and
    // verifies IR still re-zeroes and the step stays reversible.
    let rom = vec![r_ins(8, 0, 0, 0), r_ins(0, 1, 2, 0)]; // SWAPBR; ADD r1, r2
    let iso = step(&rom);
    let back = Iso::sym(iso.clone());

    for br in [0u8, 1, 5, 0x99] {
        let mut s = st(regs_a(), 0);
        s.br = br;
        let got = eval_state("swapbr", &iso, &s);
        let mut expected = s.clone();
        expected.pc = 1; // PC advance only
        assert_eq!(got, expected, "SWAPBR step must be PC-advance-only (br={br})");
        assert_eq!(got.ir, 0, "SWAPBR step: IR must re-zero");

        let rec = eval_state("swapbr/back", &back, &got);
        assert_eq!(rec, s, "SWAPBR step must stay reversible (br={br})");
    }

    // The non-SWAPBR instruction in the same ROM still works.
    let s = st(regs_b(), 1);
    assert_eq!(eval_state("swapbr-rom/add", &iso, &s), ref_step(&rom, &s));
}
