#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! Differential-oracle tests for the TISC-1 `step` iso
//! (`step(rom) : State <-> State`, tisc-1-spec §4, ADR-0034).
//!
//! A Rust reference emulator `ref_step` (fetch rom[pc] → apply the
//! instruction's semantics → PC += BR + 1 mod 256) is the oracle. For
//! small straight-line test ROMs and sampled States we check:
//!
//!   1. lowering succeeds (unbalanced), netlist valid, max cell kind ≤ K3
//!      — per-step cell counts are PRINTED (the M4 unroll-planning number);
//!   2. faithfulness: balance-then-simulate == bits of core eval
//!      — `#[ignore]`d: the balanced netlist (~1.5M K0 buffers) exceeds
//!      host RAM; step correctness is verified via core-eval + component
//!      lowering (fetch_isolated, pc_inc_isolated);
//!   3. semantics: decode(eval(step, encode(s))) == ref_step(rom, s);
//!   4. reversibility: sym(step) recovers the input, and a T-step
//!      forward run followed by T adjoint steps returns to the start
//!      (the M4 headline capability at small scale).
//!
//! Sampling constraints (decoder/step conventions): ir = 0, s0 = 0,
//! s1 = 0 on entry; ROM words avoid register aliasing and the
//! decoder-reserved scratch (s0/s1) in XCHG.
//!
//! SWAPBR: per ADR-0034, SWAPBR rd swaps BR ↔ r[rd], and the end-of-step
//! PC update is `PC += BR + 1`. Branching is functional; a dedicated
//! test verifies a taken branch, and the counterexample from the former
//! obstruction (v1 non-injectivity) is shown to produce distinct
//! post-states under v2 semantics.

use std::sync::OnceLock;

use theseus_core::{type_of, Iso};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{balance, bits_of, lower_iso, rail_width, simulate, CellKind};
use theseus_tisc::step::{fetch, pc_add_br, pc_inc, step};
use theseus_tisc::types::{StateVals, MEM_WORDS};

const EVAL_FUEL: u64 = 8_000_000_000;

// ── Instruction encoders (spec §3) ─────────────────────────────────

// A-0 32-bit word: R: fmt(31) | op[30:25] | rd[24:20] | rs[19:15] | rt[14:10].
//                  I: fmt(31)=0 | op[30:25] | rd[24:20] | imm[19:0].
fn r_ins(opcode: u32, rd: u32, rs: u32, rt: u32) -> u32 {
    assert!(opcode < 64 && rd < 16 && rs < 16 && rt < 16);
    (1 << 31) | (opcode << 25) | (rd << 20) | (rs << 15) | (rt << 10)
}

fn i_ins(opc: u32, rd: u32, imm: u8) -> u32 {
    assert!(opc < 64 && rd < 16);
    (opc << 25) | (rd << 20) | u32::from(imm)
}

// ── Reference emulator ─────────────────────────────────────────────

/// Apply the semantics of one instruction word to the register state.
/// SWAPBR rd swaps BR ↔ r[rd] (ADR-0034 D1). Reserved opcodes and
/// word 0 are no-ops given s0 = 0 (asserted in ref_step).
fn ref_instr(word: u32, s: &mut StateVals) {
    if word & (1 << 31) != 0 {
        let opcode = (word >> 25) & 0x3F;
        let rd = ((word >> 20) & 0x1F) as usize;
        let rs = ((word >> 15) & 0x1F) as usize;
        let rt = ((word >> 10) & 0x1F) as usize;
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
            0b1000 => { let t = s.regs[rd]; s.regs[rd] = s.br as u8; s.br = u16::from(t); },
            0b1001 if rt < MEM_WORDS => std::mem::swap(&mut s.regs[rd], &mut s.mem[rt]),
            _ => {} // reserved
        }
    } else {
        let opc = (word >> 25) & 0x3F;
        let rd = ((word >> 20) & 0x1F) as usize;
        let imm = (word & 0xFF) as u8;
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
/// it, PC += BR + 1 (ADR-0034 D2). IR stays 0 across the step.
fn ref_step(rom: &[u32], s: &StateVals) -> StateVals {
    assert_eq!(s.ir, 0, "P4: IR must be 0 between steps");
    assert_eq!(s.scratch[0], 0, "s0 convention (ADDI / IR=0 degeneracy)");
    assert_eq!(s.scratch[1], 0, "s1 reserved-zero convention");
    let mut out = s.clone();
    let word = rom.get(s.pc as usize).copied().unwrap_or(0);
    ref_instr(word, &mut out);
    // ADR-0034 D2 / A-6.4: PC += sext(BR) + 1 (mod 2^W_PC), post-execute BR.
    out.pc = theseus_tisc::compile::pc_update(s.pc, out.br as u8);
    out
}

// ── Test ROMs (straight-line, no aliasing, no s0/s1 in XCHG) ───────

/// Primary ROM: 8 instructions covering ADDI, ADD, XOR, ROTL, EXCH,
/// SUBI, CSWAP, ROTRI at addresses 0..8. This is the ROM the lowered /
/// balanced netlist is built for.
fn rom1() -> Vec<u32> {
    vec![
        i_ins(0, 1, 0x05), // 0: ADDI r1, 5
        r_ins(0, 2, 1, 0), // 1: ADD  r2, r1
        r_ins(2, 3, 2, 0), // 2: XOR  r3, r2
        r_ins(4, 4, 1, 0), // 3: ROTL r4, r1   (amount = r1 & 7)
        r_ins(3, 5, 0, 0), // 4: EXCH r5, r0
        i_ins(1, 6, 0x11), // 5: SUBI r6, 0x11
        r_ins(6, 7, 2, 3), // 6: CSWAP r7, r2, r3  (cond on r7[0])
        i_ins(4, 0, 0x02), // 7: ROTRI r0, 2
    ]
}

/// Secondary ROM (eval-only, no lowering): SUB, ROTR, XORI, XCHG.
fn rom2() -> Vec<u32> {
    vec![
        r_ins(1, 1, 4, 0), // 0: SUB  r1, r4
        r_ins(5, 2, 5, 0), // 1: ROTR r2, r5
        i_ins(2, 3, 0x5A), // 2: XORI r3, 0x5A
        r_ins(7, 6, 0, 3), // 3: XCHG r6, s3
    ]
}

// ── Sample states (ir = 0, s0 = s1 = 0, BR = 0 for straight-line) ─

fn st(regs: [u8; 8], pc_in: impl Into<u16>) -> StateVals {
    let pc: u16 = pc_in.into();
    StateVals {
        regs,
        pc,
        br: 0,
        ir: 0,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
        mem: [0; MEM_WORDS],
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
// 1. Phase isolation: fetch, pc_add_br, and pc_inc
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
    assert!((max_kind <= CellKind::K3 || max_kind == CellKind::R1));

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
    let s = st(regs_b(), 3u16);
    assert_eq!(eval_state("fetch twice", &twice, &s), s);
}

// ── A-6.4 widen-PC F-2 regression: narrow-fetch A_fetch=16 aliasing guard ──
//
// At A_fetch = 16 (rom_len > 32768) the per-entry predicate's AND-chain
// intermediate `fetch_chain(14)` lands on `FETCH_HZ_WIRE`, inverting the
// predicate: review probe B saw rom_len=32770 with a word at 0x8000 NOT
// fetched and 0x8001 FALSELY fetched (aliasing). The fix caps the narrow
// fetch to A_fetch ≤ 15 (rom_len ≤ MAX_FETCH_ROM_LEN = 32768) and REJECTS
// beyond it (fetch asserts) rather than corrupting.

#[test]
fn fetch_a15_boundary_no_aliasing() {
    use theseus_tisc::step::{fetch_addr_bits, MAX_FETCH_ROM_LEN};
    // The largest ACCEPTED rom_len (a=15). A sparse ROM: one non-idle word at
    // the top live address 0x7FFF; every other slot is idle (word 0). fetch
    // filters idle words, so this builds exactly ONE entry at a=15 (cheap to
    // eval) while exercising the full-a-15 predicate + high-zero gate.
    assert_eq!(MAX_FETCH_ROM_LEN, 32768);
    assert_eq!(fetch_addr_bits(MAX_FETCH_ROM_LEN), 15, "boundary must be a=15");
    let top = MAX_FETCH_ROM_LEN - 1; // 0x7FFF
    let word = 0x0EADBEEFu32; // legal 32-bit IR pattern (top nibble 0)
    let mut rom = vec![0u32; MAX_FETCH_ROM_LEN];
    rom[top] = word;
    let iso = fetch(&rom);

    // Exact match at 0x7FFF fetches the word; neighbor 0x7FFE (idle) and the
    // just-out-of-range 0x8000 (bit 15 set → high-zero gate parks it, the
    // exact address the a=16 bug FALSELY matched) both leave IR untouched.
    for (pc, want_ir) in [
        (0x7FFFu16, word),
        (0x7FFEu16, 0u32),
        (0x8000u16, 0u32), // probe B's aliasing target — MUST park
        (0x8001u16, 0u32),
        (0xFFFFu16, 0u32),
    ] {
        let s = st(regs_a(), pc);
        let got = eval_state("fetch a=15", &iso, &s);
        let mut expected = s.clone();
        expected.ir = want_ir;
        assert_eq!(
            got, expected,
            "fetch a=15: pc=0x{pc:04X} expected IR 0x{want_ir:08X} (no aliasing)"
        );
    }
}

#[test]
#[should_panic(expected = "narrow-fetch predicate limit")]
fn fetch_rom_len_over_32768_rejects_gracefully() {
    use theseus_tisc::step::MAX_FETCH_ROM_LEN;
    // Probe B: rom_len = 32770 (> 32768) forces A_fetch = 16 → chain/hz-wire
    // collision → predicate inversion. fetch must REJECT (assert), not silently
    // build a corrupt fetch. BEFORE the fix fetch built the aliasing iso (no
    // panic); AFTER the fix it panics with the narrow-fetch limit message.
    let rom_len = MAX_FETCH_ROM_LEN + 2; // 32770 = probe B
    let mut rom = vec![0u32; rom_len];
    rom[0x8000] = 0x0EADBEEF; // probe B's word at 0x8000
    let _ = fetch(&rom); // must panic BEFORE returning a corrupt fetch iso
}

#[test]
fn pc_add_br_isolated() {
    let iso = pc_add_br();

    let net = lower_iso(&iso).expect("pc_add_br lowers");
    assert_eq!(net.validate(), Ok(()));
    let counts = net.cell_counts();
    let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
    let total: usize = counts.values().sum();
    eprintln!("pc_add_br: {total} cells, max kind {max_kind:?}, counts {counts:?}");
    assert!(
        max_kind <= CellKind::K2 || max_kind == CellKind::R1,
        "pc_add_br: max kind {max_kind:?} exceeds the admitted K0..K2 + R1 vocabulary"
    );

    // Sample: PC += sext(BR) for various (pc, br) pairs, including the
    // backward-jump case (br top-bit set ⇒ negative sext ⇒ PC walks back
    // across the 16-bit boundary) and 16-bit PC values.
    let sext = |br: u8| br as i8 as i16 as u16;
    for (pc, br) in [
        (0u16, 0u8),
        (10, 5),
        (0x00FF, 1),
        (0x0100, 0xF6), // br = -10 (backward), PC 256 → 246
        (0x0000, 0xFF), // br = -1, PC 0 → 0xFFFF (wrap back)
        (0x1234, 200),
        (0xFFFF, 1),
        (0x0140, 246), // 320 + (-10) = 310
    ] {
        let mut s = st(regs_a(), pc);
        s.br = u16::from(br);
        let got = eval_state("pc_add_br", &iso, &s);
        let mut expected = s.clone();
        expected.pc = pc.wrapping_add(sext(br));
        assert_eq!(got, expected, "pc_add_br: pc={pc:#06x}, br={br:#04x}");

        // Adjoint = subtract sext(BR).
        let back = eval_state("pc_sub_br", &Iso::sym(iso.clone()), &got);
        assert_eq!(back, s, "pc_add_br adjoint: pc={pc:#06x}, br={br:#04x}");
    }
}

#[test]
fn pc_add_br_sampled_br0_is_identity() {
    // With BR = 0, pc_add_br must be the identity. Sampled at carry boundaries.
    let iso = pc_add_br();
    for pc in [0u8, 1, 7, 8, 0x7F, 0x80, 0xFE, 0xFF] {
        let s = st(regs_b(), pc);
        let got = eval_state("pc_add_br/br0", &iso, &s);
        assert_eq!(got, s, "pc_add_br with BR=0 must be identity (pc={pc})");
    }
}

#[test]
#[ignore = "authoritative exhaustive pc_add_br; run via --ignored"]
fn pc_add_br_exhaustive_br0_is_identity() {
    let iso = pc_add_br();
    for pc in 0..=255u8 {
        let s = st(regs_b(), pc);
        let got = eval_state("pc_add_br/br0", &iso, &s);
        assert_eq!(got, s, "pc_add_br with BR=0 must be identity (pc={pc})");
    }
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
    assert!((max_kind <= CellKind::K3 || max_kind == CellKind::R1));

    // 16-bit increment: include the inter-byte carry (0x00FF → 0x0100)
    // and the full wrap (0xFFFF → 0x0000).
    for pc in [
        0u16, 1, 2, 7, 8, 0x0F, 0x7F, 0x80, 0xFE, 0xFF, 0x00FF, 0x0100, 0x01FF, 0x1234, 0xFFFE,
        0xFFFF,
    ] {
        let s = st(regs_a(), pc);
        let got = eval_state("pc_inc", &iso, &s);
        let mut expected = s.clone();
        expected.pc = pc.wrapping_add(1);
        assert_eq!(got, expected, "pc_inc: pc={pc:#06x}");

        // Adjoint = decrement.
        let back = eval_state("pc_dec", &Iso::sym(iso.clone()), &got);
        assert_eq!(back, s, "pc_inc adjoint: pc={pc:#06x}");
    }
}

#[test]
fn pc_inc_sampled() {
    // Sampled at 8-bit and inter-byte carry boundaries.
    let iso = pc_inc();
    for pc in [
        0u16, 1, 7, 8, 0x7F, 0x80, 0xFE, 0xFF, 0x00FF, 0x0100, 0x7FFF, 0x8000, 0xFFFF,
    ] {
        let s = st(regs_b(), pc);
        let got = eval_state("pc_inc/sampled", &iso, &s);
        let mut expected = s.clone();
        expected.pc = pc.wrapping_add(1);
        assert_eq!(
            got, expected,
            "pc_inc: pc={pc:#06x} (carry scratch must restore)"
        );
    }
}

// ── RETIRED: the 2^16 brute-force pc_inc sweep (Tier-A speedup). ──
//
// ADR-0034 A-6.4: `pc_inc` computes (PC+1) mod 2^W_PC for EVERY 16-bit PC, with
// the carry crossing the PC_lo→PC_hi field boundary and s1 scratch restored.
// The WHOLE-DOMAIN authority for this is now the SYMBOLIC BDD proof
// `tests/bveq_step_poc.rs::bveq_pc_inc_wpc16` (proves `pc_inc_with(&default)` ≡
// (PC+1) mod 2^16 over the full domain in ~0.1 s, with a drop-+1 negative
// control and field-preservation, vs the retired 2^16 loop). The fast reality
// anchor `pc_inc_sampled` (above) still ties eval to the spec at carry
// boundaries; `bveq_step_poc::bveq_pc_update_bootstrap_wpc16` adds the eval-level
// anchor for the full pc_add_br;pc_inc composite.

// ══════════════════════════════════════════════════════════════════════
// 1b. ADR-0034 A-6.4 re-proof: the split-field PC update is exactly
//     (PC + sext(BR) + 1) mod 2^W_PC — the mechanized injectivity witness
//     for the `A : (PC,BR,rest) ↦ (PC+BR+1, BR, rest)` bijection at the
//     widened width. See docs/src/adr/0034 D2/D4 (A-6.4 amendment).
// ══════════════════════════════════════════════════════════════════════

/// The end-of-step PC update as the hardware computes it: pc_add_br ; pc_inc.
fn pc_update_iso() -> Iso {
    Iso::seq(pc_add_br(), pc_inc())
}

#[test]
fn adr0034_split_add_sampled() {
    // Fast sampled cover of the split-field update over carry boundaries
    // and the backward-jump (sext-negative BR) case.
    let iso = pc_update_iso();
    let expect = |pc: u16, br: u8| theseus_tisc::compile::pc_update(pc, br);
    for pc in [
        0u16, 1, 0x00FF, 0x0100, 0x7FFF, 0x8000, 0xFFFE, 0xFFFF, 0x1234,
    ] {
        for br in [0u8, 1, 5, 0x7F, 0x80, 0xF6, 0xFF] {
            let mut s = st(regs_a(), pc);
            s.br = u16::from(br);
            let got = eval_state("split_add", &iso, &s);
            assert_eq!(
                got.pc,
                expect(pc, br),
                "split (pc_add_br;pc_inc) != (PC+sext(BR)+1) mod 2^16: pc={pc:#06x} br={br:#04x}"
            );
            assert_eq!(got.br, u16::from(br), "BR preserved by the PC update");
            assert_eq!(got.scratch[1], 0, "s1 restored by the PC update");
        }
    }
}

// ── RETIRED: the 2^24 brute-force split-add re-proof (was HOURS; Tier-A). ──
//
// The full mechanized ADR-0034 re-proof at W_PC=16, W_BR=8 —
//   (pc_add_br ; pc_inc)(PC,BR) == (PC + sext(BR) + 1) mod 2^W_PC
// for ALL PC ∈ [0,2^16), BR ∈ [0,2^8) (2^24 = 16.7M pairs, HOURS by
// simulation) — is now the SYMBOLIC BDD proof
// `tests/bveq_step_poc.rs::bveq_pc_update_wpc16` + `bveq_pc_update_negative_control_wpc16`
// (drop-+1, zero-extend-BR, carry-less-byte-split teeth). It proves the SAME
// identity over the WHOLE (PC,BR) domain — and MORE, with regs/IR/scratch held
// SYMBOLIC — in ~0.15 s, domain-size-independent, in the FAST tier. The fast
// reality anchors `adr0034_split_add_sampled` (above, eval vs pc_update at
// boundaries) and `bveq_step_poc::bveq_pc_update_bootstrap_wpc16` tie the
// symbolic model back to the real interpreter. This is the single authority; no
// duplicate slow sweep remains.

// ══════════════════════════════════════════════════════════════════════
// 2. The full step: lower once (rom1), verify samples
// ══════════════════════════════════════════════════════════════════════

/// Light context: the rom1 step iso only (for eval-only tests, which
/// must not pay the multi-minute lowering of the heavy ctx).
fn step1() -> &'static (Vec<u32>, Iso) {
    static S: OnceLock<(Vec<u32>, Iso)> = OnceLock::new();
    S.get_or_init(|| {
        let rom = rom1();
        let iso = step(&rom);
        (rom, iso)
    })
}

/// Lowering verification: lowers the full step (unbalanced only), prints
/// per-step cell counts + max kind, asserts max kind <= K3.  Does NOT
/// call balance() — the balanced netlist (~1.5M K0 buffers) exceeds host
/// RAM.
fn verify_lowering() -> &'static () {
    static DONE: OnceLock<()> = OnceLock::new();
    DONE.get_or_init(|| {
        let rom = rom1();
        let t0 = std::time::Instant::now();
        let iso = step(&rom);
        let (in_ty, _out_ty) = type_of(&iso).expect("step types");
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
        assert!(
            (max_kind <= CellKind::K3 || max_kind == CellKind::R1),
            "step: max kind {max_kind:?} > K3"
        );

        let n_data = rail_width(&in_ty).unwrap();
        assert_eq!(net.inputs.len(), n_data + ancilla.len());
    })
}

/// Semantics check (eval vs ref_step) on one sample.  Uses step1()
/// (iso-only) to avoid triggering the expensive lowering in ctx().
fn check_semantics(name: &str, s: &StateVals) {
    let (rom, iso) = step1();
    let val = s.encode();
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, val.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));

    let got = StateVals::decode(&ev);
    let expected = ref_step(rom, s);
    assert_eq!(
        got, expected,
        "{name}: step semantics mismatch\n  got:      {got:?}\n  expected: {expected:?}"
    );
    assert_eq!(got.ir, 0, "{name}: P4 violated — IR not re-zeroed");
    assert_eq!(got.scratch[1], 0, "{name}: s1 not restored");
}

#[test]
fn step_lowering_within_budget() {
    let _ = verify_lowering();
}

#[test]
fn step_semantics() {
    // Each in-ROM pc exercises a different instruction of rom1.
    for pc in 0..8u8 {
        check_semantics(&format!("rom1/pc={pc}/a"), &st(regs_a(), pc));
    }
    // Second register file on one representative PC.
    check_semantics("rom1/pc=3/b", &st(regs_b(), 3u16));
    // Out-of-ROM pc: no-op + PC advance (word 0, s0 = 0).
    check_semantics("rom1/pc=200", &st(regs_a(), 200u16));
}

#[test]
#[ignore = "memory: full-step balanced netlist (~1.5M K0 buffers) exceeds host RAM; step is verified via core-eval + isolated-component lowering. Track: balancer buffer-materialization memory."]
fn step_faithfulness_balanced_simulate() {
    let rom = rom1();
    let iso = step(&rom);
    let (in_ty, out_ty) = type_of(&iso).expect("step types");
    let (net, ancilla) = lower_iso_with_ancillas(&iso).expect("step lowers");
    let balanced = balance(&net).expect("step balances");
    assert_eq!(balanced.validate(), Ok(()), "balanced netlist invalid");
    assert!(
        balanced.tick_imbalances().unwrap().is_empty(),
        "balanced netlist has tick imbalances"
    );
    let n_data_out = rail_width(&out_ty).unwrap();

    for pc in 0..8u8 {
        let s = st(regs_a(), pc);
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let ev = theseus_eval::eval(&iso, val.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("pc={pc}: eval failed: {e:?}"));
        let mut input_bits = bits_of(&val, &in_ty).unwrap();
        input_bits.extend_from_slice(&ancilla);
        let sim = simulate(&balanced, &input_bits)
            .unwrap_or_else(|e| panic!("pc={pc}: simulate failed: {e:?}"));
        let expected_bits = bits_of(&ev, &out_ty).unwrap();
        assert_eq!(
            &sim.outputs[..n_data_out],
            &expected_bits[..],
            "pc={pc}: balanced simulate disagrees with core eval"
        );
    }
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
    // The M4 headline capability at small scale: run rom1 forward,
    // tracking the reference emulator at every step, then T adjoint
    // steps back to the exact start state. 4 steps cover instructions
    // at PCs 0-3 (ADDI, ADD, XOR, ROTL) with full reversibility.
    let (rom, iso) = step1();
    let back = Iso::sym(iso.clone());
    const T: usize = 4;

    let start = st(regs_b(), 0u16);
    let mut cur = start.clone();
    let mut refs = vec![start.clone()];
    for t in 0..T {
        let next = eval_state(&format!("run/fwd[{t}]"), iso, &cur);
        let expected = ref_step(rom, &cur);
        assert_eq!(
            next, expected,
            "forward run diverged from reference at step {t}"
        );
        refs.push(next.clone());
        cur = next;
    }
    assert_eq!(cur.pc, T as u16, "PC must have advanced T times");

    for t in (0..T).rev() {
        cur = eval_state(&format!("run/back[{t}]"), &back, &cur);
        assert_eq!(cur, refs[t], "backward run diverged at step {t}");
    }
    assert_eq!(cur, start, "T forward + T backward must return to start");
}

// ══════════════════════════════════════════════════════════════════════
// 5. SWAPBR: functional branching + v1 counterexample regression
// ══════════════════════════════════════════════════════════════════════

#[test]
fn swapbr_step_swaps_br_and_rd() {
    // ADR-0034 D1: SWAPBR rd swaps BR ↔ r[rd]. The end-of-step PC
    // update is PC += BR + 1 (D2), using the post-execute BR.
    let rom = vec![
        r_ins(8, 2, 0, 0), // 0: SWAPBR r2
        r_ins(0, 1, 3, 0), // 1: ADD r1, r3
    ];
    let iso = step(&rom);
    let back = Iso::sym(iso.clone());

    // r2 = 0x05, BR = 0: after SWAPBR r2, r2 = 0, BR = 0x05.
    // PC update: PC = 0 + 0x05 + 1 = 6.
    let s = st(regs_a(), 0u16);
    // regs_a() = [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3]
    // s.br = 0 from st()
    let got = eval_state("swapbr/r2", &iso, &s);
    let expected = ref_step(&rom, &s);
    assert_eq!(got, expected, "SWAPBR r2 step semantics mismatch");
    assert_eq!(got.regs[2], 0, "r2 should get old BR value (0)");
    assert_eq!(got.br, 0xAA, "BR should get old r2 value (0xAA)");
    // A-6.4: BR=0xAA has its top bit set, so it sign-extends to a NEGATIVE
    // 16-bit offset: PC = 0 + sext(0xAA) + 1 = 0 + (-86) + 1 = 0xFFAB.
    assert_eq!(
        got.pc,
        theseus_tisc::compile::pc_update(0, 0xAA),
        "PC = 0 + sext(0xAA) + 1 (mod 2^16)"
    );
    assert_eq!(got.ir, 0, "P4: IR must re-zero after SWAPBR");
    assert_eq!(got.scratch[1], 0, "s1 must be restored");

    // Reversibility: adjoint recovers the input.
    let rec = eval_state("swapbr/back", &back, &got);
    assert_eq!(rec, s, "SWAPBR step must stay reversible");

    // The non-SWAPBR instruction in the same ROM still works (at pc=1).
    let s2 = st(regs_b(), 1u16);
    assert_eq!(eval_state("swapbr-rom/add", &iso, &s2), ref_step(&rom, &s2));
}

#[test]
fn v1_counterexample_distinct_post_states() {
    // ADR-0034 D4: the v1 counterexample pair now maps to DISTINCT
    // post-states under v2 semantics.
    //
    // A = (PC=5, BR=9, regs X) with rom[5] = SWAPBR r0
    // B = (PC=9, BR=5, regs Y) via fall-through
    //
    // Under v2: A yields predecessor PC = PC' - BR' - 1 = 5;
    //           B yields predecessor PC = PC' - BR' - 1 = 9.
    // These are distinct, so no collision.

    // Build a ROM: rom[5] = SWAPBR r0, rom[9] = ADD r1, r2.
    let mut rom = vec![0u32; 10];
    rom[5] = r_ins(8, 0, 0, 0); // SWAPBR r0
    rom[9] = r_ins(0, 1, 2, 0); // ADD r1, r2

    let iso = step(&rom);

    // State A: PC=5, BR=9, regs X
    let x_regs = [0x42u8, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3];
    let a = StateVals {
        regs: x_regs,
        pc: 5,
        br: 9,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    let a_out = eval_state("counterex/A", &iso, &a);

    // State B: PC=9, BR=5, regs Y (choose Y so that under v1 they
    // would collide, but under v2 they don't).
    // Under v2: B executes ADD r1,r2 -> r1 += r2; then PC = 9 + 5 + 1 = 15.
    let y_regs = x_regs; // same regs for simplicity
    let b = StateVals {
        regs: y_regs,
        pc: 9,
        br: 5,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    let b_out = eval_state("counterex/B", &iso, &b);

    // A: SWAPBR r0 swaps BR ↔ r0. r0 was 0x42, BR was 9.
    // After execute: r0=9, BR=0x42. PC = 5 + 0x42 + 1 = 0x48.
    // B: ADD r1, r2. r1 += r2 = 0xFF + 0xAA = 0xA9 (mod 256).
    // BR unchanged = 5. PC = 9 + 5 + 1 = 15.
    assert_ne!(
        a_out, b_out,
        "v1 counterexample pair MUST map to distinct post-states under v2 (ADR-0034 D4)"
    );

    // Verify the predecessor PCs are distinct (the key insight). A-6.4:
    // the predecessor PC is PC' − sext(BR') − 1 (mod 2^W_PC).
    let sext = |br: u8| br as i8 as i16 as u16;
    let pred_a = a_out.pc.wrapping_sub(sext(a_out.br as u8)).wrapping_sub(1);
    let pred_b = b_out.pc.wrapping_sub(sext(b_out.br as u8)).wrapping_sub(1);
    assert_eq!(pred_a, 5, "predecessor PC of A must be 5");
    assert_eq!(pred_b, 9, "predecessor PC of B must be 9");
    assert_ne!(pred_a, pred_b, "predecessor PCs must be distinct");

    // Both are individually reversible.
    let back = Iso::sym(iso.clone());
    let a_rec = eval_state("counterex/A/back", &back, &a_out);
    let b_rec = eval_state("counterex/B/back", &back, &b_out);
    assert_eq!(a_rec, a, "A must round-trip");
    assert_eq!(b_rec, b, "B must round-trip");
}
