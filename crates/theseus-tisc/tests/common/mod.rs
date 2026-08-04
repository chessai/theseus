//! Shared test-support helpers for the `theseus-tisc` integration tests.
//!
//! This module is included via `mod common;` from each test binary that needs
//! it (the `mod.rs` form keeps cargo from treating it as its own test binary).
//! Because no single test binary uses every helper, the whole module allows
//! dead code -- otherwise a helper unused by one binary would trip
//! `-D warnings` for that binary.
//!
//! Helpers here live in an *external* (integration-test) crate, so they may
//! only use `theseus_tisc`'s public API.
//!
//! Contents:
//! - T4 (this file): the reference emulator (`ref_instr` / `ref_step`), the
//!   manual R/I instruction-word encoders (`r_ins` / `i_ins`), and the
//!   eval-and-decode helper (`eval_state`).
//! - [`bveq_support`]: BVEQ symbolic-state helpers (varmap, symbolic words,
//!   field extraction, the shared state-fields type-walk skeleton).
//! - [`bdd_helpers`]: ripple-carry BDD adders.
//! - [`prim_builders`]: the XOR `PrimDef` / `Iso` builders.

#![allow(dead_code)]

pub mod bdd_helpers;
pub mod bveq_support;
pub mod prim_builders;

use theseus_core::Iso;
use theseus_tisc::compile::pc_update;
use theseus_tisc::types::{StateVals, MEM_WORDS};

// ── Instruction encoders (spec §3) ─────────────────────────────────
//
// A-0 32-bit word: R: fmt(31) | op[30:25] | rd[24:20] | rs[19:15] | rt[14:10].
//                  I: fmt(31)=0 | op[30:25] | rd[24:20] | imm[19:0].
//
// These are MANUAL encoders that deliberately bypass `asm::encode`'s operand
// validation, so tests can build reserved / adversarial words directly. The
// `assert!` guards only bound the bit fields (opcode < 64, reg < 16); they do
// not re-impose the assembler's semantic operand checks.

/// R-format instruction word (see module note for the layout).
pub fn r_ins(opcode: u32, rd: u32, rs: u32, rt: u32) -> u32 {
    assert!(opcode < 64 && rd < 16 && rs < 16 && rt < 16);
    (1 << 31) | (opcode << 25) | (rd << 20) | (rs << 15) | (rt << 10)
}

/// I-format instruction word (see module note for the layout).
pub fn i_ins(opc: u32, rd: u32, imm: u8) -> u32 {
    assert!(opc < 64 && rd < 16);
    (opc << 25) | (rd << 20) | u32::from(imm)
}

// ── Reference emulator ─────────────────────────────────────────────

/// Apply the semantics of one instruction word to the register state.
/// SWAPBR rd swaps BR ↔ r[rd] (ADR-0034 D1). Reserved opcodes and
/// word 0 are no-ops given s0 = 0 (asserted in `ref_step`).
#[allow(clippy::collapsible_match)] // the Cswap arm reads clearest as a nested if
pub fn ref_instr(word: u32, s: &mut StateVals) {
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
            0b1000 => {
                let t = s.regs[rd];
                s.regs[rd] = s.br as u8;
                s.br = u16::from(t);
            }
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
///
/// The three precondition asserts encode the oracle's contract (P4 IR=0
/// between steps, s0/s1 reserved-zero); keep them so a caller that violates
/// the convention fails loudly rather than silently comparing against a
/// degenerate reference.
pub fn ref_step(rom: &[u32], s: &StateVals) -> StateVals {
    assert_eq!(s.ir, 0, "P4: IR must be 0 between steps");
    assert_eq!(s.scratch[0], 0, "s0 convention (ADDI / IR=0 degeneracy)");
    assert_eq!(s.scratch[1], 0, "s1 reserved-zero convention");
    let mut out = s.clone();
    let word = rom.get(s.pc as usize).copied().unwrap_or(0);
    ref_instr(word, &mut out);
    // ADR-0034 D2 / A-6.4: PC += sext(BR) + 1 (mod 2^W_PC), post-execute BR.
    out.pc = pc_update(s.pc, out.br as u8);
    out
}

// ── Eval helper ────────────────────────────────────────────────────

/// Evaluate `iso` on `s` (encoded), panic with `name` on failure, decode
/// back to a `StateVals`. `fuel` is a parameter because the per-file eval
/// budget differs between call sites.
pub fn eval_state(name: &str, iso: &Iso, s: &StateVals, fuel: u64) -> StateVals {
    let mut fuel = fuel;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}
