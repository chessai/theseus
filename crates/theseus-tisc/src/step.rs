//! TISC-1 `step`: the full fetch–decode/execute–unfetch–advance machine
//! cycle, `step(rom) : State <-> State` (tisc-1-spec §4).
//!
//! The ROM is a compile-time-constant slice of 16-bit instruction words;
//! `step` is one iso composed of four phases:
//!
//! 1. **Fetch** — CNOT the ROM word at address PC onto the (zero) IR.
//!    For each address `p` with `rom[p] != 0`, a predicate `PC == p`
//!    (the AND of the 8 PC-bit literals, computed into s1 bit 0 via a
//!    Toffoli chain through s1 bits 1..=6, exactly the decoder's
//!    predicate technique) gates plain CNOTs from the predicate wire
//!    onto the IR bits where `rom[p]` has a 1 — the constant is folded
//!    into *which* CNOTs exist, so each set bit costs one K1 CNOT and
//!    the whole entry costs 14 Toffolis (7 compute + 7 uncompute) +
//!    popcount(rom[p]) CNOTs. Addresses ≥ `rom.len()` (and zero words)
//!    have no entry: IR stays 0.
//!
//!    Fetch is an XOR mask keyed on PC, hence self-inverse for a fixed
//!    PC — the same iso serves as the unfetch.
//!
//! 2. **Decode + Execute** — [`decoder::decode_execute`] (already built):
//!    dispatches on IR's opcode bits and mutates the named registers,
//!    preserving IR.
//!
//! 3. **Unfetch** — re-zero IR with the SAME ROM word (P4: IR provably
//!    zero between steps). If the executed instruction was SWAPBR the
//!    execute phase swapped PC↔BR, so a second predicated PC↔BR swap
//!    keyed on IR-is-SWAPBR ([`decoder::instruction`]`(Op::Swapbr)`,
//!    exact because IR is still intact here) first restores the
//!    pre-execute PC; then the fetch iso runs again and IR returns to 0
//!    for *every* instruction, branch or not.
//!
//! 4. **Advance PC** — PC += 1 mod 256, a 12-Toffoli / 7-CNOT ripple
//!    increment whose carry chain lives in s1 (zero between phases).
//!
//! The adjoint `Iso::sym(step(rom))` is the reverse cycle (un-advance,
//! re-fetch, un-execute, un-fetch) by construction.
//!
//! ## SWAPBR limitation (documented, reported)
//!
//! Because the unswap in phase 3 is *not* re-applied after IR is zeroed
//! (see below), the net effect of a SWAPBR step through `step` is
//! **PC advance only** — the PC↔BR exchange is performed by the execute
//! phase and then undone before the unfetch. Branches are therefore not
//! yet functional at the step level; straight-line programs are exact.
//!
//! This is not an implementation shortcut but a genuine obstruction:
//! the spec semantics (`swap PC↔BR, then PC += 1`) are **not injective
//! on the full State space**, so no total iso can implement them.
//! Counterexample: with `rom[5] = SWAPBR` and `rom[9]` any non-branch,
//! pre-state A = (PC=5, BR=9, regs X) steps to (PC=10, BR=5, regs X)
//! via the branch, while pre-state B = (PC=9, BR=5, regs Y) steps to
//! (PC=10, BR=5, regs Y·rom[9]) via fall-through; choosing
//! Y = X·rom[9]⁻¹ makes two distinct pre-states collide. Reversible
//! ISAs escape this only via a structural convention (Pendulum's paired
//! branches / come-from discipline) that makes the colliding states
//! unreachable, and the step iso would have to key its re-swap on that
//! convention. Concretely: after the unfetch IR = 0, and any predicate
//! for "re-do the swap" must read {PC, BR, ROM}; keying on
//! "rom[PC] ∈ SWAPBR-addresses" with uncompute "rom[BR] ∈
//! SWAPBR-addresses" is exact only when a non-branch step never runs
//! with BR pointing at a SWAPBR address — but after any branch, BR
//! holds the come-from address, which *is* a SWAPBR address, so the
//! very next instruction violates it. Resolving this needs an
//! ISA-level decision (tracked for follow-up); `step` here stays a
//! total bijection with SWAPBR degenerating to a no-op.
//!
//! ## Conventions
//!
//! Inherited from the decoder: s0 = 0 on entry whenever the fetched
//! word is ADDI/SUBI **or zero** (IR = 0 decodes as `ADDI r0, #0`,
//! which adds s0 to r0 — the identity only when s0 = 0); s1 = 0 on
//! entry, restored to 0; no register aliasing in ROM words (P1
//! checker's job).

use theseus_core::Iso;

use crate::decoder::{self, cnot_prim, on_fields, toffoli_prim, Op, AUX_PRED_FIELD};
use crate::plumbing::{at_wire, gate_at, not_iso, seq_balanced};
use crate::types::{state_ty, IR_HI_FIELD, IR_LO_FIELD, PC_FIELD, W};

// ── Fetch ──────────────────────────────────────────────────────────
//
// Wire layout of the fetch core [PC, IR_lo, IR_hi, s1] (n = 32):
// PC bit i = wire i; IR bit b = wire 8 + b; s1 bit j = wire 24 + j.

const FETCH_N: usize = 4 * W;
/// Predicate (PC == p) lands in s1 bit 0.
const FETCH_PRED_WIRE: usize = 3 * W;
/// Toffoli-chain intermediates in s1 bits 1..=6.
const FETCH_CHAIN0: usize = 3 * W + 1;

/// Compute the predicate `PC == addr` into s1 bit 0 (8-literal AND:
/// NOTs on the zero literals, 7-Toffoli chain through s1 bits 1..=6,
/// NOTs undone). `Iso::sym` uncomputes it.
fn pc_pred(addr: u8) -> Iso {
    let nots: Vec<Iso> = (0..W)
        .filter(|i| (addr >> i) & 1 == 0)
        .map(|i| at_wire(FETCH_N, i, not_iso()))
        .collect();
    let mut gates = nots.clone();
    let mut acc = 0usize;
    for j in 1..W {
        let tgt = if j == W - 1 {
            FETCH_PRED_WIRE
        } else {
            FETCH_CHAIN0 + (j - 1)
        };
        gates.push(gate_at(FETCH_N, &[acc, j, tgt], toffoli_prim()));
        acc = tgt;
    }
    gates.extend(nots);
    on_fields(&[PC_FIELD, IR_LO_FIELD, IR_HI_FIELD, AUX_PRED_FIELD], gates)
}

/// One ROM entry: iff PC == addr, XOR the constant `word` into IR.
fn fetch_entry(addr: u8, word: u16) -> Iso {
    let pred = pc_pred(addr);
    let cnots: Vec<Iso> = (0..16usize)
        .filter(|&b| (word >> b) & 1 == 1)
        .map(|b| gate_at(FETCH_N, &[FETCH_PRED_WIRE, W + b], cnot_prim()))
        .collect();
    let body = on_fields(&[PC_FIELD, IR_LO_FIELD, IR_HI_FIELD, AUX_PRED_FIELD], cnots);
    Iso::seq(pred.clone(), Iso::seq(body, Iso::sym(pred)))
}

/// Fetch phase: `IR ^= rom[PC]` (IR ← rom[PC] when IR = 0 on entry,
/// per P4). Addresses ≥ `rom.len()` and zero words leave IR unchanged.
/// Self-inverse for a fixed PC, so it doubles as the unfetch.
/// Requires s1 = 0 on entry (restored on exit).
pub fn fetch(rom: &[u16]) -> Iso {
    assert!(rom.len() <= 1 << W, "ROM exceeds the 8-bit PC address space");
    let entries: Vec<Iso> = rom
        .iter()
        .enumerate()
        .filter(|&(_, &w)| w != 0)
        .map(|(p, &w)| fetch_entry(p as u8, w))
        .collect();
    if entries.is_empty() {
        Iso::Id(state_ty())
    } else {
        seq_balanced(entries)
    }
}

// ── PC increment ───────────────────────────────────────────────────

/// PC += 1 mod 256. Ripple increment: bit k flips iff bits 0..k are
/// all 1. The prefix-AND carries c_k = AND(PC[0..k]) for k = 2..=7 are
/// computed into s1 bits 0..=5 (12 Toffolis total: 6 compute + 6
/// uncompute, interleaved MSB-down with the 6 carry CNOT flips so each
/// uncompute reads not-yet-flipped PC bits), then PC1 ^= PC0 and PC0
/// flips unconditionally. Requires s1 = 0 on entry (restored).
/// `Iso::sym(pc_inc())` is the decrement.
pub fn pc_inc() -> Iso {
    let n = 2 * W; // [PC, s1]: PC bit i = wire i, s1 bit j = wire 8 + j
    let c = |k: usize| W + (k - 2); // carry c_k lives at s1 bit k-2
    let mut gates: Vec<Iso> = Vec::new();
    // Compute carries c_2..c_7.
    gates.push(gate_at(n, &[0, 1, c(2)], toffoli_prim()));
    for k in 3..W {
        gates.push(gate_at(n, &[c(k - 1), k - 1, c(k)], toffoli_prim()));
    }
    // Flip PC[k] ^= c_k and uncompute c_k, MSB down.
    for k in (2..W).rev() {
        gates.push(gate_at(n, &[c(k), k], cnot_prim()));
        if k == 2 {
            gates.push(gate_at(n, &[0, 1, c(2)], toffoli_prim()));
        } else {
            gates.push(gate_at(n, &[c(k - 1), k - 1, c(k)], toffoli_prim()));
        }
    }
    gates.push(gate_at(n, &[0, 1], cnot_prim())); // PC1 ^= PC0
    gates.push(at_wire(n, 0, not_iso())); // PC0 ^= 1
    on_fields(&[PC_FIELD, AUX_PRED_FIELD], gates)
}

// ── The step iso ───────────────────────────────────────────────────

/// One TISC-1 machine cycle over a subset of the instruction set
/// (useful for incremental verification and cost accounting).
/// If `ops` contains [`Op::Swapbr`], the SWAPBR-keyed unswap is
/// inserted before the unfetch (see module docs).
pub fn step_ops(rom: &[u16], ops: &[Op]) -> Iso {
    let f = fetch(rom);
    let mut phases = vec![f.clone(), decoder::decode_execute_ops(ops)];
    if ops.contains(&Op::Swapbr) {
        phases.push(decoder::instruction(Op::Swapbr));
    }
    phases.push(f); // unfetch: XOR the same ROM word — IR back to 0
    phases.push(pc_inc());
    seq_balanced(phases)
}

/// `step(rom) : State <-> State` — the full TISC-1 machine cycle:
/// fetch, decode+execute, unfetch (SWAPBR-safe: IR re-zeroes on every
/// input), advance PC. Total bijection; straight-line instructions
/// follow the spec exactly, SWAPBR degenerates to PC-advance-only
/// (see module docs for why and what unblocking it requires).
pub fn step(rom: &[u16]) -> Iso {
    step_ops(rom, &Op::ALL)
}
