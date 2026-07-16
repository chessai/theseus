//! TISC-1 `step`: the full fetch–decode/execute–unfetch–advance machine
//! cycle, `step(rom) : State <-> State` (tisc-1-spec §4, ADR-0034).
//!
//! The ROM is a compile-time-constant slice of 16-bit instruction words;
//! `step` is one iso composed of five phases:
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
//!    preserving IR. Per ADR-0034 D1, **no instruction writes PC**:
//!    SWAPBR swaps BR ↔ r[rd] (Pendulum semantics), and the execute
//!    phase's write set is restricted to `{r0..r7, s0..s7, BR}`.
//!
//! 3. **Unfetch** — re-zero IR with the SAME ROM word (P4: IR provably
//!    zero between steps). Because PC is constant across fetch → execute
//!    → unfetch (D1: nothing writes PC; the PC update runs after the
//!    unfetch), the unfetch XORs the same ROM word the fetch did, for
//!    every instruction including SWAPBR, and IR returns to 0
//!    unconditionally (ADR-0034 D3).
//!
//! 4. **PC += BR** — add the branch register (an offset) into PC via
//!    the Takahashi–Kunihiro zero-ancilla in-place ripple adder (≤ K2).
//!    BR is preserved (it is the operand of the add). BR = 0 is
//!    fall-through.
//!
//! 5. **PC += 1** — the existing 12-Toffoli / 7-CNOT ripple increment.
//!
//! Together phases 4–5 implement the uniform end-of-step PC update
//! `PC += BR + 1 (mod 256)` (ADR-0034 D2). BR = 0 gives fall-through
//! `PC += 0 + 1 = PC + 1`, so **every M4-v1 straight-line ROM (which
//! never sets BR) runs bit-identically**.
//!
//! ## Proof sketch: `step` is a total bijection (ADR-0034 D4)
//!
//! Write State = PC × Rest.
//!
//! 1. `fetch` writes only IR and s1, keyed on PC ⇒ preserves PC; it is
//!    an iso by construction (self-inverse XOR mask per PC fiber).
//! 2. `decode_execute` is a Seq of per-op predicated isos none of which
//!    has PC in its write set (D1) ⇒ preserves PC; each is an iso.
//! 3. Hence `E = fetch ; decode_execute ; fetch` is a bijection with
//!    PC∘E = PC: it decomposes fiberwise, `E = ⊕_p E_p`, each `E_p` a
//!    bijection of the fiber {PC = p} onto itself.
//! 4. `A : (PC, BR, rest) ↦ (PC + BR + 1, BR, rest)` is a bijection —
//!    inverse `(PC, BR, rest) ↦ (PC − BR − 1, BR, rest)`.
//! 5. `step = A ∘ E` is a composition of bijections. ∎
//!
//! The adjoint `Iso::sym(step(rom))` is the reverse cycle by
//! construction.
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
use crate::types::{state_ty, BR_FIELD, IR_HI_FIELD, IR_LO_FIELD, PC_FIELD, W};

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

// ── PC += BR (offset add) ─────────────────────────────────────────

/// `PC += BR (mod 256)` via the Takahashi–Kunihiro zero-ancilla
/// in-place ripple adder (≤ K2 cells). BR is the operand (preserved);
/// PC is the target (modified). No scratch needed — TK is zero-ancilla.
///
/// With BR = 0 this is the identity (fall-through).
/// `Iso::sym(pc_add_br())` subtracts BR from PC.
pub fn pc_add_br() -> Iso {
    let n = 2 * W; // [PC, BR]: PC bit i = wire i, BR bit j = wire 8 + j
    let a = |i: usize| -> usize { W + i }; // BR (operand, preserved)
    let b = |i: usize| -> usize { i }; // PC (target, modified: PC += BR)
    let mut gates: Vec<Iso> = Vec::new();
    // TK zero-ancilla in-place adder: b += a (mod 2^W), a preserved.
    for i in 1..W - 1 {
        gates.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
    }
    for i in (1..W - 1).rev() {
        gates.push(gate_at(n, &[a(i), a(i + 1)], cnot_prim()));
    }
    for i in 0..W - 1 {
        gates.push(gate_at(n, &[a(i), b(i), a(i + 1)], toffoli_prim()));
    }
    gates.push(gate_at(n, &[a(W - 1), b(W - 1)], cnot_prim()));
    gates.push(gate_at(n, &[a(W - 2), b(W - 2), a(W - 1)], toffoli_prim()));
    for i in (1..W - 1).rev() {
        gates.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
        gates.push(gate_at(n, &[a(i - 1), b(i - 1), a(i)], toffoli_prim()));
    }
    for i in 1..W - 1 {
        gates.push(gate_at(n, &[a(i), a(i + 1)], cnot_prim()));
    }
    for i in 0..W - 1 {
        gates.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
    }
    on_fields(&[PC_FIELD, BR_FIELD], gates)
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
///
/// The cycle is: fetch ; decode_execute ; unfetch ; pc_add_br ; pc_inc
/// (ADR-0034 D2). No SWAPBR-keyed unswap is needed: PC is constant
/// across E = fetch;execute;unfetch (D1: no instruction writes PC),
/// so the unfetch always XORs the same ROM word the fetch did, and
/// IR returns to 0 unconditionally (D3).
pub fn step_ops(rom: &[u16], ops: &[Op]) -> Iso {
    let f = fetch(rom);
    let phases = vec![
        f.clone(),                        // fetch
        decoder::decode_execute_ops(ops), // decode + execute
        f,                                // unfetch
        pc_add_br(),                      // PC += BR
        pc_inc(),                         // PC += 1
    ];
    seq_balanced(phases)
}

/// `step(rom) : CpuState <-> CpuState` — the ring machine cycle
/// (decode_execute′, 14 non-MEXCH ops). Total bijection on CpuState;
/// SWAPBR is functional, branching works via `PC += BR + 1`.
pub fn step(rom: &[u16]) -> Iso {
    step_ops(rom, &Op::ALL)
}

/// `system_step(rom) : (CpuState, MemState) <-> (CpuState, MemState)` —
/// the full machine cycle including MEXCH on the composite carrier.
///
/// Non-MEXCH phases are `(phase ⊗ id_MemState)`; the decode+execute
/// phase uses [`decoder::system_decode_execute`] which includes the
/// cross-factor MEXCH exchange. Used by `run_on_cpu` / `run_on_cpu_fast`.
pub fn system_step(rom: &[u16]) -> Iso {
    let mem_id = Iso::Id(crate::types::mem_state_ty());
    let f = fetch(rom);
    let phases = vec![
        Iso::times(f.clone(), mem_id.clone()),           // fetch ⊗ id_Mem
        decoder::system_decode_execute(),                 // composite decode+execute
        Iso::times(f, mem_id.clone()),                    // unfetch ⊗ id_Mem
        Iso::times(pc_add_br(), mem_id.clone()),          // PC += BR ⊗ id_Mem
        Iso::times(pc_inc(), mem_id),                     // PC += 1 ⊗ id_Mem
    ];
    seq_balanced(phases)
}
