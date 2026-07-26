//! TISC-1 `step`: the full fetch–decode/execute–unfetch–advance machine
//! cycle, `step(rom) : State <-> State` (tisc-1-spec §4, ADR-0034).
//!
//! The ROM is a compile-time-constant slice of 32-bit instruction words
//! (A-0 widened the instruction word 16 → 32 bits);
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

use crate::decoder::{
    self, cnot_prim, on_fields, toffoli_prim, Op, AUX_OPERAND_FIELD, AUX_PRED_FIELD,
};
use crate::plumbing::{at_wire, gate_at, not_iso, seq_balanced};
use crate::types::{
    state_ty, BR_FIELD, IR_B0_FIELD, IR_B1_FIELD, IR_B2_FIELD, IR_B3_FIELD, PC_FIELD, PC_HI_FIELD,
    W, W_PC,
};

// ── Fetch (narrow-A, A-6.4) ────────────────────────────────────────
//
// Wire layout of the fetch core [PC_lo, PC_hi, IR_b0..IR_b3, s1]
// (A-0 32-bit IR + A-6.4 16-bit PC; n = 7*W = 56):
//   PC bit i (0..W_PC=16) = wire i ;
//   IR bit b (0..32) = wire 16 + b ;
//   s1 bit j (0..8) = wire 48 + j.
//
// **Narrow-A fetch** (design §3.3): a correct program's PC only ever
// takes values in `[0, B + S_max)`, so `A_fetch = ⌈log₂(B + S_max)⌉` low
// PC bits already distinguish every reachable PC. Each ROM entry's
// address compare is therefore over only `A_fetch` bits, gated by a
// SINGLE shared "high bits all zero" bit (`hz`, computed once per step)
// — so the `W_PC`-dependent cost (the 16-A_fetch high-zero AND) is paid
// once, not once per entry, and the per-entry predicate cost is ∝ log₂ B,
// independent of W_PC. This is the ADR-0034 fetch-injectivity witness:
// `2^A_fetch ≥ B + S_max > every reachable PC`, so all reachable PCs are
// distinct in `A_fetch` bits, and the shared `hz` gate makes any
// unreachable high PC PARK (no false match) rather than corrupt.

// The fetch core lifts over 8 fields [PC_lo, PC_hi, IR_b0..b3, s0, s1]
// (n = 8*W = 64). PC bit i (0..16) = wire i; IR bit b = wire 16+b; s0 bit
// j = wire 48+j; s1 bit j = wire 56+j. BOTH scratch bytes are used as AND-
// chain space: fetch runs first in the step (and the unfetch after decode
// restores s0=0), so s0 and s1 are both 0 at fetch/unfetch time. This
// gives 14 usable chain-intermediate bits, enough for the up-to-15-input
// high-zero AND under a 16-bit PC — a single s1 byte (the 8-bit-PC design)
// is too narrow for the wider high-zero gate.
const FETCH_N: usize = 8 * W;
/// IR bit 0 wire.
const FETCH_IR0: usize = W_PC; // IR bit 0 = wire 16
/// s0 base wire (chain scratch).
const FETCH_S0: usize = 6 * W; // s0 bit 0 = wire 48
/// s1 base wire.
const FETCH_S1: usize = 7 * W; // s1 bit 0 = wire 56
/// Shared "high bits all zero" gate (`hz`) lands in s1 bit 7.
const FETCH_HZ_WIRE: usize = FETCH_S1 + 7;
/// Per-entry predicate `PC == addr` lands in s1 bit 0.
const FETCH_PRED_WIRE: usize = FETCH_S1;

/// AND-chain intermediate wire `k` (k = 0, 1, 2, …): s0 bits 0..8, then
/// s1 bits 1..7 (skips s1 bit 0 = pred and s1 bit 7 = hz). Up to 14 slots.
const fn fetch_chain(k: usize) -> usize {
    if k < W {
        FETCH_S0 + k // s0 bits 0..8
    } else {
        FETCH_S1 + 1 + (k - W) // s1 bits 1..7
    }
}

/// The fields the fetch core lifts over: PC_lo, PC_hi, the four IR
/// bytes, s0, s1.
fn fetch_fields() -> [usize; 8] {
    [
        PC_FIELD,
        PC_HI_FIELD,
        IR_B0_FIELD,
        IR_B1_FIELD,
        IR_B2_FIELD,
        IR_B3_FIELD,
        AUX_OPERAND_FIELD,
        AUX_PRED_FIELD,
    ]
}

/// Bits needed to hold every reachable PC (= every ROM address plus the
/// parked/forward-skip zone). `A_fetch = ⌈log₂(rom.len())⌉`, ≥ 1. Since
/// the assembler caps `rom.len() ≤ 2^A_addr` and every reachable PC is
/// `< rom.len()` on a live entry (higher PCs are parked, word 0, no
/// entry), `A_fetch` low bits distinguish every entry, and the shared
/// high-zero gate rejects PC ≥ 2^A_fetch. Clamped to `W_PC`.
pub fn fetch_addr_bits(rom_len: usize) -> usize {
    if rom_len <= 1 {
        1
    } else {
        let a = (usize::BITS - (rom_len - 1).leading_zeros()) as usize;
        a.clamp(1, W_PC)
    }
}

/// Compute the shared "high bits all zero" gate `hz = AND(¬PC[a..W_PC])`
/// into `FETCH_HZ_WIRE` (7-input-max AND via NOTs + a Toffoli chain in
/// s1 bits 1..6). When `a == W_PC` there are no high bits: `hz = 1`
/// (a plain NOT sets s1 bit 7). `Iso::sym` uncomputes it. Only fetch
/// entries whose address is ≥ 0 need it; it is threaded as an extra
/// control into every per-entry predicate.
fn pc_high_zero(a: usize) -> Iso {
    if a >= W_PC {
        // No high bits: hz ≡ 1. Set s1 bit 7.
        return on_fields(&fetch_fields(), vec![at_wire(FETCH_N, FETCH_HZ_WIRE, not_iso())]);
    }
    let hi: Vec<usize> = (a..W_PC).collect();
    // NOT every high bit so "all zero" ⇒ "all negated = 1".
    let nots: Vec<Iso> = hi
        .iter()
        .map(|&i| at_wire(FETCH_N, i, not_iso()))
        .collect();
    let mut gates = nots.clone();
    if hi.len() == 1 {
        // Single high bit: hz ^= ¬PC[a].
        gates.push(gate_at(FETCH_N, &[hi[0], FETCH_HZ_WIRE], cnot_prim()));
    } else {
        // Compute the AND chain into intermediate scratch (NOT hz), CNOT
        // the final AND onto hz, then UNCOMPUTE the chain — so hz alone is
        // set and every fetch_chain scratch bit is restored to 0 before
        // the per-entry predicates reuse them.
        let mut chain: Vec<Iso> = Vec::new();
        let mut acc = hi[0];
        let mut last = acc;
        for (t, &i) in hi[1..].iter().enumerate() {
            let tgt = fetch_chain(t);
            chain.push(gate_at(FETCH_N, &[acc, i, tgt], toffoli_prim()));
            acc = tgt;
            last = tgt;
        }
        gates.extend(chain.clone());
        gates.push(gate_at(FETCH_N, &[last, FETCH_HZ_WIRE], cnot_prim())); // hz ^= AND
        chain.reverse();
        gates.extend(chain); // uncompute the AND chain (restore scratch)
    }
    gates.extend(nots); // restore the negated PC bits
    on_fields(&fetch_fields(), gates)
}

/// Compute `pred = hz ∧ (PC[0..a] == addr)` into `FETCH_PRED_WIRE`
/// (s1 bit 0): NOTs on the zero low-literals, an `(a+1)`-input AND
/// (the `a` low PC literals plus the shared `hz` control) via a Toffoli
/// chain through s1 bits 1..a, NOTs undone. `Iso::sym` uncomputes it.
fn pc_pred(addr: usize, a: usize) -> Iso {
    // Low literals (bits 0..a) NOT-ed where addr bit is 0.
    let nots: Vec<Iso> = (0..a)
        .filter(|i| (addr >> i) & 1 == 0)
        .map(|i| at_wire(FETCH_N, i, not_iso()))
        .collect();
    let mut gates = nots.clone();
    // AND chain over controls [hz, PC0, PC1, ..., PC_{a-1}] → pred.
    let controls: Vec<usize> = std::iter::once(FETCH_HZ_WIRE).chain(0..a).collect();
    if controls.len() == 1 {
        // a == 0 (impossible, a≥1) — pred = hz.
        gates.push(gate_at(FETCH_N, &[controls[0], FETCH_PRED_WIRE], cnot_prim()));
    } else {
        let mut acc = controls[0];
        for (t, &c) in controls[1..].iter().enumerate() {
            let tgt = if t == controls.len() - 2 {
                FETCH_PRED_WIRE
            } else {
                fetch_chain(t)
            };
            gates.push(gate_at(FETCH_N, &[acc, c, tgt], toffoli_prim()));
            acc = tgt;
        }
    }
    gates.extend(nots);
    on_fields(&fetch_fields(), gates)
}

/// One ROM entry: iff `hz ∧ PC[0..a] == addr`, XOR the constant 32-bit
/// `word` into IR.
fn fetch_entry(addr: usize, word: u32, a: usize) -> Iso {
    let pred = pc_pred(addr, a);
    let cnots: Vec<Iso> = (0..32usize)
        .filter(|&b| (word >> b) & 1 == 1)
        .map(|b| gate_at(FETCH_N, &[FETCH_PRED_WIRE, FETCH_IR0 + b], cnot_prim()))
        .collect();
    let body = on_fields(&fetch_fields(), cnots);
    Iso::seq(pred.clone(), Iso::seq(body, Iso::sym(pred)))
}

/// Fetch phase: `IR ^= rom[PC]` (IR ← rom[PC] when IR = 0 on entry,
/// per P4). Addresses ≥ `rom.len()` and zero words leave IR unchanged;
/// PCs with any bit ≥ A_fetch set are parked (the shared high-zero gate
/// matches nothing). Self-inverse for a fixed PC, so it doubles as the
/// unfetch. Requires s1 = 0 on entry (restored on exit).
pub fn fetch(rom: &[u32]) -> Iso {
    assert!(
        rom.len() <= 1 << W_PC,
        "ROM exceeds the {W_PC}-bit PC address space"
    );
    let a = fetch_addr_bits(rom.len());
    let entries: Vec<Iso> = rom
        .iter()
        .enumerate()
        .filter(|&(_, &w)| w != 0)
        .map(|(p, &w)| fetch_entry(p, w, a))
        .collect();
    if entries.is_empty() {
        Iso::Id(state_ty())
    } else {
        // Shared high-zero gate computed once, wrapping all entries.
        let hz = pc_high_zero(a);
        Iso::seq(
            hz.clone(),
            Iso::seq(seq_balanced(entries), Iso::sym(hz)),
        )
    }
}

// ── Standing energy cost of the widen (MEASURED) ───────────────────
//
// Isolated unbalanced cell counts, 8-bit-PC baseline (f186563) → A-6.4
// 16-bit PC:
//   pc_add_br : 74  →  194   (+120)   16-bit materialized-operand TK adder
//   pc_inc    : 43  →  162   (+119)   16-bit TK add of constant 1
//   fetch(len8): 372 → 326   (−46)    narrow-A (A_fetch=3 vs full-8 compare)
// Net PC-dependent delta ≈ +193 cells on a ~4846-cell step ≈ +4.0% for the
// whole 8→16-bit widen (8 added PC bits) ≈ ~0.5 %/bit — under the design's
// "<1 %/bit" target, and narrow-A fetch pays for part of it (the fetch
// predicate SHRANK). The absolute step_j re-pin (me_a3) is deferred: the
// wider state OOMs balance()/lower_iso on the host (see energy.rs), but
// step_j is exactly proportional to the balanced cell inventory, so the
// cell-count delta is the faithful per-step energy proxy.

// ── PC += sext(BR)  (sign-extended offset add) ─────────────────────
//
// A-6.4 widen-PC: PC is `W_PC` (=16) bits over two Word8 fields
// [PC_lo = PC_FIELD, PC_hi = PC_HI_FIELD]; BR is an 8-bit 2's-complement
// offset **sign-extended** into the 16-bit PC. The come-from token for a
// backward jump, `δu = 256 − iter_len`, has its top bit (bit 7) set
// whenever `iter_len ≤ 128`, so `sext(δu)` is negative (`0xFF..`) and
// `PC += sext(BR) + 1` walks PC *backward* across the full 16-bit space;
// forward offsets ≤ 127 sign-extend to themselves. This is why an 8-bit
// BR "suffices" (design §5) under a 16-bit PC.
//
// Adder: an in-place ripple-carry (Cuccaro MAJ/UMA, RC 2004) `PC += op`
// with ONE ancilla carry (s1 bit 0, restored to 0), where the operand
// bit for PC position i is `sext(BR)[i] = BR[min(i, 7)]` — the sign bit
// BR[7] is fanned out (read-only control) into PC bits 8..15. BR itself
// is preserved (it is only a control). Wire layout over the 4 fields
// [PC_lo, PC_hi, BR, s1] (n = 32): PC bit i (0..16) = wire i; BR bit j
// (0..8) = wire 16+j; the ancilla carry = wire 24 (s1 bit 0).

/// `PC += sext(BR) (mod 2^W_PC)`. BR (operand) is preserved; PC (target,
/// 16-bit) is modified.
///
/// Approach (reuses the PROVEN zero-ancilla Takahashi–Kunihiro in-place
/// adder that shipped at 8-bit): materialize the 16-bit sign-extended
/// operand `op = sext(BR)` in scratch — low byte in s0 (copied from BR),
/// high byte in s1 (all eight bits = BR[7]) — run the `W_PC`-bit TK adder
/// `PC += op`, then uncompute `op`. BR is only ever a CNOT *control*, so
/// it is preserved. Requires s0 = 0 and s1 = 0 on entry (the standing
/// decoder scratch conventions; asserted by the step tests), restored on
/// exit. With BR = 0 this is the identity (fall-through).
/// `Iso::sym(pc_add_br())` subtracts sext(BR) from PC.
///
/// Wire layout over [PC_lo, PC_hi, BR, s0, s1] (n = 5*W = 40):
///   PC bit i (0..16) = wire i ; BR bit j (0..8) = wire 16+j ;
///   s0 bit j = wire 24+j (operand low) ; s1 bit j = wire 32+j (operand high).
pub fn pc_add_br() -> Iso {
    let n = 5 * W;
    let pc = |i: usize| i; // PC target bit i (0..16) — wires 0..16
    let br = |j: usize| 2 * W + j; // BR bit j — wires 16..24
    let op = |i: usize| 3 * W + i; // 16-bit operand op[i] — wires 24..40 (s0‖s1)
    let np = W_PC;

    let mut build: Vec<Iso> = Vec::new();
    // ── Materialize op = sext(BR): op[i] = BR[i] (i<8), op[i]=BR[7] (i≥8) ──
    for i in 0..np {
        let src = if i < W { br(i) } else { br(W - 1) };
        build.push(gate_at(n, &[src, op(i)], cnot_prim()));
    }

    // ── PROVEN TK zero-ancilla in-place adder: PC += op (mod 2^np) ──────
    // (the shipped 8-bit adder, generalized from W to np bits; operand a =
    // op, target b = PC.)
    let a = op;
    let b = pc;
    let mut add: Vec<Iso> = Vec::new();
    for i in 1..np - 1 {
        add.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
    }
    for i in (1..np - 1).rev() {
        add.push(gate_at(n, &[a(i), a(i + 1)], cnot_prim()));
    }
    for i in 0..np - 1 {
        add.push(gate_at(n, &[a(i), b(i), a(i + 1)], toffoli_prim()));
    }
    add.push(gate_at(n, &[a(np - 1), b(np - 1)], cnot_prim()));
    add.push(gate_at(n, &[a(np - 2), b(np - 2), a(np - 1)], toffoli_prim()));
    for i in (1..np - 1).rev() {
        add.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
        add.push(gate_at(n, &[a(i - 1), b(i - 1), a(i)], toffoli_prim()));
    }
    for i in 1..np - 1 {
        add.push(gate_at(n, &[a(i), a(i + 1)], cnot_prim()));
    }
    for i in 0..np - 1 {
        add.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
    }

    // ── Uncompute op (sym of the materialization) ──────────────────────
    let mut unbuild = build.clone();
    unbuild.reverse();

    let mut gates = build;
    gates.extend(add);
    gates.extend(unbuild);
    on_fields(
        &[PC_FIELD, PC_HI_FIELD, BR_FIELD, AUX_OPERAND_FIELD, AUX_PRED_FIELD],
        gates,
    )
}

// ── PC increment ───────────────────────────────────────────────────

/// PC += 1 mod 2^W_PC (A-6.4: 16-bit increment across the PC_lo → PC_hi
/// field boundary).
///
/// Implemented, like `pc_add_br`, by reusing the PROVEN zero-ancilla TK
/// in-place adder to add a **constant 1** materialized in scratch: set
/// `op = 1` (s0 bit 0), run the `W_PC`-bit TK adder `PC += op`, clear op.
/// s0 = 0 on entry required (decoder convention), restored on exit; the
/// carry crosses the byte boundary inside the adder. `Iso::sym(pc_inc())`
/// is the decrement.
///
/// Wire layout over [PC_lo, PC_hi, s0, s1] (n = 4*W = 32):
///   PC bit i (0..16) = wire i ; operand op[i] (0..16) = wire 16+i
///   (op low byte in s0, high byte in s1; only op[0] = 1 is set).
pub fn pc_inc() -> Iso {
    let n = 4 * W;
    let pc = |i: usize| i; // PC target bit (0..16)
    let op = |i: usize| 2 * W + i; // 16-bit operand op[i] in s0‖s1 (only op[0]=1)
    let np = W_PC;

    let mut gates: Vec<Iso> = Vec::new();
    // Materialize the constant 1: op[0] = 1 (NOT s0 bit 0). op[1..np] = 0.
    gates.push(at_wire(n, op(0), not_iso()));

    // PROVEN TK zero-ancilla in-place adder: PC += op (mod 2^np).
    let a = op;
    let b = pc;
    for i in 1..np - 1 {
        gates.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
    }
    for i in (1..np - 1).rev() {
        gates.push(gate_at(n, &[a(i), a(i + 1)], cnot_prim()));
    }
    for i in 0..np - 1 {
        gates.push(gate_at(n, &[a(i), b(i), a(i + 1)], toffoli_prim()));
    }
    gates.push(gate_at(n, &[a(np - 1), b(np - 1)], cnot_prim()));
    gates.push(gate_at(n, &[a(np - 2), b(np - 2), a(np - 1)], toffoli_prim()));
    for i in (1..np - 1).rev() {
        gates.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
        gates.push(gate_at(n, &[a(i - 1), b(i - 1), a(i)], toffoli_prim()));
    }
    for i in 1..np - 1 {
        gates.push(gate_at(n, &[a(i), a(i + 1)], cnot_prim()));
    }
    for i in 0..np - 1 {
        gates.push(gate_at(n, &[a(i), b(i)], cnot_prim()));
    }

    // Clear op[0] (sym of the NOT).
    gates.push(at_wire(n, op(0), not_iso()));

    on_fields(
        &[PC_FIELD, PC_HI_FIELD, AUX_OPERAND_FIELD, AUX_PRED_FIELD],
        gates,
    )
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
pub fn step_ops(rom: &[u32], ops: &[Op]) -> Iso {
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
/// (decode_execute′, 15 non-MEXCH/MEXCHR ops). Total bijection on CpuState;
/// SWAPBR is functional, branching works via `PC += BR + 1`.
///
/// A-0 shared crossbar: uses the SHARED-crossbar `decoder::decode_execute`
/// (operands routed once per step) rather than the per-op replicated
/// `decode_execute_ops(&Op::ALL)`. This is the step net the energy/ring
/// path lowers, so the sharing is what keeps `me_a3` off the OOM cliff and
/// drops step_j. Semantically identical to `step_ops(rom, &Op::ALL)`.
pub fn step(rom: &[u32]) -> Iso {
    let f = fetch(rom);
    let phases = vec![
        f.clone(),                 // fetch
        decoder::decode_execute(), // decode + execute (SHARED crossbar)
        f,                         // unfetch
        pc_add_br(),               // PC += BR
        pc_inc(),                  // PC += 1
    ];
    seq_balanced(phases)
}

/// `system_step(rom) : ((CpuState, MemState), BusMsg) <-> ((CpuState, MemState), BusMsg)` —
/// the full machine cycle including MEXCH/MEXCHR on the 3-factor carrier
/// (Stage 1b-A, ADR-0046 §4.5.8; M-D.b).
///
/// Eight phases in the in-revolution order:
/// 1. fetch         — IR ^= rom[PC]
/// 2. decode_execute — non-MEXCH/MEXCHR instructions
/// 3. emit — MEXCH: addr from IR.rt; MEXCHR: addr from reg[ra] (AUX_PRED-gated)
/// 4. mem — route MemState[addr]→head keyed on BusMsg.addr, gated-swap BusMsg.word ↔ MemState[0]
/// 5. absorb        — sym(emit): un-swap, zero BusMsg
/// 6. unfetch       — re-zero IR (same ROM word, P4)
/// 7. pc_add_br     — PC += BR
/// 8. pc_inc        — PC += 1
///
/// CpuState-only phases are lifted to the 3-factor carrier via
/// `(cpu_phase ⊗ id_Mem) ⊗ id_Bus`. The decode+execute phase (composite,
/// non-MEXCH/MEXCHR only) is lifted via `composite_phase ⊗ id_Bus`.
/// emit/mem/absorb operate natively on system_state_ty.
pub fn system_step(rom: &[u32]) -> Iso {
    let mem_id = Iso::Id(crate::types::mem_state_ty());
    let bus_id = Iso::Id(crate::types::bus_state_ty());

    // Helper: lift CpuState iso to system = ((Cpu,Mem),Bus)
    let lift_cpu =
        |iso: Iso| -> Iso { Iso::times(Iso::times(iso, mem_id.clone()), bus_id.clone()) };

    let f = fetch(rom);
    let phases = vec![
        lift_cpu(f.clone()),                                          // 1. fetch
        Iso::times(decoder::system_decode_execute(), bus_id.clone()), // 2. decode_execute (non-MEXCH)
        decoder::emit(),                                              // 3. emit
        decoder::mem_phase(),                                         // 4. mem
        decoder::absorb(),                                            // 5. absorb
        lift_cpu(f),                                                  // 6. unfetch
        lift_cpu(pc_add_br()),                                        // 7. PC += BR
        lift_cpu(pc_inc()),                                           // 8. PC += 1
    ];
    seq_balanced(phases)
}

// ── Two-body decomposition (Stage 2a) ─────────────────────────────

/// `cpu_pre(rom) : (CpuState, BusMsg) <-> (CpuState, BusMsg)` — the first
/// body of the two-body ring (Stage 2a).
///
/// Three phases on the reduced `(Cpu, Bus)` carrier:
///   1. `fetch ⊗ id_Bus`
///   2. `decode_execute ⊗ id_Bus`  (non-MEXCH only)
///   3. `emit_on_cpu_bus`
///
/// No MemState lines exist in this iso's netlist.
pub fn cpu_pre(rom: &[u32]) -> Iso {
    let bus_id = Iso::Id(crate::types::bus_state_ty());
    let f = fetch(rom);
    let phases = vec![
        Iso::times(f, bus_id.clone()),                 // fetch ⊗ id_Bus
        Iso::times(decoder::decode_execute(), bus_id), // decode_execute ⊗ id_Bus
        decoder::emit_on_cpu_bus(),                    // emit on (Cpu,Bus)
    ];
    seq_balanced(phases)
}

/// `cpu_post(rom) : (CpuState, BusMsg) <-> (CpuState, BusMsg)` — the third
/// body of the two-body ring (Stage 2a).
///
/// Four phases on the reduced `(Cpu, Bus)` carrier:
///   1. `sym(emit_on_cpu_bus)`  = absorb on (Cpu,Bus)
///   2. `unfetch ⊗ id_Bus`
///   3. `pc_add_br ⊗ id_Bus`
///   4. `pc_inc ⊗ id_Bus`
///
/// No MemState lines exist in this iso's netlist.
pub fn cpu_post(rom: &[u32]) -> Iso {
    let bus_id = Iso::Id(crate::types::bus_state_ty());
    let f = fetch(rom); // unfetch == fetch (self-inverse XOR mask)
    let phases = vec![
        Iso::sym(decoder::emit_on_cpu_bus()),    // absorb on (Cpu,Bus)
        Iso::times(f, bus_id.clone()),           // unfetch ⊗ id_Bus
        Iso::times(pc_add_br(), bus_id.clone()), // pc_add_br ⊗ id_Bus
        Iso::times(pc_inc(), bus_id),            // pc_inc ⊗ id_Bus
    ];
    seq_balanced(phases)
}
