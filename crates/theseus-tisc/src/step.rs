//! TISC-1 `step`: the full fetch-decode/execute-unfetch-advance machine
//! cycle, `step(rom) : State <-> State` (tisc-1-spec §4, ADR-0034).
//!
//! The ROM is a compile-time-constant slice of 32-bit instruction words
//! (A-0 widened the instruction word 16 → 32 bits);
//! `step` is one iso composed of five phases:
//!
//! 1. **Fetch** -- CNOT the ROM word at address PC onto the (zero) IR.
//!    For each address `p` with `rom[p] != 0`, a predicate `PC == p`
//!    (the AND of the 8 PC-bit literals, computed into s1 bit 0 via a
//!    Toffoli chain through s1 bits 1..=6, exactly the decoder's
//!    predicate technique) gates plain CNOTs from the predicate wire
//!    onto the IR bits where `rom[p]` has a 1 -- the constant is folded
//!    into *which* CNOTs exist, so each set bit costs one K1 CNOT and
//!    the whole entry costs 14 Toffolis (7 compute + 7 uncompute) +
//!    popcount(rom[p]) CNOTs. Addresses ≥ `rom.len()` (and zero words)
//!    have no entry: IR stays 0.
//!
//!    Fetch is an XOR mask keyed on PC, hence self-inverse for a fixed
//!    PC -- the same iso serves as the unfetch.
//!
//! 2. **Decode + Execute** -- [`decoder::decode_execute`] (already built):
//!    dispatches on IR's opcode bits and mutates the named registers,
//!    preserving IR. Per ADR-0034 D1, **no instruction writes PC**:
//!    SWAPBR swaps BR ↔ r[rd] (Pendulum semantics), and the execute
//!    phase's write set is restricted to `{r0..r7, s0..s7, BR}`.
//!
//! 3. **Unfetch** -- re-zero IR with the SAME ROM word (P4: IR provably
//!    zero between steps). Because PC is constant across fetch → execute
//!    → unfetch (D1: nothing writes PC; the PC update runs after the
//!    unfetch), the unfetch XORs the same ROM word the fetch did, for
//!    every instruction including SWAPBR, and IR returns to 0
//!    unconditionally (ADR-0034 D3).
//!
//! 4. **PC += BR** -- add the branch register (an offset) into PC via
//!    the Takahashi-Kunihiro zero-ancilla in-place ripple adder (≤ K2).
//!    BR is preserved (it is the operand of the add). BR = 0 is
//!    fall-through.
//!
//! 5. **PC += 1** -- the existing 12-Toffoli / 7-CNOT ripple increment.
//!
//! Together phases 4-5 implement the uniform end-of-step PC update
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
//! 4. `A : (PC, BR, rest) ↦ (PC + BR + 1, BR, rest)` is a bijection --
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
//! which adds s0 to r0 -- the identity only when s0 = 0); s1 = 0 on
//! entry, restored to 0; no register aliasing in ROM words (P1
//! checker's job).

use theseus_core::Iso;

use crate::decoder::{self, cnot_prim, on_fields_l, toffoli_prim, Op};
use crate::plumbing::{at_wire, gate_at, not_iso, seq_balanced};
use crate::types::{Layout, W_PC};

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
// -- so the `W_PC`-dependent cost (the 16-A_fetch high-zero AND) is paid
// once, not once per entry, and the per-entry predicate cost is ∝ log₂ B,
// independent of W_PC. This is the ADR-0034 fetch-injectivity witness:
// `2^A_fetch ≥ B + S_max > every reachable PC`, so all reachable PCs are
// distinct in `A_fetch` bits, and the shared `hz` gate makes any
// unreachable high PC PARK (no false match) rather than corrupt.

// The fetch core lifts over 8 fields [PC_lo, PC_hi, IR_b0..b3, s0, s1]
// (n = 8·w_ctl = 64 at default). PC bit i (0..w_pc) = wire i; IR bit b =
// wire w_pc+b; s0 bit j = wire 6·w_ctl+j; s1 bit j = wire 7·w_ctl+j. BOTH
// scratch bytes are AND-chain space: fetch runs first in the step (and the
// unfetch after decode restores s0=0), so s0 and s1 are both 0 at
// fetch/unfetch time. This gives 2·w_ctl-2 usable chain-intermediate bits,
// enough for the up-to-(w_pc-1)-input high-zero AND -- a single s1 byte (the
// 8-bit-PC design) is too narrow for the wider high-zero gate.
//
// B-2.1a: the fetch core is entirely control-region -- PC (`w_pc` bits over
// `pc_fields` control words), the four IR bytes, and s0/s1 (`w_ctl`). The
// `FetchWires` bundle below derives every wire base from `layout.w_ctl` /
// `layout.w_pc`; at `Layout::default()` (w_ctl = 8, w_pc = 16) these are the
// shipped 8·W / W_PC / 6·W / 7·W offsets, so the fetch net is byte-identical.

/// B-2.2a (review MAJOR-1): PC must occupy a whole number of `w_ctl`-wide
/// fields. `pc_fields()` is **floor** division (`w_pc / w_ctl`), so a `w_pc`
/// that is not a positive multiple of `w_ctl` (e.g. 12) would under-count the
/// PC carrier and silently route the high PC bits onto BR/operand wires
/// (`pc_add_br` writing into BR; `pc_inc` aliasing op) instead of failing.
/// The pre-refactor code panicked loudly on such a layout (the unconditional
/// `pc_hi_field()` addressed one-past-the-last field); this restores that
/// loud-failure property at every PC-builder entry. No valid layout
/// (w_pc ∈ {8, 16}, w_ctl = 8) ever trips it.
fn assert_pc_carrier(layout: &Layout) {
    assert!(
        layout.pc_fields() >= 1 && layout.w_pc == layout.pc_fields() * layout.w_ctl,
        "PC carrier invariant violated: w_pc={} is not a positive multiple of w_ctl={} \
         (pc_fields()={}); PC would alias BR/operand wires",
        layout.w_pc,
        layout.w_ctl,
        layout.pc_fields(),
    );
}

/// W_BR-a (mirror of `assert_pc_carrier`, review B-2.2a MAJOR-1): BR must
/// occupy a whole number of `w_ctl`-wide fields. `br_fields()` is **floor**
/// division (`w_br / w_ctl`), so a `w_br` that is not a positive multiple of
/// `w_ctl` (e.g. 12) would under-count the BR carrier and silently route the
/// high BR bits onto a neighboring field. This restores loud failure at every
/// BR-builder entry. No valid layout (w_br ∈ {8, 16}, w_ctl = 8) trips it.
pub(crate) fn assert_br_carrier(layout: &Layout) {
    assert!(
        layout.br_fields() >= 1 && layout.w_br == layout.br_fields() * layout.w_ctl,
        "BR carrier invariant violated: w_br={} is not a positive multiple of w_ctl={} \
         (br_fields()={}); BR would alias neighboring fields",
        layout.w_br,
        layout.w_ctl,
        layout.br_fields(),
    );
}

/// Control-region wire bases for the fetch core `[PC(pc_fields words),
/// IR_b0..b3, s0, s1]` of a given `layout`. The core is `pc_fields + 6`
/// control words wide: PC bit i (0..w_pc) = wire i; IR bit b = wire
/// `w_pc + b`; s0 bit j = wire `(pc_fields+4)·w_ctl + j`; s1 bit j = wire
/// `(pc_fields+5)·w_ctl + j`. At `Layout::default()` (pc_fields = 2) this is
/// the shipped 8-word core (s0 = 6·w_ctl, s1 = 7·w_ctl, n = 8·w_ctl),
/// byte-identical.
#[derive(Clone, Copy)]
struct FetchWires {
    n: usize,
    ir0: usize,
    s0: usize,
    s1: usize,
    hz: usize,
    pred: usize,
    w_ctl: usize,
    w_pc: usize,
}

impl FetchWires {
    fn new(layout: &Layout) -> Self {
        assert_pc_carrier(layout);
        let wc = layout.w_ctl;
        // B-2.2a: the fetch core carrier is `[PC(pc_fields words), IR_b0..b3,
        // s0, s1]` = `pc_fields + 6` control words. At the default (pc_fields
        // = 2) this is 8 words with s0 = 6·wc, s1 = 7·wc, n = 8·wc -- the
        // shipped layout (byte-identical). At w_pc = 8 (pc_fields = 1) the
        // core is 7 words: PC drops one field, so s0/s1/n each shift down by
        // one word. `fetch_fields` returns the matching field list.
        let pcf = layout.pc_fields();
        let s0 = (pcf + 4) * wc;
        let s1 = (pcf + 5) * wc;
        FetchWires {
            n: (pcf + 6) * wc,
            ir0: layout.w_pc, // IR bit 0 (PC occupies pc_fields=w_pc/w_ctl words)
            s0,
            s1,
            hz: s1 + (wc - 1), // hz lands in s1's top bit
            pred: s1,          // per-entry predicate in s1 bit 0
            w_ctl: wc,
            w_pc: layout.w_pc,
        }
    }

    /// AND-chain intermediate wire `k`: s0 bits `0..w_ctl`, then s1 bits
    /// `1..w_ctl-1` (skips s1 bit 0 = pred and s1 top bit = hz).
    fn chain(&self, k: usize) -> usize {
        if k < self.w_ctl {
            self.s0 + k
        } else {
            self.s1 + 1 + (k - self.w_ctl)
        }
    }
}

/// A-6.4 widen-PC (F-2): the per-entry predicate `pc_pred(addr, a)` runs an
/// `(a+1)`-input AND (the `hz` control + `a` low PC bits) whose chain needs
/// `a-1` intermediate scratch slots `fetch_chain(0..a-1)` -- the highest is
/// `fetch_chain(a-2)`. There are only 14 usable slots (s0 bits 0..8 + s1 bits
/// 1..7): `fetch_chain(13) = 62` (s1 bit 6) is safe, but `fetch_chain(14) =
/// 63 = FETCH_HZ_WIRE` -- the chain intermediate lands ON the shared high-zero
/// bit and INVERTS the predicate (review probe B: rom_len 32770, a=16, word at
/// 0x8000 not fetched, 0x8001 falsely fetched). No collision requires
/// `a-2 ≤ 13`, i.e. `A_fetch ≤ 15`, i.e. `rom_len ≤ 2^15 = 32768`. Beyond
/// that, fetch REJECTS (see `fetch`), it does NOT corrupt. A wider chain
/// space (2-byte scratch) is the follow-up for ROMs > 32768 words.
pub const MAX_FETCH_A: usize = W_PC - 1; // 15
/// Largest ROM length the narrow-fetch predicate can distinguish without the
/// F-2 chain/hz-wire collision (`2^MAX_FETCH_A = 32768`).
pub const MAX_FETCH_ROM_LEN: usize = 1 << MAX_FETCH_A;

/// The fields the fetch core lifts over: the `pc_fields()` PC words (PC_lo,
/// then PC_hi only when `pc_fields() >= 2`), the four IR bytes, s0, s1.
///
/// B-2.2a: at the default (`w_pc = 16`, `pc_fields = 2`) this returns the
/// shipped 8-field list `[PC_lo, PC_hi, IR_b0..b3, s0, s1]` (byte-identical).
/// At `w_pc = 8` (`pc_fields = 1`) PC_hi does not exist (`pc_hi_field()`
/// would address one-past-the-last CPU field), so it is dropped and the list
/// is 7 fields. `FetchWires` derives its wire bases from the same
/// `pc_fields()`, so the two stay in lockstep.
fn fetch_fields(layout: &Layout) -> Vec<usize> {
    let mut fields = vec![layout.pc_field()];
    if layout.pc_fields() >= 2 {
        fields.push(layout.pc_hi_field());
    }
    fields.extend([
        layout.ir_byte_field(0),
        layout.ir_byte_field(1),
        layout.ir_byte_field(2),
        layout.ir_byte_field(3),
        layout.scratch_field(0), // s0 (AUX_OPERAND_FIELD)
        layout.scratch_field(1), // s1 (AUX_PRED_FIELD)
    ]);
    fields
}

/// Bits needed to hold every reachable PC (= every ROM address plus the
/// parked/forward-skip zone). `A_fetch = ⌈log₂(rom.len())⌉`, ≥ 1. Since
/// the assembler caps `rom.len() ≤ 2^A_addr` and every reachable PC is
/// `< rom.len()` on a live entry (higher PCs are parked, word 0, no
/// entry), `A_fetch` low bits distinguish every entry, and the shared
/// high-zero gate rejects PC ≥ 2^A_fetch. Clamped to `W_PC`.
pub fn fetch_addr_bits(rom_len: usize) -> usize {
    fetch_addr_bits_with(rom_len, W_PC)
}

/// B-2.2a: layout-aware [`fetch_addr_bits`]. Clamps `A_fetch` to the
/// program's own `w_pc` rather than the const `W_PC` (=16). At `w_pc = 16`
/// this is identical to [`fetch_addr_bits`] (byte-identical default path).
/// At a reduced `w_pc` the clamp is the correct per-program cap: DF-3 flags
/// the const-`W_PC` clamp as a latent 16-pin on a per-program path (it never
/// misfires today since `rom_len ≤ 2^(w_pc-1)` ⇒ `A_fetch ≤ w_pc - 1`, but
/// threading it is the defensive fix).
pub fn fetch_addr_bits_with(rom_len: usize, w_pc: usize) -> usize {
    if rom_len <= 1 {
        1
    } else {
        let a = (usize::BITS - (rom_len - 1).leading_zeros()) as usize;
        a.clamp(1, w_pc)
    }
}

/// Compute the shared "high bits all zero" gate `hz = AND(¬PC[a..W_PC])`
/// into `FETCH_HZ_WIRE` (7-input-max AND via NOTs + a Toffoli chain in
/// s1 bits 1..6). When `a == W_PC` there are no high bits: `hz = 1`
/// (a plain NOT sets s1 bit 7). `Iso::sym` uncomputes it. Only fetch
/// entries whose address is ≥ 0 need it; it is threaded as an extra
/// control into every per-entry predicate.
fn pc_high_zero(layout: &Layout, a: usize) -> Iso {
    let fw = FetchWires::new(layout);
    let w_pc = fw.w_pc;
    if a >= w_pc {
        // No high bits: hz ≡ 1. Set s1 top bit.
        return on_fields_l(
            layout,
            &fetch_fields(layout),
            vec![at_wire(fw.n, fw.hz, not_iso())],
        );
    }
    let hi: Vec<usize> = (a..w_pc).collect();
    // NOT every high bit so "all zero" ⇒ "all negated = 1".
    let nots: Vec<Iso> = hi.iter().map(|&i| at_wire(fw.n, i, not_iso())).collect();
    let mut gates = nots.clone();
    if hi.len() == 1 {
        // Single high bit: hz ^= ¬PC[a].
        gates.push(gate_at(fw.n, &[hi[0], fw.hz], cnot_prim()));
    } else {
        // Compute the AND chain into intermediate scratch (NOT hz), CNOT
        // the final AND onto hz, then UNCOMPUTE the chain -- so hz alone is
        // set and every fetch_chain scratch bit is restored to 0 before
        // the per-entry predicates reuse them.
        let mut chain: Vec<Iso> = Vec::new();
        let mut acc = hi[0];
        let mut last = acc;
        for (t, &i) in hi[1..].iter().enumerate() {
            let tgt = fw.chain(t);
            chain.push(gate_at(fw.n, &[acc, i, tgt], toffoli_prim()));
            acc = tgt;
            last = tgt;
        }
        gates.extend(chain.clone());
        gates.push(gate_at(fw.n, &[last, fw.hz], cnot_prim())); // hz ^= AND
        chain.reverse();
        gates.extend(chain); // uncompute the AND chain (restore scratch)
    }
    gates.extend(nots); // restore the negated PC bits
    on_fields_l(layout, &fetch_fields(layout), gates)
}

/// Compute `pred = hz ∧ (PC[0..a] == addr)` into the s1-bit-0 pred wire:
/// NOTs on the zero low-literals, an `(a+1)`-input AND (the `a` low PC
/// literals plus the shared `hz` control) via a Toffoli chain through the
/// fetch-chain slots, NOTs undone. `Iso::sym` uncomputes it.
fn pc_pred(layout: &Layout, addr: usize, a: usize) -> Iso {
    let fw = FetchWires::new(layout);
    // Low literals (bits 0..a) NOT-ed where addr bit is 0.
    let nots: Vec<Iso> = (0..a)
        .filter(|i| (addr >> i) & 1 == 0)
        .map(|i| at_wire(fw.n, i, not_iso()))
        .collect();
    let mut gates = nots.clone();
    // AND chain over controls [hz, PC0, PC1, ..., PC_{a-1}] → pred.
    let controls: Vec<usize> = std::iter::once(fw.hz).chain(0..a).collect();
    if controls.len() == 1 {
        // a == 0 (impossible, a≥1) -- pred = hz.
        gates.push(gate_at(fw.n, &[controls[0], fw.pred], cnot_prim()));
    } else {
        let mut acc = controls[0];
        for (t, &c) in controls[1..].iter().enumerate() {
            let tgt = if t == controls.len() - 2 {
                fw.pred
            } else {
                fw.chain(t)
            };
            gates.push(gate_at(fw.n, &[acc, c, tgt], toffoli_prim()));
            acc = tgt;
        }
    }
    gates.extend(nots);
    on_fields_l(layout, &fetch_fields(layout), gates)
}

/// One ROM entry: iff `hz ∧ PC[0..a] == addr`, XOR the constant 32-bit
/// `word` into IR.
fn fetch_entry(layout: &Layout, addr: usize, word: u32, a: usize) -> Iso {
    let fw = FetchWires::new(layout);
    let pred = pc_pred(layout, addr, a);
    let cnots: Vec<Iso> = (0..32usize)
        .filter(|&b| (word >> b) & 1 == 1)
        .map(|b| gate_at(fw.n, &[fw.pred, fw.ir0 + b], cnot_prim()))
        .collect();
    let body = on_fields_l(layout, &fetch_fields(layout), cnots);
    Iso::seq(pred.clone(), Iso::seq(body, Iso::sym(pred)))
}

/// Fetch phase: `IR ^= rom[PC]` (IR ← rom[PC] when IR = 0 on entry,
/// per P4). Addresses ≥ `rom.len()` and zero words leave IR unchanged;
/// PCs with any bit ≥ A_fetch set are parked (the shared high-zero gate
/// matches nothing). Self-inverse for a fixed PC, so it doubles as the
/// unfetch. Requires s1 = 0 on entry (restored on exit).
pub fn fetch(rom: &[u32]) -> Iso {
    fetch_with(&Layout::default(), rom)
}

/// B-2.1a: layout-parameterized [`fetch`]. The zero-arg entry delegates at
/// `Layout::default()`.
pub fn fetch_with(layout: &Layout, rom: &[u32]) -> Iso {
    let w_pc = layout.w_pc;
    assert!(
        rom.len() <= 1 << w_pc,
        "ROM exceeds the {w_pc}-bit PC address space"
    );
    // A-6.4 F-2: at A_fetch = w_pc (rom_len > 2^(w_pc-1)) the per-entry
    // predicate's AND-chain intermediate collides with the shared high-zero
    // wire and INVERTS the predicate (aliasing, review probe B). Reject
    // rom_len beyond the narrow-fetch chain budget rather than silently
    // corrupt. A 2-byte-scratch chain is the follow-up for larger ROMs.
    let max_rom_len = 1usize << (w_pc - 1);
    assert!(
        rom.len() <= max_rom_len,
        "ROM length {} exceeds the narrow-fetch predicate limit {} \
         (A_fetch would be {}, colliding the AND-chain with the high-zero wire; \
         A-6.4 F-2). A wider (2-byte) fetch-chain scratch is the follow-up.",
        rom.len(),
        max_rom_len,
        w_pc,
    );
    let a = fetch_addr_bits_with(rom.len(), w_pc);
    let entries: Vec<Iso> = rom
        .iter()
        .enumerate()
        .filter(|&(_, &w)| w != 0)
        .map(|(p, &w)| fetch_entry(layout, p, w, a))
        .collect();
    if entries.is_empty() {
        // Identity over the ACTUAL (possibly reduced) carrier, not the default
        // (B-2.1b review F3): an all-zero/empty ROM under a reduced layout must
        // still type-check against `state_ty_with(layout)`.
        Iso::Id(crate::types::state_ty_with(layout))
    } else {
        // Shared high-zero gate computed once, wrapping all entries.
        let hz = pc_high_zero(layout, a);
        Iso::seq(hz.clone(), Iso::seq(seq_balanced(entries), Iso::sym(hz)))
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
// whole 8→16-bit widen (8 added PC bits) ≈ ~0.5 %/bit -- under the design's
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
// bit for PC position i is `sext(BR)[i] = BR[min(i, 7)]` -- the sign bit
// BR[7] is fanned out (read-only control) into PC bits 8..15. BR itself
// is preserved (it is only a control). Wire layout over the 4 fields
// [PC_lo, PC_hi, BR, s1] (n = 32): PC bit i (0..16) = wire i; BR bit j
// (0..8) = wire 16+j; the ancilla carry = wire 24 (s1 bit 0).

/// `PC += sext(BR) (mod 2^W_PC)`. BR (operand) is preserved; PC (target,
/// 16-bit) is modified.
///
/// Approach (reuses the PROVEN zero-ancilla Takahashi-Kunihiro in-place
/// adder that shipped at 8-bit): materialize the 16-bit sign-extended
/// operand `op = sext(BR)` in scratch -- low byte in s0 (copied from BR),
/// high byte in s1 (all eight bits = BR[7]) -- run the `W_PC`-bit TK adder
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
    pc_add_br_with(&Layout::default())
}

/// PROVEN zero-ancilla Takahashi-Kunihiro in-place ripple adder: `b += a`
/// mod `2^np`, over an `n`-wire register. `a`/`b` map bit index `i` (0..np)
/// to its wire; `a` is the operand and `b` the (in-place) target. Shared by
/// [`pc_add_br_with`] (a = op = sext(BR)) and [`pc_inc_with`] (a = constant 1)
/// so the two adders cannot drift. Returns the gate sequence; the caller
/// materializes/uncomputes `a` around it.
fn tk_add_inplace<A, B>(n: usize, np: usize, a: A, b: B) -> Vec<Iso>
where
    A: Fn(usize) -> usize,
    B: Fn(usize) -> usize,
{
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
    add.push(gate_at(
        n,
        &[a(np - 2), b(np - 2), a(np - 1)],
        toffoli_prim(),
    ));
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
    add
}

/// B-2.1a: layout-parameterized [`pc_add_br`]. PC/BR/s0/s1 are all
/// control-region (`w_ctl`); PC is `w_pc` bits. The zero-arg entry
/// delegates at `Layout::default()`.
pub fn pc_add_br_with(layout: &Layout) -> Iso {
    assert_pc_carrier(layout); // review MAJOR-1: PC must occupy whole w_ctl fields
    assert_br_carrier(layout); // W_BR-a: BR must occupy whole w_ctl fields
    let wc = layout.w_ctl;
    // B-2.2a / W_BR-a: the field list is `[PC(pc_fields words), BR(br_fields
    // words), s0, s1]` = `pc_fields + br_fields + 2` control words. At the
    // default (pc_fields = 2, br_fields = 1) this is `[PC_lo, PC_hi, BR, s0,
    // s1]` (n = 5·wc), byte-identical. At w_br = 16 (br_fields = 2) BR spans
    // two words and op/s slide down one word; at w_pc = 8 (pc_fields = 1)
    // PC_hi is dropped so everything slides up one word.
    let pcf = layout.pc_fields();
    let brf = layout.br_fields();
    let wbr = layout.w_br;
    let n = (pcf + brf + 2) * wc;
    let pc = |i: usize| i; // PC target bit i (0..w_pc), spans the pcf PC words
    let br = |j: usize| pcf * wc + j; // BR bit j (0..w_br), spans the brf BR words
    let op = |i: usize| (pcf + brf) * wc + i; // w_pc-bit operand op[i] (s0‖s1)
    let np = layout.w_pc;

    let mut build: Vec<Iso> = Vec::new();
    // ── Materialize op = sext(BR): op[i] = BR[i] (i<w_br), op[i]=BR[msb] (i≥) ──
    // W_BR-a: the sext boundary is `w_br` (was `w_ctl` when BR was one field).
    // At w_br=8 (=wc) this is byte-identical to the shipped single-field sext.
    // At w_br=16=w_pc the extension is empty (op = BR verbatim); the SEMANTICS
    // of the come-from arithmetic at w_br=16 are proved in W_BR-b, not here.
    for i in 0..np {
        let src = if i < wbr { br(i) } else { br(wbr - 1) };
        build.push(gate_at(n, &[src, op(i)], cnot_prim()));
    }

    // ── PROVEN TK zero-ancilla in-place adder: PC += op (mod 2^np) ──────
    // (the shipped 8-bit adder, generalized from W to np bits; operand a =
    // op, target b = PC.)
    let add = tk_add_inplace(n, np, op, pc);

    // ── Uncompute op (sym of the materialization) ──────────────────────
    let mut unbuild = build.clone();
    unbuild.reverse();

    let mut gates = build;
    gates.extend(add);
    gates.extend(unbuild);
    // B-2.2a / W_BR-a: `[PC(pc_fields words), BR(br_fields words), s0, s1]`;
    // PC_hi appears only when pc_fields >= 2 (dropped at w_pc = 8), and the BR
    // list is all `br_fields()` BR words (one at the default, two at w_br=16).
    // The `br`/`op` closures above are keyed off `pcf`/`brf`, so they track the
    // field-list order.
    let mut fields = vec![layout.pc_field()];
    if pcf >= 2 {
        fields.push(layout.pc_hi_field());
    }
    for k in 0..brf {
        fields.push(layout.br_word_field(k));
    }
    fields.extend([layout.scratch_field(0), layout.scratch_field(1)]);
    on_fields_l(layout, &fields, gates)
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
    pc_inc_with(&Layout::default())
}

/// B-2.1a: layout-parameterized [`pc_inc`]. PC/s0/s1 are control-region
/// (`w_ctl`); PC is `w_pc` bits. The zero-arg entry delegates at
/// `Layout::default()`.
pub fn pc_inc_with(layout: &Layout) -> Iso {
    assert_pc_carrier(layout); // review MAJOR-1: PC must occupy whole w_ctl fields
    let wc = layout.w_ctl;
    // B-2.2a: the field list is `[PC(pc_fields words), s0, s1]` =
    // `pc_fields + 2` control words. At the default (pc_fields = 2) this is
    // `[PC_lo, PC_hi, s0, s1]` (n = 4·wc), byte-identical. At w_pc = 8
    // (pc_fields = 1) PC_hi is dropped, so op (s0‖s1) slides down one word to
    // slot pcf and n = 3·wc.
    let pcf = layout.pc_fields();
    let n = (pcf + 2) * wc;
    let pc = |i: usize| i; // PC target bit (0..w_pc), spans the pcf PC words
    let op = |i: usize| pcf * wc + i; // w_pc-bit operand op[i] in s0‖s1 (only op[0]=1)
    let np = layout.w_pc;

    let mut gates: Vec<Iso> = Vec::new();
    // Materialize the constant 1: op[0] = 1 (NOT s0 bit 0). op[1..np] = 0.
    gates.push(at_wire(n, op(0), not_iso()));

    // PROVEN TK zero-ancilla in-place adder: PC += op (mod 2^np).
    gates.extend(tk_add_inplace(n, np, op, pc));

    // Clear op[0] (sym of the NOT).
    gates.push(at_wire(n, op(0), not_iso()));

    // B-2.2a: `[PC(pc_fields words), s0, s1]`; PC_hi appears only when
    // pc_fields >= 2 (dropped at w_pc = 8). The `op` closure above is keyed
    // off `pcf`, so it tracks the field-list order.
    let mut fields = vec![layout.pc_field()];
    if pcf >= 2 {
        fields.push(layout.pc_hi_field());
    }
    fields.extend([layout.scratch_field(0), layout.scratch_field(1)]);
    on_fields_l(layout, &fields, gates)
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

/// `step(rom) : CpuState <-> CpuState` -- the ring machine cycle
/// (decode_execute′, 15 non-MEXCH/MEXCHR ops). Total bijection on CpuState;
/// SWAPBR is functional, branching works via `PC += BR + 1`.
///
/// A-0 shared crossbar: uses the SHARED-crossbar `decoder::decode_execute`
/// (operands routed once per step) rather than the per-op replicated
/// `decode_execute_ops(&Op::ALL)`. This is the step net the energy/ring
/// path lowers, so the sharing is what keeps `me_a3` off the OOM cliff and
/// drops step_j. Semantically identical to `step_ops(rom, &Op::ALL)`.
pub fn step(rom: &[u32]) -> Iso {
    step_with(&Layout::default(), rom)
}

/// B-2.1a: layout-parameterized [`step`] -- the CORE builder threaded end to
/// end (`fetch_with`, `decode_execute_l`, `pc_add_br_with`, `pc_inc_with`).
/// The zero-arg [`step`] delegates at `Layout::default()`, so the produced
/// step net is byte-identical to the pre-B-2.1a code (the `poc_step_net`
/// golden). B-2.1b/c pass a reduced `Layout` here to get the win.
pub fn step_with(layout: &Layout, rom: &[u32]) -> Iso {
    let f = fetch_with(layout, rom);
    let phases = vec![
        f.clone(),                         // fetch
        decoder::decode_execute_l(layout), // decode + execute (SHARED crossbar)
        f,                                 // unfetch
        pc_add_br_with(layout),            // PC += BR
        pc_inc_with(layout),               // PC += 1
    ];
    seq_balanced(phases)
}

/// `system_step(rom) : ((CpuState, MemState), BusMsg) <-> ((CpuState, MemState), BusMsg)` --
/// the full machine cycle including MEXCH/MEXCHR on the 3-factor carrier
/// (Stage 1b-A, ADR-0046 §4.5.8; M-D.b).
///
/// Eight phases in the in-revolution order:
/// 1. fetch         -- IR ^= rom[PC]
/// 2. decode_execute -- non-MEXCH/MEXCHR instructions
/// 3. emit -- MEXCH: addr from IR.rt; MEXCHR: addr from reg[ra] (AUX_PRED-gated)
/// 4. mem -- route MemState[addr]→head keyed on BusMsg.addr, gated-swap BusMsg.word ↔ MemState[0]
/// 5. absorb        -- sym(emit): un-swap, zero BusMsg
/// 6. unfetch       -- re-zero IR (same ROM word, P4)
/// 7. pc_add_br     -- PC += BR
/// 8. pc_inc        -- PC += 1
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

/// `cpu_pre(rom) : (CpuState, BusMsg) <-> (CpuState, BusMsg)` -- the first
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

/// `cpu_post(rom) : (CpuState, BusMsg) <-> (CpuState, BusMsg)` -- the third
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
