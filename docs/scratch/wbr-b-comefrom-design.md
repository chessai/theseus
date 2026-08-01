# W_BR-b — the two-byte come-from δu (RUN-correct W_BR=16) — design (advisor, 2026-07-31)

**Status:** advisor design from theseus-v2 `61f1c0a` (after W_BR-a merged). This is
the HIGHEST-BAR sub-slice of the W_BR-widen line: it makes a reversible loop with
`iter_len > 128` actually LOOP correctly on the real CPU. The come-from δu
arithmetic is the single most safety-critical code in the compiler — a
sign-extension off-by-one silently mis-jumps EVERY wide loop. This design is the
input to a Fable adversarial DESIGN-review (re-derive the arithmetic) that MUST pass
before implementation (L19).

## What W_BR-a already delivered (do not redo)

`br_fields()`-driven ADDRESSING is byte-identical at w_br=8 and type-checks at
w_br=16: `pc_add_br_with` has a `br_fields()`-wide BR operand with the sext boundary
at `w_br` (identity when w_br=w_pc); `encode_with`/`decode_with` marshal a 2-field
BR; `assert_br_carrier` guards non-`w_ctl`-multiple w_br; hard truncation asserts +
the MINOR-1 ⚠️ (the test-only `swapbr_with` chain is a 3-CYCLE, not a correct swap).
`StateVals.br` is `u16` (storage). The come-from δu MINT, the runtime `swapbr_core`
2-byte swap, the interpreter's 16-bit BR, and the `iter_len ≤ 128` caps are all
UNCHANGED (this slice's job).

## The arithmetic (re-derive this adversarially)

**Goal:** after `Lend` (the come-from `SWAPBR rb` at ROM offset `iter_len-1` from
`Ltop`), the PC update `PC += sext_{w_pc}(BR) + 1` must land back at `Ltop`, i.e.
`sext(BR) + 1 = −(iter_len − 1)` ⇒ **`sext_{w_pc}(δu) = −iter_len (mod 2^w_pc)`**.

- **W_BR=8 (shipped):** `δu = 256 − iter_len` (8-bit two's-complement of `−iter_len`).
  `sext_{16}(δu) = −iter_len` requires δu's top (bit 7) set ⇒ `iter_len ≤ 128`.
- **W_BR=16 (this slice):** BR is 16-bit; at w_pc=16, `sext_{16}(BR) = BR` (IDENTITY —
  no extension). So we need **`δu = 65536 − iter_len (mod 2^16)`** = the 16-bit
  two's-complement of `−iter_len`. Backward-jump validity (δu's top bit, bit 15, set
  ⇒ genuinely negative) ⇒ **`iter_len ≤ 32768`**. So the cap lifts 128 → 32768.
  (`k_max·iter_len = B ≤ 2^16` still bounds total B via widen-PC — orthogonal.)
- **Consistency check the review MUST do:** at iter_len ≤ 128, does the W_BR=16 δu
  reduce to the W_BR=8 behavior? `δu_16 = 65536 − iter_len` vs `δu_8 = 256 − iter_len`:
  these DIFFER (0xFF.. vs 0x00FF..), but BOTH sext/interpret to `−iter_len mod 2^16`
  — δu_8 sexts from bit 7, δu_16 is already 16-bit. So a program that stays w_br=8
  keeps δu_8 (byte-identical); a w_br=16 program uses δu_16. The DERIVE (W_BR-c)
  picks w_br per loop; W_BR-b proves the w_br=16 path in isolation.

## The two hard implementation problems

### P1 — mint a 16-bit δu (the come-from staging)
Today δu is staged by `Addi rb, δu` (8-bit immediate) then `SWAPBR rb` moves it to
BR. `Addi`'s immediate is 8-bit, so a 16-bit δu needs its two bytes staged
separately: `Addi rb_lo, δu_lo; Addi rb_hi, δu_hi` into TWO registers, then a
two-field SWAPBR moves both into the 2-field BR. **Design decision:** stage δu into
two aux registers (the loop already draws an occ-aware aux frame — extend it by one
for rb_hi), matching the existing `allocate_aux_frame`. Cost: +1 aux register in the
loop frame (check against the register budget — the gcd2 probe showed the frame is
already tight; but a low-degree wide loop like `vart_halve` K=4 has spare regs).

### P2 — the REVERSIBLE two-byte SWAPBR (the W_BR-a MINOR-1 trap)
W_BR-a's `swapbr_with` sequential chain `swap(reg, BR₀); swap(reg, BR₁)` is a
**3-cycle** (wrong). The CORRECT reversible 2-byte swap uses **two DISJOINT
transpositions**: `swap(rb_lo, BR₀); swap(rb_hi, BR₁)` — different register AND
different BR field in each, so they commute and the composition is an INVOLUTION
(self-inverse, as Pendulum SWAPBR must be). **This is the key correctness insight:
never swap one register with both BR words; swap two registers with the two BR
words pairwise.** The runtime `swapbr_core` (decoder) must implement this pairing
for the come-from, and the interpreter's `Swapbr` must do the same on the model side.
Reject `swapbr_with`'s 3-cycle chain — replace, don't extend.

### P3 — interpreter 16-bit BR
`exec_instr`'s `Swapbr` and `pc_update` currently treat BR as 8-bit (`as u8`,
MINOR-4). At w_br=16 they must carry the full 16-bit BR (the two-field swap on the
model side + `pc_update(pc, br: u16)` with `sext_{w_pc}` from w_br). The MINOR-3
hard asserts guard the w_br=8 path from a stray wide BR meanwhile.

## Byte-identity + the caps
- w_br=8 stays byte-identical: all P1/P2/P3 changes gate on `br_fields()`/`w_br`; at
  w_br=8 the mint is one Addi, the swap is one transposition, the interpreter is
  8-bit — exactly today. The `poc_step_net_bit_identical` golden is the guard.
- The two caps (`compile.rs:1020` skip-block, `:4033` loop) change `1 << (W_BR-1)`
  → `1 << (layout.w_br - 1)` so they read the per-loop w_br (32768 at w_br=16).
  `vart_halve` K=4,HB=1 (iter_len=133) flips reject→run at w_br=16.

## Proof obligations (the authority — mirror B-2.2b)
1. **Exhaustive/mechanized ADR-0034 at the BR width:** `(pc_add_br_with(&w_br16);
   pc_inc_with)` == `PC + sext_{16}(BR) + 1` over the BR range (16-bit BR × PC
   sample); + the come-from identity `sext_{16}(δu) == −iter_len` for iter_len in
   [1, 32768] (sampled at boundaries 128/129/32767/32768).
2. **P2 self-inverse:** the two-transposition swap is an involution (swap∘swap = id)
   — exhaustive over the 2-field BR × 2-reg domain.
3. **REAL `run_on_cpu == eval` on an iter_len>128 loop:** `vart_halve` at K=4,HB=1
   (iter_len=133, previously rejected) now LOWERS + runs on real `system_step`,
   run==eval over the input domain, terminal-quiescent (BR/s0/s1==0 across the wide
   back-edge). This is the reproduce-the-capability test (L20).
4. Byte-identity golden green (w_br=8 unchanged, no re-pin).

## Discipline
Full L19/L20/L21. This design → **Fable adversarial DESIGN-review that RE-DERIVES the
δu arithmetic + the two-transposition involution from scratch** (find the
off-by-one / the wrong-cycle BEFORE implementation) → delegate implementation →
Fable impl-review → advisor real authority (the iter_len>128 run==eval). Do NOT
implement before the design-review clears the arithmetic.

## Open questions for the design-review
- Q1: is `iter_len ≤ 32768` the right cap, or `< 32768` (is δu=0x8000 for
  iter_len=32768 a valid backward jump or the ambiguous most-negative)? Re-derive.
- Q2: P1's +1 aux register — does `allocate_aux_frame` have room for wide loops, and
  does the derive (W_BR-c) only pick w_br=16 when the frame fits? (Interaction with
  the operand-word budget the gcd2 probe hit.)
- Q3: does `pc_add_br` at (w_pc=16, w_br=16) need ANY sext, or is it pure add? (BR is
  already full-width — confirm the sext code is a no-op, not a subtle off-by-one.)
- Q4: the come-from `δu` for a FORWARD skip block (`compile.rs:1020`) uses the same
  BR — does W_BR-b's two-byte mint apply symmetrically to skip blocks, or only loops?
