# W_BR-b — the two-byte come-from δu (RUN-correct W_BR=16) — design (advisor, 2026-07-31)

> **⚠️ IMPLEMENTATION STATUS (2026-08-01): IN PROGRESS on branch `wbr-b-comefrom`
> — NOT merged.** This design (through REVISION 2) is GO and the come-from
> ARITHMETIC was implemented + proven correct (Fable impl-review FIX-THEN-SHIP;
> exhaustive adder + involution + `vart_halve` K4 reject→lower + fast-oracle==eval +
> byte-identity golden all green; MAJOR-1/2/3 fixed). BUT the full authority caught a
> **wide-step-NETLIST bug**: `w_br16_step_type_checks` SeqMismatches — the wide
> `swapbr_instr` decode (`fetch_rs ; with_target(R_RD, swapbr_core) ; sym(fetch_rs)`,
> decoder.rs ~1335) has a field-ARITY mismatch at w_br=16 (the fast oracle/interpreter
> passed; only the netlist is malformed — fast≠real). Plus a stale test
> (`mdd_reject_wcet_overflow`). **RESUME:** fix the wide `swapbr_instr` arity + update
> the stale test, then re-verify via the NETLIST (not just the fast oracle) + the real
> `system_step` authority. Full diagnosis: `docs/reviews/wbr-b-review.md` (on the branch).

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

---

## REVISION 1 (post Fable design-review — FIX-DESIGN-FIRST)

The design-review RE-DERIVED and CONFIRMED the arithmetic (δu = 65536−iter_len; the
cap ≤ 32768; sext is a pure add at w_pc=w_br=16 — the fan-out arm is dead code, no
bit-15 special case; the two-transposition SWAPBR is a genuine involution and
W_BR-a's chain is the diagnosed 3-cycle). But it found 4 MAJOR design flaws around
the arithmetic. This revision addresses each; implementation must follow the REVISED
design, and a quick re-review of the revised bracket trace precedes coding.

**F1 (ISA — the SWAPBR second operand). ADOPT `Instr::Swapbr { rd, rs }`.** The
two-transposition swap `swap(rb_lo,BR₀); swap(rb_hi,BR₁)` needs TWO register
operands; today `Swapbr{rd}` encodes one and decode drops rs/rt (asm.rs:389,450).
Use the existing `rs` instruction field for the hi register. The decoder's Group-A
`fetch_rs` already swap-routes `r[rs]` into the s0 slot for SWAPBR words
(decoder.rs:1271), so gated Fredkins `s0-slot ↔ BR₁` complete the pairing
reversibly. REQUIRED for the revision: (a) `asm.rs` encode/decode round-trip the rs
field for opcode 0b1000; (b) the interpreter `Swapbr` arm swaps both (rd↔BR_lo,
rs↔BR_hi); (c) at w_br=8 `rs` is unused (rs=0) and the decode/emit is
BYTE-IDENTICAL (the golden guards this). NOTE this makes SWAPBR a 2-register op at
w_br=16 GLOBALLY (see F4).

**F2 (the silent off-by-4 — δu must be minted from the EMITTED span).** The 16-bit
bracket adds in-span instructions (per the review: top +CSWAP +SUBI, bottom +ADDI
+CSWAP), so `LOOP_OVERHEAD` 10→14 and the out-of-span spine 4→6 AT w_br=16. `iter_len
= body_len + LOOP_OVERHEAD` feeds δu = 65536−iter_len; minting δu from the pre-widen
overhead lands at Ltop+4 (no SWAPBR there) → BR never absorbed → PC walks off, every
wide loop, iteration 1. FIX: make LOOP_OVERHEAD/spine `w_br`-dependent (10/4 at
w_br=8, 14/6 at w_br=16), compute δu from the ACTUAL emitted iter_len, and keep the
`debug_assert_eq!(instrs.len(), spine + iter_len)` — it must hold at the new
constants (this assert is the F2 tripwire).

**F3 (register frame — cross-paired +1, with a fit check).** The token passes
through THREE carriers (temp mint/burn, rb route→Lend, rc catch→Ltop), so a naïve
hi-partner each is +3 (7 aux → does NOT fit NUM_REGS=8 with tag+carrier). ADOPT the
review's cross-paired scheme: Lend pair `(rb, rc)`, Ltop pair `(rc, rb)` — each is
the other's hi partner, both provably 0 at every swap on entry/continue/exit
(review traced fwd+bwd; closes with BR=0 and all aux terminal-zero) — plus ONE new
`temp_hi` for the staged/burned copy (temp_hi must be distinct from the rb-pair, else
δu_hi reaches BR on the exit path). Net +1 aux register. FIT CHECK (required, not
asserted-by-hope): at +1, a wide loop needs 5 aux + tag + carrier ≤ 8 → carrier ≤ 2
leaves. `vart_halve` K=4,HB=1 carrier = [tag,v,h] = 3 leaves → **compute the exact
count**; if it doesn't fit at +1, W_BR-c's derive must not pick w_br=16 for it (gate
on frame fit, like derive_w_pc gates on the guards). `allocate_aux_frame` draws
lowest-free with NO adjacency guarantee — the cross-paired scheme must NOT assume
rb/rc adjacency.

**F4 (skip blocks — the pair discipline is GLOBAL). Extend emit_skip_block.** At
w_br=16 EVERY SWAPBR swaps a register pair with (BR₀,BR₁), including
`emit_skip_block`'s branch + come-from SWAPBRs, and wide-loop bodies CONTAIN skip
blocks. A skip SWAPBR paired with a non-zero hi register wild-jumps: e.g. fall-through
(ctrl=0) leaves `temp=dist` → BR₁=dist → `PC += (dist<<8)+1`. FIX: at w_br=16 every
skip block needs a hi partner register PROVABLY 0 at swap time on ALL paths (+1
zeroed aux for body ≤ 127; full two-byte δu staging only when dist > 255). The cap
constant change at compile.rs:1020 is necessary but INSUFFICIENT. Even iter_len ≤ 128
loops inside a w_br=16 program need the full pair mint (δu_hi = 0xFF ≠ 0). So the
pair discipline attaches to the LAYOUT (w_br=16), not to the loop-vs-skip
distinction.

**F5 (cap rationale — fix the reasoning, keep the bound).** At w_pc=w_br=16, `x ↦
x+c mod 2^16` is a pure translation, so ANY iter_len ≤ 65535 lands correctly — the
top-bit ("backward ⟺ bit 15 set") condition is NOT arithmetically binding here (it
only bites when w_pc > w_br). KEEP `iter_len ≤ 32768` as a policy invariant
(future-proofs w_pc>w_br + keeps "backward ⟺ top bit set" honest), but correct the
design's stated rationale so no one later "fixes" the guard believing the sext makes
it load-bearing at w_br=16.

**F6 (interpreter layout threading).** `pc_update(pc, br: u8)` and `exec_instr` are
layout-free (compile.rs:4786,4855); the k_max trajectory walk (compile.rs:3928) runs
on `exec_instr`. Thread `w_br` (or the layout) into the fast sim + pc_update so they
carry the 16-bit BR + sext from w_br; keep the MINOR-3 hard asserts on the 8-bit path.

**Re-review + implementation order.** A quick Fable re-review of THIS revised bracket
(the cross-paired frame trace + the `Swapbr{rd,rs}` round-trip + the emitted-span δu)
precedes coding. Then implement in the F-order (ISA first: `Swapbr{rd,rs}` + golden;
then the widened bracket + emitted-span δu; then the global skip pair; then interpreter
w_br). W_BR-c's derive gates w_br=16 on the F3 frame-fit.

---

## REVISION 2 (post re-review — GO FOR IMPLEMENTATION, with 4 pinned conditions)

Re-review verdict: **GO**. It traced the full δu/register flow against the real
w_br=8 bracket (compile.rs:4060–4140) extended by F2's counts, and confirmed: the
cross-paired frame is genuinely **+1** (temp_hi) and **fits exactly 8/8** for
`vart_halve` K=4,HB=1; `Swapbr{rd,rs}` round-trips + is byte-identical at w_br=8.
Implement from THIS spec, not the F3 prose (which the re-review found self-contradictory).

### THE EMISSION SPEC (MINOR-A — use this trace, NOT the F3 prose)
The F3 sentence "both rb,rc provably 0 at every swap" is LITERALLY FALSE at the Lend
swap on the continue path — rb=δu_lo, rc=δu_hi there (that IS the delivery). The
correct w_br=16 bracket (aux frame {rc, rb, temp, ctrl, temp_hi}; δu_lo=δu&0xFF,
δu_hi=δu>>8):
- **Continue, bottom (ctrl=1):** `ADDI temp,δu_lo; ADDI temp_hi,δu_hi;
  CSWAP ctrl,temp,rb` (rb=δu_lo, temp=0); `CSWAP ctrl,temp_hi,rc` (rc=δu_hi,
  temp_hi=0). **Lend `SWAPBR(rb,rc)`** = swap(rb,BR₀); swap(rc,BR₁) →
  BR=(δu_lo,δu_hi), rb=rc=0. (δu_hi reaches BR₁ **through rc**, the bottom
  hi-carrier — NOT through temp_hi.)
- **Re-entry at Ltop:** BR=(δu_lo,δu_hi), rb=rc=0. **`SWAPBR(rc,rb)`** →
  rc=δu_lo, rb=δu_hi, BR=0. `CSWAP ctrl,rc,temp` (temp=δu_lo);
  `CSWAP ctrl,rb,temp_hi` (temp_hi=δu_hi); `SUBI temp,δu_lo; SUBI temp_hi,δu_hi`
  → all aux 0 before the body.
- **Exit (ctrl=0):** CSWAPs don't fire; rb=rc=0 → the 2-field swap is a no-op; BR
  stays 0; epilogue SUBIs burn temp/temp_hi.
- **First entry:** BR=0, rc=rb=0 → no-op; ctrl=0 → top CSWAPs don't fire; prologue
  mint burned by the top SUBIs.
rb/rc live-windows are position-disjoint (rb: bottom-CSWAP→Lend as lo-carrier, then
Ltop→top-CSWAP as hi-catcher; rc mirrors), so each is the other's hi partner → net
+1 register (temp_hi). Closes with BR=0 + all aux terminal-zero on every path.

### PINNED CONDITIONS
- **MINOR-B (rd≠rs — mandatory):** `Swapbr{rd,rs}` MUST enforce rd≠rs (apply
  `check_distinct`, as other ops do) — rd==rs at w_br=16 collapses the two disjoint
  transpositions onto one register = the P2 3-cycle trap reborn. Also `operand_locs()`
  must include rs (rs=0 doesn't move the num_regs bound). Encode `r_word(0b1000,rd,rs,0)`;
  decode already extracts rs; at w_br=8 emit rs=0 (bit-identical). The decoder adds gated
  Fredkins `s0-slot↔BR₁` under `br_fields()==2` ONLY (w_br=8 netlist unchanged).
- **MINOR-B (fit gate):** `carrier_reg_budget = NUM_REGS − 4` → `NUM_REGS − 5` at
  w_br=16 (vart_halve K=4: n_input+1=3 ≤ 3 still passes; frame = tag+2 inputs+5 aux =
  8/8, zero slack). W_BR-c's derive picks w_br=16 ONLY when the frame fits this budget.
- **MINOR-C (iter_len/spine constants):** LOOP_OVERHEAD 10→14 AND the separate
  hardcoded `this_wcet` `spine = 4` → 6 at w_br=16 (compile.rs — grep both). `vart_halve`
  K=4,HB=1 iter_len is **137** (not 133) under LOOP_OVERHEAD=14 — pin all goldens/reject
  bounds at 137. The `debug_assert_eq!(instrs.len(), spine+iter_len)` is the drift tripwire.
- **MINOR-D (skip-dist gate):** at w_br=16 a zeroed hi partner + a SINGLE 8-bit Addi
  handles dist ≤ 255 (positive 16-bit); dist > 255 needs full two-byte staging. The
  skip-dist gate must match the staging actually emitted (don't let the cap constant admit
  dist ≤ 32767 while only a single Addi is emitted).
- The 4 SWAPBR emission sites (grep-confirmed): `lower_trace` Ltop/Lend (≈4076/4123) +
  `emit_skip_block` branch/come-from (≈1068/1074). The Swapbr-scan at compile.rs≈4550 must
  recognize the 2-operand form (mechanical).

### IMPLEMENTATION ORDER (F-order; each guarded)
1. **ISA:** `Swapbr{rd,rs}` (encode/decode round-trip + rd≠rs + operand_locs + the
   gated `s0-slot↔BR₁` decoder Fredkins under br_fields()==2). Gate: byte-identity golden
   green (w_br=8, rs=0) + a decode round-trip test.
2. **Bracket:** the REVISION-2 emission trace + emitted-span δu + LOOP_OVERHEAD/spine
   constants (w_br-dependent). Gate: the debug_assert tripwire + w_br=8 byte-identical.
3. **Global skip pair** at w_br=16 (emit_skip_block hi partner). 
4. **Interpreter** w_br threading (Swapbr 2-field swap + pc_update 16-bit BR + sext).
Then the AUTHORITY (mirror B-2.2b): exhaustive adder + P2 involution + **real
run_on_cpu==eval on vart_halve K=4 (iter_len=137) reject→run** + byte-identity golden.
Full L19/L20/L21: Fable impl-review + advisor real authority before merge.
