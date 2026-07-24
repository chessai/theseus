# A-0 · Instruction-word reformat + scratch-addressable operand crossbar — DESIGN

The one design choice A-0 must get right (flagged as "irreversible-feeling"): the instruction-word
bit allocation. This decides it, with the rejected alternative recorded.

## Requirement (from the A-1 POC + operand-datapath investigation)
Ops must be able to name **`reg ∪ scratch` = 16 locations** (8 registers r0..r7 + 8 scratch s0..s7)
as operands `rd/rs/rt` → **4-bit operand fields** (uniform: all three, to avoid op-specific
fragility — Cswap needs its control `rd` scratch-addressable, Ccnot needs `rs`/`rt`, etc.). The
operand crossbar (`select_reg`, decoder.rs:252-274) generalizes from `reg_field(0..8)` to an index
over `reg_field(0..8) ∪ scratch_field(0..8)` via its existing `base: impl Fn(usize)->usize` seam.
This is the *same* shared crossbar B-1 dedups across ops (crossbar-half of B-1, pulled forward).

## Current 16-bit word (the constraint)
- **R** (bit15=1): `fmt(1) opcode(4) rd(3) rs(3) rt(3) pad(2)` — 12 R-ops (Add/Sub/Xor/Exch/Rotl/Rotr/
  Cswap/Ccnot/Xchg/Swapbr/Mexch/MexchReg); operands 3-bit (8-way). FULL but for 2 pad bits.
- **I** (bit15=0): `fmt(1) opcode(3) rd(3) imm(8) pad(1)` — 5 I-ops (Addi/Subi/Xori/Rotli/Rotri).
16 bits cannot hold three 4-bit operands + a 12-op opcode (1+4+12 = 17 > 16), and op-unification to
free bits needs direction bits it has no room for. **⇒ the word must widen.**

## DECISION: widen the instruction word to **24 bits** (reformat once, for A-0 + A-2)
Layout (unified format bit + 5-bit opcode gives room for the 17 current ops + growth; A-6 keeps the
op count bounded so the opcode won't creep):
- **R:** `fmt(1) | opcode(5) | rd(4) | rs(4) | rt(4)` = 18 bits (6 spare — reserved for B-1 control
  lines / future).
- **I:** `fmt(1) | opcode(5) | rd(4) | imm(14)` = 24 bits. The **14-bit immediate** gives A-2 the
  wider PC-offset/δu headroom (B up to ~16k) so A-2 needs no *second* reformat — the shared-word
  real estate is allocated once now.
- Operand index: `0..8 → r0..r7`, `8..16 → s0..s7` (the crossbar `base` maps `i<8 → reg_field(i)`,
  `i≥8 → scratch_field(i-8)`). s0/s1 stay decoder-reserved (not emitted as operands) — same as today.

**Why 24 and not stay-16 / go-32:**
- *Stay 16 (op-unify):* rejected — no room for 4-bit operands + a dir bit; pinches A-2's imm; forces
  coupling A-0 to B-1's core-sharing prematurely.
- *Go 32:* more headroom but ~2× the IR/ROM standing plumbing; 24 already covers A-0 + A-2 with spare.
- *24:* 3 bytes; fits 4-bit operands (A-0) + 14-bit imm (A-2) + 5-bit opcode + 6 spare; reformats ONCE.

**Standing cost (honest):** IR 16→24 bits (IR_lo/IR_hi → three bytes) and ROM words 16→24 → ~+50% on
the *fetch-XOR + decode-field-read* portion of step_j (not the whole step). Accepted as a sized-once
foundation, consistent with "size once generously"; it is the enabling cost for operand-addressable
working store (which unblocks the GCD and every compute-heavy loop) and is shared with A-2. Re-pin
energy/ring goldens (the IR width changes the state layout — coordinate with A-2's state widen).

## Change surface (implementation)
1. **asm.rs** `r_word`/`i_word` → 24-bit builders with 4-bit operand fields + 5-bit opcode + 14-bit
   imm; operand encoders accept a `reg|scratch` index (0..16); assemble to `u32`/3 bytes.
2. **decoder.rs** field extraction (the R/I decode at ~262-283) → new bit positions; **`select_reg`
   generalized to `select_operand` over `reg_field ∪ scratch_field`** (the crossbar now routes 16
   locations — a 4-level tree, 15 word-swaps/operand vs 7; the coverage-critical change). Confirm the
   opcode decode + `pad`-zero guard at the new positions.
3. **types.rs / IR** widen IR 16→24 (a third IR byte field) + the ROM word width; encode/decode.
4. **step.rs** `fetch` XORs the 24-bit ROM word into IR; unfetch symmetric. PC/BR unchanged here
   (A-2 widens those).
5. Instructions that pack non-register operands (Xchg `sx`, Mexch `addr`, Mexchreg `ra`) re-fit into
   the new fields.

## Gates (highest bar — decoder + encoding)
- Fresh-context adversarial review (a different model): the reformat + the widened crossbar; attack
  the operand-index mapping (reg vs scratch off-by-one), the pad/opcode decode at new positions, the
  IR-width state-layout change, injectivity of the widened step.
- Differential oracle: **every existing program `run_on_cpu == eval` at the new format** (the reformat
  must be behavior-preserving for register-only programs) + a NEW test that an op names a **scratch
  operand** (e.g. `Ccnot rd, rs, s2`) and executes correctly.
- Energy/ring golden re-pin (IR/ROM width changed).
- Real-worktree behavioral test (not unit-only).

## Note
A-2 (PC/BR/δu width widen) shares this word (the 14-bit imm) and the state-layout re-pin — do A-2's
state widen and this IR widen coherently (they touch the same encode/decode + goldens). Per the
re-sequence, A-2 runs first (state widen + injectivity re-proof), then A-0 (this reformat + crossbar).
