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

## DECISION (user, 2026-07-24): widen the instruction word to **32 bits** — max headroom, never
reformat again. Byte-aligned (4 bytes). **5-bit operand fields (32 locations)** so the register/
scratch file can grow to 32 later with NO second reformat; **6-bit opcode** (64) for op growth
headroom; wide immediate for A-2. Layout:
- **R:** `fmt(1) | opcode(6) | rd(5) | rs(5) | rt(5)` = 22 bits (10 spare — reserved for B-1 control
  lines / flags / future).
- **I:** `fmt(1) | opcode(6) | rd(5) | imm(20)` = 32 bits. The **20-bit immediate** gives A-2 vast
  PC-offset/δu headroom (B up to ~1M) — A-2 needs no second reformat.
- **Operand index (5-bit, 0..31):** `0..8 → r0..r7`, `8..16 → s0..s7`, `16..32 → reserved` (future
  registers/scratch). The crossbar `base` maps `i<8 → reg_field(i)`, `8≤i<16 → scratch_field(i−8)`.
  s0/s1 stay decoder-reserved (not emitted as operands) — same as today.

**Why 32 (user's call):** never reformat again; 32-way operands + 20-bit imm cover every future slice
(A-0 operands, A-2 imm/PC, more registers/scratch, B-1 control lines) in one reformat. Accepts the
higher standing cost for zero future encoding churn.

**Standing cost (honest):** IR 16→32 bits (two more IR bytes) and ROM words 16→32 → ~2× the
*fetch-XOR + decode-field-read* portion of step_j (not the whole step). Accepted for max headroom;
partially clawed back later by B-1 (shared decode) and B-2 (per-program widths). Re-pin energy/ring
goldens (IR width changes the state layout — coordinate with A-2's state widen).

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
