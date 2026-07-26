# A-7 — wide data words `Flat(n>8)` (the data-width coverage axis)

**Status:** design draft (read-only investigation, worktree `/tmp/a7`, branch
`a7-wide-words`, off merged `chessai/theseus-v2` `a916e27`). No `.rs` edits.

**Goal.** Lift the `Flat(n) : n ≤ 8` cap so programs whose *data* exceeds a byte
(16/24/32-bit words) are expressible. This is a DIFFERENT axis from the completed
control work (M-D.e / variable-T loops) — A-7 is about the *width of a value*, not
the shape of a loop.

---

## 0. The two "Flat"s (disambiguation — read this first)

There are **two unrelated notions of `flat`** in the tree; conflating them is the
first trap:

1. **`theseus-core::Type::Flat(u8)`** (`crates/theseus-core/src/ty.rs:21`) — a
   *program-value* type: a width-parameterized SINGLE leaf, cardinality `2^n`,
   `num_leaves = 1`. Its value is the thermometer `Right^k(Left(Unit))`
   (`enumerate.rs:63`, `prim.rs:301`). This is what A-7 lifts. Currently capped
   `1..=8` (ctor `ty.rs:38`; `enumerate.rs:15`; `prim.rs:64`).
2. **`theseus-tisc::types::flat(n)`** (`crates/theseus-tisc/src/types.rs:131`) — a
   *datapath* helper: a right-nested product of `n` `Bit`s, `B×(B×…×B)`. This is
   how `word8()`/`word16()`/`word32()` (the IR) are built. It is a MULTI-leaf
   product (16 for word16), NOT a single Flat leaf. It is unrelated to `Type::Flat`
   and is NOT capped at 8 — `word32()` already exists (`types.rs:152`).

The physical machine (registers, ISA) is **byte-wide by construction**:
`StateVals.regs : [u8; NUM_REGS]` (`types.rs:260`), `operand_get/set : u8`
(`types.rs:338/350`), `W = 8` (`types.rs:27`). Multi-byte *IR* (the 32-bit
instruction word) already exists as **four separate byte fields** IR_b0..IR_b3
(`types.rs:226-232`) — the existing precedent for "a wide value is several byte
words".

---

## 1. Representation — how `Flat(n)` is marshaled today, and the choice for n>8

### Today (n ≤ 8): ONE leaf → ONE byte, everywhere.
The whole marshal/synth stack is **one `u8` per leaf**:

- `value_to_leaf_bytes` (`prim.rs:300`) turns a `Flat(n)` thermometer value into a
  SINGLE byte `vec![k]`; `value_from_leaf_bytes` (`prim.rs:359`) reads one byte and
  asserts `byte < 2^n`.
- `SpecTransfer` (`prim.rs:87`) carries `leaves: Vec<Vec<BvExpr>>` and `width: u8`
  (documented `1..=8`, `prim.rs:91`); `eval_bytes` (`prim.rs:382`) accumulates each
  leaf into a `u8` (`*ob |= 1 << b`).
- The synthesizer/transcriber (`bveq.rs`) hard-codes `BITS = 8` (`bveq.rs:30`) with
  a static assert `BITS == 8` (`bveq.rs:268`); `RegTransfer.regs : Vec<Vec<BvExpr>>`
  is `NUM_REGS × BITS` (`bveq.rs:43-56`); `eval_transfer → [u8; NUM_REGS]`
  (`bveq.rs:206`); immediates are `u8` (`bveq.rs:327`); `VarMap` is `BITS`-wide per
  operand (`bveq.rs:80-121`).
- `table_to_regtransfer` (`synth.rs:1304`) enumerates `2^Σwidths` rows into
  `u8` carrier bytes and asserts each leaf `width ≤ BITS = 8` (`synth.rs:1322`).
- `StateVals.operand_get/set` and the whole fast-sim are `u8`.
- Note `theseus-netlist::bits_of`/`value_from_bits` (`lower.rs:159/197`) do NOT even
  handle `Type::Flat` — they only know `One`/`Sum`/`Prod`. The `Flat` bit-marshal is
  a SEPARATE, core-native thermometer path (`prim.rs`), and the datapath uses the
  `Prod`-of-`Bit` `word8()` type, whose leaves ARE `Bit`s that `bits_of` handles.

**Conclusion:** a leaf is atomically a byte from the value model down to the
physical register. There is no place today where a single leaf spans two bytes.

### Options for n > 8

- **(a) Multi-BYTE-leaf.** One logical `Flat(n)` lowers to `⌈n/8⌉` physical byte
  words (registers). E.g. `Flat(16)` = a 2-word value in two 8-bit registers.
- **(b) Wide physical word.** Widen `W` beyond 8 bits (datapath-wide change:
  registers, ISA operand width, decoder, energy chardb, ring).

### DECISION: (a) multi-byte-leaf, and moreover MODEL wide values as `Prod`.

Rationale (efficiency principle — *compile-time discipline > machine growth*):

- **(b) taxes every program forever.** Widening `W` grows the register netlist, the
  decoder, and `step_j` for EVERY program regardless of whether it uses wide data —
  the exact anti-pattern the ISA-growth energy tax already demonstrated (each op
  added to `Op::ALL` raised fib's step_j). The machine stays byte-wide.
- **(a) is pay-for-what-you-use.** A `Flat(16)` program spends TWO registers and
  a ripple-carry across the byte boundary; a byte program is unchanged. This mirrors
  how the 32-bit IR is *already* stored as four independent byte fields.
- **The cleanest form of (a): don't add a new "2-byte leaf" concept at all — reuse
  `Prod`.** `Flat(16) ≜ Prod(Flat(8), Flat(8))` is already a legal, 2-leaf,
  2-byte type TODAY (`tneg_prod_flat4_flat4_is_2_leaves`, `valwidth.rs:291`, proves
  `Prod(Flat(4),Flat(4))` marshals to 2 bytes and compiles). The synthesizer already
  lowers multi-leaf specs (gcd2 = 4 leaves; masked-accum = 3 `Flat(8)` leaves,
  `status.md:94`). So the *representation is already supported*; A-7's real work is
  the **wide-op LOWERING** that crosses the byte boundary (carry), plus a decision
  about whether to also lift the single-leaf `Type::Flat` cap for ergonomics.

**Two sub-strategies inside (a):**

- **A-7-canonical (recommended POC):** keep `Type::Flat` capped at 8; express a wide
  word at the *type* level as `Prod(Flat(8),…,Flat(8))` (a "little-endian byte-word",
  LSB byte first, matching the IR convention `types.rs:264`). ZERO core changes; all
  work is wide-op lowering + verification. The unit of work is a **prim over a
  multi-Flat(8) product**.
- **A-7-ergonomic (optional later slice):** ALSO lift `Type::Flat(n≤8)` to
  `Type::Flat(n≤W_MAX)` so authors can write `Flat(16)` directly, and have the
  compiler *split* it into `⌈n/8⌉` byte leaves at the marshal boundary. This is pure
  sugar over A-7-canonical and should NOT be in the first slice — it touches
  `value_to_leaf_bytes` (one leaf → `⌈n/8⌉` bytes), `SpecTransfer.width` (must allow
  >8 or become a leaf-count), and every `≤ 8` assert. Defer it; ship the product form
  first (it is strictly less risky and unlocks the same programs).

---

## 2. ISA ops on wide words — what lowering is needed

The ISA ops are byte-wide: `Add/Sub` = TK ripple over `flat(16)` = `[PC,BR]`-style
two-byte wire block (`instr.rs:229 tk_add_gates`, `add_operand` `instr.rs:308`);
`Xor` = per-byte CNOT (`xor_operand` `instr.rs:142`); `Ccnot`/`Cswap` = per-byte
(`instr.rs`). Every one is a `Word8 ↔ Word8` operand iso; there is no carry BETWEEN
two register bytes.

Per wide op:

- **Wide `Xor` (and `Xori`, `Ccnot`, `Cswap`, `Rot` as bitwise/byte-parallel):**
  **trivial — per-byte, no carry.** `Flat(16) Xor` = `Xor{rd0,rs0}; Xor{rd1,rs1}`.
  The general synthesizer *already emits exactly this*: a `Prod(Flat(8),Flat(8))²`
  Xor prim is a 2+2-leaf spec; each output byte-leaf's row is `old ⊕ src` →
  `transcribe_into` recognizes it as one `Xor` per leaf (`synth.rs:277` affine-row
  recognizer, `valwidth.rs:t2` shows the single-leaf case). **No new lowering.**
- **Wide `Add` (and `Sub`):** **needs new lowering — inter-byte carry.**
  `Flat(16) Add` is NOT two independent byte-adds; the carry out of byte 0 must
  ripple into byte 1. The datapath has no cross-register carry today. Options:
  1. **Compose a wide ripple adder from the byte adder + a carry wire.** The
     TK adder (`instr.rs:tk_add_gates`) is a `flat(2W)` block-in-place adder; a
     `flat(4W)` version (adding a 16-bit operand into a 16-bit target laid out as
     two adjacent register bytes) is the same construction over `4W` wires. This is
     a NEW `add_operand_wide(n_bytes)` iso — a bit-level plumbing extension, still
     zero-ancilla TK, no new instruction.
  2. **OR: keep the ISA byte-wide and synthesize the wide add as a WSpec** (the A-6-W
     word-level IR, `a6-W-word-level-synthesis-design.md`): `WAdd` on byte 0, extract
     carry, `WAdd` carry into byte 1. But wide add is small and regular; option 1
     (hand-built parametric ripple, verified once by the BDD oracle) is simpler and
     the efficiency winner (`n_bytes` cells, no ancilla).
  - **Prims/combinators needing wide-aware lowering:** the `Add` reduce
    (`status.md:170`, `06ecf6b` — currently Flat(8)-only via `fold`) generalized to a
    wide accumulator; any prim whose spec is a *cross-byte-leaf arithmetic function*
    (real numeric ops on >8-bit numbers). Bitwise reduces (XOR/masked-AND) need
    NOTHING new — they are per-byte.
- **The general synthesizer's transcription over >8 bits:** see §3 — it does NOT
  transcribe a single >8-bit register; it transcribes a MULTI-BYTE-LEAF spec, one
  `BITS=8` register per byte. The wide-arithmetic ANF (a 16-bit adder as a function
  of 16 input bits) is `2^16` rows — under the `total_bits ≤ 20` cap (`synth.rs:1767`)
  but the ANF for a ripple carry is dense (carry is a deep AND-chain) → many gates.
  So for `Add` prefer the hand-built ripple (option 1), and reserve the general ANF
  path for irregular wide prims within the bit budget.

---

## 3. The synthesizer + wide words — does the existing general path already cover it?

**Partly YES, with a sharp boundary.**

- **What already works, unchanged:** a wide value modeled as `Prod(Flat(8),…)` is a
  multi-byte-*leaf* spec. `table_to_regtransfer` (`synth.rs:1304`) + `leaf_widths`
  (`compile.rs:1605`) already produce one carrier register per byte leaf, each with
  `width ≤ 8`, and the transcriber emits per-leaf gates. gcd2 (4 leaves) and the
  `Flat(8)³` masked-accum (`status.md:94`) prove multi-leaf lowering works. So a wide
  **bitwise** prim (Xor/Ccnot/masked-AND across a 2-byte word) is lowered by the
  EXISTING synthesizer with NO new code — it is just "more leaves". This confirms the
  prompt's hypothesis for the bitwise class.
- **The boundary — the ANF budget, not "8 bits".** `table_to_regtransfer` asserts
  each leaf `width ≤ BITS = 8` (`synth.rs:1322`) AND the *total* input bits
  `≤ 24` (`synth.rs:1332`), with `recognize_prim`'s wrapper capping `total_bits ≤ 20`
  → `NeedsProvisioning` (`compile.rs:1767`). A wide value is fine as long as each
  BYTE leaf is ≤ 8 bits and the whole carrier ≤ 20 input bits. `Flat(16)` = 16 bits
  (2 leaves × 8) — a 2-operand wide op (32 input bits) EXCEEDS the cap. So the
  general ANF path handles a wide UNARY prim (≤20 bits) but NOT a wide BINARY prim
  by brute ANF — which is exactly why wide binary arithmetic (Add) wants the
  structured ripple (§2) and wide bitwise ops want the per-leaf affine recognizer
  (which never enumerates the joint table — it recognizes each row structurally,
  `synth.rs:277`, sidestepping the `2^total_bits` blowup entirely).
- **`BITS` is the true wall for a SINGLE wide leaf.** IF A-7-ergonomic lifts
  `Type::Flat` above 8, the synthesizer canNOT represent a single >8-bit leaf
  (`RegTransfer` row is `BITS=8` long, `VarMap` is `BITS`-wide, static assert
  `BITS==8`). The ergonomic path MUST split a wide `Flat(n)` into `⌈n/8⌉` byte leaves
  BEFORE it reaches `bveq`/`synth` — i.e. it lowers to the product form anyway. This
  is another reason to ship the product form first and treat single-leaf-`Flat(>8)`
  as sugar.

**Net:** wide **bitwise** prims → existing synthesizer, no new lowering. Wide
**arithmetic** (cross-byte carry) → one new parametric ripple iso (Add/Sub),
verified once. The general ANF path is a fallback bounded by the 20-bit budget.

---

## 4. State / energy / WCET / injectivity impact

Because the machine stays byte-wide (choice (a)), **the CPU itself does not change**;
only *programs* consume more of the existing byte words:

- **State layout:** UNCHANGED. No new register width, no new fields. A wide value is
  `⌈n/8⌉` of the existing 8 general registers / scratch words / memory words. The
  program's operand-word budget (10 free words today) shrinks by `⌈n/8⌉−1` per live
  wide value — a *provisioning* consideration, not a state-type change. If a wide
  program needs >8 live byte-words it hits the same widen-state/spill machinery as
  any register-pressure case (M-D.e) — orthogonal to A-7.
- **Energy:** `step_j` is essentially instruction-independent (shared decode
  dominates, `energy.rs:16-20`). Wide ops add MORE INSTRUCTIONS (2× the byte ops,
  plus the ripple carry gates) → more STEPS → higher *program* energy `∝ T`, but the
  PER-STEP cost is unchanged and NO OTHER program is taxed. This is the efficiency
  win of (a) over (b). The energy model itself (`energy.rs`) needs NOTHING new — it
  prices whatever gates the wide op lowers to via the existing chardb.
- **WCET:** a wide op is a fixed, statically-known number of instructions
  (`n_bytes` for bitwise, `~n_bytes` cells for the ripple) → the compositional WCET
  cost tree (`compile.rs:2823`, TH-1 ledger `compile.rs:4273`) sums them exactly as
  it does byte ops today. No new WCET reasoning — wide ops are straight-line.
- **Injectivity / ADR-0034:** the reversibility argument is UNCHANGED for bitwise
  wide ops (per-byte CNOT/Toffoli/Fredkin are each isos; a product of isos is an
  iso). For the wide **ripple adder** it needs a fresh proof that the `⌈n/8⌉`-byte
  cross-carry construction is a total bijection — the SAME TK zero-ancilla argument
  the byte adder already carries (`instr.rs:229`, ADR-0034 D-proof for `pc_add_br`
  `step.rs:169`), just over `n_bytes·W` wires. This is the one genuinely new
  invariant and it is the highest-bar item (see §7).

**What re-proves:** only the wide-ripple-adder iso (bijectivity + net-zero ancilla +
adjoint = wide subtract). Everything bitwise inherits existing proofs by
compositionality.

---

## 5. Whole-domain oracle tractability (the 2^n blowup)

`Flat(16)` → `2^16` per operand, `2^32` for a binary op — the whole-domain
exec/enumerate is infeasible for wide binary ops (already true at `Flat(8)`
binary = `2^16` and Flat(8)³ = `2^24`, which is why the value-width ladder shipped
"verify-at-2/4, ship-8" via strided sampling + parametric ROM-shape pins,
`status.md:211`). A-7 extends the SAME playbook:

- **Symbolic (width-agnostic) is the primary oracle.** The BDD/`equiv` acceptance
  gate (`synth.rs:34`, `bveq::equiv`) proves emission == spec over the FULL symbolic
  domain without enumeration — it already recognizes `Flat(8)³ Ccnot` at the full
  `2^24` domain "with the table never built" in ~113 µs (`status.md:95,112`). For
  a wide **bitwise** prim, `equiv` over the per-leaf spec is complete and fast. This
  is the width-agnostic "verify-at-small-n ship-large" mechanism.
- **The wide ripple adder is verified structurally, not by domain enum.** Prove the
  `n_bytes`-byte adder ≡ the symbolic `RegTransfer` of "`a + b mod 2^{n·8}`" via
  `equiv` (BDD over `n·8` bits — tractable for n up to ~3 bytes = 24 bits; the BDD
  for ripple-add is LINEAR in width, not exponential). Cross-check against an
  independent Rust `u16/u32` reference on strided samples.
- **Verify-at-small-n, ship-large:** author the wide op parametrically in `n_bytes`,
  prove it EXHAUSTIVELY at `Flat(16)` where feasible for a UNARY op (`2^16` =
  tractable) and by BDD-`equiv` + strided reference for binary ops; PIN the emission
  SHAPE (gate count/pattern parametric in `n_bytes`) so the large instance is the
  same construction as the proven small one (exactly the `valwidth.rs:t3` parametric
  pin technique).
- **Real-authority tier:** `run_on_cpu == eval` on a handful of dense wide inputs
  (like `valwidth.rs:t4`), on the real `system_step` ring (L19: ancillas_restored +
  bus_quiescent). This is the merge gate for the highest-bar item.

---

## 6. Slice breakdown (smallest first)

Each slice: symbolic/exec verify → Fable design-review → delegate → Fable
impl-review → advisor-finish full-suite → merge (L19/L20/L21). Commit-only.

- **A-7.0 — wide bitwise `Xor` over `Prod(Flat(8),Flat(8))` (POC, smallest).**
  A `Flat16 ≜ Prod(Flat(8),Flat(8))` Xor prim `(a,b) ↦ (a, a^b)` (a 4-leaf prim).
  EXPECT it recognizes to `[Xor{rd0,rs0}, Xor{rd1,rs1}]` via the EXISTING synthesizer
  with zero new lowering — this slice PROVES the representation claim (§3) and
  establishes the wide-word test harness.
  - **POC / authority:** compile → assert 2-Xor emission (parametric shape pin);
    `run_on_cpu_fast == eval` whole-domain at the small `Prod(Flat(2),Flat(2))`
    modeling (tractable) + strided at `Flat(8)` bytes; ONE real-`system_step`
    `run_on_cpu == eval` on dense inputs. Reuses `valwidth.rs` structure.
- **A-7.1 — wide `Add` over `Prod(Flat(8),Flat(8))` (the real work: cross-byte carry).**
  New parametric `add_operand_wide(n_bytes)` ripple iso (TK over `n_bytes·W` wires,
  zero-ancilla) + its adjoint = wide `Sub`. This is the **highest-bar** slice
  (daemon-external but it is a new reversible arithmetic construction → adversarial
  review + real-authority integration test are mandatory).
  - **POC / authority:** `equiv` vs the symbolic 16-bit `a+b mod 2^16` RegTransfer
    (BDD, complete); cross-check vs Rust `u16` add on strided samples; a UNARY
    `Flat(16)` increment EXHAUSTIVE (`2^16`); `run_on_cpu == eval` on the real ring
    for dense inputs; adjoint round-trip (`add` then `sub` == id).
- **A-7.2 — a wide-word PROGRAM: a `Flat(16)` `Add` reduce (real numeric sum of
  16-bit words).** Generalize the Flat(8) `Add` reduce (`06ecf6b`) to a wide
  accumulator via `fold` — the useful payoff, proves the wide op composes under a
  combinator end-to-end on the CPU.
- **A-7.3 (optional, ergonomic) — lift `Type::Flat(n≤8)` to `n≤W_MAX` as SUGAR.**
  `value_to_leaf_bytes` splits a wide `Flat(n)` into `⌈n/8⌉` byte leaves; the
  compiler treats `Flat(16)` identically to `Prod(Flat(8),Flat(8))`. Pure ergonomics
  over A-7.0/.1; touches every `≤ 8` assert + `SpecTransfer.width`. Defer unless
  authoring ergonomics demand it.

---

## 7. Highest-bar gates (non-negotiable for A-7.1)

The wide ripple adder is a **new reversible-arithmetic construction** — this is the
class the repo holds to a higher bar (cf. CLAUDE.md orchestration-change discipline,
and the value-width Add slice which shipped only after Fable-built adversarial review
+ real `system_step` authority). Required:

1. **Adversarial fresh-context review** (prefer a different model from the
   implementer): enumerate the carry states across the byte boundary and attack the
   claim "the cross-byte carry ripples correctly and the ancilla nets to zero at
   EVERY intermediate width". Reviewer reports; does not rubber-stamp.
2. **BDD `equiv` vs the symbolic wide-add spec** (complete over the width, not a
   sample) — the acceptance oracle with teeth.
3. **Real `run_on_cpu == eval` on `system_step`** (ancillas_restored + bus_quiescent,
   L19) for dense wide inputs — NOT only the fast byte-sim.
4. **Adjoint round-trip** proving wide `Sub = sym(wide Add)` is the exact inverse.
5. Every finding from review ships with a regression test that REPRODUCES it
   (fails without the fix) — L20.

A-7.0 (bitwise, all-inherited-proof) is a normal-bar slice: symbolic `equiv` +
strided exec + one real-CPU witness.

---

## 8. Open questions

- **W_MAX / max wide width.** Cap wide words at what? `Prod` of `Flat(8)` is
  unbounded in principle, but the ANF general path caps at 20 input bits and BDD
  ripple-add is linear but the register file is 8 words. A pragmatic cap: wide words
  ≤ 4 bytes (32-bit, matches the IR width), living in ≤ 4 of the 8 registers. State
  this explicitly and reject wider with `NeedsProvisioning`.
- **Byte-order convention.** Fix little-endian byte-word (LSB byte first) to match
  the IR (`types.rs:264 push_ir_bits`) and `value_to_leaf_bytes` leaf order. Pin it
  in a test so a future refactor can't silently flip endianness.
- **Register-pressure interaction.** A wide program that needs >8 live byte-words is
  a spill problem (M-D.e), NOT A-7. Confirm the allocator's `NeedsProvisioning`
  reports cleanly (reject-don't-corrupt) rather than silently mis-lowering a wide
  value that doesn't fit — add a negative test.
- **Do we want A-7-ergonomic at all?** If every wide program is authored as
  `Prod(Flat(8),…)`, `Type::Flat(n>8)` may never be needed. Decide before A-7.3
  whether to spend the churn on lifting the `Type::Flat` cap.
- **Wide `Cswap`/`Rot` semantics.** Per-byte Cswap is fine; but a wide ROTATE that
  crosses the byte boundary (rotate a 16-bit word by k) is NOT per-byte — it is a
  bit-permutation across two registers. If wide rotate is needed, it is a separate
  (bit-plumbing) slice like the wide adder. Bitwise-only rotate (rotate each byte
  independently) is trivial; cross-byte rotate is not. Scope it out of the POC.

---

## Concrete file:line index

| concern | file:line |
| --- | --- |
| `Type::Flat(u8)` def | `crates/theseus-core/src/ty.rs:21` |
| `Flat` ctor cap `1..=8` | `crates/theseus-core/src/ty.rs:38` |
| cardinality `debug_assert ≤ 8` | `crates/theseus-core/src/enumerate.rs:15` |
| enumerate `Flat` thermometer, `≤ 8` | `crates/theseus-core/src/prim.rs:63` |
| `SpecTransfer.width: u8` (`1..=8`) | `crates/theseus-core/src/prim.rs:87,91` |
| `value_to_leaf_bytes` (1 leaf → 1 byte) | `crates/theseus-core/src/prim.rs:300` |
| `value_from_leaf_bytes` (`byte < 2^n`) | `crates/theseus-core/src/prim.rs:359` |
| `eval_bytes` (per-leaf `u8`) | `crates/theseus-core/src/prim.rs:382` |
| `BITS = 8` + static assert | `crates/theseus-tisc/src/bveq.rs:30,268` |
| `RegTransfer` (NUM_REGS×BITS) | `crates/theseus-tisc/src/bveq.rs:43` |
| `VarMap` (BITS-wide) | `crates/theseus-tisc/src/bveq.rs:73` |
| `eval_transfer → [u8; NUM_REGS]` | `crates/theseus-tisc/src/bveq.rs:206` |
| `table_to_regtransfer` (`≤ BITS`, `≤ 24`) | `crates/theseus-tisc/src/synth.rs:1304,1322,1332` |
| affine-row recognizer (per-byte Xor) | `crates/theseus-tisc/src/synth.rs:277` |
| `leaf_widths` (`Flat` `1..=8`) | `crates/theseus-tisc/src/compile.rs:1605` |
| `num_leaves` (`Flat` `1..=8`) | `crates/theseus-tisc/src/compile.rs:114` |
| general-synth `total_bits ≤ 20` cap | `crates/theseus-tisc/src/compile.rs:1767` |
| `recognize_prim` | `crates/theseus-tisc/src/compile.rs:1188` |
| `W = 8`, `StateVals.regs:[u8;8]` | `crates/theseus-tisc/src/types.rs:27,260` |
| `operand_get/set : u8` | `crates/theseus-tisc/src/types.rs:338,350` |
| `word16()`/`word32()` (product of bytes) | `crates/theseus-tisc/src/types.rs:146,152` |
| IR as 4 byte fields (wide-value precedent) | `crates/theseus-tisc/src/types.rs:226-232,264` |
| TK byte adder `tk_add_gates` | `crates/theseus-tisc/src/instr.rs:229` |
| `add_operand` / `xor_operand` (byte) | `crates/theseus-tisc/src/instr.rs:308,142` |
| `pc_add_br` (TK proof precedent) | `crates/theseus-tisc/src/step.rs:169` |
| energy is per-step, instr-independent | `crates/theseus-tisc/src/energy.rs:16` |
| WCET cost tree / TH-1 ledger | `crates/theseus-tisc/src/compile.rs:2823,4273` |
| `bits_of`/`value_from_bits` (no `Flat`) | `crates/theseus-netlist/src/lower.rs:159,197` |
| value-width test harness (reuse) | `crates/theseus-tisc/tests/valwidth.rs` |
| `Prod(Flat,Flat)` = 2 bytes (proven) | `crates/theseus-tisc/tests/valwidth.rs:291` |
