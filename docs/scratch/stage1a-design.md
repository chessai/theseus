# M-C2 Stage 1a — advisor design note v2 (post-review, pre-delegation)

Grounds ADR-0046 §4.5.0 Stage 1a in the post-Layer-1 code (tip `e7fbf20`,
`chessai/theseus-v2`). **v2 incorporates the fixes F1–F6 + pinned build order from
the first adversarial review** (which returned DO-NOT-SHIP on v1). This is the
design the delegation will encode.

## Goal (verbatim scope, ADR-0046 §4.5.0)

Value-level externalization, **delegatable now**:

- Move `MemState` out of `State` into a separate iso factor.
- Make the addressing layer carrier-width-parameterized (FIX-3).
- Re-target the memory exchange so it swaps `reg[rd] ↔ MemState[addr]` across the
  factor boundary, composed into a `system_step` at the **value level** — **no
  `BusMsg`, no seam-rotation, no ring.**
- Prove `system_step : (CpuState, MemState) ↔ (CpuState, MemState) == eval` on
  spill-to-memory programs, under the existing full-step `run_on_cpu` loop
  (whole-domain fast-sim + real-CPU sample); **re-verify C1 on the real
  re-targeted tree lowering**.
- Honestly **degenerate** (Prop 5.1) — a refactor-and-plumbing de-risk. The §6
  line-audit / non-degeneracy is Stage 1b.

## Decision 1 — TWO step isos, ring untouched (+ a shared-ISA guard)

Today one `step(rom) : State ↔ State` (State = `20 + MEM_WORDS` fields) is used by
**both** `run_on_cpu` (the oracle; evaluates via `eval`) **and**
`ring_run::step_ring` (lowers to a netlist and recirculates).

**Split into two isos so Stage 1a touches the oracle carrier without touching the
ring (literally "no ring"):**

1. `step(rom) : CpuState ↔ CpuState`, **CpuState = 20 fields**
   `[r0..r7, PC, BR, IR_lo, IR_hi, s0..s7]` (mem removed). `Op::Mexch` **removed**
   from `decode_execute` → `decode_execute′`. The ring path uses this unchanged;
   its step body narrows 24→20 fields. *(Honest note: only the ring's `step`
   narrows; the oracle's `system_step` below is **wider** than today's `step` —
   this is not a net area win, it is the carrier split. Do not expect the ring
   regression to reverse.)*
2. `system_step(rom) : (CpuState, MemState) ↔ (CpuState, MemState)`, **new**.
   Non-`MEXCH` ops are `(op ⊗ id_MemState)`; `MEXCH` is the cross-factor exchange
   (Decision 3). `run_on_cpu`/`run_on_cpu_fast` use this.

**Divergence guard (F3).** Two isos means `decode_execute′` (ring) and the
composite decode (oracle) could silently desync the ISA. Contract: **they must
agree bit-for-bit on all 14 non-`MEXCH` ops**; only `MEXCH` differs. Ship a test
asserting `decode_execute′(v) == proj_CpuState(system_step)(v, mem=0)` on the
non-`MEXCH` opcode domain.

## Decision 2 — carrier split at the ISO level only; `StateVals` stays flat (F4)

- **Iso level:** `State`/`state_ty()` splits. `CpuState` = 20-field right-nested
  product; `MemState` = `MEM_WORDS`-field right-nested product; `system_step`
  carrier = `(CpuState, MemState)`.
- **Byte level:** `StateVals` **stays a single flat struct**
  (`regs, pc, br, ir, scratch, mem`). `run_on_cpu_fast`'s `exec_instr`
  (`std::mem::swap` on `state.regs`/`state.mem`) is **unchanged** — the byte
  interpreter does not evaluate the iso, so the product structure is invisible to
  it. The bridge is at marshalling: add `encode_composite(&StateVals) -> (Value,
  Value)` (the CpuState value and the MemState value) and the matching
  `decode_composite`; `marshal`/`unmarshal` route through these for the oracle
  path. Flat `encode`/`decode` (single `Value`) stay for the ring's `step`.
- **`MemState` persistence (F5).** `MemState` is init-0 at program entry
  (marshalling) and **projected away only at the final comparison** (surface
  programs are not memory-typed, Prop 5.1) — **never per-step**. A store→reload
  program writes `mem` in step k and reads it in step k+j; projecting per-iteration
  would erase the spill (FIX-1's landmine).

## Decision 3 — the exchange: parameterized flat routing + ONE cross-factor gate (F1)

**Reject v1's generic `at_composite`.** FIX-3 is done instead as: **parameterize
the flat routing helpers on carrier width**, then instantiate them per factor.

- `route_to_head`/`split_fields`/`at_fields`/`on_fields` (`addressing.rs`) take the
  field count as a **parameter** (`n_fields`) instead of reading the global
  `NUM_FIELDS`. Instantiate at **`n=20`** for CpuState routing and **`n=MEM_WORDS`**
  for MemState routing. (Existing flat callers pass `20`; nothing else changes for
  them.)
- `select_reg` stays flat over CpuState (routes `reg[rd] → CpuState head`).
- `select_mem` is re-expressed over the **MemState** carrier: the same C1 Fredkin
  tree, now routing `MemState[addr] → MemState head` using the `n=MEM_WORDS`
  routing. **This tree is the piece reused verbatim at Stage 1b** (there its
  controls are relabeled IR→`BusMsg.addr`; the tree *structure over MemState* is
  identical, so 1a's C1 re-verification transfers).
- **The one cross-factor primitive** `cross_ctrl_fredkin(cpu_ctrl_field,
  ctrl_bit, mem_word_a, mem_word_b)`: a Fredkin whose **control is one bit
  (`ctrl_bit`) of a CpuState field** and whose **two `W`-bit target words are in
  MemState**. **Routing unit is the field, not the bit** (`at_fields` routes whole
  8-wire fields) — so gather the *whole* `cpu_ctrl_field` (8 wires) across the
  boundary alongside the two mem words, `flatten` the 3-field group to `flat(24)`,
  and let `gate_at` pick the `ctrl_bit` wire within it (exactly as the flat
  `on_fields`/`ctrl_word_swap` path does today). Construction (explicit, not
  hand-waved): route `cpu_ctrl_field → CpuState head` and route the two mem words
  to the `MemState` head-pair, then a fixed `Times`/`AssocLProd`/`AssocRProd`/
  `SwapProd` bracket brings those 3 fields (ctrl field + 2 words = 24 wires)
  adjacent at the very front of the `(CpuState, MemState)` product, apply the gate,
  and the bracket's `Sym` un-routes. Do **not** attempt to route a single bare bit
  across the factor boundary. `eval` threads this in O(structure) — no blow-up
  (confirmed: `interp.rs` is pure structural recursion). This cross-factor coupling
  is the **1a-only degenerate part** (replaced by emit/absorb at 1b); it is *not*
  claimed as reusable substrate — the reused substrate is the MemState-internal
  tree above.
- `MEXCH` body = `select_reg(rd); select_mem(addr); [cross-factor gated swap of
  CpuState head ↔ MemState head, predicated on the MEXCH opcode]; sym(select_mem);
  sym(select_reg)`, self-inverse. Both `select_mem`'s internal controlled swaps and
  the final gated swap use `cross_ctrl_fredkin` (the control bit is an IR/predicate
  field in CpuState; the targets are MemState words) — cross-factor control is
  unavoidable and identical in kind to 1b's BusMsg-controlled tree.

## Decision 4 — the second MEXCH path (F4/D5.3)

`instr::mexch` (`instr.rs`, `at_fields(swap_words_iso(), &[reg_field(rd),
mem_field(addr)])`) is a **second** MEXCH iso builder (used by
`tests/mem_exchange.rs::mexch_instr_iso_is_garbage_free`). Under the carrier split
`mem_field(addr)` no longer indexes the same product as `reg_field(rd)`. Re-express
it over the composite carrier (same `cross_ctrl_fredkin`/swap machinery) or retire
it in favor of the decoder path — **do not leave it referencing the flat product**;
keep its garbage-free test green (or move the assertion onto the real path).

## Oracle & acceptance gates

- `run_on_cpu` evaluates `system_step` via `eval`, T times, over the product value;
  `== eval(surface iso)` whole-domain (fast-sim) + real-CPU **sample** on: a
  >14-leaf pure spill-to-memory program, the max-ceiling program, and a
  **store→reload round-trip** (the FIX-5 witness).
- **C1 real-tree re-verification (F2, non-negotiable).** Whole-domain
  ancilla-clean checks on the **real re-targeted `select_mem` lowering** (the one
  the composite addressing builds — NOT `build_mexch_prim`) at `N ∈ {2,4,8}`,
  **explicitly including the non-head-`addr` × control-bit-set states** (longest
  swap path, where a route/un-route residue would hide).
- **Ceiling (F6).** `MAX_LEAVES = 14 + MEM_WORDS`, `MAX_LEAVES_BRANCHING = 12 +
  MEM_WORDS` unchanged (Stage 1a does not touch `MEM_WORDS`/`allocate_aux`).
  Keep the existing spill-ceiling tests green (`spill_15_*` / `mem_spill_15_*` /
  the `TooManyLeaves` negatives in `tests/compile.rs`) — re-confirm, don't
  re-derive.
- No M-A/M-B/M-C1 regression; heavy ring tests stay `#[ignore]`d.

## Forced build ORDER (F, dependency-driven — steps 1→2→3 strictly serial)

1. **Carrier split in `types.rs`** — introduce `CpuState`/`MemState` iso types +
   `encode_composite`/`decode_composite` + split `state_ty`; `StateVals` stays
   flat. Land + test `encode/decode` and `marshal/unmarshal` round-trips **before
   anything downstream**.
2. **Parameterize the addressing helpers on width** + build `cross_ctrl_fredkin`;
   **prove it self-inverse in isolation on a real 2-factor round-trip (the L15
   bar)** *before* any decoder rewrite. This is the gate — a failure here must be
   distinguishable from a C1 failure.
3. **Re-express `select_mem`/`ctrl_word_swap`/the gated swap + `MEXCH`/`instr::mexch`
   over the composite carrier**; run F2's real-tree C1 re-verification.
4. **Assemble `system_step`** (`(op ⊗ id_MemState)` + cross-factor MEXCH); wire
   `run_on_cpu`/`run_on_cpu_fast`.
5. **Oracle + shared-ISA guard (F3) + ceiling (F6)** all green.

## Delegation decomposition (A then B) — added after a turn-budget failure

The first whole-Stage-1a delegation (`01KXKPPN…`, tier 0) hit
`turn_budget_exceeded` at the default 25-turn cap having only *read* the code
(zero diff). Lesson: **set `budget.turns` explicitly** (the driven-CLI default is
25). And to fit the profile's ~60-min lifetime ceiling with margin — and to bank
the reviewer's top serial gate first — Stage 1a is delegated as two gate-green
increments:

- **1a-A (routing primitive; delegate first).** *Additive* width-parameterized
  routing (`route_to_head`/`at_fields`/`on_fields` usable at any `n_fields`, via
  new `_n` variants or a widened signature; existing callers keep behavior at
  `n = NUM_FIELDS`) **plus** the generic `cross_ctrl_fredkin(ctrl_field, ctrl_bit,
  word_a, word_b)` primitive **plus** a whole-domain self-inverse test over a
  **synthetic 2-factor product** (small left/right field groups) — the L15 gate,
  proven *before* any carrier change depends on it. No `State` change, no
  `MemState`, no oracle: existing tests stay green; the one new test proves the
  primitive. Independently mergeable.
- **1a-B (carrier split + oracle; delegate after A merges).** Everything else in
  this note — externalize `MemState`, `system_step`, re-target `MEXCH`/`instr::mexch`
  via the (now-proven) `cross_ctrl_fredkin`, split marshalling, re-point the oracle,
  the shared-ISA guard, and **C1 re-verification on the real re-targeted
  `select_mem` tree**. Branches from the post-A base.

Build steps 1–5 above still describe the *combined* work; A covers step 2's
primitive, B covers steps 1,3,4,5.

## Explicitly OUT of scope (Stage 1b)

`BusMsg`; emit/mem/absorb; seam rotation; the `StepRing` composite certificate;
the §6 MemState-line audit / non-degeneracy witness; bus-quiescence.
