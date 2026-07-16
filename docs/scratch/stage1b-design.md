# M-C2 Stage 1b — advisor design note v2 (post-review)

Grounds ADR-0046 §4.5.8 in the **merged Stage-1a-B code** (theseus-v2 `8f27ba0`).
**v2 incorporates the fixes D1–D5 + reordered build order from the first adversarial
review** (which returned SHIP-WITH-FIXES: the architecture is sound — the 3-cycle is
correct, P4 survives, the wrap cut is right, no theseus-core change — but the v1
"reuse verbatim / one-line / unchanged-green" framing undercounted the 3-factor
plumbing). Stage 1b touches `step`/`system_step` and the ring → full CLAUDE.md bar.

## Starting point (merged 1a-B)

- `system_step(rom)` (`step.rs:241`) over `(CpuState, MemState)`; MEXCH = the
  **degenerate** `mexch_composite()` (`decoder.rs:659`): the C1 tree
  `select_mem_composite(R_RT)` is **keyed on IR bits** → its cells touch CpuState →
  §6 line-audit would reject it. Making it non-degenerate is Stage 1b's job.
- The ring (`ring_run.rs`) lowers `step` (CpuState-only), **not** `system_step`.
  `StepRing` (`ring_run.rs:56`) hard-codes `state_ty()` for its encode/decode I/O
  (`ring_run.rs:53,109,124`) and computes `n_data = inputs − ancilla_init.len()`
  (`:75`) — no sub-partition-within-data, no bus-quiescence. **No BusMsg/emit/absorb
  scaffolding exists.**
- MEXCH field reads: `rd` from `R_RD` (IR_HI bits 2..0, `decoder.rs:177`); `addr`
  from `R_RT` (IR_LO bits 2..4, `:181`). `exec_instr` confirms `swap(regs[rd],
  mem[addr])` (`compile.rs:1582`). **`rd` and `addr` come from *different* IR
  fields** — do not conflate them.

## Goal (ADR-0046 §4.5.8): non-degenerate Option-B + composite ring closure

Decouple the mem access via `BusMsg` (emit/mem/absorb) so the mem tree is keyed on
`BusMsg.addr` and touches **zero CpuState fields** (line-audit passes); build
`system_step` as one iso term in the in-revolution order; close it through a
`SystemRing` with the certificate + bus-quiescence, at `N∈{2,4,8}`.

## Decision 1 — BusMsg, the 3-factor carrier, and NEW encode/decode (D4a)

- **`BusMsg = 3 × W8 fields` `[enable, addr, word]`** (`bus_state_ty() = fields(3)`).
  `enable` uses bit 0; `word` is full W8; **`addr` carries exactly `⌈log₂N⌉` low
  bits — emit MUST copy exactly `⌈log₂N⌉` bits of `IR.rt`, so `addr`'s high bits are
  provably 0** (keeps the audit/C1 domain bounded — D3).
- **Carrier: `system_state_ty() = ((CpuState, MemState), BusMsg)`** (reuse the
  merged `(CpuState,MemState)` sub-product verbatim; BusMsg outermost).
- **NEW `encode_system`/`decode_system` (D4a — NOT "extend encode_composite").**
  `encode_composite`/`decode_composite` (`types.rs:214,241`) are 2-tuple
  `Value::Pair(cpu,mem)`; a 3-factor `system_step` makes them panic. Add
  `encode_system(&StateVals)->Value` (BusMsg factor init 0) and `decode_system`
  (project BusMsg away). **StateVals stays flat** (BusMsg is a transient factor, not
  byte-carrier state). `run_on_cpu` gains a `system_step` path over
  `system_state_ty()` that inits BusMsg=0 and projects it at comparison — the
  "composite oracle" test is **re-pointed at the 3-factor encode/decode**, not
  "unchanged-green".

## Decision 2 — 3-factor reshape helpers (D2, BLOCKING — the crux)

`cross_factor_gated_swap` (`decoder.rs:618`) and `select_mem_composite_n`
(`:276`) call `split_fields(n_left,n_right)` (`addressing.rs:86`), which splits a
**flat 2-factor** product. On `((Cpu,Mem),Bus)` they are **NOT reusable verbatim**:
`emit` touches Cpu+Bus (Mem wedged between), `mem` touches Mem+Bus (Cpu in the way).
No nesting makes all three phases bracket-free.

**Deliverable (new build step, precedes emit/mem/absorb):** a small reshape-helper
set that exposes the two needed 2-factor *views* of the 3-factor carrier via
`AssocLProd`/`AssocRProd`/`SwapProd`:
- `view_mem_bus`: `((Cpu,Mem),Bus) ↔ (Cpu, (Mem,Bus))` — lift `mem` on the `(Mem,Bus)`
  sub-product with `Cpu` identity.
- `view_cpu_bus`: `((Cpu,Mem),Bus) ↔ ((Cpu,Bus),Mem)` — lift `emit`/`absorb` on the
  `(Cpu,Bus)` sub-product with `Mem` identity.
Each phase = `view ; (sub_iso ⊗ id_third) ; sym(view)`. The sub-isos then reuse the
existing 2-factor `cross_factor_gated_swap`/tree machinery on their `(n_left,n_right)`
sub-product **unchanged**. This is the FIX-3 pattern at 3 factors — bounded, but it
is real new plumbing, not an index change.

## Decision 3 — emit / mem / absorb (D1 preconditions pinned)

- **`emit` is a SELF-CONTAINED bracket, swap PREDICATE-GATED** (D1):
  `emit = pred ; selr ; [enable^=p ; addr^=IR.rt(⌈log₂N⌉ bits) ; gated_swap(reg-head ↔
  BusMsg.word)] ; sym(selr) ; sym(pred)`, where `pred = pred_compute(r_pred(0b1001))`
  (`decoder.rs:343`), `selr = select_reg(R_RD)` (`:257`), and the swap is gated on the
  predicate bit (an unpredicated swap corrupts `reg[rd]` on non-MEXCH). `selr` is
  **internal to emit** so `sym(emit)` re-selects the same `rd` (IR is unchanged).
  Built over `view_cpu_bus`.
- **`absorb ≡ Iso::sym(emit)` verbatim** (D1) — one term, **no routing hoisted across
  `mem`**. The 3-cycle correctness (verified on paper by the reviewer) depends on
  exactly these two preconditions: emit's swap gated, selr internal.
- **`mem`** built over `view_mem_bus`: `select_mem_on_bus` (Decision 4) routes
  `MemState[addr]→head` keyed on `BusMsg.addr`, then `gated_swap(BusMsg.word ↔
  MemState[0])` gated on `BusMsg.enable`, then un-route. **Controls = BusMsg, targets
  = Mem+Bus → zero CpuState fields.** This is the line-audit witness.
- The `emit→mem→absorb` 3-cycle returns BusMsg to 0 and swaps `reg[rd]↔MemState[addr]`
  (reviewer-confirmed; zeros conserved).

## Decision 4 — select_mem_on_bus + the ported C1 harness (D3)

`select_mem_on_bus`: the proven C1 tree with control bits sourced from the
`BusMsg.addr` **field** instead of IR (the Fredkin control is pass-through regardless
of source, so **gate-level cleanliness transfers to N=8** — D3). BUT the discharge
**test** `c1_re_verify_at_n` / `select_mem_samples` / `select_mem_reference`
(`tests/composite.rs:234,283,314`) is hard-wired to the IR_lo layout
(`ir_lo_offset=10*W`) and **must be ported** to source controls from the `BusMsg.addr`
field indices, re-run bounded at `N∈{2,4,8}`. Assert emit copies exactly `⌈log₂N⌉`
bits (so `addr` high bits are provably 0 and the sample domain stays bounded).

## Decision 5 — `SystemRing` (D5 — a rewrite of StepRing's I/O, not a method)

`StepRing` hard-codes `state_ty()` I/O and knows only the ancilla/data cut, not a
Bus-within-data sub-partition. **New `SystemRing`** carrying `system_state_ty()`
encode/decode + an explicit `bus_line_range` (the DATA-line indices of the BusMsg
factor). Partition: `(CpuState,MemState,BusMsg)` are carrier-type **DATA**; only
lowering scratch is ancilla (§4.5.8). MemState **persists**; BusMsg is DATA but
**checked-zero** at every revolution boundary (a `SystemRing` method — `run()` feeds
data forward, so a straddling Bus residue would corrupt the next revolution's emit,
Prop 3.4). Certificate (generalize `tests/ring_run.rs`):
`restored_ok ∧ run == run_open ∧ run == eval(system_step)^R ∧ bus-quiescent at every
boundary`, at `N∈{2,4,8}` (start N=2), bounded/sampled surface domain (Prop 5.1).
Plus the **§6 line-audit** (mechanical netlist-incidence: no MemState port reaches a
CpuState-phase active cell; no CpuState port reaches the `mem` tree).

## Delegation split (A then B) + bounded tests

Every whole-domain test **bounded** (≤ ~2^17; sample large — the 3h gate-hang
lesson). Both increments carry the full bar.

- **1b-A — BusMsg + reshape + emit/mem/absorb + line-audit (value-level, NO ring).**
  Deliver Decisions 1–4 + the value-level oracle & audit. Prove: (i)
  `run_on_cpu(system_step) == eval` over the 3-factor carrier (spill-15,
  store→reload); (ii) **BusMsg == 0 at every step boundary on EVERY oracle sample,
  MEXCH and non-MEXCH alike** (D4b — decode the Bus factor, assert 0 in/out); (iii)
  the **line-audit** (mem cells touch zero CpuState fields — netlist-incidence test);
  (iv) `emit;mem;absorb` correct (`reg[rd]↔MemState[addr]`) and self-inverse; (v) C1
  re-verified on `select_mem_on_bus` at `N∈{2,4,8}` (ported harness). Independently
  mergeable; non-degenerate at value level, not yet closed as a ring.
- **1b-B — the `SystemRing` closure (Decision 5).** Gated on 1b-A green.

## Forced build ORDER (reviewer-reordered; serial)

1. `types.rs`: `bus_state_ty` (3×W8), `system_state_ty = ((Cpu,Mem),Bus)`,
   `encode_system`/`decode_system` (BusMsg init 0); round-trip test FIRST.
2. **reshape helpers** (`view_mem_bus`, `view_cpu_bus`) — D2, MUST precede step 4.
3. `select_mem_on_bus` (re-key tree on BusMsg.addr) **+ ported C1 harness** (D3),
   bounded, N∈{2,4,8}.
4. `emit` (self-contained, gated), `absorb=sym(emit)`, `mem` (gated on enable) — over
   the reshape views.
5. `system_step` 8-phase over `system_state_ty`; drop `mexch_composite`; re-point
   `run_on_cpu` + the `shared_isa_guard` test to 3-factor encode/decode (D4a).
6. 1b-A tests: oracle (spill-15, store→reload) + per-sample bus-quiescence + line-audit
   + emit;mem;absorb correctness/self-inverse. All bounded.
7. **1b-B** (gated): `SystemRing` (system_state_ty I/O + bus_line_range + boundary
   bus-quiescence) + certificate `restored_ok ∧ run==run_open ∧ run==eval^R ∧
   bus-quiescent` at N∈{2,4,8} (start N=2).

## Explicitly OUT of scope for 1b-A (→ 1b-B)

The ring, `run`/`run_open`, the closure certificate, revolution-boundary quiescence
(1b-A checks it per-step value-level — a sound proxy since the transaction is interior
to one step, reviewer-confirmed).

## Confirmed by review (do not re-litigate)

3-cycle correct (with D1 preconditions); P4 (IR=0 between steps) survives the 8-phase
order (unfetch re-zeros IR after absorb; emit/absorb only read IR); wrap cut falls
between absorb and next fetch with Bus=0; C1 gate-level cleanliness transfers to N=8
(control-source-independent); no theseus-core change; 1b-A/1b-B split is clean.
