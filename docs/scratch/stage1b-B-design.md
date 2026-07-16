# M-C2 Stage 1b-B — advisor design note v1 (pre-review)

The final piece of Stage 1b: run the (now non-degenerate) `system_step` through a
**recirculating ring** and prove the **composite closure certificate**. Grounds
ADR-0046 §4.5.8 / §4.5 Decision 5 and the Stage-1b v2 note (Decision 5) in the
**merged 1b-A code** (theseus-v2 `1f59744`). Touches the ring → full CLAUDE.md bar.

## Starting point (merged 1b-A)

- `system_step(rom) : system_state_ty() ↔ system_state_ty()`, where
  `system_state_ty() = ((CpuState, MemState), BusMsg)` (`step.rs`). Built as one iso
  term in the in-revolution order `fetch;decode_execute′;emit;mem;absorb;unfetch;
  pc_add_br;pc_inc`. **Value-level only** — evaluated via `eval` in `run_on_cpu`;
  it does **not** yet run through any ring.
- `encode_system`/`decode_system` (`types.rs`) bridge `StateVals ↔` the 3-factor
  `Value`; BusMsg inits 0, projected at comparison. The §6 line-audit passes (mem
  cells touch zero CpuState fields).
- The existing ring is `StepRing` (`ring_run.rs:56`): `{compact: CompactBalanced,
  ancilla_init: Vec<bool>, n_data}`. `step_ring(rom)` (`:66`) lowers **`step`**
  (CpuState-only) via `lower_iso_with_ancillas`, sets `n_data = inputs −
  ancilla_init.len()`, `balance_compact`s it. `run` (`:101`) recirculates: feeds the
  ancilla-section outputs back as next inputs (zero-garbage closed loop);
  `run_open` (`:135`) re-supplies fresh `ancilla_init` each revolution;
  `ancillas_restored` (`:180`) checks the ancilla section returns to init.
  **StepRing hard-codes `state_ty()` for its encode/decode I/O** (`:53,109,124,140`).

## Goal (the closure certificate, ADR-0046 §4.4/§4.5.8)

`system_step : (CpuState, MemState, BusMsg) ↔ (…)` recirculated R times equals
`eval(system_step)^R`, closing under the same zero-garbage invariant (ZG-2,
Thm 3.5), **plus** the bus-quiescence boundary condition — at `N ∈ {2,4,8}`.

## Decision 1 — `SystemRing` (a rewrite of `StepRing`'s I/O, not a method — D5)

`StepRing`'s I/O is `state_ty()`-typed; it cannot carry `system_state_ty()`. Add a
sibling **`SystemRing`** (new type in `ring_run.rs`):
- `system_ring(rom) -> SystemRing`: lower `system_step(rom)` via
  `lower_iso_with_ancillas`; `balance_compact`; store the netlist + `ancilla_init`.
- **Partition (§4.5.8):** the carrier-type bits `(CpuState, MemState, BusMsg)` are
  **DATA**; only the lowering's own scratch is **ancilla**. So `n_data = inputs −
  ancilla_init.len()` (same formula), and MemState **persists** across revolutions
  (it is the spill store); BusMsg is DATA but **checked-zero** (below).
- `run` / `run_open` / `ancillas_restored`: same recirculation logic as `StepRing`,
  but encode/decode over `system_state_ty()` via `encode_system`/`decode_system`,
  and start-state = `StateVals` (BusMsg init 0).
- **`bus_line_range`:** the DATA-line indices of the `BusMsg` factor (the last
  `3·W` data bits, since BusMsg is the outermost/rightmost factor of
  `((Cpu,Mem),Bus)` — confirm the exact bit offset from the lowering's data-line
  order). Needed for the bus-quiescence check.

## Decision 2 — the bus-quiescence boundary check (D5/Prop 1.3)

`ancillas_restored` covers only the ancilla section; BusMsg lowered to **DATA**, so
it is NOT covered. Add `SystemRing::bus_quiescent(start) -> bool`: after one
revolution from `start`, assert the `bus_line_range` data lines are all 0. In `run`,
BusMsg data recirculates forward — a straddling non-zero residue would corrupt the
next revolution's `emit` (Prop 3.4), so the certificate MUST include this.

## Decision 3 — the certificate (generalize `tests/ring_run.rs`)

At `N ∈ {2,4,8}` (start N=2), over a **bounded sampled** surface domain (memory is a
compilation artifact, Prop 5.1 — enumerate surface inputs, project CpuState):
`restored_ok ∧ run == run_open ∧ run == eval(system_step)^R ∧ bus_quiescent`. Mirror
`tests/ring_run.rs` (`ring_matches_eval_oracle`, `ancillas_restored_after_one_revolution`,
`true_recirculation_matches_eval_oracle`, `open_vs_closed_equivalence`) but on
`SystemRing` + `system_step`, plus the bus-quiescence assertion.

## Decision 4 — the load-bearing RISK: does `system_step` lower ancilla-clean?

The zero-garbage `run` (feed ancilla outputs back) only equals `eval^R` if
`system_step` lowers **ancilla-clean** (`ancillas_restored: true`), exactly as `step`
does (ADR-0040 R1). `system_step` adds `emit`/`mem`/`absorb`: `emit`/`absorb` contain
**CNOTs** (`enable^=p`, `addr^=IR.rt`) and Fredkin swaps; the parity law (ADR-0033)
says CNOTs are ancilla-clean **only via R1** (`emit_mcnot`). If the emit/absorb
synthesis already routes CNOT→R1 (it should — it reuses the same gate builders as the
rest of the CPU), `system_step` lowers ancilla-clean and `run == run_open`. **If it
does NOT close (some K-only-dirty fan-out), STOP and report the smallest failing
state** — the fallback is the scoped `bennett.rs` wrap on the dirty sub-circuit
(ADR-0040), NOT a redesign. This is the first thing to check (at N=2).

## Decision 5 — COST / feasibility (start N=2; expect slow → `#[ignore]`)

`system_step` is wider than `step` (carrier `23+N` fields vs 20) and has more phases,
so `lower_iso_with_ancillas` + `balance_compact` + simulate will be **heavy** — like
the Layer-1 resident-memory ring tests that were `#[ignore]`d for area (ADR-0046
"area cost"). Expect the SystemRing certificate tests to be **slow**; mark the
scaling ones (`N=4,8`, high R) `#[ignore]`d (run pre-merge), keep a **fast N=2,
small-R** smoke certificate non-ignored. **Begin at N=2**: get `system_step` closing
the certificate end-to-end there before scaling (§4.5.7). If N=2 will not close,
STOP and report — do not brute-force.

## Delegation (likely ONE increment; keep the gate fast)

Unlike 1b-A this is contained (a `SystemRing` sibling + certificate tests), so one
increment. But **the gate must stay scoped** (the full theseus-tisc suite is ~25–30
min): `check_commands` = `cargo clippy … -D warnings` + `timeout 900 cargo test -p
theseus-tisc --test ring_system` (a NEW dedicated test file for the SystemRing
certificate) — NOT the whole suite. Any whole-domain/high-R enumeration **bounded**;
the heavy N=4,8 certificate `#[ignore]`d. Advisor runs the full suite + the
`#[ignore]`d certificate manually before merge.

## Forced build ORDER (serial)

1. `ring_run.rs`: `SystemRing` type + `system_ring(rom)` (lower `system_step`,
   balance_compact); `run`/`run_open`/`ancillas_restored` over `system_state_ty()` +
   `encode_system`/`decode_system`; `bus_line_range` + `bus_quiescent`.
2. **FIRST verification (N=2): does `system_step` lower ancilla-clean?** A test that
   `ancillas_restored(start)` for sampled starts. If false → STOP + report (Bennett
   fallback is Stage-1b-B.2, not this task).
3. `tests/ring_system.rs`: the certificate at N=2 (fast smoke) — `restored_ok ∧
   run==run_open ∧ run==eval^R ∧ bus_quiescent`, small R, bounded samples.
4. Scale to N∈{4,8} as `#[ignore]`d (heavy) tests.

## Review corrections (v2 — adversarial review, SHIP-WITH-FIXES → applied)

The reviewer traced the code and **verified the two big risks are resolved by
construction**; the corrections below supersede any over-caution above.

1. **CRUX (Decision 4) — `system_step` lowers ancilla-clean BY CONSTRUCTION.** The
   only ancilla-generating construct in the whole TISC vocabulary is `emit_mcnot`'s
   k≥2 CCC-NOT chain (AND-reduce K1 → R1 → reverse-replay, `lower.rs`), which is
   ancilla-clean per instance — and it appears only in the `decode_execute` gated-TK
   adder, i.e. the *same* construct `step` already lowers clean (what `StepRing`'s
   `ancillas_restored==true` proves). emit/absorb CNOTs (`enable^=p`, `addr^=IR.rt`)
   are `cnot_prim` = **R1 fast path, zero ancilla**; `cross_factor_gated_swap` is
   `gated_fredkin_prim = K2, zero ancilla`; plain Fredkins K1; the reshapes
   (`SwapProd`/`AssocProd`/`split_fields`) canonicalize to zero-cell `Permute`. There
   is **no `Branch`/`Trace`/`Reg`** in `decoder.rs`/`step.rs`/`plumbing.rs`/
   `addressing.rs`, so no K-only-dirty promotion. **Expect `ancillas_restored: true`
   at N=2** — verify it, but it is forced, not hoped.
2. **Bennett fallback is OUT OF SCOPE (→ 1b-B.2), not "the real path."** There is no
   `ClosedSystemRing` and `closed_step_ring` is hardcoded to `state_ty()`. Since the
   plain path closes by construction, do NOT build a Bennett wrap in 1b-B; if
   `ancillas_restored` ever fails, STOP and report using the **traced lowering**
   (`lower_iso_with_ancillas_traced`) — dump the failing `StateVals`,
   `ancilla_mismatches` indices, and their `AncillaOrigins` labels (tells you which
   construct leaked — the diagnosis difference between 10 min and 10 h).
3. **`SystemRing::run` must NOT inherit `StepRing::run`'s
   `debug_assert!(ancillas_restored)`** (`ring_run.rs:102`). `cargo test` is a debug
   build → that assert would panic *inside* `run` before the certificate can observe
   `run==run_open`/`bus_quiescent`, and it doubles per-call cost (an extra full
   `simulate`). The certificate test asserts `restored_ok` **explicitly first**, then
   calls a `run` with no internal assert.
4. **bus-quiescence is an ISO-level property, already proven at value level**
   (`assert_oracle_fast_with_bus_quiescence`, `composite.rs:614`, `#[ignore]`d).
   `SystemRing::bus_quiescent` is therefore a **lowering-fidelity cross-check**, not a
   new proof obligation: after one revolution, assert the `bus_range` data lines are 0.
   Compute `bus_range = COMPOSITE_BITS .. SYSTEM_BITS` **symbolically** (`types.rs`
   consts), and **assert `n_data == SYSTEM_BITS` first** so a lowering-order regression
   is caught loudly instead of silently checking the wrong lines. Do NOT hardcode
   `n_data - 24`.
5. **COST posture FLIPPED — N=2 smoke is expected FAST + non-ignored.** The Layer-1
   resident-memory ring `#[ignore]`s were slow due to `Reg`/K0 buffering, which
   `system_step` has NONE of; its cell count ≈ `step`'s (which `ring_run.rs` lowers +
   simulates fast, non-ignored) + a few Fredkin/CNOT cascades. `balance_compact` is
   O(active cells). So keep the **N=2 certificate non-ignored**; `#[ignore]` N=4/8
   only if wall-clock actually demands it (the watch driver is `simulate` on the wide
   input, not lowering). N=2 IS representative for *correctness* (ancilla-cleanliness
   is per-instance/N-independent; the MEXCH tree is C1-clean at N∈{2,4,8} already) —
   N scales only cost.
6. Provenance: repo tip is `fa2fca3` (not `1f59744`).

## Questions for the adversarial reviewer (break these)

1. Is the DATA/ancilla partition right — can `SystemRing` really reuse
   `StepRing`'s `n_data = inputs − ancilla_init.len()` when BusMsg is DATA-but-zero,
   or does putting BusMsg in DATA break the `run` recirculation (it feeds *ancilla*
   outputs back, DATA forward — is BusMsg-forward correct given it's zero each
   boundary)?
2. Will `system_step` actually lower ancilla-clean, or do emit/absorb's CNOTs leave
   dirty ancillas that make `run != run_open`? What's the minimal check?
3. Is `bus_line_range` (last `3·W` data bits) the correct offset given the
   `((Cpu,Mem),Bus)` nesting and the lowering's data-line ordering — or could the
   BusMsg bits be interleaved/elsewhere?
4. Is `bus_quiescent` per-revolution sufficient, or does the certificate need it at
   every *internal* phase boundary too (Prop 1.3 silent residue)?
5. Is N=2-first + `#[ignore]` scaling the right risk posture, or is there a reason
   the closure could hold at N=2 but fail at N=4/8 (so N=2 is not representative)?
6. Anything requiring a `StepRing`/`balance_compact`/theseus-core change I'm missing?
