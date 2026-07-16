# theseus-tisc test-suite cleanup plan (review-guided)

Goal: make the DEFAULT `cargo test -p theseus-tisc` (skips `#[ignore]`d) run in
**<~5 min** (currently ~25-30 min) so it is usable as a merge gate — WITHOUT weakening
the differential-oracle norm. **Every `#[ignore]` MUST be paired with a fast bounded
companion that evals the same path**, and the authoritative whole-domain / real-CPU /
full-recirculation coverage must stay runnable via `--ignored`. From a fresh-context
audit (timings measured). Cost driver everywhere: per-input `theseus_eval::eval` of a
wide iso, repeated over a domain.

## A. Dead code to REMOVE (safe; unblocks clippy honesty)

- `src/compile.rs` `fn leaf_types` (~95-115) — no live caller (only self-recursion).
  Remove fn + its `#[allow(dead_code)]`.
- `src/compile.rs` `fn u8_to_leaf_value` (~141-147) — no callers. Remove + `#[allow]`.
- `src/compile.rs` `fn invert_instr` (~673-692) — no callers (leftover ADR-0042
  scaffolding). Remove + `#[allow]`.
- `src/decoder.rs` `pub fn select_mem_composite` (the no-`_n` wrapper, ~309-312) — zero
  callers; superseded by the emit/mem/absorb `select_mem_on_bus` path. Remove.
  **KEEP `select_mem_composite_n`** (composite.rs `c1_re_verify_at_n` still calls it).
- After removal, `cargo build -p theseus-tisc --tests` must show no new warnings.
- Do NOT remove `run_on_cpu_fast`, `build_mexch_prim`, `select_mem_composite_n`,
  `select_mem_on_bus_n` — all live.

## B. Bound whole-domain oracles + add `#[ignore]`d whole-domain twins

Pattern: where a test calls `assert_oracle_fast(name, iso)` (which enumerates the
WHOLE `2^leaves` domain), replace with a **strided-sample** variant capped at ≤256
inputs (mirror the existing `assert_oracle_sample` index-striding, but run
`run_on_cpu_fast` + `eval`), AND add a sibling `#[ignore = "authoritative whole-domain
oracle; run via --ignored"]` test named `*_whole_domain` that calls the original
unbounded `assert_oracle_fast`. So the gate runs the 256-sample oracle (fast) and
`--ignored` still runs the full `2^18` enumeration.

- `tests/compile.rs`: `mem_spill_15_leaves_swap_oracle_agrees`,
  `mem_spill_max_leaves_swap_oracle_agrees` (2^18, the biggest), `mem_store_reload_
  round_trip_oracle_agrees`, `spill_14_leaves_ceiling_oracle_agrees` → bound + twin.
- `tests/composite.rs`: `composite_oracle_spill_15_leaves`,
  `composite_oracle_store_reload_round_trip`, `composite_oracle_max_ceiling`
  (14+MEM_WORDS leaves) → bound + twin.

## C. Ring tests — fast companions + `#[ignore]` the slow recirculation oracles

- `tests/ring_run.rs`: ADD `ring_matches_eval_oracle_fast` (`MAX_R=2`, `pc∈{0,4}`),
  asserting `ring.run == eval^R` AND `run == run_open`. Then add `#[ignore =
  "authoritative recirculation oracle; run via --ignored"]` to: `ring_matches_eval_
  oracle`, `true_recirculation_matches_eval_oracle`,
  `run_true_equals_run_when_ancillas_restored`, `closed_ring_matches_eval_oracle`,
  `closed_ring_forward_ancillas_clean_and_boundary_fixed`. KEEP fast (do not ignore):
  `ancillas_restored_after_one_revolution`, `ring_area_independent_of_revolutions`,
  `zero_boundary_vs_bennett_boundary`.
- `tests/ring_system.rs`: ADD `system_ring_matches_eval_oracle_fast` (`MAX_R=2`,
  `pc∈{0,4}`, incl. `bus_quiescent`), asserting `run == run_open == eval(system_step)^R`.
  Then `#[ignore]` `ring_matches_eval_oracle`, `true_recirculation_matches_eval_oracle`,
  `open_vs_closed_equivalence`. KEEP `ancillas_restored_after_one_revolution` and
  `bus_quiescent_after_one_revolution` (single-revolution; optionally bound to 2 PCs).

## D. decoder / step / cross_ctrl / branching — bound & de-dup

- `tests/decoder.rs`: the shared `ctx()` runs `lower` **+ `balance`** of
  `decode_execute` (~50s, the `balance()` is the heavy half). Keep the eval-vs-reference
  semantics inline in `full_decoder_*` (fast); move the balanced-`simulate` faithfulness
  behind `#[ignore = "authoritative balanced-simulate faithfulness; run via --ignored"]`
  (`full_decoder_faithfulness_all`), keeping ONE inline `_smoke` faithfulness sample
  (e.g. `Op::Add[0]` + one reserved) so the balance path still has gate coverage.
- `tests/step.rs`: shrink `pc_inc_exhaustive_all_256` and
  `pc_add_br_exhaustive_br0_is_identity` to ~8-16 sampled PCs (KEEP the carry
  boundaries `0,1,7,8,0x7F,0x80,0xFE,0xFF`); optionally add `#[ignore]`d `*_all_256`
  twins.
- `tests/cross_ctrl.rs`: replace the `0..=255 × 0..=255` inner sweeps (both) with a
  strided edge set (`[0x00,0x01,0x02,0x55,0xAA,0xFE,0xFF]` × same, ~49 pairs); keep both
  ctrl sweeps. Self-inverse is structural, so edge coverage suffices.
- `tests/branching.rs`: DELETE `v1_counterexample_produces_distinct_outputs`
  (redundant with `step.rs::v1_counterexample_distinct_post_states` — identical ROM,
  states, assertions). MERGE `taken_branch_with_come_from` + `branch_forward_then_
  backward` into one forward-then-reverse test.

## E. (only if still >5 min) mem_exchange N=8

`#[ignore]` `c1_mexch_tree_ancilla_clean_n8` (4096 inputs) behind a bounded-sample
companion; keep N∈{2,4} whole-domain.

## Acceptance

- Default `cargo test -p theseus-tisc` GREEN and completes well under ~5 min (warm).
- `cargo test -p theseus-tisc -- --ignored` GREEN (the authoritative tier still runs).
- `cargo clippy -p theseus-tisc --all-targets -- -D warnings` clean (dead code gone).
- No differential-oracle path loses gate coverage: every `#[ignore]`d oracle has a
  fast bounded companion evaluating the same path.
