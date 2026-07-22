# BVEQ slice-3a HARDENING review r2 — F1–F5 fix verification (adversarial, fresh-context)

**Reviewer:** fresh-context Fable 5, did not write the fix. Worktree @ `9d9da65` (clean before+after; teeth corruptions reverted). Diff vs `af45dba`: 5 files, +438/−94. Checklist: bveq-slice3a-impl-review.md F1–F5.

*(Advisor's faithful capture, L21.)*

## VERDICT: SHIP

All five findings fixed as prescribed; the critical ones have PROVEN teeth (the rewritten I2 flips under two independent corruptions of the real eval path — the old self-checker flipped under neither; neutering the F2 guard sends the test into 2^24 OOM). No over-rejection, no I1 regression, no blocker-class new hole. 4 LOW residuals for 3b, none gating.

## F1 (HIGH — oracle-path I2) — FIXED, TEETH PROVEN
`i2_eval_via_spec_matches_table_flat4` now builds a SYMBOLIC-ONLY twin (spec + empty tables) + a TABLE twin and asserts `theseus_eval::eval(sym) == eval(table)` over the whole Flat(4)³ domain (4096). Empty table ⇒ table-wins can't short-circuit ⇒ every eval routes through the REAL interp.rs eval-via-spec path. **TEETH run 1** (interp-local `out_bytes[0]^=1` at interp.rs:277): I2 FAILED (the exact corruption class r1's self-checker survived). **TEETH run 2** (invert bit-set in the shared `SpecTransfer::eval_bytes`, prim.rs:388): I2 FAILED. Both reverted. Control `f1_control_corrupted_spec_detected_by_eval` passes at HEAD.

## F2 (MED — hang bypass) — FIXED, TEETH PROVEN
New `theseus_core::enumerate::cardinality_u64` (structural, saturating). `core::admit` + `recognize_prim` (compile.rs:1007-1018) run the arithmetic precheck as the FIRST statement when `symbolic.is_some()` → `forward.len() != card` ⇒ `SymbolicOnly`/`PartialTableWithSpec` BEFORE any `enumerate_values`. `f2_partial_table_with_spec_rejected_fast`: Flat(8)³ prim + 1-entry table → rejected <2s. **TEETH:** restoring the old `is_empty()&&is_some()` guard + running the F2 test under `ulimit -v 6000000` → the test reached `enumerate_values(2^24)` and died (SIGABRT, alloc failed) — the enumeration is genuinely unreachable with the fix. Reverted. Over-rejection check: full-table+spec prims pass the precheck; table-only prims skip it (core 54/54, eval 126/126).

## F3 (MED — malformed spec / release corruption) — FIXED, RELEASE-ACTIVE
`PrimSpec::validate_against(input_ty, output_ty)` (first statement of admit → `InvalidSpec`): per-direction leaves.len()==count_leaves, row len==width, width==every Flat(n) in operand+result types. `try_value_from_leaf_bytes` (non-panicking Result, real if-check) threaded through interp.rs eval → clean `EvalError::DomainError`; `value_to_leaf_bytes` range check upgraded debug_assert→assert (release-active). `f3_out_of_range_byte_produces_clean_eval_error` is a genuine RELEASE L20 reproducer (under the OLD debug_assert-no-op code the malformed spec → silent out-of-domain Value → eval Ok → test would fail). `f3_spec_too_wide_width`/`_wrong_leaf_count`/`_short_row` → all InvalidSpec, green.

## F4 (LOW) — FIXED
admit both-present mirrors the loop over `def.backward` (decompose output_ty, spec.backward.eval_bytes, recompose input_ty) → SpecTableMismatch; `f4_corrupted_backward_spec_rejected` caught (pre-fix nothing checked backward).

## F5 (LOW) — FIXED
One shared `SpecTransfer::eval_bytes` in core, called by interp (oracle) + admit forward + admit backward; the I2 test's private copy deleted (calls theseus_eval::eval). Cross-crate agreement test `f5_cross_crate_decompose_agreement` (core value_to_leaf_bytes vs tisc value_to_leaf_bytes_pub over Flat(4)³).

## I1 / hygiene — PASS
Table-wins unchanged; prechecks fire only when symbolic.is_some() → table-only prims byte-identical. biodivine still only in tisc (core BDD-free). New exports additive. core 54/54, eval 126/126, symbolic_prim 17/17; clippy --workspace clean.

## Residuals (LOW, for 3b — none blocking)
1. `validate_against` doesn't walk the BvExprs' `Var{operand,bit}` indices (operand≥leaves → loud eval panic; bit≥8 → release-masked silent wrong-bit). MITIGATED: both-present certified spec==table whole-domain; symbolic-only goes through `admit_symbolic` width∈1..=8 + BDD mutual-inverse. Add a cheap expr walk in 3b.
2. F5 agreement test covers only Prod-of-Flat; the Sum zero-padding arm untested (impls verified line-identical by inspection).
3. No timed partial-table+spec test aimed at recognize_prim specifically (identical first-statement guard; empty-table case covered).
4. `cardinality_u64`'s Flat(n)≤8 check is debug_assert-only (unreachable given the project Flat≤8 cap).

## Re-run (post-revert, HEAD 9d9da65)
clippy --workspace clean; theseus-core 54/0; theseus-eval 126/0; symbolic_prim 17/0 (0.25s incl. the Flat(8)³ F2 test). Flip runs: interp corruption 14/3 FAILED; eval_bytes inversion 14/3 FAILED; F2 neuter → SIGABRT under 6GB cap. Worktree clean.
