# BVEQ slice-3a impl review — symbolic prim rep + eval + admit (adversarial, fresh-context)

**Reviewer:** fresh-context Fable 5. Worktree @ `5b80c00` (clean before+after; teeth corruptions reverted). Diff: 27 files, +852/−94. Plan: design r2 + slice-3 design-review (7 steps, hazards).

*(Advisor's faithful capture, L21. Findings F1–F5 addressed by the slice-3a hardening fix-task on top.)*

## VERDICT: SHIP-WITH-FIXES

Mechanism sound; every stated invariant holds where tested (eval-via-spec doesn't diverge, table-wins precedence, teeth-tested adjoint involution, admit_symbolic a genuine BDD mutual-inverse, both fail-closed guards exist with the vacuity guard teeth-proven, the 45-site `symbolic: None` ripple purely mechanical, existing table prims behavior-identical). 5 findings; none a blocker (no eval divergence, I1 holds, shipped guards have teeth for the states 3a defines) but F1–F3 must land before 3b.

## A. BvExpr move — PASS
`BvExpr`+`eval_expr` in `theseus-core/src/bvexpr.rs` (variants/eval line-identical to deleted bveq originals; +PartialEq/Eq); bveq.rs:34 `pub use theseus_core::bvexpr::{BvExpr, eval_expr}` — one type, no divergent copy; core stays bdd-free (no biodivine dep). **F5 residual:** tisc keeps its own `value_to_leaf_bytes`/`_from_` (compile.rs:171) — a 2nd decompose/recompose next to core's (prim.rs:244/299); agree by inspection but NO cross-crate agreement test (design Q2 required one).

## B. PrimSpec + adjoint — PASS
`PrimSpec` = mandatory `(forward,backward)` pair (prim.rs:97-101); adjoint swaps tables AND specs (prim.rs:132-135); involution test passes (symbolic_prim.rs:144). **F4 (LOW):** admit both-present verifies only forward spec vs forward table; backward spec verified by nothing (matters once 3b's recognizer uses the swapped adjoint). Fix: mirror the loop over `def.backward`.

## C. Eval-via-spec — SOUND, but I2 lacks teeth on the real path (F1)
interp.rs:269-299: table-lookup first (TABLE WINS, I1), spec fallback = core-native decompose→eval_expr→recompose. **TEETH C1:** inverted the bit-set at interp.rs:282 → `eval_via_spec_symbolic_only` FAILED but **`i2_...` STAYED GREEN**: the I2 test (symbolic_prim.rs:117-132) RE-IMPLEMENTS the loop instead of calling `theseus_eval::eval` — the L19 self-written-checker class; the authoritative interp path is certified at ONE point. **TEETH C2:** flipping core recompose (prim.rs:330) failed BOTH (I2 has teeth on the shared core helpers, not interp's private loop). **F1 (HIGH) fix:** I2 must eval a SYMBOLIC-ONLY twin via `theseus_eval::eval` over the whole Flat(4) domain vs the table twin, + keep the corrupted-interp control (fails without, passes with — L20).

## D. Admit hang guard — PASS for symbolic-only; bypass (F2)
Guard is the FIRST statement of admit (prim.rs:236-238) → `AdmitError::SymbolicOnly` before enumerate_values; recognize_prim guard likewise first (compile.rs:1007-1014); timed tests confirm instant rejection of a genuine Flat(8)³ symbolic-only prim. **F2 (MED):** both guards are `forward.is_empty() && symbolic.is_some()` — a wide prim with a spec AND a 1-entry table bypasses both → `enumerate_values(2^24)`. No current code constructs it, but admit would reject only AFTER enumeration (never). Fix: compute domain cardinality ARITHMETICALLY from the type, reject `forward.len() != cardinality` before any enumerate.

## E. admit_symbolic — SOUND certificate; no type linkage (F3)
bveq.rs:1054: validates width∈1..=8 + backward shape, checks backward∘forward==id AND forward∘backward==id per (operand,bit) via BDD (canonical → exact mutual-inverse ⇒ bijective); non-vacuity control `admit_symbolic_rejects_non_inverse` (OR-for-AND → rejected). **F3 (MED):** nothing validates the spec against the prim's TYPES (width vs Flat(w), leaves.len() vs leaf count, per-row len==width). Malformed → eval panic, or in RELEASE silent out-of-range bytes (`value_from_leaf_bytes` range check is `debug_assert!` only, prim.rs:331) → I3 reject-don't-corrupt not met. Fix: `PrimSpec::validate_against(input_ty,output_ty)` in core::admit + harden the range check to a hard check (release too).

## F. Vacuity + symbolic-only guards — PASS, teeth-proven
bveq_verify_emission (compile.rs:884-893) rejects `forward.is_empty()` BEFORE step 1 + the pointwise loop. **TEETH F:** neutered the guard → `bveq_verify_emission_rejects_empty_table` FLIPPED to FAILED (verifier returned Ok having verified nothing — the f3.5 silent-vacuity class). Reverted. Load-bearing.

## G. I1 additive — PASS
45 `symbolic: None` lines across 27 files, every non-core hunk inspected = field literal only, no logic. Existing prims eval Result-identical (miss-path message string extended — nothing matches the old string); admit/recognizer/lowering unchanged for None/table prims; bveq_verify_emission stricter only in the empty-table state (no legit caller). Goldens/energy unchanged (gate + this review's full release run: ring_two_body 606s, ring_two_body_timing, fib_cpu, energy — all green).

## Re-run
Clippy `--workspace --all-targets` clean. `-p theseus-core -p theseus-eval -p theseus-tisc --release`: all targets green EXCEPT `step` = host OOM (Chromium held ~35GB; step.rs UNTOUCHED by the diff; single-thread re-run 12/0/3 ok). Post-revert: symbolic_prim 9/9, theseus-core 49/49, theseus-eval 126/126. Clean at 5b80c00.

## Required/recommended
- **F1 HIGH:** I2 via the oracle (`theseus_eval::eval` on a symbolic-only twin over Flat(4)) + corrupted-interp control (L20).
- **F2 MED:** arithmetic domain-cardinality precheck in admit + recognize_prim before any enumerate_values.
- **F3 MED:** `PrimSpec::validate_against` in core::admit; harden `value_from_leaf_bytes` range check (release too).
- **F4 LOW:** admit both-present also verifies backward spec vs backward table.
- **F5 LOW:** single shared eval-via-spec helper in core (kills 3 copies) + cross-crate decompose agreement test.
