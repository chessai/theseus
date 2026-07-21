# BVEQ slice-2 impl review — `bveq_verify_emission` wired into `recognize_prim` (adversarial, fresh-context)

**Reviewer:** fresh-context adversarial impl-reviewer (Fable 5), did not author. Target: worktree `01KY2JT8KX9WNJQZ4FXJ8ETRRK` @ `7f4c2f2` (clean before+after; teeth-neuter applied and fully reverted). Diff: compile.rs + tests/bveq_recognize.rs (+829/−40). Spec: design r2 (decomposed check) + the slice-2 design-review spike (18/18 agreement).

## VERDICT: SHIP

The verifier is sound within the machine's canonical-domain contract, non-vacuous (teeth-neuter flips 4 tests to FAILED), fail-closed with no fall-through, TYPE-derived on `operand_regs` at every one of its 15 call sites, zero codegen change on the accept path, memexch correctly excluded. Three non-blocking hardening notes.

## A — `bveq_verify_emission` (compile.rs:879–984): SOUND
1. `symbolic_exec` fail-closed (884–890): every error → `Err(EmissionVerificationFailed)`; rejects non-register-pure / OOR / ISA-illegal-aliased. No emit on error.
2. Frame closure (915–948): non-operand output bits must equal their identity variable (canonical BDD equality — total over all 2^64 states); operand output bits' `support_set()` ⊆ operand-register variables.
3. Pointwise (950–981): every `def.forward` row checked via `eval_transfer` (operand regs = row leaf bytes, non-operands 0).
**Is (2) sufficient to make (3) total? YES** — step 2 proves non-operand outputs are identity everywhere and operand outputs depend only on operand inputs; so agreement at the single non-operand setting extends to all, and the transfer equals `prim ⊗ identity` on `{canonical operand values} × {all frame values}`. BDDs and `eval_transfer` evaluate the SAME `RegTransfer`, so 2 and 3 can't diverge; slice-2b reconciled `symbolic_exec` vs the real netlist.
**Corrupting-body attack:** the only residual freedom is behavior at *non-canonical* operand bytes (a Bit leaf holding 5) — a body agreeing on {0,1}² but diverging at 5 passes. NOT a soundness hole vs compiled programs: the canonical-layout invariant (ADR-0044 §2.1) makes such states unreachable between instructions, and the old enumeration lambdas had the SAME domain restriction; the new check is strictly stronger (adds total frame + support closure). Contractual, not a regression.
**Caller obligation (pin as doc):** step 3 iterates `def.forward` rows, so canonical-domain totality relies on the gating enumeration having verified the table is total over `enumerate_values(input_ty)` — true at every current call site.

## B — operand_regs TYPE-derived: YES at all 15 call sites
Every call passes the `with_registers` closure arg `regs` (leaves `(0..n_in)`, `n_in = num_leaves(input_ty)`), derived from the prim TYPE + `ctx.homes`, never the candidate. N5 demonstrated: `t_operand_from_type_rejects_corruption` (tests:422-461) — `Xor r1,r0; Ccnot rd=1,rs=3,rt=4` corruption REJECTED with type-derived `[0,1]`, ACCEPTED (dangerously) with candidate-derived `[0,1,3,4]`.

## C — Fail-closed, no fall-through: YES
Each arm `return with_registers(..., |regs| { ...; bveq_verify_emission(&body, def, regs)?; Ok(body) })` — the `return` precedes evaluation, so a verifier `Err` propagates straight out of `recognize_prim`; no path to a later arm. All failure paths → Err. (Nit: `VarMap::var` asserts, so a *direct* pub caller with operand_reg ≥ NUM_REGS panics; unreachable from recognize_prim.)

## D — I3 zero codegen change on accept: YES
Verifier wired strictly AFTER body construction, before `Ok(body)`; accept returns byte-identical instructions. Full release suite passed the gate (verify_passed) → goldens unchanged. Aliased-home behavior change present + tested (`t_aliased_home_xor_rejected`:465, `t_aliased_home_swap_rejected_by_recognize`:479). memexch NOT run through the decomposed check (non-register-pure) — keeps F-3 verify + F-2 home guard.

## E — TEETH: CONFIRMED
Inserted `return Ok(());` atop `bveq_verify_emission` → re-ran teeth tests:
```
failures: t_aliased_home_xor_rejected, t_operand_from_type_rejects_corruption,
          t_transposed_xor_rejected, t_wrong_arm_add_for_xor_rejected
test result: FAILED. 1 passed; 4 failed
```
All 4 teeth tests FLIP to FAILED when the verifier is vacuous (transposed-operand, wrong-arm Add-for-xorInto, over-declaration frame-closure, aliased-emission). Survivor `t_aliased_home_swap_rejected_by_recognize` passes via the swap arm's pre-existing `AliasedRegisters` guard (defense-in-depth). Neuter reverted; suite green (33/33). **Arm coverage:** all 15 register-pure arms call the verifier (2reg×6, 2leaf×2, 3reg×3, immediates×3, kleaf×1); none skipped. Scratch-homed test real (`t_scratch_homed_recognition_passes`:362 — leaf 1 at Scratch(2), end-to-end recognize_prim, asserts Xchg paging + Xor body → proves the check runs on the register-pure `body`, MAJOR-1).

## F — Cost: cheap, no minterm build
No DNF/minterm anywhere — only per-bit `expr_to_bdd` of straight-line transfers + an O(|table|) pointwise loop (same asymptotics as the pre-existing enumeration in the same arm). 33-test release binary 12.7–20.9 s; per-recognition sub-second release. No new gate-timeout hazard.

## Re-run (release, nix)
Clippy clean. `cargo test -p theseus-tisc --release --test bveq_recognize` → `ok. 33 passed; 0 failed` (20.89s; post-revert 12.66s). Worktree clean at 7f4c2f2.

## Non-blocking notes (no fix required to ship; fold into slice-3)
1. Silent length-mismatch tolerance (957-961): `if leaf_idx < in_bytes.len()` quietly tolerates a length mismatch — unreachable at live call sites; a defensive `Err` would be stricter for future callers.
2. Pub-API panic on `operand_regs` entry ≥ NUM_REGS via `VarMap::var` assert (compile.rs:906) — consider `checked_var` / early range check (bveq_verify_emission is `pub`).
3. Pin the caller obligation from A (verifier assumes the gating enumeration proved table totality) as a doc-comment sentence, alongside the N5 note.
