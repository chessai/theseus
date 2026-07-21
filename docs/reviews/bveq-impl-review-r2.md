# BVEQ slice-1 HARDENING — adversarial re-review r2 (verdict: DO-NOT-SHIP)

**Reviewer:** fresh-context adversarial impl-reviewer (did not write the hardening). Target: worktree `01KY1X4GQVJ033PN86V1VGN6VV` @ `0d6efa3` (base = slice-1 `8220a38`). Checklist = `docs/reviews/bveq-impl-review.md` (the 5 MAJORs). All claims verified by reading the code AND flip-experiments in the worktree (release, nix, foreground); every experiment reverted — worktree clean at HEAD on finish.

*(Advisor's faithful capture of the fresh-context r2 re-review for the durable record, L21. The advisor fix that resolves every item below is committed at `b5d586b` as "BVEQ hardening r3 (advisor-fix): N1-N5".)*

## VERDICT: DO-NOT-SHIP

All five MAJORs were addressed and every demonstrated escape from r1 is closed — but the MAJOR-1 fix was implemented with the wrong semantics (**unconditional** upper-bit check instead of under-the-Flat(w)-constraint), and that one choice (a) makes `equiv` **reject every reversible model at w<8, including `identity ≡ identity`** — the entire w<8 accept path is dead code, caught by **no** test because the suite contains **zero positive w<8 cases**; (b) **strips the teeth from `bveq_major2_global_constraint_escape`** — restoring the global constraint kept the test green; and (c) diverges from the Z3 leg's I4 semantics. Separately, the new `operand_regs` parameter introduces a **new panic path** (violating the never-panic contract MAJOR-3 just established), and the frame check misses short bit-rows (index panic). Everything here fails **safe** (spurious reject / panic, never false-equal) — not a corruption vector — but not shippable as the trust core slice-2 wires into.

## The 5 fixes — implemented correctly? teeth?

- **MAJOR-1 (I4 both sides):** implemented YES, but with a semantics change (unconditional, `side_bdd.sat_witness()` on the RAW bdd, vs the original under-constraint `c.and(&a_bdd)`) that breaks w<8 completeness — see N1. The in-code "checking under the constraint would be circular" justification is WRONG: under-constraint = invariant preservation. Flip: reverting to full ORIGINAL semantics (a-side + under-constraint) → `major1` FAILED (so the test reproduces the r1 escape); reverting to a-side-only-unconditional → `major1` STAYED GREEN (the both-sides guard is not individually isolated).
- **MAJOR-2 (operand-set):** implemented mechanically YES. Teeth: **NO (BLOCKER)** — restoring the global constraint kept `bveq_major2_global_constraint_escape` GREEN (the unconditional I4 rejects the model's r0 uppers regardless of constraint scope). The test does not guard its own fix.
- **MAJOR-3 (Err not panic/alias):** implemented YES for every enumerated r1 path (`check_reg`, `checked_var`, asserts→Err). The three `major3_*` tests match the specific error variant. Residual panic paths remain (N2, N3).
- **MAJOR-4 (mismap canary):** implemented YES, teeth flip-VERIFIED (neutering the `expr_to_bdd_shifted` injection → `major4` FAILED). Real encoder-level canary, not the r1 vacuous one.
- **MAJOR-5 (reject alias-illegal):** implemented YES, exact mirror of `asm.rs` distinctness; five tests, teeth by construction.

## NEW-HOLES (introduced/exposed by the fixes)

- **N1 — BLOCKER: unconditional I4 ⇒ `equiv` rejects EVERYTHING at w<8.** Demonstrated at HEAD: `equiv(identity, identity, 4, &[0])` → `NotEquivalent` (reg0 bit4, witness r0=16); `equiv(Exch{0,1}, itself, 4, &[0,1])` → `NotEquivalent`. This is total: every `symbolic_exec` output is a bijection on the 64-bit file, and a component of a bijection cannot be identically false → for ANY real model, ANY w<8, some operand upper output bit is raw-satisfiable → reject. The accept path is a constant-reject on its intended domain. No test caught it (no positive w<8 case). **Fix:** I4 under the operand-scoped constraint, both sides (`c.and(side_bdd)`); add positive w<8 tests (identity, Exch, a Flat(4) accept); re-arm major1's junk with low-bit-dependent junk.
- **N2 — BLOCKER: `operand_regs` OOB entry panics** — `equiv(&id,&id,4,&[9])` → panic at `VarMap::var` (reached from the constraint builder). The one new input surface is the one place nobody range-checks. **Fix:** validate entries < NUM_REGS → Err.
- **N3 — MAJOR: `FrameSizeMismatch` checks only the outer Vec** — a short bit-row (`regs[0]` len 4) panics `index out of bounds` at the equality loop. A slice-2 spec emitting only w bits (the obvious bug) panics instead of Err. **Fix:** validate `regs[r].len() == BITS` both sides.
- **N4 — MAJOR: BDD and Z3 legs implement DIFFERENT I4** — Z3 evaluates its I4 disjuncts under the (asserted) constraint; the BDD leg checks unconditionally → on any pass-through case (identity w=4) Z3 says equal, bveq says not-equal. Corpus has no such case so it stays latent green — the differential cannot catch the very N1 bug it exists for. **Fix:** align both legs on the N1 semantics; add a w<8 pass-through accept corpus case.
- **N5 — contract notes on the operand-set (fail-safe, document):** empty `operand_regs` at w<8 degrades to full Flat(8) equality (stricter, surprising). Omitting a touched register → spurious reject only (safe). **Over-declaring** a register that isn't Flat(w) HIDES differences conditioned on its upper bits — once N1 is fixed, this is the live false-equal-by-declaration vector. **Slice-2 must derive `operand_regs` from the prim's TYPE, never from the candidate sequence** — belongs in the doc + slice-2 spec. Residual: unbounded `expr_to_bdd`/`eval_expr` recursion → stack-overflow abort on a pathological expr (unreachable from `symbolic_exec`); document at the "Never panics" claim.

## Also verified
MINOR-1/2/4 follow-ups landed (L-b rs=0 / rt∈{3,4,5} / Cswap ctrl=0 / immediates in the wide sweep; IR==0 & PC==budget asserts; T2/T8 non-reflexive; `easy-smt` dropped, `z3-crosscheck=[]`). L-a untouched and green (full 256²/256³). Slice discipline held (only bveq.rs/tests/Cargo.toml + the r1 review doc).

## Test output
Baseline default suite at HEAD `0d6efa3`: `test result: ok. 21 passed; 0 failed; 1 ignored; finished in 137.82s`. Z3 leg `t9 ... ok` (5-case corpus, live). Flip results: M1-full-original → major1 FAILED (teeth); M1-partial → major1 green (guard not isolated); M2-global → major2 **STAYED GREEN (no teeth — BLOCKER)**; M4-injection-off → major4 FAILED (teeth).

## Required before merge (each with a flip-verified reproducing test, L20)
1. N1: I4 both-sides UNDER the operand-scoped constraint; positive w<8 accept tests; re-arm major1 junk (low-bit-dependent).
2. N2: range-validate `operand_regs` → Err.
3. N3: validate inner row lengths → Err.
4. MAJOR-2 teeth: re-arm with non-operand-register corruption (+ over-declaration case), flip-verify against a re-globalized constraint.
5. N4: align `build_smt_script` I4 with (1); add a w<8 pass-through corpus case.
6. N5: document empty-set + "operand_regs comes from the prim type" obligation + recursion-depth abort.
