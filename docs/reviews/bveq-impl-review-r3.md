# BVEQ hardening — adversarial re-review r3 (advisor fix `b5d586b`) — verdict: SHIP

**Reviewer:** fresh-context adversarial impl-reviewer (did not write the fix). Target: worktree `01KY1X4GQVJ033PN86V1VGN6VV` @ `9e05041` (advisor fix `b5d586b` over hardening `0d6efa3`). Checklist = the 6 REQUIRED-BEFORE-MERGE items from `bveq-impl-review-r2.md`. Verified by reading + flip-experiments (nix, foreground); all reverted, worktree clean at HEAD on finish.

*(Advisor's faithful capture of the fresh-context r3 review for the durable record, L21.)*

## VERDICT: SHIP

The advisor fix is correct and complete. All six required items implemented with the r2-demanded semantics; every critical test has flip-verified teeth; the BDD and Z3 legs agree on the under-constraint I4 (confirmed live against z3 4.16.0); and — the load-bearing question — a dedicated 13-probe adversarial campaign found **no false-accept**: the under-constraint I4 is exactly the invariant-preservation obligation, not a weakening. The only residual unsoundness vector is the already-documented, already-pinned N5 over-declaration hazard, a *caller* obligation by design.

## The 6 required fixes — correct? teeth?

1. **N1 (I4 under the operand-scoped constraint, both sides): CORRECT, teeth VERIFIED.** `check = constraint.and(side_bdd)` for both `a` and `b`; positive w<8 accepts (`bveq_n1_identity_accepts_at_flat4`, `_exch_`, `_flat4_xor_spec_`) pass; rejections preserved (`bveq_i4_must_reject_add_flat4`, `major1` both orders + low-bit-dependent junk). FLIP: reverting I4 to unconditional → the 3 `bveq_n1_*` tests FAILED (rejects stayed green). The positive-accept suite is exactly the guard r2 was missing.
2. **NEW-HOLE probe: SOUND, no false-accept (dedicated section below).**
3. **N2 (operand_regs validation): CORRECT, teeth VERIFIED.** `>= NUM_REGS` → `Err(OperandRegOutOfRange)`. FLIP: removing the loop → test FAILED with the exact r2 panic at `VarMap::var`.
4. **N3 (bit-row length): CORRECT, teeth VERIFIED.** → `Err(RowSizeMismatch)`. FLIP: removing the loop → test FAILED with the exact r2 index-OOB panic.
5. **MAJOR-2 teeth: NOW REAL, flip-VERIFIED.** FLIP: re-globalizing the constraint → ALL THREE major2 tests flipped to FAILED (including the old `global_constraint_escape`, which now has teeth precisely because the N1 under-constraint fix removed the unconditional-I4 masking). `overdeclaration_hides_corruption` documents (not endorses) the N5 hazard.
6. **N4 + N5: CORRECT.** Z3 I4 disjuncts evaluate under the globally-asserted per-operand constraint (Z3 was always under-constraint; BDD now matches). `exch_eq_flat4` w<8 accept case in the t9 corpus (`expect_equal: true`) — would alone have caught N1. Feature builds; t9 ran live (z3 4.16.0), 6-case corpus, ok. N5 doc carries all four statements (derive-from-TYPE obligation; over-declaration hazard + pointer to the pinning test; empty-set degradation; scoped never-panics + recursion residual).

## NEW-HOLE: can under-constraint I4 FALSE-ACCEPT a width violation? — NO.

**Why sound:** for an operand upper output bit `f`, accept iff `constraint ∧ f` is UNSAT — i.e. `f ≡ 0` on the canonical Flat(w) domain (exact, canonical ROBDD). The constraint zeroes ONLY operand upper *input* bits, which are genuinely always 0 on every reachable input (typing invariant, inductively maintained). Any `f` depending on an unconstrained bit (operand low bits = the carry class; any non-operand bit) stays satisfiable → rejected. The only "accepted violations" are functions differing from 0 solely on **unreachable** states — which is the definition of a Flat(w)-preserving op, not a violation. Unconditional falseness (r2's N1) was never the right obligation (unsatisfiable for any bijection).

**13 executed probes** (temp `bveq_probe.rs`, all vs identity at w=4, operand_regs=[0,1] unless noted; deleted after; 13 passed):

| # | Attack | Expected | Got |
|---|---|---|---|
| P1 | `r0.4 ← Var(1,6)` upper-from-upper | accept (0 on domain) | Ok ✓ |
| P1b | sides differ on r0.4, both 0 under constraint | accept | Ok ✓ |
| P2 | `r0.4 ← Var(0,0)` low-bit escape (carry class) | REJECT | Err ✓ |
| P2b | `r0.4 ← Var(1,2)` other operand low bit | REJECT | Err ✓ |
| P3 | `r0.4 ← Var(3,7)` escape on NON-operand reg | REJECT | Err ✓ |
| P3b | `r0.4 ← Var(0,0) ∧ Var(3,7)` AND-guarded | REJECT | Err ✓ |
| P4 | `r2.0 ← Var(2,0) ⊕ Var(0,6)` non-operand corruption gated on operand upper | accept (unreachable gate) | Ok ✓ |
| P5 | `r2.0 ← Var(2,0) ⊕ Var(3,7)` non-operand on free bits | REJECT (equality branch) | Err ✓ |
| P6 | `r0.4 ← Mux(Var(0,4),1,0)` mux-obfuscated pass-through | accept | Ok ✓ |
| P6b | `r0.4 ← Mux(Var(0,5),0,Var(1,1))` low-bit escape in mux else | REJECT | Err ✓ |
| P7 | byte-Add vs Flat(4) addInto, EMPTY operand_regs | REJECT (degrades to full Flat(8) eq — fail-safe) | Err ✓ |
| P8 | duplicate entries [0,0,1,1] | accept, no panic | Ok ✓ |
| P9 | `r1.5 ← Var(0,3) ∧ Var(1,3)` sneaky carry-like AND | REJECT | Err ✓ |

No transfer that is not a valid Flat(4) op on the canonical domain got Ok with honestly-declared operand_regs. **Residual burden (by contract, not holes):** (i) over-declaration false-accepts (N5, pinned, discharged by slice-2 deriving from prim TYPE); (ii) the canonical-at-entry premise (machine typing invariant, outside equiv); (iii) equiv certifies model≡spec, not spec well-formedness (upstream `admit()`); (iv) P4-class accept is sound under (ii).

## Re-run (exact)
Clippy clean. `cargo test -p theseus-tisc --release --test bveq` → `ok. 28 passed; 0 failed; 1 ignored; finished in 138.73s`. z3-crosscheck builds; `t9 ... ok` (live, z3 4.16.0). Flips: N1-unconditional → 3 n1 FAILED; M2-global → 3 major2 FAILED; N2/N3-unvalidated → the exact r2 panics. All reverted; clean at HEAD.

## Bottom line
Every r2 REQUIRED item implemented with the demanded semantics + a flip-verified reproducing test (L20). The hardest question — whether scoping I4 under the constraint opened a false-accept — is answered NO, by argument and by 13 adversarial probes. **SHIP.** Slice-2 must derive `operand_regs` from the prim's type, never the candidate sequence.
