# BVEQ slice-2 — rewire `recognize_prim` to `bveq` (retire recognizer transcription divergence)

**Status:** design r1 (pre-review). Author: advisor. Target: `recognize_prim` — the LIVE recognizer that
decides every lowering. A bug corrupts all compiled programs → highest bar (CLAUDE.md non-negotiable:
fresh-context adversarial design-review + impl-review + reproducing tests). Builds on slice-1 (`bveq`
`equiv`/`symbolic_exec`, merged) and slice-2b (`bveq_netlist`, merged — which proved `symbolic_exec` ≡
the real netlist for all 13 register-pure instructions).

## 1. Goal

Replace the recognizer's hand-written per-arm verification (`check_pattern_2reg`/`_3reg`/`_2leaf`/`_kleaf`,
which enumerate `prim.forward` against an inline lambda model) with a **`bveq`-based check**: the candidate
instruction sequence's symbolic model (`symbolic_exec`) proven equal to the prim's function via canonical
BDD. This **retires the recognizer-side transcription divergence** — the recognizer stops trusting its own
lambdas (`|a,b| (a, a^b)`) and instead uses `symbolic_exec`, the model slice-2b proved ≡ the real netlist.

Honest scope: this does NOT by itself break the enumeration wall (the prim is still a `forward` TABLE, so
building the prim's BDD still enumerates the table — Wall 2 is slice-3). The wins are (a) model
unification (one instruction-semantics source of truth, netlist-reconciled), (b) canonical/complete
verification, (c) the foundation for slice-3 (swap the prim's BDD source from table to symbolic spec, no
recognizer change).

## 2. Mechanism

Keep the pattern arms (each arm knows WHICH instruction sequence to propose). Replace the verification:

- Build the **prim's BDD** from `prim.forward`: minterm expansion of the truth table (the same technique
  slice-2b's `synthesize_gate` uses on `PrimDef::forward`) → a per-output-bit BDD over the leaf-register
  variables (via slice-1's shared `VarMap`).
- Build the **candidate's BDD** via `bveq::symbolic_exec(proposed_instrs)` → per-register BDDs.
- Compare via `bveq::equiv` (or a `equiv_bdds` core if refactored per slice-2b MAJOR-3): the candidate's
  operand-register outputs must equal the prim's, over the full frame, with the I4 upper-bits obligation
  **under the operand-scoped Flat(w) constraint** where the prim is `Flat(w<8)`.
- **`operand_regs` (the N5 obligation — CRITICAL):** derived from the PRIM's TYPE / leaf homes (the
  registers the prim's `Flat(w)` leaves are homed to), NEVER from the candidate sequence. `w` = the prim's
  leaf Flat width (8 for Bit/Flat(8) prims → I4 vacuous; <8 for narrow value-width prims).

## 3. Safety: keep enumeration as a CROSS-CHECK (the live-rewire net)

For the first release, do NOT delete `check_pattern_*`. Run BOTH the enumeration AND the `bveq` check and
**assert they agree** (both accept, or both reject) before emitting. A disagreement is a loud TCB bug
report (fail closed — reject, don't emit). This is the safety net for rewiring the live recognizer: even
if the `bveq` path has a subtle bug, the enumeration catches it, and vice versa. Remove the enumeration
only in a later cleanup once the cross-check has been green across the whole test suite.

## 4. Reachable widths (Wall 2 boundary)

- **2-operand `Flat(8)`** (Xor, Add, Sub, Exch, Rotl/Rotr, the 1-reg immediates): the table has 65 536
  entries — buildable → `bveq` check applies.
- **3-operand** (Cswap, Ccnot): the table exists only at **≤ `Flat(4)`** (3-op `Flat(8)` = 16.7M,
  unbuildable — Wall 2). Verify at the widths the table is actually constructed at. The pinned `kleaf`
  walk (`mde_spill_walk`) and any wide prim stay on the enumeration path until slice-3.
- The recognizer must pick the right check per prim (buildable table → bveq+enum cross-check; unbuildable
  → enum only, or defer).

## 5. Open questions for the design-review (BUILD a spike)

- **Q1:** Does the `bveq` check AGREE with the existing enumeration on EVERY currently-recognized prim
  (the whole test suite's prims)? Spike: wire the cross-check, run the full suite, report any disagreement
  (a disagreement is either a bveq bug or a latent recognizer-lambda bug — both valuable).
- **Q2:** How is `operand_regs` / `w` derived from the prim at the `recognize_prim` call site? What's in
  scope there (the prim's `input_ty`, the leaf→register `homes`/`leaves`)? Confirm the derivation is from
  the TYPE, and construct the case that would go wrong if derived from the candidate.
- **Q3:** Perf — building the prim BDD from a 65 536-entry table (minterm expansion) per recognized prim:
  acceptable in the gate? (slice-2b synthesized gate BDDs from ≤16-row tables; a 65 536-row prim is
  bigger — measure.) If too slow, is the cross-check gated to a subset or a `#[ignore]` tier?
- **Q4:** Which arms convert now vs. stay on enumeration (the pinned `kleaf`/`mde_spill_walk`, memexch,
  anything 3-op-at-Flat(8))? Is a partial rewire (structural 2-op/3-op arms only) the right first slice?
- **Q5:** Does reusing `equiv` work here (slice-2b found it takes `RegTransfer` both sides + builds its own
  VarMap)? Likely need the `equiv_bdds(&[Bdd], &[Bdd], &VarMap)` core refactor (slice-2b MAJOR-3) so the
  prim-from-table BDD and the `symbolic_exec` BDD compare under one shared map. This refactor is probably
  a prerequisite.

## 6. Invariants / review targets

- **I1 reject-don't-corrupt strengthened:** the cross-check must FAIL CLOSED on disagreement (reject, log)
  — never emit when the two paths disagree.
- **I2 operand_regs from TYPE:** the sole live false-accept vector (slice-2b N5); pin it with a test.
- **I3 no behavior change on the accept path:** every currently-recognized prim still recognizes to the
  SAME instruction sequence (the rewire is a verification change, not a codegen change) — the full suite +
  all goldens must be UNCHANGED. This is a pure-refactor-of-the-check slice.
- **I4 the enumeration stays** as the cross-check for ≥1 release (do not delete `check_pattern_*` yet).

## 7. Scope / what this is NOT

- NOT slice-3 (symbolic prim specs — the enumeration-wall break). The prim stays a table here.
- NOT a codegen change — the emitted instructions are identical; only the verification path changes.
- Goldens/energy untouched (I3).

## 8. Success criteria

The `bveq` cross-check agrees with enumeration on every recognized prim across the full theseus-tisc suite
(I3: zero codegen change, all goldens green); `operand_regs`-from-type pinned by a test; the cross-check
fails closed on an injected disagreement (teeth). Then the recognizer's verification is model-unified
(symbolic_exec, netlist-reconciled) and slice-3-ready.
