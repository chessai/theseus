# A-6 (D) — BDD-based reversible synthesis: the general, structure-preserving prim→gates mechanism

**Decided by user (2026-07-25):** option (D). The general synthesizer must be BOTH general (any
prim, automatic, no per-prim hand work) AND structure-preserving/compact (no ANF degree blowup).
BDD synthesis is that root-cause mechanism; it replaces raw ANF as the default. ANF-from-table
(A-6.2, `1fbb02a`) stays only as a dumb fallback / cross-check oracle.

Worktree `/tmp/a6-synth` (branch `a6-synth`). Builds on the shared gate-emission core (A-6.0/A-6.1,
being bug-fixed per the Fable review).

## Why BDD (vs ANF) — the whack-a-mole test
- **ANF** (truth-table → Reed-Muller): discards the rule's structure, re-expands to sum-of-products.
  gcd2 → degree-7 → **15,148 instrs**. General but efficiency-dead. (Not a per-prim hack, but a
  structure-destroying representation.)
- **BDD** (ROBDD): canonical, **shares subexpressions**, size = #nodes (small for structured
  functions). General (any BvExpr/table → BDD automatically) AND compact. Reuses `bveq`'s existing
  biodivine ROBDD (`expr_to_bdd`, `BddVariableSet`) — same representation `equiv` already trusts.
  This is the one true prim→gates mechanism: no per-prim gadget design, no degree blowup.

## Mechanism (standard reversible BDD synthesis, adapted to the TISC ISA)
Input: a prim's forward `RegTransfer` (per-(reg,bit) `BvExpr`) + backward `RegTransfer` (its inverse).
1. **Build the ROBDD.** For each OUTPUT bit, `expr_to_bdd(spec.regs[r][b], map)` → biodivine `Bdd`
   over the input-bit variables. Use a SHARED multi-output BDD (nodes shared across output bits) —
   biodivine's `BddVariableSet` gives a common variable universe; shared reduction is automatic.
   Choose a variable order (input bits); for small prims any order works, add a heuristic later.
2. **Transcribe bottom-up (Shannon → Mux → gates).** Each non-terminal ROBDD node on variable `x`
   is `node = ite(x, hi, lo) = lo ⊕ (x ∧ (hi ⊕ lo))`. Allocate an ancilla LINE (a scratch bit) per
   node; compute `node_line` from its children lines + the input-var line via that mux identity
   (`Xor` to copy `lo`, `Ccnot`/`Xor` for the `x∧(hi⊕lo)` term). Terminal nodes = constants.
   Output bits READ the root lines (copy root → output via `Ccnot`/`Xor`).
3. **Bennett uncompute.** After outputs are copied out, uncompute every intermediate node line in
   reverse (net-zero ancilla). This is the SAME frame discipline A-6.1 uses; body ≈ 2×(node work).
   For the full permutation-in-place, compose with the A-6.1 Bennett wrapper (compute fwd into
   ancilla; swap; uncompute via bwd) — OR, since BDD synthesis already produces the output-into-
   ancilla form, wire it directly into the wrapper's step-1/step-3 slots (fwd BDD, bwd BDD).
4. **ISA / bit-alignment.** ROBDD is bit-level; the ISA's `Ccnot`/`Cswap` are byte-parallel over
   bit positions — align input-var/node bits to a common position via rotation (as A-6.2's general
   emitter does), reusing that machinery. Byte-parallelism is not free here (gcd2 is bit-structured)
   but BDD keeps the NODE COUNT low, which is what dominates.

## Ancilla budget
One line per simultaneously-live BDD node (max antichain / cut width), plus the output ancilla for
the Bennett wrapper. Free operand-addressable words: r4–r7 (if not live loop machinery) + s2–s7 =
up to 10, minus the 4 output ancilla → ~6 working lines. If a prim's BDD cut width exceeds the
budget, that's a real finding (report; it sizes widen-STATE, the A-6.2 note that degree-7 needed 11
operands). Reuse freed lines (a node line can be recycled once all its parents are computed).

## Slices (each: symbolic/exec verify → Fable review → authority → merge)
- **D.0** — BDD access layer: build a shared multi-output ROBDD from a `RegTransfer` via `expr_to_bdd`;
  expose node-walk (low/high/var, topological order) over biodivine `Bdd`. Unit-test on small specs.
- **D.1** — the BDD→gates transcriber: Shannon-node → gates with per-node ancilla + Bennett
  uncompute + bit-alignment (reuse A-6.2's aligner). Verify each via `symbolic_exec`+`equiv`
  (register-ancilla, ≤8 regs = complete) and net-zero exec. Compare emitted length to node count.
- **D.2** — gcd2 through BDD synthesis: `table_to_regtransfer(gcd2)` (or a structured spec) → BDD →
  gates → **exec-verify 256/256 + MEASURE body.len()** (expect ≪ 15,148 — this is the efficiency
  proof). Report the number; it re-sizes the A-6.4 budget (likely modest widen, not B=75,794).
- **D.3** — integration: `recognize_prim` routes a symbolic/table prim to BDD synthesis as the
  DEFAULT general path; retire the vacuity guard; ANF kept as fallback + differential cross-check
  (BDD body ≡ ANF body under `equiv`). No regression to existing table prims.
Then A-6.4 (widen-PC/state sized to the BDD body) → A-6.5 (GCD runs end-to-end, run_on_cpu==eval).

## Verification (hardened, per the review)
Every synthesized body: (1) `encode()` every instr (catch ISA-illegal gates); (2) register-ancilla
symbolic `equiv` when carrier+ancilla ≤ 8 regs (complete correctness + `bwd=fwd⁻¹` + net-zero);
(3) whole-domain exec for small-domain prims (gcd2: 256). Scratch-ancilla version = the register-
verified body under operand renaming (assert equivalence).

## Dependency to confirm at D.0
biodivine `Bdd` node-walk API (iterate nodes with (var, low, high); terminals). If biodivine doesn't
expose a convenient walk, use `Bdd::to_boolean_expression` or its node vector — verify at D.0 start.

## Process guardrails (post-2×-OOM)
Deterministic transcription — delegable to Opus-5 safely. NO search/Python/beam/sub-agents. If a
delegate spawns search procs, STOP it. Commit-only; never push.
