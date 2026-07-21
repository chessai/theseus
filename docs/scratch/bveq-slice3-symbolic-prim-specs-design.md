# BVEQ slice-3 — symbolic prim specs (break Wall 2: wide-op recognition without a table)

**Status:** design r1 (pre-review). Author: advisor. Target: `PrimDef` in **theseus-core** + eval/admit/
the recognizer — the MOST INVASIVE BVEQ slice (it changes the prim representation the whole compiler
depends on). Highest bar. Builds on slice-1/2b/2 (all merged): `bveq::equiv`, `symbolic_exec`,
`bveq_verify_emission` (the decomposed check now live in `recognize_prim`).

## 1. The wall this breaks

`PrimDef.forward: BTreeMap<Value,Value>` is an explicit truth TABLE — exponential to construct. A
3-operand `Flat(8)` prim (16.7M entries) is UNBUILDABLE (Wall 2). So byte-wide 3-operand ops (a full
`Ccnot` masked-accumulate at `Flat(8)`, GF(2) dot products at full width, wider crypto — the TP-5
direction) cannot even be REPRESENTED, let alone recognized — regardless of slice-1/2's BDD machinery,
because the prim itself can't be built. Slice-3 lets a `PrimDef` carry a **symbolic spec** (a `BvExpr`
register-transfer) instead of / alongside a table, so a wide prim needs no table.

## 2. Mechanism (framing — the review's spike decides the exact shape)

- **theseus-core `PrimDef`:** add an OPTIONAL symbolic spec — e.g. `symbolic: Option<PrimSpec>` where
  `PrimSpec` is a bit-level transfer over the prim's leaves (reuse `bveq::BvExpr` if the layering allows,
  or a theseus-core-native mirror). Existing table prims keep `forward`/`backward` UNCHANGED (additive).
- **eval (theseus-eval):** when a prim has a symbolic spec and no/partial table, `eval` evaluates the
  spec at the input `Value` (bit-decompose → eval the BvExpr → recompose) instead of a table lookup.
  This is the load-bearing change — eval is the semantic ground truth.
- **admit (theseus-check):** a symbolic-spec prim's bijectivity/totality is checked SYMBOLICALLY (the
  spec is a bijection iff its BDD is a permutation — or via the existing admit over a bounded sample +
  the symbolic argument), not by materializing the inverse table.
- **the recognizer (`bveq_verify_emission`, slice-2):** when the prim has a symbolic spec, compare
  `symbolic_exec(body)` BDD against the **spec's BDD** directly (`bveq::equiv` — canonical, complete at
  full `Flat(8)`, no table enumeration) → THIS is the Wall-2 break: 3-op `Flat(8)` recognized in µs.
  When the prim has only a table (the status quo), keep the slice-2 decomposed pointwise check.

## 3. Why this is the right capstone

Slices 1/2b/2 built the substrate (a netlist-reconciled symbolic instruction model + a live
emission-verifier) but left the PRIM stuck as a table. Slice-3 makes the prim symbolic too, so the whole
recognition path is table-free for wide ops — the enumeration wall is gone for both the check (slice-1)
AND the construction (this slice). It also folds the slice-2 non-blocking follow-ups (the pub-API panic,
the table-totality doc obligation — which becomes "spec-is-a-bijection" here).

## 4. Open questions for the design-review (BUILD a spike + SCOUT the blast radius)

- **Q1 (BLAST RADIUS — the gating question):** SCOUT every use of `PrimDef.forward`/`backward` across the
  workspace (eval, admit, the recognizer, `enumerate_values`, marshal, energy, tests). How many touchpoints
  MUST handle a table-absent (symbolic-only) prim? Is the additive `Option<symbolic>` framing tractable, or
  does table-absence ripple too far (does anything ASSUME `forward` is always populated)? This determines
  whether slice-3 is one slice or a cluster.
- **Q2:** the layering — can theseus-core `PrimDef` depend on `bveq::BvExpr` (bveq is in theseus-tisc,
  which depends on theseus-core → a CYCLE)? Likely need a theseus-core-native bit-transfer type (or put the
  spec in theseus-tisc and have the recognizer supply it). Spike the layering.
- **Q3:** eval of a symbolic-spec prim — bit-decompose the input Value (thermometer `Flat(w)` → bits via
  `bits_of`), eval the transfer, recompose. Does this match a table prim's eval EXACTLY on a buildable
  case (the ground-truth check: for a prim that has BOTH a table and a spec, eval-via-table == eval-via-spec
  over the whole domain)? Spike it on `Ccnot`/`Add` at `Flat(4)`.
- **Q4:** admit of a symbolic-spec prim — how to certify bijectivity without the inverse table? (BDD
  permutation check, or the existing bounded admit + a symbolic argument.) Is `backward` still needed?
- **Q5:** the FIRST wide prim to unlock — a 3-op `Ccnot` masked-accumulate at `Flat(8)` (today capped at
  `Flat(4)`). Spike: can it now be represented (symbolic spec) + recognized (bveq::equiv vs symbolic_exec)
  at full `Flat(8)`? That is the concrete payoff proving the wall is broken.

## 5. Invariants / review targets

- **I1 additive:** existing table prims are UNCHANGED; eval/admit/recognizer behavior on them is
  byte-identical (all goldens green). Symbolic-spec prims are a NEW capability.
- **I2 eval ground truth:** eval-via-spec == eval-via-table on any prim that has both (Q3) — the spec is a
  faithful alternate representation, not a divergent one.
- **I3 reject-don't-corrupt:** a symbolic-spec prim that isn't a bijection is rejected by admit; the
  recognizer still fails closed.
- **I4 the wall-break demonstrated:** a 3-op `Flat(8)` prim represented + recognized (Q5) — the payoff.

## 6. Scope / risk

- MOST INVASIVE BVEQ slice (theseus-core + eval + admit). Do NOT delegate until the design-review's SCOUT
  (Q1) confirms the blast radius is tractable and the layering (Q2) is sound. If Q1 shows table-absence
  ripples too far, RE-SLICE (e.g. keep tables for eval/admit, use the symbolic spec ONLY in the recognizer
  first — a smaller "recognizer-only symbolic spec" slice that breaks the RECOGNITION wall without touching
  eval's representation).
- NOT M-D.e widen-PC (the next queue item after this).

## 7. Success criteria

The blast radius is scoped (Q1); a symbolic-spec prim evals identically to its table twin (I2/Q3); admit
certifies a symbolic-spec bijection (Q4); a 3-op `Flat(8)` prim is represented + recognized at full width
(I4/Q5 — the wall-break); all existing goldens/tests green (I1). If the full slice is too invasive, the
review delivers the re-sliced "recognizer-only" first step.
