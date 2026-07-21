# BVEQ slice-3 — symbolic prim specs (break Wall 2: wide-op recognition without a table)

**Status:** design **r2** (post design-review + spike). Author: advisor. Target: `PrimDef` in
**theseus-core** + eval/admit/recognizer — the MOST INVASIVE BVEQ slice. Highest bar.

---

## r2 revision — folding the design-review + spike (`docs/reviews/bveq-slice3-design-review.md`)

Verdict **RE-SLICE**. The mechanism is SOUND and both load-bearing claims are SPIKE-PROVEN (eval-via-spec
exact over the whole Flat(4) domain; full-Flat(8) 3-op recognition via `equiv` in **610µs**, non-vacuous —
the 2^24 table never built). Spike preserved: `docs/scratch/slice3-spike-review.rs.ref`. But the full slice
is "unsafe as ONE delegation" (3-crate blast radius + a 72-site/27-file struct ripple + 4 under-specified
gaps + 2 enumeration-HANG hazards + a silent-VACUITY vector). **Split into 3a → 3b:**

**Slice 3a — representation + eval + admit** (theseus-core + theseus-eval + tisc guards; NO new recognition):
1. Move `BvExpr`+`eval_expr` to theseus-core (re-export from bveq; +PartialEq/Eq). 2. `PrimSpec` = a
`(forward, backward)` PAIR (adjoint() is total+infallible → needs a constructive inverse) in core;
`PrimDef.symbolic: Option<PrimSpec>`; the 72-site `symbolic: None` ripple (27 files → the allowlist).
3. `adjoint()` swaps specs + involution test. 4. `core::admit` spec-presence early-return (new `AdmitError`
variant) **BEFORE** `enumerate_values` (else 2^24 OOM at wide types); both-present ⇒ verify spec==table over
the table domain (table WINS in eval = I1 zero-behavior-change). 5. `bveq::admit_symbolic` BDD mutual-inverse
(core stays bdd-free). 6. eval-via-spec in `interp.rs` (core-native decompose/recompose — `bits_of` has no
Flat arm) + the I2 whole-domain agreement test at Flat(4). 7. Fail-closed guards + tests: `bveq_verify_
emission` HARD-REJECTS an empty table (silent-vacuity vector — the f3.5 vacuous-oracle class); `recognize_
prim` rejects symbolic-only prims EXPLICITLY before any `enumerate_values` (hang guard).

**Slice 3b — symbolic recognition** (tisc only): the design's §2 recognizer path was silent on SELECTION —
fix = **selection-by-equiv** (iterate the finite arm library, `symbolic_exec` each candidate, `equiv` vs the
spec transfer, first pass wins, ~15×1ms). Spec→RegTransfer shim (shared VarMap, type-derived operand_regs =
N5). Payoff test = compile `Iso::Prim`(Flat(8)³ masked-accum) → `[Ccnot]` + ring-run vs eval-via-spec on a
BOUNDED sample (never 2^24 — gate-no-timeout).

**Delegation hazards:** file_allowlist must include all 27 `PrimDef{` files (near-workspace-wide); forbid
touching `bveq::equiv`/`VarMap` internals (TCB); bound every test domain; the hang-guards + vacuity-guard
ship with fast-explicit-rejection teeth tests.

**This slice is DEFERRED for a go/no-go** — it is the first change touching theseus-core's foundational
`PrimDef` + theseus-eval (semantic ground truth) with a near-workspace-wide allowlist; advanced to
fully-designed + adversarially-reviewed + spike-proven + re-sliced, awaiting greenlight vs. pivoting to
M-D.e widen-PC (the next queue item). The r1 body below is superseded by the 3a/3b plan above.

---

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
