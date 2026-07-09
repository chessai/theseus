# theseus — pre-implementation documentation audit (2026-07-09)

Five independent reviewers audited the doc corpus (ADRs 0001–0021, ADRs 0022–0032 +
physics design, living design doc, language spec + conformance fixtures, theory notes +
roadmap) against each other and against the conformance suite. This is the cross-cut
synthesis. Verdict: **the design is unusually disciplined for pre-code, but the language
track (M1) has hard blocking gaps — the parser, coverage checker, elaborator, and
interpreter cannot be built from the spec as written.** Close the Wave-0 items below
before delegating M1 code.

## Severity legend
- **BLOCKER** — an implementer cannot write the component from the docs; must fix first.
- **CORRECTNESS** — a claim stated as settled that is actually unproven/contradicted.
- **RISK** — plausible failure mode not acknowledged; fix before the relevant milestone.

---

## Cross-validated findings (flagged by ≥2 reviewers — highest confidence)

### X1. TH-1 (finite-trace totality) is over-claimed, and the written proof is wrong. [CORRECTNESS]
The interpreter's termination guarantee and the hardware WCET both rest on TH-1
(`≤ |u|`-bounded trace iteration). The obligation ledger honestly marks it **sketched**,
but **ADR-0009 states it as settled fact** ("the minimal core is unconditionally correct")
and ADR-0007 leans on it with no hedge. Worse, the argument in
`theory/finite-trace-totality.md` conflates injectivity of `f` on the whole sum `u+a`
with injectivity on the `u` summand alone ("a cycle under an injective map admits no entry
point" is a non-sequitur — an injective endofunction on a finite set is all cycles). The
correct argument (two pre-images collide under `f`'s injectivity on the sum) is not the one
written. There is also an unresolved **off-by-one** (`≤|u|` vs `|u|+1` body applications)
that sizes a real interpreter constant and the `⌈log(|u|+1)⌉` counter width.
*Reviewers: ADR-1, physics, theory.* → Rewrite the proof; resolve the bound; hedge or
delete "unconditionally correct" in ADR-0009.

### X2. The coverage checker — the named #1 risk — is the least specified thing in the corpus. [BLOCKER]
ADR-0004's entire algorithm is "Maranget-style, applied bidirectionally, with linear
pattern variables," and the ADR itself admits the bidirectional variant "has no
off-the-shelf treatment we know of." Running Maranget per-side checks each side is a
partition — **necessary but not sufficient for bijectivity**; the per-clause variable
correspondence and LHS↔RHS cell pairing that actually determine the permutation are never
specified. The soundness claim (TH-4, coverage ⇔ invertibility) is stated as a settled
identity in ADR-0004 but the ledger marks it **sketched**. `spec/static-semantics.md` does
not contain the missing algorithm. *Reviewers: ADR-1, spec, theory.* → Write the
bidirectional coverage/bijectivity judgment (matrix construction + variable correspondence
+ empty-type/`0` handling + T0301-vs-T0303 assignment) in `spec/static-semantics.md`;
add "(TH-4, sketched)" to ADR-0004.

### X3. `trace` / label elaboration + operational semantics are unspecified — this is P0011, the flagship M1 demo. [BLOCKER]
`core-pi.md` types `trace : (u+a ↔ u+b) → (a ↔ b)` but gives no reduction rule; the spec
defers the whole label→`trace` construct to ADR-0009 (a design doc, not normative). There
is no rule for how a `k`-label set becomes `u`, no label-type inference algorithm (referenced
in ADR-0004/0017/0009, defined nowhere), and the **fuel accounting is internally
inconsistent**: P0011 is variously "4-step", "Done at fuel ≥ 5", and "bound = 8" with no
reconciling definition. *Reviewers: design, spec, ADR-1.* → Add the `trace` static +
small-step rules and label-elaboration algorithm to `spec/dynamic-semantics.md` /
`core-pi.md`; pin fuel = clause-applications with P0011 worked end-to-end.

### X4. The per-constructor adjoint table is missing. [BLOCKER]
`core-pi.md` asserts "adjoint is a syntactic involution" and gives exactly one equation
(`factor = sym distrib`). The full table (`(f;g)† = g†;f†`, `(trace f)† = trace(f†)`,
`distrib† = factor`, unit/assoc/swap duals) is nowhere. `†` cannot be implemented, and the
adjoint-commutation law `P(f†) ≡ P(f)†` — "the single cheapest high-yield property" gating
**every pass test** — cannot be tested. *Reviewers: design.* → Add the ~12-line table to
`core-pi.md`. Also: ADR-0013 prim admission checks "forward is bijective" but must check
`backward ∘ forward = id ∧ forward ∘ backward = id` (bijective ≠ mutual-inverse).

### X5. The grammar cannot parse its own conformance fixtures. [BLOCKER]
`spec/grammar.md` has **no `ty` production at all** (tuple types `(Bit,Bit,Bit)` used by
0004/0006/0015), no `primdecl` (used by 0012/0009), no `where` body (used by 0013), no
string-literal or numeric-literal lexing (used by 0012/0014), and `pat` vs `pattern` is
never reconciled. `test-spec-m1.md` requires every `.thse` to parse, so these are hard
contradictions. *Reviewers: spec.* → Add `ty`, `primdecl`, `where`, `pat`, and literal
lexing; state n-tuple nesting convention (drives coverage enumeration + canonical leaf order).

### X6. Canonicalizer completeness is over-claimed. [CORRECTNESS]
`test-spec-m1.md` calls the canonicalizer "the equality decision procedure," but by
ADR-0014 + `theory/coherence.md` uniqueness holds **only for the structural/`Permute`
fragment** — `Distrib`/`Factor` are explicitly excluded (Laplaza coherence declined). The
design chapters never state this boundary, so an implementer will write a completeness test
that is wrong on distrib-bearing terms. The "exhaustively checkable per type" substitute for
a proof is also unbounded (`n!` permutations; no stated cap). *Reviewers: design, theory.*
→ Scope the "decision procedure" claim to structural terms in `netlist-ir.md`/`encodings.md`;
state the exhaustiveness bound.

### X7. The money-plot slope is quoted on two different axes across the top-level docs. [RISK]
Not a physics error (`P = E/T` ⇒ power slope = energy slope − 1), but README uses **−2**
(power), roadmap uses **−1** (energy), ADR-0025's harness gate is on −2, ADR-0028's budget
on −1 — only ADR-0023 states both together. Additionally the "slope −1 across ≥2 decades"
M3a acceptance criterion is bounded on both sides (RC saturation at fast ramps, leakage
crossover at slow ramps) and is **asserted, not derived** — whether 2 decades even exist in
Sky130 is an empirical fact the gate presupposes. *Reviewers: physics, theory.* → Name the
axis on every slope; state the regime window + a contingency ("widest window ≥ X decades").

---

## Single-reviewer findings worth acting on

### Language track
- **No ADR defines the M1/M2/M3 milestone boundaries.** ADRs speak in v0.1/v0.2 and
  L1/L2/L4; the prompt speaks in M1/M2/M3; no mapping exists. This forces several
  severities to stay conditional ("blocks *if* in M1"). → Add the mapping to `roadmap.md`;
  decide explicitly whether labels, canonicalization/`Permute`, and lowering are in M1.
  (ADR-1, E1)
- **`.diag` fixtures use `variable:` / `name:` keys the format spec never defines** (5/10
  negative fixtures). Harness comparison of unknown keys is undefined. → Extend the `.diag`
  format in `test-spec-m1.md`. (spec, B1)
- **`:step`/`:unstep` granularity on composites (`;`, `*`, `+`, `~`) is undefined**, yet the
  inverse relation is the one normative REPL contract. Half the positive suite is composite.
  → Give small-step rules for the combinators. (spec, D2/D3)
- **P0012 has no `.expect`; canonicalizer examples 2–3 have no fixtures at all.** The
  `Permute`-normal-form claims are graded by nothing. → Define a typecheck-only fixture
  class + a canonical-form assertion channel. (spec, F1/F2)
- **TBD-12 (mutation-score threshold) is an M1 merge gate owed "when the first M1 module
  ships" — circular.** → Give it a provisional value now. (design, G1)
- **Risk register omits the two largest language-track risks** (`trace` elaboration,
  scheduler) — 8 of 12 rows are physics. → Add them. (design, F2)
- **Interpreter value representation is unspecified** (the one M1 component with zero
  dedicated coverage). → One note pinning the typed-value domain + per-constructor eval.
  (ADR-1, E7)

### Physics track
- **CONTRADICTION: post-flatten balancing.** ADR-0026 says it is "mandatory for
  correctness"; ADR-0015 + `scheduling.md` say "QoR only, never correctness." ADR-0026
  calls this a "refinement" but it *inverts* the invariant. → Stop calling it a refinement;
  add a distinct INV-TICK invariant. (physics, Finding 1) — M3b.
- **Energy accuracy budget (±15%) has no composition rule** — independent per-cell errors
  don't stay ±15% summed over thousands of cells; TH-E3 (fit adequacy) has **no numeric
  bound anywhere**, including the ledger. → Add an error-propagation subsection + a concrete
  TH-E3 threshold. (physics, theory) — M3b/M4.
- **"Compiled reversible full adder" is named as an M3a DUT but the compiler (dual-rail
  lowering) is M3b** — either M3a secretly depends on M3b or the adder is hand-built
  (quietly violating ADR-0022's CPU-as-compiled-program principle). Also the adder itself is
  under-specified (width? structure? garbage-free?). → Reconcile roadmap vs test-spec-m3a;
  pin the adder. (physics, Findings 4/9)
- **TH-6 (physical adjointness "same silicon, zero added logic") is over-claimed for general
  logic** — transistor bidirectionality ≠ electrical reversibility of a CSWAP network under
  reversed clocking. Correctly marked empirical, but README + publication map pre-announce it
  as a result, and the M4 "run forward and backward" DoD may silently be delivered by the
  mirrored-netlist fallback. → Soften framing; flag the fallback in the M4 DoD. (physics,
  theory) — M4.
- **M3a bench matrix is ~810 SPICE sweeps per parametric cell** (3 Vt × 2 body × ramps ×
  loads) with no screening strategy. → Add a screen-then-downselect note. (physics,
  Finding 11) — M3a.

### Structural / honesty
- **The ledger is the most honest document; the dishonesty is in the *gap* between the
  ledger and the ADRs** — ADR-0009 (TH-1) and ADR-0004 (TH-4) state as fact what the ledger
  marks sketched. Every over-claim above is a ledger/ADR mismatch, not a ledger error.
- **"Two tracks share zero code until M3b" is true at the crate level but they are bound by
  a frozen shared interface** (node vocabulary ADR-0013, dual-rail encoding ADR-0011, DB
  energy-model schema). The parallel-staffing decision is only safe if that interface is
  frozen before M1 and M3a start. → Reword; make freezing the interface an explicit
  pre-M1/M3a gate. (theory, F1)

---

## Recommended Wave-0 (doc-hardening) work-list — each is one delegation

Ordered by how hard it blocks M1. Items 1–5 are BLOCKERS; do them before any M1 code.

1. **Grammar completion** (X5) — `spec/grammar.md`: `ty`, `primdecl`, `where`, `pat`,
   literal lexing, n-tuple nesting. Self-check: every `conformance/**/*.thse` parses.
2. **Bidirectional coverage judgment** (X2) — `spec/static-semantics.md`: the algorithm +
   variable correspondence + empty-type + T0301/T0303 assignment + linearity/wildcard rule;
   ADR-0004 TH-4 hedge.
3. **Elaboration map + `trace`/label operational semantics + fuel** (X3) —
   `spec/dynamic-semantics.md` + `core-pi.md`: clauses→core, label→`trace`, label-type
   inference, fuel accounting worked on P0011; `:step`/`:unstep` combinator rules.
4. **Core Π completion** (X4) — `core-pi.md`: per-constructor adjoint table, combinator
   typings, interpreter value domain; ADR-0013 prim mutual-inverse admission.
5. **Milestone map + interface freeze** (M1/M2/M3 ↔ v0.1 ↔ L-layers; node-vocab + dual-rail
   + DB-schema freeze) — `roadmap.md`. Decides which later items are in M1.
6. **TH-1 proof rewrite + off-by-one** (X1) — `theory/finite-trace-totality.md`; hedge
   ADR-0009. (Non-blocking for a fuel-capped interpreter, but fix before WCET is claimed.)
7. **Canonicalizer completeness scoping** (X6) — `netlist-ir.md`/`encodings.md`. (M2.)
8. **Conformance harness format** — `.diag` extra keys, typecheck-only fixtures,
   canonical-form assertion channel, TBD-12 provisional value.
9. **Physics reconciliations** — balancing-correctness contradiction (INV-TICK), money-plot
   axis + regime window, compiled-adder vs M3b, TH-E3 number, TH-6 framing. (M3a/M3b — can
   run parallel to the language track.)
</content>
