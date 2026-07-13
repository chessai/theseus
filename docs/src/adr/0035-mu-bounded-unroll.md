# ADR-0035: μ-types, increment 1 — bounded unrolling at lowering

**Status:** Accepted

## Context
v0.2's software dialect adds recursive (μ-) types (ADR-0006); the ratified
loop story makes `iter`/`fold`/`unfold` core *primitives* in a later
increment (ADR-0009). The first increment needs recursive nominal types to
round-trip the whole pipeline **without** touching the Core Π algebra:
coverage enumerates every inhabitant of a boundary type and would diverge
on a genuine fixpoint.

## Decision
Represent a recursive nominal type by **bounded unrolling at surface→core
lowering**. Core Π (`0 | 1 | + | ×`) and the `Iso` grammar are unchanged.

- `Env::lower` gives every named type a **per-path re-entry budget**
  `D` (`DEFAULT_UNROLL_DEPTH = 4`, overridable via
  `Env::set_unroll_depth`; a surface annotation is deferred to the
  fold/unfold increment). Entering a name spends one unit; the budget is
  restored on exit, so sibling occurrences are unaffected and only a type
  on a reference cycle can ever exhaust it. Termination is structural.
- The cutoff is **`Zero` (empty), not `One`**: `Nat = Zero | Succ Nat`
  at `D = 4` lowers to `1+(1+(1+(1+0)))` — exactly the numerals `0..=3`.
  Every inhabitant of the truncated type is a *real* value of the μ-type,
  encoded independently of `D` (numeral `n` is `inrⁿ(inl unit)` at every
  admitting depth), so coverage witnesses, the CLI value bridge, and
  elaboration's oracle all work unchanged and truncated domains nest as
  prefixes as `D` grows. A `One` cutoff would mint a junk deepest value
  with no constructor reading.
- **Depth exceedance is not a new diagnostic.** Values needing ≥ `D`
  unfoldings simply do not inhabit the truncated domain; an iso that
  escapes the depth (e.g. successor) fails ordinary coverage (`T0301`)
  with a concrete witness. Isos accepted at depth `D` are exactly the
  bijections on the truncated domain.

## Consequences
Recursive types parse, check, elaborate, and evaluate forward/backward
today, oracle-verified over the whole truncated domain (conformance
0016/0017). Depth-*generic* programs (true successor, fold) are
inexpressible until the primitive combinators land — that is the next
increment, and this lowering does not prejudge it: `fold`/`unfold` will
carry their own typing against the nominal type, while bounded unroll
remains the finite-elaboration/coverage backstop.

## Follow-up: use-site depth annotation (T2-H)
The deferred surface annotation has landed. The syntax `T@D` (e.g. `Nat@6`)
on a type name at use site overrides the per-path unroll budget for that
occurrence. The annotation is parsed as a postfix `@` + numeric literal at
the type-atom level, threaded through `lower_with` as a fuel-map override,
and restored after lowering so sibling occurrences are unaffected. An
un-annotated type name continues to use `DEFAULT_UNROLL_DEPTH`. Conformance
program 0021 exercises the annotation end-to-end.

## Alternatives rejected
Core `Mu`/`Fold` variants now (fattens core before ADR-0009's primitives
are designed, and every backend would need cases it cannot lower);
`One`-cutoff unrolling (junk value breaks value↔constructor bijection);
coverage-only symbolic treatment of recursive positions (a second pattern
semantics to keep in sync with elaboration's oracle).
