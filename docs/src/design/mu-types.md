# μ-types (v0.2, increment 1): bounded unrolling

Decision record: ADR-0035. Dialect context: ADR-0006 (software dialect),
ADR-0007 (totality), ADR-0009 (stratified loops — `iter`/`fold`/`unfold`
arrive as core primitives in a *later* increment).

## What landed

A recursive nominal type declaration, e.g.

```text
type Nat = Zero | Succ Nat
type BitList = Nil | Cons (Bit * BitList)
```

now flows through the whole pipeline: parse → names/typing/linearity/
coverage → elaborate → eval, forward and backward. No new surface syntax
was needed (recursive `type` declarations were already grammatical); no
Core Π `Type` or `Iso` variant was added.

## Representation: bounded unroll at lowering

`Env::lower` unrolls the fixpoint to a bounded depth `D` and cuts off
with `0` (the empty type):

```text
Nat @ D=4   ⇒  1 + (1 + (1 + (1 + 0)))      -- numerals 0..=3
BitList @ 4 ⇒  1 + (Bit × (1 + (Bit × …)))  -- lists of length ≤ 3
```

Mechanics (see `crates/theseus-check/src/env.rs`):

- Each named type has a **per-path re-entry budget** of `D` inside one
  `lower` call, spent on entry and restored on exit. Only a type on a
  reference cycle can exhaust it, so non-recursive programs lower exactly
  as before; mutual recursion (`A ↔ B`) terminates because each name has
  its own budget; branching recursion (`Tree * Tree`) is priced per path,
  not globally.
- The `Zero` cutoff means the truncated type's inhabitants are **exactly**
  the μ-type's values with fewer than `D` unfoldings, and each value's
  core encoding is independent of `D` (numeral `n` = `inrⁿ(inl unit)`).
  Truncated domains nest as prefixes as `D` grows, which is why coverage,
  elaboration's match/instantiate oracle, and the CLI surface↔core value
  bridge needed **no changes at all**.

## How `D` is specified

`DEFAULT_UNROLL_DEPTH = 4`, overridable programmatically with
`Env::set_unroll_depth`. A per-type surface annotation (and/or CLI flag)
is deferred to the fold/unfold increment, where depth-generic programs
first become expressible and an annotation has something to say.

## Diagnostics at the depth boundary

No new diagnostic code. A value needing ≥ `D` unfoldings does not inhabit
the truncated domain, so an iso that escapes the depth — e.g. successor,
whose image misses `Zero` and overflows the deepest numeral — fails
ordinary coverage with `T0301` and a concrete witness
(`coverage::tests::mu_depth_escaping_iso_rejected_t0301`). Accepted isos
are exactly the bijections on the truncated domain, so evaluation can
never step outside it and the finite-fragment totality argument
(ADR-0007) applies unchanged.

Known limitation: the CLI value bridge (`theseus-cli/src/value.rs`)
constructs core values from surface text without checking depth, so a
hand-written too-deep input is only caught when a pattern match fails,
not with a dedicated message.

## Verified by

- `theseus-check` unit tests: lowering shape/counts, configurable depth,
  mutual + branching recursion termination, non-recursive invariance
  (`env.rs`); staged-check cleanliness and `T0301` at the boundary
  (`coverage.rs`); whole-domain differential oracle
  `eval(sym f, eval(f, x)) == x` plus bijectivity of the forward image
  (`elaborate.rs`, `mu_*` tests).
- Conformance fixtures `0016-mu-nat` (identity on `Nat`, full truncated
  domain) and `0017-mu-bitlist` (nontrivial head-bit flip on a recursive
  list), graded forward **and** backward by
  `theseus-cli/tests/conformance.rs` and pinned in its must-grade list.

## Out of scope (next increment)

Depth-generic recursion: `iter`, `fold`/`unfold` as core primitives
shipped as adjoint pairs (ADR-0009), surface `fold`/`unfold`/`for`
keywords (already reserved), and the depth-annotation surface syntax.
