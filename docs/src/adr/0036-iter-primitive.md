# ADR-0036: `iter` -- the first bounded-iteration core primitive

**Status:** Accepted

## Context
ADR-0009 ratified the stratified loop story: v0.2 adds `iter` and
`fold`/`unfold` as core *primitives* (adjoint pairs; core stays closed
under `sym`), because elaborating a provably-total loop to `trace` over an
infinite carrier erases its totality evidence. ADR-0035 landed μ-types by
bounded unrolling, leaving programs depth-*bounded*: there was no way to
write a depth-*generic* iso (a true n-fold application). This ADR lands the
first primitive, `iter`, through the interpreter (software dialect,
ADR-0006); the netlist lowering (bounded-counter loop node, ADR-0013) is a
separate later increment.

## Decision
Add `Iso::Iter(n, f)` to Core Π with the **iteration count carried in the
data** (ADR-0007's "the count lives in the data, symmetric by
construction").

- **Counter types.** `n` must be a *unary numeral type*
  `1 + (1 + (... + 0))` -- exactly the shape bounded μ-unrolling produces
  for `Nat` (ADR-0035), with numeral `k` encoded `inrᵏ(inl ())`.
  `Type::numeral_ty(k)` builds it; `is_numeral_ty` checks it
  (`T`-side error `IterCounterNotNumeral` otherwise).
- **Typing.** `f : a ↔ a` (endo; else `IterBodyNotEndo`) gives
  `iter n f : n × a ↔ n × a`.
- **Denotation.** `⟦iter n f⟧ ⟨k, x⟩ = ⟨k, ⟦f⟧ᵏ x⟩`. The count `k` flows
  through unchanged; only the payload is transformed. This is RPP-style
  iteration, and it is a bijection whenever `⟦f⟧` is: each fiber
  `{k} × a` is mapped to itself by the bijection `⟦f⟧ᵏ`.
- **Adjoint.** `(iter n f)† = iter n (f†)`. Because `k` is preserved on
  output, the backward run knows *from the data* to un-apply the body
  exactly `k` times: `(f†)ᵏ ∘ fᵏ = id` pointwise. No search, no stored
  trajectory -- symmetry is by construction, and the core stays closed
  under `sym`.
- **Totality (ADR-0007).** Evaluation is structural induction on the
  numeral: a value of the finite type `n` encodes `k ≤ |n| − 1`, so the
  body runs at most `|n| − 1` times *in either direction* -- the
  bidirectional bound is read off the type, no certificate plumbing.
  Fuel: like `trace`, one unit per body application;
  `static_bound(iter n f) = (|n| − 1) · (1 + B(f))`.

## Consequences
Depth-generic n-fold application is expressible today in the software
dialect and oracle-verified (`eval(sym f, eval(f, x)) == x` and
`iterᵏ = k`-fold composition, swept over whole domains -- theseus-eval
`round_trip_iter_*`, `iter_equals_nfold_body_composition`).
`theseus-netlist::canonicalize` rejects `Iter` explicitly
(`unimplemented!`) until the counter-loop node lands. Surface syntax is
deferred: the frozen expression grammar (ADR-0017) has no application
form, so `iter` in source needs a grammar extension -- bundled with the
`fold`/`unfold` increment, where the depth-annotation syntax lands too.

## Alternatives rejected
Counter in the *type* (an indexed `iterᵏ` family): loses depth-genericity
-- every count is a different term, which is just unrolling again.
Elaborating to `trace` over `n × a`: erases the structural bound, the
exact failure ADR-0009 stratified the core to avoid. Arbitrary finite
counter types with an enumeration-order reading: no canonical successor,
and the μ-`Nat` numeral shape is already the pipeline's count encoding.
