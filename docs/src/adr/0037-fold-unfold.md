# ADR-0037: `fold`/`unfold` — structural-recursion core primitives

**Status:** Accepted

## Context
ADR-0009 ratified that v0.2's total loop combinators ship as core
*primitives* in adjoint pairs (the core stays closed under `sym`),
because elaborating a provably-total structural recursion into `trace`
over an infinite carrier erases its totality evidence — undecidable to
recover, so it must stay structural, never certificate-based. ADR-0036
landed the first primitive, `iter` — *count*-driven: its body is applied
`k` times but can never see the structure being consumed, only a bare
counter. What remained was *structure*-driven recursion: an iso whose
body consumes each node's payload of an inductive (μ-) value. This ADR
lands `fold` and its adjoint partner `unfold` through the interpreter
(software dialect, ADR-0006); the netlist lowering (bounded structural
loop node, ADR-0013) is a separate later increment.

## Decision
Add `Iso::Fold(l, f)` and `Iso::Unfold(l, f)` to Core Π as a
**mutually adjoint pair**, with the μ-value carried in the data
(ADR-0007's "symmetric by construction" discipline, exactly as `iter`).

- **Spine types.** `l` must be a *list-spine type*
  `Lᵈ(b) = 1 + (b × Lᵈ⁻¹(b))`, `L⁰ = 0` — exactly the shape ADR-0035
  bounded unrolling produces for `List b = Nil | Cons b List`, with
  values encoded `nil = inl ()`, `cons h t = inr ⟨h, t⟩`. The element
  type `b` must be uniform along the spine.
  `Type::list_spine_ty(b, d)` builds it; `list_spine_elem_ty` checks it
  and extracts `b` (`T`-side error `FoldSpineNotList` otherwise; the
  degenerate spine `0` is admitted with `b` unconstrained).
- **Typing.** `f : b × c ↔ b × c` (endo, product-shaped, element
  component `b` matching the spine; errors `FoldBodyNotEndo`,
  `FoldBodyNotElemProd`, `FoldBodyElemMismatch`) gives
  `fold l f : l × c ↔ l × c` and identically `unfold l f : l × c ↔ l × c`.
- **Denotation** (a reversible `mapAccumL` and its mirror):
  - `⟦fold l f⟧ ⟨[x₁ … xₖ], c₀⟩ = ⟨[y₁ … yₖ], cₖ⟩` where
    `⟨yᵢ, cᵢ⟩ = ⟦f⟧ ⟨xᵢ, cᵢ₋₁⟩` — the accumulator is threaded
    **head-to-tail** through every node, consuming each node's payload
    into it while the (possibly rewritten) spine is preserved in the
    output.
  - `⟦unfold l f⟧ ⟨[x₁ … xₖ], dₖ⟩ = ⟨[y₁ … yₖ], d₀⟩` where
    `⟨yᵢ, dᵢ₋₁⟩ = ⟦f⟧ ⟨xᵢ, dᵢ⟩` — the **tail-to-head** dual, which
    *produces* the accumulator trajectory `fold` consumed.

  Each is a bijection whenever `⟦f⟧` is: the spine length `k` is
  preserved, and on each fixed spine the map is a triangular composition
  of bijections.
- **Adjoints.** `(fold l f)† = unfold l (f†)` and
  `(unfold l f)† = fold l (f†)`. Because the spine survives in the
  output, the backward run knows *from the data* to un-thread the
  accumulator through exactly the same `k` nodes in the opposite order:
  `unfold(f†) ∘ fold(f) = id` and `fold(f†) ∘ unfold(f) = id`
  pointwise. No search, no stored trajectory; `adjoint` remains a
  syntactic involution and the core stays closed under `sym` (ADR-0009's
  "adjoint pairs" requirement).
- **Totality (ADR-0007).** Evaluation is structural induction on the
  finite spine *value*: a value of `Lᵈ(b)` is a list of length
  `k ≤ d − 1`, so the body runs at most `d − 1` times *in either
  direction* — the bidirectional bound is read off the type (the unroll
  depth), no certificate plumbing. Fuel: consistent with `trace`/`iter`,
  one unit per structural step (per node consumed);
  `static_bound(fold l f) = static_bound(unfold l f) = (d − 1) · (1 + B(f))`.

## Why `fold`/`unfold` in addition to `iter`
`iter` is count-driven: `⟦iter n f⟧⟨k, x⟩ = ⟨k, fᵏ(x)⟩` applies a fixed
endo-body a number of times read off a bare numeral; the body sees only
the payload `x`, never the structure. `fold` is structure-driven: the
body visits every node of an inductive value and consumes that node's
payload (`b`) into the accumulator — sum-, parity-, and length-style
computations over `List b` that `iter` cannot express (a numeral has no
per-node payload to consume). Both exist as primitives for ADR-0009's
reason: each carries its totality evidence *structurally* (numeral
shape for `iter`, finite spine for `fold`/`unfold`), and neither can be
elaborated to `trace` without erasing that evidence. `Nat`-spines need
no separate fold: `Nat ≅ List 1` and a payload-free structural fold is
exactly `iter` (see Alternatives).

## Consequences
Structure-driven recursion is expressible today in the software dialect
and oracle-verified over whole domains (theseus-eval:
`round_trip_fold_parity_cx`, `round_trip_unfold_parity_cx`,
`round_trip_fold_nonselfinverse_body`,
`fold_then_unfold_of_adjoint_is_identity`,
`unfold_then_fold_of_adjoint_is_identity`,
`sym_fold_equals_unfold_of_adjoint_body`,
`fold_equals_manual_left_accumulation`,
`unfold_equals_manual_right_accumulation`,
`fold_length_mod3_into_accumulator`).
`theseus-netlist::canonicalize` rejects `Fold`/`Unfold` explicitly
(`unimplemented!`) until the structural-loop node lands. Surface syntax
remains deferred: the frozen expression grammar (ADR-0017) has no
application form, so `fold`/`unfold` in source need the same grammar
extension as `iter` — bundled with the depth-annotation syntax
increment.

## Alternatives rejected
**Constructor isos only** (`fold : F(μX.F X) ↔ μX.F X` à la classical
Π with recursive types): under ADR-0035's bounded unrolling the μ type
*is* its unrolling, so the constructor iso degenerates to the identity —
no recursion is expressed. **Numeral spines admitted too**: a
payload-free fold over `Nat` adds no expressiveness over `iter`
(`Nat ≅ List 1`) but doubles the admissible-spine logic; rejected to
keep one canonical primitive per job. **A single `Fold` variant with
`sym` as the only inverse form**: loses the first-class tail-to-head
direction and makes `adjoint` produce non-canonical `Sym` wrappers,
breaking the leaf-adjoint normal form the involution maintains.
**Accumulator-only output** (`l × c ↔ c'`): not injective — folds that
forget the structure (e.g. summing) are not bijections; keeping the
spine in the data is what makes the pair total and symmetric by
construction.
