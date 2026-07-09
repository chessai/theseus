# ADR-0014: Structural plumbing canonicalizes to Permute

**Status:** Accepted

## Context
Should Reshape be ~10 enumerated micro-nodes or one general node? The
deciding fact: **distrib is the only structural combinator that
duplicates a type-tree leaf** ({a,b,c} → {a,b,a,c}); every other
structural iso is a bijection on leaves. Mac Lane coherence then says a
leaf permutation *is* the unique canonical form of any structural
morphism between the same objects — coalescing becomes permutation
composition (O(n), no case analysis), and structural equality is
decidable by comparing σ. The two monoids must not mix: `Permute(monoid,
σ)` acts on one flattened layer, applied at depth through Par/Branch.

## Decision
Core keeps the enumerated Π combinators (the calculus, the literature).
The core→IR canonicalization pass flattens monoid layers, absorbs units
(`1` under ×, `0` under + vanish), and fuses all plumbing into
`Permute(monoid, σ)`. The netlist IR never contains a raw swap or assoc.
`Distrib`/`Factor` remain distinct nodes.

## Consequences
`Permute(×,σ)` lowers to pure wiring, zero gates, both directions;
`Permute(+,σ)` to a tag remap. Structural lemmas per backend: three,
parameterized. "Structural plumbing is free in hardware" becomes a
theorem about the IR. **Documented assumption:** canonicity-by-coherence
holds for the rig fragment; fractional/negative-type extensions have
subtler coherence and require re-examination (theory ledger TH-3).
