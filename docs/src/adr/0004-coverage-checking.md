# ADR-0004: Coverage checking at elaboration

**Status:** Accepted

## Context
An iso clause set is meaningful only if, on *both* sides, patterns are
exhaustive and non-overlapping — this is the invertibility of the
generated mux/demux pairs (ADR-0012), not a style lint.

## Decision
Coverage checking runs at elaboration and failures are hard type errors.
Algorithm: Maranget-style pattern-matrix usefulness checking, applied
bidirectionally, with **linear** pattern variables (each variable exactly
once per side, present on both sides) — linearity is forced by
reversibility itself: duplication is erasure's mirror twin.

## Consequences
The typechecker is substructural. Label state spaces (ADR-0017) extend the
matched type. Errors must carry concrete witnesses (ADR-0020 amended).
This is the project's named highest technical risk: the bidirectional
variant has no off-the-shelf treatment we know of — prototype earliest;
possibly a contribution.

## Alternatives rejected
Warn-and-continue (unsound: unsynthesizable circuits); semantic-only
checking by enumeration (kept, but as the *test oracle* for the real
algorithm, per ADR-0018).
