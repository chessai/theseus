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

## Amendment (2026-07-09): the "bidirectional variant" decomposes
The Consequences called the bidirectional check "no off-the-shelf treatment
… possibly a contribution." On working it through for the plain fragment, the
risk is lower than framed: the check **decomposes** into standard pieces
rather than requiring a novel unified matrix. It is (1) a syntactic linearity
pass per clause (`vars(lᵢ) = vars(rᵢ)`, each once — `T0201`/`T0202`) plus
(2) ordinary Maranget usefulness/exhaustiveness run **independently on each
column**, LHS over `A` and RHS over `B` (`T0301`/`T0302`), staged after names
and typing (`spec/static-semantics.md`, global gating). What had no citation
was *unifying* the two sides into one pass; we do not need that. The
sufficiency of "both columns partition + per-clause linear variable
correspondence ⇒ total bijection" is now **proved** for the plain fragment as
TH-4a (`theory/coverage-bijectivity.md`) — so "coverage *is* the
invertibility" (Context) is a theorem, not an assertion. The padded-encoding
half (mux/demux invertibility, ADR-0012) is split off as TH-4b and remains a
downstream M2 obligation. Label-extended coverage (`T0303`) is deferred to the
second M1 slice.
