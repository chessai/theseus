# ADR-0009: Stratified core loop story

**Status:** Accepted

## Context
Should `trace` be the only core loop (total combinators as sugar), or
should total combinators be primitives? The deciding failure mode: a
provably-total fold elaborated into `trace` over an infinite `u` *erases*
its totality evidence -- unrecoverable in general (undecidable), so it
would need certificate plumbing, where every rewrite pass must preserve
certificates and a certificate bug is an invisible soundness bug.

## Decision
Stratified. v0.1 core: Π + `trace` only -- in the finite fragment trace is
already total with a free bound (ADR-0007), so the minimal core is
unconditionally correct. v0.2: `iter` and `fold`/`unfold` join as
**primitives** (shipped as adjoint pairs; core is closed under `sym`);
trace-over-infinite-`u` becomes the marked-partial construct.

**Principle ratified:** properties that are undecidable to recover must be
structural, not derived.

## Consequences
Netlist IR carries two loop nodes -- bounded counter loop (static WCET) and
handshake loop (partial) -- mapping one-to-one from core with no
certificates (ADR-0013). Fold clause sets reuse the ordinary coverage
machinery (ADR-0004).

## Alternatives rejected
Trace-only with certificates (the certificate tax); primitives-only from
v0.1 (fatter core before μ-types justify it).
