# ADR-0012: Canonicity — conditional and locally re-established

**Status:** Accepted

## Context
Under padded encodings (ADR-0011), reverse-direction merge logic assumes
inactive payload regions are zero; junk there breaks bit-level
invertibility while typed semantics still looks fine. Poking the naive
global invariant ("all sum nets always well-formed") broke it twice:
(1) *temporally* — power-on registers, loop feedback between iterations,
and pipeline bubbles legitimately hold trash; (2) *structurally* — one
global property that every pass must respect is the certificate tax of
ADR-0009 again. Named threats: buggy node lowerings (distrib scatters
padding regions), optimization passes reasoning "don't-care", and
synthesis lawfully exploiting any emitted `x`.

## Decision
The invariant is **`valid ⇒ well-formed`**, attached to (valid, typed
payload) bundles — promoted to the pervasive IR port discipline.
Enforcement is local, not global: (a) per-node lowering lemmas
(well-formed in ⇒ well-formed out ∧ bits match typed semantics),
mechanically checkable per instance — exhaustive at small widths, SAT
above (`--verify-lowering`); (b) defensive masking at every merge point,
in *both* directions (each direction has its own merges; lemmas quantify
over `dir`); (c) emitted HDL contains no don't-cares, ever; (d) boundary
obligation discharged by generated encoders; (e) simulator assertions as
the runtime net.

## Consequences
Junk cannot propagate past one node boundary; whole-circuit canonicity is
an induction over lemmas. Mask redundancy is refunded by synthesis
constant-propagation where provable. Second instance of the project
principle: soundness lives in structure the vocabulary cannot violate.
