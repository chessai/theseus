# ADR-0008: Differential testing as the verification strategy

**Status:** Accepted (elaborated by ADR-0018)

## Context
ADR-0001 traded away native cosimulation; correctness must come from
somewhere with teeth.

## Decision
Every pipeline artifact is executable; adjacent stages are differentially
tested on shared inputs (interpreter ≡ typed-sim ≡ bit-sim ≡ Verilator),
alongside the adjoint round-trip property `f† ∘ f ≡ id` on domains.

## Consequences
Miscompiles localize to a single pass by construction. All IRs must be
serializable and pretty-printable from day one. This decision de-risks
most of the others.
