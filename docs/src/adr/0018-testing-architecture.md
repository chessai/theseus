# ADR-0018: Testing architecture

**Status:** Accepted

## Context
Testing is a first-class design member: nothing ships without it. Unusual
position of strength: finite types make **exhaustive** checking real, and
totality means no test needs a timeout.

## Decision
**Four layers.** T1 universal properties (adjoint commutation, round-trip,
serialization, determinism — stated once, instantiated per artifact).
T2 oracle chain (per-boundary differentials; a pass without a link does
not merge; exhaustive mode kicks in automatically by type size).
T3 per-component obligations keyed to the risk register — notably:
coverage checker differentially tested against the brute-force semantic
definition plus an adversarial corpus; canonicalizer soundness,
idempotence, and *completeness* (Mac Lane coherence, executable);
lowering lemmas as tests (exhaustive fast tier, SAT nightly); the
simulators are test infrastructure and get their own obligations.
T4 adversarial/meta: fuzzing, a permanent regression corpus, and
**mutation testing as the metric of the suite itself**.

**Generators are the keystone:** typed values (random + exhaustive,
type-aware shrinking); well-typed isos (bottom-up combinator
composition); and the **bijection-table trick** — random type, random
permutation of its values, emit the fully-literal clause set, run the
whole pipeline to Verilator against the table as ground truth that owes
nothing to any compiler stage. Generator distributions are themselves
tested.

**Process:** definition-of-done checklist (CONTRIBUTING). Two-tier CI:
fast per-commit, nightly heavy with auto-minimizing failures. **Split:**
implementation tests live with code; the versioned **conformance suite**
(T1/T2 properties + corpus + diagnostics snapshots) is pinned to the
spec and runnable against any future implementation.
