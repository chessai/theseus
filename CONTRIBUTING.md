# Contributing

## Definition of done (ADR-0018)

A component or compiler pass ships only when **all** of the following hold:

1. **Invariant ledger** — its required/established invariants are declared as
   data and validated by the driver (ADR-0016).
2. **T1 universal properties** — instantiated for its artifact type:
   adjoint commutation `P(f†) ≡ P(f)†`, round-trip `f† ∘ f ≡ id` on domain,
   serialization round-trip, byte-for-byte determinism.
3. **T2 oracle-chain link** — a differential test connecting its input and
   output artifacts. A pass not covered by a differential link does not merge.
4. **T3 component obligations** — the component-specific contract in the
   design document's verification chapter.
5. **Regression corpus** — every bug found during development becomes a
   permanent corpus entry.
6. **Mutation score** — `cargo-mutants` above the module threshold.

## Test tiers

- **Fast (every commit):** units, small-type exhaustive sweeps, fixed-seed
  properties, snapshot tests. Minutes.
- **Nightly:** large property budgets, fuzzing, SAT lemma checks
  (`--verify-lowering`), Verilator differentials, mutation testing. Hours.
  Nightly failures auto-minimize into the regression corpus.

## Code policies

- **Nightly toolchain, stable-compatible source** (ADR-0019, amended): no
  `#![feature(...)]` without a ratified ADR exception. Dropping to stable
  must always remain a one-line flake change.
- **Determinism at the type level** (ADR-0020): `IndexMap`/`BTreeMap` and
  sorted iteration anywhere ordering can reach an artifact; `FxHashMap` only
  for purely internal lookups.
- Diagnostics are structured values with error codes; never eagerly
  formatted strings (ADR-0020, amended).
- Emitted HDL contains no `x`/don't-cares, ever (ADR-0012).

## Decision process

Changes of direction are new ADRs with `Supersedes:` links — existing ADRs
are never edited except to update `Status`. Every ADR must be cited at least
once outside `docs/src/adr/` (enforced by `ci/adr-citation-lint`).
