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

- **Fast (every commit / the per-attempt maestro gate):** units, small-type
  exhaustive sweeps, fixed-seed properties, snapshot tests. Seconds–low minutes.
- **Heavy (`--features heavy-tests`, advisor pre-merge):** integration-test
  binaries dominated by netlist lowering, system-ring simulation, whole-domain
  BDD/energy, or large whole-domain sweeps — each 30 s to many minutes.
- **Nightly:** large property budgets, fuzzing, SAT lemma checks
  (`--verify-lowering`), Verilator differentials, mutation testing. Hours.
  Nightly failures auto-minimize into the regression corpus.

### The `heavy-tests` gate (why the default `cargo test` is fast)

The full `theseus-tisc` integration suite exceeds ~50 min and cannot finish in a
delegation gate's window; several binaries run 4–12 min each (measured:
`compile` 723 s, `bveq_recognize` 460 s, `energy`/`decoder`/ring family, the
whole-domain `valwidth*`/`branching`). So every such binary carries
`#![cfg(feature = "heavy-tests")]` at the top — **off by default**, so a plain
`cargo test -p theseus-tisc` compiles them empty and the fast tier runs in
seconds. Two rules:

1. **Per-attempt maestro gate** (`check_commands` in a delegation spec): run only
   the fast tier, and prefer scoping to the change — e.g. `cargo test -p
   theseus-tisc --test <the_relevant_binary>` (like the χ task's `--test chi`).
   Never gate a delegation on the whole suite.
2. **Advisor pre-merge** (and whenever a change touches the energy / netlist /
   decoder / ring / recognizer path): run the heavy tier
   **`cargo test -p theseus-tisc --features heavy-tests -- --test-threads=1`**.
   The `--test-threads=1` is mandatory — concurrent heavy tests each hold GB-scale
   resident structures and stack past RAM (SIGKILL) even with the flake's
   `MALLOC_ARENA_MAX=1`. A `#[ignore]`d heavy test *within* an otherwise-fast
   binary (e.g. `chi.rs`'s system-ring authority) is run with `--test <bin> --
   --ignored --test-threads=1`.

Quality is retained because the complete suite still gates the **merge** (advisor,
above); only the fast-iteration loop is trimmed.

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
