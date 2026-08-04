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

- **Fast (every commit / the pre-commit check):** units, small-type
  exhaustive sweeps, fixed-seed properties, snapshot tests. Seconds–low minutes.
- **Heavy (`--features heavy-tests`, advisor pre-merge):** integration-test
  binaries dominated by netlist lowering, system-ring simulation, whole-domain
  BDD/energy, or large whole-domain sweeps — each 30 s to many minutes.
- **Nightly:** large property budgets, fuzzing, Verilator differentials,
  mutation testing. Hours.
  Nightly failures auto-minimize into the regression corpus.

### The `heavy-tests` gate (why the default `cargo test` is fast)

The full `theseus-tisc` integration suite is long — several binaries run minutes
each (`compile`, `bveq_recognize`, the `energy`/`decoder`/ring family, the
whole-domain `valwidth*`/`branching`) — too long for a fast edit/check loop. So
every such binary carries `#![cfg(feature = "heavy-tests")]` at the top — **off by
default**, so a plain `cargo test -p theseus-tisc` compiles them empty and the
fast tier runs in seconds. Two rules:

1. **While iterating (per commit):** run only the fast tier, and prefer scoping to
   the change — e.g. `cargo test -p theseus-tisc --test <the_relevant_binary>`.
2. **Before merging** (and whenever a change touches the energy / netlist /
   decoder / ring / recognizer path): run the heavy tier
   **`cargo test -p theseus-tisc --features heavy-tests -- --test-threads=1`**.
   The `--test-threads=1` is mandatory — concurrent heavy tests each hold GB-scale
   resident structures and stack past RAM (SIGKILL) even with the flake's
   `MALLOC_ARENA_MAX=1`. A `#[ignore]`d heavy test *within* an otherwise-fast
   binary is run with `--test <bin> -- --ignored --test-threads=1`.

Quality is retained because the complete suite still gates the **merge**; only the
fast-iteration loop is trimmed.

## Machine-checked proofs (`theseus-proofs/`)

The Lean 4 / Mathlib meta-theory lives in `theseus-proofs/` and builds separately
from the Rust workspace (pinned Mathlib; fetch its prebuilt oleans, then build):

```
nix develop .#proofs --command bash -c 'cd theseus-proofs && lake exe cache get && lake build'
```

Soundness is enforced by the build, not by convention:

- **No `sorry`, no `native_decide`, no new axioms.** Every key theorem must reduce
  to the Mathlib classical base `{propext, Classical.choice, Quot.sound}`. A plain
  `sorry` is only a *warning* to `lake`, so `TheseusProofs/AxiomGuard.lean` checks
  the axiom dependencies during `lake build` and **fails the build** on any
  violation (`sorryAx`, `native_decide`'s `ofReduceBool`, or a stray axiom). CI
  runs this on a fresh checkout, so it cannot go stale. Prefer `by decide` (kernel)
  over `native_decide` (which the guard bans anyway).
- **A proof ships only when paired** with (a) the Lean theorem, (b) a
  natural-language companion proof in `docs/src/theory/`, and (c) a row in
  `docs/src/theory/obligation-ledger.md` stating what is proved and — honestly —
  what is only modeled or scoped out. Overclaiming the Lean's reach (e.g. a prose
  claim stronger than the mechanized statement) is a defect.
- When you add or rename a checked theorem, add it to the `AxiomGuard.lean` list
  so the guard covers it.

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
