# Verification spine

Three mechanisms at every boundary (ADR-0016), four test layers
(ADR-0018):

1. **Oracle chain** -- interpreter ≡ typed-sim ≡ bit-sim ≡ Verilator,
   per-pass differential; exhaustive below the type-size threshold.
2. **Adjoint commutation** -- `P(f†) ≡ P(f)†` for every pass; the single
   cheapest high-yield property in the suite.
3. **Invariant ledger** -- passes declare required/established invariants
   as data (see the [invariant ledger](invariant-ledger.md)). Static ordering
   validation and debug artifact checks are the *intended* mechanism; the ledger
   is documentation-only today (driver-level validation is planned, v0.2+).

Ground-truth generators (actual, in use):

- **Exhaustive enumerator** (`theseus-eval`): the whole finite value domain is
  enumerated and the lowered netlist compared to the reference interpreter over
  every input -- the differential oracle. This is theseus's correctness norm.
- **Oracle-chain differential**: interpreter ≡ typed-sim ≡ bit-sim, per-pass.
- **BVEQ (whole-domain symbolic BDD equivalence)** (`theseus-tisc/src/bveq.rs`,
  `bveq_netlist.rs`): two fixed-width functions proved equal via canonical ROBDD
  equality -- domain-size-independent, so it scales past what enumeration can
  reach (e.g. a 3-operand op at full `Flat(8)`).

Conformance cases are **hand-written** fixtures (there is no randomized test
generation -- no `rand`/`proptest`/`quickcheck` in the workspace).

> **Planned (v0.2+).** A **bijection-table generator** -- pick a finite type,
> generate a permutation of its value set, emit the literal clause set, and
> compare the whole pipeline against the table (ground truth owing nothing to any
> compiler stage) -- is future work; it does not exist today. So is
> `--verify-lowering` (a miter over the bit-netlist IR → Tseitin → SAT, ADR-0020):
> there is no such flag and no SAT-based verifier -- BVEQ (above) is the actual
> whole-domain equivalence path.

Suite quality is measured, not assumed: cargo-mutants thresholds gate
merges (CONTRIBUTING definition-of-done).
