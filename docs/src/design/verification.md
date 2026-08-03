# Verification spine

Three mechanisms at every boundary (ADR-0016), four test layers
(ADR-0018):

1. **Oracle chain** -- interpreter ≡ typed-sim ≡ bit-sim ≡ Verilator,
   per-pass differential; exhaustive below the type-size threshold.
2. **Adjoint commutation** -- `P(f†) ≡ P(f)†` for every pass; the single
   cheapest high-yield property in the suite.
3. **Invariant ledger** -- passes declare required/established invariants
   as data; ordering validated statically; artifacts checked in debug.

Flagship generator: the **bijection-table trick** -- random finite type,
random permutation of its value set, emit the literal clause set, compare
the whole pipeline against the table. Ground truth owing nothing to any
compiler stage.

`--verify-lowering`: miter over the bit-netlist IR → owned Tseitin →
`CnfSolver` (ADR-0020). Small widths run exhaustively in the fast tier;
SAT obligations run nightly. Sat models decode to typed witnesses.

Suite quality is measured, not assumed: cargo-mutants thresholds gate
merges (CONTRIBUTING definition-of-done).
