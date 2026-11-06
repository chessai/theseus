# ADR index

Append-only. Statuses: **Accepted**, **Superseded (by NNNN)**. Every ADR
must be cited at least once outside this directory (`ci/adr-citation-lint`).
Numbering preserves the original planning-log identifiers. All ADRs below are
**Accepted** unless noted.

## Foundations & pipeline (0001-0021)

- [ADR-0001: Rust end to end](0001-rust-end-to-end.md)
- [ADR-0002: Pipeline shape](0002-pipeline-shape.md)
- [ADR-0003: Surface/core split](0003-surface-core-split.md)
- [ADR-0004: Coverage checking at elaboration](0004-coverage-checking.md)
- [ADR-0005: Partiality semantics](0005-partiality-semantics.md)
- [ADR-0006: Two dialects](0006-two-dialects.md)
- [ADR-0007: Totality](0007-totality.md)
- [ADR-0008: Differential testing as the verification strategy](0008-differential-testing.md)
- [ADR-0009: Stratified core loop story](0009-stratified-loops.md)
- [ADR-0010: Fixed-order rewrites in v0.1; equality saturation deferred](0010-rewrite-engine.md)
- [ADR-0011: Sums stay typed in the netlist IR; encoding is backend lowering](0011-sum-encoding.md)
- [ADR-0012: Canonicity -- conditional and locally re-established](0012-canonicity.md)
- [ADR-0013: Netlist IR node vocabulary](0013-node-vocabulary.md)
- [ADR-0014: Structural plumbing canonicalizes to Permute](0014-permute-canonicalization.md)
- [ADR-0015: Path balancing on the typed IR](0015-path-balancing.md)
- [ADR-0016: Pass pipeline](0016-pass-pipeline.md)
- [ADR-0017: Surface syntax](0017-surface-syntax.md)
- [ADR-0018: Testing architecture](0018-testing-architecture.md)
- [ADR-0019: Nix, toolchain, and dev environment](0019-nix-toolchain.md)
- [ADR-0020: Crate map and library choices](0020-crate-map.md)
- [ADR-0021: Documentation organization](0021-doc-organization.md)

## Adiabatic pivot & backend (0022-0032)

- [ADR-0022: The adiabatic pivot -- design and simulate the CPU; FPGA floats](0022-adiabatic-pivot.md)
- [ADR-0023: Logic family -- 2LAL first, S2LAL as ratified successor](0023-logic-family.md)
- [ADR-0024: PDK and device models](0024-pdk.md)
- [ADR-0025: SPICE toolchain and characterization harness](0025-spice-harness.md)
- [ADR-0026: The adiabatic backend's timing model -- phase rotation](0026-adiabatic-backend-model.md)
- [ADR-0027: Cell library -- one parameterized cell](0027-cell-library.md)
- [ADR-0028: Energy annotation methodology](0028-energy-annotation.md)
- [ADR-0029: Memory -- exchange, all the way down](0029-memory.md)
- [ADR-0030: TISC-1 -- ISA principles](0030-tisc.md)
- [ADR-0031: Dual-rail lowering -- the lemma catalog (C1)](0031-dual-rail-lowering.md)
- [ADR-0032: The idealization ladder (B3, formalized)](0032-idealization-ladder.md)

## Ring machine, reversible control & v0.2 dialect (0033-0040)

- [ADR-0033: Recirculating execution -- the ring machine (M4-v2)](0033-ring-machine.md)
- [ADR-0034: Reversible branching -- the offset-BR / come-from discipline](0034-reversible-branching.md)
- [ADR-0035: μ-types, increment 1 -- bounded unrolling at lowering](0035-mu-bounded-unroll.md)
- [ADR-0036: `iter` -- the first bounded-iteration core primitive](0036-iter-primitive.md)
- [ADR-0037: `fold`/`unfold` -- structural-recursion core primitives](0037-fold-unfold.md)
- [ADR-0038: v0.2 surface syntax -- `iter` / `fold` / `unfold` combinator forms](0038-v0.2-surface-syntax.md)
- [ADR-0039: `iter`/`fold`/`unfold` netlist lowering -- tag-gated full unroll](0039-iter-fold-unfold-lowering.md)
- [ADR-0040: The controlled rail-role-swap cell (Rk) -- voiding the parity law](0040-rail-role-swap-cell.md)

## Surface → TISC compiler milestones (0041-0046)

- [ADR-0041: Execution models -- per-program hardware vs stored-program CPU](0041-execution-models.md)
- [ADR-0042: Surface theseus → TISC-1 ROM compiler architecture](0042-thse-to-tisc-compiler.md)
- [ADR-0043: M-B -- reversible-branching codegen (conditional execution)](0043-mb-reversible-branching-codegen.md)
- [ADR-0044: M-C -- compiler-targeted memory (scratch spill → Option-B bus)](0044-mc-compiler-memory.md)
- [ADR-0045: M-C2 -- the Option-B memory bus (external recirculating memory)](0045-mc2-memory-bus.md)
- [ADR-0046: M-C2 build -- the composite-ring memory bus](0046-mc2-build-composite-ring.md)
- [ADR-0047: W<n> machine-word type and the `add_into` word-add prim](0047-wn-word-type-and-add-into-prim.md)

## Simulation (0048-0049)

- [ADR-0048: Lane-parallel (bit-sliced) netlist simulation](0048-lane-parallel-netlist-simulation.md)
- [ADR-0049: Lane-parallel `BvExpr` evaluation](0049-lane-parallel-bvexpr-eval.md)

## Verification tooling (0050-)

- [ADR-0050: Packed gate truth tables and the Shannon BDD build](0050-packed-gate-tables.md)
