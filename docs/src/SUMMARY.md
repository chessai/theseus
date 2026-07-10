# Summary

[Introduction](introduction.md)

# Architecture Decision Records

- [ADR index](adr/index.md)
  - [0001 Rust end to end](adr/0001-rust-end-to-end.md)
  - [0002 Pipeline shape](adr/0002-pipeline-shape.md)
  - [0003 Surface/core split](adr/0003-surface-core-split.md)
  - [0004 Coverage checking at elaboration](adr/0004-coverage-checking.md)
  - [0005 Partiality semantics](adr/0005-partiality-semantics.md)
  - [0006 Two dialects](adr/0006-two-dialects.md)
  - [0007 Totality](adr/0007-totality.md)
  - [0008 Differential testing](adr/0008-differential-testing.md)
  - [0009 Stratified core loop story](adr/0009-stratified-loops.md)
  - [0010 Fixed rewrites now, eqsat later](adr/0010-rewrite-engine.md)
  - [0011 Typed sums; encoding is backend lowering](adr/0011-sum-encoding.md)
  - [0012 Canonicity invariant](adr/0012-canonicity.md)
  - [0013 Netlist node vocabulary](adr/0013-node-vocabulary.md)
  - [0014 Permute canonicalization](adr/0014-permute-canonicalization.md)
  - [0015 Path balancing on typed IR](adr/0015-path-balancing.md)
  - [0016 Pass pipeline](adr/0016-pass-pipeline.md)
  - [0017 Surface syntax](adr/0017-surface-syntax.md)
  - [0018 Testing architecture](adr/0018-testing-architecture.md)
  - [0019 Nix and toolchain](adr/0019-nix-toolchain.md)
  - [0020 Crate map and library choices](adr/0020-crate-map.md)
  - [0021 Documentation organization](adr/0021-doc-organization.md)
  - [0022 The adiabatic pivot](adr/0022-adiabatic-pivot.md)
  - [0023 Logic family: 2LAL → S2LAL](adr/0023-logic-family.md)
  - [0024 PDK and device models](adr/0024-pdk.md)
  - [0025 SPICE toolchain and harness](adr/0025-spice-harness.md)
  - [0026 Adiabatic timing model](adr/0026-adiabatic-backend-model.md)
  - [0027 Cell library: one cell](adr/0027-cell-library.md)
  - [0028 Energy annotation](adr/0028-energy-annotation.md)
  - [0029 Memory: exchange](adr/0029-memory.md)
  - [0030 TISC-1 principles](adr/0030-tisc.md)
  - [0031 Dual-rail lowering lemmas](adr/0031-dual-rail-lowering.md)
  - [0032 Idealization ladder](adr/0032-idealization-ladder.md)

# Design Document

- [Overview and pipeline](design/overview.md)
- [Core Π](design/core-pi.md)
- [Typed netlist IR](design/netlist-ir.md)
- [Encodings and canonicity](design/encodings.md)
- [Scheduling and balancing](design/scheduling.md)
- [Lowering and emission](design/lowering-emission.md)
- [Verification spine](design/verification.md)
- [The adiabatic backend](design/adiabatic-backend.md)
- [Energy methodology](design/energy-methodology.md)
- [TISC-1: the theseus CPU](design/tisc.md)
- [Appendix: invariant ledger](design/invariant-ledger.md)
- [Appendix: risk register](design/risk-register.md)
- [Test specification: M3a](design/test-spec-m3a.md)
- [Appendix: crate map](design/crate-map.md)
- [Appendix: TBD register](design/tbd-register.md)

# Language Specification (v0.1 draft)

- [Conformance and notation](spec/conformance.md)
- [Lexical structure and grammar](spec/grammar.md)
- [Types](spec/types.md)
- [Static semantics](spec/static-semantics.md)
- [Dynamic semantics](spec/dynamic-semantics.md)
- [Dialects](spec/dialects.md)
- [Error code catalog](spec/error-codes.md)
- [Test specification: M1](spec/test-spec-m1.md)

# Theory Notes

- [Proof-obligation ledger](theory/obligation-ledger.md)
- [Coverage ⇔ bijectivity (TH-4a)](theory/coverage-bijectivity.md)
- [Finite-trace totality](theory/finite-trace-totality.md)
- [The bidirectional-descent trap](theory/bidirectional-descent.md)
- [Coherence and canonical permutations](theory/coherence.md)
- [Bibliography](theory/bibliography.md)

# Roadmap

- [Layers, versions, parked items](roadmap.md)
