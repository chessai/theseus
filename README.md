# theseus

A mature, reversible programming language in the Theseus/Π lineage, with an
interpreter, a compiler to reversible netlists and Verilog, and a long-range
target of adiabatic silicon.

> Every program is a partial isomorphism. In the synthesizable fragment,
> every program is a *total* isomorphism with a static worst-case latency —
> by construction, not by annotation.

## Project layout

| Path | Contents |
|---|---|
| `crates/` | Multi-crate Rust workspace (see ADR-0020 for the crate map) |
| `docs/` | mdBook: ADRs, design document, language spec, theory notes, roadmap |
| `docs/diagrams/` | Structurizr DSL architecture models (ADR-0021, amended) |
| `conformance/` | Versioned, implementation-independent conformance suite (ADR-0018) |
| `flake.nix` | Nix: fenix-pinned nightly toolchain, crane builds, dev shell (ADR-0019) |

## The stack (post-pivot, ADR-0022)

Two tracks, independent until they meet:

- **Language track:** parser → bidirectional coverage checker →
  interpreter → typed netlist IR → Verilog/Verilator (M1–M2).
- **Physics track:** 2LAL cell library in Sky130 SPICE → characterization
  DB → the slope −2 money plot (M3a).
- **Convergence:** dual-rail lowering + energy-annotated compilation of a
  reversible adder (M3b), then **TISC-1** — a CPU written as a single
  theseus iso, run forward and backward, with per-clause energy
  attribution (M4). FPGA demo floats; tape-out (M5) is optional and
  de-risked by simulation.

See `docs/src/roadmap.md` for the dependency graph, milestones, and
parked items.

## Getting started

```sh
nix develop          # toolchain + verilator/yosys/nextpnr + cargo-* tools
cargo build          # workspace skeleton builds with zero external deps
mdbook serve docs    # browse ADRs, design doc, spec, theory notes
```

The workspace skeleton is intentionally dependency-free; dependencies are
introduced per-crate as components land, following ADR-0020. `flake.lock`
is not committed in the skeleton — run `nix flake lock` once on first use.

## Design discipline

- Decisions are ADRs (`docs/src/adr/`), append-only, individually citable
  as `ADR-NNNN` from code, tests, and the invariant ledger.
- Nothing ships without the definition-of-done checklist in `CONTRIBUTING.md`.
- The language spec is implementation-silent; the conformance suite is its
  executable appendix.
