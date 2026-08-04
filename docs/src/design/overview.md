# Overview and pipeline

theseus compiles a reversible clause language to reversible **adiabatic cell
netlists** (the k-CSWAP family) priced in measured joules. Every program denotes
a partial isomorphism; in the synthesizable fragment (ADR-0006), a total one with
a static worst-case latency (ADR-0007).

> **Note (backend scope).** The FPGA / Verilog path (`─emit→ Verilog`) is
> **future work (v0.2+)**: `theseus-emit` is an empty stub and no Verilog is
> produced today. The live backend is the adiabatic cell path; `lower` (in
> `theseus-netlist`) emits a phased cell netlist. The pipeline diagram below
> retains the Verilog terminus as the *intended* FPGA fan-out.

## Pipeline (route: ADR-0002; pass structure: ADR-0016)

```text
source ─parse→ Surface AST ─elaborate→ Core Π ─rewrites→ Core Π
  ─canonicalize→ Typed IR ─typed passes→ ─schedule/balance→ Typed IR
  ─flatten→ Typed flat netlist ─lower→ cell/bit netlist ─emit→ Verilog (planned)
```

Architecture views of this pipeline are maintained as Structurizr DSL in
`docs/diagrams/workspace.dsl` (ADR-0021).

Boundary contracts:

| Boundary | Contract |
|---|---|
| elaborate | last user-facing errors; after it, failures are ICEs |
| canonicalize | no raw structural combinators survive (ADR-0014) |
| schedule | static schedule attached; validity is compile-time (ADR-0013) |
| lower | encoding chosen; per-node lemmas apply (ADR-0011/0012) |
| emit | typed Verilog mini-AST; don't-cares unrepresentable (ADR-0020) |

Post-pivot (ADR-0022), the backend fans out after `flatten`: the FPGA
path (planned, v0.2+) would lower and emit Verilog; the adiabatic path (live)
cell-maps to the phased netlist and emits SPICE decks (see the adiabatic backend
chapter). Intermediate artifacts are executable and serializable, which is what
powers the verification spine (ADR-0008, ADR-0018). *A uniform per-stage
`--emit=...` dump flag on the driver is planned (v0.2+); the CLI today exposes
`check`/`run`/`repl`/`lower` and no `--emit` flag.*
