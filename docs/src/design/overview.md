# Overview and pipeline

theseus compiles a reversible clause language to reversible netlists and
Verilog. Every program denotes a partial isomorphism; in the synthesizable
fragment (ADR-0006), a total one with a static worst-case latency
(ADR-0007).

## Pipeline (route: ADR-0002; pass structure: ADR-0016)

```text
source ─parse→ Surface AST ─elaborate→ Core Π ─rewrites→ Core Π
  ─canonicalize→ Typed IR ─typed passes→ ─schedule/balance→ Typed IR
  ─flatten→ Typed flat netlist ─lower→ Bit netlist ─emit→ Verilog
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
path lowers and emits Verilog; the adiabatic path cell-maps to the
phased netlist and emits SPICE decks (see the adiabatic backend
chapter). Every artifact is executable, serializable, and dumpable (`--emit=…`);
this is what powers the verification spine (ADR-0008, ADR-0018).
