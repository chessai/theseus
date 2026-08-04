# Lowering and emission

Flattening dissolves regions; `Branch` becomes the demux/mask/merge
pattern in one generated place (ADR-0013). Lowering applies the backend
encoding under per-node lemmas (ADR-0012).

**Lowering (live).** Lowering lives in **`theseus-netlist`**
(`crates/theseus-netlist/src/lower.rs`, `lower_iter.rs`): a Core Π iso lowers to
a **`PhasedCellNetlist`** over the k-CSWAP cell family (ADR-0027/0031), the input
to balancing (ADR-0026) and energy annotation (ADR-0028). The `theseus lower
<FILE> <ISO>` CLI verb runs this and checks the result against the differential
oracle (see [Surface → compiled circuit](../surface-to-cells.md)).

## Verilog emission -- future work (v0.2+)

The FPGA / Verilog terminus is **not implemented**. The `theseus-emit` crate is
an empty stub; there is no typed Verilog AST and no Verilator harness today. The
design below is retained as the *intended* shape of that future path (ADR-0020),
not a description of shipped functionality.

Emission is planned to go through an owned **typed Verilog mini-AST** (ADR-0020):
modules, ports, wires, instances, assigns, `always_ff` -- nothing else (structural
SystemVerilog subset). Don't-cares would be unrepresentable; declared-before-used
and width agreement constructor-enforced; the AST layer snapshot-tested
independently.

Verilator integration (planned): standalone verilated binary + framed stdio
protocol, the protocol also serving as the conformance suite's simulator
interface. `libloading` in-process is the recorded perf upgrade path.
