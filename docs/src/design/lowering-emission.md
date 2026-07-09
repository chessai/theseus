# Lowering and emission

Flattening dissolves regions; `Branch` becomes the demux/mask/merge
pattern in one generated place (ADR-0013). Lowering applies the backend
encoding under per-node lemmas (ADR-0012).

Emission goes through an owned **typed Verilog mini-AST** (ADR-0020):
modules, ports, wires, instances, assigns, `always_ff` — nothing else (structural SystemVerilog subset).
Don't-cares are unrepresentable; declared-before-used and width agreement
are constructor-enforced; the AST layer is snapshot-tested independently.

Verilator integration: standalone verilated binary + framed stdio
protocol; the protocol is also the conformance suite's simulator
interface. `libloading` in-process is the recorded perf upgrade path.
