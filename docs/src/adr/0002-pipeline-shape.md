# ADR-0002: Pipeline shape

**Status:** Accepted

## Context
With Clash removed (ADR-0001) the compilation route must be fixed.

## Decision
`surface theseus → core Π IR → typed reversible netlist IR → Verilog`
(FPGA now; Sky130 cell mapping later), with an owned cycle-level netlist
simulator, and cosimulation recovered via differential testing:
interpreter ≡ netlist-sim ≡ Verilator (ADR-0008).

## Consequences
We own every stage and every artifact. The netlist IR is the load-bearing
middle (ADR-0011..0015). Verilator is test infrastructure, reached through
the TargetTools seam (ADR-0019).

## Alternatives rejected
Emitting HDL directly from core Π (no place for typed-sum lowering,
scheduling, or the invariant story); targeting FIRRTL/Calyx (their IRs
answer different questions; ours must carry reversibility structure).
