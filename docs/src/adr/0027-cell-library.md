# ADR-0027: Cell library -- one parameterized cell

**Status:** Accepted

## Context
Dual-rail + our IR vocabulary collapse the library: NOT is a rail swap
(wiring); CNOT is a controlled rail swap; Fredkin is the same cell shape
on two rail pairs; Toffoli-class = two controls in series (series
T-gates implement control-AND natively); Branch's demux/merge is a tree
of controlled swaps; permutations are wiring (ADR-0014).

## Decision
- **B4.1** Cell set v1: **k-controlled dual-rail swap, k ∈ {0,1,2}**
  (k=0 is the buffer/latch and fabric timekeeper), plus bench
  source/sink cells and the B1.6 baseline comparators. k≤2 is a
  deliberate cap; whether deep prims prefer one 2-CSWAP or decomposed
  1-CSWAP networks is a *measured* answer, not doctrine.
- **B4.2** Registry prims must be expressible as CSWAP networks. No
  compute-uncompute ancilla macros in v1 (garbage-freedom means we never
  need Bennett scratch; admitting it forfeits token conservation).
  Relaxable with a wake condition.
- **B4.3** Cell contract: phase role, validity-window obligations
  (pinned per C2.6), DB energy-model parameters, Vt/body variants,
  reverse-drive validation. Admission test = the B5 suite including
  conservation bookkeeping.
- **B4.4** M3a bench: buffer shift register (canonical curve), CNOT
  chain, and a **reversible full adder** as the compiled block (exercises
  k=2; the right first "program" to run on physics).

## Structural payoffs (recorded because they carry design weight)
1. **Token conservation:** a controlled swap only routes charge; the
   count of high rails is invariant per tick -- a free integrity
   invariant asserted by the phased-sim (INV-TOKEN), physical cousin of
   ADR-0012.
2. **Self-inverse cells** maximize C2.5's plausibility: every cell is
   literally its own adjoint network.
3. **Linearity abolishes data fanout:** the compiled netlist has *no
   data fanout at all* -- only control fanout (loads, priced by the DB,
   managed by buffer trees). Type-system hygiene turned physical-design
   discipline.

## Named risk
Internal staging of k-CSWAP (control hold times vs target transit;
whether it composes with 2LAL buffer discipline without internal
phases) is exactly C2.6's deferred arithmetic. If characterization
reveals internal ticks, it becomes a macro-cell -- latency numbers
change, architecture doesn't. First question the bench answers.
