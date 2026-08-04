# ADR-0022: The adiabatic pivot -- design and simulate the CPU; FPGA floats

**Status:** Accepted. Supersedes the L3/L4 sequencing of the original roadmap.

## Context
The FPGA board (ULX3S) is supply-constrained, and -- more fundamentally --
the FPGA was always the *legibility* demo, not the contribution. Logical
simulation cannot see adiabatic-ness (charge recovery lives in SPICE, not
Verilator), so the promised land requires a two-level simulation program
regardless of hardware availability. Vaire's silicon results validate the
physics layer; no one has a language-to-silicon reversible stack.

## Decision
1. **Milestones restructure:** M3a = adiabatic cell library + energy-
   recovery scaling demonstrated in SPICE (ADR-0023/0024/0025/0027).
   M3b = dual-rail lowering + energy-annotated simulation of a compiled
   spatial block (ADR-0026/0028/0031). M4 = the theseus-CPU (ADR-0029/
   0030). M5 (optional) = tape-out, now de-risked by simulation. The FPGA
   demo becomes a **floating milestone** landing whenever hardware does
   (definition-of-done pinned in the roadmap so it cannot bloat).
2. **CPU-as-program principle:** the CPU is a theseus program
   (`step : State <-> State`) compiled by our own compiler through the
   adiabatic backend -- never a hand-designed sibling project. This
   principle is the one most tempting to quietly violate when the
   compiler makes something awkward; violations require a superseding ADR.
3. The energy story ascends by size: first energy-validated artifact is a
   compiled spatial block (reversible adder), the CPU is the headline act
   after. (A von Neumann CPU is thermodynamically a perverse flagship --
   fetch/control overhead spatial circuits don't pay -- recorded honestly.)

## Consequences
The project becomes two-tracked (language track, physics track),
independent for months -- see the roadmap's dependency graph. The
`silicon` devShell wakes early but thin (ADR-0019 amendment: ngspice,
xschem, netgen, pinned PDK; no OpenLane until parasitics matter).
