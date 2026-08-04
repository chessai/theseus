# ADR-0029: Memory -- exchange, all the way down

**Status:** Accepted

## Context
A conventional load *copies* -- exactly what linearity (ADR-0004) bans.
If memory needs copy, CPU-as-iso (ADR-0022) dies at the first LOAD.

## Decision
- **Memory access is EXCHANGE** (Pendulum's discipline): swap register
  with cell -- `exch : (Reg, MemCell) <-> (Reg, MemCell)` is an iso on
  its face. Addressed exchange = Branch tree on address bits steering a
  swap = a tree of controlled swaps = **literally the ADR-0027 cell**.
  Memory is a Fredkin tree; language linearity, ISA exchange, and the
  single-cell fabric are one fact in three costumes.
- **Instruction fetch = fetch-execute-unfetch:** CNOT the ROM word onto
  the zeroed InstrReg (copy-onto-known-zero is reversible, ordinary
  clauses); execute with instruction bits acting purely as *controls*
  (the one legal fanout, ADR-0027); CNOT against the same word to
  re-zero; advance PC reversibly. The ROM threads through linearly.
- **Scale stratification:** **Option A (M4 v1):** memory lives in State
  -- registers + ~8 scratch words as a product type. Honest, pure,
  finite; costs per-tick energy ∝ size (storage is motion, C2.1) -- fine
  at register-file scale. **Option B (ratified scale-up):** memory as a
  separately compiled theseus iso with a swap-bus interface; step
  becomes (CpuState, BusMsg) <-> (CpuState, BusMsg'); the composed
  system is still a theseus program (A2 survives as modular isos).
  **Option C (parked):** adiabatic SRAM macro as a registry Prim with
  characterized energy + bijectivity-checked protocol. Wake: fabric-
  resident storage dominates the energy report (C3.5 names the moment).

## Consequences -- why not B in v1 (the degeneracy argument)
Under v1's unroll-only constraint (C2.4), B's benefit is unrealizable:
monolithically compiled, the memory iso unrolls alongside the CPU -- A's
energy profile with extra plumbing; the non-degenerate one-physical-block
version is a shared sequential resource = a recirculating structure =
exactly the parked ring-loop machinery. Choosing B early buys A with
indirection, plus a bus protocol designed blind before the ring machine
exists to inform it, at v1 scale (8 words) where the problem B solves
cannot arise. **The free hedge, ratified:** v1 CPU source routes all
scratch access through named exch-style isos -- B's *seam*, not its
machinery -- so A→B is a local refactor behind stable names.
