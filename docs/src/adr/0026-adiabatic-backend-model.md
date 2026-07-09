# ADR-0026: The adiabatic backend's timing model — phase rotation

**Status:** Accepted. Refines ADR-0015 (post-flatten balancing is
**mandatory for correctness** in this backend, not optional QoR).

## Context
In 2LAL every gate takes one tick and every gate *is* a register: data
physically advances one stage per clock phase. Logic and state merge;
the combinational abstraction dissolves; the target is a fully systolic
fabric pipelined at single-gate granularity.

## Decision
- **C2.1** `Reg(n)` = n-stage buffer chain. Physics inversion recorded:
  holding costs motion; storage has per-tick energy. Retiming stays
  sound; its cost model flips sign.
- **C2.2** Reconvergent inputs must arrive on the same tick — mismatch is
  a *wrong circuit*, not a slow one. Typed-IR balancing stays coarse;
  the backend's tick-exact balancer is mandatory, with a minimal-buffer
  (= minimal-energy) objective feeding ADR-0028.
- **C2.3** Phase assignment = topological depth mod P (P from
  TargetProfile; 4 now, 8 under S2LAL). C4 resolves to a coloring that
  falls out of balanced depth. The scheduler is unit-generic (cycles for
  FPGA, ticks here).
- **C2.4** Loops: **unroll-only in v1** — ADR-0007's static bound ≤ |u|
  guarantees unrolling is always possible; a partial language could not
  make this backend choice. Recirculating rings parked; wake condition:
  unrolled area unaffordable (formally declared by M4, ADR-0030).
- **C2.5** **Physical adjointness — design goal with validation
  obligation** (ledger TH-6, empirical): T-gates are electrically
  bidirectional; reversing the phase sequence may execute the adjoint on
  the same silicon with zero added logic. Clean for shift registers;
  general logic depends on ADR-0027 cells being bidirectional as
  networks. B5 gains reverse-drive tests per cell and per block.
  Fallback: mirrored-netlist emission (adjoints are syntactic,
  ADR-0013). Caveat: direction is modal (chip-level clocking choice).
- **C2.6** Validity windows are a checked protocol: exact window
  arithmetic **deliberately deferred to B5 empirical pinning** against
  the reference decks, then encoded as a tick-level validity calculus —
  violations are compile errors; the phased-sim asserts the protocol
  every tick (spirit of ADR-0012).
- **C2.7** New artifact: the **phased cell netlist** (dual-rail,
  cell-mapped, phase-attributed) with its own tick-level simulator; the
  oracle chain gains the link. The typed IR needs *no vocabulary change*
  — ADR-0011/0013's structure-in-IR, costs-in-backends survives its
  hardest test.
- **C2.8** TargetProfile additions: P, tick-time range, fanout limit +
  buffer-tree policy (load-priced via the DB), loop policy, energy-
  weighted balancing objective.
