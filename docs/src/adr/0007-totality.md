# ADR-0007: Totality

**Status:** Accepted

## Context
Should loops be provably terminating in the type system? Three regimes:
finite types, μ-types, full generality. A reversibility-specific trap
applies throughout: forward totality does **not** imply backward totality
— any termination evidence must hold in both directions
(theory/bidirectional-descent.md).

## Decision
- **Finite fragment:** totality is free. `trace` over finite `u` cannot
  diverge: the body is injective, so trajectories through `u` cannot
  repeat a state; iteration count ≤ |u|. Every synthesizable v0.1 program
  is a total iso by construction, with a static WCET the compiler reports
  (ledger entry TH-1: exact bound + two-direction proof still owed).
- **μ-types (v0.2):** blessed total-by-construction combinators — bounded
  iteration (the count lives in the data, symmetric by construction) and
  reversible fold/unfold pairs. Raw unbounded trace stays expressible but
  explicitly marked partial, `unsafe`-style. Primitive-recursive
  completeness (RPP, Paolini–Piccolo–Roversi) is the expressiveness floor.
- **Sized types:** documented research extension, not on the roadmap.

## Consequences
Totality and hardware WCET are the same theorem; unroll-vs-counter is a
backend choice on a known bound (ADR-0013). Totality-by-default keeps the
quantum lift frictionless (unitaries are total).

## Alternatives rejected
Full sized/indexed types now (research-grade metatheory, soundness-bug
history, and it must be done *twice* — once per direction);
Turing-complete synthesizable dialect (a feature with no customer in a
netlist-bound language).
