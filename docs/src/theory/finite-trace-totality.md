# Finite-trace totality (TH-1)

**Claim.** For finite `u`, `trace f : a ↔ b` of `f : u+a ↔ u+b` is a
total isomorphism; forward and backward evaluation terminate within
|u| iterations.

**Argument sketch.** A loop trajectory visits states of `u`. `f` is
injective, so no trajectory revisits a state: a repeat would give a
cycle inside `u`, and a cycle under an injective map admits no entry
point from `a` — contradiction with the trajectory having entered.
Hence trajectories are simple paths through `u`: length ≤ |u|, then the
exit in `b` is reached. The adjoint direction is the same argument for
`f†`.

**Owed for "proved":** the exact constant (≤ |u| vs |u|+1 body
applications), the immediate-exit case (zero iterations), the statement
as a quantitative bound usable verbatim in diagnostics, and both
directions written out rather than "symmetric".
Corollary: counter registers of width ⌈log(|u|+1)⌉ suffice — totality and
hardware WCET are the same theorem (ADR-0007).
