# ADR-0016: Pass pipeline

**Status:** Accepted

## Context
Prior decisions pin the stage order; the design content is boundary
artifacts, per-pass contracts, and verification attachment points.

## Decision
`parse → elaborate → core rewrites → canonicalize (ADR-0014) → typed
passes (dedup/CSE over σ, Reg retiming, loop strategy) → schedule+balance
(ADR-0015) → flatten → lower (ADR-0011/0012) → emit`.

- **User-error/ICE line at elaboration:** any failure after successful
  elaboration is an internal compiler error. Spans are rich up to core;
  every IR node thereafter carries a provenance backpointer.
- **Prims** resolve at elaboration against the registry; registry
  bijectivity checks run offline, not per compile.
- **Minimal core rewrite set:** anything about swaps/assocs/units belongs
  to the canonicalizer — deleted from core rewrites by rule. What remains:
  trace laws (yanking, dinaturality), Branch/Par fusion, prim identities.
- **TargetProfile** parameterizes scheduling onward (latency estimates,
  unroll-vs-counter threshold; later, adiabatic phase discipline). Passes
  before scheduling are target-independent by construction.
- **Loop strategy decided pre-balancing** (it changes the schedule);
  profile-driven with a per-site override attribute.
- **Verification spine:** (1) oracle chain — every artifact executable,
  per-pass differential tests; (2) the adjoint commutation law
  `P(f†) ≡ P(f)†` for every pass — one property, huge bug net;
  (3) invariant ledger — passes declare required/established invariants
  as data; the driver validates ordering statically and checks artifacts
  in debug builds.
- **Driver:** sequential, deterministic batch; all boundaries dumpable
  (`--emit=...`); golden snapshots per stage. Salsa-style incrementality
  noted as compatible, not built.
