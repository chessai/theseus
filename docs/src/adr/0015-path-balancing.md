# ADR-0015: Path balancing on the typed IR

**Status:** Accepted

## Context
Static scheduling (ADR-0013) with `Par` arms of unequal latency needs
Reg insertion so bundles arrive in phase. Where should this run?

## Decision
On the typed IR, backend-portable, parameterized by the `TargetProfile`
(ADR-0016). A backend *may* re-balance post-flattening with exact
latencies -- a documented QoR option, never a correctness question: the
typed-level schedule remains sound, only potentially non-optimal.
