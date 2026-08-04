# Scheduling and balancing

Totality in the finite fragment (ADR-0007) makes validity a compile-time
fact: every node has static latency, so the circuit has a computable
schedule; no runtime valid wires exist until `HandshakeLoop` (v0.2).

- Latencies come from the **TargetProfile** (ADR-0016); passes before
  scheduling are target-independent by construction.
- Loop strategy (unroll vs counter) is decided pre-balancing --
  profile-driven, per-site overridable.
- **Path balancing** inserts `Reg`s so `Par` arms arrive in phase; runs on
  the typed IR (ADR-0015). Backends may re-balance post-flattening with
  exact latencies -- QoR only, never correctness.
- Schedule validity is an invariant-ledger entry checked in debug builds.
