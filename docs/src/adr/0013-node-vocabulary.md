# ADR-0013: Netlist IR node vocabulary

**Status:** Accepted

## Context
ADR-0011/0012 constrain every node to be maskable, valid-aware, and
direction-symmetric; Π's compositionality wants containers, not gate soup.

## Decision
**Hierarchical IR** (nodes contain regions; flattening is a backend step).
Vocabulary — structural: `Par(f,g)`, `Branch(f,g)` (all sum-soundness
machinery lives in this one node's lowering), `Reshape`→`Permute` per
ADR-0014, `Distrib`/`Factor`, `Absurd` (statically dead, pruned).
Iteration: `TraceLoop` (carries WCET ≤ |u|; unroll-vs-counter is a
profile choice), `Iter` (reserved, v0.2), `HandshakeLoop` (v0.2+,
quarantined). Leaves: `Prim` via a **registry** — (forward, backward)
Rust semantics + HDL template, admitted only with a mechanically
discharged bijectivity check — and `Reg(n)` (semantic identity; retiming
across it is always sound). Boundary: typed ports + generated
encode/decode.

Cross-cutting: **adjoints are syntactic per node** (graph adjoint =
reverse edges + adjoint nodes; one-dir-muxed-circuit vs two-specialized
is a backend flag). **Static scheduling in v0.1** (totality ⇒ validity is
a compile-time fact; runtime valid/ready wires enter with HandshakeLoop).

## Consequences
Lemma budget is countable (ADR-0014 makes it ~3 structural + Branch +
TraceLoop + Reg + per-prim). `Par` with unequal-latency arms requires the
path-balancing pass (ADR-0015) — why `Reg` exists before any pipelining
ambitions.
