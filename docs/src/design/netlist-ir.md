# Typed netlist IR

Hierarchical, typed, direction-symmetric (ADR-0013). Ports are
(valid, typed payload) bundles (ADR-0012).

| Node | Role | Lowering cost |
|---|---|---|
| `Par(f,g)` | product region | wiring |
| `Branch(f,g)` | sum region; all mux/demux soundness lives here | demux + masked merge |
| `Permute(monoid, σ)` | canonical structural plumbing (ADR-0014) | ×: zero gates; +: tag remap |
| `Distrib`/`Factor` | the only leaf-duplicating pair | quarantined per-backend |
| `TraceLoop` | finite trace; WCET ≤ \|u\| attribute | unroll or counter (profile) |
| `Iter` | reserved v0.2 bounded iteration | -- |
| `HandshakeLoop` | v0.2+ partial loops | valid/done |
| `Prim` | registry leaf; (fwd,bwd) semantics + HDL template | template |
| `Reg(n)` | semantic identity; retiming always sound | flops |
| `Absurd` | type-0 net; statically dead | pruned |

Adjoint: reverse edges + per-node syntactic adjoint. Whether a backend
emits one dir-muxed circuit or two specialized circuits is a flag, not an
IR concern. Scheduling is static in v0.1 (see scheduling chapter).
