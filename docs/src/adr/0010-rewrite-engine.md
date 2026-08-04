# ADR-0010: Fixed-order rewrites in v0.1; equality saturation deferred

**Status:** Accepted

## Context
(Numbering note: this fills the planning-log slot spent on session
scoping; the decision here was ratified during the pass-pipeline session,
ADR-0016.) Core rewrites need a stopping rule: fixed pass order of
individually-terminating rewrites, or a saturating e-graph engine. `egg`
exists in Rust and Π's equational theory suits equality saturation -- but
it is a research-flavored dependency whose wins matter only at scale.

## Decision
Fixed, deterministic rewrite pipeline for v0.1. E-graphs (`egg`/`egglog`)
are a documented v0.3+ candidate; the core rewrite set is deliberately
kept law-per-law so migration is mechanical.

## Consequences
Predictable compiles, trivial termination story, one fewer dependency.
Rewrite laws double as T3 test properties either way (ADR-0018).
