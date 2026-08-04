# ADR-0003: Surface/core split

**Status:** Accepted

## Context
Theseus-style clause programming is the desired surface; the compiler
wants a tiny calculus with equational laws.

## Decision
Surface = Theseus-style symmetric clauses (ADR-0017). Core = enumerated Π:
`id`, `sym`, sequential/parallel/choice composition, the structural isos
(swaps, assocs, units), `distrib`, `trace`. Elaboration compiles clauses
into core; every optimization is a law-justified rewrite on core; the
interpreter and backends consume core only.

## Consequences
Coverage checking has a precise home: a clause set elaborates iff it is a
legal iso (ADR-0004). The core stays citable against the Π literature.
Every core term has a free syntactic adjoint.

## Alternatives rejected
Interpreting/compiling surface clauses directly (duplicates semantics
across components; no equational optimization story).
