# ADR-0005: Partiality semantics

**Status:** Accepted

## Context
Isos are partial: loops may diverge (over infinite types), and domains
may be restricted. The semantics must say what non-coverage and
divergence *mean* operationally.

## Decision
Join-inverse-category flavored semantics (Kaarsgaard line). Operationally:
the interpreter is fuel-based; hardware loops carry valid/done handshakes;
non-coverage is statically excluded (ADR-0004), so divergence is the one
honest partiality.

## Consequences
Theory stays citable. v0.1's finite fragment never exercises partiality
(ADR-0007); the machinery exists for the software dialect and v0.2.

## Alternatives rejected
Total-only language from day one (kills the software dialect);
unspecified divergence behavior (untestable).
