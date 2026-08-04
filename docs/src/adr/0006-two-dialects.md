# ADR-0006: Two dialects

**Status:** Accepted

## Context
Clash's own full-vs-synthesizable split is the proven shape for languages
with a hardware target.

## Decision
Full dialect (interpreter accepts everything) vs **synthesizable
fragment**: finite types only in v0.1; recursive μ-types arrive in the
software dialect at v0.2 and stay out of synthesis until the streaming
story is designed.

## Consequences
Layer 2 is never blocked on type-theory work. The spec's dialect chapter
is normative; the conformance suite is partitioned by dialect.

## Alternatives rejected
One dialect with synthesis errors discovered late (bad UX, muddy spec).
