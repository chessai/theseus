# Error code catalog

Every diagnostic has a stable code; these pages are the single source of
truth for `--explain` (ADR-0020/0021). Skeleton allocation:

| Code | Meaning |
|---|---|
| T0001 | parse error |
| T0101 | unknown name / constructor |
| T0102 | type mismatch across `<->` |
| T0201 | nonlinear pattern variable |
| T0202 | variable missing on opposite side |
| T0301 | non-exhaustive clause side (witness: unmatched value) |
| T0302 | overlapping clauses (witness: value + clause pair) |
| T0303 | label state space not covered |
| T0401 | prim signature mismatch with registry |
| T0501 | construct outside the synthesizable dialect |

Each code gets a worked-example page as it is implemented; diagnostic
snapshots (code, spans, witness) live in the conformance suite.
