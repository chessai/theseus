# Dialects

| | v0.1 synthesizable | software dialect |
|---|---|---|
| Types | finite only | + μ-types (v0.2) |
| Loops | labels/trace (total, bounded -- ADR-0007) | + `fold`/`unfold`/`for`; raw unbounded trace marked partial |
| Guards | none | none in v0.1; recomputability-checked proposal documented |
| Totality | by construction | total combinators blessed; partial constructs explicit |

Conformance suite entries are tagged by dialect.
