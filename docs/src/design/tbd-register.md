# Appendix: TBD register

Every deliberately unnumbered tolerance or deferred contract, with its
owed-by milestone -- so "deferred by decision" is never confusable with
"forgot." Closing an entry = a PR replacing the provisional value +
deleting the row. CI counts rows; the count may only decrease within a
milestone.

| ID | What | Provisional value | Owed by | Source |
|---|---|---|---|---|
| TBD-1 | conservation closure tolerance | 1% | M3a harness bring-up | ADR-0025 B5.4 |
| TBD-2 | analytic-RC agreement gate | 5% | M3a harness bring-up | test-spec-m3a |
| TBD-3 | TH-E2 per-run energy variance bound | -- | M3b battery | ADR-0028 C3.4 |
| TBD-4 | decode thresholds/sampling for typed decode | 0.3/0.7·VDD, mid-validity | M3a first cell | ADR-0025 B5.3 |
| TBD-5 | per-cell fit adequacy bound | R² ≥ 0.99 | M3a DB build (TH-E3) | ADR-0028 C3.1 |
| TBD-6 | validity-window arithmetic | window-TBD(bench) holes | M3a bench (C2.6) | ADR-0026/0031 |
| TBD-7 | REPL output format stability | unstable | v0.2 | test-spec-m1 |
| TBD-8 | `--emit` format stability | unstable, deterministic | v0.2 | test-spec-m1 |
| TBD-9 | TISC-1 opcode encodings | -- | M4 start | ADR-0030 P5 |
| TBD-10 | characterization DB schema (beyond m3a contract) | -- | M3a DB build | ADR-0025 B5.5 |
| TBD-11 | Option-B bus protocol | seam only | M4-v2 design | ADR-0029 |
| TBD-12 | mutation-score thresholds per module | -- | first M1 module ships | ADR-0018 |
