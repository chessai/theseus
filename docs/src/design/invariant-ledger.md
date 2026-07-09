# Appendix: invariant ledger

Machine-checked registry (skeleton — grows with passes). Cited by the
driver's static ordering check (ADR-0016).

| ID | Invariant | Established by | Required by | Checked |
|---|---|---|---|---|
| INV-CANON | no raw structural combinators | canonicalize | all typed passes | artifact scan |
| INV-WF | `valid ⇒ well-formed` on sum bundles | lowering lemmas + masks (ADR-0012) | sims, emit | sim assertion, lemma tier |
| INV-SCHED | inputs valid at firing time | schedule | flatten, emit | debug validator |
| INV-FLAT | no regions | flatten | lower | artifact scan |
| INV-NOX | no don't-cares in HDL | typed Verilog AST | — | unrepresentable (ADR-0020) |
| INV-DET | byte-identical artifacts across runs | all | conformance | T1 determinism |
| INV-TOKEN | count of asserted rails conserved per tick (adiabatic backend) | ADR-0027 cell semantics | phased-sim, deck-level checks | asserted every tick; violations localize to a node |
| INV-STAMP | no unstamped energy number (versions + idealization rung) | ADR-0028/0032 report generator | all energy artifacts | constructor-enforced |
