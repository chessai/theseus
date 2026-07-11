# Proof-obligation ledger

The honest inventory of what the implementation leans on
(ADR-0021). States: **proved** / **sketched** / **assumed**.

| ID | Claim | Leaned on by | State |
|---|---|---|---|
| TH-1 | Finite-trace totality with exact bound, *both directions* (mutual-inverse), incl. immediate-exit edge case | ADR-0007 WCET diagnostics; TraceLoop unrolling | **proved** (`theory/finite-trace-totality.md`), Fable-5 adversarially reviewed — *conditional on TH-4a-label* for label-loop bodies; report WCET `≤ |u|` iterations, fuel budget `|u|+1` applications |
| TH-2 | Bidirectional descent: forward totality ⇏ backward totality; v0.2 combinators are symmetric-total | ADR-0007/0009 v0.2 design | sketched |
| TH-3 | Mac Lane coherence ⇒ Permute canonicity, rig fragment | ADR-0014 canonicalizer completeness test | assumed (textbook) + executable test; re-examine under fractional/negative types |
| TH-4a | Clause coverage + linearity ⇔ total bijection on typed values, plain fragment | ADR-0004; static-semantics soundness | **proved** (`theory/coverage-bijectivity.md`); label-extended case owed with the 2nd M1 slice |
| TH-4b | The bijection's mux/demux lowering is invertible under the padded bit encoding | ADR-0012 lowering lemmas | sketched — M2 obligation |
| TH-5 | Guard recomputability condition suffices for reversible guards | v0.2 proposal (ADR-0017) | sketched |
| TH-6 | Physical adjointness: reversed phase sequence executes the adjoint on the same fabric | C2.5 design goal; time-travel-on-silicon demo | empirical — B5 reverse-drive tests per cell and per block; fallback: mirrored-netlist emission |
| TH-E1 | Cell energies compose additively over a netlist | ADR-0028 static reports | empirical — whole-block SPICE battery, ±15% abs / ±0.1 slope budget |
| TH-E2 | Per-run energy is data-independent to first order | static-report primacy; DPA observation | empirical — phased-sim input sweeps, variance bound |
| TH-E3 | Per-cell fits are statistically adequate over swept ranges | every DB-derived number | statistical — residual bounds checked at DB build |
| TH-7 | Specializing `step` to a fixed ROM constant-folds the CPU into the program's circuit (temporal/spatial duality) | ADR-0030 framing; possible paper note | sketched |
