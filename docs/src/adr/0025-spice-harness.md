# ADR-0025: SPICE toolchain and characterization harness

**Status:** Accepted

## Decision
- **B5.1** ngspice primary (what the Sky130 models are tested against;
  what the revcomp reference decks target; in nixpkgs), behind the
  ADR-0019 tool seam. Xyce = recorded scale-up path with a named caveat:
  model-compatibility validation before any Xyce number is trusted.
- **B5.2** SPICE decks are **compiler output** (crate `theseus-spice`):
  subcircuit instantiations, 4-phase ideal sources, stimulus through the
  same typed encoders as every backend, `.measure` directives;
  deterministic and snapshot-tested. Hand-authored content confined to
  the cell library (`.subckt` in-repo; xschem for viewing; layout/Magic
  deferred until parasitics stage).
- **B5.3** **The oracle chain extends into physics:** transients decoded
  back through the typed layer (sampled at valid times, thresholded,
  rail-pairs interpreted) and compared against the interpreter.
  interpreter ≡ typed-sim ≡ bit-sim ≡ Verilator ≡ **ngspice**. No
  hand-eyeballed waveforms as a correctness method, ever.
- **B5.4** DUTs: each cell, canonical shift register, one compiled block,
  B1.6 baselines. Sweeps: ramp time ≥3 decades log-spaced × load × Vt ×
  body. Measures: ∫V·I per power-clock source per cycle (nets out
  recovered charge), leakage share via long holds, functional verdict via
  B5.3. **Harness self-validation gates belief:** energy-conservation
  bookkeeping closes within tolerance; analytic RC structures match
  closed forms; reproduction of the slope −2 shift-register curve is the
  acceptance gate -- fail that and the harness is wrong, not the field.
- **B5.5** Output artifact: the **characterization database** -- per-cell
  E(ramp, load, flavor, body) tables + fitted coefficients of the
  standard adiabatic form, version-stamped (B2.2), RON, in-repo,
  diffable. Its schema is the interface to ADR-0028; the additive-
  composition assumption it embodies is ledger entry TH-E1.
- **B5.6** CI: smoke deck (single buffer, one ramp point, conservation
  check) in fast tier; sweep campaigns nightly/weekly with DB diffs
  reviewable in PRs.
