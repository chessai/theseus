# ADR-0028: Energy annotation methodology

**Status:** Accepted

## Context -- energy is (almost) static
Dual-rail: one rail high per valid signal regardless of value. Linearity:
no data fanout. Unrolled loops: every value traverses every stage. So
per-run energy is **data-independent to first order**, and the primary
energy artifact is a *static compiler report* -- a sum over cells and
occupied ticks of DB-characterized energies. The type system's linearity
bought predictable physics; dynamic accounting demotes to validation.

## Decision
- **C3.1** Per-cell model: fitted standard adiabatic form from the DB
  (CV²·(RC/t_ramp)-scaled + leakage·t + residual) over (ramp, load,
  flavor, body). Interpolation within swept range only; **extrapolation
  is a hard error**. Fit adequacy is ledger TH-E3, checked at DB build.
- **C3.2** Composition is additive -- **declared assumption** (TH-E1,
  empirical): total = Σ instances × occupied ticks, loads from actual
  netlist fanout. Validation battery: annotated prediction vs whole-block
  ngspice on every M3a bench design. Budget: absolute ±15%; **log-log
  slope of E vs period within ±0.1 of the theoretical −1** (canonical
  axes per ADR-0023) -- slope fidelity is the hard requirement (claims
  are comparative and scaling-shaped). Fallback if
  additivity misses badly: characterize composite structures (DB
  granularity changes, architecture doesn't).
- **C3.3** Claims boundary stamped on every number: logic dissipation
  under ideal power-clocks; clock-generation/resonator losses excluded
  (ADR-0032 ladder); I/O excluded; leakage included **broken out** (it
  decides the S2LAL trigger). Terminology: report *dissipated energy per
  op* and *energy circulated*; never "recovery factor" for simulation
  results.
- **C3.4** Data-independence is itself validated (TH-E2): phased-sim
  input sweeps must show per-run variance within bound; anomalies
  indicate upstream protocol/conservation bugs. Side observation: the
  near-constant power signature is a DPA-resistance property, certified
  per-design for free.
- **C3.5** **Source-level energy attribution** via ADR-0016 provenance:
  the ledger rolls up per cell-type, per IR node, **per source clause,
  per iso**. `--emit=energy`: "this clause set costs X pJ/op at
  t_ramp=T; 34% is balancing buffers." Buffer-overhead share becomes a
  tracked optimization metric (closes the loop with C2.2).
- **C3.6** Reports are first-class emissions (RON + plot-ready; the
  three-curve money-plot generator is the M3a acceptance artifact).
  Bench energy reports are committed golden files -- **energy regressions
  are reviewable PR diffs**. Fast tier recomputes static reports;
  nightly runs the TH-E1/E2 battery against SPICE.
- **C3.7** Conservation cross-check independent of SPICE: the
  phased-sim's per-node ledger must sum to source-side totals within
  tolerance -- composition bugs fail bookkeeping before physics is
  consulted.
