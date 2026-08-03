# Energy methodology

The load-bearing claim: in this fabric, **per-run energy is
data-independent to first order** (dual-rail + linearity + unrolling),
so the primary artifact is a *static report* -- Σ over cells × occupied
ticks of DB-characterized energies (ADR-0028). Everything else exists to
keep that report honest:

- The characterization DB (ADR-0025) is the sole source of per-cell
  numbers; interpolation only, extrapolation is a hard error; every
  number stamped with PDK/simulator/harness versions and its
  idealization rung (ADR-0032).
- Additivity (TH-E1) and data-independence (TH-E2) are *validated
  assumptions* with an explicit error budget: ±15% absolute; log-log
  E-vs-period slope within ±0.1 of the theoretical −1 -- slope is the
  hard requirement (axes per ADR-0023).
- Attribution flows through provenance to the source: `--emit=energy`
  reports pJ/op per clause and per iso, with balancing-buffer share as a
  tracked metric.
- Bench energy reports are committed golden files; regressions are PR
  diffs. The three-curve money plot (canonical axes per ADR-0023: E/op
  vs period -- 2LAL slope −1, CMOS flat, PFAL floored; one netlist) is
  M3a's acceptance artifact.
- Internal conservation bookkeeping (C3.7) fails composition bugs before
  physics is consulted; token conservation (INV-TOKEN) fails protocol
  bugs before functional comparison would notice.
