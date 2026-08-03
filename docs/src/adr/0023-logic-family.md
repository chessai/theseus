# ADR-0023: Logic family -- 2LAL first, S2LAL as ratified successor

**Status:** Accepted

## Context
Most "adiabatic" CMOS families in the literature (ECRL, PFAL, 2N-2N2P...)
are quasi-adiabatic: threshold-drop residues impose an energy floor no
slowing escapes -- disqualifying for a Landauer-approach thesis. The fully
adiabatic lineage: SCRL/CRL (Younis & Knight 1993; first fully adiabatic,
not fully static, practical difficulties under nonidealities), 2LAL
(Frank ~2004; transmission-gate, dual-rail, 4-phase trapezoidal clocks,
canonical slope −2 power-vs-period result), S2LAL (Frank 2020; first
fully static AND fully adiabatic, 8 phases, 1 tick/stage), Q2LAL
(DeBenedictis refinement). Bennett-clocked CMOS rejected (throughput
collapses with depth). Active community + public reference decks
(revcomp.org, sS2.cir lineage).

## Decision
- **B1.1** 2LAL for M3a: simplest fully adiabatic family, best-documented
  reference results, and M3a's acceptance test *is* the canonical
  scaling plot (axes and expected slopes defined under Acceptance below).
  Its non-static weakness is invisible under the ideal-source regime.
  **S2LAL is the ratified successor** (not fallback); triggers: leakage
  share at target ramps exceeds threshold set during characterization, or
  M5 planning begins. Migration is a cell-library swap; phase count moves
  4→8, so all phase machinery is P-generic from day one.
- **B1.2** 4-phase quadrature trapezoids, ideal piecewise-linear sources.
  The "tick" is the TargetProfile's time unit; per-stage latency and
  initiation interval are *measured*, not assumed.
- **B1.3** Dual-rail signal encoding -- confirming the ADR-0011 earmark.
  Logical NOT is a rail swap: zero transistors, a Permute (ADR-0014
  extends one level into the physics).
- **B1.4** Cell direction: transmission-gate idiom; monotone dual-rail
  AND/OR as DeMorgan duals under rail swap; controlled-swap naturally
  TG-shaped; buffer/latch as shift-register backbone. Contract: the
  family *requires* every intermediate be decomputed by a stage that
  knows its computation -- exactly what the compiler guarantees.
- **B1.5** Sky130 wrinkle → B2: T-gate FETs want isolated bodies; NMOS
  needs deep n-well or accepted body effect. Characterization axis.
- **B1.6** Baselines as first-class artifacts: static CMOS + PFAL
  comparator cells; the money plot is three curves from one netlist.

## M3a acceptance
2LAL shift register + one compiled block in Sky130 SPICE, ideal 4-phase
clocks. **Canonical money-plot axes (log-log): dissipated energy per
operation vs clock period.** Expected: 2LAL slope −1 (E/op ∝ 1/T, since
per-cycle dissipation ∝ (RC/T)·CV²) sustained across ≥2 decades; CMOS
flat (E/op ≈ CV²); PFAL floored by its non-adiabatic residue.
Equivalently, on power-vs-period axes: slopes −2 / −1 / floored -- the
literature's traditional presentation. Both renderings are emitted; the
acceptance criterion is the −1 energy slope.
