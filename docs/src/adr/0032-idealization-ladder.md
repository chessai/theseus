# ADR-0032: The idealization ladder (B3, formalized)

**Status:** Accepted (consolidates staging decided across ADR-0023/0025/0028)

## Decision
Energy claims are always relative to a declared rung; every report
carries its rung stamp (with the ADR-0028 boundary declaration):

1. **Rung 1 (M3a/M3b/M4):** ideal piecewise-linear trapezoidal
   power-clock sources; tt corner; 27°C; leakage included, broken out.
2. **Rung 2:** source non-idealities -- finite source impedance,
   inter-phase skew/jitter sensitivity sweeps.
3. **Rung 3:** resonator model -- LC regeneration with realistic Q;
   circulated-vs-dissipated accounting extends to the clock network.
   (This rung is where comparisons to published measured systems become
   meaningful.)
4. **Rung 4 (M5):** extracted parasitics from layout; measured-silicon
   correlation.

Claims made on rung N are invalid as claims about rung M > N; the
report generator enforces the stamp. Rung 2/3 sweeps join the nightly
tier when reached.
