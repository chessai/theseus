# The adiabatic backend

The third backend (after FPGA and the bit-level simulators), targeting
2LAL transmission-gate fabric in Sky130 (ADR-0023/0024). Its organizing
fact: **every gate takes one tick and every gate is a register** — the
combinational abstraction dissolves and the target is a systolic fabric
phase-rotated at single-gate granularity (ADR-0026).

Pipeline extension:

```text
Typed flat netlist ─cell-map→ Phased cell netlist ─deck-emit→ SPICE decks
        (ADR-0031 lemmas)      (phase = depth mod P,           (theseus-spice,
                                tick-exact balancing,           ADR-0025)
                                INV-TOKEN asserted)
```

Key disciplines: unroll-only loops (guaranteed possible by ADR-0007
totality); mandatory tick-exact balancing (mismatch = wrong circuit, and
every buffer-tick is priced energy); one cell — the k-controlled
dual-rail swap (ADR-0027); no data fanout (linearity), control fanout
priced by the characterization DB; validity-window protocol checked by
the phased-sim, with window arithmetic pinned empirically (C2.6).

Physical adjointness (reverse the phase sequence → adjoint on the same
silicon) is a validated design goal, TH-6, with mirrored-netlist
emission as fallback. Direction is modal — a chip-level clocking choice.

The oracle chain through this backend:
interpreter ≡ typed-sim ≡ bit-sim ≡ **phased-sim** ≡ ngspice (ADR-0025).
