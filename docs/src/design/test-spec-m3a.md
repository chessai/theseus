# Test specification: M3a (physics track)

Executable "what done looks like" for the cell library, harness, and
characterization DB. Numbers marked **(provisional, TBD-n)** are defaults
to build against until bench data justifies revision -- revising them is a
one-line PR against this file plus the TBD register, not a re-design.

## Per-cell admission checklist (ADR-0027 B4.3, made executable)

A cell enters the library only when all of the following pass in the
harness, for every (Vt, body) variant it ships:

1. **Functional verdict** via typed decode (B5.3): the cell's truth
   table reproduced at every swept (ramp, load) point -- decode
   thresholds at 0.3·VDD / 0.7·VDD with hysteresis, sampled mid-validity
   **(provisional, TBD-4)**.
2. **Conservation closure:** per-cycle source energy = dissipation +
   Δstored within **1% (provisional, TBD-1)**.
3. **Token conservation** (INV-TOKEN): asserted-rail count invariant at
   every sampled tick.
4. **Reverse drive** (TH-6): phase sequence reversed, adjoint truth
   table reproduced -- verdict recorded per cell even if it fails
   (failure routes to the mirrored-netlist fallback, not rejection).
5. **Adiabatic scaling:** the cell's own E(period) fits the ADR-0028
   model form with residuals within TH-E3 bounds **(fit R² ≥ 0.99 over
   the swept range; provisional, TBD-5)**, and leakage share is
   reported separately per ADR-0028 C3.3.

## Harness self-validation gates (run before any cell is believed)

- Analytic RC ramp structure: measured dissipation within **5%
  (provisional, TBD-2)** of the closed-form CV²·(RC/T)-family value at
  every swept point.
- Ideal-source sanity: zero net energy into an open circuit; source
  bookkeeping exact to solver tolerance.
- The canonical 2LAL shift register reproduces slope −1 on E/op vs
  period (canonical axes, ADR-0023) within ±0.1 over ≥2 decades -- the
  gate that says the harness measures the field's known result before
  it measures anything new.

## Bench DUT matrix (M3a scope, closed)

{buffer (k=0), 1-CSWAP, 2-CSWAP} × {std, lvt, hvt} × {body-to-rail,
deep-n-well} × ramp (≥3 decades, ≥5 points/decade) × load (min, 4×,
16× min) -- plus shift register (≥8 stages), CNOT chain (≥8), the
compiled reversible full adder, and the CMOS/PFAL baseline DUTs.

## Money-plot artifact contract

RON record per curve: `{ dut, family, axes: EnergyPerOpVsPeriod,
points: [(period_s, e_per_op_J)], rung, stamps: {pdk, simulator,
harness, db} }` -- INV-STAMP applies; the plot generator refuses
unstamped or rung-less curves. M3a's acceptance artifact is the
three-family plot from this data, criteria per ADR-0023.

## Characterization DB: done means

Every admitted cell has a complete sweep table + fit; DB rebuild from
raw measurements is deterministic; interpolation queries inside the
range succeed, one epsilon outside hard-error (ADR-0028 C3.1 -- a test,
not a hope); the DB diff for a re-run against unchanged inputs is empty.
