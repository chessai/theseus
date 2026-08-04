# ADR-0033: Recirculating execution -- the ring machine (M4-v2)

**Status:** Accepted (design; implementation is M4-v2)

## Context

M4-v1 delivered a CPU whose T-cycle run is `step` applied T times -- at
the *value* level (an eval loop in the interpreter). The hardware story
is unroll-only (ADR-0026 C2.4): the phased cell netlist is a pure
feed-forward DAG per step, and running T steps as a circuit means T
spatial copies of the step.

M4-v1's energy/area report is the formal declaration of C2.4's wake
condition, exactly as ADR-0030 said it would be ("M4 formally declares
C2.4's wake condition"). The numbers (status.md, real rung0 char DB):

- one **balanced** `step` ≈ 4.8 k active cells + **~1.6 M K0 buffers**
  (98% of cells are buffers riding the 320 State rails -- "holding costs
  motion", C2.1); materialized, tens of GB of host RAM;
- `step^T` at the modest M4-v1 scale T = 64 ≈ **10⁸ cells** -- not
  merely unaffordable silicon, *unrepresentable* in memory;
- T is a compile-time constant, so data-dependent run length (the whole
  point of ADR-0034's branching) is inexpressible under unroll.

**Unrolled area is unaffordable. C2.4 is awake. M4-v2 is the ring
machine:** recirculate one step's hardware instead of copying it.

## Decision

- **D1 -- Recirculation is a backend/artifact construct, not a core Π
  combinator.** No `Recirculate`/`RingRun` is added to `Iso`
  (theseus-core). Rationale: recirculation has *no new denotation* --
  `run(T) = step^T` is already expressible as `Seq`, and value-level
  loops already have `Trace`; a core combinator would change costs, not
  meaning, violating ADR-0011/0013's "structure in the IR, costs in the
  backends" and C2.4/C2.8's framing of loop policy as a TargetProfile
  choice. The ring is a **new artifact one level above the phased cell
  netlist**:

  ```text
  RingNetlist {
      body:  CompactNetlist,   // one balanced step (D4)
      wrap:  output_i → input_i (identity port bijection, type-aligned),
      // revolutions R live in the driver/sequencer, not here (D3)
  }
  ```

  The driver-level entry point (`theseus-tisc`) is
  `ring(rom) : RingNetlist` + `run_ring(ring, state0, R)`; the
  differential oracle for it is `theseus_eval` applying `step` R times
  -- the existing verification norm, unchanged.

- **D2 -- Loop closure: the wrap edge lives *outside* the DAG; the
  closure condition is `D ≡ 0 (mod P)`.** `PhasedCellNetlist` stays a
  DAG -- no feedback edges are added to it. Every existing analysis
  (`line_arrivals`, `validate`, `balance`, `sim`) assumes acyclicity
  and is reused on the body unchanged; the ring validator adds four
  checks *about the boundary*:

  1. the body validates as today;
  2. **uniform output depth**: all output ports arrive at one tick D
     (the balancer equalizes cell inputs, not output ports -- the ring
     padder appends K0 chains to flush every output to
     `D = max(output_depths)`);
  3. **phase closure**: `D ≡ 0 (mod P)`. Phase is an attribute of a
     physical cell (`phase = depth mod P`, C2.3) and the 4-phase clock
     is already periodic; tick D drives the same trapezoids as tick 0
     iff P | D. So the phase assignment neither wraps nor changes:
     the *clock sequencer's periodicity* is what makes the feedback
     electrically legal, and the netlist only has to land on a phase
     boundary. Round D up with ≤ P−1 extra K0 stages per rail
     (trivially cheap in the compact representation, D4);
  4. the wrap map is a type-aligned identity on ports: `input_ty =
     output_ty = State`, width-equal (a bijection `State ↔ State` --
     which `step` is by construction).

  Under 1-4, revolution r of the ring is *token-for-token identical* to
  copy r of the unrolled circuit; semantics preservation reduces to the
  already-verified body.

  **Injection/extraction** is modal, per the C2.5 precedent (direction
  is a chip-level clocking choice): the sequencer loads State by
  scan-in on the same rails before starting the rotation and scans out
  after R revolutions -- 2LAL logic *is* a shift register, so load/drain
  need zero added cells. Fallback if B5-era electrical reality
  disagrees: a K1-gated injection column (controlled swap between the
  wrap rail and an external rail, keyed on a sequencer-driven load
  flag), which costs 320 K1 cells and one extra tick.

- **D3 -- The revolution count and halt live in the sequencer, not in
  State.** The run is `step^R` for an **externally fixed, finite R**
  supplied by the driver/sequencer. Totality is preserved in exactly
  ADR-0007's shape: `step` is a total bijection, so `step^R` is a total
  bijection for every finite R; the bound is static and supplied, not
  discovered at runtime (ADR-0007's consequence "unroll-vs-counter is a
  backend choice on a known bound" -- the ring *is* the counter arm).
  For programs compiled from finite-carrier traces, R is the trace
  bound ≤ |u| (TH-1/WCET); for the CPU as an interactive machine, R is
  the operator's dial.

  **Halt is a closed control orbit**, not a sequencer interrupt -- and
  not a fixpoint, because a total bijection admits *no reachable
  fixpoint*: a fixpoint's unique preimage is itself, so a running
  program can never fall into one (pigeonhole on preimages). The
  honest reversible halt is a short idle *cycle* on which the data
  registers are invariant. Under ADR-0034's PC-update rule
  (`PC += BR + 1 mod 256`), the `HALT` convention is the period-2
  orbit: addresses `a−1` and `a` both hold `SWAPBR rh` with `rh`
  preloaded `0xFE`; entering at `a` with `BR = 0` ping-pongs
  `(PC=a, BR=0, rh=FE) ↔ (PC=a−1, BR=FE, rh=0)` forever, moving only
  {PC, BR, rh} (ADR-0034 D5 derives this). Consequently the ring's
  answer -- read from the answer registers, modulo the designated halt
  register -- is **independent of R for all R ≥ T_halt**: R is an upper bound, not an exact count, and
  over-running is harmless (it costs energy, not correctness). An
  in-State cycle counter is rejected: it adds ≥ 8 always-moving rails
  (storage is motion, C2.1) and re-imports the uncompute-the-counter
  problem the halt orbit dissolves.

  Reverse execution: the adjoint ring is the same body driven by the
  reversed phase sequence for R revolutions (C2.5, modal) -- or
  mirrored-netlist emission as the recorded fallback.

- **D4 -- The compact balancer is a hard prerequisite.** Recirculation
  requires the body to be **materialized once** -- it *is* the machine.
  Today that is impossible: `balance()` on one step materializes ~1.6 M
  K0 cells (tens of GB); M4-v1 only ever *counted* them analytically
  (`theseus-tisc::energy`, validated against `balance()` exactly). The
  `balance_compact` work (in flight in parallel) stores K0 runs as
  **delay counts on edges** -- `(pair, n)` meaning "n ticks of K0" with
  no materialized cells -- making the balanced body O(active cells +
  edges) ≈ MBs. The ring consumes this representation directly:

  - the body artifact of D1 is the compact netlist, not the
    materialized one;
  - the D2 padder (output flush + round-up to P | D) just increments
    delay counts;
  - the ring simulator models a delay-n edge as a length-n FIFO of
    in-flight tokens (n tokens resident, one per tick), which is also
    exactly what the per-tick INV-TOKEN assertion and the occupancy/
    energy accounting need;
  - energy per revolution = the existing analytic breakdown, unchanged.

  Without `balance_compact` the recirculated body cannot be built,
  validated, or simulated; with it, one step fits in memory and the
  ring closes over it.

## Observable difference vs unroll

- **Area:** O(|step|) instead of O(T·|step|) -- at T = 64, ~10⁸ cells →
  ~1.6 M cell-equivalents (~64×), and the host-side artifact goes from
  unrepresentable to MBs (compact form).
- **Energy:** per revolution, *identical to one unrolled copy* to first
  order -- the same cells fire on the same tokens; energy tracks work,
  not area. (Second order: the unrolled form buffers all 320 rails
  across each copy seam; the ring replaces seam buffers with the wrap
  wires -- a small win.) The ring buys **feasibility and unbounded T**,
  not joules per op; the E(T) ∝ 1/t_ramp money-plot story is untouched.
- **Expressiveness:** T stops being a compile-time constant. With
  ADR-0034's branch-keyed step, run length becomes data-dependent
  (halt-fixpoint + generous R) -- the machine everyone imagines, which
  unroll cannot express at any area.

## Update -- zero-garbage ring closure (ADR-0040 phase C)

The R1-based `emit_mcnot` rewrite (ADR-0040 phase C) made the TISC-1
step's plain lowering ancilla-clean: `StepRing::ancillas_restored ==
true` on all sampled states, with ~468 ancilla lines (down from 1138).
`StepRing::run_true` -- feeding ALL ancilla outputs back as inputs,
re-supplying NOTHING -- equals `eval(step)^R`. The ring now physically
closes via the plain `StepRing` path with **zero per-revolution
boundary**: no externally refreshed pairs, no Bennett residue, no
garbage. This is the literal zero-garbage ring.

The Bennett `ClosedStepRing` (2× cells, 2× depth, `2·w = 320` boundary
per revolution) demotes to a **fallback** for circuits whose lowering is
still K-only-dirty (trace tag duplication, iter/fold control chains --
genuine fan-outs, not conditional NOTs). The `ClosedStepRing` and its
tests remain valid and are retained.

**Boundary comparison (TISC-1 step):**

| Path | Per-revolution boundary | Ancilla lines | Notes |
|------|------------------------|---------------|-------|
| `StepRing::run_true` (plain, R1) | **0** | ~468 (all restored) | Zero-garbage ring |
| `ClosedStepRing::run` (Bennett) | 320 (`2·w`) | forward + 320 boundary | Fallback for K-dirty circuits |

## Consequences

- theseus-netlist gains `ring` (RingNetlist, validator, padder,
  simulator over the compact body); theseus-core is untouched.
- ADR-0029's Option-B bus now has its design input: the bus is a second
  recirculating structure sharing the ring's sequencer discipline
  (`step : (CpuState, BusMsg) ↔ (CpuState, BusMsg')` with the wrap on
  both port groups). Option B's degeneracy argument dissolves once the
  ring exists -- as ADR-0029 predicted.
- The phased-sim's oracle chain gains one link: `run_ring(·, x, R)` ≡
  `eval(step)^R (x)`, checked over sampled State points (the 2^160
  domain norm from status.md).

## Alternatives rejected

- **Core `Recirculate` combinator** -- no new denotation; pollutes the
  algebra every pass must handle (D1).
- **Feedback edges inside `PhasedCellNetlist`** -- breaks the DAG
  assumption load-bearing in `line_arrivals`/`validate`/`balance`/
  `sim`; the wrap-outside-the-DAG design reuses all of them (D2).
- **In-State revolution counter** -- rails cost + uncompute problem;
  strictly dominated by sequencer-R + halt fixpoint (D3).
- **Materialized ring body** -- tens of GB; dead on arrival without the
  compact balancer (D4).
