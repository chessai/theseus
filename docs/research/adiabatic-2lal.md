# 2LAL Adiabatic Logic — Topology, 4-Phase Timing, and Slope-Minus-1 Design Rules

> **Purpose.** This document is a working engineer's reference for designing
> fully-adiabatic 2LAL (Two-Level Adiabatic Logic) circuits that achieve the
> theoretical energy-vs-period slope of **−1** on a log-log plot. It exists
> because a dual-rail 4-phase 2LAL shift register in this repo is stuck at
> slope ≈ −0.4 — indicating a residual **static / crowbar DC path** that
> leaks constant power and flattens the curve. Everything below is oriented
> toward eliminating that path.

---

## 1  What Makes a Logic Family "Adiabatic"

### 1.1  The Fundamental Mechanism

In conventional CMOS, a logic transition dumps charge from VDD through the
pull-up network into the output capacitance C_L, dissipating ½·C_L·V² per
transition regardless of clock period T.  Energy per cycle is **constant** →
log-log slope = **0**.

In an adiabatic circuit the charge is delivered (and later recovered) through a
**slowly ramping power-clock** φ(t).  The current through the channel
resistance R is set by dφ/dt, not by a sudden rail-to-rail step:

```
  I(t) ≈ C_L · (dφ/dt)  ≈  C_L · V_DD / T_ramp
```

The I²R dissipation integrated over the ramp is:

```
  E_diss  =  ∫₀^{T_ramp} I²·R dt
           =  (R · C_L² · V_DD²) / T_ramp
           =  (RC / T_ramp) · C_L · V_DD²
```

Because T_ramp is proportional to the clock period T, the dissipated energy
scales as **1/T** → log-log slope = **−1**.  This is the defining signature of
a fully-adiabatic family.

### 1.2  Charge Recovery

The slope −1 result requires that charge stored on C_L is **recovered** back
into the power-clock source on its down-ramp, not dumped to ground. If ANY
path to ground conducts while the node is charged, the charge on that path is
lost (non-recoverable), contributing a **constant** energy floor independent
of T — which degrades the slope toward 0.

### 1.3  Taxonomy of Adiabatic Families

| Family | Charge delivery | Charge recovery | Non-recoverable floor | Asymptotic slope |
|--------|----------------|-----------------|----------------------|-----------------|
| Static CMOS | Step (VDD rail) | None (dumped to GND) | ½CV² | 0 |
| ECRL (Efficient Charge Recovery Logic, Moon & Jeong) | Ramped (power-clock) | Partial (cross-coupled PMOS latch pulls high, but eval NMOS creates a brief VDD→GND path during evaluation) | Non-zero (crowbar during eval) | Between −1 and 0 |
| PFAL (Positive Feedback Adiabatic Logic, Vetuli/Pascoli/Reyneri) | Ramped (power-clock) | Partial (similar cross-coupled structure; residual crowbar through eval tree) | Non-zero | Between −1 and 0 |
| **2LAL** (Athas & Svensson, later Koller & Athas) | Ramped (power-clock) | **Full** (transmission-gate topology; no static VDD-to-GND path if correctly designed) | **Zero** (ideally) | **−1** |
| SCRL (Split-Level Charge Recovery Logic, Younis & Knight) | Ramped (power-clock) | Full (stepwise charge recovery across multiple voltage levels) | Zero (ideally) | −1 |

The critical distinction: **ECRL and PFAL** use cross-coupled PMOS latches
powered by the same clock that drives the evaluation NMOS tree. During
evaluation, there is inevitably a transient window where both the pull-up
(through the latch) and pull-down (through the eval network) conduct
simultaneously — a brief crowbar current that dissipates a fixed quantum of
energy per cycle. This creates an irreducible floor, bending the slope away
from −1 at long periods.

**2LAL avoids this** by using transmission gates (pass transistors) for
signal steering, with the power-clock controlling only the charge
injection/recovery — there is no complementary pull-up/pull-down pair that can
form a DC path. **But this guarantee holds only if the circuit is built
correctly** — which is the subject of the rest of this document.

---

## 2  The Canonical 2LAL Cell

### 2.1  Dual-Rail Encoding

2LAL is a **dual-rail** logic family. Every signal is represented by a
complementary pair (Q, Q̄):

- **Valid-1:** Q = high, Q̄ = low
- **Valid-0:** Q = low, Q̄ = high
- **Empty/Reset:** Q = low, Q̄ = low (both discharged — the "spacer" state)

The empty state is essential: it ensures that when a stage evaluates, its
inputs are already settled and valid (no glitching), and when a stage resets,
charge is returned to the clock smoothly.

### 2.2  Transmission-Gate Topology

The core 2LAL buffer/inverter stage consists of:

```
        φ_k (power-clock phase k)
        │
        ┣━━━━━━━━━━━━━━━━━━━━━━┓
        │                      │
       ┌┴┐  TG1              ┌┴┐  TG2
  A ──►│  │──► Q         Ā ──►│  │──► Q̄
       └┬┘                   └┬┘
        │                      │
        ┗━━━━━━━━━━━━━━━━━━━━━━┛
                  │
                 GND (only during reset, through φ_k down-ramp)
```

More precisely, each transmission gate (TG) is an NMOS/PMOS pair controlled by
the **input** signal and its complement:

- **TG1** passes φ_k to output Q when A is high (A gates the TG).
- **TG2** passes φ_k to output Q̄ when Ā is high.

When A = 1 (Ā = 0): TG1 conducts, so Q tracks φ_k (charges up on the ramp,
discharges on the down-ramp). TG2 is off, so Q̄ stays low. The output is
valid-1.

When A = 0 (Ā = 1): Symmetric. Q̄ tracks φ_k, Q stays low. Valid-0.

When the stage is in its **empty** phase (both inputs low, between active
clock phases): Both TGs are off. Outputs hold their state briefly on gate
capacitance, then are either already discharged or will be recovered on the
next down-ramp of their driving clock.

**Key insight:** There is no VDD rail. There is no pull-up or pull-down
network. The power-clock φ_k is the ONLY energy source, and charge flows
through TGs whose on/off state is set by the (already-valid) input data. No
combination of input values creates a path from φ_k to GND through the
evaluation logic, because TGs are unidirectional steering elements — they
connect φ_k to Q *or* Q̄, never to ground.

### 2.3  The Restoring Latch (and Where It Goes Wrong)

In practice, a bare TG stage has weak output levels and is vulnerable to
charge sharing and noise. Canonical 2LAL adds a **cross-coupled inverter
pair** (a latch) on the outputs to restore logic levels:

```
        Q ──┬──►│>o──┬── Q̄
            │        │
            └──o<│◄──┘
```

**CRITICAL:** This latch must be powered from the **power-clock φ_k**, NOT
from a static VDD rail. If the latch's supply is a fixed VDD:

1. During evaluation, the latch fights the TG (the TG is trying to pull one
   output high via the ramp, the latch's cross-coupled inverter is trying to
   hold the old state via VDD) → crowbar current from VDD to GND through the
   latch inverter and the TG.

2. During the empty/reset phase, the latch holds the outputs at VDD-level via
   the static rail, preventing charge recovery on the down-ramp of φ_k.

Both paths create **constant** per-cycle dissipation independent of T. This is
the single most common cause of slope degradation in 2LAL implementations.

**The fix:** Power the latch from φ_k. When φ_k ramps up, the latch
strengthens and restores levels. When φ_k ramps down, the latch weakens and
the outputs discharge back through the TGs into φ_k (charge recovery). When
φ_k is at ground, the latch is unpowered and both outputs are at ground
(empty state).

### 2.4  Building a Shift Stage

A 2LAL shift register is a pipeline of buffer/latch stages, each clocked by
a different phase of the 4-phase power-clock:

```
  Stage 0    Stage 1    Stage 2    Stage 3    Stage 4 ...
  (φ_0)      (φ_1)      (φ_2)      (φ_3)      (φ_0)  ...
  ───►        ───►        ───►        ───►        ───►
```

The "two-level" name refers to the fact that each stage has two voltage levels
in its operation: it ramps from GND to V_DD (evaluation) and back from V_DD
to GND (recovery), with no intermediate hold at V_DD from a static supply.

Data propagates one stage per quarter-cycle (one clock phase). After four
stages, a full clock cycle has elapsed and data has moved four positions. This
is the fundamental 2LAL pipeline throughput.

---

## 3  The 4-Phase Clock Schedule

### 3.1  Phase Definitions

The four power-clock phases φ_0, φ_1, φ_2, φ_3 are offset by exactly **90°**
(one quarter of the clock period T):

```
  φ_0:  ─────╱‾‾‾‾╲─────────────────────╱‾‾‾‾╲───
  φ_1:  ──────────╱‾‾‾‾╲─────────────────────╱‾‾‾
  φ_2:  ───────────────╱‾‾‾‾╲────────────────────╱
  φ_3:  ╲─────────────────────╱‾‾‾‾╲─────────────
        │    T/4   │    T/4   │    T/4   │   T/4  │
```

Each phase has a **trapezoidal** shape within its quarter-cycle:

```
  One phase waveform over a full period T:

  V_DD ─          ┌──────────┐
                 ╱            ╲
            ────╱              ╲──────────────────────────
              ↑    ↑        ↑    ↑
            t_rise hold   t_fall  rest (at GND)
            (ramp)        (ramp)  (idle)
```

The waveform has four sub-intervals within each period T:

| Sub-interval | Duration | φ voltage | Purpose |
|-------------|----------|-----------|---------|
| **Ramp up** | t_rise | GND → V_DD | Adiabatic charge injection through TGs |
| **Hold high** | t_hold | V_DD | Data is valid; downstream stage may evaluate |
| **Ramp down** | t_fall | V_DD → GND | Adiabatic charge recovery back into clock |
| **Rest (idle)** | t_rest | GND | Stage is empty; outputs at GND |

Typical allocation (symmetric trapezoid): t_rise = t_fall = T/8, t_hold =
t_rest = T/8, so each quarter-cycle = t_rise + t_hold = T/4. But the exact
allocation is a design parameter — see §5.

### 3.2  The Non-Overlapping Discipline

**This is the single most important timing constraint in 2LAL.** The
non-overlapping rule states:

> **A stage's power-clock must not begin its up-ramp until the PREVIOUS
> stage's power-clock has completed its up-ramp AND the previous stage's
> outputs are valid (settled).**

And symmetrically:

> **A stage's power-clock must not begin its down-ramp until the NEXT
> stage's power-clock has completed its up-ramp (so the next stage has
> captured the data).**

In terms of adjacent phases φ_k and φ_{k+1}:

1. **φ_{k+1} up-ramp starts after φ_k reaches V_DD** (or at least after φ_k
   is high enough that Stage k's outputs are valid). This ensures Stage k+1's
   TGs see valid input data during evaluation — no glitch, no partial
   evaluation, no crowbar through fighting paths.

2. **φ_k down-ramp starts after φ_{k+1} reaches V_DD** (or at least after
   Stage k+1 has captured the data). This ensures Stage k's charge is not
   pulled back before Stage k+1 is done reading it.

3. **Dead time:** There must be a gap (dead time) between the falling edge of
   one phase and the rising edge of the non-adjacent phase two positions
   later (φ_k fall and φ_{k+2} rise). This prevents charge-sharing paths
   through intermediate stages.

### 3.3  What Happens If Phases Overlap

If φ_k and φ_{k+1} are both ramping simultaneously:

- Stage k's TGs are conducting (φ_k provides charge).
- Stage k+1's TGs are also conducting (φ_{k+1} is ramping).
- If Stage k's outputs are not yet valid, Stage k+1's TGs may conduct in a
  state where both dual-rail outputs are partially high → the cross-coupled
  latch (if present) has a path from φ_{k+1} through one side of the latch
  to ground through the other TG → **crowbar current**.

Even without a latch, overlap can cause: charge dumped from φ_k through
Stage k's TG into Stage k+1's TG and out through φ_{k+1}'s return path — a
conductive path between two clock phases through the signal chain, dissipating
energy that is proportional to the overlap window, not to 1/T.

---

## 4  Crowbar / Static-Power Pitfalls

This section enumerates every known mechanism by which a 2LAL stage can
develop a DC path and leak constant power (degrading slope from −1 toward 0).
For each, the standard fix is given.

### 4.1  VDD-Powered Restoring Latch

**Mechanism:** The cross-coupled inverter pair that restores output levels is
connected to a fixed VDD supply instead of the power-clock φ_k.

- During evaluation: one latch inverter fights the TG → VDD → TG → latch →
  GND path.
- During reset: latch holds outputs high via VDD → charge cannot be recovered
  on φ_k down-ramp. Energy ½CV² is dumped to GND each cycle.

**Symptom:** Per-cycle energy has a large constant floor. Slope ≈ 0 at long T.

**Fix:** Power the latch from φ_k. The latch supply rises and falls with the
clock, so it assists evaluation (strengthens the winning side) and releases
charge on the down-ramp (both sides decay to zero together).

### 4.2  Incomplete Transmission-Gate Turn-Off

**Mechanism:** The TGs are controlled by the input data signals. If the input
dual-rail signals have slow transitions or do not reach full GND on the
"off" rail, the nominally-off TG has non-zero leakage or is weakly on.

- A weakly-on TG creates a resistive path from φ_k to the output of the
  WRONG rail → the latch now has both outputs partially driven → crowbar
  through the cross-coupled pair.

**Symptom:** Slope degrades, especially at long T where leakage has more time
to act. Energy floor that scales weakly with T (sub-linear).

**Fix:**
- Ensure the previous stage's outputs swing fully rail-to-rail (GND when off,
  V_DD when on). This requires the previous stage's restoring latch to be
  strong enough.
- Size the TG PMOS and NMOS so that the TG is fully off when the gate is at
  GND. Specifically: V_GS(NMOS) = 0 → off; V_GS(PMOS) = 0 → off. The input
  "off" rail must be at true GND, not a floating or partially charged node.
- Consider adding a small **bleeder** or **keeper** that pulls the off-rail
  firmly to GND, but power the bleeder from the clock (not VDD!).

### 4.3  Phase Overlap Windows

**Mechanism:** Adjacent power-clock phases φ_k and φ_{k+1} overlap in their
active (non-zero) intervals. During overlap, both Stage k and Stage k+1 have
active (conducting) TGs and powered latches simultaneously.

- If data has not fully propagated, both TGs in Stage k+1 may be partially
  on → crowbar in the latch.
- Even with valid data, if both φ_k and φ_{k+1} are high, there is a
  continuous conductive path from φ_{k+1} through Stage k+1's TG, through the
  interconnect, backward through Stage k's TG, into φ_k → inter-phase
  current that dissipates energy in the TG on-resistances.

**Symptom:** Constant energy per cycle at long T. The dissipation scales with
the overlap duration, not 1/T.

**Fix:**
- Insert **dead time** (non-overlapping gap) between the falling edge of φ_k
  and the rising edge of φ_{k+1}. A common rule of thumb: dead time ≥
  t_rise/2.
- Use a clock generator that guarantees non-overlap by construction (e.g.,
  a ring of inverter delays on the enable signals for each phase driver).
- Verify in SPICE that no two adjacent phases are simultaneously above V_th
  at any point in the cycle.

### 4.4  Body Diode / Substrate Leakage

**Mechanism:** In bulk CMOS, the source/drain-to-substrate (or well) junctions
form parasitic diodes. When the power-clock swings below GND (due to
undershoot or coupling), these diodes forward-bias and dump charge into the
substrate.

**Symptom:** Small constant energy floor. Often masked by larger effects.

**Fix:**
- Ensure clock waveforms do not undershoot below GND (use controlled ramp
  generators, not resonant LC tanks without clamping).
- Use SOI or FinFET processes where substrate coupling is minimal.
- In bulk CMOS, tie body terminals to the appropriate bias (NMOS body to GND,
  PMOS body to the clock φ_k if the PMOS is in the TG, or to VDD if
  applicable).

### 4.5  Charge Sharing on Internal Nodes

**Mechanism:** In multi-input logic functions (AND, OR — built from cascaded
TGs), internal nodes between TGs can trap charge. When the clock ramps, the
trapped charge is dumped rather than recovered.

**Symptom:** Small constant energy per cycle per internal node. Worsens with
logic depth.

**Fix:**
- Minimize cascaded TG depth (prefer wide, flat logic trees).
- Add explicit **precharge/discharge paths** from internal nodes to the clock
  (not to VDD/GND rails) so that trapped charge is recovered adiabatically.
- In a shift register (which has no internal cascaded logic — just buffers),
  this is typically not the dominant issue.

### 4.6  Non-Adiabatic Reset Path

**Mechanism:** The stage has a separate reset or initialization path (e.g.,
an NMOS tied to GND with a reset signal) that discharges the output
non-adiabatically. If this path is active during normal operation (even
briefly, due to glitches on the reset line), it dumps charge directly to GND.

**Symptom:** Constant energy floor. May be intermittent.

**Fix:**
- Remove dedicated reset transistors from the data path entirely if possible.
  Use the 4-phase clock's natural empty state for reset.
- If a hard reset is needed, gate it with a one-shot signal and ensure it is
  fully deasserted during normal clock operation.

---

## 5  SPICE-Level Design Rules for Slope −1

### 5.1  Device Sizing

**Transmission gates (signal-steering TGs):**
- Size for adequate on-conductance: the RC time constant of the TG
  (R_on · C_load) must be **much less than** the ramp time t_rise.
  Rule of thumb: R_on · C_load ≤ t_rise / 10.
- This ensures the output tracks the clock ramp without significant voltage
  drop across the TG, keeping I²R losses proportional to 1/T_ramp.
- If R_on · C_load ≈ t_rise, the TG acts as a significant resistor and the
  output lags the clock. The dissipation is still ∝ 1/T in principle, but
  the coefficient is large and signal integrity suffers.
- Typical NMOS/PMOS width ratio in the TG: W_n/W_p ≈ 2–3 (same as for a
  balanced CMOS TG, depending on mobility ratio).

**Restoring latch (cross-coupled inverters):**
- Size **weak** relative to the TGs. The latch's job is level restoration and
  noise rejection, not driving the output. If the latch is too strong, it
  fights the TG during transitions → excess dissipation (not necessarily a DC
  path, but increases the 1/T coefficient).
- Rule of thumb: latch inverter drive strength ≤ 1/5 of TG conductance.
- Power the latch from φ_k (§4.1).

**Input-stage sizing:**
- The gates of the TGs are driven by the previous stage's outputs. The
  previous stage must be sized to drive the TG gate capacitance without
  excessive delay. In a shift register, all stages are identical, so this is
  self-consistent — just verify that the stage's output can drive the next
  stage's TG gates within the timing margin.

### 5.2  Clock Trapezoid Edge Shaping

The ramp time t_rise (= t_fall for a symmetric trapezoid) directly controls
the adiabatic dissipation:

```
  E_adiabatic = (R_on · C_load · V_DD²) / t_rise
```

**Design trade-off:**
- Longer t_rise → less dissipation, but less time for the hold phase (data
  valid window shrinks) and more sensitivity to noise/leakage during the long
  ramp.
- Shorter t_rise → more dissipation (approaching ½CV² in the limit of a step),
  but more timing margin.

**Practical allocation for a 4-phase clock with period T:**
- Each phase occupies T/4 of the period.
- Within that T/4: t_rise + t_hold = T/4 (for the active half); t_fall +
  t_rest = T/4 (for the recovery half). Wait — more precisely, each phase's
  full cycle is T (it repeats every T), and within T the phase is active for
  roughly T/4 and idle for 3T/4. A common split:
  - t_rise = T/8
  - t_hold = T/8
  - t_fall = T/8
  - t_rest = 5T/8 (but only T/8 matters locally; the rest is dead time
    between this phase and its next activation)

  Effectively, within the active quarter-cycle of a phase: ramp for half of
  T/4, hold for the other half. This gives t_rise = T/8.

**Edge shape:**
- A linear ramp is simplest and gives the cleanest 1/T scaling.
- A sinusoidal ramp (from an LC resonant clock generator) works well and is
  easier to generate at high frequencies. The dissipation formula gains a
  constant factor (π²/8 for a half-sine vs. a linear ramp) but the 1/T
  scaling is preserved.
- Avoid step edges (abrupt transitions) — these create non-adiabatic ½CV²
  charging events.

### 5.3  Measuring Per-Cycle Energy and Separating Components

**Measurement setup in SPICE:**

1. **Settle the pipeline.** Run for at least 8–10 full clock cycles before
   measuring, to eliminate startup transients. With a shift register, fill it
   with a repeating pattern (e.g., alternating 0-1) so every stage is
   switching every cycle.

2. **Measure instantaneous power from each clock phase:**
   ```
   P_k(t) = V_φk(t) · I_φk(t)
   ```
   where I_φk is the current sourced by the φ_k generator. Positive I when
   the generator sources current (charging); negative I when it sinks current
   (recovering charge).

3. **Integrate over exactly one clock period T (after settling):**
   ```
   E_cycle = ∫_t0^{t0+T} Σ_k [V_φk(t) · I_φk(t)] dt
   ```
   This gives the net energy delivered by all four clock phases in one cycle.
   Recovered charge (negative P during down-ramp) is automatically subtracted.

4. **Sweep T** (the clock period) over at least 2 decades (e.g., 1 ns to
   100 ns). Plot E_cycle vs. T on a log-log scale.

5. **Fit the data** to the model:
   ```
   E_cycle = E_static + E_adiabatic
           = P_leak · T  +  α · (RC/T) · CV²
   ```
   - If P_leak = 0: E_cycle ∝ 1/T → slope = −1. ✓
   - If P_leak > 0: at large T, E_cycle → P_leak · T (slope = +1 from
     leakage), and the log-log curve has a minimum. At moderate T, the
     curve flattens (slope approaches 0, or −0.4, or wherever the static
     and adiabatic terms are comparable).

   **A measured slope of −0.4 means the static floor is comparable to the
   adiabatic term across your measurement range.** You haven't yet reached
   the regime where E_adiabatic dominates.

6. **Isolate the static component:** measure at very long T (e.g., T = 1 µs
   or more). If E_cycle stops decreasing and starts increasing linearly with
   T, you have confirmed a DC power draw P_leak = E_cycle / T.

7. **Localize the source:** replace each stage's power-clock connection one
   at a time with an ideal (zero-resistance) source, or probe I_φk for each
   stage individually. The stage with anomalous DC current is the culprit.

### 5.4  Confirming Full-Level Dual-Rail Restoration

After each clock cycle, verify that every stage's outputs satisfy:

- **During hold (φ_k = V_DD):** One rail is at V_DD (within 5%), the other is
  at GND (within 50 mV or V_th/10).
- **During empty (φ_k = GND):** Both rails are at GND (within 50 mV).

If the "off" rail does not reach true GND, the TGs controlled by it are
partially on → leakage path (§4.2). If the "on" rail does not reach full V_DD,
the downstream TGs are partially on → weak drive → slow propagation → timing
violations.

**Multi-stage check:** Propagate a known pattern through N stages (N ≥ 16 for
a shift register) and verify that the output waveform at Stage N is a
time-shifted, clean copy of the input at Stage 0. Any degradation (shrinking
swing, rising baseline, asymmetric rise/fall) indicates a systematic issue
that compounds across stages.

---

## 6  Debugging Checklist: "My Slope Is −0.4"

Work through this list in order. Each item is a specific, testable hypothesis.
Stop when you find the cause.

### Step 1: Check the restoring latch supply

- [ ] **Is the cross-coupled latch powered from a FIXED VDD rail?**
  If yes: this is almost certainly the dominant issue. Reconnect the latch
  supply to the stage's power-clock φ_k. Expect slope to improve dramatically.

- [ ] **Is there ANY connection from a static VDD or GND rail to the signal
  path** (other than substrate ties and bulk connections)? Search the netlist
  for any wire from VDD to a node in the dual-rail data path. Any such
  connection is a potential DC leak.

### Step 2: Check for phase overlap

- [ ] **Plot all four clock phases on the same time axis.** Verify that
  adjacent phases (φ_k and φ_{k+1}) are NEVER simultaneously above V_th.

- [ ] **Measure dead time** between the falling edge of φ_k and the rising
  edge of φ_{k+1}. If dead time < 0 (overlap), insert delay. If dead time
  is marginal (< t_rise/4), increase it.

- [ ] **Check non-adjacent phases too** (φ_0 and φ_2): with a 4-stage
  pipeline, charge can leak through two stages if both their clocks are
  simultaneously active.

### Step 3: Check dual-rail signal integrity

- [ ] **Probe the "off" rail of each stage during evaluation.** Is it at
  true GND (< 50 mV), or is it floating at some intermediate voltage?

- [ ] **If the off-rail is not at GND:** trace why. Common causes:
  - Previous stage's latch is too weak to pull the off-rail fully down.
  - Charge trapped on the off-rail from a previous cycle (incomplete
    recovery).
  - Capacitive coupling from the on-rail or the clock.

- [ ] **Fix:** Strengthen the previous stage's latch (while keeping it
  clock-powered), or add a clock-powered bleeder on the off-rail.

### Step 4: Check for parasitic DC paths in the netlist

- [ ] **Run a DC operating point** with all clocks held at V_DD/2 (mid-ramp).
  Measure the total current from all clock sources. If any source shows
  significant DC current, there is a resistive path from that clock to
  ground (or to another clock).

- [ ] **Trace the path:** use SPICE's `.op` analysis to find which devices
  are conducting. Look for MOSFETs that are on when they should be off.

### Step 5: Verify the clock waveform itself

- [ ] **Is the clock truly trapezoidal with LINEAR ramps?** Ringing,
  overshoot, or undershoot on the clock edges can forward-bias parasitic
  junctions or momentarily create unexpected conduction paths.

- [ ] **Is the clock generator's output impedance negligible** compared to
  the TG on-resistance? If the clock source has significant impedance, the
  effective ramp at the stage is distorted, and charge recovery is incomplete.

### Step 6: Quantify the static floor

- [ ] **Sweep T from 1 ns to 10 µs** (or as wide a range as feasible).
  Plot E_cycle vs. T on log-log. Fit to `E = A/T + B·T + C` (adiabatic +
  leakage + constant). If B > 0, you have a DC current somewhere. If C > 0,
  you have a fixed non-recoverable charge dump per cycle.

- [ ] **Compute P_static = B** (the leakage power). Compare to the total
  power at your target operating point. If P_static > 10% of total, the
  slope will be significantly degraded.

### Step 7: Stage-by-stage isolation

- [ ] **Measure per-stage energy** by integrating V·I for each clock phase
  individually. One or two stages will likely dominate the static floor.

- [ ] **Replace the suspect stage with an ideal buffer** (SPICE voltage
  source that mimics perfect 2LAL output). If the static floor drops, the
  suspect stage is confirmed.

- [ ] **Inspect that stage's netlist** for any of the issues in §4.

---

## 7  Canonical References

- **Athas, W.C. and Svensson, L.J.** — "Reversible Logic Issues in
  Adiabatic CMOS" (1994). Foundational paper on 2LAL-style adiabatic
  transmission-gate logic. Establishes the RC/T energy scaling and
  dual-rail 4-phase pipeline structure.

- **Athas, W.C., Svensson, L.J., Koller, J.G., Tzartzanis, N., and
  Chou, E.Y-C.** — "Low-Power Digital Systems Based on Adiabatic-Switching
  Principles" (IEEE T-VLSI, 1994). Detailed design and measurement of 2LAL
  shift registers and adders. Demonstrates slope −1 in silicon.

- **Koller, J.G. and Athas, W.C.** — "Adiabatic Switching, Signal
  Energies, and the Energy-Time Product" (1992). Theoretical foundation for
  the energy-time trade-off in adiabatic circuits.

- **Frank, M.P.** — Work on reversible/adiabatic computing foundations.
  Clarifies thermodynamic limits and the distinction between logically
  reversible and physically adiabatic dissipation.

- **Younis, S.G. and Knight, T.F.** — "Asymptotically Zero Energy Split-Level
  Charge Recovery Logic" (1994). Introduces SCRL, a related fully-adiabatic
  family using stepwise charge recovery. Useful for comparison and for
  understanding the role of intermediate voltage levels.

- **Moon, Y. and Jeong, D-K.** — "An Efficient Charge Recovery Logic Circuit"
  (JSSC, 1996). Introduces ECRL. Demonstrates the quasi-adiabatic approach
  and its inherent non-recoverable floor — a useful contrast to 2LAL.

- **Vetuli, A., Pascoli, S., and Reyneri, L.** — "Positive Feedback in
  Adiabatic Logic" (Electronics Letters, 1996). Introduces PFAL. Similar
  quasi-adiabatic trade-offs to ECRL.

---

## 8  Summary of Key Design Rules

| Rule | Rationale |
|------|-----------|
| Power the restoring latch from the power-clock, NEVER from static VDD | Eliminates the dominant DC path (§4.1) |
| Non-overlapping 4-phase clocks with dead time ≥ t_rise/2 | Prevents inter-phase crowbar current (§4.3) |
| Dual-rail off-side must reach true GND | Ensures TG is fully off; prevents partial conduction (§4.2) |
| R_on · C_load ≤ t_rise / 10 | Keeps output tracking the ramp; controls I²R coefficient (§5.1) |
| Latch strength ≤ 1/5 of TG conductance | Prevents latch from fighting TG during transitions (§5.1) |
| No static VDD or GND connections in the data path | Any static rail is a potential DC leak source (§4.1, §4.6) |
| Linear or sinusoidal ramp edges; no step edges | Preserves adiabatic 1/T scaling; avoids ½CV² events (§5.2) |
| Measure E_cycle over settled cycles; sweep T over ≥ 2 decades | Required to separate 1/T (adiabatic) from constant (static) (§5.3) |
| Verify full-swing restoration across ≥ 16 pipeline stages | Catches systematic degradation that compounds (§5.4) |
