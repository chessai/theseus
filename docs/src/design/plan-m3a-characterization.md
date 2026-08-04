# Plan: M3a characterization pipeline (PDK-backed completion)

**Status:** scoping / not started. Authored 2026-07-11.
**Owns:** the gap between the *banked* M3a headline (the three-family money plot
on generic rung0 models) and the *full* `test-spec-m3a.md` acceptance (per-cell
admission + closed DUT matrix + characterization DB, on pinned sky130 cells).

> This is formally the **completion of M3a** -- `test-spec-m3a.md` lists the
> admission checklist, DUT matrix, and char-DB "done means" as M3a acceptance.
> It is large enough to run as its own phase; call it M3a-char. The money-plot
> artifact is banked regardless (ADR-0023 headline reproduced + stamped).

## 1. What's banked vs. what this phase delivers

**Banked (rung0-ideal-models):**
- `theseus-spice::{bench, adiabatic, artifact}` (~1100 LoC): ngspice-driven energy
  harness (`run_energy`/`sweep_period`/`log_log_slope`), self-validation gates 1&2
  (RC-analytic ±5%, ideal-source), three-family money plot + stamped RON artifact
  (`Rung::Rung0IdealModels`, INV-STAMP enforced), runnable example + golden render.

**Empty stubs this phase fills:** `theseus-spice::{cells, chardb, energy, phased,
deck}` (inline `pub mod ... {}` in `lib.rs`). No PDK, no admission, no DUT matrix,
no char DB, no sky130 cells exist today.

**Deliverable:** every `test-spec-m3a.md` criterion green on **rung1** (sky130A,
tt, 27 °C, ideal trapezoidal clocks, leakage broken out), i.e. the rung promotion
rung0 → rung1.

## 2. Rung promotion (the framing device)

Everything here is one rung step (ADR-0032):

| | rung0 (banked) | rung1 (this phase) |
|---|---|---|
| Models | generic level-1 MOSFET | sky130A `tt`, 1.8 V core, 27 °C |
| Clocks | ideal trapezoidal | ideal trapezoidal (unchanged) |
| Leakage | negligible/generic | real, **broken out separately** (ADR-0028 C3.3) |
| Cells | hand-fit TG/PFAL/CMOS primitives | admitted `/cells/*.subckt` library |
| Stamp | `rung0-ideal-models` | **add `Rung::Rung1Sky130Tt`** |

Concrete first code change: extend the `Rung` enum in `artifact.rs`
(`Rung1Sky130Tt => "rung1-sky130-tt"`) and thread it through the DB + plot
generator. INV-STAMP already refuses unstamped/rung-less curves, so the machinery
is in place. Rungs 2-4 (source non-idealities, resonator, extracted parasitics)
stay owed to M3b/M5 per ADR-0032.

## 3. Worker/advisor split -- workers now HAVE ngspice (constraint lifted)

**History (now obsolete):** workers were "blind" on ngspice -- the driven worker
ran with no Bash allowlist, so a
headless `ngspice` call was denied, and the daemon's default-shell env lacked
ngspice on PATH. SPICE tasks thrashed (edit a netlist you can't run → fail blind →
lose work), which forced an advisor-hand-craft-everything model.

**Fixed 2026-07-11 (commit 501fd78):** two soft layers, both addressed --
the worker settings allowlist `Bash(ngspice:*)` (worktree checkouts inherit
it), and the default devShell now ships ngspice on PATH + the `THESEUS_NGSPICE`
seam, so daemon-spawned workers inherit both. Verified end-to-end: a delegated
task requiring a live ngspice run reached `verify_passed` (worker wrote a netlist,
ran `ngspice`, iterated, and its ngspice-gated test passed the gate + cross-model
verifier). The profile is L0 (no bwrap), so allowlisting a simulator that reads a
`.cir` from temp doesn't widen the OS blast radius.

**Revised split** -- the axis is now *design difficulty*, not *access*:

- **Delegate to maestro (most SPICE work is now in reach):** the multi-axis sweep
  driver, typed decode, conservation measurement, admission logic, DB
  fit/interpolation/rebuild, artifact emission -- AND netlist-level tasks where the
  worker can *iterate against real ngspice feedback* (parameterized cell decks,
  sweep harnesses, DUT-matrix runs, per-cell characterization once a topology is
  fixed). A worker that can see its `.meas` output can converge on netlist syntax
  and RC/timestep issues on its own.
- **Advisor still owns genuinely-hard circuit DESIGN:** PDK pinning (W0), and the
  novel-topology / novel-physics calls where the failure is conceptual, not
  syntactic -- most sharply the **adiabatic 2LAL timing** (W1: the static-crowbar
  path that pins slope at +1). Access ≠ competence: ngspice lets a worker *iterate*,
  but the advisor still supplies domain judgment where the design space is open.

Rule of thumb (updated): **delegate anything where success is a convergent
iteration against a gate (now including live SPICE); the advisor takes the
open-ended circuit-design calls.** The advisor is no longer the serial bottleneck
on *all* SPICE work -- only on the genuinely novel physics. Still prefer giving
workers golden fixtures + the `THESEUS_NGSPICE` seam where a deterministic test is
possible (faster, cheaper than a live sim every gate run), but a live run is no
longer off-limits.

## 4. Workstreams (dependency-ordered)

### W0 -- PDK pinning (infra; blocks all *real* numbers). Advisor/human.
Pin sky130A reproducibly in the flake (`devShells.silicon` is already scaffolded
with `ngspice xschem netgen gaw` and a commented `PDK_ROOT` placeholder). Bring in
open_pdks/volare via nix; export `PDK_ROOT`; make `ngspice` find the sky130 model
includes. INV-STAMP requires the PDK version in every stamp, so capture it as a
flake-pinned store path.
- **Risk (highest infra risk):** open_pdks/volare may not be in nixpkgs → a custom
  derivation or a volare-fetch-in-nix. ADR-0024 names **IHP SG13G2** as the
  fallback if sky130 logistics bite. Time-box a spike: "does `ngspice -b` run a
  sky130 inverter under nix?" before committing to the full matrix.
- **Does NOT block Rust scaffolding** -- W2/W3/W4 proceed against fixtures.

### W1 -- Cell library (hand-authored SPICE). Advisor-led; worker-assistable.
Author `/cells/*.subckt` (the repo's *only* hand-authored SPICE, per ADR-0025).
Split: the advisor owns the **topology + adiabatic timing** (the hard, open part);
a worker can now iterate the *parameterization* (Vt/body variant decks, source/
sink cells, functional truth-table checks) against live ngspice once a topology is
fixed. Contents:
- k-CSWAP family: k0 buffer/latch, k1 1-CSWAP, k2 2-CSWAP -- dual-rail,
  transmission-gate, **self-inverse** (each cell its own adjoint network, ADR-0027).
- Baselines: static CMOS + PFAL. Bench source/sink cells.
- Variants: {std, lvt, hvt} × {body-to-rail, deep-n-well} -- parameterize via
  `.subckt` params / model-corner swap, not copy-paste.
- **Inherits the deferred adiabatic-2LAL problem:** the k0 latch/SR must actually
  hit slope −1 (today's functional dual-rail SR is +1 non-adiabatic -- a static
  crowbar path during phase overlap; needs proper non-overlapping 2LAL clock
  discipline). This is a prerequisite of the W6 self-validation gate, so it lives
  here, not as a separate item. Generator seed: `theseus-overnight/gen_2lal_sr.sh`.

### W2 -- Harness generalization (Rust). Delegate (fixtures + mock ngspice).
Extend `bench.rs` from single-axis `sweep_period` to the closed sweep: ramp
(≥3 decades, ≥5 pts/dec) × load (min, 4×, 16×) × Vt × body, per cell. Add:
- Typed decode (B5.3): rails thresholded at 0.3·VDD / 0.7·VDD with hysteresis,
  sampled mid-validity → truth-table verdict.
- Conservation-closure measurement: source energy = dissipation + Δstored, the
  ≤1% gate (TBD-1). Builds on the existing `Bpwr`/`.meas ... INTEG` pattern.
- Prefer a recorded-output mode (deterministic tests, no live sim per gate run);
  but a worker may also run live ngspice via the `THESEUS_NGSPICE` seam to develop
  the netlist templates, now that workers have ngspice access.

### W3 -- Admission checklist (Rust logic + advisor for SPICE side). Delegate core.
The 5-gate executable (`test-spec-m3a.md` §1), pure logic over measurement structs:
1. functional decode verdict, 2. conservation ≤1% (TBD-1), 3. INV-TOKEN
(asserted-rail count invariant per tick), 4. reverse-drive TH-6 (phase sequence
reversed → adjoint truth table; **record verdict even on failure**, route to
mirrored-netlist fallback), 5. adiabatic-fit R² ≥ 0.99 (TBD-5) + leakage share
reported. Reverse-drive needs the phase-reversed netlist (advisor authors; Rust
consumes the recorded run).

### W4 -- Characterization DB (Rust). Delegate (fixtures).
Fill `chardb` + `energy`:
- Per-cell sweep table + fit to the ADR-0028 form `E = CV²·(RC/t_ramp) + leakage·t
  + residual` (least-squares; R² ≥ 0.99 acceptance = TH-E3).
- RON, in-repo, diffable; **deterministic rebuild** (empty diff on unchanged
  inputs).
- Interpolation query contract (C3.1): in-range succeeds; **one-ε outside → hard
  error** (a test, not a hope). Clean, high-value testable logic.

### W5 -- DUT matrix + composition (Rust + advisor). Delegate Rust, advisor runs.
The closed matrix (`test-spec-m3a.md` §DUT matrix): {buffer, 1-CSWAP, 2-CSWAP} ×
{std,lvt,hvt} × {body-to-rail, deep-n-well} × ramp × load, **plus** shift register
≥8, CNOT chain ≥8, the **compiled reversible full adder** (this ties to the
language track -- the compiler emits the netlist, exercising k=2), plus CMOS/PFAL
baselines. TH-E1 additivity check: whole-block SPICE vs Σ(cell energies), budget
±15% absolute and log-log slope within ±0.1 of −1.

### W6 -- Rung1 money plot + self-validation gate 3 (advisor). The closeout.
Re-emit the three-family plot on rung1 sky130 cells; stamp `Rung1Sky130Tt`.
Self-validation gate 3: the **canonical 2LAL shift register reproduces slope −1
±0.1 over ≥2 decades** -- "the gate that says the harness measures the field's known
result before it measures anything new." Depends on W1's adiabatic-SR being solved.

## 5. Critical path & parallelism

```
W0 (PDK spike ─► full pin) ─┐
W1 (cells; incl. adiabatic ─┼─► real runs ─► W5 matrix ─► W6 rung1 plot + gate 3
    2LAL SR fix)            │      ▲
W2 (sweep) ─► W3 (admission)┘      │
          └─► W4 (DB) ────────────►┘   (all delegable, built on fixtures first)
```

- **Off the critical path (delegate now, parallel):** W2/W3/W4 Rust scaffolding
  against golden fixtures + mock ngspice. These land and merge before any sky130
  transistor switches.
- **Critical path (advisor-led, but no longer advisor-*serial* for all SPICE):**
  W0 PDK pin → W1 cell topology + adiabatic SR → real sweep runs → W5 → W6. Since
  workers now have ngspice, the advisor's serial load narrows to the *design*
  decisions (W0 pinning, W1 topology/timing); the mechanical characterization runs
  and parameter sweeps around a fixed topology are delegable, so W5 parallelizes.
- Recommended kickoff: **(a)** an advisor spike on W0 ("sky130 inverter under nix
  ngspice?") to de-risk the PDK, in parallel with **(b)** a delegated W4 char-DB
  scaffold (fit + interpolation + rebuild, pure logic, high test surface) and a
  delegated W2 sweep-driver generalization. All three are independent.

## 6. Risks

1. **sky130-under-nix packaging (W0).** Highest infra risk. Mitigate: time-boxed
   spike; IHP SG13G2 fallback (ADR-0024) if volare/open_pdks won't pin cleanly.
2. **Adiabatic 2LAL SR (W1).** Known-hard: functional SR is slope +1 (static
   crowbar path); needs proper non-overlapping 2LAL clocking to reach −1. Blocks
   the W6 self-validation gate. Independent circuit-design effort; don't spin --
   time-box and record if it resists.
3. **Advisor-serial SPICE bottleneck.** All of W0/W1/W5-runs/W6 are advisor
   hand-craft. Parallelizing helps only the Rust half; plan advisor time
   accordingly.
4. **Provisional TBDs (TBD-1..6).** The acceptance numbers (±1%, ±5%, R²≥0.99,
   decode thresholds, variance bound, validity-window arithmetic) may need revision
   against first real data -- that's a one-line PR to `test-spec-m3a.md` + the TBD
   register, not a redesign.

## 7. Acceptance map (test-spec-m3a → workstream → done-check)

| Requirement | WS | Done when |
|---|---|---|
| Per-cell admission (5 gates) | W3 | all 5 green for every (Vt, body) variant |
| Harness gate 1 (RC ±5%) | -- | banked (rung0); re-confirm on rung1 |
| Harness gate 2 (ideal source) | -- | banked; re-confirm |
| Harness gate 3 (2LAL SR slope −1) | W1+W6 | −1 ±0.1 over ≥2 decades on sky130 SR |
| Bench DUT matrix (closed) | W5 | full matrix + SR/CNOT/adder/baselines run |
| Money-plot artifact (RON+stamp) | W6 | three families, `Rung1Sky130Tt`, INV-STAMP |
| Characterization DB ("done means") | W4 | deterministic rebuild, interp contract, empty re-run diff |

## 8. First concrete steps (when this phase starts)
1. **W0 spike (advisor):** stand up `devShells.silicon` with a pinned sky130A;
   run a bare sky130 inverter through `ngspice -b`. Go/no-go on the PDK approach.
2. **W4 delegate (maestro):** `chardb`/`energy` scaffold -- the fit
   (`CV²·RC/t_ramp + leakage·t + residual`), R²≥0.99 check, RON (de)serialize,
   deterministic rebuild, and the in-range/one-ε-outside interpolation contract,
   all against fixture sweep tables. Pure logic, high test surface, no SPICE.
3. **W2 delegate (maestro):** generalize `bench.rs` to the multi-axis sweep +
   typed decode + conservation-closure measurement, with a recorded-output mode.

Both delegations use the standard theseus gate
(`env -u LD_LIBRARY_PATH nix develop .#physics --command cargo test/clippy -p
theseus-spice`) and must NOT require a live ngspice (fixtures + `THESEUS_NGSPICE`
seam). Base branch `chessai/theseus-v2`; one task per delegation.

## STATUS UPDATE (2026-07-11): harness self-validation COMPLETE
All three §"Harness self-validation gates" are now green on theseus-v2:
- RC-analytic ±5% ✓ (banked), ideal-source ✓ (banked)
- **canonical 2LAL shift register slope −1 ✓ (NEW)** -- the last gate. Built by a review panel
  (4 convergent strategies) → adversarial verification (energy honest,
  −1 real) → synthesis: all-1s slope −0.9996 over 3 decades, data-aware terminator
  so arbitrary patterns propagate full-level with no tail burn, honest all-source
  energy sum, real non-constant-pattern test. See adiabatic.rs `two_lal_sr_*`.
Char-DB CORE (energy fit + interpolation contract + serde/ron) also landed (W4b:
chardb.rs/energy.rs). Sky130 PDK pinned (nix/sky130-pdk.nix). REMAINING for full
M3a acceptance = the PDK-backed cell characterization: W1 cells → W3 admission →
W5 DUT matrix → populated char DB (now unblocked by the PDK + the sweep harness).

## PARKING LOT: parametric approach × data-pattern harness (see also theseus-overnight/m3a-plan.md)
Make `two_lal_sr` generation parametric over the axes where the 4 panel approaches
differed (terminator strategy, clock-edge (a,b) ratios, stage structure, gmin/is)
× input data pattern (all-1s/alternating/sparse/dense/random); sweep and tabulate
energy/slope/propagation to see what wins under which data. This is the natural
instrument for TH-E2 (data-independence, ADR-0028 C3.4) -- the verifiers proved the
naive all-1s-boundary design violates it (0-bits burn CV²); the synthesized design
fixes it, and this harness would quantify data-dependence per variant. Defer.
