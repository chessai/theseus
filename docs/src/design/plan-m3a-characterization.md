# Plan: M3a characterization pipeline (PDK-backed completion)

**Status:** scoping / not started. Authored 2026-07-11.
**Owns:** the gap between the *banked* M3a headline (the three-family money plot
on generic rung0 models) and the *full* `test-spec-m3a.md` acceptance (per-cell
admission + closed DUT matrix + characterization DB, on pinned sky130 cells).

> This is formally the **completion of M3a** — `test-spec-m3a.md` lists the
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
deck}` (inline `pub mod … {}` in `lib.rs`). No PDK, no admission, no DUT matrix,
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
is in place. Rungs 2–4 (source non-idealities, resonator, extracted parasitics)
stay owed to M3b/M5 per ADR-0032.

## 3. The load-bearing constraint: workers are blind on ngspice

Hard-won operating lesson (see `theseus-overnight/m3a-plan.md`, and the memory
`theseus-overnight-run`): **headless maestro workers cannot run ngspice** (denied
in the sandbox), so any task whose gate needs a live SPICE round-trip *thrashes* —
the worker edits a netlist it can't simulate, fails blind, and loses work. This
splits every workstream cleanly:

- **Advisor hand-craft (SPICE-touching):** PDK pinning, all `/cells` `.subckt`
  authoring, reverse-drive netlist construction, and every *real* characterization
  run. These need interactive ngspice iteration in `nix develop .#silicon`.
- **Delegate to maestro (pure Rust):** harness structs, the multi-axis sweep
  driver, admission-checklist *logic*, the DB fit/interpolation/rebuild, artifact
  emission. These are worker-tractable **iff** they're built against golden
  fixtures + a mock-ngspice seam (the `THESEUS_NGSPICE` env seam already exists in
  `bench.rs`), so the gate never needs a real simulator.

Rule of thumb: **if the gate needs a transistor to switch, the advisor does it;
if the gate is `cargo test` over recorded numbers, maestro does it.** This is the
throughput bottleneck — the advisor is the serial resource on SPICE work.

## 4. Workstreams (dependency-ordered)

### W0 — PDK pinning (infra; blocks all *real* numbers). Advisor/human.
Pin sky130A reproducibly in the flake (`devShells.silicon` is already scaffolded
with `ngspice xschem netgen gaw` and a commented `PDK_ROOT` placeholder). Bring in
open_pdks/volare via nix; export `PDK_ROOT`; make `ngspice` find the sky130 model
includes. INV-STAMP requires the PDK version in every stamp, so capture it as a
flake-pinned store path.
- **Risk (highest infra risk):** open_pdks/volare may not be in nixpkgs → a custom
  derivation or a volare-fetch-in-nix. ADR-0024 names **IHP SG13G2** as the
  fallback if sky130 logistics bite. Time-box a spike: "does `ngspice -b` run a
  sky130 inverter under nix?" before committing to the full matrix.
- **Does NOT block Rust scaffolding** — W2/W3/W4 proceed against fixtures.

### W1 — Cell library (hand-authored SPICE). Advisor. Biggest domain chunk.
Author `/cells/*.subckt` (the repo's *only* hand-authored SPICE, per ADR-0025):
- k-CSWAP family: k0 buffer/latch, k1 1-CSWAP, k2 2-CSWAP — dual-rail,
  transmission-gate, **self-inverse** (each cell its own adjoint network, ADR-0027).
- Baselines: static CMOS + PFAL. Bench source/sink cells.
- Variants: {std, lvt, hvt} × {body-to-rail, deep-n-well} — parameterize via
  `.subckt` params / model-corner swap, not copy-paste.
- **Inherits the deferred adiabatic-2LAL problem:** the k0 latch/SR must actually
  hit slope −1 (today's functional dual-rail SR is +1 non-adiabatic — a static
  crowbar path during phase overlap; needs proper non-overlapping 2LAL clock
  discipline). This is a prerequisite of the W6 self-validation gate, so it lives
  here, not as a separate item. Generator seed: `theseus-overnight/gen_2lal_sr.sh`.

### W2 — Harness generalization (Rust). Delegate (fixtures + mock ngspice).
Extend `bench.rs` from single-axis `sweep_period` to the closed sweep: ramp
(≥3 decades, ≥5 pts/dec) × load (min, 4×, 16×) × Vt × body, per cell. Add:
- Typed decode (B5.3): rails thresholded at 0.3·VDD / 0.7·VDD with hysteresis,
  sampled mid-validity → truth-table verdict.
- Conservation-closure measurement: source energy = dissipation + Δstored, the
  ≤1% gate (TBD-1). Builds on the existing `Bpwr`/`.meas … INTEG` pattern.
- Keep the `THESEUS_NGSPICE` seam; add a recorded-output mode so tests run without
  a simulator (this is what makes it delegable).

### W3 — Admission checklist (Rust logic + advisor for SPICE side). Delegate core.
The 5-gate executable (`test-spec-m3a.md` §1), pure logic over measurement structs:
1. functional decode verdict, 2. conservation ≤1% (TBD-1), 3. INV-TOKEN
(asserted-rail count invariant per tick), 4. reverse-drive TH-6 (phase sequence
reversed → adjoint truth table; **record verdict even on failure**, route to
mirrored-netlist fallback), 5. adiabatic-fit R² ≥ 0.99 (TBD-5) + leakage share
reported. Reverse-drive needs the phase-reversed netlist (advisor authors; Rust
consumes the recorded run).

### W4 — Characterization DB (Rust). Delegate (fixtures).
Fill `chardb` + `energy`:
- Per-cell sweep table + fit to the ADR-0028 form `E = CV²·(RC/t_ramp) + leakage·t
  + residual` (least-squares; R² ≥ 0.99 acceptance = TH-E3).
- RON, in-repo, diffable; **deterministic rebuild** (empty diff on unchanged
  inputs).
- Interpolation query contract (C3.1): in-range succeeds; **one-ε outside → hard
  error** (a test, not a hope). Clean, high-value testable logic.

### W5 — DUT matrix + composition (Rust + advisor). Delegate Rust, advisor runs.
The closed matrix (`test-spec-m3a.md` §DUT matrix): {buffer, 1-CSWAP, 2-CSWAP} ×
{std,lvt,hvt} × {body-to-rail, deep-n-well} × ramp × load, **plus** shift register
≥8, CNOT chain ≥8, the **compiled reversible full adder** (this ties to the
language track — the compiler emits the netlist, exercising k=2), plus CMOS/PFAL
baselines. TH-E1 additivity check: whole-block SPICE vs Σ(cell energies), budget
±15% absolute and log-log slope within ±0.1 of −1.

### W6 — Rung1 money plot + self-validation gate 3 (advisor). The closeout.
Re-emit the three-family plot on rung1 sky130 cells; stamp `Rung1Sky130Tt`.
Self-validation gate 3: the **canonical 2LAL shift register reproduces slope −1
±0.1 over ≥2 decades** — "the gate that says the harness measures the field's known
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
- **Critical path (advisor-serial):** W0 PDK pin → W1 cells (esp. the adiabatic SR)
  → real sweep runs → W5 → W6. This is the throughput limit.
- Recommended kickoff: **(a)** an advisor spike on W0 ("sky130 inverter under nix
  ngspice?") to de-risk the PDK, in parallel with **(b)** a delegated W4 char-DB
  scaffold (fit + interpolation + rebuild, pure logic, high test surface) and a
  delegated W2 sweep-driver generalization. All three are independent.

## 6. Risks

1. **sky130-under-nix packaging (W0).** Highest infra risk. Mitigate: time-boxed
   spike; IHP SG13G2 fallback (ADR-0024) if volare/open_pdks won't pin cleanly.
2. **Adiabatic 2LAL SR (W1).** Known-hard: functional SR is slope +1 (static
   crowbar path); needs proper non-overlapping 2LAL clocking to reach −1. Blocks
   the W6 self-validation gate. Independent circuit-design effort; don't spin —
   time-box and record if it resists.
3. **Advisor-serial SPICE bottleneck.** All of W0/W1/W5-runs/W6 are advisor
   hand-craft. Parallelizing helps only the Rust half; plan advisor time
   accordingly.
4. **Provisional TBDs (TBD-1..6).** The acceptance numbers (±1%, ±5%, R²≥0.99,
   decode thresholds, variance bound, validity-window arithmetic) may need revision
   against first real data — that's a one-line PR to `test-spec-m3a.md` + the TBD
   register, not a redesign.

## 7. Acceptance map (test-spec-m3a → workstream → done-check)

| Requirement | WS | Done when |
|---|---|---|
| Per-cell admission (5 gates) | W3 | all 5 green for every (Vt, body) variant |
| Harness gate 1 (RC ±5%) | — | banked (rung0); re-confirm on rung1 |
| Harness gate 2 (ideal source) | — | banked; re-confirm |
| Harness gate 3 (2LAL SR slope −1) | W1+W6 | −1 ±0.1 over ≥2 decades on sky130 SR |
| Bench DUT matrix (closed) | W5 | full matrix + SR/CNOT/adder/baselines run |
| Money-plot artifact (RON+stamp) | W6 | three families, `Rung1Sky130Tt`, INV-STAMP |
| Characterization DB ("done means") | W4 | deterministic rebuild, interp contract, empty re-run diff |

## 8. First concrete steps (when this phase starts)
1. **W0 spike (advisor):** stand up `devShells.silicon` with a pinned sky130A;
   run a bare sky130 inverter through `ngspice -b`. Go/no-go on the PDK approach.
2. **W4 delegate (maestro):** `chardb`/`energy` scaffold — the fit
   (`CV²·RC/t_ramp + leakage·t + residual`), R²≥0.99 check, RON (de)serialize,
   deterministic rebuild, and the in-range/one-ε-outside interpolation contract,
   all against fixture sweep tables. Pure logic, high test surface, no SPICE.
3. **W2 delegate (maestro):** generalize `bench.rs` to the multi-axis sweep +
   typed decode + conservation-closure measurement, with a recorded-output mode.

Both delegations use the standard theseus gate
(`env -u LD_LIBRARY_PATH nix develop .#physics --command cargo test/clippy -p
theseus-spice`) and must NOT require a live ngspice (fixtures + `THESEUS_NGSPICE`
seam). Base branch `chessai/theseus-v2`; one task per delegation.
