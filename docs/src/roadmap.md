# Roadmap v2: two tracks, layers, parked items

> **Current status (2026-07-13):** M1, M3a, M3b, and **M4-v1** (the TISC-1 CPU)
> are all done and merged on `chessai/theseus-v2` (`m4-v1-complete`). See
> [Project status](status.md) for the full picture. Nearest wanted artifact: the
> **CPU money plot** (below).

Post-pivot (ADR-0022): the project is **two-tracked**, and the tracks
are independent for months.

## The dependency graph

```text
LANGUAGE TRACK                     PHYSICS TRACK
M1: parser → coverage checker      M3a: cell decks (ADR-0027) →
    (highest risk, brute-force          harness self-validation →
    oracle first) → interpreter        scaling law reproduced (ADR-0025)
      │                                  │
M2: typed IR → FPGA lowering →     characterization DB + fits
    Verilator oracle chain               │
      └──────────────┬───────────────────┘
                     ▼
      M3b: dual-rail lowering (ADR-0031) + phased-sim
           + energy-annotated compiled adder (ADR-0028)
                     ▼
      M4-v1: TISC-1, unrolled-T, run forward AND backward,
             clause-level energy report (ADR-0029/0030)
                     ▼
      M4-v2: ring machine (wakes C2.4)   M5 (optional): tape-out
```

The two tracks share zero code until M3b. **Staffing implication: the
coverage checker and the bench harness are the two highest-risk items
and can start simultaneously** — the first implementation agent gets M1,
the second gets M3a.

## Milestones (definition of demo)

1. **M1:** bijection-table programs elaborate, interpret, round-trip;
   coverage checker passes brute-force differential + mutation gate.
2. **M2:** same programs to Verilator; oracle chain green;
   `--verify-lowering` exhaustive tier green.
3. **M3a:** 2LAL shift register + CNOT chain + reversible full adder in
   Sky130 ngspice; harness conservation checks close; **energy/op ∝ 1/T
   (log-log slope −1) across ≥2 decades, CMOS flat and PFAL floored on
   the same axes** (the money plot; canonical axes per ADR-0023). Characterization DB built, fits within TH-E3 bounds.
4. **M3b:** the adder compiled *by the compiler* through ADR-0031;
   phased-sim in the oracle chain; static energy report within TH-E1
   budget of whole-block SPICE; reverse-drive (TH-6) verdicts recorded.
5. **M4-v1:** TISC-1 executes Fibonacci and a Feistel round, forward and
   backward, with per-clause energy attribution.
6. **Floating: FPGA demo (E3 definition-of-done, so it cannot bloat):**
   ULX3S bitstream of an M2-era design; `:unstep` against live hardware
   state; one page of docs. Nothing more — it is a demo, not a phase.
7. **M4-v2 / M5:** ring machine (gated on C2.4 wake); tape-out (gated on
   ADR-0032 rung 3+ and S2LAL trigger evaluation, ADR-0023).

## M1 slicing (language track)

M1 lands in two slices to keep the highest-risk work on plain, decidable
ground first (decision 2026-07-09):

- **Slice 1 — plain isos.** Parser, coverage/linearity checker, elaborator,
  and interpreter for label-free clause isos (conformance P0001–P0010,
  P0012–P0015). Coverage ranges over the plain declared type; soundness is
  TH-4a, *proved* (`theory/coverage-bijectivity.md`). This is the buildable
  first milestone: grammar, static semantics, and Core Π (typing + adjoint +
  eval) are frozen; elaboration is the last spec gate.
- **Slice 2 — labels.** `$`-patterns, label-type inference, elaboration to
  `trace`, the fuel/`:step` accounting, and the `T0301`/`T0303` split
  (conformance P0011). Gated on resolving TH-1 (finite-trace totality, exact
  bound — currently sketched) before the interpreter's bound is reported as
  WCET.

Slice 1 unblocks M2 (typed IR → FPGA lowering) for plain isos independently of
Slice 2.

## Version → feature mapping

- **v0.1** — finite dialect; trace-only core; FPGA + adiabatic backends;
  M1–M3b scope.
- **v0.2** — μ-types (software dialect); iter/fold/unfold; HandshakeLoop;
  guards proposal; `@tag`; Option-B bus design *informed by* the ring
  machine.
- **v0.3+** — eqsat (ADR-0010); S2LAL migration if triggered; reduced
  swing + corners (ADR-0024 B2.4); rung 2/3 of the idealization ladder.

## Parked items and wake conditions

| Item | Wakes when |
|---|---|
| Recirculating ring loops (C2.4) | unrolled area unaffordable — formally declared by M4 (ADR-0030) |
| Option-B memory bus (ADR-0029) | fabric-resident storage dominates the energy report (C3.5 names it) |
| Option-C SRAM macro prim | Option B insufficient at scale |
| S2LAL migration (ADR-0023) | leakage share threshold, or M5 planning |
| Xyce (ADR-0025) | block size exceeds ngspice patience; validation gate first |
| Idealization rungs 2–4 (ADR-0032) | M3b done; M5 planning |
| e-graphs; guards; sized types; salsa; `@tag`; post-flatten rebalance (FPGA) | unchanged from v1 roadmap |
| OpenLane/layout in silicon shell | rung 4 |

## M4-v1 demonstration artifacts & follow-ups

Landed for M4-v1: the TISC-1 datapath, decoder, and `step` cycle compile to
adiabatic cells; the CPU runs **Fibonacci** and a **Feistel round** forward and
backward (via the interpreter — unroll-only, C2.4); and per-clause energy is
computed **analytically** from the real rung0 char DB (the balancer's K0 buffers
are *counted* from `line_arrivals`, not materialized, so a single `step`'s
~1.6 M-buffer balanced netlist never has to fit in RAM). Open items:

| Item | Notes / wakes when |
|---|---|
| **CPU "money plot"** (energy-per-cycle vs. clock ramp) | *Wanted.* Sweep the `step`/program energy across `t_ramp` (1e-8…1e-5) via the char-DB `E(T)=k_adia/T+k_leak·T+k_res` fits (energy model already merged, `theseus-tisc::energy`), plot log-log: the adiabatic **slope −1** for the CPU crossing **below** a flat CMOS reference — the "it gets arbitrarily low as you slow the clock, unlike irreversible logic" demonstration, at the *machine* level (mirrors the M3a cell money plot). Non-blocking; a headline artifact. |
| **SWAPBR loop / data-dependent branching** | v2-flavored. SWAPBR currently degenerates to PC-advance: a mid-step PC change breaks the fetch/unfetch symmetry that keeps IR provably zero (P4). Needs the reversible come-from / paired-branch machinery (Pendulum lineage). v1 uses straight-line unrolled ROMs (consistent with unroll-only). |
| **Balancer buffer-materialization memory** | The systolic K0 buffers dominate both cell count and energy; balancing one `step` is ~1.6 M cells / tens of GB. A compact/streaming buffer representation (or not materializing pass-through buffers) would let `step^T` be balanced/simulated at T>1, not just eval'd + analytically priced. |

## Publication map (appendix; motivational, non-normative)

1. **The language + bidirectional coverage checking** (ADR-0004/0017;
   the witness-bearing checker, linearity, totality-for-free).
2. **Energy-annotated reversible compilation** (ADR-0023–0028/0031/0032:
   one-cell fabric, static energy reports, clause-level attribution, the
   money plot; TH-6 as a result either way).
3. **TISC-1: a CPU whose invertibility is its typecheck** (ADR-0029/0030;
   temporal/spatial duality note TH-7).

## Nix/CI mechanics
Unchanged from v1 (ADR-0019) plus: `silicon` devShell awake (thin);
SPICE smoke deck in fast tier; sweep campaigns + TH-E1/E2 battery in
nightly tier; DB and energy-report diffs reviewable in PRs (ADR-0028).
