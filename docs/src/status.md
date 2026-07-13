# Project status — where theseus is (2026-07-13)

One-paragraph orientation for anyone (human or agent) picking this up. theseus
is a **reversible-computing compiler + adiabatic-silicon backend**: a total,
reversible source language elaborates to Core Π (an iso calculus), lowers to a
dual-rail **adiabatic cell netlist** (the k-CSWAP family), and is **priced in
joules** from real ngspice characterization of sky130 cells. The thesis
(ADR-0022): a reversible *program* compiled all the way to *measured energy* on
characterized silicon — and, at the top of the stack, a reversible **CPU**
(TISC-1) whose invertibility is its typecheck.

Canonical branch: **`chessai/theseus-v2`** (unpushed). Tip at this writing:
`m4-v1-complete`. Everything below is merged there; each milestone is tagged and
git-bundled under `~/theseus-overnight/`.

## Status at a glance

| Milestone | State | Key tags / notes |
|---|---|---|
| **M1** language | ✅ done | parse → coverage/linearity check → elaborate → interpret → Core Π (both slices: plain isos + labels/trace) |
| **M2** FPGA / Verilator | ⏸ parked | `theseus-emit`/`theseus-verify` are stubs; roadmap downgrades this to a *demo, not a phase* |
| **M3a** physics money plot | ✅ done | adiabatic slope −1 vs CMOS-flat vs PFAL, on sky130 ngspice; char DB + DUT matrix |
| **M3b** compiled energy | ✅ done | compiler lowers programs → phased adiabatic cells → oracle-verified → energy. **Full adder** and **Fibonacci** compiled to *measured* joules |
| **M4-v1** TISC-1 CPU | ✅ **done** | `m4-v1-complete`: the CPU runs **Fibonacci and Feistel, forward and backward, with per-clause energy** — ADR-0030's milestone, met |
| **M4-v2** ring machine | ⏳ next-gen | recirculating/looping CPU (C2.4); parked until unrolled area is unaffordable |

## The compile pipeline

```
surface .thse ──parse/check/elaborate (theseus-syntax, theseus-check)──▶ Core Π Iso (theseus-core)
      │                                                                        │
      │  ADR-0004 coverage/linearity checker enforces total bijections        │ canonicalize
      ▼                                                                        ▼
  differential oracle  ◀── theseus-eval (reference interpreter) ──▶  typed netlist IR (ADR-0013)
      │  (correctness = lowered netlist == interpreter over the WHOLE domain)  │ lower (ADR-0031)
      ▼                                                                        ▼
  energy report  ◀── theseus-spice::annotate (ADR-0028) ── phased cell netlist (k-CSWAP K0..K3, ADR-0027)
                          Σ cell-energies from the rung0 char DB                │ balance (ADR-0026 C2.2)
                          (real ngspice sky130 characterization)                ▼  tick-exact, phased-sim
```

The **k-CSWAP cell family** (ADR-0027) is the whole logic basis: `K0` buffer,
`K1` Fredkin (1-control swap), `K2` (2-control), `K3` (3-control). All are
conservative (charge-routing) reversible cells; a controlled swap dissipates
only the small adiabatic residue, recoverable by the power-clock. **General
synthesis** (B4.2, done): any admitted bijection lowers to a verified k-CSWAP
network (Miller–Maslov–Dueck; Toffoli-k → CSWAP+ancilla, AND-reduced to stay
≤K3).

## TISC-1 (the M4-v1 CPU) — `crates/theseus-tisc`

A reversible ISA (ADR-0030): 160-bit State (8×W8 regs + PC + BR + IR16 + 8
scratch), 14 instructions each an iso on State, `step = fetch;decode;execute;
unfetch;PC++`. **Unroll-only in v1** (C2.4): a program is a straight-line ROM;
running T cycles = `step` applied T times. Spec: `docs/src/design/tisc-1-spec.md`.

- **Datapath** (`instr.rs`): ADD/SUB/ADDI/SUBI (Takahashi–Kunihiro in-place
  ripple, ≤K2), XOR/XORI, ROTL/ROTR/ROTLI/ROTRI, EXCH, CSWAP (control-threaded
  Fredkin cascade — no fanout), XCHG, SWAPBR. All oracle-verified.
- **Decoder** (`decoder.rs`): register-select crossbar + **predicated dispatch**
  (each instruction gated by a 1-bit opcode predicate rather than nested Branch,
  which would blow up to K7). 4192 cells, max K2.
- **Machine cycle** (`step.rs`): ~4846 cells/step (unbalanced), max K2. Runs
  forward and backward (adjoint) — verified via the interpreter.
- **Energy** (`energy.rs`): per-clause energy from the **real** char DB, with the
  key trick that the balancer's K0 buffers are **counted analytically** from
  `line_arrivals` (validated to match `balance()` exactly) rather than
  materialized — so a step's ~1.6 M-buffer balanced netlist never has to fit in
  RAM.

## Headline numbers (real rung0 char DB, `t_ramp = 1e-7 s`)

- Compiled **full adder**: 5.34e-13 J/op → real-cell 3.52e-13 J/op.
- Compiled **Fibonacci** (as a specialized circuit, M3b): ~1.5 pJ.
- **TISC-1 step** (the general CPU cycle): ~1.6–3.5 nJ — **buffer-dominated**
  (~98% is K0 buffers riding the systolic pipeline; "holding costs motion").
- **The duality gap**: the general CPU running Fibonacci costs ~20,000× the
  *specialized compiled* Fibonacci circuit. Generality is expensive — the
  temporal/spatial duality (ADR-0030) made quantitative.
- Energy is a *dial*: `E(T) ≈ k_adia/T + k_leak·T + k_res`, so it falls ∝ 1/clock
  as you slow down — the adiabatic advantage over (flat) CMOS. See the **CPU
  money plot** on the roadmap.

## Verification norm

Correctness is the **differential oracle**: the lowered cell netlist, simulated,
must equal the reference interpreter (`theseus-eval`) over the *whole finite
input domain* (or a thorough sample where the domain is astronomically large,
e.g. the CPU's 2^160 State). This is theseus's norm — it is *not* the maestro
adversarial-review rule (that governs the separate maestro repo only). The
oracle repeatedly caught real bugs this cycle (a controlled-op lowering defect, a
depth-tracking bug) that unit tests missed.

## Open items & next steps

- **CPU money plot** *(wanted; on roadmap)* — sweep step/program energy across
  clock ramps via the char-DB fits; plot the machine-level adiabatic slope −1
  crossing below a CMOS reference. Reuses `theseus-tisc::energy`.
- **SWAPBR loop / data-dependent branching** — currently SWAPBR degenerates to
  PC-advance: a mid-step PC change breaks the fetch/unfetch symmetry that keeps
  IR provably zero (P4). Needs reversible come-from / paired-branch machinery.
  v1 is unroll-only, so this is v2-flavored.
- **Balancer buffer memory** — balancing one `step` materializes ~1.6 M K0
  buffers (tens of GB). A compact/streaming buffer representation would let
  `step^T` be *balanced/simulated* at T>1, not only eval'd + analytically priced.
- Parked (roadmap): M2/FPGA demo; μ-types/iter/fold (v0.2); ring machine (M4-v2);
  idealization rungs 2–4 / layout (M5); higher-k cells if ever needed.

## Honest caveats (the claims boundary)

- Energy numbers are **logic dissipation under ideal power-clocks** (ADR-0028
  C3.3) — clock-generation/resonator losses and I/O are excluded.
- The char DB is **rung0** (generic level-1 models) plus real sky130 K0..K3
  cells; it's characterized once and committed as `crates/theseus-spice/data/
  rung0-chardb.ron` (regenerate via `cargo run --example characterize_rung0`,
  ~15 min of ngspice sweeps).
- The CPU is verified on **sampled** State (2^160 is not enumerable); every
  datapath instruction and the small combinational pieces are verified
  exhaustively or near-exhaustively.
- Register **aliasing** (e.g. `ADD r,r`) is non-injective and forbidden by the
  assembler; the P1 coverage checker is the source-level enforcement point.

## Hard-won lessons (constraints future work must respect)

- **Build multi-bit arithmetic compositionally** from small pieces — monolithic
  wide truth tables don't elaborate (the elaborator's `synthesize` only handles
  shallow-dependency bijections) and blow past the cell library. And a naive
  in-place ripple carry is *not a bijection* (unbalanced Boolean function);
  in-place ADD needs a Takahashi–Kunihiro construction. `ADDI`-by-odd is an odd
  permutation while <8-wire cells are even — a parity obstruction needing an aux
  word. (Proofs in `theseus-tisc` module docs.)
- **The energy/memory wall is the balancer's buffers**, not the compiler
  (canonicalize was made sub-quadratic; `lower_iso` of a full step succeeds; only
  `balance()` OOMs). Analytic buffer counting is the escape hatch.
- **Match agent capability to problem depth** — the hard reversible constructions
  (general synthesis, decoder, in-place adder, both programs) needed the
  strongest model; mechanical lifting did not.
