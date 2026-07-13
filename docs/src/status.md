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
`2315ac2`. Everything below is merged there. Since M4-v1 this branch also landed
the **M4-v2 ring-machine core** (a recirculating CPU) and the **v0.2 software
dialect** (μ-types + iter/fold/unfold, source-to-cells) — see the two new
sections below and ADRs 0033–0039.

## Status at a glance

| Milestone | State | Key tags / notes |
|---|---|---|
| **M1** language | ✅ done | parse → coverage/linearity check → elaborate → interpret → Core Π (both slices: plain isos + labels/trace) |
| **M2** FPGA / Verilator | ⏸ parked | `theseus-emit`/`theseus-verify` are stubs; roadmap downgrades this to a *demo, not a phase* |
| **M3a** physics money plot | ✅ done | adiabatic slope −1 vs CMOS-flat vs PFAL, on sky130 ngspice; char DB + DUT matrix |
| **M3b** compiled energy | ✅ done | compiler lowers programs → phased adiabatic cells → oracle-verified → energy. **Full adder** and **Fibonacci** compiled to *measured* joules |
| **M4-v1** TISC-1 CPU | ✅ **done** | the CPU runs **Fibonacci and Feistel, forward and backward, with per-clause energy** — ADR-0030's milestone, met |
| **M4-v2** ring machine | ✅ **done + zero-garbage** | recirculating CPU realized: `step` recirculates R times == `eval(step)^R`, runs **Fibonacci on the ring** at **20× area saving**; reversible branching works (ADR-0034). **Physical closure achieved**: the R1 rail-role-swap cell (ADR-0040, real ngspice, adiabatic slope −1) makes lowering garbage-free — the step is ancilla-clean (`ancillas_restored: true`, 945 dirty → 0), so `run_true == eval^R` closes with **zero** re-supplied boundary. |
| **v0.2 dialect** software | ✅ **done** | μ-types + `iter`/`fold`/`unfold`: core + eval + surface syntax + **netlist lowering** + diagnostics; writable `.thse` programs run source→cells (ADRs 0035–0039) |
| **M2** FPGA / Verilator | ⏸ parked | still stubs; a demo, not a phase |

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
scratch), 14 instructions each an iso on State. The machine cycle is now
`step_v2` (ADR-0034): `fetch; decode_execute; unfetch; PC += BR + 1` — **no
instruction writes PC**, SWAPBR swaps BR↔rd, and the uniform end-of-step
`PC += BR + 1` makes **data-dependent branching work as a total bijection**
(the v1 SWAPBR injectivity obstruction is resolved; BR=0 fall-through keeps every
straight-line ROM bit-identical). Running T cycles = `step` applied T times, now
available two ways: **unroll** (v1) or **recirculate one balanced step** (M4-v2,
below). Spec: `docs/src/design/tisc-1-spec.md`.

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

## M4-v2: the ring machine (recirculating CPU) — core done

The v1 CPU *unrolls* `step` T times (area ∝ T). M4-v2 **recirculates** one balanced
`step` instead. What landed this cycle (ADR-0033/0034):

- **Reversible branching** (`step_v2`, ADR-0034): SWAPBR = BR↔rd, uniform
  `PC += BR + 1`; a total bijection on all 2^160 State (proof sketch in
  `step.rs`); the v1 SWAPBR obstruction dissolved; fib/feistel bit-identical.
- **Compact-buffer balancer** (`balance_compact`): stores K0 buffer *runs* as
  per-input delay counts, so a full `step` is balanced *and* simulated without the
  ~1.6 M-cell materialization (the old memory wall). This is the enabler.
- **Ring driver** (`theseus-tisc::ring_run`): recirculates the compact-balanced
  `step` R revolutions; oracle-verified `ring.run(state, R) == eval(step)^R`, cell
  inventory independent of R.
- **Demo**: the CPU **computes Fibonacci on the ring** (`1,1,2,3,5,…,89,144`) —
  one ~9.18 M-cell body serves all 20 cycles vs 183.5 M cells unrolled = **20×
  area saving**.
- **Physical closure — achieved, zero garbage** (ADR-0033 + ADR-0040). First the
  obstruction was proved: a *machine-checked parity theorem* shows K0–K3 cells
  permute rail-pairs, so in that vocabulary a CNOT/Toffoli provably cannot leave
  its ancillas constant — garbage-free logic is impossible. Two responses landed:
  (1) a composition-level **Bennett wrap** (`bennett.rs`) that restores forward
  ancillas and shrinks the residue to a fixed 320-slot boundary (`ClosedStepRing`);
  and (2) the *root-cause fix* — the **R1 rail-role-swap cell** (ADR-0040): a
  controlled swap of the two rails *within* one pair, which flips a value without
  routing a pair, **voiding the parity law**. R1 is a real adiabatic cell (ngspice,
  slope −1, B5-admitted), a first-class `CellKind`, and the synthesis (`emit_mcnot`)
  now lowers CNOT→one R1, Toffoli→a clean K1-R1-K1 sandwich, any k-control→
  AND-chain+R1 — all ancilla-clean **with R1 alone**. Result: the TISC-1 step's
  plain lowering is ancilla-clean (`ancillas_restored: true`, 945 dirty → 0), so
  `StepRing::run_true == eval(step)^R` **physically closes with zero re-supplied
  boundary** — a genuinely self-contained recirculating CPU. The Bennett wrap
  demotes to the fallback for residual K-only-dirty fan-outs (trace/iter chains).

## v0.2: the software dialect — done (source to cells)

The v0.2 dialect adds recursive types and structural iteration, all the way from
surface syntax to adiabatic cells (ADRs 0035–0039):

- **μ-types** (ADR-0035): recursive `type Nat = Zero | Succ Nat` lowers by
  bounded unrolling to a finite Core Π type (depth-annotated), so coverage stays
  decidable.
- **`iter`** (ADR-0036, count-driven) and **`fold`/`unfold`** (ADR-0037,
  structure-driven, a reversible mapAccumL / its dual) as core Iso primitives —
  adjoint-closed, total by structural induction, fuel-costed.
- **Surface syntax** (ADR-0038): `iter Nat notb`, `fold BitList cnot`, … parse →
  coverage/linearity-check → elaborate → eval **forward and backward**; conformance
  programs 0016–0020 grade both directions; ill-formed combinators get first-class
  T0xxx diagnostics.
- **Netlist lowering** (ADR-0039): all three primitives **compile to k-CSWAP
  cells** (tag-gated unroll, ≤K2), differential-oracle verified — so v0.2 programs
  are not interpreter-only. Runtime (adiabatic) lowering of the *loop* forms is
  done; energy-pricing a compiled iter/fold program is the open follow-up.

## Verification norm

Correctness is the **differential oracle**: the lowered cell netlist, simulated,
must equal the reference interpreter (`theseus-eval`) over the *whole finite
input domain* (or a thorough sample where the domain is astronomically large,
e.g. the CPU's 2^160 State). This is theseus's norm — it is *not* the maestro
adversarial-review rule (that governs the separate maestro repo only). The
oracle repeatedly caught real bugs this cycle (a controlled-op lowering defect, a
depth-tracking bug) that unit tests missed.

## Open items & next steps

- **Physically-closing ring** — wire the Bennett wrap (`bennett.rs`) into
  `StepRing::run_true` so true recirculation feeds back the clean 1138 ancillas
  and re-supplies only the fixed 320-slot boundary; and/or evaluate an ADR-0027
  **controlled rail-role-swap cell** that would void the parity obstruction and
  make CNOTs garbage-free.
- **Energy-price a compiled loop** — now that `iter`/`fold`/`unfold` lower to
  cells, run one through `theseus-spice::annotate` for a joules figure (extends
  the M3b/energy story to the software dialect).
- **CPU money plot** *(wanted)* — sweep step/program energy across clock ramps via
  the char-DB fits; plot the machine-level adiabatic slope −1 below a CMOS
  reference. Reuses `theseus-tisc::energy`.
- **v0.2 surface polish** — depth-annotation syntax for μ-types (currently an env
  default); the remaining ADR-0038 deferrals.
- Resolved this cycle: reversible branching (was "SWAPBR degenerates"); the
  balancer memory wall (now `balance_compact`); v0.2 μ-types/iter/fold (now done).
- Parked (roadmap): M2/FPGA demo; idealization rungs 2–4 / layout (M5); higher-k
  cells if ever needed.

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
- **The K0–K3 fabric has a parity law** (ADR-0033, machine-checked): every cell
  permutes rail-pair values, so `parity(data_out) ⊕ parity(anc_out)` is fixed up
  to a constant. A conditional-NOT's parity deviation *is* its AND, so isolated
  CNOT/Toffoli lowerings *cannot* be ancilla-clean in the K0–K3 vocabulary — the
  theorem (`parity_obstruction_is_real`) stands. The **escape is a vocabulary
  extension, not a cleverer construction**: the R1 rail-role-swap cell (ADR-0040)
  voids the law's premise (it flips a pair's value instead of permuting pairs) and
  makes garbage-free logic real — no physics is added (R1 is a subset of the K1
  transmission-gate fabric). Lesson: when a limit is an *encoding-level theorem*
  rather than a physical one, the fix is at the vocabulary level.
