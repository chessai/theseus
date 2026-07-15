# Project status — where theseus is (2026-07-14)

One-paragraph orientation for anyone (human or agent) picking this up. theseus
is a **reversible-computing compiler + adiabatic-silicon backend**: a total,
reversible source language elaborates to Core Π (an iso calculus), lowers to a
dual-rail **adiabatic cell netlist** (the k-CSWAP family), and is **priced in
joules** from real ngspice characterization of sky130 cells. The thesis
(ADR-0022): a reversible *program* compiled all the way to *measured energy* on
characterized silicon — and, at the top of the stack, a reversible **CPU**
(TISC-1) whose invertibility is its typecheck.

Canonical branch: **`chessai/theseus-v2`** (unpushed). Tip at this writing:
`2424f7c`. Everything below is merged there. Since M4-v1 this branch landed the
**M4-v2 ring-machine core** (a recirculating CPU), the **v0.2 software dialect**
(μ-types + iter/fold/unfold, source-to-cells), and — the **current headline
thread** — the **surface → TISC-1 ROM compiler** (the #1 goal: a simulated CPU
that fetches and executes ROMs compiled from arbitrary `.thse` code). Milestones
**M-A, M-B, and M-C1** are done: non-recursive *and* branching / data-dependent
`.thse` compiles to a TISC-1 ROM that runs on the CPU, provably equal to the
interpreter — including the real data-dependent `fib` (`iter Nat fibStep`), which
now computes Fibonacci on the simulated CPU — and values up to **14 leaves**
(register↔scratch spill, M-C1). See the sections below and ADRs 0033–0044.

## Status at a glance

| Milestone | State | Key tags / notes |
|---|---|---|
| **M1** language | ✅ done | parse → coverage/linearity check → elaborate → interpret → Core Π (both slices: plain isos + labels/trace) |
| **M2** FPGA / Verilator | ⏸ parked | `theseus-emit`/`theseus-verify` are stubs; roadmap downgrades this to a *demo, not a phase* |
| **M3a** physics money plot | ✅ done | adiabatic slope −1 vs CMOS-flat vs PFAL, on sky130 ngspice; char DB + DUT matrix |
| **M3b** compiled energy | ✅ done | compiler lowers programs → phased adiabatic cells → oracle-verified → energy. **Full adder** and **Fibonacci** compiled to *measured* joules |
| **M4-v1** TISC-1 CPU | ✅ **done** | the CPU runs **Fibonacci and Feistel, forward and backward, with per-clause energy** — ADR-0030's milestone, met |
| **M4-v2** ring machine | ✅ **done + zero-garbage** | recirculating CPU realized: `step` recirculates R times == `eval(step)^R`, runs **Fibonacci on the ring** at **20× area saving**; reversible branching works (ADR-0034). **Physical closure achieved**: the R1 rail-role-swap cell (ADR-0040, real ngspice, adiabatic slope −1) makes lowering garbage-free — the step is ancilla-clean (`ancillas_restored: true`, 945 dirty → 0), so the default runner `run == eval^R` closes with **zero** re-supplied boundary. The machine-level **money plot** confirms it: energy/step falls at adiabatic slope **−0.997**, crossing below the flat CMOS ½CV² floor at t_ramp ≈ 2.1e-8 s (active logic below CMOS across the whole clock range). |
| **v0.2 dialect** software | ✅ **done** | μ-types + `iter`/`fold`/`unfold`: core + eval + surface syntax + **netlist lowering** + diagnostics; writable `.thse` programs run source→cells (ADRs 0035–0039) |
| **Surface → TISC-1 compiler** (the #1 goal) | 🔄 **M-A + M-B + M-C1 done** | compile `.thse` → a ROM the CPU fetches/executes. **M-A** (non-recursive), **M-B** (reversible branching + **data-dependent iter** + general controlled ops + bare sums, ADR-0043), and **M-C1** (reversible register↔scratch **spill**, ADR-0044) all compile → ROM → run on the CPU → `run_on_cpu == eval` (`theseus-tisc::compile`). The **real `iter Nat fibStep`** (`0022-fib.thse`) → a 24-instr ROM computing Fibonacci on the CPU. M-C1 lifts the leaf ceiling 8 → **14** (straight-line) / **12** (branching) by spilling into scratch `s2..s7`. Authority is the real `step`-iso CPU; the fast byte-sim is a pinned convenience (see compiler section). Milestone path **M-A→M-B→M-C→M-D→M-E** (ADR-0042); **next: M-C2** (Option-B memory bus, ADR-0044 §3 / ADR-0029) |
| **stdlib** | ✅ increment 1–2 | implicit prelude (`Bit`, `Nat`, `notb`, `swapBit`, `cnot`, `toffoli`) via structural-dedup merge — no imports needed; a compositional reversible adder demonstrates arithmetic composes (ADR-0038 constraint respected) |
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
  the **default** ring runner `StepRing::run` (feeds ancilla outputs back)
  `== eval(step)^R` and **physically closes with zero re-supplied boundary** — a
  genuinely self-contained recirculating CPU. (`StepRing::run_open`, the open-loop
  fresh-re-supply variant, agrees with `run` exactly — the equivalence that
  certifies the closure.) The Bennett wrap demotes to the fallback for residual
  K-only-dirty fan-outs (trace/iter chains).

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

## Surface → TISC-1 compiler (the #1 goal) — `theseus-tisc::compile`

The current headline thread. Two execution models coexist (ADR-0041): a `.thse`
program can compile to its **own circuit** (surface→cells, the guide in
[Surface → compiled circuit](surface-to-cells.md)), *or* — the goal here —
compile to a **ROM** the fabricated general **TISC-1 CPU** fetches and executes.
The gap this closes: there was no compiler from surface theseus to the TISC-1
ISA. The design is ADR-0042, decomposed into milestones:

- **M-A — done.** The non-recursive fragment compiles: products +
  composition + adjoint (`Sym`), the **controlled-op idiom**
  (`Distrib;Plus(f,g);Factor` recognized as a macro → `XOR`/`CSWAP` — this is
  where `cnot`/`xor` actually arrive, *not* as `Prim`), and **constant-count
  `iter`** (static unroll). `compile(iso) → Vec<Instr>` → `run_on_cpu` executes
  it on the CPU; the **run-on-CPU differential oracle** proves
  `run_on_cpu(iso,v) == theseus_eval::eval(iso,v)` over the whole domain
  (`cnot`→XOR, `swap`→EXCH, `fibStep`, a constant-count fib). Out-of-scope
  constructs (data-dependent `iter`, `Trace`/`Fold`, unrecognized controlled
  ops) are cleanly **rejected**, not miscompiled.
- **M-B — done (ADR-0043).** Reversible **conditional execution**: general
  controlled ops (multi-instruction arms), **data-dependent `iter`** via guarded
  static unroll, and **bare sums** (tag+payload register rep). Built on the
  SWAPBR skip-block primitive (`emit_skip_block`) + come-from re-convergence
  (ADR-0034); the control is preserved so the merge is clean, with Bennett
  uncompute only for per-arm internal ancillas. The **real `iter Nat fibStep`**
  (`0022-fib.thse`) compiles to a 24-instruction ROM that computes Fibonacci on
  the CPU — `run_on_cpu == eval` over the whole domain (16 inputs), alongside a
  general controlled op and a bare-sum `swapMaybe`.
  - *Oracle authority (review correction).* The acceptance oracle is
    `run_on_cpu`, which runs the ROM on the **real `step`-iso CPU**. A byte-level
    `run_on_cpu_fast` simulator is retained only as a *pinned convenience*:
    routine tests assert `fast == eval` (~40 s), while authoritative `#[ignore]`d
    `*_real_cpu` tests assert `real == eval` (run pre-merge via `--ignored`,
    ~30 min). A green fast-sim alone is *not* acceptance — it would only prove the
    compiler agrees with a second interpreter by the same author (cf. L15).
- **M-C1 — done (ADR-0044).** Reversible register↔scratch **spill**: leaves beyond
  the 8-register file page into scratch `s2..s7` via the existing `Xchg` swap,
  lifting the ceiling to **14** (straight-line) / **12** (branching). Pure
  `compile.rs` — no `State`/`step`/ISA change. A **canonical home layout** with
  page-in/out around each op keeps residence consistent (so the guarded-unroll's
  frozen body stays valid across copies); branch aux registers get a zero-source
  by paging a leaf into a zeroed scratch word. Verified by the real-`step`-iso
  oracle on sampled inputs (whole domain is `2^14`, so real-CPU is *sampled*, ADR-
  0008; fast-sim runs the whole domain). *An adversarial design review corrected
  the first draft's dishonest 8→16 ceiling and caught two data-loss bugs (aux
  zero-source, unroll consistency) before implementation — see ADR-0044 §2.*
- **M-C2 — next (Option-B bus, ADR-0044 §3 / ADR-0029).** For values exceeding the
  register+scratch file: `step` becomes `(CpuState, BusMsg) ↔ …`, memory a
  separately-compiled Fredkin-tree exchange iso through the ring. Its own design
  pass, now that the ring machine exists to inform the protocol.
- **M-D** — full algebraic codegen (heap value rep, `Fold`/`Unfold`/`Trace`,
  calling convention) = *arbitrary* `.thse`; **M-E** — polish + energy-price
  compiled ROMs.

Ergonomics landed alongside: an implicit **stdlib** prelude (no import syntax
needed — structural-dedup merge), a `theseus lower <file> <iso>` verb +
`surface_to_cells` one-call helper (self-checking via the oracle), and the
pipeline guide. Reviewer note: ADR-0042's first draft mis-drew the M-A/M-B line
at "any sum → M-B"; advisor review caught that controlled ops *elaborate* to
`Distrib;Plus;Factor`, so M-A would compile nothing — the boundary is
**recursion, not sums**, and the ADR was revised accordingly.

## Verification norm

Correctness is the **differential oracle**: the lowered cell netlist, simulated,
must equal the reference interpreter (`theseus-eval`) over the *whole finite
input domain* (or a thorough sample where the domain is astronomically large,
e.g. the CPU's 2^160 State). This is theseus's norm — it is *not* the maestro
adversarial-review rule (that governs the separate maestro repo only). The
oracle repeatedly caught real bugs this cycle (a controlled-op lowering defect, a
depth-tracking bug) that unit tests missed.

## Open items & next steps

- **Surface → TISC-1 compiler (the #1 goal) — active.** M-A, M-B **and M-C1**
  done: the real data-dependent `iter Nat fibStep` (`0022-fib.thse`) runs on the
  CPU end-to-end, and values spill to scratch up to a 14-leaf ceiling (ADR-0044).
  **Next: M-C2** — the Option-B memory bus (ADR-0044 §3 / ADR-0029): `step`
  becomes `(CpuState, BusMsg) ↔ …`, memory a separately-compiled Fredkin-tree iso
  through the ring, for values exceeding the register+scratch file. Then M-D (heap
  value rep, `Fold`/`Unfold`/`Trace` = arbitrary `.thse`) → M-E (energy).
- **Energy-price a compiled loop** — now that `iter`/`fold`/`unfold` (and
  compiled ROMs) lower to cells, run one through `theseus-spice::annotate` for a
  joules figure. Natural M-E artifact.
- **Test hygiene (done this cycle):** the ADR-0040 **R1** cell had silently
  broken a cohort of tisc tests (only ever gated by a fast build+clippy cert, not
  the full suite): stale `≤K3`/`≤K2` vocabulary bounds, a `state_instr` harness
  feeding *fabricated* ancilla inits (so it wasn't validating R1 lowering at all),
  and an R1-unaware energy check. All fixed; the tisc + netlist suites are green
  and now actually exercise R1.
- **Physically-closing ring** — ✅ **done.** The ADR-0027 controlled rail-role-swap
  cell (R1, ADR-0040) voids the parity obstruction, so `StepRing::run` (the
  default, closed runner) feeds ancilla outputs back and re-supplies **nothing** —
  zero boundary. The Bennett wrap (`bennett.rs`, 320-slot boundary) remains the
  fallback for residual K-only-dirty fan-outs.
- **CPU money plot** — ✅ **done** (`theseus-tisc::money_plot`,
  `examples/cpu_money_plot.rs`). Sweeps energy/step across the power-clock ramp
  from the committed char-DB fits over the zero-garbage step (4939 active cells,
  487 R1, priced as `cswap-r1`); the machine-level adiabatic **slope −0.997**
  crosses below the flat CMOS ½CV² floor (ngspice-derived committed constant,
  5.03e-9 J/step) at t_ramp ≈ 2.1e-8 s, and the active logic beats CMOS across the
  entire [1e-8, 1e-5] s range.
- **Energy-price a compiled loop** — now that `iter`/`fold`/`unfold` lower to
  cells, run one through `theseus-spice::annotate` for a joules figure (extends
  the M3b/energy story to the software dialect).
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
