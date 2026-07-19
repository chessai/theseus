# Project status — where theseus is (2026-07-18)

One-paragraph orientation for anyone (human or agent) picking this up. theseus
is a **reversible-computing compiler + adiabatic-silicon backend**: a total,
reversible source language elaborates to Core Π (an iso calculus), lowers to a
dual-rail **adiabatic cell netlist** (the k-CSWAP family), and is **priced in
joules** from real ngspice characterization of sky130 cells. The thesis
(ADR-0022): a reversible *program* compiled all the way to *measured energy* on
characterized silicon — and, at the top of the stack, a reversible **CPU**
(TISC-1) whose invertibility is its typecheck.

Canonical branch: **`chessai/theseus-v2`** (unpushed). Tip at this writing:
`da9ca8f` (**M-D.b2.f1 merged** — the compiler now *emits* `MexchReg` from a compiled
program: a `memexch` primitive over a 2-word **resident array**, `Bit`-indexed at runtime,
lowers to a `MexchReg` with `run_on_cpu == eval` + `SystemRing` zero-garbage closure. This
is the missing *front-half* of M-D.b, which had shipped the ISA + closure but **no codegen
path** that emits it — a gap surfaced by the test-program feedback loop. Full L19/L20/L21
discipline: design → Fable design-review (DO-NOT-SHIP → route (i) n=2) → delegate → Fable
impl-review (emission proven sound) → correction task closing 4 test-teeth/guard blockers →
advisor teeth-check + full-suite. Reviews: `docs/reviews/mdb2-*`. M-E's `8c82630` is the
prior finale.). Everything below is merged there. Since M4-v1 this branch landed the
**M4-v2 ring-machine core** (a recirculating CPU), the **v0.2 software dialect**
(μ-types + iter/fold/unfold, source-to-cells), and — the **current headline
thread** — the **surface → TISC-1 ROM compiler** (the #1 goal: a simulated CPU
that fetches and executes ROMs compiled from arbitrary `.thse` code). Milestones
**M-A, M-B, M-C1, M-C2 Layer 1, Stage 1a, Stage 1b (the composite memory ring),
and Stage 2 (two-body separation)** are done: non-recursive *and* branching /
data-dependent `.thse` compiles to a TISC-1 ROM that runs on the CPU, provably
equal to the interpreter — including the real data-dependent `fib`
(`iter Nat fibStep`) — values spill into scratch *and* **resident memory** (a
reversible `MEXCH` exchange + 1-of-N Fredkin tree), *and* **`MemState` is
externalized into a composite `(CpuState, MemState, BusMsg)` carrier whose
recirculating `SystemRing` closes zero-garbage** (the `emit/mem/absorb` exchange
touches no CpuState fields — §6 line-audit — and `run == run_open == eval^R` +
bus-quiescence hold, realizing Theorem 3.5), *and* **the memory is physically
separated into its own body** — `system_step` splits into `cpu_pre`/`mem_body`/
`cpu_post` on reduced carriers (MemState absent from the CPU bodies), a
`TwoBodyRing` proves the split `== SystemRing == eval^R`, and a static timing
certificate (Stage 2b-lite) validates the two loops clock in lockstep (absolute-
tick arrival equations + equal period + phase closure). **M-D (arbitrary `.thse`)
is now in progress** — its recursion + allocation + addressing thirds are done:
**list `Fold`/`Unfold` compile** to the CPU (guarded static unroll, per-copy
displaced `Ctx`), the register allocator is **occupancy-aware** (`OccSet` bitmask —
fixed a reproduced pre-existing M-B silent-corruption bug; nested control safely
rejected), and **memory is data-dependently addressable** (`MEXCHR rd,ra` =
`reg[rd] ↔ mem[reg[ra] % N]`, a register-sourced address that preserves the Stage-1b
bus-quiescence closure — enforced by `ra≠rd`). The **halting-model / static-WCET
harness (M-D.w, `a2479b9`)** is now done — `run_on_cpu` runs a compiler-reported WCET
budget and **parks reversibly at end** (forward-idle through identity-decoding zero
cells, never a fixpoint), discharging ADR-0007 TH-1 for the straight-line fragment
(preconditions: `BR=0`, `B + S_max ≤ 256` so the 8-bit PC can't wrap into live ROM,
`s0=s1=0`; a fresh-context review caught a *latent* parked-PC wrap bug and the real-iso
`s0`-idle condition, both now enforced). And **`Trace` now compiles as a reversible
while-loop (M-D.d, `6d180bf`)** — a genuine *runtime backward jump* on the reversible
CPU (the first construct that isn't guarded static unroll: the exit tag the body flips
defeats unroll, so PC re-enters the body up to `|u|+1` times). The loop is a conserved-
token **come-from join-dispatch** (ADR-0034): a hand-verified skeleton where `δu`
circulates once per iteration so the aux registers are 0 at every body entry, `ctrl`
rides across the backward `SWAPBR` and is uncomputed at the top from the unchanged tag.
Verified `run_on_cpu == eval` on the **real** `system_step` iso for a t=3 trajectory
(two backward re-entries) and an informative `Bit↔Bit` trace, and it **closes on the
`SystemRing`** (`run == eval^R`, bus-quiescent). An adversarial review corrected
ADR-0007's iteration bound (`|u|`→`|u|+1`) and caught an occ-blind register-aliasing bug
(a Trace under `Times` clobbered its sibling — now rejected). Honest limit (8-bit PC):
Traces compile only for tiny carriers (`|u|·|body| + spine ≤ 256`); bigger needs a
widen-PC ADR. Remaining for M-D: a reversible calling convention (M-D.d2, shares the
come-from machinery) and heap value rep (M-D.c, needs MEM_WORDS widening); plus a tracked
pre-existing latent bug — nested-sum `AssocSum` lowers as a no-op and miscompiles (F7,
unreachable today, avoided by M-D.d's prim witness). See the sections below and
ADRs 0033–0046, `docs/scratch/md*.md`, and
[theory/reversible-memory-closure.md](theory/reversible-memory-closure.md).

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
| **Surface → TISC-1 compiler** (the #1 goal) | ✅ **M-A→M-E path complete: `.thse` → ROM → CPU (`run_on_cpu==eval`) → measured joules** | compile `.thse` → a ROM the CPU fetches/executes. **M-A** (non-recursive), **M-B** (reversible branching + **data-dependent iter** + general controlled ops + bare sums, ADR-0043), **M-C1** (register↔scratch **spill**, ADR-0044), and **M-C2 Layer 1** (resident **memory** + a reversible `MEXCH` exchange instruction + 1-of-N Fredkin tree, ADR-0046) all compile → ROM → run on the CPU → `run_on_cpu == eval` (`theseus-tisc::compile`). The **real `iter Nat fibStep`** (`0022-fib.thse`) → a 24-instr ROM computing Fibonacci on the CPU. Leaf ceiling now **14 + N** (straight-line) / **12 + N** (branching), N=4 resident memory words. **Conjecture C1 discharged** (the addressed-exchange tree is whole-domain ancilla-clean). Authority is the real `step`-iso CPU; the fast byte-sim is a pinned convenience. Milestone path **M-A→M-B→M-C→M-D→M-E** (ADR-0042). **M-C2 Stage 1a** (externalize `MemState` into the `(CpuState,MemState)` composite carrier + `system_step` + composite oracle) is **done** (merged `8f27ba0`) — **C1 now discharged on the *real* re-targeted `select_mem` tree at N∈{2,4,8}**, State narrowed 24→20 fields, width-parameterized routing + one `cross_ctrl_fredkin` primitive. **M-C2 Stage 1b-A** (merged `1f59744`) makes the memory exchange **non-degenerate**: a `BusMsg` factor + the **emit/mem/absorb** 3-cycle (mem tree re-keyed on `BusMsg.addr` → touches **zero CpuState fields**, **§6 line-audit passes**), value-level `system_step == eval`, C1 re-verified on `select_mem_on_bus` at N∈{2,4,8}. **M-C2 Stage 1b-B** (merged `75c5a13`) closes it: `system_step` runs through a recirculating **`SystemRing`** and the **composite closure certificate holds** — `system_step` lowers **ancilla-clean**, `run == run_open == eval(system_step)^R`, bus-quiescent — **realizing Theorem 3.5 (the composite ring `CPU⊗bus⊗memory` closes zero-garbage)**. **Stage 1b COMPLETE.** **M-C2 Stage 2** (physical two-body separation) is **done**: **2a** (merged `2810bab` + cert-hardening `24ae386`) splits `system_step` into `cpu_pre`/`mem_body`/`cpu_post` on **reduced carriers** (MemState physically absent from the CPU bodies — the line-audit is now a *structural* fact, not a scan), a `TwoBodyRing` recirculates the three bodies and proves `two_body == SystemRing == eval^R` (authoritative wide-strided tier + direct per-body ancilla certificate); **2b-lite** (merged `2741a39`) adds a static realizability certificate — `TwoBodyTiming` validates the absolute-tick arrival equations + equal period (`D_cpu=D_mem=3028≡0 mod 4`) + phase closure, proving the two loops clock in lockstep (F1 same-revolution servicing, no pipeline skew). **Stage 2 COMPLETE** (2b-full compact-output-padder / materialized netlist deferred to the memory energy-ledger, ADR-0046 §9). **M-D (arbitrary `.thse`) IN PROGRESS** — recursion + allocation + addressing thirds done: **M-D.a** (`cf0639a`) compiles list `Fold`/`Unfold` via guarded static unroll (per-copy displaced `Ctx`, real-CPU oracle green); **M-D.alloc1** (`13ca492`) makes the register allocator **occupancy-aware** (`OccSet` bitmask — fixed a *reproduced pre-existing M-B silent-corruption bug* where a sub-context's "free" register held live data); **M-D.alloc2** (`a8b8fcc`) safely *rejects* doubly-nested skip-block control (reject-don't-corrupt; correct nested compilation deferred); **M-D.b** (`c229920`) adds **data-dependent memory addressing** — a `MEXCHR rd,ra` instruction (`reg[rd] ↔ mem[reg[ra] % N]`) whose address is a register value, preserving the Stage-1b bus-quiescence closure via `absorb=sym(emit)` cancellation (enforced by `ra≠rd`), verified by the real lowered `SystemRing == eval^R`. **M-D control track + M-E COMPLETE:** **M-D.w** (`a2479b9`) static-WCET harness + reversible park-at-end (ADR-0007 TH-1); **M-D.d** (`6d180bf`) **`Trace` as a reversible while-loop** — a runtime backward jump, real-CPU `== eval` + `SystemRing` closure; **M-E** (`8c82630`) prices a compiled ROM in **measured joules** (fib `6.62e-8 J`, WCET park tax 16.67%, honest crossing 4.62e-8 s). **Post-finale frontier** (a *cluster*, per ADR-0007 **TH-1b** + the [test-program roadmap](test-program-roadmap.md)): real variable-`T` loops (GCD/Collatz) need **M-D.e loop-carrier spill + widen-PC + typed iteration bounds** together; plus reversible **calling convention** (M-D.d2) and **heap** (M-D.c). Design docs: `docs/scratch/md*.md`, `mdw/mdd/mdc/me-design.md`; reviews `docs/reviews/` (L21) |
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
- **M-C2 Layer 1 — done (ADR-0046).** Resident **memory** + a self-inverse `MEXCH
  rd, addr` exchange instruction (`reg[rd] ↔ mem[addr]`) whose decode is a **1-of-N
  Fredkin exchange tree**; the compiler spills into it (`Location::Mem`, transient-
  squat zero-parking). Ceiling → **14 + N / 12 + N** (N=4). **Conjecture C1
  discharged**: the tree is whole-domain ancilla-clean at `N ∈ {2,4,8}`. Verified by
  the real-`step` oracle (`run_on_cpu == eval`, sampled). *This is honest Option-A
  capacity* — the area cost of widening `State` is real (it inflated the ring-
  lowering tests past the gate timeout; the heavy ones are now `#[ignore]`d,
  Theorem 6.1), which is exactly why **Stage 1 externalizes** memory.
- **M-C2 Stage 1 — designed + proven buildable (ADR-0046 §4).** Externalize
  `MemState` from the carrier into a **composite ring**: `step` gains a resident
  `BusMsg` port and the memory exchange crosses it, with the composite closure
  certificate (per [theory/reversible-memory-closure.md](theory/reversible-memory-closure.md)
  Thm 3.5). Two adversarial reviews + a **2-agent H3 design spike** resolved it
  (§4.5.0, §4.5.8): buildable with **no seam rotation, no decode latch, no
  prologue/epilogue** — an in-revolution order `fetch;decode_execute′;emit;mem;
  absorb;unfetch;pc_add_br;pc_inc` (`absorb = sym(emit)`, IR re-derives the decode),
  plus one new obligation (a **bus-quiescence check**, since `BusMsg` lowers to data
  ports). Split into **Stage 1a** (composite-aware addressing refactor + externalize
  MemState + value-level oracle — the prerequisite, delegatable now) → **Stage 1b**
  (the composite ring: emit/mem/absorb, re-key the C1 tree, bus-quiescence check,
  certificate at N∈{2,4,8}). Then **Stage 2** (two-body physical separation).
  *Note: M-C2 is not an energy win (Thm 6.1); it buys area, a CPU body invariant in
  N, and the Option-C seam.*
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

- **Surface → TISC-1 compiler (the #1 goal) — active.** M-A, M-B, M-C1, M-C2 Layer 1
  **and M-C2 Stage 1a** done: the real data-dependent `iter Nat fibStep` runs on the
  CPU end-to-end, values spill to scratch *and* resident memory, and **`MemState` is
  now externalized** into the `(CpuState,MemState)` composite carrier — a
  `system_step:(CpuState,MemState)↔(CpuState,MemState)` whose oracle equals `eval`
  (spill-15, store→reload), with the memory exchange re-targeted through a
  width-parameterized addressing layer + a `cross_ctrl_fredkin` cross-factor
  primitive (merged `8f27ba0`, ADR-0046 §4.5.0). **Conjecture C1 is now discharged on
  the *real* re-targeted `select_mem` lowering at N∈{2,4,8}** (Layer 1 had only the
  standalone proxy). `State` narrowed 24→20 fields (ring keeps `step:CpuState↔CpuState`);
  a shared-ISA guard pins `decode_execute′ == proj_CpuState(system_step)` on the
  non-MEXCH domain. **M-C2 Stage 1b-A** (merged `1f59744`) then made the exchange
  **non-degenerate**: a `BusMsg` factor + the **emit/mem/absorb** 3-cycle, with the C1
  tree re-keyed on `BusMsg.addr` so it touches **zero CpuState fields** — **the §6
  line-audit passes** (the Option-B witness). Built as one iso term in the
  in-revolution order; `system_step == eval` at the value level; C1 re-verified on
  `select_mem_on_bus` at N∈{2,4,8}. **M-C2 Stage 1b-B** (merged `75c5a13`) then closed
  it: `system_step` runs through a recirculating **`SystemRing`** and the **composite
  closure certificate holds** — `system_step` lowers ancilla-clean (forced by
  construction), `run == run_open == eval^R`, bus-quiescent — **realizing Theorem 3.5:
  the composite ring `CPU⊗bus⊗memory` closes zero-garbage.** `N=MEM_WORDS` is
  compile-time-fixed (=4); closure is N-independent + the tree's N∈{2,4,8} cleanliness
  is discharged separately. **Stage 1b COMPLETE.** **Stage 2 COMPLETE** (two-body
  separation + 2b-lite timing certificate). **M-D CONTROL TRACK DONE** (theseus-v2
  `2f331da`): list `Fold`/`Unfold` compile (M-D.a), the allocator is occupancy-aware
  (M-D.alloc1, fixing a pre-existing M-B corruption), nested control safely rejected
  (M-D.alloc2), memory is data-dependently addressable (M-D.b `MEXCHR`, Stage-1b
  closure preserved), the **halting-model / static-WCET harness + reversible
  park-at-end** is in (M-D.w `a2479b9`, ADR-0007 TH-1 straight-line), and **`Trace`
  compiles as a reversible while-loop** — a runtime backward jump, verified real-CPU
  `== eval` (t=3 two-re-entry + informative traces) + `SystemRing` closure (M-D.d
  `6d180bf`). **M-E (energy) DONE** (`8c82630`) — **the thesis finale**: a compiled ROM
  is priced in **measured joules** (`rom_program_energy`, `examples/rom_energy.rs`). The
  real `fib` ROM costs **`6.62e-8 J`** (`total = B × step_energy`, the data-independent
  static-inventory model), and — the novel program-level content — carries a **16.67%
  WCET park tax** (`B=24` physical clock cycles, `T=20` productive; the ring clocks the
  park/idle steps too), with an **honest crossing** at `t_ramp ≈ 4.62e-8 s`: even paying
  the full park tax that irreversibility never pays (reversible bills `B` steps, CMOS
  only `T`), fib still crosses below the CMOS ½CV² floor inside the characterized range.
  **Remaining (post-finale) frontier — a milestone *cluster*, not a line** (surfaced by
  the [test-program roadmap](test-program-roadmap.md) + ADR-0007 **TH-1b** sizing law):
  real variable-`T` loop programs (GCD/Collatz) need **M-D.e (loop-carrier spill) +
  widen-PC + typed iteration bounds** together; plus a reversible **calling convention**
  (M-D.d2) and **heap** (M-D.c1 widening / c2). Design docs `docs/scratch/md*.md`,
  `mdw/mdd/mdc/me-design.md`, `tp1-gcd-design.md`; reviews `docs/reviews/`. A pre-existing
  latent `AssocSum` nested-sum no-op miscompile is now loudly rejected (F7).
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
