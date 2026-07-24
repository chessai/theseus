# Roadmap v2: two tracks, layers, parked items

> **⇒ SUPERSEDED as the forward plan by [TISC-2 roadmap](tisc2-roadmap.md) (2026-07-24).**
> This page records the TISC-1 build (M1→M-E, all merged). The forward direction — the CPU
> that runs *arbitrary* well-formed theseus programs — is now the TISC-2 roadmap. The prior
> "T-loops → widen-PC → GCD" plan here was symptom-chasing (the GCD was blocked by register
> pressure, not PC width) and is retired; the GCD now falls out of TISC-2 Phase A slice A-1
> with no widen-PC.


> **Current status (2026-07-18):** M1, M3a, M3b, **M4-v1**, the **M4-v2
> ring-machine core** (recirculating CPU + reversible branching, Fibonacci on the
> ring at 20× area saving, zero-garbage closure), and the **v0.2 software dialect**
> are all done and merged on `chessai/theseus-v2` (`2faed16`; M-E finale `e444dfb`, the
> M-D.b2 map+reduce pair: f1 MEXCHR-emission `da9ca8f`, f2 array-map `52704b5`, f3 XOR-reduce
> `87c3f7e`; **f3.5 `Ccnot` first user-facing ISA instruction + masked reduce `8a8d8ef`** —
> ISA-growth energy tax moved fib to 7.23e-8 J; **TP-3 LFSR `3b4f123`**). **The #1 goal — the
> surface → TISC-1 ROM compiler — is COMPLETE end to end: `M-A→M-E`.** `.thse` compiles
> to a ROM the CPU fetches/executes (`run_on_cpu == eval`), all the way to **measured
> joules** (M-E: the real `fib` ROM = `6.62e-8 J`, WCET park tax 16.67%, honest B-vs-T
> crossing 4.62e-8 s below the CMOS floor, in-range). Path so far: **M-A +
> M-B + M-C1 + M-C2 (Stage 1 + Stage 2) + M-D.w (WCET+park) + M-D.d (`Trace` reversible
> while-loop) + M-E (energy) done**: non-recursive, branching,
> data-dependent `iter` (the real `fib`), register↔scratch spill, resident memory via
> a reversible `MEXCH` exchange + 1-of-N Fredkin tree, *and* — Stage 1a —
> **`MemState` externalized** into the `(CpuState,MemState)` composite carrier with a
> `system_step` whose oracle equals `eval` (`theseus-tisc::compile`, ADRs 0042–0046).
> **Conjecture C1 is discharged on the *real* re-targeted `select_mem` tree at
> N∈{2,4,8}** (Stage 1a; Layer 1 had only the standalone proxy). The **Option-B
> external memory bus** composite-ring closure is designed + **proven buildable** (a
> 2-agent formal spike + proofs
> ([theory/reversible-memory-closure.md](theory/reversible-memory-closure.md)),
> ADR-0046 §4.5.8) — energy-equivalent to Option A (it buys area + a CPU body
> invariant in N + the Option-C seam, not joules). **Stage 1b DONE** (merged
> `75c5a13`): Stage 1b-A made the exchange **non-degenerate** (`BusMsg` +
> emit/mem/absorb, §6 line-audit passes), and Stage 1b-B ran `system_step` through a
> recirculating **`SystemRing`** whose **closure certificate holds** (`system_step`
> ancilla-clean, `run==run_open==eval^R`, bus-quiescent) — **realizing Theorem 3.5:
> the composite ring `CPU⊗bus⊗memory` closes zero-garbage.** **Stage 2 DONE**: 2a
> (merged `2810bab`/`24ae386`) physically separates memory — `system_step` splits into
> `cpu_pre`/`mem_body`/`cpu_post` on reduced carriers (MemState absent from the CPU
> bodies), a `TwoBodyRing` proves the split `== SystemRing == eval^R`; 2b-lite (merged
> `2741a39`) is a static timing certificate validating the two loops clock in lockstep
> (absolute-tick arrival equations + equal period + phase closure). The compact
> output-padder / materialized two-body netlist (2b-full) is deferred to the memory
> energy-ledger (ADR-0046 §9). **M-D (arbitrary `.thse`) IN PROGRESS** — recursion +
> allocation + addressing thirds done: **M-D.a** (`cf0639a`) compiles list `Fold`/`Unfold`
> (guarded static unroll, real-CPU verified); **M-D.alloc1** (`13ca492`) an occupancy-aware
> allocator (`OccSet`, fixing a reproduced pre-existing M-B silent-corruption bug);
> **M-D.alloc2** (`a8b8fcc`) safely rejects doubly-nested control; **M-D.b** (`c229920`)
> data-dependent memory addressing (`MEXCHR rd,ra` = `reg[rd]↔mem[reg[ra]%N]`, preserving
> the Stage-1b bus-quiescence closure via `ra≠rd`). **M-D.w** (`a2479b9`) added the
> **static-WCET / reversible-park-at-end halting harness** (ADR-0007 TH-1, straight-line
> fragment): `run_on_cpu` runs a compiler-reported budget and idles reversibly to it —
> a fresh-context review caught a latent parked-PC wrap bug (`B+S_max≤256` now enforced)
> and corrected ADR-0007's iteration bound to `|u|+1`. **M-D.d** (`6d180bf`) compiles
> **`Trace` as a reversible while-loop** — a runtime backward jump (come-from
> join-dispatch, ADR-0034) verified `run_on_cpu == eval` on the real `system_step` iso
> (t=3 two-re-entry + informative traces) and closing on the `SystemRing`; an
> adversarial review caught an occ-blind register-aliasing bug (Trace under `Times`, now
> rejected). **M-E** (`8c82630`) prices a compiled ROM in **measured joules** — the thesis
> finale (see above). **Post-finale frontier — a *cluster*, not a line** (surfaced by the
> [test-program roadmap](test-program-roadmap.md) + ADR-0007 **TH-1b** trajectory-
> disjointness sizing law: a reversible while-loop's carrier must satisfy `|u| ≥ |input|`,
> so real variable-`T` loops like GCD/Collatz break today on the 3-leaf loop-carrier cap):
> **M-D.e (loop-carrier spill) [slice-1 DONE, see below] + widen-PC + typed iteration bounds** must
> land *together* to compile a real iterative loop; plus reversible **calling convention** (M-D.d2) and
> **heap** (M-D.c1 widening / c2). **Post-finale landed — the M-D.b2 map+reduce pair**
> (`2e4b5e1`): the compiler **emits `MexchReg`** from a compiled program (f1 `da9ca8f`: a
> `memexch` prim over a 2-word resident array → `run_on_cpu == eval` + closure — the missing
> *front-half* of M-D.b), and **composes it under `iter`** for in-memory array processing:
> **f2** (`52704b5`) = array **MAP** (`g` over the first `k` words, address varying per iteration
> → park-tax histogram `B=30, T(k)=18+4k, {40…0}%`); **f3** (`87c3f7e`) = XOR/parity **REDUCE**
> (`acc ⊕= arr[i]` via the borrow-swap idiom = a non-destructive indexed read; histogram
> `B=24, T(k)=12+6k, {50,25,0}%`). Both tight (numeral `k_max`), via `iter` (no `Trace`/M-D.e).
> Surfaced by the array-map customer. **f3.5 masked reduce DONE** (`8a8d8ef`) via Route β — a new
> user-facing `Ccnot` instruction (bit-0 Toffoli 0b1011 + decoder arm) rather than the refuted
> 7-instr carry-trick; target-first ⇒ no Exch plumbing, d=3 displaced `B=34, T(k)=14+10k`; it
> taxed every program's `step_j` (fib 6.62e-8→7.23e-8 J). **TP-3 LFSR DONE** (`3b4f123`).
> **VALUE-WIDTH slice-1 DONE** (`30c4415`): a new core `Type::Flat(n)` — a width-parameterized
> single-leaf word (cardinality 2^n, `num_leaves=1`) — the foundation for f3.5's byte-wide sequel.
> `Value` unchanged (thermometer); bare byte-`Xor` `run_on_cpu==eval`. NOTE value-width is a
> SEPARATE axis from **M-D.c** (which is MEM_WORDS/heap width — this doc previously conflated them).
> **VALUE-WIDTH LADDER COMPLETE (slices 1-4):** Type::Flat(n) `30c4415`; byte-XOR reduce `ae61941`; Flat(8) Add reduce (numeric sum via fold) `06ecf6b`; **byte-parallel Ccnot + masked accumulate `250f4f7`** (BR-buffer decoder change; acc^=key&elem via fold, caps Flat(4)). Unlocks GF(2) dot-products/gated-reductions/bit-sliced-crypto/keyed-LFSR/most-of-TP-5.
> **M-D.e slice-1 (loop-carrier scratch-spill) DONE** (`7be4a82`+`8f49741`): a reversible `Trace` carries
> a 4th payload leaf by paging a zero-init padding leaf to `Scratch(2)` across the backward jump —
> `MAX_LEAVES_LOOP` 3→4, `n_input+1 ≤ budget` guard (live-input spill still rejected), pinned
> `check_pattern_kleaf`/`mde_spill_walk` recognizer, whole-body paging bracket (2 `Xchg`). Highest-bar
> `lower_trace` change: full L19/L20/L21, Fable impl-review SHIP, **real `system_step`==eval across 5
> back-edges with the spilled leaf live** + `validate()==Ok`/eval^R. Machinery-proof (like M-D.d), not
> yet a useful program. Reviews `docs/reviews/mde-spill-{design-review-r2,impl-review}.md`.
> **BVEQ arc — symbolic-BDD recognizer trust core (2026-07-21/22, tip ~`75ff066`+):** a `bveq` module
> proves two fixed-width bit-vector functions equal via canonical BDD (`biodivine-lib-bdd`, pure Rust),
> breaking the recognizer's "2-operand enumeration wall". **slice-1** (`5eeca29`): `equiv` + `symbolic_exec`
> + `instr_semantics` (a 3-op `Ccnot` proven at full `Flat(8)` in ~113µs — unbuildable as a table). **slice-2b**
> (`0d348e6`): a symbolic NETLIST walk (`bveq_netlist`) proving `netlist(instr) ≡ instr_semantics` — **the
> netlist reconciliation is now a fast DEFAULT check; the 2.6h `#[ignore]` pointwise sweep is RETIRED**.
> **slice-2** (`94b4994`): `recognize_prim` now verifies every emission against the prim table via the
> netlist-reconciled model (`bveq_verify_emission`) — closes the lambda↔instruction edge enumeration was
> blind to. **slice-3a** (`6513f4b`): symbolic `PrimSpec` (Wall-2 REPRESENTATION broken — a `Flat(8)³` prim
> is representable/evaluable/admissible without a 16.7M table; `BvExpr` moved to theseus-core, eval-via-spec,
> `admit_symbolic` BDD mutual-inverse). **slice-3b (`604936a`, MERGED — BVEQ LADDER COMPLETE):**
> symbolic-prim RECOGNITION via selection-by-equiv (iterate the finite arm library, `symbolic_exec` each
> candidate, `equiv` vs the spec-derived `RegTransfer`, first pass wins — sound by construction) — the
> `Flat(8)³` wall-break payoff: a masked-accum prim is recognized to `[Ccnot]` at full `Flat(8)` with the
> 2²⁴ table never built. Fable impl-review SHIP-WITH-FIXES (A-1 remap teeth test + E-1 admit-first doc
> applied, teeth-verified; `docs/reviews/bveq-slice3b-impl-review.md`). Every slice: full L19/L20/L21;
> reviews `docs/reviews/bveq-*`; design `docs/scratch/bveq-*`. Two lessons: a worker who can't run cargo
> can't catch a semantic bug (advisor finishes those); "tests pass" ≠ "tests have teeth" (flip-verify every
> reproducer). **Gate infra (`76fd4ca` + tiering next):** the energy tests fragmented glibc arenas to ~86 GB
> and SIGKILL'd the gate — fixed via `MALLOC_ARENA_MAX=1` in flake.nix (→48 GB). The full tisc suite also
> exceeds the gate's 3000 s window; **gate TIERING (`#[ignore]` heavy binaries, fast-tier iteration gate,
> advisor `--ignored` pre-merge) is the next task.**
> **TP-5 slice-1 (`012aecb`, MERGED — FIRST NONLINEAR CRYPTO LAYER):** Keccak-χ
> (`A_i = a_i ⊕ (¬a_{i+1} ∧ a_{i+2})`, SHA-3's nonlinear step) bit-sliced over Flat(8) — 5 lanes × Flat(8),
> one byte-parallel `Ccnot` (the BVEQ-delivered masked-AND) does the χ gate for 8 Keccak rows at once.
> Hand-composed 17-instr in-place ROM (NEVER lowers — no recognizer/lower change). **8× bit-slicing:
> 5.25e-9 J/row-χ.** Rev-A (garbage-free UNDER-ADJOINT — honest; forward-only leaves ancillas dirty, an L20
> assertion; χ is a bijection not an involution). Fable design-review BUILT a POC + caught the garbage-free
> error; delegated productionization verified (fast tier + heavy system-ring L19 authority, advisor-run
> single-threaded 3/3 green). Reviews `docs/reviews/sbox-chi-design-review.md`, `docs/scratch/sbox-chi-*`.
> **Gate tiering DONE** (`7c78692`): 23 heavy `theseus-tisc` binaries feature-gated (`heavy-tests`) by
> MEASURED runtime so the delegation gate is seconds not ~50 min; advisor runs `--features heavy-tests --
> test-threads=1` pre-merge (CONTRIBUTING + maestro-advisor SKILL updated).
>
> **TP-5 slice-2 (`9e41e6e`, MERGED — first full byte-substitution S-box):** the **PRESENT 4-bit S-box**,
> bit-sliced over Flat(8) — a 47-instr ANF circuit (31 `Ccnot` + 10 `Xor` + 2 `Xori` + 4 `Xchg`), Rust-
> composed (never lowers). **STANDALONE garbage-free** (inputs preserved ⇒ temps uncomputed; stronger than
> χ's under-adjoint). 8× bit-slicing (3.52e-8 J/S-box). Verified vs the published table + system-ring L19
> authority. Design-review also proved **AES-8 INFEASIBLE** on the current 18-slot machine (peak-live 30 >
> 18; 115-gate Boyar–Peralta circuit validated vs an independent GF-inversion table). Reviews
> `docs/reviews/sbox-aes-design-review.md`.
> **T-LOOPS ARC (variable-T loops).** slice-1a **EXACT-K_MAX (`e918e33`, MERGED):** the Trace WCET bound
> `k_max=|u|+1` (full carrier cardinality — the BINDING wall, NOT the PC) is now trajectory-derived
> (`k_exact = max applications-to-exit + 1` via an `exec_instr` walk from every real input, `|u|+1`
> fallback) so a large-carrier fast-exit `Trace` fits under the existing 8-bit PC with ZERO energy tax.
> SOUNDNESS-CRITICAL `lower_trace` change — HIGHEST BAR; Fable adversarial review SHIP (register-layout
> attack closed by the pre-walk F-1 guard; fast oracle proven to detect park-wrap) + real-`system_step`
> H1 authority (2/2, ~2h). The design-review had REFUTED the naive "small GCD fits today" hypothesis
> (4 walls; widen-PC is NOT the first wall — alone it just fits a 99.97%-park program). Reviews
> `docs/reviews/tloops-slice1{,a}-*`. **slice-1b (gcd2 recognizer) POC** de-risked the recognizer
> machinery (pinned `gcd2_hist_bytes`) but found the 2-bit GCD is **BLOCKED-AT-EMISSION**: the minimal
> reversible walk is ~60 instrs vs the ≤40 the WCET-256 budget allows (`docs/reviews/tloops-slice1b-recognizer-poc.md`).
> Unblockers: widen-PC, a `Csub` ISA op, a spare reg, or a smaller-T_max algorithm. **CHOSEN ROUTE (IN
> FLIGHT):** **Slice A — narrow the fetch address-compare** to `A=⌈log₂(B+S_max)⌉` bits (the PC's high bits
> are provably zero) — lowers step_j today AND makes widen-PC nearly free (`docs/scratch/fetch-narrow-compare-design.md`;
> memory `tloops-fetch-narrow-state`). **THEN Slice B widen-PC** (now cheap, B>256) **→ Slice C the 8-bit
> GCD** (recognizer already pinned) = the first genuine variable-T algorithm. Fetch/decoder = HIGHEST BAR;
> the fast oracle is BLIND to fetch so validation is real-`system_step` only.
> **Other follow-ups (each its own slice + review):** AES-8 S-box (recomputation-bounded schedule / wider
> register file); factored fewer-`Ccnot` PRESENT; Rev-B true-in-place χ⁻¹ cleanup; multi-round Keccak-f
> (θ/ρ/π/ι). Design docs `docs/scratch/{md*,valwidth-flat,bveq-*,sbox-*,tloops-*}.md`; reviews
> `docs/reviews/` (L21).
> See [Project status](status.md). The post-v1
> DAG below is largely realized — the physically-closing (zero-garbage) ring and
> the CPU money plot are now **done** (ADR-0040 R1 cell; `theseus-tisc::money_plot`
> shows machine-level adiabatic slope −0.997 crossing below the CMOS floor);
> remaining: energy-price a compiled loop. ADR-0041 names the two execution
models (spatial/per-program circuit vs temporal/stored-program CPU) and positions
the surface→TISC-1 ISA backend as a v3 milestone, with a word-oriented stepping
stone as a v2.5/early-v3 increment. ADR-0042 specifies the concrete compiler
architecture for this backend: a milestone-decomposed design from the
register-only word-subset (M-A) through full algebraic codegen (M-D), with
lowering rules per core `Iso` constructor and a run-on-CPU differential oracle
as the acceptance test.

Post-pivot (ADR-0022): the project is **two-tracked**, and the tracks
are independent for months.

> **Concurrent feedback loop:** the [test-program roadmap](test-program-roadmap.md) is a
> co-evolving stress suite of non-trivial reversible programs. Its Tier-1/2 programs are
> coverage + M-E energy/park-tax artifacts; its Tier-3 programs *productively break* the
> pipeline to name the concrete customer for each remaining milestone (M-D.c widening/heap,
> M-D.d2 calling convention, Trace-with-a-memory-body). The two roadmaps advance in
> lockstep — this one adds capability, that one adds proof and pressure.

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

## Beyond v1: the post-v1 DAG

Ties the [version→feature map](#version--feature-mapping) and the parked-item
wake conditions above into one dependency picture. Same convention as [The
dependency graph](#the-dependency-graph): boxes at the same depth are
parallelizable; edges are "unblocks"; **(wake: …)** is the condition that
makes a parked node live. A node marked **▸** breaks down into its own
sub-DAG below. This is the map of what "v2 and beyond" actually contains.

> **Progress (2026-07-13):** most of this DAG is now realized.
> **T1 ring machine** — ✅ **complete**: recirculating loop + reversible
> come-from/branching (ADR-0034) + balance-compact enabler + ring driver +
> Fibonacci-on-the-ring demo + the *physically-closing zero-garbage* ring (ADR-0027
> rail-role-swap cell R1, ADR-0040: `StepRing::run` closes with zero boundary) +
> the machine-level CPU money plot (slope −0.997 below the CMOS floor). **T2
> software dialect** — ✅
> μ-types + iter/fold/unfold with surface syntax, core, eval, **and netlist
> lowering** (ADRs 0035–0039); remaining: HandshakeLoop, guards, `@tag`, the
> Option-B memory bus, and depth-annotation surface syntax. **T3 silicon tail** —
> unchanged (v0.3+/M5).

### Top level — three tracks

```text
                 v1 COMPLETE  (M1 · M3a · M3b · M4-v1)
                            │
        ┌───────────────────┼───────────────────────┐
        ▼                   ▼                         ▼
  T1 RING MACHINE ▸   T2 SOFTWARE DIALECT ▸    T3 SILICON / OPT TAIL ▸
     (M4-v2)              (v0.2)                   (v0.3+ / M5)
        │                                         (independent — deepens
        │ ring machine is the design input        the physics, not the
        └───────────► Option-B memory bus         machine model)
                      (lives in T2; see below)
```

The **only** hard cross-track edge is T1 → T2's Option-B memory bus: the
recirculating machine is what the bus design is "informed by." T3 is
independent of both.

### T1 — Ring machine (M4-v2) ▸

Recirculating execution so the CPU stops being unroll-only; this is what
unblocks data-dependent branching. The SWAPBR injectivity obstruction
(see `step.rs` module docs) is genuine, so branching is a *design* problem,
not a coding gap.

```text
  balancer buffer-materialization fix        ── v1 follow-up; ENABLER:
   (analytic pricing → streaming buffers)        lets step^T be balanced /
              │                                   simulated at T>1, not just
              ▼                                   eval'd + analytically priced
  recirculating loop primitive (C2.4)   ── (wake: unrolled area unaffordable,
              │                              formally declared by M4/ADR-0030)
              ▼
  reversible come-from / paired branch (Pendulum lineage)
              │
              ▼
  branch-keyed step iso   ──►   SWAPBR / data-dependent control flow works
```

### T2 — Software dialect (v0.2) ▸

```text
  μ-types  (recursive / sized software dialect)      ── foundational
        │
        ├───────────────┬─────────────┬────────────┐
        ▼               ▼             ▼            ▼
   iter/fold/unfold  HandshakeLoop  guards       @tag
                                    proposal   (tagged unions)

  storage sub-chain (parallel to the language work above):

   Option-B memory bus (ADR-0029)  ◄── ring machine (T1)
        │                          ── (wake: fabric-resident storage
        ▼                              dominates the energy report, C3.5)
   Option-C SRAM macro prim        ── (wake: Option-B insufficient at scale)
```

`μ-types` is the one foundational node (iter/fold/unfold fold over recursive
types); HandshakeLoop, guards, and `@tag` are parallel siblings.

### T3 — Silicon / optimization tail (v0.3+ / M5) ▸

```text
  PARALLEL, mostly independent:

    eqsat (ADR-0010)  ·  reduced swing + corners (ADR-0024)  ·
    Xyce (ADR-0025, wake: block size > ngspice patience; validation gate first)

  SEQUENTIAL — the idealization ladder (ADR-0032):

    rung 2 ──► rung 3 ──► rung 4 ──► OpenLane / layout in silicon shell

    S2LAL migration (ADR-0023) ── (wake: leakage-share threshold)
              │
    rung 3+ ──┤
              ▼
    TAPE-OUT (M5)  ── gated on: idealization rung 3+  AND  S2LAL trigger eval
```

The headline **CPU money plot** (below) is a *v1* artifact, not a v2 node —
it reuses `theseus-tisc::energy` and needs nothing above.

## M4-v1 demonstration artifacts & follow-ups

Landed for M4-v1: the TISC-1 datapath, decoder, and `step` cycle compile to
adiabatic cells; the CPU runs **Fibonacci** and a **Feistel round** forward and
backward (via the interpreter — unroll-only, C2.4); and per-clause energy is
computed **analytically** from the real rung0 char DB (the balancer's K0 buffers
are *counted* from `line_arrivals`, not materialized, so a single `step`'s
~1.6 M-buffer balanced netlist never has to fit in RAM). Open items:

| Item | Notes / wakes when |
|---|---|
| **CPU "money plot"** (energy-per-cycle vs. clock ramp) | ✅ **Done** (`theseus-tisc::money_plot`, `examples/cpu_money_plot.rs`). Sweeps `step` energy across `t_ramp` (1e-8…1e-5) via the char-DB `E(T)=k_adia/T+k_leak·T+k_res` fits over the **zero-garbage** step (4939 active cells, 487 R1). Machine-level adiabatic **slope −0.997** crosses **below** the flat CMOS ½CV² floor (ngspice-derived committed constant, 5.03e-9 J/step) at t_ramp ≈ 2.1e-8 s; active logic beats CMOS across the whole range — the "arbitrarily low as you slow the clock, unlike irreversible logic" demonstration at the *machine* level. |
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
