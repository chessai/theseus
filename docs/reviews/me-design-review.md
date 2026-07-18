# M-E DESIGN review (energy-price a compiled ROM — thesis finale)

> Verbatim fresh-context adversarial review (model: Fable), agentId `a8d6cf4906ebc1632`, committed per maestro L21.
> Findings and their test mapping are indexed in [README.md](README.md).

---

All evidence is in hand. Here is the complete review.

---

# Adversarial design review — M-E (`docs/scratch/me-design.md`, theseus-v2 @ 3826108)

Reviewer: fresh-context, read-only. Evidence: actual source at the cited lines, plus one live run of `print_clause_energy_table` (release, t_ramp = 1e-7, full step lowered) to get real numbers.

## (A) VERDICT: SHIP-WITH-FIXES

The wiring plan is sound and the milestone has real (not cosmetic) thesis content — but only after inverting the §2 Q-B decision, which as written commits a physics-model error into the thesis finale, and after fixing the program-level CMOS baseline (Finding 3), without which the artifact is the merged money plot times a constant.

## (B) Numbered findings

**F1 — MAJOR (must fix before implementation): Q-B is inverted. There is no per-word step energy in the committed model, and building one would be *unfaithful*, not more faithful.**
- Where: design §2 Q-B (me-design.md:38–49), §3.2 "per-step-by-word sum", A2 (me-design.md:70–72); vs `compiled_energy` at `crates/theseus-spice/src/annotate.rs:80–95` and `fetch` at `crates/theseus-tisc/src/step.rs:113–140`.
- Why: the energy model of record prices the **static cell inventory** — `netlist.cell_counts()` × per-cell char-DB energy, data-independent (annotate.rs:87–92). `fetch(rom)` lowers a predicate+CNOT entry for **every** nonzero ROM word into one fixed netlist (step.rs:127–140); all of those cells exist in the hardware and are clocked every cycle regardless of PC. So per-step energy is **exactly constant across steps, including idle steps** — the popcount(rom[pc]) variation the design worries about is *logical* activity, which ADR-0028's per-cell-per-cycle model deliberately does not price (claims boundary C3.3, printed at energy.rs:67–72). The design's proposed `step_energy(net_for_word(rom[pc_s]))` would lower a *different netlist per step* — i.e. price a machine that reconfigures its hardware every cycle, which is not the machine that runs (and costs ~2 min lowering per distinct word for nothing). Also note `step_energy` takes a netlist, not a word (energy.rs:157–163); the §2 formula's `step_energy(rom[pc_s], t_ramp)` doesn't exist.
- Fix: headline = `B × step_energy(step_net(fib_rom))`, **exactly**, with the data-independence stated as a model boundary. Rewrite A2 from "agrees within a stated tolerance" to an identity check (`total_j == steps_b × step_bd.total_j` up to float accumulation) — as designed, A2 would "pass" while validating an incoherent per-word model. `by_word_breakdown` may survive as a trace histogram (which words executed when) but must not carry per-word joules.

**F2 — MAJOR: the step netlist must be lowered from THE fib ROM; the merged money-plot numbers are for a different (2-word) ROM, and the balance buffers — 97.6% of step energy — are ROM-shape-dependent.**
- Where: `examples/cpu_money_plot.rs:23–29` (2-word ROM), `tests/energy.rs:33–51` (same 2-word ROM), design §1 "money_plot … done" and A6.
- Why: measured (my run, t=1e-7, 2-word ROM): step total 1.0474e-9 J = active 2.5505e-11 J + **buffer 1.0219e-9 J** (analytic K0 count; 1,078,788 cells total). The committed headline inventory "4939 active cells, 487 R1" (docs/src/roadmap.md:270, status.md:348) is the 2-word ROM's step. Fib's 24-word ROM adds ~12× the fetch tree *and* changes the arrival-skew structure, hence the dominant K0 buffer count, nonlinearly. A6 can therefore only assert *method* consistency (same `balanced_energy` path as `cpu_money_plot`, money_plot.rs:128), never numeric agreement with the merged plot. The A4 golden must be generated from fib's own step net.

**F3 — MAJOR (honesty of the artifact): the program-level CMOS baseline must pay T steps, not B.**
- Where: design §3.3 ("crosses the CMOS floor"), A6; `money_plot.rs:55` (`CMOS_STATIC_E_PER_GATE_J`), `cmos_crossing_t_ramp` money_plot.rs:100–107.
- Why: the reversible ring *must* clock B steps (WCET park, no power-gating — see (C) below). An irreversible CPU has no such obligation: it halts/clock-gates at T. If M-E compares `B×E_step(t)` against `B×E_cmos`, the B cancels — the crossing is identical to the per-step plot and the artifact says nothing new while silently granting CMOS a park tax it never pays. The honest comparison is `B×E_step(t)` vs `T×E_cmos_step`, which shifts the crossing right by exactly B/T (crossing is linear in the ratio, money_plot.rs:106). For fib, B/T ≈ 1.2–3 (see (C)); the crossing moves from ~2.1e-8 s to at worst ~6e-8 s — **still comfortably inside the characterized [1e-8, 1e-5] range**. So the honest claim survives and is *stronger*: "even paying the full WCET park tax that irreversibility never pays, the compiled program crosses below CMOS within the characterized clock range." Headline that.

**F4 — MEDIUM: `clause_report`'s trace is executed-Ops-shaped (T), and idle steps cannot be detected via `decode() == None`.**
- Where: `energy.rs:228–312`, `program_total_j = step_bd.total_j × trace.len()` at energy.rs:290; `asm.rs:277–278`; design §3.1 and §6 Q3.
- Why: `trace: Option<&[crate::decoder::Op]>` is caller-supplied; the only existing caller is the toy 2-Op test (tests/energy.rs:134). Whatever length you feed defines the meaning — fed an executed-Op trace it computes T-energy (under-reports physical joules); fed a B-length trace it is exactly the correct `B × step_energy` (per F1 the fixed multiplier is not an approximation, it is the model). So the design's per-step-by-word summation machinery is not "genuinely needed" — what is needed is only `rom_trace` producing B entries and the halt step T. Trap: `asm::decode(0) = Some(Addi{rd:0, imm:0})` (asm.rs:274–278; confirmed by mdw-design.md §3 — idle is a real ADDI r0,#0 execution). `Option<Op>` will be `Some` on every idle step; T must be defined as the first step at which PC enters the dead zone `[rom.len(), 256)` and stays (sound because post-park PC strictly increments, P2 guard `B+S_max ≤ 256` at compile.rs:2408–2414), never by `decode().is_none()`.

**F5 — MINOR: which machine is priced — `step(rom)` vs `system_step(rom)` — must be stated.**
- Where: design §5 bullet 3 ("the zero-garbage step … consistent with the CPU money plot"); `run_on_cpu` drives `system_step` (compile.rs:2423), `system_step` has 8 phases incl. emit/mem/absorb and the N=4 memory tree (step.rs:259–280).
- Why: the execution/park authority (M-D.w, `run_rom_on_cpu`) is `system_step`; the money plot and `StepRing` price the CPU-only `step(rom)`. For MEXCH-free fib the CpuState evolution is identical, but the Stage-2 thesis machine (the composite CPU⊗bus⊗memory ring, status.md:76) clocks the memory body every step, and its step energy is strictly larger. Pricing `step(rom)` is defensible ("the CPU ring fib actually needs") but the artifact text must say so explicitly, or the number under-reports the composite machine.

**F6 — MINOR: `clause_report` has no `pc_add_br` row, and per-clause rows don't sum to the step.**
- Where: energy.rs:264–284 (fetch, decode_execute, pc_inc only).
- Measured: fetch 2.5647e-12 + decode_execute 8.4007e-10 + pc_inc 5.7042e-13 ≈ 8.43e-10 J vs step total 1.0474e-9 J — ~19% of the step is inter-phase alignment buffers (seq composition skew) plus the unlisted pc_add_br. If M-E prints attribution, add the pc_add_br row and note the composition residual, or the table invites a "components don't add up" objection against the thesis number.

**F7 — MINOR (hygiene): the money-plot example/test ROM comments mis-document their own opcodes.**
- `(3u16 << 12)` is ROTLI (encode: asm.rs:235–239, Addi = opc 0b000), not "ADDI"; `(5u16 << 12)|…` is a *reserved* I-format opcode (decodes `None`), not "XORI" (examples/cpu_money_plot.rs:23–26, tests/energy.rs:33–40). Harmless for energy (data-independent, only fetch popcount matters) — but M-E is about traces; fix the comments before someone builds `rom_trace` fixtures from them.

**F8 — NOTE: A4 golden stability is real; A5 is already enforced.**
- Lowering is deterministic and the char DB is committed RON with a byte-determinism test (chardb.rs:159–162, 268–281); `query` hard-errors out of range per cell (chardb.rs:180–199). A4 can use a tight tolerance; A5 is a one-line negative test. Practical: the fib step lowering is ~2 min — memoize one net across A1–A6 (OnceLock, as tests/energy.rs:43–51 does) and keep the whole-domain sweeps out of the gate (known no-timeout gate hazard).

## (C) B vs T — verdict: **N = B is the physically honest headline**, with T and the ratio reported alongside (the design's Q-A decision is correct)

From the code, there is no sense in which idle is cheaper or the clock stops:
- `run_rom_on_cpu` runs **exactly `budget` = B steps** of the step iso, unconditionally (compile.rs:2422–2429); the fast sim likewise (compile.rs:2564–2574). The P2 guard exists precisely so the machine can *keep stepping* to B without PC wrap (compile.rs:2408–2414).
- The ring runner fires the **entire compact-balanced step netlist every revolution** — `StepRing::run` simulates the full body per revolution with ancilla feedback (ring_run.rs:106–131); there is no gating, skip, or early-out path anywhere.
- Idle is not a fixpoint but real forward motion: rom[pc]=0 fetches nothing, but decode still executes ADDI r0,#0 and PC += 1 (mdw-design.md §3; asm.rs:274–278). M-D.w chose park-by-idle *because* a stopped clock doesn't exist in this machine.

So the machine physically dissipates B × E_step; reporting T-only would under-report joules by up to ~3× for fib. Is B an "honest number to headline" even though park can dominate? Yes — it is the truth, and per F3 it is *survivable*: the crossing stays in-range even charging the reversible side B and CMOS only T. For fib concretely: k_max = |u|+1 = 5 (16-value domain = 4 Nat values × 4 payload; compile.rs:1809–1816), B = |instrs| + (k_max−1)·iter_len (compile.rs:1414–1423) = 24 + 4·iter_len ≈ 70–75 with iter_len = body+10 (compile.rs:343, 1934); T ranges roughly from the mid-20s (k=0) to near B (k=3). Park overhead (B−T)/B ≈ 15%–65% depending on input — material, headline-worthy, and the single most novel measurable of M-E. There is no defensible clock-stop model to appeal to: nothing in the codebase (and nothing in the adiabatic ring story) power-gates, and inventing one for the finale would be exactly the kind of convenient assumption the thesis must not rest on.

## (D) Idle-cost fraction — measured, not estimated

From the clause table run (t_ramp = 1e-7, 2-word ROM, full step = 1,078,788 cells, 1.0474e-9 J):

| component | J | share of step |
|---|---|---|
| step (full, incl. analytic K0 buffers) | 1.0474e-9 | 100% |
| — of which balance buffers | 1.0219e-9 | **97.6%** |
| decode_execute (standalone) | 8.4007e-10 | 80.2% |
| fetch (2 ROM words; appears 2× in step as fetch+unfetch) | 2.5647e-12 | 0.24% (×2 ≈ 0.5%) |
| pc_inc | 5.7042e-13 | 0.05% |

Per-ROM-word fetch cost ≈ 1.3e-12 J; scaled to fib's 24 words × 2 (fetch+unfetch) ≈ 6e-11 J — even generously, **≤ ~5%** of a correspondingly larger step (composition skew caveat per F2). Conclusion: under the design's own logical-activity framing, an idle step would cost ≥ 95% of a full step; under the actual committed model it costs **exactly 100%**. Decode + balance buffers utterly dominate. Therefore "report both B and T" is **essential, not cosmetic** — park steps are full price — and the per-word refinement is noise even before F1 rules it out on principle.

## (E) Does M-E demonstrate anything beyond the merged per-step money plot?

As designed (both sides × B): no — a constant multiple, slope and crossing unchanged. Cosmetic. With F3 applied: yes, two genuinely new things. (1) The **WCET park tax**: reversible execution structurally pays B while irreversible pays T — a program-level cost invisible in any per-step plot, quantified here for the first time (fib: up to ~65% of the physical energy is park). (2) The **honest program-level crossing**: adiabatic B-energy vs CMOS T-energy still crosses inside the characterized range — the thesis claim survives its own strongest self-imposed handicap. Plus the rhetorical finale (a real compiled program, measured joules), which is legitimate but only if (1) and (2) are what the headline says.

## (F) What's SOLID

- The Q-A decision (headline B, report T and ratio) is correct and the code backs it at every layer (compile.rs:2422–2429, ring_run.rs:106–131, mdw-design.md §3).
- The energy plumbing is trustworthy: `analytic_buffer_count` is pinned exactly against real `balance()` insertions on multiple nets (tests/energy.rs:54–98); `total = active + buffer` is asserted; there is no double-counting path (the step row prices one netlist once per step; per-instruction rows are attribution only, never summed into `program_total_j`, energy.rs:287–306).
- The char-DB contract has real teeth: per-cell range check, no extrapolation, committed RON with byte-determinism and an (ignored) drift-guard against fresh ngspice (chardb.rs:180–199, 220–296). A5 is nearly free.
- `wcet()`/`s_max()` and the P2/P1/P3 park guards are exactly the B-definition M-E needs, already enforced on the authoritative path (compile.rs:2367–2441).
- Deliverable §3.1 (`rom_trace`) is the right minimal gap-fill — mirror `run_rom_fast` (compile.rs:2581–2591), which is pinned to the real CPU by the `*_real_cpu` oracles — provided T is defined by PC-in-dead-zone, not by decode() (F4).
- A4's golden is stable (deterministic lowering + committed fits), and it genuinely F-teeths both codegen and the char DB.

Bottom line: keep §2 Q-A, invert §2 Q-B (fixed `B × step_energy` of fib's own step net is the *exact* model value, not an approximation — state data-independence as the ADR-0028 claims boundary), price the fib ROM's own netlist, and make the finale comparison B-vs-T-honest (CMOS pays T). With those fixes M-E is a real thesis finale, not wiring theater.
