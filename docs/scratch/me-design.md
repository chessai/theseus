# M-E — energy-price a compiled ROM (the thesis finale)

**Status:** design of record (pre-review). ADR-0042 M-E: "energy annotation of compiled
ROMs via the char-DB fits (`theseus-tisc::energy`)." The climax of the #1 goal: a
reversible **program**, compiled to a TISC-1 ROM, run on the CPU, priced in **measured
joules** from the committed rung0 ngspice characterization. Mostly wiring — the modeling
choice in §2 is the one thing that needs care.

## 1. What exists (build the gap, not this)
- `energy.rs`: `step_energy(step_net, db, t_ramp)`, `balanced_energy(net, db, t_ramp)`
  (active cells via `compiled_energy` + analytic K0-buffer count), and
  `clause_report(rom, db, t_ramp, step_net, trace: Option<&[Op]>)` — which already
  computes `program_total_j = step_bd.total_j × trace.len()`.
- `money_plot.rs` `cpu_money_plot`: energy/**step** across the power-clock ramp (done;
  slope −0.997, crosses the committed CMOS floor `1.01922e-12 J/gate`).
- `chardb.rs` `rung0_measured()`: committed real-ngspice fits for `cswap-k0..k3`,
  `cswap-r1`, `adiabatic-tg`. Valid `t_ramp` range **[1e-8, 1e-5] s** (hard contract —
  `query()` errors out of range; no extrapolation, ADR-0028 C3.1).
- **Key fact (energy.rs):** `decode_execute` lowers ALL 14 predicated instructions every
  step, so **per-step energy is ~instruction-independent** — the shared decode dominates;
  per-step cost varies only with fetch/unfetch = `popcount(rom[pc])` CNOTs.

## 2. The model — the one real decision (needs review)

`program_energy = Σ_{s=0}^{N-1} step_energy(rom[pc_s], t_ramp)`, where `pc_s` is the PC at
step `s`. Two questions:

**(Q-A) What is `N` — the step count?** The TISC-1 is a recirculating ring; to run a
program you clock it for the **static WCET budget `B`** (M-D.w) — and the adiabatic power
clock keeps running through the **park/idle steps** after the program halts (there is no
power-gating; idle is *reversible forward motion*, not a stopped clock, M-D.w §3). So the
**physically honest energy is `N = B` (WCET)** — the machine really does fire the step
netlist `B` times. The *productive* trace length `T ≤ B` (steps until halt) is a
secondary "useful-work" figure. **Decision: report BOTH, headline `B` (physical run),
report `T` (productive), and their ratio (the idle overhead of the WCET bound).** Do NOT
silently report `T`-only — that under-counts the real joules.

**(Q-B) Fixed step energy, or per-step-by-word?** Idle steps fetch `rom[pc]=0`
(`popcount 0` → no fetch CNOTs), so an idle step costs *slightly less* than a
fetch-heavy instruction step. The faithful sum is **per-step, keyed on the fetched word**:
`Σ_s step_energy(net_for_word(rom[pc_s]))`. The `trace.len() × fixed_step_energy`
approximation (clause_report today) is a **cross-check**, not the headline — state the
approximation error (the decode dominates, so it is small, but quantify it). The design
prefers the per-step-by-word sum for the headline number, with the fixed approximation
asserted to agree within a stated tolerance.

> Honesty note: if the per-word fetch variation turns out negligible (< a few %), the
> fixed `B × step_energy` is an acceptable headline **with the variation reported as the
> error bar** — but that must be MEASURED, not assumed. The review should force this.

## 3. Deliverables (theseus-tisc; energy path, NOT the compiler control flow)
1. **Trace extraction** — `fn rom_trace(rom, init: StateVals, budget) -> Vec<(u8 /*pc*/, u16 /*word*/, Option<Op>)>`
   over `budget = B` steps (mirror the fast-sim loop, compile.rs; use `asm::decode`).
   Records the fetched word each step *including idle steps* (word 0 past ROM end).
2. **Program energy** — `pub fn rom_program_energy(rom, init, budget, db, t_ramp) -> Result<ProgramEnergy, _>`
   returning `{ total_j (physical, N=B), productive_j (N=T), steps_b, steps_t,
   by_word_breakdown }`. Per-step-by-word sum (§2 Q-B); the fixed-step approximation as a
   cross-check field.
3. **Artifact example** — `examples/rom_energy.rs`: compile the real `iter Nat fibStep`
   (`0022-fib.thse` → 24-instr ROM), run `fib(n)` on the CPU, price it in joules at a
   chosen `t_ramp`, and sweep the ramp (reuse the money-plot sweep) to show the program's
   energy falls at the adiabatic slope and crosses the CMOS floor. Print the **finale
   headline**: "fib(N) compiled to a K-instr ROM, executed B steps (T productive) on the
   reversible CPU, total energy = E J @ t_ramp = t s — adiabatic slope m, crossing the
   CMOS ½CV² floor at t_ramp ≈ …".

## 4. Acceptance (every finding → a test, L21)
- **A1** `rom_program_energy` on the fib ROM is `> 0`, and `total_j == Σ per-step
  energies` (the sum is exactly the per-step accumulation — no double count).
- **A2 (cross-check)** `total_j` (per-step-by-word) agrees with `B × step_energy`
  (fixed) within a **stated tolerance** (the per-word fetch variation), asserted — this
  quantifies the §2 Q-B approximation error, and catches a regression in either path.
- **A3 (B vs T)** `steps_b == wcet(iso)` and `steps_t ==` the actual halt step for the
  input; `productive_j ≤ total_j`; the idle overhead `= total_j − productive_j ≥ 0` and
  equals the idle steps' summed energy.
- **A4 (golden)** the fib ROM's `total_j` at a pinned `t_ramp` matches a golden value
  within tolerance (F-teeth: a codegen or char-DB regression moves it, test fails loud).
- **A5 (range contract)** a `t_ramp` outside `[1e-8, 1e-5]` returns the char-DB range
  error, not an extrapolated number (ADR-0028 C3.1) — negative test.
- **A6 (money-plot consistency)** the program energy at ramp `t` equals
  `Σ_s cpu_money_plot-style step energy` — the M-E number is the per-step money-plot
  integrated over the trace, i.e. M-E is consistent with the merged CPU money plot.

## 5. Non-goals / obligations
- No new compiler behavior, no ISA change, no lowering change — M-E only *reads* a
  compiled ROM's execution and *sums* committed char-DB fits. (So the CLAUDE.md
  control-flow/lowering adversarial-review bar does not strictly apply — but per L21 this
  milestone still gets a design review AND an implementation review, both committed to
  `docs/reviews/`, with the finding→test index updated. M-E is the first milestone under
  L21 — capture the reviews at the time.)
- `t_ramp` in-range only (char-DB contract). Energy from the committed `rung0_measured()`
  fits (real ngspice), not fabricated constants.
- The step netlist is the zero-garbage step (R1-lowered, `run == eval^R`) — price THAT
  (the closing machine), consistent with the CPU money plot.

## 6. Open questions for the design reviewer (break this)
1. **Is `N = B` (WCET) the honest physical step count, or `T`?** The ring clocks the
   idle steps (reversible forward motion, no power-gating) — so `B` is what the machine
   physically runs. Is that right, or is there a halt/clock-stop convention that makes
   `T` the physical number? (This decides the headline joules.)
2. **Do idle steps really cost ~a full step's energy?** decode_execute (all 14 ops) fires
   every step regardless of the fetched word; only fetch/unfetch scale with
   `popcount(rom[pc])`, which is 0 for idle. So idle ≈ step − fetch. Quantify: what
   fraction of step_energy is fetch/unfetch vs decode? (If fetch is small, idle ≈ full
   step; the WCET park overhead is then nearly `(B−T) × step_energy` — potentially large.)
3. **Is `clause_report`'s `trace.len() × step_energy` counting the right trace?** Does its
   `trace` include idle steps or only executed Ops? If Ops-only, it computes `T`-energy,
   not `B` — reconcile with §2 Q-A.
4. **Does the per-word `step_energy` even vary enough to matter?** If not, simplify to
   `B × step_energy` + a reported error bar (but MEASURE first).
5. **Golden stability:** is the fib ROM's energy deterministic (committed char-DB fits +
   fixed lowering), so A4's golden is stable across runs?
