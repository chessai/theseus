# M-E — energy-price a compiled ROM (the thesis finale)

**Status:** design of record — **reviewed** (fresh-context Fable, verdict SHIP-WITH-FIXES;
all findings folded in — see `docs/reviews/me-design-review.md`). ADR-0042 M-E: "energy
annotation of compiled ROMs via the char-DB fits (`theseus-tisc::energy`)." The climax of
the #1 goal: a reversible **program**, compiled to a TISC-1 ROM, run on the CPU, priced in
**measured joules** from the committed rung0 ngspice characterization. The review turned
this from cosmetic wiring into a real result — the **WCET park tax** (§2) is the novel
program-level measurable no per-step plot can show.

## 1. What exists (build the gap, not this)
- `energy.rs`: `step_energy(step_net, db, t_ramp)`, `balanced_energy(net, db, t_ramp)`
  (active cells via `compiled_energy` + analytic K0-buffer count — pinned exactly against
  real `balance()` insertions, tests/energy.rs:54-98), `clause_report(rom, db, t_ramp,
  step_net, trace: Option<&[Op]>)` (computes `program_total_j = step_bd.total_j ×
  trace.len()`).
- `money_plot.rs` `cpu_money_plot`: energy/**step** vs power-clock ramp (done; slope
  −0.997, crosses committed CMOS floor `CMOS_STATIC_E_PER_GATE_J = 1.01922e-12 J/gate`).
- `chardb.rs` `rung0_measured()`: committed real-ngspice fits (`cswap-k0..k3`, `cswap-r1`,
  `adiabatic-tg`); valid `t_ramp` **[1e-8, 1e-5] s**, hard no-extrapolation contract.

## 2. The model — corrected per review

**The committed energy model prices STATIC CELL INVENTORY (data-independent).** Every
cell of `fetch(rom)`/`decode_execute` exists in the hardware and is clocked every cycle
regardless of PC (annotate.rs:80-95; step.rs:113-140). So **per-step energy is *exactly*
constant across all steps — including idle steps** (F1). The "per-word variation" my first
draft worried about is *logical* activity, which ADR-0028's per-cell-per-cycle model
deliberately does NOT price (claims boundary C3.3). There is no `step_energy(word)`; there
is one `step_energy(step_net)`. **Headline energy = `B × step_energy(step_net)`,
EXACT** — a model *value*, not an approximation. (Measured idle-cost fraction: a full step
is 97.6% balance buffers + decode; fetch/unfetch is ~0.5%. Idle costs ~100% of a step.)

**Step count `N = B` (WCET), not `T`** (Q-A, confirmed physical, review §C). The TISC-1 is
a recirculating ring with no power-gating: `run_rom_on_cpu` fires the full step netlist
**exactly `B` = `wcet(iso)` times** (compile.rs), park/idle steps included — idle is
reversible forward motion, not a stopped clock (M-D.w). The machine physically dissipates
`B × step_energy`. The **productive** trace length `T ≤ B` (steps until PC first enters
the dead zone `[rom.len(), 256)`) is a secondary figure.

**The novel measurable — the WCET park tax.** `park_tax = (B − T)/B` is the fraction of
physical energy spent idling to the static WCET bound. For fib it is ≈ **15%–65%**
depending on input (`B = |instrs| + (k_max−1)·iter_len`, `k_max = |u|+1`; `T` varies with
the actual iteration count). This program-level cost is invisible in any per-step plot and
is M-E's real content.

**The honest comparison (F3) — reversible pays `B`, irreversible pays `T`.** Comparing
`B×E_step` against `B×E_cmos` cancels the `B` and reproduces the merged per-step crossing
(cosmetic, and silently taxes CMOS for a park it never pays). An irreversible CPU
clock-gates at halt (`T` steps). So the honest finale compares **`B × E_step(t)`
(reversible, with its park tax) vs `T × E_cmos_step` (irreversible, no park)**. The
crossing shifts right by `B/T` (≈1.2–3 for fib) but stays comfortably inside the
characterized `[1e-8, 1e-5]` range. **Headline the survival:** *"even paying the full WCET
park tax that irreversibility never pays, the compiled program still crosses below the
CMOS ½CV² floor within the characterized clock range."*

## 3. Deliverables (theseus-tisc energy path — reads a ROM, sums committed fits)
1. **`rom_trace`** — `fn rom_trace(rom, init: StateVals, budget: usize) -> RomTrace`
   mirroring `run_rom_fast` (compile.rs:2581-2591, pinned to the real CPU by the
   `*_real_cpu` oracles). Runs `budget = B` steps, returns `{ steps_b: B, steps_t: T }`
   where **`T` = the first step at which PC enters the dead zone `[rom.len(), 256)` and
   stays** — NOT `decode()==None` (idle decodes to `Some(Addi r0,#0)`, F4). Optionally a
   per-step `(pc, word)` histogram for attribution (NO per-step joules — F1).
2. **`rom_program_energy`** — `pub fn rom_program_energy(iso_or_rom, init, db, t_ramp) ->
   Result<ProgramEnergy, _>` returning `{ total_j = B × step_energy, productive_j =
   T × step_energy, steps_b: B, steps_t: T, park_tax = (B−T)/B, step_j }`. Lower the step
   net **from THIS ROM** (`step(rom)`), not the money-plot 2-word ROM — fib's 24-word ROM
   has ~12× the fetch tree and a different (dominant) K0-buffer count (F2). Memoize the
   ~2min lowering in a `OnceLock` (F8). Price **`step(rom)`** (CPU-only, consistent with
   the money plot); state that the composite `system_step` adds the always-clocked memory
   body (a bounded increment, F5).
3. **Artifact `examples/rom_energy.rs`** — compile the real `iter Nat fibStep`
   (`0022-fib.thse` → 24-instr ROM), run `fib(n)`, price it, and sweep the ramp. Print the
   finale: *"fib(N) → K-instr ROM; executed B steps (T productive, park tax P%) on the
   reversible CPU; total E J @ t_ramp = t; adiabatic slope m; crosses below CMOS (which
   pays only T steps) at t_ramp ≈ …, inside the characterized range."*

## 4. Acceptance (every finding → a test — L20/L21)
- **A1** `rom_program_energy(fib)` > 0, and `total_j == steps_b × step_j` **exactly** (up
  to float accumulation) — the fixed multiplier IS the model (F1), so this is an
  **identity**, not a tolerance.
- **A2 (B vs T)** `steps_b == wcet(iso)`; `steps_t ==` the actual halt step (PC enters the
  dead zone) for the input; `productive_j == steps_t × step_j`; `park_tax == (B−T)/B ≥ 0`;
  the idle energy `total_j − productive_j == (B−T) × step_j`.
- **A3 (golden, F-teeth)** fib's `total_j` at a pinned `t_ramp` matches a golden value
  (deterministic: committed char-DB fits + fixed lowering — F8). A codegen or char-DB
  regression moves it → test fails loud. Golden from **fib's own step net** (F2).
- **A4 (honest crossing)** the program-level crossing — `B × E_step(t)` vs
  `T × E_cmos_step` — falls inside `[1e-8, 1e-5]`; assert the crossing `t_ramp` and that
  it is `≥` the per-step crossing (the park tax + T-CMOS shifts it right, never left).
- **A5 (range contract)** `t_ramp` outside `[1e-8, 1e-5]` → the char-DB range error, not
  an extrapolated number (negative test, F8/A5 already enforced by chardb.rs:180-199).
- **A6 (method consistency, NOT numeric)** `rom_program_energy` uses the same
  `balanced_energy` path as `cpu_money_plot` (money_plot.rs:128) — assert the per-step
  `step_j` equals the money-plot step energy *for the same step net*, not the merged
  plot's 2-word number (F2).
- Keep the ramp sweeps + real-CPU tiers `#[ignore]`d / out of the default gate
  (no-timeout gate hazard); memoize the fib step net across A1–A6 (F8).

## 5. Non-goals / obligations
- **No compiler behavior change, no ISA/lowering change** — M-E only *reads* a compiled
  ROM's execution and *sums* committed char-DB fits (so the CLAUDE.md control-flow/lowering
  adversarial-review bar doesn't strictly apply — but per L21 this milestone still gets an
  implementation review committed to `docs/reviews/`, with the finding→test index updated).
- `t_ramp` in-range only. Energy from committed `rung0_measured()` fits (real ngspice), no
  fabricated constants. Price the zero-garbage `step(rom)` (R1-lowered, `run == eval^R`).
- Fix the money-plot example/test ROM opcode comments (F7: `3<<12` is ROTLI not ADDI;
  `5<<12` is a reserved I-format opcode) before building `rom_trace` fixtures from them.
- If `clause_report` prints per-clause attribution, add the missing `pc_add_br` row and
  note the ~19% inter-phase composition residual (F6) — or don't print attribution.

## 6. Resolved by the design review
Q-A (headline `B`) confirmed physical. Q-B (per-word) **inverted** → exact `B×step_energy`.
Idle cost measured ~100% of a step → park tax is essential, not cosmetic. M-E demonstrates
two genuinely new things beyond the per-step money plot: the **WCET park tax** and the
**honest program-level crossing** that survives it. See `docs/reviews/me-design-review.md`.
