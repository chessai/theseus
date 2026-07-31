# Adversarial review — B-2.2c (derive per-program w_pc + measure)

**Commit reviewed:** `e81d95b` (`b22c-derive-wpc`, off theseus-v2 `9b4255c`).
**Reviewer:** fresh-context Fable (did not write it; NO cargo — read-only reasoning
against the runtime guards). Implementer: Opus (worker flaked at report/commit;
advisor salvaged + verified).
**Verdict:** **FIX-THEN-SHIP** — the derivation ARITHMETIC is sound (no
guard-violating triple exists); one MAJOR (an existing test the change didn't
update) + doc/coverage MINORs, all resolved.

## Scope

B-2.2c derives `w_pc` per program from the compiled `Lowered` and routes the run +
energy paths through it: `derive_w_pc(budget, s_max, rom_len)` → the minimum safe
PC width ∈ {8,16}; `derive_full_layout_from_lowered(iso, &lowered)` folds it into
the layout used by `run_on_cpu` (compile.rs) and `rom_program_energy_auto`
(energy.rs). Small programs auto-run on the w_pc=8 ring (1 PC field, byte-wide PC
adder); large programs stay w_pc=16 (byte-identical default path).

## The crux — CONFIRMED SOUND

The safety-critical claim is that `derive_w_pc` can never pick a width that then
FAILS the program's own runtime guards. Fable verified, by hand-tracing against
the actual guards:
- **Address-space bound** `(1<<w_pc) >= budget+s_max` matches `run_rom_on_cpu_with`'s
  `budget + s_max_val > pc_addr_space()` → Err (compile.rs ~4620; `pc_addr_space()
  == 1<<w_pc`) EXACTLY.
- **Fetch-injectivity bound** `rom_len <= 1<<(w_pc-1)` matches `fetch_with`'s assert
  (step.rs ~371, confirmed `<=` and `w_pc-1`, not `<`/`w_pc`) EXACTLY.
- Boundaries: reach 255/256/257 → w_pc 8/8/16; rom_len 127/128/129 → 8/8/16. No
  violating triple exists (`w_pc=8` ⇔ reach≤256 ∧ rom_len≤128 ⇔ both guards pass at
  8); no missed win (a 9-bit need rounds to 16 via `div_ceil(8)*8`).
- **The reach==256 equality is genuinely safe** (not an off-by-one): PC ranges over
  `[0, B+S_max)` HALF-OPEN (step.rs ~87), so the max *fetched* address is
  `B+S_max-1 ≤ 255`; only the final, never-fetched PC can reach `256 ≡ 0` (mod-2^w_pc),
  and that final PC is read by neither the terminal check (BR/s0/s1 only) nor
  `unmarshal`. So the guard's `<=` is exactly right and `derive_w_pc` neither
  inherits nor introduces an off-by-one.
- **No shift/overflow UB** (loop shifts clamped via `.min(max_w_pc)`; `bits_fetch`
  starts at 1 so `bits_fetch-1` never underflows). Composition with w_data/num_regs
  is disjoint (`Layout { w_pc, ..base }`; the w_data/num_regs derivations never read
  w_pc). w_pc=16 programs are byte-identical (`Layout{w_pc:16,..base} == base`).

## Findings (all resolved)

- **MAJOR-1 — an existing heavy test broke.** `autoderive_wdata.rs`
  `win_flat4_step_net_shrinks_vs_default_auto` asserts `rom_program_energy_auto(iso)
  .step_j == rom_program_energy(iso, &net_auto).step_j` (rel < 1e-12), where
  `net_auto` was lowered at `derive_full_layout` (→ w_pc=16). After this commit
  `rom_program_energy_auto` prices the tiny `kernel_iso(4)` at w_pc=8, so the two
  diverge ~29% and the assert fires. The change touched compile.rs/energy.rs/b22c
  but not this consumer. **RESOLVED:** the test's `auto` layout now uses
  `derive_full_layout_from_lowered` so both sides price at the same (w_pc=8) layout
  — restoring the original auto==manual equivalence intent. Verified green.
- **MINOR-2 — `rom_program_energy_auto` doc stale.** It still described the layout
  as `derive_full_layout` (w_data only). **RESOLVED:** doc updated to
  `derive_full_layout_from_lowered` (w_data + w_pc).
- **MINOR-3 — the sharpest boundary was never RUN.** The boundary run used
  chain(120), strictly inside both edges; nothing ran a w_pc=8 program at the exact
  fetch edge. **RESOLVED:** the boundary test now uses chain(128) — rom_len==128 ==
  1<<(w_pc-1), the sharpest w_pc=8 case — and asserts rom_len==128, w_pc==8, and a
  real run==eval (264s). (The reach==256 edge needs a loop to construct with a small
  rom; Fable's half-open-interval reading proves it safe, so it is cited, not run.)
- **MINOR-4 (not fixed, noted) — the exhaustive sweep hardcodes the guard
  inequalities** as literals rather than a shared `pc_guards_ok(...)` predicate.
  They match the runtime today (verified); a future guard edit could diverge while
  the sweep stays green. Left as a follow-up (low risk; the sweep is correct today).

## Blast radius (intended)

Every small-program `run_on_cpu` test in the suite (lfsr, valwidth, feistel, sbox,
composite, …) now silently routes to the w_pc=8 ring — the intended payoff,
correctness resting on B-2.2b's proof. The full heavy authority confirms none
regress.
