# M-D.e variable-T loops — scoping (advisor, 2026-07-31)

**Status:** advisor scoping from a read-only survey of theseus-v2 `b08cb01`
(post B-2 efficiency line). Purpose: map the ACTUAL terrain for running a useful
variable-iteration-count program (GCD/Collatz) on the CPU, because the older docs
(`status.md`, `mde-spill-*.md`, `widen-pc-design.md`) are partly STALE.

## Stale-doc correction (read this first)

`status.md` and the mde-spill/widen-pc docs describe the frontier as "widen-PC +
typed iteration bounds" and the pre-widen-PC world where the 8-bit PC caps B at
256 and blocks GCD. **Widen-PC has since MERGED.** The code now: `compile.rs:4019`
caps Trace WCET at `PC_ADDR_SPACE` (2^16) not 256; `derive_w_pc` (`compile.rs:3188`,
B-2.2c) derives per-program PC width ∈{8,16}; `tests/widen_pc_payoff.rs` proves a
B=300 program runs on real `system_step` crossing PC 256. So the *original* GCD
blocker ("8-bit PC") is CLOSED. `tisc2-roadmap.md` (header 2026-07-31) is the
current authority; `status.md` lags.

## Today's variable-T capability (what the Trace loop CAN / CANNOT do)

**CAN** (all CPU-proven via `run_on_cpu == eval` on real `system_step`):
- Runtime data-dependent back-edges (M-D.d `6d180bf`); a spilled carrier leaf
  survives 5 backward jumps (M-D.e slice-1 `7be4a82`/`8f49741`, `tests/mde_spill.rs`).
- Trajectory-derived exact `k_max` (slice-1a `e918e33`, `tests/exact_kmax.rs`).
- Nested Trace / loop-under-⊗ / branch-in-branch compose (A-3/A-4/A-5).
- Carry ONE over-cap **zero-init padding** leaf in scratch across the back-edge
  (`MAX_LEAVES_LOOP` 3→4, `compile.rs:428`).
- B > 256 (widen-PC merged, `tests/widen_pc_payoff.rs`).

**CANNOT** (this is what "machinery-proof, not a useful program" means):
1. **Spill a LIVE-INPUT leaf** — only a zero-init padding leaf may spill;
   `compile.rs:3690` hard-rejects `n_input + 1 > carrier_reg_budget(=4)`. Live-input
   relocation is an explicit separate future slice (mde-spill-design-review-r2 §6).
2. The spill body is a **pinned synthetic recognizer** (`check_pattern_kleaf`/
   `mde_walk_bytes`, `compile.rs:2298/2318`) — a 13-instr thermometer walk, not a
   general k-leaf map.
3. **No real algorithm has ever been lowered into a Trace and run on the CPU** —
   every green Trace test uses a synthetic `build_walk_body`/`mde_walk_bytes`
   bijection chosen to be Trace-legal, not GCD/Collatz.

## The precise gap to a useful program

GCD is synthesized as a step-body **circuit** (`synth.rs` `transcribe_bennett_general`;
`tests/synth_gcd2*.rs` verify the `gcd2_hist_bytes` rule via `exec_instr` over all
256 canonical states + net-zero ancilla) — but **never wrapped in a `Trace` and run
as a loop**. grep confirms: no test file has both `gcd` and `Trace`;
`synth_gcd2*.rs` never call `run_on_cpu`/`system_step` in a loop (`synth_gcd2.rs:35`
even prints "B vs 256: EXCEEDS 256"). The a1-gcd hand-walk was ABANDONED (a ≤40-instr
hand-built reversible GCD is infeasible; reversible-clear doubles the body). The
recommended replacement — "(a) general synthesizer + (b) larger B" — has BOTH landed,
so the surviving gap is "wire it together + measure," not "invent the body."

Concrete missing capabilities, in likely bite-order for gcd2:
1. **Compose `lower(Trace(u, gcd2_body))` and run it.** Unknown whether the
   synthesized body satisfies `lower_trace`'s preconditions: canonical input homes
   (`compile.rs:3652`), `homes_in==homes_out`, a/b leaf-count match (`:3607`).
2. **`iter_len ≤ 128` come-from cap** (`compile.rs:4033`, NOT in the stale docs):
   the per-iteration ROM span is capped at 128 instrs because the come-from δu is
   minted into an 8-bit BR + sign-extended. A ~50-100-instr Bennett GCD body +
   LOOP_OVERHEAD(10) is close to / past this → needs the **W_BR widen / two-byte δu
   mint** (design a6.4 §3.4; twin skip-block guard `compile.rs:1020`).
3. **Live-input spill** (gap #1): the 2-bit GCD carrier `[tag,a,b,h]` is ~4 leaves,
   right at `n_input+1 ≤ 4`; a wider carrier immediately hits the unbuilt live-input
   relocation spill (LARGE, highest-bar, own review).

## Ranked next slices (smallest-useful-first)

1. **★ PROBE: compose gcd2 body under a Trace + run the fast oracle
   (`run_on_cpu_fast == eval`).** Diagnostic spike — either the first real variable-T
   algorithm on the CPU, or a precise measured blocker list (which guard, with the
   exact B/iter_len/n_input/leaf-count). LOW risk, highest information; grounds the
   next real slice. Mirrors the mde-spill "spike first, then scope" methodology.
2. **W_BR widen / two-byte δu mint** (`compile.rs:4033` + `:1020`; a6.4 §3.4). Unlocks
   loop bodies > 128 instrs. Small-medium, MEDIUM risk (come-from arithmetic).
   On the GCD path IFF the body exceeds 128 instrs (the probe tells us).
3. **Live-input relocation spill** (`compile.rs:3690`; r2 §6). Unlocks wide-carrier
   variable-T programs. LARGE, HIGHEST BAR (lower_trace regalloc + reversibility
   across the back-edge), own design+review.
4. "Typed iteration bounds" — partly delivered by exact-k_max; residual scope not
   concretely specified. Low priority.

**Plan:** run the PROBE (slice 1) first — it converts the vague "widen-PC + typed
bounds" backlog into a concrete, measured blocker list before committing to the
MEDIUM (W_BR) or LARGE (live-input spill) slice.

## PROBE RESULTS (2026-07-31) — the fork is RESOLVED

Two diagnostic spikes ran (both truthful, no guards weakened):

**Probe 1 — GCD-under-Trace** (branch `gcd-trace-probe`, test `gcd_trace_probe.rs`,
commit `3bd6f0d`). The gcd2 body IS a legal Trace bijection (4 leaves `[tag,a,b,h]`,
total 256/256, injective, `body_in==body_out`); **every `lower_trace` skeleton guard
PASSES** (leaf cap, canonical-homes, a/b-match, iter_len, live-input barrier
`n_input+1≤4` passes at exactly 4≤4, WCET). It does NOT lower — the first blocker is
the **general-synth operand-word budget** (`compile.rs:1932` `NeedsProvisioning`;
`need_aux = n + 1 + node_lines`): gcd2's **degree-7 ANF → node_lines=35** → requires
44 operand words, only 10 free (4 carriers + the loop's 4-register aux frame exhaust
the register file → 6 scratch words). **So the Trace loop machinery is READY; the
gcd2 wall is operand provisioning (widen-state / A-6.4), NOT the loop machinery, NOT
W_BR, NOT live-input spill.** (Verified against the code: `need_aux = n+1+node_lines`,
`required = n+need_aux`, `available = n+free.len()`.)

**Probe 2 — low-degree variable-T fit** (branch `vart-lowdeg-probe`, test
`vart_lowdeg_probe.rs`, commit `43e8ac2`). **A genuine variable-T program FITS the
current budget.** halve-until-odd (2-adic valuation `v_2`), built as a
manifestly-bijective **bit-rotation** of `(tag‖h‖v)` (node_lines=**1** — linear body,
operand budget a non-issue), LOWERS under a Trace (at k=2,hb=2: iter_len=107, B=646,
k_max=6, 111-instr ROM) and `run_on_cpu_fast == eval` on all inputs, with the
iteration count genuinely tracking v_2 (T varies with input). NOTE: a naive
convergent countdown (`c-=1` until 0) is NOT Trace-legal (c=1→0 and c=0→0 collide →
not injective); a real reversible variable-T body must be a permutation orbit with a
history leaf. Width sweep: node_lines stays 1 everywhere; it lowers iff `1+hb+k ≤ 6`
bits — the wall is **`iter_len ≤ 128`** (`compile.rs:4033`, the W_BR=8 sign-extended
back-jump δu), which grows with carrier bit-width.

### Resolved direction (ranked, updated)
1. **★ IN PROGRESS — harden halve-until-odd into the FIRST merged real variable-T
   program** (branch `vart-halve-v1`). Tests-only; validates the real-`system_step`
   authority pipeline for a real Trace program (fast-oracle ≠ real system_step — L15).
   No new machinery. Advisor runs the real T3 authority + Fable review.
2. **W_BR widen / two-byte δu mint** (`compile.rs:4033` + `:1020`; a6.4 §3.4) — lifts
   the `iter_len ≤ 128` cap so WIDER variable-T programs (and low-degree GCD-family
   bodies) lower. MEDIUM risk (come-from arithmetic). The next capability slice.
3. **widen-state / A-6.4 operand provisioning** — DEFERRED; needed only for
   HIGH-DEGREE bodies (gcd2 degree-7, node_lines=35). LARGE; touches state layout
   (undoes some B-2 optimization). Do after W_BR, when a high-degree program is the
   target. **NOT mandatory for a first useful variable-T program** (probe 2).
4. Live-input relocation spill — unchanged (LARGE, own review), for wide LIVE
   carriers; not on the near-term path.

## Code landing sites (all `crates/theseus-tisc/src/compile.rs` unless noted)

- `lower_trace` (`:3570`) — loop state machine. Guards: leaf cap `MAX_LEAVES_LOOP=4`
  (`:428`/`:3626`), live-input spill barrier (`:3686`/`:3690`), canonical-homes
  (`:3652`), a/b leaf match (`:3607`), **iter_len ≤ 128 come-from cap (`:4033`)**,
  WCET cap (`:4019`), δu mint (`:4052`).
- WCET/S_max tree fold: `Lowered::wcet` (`:2961`), `s_max` (`:2968`), `CostTree`/
  `LoopTerm` (`:2896`), `LowerMeta.loop_terms` (`:3247`).
- The gcd2 body: `crates/theseus-tisc/src/synth.rs` (`transcribe_bennett_general`,
  `table_to_regtransfer`); exercised only by `tests/synth_gcd2*.rs`, NOT wired into
  `recognize_prim` for Trace use.
- Authority pattern for a loop program (template = `tests/mde_spill.rs`): T1 emission/
  ROM-shape → T2 `run_on_cpu_fast == eval` (fast, always-on) → T3 `#[ignore]` real
  `system_step == eval` over ≥2 back-edges + scratch-zero terminal (advisor-run) →
  T4 `#[ignore]` SystemRing closure → per-guard reject tests. `lower_trace` change =
  highest bar (L19/L20/L21: fresh-context Fable design+impl review + reproducing test).
