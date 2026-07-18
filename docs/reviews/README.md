# Adversarial review records + finding→test index

Per maestro **L21**: every adversarial review (DESIGN phase, before delegation; and
IMPLEMENTATION phase, before merge) is committed verbatim here, and **every finding is
tracked to a reproducing test** (per L20) or an explicit *not-tested-because* with
justification. The Fable review transcripts otherwise live only in an ephemeral `/tmp`
session dir — these files are the durable record.

Reviews are run by the advisor via the Agent tool (model: Fable, fresh context, a
different model from the implementer). "Verdict" is the reviewer's, not the advisor's.

## Reviews on record

| Review | Phase | Verdict | File |
|---|---|---|---|
| M-D.w (WCET + reversible park-at-end) | design | SHIP-WITH-FIXES | [mdw-design-review.md](mdw-design-review.md) |
| M-D.w | implementation | *incomplete (Fable ended early)* | [mdw-impl-review.md](mdw-impl-review.md) |
| M-D.d (Trace reversible while-loop) | design | DO-NOT-SHIP-as-drawn → revised | [mdd-design-review.md](mdd-design-review.md) |
| M-D.d | implementation | DO-NOT-SHIP → fixed | [mdd-impl-review.md](mdd-impl-review.md) |
| M-D.c1 (MEM_WORDS 4→8 widening) | design | *incomplete (ended mid-measurement)* | [mdc-design-review.md](mdc-design-review.md) |
| M-E (energy-price a compiled ROM) | design | SHIP-WITH-FIXES | [me-design-review.md](me-design-review.md) |
| M-E | implementation | SHIP-WITH-FIXES | [me-impl-review.md](me-impl-review.md) |
| TP-1 (reversible GCD formulation) | design | OVERTURNED-IN-PART | [tp1-gcd-review.md](tp1-gcd-review.md) |

> **Recovered retroactively.** These reviews predate L21; they were folded into the
> design docs + tests at the time but not committed as records. Captured here from the
> session transcripts. Going forward, commit the review at the time it is produced.

## Finding → test index

Test names are in `crates/theseus-tisc/tests/compile.rs` unless noted.

### M-D.w DESIGN review
| # | Finding | Test |
|---|---|---|
| BLOCKER-1 | parked PC = `B+S` (not `B`) → wrap; need `B+S_max≤256` | guard: `mdw_t5_p2_park_safety_overflow` (135+130 ROM rejected); **exact arithmetic: `mdw_parked_pc_is_b_plus_s_not_b`** (two inputs skip different arms → park at different PCs); `mdw_t2b` tightness |
| MAJOR-3 | real-iso idle adds `s0` → idle==identity only if `s0=0` | guard: `mdw_t5_p3_terminal_s0_nonzero`; identity: real-CPU `mdw_t6_real_cpu_park_liveness`; **exact divergence: real-CPU `mdw_real_idle_corrupts_r0_when_s0_nonzero`** (real idle adds s0, fast freezes — proves P3's teeth) |
| MAJOR-4 | park is already live (reframe, not a bug) | n/a — observation; covered by the 17 existing skip-taking `*_real_cpu` tests |
| P1 | `BR=0` at terminal | `mdw_t5_p1_terminal_br_nonzero` |
| MINOR-5 | `B≥rom.len()` invariant | `mdw_t3_wcet_ledger` asserts it |
| MINOR-6 | scale eval fuel with `B` | *not-tested* — behavioral, no failure mode with a repro |

### M-D.d DESIGN review (F1–F10)
| # | Finding | Test |
|---|---|---|
| F1/F2 | come-from witness accumulates / `ctrl` leaks (fixed via the §D skeleton) | `mdd_3cycle_trace_real_cpu` + `mdd_informative_trace_real_cpu` (t=3, two backward re-entries — fails if the come-from doesn't chain) |
| F3 | totality bound off by one: `k_max=|u|+1`, not `|u|` (ADR-0007 corrected) | `mdd_wcet_golden` (pins k_max=3→46); `mdd_informative_trace` (t=3=\|u\|+1 output would be wrong at k_max=\|u\|) |
| F4 | `S_max`/wcet must be compositional, not a ROM scan (scan mispairs the loop bracket) | **`mdd_loop_bracket_not_counted_as_forward_skip`** — compositional `s_max()==0` while the ROM scan (`max_skip_distance`) miscounts the loop bracket; also `mdd_wcet_golden` + `mdd_park_under_loop` |
| F5 | loop-carrier layout contract / leaf-count | `mdd_reject_leaf_count_mismatch` (`\|a\|≠\|b\|` rejected) |
| F6 | inner-control policy (Swapbr-free bodies for v1) | `mdd_reject_swapbr_body` (Trace body with a skip-block rejected) |
| F7 | pre-existing `AssocSum` nested-sum no-op miscompile | `f7_assoc_rsum_nested_rejected`, `f7_assoc_lsum_nested_rejected`, `f7_unite_sum_one_rejected`, `f7_uniti_sum_one_rejected`, `f7_distrib_nested_rejected`, `f7_factor_nested_rejected` + identity-preserved: `f7_assoc_identity_still_compiles`, `f7_distrib_trivial_identity`, `f7_factor_trivial_identity` |
| F8 | two-direction WCET *equality* is fragile once loops exist | *deliberately downgraded* per the review to per-direction correctness; `mdd_adjoint_trace` covers the adjoint |
| F9 | 8-bit PC limits Trace to tiny carriers | `mdd_reject_wcet_overflow` (`wcet+S_max>256` rejected) |
| F10 | budget threading needed; no pairing checker | budget threading: `mdd_wcet_golden`/`mdd_3cycle`; pairing checker: not built (noted) |

### M-D.d IMPLEMENTATION review (F-1…F-8)
| # | Finding | Test |
|---|---|---|
| F-1 | **BLOCKER** occ-blind register grab → `Times(Trace,g)` clobbers `g` | `mdd_reject_trace_under_times` (the review's repro) |
| F-2/F7 | AssocSum miscompile reachable via Trace bodies | the `f7_*` tests (above) |
| F-3 | vacuous guard tests (no assertion) | fixed: `mdd_reject_nested_trace` (real assert), `mdd_reject_trace_under_times` |
| F-4 | flagship oracle informationally empty (`One↔One`) | `mdd_informative_trace_fast` / `_real_cpu` (`Bit↔Bit`, genuine output-value check) |
| F-5 | missing leaf-count guard | `mdd_reject_leaf_count_mismatch` |
| F-7 | Swapbr-body reject guard untested | `mdd_reject_swapbr_body` |
| F-6, F-8 | comment/docstring nits | *not-tested* — non-behavioral |

### Not-tested-by-design exceptions (justified)
- **Layout-invariant guard** (`homes_in≠homes_out`, compile.rs) — a `debug_assert` in
  `lower()` fires first in debug builds, so this `lower_trace` guard is only reachable in
  release. A normal `assert!(is_err)` test can't hit it in a debug test build. Documented
  defensive backstop; would need a `#[cfg(not(debug_assertions))]` test to cover.
- **F8 / F10-pairing / comment nits** — deliberate downgrade (F8: per-direction, not
  equality), un-built (F10 pairing checker), or non-behavioral (see rows above).
- **M-D.w finding 2** (T2 sweep bound) and **finding 4** (park-already-live) — a
  test-design fix (embodied in `mdw_t2a`/`mdw_t2b`) and a reframe (covered by the 17
  existing skip-taking `*_real_cpu` tests), not standalone bugs.
- **M-D.w findings 6/7** (fuel scaling, citation nits) — non-behavioral.

### M-E DESIGN review (energy-price a compiled ROM)
Findings folded into `docs/scratch/me-design.md`; tests land with the implementation
(acceptance A1–A6). Index the test per finding when M-E is implemented.
| # | Finding | Test (acceptance) |
|---|---|---|
| F1 | per-word step energy is inverted — model prices static inventory, so `B×step_energy` is EXACT | A1 (identity check `total_j == B × step_j`) |
| F2 | must lower the step net from fib's OWN ROM (not the 2-word money-plot ROM) | A3 golden from fib's net; A6 method-consistency |
| F3 | honest comparison: reversible pays B, CMOS pays T (else the crossing is cosmetic) | A4 (crossing `B·E_step` vs `T·E_cmos`, in-range, ≥ per-step crossing) |
| F4 | T defined by PC-in-dead-zone, not `decode()==None` (idle decodes to `Some(Addi)`) | A2 (`steps_t` = halt step; park-tax arithmetic) |
| F5 | state `step(rom)` vs composite `system_step` (memory body) | design §3.2 / artifact text; noted |
| F6/F7 | attribution `pc_add_br` row + residual; money-plot ROM opcode comments wrong | §5 obligations (comment fix; optional attribution) |
| F8 | golden stable; memoize the ~2min fib step net; sweeps out of the gate | A3 golden; test fixture memoization |

*Verdict on Q-A/B/(C)(D)(E): headline B is physical; per-word inverted; idle ≈ 100% of a
step; M-E's real content is the WCET park tax + the honest program crossing.*

### M-E IMPLEMENTATION review (tests in `crates/theseus-tisc/tests/energy.rs`)
| # | Finding | Test / resolution |
|---|---|---|
| F-1 | honest-crossing test non-falsifying (passed on B-vs-B) | `me_a4_honest_crossing` now asserts crossing `== per-step × B/T` exactly (fails on a B-vs-B regression) |
| F-2 | `me_a4` `#[ignore]`d → gate never exercised the honest crossing | un-ignored (lowering already memoized) |
| F-3 | fib is guarded static-unroll (park tax = forward-skip idle), not a Trace loop — narrative | `me-design.md §5b`; `rom_energy.rs` comment; number unaffected (B=24 is the true WCET) |
| F-4/F-5 | `me_a6`/`me_a2` near-tautological/circular | acceptable — `me_a3` pins B=24/T=20/6.62e-8 J as independent goldens (carries the teeth) |
| F-6 | `rom_trace` doesn't check the "and stays" (br=0) half | fine for compiler ROMs; noted (debug_assert hardening deferred) |
| F-7 | `fib(3)`/`T−1` comment nits | fixed |
| solid | `total_j=B×step_j` right net, T=PC-in-dead-zone, memoization, F7 opcode-comment fix, scope clean | — |

### TP-1 GCD-formulation review (design-stage — no code, so findings → docs, not tests)
A test-*program* formulation review (GCD isn't implemented; it's a Tier-3 customer). Its
findings landed as **theory + roadmap**, not tests: the trajectory-disjointness sizing law
→ **ADR-0007 TH-1b**; the GCD/Collatz/TP-1a re-tiering + the milestone cluster (M-D.e +
widen-PC + `k_max`-tighten) → `docs/src/test-program-roadmap.md`; the corrected §4 fits-law
and §2 typing → `docs/scratch/tp1-gcd-design.md`. When these programs are eventually
implemented (after the cluster lands), the acceptance oracles become the tests. The
verified-solid claims (2-to-1 GCD collision, no history-free GCD, come-from can't record
branches) are recorded in the review.

## Earlier reviews — NOT recoverable (the exact gap L21 fixes)

The pre-M-D milestones (Stage 2 two-body, the type-system pass, M-D.a fold/unfold,
M-D.alloc1/alloc2, M-D.b MEXCHR) each had a design and/or implementation adversarial
review, but they ran in **prior sessions** whose Agent-tool transcripts lived in
now-wiped `/tmp` dirs — and those sessions' conversation transcripts are gone too. The
review *texts* are unrecoverable. Their **findings**, however, were fixed-with-a-test at
the time (documented in `~/.claude/.../memory/theseus-overnight-run.md`): e.g. Stage 2's
mod-P timing flaw → the absolute-tick arrival-equation cert; 2b-lite's "self-fulfilling
theater" → golden-pin + independent re-derivation; M-D.a's unsound `allocate_aux` →
reject-guards + alloc1's *reproduced* corruption test; M-D.b's invisible bus-quiescence
trap → `system_ring_mexchr_matches_eval_oracle`. Those tests are in the suite; the
verbatim reviews are lost. This is exactly why L21 mandates committing each review *at
the time it is produced*.

## Incomplete reviews (Fable ended before the final verdict)
- **M-D.w impl** and **M-D.c1 design**: the reviewer ran long and returned a status line
  instead of the structured verdict (the known Fable reporting flake — L21 says instruct
  "your final message MUST be the complete review"). The M-D.w impl was covered by the
  advisor's own review + the maestro verifier + real-CPU oracle; M-D.c1's feasibility was
  answered by a direct N=8 measurement probe (see `docs/scratch/mdc-design.md`).
