# M-E IMPLEMENTATION review (energy-price a compiled ROM)

> Verbatim fresh-context adversarial review (model: Fable), agentId `a439854641c29f625`, committed per maestro L21.
> Verdict SHIP-WITH-FIXES: model correct + number right; fixes = falsifying honest-crossing test (F-1), un-ignore it (F-2), correct the fib static-unroll narrative (F-3), comment nits (F-7).

---

All evidence is in hand. Here is the complete review.

---

# Adversarial implementation review — M-E (energy-price a compiled ROM), worktree `01KXVJB255TAZ1XG1C30FBTT24` @ fae02bd

Reviewer: fresh-context, read-only. Evidence: full read of `energy.rs`, `tests/energy.rs`, `examples/rom_energy.rs`, `money_plot.rs`, the `Iter`/`Trace` lowering in `compile.rs`, plus one live run of the artifact header (debug build, killed before lowering) to check the golden B against reality.

## (A) VERDICT: SHIP-WITH-FIXES

The corrected model is implemented faithfully on all five load-bearing points (B×step exact, right net, PC-dead-zone T, T<B for the chosen input, CMOS pays T). Nothing found makes the headline number wrong. The fixes are: the F3 acceptance test is not falsifying (it would pass on B-vs-B regressed code) and is `#[ignore]`d out of the gate; and the design's *mechanistic* story of where fib's park tax comes from does not match the code — the numbers are right, the narrative needs one correction before it goes in the thesis.

## (B) Numbered findings

**F-1 — MEDIUM: `me_a4` would pass if F3 were silently regressed (CMOS paying B instead of T).**
- `tests/energy.rs:419-424`: the shift assertion is `tc >= per_step_crossing * 0.999`. `cmos_crossing_t_ramp` is exactly linear in the ratio (`money_plot.rs:100-107`): program crossing = per-step crossing × B/T. If someone later changes `cmos_program_j` at tests/energy.rs:381 (or rom_energy.rs:103) to pay B steps, the B cancels, `tc == per_step_crossing`, and the `>= 0.999×` assertion **still passes** — as do the in-range check and everything else in the test. Per L20 ("the test must reproduce the finding"), the F3 acceptance must fail on B-vs-B code.
- Fix (one line): assert `tc >= per_step_crossing * (b as f64 / t as f64) * 0.999` (the linearity makes this exact), or equivalently `tc > per_step_crossing * 1.1` given B/T = 24/20 = 1.2. Optionally pin the crossing value per design A4 ("assert the crossing t_ramp").

**F-2 — MEDIUM: `me_a4` — the only test of the honest crossing — is `#[ignore]`d (tests/energy.rs:358-360), so the gate never exercises F3.**
- The design says keep "ramp sweeps" out of the gate, but the expensive part (the fib step-net lowering) is already paid and memoized by me_a1/a2/a3/a6 via `fib_step_net()` (tests/energy.rs:185-199); the 7-point sweep itself is just char-DB queries — seconds. Marginal cost of un-ignoring inside the suite is ~zero. Either un-ignore it, or the F3 guarantee rests entirely on someone remembering `--ignored`.

**F-3 — MEDIUM (narrative faithfulness, not a code bug): fib does not take the Trace-loop path the design/review reasoned from; B=24 is the straight-line ROM length and the park tax comes from *forward-skip savings*, not loop early-exit.**
- Design of record (me-design.md:42-44, review §C) predicts `B = |instrs| + (k_max−1)·iter_len ≈ 70–75` via `loop_terms`. In fact `iter Nat fibStep` lowers via `lower_data_dependent_iter` (compile.rs:1577-1591 → 1684-1736): a **guarded static unroll** with SWAPBR skip-blocks; it pushes no `loop_terms`, so `wcet() = instrs.len() = 24` (compile.rs:1417-1424). Verified live: the artifact prints `fib → 24-instr ROM, WCET B=24`. T=20 < B arises because fib(1) *skips* 2 of the 3 guarded iterations (forward jump over the body), reaching the dead zone 4 steps early — then idles 4 steps to the WCET.
- The headline model is unaffected (B is still the true WCET — the no-skip path executes all 24 words; skips only shorten; B×step is still exactly what the ring dissipates), and 16.7% park tax sits inside the design's claimed 15–65% band. But the thesis/example text must not describe Trace-loop mechanics (`k_max`, `iter_len`, back-jump) for fib, and the golden B=24 should be understood as "unrolled ROM length", not "24 + loop terms". Recommend one sentence in `rom_energy.rs` and/or the milestone doc stating the actual mechanism.

**F-4 — LOW: `me_a6` is near-tautological.** tests/energy.rs:461-478 compares `pe.step_j` against `balanced_energy(net, db, T_RAMP)` — but `rom_program_energy` computes `step_j` by calling `balanced_energy` on the *same* `net` reference (energy.rs:422-423). It's f(x) == f(x). It does catch an accidental scale factor on `step_j`, but it never touches `cpu_money_plot` (money_plot.rs:128), which is what design A6 named. Cheap strengthening: call `cpu_money_plot(net, db, &[T_RAMP])` and assert its `full_step[0].energy_j == pe.step_j`.

**F-5 — LOW: `me_a2`'s B and T cross-checks are circular.** tests/energy.rs:257-271 recomputes `compile_with_metadata(iso).wcet()` and `rom_trace(...)` — the exact same calls `rom_program_energy` makes internally (energy.rs:408-419), with the same arguments. They verify plumbing, not values. Acceptable only because `me_a3` pins B=24/T=20 as independent constants (tests/energy.rs:324-328) — the goldens are carrying A2's teeth; keep them exact (`assert_eq!`, as written).

**F-6 — LOW: `rom_trace` declares park at the *first* dead-zone entry and stops simulating (energy.rs:344-355); it never checks the "and stays" half (br==0 at entry).** The soundness argument (P2 guard, post-park PC strictly increments) is enforced on the guarded `run_on_cpu` path, not on this unguarded `run_rom_fast` mirror. A hand-built ROM whose SWAPBR lands PC ≥ rom_len with br≠0 pending would bounce back into the ROM and `rom_trace` would report a false early T. Fine for compiler-emitted ROMs (skip targets are in-ROM); a `debug_assert!(state.br == 0)` at the detection point is a one-line hardening.

**F-7 — COSMETIC (comments):** (a) energy.rs:346-347 "the first T-1 steps were productive" — it's T steps (indices 0..T−1); (b) tests/energy.rs:277 says "fib(3) must halt before WCET" but the input is fib(1) (tests/energy.rs:204-214); (c) rom_energy.rs:5 header says "runs fib(3)" but main runs fib(1) (rom_energy.rs:53-59). Fix before the thesis quotes them.

## (C) Is `total_j = B × step_j` correct, with the right net? — YES

- `rom_program_energy` (energy.rs:400-443): `budget = lowered.wcet()` (:409), `total_j = steps_b × step_j` (:427) — one multiply, no per-word path anywhere; `park_tax = (B−T)/B` (:429-433), **not** (B−T)/T.
- The step net is priced ONCE per step via `balanced_energy` (:422); the K0 balance buffers are counted once inside that one step (energy.rs:109-139, `total = active + buffer` asserted at tests/energy.rs:118-119) — no cross-step double-count.
- The right net: `fib_step_net()` lowers `step(&rom)` from fib's own assembled 24-word ROM (tests/energy.rs:185-199); the example does the same (rom_energy.rs:40-49). The two OnceLocks (`step_net` for the 2-word ROM, `fib_step_net` for fib) are separate statics; all six me_ tests use the fib one. Golden step_j = 2.7596e-9 J vs the 2-word ROM's known 1.0474e-9 J — a wrong-net regression fails `me_a3` loudly. Internal consistency of the goldens checks out: 24 × 2.7596147106e-9 = 6.62307530554e-8 ≈ GOLDEN_TOTAL_J.
- Residual trap: the public API takes `step_net` as a parameter (energy.rs:405) and trusts the caller — a future caller *can* silently price fib with the money-plot net. Documented (energy.rs:397-399) and golden-protected here; unverifiable cheaply at runtime. Acceptable; note it.

## (D) Is the crossing honest B-vs-T (F3)? — YES in the code; the test doesn't falsify it (F-1)

- Test: `cmos_program_j = t as f64 * cmos_step_j` with `t = pe.steps_t` (tests/energy.rs:367-381); reversible curve = `b × step_bd.total_j` (:391). Example: identical (rom_energy.rs:103, :108). CMOS gate count = fib net's active cells excluding `cswap-k0-balance` (tests/energy.rs:371-377; rom_energy.rs:96-102) — same convention as the committed money plot (money_plot.rs:137-152), conservative direction.
- The crossing genuinely shifts by B/T = 1.2 in the artifact. But per F-1, the assertion guarding this is `>=`-weak and `#[ignore]`d — fix both.

## (E) Per-test genuineness

- **me_a1** (tests/energy.rs:227-247): identity B×step_j with rel<1e-15 — verifies the struct fields cohere; near-tautological by design (A1 *is* an identity check). Genuine for its stated purpose.
- **me_a2** (:251-312): the productive_j / park_tax / idle-energy arithmetic identities are real; `T < B` is a real, load-bearing assertion (the park-tax story is non-empty: T=20 < B=24). B/T equality checks are circular (F-5).
- **me_a3** (:316-354): genuine and the strongest test — pinned constants (6.6230753055e-8 J, 2.7596147106e-9 J, B=24, T=20), exact integer asserts, rel tol 1e-6. Would catch codegen, char-DB, lowering, and wrong-net regressions. (Assumes goldens match a real run — you are verifying that live.)
- **me_a4** (:358-433): implements the honest comparison correctly but is non-falsifying for F3 (F-1) and ignored (F-2).
- **me_a5** (:437-457): genuine negative — 1e-9 and 1e-4 both flow to `CharDb::query`'s hard range error through `balanced_energy` (energy.rs:114-116); no extrapolation path exists.
- **me_a6** (:461-479): near-vacuous (F-4).

## (F) What's SOLID

- The corrected physics model is implemented exactly as reviewed: no per-word energy anywhere; data-independence stated on the struct (energy.rs:363-371); the F5 step-vs-system_step boundary is declared in both the struct doc (energy.rs:369-370) and the artifact output (rom_energy.rs:10-11, :147).
- T is defined by PC ∈ [rom_len, 256), never `decode()==None` (energy.rs:334-349); `pc: u8` (types.rs:242) makes the 256 bound structural. The step-boundary bookkeeping is correct (T counts productive steps including the one that parks PC); `run_rom_fast` is stateless per step (compile.rs:2581-2591) so the 1-step loop is equivalent to a budget run.
- The chosen input genuinely exercises the park tax: T=20 < B=24, park 16.7% — inside the design's claimed band.
- F7 done: `cpu_money_plot.rs:24-25` comments now say ROTLI / reserved-opc-0b101, and the test fixture `test_rom()` (tests/energy.rs:44-52) carries the corrected comments too.
- Scope clean: non-docs diff touches exactly `energy.rs`, `tests/energy.rs`, `examples/rom_energy.rs`, `examples/cpu_money_plot.rs` — all theseus-tisc; no compiler/ISA/lowering behavior change; `clause_report` untouched per design.
- Memoization discipline: one lowering per net via OnceLock + a `LOWER_GATE` mutex serializing the two ~GB lowerings (tests/energy.rs:34-36); the expensive fib lowering is shared across all six me_ tests.

Bottom line: the number is right and the model is the reviewed one. Before merge: strengthen me_a4 to assert the B/T shift (one line) and un-ignore it; correct the fib-mechanism narrative (static unroll + skip savings, not Trace `loop_terms`) anywhere the thesis text will quote it; fix the three fib(3)/T−1 comments. Your live run of the me_ goldens is the last piece — if me_a3 passes as committed, ship with those fixes.
