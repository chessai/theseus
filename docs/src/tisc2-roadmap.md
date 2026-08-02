# TISC-2 Roadmap — the CPU that runs arbitrary well-formed theseus programs

> **Status (2026-07-24):** Design complete + adversarially reviewed (3 passes) + keystone
> validated. **Phase A underway; re-sequenced after the A-1 POC.** This roadmap supersedes the
> earlier "T-loops → widen-PC → GCD" retrofit plan (symptom-chasing; see the pivot below).
> TISC-1's execution paradigm is **kept and proven**; TISC-2 is a *provisioning + compiler +
> datapath* redesign, not a new machine.
>
> **⇒ RE-SEQUENCE (2026-07-24, from the A-1 POC + Opus-5 operand-datapath investigation):** the
> A-1 POC proved a *scratch frame does NOT cure register pressure* — TISC ops can only name the 8
> **registers** as operands; scratch (s0..s7) is reachable only via `Xchg`, so it is spill
> storage, not working registers (measured GCD body ~200–300 instrs, not ≤40). The cure is to make
> the operand **crossbar** address scratch — which is the *same mechanism* as the B-1 shared-ALU
> crossbar. So **the operand-datapath is shared foundation for coverage AND efficiency**, and the
> "coverage-first / efficiency-second" split was wrong on this point. A new foundation slice
> **A-0 (shared, scratch-addressable operand crossbar + instruction-word reformat)** is inserted
> before the frame slice; with it, the GCD body returns to ~28 (B=194) exactly as first estimated.

> **⇒⇒ CURRENT STATUS (2026-07-28) — COVERAGE COMPLETE; Phase B B-1 shared ALU COMPLETE (step_j −39.4%); B-2 per-program widths LANDING (DF-2=Option A):**
> - **✅ B-2.1b MERGED (`4e5a4dc`): reduced `num_regs`** — few-register programs shrink the register file + crossbar; −16.8% step_j on a 4-register kernel (fib byte-identical). Advisor caught+fixed a scratch-operand misroute + the F2 s0/s1-immovability erosion (byte-identical position-based crossbar exclusion).
> - **✅ B-2.1c MERGED (`37c7a6c`): reduced `w_data` (mixed-width state)** — the big data-word win. `state_ty` is mixed-width (registers/s2../mem = w_data; s0/s1/PC/BR/IR = w_ctl=8) via `Layout::field_width` + per-field width vectors through `addressing::*`. **decode_execute 337459→103462 balanced cells (−69.3%) at w_data=4/num_regs=4 → ~−47% step_j for a narrow-data program**, stacking on B-1 (~−68% cumulative from the original baseline). Byte-identical at w_data=8 (fib/me_a3 unchanged); whole-domain Add/Xor/Addi/Subi/Xori at w_data=4 correct + run_on_cpu==eval; Fable no-BLOCKER + heavy authority 592/0. POC = register+imm; DEFERRED to B-2.1c-2: rotator/Ccnot/Cswap/scratch at reduced w_data, auto-derive w_data (from source Iso).
> - **✅ B-2.1c-2 MERGED (`0bd5ea9`): complete per-op w_data coverage** — whole-domain all ops (incl. rotator/Ccnot/Cswap/scratch) at w_data=4.
> - **✅ AUTO-DERIVE w_data MERGED (`96f40fb`, 2026-07-29): the w_data win applies automatically.** `derive_full_layout(iso, instrs)` reads `w_data = next_pow2(max_flat_width(iso))` off the source Iso's widest `Flat(n)` (exhaustive over all 22 Iso variants + Prim in/out types, theseus-core/typing.rs) — a Flat(4) program auto-runs the w_data=4 ring (−41.9% step_j, no manual `Layout`). **Fresh-context Fable review returned DO-NOT-MERGE with two silent-corruption BLOCKERs unreachable by the shipped straight-line kernel suite; advisor fixed both + MAJOR-1/2 in `derive_full_layout`, each with an L20 reproducing test:** B1 = control-in-registers truncation (a Trace come-from δu≥0x80 / forward-skip distance is staged into a general register via Addi+Swapbr → narrow w_data truncates the 8-bit offset → loop diverges; fix: `has_inner_control` ⇒ keep w_data=8, only straight-line programs narrow); B2 = num_regs under-count (marshal fills regs by INPUT LEAF, so an untouched Id/Times leaf occupies an unnamed reg → drop/assert-panic; fix: num_regs ≥ num_leaves(type_of(iso).0)); M1 = floor narrow w_data at 4 (verified only at {4,8}); M2 = cover `rom_program_energy_auto`. Full heavy authority GREEN (65 suites, 0 fail; byte-identity golden + both blocker regressions + all 6 max_flat_width pins). Review: docs/reviews/autoderive-wdata-review.md.
> - **✅ B-2.2a MERGED (`57d2500`, 2026-07-29): per-program PC width — enabling refactor.** Generalized the six sites that baked "PC = exactly 2 fields = 16 bits" (`FetchWires::new`, `fetch_fields`, `fetch_addr_bits`, `pc_add_br_with`/`pc_inc_with` field lists + wire-counts, `encode_with`/`decode_with` PC marshalling, `pc_byte_field`) to be driven by `layout.pc_fields()` (=`w_pc/w_ctl`; 2 at default w_pc=16, 1 at w_pc=8) and `layout.w_pc`. **Byte-identical at w_pc=16** (the `poc_step_net_bit_identical` golden green, no re-pin); a hand-built w_pc=8 layout now type-checks + lowers + round-trips. Nothing derives w_pc=8 yet (that's B-2.2c) — pure refactor. **Fresh-context Fable adversarial review = FIX-THEN-SHIP, one MAJOR:** `pc_fields()` is floor division with no Layout validation, so a non-byte-multiple `w_pc` (e.g. 12) would SILENTLY route high PC bits onto BR/operand wires (type_of passes — silent corruption) where the old code panicked loudly. Fixed with `assert_pc_carrier` (w_pc == pc_fields()·w_ctl && ≥1) at all three PC-builder entries + 3 L20 regression tests (w_pc=12 panics both builders; valid w_pc=8 does not). 66-suite heavy authority green. Reviews: docs/reviews/b22a-pc-fields-review.md; design docs/scratch/b22-pc-width-design.md. **NEXT: B-2.2b** (prove w_pc=8 RUN-correct: exhaustive ADR-0034 adder identity at k=8 + real `run_rom_on_cpu_with(&w_pc8)==w_pc16` — type_of can't see intra-word wire aliasing, only a real run can) **→ B-2.2c** (derive w_pc from `Lowered` B+S_max + measure the ~4% step_j drop).
> - **✅ B-2.2b MERGED (2026-07-31): w_pc=8 is RUN-correct (not just well-typed).** Tests-only. (1) `adr0034_split_add_exhaustive_wpc8` — `(pc_add_br_with;pc_inc_with)(&w_pc8)` == `(PC+BR+1) mod 256` over ALL 2^16 (PC,BR) pairs (sext identity at w_pc=8) + BR/s0/s1/regs/IR preserved + adjoint recovery. (2) real CPU runs `run_rom_on_cpu_with(&w_pc8,...)` == the SAME program at default w_pc=16, over forward-branch, **backward-branch (wraps 257≡1 mod 256)**, and high-PC-carry programs, terminal-quiescent. Proves the pcf=1 field addressing (both PC adders + fetch) is alias-free — the crux `type_of` can't see. **Fresh-context Fable review = FIX-THEN-SHIP, one MAJOR:** the proof waived backward-branch RUN coverage on a FALSE lemma ("terminating backward branch impossible without a loop counter" — false: a 2-register token-migration branch terminates in 2 passes); Fable's counterexample drives the one case with genuinely different intermediate arithmetic (modular wrap). Fixed by ADDING the backward-branch run (L20), not just correcting the comment. Reviews: docs/reviews/b22b-wpc8-proof-review.md. **PROCESS lesson:** the authority run hung 39h at 0% CPU because a review agent ran cargo in the SAME worktree/`target` — reviewers now get a separate target dir / no cargo; the authority run owns the box. **NEXT: B-2.2c** (derive w_pc from `Lowered` B+S_max + measure the ~4% win).
> - **✅ B-2.2c MERGED (2026-07-31): DERIVE w_pc per program + measure — the payoff.** `derive_w_pc(budget, s_max, rom_len)` = the MINIMUM safe PC width ∈ {8,16} satisfying BOTH runtime guards EXACTLY (`run_rom_on_cpu_with` address-space `budget+s_max ≤ 1<<w_pc` AND `fetch_with` injectivity `rom_len ≤ 1<<(w_pc-1)`), so it can NEVER mis-narrow a program into a Deferred/panic (a 9-bit need → 16). `derive_full_layout_from_lowered(iso, &lowered)` folds it into the layout; `run_on_cpu` + `rom_program_energy_auto` route through it — a small program now auto-runs on the `w_pc=8` ring (1 PC field, byte PC adder). **MEASURED WIN (minimal xor kernel, only w_pc differs): cells 2360→1976 (−16.3%), step_j 4.07e-10→2.88e-10 (−29.2%)** — inflated by the 1-instr ROM (fetch+PC dominate a tiny step net); ~few-% on a realistic multi-instr program as the design predicted. **Fresh-context Fable review (NO cargo) = FIX-THEN-SHIP:** core arithmetic SOUND (no guard-violating triple; reach==256 equality safe via half-open PC range `[0,B+S_max)` — max fetched addr ≤255, final never-fetched PC=256≡0 read by nothing). MAJOR-1 = the change broke an existing test (`win_flat4` priced auto at w_pc=8 vs a manual w_pc=16 net) → fixed (manual net uses `derive_full_layout_from_lowered`); MINOR-2 doc; MINOR-3 added an exact rom_len==128 fetch-edge run. Tests: exhaustive guard-sweep (reach 0..520 × rom_len edges) + narrow→w_pc8 run==eval + large→w_pc16 + boundary. Reviews: docs/reviews/b22c-derive-wpc-review.md. **The B-2.2 per-program-PC-width line is COMPLETE** (a: refactor → b: run-proof → c: derive+measure), stacking additively with the w_data + num_regs shrinks. PROCESS: the Opus worker FLAKED (deferred to a background monitor, left a stray cargo) — advisor salvaged the uncommitted code + fixed a test-only OOM (Flat(8) chains built 2^16-tables → SIGKILL; use Flat(4)).
>
> **⇒⇒⇒ VARIABLE-T FRONTIER (2026-07-31) — the coverage direction (governing goal: run arbitrary well-formed programs). Two probes mapped the terrain (`docs/scratch/mde-variable-t-scoping.md`): the reversible `Trace` loop machinery is READY; GCD's wall is the general-synth operand-word budget (degree-7 ANF → node_lines=35 → 44 words vs 10 free = widen-state/A-6.4, DEFERRED), NOT the loop machinery. A low-degree body fits NOW; the width wall is `iter_len ≤ 128` (W_BR, next).**
> - **✅ vart_halve MERGED (2026-07-31): first NON-SYNTHETIC `Trace` program on the real CPU.** halve-until-odd = the 2-adic valuation `v_2(n)`, encoded as a manifestly-bijective cyclic bit-rotation of `(tag‖h‖v)` (node_lines=1). Tests-only (`tests/vart_halve.rs`; K=3,HB=1: rom_len=111, B=646, iter_len=107, k_max=6). T1 ROM-shape + T2 `run_on_cpu_fast==eval` (full domain) + variable-T proof (`T(v0)=v_2(v0)+1`, T∈{1,2,3} — genuinely input-varying) + **T3 real `system_step` run_rom_on_cpu==eval on 3 sampled T-values + terminal quiescence (advisor-run, 5100s)** + a reject test pinning the `iter_len ≤ 128` boundary (K=4,HB=1 → 133 > 128, rejected). HONEST scope: the iteration COUNT computes v_2; the exit *payload* is the rotated window, not `v0>>v_2` (a readable-output program needs a counter leaf or a higher-degree body). Modest capability step over the mde-spill machinery-proof (both run data-dependent trip counts on the real CPU) — its value is the first named-arithmetic trip count + validating the fast-vs-real path for this class. **NEXT: W_BR widen / two-byte δu mint** (`compile.rs:4033`) — lift the `iter_len ≤ 128` cap for wider variable-T programs (and low-degree GCD-family bodies), then readable-output programs, then widen-state for high-degree (gcd2).
> - **✅ W_BR-a MERGED (`61f1c0a`, 2026-07-31): BR-addressing refactor (BR's B-2.2a), byte-identical at w_br=8.** Generalized ~15 sites that baked "BR = one 8-bit field" (decoder Swapbr/come-from arms, `encode_with`/`decode_with`, `pc_add_br_with`'s sext-of-BR operand, `instr::swapbr_with`, `br_hi_field`/`br_word_field`) to `layout.br_fields()`/`w_br`-driven, so a hand-built w_br=16 layout type-checks; `StateVals.br` widened u8→u16 (storage); `assert_br_carrier` guard. Fresh-context Fable review = **SHIP** (u8→u16 casts value-preserving on every path; byte-identity golden green, no re-pin; 70-suite authority green) + 3 MINOR fixes (M1 the `swapbr_with` 3-cycle comment = a W_BR-b trap, corrected; M2 assert on swapbr_with; M3 hard truncation asserts). Design `docs/scratch/wbr-widen-design.md`; review `docs/reviews/wbr-a-review.md`.
> - **🚧 W_BR-b IN PROGRESS — NOT MERGED (branch `wbr-b-comefrom`; RESUME POINT).** The two-byte come-from δu (RUN-correct w_br=16), lifting the `iter_len ≤ 128` cap so wide variable-T loops lower. **Design GO** (`docs/scratch/wbr-b-comefrom-design.md` REVISION 2 — arithmetic doubly-vetted on paper: δu = 65536−iter_len, cap ≤ 32768, two-transposition SWAPBR involution via `Swapbr{rd,rs}`, cross-paired +1 frame fits 8/8, iter_len=137). **Implemented + verified: the come-from ARITHMETIC is correct** (Fable impl-review = FIX-THEN-SHIP; exhaustive w_br=16 adder + involution + `vart_halve` K4 reject→lower + `run_on_cpu_fast==eval` + byte-identity golden all green) and **MAJOR-1/2/3 fixed** (wide+memory rejects + L20 reject tests). **BUT the full heavy authority caught a BLOCKER: a real wide-step-NETLIST bug** — `w_br16_step_type_checks` SeqMismatches: `step_with(&w_br16)` doesn't type-check; the wide `swapbr_instr` composition `fetch_rs ; with_target(R_RD, swapbr_core) ; sym(fetch_rs)` (decoder.rs ~1335) has a field-ARITY mismatch at w_br=16 (the s0-slot usage across fetch_operand/with_target/swapbr_core's high transposition). **The fast oracle/interpreter passed — only the NETLIST is malformed** (fast≠real, L15); the netlist gate caught it pre-merge, before the real `system_step` authority ran. Plus a STALE test: `mdd_reject_wcet_overflow` (compile.rs:1996, iter_len>128 now lowers). **✅ NETLIST BUG FIXED (2026-08-02, commits `3e1153e`+`0e5d46d`):** root cause was NOT the come-from — it was `fetch_operand`/`ccnot_fetch_rt_br` lifting their operand↔s0 swap over the hardcoded-DEFAULT field carrier (`at_fields`/`NUM_FIELDS`=23) instead of `at_fields_typed(&layout.cpu_field_widths())`, off-by-one field at w_br=16 (24 CPU fields); `swapbr_instr` is the first shared-step caller of `fetch_operand` at w_br=16, so only W_BR-b exposed it. Byte-identical at default. Diagnosed to ground truth (per-phase + per-instr `type_of`), fixed both sites + L20 regression `wbr16_wide_decoders_are_endos_on_the_netlist` + updated the stale test (iter_len=140 now lowers) + fixed a pre-existing dup import. Fresh-context Fable review = SHIP; **full heavy authority PASSED (707 tests, byte-identity golden green); real-cpu authority 3/4 green (T=1,2,3 ==eval)** then PAUSED on T=4 to pivot to the BVEQ rollout. **RESUME:** finish concretely (resume real-cpu T=4) → merge; OR replace the 4h real-cpu run via the BVEQ inductive-invariant method (proven on vart_halve; the wide come-from is this exact program class). Diagnosis in `docs/reviews/wbr-b-review.md`.
>
> **⇒⇒⇒⇒ BVEQ SYMBOLIC-VERIFICATION ROLLOUT (2026-08-02) — test-speedup: retire brute-force whole-domain SIMULATION sweeps → whole-domain SYMBOLIC BDD equivalence (~seconds, domain-size-independent, coverage ≥ exhaustive). Tenet: fast iteration, never at the cost of correctness.**
> - Engine: `bveq_netlist::eval_bdd` (symbolic Iso interpreter; gates synthesized from the SAME `PrimDef::forward` tables `theseus_eval` runs; canonical ROBDD equality = whole-domain equivalence; rejects Trace/Iter/Fold = fails closed). Three regimes measured: **combinational single-step GO** (PC-add 0.018s vs 387s brute; full step ~0.5s, flat across instr/width; symbolic-PC ≤47 nodes), **straight-line multi-step GO** (concrete-PC-per-step keeps <500 nodes over 16 steps), **data-dependent LOOPS NO-GO by naive composition** (symbolic PC⇒symbolic opcode⇒all-ops-at-once ⇒ >8GB OOM within one step).
> - **✅ PHASE 1 MERGED (`80caf2b`, 2026-08-02): ~3.1h of sims → ~3min, coverage-AUDITED.** New authorities (each = BVEQ whole-domain proof + concrete `theseus_eval` bootstrap [reality anchor] + negative control [teeth]): `bveq_step_poc`/`bveq_full_step_poc`/`bveq_multistep_poc`, `decoder_bveq` (**decoder ~53min→38s, now vs INDEPENDENT `instr_semantics` — STRONGER: catches mis-dispatch**), `b21c_bveq` (**straight-line ~2.27h→36s, mixed-width w_data=4**). Retired: PC-add family (b22b 387s + step.rs 2^16/2^24-hours), decoder whole-domain sims, b21c straight-line sims. **Fresh-context Fable review = FIX-THEN-SHIP: caught 3 MAJOR silent coverage-losses** (b21c frame-restoration regs-only; imm bit-lane axis regressed 256→1-2; decoder PC_hi field 22 unchecked) — all FIXED with teeth-verified negative controls (`99194ac`→merged as `80caf2b`); step.rs+b22b retirements verified genuine supersets, oracle-independence + eval_bdd-trust confirmed.
> - **PHASE 2 (NOT merged; branch `bveq-step-poc`@`99194ac` has `bveq_invariant_poc.rs`): INDUCTIVE-INVARIANT method for loops** — PROVEN on vart_halve (body-lemma [straight-line, concrete-PC, whole carrier ≡ rot_step, 3 BDD nodes] + come-from/termination lemma + induction over trip-count; no unrolling). Retires the loop sweeps (`mexch_reg`, valwidth reduces, `synth_gcd2`, `b22c`) + the ~4h real-CPU authority. Before merge: adversarial review + close 2 residuals — (a) memoize `step_with` fetch-mux (lemmas 11min→sec; wall-time is fetch-mux rebuild, not BDD size), (b) DERIVE the body-entry aux invariant (soundness; currently assumed). KEEP real-cpu `#[ignore]` anchors until (b). The #1/#2 rayon/hoist speedup (branch `parallel-sweep-tests`) is now largely OBSOLETED by BVEQ — retire, don't merge. See [[maestro-fast-tests-tenet]] (memory).
>
> **(historical) CURRENT STATUS (2026-07-27) — B-2 infra merged, B-2.1 big win awaited USER (DF-2 fork):**
> - **A-0 MERGED** to `chessai/theseus-v2` (`5f11e6b`): 32-bit word + shared scratch-addressable
>   operand crossbar; net energy WIN −11.9%.
> - **A-1 (hand-walk GCD) ABANDONED** — a correct reversible in-place gcd2 walk to ≤40 is
>   infeasible by hand (the reversible-clear ~doubles the body; ISA has no controlled arithmetic).
>   Finding: `docs/scratch/a1-gcd-emission-finding.md`. GCD is NOT the goal; it was a probe.
> - **A-6 GENERAL SYNTHESIZER = COVERAGE GAP (2) CLOSED.** `table → BDD (biodivine ROBDD) → gates`
>   lowers **any admitted-bijection prim** to correct reversible gates (proven: gcd2 256/256 +
>   symbolic, teeth). This retires the finite-arm-recognizer limitation — the deepest coverage gap.
>   Branch `a6-synth` (off A-0). BDD is the production path; ANF = differential oracle; **word-level
>   synthesis = documented NEGATIVE RESULT** (control-heavy prims explode: gcd2 778k instrs — filed,
>   not production). Efficiency of a given prim (BDD gcd2 = 5,408 gates, ~the general floor) is a
>   **Phase-B / provisioning** concern, NOT a coverage gap.
> - **STEP 1 DONE — general synthesizer INTEGRATED** (branch `a3-wcet`, `3e46244` + loose-Sum/panic
>   fix `1515b93`): `general_synthesize_prim` is the `recognize_prim` fall-through; any admitted
>   bijection prim lowers (or clean `NeedsProvisioning`); correct on loose-Sum/option types;
>   fail-closed (canonical whole-domain exec + differential). Fable-reviewed (`3a2a333`) → 2 MAJOR
>   found + FIXED. **Coverage gap (2) is now real in the compiler.**
> - **STEP 2 (COMPOSITIONAL CONTROL) — NESTED PROGRAMS COMPILE + RUN** (branch `a3-wcet`): A-3 tree-
>   WCET fold (`39a6911`, `B_fold ≥ true_B`, single-level byte-identical) → A-5 occ-aware nested
>   come-from frame stack (`fc9a664`, disjoint per-depth frames, δu net-zero, single-level byte-
>   identical) → A-4 delete the 6 nesting rejects (`3f849e9`). **branch-in-branch / Iter-in-Iter /
>   nested-skip / Swapbr-in-Trace-body / loop-under-product all `run_on_cpu_fast==eval`.** Depth-2
>   nested `Trace` rejects GRACEFULLY (register-bound, fail-closed) pending **A-5b** (frames-in-
>   scratch — using A-0's operand-addressable s2–s7). The non-compositional-control gap (1) is
>   substantially closed.
> - **✅ MERGED to `chessai/theseus-v2` (`1bd5908`, 2026-07-26) — 33 commits.** Trio Fable-reviewed
>   twice (one silent-corruption BLOCKER — `Times(Trace,g)` positional tag — caught + fixed occ-aware
>   + re-checked sound), verified on the HEAVY tier (67/0, incl. real-CPU nested authority
>   `run_on_cpu==eval`) — the fast oracle is NOT independent (shares `exec_instr` with the k_max walk),
>   so the real-`system_step` heavy tier was the gate. **Both deepest coverage gaps closed on the
>   main branch.** Deep loop-nesting rejects gracefully pending A-5b. Lessons: run heavy tier
>   `--test-threads=1`; test vectors must be non-degenerate.
> - **✅ A-5b MERGED (`b774b05`, 2026-07-26): deep loop-nesting via aux REUSE.** A nested `Trace`
>   reuses the outer level's aux frame (the come-from token is a compile-time δu immediate, so the
>   4 aux are dead across the inner-loop region) — zero standing tax. Fable-reviewed (liveness
>   premise holds across 6 probes) + real-`system_step` authority (`run_on_cpu==eval` on a depth-2
>   nest). **Honest scope:** the aux frame is O(1) in depth, but each nesting level needs a
>   `Plus`/`Seq` tag-wrapper that costs 2 non-reusable aux, so effective depth is bounded by the
>   register file (deep nests reject *gracefully*); Option A (aux-in-scratch via A-0's crossbar,
>   blocked only by a conservative `SwapbrRegOutOfRange` encoder guard) is the later generalization
>   for wide carriers / deeper nests. MINOR follow-ups: depth-limit reject diagnostic message; fold
>   per-copy occ-mark fragility note.
> - **✅ A-7.0 MERGED (`707dc78`): wide data words, bitwise class.** Wide words = multi-byte-leaf.
>   Wide **bitwise** ops (Xor/Ccnot/Cswap over 2+ byte-lanes) lower with **zero new compiler code**
>   via the **COMPOSITIONAL** form `Times(byteOp, byteOp)` (per-lane arm matchers fire → one byte-op
>   per leaf; shape-pinned, `run_on_cpu==eval`, real-CPU witness). **CORRECTION to the earlier design
>   §3:** a *monolithic* multi-leaf wide-op prim table does NOT recognize per-leaf — it hits the
>   general synthesizer's `2^total_bits` blowup / `NeedsProvisioning`. So: the general synthesizer
>   handles a single prim up to ~20 input bits; **wider values must be COMPOSED (Times/fold over
>   bytes)**, which is how real wide programs are built anyway. A-7.1 harness landed.
> - **✅ A-7.1 MERGED (`ccfdfe1`): wide ARITHMETIC.** `wide_add(k)`/`wide_sub(k)` generalize
>   `pc_add_br`'s TK ripple across k·W wires (zero-ancilla operand-iso; carry ripples free — no
>   wire-level byte boundary). Fable-reviewed SOUND (reviewer closed the k>2 gap: k=3 BDD 2⁴⁸ + k=4
>   BDD 2⁶⁴ pass, exhaustive u128 m=2..12; `tk_add_gates` refactor byte-identical by netlist
>   fingerprint). **⇒ FULL COVERAGE for "compile arbitrary well-formed programs" is now on v2:** any
>   prim lowers + control composes to arbitrary depth + wide bitwise & arithmetic data. 3 MINOR
>   follow-ups (byte-order pin test, netlist-vs-term comment, un-ignore fast k=4 BDD).
> - **✅ CAPSTONE (`0a7613e`) + ✅ WIDEN-PC MERGED (`35f596c`, 12 commits).** Capstone: small
>   end-to-end arbitrary programs (synth-prim + nested control + wide words) compile + `run_on_cpu==
>   eval` (9/0). Widen-PC: W_PC 8→16 + narrow-fetch + sext-δu (W_BR=8) → **B>256 bodies RUN** (payoff
>   crosses PC 256 on the real CPU); ~0.5%/bit tax; ADR-0034 re-proved. Fable review caught 3 bugs
>   (unwidened bveq_netlist state builders; forward-skip-dist 128..255 sext-backward; A_fetch=16
>   aliasing) — all FIXED + regression-tested. **me_a3 energy + several ring_run oracles #[ignore]d
>   — the wider 184-bit state OOMs their `lower_iso`/balance even alone (>91 GB, CONFIRMED
>   fundamental) → pending lean-`lower_iso`.**
> - **⇒ THE COVERAGE GOAL IS ESSENTIALLY COMPLETE ON `chessai/theseus-v2` (57 commits since A-0):**
>   any admitted prim lowers + control composes to arbitrary depth + wide bitwise & arithmetic data
>   + large (B>256) bodies run. Remaining = infra/polish/efficiency.
> - **✅ lean-`lower_iso` MERGED (`f63c806`, 4 commits):** the wide-state OOM was 100% redundant
>   `Type` deep-clone garbage → fixed with **`Arc<Type>` structural sharing** (bit-identical netlist,
>   proven; me_a3 86 GB→7.2 GB, 12×; a workspace-wide lowering speedup). Energy/ring tier un-deferred
>   + green at wide state; me_a3 golden re-pinned (step_j −6.6%). **⇒ COVERAGE GOAL COMPLETE +
>   memory-infra fixed + JOULES MEASURABLE AT SCALE (62 commits since A-0).** No deferred tests
>   remain from widen-PC.
> - **✅ Phase B B-1 Unit-1 (B1.1+B1.2 shared configurable reg+imm ALU) MERGED (`df876e3`, 2026-07-27):**
>   all 11 register+immediate ops fold into ONE bracket + ONE configurable ALU (Add/Sub/Addi/Subi→1
>   adder, the 4 rotates→1 rotator; the `group_b` block removed). **step_j −26.1% (2.5259595646e-9 →
>   1.8664606459e-9), `decode_execute` 874510→441015 balanced cells (nearly halved), B/T=24/20
>   invariant.** Empirical finding (design §6): the win is REMOVING whole op-blocks — dropping their
>   cells AND their share of the ~50% inter-block `seq_balanced` overhead — not incremental core-share
>   (B1.1 Group-A-only was 1.111× because A-0 pre-shared Group A's route). Full discipline: Fable review
>   (no BLOCKER, `docs/reviews/b1-unit1-shared-alu-review.md`) + real `system_step` authority + ring
>   closure + whole-domain per-op equiv + inert-crossbar tooth; goldens re-pinned; **heavy suite 561/0**
>   (also fixed 3 pre-existing widen-PC-stale compile tests → heavy tier genuinely green again).
> - **✅ Phase B B-1 Unit-2 (B1.3 universal 3-operand bracket) MERGED (`7b97227`, 2026-07-27):** the
>   universal bracket (rd→D, rs→A, **rt→B fetched once**) folds **Ccnot + Cswap** into the shared block
>   (5→3 top-level blocks). Config-scratch wall (s1's 8 bits) resolved by moving the 2 transient decode
>   bits to **s0** (also decoder-reserved + position-stable + clean at decode time), freeing s1 for the
>   Ccnot/Cswap one-hots — **zero coverage/tax cost** (compile-time discipline, not state growth).
>   **decode_execute 441015→337459 (2.59× vs baseline), step_j 1.8664606459e-9→1.5314006103e-9 (−18.0%;
>   cumulative B-1 −39.4% vs baseline), B/T=24/20 invariant.** Fable review no-BLOCKER (s0-restoration
>   proven structurally exact; `docs/reviews/b1-unit2-shared-alu-review.md`) + real authority (whole-domain
>   equiv 11 reg+imm + Ccnot + Cswap + s0-restore + run_on_cpu==eval) + ring closure + goldens re-pinned;
>   heavy suite green. Xchg/Swapbr stay separate (small/awkward).
> - **B1.4** (growth-tax-dead): demonstrated-by-construction (the 3-block structure + per-block
>   measurement prove no private crossbars remain except xchg/swapbr; s1 at 8/8 one-hots also caps new
>   sub-functions) — not shipped as a standalone heavyweight demo.
> - **✅ B-2 per-program widths — SCOPED + enabling infra MERGED (2026-07-27):** design doc
>   `docs/scratch/b2-per-program-widths-design.md` (`0cbb43b`). **B-2.0a** (`0e1a1df`): a `Layout` struct
>   is now the single source of truth for layout widths, `Layout::default()` byte-identical (fingerprint
>   gate + 568/0). **B-2.0b** (`b8a485e`): per-program minimal `Layout` derivation (`derive_layout`/
>   `is_cpu_only`) + **CPU-only MEM-drop routing** — a mem-quiescent CPU-only program runs on the
>   mem-free `step(rom)` ring instead of `system_step` (~1/3 cheaper oracle lowering). This is oracle-
>   CONSISTENCY (me_a3 already priced via `step(rom)`, mem-free) → **no priced-step_j change**. Fable
>   SHIP (no blocker; `docs/reviews/b2-0b-mem-drop-review.md`) + heavy authority 574/0.
> - **⇒ NEXT (needs USER input — decision-fork DF-2): B-2.1 = the REAL per-program step_j win.** It
>   requires threading `Layout` into the step/decoder **gate builders** (~168 const refs — 0a only did
>   the TYPE constructors) so a reduced-`num_regs` / narrower-`W_data` NETLIST is actually built. The
>   data-word-W part rests on **DF-2** (fixed 8-bit control region [s0/s1] vs aggressive per-program
>   config-minimization — see design §Q6). Est. ~30–40% additional step_j for a simple (4-bit / 3-reg /
>   no-mem) program, stacking on B-1's −39.4%. → then B-3 active-only microcode. Also: A-5b Option A
>   (aux-in-scratch) + MINOR cleanups.
> - **(prior) lean-`lower_iso`** (stream the `lower_iso`/balance netlist so wide-state programs don't
>   OOM → un-defers the me_a3 energy + ring-closure tests AND enables joules measurement at scale —
>   the prerequisite for Phase-B efficiency) → A-5b Option A (aux-in-scratch, wider carriers) →
>   MINOR cleanups → **Phase B efficiency** (shared ALU → `step_j`, gated on the lean-`lower_iso`
>   measurement).
> - **(prior) CAPSTONE** — a small end-to-end *arbitrary*
>   program (synthesized prim + nested control + wide words, B≤256 so no widen-PC) compiled +
>   `run_on_cpu==eval` — the holistic proof. THEN **provisioning** (widen-PC A-2/B-2) so LARGE bodies
>   run (handle me_a3 energy-repin OOM per decision-default: behavior-validate + defer energy repin,
>   or lean-`lower_iso` first). THEN A-5b Option A, MINORs, Phase B.
> - **(prior plan) A-7.1** (wide arithmetic) = a NEW parametric zero-ancilla ripple-adder
>   iso (cross-byte carry — canNOT be `Times` of independent byte-adds; highest-bar: adversarial
>   review + BDD `equiv` + real-CPU + adjoint round-trip) → **provisioning** (per-program widen-PC
>   A-2/B-2; lean-`lower_iso` for the me_a3 memory borderline; heavy tier `--test-threads=1`).
>   gcd2-scale bodies run only once provisioning lands, sized per-program so simple programs stay
>   cheap. Then Phase B (shared ALU) for `step_j`. MINOR follow-ups: A-5b depth-limit reject message
>   + fold occ-mark fragility; step-1 `mdd_trace_under_times` degenerate-comment.

## Why (the mandate)
Run **any well-formed theseus program** — the closed 22-combinator Iso algebra over finite
trees — correctness first, **efficiency (energy/step) critical**, and **no more whack-a-mole**
for any part of the CPU. TISC-1 hit a sequence of walls (GCD blocked → widen PC → …); the
root causes were **register pressure**, **non-compositional lowering**, and a **recognizer that
can't lower arbitrary prims** — not a paradigm limit. Design docs (committed under
`docs/scratch/`): `arbitrary-theseus-cpu-design-framing.md` (energy model + 8 axes + the
efficiency principle), `arbitrary-theseus-cpu-requirements.md` (the closed algebra + gap
ledger + WCET verdict), `tisc2-design-synthesis.md` (**the design**, incl. §7 review findings
and §8 the keystone), `tisc2-synthesis-notes.md` (the 4-proposal panel).

## The north-star principle (how every decision is judged)
**A fix that adds an instruction, a decoder arm, a state field, or PC/word bits taxes the
energy of _every_ step of _every_ program, forever. A fix that is a compile-time discipline or
a shared datapath adds _zero_ standing energy tax.** Prefer the latter always. (Measured: a new
op cost +9% step energy just by existing; each PC bit ~+10–15%.) This is the anti-whack-a-mole
rule, applied at architecture scale.

## The design in one paragraph
Keep the flat-ROM, time-multiplexed, fixed-WCET reversible step (the paradigm-stress-test
proved *why*: a shared step re-ramped B times is the reversible realization of **bounded
hardware / unbounded time** — a spatial unroll blows up on the loop-heavy programs that are the
whole point). Make its **lowering compositional** (a frame discipline + a tree-structured WCET
fold → nesting composes by construction) and its **datapath shared** (one configurable ALU →
per-step energy stops scaling with the instruction set). Lower **any** prim by transcribing its
`BvExpr` spec to gates (the keystone). Reuse the value model, the injectivity proof, the ring
closure, and the differential oracle.

---

## Phase A — COVERAGE: run every well-formed program *correctly*
Slices in **dependency order (re-sequenced 2026-07-24: A-0 promoted to a foundation)**. Every slice
is gated by the differential oracle (`run_on_cpu == eval`); control-flow / lowering / datapath
slices (the highest bar) additionally require a **fresh-context adversarial review + a real-worktree
behavioral test** (CLAUDE.md / L15 / L19–L21). Execution order:
**A-2 (width) → A-0 (operand crossbar) → A-1 (frame → GCD) → A-3 → A-4 → A-5 → A-6 → A-7.**

**A-0 · Shared, scratch-addressable operand crossbar + instruction-word reformat.** [NEW — the
coverage-enabling foundation; the crossbar half of B-1 pulled forward] The A-1 POC proved TISC ops
can only name the 8 **registers** as operands (`select_reg`, decoder.rs:252-274), and scratch is
Xchg-only spill — so a scratch "frame" is not working store. **Make the operand crossbar address
`reg ∪ scratch`** (generalize the existing `base: impl Fn(usize)->usize` seam) so ops can name
scratch words as `rd/rs/rt` — cheaper than adding registers (scratch is *already* plumbed in the
state; only the crossbar/encoding widens, **no new energized state**). This forces an
**instruction-word reformat**: the 16-bit R-format is full (asm.rs:161, 2 pad bits, 3-bit operand
fields), so it cannot address 16+ operand locations. **DECISION (user): widen to a 32-bit word**
(`R: fmt(1) op(6) rd(5) rs(5) rt(5)`, 10 spare; `I: fmt(1) op(6) rd(5) imm(20)`) — 5-bit operands
(32 locations: r0..r7, s0..s7, future), 20-bit imm (covers A-2), reformat ONCE. Full design +
rationale: `docs/scratch/a0-instruction-format.md`. *This is the same operand crossbar B-1 shares across ops — building it here delivers the
register-pressure cure (coverage) now; the gated ALU-core sharing (efficiency) stays in Phase B.*
*Gate:* fresh adversarial review (datapath + encoding = highest bar) + every existing program
`run_on_cpu == eval` at the new format + a test that an op names a scratch operand + energy re-pin.
*Risk:* HIGHEST BAR (decoder + instruction encoding). **The one design choice to get right: the
instruction-word bit allocation.**

**A-1 · Loop-body frame → the first variable-T loop (GCD).** [synthesis A2; **depends on A-0**]
Generalize the proven `with_registers` net-zero page-in/out bracket into a recursive
`with_frame(K)` giving each context K private **operand-addressable** working words (from scratch,
now nameable via A-0; `body_occ`-aware, zero at entry/exit). Fill the `gcd2_hist_step` emission
against the pinned `gcd2_hist_bytes` oracle. With K=3 operand-addressable words the 2-bit GCD body
is ~28 instrs ⇒ **B=194 < 256 ⇒ ships with NO widen-PC, no new op**. *(Note: the earlier "zero
energy tax via a compile-time scratch frame" claim was corrected by the A-1 POC — the enabling cost
is A-0's crossbar widening, paid once and shared with B-1; the frame itself remains compile-time.)*
*Gate:* POC (body ≤ 40, `run_on_cpu_fast == eval` on all inputs, B < 256) → adversarial review →
real-worktree `system_step == eval` across the GCD trajectory (≤5 back-edges) → full heavy-tier
suite → energy/ring golden re-pin → merge. *Value:* the original goal. *Risk:* HIGH (`lower_trace`).
*Existing worktree `a1-scratch-frame` (commit dbd4c2a) has the frame infra + verifier + the derived
walk algorithm (256/256 verified) — re-target it onto operand-addressable working words post-A-0.*

**A-2 · Coordinated address-width widen (B > 256).** [synthesis A1, corrected per review BLOCKER-3]
**Decouple two different quantities the first draft conflated:** the come-from token `δu` is a
*static ROM offset* (widen with `log₂ N_rom`); the WCET budget `B` is a *dynamic step count*
(widen `W_pc`). Widen PC/BR/imm and `pc_add_br`/`pc_inc` **together** (this is NOT a "pure
param" — the encoder, state, and step iso hardcode 8-bit) and **re-prove the ADR-0034
total-bijection injectivity at the new widths**. Fold in the **narrow-A fetch** (compare only the
low ⌈log₂ B⌉ PC bits) so a wider address space costs ∝ log(reachable PC), not the full width.
*Gate:* injectivity re-proof + every existing program `run_on_cpu == eval` + a synthetic B>256
program. *Risk:* HIGH (state layout + proof).

**A-3 · Tree-structured WCET/S_max fold + back-edge-honoring k_max walk.** [synthesis A3, corrected per BLOCKER-2]
Replace the flat sibling-sum WCET (`compile.rs:2103`) with a **recursive tree fold**
(`wcet(Trace)=spine + k_max·wcet(body)`, Seq/Times = Σ, Plus = stage + both arms). Make the
exact-k_max trajectory walk **honor inner back-edges** (a real mini-interpreter, not straight-line
`exec_instr`) and **merge the currently-dropped inner `body_meta` scaled by outer k_max** — else
nested `B` under-counts (a 4×100 nest counted as 88 vs real 4800) → park-wrap → silent corruption.
Behavior-preserving on flat programs (must reproduce current B exactly — the green oracle is the
guard). Keep WCET *and* `s_max` both Σ over Plus arms (do not tighten `s_max` to `max`). *Gate:*
all flat programs unchanged + a nested-loop B computed correctly. *Risk:* HIGH (soundness).

**A-4 · Occ-aware nested come-from frame stack.** [synthesis A5 — THE crux]
Replace the hand-assigned `aux_base = max_carrier_reg+1` with occ-aware allocation from
`with_scratch_frame`; each loop level's `{rc,rb,temp,ctrl,δu}` lives in its own frame (disjoint
by depth). **Prove the outer loop's pending come-from/δu survives the entire inner loop** (the
inner SWAPBR touches the shared BR every inner step). *Gate:* fresh adversarial review +
real-worktree nested-Trace `system_step == eval` across real back-edges, terminal-quiescent.
*Risk:* HIGHEST BAR (control-flow state machine). **Must precede A-5.**

**A-5 · Delete the nesting rejections.** [synthesis A4]
Remove the four `has_inner_control` guards + the F-1 `Times` reject. Nested Trace, loop-under-⊗
(`Times(Trace,g)`), and branch-in-branch now compose (A-3 gives correct WCET; A-4 gives disjoint
storage). Skip-blocks become depth-tagged frame levels in the same fold. *Gate:* Plus-in-Plus,
controlled-in-loop, `Times(Trace,g)`, nested Trace all `run_on_cpu == eval`, park-safe. *Risk:*
HIGH (correctness follows A-3 + A-4).

**A-6 · General-prim synthesis (recognizer → arbitrary).** [synthesis A6 — the keystone, POC-validated]
Build the `BvExpr → gates` transcriber. Table prims (≤12 rails): the existing enumeration
`synthesize_prim`. **Wide/symbolic prims: transcribe the `BvExpr` spec directly** (Bennett
compute-uncompute via the mandatory forward+backward spec; `Xor→`CNOT, `And→`Ccnot, `Mux→`Cswap;
polynomial because `BvExpr` omits multiply), **verified symbolically** by composing the existing
`symbolic_exec` + `equiv` (no enumeration, any width — validated:
`tests/bvexpr_synth_poc.rs`). The recognizer demotes to an optional peephole cache. Route
non-identity structural isos (AssocSum/Distrib/Factor) + body permutations through it. *Gate:* an
arbitrary admitted, unrecognized, wide-symbolic bijection lowers + symbolic-verifies +
`run_on_cpu == eval`. *Risk:* MEDIUM (mechanism validated). **Sequence any in-loop synthesized
prim after A-2** (its gate count can push B past the old 256).

**A-7 · Wide data words (Flat(n>8)).** [synthesis A7]
Parameterize the data word width `W` (decoupled from `W_pc`). *Gate:* a Flat(16) reduction
`run_on_cpu == eval`. *Risk:* MEDIUM.

**→ After A-7:** the reject surface has no reject-to-be-safe path for any well-formed program
whose prims are **structured** (BvExpr-expressible = everything real programs use). The only
residual is information-theoretically-random permutations, for which *no* method beats 2ⁿ gates.
**The machine runs arbitrary well-formed theseus programs.**

---

## Phase B — EFFICIENCY: per-step energy scales with the *active* instruction
Each slice **gated on a real `lower_iso → joules` measurement before commit** (user directive:
"fully measured"). Whack-a-mole is already gone after Phase A (a small fixed basis + synthesis
means no ops are ever added); Phase B is a further, optional efficiency optimization of an
already-fixed machine.

**B-1 · Shared configurable ALU cores.** *(The shared operand **crossbar** half of the original B-1
moved to A-0 as a coverage foundation — see the re-sequence. What remains here is the **core-sharing**
half.)* Replace `decode_execute = seq_balanced(Op::ALL.map(instruction))` (all ~15 op-netlists run
masked every step, `step_j = O(|ISA|)`) with gated **shared** cores fed by A-0's shared crossbar
(add±dir sharing one adder, xor, rotl±dir, and-family), reusing the verified gated cores. Take the
**all-blocks-gated form** (preserves the single fixed `step` iso the ring-closure proof depends on;
honest ~2–4× step_j + kills the ISA-growth tax). *Gate:* **measured** step_j drop via
`lower_iso → joules` **before commit** (user directive) + full adversarial review (datapath =
daemon-equivalent control surface) + whole-domain per-opcode oracle. *Risk:* HIGH.

**B-2 · Per-program widths.** Per-program data-word `W` and per-program-effective PC width
(pay-for-live-width; fib keeps its 5-bit-effective PC). *Gate:* measured reduction + oracle green.

**B-3 · (Deferred / optional) active-only microcode.** Only the current op's gates energized per
step (a larger win than B-1) — pursue **only** if reconciled with the ring-closure proof (it
breaks the single-fixed-step-iso). Not needed for the mandate.

---

## Cross-cutting: discipline & proof obligations
- **Highest-bar slices** (A-1, A-2, A-4, B-1 — control-flow/datapath/layout): fresh-context
  adversarial review by a different model + a reproducing real-worktree behavioral test, before
  merge. Reviews committed to `docs/reviews/`. Commit-only; never push.
- **Standing proof obligations** (all discharged by the existing differential oracle + bveq):
  (1) nested WCET-fold soundness (park-safety on nested programs — the deepest); (2) frame-stack
  disjointness across back-edges; (3) synthesized-prim faithfulness (symbolic bveq); (4) shared-ALU
  per-opcode bijectivity + mutually-exclusive decode. Each ships with a regression test that
  reproduces the specific counterexample (nested Iter/Trace under-count; Times(Trace,·) aliasing).
- **Guard the `Mul`-free `BvExpr` invariant** — it is what makes general prim synthesis polynomial
  and verifiable.

## Backlog / known limitations (surfaced by A-0)
- **Lean `lower_iso` for wide-state composed isos (BACKLOG, will matter more at A-2).** A-0's 32-bit IR
  (176-bit CpuState) inflated the *intermediate* memory of `lower_iso(step)` — the fib energy golden
  (`me_a3`) now peaks ~84 GB (fits on the 94 GB box, but at the edge), and the memory-heavy
  **whole-domain** tests (`array_map` whole-domain map, `t7_system_ring_closure`, `mexch_reg` heavy
  case) OOM on this box. This is NOT the energy computation (that already uses the analytic per-cell
  `analytic_buffer_count`, no `balance()` materialization) — it is the netlist *lowering*'s
  intermediate blowup over the wider state. It is a validation-infra cost of the 32-bit-max-headroom
  choice, not a behavior or correctness problem (behavior is validated via `t2_real_cpu_all_8` +
  `t3_real_cpu_oracle`; `me_a3` at 84 GB and `mexch_reg` at 74 GB both fit alone and are green). The
  earlier full-suite OOMs were largely **contention** from orphaned stray search processes (since
  cleaned up); the genuinely-too-big cases are the whole-domain tests already carrying `#[ignore]`
  (`array_map` whole-domain, `t7_system_ring_closure`). **Decision (option 2):** proceed — the heavy
  goldens fit at the RAM edge, the whole-domain OOM-ers stay `#[ignore]`'d + documented; a proper
  *streaming / low-intermediate-memory* `lower_iso` for wide-state isos is a deliberate backlog item
  (A-2 widens PC further, so it will bite harder — do it before/with A-2, not rushed).

## The pivot that produced this roadmap (for the record)
The prior plan (widen-PC to unblock the GCD) was **symptom-chasing**: the GCD was blocked by
*register pressure*, not PC width; widening the PC treats the symptom and pays a permanent energy
tax while the next program hits the next wall. A scope-audit + a 4-proposal design panel + 3
adversarial reviews replaced it with this holistic design. The GCD now falls out of **A-1** with
no widen-PC at all — as an early proof-point, not the goal.
