# T-loops slice-1 design review (Fable, fresh-context, with POC) — docs/scratch/tloops-slice1-design.md r1

*(Advisor's faithful capture, L21. The 7/7-green POC is preserved verbatim at
`docs/scratch/tloop_poc.rs.ref` — the E1–E6 experiments referenced below.)*

## VERDICT: NEEDS-MINIMAL-CHANGE-EXACT-KMAX+BODY-RECOGNIZER (small GCD does NOT compile today; widen-PC is NOT the first wall and must NOT be slice-1)

The draft's central reframing — "4-bit subtractive GCD fits under the existing 256 cap with zero changes (k_max ≈ 31, WCET ≈ 252)" — is **false on today's code, overdetermined by four independent walls**, each reproduced by a runnable experiment in the POC (7/7 green at `24a1c40`). But the prior review's pessimism is also out of date in one crucial way: the **bijective GCD program exists at the core level today** — `admit() == Ok`, `eval(Trace) == gcd` over the whole domain, genuinely variable T — and the exact-k_max arithmetic shows a 2-bit (and marginally 3-bit) GCD **fits under the existing 256 cap / 8-bit PC** once `k_max` is trajectory-derived instead of `|u|+1`. The enabling change is small but lowering-touching (HIGHEST BAR).

## 1. Does a small subtractive GCD compile today? NO — four walls, in firing order

All numbers from the POC run, not estimates.

**Wall 0 — no surface syntax.** The `.thse` lexer keywords are `type, iso, prim, where, id, iter, fold, unfold` (+ reserved `for`) — there is **no `trace`/`while` surface form** (`theseus-syntax/src/lexer.rs:22-44`). A GCD `.thse` conformance program à la 0026-lfsr is inexpressible; the program must be a Rust-built `Iso::trace` (the mde_spill pattern).

**Wall 1 — Bennett bijectivity (kills the design's carrier).** The draft's §0 "GCD's carrier is (a,b) + tag ⇒ ~2–3 payload leaves" is invalid: the historyless step is 2-to-1. POC E1: over 4-bit pairs, **105 of 256 outputs have >1 preimage** ((5,3)→(2,3) and (2,5)→(2,3)); as a `PrimDef` it fails `admit()` with `BackwardNotTotal`. The honest carrier is `(a, b, hist)` with a **self-delimiting history** (sentinel bit + one branch bit per subtraction — a plain shift register is ambiguous: `(3,3)` [T=0] and `(3,6)` [seq "0"] both give hist=0; the sentinel fixes it). For w-bit inputs, hist needs T_max(w) bits: w=2→3, w=3→7, w=4→15.

**Wall 2 — k_max = |u|+1, the binding wall (and it is NOT the PC).** There is **no measure mechanism**. `compile.rs:2535-2541`: `k_max = enumerate_values(u).len() + 1` — the full carrier-type cardinality. The design's "k_max = |measure|+1 ≈ 31 from a+b ≤ 30" mechanism does not exist. Measured rejections (Id-body probes, the *best possible* body at iter_len = LOOP_OVERHEAD = 10):
- 4-bit historyless (the design's exact case, already impossible per Wall 1): `Trace WCET 2574 exceeds 256 (k_max=257, iter_len=10, spine=4)` — off the design's "252" by 10x.
- 2-bit GCD with honest 3-bit history (|u| = 4·4·8 = 128): `WCET 1294 (k_max=129)`.
- 3-bit GCD with 7-bit history (|u| = 2^13): `WCET 81934 (k_max=8193)`.

Corollary (E4/E5): today's Trace admits **|u| ≤ 21** for any non-empty body (k_max·(10+L) ≤ 252), and by TH-1b trajectory disjointness the **total surplus iterations across the entire input domain is ≤ |u| ≤ 21**. E5 confirms the ceiling is exactly saturated by the shipped M-D.e walk: T(0)=6, T(1)=1, Σ(T−1) = 5 = |u|.

**Wall 3 — no lowering path for the body.** Prim lowering is recognizer-only (`recognize_prim`, compile.rs:1006-1352). The real GCD body fails **before** the WCET check with `unrecognized prim: 'gcd3_hist_step' not recognized as a TISC-1 pattern`. A pinned k-leaf recognizer (mde_spill_walk pattern) is required.

**Leaf caps are NOT a wall at w≤3** (post M-D.e): carrier `(a,b,hist)` + tag = 3 payload leaves + tag, `n_input+1 = 4 ≤ 4` — both guards pass. At w=4, hist(15 bits) needs 2 leaves ⇒ `n_input+1 = 5 > 4` ⇒ trips the M-D.e F5.2 live-input guard too.

## 2. POC results (what DOES run today)

**E2a — the bijective GCD exists at core level, today.** Built `gcd3_hist_step` as a whole-domain table `PrimDef` on `Sum(u, P)`, `u = P = (Flat(3), Flat(3), Flat(7))` (16384-value carrier; junk states leftover-paired): **`admit() == Ok`** — the Bennett-paid body is a total bijection. `eval(Iso::trace(u, body))` over the **whole 64-pair domain**: gcd component == independent Rust `gcd` on all 64, outputs pairwise distinct, degenerate zero arms exact. **T histogram: {0: 22, 2: 6, 3: 8, 4: 8, 5: 12, 6: 4, 7: 4}** — genuinely variable T, worst case gcd(7,1) = 6 subtractions + exit-check = 7. The variable-T signature the design wants — blocked from silicon only by Walls 2–3.

**E4 — what compiles under 256 today is trivial:** `Trace(Flat2×Flat2, Id)` (|u|=16, k_max=17, WCET 174) compiles, 16/16 green — but T ≡ 0 (immediate exit). Fitting ≠ variable-T.

**E5 — today's real variable-T ceiling (compiled ROM, park histogram):** the M-D.e walk `Trace(N5, mde_spill_walk)`: ROM 29 instrs, loop_terms (k_max=6, iter_len=25), **B = 154; input 0 → T=6 (surplus 0), input 1 → T=1 (surplus 125/154 = 81%)**; fast-oracle == eval on both. Variable T on a compiled ROM **already ships** — but it is the prior review's "bimodal toy," TH-1b saturated (Σ(T−1) = |u|): no richer T-distribution is reachable without changing k_max.

## 3. The minimal enabling change (and its bar)

**(i) Exact k_max in `lower_trace` (the crux).** Replace `k_max = |u|+1` by a trajectory-derived bound: after lowering the body, walk the **lowered body instructions** (`exec_instr` over marshalled register bytes — no eval dependency) from every `Right(a)` entry; `k_exact = max applications to exit (+1 safety)`, falling back to `|u|+1` if any entry exceeds it. Sound: the CPU only ever starts from real inputs (`run_on_cpu` marshals the input type), and reverse trajectories have identical lengths, so the park budget B = spine + k_exact·iter_len is a true WCET. Cost: O(|A|·T_max) byte-sim at compile time. **E6 measured what this unlocks under the EXISTING 256 cap / 8-bit PC:**

| w | T_max | k_exact | |u| today | body budget after exact-k_max |
|---|-------|---------|-----------|-------------------------------|
| 2 | 3 | 4 | 128 (k_today=129) | iter_len ≤ 63 ⇒ **body ≤ 53 instrs — comfortable** |
| 3 | 7 | 8 | 8192 (k_today=8193) | iter_len ≤ 31 ⇒ body ≤ 21 instrs — tight (mde's 5-leaf walk = 13) |
| 4 | 15 | 16 | 2^23 | body ≤ 5 instrs — **impossible: widen-PC genuinely required** |

**(ii) A pinned `gcd2_hist_step` recognizer** (mde_spill_walk pattern: name gate + full-table semantic check against a pinned byte map), emitting the compare/conditional-subtract/hist-shift walk. Budget 53 instrs at w=2 is ample.

Both (i) and (ii) touch `lower_trace`/`recognize_prim` — **control-flow lowering, HIGHEST BAR**: fresh-context adversarial impl-review + a behavioral worker→gate round-trip test per CLAUDE.md, L15/L20/L21. Neither is test/type-only. Nothing lighter ships a genuine variable-T algorithm: this POC is the proof (everything short of it either doesn't compile or has T ≡ 0).

## 4. Answers to the design's §4 questions

1. **Compile+run today?** No, at any domain. Firing order at w=3: UnrecognizedPrim (body), then k_max=8193/WCET=81934; at the design's 4-bit historyless framing: k_max=257/WCET=2574 (and Wall 1 makes that framing non-bijective anyway). It runs at **eval level** today (E2a, 64/64, T ∈ {0,2..7}).
2. **Minimal enabling change:** exact-k_max + pinned body recognizer — both lowering-touching (highest bar). See §3.
3. **The measure:** there is none. `k_max = |u|+1` from carrier cardinality (compile.rs:2535). `a+b` is the right *semantic* measure and exact-k_max computes its effect automatically (trajectory walk); a declared-measure type mechanism is unnecessary for slice-1.
4. **Leaves:** `(a,b)+tag` (3 payload) fits the cap but is not a bijection (Wall 1). The honest `(a,b,hist)+tag` fits **exactly** at w ≤ 3 (hist ≤ Flat(7|8)); w=4 needs 2 hist leaves and trips both the payload cap and the F5.2 `n_input+1 ≤ 4` guard.
5. **Honest framing:** the draft's framing is wrong in both directions. Small GCD does not fit today, AND widen-PC-as-slice-1 would be a category error: with k_max=|u|+1, an 8193-iteration budget for a ≤7-iteration loop means park tax ≈ 99.97% — a wider PC would "fit" an energy-absurd program while the actual blocker (k_max looseness) remains. **Widen-PC without exact-k_max buys nothing; exact-k_max without widen-PC buys w=2 (and marginally w=3).**

## 5. Recommended slice-1 + follow-on

**Slice-1 (recommended): "exact-k_max + 2-bit GCD" —** (a) trajectory-derived k_max in `lower_trace` (fallback `|u|+1`), (b) pinned `gcd2_hist_step` k-leaf recognizer (w=2, |u|=128, self-delimiting Flat(3) history), (c) promote the E2a construction to the shipped test: whole 16-pair domain, `run_on_cpu == eval == gcd` (L19 real-CPU authority), **T/park-tax histogram varying with input** — the first genuine variable-T *algorithm* on the machine, with **zero energy tax** (no `step.rs`/decoder change; energy goldens untouched). HIGHEST BAR discipline applies. What this POC settled: the program exists (admit Ok, eval-exact), the walls are exactly located and quantified, the leaf caps pass, and the budget arithmetic (53 instrs) leaves real room.

**Follow-on: widen-PC (W=8→9+)** — motivated by the now-concrete finding "w=4 GCD needs body ≤ 5 instrs even post-exact-k_max; w=3 is tight at 21." Blast radius as the draft's §2 lists (types.rs PC, step.rs pc_pred/pc_inc/fetch, compile.rs 2688/3209 caps, `delta_u` modulus at 2699, ROM≤256 assert, all CPU/energy goldens). **Energy-tax estimate from step.rs structure:** fetch runs `pc_pred` per ROM entry per step — a (W−1)-Toffoli chain computed and uncomputed (2·7 = 14 Toffolis/entry at W=8); +1 PC bit ⇒ +2 Toffolis/entry ≈ +14% on the fetch-predicate component, plus +2 Toffolis in `pc_inc`, plus fetch-core width 4W→4(W+1). Given the measured precedent that one extra `Op::ALL` decoder op cost every program +9.2% step_j (fib 6.62e-8→7.23e-8), expect **order +10–15% step_j per PC bit — measure and re-pin goldens when built**, and note it taxes every program forever, which is exactly why it must come after exact-k_max, not before.

## 6. POC code the delegation starts from
`/tmp/tloop-poc/crates/theseus-tisc/tests/tloop_poc.rs` → preserved at `docs/scratch/tloop_poc.rs.ref` (7/7 green, fast tier, bounded domains): E1 (Bennett non-bijection + admit reject), E2a (bijective gcd3 body admits + eval==gcd, T-histogram), E2b (compile walls in firing order), E3 (design arithmetic refuted), E4 (fitting shell is T≡0), E5 (today's mde-walk variable-T ceiling, TH-1b saturation), E6 (exact-k_max what-if table). `build_gcd3_body()` is the reusable bijective-GCD-with-history construction; `ref_gcd` the independent oracle.

## 7. Worktree state
`/tmp/tloop-poc` at detached `24a1c40`; one untracked file `tloop_poc.rs`. Nothing committed; `/home/chessai/rust/theseus` untouched. `cargo test -p theseus-tisc --test tloop_poc` — 7 passed, 0 failed, 2.1s.
