# Adversarial impl-review: T-loops slice-1a exact-k_max in `lower_trace` (worker commit 1cc212d vs 4b1ce33)

*(Advisor's faithful capture, L21. Fresh-context Fable 5, did not write the change. Fast-scoped
per the tooling constraint — the slow real-system_step H1 + mde_spill authorities were run
separately by the advisor, see §"Advisor slow-authority" appended at the end.)*

## VERDICT: SHIP

No path found to a too-small loop budget. The prime-suspect attack (walk layout vs real layout) is structurally closed by guards that run *before* the walk; I built the composed, product-split, permuted-prefix, spill, and uninhabited-payload cases and could not break it. One MINOR non-soundness follow-up (compile-time blowup) and two key assumptions to keep pinned. Fast regressions all green. **The slow authorities (H1 `#[ignore]` real-system_step tests in exact_kmax, mde_spill heavy, ring/energy suites) are DEFERRED TO THE ADVISOR — I did not run them.**

## A. Register-layout mismatch — DEFENDED (no finding; key assumption identified)

The walk sets state via `marshal(a_val, &a_ty)` (leaf i → `home_of(i,n,0)` = Reg(i) for i<8) plus `regs[n_input]=1` for the tag. The real body reads `body_homes[1..] = ctx.homes[0..n_input]`. These are guaranteed identical because, in this order in `lower_trace` and all **before** the k_max walk:

1. **F-1 canonical/occupied guard** (~line 2563): rejects unless `ctx.homes == Ctx::new(n).homes` AND `ctx.occ == OccSet::from_locations(&ctx.homes)`. `Ctx::new` delegates to the same `home_of` that `marshal` uses (the sole placement function, I-3/F-5).
2. **n_input+1 ≤ 4 guard**: with ≤3 input leaves, canonical homes are exactly `Reg(0..n_input-1)` — no Scratch/Mem home can reach the input leaves.
3. Only zero-init **padding** leaves spill to Scratch(2+); `marshal` zeroes scratch, and the real machine's padding is also zero — walk state matches.

So any Trace whose ctx could de-sync the walk is *rejected*, not mis-walked. Evidence (all built and run, fast):

- **Composed Seq**: `Seq(SwapSum(One,One), Trace(mde_walk))` compiles with the same k_max=6, whole-domain `run_on_cpu_fast == eval` 2/2. (Walk enumerates all of `a_ty` — a superset of what any prefix can deliver — so composition can only make the walk conservative.)
- **Times split**: `Times(Id(Bit), Trace(mde_walk))` → rejected with the F-1 message.
- **Permuted prefix**: `Seq(SwapProd(Bit,Bit), Trace(...2-leaf payload...))` — prefix restores canonical layout, compiles k_max=2, whole-domain 4/4 green.
- **Spill Trace** (mde_spill-style, 5-leaf carrier, padding at Scratch(2)): ROM contains the Xchg page bracket *inside* `body_instrs`, `exec_instr` models `Xchg` exactly; shipped F2/F3 run it green, k_max=6 as expected.
- Swapbr-bodies are rejected (`has_inner_control`) before the walk, so `exec_instr`'s non-PC-aware `Swapbr` handling is unreachable in the walk.

**Key assumption to keep pinned:** F-1 + `home_of`-as-sole-placement is the entire layout soundness argument. Any future relaxation of the F-1 guard (e.g. allowing relocated/resident homes under Trace) silently invalidates the walk. Worth one sentence in the code comment at the walk site (currently it asserts the marshal/homes match without citing F-1 as the reason it holds).

## B. exec_instr vs real stepping — fast oracle HAS teeth (verified destructively)

I edited compile.rs to append `k_max = k_max.saturating_sub(1)` and rebuilt:

- **Saturated mde_walk (true T_max=6, sound k=6, zero margin)** → teeth k=5: `run_on_cpu_fast` **detected it** — input `Left(Unit)` gave `cpu=Left(Unit)` vs `eval=Right(Unit)` (1/2 inputs mismatch). So the fast oracle catches the smallest budget shortfall that can corrupt (k = T_max − 1).
- **Id-body Flat(5) (T_max=1, sound k_exact=2)** → teeth k=1: 0/32 mismatches — *still green*, because k=T_max is exactly sufficient. This is correct, not a gap: the +1 safety in `k_exact = max_t + 1` is a real margin, so a hypothetical walk under-count of exactly 1 is *harmless* (budget still = T_max), and any under-count ≥ 2 (the first that corrupts) is exactly the B1 case the fast oracle detects.

Both edits reverted; also verified aux fidelity by tracing the bracket algebra: rc/rb/temp/ctrl are all 0 at body entry on every real iteration (prologue mints δu into temp, top bracket burns it; ctrl is uncomputed to 0 via `XOR ctrl,tag; XORI ctrl,1`), matching the walk's zeroed state — the per-iteration byte evolution of carrier registers is identical between walk and machine.

## C. Tag convention — SOUND, with one asymmetry worth a comment (NOTE, non-blocking)

Walk: start tag=1 (Right entry — matches the prologue `XORI tag,1` injecting Right), apply full body, break on `tag_byte != 0`; check happens only at iteration boundaries, matching the hardware's once-per-iteration bottom bracket. Id body: T=1, k_exact=2 (matches golden). mde_walk: T(0)=6, T(1)=1 → saturates → fallback 6 (matches shipped mde behavior).

The asymmetry: hardware routes on **bit 0 only** (`Cswap` tests `ctrl & 1` where `ctrl = tag ^ 1`), the walk on the **whole byte**. A tag byte of 2 would make the walk exit while the hardware continues → under-count. Unreachable: walk and machine execute byte-identical states from canonical marshalled inputs, and every lowered body maps canonical encodings to canonical encodings (tag ∈ {0,1} at boundaries) — the same pre-existing invariant the epilogue `XORI tag,1` and the `ctrl` uncompute already require (a tag of 2 was already fatal under the old bound: infinite loop / wrong output). Suggest a one-line comment at the walk's break condition citing this. Non-blocking.

## D. min-cap zero margin — SOUND

Saturated case k_max = |u|+1 is the unchanged ADR-0007 pigeonhole bound (Left-states distinct by injectivity → T ≤ |u|+1), and B1/B2 empirically pin the boundary semantics: budget k ≥ T suffices, k = T−1 corrupts. The min() can only ever tighten toward a bound the walk *proved* (T_max+1 over all real inputs); it cannot go below T_max because max_t is computed from the same byte-deterministic execution the machine performs (per A/B/C).

## E. Fallback threshold — CORRECT

Code order per iteration of the walk: exec body → `t += 1` → **tag-exit break** → `t >= u_inhabitants` fallback break. So a loop taking exactly T=|u| exits via the tag check first (k_exact = |u|+1 = fallback, no false fallback), and T=|u|+1 (pigeonhole max) correctly triggers fallback k=|u|+1=T — exactly sufficient. Non-terminating junk cannot hang compile: each trajectory is capped at |u| applications. Uninhabited payload probe: `a_ty = Zero` compiles with k_max=1 — vacuously sound.

**MINOR (non-soundness, follow-up):** the walk costs O(|enumerate_values(a_ty)| · |u| · body_len) and runs **before** the WCET-≤256 guard. `n_input ≤ 3` caps leaves but not cardinality: a payload of three Flat(8) leaves is 2^24 walked inputs at compile time — minutes of compile even when the Trace would be rejected moments later (and the maestro gate has no timeout). Suggested cheap fix in a follow-up: bail to fallback (or run the WCET pre-check with k_fallback first) when `|a_ty| · |u|` exceeds a threshold. Not blocking: no current Trace is near this, and it cannot produce a wrong k_max, only a slow compile.

## F. Fast regression tallies — ALL GREEN

- `--test exact_kmax`: 4 passed, 0 failed, 2 ignored (the H1 authorities — advisor's).
- `--test lfsr`: 4 passed, 2 ignored.
- Full fast `cargo test -p theseus-tisc`: every suite ok, 0 failures.
- `--test compile --features heavy-tests -- --test-threads=1`: **65 passed, 0 failed, 23 ignored** (real-CPU/ring authorities), 149s.
- **Golden 76→126 NOP change is legit**: `mdd_reject_wcet_overflow` tests WCET-overflow *rejection*. Under exact-k_max the old 76-NOP body no longer overflows (Id-equivalent body ⇒ k_exact=2, wcet=176 ≤ 256 — the intended feature), so the body was enlarged to 126 NOPs (wcet 276 > 256) to keep the rejection path exercised. No energy/codegen golden was re-pinned.

## Hygiene
`/tmp/exactkmax-review` is clean at 1cc212d: teeth edit reverted, temp test files deleted.

## Deferred to advisor (NOT run here)
`h1_large_u_id_body_real_cpu`, `h1_mde_walk_real_cpu` (real system_step authority), `mde_spill --features heavy-tests`, ring/energy suites. Given B1 proved the fast oracle detects the minimal corrupting shortfall, H1's marginal value is confirming `run_on_cpu_fast`'s pinning to the real CPU on these specific ROMs — still required per L19 authority discipline.

---

## Advisor slow-authority (run separately, box to itself) — GREEN
The advisor ran the deferred L19 real-system_step authorities alone (no lock contention):
- `exact_kmax --ignored --test-threads=1` (H1): **`h1_large_u_id_body_real_cpu` ok, `h1_mde_walk_real_cpu` ok — 2 passed, 0 failed** (7266s / ~2h — real system_step is slow; peak 6.6 GB). The new exact-k_max Traces run_on_cpu == eval on the REAL closed CPU.
- `mde_spill --features heavy-tests --test-threads=1`: 4 passed, 0 failed, 2 ignored (existing M-D.e Trace lowering regression intact).
All authority tiers green → SHIP confirmed; merged.
