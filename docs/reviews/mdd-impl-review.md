# M-D.d IMPLEMENTATION review (Trace while-loop)

> Verbatim fresh-context adversarial review (model: Fable), recovered from the
> ephemeral Agent-tool transcript (agentId `aff6ad452ebf1fcac`) and committed per maestro L21.
> Findings and their test mapping are indexed in [README.md](README.md).

---

All verification complete. Here is the full review.

---

# Adversarial review — M-D.d: `Trace` as reversible while-loop (worktree `01KXTBA9W89AB3MJ7SFW0FQFRN`, vs `chessai/theseus-v2`)

## (A) VERDICT: **DO-NOT-SHIP** (two reachable silent-miscompile paths; the core loop skeleton itself is correct and the fixes are small)

The §2 skeleton, the δu arithmetic, and the come-from token discipline are exactly right — I traced them instruction-by-instruction and could not break them. What breaks is the **boundary** of the construction: the compiler accepts Trace in a position where its hand-assigned registers alias live data (confirmed wrong outputs, invisible to every terminal check), and the design's mandated Prereq A (F7) was never shipped, so a miscompiled body can be fed into a correct loop. Both are the exact "invisible corruption" class the design says must be rejected loudly.

## (B) Numbered findings

**F-1. BLOCKER — `Times(Trace(u,body), g)` compiles and silently clobbers `g`'s live registers.**
`crates/theseus-tisc/src/compile.rs:1684-1690` guards "canonical context" by comparing `ctx.homes` to `Ctx::new(ctx.homes.len())` — but a **left** `Times` split (`Ctx::split`, compile.rs:283) produces homes `Reg(0..n_left)`, indistinguishable from a true top-level context. `lower_trace` then hand-assigns `tag = Reg(n_input)` (1737), extra payload `Reg(n_input+1..)` (1741-1743), and aux `rc/rb/temp/ctrl = max_carrier_reg+1..` (1811-1815) **without ever querying `ctx.occ`** — those registers hold the right factor's live data. Reproduced (fresh build against this worktree):

- `Times(Trace(Bit, 3cycle), Id(Bit×Bit))` compiles (18 instrs); whole-domain fast oracle: **2 of 4 inputs WRONG** (e.g. input `(Unit, (R,L))` → cpu `(Unit,(R,R))`, eval `(Unit,(R,L))`).
- `Times(Trace, Id(Bit))`, input `(Unit, Right)`: final `regs=[1,1,0,0,0,0,0,0]`, `BR=0`, `s0=s1=0` — dirty payload residue in `Reg(1)`, and the P1/P3 terminal check passes, so the **real** CPU path is silently wrong too, not just the fast sim.

Why: the prologue `XORI tag,1` (1828) is keyed on the assumption tag starts at 0; here tag = g's data bit, so for bit=1 the loop injects **Left** instead of Right and the whole ctrl/come-from discipline desyncs — yet often re-parks with BR=0.
Fix: in `lower_trace`, reject (Deferred) unless `tag_reg`, extra-payload regs, and all four aux regs are **unoccupied in `ctx.occ`** (the information is already in hand at 1747-1750 where `body_occ` is built), or more bluntly reject any `ctx.occ` with occupied locations outside `ctx.homes`. Ship with a regression test = my repro (L20).

**F-2. MAJOR — Prereq A (F7) was not shipped, and Trace makes it live.**
Design of record (docs/scratch/mdd-design.md §6.1) requires the assoc/unite fix-or-reject to **land first**. `compile.rs:1518-1525` still lowers `AssocLSum/AssocRSum/UniteSum/UnitiSum` as unconditional no-ops. Reproduced: `SwapSum(One,Bit) ∘ AssocRSum(One,One,One)` (the design's own 3-cycle structural route, §7/A2) compiles to a single `Xori{rd:0}` and gives **wrong answers on 2 of 3 domain values** (`L(R())→R` instead of `L(L())`; `R→L(L())` instead of `L(R())`). Worse: `Trace(Bit, that-composite)` **compiles (15 instrs)** — a correct loop faithfully iterating a miscompiled body, and since that Trace is `One ↔ One` the output oracle can't even see it. Fix-or-reject non-trivial nested-sum assoc/unite, with the design's reproducing test.

**F-3. MAJOR (test integrity) — the nested-Trace and layout-invariant guards have zero genuine coverage.**
`mdd_reject_nested_trace` (tests/compile.rs:1549) contains **no assertion at all** (just an `eprintln`). `mdd_reject_layout_invariant` (tests/compile.rs:1516) builds `Trace(One, Trace(One, Id(1+1)))`, whose body has type `One ↔ One` — it is rejected by the **Sum-shape type check** (compile.rs:1644-1650), never reaching `contains_trace` (1692) or the layout guard (1773-1776). The guard itself is real — I built the well-typed nesting `Trace(One, Plus(Id(One), Trace(One, Id(1+1))))` and it fires correctly ("come-from join does not compose") — so the genuine test is a 5-liner; ship it. (The layout-invariant guard at 1773 is additionally unreachable in debug builds: `lower()`'s `debug_assert_eq!` at 1408-1412 panics first for any homes-changing body.)

**F-4. MEDIUM — the flagship A2 oracle's output domain is informationally empty.**
The 3-cycle Trace is `One ↔ One`: `unmarshal` returns `Unit` no matter what the machine did. On the real CPU the only signal is the P1/P3 terminal check (BR/s0/s1, compile.rs:2279) — which, per the design's own §1 warning, **passes on a mid-loop stop** (I verified: an under-budget stop lands at a body boundary or post-`Cswap` point where BR=0, s0=s1=0; only `temp=δu` residue betrays it, and P1/P3 doesn't look at GPRs). So `mdd_3cycle_trace_real_cpu` (tests:1304, `#[ignore]` per repo convention) would NOT catch a `k_max=|u|` regression; only `mdd_wcet_golden`'s hardcoded 46 and `mdd_park_under_loop`'s aux asserts (fast sim only) do. Recommend: (a) add a Trace with an informative carrier (e.g. `u=Bit, a=b=Bit`, body a 2-leaf prim pattern) to A1/A2, and/or (b) assert the full final register file in the real-CPU test, mirroring tests:1391-1398.

**F-5. MEDIUM — missing guard: output/input payload leaf-count mismatch.**
`lower_trace` computes `_n_a` (1662) and `_b_ty` (1652) and drops both. It never asserts `n_input == num_leaves(a_ty)` nor `num_leaves(b_ty) == n_input`. If `|b|`-leaves ≠ `|a|`-leaves (possible with equal inhabitant counts, e.g. `b = Sum(One, Sum(One, Zero))`), unmarshal reads the **tag register as a payload leaf** → silent wrong output. Today this is unreachable only by accident (recognize_prim's 2-leaf patterns require equal 2-leaf carriers); the guard is one line — add it.

**F-6. LOW — `body_occ` comment lies (compile.rs:1746-1750):** "mark all carrier + aux registers as occupied" — only carrier homes are set; the aux regs are never marked. Currently safe (aux are provably 0 at every body entry, and body-internal spare use restores 0), but this is exactly the occ bookkeeping F-1's fix needs; do them together.

**F-7. LOW — untested guards:** the F6 Swapbr-body rejection (1766) has no test (I confirmed it fires on `Trace(Bit, Plus(SwapSum(1,1), Id(One)))`); `mdd_reject_wcet_overflow`'s first half (tests:1456) actually exercises MAX_LEAVES_LOOP, not WCET (the second half, 76×SwapSum → wcet 262, genuinely hits the WCET guard at 1789 — verified SwapSum lowers to 1 `Xori` at compile.rs:1500-1511).

**F-8. NITs:** `run_on_cpu_fast` docstring still says "budget is `instrs.len()` steps" (compile.rs:2392) — now wcet. `mdd_park_under_loop` doesn't assert `regs[0]` (tag) and `regs[1]` (payload) at exit.

## (C) δu / iter_len arithmetic (Q1): **EXACTLY CORRECT**

Emitted layout (compile.rs:1826-1865), Trace base = offset 0: prologue `[0]=Xori tag`, `[1]=Addi temp,δu`; **Ltop = [2] `Swapbr rc`**; `[3] Cswap ctrl,rc,temp`, `[4] Subi temp,δu`, `[5] Xor ctrl,tag`, `[6] Xori ctrl,1` (5 instrs Ltop..body); body `[7..7+L)`; `Xor, Xori, Addi, Cswap ctrl,temp,rb`, **Lend = [L+11] `Swapbr rb`** (5 instrs body..Lend); epilogue `[L+12] Subi`, `[L+13] Xori tag`. So Ltop..Lend inclusive = 5+L+5 = **iter_len = L+10 = L+LOOP_OVERHEAD** ✓, Lend = Ltop+(iter_len−1) ✓, total = spine(4)+iter_len ✓ (debug_assert at 1873-1879 agrees). Step semantics verified: `exec_instr` Swapbr = `swap(reg, BR)` (compile.rs:2363-2365), PC update is `PC += BR + 1` **after** execute using post-swap BR (step.rs:34-43, fast sim compile.rs:2419). Taken jump: `PC = Lend + δu + 1 = Ltop + (iter_len−1) + (256−iter_len) + 1 = Ltop + 256 ≡ Ltop (mod 256)` ✓. Token conservation hand-traced for all three comers (first entry, backward re-entry, exit): `rc=rb=temp=ctrl=0` at every body entry and at exit; ctrl uncompute at top uses the tag unchanged since bottom-compute ✓; both CSWAPs use `rd=ctrl` as control with the correct swap pairs (Cswap is symmetric in rs/rt, compile.rs:2348-2352) ✓. Skeleton matches design §2 instruction-for-instruction, no deviation. `δu` fits u8 for all accepted programs (WCET guard at 1789 forces iter_len ≤ 126 before δu is computed at 1800).

## (D) Does run_rom_on_cpu run the compositional wcet (Q3): **YES**

`run_rom_on_cpu` now takes explicit `budget`/`s_max_val` (compile.rs:2243-2248) and runs **exactly `budget` steps** (2271); `run_on_cpu` passes `lowered.wcet()` / `lowered.s_max()` (2308-2313); `run_on_cpu_fast` also runs `lowered.wcet()` (2406). `wcet = |instrs| + Σ(k_max−1)·iter_len` correctly counts the first iteration in ROM length; for t = k_max the budget is consumed exactly (4 + 3·14 = 46 for the 3-cycle), and shorter trajectories idle in the dead zone within the P2 bound `budget + s_max ≤ 256` (2255). `max_skip_distance` (2190) survives but is **out of the compiled-program path** — only tests and hand-built ROMs use it; on a Trace ROM it would pair Ltop/Lend and **over**-count (safe direction), never under-count (body is Swapbr-free by the F6 guard at 1766). Forward-skip accounting was correctly added at every `emit_skip_block` site (1140, 1593, 2035, 2113, 2119).

## (E) Per-test genuineness

| Test (tests/compile.rs) | Verdict |
|---|---|
| `mdd_wcet_golden` :1337 | **Load-bearing** — hardcoded 14/4/18/46 pins iter_len, spine, and k_max=3; would catch k_max=\|u\| |
| `mdd_park_under_loop` :1368 | **Load-bearing** (best dynamic test): aux+BR asserts catch δu off-by-one (runaway BR≠0) and mid-loop stops (temp=δu); over-run stability real. But **fast sim only** |
| `mdd_3cycle_trace_fast` :1289 | Weak — output domain is `Unit` (see F-4); value = "compiles, runs, no panic" |
| `mdd_3cycle_trace_real_cpu` :1304 | `#[ignore]` (repo convention, fine); terminal check gives it teeth against δu errors but **not** against budget under-runs (F-4) |
| `mdd_adjoint_trace` :1313 | **Genuine** — body is not self-inverse (verified: f(0,0)=(1,0) vs f⁻¹(0,0)=(0,1)); adjoint runs t=3 with two backward re-entries through freshly-lowered `Trace(u, body†)`; inverse prim pattern (compile.rs:820-833) is the correct inverse of (a⊕b⊕1, a) |
| `mdd_bare_trace_compiles` :1272 | Genuine smoke (t=1 exit path) |
| `mdd_reject_wcet_overflow` :1456 | Half genuine: part 1 tests MAX_LEAVES_LOOP (any-error assert), part 2 genuinely trips the WCET guard with message check |
| `mdd_reject_layout_invariant` :1516 | **Vacuous for its name** — trips the Sum-shape type check, not the layout guard (F-3) |
| `mdd_reject_nested_trace` :1549 | **Fully vacuous** — no assertion (F-3) |
| `mdd_system_ring_closure` :1413 | `#[ignore]`, structure matches the mexch_reg precedent; fine |

## (F) What is SOLID

- The **loop skeleton is a faithful, exact implementation of the hand-verified §2/§D design** — every instruction, operand, and ordering checked; the conserved-token invariant holds on all comers; exit leaves `rc=rb=temp=ctrl=tag=BR=0` so sequential composition works (verified `Seq(Trace,Trace)` end-to-end).
- **δu arithmetic is exact** (C above) — no off-by-one anywhere, including the ROM-offset independence of δu (relative distance only).
- **k_max = |u|+1 is correctly inhabitant-based**: `enumerate_values(u).len()` (compile.rs:1666; theseus-core/src/prim.rs:42-62 enumerates inhabitants, not leaves/bits) — Bit → 2 → k_max 3 ✓.
- **Prereq B landed properly**: `Lowered{instrs, forward_skip_total, loop_terms}` threading is complete across Seq/Times/Plus/Sym/Iter/Fold/controlled-idiom; budget = compositional wcet everywhere.
- Every guard I attacked that exists **actually fires**: non-canonical ctx (Times-right, Plus/iter/fold bodies), Swapbr-in-body, well-typed nested Trace, WCET overflow, MAX_LEAVES_LOOP, uninhabited u.
- The 3-cycle witness itself is mathematically right: `R()→L(R())→L(L())→R()`, t=3=|u|+1, prim byte-pattern `(a⊕b⊕1, a)` verified against the emitted 4-instruction GF(2) sequence.

**Bottom line:** the hardest part — the backward jump and come-from — is correct. The ship-blockers are perimeter failures: F-1 (occ-blind register grab under `Times`, silent corruption, repro'd) and F-2 (mandated Prereq A absent, silent body miscompile inside a correct loop, repro'd), plus F-3's vacuous guard tests. Fix those three with L20 regression tests (my repros are ready-made) and this is shippable.
