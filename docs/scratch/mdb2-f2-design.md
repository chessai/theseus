# M-D.b2.f2 — reversible in-memory array map (`iter` ∘ `memexch`)

**Status:** design of record — **REVIEWED** (fresh-context Fable, verdict *SHIP-WITH-FIXES*,
no blockers; all findings folded — `docs/reviews/mdb2-f2-design-review.md`). Builds on f1
(`da9ca8f`: `memexch` prim + `MexchReg` emission + resident-array marshalling). Chosen **d=4**
(F2-1). Control-flow/lowering-adjacent → CLAUDE.md adversarial-review bar (design ✓, impl
pending), reproducing tests per finding (L20/L21).

> **Post-review deltas:** (F2-1) `d=4` so `k>n` (the `g²` case) is reachable + tested. (F2-2)
> the energy deliverable needs a NEW `rom_program_energy_resident` — the existing
> `rom_program_energy` hardwires the non-resident compile/marshal and would `ResidentMisplaced`
> on the arrayMap ROM (this also resolves f1's I-5 tautology). (F2-3) the reduce finding was
> WRONG — an XOR/parity reduce IS expressible today via the borrow-swap idiom; only an
> *arithmetic* sum is blocked, by single-leaf value width (F-1), not by memexch. (F2-4) the
> home guard runs ONCE on the shared lowering (copies are duplicates), not per copy. (F2-5)
> park tax is 40%→0 at d=4 (the 6-instr skip spine always runs), not "~max". (F2-7) add a d=5
> displaced-idx oracle test — the one regime where the disjointness argument does real work.

## 0. Probe result (recon done — the composition already works)

A throwaway probe (`iter Nat3 memexch`, resident=2) **compiled (14 instrs, emits `MexchReg`)
and ran `run_on_cpu == eval` over all 48 inputs.** So `iter` + the f1 `memexch` prim + the
resident array **compose today**: the memexch home-layout guard (F-2: arr leaves at
`[Mem(0..n)]`) survives `iter`'s `dctx.split(n_counter)` body-context displacement (split
preserves absolute homes), and f1's occ-seeding keeps `allocate_aux` off the resident words.
**f2 is a build-the-program task, not a discover-a-gap task** — no new emission machinery. The
probe's `iter memexch` is degenerate (same address every iteration); f2 adds the *varying
address* + a per-element transform to make it a real map.

## 1. Goal

A compiled program that **maps a bijection `g` over the first `k` words of the resident array**,
`k` a runtime `Nat` count — the first program where the `MexchReg` address *varies per
iteration* (real data-dependent addressing under load), and the **second M-E energy artifact**,
carrying a genuine **WCET park-tax histogram** across `k` (tight `k_max = d` from the numeral
count — like fib, unlike the loose raw-Trace `|u|+1` bound TH-1b warns about).

## 2. The program (d = 4)

```
arrayMap_g : Nat_4 × (data:Bit) × (idx:Bit) × Arr[Bit;2]  ↔  (same)     [8 leaves; head 6 ≤ 14]
arrayMap_g = iter Nat_4  BODY
   BODY : (data, idx, arr) ↔ (data, idx, arr)
        = memexch ; (g ⊗ id_{idx,arr}) ; memexch ; (id_data ⊗ succ_idx ⊗ id_arr)
```

- **`memexch`** = the f1 prim on `(data, idx, arr[2])` (idx = the address register `ra`).
- **`g`** = a single-leaf bijection on `data` (baseline `g = notb`, a NOT). Lowers to `Xori{data,1}`.
- **`succ_idx`** = the reversible successor on the `Bit` index (`notb`: `0↔1`), leaving
  `data`,`arr` untouched — this makes the address *vary* across iterations.
- **`Nat_4`** = the finitized counter, `k ∈ {0,1,2,3}` (4 leaves). `iter` preserves the counter.

`d=4` is deliberate (F2-1): free regs 6,7 are exactly the iter aux `rb`/`temp`, so **no leaf is
displaced to memory** (the comfortable regime; the displaced regime is exercised separately at
d=5, T-F2-7); `B=30`, `B+S_max=42 < 256`; and `k=3 > n=2` is reachable so the `g²` case is tested.

**Per-iteration effect** (start `data=D`, `arr[idx]=A`):
`memexch`→(data=A, arr[idx]=D); `g`→data=g(A); `memexch`→(arr[idx]=g(A), data=D); `succ`→idx flips.
Net: **`arr[idx] ↦ g(arr[idx])`, `data` preserved (borrowed scratch), `idx` advanced.**

**Bijectivity (no Bennett witness).** Each iteration is a composition of involutions (`memexch`,
`notb`) and a bijection `g` → a bijection; `iter` of a bijection is a bijection; counter and
`data` preserved, `idx` advanced deterministically, `arr`'s touched elements get `g` (invertible
by `g⁻¹`). For `k ≤ n` each element is mapped ≤ once (the clean "map"). For **`k > n`** (reachable
at d=4: k=3 hits `idx mod n` twice) the effect is a well-defined `g^(count)` per word — still a
bijection; eval and run_on_cpu agree by construction (both apply BODY exactly `k` times). We do
NOT special-case k>n; the oracle covers it and T4/T-k3 pins it.

## 3. Lowering — reuses f1, no new machinery

`BODY` is a sequential composition of **recognized** operations (`memexch`→`MexchReg` via the f1
recognizer; `g=notb`, `succ=notb`→`Xori` via the existing prim arm; `⊗ id` = structural). None
emit skip-blocks, so `has_inner_control(body_instrs)` is false → `iter`'s guarded static unroll
accepts it (compile.rs:1931). Compiled via `compile_with_metadata_resident(arrayMap, resident=2)`.

**Mechanism (corrected per F2-4):** the body is lowered **ONCE** (compile.rs:1926–1927) against
`dctx.split(n_counter)`; the `d−1` unrolled copies are **literal duplicates** (1949–1957). The
memexch home guard therefore runs **once** on the shared lowering — its `arr@[Mem(0),Mem(1)]`
conclusion carries to every copy because (i) every lowering returns with canonical layout
(checked by the `debug_assert_eq!` layout invariants at 1724–1729 / 1938–1942) and (ii) all
register brackets are LIFO self-inverse, so the runtime layout is restored at each instruction
boundary. `succ_idx` mutates idx's *value*, never its *home*. Because `succ` is *inside* BODY,
duplicate copy `j` runs with `idx` already advanced `j` times → each copy's `MexchReg{rd:data,
ra:idx}` addresses the next element **at runtime** (the varying address).

## 4. Energy (M-E) — the park-tax histogram (quantified per F2-5)

`iter`'s guarded unroll skips `(d−1−k)` iterations for counter `k`: a skipped copy's `Swapbr`
jumps `body_len`, so the productive length is **`T(k) = 6(d−1) + body_len·k`** (the 6-instr skip
spine — Addi/Cswap/Swapbr/Swapbr/Cswap/Subi — always runs; only the `body_len=4` body
instructions are skippable). At **d=4**: `B = 30`, `T(k) = 18 + 4k`, so

| k | T(k) | park_tax=(B−T)/B |
|---|---|---|
| 0 | 18 | **40.0%** |
| 1 | 22 | 26.7% |
| 2 | 26 | 13.3% |
| 3 | 30 | **0%** |

A genuine program-level histogram, **tight** because `k_max=d` is a numeral bound (not the loose
raw-Trace `|u|+1`). `total_j = B × step_energy` exact (the M-E static-inventory model); the
per-step net differs from fib's (two `MexchReg`/body → different K0-buffer inventory). **This is
NOT the "near-total idling" my first draft claimed** — the spine floor caps park_tax at 40%.

**Deliverable / new API (F2-2):** `rom_program_energy` hardwires `compile_with_metadata` +
`marshal` (resident=0) and would `ResidentMisplaced` on this ROM (energy.rs:408,416). Add
`rom_program_energy_resident(iso, input, resident, db, t_ramp, step_net)` using
`compile_with_metadata_resident` + `marshal_resident`, and route f1's T7 through it too
(resolving the I-5 tautology: cross-check `total_j` from the API vs `steps_b × step_j`).

## 5. Scope

IN: `arrayMap_notb` (d=4, n=2); `run_on_cpu == eval` (fast whole-domain + `#[ignore]` real-CPU);
the map-semantics + varying-address assertions; the park-tax histogram; `rom_program_energy_resident`
+ the energy price; the d=5 displaced-idx oracle test. OUT (follow-ons):
- **Reduce (corrected per F2-3).** A **parity/XOR reduce IS expressible today** via the same
  borrow-swap idiom: carrier `(acc, data, idx, arr)`, `BODY = (id⊗memexch) ; (SwapProd;xorInto;
  SwapProd on (acc,data) ⊗ id) ; (id⊗memexch) ; succ_idx` computes `acc ⊕= arr[i]` from existing
  recognized prims — a good f3 candidate (ships with its own oracle). What is **not** expressible
  is an **arithmetic** sum: no single-leaf type holds a value > 1 (the F-1 thermometer fact;
  `1+1=2` is unrepresentable, so `addInto` on Bit leaves isn't domain-closed). The blocker is
  element/accumulator **width** (→ M-D.c widening), **not** swap-vs-read. (Do NOT record the
  false "memexch can't reduce" lesson.)
- Wider arrays / `n>2` index (needs the F-1(ii) thermometer-pack + MEM_WORDS widening) — deferred.

## 6. Reject-don't-corrupt

- Index range: `succ_idx` on a `Bit` stays in `{0,1} < n=2` (F-4). `n>2` needs >1 index leaf +
  a thermometer-pack → reject/deferred, never silently wrong.
- The f1 home guard (F-2) + semantic-verify (F-3) remain the guards (they run once on the shared
  body lowering — F2-4); f2 must not weaken them.
- If the two-memexch body trips `has_inner_control` or the home guard → clean reject, never a
  corrupt ROM. (Probe: won't for one memexch; T2/T-F2-7 confirm for two, incl. the displaced regime.)

## 7. Acceptance (each finding → a reproducing test — L20/L21)

- **T1 (emission):** `arrayMap` (d=4) compiles to a ROM containing exactly `2·(d−1) = 6`
  `MexchReg` (two per body copy) — assert the count, not just presence.
- **T2 (fast oracle, whole domain):** `run_on_cpu_fast_resident == eval` over the entire
  `(Nat_4 × data × idx × arr)` domain (64 inputs) — includes `idx=1` and **`k=3 > n`** (the g² case).
- **T3 (real-CPU, `#[ignore]`):** `run_on_cpu_resident (real system_step) == eval` on a sample
  that includes `k≥1` AND `idx=1` (the L19 authority — must exercise a *nonzero, varying* address;
  do NOT repeat f1's I-2 all-zeros mistake).
- **T4 / T-k3 (map semantics + g²):** k=2, idx=0 ⇒ `arr_out = (g(arr0), g(arr1))`, data/counter
  unchanged; **k=3, idx=0, g=notb ⇒ `arr_out = (arr0, ¬arr1)`** (g²=id on the doubly-hit word),
  `idx_out = idx0 ⊕ 1` — pins the map + the g² regime.
- **T5 (varying address, exact shape — F2-6):** the per-copy body slice is exactly
  `[MexchReg{rd,ra}, Xori{rd,1}, MexchReg{rd,ra}, Xori{ra,1}]`, the two `MexchReg` share `ra`,
  and `succ`'s `Xori{ra,1}` is the LAST body instruction — proves the address varies (not a
  constant-folded single element).
- **T6 (park-tax histogram, exact — F2-2/F2-5):** via `rom_program_energy_resident`: `Ok`;
  `B=30`; `T(k)=18+4k`; `park_tax` = {40%, 26.7%, 13.3%, 0} strictly decreasing, `park_tax(3)=0`;
  `total_j == B·step_j` exact.
- **T7 (closure, `#[ignore]`):** the map ROM's `MexchReg` steps stay bus-quiescent on the
  `SystemRing` (`run==eval^R`, reuse the f1/M-D.b harness), on an `idx=1`,`k≥1` state.
- **T-F2-7 (displaced-idx regime, d=5):** compile arrayMap at d=5, assert the aux page-in
  contains a displacement `Mexch{_, addr≥2}` (idx displaced to `Mem(2)`), and
  `run_on_cpu_fast_resident == eval` over the whole domain (80 inputs) — fails if the occ seed
  or the free-slot pop order regresses (the I-1 class). (Not an F-4 test: idx is a `Bit`, so no
  out-of-range address is representable here; F-4 reject coverage stays f1's T5-F4.)
- OOM discipline: real-CPU/closure tiers `#[ignore]` + serialized.

## 8. Residual obligations for the IMPL reviewer

Confirm the code realizes: (1) the body compiles to exactly the T5 slice (two memexch sharing
`ra`, g/succ as `Xori`); (2) `rom_program_energy_resident` actually runs on the resident ROM
(not a hand-composed dodge) and f1's T7 is rerouted through it; (3) T2 includes k=3 and T-F2-7
includes the d=5 displacement with a real `Mexch{_,≥2}`; (4) T3 exercises idx=1,k≥1 on the real
CPU. Prefer a different model from the implementer.
