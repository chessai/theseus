# M-D.b2.f3 — reversible in-memory XOR/parity reduce (`iter` ∘ `memexch` + accumulator)

**Status:** design of record — **REVIEWED** (fresh-context Fable, verdict *SHIP-WITH-FIXES*,
no blockers; all 5 minors folded — `docs/reviews/mdb2-f3-design-review.md`; the reviewer
independently recomputed the d=3/d=4 instruction counts = the probe's 24/56 exactly). Builds
on f1 (`memexch`/`MexchReg` emission) + f2 (`iter ∘ memexch` array map, `da9ca8f`/`52704b5`).
Composes emission under `iter` with a borrow-swap accumulator → CLAUDE.md adversarial-review
bar (design ✓, impl pending), reproducing tests per finding (L20/L21).

> **Post-review deltas:** (F3-1) the physical `Xor` writes the **data-home** register
> (`Xor{rd:data, rs:acc}`), not acc's — because the leading `Exch` swaps them first; T5 pins
> the exact slice so an implementer can't "fix" the assertion backward. (F3-3) T4 gains an
> `idx0=1` case — the one regime where a faithfully-compiled *idx0-ignoring* wrong program is
> invisible to `run==eval`. (F3-4) T-d4 pins the d=4 shape (56 instrs / 6 MexchReg / 3 Xor).
> (F3-2) §6 aliasing claim corrected (xor has no reject; unreachable via injective homes).
> (F3-5) the f3.5 dot-product needs a CCNOT recognizer / nested-control — not "just once
> XOR-reduce lands."

## 0. Probe result (recon done — composes in both regimes)

A throwaway probe (`reduce = iter Nat_d [ (id⊗memexch); assocL;(reduceStep⊗id);assocR;
(id⊗memexch); succ ]`, `reduceStep = SwapProd;xor;SwapProd`) **compiled and ran
`run_on_cpu == eval` over the whole domain at BOTH d=3 (24 instrs, 4 MexchReg, 2 Xor, 96
inputs) and d=4 (56 instrs — idx displaced to mem, 6 MexchReg, 3 Xor, 128 inputs).** So the
XOR-reduce is expressible **today** — confirming the f2 review's F2-3 (the borrow-swap idiom
IS a non-destructive indexed read; "memexch can't reduce" was wrong). The `AssocLProd`/
`AssocRProd` regrouping compiles to **nothing** (product re-association is a leaf-identity),
so the grouping tension (below) costs zero instructions. **f3 is a build-the-program task; no
new emission machinery.**

## 1. Goal

A compiled program that **XOR-reduces the first `k` words of the resident array into an
accumulator** (`acc ⊕= arr[i]`), `k` a runtime `Nat`. Completes the **map + reduce pair**
(f2 = map, f3 = reduce), a **third M-E energy artifact** with its own park-tax histogram, and
the first program with a genuine *accumulator* threaded through a memory-touching loop. The
`MexchReg` address varies per iteration (like f2). Overturns the "reduce needs a new
primitive" myth on the record.

## 2. The program (d=3 primary, d=4 secondary)

```
reduce : Nat_d × (acc:Bit) × (data:Bit) × (idx:Bit) × Arr[Bit;2]  ↔  (same)
reduce = iter Nat_d  BODY
   BODY : (acc, data, idx, arr) ↔ (acc, data, idx, arr) =
        (id_acc ⊗ memexch)                                  # data ↔ arr[idx]
      ; assocL ; (reduceStep ⊗ id_{idx,arr}) ; assocR       # acc ⊕= data (= arr[idx])
      ; (id_acc ⊗ memexch)                                  # data ↔ arr[idx]  (restore)
      ; (id_{acc,data} ⊗ succ_idx ⊗ id_arr)                 # idx advances
   reduceStep : (acc, data) ↦ (acc⊕data, data)  = SwapProd ; xor ; SwapProd
   xor : (a,b) ↦ (a, a^b)  (recognized → Instr::Xor);  succ_idx = notb (0↔1);  memexch = f1 prim
```

- **Carrier** `Nat_d × (acc × (data × (idx × arr)))`. arr = the trailing 2 leaves (resident=2);
  head = `d + 3` leaves (counter + acc + data + idx). `memexch` acts on `(data, idx, arr)`;
  the **acc leads** so arr stays globally trailing and memexch's sub-context arr is at
  `[Mem(0),Mem(1)]` (the f1 F-2 home guard passes — verified by the probe).
- **The grouping tension (resolved, §3):** `memexch` wants `(data,idx,arr)` contiguous;
  `reduceStep` wants `(acc,data)` contiguous; `data` is shared. `AssocLProd`/`AssocRProd`
  regroup `(acc,data)` for the XOR step and back — **free** (leaf-identity), so only the
  reduceStep is wrapped (once), not the two memexches.

**Per-iteration effect** (start `acc=C, data=D, arr[idx]=A`): memexch → `data=A, arr[idx]=D`;
reduceStep → `acc=C⊕A, data=A`; memexch → `data=D, arr[idx]=A` (restored); succ → idx flips.
Net: **`acc ⊕= arr[idx]`, `data` preserved (borrowed scratch), `arr` preserved, `idx`
advanced.**

**Bijectivity (no Bennett witness).** XOR is self-inverse; the per-iteration op is a
composition of involutions (memexch, SwapProd, notb) and the invertible xor → a bijection;
`iter` of a bijection is a bijection; counter, `data`, and `arr` are preserved, `acc`
accumulates `⊕ arr[touched]`, `idx` advances deterministically. Invert by XOR-ing the touched
elements back.

**d:** **d=3 primary** (`k ∈ {0,1,2}`; head=6 → free Reg6,7 = the iter aux rb/temp, **no
displacement**; clean histogram). **d=4 secondary** (`k ∈ {0..3}`; head=7 → 1 free reg → iter
aux **displaces idx to `Mem(2)`** — the displaced regime f2's T-F2-7 proved; AND `k=3 > n=2`
exercises **XOR-twice-cancels**: position 0 hit at iters 0,2 → `arr[0]` cancels).

## 3. Lowering — reuses f1/f2, no new machinery

`BODY` is a sequential composition of **recognized / free** operations: `memexch`→`MexchReg`
(f1 recognizer + F-2/F-3 guards); `reduceStep` = `SwapProd`→`Exch`, `xor`→`Xor`,
`SwapProd`→`Exch`; `AssocLProd`/`AssocRProd`→**nothing** (leaf-identity); `succ`→`Xori`. None
emit skip-blocks → `has_inner_control` false → `iter`'s guarded static unroll accepts it.
Compiled via `compile_with_metadata_resident(reduce, resident=2)`. At d=4 the displaced idx is
paged (`Mexch{spare,2}`) around each idx use — sound (f2 §8.1/T-F2-7), inflating the body.

## 4. Energy (M-E) — the reduce park-tax histogram (d=3, predicted)

`iter`'s guarded unroll skips `(d−1−k)` iterations. Per copy: 6-instr skip spine + `body_len=6`
(`memexch·1 + reduceStep·3 + memexch·1 + succ·1`, Assocs free) = 12; d=3 → **B = 2×12 = 24**
(matches the probe's 24 instrs, no displacement). `T(k) = 6(d−1) + body_len·k = 12 + 6k`, so:

| k | T(k) | park_tax=(B−T)/B |
|---|---|---|
| 0 | 12 | **50%** |
| 1 | 18 | 25% |
| 2 | 24 | **0%** |

A 3-point histogram, **tight** (numeral `k_max`). Priced via `rom_program_energy_resident`
(the f2 API) — a third memory-touching M-E datapoint whose per-step net differs from
fib's/f2's (2 `MexchReg` + `Exch/Xor/Exch` per body). *These are predicted; the worker MEASURES
`B` and `T(k)` via `rom_trace` and pins the exact values — if they differ, that IS the finding.*

## 5. Scope

IN: the `xorReduce` program (d=3 primary, d=4 secondary); `run_on_cpu == eval` (fast whole-domain
+ `#[ignore]` real-CPU); the reduce-semantics assertion; the varying-address assertion; the d=3
park-tax histogram; the energy price. OUT (follow-ons):
- **Arithmetic sum** — blocked by single-leaf value **width** (no 1-leaf type holds `1+1=2`, the
  F-1 thermometer fact) → needs **M-D.c** widening, NOT a new memexch-like primitive.
- **Dot-product with a resident key** (`acc ⊕= key ∧ arr[i]`) — a controlled-op variant
  (Toffoli-into-acc). **NOT clean as an f3.5 (F3-5):** a 2-controlled NOT lowers via a
  skip-block, which `iter` rejects inside its body (`has_inner_control`, compile.rs:1931); only
  CNOT/CSWAP have single-instruction recognitions. Needs a **CCNOT recognizer or nested-control
  support** first — not merely "once XOR-reduce lands."

## 6. Reject-don't-corrupt

- Index range: `succ_idx` on a `Bit` stays in `{0,1} < n=2` (F-4). `n>2` needs a thermometer
  index + pack → reject/deferred.
- The f1 home guard (F-2) + semantic-verify (F-3) remain (they run on the shared body
  lowering); both memexches must pass. `Exch`/`MEXCHR` aliasing rejects apply; **`xor` has no
  aliasing reject (F3-2) but `Xor{rd,rd}` is structurally unreachable** (`home_of` injective →
  distinct leaves get distinct regs).
- If the composition trips `has_inner_control` or the home guard → clean reject, never a corrupt
  ROM. (Probe: it doesn't, in both regimes.)

## 7. Acceptance (each finding → a reproducing test — L20/L21)

- **T1 (emission):** `xorReduce` d=3 compiles to a ROM containing exactly `2·(d−1)=4` `MexchReg`
  and `d−1=2` `Xor` (from reduceStep; assert counts).
- **T2 (fast oracle, whole domain):** `run_on_cpu_fast_resident == eval` over the entire d=3
  domain (96 inputs) — includes `idx=1` and `k=2=n` (the full reduce).
- **T3 (real-CPU, `#[ignore]`):** `run_on_cpu_resident (real system_step) == eval` on a sample
  with `k≥1`, `idx=1`, and `acc≠0`, `data≠arr[idx]` (visible, varying, nonzero — no I-2 blind).
- **T4 (reduce semantics + XOR-twice + idx0=1):** d=3, idx0=0: assert `acc_out = acc0 ⊕ (⊕
  arr[touched])` (k=1 → acc0⊕arr0; k=2 → acc0⊕arr0⊕arr1), `data`,`arr`,counter preserved,
  `idx_out=idx0⊕(k mod 2)` — over all 8 `(acc,data,arr)` combos (the load-bearing preservation
  teeth). **d=3, idx0=1, k=1 ⇒ `acc_out = acc0 ⊕ arr1`, `idx_out=0`** (F3-3 — the ONLY regime
  where an idx0-ignoring wrong program is oracle-invisible). **d=4, k=3, idx0=0 ⇒ `acc_out =
  acc0 ⊕ arr1`** (arr0 XOR-ed twice → cancels) — the reduce's g²-analog.
- **T5 (varying address, exact shape — F3-1):** assert the per-copy body slice is EXACTLY
  `[MexchReg{rd,ra}, Exch{a_r,d_r}, Xor{rd:d_r, rs:a_r}, Exch{a_r,d_r}, MexchReg{rd,ra},
  Xori{rd:ra,1}]` where `d_r`=data home=`rd`, `a_r`=acc home. NB the `Xor` writes `rd=d_r`
  (data's reg), NOT acc's — the leading `Exch` swaps acc↔data first, so the accumulate lands
  in acc's home only after the second `Exch`. The two `MexchReg` are byte-identical (shared
  `ra`); `assert!(matches!(instrs[after Xori], Instr::Swapbr{..}))` closes body_len=6;
  `assert_ne!(rd, ra)`. Proves acc⊕=data via borrow-swap + the varying address.
- **T6 (park-tax histogram, d=3):** split like f2 — a **default** `t6a` (via `rom_trace`:
  `wcet==24`, `T(k)==12+6k`, park `{50%,25%,0}` strictly decreasing) + an `#[ignore]` `t6b`
  (joules via `rom_program_energy_resident`; `step_j>0`; `total_j==B·step_j` model identity).
- **T7 (closure, `#[ignore]`):** the reduce ROM's `MexchReg` steps stay bus-quiescent on the
  `SystemRing` (`run==eval^R`) on an `idx=1`,`k≥1`,`acc≠0` state.
- **T-d4 (k>n + displaced idx, d=4 — F3-4):** compile at d=4; assert an idx-displacement
  `Mexch{_,addr≥2}` is present AND pin the measured shape: `instrs.len()==56`, `wcet()==56`,
  `MexchReg` count == 6, `Xor` count == 3 (re-derived + probe-confirmed; if the worker measures
  otherwise that is a finding, not a value to hardcode over). `run_on_cpu_fast_resident == eval`
  over the whole d=4 domain (128 inputs, incl k=3); fails if the occ seed / pop order regresses,
  the paging adds/drops instrs, OR the displaced-idx paging around the memexch pair is wrong
  (the I-1 class + f2 T-F2-7's teeth).
- OOM discipline: real-CPU/closure/energy-pricing tiers `#[ignore]` + serialized.

## 8. Attack surface for the adversarial review (please try to break these)

1. **`data` preservation across the memexch pair with the reduceStep between them:** does
   `reduceStep` (acting on acc,data) leave `data` exactly `= arr[idx]` so the second memexch
   restores `arr[idx]` and `data`? (reduceStep must be `(acc,data)↦(acc⊕data, data)` — the 2nd
   slot preserved. A `(acc⊕data, acc)`-style bug corrupts the restore.) Verify the SwapProd;xor;
   SwapProd direction.
2. **The Assoc regrouping under `iter`:** is `AssocLProd`/`AssocRProd` really a leaf-identity
   (zero instructions) that leaves all homes — especially arr@`[Mem(0),Mem(1)]` and the idx
   register — unchanged? Any input where the regroup perturbs a home → the memexch home guard or
   the address is wrong.
3. **Displaced idx (d=4):** idx is paged around EACH idx-touching op (2 memexch + succ). Is the
   paged idx value stable across the memexch pair (does the reduceStep or a page bracket clobber
   the mem-2 idx word)? Does the page-in/out squat collide with the resident words `Mem(0),Mem(1)`
   or the mem-2 idx home?
4. **XOR-twice cancellation (k>n):** do eval and run_on_cpu agree that arr[0] XOR-ed twice
   cancels (k=3 ⇒ acc⊕arr1)? Is the d=4 oracle domain covering k=3?
5. **acc vs the resident words / counter:** acc is a head register — confirm it never aliases a
   resident mem word or a counter leaf; the XOR writes to acc only.
6. **Park-tax honesty (d=3):** is `T(k)=12+6k` the real measured productive length (is body_len
   actually 6, spine 6), or does the Assoc/displacement change it? If the measured histogram
   differs, surface it (don't hardcode a wrong value — f2's first draft was wrong).
