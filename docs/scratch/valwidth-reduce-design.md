# Value-width slice-2 — byte-wide XOR reduce (`acc ^= arr[i]` over `Flat(w)` words)

> **Revision r2 — Fable design review** (`docs/reviews/valwidth-reduce-design-review.md`)
> recomputed the lowering instruction-by-instruction and confirmed the core bets HOLD
> (recognizer generalizes untouched, ROM byte-identical to f3's across w, borrow-swap composes
> over words, idx:Bit sidesteps the trap). Two amendments folded:
> - **MAJOR-1** — `memexch_prim_idx` MUST also guard `num_leaves(idx_ty)? == 1` (§1); else e.g.
>   `memexch_prim_idx(Bit, Prod(Bit,Bit), 4)` passes all other guards but mints a corrupt
>   "memexch" PrimDef eval swallows silently. Ship a negative test.
> - **MAJOR-2** — the whole-domain oracle MUST hoist-and-compile-once (§3): build the iso ONCE,
>   compile ONCE, then per input `marshal_resident` + `run_rom_fast(rom, state, budget)` (public)
>   + `unmarshal_resident`, vs a hoisted `eval(&iso, …)`. `run_on_cpu_fast_resident` recompiles
>   per call AND f3's `eval_xor_reduce` rebuilds the iso per call → a verbatim port makes Flat(4)
>   (393k inputs) HOURS on a timeout-less gate ([[maestro-gate-no-timeout]]).
> - Also: output decoder must read the FULL thermometer depth (not f3's bit-0 `to_bit`); MINOR-2
>   canonicalization = fast-path `run_rom_fast(rom,state,1)` single-step (default) + t7-style
>   real-CPU (#[ignore]); add `cargo test -p theseus-core -p theseus-eval --lib` to the gate
>   (the `--test` selection excludes lib tests, so the MINOR-1 core/eval edits would be ungated).


**Milestone.** The first *program* over the `Type::Flat(n)` value-width foundation (slice-1,
`30c4415`): f3's XOR/parity reduce lifted from **bit** elements to full **`Flat(w)` word**
elements. `acc ^= arr[i]` where `acc`/`arr[i]` are `w`-bit words in one register each. The
direct byte-wide analog of f3, and the next rung toward the masked accumulate.

Scoped by the slice-1 impl review (F5): keep **`idx : Bit`** (only the *elements* are `Flat`),
which sidesteps the "idx can't advance at Flat width" trap; caps at **Flat(4)** (the Flat(8)
memexch forward table is ~33.5M entries — the 2-operands-forever rule).

## 0. Why this is mostly f3 + `Flat`

f3's reduce (`87c3f7e`, `tests/xor_reduce.rs` / `examples/array_histogram.rs::reduce_iso`) is:
```
reduce = iter Nat_d [ (id⊗memexch); assocL; (reduceStep⊗id); assocR; (id⊗memexch); succ_idx ]
reduceStep = SwapProd; xor; SwapProd    (acc,data) ↦ (acc⊕data, data)   [borrow-swap]
```
Slice-2 changes **only the element type** `Bit → Flat(w)` for `acc`, `data`, `arr0`, `arr1`,
keeping `idx : Bit`. Because:
- `MexchReg` swaps a whole **register** (byte) ↔ mem word — works for a `Flat(w)` element (one
  byte) unchanged.
- `Instr::Xor` is **byte-parallel** — the `Flat(w)` byte-Xor prim (slice-1) recognizes to the
  same `Xor{rd,rs}`.
- `succ_idx = SwapSum(One,One)` (bit-flip = +1 mod 2) still advances a 2-element `idx:Bit`.
- Leaf count is **identical to f3** (5 payload + counter — no `key`, unlike f3.5), so it's f3's
  **clean** regime, not f3.5's displaced one.

⇒ **The compiled ROM is byte-identical to f3's** (`MexchReg, Exch, Xor{rd:data,rs:acc}, Exch,
MexchReg, Xori{idx,1}`), so `B=24, T(k)=12+6k, park {50,25,0}%` (d=3) carry over verbatim, and
the **parametric pin** is "compile(reduce@Flat(w)) == f3's ROM shape, for w∈{1,2,4}."

## 1. The one real change: `memexch_prim` gains an explicit `idx_ty`

Today `memexch_prim(elem_ty, n)` (compile.rs:1241) hardcodes `let idx_ty = elem_ty.clone()`
(line ~59 of the fn) + the guard `idx_count ≤ n`. For `Flat(w)` elements that fails
(`memexch_prim(Flat(2),2)` → `idx_count(Flat(2))=4 > n=2` Err). Fix — a **new sibling**
(keeps all f1/f2/f3 callers working):
```rust
pub fn memexch_prim(elem_ty, n)            = memexch_prim_idx(elem_ty, elem_ty, n)  // unchanged behavior
pub fn memexch_prim_idx(elem_ty, idx_ty, n) -> Result<PrimDef, CompileError>         // NEW
```
`memexch_prim_idx` is today's body with `idx_ty` taken as a param instead of `= elem_ty`. The
guards are UNCHANGED and still hold: `elem_ty` single-leaf (Flat is 1 leaf ✓); `idx_count(idx_ty)
= idx_count(Bit) = 2 ≤ n=2` ✓; `n ≤ MEM_WORDS` ✓. Carrier becomes
`(data:Flat(w)) × (addr:Bit) × Arr[Flat(w); 2]` = 1+1+2 = 4 leaves.

**The recognizer arm is NOT touched** (adversarial-review target): the memexch arm's F-3 semantic
verify reads `addr` generically from `in_bytes[1]` and checks `arr[addr]` byte-for-byte over the
whole table; F-2's home-layout guard requires arr leaves at `[Mem(0), Mem(1)]` in order. With
`addr ∈ {0,1} < n=2` always in-bounds, and arr still 2 trailing single-leaf words, both guards
pass exactly as for f1's `Bit` array. Confirm this in the probe — do NOT change
`check_pattern_*`/the memexch recognizer semantics.

## 2. The program (`tests/valwidth_reduce.rs`, new — mirrors xor_reduce.rs)

`reduce_flat(w, d)` = f3's `reduce_iso` with `bit()` → `Type::Flat(w)` for acc/data/arr0/arr1,
`idx` staying `Bit`; `memexch = Iso::Prim(memexch_prim_idx(&Flat(w), &Bit, 2))`; `xor_w` = the
byte-Xor `PrimDef` over `Flat(w)` (slice-1's builder); `reduceStep = SwapProd; xor_w; SwapProd`;
`succ_idx = SwapSum(One,One)`. Carrier `Nat_d × (acc, data, idx, arr0, arr1)`, resident=2.

## 3. Verification (verify-at-2/4 discipline)

- **Flat(2), d=3 — whole-domain (default):** domain = `Nat_3 × acc·data·arr0·arr1 (4^4) × idx (2)`
  = `3 × 256 × 2 = 1536`. `run_on_cpu_fast == eval` over ALL 1536; assert `acc_out = acc0 ⊕
  (⊕ arr[touched])` over FULL WORDS (not just bit 0 — the byte-wide teeth), `data`/`arr`/counter
  preserved, `idx_out = idx0 ⊕ (k mod 2)`.
- **Flat(4), d=3:** domain = `3 × 16^4 × 2 = 393,216`. Run whole-domain if it stays under a few
  minutes on the fast sim; else STRIDED (log the stride — L19 no silent cap) + a `#[ignore]`
  whole-domain tier. Include dense-word cases (0xA, 0x5, 0xF at w=4).
- **Parametric ROM pin:** `compile(reduce_flat(1,3)) == compile(reduce_flat(2,3)) ==
  compile(reduce_flat(4,3))` (same instruction sequence — width-independent), and each == f3's
  exact 6-instr body slice `[MexchReg, Exch{a,d}, Xor{rd:data,rs:acc}, Exch, MexchReg, Xori{idx,1}]`
  (the F3-1 shape — Xor writes the DATA-home reg).
- **`#[ignore]` real-CPU (L19):** `run_on_cpu == eval` (real `system_step`) at Flat(2) on a
  nonzero-word sample with k≥1, idx0=1 (no all-zeros/oracle-blind — L19 I-2/F3-3).
- **WCET/park-tax:** `wcet == 24`, `T(k) == 12+6k`, park `{0.5,0.25,0}` (default `rom_trace`
  tier — same as f3, now over words). Energy `#[ignore]` (`rom_program_energy_resident`) if cheap.

## 4. Carried from the slice-1 impl review (fold in here)

- **MINOR-2** — the reduce is multi-instruction, so add the raw-register canonicalization check
  the slice-1 T6 couldn't: at `Flat(2)`, assert every register byte stays `< 4` (upper bits 0)
  across the whole run, including non-leaf/scratch regs on the real-CPU path.
- **MINOR-1** — debug-assert `n ≤ 8` in the three `cardinality` copies + `enumerate_values`
  (cheap hardening the slice-1 worker skipped; do it in this task's core touch).
- **MINOR-3** — two `#[should_panic]` tests pinning the `value_to/from_leaf_bytes` `< 2^n` range
  asserts (load-bearing, currently only probe-pinned).

## 5. Probe (worker's first step / advisor pre-check)

1. Add `memexch_prim_idx`; build `memexch_prim_idx(Flat(2), Bit, 2)` — confirm it constructs and
   the recognizer accepts it (F-2/F-3 guards pass; the ROM contains `MexchReg`).
2. Build `reduce_flat(2,3)`, compile — confirm the ROM shape **equals f3's** (6-instr body,
   B=24) and `run_on_cpu_fast == eval` over the 1536 domain.
Go/no-go: if the memexch recognizer rejects the Flat/Bit combo (a guard doesn't generalize) or
the ROM shape differs from f3, STOP and report — the design changes. Low-risk (the slice-1 review
already verified `memexch_prim(Flat(2),4)=Ok` shape-wise; this only swaps in an explicit idx_ty).

## 6. Delegation shape

One maestro task (tier 0, theseus profile): `memexch_prim_idx` (+ the 3 MINOR core/tisc fixes) +
`tests/valwidth_reduce.rs`. `file_allowlist`: `crates/theseus-core/**`, `crates/theseus-eval/**`,
`crates/theseus-tisc/**`. `check_commands`: `cargo clippy` + a scoped `cargo test -p theseus-core
-p theseus-eval -p theseus-tisc --test valwidth_reduce --test valwidth --test xor_reduce` (the
last two guard against regressing slice-1 and f3). Do NOT touch the decoder, the memexch
**recognizer** semantics, or `Value`. NOTE: the tip has 4 pre-existing `theseus-spice` reds
(unrelated).

## 7. Risks / adversarial-review targets

1. **The memexch recognizer generalizing to Flat/Bit** — the F-3 semantic verify + F-2 home
   guard must pass for a `Flat(w)` element array with a `Bit` idx. The probe proves it; the
   review should confirm no guard silently assumes `elem_ty == idx_ty` or bit-valued elements.
2. **Byte-wide reduce teeth** — the oracle must check FULL-WORD equality (a bit-0-only check
   would pass even if upper bits were wrong). Assert `acc_out` as a full `w`-bit XOR.
3. **Parametric pin soundness** — is the ROM genuinely width-independent (same homes, same
   `Xori{idx,1}` immediate)? Confirm no width-dependent codegen sneaks in via the element size.
4. **F3-1 reincarnation** — the `Xor` writes the DATA-home register (leading `Exch` swaps first);
   the exact-shape test must assert the swapped reg, as f3's T5 did.
5. **Flat(4) domain cost** — 393k fast-sim runs; confirm it's minutes not hours, or strided.
