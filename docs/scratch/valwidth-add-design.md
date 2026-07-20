# Value-width slice-3 — Flat(8) **Add** reduce (real numeric sum, via `fold`)

> **Revision r2 — Fable design review** (`docs/reviews/valwidth-add-design-review.md`) BUILT the
> change in a throwaway worktree: **the crux is GREEN** — `sumFold = fold(list_spine_ty(Flat(8),3),
> addInto)` compiles + `run_on_cpu==eval` (carries `200+100→44`, `255+1→0`, `128+128→0` all pass;
> real `system_step` passes). Verified D=3 ROM (20 instrs, 2 Adds, each in a tag-guarded skip
> block, acc paged through r7): `[Mexch{6,0}, Addi{7,3}, Cswap{0,7,6}, Swapbr{7}, Mexch{7,0},
> Add{7,1}, Mexch{7,0}, Swapbr{7}, Cswap{0,7,6}, Subi{7,3}, Addi{7,3}, Cswap{2,7,6}, Swapbr{7},
> Mexch{7,0}, Add{7,3}, Mexch{7,0}, Swapbr{7}, Cswap{2,7,6}, Subi{7,3}, Mexch{6,0}]`. Test-only, no
> src change. 5 amendments folded (see the spec `theseus-valwidth-add.json`):
> - **(MAJOR-3) HOIST-COMPILE-ONCE in EVERY oracle, T-bare above all** — `run_on_cpu_fast`
>   recompiles per input; a whole-domain T-bare via `assert_oracle_fast` = 65,536 × ~2.5s/compile ≈
>   **45 HOURS** + [[maestro-gate-no-timeout]] hang. Hoisted = seconds (67µs/input). Build ONE prim
>   def per test, MOVE into the Iso.
> - **(MAJOR-2) T-sum-D2 is whole-domain tractable → make it DEFAULT** — 65,792 inputs (257×256),
>   ~2s, pushes EVERY (elem,acc) carry/wrap pair through the real fold ROM. Rescope the "no
>   whole-domain" limitation to "≥2-element chaining (D≥3) is strided."
> - **(MAJOR-1) "unconstructible" is imprecise** — an honest Flat(w<8) addInto is rejected by
>   `admit()` as `BackwardNotTotal` (T-neg pins THAT); a type-LYING padded table slips through
>   admission + compiles to `[Add]` (a pre-existing prim.rs admission gap — document, don't rely on
>   the `<2^n` assert; optional follow-up: `admit()` checks forward image ⊆ codomain).
> - **(MINOR-1) T1 pin = full-ROM equality per D** — at D=3 the acc leaf is displaced to `Mem(0)`
>   and paged through r7, so the Adds are `{rd:7,rs:1}`/`{rd:7,rs:3}`, NOT `rd:acc_home`.
> - **(MINOR-2) leaf count is `2D+1`; fold ceiling D≤7** (16 leaves via `allocate_aux`; D=8 fails);
>   T-real runs `--release` (147s/input on the real system_step).


**Milestone.** The first *arithmetic* over the value-width foundation — a real numeric
sum `acc += arr[i]` over full **8-bit words**, `acc_out = (acc0 + Σ elems) mod 256`.
Where slice-2 was byte-wide **XOR** (bitwise, canonical at every width), this is byte-wide
**addition** — carries and all — proving value-width unlocks arithmetic, not just
GF(2) ops.

## 0. Two constraints force `fold` (not `iter+memexch`) and Flat(8)-only

- **`Add` is canonical ONLY at Flat(8).** `Instr::Add` is mod-256. At `Flat(w<8)` a leaf
  is `0..2^w`, and `a+b mod 256` can exceed `2^w-1` → the result is not a valid `Flat(w)`
  value → the `addInto` `PrimDef` is **unconstructible** (its forward map has out-of-domain
  outputs; `value_from_leaf_bytes`' `<2^n` assert / prim admission rejects it; recon §4/§6).
  So — unlike XOR — **there is no verify-at-small for Add.** It lives at Flat(8) or nowhere.
- **`iter+memexch` walls at Flat(8).** The memexch forward table is `|carrier| =
  256·2·256² = 33.5M` (why slice-2 capped at Flat(4)). Add needs Flat(8); memexch forbids
  it. So the reduce vehicle **cannot** be `iter+memexch`.
- **`fold` avoids both.** M-D.a `fold` (guarded static unroll of a list spine,
  `lower_fold_unfold`, compile.rs:2352) walks the `Nil|Cons(elem×tail)` structure; the
  **only** prim table is the 2-operand `addInto` = `256² = 65536` entries — feasible (the
  slice-1 bare-Xor Flat(8) precedent). Recon §2: the fold lowering is **element-type
  agnostic** — `let m = num_leaves(&elem_ty)` treats the element opaquely by leaf count, so
  a `Flat(8)` element (1 leaf) flows through exactly like `Bit`.

## 1. The construction

```
sumFold : Flat8List * Flat(8)  ↔  Flat8List * Flat(8)  =  fold (list_spine_ty(Flat(8), D)) addInto
addInto : Flat(8) * Flat(8)    ↔  Flat(8) * Flat(8)     =  (a, b) ↦ (a, a + b)     [mod 256]
```
- `fold` threads the `Flat(8)` accumulator head-to-tail; its body is `(element, acc)` with
  the **element first / preserved** (recon §3; the parity precedent `parity = fold BitList
  cnot`). So `addInto (elem, acc) = (elem, elem + acc)` — element preserved, `acc += elem`.
  **No borrow-swap needed** (fold already presents `(element, acc)`), unlike the
  memexch reduce where the swap forced the SwapProd dance.
- `addInto` over `Flat(8)×Flat(8)` recognizes via `check_pattern_2reg(|a,b| (a,
  a.wrapping_add(b)))` (compile.rs:866, `wrapping_add` = mod-256) → `Instr::Add{rd:acc_home,
  rs:elem_home}`. Its forward table is 65536 entries.
- `list_spine_ty(Flat(8), D)` = lists of length ≤ D−1 (recon §1: D=2→len≤1, D=3→len≤2,
  D=4→len≤3). For a sum of ≥2 elements use **D≥3**. Leaf count ≈ `2(D−1)` element+tag leaves
  + 1 acc — fits `MAX_LEAVES` for small D (recon §4; D=4 Bit already compiles — `parity`).

**Value semantics:** folding `[e0, e1, …]` from `acc0` gives `acc0 + e0 + e1 + … mod 256`,
spine (`elem`s) preserved. This is a genuine multi-word numeric sum with carries.

## 2. Verification — the honest story (this is the weakest-coverage slice; flag it)

`Add`-is-Flat(8)-only + `|list_spine_ty(Flat(8),D)| × 256` (D=3 ⇒ ≈16.8M) mean the composed
reduce has **no tractable whole-domain and no verify-at-small**. Confidence is built from
three tiers that each ARE fully/strongly verified, plus an independent reference:

- **T-bare (whole-domain, DEFAULT):** the bare `addInto` over `Flat(8)` — `256² = 65536`
  inputs, `run_on_cpu_fast == eval`, compiles to exactly `[Add{rd:1,rs:0}]`. This
  **whole-domain-verifies the Flat(8) Add operation itself** (the slice-1 bare-Xor analog).
- **T-foldstruct (whole-domain, DEFAULT):** a `fold` over a `Flat(2)`-element list with a
  *canonical* body (the slice-1 byte-`Xor` prim, bitwise ⇒ canonical at Flat(2)) —
  `list_spine_ty(Flat(2),4) × Flat(2)` ≈ small, whole-domain `run_on_cpu_fast == eval`. This
  **whole-domain-verifies the fold-over-`Flat`-elements spine walk**, decoupled from Add's
  Flat(8)-only constraint. (Proves the fold machinery handles multi-inhabitant `Flat`
  element leaves, not just `Bit`.)
- **T-sum (strided Flat(8), DEFAULT):** the real `sumFold` at D=3/D=4 — `run_on_cpu_fast ==
  eval` on a STRIDED + edge-case sample, AND against an **independent modular-sum reference**
  `acc_out == (acc0 + Σ elems) mod 256` (recomputed in-test — NOT eval-vs-itself). Edge cases
  MANDATORY: identity (empty list / acc0=0), carry/overflow (`200+100=44`, `255+1=0`,
  `128+128=0`), max saturation, several explicit lists. Log the stride (L19 no silent cap).
- **T-real (strided Flat(8), `#[ignore]`, L19):** `run_on_cpu == eval` on the real
  `system_step` for a few nonzero-word lists incl. a carry case.
- **Honest limitation (state in the milestone doc + the review):** the *composed* reduce's
  L19 authority is **strided**, not whole-domain — because Add is Flat(8)-only (no
  verify-at-small) and the Flat(8) list domain is intractable. The composition confidence
  rests on: whole-domain-verified Add (T-bare) ∘ whole-domain-verified fold-over-Flat
  (T-foldstruct) + strided composition against an independent arithmetic reference (T-sum).

## 3. Probe (worker's first step / advisor pre-check)

1. Build `addInto` over `Flat(8)` (65536 forward map) + confirm `admit()` succeeds and it
   compiles to `[Add]` (T-bare go/no-go for the Add operation).
2. Build `sumFold = fold(list_spine_ty(Flat(8), 3), addInto)` and **compile it** — the crux:
   does the fold lowering compose over `Flat(8)` element leaves? Confirm the per-copy body is
   `[Add{rd:acc, rs:elem}]` (+ spine machinery) and `run_on_cpu_fast == eval` on a sample
   list `[5, 7]` from `acc0=3` → `15`.
If the fold rejects Flat(8) elements (a leaf-ceiling or a bit-specific guard the recon
missed), or `addInto` won't admit, STOP and report — the design changes (fallback: a
straight-line 2-add `(acc,e0,e1)↦(acc+e0+e1,e0,e1)` if `fold` itself is the blocker, though
recon §2 says it's agnostic).

## 4. Test plan (L20/L21) — `tests/valwidth_add.rs` (new)

- **T1 (emission):** `sumFold` at D=3 compiles; the fold body recognizes `addInto` →
  exactly one `Instr::Add{rd:acc_home, rs:elem_home}` per unrolled copy; assert the count.
- **T-bare:** bare `addInto` Flat(8), whole 65536 domain, `run_on_cpu_fast==eval`, compiles
  to `[Add{rd:1,rs:0}]`.
- **T-foldstruct:** fold over `Flat(2)` list with the byte-`Xor` body, whole-domain
  `run_on_cpu_fast==eval` (proves fold-over-`Flat` spine walk).
- **T-sum:** `sumFold` Flat(8) D=3 (and D=4), strided + mandatory edge cases, vs the
  independent `(acc0 + Σ) mod 256` reference. HOIST-COMPILE-ONCE (build iso+rom once,
  per-input `marshal`/`run_rom_fast`/`unmarshal` vs hoisted `eval` — the slice-2 MAJOR-2
  lesson; whole-domain-recompile would be catastrophic here too).
- **T-real (`#[ignore]`):** real `system_step==eval` strided, incl. a carry list.
- **T-neg:** confirm a `Flat(4)` `addInto` PrimDef is REJECTED (unconstructible /
  admission fails) — pins the "Add is Flat(8)-only" fact with a reproducing test.
- (Carry any residual slice-2 recommendations if they touch shared code — none expected;
  this slice adds no core/eval edits.)

Whole-suite around merge (`cargo test -p theseus-tisc` scoped to the new + regression
files + the fold tests `--test compile`). NOTE: 4 pre-existing `theseus-spice` reds.

## 5. Delegation shape

One maestro task (tier 0, theseus profile): `tests/valwidth_add.rs` (+ any tiny helper).
**Likely test/program-ONLY** — no `src` change expected (fold + Flat + addInto recognition
all exist; recon found no gap). If the fold lowering DOES need a Flat tweak, that's a
`compile.rs` change to review carefully. `file_allowlist`: `crates/theseus-tisc/**`
(+ `crates/theseus-core/**` only if a Flat helper is needed). `check_commands`: `cargo
clippy` + `timeout 1500 cargo test -p theseus-tisc --test valwidth_add --test compile
--test valwidth_reduce --test valwidth` (compile.rs guards the fold regression; the others
guard slices 1–2). Do NOT touch the decoder, recognizer semantics, or `Value`.

## 6. Risks / adversarial-review targets

1. **fold-over-Flat(8) feasibility (THE crux)** — the recon says the fold lowering is
   element-agnostic (`num_leaves(elem)`), but it's intricate machinery (guarded unroll,
   per-copy displaced Ctx, spine tags). The probe/review must EMPIRICALLY compile a
   `Flat(8)` fold and confirm `run_on_cpu==eval` — don't take the recon on faith.
2. **The weakened verification** — is strided-Flat(8) + the three-tier decomposition +
   independent reference genuinely adequate L19 authority, or a real gap the design should
   call out harder? The review should stress the edge-case coverage (carries/overflow are
   where Add differs from XOR — they MUST be in the sample).
3. **`addInto` argument order in fold** — is the fold body really `(element, acc)` (so
   `addInto = (a,b)↦(a,a+b)` accumulates into the SECOND factor = acc)? Or is it
   `(acc, element)`? A swapped order changes which register `Add` writes; the T1 emission
   pin + T-sum semantics must agree. (Recon §3 says element-first; verify.)
4. **The independent reference is truly independent** — `(acc0 + Σ) mod 256` computed from
   the input list bytes in-test, not from eval/compiler output.
5. **Flat(8) table cost** — the `addInto` 65536 table + the fold unroll; confirm compile +
   T-bare stay within the slice-1 ~4GB envelope; move (not clone) the PrimDef.
