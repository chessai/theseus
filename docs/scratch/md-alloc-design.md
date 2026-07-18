# M-D.alloc design — sound `allocate_aux` under sub-contexts (occupancy threading)

Status: **DESIGN — pre-review.** Canonical branch `chessai/theseus-v2`, tip `cf0639a`
(M-D.a merged). Fixes a real latent correctness bug + unblocks nested recursion.

## The bug (confirmed by adversarial review, reproduced empirically)
`allocate_aux(ctx, ...)` (compile.rs:395-504) computes `free_regs` as the registers NOT
holding a leaf **of `ctx.homes`** (compile.rs:400-410), and ASSUMES a free register is 0
("marshal zeroes everything", compile.rs:382). That assumption holds ONLY when `ctx`
spans the whole machine. But:
- `Ctx::split` (compile.rs:221) slices `homes` — a sub-context (e.g. the `f`/`g` halves
  of `Plus`/`Times`, or the per-copy `[element_j, acc]` body_ctx in `lower_fold_unfold`,
  compile.rs:1478) lists only SOME leaves.
- A register holding a leaf OUTSIDE the slice (a sibling under `Times`, an outer spine
  tag) is not in `used_regs` → treated as free → used as `rb`/`temp` aux → the skip-block
  precondition `rb/temp == 0` is violated and the SWAPBR skip distance is wrong →
  **silent ROM corruption**.

Reproduced: `Times(Id(Bit), Unfold(spine(Bit,4), swp))` destroys the sibling bit;
`Times(Id(Bit), Plus(Id, SwapSum))` (pure M-B, untouched code) miscompiles identically
(`⟨1, inl 0⟩ → ⟨0, inl 0⟩`). M-D.a currently *rejects* the fold cases (reject-don't-
corrupt guards); the M-B `Plus`/`iter`-under-`Times` cases still **silently miscompile**.

## The fix — an OccSet bitmask on `Ctx` (Fable review: NOT an Rc leaf-map)
The review's kill-shot: a leaf→location *map* structurally cannot see the registers that
actually break nested skip-blocks (outer `rb`, `temp` holding `body_len`, the `ensure_reg`
spare) — they are NOT leaf homes. So occupancy must be a **machine-location bitmask**, and
skip-block emitters must reserve their scratch registers in it before lowering the body.

**Representation** (a `Copy` bitmask — no `Rc`, no local→global index mapping, no aliasing):
```rust
#[derive(Clone, Copy)]
struct OccSet { regs: u8, scratch: u8, mem: u32 }   // one bit per machine location
struct Ctx { homes: Vec<Location>, occ: OccSet }
```
- `allocate_aux`/`ensure_reg` compute free/zero locations from `ctx.occ` (whole machine),
  NOT `ctx.homes`. A location is a valid zero-source iff its `occ` bit is clear.
- `Ctx::new(n)`: `homes` = canonical n locations; `occ` = bits for exactly those n.
- `split`/`merge`: slice/concat `homes`; **`occ` is copied unchanged** (slicing the view
  does not change what is live). `merge` debug-asserts both halves share the same `occ`.
- per-copy `body_ctx` / `ctx_f`/`ctx_g` / the idiom target slice: take `occ` from the
  **post-displacement `dctx`** (NOT the parent — the review's R1a: after `allocate_aux`
  pages a leaf to scratch, that scratch bit is now SET; a nested alloc must see it).
- displacement (`allocate_aux` paging a reg-home leaf to scratch/mem): flip two bits
  (clear the reg, set the slot) in the returned Ctx's `occ`; page-out returns the caller's
  original Ctx (original `occ`) — no in-place mutation, no restore bookkeeping.
- **`ensure_reg` has a DIFFERENT predicate than `allocate_aux`** (review R2):
  `allocate_aux` needs a *zero* register (occ bit clear); `ensure_reg` needs a
  *body-untouched* register (may be a leaf home, so long as the body does not address it),
  and brackets it with a self-inverse Xchg. Keep `ensure_reg`'s current spare selection —
  do NOT make it free-only (that regresses 14-leaf iter spill). It only needs to additionally
  avoid the reserved skip-block regs (rb/temp), which it already excludes via `exclude_regs`.
- `emit_skip_block` returns `Result` and guards `body_instrs.len() > 255` for ALL callers
  (iter/plus/idiom/fold), not just fold (nesting grows bodies — review R4-ii).

## Split into two tasks (review recommendation)
- **M-D.alloc1 (THIS task): occupancy-aware allocation — fix the real M-B corruption.**
  Add `OccSet`, thread it through `Ctx`/`allocate_aux`/`ensure_reg`/all construction
  sites, taking `occ` from `dctx` on slices. This alone fixes `Times(Id,Plus/iter)` (the
  reproduced latent bug). **KEEP M-D.a's reject guards unchanged** (do NOT relax them yet).
  Payoff test: `Times(Id(Bit), Plus(Id, SwapSum))` now compiles → `run_on_cpu == eval`
  whole-domain (it silently miscompiled before).
- **M-D.alloc2 (NEXT task): reserve skip-block regs + relax guards.** Each skip-block
  emitter sets its `rb`/`temp`/spare bits in `occ` before lowering its body, so a nested
  `allocate_aux` avoids them. THEN relax M-D.a's Guard 1 (non-canonical ctx) + Guard 2
  (body-Swapbr) → nested recursion / fold-under-Times compile correctly. Mandatory new
  test: **nested skip-block with BOTH tags set** (`Fold(spine(Bit,2), Times(Id(Bit),
  Unfold(spine(Bit,2), swp)))`, both layers active) whole-domain — the exact case that
  corrupts without reserved-reg threading.

## Risk / obligations (carried into the per-task reviews)
- **R1 — `occ` from `dctx` not parent** on every slice (the review's R1a; getting this
  wrong re-introduces corruption under displacement).
- **R2 — enumerate ALL Ctx sites** (grep-confirmed: `Ctx::new` 204/1166; `split` used
  1214/1351; `merge` 1218; allocate_aux return 500; idiom target 985; fold body_ctx 1478;
  plus ctx_f/ctx_g 1560/1563; test literal 1827). Each must set `occ` correctly.
- **R3 — golden-ROM diff the whole suite pre/post** (review R3): any ROM that changes gets
  whole-domain oracle re-verification. "Bit-identical" is an unverified promise otherwise.
- **R4 — do NOT relax guards in alloc1** (leaf-occ alone is insufficient — that is alloc2
  after reserved-reg threading).
- **Invariant:** `lower()` returns with canonical layout restored (page-out bracketing) —
  promote to a stated, debug-asserted invariant (the shared-`occ`-snapshot safety depends
  on it).

## Acceptance (M-D.alloc1)
- `Times(Id(Bit), Plus(Id, SwapSum(One,One)))` compiles → `run_on_cpu == eval` whole-domain
  (was silently corrupting; L20 regression test).
- M-D.a reject guards UNCHANGED; 0019/0020 + iter/fib/spill/branch all still oracle-agree;
  golden-ROM diff surfaces only intended changes, each oracle-verified.
- Full compile suite + a bounded real-CPU sample green.
