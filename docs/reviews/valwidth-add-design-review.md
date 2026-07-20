# Adversarial design review — value-width slice-3: Flat(8) Add reduce via `fold`

**Target:** `chessai/theseus-v2` tip `d461e09`. **Reviewer:** fresh-context Fable 5 (did not
write the design). **Method — recomputed + BUILT empirically:** re-derived the fold lowering
(`compile.rs` `lower_fold_unfold` L2352, `spine_info` L1595, `num_leaves` L72, `allocate_aux`
L627), addInto arm (L866) + `check_pattern_2reg` (L1114), `admit()` (prim.rs L125), `fold_type_of`
(typing.rs L93), `list_spine_ty`. Built the change in a throwaway worktree (deleted after; repo
untouched): 10 probes constructing addInto/Flat(8), sumFold at D=2/3/4/8/9, honest + adversarial
Flat(4) addInto, T-foldstruct — all run release AND debug incl. the real `system_step`.

**VERDICT: SHIP the direction — the crux is empirically GREEN — after 5 design-text amendments
(no code-direction change; the slice stays test-only).**

## F1 — fold-over-Flat(8): GO (empirical). VERIFIED, no blocker.
`sumFold = fold(list_spine_ty(Flat(8),3), addInto)` compiles + oracle-agrees. Lowering is genuinely
element-agnostic (`m = num_leaves(elem_ty)`=1 for Flat(8); `spine_info` matches `Sum(One,Prod(b,
tail))`; per-copy body ctx from leaf indices only). Emitted D=3 ROM (20 instrs, wcet 20):
`[Mexch{6,0}, Addi{7,3}, Cswap{0,7,6}, Swapbr{7}, Mexch{7,0}, Add{7,1}, Mexch{7,0}, Swapbr{7},
Cswap{0,7,6}, Subi{7,3}, Addi{7,3}, Cswap{2,7,6}, Swapbr{7}, Mexch{7,0}, Add{7,3}, Mexch{7,0},
Swapbr{7}, Cswap{2,7,6}, Subi{7,3}, Mexch{6,0}]` — exactly 2 Adds (one/cons layer), each in a
tag-guarded skip block. `run_on_cpu_fast == eval == independent modular-sum ref` on all 8 samples
([5,7]/acc0=3→15, 200+100→44, 255+1→0, 128+128→0, 255+[255,255]→253, empty, len-1, zeros); spine
preserved. D=4 compiles (31 instrs, 3 Adds), agrees. Real `system_step` on [200,100]→44: PASS (147s
release, 1 input). Bare addInto compiles to `[Add{rd:1,rs:0}]`. Construction works as written; §3
fallback not needed.

## F2 — argument order: element-first CONFIRMED; T1 pin wrong in detail. MINOR-1.
`fold_type_of`: `f : b×c ↔ b×c`, b=element → body sees `(element, acc)`; `body_homes = [elem…,
acc…]`; `addInto=(a,b)↦(a,a+b)` writes the acc factor (`Add{rd:acc, rs:elem}`) — element preserved,
acc+=elem. §1 + T-sum semantics correct, no F3-1 trap. BUT the T1 pin "`Add{rd:acc_home,…}` per
copy" breaks at D=3: 7 leaves + 2 aux ⇒ `allocate_aux` displaces the acc leaf (6) to `Mem(0)`, paged
through **r7** ⇒ Adds are `{rd:7,rs:1}`/`{rd:7,rs:3}` (rd = paging reg, not acc's home r6), each
bracketed by `Mexch{7,0}`. At D=2 (5 leaves) no displacement, `Add{rd:4,rs:1}` (rd IS acc home). Fix:
T1 pins the FULL ROM per D (slice-2 MAJOR-1 style) or `rs ∈ elem homes` + Add count + the Mexch pair.

## F3 — "Add is Flat(8)-only": true for honest constructions, "unconstructible" FALSE adversarially. MAJOR-1.
Honest-minimal Flat(4) addInto (forward over 256 in-range pairs, outputs `thermo(a+b)` up to 30 —
structurally buildable) is **rejected by `admit()` as `AdmitError::BackwardNotTotal`** (codomain
`(a,c)`, c<a has no in-range preimage) — NOT via the `value_from_leaf_bytes` `<2^n` assert (which
only fires if you build the table via the codec). **Adversarial loophole (empirically confirmed):**
a PADDED Flat(4) addInto (full mod-256 orbit as extra out-of-type keys) PASSES `admit()` AND compiles
to `[Add{rd:1,rs:0}]` — `admit()` checks totality over `enumerate_values` + mutual inverse but NEVER
that keys/values inhabit the declared types; `check_pattern_2reg` samples only in-range. Mislowers
silently until the `unmarshal` `<2^n` assert panics at runtime. Correct claim: *no type-honest
Flat(w<8) addInto exists; honest tables → `BackwardNotTotal`; a type-lying padded table slips through
admission (pre-existing prim.rs gap, day-one, not this slice).* Design must state this precisely;
T-neg pins the honest `BackwardNotTotal`; record the gap (cheap fix: `admit()` also checks forward
image ⊆ codomain set).

## F4 — verification story: sound, but a free upgrade left on the table. MAJOR-2.
The 3-tier decomposition is honest + correctly flagged. But "no tractable whole-domain" is
**overstated — T-sum at D=2 IS whole-domain tractable and should be DEFAULT:** 65,792 inputs
(=257×256), hoisted, 1.9s release, fast==eval==ref on all — pushes every one of the 65,536 (elem,acc)
carry/wrap pairs through the real fold ROM. With it, the residual caveat shrinks to "≥2-element
chaining (D≥3) is strided." Edge cases (200+100, 255+1, 128+128, saturation, empty) are right + remain
necessary AT D=3 (the two-Add chain). T-foldstruct is real: `fold(list_spine_ty(Flat(2),4), xor/
Flat(2))` = 340 inputs, 2.4ms — proves multi-copy spine walk over multi-inhabitant Flat leaves (which
D=2 alone doesn't; keep both). Independent reference correct.

## F4b — domain sizes recomputed. VERIFIED.
`|list_spine_ty(Flat(8),D)| = Σ_{k=0}^{D−1}256^k`: D=2→257 (×256=**65,792**); D=3→65,793
(×256≈**16.8M**); D=4≈4.3G. `|list_spine_ty(Flat(2),4)|=85` (×4=**340**). D≥3 needed for ≥2-element
sum; whole-domain at D=3 is MEMORY-infeasible via `enumerate_values` (~16.8M deep values, 100+ GB)
though compute is ~19 min streamed (optional future #[ignore]).

## F5 — leaf ceiling: D=3/4 safe; real ceiling D≤7; design formula off. MINOR-2.
D=3 (7 leaves)/D=4 (9) compile. D=8 (17 leaves) FAILS `TooManyLeaves{needed:17, max:16}` (allocate_aux
free-slot exhaustion = MAX_LEAVES_BRANCHING 12+MEM_WORDS=16), D=9 (19) `max:18`. Practical ceiling
**total ≤16 leaves ⇒ D≤7**. Design's "≈2(D−1)+1" undercounts: `num_leaves(list_spine_ty(Flat(8),D))
= 2D` exactly (innermost nil layer still costs tag+payload), total **2D+1** (D=3→7 not 5). Conclusions
unchanged; fix the arithmetic + state D≤7.

## F6 — else. One MAJOR trap inside.
- **MAJOR-3: hoist-compile-once must govern the WHOLE file, T-bare above all.** Measured: addInto table
  build 3.1s release; ONE sumFold D=3 compile 5.1s (recognize_prim enumerates 65,536 pairs/copy); bare
  addInto compile ≈2.5s. `run_on_cpu_fast` RECOMPILES per input ⇒ whole-domain T-bare via
  `run_on_cpu_fast`/`assert_oracle_fast` = 65,536 × ~2.5s ≈ **45+ HOURS** + [[maestro-gate-no-timeout]]
  hang. Hoisted (compile once → per-input marshal/run_rom_fast/unmarshal vs eval) = seconds (67µs/
  input). Design mandates hoisting only under T-sum; T-bare is where it detonates.
- Timing envelope: debug≈release (alloc-bound 1.2–1.3×), `timeout 1500` debug comfortable IF hoisted
  (~2–4 min whole file). Build ONE def per test, MOVE into Iso.
- T-real: one D=3 input on real system_step = 147s release; "a few lists" ≈10 min release; keep
  #[ignore], run `--release`.
- Test-only CONFIRMED (all probes on unmodified d461e09, no src change). Scope `theseus-tisc/**`
  adequate; no theseus-core needed. Independent reference genuinely independent.
- NIT-1: list-spine value ctor innermost-first (`nil=Left(Unit)`, `cons h t=Right(Pair(h,t))`) — shared
  helper + round-trip assert (a short-than-intended spine silently weakens coverage). NIT-2: fold fuel
  1M ample.

## SHIP / DO-NOT-SHIP-UNTIL
**SHIP:** construction (fold+addInto, element-first, no borrow-swap), Flat(8)-only vehicle, tier
structure, edge cases, independent reference, test-only shape, D=3/4, check_commands.
**DO-NOT-SHIP-UNTIL (design-doc amendments; no code-direction change):**
1. (MAJOR-1) Reword "unconstructible" → honest Flat(w<8) addInto rejected by `admit()` as
   `BackwardNotTotal` (pin in T-neg); document the padded-table admission loophole (pre-existing).
2. (MAJOR-3) Hoist-compile-once in EVERY oracle, T-bare above all (per-input recompile=45h+gate hang);
   put the measured costs (5.1s/compile, 67µs/input) in the spec.
3. (MAJOR-2) Add T-sum-D2 whole-domain DEFAULT (65,792, ~2s, all carry/wrap pairs); rescope the
   honest-limitation to "≥2-element chaining is strided."
4. (MINOR-1) T1 pin = full-ROM equality per D (D=3 acc is Mem(0)-displaced, paged r7 —
   `Add{rd:7,rs:1}`/`{rd:7,rs:3}`, not rd:acc_home).
5. (MINOR-2) Leaf count `2D+1`; ceiling D≤7 (16 leaves via allocate_aux); T-real runs `--release`
   (147s/input).
