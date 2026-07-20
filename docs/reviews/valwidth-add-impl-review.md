# Adversarial implementation review — value-width slice-3: Flat(8) Add reduce (`tests/valwidth_add.rs`)

**Target:** worktree `01KXYH90E2YMA3E3T4SWGP4NZN`, diff vs `chessai/theseus-v2` = one file,
`crates/theseus-tisc/tests/valwidth_add.rs` (+599, test-only — no src touched).
**Reviewer:** fresh-context Fable 5 (did not write the change). **Method:** full read of the diff;
verified helper semantics against worktree source (compile.rs marshal/unmarshal/`value_to_leaf_bytes`
/`run_rom_fast`/`run_on_cpu_resident`/`run_rom_on_cpu`; prim.rs `admit`/`enumerate_values`);
authoritative docs; empirical run: `cargo test -p theseus-tisc --test valwidth_add` → **6 passed, 1
ignored (T-real), 34.8s debug** — inside the `timeout 1500` gate envelope.

**VERDICT: one MAJOR (D=4 is never EXECUTED — a design-mandated tier is missing), one MINOR, two
NITs. Everything else — hoisting, whole-domain claims, reference independence, T-neg, spine
construction — checks out. FIX-BEFORE-MERGE: the MAJOR.** *(Advisor: MAJOR + both MINORs fixed in
the worktree — a hoisted T-sum-D4 tier with 3-element edges + coarse stride, D=4 len/wcet pin, len-0
acc sweep — and re-verified before merge.)*

## F1 — Hoist-compile-once (design MAJOR-3): PASS (static + empirical)
Every default oracle (T-bare, T-foldstruct, T-sum-D2, T-sum-D3) calls `compile_with_metadata` +
`assemble` + `wcet()` ONCE outside the per-input loop, then per-input `marshal` → `run_rom_fast` →
`unmarshal`. Grep: ZERO `run_on_cpu_fast`/`assert_oracle_fast`, ZERO `.clone()` on any PrimDef —
every def MOVED into `Iso::Prim(Box::new(def))`. Whole default file = 34.8s debug (the 45-hour
detonation absent).

## F2 — Whole-domain tests: PASS (genuine, count-asserted)
T-bare `assert_eq!(domain.len(), 65_536)`; T-sum-D2 `assert_eq!(65_792)` (=257×256); T-foldstruct
`assert_eq!(340)` (=85×4) — full iteration each, no stride/subset. Count asserts make silent
enumeration regression impossible.

## F3 — Independent reference: PASS (truly independent, three-way)
`reference_sum(elems, acc0)` recomputed in-test from the INPUT: `extract_list_elems` decodes the
spine structurally (counts Rights on the Value), acc0 from input bytes — neither touches eval/compiler
output. Each T-sum input checked `rom_out == eval_out` AND `rom_acc == arithmetic reference` (a shared
fold-walk bug caught by the reference leg). `extract_acc` via `bytes.last()` valid (fixed-length
zero-padded Sum encoding). Only shared atom is `wrapping_add` = the spec.

## F4 — T1 emission pin: PASS at D=3; WEAK at D=4 (MINOR → advisor-fixed)
D=3: `assert_eq!(lowered.instrs, expected_d3_rom)` pins the FULL 20-instr sequence byte-identical to
the design-review ROM (incl displaced-acc `Add{rd:7,rs:1}`/`{rd:7,rs:3}` bracketed by `Mexch{7,0}`) +
len==20 + wcet==20 + Add-count==2. **MINOR:** D=4 pinned only by add_count==3; r2 mandated full-per-D.
Advisor added `instrs.len()==31` + `wcet()==31` (review-attested).

## F5 — T-sum-D3 edges + striding: PASS; one comment lie (NIT→fixed) + a cheap gap (MINOR→fixed)
All mandatory cases present + correct (empty×2, len-1, 200+100→44, 255+1→0, 128+128→0, saturation
255+[255,255]→253 [510 mod 256=254, 509 mod 256=253 ✓], zeros), each vs eval AND reference. Stride
logged (no silent cap); grid step_by(17) → {0,17,…,255} incl the 255 corner; 16³=4096 len-2 samples
drive the 2-Add chain. **NIT:** comment "len-1 and len-0 lists too" false (no len-0 loop). **MINOR:**
a 256-acc len-0 sweep is ~free and closes the inactive-skip acc-corruption corner. Advisor added the
len-0 sweep (comment now truthful).

## F6 — T-neg: PASS (honest construction, correct error + order)
Flat(4) def honest: forward TOTAL over 256 in-range pairs, outputs `(a, thermo(a+b))` up to 30 built
as raw Values (bypassing the codec `<2^n` assert — the only way to build the honest table); backward
defined only for c≥a. `admit()` checks forward totality (passes) then backward totality → deterministic
`AdmitError::BackwardNotTotal`. `matches!(result, Err(BackwardNotTotal(_)))` pins exactly design-F3.

## F7 — Spine construction + else: PASS
`list_spine_val` innermost-first (nil=Left(Unit), cons=Right(Pair(thermo(h),t))), guards
`elems.len()<D`, round-trips every value through the codec (consumed + equality asserts) — short/malformed
spine can't silently weaken coverage (design NIT-1 done). T-real genuine (`run_on_cpu_resident` →
`system_step` netlist stepped B times vs a separate iso eval); `#[ignore]` justified (147s/input,
runnable reason). No vacuous asserts, no hardcoded number papering a discrepancy, no src touched.

## F8 (MAJOR) — D=4 compiled but never EXECUTED → advisor-fixed
Design r2 §2/§4 mandate T-sum at D=3 AND D=4. Impl only COMPILED D=4 + counted Adds (T1). No test —
default or ignored — runs a 3-element list; deepest chain executed is 2 Adds (D=3). The D=4 unroll has
a different layout (9 leaves, third tag-guarded copy at a different Mexch offset) — a bug confined to
copy 3 would pass every test while Add-count stays 3. **Advisor fix:** added hoisted `t_sum_d4_strided`
— mandatory 3-element edges ([200,100,56]→100, [255,255,255] from acc255→252, empty, len-1, len-2,
zeros) + coarse stride 85 over (e1,e2,e3,acc0) (4⁴=256 length-3 samples) vs eval AND reference_sum;
stride logged.

## SHIP / FIX-BEFORE-MERGE
**FIX-BEFORE-MERGE (advisor-done):** (F8 MAJOR) hoisted D=4 execution tier; (F4 MINOR) D=4 len/wcet
pin; (F5 MINOR/NIT) len-0 sweep + comment.
**SHIP AS-IS:** hoist-compile-once (static + 34.8s empirical), all whole-domain count-asserts, the
three-way independent-reference oracle, the full D=3 ROM pin, T-neg's honest BackwardNotTotal, the
round-trip-guarded spine constructor, T-real's genuine system_step path, test-only scope.
