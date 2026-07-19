# Adversarial implementation review — value-width slice-2: byte-wide XOR reduce (`acc ^= arr[i]` over `Flat(w)`)

**Diff:** worktree `01KXY84K4154HZ7YNSKHASK3A5` vs `chessai/theseus-v2` @ `2826c81` — 6 files, +643/−8.
**Authoritative:** `docs/scratch/valwidth-reduce-design.md` (r2) + design review. **Reviewer:**
fresh-context Fable 5; did not write the change. Static analysis; every claim recomputed against
the worktree source (recognizer arm 1043–1105, `memexch_prim_idx` 1244–1338, `marshal_resident`/
`run_rom_fast`/`run_rom_on_cpu`, `StateVals`, f3's `xor_reduce.rs`), not trusted from test names.
**Advisor note:** the 2 MAJORs below were fixed in the worktree (T1 cross-w pin; T6 re-aimed at
regs/scratch/**mem[0..2]**) and re-verified before merge.

**VERDICT: FIX-BEFORE-MERGE — 2 MAJORs, both test-teeth gaps in `tests/valwidth_reduce.rs`; the
production change `memexch_prim_idx` SHIPS as-is.**

## F1 — `memexch_prim_idx` correctness: SHIP
All 4 guards present + correct: elem single-leaf (1247), **the MAJOR-1 idx single-leaf guard IS
present** (1263–1270), `n <= MEM_WORDS` (1272), `idx_count <= n` (1278–1288). Delegation faithful
(`memexch_prim(elem,n) = memexch_prim_idx(elem,elem,n)`; body verbatim w/ idx_ty param; f1/f2/f3
callers unaffected). Negative test present (`memexch_prim_idx(&Bit,&Prod(Bit,Bit),4)` → Err; passes
every other guard so `is_err()` fires only via the new guard — real teeth). Carrier for
`(Flat(w),Bit,2)` = 4 leaves `[data,addr,arr0,arr1]`, table in-bounds, recognizer F-3/F-2 accept
(matches design-review F1). Error-string rename safe (no test pins old strings). **NIT-A:** neg test
asserts only `is_err()`, not the message; match `"idx_ty must have exactly 1 leaf"`. **NIT-B:**
design-review NIT-1 (`// always == a` comment) not addressed — non-blocking, carry.

## F2 — T1 parametric pin: MAJOR-1 (weakened — cross-w equality missing) → FIXED
T1's per-w slice pin is strong + correct (4 MexchReg + 2 Xor; the 6-instr body with F3-1 `Xor.rd`=
data-home, a_r≠ra, a_r≠d_r, recomputed correct). But it did NOT `assert_eq!` the full
`lowered.instrs` across w∈{1,2,4} (the design/review-F3 mandate), and B=24 was pinned only at w=2.
A width-dependent extra instr in the ~12 spine instrs, or a longer w=4 ROM, would pass. **Fix
applied:** collect `lowered.instrs` per w, `assert_eq!` roms[0]==roms[1]==roms[2], and
`assert_eq!(lowered.wcet(),24)` per w (home_of/emission are width-free — safe).

## F3 — byte-wide teeth T2/T3: T2 SHIP; T3 MINOR
T2 (Flat(2), whole 1536, asserted `==1536`): full teeth + correctly HOIST-COMPILED (iso+rom once,
per-input marshal_resident→run_rom_fast→unmarshal vs hoisted eval — MAJOR-2 honored, hours-trap
dead). Full-word check via `value_to_leaf_bytes_pub` raw thermometer bytes, `expected_acc` recomputed
in-test (independent, not tautology); pins data/arr/counter preservation + `idx_out=idx0⊕(k mod 2)`.
T3 (Flat(4), whole 393,216, unstrided, hoisted, dense words present) — but its semantic full-word
`expected_acc` check exists only at Flat(2); **MINOR-1 (non-blocking):** add it to T3's 3 dense inputs.

## F4 — T5 histogram / T6 canonicalization: T5 SHIP; T6 MAJOR-2 (mis-aimed) → FIXED
T5: `wcet==24`, `T(k)==12+6k`, park `{0.5,0.25,0}` — byte-for-byte f3's t6a. ✓
T6 default (`t6_canonicalization_flat2`): the single-stepping is valid (`run_rom_fast(rom,state,1)`,
`StateVals` carries pc/br, 24×budget-1 ≡ 1×budget-24). **But it checked the wrong bytes:**
`scratch[0..2]` (claimed to be the resident words) is VACUOUS — `marshal_resident` writes resident
leaves to `state.mem[0..2]`, not scratch. So `state.mem` — the memory-resident Flat(w) words
`MexchReg` swaps byte-wide, the new surface of this slice — was NEVER checked; regs[5..8] skipped.
A MexchReg transiently parking `d|0x80` in mem and cancelling by step B passed T6 AND T2. **Fix
applied:** assert every `regs`, every `scratch`, AND `mem[0..2]` `<= mask` each step. **MINOR-2:**
`t6_canonicalization_real_cpu` (#[ignore]) is a placeholder duplicating T4 (no per-step content) —
give it a per-step body or an honest name (non-blocking).

## F5 — MINOR-1 / MINOR-3 folded: SHIP
MINOR-1: `debug_assert!(*n <= 8)` at all 4 sites (core enumerate.rs, core prim.rs enumerate_values,
eval enumerate.rs, eval interp.rs). MINOR-3: both `#[should_panic(expected="out of range")]` tests
match the real messages + fire in release (plain assert). Advisor gate must include
`cargo test -p theseus-core -p theseus-eval --lib` (F6 gate gap — the `--test`-only run skips lib
tests where MINOR-1 lives).

## F6 — forbidden edits / eval reference: SHIP
Recognizer arm byte-identical (diff only in memexch_prim region 1238–1288); check_pattern_*, decoder
(no step.rs/netlist/asm in diff), Value untouched (core diff = one debug_assert). eval reference
independent where it must be (T2 recomputes expected_acc from input bytes). reduceStep recomputed:
`(acc,data)→(data,acc)→(data,data⊕acc)→(acc⊕data,data)` — borrow-swap over full words ✓.

## F7 — else
T4 is the real system_step CPU path w/ L19 samples (k≥1, idx0=1, nonzero). Helpers correct at w>1
(flat_val=Right^byte(Left), body_ty leaf bytes `[acc,data,idx_tag,arr0,arr1]`). No scope creep (6
files, allowlist). **NIT-D:** the optional energy `#[ignore]` tier skipped (design allowed "if
cheap") — note in the milestone doc.

## SHIP / FIX-BEFORE-MERGE
**FIX-BEFORE-MERGE (both in tests/valwidth_reduce.rs, ~10 lines — DONE by advisor):**
1. MAJOR-1 (T1): cross-w `assert_eq!` of full `lowered.instrs` across w∈{1,2,4} + `wcet()==24` per w.
2. MAJOR-2 (T6): re-aim per-step canonicalization at all `regs`, all `scratch`, AND `mem[0..2]`.
**RECOMMENDED (non-blocking):** T3 dense-word semantic assert (MINOR-1); t6_real_cpu per-step body
or honest name (MINOR-2); advisor confirm gate `--lib` (F6); neg-test message match (NIT-A); NIT-B.
**Everything else SHIPS:** memexch_prim_idx (4 guards incl MAJOR-1 idx-leaf + working neg test),
faithful delegation, recognizer/decoder/Value untouched; T2 whole-domain full-word independent
hoist-compiled oracle (Flat(4) hours-trap dead); T3 393,216 unstrided; T5 exact f3 pins; T4 real CPU;
MINOR-1/MINOR-3 folded.
