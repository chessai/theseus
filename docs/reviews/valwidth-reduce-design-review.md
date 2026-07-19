# Adversarial design review — value-width slice-2: byte-wide XOR reduce (`acc ^= arr[i]` over `Flat(w)`)

**Design:** `docs/scratch/valwidth-reduce-design.md` · **Tree:** `chessai/theseus-v2` `2826c81` ·
**Reviewer:** fresh-context Fable 5 (did not write the design).

**Method — recomputed, not trusted.** Re-derived every core claim against the code: the memexch
recognizer arm (`compile.rs:1043–1105`), `memexch_prim` (1241–1320), `check_pattern_2reg`/`2leaf`,
`num_leaves`/`value_to_leaf_bytes`/`value_from_leaf_bytes`, `home_of`, the structural lowering arms
(SwapProd 1805, Assoc 1824, SwapSum 1869, Iter 1847), `run_on_cpu_fast_resident`/`run_rom_fast`,
`marshal_resident`, `Type::Flat`, f3's `tests/xor_reduce.rs`, the slice-1 impl review. Homes,
domains, table sizes, and the 6-instr body recomputed by hand.

**Verdict: SHIP AFTER TWO AMENDMENTS.** The central bets all hold under recomputation — the
recognizer genuinely generalizes to Flat-elem/Bit-idx untouched, and the ROM is genuinely
byte-identical to f3's across w. But (MAJOR-1) the new `memexch_prim_idx` API is missing a
`num_leaves(idx_ty)==1` guard and can construct a semantically corrupt "memexch" PrimDef, and
(MAJOR-2) the Flat(4) whole-domain plan is hours-not-minutes if the worker mirrors `xor_reduce.rs`
verbatim (per-input iso rebuild + per-input recompile) against a gate with no timeout. Both are
one-liners.

## F1 — recognizer generalizing to Flat elem + Bit idx: HOLDS (verified)
F-3 semantic verify (1048–1079) reads raw leaf bytes (`d=in_bytes[0]`, `a=in_bytes[1]`, swaps whole
bytes), compares byte-for-byte over the full output type; `value_to_leaf_bytes(Flat(w))` yields the
raw thermometer byte `<2^w`. NO bit-valued-element or `elem_ty==idx_ty` assumption anywhere — the
recognizer sees only `def.input_ty` + the table. Addr in-bounds: `MEM_WORDS=4`, Bit idx marshals to
tag `a∈{0,1}`, so `idx=a<n_arr=2` — the bound never trips (recognizer `%MEM_WORDS` and generator
`%n` agree because `idx_count≤n≤MEM_WORDS`). F-2 home guard: Flat(w)=1 leaf, layout `[data,addr,
arr0,arr1]`, positions 2,3 → Mem0,Mem1 (recomputed). Pre-filter safe (`n_in=4` skips 2/3-reg arms).
Flat(4) table = 8192 entries — trivial. Leave the recognizer untouched (correct); probe is the right
go/no-go.

## F2 — memexch_prim_idx extension: MAJOR-1 (missing idx single-leaf guard)
Delegating-wrapper split is backward-compatible (memexch_prim(elem,n) keeps signature/behavior; body
takes idx_ty as param). But the body hard-assumes addr = 1 byte at offset 1 (`bytes[1]`, arr at
`2+idx`) with no guard. Break: `memexch_prim_idx(Bit, Prod(Bit,Bit), 4)` passes all stated guards
(elem 1 leaf, idx_count=4≤n=4, n≤MEM_WORDS) yet builds a corrupt table (layout `[d,i0,i1,a0..a3]`,
`a=bytes[1]` reads only idx's first leaf; `out[2+idx]` for idx=0 overwrites idx's second leaf). The
compile path rejects loud (F-3), but eval-only users get silent garbage. **Fix: require
`num_leaves(idx_ty)?==1` + a negative test.**
**NIT-1:** the "always==a" comment (1297) isn't implied for arbitrary single-leaf idx (`Sum(Zero,One)`
has idx_count=1 but byte 1); Bit/Flat(w) have contiguous bytes 0..idx_count-1 so slice-2 is
unaffected — guard `max byte < n` or note the contiguity assumption.

## F3 — parametric ROM-shape-matches-f3: HOLDS (recomputed instruction-by-instruction)
d=3 carrier = Nat_3(3 leaves) + 5 payload = 8 leaves; resident=2 → counter Reg0-2, acc Reg3, data
Reg4, idx Reg5, arr Mem0/Mem1 — home_of has NO widths. Body: `id⊗memexch`→`MexchReg{rd:4,ra:5}`;
assoc→0 instrs; reduceStep→`Exch{3,4}`,`Xor{rd:4,rs:3}` (check_pattern_2reg whole-table, XOR of
`<2^w` bytes stays `<2^w`, no addInto/swap misfire),`Exch{3,4}`; `succ_idx`→`Xori{rd:5,imm:1}` (only
immediate, fixed 1, idx stays Bit). Body `[MexchReg,Exch,Xor{rd:data-home},Exch,MexchReg,Xori{idx,1}]`
for EVERY w∈{1,2,4}, identical to f3's Bit ROM. head=6≤8 → no displacement → f3's CLEAN regime:
**B=24, T(k)=12+6k, park {50,25,0}%** carry over (NOT f3.5's displaced B=34). Parametric pin sound.

## F4 — byte-wide teeth + Flat(4) cost: teeth OK; MAJOR-2 (cost trap)
Teeth: §3 correctly mandates full-word `acc_out=acc0⊕(⊕arr[touched])` — load-bearing (a bit-0 check
passes an upper-bit-dirtying ROM). Make explicit the output decoder must be a full thermometer-depth
reader (f3's `to_bit` panics on non-Bit → lazy port fails loud, good). Domains: Flat(2)=1536 ✓,
Flat(4)=393,216 ✓. **Cost trap:** `run_on_cpu_fast_resident` RECOMPILES the iso every call (2957),
each compile runs F-3 whole-table verify on 4 memexch occurrences (4×8192) + 4 fresh 8192-value deep
enumerations; f3's `eval_xor_reduce` helper also REBUILDS the iso per input. Verbatim at Flat(4):
393k × O(10–100ms) = **hours**, on a timeout-less gate (the 2^24 lesson). Flat(2) is seconds — the
trap bites exactly at the width the design wants whole-domain. **Fix: §3 must mandate build-iso-once
+ compile-once; per input use `marshal_resident` + `run_rom_fast(rom,state,budget)` (public, 2979) +
`unmarshal_resident`, vs a hoisted `eval(&iso,…)`.** Then Flat(4) is minutes even in debug; striding
unnecessary. (enumerate_values(carrier) at Flat(4) = 393k deep values ~0.5GB — fine under slice-1's
5.34GB; index-driven generate-and-drop safer — note in the test header if full enumeration kept.)

## F5 — reduce composition over Flat: HOLDS
Both sides traced: eval `SwapProd;xor_w;SwapProd` on (acc,data) → (data,acc) → (data,data⊕acc) →
(acc⊕data,data) — borrow-swap gives (acc⊕data,data) over full words. CPU: after `Exch{3,4}` DATA-home
Reg4 holds acc; `Xor{rd:4,rs:3}` writes it; closing Exch restores — F3-1 reincarnates identically
(Xor's rd = data-home reg); §3's pin of f3's exact body slice (f3 T5's assertion incl a_r≠ra,
a_r≠d_r) covers it. Xor byte-parallel, closed over `<2^w` → canonicalization preserved (MINOR-2 passes).

## F6 — folded MINORs / allowlist / Flat(8)
MINOR-1 scope ✓ (3 cardinality copies core/enumerate.rs:14, eval/enumerate.rs:26, eval/interp.rs:343
+ core/prim.rs:62 enumerate_values — all in core/eval allowlist; no netlist copy). MINOR-3 feasible ✓
(asserts at compile.rs:146-149/198-201; pub wrappers make #[should_panic] writable in tisc). MINOR-2
checkable but SHARPEN: "across the whole run" needs per-step state — fast path via `run_rom_fast(rom,
state,1)` single-step loop asserting regs/scratch `<2^w` each step (default tier); real-CPU via the
f3 t7 pattern (#[ignore]). **NEW gate gap:** the `--test` check_commands run ZERO core/eval lib tests
(`--test` excludes `--lib`), so MINOR-1 core/eval edits land ungated — add `cargo test -p theseus-core
-p theseus-eval --lib`. Flat(8) exclusion honest (256·2·256²=33.5M entries, tens of GB). Silent-Flat
sweep: SwapProd/Assoc/Times/Iter/marshal all position/counter-driven, no width consult, no wildcard —
nothing found.

## SHIP / DO-NOT-SHIP-UNTIL
**DO-NOT-SHIP-UNTIL (design amendments, both one-liners):**
1. MAJOR-1: `memexch_prim_idx` gains `num_leaves(idx_ty)?==1` guard + negative test.
2. MAJOR-2: §3 mandates hoist-and-compile-once for the whole-domain oracles (iso once; per-input
   `marshal_resident`+`run_rom_fast`+`unmarshal_resident` vs hoisted `eval`).
**RECOMMENDED (fold in, non-blocking):** add `--lib` core/eval to check_commands; word MINOR-2 as
fast-path run_rom_fast single-stepping (default) + t7-style real-CPU (#[ignore]); note the full-word
output decoder explicitly; NIT-1 comment fix.
**Everything else SHIPS:** recognizer-untouched bet verified (no elem==idx/bit-valued assumption;
addr in-bounds; F-2 identical); ROM byte-identical across w and to f3 → B=24/T=12+6k/clean regime;
borrow-swap over words with F3-1 pinned; idx:Bit sidesteps the trap; Flat(4) cap honestly costed. The
probe (§5) is the right first step.
