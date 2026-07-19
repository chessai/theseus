# Adversarial implementation review — value-width slice-1: `Type::Flat(n)` + bare byte-Xor

**Diff:** maestro worktree `01KXY1Y67TEHKQ6WSZEMB7MK9S` (`ab90b91`) vs `chessai/theseus-v2`
@ `16b3ce6` · **Spec:** `docs/scratch/valwidth-flat-design.md` r2 +
`docs/reviews/valwidth-flat-design-review.md` · **Reviewer:** fresh-context Fable 5 (did not
write the change). Clippy style excluded per instructions (advisor fixed the 2 lints).

**Method — recomputed, not trusted.** Read the full diff + surrounding code; swept every crate
for wildcard `match` arms that could silently mishandle `Flat`; ran `cargo build --workspace`
(green), `theseus-eval --lib` (126/126), the full `valwidth` suite debug (12 passed + 1 ignored,
150 s, **5.34 GB peak RSS** — matches F4's forecast), **T4 `--ignored` on the real `system_step`
in release (passed, 40 s — the L19 authority)**, and full `cargo test --workspace --release`
(zero failures). A throwaway /tmp probe exercised edges no shipped test covers.

**Verdict: SHIP.** All ten arms semantically correct (recomputed), the width bound has teeth at
the chokepoint, tests non-vacuous, the L19 real-CPU oracle passes. Findings: 3 MINOR + 3 NIT,
none blocks merge.

## 1. The 10 arms — all semantically correct (VERIFIED)
1. ty.rs Display `Flat(n)=>"Flat({n})"` ✓. Variant is `Flat(u8)` (safer than the probe's u32 —
   bounds the shift exponent). 2. prim.rs enumerate_values — exactly 2^n values, k-th =
   `Right^k(Left(Unit))`, in order, iterative (no depth-255 recursion); T1 pins it against
   `numeral_ty(2^n)` for n=1..=8. 3. cardinality (core enumerate.rs, eval enumerate.rs, eval
   interp.rs:343) all `1<<n` in lockstep (eval enumerate delegates to core enumerate_values — no
   drift). 4. netlist type_is_uninhabited `Flat(_)=>false` correct; rail_width/bits_of/
   value_from_bits still loud-reject Flat (deferred-netlist mode). 5. compile.rs num_leaves
   `(1..=8).contains → Ok(1)` else CompileError. 6. value_to_leaf_bytes `[leaf_value_to_u8]` +
   `<2^n` assert; thermo(255)→255 no off-by-one. 7. value_from_leaf_bytes `<2^n` assert, rebuilds
   Right^byte(Left), consumes 1. 8. eval inhabits — accepts exactly `Right^k(Left)` k<2^n; rejects
   k=2^n, Unit, Pair, Left(non-Unit), and Bit's `Right(Unit)` against Flat(1).

## 2. Width bound — teeth at the chokepoint (VERIFIED)
num_leaves rejects n∉1..=8; it is the true chokepoint (compile_with_metadata, recognize_prim,
run_on_cpu_resident all call it). **T-neg constructs `Type::Flat(9)` DIRECTLY** and asserts
`compile(Id(Flat(9)))` errs — genuinely exercises the chokepoint, not the ctor. F8 met.
**MINOR-1:** debug asserts in cardinality/enumerate (F8's second ask) not shipped; `cardinality
(Flat(20))` silently returns 1M, `leaf_value_to_u8` u8 counter wraps in release for thermo(>255)
— reachable only via direct library misuse (every compile path protected). Cheap hardening, non-blocking.

## 3. Test teeth — non-vacuous (VERIFIED)
T2 full Prod(Flat4,Flat4) domain (asserts len==256), run_on_cpu_fast (byte exec_instr) vs eval
(table) — two distinct paths. T3 pins exact `[Xor{rd:1,rs:0}]` at BOTH widths (parametricity pin;
also indirectly proves num_leaves==1 via rd:1/rs:0 homes) + strided 81 dense-byte pairs, floor 36.
T1 pins enumerate(Flat(n))==enumerate(numeral_ty(2^n)) n=1..=8 + full-range round-trip at n∈{1,4,8}.
T4 (#[ignore]) real system_step vs program eval (not self-referential) — **ran it: passes 40 s**.
Move-not-clone ✓. Recognizer misfire impossible (addInto vs xorInto differ at (1,1), whole-table).
**NIT-1:** t1_num_leaves_flat only asserts compile is Ok (would pass if num_leaves returned 3) —
the ==1 fact is pinned by T3/T-neg instead; rename/strengthen.

## 4. Value enum / forbidden edits — clean (VERIFIED)
Value `{Unit,Left,Right,Pair}` untouched. Exactly the 10 authorized files; no decoder, no
recognizer-semantics (check_pattern_2reg/3reg byte-identical), no theseus-check, only the netlist
type_is_uninhabited arm. No scope creep.

## 5. Thermometer depth / Flat(1)-vs-Bit — correct (VERIFIED)
All four walkers iterative (no depth-255 stack risk; thermo(255) round-trips). Flat(1)'s "1"
(`Right(Left(Unit))`) distinct from Bit's (`Right(Unit)`); nothing conflates them (type-driven codecs).

## 6. eval/lib.rs +86 — real tests (VERIFIED)
inhabits accept-all-k<2^n + reject-k=2^n (n∈{1,4,8}); reject Unit/Pair; a Flat(4) Xor prim through
assert_round_trip which asserts inhabits on every intermediate — exercises the F1 silent-wildcard
regression on the hot path. **NIT-2:** test-local thermo_idx returns count on unexpected variant
instead of panicking (harmless — enumerated inputs).

## 7. Wildcard sweep — no silent mishandling (VERIFIED)
Every non-test `match` on core Type across 10 crates: all wildcards loud or conservatively correct
(is_numeral_ty→false then loud reject; interp spine conservative fuel; netlist loud Err;
flatten_layer atomic→rail_width rejects; cli/check match the surface AST). Workspace release suite
green confirms no gaps.
**MINOR-2:** T6 verifies canonicalization via unmarshal→remarshal (codec asserts fire) not raw
register bytes — moot for a 1-instr ROM; carry to the slice-2 reduce (multi-instr ROMs).
**MINOR-3:** the two codec range asserts lack `#[should_panic]` regression tests (load-bearing,
pinned only by probe). **NIT-3:** T3/T4 don't carry the F6 honesty note (at Flat(8) the eval side
is the same table the prim was built from — strided tests verify marshal+datapath+unmarshal).

## SHIP / FIX-BEFORE-MERGE
**SHIP.** Nothing blocks. Evidence: workspace build green; eval lib 126/126; valwidth 12/12 + T4
real-CPU pass; full workspace release suite 0 failed.
**FIX-BEFORE-MERGE:** none.
**RECOMMENDED (non-blocking, advisor may defer to slice-2):** (1) debug-assert n<=8 in the 3
cardinality copies + enumerate_values; (2) two #[should_panic] tests for the codec asserts;
(3) strengthen t1_num_leaves_flat; (4) F6 comment on T3/T4. Carry MINOR-2 into slice-2's reduce test.

**Evidence log:** worktree ab90b91 on 16b3ce6; debug valwidth 149.98 s / 5.34 GB; T4 release
--ignored 40.10 s pass; workspace release suite 0 failed (4 known spice reds didn't reproduce in
release); probes: inhabits(Right(Unit),Flat(1))=false, inhabits(thermo(2^n),Flat(n))=false, both
codec asserts panic out-of-range, thermo(255) round-trips, cardinality(Flat(20))=1048576 silent.
