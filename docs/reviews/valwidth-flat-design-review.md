# Adversarial design review — `Type::Flat(n)` value-width foundation

**Design:** `docs/scratch/valwidth-flat-design.md` · **Reviewed at:** `chessai/theseus-v2`
@ `16b3ce6` · **Reviewer:** fresh-context Fable 5 (did not write the design).

**Method — recomputed, not trusted.** In a throwaway worktree the reviewer actually added
`Type::Flat(u32)`, let rustc enumerate every non-exhaustive match, filled the arms per the
design, built the whole workspace, and ran: (a) the bare byte-Xor prim at Flat(4)
whole-domain AND Flat(8) strided on `run_on_cpu_fast` AND the **real** `system_step`
`run_on_cpu`; (b) memory/time of the Flat(8) 65536-entry table; (c) `memexch_prim` over
Flat elements; (d) the full workspace suite. Worktree deleted after; repo untouched.

**Verdict.** The *foundation* (§0–§2, bare-Xor §4) is sound and verified end-to-end — built
and ran. But the *integration reduce* (§3) **cannot compile as specified** (three breaks),
and the delegation allowlist (§8) has a bug that hard-fails the task. **Ship the
foundation; respec the reduce.**

## F1 — Blast radius: "small + mechanical" CONFIRMED (MAJOR spec corrections)
Exactly **9 non-exhaustive-match compile errors, 4 crates** (all others build untouched):
1. `theseus-core/src/ty.rs:66` Display → `Flat(n) => "Flat({n})"`
2. `theseus-core/src/prim.rs:43` enumerate_values → 2^n thermometers `Right^k(Left)`
3. `theseus-core/src/enumerate.rs:9` cardinality_u64 → `1u64<<n`
4. `theseus-eval/src/enumerate.rs:21` cardinality → `1usize<<n`
5. `theseus-eval/src/interp.rs:338` cardinality_u64 (**3rd copy** — static_bound fuel) → `1u64<<n`
6. `theseus-netlist/src/lower.rs:142` type_is_uninhabited → `Flat(_) => false`
7. `compile.rs:73` num_leaves → `Ok(1)`
8. `compile.rs:114` value_from_leaf_bytes → rebuild `Right^byte(Left)`, assert `byte<2^n`
9. `compile.rs:142` value_to_leaf_bytes → `vec![leaf_value_to_u8(v)]`, assert `<2^n`

Corrections to the design's crate map:
- **theseus-check needs ZERO changes** (§1.3 wrong-benign): all check matches are on the
  *surface* AST Type or only construct core types; Flat unreachable from the elaborator.
- **theseus-netlist IS touched** (§5 wrong): site #6 is a forced error. One line — but §8's
  allowlist excludes `theseus-netlist/**` → `cargo build` can't go green → task hard-fails (F9).
- **Silent wildcard found:** `theseus-eval/src/value.rs:16 inhabits` `_ => false` → every Flat
  value silently a non-inhabitant; load-bearing in `assert_round_trip` (lib.rs:43/56) → Flat
  round-trip tests fail spuriously. MUST gain a Flat arm (thermometer shape, depth<2^n) + test.
- Other wildcards verified correct-by-`_`: `is_numeral_ty`(false), spine/numeral helpers (loud
  Deferred), netlist `rail_width`/`bits_of`/`value_from_bits`(loud UnsupportedType — right for
  the deferred netlist slice), tisc Sum-guards (loud Deferred).
- Additivity proven: probe full suite **558 passed / 4 failed — all 4 pre-existing on clean
  `16b3ce6`** (`theseus-spice/tests/fib_reversible.rs:120` "cell kind beyond K3"). Unrelated.

## F2 — `run_on_cpu` Flat-agnosticism CONFIRMED (load-bearing claim TRUE)
`run_on_cpu` = type_of → compile_with_metadata → marshal → `run_rom_on_cpu` → unmarshal.
`run_rom_on_cpu` builds `step::system_step(&rom)` from ROM **bytes only**; the value Type enters
solely via the leaf-byte codecs. No theseus-netlist on the path. Empirically Flat(8) Xor on the
real system_step == eval (~6.5 s/input — T4 `#[ignore]` sizing right). No netlist Flat arm needed
for L19; §5 deferral honest (netlist *rejects* Flat loudly today = correct failure mode).

## F3 — Canonicalization invariant CONFIRMED CLOSED (one adjacent trap → F5c)
Ops reachable from the slice: `Xor` (bitwise, closed over <2^n), Exch/Xchg/Mexch/MexchReg/
Cswap/Swapbr (whole-byte perms, closed), `Xori{1}` only on the Bit idx, Addi/Subi only on iter
aux/counter — never a Flat leaf. Keep the `<2^n` assert in value_to_leaf_bytes; mirror it in
value_from_leaf_bytes. Whole-domain Flat(2)/Flat(4) verifies closure empirically. **Caveat:**
`succ` over Flat (`+1 mod 2^n`) is NOT closed/expressible at n<8 — same class as Add — and it
bites the §3 reduce's idx advance (F5c). Design defers Add but missed that succ is in the class.

## F4 — Thermometer depth / Flat(8) table cost: MAJOR (feasible, but underestimated ~3.5GB)
Depth-255 fine (recursion ≈255, u8 counter tops at 255 exactly). But entry size ~250–500 boxed
nodes/pair: **Flat(8) Xor PrimDef build = 2.7 s / +2.6 GB; admit +3.0 s; compile +0.9 s; ~4.2 GB
peak.** Constructible (T3/T4 stand) but spec must state the cost + the test must **move** the def
into the Iso (not clone). Standing rule the design should state bluntly: table-based `PrimDef`
caps Flat(8) at **2 operands forever** — 3-operand (16.7M) or the reduce's memexch at Flat(8)
(33.5M) is hundreds of GB. Width-parameterization is load-bearing, not just convenient.

## F5 — The §3 reduce CANNOT compile as specified: BLOCKER (§3/T5 only)
- (a) `memexch_prim` hardcodes `idx_ty = elem_ty` (compile.rs:1228) + requires idx_count ≤ n.
  `memexch_prim(Flat(2),2)` → **Err("index type has 4 values but n=2")**. §3 carrier unbuildable
  at every width.
- (b) Growing the array needs `n=2^w ≤ MEM_WORDS=4` → w ≤ 2. `(Flat(4),4)`/`(Flat(8),4)` both Err.
- (c) Even at Flat(2) the idx can't advance: f3's `succ_idx = SwapSum(One,One)` is the +1-mod-2
  bit-flip coincidence; a 4-valued register idx needs +1 mod 4 — no TISC-1 instr/recognizer
  (`Addi` mod-256 dirties bit 2; Xori/Rotli can't; numeral_ty(4)=3 leaves ≠ 1-register address).
- (d) At Flat(8) the memexch table is infeasible regardless (~33.5M deep entries).
**Fix the design must authorize:** extend `memexch_prim` to take an explicit `idx_ty` (keep 2
elements, `idx: Bit`, `elem: Flat(w)`). Small — semantic-verify already reads the address
generically from `in_bytes[1]`. Makes the reduce work at Flat(2) (domain ≈1536 — the "few
thousand" the author likely intended) and Flat(4) (8192-entry memexch, feasible). But it
contradicts §2.4 "recognizers UNCHANGED" and §8 "do NOT touch recognizer semantics" — the worker
is boxed out of the only change the reduce needs. And per (d) the byte-wide reduce **never** runs
at Flat(8) — ships at Flat(4) max, Flat(8) is bare-Xor only. State that; don't let T5 die at compile.

## F6 — Verify-at-4/ship-8 parametricity SOUND for bare Xor (verified)
Probe: `compile(Xor@Flat(4)) == compile(Xor@Flat(8)) == [Xor{rd:1,rs:0}]`. No width-dependent
codegen (no immediates; homes from leaf counts; WCET identical). Strided Flat(8) real-CPU +
shape-pin is adequate L19 for this ROM (existing step/ISA suites drive the byte-parallel Xor with
dense bytes). Honesty notes: (1) at Flat(8) the eval side is the same table the prim was built
from → the strided test verifies marshal→datapath→unmarshal plumbing, not the table (which is
exactly what this slice introduces — fine, but say it); (2) re-pin the shape per new Flat op.

## F7 — `Value` unchanged CONFIRMED
`enumerate(Flat(n)) == enumerate(numeral_ty(2^n))` value-for-value (n=1..4) — a Flat value IS a
numeral value. Pin this equality as a cheap invariant. `leaf_value_to_u8` already 0..255. Only
0/1-leaf assumption anywhere = `inhabits` (F1). NIT: `Flat(1) ≠ Bit` (encodings of 1 differ:
`Right(Unit)` vs `Right(Left(Unit))`) — distinct types, don't replace Bit; document it.

## F8 — Width-bound enforcement MAJOR gap
`Type::Flat(u32)` public → ctor assert bypassable. `Flat(9)`: leaf_value_to_u8 u8 counter wraps
silently in release at depth 256 (debug panics); `Flat(32+)`: `1<<n` overflow/OOM. Require:
`num_leaves` returns CompileError for n∉1..=8 (the chokepoint every compile hits) + debug asserts
in cardinality/enumerate. T-neg must construct `Type::Flat(9)` **directly**, not via the ctor.

## F9 — Test plan / delegation
- **BLOCKER: add `crates/theseus-netlist/**` to file_allowlist** (F1 site #6). Without it cargo
  build can't pass; task fails opaquely (worker can't see the allowlist).
- **T5 cannot pass** (F5): respec with the memexch idx_ty extension explicitly authorized (amend
  §2.4/§8), reduce at Flat(2)+Flat(4); OR split the reduce to a follow-on slice (this slice =
  T1–T4+T6+T-neg). Don't let the worker discover F5 mid-task.
- T3/T4: note ~4 GB/~10 s Flat(8); move (don't clone) the PrimDef. Whole-suite around merge ✓
  (note the 4 pre-existing spice reds).
- Add tests: `inhabits` Flat arm (F1); value_from_leaf_bytes range assert (F3); direct-Flat(9)
  rejection (F8); `enumerate(Flat(n))==enumerate(numeral_ty(2^n))` pin (F7).
- Add-deferral right; at n=1 `+mod2` IS xor. Xor-only genuinely demonstrates value-width (probe:
  the full byte flows marshal→register→Xor→unmarshal).

## SHIP / DO-NOT-SHIP-UNTIL
**SHIP (as scoped):** `Type::Flat(n)` + the 9 arms + `inhabits` arm; bare byte-Xor T1–T4, T6,
T-neg; verify-at-2/4, strided-real-CPU-at-8 + ROM-shape pin. Foundation correct — built and ran.
**DO-NOT-SHIP-UNTIL:** (1) allowlist += `theseus-netlist/**`; (2) §3/T5 respecced (authorize
memexch idx_ty at Flat(2)/Flat(4), OR split reduce out — state reduce never runs at Flat(8));
(3) `inhabits` Flat arm + test; (4) width bound at num_leaves + direct-Flat(9) test; (5) spec
notes Flat(8) table cost + the 2-operand-forever rule.

**Evidence log:** probe `16b3ce6`+Flat(u32): workspace builds with exactly the 9 arms; Flat(4)
Xor→`[Xor{rd:1,rs:0}]` 256/256 fast==eval; Flat(8) same ROM, 36 strided fast==eval + real
system_step==eval; Flat(8) def build 2.70 s/+2.6 GB, admit 3.01 s, compile 0.91 s;
memexch_prim(Flat(2),2)=Err, (Flat(2),4)=Ok, (Flat(4),4)=Err, (Flat(8),4)=Err; full suite 558
passed/4 failed (all pre-existing on clean tip).
