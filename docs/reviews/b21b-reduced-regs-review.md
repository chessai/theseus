# B-2.1b (per-program reduced register count) — adversarial review

Reviewer: fresh-context adversarial agent (did not write the change).
Scope: `8b0d3e2..HEAD` (`ae7ad79` + advisor fix `d339843`) — compile.rs,
decoder.rs, types.rs, tests/b21b_reduced_regs.rs. Design:
`docs/scratch/b2-per-program-widths-design.md` (DF-2 = Q3-A fixed 8-bit
control region; DF-3 pow2 rounding). Method: full read of decoder.rs /
step.rs / types.rs / asm.rs / compile.rs run paths; per-word-class routing
traces at num_regs ∈ {1,2,4,8}; fast-tier + cheap eval-only heavy-file tests
run (no heavy lowering, per RAM constraint).

**Verdict: FIX-THEN-SHIP.** No BLOCKER-class silent misroute found for any
program `derive_layout` would actually produce at the layout it picks
(confidence: moderately high — traced every currently-encodable word class).
But the branch is UNMERGEABLE as-is: the fast gate is RED (F1), and the
reduced-layout crossbar quietly abandons a load-bearing decoder invariant at
num_regs ≤ 2 that today survives only by accidental bracket cancellation (F2).

---

## F1 — MAJOR (fast gate RED): advisor's fix contradicts a stale pre-existing test

`d339843` changed `derive_layout` to pin `num_regs = 8` when any scratch
operand is named. `crates/theseus-tisc/tests/b2_layout.rs:86-93`
(`derive_reg_rounding_and_clamp`, FAST tier, not in this diff) still asserts
the PRE-fix semantics:

```rust
// Scratch operand (idx 10 = s2) is NOT a register — excluded from the
// reg bound (num_scratch is not shrunk this slice).
assert_eq!(derive_layout(&[Instr::Add { rd: 0, rs: 10 }]).num_regs, 1);
```

**Verified failing**: `cargo test -p theseus-tisc --test b2_layout` →
`left: 8, right: 1` at b2_layout.rs:91. The fix is semantically right (the
old expectation IS the misroute hazard the fix removes: idx 10 = s2 decodes
as `s[10 − num_regs]` = wrong scratch word at reduced num_regs); the test and
its comment are stale. **Fix**: update the assertion to 8 with the pinning
rationale. Trivial, but the branch cannot merge red, and a red fast gate on
the fix commit means the fix's own full-suite discipline was skipped.

## F2 — MAJOR (latent, currently unexploitable): s0/s1 position-stability is silently lost at num_regs ∈ {1, 2}

The shared-ALU design load-bears on: *no crossbar steering value can ever
move s0 (operand buffer) or s1 (config/pred word)* — decoder.rs:1650-1657
("safe as config scratch, and position-stable: `select_operand` excludes
s0/s1 from the level-3 crossbar"). At the default layout this holds for ALL
2^5 steering values. The new `select_operand_l` exclusion
(`excl = if level == rbits { 2 } else { 0 }`) protects only the SPLIT level.
Descent levels ABOVE the split move s0/s1:

- **num_regs=2** (positions: r0,r1,s0..s7 = 0..9): level 2 (operand bit 2)
  swaps (2,6)=(s0,s4) and (3,7)=(s1,s5).
- **num_regs=1** (positions: r0,s0..s7 = 0..8): level 1 (bit 1) swaps
  (0,2)=(r0-slot,s1) and (1,3)=(s0,s2); level 2 moves s0/s1 too.
- num_regs=4 and 8: s0/s1 immovable at every level (verified) — safe.

Reachability today (why this is not a BLOCKER yet): the steering bits are IR
rd/rs/rt fields.
- Register operands are < num_regs → high bits 0.
- Scratch OPERANDS pin num_regs=8 (d339843).
- I-format rs/rt regions (IR bits 10..19) are ZERO — `i_word` zero-extends an
  8-bit imm (asm.rs:312-317).
- The ONLY reduced-reachable word with nonzero rt-high bits is **Xchg**
  (sx ∈ 2..7 rides in rt; `operand_locs` deliberately excludes sx, so an
  Xchg program does NOT pin and can derive num_regs ≤ 2).

I traced the Xchg word at num_regs ∈ {1,2} through the full
`shared_alu_block` including the hostile case (s5 with bit 6/7 set via a
prior Xchg, BR ≠ 0): inside `shared_fetch_rt_br_gated`, sel_rt displaces the
config word so the CFG_CCNOT/CFG_CSWAP-gated swap reads a DATA byte's bits
6/7 and can FIRE SPURIOUSLY (swapping r0-slot ↔ BR mid-bracket). The step
survives **only because** the bracket interior (fetch_rs / sel_rd / ALU, all
config-inert for Xchg) touches neither the swapped words (BR, s2) nor the
displaced control word, so forward and adjoint transients cancel exactly.
Net step: correct. Same for I-format words (fmt-gate keeps fetch_rs inert).

So the invariant "s0/s1 immovable" has degraded to "the corruption happens
but cancels because current interiors are disjoint" — an accidental,
untested, undocumented property. It breaks silently the moment:
1. **A-2 lands 20-bit immediates** (explicitly reserved headroom, asm.rs:311):
   I-word imm bits 10..19 then steer sel_rt/sel_rs while the I-op's config
   line IS set; at num_regs ≤ 2 the displaced config makes e.g. ADDI a
   silent no-op and the gated BR swap can fire on data bits — silent wrong
   execution, exactly the class this review was asked to find; or
2. any future interior of those brackets reads/writes BR or scratch.

**Fix (cheap, restores the invariant exactly):** exclude by POSITION, not
only at the split level — skip any swap where `i` or `j` lands on positions
`num_regs` / `num_regs + 1` (s0/s1) at EVERY level. At (8,8) this reduces to
exactly the shipped level-3 `i ∈ 2..8` exclusion (byte-identity preserved);
at num_regs ≤ 2 the dropped swaps are dead for legal programs anyway (scratch
operands are pinned to 8, so the reduced-index scratch descent is never used
to route a real operand). Alternative (blunter): floor the `for_program_facts`
clamp at `num_regs ≥ 4`. Either way, per L20 the fix must ship with a
num_regs=2 end-to-end test containing an `Xchg rd, sx≥4` word and a prior
scratch write ≥ 0x40 (the trigger constructed above).

## F3 — MINOR: `fetch_with` empty/all-zero-ROM identity is built on the DEFAULT carrier

step.rs:336: `if entries.is_empty() { Iso::Id(state_ty()) }` — should be
`state_ty_with(layout)`. Pre-existing from B-2.1a, but first REACHABLE now
that non-default layouts flow into `step_with`. Trigger: empty program or a
ROM of all-zero words (`Addi r0, #0` encodes to 0) at a reduced layout → an
ill-typed phase inside the step seq. Fails loud (type/lowering error), not
silent. One-line fix.

## F4 — MINOR: stale invariant/docs left contradicting the code

- decoder.rs:1650-1657: "position-stable: `select_operand` excludes s0/s1
  from the level-3 crossbar" — FALSE at num_regs ≤ 2 (see F2); must be
  updated alongside the F2 fix.
- `select_fields` doc (decoder.rs:358-362) still says "At a reduced
  `num_regs` the tree depth shrinks (design DF-3) — a residual B-2.1b/c
  dependency" — misleading post-B-2.1b: `select_fields` is now only the
  8-field SCRATCH-file router (num_scratch fixed this slice) + default-layout
  helpers; its fixed 4/2/1 topology is correct for that use but the comment
  points at the wrong axis.

## Attack-by-attack findings (verified sound)

**1. `remap_ir_ctrl` completeness — NO 4th site found.** Exhaustive
enumeration of IR references in the reduced-reachable gate path:
- Remapped (the 3 sites): `select_fields`, `select_operand_l`,
  `fetch_rs_fmt_gated` — exactly the sites lifting default-built `(field,bit)`
  pairs (`RegSel` consts / `ir_bit(FMT_BIT)`) as raw field indices. ✓
- Local-wire uses (layout-safe by construction): `pred_compute` +
  `alu_config_decode_families` (FMT_BIT/opcode_bit as wires inside cores
  lifted over `pred_fields(layout)` / `config_decode_fields(layout)`, both
  `layout.ir_byte_field`-mapped); `imm_to_a` (`fmt_local`), `xchg_instr`
  (`fmt_wire = FMT_BIT % W` over `layout.ir_byte_field(3)`), addi/xori/rotli
  cores; step.rs fetch (`fetch_fields(layout)`, `FetchWires` off w_ctl/w_pc).
- Default-only paths never receiving a reduced layout: emit/mem/absorb
  (`R_RT.bits[i]` raw at decoder.rs:2261 — composite path is
  `Layout::default()` only; `run_rom_on_cpu_with` asserts CPU-only),
  `decode_execute_ops`, measurement hooks, `crossbar_model`. ✓
- Note: `remap_ir_ctrl`'s range guard is `debug_assert` only — fine (callers
  pass IR fields by construction), release builds just skip the check.

**2. Crossbar routing at reduced num_regs.** Byte-identity at (8,8)
confirmed structurally: `(0..4).rev()` emits level 3 `i ∈ 2..8` (+8), then
0..4, 0..2, 0..1 — same steps, same order, remap = identity
(`IR_B0_FIELD = Layout::default().ir_byte_field(0)`). num_regs=4: r0..r3
route to slot 0 and roundtrip (test-verified, 4 tests run green);
reduced-index scratch positions (traced idx 6, 8..11) route correctly but are
DEAD for real ROMs (encoded scratch idx is absolute 8+s and pins to 8).
num_regs=1: empty butterfly is safe (`excl=2 > stride` → empty Rust range, no
panic; legal num_regs=1 words have all steering bits 0). `n_levels` math
correct for all (num_regs, 8) pairs (always 4; `sel.bits[4]` unused as
before). The s0/s1 exclusion generalization is the one defect → F2.

**3. `derive_layout` scratch-operand safety.** `operand_locs` covers rd/rs/rt
for every op, including Swapbr's A-5b scratch rd (encode allows idx 10..15;
`operand_locs = [rd]` → pinned ✓), Cswap/Ccnot rt ✓, MexchReg ra ✓. `Xchg.sx`
is excluded — correct for ROUTING (sx is a scratch-file index, routed by the
scratch-only `select_fields` crossbar via `layout.scratch_field`, num_scratch
unshrunk) but it is precisely what leaves F2 reachable (an Xchg program can
derive num_regs ≤ 2). No case found where the ROM addresses a register ≥ the
derived num_regs: `compile`-emitted operands are all in `operand_locs`, and
`run_rom_on_cpu_with` re-assembles from the same instr list it derives from.
Footgun (flag): for a MEM program, `derive_layout` still returns reduced
`num_regs` (with mem_words=4); no wired consumer accepts that layout today
(CPU-only assert; composite path default-only), but nothing stops a future
caller feeding it to a composite builder whose emit path is default-indexed.

**4. `StateVals::encode_with`/`decode_with`.** Field order matches the
`Layout::*_field` helpers exactly (regs, PC_lo, BR, IR_b0..b3, scratch,
PC_hi); `decode_with` reads back via the same helpers → exact inverse ✓.
Dropped-reg-zero assert: a program that never names r_k cannot write it and
the reduced carrier cannot hold it, so the assert can only fire on a CALLER
passing nonzero un-named registers in `init` — loud panic, documented
contract, not silent corruption. Terminal BR/s0/s1 checks + fuel + parked-PC
bound mirror `run_rom_on_cpu` ✓.

**5. Interplay with merged base.** B-2.0b mem-drop: `state_ty_with` uses only
`num_cpu_fields`; `run_rom_on_cpu_with` asserts CPU-only + quiescent init.mem
→ mem-drop and reg-shrink compose ✓. B-1 shared ALU: config one-hots, pred
chain, fetch chain are all sized off `w_ctl = 8` (DF-2/Q3-A, unchanged);
`cfg_wire = 4·w_ctl`, `FetchWires.n = 8·w_ctl` — independent of num_regs ✓.
Blast radius bounded: `run_on_cpu` (the wired path) still runs
`run_rom_on_cpu` at the default layout; the reduced path is opt-in
(`run_rom_on_cpu_with`, tests/measurement).

## Test-coverage gaps (asks)

1. F1's stale test update (mandatory, gate-red).
2. A num_regs=2 end-to-end step test with Xchg sx≥4 + a scratch value ≥ 0x40
   + an odd-imm I-op (the F2 trigger class) — must exist whichever F2 fix is
   chosen (L20: reproduces the finding).
3. No BR≠0 (loop/Swapbr) program at a reduced layout — pc_add_br/pc_inc at
   shifted field offsets are analytically clean but only pc_inc is exercised
   (straight-line test).
4. num_regs=1 is derivable (`for_program_facts(_, Some(0)) → 1`, asserted in
   b2_layout) but has zero execution coverage.
5. Byte-identity at num_regs=8 rests on the separate
   `poc_step_net_bit_identical` golden (energy.rs, heavy) — advisor must run
   it pre-merge as the file header promises.

## Bottom line

- F1 (MAJOR): fast gate red — stale `derive_reg_rounding_and_clamp`
  expectation; update to the pinned semantics. Verified failing.
- F2 (MAJOR): s0/s1 immovability lost at num_regs ≤ 2; today self-cancelling
  (traced), silently wrong under A-2 imm20 or any bracket-interior growth;
  fix by position-based s0/s1 exclusion at every level (byte-identical at
  default) or floor num_regs at 4; ship with the trigger test.
- F3 (MINOR): `fetch_with` zero-ROM identity on default carrier at reduced
  layout (loud, pathological).
- F4 (MINOR): stale invariant comments.
- No blocker: no silent misroute/decode/panic reachable for programs at the
  layouts `derive_layout` actually picks for them, given today's 8-bit
  immediates and the scratch-operand pin.

**FIX-THEN-SHIP** (F1 mandatory; F2 fix-or-floor + trigger test strongly
recommended before merge — it is this slice's own invariant erosion, and the
next IR-format slice detonates it).
