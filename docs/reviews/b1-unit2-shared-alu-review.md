# B1.3 (Unit-2) adversarial review — universal 3-operand shared ALU bracket

**Reviewed:** `33ae5e2..28d9a20` (HEAD `28d9a20`, branch `phaseb-shared-alu`,
worktree `/tmp/phaseb`). Substantive change: `crates/theseus-tisc/src/decoder.rs`
(+294/−49) — s0-workspace config decode, config-gated rt→B(BR) fetch,
`shared_alu_block` universal bracket folding Ccnot (R 0b1011) + Cswap (R 0b0110),
`decode_execute_shared` block count 5→3. The advisor's uncommitted `me_a3`
golden re-pin in `tests/energy.rs` was excluded from review per task scope.

**Reviewer stance:** fresh context, did not write the change. Method: symbolic
state-walk of every bracket phase for every IR word class + targeted eval-level
test runs (NO heavy lowering — a heavy authority run owned the box's RAM).
Verdict at the end.

---

## 1. State enumeration — what each phase assumes, and what could defeat it

The block is a nest of self-inverse brackets. For each transition I list the
invariant assumed and the result of trying to defeat it.

### S0 → S1: `decode_config` forward (outermost)

*Assumes:* s0 = 0 and s1 = 0 at entry (inter-instruction scratch-quiescence
invariant, same one the old per-op `pred_compute` relied on); IR loaded;
register file un-permuted.

*Produces:* s1 = one-hot config byte (≤1 of 8 lines set); s0 restored to
exactly 0 (prefix bit 0 / chain bit 1 both computed AND uncomputed inside the
forward pass).

*Attack — construct an IR word that leaves s0 ≠ 0 (task item 1):* I could not,
and the structural argument is airtight, not sampled:

- Every write to `prefix` (s0[0]) is a NOT-conjugated Toffoli/CNOT
  (`prefix ^= g`), applied **twice per family branch** with the *same* control
  wires and *identical* NOT conjugation, with only `land_op`s in between —
  and `land_op` never touches FMT_BIT/op3 (its NOTs are on op2/op1/op0 and are
  self-cancelling within each land). So `prefix ^= g; …; prefix ^= g` = 0 for
  **every** value of fmt/op3, including reserved and op4/op5-set words. All
  three branches (fmt∧¬op3, fmt∧op3, ¬fmt) have this shape; the ¬op3 branch's
  uncompute (decoder.rs:1551-1554) and the op3 branch's paired Toffolis
  (1561-1563) and the I-branch's paired CNOTs (1569-1571, 1583-1585) are exact
  mirrors.
- `chain` (s0[1]) is XORed with the same 3-literal product twice inside each
  `land_op` (CCC-NOT at 1509 and 1523) with the literal wires bit-identical in
  between (the interleaved Toffoli at 1516 targets only `cfg_wire(final)` with
  controls prefix/chain — it cannot disturb op2/op1/op0). Restores to 0 for
  every word, value-independently.
- Wire addressing: `cfg_ws_wire(k) = 5·W+k` = slot 5 of
  `config_decode_fields()` = `AUX_OPERAND_FIELD` (s0) ✓; `cfg_wire(k) = 32+k` =
  slot 4 = s1 ✓; no collision. n = 6·W consistent in every `gate_at`.
- The high opcode bits op4/op5 are **don't-cares in the new decode exactly as
  they were in the old** — `r_pred` tests {fmt, op3..op0} and `i_pred`
  {fmt, op2..op0} (decoder.rs:513-532), and the new decode tests the *same*
  literal sets per op (Ccnot: fmt∧op3 prefix + low3=011 = old
  `r_pred(0b1011)`'s five literals; Cswap: fmt∧¬op3 + low3=110 =
  `r_pred(0b0110)`'s). So a word like R op=0b011011 aliases Ccnot in BOTH old
  and new — pre-existing, documented (asm.rs MAJOR-3 deferred; MINOR-4 pin
  test), **no old→new divergence**.

*Empirical:* `b13_s0_restored_to_zero` (10 op classes) PASS;
`b12_config_exclusion_and_fmt_gate` compares **full state** (incl. scratch[0])
vs the reference for all R 0..16 + I 0..8 words, so reserved-word s0
restoration is covered there too. Both re-run by this reviewer: PASS.

### S1 → S2: `shared_fetch_rt_br_gated` forward

*Assumes:* config one-hots valid in s1 (✓ — decode is strictly before);
r0-slot and BR position-stable inside the sel_rt bracket; ≤1 gated swap fires.

*Attacks (task item 2):*

- **(a) inertness for reg+imm/Swapbr/Xchg/reserved:** both gated Fredkins carry
  CFG_CCNOT / CFG_CSWAP as control; for every non-controlled word both lines
  are 0 ⇒ swap = id ⇒ the bracket is `sel_rt ; id ; sel_rt⁻¹ = id` **exactly**
  (permutation conjugation of identity), for ANY rt-field bits including the
  historical killer (rd=r0, rt-field=0) and maximal steer (rt=0x1F). Verified:
  `b13_rt_field_inert_for_reg_imm` PASS (Addi + Add, rt=0x1F, full-state
  compare incl. BR=0x99), and `full_decoder_arithmetic`'s Add r0,r1 (rt=0)
  sample is the exact word that caught the ungated-fetch corruption.
- **(b) fetch + BR restoration for Ccnot/Cswap:** traced both polarities —
  entry BR=br0: fetch puts BR=r[rt], r[rt]=br0 (content-agnostic swap); on
  exit the mirrored gated swap runs with the config lines STILL held (decode⁻¹
  is outside it) and restores BR=br0, r[rt]=final-B. Cswap-taken: r[rs]=y,
  r[rt]=x, BR=br0 ✓; not-taken and Ccnot: BR/r[rt] restored ✓.
- **(c) gating-condition validity:** the swap's controls are read from s1
  between sel_rt and sel_rt⁻¹. `select_operand` explicitly excludes positions
  0/1 = s0/s1 from its level-3 range (decoder.rs:361-384) and levels 2..0 act
  on reg fields 0..7 only; `scratch_field(i)=14+i` ⇒ level-3 touches fields
  16..21, never BR(9)/s0(14)/s1(15)/IR(10..13). Position-stability holds for a
  register OR scratch rt.
- **co-fire double-swap:** would require one word to set both CFG_CCNOT
  (fmt∧op3∧low3=011) and CFG_CSWAP (fmt∧¬op3∧low3=110) — contradictory in op3.
  Impossible by construction, not just by test.
- Nothing between decode and decode⁻¹ writes s1: fetch brackets target
  regs/scratch-r0-slot/s0/BR; imm_to_a targets s0+IR(restored); ALL ALU cores
  use s1 wires as *controls only* (TK adder is zero-ancilla in-place —
  decoder.rs:547-603 — rotator/xor/exch/ccnot/cswap likewise never target a
  wire ≥ 3W). Config lines cannot be corrupted mid-bracket.

### S2 → S3 → S4: `fetch_rs_fmt_gated` ; `sel_rd` ; `imm_to_a`

Unchanged B1.2 machinery; what changed is what sits around it. Checks:

- R-format: s0 ← r[rs] (swap; 0 parked in r[rs]); I-format: inert swap,
  crossbar permute-and-restore (`b12_iformat_nonzero_immediate_steer_inert`
  PASS through the new route).
- `imm_to_a` s0=0 precondition: for I-format the rs-fetch was inert AND
  decode-forward restored its s0 workspace ⇒ s0=0 ✓ (this is the L15-critical
  chain, closed by the structural argument in S0→S1).
- On unwind, `fetch_rs⁻¹` is a content-agnostic swap: s0 ends = the 0 parked
  in r[rs] regardless of what the core did to A (Exch writes old-D into A,
  Cswap writes old-B into A — both correctly land in r[rs], s0 ends 0).
  `decode⁻¹` therefore runs with s0[0]=s0[1]=0 and unchanged IR ⇒ clears the
  one-hots exactly.

### S4 → S5: the 8-core gated ALU

- `ccnot_core_at`: wire-for-wire the old `ccnot_core` with control 3W+cfg
  instead of 3W (old pred bit 0). Same slots [D, A, B, s1]. Verbatim ✓.
- `cswap_core_at`: swap A[i]↔B[i] under cfg∧D[0]. D[0] read directly at wire 0
  — valid because sel_rd has already routed rd to the r0-slot and s0/BR are
  crossbar-stable. `gated_fredkin_prim` is the K2 fast path (2 ctrl + 2 swap)
  — same prim the old `cswap_instr` used, in vocabulary; **no K4 anywhere**
  (the §8 arity wall is genuinely avoided, not squeaked past).
- Off-line cores are gate-exact identities (every gate carries its cfg
  control), so reserved/MEXCH(1001)/MEXCHR(1010)/Xchg(0111)/Swapbr(1000) words
  see: no config line + inert rt-fetch + fmt-driven-but-restored rs-fetch +
  sel_rd conjugation of identity ⇒ block identity. Verified via
  `full_decoder_reserved_opcodes_are_identity` + the exclusion sweep +
  `dispatch_exclusivity` (all PASS, re-run).

### Decode split (task item 3) — exact opcode check

| word | prefix branch | low3 land | line fired |
|---|---|---|---|
| Add..Rotr R 0000-0101 | fmt∧¬op3 | 000..101 | its own line only |
| **Cswap R 0110** | fmt∧¬op3 | 110 | CFG_CSWAP only |
| Xchg R 0111 | fmt∧¬op3 | 111 not landed | none ✓ |
| Swapbr R 1000 | fmt∧op3 | 000 ≠ 011 | none ✓ |
| MEXCH R 1001 / MEXCHR R 1010 | fmt∧op3 | 001/010 ≠ 011 | none ✓ |
| **Ccnot R 1011** | fmt∧op3 | 011 | CFG_CCNOT only |
| R 1100..1111 | fmt∧op3 | 100/101/110/111 ≠ 011 | none ✓ |
| Addi..Rotri I 000..100 | ¬fmt | landed | its own line |
| I 101/110/111 | ¬fmt | not landed | none ✓ |

Exch(0011)/Rotr(0101) vs Ccnot(1011): distinguished by the op3 prefix, not
low3 — low3(0b1011)=011 collides with Exch's low3 **only** under the ¬op3
prefix, which Ccnot doesn't satisfy. Rotr low3=101 lands CFG_ROT_REV only
under fmt∧¬op3. No cross-fire path exists. `low3()` bit-order checked:
low3(0b0110)=[1,1,0], low3(0b1011)=[0,1,1] — match the [op2,op1,op0]
convention of the six pre-existing entries.

### Cswap semantics + BR restoration (task item 4)

Full trace (distinct rd/rs/rt, entry BR=br0, r[rs]=x, r[rt]=y, r[rd]=d):
fetch_rt: BR=y, r[rt]=br0 → fetch_rs: s0=x, r[rs]=0 → sel_rd: D=d →
taken (d[0]=1): s0=y, BR=x → unwind: r[rs]=y, s0=0, r[rt]=x, BR=br0, r[rd]=d.
= `swap r[rs],r[rt] iff r[rd][0]`, BR round-trips ✓. Not-taken: core inert,
all brackets self-restore ✓. Both polarities in `b13_whole_domain_equiv_cswap`
+ `full_decoder_moves` + the 4-case real-CPU test (PASS per implementer's
heavy session; wd/moves re-run by this reviewer: PASS).

### Reg+imm behavior preservation (task item 6)

The 11 reg+imm ops now traverse: decode-outermost (was innermost) + the
gated-off rt bracket. Differences are (i) decode reads s0 ws bits at a time
s0=0 instead of s1 bits mid-bracket — same one-hot output; (ii) an extra
`sel_rt ; id ; sel_rt⁻¹` conjugation — exact identity. The b12 whole-domain
teeth **do** now exercise the new route: `wd_unified_ctx()` was repointed to
`shared_alu_block_for_measurement()` (tests/decoder.rs:660-663), so
`b12_whole_domain_equiv_{arith,logic_rot}` evaluate THROUGH the universal
bracket. Re-run here at cap 16 (smoke) + all four `full_decoder_*` semantic
teeth at full sample set: PASS. (Implementer ran cap 256 in the heavy
session.)

---

## 2. Findings

### No BLOCKER found

I could not construct an IR word, operand pattern, or entry state (within the
architectural s0=s1=0 step invariant) where the new block silently corrupts
state or diverges from the old per-op decode **on any encodable word**. The
s0-workspace restore is structurally exact (XOR-adjoint, value-independent),
the rt→B gate is provably inert off-line, the op3 prefix split is disjoint by
construction, and the ring-facing shape (one fixed all-gated iso; Seq/Branch
of bijections) is preserved, so the ADR-0034 structural argument carries.

### MAJOR — none.

### MINOR-1: Cswap alias-word behavior changed (encode-rejected words only)

**Trigger:** R 0b0110 with non-pairwise-distinct operands, e.g.
`Cswap rd==rt` (word `r_ins(6, 2, 1, 2)`): OLD read the control from
r[rd][0] via `copy_rd0` *before* any fetch; NEW reads D[0] *after* the rt
fetch has parked br0 in r[rt]=r[rd], so the control is **BR_entry[0]**, not
r[rd][0]. `rd==rs` similarly: control becomes 0 (r[rd] zeroed by fetch_rs).
`rs==rt`: old left s0 dirty (=v) and r[x]=0; new leaves s0=0 but ends
r[x]=br0, BR=v.

**Why not a blocker:** `asm::encode` rejects all of these
(`CswapNotDistinct`, asm.rs:300; mirrored in bveq MAJOR-5), so no assembled
ROM can contain them; both old and new remain total bijections (ring proof
unaffected); the pre-existing module doc already declared aliased semantics
unspecified. **Ask:** update the module header (decoder.rs:60-83) which still
describes the s1-bit-4 CSWAP copy and the OLD alias residues as the datapath,
and add one line to the Cswap core doc recording the new residue class
(control = post-fetch D[0]). Optionally pin one alias word's *bijectivity*
(not semantics) as a residue-documenting tooth.

### MINOR-2: stale comments / measurement-hook drift

1. `cswap_core_at` doc says "3 controls + 2 swap = K3" — the gate is
   `gated_fredkin_prim` = 2 controls + 2 swap (K2 fast path). Cosmetic, and
   strictly better than claimed.
2. `group_a_alu_view()` claims to "reproduce the exact B1.1 decode … so the
   B1.1 Group-A-portion measurement remains comparable", but
   `alu_config_decode_families(false)` now unconditionally lands CFG_CSWAP
   and the whole Ccnot op3 branch, and its carrier grew 5→6 fields — the
   B1.1-comparability of `group_a_shared_for_measurement` masses is broken
   (measurement-only; not wired; semantics of the view unaffected since the
   extra lines gate no core there).
3. `shared_fetch_rt_br_gated`'s comment "wire layout … s1 = 2W..3W" then
   `gate_at(3*W, &[2*W+cfg, i, W+i], …)` — correct, but the doc order
   [r0-slot, BR, s1] vs control-first gate wires is easy to misread; fine as
   is, noting I checked it: control=s1 bit cfg, swap=(r0-slot[i], BR[i]) ✓.

### MINOR-3: 3-operand scratch-operand coverage gap

The b13 3-op equivalence triples are register-indices only (task comment says
"reg∪scratch not needed for op equiv"). Ccnot-with-scratch-rt IS covered
through the full decoder by the A-0 `blocker2` regression (re-routed through
the new bracket automatically — it now proves the *gated* fetch + universal
bracket handle a scratch rt). **Cswap with a scratch operand has no direct
tooth** through the new bracket. Routing is symbolically identical to Ccnot's
(same crossbar, same position-stability argument), so risk is low — but one
`Cswap rd, rs, s2` case in `wd_equiv_3op`'s triples would close it cheaply
(L20: cover the class, not just the instance).

### Gate items (not code defects — must land before merge, per §2.3 obligations)

- **Ring closure not re-run in the reviewed commit** (`validate() == Ok`,
  bus-quiescence) — §9 says deferred to the authority run the advisor is
  executing now. Required before merge.
- **Golden re-pins** (`me_a3_golden` step_j 1.5314006103e-9 / total_j
  3.6753614647e-8, clause-table decode cells 337 459) — in the advisor's
  in-flight re-pin, outside this review's scope; must be the measured values
  from the post-review authority run, and B/T must stay 24/20.

---

## 3. What I verified and how (confidence)

- **Read every changed line** of decoder.rs (+294/−49 diff, plus the
  surrounding 400 lines of unchanged bracket machinery it composes with) and
  the full test diffs (decoder/compile/energy).
- **Symbolic:** exhaustive per-branch walk of the config decode for all IR
  word classes (live/reserved/high-bit/I-format); slot position-stability
  from `select_operand`'s level ranges + `scratch_field` layout; full Cswap/
  Ccnot traces both polarities incl. BR round-trip; alias-triple divergence
  analysis (finding MINOR-1); s1-write-freedom of every inner block.
- **Ran (eval-level only, no lowering, per the RAM constraint) — 12 teeth,
  all PASS:** `b13_s0_restored_to_zero`, `b13_rt_field_inert_for_reg_imm`,
  `b13_whole_domain_equiv_ccnot`, `b13_whole_domain_equiv_cswap`,
  `b12_config_exclusion_and_fmt_gate`,
  `full_decoder_reserved_opcodes_are_identity`, `full_decoder_moves`,
  `dispatch_exclusivity` (batch 1, 127 s) and
  `full_decoder_{arithmetic,logic,rotates}`,
  `b12_iformat_nonzero_immediate_steer_inert` (batch 2, 14 s) — all through
  the WIRED `decode_execute()` / isolated `shared_alu_block`.
- **Deliberately not run by this reviewer:** `b13_ccnot_full_rs_sweep`
  (4096 dual-evals) and the cap-256 `b12_whole_domain_equiv_*` sweeps —
  multi-hour CPU on a box whose RAM/CPU is owned by the in-flight authority
  run; the implementer's heavy session ran them to completion (§9) and the
  advisor's authority run repeats them. Also not run: the heavy lowering
  tier (`poc_b13_shared_alu_measure`, clause table), ring `validate()`, and
  the `#[ignore]` real-CPU test — same reason; these are the pre-merge gate.

Confidence: **high** on decode correctness, s0 restoration, gating inertness,
co-fire impossibility, and old==new on every encodable word — these are
structural (value-independent XOR-adjoint / control-gating arguments), not
merely sampled. Confidence on the energy/ring numbers is deferred to the
authority run by design.

---

## 4. Verdict

**FIX-THEN-SHIP** — no blocker, no major. The construction is sound and the
datapath fold is exact on all architected words. Before merge:

1. Land the MINOR-2 doc corrections + MINOR-1 module-header update (stale
   s1-bit-4/alias-residue text now misdescribes the wired datapath — that doc
   is load-bearing for the next reviewer).
2. Add the MINOR-3 scratch-rt Cswap case to `wd_equiv_3op` (one triple, ~2 s).
3. Complete the in-flight authority gate: ring `validate()` + bus-quiescence,
   the real-CPU `#[ignore]` test in CI-record, and the golden re-pins from
   measured values.

Items 1–2 are additive (docs + one test case) and do not perturb the reviewed
netlist; no re-review needed if the diff stays doc/test-only.
