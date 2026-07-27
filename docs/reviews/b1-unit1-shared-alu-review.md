# Adversarial review — B1 Unit-1 (B1.1 + B1.2): shared configurable ALU

**Reviewer:** fresh-context adversarial reviewer (did not write the change).
**Scope:** `f5ff26b..495ea70` on `phaseb-shared-alu` (`/tmp/phaseb`); substantive
change `crates/theseus-tisc/src/decoder.rs` (+371/−38), tests in
`tests/decoder.rs` / `tests/energy.rs`, design `docs/scratch/b1-shared-alu-design.md`.
**Method:** full read of the diff and every helper it composes (`select_operand`,
`ctrl_word_swap`, `fetch_operand`, `with_target`/`with_two_regs`, `pred_compute`,
`gated_tk/xor/rotl_gates`, `gate_at`/`seq_balanced`/`at_wire` in `plumbing.rs`,
`asm.rs` legality), symbolic state-walk of the new bracket, literal-by-literal
comparison of the new config decode against the old `r_pred`/`i_pred` truth
functions, plus three live default-feature eval-only runs (no lowering, per the
concurrent-heavy-run constraint).

**Verdict: FIX-THEN-SHIP. No BLOCKER found.** Two MAJOR items (one known/
deliberate gate item, one missing tooth on the load-bearing new claim) and three
MINOR items, each with a concrete reproducing case per L20.

---

## 1. The states the change moves between (and what each assumes)

The unified block is

```
S0  entry            assumes: s0 = 0, s1 = 0 (decoder scratch quiescent), IR loaded
S1  fetch_rs_fmt     sel_rs crossbar runs UNCONDITIONALLY (steered by IR bits 15..18,
                     which for I-format are IMMEDIATE bits); the r0-slot↔s0 word swap
                     is fmt-gated. Assumes: fmt (IR bit 31) is not permuted by the
                     crossbar; s0/s1 fields are never touched by the crossbar.
S2  sel_rd           rd → r0-slot. Assumes: rd field (bits 20..24) is genuine rd in
                     BOTH formats; s0/s1 excluded from the level-3 reg↔scratch swaps.
S3  imm_to_a         s0[j] ^= ¬fmt ∧ IR[j], j=0..8. Assumes: s0 = 0 at this point for
                     I-format; IR_B0 unchanged until the adjoint leg.
S4  alu_config_decode opcode+fmt → ≤1 one-hot line in s1[0..6]; prefix s1[6], chain
                     s1[7] computed AND uncomputed within the decode. Assumes: s1 = 0
                     at entry; IR restored after every literal-NOT bookend.
S5  ALU core         six gated sub-blocks in sequence; each gate carries its config
                     line as a control. Assumes: config lines are controls only
                     (never targets) inside the cores; the adder/rotator preserve
                     their operand (s0); only Exch writes s0.
S6  decode⁻¹         exact mirror of S4. Assumes: IR and the config lines unchanged
                     by S5 (true: cores never touch IR; lines are controls).
S7  imm_to_a⁻¹       restores s0 to its S3-entry value. Assumes: IR_B0 + fmt unchanged
                     (true), and that the S5 core preserved s0 unless the op's
                     semantics write it (only Exch, which is R-only ⇒ imm legs inert).
S8  sel_rd⁻¹, fetch⁻¹ exact adjoints; write s0's content back into r[rs] (R) or are
                     inert (I).
S9  exit             s0/s1 restored to entry values; only the named registers moved.
```

I attacked each assumption below.

## 2. Attack-by-attack findings

### 2.1 fmt-select correctness (attack 1) — SOUND, but the nontrivial case is UNTESTED (→ MAJOR-2)

For I-format the block is `sel_rs ; gated_swap(fmt=0 ⇒ id) ; sel_rs⁻¹`. This is
*exactly* `sel ; id ; sel⁻¹ = id` at the iso level:

- The crossbar (`select_operand`) permutes only register/scratch **fields**; IR is
  control-only (threads through each Fredkin, no fanout), so fmt (IR bit 31,
  `IR_B3` bit 7) cannot be disturbed between `sel` and `sel⁻¹` — the gate's control
  value is the same on both legs.
- The gated swap targets fields `reg_field(0)`/`AUX_OPERAND_FIELD` only; with
  fmt = 0 every one of its 8 Fredkins is identity.
- s0/s1 are excluded from the level-3 swaps (`select_operand` swaps `base(i)↔base(i+8)`
  only for i∈2..8 — the A-0 BLOCKER-1 exclusion), so even while the I-word's
  imm bits 15..18 steer arbitrary crossbar levels (including reg↔scratch s2..s7
  swaps), s0 stays untouched until `imm_to_a`.

So the cancellation is exact, and s0 is genuinely virgin at S3 for I-format.
**However**: no test in the tree ever runs an I-format word with nonzero bits
15..19 through the unified block. `i_ins()` takes an 8-bit imm; `wd_equiv_op`
builds `ir2 = (ir & !0xFF) | op2`; the exclusion test uses `i_ins`. Every I-word
ever evaluated has crossbar steer = 0000, i.e. the "inert crossbar" claim is only
exercised where the crossbar is trivially off. See MAJOR-2 for the demanded tooth.

### 2.2 imm→A vs s0 = 0 (attack 2) — SOUND on the reachable subspace; divergence class found off-convention (→ MINOR-3)

- `imm_to_a` wire audit: lifts over `[s0, IR_B0, IR_B3]`, `fmt_local = 2W+(31%8) = 23`
  ✓; Toffoli `(fmt_local, imm(j), s0(j))` targets s0 ✓; the ¬fmt is achieved by
  NOT-bookends on the fmt wire, restored ✓. `Iso::sym(imm_to_a) = imm_to_a`
  (mutually commuting Toffolis, shared control, distinct targets), so the adjoint
  leg XORs the same mask again: s0 returns to **its entry value** — 0 for I-format
  (given IR_B0 untouched by the core: true), and for R-format both legs are
  identity (fmt = 1 ⇒ NOT ⇒ control 0).
- The adder (`gated_tk_gates`) and rotator preserve the operand wires (TK
  in-place property; rotator uses s0 bits 0..2 as controls only); xor uses s0 as
  controls. The ONLY sub-block that writes s0 is Exch — and CFG_EXCH is landed
  **exclusively by the R family** (I patterns are 000/001/010/011/100; Exch is
  R 011), so an I-word can never fire the one core that would turn the imm mask
  into an s0 leak. This is load-bearing and correct.
- Is s0 = 0 guaranteed at entry for every reachable ring state? Yes for
  `assemble`-produced ROMs: `check_operand` rejects s0/s1 as operands (idx 8/9),
  `encode` rejects `XCHG s0/s1` (`XchgScratchReserved`, verified `asm.rs:314`) and
  `CcnotNotDistinct` kills the rd==rs s0-residue case; every legal instruction's
  bracket restores s0. Same invariant the old ADDI/SUBI already relied on.
- **Divergence found (off-convention only):** old `xori_core`/`rotli_core` read the
  IR immediate **directly** (controls on IR_B0 wires — no s0 involvement at all);
  the new path computes `rd ^= s0_entry⊕imm` / rotate-by `(s0_entry⊕imm)&7`. On any
  CpuState with s0 ≠ 0, new `decode_execute` ≠ old `decode_execute` for
  Xori/Rotli/Rotri (Addi/Subi are unchanged — old already added `s0⊕imm`). Both are
  bijections (ring proof unaffected); the states are unreachable for legal ROMs;
  but this is a real narrowing of the old robustness envelope and the module
  header still documents the OLD convention. See MINOR-3.

### 2.3 Config co-fire / mutual exclusion / blind spot (attack 3) — VERIFIED EXACT vs old

Literal-by-literal comparison:

- Old `r_pred(op)` = fmt ∧ op3' ∧ op2' ∧ op1' ∧ op0' (op4/op5 **not** tested —
  the documented MAJOR-3 blind spot). New R landing = (fmt ∧ ¬op3) [prefix] ∧
  op2'∧op1'∧op0' [chain]. For all six ops (op3 = 0 in 0000..0101) these are the
  **same five literals** — identical truth function on all 2^32 words, blind spot
  inherited bit-for-bit, not widened, not narrowed.
- Old `i_pred(op)` = ¬fmt ∧ op2'∧op1'∧op0' (op3/4/5 don't-care). New I landing =
  ¬fmt [prefix] ∧ same three literals. Identical.
- Co-fire: within a family, the six/five 3-bit patterns are pairwise distinct ⇒ at
  most one `land_op` chain fires; across families the prefixes are fmt vs ¬fmt ⇒
  mutually exclusive. A line shared by both families (e.g. CFG_ADD_FWD ← Add and
  Addi) can never be XORed twice for one word. R low-3 = 110/111 (Cswap/Xchg
  patterns) and I low-3 = 101/110/111 land nothing ⇒ ALU identity, the ops fire in
  their own blocks.
- CFG_PREFIX bookkeeping: computed (2 NOT + Toffoli), consumed by 6 landings,
  uncomputed by the exact mirror, then reused for g_i and uncomputed again —
  between the compute and uncompute nothing writes IR (land_op's literal-NOTs are
  paired) or the prefix wire. CFG_CHAIN toggles twice per landing (CCC-NOT,
  consume, CCC-NOT) ⇒ 0 after each. All 8 s1 bits accounted; wire arithmetic
  checked (`cfg_wire(k)=32+k`, n=40, FMT_BIT=31 = wire 31, op(k)=25+k ✓).
- s1 bit 4 (CFG_ROT_FWD) collides with CSWAP's control-copy bit **positionally**
  but not temporally: config bits are provably 0 again at S6 exit (decode⁻¹ is an
  exact mirror; cores use the lines as controls only), and `cswap_instr` runs in a
  later, disjoint block. No overlap.

### 2.4 Adjoint-under-gate (attack 4) — SOUND

`Iso::sym(add_core_at(CFG_ADD_REV))` / `Iso::sym(rotl_core_at(CFG_ROT_REV))`:
sym reverses the gate list and inverts each gate; every gate in the list carries
the config wire as a control (Toffoli/CCC-NOT/gated-Fredkin are self-inverse), so
every gate of the reversed block *still* carries the line ⇒ the adjoint is
gated-identity off its line. The Branch-tree disjointness argument survives.
This is the same `Iso::sym(add_core())` pattern Sub has used since pre-A-0 —
now with the control at PRED3+cfg instead of PRED3, which changes nothing
structurally. Live-verified: `fib_step_adjoint_identity` (sym of the whole step
through the unified block) passes — see §4.

### 2.5 Off-opcode / reserved identity (attack 5) — SOUND, tested

For any word firing no config line the whole unified block is
`fetch ; sel ; imm ; decode ; id ; decode⁻¹ ; imm⁻¹ ; sel⁻¹ ; fetch⁻¹ = id`
(every layer self-inverse-bracketed). MEXCH/MEXCHR (R 1001/1010, op3=1 ⇒
g_r=0) and all reserved R low-4 (1100..1111) / I low-3 (101..111) words are
identity with zero s0/s1/BR residue. `b12_config_exclusion_and_fmt_gate`
covers R opc 0..16 and I opc 0..8 × 3 imms against the reference model through
the FULL `decode_execute` with nonzero s2..s7 background (residue-visible). Good.

### 2.6 Single-step-iso / ring implications (attack 6) — SOUND

`decode_execute_shared` is still one fixed netlist (`seq_balanced` of 5 blocks,
was 6); block order is semantically irrelevant (each block identity off its
opcodes, opcodes mutually exclusive). The unified block touches regs, s0, s1
(transiently), IR (transiently, restored) — never PC/BR/mem; no scratch value
crosses the bracket. ADR-0034's structural Seq/Times/Branch bijection argument is
unchanged. B/T invariance is asserted (B=24/T=20) in both poc tests.

### 2.7 Teeth adequacy (attack 7) — mostly adequate; gaps listed

Good: whole-domain per-op equiv is against the OLD `instruction(op)` (a genuine
behavior-preservation oracle, not self-consistency); union-of-axes sweep (full
256 operand at rd=0xAA + full 256 rd at operand=0xAA + 8×8 corners) exercises
every carry chain / rotate amount / bit lane; the seam B1.2 adds is routing +
config (data-independent), and the arithmetic cores are verbatim-reused, so the
axis-union is a defensible stand-in for 256×256. Config-exclusion covers co-fire
and reserved identity. Gaps: (a) I-format crossbar-steer bits never nonzero
(MAJOR-2); (b) high-opcode-bit (op4/op5) aliasing words never run through the
unified block (MINOR-4); (c) fixed rd=r3/rs=r5 in the wd sweep — routing variety
rests on the pre-existing `op_samples`/`scratch_rd` heavy tests (MINOR-5);
(d) **everything new is heavy-tier** — `tests/decoder.rs` is
`#![cfg(feature = "heavy-tests")]` and every fast-tier CPU-run test is
`#[ignore]`d, so the fast gate gives ~zero signal on this change (folded into
MAJOR-1: the advisor heavy authority run is not optional).

## 3. Findings

### MAJOR-1 — heavy tier is currently RED: `me_a3_golden` not re-pinned (known, deliberate; must land before merge)
`tests/energy.rs::me_a3_golden` still asserts step_j = 2.5259595646e-9 /
total_j = 6.0623029551e-8; the branch measures 1.8664606459e-9 / 4.4795055502e-8
(design §7). Until the golden is re-pinned, `--features heavy-tests` fails.
The design doc explicitly defers the re-pin to finish-after-review — fine as a
process, but the merge gate must therefore include: re-pin + a full heavy
authority run (`b12_whole_domain_equiv_{arith,logic_rot}`,
`b12_config_exclusion_and_fmt_gate`, `full_decoder_*`, `scratch_rd`,
`fib_data_dependent_real_cpu --ignored`, ring closure) — none of which the fast
gate runs. Trigger: `cargo test -p theseus-tisc --features heavy-tests --test
energy me_a3_golden`.

### MAJOR-2 — no test exercises the fmt-inert crossbar at a nonzero steer (the ONE new claim without a tooth)
The B1.2 headline invariant — "the sel_rs crossbar still runs for I-format on
immediate bits and cancels self-inverse" — is only ever tested with IR bits
15..19 = 0 (crossbar fully off): `i_ins` takes imm8; `wd_equiv_op` masks
`!0xFF`; fib/feistel immediates are small. My symbolic audit says the
cancellation is exact (§2.1), so I expect the test to PASS — but per L20 this
reviewer finding must ship with a reproducing tooth, and this is the exact
place a future refactor (e.g. anything that moves the gated swap out from
between `sel` and `sel⁻¹`, or widens the level-3 range) would silently corrupt
r2..r7/s2..s7 on every odd-immediate I-op.
**Reproducing case:** I-format Addi, `ir = (0b000<<25)|(3<<20)|(0xF<<15)|0x34`
(imm bits 15..18 all set ⇒ every crossbar level fires, including the level-3
reg↔scratch swaps), state with distinct nonzero r0..r7 and scratch
`[0,0,0xDE,0xAD,0xBE,0xEF,0x33,0x11]`. Expected: `r3 += 0x34`, ALL other
fields (incl. every scratch word) bit-identical, s0/s1 = 0 on exit. Add the
same shape for Rotli (imm bits 15..18 set, low-3 amount) and one
`unified == instruction(op)` comparison. Cheap eval-only test; belongs next to
`b12_config_exclusion_and_fmt_gate`.

### MINOR-3 — Xori/Rotli/Rotri s0-robustness narrowed vs old decoder; module-header conventions now stale
Old `xori_core`/`rotli_core` read IR immediate bits directly and were correct
even with s0 ≠ 0; the new path computes with `s0_entry ⊕ imm`. On s0 ≠ 0 states
the new `decode_execute` diverges from the old (e.g. s0=0x37, `XORI r3, imm` →
old: `r3 ^= imm`; new: `r3 ^= 0x37⊕imm`). Unreachable for `assemble`d ROMs
(`check_operand` + `XchgScratchReserved` + `CcnotNotDistinct` verified), so no
live corruption — but (a) the decoder module header (decoder.rs ~lines 50–58)
still says only ADDI/SUBI require s0 = 0 and describes s1 as "pred bit 0 +
chain 1..3 + CSWAP bit 4" — both now wrong for the unified block (all five
immediates require s0 = 0; s1 carries 6 one-hot lines + prefix + chain);
(b) the divergence class should be stated in the `reg_imm_shared` doc comment
the way the CCNOT alias residues are. Doc fix + optionally one pinned
divergence-documenting test.

### MINOR-4 — opcode high-bit (op4/op5) aliasing equivalence untested through the unified block
Verified by literal comparison that the new decode ignores op4/op5 exactly as
old `r_pred`/`i_pred` did (§2.3), so e.g. R opcode 0b110000 still executes as
Add. But no test pins this (the exclusion test sweeps opc 0..16 only). One
sample per family (R 0b110000 ⇒ Add; I 0b101011 ⇒ Rotli) comparing unified vs
`instruction(op)` would freeze the blind-spot-preservation contract.

### MINOR-5 — wd sweep register/scratch routing variety is thin (accepted, note for the authority run)
`wd_equiv_op` fixes rd=r3, rs=r5. Routing variety (other registers, scratch
operands s2..s7 as rd/rs through the unified block) rides entirely on the
pre-existing heavy tests (`full_decoder_*` op_samples, `scratch_rd.rs`). Those
now exercise the new path — confirm they are all green in the authority run
(they should be; no code change needed).

## 4. What I verified live (default-feature, eval-only — no lowering, per constraint)

- `fib_cpu::fib_step_adjoint_identity` (--ignored, 6.5s): one real `step(rom)`
  forward + `Iso::sym(step)` = identity — exercises the unified block's full
  adjoint including sym(decode)/sym(imm_to_a)/gated-adjoint cores. PASS.
- `fib_cpu::fib_forward_and_backward` (--ignored, 75s): 20 steps forward
  (Add/Exch through the unified R path, per-pair reference check) + 20 adjoint
  steps back to the exact seed. PASS.
- `feistel::feistel_forward` (--ignored, 58s): mixed R/I program (Xor, Exch,
  **Rotli, Rotri**) on the real step iso — the fmt-selected A-source and the
  shared rotator fwd+rev on real hardware-shaped state, per-round reference
  check. PASS.
- Fast tier (`cargo test -p theseus-tisc --test fib_cpu`): assembler tests only
  (3 pass) — confirming the fast gate has no datapath coverage (→ MAJOR-1).

Not run (constraint): anything under `--features heavy-tests` (the b12 teeth,
me_a3, per-op lowering, ring closure) — these are the advisor's authority run.

## 5. Verdict

**FIX-THEN-SHIP.** No provable silent corruption or behavior divergence from
the old per-op decode on any state reachable by an assembled ROM; the config
decode is literal-exact against the old predicates including the documented
high-bit blind spot; the gated-adjoint and imm-mask brackets are sound; the
one genuinely new invariant (fmt-inert crossbar) is correct by construction but
carries no tooth at its only nontrivial point. Before merge:

1. Re-pin `me_a3_golden` (+ any clause-table goldens) and run the full heavy
   authority suite green (MAJOR-1).
2. Add the I-format nonzero-steer regression test (MAJOR-2, case given above).
3. Refresh the decoder module-header conventions + `reg_imm_shared` divergence
   note (MINOR-3); MINOR-4 tooth recommended while in there.

Confidence: high on the symbolic audit (every wire index, literal, and bracket
in the new code was checked against the helpers' actual definitions); the
residual risk is concentrated exactly where the heavy authority run + the
MAJOR-2 tooth point.
