# Implementation review — value-width slice-4: byte-parallel `Ccnot` (redefined 0b1011, convention B: BR second-operand buffer)

**Reviewer:** fresh-context adversarial agent; did NOT write the change. **Reviewed:** worker worktree
`01KXYRJHGZP1XCQX3ENG4F5FXR` @ `250f4f7` vs base `chessai/theseus-v2` @ `fc79f3b`. **Authority:**
`docs/scratch/valwidth-ccnot-design.md` (r2) + `docs/reviews/valwidth-ccnot-design-review.md`.
**Method:** full-diff line-level audit of the decoder construction against helper semantics
(`select_reg`, `fetch_operand`, `with_two_regs`, `on_fields`, `cccnot_prim`, `predicated`), manual
state-tracing of the swap-fetch bracket incl. aliased cases, arithmetic re-derivation of every golden,
and an **independent recompute** — re-ran every diff-affected test target in the worktree (release, nix
develop), incl. both `#[ignore]` authority tiers. Did not trust the maestro verifier.

**Recompute (all green, exit 0):** `ccnot` 7/7; `decoder` 9/9 (96.1s); `dot_reduce` 4/4 unmodified;
`negative_guards` 26/26 unmodified; `valwidth_masked` 4/4 (92.5s, incl. both system-ring tests) +
`t_real_cpu` green 834.8s (ignored); `ring_two_body_timing` 1/1 (420.5s); `energy me_a3` 1/1 (115.2s).

## Verdict: SHIP. No BLOCKER, no MAJOR. 2 MINOR + 4 NIT, all deferrable to advisor finish.

### 1. Decoder reversibility (THE crux) — PASS
The impl is the design review's verified construction token-for-token. Verified against real helpers:
swap-fetch `fetch_rt_br = sel_rt ; SwapProd[reg_field(0), BR_FIELD] ; sym(sel_rt)` = `fetch_operand`
retargeted at BR — a pure exchange, **content-agnostic: no BR=0 assumption**; BR's entry value rides in
r[rt]'s cell through the core (core reads BR wires 2W..3W as controls only) and is written back. Fetch
is OUTSIDE `with_two_regs` (r[rt]'s r0-slot transit completes/un-routes before rs-fetch/rd-routing).
Core `[r0-slot, s0, BR, s1]` → `gate_at(4W, [3W, W+i, 2W+i, i], cccnot_prim())` = `r[rd].i ^= pred ∧
r[rs].i ∧ r[rt].i` ×8. Step-boundary: s0/s1/BR return to entry for distinct operands → `PC += BR+1`
sees entry BR. Pred-off = identity. **Empirical teeth at BR≠0:** the decoder differential runs all 5
Ccnot samples (incl firing dense-byte) from states with br=0x99 + live scratch junk (s2..s7=DE AD BE EF
33 11), whole-StateVals equality vs independent Rust ref — a copy-fetch assuming BR=0 fails these.
Aliasing (rs==rt / rd==rt / rd==rs) all bijective, unspecified-semantics, rejected by `encode`
(CcnotNotDistinct, 3-case test intact).

### 2. Closure / validate() — PASS, MINOR M-1
T-instr is genuine CPU authority: `system_ring(rom)` lowers the real `system_step(rom)`, bit-simulates;
`ancillas_restored` re-simulates a revolution comparing every ancilla to init; `bus_quiescent` checks
bus lines. 3 dense-byte cases (0xAA∧0x55, 0xFF∧0x55→0x55, 0xC3∧0xA5→0x81) assert result + rs/rt
unchanged + both closure predicates (bit-0 decoder fails case 2). Green 92.5s (real netlist, not a stub).
`validate()==Ok` with the byte-parallel body IS asserted: ring_two_body_timing reaches+passes
`t.validate()` on MEXCH/ADD/SWAPBR ROMs (all contain the new ccnot body). Mutation tests give validate()
real teeth (8 corruptions rejected). **M-1 (MINOR):** `t7_system_ring_validate`'s name/docstring claims
"SystemRing validate()==Ok" but SystemRing has no validate() — the body (correctly) asserts
ancillas_restored + bus_quiescent; and no `two_body_timing_certificate(ccnot_rom).validate()` in the
shipped suite (the system-ring pair on a Ccnot ROM is the strongest closure evidence, so substantively
covered) — add a one-liner `validate_ccnot_rom()` + fix the docstring at advisor finish.

### 3. Golden re-pins — MEASURED, not fudged. PASS
Recomputed: `stage2b_lite_timing_certificate` green 420.5s — pins d_pre 3345/d_mem 35/d_post 289/delta1
3/delta2 1/delta_hold 39/d_locked=period 3676, and `t.validate()` reached+passes (not the f3.5 B-1
panic-before-validate). `me_a3` green 115.2s at 1e-6 rel tol. Arithmetically exact for P=4: delta1 =
(4−3345%4)%4 = 3; delta_hold = 3+35+1 = 39; d_locked = 3345+39+289 = 3673 → pad 3 → 3676. Energy:
24×3.0697567543e-9 = 7.36741621032e-8 vs pinned 7.3674162103e-8 (rel 3e-11). Growth d_pre +0.9%, step_j
+1.91% (< f3.5's +9.15% — no new Op::ALL entry, only the 0b1011 body grows 593→624 cells). B=24/T=20
unchanged.

### 4. Recognizer target-LAST arm + semantic safety — PASS
Arm `(c1,c2,t)↦(c1,c2,t^(c1&c2))` → `Ccnot{rd:regs[2],rs:regs[0],rt:regs[1]}`: leaves of `(elem,(key,
acc))` = [elem,key,acc] → rd=acc, rs=elem, rt=key; acc ^= elem&key = spec (AND commutes). Unambiguous:
`check_pattern_3reg` requires whole-table match; counterexamples (1,0,1)/(1,1,1) separate all three of
cswap/target-first/target-last on any leaf domain → arm order immaterial. Three-way &1 drop complete
(both arms + exec_instr + tests/decoder.rs reference()), byte-agreement verified empirically.
Redefinition safe: dot_reduce 4/4 green unmodified, negative_guards 26/26 untouched, only t_dense
flipped; the previously-vacuous op_samples(Op::Ccnot) are now firing dense-byte cases (f3.5 M-1 closed).

### 5. Flat(4) masked fold + discriminating case — PASS, genuine oracles
26 instrs / 2 Ccnots pinned+passing (hoisted once). Whole domain: enumerate_values asserted = 69,888
(=(1+16+256)·16·16); per input run_rom_fast==eval (eval iso a freshly-built PrimDef fold, not the
compiled artifact) AND extract_acc == acc0 ⊕ ⊕(key∧elem) parsed from INPUT leaf bytes (never reads
eval/ROM output — no tautology). Discriminating case elems=[2],key=2 over all 16 acc0: expected acc0^2;
bit-0 exec_instr yields acc0 (2&1=0) → hard fail; the case also rides T-real (real system_step per step,
green 834.8s). Byte-parallelism on all 8 bits proven at T-instr/decoder tiers (Flat(4) program drives 4,
as the design walls it).

### 6. Doc-rot + scope — PASS with NITs
decoder.rs stale s1-bits-5/6 module text rewritten (BR convention, content-agnostic swap, no-BR=0 note,
aliasing caveat, encode rejection); asm.rs Instr::Ccnot doc → byte-parallel. Scope exactly right: zero
diff outside crates/theseus-tisc; Op::ALL still [Op;15]; no allocator/MAX_LEAVES/SCRATCH_SPILL_BASE
change; Value/carrier untouched. t_real_cpu #[ignore] matches convention. **M-2 (MINOR):** docs/src/
status.md joules stale (still 7.23e-8-era) — outside the worker allowlist, do at merge/finish.

### 7. Else — nothing damning
No hardcoded value papers a discrepancy (goldens re-measured; validate() re-derives; energy identity to
3e-11). Aliasing rejection intact + tested. No new wildcard/non-exhaustive match. NITs: **N-1** module
doc over-generalizes the aliasing formula (rs==rt case stated for all aliases); **N-2** stale "bit-0"
comments (ccnot.rs banner, dot_reduce.rs:3, negative_guards.rs:32); **N-3** t_dense case 2 weakened to
regs[3]-only + non-discriminating bytes (case 1 carries discrimination — free strengthening to restore);
**N-4** t_instr doesn't assert pc/br/regs[3..] at the ring tier (covered at decoder tier — informational).

## SHIP / FIX-BEFORE-MERGE
**SHIP** — merge as-is. Crux construction faithful to the verified design, every oracle genuine, every
golden re-measured independently, both #[ignore] authority tiers pass.
**Advisor-finish (post-merge, none blocking):** (M-1) add `validate_ccnot_rom()` to the timing cert +
fix t7 docstring; (M-2) update status.md joules; (N-1,N-2) comment sweep; (N-3) optional t_dense case-2
whole-regs assert.
