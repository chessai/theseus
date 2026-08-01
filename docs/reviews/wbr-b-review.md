# Adversarial impl-review — W_BR-b (two-byte come-from δu, RUN-correct w_br=16)

**Commit reviewed:** the WIP implementation (branch `wbr-b-comefrom`, off theseus-v2
`d364ae2`). **Reviewer:** fresh-context Fable (NO cargo — read-only, traced the
register/δu flow by hand). Implementer: Opus (flaked at report; advisor salvaged +
verified). **Verdict:** **FIX-THEN-SHIP** — the come-from ARITHMETIC is correct;
three MAJORs (all the same class: wide programs reaching narrow machinery on some run
paths + missing reject tests) are fixed below.

## The arithmetic — CORRECT (Fable re-derived by hand)

Wide bracket: prologue 0–2, Ltop=3 `Swapbr{rc,rs:rb}`, top 4–9, body, bottom,
Lend=16+b `Swapbr{rb,rs:rc}`, epilogue. Total = spine(6)+iter_len(b+14); the
`debug_assert_eq!` tripwire holds with the WIDE constants. Lend−Ltop = iter_len−1 ⇒
`PC_next = Lend + δu + 1 = Ltop` exactly when δu = 65536−iter_len (pure add at
w_pc=w_br=16, no sext arm). Traced continue / re-entry / exit / first-entry: the
cross-paired `SWAPBR(rb,rc)` (Lend) / `SWAPBR(rc,rb)` (Ltop) delivers δu=(δu_lo,δu_hi)
to BR and catches+zeroes it; closes with BR=0 + all aux {rc,rb,temp,temp_hi,ctrl}
terminal-zero on EVERY path. δu minted from the EMITTED span (F2). vart_halve K=4:
iter_len=137, pinned. **No arithmetic findings.**

## Deviations — BOTH ACCEPTED (Fable-verified)

- **A: reject-guard instead of the F4 global skip-pair.** `guard_no_narrow_swapbr_under_wide`
  rejects any w_br=16 program containing a narrow (rs=0) SWAPBR (skip / ≤128 come-from),
  deferring the skip-pair to W_BR-c. Fable enumerated all 6 SWAPBR emission sites +
  confirmed `needs_w_br16` propagation + that vart_halve (pure wide Trace) is correctly
  NOT rejected. Airtight for compiler-emitted programs. **Contingent on MAJOR-1/2/3.**
- **B: early w_br derive.** `needs_w_br16 → layout.w_br=16` set exactly at wide-bracket
  emission; fit-gate enforced twice (budget `NUM_REGS−5` + the occ-scan None-reject);
  the three detection sites agree. Makes the authority runnable — do not revert.

## Findings — MAJORs FIXED

- **MAJOR-1 (FIXED) — `run_on_cpu_fast_resident` ran wide on the narrow interp.** It
  used `exec_instr`+`pc_update(br as u8)`, ignoring `w_br` (its non-resident twin was
  fixed) → a wide+resident program silently mis-jumps (no terminal check). Resident ⇒
  memory ⇒ wide+memory is deferred to W_BR-c. FIX: reject a wide (rs≠0) ROM in
  `run_on_cpu_fast_resident` (loud `Deferred`), matching the guard philosophy.
- **MAJOR-2 (FIXED) — `run_rom_on_cpu` wide+memory fell into narrow `system_step`.**
  The non-CPU-only / non-quiescent arm runs `system_step` at `Layout::default()`
  (w_br=8; no `system_step_with` exists) → wide SWAPBRs run as single transpositions →
  mis-jump (terminal check only probabilistic after PC wraps). FIX: an explicit
  `else if rom_wide` arm rejects (wide + memory → deferred to W_BR-c) BEFORE the
  narrow system_step arm.
- **MAJOR-3 (FIXED) — no reject tests (L20).** The old `vart_reject_over_cap` became a
  positive test, leaving the suite proving no reject boundary. FIX: `tests/wbr_b_comefrom.rs`
  TEST 5 reproduces the critical rejects — `wbr_swapbr_rd_eq_rs_rejected` (the
  3-cycle-trap encode guard), `wbr_guard_narrow_under_wide_rejected` (deviation-A guard,
  made `pub` for the test; wide+narrow / narrow-only-under-wide reject, pure-wide + narrow-
  under-w_br8 OK), and `wbr_wide_plus_memory_rejected` (MAJOR-2, hand-built Mexch+wide-SWAPBR
  ROM → Err). The fit-gate (Fable: enforced twice) and the 32768 cap need a large-carrier /
  huge-body program to trip — deferred to W_BR-c reject coverage (where the skip-pair +
  memory-wide land and these become naturally exercisable), NOT a silent gap.

## MINORs (noted / accepted)

- MINOR-1: `run_rom_fast` narrow-only — fine for its park-test callers; a doc note suffices.
- MINOR-2: the P2 involution TEST 2 is model-side; the netlist two-transposition is
  covered by the `#[ignore]` real-CPU TEST 4 (δu=0xFF77, distinct bytes — a lo/hi crosswire
  or operand-order swap lands PC wrong there). Acceptable per workflow.
- MINOR-3: `exec_instr_w_br` with rd==rs diverges from the netlist — UNREACHABLE from a
  real ROM (encode rejects rd==rs, now tested); a `debug_assert!(rd!=rs)` is optional
  belt-and-suspenders (accepted as noted).
- MINOR-4: `emit_skip_block` cap still hardcodes `W_BR` — CORRECT under the de-scope (all
  skips narrow; wide+skip rejects); TODO for W_BR-c.

## Everything else checked clean (Fable)

Encode/decode round-trip (`r_word(0b1000,rd,rs,0)`, 0x9000_0000 re-pinned in fib_cpu),
`operand_locs` includes rs, decoder wide path = the proven `with_two_regs` fetch gated on
`br_fields()==2` (narrow arm verbatim), the k_max walk never executes a Swapbr, all ~14
mechanical test-file updates are `rs:0` value-preserving, byte-identity golden green (no
re-pin). Full heavy authority (advisor) + the real `system_step` authority (vart_halve K=4
iter_len=137 reject→run == eval + terminal quiescence) gate the merge.

---

## FULL-AUTHORITY RESULT (advisor, 2026-08-01) — NOT MERGEABLE: 2 regressions

The full heavy authority on the WIP (after the MAJOR-1/2/3 fixes were staged)
surfaced TWO regressions the worker missed (both beyond the Fable impl-review's
scope — the review reasoned on paper; these are netlist/suite-level):

- **BLOCKER (real wide-netlist bug) — `w_br16_step_type_checks` (wbr_a_fields.rs:104)
  SeqMismatch.** `step_with(&w_br16, tiny_rom)` + `type_of` does NOT type-check at
  w_br=16 — the wide step NETLIST is malformed (left_output has +1 Word8 field vs
  right_input). The wide SWAPBR decode path (`swapbr_instr`'s
  `fetch_rs ; with_target(R_RD, swapbr_core) ; sym(fetch_rs)` bracket, decoder.rs
  ~1273-1327) has a field-count/wiring mismatch — almost certainly the s0-slot
  CONTENTION: `fetch_rs` deposits r[rs] into the s0-slot, but `with_target(R_RD)`
  and/or `swapbr_core`'s high transposition (`s0-slot ↔ BR_hi`) also use s0, so the
  bracket doesn't balance. **CRITICAL: every positive wide test passed anyway
  because they use `compile` + the INTERPRETER (`run_on_cpu_fast` / `exec_instr_w_br`),
  NOT the step netlist.** Only `w_br16_step_type_checks` (which builds `step_with`)
  and the `#[ignore]` real `system_step` authority exercise the netlist — so the
  come-from ARITHMETIC is correct (interpreter-verified 3 ways) but the NETLIST wide
  decode is broken. The real authority would have failed; the full-authority gate
  caught it first (fast≠real, L15). **Needs careful decoder-wiring debugging (the
  fetch_rs/with_target/s0 interaction) — NOT a rushed patch on the come-from netlist.**
- **STALE TEST — `mdd_reject_wcet_overflow` (compile.rs:1996).** Asserts "Trace with
  iter_len=136 > 128 must be rejected (BR back-jump reach)" — but that is EXACTLY what
  W_BR-b removes (iter_len>128 now lowers at w_br=16, like vart_reject_over_cap →
  vart_k4). The worker updated vart's reject but missed this one. Update it to the new
  boundary (iter_len=136 now lowers; the reject is at >32768 or a genuine WCET/PC-space
  overflow).

**DISPOSITION:** W_BR-b is NOT merged. The come-from arithmetic + the interpreter
path + byte-identity + the MAJOR-1/2/3 fixes are sound; the wide DECODER NETLIST has
a real type bug that must be fixed (careful decoder work) + the stale test updated,
then the FULL authority + the REAL `system_step` authority (netlist path) must pass.
The branch `wbr-b-comefrom` holds the WIP; this is the resume point.

---

## RESOLUTION (advisor, 2026-08-01) — the netlist bug fixed; NOT the come-from

The full-authority BLOCKER was diagnosed to ground truth (not the review's paper
hypothesis) by type-checking each step phase and each per-instruction decoder in
isolation at w_br=16. **The review's guess ("s0-slot contention in the come-from
wiring") was WRONG.** The come-from wiring is entirely innocent:

- `swapbr_core` (the two-transposition come-from) type-checks as a clean endo. ✓
- `with_target(R_RD, swapbr_core)` type-checks as a clean endo. ✓
- **`fetch_operand(R_RS)` itself SeqMismatches at w_br=16** (off by exactly one
  field) — and it has nothing to do with the come-from.

**Root cause.** `fetch_operand` (decoder.rs) lifted its operand↔s0 swap with the
non-layout-aware `at_fields(op, idx)` = `at_fields_n(op, idx, NUM_FIELDS)`, where
`NUM_FIELDS` is the **hardcoded default** (23, the w_br=8 CPU-field count). At
w_br=16 the state has 24 CPU fields, so the swap was lifted over a 23-field carrier
while the layout-aware `select_operand_l` used the real 24-field carrier — the two
boundary types differ by one field → SeqMismatch. This is a latent B-2.1-class
defect (a lift that should have been width-parameterized but wasn't). It never
surfaced because the SHARED step path uses the layout-aware `fetch_rs_fmt_gated`;
**`swapbr_instr` is the first shared-step caller of `fetch_operand` at w_br=16**,
which is why only W_BR-b exposed it. The identical defect existed in
`ccnot_fetch_rt_br` (the replicated Ccnot/Cswap fetch — off the step path, but it
made `instruction_l(Op::Ccnot/Cswap)` SeqMismatch at w_br=16 too).

**Fix.** Lift both swaps over the layout's real carrier —
`at_fields_typed(op, idx, &layout.cpu_field_widths())` — instead of the default
`at_fields`. At `Layout::default()` `cpu_field_widths() == [8; NUM_FIELDS]`, so
`at_fields_typed(op, idx, &that) ≡ at_fields(op, idx)` EXACTLY → byte-identical on
the default path (the energy golden + replicated-path tests guard it). The unused
`at_fields` import was dropped. **No change to the come-from arithmetic, δu mint,
SWAPBR involution, or the fit-gate** — all of which the impl-review already cleared.

**Also fixed (latent, would have failed the authority anyway):** the committed
`wbr_b_comefrom.rs` had a duplicate `run_rom_on_cpu` import (header + TEST 5) — the
test binary never compiled. Removed the redundant re-import.

**Tests (L20):**
- `w_br16_step_type_checks` (wbr_a_fields.rs) — the exact test that caught it: red
  before the fix (SeqMismatch), green after. ✓
- NEW `wbr16_wide_decoders_are_endos_on_the_netlist` (wbr_b_comefrom.rs TEST 6) —
  co-locates the regression with the come-from tests: asserts the wide step,
  `decode_execute_l`, and each BR-touching decoder (Swapbr/Ccnot/Cswap, with Xchg as
  the no-BR control) are endos on the widened netlist. ✓
- `mdd_reject_wcet_overflow` (compile.rs) — stale reject updated: the 126-NOP /
  iter_len case now asserts it LOWERS at w_br=16 with iter_len = body(126) +
  LOOP_OVERHEAD_WIDE(14) = 140 (verified). The Nat32 large-carrier reject stays. ✓

**Remaining gates before merge:** full heavy authority (SOLO) + the REAL
`system_step` authority (netlist path) + a fresh-context Fable adversarial review of
THIS fix (the netlist wiring), then rebase onto v2 + FF-merge.
