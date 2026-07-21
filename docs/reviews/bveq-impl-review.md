# BVEQ slice-1 — adversarial implementation review

**Reviewer:** fresh-context adversarial impl-reviewer (did not write the code). Target: worktree `01KY1MZJJ1VAMW3M6YE4JW61CK` @ `8220a38` (base `c3622ea`), diff = `crates/theseus-tisc/src/bveq.rs` (796 L), `tests/bveq.rs` (682 L), `Cargo.toml`. All claims below were verified by reading the code AND by running experiments in the worktree (foreground, release, nix shell); every experiment was reverted — worktree left clean at HEAD.

## VERDICT: SHIP-WITH-FIXES — the encoder core is sound and full-domain-reconciled against `exec_instr`, the I4 check has real teeth in its documented direction, and the slice is genuinely additive; but I demonstrated four concrete breaks in `equiv`'s contract (argument-swap defeats I4; the global Flat(w) constraint certifies a corrupting sequence as identity; out-of-range indices panic or silently alias instead of rejecting; the mis-map negative test does not actually guard `equiv`). None corrupts anything **today** (no production caller), but all four sit exactly where slice-2 will wire in. Slice-2 MUST NOT rewire `recognize_prim` onto `equiv` until MAJOR-1..4 below are fixed, each with a reproducing test (L20).

---

## Findings by severity

### MAJOR-1 — I4 is checked on side `a` only: swapping `equiv`'s arguments silently defeats the byte-Add-at-Flat(4) rejection (demonstrated)

`bveq.rs:615-627`: for `bit >= w` only `a_bdd` is required to be identically false; `b`'s upper output bits are never examined. Nothing in the signature (`equiv(a, b, w)`, `bveq.rs:576-580`) distinguishes model from spec.

**Break, demonstrated:** I built the exact Flat(4) addInto spec from the shipped T3 test and called `equiv(&spec, &instr_model, 4)` (arguments swapped). Result: **`Ok`** — byte-Add accepted as Flat(4) addInto, the precise silent-corruption escape MAJOR-1 exists to prevent, reachable by a one-character argument-order mistake in slice-2 wiring. (Correct order rejects, confirmed in the same run.) A well-formed spec has constant-false uppers, so checking **both** sides costs nothing, makes `equiv` symmetric, and also rejects malformed specs with junk upper bits (currently silently ignored). **Fix:** apply the `bit >= w` obligation to both `a` and `b`; add the swapped-argument case as a regression test.

### MAJOR-2 — the Flat(w) input constraint is applied to ALL registers, so a sequence that corrupts an in-frame register conditioned on an out-of-frame register's upper bits is certified equivalent-to-identity (demonstrated)

`bveq.rs:588-598`: for `w < 8` the constraint zeroes input bits `w..7` of **every** register (`for op in 0..NUM_REGS`). But in the real machine, register files are mixed-width (Flat(4) operands coexist with Flat(8) residents — the whole value-width ladder). A proof valid only when *all eight* registers are Flat(w) is not a proof about a prim whose *operands* are Flat(w).

**Break, demonstrated:** `[Rotri r3,4 ; Ccnot r0,r3,r5 ; Rotli r3,4]` — net effect `r0 ^= rotr(r3,4) & r5`, with r3 restored. `equiv(model, identity, 4)` returned **`Ok`** (under the global constraint, r3's upper nibble is assumed zero, so r0 provably unchanged). Concretely, with `r3=0xF0, r5=0x0F` (r3 an out-of-frame Flat(8) value), `eval_transfer` shows **r0 is corrupted** (`0 → 0x0F`). If slice-2 calls `equiv(…, w)` per its current documented contract, this is a certified-equal corruption of live data. **Fix:** constrain (and apply I4 to) only a caller-supplied operand set, with all other registers' bits free — or make the whole-file-Flat(w) precondition explicit, enforced, and reconciled with what the recognizer can actually guarantee (it can't).

### MAJOR-3 — "never panics across the boundary" is false, and out-of-range `Var` operands are silently RE-INTERPRETED rather than rejected (both demonstrated)

- `symbolic_exec(&[Instr::Add { rd: 8, rs: 1 }])` **panics** (index OOB at `bveq.rs:423-425`; `Instr` fields are `u8`, `asm.rs` `encode` range-checks but `bveq` does not). Demonstrated via `catch_unwind`.
- Worse: `equiv` on a transfer containing `BvExpr::var(20, 0)` does **not** panic — `VarMap::var` (`bveq.rs:154-157`) computes `idx = bit*8 + operand` with no bounds check, so `Var(20,0)` lands on index 20 = `Var(4,2)`. Demonstrated: `equiv(Var(20,0)-transfer, Var(4,2)-transfer, 8)` returns **`Ok`** — two textually different denotations compare equal via silent variable aliasing. Meanwhile `Var(20,7)` (idx 76 ≥ 64) **panics**. A spec-builder index-arithmetic bug in slice-2 would thus be silently absorbed into a wrong-but-plausible proof instead of rejected — the exact "encoder bug makes unequal equal" class MAJOR-3 was about, arriving through the front door. Also: `equiv` itself `assert!`s on `w > 8` / wrong frame size (`bveq.rs:581-583`), and `expr_to_bdd`/`eval_expr` are unboundedly recursive (stack overflow on a deep adversarial expr — an abort, not even unwindable). **Fix:** validate register indices in `apply_instr` (new `BveqError` variant) and `(operand, bit)` ranges in `VarMap::var`/`expr_to_bdd`; turn `equiv`'s asserts into `Err`; regression tests for each.

### MAJOR-4 — `bveq_mismap_negative` does not guard `equiv`'s mapping discipline: I broke the b-side mapping inside `equiv` and the test stayed GREEN

Teeth experiment: I modified `equiv` so the `b` side is encoded under a shifted operand mapping (`operand → (operand-1) mod 8` — the exact collision the test's comment describes, `Var(1,·)` landing on `a`'s `Var(0,·)`). `bveq_mismap_negative` **still passed**: the full-frame identity registers (reg1..7) turn any uniform mis-map into a *spurious inequality*, so `equiv` still returns `Err` — for the wrong reason — and the test's `is_err()` assertion is satisfied vacuously. The test's first half (`tests/bveq.rs:136-160`) demonstrates the trap on raw biodivine `Bdd`s, i.e. it tests the library, not `bveq`.

Two-sided assessment: (a) **the discipline itself is correctly implemented and structurally fail-safe** — one `VarMap` is constructed inside `equiv` (`bveq.rs:585`) and used for both `expr_to_bdd` calls, the interleaved ordering is pinned in one place (`bveq.rs:127, 155`), and my experiment shows that *because* both sides carry the full 8-register identity frame, any uniform mapping divergence manifests as a spurious **reject**, never a false **accept**. (b) But the design-review's required "negative test that deliberately mis-maps one side and asserts `equiv` breaks" is not what shipped, and no test would catch a future regression here. **Fix:** either add a test-only hook that injects a mis-mapped b-side encoder and asserts the verdict *changes*, or replace the test with a comment documenting the structural fail-safe argument above — do not keep a test whose name claims teeth it lacks.

### MAJOR-5 — `bveq` assigns clean semantics to aliased instructions the ISA rejects and the datapath does not implement

`asm.rs` `encode` returns `AliasedOperands` for `rd == rs` on all 2-op instructions and `CswapNotDistinct`/`CcnotNotDistinct` for 3-op (`asm.rs:171-220`); the module header (`asm.rs:20`) says the swap-fetch datapath *deviates* on aliased operands. Yet `apply_instr` models `Xor{rd:0,rs:0}` as zeroing, `Add{rd:0,rs:0}` as doubling (`bveq.rs:434-449`), and the aliasing tests celebrate agreement with `exec_instr` — agreement between two software fictions about instructions the machine cannot encode, which L-b can never reconcile (`run_rom_on_cpu` → `assemble` would error). If slice-2 verifies a candidate sequence containing an aliased instruction, `bveq` will certify semantics no hardware implements. Mitigation: every emit path goes through `assemble`, so the downstream failure is `Deferred`, not corruption — which is why this is MAJOR, not BLOCKER. **Fix:** reject alias-illegal instructions in `apply_instr` (mirroring `encode`), and repoint the aliasing tests at the rejection; MINOR-1 of the design review offered exactly this option.

### MINOR-1 — L-b selector stratification has systematic holes

Across the default 13 cases (`tests/bveq.rs:322-423`) **and** the `#[ignore]` wide sweep (`:483-599`): `rs = 0` is never sampled for any instruction; `rt ∈ {0,1,3,4,5}` never appears; the default Cswap case only exercises ctrl=1 (swap-taken); the wide sweep omits all five immediate instructions entirely. The per-index decoder mux trees for the rs-port index 0 and most rt-port indices are unexercised — precisely the class BLOCKER-1's stratification fix was for. rd coverage is complete (0..7). Cheap fix: add ~4 cases.

### MINOR-2 — L-b compares regs/BR/scratch/mem but not IR (or PC discipline)

`tests/bveq.rs:451-475`. The design-review's quiescence obligation includes "IR returns 0, PC advance uniform"; `run_rom_on_cpu`'s terminal check (`compile.rs:2901-2907`) covers BR/s0/s1 only. A decoder that left junk in IR would pass L-b. One-line fix: assert `ir == 0` and `pc == budget` on the netlist state.

### MINOR-3 — T2 ("wall-break") and T8 compare `instr_semantics` against itself — reflexive, zero discriminating power

`tests/bveq.rs:72-74, 611-613`: `model` and `spec` are the same function call; the test passes for *any* deterministic encoder, including a wrong one. The design's POC compared the instruction model against the *pattern closure re-expressed in BvExpr*. T3 shows the right shape (hand-built spec). The encoder-correctness burden currently rests entirely on L-a (which does carry it — but the headline test's "proven equivalent" wording overclaims). Fix: hand-build the `rd ^= rs & rt` spec in T2.

### MINOR-4 — Z3 leg: corpus never exercises the w<8 / upper-bit SMT path; `easy-smt` is a dead dependency

Corpus (`bveq.rs:723-752`) is 4 pairs, all `w = 8`, so `build_smt_script`'s Flat(w) constraint and upper-bit disjuncts (`:681-702`) are dead code in the only test that runs them; and the differential never cross-checks the I4 obligation — the one invariant that most deserves an independent decision procedure. Also `Cargo.toml` declares `easy-smt = { version = "0.2", optional = true }` but no code references it (the test shells out to `z3` directly — fine, simpler); drop the dep or use it.

### NIT-1 — `VarMap::new` truncates: `(BITS * num_operands) as u16` (`bveq.rs:143`) silently wraps for `num_operands > 8191`; unreachable via `equiv` (which pins `NUM_REGS`) but it's a `pub` constructor in the TCB.

### NIT-2 — `bv_rotl_var`/`bv_rotr_var` hardcode `take(3)` against `BITS = 8` (`bveq.rs:298, 319`) with no tying assert — fine today, a landmine if BITS ever changes.

---

## Attack-item answers (A–F)

**A. Shared var-mapping.** Checked, holds *structurally*: exactly one `VarMap` is created per `equiv` call (`bveq.rs:585`) and used by every `expr_to_bdd` on both sides; the interleaved formula lives in one function (`:155`); `VarMap` is not injectable into `equiv`, so callers cannot split mappings. Two different denotations cannot compare equal under different mappings *through `equiv`* — and my experiment shows even a deliberately split mapping fails **safe** (spurious reject) because the full identity frame pins `Var(r,b)` on both sides. However: `bveq_mismap_negative` has **no teeth against `equiv` regressions** (demonstrated — MAJOR-4), and out-of-range operands silently alias into the mapping (MAJOR-3), which is a real unequal-compares-equal vector. Ordering pinned: yes, interleaved, documented and single-sourced.

**B. I4 obligation.** Both halves present (`bveq.rs:604-627`): low-w equality under the constraint AND upper-bits-false — but only on side `a`. `bveq_i4_must_reject_add_flat4` **has teeth**: with the upper-bit branch deleted, it flipped to FAILING (assert at `tests/bveq.rs:109`), then passed again after revert. Width-escapes found: (1) argument swap (MAJOR-1, demonstrated `Ok`); (2) the global-constraint escape (MAJOR-2, demonstrated `Ok` + concrete corruption); (3) a spec with junk upper bits is silently normalized rather than rejected (folded into MAJOR-1's fix).

**C. Full-frame + register-pure gate.** The frame is genuinely full: `equiv` iterates all `NUM_REGS × BITS` (`:600-601`), asserts 8-register transfers, `RegTransfer::identity()` seeds untouched registers, L-a compares all 8 output registers. Non-register-pure rejection is genuine, not asserted: `apply_instr` has an explicit match arm returning `Err(NonRegisterPure)` for Xchg/Swapbr/Mexch/MexchReg (`:521-527`), exercised by `bveq_reject_non_register_pure`. An out-of-frame register side effect cannot slip through at `w = 8`; at `w < 8` it can when conditioned on out-of-frame upper bits (MAJOR-2, demonstrated). Non-register quiescence is checked in L-b for BR/scratch/mem but **not IR/PC** (MINOR-2); on the symbolic side it is enforced by the gate itself (nothing non-register is modeled), which is sound.

**D. Encoder soundness.** Read line-by-line and reconciled: ADD ripple-carry (`:232-249`) and SUB `a + !b + 1` (`:252-255`) textbook-correct; constant rotates are correct wire permutations (`:274-289`, directions verified against `u8::rotate_left/right` semantics); variable rotate is a correct 3-stage barrel (1/2/4) over `rs`'s low 3 bits, matching `exec_instr`'s `& 7` (`compile.rs:2963-2970`), amounts 8..255 exercised by L-a's full domain; Cswap models both branches via mux with pre-state clones (`:465-480`); Ccnot reads rs/rt pre-state (`:481-490`), target-position-independent by construction. Aliasing is modeled by concrete indices and matches `exec_instr` — but the ISA rejects aliased operands, so this is agreement between fictions (MAJOR-5). `equiv` does NOT satisfy never-panic: OOB register index panics, OOB `Var` silently aliases or panics depending on the bit, `w > 8` asserts, deep exprs overflow the stack (MAJOR-3, all demonstrated).

**E. Reconciliation completeness.** L-a covers **all 13** register-pure instructions at genuinely full domains: 2-op at 256², 3-op at 256³ (16.7M each, real — the suite ran 112 s), immediates at 256×256 including all rotate amounts; compares the full 8-register post-state both sides. No instruction silently skipped. Gaps: selectors fixed at (0,1)/(0,1,2) — acceptable for software-vs-software since both are index-keyed, but aliased selectors are only cross-checked for Xor/Add. L-b default covers all 13 instructions at one (selector, value) point each with varied selectors across instructions; the wide `#[ignore]` sweep is real (7×49 + 4×343 = 1715 netlist evals ≈ 2.6 h at the measured 5.56 s/eval) but has the systematic selector holes and missing immediates of MINOR-1. Not run here per instructions.

**F. Reject-don't-corrupt.** Unequal → `Err(Counterexample)` with a witness that T1 verifies concretely — holds. Unsupported instruction → `Err` — holds. No-spec path — not applicable until slice-2. Encoder edge cases — **fails**: panics and silent aliasing instead of `Err` (MAJOR-3). `reject_non_register_pure` is genuine (real error arm, real test).

---

## The design-review's 5 required fixes — implemented correctly?

| Fix | Status |
|---|---|
| **BLOCKER-1** (re-sliced reconciliation: L-a full vs `exec_instr`, L-b sampled vs netlist) | **CORRECTLY IMPLEMENTED.** No infeasible full-domain netlist leg; L-a is truly full-domain for all 13 instructions; L-b bounded (~72 s measured within the 112 s suite) with an honest `#[ignore]` wide sweep. Caveat: selector strata holes + no IR check (MINOR-1/2). |
| **BLOCKER-2** (corrected slicing: additive, no `recognize_prim` change, honest headline) | **CORRECTLY IMPLEMENTED.** Diff touches only `bveq.rs` + tests + `Cargo.toml`; no `compile.rs` change; `biodivine-lib-bdd = "=0.6.3"` pinned; `z3-crosscheck` non-default. Caveat: T2's "proven equivalent" is reflexive (MINOR-3). |
| **MAJOR-1** (I4 output half + must-reject test) | **PARTIALLY.** Implemented with proven teeth in the documented direction, but one-sided — argument swap defeats it (my MAJOR-1, demonstrated). Must check both sides before slice-2. |
| **MAJOR-2** (full frame + register-pure gate) | **IMPLEMENTED**, gate genuine — but the global Flat(w) constraint undermines the frame guarantee at w<8 (my MAJOR-2, demonstrated escape), and L-b omits IR/PC. |
| **MAJOR-3** (single pinned mapping + mis-map negative + never-panic) | **PARTIALLY.** Mapping: correctly implemented, structurally enforced, fail-safe. Negative test: does NOT exercise `equiv`'s discipline (my MAJOR-4, demonstrated). Never-panic: **violated** (my MAJOR-3, demonstrated). |

---

## Test-teeth results (definitive experiments, all reverted)

1. **`bveq_i4_must_reject_add_flat4` — HAS TEETH.** Deleted the `bit >= w` upper-bit branch in `equiv` → test **FAILED** (`panicked at tests/bveq.rs:109: I4: byte-Add MUST be rejected at Flat(4)`). Reverted → passes.
2. **`bveq_mismap_negative` — NO TEETH against `equiv`.** Rewired `equiv`'s b-side through a shifted operand mapping (the exact `Var(1)→Var(0)` collision the test describes) → test **STAYED GREEN** (spurious inequality on the identity frame keeps `is_err()` true for the wrong reason).
3. Plus four adversarial demos (temporary test file, deleted after): I4 argument-swap → `Ok` (escape confirmed); `symbolic_exec(rd=8)` → panic confirmed; `equiv(Var(20,0) vs Var(4,2))` → `Ok` (silent aliasing confirmed) while `Var(20,7)` panics; global-constraint escape → `equiv(…,4) = Ok` with concrete r0 corruption at `r3=0xF0`.

## Test output observed

- Default suite (release, nix): **`test result: ok. 12 passed; 0 failed; 1 ignored; 0 measured; 0 filtered out; finished in 112.33s`** (the ignored test is the wide netlist sweep, as specified; `bveq_reconciliation_lb_netlist_sampled` ran >60 s, consistent with ~13 × 5.56 s).
- Z3 feature: `cargo build -p theseus-tisc --tests --features z3-crosscheck` — **compiles clean** (4.33 s). `z3` 4.16.0 is on the dev-shell PATH, so I also ran it: `test bveq::z3_tests::t9_z3_differential_crosscheck ... ok` — **1 passed** (not merely compile-checked; the differential is live green).
- Worktree verified clean at HEAD `8220a38` after all experiments.

## Required before slice-2 rewire (each with an L20 reproducing test)

1. I4 obligation on **both** sides of `equiv` (kills the argument-swap escape and malformed-spec uppers).
2. Flat(w) constraint restricted to a caller-supplied operand set; all other registers fully free (kills the DEMO-C corruption certificate).
3. Range-validate register indices and `(operand, bit)` everywhere; all `equiv`/`symbolic_exec` failure paths return `Err` — no panic, no silent aliasing.
4. Reject alias-illegal instructions in `apply_instr`, mirroring `encode`.
5. Either a real `equiv`-level mis-map canary or an explicit comment replacing the toothless half of `bveq_mismap_negative`; plus the cheap L-b additions (rs=0 case, rt coverage, Cswap ctrl=0, immediates in the wide sweep, IR/PC asserts).
