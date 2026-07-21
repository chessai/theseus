# BVEQ slice-2 design review — rewire `recognize_prim` to bveq (adversarial, with PoC spike)

**Reviewer:** fresh-context adversarial design reviewer (Fable 5), did not author the design.
**Target:** `docs/scratch/bveq-slice2-recognizer-rewire-design.md` r1, @ `3f8a422` on `chessai/theseus-v2`.
**Spike:** throwaway worktree `/tmp/s2-spike` (removed after measurement); a `#[path]`-included
`#[cfg(test)]` module inside `compile.rs` with private access to `check_pattern_*`,
`value_to_leaf_bytes`/`value_from_leaf_bytes`, and `bveq`. All numbers below are measured, both
`cargo test` (debug — the gate's profile) and `--release`, single machine, single run.

## VERDICT: SHIP-WITH-FIXES

The goal is right and Q1 is fully affirmative — the bveq check **agrees with enumeration on every
representative arm (18/18)**, and it catches whole classes of bugs enumeration is structurally
blind to. But the mechanism **as drawn in §2 must not ship literally**, for three spike-proven
reasons: (1) the check is placed outside the `with_registers` paging bracket, where
`symbolic_exec` hard-rejects every scratch/mem-resident recognition; (2) the "operand-scoped
Flat(w) constraint" is the wrong constraint — the sound one is the table's **domain BDD**, and the
Flat(w) version demonstrably rejects correct code on the sum-carrier arms; (3) the per-recognize
minterm build costs **17–21 s per Flat(8) prim in debug** (the gate profile) — 13× the entire
existing enumeration check — which would blow up the suite. The spike also built and measured a
**decomposed check** (identity + support closure via cheap BDDs, then pointwise `eval_transfer`
over the table) that delivers the identical guarantee at **286 ms debug** — 68× cheaper than the
minterm build and ~5× cheaper than today's enumeration — with no `equiv` refactor needed. That is
the corrected construction (§"Corrected slicing" below).

---

## Q1 — does bveq agree with enumeration on every currently-recognized prim? **YES (18/18), spike-measured**

Harness: for each arm, build the `PrimDef` table from the arm's own lambda (verbatim from
`recognize_prim`), run the real `check_pattern_2reg/_3reg/_2leaf/_kleaf`/`find_constant_1reg`
(enumeration verdict), then build the prim's per-output-bit BDDs by minterm expansion over the
leaf-home registers (slice-2b `apply_gate` technique at scale) and compare against
`symbolic_exec(candidate)` per-bit **under the domain BDD** (OR of the table's input minterms),
full frame, shared `VarMap::new(NUM_REGS)`.

| case | entries | enum | bveq | agree | prim-BDD build (debug) | build (release) | compare |
|---|---|---|---|---|---|---|---|
| xorInto/F8 | 65536 | ACCEPT | ACCEPT | ✓ | 17419 ms | 1277 ms | 0.02–0.11 ms |
| addInto/F8 | 65536 | ACCEPT | ACCEPT | ✓ | 19807 ms | 1453 ms | ~0.1 ms |
| subFrom/F8 | 65536 | ACCEPT | ACCEPT | ✓ | 19733 ms | 1443 ms | ~0.1 ms |
| swap/F8 (Exch) | 65536 | ACCEPT | ACCEPT | ✓ | 17040 ms | 1258 ms | ~0.1 ms |
| rotl/F8, rotr/F8 | 65536 | ACCEPT | ACCEPT | ✓ | ~20850 ms | ~1550 ms | ~0.1 ms |
| xorInto/F4, swap/F4 | 256 | ACCEPT | ACCEPT | ✓ | ~37 ms | ~3 ms | ~0.1 ms |
| xorInto/Bit×Bit | 4 | ACCEPT | ACCEPT | ✓ | 0.4 ms | 0.04 ms | — |
| cswap/F4, ccnot-first/F4, ccnot-last/F4 | 4096 | ACCEPT | ACCEPT | ✓ | ~1330 ms | ~100 ms | ~0.3 ms |
| addi_5/F8, subi_7/F8, xori_A3/F8 | 256 | ACCEPT | ACCEPT | ✓ | ~15 ms | ~1.6 ms | — |
| 3cycle-fwd, 3cycle-inv / Sum(Bit,One) | 3 | ACCEPT | ACCEPT | ✓ | 0.25 ms | 0.03 ms | — |
| mde_spill_walk / Sum(N5,Bit) (kleaf, 13 instrs) | 7 | ACCEPT | ACCEPT | ✓ | 3.3 ms | 0.26 ms | 4.3 ms |

**No disagreement found** — no latent lambda bug, no `symbolic_exec` bug, across every arm shape
including the pinned 13-instruction kleaf walk (Xor/Xori/Cswap chain over 5 registers). Two
structural claims from §4 were also proven as BDD identities in the spike:

- `Flat(8)×Flat(8)` domain BDD `== mk_true()` — full product ≡ unconstrained frame;
- `Flat(4)×Flat(4)` domain BDD `==` the Flat(4) upper-bits-zero constraint on exactly the operand
  registers — so for **full-product Flat(w) types** the design's Flat(w) constraint and the domain
  coincide, and there the drawn mechanism is sound.

**Bonus finding (why the rewire is worth doing at all):** enumeration validates the TABLE against
the lambda; it never looks at the emitted instructions. The lambda↔instruction correspondence is
pure trust today. The spike's teeth: `Add` emitted for a xorInto table → bveq REJECT; **transposed
rd/rs** (`Xor r0,r1` instead of `Xor r1,r0`) → bveq REJECT. Both sail through `check_pattern_2reg`
unchanged. bveq is not a redundant second vote — it closes the recognizer's one unverified edge.

## Q2 — `operand_regs`/`w` derivation. **TYPE-derived is available, but ONLY inside the closure; break-case built and measured**

- At the call site, the leaf→register assignment is `reg_for` computed by `with_registers` from
  `ctx.homes` (the canonical TYPE-side layout, `home_of`) plus spare allocation for paged-in
  leaves. It is handed to the arm's closure as `regs`, with `regs[j]` = register of prim leaf `j`.
  Nothing about it derives from the candidate instructions. So yes: `operand_regs = regs`,
  widths from `def.input_ty`/`def.output_ty` leaves — available, but **only inside the `emit`
  closure** (see MAJOR-1).
- **The break-case, constructed and measured (Q2a/Q2b):** candidate
  `[Xor r1,r0; Ccnot rd=1,rs=3,rt=4]` — corrupts the result conditioned on scratch registers
  r3&r4. With the TYPE-derived domain (r3, r4 free): **REJECT** ✓. With a candidate-derived
  operand set (r3, r4 "look like operands" and get constrained the way an over-declared operand
  would): **FALSE ACCEPT** — measured, the corruption vanishes from the check entirely. This is
  slice-2b N5 made concrete against this exact design. I2 must be pinned with this test.
- **Width subtlety the design misses:** `w` is not one number. The recognizer accepts prims with
  `input_ty ≠ output_ty` (swap on `Flat(8)×Flat(4)` is a legal table today: `n_in == n_out` is the
  only structural gate), and sum carriers have per-leaf "widths" that aren't Flat at all. A
  single-`w` `equiv` call cannot express either. The domain-BDD formulation makes the whole
  question disappear: the input constraint is the table's input minterms; out-of-width output bits
  are never set in the table so equality-under-domain **subsumes I4 with no separate obligation
  and no `w` parameter**. Under the decomposed check (below) the same holds.

## Q3 — perf of the 65536-entry minterm build. **Measured: NOT acceptable as an unconditional per-recognize cost**

Head-to-head on addInto/Flat(8), same process:

| | debug (gate profile) | release |
|---|---|---|
| table construction (`BTreeMap` of thermometer Values) | 4425 ms | 2158 ms |
| existing enumeration check (`check_pattern_2reg`) | 1460 ms | 772 ms |
| **prim-BDD minterm build** | **19393 ms** | **1495 ms** |
| bveq BDD compare (after build) | 0.55 ms | 0.08 ms |
| **decomposed check (corrected construction, total)** | **286 ms** | **201 ms** |

The minterm build is 13× the entire existing enumeration check in debug, ~2× in release, and it
runs **per recognition** (per `Prim` node per compile — the suite compiles hundreds of programs).
Wiring it unconditionally into the live recognizer is a suite blow-up and a gate-timeout hazard
(the gate has no timeout — the 3h lesson). 3-op Flat(4) is 1.33 s debug per recognition — also not
free. Compare time is trivially cheap everywhere; **the build is the entire cost**. Mitigations in
order of preference: (1) the decomposed check (below) — eliminates the build from the live path
entirely; (2) if the minterm path is kept live anyway, cache keyed by a content fingerprint of the
byte table (one O(table) pass; `PrimDef` pointer identity is not stable); (3) tier-gate the ≥F8
minterm cross-check into a `#[ignore]`/nightly test.

## Q4 — which arms convert; is the partial rewire right? **Partial yes, but the boundary as drawn is misplaced**

- Under the design's Flat(w)-constraint mechanism, the convertible set is **only** the
  full-product arms: 2reg (Flat/Bit pairs), 3reg at ≤Flat(4), the 1-reg immediates. The
  **2-leaf sum-carrier arms (3-cycle fwd/inv) are inside the `n_in == 2` branch but are NOT
  full-product** — `Sum(Bit,One)` has 3 canonical byte vectors out of 4; the candidate computes
  `(a⊕b⊕1, a)` at the unreachable `[1,1]` too, where the minterm BDD is 0. Spike-measured: under a
  Flat(1)-style constraint the correct 3-cycle candidate is **REJECTED**; under the domain BDD it
  is **ACCEPTED**. §4 never mentions these arms — as drawn, a naive "convert the n_in==2 branch"
  implementation bricks every Trace-carrier compile, loudly (fail-closed) but wrongly.
- Under the domain-BDD / decomposed formulation, **everything register-pure converts**, including
  the 2leaf arms and the pinned kleaf walk (both spike-verified ACCEPT). The only arm that cannot
  convert is **memexch** — `MexchReg` is non-register-pure; `symbolic_exec` rejects it
  (spike-confirmed). It keeps its existing F-3 semantic verify (which is already a full-domain
  table check, not a lambda). 3-op Flat(8) needs no policy: the table cannot be constructed
  (Wall 2), so the recognizer never sees one.
- So: partial rewire yes, but slice by **register-purity** (everything except memexch), not by
  "structural 2-op/3-op vs kleaf".

## Q5 — is the `equiv_bdds` refactor a prerequisite? **For the minterm path yes — and bigger than slice-2b described; for the corrected construction, NO refactor at all**

- Confirmed: `equiv` takes `RegTransfer` (BvExpr) on both sides and builds its own
  `VarMap::new(NUM_REGS)` internally. The prim-from-table side is raw `Bdd`s; there is no
  `RegTransfer` for it. If the minterm path is wired, the refactor must expose
  `equiv_bdds(a: &[Vec<Bdd>], b: &[Vec<Bdd>], constraint: &Bdd, map: &VarMap)` — note it must take
  a caller-supplied **constraint BDD** (the domain), not just `w` + `operand_regs`, per Q2/Q4.
  That is more than slice-2b MAJOR-3's sketch.
- **Do not** take the tempting no-refactor shortcut of encoding the prim table as a `BvExpr` DNF
  and calling `equiv` as-is: a 32768-term Or-chain is an unbounded-depth `BvExpr`, which is the
  **one documented abort path** in `expr_to_bdd`/`eval_expr` (stack overflow) — currently
  unreachable from `symbolic_exec` and this would make it reachable from the live recognizer.
- The decomposed check needs no refactor: it uses only existing pub API (`symbolic_exec`,
  `expr_to_bdd`, `VarMap`, `eval_transfer`, `Bdd::support_set`).

---

## Findings by severity

### MAJOR-1 — the check must live INSIDE the `with_registers` closure; as drawn it breaks every spilled-operand recognition
§2 says "build the candidate's BDD via `symbolic_exec(proposed_instrs)`". The sequence
`recognize_prim` returns is `page_in ++ body ++ page_out`, and the paging instructions are `Xchg`
(scratch homes) and `Mexch` (mem homes) — both **non-register-pure**; `symbolic_exec` returns
`Err(NonRegisterPure)` on them (spike-confirmed for both). Any prim with a leaf homed in
scratch/mem (leaves ≥ 8, resident arrays, spill layouts) would fail closed on correct code — or
the implementer would "helpfully" strip the paging and check the wrong thing. Fix: run the check
on the closure's `body` against `operand_regs = regs`, inside `emit`, before returning. The paging
bracket's own correctness is out of scope (it is the transient-squat lemma, unchanged by this
slice) — state that explicitly in the design.

### MAJOR-2 — the constraint must be the domain BDD, not "the operand-scoped Flat(w) constraint"
Spike-proven equivalent for full-product Flat(w) types (BDD-identity checks) and spike-proven
**wrong** for the sum-carrier 2leaf arms (correct candidate rejected at "w=1"; accepted under
domain). The domain BDD is the OR of the table's input minterms, built in the same pass as the
output-bit BDDs, and it makes I4 a corollary (table outputs never set out-of-width bits, so
equality-under-domain forces candidate upper bits to 0 under the constraint) and dissolves the
mixed-width / `input_ty ≠ output_ty` gap (Q2). Domain node counts measured tiny (2–44 nodes).

### MAJOR-3 — perf: the minterm build cannot run unconditionally per recognition (Q3 table above)
17–21 s debug per Flat(8) recognition, 13× the whole existing check, in the gate's own profile.
Adopt the decomposed check for the live path; keep the minterm/`equiv_bdds` machinery as the
tier-gated exhaustive cross-validation and as the slice-3 substrate.

### MAJOR-4 — cross-check "agreement" semantics are mis-specified in §3
Enumeration and bveq check **different propositions**: enumeration = table ≡ arm-lambda; bveq =
candidate-instructions ≡ table. "Both must agree" is really "enumeration-accept ⇒ bveq-accept".
A bveq-reject after an enumeration-accept is not necessarily a bveq TCB bug — it is exactly how a
real emission bug (transposed operands, aliased homes, paging mistake) is supposed to surface.
Two specification fixes: (a) on disagreement, return a hard `CompileError` immediately — do
**not** fall through to later arms (recognize_prim is a first-match chain; falling through would
let the same table match a weaker arm and emit under less scrutiny); (b) the error must carry the
counterexample and distinguish "candidate ≠ table" from harness errors, so a disagreement is
triaged as {bveq bug | latent recognizer bug} rather than auto-blamed.

### MEDIUM-1 — I3 is not literally true: two today-accepted behaviors become compile-time rejects
(a) Aliased homes: today `recognize_prim` with two leaves homed in one register happily emits
`Xor r0,r0` from the xorInto arm; only `asm::encode`'s `check_distinct` catches it downstream as
an `AsmError`. `symbolic_exec` rejects it at recognize time (spike-confirmed) → earlier failure,
different error type. This is strictly better (the Exch arm already does this manually) but it is
a behavior change and the existing `recognize_prim_aliased_registers` test's sibling scenarios
must be re-pinned. (b) Any latent emission bug becomes a `CompileError`. Neither changes emitted
code on the accept path — goldens stay untouched (all 18 agreement cases confirm) — but the design
should claim "zero codegen change on accepts", not "no behavior change".

### MEDIUM-2 — I2 pin: the over-declaration false-accept must ship as a test
The spike's Q2b construction (`Ccnot rd=1,rs=3,rt=4` corruption hidden by constraining r3/r4) is
the reproducing test for the sole live false-accept vector. It fails if anyone ever derives the
operand set from the candidate; it must land with the implementation (L20).

### MINOR-1 — `find_constant_1reg` synergy
The immediate arms derive K by brute force and the candidate embeds the same K; bveq re-verifies
candidate-with-K against the table, so a K-derivation bug is caught. No action; worth a comment.

### MINOR-2 — existing recognizer cost context
Today's Flat(8) recognition already pays ~1.5 s debug in `check_pattern_2reg` (plus 4.4 s table
construction upstream). The decomposed check (286 ms) is *cheaper than the check it cross-checks*;
if the cross-check is ever relaxed post-burn-in (design §3 "later cleanup"), the enumeration side
is the one to retire on cost grounds too.

---

## Corrected construction (spike-built and measured): the decomposed check

Replace §2's live-path mechanism with, inside each arm's `with_registers` closure
(`regs` = leaf homes, `body` = the arm's instructions):

1. `symbolic_exec(body)` — fail closed on non-register-pure / aliased / out-of-range (this alone
   upgrades aliasing and paging mistakes to recognize-time rejects).
2. **Frame closure (symbolic, table-free, cheap):** for every non-operand register bit, its BDD
   must equal its own identity variable; for every operand-register output bit, its BDD's
   `support_set()` must be ⊆ operand-register variables. One shared `VarMap::new(NUM_REGS)`.
3. **Pointwise table check:** for every `forward` row, `eval_transfer(candidate, operand bytes)`
   must equal the row's output bytes. By step 2 this is total over the whole frame (outputs cannot
   depend on anything the row doesn't fix; nothing else moves).
4. Keep the enumeration arm check as the cross-check (I4), with MAJOR-4's disagreement semantics.

Spike evidence: identical verdicts to the BDD-under-domain check on the agreement cases and on all
four teeth (wrong-op, transposed rd/rs, non-operand corruption, r3&r4-conditioned corruption);
sum carriers and the kleaf walk work with no special-casing and no width parameter; Flat(8)
worst case 286 ms debug / 201 ms release, total. This is "retire the recognizer's lambdas in favor
of `symbolic_exec`" — the design's §1 goal — with the BDD machinery used where it is cheap
(closure/support) and the table used where it is the spec (pointwise), which it remains until
slice-3. The full minterm → `equiv_bdds` path stays as a `#[ignore]`/tier-gated exhaustive test
(it re-proved itself here at 65536 rows) and becomes the live path in slice-3 when the table side
is replaced by a symbolic spec and the per-recognize build disappears.

Slice-2 success criteria, amended: all of §8 plus (a) the check runs inside the paging bracket
(MAJOR-1 test: a scratch-homed leaf recognition passes), (b) the Q2b over-declaration test, (c) a
teeth test that a transposed-operand emission is rejected (the class enumeration cannot see), and
(d) suite wall-time delta measured and bounded before merge (MAJOR-3).

## Spike hygiene
Worktree `/tmp/s2-spike` (HEAD `3f8a422`): `crates/theseus-tisc/src/s2_spike.rs` (4 tests:
agreement+perf, enum-vs-bveq cost, decomposed check, teeth/Q2) + a 3-line `#[path]` hook in
`compile.rs`. All tests green in debug and release (`nix develop --command cargo test
-p theseus-tisc --lib s2_spike`). Worktree removed; main repo untouched apart from this review.
