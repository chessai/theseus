# Adversarial review — compositional-control trio (A-3 / A-5 / A-4)

**Reviewer:** fresh-context adversarial agent (different model from the Opus-5
implementers). **Scope:** commits `39a6911` (A-3 tree-WCET fold), `fc9a664`
(A-5 occ-aware frame stack), `3f849e9`+`86d58a8` (A-4 reject deletion +
tests), against `docs/scratch/compositional-control-design.md`. `1515b93`
(loose-Sum prim fix) was skimmed for interaction hazards only (none found —
BDD-synthesized prims are Swapbr-free straight-line; `with_registers`'
transient-squat is a self-inverse Xchg bracket, value-preserving even over
live registers).

**Method:** read every changed control path in `compile.rs`; hand-derived the
soundness argument for `CostTree::extra` and the s_max Σ-fold; enumerated
every construct that can *reach* `lower_trace` /
`lower_data_dependent_iter` / `lower_bare_plus` / `lower_controlled_idiom`
after the deletions; wrote 9 executable probes
(`crates/theseus-tisc/tests/adversarial_probe.rs` + `adversarial_probe2.rs`,
left on disk, NOT committed) and ran them plus the full theseus-tisc suite
(both tiers).

## VERDICT

**NOT trustworthy to merge as-is. One reproducing BLOCKER** (silent wrong
output on programs the trio newly admits, confirmed by failing probes), the
HEAVY pre-merge tier fails 5 tests including the F-1 pin that documents the
BLOCKER (Finding 2c), one latent MAJOR under-count hazard with no tripwire
(2a), and the R1 "come-from join composes" claim is falsified-by-analysis on
any wider register file (2b). The A-3 fold itself and the A-5 frame allocator
are sound as far as I could push them; the hole is what A-5 did **not** make
occ-aware before A-4 deleted the guard that covered it.

Fast-tier suite status: everything green EXCEPT my 3 probe failures (below).
No regression to existing flat programs found (byte-identity debug_assert
live through the whole debug suite; lfsr/dot_reduce/exact_kmax pins
unchanged). Heavy tier (`--features heavy-tests`): 5 failures, Finding 2c.

---

## FINDING 1 — BLOCKER (correctness bug, reachable TODAY, silent)

**`lower_trace`'s tag register (and extra-payload padding homes) are still
POSITIONAL; A-5 made only the four aux occ-aware; A-4 deleted the occ-half of
F-1 that was the only thing protecting the tag.**

- `compile.rs:3351`: `let tag_reg_idx = n_input as u8;` — the comment says
  "first free reg" but the code is positional, never consults `ctx.occ`.
- `compile.rs:3364-3376`: extra payload homes `Reg(n_input + 1 + j)` — same.
- Old F-1 (`ctx.occ != OccSet::from_locations(&ctx.homes) ⇒ reject`) was
  precisely the guard for this; A-4 kept only the `homes != canonical` half.

Under `Times(Trace, g)`, `split()` gives the Trace `homes = Reg(0..n_input)`
(canonical ⇒ passes the kept guard) with `occ ⊇ {Reg(n_input), …}` = g's live
leaves. The tag lands **exactly on g's first live register, for every payload
width**. The prologue `XORI tag,1` turns sibling value `v` into tag `v^1`:
for `v=1` the machine believes it entered on the **Left/u arm** and the
body's arm-dispatch / exit-dispatch run against a wrong tag. The BR/s0/s1
terminal check cannot see it (the tag round-trips by the self-inverse
prologue/epilogue), so the output is **silently wrong**.

**Confirmed counterexamples** (all compile fine, then fast-oracle ≠ eval on
every input with sibling = Right; `cargo test -p theseus-tisc --test
adversarial_probe`):

| probe | program | result |
|---|---|---|
| probe1 | `Times(Trace(Bit, Plus(Id, flip)), flip)` — trace factor ≡ flip | 2/4 inputs WRONG (Id arm runs instead of flip) |
| probe2 | `Times(Trace(Bit, Plus(flip, Id)), flip)` — trace factor ≡ Id | 2/4 inputs WRONG (flip arm runs) |
| probe6 | wider payload `a = Bit×Bit`, `Times(Trace(Bit, Plus(Id, flip⊗Id)), flip)` | 4/8 inputs WRONG |

**Why the shipped A-4 suite is green anyway (the coverage hole):** the R2
test uses `body = Id` — with an empty body the ctrl corruption at the top
pair cancels against the bottom pair and the tag round-trips, so the output
is accidentally correct. R3 uses `Plus(flip, flip)` — both arms identical, so
running the wrong arm is invisible. probe3 (a genuine 2-iteration back-edge
prim body under Times) is also accidentally green: the corrupted orbit lands
on the right answer for that particular bijection. Degenerate/symmetric
bodies are exactly the wrong test vectors for an arm-dispatch aliasing bug.

**Classification:** correctness bug (not just coverage gap). The trio's own
commit message claims "the F-1 aliasing hazard is closed structurally" —
false for tag/padding; only the 4-aux hazard was closed.

**Fix shape (NOT applied — review is read-only):** draw `tag` + extra-payload
padding from occ-free registers exactly like `allocate_aux_frame` (and mark
them into `body_occ` before drawing the frame, as `body_homes` already is).
This stays compatible with the k_max walk: the walk writes the tag via
`tag_reg_idx` and padding registers are zero in the walk state wherever they
live; occ-free ⇒ genuinely-zero at entry preserves the padding-is-zero
assumption that the positional rule silently violates under Times (a live
sibling in a padding slot is a second corruption vector of the same root
cause, for `|u| > |a|` carriers). For the canonical-unshared case the lowest
free registers are the old positional ones, so the A-5 byte-identity assert
still holds. **Per L20: probe1/probe2/probe6 must flip to green and ship with
the fix.**

---

## FINDING 2 — MAJOR (latent under-count, no tripwire; plus the R1 claim is false on a wider machine)

**2a. Iter's guarded unroll emits `D-1` ROM copies of a body lowered ONCE
into the parent meta** (`lower_data_dependent_iter`, `compile.rs:3154` +
emit loop at `3168-3176`). If the body ever contributes `loop_terms`, ONE
CostTree node stands for `D-1` physical loops → `wcet()` under-counts by
`(D-2)·extra(node)` → the exact L15-class park-wrap A-3 exists to prevent.
Contrast `lower_fold_unfold`, which lowers per-copy into `meta` (correct).

Today this is **unreachable**: a Trace in an iter body sees displaced homes
(`Reg(n_counter)…`) and rejects on the kept canonical-homes guard — verified
by probe4 (`Iter(Nat2, Trace(Bit, Plus(Id, flip)))` rejects with the
canonical-homes message). But the protection is an *accident of a different
guard*, and the design (§8, "walk's marshal==body-home equality") already
gives A-5-style pressure to relax canonical-homes later. There is no comment,
no `debug_assert!(body_meta-into-iter has no loop_terms)`, nothing that
would fail when someone makes it reachable.

Same analysis for `forward_skip_total` came out SOUND (the per-copy
`+= body_instrs.len()` outer-skip term dominates each copy's inner skips:
taken skips within one copy are disjoint and sum below `body_len`), so only
`loop_terms` scaling is the hazard.

**2b. The R1 deletion's own claim doesn't survive the register file
growing.** For a nested Trace reachable via a Plus arm whose slice is
`[Reg(0)]` (the R1 test shape), the inner `tag_reg_idx = n_input = 1` is the
**outer Trace's tag register** — positional, Finding-1's root cause one
level down. On NUM_REGS=8 every such nesting happens to die at the
`allocate_aux_frame` reject first ("needs 4 free registers but only 0 free" —
I enumerated the register arithmetic for the minimal shapes, including
allocate_aux displacement variants; the frame check always fires first). So
"nested Trace composes" is protected today by *register exhaustion*, not by
the frame discipline: widen the register file (a stated near-term plan) and
nested Trace compiles with the inner tag aliasing the outer tag. Fix
Finding 1 and this collapses into the same repair; until then the R1 commit
message overstates what was proven.

---

## FINDING 2c — MAJOR (regression exposure): A-4 leaves the HEAVY test tier broken, including the F-1 pin that documents Finding 1

`tests/compile.rs` is `#![cfg(feature = "heavy-tests")]` — the tier the fast
gate skips and **the advisor runs pre-merge**. The trio never ran or updated
it. Under `--features heavy-tests`, **5 tests fail** on this branch:

```
reject_nested_plus_in_plus        (now compiles — A-4's intent, test stale)
reject_nested_plus_in_iter        (same)
mdd_reject_swapbr_body            (same, R3)
mdd_reject_nested_trace           (still rejects, but via the frame message —
                                   the asserted error text changed)
mdd_reject_trace_under_times      (the F-1 PIN: "Trace under a Times sibling
                                   must be rejected (F-1: aux regs would alias
                                   g's data)" — NOW COMPILES)
```

`mdd_reject_trace_under_times` is not mere staleness: its comment documents
the exact hazard of Finding 1 ("hand-assigns its **tag**/payload/aux registers
WITHOUT consulting ctx.occ … silently clobber … the BR/s0/s1 terminal check
cannot see"). A-4 deleted the guard this test pinned while fixing only the
aux third of the documented triple. Had the heavy tier been run, this review's
BLOCKER would have been flagged by the suite itself.

(probe file 2, `adversarial_probe2.rs`, runs the pin's exact program
`Times(Trace(Bit,3cycle):One↔One, Id(Bit))` end-to-end: it compiles, and is
**fast-green only because it is observation-degenerate** — the trace factor
has output type `One` and the sibling is `Id`, so the aliased-tag round-trip
is invisible in outputs. It belongs on the real-CPU list below for the
terminal check.)

Required: reconcile the heavy tier in the same change — flip the three
now-compile tests to positive tests, re-pin `mdd_reject_nested_trace` on the
new message, and keep `mdd_reject_trace_under_times` AS A REJECT until
Finding 1 is fixed (it is currently correct to demand rejection).

## FINDING 3 — MAJOR (verification gap): the fast-oracle blind spot, and what the advisor's real-`system_step` authority MUST cover

`run_on_cpu_fast` shares **three** things with the compiler:
`exec_instr` (also the k_max walk's semantics), the **same `wcet()` budget
B**, and marshal/unmarshal layout. It performs **no P2 park guard and no
P1/P3 terminal check** (`compile.rs:4278-4302` just runs B steps and
unmarshals). Therefore `fast == eval` CANNOT certify, for the newly admitted
nested programs:

1. **B truncating only the uncompute tail (the sharpest blind class).** Every
   Trace/skip-block puts its cleanup (burn `SUBI temp,δu`, `XORI tag`,
   skip-close `CSWAP`/`SUBI`, page-out `Xchg`) at the END of its bracket. If
   the A-3 fold is short by a few steps (an off-by-N in LOOP_OVERHEAD
   accounting at some nesting level, a missing spine term), the outputs are
   already in their homes when the budget runs out — fast unmarshals the
   right answer and **passes** — but BR/temp/scratch are non-zero. Only
   `run_on_cpu`'s terminal check (`br/s0/s1 != 0 ⇒ error`) sees this, and a
   non-zero parked BR is precisely the park-liveness corruption vector.
2. **exec_instr vs netlist divergence on the nested instruction mix.** A
   wrong byte-semantics in exec_instr (Swapbr/Cswap corner, operand-crossbar
   index) poisons the k_max walk, B, and the fast sim **identically** — a
   three-way-consistent wrong answer. Only the netlist is independent.
3. **P2 itself.** fast never evaluates `budget + s_max > 256`; only the real
   path proves the program is admissible at all.

**Advisor MUST run, pre-merge (real `run_on_cpu`, whole domain — all are
≤ 8 inputs):**

| program | why |
|---|---|
| R3 `Trace(Bit, Plus(flip,flip))` (2 inputs) | the ONLY running loop-with-inner-Swapbr — the trio's core new admission; exercises fallback-k_max B + Σ s_max + frame + terminal check across a real back-edge. **probe7 in adversarial_probe.rs, launched by this review** |
| R4 `Iter(Nat2, Iter(Nat2, flip))` (8 inputs) | nested skip-blocks + allocate_aux displacement (this shape pages a leaf to scratch on the 8-reg file) → exercises Xchg/Mexch in the netlist bus path. **probe8, launched** |
| R6 `Plus(Id, Plus(flip,flip))` (6 inputs) | controlled-idiom occ-marking path |
| R5 `Plus(Plus(flip,flip), Plus(flip,flip))` (8 inputs) | branch-in-branch; adjacent Swapbr pairs in ROM |
| AFTER the Finding-1 fix: probe1/probe2/probe6 programs (4-8 inputs each) | the Times(Trace,g) family with NON-degenerate bodies — the exact re-admitted F-1 scenario. Do NOT accept a degenerate (Id/symmetric) body as evidence again |
| probe9 program `Times(Trace(Bit,3cycle), Id(Bit))` (2 inputs) | fast-green but observation-degenerate: only the real path's BR/s0/s1 terminal check can certify the aliased-tag orbit parks clean |
| For R3 additionally | `eval^R` reverse + bus-quiescence per the M-D.e precedent (the come-from bracket self-inverse claim, design §5.1) |

Check per run: output == eval (the real path already enforces P2 up front and
BR=0/s0=0/s1=0 at exit — that terminal check IS the added value; a green
fast run proves none of it). Whole-domain automatically includes the
shortest-trajectory input (max park window) and longest (max live steps).

What real-authority CANNOT yet cover, honestly flagged by A-4 already: a
nested *Trace* with a live outer δu across an inner back-edge — unreachable
on 8 registers (rejects). Re-run this review's Finding-2b analysis before
any register-widening slice re-opens it.

---

## FINDING 4 — A-3 fold soundness: NOT breakable (analysis + probes)

I tried to construct an under-count and could not, given what is reachable:

- `extra(node) = (k_max−1)·iter_len + k_max·Σ extra(child)`: per outer
  iteration the true cost is ≤ `iter_len + Σ extra(child)` (iter_len counts
  every inner body straight-line once; skips only shorten; each inner loop's
  surplus is ≤ its own sound `extra`). Summed over ≤ k_max iterations and
  adding the ROM base, `B_fold ≥ true_B` at every level. Verified the
  single-level collapse is byte-identical (suite pins: lfsr=104,
  dot_reduce=34, walk trace 24; all green).
- **The walk-fallback trigger is complete**: `body_has_inner_control =
  !body_meta.loop_terms.is_empty() || has_inner_control(&body_instrs)`.
  Every loop emitter leaves a Swapbr in the instrs; every Trace leaves a
  loop_term; a Plus/controlled arm that is all-nop emits nothing (and then
  straight-line walk is exact). No reachable emitter produces control the
  trigger misses.
- **body_meta merge cannot drop a term** on reachable shapes: Plus /
  controlled / iter / fold thread the SAME `meta` into sub-lowering, and the
  only place a fresh meta is created is `lower_trace`, which merges both
  fields back (`compile.rs:3680-3683`). The dangerous case (a dropped or
  under-multiplied term under Iter's D-1 unroll) is Finding 2a — latent, not
  reachable.
- **s_max stays a Σ-fold (MAJOR-5 respected)**: `lower_bare_plus` sums both
  arms (`3920/3926`), iter sums per copy (`3173`), trace merges the body's
  total once (`3680`) — and once is CORRECT for a loop body: a taken forward
  skip inside an iteration substitutes for stepping, it does not add net PC
  displacement; the only S_max-relevant effect is T-reduction in the final
  pass, bounded by one ROM traversal. Park arithmetic
  (`PC_final ≤ B + S_max`) stays sound.
- The pinned BLOCKER-2 counterexample (4/100/12 → extra 4788 vs flat ~88) and
  the 2/3-level multiply white-box tests are correct and meaningful.

## FINDING 5 — graceful roads: hold, with one bad error message (MINOR)

- probe4 `Trace`-under-`Iter` → clean canonical-homes Deferred. ✓
- probe5 `Times(flip, Trace)` (right side) → same. ✓ (Note the asymmetry:
  only left-of-Times Trace is admitted — fine, fail-closed.)
- R1 depth-2 Trace → clean "aux frame needs 4 free registers but only 0
  free". ✓ No panic anywhere across occ states I could construct.
- **MINOR:** R3b (`Iter`-under-`Trace`) rejects via `allocate_aux`'s
  insufficient-slots path with `TooManyLeaves{needed:3, max:16}` → prints
  *"program needs 3 leaves but maximum is 16"* — semantically false (3 < 16)
  and unrelated to the actual cause (no free slots / nothing displaceable).
  Reject-don't-corrupt holds but the message will mislead the next debugging
  session. (`compile.rs:711-716`.)

## FINDING 6 — MINOR: `max_skip_distance`'s non-nesting assumption is now false

`compile.rs:4049` pairs *consecutive* Swapbrs and its doc says "skip blocks
don't nest" — A-4 makes nested skip blocks real (R5 ROMs), where consecutive
pairing UNDER-counts (open₁,open₂ pair across the gap). Production uses the
compositional `forward_skip_total` (correct), and the function is documented
as a test-helper for hand ROMs — but any future `run_rom_on_cpu` caller that
derives `s_max` from a scan of a NESTED rom gets an unsound park bound.
Update the doc comment / assert non-nesting.

## FINDING 7 — NIT

- The A-5 byte-identity `debug_assert` is debug-only (fine — the test suite
  runs debug; state it in the invariant list).
- `nested_aux.rs` exercises only the FRAME allocator; nothing white-box pins
  tag/padding placement — which is exactly how Finding 1 escaped. After the
  fix, add the tag/padding-disjointness case to that suite.

---

## Attack-surface disposition (per the review brief)

| attack | result |
|---|---|
| 1. A-3 under-count | not breakable on reachable shapes (Finding 4); LATENT under-count via Iter unroll meta-scaling (Finding 2a) |
| 2. A-5 frame disjointness | frame itself sound; **tag/padding NOT in the frame discipline → BLOCKER (Finding 1)**; `Times(Trace,g)` aliasing is REAL and reachable, probes 1/2/6 fail |
| 3. fast-oracle blind spot | enumerated (Finding 3): uncompute-tail truncation + exec_instr-shared semantics + P2/terminal checks; advisor program list given; probe7/probe8 started |
| 4. graceful rejects | hold everywhere probed; one misleading message (Finding 5) |
| 5. regression exposure | fast tier: none (green minus my probes; single-level ROM byte-identity pinned + asserted live). **HEAVY tier: 5 stale tests FAIL, incl. the F-1 pin (Finding 2c)** |

## Bottom line

- A-3: sound, keep.
- A-5: correct for what it covers; incomplete — tag + padding must join the
  occ-aware frame before A-4's relaxation is safe.
- A-4: deleting R3/R4/R5/R6 is adequately backed. Deleting **R2's occ-half is
  premature** (Finding 1) — either restore `occ ⊋ homes ⇒ reject` until
  tag/padding are occ-aware, or land the fix in the same commit as the
  re-relaxation, with probes 1/2/6 as permanent regressions. R1's deletion is
  currently harmless only because the frame reject fires first on 8 registers
  (Finding 2b) — do not carry that assumption into a wider register file.

Probes: `crates/theseus-tisc/tests/adversarial_probe.rs` +
`adversarial_probe2.rs` (on disk, uncommitted). probe1/2/6 = failing
reproductions of Finding 1; probe4/5 = graceful-reject pins; probe3/probe9 =
documented near-misses (accidentally/degenerately green shapes); probe7/8 =
real-`system_step` authority for R3/R4 (slow, `--ignored`; launched during
this review and still running at review close, ~21 CPU-min in — advisor to
confirm completion; they are the required real authority for the two shapes
that stay admitted regardless of Finding 1). Rerun:
`nix develop -c cargo test -p theseus-tisc --test adversarial_probe --release -- --ignored --nocapture`.

Also note for the merge decision: A-4's commit message says "the B_fold≤256
park-safety bound … stays", and the heavy tier is the advisor's pre-merge
gate — per Finding 2c that gate currently fails on this branch, so the trio
cannot merge on the fast tier alone even ignoring the BLOCKER.

---

# Fix re-check — occ-aware tag/padding (580015b → f7fc4de)

**Reviewer:** fresh-context adversarial agent (Fable — different model from the
Opus implementer of the fix). **Scope:** ONLY the Finding-1 BLOCKER fix
(`git diff 580015b f7fc4de -- crates/theseus-tisc/src/compile.rs`) + the
heavy-tier reconciliation, per the re-check brief: confirm the fix is sound and
introduced no NEW aliasing. READ-ONLY (probes run, nothing fixed, not committed).

**Method:** hand-traced the new draw order in `lower_trace`
(input-homes occ-mark → ancilla drain → carrier occ-mark → `allocate_aux_frame`)
against `OccSet`/`Ctx::split`/`allocate_aux`; enumerated every context shape
that can reach `lower_trace` post-fix; ran 12 fresh probes
(`review_probe_recheck.rs`, temporary, deleted after) + the trace-adjacent fast
suites; ran the reconciled tests against the PRE-fix compiler (580015b's
`compile.rs` under f7fc4de's tests) for teeth; byte-compared single-level ROMs
old-vs-new.

## Verdict: **SOUND — trustworthy to MERGE. No residual BLOCKER or MAJOR.**

## Check 1 — new aliasing from the occ-aware draw: PASS

No ordering hands the same register to two roles:

- **tag vs padding:** both come from a single ascending `drain` of the
  distinct-filtered free list — pairwise distinct by construction.
- **tag/padding vs the 4 aux:** all carrier homes (tag + inputs + padding) are
  occ-marked into `body_occ` BEFORE `allocate_aux_frame` draws — and the frame
  allocator itself rejects (`< 4 free`) rather than displace. Verified the
  4-register reservation arithmetic: `reg_budget_for_ancilla =
  free.len().saturating_sub(4)` guarantees ≥4 registers remain after the drain.
- **tag/padding vs siblings / outer aux:** `Ctx::split` copies the WHOLE parent
  occ into both halves, and `body_occ` starts from `ctx.occ` — sibling leaves
  (register AND scratch homes; `OccSet` tracks Scratch/Mem too) and any outer
  marks are excluded from the free pool. White-box probe: for
  `Times(Trace, flip⊗flip)`-class programs the prologue `XORI tag,1` targets a
  register outside {input, sibling} whenever compilation succeeds.
- **scratch spill:** padding spills to the LOWEST occ-free scratch ≥
  `SCRATCH_SPILL_BASE` via a distinct-by-iterator filter — occ-aware (the old
  positional `SCRATCH_SPILL_BASE + count` was not), and sibling scratch homes
  are in occ. NLL borrow order is clean (the filter's borrow of `body_occ` ends
  before the carrier occ-mark loop).
- **Reachability sweep:** post-fix, `lower_trace` is reachable only with
  canonical input homes — top level, left spine of `Times`, `Seq` composition,
  `Sym` (which re-lowers the ISO-level adjoint, no instruction inversion).
  Trace under `Plus`-arm / `Iter`-body / fold-body still rejects on the
  canonical-homes guard (probes P5/P7 confirm; reject message correct).
- **Temporal reuse:** returning `ctx.clone()` (original occ) to the parent is
  correct — tag/aux/padding are net-zero after the epilogue, so a later sibling
  re-drawing those registers is safe. Probe P8
  (`Seq(Times(Trace,flip), Times(Trace,flip))`, whole domain) passes.

Probes run (all green): P1 multi-leaf sibling; P2a padded-trace + sibling
(tag+padding register-resident, full 8-reg file, whole domain); P2b padded +
2-leaf sibling; P3a/P3b/P3c occupancy-exhaustion sweep; P4 nested `Times`;
P5/P7 non-canonical rejects; P6 padded single-level; P8 sequential traces; P9
ROM dumps.

## Check 2 — occ-marking completeness + ordering: PASS

Order is: input homes marked → tag+padding drawn → ALL carrier homes marked →
aux frame drawn AND marked → body lowered under the fully-marked occ. Every
register the loop skeleton touches (`tag_reg`, `rc/rb/temp/ctrl`) is drawn from
that sequence; the k_max trajectory walk uses the same `tag_reg_idx` (not the
old positional index), and its marshal==body-home equality is still guarded by
the canonical-homes reject. No path uses the tag before it is marked; no mark
leaks to siblings (see temporal-reuse above). Rejects exit before `meta` is
mutated, so no metadata pollution on failure paths.

## Check 3 — graceful-reject boundary: PASS

Every over-budget occ state I could construct rejects CLEANLY, never aliases,
never panics:

- tag cannot fit (occ {0..3}+, incl. sibling-in-scratch 9-leaf case):
  the new Finding-1 `Deferred` message, correct and specific (P3a/P3b/P3c).
- padding cannot fit register+scratch: the second new `Deferred` arm
  (constructed arithmetically; unreachable at MAX_LEAVES_LOOP=4 with 6 free
  scratch, kept as a boundary).
- body-`Plus` aux exhaustion under a ≥2-leaf sibling (P1/P2b/P4): rejects via
  `allocate_aux`'s insufficient-displaceable path. **Capacity, not
  correctness** — fail-closed on the 8-register file. NOTE this reuses the
  pre-existing misleading `TooManyLeaves` message (Finding 5, MINOR, still
  open — "needs 2 leaves but maximum is 16").

## Check 4 — single-level byte-identity: PASS (direct evidence)

Dumped and byte-compared the compiled ROMs old(580015b)-vs-new(f7fc4de) for
three canonical single-level shapes: `Trace(Bit, Plus(Id,flip))`,
the padded trace (u=Bit², a=Bit → 1 register padding), and the M-D.e
scratch-spill shape (payload 4 = 3 live + 1 spilled). **All three
byte-identical.** Arithmetic identity also holds in general: register-resident
padding count = `min(n_extra, 3 − n_input)` under both the old positional rule
and the new `min(1+n_extra, free−4)` drain for any canonical unshared ctx. The
`debug_assert` frame equality ran live through the whole dev-profile fast
suite (nested_aux 8/8, nested_programs 7/7, mde_spill 4/4, nested_wcet 6/6,
negative_guards 26/26 — all green).

## Check 5 — heavy-tier reconciliation kept its teeth: PASS (one observation)

Ran the reconciled/new tests against the PRE-fix compiler (580015b
`compile.rs` under the new test files). Results:

| test | on pre-fix code |
|---|---|
| `times_trace_nondegenerate_body_runs` (probe1/2 pin) | **FAILS** — cpu≠eval on `Pair(Left, Right)` (the aliasing-sensitive sibling=Right input) |
| `times_trace_wider_payload_runs` (probe6 pin) | **FAILS** — cpu≠eval on `Pair(Pair(L,L), Right)` |
| `times_trace_tag_does_not_alias_sibling_end_to_end` (nested_aux) | **FAILS** — cpu≠eval on `Pair(Left, Right)` |
| `r2_trace_under_times_compiles_and_runs` (de-degenerated) | **FAILS** — the de-degeneration worked; the old degenerate vector passed on broken code, the new one does not |
| `mdd_trace_under_times_compiles_and_runs` | **PASSES on pre-fix code** (release, real CPU, 1065s) — see below |

So the new `Times(Trace,·)` vectors are genuinely non-degenerate — they
reproduce the BLOCKER exactly (fail without the fix, pass with it): the L20
bar is met. The failing outputs are the textbook wrong-arm corruption
(`cpu: Pair(Left, ·)` vs `eval: Pair(Right, ·)` — the trace factor ran the
wrong arm exactly when the sibling was `Right`).

**Empirical correction to that test's own comment (MINOR finding of this
re-check):** `mdd_trace_under_times_compiles_and_runs` claims "we assert the
REAL CPU here" because "the aliased-tag orbit … would park a nonzero
BR/scratch that only the terminal check can see". Empirically FALSE for its
vector: on the pre-fix (positional-tag, aliasing) compiler the test **passes**
— real CPU, P2 + BR/s0/s1 terminal check and all. On the degenerate
3cycle/`Id(Bit)` shape the tag corruption round-trips (prologue/epilogue
`XORI` cancel and the `Id` sibling never reads its leaf mid-loop), so neither
output nor terminal state distinguishes broken from fixed. The test is still
worth keeping — it is the real-CPU pin that the newly-admitted
`Times(Trace,g)` shape runs and terminates cleanly — but it must NOT be
counted as Finding-1 regression coverage (fix its comment when convenient).
The Finding-1 teeth are carried entirely by the four tests above (all four
fail pre-fix, all pass post-fix). `mdd_reject_nested_trace` still asserts `is_err` with the
re-pinned frame-bound message (legitimate re-target — the reject moved from
predicate to structure). `reject_nested_plus_with_trailing` and the WCET
overflow pins are untouched.

**Observation (MINOR, no action forced):** `mdd_swapbr_body_compiles_and_runs`
is `One ↔ One` — output-observation-degenerate, so its fast oracle proves
termination/clean-unmarshal but cannot see a wrong payload; the real-CPU
authority for the swapbr-in-Trace shape is the prior review's probe7 (advisor
launched it; confirm it completed before relying on that shape in production
programs). Same for probe8 (nested-Iter displacement).

## Check 6 — MAJOR-2a tripwire: PASS

`lower_data_dependent_iter` now snapshots `meta.loop_terms.len()` before
lowering the body and returns a `CompileError::Deferred` if the body added any
loop term — a real reject, not a comment. Also verified the related
under-count surfaces: the guarded unroll adds `forward_skip_total +=
body_instrs.len()` PER COPY (and the outer skip region contains any inner
regions, so the Σ-fold stays an over-approximation — sound), and
`max_skip_distance` has NO production caller (test-only; production park
bounds come from `Lowered::s_max()`), so the Finding-6 doc-fence is accurate.

## Residuals (all MINOR, none blocking merge)

1. Finding 5's misleading `TooManyLeaves` message now also fronts the
   body-aux capacity reject for `Times(Trace, wider-sibling)` (P1/P4 shapes).
   Cosmetic; reject-don't-corrupt holds.
2. `mdd_swapbr_body`'s fast oracle is observation-degenerate (above) —
   probe7/8 real-CPU completion is the closing evidence for those two shapes.
3. `mdd_trace_under_times_compiles_and_runs` is NOT a Finding-1 reproducer
   (it passes on the pre-fix compiler, real CPU included — Check 5); fix its
   comment so nobody later deletes the four real reproducers believing this
   one covers them.
4. Capacity note for program authors: on the 8-register file,
   `Times(Trace(...,Plus-body), g)` fits only a 1-leaf sibling; ≥2 leaves
   rejects (fail-closed).

## Verification runs (this re-check, independent of the advisor's)

- Fix tree @ f7fc4de, heavy tier `--release --features heavy-tests --test
  compile --test-threads=1`: **67 passed / 0 failed / 23 ignored (1225.7s)** —
  independently reproduces the advisor's gate result.
- Fix tree, dev profile (debug_asserts LIVE): nested_aux 8/8, nested_programs
  7/7, mde_spill 4/4, nested_wcet 6/6, negative_guards 26/26, the 8
  fast-checkable reconciled heavy tests 8/8, and the 12 fresh probes 12/12.
- Teeth tree (580015b `compile.rs` under f7fc4de's tests), release:
  `times_trace_nondegenerate_body_runs` FAIL, `times_trace_wider_payload_runs`
  FAIL, `mdd_trace_under_times_compiles_and_runs` ok (1065.6s; see Check 5);
  dev: nested_aux tag-alias test FAIL, nested_programs r2 FAIL — 4 genuine
  Finding-1 reproducers confirmed.
- ROM byte-compare old-vs-new (single-level canonical: plain / padded /
  scratch-spill): byte-identical.

