# Adversarial design review — M-D.e slice 1 (loop-carrier scratch-spill)

**Reviewer:** fresh-context Fable (adversarial), did not write the design.
**Reviewed:** `docs/scratch/mde-spill-design.md` @ theseus-v2 e95e3b3 against
`crates/theseus-tisc/src/compile.rs` (`lower_trace`, `with_registers`, `allocate_aux`,
`home_of`), `src/step.rs`, `src/decoder.rs`, `tests/xor_reduce.rs`, `tests/compile.rs`,
`docs/scratch/{mdb2-f3,mdd}-design.md`. Every reversibility claim recomputed from source.

**Verdict: DO-NOT-SHIP (design revision required).** The core paging mechanism is sound
— the aux-zero invariant and the back-edge were independently verified and they hold.
But the design as scoped **cannot build its own minimal test** (F6: the §4 body is
inexpressible in the current Swapbr-free lowering vocabulary), the allocation section
omits three mandatory changes (F5), and the central safety argument is stated for the
wrong reason (F1b). Two BLOCKER, two MAJOR.

---

## F1 — aux-as-paging-spare (design §1, I2) — MAJOR: conclusion correct, argument wrong

**(a) aux = 0 for the entire body, both paths — VERIFIED YES.** Skeleton walk
(compile.rs:2206–2244, semantics step.rs:217–228). Forward entry: prologue + Ltop
join-dispatch leaves rc=rb=temp=ctrl=0 at body entry. Backward re-entry: Lend mints
rb=δu→BR, Ltop's `Swapbr rc`/`Cswap`/`Subi`/`Xor`/`Xori` re-zero all aux → body entry
iteration 2+ also rc=rb=temp=ctrl=0. ✓

**(b) Does `with_registers` pick an aux register? — REFUTED AS STATED.**
`with_registers` (compile.rs:453–489) is homes/occ/aux-blind: `spares =
(0..8).filter(not an operand of THIS instr)`, then `spares.pop()` = highest-numbered
non-operand register, whatever lives there. It lands on an aux **only because the
allocator packs aux into the top registers** (`aux_base = max_carrier_reg+1`, L2191) and
aux are never body operands — a **layout coincidence**, not construction. Also the
stated load-bearing condition ("safe only because aux=0") is wrong: the actual safety is
the **self-inverse exchange bracket** `Xchg spare,s_k; op; Xchg spare,s_k` — net-zero
for ANY spare value (transient-squat, compile.rs:444–447; `swap_homes` already relies on
it with a live `tmp=Reg(0)`). Aux-zeroness is true but NOT what makes paging reversible.
If the design wants "spare ∈ aux", it must be **asserted** (T1 checks the paging Xchg's
`rd` is an aux index), not assumed.

**(c) Re-entry point — VERIFIED.** Lend `Swapbr rb`(BR=δu) + same-cycle `pc_add_br;
pc_inc` → PC = Ltop mod 256. Re-enters exactly at Ltop, never mid-body; Ltop's `Swapbr
rc` re-zeroes BR first. ✓

**(d) Ordering that leaves aux non-zero at the back-edge — NONE FOUND.** Paging brackets
are emitted inside a single body-instruction lowering (page-in/op/page-out, L508–511);
between body instrs the machine is canonical and aux=0. ✓

## F2 — f3 d=4 precedent (design §1) — MAJOR: does not transfer; drop as a safety argument

xorReduce is `Iso::iter` (xor_reduce.rs:170) → `lower_data_dependent_iter` — **guarded
static unroll**: D−1 copies, each wrapped in a *forward* Swapbr skip-block. **No backward
branch, no δu token, no join-dispatch.** The d=4 displaced regime (Mexch paging with
spare=rb inside an executed guarded block) is real and green, but exercises NONE of what
makes `Trace` dangerous (PC wrap re-entry, token exactness, aux liveness across a jump).
Every §4 test compiles to `Trace` — no paging precedent at all. The precedent covers
"with_registers bracket inside repeated straight-line code," full stop; the back-edge
argument rests on the F1a/c/d skeleton walk, and T3/T5 are **first-of-kind**, not
confirmatory.

## F3 — Layout invariant I1 — VERIFIED, no finding
Guard (compile.rs:2152–2157) compares `body_ctx.homes` vs `body_ctx_out.homes` — static
declared homes threaded through `lower()`, not runtime state. `with_registers` takes
`&Ctx`, never mutates homes; Swapbr-free paths return `ctx.clone()`. A permanent
Scratch(2) home passes the guard automatically. Keeping the guard unchanged is correct.

## F4 — δu / iter_len I3 — VERIFIED, no finding
`body_len = body_instrs.len()` (compile.rs:2159) runs AFTER `lower(body,…)` (2141), so
paging Xchgs are counted; `iter_len`, `delta_u = 256−iter_len` (2180), `loop_terms`
(2249), WCET (2168), compositional wcet (1600–1610) all inherit it. No pre-paging length
anywhere. Add the §3.5 pinning assertion as cheap insurance.

## F5 — Allocation (design §3.1) — BLOCKER: the edit list does not produce a working allocation

For the §4 carrier `Sum(Bit,P4)` (n_input=4, n_carrier=5, n_payload=4):
1. **Unmentioned guard:** `total_regs_needed = n_carrier + 4 = 9 > 8` → Deferred at
   compile.rs:2093–2099, BEFORE the allocation code §3 edits. Must learn to count only
   register-resident carrier leaves. Missing from the edit list.
2. **Live-data relocation mandatory and absent.** Input arrives canonical (leaves 0–3 in
   Reg0..3, guard 2062). Homing leaf 4 in Scratch(2) means the live value in Reg(3) must
   be physically moved: spine `Xchg r3,s2` before the loop + inverse after, and the
   returned ctx must equal the input ctx. Design emits none → loop reads 0 from s2 while
   the datum sits in Reg(3) → silent corruption. Also breaks `spine = 4` (2167) and
   `debug_assert_eq!(instrs.len(), spine + iter_len)` (2252–2258).
3. **Register formulas overflow.** Naive edit keeps `tag_reg_idx = n_input = 4`; carrier
   regs {0,1,2,4} → max_carrier_reg=4 → aux=5,6,7,**8** → rejected at 2198 (ctrl ≥
   NUM_REGS). Tag must repack into the evacuated Reg(3).

**Constructive fix (recommended):** orient **u-wide** — |u| = 4 leaves, `a` small (u =
the 4-inhabitant nat chain ⇒ k_max=5, WCET roomy). Then the spilled 4th payload leaf is a
**zero-initialized padding leaf with no input home** (n_extra ≥ 1, L2122–2125): marshal
already zeroes Scratch(2), no relocation, `tag_reg_idx` unchanged, aux pack at 4–7. If the
design keeps `Sum(Bit,P4)`, points 1–3 must all be specified explicitly.

## F6 — Minimal slice (§4) — BLOCKER: the test body is inexpressible today; the fix collides with f3.5

The §4 body must be (i) Swapbr-free (`has_inner_control` reject, 2146), (ii) touch payload
leaf 4 of a 5-leaf **sum** carrier, (iii) a legal iso. No such body lowers today:
- `recognize_prim` tops at **3 leaves** (n_in∈{1,2,3} + the memexch resident special). A
  5-leaf PrimDef → Deferred.
- Payload-touching ops on a sum require `Plus` → `lower_bare_plus` emits a Swapbr
  skip-block for any non-nop arm → rejected inside a Trace body. Controlled-op idiom
  recognizers are product-shaped (Bit×T), not sum-tag-controlled.
- Remaining Swapbr-free sum vocabulary (`Id`, `SwapSum` = tag Xori only, identity-checked
  structural isos) **never touches a payload leaf**.
- "Flip the spilled bit" unconditionally is not even a legal iso on `Sum(Bit,P4)` (tag=Left
  ⇒ leaf 4 is a zero pad; flipping produces a non-canonical encoding). The legal version is
  tag-controlled = `Plus(Id, flip)` → Swapbr → rejected.

**Corollary:** `MAX_LEAVES_LOOP = 3` headroom has NEVER been exercised — every green Trace
test uses a 2-leaf carrier (n_payload=1: 3-cycle `Sum(Bit,One)`, `Sum(Bit,Bit)`,
tests/compile.rs:1219–1331). Slice 1 jumps from 1 payload leaf straight to 4.

**Consequence for scope (§3/§6/§7):** the fix needs a `recognize_prim` extension (a k-leaf
generalization of `check_pattern_2leaf` for one pinned linear map) — which §3 explicitly
excludes AND which lands in the **same function f3.5's CCNOT recognizer touches**,
collapsing §7's "non-overlapping functions" concurrency claim. **Re-scope:** either fold
the recognizer extension into this slice and **hard-serialize behind f3.5**, or split it
out as its own reviewed slice that merges first.

**Test-strength gaps even after the fix:**
- **T1** ("ROM contains `Xchg{_,sx≥2}`") can pass **vacuously** via a spine relocation Xchg
  while the body pages nothing. Require the paging Xchg to sit **between the Ltop and Lend
  offsets**.
- **T3** "≥2 iterations" must be **asserted** (check eval-side trajectory length, or use a
  body where every input takes ≥2 iterations) — an accidental k=1 body never takes the
  back-edge, the exact untested path.
- **T-reject(5)** fine (cap guard fires before body lowering). T3+T5 are the right oracles
  once the ≥2-iteration assertion is added.

## F7 — Everything else
- **MINOR (§0 factual error):** "decoder.rs skeleton hardcodes aux reg indices" is false —
  aux indices are computed per-program (`aux_base = max_carrier_reg+1`, 2185–2195);
  decoder.rs knows nothing of loop aux. Correct unpageability rationale is **liveness
  across the back-edge** (rc/rb/temp/ctrl carry the witness through the jump).
- **NIT:** if the cap grows past 4, canonical homes of leaf 8+ land at Scratch(2)
  (`home_of`) — colliding with SCRATCH_SPILL_BASE. Unreachable in slice 1; pin with a
  comment/guard.
- **WCET (I4):** fine — u=Bit k_max=3, u-wide k_max=5, both ample under 256.
- **I5 (k_max):** structural, unaffected (2036–2042). ✓
- **Slice order:** spill-before-widen-PC is defensible as a machinery proof; but given F6
  the honest minimal slice is "k-leaf body recognizer + spill." Decide before delegating.
- **Probe (§5):** well-conceived; would hit F5/F6 on step 2 — but a design review should
  predict these, not outsource them. Revise the design, then probe.

---

## SHIP / DO-NOT-SHIP

**DO-NOT-SHIP-UNTIL:**
1. **(F6)** §4 carrier/body replaced with a provably-lowerable one: pick the vocabulary
   extension (k-leaf linear-map pattern in `recognize_prim`), add it to §3's edit list, and
   re-plan §6/§7 (f3.5 collision — serialize or re-split).
2. **(F5)** §3.1 rewritten with the exact allocation: u-wide/padding-leaf orientation (no
   relocation — recommended) or explicit spine relocation + tag repack + the L2093 guard
   change + spine/debug_assert accounting.
3. **(F1b)** I2 rationale corrected: safety = self-inverse exchange bracket +
   spare-not-an-operand; aux-zeroness verified but not load-bearing; add the T1 assertion
   that the paging spare is an aux index if the property is relied on.
4. **(F2)** §1 demotes the f3 d=4 precedent to "bracket-inside-repeated-code"; the back-edge
   argument cites the skeleton walk.
5. **(F6 tests)** T1 constrained to the loop region; T3 asserts ≥2 iterations.

**SHIP-AS-IS (verified sound, keep):** the layout-invariant guard strategy (F3), the
iter_len/δu accounting (F4), the cap-by-one + T-reject discipline, k_max/I5, the
probe-first protocol, and the overall thesis that scratch-spill inside `lower_trace` is
achievable with net-zero paging — the mechanism survives adversarial recomputation; the
plan around it does not, yet.
