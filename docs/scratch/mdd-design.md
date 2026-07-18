# M-D.d — `Trace` as a reversible while-loop (backward jump + come-from)

**Status:** **IMPLEMENTED + MERGED** (theseus-v2 `6d180bf`). Design-reviewed (first draft
DO-NOT-SHIP → revised to the hand-verified §2 skeleton), then implemented, then a second
fresh-context adversarial review of the *implementation* (verdict DO-NOT-SHIP-as-drawn:
core loop correct, but a BLOCKER perimeter bug — a Trace under `Times` grabbed registers
without consulting `ctx.occ` and clobbered its sibling; fixed by an occ-equality guard +
regression test, plus real guard-test assertions, a leaf-count guard, and an
informative-output real-CPU test). Verified `run_on_cpu == eval` on the real `system_step`
iso (t=3 two-re-entry + informative `Bit↔Bit` traces) and `SystemRing` closure. ADR-0007
iteration bound corrected `|u|`→`|u|+1`. Deferred follow-ups: F7 (`AssocSum` nested-sum
no-op miscompile, pre-existing, avoided here via the prim witness), M-D.d2 (calling
convention). All ten design findings (F1–F10) were folded into §2–§8 below.

**Scope of the core delegation (one task):** compile the `Trace` combinator to the
TISC-1 CPU as a **reversible while-loop** (genuine backward branch + ADR-0034 come-from
join-dispatch). Two **prerequisite commits** (F7, F4/§7) land first — see §6.
`bare_trace_rejected` (tests/compile.rs:190) flips to accepted. The reversible **calling
convention** (CALL/RET) is a **separate follow-on M-D.d2**, out of scope here.

---

## 1. Why `Trace` is NOT guarded static unroll (confirmed)

`Trace(u, body)`, `body : u+a ↔ u+b`, evaluates (interp.rs:163-183): `s := Right(a)`;
loop `s := body(s)` until `s = Right(y)` (return `y`), feeding `Left(w)` back. The review
confirmed guarded unroll (`emit_skip_block`) **cannot** express it: the exit guard is the
sum tag the body **flips** on the exit iteration, so `emit_skip_block`'s cleanup CSWAP
(compile.rs:654-673) fires under a changed guard → corrupts aux even at tiny `|u|`. A
counter-in-data reformulation does **not** dodge it (per-copy guard snapshots are
unrecoverable ancillas needing a Bennett reverse pass). Loop-not-unroll is also right
spatially: one body copy in ROM, PC revisits it — the M-D.w case.

**Totality bound — CORRECTED (F3, BLOCKER).** The body is applied to
`Right(a), Left(w₁), …, Left(w_{t−1})` — **`t` applications** for an exiting trajectory
of length `t`. Injectivity forces the `wᵢ` pairwise-distinct ⇒ `t − 1 ≤ |u|` ⇒
**`t ≤ |u| + 1`, and this is attained** (witness: `u = Bool`, body = the 3-cycle
`R() → L(L()) → L(R()) → R()` on `(1+1)+1`, a bijection, gives `t = 3 > |u| = 2`).
**ADR-0007's "iteration count ≤ |u|" is wrong** — this design ships the correction to its
owed TH-1 exact bound: **`k_max = |u| + 1`.** Using `|u|` would make `B` under-run the
maximal-trajectory input by exactly one iteration and stop mid-loop where `BR = 0` and
`s0 = s1 = 0`, so the M-D.w terminal check **passes** and a mid-loop payload unmarshals as
the answer — a *silent* wrong result unless the oracle domain actually reaches `t = |u|+1`.

## 2. The reversible while-loop encoding (hand-verified, §D of the review)

Tag convention `0 = Left, 1 = Right` (compile.rs:63,145,151). `L = |body|`;
`δu = 256 − (L + 9)` is the backward distance so a taken `SWAPBR rb` (`PC += BR + 1`)
lands PC back on `Ltop`. Registers `rc, rb, temp, ctrl` are **dedicated, 0 at program
start, live across the jump** (F5). The scheme is a **conserved token**: the value `δu`
circulates `prologue-ADDI → temp → rb → BR → rc → temp → SUBI` exactly once per iteration,
so `rc = rb = temp = ctrl = 0` at every body entry (this is what makes the come-from
*chain* — the first draft failed because the witness was never cleared).

```
prologue: XORI tag,1          ; inject Right (a-side)
          ADDI temp,δu        ; mint token
Ltop:  SWAPBR rc              ; catch: rc ← δu on backward re-entry, 0 on fall-through
       CSWAP  ctrl, rc, temp  ; join-dispatch: route witness rc→temp iff ctrl (=comer)
       SUBI   temp, δu        ; burn token — both comers now have temp=0 here
       XOR    ctrl, tag
       XORI   ctrl, 1         ; ctrl ⊕= ¬tag  →  0   (tag unchanged since bottom)
       <body: L instrs>       ; sees rc=rb=temp=ctrl=0, carrier layout invariant (§3)
       XOR    ctrl, tag
       XORI   ctrl, 1         ; ctrl := ¬tag   (1 = continue [Left], 0 = exit [Right])
       ADDI   temp, δu        ; mint token
       CSWAP  ctrl, temp, rb  ; continue: rb=δu,temp=0  ·  exit: rb=0,temp=δu
Lend:  SWAPBR rb              ; continue: BR=δu → PC=Ltop  ·  exit: BR=0 → fall through
       SUBI   temp, δu        ; burn token (exit path)
       XORI   tag,1           ; strip Right → tag reg back to 0; payload = y
```

**Come-from join-dispatch (F1 fix, ADR-0034 D5).** `Ltop`'s `SWAPBR rc` is the *join*;
its two comers (forward fall-through with `BR=0`, backward jump with `BR=δu`) are
distinguished **in-data** by `ctrl` (which is 1 exactly on the in-flight continue path),
so the `CSWAP ctrl, rc, temp` clears the witness on the backward comer and is a no-op on
the fall-through comer. The first draft sketched only the JUMP half of D5 and omitted the
join, so the witness accumulated and, on the third arrival, swapped a stale `δu` into BR
and jumped *before the loop*. Fixed.

**`ctrl` rides across the jump (F2 fix).** `ctrl` is **not** uncomputed before the
branch (that was the false circularity in the first draft's §8 Q2). It is computed at the
bottom (`ctrl := ¬tag`), gates `Lend`, rides across the backward jump, and is uncomputed
at the **top** from the *same tag* — which cannot change in the jump window (only the
`SWAPBR` executes between bottom-compute and top-uncompute). On the exit path `ctrl = 0`,
so nothing rides out. Verified for first-entry, t=1, t=2, t=3 (review §D hand-trace).

## 3. Loop-carrier layout contract (F5 — must be stated, not assumed)

- The carrier is **`1 + max(|u|, |a|, |b|)` registers**: tag at leaf 0, payload
  zero-extended to the widest arm (the sum encoding already zero-pads, compile.rs:147,153).
- The body is lowered **once** under that ctx with the **hard loop invariant
  `homes_in == homes_out`** (a *stronger* cousin of compile.rs:1440's "unchanged during
  lowering" — here it is "output layout equals input layout", because PC re-enters the
  same instructions). If the body's out-layout ≠ in-layout, **reject** (Deferred) — do not
  emit a loop whose feedback path misroutes the carrier.
- Feedback of `Left(w)` is then layout-exact: tag reg + first `|u|` payload regs, the rest
  provably 0.
- **Register budget:** 4 dedicated aux (`rc, rb, temp, ctrl` — all register-resident,
  live across the jump, unpageable) + the tag reg. Introduce **`MAX_LEAVES_LOOP`** (analogous
  to `MAX_LEAVES_BRANCHING`, compile.rs:234); with 8 regs this leaves ≤ ~3 payload leaves
  register-resident before paging. Bodies exceeding it are rejected.

## 4. WCET / S_max must be COMPOSITIONAL, not a ROM scan (F4 — MAJOR; also fixes an M-D.w latent hazard)

M-D.w's `max_skip_distance` (compile.rs) pairs *consecutive* `Swapbr`s and assumes
"skip blocks don't nest." The loop's `Ltop … Lend` pair breaks that: with a skip-free
body the scanner over-counts (sound but spurious); **with any forward skip inside the
body it mispairs `(Ltop,innerOpen)` and `(innerClose,Lend)` and UNDER-counts `S_max` →
P2 passes falsely → silent PC wrap** (the alloc2/MEXCHR invisible-corruption class).

Fix: stop deriving `S_max`/`wcet` by scanning the ROM. Compute them **compositionally
during lowering** — each emit site knows whether it emitted a forward skip
(`emit_skip_block`) or a backward loop bracket, and the sign of the value ADDI'd into `rb`
is the branch direction (statically invisible to a scanner). Return structured metadata
alongside `Vec<Instr>` (e.g. `Lowered { instrs, forward_skip_total, loop_terms:
Vec<(k_max, iter_len)> }`), and:
- `wcet = |instrs| + Σ loop_terms (k_max · iter_len)` with `k_max = |u| + 1`;
- `S_max = forward_skip_total` (forward skips only — the loop's backward bracket
  contributes **0** to forward-skip distance);
- **P2 guard becomes `wcet + S_max ≤ 256`** (this is the §7 budget-threading: `run_on_cpu`
  runs `wcet` steps, not `instrs.len()`).

This is the piece that makes M-D.w's WCET path load-bearing; it is a **prerequisite
commit** of M-D.d (§6), not a change to M-D.w (whose scanner is correct for the
loop-free, nesting-rejected programs that exist today).

## 5. Non-goals / honest limits (F9)

- **Practical Trace size is tiny under the 8-bit PC.** `|iter| = |body| + 9`; P2 requires
  `spine + (|u|+1)·(|body|+9) + S_max ≤ 256`. So `|u|=2 ⇒ |body| ≤ ~70`; `|u|=4,|body|=10
  ⇒ B ≈ 105 ✓`; `|u|=8 ⇒ |body| ≤ ~18`; `|u|=16 ⇒ |body| ≤ ~4`; a byte-wide carrier
  (`|u|=256`) is unconditionally rejected. **M-D.d is a machinery-proof milestone**; useful
  Traces are gated on a future **widen-PC ADR**. Rejection is loud (reject-don't-corrupt,
  M-D.w §3.1). This arithmetic is the acceptance boundary, not a bug.
- No CALL/RET/stack (that is M-D.d2). No MEM_WORDS widening / heap (M-D.c).
- **Nested Trace-in-Trace:** if the come-from join does not compose under nesting, **reject
  nested Trace** (Deferred) and defer — exactly as alloc2 did for nested skip-blocks.

## 6. Prerequisite commits + decomposition (delegable order)

1. **Prereq A (F7 — pre-existing latent bug, fix-or-reject).** `AssocLSum/AssocRSum/
   UniteSum/UnitiSum` lower as **unconditional no-ops** (compile.rs:1380-1387) but are NOT
   no-ops in the tag+payload encoding for **nested** sums: `AssocRSum((1+1)+1)` must send
   `L(R())=(0,1) ↦ R(L())=(1,0)`; the no-op leaves `(0,1)` → **silent miscompile**.
   Unreachable today (no program compiles a non-trivial sum assoc) but **Trace carriers are
   nested sums `u+a`**, so real bodies will assoc-juggle them. Either implement the correct
   tag/payload permutation for nested-sum assoc/unit, **or** loudly reject non-trivial
   assoc/unit. Reproducing test (L20): whole-domain oracle on an iso containing
   `AssocRSum((1+1)+1)` — fails today, passes after. This is independently valuable and
   should land first (own commit; small).
2. **Prereq B (F4 + §7).** Compositional `wcet`/`S_max` metadata (replace the ROM scan) +
   thread the budget so `run_on_cpu` runs `wcet` steps (`run_rom_on_cpu(instrs, init,
   budget, eval_fn)`, `run_on_cpu` passes `wcet`). Testable for the first time by the first
   loop (Prereq is wiring; the loop in step 3 is the test).
3. **M-D.d core:** single-level `Trace` → the §2 reversible while-loop; layout contract
   §3; `k_max = |u|+1`; acceptance §7 below.
4. **Follow-ons (separate tasks):** M-D.d2 reversible calling convention (CALL/RET as
   ADR-0034 D5 macros — no new opcodes, link reg = a general reg, no stack fields; shares
   the §2 join-dispatch machinery); nested Trace; Trace-with-MEXCHR body.

## 7. Acceptance (every finding ships a reproducing test — L20)

- **A1 — `bare_trace` flips to accepted:** replace `bare_trace_rejected` with a minimal
  Trace over small finite `u`; compile → ROM → `run_on_cpu == eval` (whole domain).
- **A2 — the 3-cycle maximal witness (authoritative, real-CPU L19; covers F1 + F3):** the
  `u = Bool` 3-cycle body reaching `t = 3 = |u|+1`. This is the single most valuable test:
  it exercises **two** backward re-entries (the F1 come-from-chaining crux) **and** the
  `k_max = |u|+1` boundary (F3). `run_on_cpu` (real `step`) `== eval` over the whole
  3-input domain. **If this body is not compilable under the inner-control policy (F6),
  the milestone cannot test its own hardest property — so the policy MUST admit it.** It is
  4 straight-line affine-GF(2) instructions (`t^=p; t^=1; p^=t; p^=1`); provide a prim/
  special lowering if no structural iso term produces it (and note F7 blocks the structural
  route `SwapSum∘AssocRSum`).
- **A3 — adjoint:** `(Trace f)† = Trace(f†)` (adjoint.rs, same `u`); `run_on_cpu(sym)`
  inverts `run_on_cpu(fwd)` on the domain. `k` is direction-uniform (exit on tag=Right both
  ways).
- **A4 — WCET is real and exceeds `rom.len()`:** `wcet(trace) == spine + (|u|+1)·|iter|`
  (pinned golden) **and `> rom.len()`** — the first program where `B > rom.len()`, proving
  Prereq B's budget path is load-bearing.
- **A5 — park under a loop (M-D.w P1/P3 + park-liveness with a real backward jump):** run
  to `B`; assert `rc=rb=temp=ctrl=BR=s0=s1=0` at exit; output stable for over-runs into the
  dead zone.
- **A6 — SystemRing closure (serialize like `mexch_reg`, OOM precaution):** the Trace ROM
  closes on the ring (`run == eval^R`, bus-quiescent). Re-fetch of a revisited PC is
  identical (fetch is a stateless PC-keyed XOR mask, step.rs:113-140; IR re-zeroed each
  cycle); per-iteration bus-quiescence is automatic (emit/mem/absorb close within each
  `system_step`).
- **A7 — reject-don't-corrupt:** a Trace with `B + S_max > 256`, a body violating the
  layout invariant (§3), and (if applicable) nested Trace are each **rejected** (Deferred),
  not miscompiled.
- **T4 downgrade (F8):** drop the structural `wcet(iso) == wcet(sym iso)` equality (not
  guaranteed once `|compile(body†)|` need not equal `|compile(body)|`); assert instead that
  each direction runs correctly within its **own** `wcet` (per-direction correctness).

## 8. Residual open questions for the *implementation* review (before merge)

1. Does the §2 skeleton's `k_max = |u|+1` idle span always satisfy `B + S_max ≤ 256` for
   the chosen test carriers, with the corrected compositional `S_max`?
2. Is `MAX_LEAVES_LOOP` (§3) derived correctly for the 4 aux + tag, and does the body
   layout-invariant reject the right bodies (not over-broad)?
3. F7: is the assoc fix correct over *deeper* nestings (`((1+1)+1)+1`), or does the reject
   path catch them?
4. Does the come-from join compose if the body itself contains a forward skip-block (F6
   option a) — or is the safe Swapbr-free-body policy (F6 option b) in force, with A2's
   witness provided via a prim?
