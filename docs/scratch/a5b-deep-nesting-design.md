# A-5b — deep loop-nesting (depth-2+ nested `Trace`) design

**Scope.** The compositional-control trio (A-3/A-5/A-4, merged `6b54d9d`) made
nested programs compile+run, but **depth-2 nested `Trace` rejects gracefully
because it is register-bound**: A-5's `allocate_aux_frame`
(`compile.rs:786-809`) draws each loop level's four aux `{rc, rb, temp, ctrl}`
from **free registers only** and occ-marks them before lowering the body, so a
nested inner `Trace` must draw a *spatially-disjoint* second frame. Depth-2 then
needs `1 carrier + 4 outer aux + 1 carrier + 4 inner aux = 10 > NUM_REGS(8)`
registers and rejects (pinned: `tests/compile.rs::mdd_reject_nested_trace`,
`tests/nested_aux.rs::nested_trace_program_still_rejects_via_frame_bound`).

A-5b lifts this. Two options were investigated against the real code; the
investigation **changes the recommendation the prompt anticipated**, because the
liveness of the outer aux across the inner region turns out to be *nil*.

All `file:line` are against the `a5b-scratch-frames` worktree
(`crates/theseus-tisc/src/…`).

---

## 0. The two candidate options

- **OPTION A — aux frames in SCRATCH** (reg∪scratch via A-0's operand crossbar).
  Draw aux from `reg ∪ scratch` (idx 10-15 = s2..s7) when registers are
  exhausted; the outer frame spills to scratch, the inner frame stays in regs.
  Generalizes to arbitrary depth (bounded by 6 scratch slots).
- **OPTION B — reuse the outer aux across the inner region** (compile-time
  liveness discipline). If the outer level's aux `{rc,rb,temp,ctrl}` are provably
  **dead** across the body region (where the inner loop lives), the inner loop may
  **reuse the same four registers** — no second frame is needed at all. Zero
  standing tax.

---

## 1. OPTION A — feasibility of a SCRATCH aux frame

### 1.1 The datapath routes every relevant op's operands over reg∪scratch

Under A-0 the operand fields `rd/rs/rt` are **5-bit indices over `reg ∪ scratch`**
(16 locations); `operand_field(i)` maps `i<8 → r[i]`, `8≤i<16 → s[i-8]`
(`decoder.rs:326-331`). The `select_operand` crossbar (`decoder.rs:358-381`)
routes any scratch operand `s_k` (k≥2, idx 10-15) to the fixed r0-slot, leaving
s0/s1 (idx 8/9, the decoder-reserved operand-buffer + predicate word) untouched
(level-3 swaps exclude positions 0/1, `decoder.rs:364-369`). Concretely, each aux
op used by the loop skeleton routes its operands through this crossbar:

| aux op (skeleton) | routing | scratch operand OK? |
|---|---|---|
| `ADDI/SUBI temp, δu` | `group_b_shared` → `select_operand(R_RD)` (`decoder.rs:1168`) | **yes** |
| `XORI ctrl, 1` | `group_b_shared` → `select_operand(R_RD)` | **yes** |
| `XOR ctrl, tag` | `group_a_shared` → `with_two_regs(R_RD,R_RS)` → `select_operand` (`decoder.rs:675-677`) | **yes** |
| `CSWAP ctrl, rc, temp` / `CSWAP ctrl, temp, rb` | `cswap_instr` → `with_target(R_RD)` + `with_two_regs(R_RT,R_RS)` (`decoder.rs:734-757`) | **yes** |
| `SWAPBR rc` / `SWAPBR rb` | `swapbr_instr` → `with_target(R_RD, swapbr_core())` (`decoder.rs:929-931`) | **datapath yes, encoder NO — see §1.2** |

`swapbr_core` (`decoder.rs:920-927`) operates on `[reg_field(0), BR_FIELD,
AUX_PRED_FIELD]` *after* `with_target` routes the operand to the r0-slot, so a
scratch operand is swapped with BR exactly as a register operand is. The come-from
token in a scratch word therefore works with `PC += BR + 1`: the token never
*lives* in BR — SWAPBR swaps the scratch word ↔ BR at the branch point only, and
BR is consumed and zeroed by the end-of-step `pc_add_br ; pc_inc`
(`step.rs:35-45`, `58-60`). The token's home (reg or scratch) is irrelevant to the
PC update, which reads BR alone.

### 1.2 THE BLOCKER: the assembler rejects `SWAPBR` with a scratch operand

`encode(Instr::Swapbr { rd })` has a **SWAPBR-specific guard stricter than the
general `check_operand`**:

```rust
// asm.rs:319-324
Instr::Swapbr { rd } => {
    if (rd as usize) >= NUM_REGS {          // NUM_REGS = 8
        return Err(AsmError::SwapbrRegOutOfRange { rd });
    }
    r_word(0b1000, rd, 0, 0)
}
```

The general operand validator `check_operand` (`asm.rs:222-231`) **accepts**
scratch operands 10..15 and rejects only s0/s1 (8/9) and ≥16 — so `ADDI/SUBI/
XORI/XOR/CSWAP` with a scratch aux encode fine (they go through `r_word`/`i_word`
→ `check_operand`). SWAPBR alone is gated to `rd < 8`. The ROM word format has a
full 5-bit rd field (`r_word`, `asm.rs:235-244`), so a scratch SWAPBR *fits the
encoding*; the guard is a **conservative source-level gate**, not a hardware
limit. There is no documented soundness reason (the negative test
`negative_guards.rs:353-359` only pins `rd:9` = s1, which `check_operand` would
reject anyway). The datapath iso `swapbr_instr` already routes rd over the full
crossbar (comment `decoder.rs:930`: "A-0 (BLOCKER-2 fix): route rd over
reg∪scratch").

### 1.3 Which aux can go to scratch, which must stay register-resident

- `temp`, `ctrl`, and `rc`-as-a-CSWAP-source can go to scratch **today** (encoder
  already accepts scratch on ADDI/SUBI/XORI/XOR/CSWAP).
- `rc` and `rb` are **SWAPBR operands** (`SWAPBR rc` at Ltop, `SWAPBR rb` at
  Lend). Under the current encoder they **cannot** go to scratch — the
  `SwapbrRegOutOfRange` guard blocks it. **This is the Option-A blocker.**

**Option A is not dead**, but it requires **relaxing the encoder guard** to
`check_operand(rd)` (accept 10..15, still reject s0/s1 and ≥16) *and* an authority
proof that a real scratch-SWAPBR runs correctly on `system_step` — a datapath
change surface (the crossbar path for SWAPBR-rd was, per the roadmap review
`6b54d9d`, "NO real-system_step test ever ran a scratch-operand instr (10-15)").
So Option A carries a genuine hardware-authority obligation that Option B does not.

---

## 2. OPTION B — true persistent-aux liveness across the inner region

### 2.1 The emitted outer skeleton (one iteration), `compile.rs:3675-3745`

```
prologue:  XORI tag,1                    # inject Right
           ADDI temp,δu                  # mint token → temp=δu
Ltop:      SWAPBR rc                     # rc ↔ BR  (backward entry: rc←δu; first: rc←0)
           CSWAP ctrl, rc, temp          # join-dispatch (reads rc,temp)
           SUBI temp,δu                  # BURN token → temp=0
           XOR ctrl,tag ; XORI ctrl,1    # uncompute ctrl → ctrl=0
── body ──                               # <<< the inner loop lives HERE >>>
           XOR ctrl,tag ; XORI ctrl,1    # compute ctrl = ¬tag
           ADDI temp,δu                  # MINT token → temp=δu
           CSWAP ctrl, temp, rb          # route (continue: rb=δu; exit: rb=0)
Lend:      SWAPBR rb                     # rb ↔ BR  (continue: BR=δu jumps back)
epilogue:  SUBI temp,δu                  # burn (exit path)
           XORI tag,1                    # strip Right
```

### 2.2 Aux values at the body boundary (liveness trace)

Walk the values entering the body (immediately after `XORI ctrl,1` at the top of
the bracket):

| aux | state entering body | why |
|---|---|---|
| `rc` | **0** | Ltop SWAPBR loaded rc (δu on back-edge, 0 on first), then `CSWAP ctrl,rc,temp` consumed it back to 0 (join-dispatch is a self-inverse route) |
| `temp` | **0** | minted δu at prologue/back-edge, burned by `SUBI temp,δu` immediately before the body |
| `ctrl` | **0** | uncomputed by `XOR ctrl,tag ; XORI ctrl,1` before the body |
| `rb` | **0** | last touched at Lend of the *previous* iteration; SWAPBR rb put δu→BR and left rb=0 |
| `BR` | **0** | consumed by `PC += BR + 1` at the end of the Lend step (`step.rs:35-45`); zero for the whole next step's body |

**All four aux and BR are ZERO across the entire body region.** The come-from
token is **not** carried across the inner region in any register — it is burned
before the body and re-minted after it, from the **compile-time `δu` immediate**
baked into the `ADDI/SUBI/CSWAP` words (`δu = 256 − iter_len`,
`compile.rs:3662`). The *only* state the outer loop needs live across the body is
the **carrier** (`tag` + payload leaves) — which is genuinely-live data, not aux.
The loop's continuation across the back-edge is carried by the **PC** (via BR at
the two SWAPBR points), and BR is transient (zeroed every step).

### 2.3 True persistent-aux count per level = **0**

Contrary to the A-5b framing ("the come-from token `temp`/δu must persist across
the inner region"), the token does **not** persist in a register. The
persistent-aux count across the inner region is **0**: the four aux are pure
per-iteration ancillae, net-zero at both body-entry and body-exit (the Bennett
discipline the frame already satisfies — `allocate_aux` docstring
`compile.rs:646`, net-zero verifier `compile.rs:1955-1988`).

**Consequence: the inner loop may REUSE the outer's four aux slots.** They are
disjoint in *time*, not required disjoint in *space*. The inner loop is itself a
net-zero bracket (same skeleton), so it enters with the four slots = 0 (which they
are, across the body) and exits with them = 0 (which the outer epilogue at step 6
requires). No inner instruction runs between the outer's mint (post-body `ADDI
temp,δu`) and the outer's Lend — those are outer straight-line epilogue, so the
outer's transient `temp=δu` (steps 7-9) never overlaps an inner instruction.

**Depth-2 fits 8 registers with reuse:** `1 carrier + 4 shared aux + 1 carrier`
(inner carrier reuses the outer tag reg or a distinct reg) = well within 8. In
fact reuse makes the aux cost **O(1) in depth** — arbitrary depth fits, bounded
only by carrier registers, exactly as A-0 intended (relieve register pressure).

### 2.4 The distinction Option B must encode: nesting (reuse) vs. breadth (disjoint)

The current occ-marking of the frame (`compile.rs:3470-3473`, `body_occ` with the
frame set, threaded into `body_ctx`) exists for **two** reasons that must be
separated:

1. **Against a `Times` sibling `g`** (breadth): g's leaves are live across the
   *whole* Trace, so the frame must be disjoint from g. This is the F-1 aliasing
   fix (A-5) and **must stay** — g is live, the frame must avoid it.
2. **Against a nested inner loop** (depth): the inner loop's aux need not be
   disjoint from the outer's, because the outer's aux are dead across the body
   (§2.2). Occ-marking the frame against the *inner* body is **over-conservative**
   — it is exactly what forces the depth-2 reject.

Option B's change: thread into the body the occ **with the outer frame marked
against siblings but NOT reserved against the body's own inner loops** — i.e. the
inner `allocate_aux_frame` should see the four outer-aux registers as **free for
reuse**, while a `Times` sibling's leaves remain marked. Because sibling leaves
are carrier leaves (in `ctx.occ` already) and the aux are a distinct set, the two
are distinguishable: mark carriers+siblings, but pass the aux set as *reusable* to
the inner level.

---

## 3. RECOMMENDATION — Option B (reuse), with Option A as a later generalization

**Recommend Option B.** Per the efficiency principle (compile-time discipline >
machine growth):

- **Zero standing tax.** B reuses four already-allocated registers; no scratch is
  energized as a frame, no encoder/datapath change, no new authority surface. A
  spills to scratch (already-energized, low tax) but requires **relaxing the
  `SwapbrRegOutOfRange` encoder guard AND a real system_step authority proof that
  scratch-SWAPBR is faithful** — a datapath-adjacent change (§1.2) with a heavier
  review bar.
- **B fits depth-2 in 8 registers, and in fact arbitrary depth** (aux cost is
  O(1) in depth under reuse), because the outer aux are provably dead across the
  inner region (§2.3). This is *stronger* than the A-5b goal (which asked only for
  depth-2).
- **B is the smaller, sounder diff.** The reuse safety argument is the same
  net-zero/ancilla-discipline property the frame already proves (§2.2) — no new
  invariant, just a relaxation of an over-conservative occ-mark.

**Option A is complementary, not required.** A generalizes the *carrier* pressure
too (a very wide carrier could exhaust registers even with shared aux), and would
let aux spill to scratch when even the carrier+one-frame doesn't fit. Recommend A
as a **follow-on** slice **only if** a real program needs it — and only after the
`SWAPBR`-scratch datapath authority test lands. Both are combinable: B first
(unlocks deep nesting for free), A later (widens the carrier ceiling).

**Depth bound achieved:** Option B → nesting depth bounded by **carrier
registers**, not by `4·depth` aux. For Bit carriers (1 tag + small payload) this
is effectively unbounded within the 8-register file; the aux are a single shared
4-register frame reused at every depth. State this bound explicitly and keep the
reject-don't-corrupt guard for the (now much rarer) carrier-exhaustion case.

---

## 4. The exact code change (Option B)

The reuse must relax the frame occ-mark **for the inner body only**, while keeping
it **against `Times` siblings**. The minimal, surgical change:

1. **`compile.rs:3470-3473`** — `allocate_aux_frame` returns `(frame,
   marked_occ)` and the caller threads `marked_occ` into `body_ctx.occ`. Change:
   thread `body_occ` **without** the frame set into the body context used to lower
   the inner loop, but **keep** the frame set in the occ used for the *sibling*
   safety (the `Times` path already occ-marks the sibling before this Trace is
   lowered, `compile.rs:2569-2576` for the controlled idiom / the `split` path).
   Concretely: lower the body with `body_ctx` whose occ = carriers + siblings
   (NOT the four aux), so the inner `allocate_aux_frame` re-draws the **same four
   lowest-free registers** = the outer frame (reuse), which is exactly what we
   want. Add an assertion that the inner frame ⊆ outer frame (reuse, not a new
   disjoint set) OR is drawn from genuinely-free regs disjoint from carriers.

2. **`allocate_aux_frame` (`compile.rs:786-809`)** — unchanged; it already draws
   the lowest free registers. With the aux **not** marked in the body occ, the
   inner call naturally re-draws the outer frame's registers. The self-inverse
   net-zero bracket makes reuse sound.

3. **Guards to keep:**
   - the `Times`-sibling disjointness (do NOT relax the frame occ-mark against
     siblings — only against the inner body). This preserves the A-5 F-1 fix.
   - `n_input + 1 > carrier_reg_budget` reject (`compile.rs:3352`) — carrier still
     must be register-resident; reuse relieves *aux* pressure, not carrier.
   - the canonical-input-homes assert (`compile.rs:3314-3322`) — unchanged; the
     k_max walk still requires canonical input homes.

4. **`carrier_reg_budget`** (`compile.rs:3348`, currently `NUM_REGS - 4`): under
   reuse the four aux are shared across all depths, so the budget stays `NUM_REGS
   - 4 = 4` (one shared frame reserved) regardless of depth — no change needed,
   but document that the `-4` is now depth-independent.

5. **Re-pin the two reject tests** as *pass* tests:
   `tests/compile.rs::mdd_reject_nested_trace` and
   `tests/nested_aux.rs::nested_trace_program_still_rejects_via_frame_bound` must
   become "nested Trace COMPILES via aux reuse" (or be replaced by the §5.1
   authority test); keep a reject test only for the genuine carrier-exhaustion
   case (`nested_aux.rs::nested_depth_bound_rejects_when_no_disjoint_frame_fits`
   is re-framed as carrier-bound, not aux-bound).

---

## 5. Invariant preservation

- **Net-zero frames (Bennett).** The reused frame is created-zero at each level's
  entry and consumed-to-zero at exit — this is the *same* property the current
  frame proves; reuse relies on it (§2.2) rather than weakening it. The net-zero
  verifier (`compile.rs:1955-1988`) and the terminal `BR/s0/s1 != 0` check
  (`compile.rs`, run_on_cpu) both still apply and now cover the reuse case.
- **marshal == body-home canonical.** Untouched — the input payload homes stay
  canonical `Reg(0..n_input)` (`compile.rs:3427-3429`); reuse changes only *aux*
  placement between depths, never the input homes the k_max walk depends on
  (`compile.rs:3585-3592` warning respected).
- **B_fold ≥ true_B.** Untouched — A-3's cost-tree fold (`CostTree::extra`,
  `compile.rs:2790-2794`) already multiplies inner cost through the outer `k_max`
  and uses the cardinality-bound fallback when the body has inner control
  (`compile.rs:3576`). Reuse changes register allocation, not the WCET fold.
- **Come-from composes across depth.** The outer token is carried by δu-immediates
  + PC/BR, not by a register live across the inner region (§2.2), so the inner
  loop's use of BR (zeroed on inner exit) and of the shared aux (zeroed on inner
  exit) leaves the outer's continuation intact. This is the ADR-0034 §5
  composition obligation, now discharged by liveness rather than spatial
  disjointness.

---

## 5.1 POC + authority test (the §5.1 claim, finally testable)

A **real depth-2 nested Trace** that currently REJECTS
(`mdd_reject_nested_trace`) must compile + `run_on_cpu == eval`:

- Smallest shape (mirrors the existing reject test):
  ```
  inner = Trace(One, Id(1+1))            # One ↔ One, tag-only carrier
  body  = Plus(Id(One), inner)           # (1+1) ↔ (1+1), inner loop inside an arm
  outer = Trace(One, body)               # One ↔ One, body CONTAINS a Trace
  ```
  For a *non-vacuous* authority test the inner Trace must actually iterate and the
  outer must take ≥ 2 iterations **with the inner loop running each time**. Use a
  Bit carrier with a decrementing tag body so:
  - outer takes ≥ 2 iterations (the "outer δu preserved across the ENTIRE inner
    loop" scenario — the design's central claim);
  - the inner loop runs ≥ 2 iterations per outer iteration (so the shared aux are
    genuinely torn down and rebuilt inside the outer's live region).
- Assert: `run_on_cpu(compile(outer)) == eval(outer)` over the whole (small,
  `B ≤ 256`) domain; the reverse `eval^R` check; BR/s0/s1 terminal quiescent.
  Keep `B ≤ 256` (Bit carriers, tiny k) so no A-1 widen is needed
  (compositional-control-design §4/§8).
- **L20 regression:** this test fails today (reject) and passes with reuse — it is
  the reproducing test for the A-5b finding and stays in the suite. Add a
  **shared-aux tear-down probe**: assert (via the net-zero verifier or a state
  dump) that the four shared aux are 0 at each outer-body boundary across ≥ 2
  inner iterations — the direct witness of §2.2.

---

## 6. Highest-bar review gates (`lower_trace` = daemon-equivalent top bar)

Per CLAUDE.md L15/L20 and compositional-control-design §6:

- **Fresh Fable-class adversarial review** (different model from implementer)
  targeting: *"construct a nested program where the outer aux are NOT dead across
  the body → reuse corrupts the outer come-from."* The reviewer must check the
  §2.2 liveness claim holds for **every** admitted inner-body shape (Iter/Plus/
  branch/inner-Trace inside the outer body), not just the smallest repro — the
  crux is that the outer aux really are zero across *any* inner region.
- **Real `system_step` authority** (not just `run_on_cpu_fast`): the depth-2
  program (§5.1) run on the real ring, `run_on_cpu == eval`, `eval^R`, terminal
  quiescent — the L15 minimum bar (a real worktree + gate round-trip).
- **Non-degenerate vectors:** the inner loop must actually iterate ≥ 2× and the
  outer ≥ 2× (the roadmap review `6b54d9d` flagged degenerate `body=Id`/symmetric
  vectors as NOT counting as coverage). A tag-only Id-body inner loop is
  degenerate — use a decrementing carrier.
- **Heavy tier `--test-threads=1`** (`tests/compile.rs`) must run and be zero-fail
  (the compositional-control review found the heavy tier "never run — 5 fail");
  A-5b re-pins the two former reject tests as pass tests there.
- **Byte-identity for single-level + Times(Trace,g):** reuse must NOT change the
  ROM for any currently-compiling program (single-level Trace, `Times(Trace,g)`).
  The `allocate_aux_frame` byte-identity assert (`compile.rs:3483-3509`) must
  still hold; add a ROM-compare regression for `Times(Trace,g)` proving the
  sibling frame is unchanged (only the *inner-nesting* occ-mark is relaxed).

---

## 7. Open questions

1. **Option-A encoder relaxation (deferred):** is the `SwapbrRegOutOfRange` guard
   (`asm.rs:319-323`) purely conservative, or is there an un-obvious datapath
   reason SWAPBR-rd was gated to registers? The datapath iso routes it over the
   crossbar (`decoder.rs:930`), suggesting conservative — but Option A must prove
   a real scratch-SWAPBR runs on `system_step` before relaxing. (Not needed for
   Option B.)
2. **Reuse vs. `Times(Trace, Trace)` (breadth × depth):** two *sibling* Traces
   each need a frame *live across their own body* but the siblings are sequential
   (Times lowers sequentially, compositional-control-design §1.2 `wcet(Times) =
   wcet(f)+wcet(g)`), so their frames are also time-disjoint and can share. Verify
   the sequential-lowering claim means sibling Trace frames can reuse too (likely
   yes — same liveness argument), or whether siblings must stay disjoint.
3. **Assertion form for reuse:** should the inner `allocate_aux_frame` *assert*
   `inner_frame == outer_frame` (strict reuse) or merely *permit* overlap
   (draw-lowest-free naturally reuses)? Strict-reuse assert is a stronger
   regression tripwire; recommend it for the single-carrier case.
4. **BR time-sharing proof depth:** the review's crux (compositional-control-design
   §7 Q4) — is the §2.2 liveness paper-proof sufficient, or does the review demand
   an exhaustive nested-back-edge state trace showing BR=0 and all four aux=0 at
   every inner step? Recommend the exhaustive trace as the §5.1 authority test's
   shared-aux tear-down probe.
