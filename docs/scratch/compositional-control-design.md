# Compositional Control — design for A-3 → A-5 → A-4

**Scope.** Close the deepest remaining TISC-2 coverage gap: **non-compositional
control**. Today `compile.rs` rejects every form of nested control:

- nested `Trace` (loop-in-loop),
- `Times(Trace, ·)` (loop-under-product),
- branch-in-branch / branch-in-loop / loop-in-branch (any lowered body that
  itself contains a `Swapbr` skip-block).

This doc investigates the machinery (all `file:line` against the `a6-synth`
worktree on merged A-0), then drafts the three slices **in the review-corrected
order A-3 → A-5 → A-4** with, for each: the exact change, the invariant it must
preserve, the smallest real reproducing program, the authority test, and the
review gates.

Companion docs: `tisc2-design-synthesis.md` §2 Axis 3 + §7 (BLOCKER-2/3,
MAJOR-4/5), `a6.4-widen-pc-design.md` (δu / W_pc decouple, BLOCKER-3),
`scratch-frame-gcd-estimate.md` (frame discipline).

> **CRITICAL ORDERING (MAJOR-4).** A-5 (occ-aware frame stack) MUST precede A-4
> (deleting the rejects). Deleting the F-1 reject before aux allocation is
> occ-aware would let `Times(Trace, g)` silently alias the sibling factor's live
> registers → wrong output the terminal check can't see. See §3.

---

## 0. Machinery map (where everything lives today)

| Concern | Symbol | Location |
|---|---|---|
| flat-sum WCET | `Lowered::wcet` | `compile.rs:2166-2174` |
| flat-sum S_max | `Lowered::s_max` | `compile.rs:2177-2179` |
| loop-term vec | `LowerMeta.loop_terms`, `Lowered.loop_terms` | `compile.rs:2159`, `2256` |
| Trace lowering | `lower_trace` | `compile.rs:2550-2939` |
| exact-k_max walk | (inline in `lower_trace`) | `compile.rs:2749-2801` |
| body meta DROPPED | `let mut body_meta = LowerMeta::default();` | `compile.rs:2719-2720` |
| aux hand-assign | `aux_base = max_carrier_reg + 1` | `compile.rs:2828-2841` |
| come-from token δu | `delta_u = 256 - iter_len` | `compile.rs:2821` |
| **F-1 reject** (Times sibling) | `ctx.homes != canonical …` | `compile.rs:2621-2628` |
| **nested-Trace reject** | `contains_trace(body)` | `compile.rs:2630-2635` |
| **inner-control reject** (loop body) | `has_inner_control` | `compile.rs:2490-2494` (iter), `2725-2729` (trace) |
| **inner-control reject** (Plus arm) | `has_inner_control` | `compile.rs:3154-3158` |
| **inner-control reject** (controlled idiom) | `has_inner_control` | `compile.rs:1975-1979` |
| `has_inner_control` def | `any(Swapbr)` | `compile.rs:773-775` |
| park-safety guard (P2) | `budget + s_max_val > 256` | `compile.rs:3366-3371` |
| terminal check (P1/P3) | `br/s0/s1 != 0` | `compile.rs:3389-3394` |
| ADR-0034 bijection proof | step.rs docstring | `step.rs:47-60` |
| `pc_add_br` (`n = 2*W`) | | `step.rs:169-170` |

---

## 1. A-3 — tree-structured WCET/S_max fold + back-edge-honoring k_max walk

### 1.1 The bug (BLOCKER-2), exactly

`wcet()` sums `loop_terms` as a **flat list of sibling loops** with no nesting
structure:

```rust
// compile.rs:2166-2174
pub fn wcet(&self) -> usize {
    let base = self.instrs.len();
    let loop_contribution: usize = self
        .loop_terms
        .iter()
        .map(|&(k_max, iter_len)| (k_max - 1) * iter_len)  // FLAT SUM
        .sum();
    base + loop_contribution
}
```

Two independent defects make this under-count nested loops:

1. **The inner `body_meta` is discarded** (`compile.rs:2719-2720`):
   `lower_trace` lowers its body with a *fresh* `let mut body_meta =
   LowerMeta::default();`, then never merges `body_meta.loop_terms` back into the
   parent `meta`. So if the body contains its own Trace/Iter, that inner loop's
   `(k_inner, iter_inner)` term **never reaches the top-level `Lowered`** — the
   parent's `wcet()` doesn't even see it as a sibling, let alone multiply it.
   (Today this is moot because `contains_trace` rejects first — but Iter-under-
   Trace is *not* rejected by `contains_trace`; see §1.4.)

2. **The exact-k_max walk runs the body straight-line** (`compile.rs:2769-2789`):

   ```rust
   loop {
       for &instr in &body_instrs {        // STRAIGHT-LINE exec_instr
           exec_instr(instr, &mut state);
       }
       t += 1;
       if state.regs[tag_reg_idx as usize] != 0 { break; }
       if t >= u_inhabitants { use_fallback = true; break; }
   }
   ```

   `exec_instr` (`compile.rs:3435`) has **no PC / no back-edge**; it executes
   each instruction once in program order. An inner loop's `Swapbr` is just
   another instruction to it — the inner body is simulated **once**, not
   `k_inner` times. So the walk's `t` counts outer iterations while treating each
   as if the inner loop ran once.

**Worked counterexample (BLOCKER-2).** Outer `k = 4`, inner `k = 100`,
`iter_inner = 12`:

- True `B ≈ 4 × 100 × 12 = 4800`.
- Flat substrate: outer term `(4-1)·iter_outer` where `iter_outer` counts the
  inner body **once** (walk saw it straight-line) → `≈ 88` (55× under-count).
- Park guard `budget + s_max ≤ 256` **passes on the bogus 88**. The CPU is then
  run for only 88 steps, or — worse — parked with a PC that wraps mid-run → the
  L15-class silent-loss / corruption this whole discipline exists to prevent.

### 1.2 The correct tree-structured fold

Replace the flat `loop_terms: Vec<(usize, usize)>` substrate with a **recursive
cost tree** so WCET is a structural fold that *multiplies* through nesting:

```
wcet(Prim | Id | structural)   = |instrs|                 (leaf)
wcet(Seq(f, g))                = wcet(f) + wcet(g)
wcet(Times(f, g))              = wcet(f) + wcet(g)         (sequential lowering)
wcet(Plus(f, g))               = stage + wcet(f) + wcet(g) (Σ, MAJOR-5 — NOT max for s_max)
wcet(Iter(n, body))            = spine + (D-1) · wcet(body)
wcet(Trace(u, body))           = spine + k_max · wcet(body)   ← the multiply
```

The load-bearing line is `k_max · wcet(body)`: `wcet(body)` is itself the fold,
so an inner Trace's `k_inner · wcet(inner_body)` is already inside it, and the
outer multiply propagates automatically. This is exactly the fix BLOCKER-2 and
synthesis §2 Axis-3 call for (`wcet(Trace) = spine + k_max·wcet(body)`).

**Concrete substrate change.** Two viable encodings; recommend (A):

- **(A) Nested loop terms.** Change `loop_terms` entries to carry the body's own
  sub-terms: `enum CostTree { Straight(usize), Loop { k_max, spine, body:
  Box<CostTree> }, Branch(Vec<CostTree>) }` or a flattened
  `Vec<LoopTerm { k_max, iter_spine, inner: Vec<LoopTerm> }>`. `lower_trace`
  builds its term from the **merged `body_meta`** (stop discarding it,
  `compile.rs:2719`), and `wcet()` folds recursively.
- **(B) Pre-scaled flat vec (cheaper, still sound).** Keep `Vec<(k, iter)>` but,
  when merging `body_meta` into the parent, **multiply each inner term's `k` /
  contribution by the outer `k_max`** before pushing (BLOCKER-2 fix (i): "merge
  `body_meta.loop_terms` into parent SCALED by outer k_max"). Loses some tree
  structure but the *number* is correct. Simpler diff; acceptable for A-3.

Either way `iter_len` for the outer loop must be the **WCET of one full inner
run**, not the straight-line body length.

### 1.3 The back-edge-honoring k_max walk

`exec_instr` cannot be reused for the nested walk — it has no PC. Two options
(BLOCKER-2 fix (ii)):

- **(A) Mini-interpreter with a PC over `body_instrs`.** Add a small stepper that
  maintains a program counter, executes `Swapbr` by adding the branch register to
  the PC (mirroring `step.rs` `PC += BR + 1`), and loops until the body's tag
  exits — i.e. simulate the *real back-edge*. This gives the true inner
  trajectory length per outer iteration. This is the tight, exact-k_max-preserving
  path and is preferred; it reuses the byte semantics `exec_instr` already
  encodes, wrapped in a fetch/branch loop.
- **(B) Cardinality-PRODUCT fallback.** When a body contains inner control, bound
  it by `k_outer · (|u_inner|+1) · iter_inner` (the product of the per-level
  cardinality bounds). Never looser is *not* required here — soundness only
  requires **never under-count**. The product is a sound over-estimate; use it
  when the mini-interpreter is inconclusive, exactly as today's walk falls back to
  `|u|+1` (`compile.rs:2790-2800`).

The existing soundness fallback discipline (`compile.rs:2746-2748`) generalizes:
*never looser than the cardinality bound* becomes *never looser than the
cardinality **product** bound*.

### 1.4 Sub-finding: Iter-under-Trace slips through TODAY

`contains_trace` (`compile.rs:2942-2953`) rejects only nested **Trace**. An
`Iter` inside a Trace body is **not** caught by `contains_trace` — but the iter
body lowers to `Swapbr` skip-blocks, so it's caught instead by the
`has_inner_control` guard at `compile.rs:2725-2729` ("Trace body contains
Swapbr"). So today Iter-under-Trace is rejected, but by the *skip-block* guard,
not the Trace guard. **A-3's test MUST nest Iter/Trace** (synthesis §7: "Iter-
under-Trace is the common desugar and slips through as two summed sibling
terms") so that when A-4 later deletes `has_inner_control`, the tree fold — not a
reject — is what keeps `B` sound. If A-3 only tests Seq-siblings, the multiply
bug is invisible until A-4 removes the guard.

### 1.5 MAJOR-5: Plus / s_max must stay Σ, never `max`

WCET may soundly use `max` over Plus arms (only one arm runs). **s_max must NOT.**
A parked PC can land in the *untaken* larger arm's forward-skip region, so
`forward_skip_total += body.len()` for **both** arms is required
(`compile.rs:2513` iter, and the Plus path). Keep `s_max` a Σ-fold over arms.
Guard against a future "optimization" that tightens s_max to max.

### 1.6 A-3 invariant

**Behavior-preserving on flat programs**: for every currently-compiling program
(no nesting), the new fold must produce the **identical** `B` and `s_max` as
today (synthesis A3: "must reproduce current B"). And **never under-count** for
nested programs: `B_fold ≥ true_B` for all inputs (the park-safety soundness
property — the deepest obligation, §7 top adversarial target).

### 1.7 A-3 reproducing test (smallest real nested program)

**Iter-under-Trace** (the desugar case; catches both the discarded-`body_meta`
and the straight-line-walk defects):

- Carrier `Sum(u, a)` with a body that is `Seq(prim_step, Iter(n, inner_prim))`
  — a Trace whose per-iteration work itself contains a bounded inner loop.
- Smallest concrete: `u = Bit`, `a = Bit`, inner `Iter` count-type `nat_chain(3)`
  (D=3, so `D-1=2` inner copies), inner body a Bit-flip prim, outer body a prim
  that decrements the carrier tag toward exit in ≤ 2 outer iterations.
- Because A-4 hasn't landed, this program currently **rejects** at
  `compile.rs:2725` — so the A-3 *unit* half asserts the tree fold produces the
  correct multiplied `B` when handed the (merged) inner term directly (a
  white-box test on `Lowered::wcet` with a hand-built nested `CostTree`), and the
  *end-to-end* half is deferred to A-4 where it can actually compile.
- **The pinned counterexample regression** (mandatory, from BLOCKER-2):
  synthesize the k=4 / k=100 / iter=12 nested term and assert
  `wcet() ≈ 4800`, not `≈ 88`. This is the reproducing test for the finding:
  fails against the flat sum, passes against the tree fold.

---

## 2. A-5 — occ-aware nested come-from frame stack

### 2.1 The aliasing problem (MAJOR-4), exactly

`lower_trace` hand-assigns its four aux registers by *position*, above the
carrier, **without consulting occ**:

```rust
// compile.rs:2828-2841
let max_carrier_reg = body_ctx.homes.iter()… .max().unwrap_or(0);
let aux_base = max_carrier_reg + 1;
let rc:   u8 = aux_base;       // come-from join catch
let rb:   u8 = aux_base + 1;   // route
let temp: u8 = aux_base + 2;   // token carrier (δu)
let ctrl: u8 = aux_base + 3;   // dispatch predicate
```

This is safe *today only because* the **F-1 guard** rejects any Trace whose
context is non-canonical or occupied:

```rust
// compile.rs:2621-2628
let canonical = Ctx::new(ctx.homes.len());
if ctx.homes != canonical.homes || ctx.occ != OccSet::from_locations(&ctx.homes) {
    return Err(Deferred("Trace under a non-canonical or occupied (Times sibling) …"));
}
```

Under `Times(Trace, g)`, `split` copies the full parent occ into the left half
whose homes are still `Reg(0..n_left)` — indistinguishable from canonical by
`homes` alone, which is why F-1 also checks `occ`. Without F-1, `aux_base =
max_carrier_reg + 1` would land on **g's live registers** and clobber them; the
`BR/s0/s1` terminal check (`compile.rs:3389`) can't see data corruption, so the
program silently returns wrong output (the recorded F-1 failure). **This is why
A-5 must precede A-4.**

For **nested Trace**, the same fixed `rc/rb/temp/ctrl` positions would collide:
the inner loop reuses the outer's aux slots, and the outer's *in-flight come-from
token* `δu` (held in `temp`) would be destroyed every inner iteration.

### 2.2 The come-from token δu — how it is minted / stored / restored

The skeleton (`compile.rs:2851-2922`, docstring §2528-2544):

- **δu** = `256 - iter_len (mod 256)` (`compile.rs:2821`) — the backward-jump
  distance. Minted into `temp` by `ADDI temp, δu` (prologue `compile.rs:2858`;
  and per-iteration at `compile.rs:2895`), burned by `SUBI temp, δu`
  (`compile.rs:2871`, `2914`).
- `Ltop: SWAPBR rc` (`compile.rs:2863`) — on the backward re-entry, `rc ← δu`
  (the come-from witness); on first entry `rc = 0`.
- `CSWAP ctrl, rc, temp` (`compile.rs:2865`) — join-dispatch routes the witness.
- `Lend: SWAPBR rb` (`compile.rs:2910`) — continue (`BR=δu`) jumps back, exit
  (`BR=0`) falls through.

The token lives in a **register (`temp`)** for the whole loop. An inner loop that
reuses `temp` overwrites the outer's in-flight token → the outer back-edge
mis-fires. This is synthesis §2 Axis-3's highest-risk obligation (ii): *"the
outer loop's pending δu/come-from must be provably preserved across the ENTIRE
inner loop."*

### 2.3 The frame-stack design

Replace the fixed `aux_base` positional assignment with a **per-level frame**
drawn occ-awarely, reusing the proven page-in/page-out net-zero bracket
(`allocate_aux`, `compile.rs:635-727`; and the synthesis Axis-2
`with_scratch_frame` calling convention):

1. **Draw the 4 aux registers (`rc/rb/temp/ctrl`) from genuinely-free locations**
   via `allocate_aux(ctx, 4, exclude=carrier_leaves)` (or a `with_scratch_frame`
   generalization) — consulting `ctx.occ`, never `max_carrier_reg+1`. This makes
   `Times(Trace, g)` safe: g's leaves are occ-marked, so the frame skips them.
2. **Each loop level gets a disjoint frame.** For nested Trace, the outer draws
   its frame first (occ-marks it), lowers the body *with those slots occ-marked*,
   so the inner `allocate_aux` draws a **disjoint** frame. Disjoint-by-depth
   storage; the outer's `temp`(δu) is never touched by the inner loop.
3. **BR is a time-shared channel, not per-level storage.** Only one level's
   `SWAPBR` is at the PC at any step (synthesis §2 Axis-3). The come-from *token*
   is per-level (in-frame); the BR *register* is shared but only carries the
   active level's value for the one step its SWAPBR executes. **A-5's crux
   obligation** (§7 review finding): prove the outer δu in-frame is untouched
   across every inner step and correctly re-loaded at the outer back-edge — BR is
   carried into PC every step (`PC += BR + 1`), zeroed by SWAPBR only at
   straight-line end.
4. **Reuse spill's strength for SAVE/restore** (the A-1 finding): the frame is
   created-zero at entry / consumed-to-zero at exit (ancilla discipline). Frame
   words never cross a back-edge holding live data — the safe side of the M-D.e
   F5.2 line. The `allocate_aux` page-in/page-out bracket already gives exactly
   this net-zero SAVE/restore for free (`compile.rs:690-717`).

**Depth bound.** Max nesting depth = scratch/frame capacity (a sized-once
parameter). With `NUM_SCRATCH` scratch slots and 4 aux per level, depth ≈
`(NUM_REGS + free scratch − carrier) / 4`. State this bound explicitly and reject
(don't corrupt) beyond it.

### 2.4 A-5 invariant

**Frame-stack disjointness across back-edges**: inner frame ∩ outer come-from
state = ∅, at every step (§7 gate: "the A5 gate"). Plus the existing Trace
layout invariant `homes_in == homes_out` (`compile.rs:2731-2736`) must still hold
per level. And the walk's marshal-layout == body-home-layout equality
(`compile.rs:2758-2765`) must be preserved — A-5 relaxes F-1 for the *aux*
placement but must NOT relax the input-home placement the walk depends on (the
docstring at `compile.rs:2763` warns exactly this: "A future relaxation of the
F-1 guard allowing relocated input homes → unsound k_exact").

### 2.5 A-5 reproducing tests

- **`Times(Trace, prim)` — the aliasing regression (mandatory, MAJOR-4/F-1).**
  Smallest: `Times(Trace(Bit, id_body), bit_flip_prim)`. Currently rejected by
  F-1 (`compile.rs:2621`). After A-5 it must compile and `run_on_cpu == eval`
  over the whole (small) domain, with the sibling `bit_flip` factor producing the
  **correct** output (the exact thing F-1's recorded failure got wrong). Fails
  without occ-aware allocation (sibling clobbered), passes with it.
- **Nested Trace — the frame-disjointness regression.** Smallest: outer
  `Trace(Bit, body)` whose body contains inner `Trace(Bit, inner_body)`, both
  with tiny variable-T exits. Currently rejected by `contains_trace`
  (`compile.rs:2631`). After A-5+A-4 must compile and `run_on_cpu == eval`; the
  authority test asserts the outer token survives across the inner loop (probe:
  run enough inputs that the outer takes ≥ 2 iterations *with the inner loop
  running each time* — the exact "outer δu preserved across ENTIRE inner loop"
  scenario).

---

## 3. A-4 — delete the nesting rejections (ONLY after A-5)

### 3.1 The reject sites (every one), and what each rejects

| # | file:line | rejects | why (today) |
|---|---|---|---|
| R1 | `compile.rs:2630-2635` `contains_trace(body)` | **nested Trace** | come-from join doesn't compose (fixed by A-5 frame stack) |
| R2 | `compile.rs:2621-2628` F-1 canonical/occ guard | **Trace under Times sibling / non-canonical ctx** | hand-assigned aux aliases live data (fixed by A-5 occ-aware alloc) |
| R3 | `compile.rs:2725-2729` `has_inner_control` (trace body) | **any Swapbr-containing body** = Iter/Plus/branch inside a Trace | forward skip-blocks inside a loop need compositional S_max (fixed by A-3 tree s_max + A-5 frames) |
| R4 | `compile.rs:2490-2494` `has_inner_control` (iter body) | **nested skip-block in iter body** = Iter-in-Iter, Plus-in-Iter | reservation ordering (fixed by A-3 + A-5) |
| R5 | `compile.rs:3154-3158` `has_inner_control` (Plus arm) | **branch-in-branch**, loop-in-branch | reservation ordering (fixed by A-3 + A-5) |
| R6 | `compile.rs:1975-1979` `has_inner_control` (controlled idiom) | **doubly-nested skip-block** in the controlled-op idiom | reservation ordering |

`has_inner_control` itself is `instrs.iter().any(Swapbr)` (`compile.rs:773-775`).

### 3.2 Order & partial deletion

A-4 deletes R1, R2, R3, R4, R5, R6 **only after** A-3 (so `B`/`s_max` fold is
sound for what the deletions admit) **and** A-5 (so aux allocation is occ-aware
for what R1/R2 admit). Deleting R2 (F-1) before A-5 is the specific MAJOR-4
hazard — do not.

Some rejects may need to stay as **reject-don't-corrupt depth guards** rather
than full deletions: e.g. beyond the frame-stack depth bound (§2.3), or when the
tree-fold `B` exceeds `2^W_pc` (the MINOR finding: a synthesized prim or deep
nest pushing `iter_len` past 256 — that needs A-1's widen first). Convert
"reject nesting" into "reject when the sound budget doesn't fit," never a silent
pass.

### 3.3 A-4 invariant

Every deletion must be **backed by a green end-to-end test** that the deletion
*enables* and that would corrupt without A-3+A-5. No reject is removed without
its reproducing nested program compiling + `run_on_cpu == eval`.

### 3.4 A-4 reproducing tests (the smallest program per deleted reject)

- R1 (nested Trace): nested `Trace(Bit, Trace(Bit, …))` from §2.5.
- R3 (Iter-in-Trace): the Iter-under-Trace program from §1.7 — now compiles;
  assert its `B` matches the tree fold and `run_on_cpu == eval`.
- R5 (branch-in-branch): `Plus(Plus(f,g), Plus(h,k))` on a nested Sum type.
- R4 (Iter-in-Iter): `Iter(n, Iter(m, prim))`.

Each is a mandatory regression: it stays in the suite and would fail (reject or
corrupt) without the A-3+A-5 machinery.

---

## 4. δu / N_rom vs B / W_pc — DECOUPLED (BLOCKER-3 confirmed)

**They are two different quantities and are decoupled** (confirmed against
`a6.4-widen-pc-design.md` §1c, §2, §3.4):

- **δu is a STATIC ROM back-jump offset.** `δu = 256 − iter_len (mod 256)`
  (`compile.rs:2821`), typed `u8`, minted/burned via `Addi/Subi{imm:u8}`,
  swapped into BR by `Swapbr`. Its magnitude ∝ `iter_len ≤ N_rom`. Width needed:
  `A_br = ⌈log₂(max δu + 1)⌉` — small; single short loops keep `W_br = 8`.
- **B is a DYNAMIC step count.** `B = k_max · iter_len` (per §1.2, now multiplied
  through nesting). It is the number of PC cycles and is capped by P2 at `2^W_pc`
  (`compile.rs:3366`). Width needed: `W_pc = ⌈log₂(B + S_max)⌉`.

For nested loops `B ≫ N_rom` (a 12-instr inner body run 4×100 times: N_rom ≈ 20,
B ≈ 4800). So **W_pc must widen for nested B even when δu stays tiny** — the
design must not assume `W_pc ≥ W_br` nor conflate them. `a6.4-widen-pc-design.md`
already scopes the coordinated widen (`pc:u8→u16`, `br:u8→u16`,
`Addi/Subi{imm}`, `pc_add_br` `n=2*W` at `step.rs:170`, `256`→`1<<W_pc`).

**Cross-slice dependency (MINOR).** The nested-control slices produce large `B`.
If a real nested program's `B + S_max` exceeds 256, its end-to-end authority test
**cannot pass until A-1 (widen-PC) lands**. Keep the A-3/A-5 reproducing programs
**small enough that `B ≤ 256`** (Bit carriers, k ≤ 2–3, tiny inner counts) so
they exercise the *structure* without needing the widen. The k=4/k=100 BLOCKER-2
counterexample is a **white-box `wcet()` assertion** (no CPU run), so it needs no
widen. This lets A-3→A-5→A-4 land and be authority-tested **before** A-1.

---

## 5. ADR-0034 / injectivity impact of nesting

**The single-step bijection proof survives; the composition proof gets new
obligations.**

- The `step : State ↔ State` bijection (`step.rs:47-60`) is a property of the
  **step iso**, not of the ROM. Nesting changes only the ROM contents, not the
  step netlist — so the D1–D4 argument (no instruction writes PC; `A: PC ↦ PC +
  BR + 1` is a bijection; `step = A ∘ E`) is **unchanged**. Nesting does not add
  a new op, decoder arm, or state field (synthesis §2 Axis-3: "skip-blocks become
  depth-tagged frame levels in the same fold" — no hardware change).
- **What must be re-argued** (all *program-level*, oracle-checked, synthesis §8
  obligation (b)/(c)):
  1. **Come-from join composes across depth.** The M-D.d claim "come-from join
     does not compose" (`compile.rs:2633`) is precisely what A-5's frame stack
     must overturn: prove each level's `Ltop/Lend` SWAPBR pair is a self-inverse
     bracket whose token lives in a disjoint frame, so the nested brackets
     compose to a bijection of the fiber. Authority = nested-Trace
     `run_on_cpu == eval` + `eval^R` reverse check + BR/s0/s1 terminal quiescent.
  2. **Frame ancilla discipline** (synthesis §8(b)): each frame is created-zero /
     consumed-to-zero, so it does not leak into the injectivity of the outer
     level. Verified by the net-zero page-in/page-out bracket + terminal check.
  3. **Nested WCET fold soundness** (synthesis §8(c)): `B_fold ≥ true_B` so park
     is idle (identity) for `B − T` steps — the park total-bijection invariant.
     This is the deepest re-argument (a *soundness* property): prove the tree fold
     **cannot under-count** (§7 adversarial target #1).
- **Width re-proof (BLOCKER-3, deferred to A-1).** When A-1 widens `W_pc/W_br`,
  ADR-0034's width-parametricity must be re-proven at the new widths (`pc_add_br`
  `n=2*W`, the `256`→`2^W_pc` wrap). This is A-1's obligation, not A-3/A-5/A-4's,
  but the nested slices *depend* on it for any `B > 256` program.

---

## 6. Review gates (per CLAUDE.md, L15/L20)

All three slices touch the daemon-equivalent "highest bar" code (`lower_trace`,
the gate's WCET/park math). Per CLAUDE.md non-negotiables:

- **A-3** (WCET/park math — soundness): fresh-context adversarial review (prefer a
  different model from the implementer) targeting *"construct a nested program
  where the fold under-counts B → park wrap → corruption."* Ship the BLOCKER-2
  counterexample as a reproducing white-box test (fails on flat sum, passes on
  tree fold) **and** a behavior-preserving test proving flat-program `B` is
  byte-identical to today.
- **A-5** (`lower_trace` aux allocation — TOP bar, highest-risk in the design):
  fresh Fable-class review targeting frame-disjointness across back-edges. Ship
  the `Times(Trace, prim)` aliasing regression + nested-Trace `run_on_cpu==eval`
  real-worktree authority test (a REAL nested program that currently rejects →
  compiles + matches eval). Per L15, the end-to-end worker→gate→round-trip on a
  real worktree is the minimum bar; per L20 every finding the review surfaces
  ships with its own reproducing test.
- **A-4** (deleting rejects): each deletion backed by the nested program it
  enables (§3.4), each a permanent regression. Review confirms no reject was
  removed without A-3+A-5 covering its failure mode.

---

## 7. Open questions

1. **CostTree substrate (A-3):** nested tree (option A) vs pre-scaled flat vec
   (option B)? B is a smaller diff and sound; A preserves structure for future
   tightening. Recommend B for A-3, revisit if A-7 prim-synthesis needs the tree.
2. **Mini-interpreter vs cardinality-product (A-3 walk):** the back-edge-honoring
   stepper is tight but is new code that must itself agree with `system_step`
   byte-for-byte (else k_max is wrong). The cardinality-product fallback is
   trivially sound but loose (may push small nested programs over 256, forcing
   A-1 earlier). Recommend: cardinality-product first (unblocks A-4), add the
   mini-interpreter as a tightening once A-1 lands.
3. **Frame depth bound (A-5):** exact formula for max nesting depth given
   `NUM_REGS`/`NUM_SCRATCH`/carrier size, and whether deep nests should spill
   frames to memory (`allocate_aux` already can, `compile.rs:706-712`).
4. **BR time-sharing proof (A-5):** the crux obligation — is a paper proof that
   "only the active level's SWAPBR is at the PC" sufficient, or does the review
   demand an exhaustive nested-back-edge trace? Recommend the latter as the
   authority test.

---

## 8. Blockers / surprises

- **A-1 (widen-PC) is a soft dependency, not hard.** Any nested program with
  `B > 256` cannot pass its end-to-end authority test until A-1 lands. Mitigation:
  keep A-3/A-5/A-4 reproducing programs small (`B ≤ 256`); the BLOCKER-2 fold
  counterexample is a white-box `wcet()` assertion needing no CPU run. So the
  A-3→A-5→A-4 *structure* can land and be authority-tested before A-1 — but the
  slice writeups must say so, or a reviewer will (correctly) reject a nested test
  that can't fit 256.
- **Iter-under-Trace is caught by the wrong guard today** (§1.4): rejected by
  `has_inner_control` (`compile.rs:2725`), not `contains_trace`. A-3's test must
  nest Iter/Trace so the multiply bug is visible; a Seq-sibling test would pass
  vacuously and hide it until A-4.
- **The general-prim synthesizer (gap 2 / A-6) is orthogonal** and being
  integrated separately — no dependency in either direction for A-3/A-5/A-4, EXCEPT
  the MINOR coupling: a synthesized prim used *inside* a nested loop can push
  `iter_len` past 256 and must land after A-1. Not a blocker for the nested-control
  structure itself.
- **The walk's marshal==body-home equality is load-bearing and fragile**
  (`compile.rs:2758-2765`): A-5 relaxes F-1 for aux placement but MUST keep input
  homes canonical, or the k_max walk goes unsound. Call this out in the A-5 review
  as a specific "did you break the walk's assumption?" check.
