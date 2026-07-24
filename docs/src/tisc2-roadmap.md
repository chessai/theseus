# TISC-2 Roadmap — the CPU that runs arbitrary well-formed theseus programs

> **Status (2026-07-24):** Design **complete, adversarially reviewed (3 passes), and its
> keystone empirically validated**. Beginning **Phase A**. This roadmap supersedes the
> earlier "T-loops → widen-PC → GCD" retrofit plan (which was symptom-chasing; see the pivot
> below). TISC-1's execution paradigm is **kept and proven**; TISC-2 is a *provisioning +
> compiler + datapath* redesign, not a new machine.

## Why (the mandate)
Run **any well-formed theseus program** — the closed 22-combinator Iso algebra over finite
trees — correctness first, **efficiency (energy/step) critical**, and **no more whack-a-mole**
for any part of the CPU. TISC-1 hit a sequence of walls (GCD blocked → widen PC → …); the
root causes were **register pressure**, **non-compositional lowering**, and a **recognizer that
can't lower arbitrary prims** — not a paradigm limit. Design docs (committed under
`docs/scratch/`): `arbitrary-theseus-cpu-design-framing.md` (energy model + 8 axes + the
efficiency principle), `arbitrary-theseus-cpu-requirements.md` (the closed algebra + gap
ledger + WCET verdict), `tisc2-design-synthesis.md` (**the design**, incl. §7 review findings
and §8 the keystone), `tisc2-synthesis-notes.md` (the 4-proposal panel).

## The north-star principle (how every decision is judged)
**A fix that adds an instruction, a decoder arm, a state field, or PC/word bits taxes the
energy of _every_ step of _every_ program, forever. A fix that is a compile-time discipline or
a shared datapath adds _zero_ standing energy tax.** Prefer the latter always. (Measured: a new
op cost +9% step energy just by existing; each PC bit ~+10–15%.) This is the anti-whack-a-mole
rule, applied at architecture scale.

## The design in one paragraph
Keep the flat-ROM, time-multiplexed, fixed-WCET reversible step (the paradigm-stress-test
proved *why*: a shared step re-ramped B times is the reversible realization of **bounded
hardware / unbounded time** — a spatial unroll blows up on the loop-heavy programs that are the
whole point). Make its **lowering compositional** (a frame discipline + a tree-structured WCET
fold → nesting composes by construction) and its **datapath shared** (one configurable ALU →
per-step energy stops scaling with the instruction set). Lower **any** prim by transcribing its
`BvExpr` spec to gates (the keystone). Reuse the value model, the injectivity proof, the ring
closure, and the differential oracle.

---

## Phase A — COVERAGE: run every well-formed program *correctly*
Slices in dependency order (value front-loaded). Every slice is gated by the differential
oracle (`run_on_cpu == eval`); control-flow / lowering slices (the highest bar) additionally
require a **fresh-context adversarial review + a real-worktree behavioral test** that reproduces
the specific scenario (CLAUDE.md / L15 / L19–L21). Numbering maps to the synthesis doc's A-labels.

**A-1 · Loop-body scratch frame → the first variable-T loop (GCD).** [synthesis A2]
Generalize the proven `with_registers` net-zero page-in/out bracket into a recursive
`with_scratch_frame(K)` (K private scratch words per context, `body_occ`-aware, zero at
entry/exit). Fill the `gcd2_hist_step` emission against the pinned `gcd2_hist_bytes` oracle. The
2-bit subtractive GCD body shrinks 60→28 instrs ⇒ **B=194 < 256 ⇒ ships with NO widen-PC, no new
op, zero standing energy tax**. *Gate:* POC (body ≤ 40, `run_on_cpu_fast == eval` on all inputs,
B < 256) → adversarial review → real-worktree `system_step == eval` across the GCD trajectory
(≤5 back-edges) → full heavy-tier suite → energy/ring golden re-pin → merge. *Value:* the
original goal; validates the register-pressure cure. *Risk:* HIGHEST BAR (`lower_trace`).

**A-2 · Coordinated address-width widen (B > 256).** [synthesis A1, corrected per review BLOCKER-3]
**Decouple two different quantities the first draft conflated:** the come-from token `δu` is a
*static ROM offset* (widen with `log₂ N_rom`); the WCET budget `B` is a *dynamic step count*
(widen `W_pc`). Widen PC/BR/imm and `pc_add_br`/`pc_inc` **together** (this is NOT a "pure
param" — the encoder, state, and step iso hardcode 8-bit) and **re-prove the ADR-0034
total-bijection injectivity at the new widths**. Fold in the **narrow-A fetch** (compare only the
low ⌈log₂ B⌉ PC bits) so a wider address space costs ∝ log(reachable PC), not the full width.
*Gate:* injectivity re-proof + every existing program `run_on_cpu == eval` + a synthetic B>256
program. *Risk:* HIGH (state layout + proof).

**A-3 · Tree-structured WCET/S_max fold + back-edge-honoring k_max walk.** [synthesis A3, corrected per BLOCKER-2]
Replace the flat sibling-sum WCET (`compile.rs:2103`) with a **recursive tree fold**
(`wcet(Trace)=spine + k_max·wcet(body)`, Seq/Times = Σ, Plus = stage + both arms). Make the
exact-k_max trajectory walk **honor inner back-edges** (a real mini-interpreter, not straight-line
`exec_instr`) and **merge the currently-dropped inner `body_meta` scaled by outer k_max** — else
nested `B` under-counts (a 4×100 nest counted as 88 vs real 4800) → park-wrap → silent corruption.
Behavior-preserving on flat programs (must reproduce current B exactly — the green oracle is the
guard). Keep WCET *and* `s_max` both Σ over Plus arms (do not tighten `s_max` to `max`). *Gate:*
all flat programs unchanged + a nested-loop B computed correctly. *Risk:* HIGH (soundness).

**A-4 · Occ-aware nested come-from frame stack.** [synthesis A5 — THE crux]
Replace the hand-assigned `aux_base = max_carrier_reg+1` with occ-aware allocation from
`with_scratch_frame`; each loop level's `{rc,rb,temp,ctrl,δu}` lives in its own frame (disjoint
by depth). **Prove the outer loop's pending come-from/δu survives the entire inner loop** (the
inner SWAPBR touches the shared BR every inner step). *Gate:* fresh adversarial review +
real-worktree nested-Trace `system_step == eval` across real back-edges, terminal-quiescent.
*Risk:* HIGHEST BAR (control-flow state machine). **Must precede A-5.**

**A-5 · Delete the nesting rejections.** [synthesis A4]
Remove the four `has_inner_control` guards + the F-1 `Times` reject. Nested Trace, loop-under-⊗
(`Times(Trace,g)`), and branch-in-branch now compose (A-3 gives correct WCET; A-4 gives disjoint
storage). Skip-blocks become depth-tagged frame levels in the same fold. *Gate:* Plus-in-Plus,
controlled-in-loop, `Times(Trace,g)`, nested Trace all `run_on_cpu == eval`, park-safe. *Risk:*
HIGH (correctness follows A-3 + A-4).

**A-6 · General-prim synthesis (recognizer → arbitrary).** [synthesis A6 — the keystone, POC-validated]
Build the `BvExpr → gates` transcriber. Table prims (≤12 rails): the existing enumeration
`synthesize_prim`. **Wide/symbolic prims: transcribe the `BvExpr` spec directly** (Bennett
compute-uncompute via the mandatory forward+backward spec; `Xor→`CNOT, `And→`Ccnot, `Mux→`Cswap;
polynomial because `BvExpr` omits multiply), **verified symbolically** by composing the existing
`symbolic_exec` + `equiv` (no enumeration, any width — validated:
`tests/bvexpr_synth_poc.rs`). The recognizer demotes to an optional peephole cache. Route
non-identity structural isos (AssocSum/Distrib/Factor) + body permutations through it. *Gate:* an
arbitrary admitted, unrecognized, wide-symbolic bijection lowers + symbolic-verifies +
`run_on_cpu == eval`. *Risk:* MEDIUM (mechanism validated). **Sequence any in-loop synthesized
prim after A-2** (its gate count can push B past the old 256).

**A-7 · Wide data words (Flat(n>8)).** [synthesis A7]
Parameterize the data word width `W` (decoupled from `W_pc`). *Gate:* a Flat(16) reduction
`run_on_cpu == eval`. *Risk:* MEDIUM.

**→ After A-7:** the reject surface has no reject-to-be-safe path for any well-formed program
whose prims are **structured** (BvExpr-expressible = everything real programs use). The only
residual is information-theoretically-random permutations, for which *no* method beats 2ⁿ gates.
**The machine runs arbitrary well-formed theseus programs.**

---

## Phase B — EFFICIENCY: per-step energy scales with the *active* instruction
Each slice **gated on a real `lower_iso → joules` measurement before commit** (user directive:
"fully measured"). Whack-a-mole is already gone after Phase A (a small fixed basis + synthesis
means no ops are ever added); Phase B is a further, optional efficiency optimization of an
already-fixed machine.

**B-1 · Shared configurable ALU.** Replace `decode_execute = seq_balanced(Op::ALL.map(instruction))`
(all ~15 op-netlists run masked every step, `step_j = O(|ISA|)`) with one shared 3-slot crossbar +
O(log|ISA|) control decode + gated **shared** cores (add±dir sharing one adder, xor, rotl±dir,
and-family), reusing the verified gated cores. Take the **all-blocks-gated form** (preserves the
single fixed `step` iso the ring-closure proof depends on; honest ~2–4× step_j + kills the
ISA-growth tax). *Gate:* **measured** step_j drop via `lower_iso → joules` **before commit** +
full adversarial review (datapath = daemon-equivalent control surface) + whole-domain per-opcode
oracle. *Risk:* HIGH.

**B-2 · Per-program widths.** Per-program data-word `W` and per-program-effective PC width
(pay-for-live-width; fib keeps its 5-bit-effective PC). *Gate:* measured reduction + oracle green.

**B-3 · (Deferred / optional) active-only microcode.** Only the current op's gates energized per
step (a larger win than B-1) — pursue **only** if reconciled with the ring-closure proof (it
breaks the single-fixed-step-iso). Not needed for the mandate.

---

## Cross-cutting: discipline & proof obligations
- **Highest-bar slices** (A-1, A-2, A-4, B-1 — control-flow/datapath/layout): fresh-context
  adversarial review by a different model + a reproducing real-worktree behavioral test, before
  merge. Reviews committed to `docs/reviews/`. Commit-only; never push.
- **Standing proof obligations** (all discharged by the existing differential oracle + bveq):
  (1) nested WCET-fold soundness (park-safety on nested programs — the deepest); (2) frame-stack
  disjointness across back-edges; (3) synthesized-prim faithfulness (symbolic bveq); (4) shared-ALU
  per-opcode bijectivity + mutually-exclusive decode. Each ships with a regression test that
  reproduces the specific counterexample (nested Iter/Trace under-count; Times(Trace,·) aliasing).
- **Guard the `Mul`-free `BvExpr` invariant** — it is what makes general prim synthesis polynomial
  and verifiable.

## The pivot that produced this roadmap (for the record)
The prior plan (widen-PC to unblock the GCD) was **symptom-chasing**: the GCD was blocked by
*register pressure*, not PC width; widening the PC treats the symptom and pays a permanent energy
tax while the next program hits the next wall. A scope-audit + a 4-proposal design panel + 3
adversarial reviews replaced it with this holistic design. The GCD now falls out of **A-1** with
no widen-PC at all — as an early proof-point, not the goal.
