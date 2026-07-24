# TISC-2 — the CPU that runs arbitrary well-formed theseus programs (SYNTHESIS)

Synthesis of a 4-proposal design panel (compositional-algebra, energy-first, maximal-reuse,
paradigm-stress-test) against the committed framing + requirements. This is the advisor's
recommended design. Companion docs: `arbitrary-theseus-cpu-{design-framing,requirements}.md`,
`scratch-frame-gcd-estimate.md`, `tisc2-synthesis-notes.md` (the four distilled proposals).

## 0. Verdict (one paragraph)
Keep TISC-1's execution paradigm — the flat-ROM, time-multiplexed, fixed-WCET reversible step — and
make its **lowering** compositional and its **datapath** shared. All four independent proposals
converged on this, and the paradigm-stress-test proved *why* from first principles: a shared step
re-ramped `B` times is the reversible realization of **bounded hardware / unbounded time**, the exact
property loop-heavy programs need; a spatially-unrolled machine (which already exists as `lower_iso`
and handles nesting for free) forfeits temporal gate-reuse and blows up multiplicatively on the nested
loops that are the whole point. So TISC-2 is a **provisioning + compiler + datapath** redesign, not a
new machine. It closes **every** coverage gap (nested control, general prims, wide words, WCET fields)
and attacks the **structural energy leak** (per-step cost ∝ program size), in two ordered phases —
**coverage first, efficiency second** — each slice gated by the differential oracle and (for
control-flow/datapath changes) a fresh-context adversarial review + a real-worktree behavioral test.

## 1. The decision to keep the paradigm (first-principles, from P4)
A theseus program is a closed 22-combinator algebra of total bijections over finite trees; a
structural reversible lowering of it *already exists* — `theseus_netlist::lower_iso` (lower.rs:1527),
which recurses through nested Trace/Iter/Fold via `controlled_apply` with **no `has_inner_control`
guard**. This proves two things: (a) **the nesting rejections are artifacts of the ROM flattener
(`compile.rs`), not the algebra or the gate model** — nesting is a *lowering* problem, solvable; and
(b) a fully-spatial machine is real and energy-measurable but **loses**: it materializes every
unrolled body in space (Σ over all iterations, multiplied through nesting), so a 2^24 whole-domain
loop becomes a 2^24-cell netlist. Time-multiplexing — one shared `step` netlist re-energized `B`
times — is precisely what lets bounded hardware run an arbitrarily long WCET. **We keep it.**

## 2. Architecture (the converged design + resolutions), by axis

**Axis 1 — Value model (reuse, one widen).** Keep the leaf-byte slot-tree: a value of type `T` is a
vector of `count_leaves(T)` words (Sum = tag leaf + payloads zero-extended to `max(|a|,|b|)`; Prod =
concatenation; Flat(n) = one word). All structural monoid isos (Swap/Assoc/Unite) are gate-free
`homes` permutations (0 cells — rewiring is free). Genuine reshuffles (non-identity
AssocSum/Distrib/Factor) route through the general-prim synthesizer (Axis 4). Bit-identical to today ⇒
the differential oracle and every prim table stay valid.

**Axis 2 — Working store: a frame discipline.** Generalize the proven `with_registers` net-zero
page-in/page-out bracket into a recursive `with_scratch_frame(K)` calling convention: any context
(loop body, branch arm, ⊗ factor, sub-iso) gets `K` ephemeral frame words allocated from scratch
(base chosen by consulting `body_occ`, never hardcoded), created-zero at entry / consumed-to-zero at
exit (ancilla discipline). Frame words never cross a back-edge with live data ⇒ the safe side of the
M-D.e F5.2 line. Zero standing energy tax (no op, no decoder surface, no state field). This is the
store half of nested control and the cure for register pressure (proven: GCD body 60→28, B=194).

**Axis 3 — Control: frame stack + tree-structured WCET fold (THE crux).** Nested control was rejected
for **two distinct root causes**, and both must be fixed:
- *Storage:* the come-from skeleton (`SWAPBR rc` join, `CSWAP` disambiguation, token `δu`) hand-assigns
  fixed aux registers `rc/rb/temp/ctrl`; an inner loop aliases the outer's in-flight come-from state.
  **Fix:** draw each loop level's `{rc,rb,temp,ctrl,δu}` from an Axis-2 frame → per-level storage,
  disjoint by depth. Only one level's SWAPBR is at the PC at any step, so BR is a *time-shared* channel.
  - **[REVIEW FINDING — A5's crux obligation, do NOT gloss]:** the first draft claimed "BR returns to 0
    every step" — IMPRECISE. BR is carried into PC every step (`PC += BR + 1`) and is only zeroed by
    SWAPBR at the end of *straight-line* execution; the come-from token `δu = 256 − iter_len` is an
    **8-bit backward-jump distance tied to the 256 PC wrap** (compile.rs:2763; minted `ADDI temp,δu`,
    burned `SUBI temp,δu`). Two obligations A5 MUST discharge (and a re-run review must verify): (i) **δu
    widens with W_pc** — a nested body whose `iter_len` is a multiplied cost can exceed 256, so δu, the BR
    field, and the mint/burn arithmetic must be W_pc-wide and the wrap constant → `2^W_pc`; (ii) the
    **outer loop's pending δu/come-from must be provably preserved across the ENTIRE inner loop** (it lives
    in the outer frame, but the inner loop's SWAPBR touches the shared BR every inner step — prove the
    outer δu in-frame is untouched and correctly re-loaded at the outer back-edge). This is THE highest-risk
    correctness obligation in the design.
- *Accounting (the deep bug P3 found):* `wcet()` is a **flat sum over sibling loops**
  (`compile.rs:2103`); a nested loop's body must be **multiplied** by the outer count. The flat sum
  *under-counts* `B` for nested programs → breaks park-safety and the total-bijection park invariant.
  **Fix:** make `wcet`/`s_max`/`max_skip_distance` **tree-structured folds**: `wcet(Trace)=spine +
  k_max·wcet(body)`, `wcet(Seq)=Σ`, `wcet(Times)=Σ`, `wcet(Plus)=stage+both-arms` (or `max`, a sound
  tightening), recursing through the body. This single refactor (Lowered/LowerMeta loop-term vec →
  tree) is what makes nested park-safety sound, and it *also* fixes branch-in-branch: skip-blocks
  become depth-tagged frame levels (`k_max=1`) in the same fold, deleting the four `has_inner_control`
  rejections. Max nesting depth = scratch/frame-size, a sized-once parameter. Loop-under-⊗ composes:
  the sibling's leaves are occ-marked, the loop's frame base skips them.

**Axis 4 — ISA + decode + general prims.** Two independent changes:
- *General prims (coverage, closes the deepest gap):* TISC-1 is a *recognizer*; an arbitrary admitted
  bijection has no lowering (`UnrecognizedPrim`). Add a **universal reversible-gate synthesizer**: any
  admitted finite bijection lowers to a circuit over the **already-universal** basis
  {Ccnot(Toffoli), Cswap(Fredkin), Xor, Xori} — so **no new machine surface, no new op, zero decode
  tax**; completeness is a new compile-time *pass*, not new hardware (P3's key insight; the synthesis
  route is `lower.rs`'s whole-domain-verified `synthesize_prim`, P4). The recognizer stays as the
  **fast path** (tuned cheap lowerings); the synthesizer is the **completeness path**. *Verification
  caveat:* whole-domain enumeration is tractable only at small leaf-width; for wider prims verify via
  symbolic `bveq` against the PrimSpec (both already exist). *Cost caveat:* a maximally-mixing bijection
  is worst-case exponential in gates — confined to the programs that use such prims, never a per-step
  tax.
- *Shared configurable ALU (efficiency, breaks the O(|ISA|) tax):* today `decode_execute =
  seq_balanced(Op::ALL.map(instruction))` runs all 15 op-netlists masked every step — each with its own
  crossbar (~56 K1/operand) + predicate chain + core → `step_j = O(|ISA|)`, the ISA-growth tax
  (measured: Ccnot +9.15%/+1.91% on every program). Replace with ONE shared 3-slot crossbar + decode
  into O(log|ISA|) control lines + gated **shared** cores (add±dir sharing one adder via `sub=adjoint`,
  xor, rotl±dir, and-family), reusing the exact verified gated cores. **Resolution of the P1/P2
  tension:** take P2's conservative *all-blocks-present-but-gated* form, which preserves the SINGLE
  fixed `step` iso that the ring-closure proof depends on — honest ~2–4× step_j + kills the growth tax.
  P1's active-only microprogram-ROM (only the current op's gates energized) is a larger win but breaks
  the single-step-iso; defer it as a future option gated on reconciling it with the ring proof.

**Axis 5 — Fetch/PC/ROM.** Decouple `W_pc` (PC/BR/ROM-addr/skip/δu) from `W` (data word). Widen `W_pc`
8→16 once (sized generously; the only real WCET-representation limit). Fold in **narrow fetch**
(validated this session: compare only the low ⌈log₂B⌉ PC bits) so the wider address space costs
∝ log(reachable PC), not 16 bits — the widen's standing tax is sub-linear. `W` (data word) is
per-program = max Flat(n) in the program (closes Flat(n>8); fib compiles W=8, a Flat(4) program
compiles W=4).

**Axis 6 — Memory (reuse verbatim + widen).** Keep the composite-ring two-body memory + closure
(Theorem 3.5, zero-garbage) and `MexchReg`/`memexch` untouched — the crown-jewel proof. Widen
`MEM_WORDS` once. No heap needed (finite, statically-shaped values). Address decode is already
O(log MEM_WORDS).

**Axis 7 — WCET/park/energy.** WCET = the Axis-3 tree fold, decidable for every well-formed program
(requirements §4). Park unchanged (idle B−T), now with a sound nested `B`. exact-k_max kept, extended
to nested bodies. Total energy `B·step_j` attacked on both factors (§3).

**Axis 8 — Reversibility/verification (reuse; localized new obligations).** The ADR-0034 total-bijection
proof is width-parametric ⇒ survives the widens. Ring closure + Flat + oracle untouched. New
obligations, each oracle-checked: (a) synthesized prim faithfulness = bveq vs table; (b) frame ancilla
discipline = nested-Trace `run_on_cpu==eval`; (c) nested WCET fold soundness = park-safety on nested
programs; (d) shared-ALU per-opcode correctness = whole-domain equiv per opcode + mutually-exclusive
control decode.

## 3. Energy analysis (honest)
step_j = cell-count of the one lowered `step` iso (fib golden 7.3674e-8 J total, step_j 3.07e-9, park
16.7%). TISC-2 makes step_j sub-linear in program size:

| Term | TISC-1 | TISC-2 | scaling |
|---|---|---|---|
| Fetch | O(N_rom · W_pc) | narrow-A compare | O(N_rom · log B) |
| **Decode** | O(\|ISA\| · (crossbar+pred+core)), all masked | 1 crossbar + O(log\|ISA\|) decode + gated shared cores | **constant in \|ISA\|; ~2–4× smaller; growth tax removed** |
| State/PC | full width every step | per-program W, ⌈log₂B⌉-effective PC | pay for live width |
| Memory | O(log MEM_WORDS) | unchanged | already good |

Honest bounds: the shared ALU still instantiates all gated blocks' cells (single-step-iso constraint)
→ the win is removing ×|ISA| *replication*, ~2–4× not orders of magnitude; the real prize is **killing
the ISA-growth tax** (adding an op no longer taxes every program). The frame/tree-fold/synthesis
changes are compile-time → they change `B` (for programs that use them), never step_j → zero standing
tax. The one standing tax is the W_pc widen, made sub-linear by narrow fetch. **The shared-ALU cell
win must be validated by a real lower_iso→joules measurement before commit** (P1/P2 caveat).

## 4. Migration — two phases, coverage before efficiency (each slice oracle-gated)
**Phase A — COVERAGE (make it run every well-formed program correctly):**
A1. Widen `W_pc` 8→16 + narrow fetch. (Pure param; proof width-parametric. New test: WCET>256 program.)
A2. `with_scratch_frame(K)` + cap raises (MEM_WORDS, MAX_LEAVES, MAX_LEAVES_LOOP). (Proof-of-frame:
    the GCD ships, body 60→28, B=194 — the first genuine variable-T loop falls out here, no widen-PC.)
A3. Tree-structured WCET/S_max fold (behavior-preserving on flat programs — must reproduce current B
    exactly; the green oracle is the guard). Prerequisite for A4–A5.
A4. Delete `has_inner_control` → branch-in-branch, controlled-in-loop, via the tree fold + depth-tagged
    skip-blocks. (Test: Plus-in-Plus, controlled-in-loop, run_on_cpu==eval, park-safe.)
A5. Come-from **frame stack** → nested Trace + Times(Trace,g) + live-input spill. **THE highest-bar
    slice** (control-flow state machine): full fresh-context adversarial review + real-worktree
    round-trip across real back-edges, terminal-quiescent, per CLAUDE.md L15/L19/L20.
A6. Universal-gate prim **synthesizer** + route non-identity structural isos through it + `swap_homes`
    for body permutation. (Flips "recognizer" → "arbitrary"; test: a hand-built unrecognized admitted
    bijection lowers, bveq passes, run_on_cpu==eval.)
A7. `W` data-word widen (Flat(n>8)). (Test: Flat(16) reduction run_on_cpu==eval.)
→ After A7 the reject surface has no reject-to-be-safe path for any well-formed statically-WCET-able
program. **The machine runs arbitrary well-formed theseus programs.**

**Phase B — EFFICIENCY (make step_j scale with active work; gated on measurement):**
B1. Shared configurable ALU (all-blocks-gated form; reuse the verified gated cores; preserve the single
    step-iso). Validate step_j drop via lower_iso→joules BEFORE commit. Full adversarial review
    (datapath = daemon-equivalent control surface).
B2. Per-program W / per-program-effective PC width (pay-for-live-width).
B3. (Future/optional) active-only microprogram-ROM — only if reconciled with the ring-closure proof.

## 5. Proof obligations (the new burden, all discharged by the existing oracle)
1. Nested WCET fold soundness (park-safety on nested programs) — the deepest, since it's a *soundness*
   property not just coverage. Adversarial target: construct a nested program where the fold
   under-counts B → park wrap → corruption; prove the tree fold cannot under-count.
2. Frame-stack disjointness across back-edges (inner frame ∩ outer come-from state = ∅) — the A5 gate.
3. Synthesized-prim faithfulness (bveq vs table; whole-domain at small width, symbolic wider).
4. Shared-ALU per-opcode bijectivity + mutually-exclusive control decode (B1 gate).
Each ships with a reproducing behavioral test (real worktree + gate round-trip), not unit tests alone.

## 6. Honest risks + the genuinely-open decisions
- **A5 (nested come-from + tree fold) is the hardest, highest-risk slice** — a control-flow state
  machine change, the exact class CLAUDE.md flags. It is where the design most needs adversarial review.
- **General-prim synthesis** is a new pass with its own correctness burden (mitigated by reusing bveq);
  worst-case gate blow-up for exotic bijections is real but confined.
- **THE open efficiency fork (genuinely the user's call):** how hard to push Phase B. Phase A alone
  gives a machine that *runs everything correctly* but keeps the O(|ISA|) decode tax. Phase B (shared
  ALU) removes the tax at meaningful proof-and-implementation cost. Given the user's "efficiency matters
  A LOT / no whack-a-mole for ANY part," the ISA-growth tax is exactly the efficiency-whack-a-mole to
  kill — so Phase B is recommended, but *sequenced after* coverage and *gated on measurement*, so it
  never risks the correctness mandate.
- **Deferred (not needed for the mandate):** active-only microcode (ring-proof risk); a fully
  active-only datapath; anything that would discard the injectivity/ring-closure proofs.

## 7. Adversarial review findings — corrections that make the design real (3 reviews)
The design DIRECTION survived (all reviewers endorse: keep paradigm, frame stack, tree fold, general-prim
synthesis, shared ALU). But three BLOCKER-class findings pin where the hard work actually is and correct
under-specified claims. These MUST be reflected in implementation.

**BLOCKER-1 (coverage is NOT complete — the deepest honest result).** "No reject-to-be-safe path remains
after Phase A" is FALSE for **wide symbolic prims**. Two prim routes that do NOT compose into a universal
synthesizer: (a) `theseus-netlist::synthesize_prim` (lower.rs:608-694) is genuinely universal + whole-domain
verified BUT requires an *enumerable table* and is capped at `MAX_SYNTH_WIDTH=12` rails; (b) a prim admitted
via `admit_symbolic` has an EMPTY table by construction (prim.rs:195 SymbolicOnly), so it can ONLY reach
`recognize_prim`→`selection_by_equiv` — a hardcoded pattern library that REJECTS anything outside its arms
(compile.rs:1377+). ⇒ a Flat(16)×Flat(16) nonlinear admitted bijection (WCET-able, k_max=1) is STILL
rejected. I conflated *verification* (symbolic bveq exists) with *synthesis* (no wide symbolic synthesizer
exists). **TRUE completeness requires building a symbolic transformation-based synthesizer** (emit gates
whose symbolic PrimSpec bveq-equals the target) — the one hard, UNBUILT piece. Honest scope of Phase A as
specified: "runs any well-formed program whose prims are enumerable-width (≤12 rails) or recognizer-matched."
The remaining hole is precisely characterized and is the key open research problem for full arbitrariness.
(Not-a-bug confirmed: the ≤12-rail synthesizer's Sum-unequal-arm padding/don't-care handling IS
reversibility-sound — it extends to a full-cube permutation, garbage-marked; add a Distrib→prim→Factor
round-trip test asserting padding returns to zero.)

**BLOCKER-2 (A3 is deeper than a formula change).** The nested-WCET fix is not just "flat sum → tree fold."
The exact-k_max trajectory WALK (compile.rs:2691-2743) runs `body_instrs` STRAIGHT-LINE via `exec_instr` —
it does NOT honor an inner loop's `Swapbr` back-edge, so it simulates the inner body ONCE per outer
iteration, not inner-k_max times; and `lower_trace` DROPS the inner `body_meta` (compile.rs:2661) instead of
merging it. Counterexample: outer k=4, inner k=100, inner_iter_len=12 → true B≈4800, flat substrate gives
≈88 (55× under-count) → park guard passes on bogus 88 → PC wraps mid-run → corruption (L15-class silent
loss). **A3 fix (mandatory):** (i) merge `body_meta.loop_terms` into parent SCALED by outer k_max; (ii) run
the k_max walk on an inner-back-edge-HONORING mini-interpreter (not straight-line exec_instr), or fall back
to the `|u|+1` cardinality PRODUCT. A3's test MUST nest Iter/Trace (Iter-under-Trace is the common desugar
and slips through as two summed sibling terms), not just Seq siblings.

**BLOCKER-3 / A1 correction (widen is NOT a "pure param"; decouple two widths).** The design conflated the
come-from token δu with the WCET budget B. **δu is a STATIC ROM offset** (backward-jump distance ≤ N_rom) —
widen it with `log₂ N_rom`. **B is a DYNAMIC step count** — needs W_pc bits. They are DIFFERENT quantities;
keep them decoupled (δu stays small; B needs the width). And A1's "pure param" is wrong: `pc:u8`, `br:u8`
(types.rs:242-243), `Addi/Subi{imm:u8}` (asm.rs:140,290), and `pc_add_br` `n=2*W` (step.rs:151) are ALL
hardcoded 8-bit and must widen TOGETHER (encoder + state + step iso) — a real coordinated change, and its
ADR-0034 width-parametricity must be re-proven at the new widths. State the max nesting depth W_pc supports.

**MAJOR-4 (resequence A4/A5).** `lower_trace` hand-assigns `aux_base = max_carrier_reg+1` WITHOUT consulting
occ (compile.rs:2777); the F-1 reject (compile.rs:2560) is what currently keeps that safe under Times.
Deleting the rejects (A4) to enable nesting BEFORE replacing aux allocation with occ-aware
`with_scratch_frame` (A5) would silently alias sibling/outer aux → come-from corruption. **CORRECTED ORDER:
A3 (tree fold + walk fix) → A5 (frame stack + occ-aware allocation) → A4 (delete has_inner_control/F-1
rejects).** A5 must precede A4.

**MAJOR-5 (Plus accounting).** Keep BOTH WCET and s_max as Σ over Plus arms (matches today's
`forward_skip_total += body.len()`, compile.rs:2454). Do NOT "tighten" s_max to `max` — a parked PC can land
in the untaken larger arm's skip region → unsound. WCET-`max` is sound for steps but must not propagate to
s_max.

**MINOR (sequencing coupling).** A synthesized prim inside a nested loop can push iter_len past the old 256
→ A6 (or any A6-prim used in a loop) must land strictly AFTER A1's coordinated width widening.

### Net effect on the plan
Direction and axes UNCHANGED. Corrected migration order: **A1(coordinated width widen + δu/N_rom decouple) →
A2(frame → GCD ships) → A3(recursive-tree WCET fold + back-edge-honoring k_max walk) → A5(occ-aware frame
stack) → A4(delete nesting rejects) → A6(prim synthesis, ≤12-rail scope) → A7(wide data words)**, then
Phase B. Coverage honest scope: complete EXCEPT wide symbolic prims, which need a new symbolic synthesizer
(the one precisely-characterized open hard problem). The nested-control slices (A3+A5) are the highest-risk
and need the full CLAUDE.md adversarial-review + real-worktree-round-trip discipline, with the specific
counterexamples above (nested Iter/Trace under-count; Times(Trace,·) aliasing) as mandatory regression tests.
