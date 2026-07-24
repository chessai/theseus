# Scratch-frame register model — quantitative result (design input for the frame-model axis)

Probe result (2026-07-23) that anchors the redesign's register/frame axis and proves the
"register pressure is the disease, curable at compile time with zero energy tax" thesis.

## The free-scratch fact (soundness core)
- Loop skeletons are **register-only**: the emitted come-from machinery (`rc/rb/temp/ctrl` at
  aux regs 4..7, tag in a reg) touches ONLY registers; **no skeleton instruction ever touches
  scratch** (compile.rs ~2789-2827, aux regs ~2775-2778). `SCRATCH_SPILL_BASE=2` reserves s0/s1
  for the decoder; **s2..s7 (6 words) are physically present and unused by any loop body.**
- Live inputs stay register-resident (guard `n_input+1 ≤ carrier_reg_budget=4`, compile.rs
  ~2596-2606). So scratch holds nothing live during a body ⇒ a scratch-resident working set
  aliases nothing, back-edge included.

## The frame mechanism (generalize the PROVEN M-D.e bracket)
M-D.e's `mde_spill_walk` already pages one zero-init padding leaf to `Scratch(2)` inside ONE
`with_registers` whole-body page-in/page-out bracket (2 self-inverse `Xchg`, net-zero). Generalize
to **K ephemeral frame words** `Scratch(2)..Scratch(2+K-1)`: created zero at body entry, consumed to
zero at body exit (ancilla discipline) ⇒ never cross the back-edge with live data ⇒ categorically on
the SAFE side of the live-input-relocation line M-D.e explicitly rejected (F5.2). Change surface is
narrow: `MAX_LEAVES_LOOP` and the register budget guard UNCHANGED (the frame is not a carrier leaf);
add a `with_scratch_frame` helper (~30 lines, structurally identical to `with_registers`); frame base
must consult `body_occ` (not hardcode Scratch(2)) to stay disjoint from any future spilled carrier.

## The number (why GCD needs no widen-PC)
The 2-bit GCD body is ~60 instrs; dominant cost = the conditional-op tax (§4.3): ~5 predicates
(h==0, a==b, a==0/b==0, sign(a−b), h-window), each a 5–15-instr inline recompute-from-history +
uncompute, **because there is no register to hold the predicate bit**. Staging predicates in frame
words collapses each to compute-once + cheap reads + one clear:

| K | body_len | B = 4 + 5·(body+10) | vs cap (B≤256 ⇔ body≤40) |
|---|---|---|---|
| current (K=0) | ~60 | 354 | BLOCKED |
| K=2 | ~34 | 224 | fits (thin margin) |
| **K=3** | **~28** | **194** | **fits comfortably (2 scratch words still spare)** |

## Energy verdict (the principle in action)
The frame adds **no instruction, no decoder surface, no state field** ⇒ ZERO standing step_j tax;
it costs only the per-iteration `Xchg` page-in/out, already priced into WCET, and only for programs
that use it. Contrast widen-PC (~+10–15% step_j PER PC bit on EVERY program) and a Csub op (+9.2%
decoder tax on EVERY program). **Compile-time discipline > machine growth** — the design's core rule.

## For the redesign
Axis 2 (working store + frame) should make this a FIRST-CLASS, general frame/calling-convention
discipline — not a loop-body special case — sized generously once (ample scratch), so nested contexts
and sub-isos each get private working store with zero standing energy tax. This probe proves the
mechanism is sound and bounded; the redesign generalizes it from "one loop body" to "any context."
