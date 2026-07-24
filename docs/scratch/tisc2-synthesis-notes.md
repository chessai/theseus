# TISC-2 design — synthesis notes (distilled proposal ideas, running)

Concise capture of each Phase-1 proposal's load-bearing ideas, to feed Phase-3 synthesis and
survive compaction. Full proposals in the session transcript / task outputs.

## P1 — Compositional-algebra / uniform-mechanism ("TISC-Π") [received]
Thesis: TISC-1 lowers *recognized shapes*, not *the algebra*. Lower the closed 22-combinator Iso
algebra by structural induction — one rule per combinator, each closing over its children's lowering
under a uniform frame discipline — and nesting is free by construction.
1. **One value model = the leaf-byte slot-tree** (reuse `value_to_leaf_bytes`/`count_leaves`,
   prim.rs:267-523): a value of type T = a vector of L=count_leaves(T) words. Sum tag = leaf 0;
   payloads zero-extended to max(|a|,|b|). **All structural isos (Swap/Assoc/Unite/Distrib/Factor)
   become gate-free `homes` slot-permutations** (generalizes the current no-op cases at compile.rs
   :2360-2384 to ALL cases). No per-shape code — shape lives entirely in the `homes` map.
2. **Frame stack + frame pointer FP (THE keystone):** generalize the scratch-frame from one loop body
   to any context. Descending into a child (loop body / branch arm / ⊗ factor) does `FP += frame_size`
   (self-inverse TK-adder bracket, like M-D.e page-in/out); child slots addressed FP-relative ⇒
   **disjoint from every live parent slot by construction.** Deletes the whole aliasing-guard class
   (`allocate_aux`/occ-hunting compile.rs:662; the `occ⊋homes` reject :2564,2570). `frame_size` is a
   bottom-up static fold; max nesting depth = a provisioning number, not a paradigm limit.
3. **One come-from register CF, framed:** all 4 loops share one reversible feedback (CF records
   entry-edge/counter per iteration). Framed ⇒ nested loops each get their own CF ⇒ "come-from join
   does not compose" (compile.rs:2573) is FIXED. Trace/Iter/Fold/Unfold specialize the same mechanism.
4. **Plus/branch:** tag→staged predicate bit in frame; both arms run predicate-gated over the same
   payload slots, each arm recursing into its own sub-frame ⇒ branch-in-branch composes.
5. **General prim synthesis (closes recognizer-vs-synthesizer):** every admitted prim carries a
   bit-level `PrimSpec`/`BvExpr` (prim.rs:94-120,480-512). Synthesize a reversible circuit from it
   (AND→Toffoli, Xor→CNOT, Not→X, Mux→Cswap — the gate basis already in decoder.rs:106-139), ancillas
   from the current frame, uncompute inputs via the given backward spec; **verify with the existing
   bveq emission verifier.** Recognizer demoted to a hot-path optimization. Default path = general.
6. **Shared micro-ALU + microprogram-ROM (breaks the O(|ISA|) decode tax):** opcode selects a
   *microprogram* (short (gate,wire-tuple) sequence) run once through ONE configurable reversible
   engine — not all 15 op-netlists masked every step (decoder.rs:867). Decode → O(1 engine + log|ISA|).
   New ops = new microprogram-ROM entries, ~free in step_j (vs Ccnot +9.15% today). Subsumes #5
   (a synthesized prim = a microprogram). Gate basis {CNOT,Toffoli,Fredkin,gFredkin} is the minimal
   closed universal set; all "instructions" (Add,Rotl,...) are microprograms over it.
7. **PC wide for WCET reach, fetch NARROW for energy** (narrow-A compare on low ⌈log₂B⌉ bits) ⇒
   widening PC costs ~0/step. Wider skip/WCET fields fix the only real TISC-1 WCET blocker (u8).
8. **Compositional WCET** = bottom-up fold (Seq:+, Times:+, Plus:stage+both, Trace:k_max·body,...);
   decidable for every well-formed program; exact-k_max migrates per-frame (tighter under nesting).
9. **Energy verdict:** step_j → C_fetch·logB + C_decode·(engine+log|ISA|) + C_state·live_width =
   SUB-LINEAR in program size. The one unmeasured bet: shared-ALU single-netlist gate count vs Σ
   per-op netlists — validate via lower_iso→joules BEFORE committing (do the datapath last).
10. **Reuse:** leaf-byte encoding (bit-identical → oracle stays valid), injectivity proof, ring
    closure Thm 3.5, two-body memory, Flat(n), exact-k_max, bveq, gate basis, TK adder, fetch/park,
    differential oracle. **Replace:** allocate_aux→FP bump; recognizer→BvExpr synthesis; 15-op masked
    decoder→micro-ALU; nesting rejections→deleted; u8 fields→wider. Staged migration: (1) frame stack
    [highest coverage/risk, first], (2) general prim synthesis, (3) shared micro-ALU [gated on energy
    measurement, last], (4) field widening.

## P2 — Energy-first / shared-datapath [received]
Grounded energy model: energy is PER-CELL, data-independent; `total_j = B·step_j`; step_j = cell-count
of the ONE lowered `step(rom)` iso (fib golden 7.3674e-8 J, step_j 3.07e-9, B=24/T=20, park 16.7%).
1. **Shared configurable ALU (the headline energy win):** today decode = `seq_balanced(Op::ALL.map
   (instruction))` = 15 op-netlists, each own crossbar (`select_reg` ~56 K1/operand) + predicate chain
   + core, ALL masked every step. Replace with ONE shared 3-slot crossbar [D,A,B] + decode-once into
   ~6 control lines + gated SHARED sub-blocks: add±dir (sub = adjoint, ONE adder for add/sub/addi/subi
   — today 4 separate netlists), xor, rotl±dir, and-accumulate (Ccnot/Cswap family). Crossbar+predicate
   mass drops ~10-15×; **decode step_j roughly halves-to-quarters; ISA-growth tax drops from
   O(crossbar+pred+core)/op to O(core)/op.** Reuses the EXACT verified gated cores (gated_tk_gates etc.,
   decoder.rs:402-457) — architectural change, low gate-level proof risk.
2. **HONEST caveat (key tension w/ P1):** in a SINGLE fixed step-iso (which the ring-closure proof needs)
   all gated sub-blocks' cells are still instantiated every step — the win is removing the ×|ISA|
   REPLICATION of routing+predicate, NOT making unused blocks free. So ~2-4× on step_j + killing the
   growth tax, NOT order-of-magnitude. An active-only microcode datapath (only the current op's gates)
   would go further but breaks the single-fixed-step-iso the ring proof depends on — P2 judged that too
   high a proof risk. [P1's "microprogram-ROM, only active op runs" claims the bigger win — SYNTHESIS
   MUST RESOLVE: microcode/active-only (bigger, breaks single-step-iso/ring proof) vs all-gated (honest
   2-4×, preserves proof). Likely take P2's conservative path first, revisit active-only later.]
3. **Per-program W_bit** = max Flat(n) in the program (fib→8, Flat(4)prog→4, narrower words=fewer cells)
   — closes Flat(n>8), zero tax on programs that don't need width. **Per-program-minimal PC width:**
   address space generous (16-bit field) but active pc_add_br/pc_inc/compare width = ⌈log₂B⌉ per program
   (narrow fetch makes this ~free). "Pay for live width."
4. **Universal-gate escape hatch:** recognizer = FAST PATH (tuned cheap lowerings, common algebra);
   general reversible synthesis (truth-table→controlled-NOT decomposition, admit-certified) = COMPLETENESS
   path for exotic prims. HONEST: maximally-mixing bijection = worst-case exponential controlled-NOTs →
   confine to opt-in per-prim, never a standing per-step tax. Coverage total, efficiency preserved common
   case. (Same conclusion as P1#5 / P4-salvage#2.)
5. Frame discipline (§2) = generalize M-D.e `with_registers` to recursive `with_frame`, body_occ-aware,
   zero standing tax (same as P1#2). Keep two-body ring memory verbatim (reuse). Compositional WCET fold.

## P4 — Paradigm-stress-test / reversible-abstract-machine [received] — DECISIVE RESULT
First-principles finding: a term-structured SPATIAL machine ALREADY EXISTS = `theseus_netlist::lower_iso`
(lower.rs:1527, dispatch :490-543) — recursively lowers the WHOLE Iso term to measurable gates
(PhasedCellNetlist + balanced_energy), **nesting FREE (NO has_inner_control guard in theseus-netlist!):
Trace unroll :531, Iter/Fold :539 via controlled_apply recursing into nested body nodes.** ⇒ **the nesting
rejections (compile.rs:2573,2564,2431,2667,3060,1912) are artifacts of the ROM FLATTENER, not the algebra
or the gate lowering.** BUT the verdict is a QUALIFIED NEGATIVE: spatial unroll materializes the whole term
in space (Σ over ALL unrolled bodies, MULTIPLIED through nesting) → forfeits the temporal gate-REUSE that
lets a bounded step-netlist run an arbitrary-length WCET. For loop-heavy/nested programs (the whole point)
cell count blows up multiplicatively (a 2^24 whole-domain loop = a 2^24-CELL netlist, may not even lower).
**Time-multiplexing (PC over a shared step, re-ramped B times) IS the reversible realization of
BOUNDED-HARDWARE / UNBOUNDED-TIME — the exact property the mandate needs.** ⇒ **KEEP the flat-ROM
time-multiplexed paradigm** (confirms framing + P1 + P2 from the opposite direction). Wins coverage, loses
energy; energy loss is FUNDAMENTAL (space-materialization vs time-reuse), not fixable. THREE SALVAGE
INSIGHTS to steal into TISC-2:
1. **Nesting is a LOWERING problem, not a language problem** (lower_iso proves it) → close nested control
   in the ROM compiler by GENERATING THE LOOP-BODY'S ROM RECURSIVELY (mirror controlled_apply's recursion:
   nested Trace = inner ROM region called with come-from tags). Concrete design for the biggest gap.
2. **General prims:** adopt lower.rs's whole-domain-VERIFIED `synthesize_prim` (:546-569) at the LEAF to
   emit the datapath for any admitted bijection, wrap as a TISC instruction → kills UnrecognizedPrim
   WITHOUT discarding the time-multiplexed step. (Converges w/ P1#5, P2#4.)
3. **Shared-configurable-ALU** = the dual of Π-SPATIAL's "no decoder" win → collapse seq_balanced(Op::ALL)
   into one ALU configured by O(log|ISA|) control bits, recovering the no-ISA-tax benefit INSIDE the
   time-multiplexed paradigm. (Converges w/ P1#6, P2#1.)

## CONVERGENCE (P1+P2+P4 agree; P3 pending a0ceef5e)
All three independently land on the SAME core design: KEEP flat-ROM time-multiplexed step (P4 gives the
WHY: bounded-hardware/unbounded-time) + FRAME DISCIPLINE for compositional nesting via recursive body-ROM
generation + GENERAL PRIM SYNTHESIS (whole-domain-verified, closes deepest gap) + SHARED CONFIGURABLE ALU
(breaks O(|ISA|) decode tax, honest ~2-4×) + NARROW FETCH + per-program-sized PC/word width + REUSE the
value model / ring closure / energy pipeline / oracle. KEY UNRESOLVED TENSION for synthesis: P1's
active-only microprogram-ROM (bigger energy win) vs P2's all-gated-blocks single-step-iso (preserves ring
proof) — the ring-closure proof needs ONE fixed step netlist, so lean P2-conservative first.
