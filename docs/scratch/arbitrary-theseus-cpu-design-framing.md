# The CPU that runs arbitrary well-formed theseus programs — DESIGN FRAMING (r1)

**Mandate (user, 2026-07-23):** design, holistically and over hours, the CPU that runs *arbitrary
well-formed theseus programs*. Stop the whack-a-mole: no more per-feature retrofits, for **any**
part of the CPU. **Correctness matters most; efficiency (energy/step_j) matters A LOT.**

This doc is the shared foundation for the independent architecture proposals. It fixes: the hard
constraints, the energy model (what we are optimizing and why the current machine leaks), the full
set of design axes each proposal must answer, and the decision principles. It does NOT pick a design.

---

## 0. What we already know (ground truth feeding the design)

- **The paradigm is sound and proven — keep it.** Fixed-WCET + reversible park + PC-offset
  branching: total-bijection injectivity over all 2^160 states (ADR-0034), composite-ring
  zero-garbage closure (Theorem 3.5), real-`system_step == eval` authority. Two independent audits
  found NO paradigm-level problem. This is a *provisioning + efficiency* redesign ("TISC-2"), not a
  new execution model.
- **The recurring disease is register pressure**, and the cure is a compile-time access discipline,
  not more machine. Proven quantitatively this session: loop skeletons are register-only, so scratch
  s2..s7 are free inside every body; a K=3 scratch frame shrinks the 2-bit GCD body 60→28 instrs
  (B=194<256) — GCD ships with **no widen-PC, no new op, no state-layout change**.
- **THE EFFICIENCY PRINCIPLE (the anti-whack-a-mole rule):** *a fix that adds decoder surface, an ISA
  op, a state field, or PC/word bits taxes the energy of EVERY step of EVERY program, forever
  (whether or not it is used). A fix that is a compile-time access discipline / microarchitectural
  reuse adds ZERO standing energy tax.* Prefer the latter always; accept a standing tax only when no
  compile-time discipline can express the requirement. Every design decision below is judged against
  this.

---

## 1. Hard constraints (non-negotiable; a proposal that violates one is out)

1. **Reversibility.** Every machine step is a total bijection on the state (the type-checked
   guarantee; the source of "reversibility-as-typecheck, energy-as-measurement"). No irreversible
   erasure anywhere in the datapath.
2. **Computable static WCET.** The machine runs a fixed number of steps `B` then parks. `B` must be
   statically computable from the program for every well-formed program the machine accepts. This is
   inherent to adiabatic reversibility: there is no data-dependent halt. (Open question the Phase-0
   grounding must answer: are there well-formed theseus programs with NO computable static bound? If
   so, the machine's "arbitrary" is bounded by "statically-WCET-able" — state that boundary
   explicitly rather than pretend.)
3. **Measured-energy faithfulness.** The design must remain lowerable to the gate/netlist level and
   measurable by the existing energy pipeline (`lower_iso` → joules). The differential oracle
   (`run_on_cpu == eval`) remains the verification norm.
4. **Preserve the verified machinery / migration path.** The `compile → lower → system_step → eval →
   energy` pipeline, the recognizers (`recognize_prim`, BVEQ), the composite-ring two-body memory
   closure, `Flat(n)`, exact-k_max — a proposal must state exactly what it reuses vs discards, and a
   from-scratch rewrite that discards the injectivity/closure proofs bears a very high bar.

---

## 2. The energy model — what we are optimizing (READ THIS; it is the crux of "efficiency")

Per step, because the machine cannot branch on data, it currently pays for the **entire static
program**, not the active work:

    step_j  ≈   C_fetch · N_rom_predicates          (all ROM entries compared every step)
              + C_decode · |ISA| · op_netlist_size   (all 15 op netlists run masked every step)
              + C_state · state_width                (the step iso plumbs the full state)
              + C_mem · mem_model                    (MEXCH/bus every step)

    total_energy ≈ B · step_j

**The structural leak:** `step_j` is `O(N_rom + |ISA| + state_width)` — proportional to the
program's *static size*, independent of what the instruction at this step actually does. An ADD step
still pays for the XOR netlist, the MEXCH netlist, and all N fetch predicates. This is the root of
the whack-a-mole *for efficiency*: every op/word/ROM-entry you add taxes every step forever (measured:
`Ccnot` cost fib +9.15% step_j just by existing in `Op::ALL`; each PC bit ~+10–15%).

**The central efficiency question every proposal must answer:** *can per-step energy be made to
scale with the ACTIVE instruction instead of the whole ISA/ROM?* Known and candidate levers:
- **Fetch:** narrow the PC compare to the low ⌈log₂ B⌉ bits (proven this session: N·2(A−1), A∝log B,
  independent of word width). Fetch predicate cost → ∝ log(reachable PC), not ∝ ROM size in bits.
- **Decode (the big one, unexplored):** today the decoder is `seq_balanced(Op::ALL.map(instruction))`
  — |ISA| *separate* op netlists, each masked on the opcode, all run every step → `O(|ISA|)`. A
  **shared-datapath / single-configurable-ALU** design (one reversible ALU whose *operation* is
  selected by O(log|ISA|) decoded control bits, RISC-style) would make decode `O(1 ALU pass +
  log|ISA|)` — breaking the ISA-growth tax entirely. This is likely THE highest-leverage efficiency
  idea in the whole redesign. Is it achievable reversibly? What does a reversible configurable ALU
  cost vs the sum of per-op netlists? (A proposal that keeps per-op netlists must justify the
  O(|ISA|) tax against this alternative.)
- **State width:** pay only for live state. `Flat(n)` already decouples value cardinality from
  leaf-count; how far can typed-density go? Does parked/idle state need to be plumbed every step?
- **WCET/park:** the park tax (idle steps B−T) is pure waste energy. Can the design reduce average
  park (tighter WCET — exact-k_max did this) or make parked steps cheaper than active steps?

A proposal is scored substantially on its **per-step energy scaling** (ideally sub-linear or constant
in ISA/ROM size) and its **total-energy** story (B · step_j incl. park), not just correctness.

---

## 3. The design axes (each proposal MUST answer all of these — this is the anti-whack-a-mole checklist)

1. **Value/data model.** How do arbitrary theseus Values (Unit, Bit, Sum, Prod, Flat(n), and any
   recursive/unbounded types — see Phase-0 grounding) map onto machine state? Typed registers? A
   uniform word? A typed heap/store? How are Sum (tag+payload) and nested Prod represented and
   operated on without per-shape special-casing?
2. **Working store + frame model.** The register/scratch file and — critically — how a computation
   context (loop body, nested context, called sub-iso) gets private working store WITHOUT clobbering
   its caller's live state. A general *frame/calling-convention discipline* (the scratch-frame result
   is the seed: generalize it from one loop body to arbitrary nested contexts). This is the disease's
   cure; make it a first-class part of the architecture, sized generously and once.
3. **Control model.** id/seq/times/sum/sym and — the hard part — **trace/iter loops, NESTED loops
   (currently REJECTED), and arbitrary combinator nesting**. The reversible come-from/history model
   for control. How is WCET computed compositionally for nested, data-dependent iteration? What is
   the maximum control-nesting depth the machine supports, and is that a parameter or a paradigm
   limit?
4. **ISA + decode model (efficiency-critical).** The instruction set AND how it's decoded/executed.
   Per-op netlists (O(|ISA|) tax) vs a shared configurable datapath (O(1) + log|ISA|). What is the
   minimal op/primitive basis that spans all of Core-Π's lowering needs? Every op's standing energy
   tax must be justified against §2's shared-datapath alternative.
5. **Fetch / PC / ROM / addressing.** PC width and ROM size sized *once, generously* for the WCET of
   real programs (not 256). The narrow-A fetch compare. How instructions are addressed and fetched.
6. **Memory model.** Resident memory + bus (the proven composite-ring two-body split) — how arbitrary
   programs allocate and use memory; whether recursive/dynamic data needs a heap; how memory scales
   without taxing every step.
7. **WCET / park / energy accounting.** The static WCET computation for arbitrary programs; the park
   model; the total-energy accounting. Tightness (park waste) is an efficiency axis.
8. **Reversibility & verification.** The total-bijection invariant for the new datapath; what carries
   over from the existing proofs (injectivity, ring closure); the new proof obligations; how the
   differential oracle validates it.

---

## 4. Decision principles (how proposals are judged, in priority order)

1. **Correctness / total coverage first.** Does it *provably* run every well-formed theseus program
   the WCET constraint admits — including the currently-rejected constructs (nested control, etc.)?
   A design that covers 95% and whack-a-moles the rest fails the mandate.
2. **No standing energy tax without justification (§2, the efficiency principle).** Prefer
   compile-time disciplines and shared datapaths over ISA/width/field growth. Sub-linear per-step
   energy in program size is the target.
3. **Sized once, generously.** Parameters (registers, scratch, PC/ROM, control-nesting depth, word
   model) chosen once with headroom for real programs, so we do not re-widen next month. But headroom
   that costs standing energy (wider state plumbed every step) is itself a tax — resolve via §2 (only
   pay for live/active state).
4. **Preserve verified machinery; bounded migration.** Maximize reuse of the proven pipeline and
   closures; a big rewrite must earn it.
5. **Simplicity / uniformity.** A uniform mechanism (one frame discipline, one datapath, one value
   representation) beats N special cases — special cases are exactly how whack-a-mole starts.

---

## 5. Open questions the proposals + grounding must resolve
- Are there well-formed theseus programs with no computable static WCET? (Bounds what "arbitrary"
  means.) — Phase-0 grounding.
- Recursive/unbounded types (lists, nats): does the language have them, and if so how does a
  fixed-WCET reversible machine represent+bound them? — Phase-0 grounding + value-model axis.
- Is a reversible shared-configurable-ALU cheaper (per step) than the sum of per-op netlists at
  |ISA|=15+? (The decode-energy question.) — proposals.
- Can control-nesting be made a uniform recursive mechanism (a frame stack) rather than a
  depth-limited special case? — proposals.
- What is the right value representation so Sum/Prod/Flat compose without per-shape ops? — proposals.
