# theseus paper -- outline and framing

Status: PLANNING DRAFT. The abstract and conclusion here are drafts to iterate on,
not final text. Bibliographic details live in `references.bib` and are recalled from
memory -- every entry is marked VERIFY; check each against the primary source before
citing. Do not trust years, venues, pages, or DOIs until verified.

## Working title (candidates)

- theseus: A Verified, Energy-Measured Reversible-Computing Stack from Language to Adiabatic CPU
- From Reversible Programs to Measured Joules: An End-to-End Verified Adiabatic Computing Stack
- theseus: Compiling, Verifying, and Measuring a Reversible Language on a Modeled Adiabatic CPU

## Thesis and contribution

The energy promise of reversible computing -- avoiding Landauer's kT ln 2 cost of
erasure, and recovering switching energy adiabatically -- has mostly lived at the level
of individual circuits and asymptotic argument. There is a gap between the theory and a
concrete artifact that is at once high-level, correct, and energy-measured. theseus's
contribution is the verified, end-to-end pipeline: a high-level reversible language
compiled all the way to a measured adiabatic CPU, with correctness proved (whole-domain
symbolic equivalence plus a machine-checked meta-theory) rather than tested.

Claimed contributions:

- C1. A linearly-typed reversible language whose synthesizable fragment guarantees that
  every program is a total bijection with a static worst-case running time, by
  construction rather than by annotation.
- C2. A compiler lowering programs to (a) a reversible instruction set (TISC) executed on
  a modeled adiabatic CPU, and (b) a dual-rail reversible cell netlist -- including
  general reversible synthesis of primitives (Bennett compute-uncompute; BDD/word-level).
- C3. An energy model that reports measured joules per program, not asymptotics.
- C4. Correctness by proof, not sampling: whole-domain symbolic (BDD) equivalence over
  entire input spaces, plus a machine-checked (Lean) meta-theory -- reversibility by
  construction, loop termination, and reversible control flow.

Venue framing: PL/systems (PLDI, OOPSLA, ASPLOS) or the Reversible Computation (RC)
conference. The framing below leans PL/systems; an RC submission would expand the
architecture and physics sections and compress the language-design novelty.

## Abstract (draft)

Reversible computing promises to sidestep Landauer's thermodynamic bound on energy
dissipation: a logically reversible computation need not erase information, and adiabatic
circuits can recover most switching energy. That promise has largely stayed at the
circuit level and in asymptotic argument, leaving a gap between reversible-computing
theory and a concrete, correct, energy-measured implementation. We present theseus, an
end-to-end reversible-computing stack: a linearly-typed reversible source language whose
synthesizable fragment guarantees every program is a total bijection with a static
worst-case running time, by construction; a compiler lowering programs both to a
reversible instruction set executed on a modeled adiabatic CPU and to a reversible cell
netlist; and an energy model reporting measured joules per program. Correctness is
proved, not sampled: primitive and whole-program behavior are checked by whole-domain
symbolic (BDD) equivalence, and the core meta-theory -- reversibility by construction,
loop termination, and reversible control flow -- is machine-checked in Lean. We evaluate
on [fib, LFSR, adders, variable-time loops], reporting per-program energy and correctness
coverage. theseus shows a high-level reversible language can be compiled, verified, and
energy-measured as a single artifact.

## Section outline

1. Introduction
   - The energy motivation: Landauer's bound; erasure as the thermodynamic cost; the
     adiabatic route to energy recovery, which requires logical reversibility.
   - The gap: reversible theory and circuits exist; a verified, energy-measured,
     end-to-end language-to-CPU artifact does not.
   - Contributions (C1-C4) and a teaser result (the energy "money plot").
   - Roadmap of the paper.

2. Background and motivation
   - Landauer / Bennett: reversibility and the thermodynamics of computation.
   - Adiabatic / energy-recovery logic: why recovering energy needs reversibility.
   - The reversible-language lineage (Janus; Pi / Information Effects; Theseus).
   - What is missing that this paper supplies.

3. The theseus language
   - Types: finite sum/product; values.
   - Reversible isomorphisms (isos) and the adjoint; the combinator algebra.
   - The synthesizable fragment and its guarantee: total bijection + static WCET.
   - Come-from (reversible) control flow; fuel-bounded loops; fold/unfold.
   - Small worked example (e.g. the reversible adder or LFSR).

4. Compilation
   - The core Iso intermediate representation.
   - Lowering to TISC, the reversible ISA: a program is one reversible iso run
     forward and backward; the come-from / branch codegen.
   - Lowering to the dual-rail reversible cell netlist.
   - General reversible synthesis of primitives: Bennett compute-uncompute; the
     BDD/word-level transcribers; the space/time (pebbling) trade-off.

5. The adiabatic execution model
   - The ring machine and phased execution.
   - Per-step energy accounting; from netlist to joules.
   - What is modeled vs. simulated (be explicit; this is a model, not silicon).

6. Verification
   - Whole-domain symbolic (BDD) equivalence vs. brute-force simulation: entire input
     spaces, domain-size-independent, replacing exponential sampling.
   - The machine-checked (Lean) meta-theory: reversibility by construction
     (denote is bijective), the adjoint really inverts, loop termination within a
     finite fuel bound, and the come-from involution.
   - The differential oracle (netlist / timed sim vs. interpreter).

7. Evaluation
   - Energy results: the money plot and its slope; per-program joules.
   - Correctness coverage: whole-domain results; the differential oracle.
   - Case studies: fib, LFSR, adders, variable-time loops.
   - Honest coverage boundary: what lowers, what does not, and why (register pressure,
     body-size / width budgets) -- the frontier.

8. Related work
   - Reversible languages (Janus; Pi / Information Effects; RFUN; CoreFun; Theseus).
   - Reversible architectures (Pendulum; the Bob reversible processor).
   - Reversible logic synthesis (Shende et al.; Miller-Maslov-Dueck; BDD-based,
     Wille-Drechsler).
   - Adiabatic / energy-recovery hardware (Athas et al.; Younis-Knight SCRL; 2LAL).
   - Verified compilation and proof engineering (CompCert; CakeML; Lean / mathlib).

9. Limitations and future work
   - The model-to-silicon gap; the physics track as a research frontier.
   - Programs that strain the register / width budget; the widening trade-offs and the
     lesson that hand-fed monolithic primitives are the wrong unit (compose from the
     source language instead).
   - Extending the verified fragment.

10. Conclusion (draft below).

## Conclusion (draft)

We set out to close the gap between the promise of reversible computing and a concrete
artifact that is simultaneously high-level, correct, and energy-measured. theseus does
this end to end: programs written in a linearly-typed reversible language, whose
synthesizable fragment is provably total-bijective with static worst-case time, compile
to a reversible ISA run on a modeled adiabatic CPU and to a dual-rail netlist, and their
cost is reported in joules rather than asymptotics. Crucially, correctness is a theorem,
not a test suite: whole-domain symbolic equivalence certifies behavior over entire input
spaces, and the meta-theory is machine-checked in Lean. The result is less a single
technique than an existence proof -- a full reversible-computing stack in which the
language guarantees, the compiler's correctness, and the energy story are all first-class
and mutually reinforcing. The remaining distance to physical silicon, and to richer
programs that strain the register and width budgets, marks the road ahead; theseus
provides the verified scaffolding to walk it.

## Reviewer-defense notes

- The toughest question will be "what is genuinely new vs. integrated?" The pieces
  (reversible languages, adiabatic circuits, reversible synthesis) exist. Lead on the
  COMBINATION (verified + measured + end-to-end) and on the VERIFICATION METHODOLOGY
  (whole-domain symbolic equivalence + a Lean meta-theory), which is the most defensibly
  novel part.
- Be explicit that the adiabatic CPU is a MODEL with an energy account, not taped-out
  silicon; state the fidelity of the model and its assumptions.
- State the coverage boundary honestly: which programs lower and run, and which do not
  (and why). A precise negative result is a strength, not a weakness.
- Position against the closest architectural precedents (Pendulum; the Bob reversible
  processor) and the closest language (Theseus / Pi) directly and generously.

## References

See `references.bib`. Must-cite anchors: Landauer 1961; Bennett 1973; Fredkin-Toffoli
1982 (why reversibility); Athas 1994 and Younis-Knight 1994 (adiabatic energy recovery);
Vieri 1999 and Axelsen 2011 (reversible CPUs -- closest precedents); James-Sabry
(Pi / Theseus) and Yokoyama-Glueck (Janus) (the language lineage); Bryant 1986 and
Wille-Drechsler 2009 (BDD verification / synthesis); Leroy 2009 (CompCert) and
de Moura-Ullrich 2021 (Lean) (the verification angle).
