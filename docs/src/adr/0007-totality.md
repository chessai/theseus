# ADR-0007: Totality

**Status:** Accepted

## Context
Should loops be provably terminating in the type system? Three regimes:
finite types, μ-types, full generality. A reversibility-specific trap
applies throughout: forward totality does **not** imply backward totality
-- any termination evidence must hold in both directions
(theory/bidirectional-descent.md).

## Decision
- **Finite fragment:** totality is free. `trace` over finite `u` cannot
  diverge: the body is injective, so trajectories through `u` cannot
  repeat a state. **Exact bound (corrected -- TH-1):** an exiting trajectory
  applies the body to `Right(a), Left(w₁), ..., Left(w_{t−1})` (`t`
  applications, incl. the exit); injectivity forces the `wᵢ` pairwise-
  distinct in `u`, so `t − 1 ≤ |u|`, i.e. **iteration count `t ≤ |u| + 1`,
  and this is attained** (witness: the 3-cycle `R()→L(L())→L(R())→R()` on
  `u = Bool` gives `t = 3 > |u| = 2`). The earlier "≤ |u|" was off by one.
  Every synthesizable v0.1 program is a total iso by construction, with a
  static WCET the compiler reports. **TH-1 status:** the WCET *reporting
  machinery* (a static budget the compiler emits, plus a reversible
  park-at-end making over-running to the budget answer-preserving) is
  **implemented for the straight-line fragment** by M-D.w (theseus-v2
  `a2479b9`; `wcet()`/`run_rom_on_cpu`, `docs/scratch/mdw-design.md`;
  monotone `B = rom.len()`, two-direction equal by construction). The
  **loop term** `B = spine + (|u|+1)·|body|` and its two-direction bound
  land with M-D.d (`Trace` as a reversible while-loop,
  `docs/scratch/mdd-design.md`).
- **Trajectory-disjointness / carrier sizing law (TH-1b).** A stronger,
  *formulation-independent* consequence of body injectivity, and the design
  law for every `Trace` program (surfaced by the TP-1 GCD review,
  `docs/reviews/tp1-gcd-review.md`): a bijective body is
  backward-deterministic, so **trajectories from distinct inputs are
  vertex-disjoint in `Left(u)`** (a shared carrier state would have two
  predecessors walking back to two different `Right` entries). Hence:
  (1) **`Σ_inputs (Tᵢ − 1) ≤ |u|`** -- the sum-over-inputs form of `t ≤ |u|+1`;
  (2) **`|u| ≥ |a|`** whenever (almost) every input iterates ≥ 1 time -- a
  loop carrier must be at least the *input* width; a bijection cannot shed
  information mid-loop; (3) **distinct inputs have distinct exit states**, so
  "iterate until `x == const`" is *impossible without a distinguishing witness
  in the carrier* -- the exit-until-target test is itself a Bennett-taxed
  erasure. **Corollary (the wall for variable-`T` loops):** a genuinely
  iterative program over an input domain of size `2^k` needs `|u| ≥ 2^k` ⇒
  `≥ k` payload leaves ⇒ over the `MAX_LEAVES_LOOP = 3` cap for any `k > 3` and
  any formulation (this is why GCD/Collatz break today independent of their
  branch structure). And because the emitted `k_max = |u|+1` is read from *type
  cardinality*, a large `|u|` also blows the WCET guard (`4 + k_max·|body| ≤
  256`) -- so real loop programs need, together: **loop-carrier spill (M-D.e) +
  widen-PC + a `k_max`-tightening mechanism** (a typed/verified iteration bound,
  e.g. numeral-typed fuel -- `iter`/`fold` already have this, which is why fib's
  park tax is meaningful and a raw `Trace`'s would be ~99.8% loose).
- **μ-types (v0.2):** blessed total-by-construction combinators -- bounded
  iteration (the count lives in the data, symmetric by construction) and
  reversible fold/unfold pairs. Raw unbounded trace stays expressible but
  explicitly marked partial, `unsafe`-style. Primitive-recursive
  completeness (RPP, Paolini-Piccolo-Roversi) is the expressiveness floor.
- **Sized types:** documented research extension, not on the roadmap.

## Consequences
Totality and hardware WCET are the same theorem; unroll-vs-counter is a
backend choice on a known bound (ADR-0013). Totality-by-default keeps the
quantum lift frictionless (unitaries are total).

## Alternatives rejected
Full sized/indexed types now (research-grade metatheory, soundness-bug
history, and it must be done *twice* -- once per direction);
Turing-complete synthesizable dialect (a feature with no customer in a
netlist-bound language).
