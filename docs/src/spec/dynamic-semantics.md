# Dynamic semantics

A well-formed iso denotes a partial isomorphism between its types;
`~f` denotes the inverse partial isomorphism, and `~~f ≡ f`.

Clause evaluation: a value matching clause `p <-> q` under substitution θ
maps to `q[θ]`; coverage guarantees the clause and θ are unique, in both
directions. Labels: evaluation over the label-extended sum, iterating
until a non-label value is produced (elaboration to `trace`, ADR-0009).

**Finite dialect:** every program is total; label loops terminate within
the size of the label state space (theory: finite-trace totality, TH-1).

## The fuel contract (normative for evaluators)

Evaluation is parameterized by a fuel budget. One unit of fuel is
consumed per **clause application** (including each label step). An
evaluator MUST return exactly one of:

- `Done(value)` — evaluation completed; `value` has the target type.
- `OutOfFuel { steps_taken }` — the budget was exhausted;
  `steps_taken` equals the initial fuel.

and MUST be deterministic: identical (iso, input, fuel) yields identical
results. In the finite dialect, `Done` is guaranteed for any fuel ≥ the
program's static bound (ADR-0007); `OutOfFuel` is reachable only through
constructs of the software dialect marked partial. A third verdict for
domain errors is reserved for the software dialect (statically excluded
in v0.1 by coverage); implementations MUST NOT conflate it with
`OutOfFuel`. Evaluating `~f` on `w` MUST agree with the unique `v` such
that `f` on `v` yields `Done(w)`.
