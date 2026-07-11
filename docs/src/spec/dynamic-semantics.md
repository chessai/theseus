# Dynamic semantics

A well-formed iso denotes a partial isomorphism between its types;
`~f` denotes the inverse partial isomorphism, and `~~f ≡ f`.

Clause evaluation: a value matching clause `p <-> q` under substitution θ
maps to `q[θ]`; coverage guarantees the clause and θ are unique, in both
directions. Labels: evaluation over the label-extended sum, iterating
until a non-label value is produced — this is exactly `trace` evaluation on
the elaborated body (ADR-0009; `theory/coverage-bijectivity-labels.md`).

**Finite dialect:** every program is total; label loops terminate within
the size of the label state space (`theory/finite-trace-totality.md`, TH-1).

## The fuel contract (normative for evaluators)

Evaluation is parameterized by a single **fuel budget**, a global `u64`
threaded through the whole evaluation. Fuel measures the one unbounded
quantity in a finite-dialect program: **`trace` body applications**.

### What one fuel unit is (and is not)

> **One fuel unit is charged per `trace` body application** — one evaluation
> of a traced term `Φ` on the current loop state (the `t := Φ(s)` step of the
> loop below). Equivalently at the surface: **one per label step.**

All other core formers — `id`, `sym`, `;`, `+`, `×`, the structural
isomorphisms (`swap⁺`/`swapˣ`, `assoc*`, `unite*`/`uniti*`, `distrib`/`factor`),
and a single `prim` application — evaluate **without consuming fuel**. They are
inductively total over a finite value (each recurses on strictly smaller
value/term structure and cannot diverge), so they need no budget to guarantee
termination; charging them would make the budget depend on the *elaborated term
shape* rather than on the program's semantic loop count, breaking the identity
below. This supersedes any per-combinator-node accounting: node-counting is at
most an internal debugging aid, never the normative unit.

Consequences of the unit choice — the identities the rest of the toolchain
relies on:

* A **plain** iso (no `trace`) costs **0 fuel**: it is a single structural pass
  and completes at any budget `≥ 0`.
* A single **label loop** `trace Φ` with carrier `u` costs at most **`|u| + 1`**
  fuel — one entry application plus at most `|u|` fed-back iterations (TH-1's
  bound; `|u|` is the WCET *iteration* count, `|u|+1` the *application*/fuel
  count — do not conflate the two units).
* **`Done` within fuel = the static bound**, for every finite-dialect program
  (the contract `spec/test-spec-m1.md` already asserts, now with a defined
  unit): granting fuel `≥` the program's static bound guarantees `Done`.

### Verdicts

An evaluator MUST return exactly one of:

- `Done(value)` — evaluation completed; `value` has the target type.
- `OutOfFuel { steps_taken }` — the budget was exhausted mid-loop;
  `steps_taken` equals the initial fuel (the budget hit zero only after that
  many body applications). The field is intentionally redundant with the
  initial fuel — it exists so a caller that did not retain the budget can still
  report it; an evaluator MUST NOT put a different count here.

and MUST be deterministic: identical `(iso, input, fuel)` yields identical
results. In the finite dialect, `Done` is guaranteed for any fuel `≥` the
program's static bound (ADR-0007); `OutOfFuel` is reachable only through
constructs of the software dialect marked partial (`trace` over an infinite
carrier, v0.2). A third verdict for domain errors is reserved for the software
dialect (statically excluded in v0.1 by coverage); implementations MUST NOT
conflate it with `OutOfFuel`. Evaluating `~f` on `w` MUST agree with the unique
`v` such that `f` on `v` yields `Done(w)`.

## `trace` evaluation (normative)

`trace : (u + a ↔ u + b) → (a ↔ b)`. In the core, `Trace(u, Φ)` carries the
carrier type `u` and the body `Φ : u + a ↔ u + b`. This is the loop minted and
proved terminating (with its exact bound) in `theory/finite-trace-totality.md`;
that theorem is stated against **exactly this definition**, so evaluators MUST
implement it verbatim.

**Forward**, `eval(Trace(u, Φ))` on `x : a`:

```
s := inr(x)                     -- enter on the a-side (a is the RIGHT summand)
loop:
    charge 1 fuel               -- one body application; OutOfFuel if budget was 0
    t := eval(Φ)(s)             -- evaluate the body (itself may charge fuel: nesting)
    match t:
        inr(y) => Done(y)       -- exit on the b-side
        inl(w) => s := inl(w)   -- feed the u-state back and iterate
```

The carrier `u` occupies the **left** (`inl`) summand — the fed-back loop
state; the boundary `a`/`b` is the **right** (`inr`) summand. (This is the
orientation `theory/coverage-bijectivity-labels.md` elaborates labels into, so
no reshuffle is needed between elaboration and evaluation.)

**Backward**, `eval(sym (Trace(u, Φ)))` = `eval(Trace(u, Φ†))` via the
definitional law `(trace Φ)† = trace(Φ†)` (`design/core-pi.md`): the same loop
on the adjoint body, terminating within the same `|u|+1` bound and computing
the inverse (TH-1 mutual-inverse).

### Fuel charging, and the program static bound

Fuel is charged **once per body application, at every trace nesting depth,
against the one global budget** — the `charge 1 fuel` above is the *only* place
fuel decrements. A single global counter realizes composition with no per-trace
bookkeeping; `steps_taken` on `OutOfFuel` is the total body-application count
across all depths (and equals the initial fuel — see below).

The **static bound** `B(·)` of a core term is the upper bound on total fuel
charges it can incur, a structural recurrence:

```
B(structural former) = 0      -- id, swap*, assoc*, unite*/uniti*, distrib, factor
B(prim)              = 0
B(f ; g)             = B(f) + B(g)
B(f × g)             = B(f) + B(g)          -- both components evaluated
B(f + g)             = max(B(f), B(g))      -- exactly one branch taken
B(sym f)             = B(f)                 -- adjoint preserves carriers (|u| unchanged)
B(Trace(u, Φ))       = (|u| + 1) · (1 + B(Φ))
```

The `Trace` case is the load-bearing one: a loop performs at most `|u|+1` body
applications, and **each** application charges 1 (its own `charge 1 fuel`) *plus*
`B(Φ)` for evaluating the body — hence `(|u|+1)·(1 + B(Φ))`, **not** `(|u|+1)·|u'|`.
(Charging `(|u|+1)·|u'|` under-grants and `OutOfFuel`s a total program: the outer
loop's own `|u|+1` charges are dropped.) A program's granted fuel is `B` of its
elaborated core term. Corollaries:

* A single label loop with structural body: `B = (|u|+1)·(1+0) = |u|+1`. ✓
* Sequential composition `f ; g` (e.g. `= spin ; spin`): `B(f) + B(g)` — a
  **sum**, `9 + 9 = 18`, not a product.
* A loop nesting a loop (`u` outer, `u'` inner, structural otherwise):
  `(|u|+1)·(1 + (|u'|+1)) = (|u|+1)·(|u'|+2)`.
* Sibling nested loops in one body are handled by the `;`/`×`/`+` cases (sum on
  `;`/`×`, max on `+`), not a single "nesting chain."

Note: v0.1 **surface** cannot place a `trace` inside a `trace` body (E4 bodies
are `demux`/`perm`/`sym` only, and patterns cannot invoke isos), so `B(Φ) = 0`
for every surface label loop and the program bound is a sum of per-loop `|u|+1`
terms. The full recurrence is stated for core-level evaluators and the v0.2
total combinators, where nested traces become reachable.

### Static bound and the WCET note

For a finite-dialect label loop the compiler reports a **WCET note**
(ADR-0007): `≤ |u|` iterations, where `|u| = Σ_ℓ |U_ℓ|` is the size of the
label state space (`theory/coverage-bijectivity-labels.md`). The fuel a program
is *granted* is `B` of its elaborated term (per loop, the application count
`|u|+1`, composed by the recurrence above), never the iteration count —
granting only `|u|` would `OutOfFuel` on the exit application of a maximal
(Hamiltonian) loop. Report the
bound as `≤`, never `=`: it is exact over the *class* of carriers, not
per-program (P0011 walks a 4-state sub-space of its `|u| = 8` carrier and
reports `≤ 8`).

## Implementation obligation (discharged)

All items below have been implemented in the label slice:

1. ~~`EvalError::OutOfFuel` to carry `steps_taken`~~ — **done**: `OutOfFuel { steps_taken: u64 }`, where `steps_taken` equals the initial fuel granted.
2. ~~A `B(·)` pass over the elaborated term~~ — **done**: `static_bound` in `crates/theseus-eval/src/interp.rs` implements the recurrence above.
3. ~~A conformance-harness rule/fixture key that grants exactly `B` and asserts `Done`~~ — **done**: the `# fuel-bound:` and `# steps:` fixture keys in the conformance harness (`crates/theseus-cli/tests/conformance.rs`) grant the static bound and assert `Done`, and grant one less to assert `OutOfFuel { steps_taken }` with the correct count.

The interpreter charges fuel as specified: structural formers are free, `Trace` charges one unit per body application against the single global budget.
