# Finite-trace totality (TH-1)

**Claim.** Let `f : u + a ↔ u + b` be a core term whose denotation `eval f` is a
**total bijection** on the finite semantics, with `u` finite. Then `trace f`
evaluated forward on any `x : a` and backward on any `y : b` both **terminate**,
and the two runs are **mutually inverse** -- so `trace f : a ↔ b` is a total
isomorphism. Quantitatively, a run applies the body at most `|u| + 1` times
(equivalently: performs at most `|u|` loop iterations), and this bound is exact
over the class of all such `f`.

**Mechanization.** This proof is machine-checked in Lean 4 + Mathlib at
`theseus-proofs/TheseusProofs/FiniteTrace.lean` (`lake build`, depending only on
Mathlib's base axioms; no `sorry`/`axiom`/`native_decide`). Mechanized:

* the Lemma (no loop-state repeats) as `state_injOn`, and the Theorem (forward
  termination + the `|u|+1` upper bound) as `trace_terminates`
  (`trace_terminates_of_equiv` under the bijection hypothesis);
* the tightness witness at `|u| = 2` (`hamF_*`) -- the `|u|+1` constant is
  verified load-bearing;
* the **mutual-inverse / backward run** ("both directions are one reversed
  trajectory") as `trace_mutual_inverse` (fuel-free) and `run_symm_entry`
  (fueled, `card U + 1` budget);
* the **class-wide exactness / trajectory disjointness** (ADR-0007 TH-1b) as
  `traj_cross_injective` + `traj_disjoint` (vertex-disjointness) and
  `traj_card_bound` -- the genuine per-input `Sigma_{x in S} fuel x <= |u|`
  carrier bound (each `fuel x = T_x - 1`), NOT a uniform-fuel product;
  `traj_cross_injective` takes a separate fuel per input to support it.

The `Injective`/`Fintype` hypotheses are kernel-verified load-bearing. Text-only:
(i) the `|u|`-*iterations* restatement (as opposed to the mechanized
body-application bound); (ii) tightness for GENERAL `|u|` -- the tightness witness
is mechanized at `|u| = 2` (`hamF_*`), and the claim that a maximal Hamiltonian
walk exists at every enumerated finite `|u|` remains a text argument; and (iii) the
WCET/register corollaries below (see the scope note in the Lean file's header).

## Hypothesis and its discharge (read this first)

TH-1 is *conditional* on its hypothesis -- "`eval f` is a total bijection." Where
that comes from, for a `trace` produced by elaborating a label loop:

* **Plain body:** the coverage+linearity checker certifies it (TH-4a,
  `theory/coverage-bijectivity.md`).
* **Label body -- scoped and documented (Lean-TH-1 + text-TH-4a-label).** v0.1
  `trace` bodies are produced by label-loop elaboration (`design/elaboration.md`).
  The Lean-mechanized TH-1 proves termination for an *abstract* bijection body;
  what certifies that an elaborated label loop *denotes* such a bijection is the
  **text**-argued **TH-4a-label** (`theory/coverage-bijectivity-labels.md`): the
  label checks reduce to the plain checks over the extended type `U + A ≅ U + B`,
  so the elaborated `trace Φ` has the total-bijection body this theorem needs --
  with the label carrier `U` already in the fed-back `inl` summand. The end-to-end
  discharge is therefore the **conjunction** of the mechanized termination and the
  text reduction; it is scoped and documented, not itself mechanically proved (the
  Lean proof does not know the body came from a label loop). The WCET corollaries
  below hold for well-typed finite-dialect label programs on this basis (modulo the
  standing, tested elaboration-correctness obligation). Tracked in
  `theory/obligation-ledger.md` (TH-4a / TH-1 rows).
* **Prim body:** the prim mutual-inverse admission check (`design/core-pi.md`
  §Prim admission).
* **Nested trace** (`f` itself contains a `trace`): TH-1 applied inductively --
  read the Claim as the induction *step*, "if `eval f` is a total bijection then
  `eval (trace f)` is." `core-pi.md`'s global "every well-typed core term denotes
  a bijection" is exactly this induction and is not independent of TH-1.

## Semantics (normative here, pending the slice-2 spec)

The `trace` loop is not yet pinned in a normative spec -- `adr/0009` is the
stratification *decision* (no operational semantics), and `core-pi.md` defers
trace evaluation to "the label slice." So this block is the **working normative
definition** the slice-2 label static-semantics MUST adopt verbatim (or this file
re-cites it); the theorem is stated against exactly this loop.

`trace f` forward on `x : a`:

```
s := inr(x)                     -- enter on the a-side
loop:
    t := f(s)                   -- one body application (one fuel unit)
    match t:
        inr(y) => return y      -- exit on the b-side
        inl(w) => s := inl(w)   -- feed the u-state back and iterate
```

Fuel is charged **one unit per body application** `t := f(s)` -- exactly the
spec's normative fuel unit (`spec/dynamic-semantics.md`: "one per `trace` body
application," equivalently one per label step; structural formers are free). A
label loop is granted `|u|+1` such units (see the fuel-unit note below).
Let the trajectory of `x` be the body outputs `t₁, ..., tₙ` with `t₁ = f(inr x)`,
`t_{k+1} = f(inl w_k)` while `t_k = inl(w_k)`, and `tₙ` the first `inr`. Write
`s₀ = inr(x)` and `s_k = inl(w_k)`, so `t_k = f(s_{k-1})`. The **loop-states** are
`w₁, ..., w_{n-1} ∈ u`; `n` body applications visit `n-1` loop-states.

## Lemma (no loop-state repeats)

`w₁, ..., w_{n-1}` are pairwise distinct.

*Proof (minimal-repeat descent).* Suppose not; among all repeats pick one
`w_i = w_j` (`i < j`) with `i` **minimal**. `f` is a bijection, so `t_i = t_j`
(both equal `inl(w_i)`) forces equal preimages: `s_{i-1} = s_{j-1}`.

* If `i = 1`: `s_0 = s_{j-1}`, i.e. `inr(x) = inl(w_{j-1})` -- impossible (distinct
  injection tags).
* If `i ≥ 2`: `s_{i-1} = s_{j-1}` is `inl(w_{i-1}) = inl(w_{j-1})`, i.e.
  `w_{i-1} = w_{j-1}` -- a repeat at indices `(i-1, j-1)` with `i-1 < i`,
  contradicting the minimality of `i`.

Either branch is a contradiction. ∎

(The engine of the proof is that injectivity turns a repeat into an *earlier*
repeat -- descent -- rather than an immediate contradiction; the earlier draft's
"consecutive states differ" and fixpoint sub-cases are subsumed and dropped. The
argument never uses that the trajectory terminates or that `tₙ` is `inr`, so it
holds for any prefix of a -- a priori possibly infinite -- run; that is what lets
the Theorem invoke it to *exclude* non-termination.)

## Theorem (termination + exact bound)

By the Lemma the `n-1` loop-states are distinct elements of `u`, so `n-1 ≤ |u|`,
hence:

* **Loop iterations (loop-states visited):** `≤ |u|`.
* **Body applications (fuel):** `n ≤ |u| + 1` -- one entry application plus at
  most `|u|` further applications, the last of which exits to `b`.
* **Immediate-exit base case:** if `f(inr x) = inr(y)` then `n = 1`: one
  application, zero loop-states, zero iterations. (Includes `|u| = 0`, where the
  first output cannot be `inl`, forcing immediate exit.)

Since the loop-state count is bounded it cannot run forever; some `t_k` is `inr`
by application `|u|+1` at the latest. Forward termination proved. ∎

## Both directions are one reversed trajectory (mutual inverse)

**Mechanized** as `trace_mutual_inverse` (fuel-free, over the `Steps` exit
relation) and `run_symm_entry` (fueled, with the explicit `card U + 1` budget)
in `FiniteTrace.lean`. The reversed-trajectory equation `f†(t_k) = s_{k-1}`
underlies `steps_reaches`, which states the backward run *reaches* `f s` from
`inr y` (the per-index equation is discharged inside its induction); loop
output-uniqueness is `steps_output_unique`. NOTE: `trace_mutual_inverse` mechanizes
the `id_a` direction (forward exit `x ↦ y` ⇒ backward exit `y ↦ x`); the `id_b`
direction is the same statement instantiated at `f := f.symm` (with `f.symm.symm =
f`), not a separately named lemma.

Backward evaluation of `trace f` is *defined* as forward evaluation of the
adjoint: `eval(sym (trace f)) = eval(trace (f†))`, using the definitional law
`(trace f)† = trace(f†)` (the sym-pushing table, `core-pi.md`) and
`eval (sym g) = eval(g†)`. `f†` denotes `(eval f)⁻¹`, itself a total bijection
(same hypothesis discharge as above), so the Lemma and Theorem apply verbatim to
`trace(f†)`: the backward run terminates within the same `|u|+1` / `|u|` bounds.

*Mutual inverse.* Run `trace f` forward on `x`, giving states
`s₀, ..., s_{n-1}` and outputs `t₁, ..., tₙ = inr(y)`. Because `f` is a bijection,
`f†(t_k) = s_{k-1}` for every `k`. Feed `y` to `trace(f†)`: its start `inr(y)`
equals `tₙ`, and `f†(tₙ) = s_{n-1} = inl(w_{n-1})`, then `f†(inl w_{n-1}) =
f†(t_{n-1}) = s_{n-2}`, ..., reaching `f†(t₁) = s₀ = inr(x)` and exiting with `x`.
The backward trajectory is the forward one reversed; `eval(trace(f†)) ∘
eval(trace f) = id_a`, and symmetrically `= id_b` the other way. So `trace f` is
a genuine isomorphism, not merely bidirectionally terminating. ∎

## Tightness

**Mechanized** at `|u| = 2` by `hamF_*` (`hamF_underfueled` shows `|u|` fuel
still loops; `hamF_exits` shows `|u|+1` exits), and the *class-wide* exactness /
carrier-sizing direction (ADR-0007 TH-1b) by `traj_cross_injective`,
`traj_disjoint`, and `traj_card_bound` -- the genuine per-input
`Sigma_{x in S} fuel x <= |u|` bound (`fuel x = T_x - 1`) -- in `FiniteTrace.lean`.
The single-instance tightness witness is mechanized at `|u| = 2`; the argument
below that a maximal Hamiltonian walk exists for GENERAL enumerated finite `u`
(so `|u|+1` is achieved at every size) is a text argument, not mechanized.

The `|u|` / `|u|+1` bound is achieved: over `u ≅ Bitⁿ` (`|u| = 2ⁿ`) with
`|a| = |b| = 1` -- and the successor walk generalizes (text argument) to any
enumerated finite `u` -- the bijection `inr x ↦ inl 0⃗`, `inl k ↦ inl(k+1)` for
`k < 2ⁿ−1`, `inl(2ⁿ−1) ↦ inr y` is a Hamiltonian walk: one trajectory visits all
`|u|` loop-states then exits, using exactly `|u|` iterations and `|u|+1` body
applications. The exit application's *input* is the last loop-state (already
counted) and its output is `inr`, so exiting consumes no extra state -- the tight
constant is `|u|`, not `|u|−1`. Conformance `P0011` is *sub-maximal*:
`u ≅ Bit×(Bit×Bit)`, `|u| = 8`, but each input walks only the 4-state counter
sub-space -- 4 loop-states, `n = 5` applications ("entry + 3 steps + exit"),
comfortably inside the reported `≤ |u| = 8`.

## Corollaries (the compiler's use -- mind the units)

Two different quantities, two different units -- conflating them is an off-by-one:

* **Diagnostics / hardware WCET = `|u|` *iterations*.** A `TraceLoop` runs at most
  `|u|` iterations; a hardware counter indexing them counts `0 ... |u|`, width
  `⌈log₂(|u|+1)⌉` bits (`design/netlist-ir.md` carries the `WCET ≤ |u|`
  attribute; the width follows here). Termination and register-sizing are the same
  theorem.
* **Interpreter fuel budget = `|u|+1` *body applications*.** The `Done`-within-fuel
  contract (`spec/dynamic-semantics.md`, `spec/test-spec-m1.md`) counts fuel per
  body application, and a maximal trajectory needs `|u|+1` of them -- so the fuel a
  finite-dialect program is *granted* for a label loop is `|u|+1`, **not** `|u|`.
  (Granting `|u|` would `OutOfFuel` on the exit application of a Hamiltonian loop.)
* **Report the bound as `≤`, never `=`.** "Exact" here means unimprovable over the
  *class* of all `f`, not per-program: `P0011` reports `≤ 8` against an actual 4.
  Diagnostics must print `WCET ≤ |u|`.
* **Owed for the slice-2 `eval`/fuel work** (flagged, not proved here): the
  interpreter currently charges fuel per *combinator node* (`crates/theseus-eval`),
  a third unit matching neither of the above, and the conformance harness runs with
  effectively-infinite fuel -- so "Done within fuel = static bound" is untested.
  Slice-2 must pin ONE fuel unit (body applications), thread the `|u|+1` budget,
  and specify how nested-trace fuel composes.

## Provenance (what is borrowed vs. novel)

* **Borrowed (cited, not re-proved here):** the combinator and adjoint laws,
  including the definitional `(trace f)† = trace(f†)`, are Π (Sabry et al.) over a
  symmetric rig category (Mac Lane; Laplaza -- coherence, ledger TH-3). The
  categorical semantics of the reversible `trace`/iteration operator -- under which
  a bijection over finite state has only finite entering trajectories -- is
  Kaarsgaard et al. (join inverse categories; ADR-0005). The exhaustiveness half of
  the bijection hypothesis (via TH-4a) rests on Maranget's pattern matrices.
* **Novel here:** the *exact quantitative* bound (`|u|` iterations / `|u|+1`
  applications) packaged for WCET diagnostics and hardware counter sizing, the
  both-directions-as-one-reversed-trajectory (mutual-inverse) statement, and the
  immediate-exit constant. The prior art gives the *qualitative* termination and
  the laws; the compiler-usable constants and their register/fuel corollaries are
  theseus's (roadmap publication item #1). The ancestor Theseus paper (James &
  Sabry) supplies the clause/label *syntax*, not this quantitative meta-theory.
