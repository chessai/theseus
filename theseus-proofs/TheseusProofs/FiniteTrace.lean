/-
TH-1 forward-termination bound (the §Lemma + §Theorem core), mechanized
against Mathlib.

Text proof: docs/src/theory/finite-trace-totality.md. For `trace f : a ↔ b`
elaborated from an injection `f : u + a ↔ u + b` over a *finite* `u`, a forward
run exits within `|u| + 1` body applications (`|u| = Fintype.card U`), in the
text's fuel unit of one charge per body application.

SCOPE — what is and is NOT mechanized here (keep the obligation ledger honest):
  * Mechanized (forward core): the text's §Lemma (no loop-state repeats) as
    `state_injOn`, and its §Theorem (forward termination + the `|u|+1` upper
    bound) as `trace_terminates`. Proven under the *weaker* hypothesis
    `Injective f` (the text assumes a bijection; the forward direction only needs
    injectivity, so this strengthens the result). `trace_terminates_of_equiv`
    restates it under the text's bijection hypothesis. `hamF_*` mechanizes the
    §Tightness / "granting only `|u|` fuel would OutOfFuel" corollary at |u|=2.
  * Mechanized (backward / mutual inverse, text §"Both directions are one
    reversed trajectory"): `trace_mutual_inverse` — for a bijection `f`, a
    forward exit `x ↦ y` implies a backward exit `y ↦ x` under `f.symm`, i.e. the
    two runs are one reversed trajectory (`trace f` is a genuine isomorphism);
    `run_symm_entry` is the fueled `card U + 1` restatement. Built on the fuel-
    free exit relation `Steps` (`run`-equivalent via `run_some_of_steps` /
    `steps_of_run_some`), `steps_reaches` (the reversed-trajectory equation
    `f†(t_k) = s_{k-1}`) and `steps_output_unique`.
  * Mechanized (class-wide exactness / disjointness, ADR-0007 TH-1b):
    `traj_cross_injective` (distinct inputs are backward-deterministically
    vertex-disjoint in the carrier), `traj_disjoint` (its `x ≠ x'` corollary),
    and `traj_card_bound` (the `Σ_inputs (Tᵢ − 1) ≤ |u|` carrier-sizing bound,
    here `|S| · fuel ≤ |u|`).
  * NOT mechanized: the `|u|`-*iterations* (as opposed to body-application)
    restatement and the WCET/register corollaries. These remain text-only in
    `finite-trace-totality.md`.

The mechanization strategy mirrors the text's "minimal-repeat descent":

  * `state f x` is the loop-state trajectory `s₀ = inr x`, `s_{k+1} = f s_k`
    (stuttering on exit — irrelevant while we are in the no-exit regime).
  * `run_none_imp_no_exit`: if the run has not exited within `fuel` steps then
    every state `s_k` (`k < fuel`) maps under `f` to an `inl` (we kept looping).
  * `state_injOn`: under no-exit, `state f x` is injective on the visited prefix
    — this is the descent lemma, injectivity of `f` turning a repeat into an
    *earlier* repeat (text §Lemma).
  * `trace_terminates`: pigeonhole. `card U + 1` post-entry states all live in
    `U` (they are `inl w`), are pairwise distinct, so they cannot inject into a
    `card U`-element type — contradiction with the no-exit assumption.
-/
import Mathlib

namespace Theseus.FiniteTrace

variable {U A B : Type}

/-- The trace loop from state `s : U ⊕ A`, charging one fuel unit per body
    application: `some b` on exit (`f s = inr b`), `none` if fuel is exhausted
    while still looping (`f s = inl w`). `Sum.inl` is the fed-back `u`-state;
    `Sum.inr` is the `a`/`b` boundary. -/
def run (f : U ⊕ A → U ⊕ B) : Nat → (U ⊕ A) → Option B
  | 0,        _ => none
  | fuel + 1, s =>
      match f s with
      | Sum.inr b => some b
      | Sum.inl w => run f fuel (Sum.inl w)

/-- The loop-state trajectory: `state 0 = inr x`, and each step feeds the fed-back
    `u`-state forward. On exit (`f` returns `inr`) it stutters, which never
    matters because we only reason about `state` inside the no-exit regime. -/
def state (f : U ⊕ A → U ⊕ B) (x : A) : Nat → (U ⊕ A)
  | 0     => Sum.inr x
  | n + 1 =>
      match f (state f x n) with
      | Sum.inl w => Sum.inl w
      | Sum.inr _ => state f x n

/-- No-exit regime: within `fuel` steps the run kept looping, i.e. `f (state k)`
    is an `inl` for every `k < fuel`. Read off a `run … = none`. -/
theorem run_none_imp_no_exit
    (f : U ⊕ A → U ⊕ B) (x : A) (fuel : Nat)
    (hnone : run f fuel (Sum.inr x) = none) :
    ∀ k, k < fuel → ∃ w, f (state f x k) = Sum.inl w := by
  have aux : ∀ (fuel j : Nat), run f fuel (state f x j) = none →
      ∀ k, k < fuel → ∃ w, f (state f x (j + k)) = Sum.inl w := by
    intro fuel
    induction fuel with
    | zero => intro j _ k hk; omega
    | succ fuel ih =>
        intro j hj k hk
        cases hfs : f (state f x j) with
        | inr b =>
            exfalso
            rw [run, hfs] at hj
            exact Option.some_ne_none b hj
        | inl w =>
            have hstep : state f x (j + 1) = Sum.inl w := by
              rw [state, hfs]
            have hrun : run f fuel (state f x (j + 1)) = none := by
              rw [run, hfs] at hj
              rw [hstep]; exact hj
            match k, hk with
            | 0, _ =>
                refine ⟨w, ?_⟩
                simpa using hfs
            | k' + 1, hk =>
                have hk' : k' < fuel := by omega
                obtain ⟨w', hw'⟩ := ih (j + 1) hrun k' hk'
                refine ⟨w', ?_⟩
                have : j + 1 + k' = j + (k' + 1) := by omega
                rwa [this] at hw'
  have h0 : run f fuel (state f x 0) = none := by
    simp only [state]; exact hnone
  have haux := aux fuel 0 h0
  intro k hk
  have := haux k hk
  simpa using this

/-- Descent lemma (text §Lemma). In the no-exit regime the trajectory does not
    repeat: `state f x` is injective on `{0, …, fuel}`. Proof by the minimal-
    repeat descent — `f` injective turns `state a = state b` into
    `state (a-1) = state (b-1)`, bottoming out at the `inr`/`inl` tag clash. -/
theorem state_injOn
    (f : U ⊕ A → U ⊕ B) (hf : Function.Injective f) (x : A) (fuel : Nat)
    (hno : ∀ k, k < fuel → ∃ w, f (state f x k) = Sum.inl w) :
    ∀ a b, a ≤ fuel → b ≤ fuel → state f x a = state f x b → a = b := by
  suffices H : ∀ b a, a ≤ b → b ≤ fuel → state f x a = state f x b → a = b by
    intro a b ha hb hval
    rcases le_total a b with hab | hab
    · exact H b a hab hb hval
    · exact (H a b hab ha hval.symm).symm
  intro b
  induction b with
  | zero => intro a ha _ _; omega
  | succ b ih =>
      intro a ha hb hval
      rcases Nat.eq_or_lt_of_le ha with heq | hlt
      · exact heq
      · exfalso
        have hab : a ≤ b := by omega
        have hbfuel : b < fuel := by omega
        obtain ⟨wb, hwb⟩ := hno b hbfuel
        have hb1 : state f x (b + 1) = Sum.inl wb := by rw [state, hwb]
        match a, hab, hval with
        | 0, _, hval =>
            rw [hb1] at hval
            simp only [state, reduceCtorEq] at hval
        | a' + 1, hab, hval =>
            have ha'fuel : a' < fuel := by omega
            obtain ⟨wa, hwa⟩ := hno a' ha'fuel
            have ha1 : state f x (a' + 1) = Sum.inl wa := by rw [state, hwa]
            rw [ha1, hb1] at hval
            have hww : wa = wb := Sum.inl.inj hval
            have hfeq : f (state f x a') = f (state f x b) := by
              rw [hwa, hwb, hww]
            have hstateq : state f x a' = state f x b := hf hfeq
            have ha'b : a' ≤ b := by omega
            have hbfuel' : b ≤ fuel := by omega
            have := ih a' ha'b hbfuel' hstateq
            omega

/-- TH-1 bound: for finite `U` and injective `f`, a forward run from any `x : A`
    exits within `card U + 1` body applications. -/
theorem trace_terminates
    [Fintype U]
    (f : U ⊕ A → U ⊕ B) (hf : Function.Injective f) (x : A) :
    (run f (Fintype.card U + 1) (Sum.inr x)).isSome := by
  by_contra hcon
  rw [Option.not_isSome_iff_eq_none] at hcon
  set n := Fintype.card U with hn
  set fuel := n + 1 with hfuel
  have hno := run_none_imp_no_exit f x fuel hcon
  have hinj := state_injOn f hf x fuel hno
  let g : Fin fuel → U := fun i => (hno i.val i.isLt).choose
  have hg : ∀ i : Fin fuel, f (state f x i.val) = Sum.inl (g i) := by
    intro i
    exact (hno i.val i.isLt).choose_spec
  have g_inj : Function.Injective g := by
    intro i j hij
    have : f (state f x i.val) = f (state f x j.val) := by
      rw [hg i, hg j, hij]
    have hst : state f x i.val = state f x j.val := hf this
    have : i.val = j.val :=
      hinj i.val j.val (le_of_lt i.isLt) (le_of_lt j.isLt) hst
    exact Fin.ext this
  have hcard := Fintype.card_le_of_injective g g_inj
  rw [Fintype.card_fin] at hcard
  omega

/-- TH-1 under the text's stated hypothesis: a *bijection* `f : u + a ≃ u + b`
    (not merely an injection). Immediate from `trace_terminates` via
    `Equiv.injective`. This is the row the obligation ledger cites. -/
theorem trace_terminates_of_equiv
    [Fintype U] (f : (U ⊕ A) ≃ (U ⊕ B)) (x : A) :
    (run f (Fintype.card U + 1) (Sum.inr x)).isSome :=
  trace_terminates (f : U ⊕ A → U ⊕ B) f.injective x

/-- Tightness witness at `|u| = 2` (text §Tightness): the `+1` is load-bearing.
    A Hamiltonian walk `inr ↦ inl 0 ↦ inl 1 ↦ inr` needs all `|u| + 1`
    applications — with only `|u|` fuel the run is still `none` (would
    `OutOfFuel`), and with `|u| + 1` it exits. So the constant cannot be
    lowered to `|u|`, mechanizing the corollary the text warns about. -/
def hamF : Fin 2 ⊕ Unit → Fin 2 ⊕ Unit
  | Sum.inr _ => Sum.inl 0
  | Sum.inl i => if i = 0 then Sum.inl 1 else Sum.inr ()

theorem hamF_injective : Function.Injective hamF := by decide

/-- `|u|` fuel is not enough for the Hamiltonian walk: still looping. -/
theorem hamF_underfueled :
    run hamF (Fintype.card (Fin 2)) (Sum.inr ()) = none := by decide

/-- `|u| + 1` fuel exits — matching `trace_terminates` at its bound exactly. -/
theorem hamF_exits :
    (run hamF (Fintype.card (Fin 2) + 1) (Sum.inr ())).isSome := by decide

/-!
## Mutual inverse / backward run (text §"Both directions are one reversed trajectory")

The text argues that backward evaluation of `trace f` is forward evaluation of
the adjoint `f†` — for a bijection `f`, `f† = f.symm` — and that the two runs are
ONE trajectory traversed in opposite directions, so `trace f` is a genuine
isomorphism rather than merely bidirectionally terminating.

We mechanize this fuel-free, on a `Steps` predicate that captures "the loop
started at state `s` eventually exits with output `y`" independent of the fuel
budget. `Steps` is inter-derivable with the fueled `run` (`run_some_of_steps`,
`steps_of_run_some`), so a claim about `Steps` is a claim about `run`. The heart
is `steps_reaches`: the backward loop of `f.symm` from `inr y` walks the SAME
trajectory in reverse (the text's `f†(t_k) = s_{k-1}`), and `steps_of_reaches`
turns that walk, once it reaches an exit, into a `Steps f.symm`. Specialised to
entries it is
`trace_mutual_inverse` — a forward run of `f` from `x` exiting with `y` implies
the backward run (forward run of `f.symm`) from `y` exits with `x` — and, in
fueled form with the `trace_terminates` bound, `run_symm_entry`. -/

/-- Fuel-free exit relation: `Steps f s y` iff the trace loop entered at state
    `s : U ⊕ A` eventually exits with output `y : B`. `exit`: the body exits now
    (`f s = inr y`). `loop`: the body feeds back `inl w` and the loop from `inl w`
    exits with `y`. This is exactly `run f _ s = some y` with the fuel abstracted
    away (see `run_some_of_steps` / `steps_of_run_some`). -/
inductive Steps (f : U ⊕ A → U ⊕ B) : (U ⊕ A) → B → Prop
  | exit {s : U ⊕ A} {y : B} : f s = Sum.inr y → Steps f s y
  | loop {s : U ⊕ A} {w : U} {y : B} :
      f s = Sum.inl w → Steps f (Sum.inl w) y → Steps f s y

/-- `Steps` gives a concrete fuel: an exiting trajectory is an exiting fueled run
    (with fuel one more than the number of loop steps). -/
theorem run_some_of_steps
    (f : U ⊕ A → U ⊕ B) {s : U ⊕ A} {y : B} (h : Steps f s y) :
    ∃ fuel, run f fuel s = some y := by
  induction h with
  | @exit s y hfs => exact ⟨1, by rw [run, hfs]⟩
  | @loop s w y hfs _ ih =>
      obtain ⟨fuel, hfuel⟩ := ih
      exact ⟨fuel + 1, by rw [run, hfs]; exact hfuel⟩

/-- Conversely a successful fueled run witnesses an exiting trajectory. -/
theorem steps_of_run_some
    (f : U ⊕ A → U ⊕ B) :
    ∀ (fuel : Nat) (s : U ⊕ A) (y : B), run f fuel s = some y → Steps f s y := by
  intro fuel
  induction fuel with
  | zero => intro s y h; simp only [run, reduceCtorEq] at h
  | succ fuel ih =>
      intro s y h
      rw [run] at h
      cases hfs : f s with
      | inr b =>
          rw [hfs] at h
          have : b = y := Option.some.inj h
          subst this
          exact Steps.exit hfs
      | inl w =>
          rw [hfs] at h
          exact Steps.loop hfs (ih (Sum.inl w) y h)

/-- One backward loop-step: from backward state `z : U ⊕ B` the inverse body
    feeds back `inl w`, and the loop continues at `inl w`. The reversed
    trajectory is a chain of these (`Relation.ReflTransGen`). -/
def BStep (g : U ⊕ B → U ⊕ A) (z z' : U ⊕ B) : Prop :=
  ∃ w : U, g z = Sum.inl w ∧ z' = Sum.inl w

/-- Reversed-trajectory reachability (text §"Both directions are one reversed
    trajectory," the equation `f†(t_k) = s_{k-1}`). If the forward loop from `s`
    exits with `y`, then the backward loop (steps of `g = f.symm`) started at the
    forward output `inr y` walks the SAME trajectory in reverse and reaches the
    forward output `f s` of the entry state — i.e. the backward states are the
    forward outputs `t_k` visited in reverse order. Only `g (f v) = v` is used. -/
theorem steps_reaches
    (f : (U ⊕ A) ≃ (U ⊕ B)) {s : U ⊕ A} {y : B}
    (h : Steps (f : U ⊕ A → U ⊕ B) s y) :
    Relation.ReflTransGen (BStep (f.symm : U ⊕ B → U ⊕ A))
      (Sum.inr y) (f s) := by
  induction h with
  | @exit s y hfs =>
      -- `f s = inr y`, so the target `f s` is the start `inr y`; `rw` discharges
      -- the resulting reflexive `ReflTransGen` goal via its `@[refl]` lemma.
      rw [hfs]
  | @loop s w y hfs _ ih =>
      -- IH: backward reaches `f (inl w)`. One more backward step lands at
      -- `f s = inl w`, since `g (f (inl w)) = inl w = f s`.
      refine ih.tail ?_
      refine ⟨w, ?_, ?_⟩
      · -- g (f (inl w)) = inl w
        show (f.symm : U ⊕ B → U ⊕ A) (f (Sum.inl w)) = Sum.inl w
        simp
      · -- target `f s = inl w`
        rw [hfs]

/-- If the backward loop reaches, via `BStep`s, a state `z` whose body exits to
    `inr a`, then the whole backward run from the start exits with `a`. Front-
    building companion to `steps_reaches`. -/
theorem steps_of_reaches
    (g : U ⊕ B → U ⊕ A) {start z : U ⊕ B} {a : A}
    (hreach : Relation.ReflTransGen (BStep g) start z)
    (hexit : g z = Sum.inr a) :
    Steps g start a := by
  induction hreach using Relation.ReflTransGen.head_induction_on with
  | refl => exact Steps.exit hexit
  | @head p q hpq _ ih =>
      obtain ⟨w, hgp, hq⟩ := hpq
      subst hq
      exact Steps.loop hgp ih

/-- `Steps` is deterministic: the body `f` is a function, so a state exits with
    at most one output. -/
theorem steps_output_unique
    (f : U ⊕ A → U ⊕ B) {s : U ⊕ A} {y y' : B}
    (h : Steps f s y) (h' : Steps f s y') : y = y' := by
  induction h with
  | @exit s y hfs =>
      cases h' with
      | @exit _ y' hfs' =>
          rw [hfs] at hfs'; exact Sum.inr.inj hfs'
      | @loop _ w' y' hfs' _ =>
          rw [hfs] at hfs'; exact absurd hfs' (by simp)
  | @loop s w y hfs hnext ih =>
      cases h' with
      | @exit _ y' hfs' =>
          rw [hfs] at hfs'; exact absurd hfs'.symm (by simp)
      | @loop _ w' y' hfs' hnext' =>
          rw [hfs] at hfs'
          have hww : w = w' := Sum.inl.inj hfs'
          subst hww
          exact ih hnext'

/-- **Mutual inverse (text §"Both directions are one reversed trajectory").**
    For a bijection `f`, if the forward trace loop from entry `x` exits with `y`,
    then the backward trace loop — the forward loop of the adjoint `f.symm` — from
    entry `y` exits with `x`. The two runs are one trajectory traversed in
    opposite directions, so `trace f` is a genuine isomorphism. Fuel-free. -/
theorem trace_mutual_inverse
    (f : (U ⊕ A) ≃ (U ⊕ B)) {x : A} {y : B}
    (h : Steps (f : U ⊕ A → U ⊕ B) (Sum.inr x) y) :
    Steps (f.symm : U ⊕ B → U ⊕ A) (Sum.inr y) x := by
  have hreach := steps_reaches f h
  refine steps_of_reaches (f.symm : U ⊕ B → U ⊕ A) hreach ?_
  -- backward exits at `f (inr x)`: `g (f (inr x)) = inr x`.
  show (f.symm : U ⊕ B → U ⊕ A) (f (Sum.inr x)) = Sum.inr x
  simp

/-- Fueled restatement of mutual inverse. For a bijection `f`, a successful
    forward run from `x` (any fuel) implies a successful backward run of `f.symm`
    from `y` — with an explicit `card U + 1` fuel budget, matching
    `trace_terminates`. This is the `run`-level "both directions terminate and
    are mutually inverse" claim. -/
theorem run_symm_entry
    [Fintype U] (f : (U ⊕ A) ≃ (U ⊕ B)) {x : A} {y : B}
    (fuel : Nat) (h : run (f : U ⊕ A → U ⊕ B) fuel (Sum.inr x) = some y) :
    run (f.symm : U ⊕ B → U ⊕ A) (Fintype.card U + 1) (Sum.inr y) = some x := by
  have hsteps : Steps (f : U ⊕ A → U ⊕ B) (Sum.inr x) y :=
    steps_of_run_some (f : U ⊕ A → U ⊕ B) fuel (Sum.inr x) y h
  have hback : Steps (f.symm : U ⊕ B → U ⊕ A) (Sum.inr y) x :=
    trace_mutual_inverse f hsteps
  -- The backward run terminates within `card U + 1`; identify its output as `x`.
  have hbterm := trace_terminates (f.symm : U ⊕ B → U ⊕ A) f.symm.injective y
  rw [Option.isSome_iff_exists] at hbterm
  obtain ⟨x', hx'⟩ := hbterm
  have hback' : Steps (f.symm : U ⊕ B → U ⊕ A) (Sum.inr y) x' :=
    steps_of_run_some (f.symm : U ⊕ B → U ⊕ A) (Fintype.card U + 1) (Sum.inr y) x' hx'
  -- `Steps` output is unique, so `x' = x`.
  have := steps_output_unique (f.symm : U ⊕ B → U ⊕ A) hback hback'
  rw [hx', this]

/-!
## Trajectory disjointness / class-wide exactness (ADR-0007 TH-1b)

TH-1b is the *formulation-independent* consequence of body injectivity: a
bijective (indeed merely injective) body is backward-deterministic, so
**trajectories from distinct inputs are vertex-disjoint in the carrier `U`** — a
shared carrier state would have two predecessors walking back to two distinct
`Right` entries. From this the sum-over-inputs cardinality bound
`Σ_inputs (Tᵢ − 1) ≤ |u|` follows: pack each input's distinct loop-states into a
single injection into `U`.

`traj_cross_injective` is the disjointness core (distinct `(input, step)` pairs
land on distinct carrier states), and `traj_card_bound` is the packaged
`Σ (Tᵢ − 1) ≤ |u|` bound over any finite set of inputs, each in its own no-exit
regime for `fuel` steps (so `Tᵢ − 1 = fuel` loop-states per input, giving
`(#inputs) · fuel ≤ |u|`). -/

/-- Cross-input backward-determinism / vertex-disjointness (TH-1b core). Under
    injective `f`, if two inputs `x` and `x'` are both in the no-exit regime for
    `fuel` steps, then equal loop-states at steps `a ≤ fuel`, `b ≤ fuel` force
    `x = x'` AND `a = b`. In particular trajectories of distinct inputs share no
    carrier vertex (`x ≠ x' ⇒ state f x a ≠ state f x' b` for `a,b ≤ fuel`), and
    no vertex is revisited within one trajectory (the `x = x'` diagonal recovers
    `state_injOn`). Proof: lockstep descent — injectivity turns a shared vertex
    into a shared *predecessor*, decrementing both indices until one hits the
    `inr` entry; a lone `inr` clashes with the other's `inl`, so both hit `0`
    together (`a = b`) at the same entry (`inr x = inr x'`, i.e. `x = x'`). -/
theorem traj_cross_injective
    (f : U ⊕ A → U ⊕ B) (hf : Function.Injective f)
    (x x' : A) (fuel : Nat)
    (hno  : ∀ k, k < fuel → ∃ w, f (state f x k) = Sum.inl w)
    (hno' : ∀ k, k < fuel → ∃ w, f (state f x' k) = Sum.inl w) :
    ∀ a b, a ≤ fuel → b ≤ fuel →
      state f x a = state f x' b → x = x' ∧ a = b := by
  -- Descend on `a`, tracking `b` in lockstep.
  intro a
  induction a with
  | zero =>
      intro b _ hb hval
      -- `state x 0 = inr x`. Force `b = 0` (else `inr` vs `inl` clash), then read off `x = x'`.
      cases b with
      | zero =>
          simp only [state] at hval
          exact ⟨Sum.inr.inj hval, rfl⟩
      | succ b' =>
          exfalso
          have hb'fuel : b' < fuel := by omega
          obtain ⟨wb, hwb⟩ := hno' b' hb'fuel
          have hb1 : state f x' (b' + 1) = Sum.inl wb := by rw [state, hwb]
          rw [hb1] at hval
          simp only [state, reduceCtorEq] at hval
  | succ a' ih =>
      intro b ha hb hval
      cases b with
      | zero =>
          exfalso
          have ha'fuel : a' < fuel := by omega
          obtain ⟨wa, hwa⟩ := hno a' ha'fuel
          have ha1 : state f x (a' + 1) = Sum.inl wa := by rw [state, hwa]
          rw [ha1] at hval
          simp only [state, reduceCtorEq] at hval
      | succ b' =>
          have ha'fuel : a' < fuel := by omega
          have hb'fuel : b' < fuel := by omega
          obtain ⟨wa, hwa⟩ := hno a' ha'fuel
          obtain ⟨wb, hwb⟩ := hno' b' hb'fuel
          have ha1 : state f x (a' + 1) = Sum.inl wa := by rw [state, hwa]
          have hb1 : state f x' (b' + 1) = Sum.inl wb := by rw [state, hwb]
          rw [ha1, hb1] at hval
          have hww : wa = wb := Sum.inl.inj hval
          have hfeq : f (state f x a') = f (state f x' b') := by
            rw [hwa, hwb, hww]
          have hstateq : state f x a' = state f x' b' := hf hfeq
          have ha'le : a' ≤ fuel := by omega
          have hb'le : b' ≤ fuel := by omega
          obtain ⟨hxx, hab⟩ := ih b' ha'le hb'le hstateq
          exact ⟨hxx, by omega⟩

/-- Vertex-disjointness, restated as the text puts it: distinct inputs have no
    shared carrier vertex in the no-exit regime. Immediate from
    `traj_cross_injective`. -/
theorem traj_disjoint
    (f : U ⊕ A → U ⊕ B) (hf : Function.Injective f)
    (x x' : A) (fuel : Nat)
    (hno  : ∀ k, k < fuel → ∃ w, f (state f x k) = Sum.inl w)
    (hno' : ∀ k, k < fuel → ∃ w, f (state f x' k) = Sum.inl w)
    (hxx : x ≠ x') :
    ∀ a b, a ≤ fuel → b ≤ fuel → state f x a ≠ state f x' b := by
  intro a b ha hb hval
  exact hxx (traj_cross_injective f hf x x' fuel hno hno' a b ha hb hval).1

/-- **Class-wide exactness / carrier-sizing bound (ADR-0007 TH-1b (1)).** If each
    input in a finite set `S` is in the no-exit regime for `fuel` steps, then the
    loop-states — indexed by `(input in S) × (step < fuel)` — inject into the
    carrier `U`, so `|S| · fuel ≤ |u|`. This is the `Σ_inputs (Tᵢ − 1) ≤ |u|`
    bound (here every `Tᵢ − 1 = fuel`, uniform), the sum-over-inputs form of
    `t ≤ |u| + 1`; a loop carrier must be at least as wide as the iterating input
    class. The injection is `(x, k) ↦ the loop-state at step k+1`, injective by
    `traj_cross_injective`. -/
theorem traj_card_bound
    [Fintype U] [DecidableEq A]
    (f : U ⊕ A → U ⊕ B) (hf : Function.Injective f)
    (S : Finset A) (fuel : Nat)
    (hno : ∀ x ∈ S, ∀ k, k < fuel → ∃ w, f (state f x k) = Sum.inl w) :
    S.card * fuel ≤ Fintype.card U := by
  -- Pack (input, step) ↦ carrier state at step (k+1). The step index k < fuel
  -- gives state at index k+1 (a genuine `inl` carrier vertex), and the choose
  -- extracts the carrier witness `w ∈ U`.
  classical
  -- If the index set is empty the bound is trivial (`0 ≤ card U`); otherwise the
  -- no-exit hypothesis hands us a carrier element at step 0, so `U` is inhabited
  -- and the junk default in `g` below is well-defined (no `[Nonempty U]` needed).
  rcases Nat.eq_zero_or_pos (S.card * fuel) with h0 | hpos
  · rw [h0]; exact Nat.zero_le _
  have hfpos : 0 < fuel := by
    rcases Nat.eq_zero_or_pos fuel with h | h
    · subst h; simp at hpos
    · exact h
  have hSpos : 0 < S.card := by
    rcases Nat.eq_zero_or_pos S.card with h | h
    · rw [h] at hpos; simp at hpos
    · exact h
  obtain ⟨x0, hx0⟩ := Finset.card_pos.mp hSpos
  obtain ⟨w0, _hw0⟩ := hno x0 hx0 0 hfpos
  haveI : Nonempty U := ⟨w0⟩
  -- The map into U on the finite index type `S ×ˢ Finset.range fuel`.
  set P : Finset (A × Nat) := S ×ˢ Finset.range fuel with hP
  -- `g p` is the carrier witness of the loop-state at step `p.2 + 1` from input `p.1`.
  have gexists : ∀ p ∈ P, ∃ w : U, f (state f p.1 p.2) = Sum.inl w := by
    intro p hp
    rw [hP, Finset.mem_product, Finset.mem_range] at hp
    exact hno p.1 hp.1 p.2 hp.2
  let g : A × Nat → U := fun p =>
    if h : ∃ w : U, f (state f p.1 p.2) = Sum.inl w then h.choose
    else Classical.arbitrary U
  have hg : ∀ p ∈ P, f (state f p.1 p.2) = Sum.inl (g p) := by
    intro p hp
    have hex := gexists p hp
    show f (state f p.1 p.2) = Sum.inl (if h : ∃ w : U, f (state f p.1 p.2) = Sum.inl w
      then h.choose else Classical.arbitrary U)
    rw [dif_pos hex]
    exact hex.choose_spec
  -- Injectivity of g on the product finset.
  have hginj : Set.InjOn g (P : Set (A × Nat)) := by
    intro p hp q hq hpq
    rw [Finset.mem_coe] at hp hq
    have hpP : p ∈ P := hp
    have hqP : q ∈ P := hq
    rw [hP, Finset.mem_product, Finset.mem_range] at hp hq
    have hfp := hg p hpP
    have hfq := hg q hqP
    have hstep : state f p.1 (p.2 + 1) = state f q.1 (q.2 + 1) := by
      have h1 : state f p.1 (p.2 + 1) = Sum.inl (g p) := by rw [state, hfp]
      have h2 : state f q.1 (q.2 + 1) = Sum.inl (g q) := by rw [state, hfq]
      rw [h1, h2, hpq]
    obtain ⟨hxx, hkk⟩ :=
      traj_cross_injective f hf p.1 q.1 fuel (hno p.1 hp.1) (hno q.1 hq.1)
        (p.2 + 1) (q.2 + 1) (by omega) (by omega) hstep
    have hp2 : p.2 = q.2 := by omega
    exact Prod.ext hxx hp2
  -- inject P into U, so |P| ≤ |U|; and |P| = |S| * fuel.
  have hcard : P.card ≤ Fintype.card U := by
    have himg : (P.image g).card = P.card := Finset.card_image_of_injOn hginj
    calc P.card = (P.image g).card := himg.symm
      _ ≤ Fintype.card U := by
          rw [← Finset.card_univ]
          exact Finset.card_le_card (Finset.subset_univ _)
  have hPcard : P.card = S.card * fuel := by
    rw [hP, Finset.card_product, Finset.card_range]
  rw [hPcard] at hcard
  exact hcard

end Theseus.FiniteTrace
