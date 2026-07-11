/-
TH-1 forward-termination bound (the §Lemma + §Theorem core), mechanized
against Mathlib.

Text proof: docs/src/theory/finite-trace-totality.md. For `trace f : a ↔ b`
elaborated from an injection `f : u + a ↔ u + b` over a *finite* `u`, a forward
run exits within `|u| + 1` body applications (`|u| = Fintype.card U`), in the
text's fuel unit of one charge per body application.

SCOPE — what is and is NOT mechanized here (keep the obligation ledger honest):
  * Mechanized: the text's §Lemma (no loop-state repeats) as `state_injOn`, and
    its §Theorem (forward termination + the `|u|+1` upper bound) as
    `trace_terminates`. Proven under the *weaker* hypothesis `Injective f`
    (the text assumes a bijection; the forward direction only needs injectivity,
    so this strengthens the result). `trace_terminates_of_equiv` restates it
    under the text's bijection hypothesis. `tight_card_two` mechanizes the
    §Tightness / "granting only `|u|` fuel would OutOfFuel" corollary at |u|=2.
  * NOT mechanized: the backward run / mutual-inverse ("totality as isomorphism"
    half, text §Both directions), tightness as a *class-wide* exactness
    statement, the `|u|`-iterations restatement, and the WCET/register
    corollaries. These remain text-only in `finite-trace-totality.md`.

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

end Theseus.FiniteTrace
