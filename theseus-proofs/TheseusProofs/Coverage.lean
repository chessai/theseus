/-
TH-4a: clause coverage + linearity ⇔ total bijection (plain fragment).

The signature claim of the surface language: a clause table whose two columns
are each exhaustive and non-overlapping, with every variable used exactly once
per side, denotes a total bijection, and reading the table right-to-left
denotes its inverse. The written proof is `docs/src/theory/coverage-bijectivity.md`;
this file mechanizes it.

WHAT IS MODELLED. The checker's *core* view of a clause table, i.e. what
`crates/theseus-check` works on after name resolution:

  * `Ty`  -- core types `0 | 1 | a + b | a × b` (`theseus_core::ty::Type`
    without `Flat`);
  * `Val` -- the untyped core `Value` (`Unit | Left | Right | Pair`), with
    `enumerate` transcribing `theseus_core::prim::enumerate_values`;
  * `Pat` -- core patterns `x | () | inl p | inr p | (p, q)`. A surface
    constructor pattern `C p₁ … pₖ` is its injection path applied to the
    right-nested tuple of its arguments (`env::CtorInfo::path`), so a named
    ADT is modelled as its sum-of-products desugaring;
  * `Pat.matchInto` / `Pat.matchBind` / `Pat.instantiate` -- transcriptions of
    `match_bind_inner` / `match_bind` / `instantiate` in `elaborate.rs`, and
    `Pat.matchesVal` of `matches_value` in `coverage.rs`;
  * `apply` -- the per-value loop of `compute_bijection` (first matching
    clause in declaration order, then instantiate its right-hand side);
  * `Clause.WellTyped` / `Clause.Linear` / `Covers` -- the conditions enforced
    by `typing.rs` (T0102), `linearity.rs` (T0201/T0202) and `coverage.rs`
    (T0301/T0302);
  * `LPat`, `LPat.matchBind` / `LPat.instantiate` / `LPat.matchesVal`,
    `applyExt`, `LAccepted` -- the label-aware counterparts
    (`match_bind_extended_inner`, `instantiate_extended`,
    `matches_extended_value`, step 4 of `elab_clauses_label`,
    `check_side_extended`).

The Rust matchers also thread the surface type, but only to resolve `Nat`
literals and to reject a tuple pattern at a non-product type; on well-typed
patterns that is redundant, so the Lean matchers are untyped, like `Value`.

WHY NOT `Reversibility.Ty`. That type has a primitive `bit` and no `0`, and is
denoted intrinsically (`denoteTy : Ty → Type`). The checker instead works on
*untyped* values enumerated per core type, where `Bit` is the sum `1 + 1` and
`0` exists (it is what μ-type unrolling bottoms out in). Mirroring the checker
needs the untyped `Val` + `enumerate`, so this file has its own `Ty`.

WHAT IS PROVED (all under the axiom guard, `AxiomGuard.lean`):

  * transcription sanity: `mem_enumerate`, `Pat.isSome_matchBind`,
    `covers_iff` (the counting test = exhaustive ∧ pairwise non-overlapping),
    `Clause.wellTyped_iff` (declarative typing = the `var_types` test)
  * L1  `Pat.matchBind_instantiate`, `Pat.instantiate_matchBind`,
    `Pat.instantiate_matchBind_exact`
  * L2  `Clause.fire_swap`, `Clause.fire_swap_iff`
  * TH-4a soundness  `apply_total`, `apply_adjoint`, `adjoint_apply`,
    `denote_bijective`, `denote_adjoint_leftInverse`,
    `denote_adjoint_rightInverse`, `toEquiv`, `rel_iff_apply`
  * TH-4a converse  `covers_of_totalBijection`, `coverage_iff_totalBijection`,
    and `completeness_needs_irredundant` (the converse is FALSE without the
    extra `Irredundant` hypothesis)
  * TH-4a-label  `LPat.matchBind_eq` / `LPat.instantiate_eq` /
    `LPat.matchesVal_eq` and `LAccepted.toPlain` (the tag-split reduction),
    `applyExt_eq_apply`, `applyExt_total`, `applyExt_ladjoint`,
    `ladjoint_applyExt`, `LAccepted.toEquiv`
  * `Fixtures.*`: conformance-suite tables replayed (accepted / rejected)

WHAT IS NOT. Nothing here links the Lean definitions to the Rust source: the
correspondence is by inspection. Machine-word / numeric types (`Flat`, `U<n>`,
`W<n>`), `Nat` literal patterns, the surface layer (names, constructor and
label-name resolution, label-carrier inference, μ-type unrolling), the
elaboration to Core Π (`eval ⟦f⟧ =` the clause denotation), and the
instantiation of TH-1 (`FiniteTrace.lean`) with the label body `Φ` are not
modelled.
-/
import Mathlib

namespace Theseus.Coverage

/-! ## Core types and values -/

/-- Core value types: the empty type, unit, binary sums and binary products
    (`theseus_core::ty::Type` minus the machine-word former `Flat`). -/
inductive Ty
  | zero
  | unit
  | sum  (a b : Ty)
  | prod (a b : Ty)
  deriving DecidableEq, Repr

/-- Untyped core values, exactly `theseus_core::prim::Value`. -/
inductive Val
  | unit
  | inl  (v : Val)
  | inr  (v : Val)
  | pair (a b : Val)
  deriving DecidableEq, Repr

/-- `v.HasTy T`: the value `v` inhabits the core type `T`. -/
inductive Val.HasTy : Val → Ty → Prop
  | unit : Val.HasTy .unit .unit
  | inl {v : Val} {a b : Ty} : Val.HasTy v a → Val.HasTy (.inl v) (.sum a b)
  | inr {v : Val} {a b : Ty} : Val.HasTy v b → Val.HasTy (.inr v) (.sum a b)
  | pair {v w : Val} {a b : Ty} :
      Val.HasTy v a → Val.HasTy w b → Val.HasTy (.pair v w) (.prod a b)

/-- All inhabitants of a core type, in the checker's order. Transcribes
    `enumerate_values` (`crates/theseus-core/src/prim.rs`). -/
def enumerate : Ty → List Val
  | .zero     => []
  | .unit     => [.unit]
  | .sum a b  => (enumerate a).map .inl ++ (enumerate b).map .inr
  | .prod a b => (enumerate a).flatMap fun va => (enumerate b).map fun vb => .pair va vb

/-- The checker's enumeration is exactly the typing relation: quantifying over
    `enumerate T` (what `coverage.rs` does) is quantifying over the values of `T`. -/
theorem mem_enumerate {T : Ty} {v : Val} : v ∈ enumerate T ↔ v.HasTy T := by
  induction T generalizing v with
  | zero =>
    simp only [enumerate, List.not_mem_nil, false_iff]
    intro h; cases h
  | unit =>
    simp only [enumerate, List.mem_singleton]
    constructor
    · rintro rfl; exact .unit
    · intro h; cases h; rfl
  | sum a b iha ihb =>
    simp only [enumerate, List.mem_append, List.mem_map]
    constructor
    · rintro (⟨w, hw, rfl⟩ | ⟨w, hw, rfl⟩)
      · exact .inl (iha.1 hw)
      · exact .inr (ihb.1 hw)
    · intro h
      cases h with
      | inl h => exact Or.inl ⟨_, iha.2 h, rfl⟩
      | inr h => exact Or.inr ⟨_, ihb.2 h, rfl⟩
  | prod a b iha ihb =>
    simp only [enumerate, List.mem_flatMap, List.mem_map]
    constructor
    · rintro ⟨va, hva, vb, hvb, rfl⟩
      exact .pair (iha.1 hva) (ihb.1 hvb)
    · intro h
      cases h with
      | pair h1 h2 => exact ⟨_, iha.2 h1, _, ihb.2 h2, rfl⟩

instance (v : Val) (T : Ty) : Decidable (v.HasTy T) :=
  decidable_of_iff _ mem_enumerate

/-- The typed values of `T`. -/
abbrev TVal (T : Ty) : Type := { v : Val // v.HasTy T }

/-! ## Patterns, environments, matching and instantiation -/

/-- Pattern variables are names, as in the surface AST. -/
abbrev Var := String

/-- Core patterns. `unit` is the (empty) argument list of a nullary
    constructor; `inl`/`inr` are the steps of a constructor's injection path;
    `pair` is a tuple (and the curried argument list of a constructor). -/
inductive Pat
  | var  (x : Var)
  | unit
  | inl  (p : Pat)
  | inr  (p : Pat)
  | pair (p q : Pat)
  deriving DecidableEq, Repr

/-- Variable bindings: a partial map from names to values (the checker's
    `BTreeMap<String, Value>`). -/
abbrev Env := Var → Option Val

/-- The empty binding map. -/
def Env.empty : Env := fun _ => none

/-- `BTreeMap::insert`: bind `x` to `v`, overwriting any earlier binding. -/
def Env.insert (e : Env) (x : Var) (v : Val) : Env :=
  fun y => if y = x then some v else e y

namespace Pat

/-- The variable occurrences of a pattern, left to right, with multiplicity
    (`collect_vars` in `linearity.rs`). -/
def vars : Pat → List Var
  | var x    => [x]
  | unit     => []
  | inl p    => p.vars
  | inr p    => p.vars
  | pair p q => p.vars ++ q.vars

/-- Pattern typing under a variable-type assignment `Γ` (`check_pattern` in
    `typing.rs`). Every occurrence of `x` is at type `Γ x`, which is the
    declarative form of the checker's `var_types` map: a variable seen at two
    different types is rejected (T0102). -/
inductive HasTy (Γ : Var → Ty) : Pat → Ty → Prop
  | var {x : Var} {T : Ty} : Γ x = T → HasTy Γ (.var x) T
  | unit : HasTy Γ .unit .unit
  | inl {p : Pat} {a b : Ty} : HasTy Γ p a → HasTy Γ (.inl p) (.sum a b)
  | inr {p : Pat} {a b : Ty} : HasTy Γ p b → HasTy Γ (.inr p) (.sum a b)
  | pair {p q : Pat} {a b : Ty} :
      HasTy Γ p a → HasTy Γ q b → HasTy Γ (.pair p q) (.prod a b)

/-- Match `v` against `p`, threading the binding map as the checker does
    (`match_bind_inner` in `elaborate.rs`, which mutates one `BTreeMap`).

    A variable binds (overwriting); `unit` does not inspect the value, exactly
    as `match_bind_args` returns `true` on an empty argument list without
    looking at the payload; injections and pairs require the matching value
    constructor. -/
def matchInto : Pat → Val → Env → Option Env
  | var x,    v,         e => some (e.insert x v)
  | unit,     _,         e => some e
  | inl p,    .inl v,    e => matchInto p v e
  | inr p,    .inr v,    e => matchInto p v e
  | pair p q, .pair a b, e => (matchInto p a e).bind (matchInto q b)
  | _,        _,         _ => none

/-- `match_bind`: match from the empty binding map. -/
def matchBind (p : Pat) (v : Val) : Option Env := p.matchInto v Env.empty

/-- `instantiate` (`elaborate.rs`): rebuild a value from a pattern and a
    binding map; fails on an unbound variable. -/
def instantiate : Pat → Env → Option Val
  | var x,    e => e x
  | unit,     _ => some .unit
  | inl p,    e => (instantiate p e).map .inl
  | inr p,    e => (instantiate p e).map .inr
  | pair p q, e => (instantiate p e).bind fun a => (instantiate q e).map fun b => .pair a b

/-- `matches_value` (`coverage.rs`): the boolean matcher the coverage check
    uses. Proved equal to "`matchBind` succeeds" in `isSome_matchBind`. -/
def matchesVal : Pat → Val → Bool
  | var _,    _         => true
  | unit,     _         => true
  | inl p,    .inl v    => matchesVal p v
  | inr p,    .inr v    => matchesVal p v
  | pair p q, .pair a b => matchesVal p a && matchesVal q b
  | _,        _         => false

/-! ### Basic facts about matching -/

/-- Matching only touches the pattern's own variables. -/
theorem matchInto_frame {p : Pat} {v : Val} {e e' : Env} (h : p.matchInto v e = some e')
    {x : Var} (hx : x ∉ p.vars) : e' x = e x := by
  induction p generalizing v e e' with
  | var y =>
    simp only [matchInto, Option.some.injEq] at h
    subst h
    have hxy : x ≠ y := by simpa [vars] using hx
    simp [Env.insert, hxy]
  | unit =>
    simp only [matchInto, Option.some.injEq] at h
    rw [h]
  | inl p ih =>
    cases v <;> simp only [matchInto, reduceCtorEq] at h
    exact ih h hx
  | inr p ih =>
    cases v <;> simp only [matchInto, reduceCtorEq] at h
    exact ih h hx
  | pair p q ihp ihq =>
    cases v with
    | pair a b =>
      simp only [matchInto] at h
      cases h1 : p.matchInto a e with
      | none => simp [h1] at h
      | some e1 =>
        simp only [h1, Option.bind_some] at h
        simp only [vars, List.mem_append, not_or] at hx
        rw [ihq h hx.2, ihp h1 hx.1]
    | _ => simp [matchInto] at h

/-- The two matchers of the checker agree: `matches_value` is "`match_bind`
    succeeds", from any starting binding map. -/
theorem isSome_matchInto (p : Pat) (v : Val) (e : Env) :
    (p.matchInto v e).isSome = p.matchesVal v := by
  induction p generalizing v e with
  | var y => simp [matchInto, matchesVal]
  | unit => simp [matchInto, matchesVal]
  | inl p ih => cases v <;> simp [matchInto, matchesVal, ih]
  | inr p ih => cases v <;> simp [matchInto, matchesVal, ih]
  | pair p q ihp ihq =>
    cases v with
    | pair a b =>
      have hp := ihp a e
      simp only [matchInto, matchesVal]
      cases h1 : p.matchInto a e with
      | none => rw [h1] at hp; simp [← hp]
      | some e1 => rw [h1] at hp; simp [← hp, ihq]
    | _ => simp [matchInto, matchesVal]

theorem isSome_matchBind (p : Pat) (v : Val) : (p.matchBind v).isSome = p.matchesVal v :=
  isSome_matchInto p v Env.empty

/-- Instantiation only reads the pattern's own variables. -/
theorem instantiate_congr {p : Pat} {e e' : Env} (h : ∀ x ∈ p.vars, e x = e' x) :
    p.instantiate e = p.instantiate e' := by
  induction p with
  | var y => simpa [instantiate] using h y (by simp [vars])
  | unit => rfl
  | inl p ih => simp only [instantiate]; rw [ih h]
  | inr p ih => simp only [instantiate]; rw [ih h]
  | pair p q ihp ihq =>
    simp only [instantiate]
    rw [ihp (fun x hx => h x (by simp [vars, hx])), ihq (fun x hx => h x (by simp [vars, hx]))]

/-- `e` binds every variable in `xs` to a value of the type `Γ` assigns it. -/
def _root_.Theseus.Coverage.Env.TypedOn (Γ : Var → Ty) (xs : List Var) (e : Env) : Prop :=
  ∀ x ∈ xs, ∃ w, e x = some w ∧ w.HasTy (Γ x)

/-- Matching a typed value against a typed pattern binds each variable at its type. -/
theorem matchInto_typed {Γ : Var → Ty} {p : Pat} {T : Ty} (hp : p.HasTy Γ T)
    {v : Val} (hv : v.HasTy T) {e e' : Env} (h : p.matchInto v e = some e') :
    Env.TypedOn Γ p.vars e' := by
  induction hp generalizing v e e' with
  | @var y T hy =>
    simp only [matchInto, Option.some.injEq] at h
    subst h
    intro x hx
    have hxy : x = y := by simpa [vars] using hx
    subst hxy
    exact ⟨v, by simp [Env.insert], hy ▸ hv⟩
  | unit => intro x hx; simp [vars] at hx
  | inl _ ih =>
    cases hv with
    | inl hv => simp only [matchInto] at h; exact ih hv h
    | inr hv => simp [matchInto] at h
  | inr _ ih =>
    cases hv with
    | inl hv => simp [matchInto] at h
    | inr hv => simp only [matchInto] at h; exact ih hv h
  | @pair p q _ _ _ _ ihp ihq =>
    cases hv with
    | @pair va vb _ _ ha hb =>
      simp only [matchInto] at h
      cases h1 : p.matchInto va e with
      | none => simp [h1] at h
      | some e1 =>
        simp only [h1, Option.bind_some] at h
        intro x hx
        by_cases hxq : x ∈ q.vars
        · exact ihq hb h x hxq
        · have hxp : x ∈ p.vars := by simpa [vars, hxq] using hx
          obtain ⟨w, hw, hwt⟩ := ihp ha h1 x hxp
          exact ⟨w, by rw [matchInto_frame h hxq, hw], hwt⟩

/-- Instantiating a typed pattern with well-typed bindings succeeds at the
    pattern's type. -/
theorem instantiate_typed {Γ : Var → Ty} {p : Pat} {T : Ty} (hp : p.HasTy Γ T)
    {e : Env} (he : Env.TypedOn Γ p.vars e) : ∃ v, p.instantiate e = some v ∧ v.HasTy T := by
  induction hp with
  | @var y T hy =>
    obtain ⟨w, hw, hwt⟩ := he y (by simp [vars])
    exact ⟨w, by simpa [instantiate] using hw, hy ▸ hwt⟩
  | unit => exact ⟨.unit, rfl, .unit⟩
  | inl _ ih =>
    obtain ⟨v, hv, hvt⟩ := ih he
    exact ⟨.inl v, by simp [instantiate, hv], .inl hvt⟩
  | inr _ ih =>
    obtain ⟨v, hv, hvt⟩ := ih he
    exact ⟨.inr v, by simp [instantiate, hv], .inr hvt⟩
  | @pair p q _ _ _ _ ihp ihq =>
    obtain ⟨a, ha, hat⟩ := ihp (fun x hx => he x (by simp [vars, hx]))
    obtain ⟨b, hb, hbt⟩ := ihq (fun x hx => he x (by simp [vars, hx]))
    exact ⟨.pair a b, by simp [instantiate, ha, hb], .pair hat hbt⟩

/-! ### Lemma L1: a linear pattern is a bijection onto its environment space -/

/-- L1, threaded form of `matchBind_instantiate`. -/
theorem matchInto_instantiate {Γ : Var → Ty} {p : Pat} {T : Ty} (hp : p.HasTy Γ T)
    (hnd : p.vars.Nodup) {v : Val} (hv : v.HasTy T) {e e' : Env}
    (h : p.matchInto v e = some e') : p.instantiate e' = some v := by
  induction hp generalizing v e e' with
  | var hy =>
    simp only [matchInto, Option.some.injEq] at h
    subst h
    simp [instantiate, Env.insert]
  | unit => cases hv; rfl
  | inl _ ih =>
    cases hv with
    | inl hv =>
      simp only [matchInto] at h
      simp [instantiate, ih hnd hv h]
    | inr hv => simp [matchInto] at h
  | inr _ ih =>
    cases hv with
    | inl hv => simp [matchInto] at h
    | inr hv =>
      simp only [matchInto] at h
      simp [instantiate, ih hnd hv h]
  | @pair p q _ _ _ _ ihp ihq =>
    cases hv with
    | @pair va vb _ _ ha hb =>
      simp only [matchInto] at h
      cases h1 : p.matchInto va e with
      | none => simp [h1] at h
      | some e1 =>
        simp only [h1, Option.bind_some] at h
        have hnd' : p.vars.Nodup ∧ q.vars.Nodup ∧ ∀ x ∈ p.vars, x ∉ q.vars := by
          have := List.nodup_append.1 hnd
          refine ⟨this.1, this.2.1, fun x hxp hxq => ?_⟩
          exact this.2.2 x hxp x hxq rfl
        have hq := ihq hnd'.2.1 hb h
        have hp1 := ihp hnd'.1 ha h1
        have hcongr : p.instantiate e' = p.instantiate e1 :=
          instantiate_congr (fun x hx => matchInto_frame h (hnd'.2.2 x hx))
        simp [instantiate, hcongr, hp1, hq]

/-- **L1 (match, then substitute).** For a linear, well-typed pattern,
    substituting the bindings produced by matching `v` gives `v` back.
    Linearity is essential: `(x, x)` matches `(a, b)` with `x ↦ b` and
    substitutes to `(b, b)`. -/
theorem matchBind_instantiate {Γ : Var → Ty} {p : Pat} {T : Ty} (hp : p.HasTy Γ T)
    (hnd : p.vars.Nodup) {v : Val} (hv : v.HasTy T) {e : Env}
    (h : p.matchBind v = some e) : p.instantiate e = some v :=
  matchInto_instantiate hp hnd hv h

/-- L1, threaded form of `instantiate_matchBind`. -/
theorem instantiate_matchInto {p : Pat} {e : Env} {v : Val} (h : p.instantiate e = some v)
    (e0 : Env) : ∃ e', p.matchInto v e0 = some e' ∧ ∀ x ∈ p.vars, e' x = e x := by
  induction p generalizing v e0 with
  | var y =>
    refine ⟨e0.insert y v, rfl, ?_⟩
    intro x hx
    have hxy : x = y := by simpa [vars] using hx
    subst hxy
    simp only [instantiate] at h
    simp [Env.insert, h]
  | unit => exact ⟨e0, by simp [matchInto], by simp [vars]⟩
  | inl p ih =>
    simp only [instantiate] at h
    cases hw : p.instantiate e with
    | none => simp [hw] at h
    | some w =>
      simp only [hw, Option.map_some, Option.some.injEq] at h
      subst h
      obtain ⟨e', h1, h2⟩ := ih hw e0
      exact ⟨e', by simpa [matchInto] using h1, h2⟩
  | inr p ih =>
    simp only [instantiate] at h
    cases hw : p.instantiate e with
    | none => simp [hw] at h
    | some w =>
      simp only [hw, Option.map_some, Option.some.injEq] at h
      subst h
      obtain ⟨e', h1, h2⟩ := ih hw e0
      exact ⟨e', by simpa [matchInto] using h1, h2⟩
  | pair p q ihp ihq =>
    simp only [instantiate] at h
    cases ha : p.instantiate e with
    | none => simp [ha] at h
    | some a =>
      cases hb : q.instantiate e with
      | none => simp [ha, hb] at h
      | some b =>
        simp only [ha, hb, Option.bind_some, Option.map_some, Option.some.injEq] at h
        subst h
        obtain ⟨e1, h1, h1'⟩ := ihp ha e0
        obtain ⟨e', h2, h2'⟩ := ihq hb e1
        refine ⟨e', by simp [matchInto, h1, h2], ?_⟩
        intro x hx
        by_cases hxq : x ∈ q.vars
        · exact h2' x hxq
        · have hxp : x ∈ p.vars := by simpa [vars, hxq] using hx
          rw [matchInto_frame h2 hxq, h1' x hxp]

/-- **L1 (substitute, then match).** A value obtained by substitution matches
    the pattern, and matching recovers the bindings on the pattern's variables. -/
theorem instantiate_matchBind {p : Pat} {e : Env} {v : Val} (h : p.instantiate e = some v) :
    ∃ e', p.matchBind v = some e' ∧ ∀ x ∈ p.vars, e' x = e x :=
  instantiate_matchInto h Env.empty

/-- Matching from the empty map binds exactly the pattern's variables. -/
theorem matchBind_support {p : Pat} {v : Val} {e : Env} (h : p.matchBind v = some e)
    {x : Var} (hx : x ∉ p.vars) : e x = none :=
  matchInto_frame h hx

/-- **L1, exact form.** On environments supported on `vars p` (the doc's
    `Env(p)`), substitution followed by matching is the identity on the nose.
    With `matchBind_instantiate` this makes `σ_p` and `p[·]` mutually inverse. -/
theorem instantiate_matchBind_exact {p : Pat} {e : Env} {v : Val}
    (hsupp : ∀ x, x ∉ p.vars → e x = none) (h : p.instantiate e = some v) :
    p.matchBind v = some e := by
  obtain ⟨e', h1, h2⟩ := instantiate_matchBind h
  rw [h1]
  congr 1
  funext x
  by_cases hx : x ∈ p.vars
  · exact h2 x hx
  · rw [matchBind_support h1 hx, hsupp x hx]

end Pat

/-! ## Clauses and clause sets -/

/-- A clause `l <-> r`: a left pattern and a right pattern. -/
abbrev Clause := Pat × Pat

/-- A clause table, in declaration order. -/
abbrev ClauseSet := List Clause

namespace Clause

/-- The clause read right-to-left. -/
abbrev swap (c : Clause) : Clause := Prod.swap c

/-- Typing (`typing.rs`, T0102): the left pattern has type `A`, the right
    pattern has type `B`, and each variable has one type across both sides. -/
def WellTyped (A B : Ty) (c : Clause) : Prop :=
  ∃ Γ : Var → Ty, c.1.HasTy Γ A ∧ c.2.HasTy Γ B

/-- Linearity (`linearity.rs`): no variable occurs twice on a side (T0201),
    and the two sides bind the same variables (T0202). -/
def Linear (c : Clause) : Prop :=
  c.1.vars.Nodup ∧ c.2.vars.Nodup ∧ ∀ x, x ∈ c.1.vars ↔ x ∈ c.2.vars

instance (c : Clause) : Decidable c.Linear :=
  decidable_of_iff
    (c.1.vars.Nodup ∧ c.2.vars.Nodup ∧
      (∀ x ∈ c.1.vars, x ∈ c.2.vars) ∧ ∀ x ∈ c.2.vars, x ∈ c.1.vars)
    ⟨fun h => ⟨h.1, h.2.1, fun x => ⟨h.2.2.1 x, h.2.2.2 x⟩⟩,
     fun h => ⟨h.1, h.2.1, fun x => (h.2.2 x).1, fun x => (h.2.2 x).2⟩⟩

/-- One clause, run left-to-right: match the left pattern, substitute into the
    right pattern. `none` if the left pattern does not match. -/
def fire (c : Clause) (a : Val) : Option Val :=
  (c.1.matchBind a).bind c.2.instantiate

theorem WellTyped.swap {A B : Ty} {c : Clause} (h : c.WellTyped A B) : c.swap.WellTyped B A := by
  obtain ⟨Γ, hl, hr⟩ := h
  exact ⟨Γ, hr, hl⟩

theorem Linear.swap {c : Clause} (h : c.Linear) : c.swap.Linear :=
  ⟨h.2.1, h.1, fun x => (h.2.2 x).symm⟩

theorem fire_matches_fst {c : Clause} {a b : Val} (h : c.fire a = some b) :
    c.1.matchesVal a = true := by
  rw [← Pat.isSome_matchBind]
  cases hm : c.1.matchBind a with
  | none => simp [fire, hm] at h
  | some e => rfl

theorem fire_matches_snd {c : Clause} {a b : Val} (h : c.fire a = some b) :
    c.2.matchesVal b = true := by
  cases hm : c.1.matchBind a with
  | none => simp [fire, hm] at h
  | some e =>
    simp only [fire, hm, Option.bind_some] at h
    obtain ⟨e', h1, _⟩ := Pat.instantiate_matchBind h
    rw [← Pat.isSome_matchBind, h1]
    rfl

/-- A well-typed linear clause is total on its left cell and lands in `B`. -/
theorem fire_typed {A B : Ty} {c : Clause} (hty : c.WellTyped A B) (hlin : c.Linear)
    {a : Val} (ha : a.HasTy A) (hm : c.1.matchesVal a = true) :
    ∃ b, c.fire a = some b ∧ b.HasTy B := by
  obtain ⟨Γ, hl, hr⟩ := hty
  have hs : (c.1.matchBind a).isSome = true := by rw [Pat.isSome_matchBind]; exact hm
  obtain ⟨e, he⟩ := Option.isSome_iff_exists.1 hs
  have hte : Env.TypedOn Γ c.1.vars e := Pat.matchInto_typed hl ha he
  obtain ⟨b, hb, hbt⟩ :=
    Pat.instantiate_typed hr (fun x hx => hte x ((hlin.2.2 x).2 hx))
  exact ⟨b, by simp [fire, he, hb], hbt⟩

theorem fire_hasTy {A B : Ty} {c : Clause} (hty : c.WellTyped A B) (hlin : c.Linear)
    {a b : Val} (ha : a.HasTy A) (h : c.fire a = some b) : b.HasTy B := by
  obtain ⟨b', hb', hbt⟩ := fire_typed hty hlin ha (fire_matches_fst h)
  rw [h] at hb'
  exact Option.some.inj hb' ▸ hbt

/-- **L2 (each clause is a cell-bijection, one direction).** If a well-typed
    linear clause sends `a` to `b`, then the same clause read right-to-left
    sends `b` back to `a`. -/
theorem fire_swap {A B : Ty} {c : Clause} (hty : c.WellTyped A B) (hlin : c.Linear)
    {a b : Val} (ha : a.HasTy A) (h : c.fire a = some b) : c.swap.fire b = some a := by
  obtain ⟨Γ, hl, _⟩ := hty
  cases hm : c.1.matchBind a with
  | none => simp [fire, hm] at h
  | some e =>
    simp only [fire, hm, Option.bind_some] at h
    obtain ⟨e', h1, h2⟩ := Pat.instantiate_matchBind h
    have hcongr : c.1.instantiate e' = c.1.instantiate e :=
      Pat.instantiate_congr (fun x hx => h2 x ((hlin.2.2 x).1 hx))
    have hback : c.1.instantiate e = some a := Pat.matchBind_instantiate hl hlin.1 ha hm
    show (c.2.matchBind b).bind c.1.instantiate = some a
    rw [h1, Option.bind_some, hcongr, hback]

/-- **L2.** A well-typed linear clause is a bijection between its left cell
    and its right cell, with the right-to-left reading as inverse: on typed
    values, `c` sends `a` to `b` iff `c.swap` sends `b` to `a`. -/
theorem fire_swap_iff {A B : Ty} {c : Clause} (hty : c.WellTyped A B) (hlin : c.Linear)
    {a b : Val} (ha : a.HasTy A) (hb : b.HasTy B) :
    c.fire a = some b ↔ c.swap.fire b = some a :=
  ⟨fire_swap hty hlin ha, fun h => by simpa using fire_swap hty.swap hlin.swap hb h⟩

end Clause

/-- The clause table read right-to-left: the syntactic adjoint. -/
def adjoint (cs : ClauseSet) : ClauseSet := cs.map Clause.swap

@[simp] theorem adjoint_adjoint (cs : ClauseSet) : adjoint (adjoint cs) = cs := by
  simp [adjoint, List.map_map, Function.comp_def]

theorem map_fst_adjoint (cs : ClauseSet) : (adjoint cs).map Prod.fst = cs.map Prod.snd := by
  simp [adjoint, List.map_map, Function.comp_def]

theorem map_snd_adjoint (cs : ClauseSet) : (adjoint cs).map Prod.snd = cs.map Prod.fst := by
  simp [adjoint, List.map_map, Function.comp_def]

/-- The operational reading of a clause table: the first clause, in
    declaration order, whose left pattern matches fires. Transcribes the
    per-value loop of `compute_bijection` (`elaborate.rs`): a matched clause
    whose right-hand side fails to instantiate yields no output. -/
def apply : ClauseSet → Val → Option Val
  | [],      _ => none
  | c :: cs, v =>
      match c.1.matchBind v with
      | some e => c.2.instantiate e
      | none   => apply cs v

/-- The order-free reading: `a` is related to `b` when *some* clause sends `a`
    to `b`. -/
def Rel (cs : ClauseSet) (a b : Val) : Prop := ∃ c ∈ cs, c.fire a = some b

/-- Coverage of one column (`check_side_plain` in `coverage.rs`): every value
    of the column's type is matched by exactly one clause's pattern. Zero
    matches is T0301, more than one is T0302. -/
def Covers (ps : List Pat) (T : Ty) : Prop :=
  ∀ v ∈ enumerate T, ps.countP (fun p => p.matchesVal v) = 1

instance (ps : List Pat) (T : Ty) : Decidable (Covers ps T) := by
  unfold Covers
  infer_instance

/-- The column is exhaustive: every value of `T` is matched by some pattern. -/
def Exhaustive (ps : List Pat) (T : Ty) : Prop :=
  ∀ v, v.HasTy T → ∃ p ∈ ps, p.matchesVal v = true

/-- The column is non-overlapping: no value of `T` is matched by the patterns
    at two different positions. -/
def NonOverlapping (ps : List Pat) (T : Ty) : Prop :=
  ps.Pairwise fun p q => ∀ v, v.HasTy T → ¬(p.matchesVal v = true ∧ q.matchesVal v = true)

private theorem countP_eq_one_iff {α : Type} (P : α → Bool) (l : List α) :
    l.countP P = 1 ↔
      (∃ a ∈ l, P a = true) ∧ l.Pairwise (fun a b => ¬(P a = true ∧ P b = true)) := by
  induction l with
  | nil => simp
  | cons a l ih =>
    rw [List.countP_cons, List.pairwise_cons]
    by_cases ha : P a = true
    · rw [if_pos ha]
      constructor
      · intro h
        have h0 : ∀ b ∈ l, ¬P b = true := List.countP_eq_zero.1 (by omega)
        exact ⟨⟨a, List.mem_cons_self, ha⟩, fun b hb hab => h0 b hb hab.2,
          List.pairwise_of_forall_mem_list fun x hx _ _ hxy => h0 x hx hxy.1⟩
      · rintro ⟨_, hall, _⟩
        have h0 : l.countP P = 0 :=
          List.countP_eq_zero.2 fun b hb hpb => hall b hb ⟨ha, hpb⟩
        omega
    · rw [if_neg ha, Nat.add_zero, ih]
      constructor
      · rintro ⟨⟨b, hb, hpb⟩, hpw⟩
        exact ⟨⟨b, List.mem_cons_of_mem _ hb, hpb⟩, fun b _ hab => ha hab.1, hpw⟩
      · rintro ⟨⟨b, hb, hpb⟩, _, hpw⟩
        rcases List.mem_cons.1 hb with hba | hb'
        · exact absurd (hba ▸ hpb) ha
        · exact ⟨⟨b, hb', hpb⟩, hpw⟩

private theorem pairwise_forall_iff {α ι : Type} {S : ι → Prop} {R : ι → α → α → Prop}
    {l : List α} :
    (∀ i, S i → l.Pairwise (R i)) ↔ l.Pairwise (fun a b => ∀ i, S i → R i a b) := by
  induction l with
  | nil => simp
  | cons a l ih =>
    simp only [List.pairwise_cons, ← ih]
    constructor
    · intro h
      exact ⟨fun b hb i hi => (h i hi).1 b hb, fun i hi => (h i hi).2⟩
    · rintro ⟨h1, h2⟩ i hi
      exact ⟨fun b hb => h1 b hb i hi, h2 i hi⟩

/-- The checker's counting test is the written proof's condition: a column
    passes `coverage.rs` iff it is exhaustive and pairwise non-overlapping. -/
theorem covers_iff {ps : List Pat} {T : Ty} :
    Covers ps T ↔ Exhaustive ps T ∧ NonOverlapping ps T := by
  unfold Covers Exhaustive NonOverlapping
  rw [← pairwise_forall_iff]
  constructor
  · intro h
    exact ⟨fun v hv => ((countP_eq_one_iff _ _).1 (h v (mem_enumerate.2 hv))).1,
      fun v hv => ((countP_eq_one_iff _ _).1 (h v (mem_enumerate.2 hv))).2⟩
  · rintro ⟨h1, h2⟩ v hv
    have hv' := mem_enumerate.1 hv
    exact (countP_eq_one_iff _ _).2 ⟨h1 v hv', h2 v hv'⟩

/-- A clause table accepted by the static checks, at boundary types `A <-> B`. -/
structure Accepted (cs : ClauseSet) (A B : Ty) : Prop where
  /-- Typing: every clause is well-typed (T0102). -/
  typed  : ∀ c ∈ cs, c.WellTyped A B
  /-- Linearity: every clause is linear (T0201/T0202). -/
  linear : ∀ c ∈ cs, c.Linear
  /-- Coverage of the left column over `A` (T0301/T0302, `side: left`). -/
  coverL : Covers (cs.map Prod.fst) A
  /-- Coverage of the right column over `B` (T0301/T0302, `side: right`). -/
  coverR : Covers (cs.map Prod.snd) B

/-- The checks are symmetric: the adjoint of an accepted table is accepted. -/
theorem Accepted.adjoint {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) :
    Accepted (adjoint cs) B A where
  typed := by
    intro c hc
    obtain ⟨d, hd, rfl⟩ := List.mem_map.1 hc
    exact (h.typed d hd).swap
  linear := by
    intro c hc
    obtain ⟨d, hd, rfl⟩ := List.mem_map.1 hc
    exact (h.linear d hd).swap
  coverL := by rw [map_fst_adjoint]; exact h.coverR
  coverR := by rw [map_snd_adjoint]; exact h.coverL

/-! ## Operational facts about `apply` -/

theorem apply_cons_of_matches {c : Clause} {cs : ClauseSet} {v : Val}
    (h : c.1.matchesVal v = true) : apply (c :: cs) v = c.fire v := by
  have hs := Pat.isSome_matchBind c.1 v
  rw [h] at hs
  cases hm : c.1.matchBind v with
  | none => rw [hm] at hs; simp at hs
  | some e => simp [apply, Clause.fire, hm]

theorem apply_cons_of_not_matches {c : Clause} {cs : ClauseSet} {v : Val}
    (h : c.1.matchesVal v = false) : apply (c :: cs) v = apply cs v := by
  have hs := Pat.isSome_matchBind c.1 v
  rw [h] at hs
  cases hm : c.1.matchBind v with
  | none => simp [apply, hm]
  | some e => rw [hm] at hs; simp at hs

/-- Whatever `apply` returns was produced by some clause. -/
theorem rel_of_apply {cs : ClauseSet} {a b : Val} (h : apply cs a = some b) : Rel cs a b := by
  induction cs with
  | nil => simp [apply] at h
  | cons d cs ih =>
    by_cases hd : d.1.matchesVal a = true
    · rw [apply_cons_of_matches hd] at h
      exact ⟨d, List.mem_cons_self, h⟩
    · rw [apply_cons_of_not_matches (by simpa using hd)] at h
      obtain ⟨c, hc, hf⟩ := ih h
      exact ⟨c, List.mem_cons_of_mem _ hc, hf⟩

/-- If exactly one clause's left pattern matches `v`, first-match dispatch
    runs that clause, whatever its position: declaration order is irrelevant. -/
theorem apply_eq_fire {cs : ClauseSet} {v : Val}
    (h1 : cs.countP (fun c => c.1.matchesVal v) = 1) {c : Clause} (hc : c ∈ cs)
    (hm : c.1.matchesVal v = true) : apply cs v = c.fire v := by
  induction cs with
  | nil => simp at hc
  | cons d cs ih =>
    rw [List.countP_cons] at h1
    by_cases hd : d.1.matchesVal v = true
    · have h0 : ∀ c' ∈ cs, ¬c'.1.matchesVal v = true :=
        List.countP_eq_zero.1 (by simpa [hd] using h1)
      rcases List.mem_cons.1 hc with rfl | hc'
      · exact apply_cons_of_matches hd
      · exact absurd hm (h0 c hc')
    · have hd' : d.1.matchesVal v = false := by simpa using hd
      have h1' : cs.countP (fun c => c.1.matchesVal v) = 1 := by simpa [hd'] using h1
      rcases List.mem_cons.1 hc with rfl | hc'
      · exact absurd hm hd
      · rw [apply_cons_of_not_matches hd']
        exact ih h1' hc'

theorem Accepted.countL {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) {a : Val}
    (ha : a.HasTy A) : cs.countP (fun c => c.1.matchesVal a) = 1 := by
  have := h.coverL a (mem_enumerate.2 ha)
  rw [List.countP_map] at this
  exact this

/-! ## TH-4a, soundness: an accepted clause table is a total bijection -/

/-- **TH-4a (totality).** On an accepted table, every value of `A` has an
    image, and it is a value of `B`. -/
theorem apply_total {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) {a : Val}
    (ha : a.HasTy A) : ∃ b, apply cs a = some b ∧ b.HasTy B := by
  have h1 := h.countL ha
  have hpos : 0 < cs.countP (fun c => c.1.matchesVal a) := by omega
  obtain ⟨c, hc, hm⟩ := List.countP_pos_iff.1 hpos
  obtain ⟨b, hb, hbt⟩ := Clause.fire_typed (h.typed c hc) (h.linear c hc) ha hm
  exact ⟨b, by rw [apply_eq_fire h1 hc hm]; exact hb, hbt⟩

/-- On an accepted table the first-match function and the order-free clause
    relation coincide. -/
theorem rel_iff_apply {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) {a b : Val}
    (ha : a.HasTy A) : Rel cs a b ↔ apply cs a = some b := by
  constructor
  · rintro ⟨c, hc, hf⟩
    rw [apply_eq_fire (h.countL ha) hc (Clause.fire_matches_fst hf)]
    exact hf
  · exact rel_of_apply

/-- **TH-4a (the adjoint is a left inverse).** On an accepted table, if the
    table sends `a : A` to `b`, the table read right-to-left sends `b` back
    to `a`. -/
theorem apply_adjoint {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) {a b : Val}
    (ha : a.HasTy A) (hab : apply cs a = some b) : apply (adjoint cs) b = some a := by
  obtain ⟨c, hc, hf⟩ := rel_of_apply hab
  have hbt : b.HasTy B := Clause.fire_hasTy (h.typed c hc) (h.linear c hc) ha hf
  have hsw := Clause.fire_swap (h.typed c hc) (h.linear c hc) ha hf
  have hc' : c.swap ∈ adjoint cs := List.mem_map_of_mem hc
  rw [apply_eq_fire (h.adjoint.countL hbt) hc' (Clause.fire_matches_fst hsw)]
  exact hsw

/-- **TH-4a (the adjoint is a right inverse).** On an accepted table, if the
    table read right-to-left sends `b : B` to `a`, the table sends `a` to `b`. -/
theorem adjoint_apply {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) {a b : Val}
    (hb : b.HasTy B) (hba : apply (adjoint cs) b = some a) : apply cs a = some b := by
  have := apply_adjoint h.adjoint hb hba
  rwa [adjoint_adjoint] at this

/-- The denotation of an accepted clause table as a function on typed values:
    match the left column, instantiate the right column. A plain function; that
    it is a bijection is `denote_bijective`. -/
def denote {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) (a : TVal A) : TVal B :=
  ⟨(apply cs a.1).get (by
      obtain ⟨b, hb, _⟩ := apply_total h a.2
      rw [hb]; rfl),
    by
      obtain ⟨b, hb, hbt⟩ := apply_total h a.2
      simpa [hb] using hbt⟩

/-- `denote` is `apply` on typed values. -/
theorem apply_eq_denote {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) (a : TVal A) :
    apply cs a.1 = some (denote h a).1 := by
  simp [denote]

/-- **TH-4a (adjoint corollary, left).** The denotation of the adjoint table
    undoes the denotation of the table. -/
theorem denote_adjoint_leftInverse {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) :
    Function.LeftInverse (denote h.adjoint) (denote h) := by
  intro a
  apply Subtype.ext
  have h1 := apply_adjoint h a.2 (apply_eq_denote h a)
  have h2 := apply_eq_denote h.adjoint (denote h a)
  rw [h1] at h2
  exact (Option.some.inj h2).symm

/-- **TH-4a (adjoint corollary, right).** The denotation of the table undoes
    the denotation of the adjoint table. -/
theorem denote_adjoint_rightInverse {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) :
    Function.RightInverse (denote h.adjoint) (denote h) := by
  intro b
  apply Subtype.ext
  have h1 := adjoint_apply h b.2 (apply_eq_denote h.adjoint b)
  have h2 := apply_eq_denote h (denote h.adjoint b)
  rw [h1] at h2
  exact (Option.some.inj h2).symm

/-- **TH-4a.** An accepted clause table denotes a bijection `A → B`. -/
theorem denote_bijective {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) :
    Function.Bijective (denote h) :=
  ⟨(denote_adjoint_leftInverse h).injective, (denote_adjoint_rightInverse h).surjective⟩

/-- **TH-4a, packaged.** An accepted clause table is an equivalence of typed
    values whose inverse is the denotation of the syntactic adjoint. -/
def toEquiv {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) : TVal A ≃ TVal B where
  toFun := denote h
  invFun := denote h.adjoint
  left_inv := denote_adjoint_leftInverse h
  right_inv := denote_adjoint_rightInverse h

/-! ## TH-4a, converse: a total bijection forces coverage (given irredundancy) -/

/-- `R` restricted to `A × B` is the graph of a bijection between the values
    of `A` and the values of `B`: every `a : A` has exactly one `R`-image in
    `B`, and every `b : B` has exactly one `R`-preimage in `A`. -/
def IsTotalBijection (R : Val → Val → Prop) (A B : Ty) : Prop :=
  (∀ a, a.HasTy A → ∃! b, b.HasTy B ∧ R a b) ∧
  (∀ b, b.HasTy B → ∃! a, a.HasTy A ∧ R a b)

/-- No pair `(a, b)` with `a : A` is produced by the clauses at two different
    positions: the table has no redundant clause instance. -/
def Irredundant (cs : ClauseSet) (A : Ty) : Prop :=
  cs.Pairwise fun c d => ∀ a, a.HasTy A → ∀ b, ¬(c.fire a = some b ∧ d.fire a = some b)

/-- An accepted table's clause relation is a total bijection. -/
theorem Accepted.totalBijection {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) :
    IsTotalBijection (Rel cs) A B := by
  constructor
  · intro a ha
    obtain ⟨b, hab, hb⟩ := apply_total h ha
    refine ⟨b, ⟨hb, rel_of_apply hab⟩, ?_⟩
    rintro b' ⟨_, hrel⟩
    have := (rel_iff_apply h ha).1 hrel
    exact Option.some.inj (this.symm.trans hab)
  · intro b hb
    obtain ⟨a, hba, ha⟩ := apply_total h.adjoint hb
    refine ⟨a, ⟨ha, rel_of_apply (adjoint_apply h hb hba)⟩, ?_⟩
    rintro a' ⟨ha', hrel⟩
    have := apply_adjoint h ha' ((rel_iff_apply h ha').1 hrel)
    exact Option.some.inj (this.symm.trans hba)

/-- An accepted table is irredundant. -/
theorem Accepted.irredundant {cs : ClauseSet} {A B : Ty} (h : Accepted cs A B) :
    Irredundant cs A := by
  have hno := (covers_iff.1 h.coverL).2
  unfold NonOverlapping at hno
  rw [List.pairwise_map] at hno
  exact hno.imp fun hcd a ha b hf =>
    hcd a ha ⟨Clause.fire_matches_fst hf.1, Clause.fire_matches_fst hf.2⟩

/-- **TH-4a (converse).** For a well-typed linear clause table, if the clause
    relation is a total bijection between `A` and `B` *and the table is
    irredundant*, then both columns pass the coverage check. Irredundancy
    cannot be dropped: see `completeness_needs_irredundant`. -/
theorem covers_of_totalBijection {cs : ClauseSet} {A B : Ty}
    (hty : ∀ c ∈ cs, c.WellTyped A B) (hlin : ∀ c ∈ cs, c.Linear)
    (hbij : IsTotalBijection (Rel cs) A B) (hirr : Irredundant cs A) :
    Covers (cs.map Prod.fst) A ∧ Covers (cs.map Prod.snd) B := by
  refine ⟨covers_iff.2 ⟨?_, ?_⟩, covers_iff.2 ⟨?_, ?_⟩⟩
  · -- left column exhaustive: the relation is total
    intro a ha
    obtain ⟨b, ⟨_, c, hc, hf⟩, _⟩ := hbij.1 a ha
    exact ⟨c.1, List.mem_map_of_mem hc, Clause.fire_matches_fst hf⟩
  · -- left column non-overlapping: the relation is single-valued and irredundant
    unfold NonOverlapping
    rw [List.pairwise_map]
    refine hirr.imp_of_mem ?_
    intro c d hc hd hcd a ha hm
    obtain ⟨b1, hf1, hb1⟩ := Clause.fire_typed (hty c hc) (hlin c hc) ha hm.1
    obtain ⟨b2, hf2, hb2⟩ := Clause.fire_typed (hty d hd) (hlin d hd) ha hm.2
    have hb : b1 = b2 := (hbij.1 a ha).unique ⟨hb1, c, hc, hf1⟩ ⟨hb2, d, hd, hf2⟩
    subst hb
    exact hcd a ha b1 ⟨hf1, hf2⟩
  · -- right column exhaustive: the relation is onto
    intro b hb
    obtain ⟨a, ⟨_, c, hc, hf⟩, _⟩ := hbij.2 b hb
    exact ⟨c.2, List.mem_map_of_mem hc, Clause.fire_matches_snd hf⟩
  · -- right column non-overlapping: the relation is injective and irredundant
    unfold NonOverlapping
    rw [List.pairwise_map]
    refine hirr.imp_of_mem ?_
    intro c d hc hd hcd b hb hm
    obtain ⟨a1, hf1, ha1⟩ :=
      Clause.fire_typed (hty c hc).swap (hlin c hc).swap hb hm.1
    obtain ⟨a2, hf2, ha2⟩ :=
      Clause.fire_typed (hty d hd).swap (hlin d hd).swap hb hm.2
    have hg1 : c.fire a1 = some b :=
      (Clause.fire_swap_iff (hty c hc) (hlin c hc) ha1 hb).2 hf1
    have hg2 : d.fire a2 = some b :=
      (Clause.fire_swap_iff (hty d hd) (hlin d hd) ha2 hb).2 hf2
    have ha : a1 = a2 := (hbij.2 b hb).unique ⟨ha1, c, hc, hg1⟩ ⟨ha2, d, hd, hg2⟩
    subst ha
    exact hcd a1 ha1 b ⟨hg1, hg2⟩

/-- **TH-4a, both directions.** For a well-typed linear clause table, passing
    the coverage check on both columns is *equivalent* to: the clause relation
    is a total bijection `A ≅ B` and no pair is produced by two clauses. -/
theorem coverage_iff_totalBijection {cs : ClauseSet} {A B : Ty}
    (hty : ∀ c ∈ cs, c.WellTyped A B) (hlin : ∀ c ∈ cs, c.Linear) :
    (Covers (cs.map Prod.fst) A ∧ Covers (cs.map Prod.snd) B) ↔
      (IsTotalBijection (Rel cs) A B ∧ Irredundant cs A) := by
  constructor
  · rintro ⟨hL, hR⟩
    have h : Accepted cs A B := ⟨hty, hlin, hL, hR⟩
    exact ⟨h.totalBijection, h.irredundant⟩
  · rintro ⟨hbij, hirr⟩
    exact covers_of_totalBijection hty hlin hbij hirr

/-! ### The irredundancy hypothesis is necessary

`| x <-> x | O <-> O` over `Bit = 1 + 1` is well-typed and linear, and both its
clause relation and its first-match function are the identity on `Bit` -- a
total bijection. Yet the checker rejects it (T0302: `O` is matched by both
clauses). So "bijection ⇒ coverage" is false as stated; what coverage adds over
bijectivity is exactly `Irredundant`. -/

/-- `Bit = O | I`, lowered: `1 + 1`. -/
def bit : Ty := .sum .unit .unit

/-- `| x <-> x | O <-> O`. -/
def redundantId : ClauseSet :=
  [(.var "x", .var "x"), (.inl .unit, .inl .unit)]

theorem rel_redundantId {a : Val} (ha : a.HasTy bit) (b : Val) :
    Rel redundantId a b ↔ b = a := by
  cases ha with
  | inl h =>
    cases h
    simp [Rel, redundantId, Clause.fire, Pat.matchBind, Pat.matchInto, Pat.instantiate,
      Env.insert, eq_comm]
  | inr h =>
    cases h
    simp [Rel, redundantId, Clause.fire, Pat.matchBind, Pat.matchInto, Pat.instantiate,
      Env.insert, eq_comm]

/-- **The converse of TH-4a needs `Irredundant`.** A well-typed, linear clause
    table whose clause relation is a total bijection, and which nevertheless
    fails the coverage check on its left column. -/
theorem completeness_needs_irredundant :
    (∀ c ∈ redundantId, c.WellTyped bit bit) ∧ (∀ c ∈ redundantId, c.Linear) ∧
      IsTotalBijection (Rel redundantId) bit bit ∧
      ¬Covers (redundantId.map Prod.fst) bit := by
  refine ⟨?_, ?_, ⟨?_, ?_⟩, ?_⟩
  · intro c hc
    simp only [redundantId, List.mem_cons, List.not_mem_nil, or_false] at hc
    rcases hc with rfl | rfl
    · exact ⟨fun _ => bit, .var rfl, .var rfl⟩
    · exact ⟨fun _ => bit, .inl .unit, .inl .unit⟩
  · intro c hc
    simp only [redundantId, List.mem_cons, List.not_mem_nil, or_false] at hc
    rcases hc with rfl | rfl <;> simp [Clause.Linear, Pat.vars]
  · intro a ha
    exact ⟨a, ⟨ha, (rel_redundantId ha a).2 rfl⟩, fun b hb => (rel_redundantId ha b).1 hb.2⟩
  · intro b hb
    exact ⟨b, ⟨hb, (rel_redundantId hb b).2 rfl⟩,
      fun a ha => ((rel_redundantId ha.1 b).1 ha.2).symm⟩
  · intro h
    have := h (.inl .unit) (by simp [enumerate, bit])
    simp [redundantId, Pat.matchesVal] at this

/-! ## TH-4a-label: the label-extended theorem, by reduction to the plain one

A label iso `f : A <-> B` with labels `ℓ₀ … ℓₙ₋₁` of carrier types
`U₀ … Uₙ₋₁` is checked and elaborated over the *extended* types `U + A` and
`U + B`, where `U = U₀ + (U₁ + (… + Uₙ₋₁))` (`docs/src/theory/coverage-bijectivity-labels.md`).
Each clause side is either a plain boundary pattern or a label pattern `$ℓ p`.

This section transcribes the checker's label-aware matcher
(`matches_extended_value`, `match_bind_extended_inner`, `instantiate_extended`)
and proves it *equal* to the plain matcher run on the plain pattern
`inr p` / `inl (inj_ℓ p)`. The label-extended checks are therefore literally
the plain checks over `U + A`, `U + B`, and TH-4a-label is TH-4a. -/

/-- One step of an injection path into a right-nested sum (`env::Inj`). -/
inductive Inj
  | L
  | R
  deriving DecidableEq, Repr

/-- `env::injection_path`: the path selecting summand `index` of a right-nested
    sum of `count` summands -- `R` `index` times, then `L` unless it is the
    last summand; empty when there is at most one summand. -/
def injectionPath (index count : Nat) : List Inj :=
  if count ≤ 1 then []
  else List.replicate index Inj.R ++ (if index < count - 1 then [Inj.L] else [])

/-- `env::right_nested_sum`: the label carrier `U` (empty list ⇒ `0`). -/
def rightNestedSum : List Ty → Ty
  | []           => .zero
  | [t]          => t
  | t :: u :: us => .sum t (rightNestedSum (u :: us))

/-- `env::navigate`: follow an injection path into a value. -/
def navigate : Val → List Inj → Option Val
  | v,      []         => some v
  | .inl v, .L :: path => navigate v path
  | .inr v, .R :: path => navigate v path
  | _,      _ :: _     => none

/-- `inject_value` (`elaborate.rs`): wrap a payload in the injections of a
    path, first step outermost. -/
def injectValue (v : Val) : List Inj → Val
  | []         => v
  | .L :: path => .inl (injectValue v path)
  | .R :: path => .inr (injectValue v path)

/-- The pattern-level counterpart of `injectValue`. -/
def Pat.inject (p : Pat) : List Inj → Pat
  | []         => p
  | .L :: path => .inl (p.inject path)
  | .R :: path => .inr (p.inject path)

theorem injectionPath_succ {m k : Nat} (hm : m < k + 1) :
    injectionPath (m + 1) (k + 2) = Inj.R :: injectionPath m (k + 1) := by
  unfold injectionPath
  cases k with
  | zero =>
    have hm0 : m = 0 := by omega
    subst hm0
    simp
  | succ k => simp [List.replicate_succ]

theorem Pat.vars_inject (p : Pat) (path : List Inj) : (p.inject path).vars = p.vars := by
  induction path with
  | nil => rfl
  | cons i path ih => cases i <;> simpa [Pat.inject, Pat.vars] using ih

theorem Pat.matchInto_inject (p : Pat) (path : List Inj) (v : Val) (e : Env) :
    (p.inject path).matchInto v e = (navigate v path).bind fun w => p.matchInto w e := by
  induction path generalizing v with
  | nil => simp [Pat.inject, navigate]
  | cons i path ih =>
    cases i <;> cases v <;> simp [Pat.inject, Pat.matchInto, navigate, ih]

theorem Pat.matchesVal_inject (p : Pat) (path : List Inj) (v : Val) :
    (p.inject path).matchesVal v = (navigate v path).any p.matchesVal := by
  induction path generalizing v with
  | nil => simp [Pat.inject, navigate]
  | cons i path ih =>
    cases i <;> cases v <;> simp [Pat.inject, Pat.matchesVal, navigate, ih]

theorem Pat.instantiate_inject (p : Pat) (path : List Inj) (e : Env) :
    (p.inject path).instantiate e = (p.instantiate e).map fun u => injectValue u path := by
  induction path with
  | nil => simp [Pat.inject, injectValue]
  | cons i path ih =>
    cases i <;> simp [Pat.inject, Pat.instantiate, injectValue, ih, Function.comp_def]

/-- A pattern for summand `ℓ`, wrapped in its injection path, is a pattern
    for the whole right-nested sum. -/
theorem Pat.HasTy.inject {Γ : Var → Ty} {p : Pat} {U : Ty} (hp : p.HasTy Γ U) :
    ∀ {Us : List Ty} {ℓ : Nat}, Us[ℓ]? = some U →
      (p.inject (injectionPath ℓ Us.length)).HasTy Γ (rightNestedSum Us) := by
  intro Us
  induction Us with
  | nil => intro ℓ h; simp at h
  | cons t Us ih =>
    intro ℓ h
    cases Us with
    | nil =>
      cases ℓ with
      | zero =>
        simp only [List.getElem?_cons_zero, Option.some.injEq] at h
        subst h
        simpa [injectionPath, Pat.inject, rightNestedSum] using hp
      | succ m => simp at h
    | cons u us =>
      cases ℓ with
      | zero =>
        simp only [List.getElem?_cons_zero, Option.some.injEq] at h
        subst h
        simpa [injectionPath, Pat.inject, rightNestedSum] using Pat.HasTy.inl hp
      | succ m =>
        simp only [List.getElem?_cons_succ] at h
        have hm : m < us.length + 1 := by
          have := (List.getElem?_eq_some_iff.1 h).1
          simpa using this
        have hpath : injectionPath (m + 1) (t :: u :: us).length =
            Inj.R :: injectionPath m (u :: us).length := by
          simpa using injectionPath_succ hm
        rw [hpath]
        exact Pat.HasTy.inr (ih h)

/-- One side of a label clause: a plain boundary pattern, or `$ℓ p`. The
    label is named by its index in the iso's label list (the checker resolves
    the name with `labels.iter().position`). -/
inductive LPat
  | plain (p : Pat)
  | label (ℓ : Nat) (p : Pat)
  deriving DecidableEq, Repr

namespace LPat

/-- Variables of a label-clause side; the `$ℓ` tag binds nothing. -/
def vars : LPat → List Var
  | plain p   => p.vars
  | label _ p => p.vars

/-- Typing of a label-clause side against label carriers `Us` and boundary
    type `T`: a plain pattern is a pattern of `T`; `$ℓ p` requires `p` to be a
    pattern of the `ℓ`-th carrier. -/
def HasTy (Γ : Var → Ty) (Us : List Ty) (T : Ty) : LPat → Prop
  | plain p   => p.HasTy Γ T
  | label ℓ p => ∃ U, Us[ℓ]? = some U ∧ p.HasTy Γ U

/-- The label is one of the iso's `n` labels. -/
def Known (n : Nat) : LPat → Prop
  | plain _   => True
  | label ℓ _ => ℓ < n

/-- `match_bind_extended_inner`: a label pattern matches only label states
    `inl u` (navigating to its summand); a plain pattern matches only boundary
    values `inr a`. -/
def matchBind (n : Nat) : LPat → Val → Option Env
  | label ℓ p, .inl u =>
      if ℓ < n then (navigate u (injectionPath ℓ n)).bind p.matchBind else none
  | plain p,   .inr a => p.matchBind a
  | _,         _      => none

/-- `instantiate_extended`. -/
def instantiate (n : Nat) : LPat → Env → Option Val
  | label ℓ p, e =>
      if ℓ < n then (p.instantiate e).map fun u => .inl (injectValue u (injectionPath ℓ n))
      else none
  | plain p,   e => (p.instantiate e).map .inr

/-- `matches_extended_value` (`coverage.rs`). -/
def matchesVal (n : Nat) : LPat → Val → Bool
  | label ℓ p, .inl u => decide (ℓ < n) && (navigate u (injectionPath ℓ n)).any p.matchesVal
  | plain p,   .inr a => p.matchesVal a
  | _,         _      => false

/-- The reading of a label-clause side as an ordinary pattern over `U + T`:
    `$ℓ p ↦ inl (inj_ℓ p)`, plain `p ↦ inr p`. -/
def toPat (n : Nat) : LPat → Pat
  | plain p   => .inr p
  | label ℓ p => .inl (p.inject (injectionPath ℓ n))

theorem vars_toPat (n : Nat) (lp : LPat) : (lp.toPat n).vars = lp.vars := by
  cases lp <;> simp [toPat, vars, Pat.vars, Pat.vars_inject]

theorem HasTy.known {Γ : Var → Ty} {Us : List Ty} {T : Ty} {lp : LPat}
    (h : lp.HasTy Γ Us T) : lp.Known Us.length := by
  cases lp with
  | plain p => trivial
  | label ℓ p =>
    obtain ⟨U, hU, _⟩ := h
    exact (List.getElem?_eq_some_iff.1 hU).1

/-- Typing is preserved by the reading: a well-typed label-clause side is a
    well-typed plain pattern over the extended type. -/
theorem HasTy.toPat {Γ : Var → Ty} {Us : List Ty} {T : Ty} {lp : LPat}
    (h : lp.HasTy Γ Us T) : (lp.toPat Us.length).HasTy Γ (.sum (rightNestedSum Us) T) := by
  cases lp with
  | plain p => exact Pat.HasTy.inr h
  | label ℓ p =>
    obtain ⟨U, hU, hp⟩ := h
    exact Pat.HasTy.inl (hp.inject hU)

/-- **Tag split, matching.** The label-aware matcher *is* the plain matcher on
    the plain reading. -/
theorem matchBind_eq {n : Nat} {lp : LPat} (h : lp.Known n) (v : Val) :
    lp.matchBind n v = (lp.toPat n).matchBind v := by
  cases lp with
  | plain p => cases v <;> simp [matchBind, toPat, Pat.matchBind, Pat.matchInto]
  | label ℓ p =>
    have hℓ : ℓ < n := h
    cases v <;>
      simp [matchBind, toPat, Pat.matchBind, Pat.matchInto, Pat.matchInto_inject, hℓ]

/-- **Tag split, instantiation.** -/
theorem instantiate_eq {n : Nat} {lp : LPat} (h : lp.Known n) (e : Env) :
    lp.instantiate n e = (lp.toPat n).instantiate e := by
  cases lp with
  | plain p => simp [instantiate, toPat, Pat.instantiate]
  | label ℓ p =>
    have hℓ : ℓ < n := h
    simp [instantiate, toPat, Pat.instantiate, Pat.instantiate_inject, hℓ, Function.comp_def]

/-- **Tag split, coverage.** The label-aware coverage matcher *is* the plain
    coverage matcher on the plain reading. -/
theorem matchesVal_eq {n : Nat} {lp : LPat} (h : lp.Known n) (v : Val) :
    lp.matchesVal n v = (lp.toPat n).matchesVal v := by
  cases lp with
  | plain p => cases v <;> simp [matchesVal, toPat, Pat.matchesVal]
  | label ℓ p =>
    have hℓ : ℓ < n := h
    cases v <;> simp [matchesVal, toPat, Pat.matchesVal, Pat.matchesVal_inject, hℓ]

end LPat

/-- A label clause. -/
abbrev LClause := LPat × LPat

/-- A label clause table, in declaration order. -/
abbrev LClauseSet := List LClause

/-- Typing of a label clause: both sides typed, each variable at one type,
    each label at one carrier (the same `Us`) on both columns. -/
def LClause.WellTyped (Us : List Ty) (A B : Ty) (c : LClause) : Prop :=
  ∃ Γ : Var → Ty, c.1.HasTy Γ Us A ∧ c.2.HasTy Γ Us B

/-- Linearity of a label clause, exactly as for a plain clause. -/
def LClause.Linear (c : LClause) : Prop :=
  c.1.vars.Nodup ∧ c.2.vars.Nodup ∧ ∀ x, x ∈ c.1.vars ↔ x ∈ c.2.vars

instance (c : LClause) : Decidable c.Linear :=
  decidable_of_iff
    (c.1.vars.Nodup ∧ c.2.vars.Nodup ∧
      (∀ x ∈ c.1.vars, x ∈ c.2.vars) ∧ ∀ x ∈ c.2.vars, x ∈ c.1.vars)
    ⟨fun h => ⟨h.1, h.2.1, fun x => ⟨h.2.2.1 x, h.2.2.2 x⟩⟩,
     fun h => ⟨h.1, h.2.1, fun x => (h.2.2 x).1, fun x => (h.2.2 x).2⟩⟩

/-- A label clause read as a plain clause over the extended types. -/
def LClause.toClause (n : Nat) (c : LClause) : Clause := (c.1.toPat n, c.2.toPat n)

/-- A label clause table read as a plain clause table `U + A <-> U + B`. -/
def elabLabels (n : Nat) (cs : LClauseSet) : ClauseSet := cs.map (LClause.toClause n)

/-- The label clause table read right-to-left. -/
def ladjoint (cs : LClauseSet) : LClauseSet := cs.map Prod.swap

theorem elabLabels_ladjoint (n : Nat) (cs : LClauseSet) :
    elabLabels n (ladjoint cs) = adjoint (elabLabels n cs) := by
  simp [elabLabels, ladjoint, adjoint, List.map_map, Function.comp_def, LClause.toClause]

/-- The body map `Φ` of a label iso, as the checker computes it
    (step 4 of `elab_clauses_label`): first matching clause under the
    label-aware matcher, then label-aware instantiation. -/
def applyExt (n : Nat) : LClauseSet → Val → Option Val
  | [],      _ => none
  | c :: cs, v =>
      match c.1.matchBind n v with
      | some e => c.2.instantiate n e
      | none   => applyExt n cs v

/-- A label clause table accepted by the label-extended static checks, for
    label carriers `Us` and boundary types `A <-> B`. Coverage is over the
    extended types `U + A` and `U + B` (`check_side_extended`); T0301 and
    T0303 are the two tags of one exhaustiveness condition. -/
structure LAccepted (cs : LClauseSet) (Us : List Ty) (A B : Ty) : Prop where
  /-- Typing, with one carrier per label shared by both columns. -/
  typed  : ∀ c ∈ cs, c.WellTyped Us A B
  /-- Linearity, unchanged from the plain case. -/
  linear : ∀ c ∈ cs, c.Linear
  /-- Coverage of the left column over `U + A`. -/
  coverL : ∀ v ∈ enumerate (.sum (rightNestedSum Us) A),
    (cs.map Prod.fst).countP (fun p => p.matchesVal Us.length v) = 1
  /-- Coverage of the right column over `U + B`. -/
  coverR : ∀ v ∈ enumerate (.sum (rightNestedSum Us) B),
    (cs.map Prod.snd).countP (fun p => p.matchesVal Us.length v) = 1

theorem LAccepted.known {cs : LClauseSet} {Us : List Ty} {A B : Ty} (h : LAccepted cs Us A B) :
    ∀ c ∈ cs, c.1.Known Us.length ∧ c.2.Known Us.length := by
  intro c hc
  obtain ⟨Γ, hl, hr⟩ := h.typed c hc
  exact ⟨hl.known, hr.known⟩

/-- **Tag-split lemma (the reduction).** The label-extended checks on a label
    clause table are the plain TH-4a checks on its reading over `U + A`,
    `U + B`. -/
theorem LAccepted.toPlain {cs : LClauseSet} {Us : List Ty} {A B : Ty}
    (h : LAccepted cs Us A B) :
    Accepted (elabLabels Us.length cs) (.sum (rightNestedSum Us) A)
      (.sum (rightNestedSum Us) B) where
  typed := by
    intro c hc
    obtain ⟨d, hd, rfl⟩ := List.mem_map.1 hc
    obtain ⟨Γ, hl, hr⟩ := h.typed d hd
    exact ⟨Γ, hl.toPat, hr.toPat⟩
  linear := by
    intro c hc
    obtain ⟨d, hd, rfl⟩ := List.mem_map.1 hc
    simpa [Clause.Linear, LClause.Linear, LClause.toClause, LPat.vars_toPat] using h.linear d hd
  coverL := by
    intro v hv
    have hmap : (elabLabels Us.length cs).map Prod.fst =
        (cs.map Prod.fst).map (LPat.toPat Us.length) := by
      simp [elabLabels, LClause.toClause, List.map_map, Function.comp_def]
    rw [hmap, List.countP_map, ← h.coverL v hv]
    apply List.countP_congr
    intro lp hlp
    obtain ⟨c, hc, rfl⟩ := List.mem_map.1 hlp
    simp [LPat.matchesVal_eq (h.known c hc).1]
  coverR := by
    intro v hv
    have hmap : (elabLabels Us.length cs).map Prod.snd =
        (cs.map Prod.snd).map (LPat.toPat Us.length) := by
      simp [elabLabels, LClause.toClause, List.map_map, Function.comp_def]
    rw [hmap, List.countP_map, ← h.coverR v hv]
    apply List.countP_congr
    intro lp hlp
    obtain ⟨c, hc, rfl⟩ := List.mem_map.1 hlp
    simp [LPat.matchesVal_eq (h.known c hc).2]

/-- The checker's label-aware body map is the plain clause-table function of
    the reading. -/
theorem applyExt_eq_apply {n : Nat} {cs : LClauseSet}
    (hk : ∀ c ∈ cs, c.1.Known n ∧ c.2.Known n) (v : Val) :
    applyExt n cs v = apply (elabLabels n cs) v := by
  induction cs with
  | nil => rfl
  | cons c cs ih =>
    have hc := hk c List.mem_cons_self
    have ih' := ih fun d hd => hk d (List.mem_cons_of_mem _ hd)
    simp only [applyExt, elabLabels, List.map_cons, apply, LClause.toClause,
      LPat.matchBind_eq hc.1, LPat.instantiate_eq hc.2]
    cases (c.1.toPat n).matchBind v with
    | none => simpa [elabLabels] using ih'
    | some e => rfl

/-- The label checks are symmetric in the two columns. -/
theorem LAccepted.ladjoint {cs : LClauseSet} {Us : List Ty} {A B : Ty}
    (h : LAccepted cs Us A B) : LAccepted (Coverage.ladjoint cs) Us B A where
  typed := by
    intro c hc
    obtain ⟨d, hd, rfl⟩ := List.mem_map.1 hc
    obtain ⟨Γ, hl, hr⟩ := h.typed d hd
    exact ⟨Γ, hr, hl⟩
  linear := by
    intro c hc
    obtain ⟨d, hd, rfl⟩ := List.mem_map.1 hc
    obtain ⟨h1, h2, h3⟩ := h.linear d hd
    exact ⟨h2, h1, fun x => (h3 x).symm⟩
  coverL := by
    have hmap : (Coverage.ladjoint cs).map Prod.fst = cs.map Prod.snd := by
      simp [Coverage.ladjoint, List.map_map, Function.comp_def]
    rw [hmap]; exact h.coverR
  coverR := by
    have hmap : (Coverage.ladjoint cs).map Prod.snd = cs.map Prod.fst := by
      simp [Coverage.ladjoint, List.map_map, Function.comp_def]
    rw [hmap]; exact h.coverL

/-- **TH-4a-label (totality).** The body map of an accepted label iso is
    total on `U + A` and lands in `U + B`. -/
theorem applyExt_total {cs : LClauseSet} {Us : List Ty} {A B : Ty} (h : LAccepted cs Us A B)
    {a : Val} (ha : a.HasTy (.sum (rightNestedSum Us) A)) :
    ∃ b, applyExt Us.length cs a = some b ∧ b.HasTy (.sum (rightNestedSum Us) B) := by
  rw [applyExt_eq_apply h.known]
  exact apply_total h.toPlain ha

/-- **TH-4a-label (the adjoint is a left inverse).** -/
theorem applyExt_ladjoint {cs : LClauseSet} {Us : List Ty} {A B : Ty}
    (h : LAccepted cs Us A B) {a b : Val} (ha : a.HasTy (.sum (rightNestedSum Us) A))
    (hab : applyExt Us.length cs a = some b) :
    applyExt Us.length (ladjoint cs) b = some a := by
  rw [applyExt_eq_apply h.known] at hab
  rw [applyExt_eq_apply h.ladjoint.known, elabLabels_ladjoint]
  exact apply_adjoint h.toPlain ha hab

/-- **TH-4a-label (the adjoint is a right inverse).** -/
theorem ladjoint_applyExt {cs : LClauseSet} {Us : List Ty} {A B : Ty}
    (h : LAccepted cs Us A B) {a b : Val} (hb : b.HasTy (.sum (rightNestedSum Us) B))
    (hba : applyExt Us.length (ladjoint cs) b = some a) :
    applyExt Us.length cs a = some b := by
  rw [applyExt_eq_apply h.ladjoint.known, elabLabels_ladjoint] at hba
  rw [applyExt_eq_apply h.known]
  exact adjoint_apply h.toPlain hb hba

/-- **TH-4a-label, packaged.** An accepted label clause table denotes an
    equivalence `Φ : U + A ≃ U + B`, the body `trace` consumes (TH-1's
    hypothesis). -/
def LAccepted.toEquiv {cs : LClauseSet} {Us : List Ty} {A B : Ty} (h : LAccepted cs Us A B) :
    TVal (.sum (rightNestedSum Us) A) ≃ TVal (.sum (rightNestedSum Us) B) :=
  Coverage.toEquiv h.toPlain

/-- `Φ` is the checker's body map. -/
theorem LAccepted.applyExt_eq_toEquiv {cs : LClauseSet} {Us : List Ty} {A B : Ty}
    (h : LAccepted cs Us A B) (a : TVal (.sum (rightNestedSum Us) A)) :
    applyExt Us.length cs a.1 = some (h.toEquiv a).1 := by
  rw [applyExt_eq_apply h.known]
  exact apply_eq_denote h.toPlain a

/-- `Φ⁻¹` is the checker's body map of the label table read right-to-left. -/
theorem LAccepted.applyExt_ladjoint_eq_toEquiv_symm {cs : LClauseSet} {Us : List Ty} {A B : Ty}
    (h : LAccepted cs Us A B) (b : TVal (.sum (rightNestedSum Us) B)) :
    applyExt Us.length (Coverage.ladjoint cs) b.1 = some (h.toEquiv.symm b).1 := by
  rw [applyExt_eq_apply h.ladjoint.known, elabLabels_ladjoint]
  exact apply_eq_denote h.toPlain.adjoint b

/-! ## The checks as decision procedures

`Accepted` and `LAccepted` are stated declaratively (typing is "there is a
variable-type assignment"). This section shows they are decidable by the
checker's own procedure: record the type at which each variable occurrence is
checked (`var_types` in `typing.rs`) and reject a variable seen at two types.
That makes acceptance of a concrete table a closed, kernel-checkable fact. -/

/-- The typed variable occurrences of `p` checked against `T`, left to right;
    `none` if the pattern's shape does not fit the type. -/
def Pat.occs : Pat → Ty → Option (List (Var × Ty))
  | .var x,    T         => some [(x, T)]
  | .unit,     .unit     => some []
  | .inl p,    .sum a _  => p.occs a
  | .inr p,    .sum _ b  => p.occs b
  | .pair p q, .prod a b => (p.occs a).bind fun l => (q.occs b).map fun r => l ++ r
  | _,         _         => none

/-- `Γ` gives every recorded occurrence its recorded type. -/
def Agrees (Γ : Var → Ty) (occ : List (Var × Ty)) : Prop := ∀ o ∈ occ, Γ o.1 = o.2

/-- No variable is recorded at two different types (the `var_types` test). -/
def Consistent (occ : List (Var × Ty)) : Prop :=
  ∀ a ∈ occ, ∀ b ∈ occ, a.1 = b.1 → a.2 = b.2

instance (occ : List (Var × Ty)) : Decidable (Consistent occ) := by
  unfold Consistent
  infer_instance

theorem Pat.hasTy_iff_occs {Γ : Var → Ty} {p : Pat} {T : Ty} :
    p.HasTy Γ T ↔ ∃ occ, p.occs T = some occ ∧ Agrees Γ occ := by
  constructor
  · intro h
    induction h with
    | @var x T hx => exact ⟨[(x, T)], rfl, by simpa [Agrees] using hx⟩
    | unit => exact ⟨[], rfl, by simp [Agrees]⟩
    | inl _ ih =>
      obtain ⟨occ, h1, h2⟩ := ih
      exact ⟨occ, by simpa [Pat.occs] using h1, h2⟩
    | inr _ ih =>
      obtain ⟨occ, h1, h2⟩ := ih
      exact ⟨occ, by simpa [Pat.occs] using h1, h2⟩
    | pair _ _ ihp ihq =>
      obtain ⟨l, hl, hla⟩ := ihp
      obtain ⟨r, hr, hra⟩ := ihq
      refine ⟨l ++ r, by simp [Pat.occs, hl, hr], ?_⟩
      intro o ho
      exact (List.mem_append.1 ho).elim (hla o) (hra o)
  · rintro ⟨occ, h1, h2⟩
    induction p generalizing T occ with
    | var x =>
      simp only [Pat.occs, Option.some.injEq] at h1
      subst h1
      exact .var (by simpa [Agrees] using h2)
    | unit =>
      cases T with
      | unit => exact .unit
      | _ => simp [Pat.occs] at h1
    | inl p ih =>
      cases T with
      | sum a b => exact .inl (ih _ (by simpa [Pat.occs] using h1) h2)
      | _ => simp [Pat.occs] at h1
    | inr p ih =>
      cases T with
      | sum a b => exact .inr (ih _ (by simpa [Pat.occs] using h1) h2)
      | _ => simp [Pat.occs] at h1
    | pair p q ihp ihq =>
      cases T with
      | prod a b =>
        simp only [Pat.occs] at h1
        cases hl : p.occs a with
        | none => simp [hl] at h1
        | some l =>
          cases hr : q.occs b with
          | none => simp [hl, hr] at h1
          | some r =>
            simp only [hl, hr, Option.bind_some, Option.map_some, Option.some.injEq] at h1
            subst h1
            exact .pair (ihp _ hl fun o ho => h2 o (List.mem_append_left _ ho))
              (ihq _ hr fun o ho => h2 o (List.mem_append_right _ ho))
      | _ => simp [Pat.occs] at h1

theorem exists_agrees_iff {occ : List (Var × Ty)} : (∃ Γ, Agrees Γ occ) ↔ Consistent occ := by
  constructor
  · rintro ⟨Γ, h⟩ a ha b hb hab
    rw [← h a ha, ← h b hb, hab]
  · intro h
    refine ⟨fun x => ((occ.find? fun o => o.1 == x).map Prod.snd).getD .unit, ?_⟩
    intro o ho
    cases hf : occ.find? (fun o' => o'.1 == o.1) with
    | none =>
      have := List.find?_eq_none.1 hf o ho
      simp at this
    | some o' =>
      have hmem := List.mem_of_find?_eq_some hf
      have hp := List.find?_some hf
      simp only [hf, Option.map_some, Option.getD_some]
      exact h o' hmem o ho (by simpa using hp)

/-- The typing test of one clause, as the checker runs it: both sides fit
    their types and the recorded occurrences are consistent. -/
def typeCheckOccs (oL oR : Option (List (Var × Ty))) : Bool :=
  match oL, oR with
  | some l, some r => decide (Consistent (l ++ r))
  | _,      _      => false

theorem typeCheckOccs_iff {PL PR : (Var → Ty) → Prop} {oL oR : Option (List (Var × Ty))}
    (hL : ∀ Γ, PL Γ ↔ ∃ occ, oL = some occ ∧ Agrees Γ occ)
    (hR : ∀ Γ, PR Γ ↔ ∃ occ, oR = some occ ∧ Agrees Γ occ) :
    typeCheckOccs oL oR = true ↔ ∃ Γ, PL Γ ∧ PR Γ := by
  cases oL with
  | none => simp [typeCheckOccs, hL]
  | some l =>
    cases oR with
    | none => simp [typeCheckOccs, hR]
    | some r =>
      simp only [typeCheckOccs, decide_eq_true_eq, ← exists_agrees_iff]
      constructor
      · rintro ⟨Γ, h⟩
        exact ⟨Γ, (hL Γ).2 ⟨l, rfl, fun o ho => h o (List.mem_append_left _ ho)⟩,
          (hR Γ).2 ⟨r, rfl, fun o ho => h o (List.mem_append_right _ ho)⟩⟩
      · rintro ⟨Γ, h1, h2⟩
        obtain ⟨l', hl', hla⟩ := (hL Γ).1 h1
        obtain ⟨r', hr', hra⟩ := (hR Γ).1 h2
        cases hl'
        cases hr'
        exact ⟨Γ, fun o ho => (List.mem_append.1 ho).elim (hla o) (hra o)⟩

/-- **Typing is the checker's `var_types` test.** A clause is well-typed iff
    both sides fit their types and no variable occurs at two types. -/
theorem Clause.wellTyped_iff {A B : Ty} {c : Clause} :
    typeCheckOccs (c.1.occs A) (c.2.occs B) = true ↔ c.WellTyped A B :=
  typeCheckOccs_iff (fun _ => Pat.hasTy_iff_occs) (fun _ => Pat.hasTy_iff_occs)

instance (A B : Ty) (c : Clause) : Decidable (c.WellTyped A B) :=
  decidable_of_iff _ Clause.wellTyped_iff

instance (cs : ClauseSet) (A B : Ty) : Decidable (Accepted cs A B) :=
  decidable_of_iff
    ((∀ c ∈ cs, c.WellTyped A B) ∧ (∀ c ∈ cs, c.Linear) ∧
      Covers (cs.map Prod.fst) A ∧ Covers (cs.map Prod.snd) B)
    ⟨fun h => ⟨h.1, h.2.1, h.2.2.1, h.2.2.2⟩, fun h => ⟨h.typed, h.linear, h.coverL, h.coverR⟩⟩

/-- Typed variable occurrences of a label-clause side. -/
def LPat.occs (Us : List Ty) (T : Ty) : LPat → Option (List (Var × Ty))
  | .plain p   => p.occs T
  | .label ℓ p => Us[ℓ]?.bind p.occs

theorem LPat.hasTy_iff_occs {Γ : Var → Ty} {Us : List Ty} {T : Ty} {lp : LPat} :
    lp.HasTy Γ Us T ↔ ∃ occ, lp.occs Us T = some occ ∧ Agrees Γ occ := by
  cases lp with
  | plain p => exact Pat.hasTy_iff_occs
  | label ℓ p =>
    simp only [LPat.HasTy, LPat.occs]
    cases Us[ℓ]? with
    | none => simp
    | some U => simp [Pat.hasTy_iff_occs]

theorem LClause.wellTyped_iff {Us : List Ty} {A B : Ty} {c : LClause} :
    typeCheckOccs (c.1.occs Us A) (c.2.occs Us B) = true ↔ c.WellTyped Us A B :=
  typeCheckOccs_iff (fun _ => LPat.hasTy_iff_occs) (fun _ => LPat.hasTy_iff_occs)

instance (Us : List Ty) (A B : Ty) (c : LClause) : Decidable (c.WellTyped Us A B) :=
  decidable_of_iff _ LClause.wellTyped_iff

instance (cs : LClauseSet) (Us : List Ty) (A B : Ty) : Decidable (LAccepted cs Us A B) :=
  decidable_of_iff
    ((∀ c ∈ cs, c.WellTyped Us A B) ∧ (∀ c ∈ cs, c.Linear) ∧
      (∀ v ∈ enumerate (.sum (rightNestedSum Us) A),
        (cs.map Prod.fst).countP (fun p => p.matchesVal Us.length v) = 1) ∧
      (∀ v ∈ enumerate (.sum (rightNestedSum Us) B),
        (cs.map Prod.snd).countP (fun p => p.matchesVal Us.length v) = 1))
    ⟨fun h => ⟨h.1, h.2.1, h.2.2.1, h.2.2.2⟩, fun h => ⟨h.typed, h.linear, h.coverL, h.coverR⟩⟩

/-! ## Conformance fixtures, replayed

The clause tables of the repository's conformance suite, desugared by hand to
core patterns, with the verdict the Rust checker gives each. These are sanity
checks that the definitions above are neither vacuous nor over-strict; they
are not a mechanical link to the Rust implementation. -/

namespace Fixtures

/-- `O` of `type Bit = O | I`. -/
abbrev O : Pat := .inl .unit
/-- `I` of `type Bit = O | I`. -/
abbrev I : Pat := .inr .unit
/-- `Nothing` of `type MaybeBit = Nothing | Just Bit` (`1 + (1 + 1)`). -/
abbrev nothing : Pat := .inl .unit
/-- `Just p` of `type MaybeBit = Nothing | Just Bit`. -/
abbrev just (p : Pat) : Pat := .inr p
/-- `MaybeBit`, lowered. -/
abbrev maybeBit : Ty := .sum .unit bit

/-- P0001 `swapMaybe : MaybeBit <-> MaybeBit`. -/
def swapMaybe : ClauseSet :=
  [(nothing, nothing), (just O, just I), (just I, just O)]

/-- P0004 `cnot : (Bit, Bit) <-> (Bit, Bit)`. -/
def cnot : ClauseSet :=
  [(.pair O (.var "b"), .pair O (.var "b")), (.pair I O, .pair I I), (.pair I I, .pair I O)]

/-- D0001 `bad`: `Just I` is uncovered on both sides (T0301). -/
def nonexhaustive : ClauseSet := [(nothing, nothing), (just O, just O)]

/-- D0002 `dup`: `Just O` is covered twice on both sides (T0302). -/
def overlap : ClauseSet :=
  [(just O, just O), (just (.var "b"), just (.var "b")), (nothing, nothing)]

/-- D0003 `diag`: `b` used twice on a side (T0201). -/
def nonlinear : ClauseSet := [(.pair (.var "b") (.var "b"), .pair (.var "b") (.var "b"))]

/-- D0004 `drop`: `a` is erased (T0202). -/
def erasure : ClauseSet := [(.pair (.var "a") (.var "b"), .var "b")]

/-- P0011 `spin : Bit <-> Bit`, one label `$go` of carrier `(Bit, (Bit, Bit))`. -/
def spin : LClauseSet :=
  [ (.plain (.var "b"),                         .label 0 (.pair (.var "b") (.pair O O))),
    (.label 0 (.pair (.var "b") (.pair O O)),   .label 0 (.pair (.var "b") (.pair O I))),
    (.label 0 (.pair (.var "b") (.pair O I)),   .label 0 (.pair (.var "b") (.pair I O))),
    (.label 0 (.pair (.var "b") (.pair I O)),   .label 0 (.pair (.var "b") (.pair I I))),
    (.label 0 (.pair (.var "b") (.pair I I)),   .plain (.var "b")) ]

/-- D0008 `stuck : Bit <-> Bit`, one label `$go` of carrier `Bit`:
    `$go I` is uncovered on the left (T0303), `I` on the right (T0301). -/
def stuck : LClauseSet :=
  [ (.plain (.var "b"), .label 0 (.var "b")), (.label 0 O, .plain O) ]

theorem swapMaybe_accepted : Accepted swapMaybe maybeBit maybeBit := by decide

theorem cnot_accepted : Accepted cnot (.prod bit bit) (.prod bit bit) := by decide

theorem nonexhaustive_rejected : ¬Accepted nonexhaustive maybeBit maybeBit := by decide

theorem overlap_rejected : ¬Accepted overlap maybeBit maybeBit := by decide

theorem nonlinear_rejected : ¬Accepted nonlinear (.prod bit bit) (.prod bit bit) := by decide

theorem erasure_rejected : ¬Accepted erasure (.prod bit bit) bit := by decide

theorem spin_accepted : LAccepted spin [.prod bit (.prod bit bit)] bit bit := by decide

theorem stuck_rejected : ¬LAccepted stuck [bit] bit bit := by decide

/-- `cnot` computes what it should: `(I, O) ↦ (I, I)`. -/
example : apply cnot (.pair (.inr .unit) (.inl .unit)) =
    some (.pair (.inr .unit) (.inr .unit)) := by decide

end Fixtures

end Theseus.Coverage
