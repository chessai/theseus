/-
Reversibility as a THEOREM, not a definitional shortcut.

theseus's isos are the semantic core: every combinator denotes a *bijection* of
finite value types, and the whole point of ADR-0007 / ADR-0034 is that this holds
by construction rather than by fiat. To make that a real theorem we give a typed
combinator language `Iso a b` a RAW-FUNCTION denotation `denote : Iso a b →
denoteTy a → denoteTy b` (a plain function, NOT an `Equiv`), and then PROVE the
denotation is bijective, that the syntactic adjoint `Iso.inv` genuinely inverts,
and the algebraic laws (`inv ∘ inv = id`, `inv (i ; j) = inv j ; inv i`).

If reversibility were baked into the type (`denote : Iso a b → (⋯ ≃ ⋯)`) the
theorem would be vacuous. Here `prim` is the ONLY place a bijection is assumed
(a primitive is given as an `Equiv`); every structural combinator's bijectivity
is DERIVED. This is the finite, closed-combinator analogue of the theseus iso
algebra.

Two tiers:

  * TIER 1 — reversible by construction: `denote_bijective` (over the full
    reversible-rig algebra — symmetries, associators, left unitor, distributor,
    and NOT = `SwapSum(1,1)`, each with DERIVED bijectivity), the adjoint
    `denote_inv` (inv really inverts, both directions), `inv_inv`, `seq_inv`.
  * TIER 2 — loops and branches: `iter` (n-fold endo-iteration) with `iter_succ`
    / `iter_bijective`; a `fold`/`unfold` inverse pair over a bounded vector
    (`fold_unfold_inv`, `unfold_fold_inv`); and the SWAPBR come-from back-jump as
    a self-inverse involution `swapbr` (`comefrom_involution`), the control-flow
    reversibility witness — faithful to ADR-0034's "SWAPBR rd swaps BR ↔ rd"
    (an EXCH-shaped exchange, whose own-inverse is the exchange again).
-/
import Mathlib

namespace Theseus.Reversibility

/-! ## The finite object language of types -/

/-- The finite object language of value types: `unit`, `bit`, products and sums.
    Closed under the two monoidal structures the iso algebra uses. -/
inductive Ty
  | unit
  | bit
  | prod (a b : Ty)
  | sum  (a b : Ty)
  deriving DecidableEq

open Ty

/-- Denotation of a `Ty` as an honest Lean `Type`: `unit ↦ Unit`, `bit ↦ Bool`,
    `prod ↦ ×`, `sum ↦ ⊕`. Every denoted type is a finite type with decidable
    equality (the two instances below), which is what makes "bijective" a
    checkable, non-vacuous property. -/
def denoteTy : Ty → Type
  | unit     => Unit
  | bit      => Bool
  | prod a b => denoteTy a × denoteTy b
  | sum  a b => denoteTy a ⊕ denoteTy b

instance : (t : Ty) → DecidableEq (denoteTy t)
  | unit     => inferInstanceAs (DecidableEq Unit)
  | bit      => inferInstanceAs (DecidableEq Bool)
  | prod a b =>
      letI := instDecidableEqDenoteTy a
      letI := instDecidableEqDenoteTy b
      inferInstanceAs (DecidableEq (denoteTy a × denoteTy b))
  | sum a b =>
      letI := instDecidableEqDenoteTy a
      letI := instDecidableEqDenoteTy b
      inferInstanceAs (DecidableEq (denoteTy a ⊕ denoteTy b))

instance : (t : Ty) → Inhabited (denoteTy t)
  | unit     => inferInstanceAs (Inhabited Unit)
  | bit      => inferInstanceAs (Inhabited Bool)
  | prod a b =>
      letI := instInhabitedDenoteTy a
      letI := instInhabitedDenoteTy b
      inferInstanceAs (Inhabited (denoteTy a × denoteTy b))
  | sum a _ =>
      letI := instInhabitedDenoteTy a
      -- `Inhabited (α ⊕ β)` is not a default instance (left/right ambiguity), so
      -- pick the left injection explicitly.
      ⟨Sum.inl default⟩

instance : (t : Ty) → Fintype (denoteTy t)
  | unit     => inferInstanceAs (Fintype Unit)
  | bit      => inferInstanceAs (Fintype Bool)
  | prod a b =>
      letI := instFintypeDenoteTy a
      letI := instFintypeDenoteTy b
      inferInstanceAs (Fintype (denoteTy a × denoteTy b))
  | sum a b =>
      letI := instFintypeDenoteTy a
      letI := instFintypeDenoteTy b
      inferInstanceAs (Fintype (denoteTy a ⊕ denoteTy b))

/-! ## Typed reversible combinators -/

/-- Typed reversible combinators. `prim` is the ONLY primitive-bijection input;
    every other constructor is a structural combinator whose reversibility is a
    theorem, not an assumption. `inv` is the syntactic adjoint.

    The structural algebra spans the reversible-rig fragment: the two symmetries
    (`swapT`/`swapS`), the two associators (`assocT`/`assocS`), the left unitor
    (`unitTl`), the distributor of product over sum (`distrib`), and NOT =
    `SwapSum(1,1)` (`notI`, exchanging the two one-point summands of
    `unit ⊕ unit`). Each is denoted by a Mathlib structure `Equiv` yet its
    bijectivity is still DERIVED in `denote_bijective` (the denotation is a raw
    function; the bijection is the theorem). -/
inductive Iso : Ty → Ty → Type 1
  | id    : Iso a a
  | seq   : Iso a b → Iso b c → Iso a c
  | times : Iso a b → Iso c d → Iso (prod a c) (prod b d)
  | plus  : Iso a b → Iso c d → Iso (sum a c) (sum b d)
  | swapT : Iso (prod a b) (prod b a)
  | swapS : Iso (sum a b) (sum b a)
  | assocT : Iso (prod (prod a b) c) (prod a (prod b c))
  | assocS : Iso (sum (sum a b) c) (sum a (sum b c))
  | unitTl : Iso (prod unit a) a
  | distrib : Iso (prod a (sum b c)) (sum (prod a b) (prod a c))
  | notI  : Iso (sum unit unit) (sum unit unit)
  | inv   : Iso a b → Iso b a
  | prim  : (denoteTy a ≃ denoteTy b) → Iso a b
  | iter  : Nat → Iso a a → Iso a a

/-- The semantics as a PLAIN function. `inv i` denotes the set-theoretic inverse
    of `denote i`, extracted via `Function.invFun` using the ambient nonempty /
    finite structure; Tier 1's `denote_inv` proves this really is the inverse.
    Note reversibility is NOT visible in this type — it is `denoteTy a →
    denoteTy b`, a plain function; being a bijection is the theorem below. -/
noncomputable def denote : {a b : Ty} → Iso a b → denoteTy a → denoteTy b
  | _, _, .id          => fun x => x
  | _, _, .seq i j     => denote j ∘ denote i
  | _, _, .times i j   => Prod.map (denote i) (denote j)
  | _, _, .plus i j    => Sum.map (denote i) (denote j)
  | _, _, .swapT       => Prod.swap
  | _, _, .swapS       => Sum.swap
  | _, _, .assocT      => Equiv.prodAssoc _ _ _
  | _, _, .assocS      => Equiv.sumAssoc _ _ _
  | _, _, .unitTl      => Equiv.uniqueProd _ Unit
  | _, _, .distrib     => Equiv.prodSumDistrib _ _ _
  | _, _, .notI        => Sum.swap
  | _, _, .inv i       => Function.invFun (denote i)
  | _, _, .prim e      => e
  | _, _, .iter n i    => (denote i)^[n]

/-! ## TIER 1 — reversible by construction

Every iso denotes a bijection, the syntactic adjoint really inverts, and the
adjoint obeys `inv ∘ inv = id` and `inv (i ; j) = inv j ; inv i`. -/

/-- TIER 1 core: the denotation of EVERY iso is a bijection of finite value
    types. Proved by structural induction — the only leaf that *assumes* a
    bijection is `prim` (its `Equiv`); `seq`/`times`/`plus`/`swaps`/`id`/`inv`
    all DERIVE it. So "reversible" is a theorem about the combinators, not a
    property stipulated by the `Iso` type. -/
theorem denote_bijective : ∀ {a b : Ty} (i : Iso a b), Function.Bijective (denote i)
  | _, _, .id        => Function.bijective_id
  | _, _, .seq i j   => (denote_bijective j).comp (denote_bijective i)
  | _, _, .times i j =>
      -- Prod.map of two bijections is a bijection.
      (denote_bijective i).prodMap (denote_bijective j)
  | _, _, .plus i j  =>
      -- Sum.map of two bijections is a bijection.
      have hi := denote_bijective i
      have hj := denote_bijective j
      ⟨hi.1.sumMap hj.1, hi.2.sumMap hj.2⟩
  | _, _, .swapT     => (Equiv.prodComm _ _).bijective
  | _, _, .swapS     => (Equiv.sumComm _ _).bijective
  | _, _, .assocT    =>
      -- product associator: reassociation is an Equiv, hence a bijection.
      (Equiv.prodAssoc _ _ _).bijective
  | _, _, .assocS    =>
      -- sum associator: reassociation is an Equiv, hence a bijection.
      (Equiv.sumAssoc _ _ _).bijective
  | _, _, .unitTl    =>
      -- left unitor: pairing with the unique inhabitant of `unit` is an Equiv.
      (Equiv.uniqueProd _ Unit).bijective
  | _, _, .distrib   =>
      -- distributor of product over sum: an Equiv, hence a bijection.
      (Equiv.prodSumDistrib _ _ _).bijective
  | _, _, .notI      =>
      -- NOT = SwapSum(1,1): swap of the two one-point summands, an Equiv.
      (Equiv.sumComm Unit Unit).bijective
  | _, _, .inv i     =>
      -- The set-theoretic inverse of a bijection is a bijection: it is injective
      -- (f is its left inverse) and surjective (from injectivity of f).
      have hi := denote_bijective i
      ⟨(Function.rightInverse_invFun hi.2).injective,
       Function.invFun_surjective hi.1⟩
  | _, _, .prim e    => e.bijective
  | _, _, .iter n i  =>
      -- n-fold iterate of a bijection is a bijection.
      (denote_bijective i).iterate n

/-- Package the derived bijection as an `Equiv` (data + the two-sided inverse
    laws). This is a THEOREM-carrying `Equiv`, built from the plain `denote` and
    its proven bijectivity — not the datatype's assumption. -/
noncomputable def toEquiv {a b : Ty} (i : Iso a b) : denoteTy a ≃ denoteTy b :=
  Equiv.ofBijective (denote i) (denote_bijective i)

@[simp] theorem toEquiv_apply {a b : Ty} (i : Iso a b) (x : denoteTy a) :
    toEquiv i x = denote i x := rfl

/-- The syntactic adjoint really is the inverse: `denote (inv i) = (toEquiv i).symm`.
    Since `denote (inv i) = Function.invFun (denote i)` and `denote i` is a
    bijection, its `invFun` coincides with the `Equiv` inverse. -/
theorem denote_inv {a b : Ty} (i : Iso a b) :
    denote (Iso.inv i) = (toEquiv i).symm := by
  ext y
  have hbij := denote_bijective i
  -- invFun of a bijection sends y to the unique preimage; so does the Equiv symm.
  have : denote i (denote (Iso.inv i) y) = y :=
    Function.rightInverse_invFun hbij.2 y
  -- apply the injective forward map to both sides against (toEquiv i).symm
  apply hbij.1
  rw [this]
  exact ((toEquiv i).apply_symm_apply y).symm

/-- `inv i` post-composed with `i` is the identity (adjoint left-inverts). -/
theorem denote_inv_leftInverse {a b : Ty} (i : Iso a b) :
    Function.LeftInverse (denote (Iso.inv i)) (denote i) := by
  rw [denote_inv]
  intro x
  -- goal: `(toEquiv i).symm (denote i x) = x`; `denote i x` is defeq `toEquiv i x`.
  exact (toEquiv i).symm_apply_apply x

/-- `inv i` pre-composed with `i` is the identity (adjoint right-inverts). -/
theorem denote_inv_rightInverse {a b : Ty} (i : Iso a b) :
    Function.RightInverse (denote (Iso.inv i)) (denote i) := by
  rw [denote_inv]
  intro y
  -- goal: `denote i ((toEquiv i).symm y) = y`; `denote i _` is defeq `toEquiv i _`.
  exact (toEquiv i).apply_symm_apply y

/-- Adjoint is an involution on denotations: `inv (inv i)` denotes the same
    function as `i`. -/
theorem inv_inv {a b : Ty} (i : Iso a b) :
    denote (Iso.inv (Iso.inv i)) = denote i := by
  -- Both `denote (inv (inv i))` and `denote i` invert `denote (inv i)`; a left
  -- inverse equals a right inverse, so they are equal.
  apply Function.LeftInverse.eq_rightInverse (f := denote (Iso.inv i))
  · exact denote_inv_leftInverse (Iso.inv i)
  · exact denote_inv_leftInverse i

/-- Adjoint of a composite reverses order: `inv (i ; j)` denotes the same as
    `inv j ; inv i`. This is the contravariance of the adjoint. -/
theorem seq_inv {a b c : Ty} (i : Iso a b) (j : Iso b c) :
    denote (Iso.inv (Iso.seq i j)) = denote (Iso.seq (Iso.inv j) (Iso.inv i)) := by
  -- Both sides are the inverse of denote (seq i j): the LHS is a left inverse and
  -- the RHS a right inverse of that map, and a left inverse equals a right inverse.
  apply Function.LeftInverse.eq_rightInverse (f := denote (Iso.seq i j))
  · -- denote (inv (i;j)) is a left inverse of denote (i;j)
    exact denote_inv_leftInverse (Iso.seq i j)
  · -- denote (inv j ; inv i) is a right inverse of denote (i;j):
    -- `RightInverse g₂ f` unfolds to `∀ x, f (g₂ x) = x`.
    intro x
    show denote (Iso.seq i j) (denote (Iso.seq (Iso.inv j) (Iso.inv i)) x) = x
    -- denote (seq p q) = denote q ∘ denote p
    show denote j (denote i (denote (Iso.inv i) (denote (Iso.inv j) x))) = x
    rw [denote_inv_rightInverse i (denote (Iso.inv j) x), denote_inv_rightInverse j x]

/-! ## TIER 2 — loops and branches

Endo-iteration (`iter`) is the finite loop; a `fold`/`unfold` pair models a
reversible catamorphism/anamorphism as mutual inverses; and `swapbr` models the
ADR-0034 come-from back-jump as a self-inverse involution. -/

/-- `iter (n+1) f` denotes one more application of `f` after `iter n f`:
    `denote (iter (n+1) f) = denote (iter n f) ∘ denote f`. This is the loop
    unrolling law — the semantic content of "run the body once more". -/
theorem iter_succ {a : Ty} (n : Nat) (f : Iso a a) :
    denote (Iso.iter (n + 1) f) = denote (Iso.iter n f) ∘ denote f := by
  show (denote f)^[n + 1] = (denote f)^[n] ∘ denote f
  rw [Function.iterate_succ]

/-- `iter 0 f` is the identity: zero loop iterations do nothing. -/
theorem iter_zero {a : Ty} (f : Iso a a) :
    denote (Iso.iter 0 f) = id := rfl

/-- Every finite iteration of a reversible body is reversible: `denote (iter n f)`
    is a bijection. (Corollary of `denote_bijective` at the `iter` constructor,
    surfaced as a named Tier-2 fact.) -/
theorem iter_bijective {a : Ty} (n : Nat) (f : Iso a a) :
    Function.Bijective (denote (Iso.iter n f)) :=
  denote_bijective (Iso.iter n f)

/-! ### fold / unfold as a mutually-inverse pair over a recursive structure

ADR-0037: `fold` applies a body iso structurally at every cell of a recursive
data type; `unfold` is its adjoint anamorphism. We model the recursive type as
`List Bool` (a BitList of arbitrary length) and `fold e` as the structural
application of a per-cell bijection `e : Bool ≃ Bool` to every element -- a
genuine recursion over the structure, not a fixed reshuffle. `unfold e` applies
`e.symm` at every cell. The theorems below prove the catamorphism and its
anamorphism are mutual inverses at EVERY length: building the structure with the
body and tearing it back down with the adjoint returns the input. -/

/-- `fold e`: apply the body bijection `e` to every cell of a BitList (structural
    recursion over the list) -- the reversible catamorphism. -/
def foldBits (e : Bool ≃ Bool) : List Bool → List Bool := List.map e

/-- `unfold e`: the adjoint anamorphism, applying `e.symm` at every cell. -/
def unfoldBits (e : Bool ≃ Bool) : List Bool → List Bool := List.map e.symm

/-- fold then unfold is the identity at every length: `unfold e ∘ fold e = id`.
    The anamorphism inverts the catamorphism, cell by cell. -/
theorem fold_unfold_inv (e : Bool ≃ Bool) :
    unfoldBits e ∘ foldBits e = id := by
  funext l
  simp only [Function.comp, foldBits, unfoldBits, List.map_map,
    Equiv.symm_comp_self, List.map_id, id_eq]

/-- unfold then fold is the identity too (the other direction). -/
theorem unfold_fold_inv (e : Bool ≃ Bool) :
    foldBits e ∘ unfoldBits e = id := by
  funext l
  simp only [Function.comp, foldBits, unfoldBits, List.map_map,
    Equiv.self_comp_symm, List.map_id, id_eq]

/-- Every fold of a reversible body is itself reversible: `foldBits e` is a
    bijection of BitLists, with `unfoldBits e` as a two-sided inverse. -/
theorem foldBits_bijective (e : Bool ≃ Bool) :
    Function.Bijective (foldBits e) := by
  have hL : Function.LeftInverse (unfoldBits e) (foldBits e) :=
    fun l => congrFun (fold_unfold_inv e) l
  have hR : Function.RightInverse (unfoldBits e) (foldBits e) :=
    fun l => congrFun (unfold_fold_inv e) l
  exact ⟨hL.injective, hR.surjective⟩

/-! ### SWAPBR come-from involution (ADR-0034 control-flow reversibility)

ADR-0034 D1/D6: the only control-flow instruction is `SWAPBR rd`, which SWAPS
the branch register `BR` with general register `rd` — an EXCH-shaped rail-pair
exchange (D1: "8 rail-pair exchanges gated on the opcode predicate, same shape as
EXCH"). The come-from discipline (D5) pairs a `SWAPBR rj` at the jump site with a
matching `SWAPBR rj` at the target: applying the SAME exchange twice returns the
machine to its pre-swap register contents. That self-inverse property is the
control-flow reversibility witness modeled here.

We model the relevant slice of State as the pair `(BR, rd)` and `swapbr` as the
exchange `(br, rd) ↦ (rd, br)` — exactly a `swapT`-shaped transposition. The
theorem `comefrom_involution` is `swapbr ∘ swapbr = id`: the come-from `SWAPBR`
undoes the site `SWAPBR`, which is why sequential execution resumes at the target
(D5: "BR ← 0 — sequential execution resumes"). -/

/-- The (BR, rd) slice of machine State that `SWAPBR rd` acts on. -/
abbrev BrState := Bool × Bool

/-- `SWAPBR rd` as its ADR-0034 meaning on the `(BR, rd)` slice: exchange the two
    registers. This is the `Iso.swapT`/`Prod.swap` transposition, opcode-shaped
    like EXCH. -/
def swapbr : BrState → BrState := Prod.swap

/-- The SWAPBR exchange is realized by a real theseus iso (`swapT` on `bit × bit`),
    so it is reversible by construction, not by a bespoke argument. -/
def swapbrIso : Iso (prod bit bit) (prod bit bit) := Iso.swapT

theorem swapbr_eq_denote_swapT : swapbr = denote swapbrIso := rfl

/-- CONTROL-FLOW REVERSIBILITY WITNESS (ADR-0034 D5 come-from): `SWAPBR` is its
    own inverse — `swapbr ∘ swapbr = id`. Applying the come-from `SWAPBR rj` at
    the jump target undoes the `SWAPBR rj` at the jump site, restoring the paired
    registers so sequential execution resumes. A transposition composed with
    itself is the identity. -/
theorem comefrom_involution : swapbr ∘ swapbr = id := by
  funext x
  simp [swapbr]

/-- The same involution stated through the iso denotation: the SWAPBR iso is a
    reversible involution — its denotation squared is the identity. This ties the
    come-from witness back to the verified iso algebra. -/
theorem swapbrIso_involution :
    denote swapbrIso ∘ denote swapbrIso = id := by
  rw [← swapbr_eq_denote_swapT]
  exact comefrom_involution

/-! ## Behavioral pins for the strengthened algebra

Small `by decide`/`rfl`-level checks that the new structural constructors denote
exactly the intended maps (guarding against a mis-wired `Equiv`). These are
kernel-checked, no `native_decide`. -/

/-- `notI` really is NOT on the two-point sum: it swaps the summands. -/
example : denote Iso.notI (Sum.inl ()) = Sum.inr () := rfl
example : denote Iso.notI (Sum.inr ()) = Sum.inl () := rfl

/-- `notI` is its own inverse (NOT is an involution). -/
theorem notI_involution :
    denote Iso.notI ∘ denote Iso.notI = id := by
  funext x; cases x <;> rfl

/-- `distrib` pushes a product into a sum on the left branch. -/
example {a b c : Ty} (x : denoteTy a) (y : denoteTy b) :
    denote (Iso.distrib (a := a) (b := b) (c := c)) (x, Sum.inl y) = Sum.inl (x, y) := rfl

/-- `distrib` pushes a product into a sum on the right branch. -/
example {a b c : Ty} (x : denoteTy a) (z : denoteTy c) :
    denote (Iso.distrib (a := a) (b := b) (c := c)) (x, Sum.inr z) = Sum.inr (x, z) := rfl

/-- `unitTl` drops the unit factor, returning the payload. -/
example {a : Ty} (x : denoteTy a) : denote (Iso.unitTl (a := a)) ((), x) = x := rfl

/-- `assocT` reassociates a nested product. -/
example {a b c : Ty} (x : denoteTy a) (y : denoteTy b) (z : denoteTy c) :
    denote (Iso.assocT (a := a) (b := b) (c := c)) ((x, y), z) = (x, (y, z)) := rfl

end Theseus.Reversibility
