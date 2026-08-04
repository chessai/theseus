# Reversibility by construction (TH-REV)

**Claim.** theseus's iso combinators are the semantic core: every combinator
denotes a *bijection* of finite value types, the syntactic adjoint `inv` really
inverts (both directions), and the adjoint obeys the algebraic laws
(`inv (inv i) = i`, `inv (i ; j) = inv j ; inv i`). Reversibility is a
**theorem about the combinators**, derived by structural induction from a single
assumed primitive bijection -- it is NOT stipulated by the `Iso` type. The Tier-2
constructs (finite iteration, a fold/unfold catamorphism/anamorphism pair, and
the SWAPBR come-from involution) inherit reversibility from the same core.

**Mechanization.** Machine-checked in Lean 4 + Mathlib at
`theseus-proofs/TheseusProofs/Reversibility.lean` (`lake build`-checked, axiom
base = Mathlib base `{propext, Classical.choice, Quot.sound}`). Each theorem
below names its exact Lean counterpart. The NL proofs here MATCH the mechanized
statements verbatim; where a construct is a deliberate abstraction of the full
theseus semantics, the **Modeling scope** section says precisely what is
abstracted and why the abstraction is faithful to the reversibility claim.

## Why this is a theorem and not a definition

If reversibility were baked into the type -- `denote : Iso a b -> (den a =~= den b)`
returning an `Equiv` -- the property would be true by fiat and prove nothing. The
design here deliberately gives `denote` a **raw-function** type,
`denote : Iso a b -> denoteTy a -> denoteTy b`, a plain Lean function. Being a
bijection is then a proposition one must *establish*. The only place a bijection
is assumed is the leaf constructor `prim`, which is handed an `Equiv`
(`denoteTy a =~= denoteTy b`); every structural former derives its bijectivity
from its children's. This is the finite, closed-combinator analogue of the
theseus iso algebra (ADR-0007 / ADR-0034): reversible by construction, where "by
construction" is discharged as a proof rather than asserted by the type.

## Semantics (normative for this file)

### The finite object language of types

`Ty` is the closed grammar of value types the iso algebra ranges over:

```
Ty ::= unit | bit | prod Ty Ty | sum Ty Ty
```

Its denotation into honest Lean types is

```
denoteTy unit       = Unit
denoteTy bit        = Bool
denoteTy (prod a b) = denoteTy a  x  denoteTy b
denoteTy (sum  a b) = denoteTy a (+) denoteTy b
```

Every `denoteTy t` is a **finite** type with **decidable equality** (mechanized
`Fintype`/`DecidableEq`/`Inhabited` instances by recursion on `t`). Finiteness is
what makes "bijective" a non-vacuous, checkable property on these carriers.

### The combinators

`Iso : Ty -> Ty -> Type 1` has one constructor per structural former plus the two
non-structural leaves (`prim`, `iter`):

```
id    : Iso a a
seq   : Iso a b -> Iso b c -> Iso a c
times : Iso a b -> Iso c d -> Iso (prod a c) (prod b d)
plus  : Iso a b -> Iso c d -> Iso (sum  a c) (sum  b d)
swapT : Iso (prod a b) (prod b a)
swapS : Iso (sum  a b) (sum  b a)
assocT : Iso (prod (prod a b) c) (prod a (prod b c))   -- (associator, TH-REV strengthening)
assocS : Iso (sum  (sum  a b) c) (sum  a (sum  b c))   -- (associator, TH-REV strengthening)
unitTl : Iso (prod unit a) a                           -- (left unitor, TH-REV strengthening)
distrib : Iso (prod a (sum b c)) (sum (prod a b) (prod a c))  -- (distributor, TH-REV strengthening)
notI   : Iso (sum unit unit) (sum unit unit)           -- (SwapSum(1,1) = NOT, TH-REV strengthening)
inv   : Iso a b -> Iso b a
prim  : (denoteTy a =~= denoteTy b) -> Iso a b
iter  : Nat -> Iso a a -> Iso a a
```

### The denotation (raw function)

```
denote id          = fun x => x
denote (seq i j)   = denote j o denote i
denote (times i j) = Prod.map (denote i) (denote j)
denote (plus  i j) = Sum.map  (denote i) (denote j)
denote swapT       = Prod.swap
denote swapS       = Sum.swap
denote assocT      = (Equiv.prodAssoc _ _ _)         -- ((a,b),c) |-> (a,(b,c))
denote assocS      = (Equiv.sumAssoc  _ _ _)         -- inl(inl a),inl(inr b),inr c |-> ...
denote unitTl      = fun (u,a) => a                  -- ((),a) |-> a
denote distrib     = (Equiv.prodSumDistrib _ _ _)    -- (a, inl b) |-> inl(a,b); (a, inr c) |-> inr(a,c)
denote notI        = Sum.swap                        -- inl () <-> inr ()
denote (inv i)     = Function.invFun (denote i)
denote (prim e)    = e
denote (iter n i)  = (denote i)^[n]
```

`denote (inv i)` is the set-theoretic inverse `Function.invFun (denote i)`,
extracted using the ambient nonempty/finite structure. It is a plain function;
that it genuinely inverts `denote i` is the theorem `denote_inv` below, not a
definitional fact.

---

## TIER 1 -- reversible by construction

### Theorem (core bijectivity) -- `denote_bijective`

**Statement.** For every `i : Iso a b`, `Function.Bijective (denote i)`.

*Proof (structural induction on `i`).* One case per constructor; only `prim`
consumes an assumed bijection.

* **`id`.** `denote id = fun x => x = id`, and the identity is a bijection
  (`Function.bijective_id`).
* **`seq i j`.** `denote (seq i j) = denote j o denote i`. By the induction
  hypotheses `denote i` and `denote j` are bijections; the composite of two
  bijections is a bijection (`Bijective.comp`).
* **`times i j`.** `denote (times i j) = Prod.map (denote i) (denote j)`. The
  product map of two bijections is a bijection on the product
  (`Bijective.prodMap`): it is injective coordinatewise and surjective
  coordinatewise.
* **`plus i j`.** `denote (plus i j) = Sum.map (denote i) (denote j)`. The sum
  map of two bijections is a bijection on the disjoint union: injectivity from
  `Injective.sumMap` (tags are preserved, each branch injective) and surjectivity
  from `Surjective.sumMap` (each summand is hit).
* **`swapT`.** `denote swapT = Prod.swap`, the underlying function of the
  equivalence `Equiv.prodComm`, hence bijective.
* **`swapS`.** `denote swapS = Sum.swap`, the underlying function of
  `Equiv.sumComm`, hence bijective.
* **`assocT`.** `denote assocT` is the underlying function of `Equiv.prodAssoc`,
  the product associator, hence bijective (its symm is the reverse reassociation).
* **`assocS`.** `denote assocS` is the underlying function of `Equiv.sumAssoc`,
  the sum associator, hence bijective.
* **`unitTl`.** `denote unitTl` is the underlying function of the left unitor
  `Equiv.punitProd`/`(uniqueProd ..).` -- pairing with the unique inhabitant of
  `Unit` is an equivalence -- hence bijective.
* **`distrib`.** `denote distrib` is the underlying function of
  `Equiv.prodSumDistrib`, the distributor of product over sum, hence bijective
  (its symm is `factor`).
* **`notI`.** `denote notI = Sum.swap` on `Unit (+) Unit`, the underlying function
  of `Equiv.sumComm Unit Unit`, hence bijective. (This is `SwapSum(1,1)`, the NOT
  gate: it exchanges the two one-point summands.)
* **`inv i`.** `denote (inv i) = Function.invFun (denote i)`. Let `hi` be the IH
  that `denote i` is bijective. The set-theoretic inverse of a bijection is a
  bijection: it is **injective** because `denote i` is its left inverse
  (`Function.rightInverse_invFun hi.surjective` gives a right-inverse of `invFun`
  whose existence forces `invFun` injective), and **surjective** because
  `denote i` is injective (`Function.invFun_surjective hi.injective`).
* **`prim e`.** `denote (prim e) = e`, and an `Equiv` is bijective by definition
  (`Equiv.bijective`). This is the ONLY leaf that assumes a bijection.
* **`iter n i`.** `denote (iter n i) = (denote i)^[n]`. The `n`-fold iterate of a
  bijection is a bijection (`Bijective.iterate`), by induction on `n`: `^[0] = id`
  is bijective and `^[n+1] = f o f^[n]` composes two bijections.

Every case reduces to the induction hypotheses plus a Mathlib closure lemma; the
induction is well-founded on the structure of `i`. ∎

**`toEquiv`.** From `denote_bijective` we package a *theorem-carrying* `Equiv`
`toEquiv i := Equiv.ofBijective (denote i) (denote_bijective i)`, with
`toEquiv i x = denote i x` definitionally (`toEquiv_apply`). This is the derived
equivalence, built from the raw function and its proven bijectivity -- not from
any datatype assumption.

### Theorem (the adjoint inverts, two-sided) -- `denote_inv`, `..._leftInverse`, `..._rightInverse`

**Statement (`denote_inv`).** For every `i : Iso a b`,
`denote (inv i) = (toEquiv i).symm`.

*Proof.* Fix `y : denoteTy b`. Since `denote i` is a bijection (`denote_bijective`),
`Function.rightInverse_invFun` gives `denote i (denote (inv i) y) = y`; that is,
`denote (inv i) y` is *a* preimage of `y` under `denote i`. The `Equiv` inverse
`(toEquiv i).symm y` is *the* preimage, satisfying
`denote i ((toEquiv i).symm y) = y` (`apply_symm_apply`). Apply the injective map
`denote i` to both candidate preimages: both map to `y`, so by injectivity
`denote (inv i) y = (toEquiv i).symm y`. As `y` was arbitrary, the functions are
equal. ∎

**Corollary (`denote_inv_leftInverse`).** `LeftInverse (denote (inv i)) (denote i)`
-- i.e. `denote (inv i) (denote i x) = x` for all `x`. *Proof.* Rewrite by
`denote_inv` to `(toEquiv i).symm (denote i x) = x`, which is
`(toEquiv i).symm ((toEquiv i) x) = x`, the equivalence's `symm_apply_apply`. ∎

**Corollary (`denote_inv_rightInverse`).**
`RightInverse (denote (inv i)) (denote i)` -- i.e.
`denote i (denote (inv i) y) = y`. *Proof.* Rewrite by `denote_inv` to
`(toEquiv i) ((toEquiv i).symm y) = y`, the equivalence's `apply_symm_apply`. ∎

Together these are exactly the reversibility guarantee: post- and pre-composing an
iso with its adjoint yields the identity in both directions.

### Theorem (adjoint involution) -- `inv_inv`

**Statement.** `denote (inv (inv i)) = denote i`.

*Proof.* Rewrite the outer adjoint by `denote_inv (inv i)`:
`denote (inv (inv i)) = (toEquiv (inv i)).symm`. Now `toEquiv (inv i)` is the
equivalence whose forward map is `denote (inv i) = (toEquiv i).symm` (by
`denote_inv i`); its `symm` is therefore `(toEquiv i).symm.symm = toEquiv i`,
whose forward map is `denote i`. Pointwise, `simp` discharges
`((toEquiv i).symm).symm x = (toEquiv i) x`. Hence
`denote (inv (inv i)) = denote i`. ∎

### Theorem (adjoint contravariance) -- `seq_inv`

**Statement.** `denote (inv (seq i j)) = denote (seq (inv j) (inv i))`.

*Proof.* Both sides are inverses of the single bijection `denote (seq i j)`; an
inverse of a bijection is unique, so it suffices to exhibit both as inverses of
that map and appeal to `LeftInverse.eq_rightInverse`.

* The left side is a left inverse of `denote (seq i j)`: this is
  `denote_inv_leftInverse (seq i j)`.
* The right side is a right inverse of `denote (seq i j)`. Unfolding
  `denote (seq p q) = denote q o denote p` on both composites, the claim is
  `denote (inv i) (denote (inv j) (denote j (denote i x))) = x`. Peel from the
  inside: `denote (inv j) (denote j (denote i x)) = denote i x` by
  `denote_inv_leftInverse j`, and then
  `denote (inv i) (denote i x) = x` by `denote_inv_leftInverse i`.

A left inverse and a right inverse of the same map coincide, giving the equality
of denotations. This is the standard contravariance `(i ; j)^-1 = j^-1 ; i^-1`. ∎

---

## TIER 2 -- loops and branches

### Theorem (loop unrolling and zero) -- `iter_succ`, `iter_zero`

**Statement (`iter_succ`).**
`denote (iter (n+1) f) = denote (iter n f) o denote f`.

*Proof.* `denote (iter (n+1) f) = (denote f)^[n+1]`. By `Function.iterate_succ`,
`f^[n+1] = f^[n] o f`, i.e. run the body once, then `n` more times. Reading back
through `denote (iter n f) = (denote f)^[n]` gives the stated composite. ∎

**Statement (`iter_zero`).** `denote (iter 0 f) = id`.

*Proof.* `denote (iter 0 f) = (denote f)^[0] = id` definitionally (zero iterations
do nothing). ∎

### Theorem (every finite loop is reversible) -- `iter_bijective`

**Statement.** For all `n` and `f : Iso a a`, `Bijective (denote (iter n f))`.

*Proof.* Immediate corollary of `denote_bijective` specialized to the `iter`
constructor (whose case is the `Bijective.iterate` argument above). Surfaced as a
named Tier-2 fact because "a bounded reversible loop is reversible" is the loop
form of the reversibility guarantee. ∎

### Theorem (fold/unfold mutual inverse)

theseus has two fold-shaped constructs over a recursive carrier; we mechanize
both, and are explicit about which is ADR-0037's actual `fold`.

**(a) Cellwise map (the accumulator-free special case) -- `fold_unfold_inv`,
`unfold_fold_inv`, `foldBits_bijective`.** Given a per-cell bijection
`e : Bool =~= Bool`, over `List Bool` of arbitrary length:

```
foldBits   e = List.map e
unfoldBits e = List.map e.symm
```

`unfoldBits e (foldBits e l) = List.map e.symm (List.map e l) =
List.map (e.symm o e) l` by `List.map_map`, and `e.symm o e = id`
(`Equiv.symm_comp_self`), so `= List.map id l = l`; the reverse is symmetric with
`Equiv.self_comp_symm`. Hence `foldBits e` is a bijection with `unfoldBits e` its
two-sided inverse. **Here reversibility is genuinely a per-cell fact and traversal
order is irrelevant -- which is exactly what makes this only the DEGENERATE case
of ADR-0037's fold, not the real construct.**

**(b) The genuine ADR-0037 accumulator fold -- `unfold_fold_acc`,
`fold_unfold_acc`, `foldAcc_bijective`.** ADR-0037's `fold` is a reversible
`mapAccumL`: a body bijection `f : Bool x Bool =~= Bool x Bool` threads an
accumulator (the first component) head-to-tail along the list, so the cells are
COUPLED through the accumulator and traversal order matters. Over `List Bool`:

```
foldAcc   f acc []        = (acc, [])
foldAcc   f acc (b :: bs) = let (acc',b') = f (acc,b)
                            let (accf,bs') = foldAcc f acc' bs
                            (accf, b' :: bs')
unfoldAcc f acc []          = (acc, [])
unfoldAcc f acc (b' :: bs') = let (acc1,bs) = unfoldAcc f acc bs'
                              let (acc0,b)  = f.symm (acc1, b')
                              (acc0, b :: bs)
```

`foldAcc` runs `f` head-to-tail; the adjoint `unfoldAcc` runs `f.symm`
tail-to-head -- it recurses into the tail FIRST and inverts the head cell LAST,
walking fold's accumulator trajectory in reverse.

**Statement (`unfold_fold_acc`).** For every `l` and `acc`,
`unfoldAcc f (foldAcc f acc l).1 (foldAcc f acc l).2 = (acc, l)`.

*Proof.* Induction on `l` (general in `acc`). Empty: both sides `(acc, [])`. Cons
`b :: bs`: put `(acc',b') = f (acc,b)`. Forward, `foldAcc f acc (b::bs)` returns
final accumulator `accf` and list `b' :: bs'`, where `(accf,bs') = foldAcc f acc'
bs`. Backward, `unfoldAcc` on `(accf, b' :: bs')` recurses on `bs'` from `accf`
first; the induction hypothesis at `acc'` gives `unfoldAcc f accf bs' = (acc',
bs)`, so the head cell inverts as `f.symm (acc', b') = (acc, b)`
(`Equiv.symm_apply_apply`, since `(acc',b') = f (acc,b)`), yielding `(acc, b ::
bs)`. ∎

**Statement (`fold_unfold_acc`).** The other direction,
`foldAcc f (unfoldAcc f acc l).1 (unfoldAcc f acc l).2 = (acc, l)`, by the mirror
induction using `Equiv.apply_symm_apply`.

**Statement (`foldAcc_bijective`).** `Bijective (fun p => foldAcc f p.1 p.2)` on
`Bool x List Bool`, with `fun p => unfoldAcc f p.1 p.2` the two-sided inverse
(`Function.bijective_iff_has_inverse`). This is the real theorem: an
accumulator-threading reversible fold over an arbitrary-length structure whose
adjoint reverses the traversal -- not a cellwise permutation dressed up as a
fold. ∎

### Theorem (SWAPBR come-from involution) -- `comefrom_involution`, `swapbrIso_involution`

**Model.** ADR-0034 (D1/D6): the sole control-flow instruction is `SWAPBR rd`,
which SWAPS the branch register `BR` with general register `rd` -- an EXCH-shaped
rail-pair exchange (D1: "8 rail-pair exchanges gated on the opcode predicate, same
shape as EXCH"). The come-from discipline (D5) pairs a `SWAPBR rj` at the jump
site with a matching `SWAPBR rj` at the target; applying the *same* exchange twice
returns the machine to its pre-swap register contents. We model the acted-on slice
of machine State as the pair `(BR, rd)` and

```
swapbr : Bool x Bool -> Bool x Bool := Prod.swap
```

which is exactly the `Iso.swapT` transposition (opcode-shaped like EXCH). The tie
back to the verified algebra is `swapbr_eq_denote_swapT : swapbr = denote swapbrIso`
where `swapbrIso := Iso.swapT` on `prod bit bit`.

**Statement (`comefrom_involution`).** `swapbr o swapbr = id`.

*Proof.* For `(b, r) : Bool x Bool`,
`swapbr (swapbr (b, r)) = swapbr (r, b) = (b, r)`. A transposition composed with
itself is the identity. This is the algebraic core of the control-flow
reversibility witness: the come-from `SWAPBR rj` at the target undoes the `SWAPBR
rj` at the site, restoring the paired registers. (That sequential execution then
resumes at the target additionally needs the in-flight instructions to leave
`(BR, rj)` fixed -- an unmodeled static property of D5; see Modeling scope.) ∎

**Statement (`swapbrIso_involution`).**
`denote swapbrIso o denote swapbrIso = id`.

*Proof.* Rewrite `denote swapbrIso` to `swapbr` via `swapbr_eq_denote_swapT`
(they are definitionally equal, `swapbrIso = swapT` and `denote swapT = Prod.swap
= swapbr`), then apply `comefrom_involution`. This states the involution through
the *iso denotation*, tying the come-from witness to the reversible iso algebra
rather than to a bespoke `Prod.swap` fact. ∎

### Theorem (NOT is an involution) -- `notI_involution`

**Statement.** `denote notI o denote notI = id` (with behavioral pins
`denote notI (inl ()) = inr ()` and `denote notI (inr ()) = inl ()`).

*Proof.* `denote notI = Sum.swap` on `unit (+) unit`; swapping the two one-point
summands twice returns the original tag (`cases` on the two inhabitants, each by
`rfl`). NOT = `SwapSum(1,1)` is thus its own inverse -- the sum-side companion to
the product-side `comefrom_involution`, both instances of a transposition being
self-inverse. ∎

---

## Modeling scope (what is faithful vs. what is abstracted)

The point of this file is an honest boundary between the machine-checked algebraic
content and the deliberate abstractions of the concrete theseus machine. Nothing
below is a hidden hypothesis; each item is either exact or an explicitly-scoped
model.

* **Core algebra (`denote_bijective` and the adjoint laws) -- faithful.** `Ty`,
  `denoteTy`, and the structural combinators are an exact finite model of the
  reversible-rig fragment of the theseus iso algebra: unit, bit, product (tensor),
  sum (coproduct), the symmetries, associators, left unitor, the distributor, NOT
  (`SwapSum(1,1)`), sequential composition, the two functorial actions, and the
  syntactic adjoint. Bijectivity is DERIVED for every structural former from a
  single assumed primitive `Equiv`, which is precisely the design claim
  "reversible by construction, discharged as a theorem." The abstraction from the
  full theseus grammar is only *breadth* (the modeled formers are a genuine
  sub-algebra, not a reshuffle); nothing modeled is unfaithful. Not modeled:
  fixed-point/recursive `mu`-types (the object language is the closed
  unit/bit/prod/sum grammar) and negative/fractional types (out of the reversible
  fragment); these are out of scope for the reversibility-by-construction claim and
  are tracked elsewhere (TH-3 rig coherence, ADR-0014).

* **`fold`/`unfold` -- the genuine accumulator fold is mechanized; two honest
  boundaries.** ADR-0037's `fold` is a reversible `mapAccumL` whose reversibility
  is NOT per-cell: the accumulator threaded head-to-tail couples the cells, and the
  adjoint must reverse the traversal (tail-to-head). This threading -- the actual
  content of the ADR -- IS mechanized: `foldAcc`/`unfoldAcc` over `List Bool` of
  arbitrary length, with `unfold_fold_acc`/`fold_unfold_acc` proving mutual
  inversion at every length and `foldAcc_bijective` the resulting bijection. The
  recursion is real structural recursion, and the tail-to-head un-threading is the
  genuine reversal, not a reshuffle. Two boundaries remain, both explicit:
  (i) **cell type.** The per-cell body is `Bool x Bool =~= Bool x Bool` (accumulator
  and payload each one bit) rather than an arbitrary `Iso (prod a c) (prod a c)`; a
  fully `Iso`-typed recursive carrier would need recursive `mu`-types in `Ty`
  (scoped OUT, as for the object language generally). The accumulator-threading
  argument is uniform in the body bijection, so the one-bit case is representative
  of the reversibility mechanism, but it is a representative case, not the fully
  general `Iso` statement. (ii) The separate `foldBits`/`unfoldBits` map is the
  accumulator-FREE special case (`List.map e`); it is honestly labelled as the
  degenerate, per-cell case and is NOT claimed to be ADR-0037's fold.

* **`iter` -- faithful as bounded iteration; loop control is TH-1's job; the count
  is a meta-parameter.** `iter n` is the honest `n`-fold iterate of a body endo-iso.
  Two things it abstracts: (a) *how many times the body runs* -- the concrete
  `trace` loop derives its count from a data-dependent fixed point whose
  finiteness/exactness (`<= |u|`) is the separate, already-mechanized TH-1
  (`finite-trace-totality.md` / `FiniteTrace.lean`); and (b) the count's
  representation -- ADR-0036's denotation carries the count in the value as
  `<k, x> |-> <k, f^k x>`, whereas `Iso.iter n` takes `n` as an external Lean
  parameter. The data-carried version is a trivial fibrewise lift of
  `iter_bijective` (bijective in `x` for each fixed `k`), so nothing is lost for the
  reversibility claim; `iter` proves "any bounded iteration of a reversible body is
  reversible" and does not re-prove TH-1.

* **`swapbr` / come-from -- faithful on the acted-on slice, abstracting the
  unchanged registers.** ADR-0034's `SWAPBR rd` exchanges `BR` with `rd` and leaves
  all other registers fixed; the come-from involution is entirely a property of the
  `(BR, rd)` pair, since the identity on the untouched registers trivially squares
  to the identity. We model exactly that slice as `Bool x Bool` and `swapbr` as its
  transposition, and we tie it to the verified algebra via `swapbrIso = Iso.swapT`
  so the involution is a consequence of the *same* `denote`/`swapT` semantics, not
  a standalone `Prod.swap` lemma. The abstraction (dropping the untouched
  registers) is faithful because they are acted on by the identity, which commutes
  with and is unaffected by the exchange; including them would change `Bool x Bool`
  to `Bool x Bool x (rest)` with `swapbr = Prod.swap x id` and leave the involution
  identical. Not modeled, two items: (i) the opcode-predicate gating (whether a
  given step *is* a SWAPBR) -- control-flow *dispatch*, orthogonal to the
  reversibility *of* the exchange; and (ii) the *in-flight invariance* -- that the
  instructions executed between the paired site/target SWAPBRs leave `(BR, rj)`
  fixed. `comefrom_involution` composes the two exchanges with nothing in between,
  so it is the algebraic core (the exchange is self-inverse); the claim that
  sequential execution actually resumes at the target additionally needs that
  in-flight invariance, which is a separate static-discipline property of D5, not
  proved here.

## Provenance (borrowed vs. novel)

* **Borrowed (cited, not re-proved here).** The iso combinator and adjoint laws
  are Pi (Sabry et al.) over a symmetric rig category (Mac Lane; Laplaza coherence,
  ledger TH-3); the associators, unitors, symmetries, and distributor are the
  structure morphisms of that category. The finite-set closure facts
  (`Bijective.comp`, `.prodMap`, `.sumMap`, `.iterate`, `Function.invFun` and its
  inverse laws, `Equiv.ofBijective`, `List.map_map`) are Mathlib. The categorical
  reading of reversible iteration is Kaarsgaard et al. (join inverse categories,
  ADR-0005).
* **Novel here.** The explicit *mechanized* discharge that reversibility is a
  structural-induction THEOREM over a raw-function denotation (bijectivity NOT
  baked into the type), including the syntactic adjoint's two-sided inversion,
  involution, and contravariance as derived facts; and the come-from control-flow
  reversibility cast as `swapT`'s self-inverse through the *same* verified
  denotation. The abstractions above are stated as scoped models, not as
  independent theorems.

## Axiom base

All theorems in `Reversibility.lean` reduce to Mathlib's base axioms
`{propext, Classical.choice, Quot.sound}` only -- no `sorry`, `admit`, `axiom`, or
`native_decide` (verified by `#print axioms`; kernel `decide` where used).
`Classical.choice` enters through `Function.invFun` (used to denote `inv`) and
through Mathlib's `Fintype`/`Equiv` infrastructure; this is the standard classical
base and matches the TH-1 mechanization.
