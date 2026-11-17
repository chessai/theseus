# Coverage ⇔ bijectivity (TH-4a, plain fragment)

This note discharges the **semantic** half of the coverage-checker soundness
claim: that the four static checks of `spec/static-semantics.md` (names,
typing, linearity, coverage) are together *sufficient* for a clause set to
denote a total bijection on typed values, with the right-to-left reading as
its inverse. (They are *not* exactly the condition: the converse needs an
extra hypothesis, see "Soundness and completeness of the checker" below.) It
is the proof underwriting ADR-0004's "coverage is the invertibility, not a
style lint," and publication item #1 ("totality-for-free"). It is proved for
the **plain fragment**: finite types, no labels, no guards (v0.1 first M1
slice). The label-extended case reduces to this one
(`theory/coverage-bijectivity-labels.md`); the *encoding* half -- that the
bijection's mux/demux lowering is invertible under the padded bit encoding --
is a separate downstream obligation (TH-4b, ADR-0012, M2) and is **not**
claimed here.

## Mechanization status

**Lean-mechanized** in `theseus-proofs/TheseusProofs/Coverage.lean`
(`lake build`-checked; every theorem below is under the build-time axiom guard,
so it depends only on `propext`, `Classical.choice`, `Quot.sound` -- no `sorry`,
no `native_decide`).

| This note | Lean (`Theseus.Coverage`) |
|---|---|
| Setup: values, patterns, `σ_p`, `p[ρ]` | `Val`, `Pat`, `Pat.matchBind`, `Pat.instantiate`. `enumerate`, `Pat.matchesVal`, `apply` transcribe the checker's `enumerate_values`, `matches_value`, and the first-match loop of `compute_bijection` |
| (Typing), (Linearity), (Coverage) | `Clause.WellTyped`, `Clause.Linear`, `Covers`, bundled as `Accepted cs A B`. `Covers` is the checker's literal test ("exactly one clause matches each enumerated value"); `covers_iff` proves it equal to *exhaustive and pairwise non-overlapping*. `Clause.WellTyped` is the *specified* typing over core types (every sub-pattern at the type its position demands, every variable at one core type); `Clause.wellTyped_iff` gives it as a decision procedure. `typing.rs` checks the same condition against core types -- see "Typing gap, closed" below |
| L1 | `Pat.matchBind_instantiate`, `Pat.instantiate_matchBind`, `Pat.instantiate_matchBind_exact` |
| L2 | `Clause.fire_swap`, `Clause.fire_swap_iff` |
| TH-4a, `F` total | `apply_total` |
| TH-4a, `F` a bijection | `denote_bijective`, `toEquiv` |
| TH-4a, the adjoint is the inverse | `apply_adjoint`, `adjoint_apply`, `denote_adjoint_leftInverse`, `denote_adjoint_rightInverse` |
| First-match `F` = the order-free clause relation | `rel_iff_apply` |
| Converse (see below) | `coverage_iff_totalBijection`, `covers_of_totalBijection`, `completeness_needs_irredundant`, `firstMatch_bijection_not_covered` |

**The model.** The checker's *core* view: untyped values over the types
`0 | 1 | a + b | a × b`, and patterns `x | () | inl p | inr p | (p, q)`. A named
ADT is modelled as its sum-of-products desugaring (a constructor pattern is its
injection path applied to the tuple of its arguments). Two details follow the
Rust rather than this note: the pattern for a nullary constructor's empty
argument list does not inspect the value (as `match_bind_args` does not), so L1's
"match, then substitute" half carries a typing hypothesis; and the "substitute,
then match" half turns out to need no linearity.

**What the mechanization corrected.** The completeness claim under
"Consequences" was false as originally stated; it now reads as proved. See
"Soundness and completeness of the checker" below.

**Typing gap, closed: the shipped `typing.rs` was weaker than (Typing).** The
theorem assumes the specified typing. An adversarial review of the
mechanization found two tables that `theseus-cli check` accepted (`ok`)
although they violate it and are not bijections. The Rust matcher, linearity
and coverage agreed with the Lean model on both; only typing differed.
`typing.rs` now types every sub-pattern against the *core* type of its
position, as `Clause.WellTyped` does, and rejects both with `T0102`.

1. A constructor of a named type was admitted directly at an anonymous sum
   type, while the matcher uses the constructor's path inside its own type:

   ```
   iso h : Bit + Bit <-> Bit
   | O <-> O
   | I <-> I
   ```

   sent four values onto two (Lean: `Fixtures.sumCtor_untyped_not_injective`).
   *Fix:* a constructor is well-typed only at its own named type (diagnostic
   `0011`); the only pattern typed at `S + T` is a variable. The alternative
   -- keep admitting it and make the matchers prefix the sum's injection --
   needs a rule for which summand `O` selects at `Bit + Bit`, and changes both
   matchers and the Lean model, to support a form no accepted program used.
2. A variable's occurrences were compared by *lowered surface type*, which for
   a recursive type identifies positions at different unroll depths:

   ```
   iso f : Nat * Nat <-> Nat * Nat
   | (Zero, b)   <-> (b, Zero)
   | (Succ a, b) <-> (a, Succ b)
   ```

   sent `(1, 3)` outside the depth-4 truncation (Lean:
   `Fixtures.muEscape_untyped_escapes`). *Fix:* the typing walk carries the
   per-path unroll budget that `Env::lower` spends (one shared implementation,
   `Env::unroll`), so a variable's occurrences are compared by the core type
   at each (diagnostic `0012`). `Succ n <-> Succ n` stays accepted: `n` is one
   unrolling deep on both sides. The same walk rejects a constructor at a
   position past the μ-cutoff, where the core type is `0` (diagnostic `0013`).
   That is a third table of this family, found while making the fix: a pattern
   for the numeral `4` at depth-4 `Nat` matches no value, so coverage cannot
   see it, yet as a right-hand side it sends `3` to `4`. (The fixture is the
   smallest instance: `Succ Zero` at `Nat@1`.)

So within the modelled fragment the shipped checker now enforces the theorem's
hypothesis, and the two Lean fixtures describe tables it rejects. Two limits
on that statement. The agreement of `typing.rs` with `Clause.WellTyped` is by
inspection and tests, like the rest of the transcription (next paragraph). And
numeric literal patterns are outside the model, so the theorem says nothing
about them. They had the same hole as the μ-cutoff: the matchers read a
literal as "the `k`-th value of the position's type", so an out-of-range
literal matches nothing, and
`iso g : U1 <-> U1 | 2 <-> 0 | 0 <-> 1 | 1 <-> 2` passed `check` although
clause 3 has no image. `typing.rs` now admits a literal `k` only at a builtin
`U<n>` with `k < 2^n` (diagnostic `0014`), which is what the spec's "every
numeric literal within its stdlib type" asks for; that this suffices for
literal tables is argued by the analogy with the cutoff, not proved.

**Not mechanized.** Nothing links the Lean model to the Rust source -- the
correspondence is by inspection, backed by replaying conformance fixtures
P0001, P0004, D0001-D0004 in Lean (`Coverage.Fixtures`). Machine-word and
numeric types (`U<n>`, `W<n>`, core `Flat`) and `Nat` literal patterns are not
modelled, nor is the surface layer (name and constructor resolution, μ-type
unrolling). The elaboration obligation `eval ⟦f⟧ = F` and TH-4b are untouched.

## Setup

Fix finite types `A`, `B` in the rig fragment. An iso is a clause list
`C = [(l₁,r₁), ..., (lₙ,rₙ)]` in declaration order, where each `lᵢ` is a
pattern over `A` and each `rᵢ` a pattern over `B`. A **pattern** is built
from constructors, tuples, and pattern *variables*; it is **linear** when no
variable occurs more than once in it. Write `vars(p)` for the set of
variables of `p`.

Assume `C` has passed the static checks:

- **(Typing)** each `lᵢ` is a well-typed pattern of `A`, each `rᵢ` of `B`;
- **(Linearity)** each `lᵢ` and each `rᵢ` is linear, and `vars(lᵢ) = vars(rᵢ)`
  (call it `Vᵢ`);
- **(Coverage)** `{lᵢ}` is exhaustive and pairwise non-overlapping over `A`,
  and `{rᵢ}` is exhaustive and pairwise non-overlapping over `B`.

For a pattern `p` over `T` write `⟦p⟧ = { t ∈ T : t matches p } ⊆ T`. For
`t ∈ ⟦p⟧`, matching yields an environment `σ_p(t) : vars(p) → Values` binding
each variable to the sub-value at its position. Conversely, for an
environment `ρ` typing the variables of `p`, `p[ρ] ∈ T` is the value obtained
by substitution. Let `Env(p)` be the set of well-typed environments of
`vars(p)` (a finite product of the variables' types).

## Lemma L1 (a linear pattern is a bijection to its environment space)

For a linear, well-typed pattern `p` over `T`, the maps
`σ_p : ⟦p⟧ → Env(p)` and `(·)[·] : Env(p) → ⟦p⟧, ρ ↦ p[ρ]` are mutually
inverse. Hence `σ_p` is a bijection `⟦p⟧ ≅ Env(p)`.

*Proof.* Structural induction on `p`.
- `p = x` (variable): `⟦x⟧ = T`, `Env(x) = T`, `σ_x(t) = {x ↦ t}`,
  `x[ρ] = ρ(x)`; mutually inverse.
- `p = C p₁ ... p_k` (constructor, `C` of arity `k`): a value matches iff it is
  `C v₁ ... v_k` with `vⱼ ∈ ⟦pⱼ⟧`. Constructors are injective and their images
  are disjoint (distinct constructors, distinct data), so matching decomposes
  uniquely into the `vⱼ`. Linearity makes `vars(p)` the *disjoint* union of
  the `vars(pⱼ)`, so `Env(p) = ∏ⱼ Env(pⱼ)`. By the IH each
  `σ_{pⱼ} : ⟦pⱼ⟧ ≅ Env(pⱼ)`; the product of bijections is a bijection, and it
  is exactly `σ_p`, with inverse the componentwise substitution `p[ρ]`.
- Tuples are the binary-product constructor and are the `k=2` case above.

Linearity is essential: it is what makes `vars(p)` a *disjoint* union across
sub-patterns, so the environment space factors as a product and the
substitution is single-valued. A repeated variable (a nonlinear pattern,
rejected by check T0201) would force an equality between two positions,
`σ_p` would not be onto `Env(p)`, and `p[ρ]` would be ill-defined for
environments disagreeing on the repeated variable. ∎

## Lemma L2 (each clause is a cell-bijection)

Under (Typing) and (Linearity), each clause defines a bijection
`φᵢ : ⟦lᵢ⟧ ≅ ⟦rᵢ⟧` by `φᵢ(a) = rᵢ[σ_{lᵢ}(a)]`.

*Proof.* `vars(lᵢ) = vars(rᵢ) = Vᵢ`, so `Env(lᵢ) = Env(rᵢ) =: Env(Vᵢ)`. By
L1, `σ_{lᵢ} : ⟦lᵢ⟧ ≅ Env(Vᵢ)` and `(·)[·] : Env(Vᵢ) ≅ ⟦rᵢ⟧`. Then
`φᵢ = (·)[·] ∘ σ_{lᵢ}` is a composite of bijections, hence a bijection, with
inverse `φᵢ⁻¹(b) = lᵢ[σ_{rᵢ}(b)]`. ∎

(The ground case `Vᵢ = ∅` is included: `⟦lᵢ⟧` and `⟦rᵢ⟧` are singletons and
`φᵢ` is the unique map between them.)

## Theorem TH-4a (coverage + linearity ⇒ total bijection)

Define `F : A → B` by `F(a) = φᵢ(a)`, where `i` is the unique index with
`a ∈ ⟦lᵢ⟧`. Then `F` is a bijection, and the map `G : B → A` defined
symmetrically from the RHS partition and the `φᵢ⁻¹` is its inverse. Moreover
`G` is exactly what the clause list read right-to-left, `[(r₁,l₁), ...]`,
denotes -- so the syntactic adjoint is semantically the inverse.

*Proof.* `F` is well-defined and total: by (Coverage), `{⟦lᵢ⟧}` partitions
`A`, so every `a` lies in exactly one `⟦lᵢ⟧` and `φᵢ(a) ∈ ⟦rᵢ⟧ ⊆ B` is
determined.

*Injective.* Suppose `F(a) = F(a') = b`, with `a ∈ ⟦lᵢ⟧`, `a' ∈ ⟦l_j⟧`. Then
`b ∈ ⟦rᵢ⟧ ∩ ⟦r_j⟧`; by (Coverage) the RHS cells are pairwise disjoint, so
`i = j`, and by L2 `φᵢ` is injective, so `a = a'`.

*Surjective.* Take any `b ∈ B`. By (Coverage) the RHS cells partition `B`, so
`b ∈ ⟦rᵢ⟧` for a unique `i`; let `a = φᵢ⁻¹(b) ∈ ⟦lᵢ⟧`. Then `a` lies in no
other LHS cell (LHS partition), so `F(a) = φᵢ(a) = b`.

Hence `F` is a bijection. `G` built from the RHS partition sends
`b ↦ φᵢ⁻¹(b)` for the unique `i` with `b ∈ ⟦rᵢ⟧`; `G(F(a)) = a` and
`F(G(b)) = b` clausewise, so `G = F⁻¹`. Finally, `G` is generated by the
clause list `[(rᵢ, lᵢ)]` under the very same construction, so the syntactic
adjoint denotes `F⁻¹`. ∎

## Consequences and scope

- **Soundness and completeness of the checker.** A clause set accepted by
  `spec/static-semantics.md` denotes a total bijection `A ≅ B` (soundness,
  TH-4a). The converse needs one more hypothesis than this note first
  claimed. For a well-typed linear clause set, *both columns pass coverage*
  **iff** *the clause relation is a total bijection `A ≅ B` and the set is
  irredundant* -- no pair `(a, b)` is produced by two different clauses
  (Lean: `coverage_iff_totalBijection`). Irredundancy cannot be dropped:

  ```
  iso f : Bit <-> Bit
  | x <-> x
  | O <-> O
  ```

  is well-typed and linear, and denotes the identity under both the
  first-match and the order-free reading, yet is rejected with T0302 (`O` is
  matched by clauses 1 and 2; Lean: `completeness_needs_irredundant`). So
  "each check negated exhibits a value with 0 or ≥2 images" holds only when
  images are counted *per clause*: an uncovered value has no image (a real
  non-bijection), but an overlap may be two clauses agreeing. That iff is
  about the *order-free* clause relation ("some clause sends `a` to `b`").
  For the first-match function it fails even with irredundancy: in

  ```
  iso s : Bit <-> Bit
  | O <-> I
  | I <-> O
  | x <-> x
  ```

  the last clause is shadowed, first-match dispatch is negation -- a total
  bijection with the right-to-left table as inverse -- and the table is
  rejected (T0302; Lean: `firstMatch_bijection_not_covered`). Its order-free
  relation is not single-valued, which is what coverage detects. Coverage is
  therefore invertibility **plus** order-independence-by-construction: a
  rejected well-typed linear table is one whose order-free relation is not a
  bijection, or one with a redundant clause instance. Soundness -- the
  direction ADR-0004 leans on -- is unaffected.
- **The two columns are independent.** The proof uses the LHS partition and
  the RHS partition separately; neither implies the other. This is why the
  checker enumerates both columns (a variable pattern can make one side
  exhaustive while the other is not -- e.g. `| a <-> Just a : Bit <-> MaybeBit`
  is right-non-exhaustive only), matching diagnostic `0001` and `0008`.
- **Adjoint corollary** ties to the Core Π adjoint (`design/core-pi.md`): the
  right-to-left reading is the semantic inverse, so `~` on a checked iso needs
  no separate proof obligation.
- **Not proved here.** (i) The **label-extended** case (matched type is the
  label state space) is `theory/coverage-bijectivity-labels.md` (TH-4a-label),
  a reduction to this note, also Lean-mechanized. (ii) **TH-4b**, that the padded bit
  encoding of `F` has invertible mux/demux (ADR-0012); that is a lowering
  obligation for M2 and remains sketched.
- **Test backstop.** Per ADR-0004/0018 the shipped algorithm is still
  differentially tested against the brute-force enumeration oracle on
  generated bijection-table programs; this proof says what that test is
  entitled to conclude.
