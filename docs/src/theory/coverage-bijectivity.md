# Coverage ⇔ bijectivity (TH-4a, plain fragment)

This note discharges the **semantic** half of the coverage-checker soundness
claim: that the four static checks of `spec/static-semantics.md` (names,
typing, linearity, coverage) are together *exactly* the condition for a
clause set to denote a total bijection on typed values. It is the proof
underwriting ADR-0004's "coverage is the invertibility, not a style lint,"
and publication item #1 ("totality-for-free"). It is proved for the **plain
fragment**: finite types, no labels, no guards (v0.1 first M1 slice). The
label-extended case reduces to this one once label elaboration is specified
(deferred); the *encoding* half -- that the bijection's mux/demux lowering is
invertible under the padded bit encoding -- is a separate downstream
obligation (TH-4b, ADR-0012, M2) and is **not** claimed here.

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

- **Soundness of the checker.** A clause set accepted by
  `spec/static-semantics.md` denotes a total bijection `A ≅ B`; a rejected
  one fails one of the four necessary conditions (necessity: each check
  negated exhibits a value with 0 or ≥2 forward/backward images, i.e. a
  non-bijection -- so the checks are also *complete*, no false rejections).
  Coverage is therefore invertibility, exactly as ADR-0004 states, now with
  a proof rather than an assertion.
- **The two columns are independent.** The proof uses the LHS partition and
  the RHS partition separately; neither implies the other. This is why the
  checker enumerates both columns (a variable pattern can make one side
  exhaustive while the other is not -- e.g. `| a <-> Just a : Bit <-> MaybeBit`
  is right-non-exhaustive only), matching diagnostic `0001` and `0008`.
- **Adjoint corollary** ties to the Core Π adjoint (`design/core-pi.md`): the
  right-to-left reading is the semantic inverse, so `~` on a checked iso needs
  no separate proof obligation.
- **Not proved here.** (i) The **label-extended** case (matched type is the
  label state space); reduces to this note once label elaboration is pinned
  (deferred from the first M1 slice). (ii) **TH-4b**, that the padded bit
  encoding of `F` has invertible mux/demux (ADR-0012); that is a lowering
  obligation for M2 and remains sketched.
- **Test backstop.** Per ADR-0004/0018 the shipped algorithm is still
  differentially tested against the brute-force enumeration oracle on
  generated bijection-table programs; this proof says what that test is
  entitled to conclude.
