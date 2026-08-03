# Label-extended coverage ⇔ bijectivity (TH-4a-label)

This note discharges the **label-extended** case that
`theory/coverage-bijectivity.md` (plain TH-4a) explicitly defers ("reduces to
this one once label elaboration is pinned"). It is the OWED hypothesis of
`theory/finite-trace-totality.md` (TH-1): TH-1 assumes the body of a `trace`
denotes a *total bijection* `f : u + a ↔ u + b`; TH-4a-label is what certifies
that hypothesis for a `trace` produced by elaborating a label loop. Together,
TH-4a-label + TH-1 give: **a label iso passing the static checks denotes a
total bijection `a ↔ b`, terminating within `|u| + 1` body applications.**

The whole content is a **reduction**: a label clause set *is* an ordinary
clause set over an extended finite type, and the label-extended checks *are*
the plain TH-4a checks over that type. So plain TH-4a applies verbatim; the
label machinery adds no new bijectivity obligation. What is genuinely new is
(a) the framing that makes the reduction exact, and (b) the observation that
the `T0301`/`T0303` code split is a *reporting* partition of one condition
(coverage over the extended type), not two independent obligations.

Scope: the finite fragment (finite label carriers, no guards), matching
ADR-0009's v0.1 core (Π + `trace`) and ADR-0009's ruling that "fold clause
sets reuse the ordinary coverage machinery." The `trace`-over-infinite-`u`
(v0.2 marked-partial) case is out of scope -- there termination is not free and
TH-1 does not apply.

## Setup: the label state space is a finite rig type

Fix a label iso `f : A <-> B` in the finite dialect. Its clauses are
`[(l₁,r₁), ..., (lₙ,rₙ)]` in declaration order, where each side of a clause is
either a **plain** pattern over the boundary type, or a **label** pattern
`$ℓ p` for a label `ℓ` drawn from the iso's label set `L`. By label-type
inference/annotation (ADR-0017) each label `ℓ ∈ L` carries a single **finite**
type `U_ℓ`, used identically wherever `$ℓ` occurs on either column. Write

```
U  :=  Σ_{ℓ∈L} U_ℓ          (the label carrier -- a finite rig type)
Â  :=  U + A                 (extended LHS matched type)
B̂  :=  U + B                 (extended RHS matched type)
```

The label carrier `U` sits in the **left** summand deliberately, to match
`trace`'s `u + a ↔ u + b` convention (`design/core-pi.md`): the fed-back loop
state (`inl`, per `finite-trace-totality.md`'s loop) is exactly the label
state, and `trace` then extracts the boundary map `A ↔ B` with no reshuffle.

Both `Â`, `B̂` are finite types in the rig fragment (finite sums of finite
types). Read the two pattern *shapes* as ordinary constructor patterns into
these sums:

* a label pattern `$ℓ p` is `inl (inj_ℓ p) : Â` (the `ℓ`-summand of the carrier
  `U`) -- a nullary `$ℓ` has carrier `1`, a multi-argument `$ℓ p₁ ... p_k` has
  carrier the tuple `∏ⱼ U_{ℓ,j}`; read `p` as that argument bundle throughout;
* a plain pattern `p` over `A` is `inr p : Â` (the boundary summand);

and symmetrically over `B̂`. **The `$` sigil is the top-level constructor tag**
(and, by the grammar, the *only* place a label tag appears in a well-typed
clause -- see scope item 3).
Under this reading a clause `lᵢ <-> rᵢ` is exactly an ordinary clause of a
pattern over `Â` and a pattern over `B̂`, and the label clause set is an
ordinary clause set `Â ⇄ B̂` -- nothing more.

The **label-extended static checks** -- which `spec/static-semantics.md`
§Labels defers to this M1 slice; **this note supplies their definition**, and
§Labels now cites it normatively -- are, under the same reading:

* **(Typing)** each `lᵢ : Â`, each `rᵢ : B̂`, with label carriers as inferred.
* **(Linearity)** each side linear with `vars(lᵢ) = vars(rᵢ)` -- unchanged from
  plain; the `$ℓ` wrapper binds no variables.
* **(Coverage)** each column exhaustive and non-overlapping over its extended
  type, reported by *tag*:
  * `T0301` (`side`, plain `witness`): a **boundary** value (`inr`) matched by
    no pattern -- the `A`/`B` summand is uncovered.
  * `T0303` (`side`, label `witness` `$ℓ u`): a **label state** (`inl(inj_ℓ u)`)
    matched by no pattern -- the carrier `U` summand is uncovered.
  * `T0302` (`side`, `witness`, `clauses`): two patterns in a column overlap.

## Lemma (tag split): the extended checks *are* the plain checks over `Â`, `B̂`

The top-level `U + A` tag is a discriminator. A boundary value `inr a`
(`a : A`) matches only plain patterns (a `$ℓ` pattern is `inl ...`, a different
tag); a label state `inl(inj_ℓ u)` matches only `$ℓ′` patterns with `ℓ′ = ℓ`
(distinct labels are distinct constructors of `U`, and no plain pattern reaches
the `inl` tag). Hence for the LHS column `{lᵢ}`:

```
{lᵢ} partitions Â   ⟺   ({plain lᵢ} partitions A)  ∧  (∀ℓ, {$ℓ-patterns} partition U_ℓ)
```

and likewise `{rᵢ}` over `B̂`. This is the ordinary fact that a pattern column
over a sum splits, per top constructor, into independent columns over the
summands (the leftmost-outermost discriminator of `design/elaboration.md`
applied at the outermost tag). Consequently:

* **Exhaustiveness** over `Â` = (boundary summand covered) ∧ (every label
  summand covered). The first conjunct's counter-witnesses are precisely the
  `T0301` witnesses; the second's are precisely the `T0303` witnesses. So
  `T0301 ∧ T0303` on a column is *literally* exhaustiveness over the extended
  type -- the two codes partition one obligation by tag, they are not two
  obligations.
* **Non-overlap** over `Â` = per-tag non-overlap: a plain and a label pattern
  can never overlap (different top tag), so the only overlaps are plain-plain
  (over `A`) or `$ℓ`-`$ℓ` (over `U_ℓ`); `T0302` catches exactly these. The
  cross-tag case is vacuous.
* **Linearity/typing** are pattern-local and identical to the plain checks;
  the `$ℓ` wrapper is a nullary-variable constructor.

Therefore the label-extended (Typing, Linearity, Coverage) on `[(lᵢ,rᵢ)]` are
**the same conditions** as plain TH-4a's (Typing, Linearity, Coverage) on the
ordinary clause set `[(lᵢ,rᵢ)]` viewed over `Â` (LHS column) and `B̂` (RHS
column). ∎

## Theorem TH-4a-label (extended coverage + linearity ⇒ extended bijection)

A label clause set passing the label-extended checks denotes a **total
bijection**

```
Φ : Â ≅ B̂          i.e.   Φ : U + A ≅ U + B
```

whose inverse is the same clauses read right-to-left `[(rᵢ, lᵢ)]`.

*Proof.* By the tag-split Lemma the hypotheses are exactly those of plain TH-4a
(`theory/coverage-bijectivity.md`) instantiated at the finite rig types
`Â` (LHS) and `B̂` (RHS). Apply plain TH-4a: `Φ` is a total bijection with the
right-to-left reading as inverse (the adjoint corollary transfers unchanged,
since `demux_L`/`demux_R` are built identically from the two columns over
`Â`/`B̂`). ∎

*Derived consequence (`|A| = |B|`).* Not an input: plain TH-4a's L2 makes each
clause a cell-bijection `|⟦lᵢ⟧| = |Env(Vᵢ)| = |⟦rᵢ⟧|`; summing over the
partition, `|Â| = |B̂|`, and since both share the `U` summand, `|A| = |B|`.
`trace` needs only the `u`-summands to match (typing enforces that); the
boundary sizes agreeing is a theorem, not a hypothesis.

## Corollary (discharge of TH-1's hypothesis)

Label elaboration sends `f` to `trace Φ`, where `Φ : (U+A) ↔ (U+B)` is the
plain E3 elaboration (`design/elaboration.md`) of the extended clause set --
`demux` over `Â`, per-clause variable perms, `sym(demux)` over `B̂` -- with the
`$ℓ` tag elaborated as its sum injection (ADR-0009: fold/label clause sets
reuse the ordinary coverage *and* elaboration machinery). Because every `$ℓ`
carries the same `U_ℓ` on both columns (Setup; a typing invariant of a
well-formed label iso), the two carriers coincide as one `U`, so
`Φ : u + a ↔ u + b` with `u := U`, `a := A`, `b := B` -- exactly `trace`'s
input, the label carrier already in the fed-back left summand. TH-4a-label
supplies the "total bijection" hypothesis; **TH-1** then gives:

> `trace Φ : A ↔ B` is a total bijection terminating within `|U| + 1` body
> applications (`|U|` loop iterations), where `|U| = Σ_ℓ |U_ℓ|`; forward and
> backward runs are mutually inverse.

The loop TH-1 runs re-matches the fed-back label state against the LHS column;
non-overlap makes the matching clause **unique regardless of declaration
order**, so the iteration computes exactly the surface `$ℓ`-threading (no
order-sensitivity sneaks in). This discharges the "**Label body -- currently
OWED**" clause of `finite-trace-totality.md` §Hypothesis-and-its-discharge, the
"label-extended case owed" note of `theory/coverage-bijectivity.md`, and the
ledger's TH-4a row -- **modulo the standing elaboration obligation** `eval Φ =`
the clause-set denotation, which is *tested* (oracle chain, ADR-0008/0018) not
proved (scope item 6). Note the mutually-inverse clause quoted above is
TH-1's *text-proved* half (the forward-termination core is Lean-mechanized; the
backward/mutual-inverse direction is text-only, per the ledger) -- the discharge
strictly needs only forward termination + bijectivity, both of which are firm.

## Worked check -- the two fixtures

**`stuck` (diagnostic 0008, a *rejection*).**
`stuck : Bit<->Bit | b <-> $go b | $go O <-> O`, label `$go` of carrier `Bit`,
so `Â = B̂ = go·Bit + Bit` (carrier summand first, per the `U + A` convention).
* LHS `{ b , $go O }`: plain `b` covers the `Bit` summand; `$go O` covers only
  `$go O`, missing `$go I` ⇒ `U`-summand uncovered ⇒ **`T0301` absent,
  `T0303 side:left witness:$go I`**.
* RHS `{ $go b , O }`: `$go b` covers all of `go·Bit`; `O` covers only `O`,
  missing `I` ⇒ boundary summand uncovered ⇒ **`T0301 side:right witness:I`**.

Both witnesses, both codes, both sides -- exactly the `.diag` sidecar. The
tag-split Lemma is the **specification of the witness filing**, validated
against 0008/0011 -- not a claim about the shipped implementation, which the
oracle chain (ADR-0008/0018) tests separately.

**`spin` (conformance P0011, an *acceptance*).**
`spin : Bit<->Bit`, `$go` of carrier `(Bit,(Bit,Bit))`, `|U| = 8`. The plain
`b` (clause 1 LHS, clause 5 RHS) covers each boundary summand; the four `$go`
patterns on each column cover all 8 label states, non-overlapping. So each
column partitions its extended type ⇒ `Φ : U+Bit ≅ U+Bit` a bijection ⇒
`trace Φ = spin` with `spin O = O`, `spin I = I`, terminating in ≤ `|U|+1 = 9`
applications (actual: 5). Matches the fixture's `<= 8` iteration bound and
`fuel >= 5`.

## What does *not* reduce (honest scope)

1. **Same-carrier coincidence is a typing input, not proved here.** `U+A ≅ U+B`
   sharing one `U` presupposes each label's two-column occurrences were
   assigned the *same* carrier by inference (ADR-0017). If inference gave a
   label different carriers on its two sides, `trace` would not typecheck;
   TH-4a-label presupposes a well-typed label iso and does not re-derive label
   inference soundness.
2. **The `$` sigil discriminator is load-bearing.** The tag split needs plain
   and label patterns to be statically distinguishable at the top; the grammar's
   `$` marker is what supplies the discriminator. A hypothetical dialect letting
   a bare variable range over label states would break the split (a variable
   would cover both tags) and this reduction with it.
3. **`$` occurs only at the clause root -- a typing fact, not a grammar fact.**
   The grammar (`spec/grammar.md`) *derives* `$` in nested positions (e.g.
   `$go ($stop b)`, `(b, $go c)`), so the "label patterns are top-level
   injections into `U + A`" reading is not syntactic. It holds because **no
   `ty` denotes a label state** -- label states live only in the iso's inferred
   state space, never in a declared `type` -- so typing can never demand a `$`
   at a nested (sub-`ty`) position; a nested `$` is a type error, rejected
   before this note applies. TH-4a-label presupposes a *well-typed* label iso
   and inherits this exclusion from label-type inference (ADR-0017), which is
   itself still only sketched.
4. **This note is de facto normative -- now adopted by citation.**
   `spec/static-semantics.md` §Labels formerly *deferred* the label check
   definitions and the `T0301`/`T0303` assignment to this slice; it now cites
   this note normatively for them. This note stays the authoritative
   definition and the spec section is a pointer to it (kept DRY to avoid
   drift), so there is no remaining spec debt -- only the standing convention
   that edits to the check semantics land here first.
5. **Bijectivity is not reachability.** T0303's "every label state eliminated
   on the left *and* produced on the right" is exactly the `U`-part of
   domain-totality and codomain-surjectivity of `Φ` -- no separate
   well-foundedness obligation. But a bijection on `U+A` does **not** by itself
   guarantee that iterating from a boundary input *reaches* a boundary output
   (it could cycle through label states forever on an infinite `U`). That the
   loop terminates is TH-1 (finiteness ⇒ termination), correctly kept as a
   *separate* theorem chained on top -- not folded into TH-4a-label.
6. **Elaboration correctness is inherited, not re-proved.** `eval ⟦f⟧ =
   trace Φ` with `eval Φ = ` the extended clause bijection is the plain
   elaboration obligation (`design/elaboration.md` "owes `eval⟦f⟧ = F`") over
   `Â`/`B̂`, validated by the same oracle chain (ADR-0008/0018); TH-4a-label
   adds no elaboration obligation, it only certifies the bijection that
   elaboration is trusted to preserve.

## Provenance (borrowed vs. novel)

* **Borrowed:** plain TH-4a (this repo, `coverage-bijectivity.md`) does all the
  bijectivity work; TH-1 (this repo) does all the termination work; the
  pattern-column-over-a-sum split is Maranget's pattern-matrix decomposition
  (cited in the plain note); ADR-0009 ratifies "clause sets reuse the ordinary
  coverage machinery."
* **Novel here:** the exact identification of the label-extended checks with
  the plain checks over `U + A` / `U + B` (so no new bijectivity proof is
  owed), and the reading of the `T0301`/`T0303` split as a by-tag *partition of
  one* exhaustiveness obligation -- which is simultaneously the specification of
  where the checker files each witness (validated against fixtures 0008/0011).
```
