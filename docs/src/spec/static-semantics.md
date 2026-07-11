# Static semantics

An `iso` declaration is well-formed iff it passes four checks. This chapter
is normative and implementation-silent (the *algorithm* — two-column
usefulness checking plus a syntactic linearity pass — is ADR-0004; its
soundness is `theory/coverage-bijectivity.md`, TH-4a). Scope: the plain
fragment (finite types, no labels, no guards — the first M1 slice). Labels
extend the matched type to the iso's label state space; that extension is
deferred (§Labels).

Let an iso `f : A <-> B` be given by clauses `| lᵢ <-> rᵢ`, `i = 1..n` in
declaration order, where each `lᵢ` is a pattern over `A` and each `rᵢ` a
pattern over `B`.

## The four checks, in order

The checks are **staged**, and staging is **global**: an implementation MUST
evaluate them in the order **Names → Typing → Linearity → Coverage**, and once
any stage reports at least one diagnostic it MUST NOT report diagnostics from
a later stage. *Within* a stage every applicable diagnostic MUST be reported
(comparison is set-based, `spec/test-spec-m1.md`). Global staging is why a
nonlinear clause reports only its linearity error and no coverage error
(diagnostic `0003`), and a type error suppresses coverage (diagnostic `0005`).

### 1. Names — `T0101`

Every constructor named in a pattern MUST be a constructor of a declared
type, and every iso name referenced in an `= expr` body MUST resolve —
*including through `~`*, so `~missing` is a name error (diagnostic `0010`). An
unresolved name is `T0101` with `name:` the offending identifier, and `side:`
when it occurs within a clause (diagnostic `0006`).

### 2. Typing — `T0102`

In each clause, `lᵢ` MUST be a well-typed pattern of the domain `A` and `rᵢ` a
well-typed pattern of the codomain `B`: every constructor applied at its
declared arity, every sub-pattern at the type its position demands, every
numeric literal within its stdlib type (§Types). A pattern whose type is
incompatible with its position — e.g. a `MaybeBit` constructor where the type
is `Bit` (diagnostic `0005`) — is `T0102` with the offending `side:` and
1-indexed `clauses:`. (Names catches *unknown* constructors; Typing catches
*known but mis-typed* ones.)

### 3. Linearity — `T0201`, `T0202`

For each clause, over the variables of `lᵢ` and of `rᵢ`:

- A variable occurring **more than once on one side** (within `lᵢ`, or within
  `rᵢ`) is `T0201` — `side:` that side, `variable:` the name (diagnostic
  `0003`).
- A variable occurring **on one side but not the other** is `T0202`, reported
  on the side where it occurs — this is erasure, the flagship reversibility
  error (diagnostic `0004`).

Together these are ADR-0004's linearity condition — *each variable exactly
once per side, and present on both sides*. Duplication (`T0201`) is erasure's
(`T0202`) mirror twin; both break invertibility (`theory/coverage-bijectivity`
Lemma L1/L2).

### 4. Coverage — `T0301`, `T0302`

Treat the LHS patterns `{lᵢ}` and the RHS patterns `{rᵢ}` as **two
independent pattern columns**, over `A` and over `B` respectively. On **each
side independently**:

- **Exhaustive.** If some value of the type matches no pattern in the column,
  that is `T0301` with `side:` the column and `witness:` an unmatched value
  (diagnostic `0001`). The two columns are checked independently — a gap on
  one side does not entail a gap on the other; a variable pattern can make one
  side exhaustive while the other is not (`| a <-> Just a` is right-non-
  exhaustive only). An implementation MUST report at least one `T0301` per
  uncovered side; it MAY report more than one witness.
- **Non-overlapping.** If some value matches two clauses `lᵢ`, `lⱼ` (`i < j`)
  in a column, that is `T0302` with `side:` the column, `witness:` such a
  value, and `clauses:` the 1-indexed pair (diagnostic `0002`). An
  implementation MUST report the overlap; the reported witness compares as a
  value (`spec/test-spec-m1.md`).

Coverage is over the plain declared type. When labels return, the matched
type is the label state space (§Labels); the T0301/T0303 split for uncovered
label vs. plain witnesses is specified there.

## Soundness

A clause set passing all four checks denotes a **total bijection** `A ≅ B`,
whose inverse is the same clauses read right-to-left (`| rᵢ <-> lᵢ`). This is
TH-4a (`theory/coverage-bijectivity.md`), proved for the plain fragment. A
conforming implementation MUST elaborate the checked iso to a core term
denoting this bijection; the elaboration *algorithm* is `design/elaboration.md`
and is normative only up to denotational equality (the canonicalizer decides
`≅`, ADR-0014). The
checks are also complete: a rejected clause set fails a necessary condition
for bijectivity, so there are no false rejections. The shipped algorithm is
differentially tested against the brute-force enumeration oracle on generated
bijection-table programs (ADR-0018); TH-4a is what that test is entitled to
conclude.

## Labels (M1 slice-2)

Label patterns (`$name …`) extend the matched type to the iso's label state
space `U + A` (LHS) / `U + B` (RHS), with `U = Σ_ℓ U_ℓ` the sum of label
carriers, and add `T0303` (label state space not covered).

**Label-type inference.** Each label `ℓ` has one **carrier** `U_ℓ`, obtained by
unifying the argument types of every `$ℓ` occurrence across both columns
(monomorphic rig-fragment unification; a nullary `$ℓ` has carrier `1`, a
`$ℓ p₁ … p_k` the argument tuple). An optional annotation is *checked* against
that inference. Failure — one label used at two incompatible shapes — is a
typing error (reported as `T0102` with `side:` the offending occurrence's column
and `witness:` the label name), so the "same carrier on both columns" invariant
holds by the time coverage runs. The carrier order in `U` is **first occurrence
under the canonical traversal** (clauses in declaration order, LHS before RHS,
leftmost–outermost within a pattern — deterministic). Full algorithm:
`design/elaboration.md` §E4.

**The checks.** The label-extended (typing, linearity, coverage) are exactly
the plain checks over the extended type, with the coverage exhaustiveness
obligation split by top tag — `T0301` files the uncovered **boundary** (`A`/`B`)
witnesses, `T0303` the uncovered **label-state** witnesses; non-overlap
(`T0302`) and linearity are unchanged. This is normatively
`theory/coverage-bijectivity-labels.md` (TH-4a-label), which also discharges
TH-1's bijection hypothesis for the elaborated `trace`.

**Elaboration and evaluation.** A checked label iso elaborates to `trace`
(`design/elaboration.md` §E4, ADR-0009) and evaluates by the fuel-counted trace
loop (`spec/dynamic-semantics.md`), one fuel unit per label step, terminating
within `|U|+1` applications. Implementations MUST report each label loop's
static iteration bound `≤ |U|` as a WCET note (ADR-0007).

Guards are not part of v0.1 (ADR-0017 records the recomputability condition any
future guard extension MUST satisfy).
