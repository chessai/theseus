# Elaboration (surface → Core Π)

Elaboration is the `theseus-check` pass that turns a **checked** surface iso
into a Core Π term (ADR-0003). It runs after the four static checks
(`spec/static-semantics.md`), so it may assume its input denotes a total
bijection (TH-4a, `theory/coverage-bijectivity.md`) and is a *total* function
on accepted isos. Its output is normative only **up to denotational equality**:
the canonicalizer (ADR-0014) is the equality decision procedure and owns the
canonical form, so elaboration needs only to be *correct and deterministic*,
not minimal. Scope: the plain fragment (E1-E3 below) plus **label elaboration
to `trace`** (E4), the M1 slice-2 addition. Guards remain out (v0.2).

## Expression bodies

An `= expr` body (P0007-10, P0013) elaborates compositionally, one core
former per surface operator:

```text
⟦f ; g⟧ = ⟦f⟧ ; ⟦g⟧        ⟦f * g⟧ = ⟦f⟧ × ⟦g⟧        ⟦~f⟧ = sym ⟦f⟧
⟦f + g⟧ = ⟦f⟧ + ⟦g⟧        ⟦name⟧  = the core of the referenced iso
```

`where`-bound isos elaborate in the parent's scope before the body uses them.

## Clause bodies (E3)

A clause iso `f : A <-> B` with clauses `[(l₁,r₁), ..., (lₙ,rₙ)]` elaborates to

```text
⟦f⟧  =  demux_L ; (perm₁ + ... + permₙ) ; sym(demux_R)
```

- **`demux_L : A ↔ Env(l₁) + ... + Env(lₙ)`** decomposes the input into the
  tagged sum of per-clause *environments* (an environment `Env(p)` is the
  product of the types of `p`'s variables). It is well-defined because the LHS
  column `{lᵢ}` partitions `A` -- every value routes to exactly one clause's
  summand.
- **`demux_R : B ↔ Env(r₁) + ... + Env(rₙ)`** is the *same* construction on the
  RHS column; `sym(demux_R)` recomposes `B` from the per-clause environments.
- **`permᵢ : Env(lᵢ) ↔ Env(rᵢ)`** reorders the variable bundle from
  LHS-order to RHS-order. Linearity gives `vars(lᵢ) = vars(rᵢ)` (same typed
  multiset), so `permᵢ` is a structural product permutation -- the identity
  when the orders already agree.

Decomposing each side **independently** to per-clause cells is what avoids the
RHS-alignment trap: we never require the RHS partition to line up with the LHS
discriminator; the middle is only per-clause variable shuffles.

### The one recursive primitive: `demux`

`demux(T, [p₁, ..., pₘ])` builds a core term `T ↔ Env(p₁) + ... + Env(pₘ)` for a
pattern column that partitions `T` (guaranteed by coverage):

1. **Leaf.** If `m = 1` and `p₁` discriminates no sum (it is variables,
   tuples, and single-constructor wrappers covering all of `T`), emit
   `decompose(p₁) : T ↔ Env(p₁)` -- a structural iso (`assocˣ`/`swapˣ`/`unite`)
   peeling `p₁`'s constructor-tuple skeleton down to its variable tuple.
2. **Split.** Otherwise pick the **leftmost-outermost position** at which the
   patterns use different constructors of a sum type (one exists: `m ≥ 2` over
   a finite type, non-overlapping and exhaustive, forces a discriminator).
   Reshape `T` with structural combinators (`assoc`, `swapˣ`, `distrib`) to
   bring that position to the head, exposing `T ≅ T₁ + ... + T_k` for the sum's
   `k` constructors (`R : T ↔ T₁ + ... + T_k`). Partition the column into groups
   `G₁...G_k` by the constructor used at that position; strip the constructor
   from each group's patterns. Recurse: `demux(Tⱼ, Gⱼ)`. Emit
   `R ; (demux(T₁,G₁) + ... + demux(T_k,G_k))`, then reassociate the nested sum
   of environments into the flat clause-order `Env(p₁) + ... + Env(pₘ)`.

**Determinism.** The leftmost-outermost discriminator rule and a fixed
constructor/field traversal order make `demux` a deterministic function of the
column, so `⟦f⟧` is byte-stable (INV-DET).

**Termination.** Each `Split` strips one constructor layer from every pattern
in a group; pattern size strictly decreases, and the finite type has finite
depth.

## Worked example -- `cnot`

`| (O,b)<->(O,b) | (I,O)<->(I,I) | (I,I)<->(I,O)` over `(Bit,Bit)`.

`demux_L` splits on the first bit: the `O` group is `[b]` (leaf, `Env = Bit`);
the `I` group splits on the second bit into two unit environments, routing
`(I,O)→` clause 2, `(I,I)→` clause 3. `demux_R` does the same on the RHS,
where the `I` group routes `(I,I)→` clause 2, `(I,O)→` clause 3 -- the *swapped*
routing. With all `permᵢ = id`, `⟦cnot⟧ = demux_L ; sym(demux_R)` sends
`(O,b)↦(O,b)`, `(I,O)↦(I,I)`, `(I,I)↦(I,O)` as required. The canonicalizer
reduces this to the idiomatic `distrib ; (id + not) ; factor` -- the elaborator
need not find that form itself.

## Labels (E4): elaboration to `trace`

A clause set with label patterns elaborates by **reducing to E3 over the
label-extended type, then wrapping in `trace`** -- no new demux machinery. The
soundness of this reduction is `theory/coverage-bijectivity-labels.md`
(TH-4a-label); this section is the *algorithm*.

### Label-type inference (produces the carrier `u`)

Each label `ℓ` (a `$name` pseudo-constructor) has a single **carrier type**
`U_ℓ`, inferred (or checked against an annotation, ADR-0017):

1. Collect every occurrence of `$ℓ` across both columns of the iso. A nullary
   `$ℓ` has carrier `1`; a `$ℓ p₁ ... p_k` has carrier the tuple of its
   arguments' types.
2. **Unify** those argument types across all occurrences of `$ℓ` (ordinary
   type unification over the rig fragment, monomorphic). Failure -- a label used
   at two incompatible shapes -- is a type error (`T0102` family), reported
   before elaboration. Success fixes one `U_ℓ` used identically on both columns
   (the "same-carrier" invariant TH-4a-label presupposes).
3. The iso's **label state space** is the sum `u := Σ_ℓ U_ℓ` in a fixed label
   order: **first occurrence under the canonical traversal** -- clauses in
   declaration order, the LHS pattern before the RHS, leftmost-outermost within
   a pattern (deterministic, INV-DET; this is the same traversal E3's `demux`
   uses). The extended matched types are `Â := u + A` (LHS) and `B̂ := u + B`
   (RHS), with the carrier in the **left** summand.

### The reduction

Read each clause side as an ordinary pattern over the extended type: a plain
pattern `p` is `inr p` (boundary summand); a label pattern `$ℓ p` is
`inl (inj_ℓ p)` (the `ℓ`-summand of `u`). This turns the label clause set into
an ordinary E3 clause set `Â ⇄ B̂`. Then:

```text
⟦f⟧  =  trace(u, ⟦extended clause set⟧_E3)
     =  trace(u, demux_L ; (perm₁ + ... + permₙ) ; sym(demux_R))
```

where `demux_L : Â ↔ Env(l₁)+...+Env(lₙ)` and `demux_R : B̂ ↔ Env(r₁)+...+Env(rₙ)`
are the **same** recursive `demux` of E3, now over `Â`/`B̂` -- the outermost
`u + A` tag is just the first discriminator `demux` splits on. The body
`Φ := demux_L ; (⊕ᵢ permᵢ) ; sym(demux_R) : (u + A) ↔ (u + B)` is exactly the
bijection TH-4a-label certifies and the `u + a ↔ u + b` shape `trace` consumes;
`trace` extracts `A ↔ B` by feeding the `inl`/`u` states back
(`spec/dynamic-semantics.md`).

**Determinism / termination** carry over from E3 unchanged (the label tag adds
one outer `Split`), plus label-order fixed by first occurrence. **WCET note:**
the compiler emits `≤ |u|` iterations (ADR-0007) with `|u| = Σ_ℓ |U_ℓ|`.

### Worked example -- `spin` (P0011)

`spin : Bit<->Bit`, one label `$go` inferred at carrier `(Bit,(Bit,Bit))`, so
`u = go·(Bit,(Bit,Bit))`, `|u| = 8`, `Â = B̂ = u + Bit`. The five clauses become
an E3 set over `u + Bit`: clause 1's LHS `b` is `inr b`, its RHS `$go(b,(O,O))`
is `inl(inj_go(b,(O,O)))`, etc. E3 elaborates this to `Φ : (u+Bit) ↔ (u+Bit)`;
`⟦spin⟧ = trace(u, Φ)`. Evaluating forward on `O` feeds `inr O` in, walks four
`inl` states (`(O,O)→(O,I)→(I,O)→(I,I)`), exits `inr O` -- five body
applications, inside the granted `|u|+1 = 9` fuel.

## Adjoint corollary

Because `demux_L`, `demux_R` are built identically from the two columns,
`⟦f⟧† = demux_R ; (perm₁† + ... + permₙ†) ; sym(demux_L)`, which is exactly the
elaboration of the flipped clauses `[(rᵢ, lᵢ)]`. So the surface `~f` and the
right-to-left reading agree, matching `spec/static-semantics.md` soundness and
TH-4a's adjoint corollary -- the syntactic adjoint is the semantic inverse with
no separate obligation. For a **label** iso this composes through `trace` via
the definitional law `(trace Φ)† = trace(Φ†)` (`design/core-pi.md`): `⟦f⟧† =
trace(u, Φ†)`, and `Φ†` is the E3 elaboration of the flipped extended clauses,
so `~f` on a label iso is again the right-to-left reading -- evaluated as the
backward loop of `spec/dynamic-semantics.md`.

## What this pass may assume, and what it owes

- **Assumes** its input passed names/typing/linearity/coverage (incl. the
  label-extended checks and label-type inference for E4) -- so it never sees an
  unbound variable, a type error, an unresolved label carrier, or a
  non-partitioning column, and needs no error paths (elaboration failures would
  be internal compiler errors, ADR-0002 boundary contract).
- **Owes** `eval ⟦f⟧ = F`, the bijection the clauses denote -- for E4, `F` is the
  boundary map `trace(u, Φ)` computes, i.e. `eval ⟦f⟧ = trace(Φ)` with `Φ` the
  extended-clause bijection. Validated by the same oracle chain (interpreter vs.
  brute-force table, ADR-0008/0018) and, at the IR boundary, by canonicalizer
  round-trips. This is the standing (tested, not proved) obligation
  TH-4a-label's discharge is modulo.
