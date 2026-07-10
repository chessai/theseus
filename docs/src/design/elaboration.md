# Elaboration (surface → Core Π)

Elaboration is the `theseus-check` pass that turns a **checked** surface iso
into a Core Π term (ADR-0003). It runs after the four static checks
(`spec/static-semantics.md`), so it may assume its input denotes a total
bijection (TH-4a, `theory/coverage-bijectivity.md`) and is a *total* function
on accepted isos. Its output is normative only **up to denotational equality**:
the canonicalizer (ADR-0014) is the equality decision procedure and owns the
canonical form, so elaboration needs only to be *correct and deterministic*,
not minimal. Scope: the plain fragment (no labels; `trace`/label elaboration
is deferred with the second M1 slice).

## Expression bodies

An `= expr` body (P0007–10, P0013) elaborates compositionally, one core
former per surface operator:

```text
⟦f ; g⟧ = ⟦f⟧ ; ⟦g⟧        ⟦f * g⟧ = ⟦f⟧ × ⟦g⟧        ⟦~f⟧ = sym ⟦f⟧
⟦f + g⟧ = ⟦f⟧ + ⟦g⟧        ⟦name⟧  = the core of the referenced iso
```

`where`-bound isos elaborate in the parent's scope before the body uses them.

## Clause bodies (E3)

A clause iso `f : A <-> B` with clauses `[(l₁,r₁), …, (lₙ,rₙ)]` elaborates to

```text
⟦f⟧  =  demux_L ; (perm₁ + … + permₙ) ; sym(demux_R)
```

- **`demux_L : A ↔ Env(l₁) + … + Env(lₙ)`** decomposes the input into the
  tagged sum of per-clause *environments* (an environment `Env(p)` is the
  product of the types of `p`'s variables). It is well-defined because the LHS
  column `{lᵢ}` partitions `A` — every value routes to exactly one clause's
  summand.
- **`demux_R : B ↔ Env(r₁) + … + Env(rₙ)`** is the *same* construction on the
  RHS column; `sym(demux_R)` recomposes `B` from the per-clause environments.
- **`permᵢ : Env(lᵢ) ↔ Env(rᵢ)`** reorders the variable bundle from
  LHS-order to RHS-order. Linearity gives `vars(lᵢ) = vars(rᵢ)` (same typed
  multiset), so `permᵢ` is a structural product permutation — the identity
  when the orders already agree.

Decomposing each side **independently** to per-clause cells is what avoids the
RHS-alignment trap: we never require the RHS partition to line up with the LHS
discriminator; the middle is only per-clause variable shuffles.

### The one recursive primitive: `demux`

`demux(T, [p₁, …, pₘ])` builds a core term `T ↔ Env(p₁) + … + Env(pₘ)` for a
pattern column that partitions `T` (guaranteed by coverage):

1. **Leaf.** If `m = 1` and `p₁` discriminates no sum (it is variables,
   tuples, and single-constructor wrappers covering all of `T`), emit
   `decompose(p₁) : T ↔ Env(p₁)` — a structural iso (`assocˣ`/`swapˣ`/`unite`)
   peeling `p₁`'s constructor–tuple skeleton down to its variable tuple.
2. **Split.** Otherwise pick the **leftmost–outermost position** at which the
   patterns use different constructors of a sum type (one exists: `m ≥ 2` over
   a finite type, non-overlapping and exhaustive, forces a discriminator).
   Reshape `T` with structural combinators (`assoc`, `swapˣ`, `distrib`) to
   bring that position to the head, exposing `T ≅ T₁ + … + T_k` for the sum's
   `k` constructors (`R : T ↔ T₁ + … + T_k`). Partition the column into groups
   `G₁…G_k` by the constructor used at that position; strip the constructor
   from each group's patterns. Recurse: `demux(Tⱼ, Gⱼ)`. Emit
   `R ; (demux(T₁,G₁) + … + demux(T_k,G_k))`, then reassociate the nested sum
   of environments into the flat clause-order `Env(p₁) + … + Env(pₘ)`.

**Determinism.** The leftmost–outermost discriminator rule and a fixed
constructor/field traversal order make `demux` a deterministic function of the
column, so `⟦f⟧` is byte-stable (INV-DET).

**Termination.** Each `Split` strips one constructor layer from every pattern
in a group; pattern size strictly decreases, and the finite type has finite
depth.

## Worked example — `cnot`

`| (O,b)<->(O,b) | (I,O)<->(I,I) | (I,I)<->(I,O)` over `(Bit,Bit)`.

`demux_L` splits on the first bit: the `O` group is `[b]` (leaf, `Env = Bit`);
the `I` group splits on the second bit into two unit environments, routing
`(I,O)→` clause 2, `(I,I)→` clause 3. `demux_R` does the same on the RHS,
where the `I` group routes `(I,I)→` clause 2, `(I,O)→` clause 3 — the *swapped*
routing. With all `permᵢ = id`, `⟦cnot⟧ = demux_L ; sym(demux_R)` sends
`(O,b)↦(O,b)`, `(I,O)↦(I,I)`, `(I,I)↦(I,O)` as required. The canonicalizer
reduces this to the idiomatic `distrib ; (id + not) ; factor` — the elaborator
need not find that form itself.

## Adjoint corollary

Because `demux_L`, `demux_R` are built identically from the two columns,
`⟦f⟧† = demux_R ; (perm₁† + … + permₙ†) ; sym(demux_L)`, which is exactly the
elaboration of the flipped clauses `[(rᵢ, lᵢ)]`. So the surface `~f` and the
right-to-left reading agree, matching `spec/static-semantics.md` soundness and
TH-4a's adjoint corollary — the syntactic adjoint is the semantic inverse with
no separate obligation.

## What this pass may assume, and what it owes

- **Assumes** its input passed names/typing/linearity/coverage — so it never
  sees an unbound variable, a type error, or a non-partitioning column, and
  needs no error paths (elaboration failures would be internal compiler
  errors, ADR-0002 boundary contract).
- **Owes** `eval ⟦f⟧ = F`, the bijection the clauses denote — validated by the
  oracle chain (interpreter vs. brute-force table, ADR-0008/0018) and, at the
  IR boundary, by canonicalizer round-trips.
