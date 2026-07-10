# Core Π

The v0.1 core (ADR-0003, ADR-0009) is a typed combinator language for
isomorphisms in the Theseus/Π lineage. Every well-typed core term denotes a
bijection between finite types; the adjoint is syntactic and free.

```text
types:  0 | 1 | a + b | a × b
isos:   id | sym f | f ; g | f + g | f × g
        swap⁺ swapˣ assocl⁺ assocr⁺ assoclˣ assocrˣ  unite/uniti (both monoids)
        distrib : a×(b+c) ↔ (a×b)+(a×c)      factor = sym distrib
        trace   : (u+a ↔ u+b) → (a ↔ b)
        prim    : registry-resolved opaque pairs (ADR-0013)
```

## Typing

A core iso has a type `a ↔ b`. `+` and `×` are the two monoids, with units
`0` and `1` respectively. Below, metavariables `a b c u` range over finite
types.

```text
                      f : a ↔ b    g : b ↔ c        f : a ↔ b
  id : a ↔ a          ─────────────────────         ───────────
                          f ; g : a ↔ c             sym f : b ↔ a

  f : a ↔ b   g : c ↔ d          f : a ↔ b   g : c ↔ d
  ─────────────────────          ─────────────────────
  f + g : a + c ↔ b + d          f × g : a × c ↔ b × d

  swap⁺   : a + b        ↔ b + a           swapˣ   : a × b        ↔ b × a
  assocl⁺ : a + (b + c)  ↔ (a + b) + c     assoclˣ : a × (b × c)  ↔ (a × b) × c
  assocr⁺ : (a + b) + c  ↔ a + (b + c)     assocrˣ : (a × b) × c  ↔ a × (b × c)
  unite⁺  : 0 + a        ↔ a               uniteˣ  : 1 × a        ↔ a
  uniti⁺  : a            ↔ 0 + a           unitiˣ  : a            ↔ 1 × a

  distrib : a × (b + c)  ↔ (a × b) + (a × c)     factor : (a × b) + (a × c) ↔ a × (b + c)

  f : u + a ↔ u + b                 registry p : A ↔ B (ADR-0013)
  ─────────────────                 ─────────────────────────────
  trace f : a ↔ b                   prim p : A ↔ B
```

`assocr⁺ = sym assocl⁺`, `assocrˣ = sym assoclˣ`, `uniti = sym unite` (each
monoid), and `factor = sym distrib` — they are named for readability but are
adjoints of the preceding form, not independent primitives. Full-Π's `0`-
absorption isos (`0 × a ↔ 0`) are omitted: the v0.1 surface (ADR-0017) never
produces an empty product, and `0`-absorption is a canonicalizer identity
(ADR-0014), not a surface-reachable combinator. They enter only if a later
dialect admits empty products.

## Adjoint (`sym`, a syntactic involution)

`sym f` denotes `f`'s inverse and carries **no runtime cost**: it is computed
by pushing `sym` to the leaves via the table below, where each leaf either is
self-inverse or names its partner. Write `f†` for the normal form of `sym f`.

```text
  id†        = id                        (sym f)†   = f          -- involution
  (f ; g)†   = g† ; f†                    swap⁺†     = swap⁺       -- self-inverse
  (f + g)†   = f† + g†                    swapˣ†     = swapˣ
  (f × g)†   = f† × g†                    assocl⁺†   = assocr⁺     assocr⁺† = assocl⁺
  (trace f)† = trace (f†)                 assoclˣ†   = assocrˣ     assocrˣ† = assoclˣ
  (prim p)†  = prim p̄                     unite⁺†    = uniti⁺      uniti⁺†  = unite⁺
                                          uniteˣ†    = unitiˣ      unitiˣ†  = uniteˣ
                                          distrib†   = factor      factor†  = distrib
```

Here `p̄` is the registry's *backward* pair for `prim p` (see admission,
below). `sym (sym f) ↦ f` makes `(·)†` an involution; `(f ; g)† = g† ; f†`
reverses composition (diagram order); `(trace f)† = trace (f†)` keeps the same
loop carrier `u` and swaps the `a`/`b` boundary. This table is what the
surface `~` (alias `†`) elaborates to, and it is the operation the
adjoint-commutation property `P(f†) ≡ P(f)†` tests against for every pass
(ADR-0016, ADR-0018 T1).

## Values and evaluation (plain fragment)

The interpreter (`theseus-eval`) runs core over **typed values**, before any
bit encoding. The value domain is the initial algebra of the type formers:

```text
  ⟦0⟧     = ∅                      (no values)
  ⟦1⟧     = { () }                 (unit)
  ⟦a + b⟧ = inl ⟦a⟧ ⊎ inr ⟦b⟧      (tagged sum)
  ⟦a × b⟧ = ⟦a⟧ × ⟦b⟧              (pairs)
```

Each core iso `f : a ↔ b` denotes a total bijection `eval f : ⟦a⟧ → ⟦b⟧`:

```text
  eval id            v            = v
  eval (f ; g)       v            = eval g (eval f v)          -- diagram order
  eval (sym f)       v            = eval (f†) v  = (eval f)⁻¹ v
  eval (f + g) (inl v)            = inl (eval f v)
  eval (f + g) (inr w)            = inr (eval g w)
  eval (f × g)       (v, w)       = (eval f v, eval g w)
  eval swap⁺   (inl v)            = inr v          eval swap⁺ (inr w) = inl w
  eval swapˣ         (v, w)       = (w, v)
  eval assocl⁺ (inl v)            = inl (inl v)
  eval assocl⁺ (inr (inl w))      = inl (inr w)
  eval assocl⁺ (inr (inr x))      = inr x                       -- assocr⁺ = its inverse
  eval assoclˣ (v, (w, x))        = ((v, w), x)                 -- assocrˣ = its inverse
  eval uniteˣ  ((), v)            = v            eval unitiˣ v      = ((), v)
  eval unite⁺  (inr v)            = v            eval uniti⁺ v      = inr v   -- inl unreachable (0 empty)
  eval distrib (v, inl w)         = inl (v, w)
  eval distrib (v, inr x)         = inr (v, x)                  -- factor = its inverse
  eval (prim p)      v            = forward_p v                 -- registry forward function
  eval (trace f)     v            = …                           -- deferred with labels
```

Because every clause is a bijection, `eval (sym f) = (eval f)⁻¹` holds
definitionally — the backward run is the same term read through `†`, and no
separate inverse interpreter is needed. This is the operational side of the
coverage-checker soundness result (`theory/coverage-bijectivity.md`, TH-4a
adjoint corollary). `trace` evaluation (iterate the `u`-carried loop to a
fixpoint, fuel-counted) is specified with the label slice; the plain fragment
does not produce `trace`.

## Prim admission (ADR-0013, amended)

A `prim` supplies a `(forward, backward)` pair of finite functions from the
registry. It is admitted **only if the pair are mutual inverses**:
`backward ∘ forward = id_A` and `forward ∘ backward = id_B` over the finite
domains — not merely that each is individually bijective. Bijectivity of
`forward` alone does not make `backward` its inverse; checking only that would
let `(prim p)† = prim p̄` denote a `p̄` that is *a* bijection but not `p`'s
inverse, silently breaking `f† ∘ f ≡ id` (ADR-0008 T1) at run time rather than
at admission. The check is mechanical and exhaustive at finite prim widths.

## Rewrites

- Core rewrites are minimal by rule (ADR-0016): trace laws, Branch/Par
  fusion, prim identities. Pure structural equalities (assoc/unit/swap
  coalescing) belong to the canonicalizer (ADR-0014), not here.
- Fixed-order rewriting in v0.1; eqsat is a v0.3+ candidate (ADR-0010). Each
  core rewrite ships with a termination measure (see the pass pipeline); the
  design does not orient trace dinaturality as a standalone rewrite (it lives
  inside the canonicalizer's coherence argument, `theory/coherence.md`).
- v0.2 additions (`iter`, `fold`/`unfold` as primitive adjoint pairs) are
  specified in ADR-0009 and must not require IR redesign (ADR-0013 reserves
  their nodes).
