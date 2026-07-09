# Core Π

The v0.1 core (ADR-0003, ADR-0009):

```text
types:  0 | 1 | a + b | a × b
isos:   id | sym f | f ; g | f + g | f × g
        swap⁺ swapˣ assocl⁺ assocr⁺ assoclˣ assocrˣ unite/uniti (both monoids)
        distrib : a×(b+c) ↔ (a×b)+(a×c)      factor = sym distrib
        trace   : (u+a ↔ u+b) → (a ↔ b)
        prim    : registry-resolved opaque pairs (ADR-0013)
```

- Adjoint is a syntactic involution; `sym` costs nothing anywhere.
- Core rewrites are minimal by rule (ADR-0016): trace laws, fusion,
  prim identities. Structural equalities belong to the canonicalizer.
- Fixed-order rewriting in v0.1; eqsat is a v0.3+ candidate (ADR-0010).
- v0.2 additions (`iter`, `fold`/`unfold` as primitive adjoint pairs) are
  specified in ADR-0009 and must not require IR redesign (ADR-0013
  reserves their nodes).
