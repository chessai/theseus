# Static semantics

An `iso` declaration is well-formed iff:

1. **Typing** — both sides of every clause are patterns of the declared
   types (labels extend the state space; label types are inferred or
   annotated).
2. **Linearity** — every pattern variable occurs exactly once on its side
   and exactly once on the opposite side (ADR-0004).
3. **Coverage** — on each side independently, the clause patterns MUST be
   exhaustive and pairwise non-overlapping over the (label-extended) type.

Coverage violations are errors, not warnings, and implementations MUST
report a concrete witness: an unmatched value, or a value matched by two
identified clauses. Guards are not part of v0.1 (see ADR-0017 for the
recomputability condition any future guard extension MUST satisfy).

For programs in the finite dialect, implementations SHOULD report the
static iteration bound of each label loop (ADR-0007).
