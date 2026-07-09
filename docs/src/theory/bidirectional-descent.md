# The bidirectional-descent trap (TH-2)

Forward totality does **not** imply backward totality for reversible
programs over infinite state: a measure decreasing along forward loop
iterations strictly *increases* along the adjoint's iterations, and an
infinite backward chain of states feeding an exit — never reached from
the entry side — diverges under `~f` while `f` is total.

Consequence: any termination discipline (sized types, structural
descent) must be discharged **twice**, once per direction, or replaced
by constructions symmetric by design — which is exactly why v0.2's
bounded iterator carries its count *in the data* (up one way, down the
other) and why fold/unfold ship as an adjoint pair (ADR-0009).
Any external design that certifies only the forward direction is
unsound for a reversible language.
