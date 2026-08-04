# Conformance and notation

This specification is normative for the theseus language, versioned with
the language (spec v0.1 freezes with its conformance suite), and
**implementation-silent**: nothing here may reference Rust, crates, or any
implementation strategy (ADR-0021). The key words MUST, MUST NOT, SHOULD,
MAY are per RFC 2119.

The executable appendix of this specification is the conformance suite at
`/conformance`: positive programs with expected denotations, negative
programs with expected error codes/spans/witnesses, and the universal
properties every conforming implementation MUST satisfy (round-trip,
adjoint, determinism).
