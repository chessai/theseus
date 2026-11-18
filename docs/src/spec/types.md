# Types

Core types: `0`, `1`, sums, products. Surface named ADTs desugar to
sum/product trees in declared constructor order. Type parameters are
monomorphized during elaboration. They belong to `type` declarations only:
isos and prims are monomorphic, and a type variable in an iso or prim
signature is not yet rejected (ADR-0047, known gaps).

The **synthesizable dialect** (v0.1) admits finite types only; μ-types
enter the software dialect at v0.2 (ADR-0006). Standard library numeric
types `U1/U2/U4/U8/U16` have fixed little-endian-pair representation;
literal syntax elaborates against that representation (ADR-0017).
