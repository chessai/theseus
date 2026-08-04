# ADR-0017: Surface syntax

**Status:** Accepted

## Context
Where earlier decisions become ergonomic or get buried. Ratified
principle: **the syntax should make reversal look cheap**, because it is.

## Decision
- **Named ADTs** (`type Maybe a = Nothing | Just a`); constructor order is
  significant (determines tag assignment under FPGA lowering); `@tag`
  override noted for v0.2. Type params monomorphize at elaboration.
- **Clause form:** `iso name : A <-> B` with `| lhs <-> rhs` clauses;
  linear variables per side, every variable on both sides; symmetric
  coverage errors with witnesses.
- **No guards in v0.1.** A guard is information consumed forward that must
  be recomputable from the right side backward -- a semantic condition.
  v0.2+ option documented: guards admitted only when the checker verifies
  the predicate is determined by each side independently.
- **Labels:** original-Theseus-style `$name pattern` pseudo-constructors
  of the iso's state space, either side of clauses; label types inferred
  (optionally annotated); multiple labels sum; iso-local. Elaborate to
  `trace` (ADR-0009); WCET surfaces as a diagnostic note (ADR-0007).
- **Combinator expressions:** `;` sequential in *diagram order*, `*`/`+`
  parallel/choice mirroring type operators, `~f` adjoint (`†` unicode
  alias); iso body is clauses or an expression, `where` for local isos.
- **Prims:** `prim add8 : (U8,U8) <-> (U8,U8) = "theseus.arith.add8"`.
- **Literals** via a blessed stdlib (`U1/U2/U4/U8/U16`, fixed
  little-endian-pair representation) -- the choice lives in library code.
- **Reserved now:** `fold`, `unfold`, `for` (v0.2 total combinators).
- **Flavor:** ML/Haskell-style, layout-insensitive -- the notation should
  look like the math it is, and paper↔code correspondence has real value.

## Alternatives rejected
Rust-flavored surface (braces/`=>`): pure taste axis, affects nothing
downstream of the parser; owner ratified ML flavor.
