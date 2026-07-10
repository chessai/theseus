# Test specification: M1 (language track)

Normative "what done looks like" for the parser, elaborator, coverage
checker, and interpreter — the executable complement to CONTRIBUTING's
definition-of-done. Conformance file formats are defined here.

## Conformance file formats (v1)

**Positive** (`conformance/programs/NNNN-name.thse` + `.expect`): the
`.expect` sidecar lists the complete forward table for one named iso:

```text
# iso: cnot
(O, O) |-> (O, O)
(O, I) |-> (O, I)
...
```

A conforming implementation MUST reproduce every row forward, and — with
no separate table — every row backward under `~` (right column maps to
left column). Tables are exhaustive over the input type; harnesses
SHOULD additionally check the T1 round-trip on every row.

**Typecheck-only positive** (a `.thse` with **no** `.expect` sidecar): the
program MUST parse and pass all four static checks (`spec/static-semantics.md`)
with **no diagnostics**, but there is no forward table to reproduce (its
semantics come from elsewhere — e.g. the prim registry). The harness treats a
missing `.expect` as "must elaborate cleanly, nothing to run." P0012 (`prim
id8`) is the example; it is graded on clean elaboration alone.

**Negative** (`conformance/diagnostics/NNNN-name.thse` + `.diag`): the
`.diag` sidecar lists expected diagnostics as blocks:

```text
error: T0302
side: left
witness: Just O
clauses: 1 2
```

A block is `error:` (the `T0xxx` code, required) followed by any of these
payload keys, each present exactly when the code carries that datum:

| Key | Value | Used by |
|---|---|---|
| `side:` | `left` \| `right` | clause-scoped codes: T0102, T0201, T0202, T0301, T0302, T0303 |
| `witness:` | a value (compared as a value, not a string) | T0301, T0302, T0303 |
| `clauses:` | 1-indexed indices, declaration order, compared as a set | T0302 (a pair), T0102 |
| `variable:` | a variable name | T0201, T0202 |
| `name:` | an identifier | T0101, T0401 |

A listed block **matches** a reported diagnostic iff the `error` code is equal
and every listed payload key is equal (`witness` as a value; `name`/`variable`
as identifiers; `clauses` as a set). No other keys are defined; an
implementation MUST NOT require unlisted keys to match.

Comparison is **set-based** (order-insensitive): every listed block MUST be
reported. Extra reported diagnostics are tolerated only when they share the
`error` code — and, for codes carrying `side:`, the same `side:` — as some
listed block; any other extra is a failure. (Codes without a `side:` — T0001,
T0401, and a T0101 in an `= expr` body — compare on code plus listed payload
only.)

## Parser: done means

- The precedence/associativity table in the grammar chapter reproduced
  exactly (tests: `~f ; g`, `a ; b * c`, `a + b * c` parse trees).
- `parse ∘ pretty ≡ id` (T1) on every conformance program and on
  generated isos.
- Every `.thse` in `conformance/` parses (or fails with T0001 where the
  `.diag` says so); spans cover the full offending token.

## Coverage checker: done means

- Passes the brute-force differential (enumerate; exactly one clause per
  side per value) on all conformance programs and on generated
  bijection-table programs (ADR-0018).
- Reports every block in every `.diag`, with witnesses. D0001–D0010 are
  the seed adversarial corpus; every bug found joins it.
- Mutation score over threshold on the checker module before anything
  downstream trusts it (risk register).

## Interpreter: done means

- The fuel contract (dynamic-semantics chapter) exactly; every positive
  conformance table reproduced forward and backward; `Done` within fuel
  = static bound on all finite-dialect programs (P0011 exercises this
  with a 4-step label loop, bound |u| = 8).
- REPL observable contract: `:step` performs one clause application and
  prints the resulting value (or label state, rendered `$name value`);
  `:unstep` exactly inverts the most recent `:step`; `:unstep` at
  history start is a no-op with a notice. Output *format* is unstable
  in v0.1 (TBD-7); the step/unstep inverse relation is normative now.

## Elaborator and canonicalizer: worked examples (normative modulo ≅)

An implementation's output MUST be denotationally equal to these; the
exact combinator tree may differ (the canonicalizer completeness test
is the equality decision procedure, ADR-0014):

1. `not` (`| O <-> I | I <-> O`) elaborates to the core iso `swap⁺` on
   `Bit ≅ 1+1`.
2. `| (a, b) <-> (b, a)` elaborates to `swapˣ`; after canonicalization,
   exactly `Permute(×, [1,0])`.
3. `swapˣ ; swapˣ` canonicalizes to the identity `Permute(×, [0,1])`
   (σ-composition, zero nodes emitted for it downstream).
4. `| (a,(b,c)) <-> ((a,b),c)` canonicalizes to `Permute(×, [0,1,2])`
   over the flattened 3-leaf layer — i.e., pure re-association is the
   identity permutation and costs nothing (ADR-0014).
5. P0011's label set elaborates to `trace` over `u ≅ Bit×(Bit×Bit)`
   (|u| = 8); the reported static bound MUST be ≤ 8.

## `--emit` contract (all stages)

Deterministic byte-identical output for identical input+flags (T1);
parseable by the same build's reader (serialization round-trip). Format
stability across versions is TBD-8 (not promised in v0.1).

## Test registry (for prim conformance)

Conformance runs against a declared test registry containing exactly:
`theseus.test.id8 : U8 <-> U8` (identity semantics). P0012 typechecks a
matching `prim` declaration; D0009 expects `T0401` on a mismatched
signature against this same registry.
