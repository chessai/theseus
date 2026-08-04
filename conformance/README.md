# Conformance suite (v0.1, seeding)

Pinned to the language spec; runnable against any implementation
(ADR-0018/0021). Layout:

- `programs/` — positive cases: `NNNN-name.thse` + `NNNN-name.expect`
  (complete forward value table; backward implied under `~`). Format and
  harness obligations: `docs/src/spec/test-spec-m1.md`. P0001–P0015 are
  the seed set; every expected table is machine-verified as a bijection
  before commit. The bijection-table generator contributes minimized
  cases here.
- `diagnostics/` — negative cases: `NNNN-name.thse` + `NNNN-name.diag`
  (expected code/side/witness blocks, set-compared). D0001–D0010 seed
  the adversarial corpus for the coverage checker (ADR-0004).
- Universal properties (round-trip, adjoint, determinism) are specified
  in the spec's conformance chapter; harness bindings live with each
  implementation.
