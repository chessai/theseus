# Appendix: crate map

See ADR-0020 for rationale and amendments. Workspace crates:

| Crate | Owns |
|---|---|
| `theseus-syntax` | AST, spans, lexer (logos), hand-written parser, pretty |
| `theseus-check` | elaboration, coverage/linearity, structured diagnostics |
| `theseus-core` | core Π, adjoint, fixed-order rewrites (ADR-0010) |
| `theseus-eval` | fuel interpreter, typed values, exhaustive enumerator |
| `theseus-netlist` | typed IR, canonicalizer, scheduler, flatten, both sims |
| `theseus-emit` | lowering, typed Verilog AST, Verilator harness/protocol |
| `theseus-verify` | miter, Tseitin, `CnfSolver`/`ObligationOracle` |
| `theseus-cli` | driver, invariant-ledger validation, REPL `:step`/`:unstep` |

Named implementation-time benchmarks: bitvec vs packed `Vec<u64>`;
diagnostics renderer taste call behind the structured-value model.

**Pivot addition (ADR-0022/0025):**

| Crate | Owns |
|---|---|
| `theseus-spice` | cell mapping to phased netlist, SPICE deck emission, characterization harness driver, DB build/fit, transient decode for the oracle link, energy report generator (`--emit=energy`) |
