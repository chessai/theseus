# Appendix: crate map

See ADR-0020 for rationale and amendments. Workspace crates:

| Crate | Owns |
|---|---|
| `theseus-syntax` | AST, spans, lexer (logos), hand-written parser, pretty |
| `theseus-check` | elaboration, coverage/linearity, structured diagnostics |
| `theseus-core` | core Π, adjoint, fixed-order rewrites (ADR-0010) |
| `theseus-eval` | fuel interpreter, typed values, exhaustive enumerator |
| `theseus-netlist` | typed IR, canonicalizer, scheduler, flatten, **lowering** (`lower.rs`, `lower_iter.rs` → phased cell netlist), both sims |
| `theseus-cli` | driver (`check`/`run`/`repl`/`lower`), REPL `:step`/`:unstep` |

Named implementation-time benchmarks: bitvec vs packed `Vec<u64>`;
diagnostics renderer taste call behind the structured-value model.

**Verification path (actual).** The whole-program/primitive verification path is
**BVEQ** (whole-domain symbolic BDD equivalence, `crates/theseus-tisc/src/bveq.rs`
+ `bveq_netlist.rs`) plus the differential oracle (lowered netlist vs
`theseus-eval` over the whole finite domain). The earlier `theseus-verify` crate
(miter / Tseitin / `CnfSolver`) has been **removed** -- superseded by BVEQ; it is
not part of the workspace.

**Not-yet-implemented (deferred, v0.2+).** Verilog / SystemVerilog emission and the
Verilator harness are **future work**: the `theseus-emit` crate is an empty stub
(no typed Verilog AST, no lowering) and is not on any live path. See the M2 row in
[status](../status.md) (parked -- a demo, not a phase).

**Pivot addition (ADR-0022/0025):**

| Crate | Owns |
|---|---|
| `theseus-spice` | cell mapping to phased netlist, SPICE deck emission, characterization harness driver, DB build/fit, transient decode for the oracle link, energy report generator (`annotate`, programmatic) |

*Energy attribution is a **programmatic** API (`theseus-spice::annotate`,
`rom_program_energy`) today; the `--emit=energy` CLI flag is deferred (v0.2+) --
the driver has no `--emit` subcommand yet.*
