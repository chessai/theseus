# ADR-0020: Crate map and library choices

**Status:** Accepted (amended)

## Decision
- **Lexer:** logos. **Parser:** hand-written recursive descent — small
  LL-friendly grammar (ADR-0017), total control over recovery and
  messages at the diagnostics-budget boundary (ADR-0016).
- **Diagnostics (amended — maximally helpful):** diagnostics are
  structured values (miette-modeled), never eager strings; `T0xxx` error
  codes with `--explain` pages that double as spec sections; coverage
  errors carry **concrete witnesses** (natural products of the matrix
  algorithm — a tested obligation); diagnostic snapshots live in the
  conformance suite. Renderer (ariadne vs codespan vs miette-fancy) is a
  swappable taste call behind the data model.
- **IR infra:** la-arena/id-arena + typed IDs; lasso interning; petgraph
  for analyses (not as the IR). **Determinism policy:** IndexMap/BTreeMap
  and sorted iteration wherever order can reach an artifact.
- **Pretty:** `pretty` (Wadler) for all IR dumps. **Serde:** ron for
  golden files/corpus; postcard if a hot interchange path appears.
- **Testing:** proptest (+ custom exhaustive enumerator trait, rayon
  sweeps), arbitrary for fuzz targets, insta, nextest, cargo-mutants.
- **Verification (amended — in-process):** obligations are equivalence
  checks over bit netlists we already possess → miter in our own IR →
  owned Tseitin (~200 lines) → CNF → `CnfSolver` trait
  (splr = pure-Rust config; CaDiCaL/kissat bindings = incremental
  performance option; evaluate `rustsat` as dependency or design
  reference). `ObligationOracle` above it owns encoding and hosts the
  exhaustive enumerator below the size threshold. Sat models decode to
  typed witnesses. SMT-LIB survives as *debug export only*.
- **Simulators:** custom cores; bitvec-vs-packed-Vec<u64> is a named
  implementation-time benchmark; VCD via `vcd`, typed-decoded names.
- **Verilog emission (amended after ecosystem survey):** the Rust
  ecosystem offers parsers and thin AST builders (`vast` — evaluate as
  skeleton; expect to own the code); no crate buys more than it costs.
  We own a **typed mini-AST for the structural subset we emit**, where
  `x`/don't-cares are unrepresentable and width agreement is
  constructor-enforced. RustHDL/Calyx/Veryl are wrong-shaped (noted).
- **Verilator:** standalone verilated binary + framed stdio protocol
  (sandbox-friendly, crash-isolated; the protocol doubles as the
  conformance simulator interface). In-process `libloading` = upgrade
  path.
- **CLI/plumbing:** clap, tracing(+subscriber), thiserror/miette, camino.
- **Reserved v0.3+:** egg/egglog (ADR-0010).
