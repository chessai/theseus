//! Verification oracles for `--verify-lowering` (ADR-0012, ADR-0020 amended).
//!
//! Pipeline: build a miter over our own bit-netlist IR (candidate vs spec,
//! XOR outputs, assert difference) → Tseitin (~owned, small) → CNF →
//! in-process solver. SMT-LIB export survives as a *debug artifact* only.
//!
//! ```text
//! trait CnfSolver        // add_clause, solve(assumptions) -> Sat(model)/Unsat
//! trait ObligationOracle // prove(Obligation) -> Verdict; owns encoding;
//!                        // below the size threshold the "solver" is the
//!                        // exhaustive enumerator (ADR-0018)
//! ```
//!
//! Planned impls: splr (pure Rust, keeps a zero-C-deps config alive) and
//! CaDiCaL/kissat bindings (incremental, for batched per-instance lemmas).
//! Sat models decode through the typed layer into readable witnesses.

pub mod miter {}
pub mod tseitin {}
pub mod solver {}
pub mod obligations {}
