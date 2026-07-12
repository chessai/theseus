//! The adiabatic backend and its bench (ADR-0022 pivot; ADR-0025/0026/0027/0028).
//!
//! Owns the physics-facing half of the toolchain:
//! - cell mapping: typed flat netlist → phased cell netlist (phase =
//!   depth mod P; tick-exact balancing is mandatory correctness, C2.2;
//!   INV-TOKEN asserted).
//! - deck emission: deterministic, snapshot-tested SPICE decks with
//!   4-phase ideal sources and stimulus through the shared typed
//!   encoders (ADR-0025 B5.2). Idealization rung stamped (ADR-0032).
//! - characterization harness: sweep driver, ∫V·I measurement decode,
//!   conservation bookkeeping self-validation, DB build + fits (TH-E3).
//! - the physics oracle link: transient decode back through the typed
//!   layer, compared against the interpreter (B5.3).
//! - energy reports: static Σ over cells × ticks from the DB, per-clause
//!   attribution via provenance, three-curve money-plot generator,
//!   INV-STAMP enforced (ADR-0028).

pub mod phased {
    //! Phased cell netlist + tick-level simulator (the C2.7 artifact).
    //! Validity-window protocol checks carry window-TBD(bench) holes
    //! until C2.6 pinning (risk register).
}
pub mod cells {
    //! k-CSWAP library bindings (k ∈ {0,1,2}), baselines (CMOS, PFAL),
    //! bench source/sink; `.subckt` text lives in /cells (ADR-0027).
}
pub mod deck {}
pub mod admission;
pub mod bench;
pub mod adiabatic;
pub mod cswap;
pub mod artifact;
pub mod chardb;
pub mod energy;
