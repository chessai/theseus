//! Elaboration: surface clauses → core Π, with the coverage checker.
//!
//! This crate is the named highest technical risk (ADR-0004) and the soul
//! of the language: a clause set elaborates iff both sides are exhaustive
//! and non-overlapping under linear pattern variables — which is exactly
//! the theorem that the hardware muxes are invertible (ADR-0012).
//!
//! Obligations (ADR-0018):
//! - Differential test against the brute-force semantic definition
//!   (enumerate values; each matches exactly one clause per side).
//! - Errors carry concrete witnesses (the unmatched value; the value
//!   matched twice) — a tested obligation, not a nicety (ADR-0020 amended).
//! - Label state spaces (`$name`, ADR-0017) extend coverage checking and
//!   elaborate to `trace` (ADR-0009); WCET surfaces as a diagnostic note.

pub mod elaborate {}

pub mod coverage {
    //! Maranget-style pattern matrices, bidirectional, with linearity.
    //! Lightly research territory — prototype earliest (risk register).
}

pub mod diagnostics {
    //! Structured values with `T0xxx` codes; `--explain` pages are spec
    //! sections shared by include-machinery (ADR-0021).
}
