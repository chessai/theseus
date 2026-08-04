//! The netlist IRs: the typed reversible netlist (ADR-0013) and the
//! phased cell netlist (ADR-0026 C2.7).
//!
//! Two forms live here, connected by [`crate::lower`]:
//!
//! - [`typed`] -- hierarchical, typed, direction-symmetric; sums stay
//!   *typed* (encoding is a backend decision, ADR-0011); all structural
//!   plumbing is a single `Permute(monoid, σ)` node (ADR-0014).
//! - [`cell`] -- the adiabatic backend's artifact: dual-rail, cell-mapped
//!   (k-CSWAP family, ADR-0027), phase-attributed (phase = depth mod P,
//!   ADR-0026 C2.3). This is what the phased-sim ticks over and what the
//!   energy annotation (ADR-0028) sums over.

pub mod cell;
pub mod typed;
