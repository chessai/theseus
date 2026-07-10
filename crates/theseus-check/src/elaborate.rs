//! Elaboration: surface [`IsoDecl`] → core [`Iso`] — **stub**.
//!
//! A sibling task replaces the body of [`elaborate`]. It currently returns
//! `Err(Vec::new())` unconditionally; downstream code should treat an empty
//! error vector from this stub as "not yet implemented", not "no errors".

use theseus_core::iso::Iso;
use theseus_syntax::ast::IsoDecl;

use crate::diagnostics::Diagnostic;
use crate::env::Env;

/// Elaborate a surface iso declaration into a core iso (currently a stub that
/// always returns an empty error vector).
pub fn elaborate(_iso: &IsoDecl, _env: &Env) -> Result<Iso, Vec<Diagnostic>> {
    Err(Vec::new())
}
