//! Linearity check (T0201/T0202) — **stub**.
//!
//! A sibling task replaces the body of [`check`]. For now it reports nothing.

use theseus_syntax::ast::Program;

use crate::diagnostics::Diagnostic;
use crate::env::Env;

/// Run the linearity check (currently a no-op stub).
pub fn check(_program: &Program, _env: &Env) -> Vec<Diagnostic> {
    Vec::new()
}
