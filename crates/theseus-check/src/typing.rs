//! Typing check (T0102) — **stub**.
//!
//! A sibling task replaces the body of [`check`]. For now it reports nothing so
//! the staged driver type-checks and returns clean on valid programs.

use theseus_syntax::ast::Program;

use crate::diagnostics::Diagnostic;
use crate::env::Env;

/// Run the typing check (currently a no-op stub).
pub fn check(_program: &Program, _env: &Env) -> Vec<Diagnostic> {
    Vec::new()
}
