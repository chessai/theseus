//! `theseus-syntax` -- the surface language front end for the theseus
//! reversible-computing compiler (M1 slice 1).
//!
//! This crate provides:
//! * [`ast`] -- the surface abstract syntax tree, with a [`ast::Span`] on every
//!   node (spans cover the full offending token, for diagnostics).
//! * [`lexer`] -- a `logos`-based lexer. `†` and `~` share a token; `<->` is a
//!   single token.
//! * [`parser`] -- a hand-written recursive-descent parser honoring the frozen
//!   precedence table (`~` > `*` > `+` > `;`, all infix left-associative). It
//!   covers the whole grammar (`docs/src/spec/grammar.md`): `type`/`iso`/`prim`
//!   declarations, clause and `= expr` iso bodies, `where` blocks, tuple and
//!   label (`$go`) patterns, and `Nat` literals.
//! * [`pretty`] -- a pretty-printer emitting canonical surface syntax, plus
//!   [`pretty::normalize_program`] used for the `parse ∘ pretty ≡ id` property.
//!
//! Parse errors carry the error code `T0001` (see [`parser::ParseError`]);
//! structured rendering lives in `theseus-check`.

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod pretty;

pub use ast::{Program, Span};
pub use parser::{parse, ParseError};
pub use pretty::{normalize_program, pretty_program};

/// Parse `src`, pretty-print the resulting AST, and return the canonical text.
///
/// Convenience wrapper used by tests and downstream tooling.
///
/// # Errors
/// Returns a [`ParseError`] (code `T0001`) if `src` does not parse.
pub fn reformat(src: &str) -> Result<String, ParseError> {
    let program = parse(src)?;
    Ok(pretty_program(&program))
}
