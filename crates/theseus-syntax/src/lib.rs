//! Surface syntax: AST, spans, lexing, parsing, pretty-printing.
//!
//! Boundaries and obligations:
//! - Grammar per ADR-0017 (named ADTs with significant constructor order,
//!   symmetric `<->` clause sets, `$label` states, `;`/`*`/`+`/`~`
//!   combinator expressions, registry-keyed `prim` declarations).
//! - Parser is hand-written recursive descent (ADR-0020): the grammar is
//!   small and LL-friendly, and we own error recovery because elaboration
//!   is the user-error boundary (ADR-0016) — the diagnostics budget is
//!   spent here and in `theseus-check`.
//! - `parse ∘ pretty ≡ id` is a T1 conformance property (ADR-0018).

pub mod span {
    //! `Span`/`FileId` are deliberately crate-local and tiny (ADR-0020).
}

pub mod ast {
    //! Surface AST. Every node carries a span; provenance backpointers
    //! survive all the way to netlists (ADR-0016).
}

pub mod lexer {}

pub mod parser {
    //! `fold` / `unfold` / `for` are reserved words from day one
    //! (ADR-0017) so v0.2's total combinators have syntax waiting.
}

pub mod pretty {}
