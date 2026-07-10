//! The lexer, built on `logos` (ADR-0020 / crate-map).
//!
//! `logos` gives us a fast, table-driven DFA. We keep the token set tiny and
//! faithful to the frozen grammar. Two normative subtleties:
//!
//! * `<->` is a single token (never `<` `-` `>` or `<-` `>`), so a stray `<-`
//!   is a lex/parse error (drives `conformance/diagnostics/0007-parse.thse`).
//! * `†` (U+2020 DAGGER) lexes to the *same* token as `~`; the parser and
//!   pretty-printer never observe the difference, and canonical output uses
//!   `~`.

use logos::Logos;

use crate::ast::Span;

/// The token kinds produced by the lexer.
#[derive(Logos, Clone, Debug, PartialEq, Eq)]
// Skip ASCII whitespace and `--` line comments.
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"--[^\n]*")]
pub enum Tok {
    // --- keywords ---
    #[token("type")]
    Type,
    #[token("iso")]
    Iso,
    #[token("prim")]
    Prim,
    #[token("where")]
    Where,
    #[token("id")]
    Id,

    // --- punctuation / operators ---
    /// The iso arrow `<->` (a single token by fiat).
    #[token("<->")]
    Arrow,
    #[token("=")]
    Eq,
    #[token(":")]
    Colon,
    #[token("|")]
    Bar,
    #[token(",")]
    Comma,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("$")]
    Dollar,

    /// Adjoint / dagger. Both `~` and `†` produce this token; the source glyph
    /// is not retained (canonical form is `~`).
    #[token("~")]
    #[token("\u{2020}")]
    Tilde,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,
    #[token(";")]
    Semi,

    /// A numeric literal (`Nat`). In `ty` position `0` and `1` denote the
    /// empty/unit types; in `atomPat` position any `Nat` is a literal. The
    /// disambiguation is grammatical (handled by the parser).
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<u128>().ok())]
    Nat(u128),

    /// A string literal (a `primdecl` registry key). The stored text excludes
    /// the surrounding double quotes.
    #[regex(r#""[^"\n]*""#, |lex| {
        let s = lex.slice();
        s[1..s.len() - 1].to_owned()
    })]
    Str(String),

    // --- names (ML case discipline) ---
    /// An identifier beginning with an upper-case ASCII letter.
    #[regex(r"[A-Z][A-Za-z0-9_']*", |lex| lex.slice().to_owned())]
    Upper(String),
    /// An identifier beginning with a lower-case ASCII letter or underscore.
    #[regex(r"[a-z_][A-Za-z0-9_']*", |lex| lex.slice().to_owned())]
    Lower(String),
}

/// A token paired with its source span.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned {
    pub tok: Tok,
    pub span: Span,
}

/// A lexical error: the byte span that could not be tokenized.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LexError {
    pub span: Span,
}

/// Tokenize `src`, returning the token stream or the first lexical error.
///
/// On success every token carries a span covering the full matched text.
pub fn lex(src: &str) -> Result<Vec<Spanned>, LexError> {
    let mut out = Vec::new();
    let mut lexer = Tok::lexer(src);
    while let Some(res) = lexer.next() {
        let span: Span = lexer.span().into();
        match res {
            Ok(tok) => out.push(Spanned { tok, span }),
            Err(()) => return Err(LexError { span }),
        }
    }
    Ok(out)
}
