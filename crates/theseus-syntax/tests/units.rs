//! Fast-tier unit tests: lexer token mapping and pretty-printer canonical form.

use theseus_syntax::lexer::{lex, Tok};
use theseus_syntax::{parse, pretty_program};

#[test]
fn lexes_arrow_as_single_token() {
    let toks = lex("<->").expect("`<->` lexes");
    assert_eq!(toks.len(), 1);
    assert_eq!(toks[0].tok, Tok::Arrow);
}

#[test]
fn bare_left_arrow_is_a_lex_error() {
    // `<-` is not a token; the leading `<` cannot be lexed.
    assert!(lex("<-").is_err(), "`<-` must not lex");
}

#[test]
fn dagger_and_tilde_share_a_token() {
    let t = lex("~").unwrap();
    let d = lex("\u{2020}").unwrap();
    assert_eq!(t[0].tok, Tok::Tilde);
    assert_eq!(d[0].tok, Tok::Tilde);
}

#[test]
fn spans_cover_the_full_token() {
    // `iso` occupies bytes 0..3.
    let toks = lex("iso name").unwrap();
    assert_eq!(toks[0].span.start, 0);
    assert_eq!(toks[0].span.end, 3);
    // `name` occupies bytes 4..8.
    assert_eq!(toks[1].span.start, 4);
    assert_eq!(toks[1].span.end, 8);
}

#[test]
fn line_comments_are_skipped() {
    let toks = lex("iso -- a comment\nid").unwrap();
    assert_eq!(toks[0].tok, Tok::Iso);
    assert_eq!(toks[1].tok, Tok::Id);
}

#[test]
fn pretty_elides_redundant_left_parens() {
    // `(a ; b) ; c` is the left-assoc default; canonical form drops the parens.
    let prog = parse("iso f : A <-> A = (a ; b) ; c").unwrap();
    let out = pretty_program(&prog);
    assert!(out.contains("a ; b ; c"), "got: {out}");
}

#[test]
fn pretty_keeps_needed_right_parens() {
    // `a ; (b ; c)` is NOT the default nesting, so the parens must be kept.
    let prog = parse("iso f : A <-> A = a ; (b ; c)").unwrap();
    let out = pretty_program(&prog);
    assert!(out.contains("a ; (b ; c)"), "got: {out}");
}

#[test]
fn double_arrow_iso_signature_is_a_parse_error() {
    // An iso signature has exactly one `<->`; a second arrow must error and
    // must not panic.
    assert!(parse("iso f : A <-> A <-> A = id").is_err());
}

#[test]
fn pretty_types_respect_precedence() {
    // A constructor payload is a single type atom; a product payload must be
    // parenthesized, and the pretty-printer must preserve those parentheses.
    let prog = parse("type T = C (A * B)").unwrap();
    let out = pretty_program(&prog);
    assert!(out.contains("C (A * B)"), "got: {out}");

    // `A + B * C` binds as `A + (B * C)`; the sum needs no parens on the left.
    let prog = parse("iso f : A + B * C <-> A = id").unwrap();
    let out = pretty_program(&prog);
    assert!(out.contains("A + B * C"), "got: {out}");
}

#[test]
fn parses_clause_body_isos() {
    // The `| pattern <-> pattern` body form (grammar.md isobody / clauses).
    let prog = parse("type Bit = O | I\niso f : Bit <-> Bit\n| O <-> I\n| I <-> O").unwrap();
    let out = pretty_program(&prog);
    assert!(out.contains("| O <-> I"), "got: {out}");
    assert!(out.contains("| I <-> O"), "got: {out}");
}

#[test]
fn parses_where_block() {
    // `where` attaches nested isos to a single isodecl (grammar.md).
    let prog = parse(
        "iso twice : Bit <-> Bit\n= n ; n\nwhere\n  iso n : Bit <-> Bit\n  | O <-> I\n  | I <-> O",
    )
    .unwrap();
    let out = pretty_program(&prog);
    assert!(out.contains("where"), "got: {out}");
    assert!(out.contains("iso n :"), "got: {out}");
}

#[test]
fn parses_prim_with_string_key() {
    let prog = parse(r#"prim id8 : U8 <-> U8 = "theseus.test.id8""#).unwrap();
    let out = pretty_program(&prog);
    assert!(out.contains(r#"= "theseus.test.id8""#), "got: {out}");
}

#[test]
fn parses_label_pattern() {
    // `$go` label patterns, applied and nested (grammar.md pattern).
    let prog =
        parse("iso spin : Bit <-> Bit\n| b <-> $go (b, (O, O))\n| $go (b, (I, I)) <-> b").unwrap();
    let out = pretty_program(&prog);
    assert!(out.contains("$go"), "got: {out}");
}

#[test]
fn malformed_clause_arrow_is_a_parse_error() {
    // The exact shape of conformance/diagnostics/0007-parse.thse: a `<-` where
    // a clause `<->` is required must be a T0001 parse error, not a panic.
    let err = parse("type Bit = O | I\niso bad : Bit <-> Bit\n| O <- I")
        .expect_err("`<-` must not parse");
    assert_eq!(err.code, "T0001");
}

#[test]
fn tuple_types_are_products() {
    // `(A, B)` is product sugar; nesting is right-associative and round-trips.
    let prog =
        parse("iso f : (Bit, (Bit, Bit)) <-> ((Bit, Bit), Bit)\n| (a, (b, c)) <-> ((a, b), c)")
            .unwrap();
    let out = pretty_program(&prog);
    let reparsed = parse(&out).expect("pretty output re-parses");
    assert_eq!(
        theseus_syntax::normalize_program(&prog),
        theseus_syntax::normalize_program(&reparsed)
    );
}
