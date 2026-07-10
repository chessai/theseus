//! Conformance & property tests for `theseus-syntax`.
//!
//! These walk the repo's read-only `conformance/` tree and assert the M1
//! "Parser: done means" obligations (docs/src/spec/test-spec-m1.md):
//!
//! * Every `conformance/**/*.thse` PARSES, except
//!   `conformance/diagnostics/0007-parse.thse`, which MUST fail as a parse
//!   error (its malformed `<-` where `<->` is required).
//! * `parse ∘ pretty ≡ id` (T1 round-trip) on every conformance program.
//! * The frozen precedence consequences hold.

use std::path::{Path, PathBuf};

use theseus_syntax::ast::{BinOp, Decl, Expr, IsoBody, LowerName, Span, UpperName};
use theseus_syntax::{normalize_program, parse, pretty_program};

/// Absolute path to the repo root (two levels up from this crate's manifest).
fn repo_root() -> PathBuf {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest
        .parent() // crates/
        .and_then(Path::parent) // repo root
        .expect("crate lives at <repo>/crates/theseus-syntax")
        .to_path_buf()
}

/// Recursively collect every `*.thse` file under `dir`, sorted for determinism.
fn collect_thse(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut entries: Vec<_> = entries.filter_map(Result::ok).map(|e| e.path()).collect();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            collect_thse(&path, out);
        } else if path.extension().and_then(|s| s.to_str()) == Some("thse") {
            out.push(path);
        }
    }
}

/// The one conformance program that MUST fail to parse.
fn is_expected_failure(path: &Path) -> bool {
    path.ends_with("diagnostics/0007-parse.thse")
}

fn all_programs() -> Vec<PathBuf> {
    let conformance = repo_root().join("conformance");
    let mut files = Vec::new();
    collect_thse(&conformance, &mut files);
    assert!(
        !files.is_empty(),
        "no conformance/**/*.thse files found under {}",
        conformance.display()
    );
    files
}

#[test]
fn ac1_every_conformance_program_parses_except_0007() {
    let mut parsed = 0usize;
    let mut failed = 0usize;
    for path in all_programs() {
        let src = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
        let result = parse(&src);
        if is_expected_failure(&path) {
            let err = result.expect_err(&format!(
                "{} MUST fail to parse (malformed `<-`)",
                path.display()
            ));
            assert_eq!(err.code, "T0001", "parse error must carry code T0001");
            failed += 1;
        } else {
            result.unwrap_or_else(|e| panic!("{} must parse, but: {e}", path.display()));
            parsed += 1;
        }
    }
    assert_eq!(failed, 1, "exactly one conformance program must fail (0007)");
    assert!(parsed >= 1, "at least one conformance program must parse");
    eprintln!("AC1: {parsed} parsed, {failed} expected-failure");
}

#[test]
fn ac2_parse_pretty_roundtrip_is_identity() {
    let mut checked = 0usize;
    for path in all_programs() {
        if is_expected_failure(&path) {
            continue;
        }
        let src = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
        let ast1 = parse(&src).unwrap_or_else(|e| panic!("{} must parse: {e}", path.display()));
        let printed = pretty_program(&ast1);
        let ast2 = parse(&printed).unwrap_or_else(|e| {
            panic!(
                "re-parsing pretty output of {} failed: {e}\n--- printed ---\n{printed}",
                path.display()
            )
        });
        assert_eq!(
            normalize_program(&ast1),
            normalize_program(&ast2),
            "parse ∘ pretty ≡ id failed for {}\n--- printed ---\n{printed}",
            path.display()
        );
        checked += 1;
    }
    eprintln!("AC2: {checked} programs round-tripped");
    assert!(checked >= 1);
}

// -------- AC2: precedence consequences from grammar.md --------

const ZERO: Span = Span { start: 0, end: 0 };

/// Parse a single iso body expression by wrapping it in a trivial iso decl.
fn body_shape(expr_src: &str) -> Expr {
    let src = format!("iso f : A <-> A = {expr_src}");
    let prog = parse(&src).unwrap_or_else(|e| panic!("parse `{expr_src}`: {e}"));
    match &prog.decls[0] {
        Decl::Iso(i) => match &i.body {
            IsoBody::Expr(e) => strip(e),
            IsoBody::Clauses(_) => unreachable!("body_shape wraps an `= expr` body"),
        },
        Decl::Type(_) | Decl::Prim(_) => unreachable!(),
    }
}

/// Span-free, paren-free normalization of an expression, for structural
/// comparison of parse shapes.
fn strip(e: &Expr) -> Expr {
    match e {
        Expr::Var(n) => Expr::Var(LowerName {
            text: n.text.clone(),
            span: ZERO,
        }),
        Expr::Ctor(n) => Expr::Ctor(UpperName {
            text: n.text.clone(),
            span: ZERO,
        }),
        Expr::Id(_) => Expr::Id(ZERO),
        Expr::Unit(_) => Expr::Unit(ZERO),
        Expr::Paren(inner, _) => strip(inner),
        Expr::Adjoint(inner, _) => Expr::Adjoint(Box::new(strip(inner)), ZERO),
        Expr::Bin(op, l, r, _) => Expr::Bin(*op, Box::new(strip(l)), Box::new(strip(r)), ZERO),
        Expr::Tuple(l, r, _) => Expr::Tuple(Box::new(strip(l)), Box::new(strip(r)), ZERO),
    }
}

fn var(name: &str) -> Expr {
    Expr::Var(LowerName {
        text: name.to_string(),
        span: ZERO,
    })
}

fn bin(op: BinOp, l: Expr, r: Expr) -> Expr {
    Expr::Bin(op, Box::new(l), Box::new(r), ZERO)
}

fn adj(e: Expr) -> Expr {
    Expr::Adjoint(Box::new(e), ZERO)
}

#[test]
fn ac2_precedence_adjoint_binds_tighter_than_seq() {
    // `~f ; g` = `(~f) ; g`
    assert_eq!(
        body_shape("~f ; g"),
        bin(BinOp::Seq, adj(var("f")), var("g")),
        "~f ; g must parse as (~f) ; g"
    );
}

#[test]
fn ac2_precedence_star_binds_tighter_than_seq() {
    // `a ; b * c` = `a ; (b * c)`
    assert_eq!(
        body_shape("a ; b * c"),
        bin(BinOp::Seq, var("a"), bin(BinOp::Prod, var("b"), var("c"))),
        "a ; b * c must parse as a ; (b * c)"
    );
}

#[test]
fn ac2_precedence_star_binds_tighter_than_sum() {
    // `a + b * c` = `a + (b * c)`
    assert_eq!(
        body_shape("a + b * c"),
        bin(BinOp::Sum, var("a"), bin(BinOp::Prod, var("b"), var("c"))),
        "a + b * c must parse as a + (b * c)"
    );
}

#[test]
fn ac2_infix_operators_are_left_associative() {
    // `a ; b ; c` = `(a ; b) ; c`
    assert_eq!(
        body_shape("a ; b ; c"),
        bin(BinOp::Seq, bin(BinOp::Seq, var("a"), var("b")), var("c")),
        "a ; b ; c must parse as (a ; b) ; c"
    );
}

#[test]
fn ac2_tuples_nest_to_the_right() {
    // `(a, b, c)` = `(a, (b, c))`
    let want = Expr::Tuple(
        Box::new(var("a")),
        Box::new(Expr::Tuple(Box::new(var("b")), Box::new(var("c")), ZERO)),
        ZERO,
    );
    assert_eq!(
        body_shape("(a, b, c)"),
        want,
        "(a, b, c) must nest as (a, (b, c))"
    );
}

#[test]
fn ac2_dagger_lexes_as_tilde() {
    // `†f` and `~f` produce the same AST, and pretty always emits `~`.
    assert_eq!(
        body_shape("\u{2020}f"),
        body_shape("~f"),
        "† must lex to the same token as ~"
    );
    let prog = parse("iso f : A <-> A = \u{2020}f").unwrap();
    let printed = pretty_program(&prog);
    assert!(printed.contains('~'), "canonical form uses ~");
    assert!(!printed.contains('\u{2020}'), "canonical form never emits †");
}

#[test]
fn ac1_malformed_arrow_is_a_parse_error() {
    // The disambiguation that drives conformance/diagnostics/0007-parse.thse:
    // a bare `<-` where `<->` is required must be a T0001 parse error.
    let err = parse("iso f : A <- A = id").expect_err("`<-` must not parse");
    assert_eq!(err.code, "T0001");
}
