//! Conformance test harness (public — sibling stages call it from their tests).
//!
//! This module provides:
//! * [`repo_root`] and fixture collectors over `conformance/`;
//! * [`parse_diag`] for the `.diag` sidecar grammar;
//! * [`parse_expect`] for the `.expect` forward-table grammar;
//! * [`assert_diags`], the **set-based** diagnostic comparison (see below);
//! * [`load_program`] which parses a `.thse` file, panicking with context.
//!
//! # Set-based comparison ([`assert_diags`])
//!
//! Per `docs/src/spec/test-spec-m1.md` §"Conformance file formats (v1)":
//!
//! * Every **expected** diagnostic block must be *matched* by at least one
//!   reported diagnostic. A reported diagnostic matches an expected block when
//!   their [`Code`]s are equal **and** every payload key *listed* in the
//!   expected block is equal in the reported one (`witness` compared as a
//!   value; `clauses` compared as a set; `side`/`variable`/`name` by equality).
//!   Keys *not* listed by the expected block are unconstrained.
//! * **Extra** reported diagnostics (not matching any expected block) are
//!   tolerated only if they share the [`Code`] of some expected block — and,
//!   for side-carrying codes, also the [`Side`]. Any other extra is a failure.

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use theseus_core::prim::Value;
use theseus_syntax::ast::Program;

use crate::diagnostics::{Code, Diagnostic, Side};

/// An expected diagnostic parsed from a `.diag` block. Only the keys that were
/// present are `Some`; absent keys are unconstrained during comparison.
#[derive(Debug, Clone, PartialEq)]
pub struct ExpectedDiag {
    pub code: Code,
    pub side: Option<Side>,
    pub witness: Option<Value>,
    pub clauses: Option<Vec<usize>>,
    pub variable: Option<String>,
    pub name: Option<String>,
}

/// The repository root (two directories up from this crate's manifest dir).
pub fn repo_root() -> PathBuf {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // crates/theseus-check → crates → <root>
    manifest
        .parent()
        .and_then(Path::parent)
        .expect("crate manifest dir has a grandparent")
        .to_path_buf()
}

/// Collect `conformance/programs/*.thse` fixture paths, sorted.
pub fn program_fixtures() -> Vec<PathBuf> {
    collect_with_ext(&repo_root().join("conformance/programs"), "thse")
}

/// Collect `conformance/diagnostics/*.thse` fixture paths, sorted.
pub fn diagnostic_fixtures() -> Vec<PathBuf> {
    collect_with_ext(&repo_root().join("conformance/diagnostics"), "thse")
}

fn collect_with_ext(dir: &Path, ext: &str) -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = match fs::read_dir(dir) {
        Ok(rd) => rd
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| p.extension().and_then(|s| s.to_str()) == Some(ext))
            .collect(),
        Err(_) => Vec::new(),
    };
    out.sort();
    out
}

/// Parse a `.thse` source file into a [`Program`], panicking with context.
pub fn load_program(path: &Path) -> Program {
    let src = fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
    theseus_syntax::parse(&src)
        .unwrap_or_else(|e| panic!("parsing {}: {e:?}", path.display()))
}

/// Parse a `.diag` sidecar into expected diagnostic blocks.
///
/// Grammar (one block per `error:` line, keys indented/consecutive):
/// ```text
/// error: T0101
/// side: left
/// witness: inl unit
/// clauses: 1, 2
/// variable: x
/// name: Foo
/// ```
/// Blank lines and `#` comments are ignored. A new `error:` starts a new block.
pub fn parse_diag(text: &str) -> Vec<ExpectedDiag> {
    let mut blocks: Vec<ExpectedDiag> = Vec::new();
    for raw in text.lines() {
        let line = strip_comment(raw).trim();
        if line.is_empty() {
            continue;
        }
        let Some((key, value)) = line.split_once(':') else {
            continue;
        };
        let key = key.trim();
        let value = value.trim();
        if key == "error" {
            blocks.push(ExpectedDiag {
                code: parse_code(value),
                side: None,
                witness: None,
                clauses: None,
                variable: None,
                name: None,
            });
            continue;
        }
        let block = blocks
            .last_mut()
            .expect("diag key line before any `error:` line");
        match key {
            "side" => block.side = Some(parse_side(value)),
            "witness" => block.witness = Some(parse_value(value)),
            "clauses" => block.clauses = Some(parse_clauses(value)),
            "variable" => block.variable = Some(value.to_string()),
            "name" => block.name = Some(value.to_string()),
            other => panic!("unknown .diag key: {other}"),
        }
    }
    blocks
}

/// Parse a `.expect` forward table: an iso name followed by `in => out` rows.
///
/// ```text
/// iso: not
/// inl unit => inr unit
/// inr unit => inl unit
/// ```
/// Returns `(iso_name, rows)` where each row is `(input, output)`.
pub fn parse_expect(text: &str) -> (String, Vec<(Value, Value)>) {
    let mut name = String::new();
    let mut rows: Vec<(Value, Value)> = Vec::new();
    for raw in text.lines() {
        let line = strip_comment(raw).trim();
        if line.is_empty() {
            continue;
        }
        if let Some(rest) = line.strip_prefix("iso:") {
            name = rest.trim().to_string();
            continue;
        }
        if let Some((lhs, rhs)) = line.split_once("=>") {
            rows.push((parse_value(lhs.trim()), parse_value(rhs.trim())));
        }
    }
    (name, rows)
}

/// Assert reported diagnostics satisfy the expected set (see module docs).
pub fn assert_diags(reported: &[Diagnostic], expected: &[ExpectedDiag]) {
    // 1. Every expected block must be matched by some reported diagnostic.
    for (i, exp) in expected.iter().enumerate() {
        let matched = reported.iter().any(|r| matches_expected(r, exp));
        assert!(
            matched,
            "expected diagnostic #{i} unmatched: {exp:?}\nreported: {reported:?}"
        );
    }
    // 2. Extra reported diagnostics tolerated only if they share code (and, for
    //    side-carrying codes, side) with some expected block.
    for r in reported {
        if expected.iter().any(|e| matches_expected(r, e)) {
            continue;
        }
        let tolerated = expected.iter().any(|e| {
            e.code == r.code && (e.side.is_none() || r.side.is_none() || e.side == r.side)
        });
        assert!(
            tolerated,
            "unexpected diagnostic not sharing any expected code/side: {r:?}\nexpected: {expected:?}"
        );
    }
}

/// Does a reported diagnostic match an expected block? Code must be equal;
/// every *listed* key in `exp` must be equal (witness as value, clauses as set).
fn matches_expected(r: &Diagnostic, exp: &ExpectedDiag) -> bool {
    if r.code != exp.code {
        return false;
    }
    if let Some(s) = exp.side {
        if r.side != Some(s) {
            return false;
        }
    }
    if let Some(w) = &exp.witness {
        if r.witness.as_ref() != Some(w) {
            return false;
        }
    }
    if let Some(c) = &exp.clauses {
        let want: BTreeSet<usize> = c.iter().copied().collect();
        let got: BTreeSet<usize> = match &r.clauses {
            Some(v) => v.iter().copied().collect(),
            None => return false,
        };
        if want != got {
            return false;
        }
    }
    if let Some(v) = &exp.variable {
        if r.variable.as_deref() != Some(v.as_str()) {
            return false;
        }
    }
    if let Some(n) = &exp.name {
        if r.name.as_deref() != Some(n.as_str()) {
            return false;
        }
    }
    true
}

fn strip_comment(line: &str) -> &str {
    match line.split_once('#') {
        Some((before, _)) => before,
        None => line,
    }
}

fn parse_code(s: &str) -> Code {
    match s {
        "T0101" => Code::T0101,
        "T0102" => Code::T0102,
        "T0201" => Code::T0201,
        "T0202" => Code::T0202,
        "T0301" => Code::T0301,
        "T0302" => Code::T0302,
        other => panic!("unknown diagnostic code: {other}"),
    }
}

fn parse_side(s: &str) -> Side {
    match s.to_ascii_lowercase().as_str() {
        "left" | "l" => Side::Left,
        "right" | "r" => Side::Right,
        other => panic!("unknown side: {other}"),
    }
}

fn parse_clauses(s: &str) -> Vec<usize> {
    s.split(|c: char| c == ',' || c.is_whitespace())
        .filter(|t| !t.is_empty())
        .map(|t| t.parse().unwrap_or_else(|_| panic!("bad clause index: {t}")))
        .collect()
}

/// Parse a witness/expect value: `unit`, `inl V`, `inr V`, `(V, V)`, `pair V V`.
fn parse_value(s: &str) -> Value {
    let mut tokens = tokenize_value(s);
    let v = parse_value_tokens(&mut tokens);
    assert!(tokens.is_empty(), "trailing tokens parsing value: {s:?}");
    v
}

fn tokenize_value(s: &str) -> Vec<String> {
    let mut toks = Vec::new();
    for ch in s.chars() {
        match ch {
            '(' | ')' | ',' => toks.push(ch.to_string()),
            c if c.is_whitespace() => {}
            c => {
                if let Some(last) = toks.last_mut() {
                    if last.chars().all(|x| x.is_alphanumeric() || x == '_') {
                        last.push(c);
                        continue;
                    }
                }
                toks.push(c.to_string());
            }
        }
    }
    toks
}

fn parse_value_tokens(tokens: &mut Vec<String>) -> Value {
    assert!(!tokens.is_empty(), "empty value");
    let head = tokens.remove(0);
    match head.as_str() {
        "unit" => Value::Unit,
        "inl" => Value::left(parse_value_tokens(tokens)),
        "inr" => Value::right(parse_value_tokens(tokens)),
        "pair" => {
            let a = parse_value_tokens(tokens);
            let b = parse_value_tokens(tokens);
            Value::pair(a, b)
        }
        "(" => {
            let a = parse_value_tokens(tokens);
            assert_eq!(tokens.remove(0), ",", "expected ',' in pair value");
            let b = parse_value_tokens(tokens);
            assert_eq!(tokens.remove(0), ")", "expected ')' closing pair value");
            Value::pair(a, b)
        }
        other => panic!("unknown value token: {other}"),
    }
}
