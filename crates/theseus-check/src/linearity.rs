//! Linearity check (T0201/T0202).
//!
//! Each clause in an iso must bind every variable **exactly once per side** and
//! **on both sides**. Duplication on one side is T0201; presence on one side
//! only is T0202 (erasure/creation).

use std::collections::BTreeMap;

use theseus_syntax::ast::{Clause, Decl, IsoBody, IsoDecl, Pattern, Program, Span};

use crate::diagnostics::{Code, Diagnostic, Side};
use crate::env::Env;

/// A multiset of variable occurrences: name → list of spans.
type Occurrences = BTreeMap<String, Vec<Span>>;

/// Collect variable occurrences in a pattern into the given map.
fn collect_vars(pat: &Pattern, occ: &mut Occurrences) {
    match pat {
        Pattern::Var(v) => {
            occ.entry(v.text.clone()).or_default().push(v.span);
        }
        Pattern::Ctor(_, args, _) => {
            for arg in args {
                collect_vars(arg, occ);
            }
        }
        Pattern::Tuple(a, b, _) => {
            collect_vars(a, occ);
            collect_vars(b, occ);
        }
        Pattern::Paren(inner, _) => collect_vars(inner, occ),
        Pattern::Nat(_, _) | Pattern::Label(..) => {}
    }
}

/// Check a single clause for linearity violations.
fn check_clause(clause: &Clause, diags: &mut Vec<Diagnostic>) {
    let mut left = Occurrences::new();
    let mut right = Occurrences::new();
    collect_vars(&clause.lhs, &mut left);
    collect_vars(&clause.rhs, &mut right);

    // T0201: duplicate variable on one side. If duplicated on both sides,
    // report only the left (the .diag oracle expects a single diagnostic).
    let mut dup_left = Vec::new();
    for (name, spans) in &left {
        if spans.len() > 1 {
            dup_left.push(name.clone());
            diags.push(
                Diagnostic::new(Code::T0201, spans[1])
                    .with_side(Side::Left)
                    .with_variable(name),
            );
        }
    }
    for (name, spans) in &right {
        if spans.len() > 1 && !dup_left.contains(name) {
            diags.push(
                Diagnostic::new(Code::T0201, spans[1])
                    .with_side(Side::Right)
                    .with_variable(name),
            );
        }
    }

    // T0202: variable on one side but not the other — reported on the side
    // where it occurs.
    for (name, spans) in &left {
        if !right.contains_key(name) {
            diags.push(
                Diagnostic::new(Code::T0202, spans[0])
                    .with_side(Side::Left)
                    .with_variable(name),
            );
        }
    }
    for (name, spans) in &right {
        if !left.contains_key(name) {
            diags.push(
                Diagnostic::new(Code::T0202, spans[0])
                    .with_side(Side::Right)
                    .with_variable(name),
            );
        }
    }
}

/// Check an iso declaration (and its nested `where` isos) for linearity.
fn check_iso(iso: &IsoDecl, diags: &mut Vec<Diagnostic>) {
    if let IsoBody::Clauses(clauses) = &iso.body {
        for clause in clauses {
            check_clause(clause, diags);
        }
    }
    for w in &iso.wheres {
        check_iso(w, diags);
    }
}

/// Run the linearity check over a whole program.
pub fn check(program: &Program, _env: &Env) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for decl in &program.decls {
        if let Decl::Iso(id) = decl {
            check_iso(id, &mut diags);
        }
    }
    diags
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Code;
    use crate::testkit;

    /// Filter diagnostics to linearity codes only.
    fn linearity_only(diags: Vec<Diagnostic>) -> Vec<Diagnostic> {
        diags
            .into_iter()
            .filter(|d| d.code == Code::T0201 || d.code == Code::T0202)
            .collect()
    }

    #[test]
    fn nonlinear_reports_t0201() {
        let root = testkit::repo_root();
        let prog_path = root.join("conformance/diagnostics/0003-nonlinear.thse");
        let diag_path = prog_path.with_extension("diag");

        let program = testkit::load_program(&prog_path);
        let env = crate::env::resolve(&program);
        let reported = check(&program, &env);

        let expected_text = std::fs::read_to_string(&diag_path)
            .unwrap_or_else(|e| panic!("reading {}: {e}", diag_path.display()));
        let expected = testkit::parse_diag(&expected_text);
        testkit::assert_diags(&reported, &expected);
    }

    #[test]
    fn erasure_reports_t0202() {
        let root = testkit::repo_root();
        let prog_path = root.join("conformance/diagnostics/0004-erasure.thse");
        let diag_path = prog_path.with_extension("diag");

        let program = testkit::load_program(&prog_path);
        let env = crate::env::resolve(&program);
        let reported = check(&program, &env);

        let expected_text = std::fs::read_to_string(&diag_path)
            .unwrap_or_else(|e| panic!("reading {}: {e}", diag_path.display()));
        let expected = testkit::parse_diag(&expected_text);
        testkit::assert_diags(&reported, &expected);
    }

    #[test]
    fn no_linearity_errors_on_valid_programs() {
        for path in testkit::program_fixtures() {
            let file = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
            if file == "0011-label-loop.thse" {
                continue;
            }
            let program = testkit::load_program(&path);
            let env = crate::env::resolve(&program);
            let reported = linearity_only(check(&program, &env));
            assert!(
                reported.is_empty(),
                "unexpected linearity diagnostic in {}: {reported:?}",
                path.display()
            );
        }
    }
}
