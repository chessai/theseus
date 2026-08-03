//! Names check (T0101).
//!
//! Reports a [`Code::T0101`] diagnostic for every *unresolved reference*:
//! an unknown constructor in a pattern, an unknown type name in a signature or
//! constructor payload, an unknown iso/prim reference, or an unknown iso name
//! used in an `= expr` body. Variable patterns are *binders*, not references,
//! so they are always resolved.
//!
//! The check reads the [`Env`] built by [`crate::env::resolve`]; it never
//! mutates it.

use theseus_syntax::ast::{Clause, Decl, Expr, IsoBody, IsoDecl, Pattern, Program, Type as SurfType};

use crate::diagnostics::{Code, Diagnostic, Side};
use crate::env::Env;

/// Run the Names check over a whole program.
pub fn check(program: &Program, env: &Env) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for decl in &program.decls {
        match decl {
            Decl::Type(td) => {
                for ctor in &td.ctors {
                    if let Some(payload) = &ctor.payload {
                        check_type_refs(payload, env, None, &mut diags);
                    }
                }
            }
            Decl::Iso(id) => check_iso(id, env, &mut diags),
            Decl::Prim(pd) => {
                check_type_refs(&pd.lhs, env, Some(Side::Left), &mut diags);
                check_type_refs(&pd.rhs, env, Some(Side::Right), &mut diags);
            }
        }
    }
    diags
}

fn check_iso(id: &IsoDecl, env: &Env, diags: &mut Vec<Diagnostic>) {
    check_type_refs(&id.lhs, env, Some(Side::Left), diags);
    check_type_refs(&id.rhs, env, Some(Side::Right), diags);
    match &id.body {
        IsoBody::Clauses(clauses) => {
            for clause in clauses {
                check_clause(clause, env, diags);
            }
        }
        IsoBody::Expr(e) => {
            // An `= expr` body references other isos/prims by name; a reference
            // resolves against the top-level isos/prims OR this iso's own
            // `where`-bound isos (in scope for its body).
            let local: Vec<&str> = id.wheres.iter().map(|w| w.name.text.as_str()).collect();
            check_expr(e, env, &local, diags);
        }
    }
    for w in &id.wheres {
        check_iso(w, env, diags);
    }
}

/// Walk an `= expr` combinator body; report `T0101` for an unknown iso/prim
/// reference (`Expr::Var`) or an unknown constructor used as a combinator
/// (`Expr::Ctor`). `local` holds the in-scope `where`-bound iso names.
fn check_expr(e: &Expr, env: &Env, local: &[&str], diags: &mut Vec<Diagnostic>) {
    match e {
        Expr::Var(n) => {
            if !env.has_iso(&n.text) && !env.has_prim(&n.text) && !local.contains(&n.text.as_str()) {
                diags.push(Diagnostic::new(Code::T0101, n.span).with_name(n.text.to_string()));
            }
        }
        Expr::Ctor(n) => {
            if !env.has_ctor(&n.text) {
                diags.push(Diagnostic::new(Code::T0101, n.span).with_name(n.text.to_string()));
            }
        }
        Expr::Adjoint(inner, _) | Expr::Paren(inner, _) => check_expr(inner, env, local, diags),
        Expr::Bin(_, l, r, _) | Expr::Tuple(l, r, _) => {
            check_expr(l, env, local, diags);
            check_expr(r, env, local, diags);
        }
        Expr::Combinator(_, ty, body, _) => {
            // The counter/spine TYPE argument is an ordinary type reference
            // (ADR-0038); the body is an ordinary iso expression.
            check_type_refs(ty, env, None, diags);
            check_expr(body, env, local, diags);
        }
        Expr::Id(_) | Expr::Unit(_) => {}
    }
}

fn check_clause(clause: &Clause, env: &Env, diags: &mut Vec<Diagnostic>) {
    check_pattern(&clause.lhs, env, Side::Left, diags);
    check_pattern(&clause.rhs, env, Side::Right, diags);
}

/// Walk a pattern; report unknown constructors. Variables are binders.
fn check_pattern(pat: &Pattern, env: &Env, side: Side, diags: &mut Vec<Diagnostic>) {
    match pat {
        Pattern::Var(_) => {}
        Pattern::Nat(_, _) => {}
        Pattern::Label(..) => {}
        Pattern::Ctor(name, args, _) => {
            if !env.has_ctor(&name.text) {
                diags.push(
                    Diagnostic::new(Code::T0101, name.span)
                        .with_side(side)
                        .with_name(name.text.to_string()),
                );
            }
            for inner in args {
                check_pattern(inner, env, side, diags);
            }
        }
        Pattern::Tuple(a, b, _) => {
            check_pattern(a, env, side, diags);
            check_pattern(b, env, side, diags);
        }
        Pattern::Paren(inner, _) => check_pattern(inner, env, side, diags),
    }
}

/// Walk a surface type; report unknown named type references.
fn check_type_refs(ty: &SurfType, env: &Env, side: Option<Side>, diags: &mut Vec<Diagnostic>) {
    match ty {
        SurfType::Empty(_) | SurfType::Unit(_) | SurfType::Var(_) => {}
        SurfType::Name(n) => {
            if !env.has_type(&n.text) {
                let mut d =
                    Diagnostic::new(Code::T0101, n.span).with_name(n.text.to_string());
                if let Some(s) = side {
                    d = d.with_side(s);
                }
                diags.push(d);
            }
        }
        SurfType::Paren(inner, _) => check_type_refs(inner, env, side, diags),
        SurfType::Prod(a, b, _) | SurfType::Sum(a, b, _) => {
            check_type_refs(a, env, side, diags);
            check_type_refs(b, env, side, diags);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testkit;

    /// Filter diagnostics to just T0101.
    fn names_only(diags: Vec<Diagnostic>) -> Vec<Diagnostic> {
        diags.into_iter().filter(|d| d.code == Code::T0101).collect()
    }

    #[test]
    fn unknown_ctor_reports_t0101_with_name() {
        let root = testkit::repo_root();
        let prog_path = root.join("conformance/diagnostics/0006-unknown-ctor.thse");
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
    fn no_t0101_on_valid_programs() {
        for path in testkit::program_fixtures() {
            let file = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
            if file == "0011-label-loop.thse" {
                continue; // label test -- out of slice-1 scope
            }
            let program = testkit::load_program(&path);
            let env = crate::env::resolve(&program);
            let reported = names_only(check(&program, &env));
            assert!(
                reported.is_empty(),
                "unexpected T0101 in {}: {reported:?}",
                path.display()
            );
        }
    }
}
