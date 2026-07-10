//! Coverage check (T0301/T0302).
//!
//! For each side of each iso with clause tables, enumerates all values of
//! the side's core type and verifies every value is matched by exactly one
//! clause. Values matched by no clause yield T0301; values matched by more
//! than one clause yield T0302.

use theseus_core::prim::{enumerate_values, Value};
use theseus_syntax::ast::{Clause, Decl, IsoBody, IsoDecl, Pattern, Program, Span, Type as SurfType};

use crate::diagnostics::{Code, Diagnostic, Side};
use crate::env::Env;

/// Run the coverage check over a whole program.
pub fn check(program: &Program, env: &Env) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for decl in &program.decls {
        if let Decl::Iso(id) = decl {
            check_iso(id, env, &mut diags);
        }
    }
    diags
}

fn check_iso(id: &IsoDecl, env: &Env, diags: &mut Vec<Diagnostic>) {
    if let IsoBody::Clauses(clauses) = &id.body {
        check_side(clauses, &id.lhs, Side::Left, id.span, env, diags);
        check_side(clauses, &id.rhs, Side::Right, id.span, env, diags);
    }
    for w in &id.wheres {
        check_iso(w, env, diags);
    }
}

fn check_side(
    clauses: &[Clause],
    surf_ty: &SurfType,
    side: Side,
    span: Span,
    env: &Env,
    diags: &mut Vec<Diagnostic>,
) {
    let core_ty = env.lower(surf_ty);
    let values = enumerate_values(&core_ty);

    let mut found_t0301 = false;
    let mut found_t0302 = false;

    for value in &values {
        let matching: Vec<usize> = clauses
            .iter()
            .enumerate()
            .filter(|(_, clause)| {
                let pat = match side {
                    Side::Left => &clause.lhs,
                    Side::Right => &clause.rhs,
                };
                matches_value(value, pat, surf_ty, env)
            })
            .map(|(i, _)| i + 1)
            .collect();

        if matching.is_empty() && !found_t0301 {
            found_t0301 = true;
            diags.push(
                Diagnostic::new(Code::T0301, span)
                    .with_side(side)
                    .with_witness(value.clone()),
            );
        }

        if matching.len() > 1 && !found_t0302 {
            found_t0302 = true;
            diags.push(
                Diagnostic::new(Code::T0302, span)
                    .with_side(side)
                    .with_witness(value.clone())
                    .with_clauses(matching),
            );
        }
    }
}

/// Does a core `Value` match a surface `Pattern` given the env's ctor→injection
/// mapping? The surface type `ty` is threaded through to resolve `Nat` literals.
fn matches_value(value: &Value, pattern: &Pattern, ty: &SurfType, env: &Env) -> bool {
    let ty = peel_paren_type(ty);
    match pattern {
        Pattern::Var(_) => true,
        Pattern::Paren(inner, _) => matches_value(value, inner, ty, env),
        Pattern::Tuple(a, b, _) => match (value, ty) {
            (Value::Pair(va, vb), SurfType::Prod(ta, tb, _)) => {
                matches_value(va, a, ta, env) && matches_value(vb, b, tb, env)
            }
            _ => false,
        },
        Pattern::Ctor(name, args, _) => {
            let Some(info) = env.ctor(&name.text) else {
                return false;
            };
            let Some(payload) = navigate(value, &info.path) else {
                return false;
            };
            match_args(payload, args, info.payload.as_ref(), env)
        }
        Pattern::Nat(k, _) => {
            // The k-th enumerated value of the type at this position.
            let core_ty = env.lower(ty);
            let values = enumerate_values(&core_ty);
            let k = *k as usize;
            k < values.len() && *value == values[k]
        }
        // Labels are slice-2; not checked.
        Pattern::Label(..) => false,
    }
}

/// Navigate a value through an injection path. Each `L` expects `Left`,
/// each `R` expects `Right`. Returns the payload at the end, or `None` on
/// mismatch.
fn navigate<'a>(value: &'a Value, path: &[crate::env::Inj]) -> Option<&'a Value> {
    let mut v = value;
    for step in path {
        match (step, v) {
            (crate::env::Inj::L, Value::Left(inner)) => v = inner,
            (crate::env::Inj::R, Value::Right(inner)) => v = inner,
            _ => return None,
        }
    }
    Some(v)
}

/// Match a payload value against a constructor's argument patterns.
/// Zero args means nullary (payload is Unit, nothing to check).
/// One arg matches directly. Multiple args match a right-nested product.
fn match_args(value: &Value, args: &[Pattern], payload_ty: Option<&SurfType>, env: &Env) -> bool {
    match args {
        [] => true,
        [single] => {
            if let Some(ty) = payload_ty {
                matches_value(value, single, ty, env)
            } else {
                // Nullary ctor with no payload type — structural match only.
                pattern_is_var(single)
            }
        }
        [first, rest @ ..] => match value {
            Value::Pair(l, r) => {
                if let Some(ty) = payload_ty {
                    let ty = peel_paren_type(ty);
                    if let SurfType::Prod(ta, tb, _) = ty {
                        matches_value(l, first, ta, env)
                            && match_args(r, rest, Some(tb), env)
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => false,
        },
    }
}

fn pattern_is_var(p: &Pattern) -> bool {
    matches!(strip_paren_pat(p), Pattern::Var(_))
}

fn strip_paren_pat(p: &Pattern) -> &Pattern {
    match p {
        Pattern::Paren(inner, _) => strip_paren_pat(inner),
        _ => p,
    }
}

fn peel_paren_type(ty: &SurfType) -> &SurfType {
    match ty {
        SurfType::Paren(inner, _) => peel_paren_type(inner),
        _ => ty,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testkit;
    use crate::testkit::ExpectedDiag;

    #[test]
    fn nonexhaustive_0001() {
        let root = testkit::repo_root();
        let path = root.join("conformance/diagnostics/0001-nonexhaustive.thse");
        let program = testkit::load_program(&path);
        let env = crate::env::resolve(&program);
        let reported = check(&program, &env);

        // MaybeBit = Nothing | Just Bit  →  1 + (1 + 1)
        // Just I  →  inr (inr unit)
        let witness = Value::right(Value::right(Value::Unit));
        let expected = vec![
            ExpectedDiag {
                code: Code::T0301,
                side: Some(Side::Left),
                witness: Some(witness.clone()),
                clauses: None,
                variable: None,
                name: None,
            },
            ExpectedDiag {
                code: Code::T0301,
                side: Some(Side::Right),
                witness: Some(witness),
                clauses: None,
                variable: None,
                name: None,
            },
        ];
        testkit::assert_diags(&reported, &expected);
    }

    #[test]
    fn overlap_0002() {
        let root = testkit::repo_root();
        let path = root.join("conformance/diagnostics/0002-overlap.thse");
        let program = testkit::load_program(&path);
        let env = crate::env::resolve(&program);
        let reported = check(&program, &env);

        // MaybeBit = Nothing | Just Bit  →  1 + (1 + 1)
        // Just O  →  inr (inl unit)
        let witness = Value::right(Value::left(Value::Unit));
        let expected = vec![
            ExpectedDiag {
                code: Code::T0302,
                side: Some(Side::Left),
                witness: Some(witness.clone()),
                clauses: Some(vec![1, 2]),
                variable: None,
                name: None,
            },
            ExpectedDiag {
                code: Code::T0302,
                side: Some(Side::Right),
                witness: Some(witness),
                clauses: Some(vec![1, 2]),
                variable: None,
                name: None,
            },
        ];
        testkit::assert_diags(&reported, &expected);
    }

    #[test]
    fn differential_no_coverage_errors_on_valid_programs() {
        for path in testkit::program_fixtures() {
            let file = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
            if file == "0011-label-loop.thse" || file == "0012-prim.thse" {
                continue;
            }
            let program = testkit::load_program(&path);
            let env = crate::env::resolve(&program);
            let reported: Vec<Diagnostic> = check(&program, &env)
                .into_iter()
                .filter(|d| d.code == Code::T0301 || d.code == Code::T0302)
                .collect();
            assert!(
                reported.is_empty(),
                "unexpected coverage error in {}: {reported:?}",
                path.display(),
            );
        }
    }
}
