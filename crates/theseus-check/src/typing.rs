//! Typing check (T0102).
//!
//! For each iso with clauses, checks that every pattern in each clause is
//! well-typed against the iso's declared domain (lhs) or codomain (rhs).
//! A constructor `C p*` is well-typed at type `T` iff `C` is a constructor of
//! `T` (via `env`) and its argument patterns match `C`'s payload type; a tuple
//! pattern `(p1, p2)` against a product type `A * B`; a var pattern against any
//! type; a `Nat` literal against a `U<n>` type. Mismatches are `T0102` with the
//! offending `side:` and 1-indexed `clauses:`.

use theseus_syntax::ast::{Decl, IsoBody, IsoDecl, Pattern, Program, Type as SurfType};

use crate::diagnostics::{Code, Diagnostic, Side};
use crate::env::Env;

/// Run the typing check over a whole program.
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
        for (i, clause) in clauses.iter().enumerate() {
            let clause_index = i + 1; // 1-indexed
            check_pattern(&clause.lhs, &id.lhs, env, Side::Left, clause_index, diags);
            check_pattern(&clause.rhs, &id.rhs, env, Side::Right, clause_index, diags);
        }
    }
    for w in &id.wheres {
        check_iso(w, env, diags);
    }
}

/// Check that `pat` is well-typed at surface type `ty`. If not, push a T0102.
fn check_pattern(
    pat: &Pattern,
    ty: &SurfType,
    env: &Env,
    side: Side,
    clause_index: usize,
    diags: &mut Vec<Diagnostic>,
) {
    // Unwrap Paren on the type side.
    let ty = peel_paren_type(ty);

    match pat {
        Pattern::Var(_) => {
            // Variables match any type.
        }
        Pattern::Nat(_, _) => {
            // Nat literals are well-typed only at U<n> types.
            if !is_un_type(ty) {
                diags.push(
                    Diagnostic::new(Code::T0102, pat.span())
                        .with_side(side)
                        .with_clauses(vec![clause_index]),
                );
            }
        }
        Pattern::Label(..) => {
            // Labels are out of scope for typing in slice 1.
        }
        Pattern::Ctor(name, args, _) => {
            let Some(ctor_info) = env.ctor(&name.text) else {
                // Unknown ctor — Names (T0101) already reported this; skip.
                return;
            };
            // The ctor must belong to the type at this position.
            if !type_admits_ctor(ty, &ctor_info.type_name, env) {
                diags.push(
                    Diagnostic::new(Code::T0102, name.span)
                        .with_side(side)
                        .with_clauses(vec![clause_index]),
                );
                return;
            }
            // Recursively check the ctor's arguments against its payload type.
            match (&ctor_info.payload, args.as_slice()) {
                (None, []) => {
                    // Nullary ctor applied to no args — fine.
                }
                (Some(payload_ty), [arg]) => {
                    check_pattern(arg, payload_ty, env, side, clause_index, diags);
                }
                (Some(payload_ty), args) if args.len() > 1 => {
                    // Multiple args — the ctor's payload is curried. The first
                    // arg matches the outermost component and remaining args
                    // match deeper levels. For ADTs in Theseus, a multi-arg
                    // ctor `C a b` is sugar for `C (a, b)` — a single product
                    // payload. Check each arg against its position in the product.
                    check_curried_args(args, payload_ty, env, side, clause_index, diags);
                }
                _ => {
                    // Arity mismatch (e.g. nullary ctor given args, or
                    // ctor expecting payload given none). Report T0102.
                    diags.push(
                        Diagnostic::new(Code::T0102, name.span)
                            .with_side(side)
                            .with_clauses(vec![clause_index]),
                    );
                }
            }
        }
        Pattern::Tuple(a, b, _) => {
            // A tuple pattern is well-typed against a product type.
            if let SurfType::Prod(ty_a, ty_b, _) = ty {
                check_pattern(a, ty_a, env, side, clause_index, diags);
                check_pattern(b, ty_b, env, side, clause_index, diags);
            } else {
                diags.push(
                    Diagnostic::new(Code::T0102, pat.span())
                        .with_side(side)
                        .with_clauses(vec![clause_index]),
                );
            }
        }
        Pattern::Paren(inner, _) => {
            check_pattern(inner, ty, env, side, clause_index, diags);
        }
    }
}

/// Check curried constructor arguments against a product-typed payload.
/// `C a b c` with payload `A * B * C` (left-assoc) checks `a` against `A`,
/// `b` against `B`, `c` against `C`.
fn check_curried_args(
    args: &[Pattern],
    ty: &SurfType,
    env: &Env,
    side: Side,
    clause_index: usize,
    diags: &mut Vec<Diagnostic>,
) {
    let ty = peel_paren_type(ty);
    if args.len() == 1 {
        check_pattern(&args[0], ty, env, side, clause_index, diags);
        return;
    }
    if let SurfType::Prod(ty_a, ty_b, _) = ty {
        check_pattern(&args[0], ty_a, env, side, clause_index, diags);
        check_curried_args(&args[1..], ty_b, env, side, clause_index, diags);
    } else {
        // More args than product components — arity error.
        diags.push(
            Diagnostic::new(Code::T0102, args[0].span())
                .with_side(side)
                .with_clauses(vec![clause_index]),
        );
    }
}

/// Does the surface type `ty` admit a constructor belonging to `type_name`?
fn type_admits_ctor(ty: &SurfType, type_name: &str, env: &Env) -> bool {
    let ty = peel_paren_type(ty);
    match ty {
        SurfType::Name(n) => n.text == type_name,
        SurfType::Sum(a, b, _) => {
            type_admits_ctor(a, type_name, env) || type_admits_ctor(b, type_name, env)
        }
        _ => false,
    }
}

/// Is this surface type a `U<n>` builtin numeric type?
fn is_un_type(ty: &SurfType) -> bool {
    let ty = peel_paren_type(ty);
    matches!(ty, SurfType::Name(n) if n.text.strip_prefix('U').is_some_and(|d| !d.is_empty() && d.bytes().all(|b| b.is_ascii_digit())))
}

/// Peel off any Paren wrappers on a surface type.
fn peel_paren_type(ty: &SurfType) -> &SurfType {
    match ty {
        SurfType::Paren(inner, _) => peel_paren_type(inner),
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testkit;

    /// Filter diagnostics to just T0102.
    fn typing_only(diags: Vec<Diagnostic>) -> Vec<Diagnostic> {
        diags.into_iter().filter(|d| d.code == Code::T0102).collect()
    }

    #[test]
    fn type_mismatch_reports_t0102() {
        let root = testkit::repo_root();
        let prog_path = root.join("conformance/diagnostics/0005-type-mismatch.thse");
        let diag_path = prog_path.with_extension("diag");

        let program = testkit::load_program(&prog_path);
        let env = crate::env::resolve(&program);
        let reported = typing_only(check(&program, &env));

        let expected_text = std::fs::read_to_string(&diag_path)
            .unwrap_or_else(|e| panic!("reading {}: {e}", diag_path.display()));
        let expected = testkit::parse_diag(&expected_text);
        testkit::assert_diags(&reported, &expected);
    }

    #[test]
    fn no_t0102_on_valid_programs() {
        for path in testkit::program_fixtures() {
            let file = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
            if file == "0011-label-loop.thse" {
                continue; // label test — out of slice-1 scope
            }
            let program = testkit::load_program(&path);
            let env = crate::env::resolve(&program);
            let reported = typing_only(check(&program, &env));
            assert!(
                reported.is_empty(),
                "unexpected T0102 in {}: {reported:?}",
                path.display()
            );
        }
    }
}
