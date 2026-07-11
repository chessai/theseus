//! Coverage check (T0301/T0302/T0303).
//!
//! For each side of each iso with clause tables, enumerates all values of
//! the side's core type and verifies every value is matched by exactly one
//! clause. Values matched by no clause yield T0301 (boundary) or T0303
//! (label state); values matched by more than one clause yield T0302.

use theseus_core::prim::{enumerate_values, Value};
use theseus_core::ty::Type as CoreType;
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
        let labels = env.iso_labels(&id.name.text);
        if labels.is_empty() {
            // Plain iso: check over boundary type directly.
            check_side_plain(clauses, &id.lhs, Side::Left, id.span, env, diags);
            check_side_plain(clauses, &id.rhs, Side::Right, id.span, env, diags);
        } else {
            // Label iso: check over extended type U + A / U + B.
            check_side_extended(clauses, &id.lhs, Side::Left, id.span, labels, env, diags);
            check_side_extended(clauses, &id.rhs, Side::Right, id.span, labels, env, diags);
        }
    }
    for w in &id.wheres {
        check_iso(w, env, diags);
    }
}

/// Build the label carrier core type U = Σ_ℓ U_ℓ as a right-nested sum.
fn build_label_carrier(labels: &[(String, SurfType)], env: &Env) -> CoreType {
    let summands: Vec<CoreType> = labels.iter().map(|(_, ty)| env.lower(ty)).collect();
    match summands.as_slice() {
        [] => CoreType::Zero,
        [last] => last.clone(),
        [head, rest @ ..] => {
            let mut acc = rest.last().unwrap().clone();
            for s in rest.iter().rev().skip(1) {
                acc = CoreType::Sum(Box::new(s.clone()), Box::new(acc));
            }
            CoreType::Sum(Box::new(head.clone()), Box::new(acc))
        }
    }
}

/// Plain side check (no labels) — original behavior.
fn check_side_plain(
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

/// Extended side check: enumerate values of U + boundary, route by top tag.
fn check_side_extended(
    clauses: &[Clause],
    surf_ty: &SurfType,
    side: Side,
    span: Span,
    labels: &[(String, SurfType)],
    env: &Env,
    diags: &mut Vec<Diagnostic>,
) {
    let boundary_core = env.lower(surf_ty);
    let carrier_core = build_label_carrier(labels, env);
    // Extended type: U + boundary (carrier left, boundary right).
    let extended = CoreType::Sum(Box::new(carrier_core), Box::new(boundary_core));
    let values = enumerate_values(&extended);

    let mut found_t0301 = false;
    let mut found_t0302 = false;
    let mut found_t0303 = false;

    for value in &values {
        let matching: Vec<usize> = clauses
            .iter()
            .enumerate()
            .filter(|(_, clause)| {
                let pat = match side {
                    Side::Left => &clause.lhs,
                    Side::Right => &clause.rhs,
                };
                matches_extended_value(value, pat, surf_ty, labels, env)
            })
            .map(|(i, _)| i + 1)
            .collect();

        if matching.is_empty() {
            match value {
                Value::Right(inner) => {
                    // Uncovered boundary value → T0301.
                    if !found_t0301 {
                        found_t0301 = true;
                        diags.push(
                            Diagnostic::new(Code::T0301, span)
                                .with_side(side)
                                .with_witness((**inner).clone()),
                        );
                    }
                }
                Value::Left(_) => {
                    // Uncovered label state → T0303.
                    if !found_t0303 {
                        found_t0303 = true;
                        diags.push(
                            Diagnostic::new(Code::T0303, span)
                                .with_side(side)
                                .with_witness(value.clone()),
                        );
                    }
                }
                _ => {}
            }
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

/// Match a value from the extended type (U + boundary) against a pattern.
///
/// A plain pattern matches only boundary values (`Right(a)`); a label pattern
/// `$ℓ args` matches only label-state values (`Left(inj_ℓ(payload))`).
fn matches_extended_value(
    value: &Value,
    pattern: &Pattern,
    boundary_surf: &SurfType,
    labels: &[(String, SurfType)],
    env: &Env,
) -> bool {
    let pattern = strip_paren_pat(pattern);
    match pattern {
        Pattern::Label(name, args, _) => {
            // Must be Left(...) — a label-state value.
            let Value::Left(carrier_val) = value else {
                return false;
            };
            // Find the label's index in the label list.
            let Some(label_idx) = labels.iter().position(|(l, _)| l == &name.text) else {
                return false;
            };
            // Navigate to the label_idx-th summand of the right-nested sum.
            let payload = navigate_sum(carrier_val, label_idx, labels.len());
            let Some(payload) = payload else {
                return false;
            };
            // Match args against payload, using the label's carrier type.
            let carrier_ty = &labels[label_idx].1;
            match_args(payload, args, Some(carrier_ty), env)
        }
        Pattern::Var(_) => {
            // A variable pattern in an extended-type context matches only
            // boundary values (Right), since label patterns use $ℓ.
            match value {
                Value::Right(inner) => {
                    // Variable matches any boundary value.
                    let _ = inner;
                    true
                }
                _ => false,
            }
        }
        _ => {
            // Any other plain pattern: matches only boundary values (Right).
            match value {
                Value::Right(inner) => matches_value(inner, pattern, boundary_surf, env),
                _ => false,
            }
        }
    }
}

/// Navigate to the `idx`-th summand of a right-nested sum of `count` summands.
/// Mirrors the injection path: R^idx then L (except the last summand which is R^(count-1)).
fn navigate_sum<'a>(value: &'a Value, idx: usize, count: usize) -> Option<&'a Value> {
    let mut v = value;
    for _ in 0..idx {
        match v {
            Value::Right(inner) => v = inner,
            _ => return None,
        }
    }
    if idx < count - 1 {
        // Not the last summand: expect Left.
        match v {
            Value::Left(inner) => Some(inner),
            _ => None,
        }
    } else {
        // Last summand: value is reached after all Rights.
        Some(v)
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
        // Label patterns in non-extended context: never match.
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
            if file == "0012-prim.thse" {
                continue;
            }
            let program = testkit::load_program(&path);
            let env = crate::env::resolve(&program);
            let reported: Vec<Diagnostic> = check(&program, &env)
                .into_iter()
                .filter(|d| {
                    d.code == Code::T0301
                        || d.code == Code::T0302
                        || d.code == Code::T0303
                })
                .collect();
            assert!(
                reported.is_empty(),
                "unexpected coverage error in {}: {reported:?}",
                path.display(),
            );
        }
    }

    #[test]
    fn label_coverage_stuck_0008() {
        let root = testkit::repo_root();
        let path = root.join("conformance/diagnostics/0008-label-coverage.thse");
        let program = testkit::load_program(&path);
        let env = crate::env::resolve(&program);
        let reported = check(&program, &env);

        // stuck: T0303 side:left (uncovered label state $go I)
        //        T0301 side:right (uncovered boundary I)
        let has_t0303_left = reported
            .iter()
            .any(|d| d.code == Code::T0303 && d.side == Some(Side::Left));
        let has_t0301_right = reported
            .iter()
            .any(|d| d.code == Code::T0301 && d.side == Some(Side::Right));
        assert!(
            has_t0303_left,
            "expected T0303 side:left in stuck, got: {reported:?}"
        );
        assert!(
            has_t0301_right,
            "expected T0301 side:right in stuck, got: {reported:?}"
        );
    }

    #[test]
    fn label_coverage_spin_0011() {
        let root = testkit::repo_root();
        let path = root.join("conformance/programs/0011-label-loop.thse");
        let program = testkit::load_program(&path);
        let env = crate::env::resolve(&program);
        let reported: Vec<Diagnostic> = check(&program, &env)
            .into_iter()
            .filter(|d| {
                d.code == Code::T0301
                    || d.code == Code::T0302
                    || d.code == Code::T0303
            })
            .collect();
        assert!(
            reported.is_empty(),
            "unexpected coverage error in spin: {reported:?}"
        );
    }
}
