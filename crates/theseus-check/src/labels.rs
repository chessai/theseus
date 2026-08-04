//! Label-type inference (M1 slice-2).
//!
//! [`infer_iso_labels`] walks an iso's clauses and infers the carrier type
//! `U_ℓ` for each label `$ℓ`, producing the ordered label state space.
//! Carrier-unification failures are returned as [`Clash`] values so the
//! typing stage can emit `T0102`.

use std::collections::HashMap;

use theseus_syntax::ast::{IsoBody, IsoDecl, Pattern, Span, Type as SurfType, UpperName};

use crate::diagnostics::Side;
use crate::env::Env;

/// A carrier-unification clash: the label `label` at `span` on `side`
/// conflicts with a previously inferred carrier.
#[derive(Debug, Clone)]
pub struct Clash {
    pub label: String,
    pub side: Side,
    pub span: Span,
}

/// Infer the label state space of an iso declaration.
///
/// Returns `(labels, clashes)` where `labels` is the ordered list of
/// `(label_name, carrier_type)` in first-occurrence order, and `clashes`
/// are carrier-unification failures.
pub fn infer_iso_labels(id: &IsoDecl, env: &Env) -> (Vec<(String, SurfType)>, Vec<Clash>) {
    let mut label_order: Vec<String> = Vec::new();
    let mut label_carriers: HashMap<String, SurfType> = HashMap::new();
    let mut clashes: Vec<Clash> = Vec::new();
    // Track the first occurrence (side, span) for each label so we can
    // report carrier-not-inferable labels with a meaningful location.
    let mut first_occurrence: HashMap<String, (Side, Span)> = HashMap::new();

    if let IsoBody::Clauses(clauses) = &id.body {
        for clause in clauses {
            // Collect variable types from non-label parts of both sides.
            let mut var_types: HashMap<String, SurfType> = HashMap::new();
            collect_var_types(&clause.lhs, &id.lhs, env, &mut var_types);
            collect_var_types(&clause.rhs, &id.rhs, env, &mut var_types);

            // Collect labels: LHS before RHS (canonical traversal order).
            collect_labels(
                &clause.lhs,
                &var_types,
                env,
                Side::Left,
                &mut label_order,
                &mut label_carriers,
                &mut clashes,
                &mut first_occurrence,
            );
            collect_labels(
                &clause.rhs,
                &var_types,
                env,
                Side::Right,
                &mut label_order,
                &mut label_carriers,
                &mut clashes,
                &mut first_occurrence,
            );
        }
    }

    // H1: labels that appeared but whose carrier could not be inferred
    // must produce a diagnostic -- never silently vanish.
    for name in &label_order {
        if !label_carriers.contains_key(name) {
            if let Some(&(side, span)) = first_occurrence.get(name) {
                clashes.push(Clash {
                    label: name.clone(),
                    side,
                    span,
                });
            }
        }
    }

    let result: Vec<(String, SurfType)> = label_order
        .into_iter()
        .filter_map(|name| label_carriers.remove(&name).map(|ty| (name, ty)))
        .collect();

    (result, clashes)
}

/// Walk `pat` at the expected type `ty`, recording variable → type bindings.
/// Stops at labels (their variables get types from the carrier, not the
/// surrounding context).
fn collect_var_types(
    pat: &Pattern,
    ty: &SurfType,
    env: &Env,
    var_types: &mut HashMap<String, SurfType>,
) {
    let ty = peel_paren(ty);
    match pat {
        Pattern::Var(name) => {
            var_types.insert(name.text.clone(), ty.clone());
        }
        Pattern::Ctor(name, args, _) => {
            if let Some(ci) = env.ctor(&name.text) {
                match (&ci.payload, args.as_slice()) {
                    (None, []) => {}
                    (Some(payload), [arg]) => {
                        collect_var_types(arg, payload, env, var_types);
                    }
                    (Some(payload), args) if args.len() > 1 => {
                        collect_curried_var_types(args, payload, env, var_types);
                    }
                    _ => {}
                }
            }
        }
        Pattern::Tuple(a, b, _) => {
            if let SurfType::Prod(ta, tb, _) = ty {
                collect_var_types(a, ta, env, var_types);
                collect_var_types(b, tb, env, var_types);
            }
        }
        Pattern::Paren(inner, _) => {
            collect_var_types(inner, ty, env, var_types);
        }
        Pattern::Label(..) | Pattern::Nat(..) => {}
    }
}

/// Handle curried ctor args for var type collection.
fn collect_curried_var_types(
    args: &[Pattern],
    ty: &SurfType,
    env: &Env,
    var_types: &mut HashMap<String, SurfType>,
) {
    let ty = peel_paren(ty);
    if args.len() == 1 {
        collect_var_types(&args[0], ty, env, var_types);
        return;
    }
    if let SurfType::Prod(ta, tb, _) = ty {
        collect_var_types(&args[0], ta, env, var_types);
        collect_curried_var_types(&args[1..], tb, env, var_types);
    }
}

/// Walk `pat` collecting label occurrences (leftmost-outermost traversal).
#[allow(clippy::too_many_arguments)]
fn collect_labels(
    pat: &Pattern,
    var_types: &HashMap<String, SurfType>,
    env: &Env,
    side: Side,
    label_order: &mut Vec<String>,
    label_carriers: &mut HashMap<String, SurfType>,
    clashes: &mut Vec<Clash>,
    first_occurrence: &mut HashMap<String, (Side, Span)>,
) {
    match pat {
        Pattern::Label(name, args, span) => {
            if !label_order.contains(&name.text) {
                label_order.push(name.text.clone());
            }
            first_occurrence
                .entry(name.text.clone())
                .or_insert((side, *span));

            if let Some(carrier) = infer_carrier(args, var_types, env, *span) {
                if let Some(existing) = label_carriers.get(&name.text) {
                    if !types_compatible(existing, &carrier) {
                        clashes.push(Clash {
                            label: name.text.clone(),
                            side,
                            span: *span,
                        });
                    }
                } else {
                    label_carriers.insert(name.text.clone(), carrier);
                }
            }

            // Recurse into args for nested labels.
            for arg in args {
                collect_labels(
                    arg,
                    var_types,
                    env,
                    side,
                    label_order,
                    label_carriers,
                    clashes,
                    first_occurrence,
                );
            }
        }
        Pattern::Ctor(_, args, _) => {
            for arg in args {
                collect_labels(
                    arg,
                    var_types,
                    env,
                    side,
                    label_order,
                    label_carriers,
                    clashes,
                    first_occurrence,
                );
            }
        }
        Pattern::Tuple(a, b, _) => {
            collect_labels(
                a,
                var_types,
                env,
                side,
                label_order,
                label_carriers,
                clashes,
                first_occurrence,
            );
            collect_labels(
                b,
                var_types,
                env,
                side,
                label_order,
                label_carriers,
                clashes,
                first_occurrence,
            );
        }
        Pattern::Paren(inner, _) => {
            collect_labels(
                inner,
                var_types,
                env,
                side,
                label_order,
                label_carriers,
                clashes,
                first_occurrence,
            );
        }
        Pattern::Var(_) | Pattern::Nat(..) => {}
    }
}

/// Infer the carrier type from a label's arguments.
fn infer_carrier(
    args: &[Pattern],
    var_types: &HashMap<String, SurfType>,
    env: &Env,
    span: Span,
) -> Option<SurfType> {
    match args.len() {
        0 => Some(SurfType::Unit(span)),
        1 => infer_pattern_type(&args[0], var_types, env),
        _ => {
            let types: Option<Vec<SurfType>> = args
                .iter()
                .map(|arg| infer_pattern_type(arg, var_types, env))
                .collect();
            types.map(|ts| right_nested_prod(&ts, span))
        }
    }
}

/// Infer the type of a pattern bottom-up (without a known expected type).
fn infer_pattern_type(
    pat: &Pattern,
    var_types: &HashMap<String, SurfType>,
    env: &Env,
) -> Option<SurfType> {
    match pat {
        Pattern::Var(name) => var_types.get(&name.text).cloned(),
        Pattern::Ctor(name, _, _) => env.ctor(&name.text).map(|ci| {
            SurfType::Name(UpperName {
                text: ci.type_name.clone(),
                span: name.span,
            })
        }),
        Pattern::Tuple(a, b, span) => {
            let ta = infer_pattern_type(a, var_types, env)?;
            let tb = infer_pattern_type(b, var_types, env)?;
            Some(SurfType::Prod(Box::new(ta), Box::new(tb), *span))
        }
        Pattern::Paren(inner, _) => infer_pattern_type(inner, var_types, env),
        Pattern::Nat(..) | Pattern::Label(..) => None,
    }
}

/// Build a right-nested product from a non-empty slice of types.
fn right_nested_prod(types: &[SurfType], span: Span) -> SurfType {
    match types {
        [] => unreachable!("right_nested_prod on empty slice"),
        [last] => last.clone(),
        [head, rest @ ..] => SurfType::Prod(
            Box::new(head.clone()),
            Box::new(right_nested_prod(rest, span)),
            span,
        ),
    }
}

/// Structural type compatibility (ignoring spans).
pub(crate) fn types_compatible(a: &SurfType, b: &SurfType) -> bool {
    match (peel_paren(a), peel_paren(b)) {
        (SurfType::Unit(_), SurfType::Unit(_)) => true,
        (SurfType::Empty(_), SurfType::Empty(_)) => true,
        (SurfType::Name(na), SurfType::Name(nb)) => na.text == nb.text,
        (SurfType::Var(va), SurfType::Var(vb)) => va.text == vb.text,
        (SurfType::Prod(a1, a2, _), SurfType::Prod(b1, b2, _)) => {
            types_compatible(a1, b1) && types_compatible(a2, b2)
        }
        (SurfType::Sum(a1, a2, _), SurfType::Sum(b1, b2, _)) => {
            types_compatible(a1, b1) && types_compatible(a2, b2)
        }
        _ => false,
    }
}

/// Peel off Paren wrappers on a surface type.
fn peel_paren(ty: &SurfType) -> &SurfType {
    match ty {
        SurfType::Paren(inner, _) => peel_paren(inner),
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{env, testkit};

    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    fn bit_ty() -> SurfType {
        SurfType::Name(UpperName {
            text: "Bit".into(),
            span: dummy_span(),
        })
    }

    fn unit_ty() -> SurfType {
        SurfType::Unit(dummy_span())
    }

    fn prod_ty(a: SurfType, b: SurfType) -> SurfType {
        SurfType::Prod(Box::new(a), Box::new(b), dummy_span())
    }

    #[test]
    fn spin_label_state_space() {
        let root = testkit::repo_root();
        let path = root.join("conformance/programs/0011-label-loop.thse");
        let program = testkit::load_program(&path);
        let env = env::resolve(&program);
        let labels = env.iso_labels("spin");
        assert_eq!(labels.len(), 1);
        assert_eq!(labels[0].0, "go");
        let expected = prod_ty(bit_ty(), prod_ty(bit_ty(), bit_ty()));
        assert!(
            types_compatible(&labels[0].1, &expected),
            "expected (Bit, (Bit, Bit)), got {:?}",
            labels[0].1
        );
    }

    #[test]
    fn stuck_label_carrier() {
        let root = testkit::repo_root();
        let path = root.join("conformance/diagnostics/0008-label-coverage.thse");
        let program = testkit::load_program(&path);
        let env = env::resolve(&program);
        let labels = env.iso_labels("stuck");
        assert_eq!(labels.len(), 1);
        assert_eq!(labels[0].0, "go");
        assert!(
            types_compatible(&labels[0].1, &bit_ty()),
            "expected Bit, got {:?}",
            labels[0].1
        );
    }

    #[test]
    fn nullary_label_carrier() {
        let src = "type Bit = O | I\n\
                    iso noop : Bit <-> Bit\n\
                    | O <-> $tick\n\
                    | $tick <-> O\n\
                    | I <-> I\n";
        let program = theseus_syntax::parse(src).unwrap();
        let env = env::resolve(&program);
        let labels = env.iso_labels("noop");
        assert_eq!(labels.len(), 1);
        assert_eq!(labels[0].0, "tick");
        assert!(
            types_compatible(&labels[0].1, &unit_ty()),
            "expected 1 (Unit), got {:?}",
            labels[0].1
        );
    }

    #[test]
    fn plain_isos_have_empty_label_space() {
        for path in testkit::program_fixtures() {
            let file = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
            if file == "0011-label-loop.thse" {
                continue;
            }
            let program = testkit::load_program(&path);
            let env = env::resolve(&program);
            for name in env.iso_names() {
                let labels = env.iso_labels(name);
                assert!(
                    labels.is_empty(),
                    "expected empty labels for iso '{name}' in {file}, got {labels:?}"
                );
            }
        }
    }
}
