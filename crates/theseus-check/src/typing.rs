//! Typing check (T0102).
//!
//! For each iso with clauses, checks that every pattern in each clause is
//! well-typed against the iso's declared domain (lhs) or codomain (rhs).
//! A constructor `C p*` is well-typed at type `T` iff `C` is a constructor of
//! `T` (via `env`) and its argument patterns match `C`'s payload type; a tuple
//! pattern `(p1, p2)` against a product type `A * B`; a var pattern against any
//! type; a `Nat` literal against a `U<n>` type. Mismatches are `T0102` with the
//! offending `side:` and 1-indexed `clauses:`.

use std::collections::BTreeMap;

use theseus_syntax::ast::{Decl, IsoBody, IsoDecl, Pattern, Program, Type as SurfType};

use crate::diagnostics::{Code, Diagnostic, Side};
use crate::elaborate;
use crate::env::Env;

/// Run the typing check over a whole program.
pub fn check(program: &Program, env: &Env) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    // Surface iso-name collisions detected during resolution as T0101.
    for collision in env.iso_collisions() {
        diags.push(Diagnostic::new(Code::T0101, collision.span).with_name(collision.name.clone()));
    }
    for decl in &program.decls {
        if let Decl::Iso(id) = decl {
            check_iso(id, env, &mut diags);
        }
    }
    // The clause-pattern checks above cover `| p <-> q` bodies; `= expr`
    // (combinator / composition) bodies are typed here.
    check_expr_bodies(program, env, &mut diags);
    diags
}

/// Type every `= expr` iso body against its declared signature.
///
/// The clause path is checked structurally by [`check_iso`] (patterns) plus
/// Coverage; but a combinator/composition body (`= f ; g`, `= iter T h`,
/// `= p`, `= ~q`) has no clause patterns, and elaboration alone never compares
/// the body's type to the declared signature. Without this, `iso f : A <-> B =
/// expr` is accepted even when `expr` is ill-typed (an incompatible `;`) or has
/// a type other than `A <-> B` (a cardinality/domain-escape soundness hole:
/// e.g. `f : Bit <-> Bit = swap2` where `swap2 : Bit*Bit <-> Bit*Bit`). Every
/// well-typed iso must satisfy `type_of(body) = (A, B)` -- the Pi/Theseus rule
/// that a combinator's synthesized type is exactly its declared boundary.
///
/// Reuses the elaborator + core `type_of` (so `id`, `iter`/`fold` counter and
/// endo rules, etc. are handled once, authoritatively). An `= expr` body that
/// the structural core cannot elaborate (`Err(vec![])`) is *not* a type error
/// here -- it is reported (or skipped) by the pipeline's later, elaboration-time
/// handling; a body that elaborates but is ill-typed, or whose type differs from
/// the signature, is a `T0102`. Combinator type errors (`T06xx`) elaboration
/// already localized are surfaced verbatim.
fn check_expr_bodies(program: &Program, env: &Env, diags: &mut Vec<Diagnostic>) {
    let elab = elaborate::elaborate_program(program, env);
    for decl in &program.decls {
        if let Decl::Iso(id) = decl {
            check_iso_expr_body(id, env, &elab, diags);
        }
    }
}

fn check_iso_expr_body(
    id: &IsoDecl,
    env: &Env,
    elab: &BTreeMap<String, crate::elaborate::ElabResult>,
    diags: &mut Vec<Diagnostic>,
) {
    if let IsoBody::Expr(_) = &id.body {
        match elab.get(&id.name.text) {
            Some(Ok(core)) => match theseus_core::type_of(core) {
                Ok((got_in, got_out)) => {
                    let want_in = env.lower(&id.lhs);
                    let want_out = env.lower(&id.rhs);
                    if got_in != want_in || got_out != want_out {
                        diags.push(
                            Diagnostic::new(Code::T0102, id.name.span)
                                .with_name(id.name.text.clone()),
                        );
                    }
                }
                // The body elaborates but is internally ill-typed -- e.g. an
                // incompatible sequential composition `f ; g` (out(f) != in(g)).
                Err(_) => {
                    diags.push(
                        Diagnostic::new(Code::T0102, id.name.span).with_name(id.name.text.clone()),
                    );
                }
            },
            // Combinator type errors (T06xx) already carry precise spans.
            Some(Err(errs)) if !errs.is_empty() => diags.extend(errs.iter().cloned()),
            // `Err(vec![])` = unsupported in the structural core (not a type
            // error); `None` cannot happen for a declared iso. Leave to later.
            _ => {}
        }
    }
    for w in &id.wheres {
        check_iso_expr_body(w, env, elab, diags);
    }
}

fn check_iso(id: &IsoDecl, env: &Env, diags: &mut Vec<Diagnostic>) {
    let labels = env.iso_labels(&id.name.text);
    let clashes = env.iso_label_clashes(&id.name.text);

    // Report carrier-unification clashes as T0102.
    for clash in clashes {
        diags.push(
            Diagnostic::new(Code::T0102, clash.span)
                .with_side(clash.side)
                .with_name(clash.label.clone()),
        );
    }

    if let IsoBody::Clauses(clauses) = &id.body {
        for (i, clause) in clauses.iter().enumerate() {
            let clause_index = i + 1; // 1-indexed
            let mut var_types: BTreeMap<String, SurfType> = BTreeMap::new();
            check_pattern(
                &clause.lhs,
                &id.lhs,
                env,
                Side::Left,
                clause_index,
                labels,
                true,
                &mut var_types,
                diags,
            );
            check_pattern(
                &clause.rhs,
                &id.rhs,
                env,
                Side::Right,
                clause_index,
                labels,
                true,
                &mut var_types,
                diags,
            );
        }
    }
    for w in &id.wheres {
        check_iso(w, env, diags);
    }
}

/// Check that `pat` is well-typed at surface type `ty`. If not, push a T0102.
/// `at_root` is true only when `pat` is the top-level pattern of a clause side.
/// `var_types` tracks each variable's first-seen surface type within a clause;
/// if the same variable appears at an incompatible type (comparing lowered core
/// types), a T0102 is emitted.
#[allow(clippy::too_many_arguments)]
fn check_pattern(
    pat: &Pattern,
    ty: &SurfType,
    env: &Env,
    side: Side,
    clause_index: usize,
    labels: &[(String, SurfType)],
    at_root: bool,
    var_types: &mut BTreeMap<String, SurfType>,
    diags: &mut Vec<Diagnostic>,
) {
    // Unwrap Paren on the type side.
    let ty = peel_paren_type(ty);

    match pat {
        Pattern::Var(name) => {
            // Record the variable's expected type; if already recorded,
            // check cross-side consistency via lowered core types.
            match var_types.get(&name.text) {
                None => {
                    var_types.insert(name.text.clone(), ty.clone());
                }
                Some(prev_ty) => {
                    if env.lower(prev_ty) != env.lower(ty) {
                        diags.push(
                            Diagnostic::new(Code::T0102, name.span)
                                .with_side(side)
                                .with_name(name.text.clone())
                                .with_clauses(vec![clause_index]),
                        );
                    }
                }
            }
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
        Pattern::Label(name, args, _) => {
            // H2: a label pattern is only well-typed at the clause root.
            if !at_root {
                diags.push(
                    Diagnostic::new(Code::T0102, pat.span())
                        .with_side(side)
                        .with_name(name.text.clone())
                        .with_clauses(vec![clause_index]),
                );
                return;
            }
            // Check label args against the inferred carrier type.
            if let Some((_, carrier)) = labels.iter().find(|(n, _)| n == &name.text) {
                match args.len() {
                    0 => { /* nullary label -- carrier is unit, nothing to check */ }
                    1 => {
                        check_pattern(
                            &args[0],
                            carrier,
                            env,
                            side,
                            clause_index,
                            labels,
                            false,
                            var_types,
                            diags,
                        );
                    }
                    _ => {
                        check_curried_args(
                            args,
                            carrier,
                            env,
                            side,
                            clause_index,
                            labels,
                            var_types,
                            diags,
                        );
                    }
                }
            }
        }
        Pattern::Ctor(name, args, _) => {
            let Some(ctor_info) = env.ctor(&name.text) else {
                // Unknown ctor -- Names (T0101) already reported this; skip.
                return;
            };
            // The ctor must belong to the type at this position.
            if !type_admits_ctor(ty, &ctor_info.type_name) {
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
                    // Nullary ctor applied to no args -- fine.
                }
                (Some(payload_ty), [arg]) => {
                    check_pattern(
                        arg,
                        payload_ty,
                        env,
                        side,
                        clause_index,
                        labels,
                        false,
                        var_types,
                        diags,
                    );
                }
                (Some(payload_ty), args) if args.len() > 1 => {
                    check_curried_args(
                        args,
                        payload_ty,
                        env,
                        side,
                        clause_index,
                        labels,
                        var_types,
                        diags,
                    );
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
                check_pattern(
                    a,
                    ty_a,
                    env,
                    side,
                    clause_index,
                    labels,
                    false,
                    var_types,
                    diags,
                );
                check_pattern(
                    b,
                    ty_b,
                    env,
                    side,
                    clause_index,
                    labels,
                    false,
                    var_types,
                    diags,
                );
            } else {
                diags.push(
                    Diagnostic::new(Code::T0102, pat.span())
                        .with_side(side)
                        .with_clauses(vec![clause_index]),
                );
            }
        }
        Pattern::Paren(inner, _) => {
            check_pattern(
                inner,
                ty,
                env,
                side,
                clause_index,
                labels,
                at_root,
                var_types,
                diags,
            );
        }
    }
}

/// Check curried constructor arguments against a product-typed payload.
/// `C a b c` with payload `A * B * C` checks `a` against `A`, `b` against `B`,
/// `c` against `C` (the payload tuple is right-nested, matching tuple sugar).
#[allow(clippy::too_many_arguments)]
fn check_curried_args(
    args: &[Pattern],
    ty: &SurfType,
    env: &Env,
    side: Side,
    clause_index: usize,
    labels: &[(String, SurfType)],
    var_types: &mut BTreeMap<String, SurfType>,
    diags: &mut Vec<Diagnostic>,
) {
    let ty = peel_paren_type(ty);
    if args.len() == 1 {
        check_pattern(
            &args[0],
            ty,
            env,
            side,
            clause_index,
            labels,
            false,
            var_types,
            diags,
        );
        return;
    }
    if let SurfType::Prod(ty_a, ty_b, _) = ty {
        check_pattern(
            &args[0],
            ty_a,
            env,
            side,
            clause_index,
            labels,
            false,
            var_types,
            diags,
        );
        check_curried_args(
            &args[1..],
            ty_b,
            env,
            side,
            clause_index,
            labels,
            var_types,
            diags,
        );
    } else {
        // More args than product components -- arity error.
        diags.push(
            Diagnostic::new(Code::T0102, args[0].span())
                .with_side(side)
                .with_clauses(vec![clause_index]),
        );
    }
}

/// Does the surface type `ty` admit a constructor belonging to `type_name`?
fn type_admits_ctor(ty: &SurfType, type_name: &str) -> bool {
    let ty = peel_paren_type(ty);
    match ty {
        SurfType::Name(n) => n.text == type_name,
        SurfType::Sum(a, b, _) => type_admits_ctor(a, type_name) || type_admits_ctor(b, type_name),
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
        diags
            .into_iter()
            .filter(|d| d.code == Code::T0102)
            .collect()
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

    #[test]
    fn clash_emits_t0102() {
        let src = "type Bit = O | I\n\
                    iso bad : Bit <-> Bit\n\
                    | O <-> $go O\n\
                    | I <-> $go (O, O)\n";
        let program = theseus_syntax::parse(src).unwrap();
        let env = crate::env::resolve(&program);
        let reported = typing_only(check(&program, &env));
        assert!(
            !reported.is_empty(),
            "expected T0102 for label carrier clash"
        );
        let clash_diag = reported
            .iter()
            .find(|d| d.name.as_deref() == Some("go"))
            .expect("expected a T0102 with name 'go'");
        assert_eq!(clash_diag.code, Code::T0102);
        assert_eq!(clash_diag.side, Some(Side::Right));
    }

    #[test]
    fn carrier_not_inferable_emits_t0102() {
        // H1: $go's carrier is never pinned -- must not silently vanish.
        let src = "type Bit = O | I\n\
                    iso f : Bit <-> Bit\n\
                    | $go x <-> $go x\n\
                    | b <-> b\n";
        let program = theseus_syntax::parse(src).unwrap();
        let env = crate::env::resolve(&program);
        let reported = typing_only(check(&program, &env));
        assert!(
            !reported.is_empty(),
            "expected T0102 for carrier-not-inferable label"
        );
        let diag = reported
            .iter()
            .find(|d| d.name.as_deref() == Some("go"))
            .expect("expected a T0102 with name 'go'");
        assert_eq!(diag.code, Code::T0102);
    }

    #[test]
    fn nested_label_emits_t0102() {
        // H2: $go inside a tuple is nested -- must be rejected.
        let src = "type Bit = O | I\n\
                    iso g : (Bit, Bit) <-> (Bit, Bit)\n\
                    | (a, $go b) <-> (a, $go b)\n\
                    | (a, b) <-> (b, a)\n";
        let program = theseus_syntax::parse(src).unwrap();
        let env = crate::env::resolve(&program);
        let reported = typing_only(check(&program, &env));
        assert!(
            !reported.is_empty(),
            "expected T0102 for nested label pattern"
        );
        let diag = reported
            .iter()
            .find(|d| d.name.as_deref() == Some("go"))
            .expect("expected a T0102 with name 'go' for nested label");
        assert_eq!(diag.code, Code::T0102);
    }

    #[test]
    fn cross_side_var_type_mismatch_emits_t0102() {
        // A variable used at incompatible types on opposite sides of the same
        // clause must be caught by typing, not deferred to coverage.
        let src = "type Bit = O | I\n\
                    iso m : Bit <-> Bit\n\
                    | O <-> $a O\n\
                    | I <-> $b (O, O)\n\
                    | $a x <-> $b x\n\
                    | $b y <-> $a y\n";
        let program = theseus_syntax::parse(src).unwrap();
        let env = crate::env::resolve(&program);
        let reported = typing_only(check(&program, &env));
        // `x` appears at carrier Bit (via $a on LHS) and carrier (Bit,Bit) (via $b on RHS).
        // `y` has the same issue in the opposite direction.
        let x_diag = reported
            .iter()
            .find(|d| d.name.as_deref() == Some("x"))
            .expect("expected a T0102 naming variable 'x'");
        assert_eq!(x_diag.code, Code::T0102);
        let y_diag = reported
            .iter()
            .find(|d| d.name.as_deref() == Some("y"))
            .expect("expected a T0102 naming variable 'y'");
        assert_eq!(y_diag.code, Code::T0102);
    }

    #[test]
    fn same_type_var_both_sides_no_t0102() {
        // A variable at the SAME type on both sides must NOT produce T0102.
        let src = "type Bit = O | I\n\
                    iso spin : Bit <-> Bit\n\
                    | O <-> I\n\
                    | I <-> O\n";
        let program = theseus_syntax::parse(src).unwrap();
        let env = crate::env::resolve(&program);
        let reported = typing_only(check(&program, &env));
        assert!(
            reported.is_empty(),
            "unexpected T0102 in spin: {reported:?}"
        );
    }

    // ── `= expr` body typing (soundness: body type must equal the signature) ──
    // Before this, `check` never typed combinator/composition bodies, so an
    // ill-typed or wrong-cardinality `= expr` iso was silently accepted -- the
    // declared signature (used for marshaling and as the iso's type) was a lie.

    fn check_src(src: &str) -> Vec<Diagnostic> {
        let program = theseus_syntax::parse(src).unwrap();
        let env = crate::env::resolve(&program);
        check(&program, &env)
    }

    #[test]
    fn expr_body_cardinality_mismatch_emits_t0102() {
        // f claims Bit <-> Bit (|Bit| = 2) but its body is a pair swap
        // (|Bit*Bit| = 4). A bijection cannot exist -- must be rejected.
        let src = "type Bit = O | I\n\
                    iso swap2 : Bit * Bit <-> Bit * Bit\n\
                    | (a, b) <-> (b, a)\n\
                    iso f : Bit <-> Bit = swap2\n";
        let d = typing_only(check_src(src));
        assert!(
            d.iter().any(|x| x.name.as_deref() == Some("f")),
            "expected T0102 naming f for cardinality/type mismatch, got: {d:?}"
        );
    }

    #[test]
    fn expr_body_ill_typed_composition_emits_t0102() {
        // notb : Bit<->Bit ; swap2 : Bit*Bit<->Bit*Bit -- the composition is
        // ill-typed (out(notb)=Bit != in(swap2)=Bit*Bit).
        let src = "type Bit = O | I\n\
                    iso notb : Bit <-> Bit | O <-> I | I <-> O\n\
                    iso swap2 : Bit * Bit <-> Bit * Bit | (a, b) <-> (b, a)\n\
                    iso f : Bit <-> Bit = notb ; swap2\n";
        let d = typing_only(check_src(src));
        assert!(
            d.iter().any(|x| x.name.as_deref() == Some("f")),
            "expected T0102 naming f for ill-typed composition, got: {d:?}"
        );
    }

    #[test]
    fn expr_body_bad_combinator_surfaces_t0601() {
        // `iter Bit notb` -- the counter Bit is not a numeral type. This T0601
        // was produced by the elaborator but never surfaced by `check` before.
        let src = "type Bit = O | I\n\
                    iso notb : Bit <-> Bit | O <-> I | I <-> O\n\
                    iso f : Bit <-> Bit = iter Bit notb\n";
        let d = check_src(src);
        assert!(
            d.iter().any(|x| x.code == Code::T0601),
            "expected T0601 for non-numeral iter counter, got: {d:?}"
        );
    }

    #[test]
    fn well_typed_expr_bodies_no_diag() {
        // A correct composition and a correct adjoint must stay clean.
        let src = "type Bit = O | I\n\
                    iso notb : Bit <-> Bit | O <-> I | I <-> O\n\
                    iso twice : Bit <-> Bit = notb ; notb\n\
                    iso inv : Bit <-> Bit = ~ notb\n";
        let d = check_src(src);
        assert!(
            d.is_empty(),
            "unexpected diagnostics on valid bodies: {d:?}"
        );
    }
}
