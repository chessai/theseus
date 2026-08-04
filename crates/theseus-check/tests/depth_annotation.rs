//! Tests for use-site depth-annotation syntax on μ-types (ADR-0035 follow-up).

use theseus_core::prim::enumerate_values;
use theseus_syntax::ast::{Span, Type as SurfType, UpperName};

/// Helper: parse a program and resolve its environment.
fn parse_env(src: &str) -> (theseus_check::env::Env, theseus_syntax::ast::Program) {
    let program = theseus_syntax::parse(src).unwrap();
    let env = theseus_check::env::resolve(&program);
    (env, program)
}

/// Helper: build a plain `Name` surface type.
fn named(name: &str) -> SurfType {
    SurfType::Name(UpperName {
        text: name.into(),
        span: Span::new(0, 0),
    })
}

/// Helper: build a `Name` surface type with a specific span start,
/// to be matched against a depth_override entry in the env.
fn named_at(name: &str, span_start: usize) -> SurfType {
    SurfType::Name(UpperName {
        text: name.into(),
        span: Span::new(span_start, span_start + name.len()),
    })
}

#[test]
fn depth_annotation_changes_cardinality() {
    let (mut env, _) = parse_env("type Nat = Zero | Succ Nat\n");
    // Default depth (4) vs annotated depth (6): different cardinalities.
    let default_core = env.lower(&named("Nat"));
    let default_count = enumerate_values(&default_core).len();
    assert_eq!(default_count, 4, "default depth should give 4 inhabitants");

    // Simulate a depth annotation by setting a depth override.
    env.set_depth_override(42, 6);
    let annotated_core = env.lower(&named_at("Nat", 42));
    let annotated_count = enumerate_values(&annotated_core).len();
    assert_eq!(annotated_count, 6, "Nat@6 should give 6 inhabitants");
}

#[test]
fn depth_annotation_various_depths() {
    let (mut env, _) = parse_env("type Nat = Zero | Succ Nat\n");
    for d in [0usize, 1, 2, 3, 5, 8] {
        let span_start = 1000 + d;
        env.set_depth_override(span_start, d);
        let core = env.lower(&named_at("Nat", span_start));
        assert_eq!(
            enumerate_values(&core).len(),
            d,
            "Nat@{d} should have exactly {d} inhabitants"
        );
    }
}

#[test]
fn value_within_depth_round_trips() {
    let (mut env, _) = parse_env("type Nat = Zero | Succ Nat\n");
    // Values from a shallower depth are a prefix of a deeper depth.
    env.set_depth_override(100, 3);
    let shallow = enumerate_values(&env.lower(&named_at("Nat", 100)));
    env.set_depth_override(200, 6);
    let deep = enumerate_values(&env.lower(&named_at("Nat", 200)));
    assert_eq!(
        shallow.as_slice(),
        &deep[..shallow.len()],
        "values at depth 3 should be a prefix of values at depth 6"
    );
}

#[test]
fn default_applies_without_annotation() {
    let (env, _) = parse_env("type Nat = Zero | Succ Nat\n");
    // Un-annotated Name uses the env's default unroll depth (4).
    let core = env.lower(&named("Nat"));
    assert_eq!(
        enumerate_values(&core).len(),
        theseus_check::env::DEFAULT_UNROLL_DEPTH,
        "un-annotated Nat should use the default depth"
    );
}

#[test]
fn depth_annotation_parses_and_lowers() {
    // Parse a program with a depth annotation and verify that lowering uses it.
    let src = "type Nat = Zero | Succ Nat\niso id6 : Nat@6 <-> Nat@6 = id\n";
    let program = theseus_syntax::parse(src).unwrap();
    // The depth_overrides map should have entries for the two `Nat@6` occurrences.
    assert!(
        !program.depth_overrides.is_empty(),
        "depth_overrides should be populated by the parser"
    );
    let env = theseus_check::env::resolve(&program);
    // Lowering the iso's lhs type (Nat with depth override) should give 6 inhabitants.
    if let theseus_syntax::ast::Decl::Iso(iso) = &program.decls[1] {
        let core_lhs = env.lower(&iso.lhs);
        assert_eq!(
            enumerate_values(&core_lhs).len(),
            6,
            "Nat@6 in iso signature should lower to 6 inhabitants"
        );
    } else {
        panic!("expected iso decl");
    }
}

#[test]
fn depth_annotation_pretty_round_trips() {
    let src = "type Nat = Zero | Succ Nat\niso id6 : Nat@6 <-> Nat@6 = id\n";
    let program = theseus_syntax::parse(src).unwrap();
    let pretty = theseus_syntax::pretty_program(&program);
    // The pretty output should contain @6 annotations.
    assert!(
        pretty.contains("Nat@6"),
        "pretty output should preserve @6: {pretty}"
    );
    let reparsed = theseus_syntax::parse(&pretty).unwrap();
    let norm_orig = theseus_syntax::normalize_program(&program);
    let norm_re = theseus_syntax::normalize_program(&reparsed);
    assert_eq!(
        norm_orig, norm_re,
        "parse-pretty-parse round-trip should hold"
    );
}

#[test]
fn depth_annotation_in_product_type() {
    let (mut env, _) = parse_env("type Nat = Zero | Succ Nat\ntype Bit = O | I\n");
    // Nat@6 * Bit should have 6 * 2 = 12 inhabitants.
    env.set_depth_override(42, 6);
    let ty = SurfType::Prod(
        Box::new(named_at("Nat", 42)),
        Box::new(named("Bit")),
        Span::new(0, 0),
    );
    let core = env.lower(&ty);
    assert_eq!(enumerate_values(&core).len(), 12);
}
