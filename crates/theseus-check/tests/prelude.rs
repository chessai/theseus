//! Tests for the implicit prelude.

use theseus_check::elaborate::elaborate_program;
use theseus_check::prelude::{with_prelude, PRELUDE_SRC};
use theseus_core::prim::Value;

/// A program that uses Bit, Nat, and cnot WITHOUT declaring them should
/// parse, check clean under with_prelude, and elaborate successfully.
#[test]
fn prelude_provides_undeclared_types_and_isos() {
    let src = r#"
iso myIso : Bit <-> Bit
| O <-> I
| I <-> O
"#;
    let program = theseus_syntax::parse(src).unwrap();
    let merged = with_prelude(&program);
    let diags = theseus_check::check(&merged);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");

    let env = theseus_check::env::resolve(&merged);
    let isos = elaborate_program(&merged, &env);
    assert!(
        isos.get("myIso").unwrap().is_ok(),
        "myIso should elaborate successfully"
    );
}

/// A program that self-declares `type Bit = O | I` identically should
/// still check clean under with_prelude (structural dedup drops the
/// prelude copy, no duplicate-name error).
#[test]
fn self_declared_type_wins_over_prelude() {
    let src = r#"
type Bit = O | I

iso myNotb : Bit <-> Bit
| O <-> I
| I <-> O
"#;
    let program = theseus_syntax::parse(src).unwrap();
    let merged = with_prelude(&program);
    let diags = theseus_check::check(&merged);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");
}

/// A program that uses toffoli (and cnot) WITHOUT declaring them should
/// parse, check clean under with_prelude, and elaborate successfully --
/// proving toffoli is now in the implicit prelude.
#[test]
fn prelude_provides_toffoli_and_cnot() {
    let src = r#"
iso idb : Bit <-> Bit
| O <-> O
| I <-> I

iso asc : (Bit, (Bit, Bit)) <-> ((Bit, Bit), Bit)
| (x, (y, z)) <-> ((x, y), z)

iso cnotMid : (Bit, (Bit, Bit)) <-> (Bit, (Bit, Bit))
= asc ; (cnot * idb) ; ~asc

iso tofUse : (Bit, (Bit, Bit)) <-> (Bit, (Bit, Bit))
= toffoli ; toffoli
"#;
    let program = theseus_syntax::parse(src).unwrap();
    let merged = with_prelude(&program);
    let diags = theseus_check::check(&merged);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");

    let env = theseus_check::env::resolve(&merged);
    let isos = elaborate_program(&merged, &env);
    assert!(
        isos.get("cnotMid").unwrap().is_ok(),
        "cnotMid (using prelude cnot) should elaborate"
    );
    assert!(
        isos.get("tofUse").unwrap().is_ok(),
        "tofUse (using prelude toffoli) should elaborate"
    );
}

/// The builtin `add_into` prim comes from the prelude: byte-wide Fibonacci needs
/// no `prim` line, goes through the `theseus_check::parse` entry point, and maps
/// (13, 21) to (55, 89).
#[test]
fn prelude_provides_add_into() {
    let src = r#"
iso swap2 : W8 * W8 <-> W8 * W8
| (a, b) <-> (b, a)

iso fibStep : W8 * W8 <-> W8 * W8
    = swap2 ; add_into

iso fib8 : W8 * W8 <-> W8 * W8
    = fibStep ; fibStep ; fibStep
"#;
    let program = theseus_check::parse(src).unwrap();
    let diags = theseus_check::check(&program);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");

    let env = theseus_check::env::resolve(&program);
    let isos = elaborate_program(&program, &env);
    let fib8 = isos.get("fib8").unwrap().clone().expect("fib8 elaborates");

    let w8 = |k: u8| {
        let mut v = Value::left(Value::Unit);
        for _ in 0..k {
            v = Value::right(v);
        }
        v
    };
    let mut fuel = 1_000_000;
    let out = theseus_eval::interp::eval(&fib8, Value::pair(w8(13), w8(21)), &mut fuel)
        .expect("fib8 evaluates");
    assert_eq!(out, Value::pair(w8(55), w8(89)));
}

/// Parsing with `theseus_syntax::parse` is the explicit no-prelude path: the
/// same program then has unbound names.
#[test]
fn bare_parse_does_not_merge_the_prelude() {
    let src = "iso addw : W8 * W8 <-> W8 * W8 = add_into\n";
    let program = theseus_syntax::parse(src).unwrap();
    let diags = theseus_check::check(&program);
    assert!(
        diags.iter().any(|d| d.code == theseus_check::Code::T0101),
        "expected T0101 without the prelude, got: {diags:?}"
    );
}

/// The prelude itself must parse and every prelude iso must elaborate to Ok.
#[test]
fn prelude_source_parses_and_elaborates() {
    let program = theseus_syntax::parse(PRELUDE_SRC).unwrap();
    let diags = theseus_check::check(&program);
    assert!(diags.is_empty(), "prelude diagnostics: {diags:?}");

    let env = theseus_check::env::resolve(&program);
    let isos = elaborate_program(&program, &env);
    for (name, result) in &isos {
        assert!(
            result.is_ok(),
            "prelude iso '{name}' failed to elaborate: {:?}",
            result.as_ref().err()
        );
    }
}
