//! Tests for the implicit prelude.

use theseus_check::elaborate::elaborate_program;
use theseus_check::prelude::{with_prelude, PRELUDE_SRC};

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

iso tofMid : (Bit, (Bit, Bit)) <-> (Bit, (Bit, Bit))
= asc ; (toffoli * idb) ; ~asc
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
        isos.get("tofMid").unwrap().is_ok(),
        "tofMid (using prelude toffoli) should elaborate"
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
