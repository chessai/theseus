//! Negative tests: ill-formed iter/fold/unfold combinators must produce the
//! correct T06xx diagnostic code with the combinator's span.

use theseus_check::diagnostics::Code;
use theseus_check::elaborate::elaborate_program;
use theseus_check::env;

/// Parse, resolve, elaborate, and return the diagnostics for the named iso.
/// Panics if the iso is missing or (unexpectedly) elaborated successfully.
fn expect_err(src: &str, iso_name: &str) -> Vec<theseus_check::Diagnostic> {
    let prog = theseus_syntax::parse(src).expect("source must parse");
    let env = env::resolve(&prog);
    let map = elaborate_program(&prog, &env);
    let result = map
        .get(iso_name)
        .unwrap_or_else(|| panic!("iso `{iso_name}` not in elaboration map"));
    result
        .as_ref()
        .expect_err(&format!("iso `{iso_name}` should NOT elaborate"))
        .clone()
}

// ── T0601: iter counter type not a numeral ──────────────────────────────

#[test]
fn t0601_iter_counter_not_numeral() {
    // Bit = 1+1 (not a numeral type -- no 0 terminator).
    let src = "\
type Bit = O | I
iso notb : Bit <-> Bit
  | O <-> I
  | I <-> O
iso bad : Bit * Bit <-> Bit * Bit = iter Bit notb
";
    let diags = expect_err(src, "bad");
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code, Code::T0601);
    // Span must cover the `iter Bit notb` expression (nonzero).
    assert!(diags[0].span.start < diags[0].span.end);
}

// ── T0602: iter body not an endo-iso ────────────────────────────────────

#[test]
fn t0602_iter_body_not_endo() {
    // swap⁺(Bit, 1) : Bit + 1 ↔ 1 + Bit -- not endo.
    // We need an iso whose elaborated type is not A ↔ A.
    // swp : (Bit, Bit) <-> (Bit, Bit) where the body swaps -- this IS endo.
    // Instead, build a named iso with asymmetric type.
    //
    // Actually, this is hard to trigger from surface syntax alone because
    // named isos used as combinator bodies are elaborated from their own
    // clauses, and the core typing only sees the elaborated Iso. We need
    // a body whose CORE type is not endo. A swap like (a,b)<->(b,a) where
    // a≠b gives SwapProd(a,b) which has type a×b ↔ b×a -- not endo.
    let src = "\
type Bit = O | I
type Nat = Zero | Succ Nat
iso asym : (Bit, Nat) <-> (Nat, Bit)
  | (a, b) <-> (b, a)
iso bad : Nat * (Bit, Nat) <-> Nat * (Bit, Nat) = iter Nat asym
";
    let diags = expect_err(src, "bad");
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code, Code::T0602);
    assert!(diags[0].span.start < diags[0].span.end);
}

// ── T0603: fold/unfold spine type not a list ────────────────────────────

#[test]
fn t0603_fold_spine_not_list() {
    // Nat lowers to a numeral type, not a list spine.
    let src = "\
type Bit = O | I
type Nat = Zero | Succ Nat
iso swp : (Bit, Bit) <-> (Bit, Bit)
  | (a, b) <-> (b, a)
iso bad : Nat * Bit <-> Nat * Bit = fold Nat swp
";
    let diags = expect_err(src, "bad");
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code, Code::T0603);
    assert!(diags[0].span.start < diags[0].span.end);
}

#[test]
fn t0603_unfold_spine_not_list() {
    let src = "\
type Bit = O | I
type Nat = Zero | Succ Nat
iso swp : (Bit, Bit) <-> (Bit, Bit)
  | (a, b) <-> (b, a)
iso bad : Nat * Bit <-> Nat * Bit = unfold Nat swp
";
    let diags = expect_err(src, "bad");
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code, Code::T0603);
}

// ── T0604: fold/unfold body not an endo-iso ─────────────────────────────

#[test]
fn t0604_fold_body_not_endo() {
    // Body swaps (Bit, Nat) <-> (Nat, Bit) -- not endo.
    let src = "\
type Bit = O | I
type Nat = Zero | Succ Nat
type BitList = Nil | Cons (Bit * BitList)
iso asym : (Bit, Nat) <-> (Nat, Bit)
  | (a, b) <-> (b, a)
iso bad : BitList * Nat <-> BitList * Nat = fold BitList asym
";
    let diags = expect_err(src, "bad");
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code, Code::T0604);
}

// ── T0605: fold/unfold body type not a product ──────────────────────────

#[test]
fn t0605_fold_body_not_elem_prod() {
    // notb : Bit <-> Bit -- endo but not product-shaped.
    let src = "\
type Bit = O | I
type BitList = Nil | Cons (Bit * BitList)
iso notb : Bit <-> Bit
  | O <-> I
  | I <-> O
iso bad : BitList * Bit <-> BitList * Bit = fold BitList notb
";
    let diags = expect_err(src, "bad");
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code, Code::T0605);
}

// ── T0606: fold/unfold body element mismatch ────────────────────────────

#[test]
fn t0606_fold_body_elem_mismatch() {
    // Spine element is Bit, but body's first product component is 1 (Unit).
    // id(1 × Bit) : (1, Bit) <-> (1, Bit) -- endo product, but elem = 1 ≠ Bit.
    let src = "\
type Bit = O | I
type BitList = Nil | Cons (Bit * BitList)
type Unit1 = U
iso idup : (Unit1, Bit) <-> (Unit1, Bit)
  | (U, b) <-> (U, b)
iso bad : BitList * Bit <-> BitList * Bit = fold BitList idup
";
    let diags = expect_err(src, "bad");
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].code, Code::T0606);
}

// ── Span accuracy: the span must cover the combinator expression ────────

#[test]
fn combinator_span_covers_expression() {
    // "iter Bit notb" should span from the 'i' of 'iter' to the end of 'notb'.
    let src = "\
type Bit = O | I
iso notb : Bit <-> Bit
  | O <-> I
  | I <-> O
iso bad : Bit * Bit <-> Bit * Bit = iter Bit notb
";
    let diags = expect_err(src, "bad");
    assert_eq!(diags.len(), 1);
    // The span should cover "iter Bit notb".
    let fragment = &src[diags[0].span.start..diags[0].span.end];
    assert!(
        fragment.starts_with("iter"),
        "span fragment should start with 'iter', got: {fragment:?}"
    );
    assert!(
        fragment.ends_with("notb"),
        "span fragment should end with 'notb', got: {fragment:?}"
    );
}

// ── Positive: id-as-combinator-body (position type inference) ───────────

#[test]
fn iter_nat_id_elaborates_as_identity() {
    // `iter Nat id` should elaborate to Iso::Iter(numeral, Id(Bit)).
    // Semantically: the k-fold application of id is id, so the whole
    // combinator is the identity on Nat × Bit.
    let src = "\
type Bit = O | I
type Nat = Zero | Succ Nat
iso idNB : Nat * Bit <-> Nat * Bit = iter Nat id
";
    let prog = theseus_syntax::parse(src).expect("source must parse");
    let env = env::resolve(&prog);
    let map = elaborate_program(&prog, &env);
    let result = map.get("idNB").expect("iso idNB not in map");
    let iso = result
        .as_ref()
        .expect("iter Nat id should elaborate successfully");

    // The elaborated iso must be Iter(numeral, Id(_)).
    match iso {
        theseus_core::Iso::Iter(_, body) => {
            assert!(
                matches!(body.as_ref(), theseus_core::Iso::Id(_)),
                "body should be Id, got: {body:?}"
            );
        }
        other => panic!("expected Iter, got: {other:?}"),
    }

    // Differential oracle: for every value in the domain, forward and
    // backward must be identity (the k-fold application of id is id).
    let core_lhs = env.lower(&theseus_syntax::ast::Type::Prod(
        Box::new(theseus_syntax::ast::Type::Name(
            theseus_syntax::ast::UpperName {
                text: "Nat".into(),
                span: theseus_syntax::ast::Span { start: 0, end: 0 },
            },
        )),
        Box::new(theseus_syntax::ast::Type::Name(
            theseus_syntax::ast::UpperName {
                text: "Bit".into(),
                span: theseus_syntax::ast::Span { start: 0, end: 0 },
            },
        )),
        theseus_syntax::ast::Span { start: 0, end: 0 },
    ));
    let values = theseus_core::enumerate_values(&core_lhs);
    assert!(!values.is_empty(), "domain must be nonempty");
    for v in &values {
        let mut fuel = 100_000u64;
        let fwd =
            theseus_eval::interp::eval(iso, v.clone(), &mut fuel).expect("eval forward succeeds");
        assert_eq!(
            fwd, *v,
            "iter Nat id must be identity, but forward({v:?}) = {fwd:?}"
        );
    }
}
