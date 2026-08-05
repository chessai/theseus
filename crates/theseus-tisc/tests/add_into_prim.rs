//! ADR-0047: the surface `add_into` word-add prim over the machine word `W8`.
//!
//! Reproduces the Step-2 scenario end to end from source: a `prim add_into :
//! W8*W8 <-> W8*W8 = "theseus.arith.add_into"` reference elaborates (via the
//! builtin registry), compiles to a single reversible `Add`, and a byte-wide
//! Fibonacci built from it lowers to a clean `EXCH ; ADD` ROM and evaluates to
//! the Fibonacci sequence (mod 256, carry wraps included).

use theseus_check::elaborate::elaborate_program;
use theseus_check::env::resolve;
use theseus_core::{Iso, Value};
use theseus_tisc::asm::Instr;
use theseus_tisc::compile::compile_with_metadata;

/// Shared decls: the prim, the structural swap, and one fib step.
const PRELUDE: &str = "\
prim add_into : W8 * W8 <-> W8 * W8 = \"theseus.arith.add_into\"
iso swap2 : W8 * W8 <-> W8 * W8
| (a, b) <-> (b, a)
iso fibStep : W8 * W8 <-> W8 * W8 = swap2 ; add_into
";

/// Parse, check (must be clean), resolve, elaborate; return the named iso.
fn elaborate(src: &str, iso: &str) -> Iso {
    let prog = theseus_syntax::parse(src).expect("parse");
    let diags = theseus_check::check(&prog);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = resolve(&prog);
    let map = elaborate_program(&prog, &env);
    map.get(iso)
        .unwrap_or_else(|| panic!("iso {iso} not in elaboration map"))
        .clone()
        .unwrap_or_else(|e| panic!("elaborate {iso}: {e:?}"))
}

/// A `W8`/`Flat(8)` leaf value `k` (thermometer `Right^k(Left(Unit))`).
fn w8(k: u8) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

fn pair(a: u8, b: u8) -> Value {
    Value::pair(w8(a), w8(b))
}

fn decode(v: &Value) -> u8 {
    let mut k = 0u8;
    let mut cur = v;
    while let Value::Right(r) = cur {
        k += 1;
        cur = r;
    }
    k
}

fn fib8_src(n_steps: usize) -> String {
    let mut steps = String::from("fibStep");
    for _ in 1..n_steps {
        steps.push_str(" ; fibStep");
    }
    format!("{PRELUDE}iso fib8 : W8 * W8 <-> W8 * W8 = {steps}\n")
}

#[test]
fn add_into_compiles_to_single_add() {
    let src = format!("{PRELUDE}iso addw : W8 * W8 <-> W8 * W8 = add_into\n");
    let iso = elaborate(&src, "addw");
    let lowered = compile_with_metadata(&iso).expect("add_into compiles");
    assert_eq!(
        lowered.instrs.len(),
        1,
        "add_into must be a single instruction, got {:?}",
        lowered.instrs
    );
    assert!(
        matches!(lowered.instrs[0], Instr::Add { .. }),
        "add_into must compile to a reversible Add, got {:?}",
        lowered.instrs[0]
    );
}

#[test]
fn add_into_eval_semantics_including_overflow() {
    let src = format!("{PRELUDE}iso addw : W8 * W8 <-> W8 * W8 = add_into\n");
    let iso = elaborate(&src, "addw");
    for (a, b) in [(5u8, 7u8), (200, 100), (255, 1), (0, 0), (128, 128)] {
        let out = theseus_eval::eval(&iso, pair(a, b), &mut 1_000_000u64).expect("eval");
        let Value::Pair(oa, ob) = out else {
            panic!("expected pair");
        };
        assert_eq!(decode(&oa), a, "add_into preserves the addend a");
        assert_eq!(
            decode(&ob),
            a.wrapping_add(b),
            "add_into: b becomes (a + b) mod 256 (input {a},{b})"
        );
    }
}

#[test]
fn fib8_compiles_to_exch_add_rom() {
    // N kept small: each add_into emission is BVEQ-verified against its table
    // (BDD), which is cheap in release but heavy in debug. The N=10 shape is
    // exercised by the figure (release) and eval; here 1 and 3 prove the pattern.
    for n in [1usize, 3] {
        let iso = elaborate(&fib8_src(n), "fib8");
        let lowered = compile_with_metadata(&iso).expect("fib8 compiles");
        assert_eq!(
            lowered.instrs.len(),
            2 * n,
            "fib8(N={n}) must be 2N instrs (EXCH;ADD per step), got {:?}",
            lowered.instrs
        );
        for (i, ins) in lowered.instrs.iter().enumerate() {
            if i % 2 == 0 {
                assert!(
                    matches!(ins, Instr::Exch { .. }),
                    "fib8(N={n}) idx {i}: expected EXCH, got {ins:?}"
                );
            } else {
                assert!(
                    matches!(ins, Instr::Add { .. }),
                    "fib8(N={n}) idx {i}: expected ADD, got {ins:?}"
                );
            }
        }
    }
}

#[test]
fn fib8_evals_fibonacci_sequence() {
    let iso = elaborate(&fib8_src(10), "fib8");
    // 10 steps from (0, 1) -> (fib_10, fib_11) = (55, 89).
    let out = theseus_eval::eval(&iso, pair(0, 1), &mut 1_000_000u64).expect("eval");
    let Value::Pair(a, b) = out else {
        panic!("expected pair");
    };
    assert_eq!((decode(&a), decode(&b)), (55, 89));

    // Backward run un-applies the 10 steps exactly.
    let back = theseus_eval::eval(
        &Iso::sym(iso),
        Value::pair(w8(55), w8(89)),
        &mut 1_000_000u64,
    )
    .expect("backward eval");
    let Value::Pair(a0, b0) = back else {
        panic!("expected pair");
    };
    assert_eq!((decode(&a0), decode(&b0)), (0, 1), "backward run is exact");
}

#[test]
fn add_into_spec_is_mutual_inverse_all_widths() {
    // The builtin ships a symbolic (forward, backward) spec, not a table, so
    // elaboration only validates it structurally. Certify here (theseus-tisc)
    // that forward/backward are true mutual inverses over the whole domain via
    // BVEQ -- the guarantee `core::admit` would give a table prim -- at every
    // supported width, not just the register-native W8.
    use theseus_core::ty::Type;
    for w in [1u8, 2, 4, 8] {
        let pair = Type::prod(Type::Flat(w), Type::Flat(w));
        let def = theseus_core::prim::builtin_prim(theseus_core::prim::ADD_INTO_KEY, &pair, &pair)
            .unwrap_or_else(|| panic!("add_into builtin at W{w}"));
        let spec = def.symbolic.expect("add_into carries a symbolic spec");
        theseus_tisc::bveq::admit_symbolic(&spec).unwrap_or_else(|e| {
            panic!("W{w}: add_into forward/backward not mutual inverses: {e:?}")
        });
    }
}

#[test]
fn add_into_over_non_word_type_does_not_elaborate() {
    // add_into is defined only for a Flat word pair; over Bit*Bit (a Sum tree)
    // the builtin registry returns None, so the reference is unsupported rather
    // than silently miscompiled.
    let src = "\
type Bit = O | I
prim add_into : Bit * Bit <-> Bit * Bit = \"theseus.arith.add_into\"
iso addb : Bit * Bit <-> Bit * Bit = add_into
";
    let prog = theseus_syntax::parse(src).expect("parse");
    let env = resolve(&prog);
    let map = elaborate_program(&prog, &env);
    match map.get("addb") {
        Some(Err(errs)) if errs.is_empty() => {} // unsupported (expected)
        other => panic!("expected addb to be unsupported, got {other:?}"),
    }
}
