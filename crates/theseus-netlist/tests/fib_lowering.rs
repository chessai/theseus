//! Differential-oracle test for the surface Fibonacci program
//! (`conformance/programs/0022-fib.thse`).
//!
//! Proves the full surface→cells chain: the hand-written reversible
//! Fibonacci (a `swap ; cnot` step wrapped in `iter Nat`) parses, checks,
//! elaborates to a core Iso, and lowers to a k-CSWAP/R1 cell netlist
//! whose untimed evaluation agrees with the interpreter over the ENTIRE
//! finite domain (Nat × Bit × Bit at unroll depth D = 4). It also pins
//! the actual Fibonacci-mod-2 sequence the compiled loop produces.

use theseus_core::Value;

mod common;
use common::*;

const FIB_SRC: &str = include_str!("../../../conformance/programs/0022-fib.thse");

#[test]
fn surface_fib_lowers_and_agrees_with_eval() {
    let iso = elaborate(FIB_SRC, "fib");
    assert_lowering_agrees(&iso, 100_000, None);
}

// ── Pin the compiled Fibonacci-mod-2 sequence ───────────────────────

/// Build the Nat numeral for `k` (Zero / Succ ... at depth D = 4: k ≤ 3).
fn nat(k: u32) -> Value {
    // Nat = Zero | Succ Nat  ⇒  Sum(One, Nat); Zero = Left(unit),
    // Succ n = Right(n).
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

fn bit(b: bool) -> Value {
    if b {
        Value::right(Value::Unit)
    } else {
        Value::left(Value::Unit)
    }
}

#[test]
fn compiled_fib_matches_reference_mod2() {
    let iso = elaborate(FIB_SRC, "fib");
    // Reference: (f_k, f_{k+1}) mod 2 with f_0 = f_1 = 1.
    let mut a = 1u8;
    let mut b = 1u8;
    for k in 0..4u32 {
        // Input ⟨k, (1, 1)⟩; the count is preserved, the pair advances k steps.
        let input = Value::pair(nat(k), Value::pair(bit(true), bit(true)));
        let mut fuel = 100_000u64;
        let out = theseus_eval::eval(&iso, input, &mut fuel).expect("eval");
        // Decode ⟨k, (x, y)⟩.
        let Value::Pair(kout, pair) = out else {
            panic!("expected outer pair, got {out:?}");
        };
        assert_eq!(*kout, nat(k), "iter must preserve the count k");
        let Value::Pair(x, y) = *pair else {
            panic!("expected inner pair");
        };
        assert_eq!(*x, bit(a % 2 == 1), "fib_{k} low bit");
        assert_eq!(*y, bit(b % 2 == 1), "fib_{} low bit", k + 1);
        let next = (a + b) % 2;
        a = b;
        b = next;
    }
    eprintln!("compiled surface fib matches (f_k, f_k+1) mod 2 for k = 0..3");
}
