//! Run-on-CPU differential oracle for the M-A compiler (ADR-0042 §6).
//!
//! For each M-A program, for EVERY value in `enumerate_values(input_ty)`,
//! asserts `cpu_run(iso, v) == theseus_eval::eval(iso, v)`.

use theseus_core::{enumerate_values, type_of, Iso, Type, Value};
use theseus_tisc::compile::{compile, run_on_cpu};

fn cpu_run(iso: &Iso, input: &Value) -> Result<Value, theseus_tisc::compile::CompileError> {
    run_on_cpu(iso, input, theseus_eval::eval)
}

const CNOT_SRC: &str = include_str!("../../../conformance/programs/0004-cnot.thse");
const FIB_SRC: &str = include_str!("../../../conformance/programs/0022-fib.thse");
const FIB3_SRC: &str = include_str!("../../../conformance/programs/0023-fib3.thse");

// ── Surface → core Iso (same pattern as fib_lowering.rs) ─────────────

fn elaborate(src: &str, iso_name: &str) -> Iso {
    let program = theseus_syntax::parse(src).expect("parse");
    let diags = theseus_check::check(&program);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = theseus_check::env::resolve(&program);
    let isos = theseus_check::elaborate::elaborate_program(&program, &env);
    isos.get(iso_name)
        .unwrap_or_else(|| panic!("iso {iso_name:?} not found in elaboration"))
        .clone()
        .unwrap_or_else(|e| panic!("elaboration of {iso_name:?} failed: {e:?}"))
}

// ── Differential oracle helper ───────────────────────────────────────

fn assert_oracle(name: &str, iso: &Iso) {
    let (in_ty, _out_ty) = type_of(iso).expect("iso types");
    let vals = enumerate_values(&in_ty);
    assert!(!vals.is_empty(), "{name}: domain is empty");
    let mut checked = 0usize;
    for v in &vals {
        let cpu_out = cpu_run(iso, v).unwrap_or_else(|e| {
            panic!("{name}: run_on_cpu failed on {v:?}: {e}")
        });
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("{name}: eval failed on {v:?}: {e:?}"));
        assert_eq!(
            cpu_out, eval_out,
            "{name}: CPU disagrees with eval on input {v:?}\n  cpu:  {cpu_out:?}\n  eval: {eval_out:?}"
        );
        checked += 1;
    }
    eprintln!(
        "{name}: oracle agrees on all {checked} inputs (compile → {} instructions)",
        compile(iso).unwrap().len()
    );
}

// ══════════════════════════════════════════════════════════════════════
// (a) cnot — controlled-NOT via the controlled-op idiom → XOR
// ══════════════════════════════════════════════════════════════════════

#[test]
fn cnot_compiles_and_oracle_agrees() {
    let iso = elaborate(CNOT_SRC, "cnot");
    let instrs = compile(&iso).expect("cnot should compile");
    eprintln!("cnot instructions: {instrs:?}");
    // Should contain at least one XOR.
    assert!(
        instrs.iter().any(|i| matches!(i, theseus_tisc::asm::Instr::Xor { .. })),
        "cnot ROM should contain an XOR instruction"
    );
    assert_oracle("cnot", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// (b) bare SwapProd — product swap → EXCH
// ══════════════════════════════════════════════════════════════════════

#[test]
fn swap_compiles_to_exch() {
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::SwapProd(bit.clone(), bit.clone());
    let instrs = compile(&iso).expect("swap should compile");
    assert_eq!(instrs.len(), 1);
    assert!(matches!(instrs[0], theseus_tisc::asm::Instr::Exch { .. }));
    assert_oracle("swap", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// (c) fibStep from 0022-fib.thse (swap2 ; addInto) → EXCH + XOR
// ══════════════════════════════════════════════════════════════════════

#[test]
fn fibstep_compiles_and_oracle_agrees() {
    let iso = elaborate(FIB_SRC, "fibStep");
    let instrs = compile(&iso).expect("fibStep should compile");
    eprintln!("fibStep instructions: {instrs:?}");
    assert_oracle("fibStep", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// (d) 0023-fib3.thse — constant-count (3× fibStep) → oracle agrees
// ══════════════════════════════════════════════════════════════════════

#[test]
fn fib3_compiles_and_oracle_agrees() {
    let iso = elaborate(FIB3_SRC, "fib3");
    let instrs = compile(&iso).expect("fib3 should compile");
    eprintln!("fib3 instructions: {instrs:?}");
    // 3 fibSteps × 2 instrs each = 6 instructions expected.
    assert_oracle("fib3", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// (e) NEGATIVE: data-dependent iter must be rejected
// ══════════════════════════════════════════════════════════════════════

#[test]
fn data_dependent_iter_rejected() {
    let iso = elaborate(FIB_SRC, "fib");
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "data-dependent iter (fib) must be rejected, not miscompiled"
    );
    eprintln!("fib (data-dependent iter) correctly rejected: {}", result.unwrap_err());
}

#[test]
fn bare_trace_rejected() {
    let bit = Type::sum(Type::One, Type::One);
    let body = Iso::SwapSum(bit.clone(), Type::One);
    let iso = Iso::trace(bit, body);
    let result = compile(&iso);
    assert!(result.is_err(), "Trace must be rejected in M-A");
}

#[test]
fn bare_fold_rejected() {
    let bit = Type::sum(Type::One, Type::One);
    let body = Iso::SwapProd(bit.clone(), bit.clone());
    let iso = Iso::fold(Type::list_spine_ty(bit.clone(), 3), body);
    let result = compile(&iso);
    assert!(result.is_err(), "Fold must be rejected in M-A");
}
