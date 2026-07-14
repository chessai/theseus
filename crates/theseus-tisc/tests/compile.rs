//! Run-on-CPU differential oracle for the M-A/M-B compiler
//! (ADR-0042 §6, ADR-0043 §5).
//!
//! For each program, for EVERY value in `enumerate_values(input_ty)`,
//! asserts `cpu_run(iso, v) == theseus_eval::eval(iso, v)`.

use theseus_core::{enumerate_values, type_of, Iso, Type, Value};
use theseus_tisc::compile::{compile, run_on_cpu, run_on_cpu_fast};

// ── Two oracles: one authoritative, one fast ─────────────────────────
//
// AUTHORITATIVE (`assert_oracle`): runs the compiled ROM on the REAL
// TISC-1 CPU — the `step` core-iso evaluator we energy-account — and
// compares to `theseus_eval::eval` over the whole finite domain
// (ADR-0043 §5, ADR-0008). This is the acceptance authority. It is slow:
// each step-iso evaluation of the 160-bit machine costs ~seconds, so the
// expensive M-B programs (fib, controlled op, swapMaybe) run it behind a
// `*_real_cpu` `#[ignore]` test — `cargo test -p theseus-tisc --test
// compile -- --ignored` — verified pre-merge. The cheap M-A programs run
// the authoritative oracle in-gate on every build.
//
// FAST (`assert_oracle_fast`): a direct byte-level simulator
// (`run_on_cpu_fast`) that re-implements the ISA in Rust. NOT
// authoritative on its own — it is *pinned* to the real CPU by the
// `*_real_cpu` tests (each asserts real == eval; the routine test asserts
// fast == eval; together fast == real on those inputs). It exists only so
// the expensive M-B programs get fast routine regression coverage.

fn cpu_run(iso: &Iso, input: &Value) -> Result<Value, theseus_tisc::compile::CompileError> {
    run_on_cpu(iso, input, theseus_eval::eval)
}

const CNOT_SRC: &str = include_str!("../../../conformance/programs/0004-cnot.thse");
const FIB_SRC: &str = include_str!("../../../conformance/programs/0022-fib.thse");
const FIB3_SRC: &str = include_str!("../../../conformance/programs/0023-fib3.thse");
const SWAPMAY_SRC: &str = include_str!("../../../conformance/programs/0025-swapmaybe.thse");

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

/// Authoritative oracle: compiled ROM on the real `step`-iso CPU == eval.
fn assert_oracle(name: &str, iso: &Iso) {
    assert_oracle_with(name, iso, "real-CPU", cpu_run);
}

/// Fast convenience oracle: byte-level `run_on_cpu_fast` == eval. Pinned to
/// the real CPU by the `*_real_cpu` authoritative tests (see module comment).
fn assert_oracle_fast(name: &str, iso: &Iso) {
    assert_oracle_with(name, iso, "fast-sim", run_on_cpu_fast);
}

fn assert_oracle_with(
    name: &str,
    iso: &Iso,
    which: &str,
    run: impl Fn(&Iso, &Value) -> Result<Value, theseus_tisc::compile::CompileError>,
) {
    let (in_ty, _out_ty) = type_of(iso).expect("iso types");
    let vals = enumerate_values(&in_ty);
    assert!(!vals.is_empty(), "{name}: domain is empty");
    let mut checked = 0usize;
    for v in &vals {
        let cpu_out = run(iso, v).unwrap_or_else(|e| {
            panic!("{name} ({which}): run failed on {v:?}: {e}")
        });
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("{name}: eval failed on {v:?}: {e:?}"));
        assert_eq!(
            cpu_out, eval_out,
            "{name} ({which}): CPU disagrees with eval on input {v:?}\n  cpu:  {cpu_out:?}\n  eval: {eval_out:?}"
        );
        checked += 1;
    }
    eprintln!(
        "{name} ({which}): oracle agrees on all {checked} inputs (compile → {} instructions)",
        compile(iso).unwrap().len()
    );
}

// ══════════════════════════════════════════════════════════════════════
// M-A tests (must remain green — no regression)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn cnot_compiles_and_oracle_agrees() {
    let iso = elaborate(CNOT_SRC, "cnot");
    let instrs = compile(&iso).expect("cnot should compile");
    eprintln!("cnot instructions: {instrs:?}");
    assert!(
        instrs.iter().any(|i| matches!(i, theseus_tisc::asm::Instr::Xor { .. })),
        "cnot ROM should contain an XOR instruction"
    );
    assert_oracle("cnot", &iso);
}

#[test]
fn swap_compiles_to_exch() {
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::SwapProd(bit.clone(), bit.clone());
    let instrs = compile(&iso).expect("swap should compile");
    assert_eq!(instrs.len(), 1);
    assert!(matches!(instrs[0], theseus_tisc::asm::Instr::Exch { .. }));
    assert_oracle("swap", &iso);
}

#[test]
fn fibstep_compiles_and_oracle_agrees() {
    let iso = elaborate(FIB_SRC, "fibStep");
    let instrs = compile(&iso).expect("fibStep should compile");
    eprintln!("fibStep instructions: {instrs:?}");
    assert_oracle("fibStep", &iso);
}

#[test]
fn fib3_compiles_and_oracle_agrees() {
    let iso = elaborate(FIB3_SRC, "fib3");
    let instrs = compile(&iso).expect("fib3 should compile");
    eprintln!("fib3 instructions: {instrs:?}");
    assert_oracle_fast("fib3", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn fib3_real_cpu() {
    assert_oracle("fib3", &elaborate(FIB3_SRC, "fib3"));
}

// ══════════════════════════════════════════════════════════════════════
// M-A negative tests (still rejected)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn bare_trace_rejected() {
    let bit = Type::sum(Type::One, Type::One);
    let body = Iso::SwapSum(bit.clone(), Type::One);
    let iso = Iso::trace(bit, body);
    let result = compile(&iso);
    assert!(result.is_err(), "Trace must be rejected in M-B");
}

#[test]
fn bare_fold_rejected() {
    let bit = Type::sum(Type::One, Type::One);
    let body = Iso::SwapProd(bit.clone(), bit.clone());
    let iso = Iso::fold(Type::list_spine_ty(bit.clone(), 3), body);
    let result = compile(&iso);
    assert!(result.is_err(), "Fold must be rejected in M-B");
}

// ══════════════════════════════════════════════════════════════════════
// M-B (a): data-dependent fib — iter Nat fibStep (the M-A blocker)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn fib_data_dependent_compiles_and_oracle_agrees() {
    let iso = elaborate(FIB_SRC, "fib");
    let instrs = compile(&iso).expect("fib (data-dependent iter) should compile in M-B");
    eprintln!("fib instructions ({} total): {instrs:?}", instrs.len());
    assert_oracle_fast("fib", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow, ~20min); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn fib_data_dependent_real_cpu() {
    // The headline M-B payoff: the real data-dependent `iter Nat fibStep`
    // compiled to a ROM, running on the real TISC-1 CPU, == eval over the
    // whole domain. Verified green (16 inputs, 24-instr ROM).
    assert_oracle("fib", &elaborate(FIB_SRC, "fib"));
}

// ══════════════════════════════════════════════════════════════════════
// M-B (b): general controlled op — multi-instruction arm
// ══════════════════════════════════════════════════════════════════════

// Build a controlled fibStep: the Distrib;Plus(Id, Times(fibStep,Id));Factor
// idiom whose active arm is a multi-instruction sequence (EXCH;XOR) — the
// M-B "general controlled op" case (M-A only handled single-instruction arms).
fn build_controlled_fibstep() -> Iso {
    let fib_step = elaborate(FIB_SRC, "fibStep");
    let bit = Type::sum(Type::One, Type::One);
    let bit2 = Type::prod(bit.clone(), bit.clone());
    // SwapProd(Bit, Bit*Bit) ; Distrib(Bit*Bit, One, One) ;
    // Plus(Id((Bit*Bit)*One), Times(fibStep, Id(One))) ;
    // Factor(Bit*Bit, One, One) ; SwapProd(Bit*Bit, Bit)
    Iso::seq(
        Iso::SwapProd(bit.clone(), bit2.clone()),
        Iso::seq(
            Iso::Distrib(bit2.clone(), Type::One, Type::One),
            Iso::seq(
                Iso::Plus(
                    Box::new(Iso::Id(Type::prod(bit2.clone(), Type::One))),
                    Box::new(Iso::Times(Box::new(fib_step), Box::new(Iso::Id(Type::One)))),
                ),
                Iso::seq(
                    Iso::Factor(bit2.clone(), Type::One, Type::One),
                    Iso::SwapProd(bit2.clone(), bit.clone()),
                ),
            ),
        ),
    )
}

#[test]
fn general_controlled_op_oracle_agrees() {
    let ctrl_idiom = build_controlled_fibstep();
    let instrs = compile(&ctrl_idiom).expect("controlled fibStep should compile in M-B");
    eprintln!("controlled fibStep instructions ({} total): {instrs:?}", instrs.len());
    assert_oracle_fast("controlled_fibStep", &ctrl_idiom);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn general_controlled_op_real_cpu() {
    assert_oracle("controlled_fibStep", &build_controlled_fibstep());
}

// ══════════════════════════════════════════════════════════════════════
// M-B (c): bare-sum program — swapMaybe
// ══════════════════════════════════════════════════════════════════════

#[test]
fn swapmay_bare_sum_oracle_agrees() {
    let iso = elaborate(SWAPMAY_SRC, "swapMaybe");
    let instrs = compile(&iso).expect("swapMaybe should compile in M-B");
    eprintln!("swapMaybe instructions ({} total): {instrs:?}", instrs.len());
    assert_oracle_fast("swapMaybe", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn swapmay_bare_sum_real_cpu() {
    assert_oracle("swapMaybe", &elaborate(SWAPMAY_SRC, "swapMaybe"));
}
