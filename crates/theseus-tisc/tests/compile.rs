//! Run-on-CPU differential oracle for the M-A/M-B compiler
//! (ADR-0042 §6, ADR-0043 §5).
//!
//! For each program, for EVERY value in `enumerate_values(input_ty)`,
//! asserts `cpu_run(iso, v) == theseus_eval::eval(iso, v)`.

use theseus_core::{enumerate_values, type_of, Iso, Type, Value};
use theseus_tisc::compile::{compile, run_on_cpu, run_on_cpu_fast};
use theseus_tisc::types::MEM_WORDS;

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

// ══════════════════════════════════════════════════════════════════════
// M-C1 (a): pure spill — straight-line programs needing 9–14 leaves
// ══════════════════════════════════════════════════════════════════════

/// Right-nested product of `n` Bits.
fn bit_prod(n: usize) -> Type {
    let bit = Type::sum(Type::One, Type::One);
    if n == 1 {
        bit
    } else {
        Type::prod(bit, bit_prod(n - 1))
    }
}

/// Authoritative oracle on a representative sample (for large domains
/// where exhaustive real-CPU evaluation is infeasible).
fn assert_oracle_sample(name: &str, iso: &Iso, count: usize) {
    let (in_ty, _) = type_of(iso).expect("iso types");
    let vals = enumerate_values(&in_ty);
    let n = vals.len();
    // Evenly spaced indices + boundaries.
    let mut indices: Vec<usize> = (0..count)
        .map(|i| {
            if count <= 1 {
                0
            } else {
                i * (n - 1) / (count - 1)
            }
        })
        .collect();
    indices.push(0);
    if n > 0 {
        indices.push(n - 1);
    }
    indices.sort();
    indices.dedup();

    let mut checked = 0usize;
    for &i in &indices {
        let v = &vals[i];
        let cpu_out = cpu_run(iso, v).unwrap_or_else(|e| {
            panic!("{name} (real-CPU sample): failed on #{i}: {e}")
        });
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("{name}: eval failed on #{i}: {e:?}"));
        assert_eq!(
            cpu_out, eval_out,
            "{name} (real-CPU sample): CPU disagrees with eval on input #{i} {v:?}"
        );
        checked += 1;
    }
    let instr_count = compile(iso).unwrap().len();
    eprintln!(
        "{name} (real-CPU sample): oracle agrees on {checked}/{n} inputs ({instr_count} instructions)"
    );
}

#[test]
fn spill_11_leaves_swap_oracle_agrees() {
    // SwapProd on 11 leaves: exercises spill (leaves 8–10 in scratch).
    let iso = Iso::SwapProd(bit_prod(5), bit_prod(6));
    let instrs = compile(&iso).expect("11-leaf SwapProd should compile");
    eprintln!("11-leaf SwapProd: {} instructions", instrs.len());
    assert_oracle_fast("spill_11_swap", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn spill_11_leaves_swap_real_cpu() {
    assert_oracle_sample(
        "spill_11_swap",
        &Iso::SwapProd(bit_prod(5), bit_prod(6)),
        15,
    );
}

#[test]
fn spill_14_leaves_ceiling_oracle_agrees() {
    // SwapProd on 14 leaves: the straight-line ceiling.
    let iso = Iso::SwapProd(bit_prod(7), bit_prod(7));
    let instrs = compile(&iso).expect("14-leaf SwapProd should compile");
    eprintln!("14-leaf SwapProd: {} instructions", instrs.len());
    assert_oracle_fast("spill_14_ceiling", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn spill_14_leaves_ceiling_real_cpu() {
    assert_oracle_sample(
        "spill_14_ceiling",
        &Iso::SwapProd(bit_prod(7), bit_prod(7)),
        15,
    );
}

// ══════════════════════════════════════════════════════════════════════
// M-C1 (b): spill + branch — controlled-op / iter on >8 leaves
// ══════════════════════════════════════════════════════════════════════

/// Build a controlled NOT on Bit^10 (1 ctrl + 9 target). The g arm
/// flips the first target bit; f = identity.
fn build_spill_controlled_not() -> Iso {
    let bit = Type::sum(Type::One, Type::One);
    let tgt_ty = bit_prod(9);
    let g_body = Iso::Times(
        Box::new(Iso::SwapSum(Type::One, Type::One)),
        Box::new(Iso::Id(bit_prod(8))),
    );
    Iso::seq(
        Iso::SwapProd(bit.clone(), tgt_ty.clone()),
        Iso::seq(
            Iso::Distrib(tgt_ty.clone(), Type::One, Type::One),
            Iso::seq(
                Iso::Plus(
                    Box::new(Iso::Id(Type::prod(tgt_ty.clone(), Type::One))),
                    Box::new(Iso::Times(
                        Box::new(g_body),
                        Box::new(Iso::Id(Type::One)),
                    )),
                ),
                Iso::seq(
                    Iso::Factor(tgt_ty.clone(), Type::One, Type::One),
                    Iso::SwapProd(tgt_ty, bit),
                ),
            ),
        ),
    )
}

#[test]
fn spill_branch_controlled_not_oracle_agrees() {
    let iso = build_spill_controlled_not();
    let instrs = compile(&iso).expect("10-leaf controlled NOT should compile");
    eprintln!("spill+branch controlled NOT: {} instructions", instrs.len());
    assert_oracle_fast("spill_branch_ctrl_not", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn spill_branch_controlled_not_real_cpu() {
    assert_oracle_sample("spill_branch_ctrl_not", &build_spill_controlled_not(), 15);
}

/// Build a data-dependent iter on a value with enough leaves to spill.
/// Iter Nat3 (NOT first bit) on Nat3 × Bit^9 = 11 total leaves.
fn build_spill_iter() -> Iso {
    let body = Iso::Times(
        Box::new(Iso::SwapSum(Type::One, Type::One)),
        Box::new(Iso::Id(bit_prod(8))),
    );
    let nat3 = Type::sum(
        Type::One,
        Type::sum(Type::One, Type::sum(Type::One, Type::Zero)),
    );
    Iso::Iter(nat3, Box::new(body))
}

#[test]
fn spill_branch_iter_oracle_agrees() {
    let iso = build_spill_iter();
    let instrs = compile(&iso).expect("11-leaf iter should compile");
    eprintln!("spill+branch iter: {} instructions", instrs.len());
    assert_oracle_fast("spill_branch_iter", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn spill_branch_iter_real_cpu() {
    assert_oracle_sample("spill_branch_iter", &build_spill_iter(), 15);
}

// ══════════════════════════════════════════════════════════════════════
// M-C1 (c): negative tests — ceiling enforcement
// ══════════════════════════════════════════════════════════════════════

#[test]
fn spill_15_leaves_straight_line_now_compiles() {
    // With M-C2 memory (MEM_WORDS ≥ 1), 15 leaves fit within 14+N.
    // Previously rejected at the M-C1 ceiling of 14.
    let iso = Iso::Id(bit_prod(15));
    let result = compile(&iso);
    assert!(result.is_ok(), "15-leaf program should compile with memory (ceiling = {})", 14 + MEM_WORDS);
}

#[test]
fn spill_13_leaves_branching_now_compiles() {
    // With M-C2 memory (MEM_WORDS ≥ 1), 13 branching leaves fit within 12+N.
    // Previously rejected at the M-C1 branching ceiling of 12.
    let bit = Type::sum(Type::One, Type::One);
    let tgt_ty = bit_prod(12);
    let g_body = Iso::Times(
        Box::new(Iso::SwapSum(Type::One, Type::One)),
        Box::new(Iso::Id(bit_prod(11))),
    );
    let iso = Iso::seq(
        Iso::SwapProd(bit.clone(), tgt_ty.clone()),
        Iso::seq(
            Iso::Distrib(tgt_ty.clone(), Type::One, Type::One),
            Iso::seq(
                Iso::Plus(
                    Box::new(Iso::Id(Type::prod(tgt_ty.clone(), Type::One))),
                    Box::new(Iso::Times(
                        Box::new(g_body),
                        Box::new(Iso::Id(Type::One)),
                    )),
                ),
                Iso::seq(
                    Iso::Factor(tgt_ty.clone(), Type::One, Type::One),
                    Iso::SwapProd(tgt_ty, bit),
                ),
            ),
        ),
    );
    let result = compile(&iso);
    assert!(result.is_ok(), "13-leaf branching program should compile with memory (ceiling = {})", 12 + MEM_WORDS);
}

// ══════════════════════════════════════════════════════════════════════
// M-C2 Layer 1 (a): spill-to-memory — programs needing >14 leaves
// ══════════════════════════════════════════════════════════════════════

#[test]
fn mem_spill_15_leaves_swap_oracle_agrees() {
    // 15 leaves = 8 reg + 6 scratch + 1 mem: exercises memory spill.
    // Previously would have been rejected as TooManyLeaves.
    let iso = Iso::SwapProd(bit_prod(7), bit_prod(8));
    let instrs = compile(&iso).expect("15-leaf SwapProd should compile with memory");
    eprintln!("15-leaf SwapProd (mem spill): {} instructions", instrs.len());
    assert_oracle_fast("mem_spill_15_swap", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn mem_spill_15_leaves_swap_real_cpu() {
    assert_oracle_sample(
        "mem_spill_15_swap",
        &Iso::SwapProd(bit_prod(7), bit_prod(8)),
        12,
    );
}

#[test]
fn mem_spill_max_leaves_swap_oracle_agrees() {
    // 14 + MEM_WORDS leaves: the straight-line ceiling with memory.
    let n = 14 + MEM_WORDS;
    let half = n / 2;
    let iso = Iso::SwapProd(bit_prod(half), bit_prod(n - half));
    let instrs = compile(&iso).expect(&format!("{n}-leaf SwapProd should compile"));
    eprintln!("{n}-leaf SwapProd (max mem): {} instructions", instrs.len());
    assert_oracle_fast("mem_spill_max_ceiling", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn mem_spill_max_leaves_swap_real_cpu() {
    let n = 14 + MEM_WORDS;
    let half = n / 2;
    assert_oracle_sample(
        "mem_spill_max_ceiling",
        &Iso::SwapProd(bit_prod(half), bit_prod(n - half)),
        12,
    );
}

// ══════════════════════════════════════════════════════════════════════
// M-C2 Layer 1 (b): store-then-reload round-trip through memory
// ══════════════════════════════════════════════════════════════════════

#[test]
fn mem_store_reload_round_trip_oracle_agrees() {
    // A product of 15 Bits with a swap that forces data through memory
    // and back. The compiler's marshal puts leaf 14 in mem[0], and any
    // op that touches it must MEXCH it in and out.
    // SwapProd of the last bit with the first exercises the full
    // reg → mem → reg path.
    let n = 15.min(14 + MEM_WORDS);
    let iso = Iso::SwapProd(bit_prod(1), bit_prod(n - 1));
    let instrs = compile(&iso).expect("store-reload should compile");
    eprintln!("store-reload round-trip: {} instructions on {n} leaves", instrs.len());
    assert_oracle_fast("mem_store_reload", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn mem_store_reload_round_trip_real_cpu() {
    let n = 15.min(14 + MEM_WORDS);
    assert_oracle_sample(
        "mem_store_reload",
        &Iso::SwapProd(bit_prod(1), bit_prod(n - 1)),
        12,
    );
}

// ══════════════════════════════════════════════════════════════════════
// M-C2 Layer 1 (c): negative tests — updated ceiling enforcement
// ══════════════════════════════════════════════════════════════════════

#[test]
fn mem_over_ceiling_straight_line_rejected() {
    // 14 + MEM_WORDS + 1 > 14 + MEM_WORDS → TooManyLeaves.
    let over = 14 + MEM_WORDS + 1;
    let iso = Iso::Id(bit_prod(over));
    let result = compile(&iso);
    assert!(result.is_err(), "{over}-leaf straight-line program must be rejected");
    assert!(matches!(
        result.unwrap_err(),
        theseus_tisc::compile::CompileError::TooManyLeaves { .. }
    ));
}

#[test]
fn mem_over_ceiling_branching_rejected() {
    // 12 + MEM_WORDS + 1 leaves with branching → TooManyLeaves.
    let over = 12 + MEM_WORDS + 1;
    let bit = Type::sum(Type::One, Type::One);
    let tgt_ty = bit_prod(over - 1);
    let g_body = Iso::Times(
        Box::new(Iso::SwapSum(Type::One, Type::One)),
        Box::new(Iso::Id(bit_prod(over - 2))),
    );
    let iso = Iso::seq(
        Iso::SwapProd(bit.clone(), tgt_ty.clone()),
        Iso::seq(
            Iso::Distrib(tgt_ty.clone(), Type::One, Type::One),
            Iso::seq(
                Iso::Plus(
                    Box::new(Iso::Id(Type::prod(tgt_ty.clone(), Type::One))),
                    Box::new(Iso::Times(
                        Box::new(g_body),
                        Box::new(Iso::Id(Type::One)),
                    )),
                ),
                Iso::seq(
                    Iso::Factor(tgt_ty.clone(), Type::One, Type::One),
                    Iso::SwapProd(tgt_ty, bit),
                ),
            ),
        ),
    );
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "{over}-leaf branching program must be rejected"
    );
    assert!(matches!(
        result.unwrap_err(),
        theseus_tisc::compile::CompileError::TooManyLeaves { .. }
    ));
}
