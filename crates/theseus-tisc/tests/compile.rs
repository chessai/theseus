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
const PARITY_SRC: &str = include_str!("../../../conformance/programs/0019-fold-parity.thse");
const SHIFT_SRC: &str = include_str!("../../../conformance/programs/0020-unfold-shift.thse");

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

/// Bounded fast oracle: strided ≤256-input sample of run_on_cpu_fast == eval.
/// Mirrors `assert_oracle_sample` index-striding but uses `run_on_cpu_fast`.
fn assert_oracle_fast_bounded(name: &str, iso: &Iso, cap: usize) {
    let (in_ty, _) = type_of(iso).expect("iso types");
    let vals = enumerate_values(&in_ty);
    let n = vals.len();
    let count = cap.min(n);
    let mut indices: Vec<usize> = (0..count)
        .map(|i| {
            if count <= 1 { 0 } else { i * (n - 1) / (count - 1) }
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
        let cpu_out = run_on_cpu_fast(iso, v).unwrap_or_else(|e| {
            panic!("{name} (fast-bounded): run failed on #{i}: {e}")
        });
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("{name}: eval failed on #{i}: {e:?}"));
        assert_eq!(
            cpu_out, eval_out,
            "{name} (fast-bounded): CPU disagrees with eval on input #{i} {v:?}"
        );
        checked += 1;
    }
    eprintln!(
        "{name} (fast-bounded): oracle agrees on {checked}/{n} inputs ({} instructions)",
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
fn bare_fold_compiles() {
    let bit = Type::sum(Type::One, Type::One);
    let body = Iso::SwapProd(bit.clone(), bit.clone());
    let iso = Iso::fold(Type::list_spine_ty(bit.clone(), 3), body);
    let instrs = compile(&iso).expect("Fold over list-spine should compile in M-D.a");
    eprintln!("bare fold instructions ({} total): {instrs:?}", instrs.len());
    assert_oracle_fast("bare_fold", &iso);
}

#[test]
fn bare_unfold_compiles() {
    let bit = Type::sum(Type::One, Type::One);
    let body = Iso::SwapProd(bit.clone(), bit.clone());
    let iso = Iso::unfold(Type::list_spine_ty(bit.clone(), 3), body);
    let instrs = compile(&iso).expect("Unfold over list-spine should compile in M-D.a");
    eprintln!("bare unfold instructions ({} total): {instrs:?}", instrs.len());
    assert_oracle_fast("bare_unfold", &iso);
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
    assert_oracle_fast_bounded("spill_14_ceiling", &iso, 256);
}

#[test]
#[ignore = "authoritative whole-domain oracle; run via --ignored"]
fn spill_14_leaves_ceiling_oracle_whole_domain() {
    let iso = Iso::SwapProd(bit_prod(7), bit_prod(7));
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
    let iso = Iso::SwapProd(bit_prod(7), bit_prod(8));
    let instrs = compile(&iso).expect("15-leaf SwapProd should compile with memory");
    eprintln!("15-leaf SwapProd (mem spill): {} instructions", instrs.len());
    assert_oracle_fast_bounded("mem_spill_15_swap", &iso, 256);
}

#[test]
#[ignore = "authoritative whole-domain oracle; run via --ignored"]
fn mem_spill_15_leaves_swap_oracle_whole_domain() {
    let iso = Iso::SwapProd(bit_prod(7), bit_prod(8));
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
    let instrs = compile(&iso).unwrap_or_else(|_| panic!("{n}-leaf SwapProd should compile"));
    eprintln!("{n}-leaf SwapProd (max mem): {} instructions", instrs.len());
    assert_oracle_fast_bounded("mem_spill_max_ceiling", &iso, 256);
}

#[test]
#[ignore = "authoritative whole-domain oracle; run via --ignored"]
fn mem_spill_max_leaves_swap_oracle_whole_domain() {
    let n = 14 + MEM_WORDS;
    let half = n / 2;
    let iso = Iso::SwapProd(bit_prod(half), bit_prod(n - half));
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
    // and back.
    let n = 15.min(14 + MEM_WORDS);
    let iso = Iso::SwapProd(bit_prod(1), bit_prod(n - 1));
    let instrs = compile(&iso).expect("store-reload should compile");
    eprintln!("store-reload round-trip: {} instructions on {n} leaves", instrs.len());
    assert_oracle_fast_bounded("mem_store_reload", &iso, 256);
}

#[test]
#[ignore = "authoritative whole-domain oracle; run via --ignored"]
fn mem_store_reload_round_trip_oracle_whole_domain() {
    let n = 15.min(14 + MEM_WORDS);
    let iso = Iso::SwapProd(bit_prod(1), bit_prod(n - 1));
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

// ══════════════════════════════════════════════════════════════════════
// M-D.a: Fold/Unfold over list-spine (guarded static unroll)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn parity_fold_compiles_and_oracle_agrees() {
    let iso = elaborate(PARITY_SRC, "parity");
    let instrs = compile(&iso).expect("parity (fold) should compile in M-D.a");
    eprintln!("parity instructions ({} total): {instrs:?}", instrs.len());
    // BitList@4 * Bit = 8 spine + 1 acc = 9 leaves → 2^9 = 512 inputs (whole-domain).
    assert_oracle_fast("parity", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn parity_fold_real_cpu() {
    assert_oracle("parity", &elaborate(PARITY_SRC, "parity"));
}

#[test]
fn shift_unfold_compiles_and_oracle_agrees() {
    let iso = elaborate(SHIFT_SRC, "shl");
    let instrs = compile(&iso).expect("shl (unfold) should compile in M-D.a");
    eprintln!("shl instructions ({} total): {instrs:?}", instrs.len());
    // BitList@4 * Bit = 9 leaves → 512 inputs (whole-domain).
    assert_oracle_fast("shl", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn shift_unfold_real_cpu() {
    assert_oracle("shl", &elaborate(SHIFT_SRC, "shl"));
}

#[test]
fn fold_sym_agrees_with_unfold_eval() {
    // Adjoint check: compile(Sym(fold l f)) → lower invokes adjoint → Unfold(l, f†).
    // Verify the compiled adjoint agrees with eval over the whole domain.
    let parity = elaborate(PARITY_SRC, "parity");
    let sym_parity = Iso::sym(parity);
    assert_oracle_fast("sym_parity", &sym_parity);
}

// ── M-D.a soundness guards: counterexample rejection tests ──────────

#[test]
fn counterexample_times_unfold_rejected() {
    // Times(Id(Bit), Unfold(list_spine_ty(Bit,4), swp)) — the unfold sits
    // under a Times split so its ctx is displaced (non-canonical). Guard 1
    // must reject this rather than silently corrupting the ROM.
    let bit = Type::sum(Type::One, Type::One);
    let swp = Iso::SwapProd(bit.clone(), bit.clone());
    let iso = Iso::times(
        Iso::Id(bit.clone()),
        Iso::unfold(Type::list_spine_ty(bit.clone(), 4), swp),
    );
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "Times(Id(Bit), Unfold(..)) must be rejected (non-canonical ctx)"
    );
}

#[test]
fn counterexample_nested_fold_unfold_rejected() {
    // Fold(list_spine_ty(Bit,2), Times(Id(Bit), Unfold(list_spine_ty(Bit,2), swp)))
    // — the inner Unfold is nested inside a fold body's sub-context under Times.
    // Guard 1 must reject this (the Unfold sees a non-canonical ctx).
    let bit = Type::sum(Type::One, Type::One);
    let swp = Iso::SwapProd(bit.clone(), bit.clone());
    let inner_unfold = Iso::unfold(Type::list_spine_ty(bit.clone(), 2), swp);
    let body = Iso::times(Iso::Id(bit.clone()), inner_unfold);
    let iso = Iso::fold(Type::list_spine_ty(bit.clone(), 2), body);
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "Fold(..Times(Id,Unfold(..))) must be rejected (nested non-canonical ctx)"
    );
}

// ── M-D.a direction coverage: non-self-adjoint, order-sensitive body ─

#[test]
fn fold_non_self_adjoint_body_oracle() {
    // Seq(swp, cnot) is NOT self-adjoint (adj = Seq(cnot, swp) ≠ Seq(swp, cnot))
    // and is order-sensitive, so fold vs unfold actually differ. Verify the
    // compiled fold agrees with eval over the whole domain.
    // NOTE: swp must come BEFORE cnot so that right_assoc_seq in
    // lower_fold_unfold does not flatten the controlled-op idiom chain
    // (cnot is Seq(SwapProd, Seq(Distrib, Seq(Plus, Seq(Factor, SwapProd))));
    // appending swp after it merges the trailing SwapProd and destroys the
    // idiom match, causing a spurious Swapbr rejection).
    let bit = Type::sum(Type::One, Type::One);
    let cnot = elaborate(CNOT_SRC, "cnot");
    let swp = Iso::SwapProd(bit.clone(), bit.clone());
    let body = Iso::seq(swp, cnot);
    let l = Type::list_spine_ty(bit.clone(), 4);
    let iso = Iso::fold(l, body);
    assert_oracle_fast("fold_seq_swp_cnot", &iso);
}

#[test]
fn unfold_non_self_adjoint_body_oracle() {
    // Unfold with the same non-self-adjoint body Seq(swp, cnot).
    // Verify the compiled unfold agrees with eval over the whole domain.
    let bit = Type::sum(Type::One, Type::One);
    let cnot = elaborate(CNOT_SRC, "cnot");
    let swp = Iso::SwapProd(bit.clone(), bit.clone());
    let body = Iso::seq(swp, cnot);
    let l = Type::list_spine_ty(bit.clone(), 4);
    let iso = Iso::unfold(l, body);
    assert_oracle_fast("unfold_seq_swp_cnot", &iso);
}

#[test]
fn sym_shl_oracle() {
    // Sym(shl) = Fold(list_spine_ty(Bit,4), adj(swp)) = Fold(.., swp) since
    // swp is self-adjoint. Verify compiled adjoint of the unfold agrees with eval.
    let shl = elaborate(SHIFT_SRC, "shl");
    let sym_shl = Iso::sym(shl);
    assert_oracle_fast("sym_shl", &sym_shl);
}

// ── M-D.a deep spine: scratch-resident tags ─────────────────────────

#[test]
fn deep_spine_fold_oracle() {
    // Fold over list_spine_ty(Bit, 6): 12 spine leaves + 1 acc = 13 total.
    // Tags at leaves 0, 2, 4, 6, 8, 10 — leaves 8+ spill to scratch.
    // Bounded fast oracle (2^13 = 8192 values → strided ≤256 sample).
    let bit = Type::sum(Type::One, Type::One);
    let swp = Iso::SwapProd(bit.clone(), bit.clone());
    let l = Type::list_spine_ty(bit.clone(), 6);
    let iso = Iso::fold(l, swp);
    compile(&iso).expect("deep-spine fold should compile");
    assert_oracle_fast_bounded("deep_spine_fold", &iso, 256);
}

#[test]
fn deep_spine_unfold_oracle() {
    // Unfold over list_spine_ty(Bit, 6): same as above but tail-to-head.
    let bit = Type::sum(Type::One, Type::One);
    let swp = Iso::SwapProd(bit.clone(), bit.clone());
    let l = Type::list_spine_ty(bit.clone(), 6);
    let iso = Iso::unfold(l, swp);
    compile(&iso).expect("deep-spine unfold should compile");
    assert_oracle_fast_bounded("deep_spine_unfold", &iso, 256);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow)"]
fn deep_spine_fold_real_cpu() {
    let bit = Type::sum(Type::One, Type::One);
    let swp = Iso::SwapProd(bit.clone(), bit.clone());
    let l = Type::list_spine_ty(bit.clone(), 6);
    let iso = Iso::fold(l, swp);
    assert_oracle("deep_spine_fold", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// M-D.alloc1: L20 regression — occupancy-aware allocation fixes
// ══════════════════════════════════════════════════════════════════════

#[test]
fn times_id_plus_id_swapsum_l20_regression() {
    // L20 regression: Times(Id(Bit), Plus(Id(Bit), SwapSum(One,One)))
    // silently miscompiled before occupancy-aware allocation.
    // ⟨1, inl 0⟩ → ⟨0, inl 0⟩ (sibling bit clobbered by aux reg overlap).
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::times(
        Iso::Id(bit.clone()),
        Iso::Plus(
            Box::new(Iso::Id(bit.clone())),
            Box::new(Iso::SwapSum(Type::One, Type::One)),
        ),
    );
    assert_oracle_fast("times_id_plus_id_swapsum", &iso);
}

#[test]
fn times_id_iter_l20_regression() {
    // L20 regression: Times(Id(Bit), Iter(Nat2, NOT)) — iter under Times
    // with a sibling bit. Without occ-aware allocation, the sibling's
    // register was treated as free and used as aux, corrupting data.
    let bit = Type::sum(Type::One, Type::One);
    let nat2 = Type::sum(Type::One, Type::sum(Type::One, Type::Zero));
    let body = Iso::SwapSum(Type::One, Type::One);
    let iso = Iso::times(
        Iso::Id(bit.clone()),
        Iso::Iter(nat2, Box::new(body)),
    );
    assert_oracle_fast("times_id_iter", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// M-D.alloc2: doubly-nested skip-block control — rejected not corrupted
// ══════════════════════════════════════════════════════════════════════

#[test]
fn reject_nested_plus_in_plus() {
    // doubly-nested skip-block control deferred (reservation ordering);
    // rejected not corrupted
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::Plus(
        Box::new(Iso::Id(bit.clone())),
        Box::new(Iso::Plus(
            Box::new(Iso::SwapSum(Type::One, Type::One)),
            Box::new(Iso::SwapSum(Type::One, Type::One)),
        )),
    );
    assert!(compile(&iso).is_err());
}

#[test]
fn reject_nested_plus_with_trailing() {
    // doubly-nested skip-block control deferred (reservation ordering);
    // rejected not corrupted
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::Seq(
        Box::new(Iso::Plus(
            Box::new(Iso::Id(bit.clone())),
            Box::new(Iso::Plus(
                Box::new(Iso::SwapSum(Type::One, Type::One)),
                Box::new(Iso::Id(bit.clone())),
            )),
        )),
        Box::new(Iso::SwapSum(Type::One, Type::One)),
    );
    assert!(compile(&iso).is_err());
}

#[test]
fn reject_nested_plus_in_iter() {
    // doubly-nested skip-block control deferred (reservation ordering);
    // rejected not corrupted
    let nat2 = Type::numeral_ty(2);
    let iso = Iso::iter(
        nat2,
        Iso::Plus(
            Box::new(Iso::SwapSum(Type::One, Type::One)),
            Box::new(Iso::SwapSum(Type::One, Type::One)),
        ),
    );
    assert!(compile(&iso).is_err());
}

#[test]
fn reject_fold_containing_unfold() {
    // doubly-nested skip-block control deferred (reservation ordering);
    // rejected not corrupted (already rejected by fold Guard 2)
    let bit = Type::sum(Type::One, Type::One);
    let l = Type::list_spine_ty(bit.clone(), 2);
    let iso = Iso::fold(
        l.clone(),
        Iso::times(
            Iso::Id(bit.clone()),
            Iso::unfold(l, Iso::SwapProd(bit.clone(), bit.clone())),
        ),
    );
    assert!(compile(&iso).is_err());
}
