#![cfg(feature = "heavy-tests")]
// HEAVY tier (measured >4min): gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! Run-on-CPU differential oracle for the M-A/M-B compiler
//! (ADR-0042 §6, ADR-0043 §5).
//!
//! For each program, for EVERY value in `enumerate_values(input_ty)`,
//! asserts `cpu_run(iso, v) == theseus_eval::eval(iso, v)`.

use theseus_core::{enumerate_values, type_of, Iso, Type, Value};
use theseus_tisc::compile::{
    compile, compile_with_metadata, max_skip_distance, run_on_cpu, run_on_cpu_fast, run_rom_fast,
    run_rom_on_cpu, wcet,
};
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
        let cpu_out =
            run(iso, v).unwrap_or_else(|e| panic!("{name} ({which}): run failed on {v:?}: {e}"));
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
        let cpu_out = run_on_cpu_fast(iso, v)
            .unwrap_or_else(|e| panic!("{name} (fast-bounded): run failed on #{i}: {e}"));
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
        instrs
            .iter()
            .any(|i| matches!(i, theseus_tisc::asm::Instr::Xor { .. })),
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

// bare_trace_rejected: replaced by mdd_bare_trace_compiles (M-D.d A1).

#[test]
fn bare_fold_compiles() {
    let bit = Type::sum(Type::One, Type::One);
    let body = Iso::SwapProd(bit.clone(), bit.clone());
    let iso = Iso::fold(Type::list_spine_ty(bit.clone(), 3), body);
    let instrs = compile(&iso).expect("Fold over list-spine should compile in M-D.a");
    eprintln!(
        "bare fold instructions ({} total): {instrs:?}",
        instrs.len()
    );
    assert_oracle_fast("bare_fold", &iso);
}

#[test]
fn bare_unfold_compiles() {
    let bit = Type::sum(Type::One, Type::One);
    let body = Iso::SwapProd(bit.clone(), bit.clone());
    let iso = Iso::unfold(Type::list_spine_ty(bit.clone(), 3), body);
    let instrs = compile(&iso).expect("Unfold over list-spine should compile in M-D.a");
    eprintln!(
        "bare unfold instructions ({} total): {instrs:?}",
        instrs.len()
    );
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
// B1.3 — universal 3-operand shared bracket: run_on_cpu == eval on a REAL
// program exercising Ccnot + Cswap (+ reg+imm) through the full system_step.
// The ROM is straight-line (BR stays 0), so running exactly B = rom.len()
// steps naturally parks (PC past end, BR=s0=s1=0) — `run_rom_on_cpu`'s
// terminal check (BR=0, s0=0, s1=0) is the CPU-level s0/s1-restoration
// assertion. The real system_step result is compared to the byte-level
// reference interpreter `run_rom_fast` (itself pinned to eval by the ISA
// tests) AND to a hand-computed final register file.
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "authoritative real-CPU system_step; run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn b13_shared_bracket_ccnot_cswap_real_cpu() {
    use theseus_tisc::asm::Instr;
    use theseus_tisc::types::StateVals;

    // A straight-line program mixing the universal-bracket op classes:
    //   0: Addi  r0, #0x0F      ; r0 = 0x0F
    //   1: Xor   r1, r0         ; r1 ^= r0
    //   2: Ccnot r2, r0, r1     ; r2 ^= r0 & r1   (byte-parallel AND)
    //   3: Cswap r3, r4, r5     ; swap r4,r5 iff r3[0]   (r3 odd → taken)
    //   4: Ccnot r6, r4, r5     ; r6 ^= r4 & r5   (after the swap)
    //   5: Sub   r7, r0         ; r7 -= r0
    let prog = [
        Instr::Addi { rd: 0, imm: 0x0F },
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Ccnot { rd: 2, rs: 0, rt: 1 },
        Instr::Cswap { rd: 3, rs: 4, rt: 5 },
        Instr::Ccnot { rd: 6, rs: 4, rt: 5 },
        Instr::Sub { rd: 7, rs: 0 },
    ];
    let budget = prog.len();

    // Sweep several initial register files, incl. r3 even/odd (Cswap both
    // polarities) and dense/edge bytes for the AND lanes.
    let inits: [[u8; 8]; 4] = [
        [0x00, 0xF0, 0xAA, 0x01, 0x12, 0x34, 0x55, 0x80], // r3=0x01 odd → swap taken
        [0x00, 0xF0, 0xAA, 0x02, 0x12, 0x34, 0x55, 0x80], // r3=0x02 even → not taken
        [0xFF, 0x0F, 0xFF, 0x03, 0xFF, 0xFF, 0x00, 0x7F], // dense
        [0x5A, 0xA5, 0x3C, 0x00, 0xC3, 0x1E, 0xE1, 0x81], // r3=0x00 even
    ];

    for (ci, regs0) in inits.iter().enumerate() {
        let init = StateVals {
            regs: *regs0,
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0; 8],
            mem: [0; MEM_WORDS],
        };

        // Byte-level reference interpreter (pinned to eval by the ISA tests).
        let fast = run_rom_fast(
            &theseus_tisc::asm::assemble(&prog).expect("assemble"),
            init.clone(),
            budget,
        );

        // Hand-computed expected register file (independent of both engines).
        let mut r = *regs0;
        r[0] = r[0].wrapping_add(0x0F);
        r[1] ^= r[0];
        r[2] ^= r[0] & r[1];
        if r[3] & 1 == 1 {
            r.swap(4, 5);
        }
        r[6] ^= r[4] & r[5];
        r[7] = r[7].wrapping_sub(r[0]);
        assert_eq!(
            fast.regs, r,
            "case {ci}: run_rom_fast disagrees with hand-computed reference"
        );

        // The AUTHORITATIVE oracle: run the ROM on the REAL system_step iso.
        let real = run_rom_on_cpu(&prog, init, budget, 0, |iso, v, fuel| {
            theseus_eval::eval(iso, v, fuel)
        })
        .unwrap_or_else(|e| panic!("case {ci}: real-CPU run_rom_on_cpu failed: {e}"));

        // real system_step == reference (and thus == eval on these inputs).
        // run_rom_on_cpu already enforced the terminal s0=s1=BR=0 check.
        assert_eq!(
            real.regs, r,
            "case {ci}: REAL system_step register file disagrees with reference\n \
             real={:?}\n ref={:?}",
            real.regs, r
        );
        assert_eq!(real.scratch[0], 0, "case {ci}: s0 must terminate at 0");
        assert_eq!(real.scratch[1], 0, "case {ci}: s1 must terminate at 0");
        assert_eq!(real.br, 0, "case {ci}: BR must terminate at 0");
        eprintln!("B1.3 real-CPU case {ci}: system_step == reference ✓ (regs={:?})", real.regs);
    }
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
    eprintln!(
        "controlled fibStep instructions ({} total): {instrs:?}",
        instrs.len()
    );
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
    eprintln!(
        "swapMaybe instructions ({} total): {instrs:?}",
        instrs.len()
    );
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
        let cpu_out = cpu_run(iso, v)
            .unwrap_or_else(|e| panic!("{name} (real-CPU sample): failed on #{i}: {e}"));
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
                    Box::new(Iso::Times(Box::new(g_body), Box::new(Iso::Id(Type::One)))),
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
    assert!(
        result.is_ok(),
        "15-leaf program should compile with memory (ceiling = {})",
        14 + MEM_WORDS
    );
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
                    Box::new(Iso::Times(Box::new(g_body), Box::new(Iso::Id(Type::One)))),
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
        result.is_ok(),
        "13-leaf branching program should compile with memory (ceiling = {})",
        12 + MEM_WORDS
    );
}

// ══════════════════════════════════════════════════════════════════════
// M-C2 Layer 1 (a): spill-to-memory — programs needing >14 leaves
// ══════════════════════════════════════════════════════════════════════

#[test]
fn mem_spill_15_leaves_swap_oracle_agrees() {
    // 15 leaves = 8 reg + 6 scratch + 1 mem: exercises memory spill.
    let iso = Iso::SwapProd(bit_prod(7), bit_prod(8));
    let instrs = compile(&iso).expect("15-leaf SwapProd should compile with memory");
    eprintln!(
        "15-leaf SwapProd (mem spill): {} instructions",
        instrs.len()
    );
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
    eprintln!(
        "store-reload round-trip: {} instructions on {n} leaves",
        instrs.len()
    );
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
    assert!(
        result.is_err(),
        "{over}-leaf straight-line program must be rejected"
    );
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
                    Box::new(Iso::Times(Box::new(g_body), Box::new(Iso::Id(Type::One)))),
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
    let iso = Iso::times(Iso::Id(bit.clone()), Iso::Iter(nat2, Box::new(body)));
    assert_oracle_fast("times_id_iter", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// M-D.alloc2: doubly-nested skip-block control — rejected not corrupted
// ══════════════════════════════════════════════════════════════════════

#[test]
fn nested_plus_in_plus_compiles_and_runs() {
    // A-4: doubly-nested skip-block control is now ADMITTED (the reservation-
    // ordering hazard is closed by occ-marking each Plus arm's aux before
    // lowering it — inner Plus draws a disjoint aux pair). Was
    // `reject_nested_plus_in_plus`; now must compile + fast-oracle == eval on the
    // whole domain. (Fast oracle keeps the heavy gate bounded — real-CPU terminal
    // -check authority for the newly-admitted nested programs is the advisor's
    // slow `run_on_cpu` job, review Finding 3.)
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::Plus(
        Box::new(Iso::Id(bit.clone())),
        Box::new(Iso::Plus(
            Box::new(Iso::SwapSum(Type::One, Type::One)),
            Box::new(Iso::SwapSum(Type::One, Type::One)),
        )),
    );
    assert!(compile(&iso).is_ok(), "nested Plus must now compile");
    assert_oracle_fast("nested_plus_in_plus", &iso);
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
fn nested_plus_in_iter_compiles_and_runs() {
    // A-4: a Plus (skip-block) inside an Iter body is now ADMITTED — the Iter's
    // guarded-unroll aux {rb,temp} are occ-marked before the body is lowered, so
    // the inner Plus draws a disjoint aux pair. Was `reject_nested_plus_in_iter`;
    // now must compile + fast-oracle == eval on the whole domain (real-CPU
    // authority = advisor, Finding 3).
    let nat2 = Type::numeral_ty(2);
    let iso = Iso::iter(
        nat2,
        Iso::Plus(
            Box::new(Iso::SwapSum(Type::One, Type::One)),
            Box::new(Iso::SwapSum(Type::One, Type::One)),
        ),
    );
    assert!(compile(&iso).is_ok(), "Plus-in-Iter must now compile");
    assert_oracle_fast("nested_plus_in_iter", &iso);
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

// ══════════════════════════════════════════════════════════════════════
// M-D.w: static WCET budget + reversible park-at-end halt convention
// (ADR-0007 TH-1)
// ══════════════════════════════════════════════════════════════════════

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::marshal;
use theseus_tisc::types::{StateVals, NUM_REGS, NUM_SCRATCH};

// ── T2a: park-liveness, straight-line, FAST sim (exhaustive) ────────

#[test]
fn mdw_t2a_park_liveness_straight_line() {
    // Build a small straight-line iso (no skip blocks, S=0).
    // fibStep on Bit×Bit = 2 leaves, purely straight-line.
    let iso = elaborate(FIB_SRC, "fibStep");
    let instrs = compile(&iso).unwrap();
    let rom = assemble(&instrs).unwrap();
    let rom_len = rom.len();
    assert_eq!(
        max_skip_distance(&instrs),
        0,
        "fibStep should be straight-line (no Swapbr)"
    );

    let (in_ty, out_ty) = type_of(&iso).unwrap();
    let vals = enumerate_values(&in_ty);

    for v in &vals {
        let state = marshal(v, &in_ty);
        // Baseline: run for exactly rom.len() steps.
        let baseline = run_rom_fast(&rom, state.clone(), rom_len);
        let baseline_out = theseus_tisc::compile::unmarshal(&baseline, &out_ty);

        // Run for rom.len()+k for every k in the dead zone (up to 256 - rom_len).
        let dead_zone = 256 - rom_len;
        for k in 1..=dead_zone {
            let extended = run_rom_fast(&rom, state.clone(), rom_len + k);
            let extended_out = theseus_tisc::compile::unmarshal(&extended, &out_ty);
            assert_eq!(
                baseline_out, extended_out,
                "park-liveness violated at k={k} for input {v:?}"
            );
        }
    }
}

// ── T2b: park-liveness, skip program, tightness ─────────────────────

#[test]
fn mdw_t2b_park_liveness_skip_tightness() {
    // Demonstrate the park bound is tight using a hand-built ROM with a skip
    // block. The ROM: [Addi{r0,1}, Swapbr{r1}, Addi{r0,1}, Swapbr{r1}]
    // (4 instructions, skip body_len=1, S_max=1).
    // With BR=0 (no skip): executes all 4, r0 += 2. PC ends at 4.
    // Dead zone for non-skip = 256-4 = 252 extra steps.
    // With skip taken (if setup put body_len in BR): skips 1 instr, r0 += 1.
    // PC ends at 4 + 1 = 5. Dead zone = 256 - 5 = 251 = 256 - rom_len - s_max.
    //
    // For this test we use the simpler non-skip path and show:
    // (a) stable for 252 extra steps (whole dead zone)
    // (b) at extra step 253 (total 257 = PC wraps to 1), corruption occurs.
    let instrs = vec![
        Instr::Addi { rd: 0, imm: 1 }, // r0 += 1
        Instr::Swapbr { rd: 1, rs: 0 },       // open skip
        Instr::Addi { rd: 0, imm: 1 }, // skip body: r0 += 1
        Instr::Swapbr { rd: 1, rs: 0 },       // close skip
    ];
    let rom = assemble(&instrs).unwrap();
    let rom_len = rom.len();
    let s_max = max_skip_distance(&instrs);
    assert_eq!(s_max, 1);
    assert_eq!(rom_len, 4);

    // Initial state: all zeros (BR=0 means no skip is taken, just Swapbr pairs
    // swap BR=0 ↔ r1=0 = nop in terms of skip).
    let init = StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };

    // Baseline: run for rom_len steps. r0 should be 2 (both Addi execute).
    let baseline = run_rom_fast(&rom, init.clone(), rom_len);
    assert_eq!(baseline.regs[0], 2);

    // Park-liveness: stable for entire dead zone (252 extra steps).
    let dead_zone = 256 - rom_len; // = 252 for non-skip path
    for k in 1..=dead_zone {
        let extended = run_rom_fast(&rom, init.clone(), rom_len + k);
        assert_eq!(
            extended.regs[0], baseline.regs[0],
            "park-liveness violated at k={k}"
        );
    }

    // WIDEN-PC: the old "tightness" premise is DEAD. Under an 8-bit PC the
    // step at total = rom_len + dead_zone + 2 = 257 wrapped PC to 1 and
    // re-entered the ROM, corrupting r0. Under the 16-bit PC there is NO wrap
    // at 256 — at that step the CPU is STILL PARKED (PC has advanced past the
    // ROM into the high, all-zero PARK region, executing no live ROM word), so
    // r0 stays at the baseline. Park-liveness now extends well past 256 — a
    // POSITIVE widen-PC property (the park bound is the 16-bit PC space, not
    // 256). So the assertion INVERTS: r0 must equal the baseline.
    let over = run_rom_fast(&rom, init.clone(), rom_len + dead_zone + 2);
    assert_eq!(
        over.regs[0], baseline.regs[0],
        "widen-PC: PC no longer wraps at 256; still parked within 16-bit PC space"
    );
}

// ── T3: WCET ledger — pinned golden number ──────────────────────────

#[test]
fn mdw_t3_wcet_ledger() {
    // fibStep: a fixed monotone program. Pin WCET == rom.len() == exact golden number.
    let iso = elaborate(FIB_SRC, "fibStep");
    let instrs = compile(&iso).unwrap();
    let b = wcet(&iso).unwrap();
    let golden = instrs.len();
    assert_eq!(b, golden, "WCET must equal rom.len() for monotone programs");
    assert_eq!(golden, 2, "fibStep golden instruction count pinned at 2");
    assert!(b >= instrs.len(), "WCET invariant B >= rom.len()");
}

// ── T4: two-direction bound — wcet(iso) == wcet(sym(iso)) ──────────

#[test]
fn mdw_t4_two_direction_bound() {
    // ADR-0007 TH-1: wcet must be symmetric for forward/backward.
    // Use self-adjoint isos (involutory) where fwd and bwd compile identically.
    let bit = Type::sum(Type::One, Type::One);
    let isos: Vec<(&str, Iso)> = vec![
        ("swap_bit2", Iso::SwapProd(bit.clone(), bit.clone())),
        ("cnot", elaborate(CNOT_SRC, "cnot")),
        ("swapsum", Iso::SwapSum(Type::One, Type::One)),
    ];
    for (name, iso) in &isos {
        let fwd = wcet(iso).unwrap();
        let bwd = wcet(&Iso::sym(iso.clone())).unwrap();
        assert_eq!(fwd, bwd, "WCET forward != backward for {name}");
    }
}

// ── T5: precondition rejection (negative, hand-built ROMs) ──────────

#[test]
fn mdw_t5_p2_park_safety_overflow() {
    // WIDEN-PC: the OLD premise (B + S_max = 265 > 256 ⇒ P2 rejects) is DEAD.
    // The widen lifted the P2 parked-PC cap from 256 to PC_ADDR_SPACE = 2^16
    // = 65536, so a 135-instr / S_max=130 ROM (B+S_max=265) no longer
    // overflows — it parks fine within the 16-bit PC space. Triggering the
    // real cap with an actual ROM needs B + S_max > 65536, i.e. a >65k-instr
    // ROM, which is infeasible as a unit test (assemble + a 65k-step run).
    //
    // The P2 guard logic itself is UNCHANGED — it is the inline check
    // `budget + s_max_val > PC_ADDR_SPACE` at the top of `run_rom_on_cpu`,
    // evaluated on the passed compositional budget / forward-skip distance
    // BEFORE the run loop. Because those are explicit parameters, we can pin
    // the 2^16 boundary DIRECTLY with a tiny valid ROM and synthetic
    // budget/s_max arguments — no 65k-instr ROM, no 65k-step run (the guard
    // returns Err before the loop). This exercises the real guard at the
    // widen-PC boundary.
    let nop = vec![Instr::Addi { rd: 0, imm: 0 }];
    let init = StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };

    // (a) B + S_max = 265 (the OLD trigger) is now WELL under 2^16 — accepted.
    //     Use budget=1 (run the 1-instr NOP) with s_max=264 so B+S_max=265 but
    //     the run loop only steps once.
    let ok = run_rom_on_cpu(&nop, init.clone(), 1, 264, theseus_eval::eval);
    assert!(
        ok.is_ok(),
        "widen-PC: B+S_max=265 no longer overflows the 2^16 park cap: {ok:?}"
    );

    // (b) Just OVER the 2^16 boundary: budget + s_max = 65537 > 65536. The P2
    //     guard fires BEFORE the run loop (budget large ⇒ no real 65k-step run).
    let over = run_rom_on_cpu(&nop, init.clone(), 65_537, 0, theseus_eval::eval);
    assert!(over.is_err(), "P2 guard must reject B+S_max > 2^16");
    let msg = format!("{}", over.unwrap_err());
    assert!(
        msg.contains("parked PC"),
        "expected the P2 parked-PC guard, got: {msg}"
    );

    // (c) Exactly AT the boundary (B + S_max = 65536) must be accepted (the
    //     cap is strictly-greater-than). Split across budget + s_max so the
    //     run loop stays tiny (budget=1, s_max=65535).
    let at = run_rom_on_cpu(&nop, init, 1, 65_535, theseus_eval::eval);
    assert!(
        at.is_ok(),
        "widen-PC: B+S_max == 2^16 is at the cap, not over it: {at:?}"
    );
}

#[test]
fn mdw_t5_p3_terminal_s0_nonzero() {
    // Hand-built trivial identity ROM (1 NOP), but initial s0 != 0.
    // The NOP is Addi{rd:0, imm:0} which is identity. After 1 step the
    // state is unchanged, so s0 remains nonzero → P3 terminal check fires.
    let instrs = vec![Instr::Addi { rd: 0, imm: 0 }];
    let mut init = StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    init.scratch[0] = 1; // s0 != 0
    let result = run_rom_on_cpu(
        &instrs,
        init,
        instrs.len(),
        max_skip_distance(&instrs),
        theseus_eval::eval,
    );
    assert!(result.is_err(), "P3 guard must reject (s0 != 0)");
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("terminal precondition"),
        "expected P3 error, got: {err_msg}"
    );
}

#[test]
fn mdw_t5_p1_terminal_br_nonzero() {
    // Hand-built ROM: a single Swapbr{rd:0} with r0=5, BR=0.
    // After execution: r0=0, BR=5 → terminal BR != 0 → P1 fires.
    let instrs = vec![Instr::Swapbr { rd: 0, rs: 0 }];
    let mut init = StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    init.regs[0] = 5; // will become BR after Swapbr
    let result = run_rom_on_cpu(
        &instrs,
        init,
        instrs.len(),
        max_skip_distance(&instrs),
        theseus_eval::eval,
    );
    assert!(result.is_err(), "P1 guard must reject (BR != 0)");
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("terminal precondition"),
        "expected P1 error, got: {err_msg}"
    );
}

/// B-2.0b (Fable review F2): keep `system_step`'s non-mem inertness under
/// differential coverage. The MEM-drop routing sends CPU-only + mem-quiescent
/// programs to the mem-free `step` ring, so the `system_step` emit/mem/absorb
/// bracketing (net-identity for non-mem opcodes) otherwise loses coverage. A
/// CPU-only ROM run with NONZERO `init.mem` defeats the guard → routes to
/// `system_step`; assert (a) it leaves mem INERT (untouched — the program
/// never emits a mem op) and (b) its CpuState EQUALS the mem-free `step` run
/// of the same ROM (the two carriers agree — a pure machinery drop).
#[test]
fn mdw_b2_cpu_only_nonzero_mem_routes_system_step_inert() {
    // CPU-only straight-line ROM (no Mexch/MexchReg): Addi r0,#1 ; Xori r1,#0xFF.
    let instrs = vec![
        Instr::Addi { rd: 0, imm: 1 },
        Instr::Xori { rd: 1, imm: 0xFF },
    ];
    let s_max = max_skip_distance(&instrs);
    let mk = |mem: [u8; MEM_WORDS]| StateVals {
        regs: [0x10, 0x20, 0, 0, 0, 0, 0, 0],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem,
    };
    // mem all-zero → mem-free `step` path; nonzero sentinels → `system_step`.
    let step_run = run_rom_on_cpu(
        &instrs,
        mk([0; MEM_WORDS]),
        instrs.len(),
        s_max,
        theseus_eval::eval,
    )
    .expect("CPU-only step path");
    let mut sentinels = [0u8; MEM_WORDS];
    for (i, m) in sentinels.iter_mut().enumerate() {
        *m = 0xA0 + i as u8;
    }
    let sys_run = run_rom_on_cpu(&instrs, mk(sentinels), instrs.len(), s_max, theseus_eval::eval)
        .expect("CPU-only system_step path (nonzero mem)");
    // (a) system_step left mem INERT for the CPU-only program.
    assert_eq!(
        sys_run.mem, sentinels,
        "system_step path corrupted mem for a CPU-only (no-Mexch) program"
    );
    // (b) The two carriers agree on the whole CpuState.
    assert_eq!(sys_run.regs, step_run.regs, "step vs system_step regs differ");
    assert_eq!(sys_run.pc, step_run.pc, "step vs system_step pc differ");
    assert_eq!(sys_run.br, step_run.br, "step vs system_step br differ");
    assert_eq!(
        sys_run.scratch, step_run.scratch,
        "step vs system_step scratch differ"
    );
    // Sanity: the program actually executed (r0=0x10+1, r1=0x20^0xFF).
    assert_eq!(step_run.regs[0], 0x11, "Addi did not run");
    assert_eq!(step_run.regs[1], 0x20 ^ 0xFF, "Xori did not run");
}

/// A-0 SCRATCH-OPERAND authority gap closure (A-5b scoping Piece 1; step-1
/// review M7 + roadmap open-Q1): A-0 made scratch (s2..s7) operand-addressable
/// via the reg∪scratch crossbar, but until now NO real-ring `run_on_cpu`
/// (step / system_step) had EVER executed a scratch-operand instruction — only
/// eval-level `decode_execute` (`blocker2_ccnot_uses_scratch_operand…`). This
/// promotes that to the REAL ring authority: `Ccnot r0, r1, s2` (rt = operand
/// idx 10 = s2) run through both real rings, `r0 ^= r1 & s2`, everything else
/// preserved. Closes the "scratch operand never ran on authority" gap.
#[test]
fn scratch_operand_ccnot_runs_on_real_ring() {
    // Ccnot rd=r0, rs=r1, rt=s2 (operand idx 10): r0 ^= r1 & s2.
    let instrs = vec![Instr::Ccnot { rd: 0, rs: 1, rt: 10 }];
    let s_max = max_skip_distance(&instrs);
    // r1=0xFF, s2=0xAA → r0 (0x00) ^= 0xFF & 0xAA = 0xAA. Distinct sentinels
    // elsewhere so a routing error (reading r2 instead of s2, say) is visible.
    let mk = |mem: [u8; MEM_WORDS]| StateVals {
        regs: [0x00, 0xFF, 0x3C, 0x11, 0x22, 0x33, 0x44, 0x55],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0x00, 0x00, 0xAA, 0xDE, 0xAD, 0xBE, 0xEF, 0x77],
        mem,
    };
    let check = |got: StateVals, ring: &str| {
        assert_eq!(
            got.regs[0], 0xAA,
            "{ring}: Ccnot r0,r1,s2 must give r0 = r1 & s2 = 0xFF & 0xAA = 0xAA \
             (a routing bug reading r2=0x3C would give 0xFF&0x3C=0x3C)"
        );
        assert_eq!(got.regs[1], 0xFF, "{ring}: rs (r1) preserved");
        assert_eq!(got.scratch[2], 0xAA, "{ring}: scratch operand s2 preserved");
        assert_eq!(got.regs[2], 0x3C, "{ring}: r2 (the WRONG operand) untouched");
        assert_eq!(got.scratch[0], 0, "{ring}: s0 restored");
        assert_eq!(got.scratch[1], 0, "{ring}: s1 restored");
        assert_eq!(got.br, 0, "{ring}: BR restored (Ccnot rides rt through BR)");
    };
    // Real ring #1 — CPU-only `step(rom)` (mem-quiescent).
    check(
        run_rom_on_cpu(&instrs, mk([0; MEM_WORDS]), 1, s_max, theseus_eval::eval)
            .expect("scratch-operand Ccnot on step ring"),
        "step",
    );
    // Real ring #2 — `system_step` (forced by nonzero init.mem), which must
    // leave mem inert AND give the identical CpuState result.
    let mut memnz = [0u8; MEM_WORDS];
    memnz[0] = 0x9A;
    let sys = run_rom_on_cpu(&instrs, mk(memnz), 1, s_max, theseus_eval::eval)
        .expect("scratch-operand Ccnot on system_step ring");
    check(sys.clone(), "system_step");
    assert_eq!(sys.mem, memnz, "system_step: mem inert for a CPU-only op");
}

/// A-5b Piece 1 — scratch-`rd` SWAPBR real-ring AUTHORITY. The encoder
/// relaxation (asm.rs) lets `Swapbr` name a scratch operand (s2..s7); the
/// datapath already routes rd over the reg∪scratch A-0 crossbar. This proves a
/// scratch-`rd` SWAPBR executes a REAL branch + come-from on the real ring
/// (`run_rom_on_cpu == eval`), where the come-from token lives in scratch s2 the
/// whole time — the SWAPBR-specific analogue of `scratch_operand_ccnot_…`.
///
/// It mirrors `branching::taken_branch_with_come_from`, but the come-from token
/// rests in s2 (operand idx 10) instead of r0:
///   init:  s2 = δ = 3, BR = 0, all regs 0.
///   PC=0:  SWAPBR s2   — BR ← 3, s2 ← 0.  end-of-step PC = 0 + 3 + 1 = 4 (branch).
///   PC=4:  SWAPBR s2   — come-from: BR ← 0, s2 ← 3 (token restored). PC = 4+0+1 = 5.
///   PC=5:  ADDI r1,0x42 — arrival proof: r1 = 0x42.
/// Indices 1..4 are XORI r7,0 NOPs (identity) so PC=4 lands on the come-from.
/// After PC=5 the CPU parks (BR=0, zero ROM words = ADDI r0,0 identity). Terminal
/// BR/s0/s1 = 0 (checked by run_rom_on_cpu); s2 restored to its input δ.
#[test]
fn scratch_operand_swapbr_runs_on_real_ring() {
    let delta: u8 = 3;
    let target = delta as usize + 1; // = 4 (PC after the branch SWAPBR)
    let mut instrs: Vec<Instr> = vec![
        Instr::Swapbr { rd: 10, rs: 0 }, // 0: branch — BR ← s2(=δ), s2 ← 0
    ];
    while instrs.len() < target {
        instrs.push(Instr::Xori { rd: 7, imm: 0 }); // NOP filler (identity)
    }
    instrs.push(Instr::Swapbr { rd: 10, rs: 0 }); // 4: come-from — BR ← 0, s2 ← δ
    instrs.push(Instr::Addi { rd: 1, imm: 0x42 }); // 5: arrival proof

    // The encoder must now ACCEPT the scratch-rd SWAPBR (the relaxation).
    theseus_tisc::asm::assemble(&instrs).expect("scratch-rd SWAPBR must now assemble");

    let s_max = max_skip_distance(&instrs);
    // Budget: run past the ADDI plus a couple of park steps to prove the CPU
    // idles quiescently after arrival (BR stays 0, s2 stays δ).
    let budget = instrs.len() + 2;

    let mk = |mem: [u8; MEM_WORDS]| StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        // s2 (idx 10 operand) = δ come-from token; distinct sentinels in the
        // OTHER data scratch words so a routing bug (reading s3 instead of s2)
        // is visible.
        scratch: [0x00, 0x00, delta, 0xDE, 0xAD, 0xBE, 0xEF, 0x77],
        mem,
    };

    let check = |got: &StateVals, ring: &str| {
        assert_eq!(
            got.regs[1], 0x42,
            "{ring}: arrival ADDI must have run (r1 = 0x42) — proves the branch \
             SWAPBR jumped to the come-from at PC={target} then fell through to ADDI"
        );
        assert_eq!(
            got.scratch[2], delta,
            "{ring}: come-from must RESTORE the token into s2 (= δ); a datapath \
             gap swapping BR against the wrong word would leave s2 = 0"
        );
        assert_eq!(got.scratch[3], 0xDE, "{ring}: s3 (the WRONG operand) untouched");
        assert_eq!(got.scratch[4], 0xAD, "{ring}: s4 untouched");
        assert_eq!(got.br, 0, "{ring}: BR restored to 0 (come-from cleared)");
        assert_eq!(got.scratch[0], 0, "{ring}: s0 restored");
        assert_eq!(got.scratch[1], 0, "{ring}: s1 restored");
        // No other register perturbed (only r1 got the arrival marker).
        for (i, &r) in got.regs.iter().enumerate() {
            if i != 1 {
                assert_eq!(r, 0, "{ring}: r{i} must stay 0");
            }
        }
    };

    // Cross-check against the byte-level reference sim (pinned to eval).
    let fast = run_rom_fast(
        &theseus_tisc::asm::assemble(&instrs).unwrap(),
        mk([0; MEM_WORDS]),
        budget,
    );
    check(&fast, "run_rom_fast");

    // Real ring #1 — CPU-only `step(rom)` (mem-quiescent).
    let step_run = run_rom_on_cpu(&instrs, mk([0; MEM_WORDS]), budget, s_max, theseus_eval::eval)
        .expect("scratch-rd SWAPBR on step ring");
    check(&step_run, "step");

    // Real ring #2 — `system_step` (forced by nonzero init.mem), which must
    // leave mem inert AND give the identical CpuState result.
    let mut memnz = [0u8; MEM_WORDS];
    memnz[0] = 0x9A;
    let sys = run_rom_on_cpu(&instrs, mk(memnz), budget, s_max, theseus_eval::eval)
        .expect("scratch-rd SWAPBR on system_step ring");
    check(&sys, "system_step");
    assert_eq!(sys.mem, memnz, "system_step: mem inert for a CPU-only program");

    // The two real rings agree on the whole CpuState.
    assert_eq!(sys.regs, step_run.regs, "step vs system_step regs differ");
    assert_eq!(sys.scratch, step_run.scratch, "step vs system_step scratch differ");
    assert_eq!(sys.br, step_run.br, "step vs system_step br differ");
}

// ── T6: real-CPU spot-checks (AUTHORITATIVE, #[ignore]) ────────────

#[test]
#[ignore = "authoritative real-CPU park-liveness (slow); run via `cargo test -p theseus-tisc --test compile -- --ignored`"]
fn mdw_t6_real_cpu_park_liveness() {
    // For the SMALL straight-line program of T2a (fibStep), assert the REAL
    // run_on_cpu gives the same output when run for rom.len(), rom.len()+1,
    // and rom.len()+(dead_zone-1) steps — proving the real iso idles to identity.
    let iso = elaborate(FIB_SRC, "fibStep");
    let instrs = compile(&iso).unwrap();
    let rom_len = instrs.len();
    let dead_zone = 256 - rom_len;

    let (in_ty, out_ty) = type_of(&iso).unwrap();
    let vals = enumerate_values(&in_ty);

    for v in &vals {
        let state = marshal(v, &in_ty);
        let rom = assemble(&instrs).unwrap();
        let step_iso = theseus_tisc::step::system_step(&rom);

        // Helper: run real CPU for `n` steps.
        let run_real = |n: usize| -> StateVals {
            let mut cur = theseus_tisc::types::encode_system(&state);
            let mut fuel = 8_000_000_000u64;
            for _ in 0..n {
                cur = theseus_eval::eval(&step_iso, cur, &mut fuel).unwrap();
            }
            theseus_tisc::types::decode_system(&cur)
        };

        let base = run_real(rom_len);
        let base_out = theseus_tisc::compile::unmarshal(&base, &out_ty);

        // Idle is HOMOGENEOUS on the real iso: each idle step fetches a zero
        // word (Addi r0,0) that adds s0 (=0) into r0 — identity — and never
        // touches s0, so if a few idle steps are identity, all of them are
        // (up to the dead-zone wrap boundary, a fast-sim concern covered
        // exhaustively by mdw_t2a). A few real idle steps therefore suffice to
        // prove the real system_step iso idles to identity (the s0=0 property
        // the fast sim cannot witness). A full 253-step real-CPU sweep is
        // infeasible (~1.7s/step) and redundant — bound the domain (L19).
        let _ = dead_zone;
        for k in [1usize, 2, 3, 8] {
            let ext = run_real(rom_len + k);
            let ext_out = theseus_tisc::compile::unmarshal(&ext, &out_ty);
            assert_eq!(
                base_out, ext_out,
                "real CPU park failed at +{k} for input {v:?}"
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// M-D.d: Trace as reversible while-loop (backward jump + come-from)
// ══════════════════════════════════════════════════════════════════════

use std::collections::BTreeMap;
use theseus_core::prim::PrimDef;

/// Build the 3-cycle body PrimDef on Sum(Bit, One) ↔ Sum(Bit, One).
///
/// The bijection on {L(L()), L(R()), R()} is the 3-cycle:
///   R() → L(R()) → L(L()) → R()   (evaluation trace with t=3)
///
/// In leaf bytes: (a,b) ↦ (a⊕b⊕1, a).
/// Instruction sequence: XOR t,p; XORI t,1; XOR p,t; XORI p,1.
fn build_3cycle_body() -> (Iso, Type) {
    let bit = Type::sum(Type::One, Type::One);
    let carrier_ty = Type::sum(bit.clone(), Type::One);
    let vals = enumerate_values(&carrier_ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    // 3-cycle: L(L()) → R(), R() → L(R()), L(R()) → L(L())
    // In leaf bytes:
    //   (0,0) → (1,0)   L(L()) → R()
    //   (0,1) → (0,0)   L(R()) → L(L())
    //   (1,0) → (0,1)   R()    → L(R())
    for v in &vals {
        let in_bytes = theseus_tisc::compile::marshal(v, &carrier_ty);
        let a = in_bytes.regs[0];
        let b = in_bytes.regs[1];
        let out_a = a ^ b ^ 1;
        let out_b = a;
        // Reconstruct output value from bytes
        let out_state = StateVals {
            regs: {
                let mut r = [0u8; NUM_REGS];
                r[0] = out_a;
                r[1] = out_b;
                r
            },
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0; NUM_SCRATCH],
            mem: [0; MEM_WORDS],
        };
        let out_val = theseus_tisc::compile::unmarshal(&out_state, &carrier_ty);
        forward.insert(v.clone(), out_val.clone());
        backward.insert(out_val, v.clone());
    }

    let prim = PrimDef {
        forward_name: "trace_3cycle".into(),
        backward_name: "trace_3cycle_inv".into(),
        input_ty: carrier_ty.clone(),
        output_ty: carrier_ty.clone(),
        forward,
        backward,
        symbolic: None,
    };

    (Iso::Prim(Box::new(prim)), bit)
}

/// Build an informative Trace body (F-4): the SAME recognized linear map
/// `(a,b) ↦ (a⊕b⊕1, a)` as the 3-cycle, but on the carrier `Sum(Bit, Bit)`
/// (a = b = Bit, so the trace OUTPUT carries a real Bit — unlike the
/// One↔One 3-cycle whose unmarshal is always Unit and cannot witness a
/// wrong payload). `Trace(Bit, body)` is the identity on Bit, but input 0
/// takes t=3 (TWO backward re-entries) and MUST return exactly 0; a
/// corrupted come-from would change the observed output. Input 1 exits at
/// t=1 → 1. This gives the crown-jewel a genuine output-value check.
fn build_informative_trace_body() -> (Iso, Type) {
    let bit = Type::sum(Type::One, Type::One);
    let carrier_ty = Type::sum(bit.clone(), bit.clone());
    let vals = enumerate_values(&carrier_ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let in_bytes = theseus_tisc::compile::marshal(v, &carrier_ty);
        let a = in_bytes.regs[0];
        let b = in_bytes.regs[1];
        let out_a = a ^ b ^ 1;
        let out_b = a;
        let out_state = StateVals {
            regs: {
                let mut r = [0u8; NUM_REGS];
                r[0] = out_a;
                r[1] = out_b;
                r
            },
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0; NUM_SCRATCH],
            mem: [0; MEM_WORDS],
        };
        let out_val = theseus_tisc::compile::unmarshal(&out_state, &carrier_ty);
        forward.insert(v.clone(), out_val.clone());
        backward.insert(out_val, v.clone());
    }

    let prim = PrimDef {
        forward_name: "trace_informative".into(),
        backward_name: "trace_informative_inv".into(),
        input_ty: carrier_ty.clone(),
        output_ty: carrier_ty.clone(),
        forward,
        backward,
        symbolic: None,
    };

    (Iso::Prim(Box::new(prim)), bit)
}

// ── F-4: informative-output Trace (genuine OUTPUT correctness) ──────
// Unlike the One↔One 3-cycle (whose unmarshal is always Unit), this Trace
// has an informative Bit output, so the oracle checks the computed VALUE.

#[test]
fn mdd_informative_trace_fast() {
    let (body, u_ty) = build_informative_trace_body();
    let iso = Iso::trace(u_ty, body);
    let instrs = compile(&iso).expect("informative trace should compile");
    eprintln!("informative trace ({} instrs)", instrs.len());
    assert_oracle_fast("mdd_informative_trace", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow)"]
fn mdd_informative_trace_real_cpu() {
    let (body, u_ty) = build_informative_trace_body();
    let iso = Iso::trace(u_ty, body);
    assert_oracle("mdd_informative_trace", &iso);
}

// ── A1: bare_trace compiles (replaces bare_trace_rejected) ──────────

#[test]
fn mdd_bare_trace_compiles() {
    // Minimal Trace over small finite u, compiles → ROM → run_on_cpu == eval.
    // body: identity on Sum(One, One) = Bit. Trace(One, Id(Sum(One,One))):
    //   body : 1+1 ↔ 1+1, u = One (1 inhabitant), a = b = One.
    //   k_max = |One|+1 = 2. Body is identity → exits on first iteration.
    let u_ty = Type::One;
    let carrier = Type::sum(u_ty.clone(), Type::One);
    let body = Iso::Id(carrier);
    let iso = Iso::trace(u_ty, body);
    let instrs = compile(&iso).expect("bare_trace should compile in M-D.d");
    eprintln!(
        "bare_trace instructions ({} total): {instrs:?}",
        instrs.len()
    );
    assert_oracle_fast("mdd_bare_trace", &iso);
}

// ── A2: 3-cycle maximal witness (THE critical test) ─────────────────

#[test]
fn mdd_3cycle_trace_fast() {
    // u = Bit (|u|=2), body = the 3-cycle on Sum(Bit, One).
    // Evaluation trace for input Unit (a=One): R() → L(R()) → L(L()) → R().
    // That's t=3 = |u|+1 iterations — exercises TWO backward re-entries.
    let (body, u_ty) = build_3cycle_body();
    let iso = Iso::trace(u_ty, body);
    let instrs = compile(&iso).expect("3-cycle trace should compile");
    eprintln!(
        "3-cycle trace instructions ({} total): {instrs:?}",
        instrs.len()
    );

    // Whole-domain oracle (a=b=One, so only 1 input value).
    assert_oracle_fast("mdd_3cycle_trace", &iso);
}

#[test]
#[ignore = "authoritative real-CPU oracle (slow)"]
fn mdd_3cycle_trace_real_cpu() {
    let (body, u_ty) = build_3cycle_body();
    let iso = Iso::trace(u_ty, body);
    assert_oracle("mdd_3cycle_trace", &iso);
}

// ── A3: adjoint — (Trace f)† = Trace(f†) ───────────────────────────

#[test]
fn mdd_adjoint_trace() {
    let (body, u_ty) = build_3cycle_body();
    let iso = Iso::trace(u_ty, body);

    // Forward
    let fwd_out = run_on_cpu_fast(&iso, &Value::Unit).expect("forward trace should succeed");

    // Backward = Sym(Trace(u, body)) = Trace(u, body†)
    let sym_iso = Iso::sym(iso);
    let bwd_out = run_on_cpu_fast(&sym_iso, &fwd_out).expect("adjoint trace should succeed");

    // Adjoint must invert: apply(sym, apply(fwd, x)) == x
    assert_eq!(
        bwd_out,
        Value::Unit,
        "adjoint trace must invert forward trace"
    );
}

// ── A4: WCET golden — wcet > rom.len() ──────────────────────────────

#[test]
fn mdd_wcet_golden() {
    let (body, u_ty) = build_3cycle_body();
    let iso = Iso::trace(u_ty, body);

    let lowered = compile_with_metadata(&iso).expect("trace should compile");
    let b = lowered.wcet();
    let rom_len = lowered.instrs.len();

    // k_max = |Bit|+1 = 3. Body = 4 instructions.
    // iter_len = 4 + LOOP_OVERHEAD = 4 + 10 = 14.
    // spine = 4 (prologue 2 + epilogue 2).
    // rom_len = spine + iter_len = 4 + 14 = 18.
    // wcet = rom_len + (k_max - 1) * iter_len = 18 + 2*14 = 46.
    let expected_iter_len = 14usize;
    let expected_spine = 4usize;
    let expected_rom = expected_spine + expected_iter_len;
    let expected_wcet = expected_rom + (3 - 1) * expected_iter_len;

    assert_eq!(rom_len, expected_rom, "ROM length mismatch");
    assert_eq!(b, expected_wcet, "WCET mismatch");
    assert!(
        b > rom_len,
        "WCET ({b}) must exceed rom.len() ({rom_len}) — \
         the first program where B > rom.len(), proving the budget path is load-bearing"
    );
    assert!(b >= rom_len, "WCET invariant B >= rom.len()");
}

// ── A5: park under a loop ───────────────────────────────────────────

#[test]
fn mdd_park_under_loop() {
    let (body, u_ty) = build_3cycle_body();
    let iso = Iso::trace(u_ty, body);

    let lowered = compile_with_metadata(&iso).expect("trace should compile");
    let budget = lowered.wcet();
    let rom = assemble(&lowered.instrs).expect("assemble");

    // Run for exactly budget steps.
    let input = Value::Unit;
    let (in_ty, out_ty) = type_of(&iso).unwrap();
    let state = marshal(&input, &in_ty);
    let final_state = run_rom_fast(&rom, state.clone(), budget);

    // Assert aux registers are zeroed at exit.
    let n_carrier = 2usize; // num_leaves(Sum(Bit, One)) = 2
    let rc_idx = n_carrier;
    let rb_idx = n_carrier + 1;
    let temp_idx = n_carrier + 2;
    let ctrl_idx = n_carrier + 3;
    assert_eq!(final_state.regs[rc_idx], 0, "rc must be 0 at exit");
    assert_eq!(final_state.regs[rb_idx], 0, "rb must be 0 at exit");
    assert_eq!(final_state.regs[temp_idx], 0, "temp must be 0 at exit");
    assert_eq!(final_state.regs[ctrl_idx], 0, "ctrl must be 0 at exit");
    assert_eq!(final_state.br, 0, "BR must be 0 at exit");
    assert_eq!(final_state.scratch[0], 0, "s0 must be 0 at exit");
    assert_eq!(final_state.scratch[1], 0, "s1 must be 0 at exit");

    // Output must be stable for over-runs into the dead zone.
    let baseline_out = theseus_tisc::compile::unmarshal(&final_state, &out_ty);
    let dead_zone = 256 - budget - lowered.s_max();
    for k in 1..=dead_zone.min(20) {
        let extended = run_rom_fast(&rom, state.clone(), budget + k);
        let ext_out = theseus_tisc::compile::unmarshal(&extended, &out_ty);
        assert_eq!(
            baseline_out, ext_out,
            "park-liveness violated at +{k} for trace"
        );
    }
}

// ── A6: SystemRing closure (mark #[ignore]) ─────────────────────────

#[test]
#[ignore = "ring closure test (slow, OOM-sensitive); run via --ignored"]
fn mdd_system_ring_closure() {
    // Trace ROM closes on the ring (run == eval^R, bus-quiescent).
    // Uses the same sequential-test pattern as mexch_reg to avoid OOM.
    let (body, u_ty) = build_3cycle_body();
    let iso = Iso::trace(u_ty, body);

    let lowered = compile_with_metadata(&iso).expect("trace should compile");
    let rom = assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();

    let step_iso = theseus_tisc::step::system_step(&rom);

    let (in_ty, out_ty) = type_of(&iso).unwrap();
    let vals = enumerate_values(&in_ty);

    for v in &vals {
        let state = marshal(v, &in_ty);
        let mut cur = theseus_tisc::types::encode_system(&state);
        let mut fuel = 8_000_000_000u64;

        // Run R = budget revolutions.
        for _ in 0..budget {
            cur = theseus_eval::eval(&step_iso, cur, &mut fuel).expect("ring step failed");
        }

        let final_state = theseus_tisc::types::decode_system(&cur);
        let cpu_out = theseus_tisc::compile::unmarshal(&final_state, &out_ty);

        let mut eval_fuel = 100_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut eval_fuel).expect("eval failed");

        assert_eq!(
            cpu_out, eval_out,
            "ring closure: CPU disagrees with eval on input {v:?}"
        );
    }
}

// ── A7: reject-don't-corrupt ────────────────────────────────────────

#[test]
fn mdd_reject_wcet_overflow() {
    // Trace with wcet + S_max > 256 → rejected.
    // Use a large |u| so k_max * iter_len > 256.
    // u = Nat256 has 256 inhabitants → k_max = 257. iter_len >= 10.
    // wcet = 4 + 257*10 = 2574 > 256. Rejected.
    let _u_ty = Type::sum(
        Type::One,
        Type::sum(
            Type::One,
            Type::sum(
                Type::One,
                Type::sum(
                    Type::One,
                    Type::sum(
                        Type::One,
                        Type::sum(
                            Type::One,
                            Type::sum(Type::One, Type::sum(Type::One, Type::Zero)),
                        ),
                    ),
                ),
            ),
        ),
    );
    // u = Nat8 (8 inhabitants). k_max = 9. Body = Id → 0 instrs. iter_len = 10.
    // spine = 4. wcet = 4 + 9*10 = 94. S_max = 0. 94 ≤ 256 → should compile.
    // We need a bigger u. Let's try Nat32: 32 inhabitants. k_max=33. iter_len=10.
    // wcet = 4 + 33*10 = 334 > 256 → rejected.
    // Build Nat32 programmatically:
    fn nat_ty(n: usize) -> Type {
        if n == 0 {
            Type::Zero
        } else {
            Type::sum(Type::One, nat_ty(n - 1))
        }
    }
    let u_ty_big = nat_ty(32);
    let carrier = Type::sum(u_ty_big.clone(), Type::One);
    let body = Iso::Id(carrier);
    let iso = Iso::trace(u_ty_big, body);
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "Trace with large carrier must be rejected (payload or register limit)"
    );

    // W_BR-b UPDATE (was a stale reject): u = Bit, body = 126 NOPs (Seq of
    // SwapSum pairs). exact-k_max: body ≡ Id ⇒ T=1, k_exact=2, k_max=min(2,3)=2.
    //
    // History of this case's binding guard:
    //   • pre-widen-PC: "WCET > 256" — DEAD (widen-PC lifted the per-Trace step
    //     budget to PC_ADDR_SPACE = 2^16 in lower_trace).
    //   • W_BR-a: the 8-bit BR back-jump reach guard (δu = 256 − iter_len minted
    //     into an 8-bit BR reaches a genuine backward jump only for iter_len ≤ 128,
    //     so the narrow iter_len 136 > 128 was REJECTED).
    //   • W_BR-b (this change): a Trace whose narrow iter_len exceeds 128 now
    //     auto-LIFTS to the two-byte (w_br=16) come-from (δu = 65536 − iter_len,
    //     cap 32768) instead of rejecting — exactly as `vart_k4_now_lowers_at_wbr16`.
    //     The narrow w_br=8 cap is UNTOUCHED; the loop is simply lifted. So this
    //     case now LOWERS. Its wide iter_len = body(126) + LOOP_OVERHEAD_WIDE(14).
    //     (>32768 cap coverage needs a huge body — deferred to W_BR-c per review.)
    let bit = Type::sum(Type::One, Type::One);
    let carrier_b = Type::sum(bit.clone(), Type::One);
    // Build body = 63 × Seq(SwapSum(Bit, One), SwapSum(One, Bit)) = 126 NOPs.
    let mut body_b: Iso = Iso::Id(carrier_b.clone());
    for _ in 0..63 {
        body_b = Iso::seq(
            body_b,
            Iso::seq(
                Iso::SwapSum(bit.clone(), Type::One),
                Iso::SwapSum(Type::One, bit.clone()),
            ),
        );
    }
    let iso_b = Iso::trace(bit.clone(), body_b);
    let lowered_b =
        compile_with_metadata(&iso_b).expect("iter_len > 128 must LOWER at w_br=16 (W_BR-b)");
    assert_eq!(
        lowered_b.w_br, 16,
        "narrow iter_len > 128 lifts to the two-byte come-from"
    );
    assert_eq!(
        lowered_b.loop_terms[0].iter_len, 140,
        "wide iter_len = body(126) + LOOP_OVERHEAD_WIDE(14) = 140"
    );
}

#[test]
fn mdd_trace_under_times_compiles_and_runs() {
    // FINDING-1 FIX (compositional-control review): this was the F-1 pin
    // `mdd_reject_trace_under_times`. A Trace under a `Times` sibling used to
    // clobber g's data because the TAG register was positional (`Reg(n_input)`),
    // landing exactly on g's first live leaf. The tag + extra-payload padding are
    // now drawn OCC-AWARELY (disjoint from carriers, siblings, and outer aux), so
    // `Times(Trace, g)` no longer aliases — it compiles and runs correctly.
    // Real CPU (P2 + BR/s0/s1 terminal check) == eval on the WHOLE domain is the
    // value here: the aliased-tag orbit, if it existed, would park a nonzero
    // BR/scratch that only the terminal check can see. This program was the exact
    // observation-DEGENERATE case (trace factor : One↔One, sibling Id) that the
    // fast oracle could not certify — so we assert the REAL CPU here.
    //
    // DEGENERATE for Finding-1 coverage — DO NOT count this as the adversarial
    // reproducer. The body here is a self-cancelling One↔One trace with a
    // symmetric Id sibling: a wrong arm / aliased tag is INVISIBLE in the output
    // payload, so this program passes green even if the tag aliases g. It earns
    // its place only as a REAL-CPU witness (the terminal BR/s0/s1 check catches
    // parked residue that the fast oracle cannot). The genuine Finding-1
    // reproducers — asymmetric arms where a wrong arm is VISIBLE in the payload —
    // are `times_trace_nondegenerate_body_runs` (probe1/probe2) and
    // `times_trace_wider_payload_runs` (probe6). If those are ever removed,
    // this test does NOT cover the Finding-1 regression on its own.
    let (body, u_ty) = build_3cycle_body(); // Trace(Bit, 3cycle) : One ↔ One
    let trace = Iso::trace(u_ty, body);
    let g = Iso::Id(Type::sum(Type::One, Type::One)); // Id(Bit) — the sibling factor
    let iso = Iso::times(trace, g); // (One × Bit) ↔ (One × Bit)
    assert!(
        compile(&iso).is_ok(),
        "Times(Trace, g) must now compile (Finding 1: tag/padding occ-aware)"
    );
    assert_oracle("trace_under_times", &iso);
}

#[test]
fn times_trace_nondegenerate_body_runs() {
    // L20 regression for compositional-control Finding 1 (probe1/probe2): the
    // NON-degenerate `Times(Trace, g)` counterexamples the review used to
    // reproduce the BLOCKER. The shipped A-4 vectors were degenerate (body=Id
    // self-cancels; symmetric arms hide a wrong-arm run), so they passed even
    // while the tag aliased g. These use asymmetric arms so a wrong arm is
    // VISIBLE; they must run_on_cpu == eval on the whole domain (esp. sibling
    // input = Right, the aliasing-sensitive case).
    let bit = Type::sum(Type::One, Type::One);
    let flip = Iso::SwapSum(Type::One, Type::One); // Bit ↔ Bit flip (non-self-cancelling)
    // probe1: Times(Trace(Bit, Plus(Id, flip)), flip) — trace factor ≡ flip.
    let left1 = Iso::trace(bit.clone(), Iso::plus(Iso::Id(bit.clone()), flip.clone()));
    let iso1 = Iso::times(left1, flip.clone());
    assert!(compile(&iso1).is_ok(), "probe1 must compile");
    // Fast oracle here (non-degenerate body ⇒ a wrong arm is VISIBLE in the
    // payload, so fast==eval already reproduces the BLOCKER — exactly how the
    // review's probes caught it). Real-CPU terminal-check authority for this
    // family is the advisor's job (Finding 3); the degenerate probe9 program
    // (mdd_trace_under_times below) IS run on the real CPU here.
    assert_oracle_fast("times_trace_probe1", &iso1);
    // probe2: arms swapped — Times(Trace(Bit, Plus(flip, Id)), flip).
    let left2 = Iso::trace(bit.clone(), Iso::plus(flip.clone(), Iso::Id(bit.clone())));
    let iso2 = Iso::times(left2, flip.clone());
    assert!(compile(&iso2).is_ok(), "probe2 must compile");
    assert_oracle_fast("times_trace_probe2", &iso2);
}

#[test]
fn times_trace_wider_payload_runs() {
    // L20 regression for compositional-control Finding 1 (probe6): a WIDER trace
    // payload (a = Bit×Bit) under Times. The tag lands at Reg(n_input) which is
    // still a sibling live leaf for any payload width — the positional bug's
    // second corruption vector. Must run_on_cpu == eval on the whole domain.
    let bit = Type::sum(Type::One, Type::One);
    let flip = Iso::SwapSum(Type::One, Type::One);
    // body : Sum(Bit, Bit×Bit) ↔ Sum(Bit, Bit×Bit) = Plus(Id(Bit), flip⊗Id) — asymmetric.
    let flip_left = Iso::times(flip.clone(), Iso::Id(bit.clone())); // (Bit×Bit) ↔ (Bit×Bit)
    let body = Iso::plus(Iso::Id(bit.clone()), flip_left);
    let left = Iso::trace(bit.clone(), body); // (Bit×Bit) ↔ (Bit×Bit)
    let iso = Iso::times(left, flip); // (Bit×Bit) × Bit ↔ same
    assert!(compile(&iso).is_ok(), "probe6 must compile");
    assert_oracle_fast("times_trace_probe6", &iso);
}

#[test]
fn mdd_nested_trace_compiles_via_aux_reuse() {
    // A-5b (was `mdd_reject_nested_trace`): a depth-2 nested Trace now COMPILES.
    // The inner Trace REUSES the outer level's four aux {rc,rb,temp,ctrl} — dead
    // across the inner region (design §2.2) — rather than needing a disjoint
    // second 4-register frame. Aux cost is O(1) in depth; nesting is bounded by
    // CARRIER + per-level skip-block registers, not by 4·depth aux. This is the
    // L20 reproducing test: it REJECTED before A-5b and compiles now.
    //   inner Trace(One, Id(1+1)) : One ↔ One
    //   Plus(Id(One), inner)      : (1+1) ↔ (1+1)
    //   outer Trace(One, Plus)    : One ↔ One, whose body CONTAINS a Trace.
    let bit = Type::sum(Type::One, Type::One);
    let inner = Iso::trace(Type::One, Iso::Id(bit.clone()));
    let body = Iso::plus(Iso::Id(Type::One), inner);
    let outer = Iso::trace(Type::One, body);
    let lowered =
        compile_with_metadata(&outer).expect("A-5b: depth-2 nested Trace must COMPILE via reuse");
    assert!(
        !lowered.loop_terms.is_empty() && !lowered.loop_terms[0].inner.is_empty(),
        "A-5b: outer loop term must carry a nested inner loop term (depth-2)"
    );
}

#[test]
fn mdd_swapbr_body_compiles_and_runs() {
    // A-4 (R3): a Trace body containing a forward skip-block (Swapbr) is now
    // ADMITTED. `Plus` lowers its arms via emit_skip_block → the SwapSum arm
    // emits a Swapbr; the A-3 Σ-fold sums the body's forward_skip_total into the
    // parent and the k_max walk falls back to the cardinality bound when the body
    // has inner control, so B_fold stays sound. Was `mdd_reject_swapbr_body`.
    // Trace(Bit, Plus(SwapSum(1,1), Id(One))) : body Sum(Bit,One)↔Sum(Bit,One)
    // (u=Bit, a=b=One). Must compile + fast-oracle == eval. The real-CPU
    // P2/terminal-check authority across the back-edge is the advisor's slow
    // run_on_cpu job (Finding 3 — this Trace's loop is expensive on the step-iso
    // CPU); the fast oracle here is the bounded heavy-gate regression.
    let body = Iso::plus(Iso::SwapSum(Type::One, Type::One), Iso::Id(Type::One));
    let iso = Iso::trace(Type::sum(Type::One, Type::One), body);
    assert!(
        compile(&iso).is_ok(),
        "Trace body with a skip-block (Swapbr) must now compile (A-4 R3)"
    );
    assert_oracle_fast("swapbr_body_trace", &iso);
}

#[test]
fn mdd_reject_leaf_count_mismatch() {
    // F-5: a Trace body u+a ↔ u+b whose a/b leaf counts differ is rejected
    // (the carrier is sized for `a`; unmarshal of `b` would misread the tag
    // as a payload leaf). u=One, a=Bit (1 leaf, 2 inhabitants),
    // b=Sum(One,Sum(One,Zero)) (2 leaves, 2 inhabitants). in/out both have 3
    // inhabitants → a valid bijective prim body, but |a|=1 ≠ |b|=2 leaves.
    let one = Type::One;
    let bit = Type::sum(one.clone(), one.clone());
    let b = Type::sum(one.clone(), Type::sum(one.clone(), Type::Zero));
    let in_ty = Type::sum(one.clone(), bit.clone()); // u+a, u=One, a=Bit
    let out_ty = Type::sum(one.clone(), b.clone()); // u+b
    let ins = enumerate_values(&in_ty);
    let outs = enumerate_values(&out_ty);
    assert_eq!(
        ins.len(),
        outs.len(),
        "need equal inhabitants for a bijection"
    );
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for (i, o) in ins.iter().zip(outs.iter()) {
        forward.insert(i.clone(), o.clone());
        backward.insert(o.clone(), i.clone());
    }
    let prim = PrimDef {
        forward_name: "lc_mismatch".into(),
        backward_name: "lc_mismatch_inv".into(),
        input_ty: in_ty,
        output_ty: out_ty,
        forward,
        backward,
        symbolic: None,
    };
    let iso = Iso::trace(one, Iso::Prim(Box::new(prim)));
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "Trace body with |a| != |b| leaves must be rejected (F-5)"
    );
    let msg = format!("{}", result.unwrap_err());
    assert!(
        msg.contains("F-5") || msg.contains("leaf-count"),
        "expected F-5 leaf-count rejection, got: {msg}"
    );
}

// ── F7: reject-don't-corrupt for structural sum-renamings ───────────

#[test]
fn f7_assoc_rsum_nested_rejected() {
    // AssocRSum((1+1)+1) i.e. (a+b)+c ↔ a+(b+c) with a=b=c=One
    // is NOT identity on M-B leaf-byte encoding (sends [0,1]→[1,0],
    // [1,0]→[1,1]). The compiler must reject it, not silently miscompile.
    let one = Type::One;
    let iso = Iso::AssocRSum(one.clone(), one.clone(), one.clone());
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "AssocRSum(One,One,One) must be rejected (F7)"
    );
    let msg = format!("{}", result.unwrap_err());
    assert!(
        msg.contains("F7"),
        "expected F7 deferred rejection, got: {msg}"
    );
}

#[test]
fn f7_assoc_lsum_nested_rejected() {
    // AssocLSum(One,One,One): a+(b+c) → (a+b)+c — also non-identity.
    let one = Type::One;
    let iso = Iso::AssocLSum(one.clone(), one.clone(), one.clone());
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "AssocLSum(One,One,One) must be rejected (F7)"
    );
    let msg = format!("{}", result.unwrap_err());
    assert!(
        msg.contains("F7"),
        "expected F7 deferred rejection, got: {msg}"
    );
}

#[test]
fn f7_assoc_identity_still_compiles() {
    // UniteSum(One): 0+One → One. The only inhabitant is Right(Unit),
    // which maps to Unit. Input encoding: Sum(Zero,One) has
    // num_leaves = 1 + max(0,0) = 1 → [tag=1]. Output encoding: One has
    // num_leaves = 0 → []. After zero-padding to len=1: [1] vs [0...; pad to 1]
    // — actually this IS non-identity because the tag byte 1 doesn't match 0.
    //
    // UnitiSum(One): One → 0+One. Input: [] (One). Output: Right(Unit)
    // in Sum(Zero,One) → [1]. After padding to 1: [0] vs [1]. Non-identity.
    //
    // For truly degenerate cases: UniteSum(Zero) has input Sum(Zero,Zero)
    // which is uninhabited → vacuously identity (no values to check).
    // UnitiSum(Zero) has input Zero → also uninhabited → vacuously identity.
    // These are the only cases that remain no-op.
    let iso_unite_zero = Iso::UniteSum(Type::Zero);
    let result = compile(&iso_unite_zero);
    assert!(
        result.is_ok(),
        "UniteSum(Zero) should compile (vacuously identity): {:?}",
        result.err()
    );
    let instrs = result.unwrap();
    assert_eq!(
        instrs.len(),
        0,
        "UniteSum(Zero) should emit no instructions"
    );

    let iso_uniti_zero = Iso::UnitiSum(Type::Zero);
    let result = compile(&iso_uniti_zero);
    assert!(
        result.is_ok(),
        "UnitiSum(Zero) should compile (vacuously identity): {:?}",
        result.err()
    );
    let instrs = result.unwrap();
    assert_eq!(
        instrs.len(),
        0,
        "UnitiSum(Zero) should emit no instructions"
    );
}

#[test]
fn f7_unite_sum_one_rejected() {
    // UniteSum(One): 0+One → One. The tag byte 1 (Right) doesn't match
    // the empty encoding of One. Non-identity → rejected.
    let iso = Iso::UniteSum(Type::One);
    let result = compile(&iso);
    assert!(result.is_err(), "UniteSum(One) must be rejected (F7)");
    let msg = format!("{}", result.unwrap_err());
    assert!(
        msg.contains("F7"),
        "expected F7 deferred rejection, got: {msg}"
    );
}

#[test]
fn f7_uniti_sum_one_rejected() {
    // UnitiSum(One): One → 0+One. Non-identity (0 leaves → 1 leaf).
    let iso = Iso::UnitiSum(Type::One);
    let result = compile(&iso);
    assert!(result.is_err(), "UnitiSum(One) must be rejected (F7)");
    let msg = format!("{}", result.unwrap_err());
    assert!(
        msg.contains("F7"),
        "expected F7 deferred rejection, got: {msg}"
    );
}

#[test]
fn f7_distrib_nested_rejected() {
    // Distrib(Bit, One, One): Bit × Bit → Bit + Bit.
    // Input Prod(Bit,Bit): [tag_a, tag_bc]. Output Sum(Bit,Bit):
    // [tag_sum, tag_inner, pad]. Non-identity encoding.
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::Distrib(bit, Type::One, Type::One);
    let result = compile(&iso);
    assert!(
        result.is_err(),
        "Distrib(Bit,One,One) must be rejected (F7)"
    );
    let msg = format!("{}", result.unwrap_err());
    assert!(
        msg.contains("F7"),
        "expected F7 deferred rejection, got: {msg}"
    );
}

#[test]
fn f7_factor_nested_rejected() {
    // Factor(Bit, One, One): Bit + Bit → Bit × Bit. Inverse of above.
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::Factor(bit, Type::One, Type::One);
    let result = compile(&iso);
    assert!(result.is_err(), "Factor(Bit,One,One) must be rejected (F7)");
    let msg = format!("{}", result.unwrap_err());
    assert!(
        msg.contains("F7"),
        "expected F7 deferred rejection, got: {msg}"
    );
}

#[test]
fn f7_distrib_trivial_identity() {
    // Distrib(One, One, One): One × (One+One) → (One×One) + (One×One).
    // = One × Bit → Bit. Input: Prod(One, Bit) → [tag]. Output: Sum(One,One) → [tag].
    // (Unit, Left(Unit)) → Left(Pair(Unit,Unit)) → [0]. Identity!
    // (Unit, Right(Unit)) → Right(Pair(Unit,Unit)) → [1]. Identity!
    let one = Type::One;
    let iso = Iso::Distrib(one.clone(), one.clone(), one.clone());
    let result = compile(&iso);
    assert!(
        result.is_ok(),
        "Distrib(One,One,One) should compile (identity): {:?}",
        result.err()
    );
    let instrs = result.unwrap();
    assert_eq!(instrs.len(), 0, "Distrib(One,One,One) should be a no-op");
}

#[test]
fn f7_factor_trivial_identity() {
    // Factor(One, One, One): (One×One) + (One×One) → One × (One+One).
    // = Bit → One × Bit. Inverse of above, also identity.
    let one = Type::One;
    let iso = Iso::Factor(one.clone(), one.clone(), one.clone());
    let result = compile(&iso);
    assert!(
        result.is_ok(),
        "Factor(One,One,One) should compile (identity): {:?}",
        result.err()
    );
    let instrs = result.unwrap();
    assert_eq!(instrs.len(), 0, "Factor(One,One,One) should be a no-op");
}

// ══════════════════════════════════════════════════════════════════════
// Exact-behavior tests for DESIGN-review findings (maestro L21).
// Each reproduces the SPECIFIC behavior a design reviewer flagged.
// ══════════════════════════════════════════════════════════════════════

// M-D.w design finding 1 (BLOCKER): the parked PC after `budget` steps is
// B + S(input), NOT B — it depends on the forward-skip distance the input
// takes. (mdw_t2b only exercised the S=0 path.) Two inputs of one branching
// program that skip different-length arms park at DIFFERENT PCs, both past
// rom.len() — the exact `B+S` arithmetic the reviewer derived.
#[test]
fn mdw_parked_pc_is_b_plus_s_not_b() {
    let one = Type::One;
    // Plus with asymmetric arms: f = 2 instrs, g = 1 instr, so Left and Right
    // inputs skip different lengths.
    let f = Iso::seq(
        Iso::SwapSum(one.clone(), one.clone()),
        Iso::SwapSum(one.clone(), one.clone()),
    ); // Bit ↔ Bit, 2 Xori
    let g = Iso::SwapSum(one.clone(), one.clone()); // Bit ↔ Bit, 1 Xori
    let iso = Iso::plus(f, g); // Sum(Bit, Bit) ↔ Sum(Bit, Bit)
    let instrs = compile(&iso).expect("plus compiles");
    let rom = assemble(&instrs).expect("assemble");
    let rom_len = rom.len();
    let (in_ty, _out) = type_of(&iso).unwrap();

    let left_in = Value::left(Value::left(Value::Unit)); // Left(0)
    let right_in = Value::right(Value::left(Value::Unit)); // Right(0)
    let parked_left = run_rom_fast(&rom, marshal(&left_in, &in_ty), rom_len);
    let parked_right = run_rom_fast(&rom, marshal(&right_in, &in_ty), rom_len);

    // Both idled past rom.len() (a skip was taken), and the two differ because
    // S differs by arm — i.e. parked PC = rom.len() + S(input) = B + S, not B.
    assert!(
        parked_left.pc as usize > rom_len || parked_right.pc as usize > rom_len,
        "an input must idle past rom.len (parked PC = B + S, S>0): L={} R={} rom_len={}",
        parked_left.pc,
        parked_right.pc,
        rom_len
    );
    assert_ne!(
        parked_left.pc, parked_right.pc,
        "parked PC depends on the input's skip distance S (B+S), not just B"
    );
}

// M-D.w design finding 3 (MAJOR): on the REAL system_step iso the idle cell
// (IR=0 → ADDI r0,#0) adds s0 into r0, so with s0!=0 idling CORRUPTS r0 —
// while the fast sim freezes. This is the exact fast-vs-real divergence that
// precondition P3 (s0=0 at terminal) guards. Proves P3's teeth (L20 mutation).
#[test]
#[ignore = "authoritative real-CPU: real idle adds s0 (proves P3's teeth)"]
fn mdw_real_idle_corrupts_r0_when_s0_nonzero() {
    let instrs = vec![Instr::Addi { rd: 0, imm: 0 }]; // 1-instr identity ROM
    let rom = assemble(&instrs).unwrap();
    let init = StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: {
            let mut s = [0u8; NUM_SCRATCH];
            s[0] = 1; // s0 != 0
            s
        },
        mem: [0; MEM_WORDS],
    };
    // REAL iso: r0 accumulates s0 across the (idle) steps.
    let step_iso = theseus_tisc::step::system_step(&rom);
    let mut cur = theseus_tisc::types::encode_system(&init);
    let mut fuel = 8_000_000_000u64;
    for _ in 0..4 {
        cur = theseus_eval::eval(&step_iso, cur, &mut fuel).unwrap();
    }
    let real_final = theseus_tisc::types::decode_system(&cur);
    assert_ne!(
        real_final.regs[0], 0,
        "real idle with s0!=0 must corrupt r0 (adds s0 each step) — why P3 rejects s0!=0"
    );
    // Fast sim: idle freezes r0 (the differential blind spot P3 covers).
    let fast_final = run_rom_fast(&rom, init.clone(), 4);
    assert_eq!(
        fast_final.regs[0], 0,
        "fast sim idle freezes r0 — the divergence the real oracle (and P3) exposes"
    );
}

// M-D.d design F4 (MAJOR): the loop's Ltop..Lend Swapbr pair must NOT be
// counted as a forward skip. The compositional s_max (loop bracket => 0) is
// correct; the old consecutive-Swapbr ROM scan (max_skip_distance) MISPAIRS
// the two loop Swapbrs and counts the body as forward-skip distance — the
// exact bug the compositional path avoids (and why the run budget must use
// the compositional wcet/s_max, not a ROM scan).
#[test]
fn mdd_loop_bracket_not_counted_as_forward_skip() {
    let (body, u_ty) = build_3cycle_body();
    let iso = Iso::trace(u_ty, body);
    let lowered = compile_with_metadata(&iso).expect("trace compiles");
    assert_eq!(
        lowered.s_max(),
        0,
        "compositional S_max: the loop bracket contributes 0 forward-skip distance"
    );
    let scan = max_skip_distance(&lowered.instrs);
    assert!(
        scan > lowered.s_max(),
        "the consecutive-Swapbr ROM scan mispairs the loop bracket (scan={} > compositional={}) \
         — F4: this is why the run budget must NOT derive S_max from a ROM scan",
        scan,
        lowered.s_max()
    );
}
