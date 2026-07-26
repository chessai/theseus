//! TP-3: Width-4 Galois LFSR (x^4+x+1) iterated K steps via `iter Nat@6`.
//!
//! The longest-yet-compiled iter with the lowest-branching body: 4 straight-line
//! instructions per step (1 Xor + 3 Exch), zero Swapbr in the body.
//! Domain = 6×16 = 96 inputs; ROM = 104 instrs; wcet B = 104; 10 Swapbr total
//! (2 guard × 5 copies).
//!
//! Tests map to tp3-lfsr-design §3 (L20/L21).

use theseus_core::{enumerate_values, type_of, Iso, Value};
use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::{compile, compile_with_metadata, marshal, run_on_cpu, run_on_cpu_fast};

// ── Surface → core Iso ──────────────────────────────────────────────

const LFSR_SRC: &str = include_str!("../../../conformance/programs/0026-lfsr.thse");

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

// ── Differential oracle helpers ─────────────────────────────────────

fn cpu_run(iso: &Iso, input: &Value) -> Result<Value, theseus_tisc::compile::CompileError> {
    run_on_cpu(iso, input, theseus_eval::eval)
}

fn assert_oracle_fast(name: &str, iso: &Iso) {
    assert_oracle_with(name, iso, "fast-sim", run_on_cpu_fast);
}

#[allow(dead_code)]
fn assert_oracle(name: &str, iso: &Iso) {
    assert_oracle_with(name, iso, "real-CPU", cpu_run);
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

// ══════════════════════════════════════════════════════════════════════
// T1: compiles + low-branching emission
// ══════════════════════════════════════════════════════════════════════

#[test]
fn t1_lfsr_compiles_and_emission() {
    // lfsrStep must be Swapbr-free (straight-line body), exactly 4 instructions.
    let lfsr_step = elaborate(LFSR_SRC, "lfsrStep");
    let step_instrs = compile(&lfsr_step).expect("lfsrStep should compile");
    assert_eq!(
        step_instrs.len(),
        4,
        "lfsrStep must be 4 instructions (1 Xor + 3 Exch), got {}",
        step_instrs.len()
    );
    let step_swapbr_count = step_instrs
        .iter()
        .filter(|i| matches!(i, Instr::Swapbr { .. }))
        .count();
    assert_eq!(
        step_swapbr_count, 0,
        "lfsrStep must be Swapbr-free (straight-line), got {step_swapbr_count}"
    );

    // Full lfsr program.
    let iso = elaborate(LFSR_SRC, "lfsr");
    let instrs = compile(&iso).expect("lfsr should compile");

    // ROM length == 104 (MEASURED).
    assert_eq!(
        instrs.len(),
        104,
        "lfsr ROM length must be 104, got {}",
        instrs.len()
    );

    // Swapbr count == 2 * (D-1) = 2 * 5 = 10.
    let swapbr_count = instrs
        .iter()
        .filter(|i| matches!(i, Instr::Swapbr { .. }))
        .count();
    assert_eq!(
        swapbr_count, 10,
        "lfsr Swapbr count must be 10 (2 guard × 5 copies), got {swapbr_count}"
    );
}

// ══════════════════════════════════════════════════════════════════════
// T2: whole-domain oracle (fast sim == eval over all 96 inputs)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn t2_lfsr_whole_domain_oracle() {
    let iso = elaborate(LFSR_SRC, "lfsr");
    let (in_ty, _) = type_of(&iso).unwrap();
    let vals = enumerate_values(&in_ty);
    assert_eq!(
        vals.len(),
        96,
        "lfsr domain must be 96 (6×16), got {}",
        vals.len()
    );
    assert_oracle_fast("lfsr", &iso);
}

// ══════════════════════════════════════════════════════════════════════
// T3: real CPU oracle — maximal k=5 trajectory from seed 0b0001
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --test lfsr -- --ignored`"]
fn t3_lfsr_real_cpu() {
    let iso = elaborate(LFSR_SRC, "lfsr");

    // Rescoped: single maximal-k=5 trajectory from nonzero seed 0b0001 only
    // (whole-domain sweep takes hours — design explicitly rejected it).
    let seed = 1u8;
    let expected_outputs: [u8; 5] = [9, 13, 15, 14, 7];
    for (k_idx, &expected_out) in expected_outputs.iter().enumerate() {
        let k = k_idx + 1; // k=1..=5
        let input = Value::pair(encode_nat6(k), encode_reg(seed));
        let cpu_out =
            cpu_run(&iso, &input).unwrap_or_else(|e| panic!("real CPU failed for k={k}: {e}"));

        // Output is (Nat@6, Reg) — extract both parts.
        let (out_k, reg_val) = match &cpu_out {
            Value::Pair(nat, reg) => {
                // Decode k from output nat to verify iter preserves ⟨k,·⟩.
                let mut depth = 0usize;
                let mut cur = nat.as_ref().clone();
                loop {
                    match cur {
                        Value::Left(_) => break,
                        Value::Right(inner) => {
                            depth += 1;
                            cur = inner.as_ref().clone();
                        }
                        _ => panic!("malformed Nat in output"),
                    }
                }
                (depth, decode_reg(reg))
            }
            _ => panic!("expected Pair(nat, reg), got {cpu_out:?}"),
        };
        assert_eq!(out_k, k, "iter must preserve k: expected {k}, got {out_k}");
        assert_eq!(
            reg_val, expected_out,
            "orbit k={k} (real CPU): expected register={expected_out}, got {reg_val}"
        );
    }
    eprintln!("t3: real-CPU oracle agrees on 5-step trajectory from seed 1");
}

// ══════════════════════════════════════════════════════════════════════
// T4: orbit correctness — pins the polynomial
// ══════════════════════════════════════════════════════════════════════

/// Encode a 4-bit register value as a nested Bit product Value.
/// b0 = lsb = first tuple component.
/// Layout: (Bit, (Bit, (Bit, Bit))) — right-nested.
fn encode_reg(val: u8) -> Value {
    let bit = |b: u8| -> Value {
        if b == 0 {
            Value::left(Value::Unit) // O
        } else {
            Value::right(Value::Unit) // I
        }
    };
    let b0 = val & 1;
    let b1 = (val >> 1) & 1;
    let b2 = (val >> 2) & 1;
    let b3 = (val >> 3) & 1;
    Value::pair(bit(b0), Value::pair(bit(b1), Value::pair(bit(b2), bit(b3))))
}

/// Decode a 4-bit register from nested Bit product Value back to u8.
fn decode_reg(v: &Value) -> u8 {
    fn decode_bit(v: &Value) -> u8 {
        match v {
            Value::Left(_) => 0,  // O
            Value::Right(_) => 1, // I
            _ => panic!("expected a Bit (Left|Right), got {v:?}"),
        }
    }
    // Layout: Pair(b0, Pair(b1, Pair(b2, b3)))
    match v {
        Value::Pair(b0, rest1) => {
            let bit0 = decode_bit(b0);
            match rest1.as_ref() {
                Value::Pair(b1, rest2) => {
                    let bit1 = decode_bit(b1);
                    match rest2.as_ref() {
                        Value::Pair(b2, b3) => {
                            let bit2 = decode_bit(b2);
                            let bit3 = decode_bit(b3);
                            bit0 | (bit1 << 1) | (bit2 << 2) | (bit3 << 3)
                        }
                        _ => panic!("expected Pair(b2, b3), got {rest2:?}"),
                    }
                }
                _ => panic!("expected Pair(b1, ...), got {rest1:?}"),
            }
        }
        _ => panic!("expected Pair(b0, ...), got {v:?}"),
    }
}

/// Encode a Nat value (depth D=6, giving values 0..=5).
/// Nat = Zero | Succ Nat
/// k=0 → Left(Unit)
/// k=1 → Right(Left(Unit))
/// k=n → Right^n(Left(Unit))
fn encode_nat6(k: usize) -> Value {
    assert!(k < 6, "Nat@6 only supports k in 0..5, got {k}");
    let mut v = Value::left(Value::Unit); // Zero (base)
    for _ in 0..k {
        v = Value::right(v); // Succ
    }
    v
}

/// Reference Galois LFSR step: x ↦ (x >> 1) ⊕ (mask · b0).
/// Polynomial x^4+x+1, mask = 0b1001. b0 = lsb.
fn galois_step(x: u8) -> u8 {
    let b0 = x & 1;
    let shifted = x >> 1;
    if b0 == 1 {
        shifted ^ 0b1001
    } else {
        shifted
    }
}

#[test]
fn t4_orbit_correctness() {
    let iso = elaborate(LFSR_SRC, "lfsr");

    // Expected orbit from seed 0b0001 = 1, stepping 1..=5 times:
    // k=1: 1→9, k=2: 1→9→13, k=3: 1→..→15, k=4: 1→..→14, k=5: 1→..→7
    let expected_outputs: [u8; 5] = [9, 13, 15, 14, 7];

    // Verify reference Galois step produces this sequence.
    let mut x = 1u8;
    for (i, &expected) in expected_outputs.iter().enumerate() {
        x = galois_step(x);
        assert_eq!(
            x,
            expected,
            "reference Galois step {}: expected {expected}, got {x}",
            i + 1
        );
    }

    // Verify compiled program matches for each k=1..=5.
    let seed = 1u8;
    for (k_idx, &expected_out) in expected_outputs.iter().enumerate() {
        let k = k_idx + 1; // k=1..=5
        let input = Value::pair(encode_nat6(k), encode_reg(seed));
        let mut fuel = 100_000u64;
        let output = theseus_eval::eval(&iso, input.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("eval failed for k={k}: {e:?}"));

        // Output is (Nat@6, Reg) — extract both parts; iter must preserve ⟨k,·⟩.
        let (out_k, reg_val) = match &output {
            Value::Pair(nat, reg) => {
                let mut depth = 0usize;
                let mut cur = nat.as_ref().clone();
                loop {
                    match cur {
                        Value::Left(_) => break,
                        Value::Right(inner) => {
                            depth += 1;
                            cur = inner.as_ref().clone();
                        }
                        _ => panic!("malformed Nat in output"),
                    }
                }
                (depth, decode_reg(reg))
            }
            _ => panic!("expected Pair(nat, reg), got {output:?}"),
        };
        assert_eq!(
            out_k, k,
            "iter must preserve k in output: expected {k}, got {out_k}"
        );
        assert_eq!(
            reg_val, expected_out,
            "orbit k={k}: expected register={expected_out} (0b{expected_out:04b}), got {reg_val} (0b{reg_val:04b})"
        );
    }

    // Also verify via fast CPU.
    for (k_idx, &expected_out) in expected_outputs.iter().enumerate() {
        let k = k_idx + 1;
        let input = Value::pair(encode_nat6(k), encode_reg(seed));
        let cpu_out = run_on_cpu_fast(&iso, &input)
            .unwrap_or_else(|e| panic!("run_on_cpu_fast failed for k={k}: {e}"));
        let reg_val = match &cpu_out {
            Value::Pair(_nat, reg) => decode_reg(reg),
            _ => panic!("expected Pair(nat, reg), got {cpu_out:?}"),
        };
        assert_eq!(
            reg_val, expected_out,
            "orbit k={k} (fast CPU): expected register={expected_out}, got {reg_val}"
        );
    }
}

// ══════════════════════════════════════════════════════════════════════
// T5: park-tax / WCET via rom_trace
// ══════════════════════════════════════════════════════════════════════

#[test]
fn t5_park_tax_trace() {
    use theseus_tisc::energy::rom_trace;

    let iso = elaborate(LFSR_SRC, "lfsr");
    let lowered = compile_with_metadata(&iso).expect("compile lfsr");
    let rom = assemble(&lowered.instrs).expect("assemble lfsr");

    // WCET == 104.
    let b = lowered.wcet();
    assert_eq!(b, 104, "lfsr WCET must be 104, got {b}");

    let (in_ty, _) = type_of(&iso).unwrap();

    // Measure T(k) for k=0..=5.
    // Domain: 6 values of k × 16 register values. We pick a representative
    // seed (0b0001 = 1) for each k and verify T(k) is affine + monotone.
    let seed = 1u8;
    let mut t_values = Vec::new();

    for k in 0..6usize {
        let input = Value::pair(encode_nat6(k), encode_reg(seed));
        let init = marshal(&input, &in_ty);
        let trace = rom_trace(&rom, init, b);
        assert_eq!(trace.steps_b, b, "steps_b must equal B={b}");
        t_values.push(trace.steps_t);
    }

    // T(k) must be monotonically non-decreasing (more iterations = more productive steps).
    for i in 0..5 {
        assert!(
            t_values[i] <= t_values[i + 1],
            "T(k) must be monotone: T({i})={} > T({})={}",
            t_values[i],
            i + 1,
            t_values[i + 1]
        );
    }

    // Park-tax is 13.5% at k=4 and 0% at k=5 (k_max). T(5) == B.
    assert_eq!(
        t_values[5], b,
        "park_tax at k_max=5 must be 0: T(5)={}, B={b}",
        t_values[5]
    );

    // Pin the measured T(k) values: T(k) = 34 + 14k for k∈0..5.
    // Guards the milestone's headline park-tax datapoint against per-copy-cost regression.
    assert_eq!(
        t_values,
        vec![34, 48, 62, 76, 90, 104],
        "T(k) regression: expected [34,48,62,76,90,104], got {t_values:?}"
    );
    eprintln!("lfsr T(k) for k=0..5: {t_values:?}");
    eprintln!(
        "lfsr B={b}, park_taxes: {:?}",
        t_values
            .iter()
            .map(|&t| (b - t) as f64 / b as f64)
            .collect::<Vec<_>>()
    );

    // Verify affine: stride = T(k+1) - T(k) must be constant for all k.
    let strides: Vec<isize> = (0..5)
        .map(|i| t_values[i + 1] as isize - t_values[i] as isize)
        .collect();
    // All strides must be equal (affine).
    let stride = strides[0];
    for (i, &s) in strides.iter().enumerate() {
        assert_eq!(
            s, stride,
            "T(k) must be affine: stride[{i}]={s} != stride[0]={stride}"
        );
    }
    assert!(
        stride > 0,
        "stride must be positive (T is strictly increasing)"
    );
}

// ── T5b: energy pricing (joules — #[ignore], step-net lowering costly) ──

#[test]
#[ignore = "step-net lowering at ROM=104 is expensive (>9min release CPU, never yet run to completion); assertions (step_j>0, total_j==B·step_j) are sound but costly"]
fn t5b_energy_pricing() {
    use theseus_spice::chardb::rung0_measured;
    use theseus_tisc::energy::rom_program_energy;

    let iso = elaborate(LFSR_SRC, "lfsr");
    let db = rung0_measured();
    let t_ramp = 1e-7;

    let lowered = compile_with_metadata(&iso).expect("compile lfsr");
    let rom = assemble(&lowered.instrs).expect("assemble lfsr");
    let step_iso = theseus_tisc::step::step(&rom);
    let step_net = theseus_netlist::lower_iso(&step_iso).expect("lower step");

    let b = lowered.wcet();
    let seed = 1u8;

    // Run at k=5 (maximum — zero park tax).
    let input = Value::pair(encode_nat6(5), encode_reg(seed));
    let pe =
        rom_program_energy(&iso, &input, &db, t_ramp, &step_net).expect("rom_program_energy lfsr");

    assert_eq!(pe.steps_b, b, "steps_b must equal B={b}");
    assert!(pe.step_j > 0.0, "step_j must be > 0");

    // Static-inventory MODEL IDENTITY: total_j == B·step_j.
    assert!(
        (pe.total_j - b as f64 * pe.step_j).abs() < 1e-30,
        "total_j must be B·step_j (model identity)"
    );
}
