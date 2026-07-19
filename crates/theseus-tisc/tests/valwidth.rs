//! Value-width slice-1: Type::Flat(n) foundation + bare byte-Xor.
//!
//! T1: structural properties (num_leaves, cardinality, enumerate, round-trip).
//! T2: Flat(4) Xor → exactly [Xor{rd,rs}]; run_on_cpu_fast == eval whole domain.
//! T3: Flat(8) Xor → same [Xor] shape; run_on_cpu_fast == eval strided.
//! T4: real CPU on a handful of Flat(8) Xor inputs.
//! T6: canonicalization — Flat(4) Xor register bytes stay < 16.
//! T-neg: Flat(9) rejected; Prod(Flat(4),Flat(4)) is 2 leaves.

use std::collections::BTreeMap;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    compile, compile_with_metadata, run_on_cpu, run_on_cpu_fast,
    value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};

// ── Helpers ─────────────────────────────────────────────────────────

/// Build the thermometer value Right^k(Left(Unit)).
fn thermo(k: usize) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

/// Build a bare byte-Xor PrimDef over Flat(n) × Flat(n):
/// (a, b) ↦ (a, a ^ b). Self-inverse.
fn xor_prim_def(n: u8) -> PrimDef {
    let flat = Type::Flat(n);
    let ty = Type::prod(flat.clone(), flat.clone());
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let a = bytes[0];
        let b = bytes[1];
        let out_b = a ^ b;
        let out_bytes = vec![a, out_b];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        forward.insert(v.clone(), out_val.clone());
        // Self-inverse: backward is the same function
        backward.insert(v.clone(), out_val);
    }

    PrimDef {
        forward_name: "xorInto".into(),
        backward_name: "xorInto".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
    }
}

/// Build the Xor prim Iso, MOVING the PrimDef into the Iso (no clone).
fn xor_prim_iso(n: u8) -> Iso {
    Iso::Prim(Box::new(xor_prim_def(n)))
}

// ── T1: structural properties ───────────────────────────────────────

#[test]
fn t1_num_leaves_flat() {
    for n in 1u8..=8 {
        let ty = Type::Flat(n);
        let instrs = compile(&Iso::Id(ty.clone()));
        // Id always compiles; num_leaves is exercised internally.
        assert!(instrs.is_ok(), "Flat({n}) should compile Id");
    }
}

#[test]
fn t1_cardinality() {
    for n in 1u8..=8 {
        let ty = Type::Flat(n);
        let card = theseus_eval::cardinality(&ty);
        assert_eq!(card, 1usize << n, "cardinality of Flat({n})");
    }
}

#[test]
fn t1_enumerate_values_are_thermometers() {
    for n in 1u8..=8 {
        let ty = Type::Flat(n);
        let vals = enumerate_values(&ty);
        let card = 1usize << n;
        assert_eq!(vals.len(), card, "Flat({n}) should have {card} values");
        for (k, v) in vals.iter().enumerate() {
            assert_eq!(v, &thermo(k), "Flat({n}) value {k} mismatch");
        }
    }
}

#[test]
fn t1_enumerate_flat_equals_numeral_ty() {
    // PIN: enumerate(Flat(n)) == enumerate(numeral_ty(2^n)) value-for-value.
    for n in 1u8..=8 {
        let flat_vals = enumerate_values(&Type::Flat(n));
        let numeral_vals = enumerate_values(&Type::numeral_ty(1usize << n));
        assert_eq!(
            flat_vals, numeral_vals,
            "Flat({n}) values must equal numeral_ty(2^{n}) values"
        );
    }
}

#[test]
fn t1_leaf_bytes_round_trip() {
    for n in [1u8, 4, 8] {
        let ty = Type::Flat(n);
        let card = 1usize << n;
        for k in 0..card {
            let v = thermo(k);
            let bytes = value_to_leaf_bytes_pub(&v, &ty);
            assert_eq!(bytes, vec![k as u8], "Flat({n}) thermo({k}) to bytes");
            let (v2, consumed) = value_from_leaf_bytes_pub(&bytes, &ty);
            assert_eq!(consumed, 1, "Flat({n}) consumes 1 byte");
            assert_eq!(v2, v, "Flat({n}) round-trip thermo({k})");
        }
    }
}

// ── T2: Flat(4) Xor → [Xor{rd,rs}], whole domain ───────────────────

#[test]
fn t2_flat4_xor_compiles_to_single_xor() {
    let iso = xor_prim_iso(4);
    let lowered = compile_with_metadata(&iso).expect("compile Flat(4) Xor");
    assert_eq!(
        lowered.instrs.len(), 1,
        "Flat(4) Xor must compile to exactly 1 instruction, got {:?}",
        lowered.instrs
    );
    match lowered.instrs[0] {
        Instr::Xor { rd, rs } => {
            assert_eq!(rd, 1, "rd must be 1 (second leaf)");
            assert_eq!(rs, 0, "rs must be 0 (first leaf)");
        }
        ref other => panic!("expected Xor, got {other:?}"),
    }
}

#[test]
fn t2_flat4_xor_fast_equals_eval() {
    let iso = xor_prim_iso(4);
    let ty = Type::prod(Type::Flat(4), Type::Flat(4));
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 256, "Flat(4)×Flat(4) has 256 inputs");

    for v in &domain {
        let fast_out = run_on_cpu_fast(&iso, v)
            .unwrap_or_else(|e| panic!("fast failed on {v:?}: {e}"));
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("eval failed on {v:?}: {e:?}"));
        assert_eq!(
            fast_out, eval_out,
            "Flat(4) Xor: fast disagrees with eval on {v:?}"
        );
    }
}

// ── T3: Flat(8) Xor → same [Xor] shape, strided ────────────────────

#[test]
fn t3_flat8_xor_compiles_to_single_xor() {
    let iso = xor_prim_iso(8);
    let lowered = compile_with_metadata(&iso).expect("compile Flat(8) Xor");
    assert_eq!(
        lowered.instrs.len(), 1,
        "Flat(8) Xor must compile to exactly 1 instruction, got {:?}",
        lowered.instrs
    );
    match lowered.instrs[0] {
        Instr::Xor { rd, rs } => {
            assert_eq!(rd, 1, "rd must be 1 (parametric pin: same as Flat(4))");
            assert_eq!(rs, 0, "rs must be 0 (parametric pin: same as Flat(4))");
        }
        ref other => panic!("expected Xor, got {other:?}"),
    }
}

#[test]
fn t3_flat8_xor_fast_equals_eval_strided() {
    let iso = xor_prim_iso(8);
    // Strided sample: corners + key byte patterns + evenly spaced.
    let key_bytes: Vec<u8> = vec![0, 1, 0x55, 0xAA, 0xC3, 127, 128, 254, 255];
    let mut pairs: Vec<(u8, u8)> = Vec::new();
    for &a in &key_bytes {
        for &b in &key_bytes {
            pairs.push((a, b));
        }
    }
    // Deduplicate (already unique with key_bytes but be safe)
    pairs.sort();
    pairs.dedup();

    let ty = Type::prod(Type::Flat(8), Type::Flat(8));
    let mut checked = 0usize;
    for (a, b) in &pairs {
        let input_bytes = vec![*a, *b];
        let (v, _) = value_from_leaf_bytes_pub(&input_bytes, &ty);
        let fast_out = run_on_cpu_fast(&iso, &v)
            .unwrap_or_else(|e| panic!("fast failed on ({a},{b}): {e}"));
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("eval failed on ({a},{b}): {e:?}"));
        assert_eq!(
            fast_out, eval_out,
            "Flat(8) Xor: fast disagrees with eval on ({a},{b})"
        );
        checked += 1;
    }
    assert!(checked >= 36, "must check at least 36 pairs, got {checked}");
    eprintln!("T3: Flat(8) Xor strided oracle agrees on {checked} pairs");
}

// ── T4: real CPU on Flat(8) Xor ─────────────────────────────────────

#[test]
#[ignore = "real-CPU system_step; run via --ignored (~6.5s/input)"]
fn t4_flat8_xor_real_cpu() {
    let iso = xor_prim_iso(8);
    let ty = Type::prod(Type::Flat(8), Type::Flat(8));

    // A handful of dense-byte inputs.
    let test_pairs: Vec<(u8, u8)> = vec![
        (0x01, 0x02),
        (0xAA, 0x55),
        (0xFF, 0x01),
        (0xC3, 0x3C),
    ];

    for (a, b) in &test_pairs {
        let input_bytes = vec![*a, *b];
        let (v, _) = value_from_leaf_bytes_pub(&input_bytes, &ty);
        let cpu_out = run_on_cpu(&iso, &v, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("real-CPU failed on ({a:#04x},{b:#04x}): {e}"));
        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("eval failed on ({a:#04x},{b:#04x}): {e:?}"));
        assert_eq!(
            cpu_out, eval_out,
            "Flat(8) Xor: real CPU disagrees with eval on ({a:#04x},{b:#04x})"
        );
    }
}

// ── T6: canonicalization — Flat(4) register bytes stay < 16 ─────────

#[test]
fn t6_flat4_xor_register_bytes_canonical() {
    let iso = xor_prim_iso(4);
    let ty = Type::prod(Type::Flat(4), Type::Flat(4));
    let out_ty = ty.clone();
    let domain = enumerate_values(&ty);

    for v in &domain {
        let fast_out = run_on_cpu_fast(&iso, v)
            .unwrap_or_else(|e| panic!("fast failed on {v:?}: {e}"));
        let out_bytes = value_to_leaf_bytes_pub(&fast_out, &out_ty);
        for (i, &byte) in out_bytes.iter().enumerate() {
            assert!(
                byte < 16,
                "Flat(4) Xor output byte[{i}] = {byte} >= 16 (upper bits nonzero)"
            );
        }
    }
}

// ── T-neg: Flat(9) rejected; Prod(Flat(4),Flat(4)) is 2 leaves ─────

#[test]
fn tneg_flat9_num_leaves_rejected() {
    // Construct Flat(9) DIRECTLY (bypass the ctor assert).
    let ty = Type::Flat(9);
    // compile(Id(Flat(9))) should fail because num_leaves rejects width > 8.
    let result = compile(&Iso::Id(ty));
    assert!(result.is_err(), "Flat(9) must be rejected by num_leaves");
}

#[test]
fn tneg_prod_flat4_flat4_is_2_leaves() {
    let ty = Type::prod(Type::Flat(4), Type::Flat(4));
    // Compiling Id on this type should work, and internally num_leaves == 2.
    let result = compile(&Iso::Id(ty.clone()));
    assert!(result.is_ok(), "Prod(Flat(4), Flat(4)) should compile");

    // Marshal/unmarshal a value to verify 2 bytes.
    let v = Value::pair(thermo(5), thermo(10));
    let bytes = value_to_leaf_bytes_pub(&v, &ty);
    assert_eq!(bytes.len(), 2, "Prod(Flat(4), Flat(4)) marshals to 2 bytes");
    assert_eq!(bytes, vec![5, 10]);
}
