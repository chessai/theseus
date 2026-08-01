#![cfg(feature = "heavy-tests")]
// HEAVY tier (measured >4min): gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! Value-width slice-3: Flat(8) Add REDUCE (real numeric sum acc += arr[i]
//! over 8-bit words) via `fold` over a Flat8List.
//!
//! sumFold = fold(list_spine_ty(Flat(8), D), addInto) where
//! addInto : (a, b) ↦ (a, a+b mod 256) over Flat(8)×Flat(8).
//! Add is Flat(8)-only (at Flat(w<8), a+b overflows Flat(w) → uncanonical).
//!
//! T1: emission pin — D=3 and D=4 compile; pin full ROM at D=3, wcet=20.
//! T-bare: bare addInto compiles to [Add{rd:1,rs:0}]; whole-domain 65536.
//! T-foldstruct: fold over Flat(2) spine with xorInto; whole-domain 340.
//! T-sum-D2: sumFold at D=2 whole-domain 65792; independent mod-sum ref.
//! T-sum-D3: sumFold at D=3 strided + mandatory edge cases; independent ref.
//! T-real: real CPU (#[ignore]).
//! T-neg: Flat(4) addInto rejected by admit() as BackwardNotTotal.

use std::collections::BTreeMap;

use theseus_core::prim::{admit, enumerate_values, AdmitError, PrimDef};
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::{
    compile_with_metadata, marshal, run_on_cpu_resident, run_rom_fast, unmarshal,
    value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};

// ── Helpers ────────────────────────────────────────────────────────────

/// Thermometer value Right^k(Left(Unit)).
fn thermo(k: usize) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

/// Build the addInto PrimDef over Flat(8)×Flat(8):
/// forward: (a, b) ↦ (a, a.wrapping_add(b))
/// backward: (a, c) ↦ (a, c.wrapping_sub(a))
fn add_prim_def() -> PrimDef {
    let flat8 = Type::Flat(8);
    let ty = Type::prod(flat8.clone(), flat8.clone());
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let a = bytes[0];
        let b = bytes[1];
        let c = a.wrapping_add(b);
        let fwd_bytes = vec![a, c];
        let (fwd_val, _) = value_from_leaf_bytes_pub(&fwd_bytes, &ty);
        forward.insert(v.clone(), fwd_val);

        // backward: (a, c) ↦ (a, c.wrapping_sub(a))
        let b_back = b.wrapping_sub(a);
        let bwd_bytes = vec![a, b_back];
        let (bwd_val, _) = value_from_leaf_bytes_pub(&bwd_bytes, &ty);
        backward.insert(v.clone(), bwd_val);
    }

    PrimDef {
        forward_name: "addInto".into(),
        backward_name: "subFrom".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

/// Build the xorInto PrimDef over Flat(w)×Flat(w) (self-inverse).
fn xor_prim_def(w: u8) -> PrimDef {
    let flat = Type::Flat(w);
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
        backward.insert(v.clone(), out_val);
    }

    PrimDef {
        forward_name: "xorInto".into(),
        backward_name: "xorInto".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

/// Build a list-spine value from a slice of element bytes.
/// list_spine_ty(Flat(w), D) — elements are innermost-first:
///   nil = Left(Unit)
///   cons(h, t) = Right(Pair(thermo(h), t))
/// Panics if elems.len() >= D (spine can hold at most D-1 elements).
/// Includes round-trip assert via value_to/from_leaf_bytes.
fn list_spine_val(elems: &[u8], w: u8, d: usize) -> Value {
    assert!(
        elems.len() < d,
        "list_spine_val: elems.len()={} >= D={d} (spine holds at most D-1)",
        elems.len()
    );
    // Build innermost-first: start with nil, prepend from the back.
    let mut val = Value::left(Value::Unit); // nil
    for &e in elems.iter().rev() {
        val = Value::right(Value::pair(thermo(e as usize), val)); // cons(e, tail)
    }
    // Round-trip assert: marshal and unmarshal through leaf bytes.
    let ty = Type::list_spine_ty(Type::Flat(w), d);
    let bytes = value_to_leaf_bytes_pub(&val, &ty);
    let (rt_val, consumed) = value_from_leaf_bytes_pub(&bytes, &ty);
    assert_eq!(
        consumed,
        bytes.len(),
        "list_spine_val round-trip: consumed mismatch"
    );
    assert_eq!(rt_val, val, "list_spine_val round-trip: value mismatch");
    val
}

/// Build a full sumFold input: Pair(list_spine_val, thermo(acc)).
fn sum_fold_input(elems: &[u8], acc: u8, d: usize) -> Value {
    let list_val = list_spine_val(elems, 8, d);
    Value::pair(list_val, thermo(acc as usize))
}

/// Build sumFold iso at depth D.
fn sum_fold_iso(d: usize) -> Iso {
    let def = add_prim_def();
    Iso::fold(
        Type::list_spine_ty(Type::Flat(8), d),
        Iso::Prim(Box::new(def)),
    )
}

/// Full carrier type for sumFold: list_spine_ty(Flat(8), D) × Flat(8).
fn sum_fold_ty(d: usize) -> Type {
    Type::prod(Type::list_spine_ty(Type::Flat(8), d), Type::Flat(8))
}

/// Extract the accumulator byte from a sumFold output value.
fn extract_acc(v: &Value, d: usize) -> u8 {
    let ty = sum_fold_ty(d);
    let bytes = value_to_leaf_bytes_pub(v, &ty);
    // The acc is the LAST leaf byte.
    *bytes.last().expect("empty bytes")
}

/// Independent reference: acc_out = (acc0 + Σ elems) mod 256.
fn reference_sum(elems: &[u8], acc0: u8) -> u8 {
    let mut acc = acc0;
    for &e in elems {
        acc = acc.wrapping_add(e);
    }
    acc
}

/// Extract the list elements from a sumFold value by decoding the list spine.
fn extract_list_elems(v: &Value, d: usize) -> Vec<u8> {
    // The value is Pair(list, acc). Peel off the list part.
    let Value::Pair(list, _acc) = v else {
        panic!("expected Pair, got {v:?}");
    };
    let mut elems = Vec::new();
    let mut cur = list.as_ref();
    for _ in 0..d {
        match cur {
            Value::Left(_) => break, // nil
            Value::Right(inner) => {
                // cons(h, t) = Right(Pair(h, t))
                let Value::Pair(h, t) = inner.as_ref() else {
                    panic!("expected Pair in cons, got {inner:?}");
                };
                // h is thermo(byte) — decode by counting Rights
                let mut byte = 0u8;
                let mut hc = h.as_ref();
                loop {
                    match hc {
                        Value::Left(_) => break,
                        Value::Right(next) => {
                            byte += 1;
                            hc = next.as_ref();
                        }
                        _ => panic!("unexpected element shape: {hc:?}"),
                    }
                }
                elems.push(byte);
                cur = t.as_ref();
            }
            _ => panic!("unexpected list shape: {cur:?}"),
        }
    }
    elems
}

// ── T1: emission pin ──────────────────────────────────────────────────

#[test]
fn t1_emission_pin() {
    // D=3: pin the FULL ROM sequence (20 instrs, wcet=20).
    let iso_d3 = sum_fold_iso(3);
    let lowered_d3 = compile_with_metadata(&iso_d3).expect("compile sumFold D=3");

    let expected_d3_rom = vec![
        Instr::Mexch { rd: 6, addr: 0 },
        Instr::Addi { rd: 7, imm: 3 },
        Instr::Cswap {
            rd: 0,
            rs: 7,
            rt: 6,
        },
        Instr::Swapbr { rd: 7, rs: 0 },
        Instr::Mexch { rd: 7, addr: 0 },
        Instr::Add { rd: 7, rs: 1 },
        Instr::Mexch { rd: 7, addr: 0 },
        Instr::Swapbr { rd: 7, rs: 0 },
        Instr::Cswap {
            rd: 0,
            rs: 7,
            rt: 6,
        },
        Instr::Subi { rd: 7, imm: 3 },
        Instr::Addi { rd: 7, imm: 3 },
        Instr::Cswap {
            rd: 2,
            rs: 7,
            rt: 6,
        },
        Instr::Swapbr { rd: 7, rs: 0 },
        Instr::Mexch { rd: 7, addr: 0 },
        Instr::Add { rd: 7, rs: 3 },
        Instr::Mexch { rd: 7, addr: 0 },
        Instr::Swapbr { rd: 7, rs: 0 },
        Instr::Cswap {
            rd: 2,
            rs: 7,
            rt: 6,
        },
        Instr::Subi { rd: 7, imm: 3 },
        Instr::Mexch { rd: 6, addr: 0 },
    ];
    assert_eq!(
        lowered_d3.instrs, expected_d3_rom,
        "D=3 ROM mismatch: got {:?}",
        lowered_d3.instrs
    );
    assert_eq!(lowered_d3.instrs.len(), 20, "D=3 ROM length");
    assert_eq!(lowered_d3.wcet(), 20, "D=3 WCET must be 20");

    // Count Adds at D=3: exactly 2.
    let add_count_d3 = lowered_d3
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::Add { .. }))
        .count();
    assert_eq!(add_count_d3, 2, "D=3 must have 2 Adds");

    // D=4: compiles; must have 3 Adds (D-1 copies).
    let iso_d4 = sum_fold_iso(4);
    let lowered_d4 = compile_with_metadata(&iso_d4).expect("compile sumFold D=4");
    let add_count_d4 = lowered_d4
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::Add { .. }))
        .count();
    assert_eq!(
        add_count_d4, 3,
        "D=4 must have 3 Adds, got {add_count_d4}; ROM: {:?}",
        lowered_d4.instrs
    );
    // D=4 = 31 instrs / wcet 31 (design-review-attested; MINOR-1 full-per-D pin).
    assert_eq!(
        lowered_d4.instrs.len(),
        31,
        "D=4 ROM must be 31 instrs; ROM: {:?}",
        lowered_d4.instrs
    );
    assert_eq!(lowered_d4.wcet(), 31, "D=4 WCET must be 31");
}

// ── T-bare: bare addInto, whole-domain 65536, HOISTED ─────────────────

#[test]
fn t_bare_add_whole_domain() {
    let def = add_prim_def();
    let iso = Iso::Prim(Box::new(def));

    // Pin: compiles to exactly [Add{rd:1, rs:0}].
    let lowered = compile_with_metadata(&iso).expect("compile addInto");
    assert_eq!(
        lowered.instrs,
        vec![Instr::Add { rd: 1, rs: 0 }],
        "bare addInto must compile to [Add{{rd:1,rs:0}}], got {:?}",
        lowered.instrs
    );

    // Hoist-compile-once: compile once, run per input.
    let ty = Type::prod(Type::Flat(8), Type::Flat(8));
    let rom = assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();

    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 65_536, "Flat(8)×Flat(8) domain size");

    // Also build eval iso with a FRESH def (don't share the table).
    let eval_def = add_prim_def();
    let eval_iso = Iso::Prim(Box::new(eval_def));

    for v in &domain {
        let state = marshal(v, &ty);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal(&final_state, &ty);

        let mut fuel = 100_000u64;
        let eval_out = theseus_eval::eval(&eval_iso, v.clone(), &mut fuel).expect("eval addInto");

        assert_eq!(rom_out, eval_out, "bare addInto: rom vs eval mismatch");
    }
}

// ── T-foldstruct: fold over Flat(2) spine with xorInto, 340 inputs ────

#[test]
fn t_foldstruct_flat2_xor_fold() {
    let w = 2u8;
    let d = 4usize;
    let def = xor_prim_def(w);
    let iso = Iso::fold(
        Type::list_spine_ty(Type::Flat(w), d),
        Iso::Prim(Box::new(def)),
    );

    let ty = Type::prod(Type::list_spine_ty(Type::Flat(w), d), Type::Flat(w));
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 340, "Flat(2) fold D=4 domain size");

    // Hoist compile.
    let lowered = compile_with_metadata(&iso).expect("compile fold(Flat(2), xorInto)");
    let rom = assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();

    // Fresh eval iso.
    let eval_def = xor_prim_def(w);
    let eval_iso = Iso::fold(
        Type::list_spine_ty(Type::Flat(w), d),
        Iso::Prim(Box::new(eval_def)),
    );

    for v in &domain {
        let state = marshal(v, &ty);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal(&final_state, &ty);

        let mut fuel = 100_000_000u64;
        let eval_out =
            theseus_eval::eval(&eval_iso, v.clone(), &mut fuel).expect("eval fold xorInto");

        assert_eq!(
            rom_out, eval_out,
            "foldstruct Flat(2) xor: rom vs eval mismatch"
        );
    }
}

// ── T-sum-D2: whole-domain 65792, HOISTED ─────────────────────────────

#[test]
fn t_sum_d2_whole_domain() {
    let d = 2usize;
    let iso = sum_fold_iso(d);
    let ty = sum_fold_ty(d);

    // Hoist compile.
    let lowered = compile_with_metadata(&iso).expect("compile sumFold D=2");
    let rom = assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();

    let domain = enumerate_values(&ty);
    // D=2: 257 list values (1 nil + 256 cons) × 256 acc = 65,792
    assert_eq!(domain.len(), 65_792, "sumFold D=2 domain size");

    // Fresh eval iso.
    let eval_def = add_prim_def();
    let eval_iso = Iso::fold(
        Type::list_spine_ty(Type::Flat(8), d),
        Iso::Prim(Box::new(eval_def)),
    );

    for v in &domain {
        let state = marshal(v, &ty);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal(&final_state, &ty);

        let mut fuel = 100_000_000u64;
        let eval_out =
            theseus_eval::eval(&eval_iso, v.clone(), &mut fuel).expect("eval sumFold D=2");

        assert_eq!(rom_out, eval_out, "sumFold D=2: rom vs eval mismatch");

        // Independent reference: acc_out == (acc0 + Σ elems) mod 256.
        let elems = extract_list_elems(v, d);
        let in_bytes = value_to_leaf_bytes_pub(v, &ty);
        let acc0 = *in_bytes.last().unwrap();
        let expected_acc = reference_sum(&elems, acc0);
        let actual_acc = extract_acc(&rom_out, d);
        assert_eq!(
            actual_acc, expected_acc,
            "sumFold D=2: independent ref mismatch: elems={elems:?}, acc0={acc0}, expected={expected_acc}, got={actual_acc}"
        );
    }
}

// ── T-sum-D3: strided + edge cases, HOISTED ───────────────────────────

#[test]
fn t_sum_d3_strided() {
    let d = 3usize;
    let iso = sum_fold_iso(d);
    let ty = sum_fold_ty(d);

    // Hoist compile.
    let lowered = compile_with_metadata(&iso).expect("compile sumFold D=3");
    let rom = assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();

    // Fresh eval iso.
    let eval_def = add_prim_def();
    let eval_iso = Iso::fold(
        Type::list_spine_ty(Type::Flat(8), d),
        Iso::Prim(Box::new(eval_def)),
    );

    // Mandatory edge cases: (elems, acc0).
    let mandatory: Vec<(Vec<u8>, u8)> = vec![
        (vec![], 0),           // empty list
        (vec![], 42),          // empty list nonzero acc
        (vec![7], 3),          // len-1
        (vec![200, 100], 0),   // 200+100=300→44 (carry)
        (vec![255, 1], 0),     // 255+1=256→0
        (vec![128, 128], 0),   // 128+128=256→0
        (vec![255, 255], 255), // 255+[255,255] → 255+255=254, 254+255=253
        (vec![0, 0], 0),       // all zeros
        (vec![1, 1], 1),       // small values
    ];

    let mut checked = 0usize;

    // Run mandatory edge cases.
    for (elems, acc0) in &mandatory {
        let input = sum_fold_input(elems, *acc0, d);
        let state = marshal(&input, &ty);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal(&final_state, &ty);

        let mut fuel = 100_000_000u64;
        let eval_out =
            theseus_eval::eval(&eval_iso, input.clone(), &mut fuel).expect("eval sumFold D=3 edge");

        assert_eq!(
            rom_out, eval_out,
            "sumFold D=3 edge: rom vs eval mismatch on elems={elems:?}, acc0={acc0}"
        );

        let expected_acc = reference_sum(elems, *acc0);
        let actual_acc = extract_acc(&rom_out, d);
        assert_eq!(
            actual_acc, expected_acc,
            "sumFold D=3 edge: ref mismatch: elems={elems:?}, acc0={acc0}, expected={expected_acc}, got={actual_acc}"
        );
        checked += 1;
    }

    // Strided sample over len-2 lists: stride through (e1, e2, acc0) space.
    // Full space: 256³ = 16M; stride of 17 gives ~3300 samples.
    let stride = 17u16;
    eprintln!("T-sum-D3: stride={stride}");
    for e1 in (0u16..256).step_by(stride as usize) {
        for e2 in (0u16..256).step_by(stride as usize) {
            for acc0 in (0u16..256).step_by(stride as usize) {
                let elems = vec![e1 as u8, e2 as u8];
                let input = sum_fold_input(&elems, acc0 as u8, d);
                let state = marshal(&input, &ty);
                let final_state = run_rom_fast(&rom, state, budget);
                let rom_out = unmarshal(&final_state, &ty);

                let mut fuel = 100_000_000u64;
                let eval_out = theseus_eval::eval(&eval_iso, input.clone(), &mut fuel)
                    .expect("eval sumFold D=3 strided");

                assert_eq!(
                    rom_out, eval_out,
                    "sumFold D=3 strided: rom vs eval mismatch on elems={elems:?}, acc0={acc0}"
                );

                let expected_acc = reference_sum(&elems, acc0 as u8);
                let actual_acc = extract_acc(&rom_out, d);
                assert_eq!(
                    actual_acc, expected_acc,
                    "sumFold D=3 strided: ref mismatch: elems={elems:?}, acc0={acc0}"
                );
                checked += 1;
            }
        }
    }

    // Strided sample over len-1 and len-0 lists too.
    for e1 in (0u16..256).step_by(stride as usize) {
        for acc0 in (0u16..256).step_by(stride as usize) {
            let elems = vec![e1 as u8];
            let input = sum_fold_input(&elems, acc0 as u8, d);
            let state = marshal(&input, &ty);
            let final_state = run_rom_fast(&rom, state, budget);
            let rom_out = unmarshal(&final_state, &ty);

            let mut fuel = 100_000_000u64;
            let eval_out = theseus_eval::eval(&eval_iso, input.clone(), &mut fuel)
                .expect("eval sumFold D=3 strided len-1");

            assert_eq!(rom_out, eval_out, "sumFold D=3 strided len-1 mismatch");

            let expected_acc = reference_sum(&elems, acc0 as u8);
            let actual_acc = extract_acc(&rom_out, d);
            assert_eq!(
                actual_acc, expected_acc,
                "sumFold D=3 strided len-1 ref mismatch"
            );
            checked += 1;
        }
    }

    // len-0 (empty) list: sweep ALL 256 acc0 — the inactive-skip path must
    // preserve acc for every value (acc-dependent-corruption corner, Finding 5).
    for acc0 in 0u16..256 {
        let input = sum_fold_input(&[], acc0 as u8, d);
        let state = marshal(&input, &ty);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal(&final_state, &ty);
        let expected_acc = reference_sum(&[], acc0 as u8);
        let actual_acc = extract_acc(&rom_out, d);
        assert_eq!(
            actual_acc, expected_acc,
            "sumFold D=3 empty-list ref mismatch acc0={acc0}"
        );
        checked += 1;
    }

    eprintln!("T-sum-D3: checked {checked} inputs (stride={stride})");
}

// ── T-sum-D4: 3-element chain (D=4), edge cases + strided, HOISTED ─────
// Finding 8 (MAJOR): D=4 was only compiled/Add-counted, never EXECUTED — a
// bug confined to the third fold copy would pass every other test. Run it.
#[test]
fn t_sum_d4_strided() {
    let d = 4usize; // lists of length <= 3
    let iso = sum_fold_iso(d);
    let ty = sum_fold_ty(d);

    let lowered = compile_with_metadata(&iso).expect("compile sumFold D=4");
    let rom = assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();

    let eval_def = add_prim_def();
    let eval_iso = Iso::fold(
        Type::list_spine_ty(Type::Flat(8), d),
        Iso::Prim(Box::new(eval_def)),
    );

    let check = |elems: &[u8], acc0: u8, checked: &mut usize| {
        let input = sum_fold_input(elems, acc0, d);
        let state = marshal(&input, &ty);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal(&final_state, &ty);
        let mut fuel = 100_000_000u64;
        let eval_out = theseus_eval::eval(&eval_iso, input.clone(), &mut fuel).expect("eval D=4");
        assert_eq!(
            rom_out, eval_out,
            "sumFold D=4: rom vs eval on elems={elems:?}, acc0={acc0}"
        );
        let expected_acc = reference_sum(elems, acc0);
        let actual_acc = extract_acc(&rom_out, d);
        assert_eq!(
            actual_acc, expected_acc,
            "sumFold D=4: ref mismatch elems={elems:?}, acc0={acc0}, expected={expected_acc}, got={actual_acc}"
        );
        *checked += 1;
    };

    let mut checked = 0usize;
    // Mandatory 3-element edge cases (the third-copy chain).
    check(&[200, 100, 56], 0, &mut checked); // 200+100+56 = 356 → 100
    check(&[255, 255, 255], 255, &mut checked); // 255*4 = 1020 → 252
    check(&[0, 0, 0], 0, &mut checked);
    check(&[], 99, &mut checked); // empty at D=4
    check(&[7], 3, &mut checked); // len-1
    check(&[7, 11], 3, &mut checked); // len-2
                                      // Coarse stride over (e1,e2,e3,acc0) for length-3 lists: stride 85 → {0,85,170,255}.
    let stride = 85u16;
    eprintln!("T-sum-D4: stride={stride}");
    for e1 in (0u16..256).step_by(stride as usize) {
        for e2 in (0u16..256).step_by(stride as usize) {
            for e3 in (0u16..256).step_by(stride as usize) {
                for acc0 in (0u16..256).step_by(stride as usize) {
                    check(&[e1 as u8, e2 as u8, e3 as u8], acc0 as u8, &mut checked);
                }
            }
        }
    }
    eprintln!("T-sum-D4: checked {checked} inputs (stride={stride})");
}

// ── T-real: real CPU (#[ignore]) ──────────────────────────────────────

#[test]
#[ignore = "real-CPU system_step ~147s/input in RELEASE; run via `cargo test -p theseus-tisc --test valwidth_add -- --ignored --release`"]
fn t_real_cpu_d3() {
    let d = 3usize;
    let iso = sum_fold_iso(d);

    let samples: Vec<(Vec<u8>, u8)> = vec![
        (vec![], 5),         // empty list
        (vec![7], 3),        // len-1: 3+7=10
        (vec![200, 100], 0), // carry: 200+100=44 (mod 256)
        (vec![5, 7], 3),     // design-review example: 3+5+7=15
    ];

    // Fresh eval iso for reference.
    let eval_def = add_prim_def();
    let eval_iso = Iso::fold(
        Type::list_spine_ty(Type::Flat(8), d),
        Iso::Prim(Box::new(eval_def)),
    );

    for (elems, acc0) in &samples {
        let input = sum_fold_input(elems, *acc0, d);
        let mut fuel = 100_000_000u64;
        let expected =
            theseus_eval::eval(&eval_iso, input.clone(), &mut fuel).expect("eval sumFold D=3");

        let got = run_on_cpu_resident(&iso, &input, 0, |iso, v, fuel| {
            theseus_eval::eval(iso, v, fuel)
        })
        .expect("run_on_cpu_resident sumFold D=3");

        assert_eq!(
            got, expected,
            "T-real D=3: real CPU mismatch on elems={elems:?}, acc0={acc0}"
        );
    }
}

// ── T-neg: Flat(4) addInto rejected by admit ──────────────────────────

#[test]
fn t_neg_flat4_add_rejected() {
    let flat4 = Type::Flat(4);
    let ty = Type::prod(flat4.clone(), flat4.clone());
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    // Forward: (a, b) ↦ (a, thermo(a+b)) — HONEST, a+b can exceed 15.
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let a = bytes[0];
        let b = bytes[1];
        let sum = (a as u16) + (b as u16); // 0..30
        let fwd_val = Value::pair(thermo(a as usize), thermo(sum as usize));
        forward.insert(v.clone(), fwd_val);
    }

    // Backward: (a, c) ↦ (a, thermo(c - a)) — only for c >= a within Flat(4).
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let a = bytes[0];
        let c = bytes[1];
        if c >= a {
            let diff = c - a;
            let bwd_val = Value::pair(thermo(a as usize), thermo(diff as usize));
            backward.insert(v.clone(), bwd_val);
        }
        // c < a: no valid preimage in Flat(4), entry omitted.
    }

    let def = PrimDef {
        forward_name: "addInto_flat4".into(),
        backward_name: "subFrom_flat4".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    };

    let result = admit(&def);
    assert!(
        matches!(result, Err(AdmitError::BackwardNotTotal(_))),
        "Flat(4) addInto must be rejected as BackwardNotTotal, got {result:?}"
    );
}
