//! Reversible in-memory XOR/parity reduce tests (M-D.b2.f3).
//!
//! Proves: `xorReduce = iter Nat_d BODY` compiles via
//! `compile_with_metadata_resident` and matches `eval` over the full domain.
//! The body uses borrow-swap: `memexch` loads arr[idx] into data, `reduceStep`
//! XORs it into acc, `memexch` restores, `succ` advances idx.
//!
//! Primary: d=3 (24 instrs, 96 inputs, no displacement).
//! Secondary: d=4 (56 instrs, 128 inputs, idx displaced to Mem(2)).
//!
//! Each test maps to a design finding (§7 of mdb2-f3-design.md).
//! OOM discipline: real-CPU / closure / energy-pricing tests are `#[ignore]`.

use std::collections::BTreeMap;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    compile_with_metadata_resident, marshal_resident, memexch_prim,
    run_on_cpu_fast_resident, run_on_cpu_resident, unmarshal_resident,
    value_to_leaf_bytes_pub, value_from_leaf_bytes_pub,
};

// ── Type helpers ────────────────────────────────────────────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// `(idx:Bit) × (arr0:Bit) × (arr1:Bit) × 1`
fn idx_arr_ty() -> Type {
    let b = bit();
    Type::prod(b.clone(), Type::prod(b.clone(), Type::prod(b.clone(), Type::One)))
}

/// `(arr0:Bit) × (arr1:Bit) × 1`
fn arr_ty() -> Type {
    let b = bit();
    Type::prod(b.clone(), Type::prod(b.clone(), Type::One))
}

/// Body type: `(acc:Bit) × (data:Bit) × (idx:Bit) × (arr0:Bit) × (arr1:Bit) × 1`
fn body_ty() -> Type {
    let b = bit();
    Type::prod(
        b.clone(),
        Type::prod(
            b.clone(),
            Type::prod(b.clone(), Type::prod(b.clone(), Type::prod(b.clone(), Type::One))),
        ),
    )
}

/// Full carrier type: `Nat_d × body_ty`  (head = d+3 leaves with resident=2).
fn carrier_ty(d: usize) -> Type {
    Type::prod(Type::numeral_ty(d), body_ty())
}

// ── Program construction ────────────────────────────────────────────

/// Build memexch Iso for `(data:Bit, idx:Bit, arr[Bit;2])`.
fn memexch_iso() -> Iso {
    let def = memexch_prim(&bit(), 2).expect("memexch_prim(Bit,2)");
    Iso::Prim(Box::new(def))
}

/// Build the XOR prim: `(a, b) ↦ (a, a^b)` on `Bit × Bit`.
/// Recognized by the compiler as `Instr::Xor`.
fn xor_prim() -> Iso {
    let b = bit();
    let ty = Type::prod(b.clone(), b.clone());
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let a = bytes[0];
        let b_val = bytes[1];
        // forward: (a, b) ↦ (a, a^b)
        let out_bytes = vec![a, a ^ b_val];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        forward.insert(v.clone(), out_val);
    }

    // Compute backward from forward (it's an involution: applying twice = identity)
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let a = bytes[0];
        let b_val = bytes[1];
        // backward of (a, a^b) is (a, b) — i.e. same operation: (a, c) ↦ (a, a^c)
        let out_bytes = vec![a, a ^ b_val];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        backward.insert(v.clone(), out_val);
    }

    let def = PrimDef {
        forward_name: "xor".into(),
        backward_name: "xor".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
    };
    Iso::Prim(Box::new(def))
}

/// `reduceStep : (acc, data) ↦ (acc⊕data, data) = SwapProd ; xor ; SwapProd`
fn reduce_step() -> Iso {
    let b = bit();
    let swap = Iso::SwapProd(b.clone(), b.clone());
    Iso::seq(Iso::seq(swap.clone(), xor_prim()), swap)
}

/// `succ_idx = notb = SwapSum(One, One)` on Bit (0↔1, → Xori{idx,1}).
fn succ_idx() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// Build the XOR-reduce body:
/// ```text
/// BODY : (acc, data, idx, arr) ↔ (acc, data, idx, arr) =
///     (id_acc ⊗ memexch)
///   ; assocL ; (reduceStep ⊗ id_{idx,arr}) ; assocR
///   ; (id_acc ⊗ memexch)
///   ; (id_{acc,data} ⊗ succ_idx ⊗ id_arr)
/// ```
fn xor_reduce_body() -> Iso {
    let b = bit();
    let mexch = memexch_iso();

    // (id_acc ⊗ memexch): on (acc, (data, idx, arr))
    let id_acc_mexch = Iso::times(Iso::Id(b.clone()), mexch.clone());

    // assocL: (acc, (data, rest)) → ((acc, data), rest)
    // where rest = idx_arr_ty
    let assoc_l = Iso::AssocLProd(b.clone(), b.clone(), idx_arr_ty());

    // (reduceStep ⊗ id_{idx,arr}): on ((acc, data), (idx, arr))
    let rs_times_id = Iso::times(reduce_step(), Iso::Id(idx_arr_ty()));

    // assocR: ((acc, data), rest) → (acc, (data, rest))
    let assoc_r = Iso::AssocRProd(b.clone(), b.clone(), idx_arr_ty());

    // (id_{acc,data} ⊗ succ_idx ⊗ id_arr): on (acc, (data, (idx, arr)))
    let succ_part = Iso::times(
        Iso::Id(b.clone()),
        Iso::times(Iso::Id(b.clone()), Iso::times(succ_idx(), Iso::Id(arr_ty()))),
    );

    // Compose all:
    // id_acc_mexch ; assocL ; rs_times_id ; assocR ; id_acc_mexch ; succ_part
    Iso::seq(
        Iso::seq(
            Iso::seq(
                Iso::seq(Iso::seq(id_acc_mexch.clone(), assoc_l), rs_times_id),
                assoc_r,
            ),
            id_acc_mexch,
        ),
        succ_part,
    )
}

/// Build the full xorReduce program: `iter Nat_d BODY`.
fn xor_reduce_iso(d: usize) -> Iso {
    Iso::iter(Type::numeral_ty(d), xor_reduce_body())
}

/// Reference eval for the XOR reduce.
fn eval_xor_reduce(d: usize, input: &Value) -> Value {
    let iso = xor_reduce_iso(d);
    let mut fuel = 10_000_000u64;
    theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval xor_reduce")
}

// ── Value constructors ─────────────────────────────────────────────

/// Build the numeral value k in Nat_d.
fn numeral_val(k: usize) -> Value {
    let mut val = Value::left(Value::Unit);
    for _ in 0..k {
        val = Value::right(val);
    }
    val
}

fn bit_val(b: u8) -> Value {
    if b == 0 {
        Value::left(Value::Unit)
    } else {
        Value::right(Value::Unit)
    }
}

/// Build a carrier value: `(numeral k, (acc, (data, (idx, (arr0, (arr1, ()))))))`
fn make_input(k: usize, acc: u8, data: u8, idx: u8, arr0: u8, arr1: u8) -> Value {
    Value::pair(
        numeral_val(k),
        Value::pair(
            bit_val(acc),
            Value::pair(
                bit_val(data),
                Value::pair(
                    bit_val(idx),
                    Value::pair(bit_val(arr0), Value::pair(bit_val(arr1), Value::Unit)),
                ),
            ),
        ),
    )
}

/// Extract `(k, acc, data, idx, arr0, arr1)` from a carrier value.
fn decompose_output(v: &Value) -> (usize, u8, u8, u8, u8, u8) {
    let Value::Pair(counter, body) = v else {
        panic!("expected Pair, got {v:?}")
    };
    let mut k = 0usize;
    let mut cur = counter.as_ref();
    loop {
        match cur {
            Value::Left(_) => break,
            Value::Right(inner) => {
                k += 1;
                cur = inner.as_ref();
            }
            _ => panic!("unexpected counter shape: {cur:?}"),
        }
    }
    let Value::Pair(acc_v, rest1) = body.as_ref() else {
        panic!("expected body Pair")
    };
    let Value::Pair(data_v, rest2) = rest1.as_ref() else {
        panic!("expected data Pair")
    };
    let Value::Pair(idx_v, rest3) = rest2.as_ref() else {
        panic!("expected idx Pair")
    };
    let Value::Pair(arr0_v, rest4) = rest3.as_ref() else {
        panic!("expected arr0 Pair")
    };
    let Value::Pair(arr1_v, _unit) = rest4.as_ref() else {
        panic!("expected arr1 Pair")
    };
    let to_bit = |v: &Value| -> u8 {
        match v {
            Value::Left(_) => 0,
            Value::Right(_) => 1,
            _ => panic!("not a bit"),
        }
    };
    (k, to_bit(acc_v), to_bit(data_v), to_bit(idx_v), to_bit(arr0_v), to_bit(arr1_v))
}

// ── T1: emission contains exactly 2·(d-1)=4 MexchReg and d-1=2 Xor ──

#[test]
fn t1_emission_counts() {
    let iso = xor_reduce_iso(3);
    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile xorReduce d=3");
    let mexchr_count = lowered
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::MexchReg { .. }))
        .count();
    let xor_count = lowered
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::Xor { .. }))
        .count();
    assert_eq!(
        mexchr_count, 4,
        "xorReduce d=3 must contain exactly 2·(d-1)=4 MexchReg, got {mexchr_count}; ROM: {:?}",
        lowered.instrs
    );
    assert_eq!(
        xor_count, 2,
        "xorReduce d=3 must contain exactly d-1=2 Xor, got {xor_count}; ROM: {:?}",
        lowered.instrs
    );
}

// ── T2: fast oracle == eval over whole d=3 domain (96 inputs) ──────

#[test]
fn t2_fast_oracle_whole_domain() {
    let d = 3;
    let iso = xor_reduce_iso(d);
    let ty = carrier_ty(d);
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 96, "Nat_3 × body must have 96 inhabitants");

    // Verify domain includes idx=1 and k=2=n.
    let mut has_idx1 = false;
    let mut has_k2 = false;
    for input in &domain {
        let (k, _acc, _d, idx, _a0, _a1) = decompose_output(input);
        if idx == 1 {
            has_idx1 = true;
        }
        if k == 2 {
            has_k2 = true;
        }
    }
    assert!(has_idx1, "domain must include idx=1");
    assert!(has_k2, "domain must include k=2 (full reduce)");

    for input in &domain {
        let expected = eval_xor_reduce(d, input);
        let got =
            run_on_cpu_fast_resident(&iso, input, 2).expect("run_on_cpu_fast_resident");
        assert_eq!(
            got, expected,
            "fast oracle mismatch for input {input:?}"
        );
    }
}

// ── T3: real CPU oracle (#[ignore], serialized) ─────────────────────

#[test]
#[ignore]
fn t3_real_cpu_oracle() {
    let d = 3;
    let iso = xor_reduce_iso(d);
    // Sample: k≥1, idx=1, acc≠0, data≠arr[idx] (visible/varying/nonzero).
    let samples = vec![
        make_input(1, 1, 0, 1, 0, 1), // k=1, idx=1, acc=1, data=0, arr=[0,1]; data≠arr[1]=1
        make_input(2, 1, 1, 1, 0, 0), // k=2, idx=1, acc=1
        make_input(2, 1, 0, 1, 1, 0), // k=2, idx=1, acc=1, data=0, arr=[1,0] (data==arr[1]=0 — reduce still visible via acc XOR); sample 1 covers data≠arr[idx]
    ];
    for input in &samples {
        let expected = eval_xor_reduce(d, input);
        let got = run_on_cpu_resident(&iso, input, 2, |iso, v, fuel| {
            theseus_eval::eval(iso, v, fuel)
        })
        .expect("run_on_cpu_resident");
        assert_eq!(
            got, expected,
            "real CPU oracle mismatch for input {input:?}"
        );
    }
}

// ── T4: reduce semantics + idx0=1 + XOR-twice ─────────────────────

#[test]
fn t4_reduce_semantics() {
    let d = 3;

    // d=3, idx0=0: k iterations starting at idx=0.
    // k=1 → acc_out = acc0 ⊕ arr[0], idx_out = 1
    // k=2 → acc_out = acc0 ⊕ arr[0] ⊕ arr[1], idx_out = 0
    for acc in 0..=1u8 {
        for data in 0..=1u8 {
            for arr0 in 0..=1u8 {
                for arr1 in 0..=1u8 {
                    // k=1, idx0=0
                    let input = make_input(1, acc, data, 0, arr0, arr1);
                    let out = eval_xor_reduce(d, &input);
                    let (k_out, acc_out, data_out, idx_out, a0_out, a1_out) =
                        decompose_output(&out);
                    assert_eq!(k_out, 1, "counter preserved");
                    assert_eq!(acc_out, acc ^ arr0, "acc_out = acc0 ⊕ arr[0]");
                    assert_eq!(data_out, data, "data preserved");
                    assert_eq!(idx_out, 1, "idx_out = idx0 ⊕ (k mod 2) = 0⊕1 = 1");
                    assert_eq!(a0_out, arr0, "arr[0] preserved");
                    assert_eq!(a1_out, arr1, "arr[1] preserved");

                    // k=2, idx0=0
                    let input = make_input(2, acc, data, 0, arr0, arr1);
                    let out = eval_xor_reduce(d, &input);
                    let (k_out, acc_out, data_out, idx_out, a0_out, a1_out) =
                        decompose_output(&out);
                    assert_eq!(k_out, 2, "counter preserved");
                    assert_eq!(
                        acc_out,
                        acc ^ arr0 ^ arr1,
                        "acc_out = acc0 ⊕ arr[0] ⊕ arr[1]"
                    );
                    assert_eq!(data_out, data, "data preserved");
                    assert_eq!(idx_out, 0, "idx_out = idx0 ⊕ (k mod 2) = 0⊕0 = 0");
                    assert_eq!(a0_out, arr0, "arr[0] preserved");
                    assert_eq!(a1_out, arr1, "arr[1] preserved");
                }
            }
        }
    }

    // F3-3: d=3, idx0=1, k=1 ⇒ acc_out = acc0 ⊕ arr[1], idx_out = 0
    // (The ONLY regime where an idx0-ignoring wrong program is oracle-invisible.)
    for acc in 0..=1u8 {
        for data in 0..=1u8 {
            for arr0 in 0..=1u8 {
                for arr1 in 0..=1u8 {
                    let input = make_input(1, acc, data, 1, arr0, arr1);
                    let out = eval_xor_reduce(d, &input);
                    let (k_out, acc_out, data_out, idx_out, a0_out, a1_out) =
                        decompose_output(&out);
                    assert_eq!(k_out, 1, "counter preserved");
                    assert_eq!(
                        acc_out,
                        acc ^ arr1,
                        "F3-3: acc_out = acc0 ⊕ arr[1] (idx0=1, k=1)"
                    );
                    assert_eq!(data_out, data, "data preserved");
                    assert_eq!(idx_out, 0, "F3-3: idx_out = 0 (idx0=1, flip once)");
                    assert_eq!(a0_out, arr0, "arr[0] preserved");
                    assert_eq!(a1_out, arr1, "arr[1] preserved");
                }
            }
        }
    }

    // design T4 / §8.4 (XOR-twice-cancels): d=4, k=3, idx0=0 ⇒ acc_out = acc0 ⊕ arr[1] (arr[0] XOR-ed twice cancels).
    // Iterations: idx=0→arr[0], idx=1→arr[1], idx=0→arr[0] again.
    // acc = acc0 ⊕ arr[0] ⊕ arr[1] ⊕ arr[0] = acc0 ⊕ arr[1].
    let d4 = 4;
    for acc in 0..=1u8 {
        for data in 0..=1u8 {
            for arr0 in 0..=1u8 {
                for arr1 in 0..=1u8 {
                    let input = make_input(3, acc, data, 0, arr0, arr1);
                    let out = eval_xor_reduce(d4, &input);
                    let (k_out, acc_out, data_out, idx_out, a0_out, a1_out) =
                        decompose_output(&out);
                    assert_eq!(k_out, 3, "counter preserved");
                    assert_eq!(
                        acc_out,
                        acc ^ arr1,
                        "§8.4: acc_out = acc0 ⊕ arr[1] (arr[0] XOR-ed twice cancels)"
                    );
                    assert_eq!(data_out, data, "data preserved");
                    assert_eq!(
                        idx_out, 1,
                        "idx_out = idx0 ⊕ (k mod 2) = 0⊕1 = 1"
                    );
                    assert_eq!(a0_out, arr0, "arr[0] preserved");
                    assert_eq!(a1_out, arr1, "arr[1] preserved");
                }
            }
        }
    }
}

// ── T5: varying address, exact shape (F3-1) ────────────────────────

#[test]
fn t5_varying_address_exact_shape() {
    let iso = xor_reduce_iso(3);
    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile xorReduce d=3");
    let instrs = &lowered.instrs;

    // Collect all MexchReg positions.
    let mexchr_positions: Vec<(usize, u8, u8)> = instrs
        .iter()
        .enumerate()
        .filter_map(|(i, instr)| match instr {
            Instr::MexchReg { rd, ra } => Some((i, *rd, *ra)),
            _ => None,
        })
        .collect();

    assert_eq!(
        mexchr_positions.len(),
        4,
        "expected 4 MexchReg (2 per copy × 2 copies), got {:?}",
        mexchr_positions
    );

    // MexchReg come in pairs (two per body copy). Each copy's body is:
    // [MexchReg{rd,ra}, Exch{a_r,d_r}, Xor{rd:d_r, rs:a_r}, Exch{a_r,d_r}, MexchReg{rd,ra}, Xori{rd:ra,1}]
    // where d_r = data home = rd (the MexchReg data register),
    //       a_r = acc home.
    for copy_idx in 0..2 {
        let (pos1, rd1, ra1) = mexchr_positions[copy_idx * 2];
        let (pos2, rd2, ra2) = mexchr_positions[copy_idx * 2 + 1];

        // Both MexchReg must share the same rd and ra.
        assert_eq!(
            rd1, rd2,
            "copy {copy_idx}: both MexchReg must share rd, got rd1={rd1} rd2={rd2}"
        );
        assert_eq!(
            ra1, ra2,
            "copy {copy_idx}: both MexchReg must share ra, got ra1={ra1} ra2={ra2}"
        );
        // rd ≠ ra (aliasing rejected).
        assert_ne!(rd1, ra1, "copy {copy_idx}: data reg must differ from address reg");

        let d_r = rd1; // data home register
        let ra = ra1; // address register (idx home)

        // The body slice between the two MexchReg is:
        //   [MexchReg{rd,ra}] @ [Exch{a_r,d_r}, Xor{rd:d_r, rs:a_r}, Exch{a_r,d_r}] @ [MexchReg{rd,ra}]
        // So pos2 = pos1 + 4.
        assert_eq!(
            pos2,
            pos1 + 4,
            "copy {copy_idx}: second MexchReg must be 4 instrs after first, got offset {}",
            pos2 - pos1
        );

        // instrs[pos1+1] = Exch{a_r, d_r}
        let (a_r, d_r_check) = match instrs[pos1 + 1] {
            Instr::Exch { rd: r1, rs: r2 } => {
                // One of them should be d_r
                if r1 == d_r {
                    (r2, r1)
                } else {
                    assert_eq!(r2, d_r, "copy {copy_idx}: Exch must involve data reg d_r={d_r}");
                    (r1, r2)
                }
            }
            ref other => panic!(
                "copy {copy_idx}: expected Exch at pos {}, got {other:?}",
                pos1 + 1
            ),
        };
        assert_eq!(d_r_check, d_r);

        // impl-review #1: a_r must be the ACCUMULATOR register — distinct from the address
        // reg (ra) and the data reg (d_r). Without this, T5 alone would accept an
        // acc-untouched "xor idx into data" slice (a_r==ra) — compositionally killed by
        // T2/T4, but pin it here too so T5 has teeth in isolation.
        assert_ne!(a_r, ra, "copy {copy_idx}: acc reg a_r must differ from address reg ra");
        assert_ne!(a_r, d_r, "copy {copy_idx}: acc reg a_r must differ from data reg d_r");

        // instrs[pos1+2] = Xor{rd: d_r, rs: a_r}  (F3-1: Xor writes DATA's reg, NOT acc's)
        assert_eq!(
            instrs[pos1 + 2],
            Instr::Xor { rd: d_r, rs: a_r },
            "copy {copy_idx}: F3-1 — Xor must be Xor{{rd:{d_r}(data), rs:{a_r}(acc)}}"
        );

        // instrs[pos1+3] = Exch{a_r, d_r} (same as pos1+1)
        assert_eq!(
            instrs[pos1 + 3], instrs[pos1 + 1],
            "copy {copy_idx}: second Exch must match first"
        );

        // After second MexchReg: Xori{rd: ra, 1} (succ_idx)
        assert_eq!(
            instrs[pos2 + 1],
            Instr::Xori { rd: ra, imm: 1 },
            "copy {copy_idx}: succ must be Xori{{rd={ra}, imm=1}} after second memexch"
        );

        // Swapbr closes the body (pins body_len = 6).
        assert!(
            matches!(instrs[pos2 + 2], Instr::Swapbr { .. }),
            "copy {copy_idx}: body must be exactly 6 instrs — Xori followed by closing Swapbr, got {:?}",
            instrs[pos2 + 2]
        );
    }
}

// ── T6a: park-tax histogram TRACE (default tier) ────────────────────

#[test]
fn t6a_trace_histogram() {
    use theseus_tisc::energy::rom_trace;

    let d = 3usize;
    let iso = xor_reduce_iso(d);
    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile xorReduce d=3");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");

    // B = wcet for d=3 iter (predicted: 24).
    let b = lowered.wcet();
    assert_eq!(b, 24, "B (WCET) must be 24 for xorReduce d=3");

    // T(k) = 12 + 6k (6-instr skip spine × 2 copies = 12 base; body_len=6 per active copy).
    let expected_t = |k: usize| -> usize { 12 + 6 * k };
    let ty = carrier_ty(d);
    let mut park_taxes = Vec::new();

    for k in 0..d {
        let input = make_input(k, 0, 0, 0, 1, 0);
        let init = marshal_resident(&input, &ty, 2);
        let trace = rom_trace(&rom, init, b);
        assert_eq!(trace.steps_b, b, "steps_b must equal B={b}");
        assert_eq!(
            trace.steps_t,
            expected_t(k),
            "T(k={k}) must be {}, got {}",
            expected_t(k),
            trace.steps_t
        );
        park_taxes.push((b - trace.steps_t) as f64 / b as f64);
    }

    // Strictly decreasing: 50% > 25% > 0%.
    assert!((park_taxes[0] - 0.5).abs() < 1e-10, "park_tax(0) must be 50%");
    assert!((park_taxes[1] - 0.25).abs() < 1e-10, "park_tax(1) must be 25%");
    assert!(park_taxes[2].abs() < 1e-10, "park_tax(2) must be 0%");
    for i in 0..2 {
        assert!(
            park_taxes[i] > park_taxes[i + 1],
            "park_tax must be strictly decreasing: park_tax({i})={} <= park_tax({})={}",
            park_taxes[i],
            i + 1,
            park_taxes[i + 1]
        );
    }
}

// ── T6b: park-tax histogram PRICING (joules — #[ignore]) ───────────

#[test]
#[ignore]
fn t6b_park_tax_pricing() {
    use theseus_tisc::energy::rom_program_energy_resident;
    use theseus_spice::chardb::rung0_measured;

    let d = 3usize;
    let iso = xor_reduce_iso(d);
    let db = rung0_measured();
    let t_ramp = 1e-7;

    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile xorReduce d=3");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let step_iso = theseus_tisc::step::step(&rom);
    let step_net = theseus_netlist::lower_iso(&step_iso).expect("lower step");

    let b = lowered.wcet();
    let expected_t = |k: usize| -> usize { 12 + 6 * k };

    for k in 0..d {
        let input = make_input(k, 0, 0, 0, 1, 0);
        let pe = rom_program_energy_resident(&iso, &input, 2, &db, t_ramp, &step_net)
            .expect("rom_program_energy_resident");
        assert_eq!(pe.steps_b, b, "steps_b must equal B={b}");
        assert_eq!(pe.steps_t, expected_t(k), "T(k={k}) must match the trace");
        assert!(pe.step_j > 0.0, "step_j must be > 0 (a memory-touching ROM is priced)");
        // total_j == B·step_j is the static-inventory MODEL IDENTITY (not a falsifiable check).
        assert!(
            (pe.total_j - b as f64 * pe.step_j).abs() < 1e-30,
            "total_j is B·step_j (model identity)"
        );
    }
}

// ── T7: closure — MexchReg stays bus-quiescent (#[ignore]) ──────────

#[test]
#[ignore]
fn t7_system_ring_closure() {
    use theseus_tisc::ring_run::system_ring;
    use theseus_tisc::types::{decode_system, encode_system};

    let d = 3;
    let iso = xor_reduce_iso(d);
    let ty = carrier_ty(d);

    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile xorReduce d=3");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let ring = system_ring(&rom);

    // Sample with idx=1, k≥1, acc≠0.
    let sample = make_input(2, 1, 0, 1, 1, 0);
    let init = marshal_resident(&sample, &ty, 2);
    assert!(ring.bus_quiescent(&init), "SystemRing must be bus-quiescent");

    // Check run == eval^R on the sample.
    let sys_val = encode_system(&init);
    let step_iso = theseus_tisc::step::system_step(&rom);
    let mut cur = sys_val.clone();
    let budget = lowered.wcet();
    let mut fuel = 8_000_000_000u64;
    for _ in 0..budget {
        cur = theseus_eval::eval(&step_iso, cur, &mut fuel).expect("step eval");
    }
    let final_state = decode_system(&cur);
    let out = unmarshal_resident(&final_state, &ty, 2);
    let expected = eval_xor_reduce(d, &sample);
    assert_eq!(out, expected, "ring closure oracle mismatch");
}

// ── T-d4: k>n + displaced idx (d=4, F3-4) ──────────────────────────

#[test]
fn t_d4_displaced_idx() {
    let d = 4;
    let iso = xor_reduce_iso(d);

    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile xorReduce d=4");

    // Assert idx-displacement Mexch{_, addr≥2} is present.
    let has_displacement_mexch = lowered.instrs.iter().any(|instr| {
        matches!(instr, Instr::Mexch { rd: _, addr } if *addr >= 2)
    });
    assert!(
        has_displacement_mexch,
        "d=4 ROM must contain a displacement Mexch{{_, addr≥2}}; ROM: {:?}",
        lowered.instrs
    );

    // Pin shape: instrs.len()==56, wcet()==56, MexchReg count==6, Xor count==3.
    assert_eq!(
        lowered.instrs.len(),
        56,
        "d=4 ROM must have exactly 56 instructions, got {}",
        lowered.instrs.len()
    );
    assert_eq!(
        lowered.wcet(),
        56,
        "d=4 WCET must be 56, got {}",
        lowered.wcet()
    );

    let mexchr_count = lowered
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::MexchReg { .. }))
        .count();
    assert_eq!(
        mexchr_count, 6,
        "d=4 must have exactly 6 MexchReg, got {mexchr_count}"
    );

    let xor_count = lowered
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::Xor { .. }))
        .count();
    assert_eq!(
        xor_count, 3,
        "d=4 must have exactly 3 Xor, got {xor_count}"
    );

    // Whole-domain fast oracle: d=4 × 32 body vals = 128 inputs.
    let ty = carrier_ty(d);
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 128, "Nat_4 × body must have 128 inhabitants");

    // Verify domain includes k=3 (the XOR-twice-cancels case).
    let has_k3 = domain.iter().any(|v| decompose_output(v).0 == 3);
    assert!(has_k3, "domain must include k=3");

    for input in &domain {
        let expected = eval_xor_reduce(d, input);
        let got =
            run_on_cpu_fast_resident(&iso, input, 2).expect("run_on_cpu_fast_resident d=4");
        assert_eq!(
            got, expected,
            "d=4 fast oracle mismatch for input {input:?}"
        );
    }
}
