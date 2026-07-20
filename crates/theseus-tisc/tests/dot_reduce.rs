//! Masked-XOR REDUCE tests: `acc ⊕= key ∧ arr[i]` (M-D.b2.f3.5).
//!
//! The program uses the Ccnot instruction (byte-parallel Toffoli) inside a borrow-swap
//! loop: memexch loads arr[idx] into data, reduceStep3 applies Ccnot, memexch
//! restores, succ advances idx.
//!
//! T5: masked reduce d=3 — run_on_cpu_fast == eval for key=0, key=1, mixed.
//! T6a: park-tax histogram via rom_trace.
//! T6b: rom_program_energy_resident (#[ignore]).
//! T7: SystemRing closure validate()==Ok with Op::Ccnot in Op::ALL (#[ignore]).

use std::collections::BTreeMap;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    compile_with_metadata_resident, marshal_resident, memexch_prim,
    run_on_cpu_fast_resident,
    value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
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

/// Body type: `(acc:Bit) × (key:Bit) × (data:Bit) × (idx:Bit) × (arr0:Bit) × (arr1:Bit) × 1`
fn body_ty() -> Type {
    let b = bit();
    Type::prod(
        b.clone(),
        Type::prod(
            b.clone(),
            Type::prod(
                b.clone(),
                Type::prod(b.clone(), Type::prod(b.clone(), Type::prod(b.clone(), Type::One))),
            ),
        ),
    )
}

/// Full carrier: `Nat_d × body_ty` (head = d+4 leaves with resident=2).
fn carrier_ty(d: usize) -> Type {
    Type::prod(Type::numeral_ty(d), body_ty())
}

// ── Program construction ────────────────────────────────────────────

/// Build memexch Iso for `(data:Bit, idx:Bit, arr[Bit;2])`.
fn memexch_iso() -> Iso {
    let def = memexch_prim(&bit(), 2).expect("memexch_prim(Bit,2)");
    Iso::Prim(Box::new(def))
}

/// Build the Ccnot prim (reduceStep3): `(acc, key, data) ↦ (acc ⊕ (key&1 & data&1), key, data)`
/// on `Bit × (Bit × Bit)`. TARGET-FIRST so recognizer emits Ccnot{rd=acc, rs=key, rt=data}.
fn reduce_step3_prim() -> Iso {
    let b = bit();
    let ty = Type::prod(b.clone(), Type::prod(b.clone(), b.clone()));
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let acc = bytes[0];
        let key = bytes[1];
        let data = bytes[2];
        let out_acc = acc ^ ((key & 1) & (data & 1));
        let out_bytes = vec![out_acc, key, data];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        forward.insert(v.clone(), out_val);
    }

    // Self-inverse (Toffoli is involution).
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let acc = bytes[0];
        let key = bytes[1];
        let data = bytes[2];
        let out_acc = acc ^ ((key & 1) & (data & 1));
        let out_bytes = vec![out_acc, key, data];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        backward.insert(v.clone(), out_val);
    }

    let def = PrimDef {
        forward_name: "ccnot".into(),
        backward_name: "ccnot".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
    };
    Iso::Prim(Box::new(def))
}

/// `succ_idx = notb = SwapSum(One, One)` on Bit (0↔1, → Xori{idx,1}).
fn succ_idx() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// Build the masked-XOR reduce body:
/// ```text
/// BODY : (acc, key, data, idx, arr) ↔ (acc, key, data, idx, arr) =
///     (id_{acc,key} ⊗ memexch)                         # data ↔ arr[idx]
///   ; assocL ; (reduceStep3 ⊗ id_{idx,arr}) ; assocR   # acc ⊕= key∧data
///   ; (id_{acc,key} ⊗ memexch)                         # restore
///   ; (id_{acc,key,data} ⊗ succ_idx ⊗ id_arr)          # idx advances
/// ```
fn dot_reduce_body() -> Iso {
    let b = bit();
    let mexch = memexch_iso();

    // Types for regrouping:
    // body = (acc, (key, (data, (idx, (arr0, (arr1, ()))))))
    // memexch acts on (data, idx, arr) = the tail starting from data.
    // reduce_step3 acts on (acc, key, data) = the first 3 leaves.

    // (id_{acc,key} ⊗ memexch): on (acc, (key, (data, idx, arr)))
    let id_acc_key_mexch = Iso::times(
        Iso::Id(b.clone()),
        Iso::times(Iso::Id(b.clone()), mexch.clone()),
    );

    // For reduceStep3, we need (acc, key, data) contiguous. The carrier is:
    //   (acc, (key, (data, (idx, arr))))
    // We can regroup: AssocL on (acc, key, rest) gives ((acc, key), rest)...
    // Actually the prim acts on Bit × (Bit × Bit), which is (acc, (key, data)).
    // The carrier has (acc, (key, (data, (idx, arr)))).
    // We need to isolate (acc, (key, data)) from (idx, arr).
    //
    // Strategy: AssocL on key's product to get (key, data) out of (key, (data, (idx, arr))):
    //   (key, (data, idx_arr)) → use AssocL(key, data, idx_arr) → ((key, data), idx_arr)
    // Then from (acc, ((key, data), idx_arr)) → AssocL(acc, (key,data), idx_arr) → ((acc,(key,data)), idx_arr)
    // That gives ((acc, (key, data)), (idx, arr)) — reduceStep3 on the left.

    // inner_assocl: (key, (data, idx_arr)) → ((key, data), idx_arr)
    let inner_assocl = Iso::AssocLProd(b.clone(), b.clone(), idx_arr_ty());
    // outer_assocl: (acc, ((key, data), idx_arr)) → ((acc, (key, data)), idx_arr)
    let kd_ty = Type::prod(b.clone(), b.clone());
    let outer_assocl = Iso::AssocLProd(b.clone(), kd_ty.clone(), idx_arr_ty());

    // Compose: (acc, (key, (data, idx_arr))) → (acc, ((key,data), idx_arr)) → ((acc,(key,data)), idx_arr)
    let assocl_full = Iso::seq(
        Iso::times(Iso::Id(b.clone()), inner_assocl.clone()),
        outer_assocl.clone(),
    );

    // reduceStep3 ⊗ id_{idx,arr}
    let rs3_times_id = Iso::times(reduce_step3_prim(), Iso::Id(idx_arr_ty()));

    // assocr_full: inverse of assocl_full
    let inner_assocr = Iso::AssocRProd(b.clone(), b.clone(), idx_arr_ty());
    let outer_assocr = Iso::AssocRProd(b.clone(), kd_ty, idx_arr_ty());
    let assocr_full = Iso::seq(
        outer_assocr,
        Iso::times(Iso::Id(b.clone()), inner_assocr),
    );

    // succ part: (id_{acc,key,data} ⊗ succ_idx ⊗ id_arr)
    let succ_part = Iso::times(
        Iso::Id(b.clone()),
        Iso::times(
            Iso::Id(b.clone()),
            Iso::times(Iso::Id(b.clone()), Iso::times(succ_idx(), Iso::Id(arr_ty()))),
        ),
    );

    // Compose: id_acc_key_mexch ; assocl ; rs3_times_id ; assocr ; id_acc_key_mexch ; succ
    Iso::seq(
        Iso::seq(
            Iso::seq(
                Iso::seq(Iso::seq(id_acc_key_mexch.clone(), assocl_full), rs3_times_id),
                assocr_full,
            ),
            id_acc_key_mexch,
        ),
        succ_part,
    )
}

/// Build the full dot-reduce program: `iter Nat_d BODY`.
fn dot_reduce_iso(d: usize) -> Iso {
    Iso::iter(Type::numeral_ty(d), dot_reduce_body())
}

/// Reference eval for the dot reduce.
fn eval_dot_reduce(d: usize, input: &Value) -> Value {
    let iso = dot_reduce_iso(d);
    let mut fuel = 10_000_000u64;
    theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval dot_reduce")
}

// ── Value constructors ─────────────────────────────────────────────

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

/// Build a carrier value: `(numeral k, (acc, (key, (data, (idx, (arr0, (arr1, ())))))))`
fn make_input(k: usize, acc: u8, key: u8, data: u8, idx: u8, arr0: u8, arr1: u8) -> Value {
    Value::pair(
        numeral_val(k),
        Value::pair(
            bit_val(acc),
            Value::pair(
                bit_val(key),
                Value::pair(
                    bit_val(data),
                    Value::pair(
                        bit_val(idx),
                        Value::pair(bit_val(arr0), Value::pair(bit_val(arr1), Value::Unit)),
                    ),
                ),
            ),
        ),
    )
}

/// Extract `(k, acc, key, data, idx, arr0, arr1)` from a carrier value.
fn decompose_output(v: &Value) -> (usize, u8, u8, u8, u8, u8, u8) {
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
    let Value::Pair(key_v, rest2) = rest1.as_ref() else {
        panic!("expected key Pair")
    };
    let Value::Pair(data_v, rest3) = rest2.as_ref() else {
        panic!("expected data Pair")
    };
    let Value::Pair(idx_v, rest4) = rest3.as_ref() else {
        panic!("expected idx Pair")
    };
    let Value::Pair(arr0_v, rest5) = rest4.as_ref() else {
        panic!("expected arr0 Pair")
    };
    let Value::Pair(arr1_v, _unit) = rest5.as_ref() else {
        panic!("expected arr1 Pair")
    };
    let to_bit = |v: &Value| -> u8 {
        match v {
            Value::Left(_) => 0,
            Value::Right(_) => 1,
            _ => panic!("not a bit"),
        }
    };
    (
        k,
        to_bit(acc_v),
        to_bit(key_v),
        to_bit(data_v),
        to_bit(idx_v),
        to_bit(arr0_v),
        to_bit(arr1_v),
    )
}

// ── T5: masked reduce d=3 — run_on_cpu_fast == eval ────────────────

#[test]
fn t5_masked_reduce_fast_oracle() {
    let d = 3;
    let iso = dot_reduce_iso(d);
    let ty = carrier_ty(d);
    let domain = enumerate_values(&ty);
    // Nat_3 × (acc × key × data × idx × arr0 × arr1) = 3 × 2^6 = 192
    assert_eq!(domain.len(), 192, "Nat_3 × body must have 192 inhabitants");

    // Verify the compilation produces exactly one Ccnot per body copy.
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile dot_reduce d=3");
    let ccnot_count = lowered
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::Ccnot { .. }))
        .count();
    assert_eq!(
        ccnot_count,
        2, // d-1 = 2 body copies, each has one Ccnot
        "dot_reduce d=3 must have exactly 2 Ccnot (one per body copy), got {ccnot_count}; ROM: {:?}",
        lowered.instrs
    );

    // Verify the Ccnot target is the acc home register (Reg 3: the Nat_3 counter
    // occupies Reg 0-2, acc = first payload leaf). All Ccnot must target the same register.
    let ccnot_rds: Vec<u8> = lowered
        .instrs
        .iter()
        .filter_map(|i| match i {
            Instr::Ccnot { rd, .. } => Some(*rd),
            _ => None,
        })
        .collect();
    assert!(
        ccnot_rds.iter().all(|&r| r == ccnot_rds[0]),
        "all Ccnot instructions must target the same register (acc home), got {ccnot_rds:?}"
    );
    assert_eq!(
        ccnot_rds[0], 3,
        "Ccnot rd must be 3 (acc home: Nat_3 counter occupies Reg 0-2, acc = first payload → Reg 3)"
    );

    // Full domain oracle check.
    for input in &domain {
        let expected = eval_dot_reduce(d, input);
        let got = run_on_cpu_fast_resident(&iso, input, 2).expect("run_on_cpu_fast_resident");
        assert_eq!(
            got, expected,
            "fast oracle mismatch for input {input:?}"
        );
    }
}

#[test]
fn t5_key0_acc_unchanged() {
    let d = 3;
    // key=0 means the Toffoli never fires → acc stays unchanged (distinguishes from CNOT).
    for k in 1..d {
        for acc in 0..=1u8 {
            for data in 0..=1u8 {
                for arr0 in 0..=1u8 {
                    for arr1 in 0..=1u8 {
                        let input = make_input(k, acc, 0, data, 0, arr0, arr1);
                        let out = eval_dot_reduce(d, &input);
                        let (_, acc_out, key_out, _, _, _, _) = decompose_output(&out);
                        assert_eq!(
                            acc_out, acc,
                            "key=0 must leave acc unchanged (k={k}, acc={acc}, arr=[{arr0},{arr1}])"
                        );
                        assert_eq!(key_out, 0, "key preserved");
                    }
                }
            }
        }
    }
}

#[test]
fn t5_key1_reduces_to_xor_parity() {
    let d = 3;
    // key=1 means the Toffoli fires whenever data=1, so acc ⊕= arr[i] (like f3 XOR reduce).
    for acc in 0..=1u8 {
        for data in 0..=1u8 {
            for arr0 in 0..=1u8 {
                for arr1 in 0..=1u8 {
                    // k=2, idx=0: iterates twice (arr[0], arr[1])
                    let input = make_input(2, acc, 1, data, 0, arr0, arr1);
                    let out = eval_dot_reduce(d, &input);
                    let (_, acc_out, key_out, data_out, idx_out, a0_out, a1_out) =
                        decompose_output(&out);
                    assert_eq!(
                        acc_out,
                        acc ^ arr0 ^ arr1,
                        "key=1, k=2: acc_out = acc ⊕ arr[0] ⊕ arr[1]"
                    );
                    assert_eq!(key_out, 1, "key preserved");
                    assert_eq!(data_out, data, "data preserved");
                    assert_eq!(idx_out, 0, "idx_out = 0 (k=2 mod 2 = 0)");
                    assert_eq!(a0_out, arr0, "arr[0] preserved");
                    assert_eq!(a1_out, arr1, "arr[1] preserved");
                }
            }
        }
    }
}

// ── T6a: park-tax histogram (default tier) ──────────────────────────

#[test]
fn t6a_trace_histogram() {
    use theseus_tisc::energy::rom_trace;

    let d = 3usize;
    let iso = dot_reduce_iso(d);
    let ty = carrier_ty(d);
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile dot_reduce d=3");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");

    // B = wcet for d=3 iter. MEASURED (displaced regime, per design F35-6): at d=3 the
    // Nat_3 counter + payload (acc,key,data,idx,arr0,arr1) leave idx displaced to Mem, so
    // the body PAGES idx each iteration → body_len = 10 productive instrs (MexchReg +
    // Ccnot + MexchReg + Xori + idx-displacement paging), not the naive clean-regime 4.
    // copies = d-1 = 2 → B = 34.
    let b = lowered.wcet();
    assert_eq!(b, 34, "B (WCET) must be 34 for dot_reduce d=3");

    // MEASURED T(k) = 14 + 10k → T(0)=14, T(1)=24, T(2)=34=B (affine, tight numeral k_max).
    let expected_t = |k: usize| -> usize { 14 + 10 * k };

    let mut measured_t = Vec::new();
    for k in 0..d {
        let input = make_input(k, 0, 1, 0, 0, 1, 0);
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
        measured_t.push(trace.steps_t);
    }

    // T(d-1) == B (park-tax 0 at max k).
    assert_eq!(
        measured_t[d - 1], b,
        "T(d-1) must equal B (zero park-tax at max k)"
    );

    // Park-tax: strictly decreasing, ends at 0.
    // park_tax(0) = (34-14)/34 = 20/34 ≈ 58.8%, park_tax(1) = (34-24)/34 = 10/34 ≈ 29.4%,
    // park_tax(2) = 0%.
    let park_taxes: Vec<f64> = measured_t
        .iter()
        .map(|&t| (b - t) as f64 / b as f64)
        .collect();
    assert!((park_taxes[0] - 20.0 / 34.0).abs() < 1e-10, "park_tax(0) must be 20/34");
    assert!((park_taxes[1] - 10.0 / 34.0).abs() < 1e-10, "park_tax(1) must be 10/34");
    assert!(park_taxes[2].abs() < 1e-10, "park_tax(2) must be 0%");
    for i in 0..d - 1 {
        assert!(
            park_taxes[i] > park_taxes[i + 1],
            "park_tax must be strictly decreasing: park_tax({i})={} <= park_tax({})={}",
            park_taxes[i],
            i + 1,
            park_taxes[i + 1]
        );
    }
}

// ── T6b: energy pricing (#[ignore]) ─────────────────────────────────

#[test]
#[ignore = "energy pricing requires chardb + netlist lowering; run via --ignored"]
fn t6b_energy_pricing() {
    use theseus_tisc::energy::rom_program_energy_resident;
    use theseus_spice::chardb::rung0_measured;

    let d = 3usize;
    let iso = dot_reduce_iso(d);
    let db = rung0_measured();
    let t_ramp = 1e-7;

    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile dot_reduce d=3");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let step_iso = theseus_tisc::step::step(&rom);
    let step_net = theseus_netlist::lower_iso(&step_iso).expect("lower step");

    let b = lowered.wcet();

    for k in 0..d {
        let input = make_input(k, 0, 1, 0, 0, 1, 0);
        let pe = rom_program_energy_resident(&iso, &input, 2, &db, t_ramp, &step_net)
            .expect("rom_program_energy_resident");
        assert_eq!(pe.steps_b, b, "steps_b must equal B={b}");
        assert!(pe.step_j > 0.0, "step_j must be > 0");
        assert!(
            (pe.total_j - b as f64 * pe.step_j).abs() < 1e-30,
            "total_j == B·step_j (model identity)"
        );
    }
}

// ── T7: SystemRing closure validate()==Ok (#[ignore]) ───────────────
//
// Design §7.4 obligation: validate()==Ok WITH Op::Ccnot in Op::ALL.
// The default-tier ring_two_body_timing::stage2b_lite_timing_certificate
// already exercises validate() on the full decode_execute netlist (which
// includes Op::Ccnot via Op::ALL). This test adds a dot-reduce-specific
// system_step oracle check and an explicit validate() call on the
// dot-reduce ROM's two_body_timing_certificate.

#[test]
#[ignore = "SystemRing closure requires full system_step lowering; run via --ignored"]
fn t7_system_ring_closure() {
    use theseus_tisc::ring_run::{system_ring, two_body_timing_certificate};
    use theseus_tisc::types::{decode_system, encode_system};

    let d = 3;
    let iso = dot_reduce_iso(d);
    let ty = carrier_ty(d);

    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile dot_reduce d=3");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let ring = system_ring(&rom);

    // §7.4: validate()==Ok on the dot-reduce ROM's timing certificate.
    let cert = two_body_timing_certificate(&rom).expect("no rail skew on dot-reduce ROM");
    cert.validate()
        .expect("validate() must pass on dot-reduce ROM (§7.4 closure obligation)");

    // Sample with k≥1, key=1, acc≠0.
    let sample = make_input(2, 1, 1, 0, 0, 1, 0);
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
    let out = theseus_tisc::compile::unmarshal_resident(&final_state, &ty, 2);
    let expected = eval_dot_reduce(d, &sample);
    assert_eq!(out, expected, "ring closure oracle mismatch");
}
