//! Reversible in-memory array map tests (M-D.b2.f2).
//!
//! Proves: `arrayMap = iter Nat_4 BODY` (d=4, n=2, g=notb) compiles via
//! `compile_with_metadata_resident` and matches `eval` over the full domain.
//! Each test maps to a design finding (§7 of mdb2-f2-design.md).
//!
//! OOM discipline: real-CPU / closure tests are `#[ignore]` + serialized.

use theseus_core::prim::enumerate_values;
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    compile_with_metadata_resident, marshal_resident, memexch_prim,
    run_on_cpu_fast_resident, run_on_cpu_resident, unmarshal_resident,
};

// ── Type helpers ────────────────────────────────────────────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// `notb : Bit ↔ Bit` — the bijection g.
fn notb() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// The body type: (data:Bit) × (idx:Bit) × (arr0:Bit) × (arr1:Bit) × 1
fn body_ty() -> Type {
    let b = bit();
    Type::prod(
        b.clone(),
        Type::prod(b.clone(), Type::prod(b.clone(), Type::prod(b.clone(), Type::One))),
    )
}

/// The idx+arr remainder type: (idx:Bit) × (arr0:Bit) × (arr1:Bit) × 1
fn idx_arr_ty() -> Type {
    let b = bit();
    Type::prod(b.clone(), Type::prod(b.clone(), Type::prod(b.clone(), Type::One)))
}

/// The arr type: (arr0:Bit) × (arr1:Bit) × 1
fn arr_ty() -> Type {
    let b = bit();
    Type::prod(b.clone(), Type::prod(b.clone(), Type::One))
}

/// The full carrier type: Nat_4 × body_ty  (8 leaves; head 6 with resident=2).
fn carrier_ty(d: usize) -> Type {
    Type::prod(Type::numeral_ty(d), body_ty())
}

// ── Program construction ────────────────────────────────────────────

/// Build memexch Iso for (data:Bit, idx:Bit, arr[Bit;2]).
fn memexch_iso() -> Iso {
    let def = memexch_prim(&bit(), 2).expect("memexch_prim(Bit,2)");
    Iso::Prim(Box::new(def))
}

/// Build the array map body:
///   BODY = memexch ; (g ⊗ id_{idx,arr}) ; memexch ; (id_data ⊗ succ_idx ⊗ id_arr)
fn array_map_body() -> Iso {
    let mexch = memexch_iso();
    let g = notb(); // on data (Bit)
    let succ = notb(); // on idx (Bit)

    // g ⊗ id_{idx,arr} : (data, idx, arr) ↔ (g(data), idx, arr)
    let g_times_id = Iso::times(g, Iso::Id(idx_arr_ty()));

    // id_data ⊗ (succ_idx ⊗ id_arr) : (data, idx, arr) ↔ (data, succ(idx), arr)
    let succ_times_id = Iso::times(Iso::Id(bit()), Iso::times(succ, Iso::Id(arr_ty())));

    // BODY = memexch ; (g ⊗ id) ; memexch ; (id ⊗ succ ⊗ id)
    Iso::seq(
        Iso::seq(Iso::seq(mexch.clone(), g_times_id), mexch),
        succ_times_id,
    )
}

/// Build the full arrayMap program: iter Nat_d BODY.
fn array_map_iso(d: usize) -> Iso {
    Iso::iter(Type::numeral_ty(d), array_map_body())
}

/// Reference eval for the array map.
fn eval_array_map(d: usize, input: &Value) -> Value {
    let iso = array_map_iso(d);
    let mut fuel = 10_000_000u64;
    theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval array_map")
}

// ── Numeral value constructors ──────────────────────────────────────

/// Build the numeral value k in Nat_d.
/// Encoding: numeral i = inr^i(inl ()).
/// 0 = Left(Unit), 1 = Right(Left(Unit)), 2 = Right(Right(Left(Unit))), ...
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

/// Build a carrier value: (numeral k, (data, (idx, (arr0, (arr1, ())))))
fn make_input(k: usize, data: u8, idx: u8, arr0: u8, arr1: u8) -> Value {
    Value::pair(
        numeral_val(k),
        Value::pair(
            bit_val(data),
            Value::pair(
                bit_val(idx),
                Value::pair(bit_val(arr0), Value::pair(bit_val(arr1), Value::Unit)),
            ),
        ),
    )
}

/// Extract (k_tag_count, data, idx, arr0, arr1) from a carrier value.
/// k_tag_count = how many Right wrappers before the Left(Unit).
fn decompose_output(v: &Value) -> (usize, u8, u8, u8, u8) {
    let Value::Pair(counter, body) = v else {
        panic!("expected Pair, got {v:?}")
    };
    // Count the numeral
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
    let Value::Pair(data_v, rest1) = body.as_ref() else {
        panic!("expected body Pair")
    };
    let Value::Pair(idx_v, rest2) = rest1.as_ref() else {
        panic!("expected idx Pair")
    };
    let Value::Pair(arr0_v, rest3) = rest2.as_ref() else {
        panic!("expected arr0 Pair")
    };
    let Value::Pair(arr1_v, _unit) = rest3.as_ref() else {
        panic!("expected arr1 Pair")
    };
    let to_bit = |v: &Value| -> u8 {
        match v {
            Value::Left(_) => 0,
            Value::Right(_) => 1,
            _ => panic!("not a bit"),
        }
    };
    (k, to_bit(data_v), to_bit(idx_v), to_bit(arr0_v), to_bit(arr1_v))
}

// ── T1: emission contains exactly 2·(d-1) = 6 MexchReg ─────────────

#[test]
fn t1_emission_mexchr_count() {
    let iso = array_map_iso(4);
    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile arrayMap d=4");
    let mexchr_count = lowered
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::MexchReg { .. }))
        .count();
    assert_eq!(
        mexchr_count,
        6,
        "arrayMap d=4 must contain exactly 2·(d-1)=6 MexchReg, got {mexchr_count}; ROM: {:?}",
        lowered.instrs
    );
}

// ── T2: fast oracle == eval over whole 64-input domain ──────────────

#[test]
fn t2_fast_oracle_whole_domain() {
    let d = 4;
    let iso = array_map_iso(d);
    let ty = carrier_ty(d);
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 64, "Nat_4 × body must have 64 inhabitants");

    // Verify the domain includes idx=1 and k=3 (the g² case).
    let mut has_idx1 = false;
    let mut has_k3 = false;
    for input in &domain {
        let (k, _d, idx, _a0, _a1) = decompose_output(input);
        if idx == 1 {
            has_idx1 = true;
        }
        if k == 3 {
            has_k3 = true;
        }
    }
    assert!(has_idx1, "domain must include idx=1");
    assert!(has_k3, "domain must include k=3 (g² case, k>n=2)");

    for input in &domain {
        let expected = eval_array_map(d, input);
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
    let d = 4;
    let iso = array_map_iso(d);
    // Sample that includes k≥1 AND idx=1, with NONZERO varying address.
    // Pick inputs with k=1,idx=1 and k=2,idx=1 and k=3,idx=0.
    let samples = vec![
        make_input(1, 0, 1, 1, 0), // k=1, idx=1: addresses arr[1] then succ→idx=0
        make_input(2, 1, 1, 0, 1), // k=2, idx=1: addresses arr[1],arr[0]
        make_input(3, 0, 0, 1, 1), // k=3, idx=0: the g² case
        make_input(1, 1, 0, 0, 1), // k=1, idx=0
    ];
    for input in &samples {
        let expected = eval_array_map(d, input);
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

// ── T4 / T-k3: map semantics + g² ──────────────────────────────────

#[test]
fn t4_map_semantics_and_g_squared() {
    let d = 4;

    // k=2, idx=0 ⇒ arr_out = (g(arr0), g(arr1)), data/counter unchanged.
    // With g=notb: arr_out = (¬arr0, ¬arr1), idx_out = 0 (even k).
    for data in 0..=1u8 {
        for arr0 in 0..=1u8 {
            for arr1 in 0..=1u8 {
                let input = make_input(2, data, 0, arr0, arr1);
                let out = eval_array_map(d, &input);
                let (k_out, d_out, idx_out, a0_out, a1_out) = decompose_output(&out);
                assert_eq!(k_out, 2, "counter must be preserved");
                assert_eq!(d_out, data, "data must be preserved (borrowed scratch)");
                assert_eq!(idx_out, 0, "idx must return to 0 (k=2 even)");
                assert_eq!(a0_out, arr0 ^ 1, "arr[0] must be g(arr0) = ¬arr0");
                assert_eq!(a1_out, arr1 ^ 1, "arr[1] must be g(arr1) = ¬arr1");
            }
        }
    }

    // k=3, idx=0, g=notb ⇒ arr_out = (arr0, ¬arr1), idx_out = 1.
    // g² = id on the doubly-hit word (arr[0] is hit at iter 0 and iter 2).
    for data in 0..=1u8 {
        for arr0 in 0..=1u8 {
            for arr1 in 0..=1u8 {
                let input = make_input(3, data, 0, arr0, arr1);
                let out = eval_array_map(d, &input);
                let (k_out, d_out, idx_out, a0_out, a1_out) = decompose_output(&out);
                assert_eq!(k_out, 3, "counter must be preserved");
                assert_eq!(d_out, data, "data must be preserved");
                assert_eq!(idx_out, 1, "idx must be idx0⊕1 = 1 (k=3 odd)");
                assert_eq!(
                    a0_out, arr0,
                    "arr[0] must be arr0 (g²=id on doubly-hit word)"
                );
                assert_eq!(a1_out, arr1 ^ 1, "arr[1] must be ¬arr1");
            }
        }
    }
}

// ── T5: varying address — exact per-copy body slice shape ───────────

#[test]
fn t5_varying_address_exact_shape() {
    let iso = array_map_iso(4);
    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile arrayMap d=4");

    // The ROM structure: aux_page_in, then d-1=3 skip-guarded copies,
    // then aux_page_out. Each copy is:
    //   [ctrl_pi, Addi, Cswap, Swapbr, <body>, Swapbr, Cswap, Subi, ctrl_po]
    // The body slice is exactly 4 instructions:
    //   [MexchReg{rd,ra}, Xori{rd,1}, MexchReg{rd,ra}, Xori{ra,1}]
    //
    // Find all MexchReg instructions and verify the body slices.
    let instrs = &lowered.instrs;

    // Collect all (index, MexchReg{rd,ra}) positions.
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
        6,
        "expected 6 MexchReg, got {:?}",
        mexchr_positions
    );

    // MexchReg come in pairs (two per body copy). Check each pair.
    for pair_idx in 0..3 {
        let (pos1, rd1, ra1) = mexchr_positions[pair_idx * 2];
        let (pos2, rd2, ra2) = mexchr_positions[pair_idx * 2 + 1];

        // Both MexchReg must share the same ra (address register).
        assert_eq!(
            ra1, ra2,
            "copy {pair_idx}: both MexchReg must share ra, got ra1={ra1} ra2={ra2}"
        );
        // Both MexchReg must share the same rd (data register).
        assert_eq!(
            rd1, rd2,
            "copy {pair_idx}: both MexchReg must share rd, got rd1={rd1} rd2={rd2}"
        );

        // The 4-instruction body slice is [MexchReg{rd,ra}, Xori{rd,1}, MexchReg{rd,ra}, Xori{ra,1}].
        // Between the two MexchReg is g's Xori{rd,1}.
        assert_eq!(
            pos2,
            pos1 + 2,
            "copy {pair_idx}: second MexchReg must be 2 instrs after first"
        );
        assert_eq!(
            instrs[pos1 + 1],
            Instr::Xori { rd: rd1, imm: 1 },
            "copy {pair_idx}: g must be Xori{{rd={rd1}, imm=1}} between memexch pair"
        );
        // After the second MexchReg is succ's Xori{ra,1} — the LAST body instruction.
        assert_eq!(
            instrs[pos2 + 1],
            Instr::Xori { rd: ra1, imm: 1 },
            "copy {pair_idx}: succ must be Xori{{rd={ra1}, imm=1}} after second memexch"
        );
    }
}

// ── T6: park-tax histogram (exact) via rom_program_energy_resident ───

#[test]
#[ignore]
fn t6_park_tax_histogram() {
    use theseus_tisc::energy::{rom_program_energy_resident, rom_trace};
    use theseus_spice::chardb::rung0_measured;

    let d = 4usize;
    let iso = array_map_iso(d);
    let db = rung0_measured();
    let t_ramp = 1e-7;

    // Lower the step net once (expensive).
    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile arrayMap d=4");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let step_iso = theseus_tisc::step::step(&rom);
    let step_net = theseus_netlist::lower_iso(&step_iso).expect("lower step");

    // B = 30 (ROM length for d=4 iter).
    let b = lowered.wcet();
    assert_eq!(b, 30, "B (WCET) must be 30 for arrayMap d=4");

    // T(k) = 18 + 4k (6-instr skip spine × 3 copies = 18 base; body_len=4 per active copy).
    let expected_t = |k: usize| -> usize { 18 + 4 * k };
    let expected_park_tax = |k: usize| -> f64 {
        (b - expected_t(k)) as f64 / b as f64
    };

    // Price through the API for one representative input per k.
    let mut park_taxes = Vec::new();

    for k in 0..d {
        let input = make_input(k, 0, 0, 1, 0);
        let ty = carrier_ty(d);
        let init = marshal_resident(&input, &ty, 2);
        let trace = rom_trace(&rom, init, b);

        assert_eq!(
            trace.steps_t,
            expected_t(k),
            "T(k={k}) must be {}, got {}",
            expected_t(k),
            trace.steps_t
        );

        let pe = rom_program_energy_resident(&iso, &input, 2, &db, t_ramp, &step_net)
            .expect("rom_program_energy_resident");

        assert_eq!(pe.steps_b, b, "steps_b must equal B={b}");

        // Park tax must match expected.
        let expected_pt = expected_park_tax(k);
        let diff = (pe.park_tax - expected_pt).abs();
        assert!(
            diff < 1e-10,
            "park_tax(k={k}): expected {expected_pt:.6}, got {:.6}",
            pe.park_tax
        );
        park_taxes.push(pe.park_tax);

        // total_j == B * step_j exact.
        let total_expected = b as f64 * pe.step_j;
        assert!(
            (pe.total_j - total_expected).abs() < 1e-30,
            "total_j must be B*step_j exactly"
        );
    }

    // Park tax must be strictly decreasing: 40% > 26.7% > 13.3% > 0%.
    assert!((park_taxes[0] - 0.4).abs() < 1e-10, "park_tax(0) must be 40%");
    assert!(
        (park_taxes[1] - 8.0 / 30.0).abs() < 1e-10,
        "park_tax(1) must be 8/30 ≈ 26.7%"
    );
    assert!(
        (park_taxes[2] - 4.0 / 30.0).abs() < 1e-10,
        "park_tax(2) must be 4/30 ≈ 13.3%"
    );
    assert!(
        park_taxes[3].abs() < 1e-10,
        "park_tax(3) must be 0%"
    );
    for i in 0..3 {
        assert!(
            park_taxes[i] > park_taxes[i + 1],
            "park_tax must be strictly decreasing: park_tax({i})={} <= park_tax({})={}",
            park_taxes[i],
            i + 1,
            park_taxes[i + 1]
        );
    }
}

// ── T7: closure — MexchReg stays bus-quiescent (#[ignore]) ──────────

#[test]
#[ignore]
fn t7_system_ring_closure() {
    use theseus_tisc::ring_run::system_ring;
    use theseus_tisc::types::{decode_system, encode_system};

    let d = 4;
    let iso = array_map_iso(d);
    let ty = carrier_ty(d);

    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile arrayMap d=4");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let ring = system_ring(&rom);

    // Use a sample with idx=1, k≥1 (nonzero varying address).
    let sample = make_input(2, 1, 1, 0, 1);
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
    let expected = eval_array_map(d, &sample);
    assert_eq!(out, expected, "ring closure oracle mismatch");
}

// ── T-F2-7: displaced-idx regime (d=5) ──────────────────────────────

#[test]
fn t_f2_7_displaced_idx_d5() {
    let d = 5;
    let iso = array_map_iso(d);

    let lowered =
        compile_with_metadata_resident(&iso, 2).expect("compile arrayMap d=5");

    // Assert the aux page-in contains a displacement Mexch{_, addr≥2}
    // (idx displaced to Mem(2) or higher).
    let has_displacement_mexch = lowered.instrs.iter().any(|instr| {
        matches!(instr, Instr::Mexch { rd: _, addr } if *addr >= 2)
    });
    assert!(
        has_displacement_mexch,
        "d=5 ROM must contain a displacement Mexch{{_, addr≥2}}; ROM: {:?}",
        lowered.instrs
    );

    // Whole-domain fast oracle: 5 × 16 = 80 inputs.
    let ty = carrier_ty(d);
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 80, "Nat_5 × body must have 80 inhabitants");

    for input in &domain {
        let expected = eval_array_map(d, input);
        let got =
            run_on_cpu_fast_resident(&iso, input, 2).expect("run_on_cpu_fast_resident d=5");
        assert_eq!(
            got, expected,
            "d=5 fast oracle mismatch for input {input:?}"
        );
    }
}
