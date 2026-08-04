//! Value-width slice-2: byte-wide XOR REDUCE over Flat(w) words.
//!
//! f3's XOR reduce (xor_reduce.rs) with element type lifted from Bit to
//! Type::Flat(w); idx STAYS Bit. memexch uses memexch_prim_idx(&Flat(w), &Bit, 2).
//! Caps at Flat(4) (Flat(8) memexch table = 33.5M entries -- infeasible).
//!
//! T1: emission/parametric pin -- same instruction sequence for w∈{1,2,4}.
//! T2: Flat(2) whole-domain hoist-compiled oracle.
//! T3: Flat(4) whole-domain hoist-compiled oracle.
//! T4: real CPU (#[ignore]).
//! T5: WCET/histogram via rom_trace.
//! T6: canonicalization via single-step run_rom_fast.
//! MINOR-1: debug-assert n<=8 (tested via the cardinality functions).
//! MINOR-3: should_panic for value_to/from_leaf_bytes range.
//! MAJOR-1: negative test for memexch_prim_idx with multi-leaf idx.

use theseus_core::prim::enumerate_values;
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::Instr;

mod common;
use common::prim_builders::xor_prim_iso;
use theseus_tisc::compile::{
    compile_with_metadata_resident, marshal_resident, memexch_prim_idx, run_on_cpu_resident,
    run_rom_fast, unmarshal_resident, value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};

// ── Type helpers ────────────────────────────────────────────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// `(idx:Bit) × (arr0:Flat(w)) × (arr1:Flat(w)) × 1`
fn idx_arr_ty(w: u8) -> Type {
    let flat = Type::Flat(w);
    Type::prod(bit(), Type::prod(flat.clone(), Type::prod(flat, Type::One)))
}

/// `(arr0:Flat(w)) × (arr1:Flat(w)) × 1`
fn arr_ty(w: u8) -> Type {
    let flat = Type::Flat(w);
    Type::prod(flat.clone(), Type::prod(flat, Type::One))
}

/// Body type: `(acc:Flat(w)) × (data:Flat(w)) × (idx:Bit) × (arr0:Flat(w)) × (arr1:Flat(w)) × 1`
fn body_ty(w: u8) -> Type {
    let flat = Type::Flat(w);
    let b = bit();
    Type::prod(
        flat.clone(),
        Type::prod(
            flat.clone(),
            Type::prod(b, Type::prod(flat.clone(), Type::prod(flat, Type::One))),
        ),
    )
}

/// Full carrier type: `Nat_d × body_ty(w)` (head = d+3 leaves with resident=2).
fn carrier_ty(w: u8, d: usize) -> Type {
    Type::prod(Type::numeral_ty(d), body_ty(w))
}

// ── Program construction ────────────────────────────────────────────

/// Build memexch Iso for `(data:Flat(w), idx:Bit, arr[Flat(w);2])`.
fn memexch_iso(w: u8) -> Iso {
    let flat = Type::Flat(w);
    let b = bit();
    let def = memexch_prim_idx(&flat, &b, 2).expect("memexch_prim_idx(Flat(w), Bit, 2)");
    Iso::Prim(Box::new(def))
}

/// `reduceStep : (acc, data) ↦ (acc⊕data, data) = SwapProd ; xor ; SwapProd`
fn reduce_step(w: u8) -> Iso {
    let flat = Type::Flat(w);
    let swap = Iso::SwapProd(flat.clone(), flat.clone());
    Iso::seq(Iso::seq(swap.clone(), xor_prim_iso(w)), swap)
}

/// `succ_idx = notb = SwapSum(One, One)` on Bit (0↔1, → Xori{idx,1}).
fn succ_idx() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// Build the XOR-reduce body for Flat(w) elements, Bit index:
/// ```text
/// BODY : (acc, data, idx, arr) ↔ (acc, data, idx, arr) =
///     (id_acc ⊗ memexch)
///   ; assocL ; (reduceStep ⊗ id_{idx,arr}) ; assocR
///   ; (id_acc ⊗ memexch)
///   ; (id_{acc,data} ⊗ succ_idx ⊗ id_arr)
/// ```
fn xor_reduce_body(w: u8) -> Iso {
    let flat = Type::Flat(w);
    let mexch = memexch_iso(w);

    // (id_acc ⊗ memexch): on (acc, (data, idx, arr))
    let id_acc_mexch = Iso::times(Iso::Id(flat.clone()), mexch.clone());

    // assocL: (acc, (data, rest)) → ((acc, data), rest)
    let assoc_l = Iso::AssocLProd(flat.clone(), flat.clone(), idx_arr_ty(w));

    // (reduceStep ⊗ id_{idx,arr}): on ((acc, data), (idx, arr))
    let rs_times_id = Iso::times(reduce_step(w), Iso::Id(idx_arr_ty(w)));

    // assocR: ((acc, data), rest) → (acc, (data, rest))
    let assoc_r = Iso::AssocRProd(flat.clone(), flat.clone(), idx_arr_ty(w));

    // (id_{acc,data} ⊗ succ_idx ⊗ id_arr): on (acc, (data, (idx, arr)))
    let succ_part = Iso::times(
        Iso::Id(flat.clone()),
        Iso::times(
            Iso::Id(flat.clone()),
            Iso::times(succ_idx(), Iso::Id(arr_ty(w))),
        ),
    );

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

/// Build the full reduce program: `iter Nat_d BODY`.
fn reduce_flat(w: u8, d: usize) -> Iso {
    Iso::iter(Type::numeral_ty(d), xor_reduce_body(w))
}

/// Reference eval for the reduce.
fn eval_reduce(w: u8, d: usize, input: &Value) -> Value {
    let iso = reduce_flat(w, d);
    let mut fuel = 100_000_000u64;
    theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval reduce")
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

/// Build a Flat(w) value from a byte (thermometer encoding).
fn flat_val(byte: u8) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..byte {
        v = Value::right(v);
    }
    v
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
            flat_val(acc),
            Value::pair(
                flat_val(data),
                Value::pair(
                    bit_val(idx),
                    Value::pair(flat_val(arr0), Value::pair(flat_val(arr1), Value::Unit)),
                ),
            ),
        ),
    )
}

/// Extract `(k, acc, data, idx, arr0, arr1)` as bytes from a carrier value.
fn decompose_output(v: &Value, w: u8) -> (usize, u8, u8, u8, u8, u8) {
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
    // body = (acc, (data, (idx, (arr0, (arr1, ())))))
    // marshal via leaf bytes to read the values
    let body_t = body_ty(w);
    let bytes = value_to_leaf_bytes_pub(body.as_ref(), &body_t);
    // bytes = [acc, data, idx_tag, arr0, arr1]
    // idx is a Sum(One,One), which marshals as 1 tag byte
    (k, bytes[0], bytes[1], bytes[2], bytes[3], bytes[4])
}

// ── T1: emission/parametric pin ─────────────────────────────────────

#[test]
fn t1_emission_parametric_pin() {
    // MAJOR-1: pin that the FULL instruction sequence is identical across widths
    // (the parametric argument -- the ROM is width-independent, == f3's Bit ROM),
    // and that WCET==24 for every w (not just w=2 via T5).
    let mut roms: Vec<Vec<Instr>> = Vec::new();
    for w in [1u8, 2, 4] {
        let iso = reduce_flat(w, 3);
        let lowered = compile_with_metadata_resident(&iso, 2)
            .unwrap_or_else(|_| panic!("compile reduce_flat({w}, 3)"));
        assert_eq!(
            lowered.wcet(),
            24,
            "w={w}: WCET must be 24 (f3's clean-regime B)"
        );
        roms.push(lowered.instrs.clone());

        // Assert exactly 4 MexchReg and 2 Xor at d=3.
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
            "w={w}: must have 4 MexchReg, got {mexchr_count}; ROM: {:?}",
            lowered.instrs
        );
        assert_eq!(
            xor_count, 2,
            "w={w}: must have 2 Xor, got {xor_count}; ROM: {:?}",
            lowered.instrs
        );

        // Per-copy body slice: [MexchReg{rd,ra}, Exch{a,d}, Xor{rd:d,rs:a}, Exch{a,d}, MexchReg{rd,ra}, Xori{rd:idx,1}]
        let mexchr_positions: Vec<(usize, u8, u8)> = lowered
            .instrs
            .iter()
            .enumerate()
            .filter_map(|(i, instr)| match instr {
                Instr::MexchReg { rd, ra } => Some((i, *rd, *ra)),
                _ => None,
            })
            .collect();

        for copy_idx in 0..2 {
            let (pos1, rd, ra) = mexchr_positions[copy_idx * 2];
            let (pos2, rd2, ra2) = mexchr_positions[copy_idx * 2 + 1];

            assert_eq!(rd, rd2, "w={w} copy {copy_idx}: MexchReg rd mismatch");
            assert_eq!(ra, ra2, "w={w} copy {copy_idx}: MexchReg ra mismatch");
            assert_ne!(rd, ra, "w={w} copy {copy_idx}: rd must differ from ra");

            // pos2 = pos1 + 4
            assert_eq!(
                pos2,
                pos1 + 4,
                "w={w} copy {copy_idx}: second MexchReg must be at pos1+4"
            );

            let d_r = rd;

            // instrs[pos1+1] = Exch involving d_r
            let a_r = match lowered.instrs[pos1 + 1] {
                Instr::Exch { rd: r1, rs: r2 } => {
                    if r1 == d_r {
                        r2
                    } else {
                        assert_eq!(r2, d_r, "w={w} copy {copy_idx}: Exch must involve d_r");
                        r1
                    }
                }
                ref other => {
                    panic!("w={w} copy {copy_idx}: expected Exch at pos1+1, got {other:?}")
                }
            };

            assert_ne!(
                a_r, ra,
                "w={w} copy {copy_idx}: acc reg must differ from addr reg"
            );
            assert_ne!(
                a_r, d_r,
                "w={w} copy {copy_idx}: acc reg must differ from data reg"
            );

            // instrs[pos1+2] = Xor{rd: d_r, rs: a_r} (F3-1: Xor writes DATA reg)
            assert_eq!(
                lowered.instrs[pos1 + 2],
                Instr::Xor { rd: d_r, rs: a_r },
                "w={w} copy {copy_idx}: F3-1 Xor shape"
            );

            // instrs[pos1+3] = same Exch
            assert_eq!(
                lowered.instrs[pos1 + 3],
                lowered.instrs[pos1 + 1],
                "w={w} copy {copy_idx}: second Exch must match first"
            );

            // After second MexchReg: Xori{rd: ra, 1}
            assert_eq!(
                lowered.instrs[pos2 + 1],
                Instr::Xori { rd: ra, imm: 1 },
                "w={w} copy {copy_idx}: succ must be Xori{{rd={ra}, imm=1}}"
            );
        }
    }
    // Cross-w: the full ROM must be byte-identical across widths (width-independent
    // codegen -- the parametric pin the design/review F3 mandated).
    assert_eq!(
        roms[0], roms[1],
        "reduce ROM must be identical for Flat(1) vs Flat(2)"
    );
    assert_eq!(
        roms[1], roms[2],
        "reduce ROM must be identical for Flat(2) vs Flat(4)"
    );
}

// ── T2: Flat(2) whole-domain, hoist-compiled ────────────────────────

#[test]
fn t2_flat2_whole_domain() {
    let w = 2u8;
    let d = 3usize;
    let iso = reduce_flat(w, d);
    let ty = carrier_ty(w, d);

    // Compile once
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();

    let domain = enumerate_values(&ty);
    // Nat_3 (3 vals) × body: 3 × 4^4 × 2 = 3 × 256 × 2 = 1536
    assert_eq!(domain.len(), 1536, "Flat(2) d=3 domain size");

    for input in &domain {
        // ROM oracle
        let state = marshal_resident(input, &ty, 2);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal_resident(&final_state, &ty, 2);

        // Eval oracle
        let mut fuel = 100_000_000u64;
        let eval_out = theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval");

        assert_eq!(rom_out, eval_out, "Flat(2) rom vs eval mismatch on input");

        // Semantic check: full-word acc_out = acc0 ⊕ (⊕ arr[touched])
        let (k, acc0, data0, idx0, a0, a1) = decompose_output(input, w);
        let (k_out, acc_out, data_out, idx_out, a0_out, a1_out) = decompose_output(&rom_out, w);

        assert_eq!(k_out, k, "counter preserved");
        assert_eq!(data_out, data0, "data preserved");
        assert_eq!(a0_out, a0, "arr[0] preserved");
        assert_eq!(a1_out, a1, "arr[1] preserved");

        // Compute expected acc and idx
        let mut expected_acc = acc0;
        let mut cur_idx = idx0;
        for _ in 0..k {
            let arr_val = if cur_idx == 0 { a0 } else { a1 };
            expected_acc ^= arr_val;
            cur_idx ^= 1;
        }
        assert_eq!(acc_out, expected_acc, "acc_out = acc0 ⊕ (⊕ arr[touched])");
        assert_eq!(idx_out, cur_idx, "idx_out = idx0 ⊕ (k mod 2)");
    }
}

// ── T3: Flat(4) whole-domain, hoist-compiled ────────────────────────

#[test]
fn t3_flat4_whole_domain() {
    let w = 4u8;
    let d = 3usize;
    let iso = reduce_flat(w, d);
    let ty = carrier_ty(w, d);

    // Compile once
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();

    let domain = enumerate_values(&ty);
    // Nat_3 (3 vals) × body: 3 × 16^4 × 2 = 3 × 65536 × 2 = 393216
    assert_eq!(domain.len(), 393_216, "Flat(4) d=3 domain size");

    // Include dense words check
    let dense_inputs: Vec<Value> = vec![
        make_input(2, 0xA, 0x5, 0, 0xF, 0x3),
        make_input(2, 0x5, 0xA, 1, 0xC, 0xF),
        make_input(1, 0xF, 0x0, 0, 0xA, 0x5),
    ];
    for input in &dense_inputs {
        let state = marshal_resident(input, &ty, 2);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal_resident(&final_state, &ty, 2);
        let mut fuel = 100_000_000u64;
        let eval_out = theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval");
        assert_eq!(rom_out, eval_out, "Flat(4) dense word mismatch");
    }

    // Full domain
    for input in &domain {
        let state = marshal_resident(input, &ty, 2);
        let final_state = run_rom_fast(&rom, state, budget);
        let rom_out = unmarshal_resident(&final_state, &ty, 2);

        let mut fuel = 100_000_000u64;
        let eval_out = theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval");

        assert_eq!(rom_out, eval_out, "Flat(4) rom vs eval mismatch");
    }
}

// ── T4: real CPU (#[ignore]) ────────────────────────────────────────

#[test]
#[ignore = "real-CPU system_step; run via --ignored"]
fn t4_real_cpu() {
    let w = 2u8;
    let d = 3usize;
    let iso = reduce_flat(w, d);

    // Sample: k≥1, idx0=1, nonzero words
    let samples = vec![
        make_input(1, 3, 1, 1, 2, 3),
        make_input(2, 2, 0, 1, 1, 3),
        make_input(2, 1, 2, 0, 3, 2),
    ];
    for input in &samples {
        let expected = eval_reduce(w, d, input);
        let got = run_on_cpu_resident(&iso, input, 2, |iso, v, fuel| {
            theseus_eval::eval(iso, v, fuel)
        })
        .expect("run_on_cpu_resident");
        assert_eq!(got, expected, "T4 real CPU mismatch");
    }
}

// ── T5: WCET/histogram via rom_trace ────────────────────────────────

#[test]
fn t5_wcet_histogram() {
    use theseus_tisc::energy::rom_trace;

    let w = 2u8;
    let d = 3usize;
    let iso = reduce_flat(w, d);
    let ty = carrier_ty(w, d);
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");

    let b = lowered.wcet();
    assert_eq!(b, 24, "B (WCET) must be 24 for reduce d=3, got {b}");

    // T(k) = 12 + 6k
    let expected_t = |k: usize| -> usize { 12 + 6 * k };
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
    assert!(
        (park_taxes[0] - 0.5).abs() < 1e-10,
        "park_tax(0) must be 50%"
    );
    assert!(
        (park_taxes[1] - 0.25).abs() < 1e-10,
        "park_tax(1) must be 25%"
    );
    assert!(park_taxes[2].abs() < 1e-10, "park_tax(2) must be 0%");
}

// ── T6: canonicalization -- register bytes stay < 2^w each step ──────

#[test]
fn t6_canonicalization_flat2() {
    let w = 2u8;
    let d = 3usize;
    let iso = reduce_flat(w, d);
    let ty = carrier_ty(w, d);
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();
    let mask = (1u8 << w) - 1; // 0x03 for Flat(2)

    let domain = enumerate_values(&ty);

    for input in &domain {
        let mut state = marshal_resident(input, &ty, 2);
        // Single-step loop
        for _step in 0..budget {
            state = run_rom_fast(&rom, state, 1);
            // Canonicalization (MAJOR-2): every VALUE byte must stay < 2^w each step.
            // The value leaves live in regs 0..6 (Nat_3 counter 0-2, acc, data, idx --
            // all thermometer tags) and the resident Flat(w) words in mem[0..2] -- the
            // latter is MexchReg's byte-wide surface and the whole point of this check
            // (marshal_resident writes resident leaves to state.mem, NOT scratch, so the
            // prior scratch[0..2] check was vacuous). regs >= 6 are iter's internal
            // skip-block aux (e.g. the body-length jump offset) -- non-canonical BY DESIGN,
            // not value bytes, so they are deliberately excluded.
            for i in 0..6 {
                assert!(
                    state.regs[i] <= mask,
                    "step canon: reg[{i}]={} > mask={mask}",
                    state.regs[i]
                );
            }
            for i in 0..2 {
                assert!(
                    state.mem[i] <= mask,
                    "step canon: mem[{i}]={} > mask={mask} (resident Flat word dirtied)",
                    state.mem[i]
                );
            }
        }
    }
}

#[test]
#[ignore = "real-CPU single-step canonicalization"]
fn t6_canonicalization_real_cpu() {
    // Placeholder for a real-CPU stepping tier at Flat(2).
    let w = 2u8;
    let d = 3usize;
    let iso = reduce_flat(w, d);

    let samples = vec![make_input(2, 3, 1, 1, 2, 3), make_input(1, 1, 0, 0, 2, 1)];
    for input in &samples {
        let expected = eval_reduce(w, d, input);
        let got = run_on_cpu_resident(&iso, input, 2, |iso, v, fuel| {
            theseus_eval::eval(iso, v, fuel)
        })
        .expect("run_on_cpu_resident");
        assert_eq!(got, expected, "T6 real CPU canon mismatch");
    }
}

// ── MAJOR-1: negative test -- multi-leaf idx rejected ────────────────

#[test]
fn major1_memexch_prim_idx_multi_leaf_idx_rejected() {
    let bit_ty = Type::sum(Type::One, Type::One);
    let prod_idx = Type::prod(bit_ty.clone(), bit_ty.clone());
    let result = memexch_prim_idx(&bit_ty, &prod_idx, 4);
    assert!(
        result.is_err(),
        "memexch_prim_idx(&Bit, &Prod(Bit,Bit), 4) must return Err, got Ok"
    );
}

// ── MINOR-3: should_panic for value_to/from_leaf_bytes range ────────

#[test]
#[should_panic(expected = "out of range")]
fn minor3_value_to_leaf_bytes_out_of_range() {
    // Flat(2) has values 0..3; feed it byte value 4 (thermo(4) = Right^4(Left(Unit)))
    let v = flat_val(4);
    let ty = Type::Flat(2);
    value_to_leaf_bytes_pub(&v, &ty);
}

#[test]
#[should_panic(expected = "out of range")]
fn minor3_value_from_leaf_bytes_out_of_range() {
    // Flat(2) allows bytes 0..3; feed byte 4
    let ty = Type::Flat(2);
    value_from_leaf_bytes_pub(&[4], &ty);
}
