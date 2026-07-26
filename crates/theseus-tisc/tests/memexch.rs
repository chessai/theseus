#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! MEXCHR compiler emission tests (M-D.b2.f1).
//!
//! Proves: a `memexch` primitive (Bit-indexed, 2-word resident array)
//! compiles to a ROM containing `MexchReg`, and the run-on-CPU oracle
//! matches eval over the whole 16-input domain. Each reject test
//! corresponds to a design finding (F-2, F-3, F-4, F-6, F-7, F-8).
//!
//! OOM discipline: tests lowering `system_step` are `#[ignore]` + serialized.

use std::collections::BTreeMap;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    compile_with_metadata_resident, marshal_resident, memexch_prim, run_on_cpu_fast_resident,
    run_on_cpu_resident, unmarshal_resident, CompileError,
};
use theseus_tisc::types::{StateVals, MEM_WORDS};

// ── Helpers ──────────────────────────────────────────────────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// Build the memexch Iso for elem_ty=Bit, n=2.
fn memexch_iso() -> Iso {
    let def = memexch_prim(&bit(), 2).expect("memexch_prim(Bit,2)");
    Iso::Prim(Box::new(def))
}

/// The full carrier type: Bit × (Bit × (Bit × (Bit × 1)))
/// = data × addr × arr[0] × arr[1]  (4 leaves)
fn memexch_ty() -> Type {
    let b = bit();
    Type::prod(
        b.clone(),
        Type::prod(
            b.clone(),
            Type::prod(b.clone(), Type::prod(b.clone(), Type::One)),
        ),
    )
}

/// Enumerate the full 16-value domain.
fn full_domain() -> Vec<Value> {
    enumerate_values(&memexch_ty())
}

/// Reference eval for memexch.
fn eval_memexch(input: &Value) -> Value {
    let iso = memexch_iso();
    let mut fuel = 1_000_000u64;
    theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval memexch")
}

// ── T1: compile_with_metadata_resident yields MexchReg ───────────────

#[test]
fn t1_emission_contains_mexch_reg() {
    let iso = memexch_iso();
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile memexch");
    let has_mexchr = lowered
        .instrs
        .iter()
        .any(|i| matches!(i, Instr::MexchReg { .. }));
    assert!(
        has_mexchr,
        "compiled memexch ROM must contain MexchReg, got: {:?}",
        lowered.instrs
    );
}

// ── T2: fast oracle == eval over whole 16-input domain ───────────────

#[test]
fn t2_fast_oracle_whole_domain() {
    let iso = memexch_iso();
    for input in full_domain() {
        let expected = eval_memexch(&input);
        let got = run_on_cpu_fast_resident(&iso, &input, 2).expect("run_on_cpu_fast_resident");
        assert_eq!(got, expected, "fast oracle mismatch for input {input:?}");
    }
}

// ── T3: real CPU oracle (system_step) — #[ignore], serialized ────────

#[test]
#[ignore]
fn t3_real_cpu_oracle() {
    let iso = memexch_iso();
    // I-2: sample inputs that INCLUDE addr=1 (not just the first 4 which
    // are all addr=0 due to left-factor-major enumeration). Take the first
    // 2 (addr=0) and last 2 (addr=1) to cover both address values.
    let domain = full_domain();
    let samples: Vec<_> = domain
        .iter()
        .take(2)
        .chain(domain.iter().rev().take(2))
        .cloned()
        .collect();
    for input in &samples {
        let expected = eval_memexch(input);
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

// ── T4: compiled ROM MexchReg closes on SystemRing — #[ignore] ───────

#[test]
#[ignore]
fn t4_system_ring_closure() {
    use theseus_tisc::ring_run::system_ring;
    use theseus_tisc::types::{decode_system, encode_system};

    let iso = memexch_iso();
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");
    let ring = system_ring(&rom);

    // I-2: use a sample with addr=1 (data != arr[1]) so the MexchReg
    // actually exchanges — not just the all-zeros no-op.
    // Pick the LAST value in the domain (addr=1, data=1, arr=[1,1]... the
    // highest thermometer encoding). Fall back to last if domain is small.
    let domain = full_domain();
    let sample = domain.last().unwrap().clone();
    let init = marshal_resident(&sample, &memexch_ty(), 2);
    assert!(
        ring.bus_quiescent(&init),
        "SystemRing must be bus-quiescent"
    );

    // Check run == eval^R on the sample input.
    let sys_val = encode_system(&init);

    let mut fuel = 8_000_000_000u64;
    let step_iso = theseus_tisc::step::system_step(&rom);
    let mut cur = sys_val.clone();
    let budget = lowered.wcet();
    for _ in 0..budget {
        cur = theseus_eval::eval(&step_iso, cur, &mut fuel).expect("step eval");
    }
    let final_state = decode_system(&cur);
    let out = unmarshal_resident(&final_state, &memexch_ty(), 2);
    let expected = eval_memexch(&sample);
    assert_eq!(out, expected, "ring closure oracle mismatch");
}

// ── T5-F2a: Times(memexch, id_Bit) misaligned → ResidentMisplaced ────

#[test]
fn t5_f2a_misaligned_resident_rejected() {
    // Times(memexch, id_Bit): memexch's arr leaves are at positions 2,3
    // in the left sub-context, but the full context has 5 leaves total
    // (4 from memexch + 1 from id_Bit). With resident=2, the LAST 2 leaves
    // (positions 3,4) go to Mem — but memexch expects its arr (positions 2,3
    // within its sub-context) at Mem(0),Mem(1). This misaligns.
    let memexch = memexch_iso();
    let id_bit = Iso::Id(bit());
    let composed = Iso::Times(Box::new(memexch), Box::new(id_bit));
    // resident=2 means last 2 of 5 leaves → Mem(0),Mem(1) = positions 3,4.
    // But memexch's arr is at sub-positions 2,3 (globally 2,3) which are NOT in Mem.
    let result = compile_with_metadata_resident(&composed, 2);
    assert!(
        matches!(result, Err(CompileError::ResidentMisplaced(_))),
        "expected ResidentMisplaced, got {result:?}"
    );
}

// ── T5-F2b: Times(id_Bit, memexch) with arr globally trailing → OK ───

#[test]
fn t5_f2b_aligned_times_compiles() {
    // Times(id_Bit, memexch): id_Bit is leaf 0 (head), memexch is leaves 1..5.
    // With resident=2, last 2 of 5 leaves (positions 3,4) → Mem(0),Mem(1).
    // memexch's sub-context: leaves 1..5 → sub-positions 0,1,2,3.
    // The arr leaves are sub-positions 2,3 = global 3,4 → Mem(0),Mem(1). Correct!
    let id_bit = Iso::Id(bit());
    let memexch = memexch_iso();
    let composed = Iso::Times(Box::new(id_bit), Box::new(memexch));
    let _lowered = compile_with_metadata_resident(&composed, 2)
        .expect("Times(id_Bit, memexch) should compile");
    // Whole-domain oracle check.
    let full_ty = Type::prod(bit(), memexch_ty());
    for input in enumerate_values(&full_ty) {
        let expected = {
            let mut fuel = 1_000_000u64;
            theseus_eval::eval(&composed, input.clone(), &mut fuel).expect("eval")
        };
        let got = run_on_cpu_fast_resident(&composed, &input, 2).expect("fast oracle");
        assert_eq!(got, expected, "T5-F2b oracle mismatch for {input:?}");
    }
}

// ── T5-F3: forged "memexch" table → UnrecognizedPrim ─────────────────

#[test]
fn t5_f3_forged_table_rejected() {
    // Build a PrimDef named "memexch" but with a wrong table:
    // swaps at (a+1)%n instead of a.
    let _b = bit();
    let full_ty = memexch_ty();
    let vals = enumerate_values(&full_ty);
    let n = 2usize;

    let mut forward = BTreeMap::new();
    for v in &vals {
        let bytes = theseus_tisc::compile::value_to_leaf_bytes_pub(v, &full_ty);
        let d = bytes[0];
        let a = bytes[1];
        let idx = ((a as usize) + 1) % n; // WRONG: (a+1)%n instead of a
        let mut out_bytes = bytes.clone();
        out_bytes[0] = bytes[2 + idx];
        out_bytes[2 + idx] = d;
        let (out_val, _) = theseus_tisc::compile::value_from_leaf_bytes_pub(&out_bytes, &full_ty);
        forward.insert(v.clone(), out_val);
    }
    let backward = forward.clone(); // involution-ish (doesn't matter for this test)

    let def = PrimDef {
        forward_name: "memexch".into(),
        backward_name: "memexch".into(),
        input_ty: full_ty.clone(),
        output_ty: full_ty,
        forward,
        backward,
        symbolic: None,
    };
    let iso = Iso::Prim(Box::new(def));
    let result = compile_with_metadata_resident(&iso, 2);
    assert!(
        matches!(result, Err(CompileError::UnrecognizedPrim(_))),
        "expected UnrecognizedPrim for forged table, got {result:?}"
    );
}

// ── T5-F4: memexch_prim with over-wide index → Err ──────────────────

#[test]
fn t5_f4_overwide_index_rejected() {
    // 3-value index type: Bit has 2 values, so asking for n=1 with Bit index
    // means idx_count=2 > n=1 → Err.
    let result = memexch_prim(&bit(), 1);
    assert!(
        result.is_err(),
        "memexch_prim(Bit, 1) should fail (2 > 1), got Ok"
    );
}

// ── T6: marshal_resident↔Ctx home property ───────────────────────────
// I-8: The marshal↔Ctx-home property is satisfied BY CONSTRUCTION: both
// marshal_resident and Ctx::new_resident call home_of (per I-3, the sole
// placement function). This roundtrip test confirms the property holds.

#[test]
fn t6_marshal_unmarshal_roundtrip() {
    let ty = memexch_ty();
    let resident = 2usize;
    for input in full_domain() {
        let state = marshal_resident(&input, &ty, resident);
        let recovered = unmarshal_resident(&state, &ty, resident);
        assert_eq!(
            recovered, input,
            "marshal/unmarshal roundtrip failed for {input:?}"
        );
    }
}

// ── T5-F6a: head>14 bounds reject (NOT an occ test — see I-1 below) ──

#[test]
fn t5_f6a_head_exceeds_14_rejected() {
    // Build a program with resident=MEM_WORDS (=4) and enough head leaves
    // to force spill exhaustion. Head leaves > 14 → TooManyLeaves.
    // resident=MEM_WORDS means head leaves = n - MEM_WORDS.
    // If n = MEM_WORDS + 15 = 19, head = 15 > 14 → reject.
    // We just need to check the bounds reject.
    // Actually: resident=MEM_WORDS=4, and if head > 14 we get TooManyLeaves.
    // Build a trivial iso with many leaves: a product of 15 Bits + 4 resident.
    // Total = 19 leaves, head = 15 > 14.
    let mut ty = Type::One;
    for _ in 0..19 {
        ty = Type::prod(bit(), ty);
    }
    let iso = Iso::Id(ty);
    let result = compile_with_metadata_resident(&iso, MEM_WORDS);
    assert!(
        matches!(result, Err(CompileError::TooManyLeaves { .. })),
        "expected TooManyLeaves for exhausted layout, got {result:?}"
    );
}

// ── T5-F6b: resident=2 + displacement disjoint from resident words ───

#[test]
fn t5_f6b_displacement_disjoint() {
    // A program with resident=2 that forces a mem displacement (spill).
    // Need > 8 head leaves to require scratch, then > 14 head to require mem.
    // Actually: with resident=2, head leaves > 8 get scratch. A displacement
    // Mexch is emitted when head leaves require paging from mem.
    // Simpler test: the memexch itself already uses resident=2 and if we pad
    // it with enough head leaves to force one auxiliary displacement, the
    // displacement addr must be >= 2.
    //
    // Build: Times(big_id, memexch) where big_id has 8 leaves (fills regs),
    // so memexch's data/addr go to scratch, paging will emit Xchg bracket.
    // The resident words are at Mem(0),Mem(1); any spill displacement must
    // target Mem(2) or Mem(3) (disjoint).
    //
    // For T5-F6b we just need to verify the oracle passes and any Mexch in
    // the ROM uses addr >= 2.
    let mut big_ty = Type::One;
    for _ in 0..8 {
        big_ty = Type::prod(bit(), big_ty);
    }
    let big_id = Iso::Id(big_ty.clone());
    let memexch = memexch_iso();
    let composed = Iso::Times(Box::new(big_id), Box::new(memexch));
    // Total leaves = 8 + 4 = 12; resident=2 → head=10, which fits (≤14).
    let lowered = compile_with_metadata_resident(&composed, 2).expect("compile with displacement");

    // Check any immediate Mexch has addr >= 2 (disjoint from resident).
    for instr in &lowered.instrs {
        if let Instr::Mexch { rd: _, addr } = instr {
            assert!(
                *addr >= 2,
                "Mexch displacement addr={} < 2, overlaps resident words",
                addr
            );
        }
    }

    // I-2: Oracle check on samples that INCLUDE addr=1 (the memexch
    // portion is the rightmost factor; take first 2 + last 2 to cover
    // both addr values since left-factor-major cycles the rightmost fastest).
    let full_ty = Type::prod(big_ty, memexch_ty());
    let all_vals = enumerate_values(&full_ty);
    let sample: Vec<_> = all_vals
        .iter()
        .take(2)
        .chain(all_vals.iter().rev().take(2))
        .cloned()
        .collect();
    for input in &sample {
        let expected = {
            let mut fuel = 10_000_000u64;
            theseus_eval::eval(&composed, input.clone(), &mut fuel).expect("eval")
        };
        let got = run_on_cpu_fast_resident(&composed, input, 2).expect("fast oracle");
        assert_eq!(got, expected, "T5-F6b oracle mismatch");
    }
}

// ── T5-F7: junk-padded carrier (≥8 head leaves) → paging bracket ─────

#[test]
fn t5_f7_paging_bracket() {
    // With 8 head leaves, data/addr are in scratch → paging emits Xchg.
    // Build: Times(pad_8, memexch) with resident=2.
    // pad_8 = id on 8 Bit leaves.
    let mut pad_ty = Type::One;
    for _ in 0..8 {
        pad_ty = Type::prod(bit(), pad_ty);
    }
    let pad_id = Iso::Id(pad_ty.clone());
    let memexch = memexch_iso();
    let composed = Iso::Times(Box::new(pad_id), Box::new(memexch));

    let lowered = compile_with_metadata_resident(&composed, 2).expect("compile padded");

    // Must contain MexchReg AND Xchg bracket around it.
    let has_mexchr = lowered
        .instrs
        .iter()
        .any(|i| matches!(i, Instr::MexchReg { .. }));
    let has_xchg = lowered
        .instrs
        .iter()
        .any(|i| matches!(i, Instr::Xchg { .. }));
    assert!(has_mexchr, "padded ROM must contain MexchReg");
    assert!(has_xchg, "padded ROM must contain Xchg bracket for paging");

    // I-2: Sampled oracle — include addr=1 (take first 2 + last 2).
    let full_ty = Type::prod(pad_ty, memexch_ty());
    let all_vals = enumerate_values(&full_ty);
    let sample: Vec<_> = all_vals
        .iter()
        .take(2)
        .chain(all_vals.iter().rev().take(2))
        .cloned()
        .collect();
    for input in &sample {
        let expected = {
            let mut fuel = 10_000_000u64;
            theseus_eval::eval(&composed, input.clone(), &mut fuel).expect("eval")
        };
        let got = run_on_cpu_fast_resident(&composed, input, 2).expect("fast oracle");
        assert_eq!(got, expected, "T5-F7 oracle mismatch");
    }
}

// ── T5-F8: Sym(memexch) compiles to same MexchReg; oracle passes ─────

#[test]
fn t5_f8_sym_memexch() {
    let memexch = memexch_iso();
    let sym_memexch = Iso::Sym(Box::new(memexch.clone()));

    let lowered = compile_with_metadata_resident(&sym_memexch, 2).expect("compile Sym(memexch)");
    let has_mexchr = lowered
        .instrs
        .iter()
        .any(|i| matches!(i, Instr::MexchReg { .. }));
    assert!(has_mexchr, "Sym(memexch) ROM must contain MexchReg");

    // Whole-domain oracle.
    for input in full_domain() {
        let expected = {
            let mut fuel = 1_000_000u64;
            theseus_eval::eval(&sym_memexch, input.clone(), &mut fuel).expect("eval")
        };
        let got = run_on_cpu_fast_resident(&sym_memexch, &input, 2).expect("fast oracle");
        assert_eq!(got, expected, "T5-F8 Sym oracle mismatch for {input:?}");
    }
}

// ── T7: rom_program_energy_resident prices MEXCHR-bearing ROM > 0 ────
//
// Rerouted through `rom_program_energy_resident` (F2-2) to resolve the
// I-5 tautology: cross-check total_j from the API vs steps_b × step_j.

#[test]
#[ignore]
fn t7_energy_prices_mexchr_rom() {
    use theseus_spice::chardb::rung0_measured;
    use theseus_tisc::energy::{balanced_energy, rom_program_energy_resident};

    let iso = memexch_iso();
    let input = full_domain().into_iter().next().unwrap();
    let db = rung0_measured();
    let t_ramp = 1e-7;

    // Compile with resident=2 and get the ROM + step net.
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
    let budget = lowered.wcet();
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).expect("assemble");

    let step_iso = theseus_tisc::step::step(&rom);
    let net = theseus_netlist::lower_iso(&step_iso).expect("lower_iso");

    // Path 1: rom_program_energy_resident API (the new entry point).
    let pe = rom_program_energy_resident(&iso, &input, 2, &db, t_ramp, &net)
        .expect("rom_program_energy_resident");
    assert!(pe.total_j > 0.0, "total energy must be > 0");
    assert!(pe.step_j > 0.0, "step energy must be > 0");

    // Path 2: manual balanced_energy × budget (independent calculation).
    let step_bd = balanced_energy(&net, &db, t_ramp).expect("balanced_energy");
    let step_j = step_bd.total_j;
    let total_j_manual = budget as f64 * step_j;

    // Cross-check: total_j from API == steps_b × step_j (exact).
    let api_total = pe.steps_b as f64 * pe.step_j;
    assert!(
        (pe.total_j - api_total).abs() < 1e-30,
        "API total_j must equal steps_b * step_j exactly"
    );

    // Cross-check: API total matches manual calculation.
    let rel_err = ((pe.total_j - total_j_manual) / total_j_manual).abs();
    assert!(
        rel_err < 1e-15,
        "total_j from API ({:.6e}) != budget*step_j ({:.6e}), rel_err={:.2e}",
        pe.total_j,
        total_j_manual,
        rel_err
    );

    // Straight-line: B == T (no loops, park tax 0).
    assert_eq!(pe.steps_b, budget, "steps_b must equal static wcet budget");
    assert_eq!(
        pe.steps_b, pe.steps_t,
        "memexch is straight-line: B must equal T"
    );
    assert!(pe.park_tax.abs() < 1e-15, "memexch park_tax must be 0");
}

// ── I-1: occ obligation test — occ seed is load-bearing at head=14, resident=2 ─
//
// With head=14 (all 8 regs + 6 scratch occupied) and resident=2, the branching
// `Plus` forces `allocate_aux` to grab an aux zero-source. WITHOUT the occ seed,
// `allocate_aux` believes every location is free and grabs an OCCUPIED slot.
//
// ADVISOR TEETH-CHECK (verified, L20): neutering the occ seed
// (`OccSet::from_locations(&homes[..0])` in `Ctx::new_resident`) makes THIS test
// FAIL — but via *register aliasing* (`allocate_aux` reuses the occupied `r0` →
// `assemble` rejects `CSWAP r0,r1,r0: operands must be pairwise distinct`), which
// fires BEFORE any memory displacement. So this test proves the occ seed is
// *necessary*; F-6's specific "a mem spill never lands on a resident word"
// sub-case (register aliasing dominates first at this size) is a coverage
// refinement deferred to f2. The `Mexch.addr >= 2` assertion below is therefore a
// belt-and-braces guard (it holds; it is not the failing edge here).
//
// Construction: Times(Plus(swap_on_sum_14, _), Id(arr_2))
//   - sum_14 = Sum(payload_13, payload_13): 1 tag + 13 payload = 14 head leaves
//   - Plus with one non-identity arm forces allocate_aux
//   - Id(arr_2) = the 2 resident leaves at Mem(0),Mem(1)

#[test]
fn i1_occ_obligation_displacement_disjoint() {
    // Build a 13-leaf payload type: product of 13 Bits.
    let mut payload_ty = Type::One;
    for _ in 0..13 {
        payload_ty = Type::prod(bit(), payload_ty);
    }

    // Sum type: 1 tag + 13 payload = 14 head leaves.
    let sum_ty = Type::sum(payload_ty.clone(), payload_ty.clone());

    // Build a non-identity arm for the Left branch: SwapProd swaps the
    // first two Bit fields (so is_structural_identity returns false).
    // The payload is Bit × (Bit × rest) so SwapProd(Bit, Bit × rest)
    // would swap at the top level — but we need it to be on the payload only.
    // Use a simple Seq(SwapProd(Bit, remaining), SwapProd(Bit, remaining)) which
    // composes to identity semantically but is NOT recognized as structural id.
    // Actually simpler: just use SwapProd on the first two factors.
    let rest_ty = {
        let mut t = Type::One;
        for _ in 0..12 {
            t = Type::prod(bit(), t);
        }
        t
    };
    let swap_arm = Iso::SwapProd(bit(), rest_ty);
    // g arm: identity on the Right payload.
    let id_arm = Iso::Id(payload_ty.clone());
    let plus_iso = Iso::Plus(Box::new(swap_arm), Box::new(id_arm));

    // Arr type: 2 Bit leaves (the resident portion).
    let arr_ty = Type::prod(bit(), Type::prod(bit(), Type::One));
    let id_arr = Iso::Id(arr_ty.clone());

    // Compose: Times(plus_on_sum14, id_arr) → 14 + 2 = 16 leaves.
    let composed = Iso::Times(Box::new(plus_iso), Box::new(id_arr));

    // Compile with resident=2 (head=14).
    let lowered =
        compile_with_metadata_resident(&composed, 2).expect("compile branching + resident=2");

    // (a) Any immediate Mexch displacement must have addr >= 2.
    for instr in &lowered.instrs {
        if let Instr::Mexch { rd: _, addr } = instr {
            assert!(
                *addr >= 2,
                "I-1: Mexch displacement addr={} < 2, would clobber resident word",
                addr
            );
        }
    }

    // (b) Whole-domain oracle (on a sample including both Left and Right inputs).
    let full_ty = Type::prod(sum_ty, arr_ty);
    let all_vals = enumerate_values(&full_ty);
    // Sample first 4 + last 4 to cover both sum arms.
    let sample: Vec<_> = all_vals
        .iter()
        .take(4)
        .chain(all_vals.iter().rev().take(4))
        .cloned()
        .collect();
    for input in &sample {
        let expected = {
            let mut fuel = 10_000_000u64;
            theseus_eval::eval(&composed, input.clone(), &mut fuel).expect("eval")
        };
        let got = run_on_cpu_fast_resident(&composed, input, 2).expect("fast oracle");
        assert_eq!(got, expected, "I-1 oracle mismatch for {input:?}");
    }
}

// ── I-4: marshal_resident panics on head>14 with resident>0 ─────────────

#[test]
#[should_panic(expected = "head leaves")]
fn i4_marshal_resident_panics_on_aliasing_layout() {
    // 17-leaf type with resident=2: head = 15 > 14 → would alias.
    // Today without the guard this round-trip-corrupts; with the guard it panics.
    let mut ty = Type::One;
    for _ in 0..17 {
        ty = Type::prod(bit(), ty);
    }
    let val = enumerate_values(&ty).into_iter().next().unwrap();
    // This must panic (the failing-without-fix witness).
    let _ = marshal_resident(&val, &ty, 2);
}

#[test]
#[should_panic(expected = "head leaves")]
fn i4_unmarshal_resident_panics_on_aliasing_layout() {
    let mut ty = Type::One;
    for _ in 0..17 {
        ty = Type::prod(bit(), ty);
    }
    let state = StateVals {
        regs: [0; 8],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    // This must panic.
    let _ = unmarshal_resident(&state, &ty, 2);
}

// ── I-6: memexch_prim rejects multi-leaf elem_ty ────────────────────────

#[test]
fn i6_memexch_prim_rejects_multi_leaf_elem() {
    // A 2-leaf elem_ty (Bit × Bit) must be rejected.
    let two_leaf = Type::prod(bit(), Type::prod(bit(), Type::One));
    let result = memexch_prim(&two_leaf, 2);
    assert!(
        result.is_err(),
        "memexch_prim with 2-leaf elem_ty should be rejected, got Ok"
    );
}

#[test]
fn i6_memexch_prim_rejects_n_over_mem_words() {
    // n > MEM_WORDS must be rejected.
    let result = memexch_prim(&bit(), MEM_WORDS + 1);
    assert!(
        result.is_err(),
        "memexch_prim with n={} > MEM_WORDS should be rejected, got Ok",
        MEM_WORDS + 1
    );
}

// ── I-7: compile_with_metadata_resident rejects resident > n ────────────

#[test]
fn i7_resident_greater_than_n_rejected() {
    // Iso::Id(Bit) has 1 leaf; resident=2 > 1 → reject (not underflow-panic).
    let iso = Iso::Id(bit());
    let result = compile_with_metadata_resident(&iso, 2);
    assert!(
        matches!(result, Err(CompileError::TooManyLeaves { .. })),
        "expected TooManyLeaves for resident > n, got {result:?}"
    );
}
