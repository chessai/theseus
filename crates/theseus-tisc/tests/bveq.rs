//! Tests for the `bveq` symbolic bit-vector equivalence module.
//!
//! T1–T8 cover the full trust-core surface: equiv correctness, the Ccnot
//! wall-break, I4 must-reject, shared-mapping discipline, aliasing,
//! tri-model reconciliation (L-a full-domain, L-b netlist-sampled),
//! and the perf POC.
//!
//! MAJOR-1..5 regression tests: each reproduces a demonstrated break from
//! the adversarial impl-review and asserts the fix holds.

use theseus_tisc::asm::Instr;
use theseus_tisc::bveq::{
    self, biodivine_lib_bdd, BvExpr, BveqError, RegTransfer, BITS,
};
use theseus_tisc::compile::{exec_instr, run_rom_on_cpu};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

fn state_with_regs(bytes: &[u8]) -> StateVals {
    let mut r = [0u8; NUM_REGS];
    r[..bytes.len()].copy_from_slice(bytes);
    StateVals {
        regs: r,
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    }
}

// ── T1: equiv correctness ───────────────────────────────────────────

#[test]
fn bveq_equiv_correctness_xor_equal() {
    let model = bveq::instr_semantics(&Instr::Xor { rd: 0, rs: 1 }).unwrap();
    let spec = bveq::instr_semantics(&Instr::Xor { rd: 0, rs: 1 }).unwrap();
    bveq::equiv(&model, &spec, 8, &[0, 1]).expect("XOR must equal itself");
}

#[test]
fn bveq_equiv_correctness_add_equal() {
    let model = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();
    let spec = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();
    bveq::equiv(&model, &spec, 8, &[0, 1]).expect("ADD must equal itself");
}

#[test]
fn bveq_equiv_correctness_wrong_candidate() {
    let model = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();
    let wrong_spec = bveq::instr_semantics(&Instr::Xor { rd: 0, rs: 1 }).unwrap();
    let err = bveq::equiv(&model, &wrong_spec, 8, &[0, 1])
        .unwrap_err()
        .unwrap_counterexample();

    let regs = err.operand_values;
    let model_out = bveq::eval_transfer(&model, &regs);
    let spec_out = bveq::eval_transfer(&wrong_spec, &regs);
    assert_ne!(
        model_out, spec_out,
        "counterexample must produce different outputs: regs={regs:?}"
    );
}

// ── T2: the wall-break (Ccnot 3-operand full Flat(8)) ──────────────

#[test]
fn bveq_wall_break_ccnot_flat8() {
    // MINOR-3: non-reflexive — hand-built `rd ^= rs & rt` spec compared
    // against instr_semantics. Previously both sides were the same call.
    let t = std::time::Instant::now();
    let model = bveq::instr_semantics(&Instr::Ccnot { rd: 0, rs: 1, rt: 2 }).unwrap();

    // Hand-built spec: rd ^= rs & rt (byte-parallel Toffoli).
    let mut spec = RegTransfer::identity();
    for bit in 0..BITS {
        let rd_bit = BvExpr::var(0, bit);
        let rs_bit = BvExpr::var(1, bit);
        let rt_bit = BvExpr::var(2, bit);
        spec.regs[0][bit] = rd_bit.xor(rs_bit.and(rt_bit));
    }

    bveq::equiv(&model, &spec, 8, &[0, 1, 2])
        .expect("Ccnot must equal hand-built rd ^= rs & rt at full Flat(8)");
    let elapsed = t.elapsed();
    eprintln!(
        "T2 wall-break: Ccnot Flat(8) equiv in {:.1} µs (POC: 11 µs)",
        elapsed.as_secs_f64() * 1e6
    );
}

// ── T3: I4 must-reject (byte-Add at Flat(4)) ───────────────────────

#[test]
fn bveq_i4_must_reject_add_flat4() {
    let instr_model = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();

    // Build a Flat(4) add spec: low 4 bits = mod-16 add, upper 4 bits = 0.
    let mut spec = RegTransfer::identity();
    {
        let mut carry = BvExpr::Const(false);
        for i in 0..4 {
            let a = BvExpr::var(0, i);
            let b = BvExpr::var(1, i);
            let sum = a.clone().xor(b.clone()).xor(carry.clone());
            let new_carry = a.clone().and(b.clone()).or(carry.and(a.xor(b)));
            spec.regs[0][i] = sum;
            carry = new_carry;
        }
        for i in 4..BITS {
            spec.regs[0][i] = BvExpr::Const(false);
        }
    }

    let result = bveq::equiv(&instr_model, &spec, 4, &[0, 1]);
    assert!(
        result.is_err(),
        "I4: byte-Add MUST be rejected at Flat(4) — upper output bits leak carry"
    );

    let cex = result.unwrap_err().unwrap_counterexample();
    let regs = cex.operand_values;
    let model_out = bveq::eval_transfer(&instr_model, &regs);
    let upper_bits = model_out[0] >> 4;
    assert_ne!(
        upper_bits, 0,
        "counterexample must show carry leak: a={}, b={}, result={}",
        regs[0], regs[1], model_out[0]
    );
}

// ── MAJOR-1 regression: I4 swapped-argument escape ──────────────────

#[test]
fn bveq_major1_i4_argument_swap_rejection() {
    // MAJOR-1 regression (L20): the review demonstrated that swapping
    // equiv's arguments defeats the I4 check when it only examines side `a`.
    // Fix: I4 checks BOTH sides. This test asserts rejection under BOTH
    // argument orders.
    let instr_model = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();

    // Flat(4) addInto spec (clean upper bits).
    let mut spec = RegTransfer::identity();
    {
        let mut carry = BvExpr::Const(false);
        for i in 0..4 {
            let a = BvExpr::var(0, i);
            let b = BvExpr::var(1, i);
            let sum = a.clone().xor(b.clone()).xor(carry.clone());
            let new_carry = a.clone().and(b.clone()).or(carry.and(a.xor(b)));
            spec.regs[0][i] = sum;
            carry = new_carry;
        }
        for i in 4..BITS {
            spec.regs[0][i] = BvExpr::Const(false);
        }
    }

    // Original order: model first (leaky upper bits on a-side) → must reject.
    assert!(
        bveq::equiv(&instr_model, &spec, 4, &[0, 1]).is_err(),
        "MAJOR-1: equiv(model, spec, 4) must reject (carry leak in model's upper bits)"
    );

    // SWAPPED order: spec first (leaky upper bits now on b-side) → must ALSO reject.
    // Before fix: this returned Ok (I4 only checked a-side).
    assert!(
        bveq::equiv(&spec, &instr_model, 4, &[0, 1]).is_err(),
        "MAJOR-1: equiv(spec, model, 4) must ALSO reject (I4 must check BOTH sides)"
    );

    // Also reject a spec with junk upper bits.
    let mut junk_spec = RegTransfer::identity();
    {
        let mut carry = BvExpr::Const(false);
        for i in 0..4 {
            let a = BvExpr::var(0, i);
            let b = BvExpr::var(1, i);
            let sum = a.clone().xor(b.clone()).xor(carry.clone());
            let new_carry = a.clone().and(b.clone()).or(carry.and(a.xor(b)));
            junk_spec.regs[0][i] = sum;
            carry = new_carry;
        }
        // Junk in upper bits instead of constant-false.
        for i in 4..BITS {
            junk_spec.regs[0][i] = BvExpr::var(0, i);
        }
    }
    assert!(
        bveq::equiv(&junk_spec, &junk_spec, 4, &[0, 1]).is_err(),
        "MAJOR-1: spec with junk upper bits must be rejected by I4 on both sides"
    );
}

// ── MAJOR-2 regression: global Flat(w) constraint escape ────────────

#[test]
fn bveq_major2_global_constraint_escape() {
    // MAJOR-2 regression (L20): the review demonstrated that the global
    // Flat(w) constraint on ALL registers certifies a corrupting sequence
    // as identity. The sequence [Rotri r3,4 ; Ccnot r0,r3,r5 ; Rotli r3,4]
    // has net effect r0 ^= rotr(r3,4) & r5, with r3 restored. Under global
    // Flat(4), r3's upper nibble is assumed zero, hiding the corruption.
    //
    // Fix: operand_regs restricts the constraint to declared operands only.
    // With r3 NOT declared as Flat(4), its upper bits are free and the
    // corruption is visible.
    //
    // Concrete corruption witness: r3=0xF0, r5=0x0F → r0 corrupted (0→0x0F).
    let model = bveq::symbolic_exec(&[
        Instr::Rotri { rd: 3, imm: 4 },
        Instr::Ccnot { rd: 0, rs: 3, rt: 5 },
        Instr::Rotli { rd: 3, imm: 4 },
    ])
    .unwrap();
    let identity = RegTransfer::identity();

    // With operand_regs = [0, 5] (r3 NOT declared as Flat(4)):
    // r3's upper bits are free → corruption visible → must REJECT.
    let result = bveq::equiv(&model, &identity, 4, &[0, 5]);
    assert!(
        result.is_err(),
        "MAJOR-2: sequence must be rejected when r3 is not Flat(4) \
         (concrete corruption: r3=0xF0, r5=0x0F → r0 corrupted 0→0x0F)"
    );

    // Verify the corruption concretely.
    let regs_before = [0u8, 0, 0, 0xF0, 0, 0x0F, 0, 0];
    let regs_after = bveq::eval_transfer(&model, &regs_before);
    assert_ne!(
        regs_after[0], regs_before[0],
        "concrete corruption: r0 should change when r3=0xF0, r5=0x0F"
    );
    assert_eq!(
        regs_after[0], 0x0F,
        "r0 should be 0x0F (rotr(0xF0,4)=0x0F, 0x0F & 0x0F = 0x0F, 0 ^ 0x0F = 0x0F)"
    );
}

// ── MAJOR-3 regression: panics + silent aliasing → Err ──────────────

#[test]
fn bveq_major3_oob_register_returns_err() {
    // MAJOR-3 regression (L20): symbolic_exec with rd=8 panicked on
    // index OOB at apply_instr. Fix: returns Err(RegisterOutOfRange).
    let result = bveq::symbolic_exec(&[Instr::Add { rd: 8, rs: 1 }]);
    assert!(
        result.is_err(),
        "MAJOR-3: symbolic_exec with rd=8 must return Err, not panic"
    );
    match result.unwrap_err() {
        BveqError::RegisterOutOfRange { val: 8, .. } => {}
        other => panic!("expected RegisterOutOfRange(rd=8), got: {other}"),
    }
}

#[test]
fn bveq_major3_oob_var_returns_err() {
    // MAJOR-3 regression (L20): equiv on Var(20,0) silently aliased to
    // Var(4,2), making textually different denotations compare equal.
    // Fix: expr_to_bdd validates (operand, bit) ranges via checked_var.
    let mut transfer_a = RegTransfer::identity();
    transfer_a.regs[0][0] = BvExpr::var(20, 0); // out-of-range operand

    let mut transfer_b = RegTransfer::identity();
    transfer_b.regs[0][0] = BvExpr::var(4, 2); // the alias target

    // Before fix: equiv(a, b, 8) returned Ok (silent aliasing).
    // After fix: must return Err(VarOutOfRange).
    let result = bveq::equiv(&transfer_a, &transfer_b, 8, &[0, 1]);
    assert!(
        result.is_err(),
        "MAJOR-3: Var(20,0) must return Err, not silently alias to Var(4,2)"
    );
    match result.unwrap_err() {
        BveqError::VarOutOfRange { operand: 20, .. } => {}
        other => panic!("expected VarOutOfRange(operand=20), got: {other}"),
    }
}

#[test]
fn bveq_major3_width_out_of_range_returns_err() {
    // MAJOR-3 regression (L20): equiv assert!(w <= BITS) panicked.
    // Fix: returns Err(WidthOutOfRange).
    let identity = RegTransfer::identity();
    let result = bveq::equiv(&identity, &identity, 9, &[0]);
    assert!(
        result.is_err(),
        "MAJOR-3: w=9 must return Err, not panic"
    );
    match result.unwrap_err() {
        BveqError::WidthOutOfRange { w: 9 } => {}
        other => panic!("expected WidthOutOfRange(9), got: {other}"),
    }

    // w=0 must also be rejected.
    let result = bveq::equiv(&identity, &identity, 0, &[0]);
    assert!(
        result.is_err(),
        "MAJOR-3: w=0 must return Err"
    );
}

// ── MAJOR-4: mis-map negative with teeth on equiv ───────────────────

#[test]
fn bveq_mismap_negative() {
    // Part 1: raw BDD demonstration (kept for documentation).
    // Proves the structural BDD equality trap exists.
    use biodivine_lib_bdd::BddVariableSet;

    let bdd_vars = BddVariableSet::new_anonymous(16);
    let all_vars = bdd_vars.variables();

    let a_correct = bdd_vars.mk_var(all_vars[0]);
    let b_correct = bdd_vars.mk_var(all_vars[1]);

    assert_ne!(
        a_correct, b_correct,
        "different functions must be unequal with correct (shared) mapping"
    );

    let b_mismap = bdd_vars.mk_var(all_vars[0]);
    assert_eq!(
        a_correct, b_mismap,
        "mis-mapping must make different functions appear equal (proving the trap)"
    );

    // Part 2: equiv-level shared-mapping discipline check.
    let mut transfer_a = RegTransfer::identity();
    transfer_a.regs[0] = (0..BITS)
        .map(|b| BvExpr::var(0, b))
        .collect();

    let mut transfer_b = RegTransfer::identity();
    transfer_b.regs[0] = (0..BITS)
        .map(|b| BvExpr::var(1, b))
        .collect();

    let result = bveq::equiv(&transfer_a, &transfer_b, 8, &[0, 1]);
    assert!(
        result.is_err(),
        "equiv must detect different functions with shared mapping"
    );
}

#[test]
fn bveq_major4_mismap_canary_has_teeth() {
    // MAJOR-4 regression (L20): proves the shared-mapping discipline is
    // LOAD-BEARING for equiv. Two genuinely-equal transfers: under normal
    // (shared) mapping their BDDs are identical; under a mis-mapped b-side
    // (operands shifted by 1) the BDDs differ — proving that a mapping
    // divergence would cause equiv to give wrong results.
    //
    // This is the "real canary" the review demanded: it exercises equiv's
    // mapping discipline at the BDD level, not just raw biodivine.
    let add = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();
    let map = bveq::VarMap::new(NUM_REGS).unwrap();

    // Normal encoding: two identical transfers produce identical BDDs.
    let normal_a = bveq::expr_to_bdd(&add.regs[0][0], &map).unwrap();
    let normal_b = bveq::expr_to_bdd(&add.regs[0][0], &map).unwrap();
    assert_eq!(
        normal_a, normal_b,
        "identical expressions with shared mapping must produce equal BDDs"
    );

    // Mis-mapped encoding: same expression but b-side operands shifted by 1.
    // The Add output for reg[0] references operand 0 and 1; under shift,
    // it would reference operand 1 and 2 instead — different BDD.
    let shifted_b = bveq::expr_to_bdd_shifted(&add.regs[0][0], &map, 1).unwrap();
    assert_ne!(
        normal_a, shifted_b,
        "MAJOR-4: mis-mapped encoding MUST produce a different BDD — \
         this proves the shared-mapping discipline is load-bearing. \
         If this assertion failed, the mapping discipline would be vacuous."
    );

    // Also verify that normal equiv on the same transfer returns Ok.
    bveq::equiv(&add, &add, 8, &[0, 1])
        .expect("identical Add transfers must be equal under shared mapping");
}

// ── MAJOR-5 regression: aliased operands rejected ───────────────────

#[test]
fn bveq_major5_aliased_xor_rejected() {
    // MAJOR-5 regression (L20): Xor{rd:0, rs:0} is rejected by asm::encode
    // (AliasedOperands) but bveq modeled it as zeroing. The hardware deviates.
    // Fix: apply_instr rejects aliased 2-op instructions.
    let result = bveq::instr_semantics(&Instr::Xor { rd: 0, rs: 0 });
    assert!(
        result.is_err(),
        "MAJOR-5: Xor{{rd:0, rs:0}} must be rejected (ISA-illegal aliased operands)"
    );
    match result.unwrap_err() {
        BveqError::AliasedOperands { .. } => {}
        other => panic!("expected AliasedOperands, got: {other}"),
    }
}

#[test]
fn bveq_major5_aliased_add_rejected() {
    // MAJOR-5: Add{rd:0, rs:0} → Err (ISA rejects, hardware deviates).
    let result = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 0 });
    assert!(
        result.is_err(),
        "MAJOR-5: Add{{rd:0, rs:0}} must be rejected"
    );
    match result.unwrap_err() {
        BveqError::AliasedOperands { .. } => {}
        other => panic!("expected AliasedOperands, got: {other}"),
    }
}

#[test]
fn bveq_major5_non_distinct_ccnot_rejected() {
    // MAJOR-5 regression (L20): non-distinct Ccnot must be rejected.
    // asm::encode returns CcnotNotDistinct; bveq must mirror this.
    let result = bveq::instr_semantics(&Instr::Ccnot { rd: 0, rs: 0, rt: 1 });
    assert!(
        result.is_err(),
        "MAJOR-5: Ccnot with rd==rs must be rejected"
    );
    match result.unwrap_err() {
        BveqError::NonDistinctOperands { .. } => {}
        other => panic!("expected NonDistinctOperands, got: {other}"),
    }

    // All three equal.
    let result = bveq::instr_semantics(&Instr::Ccnot { rd: 1, rs: 1, rt: 1 });
    assert!(result.is_err(), "MAJOR-5: Ccnot with all-same must be rejected");

    // rs == rt.
    let result = bveq::instr_semantics(&Instr::Ccnot { rd: 0, rs: 1, rt: 1 });
    assert!(result.is_err(), "MAJOR-5: Ccnot with rs==rt must be rejected");
}

#[test]
fn bveq_major5_non_distinct_cswap_rejected() {
    // MAJOR-5: non-distinct Cswap → Err.
    let result = bveq::instr_semantics(&Instr::Cswap { rd: 0, rs: 1, rt: 0 });
    assert!(
        result.is_err(),
        "MAJOR-5: Cswap with rd==rt must be rejected"
    );
    match result.unwrap_err() {
        BveqError::NonDistinctOperands { .. } => {}
        other => panic!("expected NonDistinctOperands, got: {other}"),
    }
}

#[test]
fn bveq_major5_all_two_op_aliased_rejected() {
    // Verify all 2-operand instructions reject rd==rs.
    let aliased = vec![
        Instr::Add { rd: 3, rs: 3 },
        Instr::Sub { rd: 2, rs: 2 },
        Instr::Xor { rd: 5, rs: 5 },
        Instr::Exch { rd: 0, rs: 0 },
        Instr::Rotl { rd: 7, rs: 7 },
        Instr::Rotr { rd: 1, rs: 1 },
    ];
    for instr in &aliased {
        let result = bveq::instr_semantics(instr);
        assert!(
            result.is_err(),
            "MAJOR-5: {instr:?} with aliased operands must be rejected"
        );
    }
}

// ── T6: reconciliation L-a (symbolic ≡ exec_instr, full domain) ─────

#[test]
fn bveq_reconciliation_la_full_domain() {
    // 2-operand instructions: 256^2 = 65536 iterations each.
    let two_op: Vec<Instr> = vec![
        Instr::Add { rd: 0, rs: 1 },
        Instr::Sub { rd: 0, rs: 1 },
        Instr::Xor { rd: 0, rs: 1 },
        Instr::Exch { rd: 0, rs: 1 },
        Instr::Rotl { rd: 0, rs: 1 },
        Instr::Rotr { rd: 0, rs: 1 },
    ];

    for instr in &two_op {
        let transfer = bveq::instr_semantics(instr).unwrap();
        for a in 0..=255u8 {
            for b in 0..=255u8 {
                let regs = [a, b, 0, 0, 0, 0, 0, 0];
                let sym_out = bveq::eval_transfer(&transfer, &regs);

                let mut state = state_with_regs(&[a, b]);
                exec_instr(*instr, &mut state);

                assert_eq!(
                    sym_out, state.regs,
                    "L-a: symbolic ≠ exec_instr for {instr:?} on ({a}, {b})"
                );
            }
        }
    }

    // 3-operand instructions: 256^3 = 16,777,216 iterations each.
    let three_op: Vec<Instr> = vec![
        Instr::Cswap { rd: 0, rs: 1, rt: 2 },
        Instr::Ccnot { rd: 0, rs: 1, rt: 2 },
    ];

    for instr in &three_op {
        let transfer = bveq::instr_semantics(instr).unwrap();
        for a in 0..=255u8 {
            for b in 0..=255u8 {
                for c in 0..=255u8 {
                    let regs = [a, b, c, 0, 0, 0, 0, 0];
                    let sym_out = bveq::eval_transfer(&transfer, &regs);

                    let mut state = state_with_regs(&[a, b, c]);
                    exec_instr(*instr, &mut state);

                    assert_eq!(
                        sym_out, state.regs,
                        "L-a: symbolic ≠ exec_instr for {instr:?} on ({a}, {b}, {c})"
                    );
                }
            }
        }
    }

    // Immediate instructions: test ALL 256 immediates × 256 register values.
    for imm in 0..=255u8 {
        let imm_instrs: Vec<Instr> = vec![
            Instr::Addi { rd: 0, imm },
            Instr::Subi { rd: 0, imm },
            Instr::Xori { rd: 0, imm },
            Instr::Rotli { rd: 0, imm },
            Instr::Rotri { rd: 0, imm },
        ];
        for instr in &imm_instrs {
            let transfer = bveq::instr_semantics(instr).unwrap();
            for a in 0..=255u8 {
                let regs = [a, 0, 0, 0, 0, 0, 0, 0];
                let sym_out = bveq::eval_transfer(&transfer, &regs);

                let mut state = state_with_regs(&[a]);
                exec_instr(*instr, &mut state);

                assert_eq!(
                    sym_out, state.regs,
                    "L-a: symbolic ≠ exec_instr for {instr:?} on ({a})"
                );
            }
        }
    }
}

// ── T7: reconciliation L-b (exec_instr ≡ netlist, stratified sample) ─

/// Generate test cases for a given instruction: (instruction, initial_state) pairs.
/// Varies operand values AND register selectors to cover decoder mux paths.
fn netlist_test_cases() -> Vec<(&'static str, Instr, StateVals)> {
    let mut cases = Vec::new();

    // Add with different register selectors + representative values.
    cases.push((
        "Add(0,1) vals=42,170",
        Instr::Add { rd: 0, rs: 1 },
        state_with_regs(&[42, 170]),
    ));

    // Sub with (3,5): non-default register path.
    let mut s = state_with_regs(&[]);
    s.regs[3] = 200;
    s.regs[5] = 100;
    cases.push(("Sub(3,5) vals=200,100", Instr::Sub { rd: 3, rs: 5 }, s));

    // Xor with (2,6).
    let mut s = state_with_regs(&[]);
    s.regs[2] = 0xAA;
    s.regs[6] = 0x55;
    cases.push(("Xor(2,6) vals=AA,55", Instr::Xor { rd: 2, rs: 6 }, s));

    // Exch with (1,4).
    let mut s = state_with_regs(&[]);
    s.regs[1] = 0xFF;
    s.regs[4] = 0x01;
    cases.push(("Exch(1,4) vals=FF,01", Instr::Exch { rd: 1, rs: 4 }, s));

    // Rotl with (5,3): amount register path.
    let mut s = state_with_regs(&[]);
    s.regs[5] = 0xC3;
    s.regs[3] = 3;
    cases.push(("Rotl(5,3) vals=C3,3", Instr::Rotl { rd: 5, rs: 3 }, s));

    // Rotr with (0,7).
    let mut s = state_with_regs(&[]);
    s.regs[0] = 0xE7;
    s.regs[7] = 5;
    cases.push(("Rotr(0,7) vals=E7,5", Instr::Rotr { rd: 0, rs: 7 }, s));

    // Cswap with (4,2,6): 3-op register paths.
    let mut s = state_with_regs(&[]);
    s.regs[4] = 1; // control bit set → swap
    s.regs[2] = 0xAB;
    s.regs[6] = 0xCD;
    cases.push((
        "Cswap(4,2,6) ctrl=1",
        Instr::Cswap { rd: 4, rs: 2, rt: 6 },
        s,
    ));

    // Ccnot with (3,5,7): different 3-op path.
    let mut s = state_with_regs(&[]);
    s.regs[3] = 0x0F;
    s.regs[5] = 0xFF;
    s.regs[7] = 0xAA;
    cases.push((
        "Ccnot(3,5,7) vals=0F,FF,AA",
        Instr::Ccnot { rd: 3, rs: 5, rt: 7 },
        s,
    ));

    // Addi with (6, imm=42).
    let mut s = state_with_regs(&[]);
    s.regs[6] = 200;
    cases.push(("Addi(6,42) val=200", Instr::Addi { rd: 6, imm: 42 }, s));

    // Subi with (1, imm=100).
    let mut s = state_with_regs(&[]);
    s.regs[1] = 50;
    cases.push(("Subi(1,100) val=50", Instr::Subi { rd: 1, imm: 100 }, s));

    // Xori with (7, imm=0xFF).
    let mut s = state_with_regs(&[]);
    s.regs[7] = 0xAA;
    cases.push((
        "Xori(7,0xFF) val=AA",
        Instr::Xori { rd: 7, imm: 0xFF },
        s,
    ));

    // Rotli with (0, imm=3).
    let mut s = state_with_regs(&[]);
    s.regs[0] = 0b1100_0011;
    cases.push((
        "Rotli(0,3) val=C3",
        Instr::Rotli { rd: 0, imm: 3 },
        s,
    ));

    // Rotri with (4, imm=5).
    let mut s = state_with_regs(&[]);
    s.regs[4] = 0b1110_0111;
    cases.push((
        "Rotri(4,5) val=E7",
        Instr::Rotri { rd: 4, imm: 5 },
        s,
    ));

    // MINOR-1: rs=0 case — exercises the rs-port index-0 decoder mux.
    let mut s = state_with_regs(&[]);
    s.regs[1] = 42;
    s.regs[0] = 170;
    cases.push((
        "Add(1,0) rs=0",
        Instr::Add { rd: 1, rs: 0 },
        s,
    ));

    // MINOR-1: additional rt indices — rt=3 for Ccnot.
    let mut s = state_with_regs(&[]);
    s.regs[0] = 0xFF;
    s.regs[1] = 0xAA;
    s.regs[3] = 0x55;
    cases.push((
        "Ccnot(0,1,3) rt=3",
        Instr::Ccnot { rd: 0, rs: 1, rt: 3 },
        s,
    ));

    // MINOR-1: Cswap with ctrl=0 (swap-not-taken).
    let mut s = state_with_regs(&[]);
    s.regs[4] = 0; // control bit clear → no swap
    s.regs[2] = 0xAB;
    s.regs[6] = 0xCD;
    cases.push((
        "Cswap(4,2,6) ctrl=0 (not-taken)",
        Instr::Cswap { rd: 4, rs: 2, rt: 6 },
        s,
    ));

    // MINOR-1: rt=4 for Ccnot.
    let mut s = state_with_regs(&[]);
    s.regs[0] = 0x33;
    s.regs[1] = 0xCC;
    s.regs[4] = 0xFF;
    cases.push((
        "Ccnot(0,1,4) rt=4",
        Instr::Ccnot { rd: 0, rs: 1, rt: 4 },
        s,
    ));

    // MINOR-1: rt=5 for Cswap.
    let mut s = state_with_regs(&[]);
    s.regs[0] = 1;
    s.regs[1] = 0xDE;
    s.regs[5] = 0xAD;
    cases.push((
        "Cswap(0,1,5) rt=5",
        Instr::Cswap { rd: 0, rs: 1, rt: 5 },
        s,
    ));

    cases
}

#[test]
fn bveq_reconciliation_lb_netlist_sampled() {
    let cases = netlist_test_cases();

    for (name, instr, init) in &cases {
        let mut exec_state = init.clone();
        exec_instr(*instr, &mut exec_state);

        let netlist_state = run_rom_on_cpu(
            &[*instr],
            init.clone(),
            1,
            0,
            theseus_eval::eval,
        )
        .unwrap_or_else(|e| panic!("run_rom_on_cpu failed for {name}: {e}"));

        assert_eq!(
            netlist_state.regs, exec_state.regs,
            "L-b regs mismatch for {name}: netlist={:?} exec={:?}",
            netlist_state.regs, exec_state.regs
        );

        assert_eq!(
            netlist_state.br, exec_state.br,
            "L-b BR mismatch for {name}"
        );

        assert_eq!(
            netlist_state.scratch, exec_state.scratch,
            "L-b scratch mismatch for {name}: netlist={:?} exec={:?}",
            netlist_state.scratch, exec_state.scratch
        );

        assert_eq!(
            netlist_state.mem, exec_state.mem,
            "L-b mem mismatch for {name}"
        );

        // MINOR-2: quiescence — IR must return to 0, PC must advance to budget.
        assert_eq!(
            netlist_state.ir, 0,
            "L-b IR quiescence: IR must be 0 after execution for {name}"
        );
        assert_eq!(
            netlist_state.pc, 1,
            "L-b PC quiescence: PC must equal budget (1) after execution for {name}"
        );
    }
}

/// Wider netlist reconciliation sweep: more operand values and register
/// assignments per instruction. Behind #[ignore] for the advisor to run
/// via `cargo test -p theseus-tisc --test bveq -- --ignored`.
#[test]
#[ignore = "authoritative netlist reconciliation (slow)"]
fn bveq_reconciliation_lb_netlist_wide() {
    let boundary_vals: &[u8] = &[0, 1, 42, 127, 128, 170, 255];

    // 2-operand instructions with varied register selectors.
    let two_op_configs: Vec<(Instr, &str)> = vec![
        (Instr::Add { rd: 0, rs: 1 }, "Add(0,1)"),
        (Instr::Add { rd: 3, rs: 5 }, "Add(3,5)"),
        (Instr::Sub { rd: 2, rs: 4 }, "Sub(2,4)"),
        (Instr::Xor { rd: 6, rs: 7 }, "Xor(6,7)"),
        (Instr::Exch { rd: 0, rs: 7 }, "Exch(0,7)"),
        (Instr::Rotl { rd: 1, rs: 3 }, "Rotl(1,3)"),
        (Instr::Rotr { rd: 5, rs: 2 }, "Rotr(5,2)"),
    ];

    for (instr, name) in &two_op_configs {
        let (rd, rs) = match instr {
            Instr::Add { rd, rs }
            | Instr::Sub { rd, rs }
            | Instr::Xor { rd, rs }
            | Instr::Exch { rd, rs }
            | Instr::Rotl { rd, rs }
            | Instr::Rotr { rd, rs } => (*rd as usize, *rs as usize),
            _ => unreachable!(),
        };
        for &a in boundary_vals {
            for &b in boundary_vals {
                let mut init = state_with_regs(&[]);
                init.regs[rd] = a;
                init.regs[rs] = b;

                let mut exec_state = init.clone();
                exec_instr(*instr, &mut exec_state);

                let netlist_state = run_rom_on_cpu(
                    &[*instr],
                    init.clone(),
                    1,
                    0,
                    theseus_eval::eval,
                )
                .unwrap_or_else(|e| panic!("netlist failed for {name}({a},{b}): {e}"));

                assert_eq!(
                    netlist_state.regs, exec_state.regs,
                    "wide L-b: regs mismatch {name}({a},{b})"
                );
                assert_eq!(netlist_state.br, exec_state.br, "wide L-b: BR {name}");
                assert_eq!(
                    netlist_state.scratch, exec_state.scratch,
                    "wide L-b: scratch {name}"
                );
                assert_eq!(netlist_state.mem, exec_state.mem, "wide L-b: mem {name}");
                // MINOR-2: quiescence
                assert_eq!(netlist_state.ir, 0, "wide L-b: IR quiescence {name}");
                assert_eq!(netlist_state.pc, 1, "wide L-b: PC quiescence {name}");
            }
        }
    }

    // 3-operand instructions with varied register selectors.
    let three_op_configs: Vec<(Instr, &str)> = vec![
        (Instr::Cswap { rd: 0, rs: 1, rt: 2 }, "Cswap(0,1,2)"),
        (Instr::Cswap { rd: 4, rs: 3, rt: 6 }, "Cswap(4,3,6)"),
        (Instr::Ccnot { rd: 0, rs: 1, rt: 2 }, "Ccnot(0,1,2)"),
        (Instr::Ccnot { rd: 5, rs: 3, rt: 7 }, "Ccnot(5,3,7)"),
    ];

    for (instr, name) in &three_op_configs {
        let (rd, rs, rt) = match instr {
            Instr::Cswap { rd, rs, rt } | Instr::Ccnot { rd, rs, rt } => {
                (*rd as usize, *rs as usize, *rt as usize)
            }
            _ => unreachable!(),
        };
        for &a in boundary_vals {
            for &b in boundary_vals {
                for &c in boundary_vals {
                    let mut init = state_with_regs(&[]);
                    init.regs[rd] = a;
                    init.regs[rs] = b;
                    init.regs[rt] = c;

                    let mut exec_state = init.clone();
                    exec_instr(*instr, &mut exec_state);

                    let netlist_state = run_rom_on_cpu(
                        &[*instr],
                        init.clone(),
                        1,
                        0,
                        theseus_eval::eval,
                    )
                    .unwrap_or_else(|e| {
                        panic!("netlist failed for {name}({a},{b},{c}): {e}")
                    });

                    assert_eq!(
                        netlist_state.regs, exec_state.regs,
                        "wide L-b: regs {name}({a},{b},{c})"
                    );
                    assert_eq!(
                        netlist_state.br, exec_state.br,
                        "wide L-b: BR {name}"
                    );
                    assert_eq!(
                        netlist_state.scratch, exec_state.scratch,
                        "wide L-b: scratch {name}"
                    );
                    assert_eq!(
                        netlist_state.mem, exec_state.mem,
                        "wide L-b: mem {name}"
                    );
                    // MINOR-2: quiescence
                    assert_eq!(netlist_state.ir, 0, "wide L-b: IR quiescence {name}");
                    assert_eq!(netlist_state.pc, 1, "wide L-b: PC quiescence {name}");
                }
            }
        }
    }

    // MINOR-1: immediate instructions in the wide sweep.
    let imm_configs: Vec<(Instr, &str)> = vec![
        (Instr::Addi { rd: 0, imm: 42 }, "Addi(0,42)"),
        (Instr::Subi { rd: 3, imm: 100 }, "Subi(3,100)"),
        (Instr::Xori { rd: 5, imm: 0xFF }, "Xori(5,0xFF)"),
        (Instr::Rotli { rd: 2, imm: 3 }, "Rotli(2,3)"),
        (Instr::Rotri { rd: 7, imm: 5 }, "Rotri(7,5)"),
    ];

    for (instr, name) in &imm_configs {
        let rd = match instr {
            Instr::Addi { rd, .. }
            | Instr::Subi { rd, .. }
            | Instr::Xori { rd, .. }
            | Instr::Rotli { rd, .. }
            | Instr::Rotri { rd, .. } => *rd as usize,
            _ => unreachable!(),
        };
        for &a in boundary_vals {
            let mut init = state_with_regs(&[]);
            init.regs[rd] = a;

            let mut exec_state = init.clone();
            exec_instr(*instr, &mut exec_state);

            let netlist_state = run_rom_on_cpu(
                &[*instr],
                init.clone(),
                1,
                0,
                theseus_eval::eval,
            )
            .unwrap_or_else(|e| panic!("netlist failed for {name}({a}): {e}"));

            assert_eq!(
                netlist_state.regs, exec_state.regs,
                "wide L-b: regs {name}({a})"
            );
            assert_eq!(netlist_state.br, exec_state.br, "wide L-b: BR {name}");
            assert_eq!(
                netlist_state.scratch, exec_state.scratch,
                "wide L-b: scratch {name}"
            );
            assert_eq!(netlist_state.mem, exec_state.mem, "wide L-b: mem {name}");
            assert_eq!(netlist_state.ir, 0, "wide L-b: IR quiescence {name}");
            assert_eq!(netlist_state.pc, 1, "wide L-b: PC quiescence {name}");
        }
    }
}

// ── T8: perf POC (r2 §7 gate) ──────────────────────────────────────

#[test]
fn bveq_perf_poc() {
    // --- BDD equiv: Ccnot at full Flat(8) (2^24 domain) ---
    // MINOR-3: non-reflexive — hand-built spec.
    let t = std::time::Instant::now();
    let ccnot_a = bveq::instr_semantics(&Instr::Ccnot { rd: 0, rs: 1, rt: 2 }).unwrap();
    let mut ccnot_b = RegTransfer::identity();
    for bit in 0..BITS {
        let rd_bit = BvExpr::var(0, bit);
        let rs_bit = BvExpr::var(1, bit);
        let rt_bit = BvExpr::var(2, bit);
        ccnot_b.regs[0][bit] = rd_bit.xor(rs_bit.and(rt_bit));
    }
    bveq::equiv(&ccnot_a, &ccnot_b, 8, &[0, 1, 2]).unwrap();
    let ccnot_bdd_us = t.elapsed().as_secs_f64() * 1e6;

    // --- BDD equiv: 2-op Add at Flat(8) (2^16 domain) ---
    let t = std::time::Instant::now();
    let add_a = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();
    let add_b = bveq::instr_semantics(&Instr::Add { rd: 0, rs: 1 }).unwrap();
    bveq::equiv(&add_a, &add_b, 8, &[0, 1]).unwrap();
    let add_bdd_us = t.elapsed().as_secs_f64() * 1e6;

    // --- Raw-u8 closure enumeration: 2-op Flat(8) (65536 iterations) ---
    let t = std::time::Instant::now();
    let mut _count = 0u64;
    for a in 0..=255u8 {
        for b in 0..=255u8 {
            let result = std::hint::black_box(a.wrapping_add(b));
            if std::hint::black_box(result) != a.wrapping_add(b) {
                _count += 1;
            }
        }
    }
    let raw_enum_us = t.elapsed().as_secs_f64() * 1e6;

    // --- exec_instr enumeration: 2-op Flat(8) (65536 iterations) ---
    let t = std::time::Instant::now();
    for a in 0..=255u8 {
        for b in 0..=255u8 {
            let mut state = state_with_regs(&[a, b]);
            exec_instr(Instr::Add { rd: 0, rs: 1 }, &mut state);
            std::hint::black_box(state.regs[0]);
        }
    }
    let exec_enum_us = t.elapsed().as_secs_f64() * 1e6;

    eprintln!("=== T8 PERF POC ===");
    eprintln!("BDD equiv — Ccnot Flat(8) 3-op (2^24 domain): {ccnot_bdd_us:.1} µs (POC: 11 µs)");
    eprintln!("BDD equiv — Add Flat(8) 2-op (2^16 domain):   {add_bdd_us:.1} µs (POC: 40 µs)");
    eprintln!("Raw-u8 closure enum — 2-op Flat(8) (65536):   {raw_enum_us:.1} µs (POC: 32 µs)");
    eprintln!("exec_instr enum — 2-op Flat(8) (65536):       {exec_enum_us:.1} µs");
    eprintln!("---");
    eprintln!("The win is COMPLETENESS WITHOUT A TABLE:");
    eprintln!("  BDD proves 16.7M-domain equivalence in microseconds");
    eprintln!("  where the Value/BTreeMap table cannot exist at any cost");
    eprintln!("  (projected ~750s CPU + O(100GB) heap for 3-op Flat(8)).");
    eprintln!("  POC measured real Value/BTreeMap path: ~2.0s for 2-op Flat(8).");
}

// ── Non-register-pure rejection ─────────────────────────────────────

#[test]
fn bveq_reject_non_register_pure() {
    let non_pure = vec![
        Instr::Xchg { rd: 0, sx: 2 },
        Instr::Swapbr { rd: 0 },
        Instr::Mexch { rd: 0, addr: 0 },
        Instr::MexchReg { rd: 0, ra: 1 },
    ];
    for instr in &non_pure {
        let result = bveq::instr_semantics(instr);
        assert!(
            result.is_err(),
            "must reject non-register-pure instruction: {instr:?}"
        );
        match result.unwrap_err() {
            BveqError::NonRegisterPure(_) => {}
            other => panic!("expected NonRegisterPure, got: {other}"),
        }
    }
}
