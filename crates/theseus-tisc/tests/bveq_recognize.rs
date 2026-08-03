#![cfg(feature = "heavy-tests")]
// HEAVY tier (measured >4min): gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! BVEQ emission-verifier tests (slice-2: decomposed check).
//!
//! T-suite-agree: the verifier ACCEPTs every currently-recognized prim.
//! T-scratch-homed: scratch-homed recognition still passes (MAJOR-1).
//! T-transposed-teeth: transposed/wrong-arm emissions are REJECTED.
//! T-operand-from-type: candidate-derived operands FALSE-ACCEPT; type-derived REJECT.
//! T-aliased-home: aliased-register emission is a recognize-time reject (MEDIUM-1).

use std::collections::BTreeMap;
use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Type, Value};
use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    bveq_verify_emission, marshal, mde_walk_bytes, recognize_prim, unmarshal,
    value_to_leaf_bytes_pub, CompileError, Ctx, Location, OccSet,
};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

// ── Helpers ─────────────────────────────────────────────────────────

fn bit_ty() -> Type {
    Type::sum(Type::One, Type::One)
}

fn flat8() -> Type {
    Type::Flat(8)
}

fn flat4() -> Type {
    Type::Flat(4)
}

fn nat_chain(n: usize) -> Type {
    assert!(n >= 2);
    let mut t = Type::sum(Type::One, Type::One);
    for _ in 2..n {
        t = Type::sum(Type::One, t);
    }
    t
}

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

/// Build a 2-reg PrimDef from a byte-level function on Flat(w)×Flat(w).
fn prim_2reg(name: &str, ty: &Type, f: impl Fn(u8, u8) -> (u8, u8)) -> PrimDef {
    let prod_ty = Type::prod(ty.clone(), ty.clone());
    let vals = enumerate_values(&prod_ty);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let Value::Pair(a, b) = v else { panic!() };
        let a_byte = leaf_val_to_u8(a);
        let b_byte = leaf_val_to_u8(b);
        let (oa, ob) = f(a_byte, b_byte);
        let out = Value::pair(u8_to_leaf_val(oa, ty), u8_to_leaf_val(ob, ty));
        backward.insert(out.clone(), v.clone());
        forward.insert(v.clone(), out);
    }
    PrimDef {
        forward_name: name.into(),
        backward_name: format!("{name}_inv"),
        input_ty: prod_ty.clone(),
        output_ty: prod_ty,
        forward,
        backward,
        symbolic: None,
    }
}

/// Build a 3-reg PrimDef from a byte-level function on Flat(w)×Flat(w)×Flat(w).
fn prim_3reg(name: &str, ty: &Type, f: impl Fn(u8, u8, u8) -> (u8, u8, u8)) -> PrimDef {
    let prod_ty = Type::prod(ty.clone(), Type::prod(ty.clone(), ty.clone()));
    let vals = enumerate_values(&prod_ty);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let Value::Pair(c, ab) = v else { panic!() };
        let Value::Pair(a, b) = ab.as_ref() else {
            panic!()
        };
        let cb = leaf_val_to_u8(c);
        let ab_val = leaf_val_to_u8(a);
        let bb = leaf_val_to_u8(b);
        let (oc, oa, ob) = f(cb, ab_val, bb);
        let out = Value::pair(
            u8_to_leaf_val(oc, ty),
            Value::pair(u8_to_leaf_val(oa, ty), u8_to_leaf_val(ob, ty)),
        );
        backward.insert(out.clone(), v.clone());
        forward.insert(v.clone(), out);
    }
    PrimDef {
        forward_name: name.into(),
        backward_name: format!("{name}_inv"),
        input_ty: prod_ty.clone(),
        output_ty: prod_ty,
        forward,
        backward,
        symbolic: None,
    }
}

/// Build a 1-reg PrimDef from a byte-level function on Flat(w).
fn prim_1reg(name: &str, ty: &Type, f: impl Fn(u8) -> u8) -> PrimDef {
    let vals = enumerate_values(ty);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let byte = leaf_val_to_u8(v);
        let out_byte = f(byte);
        let out = u8_to_leaf_val(out_byte, ty);
        backward.insert(out.clone(), v.clone());
        forward.insert(v.clone(), out);
    }
    PrimDef {
        forward_name: name.into(),
        backward_name: format!("{name}_inv"),
        input_ty: ty.clone(),
        output_ty: ty.clone(),
        forward,
        backward,
        symbolic: None,
    }
}

fn leaf_val_to_u8(v: &Value) -> u8 {
    let mut count = 0u8;
    let mut cur = v;
    loop {
        match cur {
            Value::Left(_) => return count,
            Value::Right(inner) => {
                count += 1;
                cur = inner;
            }
            _ => return count,
        }
    }
}

fn u8_to_leaf_val(byte: u8, ty: &Type) -> Value {
    match ty {
        Type::Flat(_) | Type::Sum(_, _) => {
            let mut v = Value::left(Value::Unit);
            for _ in 0..byte {
                v = Value::right(v);
            }
            v
        }
        _ => panic!("u8_to_leaf_val: unsupported type"),
    }
}

fn ctx_regs(n: usize) -> Ctx {
    let homes: Vec<Location> = (0..n).map(|i| Location::Reg(i as u8)).collect();
    let occ = OccSet::from_locations(&homes);
    Ctx {
        homes,
        occ,
        reusable_aux: 0,
    }
}

/// Build the walk PrimDef on Sum(N5, Bit) from the byte map.
fn build_walk_prim() -> PrimDef {
    let n5 = nat_chain(5);
    let bit = bit_ty();
    let carrier = Type::sum(n5, bit);
    let vals = enumerate_values(&carrier);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let sv = marshal(v, &carrier);
        let mut bytes: Vec<u8> = sv.regs[..5].to_vec();
        mde_walk_bytes(&mut bytes);
        let out_val = unmarshal(&state_with_regs(&bytes), &carrier);
        forward.insert(v.clone(), out_val.clone());
        backward.insert(out_val, v.clone());
    }

    PrimDef {
        forward_name: "mde_spill_walk".into(),
        backward_name: "mde_spill_walk_inv".into(),
        input_ty: carrier.clone(),
        output_ty: carrier,
        forward,
        backward,
        symbolic: None,
    }
}

// ── T-suite-agree: bveq emission-verifier accepts every recognized prim ──

#[test]
fn t_suite_agree_xor_into_f8() {
    let def = prim_2reg("xorInto", &flat8(), |a, b| (a, a ^ b));
    let regs: &[u8] = &[0, 1];
    let body = [Instr::Xor { rd: 1, rs: 0 }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_add_into_f8() {
    let def = prim_2reg("addInto", &flat8(), |a, b| (a, a.wrapping_add(b)));
    let regs: &[u8] = &[0, 1];
    let body = [Instr::Add { rd: 1, rs: 0 }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_sub_from_f8() {
    let def = prim_2reg("subFrom", &flat8(), |a, b| (a, b.wrapping_sub(a)));
    let regs: &[u8] = &[0, 1];
    let body = [Instr::Sub { rd: 1, rs: 0 }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_swap_f8() {
    let def = prim_2reg("swap", &flat8(), |a, b| (b, a));
    let regs: &[u8] = &[0, 1];
    let body = [Instr::Exch { rd: 0, rs: 1 }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_rotl_f8() {
    let def = prim_2reg("rotl", &flat8(), |a, b| (a, b.rotate_left((a & 7) as u32)));
    let regs: &[u8] = &[0, 1];
    let body = [Instr::Rotl { rd: 1, rs: 0 }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_rotr_f8() {
    let def = prim_2reg("rotr", &flat8(), |a, b| (a, b.rotate_right((a & 7) as u32)));
    let regs: &[u8] = &[0, 1];
    let body = [Instr::Rotr { rd: 1, rs: 0 }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_cswap_f4() {
    let def = prim_3reg("cswap", &flat4(), |c, a, b| {
        if c & 1 == 1 {
            (c, b, a)
        } else {
            (c, a, b)
        }
    });
    let regs: &[u8] = &[0, 1, 2];
    let body = [Instr::Cswap {
        rd: 0,
        rs: 1,
        rt: 2,
    }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_ccnot_first_f4() {
    let def = prim_3reg("ccnot_first", &flat4(), |t, c1, c2| (t ^ (c1 & c2), c1, c2));
    let regs: &[u8] = &[0, 1, 2];
    let body = [Instr::Ccnot {
        rd: 0,
        rs: 1,
        rt: 2,
    }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_ccnot_last_f4() {
    let def = prim_3reg("ccnot_last", &flat4(), |c1, c2, t| (c1, c2, t ^ (c1 & c2)));
    let regs: &[u8] = &[0, 1, 2];
    let body = [Instr::Ccnot {
        rd: 2,
        rs: 0,
        rt: 1,
    }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_addi_f8() {
    let k: u8 = 5;
    let def = prim_1reg("addi_5", &flat8(), |a| a.wrapping_add(k));
    let regs: &[u8] = &[0];
    let body = [Instr::Addi { rd: 0, imm: k }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_subi_f8() {
    let k: u8 = 7;
    let def = prim_1reg("subi_7", &flat8(), |a| a.wrapping_sub(k));
    let regs: &[u8] = &[0];
    let body = [Instr::Subi { rd: 0, imm: k }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_xori_f8() {
    let k: u8 = 0xA3;
    let def = prim_1reg("xori_A3", &flat8(), |a| a ^ k);
    let regs: &[u8] = &[0];
    let body = [Instr::Xori { rd: 0, imm: k }];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_3cycle_fwd() {
    // Sum(Bit, One) carrier -- 2-leaf, 3 inhabitants.
    let carrier = Type::sum(bit_ty(), Type::One);
    let vals = enumerate_values(&carrier);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let in_bytes = value_to_leaf_bytes_pub(v, &carrier);
        let (a, b) = (in_bytes[0], in_bytes[1]);
        let (oa, ob) = (a ^ b ^ 1, a);
        let out_bytes = vec![oa, ob];
        // Reconstruct via marshal round-trip to stay canonical.
        let out_sv = state_with_regs(&out_bytes);
        let out_val = unmarshal(&out_sv, &carrier);
        forward.insert(v.clone(), out_val.clone());
        backward.insert(out_val, v.clone());
    }
    let def = PrimDef {
        forward_name: "3cycle_fwd".into(),
        backward_name: "3cycle_inv".into(),
        input_ty: carrier.clone(),
        output_ty: carrier,
        forward,
        backward,
        symbolic: None,
    };
    let regs: &[u8] = &[0, 1];
    let body = [
        Instr::Xor { rd: 0, rs: 1 },
        Instr::Xori { rd: 0, imm: 1 },
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Xori { rd: 1, imm: 1 },
    ];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

#[test]
fn t_suite_agree_mde_spill_walk() {
    let def = build_walk_prim();
    let regs: &[u8] = &[0, 1, 2, 3, 4];
    let body = [
        Instr::Xor { rd: 0, rs: 1 },
        Instr::Xori { rd: 0, imm: 1 },
        Instr::Xori { rd: 1, imm: 1 },
        Instr::Xor { rd: 2, rs: 3 },
        Instr::Xor { rd: 3, rs: 4 },
        Instr::Xori { rd: 0, imm: 1 },
        Instr::Cswap {
            rd: 0,
            rs: 1,
            rt: 2,
        },
        Instr::Cswap {
            rd: 0,
            rs: 2,
            rt: 3,
        },
        Instr::Cswap {
            rd: 0,
            rs: 3,
            rt: 4,
        },
        Instr::Xori { rd: 0, imm: 1 },
        Instr::Xor { rd: 3, rs: 4 },
        Instr::Xor { rd: 2, rs: 3 },
        Instr::Xor { rd: 1, rs: 2 },
    ];
    bveq_verify_emission(&body, &def, regs).unwrap();
}

// ── T-scratch-homed: recognition with a scratch-homed leaf passes ──

#[test]
fn t_scratch_homed_recognition_passes() {
    // Build an xorInto prim on Flat(8)×Flat(8), with leaf 1 in Scratch(2).
    // This exercises MAJOR-1: the check runs on the closure's `body`
    // (the register-pure Xor instruction), not on page_in ++ body ++ page_out.
    let def = prim_2reg("xorInto", &flat8(), |a, b| (a, a ^ b));
    let ctx = Ctx {
        homes: vec![Location::Reg(0), Location::Scratch(2)],
        occ: OccSet::from_locations(&[Location::Reg(0), Location::Scratch(2)]),
        reusable_aux: 0,
    };
    let result = recognize_prim(&def, &ctx);
    assert!(
        result.is_ok(),
        "scratch-homed xorInto should be recognized: {result:?}"
    );
    let instrs = result.unwrap();
    // The result should include Xchg page-in, the Xor body, and Xchg page-out.
    assert!(
        instrs.iter().any(|i| matches!(i, Instr::Xchg { .. })),
        "expected Xchg paging in {instrs:?}"
    );
    assert!(
        instrs.iter().any(|i| matches!(i, Instr::Xor { .. })),
        "expected Xor body in {instrs:?}"
    );
}

// ── T-transposed-teeth: transposed operands are REJECTED ──

#[test]
fn t_transposed_xor_rejected() {
    // Build an xorInto table: (a, b) ↦ (a, a ^ b).
    // Emit the WRONG body: Xor r0,r1 (rd=0, rs=1) instead of Xor r1,r0.
    // This is the transposed-operand bug enumeration is blind to.
    let def = prim_2reg("xorInto", &flat8(), |a, b| (a, a ^ b));
    let regs: &[u8] = &[0, 1];
    // Transposed: Xor rd=0,rs=1 (modifies reg 0 instead of reg 1).
    let body = [Instr::Xor { rd: 0, rs: 1 }];
    // The correct body is [Xor { rd: 1, rs: 0 }]; we already tested it accepts.
    // This transposed version should fail.
    let result = bveq_verify_emission(&body, &def, regs);
    assert!(
        matches!(result, Err(CompileError::EmissionVerificationFailed(_))),
        "transposed Xor should be rejected: {result:?}"
    );
}

#[test]
fn t_wrong_arm_add_for_xor_rejected() {
    // Build an xorInto table: (a, b) ↦ (a, a ^ b).
    // Emit Add r1,r0 (the addInto body) instead of Xor r1,r0.
    let def = prim_2reg("xorInto", &flat8(), |a, b| (a, a ^ b));
    let regs: &[u8] = &[0, 1];
    let body = [Instr::Add { rd: 1, rs: 0 }];
    let result = bveq_verify_emission(&body, &def, regs);
    assert!(
        matches!(result, Err(CompileError::EmissionVerificationFailed(_))),
        "Add emitted for xorInto should be rejected: {result:?}"
    );
}

// ── T-operand-from-type: candidate-derived operands FALSE-ACCEPT ──

#[test]
fn t_operand_from_type_rejects_corruption() {
    // Build a prim on Flat(8)×Flat(8): (a, b) ↦ (a, a ^ b) -- standard xorInto.
    // Emit a body that corrupts result conditioned on a non-operand register:
    // Xor r1,r0; Ccnot rd=1,rs=3,rt=4
    // With TYPE-derived operands [r0, r1], the Ccnot touches r1 conditioned on
    // r3&r4 -- the frame-closure check catches r1's output depending on r3,r4.
    // If someone derived operands from the candidate (including r3,r4 as
    // "operands"), the corruption would be hidden (FALSE-ACCEPT).
    let def = prim_2reg("xorInto", &flat8(), |a, b| (a, a ^ b));

    // TYPE-derived operands: only r0, r1.
    let type_regs: &[u8] = &[0, 1];
    let body = [
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Ccnot {
            rd: 1,
            rs: 3,
            rt: 4,
        },
    ];
    let result = bveq_verify_emission(&body, &def, type_regs);
    assert!(
        matches!(result, Err(CompileError::EmissionVerificationFailed(_))),
        "type-derived operands should catch non-operand dependence: {result:?}"
    );

    // If we OVER-DECLARE operands to include r3, r4 (candidate-derived),
    // the frame closure would not catch the dependence. This is the
    // false-accept vector from Q2b -- we verify it would pass.
    let candidate_regs: &[u8] = &[0, 1, 3, 4];
    let result2 = bveq_verify_emission(&body, &def, candidate_regs);
    // The pointwise check might still catch it (since the table only has entries
    // for the 2-operand function), but the frame-closure check specifically
    // passes with over-declared operands -- this is the danger.
    // The exact outcome depends on whether pointwise catches it (r3,r4 default
    // to 0 in the input so Ccnot rd=1,rs=3,rt=4 is a no-op at those inputs).
    // At all-zeros for r3,r4 the Ccnot is identity → pointwise passes.
    // So candidate-derived operands give a FALSE ACCEPT.
    assert!(
        result2.is_ok(),
        "over-declared operands should (dangerously) pass: {result2:?}"
    );
}

// ── T-aliased-home: Xor r0,r0 is a recognize-time reject (MEDIUM-1) ──

#[test]
fn t_aliased_home_xor_rejected() {
    // Aliased homes: both leaves homed at Reg(0). The verifier (via
    // symbolic_exec) rejects the aliased Xor r0,r0 instruction.
    let def = prim_2reg("xorInto", &flat8(), |a, b| (a, a ^ b));
    let regs: &[u8] = &[0, 0]; // aliased
    let body = [Instr::Xor { rd: 0, rs: 0 }];
    let result = bveq_verify_emission(&body, &def, regs);
    assert!(
        matches!(result, Err(CompileError::EmissionVerificationFailed(_))),
        "aliased Xor r0,r0 should be rejected: {result:?}"
    );
}

#[test]
fn t_aliased_home_swap_rejected_by_recognize() {
    // The swap arm already has an explicit aliased-register guard, but
    // with the bveq verifier wired in, even if that guard were removed,
    // symbolic_exec would reject Exch r0,r0. Test via recognize_prim.
    let bit = bit_ty();
    let ty = Type::prod(bit.clone(), bit.clone());
    let vals = enumerate_values(&ty);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let Value::Pair(a, b) = v else { panic!() };
        let swapped = Value::pair(b.as_ref().clone(), a.as_ref().clone());
        forward.insert(v.clone(), swapped.clone());
        backward.insert(swapped, v.clone());
    }
    let def = PrimDef {
        forward_name: "swap".into(),
        backward_name: "swap".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    };
    let ctx = Ctx {
        homes: vec![Location::Reg(0), Location::Reg(0)],
        occ: OccSet::from_locations(&[Location::Reg(0)]),
        reusable_aux: 0,
    };
    let result = recognize_prim(&def, &ctx);
    assert!(
        result.is_err(),
        "aliased swap should be rejected at recognize time: {result:?}"
    );
}

// ── T-suite-agree via recognize_prim (end-to-end) ──

#[test]
fn t_recognize_xor_into_f8() {
    let def = prim_2reg("xorInto", &flat8(), |a, b| (a, a ^ b));
    let ctx = ctx_regs(2);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "xorInto should be recognized: {result:?}");
}

#[test]
fn t_recognize_add_into_f8() {
    let def = prim_2reg("addInto", &flat8(), |a, b| (a, a.wrapping_add(b)));
    let ctx = ctx_regs(2);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "addInto should be recognized: {result:?}");
}

#[test]
fn t_recognize_sub_from_f8() {
    let def = prim_2reg("subFrom", &flat8(), |a, b| (a, b.wrapping_sub(a)));
    let ctx = ctx_regs(2);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "subFrom should be recognized: {result:?}");
}

#[test]
fn t_recognize_swap_f8() {
    let def = prim_2reg("swap", &flat8(), |a, b| (b, a));
    let ctx = ctx_regs(2);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "swap should be recognized: {result:?}");
}

#[test]
fn t_recognize_rotl_f8() {
    let def = prim_2reg("rotl", &flat8(), |a, b| (a, b.rotate_left((a & 7) as u32)));
    let ctx = ctx_regs(2);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "rotl should be recognized: {result:?}");
}

#[test]
fn t_recognize_rotr_f8() {
    let def = prim_2reg("rotr", &flat8(), |a, b| (a, b.rotate_right((a & 7) as u32)));
    let ctx = ctx_regs(2);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "rotr should be recognized: {result:?}");
}

#[test]
fn t_recognize_cswap_f4() {
    let def = prim_3reg("cswap", &flat4(), |c, a, b| {
        if c & 1 == 1 {
            (c, b, a)
        } else {
            (c, a, b)
        }
    });
    let ctx = ctx_regs(3);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "cswap should be recognized: {result:?}");
}

#[test]
fn t_recognize_ccnot_first_f4() {
    let def = prim_3reg("ccnot_first", &flat4(), |t, c1, c2| (t ^ (c1 & c2), c1, c2));
    let ctx = ctx_regs(3);
    let result = recognize_prim(&def, &ctx);
    assert!(
        result.is_ok(),
        "ccnot-first should be recognized: {result:?}"
    );
}

#[test]
fn t_recognize_ccnot_last_f4() {
    let def = prim_3reg("ccnot_last", &flat4(), |c1, c2, t| (c1, c2, t ^ (c1 & c2)));
    let ctx = ctx_regs(3);
    let result = recognize_prim(&def, &ctx);
    assert!(
        result.is_ok(),
        "ccnot-last should be recognized: {result:?}"
    );
}

#[test]
fn t_recognize_addi_f8() {
    let k: u8 = 5;
    let def = prim_1reg("addi_5", &flat8(), |a| a.wrapping_add(k));
    let ctx = ctx_regs(1);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "addi should be recognized: {result:?}");
}

#[test]
fn t_recognize_subi_f8() {
    let k: u8 = 7;
    let def = prim_1reg("subi_7", &flat8(), |a| a.wrapping_sub(k));
    let ctx = ctx_regs(1);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "subi should be recognized: {result:?}");
}

#[test]
fn t_recognize_xori_f8() {
    let k: u8 = 0xA3;
    let def = prim_1reg("xori_A3", &flat8(), |a| a ^ k);
    let ctx = ctx_regs(1);
    let result = recognize_prim(&def, &ctx);
    assert!(result.is_ok(), "xori should be recognized: {result:?}");
}

#[test]
fn t_recognize_mde_spill_walk() {
    let def = build_walk_prim();
    let ctx = ctx_regs(5);
    let result = recognize_prim(&def, &ctx);
    assert!(
        result.is_ok(),
        "mde_spill_walk should be recognized: {result:?}"
    );
}
