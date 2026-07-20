//! Ccnot (byte-parallel) instruction tests.
//!
//! T1: exact single-Ccnot emission from a 3-leaf target-first prim.
//! T2: run_on_cpu == eval (real system_step) on all 8 inputs.
//! T3: encode/decode round-trip + CcnotNotDistinct rejection.
//! T4: Ccnot is self-inverse on CPU (distinct operands).
//! T-dense: dense-byte full-byte AND (r[rd] ^= r[rs] & r[rt] over all 8 bits).
//! T-neg: a 2-leaf (c,t)↦(c,t⊕c) recognizes as Xor NOT Ccnot; aliased Ccnot rejected.

use std::collections::BTreeMap;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type};

use theseus_tisc::asm::{decode, encode, AsmError, Instr};
use theseus_tisc::compile::{
    compile_with_metadata, run_on_cpu_resident,
    value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};

// ── Type / prim helpers ────────────────────────────────────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// The Ccnot prim: `(t, c1, c2) ↦ (t ^ (c1&1 & c2&1), c1, c2)` on `Bit × (Bit × Bit)`.
/// TARGET-FIRST layout so the recognizer maps regs[0]=target=rd.
fn ccnot_prim_iso() -> Iso {
    let b = bit();
    let ty = Type::prod(b.clone(), Type::prod(b.clone(), b.clone()));
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let t = bytes[0];
        let c1 = bytes[1];
        let c2 = bytes[2];
        let out_t = t ^ ((c1 & 1) & (c2 & 1));
        let out_bytes = vec![out_t, c1, c2];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        forward.insert(v.clone(), out_val);
    }

    // Ccnot is self-inverse: applying the same function twice = identity.
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let t = bytes[0];
        let c1 = bytes[1];
        let c2 = bytes[2];
        let out_t = t ^ ((c1 & 1) & (c2 & 1));
        let out_bytes = vec![out_t, c1, c2];
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

// ── T1: exact single-Ccnot emission, target-first leaf order ───────

#[test]
fn t1_single_ccnot_emission() {
    let iso = ccnot_prim_iso();
    let lowered = compile_with_metadata(&iso).expect("compile ccnot prim");
    assert_eq!(
        lowered.instrs.len(),
        1,
        "ccnot prim must compile to exactly 1 instruction, got {:?}",
        lowered.instrs
    );
    match lowered.instrs[0] {
        Instr::Ccnot { rd, rs, rt } => {
            // Target-first: rd is the target (leaf 0 → Reg 0),
            // rs/rt are controls (leaf 1 → Reg 1, leaf 2 → Reg 2).
            // Spec §6 T1: fails if rd/rs/rt permuted.
            assert_eq!(rd, 0, "rd must be 0 (target = leaf 0 home)");
            assert_eq!(rs, 1, "rs must be 1 (control1 = leaf 1 home)");
            assert_eq!(rt, 2, "rt must be 2 (control2 = leaf 2 home)");
        }
        ref other => panic!("expected Ccnot, got {other:?}"),
    }
}

// ── T2: run_on_cpu == eval (real system_step) on all 8 inputs ──────

#[test]
#[ignore = "real-CPU system_step; run via --ignored"]
fn t2_real_cpu_all_8() {
    let iso = ccnot_prim_iso();
    let b = bit();
    let ty = Type::prod(b.clone(), Type::prod(b.clone(), b.clone()));
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 8, "Bit×Bit×Bit has 8 inhabitants");

    for input in &domain {
        let expected = {
            let mut fuel = 10_000_000u64;
            theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval ccnot")
        };
        let got = run_on_cpu_resident(&iso, input, 0, |iso, v, fuel| {
            theseus_eval::eval(iso, v, fuel)
        })
        .expect("run_on_cpu_resident");
        assert_eq!(
            got, expected,
            "real CPU mismatch for input {input:?}"
        );
    }
}

// ── T3: encode/decode round-trip + CcnotNotDistinct rejection ──────

#[test]
fn t3_encode_decode_roundtrip() {
    // Valid distinct operands.
    for rd in 0..8u8 {
        for rs in 0..8u8 {
            for rt in 0..8u8 {
                if rd == rs || rd == rt || rs == rt {
                    continue;
                }
                let instr = Instr::Ccnot { rd, rs, rt };
                let word = encode(instr).expect("encode Ccnot");
                // Verify opcode is 0b1011.
                assert_eq!((word >> 11) & 0xF, 0b1011, "opcode must be 0b1011");
                assert_eq!(word & 0x8000, 0x8000, "must be R-format");
                // Round-trip.
                let decoded = decode(word).expect("decode Ccnot");
                assert_eq!(decoded, instr, "round-trip failed for rd={rd} rs={rs} rt={rt}");
            }
        }
    }
}

#[test]
fn t3_ccnot_not_distinct_rejects() {
    // rd == rs
    assert!(matches!(
        encode(Instr::Ccnot { rd: 1, rs: 1, rt: 2 }),
        Err(AsmError::CcnotNotDistinct { rd: 1, rs: 1, rt: 2 })
    ));
    // rd == rt
    assert!(matches!(
        encode(Instr::Ccnot { rd: 3, rs: 0, rt: 3 }),
        Err(AsmError::CcnotNotDistinct { rd: 3, rs: 0, rt: 3 })
    ));
    // rs == rt
    assert!(matches!(
        encode(Instr::Ccnot { rd: 0, rs: 4, rt: 4 }),
        Err(AsmError::CcnotNotDistinct { rd: 0, rs: 4, rt: 4 })
    ));
}

// ── T4: Ccnot is self-inverse on CPU (distinct operands) ───────────

#[test]
fn t4_self_inverse() {
    use theseus_tisc::compile::run_on_cpu_fast;

    let iso = ccnot_prim_iso();
    let b = bit();
    let ty = Type::prod(b.clone(), Type::prod(b.clone(), b.clone()));
    let domain = enumerate_values(&ty);

    // Apply twice = identity.
    let double = Iso::seq(iso.clone(), iso);
    for input in &domain {
        let got = run_on_cpu_fast(&double, input).expect("run_on_cpu_fast double ccnot");
        assert_eq!(
            got, *input,
            "Ccnot applied twice must be identity, failed for {input:?}"
        );
    }
}

// ── T-dense: dense-byte 0xAA/0x55 — Ccnot changes ONLY bit0 ───────

#[test]
fn t_dense_full_byte_and() {
    use theseus_tisc::compile::run_rom_fast;
    use theseus_tisc::types::{StateVals, MEM_WORDS};

    // Dense-byte full-byte AND: r[rd] ^= r[rs] & r[rt] over all 8 bits.
    // r0=0xAA, r1=0x55, r2=0xFF → r0 ^= (0x55 & 0xFF) = 0xAA ^ 0x55 = 0xFF
    let instr = Instr::Ccnot { rd: 0, rs: 1, rt: 2 };
    let rom = vec![encode(instr).unwrap()];

    let state = StateVals {
        regs: [0xAA, 0x55, 0xFF, 0x33, 0x44, 0x66, 0x77, 0x88],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    let out = run_rom_fast(&rom, state.clone(), 1);
    assert_eq!(
        out.regs[0],
        0xAA ^ (0x55 & 0xFF),
        "r0 should be 0xAA ^ (0x55 & 0xFF) = 0xFF (full-byte AND)"
    );
    assert_eq!(out.regs[1], 0x55, "r1 unchanged");
    assert_eq!(out.regs[2], 0xFF, "r2 unchanged");
    assert_eq!(out.regs[3..], state.regs[3..], "other regs unchanged");

    // Case 2: r3=0x33, rs=r0=0xAA, rt=r4=0x44 → r3 ^= (0xAA & 0x44) = 0x33 ^ 0x00 = 0x33
    let instr2 = Instr::Ccnot { rd: 3, rs: 0, rt: 4 };
    let rom2 = vec![encode(instr2).unwrap()];
    let state2 = StateVals {
        regs: [0xAA, 0x55, 0xFF, 0x33, 0x44, 0x66, 0x77, 0x88],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    let out2 = run_rom_fast(&rom2, state2.clone(), 1);
    assert_eq!(
        out2.regs[3],
        0x33 ^ (0xAA & 0x44),
        "r3 ^= (0xAA & 0x44) = 0x33 ^ 0x00 = 0x33"
    );
}

// ── T-neg: 2-leaf xor recognizes as Xor NOT Ccnot ─────────────────

#[test]
fn t_neg_xor_not_ccnot() {
    // Build a 2-leaf (c, t) ↦ (c, t⊕c) prim — this is Xor, NOT Ccnot.
    let b = bit();
    let ty = Type::prod(b.clone(), b.clone());
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let c = bytes[0];
        let t = bytes[1];
        let out_bytes = vec![c, t ^ c];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        forward.insert(v.clone(), out_val.clone());
    }
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let c = bytes[0];
        let t = bytes[1];
        let out_bytes = vec![c, t ^ c];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        backward.insert(v.clone(), out_val);
    }

    let def = PrimDef {
        forward_name: "xor_into".into(),
        backward_name: "xor_into".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
    };
    let iso = Iso::Prim(Box::new(def));
    let lowered = compile_with_metadata(&iso).expect("compile xor prim");
    // Must recognize as Xor, not Ccnot.
    assert_eq!(lowered.instrs.len(), 1, "must be single instruction");
    assert!(
        matches!(lowered.instrs[0], Instr::Xor { .. }),
        "2-leaf (c,t)↦(c,t⊕c) must recognize as Xor, got {:?}",
        lowered.instrs[0]
    );
}

#[test]
fn t_neg_aliased_ccnot_rejected_at_encode() {
    // All three aliased-pair cases must be rejected.
    assert!(encode(Instr::Ccnot { rd: 2, rs: 2, rt: 3 }).is_err());
    assert!(encode(Instr::Ccnot { rd: 2, rs: 3, rt: 2 }).is_err());
    assert!(encode(Instr::Ccnot { rd: 2, rs: 3, rt: 3 }).is_err());
}
