//! BVEQ slice-3a tests: symbolic prim representation + eval + admit + guards.

use std::collections::BTreeMap;

use theseus_core::bvexpr::BvExpr;
use theseus_core::prim::{
    self, enumerate_values, AdmitError, PrimDef, PrimSpec, SpecTransfer, Value,
};
use theseus_core::ty::Type;
use theseus_tisc::bveq;

// ── helpers ─────────────────────────────────────────────────────────

fn u8_leaf(k: u8) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

fn leaf_u8(v: &Value) -> u8 {
    let mut k = 0u8;
    let mut cur = v;
    loop {
        match cur {
            Value::Left(u) if u.as_ref() == &Value::Unit => return k,
            Value::Right(rest) => {
                k += 1;
                cur = rest;
            }
            _ => panic!("not a thermometer leaf: {cur:?}"),
        }
    }
}

/// Build the masked-accumulate spec at a given width:
/// out_leaf[0].bit[b] = var(0,b) XOR (var(1,b) AND var(2,b))  (acc ^ (key & elem))
/// out_leaf[1..] = identity pass-through.
fn masked_accum_spec(width: u8) -> PrimSpec {
    let w = width as usize;
    let make_transfer = |_is_forward: bool| -> SpecTransfer {
        // Masked-accumulate is self-inverse, so forward == backward.
        let mut leaves = Vec::with_capacity(3);
        for leaf in 0..3usize {
            let mut bits = Vec::with_capacity(w);
            for b in 0..w {
                let expr = if leaf == 0 {
                    BvExpr::var(0, b).xor(BvExpr::var(1, b).and(BvExpr::var(2, b)))
                } else {
                    BvExpr::var(leaf, b)
                };
                bits.push(expr);
            }
            leaves.push(bits);
        }
        SpecTransfer { leaves, width }
    };
    PrimSpec {
        forward: make_transfer(true),
        backward: make_transfer(false),
    }
}

/// Build the masked-accumulate table prim at Flat(w), 3 operands.
fn masked_accum_table_prim(w: u8) -> PrimDef {
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));
    let card = 1u16 << w;

    let mut fwd = BTreeMap::new();
    for a in 0..card as u8 {
        for k in 0..card as u8 {
            for e in 0..card as u8 {
                let inp = Value::pair(u8_leaf(a), Value::pair(u8_leaf(k), u8_leaf(e)));
                let out = Value::pair(
                    u8_leaf(a ^ (k & e)),
                    Value::pair(u8_leaf(k), u8_leaf(e)),
                );
                fwd.insert(inp, out);
            }
        }
    }
    let bwd: BTreeMap<Value, Value> = fwd.iter().map(|(x, y)| (y.clone(), x.clone())).collect();
    PrimDef {
        forward_name: format!("maskacc{w}"),
        backward_name: format!("maskacc{w}_inv"),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: fwd,
        backward: bwd,
        symbolic: None,
    }
}

// ── I2: eval-via-spec == eval-via-table at Flat(4) ──────────────────

#[test]
fn i2_eval_via_spec_matches_table_flat4() {
    let w = 4u8;
    let spec = masked_accum_spec(w);
    let mut def = masked_accum_table_prim(w);
    def.symbolic = Some(spec);

    // Table prim must be admissible (also verifies spec==table in admit).
    prim::admit(&def).expect("table+spec prim admissible");

    // Now eval every input via both table and spec and compare.
    let domain = enumerate_values(&def.input_ty);
    assert_eq!(domain.len(), 4096); // 16^3

    for v in &domain {
        // Table eval:
        let table_out = def.forward.get(v).expect("total").clone();

        // Spec eval (same path as interp.rs):
        let spec = def.symbolic.as_ref().unwrap();
        let width = spec.forward.width as usize;
        let in_bytes = prim::value_to_leaf_bytes(v, &def.input_ty);
        let num_out = spec.forward.leaves.len();
        let mut out_bytes = vec![0u8; num_out];
        for (leaf, ob) in out_bytes.iter_mut().enumerate() {
            for b in 0..width {
                if theseus_core::bvexpr::eval_expr(
                    &spec.forward.leaves[leaf][b],
                    &in_bytes,
                ) {
                    *ob |= 1 << b;
                }
            }
        }
        let spec_out = prim::value_from_leaf_bytes(&out_bytes, &def.output_ty);
        assert_eq!(
            spec_out, table_out,
            "spec/table divergence at {:?}",
            in_bytes
        );
    }
}

// ── Adjoint involution with symbolic spec ───────────────────────────

#[test]
fn adjoint_involution_symbolic_prim() {
    let w = 4u8;
    let spec = masked_accum_spec(w);
    let mut def = masked_accum_table_prim(w);
    def.symbolic = Some(spec);

    let double = def.adjoint().adjoint();
    assert_eq!(double, def, "adjoint∘adjoint must be identity on symbolic prims");
}

// ── admit: symbolic-only → SymbolicOnly ─────────────────────────────

#[test]
fn admit_symbolic_only_returns_symbolic_only() {
    let w = 8u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));

    let def = PrimDef {
        forward_name: "maskacc8".into(),
        backward_name: "maskacc8_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    };

    // core::admit must return SymbolicOnly BEFORE attempting enumeration.
    let err = prim::admit(&def).unwrap_err();
    assert_eq!(err, AdmitError::SymbolicOnly);
}

// ── bveq::admit_symbolic: mutual-inverse certificate ────────────────

#[test]
fn admit_symbolic_valid_spec() {
    let spec = masked_accum_spec(8);
    bveq::admit_symbolic(&spec).expect("self-inverse masked-accum must pass");
}

#[test]
fn admit_symbolic_rejects_non_inverse() {
    // Corrupt the backward spec (use OR instead of AND) so it's not the inverse.
    let w = 4u8;
    let ww = w as usize;
    let mut spec = masked_accum_spec(w);
    // forward: acc ^ (key & elem), backward (corrupted): acc ^ (key | elem)
    for b in 0..ww {
        spec.backward.leaves[0][b] =
            BvExpr::var(0, b).xor(BvExpr::var(1, b).or(BvExpr::var(2, b)));
    }
    let err = bveq::admit_symbolic(&spec);
    assert!(err.is_err(), "corrupted backward must be rejected");
}

// ── recognize_prim: symbolic-only → fast explicit rejection ─────────

#[test]
fn recognize_prim_rejects_symbolic_only_instantly() {
    let w = 8u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));

    let def = PrimDef {
        forward_name: "maskacc8".into(),
        backward_name: "maskacc8_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    };

    // This must complete FAST (not hang enumerating 2^24 values).
    let t0 = std::time::Instant::now();
    let ctx = theseus_tisc::compile::Ctx {
        homes: vec![
            theseus_tisc::compile::Location::Reg(0),
            theseus_tisc::compile::Location::Reg(1),
            theseus_tisc::compile::Location::Reg(2),
        ],
        occ: theseus_tisc::compile::OccSet::from_locations(&[
            theseus_tisc::compile::Location::Reg(0),
            theseus_tisc::compile::Location::Reg(1),
            theseus_tisc::compile::Location::Reg(2),
        ]),
    };
    let result = theseus_tisc::compile::recognize_prim(&def, &ctx);
    let dt = t0.elapsed();

    assert!(result.is_err(), "symbolic-only prim must be rejected");
    assert!(
        dt.as_secs() < 2,
        "rejection must be instant, not a hang; took {dt:?}"
    );
}

// ── bveq_verify_emission: empty table → error ──────────────────────

#[test]
fn bveq_verify_emission_rejects_empty_table() {
    use theseus_tisc::asm::Instr;

    let w = 8u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));

    let def = PrimDef {
        forward_name: "maskacc8".into(),
        backward_name: "maskacc8_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    };

    // A dummy body — the guard should reject BEFORE reaching symbolic_exec.
    let body = [Instr::Ccnot { rd: 0, rs: 1, rt: 2 }];
    let operand_regs = &[0u8, 1, 2];

    let result = theseus_tisc::compile::bveq_verify_emission(&body, &def, operand_regs);
    assert!(
        result.is_err(),
        "empty-table prim must be rejected by bveq_verify_emission"
    );
}

// ── admit: both-present spec/table mismatch → error ─────────────────

#[test]
fn admit_rejects_spec_table_mismatch() {
    let w = 4u8;
    let mut def = masked_accum_table_prim(w);
    let mut spec = masked_accum_spec(w);
    // Corrupt the forward spec (OR instead of AND).
    let ww = w as usize;
    for b in 0..ww {
        spec.forward.leaves[0][b] =
            BvExpr::var(0, b).xor(BvExpr::var(1, b).or(BvExpr::var(2, b)));
    }
    def.symbolic = Some(spec);

    let err = prim::admit(&def).unwrap_err();
    assert!(
        matches!(err, AdmitError::SpecTableMismatch { .. }),
        "mismatched spec/table must produce SpecTableMismatch, got {err:?}"
    );
}

// ── eval-via-spec on a symbolic-only prim (no table) ────────────────

#[test]
fn eval_via_spec_symbolic_only() {
    let w = 4u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));

    let def = PrimDef {
        forward_name: "maskacc4_sym".into(),
        backward_name: "maskacc4_sym_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    };

    // Evaluate at a specific input and check the result.
    // acc=3, key=5, elem=7 → acc ^ (key & elem) = 3 ^ (5&7) = 3 ^ 5 = 6
    let input = Value::pair(u8_leaf(3), Value::pair(u8_leaf(5), u8_leaf(7)));
    let iso = theseus_core::Iso::Prim(Box::new(def));
    let result = theseus_eval::eval(&iso, input, &mut 1000).expect("eval-via-spec");

    let Value::Pair(out_acc, rest) = result else {
        panic!("expected pair");
    };
    let Value::Pair(out_key, out_elem) = *rest else {
        panic!("expected pair");
    };
    assert_eq!(leaf_u8(&out_acc), 6); // 3 ^ (5 & 7) = 6
    assert_eq!(leaf_u8(&out_key), 5);
    assert_eq!(leaf_u8(&out_elem), 7);
}
