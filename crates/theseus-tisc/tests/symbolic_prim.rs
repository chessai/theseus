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

// ── I2: eval-via-spec == eval-via-table at Flat(4) (F1 fix: oracle path) ─

#[test]
fn i2_eval_via_spec_matches_table_flat4() {
    // F1 FIX: build a SYMBOLIC-ONLY twin (same spec, empty table) and a
    // TABLE twin (full table, no spec). Route through theseus_eval::eval
    // so the REAL interp.rs eval-via-spec path is exercised (the old test
    // re-implemented the loop — the L19 self-written-checker class).
    let w = 4u8;
    let spec = masked_accum_spec(w);
    let table_def = masked_accum_table_prim(w);

    // Table prim must be admissible.
    prim::admit(&table_def).expect("table prim admissible");

    // Symbolic-only twin (same spec, empty table).
    let sym_def = PrimDef {
        forward_name: "maskacc4_sym".into(),
        backward_name: "maskacc4_sym_inv".into(),
        input_ty: table_def.input_ty.clone(),
        output_ty: table_def.output_ty.clone(),
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    };

    let table_iso = theseus_core::Iso::Prim(Box::new(table_def.clone()));
    let sym_iso = theseus_core::Iso::Prim(Box::new(sym_def));

    let domain = enumerate_values(&table_def.input_ty);
    assert_eq!(domain.len(), 4096); // 16^3

    for v in &domain {
        let table_out = theseus_eval::eval(&table_iso, v.clone(), &mut 1000)
            .expect("table eval");
        let spec_out = theseus_eval::eval(&sym_iso, v.clone(), &mut 1000)
            .expect("spec eval");
        assert_eq!(
            spec_out, table_out,
            "oracle divergence (eval-via-spec vs eval-via-table)"
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

// ── recognize_prim: symbolic-only → selection-by-equiv (3b) ─────────

#[test]
fn recognize_prim_selects_ccnot_for_symbolic_only() {
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

    // Must complete FAST via selection-by-equiv (not hang enumerating 2^24).
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

    let body = result.expect("selection-by-equiv must recognize masked-accum");
    assert!(
        dt.as_secs() < 2,
        "selection must be fast, not a hang; took {dt:?}"
    );

    // The selected body must be [Ccnot{rd:0, rs:1, rt:2}].
    assert_eq!(body.len(), 1, "expected single Ccnot instruction");
    assert!(
        matches!(body[0], Instr::Ccnot { rd: 0, rs: 1, rt: 2 }),
        "expected Ccnot{{rd:0, rs:1, rt:2}}, got {:?}",
        body[0]
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

// ═══════════════════════════════════════════════════════════════════════
// F1–F5 REPRODUCING / REGRESSION TESTS (L20)
// Each test reproduces the finding — fails without the fix, passes with it.
// ═══════════════════════════════════════════════════════════════════════

// ── F1 CONTROL: a corrupted spec is detected by eval-via-spec ──────
// The old I2 test's self-written loop would NOT detect this because it
// never called theseus_eval::eval. The fixed I2 above routes through
// the real interp.rs path, so a corrupted spec produces different
// results from the table twin (this test confirms the control has teeth).

#[test]
fn f1_control_corrupted_spec_detected_by_eval() {
    let w = 4u8;
    let mut spec = masked_accum_spec(w);
    // Corrupt forward leaf 0: use OR instead of AND.
    let ww = w as usize;
    for b in 0..ww {
        spec.forward.leaves[0][b] =
            BvExpr::var(0, b).xor(BvExpr::var(1, b).or(BvExpr::var(2, b)));
    }
    let table_def = masked_accum_table_prim(w);
    let sym_def = PrimDef {
        forward_name: "maskacc4_corrupt".into(),
        backward_name: "maskacc4_corrupt_inv".into(),
        input_ty: table_def.input_ty.clone(),
        output_ty: table_def.output_ty.clone(),
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    };

    let table_iso = theseus_core::Iso::Prim(Box::new(table_def.clone()));
    let sym_iso = theseus_core::Iso::Prim(Box::new(sym_def));

    // There must exist at least one input where the corrupted spec diverges.
    // acc=1, key=2, elem=3: AND gives 2, OR gives 3 → different XOR results.
    let input = Value::pair(u8_leaf(1), Value::pair(u8_leaf(2), u8_leaf(3)));
    let table_out = theseus_eval::eval(&table_iso, input.clone(), &mut 1000)
        .expect("table eval");
    let spec_out = theseus_eval::eval(&sym_iso, input, &mut 1000)
        .expect("spec eval");
    assert_ne!(
        spec_out, table_out,
        "F1 CONTROL: corrupted spec MUST diverge from table — \
         if this passes, the test has no teeth"
    );
}

// ── F2: partial-table + spec bypass → PartialTableWithSpec ─────────
// A wide prim with a spec AND a 1-entry table used to bypass the hang
// guard. Now rejected FAST by the arithmetic cardinality precheck.

#[test]
fn f2_partial_table_with_spec_rejected_fast() {
    let w = 8u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));

    // Build a prim with 1 table entry + a spec.
    let single_input = Value::pair(u8_leaf(0), Value::pair(u8_leaf(0), u8_leaf(0)));
    let single_output = single_input.clone(); // masked_accum(0,0,0) = (0,0,0)
    let mut fwd = BTreeMap::new();
    fwd.insert(single_input.clone(), single_output.clone());
    let mut bwd = BTreeMap::new();
    bwd.insert(single_output, single_input);

    let def = PrimDef {
        forward_name: "maskacc8_partial".into(),
        backward_name: "maskacc8_partial_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: fwd,
        backward: bwd,
        symbolic: Some(spec),
    };

    // Must be rejected FAST (no enumeration of 2^24 values).
    let t0 = std::time::Instant::now();
    let err = prim::admit(&def).unwrap_err();
    let dt = t0.elapsed();

    assert_eq!(err, AdmitError::PartialTableWithSpec);
    assert!(
        dt.as_secs() < 2,
        "F2: rejection must be instant, not a hang; took {dt:?}"
    );
}

// ── F3: malformed spec → InvalidSpec / clean eval error ────────────

#[test]
fn f3_spec_too_wide_width_rejected() {
    let w = 4u8;
    let mut spec = masked_accum_spec(w);
    // Set width to 5 but the type is Flat(4) → width mismatch.
    spec.forward.width = 5;
    let mut def = masked_accum_table_prim(w);
    def.symbolic = Some(spec);

    let err = prim::admit(&def).unwrap_err();
    assert!(
        matches!(err, AdmitError::InvalidSpec(_)),
        "F3: too-wide width must produce InvalidSpec, got {err:?}"
    );
}

#[test]
fn f3_spec_wrong_leaf_count_rejected() {
    let w = 4u8;
    let mut spec = masked_accum_spec(w);
    // Remove a leaf from forward — 2 leaves for a 3-leaf type.
    spec.forward.leaves.pop();
    let mut def = masked_accum_table_prim(w);
    def.symbolic = Some(spec);

    let err = prim::admit(&def).unwrap_err();
    assert!(
        matches!(err, AdmitError::InvalidSpec(_)),
        "F3: wrong leaf count must produce InvalidSpec, got {err:?}"
    );
}

#[test]
fn f3_spec_short_row_rejected() {
    let w = 4u8;
    let mut spec = masked_accum_spec(w);
    // Truncate leaf 0 to 2 bits instead of 4.
    spec.forward.leaves[0].truncate(2);
    let mut def = masked_accum_table_prim(w);
    def.symbolic = Some(spec);

    let err = prim::admit(&def).unwrap_err();
    assert!(
        matches!(err, AdmitError::InvalidSpec(_)),
        "F3: short row must produce InvalidSpec, got {err:?}"
    );
}

#[test]
fn f3_out_of_range_byte_produces_clean_eval_error() {
    // Construct a symbolic-only prim whose spec produces an out-of-range
    // byte (width=4 spec on Flat(3) type → byte can be 15 but Flat(3) max is 7).
    // The eval path should produce a clean EvalError, not a panic.
    let w3 = Type::Flat(3);
    let ty = Type::prod(w3.clone(), Type::prod(w3.clone(), w3));

    // A spec with width=4 (valid BvExpr, but the type only allows Flat(3)).
    // This bypasses validate_against (we skip admit), so the eval path's
    // try_value_from_leaf_bytes must catch it.
    let make_transfer = || -> theseus_core::SpecTransfer {
        let mut leaves = Vec::with_capacity(3);
        for leaf in 0..3usize {
            let mut bits = Vec::with_capacity(4);
            for b in 0..4usize {
                // bit 3 always set = output byte = 8..15 for any nonzero input
                if b == 3 {
                    bits.push(BvExpr::constant(true));
                } else {
                    bits.push(BvExpr::var(leaf, b));
                }
            }
            leaves.push(bits);
        }
        theseus_core::SpecTransfer { leaves, width: 4 }
    };
    let spec = PrimSpec {
        forward: make_transfer(),
        backward: make_transfer(),
    };
    let def = PrimDef {
        forward_name: "bad_range".into(),
        backward_name: "bad_range_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    };

    let input = Value::pair(u8_leaf(1), Value::pair(u8_leaf(1), u8_leaf(1)));
    let iso = theseus_core::Iso::Prim(Box::new(def));
    let result = theseus_eval::eval(&iso, input, &mut 1000);
    assert!(
        result.is_err(),
        "F3: out-of-range byte must produce EvalError, not silent corruption"
    );
}

// ── F4: backward spec mismatch → SpecTableMismatch ─────────────────

#[test]
fn f4_corrupted_backward_spec_rejected() {
    let w = 4u8;
    let mut spec = masked_accum_spec(w);
    // Corrupt the backward spec (OR instead of AND).
    let ww = w as usize;
    for b in 0..ww {
        spec.backward.leaves[0][b] =
            BvExpr::var(0, b).xor(BvExpr::var(1, b).or(BvExpr::var(2, b)));
    }
    let mut def = masked_accum_table_prim(w);
    def.symbolic = Some(spec);

    let err = prim::admit(&def).unwrap_err();
    assert!(
        matches!(err, AdmitError::SpecTableMismatch { .. }),
        "F4: corrupted backward spec must be caught, got {err:?}"
    );
}

// ── F5: cross-crate decompose agreement ─────────────────────────────
// Design Q2: theseus-core's value_to_leaf_bytes vs theseus-tisc's agree.
// (They are two implementations of the same convention.)

#[test]
fn f5_cross_crate_decompose_agreement() {
    // Sample: Flat(4)^3, a bounded domain (4096 values).
    let w = 4u8;
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 4096);

    for v in &domain {
        let core_bytes = prim::value_to_leaf_bytes(v, &ty);
        let tisc_bytes = theseus_tisc::compile::value_to_leaf_bytes_pub(v, &ty);
        assert_eq!(
            core_bytes, tisc_bytes,
            "F5: core vs tisc decompose disagree at {v:?}"
        );
    }
}

// ═══════════════════════════════════════════════════════════════════════
// SLICE 3b — SELECTION-BY-EQUIV TESTS
// ═══════════════════════════════════════════════════════════════════════

// ── R1: validate_against rejects out-of-range Var indices ─────────────

#[test]
fn r1_operand_index_out_of_range_rejected() {
    let w = 4u8;
    let mut spec = masked_accum_spec(w);
    // Corrupt forward leaf[0][0]: reference operand 5 (only 3 input leaves).
    spec.forward.leaves[0][0] = BvExpr::var(5, 0);
    let def = masked_accum_table_prim(w);

    let err = spec.validate_against(&def.input_ty, &def.output_ty);
    assert!(
        err.is_err(),
        "R1: operand index >= input leaf count must be rejected"
    );
    let msg = err.unwrap_err();
    assert!(
        msg.contains("operand index 5"),
        "R1: error should mention the out-of-range operand, got: {msg}"
    );
}

#[test]
fn r1_bit_index_out_of_range_rejected() {
    let w = 4u8;
    let mut spec = masked_accum_spec(w);
    // Corrupt forward leaf[1][0]: reference bit 8 (width=4, so max bit=3).
    spec.forward.leaves[1][0] = BvExpr::var(1, 8);
    let def = masked_accum_table_prim(w);

    let err = spec.validate_against(&def.input_ty, &def.output_ty);
    assert!(
        err.is_err(),
        "R1: bit index >= width must be rejected"
    );
    let msg = err.unwrap_err();
    assert!(
        msg.contains("bit index 8"),
        "R1: error should mention the out-of-range bit, got: {msg}"
    );
}

// ── R2: Sum zero-padding cross-crate decompose agreement ──────────────

#[test]
fn r2_f5_sum_type_decompose_agreement() {
    // Sum(Flat(4), Flat(4)): tag + max(1,1) = 2 leaves. Tests the Sum
    // zero-padding arm that R2 identified as untested.
    let w = 4u8;
    let f = Type::Flat(w);
    let ty = Type::Sum(Box::new(f.clone()), Box::new(f));
    let domain = enumerate_values(&ty);
    // Sum(Flat(4), Flat(4)) has 16 + 16 = 32 values.
    assert_eq!(domain.len(), 32);

    for v in &domain {
        let core_bytes = prim::value_to_leaf_bytes(v, &ty);
        let tisc_bytes = theseus_tisc::compile::value_to_leaf_bytes_pub(v, &ty);
        assert_eq!(
            core_bytes, tisc_bytes,
            "R2: core vs tisc Sum decompose disagree at {v:?}"
        );
    }
}

// ── R3: timed partial-table+spec test for recognize_prim ──────────────

#[test]
fn r3_partial_table_spec_recognize_prim_fast() {
    use theseus_tisc::asm::Instr;

    let w = 8u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));

    // Build a prim with 1 table entry + spec (partial-table+spec).
    let single_input = Value::pair(u8_leaf(0), Value::pair(u8_leaf(0), u8_leaf(0)));
    let single_output = single_input.clone();
    let mut fwd = BTreeMap::new();
    fwd.insert(single_input.clone(), single_output.clone());
    let mut bwd = BTreeMap::new();
    bwd.insert(single_output, single_input);

    let def = PrimDef {
        forward_name: "maskacc8_partial".into(),
        backward_name: "maskacc8_partial_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: fwd,
        backward: bwd,
        symbolic: Some(spec),
    };

    // recognize_prim must complete FAST (selection-by-equiv, no enumeration).
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

    // Must succeed (spec matches Ccnot) and be fast (no 2^24 enumeration).
    let body = result.expect("partial-table+spec must be recognized via selection-by-equiv");
    assert!(
        dt.as_secs() < 2,
        "R3: must complete fast, not hang; took {dt:?}"
    );
    assert_eq!(body.len(), 1);
    assert!(
        matches!(body[0], Instr::Ccnot { rd: 0, rs: 1, rt: 2 }),
        "R3: expected Ccnot, got {:?}",
        body[0]
    );
}

// ── PAYOFF TEST: Flat(8)^3 masked-accum recognized + bounded oracle ───

#[test]
fn payoff_flat8_masked_accum_recognized_as_ccnot() {
    use theseus_tisc::asm::Instr;

    let w = 8u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));

    // Build a symbolic-only prim (no table — the 2^24 table NEVER built).
    let def = PrimDef {
        forward_name: "maskacc8".into(),
        backward_name: "maskacc8_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty.clone(),
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec.clone()),
    };

    // 1. Admit via admit_symbolic (BDD mutual-inverse certificate).
    bveq::admit_symbolic(&spec).expect("masked-accum self-inverse must pass");

    // 2. Recognize via selection-by-equiv → assert [Ccnot{rd,rs,rt}].
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
    let t0 = std::time::Instant::now();
    let body = theseus_tisc::compile::recognize_prim(&def, &ctx)
        .expect("selection-by-equiv must recognize Flat(8)^3 masked-accum");
    let dt = t0.elapsed();

    assert_eq!(body.len(), 1, "expected single Ccnot instruction");
    let Instr::Ccnot { rd, rs, rt } = body[0] else {
        panic!("expected Ccnot, got {:?}", body[0]);
    };
    assert_eq!((rd, rs, rt), (0, 1, 2), "register assignment");
    assert!(
        dt.as_secs() < 2,
        "recognition at full Flat(8) must be fast; took {dt:?}"
    );

    // 3. Bounded oracle check: exec the emitted Ccnot on concrete inputs,
    //    compare with eval-via-spec. NEVER the 2^24 domain (gate-no-timeout).
    let sym_iso = theseus_core::Iso::Prim(Box::new(def));

    // A handful of representative inputs covering corners.
    let test_inputs: [(u8, u8, u8); 8] = [
        (0, 0, 0),
        (1, 2, 3),
        (255, 255, 255),
        (0, 255, 0),
        (128, 64, 32),
        (7, 13, 42),
        (200, 100, 50),
        (1, 1, 1),
    ];

    for (acc, key, elem) in test_inputs {
        // Eval-via-spec (the ground truth for symbolic-only prims).
        let input = Value::pair(u8_leaf(acc), Value::pair(u8_leaf(key), u8_leaf(elem)));
        let spec_out = theseus_eval::eval(&sym_iso, input, &mut 1000)
            .expect("eval-via-spec");

        // Exec the emitted Ccnot directly.
        let mut regs = [0u8; 8];
        regs[rd as usize] = acc;
        regs[rs as usize] = key;
        regs[rt as usize] = elem;
        let mut state = theseus_tisc::types::StateVals {
            regs,
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0u8; 8],
            mem: [0u8; 4],
        };
        theseus_tisc::compile::exec_instr(body[0], &mut state);

        let exec_acc = state.regs[rd as usize];
        let exec_key = state.regs[rs as usize];
        let exec_elem = state.regs[rt as usize];

        // Decompose the spec output.
        let Value::Pair(out_a, rest) = spec_out else {
            panic!("expected pair");
        };
        let Value::Pair(out_k, out_e) = *rest else {
            panic!("expected pair");
        };
        let spec_acc = leaf_u8(&out_a);
        let spec_key = leaf_u8(&out_k);
        let spec_elem = leaf_u8(&out_e);

        assert_eq!(
            (exec_acc, exec_key, exec_elem),
            (spec_acc, spec_key, spec_elem),
            "bounded oracle mismatch at input ({acc}, {key}, {elem}): \
             exec=({exec_acc},{exec_key},{exec_elem}) vs spec=({spec_acc},{spec_key},{spec_elem})"
        );
    }
}

// ── NON-VACUITY: wrong spec is rejected (no false selection) ──────────

#[test]
fn non_vacuity_wrong_spec_rejected() {
    let w = 8u8;
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));

    // Build a deliberately-wrong spec: OR instead of AND (no arm computes this).
    let make_wrong_transfer = || -> theseus_core::SpecTransfer {
        let ww = w as usize;
        let mut leaves = Vec::with_capacity(3);
        for leaf in 0..3usize {
            let mut bits = Vec::with_capacity(ww);
            for b in 0..ww {
                let expr = if leaf == 0 {
                    // WRONG: OR instead of AND
                    BvExpr::var(0, b).xor(BvExpr::var(1, b).or(BvExpr::var(2, b)))
                } else {
                    BvExpr::var(leaf, b)
                };
                bits.push(expr);
            }
            leaves.push(bits);
        }
        theseus_core::SpecTransfer { leaves, width: w }
    };
    let wrong_spec = PrimSpec {
        forward: make_wrong_transfer(),
        backward: make_wrong_transfer(),
    };

    let def = PrimDef {
        forward_name: "wrong_spec".into(),
        backward_name: "wrong_spec_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(wrong_spec),
    };

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
    let t0 = std::time::Instant::now();
    let result = theseus_tisc::compile::recognize_prim(&def, &ctx);
    let dt = t0.elapsed();

    assert!(
        result.is_err(),
        "NON-VACUITY: wrong spec must NOT be selected by any arm"
    );
    assert!(
        dt.as_secs() < 5,
        "rejection must be bounded (arm library iteration), not a hang; took {dt:?}"
    );
}

// ── A-1: teeth on the spec→register remap shim (impl-review MAJOR) ─────

/// Every other selection test homes the operands at the IDENTITY mapping
/// Reg(0),Reg(1),Reg(2), so the suite is blind to a bug in
/// `remap_expr_operands` (disabling the remap leaves them all green — the
/// exact attack-A coverage hole the impl-review found). This test homes the
/// three Flat(8) operands at SCRAMBLED, non-identity registers Reg(4),Reg(2),
/// Reg(7) and asserts selection recognizes the masked-accum prim as exactly
/// `Ccnot{rd:4, rs:2, rt:7}` — the assignment the TYPE-derived homes dictate.
///
/// Teeth (both proven in the impl-review): at HEAD this passes; disabling the
/// operand→register remap makes selection over-reject (Deferred) so the
/// `.expect` fails, and neutering the `equiv` gate selects the wrong body so
/// the `matches!` fails.
#[test]
fn selection_non_identity_register_mapping() {
    use theseus_tisc::asm::Instr;

    let w = 8u8;
    let spec = masked_accum_spec(w);
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), Type::prod(f.clone(), f));
    let def = PrimDef {
        forward_name: "maskacc8_scrambled".into(),
        backward_name: "maskacc8_scrambled_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    };
    // Scrambled homes: leaf0→r4, leaf1→r2, leaf2→r7 (NON-identity).
    let ctx = theseus_tisc::compile::Ctx {
        homes: vec![
            theseus_tisc::compile::Location::Reg(4),
            theseus_tisc::compile::Location::Reg(2),
            theseus_tisc::compile::Location::Reg(7),
        ],
        occ: theseus_tisc::compile::OccSet::from_locations(&[
            theseus_tisc::compile::Location::Reg(4),
            theseus_tisc::compile::Location::Reg(2),
            theseus_tisc::compile::Location::Reg(7),
        ]),
    };
    let body = theseus_tisc::compile::recognize_prim(&def, &ctx)
        .expect("scrambled-home masked-accum must still be recognized");
    assert_eq!(body.len(), 1, "expected single instr, got {body:?}");
    assert!(
        matches!(body[0], Instr::Ccnot { rd: 4, rs: 2, rt: 7 }),
        "expected Ccnot{{rd:4, rs:2, rt:7}}, got {:?}",
        body[0]
    );
}
