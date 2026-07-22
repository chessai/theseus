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
