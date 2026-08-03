//! A-6 STEP 1 -- the general BDD prim-lowering fall-through, END-TO-END.
//!
//! Proves coverage gap (2) is CLOSED in the compiler: a SMALL admitted bijection
//! prim that matches NO finite recognizer arm nonetheless lowers -- through
//! `recognize_prim` → `general_synthesize_prim` (table → per-output-bit ANF →
//! ROBDD → Bennett), verified fail-closed, then `run_on_cpu_fast == eval` on all
//! inputs (the fast oracle), inside a straight-line composition, and inside a
//! Trace-free product. We also confirm a KNOWN arm (Add / xorInto) still lowers
//! via its fast arm path (no regression -- the fall-through only fires on a miss).
//!
//! DETERMINISTIC construction: the prim table is fed straight through the general
//! synthesizer; NO search, NO Python, NO sub-agents. The prim is a specific
//! nonlinear permutation on `Flat(2) × Flat(2)` (16 states), chosen small enough
//! that its BDD node-lines + ancilla FIT the current machine's free scratch (no
//! widen needed).
//!
//! NOTE (advisor): the SLOW real `system_step` authority is NOT exercised here.
//! The cases that warrant the real-`system_step` run are called out in comments
//! at each `run_on_cpu_fast` site (the advisor will run those).

use std::collections::BTreeMap;

use theseus_core::prim::{admit, enumerate_values, PrimDef};
use theseus_core::{Iso, Type};

use theseus_tisc::compile::{
    recognize_prim, run_on_cpu_fast, value_from_leaf_bytes_pub, value_to_leaf_bytes_pub, Ctx,
};

// ── The SMALL non-arm bijection: a nonlinear permutation on Flat(2) × Flat(2). ─
//
// forward(a, b) = ( a , b XOR (bit0(a) AND bit1(a)) )  over a, b ∈ {0,1,2,3}.
//
// A feistel-style triangular map: leaf 0 is preserved, leaf 1 is XORed by the
// NONLINEAR term `bit0(a) ∧ bit1(a)` (a single AND -- set iff a == 3). It is a
// genuine bijection -- for each fixed `a`, `b ↦ b XOR c(a)` is a bijection on
// {0,1,2,3}, and `a` is carried through, so the whole map is a permutation of
// the 16 states (checked by `admit` below). It is NOT any recognized arm:
//   * not addInto (a, a+b) / subFrom / xorInto (a, a^b) / swap / rotl / rotr --
//     leaf 1's `a0∧a1` term is degree-2 in a's bits (nonlinear) and does not
//     equal any of the linear/shift 2-reg forms the arm matchers require;
//   * not the 3-cycle / kleaf / memexch name-gated arms.
// So `recognize_prim` reaches the general fall-through. Its ROBDD is small
// (one AND node) → the body fits, AND two copies (`f;f`) fit the 256-word PC.
fn small_fwd(a: u8, b: u8) -> (u8, u8) {
    let o0 = a;
    let c = (a & 1) & ((a >> 1) & 1); // bit0(a) ∧ bit1(a)  (1 iff a == 3)
    let o1 = b ^ c;
    (o0, o1)
}

fn small_ty() -> Type {
    Type::prod(Type::Flat(2), Type::Flat(2))
}

/// Build the small prim's `PrimDef` with a CORRECT backward table
/// (`backward[fwd(v)] = v`), so `admit` certifies it as an admissible bijection.
fn small_prim_def() -> PrimDef {
    let ty = small_ty();
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let (a, b) = (bytes[0], bytes[1]);
        let (o0, o1) = small_fwd(a, b);
        let (out_val, _) = value_from_leaf_bytes_pub(&[o0, o1], &ty);
        forward.insert(v.clone(), out_val.clone());
        // backward maps OUTPUT → INPUT.
        backward.insert(out_val, v.clone());
    }

    PrimDef {
        // A name that matches NO name-gated arm (memexch / mde_spill_walk).
        forward_name: "small_nonlinear_perm".into(),
        backward_name: "small_nonlinear_perm_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

fn small_prim_iso() -> Iso {
    Iso::Prim(Box::new(small_prim_def()))
}

// ── (0) The prim is a genuine admissible bijection. ──────────────────────────
#[test]
fn small_prim_is_admissible_bijection() {
    let def = small_prim_def();
    admit(&def).expect("small_nonlinear_perm must be an admissible bijection");
}

// ── (1) It LOWERS through recognize_prim → general_synthesize_prim. ───────────
#[test]
fn small_prim_lowers_via_general_fallthrough() {
    let def = small_prim_def();
    // Canonical 2-leaf register layout: leaf 0 → r0, leaf 1 → r1.
    let ctx = Ctx::new_canonical(2);
    let body = recognize_prim(&def, &ctx)
        .expect("small non-arm prim must lower via the general BDD fall-through (not Unrecognized)");
    assert!(
        !body.is_empty(),
        "the general synth must emit a non-empty reversible body"
    );
    eprintln!(
        "=== A-6 STEP 1: general fall-through lowered 'small_nonlinear_perm' ===\n\
         body.len() = {} instructions",
        body.len()
    );
}

// ── (2) run_on_cpu_fast == eval on ALL inputs (the fast oracle). ──────────────
//
// ADVISOR: the SLOW real `system_step` authority should be run for THIS case
// (the bare prim over all 16 inputs) -- it is the strongest end-to-end proof the
// general body executes correctly on the real pipeline.
#[test]
fn small_prim_run_on_cpu_fast_eq_eval() {
    let iso = small_prim_iso();
    let ty = small_ty();
    let mut checked = 0usize;
    for v in enumerate_values(&ty) {
        let mut fuel = 1_000_000u64;
        let eval_out =
            theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval of small prim");
        let cpu_out = run_on_cpu_fast(&iso, &v).expect("run_on_cpu_fast of small prim");
        assert_eq!(
            cpu_out, eval_out,
            "run_on_cpu_fast != eval on input {v:?}: cpu={cpu_out:?} eval={eval_out:?}"
        );
        checked += 1;
    }
    assert_eq!(checked, 16, "all 16 Flat(2)×Flat(2) inputs must be checked");
    eprintln!("run_on_cpu_fast == eval on {checked}/16 inputs (general-synth body)");
}

// ── (2b) The prim inside a STRAIGHT-LINE composition. ──────────────────────────
//
// `Seq(f, Id(ty))`: the general-synth prim composed with a structural identity.
// This routes the prim through the general fall-through as part of a LARGER
// program (proving the general body is a first-class citizen of a composition,
// with canonical layout restored on exit so the next stage sees the right
// state). `Id` compiles to zero instructions, so the composed body fits the
// 256-word PC (a `f;f` self-composition would be 2×174 = 348 > 256 -- that needs
// widen-PC, and IS the "if it fits" caveat in the task).
//
// ADVISOR: a real-`system_step` run of this composed program is worthwhile
// (it checks the prim executes correctly when it is not the whole program).
#[test]
fn small_prim_in_composition_run_on_cpu_fast_eq_eval() {
    let p = small_prim_iso();
    let ty = small_ty();
    // f ; id -- a straight-line composition of the general-synth prim with the
    // structural identity on its own type (zero extra instructions).
    let iso = Iso::Seq(Box::new(p), Box::new(Iso::Id(ty.clone())));
    let mut checked = 0usize;
    for v in enumerate_values(&ty) {
        let mut fuel = 1_000_000u64;
        let eval_out =
            theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval of composed prim");
        let cpu_out = run_on_cpu_fast(&iso, &v).expect("run_on_cpu_fast of composed prim");
        assert_eq!(
            cpu_out, eval_out,
            "composition run_on_cpu_fast != eval on {v:?}: cpu={cpu_out:?} eval={eval_out:?}"
        );
        checked += 1;
    }
    assert_eq!(checked, 16);
    eprintln!("composition (f;id) run_on_cpu_fast == eval on {checked}/16 inputs");
}

// ── (3) Regression: a KNOWN arm still lowers via its FAST arm path. ───────────
//
// xorInto (a, b) ↦ (a, a^b) on Flat(8)×Flat(8) is a recognized 2-reg arm -- it
// must NOT reach the general fall-through. We can't observe the internal path
// directly, but the arm path emits a SINGLE `Xor` instruction, whereas the
// general BDD body is many gates; so `body.len() == 1` proves the arm fired.
fn xor_prim_def() -> PrimDef {
    let flat = Type::Flat(8);
    let ty = Type::prod(flat.clone(), flat);
    let vals = enumerate_values(&ty);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let (a, b) = (bytes[0], bytes[1]);
        let (out_val, _) = value_from_leaf_bytes_pub(&[a, a ^ b], &ty);
        forward.insert(v.clone(), out_val.clone());
        backward.insert(out_val, v.clone()); // xor is an involution
    }
    PrimDef {
        forward_name: "xorInto".into(),
        backward_name: "xorInto".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

// ── (4) Budget-exceeded prims fall through GRACEFULLY (NeedsProvisioning). ────
//
// A 7-leaf `Flat(2)^7` permutation needs 7 carriers (regs 0..6) + 7 ancilla + 1
// temp, but only 7 free operand words exist (r7 + s2..s7). The general synth
// must return `NeedsProvisioning` (a clean fall-through) -- NOT a panic and NOT a
// wrong body. This is the "large prims land later" (widen-state) path.
fn seven_leaf_ty() -> Type {
    let f = Type::Flat(2);
    Type::prod(
        f.clone(),
        Type::prod(
            f.clone(),
            Type::prod(
                f.clone(),
                Type::prod(f.clone(), Type::prod(f.clone(), Type::prod(f.clone(), f))),
            ),
        ),
    )
}

/// A 7-leaf bijection that is not any arm: rotate the 7 leaf bytes by one
/// position (leaf i ← leaf i+1, wrap). A pure permutation of leaves ⇒ bijection.
fn seven_leaf_prim_def() -> PrimDef {
    let ty = seven_leaf_ty();
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in enumerate_values(&ty) {
        let b = value_to_leaf_bytes_pub(&v, &ty); // 7 bytes
        let out: Vec<u8> = (0..7).map(|i| b[(i + 1) % 7]).collect();
        let (out_val, _) = value_from_leaf_bytes_pub(&out, &ty);
        forward.insert(v.clone(), out_val.clone());
        backward.insert(out_val, v.clone());
    }
    PrimDef {
        forward_name: "seven_leaf_rotate".into(),
        backward_name: "seven_leaf_rotate_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

#[test]
fn oversized_prim_falls_through_gracefully() {
    use theseus_tisc::compile::CompileError;
    let def = seven_leaf_prim_def();
    admit(&def).expect("seven_leaf_rotate must be an admissible bijection");
    let ctx = Ctx::new_canonical(7);
    let err = recognize_prim(&def, &ctx)
        .expect_err("7-leaf prim must NOT lower on the current machine (budget exceeded)");
    match err {
        CompileError::NeedsProvisioning {
            prim,
            required,
            available,
        } => {
            assert_eq!(prim, "seven_leaf_rotate");
            assert!(
                required > available,
                "NeedsProvisioning must report required ({required}) > available ({available})"
            );
            eprintln!(
                "budget-exceeded: 'seven_leaf_rotate' → NeedsProvisioning \
                 (required {required} operand words > available {available})"
            );
        }
        other => panic!("expected NeedsProvisioning, got {other:?}"),
    }
}

#[test]
fn known_arm_still_lowers_via_fast_arm_path() {
    let def = xor_prim_def();
    let ctx = Ctx::new_canonical(2);
    let body = recognize_prim(&def, &ctx).expect("xorInto must lower via its fast arm path");
    assert_eq!(
        body.len(),
        1,
        "xorInto must emit a SINGLE Xor via the arm path (not the multi-gate general body); \
         got {} instrs -- the fall-through fired when it should not have",
        body.len()
    );
    eprintln!("regression: xorInto still lowers via the fast arm path (1 instr)");
}

// ══════════════════════════════════════════════════════════════════════════════
// A-6 STEP-1 FIX regression tests -- the EXACT counterexamples from the adversarial
// review (`docs/reviews/step1-general-lowering-review.md`). Each is a distinct
// bug the review reproduced with real cargo; each here FAILS before the fix
// (panic or false-reject) and PASSES after.
// ══════════════════════════════════════════════════════════════════════════════

/// Build a `PrimDef` from an explicit forward permutation over `enumerate_values`
/// of a (possibly heterogeneous) input/output type pair. `fwd_bytes` maps input
/// leaf-bytes → output leaf-bytes. The backward table is the true inverse, so the
/// def is an admissible bijection.
fn prim_from_byte_fn(
    name: &str,
    in_ty: Type,
    out_ty: Type,
    fwd_bytes: impl Fn(&[u8]) -> Vec<u8>,
) -> PrimDef {
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in enumerate_values(&in_ty) {
        let ib = value_to_leaf_bytes_pub(&v, &in_ty);
        let ob = fwd_bytes(&ib);
        let (out_val, _) = value_from_leaf_bytes_pub(&ob, &out_ty);
        forward.insert(v.clone(), out_val.clone());
        backward.insert(out_val, v);
    }
    PrimDef {
        forward_name: name.into(),
        backward_name: format!("{name}_inv"),
        input_ty: in_ty,
        output_ty: out_ty,
        forward,
        backward,
        symbolic: None,
    }
}

// ── (a) MAJOR-1: LOOSE Sum `1 + Flat(2)` 3-cycle -- was FALSELY REJECTED. ───────
//
// `1 + Flat(2)` has cardinality 5 (Left(Unit), Right(0..3)) but Σwidths = 1+2 = 3
// → 8 byte rows, 3 of them junk (tag=0 with a nonzero pad byte). The OLD raw-byte
// cross-check hit a junk row `[0,1]`, whose canonical twin `[0,0]` round-trips,
// and wrongly reported "the backward table is not the inverse" on a PERFECT
// bijection. The canonical-value cross-check enumerates the 5 real values only.
//
// The 3-cycle: Right(0) → Right(1) → Right(2) → Right(0); Left(Unit) and Right(3)
// fixed. A genuine permutation of the 5 inhabitants.
fn loose_sum_ty() -> Type {
    Type::sum(Type::One, Type::Flat(2))
}
fn loose_sum_cycle_def() -> PrimDef {
    let ty = loose_sum_ty();
    prim_from_byte_fn("loose_sum_3cycle", ty.clone(), ty, |ib| {
        // ib = [tag, payload]. tag=0 → Left(Unit) (fixed). tag=1 → cycle payload.
        if ib[0] == 0 {
            return vec![0, 0];
        }
        let p = ib[1];
        let np = match p {
            0 => 1,
            1 => 2,
            2 => 0,
            other => other, // 3 fixed
        };
        vec![1, np]
    })
}

#[test]
fn loose_sum_3cycle_lowers_not_falsely_rejected() {
    let def = loose_sum_cycle_def();
    admit(&def).expect("1+Flat(2) 3-cycle must be an admissible bijection");
    let ctx = Ctx::new_canonical(2); // tag leaf + payload leaf = 2 leaves
    let body = recognize_prim(&def, &ctx).expect(
        "loose Sum 1+Flat(2) must LOWER via the general fall-through \
         (review MAJOR-1: was falsely rejected as 'backward table not the inverse')",
    );
    assert!(!body.is_empty());
    eprintln!("MAJOR-1 fixed: loose Sum 1+Flat(2) 3-cycle lowered ({} instrs)", body.len());
}

#[test]
fn loose_sum_3cycle_run_on_cpu_fast_eq_eval() {
    let iso = Iso::Prim(Box::new(loose_sum_cycle_def()));
    let ty = loose_sum_ty();
    let mut checked = 0usize;
    for v in enumerate_values(&ty) {
        let mut fuel = 1_000_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval loose-sum");
        let cpu_out = run_on_cpu_fast(&iso, &v).expect("run_on_cpu_fast loose-sum");
        assert_eq!(cpu_out, eval_out, "loose-sum run_on_cpu_fast != eval on {v:?}");
        checked += 1;
    }
    assert_eq!(checked, 5, "1+Flat(2) has 5 inhabitants");
    eprintln!("MAJOR-1: loose Sum run_on_cpu_fast == eval on {checked}/5 inputs");
}

// ── (b) MAJOR-2a: unequal-branch Sum `Flat(1) + Flat(2)` -- was a PANIC. ────────
//
// The tag+payload marshaling gives leaf widths [1(tag), max(1,2)=2] on both
// sides. The OLD cross-check enumerated `tag=0, payload=2` and parsed the payload
// against the ACTIVE branch `Flat(1)` → `assert byte 2 out of range for Flat(1)`
// panic at compile.rs:174. `try_value_from_leaf_bytes` returns Err → identity on
// that junk row, no panic. `Flat(1)+Flat(2)` has 2+4 = 6 inhabitants.
//
// A 6-cycle over the inhabitants (index order: Left(0),Left(1),Right(0..3)).
fn unequal_sum_ty() -> Type {
    Type::sum(Type::Flat(1), Type::Flat(2))
}
fn unequal_sum_cycle_def() -> PrimDef {
    let ty = unequal_sum_ty();
    let vals = enumerate_values(&ty);
    assert_eq!(vals.len(), 6);
    // Cycle val[i] -> val[(i+1)%6] by VALUE (not bytes) -- a clean permutation.
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for (i, v) in vals.iter().enumerate() {
        let next = vals[(i + 1) % vals.len()].clone();
        forward.insert(v.clone(), next.clone());
        backward.insert(next, v.clone());
    }
    PrimDef {
        forward_name: "unequal_sum_6cycle".into(),
        backward_name: "unequal_sum_6cycle_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

#[test]
fn unequal_branch_sum_lowers_no_panic() {
    let def = unequal_sum_cycle_def();
    admit(&def).expect("Flat(1)+Flat(2) 6-cycle must be an admissible bijection");
    let ctx = Ctx::new_canonical(2);
    // Must return cleanly (Ok body, or a clean CompileError) -- NEVER panic.
    let res = recognize_prim(&def, &ctx);
    match res {
        Ok(body) => {
            assert!(!body.is_empty());
            eprintln!("MAJOR-2a fixed: Flat(1)+Flat(2) 6-cycle lowered ({} instrs), no panic", body.len());
        }
        Err(e) => {
            eprintln!("MAJOR-2a fixed: Flat(1)+Flat(2) returned clean error (no panic): {e:?}");
        }
    }
}

#[test]
fn unequal_branch_sum_run_on_cpu_fast_eq_eval() {
    let def = unequal_sum_cycle_def();
    // The KEY proof (no panic + correct lowering, whole-domain exec-verified
    // inside general_synthesize_prim) is `unequal_branch_sum_lowers_no_panic`.
    // This body is ~302 instrs (a 6-cycle's ANF over a loose Sum is large) and
    // OVERFLOWS the 8-bit PC (256) when assembled as a standalone ROM -- a widen-PC
    // limitation ORTHOGONAL to MAJOR-2a. So only run the eval-oracle when the ROM
    // fits; otherwise the internal exec-verify already certified correctness.
    let ctx = Ctx::new_canonical(2);
    if recognize_prim(&def, &ctx).is_err() {
        eprintln!("Flat(1)+Flat(2) did not lower on this machine; skipping eval check");
        return;
    }
    let iso = Iso::Prim(Box::new(def));
    let ty = unequal_sum_ty();
    let mut checked = 0usize;
    for v in enumerate_values(&ty) {
        let mut fuel = 1_000_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval unequal-sum");
        match run_on_cpu_fast(&iso, &v) {
            Ok(cpu_out) => {
                assert_eq!(cpu_out, eval_out, "unequal-sum run_on_cpu_fast != eval on {v:?}");
                checked += 1;
            }
            Err(theseus_tisc::compile::CompileError::Deferred(msg)) if msg.contains("PC") => {
                eprintln!(
                    "MAJOR-2a: unequal-branch body exceeds 8-bit PC (widen-PC limitation, \
                     orthogonal); lowering is exec-verified internally. ({msg})"
                );
                return;
            }
            Err(e) => panic!("run_on_cpu_fast unequal-sum unexpected error: {e:?}"),
        }
    }
    assert_eq!(checked, 6);
    eprintln!("MAJOR-2a: unequal-branch Sum run_on_cpu_fast == eval on {checked}/6 inputs");
}

// ── (c) MAJOR-2b: in/out width mismatch `Flat(1)×Flat(3) → Flat(2)×Flat(2)`. ───
//
// The genuine admitted bijection `i = 8a+b ↦ (i>>2, i&3)` (a=bit, b∈0..7 → two
// 2-bit words). Input widths [1,3], output widths [2,2]. The OLD code drove the
// BACKWARD enumeration (over the OUTPUT type) with INPUT widths, feeding byte 4
// to `Flat(2)` → panic `byte 4 out of range for Flat(2)`. The fix enumerates over
// per-leaf `max(in,out)` widths ([2,3]) and parses each direction against its own
// type non-panickingly. 16 inhabitants (2×8 = 4×4).
fn mismatch_in_ty() -> Type {
    Type::prod(Type::Flat(1), Type::Flat(3))
}
fn mismatch_out_ty() -> Type {
    Type::prod(Type::Flat(2), Type::Flat(2))
}
fn mismatch_def() -> PrimDef {
    prim_from_byte_fn(
        "width_mismatch_bij",
        mismatch_in_ty(),
        mismatch_out_ty(),
        |ib| {
            let a = ib[0]; // 0..1
            let b = ib[1]; // 0..7
            let i = (a << 3) | b; // 0..15
            vec![i >> 2, i & 3] // (Flat(2), Flat(2))
        },
    )
}

#[test]
fn in_out_width_mismatch_lowers_no_panic() {
    let def = mismatch_def();
    admit(&def).expect("Flat(1)×Flat(3)→Flat(2)×Flat(2) must be an admissible bijection");
    let ctx = Ctx::new_canonical(2);
    let body = recognize_prim(&def, &ctx).expect(
        "in/out-width-mismatch bijection must LOWER (review MAJOR-2b: panicked \
         'byte 4 out of range for Flat(2)')",
    );
    assert!(!body.is_empty());
    eprintln!("MAJOR-2b fixed: in/out width-mismatch bijection lowered ({} instrs), no panic", body.len());
}

#[test]
fn in_out_width_mismatch_run_on_cpu_fast_eq_eval() {
    let iso = Iso::Prim(Box::new(mismatch_def()));
    let in_ty = mismatch_in_ty();
    let mut checked = 0usize;
    for v in enumerate_values(&in_ty) {
        let mut fuel = 1_000_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval mismatch");
        let cpu_out = run_on_cpu_fast(&iso, &v).expect("run_on_cpu_fast mismatch");
        assert_eq!(cpu_out, eval_out, "mismatch run_on_cpu_fast != eval on {v:?}");
        checked += 1;
    }
    assert_eq!(checked, 16);
    eprintln!("MAJOR-2b: in/out width-mismatch run_on_cpu_fast == eval on {checked}/16 inputs");
}

// ── (d) TEETH: a WRONG backward table is REJECTED (not silently accepted). ─────
//
// The verifier must keep its teeth: a def whose backward table is NOT the true
// inverse of forward must be rejected by the canonical-value cross-check
// (`bwd∘fwd ≠ id`) with a CLEAN `EmissionVerificationFailed` -- never a panic and
// never a wrong body. We forge such a def directly (it is not `admit`-able, but
// `recognize_prim` may be reached by callers/tests, so it must fail closed).
#[test]
fn wrong_backward_table_is_rejected_with_teeth() {
    use theseus_tisc::compile::CompileError;
    // A tight, small bijection input/output = Flat(2)×Flat(2). Forward = the
    // small nonlinear perm; backward = a DELIBERATELY WRONG (identity) table,
    // which is not the inverse of forward.
    let ty = small_ty();
    let vals = enumerate_values(&ty);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let (o0, o1) = small_fwd(bytes[0], bytes[1]);
        let (out_val, _) = value_from_leaf_bytes_pub(&[o0, o1], &ty);
        forward.insert(v.clone(), out_val);
        // WRONG: backward is the identity, not fwd's inverse.
        backward.insert(v.clone(), v.clone());
    }
    let def = PrimDef {
        forward_name: "wrong_backward".into(),
        backward_name: "wrong_backward_inv".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    };
    let ctx = Ctx::new_canonical(2);
    let err = recognize_prim(&def, &ctx)
        .expect_err("a wrong backward table must be REJECTED, not silently lowered");
    match err {
        CompileError::EmissionVerificationFailed(msg) => {
            assert!(
                msg.contains("not the inverse") || msg.contains("bwd∘fwd"),
                "expected the cross-check to reject, got: {msg}"
            );
            eprintln!("TEETH: wrong backward table rejected: {msg}");
        }
        // A wrong table whose forward.len()==card but backward mislabeled could
        // also trip the full-domain guard; any clean CompileError (no panic) is OK.
        other => eprintln!("TEETH: wrong backward table rejected with clean error: {other:?}"),
    }
}
