//! A-6.0 transcriber-core tests — the AUTOMATION of `bvexpr_synth_poc.rs`.
//!
//! The POC hand-wrote the gate sequences; here the transcriber PRODUCES them
//! and the same symbolic oracle (`symbolic_exec` + `equiv`) verifies the
//! emission. Each supported spec: assert the exact `Instr` sequence produced AND
//! that it symbolically equals the spec. Plus teeth (a corrupted spec fails
//! equiv) and an out-of-class spec returns `Err`.

use theseus_tisc::asm::Instr;
use theseus_tisc::bveq::{equiv, symbolic_exec, BvExpr, RegTransfer, BITS};
use theseus_tisc::synth::{transcribe_inplace, SynthError};

/// Build a full-frame spec `RegTransfer` from a per-(reg,bit) closure.
/// (Identical harness to the POC.)
fn spec(f: impl Fn(usize, usize) -> BvExpr) -> RegTransfer {
    let mut t = RegTransfer::identity();
    for r in 0..t.regs.len() {
        for b in 0..BITS {
            t.regs[r][b] = f(r, b);
        }
    }
    t
}

fn v(r: usize, b: usize) -> BvExpr {
    BvExpr::var(r, b)
}

/// Assert the transcriber both PRODUCES `expected` and that it verifies.
fn assert_produces_and_verifies(spec_t: &RegTransfer, vars: &[usize], expected: &[Instr]) {
    let body = transcribe_inplace(spec_t).expect("in-place transcription must succeed");
    assert_eq!(
        body, expected,
        "transcriber must produce the expected gate sequence"
    );
    let got = symbolic_exec(&body).expect("symbolic_exec");
    equiv(&got, spec_t, BITS, vars)
        .expect("transcribed gates must symbolically equal the BvExpr spec");
}

/// (1) POC `ccnot_mix`: (x,y,z) ↦ (x ⊕ (y∧z), y, z) → PRODUCE `[Ccnot{0,1,2}]`.
#[test]
fn ccnot_mix_produces_and_verifies() {
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b).and(v(2, b))),
        _ => v(r, b),
    });
    assert_produces_and_verifies(
        &spec_t,
        &[0, 1, 2],
        &[Instr::Ccnot { rd: 0, rs: 1, rt: 2 }],
    );
}

/// (2) POC `affine_nonlinear_mix`: (x,y,z,w) ↦ (x ⊕ w ⊕ (y∧z), …)
/// → PRODUCE `[Xor{0,3}, Ccnot{0,1,2}]`.
#[test]
fn affine_nonlinear_mix_produces_and_verifies() {
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(3, b)).xor(v(1, b).and(v(2, b))),
        _ => v(r, b),
    });
    assert_produces_and_verifies(
        &spec_t,
        &[0, 1, 2, 3],
        &[
            Instr::Xor { rd: 0, rs: 3 },
            Instr::Ccnot { rd: 0, rs: 1, rt: 2 },
        ],
    );
}

/// (3) Pure-affine spec (only Xor): (x,y) ↦ (x ⊕ y, y) → `[Xor{0,1}]`.
#[test]
fn pure_affine_produces_and_verifies() {
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b)),
        _ => v(r, b),
    });
    assert_produces_and_verifies(&spec_t, &[0, 1], &[Instr::Xor { rd: 0, rs: 1 }]);
}

/// (3b) Multi-source affine: (x,y,z) ↦ (x ⊕ y ⊕ z, y, z) → `[Xor{0,1}, Xor{0,2}]`.
#[test]
fn multi_source_affine_produces_and_verifies() {
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b)).xor(v(2, b)),
        _ => v(r, b),
    });
    assert_produces_and_verifies(
        &spec_t,
        &[0, 1, 2],
        &[Instr::Xor { rd: 0, rs: 1 }, Instr::Xor { rd: 0, rs: 2 }],
    );
}

/// (4) Const-flip spec: x ^= 0b1010_0101 (bits 0,2,5,7) → `[Xori{0, 0xA5}]`.
#[test]
fn const_flip_produces_and_verifies() {
    const MASK: u8 = 0b1010_0101;
    let spec_t = spec(|r, b| match r {
        0 => {
            if (MASK >> b) & 1 == 1 {
                v(0, b).xor(BvExpr::Const(true))
            } else {
                v(0, b)
            }
        }
        _ => v(r, b),
    });
    assert_produces_and_verifies(&spec_t, &[0], &[Instr::Xori { rd: 0, imm: MASK }]);
}

/// (4b) Affine + const combined: (x,y) ↦ (x ⊕ y ⊕ 0x0F, y) → `[Xor{0,1}, Xori{0,0x0F}]`.
#[test]
fn affine_plus_const_produces_and_verifies() {
    const MASK: u8 = 0x0F;
    let spec_t = spec(|r, b| match r {
        0 => {
            let base = v(0, b).xor(v(1, b));
            if (MASK >> b) & 1 == 1 {
                base.xor(BvExpr::Const(true))
            } else {
                base
            }
        }
        _ => v(r, b),
    });
    assert_produces_and_verifies(
        &spec_t,
        &[0, 1],
        &[Instr::Xor { rd: 0, rs: 1 }, Instr::Xori { rd: 0, imm: MASK }],
    );
}

/// (Cswap) bit-0-controlled swap: reg[1] ↔ reg[2] iff reg[0]&1 → `[Cswap{0,1,2}]`.
#[test]
fn cswap_produces_and_verifies() {
    // Emit the ground truth from a real Cswap and use that as the spec: this
    // guarantees the spec is EXACTLY what Cswap does, and the transcriber must
    // recover the single gate.
    let spec_t = symbolic_exec(&[Instr::Cswap { rd: 0, rs: 1, rt: 2 }]).unwrap();
    assert_produces_and_verifies(
        &spec_t,
        &[0, 1, 2],
        &[Instr::Cswap { rd: 0, rs: 1, rt: 2 }],
    );
}

/// (5) TEETH: a spec the transcriber emits for VERIFIES, and a DELIBERATELY
/// CORRUPTED expected-spec FAILS equiv against the same emission — proving the
/// oracle is real, not vacuous (the f3.5 lesson).
#[test]
fn teeth_corrupted_spec_fails_equiv() {
    // Real spec: x ⊕ (y ∧ z). Transcriber emits Ccnot{0,1,2}, which verifies.
    let real = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b).and(v(2, b))),
        _ => v(r, b),
    });
    let body = transcribe_inplace(&real).expect("transcribe");
    let got = symbolic_exec(&body).unwrap();
    equiv(&got, &real, BITS, &[0, 1, 2]).expect("real spec verifies");

    // Corrupt the spec: use w-analog by swapping the AND to (y ∧ y-shifted) —
    // here use reg 1 twice differently: x ⊕ (y ∧ y) = x ⊕ y (a real difference).
    let corrupted = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b)), // NOT equal to x ⊕ (y∧z)
        _ => v(r, b),
    });
    assert!(
        equiv(&got, &corrupted, BITS, &[0, 1, 2]).is_err(),
        "the Ccnot emission must NOT verify against a corrupted (x⊕y) spec (teeth)"
    );
}

/// (6) OUT-OF-CLASS: a spec outside the in-place ancilla-free class returns
/// `Err` (NOT a wrong body). A pure register PERMUTATION — reg0 ← reg1, reg1 ←
/// reg0 written WITHOUT the swap-mux shape (i.e. each new value is the OTHER
/// register's old value, not `old ⊕ f`) — is not in-place realizable: neither
/// row is `old ⊕ …`, so `peel_self_xor` fails.
#[test]
fn out_of_class_permutation_returns_err() {
    // (a,b) ↦ (b, a): row 0 is Var(1,b) (no self-term), row 1 is Var(0,b).
    let spec_t = spec(|r, b| match r {
        0 => v(1, b),
        1 => v(0, b),
        _ => v(r, b),
    });
    let res = transcribe_inplace(&spec_t);
    assert!(
        matches!(res, Err(SynthError::RowNotInPlace { .. })),
        "a bare permutation (not old⊕f) must return Err(RowNotInPlace), got {res:?}"
    );
}

/// (6b) OUT-OF-CLASS: a cyclic in-place dependency needs ancilla → `Err`.
/// (x,y) ↦ (x⊕y, y⊕x_new?) is impossible; instead build the classic cycle:
/// row0 = x ⊕ y, row1 = y ⊕ x — each is `old ⊕ (other)`, but they mutually read
/// each other's INPUT while writing it, so no ordering preserves both reads.
#[test]
fn out_of_class_cyclic_ordering_returns_err() {
    // row0: x ^= y ; row1: y ^= x  (each reads the OTHER's input value).
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b)),
        1 => v(1, b).xor(v(0, b)),
        _ => v(r, b),
    });
    let res = transcribe_inplace(&spec_t);
    assert!(
        matches!(res, Err(SynthError::OrderingConflict { .. })),
        "a mutual read-after-write cycle must return Err(OrderingConflict), got {res:?}"
    );
}

/// (6c) OUT-OF-CLASS: two AND terms on one bit (needs 2 Ccnots) → `Err`.
#[test]
fn out_of_class_two_and_terms_returns_err() {
    // x ⊕ (y∧z) ⊕ (y∧w): a single in-place row can carry only one Ccnot.
    let spec_t = spec(|r, b| match r {
        0 => v(0, b)
            .xor(v(1, b).and(v(2, b)))
            .xor(v(1, b).and(v(3, b))),
        _ => v(r, b),
    });
    let res = transcribe_inplace(&spec_t);
    assert!(
        matches!(res, Err(SynthError::RowNotInPlace { .. })),
        "two AND terms on one target must return Err(RowNotInPlace), got {res:?}"
    );
}

/// (identity) The all-identity spec transcribes to the empty program.
#[test]
fn identity_transcribes_to_empty() {
    let spec_t = RegTransfer::identity();
    let body = transcribe_inplace(&spec_t).expect("identity transcribes");
    assert!(body.is_empty(), "identity must emit no gates");
    let got = symbolic_exec(&body).unwrap();
    equiv(&got, &spec_t, BITS, &[]).expect("empty program equals identity");
}

/// (ordering) A NON-cyclic in-place dependency is scheduled correctly:
/// row0 = x ⊕ y, row1 = z ⊕ y (both read y, only x and z written; no cycle).
/// Must succeed and verify regardless of emission order.
#[test]
fn acyclic_shared_read_schedules_and_verifies() {
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b)),
        2 => v(2, b).xor(v(1, b)),
        _ => v(r, b),
    });
    let body = transcribe_inplace(&spec_t).expect("acyclic schedule");
    let got = symbolic_exec(&body).unwrap();
    equiv(&got, &spec_t, BITS, &[0, 1, 2]).expect("verifies");
}
