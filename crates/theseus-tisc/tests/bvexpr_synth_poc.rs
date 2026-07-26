//! TISC-2 KEYSTONE POC — `BvExpr`-spec → reversible gates → SYMBOLIC verify.
//!
//! Design claim under test (docs/scratch/tisc2-design-synthesis.md §8): a WIDE
//! SYMBOLIC prim — one admitted with a `BvExpr` spec and NO enumerable table,
//! wider than the 12-rail `synthesize_prim` cap — lowers to TISC `Instr`s by a
//! mechanical transcription of its spec, and the emitted gates are verified
//! SYMBOLICALLY (BvExpr → BDD via `symbolic_exec` + `equiv`) with NO enumeration.
//!
//! This is exactly the path the current `bveq_verify_emission` does NOT take
//! (it has a vacuity guard rejecting empty-table/symbolic-only prims and verifies
//! pointwise over the table). The building blocks it needs — `symbolic_exec`
//! (Instr → per-bit BvExpr) and `equiv` (BDD equality) — already exist; this POC
//! composes them into the symbolic verify the keystone requires.
//!
//! Scope (honest): these prims transcribe to in-place, ancilla-free gate networks
//! (the common structured case). The general Bennett case (zero-init ancilla +
//! backward-uncompute) composes this transcription with the frame discipline
//! (which guarantees the zero-init) — verified there, not here.

use theseus_tisc::asm::Instr;
use theseus_tisc::bveq::{equiv, symbolic_exec, BvExpr, RegTransfer, BITS};

/// Build a full-frame spec `RegTransfer` from a per-(reg,bit) closure.
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

/// Nonlinear, wide (3 leaves × 8 = 24 bits > 12-rail cap), symbolic-only.
/// Prim: (x,y,z) ↦ (x ⊕ (y ∧ z), y, z). Forward spec transcribes to ONE
/// byte-parallel Ccnot. Verified symbolically — no table, no enumeration.
#[test]
fn ccnot_mix_symbolic_transcribe_verify() {
    let body = vec![Instr::Ccnot {
        rd: 0,
        rs: 1,
        rt: 2,
    }];
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b).and(v(2, b))),
        _ => v(r, b),
    });
    let got = symbolic_exec(&body).expect("symbolic_exec");
    equiv(&got, &spec_t, BITS, &[0, 1, 2])
        .expect("transcribed gates must symbolically equal the BvExpr spec");
}

/// Multi-gate, mixes linear + nonlinear, ancilla-free, wide (4 leaves = 32 bits).
/// Prim: (x,y,z,w) ↦ (x ⊕ w ⊕ (y ∧ z), y, z, w). Transcription = TWO gates
/// (Xor then Ccnot into x). Demonstrates that a composite spec transcribes and
/// verifies as the composition of its gate steps.
#[test]
fn affine_nonlinear_mix_multi_gate_verify() {
    let body = vec![
        Instr::Xor { rd: 0, rs: 3 }, // x ^= w
        Instr::Ccnot {
            rd: 0,
            rs: 1,
            rt: 2,
        }, // x ^= y & z
    ];
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(3, b)).xor(v(1, b).and(v(2, b))),
        _ => v(r, b),
    });
    let got = symbolic_exec(&body).expect("symbolic_exec");
    equiv(&got, &spec_t, BITS, &[0, 1, 2, 3])
        .expect("multi-gate transcription must symbolically equal the spec");
}

/// TEETH (anti-vacuity): a WRONG transcription MUST be rejected by the symbolic
/// verifier — proving the check is real, not vacuously green (the f3.5 lesson).
#[test]
fn wrong_transcription_is_symbolically_rejected() {
    // Uses w (reg 3) instead of z (reg 2) in the AND term — a real bug.
    let wrong = vec![Instr::Ccnot {
        rd: 0,
        rs: 1,
        rt: 3,
    }];
    let spec_t = spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b).and(v(2, b))),
        _ => v(r, b),
    });
    let got = symbolic_exec(&wrong).expect("symbolic_exec");
    assert!(
        equiv(&got, &spec_t, BITS, &[0, 1, 2, 3]).is_err(),
        "a wrong transcription must FAIL symbolic verify (teeth, not vacuous)"
    );
}
