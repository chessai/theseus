//! A-6.1 tests — `transcribe_into` (fresh disjoint targets) and the Bennett
//! wrapper (`transcribe_bennett`) for the general permutation case.
//!
//! Two verification tiers, both with teeth:
//!   * SYMBOLIC (`symbolic_exec` + `equiv`, all-register operands) — proves the
//!     emitted gate network equals the spec as a boolean function.
//!   * NET-ZERO / EXEC (`exec_instr`, register AND scratch ancilla) — runs the
//!     body from concrete carrier states with ancilla = 0 and asserts carriers
//!     end at `P(x)` and EVERY ancilla word returns to 0.
//!
//! The symbolic path cannot model scratch (`symbolic_exec` rejects `Xchg` and
//! operands ≥ 8), so scratch ancilla is exercised only by the exec path — which
//! is exactly where net-zero must be proven anyway.

use theseus_tisc::asm::Instr;
use theseus_tisc::bveq::{equiv, symbolic_exec, BvExpr, RegTransfer, BITS};
use theseus_tisc::compile::exec_instr;
use theseus_tisc::synth::{transcribe_bennett, transcribe_into};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

/// A fully zero-init machine state (all regs, scratch, mem = 0).
fn zero_state() -> StateVals {
    StateVals {
        regs: [0u8; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0u8; NUM_SCRATCH],
        mem: [0u8; MEM_WORDS],
    }
}

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

/// Substitute every `Var { operand, .. }` with `operand ∈ zero_regs` by
/// `Const(false)` — models "the ancilla started at 0". Used by the symbolic
/// Bennett check (the composed body only realizes the permutation when the
/// ancilla input is zero; symbolic_exec starts every register as a free var).
fn zero_out(e: &BvExpr, zero_regs: &[usize]) -> BvExpr {
    match e {
        BvExpr::Var { operand, .. } if zero_regs.contains(operand) => BvExpr::Const(false),
        BvExpr::Var { .. } | BvExpr::Const(_) => e.clone(),
        BvExpr::Not(a) => zero_out(a, zero_regs).not(),
        BvExpr::Xor(a, b) => zero_out(a, zero_regs).xor(zero_out(b, zero_regs)),
        BvExpr::And(a, b) => zero_out(a, zero_regs).and(zero_out(b, zero_regs)),
        BvExpr::Or(a, b) => zero_out(a, zero_regs).or(zero_out(b, zero_regs)),
        BvExpr::Mux { sel, then_, else_ } => BvExpr::mux(
            zero_out(sel, zero_regs),
            zero_out(then_, zero_regs),
            zero_out(else_, zero_regs),
        ),
    }
}

fn transfer_zero_out(t: &RegTransfer, zero_regs: &[usize]) -> RegTransfer {
    RegTransfer {
        regs: t
            .regs
            .iter()
            .map(|row| row.iter().map(|e| zero_out(e, zero_regs)).collect())
            .collect(),
    }
}

// ── Part 1: transcribe_into (fresh disjoint targets) ─────────────────

/// Operand maps that route spec regs 0..3 (the carriers) into a FRESH,
/// DISJOINT ancilla bank at regs 3,4,5: dst[r] = ancilla, src[r] = carrier.
/// Rows 3..8 are identity in the spec and, being confined to the carriers,
/// never contribute a live copy we assert on.
fn into_maps_carriers_012_ancilla_345() -> (Vec<u8>, Vec<u8>) {
    // dst: carrier row r (0,1,2) → ancilla op (3,4,5); other rows → self.
    let dst_ops: Vec<u8> = vec![3, 4, 5, 3, 4, 5, 6, 7];
    // src: identity operand map (spec Var(r,·) reads register r).
    let src_ops: Vec<u8> = (0..NUM_REGS as u8).collect();
    (dst_ops, src_ops)
}

/// `transcribe_into` fresh disjoint ancilla: the Toffoli forward, confined to
/// carriers r0,r1,r2, computed into fresh registers r3,r4,r5. Verified
/// symbolically: the ancilla (zeroed) equals `P` applied to the carriers.
#[test]
fn transcribe_into_toffoli_fresh_target_verifies() {
    // Spec confined to carriers 0,1,2: reg0 = a⊕(b∧c), reg1/2 identity.
    let spec_t = toffoli_fwd();
    let (dst_ops, src_ops) = into_maps_carriers_012_ancilla_345();

    let body = transcribe_into(&spec_t, &dst_ops, &src_ops).expect("transcribe_into");
    // reg0 (→ op3): op3 ^= r0 ; op3 ^= (r1∧r2). reg1 (→op4): op4 ^= r1 (copy).
    // reg2 (→op5): op5 ^= r2 (copy). Identity rows 3..8 → self-copies but their
    // dst=src (3→3,4→4,5→5,6→6,7→7) so they must be SKIPPED (dst==src no-op).
    assert_eq!(
        body,
        vec![
            Instr::Xor { rd: 3, rs: 0 },
            Instr::Ccnot { rd: 3, rs: 1, rt: 2 },
            Instr::Xor { rd: 4, rs: 1 },
            Instr::Xor { rd: 5, rs: 2 },
        ],
        "transcribe_into: copy+Ccnot for carrier rows, plain copies for the \
         identity carriers, nothing for the self-mapped rows"
    );

    // Symbolic: with the ancilla (regs 3,4,5) zeroed, they equal P(carriers).
    let got0 = transfer_zero_out(&symbolic_exec(&body).expect("symbolic_exec"), &[3, 4, 5]);
    let expected = spec(|r, b| match r {
        3 => v(0, b).xor(v(1, b).and(v(2, b))), // = a ⊕ (b∧c)
        4 => v(1, b),                            // = b
        5 => v(2, b),                            // = c
        _ => v(r, b),
    });
    equiv(&got0, &expected, BITS, &[0, 1, 2, 3, 4, 5])
        .expect("transcribe_into result must equal P(carriers) on the fresh ancilla");
}

/// TEETH for transcribe_into: a corrupted expected spec must FAIL equiv.
#[test]
fn transcribe_into_teeth() {
    let spec_t = toffoli_fwd();
    let (dst_ops, src_ops) = into_maps_carriers_012_ancilla_345();
    let body = transcribe_into(&spec_t, &dst_ops, &src_ops).unwrap();
    let got0 = transfer_zero_out(&symbolic_exec(&body).unwrap(), &[3, 4, 5]);
    // Corrupted: ancilla reg3 = a ⊕ b (drop the AND) — a real difference.
    let corrupted = spec(|r, b| match r {
        3 => v(0, b).xor(v(1, b)),
        4 => v(1, b),
        5 => v(2, b),
        _ => v(r, b),
    });
    assert!(
        equiv(&got0, &corrupted, BITS, &[0, 1, 2, 3, 4, 5]).is_err(),
        "corrupted spec must FAIL equiv (teeth)"
    );
}

// ── Part 2/3: the Bennett wrapper ─────────────────────────────────────

/// Symbolic Bennett check: the composed body, with the ancilla inputs zeroed,
/// realizes the forward permutation on the carriers and returns the ancilla to
/// 0. Carriers = regs 0..n, ancilla = regs n..2n (all registers so the
/// symbolic model applies; the swap step emits `Exch`, which symbolic_exec
/// supports).
fn bennett_symbolic_check(fwd: &RegTransfer, bwd: &RegTransfer, n: usize) {
    let carrier_ops: Vec<u8> = (0..n as u8).collect();
    let ancilla_ops: Vec<u8> = (n as u8..2 * n as u8).collect();
    let body = transcribe_bennett(fwd, bwd, &carrier_ops, &ancilla_ops)
        .expect("transcribe_bennett");

    let got = symbolic_exec(&body).expect("symbolic_exec of Bennett body");
    // The permutation is realized only when the ancilla started at 0.
    let ancilla_regs: Vec<usize> = (n..2 * n).collect();
    let got0 = transfer_zero_out(&got, &ancilla_regs);

    // Expected: carriers (regs 0..n) = fwd rows; ancilla (regs n..2n) = 0;
    // untouched regs identity.
    let expected = RegTransfer {
        regs: (0..NUM_REGS)
            .map(|r| {
                if r < n {
                    fwd.regs[r].clone()
                } else if (n..2 * n).contains(&r) {
                    vec![BvExpr::Const(false); BITS]
                } else {
                    (0..BITS).map(|b| v(r, b)).collect()
                }
            })
            .collect(),
    };
    let touched: Vec<usize> = (0..2 * n).collect();
    equiv(&got0, &expected, BITS, &touched)
        .expect("Bennett body (ancilla=0) must equal fwd on carriers, 0 on ancilla");
}

/// Exec / net-zero check: run the Bennett body via `exec_instr` from a sample
/// of carrier states (ancilla = 0) and assert carriers end at the concrete
/// forward permutation AND every ancilla word returns to 0.
///
/// `apply_fwd` is the ground-truth concrete permutation on the carrier bytes.
fn bennett_exec_netzero(
    fwd: &RegTransfer,
    bwd: &RegTransfer,
    carrier_ops: &[u8],
    ancilla_ops: &[u8],
    samples: &[Vec<u8>],
    apply_fwd: impl Fn(&[u8]) -> Vec<u8>,
) {
    let n = carrier_ops.len();
    let body = transcribe_bennett(fwd, bwd, carrier_ops, ancilla_ops)
        .expect("transcribe_bennett");

    for sample in samples {
        assert_eq!(sample.len(), n, "sample must set every carrier");
        let mut st = zero_state();
        // Load carriers; ancilla left at the default 0.
        for (i, &op) in carrier_ops.iter().enumerate() {
            st.operand_set(op, sample[i]);
        }
        // Sanity: every ancilla word is 0 before the body.
        for &a in ancilla_ops {
            assert_eq!(st.operand_get(a), 0, "ancilla must start 0");
        }

        for &instr in &body {
            exec_instr(instr, &mut st);
        }

        // Carriers must equal the concrete forward permutation.
        let want = apply_fwd(sample);
        for (i, &op) in carrier_ops.iter().enumerate() {
            assert_eq!(
                st.operand_get(op),
                want[i],
                "carrier op {op} wrong for sample {sample:?}: got {}, want {}",
                st.operand_get(op),
                want[i]
            );
        }
        // NET-ZERO: every ancilla word back to 0.
        for &a in ancilla_ops {
            assert_eq!(
                st.operand_get(a),
                0,
                "ancilla op {a} not restored to 0 for sample {sample:?} (Bennett net-zero)"
            );
        }
    }
}

/// Forward spec: 2-register swap `(a,b) ↦ (b,a)`. Involution ⇒ bwd = fwd.
fn swap_fwd() -> RegTransfer {
    spec(|r, b| match r {
        0 => v(1, b),
        1 => v(0, b),
        _ => v(r, b),
    })
}

/// Forward spec: byte-parallel Toffoli `(a,b,c) ↦ (a ⊕ (b∧c), b, c)`.
/// Involution ⇒ bwd = fwd.
fn toffoli_fwd() -> RegTransfer {
    spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b).and(v(2, b))),
        _ => v(r, b),
    })
}

#[test]
fn bennett_swap_symbolic() {
    bennett_symbolic_check(&swap_fwd(), &swap_fwd(), 2);
}

#[test]
fn bennett_toffoli_symbolic() {
    bennett_symbolic_check(&toffoli_fwd(), &toffoli_fwd(), 3);
}

#[test]
fn bennett_swap_netzero_register_ancilla() {
    // carriers regs 0,1 ; ancilla regs 2,3.
    bennett_exec_netzero(
        &swap_fwd(),
        &swap_fwd(),
        &[0, 1],
        &[2, 3],
        &[vec![0x00, 0x00], vec![0xA5, 0x3C], vec![0xFF, 0x01], vec![0x12, 0x34]],
        |x| vec![x[1], x[0]],
    );
}

#[test]
fn bennett_swap_netzero_scratch_ancilla() {
    // carriers regs 0,1 ; ancilla SCRATCH s2,s3 (operand idx 10,11 — s0/s1
    // reserved). The swap step emits Xchg (reg↔scratch); exec_instr supports it.
    bennett_exec_netzero(
        &swap_fwd(),
        &swap_fwd(),
        &[0, 1],
        &[10, 11],
        &[vec![0x00, 0x00], vec![0xA5, 0x3C], vec![0xFF, 0x01]],
        |x| vec![x[1], x[0]],
    );
}

#[test]
fn bennett_toffoli_netzero_register_ancilla() {
    // carriers regs 0,1,2 ; ancilla regs 3,4,5.
    bennett_exec_netzero(
        &toffoli_fwd(),
        &toffoli_fwd(),
        &[0, 1, 2],
        &[3, 4, 5],
        &[
            vec![0x00, 0x00, 0x00],
            vec![0x00, 0xFF, 0xFF],
            vec![0xA5, 0x0F, 0xF0],
            vec![0x12, 0xFF, 0x34],
            vec![0xFF, 0xFF, 0xFF],
        ],
        |x| vec![x[0] ^ (x[1] & x[2]), x[1], x[2]],
    );
}

#[test]
fn bennett_toffoli_netzero_scratch_ancilla() {
    // carriers regs 0,1,2 ; ancilla scratch s2,s3,s4 (operand idx 10,11,12).
    bennett_exec_netzero(
        &toffoli_fwd(),
        &toffoli_fwd(),
        &[0, 1, 2],
        &[10, 11, 12],
        &[
            vec![0x00, 0x00, 0x00],
            vec![0x00, 0xFF, 0xFF],
            vec![0xA5, 0x0F, 0xF0],
            vec![0xFF, 0xFF, 0xFF],
        ],
        |x| vec![x[0] ^ (x[1] & x[2]), x[1], x[2]],
    );
}

/// TEETH for the Bennett exec path: a WRONG backward spec must break net-zero
/// (the ancilla will NOT return to 0). Proves the exec check is real.
#[test]
fn bennett_wrong_backward_breaks_netzero() {
    // Correct fwd = Toffoli; WRONG bwd = identity (not P⁻¹). Then step 3 XORs
    // P⁻¹=identity(y) = y onto O(=x): O = x ⊕ y ≠ 0 in general.
    let fwd = toffoli_fwd();
    let wrong_bwd = RegTransfer::identity();
    let carrier_ops = [0u8, 1, 2];
    let ancilla_ops = [3u8, 4, 5];
    let body = transcribe_bennett(&fwd, &wrong_bwd, &carrier_ops, &ancilla_ops).unwrap();

    // A sample where the Toffoli actually flips a bit (b=c=0xFF, a=0x00) so
    // carriers change ⇒ O = x ⊕ y is nonzero.
    let mut st = zero_state();
    st.operand_set(0, 0x00);
    st.operand_set(1, 0xFF);
    st.operand_set(2, 0xFF);
    for &instr in &body {
        exec_instr(instr, &mut st);
    }
    let any_nonzero = ancilla_ops.iter().any(|&a| st.operand_get(a) != 0);
    assert!(
        any_nonzero,
        "a wrong backward spec must leave the ancilla dirty (teeth on net-zero)"
    );
}

/// Edge: carrier/ancilla count mismatch (ancilla bank too small) → Err.
#[test]
fn bennett_ancilla_too_small_errs() {
    let fwd = toffoli_fwd();
    let res = transcribe_bennett(&fwd, &fwd, &[0, 1, 2], &[3, 4]);
    assert!(res.is_err(), "too-few ancilla words must return Err, got {res:?}");
}

/// Edge: a permutation that touches a non-carrier register is rejected (the
/// Bennett wrapper only handles permutations confined to the carriers).
#[test]
fn bennett_spec_outside_carriers_errs() {
    // fwd touches reg 5, but n = 2 carriers ⇒ reg 5 is non-carrier.
    let fwd = spec(|r, b| match r {
        0 => v(1, b),
        1 => v(0, b),
        5 => v(5, b).xor(BvExpr::Const(true)),
        _ => v(r, b),
    });
    let res = transcribe_bennett(&fwd, &fwd, &[0, 1], &[2, 3]);
    assert!(
        res.is_err(),
        "a spec touching a non-carrier reg must return Err, got {res:?}"
    );
}
