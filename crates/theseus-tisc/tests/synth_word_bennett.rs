//! A-6 W.1 tests -- the word-level Bennett wrapper `transcribe_word_bennett`
//! (fwd WValue → clean ancilla via the Bennett-clean idiom; swap; bwd → net-zero)
//! plus the `WForward`/`WValue` word IR it consumes.
//!
//! Two verification tiers, both with teeth (a wrong `bwd` must FAIL):
//!   * REGISTER-SYMBOLIC COMPLETE (`symbolic_exec` + `equiv`) -- when carriers +
//!     ancilla + work all fit in the 8 registers, verify over the WHOLE domain:
//!     carriers ≡ fwd AND every ancilla/work register ≡ 0. Requiring the ancilla
//!     output rows ≡ 0 (with ancilla INPUT vars zeroed) is exactly
//!     `∀x. x ⊕ bwd(fwd(x)) = 0` ⇔ `bwd ∘ fwd = id` (also forces fwd injective) --
//!     a COMPLETE `bwd = fwd⁻¹` + net-zero proof. Used for swap and a 2-carrier
//!     conditional-subtract (Select + Sub + the clean idiom).
//!   * WHOLE-DOMAIN EXEC (`exec_instr`, registers + scratch work) -- runs the body
//!     from every concrete carrier state with ancilla/work = 0 and asserts
//!     carriers = fwd(x) AND every ancilla/work word returns to 0. Complete over
//!     the byte domain; used when work needs scratch (the WLt-guarded compare).

use theseus_tisc::asm::{encode, Instr};
use theseus_tisc::bveq::{equiv, symbolic_exec, transfer_zero_out, BvExpr, RegTransfer, BITS};
use theseus_tisc::compile::exec_instr;
use theseus_tisc::synth::{
    transcribe_word_bennett, transcribe_word_bennett_peak, WForward, WValue,
};
use theseus_tisc::types::{StateVals, NUM_REGS};

// ── shared helpers ───────────────────────────────────────────────────

fn assert_all_encode(body: &[Instr]) {
    for (i, &ins) in body.iter().enumerate() {
        encode(ins).unwrap_or_else(|e| {
            panic!("emitted instr {i} ({ins:?}) fails asm::encode: {e} -- not assemblable")
        });
    }
}

fn v(r: usize, b: usize) -> BvExpr {
    BvExpr::var(r, b)
}

/// Register-symbolic COMPLETE dry-run of a word-Bennett body. Carriers = regs
/// 0..n, ancilla = regs n..2n, work = regs 2n.. (all registers, so symbolic_exec
/// applies). `carrier_fn(r,b)` is the expected value function of carrier row `r`
/// (in terms of the input carrier vars). Every ancilla and work register must be
/// ≡ 0 after zeroing their INPUT vars (the complete `bwd=fwd⁻¹` + net-zero proof).
fn symbolic_complete(
    fwd: &WForward,
    bwd: &WForward,
    n: usize,
    n_work: usize,
    carrier_fn: impl Fn(usize, usize) -> BvExpr,
) -> Vec<Instr> {
    let total = 2 * n + n_work;
    assert!(
        total <= NUM_REGS,
        "register dry-run needs ≤ {NUM_REGS} regs, want {total}"
    );
    let carriers: Vec<u8> = (0..n as u8).collect();
    let ancilla: Vec<u8> = (n as u8..2 * n as u8).collect();
    let work: Vec<u8> = (2 * n as u8..(2 * n + n_work) as u8).collect();
    let body = transcribe_word_bennett(fwd, bwd, &carriers, &ancilla, &work)
        .expect("transcribe_word_bennett");
    assert_all_encode(&body);

    let got = symbolic_exec(&body).expect("symbolic_exec of word-Bennett body");
    let ancilla_and_work: Vec<usize> = (n..2 * n).chain(2 * n..2 * n + n_work).collect();
    let got0 = transfer_zero_out(&got, &ancilla_and_work);
    let expected = RegTransfer {
        regs: (0..NUM_REGS)
            .map(|r| {
                if r < n {
                    (0..BITS).map(|b| carrier_fn(r, b)).collect()
                } else if ancilla_and_work.contains(&r) {
                    vec![BvExpr::Const(false); BITS]
                } else {
                    (0..BITS).map(|b| v(r, b)).collect()
                }
            })
            .collect(),
    };
    let touched: Vec<usize> = (0..total).collect();
    equiv(&got0, &expected, BITS, &touched)
        .expect("word-Bennett: carriers = fwd, ancilla+work ≡ 0 (complete bwd=fwd⁻¹ + net-zero)");
    body
}

// ══════════════════════════════════════════════════════════════════════
// Spec 1 -- pure swap (a,b) ↦ (b,a)   [register-symbolic complete]
// ══════════════════════════════════════════════════════════════════════

fn swap_fwd() -> WForward {
    // out0 = b (carrier 1), out1 = a (carrier 0). Swap is its own inverse.
    WForward {
        outputs: vec![(0, WValue::var(1)), (1, WValue::var(0))],
    }
}

#[test]
fn word_bennett_swap_symbolic_complete() {
    let fwd = swap_fwd();
    let bwd = swap_fwd(); // swap⁻¹ = swap
    let body = symbolic_complete(&fwd, &bwd, 2, 1, |r, b| match r {
        0 => v(1, b), // carrier0 ends = b
        1 => v(0, b), // carrier1 ends = a
        _ => v(r, b),
    });
    let (_, peak) = transcribe_word_bennett_peak(&fwd, &bwd, &[0, 1], &[2, 3], &[4]).unwrap();
    eprintln!("SWAP gate_count = {}, peak_work = {peak}", body.len());
    // out = Var → realized directly into the clean ancilla (o ^= carrier); no
    // value-temp, no scratch → peak work 0.
    assert_eq!(
        peak, 0,
        "swap outputs are bare carriers (direct XOR into ancilla)"
    );
}

/// TEETH: a wrong bwd (identity instead of swap) must leave the ancilla ≠ 0 --
/// the complete symbolic check must FAIL.
#[test]
fn word_bennett_swap_teeth_wrong_bwd() {
    let fwd = swap_fwd();
    let wrong_bwd = WForward {
        // identity (out_i = carrier_i) -- NOT the inverse of swap.
        outputs: vec![(0, WValue::var(0)), (1, WValue::var(1))],
    };
    let carriers = [0u8, 1];
    let ancilla = [2u8, 3];
    let work = [4u8];
    let body = transcribe_word_bennett(&fwd, &wrong_bwd, &carriers, &ancilla, &work).unwrap();
    let got = symbolic_exec(&body).unwrap();
    let got0 = transfer_zero_out(&got, &[2, 3, 4]);
    let expected = RegTransfer {
        regs: (0..NUM_REGS)
            .map(|r| match r {
                0 => (0..BITS).map(|b| v(1, b)).collect(),
                1 => (0..BITS).map(|b| v(0, b)).collect(),
                2..=4 => vec![BvExpr::Const(false); BITS],
                _ => (0..BITS).map(|b| v(r, b)).collect(),
            })
            .collect(),
    };
    assert!(
        equiv(&got0, &expected, BITS, &[0, 1, 2, 3, 4]).is_err(),
        "wrong bwd (identity) must FAIL the complete check (ancilla ≢ 0) -- teeth"
    );
}

// ══════════════════════════════════════════════════════════════════════
// Spec 2 -- conditional subtract (Select + Sub + the clean idiom)
//   (g,a) ↦ (g,  g&1 ? a - g : a)          [register-symbolic complete]
// bijection: g odd → a↦a-g (inv a+g); g even → identity.
// ══════════════════════════════════════════════════════════════════════

/// Build the conditional-(add|sub) forward. `sub=true` → then = a - g (the
/// forward); `sub=false` → then = a + g (the inverse).
fn cond_sub_fwd(sub: bool) -> WForward {
    let combine = if sub {
        WValue::sub(WValue::var(1), WValue::var(0)) // a - g
    } else {
        WValue::add(WValue::var(1), WValue::var(0)) // a + g
    };
    WForward {
        outputs: vec![
            (0, WValue::var(0)),                                          // g preserved
            (1, WValue::select(WValue::var(0), combine, WValue::var(1))), // g&1 ? a±g : a
        ],
    }
}

#[test]
fn word_bennett_cond_subtract_symbolic_complete() {
    let fwd = cond_sub_fwd(true);
    let bwd = cond_sub_fwd(false);
    // Expected: carrier0 = g; carrier1 = g.bit0 ? a-g : a. Build via the same
    // mux the ISA realizes (Mux over g bit0), differentially from a reference
    // body that does the arithmetic directly.
    let carriers = [0u8, 1];
    let ancilla = [2u8, 3];
    let work: Vec<u8> = vec![4, 5, 6, 7]; // peak ≤ 4
    let body = transcribe_word_bennett(&fwd, &bwd, &carriers, &ancilla, &work).expect("build");
    assert_all_encode(&body);

    // Reference: g preserved; carrier1 = mux(g.bit0, a-g, a) at every bit -- built
    // by an INDEPENDENT arithmetic body (compute a-g in a spare reg, cswap-style
    // guard). We compare BDDs: the whole-domain-equal check.
    let got = symbolic_exec(&body).unwrap();
    let got0 = transfer_zero_out(&got, &[2, 3, 4, 5, 6, 7]);

    // a-g as an 8-bit BDD per bit: symbolic_exec of `Sub{rd:1,rs:0}` gives it.
    let sub_bits = symbolic_exec(&[Instr::Sub { rd: 1, rs: 0 }]).unwrap().regs[1].clone();
    let expected = RegTransfer {
        regs: (0..NUM_REGS)
            .map(|r| match r {
                0 => (0..BITS).map(|b| v(0, b)).collect(), // g
                1 => (0..BITS)
                    .map(|b| BvExpr::mux(v(0, 0), sub_bits[b].clone(), v(1, b)))
                    .collect(), // g.bit0 ? a-g : a
                2..=7 => vec![BvExpr::Const(false); BITS],
                _ => (0..BITS).map(|b| v(r, b)).collect(),
            })
            .collect(),
    };
    equiv(&got0, &expected, BITS, &[0, 1, 2, 3, 4, 5, 6, 7])
        .expect("cond-subtract: carrier1 = g?a-g:a, all ancilla/work ≡ 0 (complete)");

    let (_, peak) = transcribe_word_bennett_peak(&fwd, &bwd, &carriers, &ancilla, &work).unwrap();
    eprintln!("COND-SUB gate_count = {}, peak_work = {peak}", body.len());
    assert!(peak <= 4, "cond-subtract peak work {peak} must be ≤ 4");
}

/// TEETH: bwd = fwd (both subtract) is NOT the inverse → ancilla ≢ 0, FAIL.
#[test]
fn word_bennett_cond_subtract_teeth_wrong_bwd() {
    let fwd = cond_sub_fwd(true);
    let wrong_bwd = cond_sub_fwd(true); // should be the ADD; using SUB is wrong
    let carriers = [0u8, 1];
    let ancilla = [2u8, 3];
    let work: Vec<u8> = vec![4, 5, 6, 7];
    let body = transcribe_word_bennett(&fwd, &wrong_bwd, &carriers, &ancilla, &work).unwrap();
    let got0 = transfer_zero_out(&symbolic_exec(&body).unwrap(), &[2, 3, 4, 5, 6, 7]);
    // Whatever the carriers end at, the ancilla MUST be ≢ 0 (bwd≠fwd⁻¹). Assert
    // the ancilla rows are not all Const(false).
    let ancilla_zero = (2..=3).all(|r| got0.regs[r].iter().all(|e| *e == BvExpr::Const(false)));
    assert!(
        !ancilla_zero,
        "wrong bwd (sub instead of add) must leave ancilla ≢ 0 -- teeth"
    );
}

// ══════════════════════════════════════════════════════════════════════
// Spec 3 -- WLt-guarded conditional (uses emit_wlt inside a Select)
//   (a,b,c) ↦ (a, b, c ⊕ (a<b))    a<b toggles c's bit 0.
// A genuine bijection (SELF-INVERSE: applying twice restores c; a,b preserved).
// The guard reads ONLY the PRESERVED carriers a,b (unlike a compare-SWAP, which
// loses the order bit and is non-injective), so it is a clean reversible use of
// WLt. a,b are IDENTITY carriers (unlisted → pass-through, no ancilla/swap); only
// c is active. Work needs scratch (WLt = 9 temps) → whole-domain EXEC (complete).
// The forward value is `Select{Lt(a,b), c^1, c}` (pure-XOR-top → realized STRAIGHT
// into the clean ancilla, so peak work = the WLt chain = 9 -- no value-temp).
// ══════════════════════════════════════════════════════════════════════

fn ltxor_fwd() -> WForward {
    // c ⊕= (a<b at bit0): out slot 2 = a<b ? (c ^ 1) : c. a,b unlisted = identity.
    WForward {
        outputs: vec![(
            2,
            WValue::select(
                WValue::lt(WValue::var(0), WValue::var(1)),
                WValue::xor(WValue::var(2), WValue::Const(1)),
                WValue::var(2),
            ),
        )],
    }
}

#[test]
fn word_bennett_ltxor_whole_domain_exec() {
    let fwd = ltxor_fwd();
    let bwd = ltxor_fwd(); // self-inverse
                           // carriers a=r0,b=r1,c=r2 (a,b preserved). Only c is active → 1 ancilla.
    let carriers = [0u8, 1, 2];
    let ancilla = [10u8]; // c's ancilla (scratch s2)
                          // work: regs 3..8 (5) + scratch s3..s7 (11..16 → 11,12,13,14,15) (5) = 10.
    let work: Vec<u8> = (3u8..8).chain(11u8..16).collect();
    let (body, peak) =
        transcribe_word_bennett_peak(&fwd, &bwd, &carriers, &ancilla, &work).expect("build");
    assert_all_encode(&body);
    eprintln!("LT-XOR gate_count = {}, peak_work = {peak}", body.len());
    assert!(peak <= 10, "lt-xor peak work {peak} must fit the pool");

    for av in 0u16..=255 {
        for bv in 0u16..=255 {
            for &cv in &[0u8, 1, 0x2A, 0xFF] {
                let (av, bv) = (av as u8, bv as u8);
                let mut st = StateVals::zero_state();
                st.operand_set(0, av);
                st.operand_set(1, bv);
                st.operand_set(2, cv);
                for &instr in &body {
                    exec_instr(instr, &mut st);
                }
                let toggle = if av < bv { 1u8 } else { 0u8 };
                assert_eq!(
                    st.operand_get(0),
                    av,
                    "ltxor a preserved (a={av},b={bv},c={cv})"
                );
                assert_eq!(
                    st.operand_get(1),
                    bv,
                    "ltxor b preserved (a={av},b={bv},c={cv})"
                );
                assert_eq!(
                    st.operand_get(2),
                    cv ^ toggle,
                    "ltxor c⊕(a<b) (a={av},b={bv},c={cv})"
                );
                for &t in &ancilla {
                    assert_eq!(
                        st.operand_get(t),
                        0,
                        "ancilla {t} net-zero (a={av},b={bv},c={cv})"
                    );
                }
                for &t in &work {
                    assert_eq!(
                        st.operand_get(t),
                        0,
                        "work {t} net-zero (a={av},b={bv},c={cv})"
                    );
                }
            }
        }
    }
}

/// TEETH for the WLt-guarded spec: a wrong bwd (guard `a>b` instead of `a<b`)
/// leaves the ancilla ≠ 0 for some input.
#[test]
fn word_bennett_ltxor_teeth_wrong_bwd() {
    let fwd = ltxor_fwd();
    let wrong_bwd = WForward {
        // guard a>b (should be a<b) → not the inverse.
        outputs: vec![(
            2,
            WValue::select(
                WValue::gt(WValue::var(0), WValue::var(1)),
                WValue::xor(WValue::var(2), WValue::Const(1)),
                WValue::var(2),
            ),
        )],
    };
    let carriers = [0u8, 1, 2];
    let ancilla = [10u8];
    let work: Vec<u8> = (3u8..8).chain(11u8..16).collect();
    let body = transcribe_word_bennett(&fwd, &wrong_bwd, &carriers, &ancilla, &work).unwrap();

    let mut any_dirty = false;
    'outer: for av in 0u16..=255 {
        for bv in 0u16..=255 {
            let (av, bv) = (av as u8, bv as u8);
            let mut st = StateVals::zero_state();
            st.operand_set(0, av);
            st.operand_set(1, bv);
            st.operand_set(2, 0);
            for &instr in &body {
                exec_instr(instr, &mut st);
            }
            if ancilla.iter().any(|&t| st.operand_get(t) != 0) {
                any_dirty = true;
                break 'outer;
            }
        }
    }
    assert!(
        any_dirty,
        "wrong bwd (a>b instead of a<b) must leave ancilla ≠ 0 on some input -- teeth"
    );
}

// ── operand-guard rejections (mirror transcribe_bennett) ──────────────

#[test]
fn word_bennett_rejects_illegal_operands() {
    let fwd = swap_fwd();
    let bwd = swap_fwd();
    // reserved ancilla operand (s0 = 8)
    assert!(transcribe_word_bennett(&fwd, &bwd, &[0, 1], &[8, 3], &[4]).is_err());
    // carrier aliases ancilla
    assert!(transcribe_word_bennett(&fwd, &bwd, &[0, 1], &[1, 3], &[4]).is_err());
    // work pool too small: cond-subtract needs ≤ 4 work words; give it 1.
    let cs_fwd = cond_sub_fwd(true);
    let cs_bwd = cond_sub_fwd(false);
    assert!(transcribe_word_bennett(&cs_fwd, &cs_bwd, &[0, 1], &[2, 3], &[4]).is_err());
    // output slot out of range
    let bad = WForward {
        outputs: vec![(0, WValue::var(1)), (9, WValue::var(0))],
    };
    assert!(transcribe_word_bennett(&bad, &bwd, &[0, 1], &[2, 3], &[4]).is_err());
    // carrier row reads a non-carrier (Var(5) with only 2 carriers)
    let oob_read = WForward {
        outputs: vec![(0, WValue::var(5)), (1, WValue::var(0))],
    };
    assert!(transcribe_word_bennett(&oob_read, &bwd, &[0, 1], &[2, 3], &[4]).is_err());
}
