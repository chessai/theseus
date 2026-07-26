//! A-6 W.0 tests — the word/arithmetic-level IR (`WOp`/`WSpec`), its 1:1
//! transcriber (`transcribe_word`), and the reversible macros
//! (`emit_wselect`, `emit_weqzero`).
//!
//! Two verification tiers, both with teeth (a corrupted expectation must FAIL):
//!   * SYMBOLIC (`symbolic_exec` + `equiv`, all-register operands, whole domain)
//!     — proves the emitted gate network equals the intended word function AND
//!     (for macros) that temps are net-zero (temp rows ≡ identity) and sources
//!     preserved. Used for the core WOps and WSelect (≤ 8 register operands).
//!   * WHOLE-DOMAIN EXEC (`exec_instr`, register carriers + scratch temps) — runs
//!     the body from every one of the 256 concrete src bytes with temps = 0 and
//!     asserts the dst bit and net-zero. Used for WEqZero (needs 11 operands, so
//!     it cannot fit the 8-register symbolic model; whole-domain exec is complete
//!     over the byte).
//!
//! `assert_all_encode` checks every emitted body assembles (catches ISA-illegal
//! gates the permissive `exec_instr` would run silently).

use theseus_tisc::asm::{encode, Instr};
use theseus_tisc::bveq::{equiv, symbolic_exec, BvExpr, RegTransfer, BITS};
use theseus_tisc::compile::exec_instr;
use theseus_tisc::synth::{
    emit_weqzero, emit_wgt, emit_wlt, emit_wselect, transcribe_word, WOp, WLT_NTEMP,
};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

// ── shared helpers ───────────────────────────────────────────────────

fn assert_all_encode(body: &[Instr]) {
    for (i, &ins) in body.iter().enumerate() {
        encode(ins).unwrap_or_else(|e| {
            panic!("emitted instr {i} ({ins:?}) fails asm::encode: {e} — not assemblable")
        });
    }
}

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

fn v(r: usize, b: usize) -> BvExpr {
    BvExpr::var(r, b)
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

/// Substitute every `Var{operand,..}` with `operand ∈ zero_regs` by
/// `Const(false)` (models "the temp started at 0"), for the dst-function check.
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

// ══════════════════════════════════════════════════════════════════════
// Part 1 — the core WOps, 1:1 transcription
// ══════════════════════════════════════════════════════════════════════

/// Each WOp maps to exactly the ISA gate the design specifies.
#[test]
fn transcribe_word_maps_1to1() {
    let spec = vec![
        WOp::WXor { d: 0, s: 1 },
        WOp::WAdd { d: 2, s: 3 },
        WOp::WSub { d: 4, s: 5 },
        WOp::WRotl { d: 6, imm: 3 },
        WOp::WRotr { d: 7, imm: 2 },
        WOp::WXorConst { d: 0, imm: 0x5A },
        WOp::WCcnot { d: 0, s: 1, t: 2 },
        WOp::WCswap { c: 3, u: 4, v: 5 },
    ];
    let body = transcribe_word(&spec).expect("transcribe_word");
    assert_eq!(
        body,
        vec![
            Instr::Xor { rd: 0, rs: 1 },
            Instr::Add { rd: 2, rs: 3 },
            Instr::Sub { rd: 4, rs: 5 },
            Instr::Rotli { rd: 6, imm: 3 },
            Instr::Rotri { rd: 7, imm: 2 },
            Instr::Xori { rd: 0, imm: 0x5A },
            Instr::Ccnot { rd: 0, rs: 1, rt: 2 },
            Instr::Cswap { rd: 3, rs: 4, rt: 5 },
        ]
    );
    assert_all_encode(&body);
}

/// Symbolic: a straight-line reversible WSpec realizes exactly its word function
/// over the WHOLE 8-bit domain (all-register operands). Covers WXor/WAdd/WSub/
/// WRotl/WRotr/WXorConst/WCcnot/WCswap composed.
#[test]
fn transcribe_word_symbolic_verifies() {
    // A reversible in-place mix touching regs 0..6:
    //   r0 ^= r1
    //   r2 += r3
    //   r4  = rotl(r4, 1)
    //   r0 ^= 0x0F
    //   r5 ^= r1 & r2'   (r2 already changed; symbolic tracks it)
    //   cswap(c=r6, r3, r4)
    let ws = vec![
        WOp::WXor { d: 0, s: 1 },
        WOp::WAdd { d: 2, s: 3 },
        WOp::WRotl { d: 4, imm: 1 },
        WOp::WXorConst { d: 0, imm: 0x0F },
        WOp::WCcnot { d: 5, s: 1, t: 2 },
        WOp::WCswap { c: 6, u: 3, v: 4 },
    ];
    let body = transcribe_word(&ws).expect("transcribe_word");
    assert_all_encode(&body);
    let got = symbolic_exec(&body).expect("symbolic_exec");
    // Independent reference: a second body built directly from the ISA gates in
    // the SAME program order (differential — `equiv` compares BDDs).
    let ref_body = vec![
        Instr::Xor { rd: 0, rs: 1 },
        Instr::Add { rd: 2, rs: 3 },
        Instr::Rotli { rd: 4, imm: 1 },
        Instr::Xori { rd: 0, imm: 0x0F },
        Instr::Ccnot { rd: 5, rs: 1, rt: 2 },
        Instr::Cswap { rd: 6, rs: 3, rt: 4 },
    ];
    let expected = symbolic_exec(&ref_body).unwrap();
    equiv(&got, &expected, BITS, &[0, 1, 2, 3, 4, 5, 6])
        .expect("transcribe_word body ≡ direct ISA body (whole domain)");
}

/// TEETH: a corrupted expectation must FAIL equiv.
#[test]
fn transcribe_word_symbolic_teeth() {
    let ws = vec![WOp::WXor { d: 0, s: 1 }, WOp::WAdd { d: 2, s: 3 }];
    let body = transcribe_word(&ws).unwrap();
    let got = symbolic_exec(&body).unwrap();
    // Corrupt: claim r0 = r0 ⊕ r2 (wrong source).
    let corrupted = spec(|r, b| match r {
        0 => v(0, b).xor(v(2, b)),
        2 => symbolic_exec(&[Instr::Add { rd: 2, rs: 3 }]).unwrap().regs[2][b].clone(),
        _ => v(r, b),
    });
    assert!(
        equiv(&got, &corrupted, BITS, &[0, 1, 2, 3]).is_err(),
        "corrupted expectation must FAIL equiv (teeth)"
    );
}

// ── operand-legality / distinctness rejections ────────────────────────

#[test]
fn transcribe_word_rejects_reserved_and_oob_and_aliased() {
    // reserved s0 (8) / s1 (9)
    assert!(transcribe_word(&[WOp::WXor { d: 8, s: 0 }]).is_err());
    assert!(transcribe_word(&[WOp::WXor { d: 0, s: 9 }]).is_err());
    // out of range ≥16
    assert!(transcribe_word(&[WOp::WXorConst { d: 16, imm: 1 }]).is_err());
    // aliased two-op (d == s)
    assert!(transcribe_word(&[WOp::WXor { d: 3, s: 3 }]).is_err());
    assert!(transcribe_word(&[WOp::WAdd { d: 3, s: 3 }]).is_err());
    // non-distinct Ccnot / Cswap (MINOR-2)
    assert!(transcribe_word(&[WOp::WCcnot { d: 0, s: 0, t: 1 }]).is_err());
    assert!(transcribe_word(&[WOp::WCswap { c: 0, u: 0, v: 1 }]).is_err());
    // a legal scratch operand (10..16) is accepted
    assert!(transcribe_word(&[WOp::WXorConst { d: 10, imm: 1 }]).is_ok());
}

// ══════════════════════════════════════════════════════════════════════
// Part 2 — WSelect (guarded word copy), symbolic
// ══════════════════════════════════════════════════════════════════════

/// Symbolic WSelect over registers: dst=0 (clean), guard=1, then=2, else=3,
/// mask=4. Verify (with dst and mask zeroed) that dst = guard?then:else, and
/// (without zeroing) that guard/then/else are preserved and mask is net-zero.
///
/// The guard is a BIT WORD; symbolic_exec models guard as a free byte, so the
/// broadcast reads only bit 0 — we verify the function under that contract by
/// checking against `Mux(Var(guard,0), then, else)` at every bit.
#[test]
fn wselect_symbolic_both_branches() {
    let (dst, guard, then_w, else_w, mask, one) = (0u8, 1u8, 2u8, 3u8, 4u8, 5u8);
    let mut body = Vec::new();
    emit_wselect(dst, guard, then_w, else_w, mask, one, &mut body).expect("emit_wselect");
    assert_all_encode(&body);

    let got = symbolic_exec(&body).expect("symbolic_exec");

    // dst FUNCTION check: with dst, mask, and one started at 0.
    let got0 = transfer_zero_out(&got, &[dst as usize, mask as usize, one as usize]);
    let expected_dst = spec(|r, b| {
        if r == dst as usize {
            // dst = guard.bit0 ? then : else
            BvExpr::mux(v(guard as usize, 0), v(then_w as usize, b), v(else_w as usize, b))
        } else if r == mask as usize || r == one as usize {
            // these temps started at 0 (we zeroed their inputs) and net-zero → 0.
            BvExpr::Const(false)
        } else {
            v(r, b)
        }
    });
    equiv(&got0, &expected_dst, BITS, &[0, 1, 2, 3, 4, 5])
        .expect("WSelect: dst = guard?then:else (both branches, whole domain)");

    // NET-ZERO + PRESERVE check: WITHOUT zeroing, mask/one return to their input
    // (identity rows) and guard/then/else are untouched (identity rows). dst is
    // allowed to change (it is the output). We check the non-dst rows equal
    // identity — this proves mask/one net-zero for ANY init and sources preserved.
    let preserved = spec(|r, b| v(r, b)); // full identity
    // Compare each preserved row: guard(1), then(2), else(3), mask(4), one(5).
    for r in [guard, then_w, else_w, mask, one] {
        let single_got = RegTransfer {
            regs: (0..NUM_REGS)
                .map(|rr| {
                    if rr == r as usize {
                        got.regs[rr].clone()
                    } else {
                        (0..BITS).map(|b| v(rr, b)).collect()
                    }
                })
                .collect(),
        };
        equiv(&single_got, &preserved, BITS, &[r as usize])
            .unwrap_or_else(|e| panic!("WSelect must preserve / net-zero operand {r}: {e:?}"));
    }
}

/// TEETH for WSelect: a corrupted dst function (swap then/else) must FAIL equiv.
#[test]
fn wselect_teeth() {
    let (dst, guard, then_w, else_w, mask, one) = (0u8, 1u8, 2u8, 3u8, 4u8, 5u8);
    let mut body = Vec::new();
    emit_wselect(dst, guard, then_w, else_w, mask, one, &mut body).unwrap();
    let got0 = transfer_zero_out(
        &symbolic_exec(&body).unwrap(),
        &[dst as usize, mask as usize, one as usize],
    );
    // Corrupt: swap then/else branches.
    let corrupted = spec(|r, b| {
        if r == dst as usize {
            BvExpr::mux(v(guard as usize, 0), v(else_w as usize, b), v(then_w as usize, b))
        } else {
            v(r, b)
        }
    });
    assert!(
        equiv(&got0, &corrupted, BITS, &[0, 1, 2, 3, 4, 5]).is_err(),
        "swapped-branch WSelect must FAIL equiv (teeth)"
    );
}

#[test]
fn wselect_rejects_illegal_operands() {
    let mut body = Vec::new();
    // mask aliases guard
    assert!(emit_wselect(0, 1, 2, 3, 1, 5, &mut body).is_err());
    // dst aliases then_w
    assert!(emit_wselect(2, 1, 2, 3, 4, 5, &mut body).is_err());
    // reserved operand
    assert!(emit_wselect(0, 1, 2, 3, 8, 5, &mut body).is_err());
    // mask aliases one
    assert!(emit_wselect(0, 1, 2, 3, 4, 4, &mut body).is_err());
}

// ══════════════════════════════════════════════════════════════════════
// Part 3 — WEqZero (is-zero predicate), whole-domain exec
// ══════════════════════════════════════════════════════════════════════

/// WEqZero over the WHOLE 256-value domain of `src`: for each src byte, run the
/// body from a clean state (dst_bit=0, all temps=0) and assert
///   dst_bit.bit0 == (src == 0)  AND  src preserved  AND  every temp back to 0.
///
/// dst_bit=r0, src=r1, temps = 9 SCRATCH words s2..s7 + regs r2,r3,r4? We need 9
/// temps; use scratch s2..s7 (idx 10..15 = 6) + regs 2,3,4 (3) = 9 words.
#[test]
fn weqzero_whole_domain_exec_and_netzero() {
    let dst_bit = 0u8;
    let src = 1u8;
    // 9 temps: regs 2,3,4 + scratch s2..s7 (operand idx 10..15).
    let temps: Vec<u8> = vec![2, 3, 4, 10, 11, 12, 13, 14, 15];
    let mut body = Vec::new();
    emit_weqzero(dst_bit, src, &temps, &mut body).expect("emit_weqzero");
    assert_all_encode(&body);

    for x in 0u16..=255 {
        let x = x as u8;
        let mut st = zero_state();
        st.operand_set(src, x);
        // sanity: all temps and dst start 0.
        assert_eq!(st.operand_get(dst_bit), 0);
        for &t in &temps {
            assert_eq!(st.operand_get(t), 0, "temp {t} must start 0");
        }
        for &instr in &body {
            exec_instr(instr, &mut st);
        }
        let want = if x == 0 { 1u8 } else { 0u8 };
        assert_eq!(
            st.operand_get(dst_bit) & 1,
            want,
            "WEqZero: is-zero(src={x:#04x}) wrong (dst.bit0={}, want {want})",
            st.operand_get(dst_bit) & 1
        );
        // dst_bit's other bits must be untouched (stayed 0).
        assert_eq!(
            st.operand_get(dst_bit) & !1u8,
            0,
            "WEqZero must touch only dst bit 0 (src={x:#04x})"
        );
        // src preserved.
        assert_eq!(st.operand_get(src), x, "WEqZero must preserve src (={x:#04x})");
        // NET-ZERO: every temp back to 0.
        for &t in &temps {
            assert_eq!(
                st.operand_get(t),
                0,
                "WEqZero temp {t} not restored to 0 (src={x:#04x})"
            );
        }
    }
}

/// TEETH for WEqZero: if we DROP the final invert (`Xori dst,1`), the predicate
/// becomes `src != 0` — i.e. the src=0 case now yields 0. A hand-built wrong body
/// must disagree with the true is-zero on at least one input (proves the exec
/// check is real, not vacuous).
#[test]
fn weqzero_teeth_wrong_body_disagrees() {
    let dst_bit = 0u8;
    let src = 1u8;
    let temps: Vec<u8> = vec![2, 3, 4, 10, 11, 12, 13, 14, 15];
    let mut body = Vec::new();
    emit_weqzero(dst_bit, src, &temps, &mut body).unwrap();
    // Corrupt: remove the FIRST `Xori{rd:dst_bit, imm:1}` in the use step (the
    // invert). Find and drop exactly one such instruction targeting dst_bit that
    // is NOT the `one` setup (dst_bit != one). This flips is-zero → nonzero.
    let mut corrupted = Vec::new();
    let mut dropped = false;
    for &ins in &body {
        if !dropped {
            if let Instr::Xori { rd, imm: 1 } = ins {
                if rd == dst_bit {
                    dropped = true;
                    continue;
                }
            }
        }
        corrupted.push(ins);
    }
    assert!(dropped, "test setup: expected an Xori dst,1 to drop");

    // src=0 should give is-zero=1; the corrupted body gives 0 → disagreement.
    let mut st = zero_state();
    st.operand_set(src, 0);
    for &instr in &corrupted {
        exec_instr(instr, &mut st);
    }
    assert_eq!(
        st.operand_get(dst_bit) & 1,
        0,
        "corrupted body (invert dropped) must NOT report is-zero for src=0 (teeth)"
    );
}

#[test]
fn weqzero_rejects_too_few_temps_and_aliases() {
    let mut body = Vec::new();
    // too few temps (need 9)
    assert!(emit_weqzero(0, 1, &[2, 3, 4, 10, 11, 12, 13, 14], &mut body).is_err());
    // aliased temp with src
    assert!(emit_weqzero(0, 1, &[1, 3, 4, 10, 11, 12, 13, 14, 15], &mut body).is_err());
    // reserved temp (s0=8)
    assert!(emit_weqzero(0, 1, &[2, 3, 4, 8, 11, 12, 13, 14, 15], &mut body).is_err());
}

// ══════════════════════════════════════════════════════════════════════
// Part 4 — WLt / WGt (unsigned compare), whole-domain exec
// ══════════════════════════════════════════════════════════════════════

/// 9 temp words for WLt/WGt: regs 2,3,4 + scratch s2..s7 (idx 10..15).
fn wlt_temps() -> Vec<u8> {
    // WLT_NTEMP == 9.
    vec![2, 3, 4, 10, 11, 12, 13, 14, 15]
}

/// WLt over the WHOLE 256×256 domain of (a,b): dst_bit.bit0 == (a<b) unsigned,
/// a/b preserved, every temp net-zero, dst other bits untouched.
#[test]
fn wlt_whole_domain_exec_and_netzero() {
    assert_eq!(WLT_NTEMP, 9);
    let (dst_bit, a, b) = (0u8, 1u8, 6u8);
    let temps = wlt_temps();
    let mut body = Vec::new();
    emit_wlt(dst_bit, a, b, &temps, &mut body).expect("emit_wlt");
    assert_all_encode(&body);

    for av in 0u16..=255 {
        for bv in 0u16..=255 {
            let (av, bv) = (av as u8, bv as u8);
            let mut st = zero_state();
            st.operand_set(a, av);
            st.operand_set(b, bv);
            for &instr in &body {
                exec_instr(instr, &mut st);
            }
            let want = if av < bv { 1u8 } else { 0u8 };
            assert_eq!(
                st.operand_get(dst_bit) & 1,
                want,
                "WLt(a={av},b={bv}) wrong: dst.bit0={}, want {want}",
                st.operand_get(dst_bit) & 1
            );
            assert_eq!(st.operand_get(dst_bit) & !1u8, 0, "WLt touched dst>bit0 (a={av},b={bv})");
            assert_eq!(st.operand_get(a), av, "WLt must preserve a (a={av},b={bv})");
            assert_eq!(st.operand_get(b), bv, "WLt must preserve b (a={av},b={bv})");
            for &t in &temps {
                assert_eq!(st.operand_get(t), 0, "WLt temp {t} not netzero (a={av},b={bv})");
            }
        }
    }
}

/// WGt(a,b) == WLt(b,a): whole-domain exec.
#[test]
fn wgt_whole_domain_exec_and_netzero() {
    let (dst_bit, a, b) = (0u8, 1u8, 6u8);
    let temps = wlt_temps();
    let mut body = Vec::new();
    emit_wgt(dst_bit, a, b, &temps, &mut body).expect("emit_wgt");
    assert_all_encode(&body);

    for av in 0u16..=255 {
        for bv in 0u16..=255 {
            let (av, bv) = (av as u8, bv as u8);
            let mut st = zero_state();
            st.operand_set(a, av);
            st.operand_set(b, bv);
            for &instr in &body {
                exec_instr(instr, &mut st);
            }
            let want = if av > bv { 1u8 } else { 0u8 };
            assert_eq!(st.operand_get(dst_bit) & 1, want, "WGt(a={av},b={bv}) wrong");
            assert_eq!(st.operand_get(a), av, "WGt must preserve a");
            assert_eq!(st.operand_get(b), bv, "WGt must preserve b");
            for &t in &temps {
                assert_eq!(st.operand_get(t), 0, "WGt temp {t} not netzero (a={av},b={bv})");
            }
        }
    }
}

/// TEETH for WLt: dropping the `use` step (every gate writing the final borrow
/// into dst) makes the predicate constant 0 — it must disagree with the true
/// `a<b` on some input. The final borrow node XOR-accumulates into `dst_bit` via
/// `Ccnot`s; removing all of them (dst is never a Ccnot target elsewhere) zeroes
/// the answer.
#[test]
fn wlt_teeth_wrong_body_disagrees() {
    let (dst_bit, a, b) = (0u8, 1u8, 6u8);
    let temps = wlt_temps();
    let mut body = Vec::new();
    emit_wlt(dst_bit, a, b, &temps, &mut body).unwrap();
    // Drop ALL `Ccnot { rd: dst_bit, .. }` (the final-borrow accumulation into
    // dst). dst_bit is never written by any other gate, so the result is 0.
    let corrupted: Vec<Instr> = body
        .iter()
        .copied()
        .filter(|ins| !matches!(ins, Instr::Ccnot { rd, .. } if *rd == dst_bit))
        .collect();
    assert!(corrupted.len() < body.len(), "test setup: expected Ccnots into dst_bit");
    // a=0,b=1 → true a<b = 1; the corrupted body yields 0 → disagreement.
    let mut st = zero_state();
    st.operand_set(a, 0);
    st.operand_set(b, 1);
    for &instr in &corrupted {
        exec_instr(instr, &mut st);
    }
    assert_eq!(
        st.operand_get(dst_bit) & 1,
        0,
        "corrupted WLt (use dropped) must NOT report a<b for (0,1) (teeth)"
    );
}

#[test]
fn wlt_rejects_too_few_temps_and_aliases() {
    let mut body = Vec::new();
    // too few temps (need 9)
    assert!(emit_wlt(0, 1, 6, &[2, 3, 4, 10, 11, 12, 13, 14], &mut body).is_err());
    // aliased temp with a
    let mut t = wlt_temps();
    t[0] = 1; // == a
    assert!(emit_wlt(0, 1, 6, &t, &mut body).is_err());
    // reserved temp (s1=9)
    let mut t = wlt_temps();
    t[4] = 9;
    assert!(emit_wlt(0, 1, 6, &t, &mut body).is_err());
}
