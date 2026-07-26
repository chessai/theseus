//! A-6.2 — synthesize the gcd2 step through the GENERAL mechanism.
//!
//! No hand-walk: the pinned GCD rule (`gcd2_hist_bytes`, copied verbatim below
//! as test data) is fed through `synth::table_to_regtransfer` (per-output-bit
//! ANF / Möbius transform) to produce a symbolic `RegTransfer`, then through the
//! A-6.1 `transcribe_bennett` wrapper to produce the reversible body. The body
//! is EXEC-VERIFIED on all 256 canonical states (carriers land at the rule's
//! output AND every scratch ancilla returns to 0 — net-zero).
//!
//! This is DETERMINISTIC construction (ANF + the existing transcriber), NOT
//! search. It reuses the `emit_verify_gcd2` oracle pattern (run the body through
//! `exec_instr` over all 256 states), which is the authority the symbolic oracle
//! cannot provide for scratch ancilla.

use theseus_tisc::asm::{encode, AsmError, Instr};
use theseus_tisc::bveq::{equiv, symbolic_exec, BvExpr, RegTransfer, BITS};
use theseus_tisc::compile::exec_instr;
use theseus_tisc::synth::{
    table_to_regtransfer, transcribe_bennett_general, transcribe_inplace, HelperBank,
};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

// ── The pinned GCD rule (copied verbatim from compile.rs `gcd2_hist_bytes`
//    on branch a1-gcd; test DATA, not a re-derivation). Carrier byte order is
//    [tag, a, b, h]. This is the concrete permutation the synthesizer must
//    reproduce. It is NOT modified — it is the ground-truth spec. ─────────────
fn gcd2_hist_bytes(bb: &mut [u8]) {
    let (t, a, b, h) = (bb[0], bb[1], bb[2], bb[3]);
    let d = a != b && a != 0 && b != 0;
    let out: [u8; 4] = if t == 1 {
        if h == 0 {
            [0, a, b, 1] // A1: entry, sentinel h=1
        } else if a == 1 && b == 1 && h >= 2 {
            let p: [(u8, u8); 6] = [(1, 2), (2, 1), (1, 3), (3, 1), (2, 3), (3, 2)];
            let (x, y) = p[(h - 2) as usize];
            [1, x, y, 0]
        } else {
            [t, a, b, h] // A2: junk entries park
        }
    } else if !d && h == 1 {
        [1, a, b, 0] // A3: zE exit
    } else if a == 1 && b == 1 && h >= 2 {
        [1, a, b, h] // A3: real gcd exit
    } else if d && (1..=3).contains(&h) {
        let (x, y, s) = if a > b { (a - b, b, 0) } else { (b - a, a, 1) };
        [0, x, y, 2 * h + s] // A4: normalize-subtract
    } else if ((a, b) == (2, 1) || (a, b) == (1, 2)) && h >= 4 {
        let (x, y) = if (a, b) == (2, 1) {
            if h < 6 { (1, 3) } else { (3, 1) }
        } else if h < 6 {
            (2, 3)
        } else {
            (3, 2)
        };
        [0, x, y, 2 + (h & 1)] // Y-remap
    } else {
        [t, a, b, h] // id (junk)
    };
    bb[..4].copy_from_slice(&out);
}

/// Canonical carrier layout (matches `gcd2_walk_verify_canonical`):
///   leaf order [tag, a, b, h] → PHYSICAL registers [3, 0, 1, 2].
/// Widths: tag = 1 bit, a = 2, b = 2, h = 3 (Σ = 8 → 2^8 = 256 rows).
///
/// The ANF spec is built in LEAF SPACE (`SPEC_LEAVES = [0,1,2,3]`, i.e. leaf i
/// occupies symbolic register i); `transcribe_bennett`'s `carrier_ops = PHYS`
/// remaps each leaf to its physical register. (The A-6.1 wrapper reads carriers
/// from `carrier_ops[leaf]` and expects a leaf-indexed spec.)
const PHYS: [u8; 4] = [3, 0, 1, 2];
const SPEC_LEAVES: [u8; 4] = [0, 1, 2, 3];
const WIDTHS: [u8; 4] = [1, 2, 2, 3];

/// All 256 canonical carrier states [t, a, b, h] with t∈0..2, a∈0..4, b∈0..4,
/// h∈0..8 (exactly the meaningful-bit assignments the ANF enumerates).
fn canonical_states() -> Vec<[u8; 4]> {
    let mut v = Vec::with_capacity(256);
    for t in 0..2u8 {
        for a in 0..4u8 {
            for b in 0..4u8 {
                for h in 0..8u8 {
                    v.push([t, a, b, h]);
                }
            }
        }
    }
    v
}

/// The forward map as a `Vec<u8>`-closure over carrier bytes (leaf order).
fn gcd2_fwd(inb: &[u8]) -> Vec<u8> {
    let mut bb = [inb[0], inb[1], inb[2], inb[3]];
    gcd2_hist_bytes(&mut bb);
    bb.to_vec()
}

/// The INVERSE map, built by inverting the 256-entry bijection: invert[out] = in.
/// Panics if the rule is not injective on the canonical domain (guarding the
/// Bennett wrapper's `bwd = fwd⁻¹` assumption at construction time).
fn build_inverse() -> impl Fn(&[u8]) -> Vec<u8> {
    use std::collections::HashMap;
    let mut inv: HashMap<[u8; 4], [u8; 4]> = HashMap::new();
    for s in canonical_states() {
        let mut o = s;
        gcd2_hist_bytes(&mut o);
        assert!(
            inv.insert(o, s).is_none(),
            "gcd2_hist_bytes is NOT injective: output {o:?} produced by two inputs"
        );
    }
    move |inb: &[u8]| {
        let key = [inb[0], inb[1], inb[2], inb[3]];
        inv.get(&key)
            .copied()
            .expect("inverse queried outside the canonical image")
            .to_vec()
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

// ── Part 1: converter sanity (small known maps) ─────────────────────────────

/// A 2-bit swap of two 1-leaf registers, synthesized via ANF, must equal the
/// hand-written swap spec symbolically. Carrier = regs 0,1 (full 8-bit leaves,
/// but only bit0 meaningful here → we use full width 8 so equiv is over bytes).
#[test]
fn converter_swap_matches_spec() {
    // map: (x, y) ↦ (y, x) over full bytes (width 8 each) — a linear permutation.
    let fwd = table_to_regtransfer(|b| vec![b[1], b[0]], &[0, 1], &[8, 8]);
    // transcribe_inplace realizes a whole-register swap? No — a swap is not
    // in-place `old ⊕ f`. So verify the RegTransfer itself equals the spec via
    // its bit expressions (ANF of a pure copy is a single Var per bit).
    let expected = {
        let mut t = RegTransfer::identity();
        for b in 0..BITS {
            t.regs[0][b] = BvExpr::var(1, b);
            t.regs[1][b] = BvExpr::var(0, b);
        }
        t
    };
    equiv(&fwd, &expected, BITS, &[0, 1]).expect("ANF swap must equal the swap spec");
}

/// An AND-mix `reg0 ^= reg1 & reg2` (in-place Toffoli) synthesized via ANF must
/// (a) equal the spec symbolically and (b) transcribe in place to a single
/// Ccnot, verified by the symbolic oracle round-trip.
#[test]
fn converter_and_mix_transcribes_and_verifies() {
    // map over 3 full-byte leaves regs 0,1,2: reg0' = reg0 ^ (reg1 & reg2).
    let fwd = table_to_regtransfer(
        |b| vec![b[0] ^ (b[1] & b[2]), b[1], b[2]],
        &[0, 1, 2],
        &[8, 8, 8],
    );
    // (a) symbolic equality to the hand spec.
    let expected = {
        let mut t = RegTransfer::identity();
        for b in 0..BITS {
            t.regs[0][b] = BvExpr::var(0, b).xor(BvExpr::var(1, b).and(BvExpr::var(2, b)));
        }
        t
    };
    equiv(&fwd, &expected, BITS, &[0, 1, 2]).expect("ANF AND-mix must equal the spec");

    // (b) it is in-place realizable → transcribe + oracle round-trip.
    let body = transcribe_inplace(&fwd).expect("AND-mix is in-place (a single Ccnot)");
    let got = symbolic_exec(&body).expect("symbolic_exec");
    equiv(&got, &fwd, BITS, &[0, 1, 2]).expect("transcribed body must equal the ANF spec");
}

/// TEETH: a corrupted expected spec must FAIL equiv against the ANF result.
#[test]
fn converter_teeth() {
    let fwd = table_to_regtransfer(
        |b| vec![b[0] ^ (b[1] & b[2]), b[1], b[2]],
        &[0, 1, 2],
        &[8, 8, 8],
    );
    let corrupted = {
        let mut t = RegTransfer::identity();
        for b in 0..BITS {
            // drop the AND: reg0' = reg0 ^ reg1 (a real difference)
            t.regs[0][b] = BvExpr::var(0, b).xor(BvExpr::var(1, b));
        }
        t
    };
    assert!(
        equiv(&fwd, &corrupted, BITS, &[0, 1, 2]).is_err(),
        "corrupted spec must FAIL equiv (teeth)"
    );
}

// ── Part 2: gcd2 synthesis ──────────────────────────────────────────────────

/// The ANF forward spec reproduces `gcd2_hist_bytes` on all 256 states — a table
/// cross-check of the converter against the ground-truth rule (evaluating each
/// per-bit BvExpr on the concrete input assignment).
#[test]
fn gcd2_anf_matches_rule_over_256() {
    let fwd = table_to_regtransfer(gcd2_fwd, &SPEC_LEAVES, &WIDTHS);
    for s in canonical_states() {
        // Concrete symbolic-register values: leaf i → symbolic reg SPEC_LEAVES[i].
        let mut regvals = [0u8; NUM_REGS];
        for (i, &r) in SPEC_LEAVES.iter().enumerate() {
            regvals[r as usize] = s[i];
        }
        // Evaluate each carrier leaf's output bits from the ANF expressions.
        let mut got = [0u8; 4];
        for (i, &r) in SPEC_LEAVES.iter().enumerate() {
            let w = WIDTHS[i] as usize;
            let mut byte = 0u8;
            for b in 0..w {
                if eval_bit(&fwd.regs[r as usize][b], &regvals) {
                    byte |= 1 << b;
                }
            }
            got[i] = byte;
        }
        let mut exp = s;
        gcd2_hist_bytes(&mut exp);
        assert_eq!(got, exp, "ANF spec mismatch on {s:?}");
    }
}

/// Evaluate a BvExpr on concrete register bytes (bit `bit` of operand `operand`).
fn eval_bit(e: &BvExpr, regvals: &[u8]) -> bool {
    match e {
        BvExpr::Var { operand, bit } => (regvals[*operand] >> bit) & 1 == 1,
        BvExpr::Const(c) => *c,
        BvExpr::Not(a) => !eval_bit(a, regvals),
        BvExpr::Xor(a, b) => eval_bit(a, regvals) ^ eval_bit(b, regvals),
        BvExpr::And(a, b) => eval_bit(a, regvals) & eval_bit(b, regvals),
        BvExpr::Or(a, b) => eval_bit(a, regvals) | eval_bit(b, regvals),
        BvExpr::Mux { sel, then_, else_ } => {
            if eval_bit(sel, regvals) {
                eval_bit(then_, regvals)
            } else {
                eval_bit(else_, regvals)
            }
        }
    }
}

/// CROSS-CHECK: `bwd` is a true inverse of `fwd` on all 256 states BEFORE
/// transcribing — compose the two byte maps and assert identity. This guards the
/// Bennett wrapper's `bwd = fwd⁻¹` assumption.
#[test]
fn gcd2_bwd_is_inverse_of_fwd_256() {
    let inv = build_inverse();
    let mut ok = 0usize;
    for s in canonical_states() {
        let f = gcd2_fwd(&s);
        let back = inv(&f);
        assert_eq!(back, s.to_vec(), "bwd∘fwd ≠ id on {s:?}: got {back:?}");
        // also fwd∘bwd = id (bwd's domain is the image, which = full domain here)
        let g = inv(&s);
        let fwd_g = gcd2_fwd(&g);
        assert_eq!(fwd_g, s.to_vec(), "fwd∘bwd ≠ id on {s:?}");
        ok += 1;
    }
    assert_eq!(ok, 256, "bwd verified inverse on all 256 states");
    eprintln!("gcd2 bwd verified as fwd⁻¹ on {ok}/256 states");
}

/// General emitter sanity: on a small identity + degree-2 (Toffoli) map, the
/// fresh targets end at `P(src)` and every helper (mask + temp) is net-zero.
/// Also exercises the self-copy skip (identity rows 3..8 self-map → no gates).
#[test]
fn general_emitter_small_map_netzero() {
    use theseus_tisc::synth::{transcribe_into_general, HelperBank};
    // map over regs 0,1,2 (width 8): reg0'=reg0^(reg1&reg2), reg1'=reg1, reg2'=reg2
    let fwd = table_to_regtransfer(
        |b| vec![b[0] ^ (b[1] & b[2]), b[1], b[2]],
        &[0, 1, 2],
        &[8, 8, 8],
    );
    // dst = fresh regs 3,4,5 for carriers; identity rows 3..8 self-map (dst==src
    // → skipped). Helpers in scratch (s2=10 mask, s3=11 temp) so they never
    // collide with a dst/src register.
    let dst: Vec<u8> = vec![3, 4, 5, 3, 4, 5, 6, 7];
    let src: Vec<u8> = (0..8u8).collect();
    let temps = [11u8];
    let helpers = HelperBank { mask: 10, temps: &temps };
    let body = transcribe_into_general(&fwd, &dst, &src, helpers).expect("emit");
    // exec check: for a few concrete states, ancilla 3,4,5 = P(regs 0,1,2), and
    // helpers 6,7 net-zero.
    for &(r0, r1, r2) in &[(0u8, 0u8, 0u8), (0xFF, 0x0F, 0xF0), (0xAA, 0xFF, 0xFF), (0x12, 0x34, 0x56)] {
        let mut st = zero_state();
        st.operand_set(0, r0);
        st.operand_set(1, r1);
        st.operand_set(2, r2);
        for &ins in &body {
            exec_instr(ins, &mut st);
        }
        assert_eq!(st.operand_get(3), r0 ^ (r1 & r2), "reg3 wrong for {r0:x},{r1:x},{r2:x}");
        assert_eq!(st.operand_get(4), r1);
        assert_eq!(st.operand_get(5), r2);
        assert_eq!(st.operand_get(10), 0, "mask leak");
        assert_eq!(st.operand_get(11), 0, "temp leak");
    }
}

/// ANF STATISTICS (the budget finding): report the max monomial degree and max
/// per-operand multiplicity across the fwd+bwd specs. gcd2 reaches degree 7 —
/// the reversible AND-ladder therefore needs 6 accumulators + 1 mask = 7 helper
/// words, which (with 4 output ancilla) exceeds the 10 non-carrier operands.
#[test]
fn gcd2_anf_degree_report() {
    fn flat_xor<'a>(e: &'a BvExpr, out: &mut Vec<&'a BvExpr>) {
        match e {
            BvExpr::Xor(a, b) => {
                flat_xor(a, out);
                flat_xor(b, out);
            }
            BvExpr::Const(false) => {}
            other => out.push(other),
        }
    }
    fn deg_ops(t: &BvExpr, ops: &mut Vec<usize>) {
        match t {
            BvExpr::And(a, b) => {
                deg_ops(a, ops);
                deg_ops(b, ops);
            }
            BvExpr::Var { operand, .. } => ops.push(*operand),
            _ => {}
        }
    }
    let fwd = table_to_regtransfer(gcd2_fwd, &SPEC_LEAVES, &WIDTHS);
    let inv = build_inverse();
    let bwd = table_to_regtransfer(inv, &SPEC_LEAVES, &WIDTHS);
    let mut max_deg = 0usize;
    let mut max_rep = 0usize;
    for t in [&fwd, &bwd] {
        for row in &t.regs {
            for e in row {
                let mut terms = Vec::new();
                flat_xor(e, &mut terms);
                for term in terms {
                    let mut ops = Vec::new();
                    deg_ops(term, &mut ops);
                    max_deg = max_deg.max(ops.len());
                    ops.sort_unstable();
                    let mut run = 1usize;
                    for w in ops.windows(2) {
                        if w[0] == w[1] {
                            run += 1;
                            max_rep = max_rep.max(run);
                        } else {
                            run = 1;
                        }
                    }
                }
            }
        }
    }
    eprintln!("gcd2 ANF: max monomial degree = {max_deg}, max operand multiplicity = {max_rep}");
    assert_eq!(max_deg, 7, "gcd2 per-bit ANF reaches degree 7");
}

/// THE AUTHORITY: synthesize the full Bennett body from the ANF fwd/bwd specs
/// and exec-verify on all 256 states — carriers land at `gcd2_hist_bytes` AND
/// every scratch ancilla word returns to 0 (net-zero). Report body.len() + B.
#[test]
fn gcd2_bennett_exec_verify_256_and_report() {
    let fwd = table_to_regtransfer(gcd2_fwd, &SPEC_LEAVES, &WIDTHS);
    let inv = build_inverse();
    let bwd = table_to_regtransfer(inv, &SPEC_LEAVES, &WIDTHS);

    // Output ancilla = scratch words s2,s3,s4,s5 (operand idx 10..14) — one per
    // carrier byte. Bennett requires zero-init ancilla disjoint from carriers.
    let ancilla: [u8; 4] = [10, 11, 12, 13];
    // General-emitter helper bank (zero-init, disjoint): a mask word + AND-tree
    // accumulator temps. FINDING: gcd2's per-output-bit ANF reaches DEGREE 7
    // (measured — every carrier bit has a degree-7 monomial), so the reversible
    // AND-ladder needs degree-1 = 6 concurrent accumulators PLUS 1 mask = 7
    // helper words, ON TOP OF the 4 output ancilla. The non-carrier operand
    // budget is only 10 (regs r4..r7 + scratch s2..s7), so 4 + 7 = 11 EXCEEDS
    // it by one. For this fast-`exec_instr` proof we borrow s0 (operand 8) as
    // the 7th helper; s0/s1 are decoder-RESERVED on the REAL pipeline, so the
    // real system_step GCD run (A-6.5) genuinely REQUIRES widen-state (A-6.4) —
    // this is the budget wall the design predicted, now quantified.
    //   mask = r4 ; temps = r5,r6,r7, s6(14), s7(15), s0(8).
    let temps = [5u8, 6u8, 7u8, 14u8, 15u8, 8u8];
    let helpers = HelperBank { mask: 4, temps: &temps };
    let body = transcribe_bennett_general(&fwd, &bwd, &PHYS, &ancilla, helpers)
        .expect("transcribe_bennett_general of the ANF gcd2 specs");

    let len = body.len();
    let b_budget = 4 + 5 * (len + 10);
    eprintln!("=== A-6.2 GCD synthesis (ANF → Bennett) ===");
    eprintln!("body.len() = {len}");
    eprintln!("B = 4 + 5*(len+10) = {b_budget}");
    eprintln!("(B vs 256: {})", if b_budget > 256 { "EXCEEDS 256 (needs widen-PC / A-6.4)" } else { "fits in 256" });

    let mut pass = 0usize;
    for s in canonical_states() {
        let mut st = zero_state();
        // Load carriers into their PHYSICAL registers; ancilla left at default 0.
        for (i, &r) in PHYS.iter().enumerate() {
            st.operand_set(r, s[i]);
        }
        for &a in &ancilla {
            assert_eq!(st.operand_get(a), 0, "ancilla must start 0");
        }

        for &ins in &body {
            exec_instr(ins, &mut st);
        }

        // Carriers must equal the concrete rule.
        let mut exp = s;
        gcd2_hist_bytes(&mut exp);
        let got = [
            st.operand_get(PHYS[0]),
            st.operand_get(PHYS[1]),
            st.operand_get(PHYS[2]),
            st.operand_get(PHYS[3]),
        ];
        assert_eq!(
            got, exp,
            "carrier mismatch on [t,a,b,h]={s:?}: got {got:?} expected {exp:?}"
        );
        // Net-zero: every output-ancilla word restored to 0.
        for &a in &ancilla {
            assert_eq!(
                st.operand_get(a),
                0,
                "ancilla op {a} not restored to 0 on {s:?} (Bennett net-zero violated)"
            );
        }
        // Net-zero: every general-emitter helper word (mask + all temps)
        // restored to 0.
        let mut helper_words = vec![helpers.mask];
        helper_words.extend_from_slice(&temps);
        for &hw in &helper_words {
            assert_eq!(
                st.operand_get(hw),
                0,
                "helper op {hw} not restored to 0 on {s:?} (general-emitter net-zero violated)"
            );
        }
        pass += 1;
    }
    assert_eq!(pass, 256, "all 256 states must pass carrier + net-zero");
    eprintln!("exec-verify: {pass}/256 states — carriers correct AND ancilla net-zero");
}

/// True iff an encode failure is SOLELY due to the borrowed decoder-reserved
/// scratch s0/s1 (operand 8/9) — the KNOWN, documented budget wall the gcd2
/// synthesis hits (MAJOR-3 / A-6.4 widen-state). Any OTHER encode failure
/// (non-distinct Ccnot, out-of-range, aliased regs) is a real bug.
fn is_reserved_scratch_only(e: &AsmError) -> bool {
    matches!(
        e,
        AsmError::OperandReservedScratch { .. } | AsmError::XchgScratchReserved { .. }
    )
}

/// MAJOR-3(1): run `asm::encode` on EVERY emitted instruction of the gcd2
/// Bennett body. The permissive `exec_instr` (used by the 256-state authority)
/// silently runs ISA-illegal gates; encode is the assemblability gate. This body
/// is EXPECTED to have encode failures — but ONLY the documented reserved-s0
/// borrow (operand 8, the 7th helper the operand budget cannot fit). Assert
/// every failure is that known wall and NOTHING else (no non-distinct Ccnot, no
/// out-of-range, no aliased operands slipping through).
#[test]
fn gcd2_body_encode_only_reserved_scratch_wall() {
    let fwd = table_to_regtransfer(gcd2_fwd, &SPEC_LEAVES, &WIDTHS);
    let inv = build_inverse();
    let bwd = table_to_regtransfer(inv, &SPEC_LEAVES, &WIDTHS);
    let ancilla: [u8; 4] = [10, 11, 12, 13];
    let temps = [5u8, 6u8, 7u8, 14u8, 15u8, 8u8]; // s0 (op 8) is the reserved borrow
    let helpers = HelperBank { mask: 4, temps: &temps };
    let body = transcribe_bennett_general(&fwd, &bwd, &PHYS, &ancilla, helpers)
        .expect("transcribe_bennett_general");

    let mut reserved = 0usize;
    let mut ok = 0usize;
    for (i, &ins) in body.iter().enumerate() {
        match encode(ins) {
            Ok(_) => ok += 1,
            Err(ref e) if is_reserved_scratch_only(e) => reserved += 1,
            Err(e) => panic!(
                "gcd2 body instr {i} ({ins:?}) fails encode for a NON-budget reason: {e}"
            ),
        }
    }
    // The borrow is real: at least one reserved-s0 failure must exist (else the
    // budget wall the test documents has silently vanished).
    assert!(
        reserved > 0,
        "expected the documented reserved-s0 borrow to appear in the body"
    );
    eprintln!(
        "gcd2 encode-check: {ok} assemblable, {reserved} reserved-s0 (the A-6.4 widen-state wall)"
    );
}

/// MAJOR-3(1) teeth: `assert_all_encode` (all-clean expectation) is meaningful —
/// a body with a non-distinct Ccnot is rejected by encode (proving the check has
/// teeth for the register/Bennett path where NO reserved borrow is allowed).
#[test]
fn encode_check_rejects_nondistinct_ccnot() {
    let bad = Instr::Ccnot { rd: 0, rs: 0, rt: 2 }; // non-distinct → ISA-illegal
    assert!(
        encode(bad).is_err(),
        "encode must reject a non-distinct Ccnot (MAJOR-3 encode-check teeth)"
    );
}
