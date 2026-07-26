//! A-6 slice A-6.D.2 — synthesize gcd2 through the BDD path and MEASURE the body.
//!
//! The decisive efficiency test: build gcd2's fwd/bwd `RegTransfer` (the exact
//! construction reused from `synth_gcd2.rs`) and synthesize the reversible body
//! via `transcribe_bdd_bennett` (the BDD synthesizer, D.0/D.1) instead of the ANF
//! path `transcribe_bennett_general`. The headline is `body.len()` / `B` vs ANF's
//! 15,148 / 75,794, plus gcd2's per-output-bit ROBDD node counts and the required
//! node-LINE budget (max single-bit node count) vs the ~10 free operand words.
//!
//! DETERMINISTIC construction (ANF-built spec → ROBDD walk), NOT search. The
//! 256-state `exec_instr` round-trip is the authority (carriers land at the rule,
//! every ancilla/helper/node word returns to 0 — Bennett net-zero); the symbolic
//! oracle cannot model the scratch node lines.

use theseus_tisc::bveq::{expr_to_bdd, BvExpr, RegTransfer, VarMap};
use theseus_tisc::compile::exec_instr;
use theseus_tisc::synth::{table_to_regtransfer, transcribe_bdd_bennett, BddHelpers};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

// ── The pinned GCD rule (copied verbatim from `synth_gcd2.rs`, which copied it
//    verbatim from compile.rs `gcd2_hist_bytes`). Test DATA, not a re-derivation.
//    Carrier byte order is [tag, a, b, h]. ─────────────────────────────────────
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

// Canonical carrier layout (matches `synth_gcd2.rs`):
//   leaf order [tag, a, b, h] → PHYSICAL registers [3, 0, 1, 2].
//   Widths: tag = 1 bit, a = 2, b = 2, h = 3 (Σ = 8 → 2^8 = 256 rows).
// Spec is built in LEAF SPACE (leaf i occupies symbolic register i); the wrapper
// remaps leaf → physical register via carrier_ops = PHYS.
const PHYS: [u8; 4] = [3, 0, 1, 2];
const SPEC_LEAVES: [u8; 4] = [0, 1, 2, 3];
const WIDTHS: [u8; 4] = [1, 2, 2, 3];

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

fn gcd2_fwd(inb: &[u8]) -> Vec<u8> {
    let mut bb = [inb[0], inb[1], inb[2], inb[3]];
    gcd2_hist_bytes(&mut bb);
    bb.to_vec()
}

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

/// Build the gcd2 fwd/bwd RegTransfer pair (exact construction from synth_gcd2.rs).
fn gcd2_specs() -> (RegTransfer, RegTransfer) {
    let fwd = table_to_regtransfer(gcd2_fwd, &SPEC_LEAVES, &WIDTHS);
    let inv = build_inverse();
    let bwd = table_to_regtransfer(inv, &SPEC_LEAVES, &WIDTHS);
    (fwd, bwd)
}

// ── (2) Per-output-bit ROBDD node counts (the structural number) ─────────────

/// Count ROBDD nodes for one output-bit expression (excluding the two terminals),
/// using the SAME VarMap/`expr_to_bdd` path the synthesizer uses.
fn bdd_node_count(e: &BvExpr, map: &VarMap) -> usize {
    let bdd = expr_to_bdd(e, map).expect("expr_to_bdd");
    let root = bdd.root_pointer();
    if root.is_zero() || root.is_one() {
        0 // constant output bit — no non-terminal nodes
    } else {
        bdd.size() - 2
    }
}

/// Report the per-(reg,bit) ROBDD node counts across fwd+bwd: the sum (total node
/// LINES that would be needed if none were recycled across bits) and the MAX
/// single-output-bit count (the required node-line bank size, since the
/// synthesizer recycles node words across output bits but NOT within one bit).
#[test]
fn gcd2_bdd_node_counts_report() {
    let (fwd, bwd) = gcd2_specs();
    let map = VarMap::new(NUM_REGS).expect("VarMap");

    let mut sum = 0usize;
    let mut max_single = 0usize;
    let mut max_where = (String::new(), 0usize, 0usize);
    for (label, t) in [("fwd", &fwd), ("bwd", &bwd)] {
        for (r, row) in t.regs.iter().enumerate() {
            for (b, e) in row.iter().enumerate() {
                let n = bdd_node_count(e, &map);
                sum += n;
                if n > max_single {
                    max_single = n;
                    max_where = (label.to_string(), r, b);
                }
            }
        }
    }
    eprintln!("=== gcd2 ROBDD node counts (fwd+bwd, per output bit) ===");
    eprintln!("sum of all per-bit node counts = {sum}");
    eprintln!(
        "MAX single-output-bit node count = {max_single}  (at {} reg {} bit {})",
        max_where.0, max_where.1, max_where.2
    );
    eprintln!(
        "required node-LINE bank = {max_single} (words recycled across bits, NOT within a bit)"
    );
    eprintln!(
        "free operand words available ≈ 10 (r4..r7 + s2..s7, minus reserved s0/s1); need mask/temp + 4 out-ancilla too"
    );
}

/// Measure, for the worst single output bit, the STRUCTURAL floor on live node
/// lines even under an ideal recycler. Two numbers:
///   * total nodes (= the CURRENT synthesizer's live-line requirement, since it
///     computes ALL nodes forward, reads root, then uncomputes ALL — nothing is
///     freed early);
///   * max BDD LEVEL WIDTH (antichain) = the classic lower bound on the pebbling
///     width a level-recycler could hope to reach.
/// This sizes widen-STATE for A-6.4 and is honest about whether recycling helps.
#[test]
fn gcd2_bdd_cut_width_report() {
    use biodivine_lib_bdd::BddPointer;
    use std::collections::BTreeMap;
    let (fwd, bwd) = gcd2_specs();
    let map = VarMap::new(NUM_REGS).expect("VarMap");

    let mut worst_total = 0usize;
    let mut worst_level = 0usize;
    for t in [&fwd, &bwd] {
        for row in &t.regs {
            for e in row {
                let bdd = expr_to_bdd(e, &map).expect("expr_to_bdd");
                let root = bdd.root_pointer();
                if root.is_zero() || root.is_one() {
                    continue;
                }
                let total = bdd.size() - 2;
                worst_total = worst_total.max(total);
                // Level width: count non-terminal nodes per decision variable.
                let mut per_var: BTreeMap<usize, usize> = BTreeMap::new();
                for p in bdd.pointers() {
                    if p == BddPointer::zero() || p == BddPointer::one() {
                        continue;
                    }
                    *per_var.entry(bdd.var_of(p).to_index()).or_insert(0) += 1;
                }
                let max_level = per_var.values().copied().max().unwrap_or(0);
                worst_level = worst_level.max(max_level);
            }
        }
    }
    eprintln!("=== gcd2 BDD live-line budget (worst single output bit) ===");
    eprintln!("current synthesizer live lines (= total nodes, NO recycling) = {worst_total}");
    eprintln!("max BDD level width (antichain / pebbling floor) = {worst_level}");
    eprintln!(
        "operand words in the WHOLE machine = 16 (8 reg + 8 scratch); legal free ≈ 10; \
         carriers 4 + ancilla 4 + temp 1 already consume 9"
    );
}

// ── (1) THE HEADLINE: full-body BDD length via transcribe_bdd_into ────────────
//
// gcd2's worst single-output-bit ROBDD is 35 nodes (`gcd2_bdd_node_counts_report`),
// so a full-body synthesis needs a 35-word node bank. The WHOLE machine has only
// 16 operand words (8 reg + 8 scratch) and `transcribe_bdd_bennett` enforces
// pairwise-disjoint LEGAL operands (≤14 usable) — it CANNOT be handed 35 node
// lines, and `exec_instr` panics on any operand index ≥ 16. So the full body can
// neither be synthesized through the Bennett wrapper nor exec-verified on this
// machine. That is itself the decisive budget finding (see the budget test).
//
// The LENGTH, however, is a pure property of the emission and needs no execution.
// `transcribe_bdd_into` performs NO operand-range/disjointness validation (it just
// wires gate shapes onto the given indices), so we measure the full fwd/bwd body
// length by handing it a 35-word node bank at indices the emitter accepts. These
// indices are NEVER executed — they exist only to count the emitted gates.

/// Distinct node-word indices for LENGTH MEASUREMENT ONLY (never exec'd). We need
/// one per simultaneously-live node = up to the max single-bit node count (35);
/// use a generous 40-word bank at indices 16.. (out of the real operand space, so
/// there is zero chance of aliasing a real carrier/ancilla and silently passing).
fn measurement_node_bank() -> Vec<u8> {
    (16u8..16 + 40).collect()
}

/// The full BDD Bennett body length = fwd BDD synth + 4 carrier↔ancilla swaps +
/// bwd BDD synth, replicating `transcribe_bdd_bennett`'s composition but via the
/// unguarded `transcribe_bdd_into` so the 35-node bank is accepted for counting.
fn bdd_body_length() -> usize {
    use theseus_tisc::synth::transcribe_bdd_into;
    let (fwd, bwd) = gcd2_specs();

    // src_map: spec leaf r → its physical carrier (PHYS for the 4 carriers).
    // dst_map: spec leaf r → its output-ancilla word. Non-carrier rows self-map
    // (identity → skipped), matching the wrapper.
    let carriers = PHYS;
    let ancilla: [u8; 4] = [10, 11, 12, 13];
    let mut src_map = vec![0u8; NUM_REGS];
    let mut dst_map = vec![0u8; NUM_REGS];
    for r in 0..NUM_REGS {
        if r < carriers.len() {
            src_map[r] = carriers[r];
            dst_map[r] = ancilla[r];
        } else {
            src_map[r] = carriers.get(r).copied().unwrap_or(0);
            dst_map[r] = carriers.get(r).copied().unwrap_or(0);
        }
    }
    let nodes = measurement_node_bank();
    // temp at index 55 — also outside the real space, count-only.
    let helpers = BddHelpers { temp: 55, nodes: &nodes };

    let fwd_body = transcribe_bdd_into(&fwd, &dst_map, &src_map, helpers)
        .expect("transcribe_bdd_into(fwd)");
    let bwd_body = transcribe_bdd_into(&bwd, &dst_map, &src_map, helpers)
        .expect("transcribe_bdd_into(bwd)");
    // + 4 carrier↔ancilla swaps (the middle step of the Bennett wrapper).
    fwd_body.len() + 4 + bwd_body.len()
}

#[test]
fn gcd2_bdd_body_length_headline() {
    let len = bdd_body_length();
    let b_budget = 4 + 5 * (len + 10);
    eprintln!("=== A-6.D.2 GCD synthesis (BDD → Bennett) — HEADLINE ===");
    eprintln!("body.len() = {len}");
    eprintln!("B = 4 + 5*(len+10) = {b_budget}");
    eprintln!("--- vs ANF path: body.len() = 15148, B = 75794 ---");
    eprintln!(
        "BDD/ANF length ratio = {:.4}  ({:.2}x shorter)",
        len as f64 / 15148.0,
        15148.0 / len as f64
    );
    eprintln!(
        "(B vs 256: {})",
        if b_budget > 256 {
            "EXCEEDS 256 (needs widen-PC / A-6.4)"
        } else {
            "fits in 256"
        }
    );
    // Sanity floor: the body cannot be empty and must beat ANF (the whole point).
    assert!(len > 0, "BDD body must emit gates");
    assert!(
        len < 15148,
        "BDD body ({len}) must be shorter than the ANF body (15148) — else the slice failed"
    );
}

// ── (4) EXEC-VERIFY: per-output-bit BDD, exhaustively, on gcd2's real exprs ───
//
// The full 256-state whole-body exec is IMPOSSIBLE (35 node lines > 16 machine
// words). But every output bit whose ROBDD is small enough to fit
// (nodes + 1 src-slot + 1 dst + 1 temp ≤ 16 words) IS exec-verifiable, and doing
// so exhaustively over all 256 input assignments is the strongest authority the
// machine can give on gcd2's ACTUAL bit-expressions. We synthesize each fittable
// bit with `transcribe_bdd_into` onto a fresh dst, run all 256 inputs through
// `exec_instr`, and assert: (a) dst bit b = the ANF-evaluated bit, and (b) every
// node/temp word is net-zero. Bits too large to fit are reported, not skipped
// silently — that residue IS the widen-state requirement.

/// Evaluate a BvExpr on concrete per-leaf carrier bytes (leaf i in symbolic reg i).
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

#[test]
fn gcd2_bdd_per_bit_exec_verify_fittable() {
    use theseus_tisc::synth::transcribe_bdd_into;
    let (fwd, bwd) = gcd2_specs();
    let map = VarMap::new(NUM_REGS).expect("VarMap");

    // `table_to_regtransfer` builds the ANF over the FULL 8-register VarMap, so
    // gcd2's per-bit expressions may branch on ANY of operands 0..7 (the higher
    // ones hold 0 in the canonical domain but the BDD still has decision nodes for
    // them). The source words therefore occupy ALL of regs 0..7; we can only place
    // dst/temp/node words in scratch. Reserved s0/s1 (8/9) are off-limits, leaving
    // s2..s7 (10..15) = 6 scratch words: dst=10, temp=11, nodes=12,13,14,15.
    //   sources: regs 0..7 (leaves 0..3 carry [t,a,b,h]; regs 4..7 = 0)
    //   dst:     s2 (10)
    //   temp:    s3 (11)
    //   nodes:   s4..s7 (12,13,14,15) = 4 words
    // So the largest exec'able single-bit BDD is 4 nodes; larger bits are reported.
    let dst: u8 = 10;
    let temp: u8 = 11;
    let node_words: Vec<u8> = vec![12, 13, 14, 15]; // 4 words
    let max_fit = node_words.len();

    // Full 256 input assignments over the 8 meaningful bits (t,a,b,h widths).
    let states = canonical_states();

    let mut verified_bits = 0usize;
    let mut verified_states = 0usize;
    let mut skipped: Vec<(String, usize, usize, usize)> = Vec::new(); // (which,reg,bit,nodes)

    for (which, t) in [("fwd", &fwd), ("bwd", &bwd)] {
        for (r, row) in t.regs.iter().enumerate() {
            for (b, e) in row.iter().enumerate() {
                let n_nodes = bdd_node_count(e, &map);
                if n_nodes > max_fit {
                    skipped.push((which.to_string(), r, b, n_nodes));
                    continue;
                }
                // Build a spec that is identity everywhere except this one bit,
                // and synthesize ONLY this bit onto `dst` bit b.
                // We call transcribe_bdd_into on a single-row-active spec whose
                // row `r` has our expr at bit b and Const(false) elsewhere; every
                // other row is a self-mapped identity (skipped). dst_ops[r]=dst.
                let mut one = RegTransfer::identity();
                for bb in 0..one.regs[r].len() {
                    one.regs[r][bb] = if bb == b {
                        e.clone()
                    } else {
                        BvExpr::Const(false)
                    };
                }
                let mut src_map: Vec<u8> = (0..NUM_REGS as u8).collect();
                let mut dst_map: Vec<u8> = (0..NUM_REGS as u8).collect();
                // Row r writes to `dst`; all other rows self-map (identity → skip).
                dst_map[r] = dst;
                // Row r must not be treated as an identity self-map: dst != src[r].
                let _ = &mut src_map; // src self-maps; leaf reads reg==leaf index.
                let helpers = BddHelpers { temp, nodes: &node_words };
                let body = transcribe_bdd_into(&one, &dst_map, &src_map, helpers)
                    .expect("transcribe_bdd_into(single bit)");

                for s in &states {
                    // regvals: leaf i → register i (source map is identity).
                    let mut regvals = [0u8; NUM_REGS];
                    for (i, &leaf_reg) in SPEC_LEAVES.iter().enumerate() {
                        regvals[leaf_reg as usize] = s[i];
                    }
                    let mut st = zero_state();
                    for rr in 0..4usize {
                        st.operand_set(rr as u8, regvals[rr]);
                    }
                    // dst/temp/nodes start 0 (assert).
                    assert_eq!(st.operand_get(dst), 0);
                    assert_eq!(st.operand_get(temp), 0);
                    for &w in &node_words {
                        assert_eq!(st.operand_get(w), 0);
                    }
                    for &ins in &body {
                        exec_instr(ins, &mut st);
                    }
                    // dst bit b must equal the ANF-evaluated expression bit; every
                    // OTHER bit of dst must be 0 (single-bit synthesis).
                    let want = eval_bit(e, &regvals) as u8;
                    let got_dst = st.operand_get(dst);
                    assert_eq!(
                        (got_dst >> b) & 1,
                        want,
                        "{which} reg{r} bit{b}: exec bit != ANF eval on {s:?}"
                    );
                    assert_eq!(
                        got_dst & !(1u8 << b),
                        0,
                        "{which} reg{r} bit{b}: dst has spurious bits set on {s:?}"
                    );
                    // Net-zero: temp + every node word restored to 0.
                    assert_eq!(st.operand_get(temp), 0, "temp leak {which} r{r} b{b} {s:?}");
                    for &w in &node_words {
                        assert_eq!(
                            st.operand_get(w),
                            0,
                            "node word {w} leak {which} r{r} b{b} on {s:?}"
                        );
                    }
                    verified_states += 1;
                }
                verified_bits += 1;
            }
        }
    }
    eprintln!("=== gcd2 BDD per-output-bit exec-verify (fittable bits) ===");
    eprintln!(
        "exec-verified {verified_bits} output bits × 256 inputs = {verified_states} \
         state-checks (dst bit correct AND all nodes/temp net-zero)"
    );
    eprintln!(
        "bits too large to fit ({max_fit}-node machine budget), NOT exec'd: {}",
        skipped.len()
    );
    for (w, r, b, n) in &skipped {
        eprintln!("  UNFITTABLE: {w} reg {r} bit {b} — {n} nodes > {max_fit}");
    }
    assert!(verified_bits > 0, "at least some gcd2 bits must be exec-verified");
}

// ── (3) Budget verdict: node-line count vs machine words ─────────────────────

/// The BUDGET VERDICT: quantify the node-line requirement against the machine.
/// Reports total-nodes (current synthesizer, no recycling) and the antichain
/// (pebbling floor an ideal recycler could reach), and asserts that BOTH exceed
/// the free operand budget — i.e. gcd2's BDD needs widen-STATE regardless of
/// recycling. Also confirms `transcribe_bdd_bennett` REJECTS the real attempt
/// (it cannot be handed enough legal disjoint node words).
#[test]
fn gcd2_bdd_budget_verdict() {
    use biodivine_lib_bdd::BddPointer;
    use std::collections::BTreeMap;
    let (fwd, bwd) = gcd2_specs();
    let map = VarMap::new(NUM_REGS).expect("VarMap");

    let mut worst_total = 0usize;
    let mut worst_level = 0usize;
    for t in [&fwd, &bwd] {
        for row in &t.regs {
            for e in row {
                let bdd = expr_to_bdd(e, &map).expect("expr_to_bdd");
                let root = bdd.root_pointer();
                if root.is_zero() || root.is_one() {
                    continue;
                }
                worst_total = worst_total.max(bdd.size() - 2);
                let mut per_var: BTreeMap<usize, usize> = BTreeMap::new();
                for p in bdd.pointers() {
                    if p == BddPointer::zero() || p == BddPointer::one() {
                        continue;
                    }
                    *per_var.entry(bdd.var_of(p).to_index()).or_insert(0) += 1;
                }
                worst_level = worst_level.max(per_var.values().copied().max().unwrap_or(0));
            }
        }
    }

    // Fixed working words that must coexist with the node lines during synthesis:
    //   4 carriers + 4 out-ancilla + 1 temp = 9 words, leaving 16-9 = 7 for nodes.
    let free_for_nodes_realistic = 16usize - (4 + 4 + 1);
    // Legal-operand free budget (the design's "~10": r4..r7 + s2..s7 = 10, minus
    // the 4 ancilla + 1 temp when those come from scratch) — the coarse "~10".
    eprintln!("=== gcd2 BDD budget verdict ===");
    eprintln!("required node LINES, NO recycling (= total nodes)         = {worst_total}");
    eprintln!("required node LINES, ideal recycling (antichain floor)    = {worst_level}");
    eprintln!("node words the 16-word machine can spare (4c+4a+1t used)  = {free_for_nodes_realistic}");
    eprintln!(
        "VERDICT: even the antichain floor ({worst_level}) {} the {free_for_nodes_realistic} spare words → \
         gcd2 BDD needs WIDEN-STATE (A-6.4) regardless of recycling.",
        if worst_level > free_for_nodes_realistic { "EXCEEDS" } else { "fits within" }
    );

    assert!(
        worst_total > free_for_nodes_realistic,
        "no-recycling node lines must exceed the machine (else the wall vanished)"
    );
    assert!(
        worst_level > free_for_nodes_realistic,
        "even the antichain floor must exceed the machine (widen-state required)"
    );

    // The real Bennett attempt must REJECT: it cannot be given `worst_total`
    // distinct legal node words (only ~10 legal operands exist, all needed).
    let ancilla: [u8; 4] = [10, 11, 12, 13];
    // Try the most generous legal node bank we can assemble that is disjoint from
    // carriers(PHYS=3,0,1,2)+ancilla(10..14)+temp: r4,r5,r6,r7 + s7(15) = 5 words.
    let nodes: Vec<u8> = vec![4, 5, 6, 7, 15];
    let helpers = BddHelpers { temp: 14, nodes: &nodes };
    let res = transcribe_bdd_bennett(&fwd, &bwd, &PHYS, &ancilla, helpers);
    assert!(
        res.is_err(),
        "transcribe_bdd_bennett must REJECT gcd2: the biggest legal node bank (5 words) \
         is far below the {worst_total} node lines gcd2 needs"
    );
    eprintln!(
        "transcribe_bdd_bennett REJECTS the real gcd2 attempt (5-word node bank ≪ {worst_total} nodes): {}",
        res.unwrap_err()
    );
}
