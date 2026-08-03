//! A-3: tree-structured WCET fold -- nested loops counted correctly.
//!
//! Closes review BLOCKER-2: `wcet()` used to sum `loop_terms` as a FLAT
//! sibling list `(k_max, iter_len)` with `(k_max-1)·iter_len`, which
//! UNDER-counts nested loops -- an inner loop's cost was never multiplied by
//! the enclosing `k_max`. Under-count → park guard passes a bogus budget →
//! parked PC wraps mid-run → L15-class silent corruption.
//!
//! These are WHITE-BOX tests on the `CostTree` cost substrate + `Lowered::wcet`.
//! Real nested PROGRAMS still reject at compile time (`has_inner_control` /
//! `contains_trace` in `lower_trace`) -- deleting those rejects is A-4's job,
//! which must land after A-5. So we construct the nested cost tree directly
//! (a test-only path that bypasses the reject) and assert the fold multiplies
//! correctly. When A-4 lands, the SAME fold -- not a reject -- is what keeps `B`
//! sound; see `docs/scratch/compositional-control-design.md` §1.
//!
//! Invariant under test: **`B_fold ≥ true_B`** (never under-count). This is the
//! park total-bijection soundness property.

use theseus_tisc::compile::{compile_with_metadata, CostTree, Lowered};
use theseus_core::{Iso, Type};

// ─────────────────────────────────────────────────────────────────────
// BLOCKER-2 pinned counterexample: outer k=4 / inner k=100 / iter=12.
// True B ≈ 4 · 100 · 12 = 4800. Flat sum gives ≈ 88 (55× under-count).
// ─────────────────────────────────────────────────────────────────────

/// The exact scenario from the design doc / BLOCKER-2:
///   outer Trace k_max = 4, inner Trace k_max = 100, inner iter_len = 12.
/// We give the outer loop iter_len = 12 as well (one straight-line pass of the
/// outer body -- which contains ONE straight-line pass of the inner body).
fn blocker2_outer_node() -> CostTree {
    let inner = CostTree {
        k_max: 100,
        iter_len: 12,
        inner: vec![],
    };
    CostTree {
        k_max: 4,
        iter_len: 12,
        inner: vec![inner],
    }
}

/// The OLD flat-sum accounting, reproduced here to make the regression
/// explicit: the inner term is treated as a SIBLING (never multiplied by the
/// outer k_max), and -- because the exact-k_max walk ran the body straight-line
/// -- the outer iter_len counted the inner body only ONCE. So the flat sum is:
///   (k_outer-1)·iter_outer + (k_inner-1)·iter_inner_counted_once
/// but under the real defect the inner loop was DROPPED entirely (body_meta
/// discarded), so the outer contribution alone is (4-1)·(12+one inner pass).
/// Either framing lands near ~88, an order of magnitude under 4800.
fn old_flat_sum_extra() -> usize {
    // Defect (a): inner body_meta discarded → inner term absent. Only the
    // outer loop's flat term survives, and its iter_len saw the inner body
    // straight-line once (defect (b)). Outer iter_len ≈ 12 + (inner body ~12)
    // seen once ≈ 24; (4-1)·24 ≈ 72 ... take the most generous flat reading
    // (inner also a sibling term): (4-1)·12 + (100-1)·... no -- under the real
    // defect the inner is gone. We pin the magnitude: flat ≪ 100.
    let outer_iter_counting_inner_once = 12 + 12; // outer spine + one inner pass
    (4 - 1) * outer_iter_counting_inner_once
}

#[test]
fn blocker2_nested_wcet_is_multiplied_not_flat() {
    let node = blocker2_outer_node();

    // Tree fold: extra(outer) = (4-1)·12 + 4·extra(inner)
    //          = 36 + 4·((100-1)·12)
    //          = 36 + 4·1188 = 36 + 4752 = 4788.
    let extra = node.extra();
    assert_eq!(
        extra, 4788,
        "tree fold: (4-1)·12 + 4·((100-1)·12) = 36 + 4752 = 4788"
    );

    // The load-bearing soundness assertion: the folded extra is ON THE ORDER
    // of the true nested step count (≈ 4800), NOT the flat ≈ 88.
    let true_b_lower_bound = 4 * 100 * 12; // 4800 -- dynamic inner-body executions
    // extra() omits the single ROM pass already in instrs.len(); adding the one
    // ROM pass of the inner body (12) recovers ≥ true_b for the inner work:
    //   extra + inner_rom_pass = 4788 + 12 = 4800 = true_b.
    assert!(
        extra + node.inner[0].iter_len >= true_b_lower_bound,
        "B_fold must be ≥ true_B ≈ {true_b_lower_bound}, got extra={extra}"
    );

    // And it is NOT the flat under-count.
    let flat = old_flat_sum_extra();
    assert!(flat < 100, "flat sum under-counts to ~{flat} (< 100)");
    assert!(
        extra > 50 * flat,
        "tree fold ({extra}) must massively exceed the flat under-count ({flat})"
    );
}

#[test]
fn blocker2_lowered_wcet_uses_tree_fold() {
    // Wrap the node in a Lowered with a plausible ROM footprint and check the
    // top-level wcet() folds it. `instrs` length is arbitrary here (white-box);
    // pick a ROM that includes one straight-line pass of the whole nest.
    let node = blocker2_outer_node();
    // One straight-line ROM pass: outer spine(4) + outer iter_len(12) which
    // already contains the inner body's one pass. Use a dummy instr vec of that
    // length so wcet() = instrs.len() + extra.
    let rom_len = 4 + node.iter_len; // 16
    let dummy = dummy_instrs(rom_len);
    let lowered = Lowered {
        instrs: dummy,
        forward_skip_total: 0,
        loop_terms: vec![node.clone()],
        w_br: 8,
    };
    let b = lowered.wcet();
    assert_eq!(b, rom_len + node.extra(), "wcet = |instrs| + Σ extra(node)");
    assert_eq!(b, 16 + 4788, "16 + 4788 = 4804");
    assert!(b >= 4800, "B_fold ≥ true_B (4800)");
}

// ─────────────────────────────────────────────────────────────────────
// CostTree::extra unit checks -- the fold arithmetic in isolation.
// ─────────────────────────────────────────────────────────────────────

#[test]
fn extra_single_level_matches_old_flat_term() {
    // A single-level loop must produce EXACTLY the old flat term (k-1)·iter.
    for &(k, iter) in &[(2usize, 10usize), (6, 14), (33, 10), (100, 12)] {
        let node = CostTree {
            k_max: k,
            iter_len: iter,
            inner: vec![],
        };
        assert_eq!(
            node.extra(),
            (k - 1) * iter,
            "single-level extra must equal the old flat term (k-1)·iter"
        );
    }
}

#[test]
fn extra_two_level_multiplies_through_outer_kmax() {
    // Generic: extra(outer) = (ko-1)·io + ko·((ki-1)·ii)
    let ko = 3;
    let io = 20;
    let ki = 7;
    let ii = 9;
    let node = CostTree {
        k_max: ko,
        iter_len: io,
        inner: vec![CostTree {
            k_max: ki,
            iter_len: ii,
            inner: vec![],
        }],
    };
    let expected = (ko - 1) * io + ko * ((ki - 1) * ii);
    assert_eq!(node.extra(), expected);
    // Sanity: strictly greater than the flat (non-multiplied) reading.
    let flat = (ko - 1) * io + (ki - 1) * ii;
    assert!(node.extra() > flat, "multiply must exceed flat sibling sum");
}

#[test]
fn extra_three_level_deep_nest_multiplies_at_every_level() {
    // outer=2, mid=3, inner=5 with iters 4,6,8.
    // extra(inner) = (5-1)·8 = 32
    // extra(mid)   = (3-1)·6 + 3·32 = 12 + 96 = 108
    // extra(outer) = (2-1)·4 + 2·108 = 4 + 216 = 220
    let node = CostTree {
        k_max: 2,
        iter_len: 4,
        inner: vec![CostTree {
            k_max: 3,
            iter_len: 6,
            inner: vec![CostTree {
                k_max: 5,
                iter_len: 8,
                inner: vec![],
            }],
        }],
    };
    assert_eq!(node.inner[0].inner[0].extra(), 32);
    assert_eq!(node.inner[0].extra(), 108);
    assert_eq!(node.extra(), 220);
}

// ─────────────────────────────────────────────────────────────────────
// Regression: single-level (flat) programs keep their EXACT current B.
// These COMPILE today (no nesting) and pin the byte-identical WCET.
// ─────────────────────────────────────────────────────────────────────

#[test]
fn regression_flat_program_wcet_unchanged() {
    // Flat(5) Id-body Trace: k_max=2, iter_len=10 (Id body = 0 instrs).
    // Old fold: |instrs| + (2-1)·10. New fold must be identical.
    let u_ty = Type::flat(5);
    let carrier = Type::sum(u_ty.clone(), u_ty.clone());
    let iso = Iso::trace(u_ty.clone(), Iso::Id(carrier));

    let lowered = compile_with_metadata(&iso).expect("compiles");
    assert_eq!(lowered.loop_terms.len(), 1, "single top-level loop");
    let node = &lowered.loop_terms[0];
    assert_eq!(node.k_max, 2);
    assert_eq!(node.iter_len, 10);
    assert!(node.inner.is_empty(), "no nesting");

    // B must equal instrs.len() + (k-1)·iter -- byte-identical to the old flat.
    let k = 2usize;
    let iter = 10usize;
    let expected = lowered.instrs.len() + (k - 1) * iter;
    assert_eq!(lowered.wcet(), expected);
    assert_eq!(lowered.wcet(), 24, "spine(4)+iter(10)*2 -> 4+10 + 10 = 24");
}

// ── helper ───────────────────────────────────────────────────────────

fn dummy_instrs(n: usize) -> Vec<theseus_tisc::asm::Instr> {
    use theseus_tisc::asm::Instr;
    (0..n).map(|_| Instr::Xori { rd: 0, imm: 0 }).collect()
}
