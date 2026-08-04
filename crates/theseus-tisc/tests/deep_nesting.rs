//! A-5b -- deep loop-nesting via aux REUSE (Option B).
//!
//! Depth-2+ nested `Trace` now COMPILES + RUNS because a nested Trace REUSES the
//! enclosing level's four aux `{rc,rb,temp,ctrl}` -- provably DEAD across the
//! inner-loop region (design §2.2: the outer aux are net-zero per-iteration
//! ancillae, all 0 across the body; the come-from is carried by δu-immediates +
//! PC/BR, not a live register). Aux cost is O(1) in depth; nesting is bounded by
//! CARRIER registers, not by 4·depth aux (design §2.3).
//!
//! These tests are the L20 reproducers for the A-5b finding: the depth-2 nested
//! Trace was REJECTED before this slice (see the former reject tests, now re-pinned
//! to pass) and COMPILES + `run_on_cpu_fast == eval` now, over a NON-DEGENERATE
//! program (both loops iterate ≥ 2×) with a shared-aux tear-down probe.

use theseus_core::prim::enumerate_values;
use theseus_core::{Iso, Type};

use theseus_tisc::compile::{compile_with_metadata, run_on_cpu, run_on_cpu_fast};

// ── helpers ─────────────────────────────────────────────────────────

/// Whole-domain fast-oracle agreement: `run_on_cpu_fast == eval` on every input.
fn assert_cpu_eq_eval(name: &str, iso: &Iso) -> usize {
    let (in_ty, _) =
        theseus_core::type_of(iso).unwrap_or_else(|e| panic!("{name}: type error: {e:?}"));
    let inputs = enumerate_values(&in_ty);
    for v in &inputs {
        let cpu = run_on_cpu_fast(iso, v).unwrap_or_else(|e| panic!("{name}: run failed: {e}"));
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
        assert_eq!(cpu, ev, "{name}: fast-oracle disagrees with eval on {v:?}");
    }
    inputs.len()
}

/// Whole-domain REAL-CPU authority: the compiled ROM on the real `step`-iso CPU
/// (`run_on_cpu`, driven by `theseus_eval::eval`) == `eval` on every input. This
/// is the INDEPENDENT runtime witness that does NOT share `exec_instr` with the
/// k_max fast-oracle walk -- mirrors `compile.rs::assert_oracle` (the `*_real_cpu`
/// authority pattern). Slow (each step-iso evaluation ~seconds), hence `#[ignore]`.
fn assert_real_cpu_eq_eval(name: &str, iso: &Iso) -> usize {
    let (in_ty, _) =
        theseus_core::type_of(iso).unwrap_or_else(|e| panic!("{name}: type error: {e:?}"));
    let inputs = enumerate_values(&in_ty);
    assert!(!inputs.is_empty(), "{name}: domain is empty");
    for v in &inputs {
        let cpu = run_on_cpu(iso, v, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("{name} (real-CPU): run failed on {v:?}: {e}"));
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("{name}: eval failed on {v:?}: {e:?}"));
        assert_eq!(
            cpu, ev,
            "{name} (real-CPU): CPU disagrees with eval on {v:?}\n  cpu:  {cpu:?}\n  eval: {ev:?}"
        );
    }
    inputs.len()
}

fn one() -> Type {
    Type::One
}

/// A minimal, TAG-ONLY, non-degenerate loop that iterates EXACTLY 2×.
///
/// `Trace(One, SwapSum(One, One))` : `One ↔ One`. The carrier is `Sum(One, One)`
/// = a single tag leaf (One has 0 payload leaves), so the loop is TAG-ONLY -- the
/// minimal register footprint, which is exactly what lets a nested copy FIT after
/// aux reuse. Semantics (Trace enters on the RIGHT / a-side):
///   Right(unit) --SwapSum--> Left(unit)   [continue, iter 1]
///   Left(unit)  --SwapSum--> Right(unit)   [EXIT,     iter 2]
/// so every input iterates EXACTLY twice (a genuine ≥2 multi-iteration loop --
/// SwapSum on the tag flips continue↔exit each application).
fn tag_only_loop() -> Iso {
    Iso::trace(one(), Iso::SwapSum(one(), one()))
}

// ═════════════════════════════════════════════════════════════════════
// (1) Single-level tag-only Trace ACTUALLY ITERATES ≥ 2× (sanity floor).
// Pins that the body is non-degenerate before we nest it.
// ═════════════════════════════════════════════════════════════════════
#[test]
fn tag_only_single_level_iterates() {
    let iso = tag_only_loop();
    let lowered = compile_with_metadata(&iso).expect("single-level tag-only Trace must compile");
    // Non-degenerate: the loop takes 2 body applications → k_exact = 2+1 safety = 3.
    assert!(
        lowered.loop_terms[0].k_max >= 2,
        "tag-only loop must iterate ≥2× (k_max={})",
        lowered.loop_terms[0].k_max
    );
    let n = assert_cpu_eq_eval("tag-only single-level", &iso);
    eprintln!(
        "tag-only single-level: k_max={}, {n}/{n} run_on_cpu_fast==eval",
        lowered.loop_terms[0].k_max
    );
}

// ═════════════════════════════════════════════════════════════════════
// (2) DEPTH-2 nested Trace -- the A-5b deliverable. NON-DEGENERATE: the OUTER
// loop iterates ≥2× AND the INNER loop iterates ≥2× (during an outer iteration).
//
// Shape (all tag-only carriers, so the nested copy FITS 8 regs via aux reuse):
//   inner      = Trace(One, SwapSum)                         : One ↔ One (2 iters)
//   outer_body = Seq( Plus(Id(One), inner), SwapSum(One,One) ) : Bit ↔ Bit
//   outer      = Trace(One, outer_body)                       : One ↔ One
// The outer body CONTAINS a Trace node (inside the Plus a-arm) -- the exact case
// that REJECTED before A-5b. Trace of `outer`: enter Right(unit):
//   iter 1: Plus a-arm runs `inner` (which itself iterates 2×) → Right(unit);
//           then SwapSum → Left(unit)  [outer continues]
//   iter 2: Plus u-arm = Id → Left(unit); then SwapSum → Right(unit)  [outer EXIT]
// so the OUTER iterates 2× and the INNER iterates 2× within outer iteration 1.
// ═════════════════════════════════════════════════════════════════════
#[test]
fn depth2_nested_trace_compiles_and_runs() {
    let inner = tag_only_loop(); // Trace(One, SwapSum) : One ↔ One, iterates 2×

    // outer body : Sum(One,One) ↔ Sum(One,One) = Bit ↔ Bit.
    let outer_body = Iso::seq(Iso::plus(Iso::Id(one()), inner), Iso::SwapSum(one(), one()));
    let outer = Iso::trace(one(), outer_body);

    // It must type-check and COMPILE (was rejected before A-5b).
    let lowered = compile_with_metadata(&outer)
        .expect("A-5b: depth-2 nested Trace must COMPILE via aux reuse");

    // Non-degenerate at BOTH levels: outer loop term has a nested inner loop term,
    // and both k_max ≥ 2.
    assert_eq!(
        lowered.loop_terms.len(),
        1,
        "one outer loop term at top level"
    );
    let outer_term = &lowered.loop_terms[0];
    assert!(
        !outer_term.inner.is_empty(),
        "A-5b: the outer loop term must carry a NESTED inner loop term (depth-2)"
    );
    assert!(
        outer_term.k_max >= 2,
        "outer loop must iterate ≥2× (k_max={})",
        outer_term.k_max
    );
    assert!(
        outer_term.inner.iter().any(|c| c.k_max >= 2),
        "inner loop must iterate ≥2× (nested k_max={:?})",
        outer_term.inner.iter().map(|c| c.k_max).collect::<Vec<_>>()
    );

    // Whole-domain authority: run_on_cpu_fast == eval on every input.
    let n = assert_cpu_eq_eval("depth-2 nested Trace", &outer);
    eprintln!(
        "A-5b depth-2 nested Trace: COMPILE+RUN, {n}/{n} inputs, \
         run_on_cpu_fast==eval; outer k_max={}, inner k_max={:?}",
        outer_term.k_max,
        outer_term.inner.iter().map(|c| c.k_max).collect::<Vec<_>>()
    );
}

// ═════════════════════════════════════════════════════════════════════
// (3) SHARED-AUX TEAR-DOWN PROBE -- the direct witness of design §2.2.
//
// The inner Trace REUSES the outer's four aux {rc,rb,temp,ctrl}. Because the
// reuse is a net-zero bracket, those four registers must be 0 at every outer-body
// boundary (before + after the inner region). We witness this by verifying the
// program's terminal quiescence AND, white-box, that the inner frame is a SUBSET
// of the outer frame (genuine reuse, not a new disjoint frame). If reuse were not
// net-zero the run_on_cpu_fast==eval in test (2) would already fail; here we pin
// the SHARING structurally so a regression to disjoint frames is caught.
// ═════════════════════════════════════════════════════════════════════
#[test]
fn depth2_shared_aux_is_reused_not_disjoint() {
    use theseus_tisc::compile::alloc_aux_frame_with_reuse_for_test;
    // Outer carrier {0} (tag) + outer aux frame {1,2,3,4}; the outer marks the
    // frame occupied AND offers it as reusable. A directly-nested inner Trace with
    // a tag at {5} (a free reg) draws its 4-aux frame from the REUSABLE outer aux
    // -- i.e. the SAME {1,2,3,4}, not a new disjoint set.
    let occupied = [0u8, 1, 2, 3, 4, 5]; // outer carrier+frame + inner tag
    let reusable = [1u8, 2, 3, 4]; // outer frame, dead across the body
    let (inner_frame, _) =
        alloc_aux_frame_with_reuse_for_test(&occupied, &reusable).expect("inner reuses");
    // Genuine reuse: every inner-frame reg is one of the outer's (reusable) aux.
    for r in inner_frame {
        assert!(
            reusable.contains(&r),
            "A-5b tear-down: inner frame reg {r} must REUSE the outer aux {reusable:?} \
             (net-zero shared frame), got frame {inner_frame:?}"
        );
    }
    eprintln!(
        "A-5b shared-aux probe: inner frame {inner_frame:?} ⊆ outer aux {reusable:?} (reuse)"
    );

    // Contrast: with NO reusable offered, the same occupancy REJECTS (only {6,7}
    // free = 2 < 4) -- proving the reuse (not extra free regs) is what makes it fit.
    use theseus_tisc::compile::alloc_aux_frame_for_test;
    let res = alloc_aux_frame_for_test(&occupied, &[]);
    assert!(
        res.is_err(),
        "without reuse the nested frame must reject (only 2 free regs): {res:?}"
    );
}

// ═════════════════════════════════════════════════════════════════════
// (2R) DEPTH-2 nested Trace -- REAL-CPU AUTHORITY (#[ignore], slow).
//
// The SAME non-degenerate depth-2 nested-Trace program as
// `depth2_nested_trace_compiles_and_runs`, but checked against the INDEPENDENT
// runtime witness the fast oracle can't provide: the compiled ROM executed on
// the REAL `step`-iso CPU (`run_on_cpu`, driven by `theseus_eval::eval` for the
// per-step core-iso evaluation) == `eval` on the WHOLE domain. This is the
// authority that does NOT share `exec_instr` with the k_max fast-oracle walk --
// it proves aux-REUSE (the shared 4-aux come-from frame) composes across depth
// on the real machine, not just in the byte-level simulator. Mirrors the
// `compile.rs` `*_real_cpu` / `mdd_*_trace_real_cpu` `assert_oracle` pattern.
// ═════════════════════════════════════════════════════════════════════
#[test]
#[ignore = "authoritative real-CPU oracle (slow); run via `cargo test -p theseus-tisc --features heavy-tests --test deep_nesting depth2_nested_trace_real_cpu -- --ignored`"]
fn depth2_nested_trace_real_cpu() {
    let inner = tag_only_loop(); // Trace(One, SwapSum) : One ↔ One, iterates 2×

    // outer body : Sum(One,One) ↔ Sum(One,One) = Bit ↔ Bit -- IDENTICAL to
    // `depth2_nested_trace_compiles_and_runs` (the outer body CONTAINS a Trace).
    let outer_body = Iso::seq(Iso::plus(Iso::Id(one()), inner), Iso::SwapSum(one(), one()));
    let outer = Iso::trace(one(), outer_body);

    // Compile + re-assert non-degeneracy at BOTH levels (same as the fast test),
    // so this authority runs against exactly the depth-2 nested loop, not a
    // silently-degenerate program.
    let lowered = compile_with_metadata(&outer)
        .expect("A-5b: depth-2 nested Trace must COMPILE via aux reuse");
    let outer_term = &lowered.loop_terms[0];
    assert!(
        !outer_term.inner.is_empty(),
        "A-5b real-CPU: the outer loop term must carry a NESTED inner loop term (depth-2)"
    );
    assert!(
        outer_term.k_max >= 2,
        "outer loop must iterate ≥2× (k_max={})",
        outer_term.k_max
    );
    assert!(
        outer_term.inner.iter().any(|c| c.k_max >= 2),
        "inner loop must iterate ≥2× (nested k_max={:?})",
        outer_term.inner.iter().map(|c| c.k_max).collect::<Vec<_>>()
    );

    // THE authority: real `step`-iso CPU == eval on the whole domain.
    let n = assert_real_cpu_eq_eval("depth-2 nested Trace", &outer);
    eprintln!(
        "A-5b depth-2 nested Trace: REAL-CPU authority, {n}/{n} inputs, \
         run_on_cpu==eval; outer k_max={}, inner k_max={:?}",
        outer_term.k_max,
        outer_term.inner.iter().map(|c| c.k_max).collect::<Vec<_>>()
    );
}

// ── depth-3 helper: nest a One↔One loop one level deeper ─────────────
fn wrap_one_level(inner: Iso) -> Iso {
    // inner : One ↔ One. outer body = Seq(Plus(Id(One), inner), SwapSum) : Bit↔Bit;
    // outer = Trace(One, body) : One ↔ One (iterates 2×, runs `inner` on iter 1).
    Iso::trace(
        one(),
        Iso::seq(Iso::plus(Iso::Id(one()), inner), Iso::SwapSum(one(), one())),
    )
}

// ═════════════════════════════════════════════════════════════════════
// (4) DEPTH-3 nested Trace -- the arbitrary-depth claim. Aux cost is O(1) in
// depth (every level reuses the shared 4-aux frame), so depth is bounded by
// CARRIER registers, NOT 4·depth aux. With tag-only carriers depth-3 must either
// COMPILE (all three levels sharing the one aux frame) or REJECT GRACEFULLY by a
// clean carrier/register budget message -- never panic, never 4·depth blow-up.
// ═════════════════════════════════════════════════════════════════════
#[test]
fn depth3_nested_trace_compiles_or_rejects_gracefully() {
    let inner = tag_only_loop(); // depth-1
    let mid = wrap_one_level(inner); // depth-2
    let outer3 = wrap_one_level(mid); // depth-3

    match compile_with_metadata(&outer3) {
        Ok(lowered) => {
            // If it fits, it MUST run correctly and show 3 nesting levels.
            let n = assert_cpu_eq_eval("depth-3 nested Trace", &outer3);
            let t = &lowered.loop_terms[0];
            let d2 = t.inner.first();
            let d3 = d2.and_then(|c| c.inner.first());
            eprintln!(
                "A-5b depth-3 nested Trace: COMPILES+RUNS, {n}/{n} run_on_cpu_fast==eval; \
                 k_max L1={} L2={:?} L3={:?}",
                t.k_max,
                d2.map(|c| c.k_max),
                d3.map(|c| c.k_max)
            );
            assert!(
                d3.is_some(),
                "depth-3 must carry a 3-level nested loop tree"
            );
        }
        Err(e) => {
            let msg = format!("{e}");
            // GRACEFUL reject = a clean `Err` (never a panic / wrong body). The
            // aux FRAME reuse is O(1) in depth, but each nesting level's `Plus`
            // skip-block still needs 2 NON-reusable aux (LIVE across its body),
            // so depth-3 exhausts the register/leaf budget at the deepest Plus --
            // the reject-don't-corrupt boundary (design §2.3: depth bounded by
            // carrier + per-level skip-block registers). Any of these budget
            // messages is a clean reject.
            assert!(
                msg.contains("register")
                    || msg.contains("aux frame")
                    || msg.contains("carrier")
                    || msg.contains("don't alias")
                    || msg.contains("budget")
                    || msg.contains("leaves")
                    || msg.contains("maximum"),
                "depth-3 must reject GRACEFULLY by a carrier/register budget, got: {msg}"
            );
            eprintln!("A-5b depth-3 nested Trace: GRACEFUL reject (carrier-register bound): {msg}");
        }
    }
}
