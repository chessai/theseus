//! T-loops slice-1a: EXACT-K_MAX tests.
//!
//! Verifies the trajectory-derived exact k_max bound in `lower_trace`:
//!   k_max = min(max(T over all inputs) + 1, |u| + 1)
//! where T = number of body applications until exit, computed via exec_instr.
//!
//! FAST tier (default gate):
//!   F1: large-|u| fast-exit Trace now compiles (was WCET-rejected under |u|+1)
//!   F2: k_exact ≤ |u|+1 always (never looser) — property over several Traces
//!   F3: soundness — run_on_cpu_fast == eval, k_max is correct
//!
//! HEAVY tier (#[ignore], advisor pre-merge):
//!   H1: run_on_cpu (real system_step) == eval — authoritative round-trip

use std::collections::BTreeMap;
use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type};
use theseus_tisc::compile::{
    compile_with_metadata, marshal, mde_walk_bytes, run_on_cpu_fast, unmarshal,
};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

// ── Helpers ─────────────────────────────────────────────────────────

fn state_with_regs(bytes: &[u8]) -> StateVals {
    let mut r = [0u8; NUM_REGS];
    r[..bytes.len()].copy_from_slice(bytes);
    StateVals {
        regs: r,
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    }
}

fn nat_chain(n: usize) -> Type {
    assert!(n >= 2);
    let mut t = Type::sum(Type::One, Type::One);
    for _ in 2..n {
        t = Type::sum(Type::One, t);
    }
    t
}

/// Build the mde_spill_walk body (same as mde_spill.rs and the POC).
fn build_walk_body() -> (Iso, Type, Type) {
    let n5 = nat_chain(5);
    let bit = Type::sum(Type::One, Type::One);
    let carrier = Type::sum(n5.clone(), bit.clone());
    let vals = enumerate_values(&carrier);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let sv = marshal(v, &carrier);
        let mut bytes: Vec<u8> = sv.regs[..5].to_vec();
        mde_walk_bytes(&mut bytes);
        let out_val = unmarshal(&state_with_regs(&bytes), &carrier);
        forward.insert(v.clone(), out_val.clone());
        assert!(backward.insert(out_val, v.clone()).is_none());
    }
    let prim = PrimDef {
        forward_name: "mde_spill_walk".into(),
        backward_name: "mde_spill_walk_inv".into(),
        input_ty: carrier.clone(),
        output_ty: carrier.clone(),
        forward,
        backward,
        symbolic: None,
    };
    (Iso::Prim(Box::new(prim)), n5, carrier)
}

// ═════════════════════════════════════════════════════════════════════
// F1: large-|u| fast-exit Trace — was REJECTED under |u|+1, now COMPILES
// ═════════════════════════════════════════════════════════════════════

#[test]
fn f1_large_u_fast_exit_compiles() {
    // u = Flat(5): |u| = 32 (2^5), 1 leaf.
    // Body = Id on Sum(Flat(5), Flat(5)).
    // Under |u|+1: k_max = 33, iter_len = 10, WCET = 4 + 33*10 = 334 > 256. REJECTED.
    // Under exact-k_max: T = 1 for all inputs (Id exits immediately).
    //   k_exact = 1 + 1 = 2, k_max = min(2, 33) = 2.
    //   WCET = 4 + 2*10 = 24 ≤ 256. COMPILES.
    let u_ty = Type::flat(5);
    let carrier = Type::sum(u_ty.clone(), u_ty.clone());
    let body = Iso::Id(carrier);
    let iso = Iso::trace(u_ty.clone(), body);

    let u_card = enumerate_values(&u_ty).len();
    assert_eq!(u_card, 32);

    // Document that under the old |u|+1 bound this WOULD have been rejected:
    let old_wcet = 4 + (u_card + 1) * 10;
    assert_eq!(old_wcet, 334);
    assert!(old_wcet > 256, "old WCET {old_wcet} should exceed 256 cap");

    // A-6.4 widen-PC: the 334 > 256 rejection is ALSO lifted structurally —
    // the PC address space is now 2^16, so even without exact-k_max a WCET
    // of 334 fits (see tests/widen_pc_payoff.rs for a program that actually
    // runs at B > 256). Here exact-k_max additionally shrinks it to 24.
    let lowered = compile_with_metadata(&iso)
        .expect("Flat(5) Id-body Trace must compile under exact-k_max (k_exact=2, WCET=24)");
    let (k_max, iter_len) = (lowered.loop_terms[0].k_max, lowered.loop_terms[0].iter_len);
    assert!(lowered.loop_terms[0].inner.is_empty(), "single-level: no nesting");
    assert_eq!(k_max, 2, "k_exact = max(T=1) + 1 = 2");
    assert_eq!(
        iter_len, 10,
        "Id body = 0 instrs, iter_len = LOOP_OVERHEAD = 10"
    );
    let wcet = lowered.wcet();
    assert_eq!(wcet, 24, "spine(4) + 2*10 = 24");
    assert!(wcet <= 256, "exact-k_max WCET {wcet} must fit in 256 cap");

    // Whole-domain run_on_cpu_fast == eval (bounded: 32 inputs).
    for v in enumerate_values(&u_ty) {
        let cpu = run_on_cpu_fast(&iso, &v).expect("run");
        let mut fuel = 100_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "fast-oracle disagrees with eval on {v:?}");
    }
    eprintln!("F1: Flat(5) Id-body Trace: {u_card}/{u_card} green (k_max={k_max}, WCET={wcet})");
}

// ═════════════════════════════════════════════════════════════════════
// F2: k_exact ≤ |u|+1 always — never looser than the old bound
// ═════════════════════════════════════════════════════════════════════

#[test]
fn f2_k_exact_never_looser() {
    // Property: for every Trace that compiles, loop_terms k_max ≤ |u|+1.
    let cases: Vec<(&str, Iso, Type)> = vec![
        // Id-body, small u
        {
            let u = Type::sum(Type::One, Type::One); // Bit, |u|=2
            let c = Type::sum(u.clone(), Type::One);
            ("Id(Bit)", Iso::trace(u.clone(), Iso::Id(c)), u)
        },
        // Id-body, large u (Flat(5) = 32 inhabitants)
        {
            let u = Type::flat(5);
            let c = Type::sum(u.clone(), u.clone());
            ("Id(Flat5)", Iso::trace(u.clone(), Iso::Id(c)), u)
        },
        // mde_spill walk body (variable-T, saturated)
        {
            let (body, u_ty, _carrier) = build_walk_body();
            let name = "mde_walk";
            (name, Iso::trace(u_ty.clone(), body), u_ty)
        },
    ];

    for (name, iso, u_ty) in &cases {
        let lowered =
            compile_with_metadata(iso).unwrap_or_else(|e| panic!("{name}: compile failed: {e}"));
        let k_max = lowered.loop_terms[0].k_max;
        let u_card = enumerate_values(u_ty).len();
        let k_fallback = u_card + 1;
        assert!(
            k_max <= k_fallback,
            "{name}: k_max={k_max} > |u|+1={k_fallback} — exact-k_max is LOOSER than fallback"
        );
        eprintln!("{name}: k_max={k_max} ≤ |u|+1={k_fallback} ✓");
    }
}

// ═════════════════════════════════════════════════════════════════════
// F3: soundness — k_max is correct, run_on_cpu_fast == eval
// ═════════════════════════════════════════════════════════════════════

#[test]
fn f3_mde_walk_soundness() {
    // The mde_spill walk Trace: u=N5 (|u|=5), T(0)=6, T(1)=1.
    // max(T) = 6 = |u|+1 (saturated) → k_exact = 7 > 6 → fallback: k_max = 6.
    let (body, u_ty, _carrier) = build_walk_body();
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::trace(u_ty, body);

    let lowered = compile_with_metadata(&iso).expect("mde_walk compiles");
    let k_max = lowered.loop_terms[0].k_max;
    // max(T) = 6. k_exact = 6+1 = 7. k_fallback = 5+1 = 6. min(7,6) = 6.
    assert_eq!(k_max, 6, "mde_walk: saturated, k_max = |u|+1 = 6");

    // Whole-domain fast oracle (2 inputs).
    for v in enumerate_values(&bit) {
        let cpu = run_on_cpu_fast(&iso, &v).expect("run");
        let mut fuel = 10_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(
            cpu, ev,
            "mde_walk: fast-oracle disagrees with eval on {v:?}"
        );
    }
    eprintln!(
        "F3: mde_walk soundness: k_max={k_max}, WCET={}, 2/2 green",
        lowered.wcet()
    );
}

#[test]
fn f3_large_u_id_body_soundness() {
    // Id-body Trace on Flat(5): |u|=32, T=1, k_exact=2, k_max=2.
    let u_ty = Type::flat(5);
    let carrier = Type::sum(u_ty.clone(), u_ty.clone());
    let body = Iso::Id(carrier);
    let iso = Iso::trace(u_ty.clone(), body);

    let u_card = enumerate_values(&u_ty).len();
    assert_eq!(u_card, 32);

    let lowered = compile_with_metadata(&iso).expect("Flat(5) Id-body compiles");
    let k_max = lowered.loop_terms[0].k_max;
    assert_eq!(k_max, 2, "Id body: T=1 for all inputs, k_exact=2");

    // Whole-domain: all 32 inputs.
    for v in enumerate_values(&u_ty) {
        let cpu = run_on_cpu_fast(&iso, &v).expect("run");
        let mut fuel = 100_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "fast-oracle disagrees with eval");
    }
    eprintln!("F3: Flat(5) Id-body: k_max={k_max}, {u_card}/{u_card} green");
}

// ═════════════════════════════════════════════════════════════════════
// H1: HEAVY — real system_step authority (run_on_cpu == eval)
// ═════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "HEAVY: real system_step authority — run_on_cpu (step-iso evaluator) == eval"]
fn h1_large_u_id_body_real_cpu() {
    // Flat(5) Id-body Trace with exact-k_max (k_max=2): run on the REAL
    // step-iso CPU (system_step). If k_max were unsound (too small), the
    // parked PC would wrap into live ROM → silent corruption → output mismatch.
    let u_ty = Type::flat(5);
    let carrier = Type::sum(u_ty.clone(), u_ty.clone());
    let body = Iso::Id(carrier);
    let iso = Iso::trace(u_ty.clone(), body);

    let u_card = enumerate_values(&u_ty).len();
    for v in enumerate_values(&u_ty) {
        let cpu =
            theseus_tisc::compile::run_on_cpu(&iso, &v, theseus_eval::eval).expect("run_on_cpu");
        let mut fuel = 100_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "real CPU disagrees with eval on {v:?}");
    }
    eprintln!("H1: {u_card}-inhabitant Id-body real-CPU: {u_card}/{u_card} green (k_max=2, exact-k_max authority)");
}

#[test]
#[ignore = "HEAVY: mde_walk on real system_step — existing variable-T Trace still sound"]
fn h1_mde_walk_real_cpu() {
    let (body, u_ty, _carrier) = build_walk_body();
    let bit = Type::sum(Type::One, Type::One);
    let iso = Iso::trace(u_ty, body);

    for v in enumerate_values(&bit) {
        let cpu =
            theseus_tisc::compile::run_on_cpu(&iso, &v, theseus_eval::eval).expect("run_on_cpu");
        let mut fuel = 10_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "real CPU disagrees with eval on {v:?}");
    }
    eprintln!("H1: mde_walk real-CPU: 2/2 green (k_max=6, trajectory-saturated)");
}
