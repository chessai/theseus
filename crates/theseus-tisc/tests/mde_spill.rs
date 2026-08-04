#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! M-D.e slice 1 -- loop-carrier scratch-spill machinery proof.
//!
//! Trace(u=N5, walk) with a 5-leaf carrier (4 payload leaves) so payload
//! leaf 4 spills to Scratch(2), body touches the spilled leaf, trajectory
//! takes 6 iterations (5 back-edges) for input 0.
//!
//! Carrier Sum(N5, Bit), N5 = 4-leaf nat chain (5 inhabitants, k_max=6).
//! Body = pinned 13-instr walk (7-cycle):
//!   R0 → L4 → L3 → L2 → L1 → L0 → R1 → R0
//!
//! Tests:
//!   T1 (emission): ROM structure + paging Xchg assertions
//!   T2 (fast oracle): run_on_cpu_fast == eval over Bit inputs
//!   T3 (#[ignore] real-CPU): run_on_cpu == eval + scratch-all-zero terminal
//!   T4 (#[ignore] closure): SystemRing validate()==Ok / run==eval^R
//!   T-reject-cap: 5-payload Trace rejected (structured error match)
//!   T-reject-input: 4-leaf input type Trace rejected (n_input+1 guard)

use std::collections::BTreeMap;
use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type};
use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::{
    compile, compile_with_metadata, marshal, mde_walk_bytes, run_on_cpu_fast, run_rom_on_cpu,
    unmarshal, CompileError,
};
use theseus_tisc::types::{StateVals, NUM_SCRATCH};

// ── Helpers ─────────────────────────────────────────────────────────

fn nat_chain(n: usize) -> Type {
    // n inhabitants: Sum(One, Sum(One, ... )) with n-1 nested sums
    assert!(n >= 2);
    let mut t = Type::sum(Type::One, Type::One); // 2 inhabitants
    for _ in 2..n {
        t = Type::sum(Type::One, t);
    }
    t
}

/// Build the walk PrimDef on Sum(N5, Bit) from the byte map + marshal.
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
        let out_val = unmarshal(&StateVals::state_with_regs(&bytes), &carrier);
        // Closure: the byte map must land on a canonical encoding.
        let out_re = marshal(&out_val, &carrier);
        assert_eq!(
            &out_re.regs[..5],
            &bytes[..],
            "walk not closed on canonical encodings"
        );
        forward.insert(v.clone(), out_val.clone());
        assert!(
            backward.insert(out_val, v.clone()).is_none(),
            "walk is not injective on canonical values"
        );
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

/// Count eval-side trajectory length (body applications until Right).
fn trajectory_len(body: &Iso, carrier: &Type, input_bit: u8) -> usize {
    let Iso::Prim(def) = body else { panic!() };
    let vals = enumerate_values(carrier);
    let mut cur = vals
        .iter()
        .find(|v| {
            let b = marshal(v, carrier).regs;
            b[0] == 1 && b[1] == input_bit
        })
        .expect("Right(input) canonical value")
        .clone();
    for step in 1..=100 {
        cur = def.forward.get(&cur).expect("total table").clone();
        let b = marshal(&cur, carrier).regs;
        if b[0] == 1 {
            return step;
        }
    }
    panic!("trajectory did not terminate");
}

// ── T1: emission ────────────────────────────────────────────────────

#[test]
fn mde_spill_emission() {
    let (body, u_ty, carrier) = build_walk_body();

    // Trajectory lengths (eval-side ground truth).
    let t0 = trajectory_len(&body, &carrier, 0);
    let t1 = trajectory_len(&body, &carrier, 1);
    eprintln!("trajectory: input 0 -> {t0} iterations, input 1 -> {t1}");
    assert!(t0 >= 3, "input 0 must take >= 3 iterations (actual {t0})");

    let iso = Iso::trace(u_ty, body);

    // Compiles at 4 payload leaves.
    let instrs = compile(&iso).expect("4-payload spill trace should compile");
    eprintln!("ROM ({} instrs):", instrs.len());
    for (i, ins) in instrs.iter().enumerate() {
        eprintln!("  [{i:2}] {ins:?}");
    }

    // Loop region: prologue = 2 instrs, epilogue = 2 instrs.
    // Ltop at index 2, Lend at index rom_len-3.
    let ltop = 2usize;
    let lend = instrs.len() - 3;

    // The walk body (between skeleton pre/post) must be Swapbr-free.
    // Skeleton: 5 instrs before body (Swapbr Ltop, Cswap, Subi, Xor, Xori),
    // then body, then 5 after (Xor, Xori, Addi, Cswap, Swapbr Lend).
    // Body region = [ltop+5 .. lend-4).
    let body_start = ltop + 5;
    let body_end = lend - 4;
    for (i, instr) in instrs.iter().enumerate().take(body_end).skip(body_start) {
        assert!(
            !matches!(instr, Instr::Swapbr { .. }),
            "walk body must be Swapbr-free (found Swapbr at index {i})"
        );
    }

    // Find all Xchg paging instructions (scratch exchange).
    let pagings: Vec<(usize, u8, u8)> = instrs
        .iter()
        .enumerate()
        .filter_map(|(i, ins)| match ins {
            Instr::Xchg { rd, sx } => Some((i, *rd, *sx)),
            _ => None,
        })
        .collect();
    eprintln!("paging Xchg instrs (idx, rd, sx): {pagings:?}");

    // EXACTLY 2 paging Xchgs (one page-in, one page-out = whole-body bracket).
    assert_eq!(
        pagings.len(),
        2,
        "net-zero bracket: exactly one page-in + one page-out"
    );

    // Both STRICTLY INSIDE the loop body (between Ltop and Lend offsets).
    for &(i, rd, sx) in &pagings {
        assert!(
            i > ltop && i < lend,
            "paging Xchg at {i} must be INSIDE the loop body (Ltop={ltop}, Lend={lend})"
        );
        // Scratch slot must be spill area (sx >= 2).
        assert!(sx >= 2, "paging must target spill scratch (s{sx})");
        // Spare register must be an aux register (rd >= 4).
        assert!(
            rd >= 4,
            "paging spare must be an aux register (rd={rd}, aux at 4..8)"
        );
    }

    // Same spare register and scratch slot on both sides.
    assert_eq!(pagings[0].1, pagings[1].1, "same spare both sides");
    assert_eq!(pagings[0].2, pagings[1].2, "same scratch slot both sides");
}

// ── T2: fast oracle (DEFAULT, runs in gate) ─────────────────────────

#[test]
fn mde_spill_fast_oracle() {
    let (body, u_ty, carrier) = build_walk_body();

    // Verify trajectory: input 0 has >= 3 iterations (actual 6).
    let t0 = trajectory_len(&body, &carrier, 0);
    assert!(t0 >= 3, "input 0 must exercise the back-edge (t={t0})");

    let iso = Iso::trace(u_ty, body);
    let bit = Type::sum(Type::One, Type::One);
    for v in enumerate_values(&bit) {
        let cpu = run_on_cpu_fast(&iso, &v).expect("fast run");
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "fast-sim disagrees with eval on {v:?}");
        eprintln!("fast:  {v:?} -> {cpu:?}  == eval OK");
    }
}

// ── T3: real-CPU oracle + scratch-all-zero terminal (conditions 1, 4) ─

#[test]
#[ignore = "authoritative real-CPU oracle (slow)"]
fn mde_spill_real_cpu() {
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);

    let lowered = compile_with_metadata(&iso).expect("compile");
    let budget = lowered.wcet();
    let s_max_val = lowered.s_max();

    let bit = Type::sum(Type::One, Type::One);
    let (in_ty, out_ty) = theseus_core::type_of(&iso).unwrap();

    for v in enumerate_values(&bit) {
        eprintln!("real-cpu: running input {v:?} ...");

        // Run via run_rom_on_cpu to get the full final StateVals (condition 4).
        let state = marshal(&v, &in_ty);
        let final_state = run_rom_on_cpu(
            &lowered.instrs,
            state,
            budget,
            s_max_val,
            theseus_eval::eval,
        )
        .expect("real-cpu run");

        let cpu_out = unmarshal(&final_state, &out_ty);
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu_out, ev, "REAL CPU disagrees with eval on {v:?}");
        eprintln!("real:  {v:?} -> {cpu_out:?}  == eval OK");

        // Condition 4 (5c): scratch-all-zero terminal assertion.
        // A paging bug leaving garbage in s2 at exit with correct output
        // regs would otherwise pass both oracles (unmarshal of Bit reads r0 only).
        assert_eq!(
            final_state.scratch, [0u8; NUM_SCRATCH],
            "scratch must be all-zero at exit (paging leak check): scratch={:?}",
            final_state.scratch
        );
    }
}

// ── T4: SystemRing closure (condition 3) ────────────────────────────

#[test]
#[ignore = "ring closure test (slow, OOM-sensitive); run via --ignored"]
fn mde_spill_system_ring_closure() {
    use theseus_tisc::ring_run::{system_ring, two_body_timing_certificate};
    use theseus_tisc::types::{decode_system, encode_system};

    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);

    let lowered = compile_with_metadata(&iso).expect("compile");
    let rom = assemble(&lowered.instrs).expect("assemble");
    let budget = lowered.wcet();
    let ring = system_ring(&rom);

    // validate()==Ok on the spilled-carrier ROM's timing certificate.
    let cert = two_body_timing_certificate(&rom).expect("no rail skew on spill-trace ROM");
    cert.validate()
        .expect("validate() must pass on spill-trace ROM (M-D.e closure obligation)");

    // run == eval^R over the Bit domain.
    let step_iso = theseus_tisc::step::system_step(&rom);
    let (in_ty, out_ty) = theseus_core::type_of(&iso).unwrap();
    let bit = Type::sum(Type::One, Type::One);

    for v in enumerate_values(&bit) {
        let state = marshal(&v, &in_ty);
        // Bus-quiescence of the spilled-carrier ROM (M-D.e 5b): the paging
        // `Xchg` is a CPU-internal reg↔scratch op with no bus leg, so the ring
        // must be bus-quiescent at the initial state even under the back-edge.
        assert!(
            ring.bus_quiescent(&state),
            "SystemRing must be bus-quiescent on the spilled-carrier ROM (input {v:?})"
        );
        let mut cur = encode_system(&state);
        let mut fuel = 8_000_000_000u64;

        for _ in 0..budget {
            cur = theseus_eval::eval(&step_iso, cur, &mut fuel).expect("ring step failed");
        }

        let final_state = decode_system(&cur);
        let cpu_out = unmarshal(&final_state, &out_ty);

        let mut eval_fuel = 1_000_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut eval_fuel).expect("eval failed");

        assert_eq!(
            cpu_out, eval_out,
            "ring closure: CPU disagrees with eval on input {v:?}"
        );
        eprintln!("ring:  {v:?} -> {cpu_out:?}  == eval^R OK");
    }
}

// ── T-reject-cap: 5-payload Trace rejected (condition 5d) ───────────

#[test]
fn mde_spill_reject_5_payload() {
    // 5 payload leaves (u = 6-inhabitant chain, 5 leaves) must reject.
    // Cap moved by exactly one (3→4); no silent over-reach.
    let n6 = nat_chain(6);
    let carrier = Type::sum(n6.clone(), Type::One);
    let body = Iso::Id(carrier);
    let iso = Iso::trace(n6, body);
    let err = compile(&iso).expect_err("5-payload trace must be rejected");
    // Structured error match (condition 5d): must be Deferred with
    // the payload-cap message, not a substring match.
    match &err {
        CompileError::Deferred(msg) => {
            assert!(
                msg.contains("payload leaves"),
                "expected payload-cap rejection, got Deferred: {msg}"
            );
        }
        other => panic!("expected CompileError::Deferred, got: {other}"),
    }
    eprintln!("5-payload reject: {err}");
}

// ── T-reject-input: 4-leaf input type rejected (condition 2) ────────

#[test]
fn mde_spill_reject_4leaf_input() {
    // A Trace over a 4-LEAF INPUT type (n_input=4) MUST be rejected.
    // With n_input=4, n_input+1=5 > carrier_reg_budget=4, so a live
    // leaf would be forced to Scratch with no relocation → silent
    // corruption. This is the sole barrier against r1 F5.2.
    //
    // u = One (1 inhabitant), a = 4-leaf type (N5 = nat_chain(5) has 4 leaves).
    // carrier = Sum(One, N5) → n_carrier = 1 + max(1, 4) = 5 leaves.
    // body = Id(carrier) → trivially closes.
    let n5 = nat_chain(5); // 4 leaves, 5 inhabitants
    let carrier = Type::sum(Type::One, n5.clone());
    let body = Iso::Id(carrier);
    let iso = Iso::trace(Type::One, body);
    let err = compile(&iso).expect_err("4-leaf input Trace must be rejected");
    // Structured error match: must be Deferred with the live-input guard message.
    match &err {
        CompileError::Deferred(msg) => {
            assert!(
                msg.contains("tag+input") || msg.contains("live-input"),
                "expected live-input-spill rejection, got Deferred: {msg}"
            );
        }
        other => panic!("expected CompileError::Deferred, got: {other}"),
    }
    eprintln!("4-leaf input reject: {err}");
}
