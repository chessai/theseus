//! Two-body timing realizability certificate (M-C2 Stage 2b-lite).
//!
//! Proves the two-body ring (cpu_pre / mem_body / cpu_post) can be
//! clocked as two concurrent lockstep loops joined by a bus cross-edge.
//! Pure integer arithmetic over lowered-body depths; no domain
//! enumeration, no eval, bounded to a few ROMs × one lowering each.
//!
//! # Assertions
//!
//! - F-teeth-1: golden-tuple pin for the MEXCH ROM
//! - validate() Ok for a handful of ROMs (MEXCH, ADD, SWAPBR)
//! - §F line-audit: input_ty checks on all three bodies

use theseus_core::Type;
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_tisc::ring_run::{
    two_body_timing_certificate,
    CPU_BODY_N_DATA, MEM_BODY_N_DATA,
};
use theseus_tisc::types::{bus_state_ty, mem_state_ty, state_ty};

// ── Instruction encoders ─────────────────────────────────────────

fn r_ins(opcode: u16, rd: u16, rs: u16, rt: u16) -> u16 {
    assert!(opcode < 16 && rd < 8 && rs < 8 && rt < 8);
    (1 << 15) | (opcode << 11) | (rd << 8) | (rs << 5) | (rt << 2)
}

// ══════════════════════════════════════════════════════════════════════
// 1. F-teeth-1: golden-tuple pin for the MEXCH ROM
// ══════════════════════════════════════════════════════════════════════

#[test]
fn golden_pin_mexch_rom() {
    // Single-entry MEXCH ROM: MEXCH r2, rt=2 (rd==rt, addr=2)
    let rom = vec![r_ins(0b1001, 2, 0, 2)];
    let t = two_body_timing_certificate(&rom);

    // F-teeth-1: assert the EXACT tuple from measured grounding (§4b).
    // If these fail, STOP and report the real measured values.
    assert_eq!(
        t.d_pre, 2839,
        "GOLDEN PIN: d_pre={} (expected 2839)",
        t.d_pre
    );
    assert_eq!(
        t.d_mem, 35,
        "GOLDEN PIN: d_mem={} (expected 35)",
        t.d_mem
    );
    assert_eq!(
        t.d_post, 152,
        "GOLDEN PIN: d_post={} (expected 152)",
        t.d_post
    );
    assert_eq!(
        t.delta1, 1,
        "GOLDEN PIN: delta1={} (expected 1)",
        t.delta1
    );
    assert_eq!(
        t.delta2, 1,
        "GOLDEN PIN: delta2={} (expected 1)",
        t.delta2
    );
    assert_eq!(
        t.delta_hold, 37,
        "GOLDEN PIN: delta_hold={} (expected 37)",
        t.delta_hold
    );
    assert_eq!(
        t.d_locked, 3028,
        "GOLDEN PIN: d_locked={} (expected 3028)",
        t.d_locked
    );
    assert_eq!(
        t.period, 3028,
        "GOLDEN PIN: period={} (expected 3028)",
        t.period
    );

    // validate() must pass on the golden ROM
    t.validate().expect("validate() failed on MEXCH golden ROM");
}

// ══════════════════════════════════════════════════════════════════════
// 2. validate() Ok for a handful of ROMs
// ══════════════════════════════════════════════════════════════════════

#[test]
fn validate_add_rom() {
    // Non-MEXCH ROM: single ADD r0, r1, rt=3
    let rom = vec![r_ins(0b0000, 0, 1, 3)];
    let t = two_body_timing_certificate(&rom);
    t.validate()
        .expect("validate() failed on ADD ROM");
}

#[test]
fn validate_swapbr_rom() {
    // Branch ROM: SWAPBR r1
    let rom = vec![r_ins(0b1000, 1, 0, 0)];
    let t = two_body_timing_certificate(&rom);
    t.validate()
        .expect("validate() failed on SWAPBR ROM");
}

// ══════════════════════════════════════════════════════════════════════
// 3. §F line-audit: input_ty checks on all three bodies
// ══════════════════════════════════════════════════════════════════════

#[test]
fn line_audit_input_types() {
    let cpu_bus_ty = Type::prod(state_ty(), bus_state_ty());
    let mem_bus_ty = Type::prod(mem_state_ty(), bus_state_ty());

    let rom = vec![r_ins(0b1001, 2, 0, 2)]; // MEXCH

    // cpu_pre
    let iso_pre = theseus_tisc::step::cpu_pre(&rom);
    let (net_pre, anc_pre) =
        lower_iso_with_ancillas(&iso_pre).expect("cpu_pre lowers");
    let n_data_pre = net_pre.inputs.len() - anc_pre.len();
    assert_eq!(
        net_pre.input_ty, cpu_bus_ty,
        "cpu_pre input_ty must be Prod(state_ty(), bus_state_ty())"
    );
    assert_eq!(
        n_data_pre, CPU_BODY_N_DATA,
        "cpu_pre n_data must be {CPU_BODY_N_DATA}"
    );

    // mem_body
    let iso_mem = theseus_tisc::decoder::mem_body();
    let (net_mem, anc_mem) =
        lower_iso_with_ancillas(&iso_mem).expect("mem_body lowers");
    let n_data_mem = net_mem.inputs.len() - anc_mem.len();
    assert_eq!(
        net_mem.input_ty, mem_bus_ty,
        "mem_body input_ty must be Prod(mem_state_ty(), bus_state_ty())"
    );
    assert_eq!(
        n_data_mem, MEM_BODY_N_DATA,
        "mem_body n_data must be {MEM_BODY_N_DATA}"
    );

    // cpu_post
    let iso_post = theseus_tisc::step::cpu_post(&rom);
    let (net_post, anc_post) =
        lower_iso_with_ancillas(&iso_post).expect("cpu_post lowers");
    let n_data_post = net_post.inputs.len() - anc_post.len();
    assert_eq!(
        net_post.input_ty, cpu_bus_ty,
        "cpu_post input_ty must be Prod(state_ty(), bus_state_ty())"
    );
    assert_eq!(
        n_data_post, CPU_BODY_N_DATA,
        "cpu_post n_data must be {CPU_BODY_N_DATA}"
    );
}
