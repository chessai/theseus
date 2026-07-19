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
//!
//! # One consolidated test (memory-safe)
//!
//! All checks run inside a SINGLE `#[test]` so cargo executes them
//! sequentially on one thread. Each `two_body_timing_certificate` /
//! lowering materializes the depth-2839 `cpu_pre` body; running the
//! checks concurrently (the default multi-thread harness) keeps several
//! such lowerings live at once and exhausts memory. Sequencing them here
//! caps peak memory at one lowering, independent of `--test-threads`.

use theseus_core::Type;
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_tisc::ring_run::{
    two_body_timing_certificate, CPU_BODY_N_DATA, MEM_BODY_N_DATA,
};
use theseus_tisc::types::{bus_state_ty, mem_state_ty, state_ty};

fn r_ins(opcode: u16, rd: u16, rs: u16, rt: u16) -> u16 {
    assert!(opcode < 16 && rd < 8 && rs < 8 && rt < 8);
    (1 << 15) | (opcode << 11) | (rd << 8) | (rs << 5) | (rt << 2)
}

// ── F-teeth-1: golden-tuple pin for the MEXCH ROM ──────────────────

fn golden_pin_mexch_rom() {
    // Single-entry MEXCH ROM: MEXCH r2, rt=2 (rd==rt, addr=2)
    let rom = vec![r_ins(0b1001, 2, 0, 2)];
    let t = two_body_timing_certificate(&rom).expect("no rail skew");

    // F-teeth-1: assert the EXACT tuple from measured grounding (§4b).
    // If these fail, STOP and report the real measured values.
    // RE-PINNED (advisor, da9ca8f-era): M-D.b (c229920) added the MexchReg decode
    // arm, growing the cpu_pre/cpu_post netlist (d_pre 2839→2976, d_post 152→289,
    // d_locked/period 3028→3304, delta1 1→0, delta_hold 37→36). validate() still
    // returns Ok (physical lockstep intact) — a legitimate re-pin, not a regression.
    // M-D.b's scoped gate ran `--test ring_two_body` but not `ring_two_body_timing`,
    // so this golden staled undetected until the full-suite run around M-D.b2.f1.
    assert_eq!(t.d_pre, 2976, "GOLDEN PIN: d_pre={} (expected 2976)", t.d_pre);
    assert_eq!(t.d_mem, 35, "GOLDEN PIN: d_mem={} (expected 35)", t.d_mem);
    assert_eq!(t.d_post, 289, "GOLDEN PIN: d_post={} (expected 289)", t.d_post);
    assert_eq!(t.delta1, 0, "GOLDEN PIN: delta1={} (expected 0)", t.delta1);
    assert_eq!(t.delta2, 1, "GOLDEN PIN: delta2={} (expected 1)", t.delta2);
    assert_eq!(
        t.delta_hold, 36,
        "GOLDEN PIN: delta_hold={} (expected 36)",
        t.delta_hold
    );
    assert_eq!(
        t.d_locked, 3304,
        "GOLDEN PIN: d_locked={} (expected 3304)",
        t.d_locked
    );
    assert_eq!(t.period, 3304, "GOLDEN PIN: period={} (expected 3304)", t.period);

    // validate() must pass on the golden ROM
    t.validate().expect("validate() failed on MEXCH golden ROM");
}

// ── validate() Ok for a handful of ROMs ────────────────────────────

fn validate_add_rom() {
    // Non-MEXCH ROM: single ADD r0, r1, rt=3
    let rom = vec![r_ins(0b0000, 0, 1, 3)];
    two_body_timing_certificate(&rom)
        .expect("no rail skew")
        .validate()
        .expect("validate() failed on ADD ROM");
}

fn validate_swapbr_rom() {
    // Branch ROM: SWAPBR r1
    let rom = vec![r_ins(0b1000, 1, 0, 0)];
    two_body_timing_certificate(&rom)
        .expect("no rail skew")
        .validate()
        .expect("validate() failed on SWAPBR ROM");
}

// ── §F line-audit: input_ty checks on all three bodies ─────────────

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
    assert_eq!(n_data_pre, CPU_BODY_N_DATA, "cpu_pre n_data must be {CPU_BODY_N_DATA}");

    // mem_body
    let iso_mem = theseus_tisc::decoder::mem_body();
    let (net_mem, anc_mem) =
        lower_iso_with_ancillas(&iso_mem).expect("mem_body lowers");
    let n_data_mem = net_mem.inputs.len() - anc_mem.len();
    assert_eq!(
        net_mem.input_ty, mem_bus_ty,
        "mem_body input_ty must be Prod(mem_state_ty(), bus_state_ty())"
    );
    assert_eq!(n_data_mem, MEM_BODY_N_DATA, "mem_body n_data must be {MEM_BODY_N_DATA}");

    // cpu_post
    let iso_post = theseus_tisc::step::cpu_post(&rom);
    let (net_post, anc_post) =
        lower_iso_with_ancillas(&iso_post).expect("cpu_post lowers");
    let n_data_post = net_post.inputs.len() - anc_post.len();
    assert_eq!(
        net_post.input_ty, cpu_bus_ty,
        "cpu_post input_ty must be Prod(state_ty(), bus_state_ty())"
    );
    assert_eq!(n_data_post, CPU_BODY_N_DATA, "cpu_post n_data must be {CPU_BODY_N_DATA}");
}

// ── Mutation / negative tests: prove validate() has real teeth ─────

/// Corrupt a valid certificate one field at a time and assert validate()
/// rejects every mutation. Proves the certificate is not self-fulfilling
/// theater: each mutation produces a concrete Err.
///
/// # Which check fires for each mutation
///
/// Most single-field mutations are caught by the independent re-derivation
/// comparison (the first block of checks in validate()), because validate()
/// re-derives delta1/delta2/delta_hold/d_locked/period from d_pre/d_mem/d_post
/// and compares them to the stored fields. So mutating a stored derived field
/// (delta1, delta2, delta_hold, d_locked, period) triggers a "mismatch" error,
/// while mutating a basis field (d_pre, d_mem, d_post) changes what the
/// re-derivation computes and triggers a mismatch against the (now-stale)
/// stored derived fields.
///
/// Assertions A (co-arrival), B (revolution skew), C (phase closure),
/// D (period lock), and E (minimality) are DEFENSE-IN-DEPTH: they are
/// implied by the re-derivation comparison for single-field mutations
/// (the re-derivation fires first). They remain valuable as guards against
/// multi-field correlated corruption that might pass re-derivation.
///
/// Assertion G (bus-pad vector lengths) IS independently reachable by
/// truncating a pad vector — re-derivation doesn't examine pad vectors.
fn validate_rejects_mutations() {
    // PHASE_COUNT = 4 (theseus_netlist::PHASE_COUNT)
    const P: u32 = 4;

    let rom = vec![r_ins(0b1001, 2, 0, 2)]; // MEXCH
    let valid = two_body_timing_certificate(&rom).expect("no rail skew");
    valid
        .validate()
        .expect("pristine certificate must validate");

    // Helper: clone, mutate, assert Err, optionally check message substring.
    macro_rules! assert_rejects {
        ($label:expr, $mutate:expr) => {{
            let mut bad = valid.clone();
            $mutate(&mut bad);
            let result = bad.validate();
            assert!(
                result.is_err(),
                "validate() must reject mutation '{}' but returned Ok",
                $label
            );
            let msg = result.unwrap_err();
            assert!(
                !msg.is_empty(),
                "Err message for mutation '{}' must be non-empty",
                $label
            );
        }};
    }

    // ── Derived-field mutations (caught by re-derivation comparison) ──

    // delta_hold += 1: re-derivation computes delta_hold from delta1+d_mem+delta2,
    // stored delta_hold no longer matches → "delta_hold mismatch".
    // (Defense-in-depth assertion A would also catch this, but re-derivation fires first.)
    assert_rejects!("delta_hold += 1", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.delta_hold += 1;
    });

    // F1 revolution-skew: period = d_locked + P (one full phase-period late).
    // Re-derivation computes period == d_locked; stored period is d_locked + P
    // → "period mismatch". This explicitly reproduces the F1 timing-skew bug
    // where the bus return is a full period late.
    assert_rejects!("F1 revolution-skew: period = d_locked + P", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.period = t.d_locked + P;
    });

    // d_locked += P: re-derivation computes d_locked from the basis fields;
    // stored d_locked no longer matches → "d_locked mismatch".
    assert_rejects!("d_locked += P", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.d_locked += P;
    });

    // delta1 += 1: re-derivation computes delta1 from d_pre; stored delta1
    // no longer matches → "delta1 mismatch".
    assert_rejects!("delta1 += 1", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.delta1 += 1;
    });

    // delta2 += 1: re-derivation computes delta2 from d_pre+delta1+d_mem;
    // stored delta2 no longer matches → "delta2 mismatch".
    assert_rejects!("delta2 += 1", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.delta2 += 1;
    });

    // ── Basis-field mutations (change re-derivation result → mismatch) ──

    // d_pre += 1: changes re-derived delta1 (different phase alignment)
    // → "delta1 mismatch" (or whichever derived field diverges first).
    assert_rejects!("d_pre += 1", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.d_pre += 1;
    });

    // d_mem += 1: changes re-derived delta2 → "delta2 mismatch".
    assert_rejects!("d_mem += 1", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.d_mem += 1;
    });

    // d_post += P: d_post feeds d_locked via period-alignment (align-up to P), so a
    // sub-period perturbation (+1) is ABSORBED by the delta_hold slack — that is correct
    // physics, not a cert gap (the old +1 happened to cross a boundary only because the
    // pre-M-D.b d_post=152 sat on one; the re-pinned d_post=289 does not). Perturb by a
    // full period P to shift the aligned d_locked → "d_locked mismatch". (delta1/delta2
    // are mod-P residues, so d_pre/d_mem +1 above ARE caught; d_post's alignment is not
    // +1-sensitive.)
    assert_rejects!("d_post += P", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.d_post += P;
    });

    // ── Assertion G: independently reachable (not implied by re-derivation) ──

    // Truncate pre_bus_pads: re-derivation doesn't check pad vectors,
    // so this is caught by assertion G's length check directly.
    assert_rejects!("truncate pre_bus_pads", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.pre_bus_pads.pop();
    });

    // Truncate mem_bus_pads: same — assertion G length check.
    assert_rejects!("truncate mem_bus_pads", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.mem_bus_pads.pop();
    });

    // Truncate post_bus_pads: same — assertion G length check.
    assert_rejects!("truncate post_bus_pads", |t: &mut theseus_tisc::ring_run::TwoBodyTiming| {
        t.post_bus_pads.pop();
    });
}

/// All Stage 2b-lite certificate checks, run SEQUENTIALLY in one test to
/// cap peak memory at a single body lowering (see module docs).
#[test]
fn stage2b_lite_timing_certificate() {
    golden_pin_mexch_rom();
    validate_add_rom();
    validate_swapbr_rom();
    line_audit_input_types();
    validate_rejects_mutations();
}
