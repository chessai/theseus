//! TP-5 slice-1: Keccak-χ, bit-sliced over Flat(8).
//!
//! χ on a 5-lane Keccak row:  A_i = a_i ⊕ (¬a_{i+1} ∧ a_{i+2})  (mod 5).
//! Bit-sliced: each lane a_i is a Flat(8) register = the SAME lane across 8
//! independent Keccak rows, so one byte-parallel `Ccnot` (r[rd]^=r[rs]&r[rt])
//! computes the χ AND-gate for all 8 rows at once (the 8× bit-slicing win).
//!
//! Rust-composed instruction ROM (feistel.rs style). The program is a
//! hand-composed `Vec<Instr>` assembled directly — it NEVER touches the
//! lowering/recognition path (no source Iso, no recognize_prim, no Deferred).
//!
//! Register map (in-place 2-ancilla schedule):
//!   r0..r4 = lanes a0..a4 (in-place: overwritten with A0..A4)
//!   r5, r6 = ancilla, hold OLD a0, a1 (start 0)
//!
//! Negated-control masked-AND `A_i ^= ¬x ∧ y` = Xori{x,0xFF}; Ccnot{A_i,x,y};
//! Xori{x,0xFF}  (complement control, byte-parallel Ccnot, restore control).
//!
//! ## Rev-A garbage-free framing
//!
//! The 17-instruction in-place χ is **NOT** standalone garbage-free. After a
//! forward-only run, ancillas r5 and r6 hold the OLD a0 and a1 values — dirty
//! in 24 of 32 per-row states (precisely the rows where a0 or a1 is set).
//!
//! Garbage-freeness holds **only under the adjoint**: the full ROM run backward
//! (17 adjoint cycles) restores the input AND zeroes the ancillas exactly
//! (proven by `chi_reversible_roundtrip_eval`). χ is a bijection but NOT an
//! involution — the correct reversibility check is the ROM adjoint, not χ∘χ.
//!
//! The `chi_whole_perrow_domain_packed` test explicitly asserts the dirty-
//! ancilla fact as an L20 regression test: it must FAIL if someone later
//! wrongly forces the ancillas to zero in the forward path.

#![allow(clippy::needless_range_loop)]

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_netlist::lower_iso;
use theseus_spice::chardb::rung0_measured;
use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::compile::run_rom_fast;
use theseus_tisc::energy::balanced_energy;
use theseus_tisc::ring_run::{system_ring, SystemRing};
use theseus_tisc::step::step;
use theseus_tisc::types::{StateVals, MEM_WORDS};

// ── The χ program (in-place, 2 ancilla) ──────────────────────────────

/// One in-place χ over a 5-lane Flat(8) row. r0..r4 = lanes; r5, r6 = 0
/// ancilla on entry. 17 instrs, 5 byte-parallel Ccnots.
fn chi_instrs() -> Vec<Instr> {
    let masked_and = |target: u8, ctrl: u8, other: u8| {
        // target ^= (¬ctrl) & other ; ctrl restored.
        vec![
            Instr::Xori { rd: ctrl, imm: 0xFF },
            Instr::Ccnot { rd: target, rs: ctrl, rt: other },
            Instr::Xori { rd: ctrl, imm: 0xFF },
        ]
    };
    let mut p = Vec::new();
    // Save OLD a0, a1 into ancilla r5, r6 (copy: r5^=r0, r6^=r1).
    p.push(Instr::Xor { rd: 5, rs: 0 }); // r5 = a0
    p.push(Instr::Xor { rd: 6, rs: 1 }); // r6 = a1
    // g0: A0 = a0 ⊕ (¬a1 ∧ a2)   ctrls r1(a1), r2(a2)
    p.extend(masked_and(0, 1, 2));
    // g1: A1 = a1 ⊕ (¬a2 ∧ a3)
    p.extend(masked_and(1, 2, 3));
    // g2: A2 = a2 ⊕ (¬a3 ∧ a4)
    p.extend(masked_and(2, 3, 4));
    // g3: A3 = a3 ⊕ (¬a4 ∧ a0_old)   ctrls r4(a4), r5(saved a0)
    p.extend(masked_and(3, 4, 5));
    // g4: A4 = a4 ⊕ (¬a0_old ∧ a1_old) ctrls r5(saved a0), r6(saved a1)
    p.extend(masked_and(4, 5, 6));
    p
}

/// Estimate: a true garbage-free in-place χ (Rev-B) would add χ⁻¹ cleanup
/// over live outputs to zero r5/r6 — ~20 extra instructions. Deferred to a
/// separate slice.
fn chi_gf_extra_cleanup_len() -> usize {
    20
}

// ── Independent byte-parallel χ reference (NOT the compiled path) ─────

/// Byte-parallel χ over 5 lane-bytes (8 rows packed). out[i] = a[i] ^ (¬a[i+1] & a[i+2]).
fn chi_ref_bytes(a: [u8; 5]) -> [u8; 5] {
    let mut o = [0u8; 5];
    for i in 0..5 {
        o[i] = a[i] ^ (!a[(i + 1) % 5] & a[(i + 2) % 5]);
    }
    o
}

/// Scalar (1-bit-per-lane) χ reference. Independent golden.
fn chi_ref_scalar(a: [u8; 5]) -> [u8; 5] {
    let mut o = [0u8; 5];
    for i in 0..5 {
        o[i] = a[i] ^ ((1 - a[(i + 1) % 5]) & a[(i + 2) % 5]);
    }
    o
}

fn state_with_lanes(a: [u8; 5]) -> StateVals {
    let mut s = StateVals {
        regs: [0; 8],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    s.regs[..5].copy_from_slice(&a);
    s
}

/// Cached χ ROM for tests.
fn chi_rom() -> &'static Vec<u16> {
    static ROM: OnceLock<Vec<u16>> = OnceLock::new();
    ROM.get_or_init(|| assemble(&chi_instrs()).expect("assemble χ"))
}

// ══════════════════════════════════════════════════════════════════════
// 1. Transform correctness: whole per-row 2^5 domain, packed into all 8 lanes
// ══════════════════════════════════════════════════════════════════════

#[test]
fn chi_whole_perrow_domain_packed() {
    let rom = chi_rom();
    let budget = chi_instrs().len();
    let mut dirty_ancilla = 0usize;

    for row in 0u8..32 {
        // Pack the same 5-bit row identically into all 8 bits of each lane.
        let mut a = [0u8; 5];
        for i in 0..5 {
            a[i] = if (row >> i) & 1 == 1 { 0xFF } else { 0x00 };
        }
        let out = run_rom_fast(rom, state_with_lanes(a), budget);

        let expect = chi_ref_bytes(a);
        for i in 0..5 {
            assert_eq!(
                out.regs[i], expect[i],
                "row {row:#07b} lane {i}: ROM {:#04X} != ref {:#04X}",
                out.regs[i], expect[i]
            );
        }
        // Cross-check against scalar χ (bit 0 of each byte).
        let scalar_in = [
            a[0] & 1, a[1] & 1, a[2] & 1, a[3] & 1, a[4] & 1,
        ];
        let scalar_out = chi_ref_scalar(scalar_in);
        for i in 0..5 {
            assert_eq!(out.regs[i] & 1, scalar_out[i], "scalar χ lane {i} row {row}");
        }
        // Rev-A FINDING: ancilla r5, r6 dirtiness after forward-only run.
        // This assertion is the L20 regression test — it must FAIL if
        // someone later wrongly claims/forces the ancillas to zero.
        if out.regs[5] != 0 || out.regs[6] != 0 {
            dirty_ancilla += 1;
            // The dirty value equals OLD a0 / a1 exactly.
            assert_eq!(out.regs[5], a[0], "ancilla r5 holds OLD a0");
            assert_eq!(out.regs[6], a[1], "ancilla r6 holds OLD a1");
        }
    }
    // 24/32 rows have a0 or a1 set → ancilla nonzero: NOT standalone garbage-free.
    // Garbage-free only under the adjoint (Rev-A).
    assert_eq!(
        dirty_ancilla, 24,
        "Rev-A regression: forward-only χ must leave ancillas dirty in exactly \
         24/32 cases (old a0/a1); {dirty_ancilla}/32 observed"
    );
    eprintln!(
        "χ transform correct on all 32 per-row states (packed x8). \
         Rev-A: forward-only leaves ancilla dirty in {dirty_ancilla}/32 cases \
         (garbage-free under adjoint only)."
    );
}

// ══════════════════════════════════════════════════════════════════════
// 2. Discriminating packing: 8 DIFFERENT rows across the 8 bit positions
// ══════════════════════════════════════════════════════════════════════

#[test]
fn chi_discriminating_lane_packing() {
    let rom = chi_rom();
    let budget = chi_instrs().len();

    // 8 distinct rows, one per bit position b (row_b = the 5-bit state in bit b).
    let rows: [u8; 8] = [0b00000, 0b11111, 0b10110, 0b01001, 0b10101, 0b01010, 0b00011, 0b11000];

    // Build the 5 lane-bytes: lane i, bit b = bit i of rows[b].
    let mut a = [0u8; 5];
    for i in 0..5 {
        for b in 0..8 {
            if (rows[b] >> i) & 1 == 1 {
                a[i] |= 1 << b;
            }
        }
    }
    let out = run_rom_fast(rom, state_with_lanes(a), budget);

    // Each bit position must independently equal scalar χ of its row.
    for b in 0..8 {
        let ain = [
            (a[0] >> b) & 1, (a[1] >> b) & 1, (a[2] >> b) & 1, (a[3] >> b) & 1, (a[4] >> b) & 1,
        ];
        let expect = chi_ref_scalar(ain);
        for i in 0..5 {
            assert_eq!(
                (out.regs[i] >> b) & 1, expect[i],
                "discriminating: bit-lane {b} (row {:#07b}) lane {i}", rows[b]
            );
        }
    }
    // And byte-parallel reference agrees on the whole bytes.
    let expect_bytes = chi_ref_bytes(a);
    for i in 0..5 {
        assert_eq!(out.regs[i], expect_bytes[i], "byte-parallel lane {i}");
    }
    eprintln!("χ discriminating packing: 8 distinct rows computed independently in one ROM run.");
}

// ══════════════════════════════════════════════════════════════════════
// 3. Keccak golden vector (independent hand-computed χ)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn chi_keccak_golden_vector() {
    // Hand-computed χ(1,0,1,1,0):
    //   A0 = 1 ^ (¬0 & 1) = 1^1 = 0
    //   A1 = 0 ^ (¬1 & 1) = 0^0 = 0
    //   A2 = 1 ^ (¬1 & 0) = 1^0 = 1
    //   A3 = 1 ^ (¬0 & 1) = 1^1 = 0
    //   A4 = 0 ^ (¬1 & 0) = 0^0 = 0
    // => (0,0,1,0,0)  — PINNED, not derived from the compiled path.
    const GOLDEN_IN: [u8; 5] = [1, 0, 1, 1, 0];
    const GOLDEN_OUT: [u8; 5] = [0, 0, 1, 0, 0];
    assert_eq!(chi_ref_scalar(GOLDEN_IN), GOLDEN_OUT, "scalar ref must match hand golden");

    let rom = chi_rom();
    let budget = chi_instrs().len();
    // Pack the golden row into bit 0 of each lane (rest 0).
    let out = run_rom_fast(rom, state_with_lanes(GOLDEN_IN), budget);
    for i in 0..5 {
        assert_eq!(out.regs[i] & 1, GOLDEN_OUT[i], "golden χ lane {i}");
    }
    eprintln!("χ Keccak golden vector χ(1,0,1,1,0)=(0,0,1,0,0): ROM matches.");
}

// ══════════════════════════════════════════════════════════════════════
// 4. Forward injectivity over domain (structural reversibility proof)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn chi_forward_injective_over_domain() {
    let rom = chi_rom();
    let budget = chi_instrs().len();
    use std::collections::HashSet;
    let mut seen = HashSet::new();
    for row in 0u8..32 {
        let mut a = [0u8; 5];
        for i in 0..5 {
            a[i] = if (row >> i) & 1 == 1 { 0xFF } else { 0x00 };
        }
        let out = run_rom_fast(rom, state_with_lanes(a), budget);
        // Full extended state (r0..r6) must be injective — reversibility.
        let key: [u8; 7] = [
            out.regs[0], out.regs[1], out.regs[2], out.regs[3], out.regs[4],
            out.regs[5], out.regs[6],
        ];
        assert!(seen.insert(key), "collision at row {row}: NOT injective");
    }
    assert_eq!(seen.len(), 32);
    eprintln!("χ forward map injective over all 32 per-row states (reversible embedding).");
}

// ══════════════════════════════════════════════════════════════════════
// 5. Instruction-count / gate-inventory pin (perf accounting)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn chi_instruction_inventory() {
    let instrs = chi_instrs();
    assert_eq!(instrs.len(), 17, "χ = 2 saves + 5×3 gate instrs = 17");
    let ccnots = instrs.iter().filter(|i| matches!(i, Instr::Ccnot { .. })).count();
    let xoris = instrs.iter().filter(|i| matches!(i, Instr::Xori { .. })).count();
    let xors = instrs.iter().filter(|i| matches!(i, Instr::Xor { .. })).count();
    assert_eq!(ccnots, 5, "5 byte-parallel masked-ANDs");
    assert_eq!(xoris, 10, "5 gates × 2 negate/restore Xori");
    assert_eq!(xors, 2, "2 ancilla saves");
    eprintln!(
        "χ inventory: {} instrs = {ccnots} Ccnot + {xoris} Xori + {xors} Xor; \
         Rev-B garbage-free in-place would add ~{} cleanup instrs (χ⁻¹ over live outputs).",
        instrs.len(), chi_gf_extra_cleanup_len()
    );
}

// ══════════════════════════════════════════════════════════════════════
// 6. Reversibility: forward ROM then adjoint restores input + ancilla
//    (HEAVY — eval-based step-iso path)
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "HEAVY: eval-based reversibility (step-iso); run via --ignored / advisor pre-merge"]
fn chi_reversible_roundtrip_eval() {
    let rom = chi_rom();
    let iso = step(rom);
    let back = Iso::sym(iso.clone());
    let t = chi_instrs().len();

    // A few representative rows (packed discriminating byte).
    let a: [u8; 5] = [0xAB, 0xCD, 0x5E, 0x0F, 0xF0];
    let start = state_with_lanes(a);

    // Forward t cycles.
    let mut cur = start.encode();
    let mut fuel = 8_000_000_000u64;
    for _ in 0..t {
        cur = theseus_eval::eval(&iso, cur, &mut fuel).expect("fwd");
    }
    let fwd = StateVals::decode(&cur);
    // Transform correct.
    let expect = chi_ref_bytes(a);
    for i in 0..5 {
        assert_eq!(fwd.regs[i], expect[i], "fwd lane {i}");
    }
    // Backward t cycles restores EVERYTHING (incl. ancilla to 0).
    // This is the Rev-A proof: garbage-free under the adjoint.
    for _ in 0..t {
        cur = theseus_eval::eval(&back, cur, &mut fuel).expect("back");
    }
    let restored = StateVals::decode(&cur);
    assert_eq!(restored, start, "adjoint must restore the exact input state (Rev-A: ancillas → 0)");
    eprintln!(
        "χ ROM reversible (Rev-A): {t} fwd cycles then {t} adjoint cycles \
         restore input + ancillas → 0."
    );
}

// ══════════════════════════════════════════════════════════════════════
// 7. Energy / perf-win: price the χ ROM; bit-slicing factor
//    (HEAVY — requires lower_iso of step netlist)
// ══════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "HEAVY: energy lowering (lower_iso step net) is slow; run via --ignored / advisor pre-merge"]
fn chi_energy_and_bitslicing() {
    let instrs = chi_instrs();
    let rom = chi_rom();
    let db = rung0_measured();
    let t_ramp = 1e-7;

    let step_net = lower_iso(&step(rom)).expect("lower step(χ rom)");
    let step_bd = balanced_energy(&step_net, &db, t_ramp).expect("energy");
    let step_j = step_bd.total_j;
    let b = rom.len(); // straight-line: B = ROM length (PC += 1 each cycle)
    let total_j = step_j * b as f64;

    let ccnots = instrs.iter().filter(|i| matches!(i, Instr::Ccnot { .. })).count();
    assert_eq!(ccnots, 5, "χ has 5 byte-parallel masked-AND gates");

    // Bit-slicing: the SAME ROM/energy computes χ for 8 rows at once (data-
    // independent model). J per row-χ = total_j / 8 vs total_j / 1 un-sliced.
    let j_per_rowchi_sliced = total_j / 8.0;
    let j_per_rowchi_unsliced = total_j / 1.0;

    eprintln!("== χ ROM energy ({} instrs, {ccnots} Ccnots) ==", instrs.len());
    eprintln!("  step_j = {step_j:.4e} J/cycle;  B = {b} cycles;  total = {total_j:.4e} J");
    eprintln!("  bit-sliced (Flat8, 8 rows): {j_per_rowchi_sliced:.4e} J / row-χ");
    eprintln!("  un-sliced  (Flat1, 1 row):  {j_per_rowchi_unsliced:.4e} J / row-χ");
    eprintln!("  BIT-SLICING WIN: 8x throughput for the same ROM energy.");
    assert!(step_j > 0.0);
}

// ══════════════════════════════════════════════════════════════════════
// 8. System-ring authority tier: real CPU (L19)
//    (HEAVY — system_ring build is expensive)
// ══════════════════════════════════════════════════════════════════════

/// Cached system ring for the χ ROM.
fn chi_ring() -> &'static SystemRing {
    static RING: OnceLock<SystemRing> = OnceLock::new();
    RING.get_or_init(|| system_ring(chi_rom()))
}

#[test]
#[ignore = "HEAVY: system-ring authority (L19 real-run); run via --ignored / advisor pre-merge"]
fn chi_system_ring_authority() {
    let ring = chi_ring();
    let t = chi_instrs().len() as u32; // 17 revolutions = full χ

    // ── 1. Whole per-row 32-state domain (packed ×8) ─────────────────
    for row in 0u8..32 {
        let mut a = [0u8; 5];
        for i in 0..5 {
            a[i] = if (row >> i) & 1 == 1 { 0xFF } else { 0x00 };
        }
        let state = state_with_lanes(a);
        let out = ring.run(&state, t);

        let expect = chi_ref_bytes(a);
        for i in 0..5 {
            assert_eq!(
                out.regs[i], expect[i],
                "ring: row {row:#07b} lane {i}: got {:#04X} != ref {:#04X}",
                out.regs[i], expect[i]
            );
        }
        // Cross-check scalar ref on bit 0.
        let scalar_in = [a[0] & 1, a[1] & 1, a[2] & 1, a[3] & 1, a[4] & 1];
        let scalar_out = chi_ref_scalar(scalar_in);
        for i in 0..5 {
            assert_eq!(out.regs[i] & 1, scalar_out[i], "ring scalar χ lane {i} row {row}");
        }
    }

    // ── 2. Discriminating packing (8 distinct rows one-per-bit) ──────
    let disc_rows: [u8; 8] = [0b00000, 0b11111, 0b10110, 0b01001, 0b10101, 0b01010, 0b00011, 0b11000];
    let mut a_disc = [0u8; 5];
    for i in 0..5 {
        for b in 0..8 {
            if (disc_rows[b] >> i) & 1 == 1 {
                a_disc[i] |= 1 << b;
            }
        }
    }
    let disc_out = ring.run(&state_with_lanes(a_disc), t);
    let disc_expect = chi_ref_bytes(a_disc);
    for i in 0..5 {
        assert_eq!(disc_out.regs[i], disc_expect[i], "ring discriminating lane {i}");
    }

    // ── 3. Keccak golden vector ──────────────────────────────────────
    let golden_in: [u8; 5] = [1, 0, 1, 1, 0];
    let golden_expect: [u8; 5] = [0, 0, 1, 0, 0];
    let golden_out = ring.run(&state_with_lanes(golden_in), t);
    for i in 0..5 {
        assert_eq!(golden_out.regs[i] & 1, golden_expect[i], "ring golden χ lane {i}");
    }

    // ── 4. Forward + adjoint restores input, ancillas → 0 ───────────
    // Run forward T then backward T (2T revolutions total on the ring).
    // The system_ring recirculates: the adjoint is the SAME ring running
    // for another T revolutions of the REVERSE ROM. But SystemRing only
    // has the forward system_step. Instead, we verify that running 2T
    // revolutions on the forward ring does NOT restore the input (χ is
    // not an involution), then verify ancilla restoration and bus
    // quiescence per-revolution (structural property of the lowering).
    let sample_a: [u8; 5] = [0xAB, 0xCD, 0x5E, 0x0F, 0xF0];
    let sample_state = state_with_lanes(sample_a);
    let fwd = ring.run(&sample_state, t);
    let fwd_expect = chi_ref_bytes(sample_a);
    for i in 0..5 {
        assert_eq!(fwd.regs[i], fwd_expect[i], "ring sample fwd lane {i}");
    }
    // χ is NOT an involution: χ∘χ ≠ id.
    let double = ring.run(&sample_state, 2 * t);
    assert_ne!(
        [double.regs[0], double.regs[1], double.regs[2], double.regs[3], double.regs[4]],
        sample_a,
        "χ must NOT be an involution"
    );

    // ── 5. Ancilla restoration + bus quiescence (per-revolution) ─────
    // These are structural properties of the system_step lowering: each
    // single revolution restores its ancillas and leaves the bus quiet.
    let check_states: Vec<StateVals> = vec![
        state_with_lanes([0x00, 0x00, 0x00, 0x00, 0x00]),
        state_with_lanes([0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
        state_with_lanes([0xAB, 0xCD, 0x5E, 0x0F, 0xF0]),
        state_with_lanes([1, 0, 1, 1, 0]),
    ];
    for (idx, st) in check_states.iter().enumerate() {
        assert!(
            ring.ancillas_restored(st),
            "system-ring ancillas NOT restored for check state {idx}"
        );
        assert!(
            ring.bus_quiescent(st),
            "system-ring bus NOT quiescent for check state {idx}"
        );
    }

    eprintln!(
        "χ system-ring authority (L19): all 32 per-row states + discriminating + golden \
         match independent ref on the real system_ring. Ancilla restoration + bus \
         quiescence confirmed."
    );
}
