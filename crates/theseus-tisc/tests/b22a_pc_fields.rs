//! B-2.2a — the 2-PC-field builders generalized to `pc_fields()`.
//!
//! This slice makes `fetch_fields`/`FetchWires`, `pc_add_br_with`/
//! `pc_inc_with` field lists + wire-counts, and `encode_with`/`decode_with`
//! PC marshalling all driven by `layout.pc_fields()` (= `w_pc / w_ctl`; 2 at
//! the default w_pc=16, 1 at w_pc=8), plus threads the `fetch_addr_bits`
//! clamp to `layout.w_pc`. Nothing derives w_pc=8 in the run path yet — this
//! is the pure enabling refactor. Actual w_pc=8 run-correctness is B-2.2b.
//!
//! **The primary byte-identity guard lives elsewhere:** the
//! `poc_step_net_bit_identical_across_type_repr` golden in `tests/energy.rs`
//! (also `b21c_data_word.rs`) lowers `step(Layout::default())` and asserts a
//! fixed netlist fingerprint — it proves the DEFAULT (w_pc=16) path is
//! unchanged. This file proves the NEW capability: a hand-built w_pc=8 layout
//! is now well-formed (type-checks + lowers) and drops the PC_hi field.

use theseus_core::type_of;
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

/// A w_pc=8 layout: PC occupies a single 8-bit CONTROL field (no PC_hi).
/// Everything else is the default so the change under test (PC width) is
/// isolated.
fn layout_w_pc8() -> Layout {
    Layout {
        w_pc: 8,
        ..Layout::default()
    }
}

/// A tiny ROM (well under the w_pc=8 narrow-fetch cap of 2^(w_pc-1) = 128).
#[allow(clippy::identity_op)]
fn tiny_rom() -> Vec<u32> {
    vec![
        // addr 0: ROTLI r0, #1 (A-0 I-format: opc=0b011=ROTLI, rd=0, imm=1)
        (3u32 << 25) | (0 << 20) | 1,
        // addr 1: reserved I-format opc 0b101 (decodes None)
        (5u32 << 25) | (1 << 20) | 0xFF,
    ]
}

// ══════════════════════════════════════════════════════════════════════
// 1. The w_pc=8 layout drops exactly the PC_hi field.
// ══════════════════════════════════════════════════════════════════════

#[test]
fn w_pc8_drops_pc_hi_field() {
    let d = Layout::default();
    let s = layout_w_pc8();

    // Default PC occupies 2 fields (PC_lo + PC_hi); w_pc=8 occupies 1.
    assert_eq!(d.pc_fields(), 2, "default w_pc=16 → 2 PC fields");
    assert_eq!(s.pc_fields(), 1, "w_pc=8 → 1 PC field (no PC_hi)");

    // Dropping PC_hi shrinks the CPU field count by exactly one; every other
    // layout axis is unchanged.
    assert_eq!(
        s.num_cpu_fields(),
        d.num_cpu_fields() - 1,
        "w_pc=8 field count = default count minus the dropped PC_hi field"
    );

    // The state carrier shrinks by exactly one w_ctl (=8) field.
    assert_eq!(
        s.state_bits(),
        d.state_bits() - d.w_ctl,
        "w_pc=8 state shrinks by one control word (the dropped PC_hi)"
    );
}

// ══════════════════════════════════════════════════════════════════════
// 2. `step_with(&layout_w_pc8, rom)` TYPE-CHECKS (the well-formedness proof).
//    `type_of` walks the whole step iso and verifies every Seq/Times/etc.
//    composition's boundary types agree — this is exactly where a bad PC
//    field index or a wire-count/field-list mismatch (the R-1 misroute) would
//    surface, without paying for a full `lower_iso`.
// ══════════════════════════════════════════════════════════════════════

#[test]
fn w_pc8_step_type_checks() {
    let layout = layout_w_pc8();
    let rom = tiny_rom();

    // Building the step iso exercises fetch_with + decode_execute_l +
    // pc_add_br_with + pc_inc_with over the reduced (1-PC-field) carrier.
    let iso = theseus_tisc::step::step_with(&layout, &rom);

    let (a, b) = type_of(&iso).expect("w_pc=8 step must type-check");
    let sty = state_ty_with(&layout);
    assert_eq!(a, sty, "step input type = reduced CpuState");
    assert_eq!(b, sty, "step is an endo-iso on the reduced CpuState");
    assert_eq!(a, b, "step is an endomorphism");
}

// ══════════════════════════════════════════════════════════════════════
// 3. encode_with/decode_with round-trip on the w_pc=8 carrier (PC_hi is not
//    marshalled). Proves the marshalling branch on `pc_fields()` is coherent
//    with the reduced state_ty.
// ══════════════════════════════════════════════════════════════════════

#[test]
fn w_pc8_encode_decode_roundtrip() {
    let layout = layout_w_pc8();
    let sv = StateVals {
        regs: {
            let mut r = [0u8; NUM_REGS];
            r[0] = 0x2A;
            r
        },
        pc: 0x37, // 8-bit PC (high byte must be 0 at w_pc=8)
        br: 0x11,
        ir: 0xDEAD_BEEF,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    // encode_with panics if the value does not inhabit state_ty_with(layout)
    // (via value_from_bits); decode_with asserts the bit-length matches the
    // reduced state_bits(). So a clean round-trip proves the marshalling is
    // coherent with the 1-PC-field carrier.
    let val = sv.encode_with(&layout);
    let back = StateVals::decode_with(&layout, &val);
    assert_eq!(back.pc, sv.pc, "PC round-trips at w_pc=8 (no PC_hi)");
    assert_eq!(back.br, sv.br, "BR round-trips");
    assert_eq!(back.regs[0], sv.regs[0], "r0 round-trips");
    assert_eq!(back.ir, sv.ir, "IR round-trips");
}
