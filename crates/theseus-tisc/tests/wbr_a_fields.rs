//! W_BR-a -- the BR-addressing sites generalized to `br_fields()`.
//!
//! This slice makes `pc_add_br_with`'s BR operand + field list,
//! `encode_with`/`decode_with` BR marshalling, `instr::swapbr_with`, and the
//! new `br_hi_field()`/`br_word_field()` helpers all driven by
//! `layout.br_fields()` (= `w_br / w_ctl`; 1 at the default w_br=8, 2 at
//! w_br=16), so a hand-built `w_br=16` layout is well-formed (type-checks +
//! lowers) while the default (w_br=8) path stays BYTE-IDENTICAL.
//!
//! **The primary byte-identity guard lives elsewhere:** the
//! `poc_step_net_bit_identical_across_type_repr` golden in `tests/energy.rs`
//! lowers `step(Layout::default())` and asserts a fixed netlist fingerprint --
//! it proves the DEFAULT (w_br=8) path is unchanged (no re-pin). This file
//! proves the NEW capability: a hand-built w_br=16 layout gains exactly one BR
//! field, type-checks, and round-trips a 16-bit BR through encode/decode. It
//! does NOT assert w_br=16 RUN-correctness (the two-byte come-from δu mint +
//! sext is W_BR-b, a separate highest-bar slice).

use theseus_core::type_of;
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

/// A w_br=16 layout: BR occupies two 8-bit CONTROL fields (BR_lo + BR_hi).
/// Everything else is the default so the change under test (BR width) is
/// isolated (w_pc stays 16 → pc_fields()=2).
fn layout_w_br16() -> Layout {
    Layout {
        w_br: 16,
        ..Layout::default()
    }
}

/// A tiny ROM (the actual instruction content is irrelevant to the field-count
/// / type-check assertions here).
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
// 1. The w_br=16 layout adds exactly one BR field.
// ══════════════════════════════════════════════════════════════════════

#[test]
fn w_br16_adds_one_br_field() {
    let d = Layout::default();
    let s = layout_w_br16();

    // Default BR occupies 1 field (BR_lo); w_br=16 occupies 2 (BR_lo + BR_hi).
    assert_eq!(d.br_fields(), 1, "default w_br=8 → 1 BR field");
    assert_eq!(s.br_fields(), 2, "w_br=16 → 2 BR fields (BR_lo + BR_hi)");

    // Adding BR_hi grows the CPU field count by exactly one; every other
    // layout axis is unchanged.
    assert_eq!(
        s.num_cpu_fields(),
        d.num_cpu_fields() + 1,
        "w_br=16 field count = default count plus the appended BR_hi field"
    );

    // The state carrier grows by exactly one w_ctl (=8) field.
    assert_eq!(
        s.state_bits(),
        d.state_bits() + d.w_ctl,
        "w_br=16 state grows by one control word (the appended BR_hi)"
    );

    // BR_hi lands right after PC_hi (appended region order:
    // [PC-hi × (pc_fields-1), BR-hi × (br_fields-1)]).
    assert_eq!(
        s.br_hi_field(),
        s.pc_hi_field() + 1,
        "BR_hi is appended immediately after PC_hi at the default w_pc=16"
    );
    // The BR word enumerator agrees with the low/hi helpers.
    assert_eq!(s.br_word_field(0), s.br_field(), "BR word 0 = BR_lo");
    assert_eq!(s.br_word_field(1), s.br_hi_field(), "BR word 1 = BR_hi");
    // BR_hi is a fresh field, distinct from every other CPU field it neighbors.
    assert_ne!(s.br_hi_field(), s.pc_hi_field(), "BR_hi ≠ PC_hi");
    assert_ne!(s.br_hi_field(), s.br_field(), "BR_hi ≠ BR_lo");
}

// ══════════════════════════════════════════════════════════════════════
// 2. `step_with(&layout_w_br16, rom)` TYPE-CHECKS (the well-formedness proof).
//    `type_of` walks the whole step iso and verifies every Seq/Times/etc.
//    composition's boundary types agree -- this is exactly where a bad BR field
//    index or a wire-count/field-list mismatch in `pc_add_br_with` would
//    surface, without paying for a full `lower_iso`.
// ══════════════════════════════════════════════════════════════════════

#[test]
fn w_br16_step_type_checks() {
    let layout = layout_w_br16();
    let rom = tiny_rom();

    // Building the step iso exercises pc_add_br_with over the 2-BR-field
    // carrier (its BR operand + field list must span br_fields()=2 words).
    let iso = theseus_tisc::step::step_with(&layout, &rom);

    let (a, b) = type_of(&iso).expect("w_br=16 step must type-check");
    let sty = state_ty_with(&layout);
    assert_eq!(a, sty, "step input type = widened CpuState (w_br=16)");
    assert_eq!(b, sty, "step is an endo-iso on the widened CpuState");
    assert_eq!(a, b, "step is an endomorphism");
}

// The BR-widening builders type-check standalone at w_br=16 too.
#[test]
fn w_br16_builders_type_check() {
    let layout = layout_w_br16();
    let sty = state_ty_with(&layout);

    let add = theseus_tisc::step::pc_add_br_with(&layout);
    let (a, b) = type_of(&add).expect("pc_add_br_with must type-check at w_br=16");
    assert_eq!(a, sty, "pc_add_br_with input = widened CpuState");
    assert_eq!(a, b, "pc_add_br_with is an endomorphism");

    // The standalone assembler swapbr helper spans both BR words at w_br=16.
    let sb = theseus_tisc::instr::swapbr_with(&layout, 0);
    let (c, d) = type_of(&sb).expect("swapbr_with must type-check at w_br=16");
    assert_eq!(c, sty, "swapbr_with input = widened CpuState");
    assert_eq!(c, d, "swapbr_with is an endomorphism");
}

// ══════════════════════════════════════════════════════════════════════
// 3. encode_with/decode_with round-trip a 16-bit BR on the w_br=16 carrier
//    (BR_hi IS marshalled). Proves the marshalling branch on `br_fields()` is
//    coherent with the widened state_ty.
// ══════════════════════════════════════════════════════════════════════

#[test]
fn w_br16_encode_decode_roundtrip_16bit_br() {
    let layout = layout_w_br16();
    let sv = StateVals {
        regs: {
            let mut r = [0u8; NUM_REGS];
            r[0] = 0x2A;
            r
        },
        pc: 0x0137,      // 16-bit PC (default w_pc=16)
        br: 0xBEEF,      // 16-bit BR -- the high byte must survive the round-trip
        ir: 0xDEAD_BEEF,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    let val = sv.encode_with(&layout);
    let back = StateVals::decode_with(&layout, &val);
    assert_eq!(back.br, 0xBEEF, "full 16-bit BR round-trips at w_br=16");
    assert_eq!(back.pc, sv.pc, "PC round-trips (undisturbed by the BR widening)");
    assert_eq!(back.regs[0], sv.regs[0], "r0 round-trips");
    assert_eq!(back.ir, sv.ir, "IR round-trips");
}

// And the default (w_br=8) carrier still round-trips an 8-bit BR (byte path
// intact) and REJECTS a >8-bit BR loudly (no silent truncation).
#[test]
fn default_w_br8_roundtrips_low_byte() {
    let layout = Layout::default();
    let sv = StateVals {
        regs: [0u8; NUM_REGS],
        pc: 0x0100,
        br: 0x00A5, // fits the single 8-bit BR field
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    let back = StateVals::decode_with(&layout, &sv.encode_with(&layout));
    assert_eq!(back.br, 0xA5, "8-bit BR round-trips on the default carrier");
}

#[test]
#[should_panic(expected = "exceeds w_br")]
fn default_w_br8_rejects_wide_br() {
    let layout = Layout::default();
    let sv = StateVals {
        regs: [0u8; NUM_REGS],
        pc: 0,
        br: 0x0100, // > 8 bits: no BR_hi field on the default carrier
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    // encode_with must panic rather than silently drop the high BR byte.
    let _ = sv.encode_with(&layout);
}

// ══════════════════════════════════════════════════════════════════════
// 4. assert_br_carrier regression (mirror b22a MAJOR-1): a non-byte-multiple
//    w_br fails LOUDLY at the BR-builder entry.
// ══════════════════════════════════════════════════════════════════════
//
// `br_fields()` is floor division, so `w_br=12` (w_ctl=8) gives br_fields()=1
// but w_br=12 -- the BR operand sext would read BR bits 8..11 off a field that
// only holds 8 bits (aliasing a neighbor). The `assert_br_carrier` guard
// (called from `pc_add_br_with`) makes that a loud panic. No valid layout
// (w_br ∈ {8,16}) trips it.

#[test]
#[should_panic(expected = "BR carrier invariant violated")]
fn non_byte_multiple_w_br_panics_add() {
    let bad = Layout {
        w_br: 12,
        ..Layout::default()
    };
    let _ = theseus_tisc::step::pc_add_br_with(&bad);
}

// A w_br=16 layout (a VALID multiple) must NOT panic -- the guard is not
// over-eager.
#[test]
fn valid_w_br16_builder_does_not_panic() {
    let s = layout_w_br16();
    let _ = theseus_tisc::step::pc_add_br_with(&s);
}
