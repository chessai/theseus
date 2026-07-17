//! Negative / failure-path tests for stable guards in theseus-tisc.
//!
//! These cover rejection paths that must remain correct even as the
//! crate evolves — reserved opcodes, unrecognized prims, and
//! addressing precondition panics.

use std::collections::BTreeMap;
use theseus_core::prim::PrimDef;
use theseus_core::{Iso, Type, Value};
use theseus_tisc::asm::decode;
use theseus_tisc::compile::{compile, CompileError};

// ── asm::decode: reserved opcodes → None ────────────────────────────

#[test]
fn decode_reserved_r_format_opcode() {
    // R-format: bit15=1, opcode 0b1111 (reserved), rd=rs=rt=0, pad=0.
    // Layout: [1 | 1111 | 000 | 000 | 000 | 00] = 0xF800
    assert_eq!(decode(0xF800), None);
}

#[test]
fn decode_reserved_r_format_opcode_1010() {
    // R-format opcode 0b1010 is also reserved (only 0b0000..0b1001 are valid).
    // [1 | 1010 | 000 | 000 | 000 | 00] = 0x8000 | (0b1010 << 11) = 0xD000
    assert_eq!(decode(0xD000), None);
}

#[test]
fn decode_reserved_i_format_opcode_101() {
    // I-format: bit15=0, opc 0b101, rd=0, imm=0, pad=0.
    // Layout: [0 | 101 | 000 | 00000000 | 0] = 0x5000
    assert_eq!(decode(0x5000), None);
}

#[test]
fn decode_reserved_i_format_opcode_110() {
    // I-format: opc 0b110.
    // [0 | 110 | 000 | 00000000 | 0] = 0x6000
    assert_eq!(decode(0x6000), None);
}

#[test]
fn decode_reserved_i_format_opcode_111() {
    // I-format: opc 0b111.
    // [0 | 111 | 000 | 00000000 | 0] = 0x7000
    assert_eq!(decode(0x7000), None);
}

#[test]
fn decode_r_format_nonzero_pad_bits() {
    // R-format word with pad bits [1:0] set.
    // 0x8000 (minimal valid R-format header) | 0b11 = 0x8003.
    // decode checks `word & 0b11 != 0` and returns None.
    assert_eq!(decode(0x8003), None);
}

#[test]
fn decode_r_format_single_pad_bit() {
    // R-format word with only pad bit 0 set.
    // 0x8000 | 0b01 = 0x8001.
    assert_eq!(decode(0x8001), None);
}

#[test]
fn decode_i_format_nonzero_pad_bit() {
    // I-format word with pad bit [0] set.
    // 0x0001: bit15=0 (I-format), opc=0b000, rd=0, imm=0, pad=1.
    // decode checks `word & 1 != 0` and returns None.
    assert_eq!(decode(0x0001), None);
}

// ── compile: UnrecognizedPrim ───────────────────────────────────────

#[test]
fn compile_unrecognized_prim() {
    // A prim on One→One (0 leaves) doesn't match any recognized
    // 1-/2-/3-register pattern, so it falls through to UnrecognizedPrim.
    let forward = BTreeMap::from([(Value::Unit, Value::Unit)]);
    let backward = forward.clone();
    let def = PrimDef {
        forward_name: "bogus".into(),
        backward_name: "bogus_inv".into(),
        input_ty: Type::One,
        output_ty: Type::One,
        forward,
        backward,
    };
    let iso = Iso::Prim(Box::new(def));
    let result = compile(&iso);
    assert!(
        matches!(result, Err(CompileError::UnrecognizedPrim(_))),
        "expected UnrecognizedPrim, got {result:?}"
    );
}

// ── addressing::at_fields_n: precondition panics ────────────────────

#[test]
#[should_panic(expected = "duplicate field index")]
fn at_fields_n_duplicate_index() {
    let op = Iso::Id(theseus_tisc::addressing::fields(2));
    theseus_tisc::addressing::at_fields_n(op, &[3, 3], 4);
}

#[test]
#[should_panic(expected = "out of range")]
fn at_fields_n_out_of_range() {
    let op = Iso::Id(theseus_tisc::addressing::fields(1));
    // n_fields=4, so index 4 is out of range (valid: 0..3).
    theseus_tisc::addressing::at_fields_n(op, &[4], 4);
}
