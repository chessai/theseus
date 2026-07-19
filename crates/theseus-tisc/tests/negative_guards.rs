//! Negative / failure-path tests for stable guards in theseus-tisc.
//!
//! These cover rejection paths that must remain correct even as the
//! crate evolves — reserved opcodes, unrecognized prims, and
//! addressing precondition panics.

use std::collections::BTreeMap;
use theseus_core::prim::PrimDef;
use theseus_core::{Iso, Type, Value};
use theseus_tisc::asm::{decode, encode, assemble, AsmError, Instr};
use theseus_tisc::compile::{compile, CompileError};

// ── asm::decode: reserved opcodes → None ────────────────────────────

#[test]
fn decode_reserved_r_format_opcode() {
    // R-format: bit15=1, opcode 0b1111 (reserved), rd=rs=rt=0, pad=0.
    // Layout: [1 | 1111 | 000 | 000 | 000 | 00] = 0xF800
    assert_eq!(decode(0xF800), None);
}

#[test]
fn decode_mexchr_opcode_1010() {
    // R-format opcode 0b1010 is MEXCHR (M-D.b: data-dependent memory addressing).
    // [1 | 1010 | 000 | 000 | 000 | 00] = 0x8000 | (0b1010 << 11) = 0xD000
    // rd=0, rs=0 (=ra), rt=0 — decodes to MexchReg{rd:0, ra:0}.
    assert_eq!(decode(0xD000), Some(Instr::MexchReg { rd: 0, ra: 0 }));
}

#[test]
fn decode_r_format_opcode_1011_is_ccnot() {
    // R-format opcode 0b1011 is CCNOT (bit-0 Toffoli).
    // [1 | 1011 | 000 | 000 | 000 | 00] = 0x8000 | (0b1011 << 11) = 0xD800
    // rd=0, rs=0, rt=0
    assert_eq!(decode(0xD800), Some(Instr::Ccnot { rd: 0, rs: 0, rt: 0 }));
    // With distinct operands: rd=1, rs=2, rt=3
    // [1 | 1011 | 001 | 010 | 011 | 00] = 0xD94C
    let word = 0x8000 | (0b1011 << 11) | (1 << 8) | (2 << 5) | (3 << 2);
    assert_eq!(decode(word), Some(Instr::Ccnot { rd: 1, rs: 2, rt: 3 }));
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

// ── MemWidth: illegal widths rejected by TryFrom ────────────────────

#[test]
fn mem_width_try_from_rejects_illegal() {
    use theseus_tisc::types::MemWidth;

    // Illegal widths must be Err.
    for bad in [0, 3, 5, 6, 7, 9] {
        assert!(
            MemWidth::try_from(bad).is_err(),
            "MemWidth::try_from({bad}) should be Err"
        );
    }
}

#[test]
fn mem_width_try_from_accepts_legal() {
    use theseus_tisc::types::MemWidth;

    assert_eq!(MemWidth::try_from(1), Ok(MemWidth::W1));
    assert_eq!(MemWidth::try_from(2), Ok(MemWidth::W2));
    assert_eq!(MemWidth::try_from(4), Ok(MemWidth::W4));
    assert_eq!(MemWidth::try_from(8), Ok(MemWidth::W8));
}

#[test]
fn mem_width_words_and_addr_bits() {
    use theseus_tisc::types::MemWidth;

    assert_eq!(MemWidth::W1.words(), 1);
    assert_eq!(MemWidth::W1.addr_bits(), 0);
    assert_eq!(MemWidth::W2.words(), 2);
    assert_eq!(MemWidth::W2.addr_bits(), 1);
    assert_eq!(MemWidth::W4.words(), 4);
    assert_eq!(MemWidth::W4.addr_bits(), 2);
    assert_eq!(MemWidth::W8.words(), 8);
    assert_eq!(MemWidth::W8.addr_bits(), 3);
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

// ── asm::encode: AsmError negative tests ───────────────────────────

#[test]
fn encode_reg_out_of_range() {
    // rd=9 is out of range (NUM_REGS=8, valid: 0..7).
    assert!(matches!(
        encode(Instr::Add { rd: 9, rs: 0 }),
        Err(AsmError::RegOutOfRange { which: "rd", val: 9 })
    ));
    // rs=8 is out of range.
    assert!(matches!(
        encode(Instr::Sub { rd: 0, rs: 8 }),
        Err(AsmError::RegOutOfRange { which: "rs", val: 8 })
    ));
    // I-format: rd out of range.
    assert!(matches!(
        encode(Instr::Addi { rd: 10, imm: 0 }),
        Err(AsmError::RegOutOfRange { which: "rd", val: 10 })
    ));
}

#[test]
fn encode_aliased_operands() {
    // All two-operand R-format instructions reject rd == rs.
    assert!(matches!(
        encode(Instr::Add { rd: 2, rs: 2 }),
        Err(AsmError::AliasedOperands { mnemonic: "ADD", rd: 2, rs: 2 })
    ));
    assert!(matches!(
        encode(Instr::Sub { rd: 5, rs: 5 }),
        Err(AsmError::AliasedOperands { mnemonic: "SUB", .. })
    ));
    assert!(matches!(
        encode(Instr::Xor { rd: 0, rs: 0 }),
        Err(AsmError::AliasedOperands { mnemonic: "XOR", .. })
    ));
    assert!(matches!(
        encode(Instr::Exch { rd: 1, rs: 1 }),
        Err(AsmError::AliasedOperands { mnemonic: "EXCH", .. })
    ));
    assert!(matches!(
        encode(Instr::Rotl { rd: 3, rs: 3 }),
        Err(AsmError::AliasedOperands { mnemonic: "ROTL", .. })
    ));
    assert!(matches!(
        encode(Instr::Rotr { rd: 7, rs: 7 }),
        Err(AsmError::AliasedOperands { mnemonic: "ROTR", .. })
    ));
}

#[test]
fn encode_cswap_not_distinct() {
    // rd == rs
    assert!(matches!(
        encode(Instr::Cswap { rd: 1, rs: 1, rt: 2 }),
        Err(AsmError::CswapNotDistinct { rd: 1, rs: 1, rt: 2 })
    ));
    // rd == rt
    assert!(matches!(
        encode(Instr::Cswap { rd: 3, rs: 0, rt: 3 }),
        Err(AsmError::CswapNotDistinct { rd: 3, rs: 0, rt: 3 })
    ));
    // rs == rt
    assert!(matches!(
        encode(Instr::Cswap { rd: 0, rs: 4, rt: 4 }),
        Err(AsmError::CswapNotDistinct { rd: 0, rs: 4, rt: 4 })
    ));
}

#[test]
fn encode_xchg_scratch_out_of_range() {
    // sx=9 is out of range (NUM_SCRATCH=8).
    assert!(matches!(
        encode(Instr::Xchg { rd: 0, sx: 9 }),
        Err(AsmError::XchgScratchOutOfRange { sx: 9 })
    ));
}

#[test]
fn encode_xchg_scratch_reserved() {
    // s0 and s1 are decoder-reserved.
    assert!(matches!(
        encode(Instr::Xchg { rd: 0, sx: 0 }),
        Err(AsmError::XchgScratchReserved { sx: 0 })
    ));
    assert!(matches!(
        encode(Instr::Xchg { rd: 0, sx: 1 }),
        Err(AsmError::XchgScratchReserved { sx: 1 })
    ));
}

#[test]
fn encode_mexch_addr_out_of_range() {
    // MEM_WORDS=4, so addr=200 is out of range.
    assert!(matches!(
        encode(Instr::Mexch { rd: 0, addr: 200 }),
        Err(AsmError::MexchAddrOutOfRange { addr: 200 })
    ));
}

#[test]
fn encode_swapbr_reg_out_of_range() {
    assert!(matches!(
        encode(Instr::Swapbr { rd: 9 }),
        Err(AsmError::SwapbrRegOutOfRange { rd: 9 })
    ));
}

#[test]
fn assemble_program_too_long() {
    let prog = vec![Instr::Addi { rd: 0, imm: 0 }; 257];
    assert!(matches!(
        assemble(&prog),
        Err(AsmError::ProgramTooLong { len: 257 })
    ));
}

// ── decode_* guard tests: non-Pair panics ──────────────────────────

#[test]
#[should_panic(expected = "expected Pair")]
fn decode_composite_rejects_non_pair() {
    use theseus_tisc::types::decode_composite;
    let non_pair = Value::Unit;
    decode_composite(&non_pair);
}

#[test]
#[should_panic(expected = "expected Pair")]
fn decode_system_rejects_non_pair() {
    use theseus_tisc::types::decode_system;
    let non_pair = Value::Unit;
    decode_system(&non_pair);
}

#[test]
#[should_panic(expected = "expected Pair")]
fn decode_bus_rejects_non_pair() {
    use theseus_tisc::types::decode_bus;
    let non_pair = Value::Unit;
    decode_bus(&non_pair);
}
