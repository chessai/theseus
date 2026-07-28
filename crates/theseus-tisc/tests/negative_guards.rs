//! Negative / failure-path tests for stable guards in theseus-tisc.
//!
//! These cover rejection paths that must remain correct even as the
//! crate evolves — reserved opcodes, unrecognized prims, and
//! addressing precondition panics.

use std::collections::BTreeMap;
use theseus_core::prim::PrimDef;
use theseus_core::{Iso, Type, Value};
use theseus_tisc::asm::{assemble, decode, encode, AsmError, Instr};
use theseus_tisc::compile::{compile, CompileError};

// ── asm::decode: reserved opcodes → None ────────────────────────────

// A-0 32-bit word layout:
//   R: fmt(31)=1 | opcode[30:25] | rd[24:20] | rs[19:15] | rt[14:10] | spare[9:0]
//   I: fmt(31)=0 | opcode[30:25] | rd[24:20] | imm[19:0]

#[test]
fn decode_reserved_r_format_opcode() {
    // R-format, reserved opcode 0b1111 (=15): fmt=1, op=15 = 0x9E00_0000.
    assert_eq!(decode(0x9E00_0000), None);
}

#[test]
fn decode_mexchr_opcode_1010() {
    // R-format opcode 0b1010 (=10) is MEXCHR: fmt=1, op=10, rd=rs=rt=0 = 0x9400_0000.
    assert_eq!(decode(0x9400_0000), Some(Instr::MexchReg { rd: 0, ra: 0 }));
}

#[test]
fn decode_r_format_opcode_1011_is_ccnot() {
    // R-format opcode 0b1011 (=11) is CCNOT: fmt=1, op=11, rd=rs=rt=0 = 0x9600_0000.
    assert_eq!(
        decode(0x9600_0000),
        Some(Instr::Ccnot {
            rd: 0,
            rs: 0,
            rt: 0
        })
    );
    // With distinct operands: rd=1, rs=2, rt=3.
    let word = (1u32 << 31) | (0b1011 << 25) | (1 << 20) | (2 << 15) | (3 << 10);
    assert_eq!(
        decode(word),
        Some(Instr::Ccnot {
            rd: 1,
            rs: 2,
            rt: 3
        })
    );
}

#[test]
fn decode_reserved_i_format_opcode_101() {
    // I-format reserved opcode 0b101 (=5): fmt=0, op=5 = 0x0A00_0000.
    assert_eq!(decode(0x0A00_0000), None);
}

#[test]
fn decode_reserved_i_format_opcode_110() {
    // I-format reserved opcode 0b110 (=6): fmt=0, op=6 = 0x0C00_0000.
    assert_eq!(decode(0x0C00_0000), None);
}

#[test]
fn decode_reserved_i_format_opcode_111() {
    // I-format reserved opcode 0b111 (=7): fmt=0, op=7 = 0x0E00_0000.
    assert_eq!(decode(0x0E00_0000), None);
}

#[test]
fn decode_r_format_nonzero_spare_bits() {
    // R-format word (opcode 0b0000 = Add) with spare bits [9:0] set.
    // fmt=1, op=0, rd=rs=rt=0, spare = 0b11 → decode returns None.
    assert_eq!(decode(0x8000_0003), None);
}

#[test]
fn decode_r_format_single_spare_bit() {
    // R-format Add with only spare bit 0 set → None.
    assert_eq!(decode(0x8000_0001), None);
}

#[test]
fn decode_i_format_bit0_is_imm_lsb() {
    // A-0: the old I-format pad bit is gone; bit 0 is the immediate LSB.
    // 0x0000_0001: fmt=0, op=0 (Addi), rd=0, imm=1 → Addi{rd:0, imm:1}.
    assert_eq!(decode(0x0000_0001), Some(Instr::Addi { rd: 0, imm: 1 }));
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
        symbolic: None,
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
fn encode_operand_out_of_range() {
    // A-0: operand fields address `reg ∪ scratch` = 16 locations (0..16);
    // indices 8..16 name scratch words. Only `≥ 16` (the reserved
    // locations) are rejected.
    // rd=16 is out of range.
    assert!(matches!(
        encode(Instr::Add { rd: 16, rs: 0 }),
        Err(AsmError::RegOutOfRange {
            which: "rd",
            val: 16
        })
    ));
    // rs=20 is out of range.
    assert!(matches!(
        encode(Instr::Sub { rd: 0, rs: 20 }),
        Err(AsmError::RegOutOfRange {
            which: "rs",
            val: 20
        })
    ));
    // I-format: rd out of range.
    assert!(matches!(
        encode(Instr::Addi { rd: 31, imm: 0 }),
        Err(AsmError::RegOutOfRange {
            which: "rd",
            val: 31
        })
    ));
    // s0 (idx 8) and s1 (idx 9) are decoder-reserved (operand buffer +
    // predicate) → rejected as OperandReservedScratch, NOT accepted.
    assert!(matches!(
        encode(Instr::Add { rd: 0, rs: 8 }),
        Err(AsmError::OperandReservedScratch {
            which: "rs",
            val: 8
        })
    ));
    assert!(matches!(
        encode(Instr::Add { rd: 9, rs: 0 }),
        Err(AsmError::OperandReservedScratch {
            which: "rd",
            val: 9
        })
    ));
    // Legal scratch operands are s2..s7 (idx 10..16) — scratch-addressable, A-0.
    assert!(
        encode(Instr::Add { rd: 10, rs: 0 }).is_ok(),
        "rd=10 (s2) is a valid operand"
    );
    assert!(
        encode(Instr::Add { rd: 0, rs: 15 }).is_ok(),
        "rs=15 (s7) is a valid operand"
    );
}

#[test]
fn encode_aliased_operands() {
    // All two-operand R-format instructions reject rd == rs.
    assert!(matches!(
        encode(Instr::Add { rd: 2, rs: 2 }),
        Err(AsmError::AliasedOperands {
            mnemonic: "ADD",
            rd: 2,
            rs: 2
        })
    ));
    assert!(matches!(
        encode(Instr::Sub { rd: 5, rs: 5 }),
        Err(AsmError::AliasedOperands {
            mnemonic: "SUB",
            ..
        })
    ));
    assert!(matches!(
        encode(Instr::Xor { rd: 0, rs: 0 }),
        Err(AsmError::AliasedOperands {
            mnemonic: "XOR",
            ..
        })
    ));
    assert!(matches!(
        encode(Instr::Exch { rd: 1, rs: 1 }),
        Err(AsmError::AliasedOperands {
            mnemonic: "EXCH",
            ..
        })
    ));
    assert!(matches!(
        encode(Instr::Rotl { rd: 3, rs: 3 }),
        Err(AsmError::AliasedOperands {
            mnemonic: "ROTL",
            ..
        })
    ));
    assert!(matches!(
        encode(Instr::Rotr { rd: 7, rs: 7 }),
        Err(AsmError::AliasedOperands {
            mnemonic: "ROTR",
            ..
        })
    ));
}

#[test]
fn encode_cswap_not_distinct() {
    // rd == rs
    assert!(matches!(
        encode(Instr::Cswap {
            rd: 1,
            rs: 1,
            rt: 2
        }),
        Err(AsmError::CswapNotDistinct {
            rd: 1,
            rs: 1,
            rt: 2
        })
    ));
    // rd == rt
    assert!(matches!(
        encode(Instr::Cswap {
            rd: 3,
            rs: 0,
            rt: 3
        }),
        Err(AsmError::CswapNotDistinct {
            rd: 3,
            rs: 0,
            rt: 3
        })
    ));
    // rs == rt
    assert!(matches!(
        encode(Instr::Cswap {
            rd: 0,
            rs: 4,
            rt: 4
        }),
        Err(AsmError::CswapNotDistinct {
            rd: 0,
            rs: 4,
            rt: 4
        })
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
fn encode_swapbr_operand_legality() {
    // A-5b Piece 1: SWAPBR's rd now goes through the SAME `check_operand`
    // legality as every other reg∪scratch op (the SWAPBR-specific
    // `SwapbrRegOutOfRange` guard was removed). So the rejection variants
    // change, and a legal scratch rd now ENCODES.

    // rd = 9 (= s1, decoder-reserved) is still rejected — but now as the
    // uniform `OperandReservedScratch`, not the old `SwapbrRegOutOfRange`.
    assert!(matches!(
        encode(Instr::Swapbr { rd: 9 }),
        Err(AsmError::OperandReservedScratch { which: "rd", val: 9 })
    ));
    // rd = 8 (= s0, the operand buffer) is likewise reserved.
    assert!(matches!(
        encode(Instr::Swapbr { rd: 8 }),
        Err(AsmError::OperandReservedScratch { which: "rd", val: 8 })
    ));
    // rd = 16 (out of the 0..16 operand space) still rejects.
    assert!(matches!(
        encode(Instr::Swapbr { rd: 16 }),
        Err(AsmError::RegOutOfRange { which: "rd", val: 16 })
    ));

    // POSITIVE: rd = 10 (= s2, a legal scratch operand) now ENCODES, and
    // round-trips through decode exactly like a register rd.
    let word = encode(Instr::Swapbr { rd: 10 }).expect("SWAPBR s2 must now encode");
    assert_eq!(
        decode(word),
        Some(Instr::Swapbr { rd: 10 }),
        "scratch-rd SWAPBR must round-trip encode∘decode"
    );
    // r0..r7 unaffected — a register rd still encodes.
    assert!(encode(Instr::Swapbr { rd: 0 }).is_ok());
    assert!(encode(Instr::Swapbr { rd: 7 }).is_ok());
    // Top legal scratch operand s7 (= idx 15) encodes.
    assert!(encode(Instr::Swapbr { rd: 15 }).is_ok());
}

#[test]
fn assemble_program_too_long() {
    // A-6.4 widen-PC: the assembler cap is the PC address space (2^W_PC =
    // 65536), not the old 8-bit 256. A 257-word program (which the 8-bit PC
    // rejected) now assembles fine — the widen-PC payoff at the assembler.
    use theseus_tisc::types::PC_ADDR_SPACE;
    assert!(
        assemble(&vec![Instr::Addi { rd: 0, imm: 0 }; 257]).is_ok(),
        "257-word program must assemble under the 16-bit PC (was rejected at 8-bit)"
    );
    // The cap is enforced one past the PC address space.
    let too_long = PC_ADDR_SPACE + 1;
    let prog = vec![Instr::Addi { rd: 0, imm: 0 }; too_long];
    assert!(matches!(
        assemble(&prog),
        Err(AsmError::ProgramTooLong { len }) if len == too_long
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
