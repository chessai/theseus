//! A-0 fast validation: 32-bit instruction-word reformat + the
//! scratch-addressable operand crossbar.
//!
//! FAST tier (pure encode/decode round-trips + small crossbar-iso evals;
//! no heavy/ring/energy tests). Covers:
//!   1. `assemble → word → decode → fields` round-trip for EVERY Instr
//!      variant, including scratch operands.
//!   2. `select_operand` crossbar correctness: routes `r[i]` for i<8 and
//!      `s[i-8]` for 8≤i<16 (all 16 indices enumerated).
//!   3. A compiled program's ROM round-trips assemble→decode (exercises
//!      the WORD encoding, which `run_on_cpu_fast` uses).
//!   4. A program naming a scratch operand (`Ccnot rd, rs, s2`) round-trips
//!      and the operand-fetch iso routes s2 to the fixed slot.

use theseus_core::{Iso, Value};
use theseus_netlist::value_from_bits;
use theseus_tisc::asm::{self, assemble, decode, encode, Instr};
use theseus_tisc::decoder::{self, operand_field, select_operand, R_RD};
use theseus_tisc::types::{
    state_ty, StateVals, MEM_WORDS, NUM_CPU_FIELDS, NUM_REGS, NUM_SCRATCH, W,
};

const FUEL: u64 = 50_000_000;

// ══════════════════════════════════════════════════════════════════════
// 1. Round-trip over every Instr variant (incl. scratch operands)
// ══════════════════════════════════════════════════════════════════════

/// Every architected `Instr`, with register-only operands.
fn all_variants() -> Vec<Instr> {
    vec![
        Instr::Add { rd: 1, rs: 2 },
        Instr::Sub { rd: 3, rs: 4 },
        Instr::Xor { rd: 5, rs: 6 },
        Instr::Exch { rd: 0, rs: 7 },
        Instr::Rotl { rd: 2, rs: 3 },
        Instr::Rotr { rd: 4, rs: 5 },
        Instr::Cswap {
            rd: 1,
            rs: 2,
            rt: 3,
        },
        Instr::Ccnot {
            rd: 4,
            rs: 5,
            rt: 6,
        },
        Instr::Xchg { rd: 3, sx: 5 },
        Instr::Swapbr { rd: 2, rs: 0 },
        Instr::Mexch { rd: 1, addr: 3 },
        Instr::MexchReg { rd: 2, ra: 5 },
        Instr::Addi { rd: 0, imm: 0xFF },
        Instr::Subi { rd: 1, imm: 0x01 },
        Instr::Xori { rd: 2, imm: 0xA5 },
        Instr::Rotli { rd: 3, imm: 3 },
        Instr::Rotri { rd: 4, imm: 7 },
    ]
}

#[test]
fn roundtrip_every_variant() {
    for ins in all_variants() {
        let word = encode(ins).unwrap_or_else(|e| panic!("encode {ins:?}: {e}"));
        assert_eq!(decode(word), Some(ins), "round-trip failed for {ins:?}");
    }
}

#[test]
fn roundtrip_scratch_operands() {
    // A-0: operand fields address `reg ∪ scratch` (0..16). A Ccnot naming
    // a scratch operand as rt (s2 = index 8+2 = 10) must encode and round-
    // trip with exact field recovery.
    let ins = Instr::Ccnot {
        rd: 0,
        rs: 1,
        rt: 10,
    };
    let word = encode(ins).expect("encode Ccnot with scratch operand");
    // rt field is bits 10..14; index 10 must be recovered exactly.
    assert_eq!(decode(word), Some(ins), "scratch-operand round-trip failed");
    assert_eq!(
        (word >> 10) & 0x1F,
        10,
        "rt field must hold operand index 10 (s2)"
    );

    // rd naming scratch (s5 = 13), rs naming a register.
    let ins2 = Instr::Add { rd: 13, rs: 0 };
    let word2 = encode(ins2).expect("encode Add with scratch rd");
    assert_eq!(decode(word2), Some(ins2));
    assert_eq!(
        (word2 >> 20) & 0x1F,
        13,
        "rd field must hold operand index 13 (s5)"
    );

    // The legal operand range round-trips through the rs field: registers
    // r0..r7 (0..8) and scratch operands s2..s7 (10..16). s0/s1 (8/9) are
    // decoder-reserved (MAJOR-4) and rejected -- see `rejects_reserved_operands`.
    for i in (0..8u8).chain(10..16u8) {
        // Add rd=..,rs=i needs rd != rs and rd legal; pick a distinct
        // legal register for rd.
        let rd = if i == 0 { 1 } else { 0 };
        let ins = Instr::Add { rd, rs: i };
        let word = encode(ins).unwrap_or_else(|e| panic!("encode Add rs={i}: {e}"));
        assert_eq!(decode(word), Some(ins), "rs={i} round-trip");
    }

    // Index ≥ 16 (reserved) is rejected.
    assert!(
        encode(Instr::Add { rd: 0, rs: 16 }).is_err(),
        "rs=16 must be rejected"
    );
}

#[test]
fn rejects_reserved_operands() {
    use theseus_tisc::asm::AsmError;
    // MAJOR-4: s0 (idx 8) and s1 (idx 9) are decoder-reserved and must be
    // rejected as rd/rs/rt operands (routing the live predicate/operand
    // buffer into the datapath mid-instruction corrupts decode).
    for &(which, ins) in &[
        ("rd", Instr::Add { rd: 8, rs: 0 }),
        ("rs", Instr::Add { rd: 0, rs: 8 }),
        ("rd", Instr::Add { rd: 9, rs: 0 }),
        ("rs", Instr::Add { rd: 0, rs: 9 }),
        (
            "rt",
            Instr::Ccnot {
                rd: 0,
                rs: 1,
                rt: 8,
            },
        ),
        (
            "rt",
            Instr::Ccnot {
                rd: 0,
                rs: 1,
                rt: 9,
            },
        ),
    ] {
        match encode(ins) {
            Err(AsmError::OperandReservedScratch { .. }) => {}
            other => panic!(
                "{ins:?} ({which}=reserved scratch) must be rejected as \
                 OperandReservedScratch, got {other:?}"
            ),
        }
    }

    // Legal scratch operands s2..s7 (idx 10..16) are accepted.
    for i in 10..16u8 {
        assert!(
            encode(Instr::Add { rd: 0, rs: i }).is_ok(),
            "s{} (idx {i}) must be a legal operand",
            i - 8
        );
    }

    // Reserved locations idx ≥ 16 are rejected (RegOutOfRange).
    for &ins in &[
        Instr::Add { rd: 16, rs: 0 },
        Instr::Add { rd: 0, rs: 31 },
        Instr::Ccnot {
            rd: 0,
            rs: 1,
            rt: 20,
        },
    ] {
        match encode(ins) {
            Err(AsmError::RegOutOfRange { .. }) => {}
            other => panic!("{ins:?} (idx ≥ 16) must be RegOutOfRange, got {other:?}"),
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 2. select_operand crossbar correctness (all 16 reg ∪ scratch indices)
// ══════════════════════════════════════════════════════════════════════

/// A StateVals with a DISTINCT value in every reg and scratch word, so
/// the routed value at field 0 uniquely identifies the source location.
///   r[i] = 0x10 + i   (0x10..0x18)
///   s[i] = 0x20 + i   (0x20..0x28)
fn distinct_state(operand_index: u32) -> StateVals {
    let mut regs = [0u8; NUM_REGS];
    for (i, r) in regs.iter_mut().enumerate() {
        *r = 0x10 + i as u8;
    }
    let mut scratch = [0u8; NUM_SCRATCH];
    for (i, s) in scratch.iter_mut().enumerate() {
        *s = 0x20 + i as u8;
    }
    StateVals {
        regs,
        pc: 0,
        br: 0,
        // Place the operand index into the rd field (IR bits 20..24).
        ir: operand_index << 20,
        scratch,
        mem: [0; MEM_WORDS],
    }
}

fn eval_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

#[test]
fn select_operand_routes_reg_and_scratch() {
    // The 1-of-16 operand crossbar keyed on the rd field (IR bits 20..24).
    let iso = select_operand(R_RD, operand_field);

    for idx in 0..16u32 {
        // idx 8/9 = s0/s1 are decoder-reserved (the operand buffer + the
        // predicate word); they are NOT legal operands (asm.rs check_operand
        // rejects them) and the crossbar deliberately does NOT route them
        // (level-3 swap range is 2..8, so positions 0/1 ↔ s0/s1 are untouched
        // -- the A-0 BLOCKER-1 fix). Legal operands: reg 0..7, scratch s2..s7
        // (idx 10..15). Skip the reserved pair.
        if idx == 8 || idx == 9 {
            continue;
        }
        let s = distinct_state(idx);
        let got = eval_state("select_operand", &iso, &s);

        // The selected location's value must land at the fixed slot r0.
        let expected_val = if idx < 8 {
            0x10 + idx as u8 // r[idx]
        } else {
            0x20 + (idx - 8) as u8 // s[idx-8] (s2..s7 for idx 10..15)
        };
        assert_eq!(
            got.regs[0],
            expected_val,
            "operand index {idx}: r0 slot must hold {}'s value {expected_val:#04x}, got {:#04x}",
            if idx < 8 {
                format!("r{idx}")
            } else {
                format!("s{}", idx - 8)
            },
            got.regs[0],
        );

        // Non-routed fields (PC/BR/IR/mem) are preserved.
        assert_eq!(got.ir, s.ir, "operand index {idx}: IR preserved");
        assert_eq!(got.br, s.br, "operand index {idx}: BR preserved");
    }
}

#[test]
fn select_operand_roundtrip_identity() {
    // select ; unselect == identity for every operand index.
    let iso = select_operand(R_RD, operand_field);
    let rt = Iso::seq(iso.clone(), Iso::sym(iso));
    for idx in 0..16u32 {
        let s = distinct_state(idx);
        assert_eq!(
            eval_state("select_operand roundtrip", &rt, &s),
            s,
            "select ; unselect must be identity at operand index {idx}"
        );
    }
}

#[test]
fn select_operand_low8_matches_select_reg() {
    // For indices 0..8 (register operands), select_operand must agree
    // bit-for-bit with the legacy 3-level select_reg (backward-compat).
    let op_iso = select_operand(R_RD, operand_field);
    let reg_iso = decoder::select_reg(R_RD);
    for idx in 0..8u32 {
        let s = distinct_state(idx);
        let via_operand = eval_state("select_operand", &op_iso, &s);
        let via_reg = eval_state("select_reg", &reg_iso, &s);
        assert_eq!(
            via_operand.regs, via_reg.regs,
            "register operand {idx}: select_operand must match select_reg"
        );
    }
}

// ══════════════════════════════════════════════════════════════════════
// 3. A compiled program's ROM round-trips assemble→decode
// ══════════════════════════════════════════════════════════════════════

#[test]
fn compiled_rom_assemble_decode_roundtrip() {
    // Hand-build a small straight-line program and exercise the WORD
    // encoding end-to-end (this is the path run_on_cpu_fast walks).
    let prog = vec![
        Instr::Addi { rd: 0, imm: 5 },
        Instr::Add { rd: 1, rs: 2 },
        Instr::Exch { rd: 1, rs: 2 },
        Instr::Xori { rd: 3, imm: 0xAA },
        Instr::Rotli { rd: 4, imm: 3 },
        Instr::Ccnot {
            rd: 5,
            rs: 6,
            rt: 7,
        },
    ];
    let rom = assemble(&prog).expect("assemble program");
    assert_eq!(rom.len(), prog.len());
    for (p, &ins) in prog.iter().enumerate() {
        assert_eq!(rom[p], encode(ins).unwrap(), "rom[{p}] != encode({ins:?})");
        assert_eq!(
            asm::decode(rom[p]),
            Some(ins),
            "rom[{p}] decode must recover {ins:?}"
        );
    }
}

// ══════════════════════════════════════════════════════════════════════
// 4. A program naming a scratch operand + operand-fetch iso routes s2
// ══════════════════════════════════════════════════════════════════════

#[test]
fn scratch_operand_program_and_fetch() {
    // `Ccnot rd, rs, s2`: rt names scratch s2 (operand index 10).
    let ins = Instr::Ccnot {
        rd: 0,
        rs: 1,
        rt: 10,
    };
    let word = encode(ins).expect("encode Ccnot rd,rs,s2");
    assert_eq!(decode(word), Some(ins), "Ccnot rd,rs,s2 round-trip");

    // The operand-fetch iso must route s2 to the fixed slot r0.
    // Put the operand index (10 = s2) in the rd field so R_RD selects it.
    let iso = select_operand(R_RD, operand_field);
    let s = distinct_state(10);
    let got = eval_state("fetch-s2", &iso, &s);
    assert_eq!(
        got.regs[0],
        0x22, // s[2] = 0x20 + 2
        "operand-fetch must route s2's value (0x22) to the r0 slot"
    );
    // And the routing is invertible (frame preserved on round-trip).
    let rt = Iso::seq(iso.clone(), Iso::sym(iso));
    assert_eq!(eval_state("fetch-s2 rt", &rt, &s), s);
}

// ══════════════════════════════════════════════════════════════════════
// 5. IR-width state layout: 32-bit IR encode/decode round-trip
// ══════════════════════════════════════════════════════════════════════

#[test]
fn ir32_state_encode_decode_roundtrip() {
    // The IR field is now 32-bit (4 bytes). A full StateVals must survive
    // encode → Value → decode with the wide IR intact.
    let s = StateVals {
        regs: [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77],
        pc: 0x42,
        br: 0x99,
        ir: 0xDEAD_BEEF,
        scratch: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
        mem: [0; MEM_WORDS],
    };
    let decoded = StateVals::decode(&s.encode());
    assert_eq!(decoded.ir, s.ir, "32-bit IR must round-trip exactly");
    assert_eq!(decoded, s, "full StateVals must round-trip");

    // Sanity: state_ty has the widened field count and the encoding fills it.
    let bits = theseus_netlist::bits_of(&s.encode(), &state_ty()).unwrap();
    assert_eq!(
        bits.len(),
        NUM_CPU_FIELDS * W,
        "state bit-width must be 22*8"
    );

    // A raw all-ones IR round-trips (exercises every IR byte).
    let mut s2 = s.clone();
    s2.ir = 0xFFFF_FFFF;
    assert_eq!(StateVals::decode(&s2.encode()).ir, 0xFFFF_FFFF);

    // Guard against an unused-import warning while asserting the encoder
    // produces a well-typed Value.
    let _v: Value = value_from_bits(&state_ty(), &bits).unwrap();
}
