//! TISC-1 assembler: typed instruction mnemonics → 32-bit ROM words
//! (tisc-1-spec §3; A-0 instruction-format reformat).
//!
//! [`Instr`] covers the 17 architected instructions; [`encode`] emits
//! the exact 32-bit bit layout the decoder dispatches on (A-0):
//!
//! - **R-format** (bit 31 = 1):
//!   `[1 | opcode[30:25] | rd[24:20] | rs[19:15] | rt[14:10] | spare[9:0] = 0]`
//! - **I-format** (bit 31 = 0):
//!   `[0 | opcode[30:25] | rd[24:20] | imm20[19:0]]`
//!
//! Operand fields (rd/rs/rt) are **5 bits** = 32 locations; index
//! `0..8 → r0..r7`, `8..16 → s0..s7`, `16..32 → reserved` (the
//! scratch-addressable operand crossbar, A-0). The opcode is 6 bits;
//! the immediate is 20 bits.
//!
//! Spare bits are always zero.
//!
//! `encode` also enforces the datapath's legality conventions (the
//! P1-checker rules the iso semantics assume), rejecting programs the
//! hardware would mangle rather than assembling them:
//!
//! - **No aliased operands.** `ADD/SUB/XOR/EXCH/ROTL/ROTR rd, rs` with
//!   `rd == rs` is non-injective (or deviates: the swap-fetch datapath
//!   preserves the target and trashes s0), so it is a hard error.
//!   CSWAP requires rd, rs, rt pairwise distinct.
//! - **XCHG may not name s0 or s1** — the decoder reserves s0 as the
//!   operand buffer and s1 as the predicate/carry scratch.
//!
//! [`decode`] is the inverse (for round-trip checks and listings);
//! reserved opcodes and set pad bits decode to `None`.

use std::fmt;

use crate::types::{MEM_WORDS, NUM_REGS, NUM_SCRATCH};

// ── A-0 32-bit instruction-word layout ─────────────────────────────
//
// Shared:  fmt = bit 31 (1 = R, 0 = I); opcode = bits 30..25 (6 bits).
// R:       rd = bits 24..20, rs = bits 19..15, rt = bits 14..10 (5 bits
//          each); spare = bits 9..0 (always 0).
// I:       rd = bits 24..20 (5 bits); imm = bits 19..0 (20 bits).

/// LSB position of the format bit (1 = R-format, 0 = I-format).
const FMT_SHIFT: u32 = 31;
/// LSB position of the 6-bit opcode field.
const OPCODE_SHIFT: u32 = 25;
/// LSB position of the 5-bit rd operand field.
const RD_SHIFT: u32 = 20;
/// LSB position of the 5-bit rs operand field (R-format).
const RS_SHIFT: u32 = 15;
/// LSB position of the 5-bit rt operand field (R-format).
const RT_SHIFT: u32 = 10;
/// I-format immediate occupies bits 19..0 (20 bits).
const IMM_BITS: u32 = 20;

/// Number of operand-addressable locations: `reg ∪ scratch` = 16
/// (`0..8 → r0..r7`, `8..16 → s0..s7`; `16..32` reserved).
const NUM_OPERAND_LOCS: usize = NUM_REGS + NUM_SCRATCH;

/// Assembler error: an instruction or program violates a legality rule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AsmError {
    /// An operand index is out of range (must be < 16 = `reg ∪ scratch`;
    /// A-0). `which` is "rd"/"rs"/"rd".
    RegOutOfRange { which: &'static str, val: u8 },
    /// An operand names decoder-reserved scratch s0 (idx 8) or s1 (idx 9)
    /// as an rd/rs/rt operand. s0 is the operand/immediate buffer and s1
    /// the predicate/carry scratch — routing them into the datapath
    /// mid-instruction corrupts decode. Legal scratch operands are
    /// s2..s7 (idx 10..15). `which` is "rd"/"rs"/"rt".
    OperandReservedScratch { which: &'static str, val: u8 },
    /// A two-operand instruction has `rd == rs` (non-injective datapath).
    AliasedOperands { mnemonic: &'static str, rd: u8, rs: u8 },
    /// CSWAP operands are not pairwise distinct.
    CswapNotDistinct { rd: u8, rs: u8, rt: u8 },
    /// CCNOT operands are not pairwise distinct.
    CcnotNotDistinct { rd: u8, rs: u8, rt: u8 },
    /// XCHG scratch index out of range (must be < NUM_SCRATCH).
    XchgScratchOutOfRange { sx: u8 },
    /// XCHG names decoder-reserved scratch s0 or s1.
    XchgScratchReserved { sx: u8 },
    /// MEXCH memory address out of range (must be < MEM_WORDS).
    MexchAddrOutOfRange { addr: u8 },
    /// MEXCHR address register aliases data register (ra == rd),
    /// which breaks bus-quiescence (the mem phase mutates reg[rd],
    /// so absorb's re-read of reg[ra=rd] gets a different value).
    MexchAddrAliasesData { rd: u8, ra: u8 },
    /// SWAPBR register index out of range (must be < NUM_REGS).
    SwapbrRegOutOfRange { rd: u8 },
    /// Program exceeds the 8-bit PC address space (> 256 instructions).
    ProgramTooLong { len: usize },
}

impl fmt::Display for AsmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmError::RegOutOfRange { which, val } => {
                write!(f, "register index out of range: {which}={val}")
            }
            AsmError::OperandReservedScratch { which, val } => {
                write!(
                    f,
                    "operand {which}={val} names decoder-reserved scratch s{} \
                     (s0/s1 are the operand/predicate buffers; legal scratch operands are s2..s7)",
                    val - NUM_REGS as u8
                )
            }
            AsmError::AliasedOperands { mnemonic, rd, rs } => {
                write!(f, "{mnemonic} r{rd}, r{rs}: aliased operands are illegal (non-injective datapath)")
            }
            AsmError::CswapNotDistinct { rd, rs, rt } => {
                write!(f, "CSWAP r{rd}, r{rs}, r{rt}: operands must be pairwise distinct")
            }
            AsmError::CcnotNotDistinct { rd, rs, rt } => {
                write!(f, "CCNOT r{rd}, r{rs}, r{rt}: operands must be pairwise distinct")
            }
            AsmError::XchgScratchOutOfRange { sx } => {
                write!(f, "XCHG scratch index out of range: s{sx}")
            }
            AsmError::XchgScratchReserved { sx } => {
                write!(f, "XCHG s{sx}: s0/s1 are decoder-reserved (operand/predicate scratch)")
            }
            AsmError::MexchAddrOutOfRange { addr } => {
                write!(f, "MEXCH memory address out of range: m{addr} (MEM_WORDS={MEM_WORDS})")
            }
            AsmError::MexchAddrAliasesData { rd, ra } => {
                write!(
                    f,
                    "MEXCHR r{rd}, r{ra}: address register aliases data register \
                     (ra == rd breaks bus-quiescence)"
                )
            }
            AsmError::SwapbrRegOutOfRange { rd } => {
                write!(f, "SWAPBR r{rd}: register index out of range")
            }
            AsmError::ProgramTooLong { len } => {
                write!(f, "program exceeds the 8-bit PC address space: {len} > 256")
            }
        }
    }
}

impl std::error::Error for AsmError {}

/// One TISC-1 instruction with typed operands.
///
/// `rd`, `rs`, `rt` are operand indices over `reg ∪ scratch` (A-0):
/// `0..8 → r0..r7`, `8..16 → s0..s7` (the scratch-addressable operand
/// crossbar); `16..32` are reserved and rejected by [`encode`]. The
/// architected programs still name registers (0..8); a scratch operand
/// (8..16) is legal to encode. `sx` is a scratch-word index (0..8, but
/// 0 and 1 are decoder-reserved and rejected); `imm` is the 8-bit
/// immediate (zero-extended into the 20-bit I-format immediate field).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instr {
    /// `r[rd] += r[rs]` (mod 256). Requires `rd != rs`.
    Add { rd: u8, rs: u8 },
    /// `r[rd] -= r[rs]` (mod 256). Requires `rd != rs`.
    Sub { rd: u8, rs: u8 },
    /// `r[rd] ^= r[rs]`. Requires `rd != rs`.
    Xor { rd: u8, rs: u8 },
    /// `r[rd] ↔ r[rs]`. Requires `rd != rs`.
    Exch { rd: u8, rs: u8 },
    /// `r[rd] = rotl(r[rd], r[rs] & 7)`. Requires `rd != rs`.
    Rotl { rd: u8, rs: u8 },
    /// `r[rd] = rotr(r[rd], r[rs] & 7)`. Requires `rd != rs`.
    Rotr { rd: u8, rs: u8 },
    /// If `r[rd] & 1 == 1` then `r[rs] ↔ r[rt]`. Pairwise distinct.
    Cswap { rd: u8, rs: u8, rt: u8 },
    /// `r[rd] ^= r[rs] & r[rt]` (byte-parallel). Pairwise distinct.
    Ccnot { rd: u8, rs: u8, rt: u8 },
    /// `r[rd] ↔ s[sx]`. `sx` must be 2..8 (s0/s1 decoder-reserved).
    Xchg { rd: u8, sx: u8 },
    /// `BR ↔ r[rd]` (ADR-0034 D1: Pendulum semantics). The branch
    /// register is a PC offset consumed by the uniform end-of-step
    /// update `PC += BR + 1`.
    Swapbr { rd: u8 },
    /// `r[rd] ↔ mem[addr]` (M-C2 Layer 1, ADR-0046 §3.1). Self-inverse
    /// memory exchange; `addr` indexes the memory bank (0..MEM_WORDS).
    Mexch { rd: u8, addr: u8 },
    /// `r[rd] ↔ mem[r[ra] % MEM_WORDS]` (M-D.b). Self-inverse
    /// data-dependent memory exchange; address comes from register `ra`
    /// at runtime. Requires `ra != rd` (bus-quiescence invariant).
    MexchReg { rd: u8, ra: u8 },
    /// `r[rd] += imm` (mod 256).
    Addi { rd: u8, imm: u8 },
    /// `r[rd] -= imm` (mod 256).
    Subi { rd: u8, imm: u8 },
    /// `r[rd] ^= imm`.
    Xori { rd: u8, imm: u8 },
    /// `r[rd] = rotl(r[rd], imm & 7)`.
    Rotli { rd: u8, imm: u8 },
    /// `r[rd] = rotr(r[rd], imm & 7)`.
    Rotri { rd: u8, imm: u8 },
}

/// Validate one 5-bit operand index against the *legal* operand space
/// (A-0, MAJOR-4). Legal operands are:
///   - `0..8`   → r0..r7 (general registers), and
///   - `10..16` → s2..s7 (scratch operands).
/// Rejected:
///   - `8`/`9`  → s0/s1, the decoder-reserved operand/predicate buffers
///     (routing them into the datapath mid-instruction corrupts decode);
///   - `≥ 16`   → reserved locations no crossbar routes.
fn check_operand(which: &'static str, val: u8) -> Result<(), AsmError> {
    if (val as usize) >= NUM_OPERAND_LOCS {
        return Err(AsmError::RegOutOfRange { which, val });
    }
    // idx 8 = s0, idx 9 = s1 are decoder-reserved.
    if val == NUM_REGS as u8 || val == NUM_REGS as u8 + 1 {
        return Err(AsmError::OperandReservedScratch { which, val });
    }
    Ok(())
}

/// Build a 32-bit R-format word (A-0). Operand fields are 5-bit indices
/// over `reg ∪ scratch` (0..16); the 6-bit opcode occupies bits 30..25.
fn r_word(opcode: u32, rd: u8, rs: u8, rt: u8) -> Result<u32, AsmError> {
    check_operand("rd", rd)?;
    check_operand("rs", rs)?;
    check_operand("rt", rt)?;
    Ok((1 << FMT_SHIFT)
        | (opcode << OPCODE_SHIFT)
        | (u32::from(rd) << RD_SHIFT)
        | (u32::from(rs) << RS_SHIFT)
        | (u32::from(rt) << RT_SHIFT))
}

/// Build a 32-bit I-format word (A-0). `rd` is a 5-bit `reg ∪ scratch`
/// index; `imm` occupies the low 20 bits (only the low 8 are used by
/// the current I-ops — the high 12 are zero-extended headroom for A-2).
fn i_word(opc: u32, rd: u8, imm: u8) -> Result<u32, AsmError> {
    check_operand("rd", rd)?;
    // imm is currently 8-bit; zero-extend into the 20-bit I-field.
    let imm20 = u32::from(imm) & ((1u32 << IMM_BITS) - 1);
    Ok((opc << OPCODE_SHIFT) | (u32::from(rd) << RD_SHIFT) | imm20)
}

fn check_distinct(name: &'static str, rd: u8, rs: u8) -> Result<(), AsmError> {
    if rd == rs {
        return Err(AsmError::AliasedOperands { mnemonic: name, rd, rs });
    }
    Ok(())
}

/// Encode one instruction as its 32-bit ROM word (A-0).
///
/// Returns `Err(AsmError)` on out-of-range operands, aliased register
/// operands, or XCHG naming the decoder-reserved scratch words s0/s1 —
/// all programs the datapath isos do not implement faithfully.
pub fn encode(instr: Instr) -> Result<u32, AsmError> {
    match instr {
        Instr::Add { rd, rs } => {
            check_distinct("ADD", rd, rs)?;
            r_word(0b0000, rd, rs, 0)
        }
        Instr::Sub { rd, rs } => {
            check_distinct("SUB", rd, rs)?;
            r_word(0b0001, rd, rs, 0)
        }
        Instr::Xor { rd, rs } => {
            check_distinct("XOR", rd, rs)?;
            r_word(0b0010, rd, rs, 0)
        }
        Instr::Exch { rd, rs } => {
            check_distinct("EXCH", rd, rs)?;
            r_word(0b0011, rd, rs, 0)
        }
        Instr::Rotl { rd, rs } => {
            check_distinct("ROTL", rd, rs)?;
            r_word(0b0100, rd, rs, 0)
        }
        Instr::Rotr { rd, rs } => {
            check_distinct("ROTR", rd, rs)?;
            r_word(0b0101, rd, rs, 0)
        }
        Instr::Cswap { rd, rs, rt } => {
            if rd == rs || rd == rt || rs == rt {
                return Err(AsmError::CswapNotDistinct { rd, rs, rt });
            }
            r_word(0b0110, rd, rs, rt)
        }
        Instr::Ccnot { rd, rs, rt } => {
            if rd == rs || rd == rt || rs == rt {
                return Err(AsmError::CcnotNotDistinct { rd, rs, rt });
            }
            r_word(0b1011, rd, rs, rt)
        }
        Instr::Xchg { rd, sx } => {
            if (sx as usize) >= NUM_SCRATCH {
                return Err(AsmError::XchgScratchOutOfRange { sx });
            }
            if sx < 2 {
                return Err(AsmError::XchgScratchReserved { sx });
            }
            r_word(0b0111, rd, 0, sx)
        }
        Instr::Swapbr { rd } => {
            if (rd as usize) >= NUM_REGS {
                return Err(AsmError::SwapbrRegOutOfRange { rd });
            }
            r_word(0b1000, rd, 0, 0)
        }
        Instr::Mexch { rd, addr } => {
            if (addr as usize) >= MEM_WORDS {
                return Err(AsmError::MexchAddrOutOfRange { addr });
            }
            r_word(0b1001, rd, 0, addr)
        }
        Instr::MexchReg { rd, ra } => {
            if rd == ra {
                return Err(AsmError::MexchAddrAliasesData { rd, ra });
            }
            r_word(0b1010, rd, ra, 0)
        }
        Instr::Addi { rd, imm } => i_word(0b000, rd, imm),
        Instr::Subi { rd, imm } => i_word(0b001, rd, imm),
        Instr::Xori { rd, imm } => i_word(0b010, rd, imm),
        Instr::Rotli { rd, imm } => i_word(0b011, rd, imm),
        Instr::Rotri { rd, imm } => i_word(0b100, rd, imm),
    }
}

/// Decode a 32-bit ROM word back to its instruction (inverse of
/// [`encode`], A-0).
///
/// Returns `None` for reserved opcodes and for R-format words with
/// nonzero spare bits (which `encode` never emits).
///
/// MAJOR-3 (decode/datapath agreement): `decode_execute`'s predicates
/// test only the **low 4** opcode bits (R) / **low 3** (I) — widening
/// them to the full 6-bit opcode is DEFERRED (it needs the s1-convention
/// rework: `pred_compute` caps at 5 controls / 3 chain-scratch bits, and
/// s1 bit 4 already holds CSWAP's control copy, so 6 opcode bits + fmt =
/// 7 controls do not fit). Until then the datapath dispatches on the low
/// opcode bits, so a word whose HIGH opcode bits are set aliases a live
/// op (e.g. R opcode 0b011001 = 25 executes as MEXCH 0b1001). `encode`
/// never emits such a word (all architected opcodes fit the low bits, and
/// `assemble` is the sole ROM source), so no emitted ROM can contain one.
/// To keep `decode` (hence the fast sim) from silently DISAGREEING with
/// the datapath on hand-crafted words, `decode` masks the opcode to the
/// same low bits the datapath tests — so `decode` returns exactly the op
/// the hardware would execute, never a mismatched `None`.
pub fn decode(word: u32) -> Option<Instr> {
    let rd = ((word >> RD_SHIFT) & 0x1F) as u8; // 5-bit operand
    if word & (1 << FMT_SHIFT) != 0 {
        // R-format: spare bits 9..0 must be zero.
        if word & ((1u32 << RT_SHIFT) - 1) != 0 {
            return None;
        }
        // Datapath tests only the low 4 opcode bits (MAJOR-3, deferred).
        let opcode = (word >> OPCODE_SHIFT) & 0xF;
        let rs = ((word >> RS_SHIFT) & 0x1F) as u8;
        let rt = ((word >> RT_SHIFT) & 0x1F) as u8;
        match opcode {
            0b0000 => Some(Instr::Add { rd, rs }),
            0b0001 => Some(Instr::Sub { rd, rs }),
            0b0010 => Some(Instr::Xor { rd, rs }),
            0b0011 => Some(Instr::Exch { rd, rs }),
            0b0100 => Some(Instr::Rotl { rd, rs }),
            0b0101 => Some(Instr::Rotr { rd, rs }),
            0b0110 => Some(Instr::Cswap { rd, rs, rt }),
            0b0111 => Some(Instr::Xchg { rd, sx: rt }),
            0b1000 => Some(Instr::Swapbr { rd }),
            0b1001 => Some(Instr::Mexch { rd, addr: rt }),
            0b1010 => Some(Instr::MexchReg { rd, ra: rs }),
            0b1011 => Some(Instr::Ccnot { rd, rs, rt }),
            _ => None, // reserved
        }
    } else {
        // Datapath tests only the low 3 opcode bits (MAJOR-3, deferred).
        let opc = (word >> OPCODE_SHIFT) & 0x7;
        let imm = (word & 0xFF) as u8; // low 8 bits of the 20-bit imm
        match opc {
            0b000 => Some(Instr::Addi { rd, imm }),
            0b001 => Some(Instr::Subi { rd, imm }),
            0b010 => Some(Instr::Xori { rd, imm }),
            0b011 => Some(Instr::Rotli { rd, imm }),
            0b100 => Some(Instr::Rotri { rd, imm }),
            _ => None, // reserved
        }
    }
}

/// Assemble a program into its ROM image: `rom[p] = encode(prog[p])`.
///
/// The ROM must fit the 8-bit PC address space (≤ 256 words).
///
/// Returns `Err(AsmError)` if the program exceeds 256 instructions or
/// any instruction fails [`encode`]'s legality checks.
pub fn assemble(prog: &[Instr]) -> Result<Vec<u32>, AsmError> {
    if prog.len() > 256 {
        return Err(AsmError::ProgramTooLong { len: prog.len() });
    }
    prog.iter().map(|&i| encode(i)).collect()
}
