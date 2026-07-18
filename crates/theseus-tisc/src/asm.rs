//! TISC-1 assembler: typed instruction mnemonics → 16-bit ROM words
//! (tisc-1-spec §3).
//!
//! [`Instr`] covers the 14 architected instructions; [`encode`] emits
//! the exact bit layout the decoder dispatches on:
//!
//! - **R-format** (bit 15 = 1): `[1 | opcode[14:11] | rd[10:8] |
//!   rs[7:5] | rt[4:2] | pad[1:0] = 0]`
//! - **I-format** (bit 15 = 0): `[0 | opc[14:12] | rd[11:9] |
//!   imm8[8:1] | pad[0] = 0]`
//!
//! Pad bits are always zero — the decoder's predicates (e.g. XCHG's
//! gate keyed on IR bit 0) require it.
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

/// Assembler error: an instruction or program violates a legality rule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AsmError {
    /// A register index is out of range (must be < NUM_REGS).
    RegOutOfRange { which: &'static str, val: u8 },
    /// A two-operand instruction has `rd == rs` (non-injective datapath).
    AliasedOperands { mnemonic: &'static str, rd: u8, rs: u8 },
    /// CSWAP operands are not pairwise distinct.
    CswapNotDistinct { rd: u8, rs: u8, rt: u8 },
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
            AsmError::AliasedOperands { mnemonic, rd, rs } => {
                write!(f, "{mnemonic} r{rd}, r{rs}: aliased operands are illegal (non-injective datapath)")
            }
            AsmError::CswapNotDistinct { rd, rs, rt } => {
                write!(f, "CSWAP r{rd}, r{rs}, r{rt}: operands must be pairwise distinct")
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
/// `rd`, `rs`, `rt` are general-register indices (0..8); `sx` is a
/// scratch-word index (0..8, but 0 and 1 are decoder-reserved and
/// rejected); `imm` is the full 8-bit immediate.
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

fn r_word(opcode: u16, rd: u8, rs: u8, rt: u8) -> Result<u16, AsmError> {
    if (rd as usize) >= NUM_REGS {
        return Err(AsmError::RegOutOfRange { which: "rd", val: rd });
    }
    if (rs as usize) >= NUM_REGS {
        return Err(AsmError::RegOutOfRange { which: "rs", val: rs });
    }
    if (rt as usize) >= NUM_REGS {
        return Err(AsmError::RegOutOfRange { which: "rt", val: rt });
    }
    Ok((1 << 15) | (opcode << 11) | (u16::from(rd) << 8) | (u16::from(rs) << 5) | (u16::from(rt) << 2))
}

fn i_word(opc: u16, rd: u8, imm: u8) -> Result<u16, AsmError> {
    if (rd as usize) >= NUM_REGS {
        return Err(AsmError::RegOutOfRange { which: "rd", val: rd });
    }
    Ok((opc << 12) | (u16::from(rd) << 9) | (u16::from(imm) << 1))
}

fn check_distinct(name: &'static str, rd: u8, rs: u8) -> Result<(), AsmError> {
    if rd == rs {
        return Err(AsmError::AliasedOperands { mnemonic: name, rd, rs });
    }
    Ok(())
}

/// Encode one instruction as its 16-bit ROM word (spec §3).
///
/// Returns `Err(AsmError)` on out-of-range operands, aliased register
/// operands, or XCHG naming the decoder-reserved scratch words s0/s1 —
/// all programs the datapath isos do not implement faithfully.
pub fn encode(instr: Instr) -> Result<u16, AsmError> {
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

/// Decode a ROM word back to its instruction (inverse of [`encode`]).
///
/// Returns `None` for reserved opcodes and for words with nonzero pad
/// bits (which `encode` never emits and the decoder does not accept).
pub fn decode(word: u16) -> Option<Instr> {
    if word & 0x8000 != 0 {
        if word & 0b11 != 0 {
            return None; // R-format pad bits must be zero
        }
        let opcode = (word >> 11) & 0xF;
        let rd = ((word >> 8) & 7) as u8;
        let rs = ((word >> 5) & 7) as u8;
        let rt = ((word >> 2) & 7) as u8;
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
            _ => None, // reserved
        }
    } else {
        if word & 1 != 0 {
            return None; // I-format pad bit must be zero
        }
        let opc = (word >> 12) & 7;
        let rd = ((word >> 9) & 7) as u8;
        let imm = ((word >> 1) & 0xFF) as u8;
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
pub fn assemble(prog: &[Instr]) -> Result<Vec<u16>, AsmError> {
    if prog.len() > 256 {
        return Err(AsmError::ProgramTooLong { len: prog.len() });
    }
    prog.iter().map(|&i| encode(i)).collect()
}
