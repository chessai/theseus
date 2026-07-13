//! TISC-1 State type and Word helpers.
//!
//! State layout (160 bits, fixed field order):
//!   [r0, r1, r2, r3, r4, r5, r6, r7, PC, BR, IR_lo, IR_hi, s0, s1, s2, s3, s4, s5, s6, s7]
//!
//! Each general register, PC, BR, and scratch word is a Word8 (8 bits,
//! right-nested product, index 0 = LSB). IR is a Word16 (two Word8s:
//! IR_lo x IR_hi).
//!
//! The product nesting is right-associated at the field level:
//!   r0 x (r1 x (r2 x ... x s7))

use theseus_core::{Type, Value};
use theseus_netlist::{bits_of, value_from_bits};

/// Number of bits per general register / scratch word / PC / BR.
pub const W: usize = 8;

/// Number of general registers.
pub const NUM_REGS: usize = 8;

/// Number of scratch words.
pub const NUM_SCRATCH: usize = 8;

/// Total number of Word8-sized fields in State:
/// 8 regs + PC + BR + IR_lo + IR_hi + 8 scratch = 20 fields.
pub const NUM_FIELDS: usize = 20;

/// Total bits in State.
pub const STATE_BITS: usize = NUM_FIELDS * W; // 160

// ── Type helpers ────────────────────────────────────────────────────

/// A single Bit type: `1 + 1`.
pub fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// Right-nested product of `n` Bits: `B x (B x (... x B))`, index 0 = LSB.
pub fn flat(n: usize) -> Type {
    assert!(n >= 1);
    if n == 1 {
        bit()
    } else {
        Type::prod(bit(), flat(n - 1))
    }
}

/// Word8 type: right-nested product of 8 Bits.
pub fn word8() -> Type {
    flat(W)
}

/// Word16 type: `(Word8, Word8)` = IR_lo x IR_hi.
pub fn word16() -> Type {
    Type::prod(word8(), word8())
}

/// Right-nested product of the given types.
fn right_nested(types: &[Type]) -> Type {
    assert!(!types.is_empty());
    if types.len() == 1 {
        types[0].clone()
    } else {
        Type::prod(types[0].clone(), right_nested(&types[1..]))
    }
}

/// The 160-bit State Type as a right-nested product of its 20 Word8 fields.
///
/// Field order (index):
///   0..8   = r0..r7    (general registers)
///   8      = PC
///   9      = BR
///   10     = IR_lo     (low byte of 16-bit IR)
///   11     = IR_hi     (high byte of 16-bit IR)
///   12..20 = s0..s7    (scratch words)
pub fn state_ty() -> Type {
    let fields: Vec<Type> = (0..NUM_FIELDS).map(|_| word8()).collect();
    right_nested(&fields)
}

// ── Field index constants ──────────────────────────────────────────

/// Field index of general register `i` (0..8).
pub fn reg_field(i: usize) -> usize {
    assert!(i < NUM_REGS);
    i
}

/// Field index of PC.
pub const PC_FIELD: usize = 8;

/// Field index of BR.
pub const BR_FIELD: usize = 9;

/// Field index of IR_lo.
pub const IR_LO_FIELD: usize = 10;

/// Field index of IR_hi.
pub const IR_HI_FIELD: usize = 11;

/// Field index of scratch word `i` (0..8).
pub fn scratch_field(i: usize) -> usize {
    assert!(i < NUM_SCRATCH);
    12 + i
}

// ── StateVals encode / decode ──────────────────────────────────────

/// Byte-level representation of the TISC-1 machine State.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateVals {
    pub regs: [u8; NUM_REGS],
    pub pc: u8,
    pub br: u8,
    pub ir: u16,
    pub scratch: [u8; NUM_SCRATCH],
}

impl StateVals {
    /// Encode this State as a `Value` inhabiting `state_ty()`.
    pub fn encode(&self) -> Value {
        let ty = state_ty();
        let mut all_bits: Vec<bool> = Vec::with_capacity(STATE_BITS);
        // Fields in order: r0..r7, PC, BR, IR_lo, IR_hi, s0..s7
        for &r in &self.regs {
            push_u8_bits(&mut all_bits, r);
        }
        push_u8_bits(&mut all_bits, self.pc);
        push_u8_bits(&mut all_bits, self.br);
        push_u8_bits(&mut all_bits, self.ir as u8);       // IR_lo
        push_u8_bits(&mut all_bits, (self.ir >> 8) as u8); // IR_hi
        for &s in &self.scratch {
            push_u8_bits(&mut all_bits, s);
        }
        value_from_bits(&ty, &all_bits).expect("StateVals::encode")
    }

    /// Decode a `Value` of `state_ty()` back to `StateVals`.
    pub fn decode(val: &Value) -> Self {
        let ty = state_ty();
        let all_bits = bits_of(val, &ty).expect("StateVals::decode");
        assert_eq!(all_bits.len(), STATE_BITS);

        let mut regs = [0u8; NUM_REGS];
        for (i, r) in regs.iter_mut().enumerate() {
            *r = read_u8_bits(&all_bits, i * W);
        }
        let pc = read_u8_bits(&all_bits, 8 * W);
        let br = read_u8_bits(&all_bits, 9 * W);
        let ir_lo = read_u8_bits(&all_bits, 10 * W);
        let ir_hi = read_u8_bits(&all_bits, 11 * W);
        let ir = u16::from(ir_lo) | (u16::from(ir_hi) << 8);
        let mut scratch = [0u8; NUM_SCRATCH];
        for (i, s) in scratch.iter_mut().enumerate() {
            *s = read_u8_bits(&all_bits, (12 + i) * W);
        }

        StateVals { regs, pc, br, ir, scratch }
    }
}

fn push_u8_bits(bits: &mut Vec<bool>, val: u8) {
    for i in 0..W {
        bits.push((val >> i) & 1 == 1);
    }
}

fn read_u8_bits(bits: &[bool], offset: usize) -> u8 {
    let mut val = 0u8;
    for i in 0..W {
        if bits[offset + i] {
            val |= 1 << i;
        }
    }
    val
}
