//! TISC-1 State type and Word helpers.
//!
//! ## Carrier types (M-C2 Stage 1a-B)
//!
//! **CpuState** (= `state_ty()`, 20 Word8 fields):
//!   [r0..r7, PC, BR, IR_lo, IR_hi, s0..s7]
//!
//! **MemState** (= `mem_state_ty()`, `MEM_WORDS` Word8 fields):
//!   [m0..m_{MEM_WORDS-1}]
//!
//! **Composite** (= `composite_state_ty()`):
//!   `(CpuState, MemState)` — carrier for `system_step`.
//!
//! Each general register, PC, BR, scratch word, and memory word is a
//! Word8 (8 bits, right-nested product, index 0 = LSB). IR is a Word16
//! (two Word8s: IR_lo x IR_hi).
//!
//! The product nesting is right-associated at the field level within
//! each factor.

use theseus_core::{Type, Value};
use theseus_netlist::{bits_of, value_from_bits};

/// Number of bits per general register / scratch word / PC / BR.
pub const W: usize = 8;

/// Number of general registers.
pub const NUM_REGS: usize = 8;

/// Number of scratch words.
pub const NUM_SCRATCH: usize = 8;

/// Number of memory words (M-C2 Layer 1, ADR-0046 §3.1).
/// Parameterized; C1 demonstrated at N ∈ {2,4,8}. Shipped N=4
/// (the largest N that keeps all tests green without OOM/timeout).
pub const MEM_WORDS: usize = 4;

/// Type-safe memory width — makes illegal widths unrepresentable.
///
/// Replaces raw `n_mem: usize` parameters validated by runtime panics.
/// Only powers of two in {1,2,4,8} are valid.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemWidth {
    W1,
    W2,
    W4,
    W8,
}

/// The shipped memory width as a `MemWidth` (must agree with `MEM_WORDS`).
pub const MEM_WIDTH: MemWidth = MemWidth::W4;

// Static assert: MEM_WIDTH.words() == MEM_WORDS.
// (Rust const-eval doesn't allow trait methods in const context, so we
// use a const array-index trick that fails to compile on mismatch.)
const _: [(); MEM_WORDS] = [(); MEM_WIDTH.words()];

impl MemWidth {
    /// Total number of memory words: 1, 2, 4, or 8.
    pub const fn words(self) -> usize {
        match self {
            MemWidth::W1 => 1,
            MemWidth::W2 => 2,
            MemWidth::W4 => 4,
            MemWidth::W8 => 8,
        }
    }

    /// Number of address bits needed: 0, 1, 2, or 3.
    pub const fn addr_bits(self) -> usize {
        match self {
            MemWidth::W1 => 0,
            MemWidth::W2 => 1,
            MemWidth::W4 => 2,
            MemWidth::W8 => 3,
        }
    }
}

impl TryFrom<usize> for MemWidth {
    type Error = String;

    fn try_from(n: usize) -> Result<Self, Self::Error> {
        match n {
            1 => Ok(MemWidth::W1),
            2 => Ok(MemWidth::W2),
            4 => Ok(MemWidth::W4),
            8 => Ok(MemWidth::W8),
            _ => Err(format!("MemWidth: {n} not in {{1,2,4,8}}")),
        }
    }
}

/// Number of Word8-sized fields in CpuState:
/// 8 regs + PC + BR + IR_lo + IR_hi + 8 scratch = 20.
pub const NUM_CPU_FIELDS: usize = 20;

/// Total number of Word8-sized fields in CpuState (= `NUM_CPU_FIELDS`).
/// The ring step and all non-MEXCH instructions operate on this carrier.
pub const NUM_FIELDS: usize = NUM_CPU_FIELDS;

/// Total bits in CpuState.
pub const STATE_BITS: usize = NUM_FIELDS * W;

/// Total bits in the composite (CpuState, MemState) carrier.
pub const COMPOSITE_BITS: usize = (NUM_CPU_FIELDS + MEM_WORDS) * W;

/// Number of Word8-sized fields in BusMsg: [enable, addr, word].
pub const NUM_BUS_FIELDS: usize = 3;

/// BusMsg field index: enable (bit 0 = bus active).
pub const BUS_ENABLE_FIELD: usize = 0;
/// BusMsg field index: addr (ceil(log2 N) low bits = memory address).
pub const BUS_ADDR_FIELD: usize = 1;
/// BusMsg field index: word (data being transferred).
pub const BUS_WORD_FIELD: usize = 2;

/// Total bits in the system ((CpuState, MemState), BusMsg) carrier.
pub const SYSTEM_BITS: usize = (NUM_CPU_FIELDS + MEM_WORDS + NUM_BUS_FIELDS) * W;

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

/// CpuState type: right-nested product of 20 Word8 fields.
///
/// Field order (index):
///   0..8   = r0..r7    (general registers)
///   8      = PC
///   9      = BR
///   10     = IR_lo     (low byte of 16-bit IR)
///   11     = IR_hi     (high byte of 16-bit IR)
///   12..20 = s0..s7    (scratch words)
///
/// The ring step and all non-MEXCH instructions operate on this carrier.
pub fn state_ty() -> Type {
    let fields: Vec<Type> = (0..NUM_CPU_FIELDS).map(|_| word8()).collect();
    right_nested(&fields)
}

/// MemState type: right-nested product of `MEM_WORDS` Word8 fields.
///
/// Field order (index): 0..MEM_WORDS = m0..m_{MEM_WORDS-1}.
pub fn mem_state_ty() -> Type {
    let fields: Vec<Type> = (0..MEM_WORDS).map(|_| word8()).collect();
    right_nested(&fields)
}

/// Composite state type: `(CpuState, MemState)` — carrier for `system_step`.
pub fn composite_state_ty() -> Type {
    Type::prod(state_ty(), mem_state_ty())
}

/// BusMsg type: right-nested product of 3 Word8 fields [enable, addr, word].
///
/// The bus is a transient factor in `system_state_ty()`; it carries data
/// between the emit/mem/absorb phases and is zero at every step boundary.
pub fn bus_state_ty() -> Type {
    crate::addressing::fields(NUM_BUS_FIELDS)
}

/// System state type: `((CpuState, MemState), BusMsg)` — the 3-factor
/// carrier for `system_step` (Stage 1b-A).
///
/// The inner `(CpuState, MemState)` sub-product is `composite_state_ty()`;
/// BusMsg is the outermost factor, initialized to zero by `encode_system`.
pub fn system_state_ty() -> Type {
    Type::prod(composite_state_ty(), bus_state_ty())
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

/// Field index of memory word `i` (0..MEM_WORDS).
pub fn mem_field(i: usize) -> usize {
    assert!(i < MEM_WORDS);
    20 + i
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
    pub mem: [u8; MEM_WORDS],
}

impl StateVals {
    /// Encode this State's CPU fields as a `Value` inhabiting `state_ty()`
    /// (CpuState, 20 fields). Memory fields are NOT included — use
    /// [`encode_composite`] for the system_step carrier.
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

    /// Decode a `Value` of `state_ty()` (CpuState) back to `StateVals`.
    /// Memory fields are set to zero — use [`decode_composite`] for the
    /// system_step carrier.
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

        StateVals { regs, pc, br, ir, scratch, mem: [0; MEM_WORDS] }
    }
}

// ── Composite encode / decode (system_step carrier) ────────────────

/// Encode a `StateVals` as a composite `Value` inhabiting
/// `composite_state_ty()` = `(CpuState, MemState)`.
pub fn encode_composite(s: &StateVals) -> Value {
    let cpu_ty = state_ty();
    let mem_ty = mem_state_ty();

    let mut cpu_bits: Vec<bool> = Vec::with_capacity(STATE_BITS);
    for &r in &s.regs {
        push_u8_bits(&mut cpu_bits, r);
    }
    push_u8_bits(&mut cpu_bits, s.pc);
    push_u8_bits(&mut cpu_bits, s.br);
    push_u8_bits(&mut cpu_bits, s.ir as u8);
    push_u8_bits(&mut cpu_bits, (s.ir >> 8) as u8);
    for &sc in &s.scratch {
        push_u8_bits(&mut cpu_bits, sc);
    }
    let cpu_val = value_from_bits(&cpu_ty, &cpu_bits).expect("encode_composite: cpu");

    let mut mem_bits: Vec<bool> = Vec::with_capacity(MEM_WORDS * W);
    for &m in &s.mem {
        push_u8_bits(&mut mem_bits, m);
    }
    let mem_val = value_from_bits(&mem_ty, &mem_bits).expect("encode_composite: mem");

    Value::pair(cpu_val, mem_val)
}

/// Decode a composite `Value` of `composite_state_ty()` back to `StateVals`.
pub fn decode_composite(val: &Value) -> StateVals {
    let cpu_ty = state_ty();
    let mem_ty = mem_state_ty();

    let Value::Pair(cpu_val, mem_val) = val else {
        panic!("decode_composite: expected Pair, got {val:?}");
    };

    let cpu_bits = bits_of(cpu_val, &cpu_ty).expect("decode_composite: cpu bits");
    assert_eq!(cpu_bits.len(), STATE_BITS);

    let mut regs = [0u8; NUM_REGS];
    for (i, r) in regs.iter_mut().enumerate() {
        *r = read_u8_bits(&cpu_bits, i * W);
    }
    let pc = read_u8_bits(&cpu_bits, 8 * W);
    let br = read_u8_bits(&cpu_bits, 9 * W);
    let ir_lo = read_u8_bits(&cpu_bits, 10 * W);
    let ir_hi = read_u8_bits(&cpu_bits, 11 * W);
    let ir = u16::from(ir_lo) | (u16::from(ir_hi) << 8);
    let mut scratch = [0u8; NUM_SCRATCH];
    for (i, s) in scratch.iter_mut().enumerate() {
        *s = read_u8_bits(&cpu_bits, (12 + i) * W);
    }

    let mem_bits = bits_of(mem_val, &mem_ty).expect("decode_composite: mem bits");
    assert_eq!(mem_bits.len(), MEM_WORDS * W);
    let mut mem = [0u8; MEM_WORDS];
    for (i, m) in mem.iter_mut().enumerate() {
        *m = read_u8_bits(&mem_bits, i * W);
    }

    StateVals { regs, pc, br, ir, scratch, mem }
}

// ── System encode / decode (3-factor carrier, Stage 1b-A) ─────────

/// Encode a `StateVals` as a system `Value` inhabiting
/// `system_state_ty()` = `((CpuState, MemState), BusMsg)`.
///
/// BusMsg is initialized to all-zero (the bus is quiescent between steps).
/// `StateVals` stays flat — BusMsg is a transient factor, not byte-carrier state.
pub fn encode_system(s: &StateVals) -> Value {
    let composite = encode_composite(s);
    let bus_ty = bus_state_ty();
    let bus_bits = vec![false; NUM_BUS_FIELDS * W];
    let bus_val = value_from_bits(&bus_ty, &bus_bits).expect("encode_system: bus");
    Value::pair(composite, bus_val)
}

/// Decode a system `Value` of `system_state_ty()` back to `StateVals`.
///
/// Projects the BusMsg factor away (it should be zero at step boundaries;
/// the caller is responsible for asserting quiescence if needed).
pub fn decode_system(val: &Value) -> StateVals {
    let Value::Pair(composite_val, _bus_val) = val else {
        panic!("decode_system: expected Pair((composite, bus)), got {val:?}");
    };
    decode_composite(composite_val)
}

/// Decode the BusMsg factor from a system `Value` as `[enable, addr, word]`.
///
/// Returns three bytes; for quiescence checks all three must be zero.
pub fn decode_bus(val: &Value) -> [u8; NUM_BUS_FIELDS] {
    let Value::Pair(_composite_val, bus_val) = val else {
        panic!("decode_bus: expected Pair((composite, bus)), got {val:?}");
    };
    let bus_ty = bus_state_ty();
    let bus_bits = bits_of(bus_val, &bus_ty).expect("decode_bus: bits");
    assert_eq!(bus_bits.len(), NUM_BUS_FIELDS * W);
    let mut result = [0u8; NUM_BUS_FIELDS];
    for (i, r) in result.iter_mut().enumerate() {
        *r = read_u8_bits(&bus_bits, i * W);
    }
    result
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
