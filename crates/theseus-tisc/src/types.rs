//! TISC-1 State type and Word helpers.
//!
//! ## Carrier types (M-C2 Stage 1a-B)
//!
//! **CpuState** (= `state_ty()`, 22 Word8 fields -- A-0 widened IR 16→32,
//! adding two IR byte-fields):
//!   [r0..r7, PC, BR, IR_b0, IR_b1, IR_b2, IR_b3, s0..s7]
//!
//! **MemState** (= `mem_state_ty()`, `MEM_WORDS` Word8 fields):
//!   [m0..m_{MEM_WORDS-1}]
//!
//! **Composite** (= `composite_state_ty()`):
//!   `(CpuState, MemState)` -- carrier for `system_step`.
//!
//! Each general register, PC, BR, scratch word, and memory word is a
//! Word8 (8 bits, right-nested product, index 0 = LSB). IR is a Word32
//! (four Word8s: IR_b0 x IR_b1 x IR_b2 x IR_b3, LSB byte first) -- A-0
//! widened the instruction word 16 → 32 bits.
//!
//! The product nesting is right-associated at the field level within
//! each factor.

use theseus_core::{Type, Value};
use theseus_netlist::{bits_of, value_from_bits};

// ── Layout -- per-program state-layout parameters (B-2.0a) ───────────
//
// B-2 (per-program widths) makes the state layout a per-program parameter
// instead of a global const. B-2.0a introduces the `Layout` struct as the
// single source of truth for the layout parameters and threads it through
// the CORE layout computation (`state_ty`/field offsets), while
// `Layout::default()` reproduces TODAY's fixed layout byte-identically.
//
// The public `const`s below (`W`, `NUM_REGS`, ...) are RETAINED as thin
// aliases of `Layout::default()` fields so the ~500 existing const-callers
// keep compiling unchanged; only the core layout functions gained a
// `Layout`-parameterized `_with` variant. B-2.0b/B-2.1 then vary the
// `Layout` per program -- nothing in B-2.0a changes behavior.

/// Per-program state-layout parameters.
///
/// Every field maps 1:1 onto a former layout `const`. `Layout::default()`
/// is the shipped fixed layout (byte-identical to the pre-B-2 netlist).
/// The `w_ctl` field decouples the decoder-reserved control region (s0/s1,
/// PC operand scratch, the config one-hots) from the data-word width
/// `w_data`, per the design Q3 resolution -- for B-2.0a both are 8.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Layout {
    /// Data-word width (bits per general register / scratch / memory word,
    /// and the ALU datapath width).
    pub w_data: usize,
    /// Control-region width (the decoder-reserved s0/s1 words: config
    /// one-hots + PC/fetch operand scratch). Decoupled from `w_data`
    /// (design Q3-A); B-2.0a keeps `w_ctl == w_data == 8`.
    pub w_ctl: usize,
    /// Number of general registers.
    pub num_regs: usize,
    /// Number of scratch words.
    pub num_scratch: usize,
    /// Number of memory words.
    pub mem_words: usize,
    /// PC value width in bits.
    pub w_pc: usize,
    /// BR (branch-register / come-from-offset) width in bits.
    pub w_br: usize,
}

impl Layout {
    /// The shipped fixed layout -- reproduces the pre-B-2 netlist
    /// byte-identically (W=8, 8 regs, 8 scratch, 4 mem words, 16-bit PC,
    /// 8-bit BR, control region width 8).
    pub const fn default() -> Self {
        Layout {
            w_data: 8,
            w_ctl: 8,
            num_regs: 8,
            num_scratch: 8,
            mem_words: 4,
            w_pc: 16,
            w_br: 8,
        }
    }

    /// Derive the minimal per-program layout (B-2.0b) from two facts about
    /// a compiled program: whether it emits ANY memory op, and the highest
    /// general-register operand index it names.
    ///
    /// - `mem_words = 0` for a CPU-only program (no Mexch/MexchReg) -- it runs
    ///   on the CPU-only `step(rom)` ring, dropping the composite + bus
    ///   machinery entirely; otherwise the default bank size.
    /// - `num_regs = next_pow2(max_named_reg + 1)`, clamped to `[1, 8]` (the
    ///   operand select tree is binary, so the register count rounds up to a
    ///   power of two -- design DF-3).
    ///
    /// `w_data`, `w_ctl`, `w_pc`, `w_br`, and `num_scratch` are kept at their
    /// default values -- data-word narrowing is B-2.1, PC narrowing is B-2.2.
    pub const fn for_program_facts(uses_mem: bool, max_named_reg: Option<usize>) -> Self {
        let d = Layout::default();
        let mem_words = if uses_mem { d.mem_words } else { 0 };
        let num_regs = match max_named_reg {
            None => 1,
            Some(m) => {
                let want = m + 1;
                // next_pow2(want), clamped to [1, d.num_regs].
                let mut p = 1;
                while p < want {
                    p <<= 1;
                }
                if p > d.num_regs {
                    d.num_regs
                } else {
                    p
                }
            }
        };
        Layout {
            num_regs,
            mem_words,
            ..d
        }
    }

    /// Number of Word-sized fields PC occupies (**floor** `w_pc / w_ctl`).
    /// A well-formed layout keeps `w_pc` a positive multiple of `w_ctl` (so
    /// this is exact); the PC builders assert that invariant
    /// (`step::assert_pc_carrier`, Fable B-2.2a MAJOR-1) -- a non-multiple
    /// `w_pc` would under-count the carrier and alias PC onto BR/op wires.
    pub const fn pc_fields(&self) -> usize {
        self.w_pc / self.w_ctl
    }

    /// Number of Word-sized fields BR occupies (`⌈w_br / w_ctl⌉`).
    pub const fn br_fields(&self) -> usize {
        self.w_br / self.w_ctl
    }

    /// PC address space = `2^w_pc`.
    pub const fn pc_addr_space(&self) -> usize {
        1 << self.w_pc
    }

    /// Number of Word-sized fields in CpuState:
    /// regs + PC_lo + BR_lo + 4 IR bytes + scratch + (PC_fields-1)
    /// + (BR_fields-1). At the default layout (w_pc=16→pc_fields=2, w_br=8→
    /// br_fields=1) this is 8+1+1+4+8+1+0 = **23** (there is NO BR_hi field at
    /// the default w_br=8). It becomes 24 only at w_br=16 (the appended BR_hi).
    pub const fn num_cpu_fields(&self) -> usize {
        self.num_regs + 1 + 1 + 4 + self.num_scratch + (self.pc_fields() - 1)
            + (self.br_fields() - 1)
    }

    /// Total number of Word-sized fields in CpuState (= `num_cpu_fields`).
    pub const fn num_fields(&self) -> usize {
        self.num_cpu_fields()
    }

    /// Total bits in CpuState. Under the mixed-width carrier (Option A) this
    /// is `Σ field_width(idx)` over all CPU fields, NOT `num_fields *
    /// w_data`; at the default layout (all fields 8-wide) the two agree.
    pub const fn state_bits(&self) -> usize {
        let mut total = 0;
        let mut i = 0;
        while i < self.num_cpu_fields() {
            total += self.field_width(i);
            i += 1;
        }
        total
    }

    // ── Field index helpers ────────────────────────────────────────

    /// Field index of general register `i`.
    pub const fn reg_field(&self, i: usize) -> usize {
        i
    }

    /// Field index of PC low byte.
    pub const fn pc_field(&self) -> usize {
        self.num_regs
    }

    /// Field index of BR low byte.
    pub const fn br_field(&self) -> usize {
        self.num_regs + 1
    }

    /// Field index of IR byte `k` (0..4).
    pub const fn ir_byte_field(&self, k: usize) -> usize {
        self.num_regs + 2 + k
    }

    /// Field index of scratch word `i`.
    pub const fn scratch_field(&self, i: usize) -> usize {
        self.num_regs + 6 + i
    }

    /// Field index of PC high byte (appended after the scratch words).
    pub const fn pc_hi_field(&self) -> usize {
        self.num_regs + 6 + self.num_scratch
    }

    /// Field index of BR high byte (appended after the scratch words AND the
    /// `pc_fields()-1` PC-hi fields). At the default layout (`w_pc=16`,
    /// `w_br=8`) `br_fields()==1` so there is no BR-hi field and this index is
    /// never addressed; at `w_br=16` (`br_fields()==2`) it is the single
    /// appended BR word, positioned right after PC_hi (W_BR-a). The appended
    /// region order is: `[PC-hi × (pc_fields-1), BR-hi × (br_fields-1)]`, which
    /// is exactly the order `encode_with`/`decode_with` marshal and the order
    /// `field_off` walks -- so the two stay in lockstep.
    pub const fn br_hi_field(&self) -> usize {
        self.num_regs + 6 + self.num_scratch + (self.pc_fields() - 1)
    }

    /// Field index of BR word `k` (`0` → low byte at `br_field()`; `k>=1` →
    /// the `k`-th appended BR-hi word). `k` must be `< br_fields()`.
    pub const fn br_word_field(&self, k: usize) -> usize {
        if k == 0 {
            self.br_field()
        } else {
            self.br_hi_field() + (k - 1)
        }
    }

    /// Field index of memory word `i` in the composite carrier.
    pub const fn mem_field(&self, i: usize) -> usize {
        self.num_cpu_fields() + i
    }

    /// The bit-width of CpuState/MemState field `idx` under Option A
    /// (the mixed-width carrier, design Q1). This is the single point that
    /// decides, per field, whether it is a **data** word (`w_data`) or a
    /// **control** word (`w_ctl`):
    ///
    /// - general registers `r0..`, operand-scratch `s2..`, and memory words
    ///   are DATA → `w_data` (they hold `Flat(w_data)` values and are the
    ///   operand space the crossbar swaps within);
    /// - `s0` (operand-A buffer + decode workspace), `s1` (config one-hots +
    ///   predicate), PC bytes, BR, and the four IR bytes are CONTROL →
    ///   `w_ctl` (the fixed 8-bit control region, per Option A / DF-2).
    ///
    /// At `Layout::default()` (`w_data == w_ctl == 8`) every field is 8 wide,
    /// so `state_ty_with` and every width-aware router collapse to the shipped
    /// uniform `word8()` machinery -- byte-identical.
    pub const fn field_width(&self, idx: usize) -> usize {
        // Registers r0..r{num_regs-1}: DATA.
        if idx < self.num_regs {
            return self.w_data;
        }
        // Scratch words: s0/s1 are CONTROL, s2.. are DATA operand space.
        let s0 = self.scratch_field(0);
        if idx >= s0 && idx < s0 + self.num_scratch {
            let s = idx - s0;
            return if s <= 1 { self.w_ctl } else { self.w_data };
        }
        // Memory words (composite carrier): DATA.
        if idx >= self.num_cpu_fields() {
            return self.w_data;
        }
        // Everything else in the CPU region -- PC bytes, BR, IR bytes -- CONTROL.
        self.w_ctl
    }

    /// The per-field width vector for the whole CpuState product
    /// (`num_cpu_fields()` entries), used to drive the width-aware field
    /// routers. At the default layout this is `[8; 24]`.
    pub fn cpu_field_widths(&self) -> Vec<usize> {
        (0..self.num_cpu_fields()).map(|i| self.field_width(i)).collect()
    }
}

/// Number of bits per general register / scratch word / memory word.
pub const W: usize = Layout::default().w_data;

/// PC value width (A-6.4 / A-2 widen-PC). Widened from `W` (8) to 16 so
/// program bodies with `B > 256` can run: the end-of-step update
/// `PC += BR + 1` is arithmetic mod `2^W_PC`, so the reachable step index
/// (∝ B) must fit in `W_PC` bits. PC occupies `⌈W_PC / W⌉ = 2` Word8
/// fields (PC_lo at `PC_FIELD`, PC_hi at `PC_HI_FIELD`).
pub const W_PC: usize = Layout::default().w_pc;

/// BR (branch-register / come-from-offset) width. **Stays 8** (minimal
/// widening, resolves design open-Q #2): the end-of-step update adds BR
/// into the 16-bit PC as a **sign-extended** 8-bit 2's-complement offset
/// (`pc_add_br`, step.rs). A backward jump is minted exactly as before --
/// `δu = (256 − iter_len) mod 256` (an 8-bit register value swapped into
/// BR) -- and its top bit (set whenever `iter_len ≤ 128`) sign-extends to
/// `0xFF..` so `PC += BR + 1` walks *backward* across the full 16-bit PC.
/// So the entire come-from / SWAPBR / δu-mint machinery is byte-identical
/// to the 8-bit-PC design; only the *adder* changes (it sign-extends).
/// This is why "W_BR = 8 suffices" (design §5/§6): an 8-bit BR expresses
/// every backward offset up to −128 and every forward offset up to +127.
pub const W_BR: usize = Layout::default().w_br;

/// Number of Word8 fields PC occupies (`⌈W_PC / W⌉`).
pub const PC_FIELDS: usize = Layout::default().pc_fields();
/// Number of Word8 fields BR occupies (`⌈W_BR / W⌉` = 1).
pub const BR_FIELDS: usize = Layout::default().br_fields();

/// PC address space = `2^W_PC` (was 256). Every wrap / cap / dead-zone
/// bound routes through this single constant -- no scattered `256`s.
pub const PC_ADDR_SPACE: usize = Layout::default().pc_addr_space();

/// Number of general registers.
pub const NUM_REGS: usize = Layout::default().num_regs;

/// Number of scratch words.
pub const NUM_SCRATCH: usize = Layout::default().num_scratch;

/// Number of memory words (M-C2 Layer 1, ADR-0046 §3.1).
/// Parameterized; C1 demonstrated at N ∈ {2,4,8}. Shipped N=4
/// (the largest N that keeps all tests green without OOM/timeout).
pub const MEM_WORDS: usize = Layout::default().mem_words;

/// Type-safe memory width -- makes illegal widths unrepresentable.
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
/// 8 regs + PC_lo + BR_lo + IR_b0..IR_b3 (32-bit IR) + 8 scratch
/// = 22, + (PC_FIELDS-1 = 1) + (BR_FIELDS-1 = 0) = **23** at the default
/// layout (PC_hi is appended; NO BR_hi at the default w_br=8).
///
/// A-6.4 widen-PC **appends** the extra PC byte (PC_hi at index 22)
/// rather than inserting it, so every reg / IR / scratch field index
/// stays stable and no `bits_of` consumer that slices a contiguous PC
/// byte is disturbed (the fetch and PC arithmetic address bytes by
/// *field index*, never by bit-adjacency -- see `pc_add_br` / `pc_inc` /
/// `fetch` in step.rs). BR stays a single byte (W_BR = 8, sign-extended
/// into PC by the adder), so no BR_hi field is added.
pub const NUM_CPU_FIELDS: usize = Layout::default().num_cpu_fields();

/// Total number of Word8-sized fields in CpuState (= `NUM_CPU_FIELDS`).
/// The ring step and all non-MEXCH instructions operate on this carrier.
pub const NUM_FIELDS: usize = Layout::default().num_fields();

/// Total bits in CpuState.
pub const STATE_BITS: usize = Layout::default().state_bits();

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

/// Word16 type: `(Word8, Word8)`.
pub fn word16() -> Type {
    Type::prod(word8(), word8())
}

/// Word32 type: right-nested product of 4 Word8s = the 32-bit IR
/// (IR_b0 x IR_b1 x IR_b2 x IR_b3, LSB byte first).
pub fn word32() -> Type {
    Type::prod(word8(), Type::prod(word8(), word16()))
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

/// Data-word type for a given layout: right-nested product of `w_data` bits.
/// At the default layout this is `word8()`.
pub fn data_word(layout: &Layout) -> Type {
    flat(layout.w_data)
}

/// CpuState type for a given `Layout` (core, layout-parameterized).
///
/// Field order (index), at the default layout (24 fields):
///   0..8   = r0..r7    (general registers)
///   8      = PC_lo     (bits 0..8 of the 16-bit PC)
///   9      = BR        (8-bit branch offset, sign-extended into PC)
///   10..14 = IR_b0..IR_b3 (four bytes of the 32-bit IR, LSB byte first)
///   14..22 = s0..s7    (scratch words)
///   22     = PC_hi     (bits 8..16 of the 16-bit PC) -- appended (A-6.4)
///
/// The ring step and all non-MEXCH instructions operate on this carrier.
pub fn state_ty_with(layout: &Layout) -> Type {
    // Mixed-width carrier (Option A / B-2.1c): each field takes its own
    // `field_width` -- DATA fields (regs, s2.., mem) are `flat(w_data)`,
    // CONTROL fields (s0, s1, PC, BR, IR) are `flat(w_ctl)`. At the default
    // layout every field is `flat(8) = word8()`, so this is byte-identical.
    let fields: Vec<Type> = (0..layout.num_cpu_fields())
        .map(|i| flat(layout.field_width(i)))
        .collect();
    right_nested(&fields)
}

/// CpuState type at the default layout (byte-identical to today).
pub fn state_ty() -> Type {
    state_ty_with(&Layout::default())
}

/// MemState type for a given `Layout`: right-nested product of
/// `mem_words` data-word fields.
pub fn mem_state_ty_with(layout: &Layout) -> Type {
    let w = data_word(layout);
    let fields: Vec<Type> = (0..layout.mem_words).map(|_| w.clone()).collect();
    right_nested(&fields)
}

/// MemState type at the default layout.
///
/// Field order (index): 0..MEM_WORDS = m0..m_{MEM_WORDS-1}.
pub fn mem_state_ty() -> Type {
    mem_state_ty_with(&Layout::default())
}

/// Composite state type: `(CpuState, MemState)` -- carrier for `system_step`.
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

/// System state type: `((CpuState, MemState), BusMsg)` -- the 3-factor
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
    Layout::default().reg_field(i)
}

/// Field index of PC low byte (bits 0..8 of the `W_PC`-bit PC).
pub const PC_FIELD: usize = Layout::default().pc_field();

/// Field index of BR low byte (bits 0..8 of the `W_BR`-bit BR).
pub const BR_FIELD: usize = Layout::default().br_field();

/// Field index of PC high byte (bits 8..16). Appended after the scratch
/// words so the low-index layout is unchanged (A-6.4 widen-PC).
pub const PC_HI_FIELD: usize = Layout::default().pc_hi_field();

/// Field index of BR high byte (bits 8..16) under the **default** layout.
/// At the default (`w_br=8`, `br_fields()==1`) BR occupies a single field, so
/// this index is one-past-BR and unused; it becomes live only at `w_br=16`
/// (W_BR-a). Callers on the wide path must guard on `layout.br_fields()`.
pub const BR_HI_FIELD: usize = Layout::default().br_hi_field();

/// The Word8 field index of PC byte `k` under the **default** layout
/// (`k=0` → low, `k=1` → high). B-2.2a: this is a default-layout convenience
/// (it has no `Layout` param and no callers in the reduced-`w_pc` path -- see
/// the grep in the B-2.2 design Q4.1); the bound is
/// `Layout::default().pc_fields()` (= 2 at w_pc=16) rather than a hardcoded
/// "2", so it does not silently bake the 2-field assumption. A reduced-layout
/// caller should address PC via `layout.pc_field()` / `layout.pc_hi_field()`
/// guarded on `layout.pc_fields()`.
pub const fn pc_byte_field(k: usize) -> usize {
    assert!(
        k < Layout::default().pc_fields(),
        "pc_byte_field: k out of range for the default layout's pc_fields()"
    );
    match k {
        0 => PC_FIELD,
        1 => PC_HI_FIELD,
        _ => unreachable!(),
    }
}

/// Field index of IR byte 0 (bits 0..8 of the 32-bit IR).
pub const IR_B0_FIELD: usize = Layout::default().ir_byte_field(0);
/// Field index of IR byte 1 (bits 8..16).
pub const IR_B1_FIELD: usize = Layout::default().ir_byte_field(1);
/// Field index of IR byte 2 (bits 16..24).
pub const IR_B2_FIELD: usize = Layout::default().ir_byte_field(2);
/// Field index of IR byte 3 (bits 24..32).
pub const IR_B3_FIELD: usize = Layout::default().ir_byte_field(3);

/// Legacy alias: IR byte 0 (was `IR_lo` of the 16-bit IR).
pub const IR_LO_FIELD: usize = IR_B0_FIELD;
/// Legacy alias: IR byte 1 (was `IR_hi` of the 16-bit IR).
pub const IR_HI_FIELD: usize = IR_B1_FIELD;

/// Field index of scratch word `i` (0..8).
pub fn scratch_field(i: usize) -> usize {
    assert!(i < NUM_SCRATCH);
    Layout::default().scratch_field(i)
}

/// Field index of memory word `i` (0..MEM_WORDS).
pub fn mem_field(i: usize) -> usize {
    assert!(i < MEM_WORDS);
    Layout::default().mem_field(i)
}

// ── StateVals encode / decode ──────────────────────────────────────

/// Byte-level representation of the TISC-1 machine State.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateVals {
    pub regs: [u8; NUM_REGS],
    /// Program counter -- 16-bit (A-6.4 widen-PC). Low byte at `PC_FIELD`,
    /// high byte at `PC_HI_FIELD`.
    pub pc: u16,
    /// Branch register / come-from offset -- up to 16-bit (`W_BR ∈ {8,16}`),
    /// sign-extended into the 16-bit PC by the end-of-step adder
    /// (`pc_add_br`). W_BR-a widened the storage from `u8` to `u16` so a
    /// `w_br=16` layout can marshal a two-byte offset; at the default
    /// (`w_br=8`) only the low byte is populated/marshalled and the RUN-path
    /// interpreter still treats BR as 8-bit (`pc_update`/`exec_instr` take the
    /// low byte) -- semantics are byte-identical. The two-byte come-from
    /// arithmetic is W_BR-b.
    pub br: u16,
    pub ir: u32,
    pub scratch: [u8; NUM_SCRATCH],
    pub mem: [u8; MEM_WORDS],
}

/// Push the four bytes of a 32-bit IR word onto `bits` (LSB byte first).
fn push_ir_bits(bits: &mut Vec<bool>, ir: u32) {
    push_u8_bits(bits, ir as u8);
    push_u8_bits(bits, (ir >> 8) as u8);
    push_u8_bits(bits, (ir >> 16) as u8);
    push_u8_bits(bits, (ir >> 24) as u8);
}

/// Read the four IR bytes back into a `u32` at the given bit offset
/// (LSB byte first -- matches [`push_ir_bits`]).
fn read_ir_bits(bits: &[bool], offset: usize) -> u32 {
    let b0 = read_u8_bits(bits, offset);
    let b1 = read_u8_bits(bits, offset + W);
    let b2 = read_u8_bits(bits, offset + 2 * W);
    let b3 = read_u8_bits(bits, offset + 3 * W);
    u32::from(b0) | (u32::from(b1) << 8) | (u32::from(b2) << 16) | (u32::from(b3) << 24)
}

impl StateVals {
    /// Encode this State's CPU fields as a `Value` inhabiting `state_ty()`
    /// (CpuState, 22 fields -- A-0 32-bit IR). Memory fields are NOT
    /// included -- use [`encode_composite`] for the system_step carrier.
    pub fn encode(&self) -> Value {
        let ty = state_ty();
        let mut all_bits: Vec<bool> = Vec::with_capacity(STATE_BITS);
        // Fields in order: r0..r7, PC_lo, BR, IR_b0..b3, s0..s7, PC_hi.
        for &r in &self.regs {
            push_u8_bits(&mut all_bits, r);
        }
        push_u8_bits(&mut all_bits, self.pc as u8); // PC_lo
        // MINOR-3: HARD assert (not debug_assert) -- the default carrier has one
        // 8-bit BR field, so a >8-bit BR must fail LOUDLY (in release too) rather
        // than silently truncate once W_BR-b lets wide BR values into the model.
        assert_eq!(self.br >> 8, 0, "encode: default carrier has one 8-bit BR field");
        push_u8_bits(&mut all_bits, self.br as u8); // BR (8-bit)
        push_ir_bits(&mut all_bits, self.ir); // IR_b0..IR_b3 (32-bit)
        for &s in &self.scratch {
            push_u8_bits(&mut all_bits, s);
        }
        push_u8_bits(&mut all_bits, (self.pc >> 8) as u8); // PC_hi (appended)
        value_from_bits(&ty, &all_bits).expect("StateVals::encode")
    }

    /// B-2.1b: encode this State's CPU fields against a **reduced** `Layout`
    /// (`state_ty_with(layout)`). Only `layout.num_regs` registers and
    /// `layout.num_scratch` scratch words are plumbed into the reduced-field
    /// carrier; the remaining `StateVals` regs/scratch are asserted zero so
    /// the reduced state is a faithful projection (the compiler shrinks
    /// `num_regs` to `next_pow2(max_named_reg+1)`, so every dropped register
    /// is un-named and stays zero across the run).
    ///
    /// Field order at the reduced layout follows the `Layout::*_field`
    /// helpers: r0..r{num_regs-1}, PC_lo, BR, IR_b0..b3, s0..s{num_scratch-1},
    /// PC_hi. `w_data` is 8 in B-2.1b so each word is a full byte.
    pub fn encode_with(&self, layout: &Layout) -> Value {
        assert!(layout.w_data <= W, "encode_with: w_data must be ≤ 8");
        assert_eq!(layout.w_ctl, W, "encode_with: w_ctl is the fixed 8-bit control region");
        // Dropped (un-named) registers/scratch must be zero -- the reduced
        // state cannot represent them, and the compiler only shrinks past
        // registers it proved un-named.
        for (i, &r) in self.regs.iter().enumerate() {
            if i >= layout.num_regs {
                assert_eq!(r, 0, "encode_with: dropped reg r{i} must be 0, got {r:#04x}");
            }
        }
        for (i, &s) in self.scratch.iter().enumerate() {
            if i >= layout.num_scratch {
                assert_eq!(s, 0, "encode_with: dropped scratch s{i} must be 0, got {s:#04x}");
            }
        }
        // At a reduced `w_data`, a DATA word (register / s2..) may only carry
        // `w_data` bits -- the high bits are un-representable and must be 0.
        let wd = layout.w_data;
        if wd < W {
            let mask: u8 = ((1u16 << wd) - 1) as u8;
            for (i, &r) in self.regs.iter().take(layout.num_regs).enumerate() {
                assert_eq!(r & !mask, 0, "encode_with: reg r{i}={r:#04x} exceeds w_data={wd}");
            }
            // s0/s1 are CONTROL (8-bit); s2.. are DATA (w_data).
            for (i, &s) in self.scratch.iter().take(layout.num_scratch).enumerate() {
                if i >= 2 {
                    assert_eq!(s & !mask, 0, "encode_with: scratch s{i}={s:#04x} exceeds w_data={wd}");
                }
            }
        }
        let ty = state_ty_with(layout);
        let mut all_bits: Vec<bool> = Vec::with_capacity(layout.state_bits());
        // Fields in `field_width` order: r0..r{nr-1} (w_data), PC_lo (w_ctl),
        // BR (w_ctl), IR_b0..b3 (w_ctl), s0..s{ns-1} (s0/s1 w_ctl, s2.. w_data),
        // PC_hi (w_ctl).
        for &r in self.regs.iter().take(layout.num_regs) {
            push_n_bits(&mut all_bits, r, wd);
        }
        push_u8_bits(&mut all_bits, self.pc as u8); // PC_lo (w_ctl=8)
        push_u8_bits(&mut all_bits, self.br as u8); // BR_lo (w_ctl=8)
        push_ir_bits(&mut all_bits, self.ir); // IR_b0..IR_b3 (w_ctl=8)
        for (i, &s) in self.scratch.iter().take(layout.num_scratch).enumerate() {
            if i <= 1 {
                push_u8_bits(&mut all_bits, s); // s0/s1 CONTROL (8-bit)
            } else {
                push_n_bits(&mut all_bits, s, wd); // s2.. DATA (w_data)
            }
        }
        // B-2.2a: PC_hi is appended only when PC occupies >= 2 fields (the
        // default w_pc=16). At w_pc=8 (pc_fields()==1) there is no PC_hi
        // field, so pushing it would over-fill the reduced carrier; the high
        // PC byte must be 0 (the reduced PC is 8-bit).
        if layout.pc_fields() >= 2 {
            push_u8_bits(&mut all_bits, (self.pc >> 8) as u8); // PC_hi (w_ctl=8)
        } else {
            assert_eq!(
                self.pc >> 8,
                0,
                "encode_with: pc {:#06x} exceeds w_pc={} (no PC_hi field)",
                self.pc,
                layout.w_pc
            );
        }
        // W_BR-a: the BR-hi word(s) are appended after PC_hi (matching the
        // `br_hi_field()` ordering). At the default (`br_fields()==1`) none are
        // pushed and the whole `self.br` must fit in the single 8-bit BR field.
        // At `w_br=16` (`br_fields()==2`) the BR high byte follows PC_hi. The
        // low BR byte was already pushed inline above (BR_lo, w_ctl=8).
        if layout.br_fields() < 2 {
            assert_eq!(
                self.br >> 8,
                0,
                "encode_with: br {:#06x} exceeds w_br={} (no BR-hi field)",
                self.br,
                layout.w_br
            );
        }
        for k in 1..layout.br_fields() {
            push_u8_bits(&mut all_bits, (self.br >> (8 * k)) as u8); // BR-hi word k
        }
        value_from_bits(&ty, &all_bits).expect("StateVals::encode_with")
    }

    /// B-2.1b: decode a `Value` of `state_ty_with(layout)` (reduced CpuState)
    /// back to a full `StateVals`. Registers/scratch beyond the reduced
    /// counts read back as zero (they were never plumbed). Memory is zero.
    pub fn decode_with(layout: &Layout, val: &Value) -> Self {
        assert!(layout.w_data <= W, "decode_with: w_data must be ≤ 8");
        assert_eq!(layout.w_ctl, W, "decode_with: w_ctl is the fixed 8-bit control region");
        let ty = state_ty_with(layout);
        let all_bits = bits_of(val, &ty).expect("StateVals::decode_with");
        assert_eq!(all_bits.len(), layout.state_bits());

        // Bit offset of a CPU field = Σ field_width(<idx) (mixed-width carrier).
        let field_off = |idx: usize| -> usize {
            (0..idx).map(|j| layout.field_width(j)).sum()
        };
        let wd = layout.w_data;

        let mut regs = [0u8; NUM_REGS];
        for (i, r) in regs.iter_mut().enumerate().take(layout.num_regs) {
            *r = read_n_bits(&all_bits, field_off(layout.reg_field(i)), wd);
        }
        let pc = {
            let lo = read_u8_bits(&all_bits, field_off(layout.pc_field()));
            // B-2.2a: read the PC_hi byte only when PC occupies >= 2 fields.
            // At w_pc=8 (pc_fields()==1) there is no PC_hi field, so hi = 0.
            let hi = if layout.pc_fields() >= 2 {
                read_u8_bits(&all_bits, field_off(layout.pc_hi_field()))
            } else {
                0
            };
            u16::from(lo) | (u16::from(hi) << 8)
        };
        // W_BR-a: reassemble BR from its `br_fields()` words (low at
        // `br_field()`, the appended hi words at `br_hi_field()+..`). At the
        // default (`br_fields()==1`) this reads exactly the single low byte.
        let br = {
            let mut v = 0u16;
            for k in 0..layout.br_fields() {
                let byte = read_u8_bits(&all_bits, field_off(layout.br_word_field(k)));
                v |= u16::from(byte) << (8 * k);
            }
            v
        };
        let ir = read_ir_bits(&all_bits, field_off(layout.ir_byte_field(0)));
        let mut scratch = [0u8; NUM_SCRATCH];
        for (i, s) in scratch.iter_mut().enumerate().take(layout.num_scratch) {
            let off = field_off(layout.scratch_field(i));
            *s = if i <= 1 {
                read_u8_bits(&all_bits, off) // s0/s1 CONTROL (8-bit)
            } else {
                read_n_bits(&all_bits, off, wd) // s2.. DATA (w_data)
            };
        }

        StateVals {
            regs,
            pc,
            br,
            ir,
            scratch,
            mem: [0; MEM_WORDS],
        }
    }

    /// Decode a `Value` of `state_ty()` (CpuState) back to `StateVals`.
    /// Memory fields are set to zero -- use [`decode_composite`] for the
    /// system_step carrier.
    pub fn decode(val: &Value) -> Self {
        let ty = state_ty();
        let all_bits = bits_of(val, &ty).expect("StateVals::decode");
        assert_eq!(all_bits.len(), STATE_BITS);

        let mut regs = [0u8; NUM_REGS];
        for (i, r) in regs.iter_mut().enumerate() {
            *r = read_u8_bits(&all_bits, i * W);
        }
        let pc = read_pc_bits(&all_bits);
        let br = u16::from(read_u8_bits(&all_bits, BR_FIELD * W));
        let ir = read_ir_bits(&all_bits, 10 * W);
        let mut scratch = [0u8; NUM_SCRATCH];
        for (i, s) in scratch.iter_mut().enumerate() {
            *s = read_u8_bits(&all_bits, (14 + i) * W);
        }

        StateVals {
            regs,
            pc,
            br,
            ir,
            scratch,
            mem: [0; MEM_WORDS],
        }
    }

    /// Read an operand-indexed word over `reg ∪ scratch` (A-0): idx
    /// `0..8 → r[idx]`, `8..16 → s[idx-8]`. Panics on idx ≥ 16 (the
    /// reserved locations the assembler rejects). This is the fast-sim
    /// counterpart of the datapath's `select_operand` crossbar, so
    /// `exec_instr` agrees with the hardware on scratch operands.
    #[inline]
    pub fn operand_get(&self, idx: u8) -> u8 {
        let i = idx as usize;
        if i < NUM_REGS {
            self.regs[i]
        } else {
            self.scratch[i - NUM_REGS]
        }
    }

    /// Write an operand-indexed word over `reg ∪ scratch` (A-0).
    /// Panics on idx ≥ 16.
    #[inline]
    pub fn operand_set(&mut self, idx: u8, val: u8) {
        let i = idx as usize;
        if i < NUM_REGS {
            self.regs[i] = val;
        } else {
            self.scratch[i - NUM_REGS] = val;
        }
    }

    /// Swap two operand-indexed words over `reg ∪ scratch` (A-0).
    #[inline]
    pub fn operand_swap(&mut self, a: u8, b: u8) {
        let va = self.operand_get(a);
        let vb = self.operand_get(b);
        self.operand_set(a, vb);
        self.operand_set(b, va);
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
    push_u8_bits(&mut cpu_bits, s.pc as u8); // PC_lo
    // MINOR-3: HARD assert -- loud failure (release too) on a >8-bit BR reaching
    // the single-field composite carrier, rather than a silent `as u8` truncation.
    assert_eq!(s.br >> 8, 0, "encode_composite: default carrier has one 8-bit BR field");
    push_u8_bits(&mut cpu_bits, s.br as u8); // BR (8-bit)
    push_ir_bits(&mut cpu_bits, s.ir);
    for &sc in &s.scratch {
        push_u8_bits(&mut cpu_bits, sc);
    }
    push_u8_bits(&mut cpu_bits, (s.pc >> 8) as u8); // PC_hi (appended)
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
    let pc = read_pc_bits(&cpu_bits);
    let br = u16::from(read_u8_bits(&cpu_bits, BR_FIELD * W));
    let ir = read_ir_bits(&cpu_bits, 10 * W);
    let mut scratch = [0u8; NUM_SCRATCH];
    for (i, s) in scratch.iter_mut().enumerate() {
        *s = read_u8_bits(&cpu_bits, (14 + i) * W);
    }

    let mem_bits = bits_of(mem_val, &mem_ty).expect("decode_composite: mem bits");
    assert_eq!(mem_bits.len(), MEM_WORDS * W);
    let mut mem = [0u8; MEM_WORDS];
    for (i, m) in mem.iter_mut().enumerate() {
        *m = read_u8_bits(&mem_bits, i * W);
    }

    StateVals {
        regs,
        pc,
        br,
        ir,
        scratch,
        mem,
    }
}

// ── System encode / decode (3-factor carrier, Stage 1b-A) ─────────

/// Encode a `StateVals` as a system `Value` inhabiting
/// `system_state_ty()` = `((CpuState, MemState), BusMsg)`.
///
/// BusMsg is initialized to all-zero (the bus is quiescent between steps).
/// `StateVals` stays flat -- BusMsg is a transient factor, not byte-carrier state.
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

/// Push the low `w` bits of `val` (LSB first). For the mixed-width carrier:
/// DATA words push `w_data` bits, CONTROL words push `w_ctl = 8`.
fn push_n_bits(bits: &mut Vec<bool>, val: u8, w: usize) {
    for i in 0..w {
        bits.push((val >> i) & 1 == 1);
    }
}

/// Read a `w`-bit word (LSB first) at `offset` back into a `u8` (high bits 0).
fn read_n_bits(bits: &[bool], offset: usize, w: usize) -> u8 {
    let mut val = 0u8;
    for i in 0..w {
        if bits[offset + i] {
            val |= 1 << i;
        }
    }
    val
}

/// Read a `W_PC`-bit PC value split across `PC_FIELD` (low byte) and
/// `PC_HI_FIELD` (high byte) from a full CpuState bit vector.
fn read_pc_bits(bits: &[bool]) -> u16 {
    let lo = read_u8_bits(bits, PC_FIELD * W);
    let hi = read_u8_bits(bits, PC_HI_FIELD * W);
    u16::from(lo) | (u16::from(hi) << 8)
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
