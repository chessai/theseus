//! Symbolic netlist walk: proves `netlist(instr) ≡ instr_semantics(instr)`
//! at full Flat(8) via BDD walk of the decoder's instruction netlist.
//!
//! # Trust core (NEW -- a bug here makes the netlist reconciliation vacuous)
//!
//! - `eval_bdd`: symbolic Iso interpreter (mirrors `theseus_eval::eval`)
//! - Gate BDD synthesis from `PrimDef::forward` truth tables (MAJOR-4),
//!   packed per output column and built by Shannon expansion (ADR-0050)
//! - Extended `VarMap` (regs + s0 + BR + mem) -- the ONE TCB mapping
//!
//! Defended by: walk-vs-`run_rom_on_cpu` bootstrap (T2), negative controls,
//! the convention-pinning test (T1), and the full-`system_step` authority tier.

use std::collections::HashMap;

use biodivine_lib_bdd::{Bdd, BddVariableSet};
use theseus_core::{adjoint, enumerate_values, Iso, Type};
use theseus_netlist::{bits_of, rail_width};

use crate::asm::Instr;
use crate::bveq::VarMap;
use crate::types::{MEM_WORDS, NUM_BUS_FIELDS, NUM_CPU_FIELDS, NUM_REGS, W};

// ── Constants: extended VarMap operand indices ──────────────────────

/// Number of operands in the extended VarMap: 8 regs + s0 + BR + 4 mem.
pub const EXTENDED_NUM_OPERANDS: usize = NUM_REGS + 1 + 1 + MEM_WORDS; // 14

/// VarMap operand index for scratch word s0 (CpuState field 14 -- A-0
/// 32-bit IR shifted the scratch base 12 → 14).
pub const OP_S0: usize = NUM_REGS; // 8

/// VarMap operand index for BR (CpuState field 9).
pub const OP_BR: usize = NUM_REGS + 1; // 9

/// VarMap operand base index for memory words m0..m3.
pub const OP_MEM_BASE: usize = NUM_REGS + 2; // 10

// ── SV: symbolic value ─────────────────────────────────────────────

/// Symbolic value mirroring `Value`, with BDD leaves at bit positions.
///
/// Convention: `Bit(bdd)` where `bdd = true` ↔ `Left(Unit)` ↔ bit = 1.
/// Pinned by `theseus_netlist::bits_of` (`Left(Unit) → [true]`).
#[derive(Clone, Debug)]
pub enum SV {
    Unit,
    Bit(Bdd),
    Pair(Box<SV>, Box<SV>),
}

impl SV {
    pub fn pair(a: SV, b: SV) -> Self {
        SV::Pair(Box::new(a), Box::new(b))
    }
}

// ── Error ──────────────────────────────────────────────────────────

/// Errors from the symbolic netlist walk (MINOR-6: fail closed, never panic).
#[derive(Debug)]
pub enum NetlistWalkError {
    /// Iso node outside the loop-free product fragment.
    UnsupportedIso(String),
    /// SV shape mismatch during interpretation.
    DomainError(String),
    /// Gate synthesis or application error.
    GateError(String),
}

impl std::fmt::Display for NetlistWalkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedIso(s) => write!(f, "unsupported iso: {s}"),
            Self::DomainError(s) => write!(f, "domain error: {s}"),
            Self::GateError(s) => write!(f, "gate error: {s}"),
        }
    }
}

impl std::error::Error for NetlistWalkError {}

// ── SV helpers ─────────────────────────────────────────────────────

/// Build a right-nested product SV from a slice.
fn sv_right_nested(svs: &[SV]) -> SV {
    debug_assert!(!svs.is_empty());
    if svs.len() == 1 {
        svs[0].clone()
    } else {
        SV::pair(svs[0].clone(), sv_right_nested(&svs[1..]))
    }
}

/// Constant byte: 8-bit right-nested product of constant Bit SVs.
pub fn sv_word8_const(val: u8, vs: &BddVariableSet) -> SV {
    let bits: Vec<SV> = (0..W)
        .map(|i| {
            SV::Bit(if (val >> i) & 1 == 1 {
                vs.mk_true()
            } else {
                vs.mk_false()
            })
        })
        .collect();
    sv_right_nested(&bits)
}

/// Symbolic byte from VarMap operand `op`.
pub fn sv_word8_symbolic(map: &VarMap, op: usize) -> SV {
    let bits: Vec<SV> = (0..W)
        .map(|i| SV::Bit(map.bdd_vars().mk_var(map.var(op, i))))
        .collect();
    sv_right_nested(&bits)
}

/// Flatten SV to bit BDDs in structural (left-to-right, LSB-first) order.
pub fn sv_flatten(sv: &SV) -> Vec<Bdd> {
    match sv {
        SV::Unit => vec![],
        SV::Bit(b) => vec![b.clone()],
        SV::Pair(a, b) => {
            let mut v = sv_flatten(a);
            v.extend(sv_flatten(b));
            v
        }
    }
}

/// Rebuild SV from flat BDDs guided by a type's structure.
fn sv_from_flat(ty: &Type, bits: &mut impl Iterator<Item = Bdd>) -> SV {
    match ty {
        Type::One => SV::Unit,
        Type::Sum(a, b) if matches!((a.as_ref(), b.as_ref()), (Type::One, Type::One)) => {
            SV::Bit(bits.next().expect("sv_from_flat: bit underflow"))
        }
        Type::Prod(a, b) => {
            let left = sv_from_flat(a, bits);
            let right = sv_from_flat(b, bits);
            SV::pair(left, right)
        }
        other => panic!("sv_from_flat: unsupported type {other:?}"),
    }
}

/// Extract the k-th field from a right-nested product of n fields.
pub fn extract_field(sv: &SV, k: usize, n: usize) -> &SV {
    if n == 1 {
        debug_assert_eq!(k, 0);
        return sv;
    }
    match sv {
        SV::Pair(left, right) => {
            if k == 0 {
                left
            } else {
                extract_field(right, k - 1, n - 1)
            }
        }
        _ => panic!("extract_field: expected Pair at k={k}, n={n}"),
    }
}

/// Extract 8 bit BDDs from a Word8 SV.
pub fn extract_word8_bdds(sv: &SV) -> [Bdd; W] {
    let flat = sv_flatten(sv);
    assert_eq!(
        flat.len(),
        W,
        "extract_word8_bdds: expected {W} bits, got {}",
        flat.len()
    );
    std::array::from_fn(|i| flat[i].clone())
}

/// Convert a constant-BDD Word8 SV back to a concrete byte.
pub fn sv_to_u8(sv: &SV, vs: &BddVariableSet) -> u8 {
    let bits = sv_flatten(sv);
    assert_eq!(bits.len(), W, "sv_to_u8: expected {W} bits");
    let t = vs.mk_true();
    let mut val = 0u8;
    for (i, b) in bits.iter().enumerate() {
        if *b == t {
            val |= 1 << i;
        }
    }
    val
}

/// Convert a constant-BDD Word16 SV (Pair(lo8, hi8)) back to a concrete u16.
pub fn sv_to_u16(sv: &SV, vs: &BddVariableSet) -> u16 {
    match sv {
        SV::Pair(lo, hi) => u16::from(sv_to_u8(lo, vs)) | (u16::from(sv_to_u8(hi, vs)) << 8),
        _ => panic!("sv_to_u16: expected Pair(lo, hi)"),
    }
}

// ── Gate truth table synthesis (MAJOR-4) ───────────────────────────

/// Gates with at most this many input bits pack each output column into one
/// `u64` (`2^6` rows). Every cell-family gate (K1–K3, R1–R3, CNOT, Toffoli,
/// CCCNOT: ≤ 5 wires) takes this path (ADR-0050).
const WORD_INPUTS: usize = 6;

/// Widest gate the packed table admits: `2^24` rows, 2 MiB per column.
/// A wider prim is rejected (MINOR-6: fail closed), never materialized.
const MAX_GATE_INPUTS: usize = 24;

/// `LIT[i]`: the rows of a word whose index has bit `i` set -- the packed
/// column of input `i` itself.
const LIT: [u64; WORD_INPUTS] = [
    0xAAAA_AAAA_AAAA_AAAA,
    0xCCCC_CCCC_CCCC_CCCC,
    0xF0F0_F0F0_F0F0_F0F0,
    0xFF00_FF00_FF00_FF00,
    0xFFFF_0000_FFFF_0000,
    0xFFFF_FFFF_0000_0000,
];

/// The live rows of a one-word column over `m ≤ 6` inputs: the low `2^m` bits.
const fn word_rows(m: usize) -> u64 {
    if m >= WORD_INPUTS {
        u64::MAX
    } else {
        (1 << (1 << m)) - 1
    }
}

/// Word `w` of the packed column of input `j` over `> 6` inputs.
fn lit_word(j: usize, w: usize) -> u64 {
    if j < WORD_INPUTS {
        LIT[j]
    } else if (w >> (j - WORD_INPUTS)) & 1 == 1 {
        u64::MAX
    } else {
        0
    }
}

/// Gather the even-indexed bits of `x` into its low 32 bits: the
/// input-0 = 0 cofactor of a packed word (`x >> 1` gives input-0 = 1).
fn pack_even(x: u64) -> u64 {
    let mut x = x & 0x5555_5555_5555_5555;
    x = (x | (x >> 1)) & 0x3333_3333_3333_3333;
    x = (x | (x >> 2)) & 0x0F0F_0F0F_0F0F_0F0F;
    x = (x | (x >> 4)) & 0x00FF_00FF_00FF_00FF;
    x = (x | (x >> 8)) & 0x0000_FFFF_0000_FFFF;
    (x | (x >> 16)) & 0x0000_0000_FFFF_FFFF
}

/// Row index of an input bit string: bit `i` of the row is `bits[i]`
/// (`bits_of` order, LSB first). The ONE place the packing order is
/// encoded (ADR-0050); pinned by `tests::row_order_pins_bits_of`.
fn row_of(bits: &[bool]) -> usize {
    bits.iter().rev().fold(0, |x, &b| (x << 1) | usize::from(b))
}

/// A packed truth-table column, or a cofactor of one, over `m` inputs:
/// bit `x` is the output on the input row [`row_of`] maps to `x`. Rows
/// `bits_of` never produces (uneven-sum padding) are 0, exactly as the
/// minterm expansion left them. Invariant: bits past row `2^m` are 0.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Mask {
    /// `m ≤ 6`: the low `2^m` bits of one word.
    Word(u64),
    /// `m > 6`: `2^(m-6)` words; row `x` is word `x / 64`, bit `x % 64`.
    Words(Vec<u64>),
}

impl Mask {
    /// The all-0 column over `m` inputs.
    fn zero(m: usize) -> Self {
        if m <= WORD_INPUTS {
            Mask::Word(0)
        } else {
            Mask::Words(vec![0; 1 << (m - WORD_INPUTS)])
        }
    }

    fn set(&mut self, x: usize) {
        match self {
            Mask::Word(w) => *w |= 1 << x,
            Mask::Words(ws) => ws[x / 64] |= 1 << (x % 64),
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            Mask::Word(w) => *w == 0,
            Mask::Words(ws) => ws.iter().all(|&w| w == 0),
        }
    }

    fn is_ones(&self, m: usize) -> bool {
        match self {
            Mask::Word(w) => *w == word_rows(m),
            Mask::Words(ws) => ws.iter().all(|&w| w == u64::MAX),
        }
    }

    /// Whether `self == !other` over `m` inputs.
    fn is_complement_of(&self, other: &Mask, m: usize) -> bool {
        match (self, other) {
            (Mask::Word(a), Mask::Word(b)) => a ^ b == word_rows(m),
            (Mask::Words(a), Mask::Words(b)) => a.iter().zip(b).all(|(x, y)| x ^ y == u64::MAX),
            _ => false,
        }
    }

    /// The input this column passes through: `Some((j, true))` if it is
    /// input `j`, `Some((j, false))` if it is `¬input j`.
    fn literal(&self, m: usize) -> Option<(usize, bool)> {
        (0..m).find_map(|j| {
            let (pos, neg) = match self {
                Mask::Word(w) => {
                    let lit = LIT[j] & word_rows(m);
                    (*w == lit, *w == lit ^ word_rows(m))
                }
                Mask::Words(ws) => (
                    ws.iter().enumerate().all(|(i, &w)| w == lit_word(j, i)),
                    ws.iter().enumerate().all(|(i, &w)| w == !lit_word(j, i)),
                ),
            };
            (pos || neg).then_some((j, pos))
        })
    }

    /// Cofactors on input 0, `(f|x₀=0, f|x₀=1)`, each over `m - 1` inputs.
    fn split(&self, m: usize) -> (Mask, Mask) {
        match self {
            Mask::Word(w) => (Mask::Word(pack_even(*w)), Mask::Word(pack_even(w >> 1))),
            Mask::Words(ws) => {
                // Cofactor row y sits at bit 2y + x₀: words 2k, 2k+1 pack into word k.
                let half = |x0: u32| -> Vec<u64> {
                    let (pairs, _) = ws.as_chunks::<2>();
                    pairs
                        .iter()
                        .map(|&[a, b]| pack_even(a >> x0) | (pack_even(b >> x0) << 32))
                        .collect()
                };
                let (lo, hi) = (half(0), half(1));
                if m - 1 == WORD_INPUTS {
                    (Mask::Word(lo[0]), Mask::Word(hi[0]))
                } else {
                    (Mask::Words(lo), Mask::Words(hi))
                }
            }
        }
    }
}

/// A gate's truth table packed by output column (ADR-0050), ready for
/// BDD application.
struct GateTable {
    /// `cols[j]`: output bit `j` over every input row.
    cols: Vec<Mask>,
    /// Number of input bits.
    n_in: usize,
    /// Output type (for rebuilding SV structure).
    out_ty: Type,
}

/// Synthesize a gate's packed truth table from its `PrimDef::forward` map.
///
/// Uses `bits_of` on the admission-checked `forward` map -- the identical
/// table `theseus_eval::eval` runs (MAJOR-4: no hand-written gate BDDs).
/// Each value lands on the row [`row_of`] its input bits name, so the
/// packing never depends on `enumerate_values` order.
fn synthesize_gate(def: &theseus_core::PrimDef) -> Result<GateTable, NetlistWalkError> {
    let err =
        |what: String| NetlistWalkError::GateError(format!("prim '{}': {what}", def.forward_name));
    let n_in = rail_width(&def.input_ty).map_err(|e| err(format!("input width: {e:?}")))?;
    let n_out = rail_width(&def.output_ty).map_err(|e| err(format!("output width: {e:?}")))?;
    if n_in > MAX_GATE_INPUTS {
        return Err(err(format!(
            "{n_in} input bits exceed the packed-table limit {MAX_GATE_INPUTS}"
        )));
    }
    let mut cols = vec![Mask::zero(n_in); n_out];
    for v in enumerate_values(&def.input_ty) {
        let in_bits =
            bits_of(&v, &def.input_ty).map_err(|e| err(format!("bits_of input: {e:?}")))?;
        let out_v = def
            .forward
            .get(&v)
            .ok_or_else(|| err(format!("input not in forward map: {v:?}")))?;
        let out_bits =
            bits_of(out_v, &def.output_ty).map_err(|e| err(format!("bits_of output: {e:?}")))?;
        if in_bits.len() != n_in || out_bits.len() != n_out {
            return Err(err(format!(
                "bits_of width disagrees with rail_width at {v:?}"
            )));
        }
        let x = row_of(&in_bits);
        for (col, &b) in cols.iter_mut().zip(&out_bits) {
            if b {
                col.set(x);
            }
        }
    }
    Ok(GateTable {
        cols,
        n_in,
        out_ty: def.output_ty.clone(),
    })
}

/// Apply a gate's packed truth table to BDD inputs: one [`shannon`] build
/// per output column (ADR-0050).
fn apply_gate(table: &GateTable, input_bdds: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    table
        .cols
        .iter()
        .map(|col| shannon(col, input_bdds, vs))
        .collect()
}

/// Build the BDD of packed column `f` over `inputs` by Shannon expansion,
/// `f = ite(x₀, f|x₀=1, f|x₀=0)`, splitting inputs in `bits_of` order
/// (ADR-0050). Cells list controls first, so K2's swap arm comes out as
/// `ite(c1, ite(c2, b, a), a)`. Every level short-circuits from the masks
/// alone: constant and pass-through columns cost no BDD op, equal cofactors
/// or a constant input skip the split, and a constant or complementary
/// cofactor folds the `ite` into one binary op.
fn shannon(f: &Mask, inputs: &[Bdd], vs: &BddVariableSet) -> Bdd {
    let m = inputs.len();
    if f.is_zero() {
        return vs.mk_false();
    }
    if f.is_ones(m) {
        return vs.mk_true();
    }
    if let Some((j, pos)) = f.literal(m) {
        return if pos {
            inputs[j].clone()
        } else {
            inputs[j].not()
        };
    }
    let (lo, hi) = f.split(m);
    let (x, rest) = (&inputs[0], &inputs[1..]);
    if lo == hi || x.is_false() {
        return shannon(&lo, rest, vs);
    }
    if x.is_true() {
        return shannon(&hi, rest, vs);
    }
    #[cfg(test)]
    tests::APPLY_OPS.with(|n| n.set(n.get() + 1));
    let n = m - 1;
    if lo.is_zero() {
        x.and(&shannon(&hi, rest, vs))
    } else if hi.is_zero() {
        shannon(&lo, rest, vs).and_not(x)
    } else if lo.is_ones(n) {
        x.imp(&shannon(&hi, rest, vs))
    } else if hi.is_ones(n) {
        x.or(&shannon(&lo, rest, vs))
    } else if hi.is_complement_of(&lo, n) {
        x.xor(&shannon(&lo, rest, vs))
    } else {
        Bdd::if_then_else(x, &shannon(&hi, rest, vs), &shannon(&lo, rest, vs))
    }
}

// ── Walk context (gate cache) ──────────────────────────────────────

/// Holds the gate truth-table cache for an `eval_bdd` session.
pub struct WalkCtx {
    cache: HashMap<String, GateTable>,
}

impl WalkCtx {
    pub fn new() -> Self {
        WalkCtx {
            cache: HashMap::new(),
        }
    }
}

impl Default for WalkCtx {
    fn default() -> Self {
        Self::new()
    }
}

// ── eval_bdd: the symbolic Iso interpreter ─────────────────────────

/// Symbolically evaluate an Iso on an SV, producing an SV.
///
/// Structurally recurses exactly like `theseus_eval::eval`, except BDD
/// leaves replace concrete values and fuel/trace/iter are rejected
/// (MINOR-6: fail closed outside the loop-free product fragment).
///
/// Fragment: {Id, Sym, Seq, Times, SwapProd, AssocL/RProd, UniteProd,
/// UnitiProd, SwapSum(One,One), Prim}.
pub fn eval_bdd(
    iso: &Iso,
    sv: SV,
    vs: &BddVariableSet,
    ctx: &mut WalkCtx,
) -> Result<SV, NetlistWalkError> {
    match iso {
        Iso::Id(_) => Ok(sv),

        Iso::Sym(f) => {
            let adj = adjoint(f);
            eval_bdd(&adj, sv, vs, ctx)
        }

        Iso::Seq(f, g) => {
            let mid = eval_bdd(f, sv, vs, ctx)?;
            eval_bdd(g, mid, vs, ctx)
        }

        Iso::Times(f, g) => match sv {
            SV::Pair(a, b) => {
                let ra = eval_bdd(f, *a, vs, ctx)?;
                let rb = eval_bdd(g, *b, vs, ctx)?;
                Ok(SV::pair(ra, rb))
            }
            _ => Err(NetlistWalkError::DomainError("Times: expected Pair".into())),
        },

        Iso::SwapProd(_, _) => match sv {
            SV::Pair(a, b) => Ok(SV::pair(*b, *a)),
            _ => Err(NetlistWalkError::DomainError(
                "SwapProd: expected Pair".into(),
            )),
        },

        Iso::AssocLProd(_, _, _) => match sv {
            SV::Pair(a, bc) => match *bc {
                SV::Pair(b, c) => Ok(SV::pair(SV::pair(*a, *b), *c)),
                _ => Err(NetlistWalkError::DomainError(
                    "AssocLProd: inner not Pair".into(),
                )),
            },
            _ => Err(NetlistWalkError::DomainError(
                "AssocLProd: expected Pair".into(),
            )),
        },

        Iso::AssocRProd(_, _, _) => match sv {
            SV::Pair(ab, c) => match *ab {
                SV::Pair(a, b) => Ok(SV::pair(*a, SV::pair(*b, *c))),
                _ => Err(NetlistWalkError::DomainError(
                    "AssocRProd: inner not Pair".into(),
                )),
            },
            _ => Err(NetlistWalkError::DomainError(
                "AssocRProd: expected Pair".into(),
            )),
        },

        Iso::UniteProd(_) => match sv {
            SV::Pair(u, v) => match *u {
                SV::Unit => Ok(*v),
                _ => Err(NetlistWalkError::DomainError(
                    "UniteProd: expected Unit in left".into(),
                )),
            },
            _ => Err(NetlistWalkError::DomainError(
                "UniteProd: expected Pair".into(),
            )),
        },

        Iso::UnitiProd(_) => Ok(SV::pair(SV::Unit, sv)),

        // SwapSum(One, One) = bit NOT (structural case, not a prim -- MAJOR-4).
        Iso::SwapSum(a, b) => match (a, b) {
            (Type::One, Type::One) => match sv {
                SV::Bit(bdd) => Ok(SV::Bit(bdd.not())),
                _ => Err(NetlistWalkError::DomainError(
                    "SwapSum(1,1): expected Bit".into(),
                )),
            },
            _ => Err(NetlistWalkError::UnsupportedIso(format!(
                "SwapSum({a:?}, {b:?}): only (One, One) supported"
            ))),
        },

        // Prim: synthesize gate BDDs from the forward truth table.
        Iso::Prim(def) => {
            let name = &def.forward_name;
            if !ctx.cache.contains_key(name) {
                let table = synthesize_gate(def)?;
                ctx.cache.insert(name.clone(), table);
            }
            let table = &ctx.cache[name];
            let input_bdds = sv_flatten(&sv);
            if input_bdds.len() != table.n_in {
                return Err(NetlistWalkError::GateError(format!(
                    "prim '{}': expected {} input bits, got {}",
                    name,
                    table.n_in,
                    input_bdds.len()
                )));
            }
            let out_bdds = apply_gate(table, &input_bdds, vs);
            let mut iter = out_bdds.into_iter();
            Ok(sv_from_flat(&table.out_ty, &mut iter))
        }

        // ── Everything outside the fragment: fail closed (MINOR-6) ──
        Iso::Plus(..) => Err(NetlistWalkError::UnsupportedIso("Plus".into())),
        Iso::Trace(..) => Err(NetlistWalkError::UnsupportedIso("Trace".into())),
        Iso::Iter(..) => Err(NetlistWalkError::UnsupportedIso("Iter".into())),
        Iso::Fold(..) => Err(NetlistWalkError::UnsupportedIso("Fold".into())),
        Iso::Unfold(..) => Err(NetlistWalkError::UnsupportedIso("Unfold".into())),
        Iso::AssocLSum(..) => Err(NetlistWalkError::UnsupportedIso("AssocLSum".into())),
        Iso::AssocRSum(..) => Err(NetlistWalkError::UnsupportedIso("AssocRSum".into())),
        Iso::UniteSum(..) => Err(NetlistWalkError::UnsupportedIso("UniteSum".into())),
        Iso::UnitiSum(..) => Err(NetlistWalkError::UnsupportedIso("UnitiSum".into())),
        Iso::Distrib(..) => Err(NetlistWalkError::UnsupportedIso("Distrib".into())),
        Iso::Factor(..) => Err(NetlistWalkError::UnsupportedIso("Factor".into())),
    }
}

// ── Extended VarMap ────────────────────────────────────────────────

/// Create the extended VarMap for the netlist walk.
///
/// Operand layout (interleaved bit ordering):
///   r0-r7 (0-7), s0 (8), BR (9), m0-m3 (10-13).
///
/// This is the ONE TCB mapping: both sides of the comparison
/// (walk output and `instr_semantics`) are encoded through it.
pub fn extended_varmap() -> VarMap {
    VarMap::new(EXTENDED_NUM_OPERANDS)
        .expect("extended VarMap: 14 operands × 8 bits = 112 ≤ u16::MAX")
}

// ── Initial state construction ─────────────────────────────────────

/// Build the initial CpuState SV for a one-arm `instruction(op)` walk.
///
/// Fields: r0-r7 symbolic, PC=0, BR symbolic, IR=concrete word,
/// s0 symbolic (or 0 if `pin_s0_zero`), s1-s7=0.
pub fn build_cpu_init(map: &VarMap, ir_word: u32, pin_s0_zero: bool) -> SV {
    let vs = map.bdd_vars();
    let mut fields = Vec::with_capacity(NUM_CPU_FIELDS);

    // Fields 0-7: r0-r7 symbolic
    for i in 0..NUM_REGS {
        fields.push(sv_word8_symbolic(map, i));
    }
    // Field 8: PC = 0
    fields.push(sv_word8_const(0, vs));
    // Field 9: BR symbolic
    fields.push(sv_word8_symbolic(map, OP_BR));
    // Fields 10-13: IR_b0..IR_b3 (32-bit IR, A-0), LSB byte first.
    fields.push(sv_word8_const(ir_word as u8, vs));
    fields.push(sv_word8_const((ir_word >> 8) as u8, vs));
    fields.push(sv_word8_const((ir_word >> 16) as u8, vs));
    fields.push(sv_word8_const((ir_word >> 24) as u8, vs));
    // Field 14: s0 -- symbolic or pinned to 0 for Addi/Subi
    // (unpredicated immediate mask requires s0=0)
    if pin_s0_zero {
        fields.push(sv_word8_const(0, vs));
    } else {
        fields.push(sv_word8_symbolic(map, OP_S0));
    }
    // Fields 15-21: s1-s7 = 0
    for _ in 1..crate::types::NUM_SCRATCH {
        fields.push(sv_word8_const(0, vs));
    }
    // Field 22: PC_hi = 0 (A-6.4 widen-PC appended byte; PC = 0 above).
    fields.push(sv_word8_const(0, vs));

    debug_assert_eq!(fields.len(), NUM_CPU_FIELDS);
    sv_right_nested(&fields)
}

/// Build the initial system state SV for a `system_step([word])` walk.
///
/// System state = `((CpuState, MemState), BusState)`.
///
/// CpuState: r0-r7 symbolic, PC=0, BR=0 (MINOR-7: PC_out = BR+PC+1),
/// IR=0 (fetched from ROM), s0 symbolic (or 0), s1-s7=0.
/// MemState: m0-m3 symbolic. BusState: 3×Word8 = 0.
pub fn build_system_init(map: &VarMap, pin_s0_zero: bool) -> SV {
    let vs = map.bdd_vars();

    // CpuState (NUM_CPU_FIELDS fields -- A-0 32-bit IR, A-6.4 PC_hi appended)
    let mut cpu_fields = Vec::with_capacity(NUM_CPU_FIELDS);
    for i in 0..NUM_REGS {
        cpu_fields.push(sv_word8_symbolic(map, i));
    }
    cpu_fields.push(sv_word8_const(0, vs)); // PC = 0
    cpu_fields.push(sv_word8_const(0, vs)); // BR = 0 (MINOR-7)
                                            // IR_b0..IR_b3 = 0 (fetched from ROM); 32-bit IR (A-0).
    cpu_fields.push(sv_word8_const(0, vs));
    cpu_fields.push(sv_word8_const(0, vs));
    cpu_fields.push(sv_word8_const(0, vs));
    cpu_fields.push(sv_word8_const(0, vs));
    if pin_s0_zero {
        cpu_fields.push(sv_word8_const(0, vs)); // s0 = 0 for Addi/Subi
    } else {
        cpu_fields.push(sv_word8_symbolic(map, OP_S0));
    }
    for _ in 1..crate::types::NUM_SCRATCH {
        cpu_fields.push(sv_word8_const(0, vs)); // s1-s7 = 0
    }
    // PC_hi = 0 (A-6.4 widen-PC appended byte; PC = 0 above).
    cpu_fields.push(sv_word8_const(0, vs));
    debug_assert_eq!(cpu_fields.len(), NUM_CPU_FIELDS);
    let cpu_sv = sv_right_nested(&cpu_fields);

    // MemState (MEM_WORDS = 4 fields, symbolic)
    let mut mem_fields = Vec::with_capacity(MEM_WORDS);
    for i in 0..MEM_WORDS {
        mem_fields.push(sv_word8_symbolic(map, OP_MEM_BASE + i));
    }
    let mem_sv = sv_right_nested(&mem_fields);

    // BusState (NUM_BUS_FIELDS = 3 fields, constant 0)
    let mut bus_fields = Vec::with_capacity(NUM_BUS_FIELDS);
    for _ in 0..NUM_BUS_FIELDS {
        bus_fields.push(sv_word8_const(0, vs));
    }
    let bus_sv = sv_right_nested(&bus_fields);

    // System = ((Cpu, Mem), Bus)
    SV::pair(SV::pair(cpu_sv, mem_sv), bus_sv)
}

/// Build a concrete (all-constant-BDD) system state SV from `StateVals`.
///
/// Used by the T2 bootstrap test to compare walk output against
/// `run_rom_on_cpu` at concrete points.
pub fn build_concrete_system_sv(s: &crate::types::StateVals, vs: &BddVariableSet) -> SV {
    // CpuState field order (state_ty()): r0..r7, PC_lo, BR, IR_b0..b3,
    // s0..s7, PC_hi (A-6.4 widen-PC appended PC_hi; A-0 32-bit IR = 4 bytes).
    let mut cpu_fields = Vec::with_capacity(NUM_CPU_FIELDS);
    for &r in &s.regs {
        cpu_fields.push(sv_word8_const(r, vs));
    }
    cpu_fields.push(sv_word8_const(s.pc as u8, vs)); // PC_lo
    cpu_fields.push(sv_word8_const(s.br as u8, vs)); // BR (8-bit, default carrier)
    cpu_fields.push(sv_word8_const(s.ir as u8, vs)); // IR_b0
    cpu_fields.push(sv_word8_const((s.ir >> 8) as u8, vs)); // IR_b1
    cpu_fields.push(sv_word8_const((s.ir >> 16) as u8, vs)); // IR_b2
    cpu_fields.push(sv_word8_const((s.ir >> 24) as u8, vs)); // IR_b3
    for &sc in &s.scratch {
        cpu_fields.push(sv_word8_const(sc, vs));
    }
    cpu_fields.push(sv_word8_const((s.pc >> 8) as u8, vs)); // PC_hi (appended)
    let cpu_sv = sv_right_nested(&cpu_fields);

    let mut mem_fields = Vec::with_capacity(MEM_WORDS);
    for &m in &s.mem {
        mem_fields.push(sv_word8_const(m, vs));
    }
    let mem_sv = sv_right_nested(&mem_fields);

    let mut bus_fields = Vec::with_capacity(NUM_BUS_FIELDS);
    for _ in 0..NUM_BUS_FIELDS {
        bus_fields.push(sv_word8_const(0, vs));
    }
    let bus_sv = sv_right_nested(&bus_fields);

    SV::pair(SV::pair(cpu_sv, mem_sv), bus_sv)
}

// ── Helpers ────────────────────────────────────────────────────────

/// Whether this instruction requires s0 pinned to 0 in the initial state.
///
/// Addi/Subi use an unpredicated CNOT mask that copies the immediate
/// into s0 before adding. If s0 ≠ 0, the XOR produces `s0 ^ imm`
/// instead of `imm`, giving the wrong result (decoder.rs:720).
/// Symbolic s0 correctly REJECTS these two instructions (spike E2).
pub fn needs_s0_pinned(instr: &Instr) -> bool {
    matches!(instr, Instr::Addi { .. } | Instr::Subi { .. })
}

// ── Tests: packed tables vs the minterm oracle (ADR-0050) ──────────

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use theseus_core::PrimDef;
    use theseus_netlist::{prim_from_bits, CellKind, RoleSwapKind};

    use super::*;
    use crate::types::{bit, flat};

    thread_local! {
        /// BDD apply ops `shannon` issued on this thread.
        pub(super) static APPLY_OPS: Cell<usize> = const { Cell::new(0) };
    }

    /// `apply_gate`, also returning the BDD apply ops it issued.
    fn counted_apply(table: &GateTable, inputs: &[Bdd], vs: &BddVariableSet) -> (Vec<Bdd>, usize) {
        let before = APPLY_OPS.with(Cell::get);
        let out = apply_gate(table, inputs, vs);
        (out, APPLY_OPS.with(Cell::get) - before)
    }

    // ── Oracle: the pre-ADR-0050 row table + minterm expansion ─────

    /// One truth-table row: `(input_bits, output_bits)`.
    type Row = (Vec<bool>, Vec<bool>);

    /// `(input_bits, output_bits)` per value, in `enumerate_values` order.
    fn minterm_rows(def: &PrimDef) -> Vec<Row> {
        enumerate_values(&def.input_ty)
            .iter()
            .map(|v| {
                let out = &def.forward[v];
                (
                    bits_of(v, &def.input_ty).unwrap(),
                    bits_of(out, &def.output_ty).unwrap(),
                )
            })
            .collect()
    }

    /// For each output bit j, OR together the minterms (AND of input
    /// literals) for all rows where output bit j is 1.
    fn apply_minterm(
        rows: &[Row],
        n_out: usize,
        input_bdds: &[Bdd],
        vs: &BddVariableSet,
    ) -> Vec<Bdd> {
        let mut outputs = Vec::with_capacity(n_out);
        for j in 0..n_out {
            let mut out_bdd = vs.mk_false();
            for (in_bits, out_bits) in rows {
                if out_bits[j] {
                    let mut term = vs.mk_true();
                    for (i, &b) in in_bits.iter().enumerate() {
                        if b {
                            term = term.and(&input_bdds[i]);
                        } else {
                            term = term.and(&input_bdds[i].not());
                        }
                    }
                    out_bdd = out_bdd.or(&term);
                }
            }
            outputs.push(out_bdd);
        }
        outputs
    }

    // ── Inputs: plain variables, then composite BDDs ───────────────

    /// SplitMix64: a fixed-seed generator for reproducible random BDDs.
    struct Rng(u64);

    impl Rng {
        fn next(&mut self) -> u64 {
            self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
            z ^ (z >> 31)
        }

        fn below(&mut self, n: usize) -> usize {
            (self.next() % n as u64) as usize
        }
    }

    /// A small random formula: a literal combined with 1-3 more by and/or/xor.
    fn random_bdd(rng: &mut Rng, vs: &BddVariableSet) -> Bdd {
        let vars = vs.variables();
        let lit = |rng: &mut Rng| vs.mk_literal(vars[rng.below(vars.len())], rng.next() & 1 == 1);
        let mut f = lit(rng);
        for _ in 0..1 + rng.below(3) {
            let g = lit(rng);
            f = match rng.below(3) {
                0 => f.and(&g),
                1 => f.or(&g),
                _ => f.xor(&g),
            };
        }
        f
    }

    /// Input vectors for a `k`-input gate: plain variables; all-random
    /// formulas; then mixes with constants and copies/complements of an
    /// earlier input (the constant-input and equal-cofactor paths).
    fn input_sets(k: usize, vs: &BddVariableSet, rng: &mut Rng) -> Vec<Vec<Bdd>> {
        let vars = vs.variables();
        let mut sets = vec![(0..k).map(|i| vs.mk_var(vars[i])).collect::<Vec<_>>()];
        for round in 0..4 {
            let mut ins: Vec<Bdd> = Vec::with_capacity(k);
            for i in 0..k {
                let b = match rng.below(4) {
                    2 if round > 0 => {
                        if rng.next() & 1 == 1 {
                            vs.mk_true()
                        } else {
                            vs.mk_false()
                        }
                    }
                    3 if round > 0 && i > 0 => {
                        let src = &ins[rng.below(i)];
                        if rng.next() & 1 == 1 {
                            src.clone()
                        } else {
                            src.not()
                        }
                    }
                    _ => random_bdd(rng, vs),
                };
                ins.push(b);
            }
            sets.push(ins);
        }
        sets
    }

    /// Assert the Shannon build equals the minterm oracle column by column
    /// (biodivine BDDs are canonical, so `==` is semantic equality).
    fn assert_same(got: &[Bdd], want: &[Bdd], what: &str) {
        assert_eq!(got.len(), want.len(), "{what}: output count");
        for (j, (g, w)) in got.iter().zip(want).enumerate() {
            assert!(
                g == w,
                "{what}: output bit {j} diverges from the minterm oracle"
            );
        }
    }

    /// `apply_gate` on `def`'s packed table == the minterm oracle on every input set.
    fn assert_prim_matches_oracle(def: &PrimDef, vs: &BddVariableSet, rng: &mut Rng) {
        let table = synthesize_gate(def).unwrap();
        let rows = minterm_rows(def);
        for inputs in input_sets(table.n_in, vs, rng) {
            let got = apply_gate(&table, &inputs, vs);
            let want = apply_minterm(&rows, table.cols.len(), &inputs, vs);
            assert_same(&got, &want, &def.forward_name);
        }
    }

    /// A one-column table over `k` inputs with row `x` set iff `f(x)`,
    /// and its minterm rows (input `i` = bit `i` of `x`).
    fn column(k: usize, f: impl Fn(usize) -> bool) -> (GateTable, Vec<Row>) {
        let mut col = Mask::zero(k);
        let mut rows = Vec::new();
        for x in 0..1usize << k {
            if f(x) {
                col.set(x);
            }
            rows.push(((0..k).map(|i| (x >> i) & 1 == 1).collect(), vec![f(x)]));
        }
        let table = GateTable {
            cols: vec![col],
            n_in: k,
            out_ty: bit(),
        };
        (table, rows)
    }

    fn cell_prim(name: &str, arity: usize, eval: impl Fn(&[bool]) -> Vec<bool>) -> PrimDef {
        prim_from_bits(name, flat(arity), flat(arity), eval).unwrap()
    }

    // ── Packing order ──────────────────────────────────────────────

    /// Pins the row packing: input `i` is bit `i` of the row (`bits_of`
    /// order), read off each value's bits -- NOT its `enumerate_values`
    /// index, which walks `Left` (bit 1) first, outer field first.
    #[test]
    fn row_order_pins_bits_of() {
        let ty = flat(3);
        let rows: Vec<usize> = enumerate_values(&ty)
            .iter()
            .map(|v| row_of(&bits_of(v, &ty).unwrap()))
            .collect();
        assert_eq!(rows, [7, 3, 5, 1, 6, 2, 4, 0]);

        // Toffoli (c1, c2, t): the controls pass as LIT[0], LIT[1]; the
        // target is LIT[2] ^ (LIT[0] & LIT[1]) over the 8 live rows.
        let tof = cell_prim("tof", 3, |b| vec![b[0], b[1], b[2] ^ (b[0] & b[1])]);
        let table = synthesize_gate(&tof).unwrap();
        assert_eq!(table.n_in, 3);
        assert_eq!(
            table.cols,
            [Mask::Word(0xAA), Mask::Word(0xCC), Mask::Word(0x78)]
        );

        // Uneven sum 1 + B×B: Left(()) pads to [1, 0, 0] (row 1); rows 3, 5,
        // 7 are never produced and stay 0 in every column, as in the minterm
        // expansion.
        let ty = Type::sum(Type::One, Type::prod(bit(), bit()));
        let pad = prim_from_bits("pad_id", ty.clone(), ty, <[bool]>::to_vec).unwrap();
        let table = synthesize_gate(&pad).unwrap();
        assert_eq!(
            table.cols,
            [Mask::Word(0x02), Mask::Word(0x44), Mask::Word(0x50)]
        );
        let vs = BddVariableSet::new_anonymous(10);
        assert_prim_matches_oracle(&pad, &vs, &mut Rng(0xADC0_0050));
    }

    /// `Mask::Words` packs and splits like the one-word form: row `x` of a
    /// multi-word column is word `x / 64`, and the input-0 cofactors of an
    /// 8-input column regroup into the 7-, then 6-input layouts.
    #[test]
    fn wide_masks_pack_and_split() {
        let mut m = Mask::zero(8);
        for x in [0, 63, 64, 130, 255] {
            m.set(x);
        }
        assert_eq!(m, Mask::Words(vec![1 | (1 << 63), 1, 1 << 2, 1 << 63]));
        let (lo, hi) = m.split(8);
        // x₀ = 0: rows 0, 64, 130 -> 0, 32, 65; x₀ = 1: rows 63, 255 -> 31, 127.
        assert_eq!(lo, Mask::Words(vec![1 | (1 << 32), 1 << 1]));
        assert_eq!(hi, Mask::Words(vec![1 << 31, 1 << 63]));
        // x₀ = 0 again: rows 0, 32 -> 0, 16; x₀ = 1: row 65 -> 32.
        assert_eq!(
            lo.split(7),
            (Mask::Word(1 | (1 << 16)), Mask::Word(1 << 32))
        );
    }

    // ── Shannon build vs the oracle ────────────────────────────────

    /// Constant and pass-through columns short-circuit: zero BDD ops, the
    /// constant or the input BDD itself (or its complement), at every
    /// width including the multi-word `k = 7, 8`.
    #[test]
    fn constant_and_pass_through_columns_cost_no_ops() {
        let vs = BddVariableSet::new_anonymous(10);
        let mut rng = Rng(0xC0_57A7);
        for k in 0..=8 {
            for inputs in input_sets(k, &vs, &mut rng) {
                let mut cases = vec![
                    (column(k, |_| false), vs.mk_false()),
                    (column(k, |_| true), vs.mk_true()),
                ];
                for (j, input) in inputs.iter().enumerate() {
                    cases.push((column(k, |x| (x >> j) & 1 == 1), input.clone()));
                    cases.push((column(k, |x| (x >> j) & 1 == 0), input.not()));
                }
                for ((table, rows), expect) in cases {
                    let (got, ops) = counted_apply(&table, &inputs, &vs);
                    assert_eq!(ops, 0, "k={k}: short-circuit column issued {ops} ops");
                    assert!(got[0] == expect, "k={k}: short-circuit column value");
                    let want = apply_minterm(&rows, 1, &inputs, &vs);
                    assert_same(&got, &want, &format!("k={k} short-circuit"));
                }
            }
        }
    }

    /// Every function of ≤ 3 inputs, then random columns up to 8 inputs
    /// (`k = 7, 8` take the multi-word path), on plain-variable and
    /// composite inputs.
    #[test]
    fn synthetic_columns_match_minterm_oracle() {
        let vs = BddVariableSet::new_anonymous(10);
        let mut rng = Rng(0x5EED_0050);
        for k in 0..=3 {
            for bits in 0u64..1 << (1 << k) {
                let (table, rows) = column(k, |x| (bits >> x) & 1 == 1);
                for inputs in input_sets(k, &vs, &mut rng) {
                    let got = apply_gate(&table, &inputs, &vs);
                    let want = apply_minterm(&rows, 1, &inputs, &vs);
                    assert_same(&got, &want, &format!("k={k} column {bits:#x}"));
                }
            }
        }
        for k in 4..=8 {
            for _ in 0..4 {
                let words: Vec<u64> = (0..(1usize << k).div_ceil(64))
                    .map(|_| rng.next())
                    .collect();
                let (table, rows) = column(k, |x| (words[x / 64] >> (x % 64)) & 1 == 1);
                for inputs in input_sets(k, &vs, &mut rng) {
                    let got = apply_gate(&table, &inputs, &vs);
                    let want = apply_minterm(&rows, 1, &inputs, &vs);
                    assert_same(&got, &want, &format!("k={k} random column"));
                }
            }
        }
    }

    /// The cell family (ADR-0027 K-cells, ADR-0040 R-cells; R2 is the
    /// decoder's Toffoli, R3 its CCCNOT), both directions, matches the
    /// oracle -- and the Shannon build pins their op counts: control
    /// columns pass through (0 ops), a k-controlled swap arm is k nested
    /// `ite`s, a k-controlled NOT is k-1 `ite`s around one `xor`. (The
    /// minterm expansion spent `2^(n-1)·(n+1)` ops per balanced column.)
    #[test]
    fn cell_family_matches_minterm_oracle() {
        let vs = BddVariableSet::new_anonymous(10);
        let vars: Vec<Bdd> = vs.variables().iter().map(|&v| vs.mk_var(v)).collect();
        let mut rng = Rng(0xCE11_0050);
        let mut gates: Vec<(PrimDef, usize)> = Vec::new();
        for (kind, ops) in [
            (CellKind::K0, 0),
            (CellKind::K1, 2),
            (CellKind::K2, 4),
            (CellKind::K3, 6),
            (CellKind::R1, 1),
        ] {
            let name = format!("{kind:?}");
            gates.push((cell_prim(&name, kind.arity(), |b| kind.eval(b)), ops));
        }
        for (kind, ops) in [
            (RoleSwapKind::R1, 1),
            (RoleSwapKind::R2, 2),
            (RoleSwapKind::R3, 3),
        ] {
            let name = format!("role_{kind:?}");
            gates.push((cell_prim(&name, kind.arity(), |b| kind.eval(b)), ops));
        }
        // Target-first CNOT (`instr::cnot_tc`): the xor folds either way.
        gates.push((cell_prim("cnot_tc", 2, |b| vec![b[0] ^ b[1], b[1]]), 1));
        for (def, ops) in &gates {
            for def in [def.clone(), def.adjoint()] {
                assert_prim_matches_oracle(&def, &vs, &mut rng);
                let table = synthesize_gate(&def).unwrap();
                let (_, n) = counted_apply(&table, &vars[..table.n_in], &vs);
                assert_eq!(n, *ops, "{}: Shannon op count", def.forward_name);
            }
        }
    }

    /// Every prim one `step(rom)` / `system_step(rom)` walk applies, plus
    /// its adjoint (what `Sym` hands `eval_bdd`), matches the oracle.
    #[test]
    fn step_prims_match_minterm_oracle() {
        let defs = std::thread::Builder::new()
            .stack_size(1024 * 1024 * 1024)
            .spawn(|| {
                let rom = [crate::asm::encode(Instr::Add { rd: 0, rs: 1 }).unwrap()];
                let mut defs: Vec<PrimDef> = Vec::new();
                for iso in [crate::step::step(&rom), crate::step::system_step(&rom)] {
                    let mut stack = vec![&iso];
                    while let Some(node) = stack.pop() {
                        match node {
                            Iso::Prim(def) if !defs.iter().any(|d| d == &**def) => {
                                defs.push((**def).clone());
                                defs.push(def.adjoint());
                            }
                            Iso::Sym(f)
                            | Iso::Trace(_, f)
                            | Iso::Iter(_, f)
                            | Iso::Fold(_, f)
                            | Iso::Unfold(_, f) => stack.push(f),
                            Iso::Seq(f, g) | Iso::Plus(f, g) | Iso::Times(f, g) => {
                                stack.push(f);
                                stack.push(g);
                            }
                            _ => {}
                        }
                    }
                }
                defs
            })
            .unwrap()
            .join()
            .unwrap();
        assert!(!defs.is_empty(), "step/system_step applies no prims?");
        let vs = BddVariableSet::new_anonymous(10);
        let mut rng = Rng(0x57E9_0050);
        for def in &defs {
            assert_prim_matches_oracle(def, &vs, &mut rng);
        }
    }
}
