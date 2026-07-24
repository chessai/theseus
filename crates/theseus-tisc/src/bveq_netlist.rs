//! Symbolic netlist walk: proves `netlist(instr) ≡ instr_semantics(instr)`
//! at full Flat(8) via BDD walk of the decoder's instruction netlist.
//!
//! # Trust core (NEW — a bug here makes the netlist reconciliation vacuous)
//!
//! - `eval_bdd`: symbolic Iso interpreter (mirrors `theseus_eval::eval`)
//! - Gate BDD synthesis from `PrimDef::forward` truth tables (MAJOR-4)
//! - Extended `VarMap` (regs + s0 + BR + mem) — the ONE TCB mapping
//!
//! Defended by: walk-vs-`run_rom_on_cpu` bootstrap (T2), negative controls,
//! the convention-pinning test (T1), and the full-`system_step` authority tier.

use std::collections::HashMap;

use biodivine_lib_bdd::{Bdd, BddVariableSet};
use theseus_core::{adjoint, enumerate_values, Iso, Type};
use theseus_netlist::bits_of;

use crate::asm::Instr;
use crate::bveq::VarMap;
use crate::types::{MEM_WORDS, NUM_BUS_FIELDS, NUM_CPU_FIELDS, NUM_REGS, W};

// ── Constants: extended VarMap operand indices ──────────────────────

/// Number of operands in the extended VarMap: 8 regs + s0 + BR + 4 mem.
pub const EXTENDED_NUM_OPERANDS: usize = NUM_REGS + 1 + 1 + MEM_WORDS; // 14

/// VarMap operand index for scratch word s0 (CpuState field 12).
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
        SV::Pair(lo, hi) => {
            u16::from(sv_to_u8(lo, vs)) | (u16::from(sv_to_u8(hi, vs)) << 8)
        }
        _ => panic!("sv_to_u16: expected Pair(lo, hi)"),
    }
}

// ── Gate truth table synthesis (MAJOR-4) ───────────────────────────

/// Pre-computed truth table for a gate prim, ready for BDD application.
struct GateTable {
    /// (input_bits, output_bits) for each row of the truth table.
    rows: Vec<(Vec<bool>, Vec<bool>)>,
    /// Number of input bits.
    n_in: usize,
    /// Number of output bits.
    n_out: usize,
    /// Output type (for rebuilding SV structure).
    out_ty: Type,
}

/// Synthesize a gate's truth table from its `PrimDef::forward` map.
///
/// Uses `bits_of` on the admission-checked `forward` map — the identical
/// table `theseus_eval::eval` runs (MAJOR-4: no hand-written gate BDDs).
fn synthesize_gate(def: &theseus_core::PrimDef) -> Result<GateTable, NetlistWalkError> {
    let in_vals = enumerate_values(&def.input_ty);
    let mut rows = Vec::with_capacity(in_vals.len());
    let mut n_in = 0;
    let mut n_out = 0;
    for v in &in_vals {
        let in_bits = bits_of(v, &def.input_ty)
            .map_err(|e| NetlistWalkError::GateError(format!("bits_of input: {e:?}")))?;
        let out_v = def.forward.get(v).ok_or_else(|| {
            NetlistWalkError::GateError(format!(
                "prim '{}': input not in forward map: {v:?}",
                def.forward_name
            ))
        })?;
        let out_bits = bits_of(out_v, &def.output_ty)
            .map_err(|e| NetlistWalkError::GateError(format!("bits_of output: {e:?}")))?;
        n_in = in_bits.len();
        n_out = out_bits.len();
        rows.push((in_bits, out_bits));
    }
    Ok(GateTable {
        rows,
        n_in,
        n_out,
        out_ty: def.output_ty.clone(),
    })
}

/// Apply a gate's truth table to BDD inputs via minterm expansion.
///
/// For each output bit j, OR together the minterms (AND of input literals)
/// for all truth-table rows where output bit j is 1.
fn apply_gate(table: &GateTable, input_bdds: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let mut outputs = Vec::with_capacity(table.n_out);
    for j in 0..table.n_out {
        let mut out_bdd = vs.mk_false();
        for (in_bits, out_bits) in &table.rows {
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
            _ => Err(NetlistWalkError::DomainError(
                "Times: expected Pair".into(),
            )),
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

        // SwapSum(One, One) = bit NOT (structural case, not a prim — MAJOR-4).
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
    // Field 14: s0 — symbolic or pinned to 0 for Addi/Subi
    // (unpredicated immediate mask requires s0=0)
    if pin_s0_zero {
        fields.push(sv_word8_const(0, vs));
    } else {
        fields.push(sv_word8_symbolic(map, OP_S0));
    }
    // Fields 13-19: s1-s7 = 0
    for _ in 1..crate::types::NUM_SCRATCH {
        fields.push(sv_word8_const(0, vs));
    }

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

    // CpuState (NUM_CPU_FIELDS = 20 fields)
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
pub fn build_concrete_system_sv(
    s: &crate::types::StateVals,
    vs: &BddVariableSet,
) -> SV {
    let mut cpu_fields = Vec::with_capacity(NUM_CPU_FIELDS);
    for &r in &s.regs {
        cpu_fields.push(sv_word8_const(r, vs));
    }
    cpu_fields.push(sv_word8_const(s.pc, vs));
    cpu_fields.push(sv_word8_const(s.br, vs));
    cpu_fields.push(sv_word8_const(s.ir as u8, vs));
    cpu_fields.push(sv_word8_const((s.ir >> 8) as u8, vs));
    for &sc in &s.scratch {
        cpu_fields.push(sv_word8_const(sc, vs));
    }
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
