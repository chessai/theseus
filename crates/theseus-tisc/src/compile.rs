//! M-A/M-B surface→TISC-1 ROM compiler (ADR-0042 + ADR-0043).
//!
//! Compiles the product/composition fragment of core `Iso` — plus the
//! controlled-op idiom (general arms), data-dependent `Iter`, and bare
//! sum operations — to TISC-1 instruction sequences. Values fit in
//! registers (≤ 8 word-sized leaves); no memory bus, no recursion.
//!
//! M-B extends M-A with: (1) general controlled ops (multi-instruction
//! arms in the Distrib;Plus;Factor idiom), (2) data-dependent iter
//! (guarded static unroll), (3) bare sums (tag+payload representation).
//!
//! The acceptance test is the run-on-CPU differential oracle (ADR-0042 §6,
//! ADR-0043 §5): for every input value, the compiled ROM executed on the
//! simulated CPU must agree with `theseus_eval::eval`.

use std::fmt;

use theseus_core::typing::is_numeral_ty;
use theseus_core::{adjoint, Iso, Type, Value};

use crate::asm::{assemble, Instr};
use crate::types::{StateVals, NUM_REGS};

// ── Errors ────────────────────────────────────────────────────────────

/// Compile-time errors for the M-A/M-B compiler.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompileError {
    /// The program requires more registers than the TISC-1 has (8).
    TooManyLeaves { needed: usize, max: usize },
    /// A sum-type constructor outside the controlled-op idiom.
    UnsupportedSum(String),
    /// A constructor deferred to a later milestone.
    Deferred(String),
    /// A `Prim` whose clause table doesn't match any recognized pattern.
    UnrecognizedPrim(String),
    /// Data-dependent `Iter` (count varies per input).
    DataDependentIter,
    /// The value type contains `Type::Zero` (empty, no inhabitants).
    EmptyType,
    /// Instruction would alias registers (rd == rs).
    AliasedRegisters { instr: String },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::TooManyLeaves { needed, max } => {
                write!(f, "program needs {needed} registers but TISC-1 has {max}")
            }
            CompileError::UnsupportedSum(msg) => write!(f, "unsupported sum: {msg}"),
            CompileError::Deferred(msg) => write!(f, "deferred to later milestone: {msg}"),
            CompileError::UnrecognizedPrim(msg) => write!(f, "unrecognized prim: {msg}"),
            CompileError::DataDependentIter => {
                write!(f, "data-dependent iter count (deferred to M-B)")
            }
            CompileError::EmptyType => write!(f, "empty type (Type::Zero)"),
            CompileError::AliasedRegisters { instr } => {
                write!(f, "aliased registers in {instr}")
            }
        }
    }
}

// ── Type helpers ──────────────────────────────────────────────────────

/// A "leaf" type is a type that occupies exactly one register (u8).
/// For M-A these are: Bit = 1+1, bounded Nat = 1+(1+(…+0)), and any
/// sum-chain type whose values fit in a byte.
/// Count the number of word-sized leaf fields in a type.
///
/// M-B tag+payload representation (ADR-0043 §4):
///   Zero → 0, One → 0, Sum(a,b) → 1 + max(|a|,|b|), Prod(a,b) → |a|+|b|.
/// The 1 is the tag register (0=Left, 1=Right); the payload shares
/// max(|a|,|b|) registers (narrower arm zero-extends).
fn num_leaves(ty: &Type) -> Result<usize, CompileError> {
    match ty {
        Type::Zero => Ok(0),
        Type::One => Ok(0),
        Type::Sum(a, b) => {
            let la = num_leaves(a)?;
            let lb = num_leaves(b)?;
            Ok(1 + la.max(lb))
        }
        Type::Prod(a, b) => {
            let la = num_leaves(a)?;
            let lb = num_leaves(b)?;
            Ok(la + lb)
        }
    }
}

/// Collect the leaf types in left-to-right order.
/// For Sum(a,b): tag (Bit) followed by max(|a|,|b|) payload leaves.
#[allow(dead_code)]
fn leaf_types(ty: &Type) -> Vec<Type> {
    match ty {
        Type::Zero | Type::One => vec![],
        Type::Sum(a, b) => {
            let la = num_leaves(a).unwrap_or(0);
            let lb = num_leaves(b).unwrap_or(0);
            let max_payload = la.max(lb);
            let bit = Type::sum(Type::One, Type::One);
            let mut types = vec![bit.clone()]; // tag register
            for _ in 0..max_payload {
                types.push(bit.clone()); // payload registers
            }
            types
        }
        Type::Prod(a, b) => {
            let mut v = leaf_types(a);
            v.extend(leaf_types(b));
            v
        }
    }
}

/// Encode a value inhabiting a sum-chain leaf type as a u8.
/// Counts the number of `Right` wrappers before reaching a terminal.
/// For `1 + (1 + (... + 0))`: Left(Unit) = 0, Right(Left(Unit)) = 1, etc.
/// For `Bit = 1 + 1`: Left(Unit) = 0, Right(Unit) = 1.
fn leaf_value_to_u8(v: &Value) -> u8 {
    let mut count = 0u8;
    let mut cur = v;
    loop {
        match cur {
            Value::Left(_) => return count,
            Value::Right(inner) => {
                count += 1;
                cur = inner;
            }
            // Terminal: reached a non-Left/Right value (e.g. Unit in Bit = 1+1).
            _ => return count,
        }
    }
}

/// Decode a u8 back to a value inhabiting a sum-chain leaf type.
/// Uses `enumerate_values` to get the canonical value ordering, then
/// indexes by k. This handles Bit, Nat, and any sum-chain type correctly.
#[allow(dead_code)]
fn u8_to_leaf_value(k: u8, ty: &Type) -> Value {
    let vals = enumerate_values(ty);
    vals.into_iter()
        .nth(k as usize)
        .unwrap_or_else(|| panic!("u8_to_leaf_value: k={k} exceeds type cardinality for {ty}"))
}

/// Reconstruct a Value from flat leaf bytes + the type structure.
///
/// M-B tag+payload encoding for Sum(a,b): byte[0] = tag (0=Left, 1=Right),
/// bytes[1..1+max(|a|,|b|)] = payload (left or right, zero-padded).
fn value_from_leaf_bytes(bytes: &[u8], ty: &Type) -> (Value, usize) {
    match ty {
        Type::Zero => panic!("value_from_leaf_bytes: Zero type"),
        Type::One => (Value::Unit, 0),
        Type::Sum(a, b) => {
            let la = num_leaves(a).unwrap_or(0);
            let lb = num_leaves(b).unwrap_or(0);
            let max_payload = la.max(lb);
            let tag = bytes[0];
            if tag == 0 {
                let (v, _) = value_from_leaf_bytes(&bytes[1..], a);
                (Value::Left(Box::new(v)), 1 + max_payload)
            } else {
                let (v, _) = value_from_leaf_bytes(&bytes[1..], b);
                (Value::Right(Box::new(v)), 1 + max_payload)
            }
        }
        Type::Prod(a, b) => {
            let (va, na) = value_from_leaf_bytes(bytes, a);
            let (vb, nb) = value_from_leaf_bytes(&bytes[na..], b);
            (Value::pair(va, vb), na + nb)
        }
    }
}

/// Flatten a Value into leaf bytes.
///
/// M-B tag+payload encoding for Sum(a,b): [tag, payload..., zero-pad...].
fn value_to_leaf_bytes(v: &Value, ty: &Type) -> Vec<u8> {
    match ty {
        Type::Zero => panic!("value_to_leaf_bytes: Zero type"),
        Type::One => vec![],
        Type::Sum(a, b) => {
            let la = num_leaves(a).unwrap_or(0);
            let lb = num_leaves(b).unwrap_or(0);
            let max_payload = la.max(lb);
            let total = 1 + max_payload;
            match v {
                Value::Left(inner) => {
                    let mut bytes = vec![0u8]; // tag = 0 (Left)
                    bytes.extend(value_to_leaf_bytes(inner, a));
                    bytes.resize(total, 0); // zero-pad
                    bytes
                }
                Value::Right(inner) => {
                    let mut bytes = vec![1u8]; // tag = 1 (Right)
                    bytes.extend(value_to_leaf_bytes(inner, b));
                    bytes.resize(total, 0); // zero-pad
                    bytes
                }
                _ => panic!("value_to_leaf_bytes: expected Left/Right for Sum type, got {v:?}"),
            }
        }
        Type::Prod(a, b) => {
            let Value::Pair(va, vb) = v else {
                panic!("value_to_leaf_bytes: expected Pair for Prod type, got {v:?}");
            };
            let mut bytes = value_to_leaf_bytes(va, a);
            bytes.extend(value_to_leaf_bytes(vb, b));
            bytes
        }
    }
}

// ── Register allocation context ──────────────────────────────────────

/// Maps leaf positions to physical register indices.
#[derive(Clone, Debug)]
struct Ctx {
    /// `mapping[i]` = physical register index for leaf `i`.
    mapping: Vec<u8>,
}

impl Ctx {
    /// Initial context: leaf i → reg i.
    fn new(n_leaves: usize) -> Self {
        Ctx {
            mapping: (0..n_leaves as u8).collect(),
        }
    }

    /// Split into two sub-contexts for `Times(f, g)`.
    fn split(&self, n_left: usize) -> (Ctx, Ctx) {
        let left = Ctx {
            mapping: self.mapping[..n_left].to_vec(),
        };
        let right = Ctx {
            mapping: self.mapping[n_left..].to_vec(),
        };
        (left, right)
    }

    /// Merge two sub-contexts back.
    fn merge(left: &Ctx, right: &Ctx) -> Ctx {
        let mut mapping = left.mapping.clone();
        mapping.extend_from_slice(&right.mapping);
        Ctx { mapping }
    }
}

// ── Auxiliary register allocation ────────────────────────────────────

/// Allocate `count` free physical registers not used by any leaf in `ctx`.
fn allocate_aux(ctx: &Ctx, count: usize) -> Result<Vec<u8>, CompileError> {
    let used: std::collections::BTreeSet<u8> = ctx.mapping.iter().copied().collect();
    let free: Vec<u8> = (0..NUM_REGS as u8).filter(|r| !used.contains(r)).collect();
    if free.len() < count {
        return Err(CompileError::TooManyLeaves {
            needed: ctx.mapping.len() + count,
            max: NUM_REGS,
        });
    }
    Ok(free[..count].to_vec())
}

// ── SWAPBR skip-block primitive (ADR-0043 §2) ──────────────────────
//
// Emit a "skip block" that conditionally skips `body_instrs` based on
// a control register `ctrl_reg`:
//
// When `skip_when_set = true`: skip body iff ctrl_reg bit 0 = 1.
// When `skip_when_set = false`: skip body iff ctrl_reg bit 0 = 0.
//
// Uses two auxiliary registers: `rb` (for SWAPBR offset) and `temp`
// (for CSWAP routing). Both must be 0 on entry and are 0 on exit.

fn emit_skip_block(
    ctrl_reg: u8,
    body_instrs: &[Instr],
    rb: u8,
    temp: u8,
    skip_when_set: bool,
) -> Vec<Instr> {
    let body_len = body_instrs.len() as u8;
    if body_len == 0 {
        return vec![];
    }
    let mut out = Vec::new();

    if skip_when_set {
        // Skip iff ctrl=1: load skip_dist into rb iff ctrl=1.
        // Start with temp=skip_dist, rb=0. CSWAP(ctrl, temp, rb):
        //   ctrl=1 → swap: temp=0, rb=skip_dist (jump, skip body)
        //   ctrl=0 → no swap: temp=skip_dist, rb=0 (fall through)
        out.push(Instr::Addi { rd: temp, imm: body_len });
        out.push(Instr::Cswap { rd: ctrl_reg, rs: temp, rt: rb });
    } else {
        // Skip iff ctrl=0: load skip_dist into rb iff ctrl=0.
        // Start with rb=skip_dist, temp=0. CSWAP(ctrl, rb, temp):
        //   ctrl=1 → swap: rb=0, temp=skip_dist (don't skip = run body)
        //   ctrl=0 → no swap: rb=skip_dist (skip body)
        out.push(Instr::Addi { rd: rb, imm: body_len });
        out.push(Instr::Cswap { rd: ctrl_reg, rs: rb, rt: temp });
    }

    // Branch: SWAPBR rb
    out.push(Instr::Swapbr { rd: rb });

    // Body
    out.extend_from_slice(body_instrs);

    // Come-from: SWAPBR rb (clears BR, captures offset into rb)
    out.push(Instr::Swapbr { rd: rb });

    // Cleanup: reverse the CSWAP + SUBI
    if skip_when_set {
        out.push(Instr::Cswap { rd: ctrl_reg, rs: temp, rt: rb });
        out.push(Instr::Subi { rd: temp, imm: body_len });
    } else {
        out.push(Instr::Cswap { rd: ctrl_reg, rs: rb, rt: temp });
        out.push(Instr::Subi { rd: rb, imm: body_len });
    }

    out
}

// ── Instruction inversion (ADR-0042 §4.1 table) ──────────────────────

#[allow(dead_code)]
fn invert_instr(instr: Instr) -> Instr {
    match instr {
        Instr::Add { rd, rs } => Instr::Sub { rd, rs },
        Instr::Sub { rd, rs } => Instr::Add { rd, rs },
        Instr::Xor { rd, rs } => Instr::Xor { rd, rs },
        Instr::Exch { rd, rs } => Instr::Exch { rd, rs },
        Instr::Rotl { rd, rs } => Instr::Rotr { rd, rs },
        Instr::Rotr { rd, rs } => Instr::Rotl { rd, rs },
        Instr::Cswap { rd, rs, rt } => Instr::Cswap { rd, rs, rt },
        Instr::Xchg { rd, sx } => Instr::Xchg { rd, sx },
        Instr::Swapbr { rd } => Instr::Swapbr { rd },
        Instr::Addi { rd, imm } => Instr::Subi { rd, imm },
        Instr::Subi { rd, imm } => Instr::Addi { rd, imm },
        Instr::Xori { rd, imm } => Instr::Xori { rd, imm },
        Instr::Rotli { rd, imm } => Instr::Rotri { rd, imm },
        Instr::Rotri { rd, imm } => Instr::Rotli { rd, imm },
    }
}

// ── Prim recognition ─────────────────────────────────────────────────

use theseus_core::prim::{enumerate_values, PrimDef};

/// Try to recognize a PrimDef as a known TISC-1 instruction pattern.
fn recognize_prim(def: &PrimDef, ctx: &Ctx) -> Result<Vec<Instr>, CompileError> {
    let n_in = num_leaves(&def.input_ty)?;
    let n_out = num_leaves(&def.output_ty)?;

    // Two-register prims: (a, b) ↦ (a', b')
    if n_in == 2 && n_out == 2 {
        let rd = ctx.mapping[0]; // first leaf
        let rs = ctx.mapping[1]; // second leaf

        // Check each known two-register pattern by evaluating the clause table.
        let in_ty = &def.input_ty;
        let vals = enumerate_values(in_ty);

        // addInto: (a, b) ↦ (a, a + b mod 256)
        // The source (first) is preserved, target (second) gets src + tgt.
        if check_pattern_2reg(&def.forward, &vals, |a, b| (a, a.wrapping_add(b))) {
            return Ok(vec![Instr::Add { rd: rs, rs: rd }]);
        }

        // subFrom: (a, b) ↦ (a, b - a mod 256)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (a, b.wrapping_sub(a))) {
            return Ok(vec![Instr::Sub { rd: rs, rs: rd }]);
        }

        // xorInto: (a, b) ↦ (a, a ^ b)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (a, a ^ b)) {
            return Ok(vec![Instr::Xor { rd: rs, rs: rd }]);
        }

        // swap: (a, b) ↦ (b, a)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (b, a)) {
            if rd == rs {
                return Err(CompileError::AliasedRegisters {
                    instr: "EXCH".into(),
                });
            }
            return Ok(vec![Instr::Exch { rd, rs }]);
        }

        // rotl: (a, b) ↦ (a, rotl(b, a & 7))
        if check_pattern_2reg(&def.forward, &vals, |a, b| {
            (a, b.rotate_left((a & 7) as u32))
        }) {
            return Ok(vec![Instr::Rotl { rd: rs, rs: rd }]);
        }

        // rotr: (a, b) ↦ (a, rotr(b, a & 7))
        if check_pattern_2reg(&def.forward, &vals, |a, b| {
            (a, b.rotate_right((a & 7) as u32))
        }) {
            return Ok(vec![Instr::Rotr { rd: rs, rs: rd }]);
        }
    }

    // Three-register prims: (c, a, b)
    if n_in == 3 && n_out == 3 {
        let rc = ctx.mapping[0];
        let ra = ctx.mapping[1];
        let rb = ctx.mapping[2];

        let vals = enumerate_values(&def.input_ty);

        // cswap: (c, a, b) ↦ (c, if c&1 then (b,a) else (a,b))
        if check_pattern_3reg(&def.forward, &vals, |c, a, b| {
            if c & 1 == 1 {
                (c, b, a)
            } else {
                (c, a, b)
            }
        }) {
            return Ok(vec![Instr::Cswap {
                rd: rc,
                rs: ra,
                rt: rb,
            }]);
        }
    }

    // One-register prims: a ↦ f(a)
    if n_in == 1 && n_out == 1 {
        let rd = ctx.mapping[0];
        let vals = enumerate_values(&def.input_ty);

        // addi_K: a ↦ a + K
        // Try to find a constant K that works.
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a.wrapping_add(k)) {
            return Ok(vec![Instr::Addi { rd, imm: k }]);
        }

        // subi_K: a ↦ a - K
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a.wrapping_sub(k)) {
            return Ok(vec![Instr::Subi { rd, imm: k }]);
        }

        // xori_K: a ↦ a ^ K
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a ^ k) {
            return Ok(vec![Instr::Xori { rd, imm: k }]);
        }
    }

    Err(CompileError::UnrecognizedPrim(format!(
        "prim '{}' not recognized as a TISC-1 pattern",
        def.forward_name
    )))
}

/// Check if a 2-register prim's clause table matches a pattern.
fn check_pattern_2reg(
    forward: &std::collections::BTreeMap<Value, Value>,
    vals: &[Value],
    f: impl Fn(u8, u8) -> (u8, u8),
) -> bool {
    for v in vals {
        let Value::Pair(a, b) = v else { return false };
        let a_byte = leaf_value_to_u8(a);
        let b_byte = leaf_value_to_u8(b);
        let (exp_a, exp_b) = f(a_byte, b_byte);
        let Some(out) = forward.get(v) else {
            return false;
        };
        let Value::Pair(oa, ob) = out else {
            return false;
        };
        if leaf_value_to_u8(oa) != exp_a || leaf_value_to_u8(ob) != exp_b {
            return false;
        }
    }
    true
}

/// Check if a 3-register prim's clause table matches a pattern.
fn check_pattern_3reg(
    forward: &std::collections::BTreeMap<Value, Value>,
    vals: &[Value],
    f: impl Fn(u8, u8, u8) -> (u8, u8, u8),
) -> bool {
    for v in vals {
        let Value::Pair(c, ab) = v else { return false };
        let Value::Pair(a, b) = ab.as_ref() else {
            return false;
        };
        let cb = leaf_value_to_u8(c);
        let ab_val = leaf_value_to_u8(a);
        let bb = leaf_value_to_u8(b);
        let (ec, ea, eb) = f(cb, ab_val, bb);
        let Some(out) = forward.get(v) else {
            return false;
        };
        let Value::Pair(oc, oab) = out else {
            return false;
        };
        let Value::Pair(oa, ob) = oab.as_ref() else {
            return false;
        };
        if leaf_value_to_u8(oc) != ec || leaf_value_to_u8(oa) != ea || leaf_value_to_u8(ob) != eb {
            return false;
        }
    }
    true
}

/// Find a constant K such that a 1-register prim matches a ↦ f(a, K).
fn find_constant_1reg(
    forward: &std::collections::BTreeMap<Value, Value>,
    vals: &[Value],
    f: impl Fn(u8, u8) -> u8,
) -> Option<u8> {
    if vals.is_empty() {
        return None;
    }
    // Derive K from the first entry.
    let first_in = leaf_value_to_u8(&vals[0]);
    let first_out = leaf_value_to_u8(forward.get(&vals[0])?);
    // Try all possible K values (brute force over u8).
    'outer: for k in 0..=255u8 {
        if f(first_in, k) != first_out {
            continue;
        }
        for v in vals {
            let a = leaf_value_to_u8(v);
            let expected = f(a, k);
            let Some(out) = forward.get(v) else {
                continue 'outer;
            };
            if leaf_value_to_u8(out) != expected {
                continue 'outer;
            }
        }
        return Some(k);
    }
    None
}

// ── Controlled-op idiom recognition ──────────────────────────────────

/// Try to match the controlled-op idiom shape:
///   Seq(SwapProd(c,t), Seq(Distrib(t,cl,cr), Seq(Plus(f,g), Seq(Factor(t,cl,cr), SwapProd(t,c)))))
///
/// Returns Some((ctrl_ty, tgt_ty, ctrl_l, ctrl_r, f, g)) if matched.
fn match_controlled_idiom(iso: &Iso) -> Option<(&Type, &Type, &Type, &Type, &Iso, &Iso)> {
    // Seq(swap1, Seq(distrib, Seq(Plus(f,g), Seq(factor, swap2))))
    let Iso::Seq(swap1, rest1) = iso else {
        return None;
    };
    let Iso::SwapProd(c1, t1) = swap1.as_ref() else {
        return None;
    };
    let Iso::Seq(distrib, rest2) = rest1.as_ref() else {
        return None;
    };
    let Iso::Distrib(dt, dcl, dcr) = distrib.as_ref() else {
        return None;
    };
    if dt != t1 {
        return None;
    }
    let Iso::Seq(plus, rest3) = rest2.as_ref() else {
        return None;
    };
    let Iso::Plus(f, g) = plus.as_ref() else {
        return None;
    };
    let Iso::Seq(factor, swap2) = rest3.as_ref() else {
        return None;
    };
    let Iso::Factor(ft, fcl, fcr) = factor.as_ref() else {
        return None;
    };
    if ft != dt || fcl != dcl || fcr != dcr {
        return None;
    }
    let Iso::SwapProd(s2t, s2c) = swap2.as_ref() else {
        return None;
    };
    if s2t != t1 || s2c != c1 {
        return None;
    }
    // Verify c1 = Sum(dcl, dcr)
    let expected_ctrl = Type::sum(dcl.clone(), dcr.clone());
    if *c1 != expected_ctrl {
        return None;
    }
    Some((c1, t1, dcl, dcr, f.as_ref(), g.as_ref()))
}

/// Check if an iso is SwapSum(One, One) — i.e. NOT on Bit.
fn is_bit_not(iso: &Iso) -> bool {
    matches!(iso, Iso::SwapSum(a, b) if *a == Type::One && *b == Type::One)
}

/// Lower a recognized controlled-op idiom to TISC-1 instructions.
/// The `outer_ctx` covers [ctrl_leaves..., tgt_leaves...] (the input layout
/// before the first SwapProd).
fn lower_controlled_idiom(
    ctrl_ty: &Type,
    tgt_ty: &Type,
    ctrl_l: &Type,
    ctrl_r: &Type,
    f: &Iso,
    g: &Iso,
    ctx: &Ctx,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    let n_ctrl = num_leaves(ctrl_ty)?;
    let n_tgt = num_leaves(tgt_ty)?;

    // The `f` branch: tgt × ctrl_l. It should be identity.
    // The `g` branch: tgt × ctrl_r. This determines the instruction.

    // For M-A, we only handle ctrl = Bit = Sum(One, One),
    // so ctrl_l = ctrl_r = One (zero leaves each), and n_ctrl = 1.
    if *ctrl_l != Type::One || *ctrl_r != Type::One {
        return Err(CompileError::Deferred(
            "controlled-op with non-Bit control type".into(),
        ));
    }

    // f operates on tgt × One. Check it's identity.
    if !is_structural_identity(f) {
        return Err(CompileError::Deferred(format!(
            "controlled-op left branch is not identity: {f:?}"
        )));
    }

    // g operates on tgt × One. Check what it does to tgt.
    // Recognized patterns:
    //   1. NOT on a single-bit target: g = Times(SwapSum(1,1), Id(1))
    //      or g = Seq(Plus(SwapSum,SwapSum), SwapSum) on Bit×One
    //      → XOR instruction
    //   2. SWAP on a two-leaf target: g = Times(SwapProd(a,b), Id(1))
    //      → CSWAP instruction

    // The context is [ctrl_regs..., tgt_regs...] (original layout).
    // The ctrl reg is ctx.mapping[0..n_ctrl] and tgt regs are ctx.mapping[n_ctrl..].
    let ctrl_reg = ctx.mapping[0]; // n_ctrl == 1 for Bit control

    // Try to recognize g as controlled-NOT (target is single Bit).
    if n_tgt == 1 {
        let tgt_reg = ctx.mapping[n_ctrl];
        // g should be NOT-on-target × Id(One)
        if is_not_on_target_times_id(g, tgt_ty) {
            return Ok((vec![Instr::Xor { rd: tgt_reg, rs: ctrl_reg }], ctx.clone()));
        }
    }

    // Try to recognize g as controlled-SWAP (target is two leaves).
    if n_tgt == 2 {
        let tgt_r0 = ctx.mapping[n_ctrl];
        let tgt_r1 = ctx.mapping[n_ctrl + 1];
        if is_swap_on_target_times_id(g, tgt_ty) {
            return Ok((
                vec![Instr::Cswap {
                    rd: ctrl_reg,
                    rs: tgt_r0,
                    rt: tgt_r1,
                }],
                ctx.clone(),
            ));
        }
    }

    // M-B fallback: general controlled-op with multi-instruction arms.
    // Compile g's body (on tgt × ctrl_r = tgt × One) and use skip-block.
    // Since ctrl_r = One (0 leaves), g operates on just the target registers.
    let ctx_tgt = Ctx {
        mapping: ctx.mapping[n_ctrl..n_ctrl + n_tgt].to_vec(),
    };
    let (instrs_g, _) = lower(g, &ctx_tgt)?;
    if instrs_g.is_empty() && is_structural_identity(f) {
        // Both arms are identity → no-op.
        return Ok((vec![], ctx.clone()));
    }

    // f must be identity for now (general f+g handled via bare Plus).
    if !is_structural_identity(f) {
        // Compile f as well.
        let (instrs_f, _) = lower(f, &ctx_tgt)?;
        let aux = allocate_aux(ctx, 2)?;
        let rb = aux[0];
        let temp = aux[1];
        let mut instrs = Vec::new();
        // Phase 1: f runs iff ctrl=0 → skip f iff ctrl=1
        if !instrs_f.is_empty() {
            instrs.extend(emit_skip_block(ctrl_reg, &instrs_f, rb, temp, true));
        }
        // Phase 2: g runs iff ctrl=1 → skip g iff ctrl=0
        if !instrs_g.is_empty() {
            instrs.extend(emit_skip_block(ctrl_reg, &instrs_g, rb, temp, false));
        }
        return Ok((instrs, ctx.clone()));
    }

    // f = identity, g = general multi-instruction sequence.
    // Skip g iff ctrl=0 (Left branch = identity).
    let aux = allocate_aux(ctx, 2)?;
    let rb = aux[0];
    let temp = aux[1];
    let instrs = emit_skip_block(ctrl_reg, &instrs_g, rb, temp, false);
    Ok((instrs, ctx.clone()))
}

/// Check if iso is structurally the identity (possibly wrapped).
fn is_structural_identity(iso: &Iso) -> bool {
    match iso {
        Iso::Id(_) => true,
        Iso::Times(f, g) => is_structural_identity(f) && is_structural_identity(g),
        Iso::Seq(f, g) => is_structural_identity(f) && is_structural_identity(g),
        _ => false,
    }
}

/// Check if g = Times(SwapSum(1,1), Id(1)) — NOT on target with Id on ctrl_half.
/// Also handles the form Seq(Plus(f, g), SwapSum(a, b)) which synthesize
/// produces for a sum-swapping bijection.
fn is_not_on_target_times_id(g: &Iso, _tgt_ty: &Type) -> bool {
    // Direct form: Times(SwapSum(1,1), Id(1))
    if let Iso::Times(f, id) = g {
        if is_bit_not(f) && is_structural_identity(id) {
            return true;
        }
    }
    // synthesize may produce: Seq(Plus(Id(1), Id(1)), SwapSum(1,1)) for
    // a complete tag-swap on Bit (which is NOT). The Plus(Id,Id) is identity
    // on payloads, then SwapSum flips the tags.
    if let Iso::Seq(plus_part, swap_part) = g {
        if let Iso::SwapSum(a, b) = swap_part.as_ref() {
            if *a == Type::One && *b == Type::One {
                // The Plus part should be Plus(id_on_One, id_on_One)
                if is_structural_identity(plus_part) {
                    return true;
                }
                // Or Plus(f, g) where both f and g are identity
                if let Iso::Plus(pf, pg) = plus_part.as_ref() {
                    if is_structural_identity(pf) && is_structural_identity(pg) {
                        return true;
                    }
                }
            }
        }
    }
    // synthesize for NOT on Bit×One produces Times(NOT_on_Bit, Id(One))
    // where NOT_on_Bit = Seq(Plus(Id(1),Id(1)), SwapSum(1,1)).
    if let Iso::Times(f, id) = g {
        if is_structural_identity(id) {
            // Check if f is NOT on Bit (possibly via Seq(Plus(Id,Id), SwapSum))
            if is_not_iso(f) {
                return true;
            }
        }
    }
    false
}

/// Check if an iso is NOT on Bit (SwapSum(1,1) or Seq(Plus(Id,Id),SwapSum(1,1))).
fn is_not_iso(iso: &Iso) -> bool {
    if is_bit_not(iso) {
        return true;
    }
    if let Iso::Seq(plus_part, swap_part) = iso {
        if let Iso::SwapSum(a, b) = swap_part.as_ref() {
            if *a == Type::One && *b == Type::One {
                if let Iso::Plus(pf, pg) = plus_part.as_ref() {
                    if is_structural_identity(pf) && is_structural_identity(pg) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

/// Check if g = Times(SwapProd(a,b), Id(1)) — SWAP on target with Id on ctrl_half.
fn is_swap_on_target_times_id(g: &Iso, _tgt_ty: &Type) -> bool {
    if let Iso::Times(f, id) = g {
        if let Iso::SwapProd(_, _) = f.as_ref() {
            if is_structural_identity(id) {
                return true;
            }
        }
    }
    false
}

// ── Numeral helpers ──────────────────────────────────────────────────

/// Count the number of inhabitants of a numeral type.
fn numeral_count(ty: &Type) -> Option<usize> {
    if !is_numeral_ty(ty) {
        return None;
    }
    let mut count = 0usize;
    let mut cur = ty;
    loop {
        match cur {
            Type::Zero => return Some(count),
            Type::Sum(_, r) => {
                count += 1;
                cur = r;
            }
            _ => return None,
        }
    }
}

// ── Core lowering ────────────────────────────────────────────────────

/// Compile a core `Iso` (M-A/M-B) to a TISC-1 instruction sequence.
///
/// Returns `Err(CompileError)` for out-of-scope constructs (Trace/Fold/Unfold).
pub fn compile(iso: &Iso) -> Result<Vec<Instr>, CompileError> {
    let (in_ty, _out_ty) = theseus_core::type_of(iso).map_err(|e| {
        CompileError::Deferred(format!("type error: {e:?}"))
    })?;
    let n = num_leaves(&in_ty)?;
    if n > NUM_REGS {
        return Err(CompileError::TooManyLeaves {
            needed: n,
            max: NUM_REGS,
        });
    }
    let ctx = Ctx::new(n);
    let (instrs, _final_ctx) = lower(iso, &ctx)?;
    Ok(instrs)
}

/// Recursive lowering: iso + register context → (instructions, updated context).
fn lower(iso: &Iso, ctx: &Ctx) -> Result<(Vec<Instr>, Ctx), CompileError> {
    // First, try matching the controlled-op idiom (before decomposing Seq).
    if let Some((ctrl_ty, tgt_ty, ctrl_l, ctrl_r, f, g)) = match_controlled_idiom(iso) {
        return lower_controlled_idiom(ctrl_ty, tgt_ty, ctrl_l, ctrl_r, f, g, ctx);
    }

    match iso {
        Iso::Id(_) => Ok((vec![], ctx.clone())),

        Iso::Seq(f, g) => {
            let (mut instrs_f, ctx2) = lower(f, ctx)?;
            let (instrs_g, ctx3) = lower(g, &ctx2)?;
            instrs_f.extend(instrs_g);
            Ok((instrs_f, ctx3))
        }

        Iso::Times(f, g) => {
            let (in_f, _) = theseus_core::type_of(f).map_err(|e| {
                CompileError::Deferred(format!("type error in Times/f: {e:?}"))
            })?;
            let n_left = num_leaves(&in_f)?;
            let (ctx_f, ctx_g) = ctx.split(n_left);
            let (mut instrs_f, ctx_f2) = lower(f, &ctx_f)?;
            let (instrs_g, ctx_g2) = lower(g, &ctx_g)?;
            instrs_f.extend(instrs_g);
            Ok((instrs_f, Ctx::merge(&ctx_f2, &ctx_g2)))
        }

        Iso::SwapProd(a, b) => {
            let n_a = num_leaves(a)?;
            let n_b = num_leaves(b)?;
            let mut instrs = Vec::new();
            // Emit EXCH instructions to physically swap the register
            // contents. After the EXCHs, the data has moved into the
            // standard positions, so the context mapping does NOT change.
            if n_a == n_b {
                // Equal-sized sides: pairwise EXCH.
                for i in 0..n_a {
                    let rd = ctx.mapping[i];
                    let rs = ctx.mapping[n_a + i];
                    if rd != rs {
                        instrs.push(Instr::Exch { rd, rs });
                    }
                }
            } else {
                // Unequal sizes: permute via transpositions.
                // Target permutation: positions [0..n_b) get values from
                // [n_a..n_a+n_b), positions [n_b..n_a+n_b) get values from [0..n_a).
                let total = n_a + n_b;
                let target: Vec<usize> = (n_a..total).chain(0..n_a).collect();
                let mut perm: Vec<usize> = (0..total).collect();
                for i in 0..total {
                    if perm[i] != target[i] {
                        let j = perm.iter().position(|&x| x == target[i]).unwrap();
                        let rd = ctx.mapping[i];
                        let rs = ctx.mapping[j];
                        if rd != rs {
                            instrs.push(Instr::Exch { rd, rs });
                        }
                        perm.swap(i, j);
                    }
                }
            }
            Ok((instrs, ctx.clone()))
        }

        Iso::AssocLProd(_, _, _)
        | Iso::AssocRProd(_, _, _)
        | Iso::UniteProd(_)
        | Iso::UnitiProd(_) => {
            // Zero instructions — register mapping unchanged.
            Ok((vec![], ctx.clone()))
        }

        Iso::Sym(f) => {
            // Adjoint: lower the original, then reverse + invert.
            // We use the core adjoint to get f†, then lower that directly.
            let adj = adjoint(f);
            lower(&adj, ctx)
        }

        Iso::Prim(def) => {
            let instrs = recognize_prim(def, ctx)?;
            Ok((instrs, ctx.clone()))
        }

        Iso::Iter(n_ty, body) => {
            // M-A/M-B: iter with numeral count type.
            // Constant count (≤1 inhabitant): trivial (no code).
            // Data-dependent count (M-B): guarded static unroll (ADR-0043 §3).
            let count = numeral_count(n_ty);
            match count {
                Some(0) | Some(1) => {
                    // 0 inhabitants: empty domain, no code needed.
                    // 1 inhabitant: k=0, body never runs.
                    Ok((vec![], ctx.clone()))
                }
                Some(d) => {
                    // M-B: guarded static unroll to D-1 iterations.
                    lower_data_dependent_iter(n_ty, d, body, ctx)
                }
                None => Err(CompileError::Deferred(format!(
                    "iter with non-numeral type: {n_ty}"
                ))),
            }
        }

        // ── M-B bare sum operations (ADR-0043 §3) ──────────────────
        Iso::SwapSum(a, b) => {
            // SwapSum: flip the tag register (0↔1). XORI tag, 1.
            let _la = num_leaves(a)?;
            let _lb = num_leaves(b)?;
            let tag_reg = ctx.mapping[0];
            Ok((vec![Instr::Xori { rd: tag_reg, imm: 1 }], ctx.clone()))
        }

        Iso::Plus(f, g) => {
            // Bare Plus on Sum(a,b): branch on tag, apply f to Left, g to Right.
            lower_bare_plus(f, g, ctx)
        }

        Iso::AssocLSum(_, _, _)
        | Iso::AssocRSum(_, _, _)
        | Iso::UniteSum(_)
        | Iso::UnitiSum(_) => {
            // Structural sum renamings — no register moves in the
            // tag+payload representation for the cases M-B encounters.
            Ok((vec![], ctx.clone()))
        }

        Iso::Distrib(_, _, _) | Iso::Factor(_, _, _) => {
            // Structural (tag,payload)↔branched-form conversion — no-op
            // in the tag+payload register model (ADR-0043 §3).
            Ok((vec![], ctx.clone()))
        }

        // ── Still deferred to M-C/M-D ──────────────────────────────
        Iso::Trace(_, _) => Err(CompileError::Deferred("Trace (M-C/M-D)".into())),

        Iso::Fold(_, _) | Iso::Unfold(_, _) => {
            Err(CompileError::Deferred("Fold/Unfold (M-D)".into()))
        }
    }
}

// ── M-B: data-dependent iter (guarded static unroll) ────────────────

/// Lower `iter n_ty body` with data-dependent count (ADR-0043 §3).
///
/// Emits D-1 guarded copies of `code(body)`, where D = numeral_count(n_ty).
/// Each copy `i` is guarded on the i-th tag register of the unary-encoded
/// Nat value (register `ctx.mapping[i]`): tag=1 iff k > i, meaning the
/// body should execute for this iteration.
///
/// Uses the SWAPBR skip-block: skip the body iff the tag is 0.
fn lower_data_dependent_iter(
    n_ty: &Type,
    d: usize,
    body: &Iso,
    ctx: &Ctx,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    let n_counter = num_leaves(n_ty)?;
    let (ctx_counter, ctx_body) = ctx.split(n_counter);

    // Compile the body once to get its instruction sequence.
    let (body_instrs, _) = lower(body, &ctx_body)?;

    // Allocate 2 aux registers for the skip-block (rb + temp).
    let full_ctx = ctx.clone();
    let aux = allocate_aux(&full_ctx, 2)?;
    let rb = aux[0];
    let temp = aux[1];

    let mut instrs = Vec::new();

    // Emit D-1 guarded copies. Iteration i uses counter tag register i
    // as control: tag=1 → execute (don't skip), tag=0 → skip.
    for i in 0..(d - 1) {
        let ctrl_reg = ctx_counter.mapping[i];
        // skip_when_set=false: skip body iff ctrl=0 (tag=0 means k ≤ i)
        let block = emit_skip_block(ctrl_reg, &body_instrs, rb, temp, false);
        instrs.extend(block);
    }

    Ok((instrs, ctx.clone()))
}

// ── M-B: bare Plus (branch on tag) ──────────────────────────────────

/// Lower `Plus(f, g)` on Sum(a,b): branch on tag register.
///
/// Uses two skip-blocks: skip f iff tag=1, skip g iff tag=0.
/// If one arm is identity (no instructions), its skip-block is omitted.
fn lower_bare_plus(
    f: &Iso,
    g: &Iso,
    ctx: &Ctx,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    let (in_f, _) = theseus_core::type_of(f).map_err(|e| {
        CompileError::Deferred(format!("type error in Plus/f: {e:?}"))
    })?;
    let (in_g, _) = theseus_core::type_of(g).map_err(|e| {
        CompileError::Deferred(format!("type error in Plus/g: {e:?}"))
    })?;
    let n_f = num_leaves(&in_f)?;
    let n_g = num_leaves(&in_g)?;

    // Tag is at ctx.mapping[0]. Payload starts at ctx.mapping[1..].
    let tag_reg = ctx.mapping[0];

    // Build sub-contexts for f (Left payload) and g (Right payload).
    let ctx_f = Ctx {
        mapping: ctx.mapping[1..1 + n_f].to_vec(),
    };
    let ctx_g = Ctx {
        mapping: ctx.mapping[1..1 + n_g].to_vec(),
    };

    let (instrs_f, _) = lower(f, &ctx_f)?;
    let (instrs_g, _) = lower(g, &ctx_g)?;

    let f_is_nop = instrs_f.is_empty();
    let g_is_nop = instrs_g.is_empty();

    if f_is_nop && g_is_nop {
        return Ok((vec![], ctx.clone()));
    }

    // Allocate 2 aux registers for skip-blocks.
    let aux = allocate_aux(ctx, 2)?;
    let rb = aux[0];
    let temp = aux[1];

    let mut instrs = Vec::new();

    // Phase 1: f runs iff tag=0 → skip f iff tag=1
    if !f_is_nop {
        instrs.extend(emit_skip_block(tag_reg, &instrs_f, rb, temp, true));
    }

    // Phase 2: g runs iff tag=1 → skip g iff tag=0
    if !g_is_nop {
        instrs.extend(emit_skip_block(tag_reg, &instrs_g, rb, temp, false));
    }

    Ok((instrs, ctx.clone()))
}

// ── Marshal / unmarshal ──────────────────────────────────────────────

/// Marshal a `Value` of a given type into `StateVals`, writing leaf `i`
/// to `regs[i]`. All other state fields are zeroed.
pub fn marshal(v: &Value, ty: &Type) -> StateVals {
    let bytes = value_to_leaf_bytes(v, ty);
    let mut state = StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_REGS],
    };
    for (i, &b) in bytes.iter().enumerate() {
        state.regs[i] = b;
    }
    state
}

/// Unmarshal a `StateVals` back to a `Value`, reading `regs[i]` as leaf `i`.
pub fn unmarshal(s: &StateVals, ty: &Type) -> Value {
    let n = num_leaves(ty).expect("unmarshal: num_leaves");
    let bytes: Vec<u8> = (0..n).map(|i| s.regs[i]).collect();
    let (v, _) = value_from_leaf_bytes(&bytes, ty);
    v
}

// ── Run-on-CPU (available when theseus-eval is linked) ───────────────

/// Compile an `Iso` to a TISC-1 ROM, marshal an input value, run it on
/// the simulated CPU for exactly `B = rom.len()` steps, and unmarshal
/// the output. The `eval_fn` parameter is the interpreter
/// (`theseus_eval::eval`), passed in because theseus-eval is a
/// dev-dependency.
///
/// This is the M-A end-to-end pipeline: surface → ROM → CPU → result.
pub fn run_on_cpu<E: std::fmt::Debug>(
    iso: &Iso,
    input: &Value,
    eval_fn: impl Fn(&Iso, Value, &mut u64) -> Result<Value, E>,
) -> Result<Value, CompileError> {
    let (in_ty, out_ty) = theseus_core::type_of(iso).map_err(|e| {
        CompileError::Deferred(format!("type error: {e:?}"))
    })?;

    let instrs = compile(iso)?;
    let rom = assemble(&instrs);
    let budget = instrs.len();

    let state = marshal(input, &in_ty);

    // Run the CPU for exactly `budget` steps using eval(step).
    let step_iso = crate::step::step(&rom);
    let mut cur = state.encode();
    let mut fuel = 8_000_000_000u64;
    for _ in 0..budget {
        cur = eval_fn(&step_iso, cur, &mut fuel).map_err(|e| {
            CompileError::Deferred(format!("CPU eval failed: {e:?}"))
        })?;
    }
    let final_state = StateVals::decode(&cur);

    Ok(unmarshal(&final_state, &out_ty))
}

// ── Fast direct TISC-1 simulator (M-B) ─────────────────────────────
//
// Direct byte-level interpretation of the TISC-1 instruction set,
// matching the step iso's semantics without going through the core-iso
// evaluator. This makes the differential oracle practical for M-B
// programs with larger ROMs (e.g. guarded-unroll iter).

/// Execute one TISC-1 instruction on the machine state (direct simulation).
fn exec_instr(instr: Instr, state: &mut StateVals) {
    match instr {
        Instr::Add { rd, rs } => {
            state.regs[rd as usize] = state.regs[rd as usize].wrapping_add(state.regs[rs as usize]);
        }
        Instr::Sub { rd, rs } => {
            state.regs[rd as usize] = state.regs[rd as usize].wrapping_sub(state.regs[rs as usize]);
        }
        Instr::Xor { rd, rs } => {
            state.regs[rd as usize] ^= state.regs[rs as usize];
        }
        Instr::Exch { rd, rs } => {
            let tmp = state.regs[rd as usize];
            state.regs[rd as usize] = state.regs[rs as usize];
            state.regs[rs as usize] = tmp;
        }
        Instr::Rotl { rd, rs } => {
            let amount = state.regs[rs as usize] & 7;
            state.regs[rd as usize] = state.regs[rd as usize].rotate_left(amount as u32);
        }
        Instr::Rotr { rd, rs } => {
            let amount = state.regs[rs as usize] & 7;
            state.regs[rd as usize] = state.regs[rd as usize].rotate_right(amount as u32);
        }
        Instr::Cswap { rd, rs, rt } => {
            if state.regs[rd as usize] & 1 == 1 {
                let tmp = state.regs[rs as usize];
                state.regs[rs as usize] = state.regs[rt as usize];
                state.regs[rt as usize] = tmp;
            }
        }
        Instr::Xchg { rd, sx } => {
            let tmp = state.regs[rd as usize];
            state.regs[rd as usize] = state.scratch[sx as usize];
            state.scratch[sx as usize] = tmp;
        }
        Instr::Swapbr { rd } => {
            let tmp = state.regs[rd as usize];
            state.regs[rd as usize] = state.br;
            state.br = tmp;
        }
        Instr::Addi { rd, imm } => {
            state.regs[rd as usize] = state.regs[rd as usize].wrapping_add(imm);
        }
        Instr::Subi { rd, imm } => {
            state.regs[rd as usize] = state.regs[rd as usize].wrapping_sub(imm);
        }
        Instr::Xori { rd, imm } => {
            state.regs[rd as usize] ^= imm;
        }
        Instr::Rotli { rd, imm } => {
            state.regs[rd as usize] = state.regs[rd as usize].rotate_left((imm & 7) as u32);
        }
        Instr::Rotri { rd, imm } => {
            state.regs[rd as usize] = state.regs[rd as usize].rotate_right((imm & 7) as u32);
        }
    }
}

/// Fast direct TISC-1 simulation: compile → assemble → run → unmarshal.
///
/// Equivalent to `run_on_cpu` but uses direct byte-level interpretation
/// instead of the step iso evaluator. The budget is `instrs.len()` steps
/// (worst-case trace length; after the ROM ends the machine idles).
pub fn run_on_cpu_fast(
    iso: &Iso,
    input: &Value,
) -> Result<Value, CompileError> {
    let (in_ty, out_ty) = theseus_core::type_of(iso).map_err(|e| {
        CompileError::Deferred(format!("type error: {e:?}"))
    })?;

    let instrs = compile(iso)?;
    let rom = assemble(&instrs);
    let budget = instrs.len();

    let mut state = marshal(input, &in_ty);

    for _ in 0..budget {
        // Fetch
        let pc = state.pc as usize;
        let word = if pc < rom.len() { rom[pc] } else { 0 };
        // Decode
        if let Some(instr) = crate::asm::decode(word) {
            exec_instr(instr, &mut state);
        }
        // PC update: PC += BR + 1
        state.pc = state.pc.wrapping_add(state.br).wrapping_add(1);
    }

    Ok(unmarshal(&state, &out_ty))
}
