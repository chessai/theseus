//! M-A/M-B/M-C1 surface→TISC-1 ROM compiler (ADR-0042 + ADR-0043 + ADR-0044).
//!
//! Compiles the product/composition fragment of core `Iso` — plus the
//! controlled-op idiom (general arms), data-dependent `Iter`, and bare
//! sum operations — to TISC-1 instruction sequences. M-C1 extends the
//! register-only M-A/M-B fragment with scratch-spill support via Xchg,
//! raising the leaf ceiling from 8 to 14 (straight-line) or 12 (branching).
//!
//! The acceptance test is the run-on-CPU differential oracle (ADR-0042 §6,
//! ADR-0043 §5, ADR-0044 §4): for every input value, the compiled ROM
//! executed on the simulated CPU must agree with `theseus_eval::eval`.

use std::fmt;

use theseus_core::typing::is_numeral_ty;
use theseus_core::{adjoint, Iso, Type, Value};

use crate::asm::{assemble, Instr};
use crate::types::{
    decode_system, encode_system,
    StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH,
};

// ── Errors ────────────────────────────────────────────────────────────

/// Compile-time errors for the M-A/M-B/M-C1 compiler.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompileError {
    /// The program requires more leaves than the compiler supports.
    TooManyLeaves { needed: usize, max: usize },
    /// A constructor deferred to a later milestone.
    Deferred(String),
    /// A `Prim` whose clause table doesn't match any recognized pattern.
    UnrecognizedPrim(String),
    /// Instruction would alias registers (rd == rs).
    AliasedRegisters { instr: String },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::TooManyLeaves { needed, max } => {
                write!(f, "program needs {needed} leaves but maximum is {max}")
            }
            CompileError::Deferred(msg) => write!(f, "deferred to later milestone: {msg}"),
            CompileError::UnrecognizedPrim(msg) => write!(f, "unrecognized prim: {msg}"),
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

// ── Location and constants (M-C1, ADR-0044 §2) ─────────────────────

/// Where a leaf value resides: register, scratch word, or memory word.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Location {
    Reg(u8),
    Scratch(u8),
    Mem(u8),
}

/// Maximum leaves for straight-line programs (8 regs + s2..s7 + MEM_WORDS mem = 14+N).
const MAX_LEAVES: usize = 14 + MEM_WORDS;

/// Maximum leaves for branching programs (need 2 free-zero cells for aux
/// zero-source, ADR-0044 §2.4 / ADR-0046 §3.1).
const MAX_LEAVES_BRANCHING: usize = 12 + MEM_WORDS;

/// First spillable scratch index (s0/s1 are decoder-reserved).
const SCRATCH_SPILL_BASE: u8 = 2;

// ── Register allocation context ──────────────────────────────────────

/// Maps leaf positions to physical locations (register or scratch word).
/// Between every instruction the machine is in canonical layout — every
/// leaf sits in its home (ADR-0044 §2.1).
#[derive(Clone, Debug)]
struct Ctx {
    /// `homes[i]` = canonical physical location for leaf `i`.
    homes: Vec<Location>,
}

impl Ctx {
    /// Canonical home layout (ADR-0044 §2.1 + ADR-0046 §3.1):
    /// L < 8 → Reg(L); 8 ≤ L < 14 → Scratch(L − 6); 14 ≤ L < 14+N → Mem(L − 14).
    fn new(n_leaves: usize) -> Self {
        Ctx {
            homes: (0..n_leaves)
                .map(|i| {
                    if i < NUM_REGS {
                        Location::Reg(i as u8)
                    } else if i < 14 {
                        Location::Scratch((i - 6) as u8)
                    } else {
                        Location::Mem((i - 14) as u8)
                    }
                })
                .collect(),
        }
    }

    /// Split into two sub-contexts for `Times(f, g)`.
    fn split(&self, n_left: usize) -> (Ctx, Ctx) {
        let left = Ctx {
            homes: self.homes[..n_left].to_vec(),
        };
        let right = Ctx {
            homes: self.homes[n_left..].to_vec(),
        };
        (left, right)
    }

    /// Merge two sub-contexts back.
    fn merge(left: &Ctx, right: &Ctx) -> Ctx {
        let mut homes = left.homes.clone();
        homes.extend_from_slice(&right.homes);
        Ctx { homes }
    }
}

// ── with_registers (M-C1 §2.3) ─────────────────────────────────────

/// Page scratch-resident and memory-resident operands into spare registers,
/// emit instructions via the closure, then undo the paging to restore
/// canonical layout.
///
/// For each scratch-home operand, Xchg it into a spare register.
/// For each mem-home operand, Mexch it into a spare register (the
/// transient-squat pattern, Lemma 7.2 — the spare's leaf transiently
/// squats in mem[addr] and is restored by the self-inverse Mexch).
/// After the closure, undo the page-ins in reverse order.
fn with_registers(
    ctx: &Ctx,
    leaf_indices: &[usize],
    emit: impl FnOnce(&[u8]) -> Result<Vec<Instr>, CompileError>,
) -> Result<Vec<Instr>, CompileError> {
    let mut used_regs = std::collections::BTreeSet::new();
    let mut reg_for = vec![0u8; leaf_indices.len()];
    let mut scratch_ops: Vec<(usize, u8)> = Vec::new();
    let mut mem_ops: Vec<(usize, u8)> = Vec::new();

    for (i, &leaf) in leaf_indices.iter().enumerate() {
        match ctx.homes[leaf] {
            Location::Reg(r) => {
                used_regs.insert(r);
                reg_for[i] = r;
            }
            Location::Scratch(sx) => {
                scratch_ops.push((i, sx));
            }
            Location::Mem(addr) => {
                mem_ops.push((i, addr));
            }
        }
    }

    if scratch_ops.is_empty() && mem_ops.is_empty() {
        return emit(&reg_for);
    }

    // Spare registers: any of 0..7 not used by register-home operands.
    let mut spares: Vec<u8> = (0..NUM_REGS as u8)
        .filter(|r| !used_regs.contains(r))
        .collect();

    // Page-in/out stack: records (instr) for undo in reverse.
    let mut page_in = Vec::new();
    let mut page_out_stack: Vec<Instr> = Vec::new();

    for &(i, sx) in &scratch_ops {
        let spare = spares
            .pop()
            .expect("with_registers: no spare register");
        reg_for[i] = spare;
        page_in.push(Instr::Xchg { rd: spare, sx });
        page_out_stack.push(Instr::Xchg { rd: spare, sx });
    }

    for &(i, addr) in &mem_ops {
        let spare = spares
            .pop()
            .expect("with_registers: no spare register for mem page-in");
        reg_for[i] = spare;
        page_in.push(Instr::Mexch { rd: spare, addr });
        page_out_stack.push(Instr::Mexch { rd: spare, addr });
    }

    let body = emit(&reg_for)?;

    let page_out: Vec<Instr> = page_out_stack.into_iter().rev().collect();

    let mut result = page_in;
    result.extend(body);
    result.extend(page_out);
    Ok(result)
}

// ── swap_homes (SwapProd with spill) ────────────────────────────────

/// Swap data between two leaf homes. Handles all combinations of
/// Reg, Scratch, Mem via the appropriate exchange instructions.
fn swap_homes(ctx: &Ctx, leaf_a: usize, leaf_b: usize) -> Vec<Instr> {
    let ha = ctx.homes[leaf_a];
    let hb = ctx.homes[leaf_b];
    match (ha, hb) {
        (Location::Reg(ra), Location::Reg(rb)) => {
            if ra == rb {
                vec![]
            } else {
                vec![Instr::Exch { rd: ra, rs: rb }]
            }
        }
        (Location::Reg(r), Location::Scratch(sx))
        | (Location::Scratch(sx), Location::Reg(r)) => {
            vec![Instr::Xchg { rd: r, sx }]
        }
        (Location::Reg(r), Location::Mem(addr))
        | (Location::Mem(addr), Location::Reg(r)) => {
            vec![Instr::Mexch { rd: r, addr }]
        }
        (Location::Scratch(sa), Location::Scratch(sb)) => {
            if sa == sb {
                return vec![];
            }
            let tmp = 0u8;
            vec![
                Instr::Xchg { rd: tmp, sx: sa },
                Instr::Xchg { rd: tmp, sx: sb },
                Instr::Xchg { rd: tmp, sx: sa },
            ]
        }
        (Location::Scratch(sx), Location::Mem(addr))
        | (Location::Mem(addr), Location::Scratch(sx)) => {
            // Scratch↔Mem via temp register: Xchg into reg, Mexch, Xchg back.
            let tmp = 0u8;
            vec![
                Instr::Xchg { rd: tmp, sx },
                Instr::Mexch { rd: tmp, addr },
                Instr::Xchg { rd: tmp, sx },
            ]
        }
        (Location::Mem(a), Location::Mem(b)) => {
            if a == b {
                return vec![];
            }
            // Mem↔Mem via temp register.
            let tmp = 0u8;
            vec![
                Instr::Mexch { rd: tmp, addr: a },
                Instr::Mexch { rd: tmp, addr: b },
                Instr::Mexch { rd: tmp, addr: a },
            ]
        }
    }
}

// ── Auxiliary register allocation (M-C1 §2.4) ──────────────────────

/// Allocate `count` auxiliary registers (zero-valued on entry) for
/// skip-block use.
///
/// Returns `(aux_regs, page_in, page_out, displaced_ctx)`.
///
/// When free registers exist, they're already 0 (marshal zeroes
/// everything). When the register file is full, uses the **zero-source
/// pattern** (ADR-0044 §2.4): Xchg a register-home leaf into a free
/// (already-zero) scratch slot, which zeroes the register and parks
/// the leaf data in scratch.
///
/// `exclude_leaves`: leaf indices that must NOT be displaced (e.g. the
/// control leaf, whose data must remain accessible during the skip block).
/// Result of `allocate_aux`: the zeroed aux register indices, the instructions
/// that page leaves out to free the registers, the instructions that restore
/// them afterward, and the updated `Ctx` reflecting the temporary residence.
type AuxAlloc = (Vec<u8>, Vec<Instr>, Vec<Instr>, Ctx);

fn allocate_aux(
    ctx: &Ctx,
    count: usize,
    exclude_leaves: &[usize],
) -> Result<AuxAlloc, CompileError> {
    let used_regs: std::collections::BTreeSet<u8> = ctx
        .homes
        .iter()
        .filter_map(|h| match h {
            Location::Reg(r) => Some(*r),
            _ => None,
        })
        .collect();
    let free_regs: Vec<u8> = (0..NUM_REGS as u8)
        .filter(|r| !used_regs.contains(r))
        .collect();

    if free_regs.len() >= count {
        return Ok((free_regs[..count].to_vec(), vec![], vec![], ctx.clone()));
    }

    // Zero-source: displace register-home leaves into free scratch OR
    // free memory slots (ADR-0046 §3.1 — unused marshalling-zeroed words).
    let used_scratch: std::collections::BTreeSet<u8> = ctx
        .homes
        .iter()
        .filter_map(|h| match h {
            Location::Scratch(s) => Some(*s),
            _ => None,
        })
        .collect();
    let used_mem: std::collections::BTreeSet<u8> = ctx
        .homes
        .iter()
        .filter_map(|h| match h {
            Location::Mem(a) => Some(*a),
            _ => None,
        })
        .collect();

    // Collect free zero-source slots: scratch s2..s7 first, then mem words.
    enum ZeroSlot {
        Scratch(u8),
        Mem(u8),
    }
    let mut free_slots: Vec<ZeroSlot> = Vec::new();
    for s in (SCRATCH_SPILL_BASE..NUM_SCRATCH as u8).rev() {
        if !used_scratch.contains(&s) {
            free_slots.push(ZeroSlot::Scratch(s));
        }
    }
    for a in (0..MEM_WORDS as u8).rev() {
        if !used_mem.contains(&a) {
            free_slots.push(ZeroSlot::Mem(a));
        }
    }

    let needed = count - free_regs.len();

    // Displaceable = register-home leaves NOT in the exclude set.
    let displaceable: Vec<(usize, u8)> = ctx
        .homes
        .iter()
        .enumerate()
        .filter_map(|(i, h)| match h {
            Location::Reg(r) if !exclude_leaves.contains(&i) => Some((i, *r)),
            _ => None,
        })
        .collect();

    if free_slots.len() < needed || displaceable.len() < needed {
        return Err(CompileError::TooManyLeaves {
            needed: ctx.homes.len(),
            max: MAX_LEAVES_BRANCHING,
        });
    }

    let mut aux_regs = free_regs;
    let mut page_in = Vec::new();
    let mut page_out = Vec::new();
    let mut modified_homes = ctx.homes.clone();

    for i in 0..needed {
        let (leaf_idx, reg) = displaceable[displaceable.len() - 1 - i];
        let slot = free_slots.pop().unwrap();
        match slot {
            ZeroSlot::Scratch(sx) => {
                page_in.push(Instr::Xchg { rd: reg, sx });
                page_out.push(Instr::Xchg { rd: reg, sx });
                modified_homes[leaf_idx] = Location::Scratch(sx);
            }
            ZeroSlot::Mem(addr) => {
                page_in.push(Instr::Mexch { rd: reg, addr });
                page_out.push(Instr::Mexch { rd: reg, addr });
                modified_homes[leaf_idx] = Location::Mem(addr);
            }
        }
        aux_regs.push(reg);
    }

    page_out.reverse();
    Ok((
        aux_regs[..count].to_vec(),
        page_in,
        page_out,
        Ctx {
            homes: modified_homes,
        },
    ))
}

/// Ensure a leaf is in a register. If it's in scratch, page it into a
/// spare register (excluded from `exclude_regs`).
/// Returns `(register_index, page_in_instrs, page_out_instrs)`.
fn ensure_reg(ctx: &Ctx, leaf: usize, exclude_regs: &[u8]) -> (u8, Vec<Instr>, Vec<Instr>) {
    match ctx.homes[leaf] {
        Location::Reg(r) => (r, vec![], vec![]),
        Location::Scratch(sx) => {
            let spare = (0..NUM_REGS as u8)
                .find(|r| !exclude_regs.contains(r))
                .expect("ensure_reg: no spare register");
            (
                spare,
                vec![Instr::Xchg { rd: spare, sx }],
                vec![Instr::Xchg { rd: spare, sx }],
            )
        }
        Location::Mem(addr) => {
            let spare = (0..NUM_REGS as u8)
                .find(|r| !exclude_regs.contains(r))
                .expect("ensure_reg: no spare register for mem");
            (
                spare,
                vec![Instr::Mexch { rd: spare, addr }],
                vec![Instr::Mexch { rd: spare, addr }],
            )
        }
    }
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
        out.push(Instr::Addi {
            rd: temp,
            imm: body_len,
        });
        out.push(Instr::Cswap {
            rd: ctrl_reg,
            rs: temp,
            rt: rb,
        });
    } else {
        // Skip iff ctrl=0: load skip_dist into rb iff ctrl=0.
        // Start with rb=skip_dist, temp=0. CSWAP(ctrl, rb, temp):
        //   ctrl=1 → swap: rb=0, temp=skip_dist (don't skip = run body)
        //   ctrl=0 → no swap: rb=skip_dist (skip body)
        out.push(Instr::Addi {
            rd: rb,
            imm: body_len,
        });
        out.push(Instr::Cswap {
            rd: ctrl_reg,
            rs: rb,
            rt: temp,
        });
    }

    // Branch: SWAPBR rb
    out.push(Instr::Swapbr { rd: rb });

    // Body
    out.extend_from_slice(body_instrs);

    // Come-from: SWAPBR rb (clears BR, captures offset into rb)
    out.push(Instr::Swapbr { rd: rb });

    // Cleanup: reverse the CSWAP + SUBI
    if skip_when_set {
        out.push(Instr::Cswap {
            rd: ctrl_reg,
            rs: temp,
            rt: rb,
        });
        out.push(Instr::Subi {
            rd: temp,
            imm: body_len,
        });
    } else {
        out.push(Instr::Cswap {
            rd: ctrl_reg,
            rs: rb,
            rt: temp,
        });
        out.push(Instr::Subi {
            rd: rb,
            imm: body_len,
        });
    }

    out
}

// ── Prim recognition ─────────────────────────────────────────────────

use theseus_core::prim::{enumerate_values, PrimDef};

/// Try to recognize a PrimDef as a known TISC-1 instruction pattern.
/// All register accesses are routed through `with_registers` so that
/// scratch-resident operands are paged in/out correctly.
fn recognize_prim(def: &PrimDef, ctx: &Ctx) -> Result<Vec<Instr>, CompileError> {
    let n_in = num_leaves(&def.input_ty)?;
    let n_out = num_leaves(&def.output_ty)?;
    let leaves: Vec<usize> = (0..n_in).collect();

    // Two-register prims: (a, b) ↦ (a', b')
    if n_in == 2 && n_out == 2 {
        let in_ty = &def.input_ty;
        let vals = enumerate_values(in_ty);

        // addInto: (a, b) ↦ (a, a + b mod 256)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (a, a.wrapping_add(b))) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Add {
                    rd: regs[1],
                    rs: regs[0],
                }])
            });
        }

        // subFrom: (a, b) ↦ (a, b - a mod 256)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (a, b.wrapping_sub(a))) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Sub {
                    rd: regs[1],
                    rs: regs[0],
                }])
            });
        }

        // xorInto: (a, b) ↦ (a, a ^ b)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (a, a ^ b)) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Xor {
                    rd: regs[1],
                    rs: regs[0],
                }])
            });
        }

        // swap: (a, b) ↦ (b, a)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (b, a)) {
            return with_registers(ctx, &leaves, |regs| {
                if regs[0] == regs[1] {
                    return Err(CompileError::AliasedRegisters {
                        instr: "EXCH".into(),
                    });
                }
                Ok(vec![Instr::Exch {
                    rd: regs[0],
                    rs: regs[1],
                }])
            });
        }

        // rotl: (a, b) ↦ (a, rotl(b, a & 7))
        if check_pattern_2reg(&def.forward, &vals, |a, b| {
            (a, b.rotate_left((a & 7) as u32))
        }) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Rotl {
                    rd: regs[1],
                    rs: regs[0],
                }])
            });
        }

        // rotr: (a, b) ↦ (a, rotr(b, a & 7))
        if check_pattern_2reg(&def.forward, &vals, |a, b| {
            (a, b.rotate_right((a & 7) as u32))
        }) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Rotr {
                    rd: regs[1],
                    rs: regs[0],
                }])
            });
        }
    }

    // Three-register prims: (c, a, b)
    if n_in == 3 && n_out == 3 {
        let vals = enumerate_values(&def.input_ty);

        // cswap: (c, a, b) ↦ (c, if c&1 then (b,a) else (a,b))
        if check_pattern_3reg(&def.forward, &vals, |c, a, b| {
            if c & 1 == 1 {
                (c, b, a)
            } else {
                (c, a, b)
            }
        }) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Cswap {
                    rd: regs[0],
                    rs: regs[1],
                    rt: regs[2],
                }])
            });
        }
    }

    // One-register prims: a ↦ f(a)
    if n_in == 1 && n_out == 1 {
        let vals = enumerate_values(&def.input_ty);

        // addi_K: a ↦ a + K
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a.wrapping_add(k)) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Addi {
                    rd: regs[0],
                    imm: k,
                }])
            });
        }

        // subi_K: a ↦ a - K
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a.wrapping_sub(k)) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Subi {
                    rd: regs[0],
                    imm: k,
                }])
            });
        }

        // xori_K: a ↦ a ^ K
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a ^ k) {
            return with_registers(ctx, &leaves, |regs| {
                Ok(vec![Instr::Xori {
                    rd: regs[0],
                    imm: k,
                }])
            });
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
/// The `ctx` covers [ctrl_leaves..., tgt_leaves...] (the input layout
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

    // Single-instruction recognition: CNOT (ctrl XOR into target)
    if n_tgt == 1 && is_not_on_target_times_id(g, tgt_ty) {
        let instrs = with_registers(ctx, &[0, n_ctrl], |regs| {
            Ok(vec![Instr::Xor {
                rd: regs[1],
                rs: regs[0],
            }])
        })?;
        return Ok((instrs, ctx.clone()));
    }

    // Single-instruction recognition: CSWAP
    if n_tgt == 2 && is_swap_on_target_times_id(g, tgt_ty) {
        let instrs = with_registers(ctx, &[0, n_ctrl, n_ctrl + 1], |regs| {
            Ok(vec![Instr::Cswap {
                rd: regs[0],
                rs: regs[1],
                rt: regs[2],
            }])
        })?;
        return Ok((instrs, ctx.clone()));
    }

    // Both identity → no-op
    if is_structural_identity(g) {
        return Ok((vec![], ctx.clone()));
    }

    // General case: compile g's body and use skip-block.
    // Allocate aux first (may displace register-home leaves to scratch).
    let (aux, aux_pi, aux_po, dctx) = allocate_aux(ctx, 2, &[0])?;
    let rb = aux[0];
    let temp = aux[1];

    let (ctrl_reg, ctrl_pi, ctrl_po) = ensure_reg(&dctx, 0, &[rb, temp]);

    // Compile g's body with the displaced context's target sub-slice.
    let ctx_tgt = Ctx {
        homes: dctx.homes[n_ctrl..n_ctrl + n_tgt].to_vec(),
    };
    let (instrs_g, _) = lower(g, &ctx_tgt)?;

    if instrs_g.is_empty() {
        return Ok((vec![], ctx.clone()));
    }

    // f = identity, g = general multi-instruction sequence.
    // Skip g iff ctrl=0 (Left branch = identity).
    let mut instrs = Vec::new();
    instrs.extend(aux_pi);
    instrs.extend(ctrl_pi);
    instrs.extend(emit_skip_block(ctrl_reg, &instrs_g, rb, temp, false));
    instrs.extend(ctrl_po);
    instrs.extend(aux_po);
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

/// Check if an iso is NOT on Bit (SwapSum(1,1) or Seq(Plus(Id,Id),SwapSum(1,1))
/// or the adjoint form Seq(SwapSum(1,1),Plus(Id,Id))).
fn is_not_iso(iso: &Iso) -> bool {
    if is_bit_not(iso) {
        return true;
    }
    if let Iso::Seq(a, b) = iso {
        let (swap, plus) = if matches!(b.as_ref(), Iso::SwapSum(_, _)) {
            (b.as_ref(), a.as_ref())
        } else {
            (a.as_ref(), b.as_ref())
        };
        if let Iso::SwapSum(sa, sb) = swap {
            if *sa == Type::One && *sb == Type::One {
                if let Iso::Plus(pf, pg) = plus {
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

// ── Spine helpers (M-D.a: list-spine fold/unfold) ───────────────────

/// Analyze a list-spine type: return `(depth, element_type)` if `l` is
/// `list_spine_ty(b, d)` with at least one cons layer (depth ≥ 1).
/// Returns `None` if not a list-spine shape (caller keeps `Deferred`).
///
/// Walks the `Sum(One, Prod(b, rest))` nesting, verifying every layer
/// has the same element type `b`. Analogous to `numeral_count` for Nat.
fn spine_info(l: &Type) -> Option<(usize, Type)> {
    let mut depth = 0usize;
    let mut elem: Option<Type> = None;
    let mut cur = l;
    loop {
        match cur {
            Type::Zero => {
                // Bottom of the spine. Return if we saw ≥ 1 cons layer.
                return elem.map(|e| (depth, e));
            }
            Type::Sum(one, rest) if one.as_ref() == &Type::One => match rest.as_ref() {
                Type::Prod(b, tail) => {
                    match &elem {
                        Some(e) if *e != **b => return None,
                        None => elem = Some(b.as_ref().clone()),
                        _ => {}
                    }
                    depth += 1;
                    cur = tail;
                }
                _ => return None,
            },
            _ => return None,
        }
    }
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

/// Compile a core `Iso` (M-A/M-B/M-C1) to a TISC-1 instruction sequence.
///
/// Returns `Err(CompileError)` for out-of-scope constructs (Trace/Fold/Unfold).
pub fn compile(iso: &Iso) -> Result<Vec<Instr>, CompileError> {
    let (in_ty, _out_ty) = theseus_core::type_of(iso).map_err(|e| {
        CompileError::Deferred(format!("type error: {e:?}"))
    })?;
    let n = num_leaves(&in_ty)?;
    if n > MAX_LEAVES {
        return Err(CompileError::TooManyLeaves {
            needed: n,
            max: MAX_LEAVES,
        });
    }
    let ctx = Ctx::new(n);
    let (instrs, _final_ctx) = lower(iso, &ctx)?;
    Ok(instrs)
}

/// Right-associate a left-nested `Seq` chain so that idiom recognizers
/// (e.g. `match_controlled_idiom`) see the canonical right-nested shape.
///
/// `Seq(Seq(a, b), c)` → `Seq(a, Seq(b, c))`, applied recursively until
/// the spine is fully right-associated. Leaves non-`Seq` nodes unchanged.
/// This is needed because `adjoint(Seq(f, g)) = Seq(adj(g), adj(f))`
/// turns a right-nested chain into a left-nested one.
fn right_assoc_seq(iso: Iso) -> Iso {
    match iso {
        Iso::Seq(f, g) => {
            let f = right_assoc_seq(*f);
            let g = right_assoc_seq(*g);
            match f {
                Iso::Seq(a, b) => right_assoc_seq(Iso::seq(*a, Iso::seq(*b, g))),
                other => Iso::seq(other, g),
            }
        }
        other => other,
    }
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
            let total = n_a + n_b;
            // Decompose the permutation into transpositions, using
            // swap_homes to handle Reg↔Reg, Reg↔Scratch, Scratch↔Scratch.
            let target: Vec<usize> = (n_a..total).chain(0..n_a).collect();
            let mut perm: Vec<usize> = (0..total).collect();
            let mut instrs = Vec::new();
            for i in 0..total {
                if perm[i] != target[i] {
                    let j = perm.iter().position(|&x| x == target[i]).unwrap();
                    instrs.extend(swap_homes(ctx, i, j));
                    perm.swap(i, j);
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
            // Right-associate Seq chains: adjoint reverses nesting
            // (right-nested → left-nested), which prevents the
            // controlled-op idiom recognizer from matching.
            let adj = right_assoc_seq(adjoint(f));
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
            let instrs = with_registers(ctx, &[0], |regs| {
                Ok(vec![Instr::Xori {
                    rd: regs[0],
                    imm: 1,
                }])
            })?;
            Ok((instrs, ctx.clone()))
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

        Iso::Fold(l, body) => lower_fold_unfold(l, body, ctx, true),
        Iso::Unfold(l, body) => lower_fold_unfold(l, body, ctx, false),
    }
}

// ── M-B: data-dependent iter (guarded static unroll) ────────────────

/// Lower `iter n_ty body` with data-dependent count (ADR-0043 §3).
///
/// Emits D-1 guarded copies of `code(body)`, where D = numeral_count(n_ty).
/// Each copy `i` is guarded on the i-th tag register of the unary-encoded
/// Nat value (register `ctx.homes[i]`): tag=1 iff k > i, meaning the
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

    // Allocate 2 aux registers for skip-blocks, excluding counter leaves.
    let exclude: Vec<usize> = (0..n_counter).collect();
    let (aux, aux_pi, aux_po, dctx) = allocate_aux(ctx, 2, &exclude)?;
    let rb = aux[0];
    let temp = aux[1];

    // Compile the body once with the displaced context's body sub-slice.
    let (_, dctx_body) = dctx.split(n_counter);
    let (body_instrs, body_ctx_out) = lower(body, &dctx_body)?;

    // §2.6 guarded-unroll consistency: canonical layout at body boundary.
    debug_assert_eq!(
        dctx_body.homes, body_ctx_out.homes,
        "M-C1 §2.6: Ctx residence changed during body lowering — \
         frozen unroll body requires canonical layout at boundaries"
    );

    let mut instrs = Vec::new();
    instrs.extend(aux_pi);

    // Emit D-1 guarded copies. Iteration i uses counter tag register i
    // as control: tag=1 → execute (don't skip), tag=0 → skip.
    for i in 0..(d - 1) {
        let (ctrl_reg, ctrl_pi, ctrl_po) = ensure_reg(&dctx, i, &[rb, temp]);
        instrs.extend(ctrl_pi);
        // skip_when_set=false: skip body iff ctrl=0 (tag=0 means k ≤ i)
        instrs.extend(emit_skip_block(ctrl_reg, &body_instrs, rb, temp, false));
        instrs.extend(ctrl_po);
    }

    instrs.extend(aux_po);
    Ok((instrs, ctx.clone()))
}

// ── M-D.a: fold/unfold over list-spine (guarded static unroll) ──────

/// Lower `fold l body` or `unfold l body` by guarded static unroll
/// (ADR-0042 M-D.a). Each cons layer `j` of the finite list spine gets
/// its own copy of the body, guarded on the cons-tag leaf (tag=1 ⇒
/// element present ⇒ run; tag=0 ⇒ shorter list ⇒ skip). The body is
/// lowered per-copy under a displaced `Ctx` targeting element `j`'s
/// leaves + the accumulator leaves.
///
/// `ascending = true` for fold (head-to-tail), `false` for unfold
/// (tail-to-head).
fn lower_fold_unfold(
    l: &Type,
    body: &Iso,
    ctx: &Ctx,
    ascending: bool,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    let (depth, elem_ty) = spine_info(l).ok_or_else(|| {
        CompileError::Deferred("Fold/Unfold with non-list-spine type".into())
    })?;

    let m = num_leaves(&elem_ty)?;
    let n_spine = num_leaves(l)?;
    let stride = 1 + m; // leaves per cons layer: 1 tag + m element

    // Extract accumulator type from body: body : b × c ↔ b × c.
    let (body_in, _) = theseus_core::type_of(body).map_err(|e| {
        CompileError::Deferred(format!("type error in Fold/Unfold body: {e:?}"))
    })?;
    let n_acc = match &body_in {
        Type::Prod(_, c) => num_leaves(c)?,
        _ => {
            return Err(CompileError::Deferred(
                "Fold/Unfold body input not a product".into(),
            ))
        }
    };

    let total = n_spine + n_acc;
    if total > MAX_LEAVES {
        return Err(CompileError::TooManyLeaves {
            needed: total,
            max: MAX_LEAVES,
        });
    }

    // depth ≤ 1 ⇒ spine can only hold nil (length 0) ⇒ no elements ⇒ no-op.
    if depth <= 1 {
        return Ok((vec![], ctx.clone()));
    }

    // Right-associate the body Seq chain: adjoint(Fold(l, f)) = Unfold(l, adj(f)),
    // and adj(f) is left-nested when f was right-nested. The per-copy body is
    // lowered against a SMALL sub-context, so the controlled-op idiom must fire
    // to avoid aux-register conflicts with the outer spine context.
    let body_norm = right_assoc_seq(body.clone());
    let body = &body_norm;

    // Exclude tag leaves we guard on from aux displacement so they stay
    // in registers and ensure_reg returns them directly.
    let used_tag_leaves: Vec<usize> = (0..depth - 1).map(|j| j * stride).collect();
    let (aux, aux_pi, aux_po, dctx) = allocate_aux(ctx, 2, &used_tag_leaves)?;
    let rb = aux[0];
    let temp = aux[1];

    let acc_start = n_spine; // accumulator leaves start after spine

    let mut instrs = Vec::new();
    instrs.extend(aux_pi);

    // Layer indices: ascending (1..depth) for fold, descending for unfold.
    let layers: Vec<usize> = if ascending {
        (1..depth).collect()
    } else {
        (1..depth).rev().collect()
    };

    for j in layers {
        let tag_leaf = (j - 1) * stride;
        let elem_start = tag_leaf + 1;

        // Build per-copy body sub-context: [element_j, accumulator].
        let mut body_homes = Vec::with_capacity(m + n_acc);
        for k in 0..m {
            body_homes.push(dctx.homes[elem_start + k]);
        }
        for k in 0..n_acc {
            body_homes.push(dctx.homes[acc_start + k]);
        }
        let body_ctx = Ctx { homes: body_homes };

        // Lower the body against this per-copy context.
        let (body_instrs, body_ctx_out) = lower(body, &body_ctx)?;

        // Home-preservation invariant: the body must return every leaf
        // to its entry home (the existing canonical-layout requirement).
        debug_assert_eq!(
            body_ctx.homes, body_ctx_out.homes,
            "M-D.a: Ctx residence changed during fold/unfold body lowering (layer {j})"
        );

        if body_instrs.is_empty() {
            continue;
        }

        // Ensure tag register is accessible.
        let (tag_reg, tag_pi, tag_po) = ensure_reg(&dctx, tag_leaf, &[rb, temp]);

        instrs.extend(tag_pi);
        // skip_when_set=false: skip body iff tag=0 (nil / shorter list).
        instrs.extend(emit_skip_block(tag_reg, &body_instrs, rb, temp, false));
        instrs.extend(tag_po);
    }

    instrs.extend(aux_po);
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

    // Early out: both structurally identity → no-op.
    if is_structural_identity(f) && is_structural_identity(g) {
        return Ok((vec![], ctx.clone()));
    }

    // Allocate 2 aux registers for skip-blocks, excluding tag leaf 0.
    let (aux, aux_pi, aux_po, dctx) = allocate_aux(ctx, 2, &[0])?;
    let rb = aux[0];
    let temp = aux[1];

    // Tag register (may need paging from scratch).
    let (tag_reg, tag_pi, tag_po) = ensure_reg(&dctx, 0, &[rb, temp]);

    // Build sub-contexts for f (Left payload) and g (Right payload)
    // from the displaced context.
    let ctx_f = Ctx {
        homes: dctx.homes[1..1 + n_f].to_vec(),
    };
    let ctx_g = Ctx {
        homes: dctx.homes[1..1 + n_g].to_vec(),
    };

    let (instrs_f, _) = lower(f, &ctx_f)?;
    let (instrs_g, _) = lower(g, &ctx_g)?;

    let f_is_nop = instrs_f.is_empty();
    let g_is_nop = instrs_g.is_empty();

    if f_is_nop && g_is_nop {
        return Ok((vec![], ctx.clone()));
    }

    let mut instrs = Vec::new();
    instrs.extend(aux_pi);
    instrs.extend(tag_pi);

    // Phase 1: f runs iff tag=0 → skip f iff tag=1
    if !f_is_nop {
        instrs.extend(emit_skip_block(tag_reg, &instrs_f, rb, temp, true));
    }

    // Phase 2: g runs iff tag=1 → skip g iff tag=0
    if !g_is_nop {
        instrs.extend(emit_skip_block(tag_reg, &instrs_g, rb, temp, false));
    }

    instrs.extend(tag_po);
    instrs.extend(aux_po);
    Ok((instrs, ctx.clone()))
}

// ── Marshal / unmarshal (M-C1 §2.7) ────────────────────────────────

/// Marshal a `Value` of a given type into `StateVals`, writing leaf `i`
/// to its canonical home: `regs[i]` for i < 8, `scratch[i-6]` for
/// 8 ≤ i < 14, `mem[i-14]` for 14 ≤ i < 14+MEM_WORDS.
/// All other state fields are zeroed.
pub fn marshal(v: &Value, ty: &Type) -> StateVals {
    let bytes = value_to_leaf_bytes(v, ty);
    let mut state = StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    for (i, &b) in bytes.iter().enumerate() {
        if i < NUM_REGS {
            state.regs[i] = b;
        } else if i < 14 {
            // Spill: leaf i → scratch[i - 6] (s2..s7, skipping s0/s1).
            state.scratch[i - 6] = b;
        } else {
            // Memory: leaf i → mem[i - 14].
            state.mem[i - 14] = b;
        }
    }
    state
}

/// Unmarshal a `StateVals` back to a `Value`, reading leaf `i` from
/// its canonical home: `regs[i]` for i < 8, `scratch[i-6]` for
/// 8 ≤ i < 14, `mem[i-14]` for 14 ≤ i < 14+MEM_WORDS.
pub fn unmarshal(s: &StateVals, ty: &Type) -> Value {
    let n = num_leaves(ty).expect("unmarshal: num_leaves");
    let bytes: Vec<u8> = (0..n)
        .map(|i| {
            if i < NUM_REGS {
                s.regs[i]
            } else if i < 14 {
                s.scratch[i - 6]
            } else {
                s.mem[i - 14]
            }
        })
        .collect();
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
/// Uses `system_step` on the 3-factor `((CpuState, MemState), BusMsg)` carrier
/// so that programs with memory-spill (MEXCH) are evaluated correctly.
/// MemState and BusMsg are initialized to zero at program entry; BusMsg is
/// projected away at the final comparison (Stage 1b-A).
pub fn run_on_cpu<E: std::fmt::Debug>(
    iso: &Iso,
    input: &Value,
    eval_fn: impl Fn(&Iso, Value, &mut u64) -> Result<Value, E>,
) -> Result<Value, CompileError> {
    let (in_ty, out_ty) = theseus_core::type_of(iso).map_err(|e| {
        CompileError::Deferred(format!("type error: {e:?}"))
    })?;

    let instrs = compile(iso)?;
    let rom = assemble(&instrs).map_err(|e| {
        CompileError::Deferred(format!("assemble failed: {e}"))
    })?;
    let budget = instrs.len();

    let state = marshal(input, &in_ty);

    // Run the CPU for exactly `budget` steps using eval(system_step)
    // on the 3-factor ((CpuState, MemState), BusMsg) carrier.
    let step_iso = crate::step::system_step(&rom);
    let mut cur = encode_system(&state);
    let mut fuel = 8_000_000_000u64;
    for _ in 0..budget {
        cur = eval_fn(&step_iso, cur, &mut fuel).map_err(|e| {
            CompileError::Deferred(format!("CPU eval failed: {e:?}"))
        })?;
    }
    let final_state = decode_system(&cur);

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
            state.regs.swap(rd as usize, rs as usize);
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
                state.regs.swap(rs as usize, rt as usize);
            }
        }
        Instr::Xchg { rd, sx } => {
            std::mem::swap(&mut state.regs[rd as usize], &mut state.scratch[sx as usize]);
        }
        Instr::Mexch { rd, addr } => {
            std::mem::swap(&mut state.regs[rd as usize], &mut state.mem[addr as usize]);
        }
        Instr::Swapbr { rd } => {
            std::mem::swap(&mut state.regs[rd as usize], &mut state.br);
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
/// A byte-level re-implementation of the ISA, used only as a *fast* oracle
/// for routine tests. It is **NOT the authoritative CPU model**: correctness
/// is defined against [`run_on_cpu`], which drives the real `step` core-iso
/// evaluator we energy-account. This function is pinned to that model by the
/// `*_real_cpu` tests in `tests/compile.rs` (each asserts real == eval; the
/// routine tests assert fast == eval). Do not treat a green `run_on_cpu_fast`
/// as acceptance on its own. The budget is `instrs.len()` steps (worst-case
/// trace length; after the ROM ends the machine idles).
pub fn run_on_cpu_fast(
    iso: &Iso,
    input: &Value,
) -> Result<Value, CompileError> {
    let (in_ty, out_ty) = theseus_core::type_of(iso).map_err(|e| {
        CompileError::Deferred(format!("type error: {e:?}"))
    })?;

    let instrs = compile(iso)?;
    let rom = assemble(&instrs).map_err(|e| {
        CompileError::Deferred(format!("assemble failed: {e}"))
    })?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use theseus_core::prim::PrimDef;
    use theseus_core::{Type, Value};

    /// Build a swap PrimDef on Bit×Bit: (a,b)↦(b,a).
    fn swap_prim_def() -> PrimDef {
        let bit = Type::sum(Type::One, Type::One);
        let ty = Type::prod(bit.clone(), bit.clone());
        let vals = theseus_core::prim::enumerate_values(&ty);
        let mut forward = BTreeMap::new();
        let mut backward = BTreeMap::new();
        for v in &vals {
            let Value::Pair(a, b) = v else { panic!() };
            let swapped = Value::pair(b.as_ref().clone(), a.as_ref().clone());
            forward.insert(v.clone(), swapped.clone());
            backward.insert(swapped, v.clone());
        }
        PrimDef {
            forward_name: "swap".into(),
            backward_name: "swap".into(),
            input_ty: ty.clone(),
            output_ty: ty,
            forward,
            backward,
        }
    }

    #[test]
    fn recognize_prim_aliased_registers() {
        let def = swap_prim_def();
        // Craft a context where two leaves alias the same register.
        let ctx = Ctx {
            homes: vec![Location::Reg(0), Location::Reg(0)],
        };
        let result = recognize_prim(&def, &ctx);
        assert!(
            matches!(result, Err(CompileError::AliasedRegisters { .. })),
            "expected AliasedRegisters, got {result:?}"
        );
    }
}
