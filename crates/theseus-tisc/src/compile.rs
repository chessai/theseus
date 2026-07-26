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
use crate::bveq;
use crate::types::{decode_system, encode_system, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

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
    /// Resident array leaves are not at the expected memory locations.
    /// The arr leaves must home at [Mem(0), Mem(1), …, Mem(n-1)] in order;
    /// a composition that shifts them would silently corrupt execution.
    ResidentMisplaced(String),
    /// The bveq emission verifier rejected an instruction sequence that
    /// enumeration accepted — a real emission bug (transposed operands,
    /// wrong arm). Carries a human-readable description of the failure.
    EmissionVerificationFailed(String),
    /// The general BDD synthesizer (the prim-lowering fall-through) could lower
    /// this prim in principle, but the machine does not have enough free operand
    /// words for the ancilla + BDD node-lines it needs. This is a GRACEFUL
    /// fall-through (widen-state / provisioning lands later — A-6.4), NOT a
    /// verification failure. Carries the required vs. available word counts.
    NeedsProvisioning {
        /// Human-readable prim name (for diagnostics).
        prim: String,
        /// Total working operand words the synthesis requires
        /// (carriers + ancilla + temp + node-lines).
        required: usize,
        /// Legal operand words available on the current machine.
        available: usize,
    },
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
            CompileError::ResidentMisplaced(msg) => {
                write!(f, "resident array misplaced: {msg}")
            }
            CompileError::EmissionVerificationFailed(msg) => {
                write!(f, "emission verification failed: {msg}")
            }
            CompileError::NeedsProvisioning {
                prim,
                required,
                available,
            } => write!(
                f,
                "prim '{prim}' needs general-synth provisioning: requires {required} operand \
                 words but only {available} are free (widen-state / A-6.4)"
            ),
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
        Type::Flat(n) => {
            if (1..=8).contains(n) {
                Ok(1)
            } else {
                Err(CompileError::Deferred(format!(
                    "Flat({n}): width must be 1..=8"
                )))
            }
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
        Type::Flat(n) => {
            let byte = bytes[0];
            assert!(
                (byte as u64) < (1u64 << n),
                "value_from_leaf_bytes: byte {byte} out of range for Flat({n})"
            );
            // Build Right^byte(Left(Unit))
            let mut v = Value::left(Value::Unit);
            for _ in 0..byte {
                v = Value::right(v);
            }
            (v, 1)
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
        Type::Flat(n) => {
            let byte = leaf_value_to_u8(v);
            assert!(
                (byte as u64) < (1u64 << n),
                "value_to_leaf_bytes: byte {byte} out of range for Flat({n})"
            );
            vec![byte]
        }
    }
}

// ── F7: structural-renaming identity guard ──────────────────────────

/// Apply AssocRSum semantics: (a+b)+c → a+(b+c).
fn apply_assoc_r_sum(v: &Value) -> Value {
    match v {
        Value::Left(inner) => match inner.as_ref() {
            Value::Left(x) => Value::left(x.as_ref().clone()),
            Value::Right(y) => Value::right(Value::left(y.as_ref().clone())),
            _ => panic!("apply_assoc_r_sum: expected Left/Right inside Left"),
        },
        Value::Right(z) => Value::right(Value::right(z.as_ref().clone())),
        _ => panic!("apply_assoc_r_sum: expected Left/Right"),
    }
}

/// Apply AssocLSum semantics: a+(b+c) → (a+b)+c.
fn apply_assoc_l_sum(v: &Value) -> Value {
    match v {
        Value::Left(x) => Value::left(Value::left(x.as_ref().clone())),
        Value::Right(inner) => match inner.as_ref() {
            Value::Left(y) => Value::left(Value::right(y.as_ref().clone())),
            Value::Right(z) => Value::right(z.as_ref().clone()),
            _ => panic!("apply_assoc_l_sum: expected Left/Right inside Right"),
        },
        _ => panic!("apply_assoc_l_sum: expected Left/Right"),
    }
}

/// Apply UniteSum semantics: 0+a → a.
fn apply_unite_sum(v: &Value) -> Value {
    match v {
        Value::Right(x) => x.as_ref().clone(),
        // Left branch is uninhabited (Zero), so unreachable.
        _ => panic!("apply_unite_sum: expected Right (Zero+a, Left is uninhabited)"),
    }
}

/// Apply UnitiSum semantics: a → 0+a.
fn apply_uniti_sum(v: &Value) -> Value {
    Value::right(v.clone())
}

/// Apply Distrib semantics: a × (b+c) → (a×b) + (a×c).
fn apply_distrib(v: &Value) -> Value {
    let Value::Pair(a, bc) = v else {
        panic!("apply_distrib: expected Pair");
    };
    match bc.as_ref() {
        Value::Left(b) => Value::left(Value::pair(a.as_ref().clone(), b.as_ref().clone())),
        Value::Right(c) => Value::right(Value::pair(a.as_ref().clone(), c.as_ref().clone())),
        _ => panic!("apply_distrib: expected Left/Right in sum component"),
    }
}

/// Apply Factor semantics: (a×b) + (a×c) → a × (b+c).
fn apply_factor(v: &Value) -> Value {
    match v {
        Value::Left(ab) => {
            let Value::Pair(a, b) = ab.as_ref() else {
                panic!("apply_factor: expected Pair in Left");
            };
            Value::pair(a.as_ref().clone(), Value::left(b.as_ref().clone()))
        }
        Value::Right(ac) => {
            let Value::Pair(a, c) = ac.as_ref() else {
                panic!("apply_factor: expected Pair in Right");
            };
            Value::pair(a.as_ref().clone(), Value::right(c.as_ref().clone()))
        }
        _ => panic!("apply_factor: expected Left/Right"),
    }
}

/// Check whether a structural iso is the identity on the M-B leaf-byte
/// encoding. Returns `true` if every inhabitant maps to the same byte
/// vector (i.e. the no-op lowering is correct), `false` otherwise.
fn structural_iso_is_identity(in_ty: &Type, out_ty: &Type, transform: fn(&Value) -> Value) -> bool {
    let vals = enumerate_values(in_ty);
    for v_in in &vals {
        let v_out = transform(v_in);
        let bytes_in = value_to_leaf_bytes(v_in, in_ty);
        let bytes_out = value_to_leaf_bytes(&v_out, out_ty);
        // Zero-pad to the same length for comparison.
        let len = bytes_in.len().max(bytes_out.len());
        let mut padded_in = bytes_in;
        let mut padded_out = bytes_out;
        padded_in.resize(len, 0);
        padded_out.resize(len, 0);
        if padded_in != padded_out {
            return false;
        }
    }
    true
}

// ── Location and constants (M-C1, ADR-0044 §2) ─────────────────────

/// Where a leaf value resides: register, scratch word, or memory word.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Location {
    Reg(u8),
    Scratch(u8),
    Mem(u8),
}

/// Whole-machine occupancy bitmask: one bit per physical location.
/// Used by `allocate_aux` to determine which locations are genuinely
/// free (zero from marshalling) vs. occupied by a leaf outside the
/// current sub-context.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OccSet {
    pub regs: u8,
    pub scratch: u8,
    pub mem: u32,
}

impl OccSet {
    fn empty() -> Self {
        OccSet {
            regs: 0,
            scratch: 0,
            mem: 0,
        }
    }

    pub fn from_locations(locs: &[Location]) -> Self {
        let mut occ = Self::empty();
        for loc in locs {
            occ.set(*loc);
        }
        occ
    }

    fn is_free(&self, loc: Location) -> bool {
        match loc {
            Location::Reg(r) => self.regs & (1 << r) == 0,
            Location::Scratch(s) => self.scratch & (1 << s) == 0,
            Location::Mem(a) => self.mem & (1 << a) == 0,
        }
    }

    fn set(&mut self, loc: Location) {
        match loc {
            Location::Reg(r) => self.regs |= 1 << r,
            Location::Scratch(s) => self.scratch |= 1 << s,
            Location::Mem(a) => self.mem |= 1 << a,
        }
    }

    fn clear(&mut self, loc: Location) {
        match loc {
            Location::Reg(r) => self.regs &= !(1 << r),
            Location::Scratch(s) => self.scratch &= !(1 << s),
            Location::Mem(a) => self.mem &= !(1 << a),
        }
    }
}

/// Maximum leaves for straight-line programs (8 regs + s2..s7 + MEM_WORDS mem = 14+N).
const MAX_LEAVES: usize = 14 + MEM_WORDS;

/// Maximum leaves for branching programs (need 2 free-zero cells for aux
/// zero-source, ADR-0044 §2.4 / ADR-0046 §3.1).
const MAX_LEAVES_BRANCHING: usize = 12 + MEM_WORDS;

/// Maximum payload leaves for loop-carrier bodies (M-D.d §3, M-D.e slice 1).
/// 4 dedicated aux (rc, rb, temp, ctrl) + 1 tag register = 5 occupied;
/// with 8 registers that leaves ≤ 3 payload leaves register-resident.
/// The 4th payload leaf spills to Scratch(SCRATCH_SPILL_BASE) via the
/// self-inverse exchange bracket in `with_registers` (M-D.e: one
/// scratch-spilled zero-init padding leaf; live-input spill still rejected
/// by the `n_input+1 ≤ carrier_reg_budget` guard in `lower_trace`).
const MAX_LEAVES_LOOP: usize = 4;

/// Number of overhead instructions per loop iteration (Ltop to Lend,
/// inclusive of both SWAPBRs). 5 before body + 5 after body = 10.
const LOOP_OVERHEAD: usize = 10;

/// First spillable scratch index (s0/s1 are decoder-reserved).
const SCRATCH_SPILL_BASE: u8 = 2;

// ── Placement (sole layout function — F-5) ──────────────────────────

/// The SOLE placement function for leaf→location mapping.
/// Trailing `resident` leaves [n-resident, n) → Mem(leaf - (n - resident)).
/// Head leaves [0, n-resident) → Reg(0..8) then Scratch(2..8) by the existing rule.
/// Marshal, unmarshal, and Ctx MUST all call this — no re-implemented layout (F-5).
fn home_of(leaf: usize, n_leaves: usize, resident: usize) -> Location {
    let head = n_leaves - resident;
    if leaf >= head {
        // Trailing resident leaf → memory
        Location::Mem((leaf - head) as u8)
    } else if leaf < NUM_REGS {
        Location::Reg(leaf as u8)
    } else if leaf < 14 {
        // NOTE (M-D.e 3b): canonical leaf 8 maps here to Scratch(2) =
        // SCRATCH_SPILL_BASE — the same slot `lower_trace` pages a spilled
        // loop-carrier leaf to. Unreachable under Trace at MAX_LEAVES_LOOP=4
        // (would need n_input ≥ 9), but the collision is pinned here at the
        // producer as well as at the spill site in `lower_trace`.
        Location::Scratch((leaf - 6) as u8)
    } else {
        // This case should not be reachable if bounds are checked,
        // but for safety map to Mem (will be caught by bounds check).
        Location::Mem((leaf - 14) as u8)
    }
}

// ── Register allocation context ──────────────────────────────────────

/// Maps leaf positions to physical locations (register or scratch word).
/// Between every instruction the machine is in canonical layout — every
/// leaf sits in its home (ADR-0044 §2.1).
#[derive(Clone, Debug)]
pub struct Ctx {
    /// `homes[i]` = canonical physical location for leaf `i`.
    pub homes: Vec<Location>,
    /// Whole-machine occupancy: bits for every location holding live data.
    /// Copied unchanged across split/merge; updated only by displacement.
    pub occ: OccSet,
}

impl Ctx {
    /// Canonical home layout (ADR-0044 §2.1 + ADR-0046 §3.1):
    /// L < 8 → Reg(L); 8 ≤ L < 14 → Scratch(L − 6); 14 ≤ L < 14+N → Mem(L − 14).
    /// Delegates to `new_resident(n_leaves, 0)` — `home_of` is the sole
    /// placement function (I-3 / design §8(1)).
    fn new(n_leaves: usize) -> Self {
        Self::new_resident(n_leaves, 0)
    }

    /// Public canonical-layout constructor (for tests / external drivers that
    /// need to call `recognize_prim` directly with a fresh `n`-leaf context).
    pub fn new_canonical(n_leaves: usize) -> Self {
        Self::new(n_leaves)
    }

    /// Resident-aware home layout: trailing `resident` leaves map to Mem,
    /// head leaves use the existing Reg/Scratch rule. Seeds `occ` from
    /// homes (F-6: prevents allocate_aux from placing spills on live
    /// resident words). `resident == 0` reproduces `Ctx::new` bit-for-bit.
    fn new_resident(n_leaves: usize, resident: usize) -> Self {
        let homes: Vec<Location> = (0..n_leaves)
            .map(|i| home_of(i, n_leaves, resident))
            .collect();
        let occ = OccSet::from_locations(&homes);
        Ctx { homes, occ }
    }

    /// Split into two sub-contexts for `Times(f, g)`.
    fn split(&self, n_left: usize) -> (Ctx, Ctx) {
        let left = Ctx {
            homes: self.homes[..n_left].to_vec(),
            occ: self.occ,
        };
        let right = Ctx {
            homes: self.homes[n_left..].to_vec(),
            occ: self.occ,
        };
        (left, right)
    }

    /// Merge two sub-contexts back.
    fn merge(left: &Ctx, right: &Ctx) -> Ctx {
        debug_assert_eq!(
            left.occ, right.occ,
            "merge: both halves must share the same occ"
        );
        let mut homes = left.homes.clone();
        homes.extend_from_slice(&right.homes);
        Ctx {
            homes,
            occ: left.occ,
        }
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
        let spare = spares.pop().expect("with_registers: no spare register");
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
        (Location::Reg(r), Location::Scratch(sx)) | (Location::Scratch(sx), Location::Reg(r)) => {
            vec![Instr::Xchg { rd: r, sx }]
        }
        (Location::Reg(r), Location::Mem(addr)) | (Location::Mem(addr), Location::Reg(r)) => {
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
    // Use whole-machine occupancy to find genuinely free (zero) registers,
    // not just what this sub-context's homes list says.
    let free_regs: Vec<u8> = (0..NUM_REGS as u8)
        .filter(|r| ctx.occ.is_free(Location::Reg(*r)))
        .collect();

    if free_regs.len() >= count {
        return Ok((free_regs[..count].to_vec(), vec![], vec![], ctx.clone()));
    }

    // Zero-source: displace register-home leaves into free scratch OR
    // free memory slots (ADR-0046 §3.1 — unused marshalling-zeroed words).
    // Use whole-machine occupancy for free-slot detection.
    enum ZeroSlot {
        Scratch(u8),
        Mem(u8),
    }
    let mut free_slots: Vec<ZeroSlot> = Vec::new();
    for s in (SCRATCH_SPILL_BASE..NUM_SCRATCH as u8).rev() {
        if ctx.occ.is_free(Location::Scratch(s)) {
            free_slots.push(ZeroSlot::Scratch(s));
        }
    }
    for a in (0..MEM_WORDS as u8).rev() {
        if ctx.occ.is_free(Location::Mem(a)) {
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
    let mut occ = ctx.occ;

    for i in 0..needed {
        let (leaf_idx, reg) = displaceable[displaceable.len() - 1 - i];
        let slot = free_slots.pop().unwrap();
        match slot {
            ZeroSlot::Scratch(sx) => {
                page_in.push(Instr::Xchg { rd: reg, sx });
                page_out.push(Instr::Xchg { rd: reg, sx });
                modified_homes[leaf_idx] = Location::Scratch(sx);
                occ.clear(Location::Reg(reg));
                occ.set(Location::Scratch(sx));
            }
            ZeroSlot::Mem(addr) => {
                page_in.push(Instr::Mexch { rd: reg, addr });
                page_out.push(Instr::Mexch { rd: reg, addr });
                modified_homes[leaf_idx] = Location::Mem(addr);
                occ.clear(Location::Reg(reg));
                occ.set(Location::Mem(addr));
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
            occ,
        },
    ))
}

/// A-5: draw a Trace loop-level's aux frame `{rc, rb, temp, ctrl}` OCC-AWARELY.
///
/// The frame's four aux registers must be disjoint from (a) the loop carrier's
/// leaves, (b) any `Times` sibling's live registers, and (c) any outer loop
/// level's live aux (for nested Trace). We achieve all three by drawing the
/// frame from *genuinely-free* registers according to whole-machine occupancy
/// (`ctx.occ`), never the positional `max_carrier_reg + 1` — which was safe only
/// because the F-1 guard rejected every non-canonical/occupied ctx (M-D.d F-1).
///
/// Returns `(frame, marked_occ)` where `frame = [rc, rb, temp, ctrl]` and
/// `marked_occ` is `ctx.occ` with all four aux registers marked occupied — the
/// caller threads `marked_occ` into the body's `Ctx` so that an INNER Trace's
/// `allocate_aux_frame` sees this level's aux as taken and draws a DISJOINT
/// frame one level down. This is the "outer occ-marks its frame before lowering
/// the body" discipline (design §2.3.2).
///
/// The four aux are drawn as free registers only (never scratch/mem): all four
/// are live SWAPBR/CSWAP/ADDI operands across the whole loop bracket, so they
/// must be register-resident — this is why we reject rather than spill when the
/// register file cannot host a disjoint frame (design §2.3 depth bound).
///
/// **Byte-identity for the canonical single-level case.** `allocate_aux(ctx, 4,
/// carrier_leaves)` returns the four lowest free registers in ascending order.
/// For a canonical single-level Trace the carrier occupies registers
/// `0..=max_carrier_reg` contiguously from 0, so the four lowest free registers
/// are exactly `[max_carrier_reg+1, +2, +3, +4]` — byte-identical to the old
/// `aux_base = max_carrier_reg + 1` positional assignment. (We assert this in
/// `lower_trace` immediately after the call.)
fn allocate_aux_frame(ctx: &Ctx) -> Result<([u8; 4], OccSet), CompileError> {
    // Draw 4 free registers occ-awarely. We pass NO exclude_leaves and require
    // that the register file already has ≥ 4 free registers: the four aux are
    // all register-resident live operands for the loop's lifetime, so we do NOT
    // want allocate_aux's page-out-a-carrier-leaf-to-scratch fallback here (that
    // would relocate a live carrier leaf → the very corruption A-5 avoids).
    let free_regs: Vec<u8> = (0..NUM_REGS as u8)
        .filter(|r| ctx.occ.is_free(Location::Reg(*r)))
        .collect();
    if free_regs.len() < 4 {
        return Err(CompileError::Deferred(format!(
            "Trace aux frame needs 4 free registers but only {} are free \
             (carrier + sibling + outer-aux occupancy leaves no room — \
             nesting depth / carrier width bound, design §2.3)",
            free_regs.len()
        )));
    }
    let frame = [free_regs[0], free_regs[1], free_regs[2], free_regs[3]];
    let mut marked = ctx.occ;
    for r in frame {
        marked.set(Location::Reg(r));
    }
    Ok((frame, marked))
}

/// A-5 WHITE-BOX test hook: draw an occ-aware aux frame for a context built
/// from `homes` (leaf → register index) with `also_occupied` registers marked
/// live on top of the homes (models `Times` siblings / outer-level live aux).
///
/// Returns `(frame, marked_regs_bitmask)` where `frame = [rc, rb, temp, ctrl]`
/// and `marked_regs_bitmask` is the `regs` occupancy AFTER the frame is marked
/// (what the caller threads into a nested body). This lets the nested-aux tests
/// construct the `Times(Trace, g)` and nested-Trace aliasing scenarios directly
/// — the reject sites (A-4's job) still block these programs end-to-end, so the
/// frame machinery must be exercised at the allocator boundary (design §2.5).
pub fn alloc_aux_frame_for_test(
    homes: &[u8],
    also_occupied: &[u8],
) -> Result<([u8; 4], u8), CompileError> {
    let mut occ = OccSet::empty();
    for &r in homes {
        occ.set(Location::Reg(r));
    }
    for &r in also_occupied {
        occ.set(Location::Reg(r));
    }
    let ctx = Ctx {
        homes: homes.iter().map(|&r| Location::Reg(r)).collect(),
        occ,
    };
    let (frame, marked) = allocate_aux_frame(&ctx)?;
    Ok((frame, marked.regs))
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

/// Returns `true` when `instrs` contains a `Swapbr`, meaning the
/// lowered body itself uses a skip-block — doubly-nested control that
/// the current allocator cannot compile safely (reservation ordering
/// deferred).
fn has_inner_control(instrs: &[Instr]) -> bool {
    instrs.iter().any(|i| matches!(i, Instr::Swapbr { .. }))
}

fn emit_skip_block(
    ctrl_reg: u8,
    body_instrs: &[Instr],
    rb: u8,
    temp: u8,
    skip_when_set: bool,
) -> Result<Vec<Instr>, CompileError> {
    if body_instrs.len() > 255 {
        return Err(CompileError::Deferred(
            "skip-block body too long (>255 instructions)".into(),
        ));
    }
    let body_len = body_instrs.len() as u8;
    if body_len == 0 {
        return Ok(vec![]);
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

    Ok(out)
}

// ── BVEQ emission verifier (slice-2: decomposed check) ──────────────

/// Verify that a register-pure instruction sequence `body` computes the
/// prim defined by `def`, with operands homed in `operand_regs`.
///
/// The DECOMPOSED CHECK (design r2):
/// 1. `symbolic_exec(body)` — fail closed on non-register-pure / aliased / OOB.
/// 2. FRAME CLOSURE: non-operand registers unchanged (identity); operand
///    output bits depend only on operand-register input variables.
/// 3. POINTWISE over the table: every `(in, out)` row of `def.forward`
///    agrees with `eval_transfer` on the operand registers.
///
/// Returns `Ok(())` on success, `Err(CompileError::EmissionVerificationFailed)`
/// on mismatch. `operand_regs` MUST be TYPE-derived (from `regs`/`ctx.homes`),
/// NEVER candidate-derived (N5).
pub fn bveq_verify_emission(
    body: &[Instr],
    def: &PrimDef,
    operand_regs: &[u8],
) -> Result<(), CompileError> {
    // VACUITY GUARD: reject an empty/absent table BEFORE the pointwise loop.
    // An empty table means the `for (in,out) in &def.forward` loop below
    // returns Ok having verified NOTHING (the f3.5 vacuous-oracle class).
    if def.forward.is_empty() {
        return Err(CompileError::EmissionVerificationFailed(format!(
            "prim '{}': forward table is empty — cannot verify emission \
             (symbolic-only prims are not recognized in 3a)",
            def.forward_name
        )));
    }

    // Step 1: symbolic_exec — fail closed.
    let transfer = bveq::symbolic_exec(body).map_err(|e| {
        CompileError::EmissionVerificationFailed(format!(
            "symbolic_exec rejected body for '{}': {e}",
            def.forward_name
        ))
    })?;

    // Build the shared VarMap over the full register file.
    let map = bveq::VarMap::new(NUM_REGS).map_err(|e| {
        CompileError::EmissionVerificationFailed(format!(
            "VarMap construction failed for '{}': {e}",
            def.forward_name
        ))
    })?;

    // Collect the set of operand register indices for fast lookup.
    let operand_set: std::collections::BTreeSet<usize> =
        operand_regs.iter().map(|&r| r as usize).collect();

    // Collect BDD variable indices for operand registers (all bits).
    let operand_bdd_vars: std::collections::BTreeSet<bveq::biodivine_lib_bdd::BddVariable> = {
        let mut s = std::collections::BTreeSet::new();
        for &r in operand_regs {
            for bit in 0..bveq::BITS {
                s.insert(map.var(r as usize, bit));
            }
        }
        s
    };

    // Step 2: Frame closure.
    for reg in 0..NUM_REGS {
        for bit in 0..bveq::BITS {
            let bdd = bveq::expr_to_bdd(&transfer.regs[reg][bit], &map).map_err(|e| {
                CompileError::EmissionVerificationFailed(format!(
                    "expr_to_bdd failed for '{}' reg {} bit {}: {e}",
                    def.forward_name, reg, bit
                ))
            })?;

            if !operand_set.contains(&reg) {
                // Non-operand register: output must equal identity (its own input variable).
                let identity = map.bdd_vars().mk_var(map.var(reg, bit));
                if bdd != identity {
                    return Err(CompileError::EmissionVerificationFailed(format!(
                        "frame closure: non-operand reg {} bit {} is not identity for '{}'",
                        reg, bit, def.forward_name
                    )));
                }
            } else {
                // Operand register: support_set must be ⊆ operand variables.
                let support = bdd.support_set();
                for var in support.into_iter() {
                    if !operand_bdd_vars.contains(&var) {
                        return Err(CompileError::EmissionVerificationFailed(format!(
                            "frame closure: operand reg {} bit {} depends on non-operand \
                             variable for '{}'",
                            reg, bit, def.forward_name
                        )));
                    }
                }
            }
        }
    }

    // Step 3: Pointwise over the table.
    for (in_val, out_val) in &def.forward {
        let in_bytes = value_to_leaf_bytes(in_val, &def.input_ty);
        let out_bytes = value_to_leaf_bytes(out_val, &def.output_ty);

        // Build input register state: operand regs get their leaf bytes,
        // all other regs are zero (they are unconstrained by step 2's closure,
        // so any fixed value works — zero is canonical).
        let mut input_regs = [0u8; NUM_REGS];
        for (leaf_idx, &r) in operand_regs.iter().enumerate() {
            if leaf_idx < in_bytes.len() {
                input_regs[r as usize] = in_bytes[leaf_idx];
            }
        }

        let result_regs = bveq::eval_transfer(&transfer, &input_regs);

        // Check operand registers against expected output.
        for (leaf_idx, &r) in operand_regs.iter().enumerate() {
            let expected = if leaf_idx < out_bytes.len() {
                out_bytes[leaf_idx]
            } else {
                0
            };
            if result_regs[r as usize] != expected {
                return Err(CompileError::EmissionVerificationFailed(format!(
                    "pointwise mismatch for '{}': input={:?}, reg {} (leaf {}): \
                     got {} expected {}",
                    def.forward_name, in_bytes, r, leaf_idx, result_regs[r as usize], expected
                )));
            }
        }
    }

    Ok(())
}

// ── Prim recognition ─────────────────────────────────────────────────

use theseus_core::prim::{enumerate_values, PrimDef};

/// Try to recognize a PrimDef as a known TISC-1 instruction pattern.
/// All register accesses are routed through `with_registers` so that
/// scratch-resident operands are paged in/out correctly.
pub fn recognize_prim(def: &PrimDef, ctx: &Ctx) -> Result<Vec<Instr>, CompileError> {
    // HANG GUARD (F2-hardened): for symbolic prims whose table does NOT
    // cover the full domain, use SELECTION-BY-EQUIV (3b) instead of
    // table-based pattern matching. NO enumerate_values call.
    if def.symbolic.is_some() {
        let card = theseus_core::enumerate::cardinality_u64(&def.input_ty);
        if def.forward.len() as u64 != card {
            // Selection-by-equiv: iterate the finite arm library,
            // symbolic_exec each candidate, equiv vs the spec RegTransfer.
            // First pass wins. Sound by construction: a wrong selection
            // cannot pass equiv. Bounded: ~arm-library-size × ~1ms.
            return selection_by_equiv(def, ctx);
        }
    }

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
                let body = vec![Instr::Add {
                    rd: regs[1],
                    rs: regs[0],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // subFrom: (a, b) ↦ (a, b - a mod 256)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (a, b.wrapping_sub(a))) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Sub {
                    rd: regs[1],
                    rs: regs[0],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // xorInto: (a, b) ↦ (a, a ^ b)
        if check_pattern_2reg(&def.forward, &vals, |a, b| (a, a ^ b)) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Xor {
                    rd: regs[1],
                    rs: regs[0],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
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
                let body = vec![Instr::Exch {
                    rd: regs[0],
                    rs: regs[1],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // rotl: (a, b) ↦ (a, rotl(b, a & 7))
        if check_pattern_2reg(&def.forward, &vals, |a, b| {
            (a, b.rotate_left((a & 7) as u32))
        }) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Rotl {
                    rd: regs[1],
                    rs: regs[0],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // rotr: (a, b) ↦ (a, rotr(b, a & 7))
        if check_pattern_2reg(&def.forward, &vals, |a, b| {
            (a, b.rotate_right((a & 7) as u32))
        }) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Rotr {
                    rd: regs[1],
                    rs: regs[0],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // ── Leaf-byte-level patterns (works for sum types too) ──────
        //
        // The patterns above require Value::Pair (product types). The
        // patterns below use the generic leaf-byte encoding and work for
        // any 2-leaf type (including sum types like Sum(Bit, One) used
        // as Trace carriers).

        // 3-cycle forward: (a, b) ↦ (a⊕b⊕1, a)
        // Instruction sequence: XOR r0,r1; XORI r0,1; XOR r1,r0; XORI r1,1
        // Used as the Trace body witness for the k=|u|+1 maximal trajectory.
        if check_pattern_2leaf(
            &def.forward,
            &vals,
            &def.input_ty,
            &def.output_ty,
            |a, b| (a ^ b ^ 1, a),
        ) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![
                    Instr::Xor {
                        rd: regs[0],
                        rs: regs[1],
                    },
                    Instr::Xori {
                        rd: regs[0],
                        imm: 1,
                    },
                    Instr::Xor {
                        rd: regs[1],
                        rs: regs[0],
                    },
                    Instr::Xori {
                        rd: regs[1],
                        imm: 1,
                    },
                ];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // 3-cycle inverse (adjoint): (a, b) ↦ (b, a⊕b⊕1)
        if check_pattern_2leaf(
            &def.forward,
            &vals,
            &def.input_ty,
            &def.output_ty,
            |a, b| (b, a ^ b ^ 1),
        ) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![
                    Instr::Xor {
                        rd: regs[1],
                        rs: regs[0],
                    },
                    Instr::Xori {
                        rd: regs[1],
                        imm: 1,
                    },
                    Instr::Xor {
                        rd: regs[0],
                        rs: regs[1],
                    },
                    Instr::Xori {
                        rd: regs[0],
                        imm: 1,
                    },
                ];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
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
                let body = vec![Instr::Cswap {
                    rd: regs[0],
                    rs: regs[1],
                    rt: regs[2],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // ccnot (byte-parallel): (t, c1, c2) ↦ (t ^ (c1 & c2), c1, c2)
        // TARGET-FIRST: regs[0] = target (rd), regs[1] = control1 (rs), regs[2] = control2 (rt).
        if check_pattern_3reg(&def.forward, &vals, |t, c1, c2| (t ^ (c1 & c2), c1, c2)) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Ccnot {
                    rd: regs[0],
                    rs: regs[1],
                    rt: regs[2],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // ccnot TARGET-LAST: (c1, c2, t) ↦ (c1, c2, t ^ (c1 & c2))
        if check_pattern_3reg(&def.forward, &vals, |c1, c2, t| (c1, c2, t ^ (c1 & c2))) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Ccnot {
                    rd: regs[2],
                    rs: regs[0],
                    rt: regs[1],
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }
    }

    // One-register prims: a ↦ f(a)
    if n_in == 1 && n_out == 1 {
        let vals = enumerate_values(&def.input_ty);

        // addi_K: a ↦ a + K
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a.wrapping_add(k)) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Addi {
                    rd: regs[0],
                    imm: k,
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // subi_K: a ↦ a - K
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a.wrapping_sub(k)) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Subi {
                    rd: regs[0],
                    imm: k,
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }

        // xori_K: a ↦ a ^ K
        if let Some(k) = find_constant_1reg(&def.forward, &vals, |a, k| a ^ k) {
            return with_registers(ctx, &leaves, |regs| {
                let body = vec![Instr::Xori {
                    rd: regs[0],
                    imm: k,
                }];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }
    }

    // ── memexch: (d, a, arr[0..n]) ↦ (arr[a], a, arr[a←d]) ────────────
    // Fast pre-filter: name == "memexch" and shape is 2 head + n resident leaves.
    if def.forward_name == "memexch" && n_in >= 3 && n_in == n_out {
        let n_arr = n_in - 2; // number of array elements

        // F-3: SEMANTIC VERIFY — check every input against memexch semantics.
        let all_vals = enumerate_values(&def.input_ty);
        for v in &all_vals {
            let in_bytes = value_to_leaf_bytes(v, &def.input_ty);
            let d = in_bytes[0];
            let a = in_bytes[1];
            let idx = (a as usize) % MEM_WORDS; // CPU's rule

            // Expected output: (arr[idx], a, arr[idx ← d])
            let mut exp_bytes = in_bytes.clone();
            if idx < n_arr {
                exp_bytes[0] = in_bytes[2 + idx];
                exp_bytes[2 + idx] = d;
            } else {
                // Index outside array — table cannot match memexch semantics
                return Err(CompileError::UnrecognizedPrim(
                    "memexch semantic verify: index out of array bounds".into(),
                ));
            }

            let Some(out) = def.forward.get(v) else {
                return Err(CompileError::UnrecognizedPrim(
                    "memexch semantic verify: missing forward entry".into(),
                ));
            };
            let out_bytes = value_to_leaf_bytes(out, &def.output_ty);
            if out_bytes != exp_bytes {
                return Err(CompileError::UnrecognizedPrim(
                    "memexch semantic verify: table mismatch".into(),
                ));
            }
        }

        // F-2: HOME-LAYOUT GUARD — arr leaves must be at [Mem(0), ..., Mem(n_arr-1)] in order.
        for j in 0..n_arr {
            let leaf_idx = 2 + j; // arr leaves start at position 2
            if leaf_idx >= ctx.homes.len() || ctx.homes[leaf_idx] != Location::Mem(j as u8) {
                return Err(CompileError::ResidentMisplaced(format!(
                    "arr leaf {} must be at Mem({}) but is at {:?}",
                    j,
                    j,
                    ctx.homes.get(leaf_idx)
                )));
            }
        }

        // EMIT: page data (leaf 0) and addr (leaf 1) into registers.
        return with_registers(ctx, &[0, 1], |regs| {
            let rd = regs[0]; // data register
            let ra = regs[1]; // address register
            if rd == ra {
                return Err(CompileError::AliasedRegisters {
                    instr: "MEXCHR".into(),
                });
            }
            Ok(vec![Instr::MexchReg { rd, ra }])
        });
    }

    // ── M-D.e slice 1: 5-leaf pinned walk (k-leaf recognizer) ──────────
    // The 7-cycle "thermometer walk" on Sum(N5, Bit):
    //   R0 → L4 → L3 → L2 → L1 → L0 → R1 → R0
    // Swapbr-free 13-instruction sequence; touches ALL payload leaves
    // including the spilled leaf 4. Pinned recognizer: name-gate +
    // semantic gate (check_pattern_kleaf verifies the full forward table
    // vs the byte map over all canonical values — a mismatch rejects).
    // NOTE: check_pattern_kleaf enumerates the whole input type
    // (exponential in leaves) — acceptable for k=5 (7 values).
    if def.forward_name == "mde_spill_walk" && n_in == 5 && n_out == 5 {
        let vals = enumerate_values(&def.input_ty);
        if check_pattern_kleaf(
            &def.forward,
            &vals,
            &def.input_ty,
            &def.output_ty,
            5,
            mde_walk_bytes,
        ) {
            // Emit all 13 instructions in ONE `with_registers` call →
            // the paging is a WHOLE-BODY bracket (one page-in at entry,
            // one page-out at exit), NOT per-instruction.
            return with_registers(ctx, &leaves, |regs| {
                let (t, p1, p2, p3, p4) = (regs[0], regs[1], regs[2], regs[3], regs[4]);
                let body = vec![
                    Instr::Xor { rd: t, rs: p1 },
                    Instr::Xori { rd: t, imm: 1 },
                    Instr::Xori { rd: p1, imm: 1 },
                    Instr::Xor { rd: p2, rs: p3 },
                    Instr::Xor { rd: p3, rs: p4 },
                    Instr::Xori { rd: t, imm: 1 },
                    Instr::Cswap {
                        rd: t,
                        rs: p1,
                        rt: p2,
                    },
                    Instr::Cswap {
                        rd: t,
                        rs: p2,
                        rt: p3,
                    },
                    Instr::Cswap {
                        rd: t,
                        rs: p3,
                        rt: p4,
                    },
                    Instr::Xori { rd: t, imm: 1 },
                    Instr::Xor { rd: p3, rs: p4 },
                    Instr::Xor { rd: p2, rs: p3 },
                    Instr::Xor { rd: p1, rs: p2 },
                ];
                bveq_verify_emission(&body, def, regs)?;
                Ok(body)
            });
        }
    }

    // ── GENERAL FALL-THROUGH (A-6 STEP 1): no finite arm matched. ──────
    // Deterministically synthesize the prim's whole byte-map via the BDD
    // path (table → RegTransfer → Bennett). This retires the finite-arm
    // limitation: ANY admitted bijection prim that FITS the machine's free
    // operand budget lowers here. Prims too large fail gracefully with
    // `NeedsProvisioning` (widen-state lands later). The general synth carries
    // its OWN (whole-domain exec + ANF-differential) verification, so it does
    // NOT go through the `bveq_verify_emission` vacuity guard.
    general_synthesize_prim(def, ctx)
}

// ── General BDD prim-lowering fall-through (A-6 STEP 1) ─────────────────
//
// The reusable, prim-agnostic lowering that closes coverage gap (2): any
// admitted bijection `PrimDef` that fits the machine's free operand budget is
// lowered by DETERMINISTIC wiring + transcription (table → per-output-bit ANF →
// ROBDD → Bennett), NOT search. Large prims (e.g. gcd2, whose BDD needs 35 node
// lines) fall through gracefully with `NeedsProvisioning` — widen-state (A-6.4)
// funds them later.

/// Per-leaf meaningful bit-width for a type, in canonical leaf order — the
/// widths `table_to_regtransfer` needs. A `Sum` tag leaf is 1 bit; payload leaves
/// take the position-wise MAX of the two branches' widths (matching the
/// zero-padded tag+payload marshaling of `value_to_leaf_bytes`). `Flat(n)` is `n`
/// bits; `One`/`Zero` contribute no leaves.
fn leaf_widths(ty: &Type) -> Result<Vec<u8>, CompileError> {
    match ty {
        Type::Zero | Type::One => Ok(vec![]),
        Type::Flat(n) => {
            if (1..=8).contains(n) {
                Ok(vec![*n])
            } else {
                Err(CompileError::Deferred(format!(
                    "Flat({n}): width must be 1..=8"
                )))
            }
        }
        Type::Prod(a, b) => {
            let mut w = leaf_widths(a)?;
            w.extend(leaf_widths(b)?);
            Ok(w)
        }
        Type::Sum(a, b) => {
            let wa = leaf_widths(a)?;
            let wb = leaf_widths(b)?;
            // Tag leaf = 1 bit; payload leaves = position-wise max of the two
            // branches' widths (the shorter branch zero-pads).
            let payload = wa.len().max(wb.len());
            let mut w = Vec::with_capacity(1 + payload);
            w.push(1u8);
            for i in 0..payload {
                let ma = wa.get(i).copied().unwrap_or(0);
                let mb = wb.get(i).copied().unwrap_or(0);
                w.push(ma.max(mb));
            }
            Ok(w)
        }
    }
}

/// Build a byte-map closure `[u8;n_leaves] -> [u8;n_leaves]` from a prim table,
/// by round-tripping through the leaf-byte marshaling: reconstruct the input
/// `Value` from the carrier bytes, look it up in `table`, marshal the output back
/// to `n_leaves` bytes. Returns `None` (as a per-call closure result) when the
/// table has no entry — the caller guarantees the table is total over the domain
/// it enumerates (a cross-checked, admitted bijection), so a miss is a bug we
/// surface, not silently accept.
fn table_byte_map<'a>(
    table: &'a std::collections::BTreeMap<Value, Value>,
    in_ty: &'a Type,
    out_ty: &'a Type,
    n_leaves: usize,
) -> impl Fn(&[u8]) -> Vec<u8> + 'a {
    move |in_bytes: &[u8]| {
        let (v_in, _) = value_from_leaf_bytes(in_bytes, in_ty);
        let v_out = table
            .get(&v_in)
            .unwrap_or_else(|| panic!("table_byte_map: no entry for input {v_in:?}"));
        let mut out = value_to_leaf_bytes(v_out, out_ty);
        out.resize(n_leaves, 0);
        out
    }
}

/// The general prim-lowering fall-through: synthesize a reversible body for ANY
/// admitted bijection prim that fits the machine's free operand budget.
///
/// Contract (called from `recognize_prim` AFTER every finite arm misses, BEFORE
/// the `UnrecognizedPrim` reject):
///   1. Build leaf-space `fwd`/`bwd` `RegTransfer`s from `def.forward`/`backward`
///      via `table_to_regtransfer` (per-leaf widths from the types). CROSS-CHECK
///      `bwd∘fwd == id` over the domain (guards the Bennett `bwd = fwd⁻¹`
///      assumption).
///   2. Allocate register-resident carriers + free ancilla/temp/node-line words
///      from the machine's free operand budget (occupancy-aware).
///   3. If the budget is EXCEEDED → `NeedsProvisioning` (graceful fall-through).
///   4. Synthesize via `transcribe_bdd_bennett`.
///   5. VERIFY the emitted body (whole-domain `exec_instr`: carriers land at the
///      rule AND every ancilla/temp/node word is net-zero) AND differentially vs
///      the ANF path (`transcribe_bennett_general`). Fail-closed → `CompileError`.
fn general_synthesize_prim(def: &PrimDef, ctx: &Ctx) -> Result<Vec<Instr>, CompileError> {
    use crate::synth::{
        table_to_regtransfer, transcribe_bdd_bennett, transcribe_bennett_general, BddHelpers,
        HelperBank,
    };

    let n_in = num_leaves(&def.input_ty)?;
    let n_out = num_leaves(&def.output_ty)?;

    // The general path handles same-shape byte-permutation prims (n carriers in,
    // n carriers out). Differing leaf counts are a shape the Bennett wrapper
    // cannot carry; leave them to `UnrecognizedPrim`.
    if n_in == 0 || n_in != n_out {
        return Err(CompileError::UnrecognizedPrim(format!(
            "prim '{}' not recognized as a TISC-1 pattern (general synth: \
             requires n_in == n_out ≥ 1, got n_in={n_in} n_out={n_out})",
            def.forward_name
        )));
    }
    let n = n_in;

    // The table must be TOTAL over the input domain (a full-table bijection). A
    // symbolic-only / partial-table prim is handled by selection_by_equiv above;
    // reaching here with a partial table would make the byte-map closure panic.
    let card = theseus_core::enumerate::cardinality_u64(&def.input_ty);
    if def.forward.len() as u64 != card || def.backward.len() as u64 != card {
        return Err(CompileError::UnrecognizedPrim(format!(
            "prim '{}' not recognized as a TISC-1 pattern (general synth: \
             forward/backward tables must cover the full domain; have fwd={} bwd={} of {card})",
            def.forward_name,
            def.forward.len(),
            def.backward.len()
        )));
    }

    // Per-leaf widths from the TYPE (never from the table — N5 obligation).
    let widths = leaf_widths(&def.input_ty)?;
    if widths.len() != n {
        return Err(CompileError::UnrecognizedPrim(format!(
            "prim '{}' general synth: {} leaf widths for {n} leaves",
            def.forward_name,
            widths.len()
        )));
    }
    let total_bits: usize = widths.iter().map(|&w| w as usize).sum();
    // `table_to_regtransfer` enumerates 2^total_bits rows. Cap defensively (it
    // asserts ≤ 24 internally); a wide prim is a provisioning problem, not a
    // panic. Anything above the ANF budget belongs to a later slice.
    if total_bits > 20 {
        return Err(CompileError::NeedsProvisioning {
            prim: def.forward_name.clone(),
            required: 1usize << total_bits.min(63),
            available: 1usize << 20,
        });
    }

    // ── Step 1: leaf-space fwd/bwd RegTransfers + bwd∘fwd == id cross-check ──
    // Spec is built in LEAF SPACE (leaf i occupies symbolic register i); the
    // Bennett wrapper remaps leaf → physical carrier via `carrier_ops`.
    let spec_leaves: Vec<u8> = (0..n as u8).collect();
    let fwd_map = table_byte_map(&def.forward, &def.input_ty, &def.output_ty, n);
    let bwd_map = table_byte_map(&def.backward, &def.output_ty, &def.input_ty, n);

    // CROSS-CHECK bwd∘fwd == id over the WHOLE enumerated domain (guards the
    // Bennett `bwd = fwd⁻¹` assumption at construction time). Enumerate the same
    // 2^total_bits carrier-bit assignments `table_to_regtransfer` will.
    {
        let n_rows = 1usize << total_bits;
        // Flattened meaningful input bits, in carrier order (leaf, bit).
        let mut vars: Vec<(usize, usize)> = Vec::new();
        for (leaf, &w) in widths.iter().enumerate() {
            for bit in 0..w as usize {
                vars.push((leaf, bit));
            }
        }
        for x in 0..n_rows {
            let mut in_bytes = vec![0u8; n];
            for (k, &(leaf, bit)) in vars.iter().enumerate() {
                if (x >> k) & 1 == 1 {
                    in_bytes[leaf] |= 1 << bit;
                }
            }
            let out = fwd_map(&in_bytes);
            let back = bwd_map(&out);
            if back != in_bytes {
                return Err(CompileError::EmissionVerificationFailed(format!(
                    "general synth '{}': bwd∘fwd ≠ id on carrier bytes {in_bytes:?} \
                     (fwd→{out:?}, bwd→{back:?}); the backward table is not the inverse",
                    def.forward_name
                )));
            }
        }
    }

    let fwd = table_to_regtransfer(&fwd_map, &spec_leaves, &widths);
    let bwd = table_to_regtransfer(&bwd_map, &spec_leaves, &widths);

    // ── Node-line budget: max single-output-bit ROBDD node count over fwd+bwd ──
    let map = bveq::VarMap::new(NUM_REGS).map_err(|e| {
        CompileError::EmissionVerificationFailed(format!(
            "general synth '{}': VarMap: {e}",
            def.forward_name
        ))
    })?;
    let mut node_lines = 0usize;
    for t in [&fwd, &bwd] {
        for row in &t.regs {
            for e in row {
                let bdd = bveq::expr_to_bdd(e, &map).map_err(|e| {
                    CompileError::EmissionVerificationFailed(format!(
                        "general synth '{}': expr_to_bdd: {e}",
                        def.forward_name
                    ))
                })?;
                let root = bdd.root_pointer();
                let nodes = if root.is_zero() || root.is_one() {
                    0
                } else {
                    bdd.size() - 2
                };
                node_lines = node_lines.max(nodes);
            }
        }
    }

    // ── Step 2: allocate carriers (register-resident) + free operand words ──
    // Carriers must be register-home for the general path (small prims); a
    // scratch/mem-home carrier means the prim is too big to sit in registers →
    // provisioning. Collect carrier register indices from `ctx.homes`.
    let mut carrier_ops: Vec<u8> = Vec::with_capacity(n);
    for leaf in 0..n {
        match ctx.homes.get(leaf) {
            Some(Location::Reg(r)) => carrier_ops.push(*r),
            _ => {
                // A non-register carrier home → out of the general path's small
                // fast fit; report as provisioning (needs widen/paging support).
                return Err(CompileError::NeedsProvisioning {
                    prim: def.forward_name.clone(),
                    required: n + n + 1 + node_lines,
                    available: free_operand_words(ctx),
                });
            }
        }
    }

    // Free operand words: registers NOT holding carriers/live data, plus legal
    // scratch (s2..s7 = operands 10..15; s0/s1 are decoder-reserved). Use
    // whole-machine occupancy so we never place a helper on a live word.
    let carrier_set: std::collections::BTreeSet<u8> = carrier_ops.iter().copied().collect();
    let mut free: Vec<u8> = Vec::new();
    for r in 0..NUM_REGS as u8 {
        if !carrier_set.contains(&r) && ctx.occ.is_free(Location::Reg(r)) {
            free.push(r);
        }
    }
    for s in SCRATCH_SPILL_BASE..NUM_SCRATCH as u8 {
        if ctx.occ.is_free(Location::Scratch(s)) {
            // Operand index of scratch word s is NUM_REGS + s.
            free.push(NUM_REGS as u8 + s);
        }
    }

    // Working words needed beyond carriers: n ancilla + 1 temp + node_lines.
    let need_aux = n + 1 + node_lines;
    if free.len() < need_aux {
        return Err(CompileError::NeedsProvisioning {
            prim: def.forward_name.clone(),
            required: n + need_aux, // carriers + aux
            available: n + free.len(),
        });
    }
    let ancilla: Vec<u8> = free[..n].to_vec();
    let temp: u8 = free[n];
    let nodes: Vec<u8> = free[n + 1..n + 1 + node_lines].to_vec();

    // ── Step 4: synthesize the reversible Bennett body via the BDD path. ──
    let helpers = BddHelpers {
        temp,
        nodes: &nodes,
    };
    let body = transcribe_bdd_bennett(&fwd, &bwd, &carrier_ops, &ancilla, helpers).map_err(|e| {
        CompileError::EmissionVerificationFailed(format!(
            "general synth '{}': transcribe_bdd_bennett: {e}",
            def.forward_name
        ))
    })?;

    // ── Step 5: VERIFY before returning. Fail-closed. ──
    // Working words that MUST be net-zero after the body: ancilla, temp, nodes.
    let mut aux_words: Vec<u8> = ancilla.clone();
    aux_words.push(temp);
    aux_words.extend_from_slice(&nodes);

    // (a) Whole-domain exec: carriers land at fwd(x) AND every aux word is 0.
    verify_general_body_exec(&body, def, &carrier_ops, &widths, &aux_words).map_err(|msg| {
        CompileError::EmissionVerificationFailed(format!(
            "general synth '{}': BDD body exec-verify failed: {msg}",
            def.forward_name
        ))
    })?;

    // (b) Differential vs the ANF path: synthesize the SAME prim through the ANF
    // transcriber and assert both bodies agree on the whole domain (carriers +
    // net-zero). This is an independent second synthesis mechanism, so an
    // agreement is strong evidence neither path is wrong the same way.
    {
        // ANF needs its own helper bank (mask + AND-tree temps). Reuse the free
        // pool: mask + temps from the words not used as ancilla. If the ANF
        // helper bank does not fit the free budget, SKIP the differential (the
        // primary exec-verify already certified the BDD body); this only happens
        // for prims whose ANF is much wider than the BDD, which is exactly the
        // case the BDD path exists to handle.
        let anf_free: Vec<u8> = free[n..].to_vec(); // all non-ancilla free words
        if let Some((mask, temps)) = anf_helper_bank(&anf_free) {
            let anf_helpers = HelperBank {
                mask,
                temps: &temps,
            };
            match transcribe_bennett_general(&fwd, &bwd, &carrier_ops, &ancilla, anf_helpers) {
                Ok(anf_body) => {
                    let mut anf_aux: Vec<u8> = ancilla.clone();
                    anf_aux.push(mask);
                    anf_aux.extend_from_slice(&temps);
                    verify_general_body_exec(&anf_body, def, &carrier_ops, &widths, &anf_aux)
                        .map_err(|msg| {
                            CompileError::EmissionVerificationFailed(format!(
                                "general synth '{}': ANF differential body failed exec-verify: \
                                 {msg}",
                                def.forward_name
                            ))
                        })?;
                    // Both bodies exec-verified against the SAME table over the
                    // full domain → they agree. (Comparing carrier outputs
                    // instr-by-instr is unnecessary: both were checked === table.)
                }
                Err(_) => {
                    // ANF path could not be wired within the free bank — the BDD
                    // exec-verify stands alone.
                }
            }
        }
    }

    Ok(body)
}

/// Count legal free operand words on the machine for `ctx` (free registers +
/// legal scratch s2..s7). Used only for `NeedsProvisioning` diagnostics.
fn free_operand_words(ctx: &Ctx) -> usize {
    let mut n = 0usize;
    for r in 0..NUM_REGS as u8 {
        if ctx.occ.is_free(Location::Reg(r)) {
            n += 1;
        }
    }
    for s in SCRATCH_SPILL_BASE..NUM_SCRATCH as u8 {
        if ctx.occ.is_free(Location::Scratch(s)) {
            n += 1;
        }
    }
    n
}

/// Split a free-word pool into an ANF helper bank `(mask, temps)` — needs ≥ 2
/// words (1 mask + ≥1 temp). Returns `None` if too few words remain.
fn anf_helper_bank(free: &[u8]) -> Option<(u8, Vec<u8>)> {
    if free.len() < 2 {
        return None;
    }
    Some((free[0], free[1..].to_vec()))
}

/// Whole-domain exec-verify of a synthesized reversible body: for every input in
/// `def.forward`, load carriers, run the body through `exec_instr`, then assert
/// (a) each carrier register holds the rule's output byte, and (b) every aux word
/// (ancilla + helpers + node lines) is restored to 0 (Bennett net-zero).
///
/// Complete for the small prims the general path fits: it enumerates the whole
/// table, so a wrong emission is caught on some input. `widths` bound the
/// meaningful output bits per carrier (Flat(w) zero-extension).
fn verify_general_body_exec(
    body: &[Instr],
    def: &PrimDef,
    carrier_ops: &[u8],
    widths: &[u8],
    aux_words: &[u8],
) -> Result<(), String> {
    let n = carrier_ops.len();
    for (in_val, out_val) in &def.forward {
        let in_bytes = value_to_leaf_bytes(in_val, &def.input_ty);
        let out_bytes = value_to_leaf_bytes(out_val, &def.output_ty);

        let mut st = StateVals {
            regs: [0u8; NUM_REGS],
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0u8; NUM_SCRATCH],
            mem: [0u8; MEM_WORDS],
        };
        for i in 0..n {
            let b = in_bytes.get(i).copied().unwrap_or(0);
            st.operand_set(carrier_ops[i], b);
        }
        // Aux words must start at 0.
        for &w in aux_words {
            if st.operand_get(w) != 0 {
                return Err(format!("aux word {w} not zero at entry"));
            }
        }
        for &ins in body {
            exec_instr(ins, &mut st);
        }
        // Carriers land at the rule's output (compare only meaningful bits).
        for i in 0..n {
            let want = out_bytes.get(i).copied().unwrap_or(0);
            let got = st.operand_get(carrier_ops[i]);
            let w = widths.get(i).copied().unwrap_or(8) as u32;
            let mask: u8 = if w >= 8 { 0xFF } else { (1u8 << w) - 1 };
            if (got & mask) != (want & mask) {
                return Err(format!(
                    "carrier {i} (op {}) mismatch on input {in_bytes:?}: got {got:#04x} \
                     want {want:#04x} (mask {mask:#04x})",
                    carrier_ops[i]
                ));
            }
            // Above-width bits must be zero (Flat(w) zero-extension).
            if (got & !mask) != 0 {
                return Err(format!(
                    "carrier {i} (op {}) has spurious high bits {:#04x} on input {in_bytes:?}",
                    carrier_ops[i],
                    got & !mask
                ));
            }
        }
        // Bennett net-zero: every aux word restored to 0.
        for &w in aux_words {
            let v = st.operand_get(w);
            if v != 0 {
                return Err(format!(
                    "aux word {w} not net-zero (={v:#04x}) after body on input {in_bytes:?}"
                ));
            }
        }
    }
    Ok(())
}

// ── Selection-by-equiv (3b: symbolic prim recognition) ─────────────────

/// Recognize a symbolic-spec prim by iterating the finite arm library and
/// finding a candidate instruction body that provably computes the spec.
///
/// For each candidate body: `symbolic_exec(body)` → `RegTransfer`, then
/// `equiv` against the spec-derived `RegTransfer`. First pass wins (sound
/// by construction: equiv is canonical and complete). If no candidate
/// verifies, returns `Deferred` (no enumeration, bounded).
///
/// ADMIT-FIRST CONTRACT (impl-review E-1): for a partial-table+spec prim,
/// selection compares the emitted body only against the SPEC (via `equiv`),
/// not against the partial table. `core::admit` (which every prim passes
/// through before it can be recognized) rejects a partial-table+spec prim
/// whose table and spec disagree (`SpecTableMismatch`) — so a recognized
/// symbolic prim is always one whose table (if any) is spec-consistent, and
/// eval's table-wins-else-spec path (interp.rs) cannot diverge from the
/// emitted code. This function assumes that contract: it does NOT itself
/// re-check the table against the spec.
fn selection_by_equiv(def: &PrimDef, ctx: &Ctx) -> Result<Vec<Instr>, CompileError> {
    let spec = def.symbolic.as_ref().unwrap();
    let n_in = num_leaves(&def.input_ty)?;
    let n_out = num_leaves(&def.output_ty)?;

    if n_in != n_out {
        return Err(CompileError::Deferred(format!(
            "prim '{}': symbolic selection requires n_in == n_out ({n_in} vs {n_out})",
            def.forward_name
        )));
    }

    let leaves: Vec<usize> = (0..n_in).collect();
    let w = spec.forward.width as usize;

    with_registers(ctx, &leaves, |regs| {
        // Build operand_regs (usize) for the spec→RegTransfer shim and equiv.
        let operand_regs: Vec<usize> = regs.iter().map(|&r| r as usize).collect();

        // Build the spec-derived RegTransfer (N5: operand_regs from the TYPE,
        // via with_registers' leaf homes — never from the candidate body).
        let spec_transfer = bveq::spec_to_reg_transfer(&spec.forward, &operand_regs);

        // Generate and try every candidate in the finite arm library.
        let candidates = generate_arm_candidates(n_in, regs);

        for body in &candidates {
            let exec_transfer = match bveq::symbolic_exec(body) {
                Ok(t) => t,
                Err(_) => continue, // skip ISA-invalid candidates (aliased regs, etc.)
            };

            if bveq::equiv(&exec_transfer, &spec_transfer, w, &operand_regs).is_ok() {
                // Sound by construction: equiv ensures body computes the spec.
                // Verification IS the selection (not bveq_verify_emission, which
                // is for table prims — the empty-table hard-reject stays intact).
                return Ok(body.clone());
            }
        }

        Err(CompileError::Deferred(format!(
            "prim '{}': no arm-library candidate matches the symbolic spec \
             ({} candidates tried)",
            def.forward_name,
            candidates.len()
        )))
    })
}

/// Generate the finite arm library of candidate instruction bodies for a
/// given arity and register assignment.
///
/// 2-op: Add/Sub/Xor/Exch/Rotl/Rotr (all rd/rs permutations) + 3-cycle bodies.
/// 3-op: Cswap/Ccnot (all rd/rs/rt permutations).
/// 1-op: Addi/Subi/Xori (imm 0..=255) + Rotli/Rotri (imm 0..=7).
fn generate_arm_candidates(n_leaves: usize, regs: &[u8]) -> Vec<Vec<Instr>> {
    let mut candidates = Vec::new();

    match n_leaves {
        1 => {
            let r = regs[0];
            for k in 0..=255u8 {
                candidates.push(vec![Instr::Addi { rd: r, imm: k }]);
                candidates.push(vec![Instr::Subi { rd: r, imm: k }]);
                candidates.push(vec![Instr::Xori { rd: r, imm: k }]);
            }
            // Rotli/Rotri: only imm & 7 matters, so 8 distinct values each.
            for k in 0..=7u8 {
                candidates.push(vec![Instr::Rotli { rd: r, imm: k }]);
                candidates.push(vec![Instr::Rotri { rd: r, imm: k }]);
            }
        }
        2 => {
            // All (rd, rs) permutations of the two registers.
            let perms: [(u8, u8); 2] = [(regs[0], regs[1]), (regs[1], regs[0])];
            for &(rd, rs) in &perms {
                // symbolic_exec rejects rd==rs, so skip (continue in the loop).
                candidates.push(vec![Instr::Add { rd, rs }]);
                candidates.push(vec![Instr::Sub { rd, rs }]);
                candidates.push(vec![Instr::Xor { rd, rs }]);
                candidates.push(vec![Instr::Rotl { rd, rs }]);
                candidates.push(vec![Instr::Rotr { rd, rs }]);
            }
            // Exch is symmetric; one ordering suffices.
            candidates.push(vec![Instr::Exch {
                rd: regs[0],
                rs: regs[1],
            }]);
            // 3-cycle multi-instruction bodies (Trace witness patterns).
            candidates.push(vec![
                Instr::Xor {
                    rd: regs[0],
                    rs: regs[1],
                },
                Instr::Xori {
                    rd: regs[0],
                    imm: 1,
                },
                Instr::Xor {
                    rd: regs[1],
                    rs: regs[0],
                },
                Instr::Xori {
                    rd: regs[1],
                    imm: 1,
                },
            ]);
            candidates.push(vec![
                Instr::Xor {
                    rd: regs[1],
                    rs: regs[0],
                },
                Instr::Xori {
                    rd: regs[1],
                    imm: 1,
                },
                Instr::Xor {
                    rd: regs[0],
                    rs: regs[1],
                },
                Instr::Xori {
                    rd: regs[0],
                    imm: 1,
                },
            ]);
        }
        3 => {
            // All 3! = 6 permutations of the three registers.
            let r = [regs[0], regs[1], regs[2]];
            let perms = [
                (r[0], r[1], r[2]),
                (r[0], r[2], r[1]),
                (r[1], r[0], r[2]),
                (r[1], r[2], r[0]),
                (r[2], r[0], r[1]),
                (r[2], r[1], r[0]),
            ];
            for &(rd, rs, rt) in &perms {
                candidates.push(vec![Instr::Cswap { rd, rs, rt }]);
                candidates.push(vec![Instr::Ccnot { rd, rs, rt }]);
            }
        }
        _ => {} // No candidates for other arities.
    }

    candidates
}

/// Byte semantics of the pinned 13-instruction walk (M-D.e slice 1).
/// Must match the emitted instruction sequence EXACTLY (byte-wide).
/// Scaffold for the machinery-proof test; not a public API.
#[doc(hidden)]
pub fn mde_walk_bytes(b: &mut [u8]) {
    b[0] ^= b[1];
    b[0] ^= 1;
    b[1] ^= 1;
    b[2] ^= b[3];
    b[3] ^= b[4];
    b[0] ^= 1;
    if b[0] & 1 == 1 {
        b.swap(1, 2);
        b.swap(2, 3);
        b.swap(3, 4);
    }
    b[0] ^= 1;
    b[3] ^= b[4];
    b[2] ^= b[3];
    b[1] ^= b[2];
}

/// k-leaf generalization of `check_pattern_2leaf` — verify a prim's
/// clause table matches a pinned byte-level map over k leaves.
fn check_pattern_kleaf(
    forward: &std::collections::BTreeMap<Value, Value>,
    vals: &[Value],
    in_ty: &Type,
    out_ty: &Type,
    k: usize,
    f: impl Fn(&mut [u8]),
) -> bool {
    for v in vals {
        let mut bytes = value_to_leaf_bytes(v, in_ty);
        if bytes.len() != k {
            return false;
        }
        f(&mut bytes);
        let Some(out) = forward.get(v) else {
            return false;
        };
        if value_to_leaf_bytes(out, out_ty) != bytes {
            return false;
        }
    }
    true
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

/// Check if a 2-leaf prim's clause table matches a byte-level pattern,
/// using the generic leaf-byte encoding (works for sum types, not just
/// products). Used for patterns that the `check_pattern_2reg` helper
/// cannot match because it requires `Value::Pair`.
fn check_pattern_2leaf(
    forward: &std::collections::BTreeMap<Value, Value>,
    vals: &[Value],
    in_ty: &Type,
    out_ty: &Type,
    f: impl Fn(u8, u8) -> (u8, u8),
) -> bool {
    for v in vals {
        let in_bytes = value_to_leaf_bytes(v, in_ty);
        if in_bytes.len() != 2 {
            return false;
        }
        let (exp_a, exp_b) = f(in_bytes[0], in_bytes[1]);
        let Some(out) = forward.get(v) else {
            return false;
        };
        let out_bytes = value_to_leaf_bytes(out, out_ty);
        if out_bytes.len() != 2 {
            return false;
        }
        if out_bytes[0] != exp_a || out_bytes[1] != exp_b {
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

// ── memexch primitive generator (M-D.b2) ────────────────────────────

/// Build a `PrimDef` for `memexch(elem_ty, n)`:
///   (data:elem_ty) × (addr:idx_ty) × Arr[elem_ty; n]
///     ↔ (data:elem_ty) × (addr:idx_ty) × Arr[elem_ty; n]
/// Semantics: (d, a, arr) ↦ (arr[a], a, arr[a ← d])
///
/// The index type is taken as `elem_ty` itself for f1 (Bit indexes a 2-word array).
/// F-4 CHECK: returns Err if the index type has MORE THAN `n` representable values.
/// The op is an involution, so forward == backward and both names are "memexch" (F-8).
///
/// Delegates to `memexch_prim_idx(elem_ty, elem_ty, n)` — index type equals element type.
pub fn memexch_prim(elem_ty: &Type, n: usize) -> Result<PrimDef, CompileError> {
    memexch_prim_idx(elem_ty, elem_ty, n)
}

/// Like `memexch_prim`, but with an explicit `idx_ty` (may differ from `elem_ty`).
///
/// Guards:
/// - elem_ty must be a single-leaf type.
/// - idx_ty must be a single-leaf type (else the addr byte at offset 1 is corrupt).
/// - idx_count(idx_ty) <= n.
/// - n <= MEM_WORDS.
pub fn memexch_prim_idx(elem_ty: &Type, idx_ty: &Type, n: usize) -> Result<PrimDef, CompileError> {
    // I-6: elem_ty must be a single-leaf type (multi-leaf elements can't match
    // the MexchReg single-register exchange semantics).
    let elem_leaves = num_leaves(elem_ty)?;
    if elem_leaves != 1 {
        return Err(CompileError::UnrecognizedPrim(format!(
            "memexch_prim_idx: elem_ty must have exactly 1 leaf, got {elem_leaves}"
        )));
    }
    // MAJOR-1: idx_ty must also be a single-leaf type — else the body hard-assumes
    // addr = 1 byte at offset 1; a multi-leaf idx overlaps the arr.
    let idx_leaves = num_leaves(idx_ty)?;
    if idx_leaves != 1 {
        return Err(CompileError::UnrecognizedPrim(format!(
            "memexch_prim_idx: idx_ty must have exactly 1 leaf, got {idx_leaves}"
        )));
    }
    // I-6: n must fit within MEM_WORDS (the max representable addr byte).
    if n > MEM_WORDS {
        return Err(CompileError::TooManyLeaves {
            needed: n,
            max: MEM_WORDS,
        });
    }
    let idx_count = enumerate_values(idx_ty).len();
    // F-4: every representable index value must be < n (so a % MEM_WORDS == a
    // for all representable a, since n <= MEM_WORDS). The tag byte a encodes
    // the thermometer depth ∈ {0, ..., idx_count-1}, not bounded by idx_count
    // in the general case — the constraint is idx_count <= n.
    if idx_count > n {
        return Err(CompileError::Deferred(format!(
            "memexch_prim_idx: index type has {idx_count} values but n={n}; \
             every representable index must be < n"
        )));
    }

    // Build the carrier type: (elem × idx) × (elem^n)
    // elem^n = elem × elem × ... × One (right-nested product, n times)
    let mut arr_ty = Type::One;
    for _ in (0..n).rev() {
        arr_ty = Type::prod(elem_ty.clone(), arr_ty);
    }
    // Full type: (data × (addr × arr))
    // Flatten as a product: data × addr × arr[0] × arr[1] × ... × arr[n-1]
    // which gives num_leaves = 1 + 1 + n leaves (for single-leaf elem_ty).
    let full_ty = Type::prod(elem_ty.clone(), Type::prod(idx_ty.clone(), arr_ty.clone()));

    let vals = enumerate_values(&full_ty);
    let n_leaves = num_leaves(&full_ty)?;

    let mut forward = std::collections::BTreeMap::new();

    for v in &vals {
        // Decompose into leaf bytes: [data, addr, arr[0], arr[1], ..., arr[n-1]]
        let bytes = value_to_leaf_bytes(v, &full_ty);
        debug_assert_eq!(bytes.len(), n_leaves);

        let d = bytes[0];
        let a = bytes[1];
        let idx = (a as usize) % n; // always == a since a ∈ {0,..,idx_count-1} and idx_count <= n

        // Output: (arr[idx], a, arr[idx ← d])
        let mut out_bytes = bytes.clone();
        out_bytes[0] = bytes[2 + idx]; // data_out = arr[idx]
                                       // out_bytes[1] = a (unchanged)
        out_bytes[2 + idx] = d; // arr[idx] ← d

        let (out_val, _) = value_from_leaf_bytes(&out_bytes, &full_ty);
        forward.insert(v.clone(), out_val);
    }

    // Involution: backward == forward (F-8)
    let backward = forward.clone();

    Ok(PrimDef {
        forward_name: "memexch".into(),
        backward_name: "memexch".into(),
        input_ty: full_ty.clone(),
        output_ty: full_ty,
        forward,
        backward,
        symbolic: None,
    })
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
#[allow(clippy::too_many_arguments)]
fn lower_controlled_idiom(
    ctrl_ty: &Type,
    tgt_ty: &Type,
    ctrl_l: &Type,
    ctrl_r: &Type,
    f: &Iso,
    g: &Iso,
    ctx: &Ctx,
    meta: &mut LowerMeta,
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
    // occ from dctx (post-displacement), not the parent.
    let ctx_tgt = Ctx {
        homes: dctx.homes[n_ctrl..n_ctrl + n_tgt].to_vec(),
        occ: dctx.occ,
    };
    let (instrs_g, _) = lower(g, &ctx_tgt, meta)?;

    // Guard: reject doubly-nested skip-block control (reservation
    // ordering deferred).
    if has_inner_control(&instrs_g) {
        return Err(CompileError::Deferred(
            "nested skip-block control in controlled idiom — deferred".into(),
        ));
    }

    if instrs_g.is_empty() {
        return Ok((vec![], ctx.clone()));
    }

    // f = identity, g = general multi-instruction sequence.
    // Skip g iff ctrl=0 (Left branch = identity).
    let mut instrs = Vec::new();
    instrs.extend(aux_pi);
    instrs.extend(ctrl_pi);
    meta.forward_skip_total += instrs_g.len();
    instrs.extend(emit_skip_block(ctrl_reg, &instrs_g, rb, temp, false)?);
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

/// A node in the compositional WCET **cost tree** (A-3, BLOCKER-2).
///
/// Each `Trace` loop emits exactly one `CostTree` node. A node's `inner`
/// children are the loop nodes lowered *inside* this loop's body — the ROM
/// contains each inner body straight-line (once), but at run time the inner
/// loop actually runs up to its own `k_max` times per *outer* iteration. The
/// flat `(k_max, iter_len)` sibling list this replaces could not express that
/// multiply, so it under-counted `B` for nested loops (outer k=4 / inner k=100
/// / iter=12: true B ≈ 4800, flat sum ≈ 88 — a 55× under-count that lets the
/// park guard pass a bogus budget → PC wrap → L15-class corruption).
///
/// `iter_len` is the *ROM* length of one straight-line body iteration
/// (`|body| + LOOP_OVERHEAD`); it counts each inner loop's body **once**
/// (the ROM does). The extra dynamic cost of the inner loops is folded in
/// separately via `k_max · Σ extra(child)` — see [`CostTree::extra`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CostTree {
    /// Trajectory-derived outer iteration bound, `min(k_exact, |u| + 1)`.
    pub k_max: usize,
    /// ROM length of one straight-line body iteration (`|body| + LOOP_OVERHEAD`).
    /// Counts each inner loop's straight-line body exactly once.
    pub iter_len: usize,
    /// Loop nodes nested inside this loop's body. The ROM runs each once;
    /// at run time each runs up to `k_max` times *per outer iteration*.
    pub inner: Vec<CostTree>,
}

impl CostTree {
    /// The **extra** dynamic step count this loop contributes *beyond* its
    /// straight-line ROM footprint:
    ///
    /// ```text
    /// extra(node) = (k_max − 1) · iter_len          # outer spine repeats
    ///             + k_max · Σ_child extra(child)     # each inner loop's extra,
    ///                                                #   once per outer iteration
    /// ```
    ///
    /// **Why this is a sound upper bound (`B_fold ≥ true_B`).** The ROM already
    /// counts one straight-line pass of the body (that pass is inside
    /// `instrs.len()`), which includes one pass of every inner loop's body. On
    /// each of the `k_max` outer iterations the inner loops may run up to their
    /// own `k_max` — so the extra work an inner loop does *beyond its one ROM
    /// pass* (`extra(child)`) happens at most `k_max` times. `k_max` is itself an
    /// over-estimate of the true outer trajectory (min of the trajectory walk
    /// and the cardinality bound `|u|+1`), and each `extra(child)` is recursively
    /// an over-estimate, so the product is `≥ true_B`. Multiplication of
    /// non-negative over-estimates preserves the inequality at every level: the
    /// fold **cannot under-count**.
    pub fn extra(&self) -> usize {
        let outer = (self.k_max - 1) * self.iter_len;
        let inner_extra: usize = self.inner.iter().map(CostTree::extra).sum();
        outer + self.k_max * inner_extra
    }
}

/// Structured metadata returned by compilation (M-D.d §4).
///
/// Replaces the ROM-scanning `max_skip_distance` with compositionally-
/// computed WCET and forward-skip metadata. The ROM scanner mispairs
/// once a loop bracket exists; this structure is always correct.
#[derive(Clone, Debug)]
pub struct Lowered {
    pub instrs: Vec<Instr>,
    /// Total forward-skip distance (from forward skip-blocks only;
    /// the loop's backward bracket contributes 0).
    pub forward_skip_total: usize,
    /// Cost tree: one node per top-level Trace loop, each carrying its nested
    /// inner loops (A-3). Replaces the old flat `Vec<(k_max, iter_len)>` that
    /// could not multiply an inner loop's cost through the enclosing `k_max`.
    /// For a single-level loop this is `[CostTree { k_max, iter_len, inner: [] }]`
    /// and reproduces the exact old `B`.
    pub loop_terms: Vec<CostTree>,
}

impl Lowered {
    /// Compositional WCET: `|instrs| + Σ_top extra(node)` over the cost tree.
    /// The first (straight-line) pass of every loop body is already counted in
    /// `|instrs|` (the ROM length); [`CostTree::extra`] adds the additional
    /// iterations, multiplying an inner loop's extra cost through the enclosing
    /// `k_max` so nested loops are counted correctly (`B_fold ≥ true_B`).
    ///
    /// For a flat program (single-level loops, `inner` empty) this collapses to
    /// the old `|instrs| + Σ (k_max − 1) · iter_len` — byte-identical `B`.
    pub fn wcet(&self) -> usize {
        let base = self.instrs.len();
        let loop_contribution: usize = self.loop_terms.iter().map(CostTree::extra).sum();
        base + loop_contribution
    }

    /// Compositional S_max: forward-skip distance only.
    pub fn s_max(&self) -> usize {
        self.forward_skip_total
    }
}

/// Compile a core `Iso` to a TISC-1 instruction sequence with metadata.
pub fn compile_with_metadata(iso: &Iso) -> Result<Lowered, CompileError> {
    let (in_ty, _out_ty) = theseus_core::type_of(iso)
        .map_err(|e| CompileError::Deferred(format!("type error: {e:?}")))?;
    let n = num_leaves(&in_ty)?;
    if n > MAX_LEAVES {
        return Err(CompileError::TooManyLeaves {
            needed: n,
            max: MAX_LEAVES,
        });
    }
    let ctx = Ctx::new(n);
    let mut meta = LowerMeta::default();
    let (instrs, _final_ctx) = lower(iso, &ctx, &mut meta)?;
    Ok(Lowered {
        instrs,
        forward_skip_total: meta.forward_skip_total,
        loop_terms: meta.loop_terms,
    })
}

/// Compile with a `resident` count: trailing `resident` leaves live in
/// mem[0..resident]. The head leaves (n - resident) must fit in 14 slots
/// (8 regs + 6 scratch). `resident == 0` reproduces `compile_with_metadata`.
pub fn compile_with_metadata_resident(iso: &Iso, resident: usize) -> Result<Lowered, CompileError> {
    let (in_ty, _out_ty) = theseus_core::type_of(iso)
        .map_err(|e| CompileError::Deferred(format!("type error: {e:?}")))?;
    let n = num_leaves(&in_ty)?;
    // I-7: Reject: resident > n (would underflow n - resident)
    if resident > n {
        return Err(CompileError::TooManyLeaves {
            needed: resident,
            max: n,
        });
    }
    // Reject: resident > MEM_WORDS
    if resident > MEM_WORDS {
        return Err(CompileError::TooManyLeaves {
            needed: resident,
            max: MEM_WORDS,
        });
    }
    // Reject: head leaves > 14
    let head = n - resident;
    if head > 14 {
        return Err(CompileError::TooManyLeaves {
            needed: head,
            max: 14,
        });
    }
    let ctx = Ctx::new_resident(n, resident);
    let mut meta = LowerMeta::default();
    let (instrs, _final_ctx) = lower(iso, &ctx, &mut meta)?;
    Ok(Lowered {
        instrs,
        forward_skip_total: meta.forward_skip_total,
        loop_terms: meta.loop_terms,
    })
}

/// Compile a core `Iso` (M-A/M-B/M-C1/M-D.d) to a TISC-1 instruction sequence.
///
/// Returns `Err(CompileError)` for out-of-scope constructs.
pub fn compile(iso: &Iso) -> Result<Vec<Instr>, CompileError> {
    compile_with_metadata(iso).map(|l| l.instrs)
}

/// Metadata accumulated during lowering (M-D.d §4).
///
/// Threaded through all `lower` calls via `&mut LowerMeta` to
/// compositionally track WCET and forward-skip distance.
#[derive(Clone, Debug, Default)]
struct LowerMeta {
    forward_skip_total: usize,
    /// Cost-tree nodes for the Trace loops lowered at this level. A nested
    /// Trace merges its body's `loop_terms` into the outer node's `inner`
    /// (see `lower_trace`), so the top-level `LowerMeta` carries the full tree.
    loop_terms: Vec<CostTree>,
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
/// INVARIANT: lower() returns with canonical layout restored (page-out
/// bracketing). The shared-occ safety depends on this.
fn lower(iso: &Iso, ctx: &Ctx, meta: &mut LowerMeta) -> Result<(Vec<Instr>, Ctx), CompileError> {
    let result = lower_dispatch(iso, ctx, meta);
    if let Ok((_, ref out_ctx)) = result {
        debug_assert_eq!(
            out_ctx.homes, ctx.homes,
            "lower() invariant: canonical layout must be restored"
        );
    }
    result
}

fn lower_dispatch(
    iso: &Iso,
    ctx: &Ctx,
    meta: &mut LowerMeta,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    // First, try matching the controlled-op idiom (before decomposing Seq).
    if let Some((ctrl_ty, tgt_ty, ctrl_l, ctrl_r, f, g)) = match_controlled_idiom(iso) {
        return lower_controlled_idiom(ctrl_ty, tgt_ty, ctrl_l, ctrl_r, f, g, ctx, meta);
    }

    match iso {
        Iso::Id(_) => Ok((vec![], ctx.clone())),

        Iso::Seq(f, g) => {
            let (mut instrs_f, ctx2) = lower(f, ctx, meta)?;
            let (instrs_g, ctx3) = lower(g, &ctx2, meta)?;
            instrs_f.extend(instrs_g);
            Ok((instrs_f, ctx3))
        }

        Iso::Times(f, g) => {
            let (in_f, _) = theseus_core::type_of(f)
                .map_err(|e| CompileError::Deferred(format!("type error in Times/f: {e:?}")))?;
            let n_left = num_leaves(&in_f)?;
            let (ctx_f, ctx_g) = ctx.split(n_left);
            let (mut instrs_f, ctx_f2) = lower(f, &ctx_f, meta)?;
            let (instrs_g, ctx_g2) = lower(g, &ctx_g, meta)?;
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
            lower(&adj, ctx, meta)
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
                    lower_data_dependent_iter(n_ty, d, body, ctx, meta)
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
            lower_bare_plus(f, g, ctx, meta)
        }

        Iso::AssocLSum(_, _, _) | Iso::AssocRSum(_, _, _) | Iso::UniteSum(_) | Iso::UnitiSum(_) => {
            // F7 guard: verify the structural sum-renaming is identity
            // on the M-B leaf-byte encoding. If not, reject rather than
            // silently miscompile.
            let (in_ty, out_ty) = theseus_core::type_of(iso)
                .map_err(|e| CompileError::Deferred(format!("type error: {e:?}")))?;
            let transform: fn(&Value) -> Value = match iso {
                Iso::AssocRSum(_, _, _) => apply_assoc_r_sum,
                Iso::AssocLSum(_, _, _) => apply_assoc_l_sum,
                Iso::UniteSum(_) => apply_unite_sum,
                Iso::UnitiSum(_) => apply_uniti_sum,
                _ => unreachable!(),
            };
            if structural_iso_is_identity(&in_ty, &out_ty, transform) {
                Ok((vec![], ctx.clone()))
            } else {
                Err(CompileError::Deferred(
                    "non-trivial structural sum-renaming (AssocSum/UniteSum) \
                     — reversible-circuit lowering deferred (F7)"
                        .to_string(),
                ))
            }
        }

        Iso::Distrib(_, _, _) | Iso::Factor(_, _, _) => {
            // F7 guard: same identity check for distributivity isos.
            let (in_ty, out_ty) = theseus_core::type_of(iso)
                .map_err(|e| CompileError::Deferred(format!("type error: {e:?}")))?;
            let transform: fn(&Value) -> Value = match iso {
                Iso::Distrib(_, _, _) => apply_distrib,
                Iso::Factor(_, _, _) => apply_factor,
                _ => unreachable!(),
            };
            if structural_iso_is_identity(&in_ty, &out_ty, transform) {
                Ok((vec![], ctx.clone()))
            } else {
                Err(CompileError::Deferred(
                    "non-trivial structural distributivity (Distrib/Factor) \
                     — reversible-circuit lowering deferred (F7)"
                        .to_string(),
                ))
            }
        }

        // ── M-D.d: Trace (reversible while-loop) ────────────────────
        Iso::Trace(u, body) => lower_trace(u, body, ctx, meta),

        Iso::Fold(l, body) => lower_fold_unfold(l, body, ctx, true, meta),
        Iso::Unfold(l, body) => lower_fold_unfold(l, body, ctx, false, meta),
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
    meta: &mut LowerMeta,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    let n_counter = num_leaves(n_ty)?;

    // Allocate 2 aux registers for skip-blocks, excluding counter leaves.
    let exclude: Vec<usize> = (0..n_counter).collect();
    let (aux, aux_pi, aux_po, dctx) = allocate_aux(ctx, 2, &exclude)?;
    let rb = aux[0];
    let temp = aux[1];

    // Compile the body once with the displaced context's body sub-slice.
    let (_, dctx_body) = dctx.split(n_counter);
    let (body_instrs, body_ctx_out) = lower(body, &dctx_body, meta)?;

    // Guard: reject doubly-nested skip-block control (reservation
    // ordering deferred).
    if has_inner_control(&body_instrs) {
        return Err(CompileError::Deferred(
            "nested skip-block control in iter body — deferred".into(),
        ));
    }

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
        let skip_instrs = emit_skip_block(ctrl_reg, &body_instrs, rb, temp, false)?;
        meta.forward_skip_total += body_instrs.len();
        instrs.extend(skip_instrs);
        instrs.extend(ctrl_po);
    }

    instrs.extend(aux_po);
    Ok((instrs, ctx.clone()))
}

// ── M-D.d: Trace as reversible while-loop ──────────────────────────

/// Lower `Trace(u, body)` to a reversible while-loop using a backward
/// branch and the ADR-0034 come-from join-dispatch discipline.
///
/// The loop skeleton (design §2, hand-verified):
/// ```text
/// prologue: XORI tag,1          ; inject Right (a-side)
///           ADDI temp,δu        ; mint token
/// Ltop:  SWAPBR rc              ; catch: rc ← δu on backward re-entry
///        CSWAP  ctrl, rc, temp  ; join-dispatch: route witness
///        SUBI   temp, δu        ; burn token
///        XOR    ctrl, tag
///        XORI   ctrl, 1         ; ctrl ⊕= ¬tag  →  0
///        <body: L instrs>
///        XOR    ctrl, tag
///        XORI   ctrl, 1         ; ctrl := ¬tag
///        ADDI   temp, δu        ; mint token
///        CSWAP  ctrl, temp, rb  ; route: continue or exit
/// Lend:  SWAPBR rb              ; continue → backward, exit → fall through
///        SUBI   temp, δu        ; burn token (exit path)
///        XORI   tag,1           ; strip Right
/// ```
///
/// `δu = 256 − (L + LOOP_OVERHEAD)` is the backward jump distance.
/// `k_max = min(k_exact, |u| + 1)` where `k_exact` is the trajectory-derived
/// exact bound (max body applications to exit over all real inputs, +1 safety),
/// falling back to `|u| + 1` if any trajectory is inconclusive.
fn lower_trace(
    u: &Type,
    body: &Iso,
    ctx: &Ctx,
    meta: &mut LowerMeta,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    // ── Type analysis ──────────────────────────────────────────────
    let (body_in_ty, body_out_ty) = theseus_core::type_of(body)
        .map_err(|e| CompileError::Deferred(format!("type error in Trace body: {e:?}")))?;

    // body : u+a ↔ u+b. Extract a, b.
    let a_ty = match &body_in_ty {
        Type::Sum(u1, a) if u1.as_ref() == u => a.as_ref().clone(),
        _ => {
            return Err(CompileError::Deferred(format!(
                "Trace body input not Sum(u, _): {body_in_ty}"
            )));
        }
    };
    let _b_ty = match &body_out_ty {
        Type::Sum(u2, b) if u2.as_ref() == u => b.as_ref().clone(),
        _ => {
            return Err(CompileError::Deferred(format!(
                "Trace body output not Sum(u, _): {body_out_ty}"
            )));
        }
    };

    // Carrier = Sum(u, a). Leaf count = 1 + max(|u|, |a|).
    let _n_u = num_leaves(u)?;
    let _n_a = num_leaves(&a_ty)?;
    let n_carrier = num_leaves(&body_in_ty)?; // = 1 + max(n_u, n_a)
    let n_payload = n_carrier - 1; // payload leaves (tag excluded)

    // Guard (F-5): the loop carrier is sized for `a` (n_carrier = 1+max(|u|,|a|)).
    // If `b` has a different leaf count, the output unmarshal (which uses the
    // Trace output type `b`) would misread the tag register as a payload leaf.
    if num_leaves(&a_ty)? != num_leaves(&_b_ty)? {
        return Err(CompileError::Deferred(format!(
            "Trace body a/b payload leaf-count mismatch ({} vs {}) — deferred (M-D.d F-5)",
            num_leaves(&a_ty)?,
            num_leaves(&_b_ty)?
        )));
    }

    // |u| = number of inhabitants of u (for k_max fallback).
    let u_inhabitants = enumerate_values(u).len();
    if u_inhabitants == 0 {
        return Err(CompileError::Deferred(
            "Trace with uninhabited carrier type u".into(),
        ));
    }

    // ── Reject-don't-corrupt guards ────────────────────────────────

    // Guard: payload leaves must fit in MAX_LEAVES_LOOP.
    if n_payload > MAX_LEAVES_LOOP {
        return Err(CompileError::Deferred(format!(
            "Trace body carrier has {n_payload} payload leaves, max is {MAX_LEAVES_LOOP} (M-D.d §3)"
        )));
    }

    // Guard: require a canonical AND UNSHARED context. The homes must be
    // canonical, AND the whole-machine occ must hold nothing beyond this
    // Trace's own input leaves. lower_trace hand-assigns tag/payload/aux
    // registers WITHOUT consulting occ, so under a `Times` split — where
    // `split` copies the full parent occ into a left half whose homes are
    // still Reg(0..n_left) (indistinguishable from canonical) — those aux
    // registers would alias the sibling factor's live data and silently
    // clobber it (F-1: `Times(Trace, g)` produced wrong outputs that the
    // BR/s0/s1 terminal check could not see). occ ⊋ homes ⇒ reject.
    let canonical = Ctx::new(ctx.homes.len());
    if ctx.homes != canonical.homes || ctx.occ != OccSet::from_locations(&ctx.homes) {
        return Err(CompileError::Deferred(
            "Trace under a non-canonical or occupied (e.g. Times sibling) context — \
             its hand-assigned aux registers would alias live data (M-D.d F-1)"
                .into(),
        ));
    }

    // Guard: reject nested Trace — the come-from join doesn't compose.
    if contains_trace(body) {
        return Err(CompileError::Deferred(
            "nested Trace not supported — come-from join does not compose (M-D.d §5)".into(),
        ));
    }

    // ── Lower the body once ────────────────────────────────────────

    // The carrier Sum(u, a) has n_carrier = 1 + max(|u|, |a|) leaves:
    //   leaf 0 = tag, leaves 1..n_carrier = payload (zero-extended).
    // The ctx only has num_leaves(a) homes (the Trace input type).
    // We need to allocate registers for: tag + extra payload + aux.
    //
    // Register layout:
    //   regs 0..|a| = input a (from ctx)
    //   We'll build the body_ctx with carrier layout:
    //     body_ctx[0] = tag register (a free register)
    //     body_ctx[1..1+|a|] = input registers (ctx.homes[0..|a|])
    //     body_ctx[1+|a|..n_carrier] = extra payload (free registers)
    let n_input = ctx.homes.len(); // = num_leaves(a)

    // M-D.e slice 1: registers available to the carrier = NUM_REGS - 4 aux.
    // Overflow zero-init padding leaves spill to Scratch(SCRATCH_SPILL_BASE+).
    let carrier_reg_budget = NUM_REGS - 4; // = 4
                                           // Guard (a): tag + all LIVE INPUT leaves must be register-resident.
                                           // This is the ONLY barrier against live-input silent corruption (r1 F5.2):
                                           // a live leaf forced to Scratch with no relocation → loop reads 0.
    if n_input + 1 > carrier_reg_budget {
        return Err(CompileError::Deferred(format!(
            "Trace tag+input needs {} registers but carrier budget is {carrier_reg_budget} \
             (live-input spill not supported, M-D.e F5.2)",
            n_input + 1
        )));
    }

    // Build body_ctx: tag at leaf 0, then input payload, then zero-padding.
    let mut body_homes = Vec::with_capacity(n_carrier);
    // Tag register: first free register after input.
    let _tag_start = n_input as u8;
    // Actually, we need a specific layout. Let's assign:
    //   tag = reg[n_input]  (first free reg after input)
    //   payload[0..|a|] = reg[0..|a|] (input regs, unchanged)
    //   payload[|a|..max(|u|,|a|)] = reg[n_input+1..] (additional free regs)
    //   aux: rc, rb, temp, ctrl = next free regs after carrier
    //
    // But the body expects leaf 0 = tag. So body_ctx.homes[0] = Reg(tag_reg).
    // body_ctx.homes[1..1+|a|] = ctx.homes[0..|a|] = Reg(0..n_input).
    // body_ctx.homes[1+|a|..n_carrier] = Reg(n_input+1..) for extra payload.
    let tag_reg_idx = n_input as u8; // allocate tag from first free reg
    body_homes.push(Location::Reg(tag_reg_idx)); // leaf 0 = tag

    // Payload: first n_input leaves from ctx (the input data).
    for i in 0..n_input {
        body_homes.push(ctx.homes[i]);
    }
    // Extra payload (if carrier is wider than input): register-resident
    // up to the carrier_reg_budget, then spill to Scratch(SCRATCH_SPILL_BASE+).
    // Only zero-init PADDING leaves may spill (no live-data relocation — the
    // n_input+1 guard above ensures all input leaves are register-resident).
    let n_extra = n_carrier - 1 - n_input; // max(|u|,|a|) - |a| = max(0, |u|-|a|)
    let mut spill_count = 0u8;
    for j in 0..n_extra {
        let reg = n_input + 1 + j; // candidate register index
        if reg < carrier_reg_budget {
            body_homes.push(Location::Reg(reg as u8));
        } else {
            // M-D.e: zero-init padding leaf spills to scratch.
            // Marshal already zeroes Scratch — no live-data relocation (F5).
            // NOTE: home_of maps canonical leaf 8 → Scratch(2)=SCRATCH_SPILL_BASE;
            // at cap 4 this is unreachable, but the collision is pinned here.
            body_homes.push(Location::Scratch(SCRATCH_SPILL_BASE + spill_count));
            spill_count += 1;
        }
    }

    // Build occ: mark all carrier registers as occupied.
    let mut body_occ = ctx.occ;
    for h in &body_homes {
        body_occ.set(*h);
    }

    // ── A-5: occ-aware aux frame, drawn BEFORE lowering the body ────────
    // Draw this loop level's four aux registers {rc, rb, temp, ctrl} from
    // genuinely-free registers (per occ), NOT the positional `max_carrier_reg+1`.
    // Then OCC-MARK the frame into `body_occ` before lowering the body, so that a
    // nested Trace's own `allocate_aux_frame` sees this level's aux as taken and
    // draws a DISJOINT frame. This is the frame-stack discipline (design §2.3):
    // each depth gets a frame disjoint from carriers, siblings, and outer aux.
    //
    // A carrier ctx has homes marked in `body_occ` above, so the frame is
    // disjoint from the carrier; under `Times(Trace, g)` the sibling g's leaves
    // are already in `ctx.occ`, so the frame is disjoint from g's live regs too
    // (the F-1 aliasing hazard, now closed structurally rather than by reject).
    let ([rc, rb, temp, ctrl], body_occ) = allocate_aux_frame(&Ctx {
        homes: body_homes.clone(),
        occ: body_occ,
    })?;

    // SOUNDNESS / byte-identity assert (A-5 review check §2.3): for the canonical
    // single-level Trace the carrier occupies regs 0..=max_carrier_reg
    // contiguously, so the occ-aware frame MUST equal the old positional
    // assignment `[max_carrier_reg+1 .. +4]`. If this ever diverges for a
    // single-level program the aux placement (hence the ROM) changed — a
    // regression. We only assert the equality when the frame is register-drawn
    // above the carrier (always, here); for nested/sibling contexts the frame is
    // *intentionally* different, which is the whole point of A-5.
    #[cfg(debug_assertions)]
    {
        let max_carrier_reg = body_homes
            .iter()
            .filter_map(|h| match h {
                Location::Reg(r) => Some(*r),
                _ => None,
            })
            .max()
            .unwrap_or(0);
        // Only meaningful when the ctx is canonical + unshared (single-level):
        // occ holds nothing beyond this Trace's own carrier leaves.
        let canonical = Ctx::new(ctx.homes.len());
        if ctx.homes == canonical.homes && ctx.occ == OccSet::from_locations(&ctx.homes) {
            debug_assert_eq!(
                [rc, rb, temp, ctrl],
                [
                    max_carrier_reg + 1,
                    max_carrier_reg + 2,
                    max_carrier_reg + 3,
                    max_carrier_reg + 4
                ],
                "A-5: occ-aware aux frame must be byte-identical to the old \
                 max_carrier_reg+1 positional assignment for a canonical \
                 single-level Trace (no ROM regression)"
            );
        }
    }

    let body_ctx = Ctx {
        homes: body_homes,
        occ: body_occ,
    };

    // Body must not use Swapbr (no forward skip-blocks inside loops
    // for v1 — the safe Swapbr-free-body policy, design §4/F6).
    // A-3: lower the body into its OWN meta, then merge (see below). The old
    // code discarded `body_meta` entirely (defect (a), BLOCKER-2) — so an inner
    // loop's cost tree never reached the top-level `Lowered` and `wcet()` could
    // not multiply it through this loop's `k_max`.
    let mut body_meta = LowerMeta::default();
    let (body_instrs, body_ctx_out) = lower(body, &body_ctx, &mut body_meta)?;

    // A-3: does this body contain its own loop(s)? Inner loops leave nodes in
    // `body_meta.loop_terms`; inner forward skip-blocks (Iter/Plus) show up as
    // Swapbrs in `body_instrs` / `forward_skip_total`. Either way the exact-k_max
    // walk below runs `body_instrs` STRAIGHT-LINE through `exec_instr` (which has
    // no PC / no back-edge), so it would simulate an inner back-edge only ONCE
    // per outer iteration (defect (b), BLOCKER-2). When the body contains inner
    // control we therefore do NOT trust the trajectory walk for the OUTER
    // `k_max` and fall back to the cardinality bound `|u|+1` — the sound
    // over-estimate. The inner loops' *extra* cost is folded in structurally via
    // the cost tree (`k_max · Σ extra(child)`), not via the straight-line walk.
    let body_has_inner_control =
        !body_meta.loop_terms.is_empty() || has_inner_control(&body_instrs);

    // Guard: reject body containing Swapbr (forward skip-blocks inside
    // a loop body would require compositional S_max accounting under
    // nesting, deferred per design §4/F6 safe policy).
    // NOTE (A-3): this reject is what keeps real nested programs out TODAY; it
    // is A-4's job to delete it. The A-3 cost-tree merge + walk fallback below
    // are dormant until then and are exercised white-box by the A-3 tests.
    if has_inner_control(&body_instrs) {
        return Err(CompileError::Deferred(
            "Trace body contains Swapbr (skip-block) — deferred (M-D.d F6 safe policy)".into(),
        ));
    }

    // Guard: layout invariant — homes_in == homes_out (§3, hard error).
    if body_ctx.homes != body_ctx_out.homes {
        return Err(CompileError::Deferred(
            "Trace body violates layout invariant: homes_in != homes_out (M-D.d §3)".into(),
        ));
    }

    let body_len = body_instrs.len();
    let iter_len = body_len + LOOP_OVERHEAD;

    // ── Trajectory-derived exact k_max ────────────────────────────────
    // Walk the lowered body (via exec_instr — the same byte-semantics the
    // real CPU executes) from every real input Right(a) to compute the
    // maximum number of body applications until the tag reaches exit (Right).
    // k_exact = max(T over all inputs) + 1 (+1 safety: exit-check application).
    // SOUNDNESS FALLBACK: if any trajectory does not exit within |u|
    // applications, or if k_exact > |u|+1, use k_max = |u|+1 (the prior
    // full-cardinality bound). So k_exact is NEVER looser than today.
    let k_max = {
        let k_fallback = u_inhabitants + 1;
        let mut max_t: usize = 0;
        // A-3 (defect (b)): the walk simulates the body straight-line, so an
        // inner back-edge would run once, not k_inner×. If the body has inner
        // control, the walk's outer trajectory is untrustworthy → force the
        // sound cardinality-bound fallback for the OUTER k_max.
        let mut use_fallback = body_has_inner_control;
        for a_val in enumerate_values(&a_ty) {
            if use_fallback {
                break;
            }
            // Set up carrier state: Right(a) = tag 1, payload = a's leaf bytes.
            // marshal(a, a_ty) places a's leaves in Reg(0..n_input-1) — exactly
            // where body_homes[1..1+n_input] maps them. Set tag register to 1.
            //
            // SOUNDNESS (impl-review A): this marshal-layout == body-home-layout
            // equality is the ENTIRE reason the walk simulates the same registers
            // the real CPU does. It holds ONLY because the F-1 canonical/occupied
            // guard above rejects any Trace whose ctx.homes != Ctx::new(n).homes,
            // and both marshal() and body_homes derive from the same sole
            // placement fn `home_of`. **A future relaxation of the F-1 guard
            // (allowing relocated/resident input homes under Trace) would silently
            // invalidate this walk → an unsound k_exact.** Keep F-1 before this.
            let mut state = marshal(&a_val, &a_ty);
            state.regs[tag_reg_idx as usize] = 1; // Right tag (input arm)
            let mut t: usize = 0;
            loop {
                // Apply the lowered body once (exec_instr = real CPU semantics).
                for &instr in &body_instrs {
                    exec_instr(instr, &mut state);
                }
                t += 1;
                // Right (tag != 0) = exit arm. NOTE (impl-review C): the walk
                // tests the whole tag BYTE, but the hardware routes on bit 0 only
                // (Cswap tests ctrl&1, ctrl = tag^1). Safe because every lowered
                // body maps canonical encodings to canonical encodings, so the tag
                // is ∈ {0,1} at every iteration boundary (a tag byte ≥ 2 was already
                // fatal under the old bound — infinite loop / wrong output).
                if state.regs[tag_reg_idx as usize] != 0 {
                    break;
                }
                // Soundness: if trajectory exceeds |u| applications, fallback.
                if t >= u_inhabitants {
                    use_fallback = true;
                    break;
                }
            }
            if use_fallback {
                break;
            }
            max_t = max_t.max(t);
        }
        if use_fallback {
            k_fallback
        } else {
            // k_exact = max(T) + 1; never looser than the cardinality bound.
            (max_t + 1).min(k_fallback)
        }
    };

    // A-3: assemble this loop's cost-tree node. The inner children come from
    // the body's own meta (defect (a) fix: previously discarded). `iter_len`
    // counts each inner body straight-line ONCE (the ROM does); the inner
    // loops' extra iterations are folded via `k_max · Σ extra(child)` in
    // `CostTree::extra`, so nesting is multiplied through this loop's k_max.
    let cost_node = CostTree {
        k_max,
        iter_len,
        inner: body_meta.loop_terms.clone(),
    };

    // Guard: P2 — wcet + S_max must fit in 256.
    // prologue(2) + iter_len + epilogue(2) = 4 + iter_len = rom_len for the Trace.
    // But the Trace might be composed with other code; P2 is checked at
    // the top level in run_on_cpu. Here we check the Trace's own
    // contribution is feasible.
    // A-3: the feasibility bound is the FULL tree cost (ROM footprint of one
    // straight-line pass + all extra iterations, inner loops multiplied through
    // k_max), not just `spine + k_max·iter_len` which ignored nested cost.
    let spine = 4; // prologue(2) + epilogue(2)
    let this_wcet = spine + iter_len + cost_node.extra();
    if this_wcet > 256 {
        return Err(CompileError::Deferred(format!(
            "Trace WCET {} exceeds 256 (k_max={}, iter_len={}, spine={}) (M-D.d §5)",
            this_wcet, k_max, iter_len, spine
        )));
    }

    // ── Compute δu ─────────────────────────────────────────────────
    // After Lend (SWAPBR rb at offset iter_len-1 from Ltop):
    //   PC = Ltop + (iter_len - 1) + δu + 1 = Ltop + iter_len + δu
    // Want PC = Ltop → δu = 256 - iter_len (mod 256).
    let delta_u: u8 = (256usize.wrapping_sub(iter_len) & 0xFF) as u8;

    // ── Register assignment (§3, A-5 occ-aware frame) ──────────────────
    // The body_ctx has tag at body_homes[0] = Reg(tag_reg_idx). The four aux
    // {rc, rb, temp, ctrl} were drawn OCC-AWARELY above (`allocate_aux_frame`)
    // *before* the body was lowered, and occ-marked into `body_ctx.occ` so a
    // nested Trace draws a disjoint frame. `allocate_aux_frame` already
    // guarantees all four are distinct in-register slots disjoint from the
    // carrier and any sibling/outer live data — no positional `max_carrier_reg+1`
    // and no register-file overflow check needed here (it rejected up front).
    let tag_reg: u8 = tag_reg_idx;

    // ── Emit the skeleton ──────────────────────────────────────────
    let mut instrs = vec![
        // Prologue (2 instructions):
        Instr::Xori {
            rd: tag_reg,
            imm: 1,
        }, // inject Right (a→u+a)
        Instr::Addi {
            rd: temp,
            imm: delta_u,
        }, // mint token
        // Ltop: SWAPBR rc
        Instr::Swapbr { rd: rc },
        // Join-dispatch: CSWAP ctrl, rc, temp
        Instr::Cswap {
            rd: ctrl,
            rs: rc,
            rt: temp,
        },
        // Burn token: SUBI temp, δu
        Instr::Subi {
            rd: temp,
            imm: delta_u,
        },
        // Uncompute ctrl from tag (top): XOR ctrl, tag; XORI ctrl, 1
        Instr::Xor {
            rd: ctrl,
            rs: tag_reg,
        },
        Instr::Xori { rd: ctrl, imm: 1 },
    ];

    // Body
    instrs.extend_from_slice(&body_instrs);

    // Compute ctrl from tag (bottom): XOR ctrl, tag; XORI ctrl, 1
    // ctrl := ¬tag (1 = continue [Left], 0 = exit [Right])
    instrs.push(Instr::Xor {
        rd: ctrl,
        rs: tag_reg,
    });
    instrs.push(Instr::Xori { rd: ctrl, imm: 1 });

    // Mint token: ADDI temp, δu
    instrs.push(Instr::Addi {
        rd: temp,
        imm: delta_u,
    });

    // Route: CSWAP ctrl, temp, rb
    // continue: rb=δu, temp=0 · exit: rb=0, temp=δu
    instrs.push(Instr::Cswap {
        rd: ctrl,
        rs: temp,
        rt: rb,
    });

    // Lend: SWAPBR rb
    // continue: BR=δu → PC jumps back to Ltop · exit: BR=0 → fall through
    instrs.push(Instr::Swapbr { rd: rb });

    // Epilogue (2 instructions):
    // Burn token (exit path): SUBI temp, δu
    instrs.push(Instr::Subi {
        rd: temp,
        imm: delta_u,
    });
    // Strip Right: XORI tag, 1
    instrs.push(Instr::Xori {
        rd: tag_reg,
        imm: 1,
    });

    // ── Record metadata ────────────────────────────────────────────
    // The loop's backward bracket contributes 0 to forward skip distance.
    // A-3: merge the body's forward-skip distance into the parent. Today the
    // body is Swapbr-free (the reject above), so this is 0; it becomes
    // load-bearing when A-4 admits Iter/Plus-under-Trace (MAJOR-5: s_max stays
    // a Σ-fold — a parked PC can land in ANY forward-skip region regardless of
    // which arm ran, so we sum, never max).
    meta.forward_skip_total += body_meta.forward_skip_total;
    // A-3: push the cost-tree node (with its inner children merged) so the
    // top-level `Lowered` carries the full nesting structure.
    meta.loop_terms.push(cost_node);

    // Verify emitted instruction count.
    debug_assert_eq!(
        instrs.len(),
        spine + iter_len,
        "Trace ROM length mismatch: expected {}, got {}",
        spine + iter_len,
        instrs.len()
    );

    Ok((instrs, ctx.clone()))
}

/// Check if an Iso contains a Trace node (for nested-Trace rejection).
fn contains_trace(iso: &Iso) -> bool {
    match iso {
        Iso::Trace(_, _) => true,
        Iso::Seq(f, g) => contains_trace(f) || contains_trace(g),
        Iso::Times(f, g) => contains_trace(f) || contains_trace(g),
        Iso::Plus(f, g) => contains_trace(f) || contains_trace(g),
        Iso::Sym(f) => contains_trace(f),
        Iso::Iter(_, f) => contains_trace(f),
        Iso::Fold(_, f) => contains_trace(f),
        Iso::Unfold(_, f) => contains_trace(f),
        _ => false,
    }
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
    meta: &mut LowerMeta,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    let (depth, elem_ty) = spine_info(l)
        .ok_or_else(|| CompileError::Deferred("Fold/Unfold with non-list-spine type".into()))?;

    let m = num_leaves(&elem_ty)?;
    let n_spine = num_leaves(l)?;
    let stride = 1 + m; // leaves per cons layer: 1 tag + m element

    // Extract accumulator type from body: body : b × c ↔ b × c.
    let (body_in, _) = theseus_core::type_of(body)
        .map_err(|e| CompileError::Deferred(format!("type error in Fold/Unfold body: {e:?}")))?;
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

    // Guard 1: Refuse when the incoming ctx is not the full canonical
    // top-level layout. A displaced/sliced ctx (e.g. fold under Times or
    // nested inside another fold body) means allocate_aux's zero assumption
    // for free registers is violated → silent ROM corruption.
    let canonical = Ctx::new(ctx.homes.len());
    if ctx.homes != canonical.homes {
        return Err(CompileError::Deferred(
            "Fold/Unfold under a non-canonical context (M-D.alloc)".into(),
        ));
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
        let body_ctx = Ctx {
            homes: body_homes,
            occ: dctx.occ,
        };

        // Lower the body against this per-copy context.
        let (body_instrs, body_ctx_out) = lower(body, &body_ctx, meta)?;

        // Guard 3: Home-preservation invariant — promoted from debug_assert
        // to a hard error. A body that permutes homes would corrupt the
        // outer context silently in release builds.
        if body_ctx.homes != body_ctx_out.homes {
            return Err(CompileError::Deferred(
                "Fold/Unfold body permuted homes".into(),
            ));
        }

        // Guard 2: Refuse when the body itself contains a Swapbr — that
        // means the body needs an inner skip block, whose allocate_aux is
        // likewise unsound under the narrow sub-context.
        if has_inner_control(&body_instrs) {
            return Err(CompileError::Deferred(
                "Fold/Unfold body needs inner control (M-D.alloc)".into(),
            ));
        }

        if body_instrs.is_empty() {
            continue;
        }

        // Guard 4: body_len must fit in u8 for the skip-block immediate.
        if body_instrs.len() > 255 {
            return Err(CompileError::Deferred(
                "Fold/Unfold body too long for skip-block (>255 instructions)".into(),
            ));
        }

        // Ensure tag register is accessible.
        let (tag_reg, tag_pi, tag_po) = ensure_reg(&dctx, tag_leaf, &[rb, temp]);

        instrs.extend(tag_pi);
        // skip_when_set=false: skip body iff tag=0 (nil / shorter list).
        meta.forward_skip_total += body_instrs.len();
        instrs.extend(emit_skip_block(tag_reg, &body_instrs, rb, temp, false)?);
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
    meta: &mut LowerMeta,
) -> Result<(Vec<Instr>, Ctx), CompileError> {
    let (in_f, _) = theseus_core::type_of(f)
        .map_err(|e| CompileError::Deferred(format!("type error in Plus/f: {e:?}")))?;
    let (in_g, _) = theseus_core::type_of(g)
        .map_err(|e| CompileError::Deferred(format!("type error in Plus/g: {e:?}")))?;
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
        occ: dctx.occ,
    };
    let ctx_g = Ctx {
        homes: dctx.homes[1..1 + n_g].to_vec(),
        occ: dctx.occ,
    };

    let (instrs_f, _) = lower(f, &ctx_f, meta)?;
    let (instrs_g, _) = lower(g, &ctx_g, meta)?;

    // Guard: reject doubly-nested skip-block control (reservation
    // ordering deferred).
    if has_inner_control(&instrs_f) || has_inner_control(&instrs_g) {
        return Err(CompileError::Deferred(
            "nested skip-block control in Plus arm — reservation ordering deferred".into(),
        ));
    }

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
        meta.forward_skip_total += instrs_f.len();
        instrs.extend(emit_skip_block(tag_reg, &instrs_f, rb, temp, true)?);
    }

    // Phase 2: g runs iff tag=1 → skip g iff tag=0
    if !g_is_nop {
        meta.forward_skip_total += instrs_g.len();
        instrs.extend(emit_skip_block(tag_reg, &instrs_g, rb, temp, false)?);
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
/// Delegates to `marshal_resident(v, ty, 0)` — `home_of` is the sole
/// placement function (I-3 / design §8(1)).
pub fn marshal(v: &Value, ty: &Type) -> StateVals {
    marshal_resident(v, ty, 0)
}

/// Unmarshal a `StateVals` back to a `Value`, reading leaf `i` from
/// its canonical home: `regs[i]` for i < 8, `scratch[i-6]` for
/// 8 ≤ i < 14, `mem[i-14]` for 14 ≤ i < 14+MEM_WORDS.
/// Delegates to `unmarshal_resident(s, ty, 0)` — `home_of` is the sole
/// placement function (I-3 / design §8(1)).
pub fn unmarshal(s: &StateVals, ty: &Type) -> Value {
    unmarshal_resident(s, ty, 0)
}

/// Marshal a `Value` into `StateVals` using `home_of` with a `resident` count.
/// Trailing `resident` leaves land in mem[0..resident]; head leaves use
/// Reg/Scratch as usual. `resident == 0` reproduces `marshal` bit-for-bit.
pub fn marshal_resident(v: &Value, ty: &Type, resident: usize) -> StateVals {
    let bytes = value_to_leaf_bytes(v, ty);
    let n = bytes.len();
    // I-4: reject-don't-corrupt — panic if layout would alias resident words.
    // Only fires when resident > 0 (resident=0 has no collision risk).
    if resident > 0 {
        assert!(
            resident <= MEM_WORDS,
            "marshal_resident: resident ({resident}) > MEM_WORDS ({MEM_WORDS})"
        );
        assert!(
            resident <= n,
            "marshal_resident: resident ({resident}) > n_leaves ({n})"
        );
        let head = n - resident;
        assert!(
            head <= 14,
            "marshal_resident: head leaves ({head}) > 14 — would alias resident words"
        );
    }
    let mut state = StateVals {
        regs: [0; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    for (i, &b) in bytes.iter().enumerate() {
        match home_of(i, n, resident) {
            Location::Reg(r) => state.regs[r as usize] = b,
            Location::Scratch(s) => state.scratch[s as usize] = b,
            Location::Mem(a) => state.mem[a as usize] = b,
        }
    }
    state
}

/// Unmarshal a `StateVals` back to a `Value` using `home_of` with a `resident` count.
/// `resident == 0` reproduces `unmarshal` bit-for-bit.
pub fn unmarshal_resident(s: &StateVals, ty: &Type, resident: usize) -> Value {
    let n = num_leaves(ty).expect("unmarshal_resident: num_leaves");
    // I-4: reject-don't-corrupt — panic if layout would alias resident words.
    // Only fires when resident > 0 (resident=0 has no collision risk).
    if resident > 0 {
        assert!(
            resident <= MEM_WORDS,
            "unmarshal_resident: resident ({resident}) > MEM_WORDS ({MEM_WORDS})"
        );
        assert!(
            resident <= n,
            "unmarshal_resident: resident ({resident}) > n_leaves ({n})"
        );
        let head = n - resident;
        assert!(
            head <= 14,
            "unmarshal_resident: head leaves ({head}) > 14 — would alias resident words"
        );
    }
    let bytes: Vec<u8> = (0..n)
        .map(|i| match home_of(i, n, resident) {
            Location::Reg(r) => s.regs[r as usize],
            Location::Scratch(sx) => s.scratch[sx as usize],
            Location::Mem(a) => s.mem[a as usize],
        })
        .collect();
    let (v, _) = value_from_leaf_bytes(&bytes, ty);
    v
}

/// Public wrapper for `value_to_leaf_bytes` (needed by tests that build
/// forged PrimDefs via raw leaf-byte manipulation).
pub fn value_to_leaf_bytes_pub(v: &Value, ty: &Type) -> Vec<u8> {
    value_to_leaf_bytes(v, ty)
}

/// Public wrapper for `value_from_leaf_bytes`.
pub fn value_from_leaf_bytes_pub(bytes: &[u8], ty: &Type) -> (Value, usize) {
    value_from_leaf_bytes(bytes, ty)
}

// ── WCET budget + park-at-end convention (M-D.w, ADR-0007 TH-1) ─────

/// Static upper bound on total forward-skip distance in a ROM.
///
/// Computed by pairing consecutive `Swapbr` instructions (they come in
/// clean non-overlapping ordered pairs because skip blocks don't nest)
/// and summing each pair's body length = (close_index - open_index - 1).
/// This over-approximates (counts mutually-exclusive Plus arms as
/// both-taken) which is SOUND.
pub fn max_skip_distance(instrs: &[Instr]) -> usize {
    let mut total = 0usize;
    let mut i = 0;
    while i < instrs.len() {
        if matches!(instrs[i], Instr::Swapbr { .. }) {
            // Find the matching close Swapbr
            let open = i;
            i += 1;
            while i < instrs.len() && !matches!(instrs[i], Instr::Swapbr { .. }) {
                i += 1;
            }
            if i < instrs.len() {
                // body_len = close - open - 1
                total += i - open - 1;
            }
            i += 1;
        } else {
            i += 1;
        }
    }
    total
}

/// Static WCET budget B for an `Iso` (ADR-0007 TH-1 ledger).
///
/// Compositional: `B = |instrs| + Σ (k_max − 1)·iter_len` where the
/// first iteration is already counted in `|instrs|`. For monotone
/// programs (no loops) `B == rom.len()`. The invariant `B >= rom.len()`
/// always holds. `k_max = min(k_exact, |u| + 1)` (trajectory-derived exact bound).
pub fn wcet(iso: &Iso) -> Result<usize, CompileError> {
    let lowered = compile_with_metadata(iso)?;
    let b = lowered.wcet();
    debug_assert!(b >= lowered.instrs.len());
    Ok(b)
}

/// Compositional S_max for an `Iso` — forward-skip distance only.
pub fn s_max(iso: &Iso) -> Result<usize, CompileError> {
    let lowered = compile_with_metadata(iso)?;
    Ok(lowered.s_max())
}

// ── Run-on-CPU (available when theseus-eval is linked) ───────────────

/// Run a raw instruction stream on the real CPU (system_step) for `budget`
/// steps with park-safety guards. This is the guarded core factored out of
/// `run_on_cpu` so that negative tests can feed hand-built ROMs.
///
/// `budget` and `s_max_val` are the compositional WCET and forward-skip
/// distance. For hand-built ROMs without loop metadata, pass
/// `instrs.len()` and `max_skip_distance(instrs)`.
///
/// Guard order: P2 (park-safety) → run → P1/P3 (terminal check).
pub fn run_rom_on_cpu<E: std::fmt::Debug>(
    instrs: &[Instr],
    init: StateVals,
    budget: usize,
    s_max_val: usize,
    eval_fn: impl Fn(&Iso, Value, &mut u64) -> Result<Value, E>,
) -> Result<StateVals, CompileError> {
    let rom =
        assemble(instrs).map_err(|e| CompileError::Deferred(format!("assemble failed: {e}")))?;

    // PARK-SAFETY GUARD (P2): parked PC must not wrap.
    if budget + s_max_val > 256 {
        return Err(CompileError::Deferred(format!(
            "parked PC B+S_max={} exceeds PC address space 256; program cannot park by idle (M-D.w)",
            budget + s_max_val
        )));
    }

    // Scale eval fuel with budget (avoid spurious exhaustion on larger programs).
    let mut fuel = (budget as u64) * 500_000_000u64;
    if fuel < 8_000_000_000 {
        fuel = 8_000_000_000;
    }

    // Run exactly B steps of system_step.
    let step_iso = crate::step::system_step(&rom);
    let mut cur = encode_system(&init);
    for _ in 0..budget {
        cur = eval_fn(&step_iso, cur, &mut fuel)
            .map_err(|e| CompileError::Deferred(format!("CPU eval failed: {e:?}")))?;
    }
    let final_state = decode_system(&cur);

    // DYNAMIC TERMINAL CHECK (P1, P3): BR=0, s0=0, s1=0.
    if final_state.br != 0 || final_state.scratch[0] != 0 || final_state.scratch[1] != 0 {
        return Err(CompileError::Deferred(format!(
            "terminal precondition violated (BR={}/s0={}/s1={} != 0); park unsound (M-D.w)",
            final_state.br, final_state.scratch[0], final_state.scratch[1]
        )));
    }

    Ok(final_state)
}

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
    let (in_ty, out_ty) = theseus_core::type_of(iso)
        .map_err(|e| CompileError::Deferred(format!("type error: {e:?}")))?;

    let lowered = compile_with_metadata(iso)?;
    let budget = lowered.wcet();
    let s_max_val = lowered.s_max();
    let state = marshal(input, &in_ty);

    let final_state = run_rom_on_cpu(&lowered.instrs, state, budget, s_max_val, eval_fn)?;

    Ok(unmarshal(&final_state, &out_ty))
}

// ── Fast direct TISC-1 simulator (M-B) ─────────────────────────────
//
// Direct byte-level interpretation of the TISC-1 instruction set,
// matching the step iso's semantics without going through the core-iso
// evaluator. This makes the differential oracle practical for M-B
// programs with larger ROMs (e.g. guarded-unroll iter).

/// Execute one TISC-1 instruction on the machine state (direct simulation).
pub fn exec_instr(instr: Instr, state: &mut StateVals) {
    // A-0 (BLOCKER-2 fix): rd/rs/rt are operand indices over reg∪scratch
    // (0..8 → r, 8..16 → s), matching the datapath's `select_operand`
    // crossbar; use the `operand_*` accessors so the fast sim agrees with
    // the hardware on scratch operands. `sx`/`addr` remain scratch/mem
    // indices (not operand indices) and index directly.
    match instr {
        Instr::Add { rd, rs } => {
            state.operand_set(
                rd,
                state.operand_get(rd).wrapping_add(state.operand_get(rs)),
            );
        }
        Instr::Sub { rd, rs } => {
            state.operand_set(
                rd,
                state.operand_get(rd).wrapping_sub(state.operand_get(rs)),
            );
        }
        Instr::Xor { rd, rs } => {
            state.operand_set(rd, state.operand_get(rd) ^ state.operand_get(rs));
        }
        Instr::Exch { rd, rs } => {
            state.operand_swap(rd, rs);
        }
        Instr::Rotl { rd, rs } => {
            let amount = state.operand_get(rs) & 7;
            state.operand_set(rd, state.operand_get(rd).rotate_left(amount as u32));
        }
        Instr::Rotr { rd, rs } => {
            let amount = state.operand_get(rs) & 7;
            state.operand_set(rd, state.operand_get(rd).rotate_right(amount as u32));
        }
        Instr::Cswap { rd, rs, rt } => {
            if state.operand_get(rd) & 1 == 1 {
                state.operand_swap(rs, rt);
            }
        }
        Instr::Ccnot { rd, rs, rt } => {
            state.operand_set(
                rd,
                state.operand_get(rd) ^ (state.operand_get(rs) & state.operand_get(rt)),
            );
        }
        Instr::Xchg { rd, sx } => {
            let rv = state.operand_get(rd);
            let sv = state.scratch[sx as usize];
            state.operand_set(rd, sv);
            state.scratch[sx as usize] = rv;
        }
        Instr::Mexch { rd, addr } => {
            let rv = state.operand_get(rd);
            let mv = state.mem[addr as usize];
            state.operand_set(rd, mv);
            state.mem[addr as usize] = rv;
        }
        Instr::MexchReg { rd, ra } => {
            let addr = (state.operand_get(ra) as usize) % MEM_WORDS;
            let rv = state.operand_get(rd);
            let mv = state.mem[addr];
            state.operand_set(rd, mv);
            state.mem[addr] = rv;
        }
        Instr::Swapbr { rd } => {
            let rv = state.operand_get(rd);
            state.operand_set(rd, state.br);
            state.br = rv;
        }
        Instr::Addi { rd, imm } => {
            state.operand_set(rd, state.operand_get(rd).wrapping_add(imm));
        }
        Instr::Subi { rd, imm } => {
            state.operand_set(rd, state.operand_get(rd).wrapping_sub(imm));
        }
        Instr::Xori { rd, imm } => {
            state.operand_set(rd, state.operand_get(rd) ^ imm);
        }
        Instr::Rotli { rd, imm } => {
            state.operand_set(rd, state.operand_get(rd).rotate_left((imm & 7) as u32));
        }
        Instr::Rotri { rd, imm } => {
            state.operand_set(rd, state.operand_get(rd).rotate_right((imm & 7) as u32));
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
pub fn run_on_cpu_fast(iso: &Iso, input: &Value) -> Result<Value, CompileError> {
    let (in_ty, out_ty) = theseus_core::type_of(iso)
        .map_err(|e| CompileError::Deferred(format!("type error: {e:?}")))?;

    let lowered = compile_with_metadata(iso)?;
    let rom = assemble(&lowered.instrs)
        .map_err(|e| CompileError::Deferred(format!("assemble failed: {e}")))?;
    let budget = lowered.wcet();

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

/// Resident-aware real-CPU oracle: compile with `resident`, marshal with
/// `resident`, run on the real system_step, unmarshal with `resident`.
/// F-9: asserts `resident > 0 ⇒ num_leaves(in_ty) == num_leaves(out_ty)`.
pub fn run_on_cpu_resident<E: std::fmt::Debug>(
    iso: &Iso,
    input: &Value,
    resident: usize,
    eval_fn: impl Fn(&Iso, Value, &mut u64) -> Result<Value, E>,
) -> Result<Value, CompileError> {
    let (in_ty, out_ty) = theseus_core::type_of(iso)
        .map_err(|e| CompileError::Deferred(format!("type error: {e:?}")))?;
    if resident > 0 {
        let n_in = num_leaves(&in_ty)?;
        let n_out = num_leaves(&out_ty)?;
        assert_eq!(
            n_in, n_out,
            "run_on_cpu_resident: resident>0 requires num_leaves(in)==num_leaves(out), got {n_in} vs {n_out}"
        );
    }

    let lowered = compile_with_metadata_resident(iso, resident)?;
    let budget = lowered.wcet();
    let s_max_val = lowered.s_max();
    let state = marshal_resident(input, &in_ty, resident);

    let final_state = run_rom_on_cpu(&lowered.instrs, state, budget, s_max_val, eval_fn)?;

    Ok(unmarshal_resident(&final_state, &out_ty, resident))
}

/// Resident-aware fast-sim oracle: compile with `resident`, marshal with
/// `resident`, run the direct simulator, unmarshal with `resident`.
pub fn run_on_cpu_fast_resident(
    iso: &Iso,
    input: &Value,
    resident: usize,
) -> Result<Value, CompileError> {
    let (in_ty, out_ty) = theseus_core::type_of(iso)
        .map_err(|e| CompileError::Deferred(format!("type error: {e:?}")))?;
    if resident > 0 {
        let n_in = num_leaves(&in_ty)?;
        let n_out = num_leaves(&out_ty)?;
        assert_eq!(
            n_in, n_out,
            "run_on_cpu_fast_resident: resident>0 requires num_leaves(in)==num_leaves(out), got {n_in} vs {n_out}"
        );
    }

    let lowered = compile_with_metadata_resident(iso, resident)?;
    let rom = assemble(&lowered.instrs)
        .map_err(|e| CompileError::Deferred(format!("assemble failed: {e}")))?;
    let budget = lowered.wcet();

    let mut state = marshal_resident(input, &in_ty, resident);

    for _ in 0..budget {
        let pc = state.pc as usize;
        let word = if pc < rom.len() { rom[pc] } else { 0 };
        if let Some(instr) = crate::asm::decode(word) {
            exec_instr(instr, &mut state);
        }
        state.pc = state.pc.wrapping_add(state.br).wrapping_add(1);
    }

    Ok(unmarshal_resident(&state, &out_ty, resident))
}

/// Fast-sim helper: run a pre-assembled ROM for an explicit number of steps.
/// Used by park-liveness tests (T2a/T2b) to verify the dead zone is inert.
pub fn run_rom_fast(rom: &[u32], mut state: StateVals, steps: usize) -> StateVals {
    for _ in 0..steps {
        let pc = state.pc as usize;
        let word = if pc < rom.len() { rom[pc] } else { 0 };
        if let Some(instr) = crate::asm::decode(word) {
            exec_instr(instr, &mut state);
        }
        state.pc = state.pc.wrapping_add(state.br).wrapping_add(1);
    }
    state
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
            symbolic: None,
        }
    }

    #[test]
    fn recognize_prim_aliased_registers() {
        let def = swap_prim_def();
        // Craft a context where two leaves alias the same register.
        let ctx = Ctx {
            homes: vec![Location::Reg(0), Location::Reg(0)],
            occ: OccSet::from_locations(&[Location::Reg(0)]),
        };
        let result = recognize_prim(&def, &ctx);
        assert!(
            matches!(result, Err(CompileError::AliasedRegisters { .. })),
            "expected AliasedRegisters, got {result:?}"
        );
    }
}
