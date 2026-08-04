//! A-6.0 -- the transcriber core: deterministic `RegTransfer` → `Instr` gates
//! for the IN-PLACE, ANCILLA-FREE structured class.
//!
//! This is the automation of the hand-written transcriptions in the keystone
//! POC (`tests/bvexpr_synth_poc.rs`). It is **deterministic transcription**, a
//! direct structural walk of each register row's `BvExpr` tree into ISA gates --
//! NOT search. There is no enumeration, no beam, no brute force.
//!
//! # The in-place ancilla-free class
//!
//! Each register row of the spec is one of:
//! - **identity**: `Var(r, b)` at every bit `b` -- the register is untouched →
//!   emit no gate.
//! - **affine XOR of whole registers**: `x_new = Var(x, ·) ⊕ Σ Var(y_i, ·)`,
//!   matching bit positions (bit `b` of the target is bit `b` of each source) →
//!   one `Xor { rd: x, rs: y_i }` per source term.
//! - **constant flip**: `x_new = Var(x, ·) ⊕ const` (some bits negated) →
//!   one `Xori { rd: x, imm }` with `imm` the flip mask.
//! - **byte-parallel Toffoli**: `x_new = Var(x, ·) ⊕ (Var(y, ·) ∧ Var(z, ·))` →
//!   one `Ccnot { rd: x, rs: y, rt: z }`.
//! - a **bit-0-controlled swap** across two registers (`Mux(Var(c,0), ...)`) →
//!   one `Cswap { rd: c, rs: u, rt: v }`.
//!
//! Anything else -- a row that is not `old ⊕ f(others)` in the ISA basis, or a
//! set of rows that cannot be ordered so every read happens before the
//! corresponding write -- is **rejected** with a [`SynthError`]. Those specs are
//! A-6.1's Bennett job (zero-init ancilla + backward uncompute), not this
//! slice's.
//!
//! # Verification
//!
//! Callers verify the emission with the existing symbolic oracle:
//! `equiv(symbolic_exec(&transcribe_inplace(spec)?), spec, BITS, &vars)`. The
//! transcriber never claims correctness itself -- the BDD oracle is the acceptance
//! gate (it has teeth: a wrong emission is rejected).

use crate::asm::Instr;
use crate::bveq::{expr_to_bdd, BvExpr, RegTransfer, VarMap, BITS};
use crate::types::NUM_REGS;

/// Why a spec could not be transcribed into the in-place ancilla-free class.
///
/// Every variant means "out of A-6.0's class → this is A-6.1's Bennett job",
/// NOT "the spec is wrong". Returning `Err` here is the correct behavior for the
/// general permutation case.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SynthError {
    /// The spec frame is not `NUM_REGS` rows, or a row is not `BITS` bits.
    MalformedSpec { detail: String },
    /// A register row is not of the in-place form `old ⊕ f(others)` in the ISA
    /// gate basis (e.g. it overwrites the target with a value not XOR-ed onto
    /// its own old contents, or uses an unsupported expression shape).
    RowNotInPlace { reg: usize, detail: String },
    /// The rows cannot be scheduled so that every register a row reads still
    /// holds its input value when that row runs (a read-after-write conflict
    /// among the modified registers). This is the Bennett case.
    OrderingConflict { detail: String },
}

impl std::fmt::Display for SynthError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SynthError::MalformedSpec { detail } => write!(f, "malformed spec: {detail}"),
            SynthError::RowNotInPlace { reg, detail } => {
                write!(f, "reg {reg} row not in-place-realizable: {detail}")
            }
            SynthError::OrderingConflict { detail } => {
                write!(f, "ordering conflict (Bennett case): {detail}")
            }
        }
    }
}

impl std::error::Error for SynthError {}

/// One recognized register row, before scheduling.
///
/// Each variant carries the gate(s) it emits plus the read/write footprint the
/// scheduler needs to order the rows safely.
#[derive(Debug, Clone)]
enum Row {
    /// Identity: no gate, reads/writes nothing.
    Identity,
    /// `x ^= Σ Var(y_i, ·)` (affine) and/or `x ^= const`.
    ///
    /// `target` is written; `xor_sources` are read; `const_mask` (if nonzero)
    /// contributes one `Xori`.
    Affine {
        target: usize,
        xor_sources: Vec<usize>,
        const_mask: u8,
    },
    /// `x ^= Σ Var(y_i,·) ⊕ const ⊕ (Var(rs,·) ∧ Var(rt,·))` -- the combined
    /// affine/const + single-Ccnot in-place row (POC `affine_nonlinear_mix`).
    /// Emits the Xor/Xori gates then the Ccnot; all XOR-accumulate onto target.
    AffineCcnot {
        target: usize,
        xor_sources: Vec<usize>,
        const_mask: u8,
        rs: usize,
        rt: usize,
    },
    /// bit-0-controlled swap of two registers → one `Cswap`.
    ///
    /// This row writes BOTH `u` and `v` and reads `c`, `u`, `v`.
    Cswap { ctrl: usize, u: usize, v: usize },
}

impl Row {
    /// Registers this row writes (overwrites).
    fn writes(&self) -> Vec<usize> {
        match self {
            Row::Identity => vec![],
            Row::Affine { target, .. } => vec![*target],
            Row::AffineCcnot { target, .. } => vec![*target],
            Row::Cswap { u, v, .. } => vec![*u, *v],
        }
    }

    /// Registers this row reads (must still hold their input value when run).
    ///
    /// For an in-place `x ^= f(others)` row the target is read too, but it is
    /// read AND written by the same gate in one step, so it does not constrain
    /// ordering against itself -- we only list the *other* registers it reads.
    fn reads_others(&self) -> Vec<usize> {
        match self {
            Row::Identity => vec![],
            Row::Affine { xor_sources, .. } => xor_sources.clone(),
            Row::AffineCcnot {
                xor_sources,
                rs,
                rt,
                ..
            } => {
                let mut v = xor_sources.clone();
                v.push(*rs);
                v.push(*rt);
                v
            }
            // A Cswap reads its control and both data regs; all three are also
            // its footprint. The data regs are its own writes (handled by the
            // in-one-gate rule); the control is an external read.
            Row::Cswap { ctrl, .. } => vec![*ctrl],
        }
    }

    /// Emit the gate(s) for this row.
    fn emit(&self) -> Vec<Instr> {
        match self {
            Row::Identity => vec![],
            Row::Affine {
                target,
                xor_sources,
                const_mask,
            } => {
                let mut out = Vec::new();
                for &y in xor_sources {
                    out.push(Instr::Xor {
                        rd: *target as u8,
                        rs: y as u8,
                    });
                }
                if *const_mask != 0 {
                    out.push(Instr::Xori {
                        rd: *target as u8,
                        imm: *const_mask,
                    });
                }
                out
            }
            Row::AffineCcnot {
                target,
                xor_sources,
                const_mask,
                rs,
                rt,
            } => {
                let mut out = Vec::new();
                for &y in xor_sources {
                    out.push(Instr::Xor {
                        rd: *target as u8,
                        rs: y as u8,
                    });
                }
                if *const_mask != 0 {
                    out.push(Instr::Xori {
                        rd: *target as u8,
                        imm: *const_mask,
                    });
                }
                out.push(Instr::Ccnot {
                    rd: *target as u8,
                    rs: *rs as u8,
                    rt: *rt as u8,
                });
                out
            }
            Row::Cswap { ctrl, u, v } => vec![Instr::Cswap {
                rd: *ctrl as u8,
                rs: *u as u8,
                rt: *v as u8,
            }],
        }
    }
}

/// Transcribe an in-place ancilla-free `RegTransfer` spec into an `Instr`
/// sequence, or return a [`SynthError`] for specs outside that class.
///
/// See the module docs for the exact class. The result, when `Ok`, satisfies
/// `equiv(symbolic_exec(&result)?, spec, BITS, &operand_regs) == Ok(())` -- but
/// callers should still run that oracle; it is the acceptance gate, not this
/// function.
pub fn transcribe_inplace(spec: &RegTransfer) -> Result<Vec<Instr>, SynthError> {
    // ── Frame validation ────────────────────────────────────────────
    if spec.regs.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: format!("frame has {} rows, expected {NUM_REGS}", spec.regs.len()),
        });
    }
    for (r, row) in spec.regs.iter().enumerate() {
        if row.len() != BITS {
            return Err(SynthError::MalformedSpec {
                detail: format!("reg {r} row has {} bits, expected {BITS}", row.len()),
            });
        }
    }

    // ── Recognize each row's shape ──────────────────────────────────
    // A Cswap row spans TWO registers (u, v) whose expressions are the swap
    // mux pair. We recognize it once and mark both registers consumed.
    let mut recognized: Vec<Option<Row>> = vec![None; NUM_REGS];

    for r in 0..NUM_REGS {
        if recognized[r].is_some() {
            continue; // already claimed by a Cswap partner
        }
        // Identity first (cheapest, most common).
        if is_identity_row(&spec.regs[r], r) {
            recognized[r] = Some(Row::Identity);
            continue;
        }
        // Try the bit-0-controlled swap shape (writes two regs).
        if let Some((ctrl, u, v)) = recognize_cswap(spec, r) {
            let row = Row::Cswap { ctrl, u, v };
            recognized[u] = Some(row.clone());
            recognized[v] = Some(row);
            continue;
        }
        // Otherwise it must be an in-place `old ⊕ f(others)` row.
        recognized[r] = Some(recognize_inplace_row(&spec.regs[r], r)?);
    }

    let rows: Vec<Row> = recognized
        .into_iter()
        .map(|o| o.expect("every reg row recognized"))
        .collect();

    // ── Schedule in a dependency-safe order ─────────────────────────
    let ordered = schedule(&rows)?;

    // ── Emit ─────────────────────────────────────────────────────────
    let mut out = Vec::new();
    for row in ordered {
        out.extend(row.emit());
    }
    Ok(out)
}

/// True iff row `r` is the identity `Var(r, b)` at every bit.
fn is_identity_row(row: &[BvExpr], r: usize) -> bool {
    row.iter()
        .enumerate()
        .all(|(b, e)| matches!(e, BvExpr::Var { operand, bit } if *operand == r && *bit == b))
}

/// Recognize a byte-parallel in-place row `x ^= f(others)` where `f` is in the
/// ISA gate basis realizable by a single in-place row (affine + const, or one
/// Ccnot). Returns `Err(RowNotInPlace)` for anything else.
fn recognize_inplace_row(row: &[BvExpr], r: usize) -> Result<Row, SynthError> {
    // Every bit must be `Var(r, b) ⊕ rest_b`. Peel the self-term off the top of
    // each bit's XOR tree; the remainder `rest_b` is `f`'s contribution to bit b.
    let mut rest: Vec<BvExpr> = Vec::with_capacity(BITS);
    for (b, e) in row.iter().enumerate() {
        match peel_self_xor(e, r, b) {
            Some(remainder) => rest.push(remainder),
            None => {
                return Err(SynthError::RowNotInPlace {
                    reg: r,
                    detail: format!("bit {b} is not `Var({r},{b}) ⊕ f`; got {e:?}"),
                });
            }
        }
    }

    // Now classify `rest` (the per-bit XOR-delta applied to the register).
    // It is a flat XOR of terms; each term is one of:
    //   - Var(y, b)            (whole-register affine source y)
    //   - Const(true)          (constant bit-flip at bit b)
    //   - Var(y,b) ∧ Var(z,b)  (the single Ccnot term)
    // We gather affine sources / const mask, and AT MOST ONE And-term.
    let mut const_mask: u8 = 0;

    // Collect the flat XOR terms of each bit; they must be structurally uniform
    // across bits (a whole-register byte-parallel operation) so that a single
    // gate realizes all 8 bits at once.
    let per_bit_terms: Vec<Vec<&BvExpr>> = rest.iter().map(flatten_xor).collect();

    // Determine the term *shapes* from bit 0..7 and check uniformity.
    // We categorize each term and verify the same multiset of categories holds
    // (with the right bit) across all bits.
    //
    // Strategy: process bit 0's terms to establish the source set / and-term /
    // (bit-0 const), then confirm every other bit matches (same sources at that
    // bit, same and-term at that bit), and record const bits per bit position.

    // First pass: from EACH bit collect (affine sources at that bit, const at
    // that bit, and-term at that bit).
    struct BitClass {
        sources: Vec<usize>,
        has_const: bool,
        and_term: Option<(usize, usize)>,
    }
    let mut classes: Vec<BitClass> = Vec::with_capacity(BITS);
    for (b, terms) in per_bit_terms.iter().enumerate() {
        let mut sources = Vec::new();
        // const is a PARITY fold, not a bool -- `x ⊕ 1 ⊕ 1` is identity,
        // so two Const(true) terms at one bit cancel (no flip).
        let mut const_parity = false;
        let mut at_bit_and: Option<(usize, usize)> = None;
        for t in terms {
            match classify_term(t, b) {
                Some(Term::Var(y)) => sources.push(y),
                Some(Term::Const) => const_parity ^= true,
                Some(Term::And(y, z)) => {
                    if at_bit_and.is_some() {
                        return Err(SynthError::RowNotInPlace {
                            reg: r,
                            detail: format!(
                                "bit {b} has more than one AND term (needs ≥2 Ccnots -- Bennett)"
                            ),
                        });
                    }
                    at_bit_and = Some((y, z));
                }
                None => {
                    return Err(SynthError::RowNotInPlace {
                        reg: r,
                        detail: format!("bit {b} has an unsupported XOR term: {t:?}"),
                    });
                }
            }
        }
        sources.sort_unstable();
        classes.push(BitClass {
            sources,
            has_const: const_parity,
            and_term: at_bit_and,
        });
    }

    // Establish the affine source set and the and-term from bit 0, then require
    // every bit to match (byte-parallel uniformity -- one gate does all 8 bits).
    let xor_sources = classes[0].sources.clone();
    let and_term = classes[0].and_term;
    for (b, c) in classes.iter().enumerate() {
        if c.sources != xor_sources {
            return Err(SynthError::RowNotInPlace {
                reg: r,
                detail: format!(
                    "affine sources differ across bits (bit 0: {xor_sources:?}, bit {b}: {:?}) \
                     -- not a whole-register byte-parallel op",
                    c.sources
                ),
            });
        }
        if c.and_term != and_term {
            return Err(SynthError::RowNotInPlace {
                reg: r,
                detail: format!(
                    "AND term differs across bits (bit 0: {and_term:?}, bit {b}: {:?})",
                    c.and_term
                ),
            });
        }
        if c.has_const {
            const_mask |= 1 << b;
        }
    }

    // A single in-place row realizes: {affine sources}·Xor + {const}·Xori + AT
    // MOST ONE Ccnot. If there is both an and-term AND affine/const work, that
    // is still fine -- but it is TWO gate kinds on one target. The POC's
    // `affine_nonlinear_mix` is exactly `x ^= w ⊕ (y∧z)` → [Xor, Ccnot]. So we
    // support the combination on a single target (Ccnot after the Xor/Xori,
    // both XOR-accumulate onto the target -- order among them is immaterial).
    if let Some((y, z)) = and_term {
        // MINOR: the emitted `Ccnot { rd: r, rs: y, rt: z }` must have pairwise
        // DISTINCT operands (ISA-illegal / non-bijective otherwise -- asm::encode
        // rejects it). The AND term touching the target (`y == r` or `z == r`, the
        // spec `x ^= x∧z` CE) is non-in-place anyway; `y == z` (`x ^= y∧y = y`)
        // is degenerate. Reject rather than emit an unassemblable body.
        if y == z || y == r || z == r {
            return Err(SynthError::RowNotInPlace {
                reg: r,
                detail: format!(
                    "Ccnot operands not pairwise-distinct (rd={r}, rs={y}, rt={z}); \
                     the AND term touches the target or repeats an operand -- non-bijective"
                ),
            });
        }
        // Represent as an Affine row (for the xor/const part) FOLLOWED by a
        // Ccnot on the same target. We fold both into the emitted sequence via a
        // combined row: reuse Affine for xor/const then a Ccnot. To keep the Row
        // enum simple and the read/write footprint honest, emit a small compound.
        // We model it as two logical rows sharing the target; but the scheduler
        // treats a target as a single unit, so we instead return a dedicated
        // combined representation.
        return Ok(Row::from_affine_plus_ccnot(
            r,
            xor_sources,
            const_mask,
            y,
            z,
        ));
    }

    Ok(Row::Affine {
        target: r,
        xor_sources,
        const_mask,
    })
}

// ── A-6.1: transcribe_into (fresh disjoint targets) ──────────────────

/// Transcribe a `RegTransfer` spec into gates that XOR each row's function of
/// the SOURCE operands onto a set of FRESH, disjoint DESTINATION operands.
///
/// Semantics: for each spec row `i`, emit gates such that
/// `dst_ops[i] ^= (spec.regs[i], with every Var(r,·) reading src_ops[r])`,
/// **assuming `dst_ops` start ZERO and are DISJOINT from `src_ops`**.
///
/// This is STRICTLY SIMPLER than the in-place case ([`transcribe_inplace`]):
/// because the targets are fresh and disjoint from the sources, there is no
/// self-term to peel, no read-after-write hazard among the outputs, and no
/// scheduling -- each output row is emitted independently, reading only sources
/// that are never written by this routine. It is the building block A-6.1's
/// Bennett wrapper composes (compute `O ^= P(x)`, uncompute `O ^= P⁻¹(y)`).
///
/// `dst_ops` / `src_ops` are raw **operand indices** over reg∪scratch (A-0:
/// `0..8` = registers r0..r7, `8..16` = scratch s0..s7). They may name
/// registers OR scratch words; `transcribe_into` never inspects the model, it
/// just wires the recognized gate shapes onto the given operand indices.
///
/// Row shapes recognized (per spec row, byte-parallel across all `BITS`):
/// - **copy / affine**: `Σ Var(y_i,·)` → one `Xor { rd: dst, rs: src_ops[y_i] }`
///   per source term (an identity row `Var(i,·)` is just the single source `i`);
/// - **constant**: a `Const(true)` at some bits → one `Xori { rd: dst, imm }`;
/// - **single Toffoli term**: `Var(y,·) ∧ Var(z,·)` → one
///   `Ccnot { rd: dst, rs: src_ops[y], rt: src_ops[z] }`.
///
/// Anything outside this basis (e.g. a `Mux`, a second AND term on one bit, or a
/// shape not uniform across bits) is rejected with [`SynthError`]. Callers
/// verify the emission with the symbolic oracle (`symbolic_exec` + `equiv`) when
/// the operands are all registers, and/or `exec_instr` for the general case.
pub fn transcribe_into(
    spec: &RegTransfer,
    dst_ops: &[u8],
    src_ops: &[u8],
) -> Result<Vec<Instr>, SynthError> {
    // ── Frame + operand-map validation ──────────────────────────────
    if spec.regs.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: format!("frame has {} rows, expected {NUM_REGS}", spec.regs.len()),
        });
    }
    if dst_ops.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "dst_ops has {} entries, expected one per spec row ({NUM_REGS})",
                dst_ops.len()
            ),
        });
    }
    if src_ops.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "src_ops has {} entries, expected one per spec reg ({NUM_REGS})",
                src_ops.len()
            ),
        });
    }
    for (r, row) in spec.regs.iter().enumerate() {
        if row.len() != BITS {
            return Err(SynthError::MalformedSpec {
                detail: format!("reg {r} row has {} bits, expected {BITS}", row.len()),
            });
        }
    }

    // ── validate the DESTINATION operand map. Only rows
    // that actually emit gates matter (a spec-identity row whose dst self-maps
    // to its own src contributes nothing). For every row that will
    // emit, the dst operand must be a legal operand and pairwise-distinct from
    // every OTHER emitting row's dst (aliased fresh targets clobber each other).
    let mut used_dsts: Vec<u8> = Vec::new();
    for (i, row) in spec.regs.iter().enumerate() {
        let dst = dst_ops[i];
        // the skip fires ONLY on a genuine IDENTITY row that self-maps
        // (dst == src for its own reg). Any NON-identity row that happens to
        // alias a source is a caller contract violation, not a no-op -- never
        // silently drop its gates.
        if is_identity_row(row, i) && dst == src_ops[i] {
            continue;
        }
        check_operand_legal("dst", dst)?;
        if used_dsts.contains(&dst) {
            return Err(SynthError::MalformedSpec {
                detail: format!(
                    "dst operand {dst} used by two rows -- fresh targets must be distinct"
                ),
            });
        }
        used_dsts.push(dst);
    }

    let mut out = Vec::new();
    for (i, row) in spec.regs.iter().enumerate() {
        let dst = dst_ops[i];
        // Recognize this row as a byte-parallel `f(vars)` in the ISA basis.
        // NOTE: unlike the in-place case, there is NO self-term to peel -- the
        // fresh target already holds 0 and we XOR the whole function onto it.
        let (xor_sources, const_mask, and_term) = recognize_into_row(row, i)?;
        // only skip genuine identity self-maps (validated above). If we
        // reach here with a non-identity row, every gate is emitted -- no source
        // may alias the dst (that would be an ISA-illegal self-XOR / self-Ccnot).
        let is_id_selfmap = is_identity_row(row, i) && dst == src_ops[i];
        if is_id_selfmap {
            continue;
        }
        for &y in &xor_sources {
            let rs = src_ops[y];
            // A copy where dst == src on a non-identity row is an ISA-illegal
            // self-XOR AND silently drops a needed term (CE:
            // `row0 = v0⊕v1`, dst[0]=1 would drop the self-copy). Reject.
            if rs == dst {
                return Err(SynthError::MalformedSpec {
                    detail: format!(
                        "row {i}: source operand {rs} aliases dst {dst} (self-XOR is \
                         ISA-illegal; the row is not a fresh-target copy)"
                    ),
                });
            }
            out.push(Instr::Xor { rd: dst, rs });
        }
        if const_mask != 0 {
            out.push(Instr::Xori {
                rd: dst,
                imm: const_mask,
            });
        }
        if let Some((y, z)) = and_term {
            let (rs, rt) = (src_ops[y], src_ops[z]);
            // MINOR: the Ccnot operands must be pairwise-distinct (ISA-illegal /
            // non-bijective otherwise -- asm::encode rejects it).
            if rs == rt || rs == dst || rt == dst {
                return Err(SynthError::RowNotInPlace {
                    reg: i,
                    detail: format!(
                        "row {i}: Ccnot operands not pairwise-distinct (rd={dst}, rs={rs}, \
                         rt={rt})"
                    ),
                });
            }
            out.push(Instr::Ccnot {
                rd: dst,
                rs: src_ops[y],
                rt: src_ops[z],
            });
        }
    }
    Ok(out)
}

/// Number of legal operand locations (reg ∪ scratch): 0..16.
const NUM_OPERAND_LOCS: usize = 16;

/// Validate a raw operand index against the LEGAL operand space (mirrors
/// `asm::check_operand`): `0..8` = registers, `10..16` = scratch s2..s7.
/// Rejected: `8`/`9` (decoder-reserved s0/s1) and `≥16` (no crossbar route,
/// and ≥16 would panic `operand_get`). Returns `Err(MalformedSpec)` -- a
/// verification-time reject, not a ROM-time panic.
fn check_operand_legal(what: &str, op: u8) -> Result<(), SynthError> {
    // The operand cap is the strict machine cap `NUM_OPERAND_LOCS` in
    // production. Under the `measurement` feature it may be RAISED for the
    // duration of `transcribe_word_bennett_peak_measure` (thread-local) so an
    // over-budget body can PLACE for length/peak COUNTING using the future
    // widen-state scratch slots (idx 16..=19); behavior is byte-identical off
    // the measurement path. The reserved s0/s1 (8/9) are rejected regardless.
    // Bodies emitted under the relaxed cap are NOT executable (exec_instr
    // panics at idx >= 16).
    let cap = current_operand_cap();
    if (op as usize) >= cap {
        return Err(SynthError::MalformedSpec {
            detail: format!("{what} operand {op} is out of range (≥{cap})"),
        });
    }
    if op == NUM_REGS as u8 || op == NUM_REGS as u8 + 1 {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "{what} operand {op} names decoder-reserved scratch s{} (s0/s1 unusable)",
                op - NUM_REGS as u8
            ),
        });
    }
    Ok(())
}

/// Current exclusive upper bound for operand-index legality. In production this
/// is the strict machine cap `NUM_OPERAND_LOCS` (=16). Under the `measurement`
/// feature it reads the thread-local cap, which is raised only inside
/// `transcribe_word_bennett_peak_measure`.
#[cfg(not(feature = "measurement"))]
#[inline]
fn current_operand_cap() -> usize {
    NUM_OPERAND_LOCS
}

#[cfg(feature = "measurement")]
#[inline]
fn current_operand_cap() -> usize {
    RELAXED_OPERAND_CAP.with(|c| c.get())
}

#[cfg(feature = "measurement")]
thread_local! {
    /// Exclusive upper bound for operand-index legality. Defaults to the strict
    /// machine cap `NUM_OPERAND_LOCS` (=16). Raised ONLY inside
    /// `transcribe_word_bennett_peak_measure` (measurement-only) so over-budget
    /// bodies can PLACE for length/peak counting; restored on exit (RAII guard).
    static RELAXED_OPERAND_CAP: std::cell::Cell<usize> = const { std::cell::Cell::new(NUM_OPERAND_LOCS) };
}

/// RAII guard that raises `RELAXED_OPERAND_CAP` for the duration of a
/// MEASUREMENT-ONLY word-Bennett placement and restores it on drop (even on the
/// early-return `?` paths). Production code never constructs this guard.
#[cfg(feature = "measurement")]
struct RelaxedOperandCap {
    prev: usize,
}
#[cfg(feature = "measurement")]
impl RelaxedOperandCap {
    fn new(cap: usize) -> Self {
        let prev = RELAXED_OPERAND_CAP.with(|c| c.replace(cap));
        RelaxedOperandCap { prev }
    }
}
#[cfg(feature = "measurement")]
impl Drop for RelaxedOperandCap {
    fn drop(&mut self) {
        let prev = self.prev;
        RELAXED_OPERAND_CAP.with(|c| c.set(prev));
    }
}

/// Recognized fresh-target row: `(affine sources, const mask, optional single
/// AND term)`. Indices are SPEC register indices (into `src_ops`).
type IntoRow = (Vec<usize>, u8, Option<(usize, usize)>);

/// Recognize a fresh-target row `f(vars)` (no self-term) as an [`IntoRow`],
/// byte-parallel across all bits. Reused by [`transcribe_into`].
fn recognize_into_row(row: &[BvExpr], r: usize) -> Result<IntoRow, SynthError> {
    // Classify each bit's flat-XOR terms (identical machinery to the in-place
    // recognizer, but WITHOUT peeling a self-term first).
    struct BitClass {
        sources: Vec<usize>,
        has_const: bool,
        and_term: Option<(usize, usize)>,
    }
    let mut classes: Vec<BitClass> = Vec::with_capacity(BITS);
    for (b, e) in row.iter().enumerate() {
        let terms = flatten_xor(e);
        let mut sources = Vec::new();
        // parity fold -- even count of Const(true) is no flip.
        let mut const_parity = false;
        let mut at_bit_and: Option<(usize, usize)> = None;
        for t in terms {
            match classify_term(t, b) {
                Some(Term::Var(y)) => sources.push(y),
                Some(Term::Const) => const_parity ^= true,
                Some(Term::And(y, z)) => {
                    if at_bit_and.is_some() {
                        return Err(SynthError::RowNotInPlace {
                            reg: r,
                            detail: format!(
                                "bit {b} has more than one AND term (needs ≥2 Ccnots -- \
                                 outside transcribe_into's single-Toffoli basis)"
                            ),
                        });
                    }
                    at_bit_and = Some((y, z));
                }
                None => {
                    return Err(SynthError::RowNotInPlace {
                        reg: r,
                        detail: format!("bit {b} has an unsupported XOR term: {t:?}"),
                    });
                }
            }
        }
        sources.sort_unstable();
        classes.push(BitClass {
            sources,
            has_const: const_parity,
            and_term: at_bit_and,
        });
    }

    // Byte-parallel uniformity: the source set and AND term must be identical
    // across all bits (one gate realizes all 8 bits); the const may vary per bit.
    let xor_sources = classes[0].sources.clone();
    let and_term = classes[0].and_term;
    let mut const_mask: u8 = 0;
    for (b, c) in classes.iter().enumerate() {
        if c.sources != xor_sources {
            return Err(SynthError::RowNotInPlace {
                reg: r,
                detail: format!(
                    "affine sources differ across bits (bit 0: {xor_sources:?}, bit {b}: {:?}) \
                     -- not a whole-register byte-parallel op",
                    c.sources
                ),
            });
        }
        if c.and_term != and_term {
            return Err(SynthError::RowNotInPlace {
                reg: r,
                detail: format!(
                    "AND term differs across bits (bit 0: {and_term:?}, bit {b}: {:?})",
                    c.and_term
                ),
            });
        }
        if c.has_const {
            const_mask |= 1 << b;
        }
    }
    Ok((xor_sources, const_mask, and_term))
}

// --- Shared Bennett-transcriber validation / construction helpers ---
//
// The three Bennett transcribers (transcribe_bennett, transcribe_bennett_general,
// transcribe_bdd_bennett) and the width-aware emit_* gadgets share several
// byte-identical validation/construction blocks. These private helpers hold the
// single copy; every call site routes through them with IDENTICAL logic and
// identical emitted output (error `detail` strings included). Where a call site
// differs even slightly (e.g. the BDD variant's temp+node disjointness, which
// legitimately unifies more working words) the block is kept inline at that site.

/// Legal-operand + no-duplicate validation shared by `transcribe_bennett` and
/// `transcribe_bdd_bennett`. Every carrier and every USED ancilla word must be a
/// legal operand (rejects reserved s0/s1 = 8/9 and out-of-range >= cap, which
/// would otherwise panic `operand_get` at exec time rather than fail
/// verification); no carrier may repeat, and no USED ancilla word may repeat
/// (an aliased ancilla defeats the "ancilla returned to 0" teeth).
///
/// Used by `transcribe_bennett`. The BDD transcriber keeps its own copy inline
/// because it interleaves temp/node legality checks with this block (preserving
/// its distinct first-error ordering) and uses a shorter ancilla-dup message.
fn validate_bennett_operands(
    carrier_ops: &[u8],
    ancilla_ops: &[u8],
    n: usize,
) -> Result<(), SynthError> {
    for &c in &carrier_ops[..n] {
        check_operand_legal("carrier", c)?;
    }
    for &a in &ancilla_ops[..n] {
        check_operand_legal("ancilla", a)?;
    }
    for i in 0..n {
        for j in (i + 1)..n {
            if carrier_ops[i] == carrier_ops[j] {
                return Err(SynthError::MalformedSpec {
                    detail: format!("carrier operand {} appears twice", carrier_ops[i]),
                });
            }
            if ancilla_ops[i] == ancilla_ops[j] {
                return Err(SynthError::MalformedSpec {
                    detail: format!(
                        "ancilla operand {} appears twice (aliased ancilla)",
                        ancilla_ops[i]
                    ),
                });
            }
        }
    }
    Ok(())
}

/// Carrier-read validation: a carrier row (0..n) may only READ carriers. A
/// `Var(y)` with `y >= n` reads a register OUTSIDE the carrier set; a permutation
/// spec on `n` carriers must be a function of those `n` carriers alone -- reject.
fn validate_bennett_carrier_reads(
    fwd: &RegTransfer,
    bwd: &RegTransfer,
    n: usize,
) -> Result<(), SynthError> {
    for spec in [fwd, bwd] {
        for r in 0..n {
            let mut reads: Vec<usize> = Vec::new();
            for e in &spec.regs[r] {
                collect_var_operands(e, &mut reads);
            }
            if let Some(&y) = reads.iter().find(|&&y| y >= n) {
                return Err(SynthError::MalformedSpec {
                    detail: format!(
                        "carrier row {r} reads register {y} outside the {n} carriers; a \
                         permutation spec must read only carriers"
                    ),
                });
            }
        }
    }
    Ok(())
}

/// Non-carrier identity-row check: the permutation must be identity on registers
/// `n..NUM_REGS` (the permutation only touches the carriers).
fn validate_bennett_identity_rows(
    fwd: &RegTransfer,
    bwd: &RegTransfer,
    n: usize,
) -> Result<(), SynthError> {
    for r in n..NUM_REGS {
        if !is_identity_row(&fwd.regs[r], r) || !is_identity_row(&bwd.regs[r], r) {
            return Err(SynthError::MalformedSpec {
                detail: format!(
                    "spec touches reg {r} outside the {n} carriers; the permutation must be \
                     identity on non-carrier registers"
                ),
            });
        }
    }
    Ok(())
}

/// Build the NUM_REGS-length dst/src operand maps. For carrier rows `0..n`,
/// `src = carrier`, `dst = ancilla`. Rows `n..NUM_REGS` (which must be
/// spec-identity) get a harmless self-map so any identity emission is a self-noop.
fn build_bennett_operand_maps(
    carrier_ops: &[u8],
    ancilla_ops: &[u8],
    n: usize,
) -> (Vec<u8>, Vec<u8>) {
    let mut src_map = vec![0u8; NUM_REGS];
    let mut dst_map = vec![0u8; NUM_REGS];
    for r in 0..NUM_REGS {
        if r < n {
            src_map[r] = carrier_ops[r];
            dst_map[r] = ancilla_ops[r];
        } else {
            src_map[r] = carrier_ops.get(r).copied().unwrap_or(0);
            dst_map[r] = carrier_ops.get(r).copied().unwrap_or(0);
        }
    }
    (src_map, dst_map)
}

/// Emit the Bennett carrier<->ancilla swap loop (step 2 of every transcriber):
/// one exchange per carrier/ancilla pair, `Exch` for reg<->reg, `Xchg` when one
/// side is a scratch word.
fn emit_bennett_swaps(
    carrier_ops: &[u8],
    ancilla_ops: &[u8],
    n: usize,
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    for i in 0..n {
        out.push(swap_operands(carrier_ops[i], ancilla_ops[i])?);
    }
    Ok(())
}

/// Generic pairwise-distinctness guard over an operand list: every entry in
/// `operands` must be distinct. On the first collision, `on_dup` builds the
/// call-site-specific error detail (so each caller's exact message is preserved).
fn validate_operands_distinct(
    operands: &[u8],
    on_dup: impl Fn() -> String,
) -> Result<(), SynthError> {
    for i in 0..operands.len() {
        for j in (i + 1)..operands.len() {
            if operands[i] == operands[j] {
                return Err(SynthError::MalformedSpec { detail: on_dup() });
            }
        }
    }
    Ok(())
}

// ── A-6.1: the Bennett wrapper (general permutation via ancilla) ─────

/// Textbook Bennett reversible-in-place-permutation: realize a permutation
/// `P` (given as forward spec `fwd` and its inverse `bwd = P⁻¹`) on a set of
/// CARRIER operands, using a bank of ZERO-INIT ANCILLA operands that returns to
/// zero by construction.
///
/// Given carriers holding `x` and ancilla `O = 0`:
/// 1. `O ^= P(x)`   -- [`transcribe_into`]`(fwd, dst=ancilla, src=carrier)`.
///    Now `O = P(x)`, carriers still `x`.
/// 2. swap carriers ↔ O, per carrier↔ancilla pair (`Exch` for reg↔reg,
///    `Xchg` when the ancilla is a scratch word). Now carriers `= P(x) = y`,
///    `O = x`.
/// 3. `O ^= P⁻¹(y)` -- [`transcribe_into`]`(bwd, dst=ancilla, src=carrier)`.
///    Since carriers now hold `y` and `bwd = P⁻¹`, this XORs `P⁻¹(y) = x` onto
///    `O(=x)` → `O = 0`. Net-zero ancilla, guaranteed by construction.
///
/// Result: carriers hold `P(x)`, ancilla back to `0`. This is the ~2× body the
/// A-1 finding predicted -- CORRECT by construction; length is whatever it is.
///
/// `carrier_ops` / `ancilla_ops` are raw operand indices (reg `0..8`, scratch
/// `8..16`). `ancilla_ops` must be zero-init and have at least
/// `carrier_ops.len()` words (the extras are unused). Carriers and the used
/// ancilla words must be disjoint (Bennett requires fresh scratch).
pub fn transcribe_bennett(
    fwd: &RegTransfer,
    bwd: &RegTransfer,
    carrier_ops: &[u8],
    ancilla_ops: &[u8],
) -> Result<Vec<Instr>, SynthError> {
    let n = carrier_ops.len();
    if ancilla_ops.len() < n {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "ancilla bank has {} words, need ≥ carriers ({n})",
                ancilla_ops.len()
            ),
        });
    }
    // The transcriber operates over a full NUM_REGS frame: build the per-row
    // dst/src operand maps. Spec register `r` (0..NUM_REGS) reads from carrier
    // operand `carrier_ops[r]` and (for the compute/uncompute) writes to ancilla
    // operand `ancilla_ops[r]`. Rows beyond `n` carriers must be identity in the
    // spec (the permutation only touches the carriers); we still need operand
    // entries for them, so pad the maps with the carrier/ancilla we have and
    // require the caller's specs to be identity outside the carriers.
    if fwd.regs.len() != NUM_REGS || bwd.regs.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "fwd/bwd frames must be {NUM_REGS} rows (fwd={}, bwd={})",
                fwd.regs.len(),
                bwd.regs.len()
            ),
        });
    }
    // every carrier and every USED ancilla operand must be a legal operand, and
    // the operand maps must be internally disjoint with no dups: no carrier
    // repeated; no USED ancilla word repeated (aliased ancilla `[3,3]` defeats
    // "ancilla returned to 0" teeth -- CE).
    validate_bennett_operands(carrier_ops, ancilla_ops, n)?;
    // carriers and used ancilla must be disjoint (Bennett requires fresh scratch).
    for c in &carrier_ops[..n] {
        if ancilla_ops[..n].contains(c) {
            return Err(SynthError::MalformedSpec {
                detail: format!("carrier operand {c} aliases an ancilla word (must be disjoint)"),
            });
        }
    }

    // a carrier row (0..n) may only READ carriers. `Var(y)` with `y >= n` reads a
    // register OUTSIDE the carrier set; the src_map padding below wired every such
    // read to a bogus operand (operand 0), silently producing a wrong body + dirty
    // ancilla. A permutation spec on `n` carriers is a function of those `n`
    // carriers alone -- reject.
    validate_bennett_carrier_reads(fwd, bwd, n)?;

    // Build NUM_REGS-length dst/src maps. For carrier rows 0..n, src = carrier,
    // dst = ancilla. For rows n..NUM_REGS (which must be spec-identity), point
    // both at the carrier operand so any (identity) emission is a self-noop we
    // never actually emit -- but to be safe reject non-identity there.
    validate_bennett_identity_rows(fwd, bwd, n)?;
    let (src_map, dst_map) = build_bennett_operand_maps(carrier_ops, ancilla_ops, n);

    let mut out = Vec::new();

    // Step 1: O ^= P(x)   (dst = ancilla, src = carriers = x).
    out.extend(transcribe_into(fwd, &dst_map, &src_map)?);

    // Step 2: swap carriers ↔ ancilla, per pair. Use Exch for reg↔reg, Xchg
    // (reg↔scratch) when exactly one side is a scratch operand.
    emit_bennett_swaps(carrier_ops, ancilla_ops, n, &mut out)?;

    // Step 3: O ^= P⁻¹(y)  (dst = ancilla, src = carriers = y now). Clears O→0.
    out.extend(transcribe_into(bwd, &dst_map, &src_map)?);

    Ok(out)
}

/// Emit the single exchange instruction that swaps two operands `a` and `b`.
///
/// - reg↔reg (`a,b < 8`): `Exch { rd: a, rs: b }`.
/// - reg↔scratch (one side ≥ 8): `Xchg { rd: reg, sx: scratch-8 }`.
/// - scratch↔scratch: not supported directly (needs a temp register); Bennett
///   never needs it because the carriers are the working registers. Rejected.
fn swap_operands(a: u8, b: u8) -> Result<Instr, SynthError> {
    let a_reg = (a as usize) < NUM_REGS;
    let b_reg = (b as usize) < NUM_REGS;
    match (a_reg, b_reg) {
        (true, true) => Ok(Instr::Exch { rd: a, rs: b }),
        (true, false) => Ok(Instr::Xchg {
            rd: a,
            sx: b - NUM_REGS as u8,
        }),
        (false, true) => Ok(Instr::Xchg {
            rd: b,
            sx: a - NUM_REGS as u8,
        }),
        (false, false) => Err(SynthError::MalformedSpec {
            detail: format!(
                "carrier↔ancilla swap of two scratch operands ({a},{b}) is unsupported \
                 (at least one side must be a register)"
            ),
        }),
    }
}

// ── A-6.2: general ANF emission (arbitrary-degree monomials via scratch) ──
//
// FINDING (A-6.2): `transcribe_into` (A-6.1) realizes only the SINGLE-Toffoli
// basis -- at most one degree-2 AND term per output bit, byte-parallel. A general
// ANF (e.g. gcd2's) has per-bit XORs of *arbitrary-degree* monomials (gcd2 hits
// degree 3, several per bit, mixing bit positions: e.g. `a1 ∧ b0 ∧ h0`). Those
// are OUTSIDE that basis, so `transcribe_bennett` (which calls `transcribe_into`)
// rejects gcd2 with `RowNotInPlace`. `transcribe_into_general` /
// `transcribe_bennett_general` add the general path: they emit ANY ANF onto
// fresh zero destinations with a reversible, net-zero AND-tree over 2-input
// Ccnot gates plus a small bank of HELPER words (a bit-mask word + one temp per
// AND beyond the first). Everything is uncomputed, so every helper and every
// output ancilla returns to 0. This is DETERMINISTIC (no search); the extra
// helper words are the cost the design's (b)/widen-PC budget must cover.

/// Bank of zero-init HELPER operands the general emitter needs, disjoint from
/// carriers, output ancilla, sources, and destinations.
///
/// - `mask`: one word used to isolate a single output bit (holds `1<<b`
///   transiently; always restored to 0).
/// - `temps`: AND-tree accumulators. A degree-`d` monomial needs `d-1` temps
///   (the first Ccnot ANDs two factors, each further factor needs one more).
///   `temps.len()` must be ≥ (max monomial degree − 1).
///
/// All helper words are net-zero across each emitted term (compute → use →
/// uncompute), so they may be reused freely across rows and bits.
#[derive(Clone, Copy)]
pub struct HelperBank<'a> {
    pub mask: u8,
    pub temps: &'a [u8],
}

/// Transcribe a `RegTransfer` spec (already in register space; row `r` holds the
/// BvExpr for register `r`) onto FRESH, DISJOINT zero-init destinations, for the
/// **general ANF class** -- XOR of arbitrary-degree AND-monomials, per bit.
///
/// For each spec row `i` and output bit `b`, the expression is flattened to its
/// ANF terms; each term is emitted into bit `b` of `dst_ops[i]` (which starts 0)
/// by [`emit_anf_term`], reading spec input `y` from `src_ops[y]` and using the
/// zero-init [`HelperBank`] for the AND-tree + bit isolation. Everything is
/// uncomputed → destinations hold `spec(src)`, helpers back to 0.
pub fn transcribe_into_general(
    spec: &RegTransfer,
    dst_ops: &[u8],
    src_ops: &[u8],
    helpers: HelperBank,
) -> Result<Vec<Instr>, SynthError> {
    if spec.regs.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: format!("frame has {} rows, expected {NUM_REGS}", spec.regs.len()),
        });
    }
    if dst_ops.len() != NUM_REGS || src_ops.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: "dst_ops/src_ops must each have NUM_REGS entries".to_string(),
        });
    }
    let mut out = Vec::new();
    for (i, row) in spec.regs.iter().enumerate() {
        if row.len() != BITS {
            return Err(SynthError::MalformedSpec {
                detail: format!("reg {i} row has {} bits, expected {BITS}", row.len()),
            });
        }
        let dst = dst_ops[i];
        // A row that is the identity AND self-maps (dst operand == src operand
        // for this row's own register) contributes nothing: the value is already
        // in place. Emitting `Ccnot dst, dst, mask` self-copies would corrupt a
        // live word (matches `transcribe_into`'s dst==src skip). This covers the
        // Bennett wrapper's non-carrier rows (dst_map==src_map==carrier).
        if is_identity_row(row, i) && dst == src_ops[i] {
            continue;
        }
        for (b, e) in row.iter().enumerate() {
            for t in flatten_xor(e) {
                emit_anf_term(t, dst, b, src_ops, &helpers, &mut out)?;
            }
        }
    }
    Ok(out)
}

/// Emit gates that XOR one ANF term's contribution into bit `b` of `dst`
/// (`dst` starts 0 at bit `b` for this term's turn; multiple terms XOR-accumulate).
///
/// A term is `Const(true)`, a single `Var(y,c)`, or an AND-monomial
/// `Var(y0,c0) ∧ ... ∧ Var(y_{d-1},c_{d-1})` of any degree `d`; each `Var` reads
/// its OWN bit `c_k` (ANF monomials mix bit positions). Realization over the
/// 2-input-Ccnot ISA, net-zero:
///   - `Const(true)`  → `Xori { dst, 1<<b }`.
///   - degree 1 → align src bit `c0` to bit `b` (rotate, paired-undone),
///     load `mask = 1<<b`, `Ccnot dst, src_aligned, mask`
///     (dst bit b ^= src bit c0), unload mask, un-rotate.
///   - degree d ≥ 2 → align all factors to bit `b`; AND-tree:
///     `Ccnot temp0, f0, f1` (temp0 = f0∧f1),
///     `Ccnot temp1, temp0, f2` (temp1 = f0∧f1∧f2), ...;
///     then `mask = 1<<b`, `Ccnot dst, temp_last, mask`
///     (dst bit b ^= product bit b only), unload mask, and run
///     the AND-tree in REVERSE to clear every temp, then
///     un-rotate the sources. Net-zero by construction.
///
/// The `mask` isolates bit `b` because the byte-parallel Ccnot AND-tree leaves
/// garbage in temp's OTHER bits; ANDing with `1<<b` selects only bit `b` when
/// XOR-ing onto `dst`.
fn emit_anf_term(
    term: &BvExpr,
    dst: u8,
    b: usize,
    src_ops: &[u8],
    helpers: &HelperBank,
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    let factors: Vec<(usize, usize)> = match term {
        BvExpr::Const(true) => {
            out.push(Instr::Xori {
                rd: dst,
                imm: 1u8 << b,
            });
            return Ok(());
        }
        BvExpr::Const(false) => return Ok(()), // XOR identity -- no-op
        BvExpr::Var { operand, bit } => vec![(*operand, *bit)],
        BvExpr::And(_, _) => flatten_and(term)?,
        other => {
            return Err(SynthError::RowNotInPlace {
                reg: dst as usize,
                detail: format!("unsupported ANF term shape: {other:?}"),
            });
        }
    };
    let degree = factors.len();

    if degree == 1 {
        // dst bit b ^= (src bit c0): align src bit c0 → bit b, mask-select onto
        // dst, restore. Net-zero on src (paired rotate) and mask.
        let (y, c) = factors[0];
        let s = src_ops[y];
        // Self-copy `dst ^= dst&mask` on a fresh zero dst is a no-op AND would
        // read/write the same word; skip it (fresh targets start 0).
        if s == dst && c == b {
            return Ok(());
        }
        let sh = b as i32 - c as i32;
        rotate_by(s, sh, out);
        out.push(Instr::Xori {
            rd: helpers.mask,
            imm: 1u8 << b,
        });
        out.push(Instr::Ccnot {
            rd: dst,
            rs: s,
            rt: helpers.mask,
        });
        out.push(Instr::Xori {
            rd: helpers.mask,
            imm: 1u8 << b,
        });
        rotate_by(s, -sh, out);
        return Ok(());
    }

    // degree ≥ 2: build the product with an AND-LADDER of `degree-1` clean
    // single-bit accumulators, uncomputed for net-zero. Every factor is aligned
    // to bit `b` JUST FOR its Ccnot and un-aligned right after, so two factors
    // that share a source operand (multiplicity > 1) are never rotated
    // simultaneously -- the emitter is aliasing-safe without extra copy words.
    if helpers.temps.len() < degree - 1 {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "ANF monomial degree {degree} needs {} accumulator temp(s), have {}",
                degree - 1,
                helpers.temps.len()
            ),
        });
    }

    // Helper: emit `Ccnot{acc, x, aligned(factor k)}` bracketed by the align /
    // un-align rotations of factor k's source, appended to `out`.
    let emit_and = |out: &mut Vec<Instr>, acc: u8, x: u8, k: usize| {
        let (y, c) = factors[k];
        let s = src_ops[y];
        let sh = b as i32 - c as i32;
        rotate_by(s, sh, out);
        out.push(Instr::Ccnot {
            rd: acc,
            rs: x,
            rt: s,
        });
        rotate_by(s, -sh, out);
    };

    // FORWARD LADDER (compute):
    //   temp0 := 1<<b ∧ f0    (clean single bit b = f0's bit)  -- mask seeds a
    //   temp_{k-1} := temp_{k-2} ∧ f_k                          clean accumulator
    // Because temp0 is clean (0 outside bit b), every downstream AND stays clean,
    // so the final XOR onto dst needs no mask.
    out.push(Instr::Xori {
        rd: helpers.mask,
        imm: 1u8 << b,
    }); // mask = 1<<b
    emit_and(out, helpers.temps[0], helpers.mask, 0); // temp0 = mask ∧ f0 (clean)
    for k in 1..degree - 1 {
        // temp[k] = temp[k-1] ∧ f_k  (factors 1..degree-1; the last goes to dst)
        emit_and(out, helpers.temps[k], helpers.temps[k - 1], k);
    }
    // Last factor ANDs the running product directly onto dst (clean → bit b only).
    let last_acc = helpers.temps[degree - 2];
    emit_and(out, dst, last_acc, degree - 1);

    // REVERSE LADDER (uncompute): mirror the forward steps to restore every temp
    // and the mask to 0. (dst keeps the XOR-ed product; only helpers are cleared.)
    for k in (1..degree - 1).rev() {
        emit_and(out, helpers.temps[k], helpers.temps[k - 1], k);
    }
    emit_and(out, helpers.temps[0], helpers.mask, 0); // clears temp0
    out.push(Instr::Xori {
        rd: helpers.mask,
        imm: 1u8 << b,
    }); // mask back to 0
    Ok(())
}

/// Collect every register operand index READ by a `BvExpr` (walks all node
/// kinds; used by the Bennett carrier-read validation).
fn collect_var_operands(e: &BvExpr, out: &mut Vec<usize>) {
    match e {
        BvExpr::Var { operand, .. } => out.push(*operand),
        BvExpr::Const(_) => {}
        BvExpr::Not(a) => collect_var_operands(a, out),
        BvExpr::Xor(a, b) | BvExpr::And(a, b) | BvExpr::Or(a, b) => {
            collect_var_operands(a, out);
            collect_var_operands(b, out);
        }
        BvExpr::Mux { sel, then_, else_ } => {
            collect_var_operands(sel, out);
            collect_var_operands(then_, out);
            collect_var_operands(else_, out);
        }
    }
}

/// Flatten a (possibly nested) `And` of `Var`s into its factor list.
fn flatten_and(e: &BvExpr) -> Result<Vec<(usize, usize)>, SynthError> {
    let mut out = Vec::new();
    fn go(e: &BvExpr, out: &mut Vec<(usize, usize)>) -> Result<(), SynthError> {
        match e {
            BvExpr::And(a, b) => {
                go(a, out)?;
                go(b, out)
            }
            BvExpr::Var { operand, bit } => {
                out.push((*operand, *bit));
                Ok(())
            }
            other => Err(SynthError::RowNotInPlace {
                reg: 0,
                detail: format!("AND monomial has a non-Var factor: {other:?}"),
            }),
        }
    }
    go(e, &mut out)?;
    Ok(out)
}

/// Emit a rotate-left by `sh` bits (negative ⇒ rotate right), or nothing if 0.
fn rotate_by(op: u8, sh: i32, out: &mut Vec<Instr>) {
    let sh = sh.rem_euclid(BITS as i32) as u8;
    if sh != 0 {
        out.push(Instr::Rotli { rd: op, imm: sh });
    }
}

/// General Bennett wrapper (A-6.2): like [`transcribe_bennett`] but the
/// compute/uncompute steps use [`transcribe_into_general`], so the permutation's
/// per-bit ANF may contain arbitrary-degree monomials (gcd2). Same net-zero
/// guarantee: carriers end at `P(x)`, output ancilla back to 0, and the general
/// emitter's helper bank back to 0.
///
/// `carrier_ops` (leaf order) hold `x`; `out_ancilla` (≥ n, zero-init, disjoint)
/// receive `P(x)`; `helpers` is the general emitter's zero-init helper bank
/// (also disjoint). Because the AND-tree reads carriers as sources, in step 3
/// the carriers already hold `y = P(x)`, and `bwd = P⁻¹` XORs `P⁻¹(y) = x` onto
/// the ancilla (= x) → 0.
pub fn transcribe_bennett_general(
    fwd: &RegTransfer,
    bwd: &RegTransfer,
    carrier_ops: &[u8],
    out_ancilla: &[u8],
    helpers: HelperBank,
) -> Result<Vec<Instr>, SynthError> {
    let n = carrier_ops.len();
    if out_ancilla.len() < n {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "output ancilla bank has {} words, need ≥ carriers ({n})",
                out_ancilla.len()
            ),
        });
    }
    if fwd.regs.len() != NUM_REGS || bwd.regs.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: "fwd/bwd frames must be NUM_REGS rows".to_string(),
        });
    }
    for c in &carrier_ops[..n] {
        if out_ancilla[..n].contains(c) {
            return Err(SynthError::MalformedSpec {
                detail: format!("carrier operand {c} aliases an output-ancilla word"),
            });
        }
    }
    for r in n..NUM_REGS {
        if !is_identity_row(&fwd.regs[r], r) || !is_identity_row(&bwd.regs[r], r) {
            return Err(SynthError::MalformedSpec {
                detail: format!(
                    "spec touches reg {r} outside the {n} carriers; permutation must be \
                     identity on non-carrier registers"
                ),
            });
        }
    }
    // Build the NUM_REGS-length src/dst maps (spec leaf r → carrier/ancilla op).
    let (src_map, dst_map) = build_bennett_operand_maps(carrier_ops, out_ancilla, n);

    let mut out = Vec::new();
    // 1. O ^= P(x).
    out.extend(transcribe_into_general(fwd, &dst_map, &src_map, helpers)?);
    // 2. swap carriers ↔ output ancilla.
    emit_bennett_swaps(carrier_ops, out_ancilla, n, &mut out)?;
    // 3. O ^= P⁻¹(y) → clears O to 0.
    out.extend(transcribe_into_general(bwd, &dst_map, &src_map, helpers)?);
    Ok(out)
}

// ── A-6.2: general table → RegTransfer via per-output-bit ANF ─────────

/// Deterministically build a full-frame `RegTransfer` from a byte-level
/// bijection, by computing the **algebraic normal form (Reed-Muller / Möbius
/// transform)** of every output bit as a boolean function of the input bits.
///
/// This is the reusable, prim-agnostic front-half of the synthesizer: it turns
/// *any* concrete permutation on a small carrier (given as a closure over the
/// carrier bytes) into the symbolic `RegTransfer` the transcriber consumes. It
/// is EXACT and DETERMINISTIC -- no search, no enumeration of instruction
/// sequences; it enumerates only the carrier's input-bit assignments (2^Σwidths)
/// and Möbius-transforms each output bit's truth table.
///
/// # Carrier model
/// The carrier is `carrier_regs.len()` leaves. Leaf `i` lives in operand
/// (register) `carrier_regs[i]` and has `in_bit_widths[i]` MEANINGFUL bits
/// (bits `0..in_bit_widths[i]`); bits at/above the width are assumed to always
/// be zero on input and are produced as `Const(false)` on output (matching the
/// Flat(w) zero-extension convention). The `map` closure receives a byte vector
/// of length `carrier_regs.len()` (one byte per leaf, in carrier order) holding
/// the meaningful bits, and returns the post-transfer byte vector (same layout).
///
/// # ANF construction (per output bit)
/// For output bit `(reg=carrier_regs[i], bit b < width_i)`:
///   1. Enumerate all `2^N` assignments of the `N = Σ in_bit_widths` input bits.
///      For each assignment, materialise the carrier bytes, call `map`, and read
///      truth-table entry `f[x] = bit b of output leaf i`.
///   2. Möbius transform `f` in place → ANF coefficients `c[S]` (over subsets `S`
///      of the input bits). `c[S] = XOR_{T ⊆ S} f[T]`.
///   3. Emit `XOR over {S : c[S]=1}` of the AND-monomial `∧_{k ∈ S} Var(reg_k,
///      bit_k)` of the input variables in `S`. The empty monomial (`S=∅`)
///      contributes `Const(true)`.
///
/// Rows and bits outside the carrier, or beyond a leaf's width, are the identity
/// `Var(r,b)` (and `Const(false)` for the above-width bits of carrier leaves).
///
/// The result is ANF -- length is whatever the ANF is (not minimised); A-6.4 /
/// widen-PC funds the resulting body size. Callers verify with the symbolic
/// oracle (all-register operands) and/or the exec/net-zero path.
// Loop indices here double as bit-shift amounts and packed-assignment keys (x, bit,
// k address truth/out_bytes/var_pairs in lockstep with `>> k`/`>> bit` arithmetic),
// so the explicit range loops keep the truth-table construction unambiguous.
#[allow(clippy::needless_range_loop)]
pub fn table_to_regtransfer(
    map: impl Fn(&[u8]) -> Vec<u8>,
    carrier_regs: &[u8],
    in_bit_widths: &[u8],
) -> RegTransfer {
    assert_eq!(
        carrier_regs.len(),
        in_bit_widths.len(),
        "table_to_regtransfer: carrier_regs and in_bit_widths must align"
    );
    let n_leaves = carrier_regs.len();

    // Flattened list of meaningful input bits, one entry per bit in carrier
    // order: `input_vars[k] = (leaf index, register, bit position)` is the k-th
    // input variable (bit `k` of the packed assignment `x`).
    let mut input_vars: Vec<(usize, usize, usize)> = Vec::new();
    for (leaf, (&reg, &w)) in carrier_regs.iter().zip(in_bit_widths.iter()).enumerate() {
        assert!(
            (w as usize) <= BITS,
            "table_to_regtransfer: leaf {leaf} width {w} exceeds BITS={BITS}"
        );
        for bit in 0..w as usize {
            input_vars.push((leaf, reg as usize, bit));
        }
    }
    // The (reg, bit) pair of each input variable, used to build ANF monomials.
    let var_pairs: Vec<(usize, usize)> = input_vars.iter().map(|&(_, r, b)| (r, b)).collect();
    let n_in = input_vars.len();
    assert!(
        n_in <= 24,
        "table_to_regtransfer: {n_in} input bits → 2^{n_in} rows is too large to enumerate; \
         the ANF construction is exponential in the total carrier width"
    );
    let n_rows = 1usize << n_in;

    // ── Build the truth table for every output (leaf, bit) in one sweep. ──
    // truth[leaf][bit][x] = bit `bit` of output leaf `leaf` for input row `x`,
    // where `x` is the packed assignment of the `n_in` input bits (bit k of `x`
    // = input_vars[k]).
    let mut truth: Vec<Vec<Vec<bool>>> = (0..n_leaves)
        .map(|leaf| {
            let w = in_bit_widths[leaf] as usize;
            vec![vec![false; n_rows]; w]
        })
        .collect();

    for x in 0..n_rows {
        // Materialise the carrier bytes for assignment `x`.
        let mut in_bytes = vec![0u8; n_leaves];
        for (k, &(leaf, _reg, bit)) in input_vars.iter().enumerate() {
            if (x >> k) & 1 == 1 {
                in_bytes[leaf] |= 1 << bit;
            }
        }
        let out_bytes = map(&in_bytes);
        assert_eq!(
            out_bytes.len(),
            n_leaves,
            "table_to_regtransfer: map returned {} bytes, expected {n_leaves}",
            out_bytes.len()
        );
        for leaf in 0..n_leaves {
            let w = in_bit_widths[leaf] as usize;
            for bit in 0..w {
                truth[leaf][bit][x] = (out_bytes[leaf] >> bit) & 1 == 1;
            }
        }
    }

    // ── Möbius transform each output-bit truth table → ANF, build BvExpr. ──
    let mut transfer = RegTransfer::identity();
    for (leaf, &reg) in carrier_regs.iter().enumerate() {
        let reg = reg as usize;
        let w = in_bit_widths[leaf] as usize;
        for bit in 0..w {
            let coeffs = mobius_transform(&truth[leaf][bit]);
            transfer.regs[reg][bit] = anf_to_bvexpr(&coeffs, &var_pairs, n_in);
        }
        // Above-width bits of a carrier leaf are Flat(w) zero-extended.
        for bit in w..BITS {
            transfer.regs[reg][bit] = BvExpr::Const(false);
        }
    }
    transfer
}

/// In-place Möbius (Reed-Muller) transform of a truth table over `n` bits.
///
/// Input `f[x]` is the truth table (`x` packs the `n` input bits). Output is the
/// ANF coefficient vector `c[S]`, where `c[S] = XOR_{T ⊆ S} f[T]`. Classic
/// butterfly: for each bit `k`, XOR the low half into the high half. O(n·2^n).
fn mobius_transform(f: &[bool]) -> Vec<bool> {
    let len = f.len();
    debug_assert!(len.is_power_of_two());
    let mut c = f.to_vec();
    let mut step = 1usize;
    while step < len {
        let mut i = 0usize;
        while i < len {
            for j in i..i + step {
                // The "high" half (index with this bit set) XORs in the "low".
                let hi = j + step;
                c[hi] ^= c[j];
            }
            i += step << 1;
        }
        step <<= 1;
    }
    c
}

/// Build the `BvExpr` for one output bit from its ANF coefficients.
///
/// `coeffs[S]` (indexed by the subset bitmask `S` over the `n_in` input bits) is
/// 1 iff monomial `∧_{k ∈ S} var_pairs[k]` is present. The result is the XOR of
/// all present monomials; the empty monomial (`S = 0`) is `Const(true)`. If no
/// coefficient is set the function is the constant `Const(false)`.
// Index k is the subset-mask bit position: it is both the shift `s >> k` and the
// var_pairs index, so the range loop mirrors the ANF monomial construction.
#[allow(clippy::needless_range_loop)]
fn anf_to_bvexpr(coeffs: &[bool], var_pairs: &[(usize, usize)], n_in: usize) -> BvExpr {
    let mut terms: Vec<BvExpr> = Vec::new();
    for (s, &present) in coeffs.iter().enumerate() {
        if !present {
            continue;
        }
        // Build the AND-monomial for subset `s`.
        let mut monomial: Option<BvExpr> = None;
        for k in 0..n_in {
            if (s >> k) & 1 == 1 {
                let (reg, bit) = var_pairs[k];
                let v = BvExpr::var(reg, bit);
                monomial = Some(match monomial {
                    None => v,
                    Some(m) => m.and(v),
                });
            }
        }
        terms.push(monomial.unwrap_or(BvExpr::Const(true)));
    }
    // XOR all present monomials; none present ⇒ constant 0.
    let mut it = terms.into_iter();
    match it.next() {
        None => BvExpr::Const(false),
        Some(first) => it.fold(first, |acc, t| acc.xor(t)),
    }
}

/// A flat XOR term category (at a fixed bit position).
enum Term {
    Var(usize),
    Const,
    And(usize, usize),
}

/// Classify one flat-XOR term at bit `b`. Returns `None` for anything not in
/// the supported basis.
fn classify_term(t: &BvExpr, b: usize) -> Option<Term> {
    match t {
        BvExpr::Var { operand, bit } if *bit == b => Some(Term::Var(*operand)),
        BvExpr::Const(true) => Some(Term::Const),
        BvExpr::Const(false) => None, // a false const term should not appear post-flatten; reject
        BvExpr::And(a, c) => {
            // Require Var(y,b) ∧ Var(z,b) -- matching bit, two distinct-or-not regs.
            if let (
                BvExpr::Var {
                    operand: y,
                    bit: yb,
                },
                BvExpr::Var {
                    operand: z,
                    bit: zb,
                },
            ) = (a.as_ref(), c.as_ref())
            {
                if *yb == b && *zb == b {
                    return Some(Term::And(*y, *z));
                }
            }
            None
        }
        _ => None,
    }
}

impl Row {
    /// Build the combined `x ^= (affine/const) ⊕ (y∧z)` in-place row.
    ///
    /// Emitted as the affine/const gates followed by the Ccnot; both
    /// XOR-accumulate onto `target` so their relative order is immaterial. The
    /// read footprint is the union (affine sources + y + z).
    fn from_affine_plus_ccnot(
        target: usize,
        xor_sources: Vec<usize>,
        const_mask: u8,
        y: usize,
        z: usize,
    ) -> Row {
        Row::AffineCcnot {
            target,
            xor_sources,
            const_mask,
            rs: y,
            rt: z,
        }
    }
}

/// Peel a single `Var(r, b)` self-term off the top of an XOR tree, returning the
/// remainder. If `e == Var(r,b)` exactly, the remainder is `Const(false)`.
/// Returns `None` if there is no self-term at the top level (the row is not
/// in-place: its new value is not `old ⊕ ...`).
fn peel_self_xor(e: &BvExpr, r: usize, b: usize) -> Option<BvExpr> {
    let terms = flatten_xor(e);
    // Count self-terms; there must be exactly one (odd count would leave one,
    // but a clean spec has exactly one).
    let mut self_count = 0usize;
    let mut rest: Vec<BvExpr> = Vec::new();
    for t in terms {
        if matches!(t, BvExpr::Var { operand, bit } if *operand == r && *bit == b) {
            self_count += 1;
        } else {
            rest.push((*t).clone());
        }
    }
    if self_count.is_multiple_of(2) {
        // No net self-term ⇒ not in-place (value doesn't XOR onto its own old).
        return None;
    }
    Some(rebuild_xor(rest))
}

/// Flatten an XOR tree into its flat list of non-XOR leaf terms.
fn flatten_xor(e: &BvExpr) -> Vec<&BvExpr> {
    let mut out = Vec::new();
    flatten_xor_into(e, &mut out);
    out
}

fn flatten_xor_into<'a>(e: &'a BvExpr, out: &mut Vec<&'a BvExpr>) {
    match e {
        BvExpr::Xor(a, b) => {
            flatten_xor_into(a, out);
            flatten_xor_into(b, out);
        }
        // Drop XOR identity elements (Const(false)) at flatten time.
        BvExpr::Const(false) => {}
        other => out.push(other),
    }
}

/// Rebuild an XOR tree from a list of terms. Empty ⇒ `Const(false)`.
fn rebuild_xor(terms: Vec<BvExpr>) -> BvExpr {
    let mut it = terms.into_iter();
    match it.next() {
        None => BvExpr::Const(false),
        Some(first) => it.fold(first, |acc, t| acc.xor(t)),
    }
}

/// Recognize the bit-0-controlled swap shape starting at register `r`.
///
/// `Cswap { rd: c, rs: u, rt: v }` produces, for every bit `b`:
///   reg[u][b] = Mux(Var(c,0), Var(v,b), Var(u,b))
///   reg[v][b] = Mux(Var(c,0), Var(u,b), Var(v,b))
/// and leaves reg[c] identity.
///
/// We look for a register `r` whose row is the `u`-side mux against some partner
/// `v`, then confirm `v`'s row is the matching `v`-side mux and `c`'s row is
/// identity. Returns `(c, u, v)` on success.
fn recognize_cswap(spec: &RegTransfer, r: usize) -> Option<(usize, usize, usize)> {
    // Parse r's bit 0 as Mux(Var(c,0), Var(a,0), Var(b,0)) and infer the roles.
    let (ctrl, then_reg, else_reg) = parse_mux_bit(&spec.regs[r][0])?;
    // For the u-side row, else_ is the register's own value (Var(u,0)) and then_
    // is the partner (Var(v,0)). So u = else_reg = r, v = then_reg.
    if else_reg != r {
        return None;
    }
    let u = r;
    let v = then_reg;
    let c = ctrl;
    if c == u || c == v || u == v {
        return None; // Cswap needs pairwise-distinct operands
    }
    // Confirm ALL bits of u: Mux(Var(c,0), Var(v,b), Var(u,b)).
    if !matches_swap_side(&spec.regs[u], c, v, u) {
        return None;
    }
    // Confirm ALL bits of v: Mux(Var(c,0), Var(u,b), Var(v,b)).
    if !matches_swap_side(&spec.regs[v], c, u, v) {
        return None;
    }
    // Confirm c is identity (Cswap leaves the control register unchanged).
    if !is_identity_row(&spec.regs[c], c) {
        return None;
    }
    Some((c, u, v))
}

/// Parse `Mux(Var(c,0), Var(then,0), Var(else,0))` → `(c, then, else)` at bit 0.
fn parse_mux_bit(e: &BvExpr) -> Option<(usize, usize, usize)> {
    if let BvExpr::Mux { sel, then_, else_ } = e {
        if let (
            BvExpr::Var {
                operand: c,
                bit: cb,
            },
            BvExpr::Var {
                operand: t,
                bit: _tb,
            },
            BvExpr::Var {
                operand: el,
                bit: _eb,
            },
        ) = (sel.as_ref(), then_.as_ref(), else_.as_ref())
        {
            if *cb == 0 {
                return Some((*c, *t, *el));
            }
        }
    }
    None
}

/// True iff `row[b] == Mux(Var(c,0), Var(then,b), Var(self_reg,b))` for all b.
fn matches_swap_side(row: &[BvExpr], c: usize, then_reg: usize, self_reg: usize) -> bool {
    row.iter().enumerate().all(|(b, e)| {
        matches!(
            e,
            BvExpr::Mux { sel, then_, else_ }
            if matches!(sel.as_ref(), BvExpr::Var { operand, bit } if *operand == c && *bit == 0)
                && matches!(then_.as_ref(), BvExpr::Var { operand, bit } if *operand == then_reg && *bit == b)
                && matches!(else_.as_ref(), BvExpr::Var { operand, bit } if *operand == self_reg && *bit == b)
        )
    })
}

/// Schedule the recognized rows so each row's external reads still hold input
/// values when it runs.
///
/// For the ancilla-free in-place class the only hazard is a register that one
/// row *reads* while another row *overwrites* it -- a read-after-write where the
/// read must precede the write, but the write's own row might depend on the
/// reader too (a cycle ⇒ Bennett). We do a Kahn-style topological sort with the
/// edge "reader-before-writer": if row A reads register g and a *different* row
/// B writes g, then A must run before B. A cycle ⇒ `OrderingConflict`.
fn schedule(rows: &[Row]) -> Result<Vec<Row>, SynthError> {
    // writer_of[g] = index of the (unique) row that writes register g, if any.
    // A register written by two different rows is out of the simple in-place
    // class (only Cswap writes two regs, and it registers both indices to the
    // SAME row instance -- but our `rows` vec has one entry per register, so a
    // Cswap appears twice; treat those two entries as the same logical unit).
    //
    // To keep it simple and correct: build a list of logical *units*. Cswap
    // occupies two register slots with identical content; dedup by identity of
    // (writes-set).
    let mut units: Vec<(Vec<usize>, Row)> = Vec::new();
    let mut seen_cswap: Vec<(usize, usize)> = Vec::new();
    for row in rows.iter() {
        match row {
            Row::Cswap { u, v, .. } => {
                let key = (*u.min(v), *u.max(v));
                if seen_cswap.contains(&key) {
                    continue;
                }
                seen_cswap.push(key);
                units.push((row.writes(), row.clone()));
            }
            Row::Identity => { /* no unit, no gate */ }
            _ => units.push((row.writes(), row.clone())),
        }
    }

    let m = units.len();
    // writer_of: register → unit index that writes it.
    let mut writer_of: std::collections::HashMap<usize, usize> = std::collections::HashMap::new();
    for (i, (writes, _)) in units.iter().enumerate() {
        for &w in writes {
            if let Some(_prev) = writer_of.insert(w, i) {
                return Err(SynthError::OrderingConflict {
                    detail: format!("register {w} written by two rows"),
                });
            }
        }
    }

    // Edges: for each unit i, for each register g it reads (external), if some
    // unit j != i writes g, add edge i → j (i must precede j).
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); m];
    let mut indeg: Vec<usize> = vec![0; m];
    for (i, (_, row)) in units.iter().enumerate() {
        for g in row.reads_others() {
            if let Some(&j) = writer_of.get(&g) {
                if j != i && !adj[i].contains(&j) {
                    adj[i].push(j);
                    indeg[j] += 1;
                }
            }
        }
    }

    // Kahn topological sort.
    let mut queue: std::collections::VecDeque<usize> = (0..m).filter(|&i| indeg[i] == 0).collect();
    let mut order: Vec<usize> = Vec::with_capacity(m);
    while let Some(i) = queue.pop_front() {
        order.push(i);
        for &j in &adj[i] {
            indeg[j] -= 1;
            if indeg[j] == 0 {
                queue.push_back(j);
            }
        }
    }
    if order.len() != m {
        return Err(SynthError::OrderingConflict {
            detail: "read-after-write cycle among modified registers (needs ancilla)".to_string(),
        });
    }

    Ok(order.into_iter().map(|i| units[i].1.clone()).collect())
}

// ── A-6.D (D.0 + D.1): BDD-based reversible synthesis ─────────────────
//
// The general, STRUCTURE-PRESERVING prim→gates mechanism. Where the A-6.2 ANF
// path (`transcribe_into_general`) re-expands each output bit to its Reed-Muller
// sum-of-products (degree blowup: gcd2 → 15,148 instrs), this walks the CANONICAL
// ROBDD of each output bit and emits O(#nodes) gates. Same generality (any
// BvExpr → BDD via `expr_to_bdd`, the representation `equiv`/`admit_symbolic`
// already trust), but the node count -- not the ANF degree -- sets the length.
//
// DETERMINISTIC: build the ROBDD, walk its nodes children-before-parents, emit a
// fixed gate gadget per node, uncompute for net-zero. No search, no enumeration.
//
// # ROBDD access (D.0)
//
// biodivine's `Bdd` is a `Vec<BddNode>` in which indices 0 and 1 are the FALSE
// and TRUE terminals and every non-terminal is appended AFTER its children, so
// plain ascending pointer order (`bdd.pointers()`, skipping the two terminals) is
// already a valid children-before-parents topological order. Per node we read
// `bdd.var_of(p)` (the decision variable), `bdd.low_link_of(p)` (the `else`/x=0
// child) and `bdd.high_link_of(p)` (the `then`/x=1 child); `bdd.root_pointer()`
// is the output. `BddVariable::to_index()` inverts the shared `VarMap` mapping
// (`idx = bit*num_operands + operand`), recovering the input `(operand, bit)`.
//
// We build ONE BDD per (carrier output operand, output bit) -- per-bit BDDs, which
// the design explicitly permits for this slice; node lines are allocated and
// uncomputed per output bit, so the ancilla-word budget is the MAX single-bit
// node count, not the sum.
//
// # Shannon-node → gates (D.1)
//
// Each node value lives at BIT 0 of a dedicated ancilla WORD, kept clean (bits
// 1..7 always 0). A node `n = ite(x, hi, lo) = lo ⊕ (x ∧ (hi ⊕ lo))` is built:
//   1. `n ^= lo`                (copy the clean low child; XOR of a clean word)
//   2. `T ^= hi ; T ^= lo`      (T = hi⊕lo in a clean reused temp word, bit 0)
//   3. `Ccnot(n, x_aligned, T)` (n bit0 ^= x_bit ∧ (hi⊕lo); T clean ⇒ n stays clean)
//   4. `T ^= lo ; T ^= hi`      (restore T = 0)
// A child that is the FALSE terminal contributes nothing (clean 0); the TRUE
// terminal contributes `Xori(word, 1)`. The output carrier bit reads the root
// node's line (or the root terminal directly). Every node's 4-step gadget is its
// own inverse (all XOR-accumulate), so the reverse pass replays it to return each
// node word to 0 -- Bennett net-zero.

/// Zero-init helper bank for [`transcribe_bdd_into`]'s per-bit BDD synthesis.
///
/// - `temp`: one clean scratch word holding `hi ⊕ lo` at bit 0 during a node's
///   Ccnot; always restored to 0 within each node's gadget.
/// - `nodes`: one word per BDD node LINE (value at bit 0). Must have at least the
///   max single-output-bit ROBDD node count; node words are recycled across
///   output bits (each bit fully uncomputes its lines before the next). Every
///   word is disjoint from carriers, destinations, sources, and `temp`.
///
/// All helper words are net-zero across each output bit (compute → read →
/// uncompute), so they are reused freely bit-to-bit and row-to-row.
#[derive(Clone, Copy)]
pub struct BddHelpers<'a> {
    pub temp: u8,
    pub nodes: &'a [u8],
}

/// A clean bit-0 value source feeding a node gadget.
#[derive(Clone, Copy)]
enum Line {
    /// Constant 0 (FALSE terminal) -- contributes nothing.
    Zero,
    /// Constant 1 (TRUE terminal) -- one `Xori(word, 1)`.
    One,
    /// Bit 0 of node ancilla word `w` (clean: upper bits are 0).
    Node(u8),
}

/// Transcribe a register-space `RegTransfer` spec onto FRESH, DISJOINT zero-init
/// destinations via BDD synthesis (D.1). For each spec row `i` and output bit
/// `b`, builds the ROBDD of `spec.regs[i][b]` over the input variables, walks it,
/// and emits O(#nodes) gates that XOR `spec.regs[i][b]` into bit `b` of
/// `dst_ops[i]` (which starts 0). Every node line is uncomputed → destinations
/// hold `spec(src)`, helpers back to 0.
///
/// Mirrors [`transcribe_into_general`]'s operand contract: `dst_ops`/`src_ops`
/// are NUM_REGS-length raw operand maps; spec input `Var(y, c)` reads bit `c` of
/// `src_ops[y]`. An identity row that self-maps (`dst == src`) is skipped.
pub fn transcribe_bdd_into(
    spec: &RegTransfer,
    dst_ops: &[u8],
    src_ops: &[u8],
    helpers: BddHelpers,
) -> Result<Vec<Instr>, SynthError> {
    if spec.regs.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: format!("frame has {} rows, expected {NUM_REGS}", spec.regs.len()),
        });
    }
    if dst_ops.len() != NUM_REGS || src_ops.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: "dst_ops/src_ops must each have NUM_REGS entries".to_string(),
        });
    }
    // Shared variable mapping over the register space (operand = register index):
    // idx = bit * NUM_REGS + operand, inverted below to recover (operand, bit).
    let map = VarMap::new(NUM_REGS).map_err(|e| SynthError::MalformedSpec {
        detail: format!("VarMap::new failed: {e}"),
    })?;

    let mut out = Vec::new();
    for (i, row) in spec.regs.iter().enumerate() {
        if row.len() != BITS {
            return Err(SynthError::MalformedSpec {
                detail: format!("reg {i} row has {} bits, expected {BITS}", row.len()),
            });
        }
        let dst = dst_ops[i];
        // Genuine identity self-map contributes nothing (matches the ANF path).
        if is_identity_row(row, i) && dst == src_ops[i] {
            continue;
        }
        for (b, e) in row.iter().enumerate() {
            emit_bdd_output_bit(e, dst, b, src_ops, &helpers, &map, &mut out)?;
        }
    }
    Ok(out)
}

/// Emit gates that XOR `expr` (a boolean function of the input vars) into bit `b`
/// of `dst`, via ROBDD synthesis with per-node ancilla + Bennett uncompute.
fn emit_bdd_output_bit(
    expr: &BvExpr,
    dst: u8,
    b: usize,
    src_ops: &[u8],
    helpers: &BddHelpers,
    map: &VarMap,
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    let bdd = expr_to_bdd(expr, map).map_err(|e| SynthError::MalformedSpec {
        detail: format!("expr_to_bdd failed for output bit {b}: {e}"),
    })?;

    let root = bdd.root_pointer();
    // Constant output bit: no nodes to walk.
    if root.is_zero() {
        return Ok(()); // dst bit b already 0
    }
    if root.is_one() {
        out.push(Instr::Xori {
            rd: dst,
            imm: 1u8 << b,
        });
        return Ok(());
    }

    // Assign a node WORD to every non-terminal pointer (ascending order = a
    // children-before-parents topological order). node_word[pointer_index] = word.
    let n_nodes = bdd.size() - 2; // exclude the two terminals
    if n_nodes > helpers.nodes.len() {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "output bit {b} ROBDD has {n_nodes} nodes, helper node bank has only {} words",
                helpers.nodes.len()
            ),
        });
    }
    // Map a pointer index (≥2) to its allocated node word.
    let node_word = |idx: usize| -> u8 { helpers.nodes[idx - 2] };
    // Interpret a child pointer as a clean bit-0 Line.
    let child_line = |p: biodivine_lib_bdd::BddPointer| -> Line {
        if p.is_zero() {
            Line::Zero
        } else if p.is_one() {
            Line::One
        } else {
            Line::Node(node_word(p.to_index()))
        }
    };

    // Record, per node (in emission order), the gadget parameters so the reverse
    // pass can replay them exactly (the gadget is its own inverse).
    let mut gadgets: Vec<NodeGadgetRef> = Vec::with_capacity(n_nodes);

    // FORWARD PASS: compute every node line, children before parents.
    for p in bdd.pointers() {
        if p.is_terminal() {
            continue;
        }
        let idx = p.to_index();
        let var = bdd.var_of(p).to_index();
        // Invert the shared VarMap: idx = bit * NUM_REGS + operand.
        let var_reg = var % NUM_REGS;
        let var_bit = var / NUM_REGS;
        let lo = child_line(bdd.low_link_of(p)); // x = 0 branch
        let hi = child_line(bdd.high_link_of(p)); // x = 1 branch
        let g = NodeGadgetRef {
            word: node_word(idx),
            var_reg,
            var_bit,
            lo,
            hi,
        };
        emit_node_gadget(&g, src_ops, helpers, out)?;
        gadgets.push(g);
    }

    // Copy the ROOT node line into dst bit b: dst bit b ^= root bit0.
    // Root is a non-terminal (handled the terminal cases above), so it's a clean
    // node word -- a single Ccnot against a bit-b mask realized by the temp word.
    let root_word = node_word(root.to_index());
    // dst bit b ^= root_word bit0. Align root's bit0 to bit b, then a masked XOR.
    // root_word is clean (bit0 only). Rotate it so bit0 → bit b, XOR the whole
    // word onto dst (only bit b is set), rotate back. dst distinct from root_word.
    if dst == root_word {
        return Err(SynthError::MalformedSpec {
            detail: format!("dst operand {dst} aliases a node word (must be disjoint)"),
        });
    }
    rotate_by(root_word, b as i32, out); // bit0 → bit b
    out.push(Instr::Xor {
        rd: dst,
        rs: root_word,
    });
    rotate_by(root_word, -(b as i32), out); // restore

    // REVERSE PASS: uncompute every node line (replay its gadget = its inverse).
    for g in gadgets.iter().rev() {
        emit_node_gadget(g, src_ops, helpers, out)?;
    }
    Ok(())
}

/// Emit the 4-step Shannon-mux gadget for one node
/// `word ^= lo ⊕ (x ∧ (hi ⊕ lo))`, all at bit 0, net-zero on `temp`.
///
/// Because every step XOR-accumulates onto `word`/`temp`, re-emitting the SAME
/// gadget is its own inverse -- the reverse pass uses this to clear the word.
fn emit_node_gadget(
    g: &NodeGadgetRef,
    src_ops: &[u8],
    helpers: &BddHelpers,
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    let word = g.word;
    let temp = helpers.temp;
    // Validate the operands this gadget will actually use are legal + distinct
    // where the ISA requires it. `word` and `temp` must differ; the Ccnot needs
    // word, x_word, temp pairwise-distinct.
    if word == temp {
        return Err(SynthError::MalformedSpec {
            detail: format!("node word {word} aliases the temp helper {temp}"),
        });
    }

    // Step 1: word ^= lo  (copy clean low child).
    xor_clean_line(word, g.lo, out)?;

    // Steps 2/4 wrap the Ccnot: if hi == lo the AND term is 0 (n = lo already),
    // so skip the diff+Ccnot entirely (also avoids a degenerate empty temp).
    if !lines_equal(g.hi, g.lo) {
        // Step 2: temp = hi ⊕ lo  (clean, bit 0).
        xor_clean_line(temp, g.hi, out)?;
        xor_clean_line(temp, g.lo, out)?;

        // Step 3: word bit0 ^= x_bit ∧ temp_bit0. Align x's bit to bit 0.
        let x_word = src_ops[g.var_reg];
        if x_word == word || x_word == temp {
            return Err(SynthError::MalformedSpec {
                detail: format!(
                    "input source operand {x_word} aliases node word {word} or temp {temp} \
                     (Ccnot needs distinct operands)"
                ),
            });
        }
        let sh = -(g.var_bit as i32); // bring bit var_bit → bit 0
        rotate_by(x_word, sh, out);
        out.push(Instr::Ccnot {
            rd: word,
            rs: x_word,
            rt: temp,
        });
        rotate_by(x_word, -sh, out);

        // Step 4: restore temp = 0 (reverse of step 2).
        xor_clean_line(temp, g.lo, out)?;
        xor_clean_line(temp, g.hi, out)?;
    }
    Ok(())
}

/// Borrowed view of a node gadget (owned `NodeGadget` lives in
/// `emit_bdd_output_bit`; this is the shape `emit_node_gadget` consumes).
struct NodeGadgetRef {
    word: u8,
    var_reg: usize,
    var_bit: usize,
    lo: Line,
    hi: Line,
}

/// XOR a clean bit-0 [`Line`] onto bit 0 of `word`.
fn xor_clean_line(word: u8, line: Line, out: &mut Vec<Instr>) -> Result<(), SynthError> {
    match line {
        Line::Zero => {}
        Line::One => out.push(Instr::Xori { rd: word, imm: 1 }),
        Line::Node(w) => {
            if w == word {
                return Err(SynthError::MalformedSpec {
                    detail: format!("Xor operands alias (rd=rs={word}) copying a node line"),
                });
            }
            // Both words are clean (bit0 only) ⇒ plain Xor moves only bit 0.
            out.push(Instr::Xor { rd: word, rs: w });
        }
    }
    Ok(())
}

/// True iff two [`Line`]s denote the same value source.
fn lines_equal(a: Line, b: Line) -> bool {
    matches!((a, b), (Line::Zero, Line::Zero) | (Line::One, Line::One))
        || matches!((a, b), (Line::Node(x), Line::Node(y)) if x == y)
}

/// BDD analog of [`transcribe_bennett_general`]: realize a permutation `P`
/// (forward `fwd` + inverse `bwd`) in place on `carrier_ops`, computing the fwd
/// BDD into `out_ancilla`, swapping, then the bwd BDD to clear the ancilla --
/// but the compute/uncompute steps use [`transcribe_bdd_into`] (BDD synthesis,
/// compact) instead of ANF. Same net-zero guarantee.
///
/// Carries the same carrier-read / operand-disjointness / reserved-operand validation as
/// [`transcribe_bennett`]: rejects non-carrier reads, duplicate/aliased/reserved
/// carrier & ancilla operands, and specs touching non-carrier registers.
pub fn transcribe_bdd_bennett(
    fwd: &RegTransfer,
    bwd: &RegTransfer,
    carrier_ops: &[u8],
    out_ancilla: &[u8],
    helpers: BddHelpers,
) -> Result<Vec<Instr>, SynthError> {
    let n = carrier_ops.len();
    if out_ancilla.len() < n {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "output ancilla bank has {} words, need ≥ carriers ({n})",
                out_ancilla.len()
            ),
        });
    }
    if fwd.regs.len() != NUM_REGS || bwd.regs.len() != NUM_REGS {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "fwd/bwd frames must be {NUM_REGS} rows (fwd={}, bwd={})",
                fwd.regs.len(),
                bwd.regs.len()
            ),
        });
    }
    // Legal-operand guards (carry from transcribe_bennett): reject reserved
    // s0/s1 (8/9) and out-of-range (≥16) carriers, ancilla, temp, node words.
    for &c in &carrier_ops[..n] {
        check_operand_legal("carrier", c)?;
    }
    for &a in &out_ancilla[..n] {
        check_operand_legal("ancilla", a)?;
    }
    check_operand_legal("temp", helpers.temp)?;
    for &w in helpers.nodes {
        check_operand_legal("node", w)?;
    }

    // Internal-disjointness + no-dup guards. (The BDD variant checks temp/node
    // legality above, before the no-dup loop, and uses the plain "appears twice"
    // ancilla message -- so it validates its carrier/ancilla legality inline
    // rather than through `validate_bennett_operands`, to preserve that ordering.)
    for i in 0..n {
        for j in (i + 1)..n {
            if carrier_ops[i] == carrier_ops[j] {
                return Err(SynthError::MalformedSpec {
                    detail: format!("carrier operand {} appears twice", carrier_ops[i]),
                });
            }
            if out_ancilla[i] == out_ancilla[j] {
                return Err(SynthError::MalformedSpec {
                    detail: format!("ancilla operand {} appears twice", out_ancilla[i]),
                });
            }
        }
    }
    // All working words must be mutually disjoint: carriers, ancilla, temp, nodes.
    // This BDD-variant disjointness legitimately UNIFIES temp+nodes with the
    // carrier/ancilla words, so it is kept per-site (not force-merged into the
    // generic carrier/ancilla helper) -- only the pairwise-distinct sweep routes
    // through `validate_operands_distinct`.
    let mut all: Vec<u8> = Vec::new();
    all.extend_from_slice(&carrier_ops[..n]);
    all.extend_from_slice(&out_ancilla[..n]);
    all.push(helpers.temp);
    all.extend_from_slice(helpers.nodes);
    validate_operands_distinct(&all, || {
        // On a collision, report the FIRST duplicated operand (matching the
        // original nested-loop's `all[i]`).
        let dup = (0..all.len())
            .find_map(|i| all[i + 1..].iter().find(|&&x| x == all[i]).map(|_| all[i]))
            .unwrap_or(0);
        format!(
            "working operand {dup} is used twice (carriers/ancilla/temp/nodes must be \
             pairwise disjoint)"
        )
    })?;

    // a carrier row may only READ carriers (a permutation on `n`
    // carriers is a function of those carriers alone).
    validate_bennett_carrier_reads(fwd, bwd, n)?;
    // The permutation must be identity on non-carrier registers.
    validate_bennett_identity_rows(fwd, bwd, n)?;

    // Build the NUM_REGS-length src/dst maps (spec leaf r → carrier/ancilla op).
    let (src_map, dst_map) = build_bennett_operand_maps(carrier_ops, out_ancilla, n);

    let mut out = Vec::new();
    // 1. O ^= P(x)  (BDD synthesis: dst = ancilla, src = carriers = x).
    out.extend(transcribe_bdd_into(fwd, &dst_map, &src_map, helpers)?);
    // 2. swap carriers ↔ output ancilla.
    emit_bennett_swaps(carrier_ops, out_ancilla, n, &mut out)?;
    // 3. O ^= P⁻¹(y)  (carriers now hold y) → clears O to 0.
    out.extend(transcribe_bdd_into(bwd, &dst_map, &src_map, helpers)?);
    Ok(out)
}

// ── A-6 W.0: the word/arithmetic-level synthesis IR ──────────────────
//
// A small straight-line WORD-level IR (`WOp` / `WSpec`) whose ops map ~1:1 to
// the ISA's byte/arithmetic gates, plus reversible MACROS (guarded copy, is-zero,
// compare) that each expand to a small NET-ZERO gate sequence over clean temps
// while preserving their source operands. This is DETERMINISTIC construction --
// each `WOp` is a known gate, each macro a fixed gadget -- NOT search.
//
// The transcriber is the word analogue of `transcribe_inplace`: it is for specs
// already reversible in-place (an in-place bijection). The macros are the
// building blocks a word-level Bennett wrapper (W.1) composes. All operand
// indices are over reg∪scratch (0..8 reg, 10..16 legal scratch); every operand
// is validated with `check_operand_legal` (rejecting reserved 8/9 and ≥16) and
// Ccnot/Cswap operands must be pairwise distinct.

/// A word-level operation over operand indices (0..8 reg, 10..16 legal scratch).
///
/// Each core op maps 1:1 to one ISA gate (see [`transcribe_word`]). `d`/`s`/`t`
/// are operand indices; `imm` is the 8-bit immediate (for rotates only the low 3
/// bits are used by the ISA).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WOp {
    /// `w[d] ^= w[s]` → `Xor`.
    WXor { d: u8, s: u8 },
    /// `w[d] += w[s]` (mod 256) → `Add`.
    WAdd { d: u8, s: u8 },
    /// `w[d] -= w[s]` (mod 256) → `Sub`.
    WSub { d: u8, s: u8 },
    /// `w[d] = rotl(w[d], imm & 7)` → `Rotli`.
    WRotl { d: u8, imm: u8 },
    /// `w[d] = rotr(w[d], imm & 7)` → `Rotri`.
    WRotr { d: u8, imm: u8 },
    /// `w[d] ^= imm` → `Xori`.
    WXorConst { d: u8, imm: u8 },
    /// `w[d] ^= w[s] & w[t]` (byte-parallel) → `Ccnot`. Pairwise distinct.
    WCcnot { d: u8, s: u8, t: u8 },
    /// If `w[c] & 1 == 1` then `w[u] ↔ w[v]` → `Cswap`. Pairwise distinct.
    WCswap { c: u8, u: u8, v: u8 },
}

/// A straight-line word-level program.
pub type WSpec = Vec<WOp>;

/// Direct 1:1 transcriber: map each [`WOp`] to its corresponding ISA [`Instr`].
///
/// This is for specs that are already reversible IN PLACE (an in-place bijection
/// -- no separate output words). Every operand is validated with
/// [`check_operand_legal`] (reject reserved s0/s1 = 8/9 and out-of-range ≥16);
/// two-operand ops require `d != s` (the ISA's non-aliasing rule), and
/// `WCcnot`/`WCswap` require pairwise-distinct operands.
///
/// The result, when `Ok`, is an assemblable body realizing the WSpec. Callers
/// still verify with the symbolic oracle (`symbolic_exec` + `equiv`) -- this
/// function does not claim correctness, only faithful 1:1 transcription.
pub fn transcribe_word(spec: &[WOp]) -> Result<Vec<Instr>, SynthError> {
    let mut out = Vec::with_capacity(spec.len());
    for &op in spec {
        out.push(transcribe_word_op(op)?);
    }
    Ok(out)
}

/// Reject a two-operand `d,s` op with `d == s` (ISA non-aliasing rule; `encode`
/// would reject it, and it is non-injective on the datapath).
fn check_two_distinct(mnemonic: &str, d: u8, s: u8) -> Result<(), SynthError> {
    if d == s {
        return Err(SynthError::MalformedSpec {
            detail: format!("{mnemonic} operands d={d}, s={s} alias (must differ)"),
        });
    }
    Ok(())
}

/// Reject a three-operand op whose operands are not pairwise distinct (Ccnot /
/// Cswap ISA requirement; non-distinct is ISA-illegal / non-bijective).
fn check_three_distinct(mnemonic: &str, a: u8, b: u8, c: u8) -> Result<(), SynthError> {
    if a == b || a == c || b == c {
        return Err(SynthError::MalformedSpec {
            detail: format!("{mnemonic} operands ({a},{b},{c}) not pairwise distinct"),
        });
    }
    Ok(())
}

/// Transcribe one [`WOp`] into its ISA [`Instr`], validating operand legality.
fn transcribe_word_op(op: WOp) -> Result<Instr, SynthError> {
    match op {
        WOp::WXor { d, s } => {
            check_operand_legal("d", d)?;
            check_operand_legal("s", s)?;
            check_two_distinct("WXor", d, s)?;
            Ok(Instr::Xor { rd: d, rs: s })
        }
        WOp::WAdd { d, s } => {
            check_operand_legal("d", d)?;
            check_operand_legal("s", s)?;
            check_two_distinct("WAdd", d, s)?;
            Ok(Instr::Add { rd: d, rs: s })
        }
        WOp::WSub { d, s } => {
            check_operand_legal("d", d)?;
            check_operand_legal("s", s)?;
            check_two_distinct("WSub", d, s)?;
            Ok(Instr::Sub { rd: d, rs: s })
        }
        WOp::WRotl { d, imm } => {
            check_operand_legal("d", d)?;
            Ok(Instr::Rotli { rd: d, imm })
        }
        WOp::WRotr { d, imm } => {
            check_operand_legal("d", d)?;
            Ok(Instr::Rotri { rd: d, imm })
        }
        WOp::WXorConst { d, imm } => {
            check_operand_legal("d", d)?;
            Ok(Instr::Xori { rd: d, imm })
        }
        WOp::WCcnot { d, s, t } => {
            check_operand_legal("d", d)?;
            check_operand_legal("s", s)?;
            check_operand_legal("t", t)?;
            check_three_distinct("WCcnot", d, s, t)?;
            Ok(Instr::Ccnot {
                rd: d,
                rs: s,
                rt: t,
            })
        }
        WOp::WCswap { c, u, v } => {
            check_operand_legal("c", c)?;
            check_operand_legal("u", u)?;
            check_operand_legal("v", v)?;
            check_three_distinct("WCswap", c, u, v)?;
            Ok(Instr::Cswap {
                rd: c,
                rs: u,
                rt: v,
            })
        }
    }
}

// ── W.0 reversible macros ────────────────────────────────────────────
//
// Each macro emits a small NET-ZERO gate sequence: every temp word it touches
// returns to its input value (so, starting from 0, it ends at 0), and every
// SOURCE operand is preserved. The result-carrying `dst` is XOR-accumulated
// (`dst ^= ...`), so a clean (zero) `dst` receives the intended word/bit.
//
// Net-zero holds UNCONDITIONALLY (for any temp init) because every temp mutation
// is paired with its exact inverse -- this is what makes the symbolic oracle
// (which models a temp's initial value as a free variable) able to PROVE
// net-zero, not merely sample it. The `dst ^= f(guard,then,else)` contract, by
// contrast, is only correct when the temps START at 0 (the mask/accumulator are
// seeded from 0); callers/tests verify the dst function with the temp inputs
// substituted by 0 (the `zero_out` pattern), exactly as the Bennett path does.

/// `WSelect(dst, guard, then_w, else_w, mask_tmp, one)` -- guarded word copy
/// `dst ^= guard ? then_w : else_w`, selected by BIT 0 of `guard`.
///
/// # Contract
/// - The guard is BIT 0 of `guard`; higher bits of `guard` are IGNORED (isolated
///   internally), so any guard byte is accepted (no bit-word precondition).
/// - `dst` is XOR-accumulated (`dst ^= selected`); for a plain copy start it at 0.
/// - `mask_tmp` and `one` are zero-init scratch words, returned to 0 (net-zero).
/// - `guard`, `then_w`, `else_w` are PRESERVED.
///
/// # Gadget (net-zero)
///   isolate:   `one ^= 1; Ccnot(mask, guard, one)`   ⇒ mask.bit0 = guard.bit0
///   broadcast: `mask ^= 0xFF; mask += 1`             ⇒ mask = 0x00 / 0xFF
///   copy:      `dst ^= else_w; Ccnot(dst, then_w, mask); Ccnot(dst, else_w, mask)`
///              ⇒ `dst ^= else ⊕ ((then⊕else) & mask)` = guard?then:else
///   uncompute: reverse the broadcast + isolate ⇒ mask, one back to 0.
///
/// Distinctness: `mask_tmp`/`one` are fresh temps distinct from every other
/// operand; the two guarded-copy `Ccnot`s need `dst` distinct from `then_w` /
/// `else_w`; all six operands must be legal (validated below).
pub fn emit_wselect(
    dst: u8,
    guard: u8,
    then_w: u8,
    else_w: u8,
    mask_tmp: u8,
    one: u8,
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    for (what, op) in [
        ("dst", dst),
        ("guard", guard),
        ("then_w", then_w),
        ("else_w", else_w),
        ("mask_tmp", mask_tmp),
        ("one", one),
    ] {
        check_operand_legal(what, op)?;
    }
    // mask_tmp and one must be fresh: distinct from every other operand and each
    // other (they are net-zero scratch; aliasing defeats the isolate/broadcast).
    for &t in &[mask_tmp, one] {
        if [dst, guard, then_w, else_w].contains(&t) {
            return Err(SynthError::MalformedSpec {
                detail: format!("WSelect temp {t} aliases a value operand"),
            });
        }
    }
    if mask_tmp == one {
        return Err(SynthError::MalformedSpec {
            detail: format!("WSelect mask_tmp and one alias ({mask_tmp})"),
        });
    }
    // Ccnot legality: dst ≠ then_w, dst ≠ else_w (rt=mask already ≠ dst); and the
    // isolate Ccnot needs mask,guard,one distinct (guard ≠ one, mask ≠ guard).
    if dst == then_w || dst == else_w {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "WSelect dst {dst} aliases a source word (then={then_w}, else={else_w}); \
                 the guarded-copy Ccnot needs dst distinct from its sources"
            ),
        });
    }
    // Isolate guard bit0 into mask_tmp (clean), then broadcast to 0x00/0xFF.
    out.push(Instr::Xori { rd: one, imm: 1 }); // one.bit0 = 1
    out.push(Instr::Ccnot {
        rd: mask_tmp,
        rs: guard,
        rt: one,
    }); // mask.bit0 = guard.bit0
    out.push(Instr::Xori {
        rd: mask_tmp,
        imm: 0xFF,
    }); // mask = ~mask
    out.push(Instr::Addi {
        rd: mask_tmp,
        imm: 1,
    }); // mask = -guardbit0 = 0x00/0xFF
        // Guarded copy: dst ^= else ⊕ (then⊕else)&mask.
    out.push(Instr::Xor {
        rd: dst,
        rs: else_w,
    }); // dst ^= else
    out.push(Instr::Ccnot {
        rd: dst,
        rs: then_w,
        rt: mask_tmp,
    }); // dst ^= then & mask
    out.push(Instr::Ccnot {
        rd: dst,
        rs: else_w,
        rt: mask_tmp,
    }); // dst ^= else & mask
        // Uncompute mask + one (exact inverse of isolate/broadcast).
    out.push(Instr::Subi {
        rd: mask_tmp,
        imm: 1,
    });
    out.push(Instr::Xori {
        rd: mask_tmp,
        imm: 0xFF,
    });
    out.push(Instr::Ccnot {
        rd: mask_tmp,
        rs: guard,
        rt: one,
    });
    out.push(Instr::Xori { rd: one, imm: 1 });
    Ok(())
}

/// `WEqZero(dst_bit, src, temps)` -- `dst_bit's bit 0 ^= (src == 0)` (byte-wide),
/// with `src` PRESERVED and every temp word NET-ZERO.
///
/// # Deviation from the design's single-`tmp` signature (REPORTED)
/// A reversible OR-reduce of `src`'s 8 bits into one bit, over the 2-input-Ccnot
/// ISA, cannot be done net-zero with a single scratch word: `OR(a,x)=a⊕x⊕(a∧x)`
/// needs the AND of the ORIGINAL operands to cancel, but an in-place merge
/// destroys the accumulator's old value first. The clean, provably net-zero
/// construction is a Bennett compute→use→uncompute of an OR-CHAIN: build the
/// running OR of bits `0..=k` into a chain of CLEAN partial-OR words `p[k]`, read
/// the final one, then replay the chain in REVERSE to clear every `p[k]`. This
/// needs `BITS = 8` partial-OR words (`p[k] = OR(src bits 0..=k)`) plus one `one`
/// word (holding `0x01`, to isolate a single bit under `Ccnot`), so `WEqZero`
/// takes a `temps` slice of `len ≥ BITS+1 = 9`: `temps[0..8]` are the partial-OR
/// words, `temps[8]` is `one`. All are net-zero.
///
/// # Gadget (net-zero, all work confined to bit 0)
///   setup:  `one ^= 1`                              (one.bit0 = 1)
///   chain:  `p[0].bit0 := src.bit0`                 (one OR node vs implicit 0)
///           `p[k].bit0 := p[k-1] | src.bit_k`       (clean OR node, k = 1..7)
///   use:    `dst.bit0 ^= ¬p[7]` = is-zero           (`Xori dst,1 ; Ccnot dst,p7,one`)
///   reverse: replay the 8 OR nodes backwards → every `p[k]` back to 0
///   teardown: `one ^= 1`                            (one back to 0)
///
/// One OR node `w.bit0 = acc.bit0 | src.bit_k` into a CLEAN `w`, with `src` bit
/// `k` isolated to bit 0 by a paired rotate and the `one` mask:
///   `Ccnot(w, src, one)`            w.bit0 ^= src.bit_k          ( ⊕ x )
///   `Xor(w, acc)`                   w.bit0 ^= acc.bit0           ( ⊕ acc )   [k>0]
///   `Ccnot(w, acc, src)`           w.bit0 ^= acc.bit0 ∧ src.bit_k ( ⊕ acc∧x )[k>0]
/// so `w.bit0 = x ⊕ acc ⊕ acc∧x = acc | x`. Both `acc` (a prior p-word, bit-0
/// only) and `one` keep the AND/XOR confined to bit 0, so `w` stays bit-0-only.
/// Every op XOR-accumulates onto the clean `w`, so the node is self-inverse and
/// the reverse pass clears it.
pub fn emit_weqzero(
    dst_bit: u8,
    src: u8,
    temps: &[u8],
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    emit_weqzero_width(dst_bit, src, BITS, temps, out)
}

/// Number of temp words [`emit_weqzero_width`] needs for a `width`-bit reduce:
/// `width` partial-OR words + one `0x01` isolate word.
pub const fn weqzero_ntemp(width: usize) -> usize {
    width + 1
}

/// WIDTH-AWARE [`emit_weqzero`] (A-6 W.2): `dst_bit.bit0 ^= (src's low `width`
/// bits == 0)`. Reduces ONLY bits `0..width` of `src`, so for a NARROW operand
/// (gcd2's `a`/`b` = 2 bits, `h` = 3 bits) it emits far fewer gates and needs
/// only [`weqzero_ntemp`]`(width) = width+1` temps instead of the full-8-bit 9.
///
/// The gadget is identical to the full-width construction (see [`emit_weqzero`]),
/// merely stopping the OR-chain at bit `width-1`. `src`'s bits ≥ `width` are
/// IGNORED -- the caller guarantees they are 0 on the operand's meaningful domain
/// (true for gcd2's bit-packed carriers), which the whole-domain exec confirms.
///
/// `width` must be in `1..=BITS`. `src` is PRESERVED; every temp is NET-ZERO.
pub fn emit_weqzero_width(
    dst_bit: u8,
    src: u8,
    width: usize,
    temps: &[u8],
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    check_operand_legal("dst_bit", dst_bit)?;
    check_operand_legal("src", src)?;
    if width == 0 || width > BITS {
        return Err(SynthError::MalformedSpec {
            detail: format!("WEqZero width {width} must be in 1..={BITS}"),
        });
    }
    let ntemp = weqzero_ntemp(width); // `width` partial-OR words + 1 `one` word
    if temps.len() < ntemp {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "WEqZero(width={width}) needs ≥ {ntemp} temp words, have {}",
                temps.len()
            ),
        });
    }
    let p = &temps[..width]; // p[0..width): running-OR partial words
    let one = temps[width]; // holds 0x01 to isolate a single bit under Ccnot
    for (i, &t) in temps[..ntemp].iter().enumerate() {
        check_operand_legal(&format!("temp{i}"), t)?;
    }
    // dst_bit, src, and every temp must be pairwise distinct (fresh temps +
    // distinct Ccnot operands throughout).
    let mut all: Vec<u8> = vec![dst_bit, src];
    all.extend_from_slice(&temps[..ntemp]);
    validate_operands_distinct(&all, || {
        format!(
            "WEqZero operands not distinct (dst_bit={dst_bit}, src={src}, temps={:?})",
            &temps[..ntemp]
        )
    })?;

    // One OR node: `w.bit0 = acc.bit0 | src.bit_k` into clean `w`. `acc = None`
    // means the implicit clean-zero accumulator (the chain seed, p[0]). `src` is
    // rotated so bit `k` sits at bit 0 for the node, then rotated back (preserved).
    // The gadget is self-inverse (all XOR-accumulate onto clean `w`), so the same
    // closure, appended in reverse node order, uncomputes the chain.
    let emit_or_node = |out: &mut Vec<Instr>, w: u8, acc: Option<u8>, k: usize| {
        let rot = (k as u8) & 7;
        if rot != 0 {
            out.push(Instr::Rotri { rd: src, imm: rot }); // src bit k → bit 0
        }
        out.push(Instr::Ccnot {
            rd: w,
            rs: src,
            rt: one,
        }); // w.bit0 ^= x
        if let Some(a) = acc {
            out.push(Instr::Xor { rd: w, rs: a }); // w.bit0 ^= acc
            out.push(Instr::Ccnot {
                rd: w,
                rs: a,
                rt: src,
            }); // w.bit0 ^= acc∧x
        }
        if rot != 0 {
            out.push(Instr::Rotli { rd: src, imm: rot }); // restore src
        }
    };

    // setup
    out.push(Instr::Xori { rd: one, imm: 1 });
    // forward chain: p[0] = src.bit0 ; p[k] = p[k-1] | src.bit_k  (k = 1..width).
    emit_or_node(out, p[0], None, 0);
    for k in 1..width {
        emit_or_node(out, p[k], Some(p[k - 1]), k);
    }
    // p[width-1] = OR of all `width` src bits; is-zero = ¬p[width-1].bit0.
    let last = p[width - 1];
    out.push(Instr::Xori {
        rd: dst_bit,
        imm: 1,
    }); // dst.bit0 ^= 1
    out.push(Instr::Ccnot {
        rd: dst_bit,
        rs: last,
        rt: one,
    }); // dst.bit0 ^= p_last
        // reverse chain: uncompute p[k] for k = width-1 .. 1, then p[0].
    for k in (1..width).rev() {
        emit_or_node(out, p[k], Some(p[k - 1]), k);
    }
    emit_or_node(out, p[0], None, 0);
    // teardown
    out.push(Instr::Xori { rd: one, imm: 1 });
    Ok(())
}

/// Number of temp words [`emit_wlt`] / [`emit_wgt`] require: one clean borrow
/// word for each of bits `0..BITS-1` (`BITS-1` words -- the final borrow-out is
/// folded straight into `dst_bit`, so no word for it) + one `na` scratch
/// (holding `¬a_k` at bit 0) + one `one` mask word (`0x01`, to isolate a single
/// bit under `Ccnot`). Kept at 9 so a `Lt`/`Gt` guard fits inside the word-Bennett
/// wrapper's operand budget (2 carriers + 2 ancilla + ≤ 10 work ≤ 14 locations).
pub const WLT_NTEMP: usize = (BITS - 1) + 2;

/// `WLt(dst_bit, a, b, temps)` -- `dst_bit's bit0 ^= (a < b)` (unsigned byte
/// compare), `a`/`b` PRESERVED, every temp word NET-ZERO.
///
/// # The predicate
/// Unsigned `a < b` is exactly the borrow-OUT of the full 8-bit subtract
/// `a - b`. The borrow propagates LSB→MSB by the recurrence (with borrow-in
/// `bin`, borrow-out `bout` at bit `k`):
///   `bout = maj(¬a_k, b_k, bin) = (¬a_k ∧ b_k) ⊕ (¬a_k ∧ bin) ⊕ (b_k ∧ bin)`
/// seeded `bin = 0` at bit 0. `a < b` = the borrow out of bit 7.
///
/// # Gadget (net-zero, all work confined to bit 0)
/// The same clean-chain compute→use→uncompute discipline as [`emit_weqzero`]:
/// build a chain of CLEAN borrow words `bor[k] = borrow-out of bits 0..=k` (one
/// per bit), read the final one into `dst_bit`, then replay the chain in REVERSE
/// so every intermediate `bor[k]` returns to 0. Each borrow node isolates
/// `a_k`/`b_k` to bit 0 with a paired `Rotr`/`Rotl` (so `a`/`b` are preserved),
/// forms `¬a_k` in a clean `na` word (`Ccnot(na,a,one); Xori(na,1)`, then
/// uncomputed), and XOR-accumulates the three majority AND terms onto the target
/// word. The final bit's borrow-out IS the answer, so it is XOR-accumulated
/// straight into `dst_bit` (no chain word for it, and no reverse for that node's
/// dst term -- `dst_bit` is the output). The remaining `BITS-1` intermediate
/// borrows are clean chain words, each self-inverse, so the reverse pass clears
/// them. `na` is uncomputed inside every node (including the final one).
///
/// # Temps
/// `temps.len() ≥ WLT_NTEMP = (BITS-1) + 2 = 9`: `temps[0..BITS-1]` are the
/// intermediate borrow words (`temps[k]` = borrow-out of bit `k`, `k < BITS-1`,
/// seeded from the previous), `temps[BITS-1]` is `na`, `temps[BITS]` is `one`.
/// All are net-zero.
///
/// `dst_bit`, `a`, `b`, and every temp must be pairwise distinct legal operands
/// (fresh temps + distinct `Ccnot` operands throughout).
pub fn emit_wlt(
    dst_bit: u8,
    a: u8,
    b: u8,
    temps: &[u8],
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    emit_wlt_width(dst_bit, a, b, BITS, temps, out)
}

/// Number of temp words [`emit_wlt_width`] / [`emit_wgt_width`] need for a
/// `width`-bit compare: `width-1` intermediate borrow words + `na` + `one`.
pub const fn wlt_ntemp(width: usize) -> usize {
    (width - 1) + 2
}

/// WIDTH-AWARE [`emit_wlt`] (A-6 W.2): `dst_bit.bit0 ^= (a < b)` over the low
/// `width` bits (unsigned). Runs the borrow chain over ONLY bits `0..width`, so a
/// NARROW compare (gcd2's `a`/`b` = 2 bits) needs only [`wlt_ntemp`]`(width) =
/// width+1` temps instead of the full-8-bit 9, and emits far fewer gates.
///
/// The gadget is the full-width construction (see [`emit_wlt`]) truncated to
/// `width` bits: `a < b` is the borrow-out of bit `width-1` of the `width`-bit
/// subtract. Bits ≥ `width` of `a`/`b` are IGNORED (0 on the meaningful domain).
///
/// `width` must be in `1..=BITS`. `a`/`b` are PRESERVED; every temp is NET-ZERO.
pub fn emit_wlt_width(
    dst_bit: u8,
    a: u8,
    b: u8,
    width: usize,
    temps: &[u8],
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    check_operand_legal("dst_bit", dst_bit)?;
    check_operand_legal("a", a)?;
    check_operand_legal("b", b)?;
    if width == 0 || width > BITS {
        return Err(SynthError::MalformedSpec {
            detail: format!("WLt width {width} must be in 1..={BITS}"),
        });
    }
    let ntemp = wlt_ntemp(width);
    if temps.len() < ntemp {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "WLt(width={width}) needs ≥ {ntemp} temp words, have {}",
                temps.len()
            ),
        });
    }
    for (i, &t) in temps[..ntemp].iter().enumerate() {
        check_operand_legal(&format!("temp{i}"), t)?;
    }
    let nbor: usize = width - 1; // intermediate borrow words (bits 0..width-1)
    let bor = &temps[..nbor]; // bor[k] = borrow-out of bit k (k < width-1), clean
    let na = temps[nbor]; // holds ¬a_k at bit 0 (clean, uncomputed per node)
    let one = temps[nbor + 1]; // holds 0x01 to isolate a single bit under Ccnot

    // All of dst_bit, a, b, and the temps must be pairwise distinct.
    let mut all: Vec<u8> = vec![dst_bit, a, b];
    all.extend_from_slice(&temps[..ntemp]);
    validate_operands_distinct(&all, || {
        format!(
            "WLt operands not distinct (dst_bit={dst_bit}, a={a}, b={b}, temps={:?})",
            &temps[..ntemp]
        )
    })?;

    // One borrow node: XOR-accumulate `maj(¬a_k, b_k, bin)` into the target word
    // `c` (a clean chain word, or `dst_bit` for the final bit), with `bin` = the
    // previous borrow word (`None` at bit 0 = seed 0). `a`/`b` are rotated so bit
    // `k` sits at bit 0 for the AND/Ccnot terms, then rotated back (preserved).
    // The node XOR-accumulates onto `c` and self-inverse-uncomputes `na`, so --
    // for the CHAIN words -- the same closure, appended in reverse bit order,
    // clears the chain. (The final node targets `dst_bit` and is NOT reversed.)
    let emit_borrow_node = |out: &mut Vec<Instr>, k: usize, bin: Option<u8>, c: u8| {
        let rot = (k as u8) & 7;
        if rot != 0 {
            out.push(Instr::Rotri { rd: a, imm: rot }); // a bit k → bit 0
            out.push(Instr::Rotri { rd: b, imm: rot }); // b bit k → bit 0
        }
        // na.bit0 = ¬a_k  (compute; uncomputed at the tail of this node).
        out.push(Instr::Ccnot {
            rd: na,
            rs: a,
            rt: one,
        }); // na.bit0 ^= a_k
        out.push(Instr::Xori { rd: na, imm: 1 }); // na.bit0 ^= 1  → ¬a_k
        out.push(Instr::Ccnot {
            rd: c,
            rs: na,
            rt: b,
        }); // c ^= ¬a_k ∧ b_k
        if let Some(prev) = bin {
            out.push(Instr::Ccnot {
                rd: c,
                rs: na,
                rt: prev,
            }); // c ^= ¬a_k ∧ bin
            out.push(Instr::Ccnot {
                rd: c,
                rs: b,
                rt: prev,
            }); //  c ^= b_k ∧ bin
        }
        // uncompute na
        out.push(Instr::Xori { rd: na, imm: 1 });
        out.push(Instr::Ccnot {
            rd: na,
            rs: a,
            rt: one,
        });
        if rot != 0 {
            out.push(Instr::Rotli { rd: a, imm: rot }); // restore a
            out.push(Instr::Rotli { rd: b, imm: rot }); // restore b
        }
    };

    // setup
    out.push(Instr::Xori { rd: one, imm: 1 });
    // forward chain: bor[0] = ¬a_0 ∧ b_0 (seed bin=0); bor[k] from bor[k-1] for
    // bits 1..width-1. These are the INTERMEDIATE borrows (clean chain words).
    // For width == 1 there are NO intermediate borrows (nbor == 0): the sole bit's
    // borrow-out IS the answer, computed directly into dst_bit with seed bin=None.
    if nbor > 0 {
        emit_borrow_node(out, 0, None, bor[0]);
        for k in 1..nbor {
            emit_borrow_node(out, k, Some(bor[k - 1]), bor[k]);
        }
    }
    // final bit (width-1): borrow-out = (a<b). XOR it straight into dst_bit, using
    // the last intermediate borrow bor[width-2] (if any) as borrow-in. Not reversed.
    let final_bin = if nbor > 0 { Some(bor[nbor - 1]) } else { None };
    emit_borrow_node(out, width - 1, final_bin, dst_bit);
    // reverse chain: uncompute bor[k] for k = nbor-1 .. 1, then bor[0].
    if nbor > 0 {
        for k in (1..nbor).rev() {
            emit_borrow_node(out, k, Some(bor[k - 1]), bor[k]);
        }
        emit_borrow_node(out, 0, None, bor[0]);
    }
    // teardown
    out.push(Instr::Xori { rd: one, imm: 1 });
    Ok(())
}

/// `WGt(dst_bit, a, b, temps)` -- `dst_bit's bit0 ^= (a > b)` (unsigned), by the
/// identity `a > b ⇔ b < a`. Same temp budget ([`WLT_NTEMP`]) and net-zero /
/// preserve contract as [`emit_wlt`].
pub fn emit_wgt(
    dst_bit: u8,
    a: u8,
    b: u8,
    temps: &[u8],
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    emit_wlt(dst_bit, b, a, temps, out)
}

/// WIDTH-AWARE [`emit_wgt`] (A-6 W.2): `dst_bit.bit0 ^= (a > b)` over the low
/// `width` bits, by `a > b ⇔ b < a`. Same temp budget ([`wlt_ntemp`]) and
/// net-zero / preserve contract as [`emit_wlt_width`].
pub fn emit_wgt_width(
    dst_bit: u8,
    a: u8,
    b: u8,
    width: usize,
    temps: &[u8],
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    emit_wlt_width(dst_bit, b, a, width, temps, out)
}

// ══════════════════════════════════════════════════════════════════════
// W.1 -- the word-level Bennett wrapper
// ══════════════════════════════════════════════════════════════════════
//
// The word analogue of [`transcribe_bennett`]. A permutation is described by a
// FORWARD word function [`WForward`] (each output word as a [`WValue`] of the
// carriers) plus its inverse `bwd`; the wrapper emits the textbook reversible
// in-place-permutation:
//   1. O ^= fwd(x)   -- XOR-accumulate each output value into a CLEAN ancilla O_i
//                      via the Bennett-clean idiom (compute value → temp; XOR into
//                      O_i; UNCOMPUTE temp so temp returns to 0). Keeping O a pure
//                      XOR-accumulation is what makes step 3 cancel.
//   2. swap carriers ↔ O   -- Exch/Xchg per carrier↔ancilla pair.
//   3. O ^= bwd(y)   -- same idiom with the inverse; carriers now hold y = fwd(x),
//                      O holds x, so O ^= fwd⁻¹(y) = x → O = 0 (net-zero).
//
// Every carrier/ancilla/work guard from `transcribe_bennett` is carried: carrier
// rows read only carriers, operands are legal (reserved 8/9 and ≥16 rejected),
// carriers/ancilla/work are pairwise disjoint and non-duplicated.

/// A word-valued expression over CARRIER operands, realized reversibly into a
/// clean temp (and uncomputed) by the Bennett-clean idiom. Indices in `Var` /
/// `Lt` / `Gt` are CARRIER indices (0..n), resolved to operands by the wrapper.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WValue {
    /// Carrier word `carriers[i]`.
    Var(usize),
    /// The 8-bit constant `c`.
    Const(u8),
    /// `x + y` (mod 256).
    Add(Box<WValue>, Box<WValue>),
    /// `x - y` (mod 256).
    Sub(Box<WValue>, Box<WValue>),
    /// `rotl(x, imm & 7)`.
    Rotl(Box<WValue>, u8),
    /// `rotr(x, imm & 7)`.
    Rotr(Box<WValue>, u8),
    /// `x ^ y`.
    Xor(Box<WValue>, Box<WValue>),
    /// A bit-0 predicate word `(x < y)` (unsigned); higher bits 0.
    Lt(Box<WValue>, Box<WValue>),
    /// A bit-0 predicate word `(x > y)` (unsigned); higher bits 0.
    Gt(Box<WValue>, Box<WValue>),
    /// WIDTH-AWARE `(x < y)` over the low `width` bits (A-6 W.2). A bit-0
    /// predicate; uses [`emit_wlt_width`] → [`wlt_ntemp`]`(width)` temps.
    LtW {
        x: Box<WValue>,
        y: Box<WValue>,
        width: usize,
    },
    /// WIDTH-AWARE `(x > y)` over the low `width` bits (A-6 W.2).
    GtW {
        x: Box<WValue>,
        y: Box<WValue>,
        width: usize,
    },
    /// WIDTH-AWARE `(x == 0)` over the low `width` bits (A-6 W.2). A bit-0
    /// predicate; uses [`emit_weqzero_width`] → [`weqzero_ntemp`]`(width)` temps.
    EqZeroW { x: Box<WValue>, width: usize },
    /// Bit-0 boolean AND of two bit-0 predicates (A-6 W.2): `dst.bit0 =
    /// x.bit0 ∧ y.bit0`. Realized by one `Ccnot` into the clean destination -- far
    /// cheaper than the `Select(x, y, 0)` idiom (no mask/one, no else-temp), which
    /// is the compactness key for gcd2's multi-predicate arm guards (`d`, etc.).
    /// Pure-XOR-top: composes into the clean ancilla without a value-temp.
    AndBit { x: Box<WValue>, y: Box<WValue> },
    /// `guard.bit0 ? val : 0` (A-6 W.2) -- a byte MASK-and-copy: broadcast `guard`'s
    /// bit 0 to a `0x00/0xFF` mask, then `dst ^= val & mask`. Cheaper than
    /// `Select(guard, val, 0)` (needs only ONE scratch mask word -- no `one`, no
    /// else-temp), so a FLAT XOR of disjoint `Masked` arm-corrections keeps the
    /// concurrent-work peak low. Pure-XOR-top.
    Masked {
        guard: Box<WValue>,
        val: Box<WValue>,
    },
    /// `guard.bit0 ? then : else` (guard is any WValue; its bit 0 selects).
    Select {
        guard: Box<WValue>,
        then_: Box<WValue>,
        else_: Box<WValue>,
    },
}

// `add`/`sub`/`xor` are builder names for the IR, not arithmetic on `WValue`
// itself -- intentionally not the std `Add`/`Sub` traits.
#[allow(clippy::should_implement_trait)]
impl WValue {
    /// Convenience constructors.
    pub fn var(i: usize) -> Self {
        WValue::Var(i)
    }
    pub fn add(x: WValue, y: WValue) -> Self {
        WValue::Add(Box::new(x), Box::new(y))
    }
    pub fn sub(x: WValue, y: WValue) -> Self {
        WValue::Sub(Box::new(x), Box::new(y))
    }
    pub fn xor(x: WValue, y: WValue) -> Self {
        WValue::Xor(Box::new(x), Box::new(y))
    }
    pub fn rotl(x: WValue, imm: u8) -> Self {
        WValue::Rotl(Box::new(x), imm)
    }
    pub fn rotr(x: WValue, imm: u8) -> Self {
        WValue::Rotr(Box::new(x), imm)
    }
    pub fn lt(x: WValue, y: WValue) -> Self {
        WValue::Lt(Box::new(x), Box::new(y))
    }
    pub fn gt(x: WValue, y: WValue) -> Self {
        WValue::Gt(Box::new(x), Box::new(y))
    }
    /// Width-aware `x < y` over the low `width` bits.
    pub fn ltw(x: WValue, y: WValue, width: usize) -> Self {
        WValue::LtW {
            x: Box::new(x),
            y: Box::new(y),
            width,
        }
    }
    /// Width-aware `x > y` over the low `width` bits.
    pub fn gtw(x: WValue, y: WValue, width: usize) -> Self {
        WValue::GtW {
            x: Box::new(x),
            y: Box::new(y),
            width,
        }
    }
    /// Width-aware `x == 0` over the low `width` bits.
    pub fn eqzerow(x: WValue, width: usize) -> Self {
        WValue::EqZeroW {
            x: Box::new(x),
            width,
        }
    }
    /// Bit-0 boolean AND of two bit-0 predicates (one `Ccnot`).
    pub fn andbit(x: WValue, y: WValue) -> Self {
        WValue::AndBit {
            x: Box::new(x),
            y: Box::new(y),
        }
    }
    /// `guard.bit0 ? val : 0` via a broadcast byte mask (one scratch word).
    pub fn masked(guard: WValue, val: WValue) -> Self {
        WValue::Masked {
            guard: Box::new(guard),
            val: Box::new(val),
        }
    }
    pub fn select(guard: WValue, then_: WValue, else_: WValue) -> Self {
        WValue::Select {
            guard: Box::new(guard),
            then_: Box::new(then_),
            else_: Box::new(else_),
        }
    }
}

/// A forward word function: each named OUTPUT word (by ancilla slot index) is a
/// [`WValue`] of the carriers. `outputs[k] = (slot, value)` means ancilla slot
/// `slot` (index into `out_ancilla`) receives `value`. Slots not listed stay 0.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WForward {
    /// `(ancilla-slot, value)` pairs. Each slot at most once.
    pub outputs: Vec<(usize, WValue)>,
}

/// A pool of clean (zero-init) scratch WORD operands the Bennett-clean idiom and
/// the compare/select macros draw from. Reused sequentially across outputs; every
/// word is returned to 0. Must be disjoint from carriers and ancilla.
///
/// A single output's realization can need several nested temps at once (e.g.
/// `Select{ Sub(a,b), a }` needs a value temp + a Sub temp + then/else temps +
/// the WSelect mask/one, and a `Lt` guard needs [`WLT_NTEMP`] more). The wrapper
/// allocates from `work_ops` with a running high-water mark and reports if the
/// pool is too small.
pub type WorkOps = Vec<u8>;

/// Realize a [`WValue`] into a CLEAN temp `dst`, returning the compute op list
/// (also pushed onto `out`) so the caller can UNCOMPUTE by replaying the inverse
/// in reverse. `dst` must start at 0. Temps are drawn from `work[next..]`; `next`
/// is advanced past every temp this call consumes (nested realizations get fresh
/// temps). `carriers` maps `WValue::Var(i)` to its operand.
///
/// The returned `Vec<Instr>` is exactly what was appended to `out`; to uncompute,
/// emit `invert_body(&compute)` (reverse order, each instr self-or-paired-inverse
/// -- see [`invert_instr`]). Every macro used (WSelect/WLt) is itself net-zero, so
/// inverting the whole compute list restores `dst` and all nested temps to 0.
fn realize_wvalue(
    val: &WValue,
    dst: u8,
    carriers: &[u8],
    work: &[u8],
    next: &mut usize,
    peak: &mut usize,
    out: &mut Vec<Instr>,
) -> Result<Vec<Instr>, SynthError> {
    let start = out.len();
    match val {
        WValue::Var(i) => {
            let c = resolve_var(*i, carriers)?;
            check_two_distinct("WValue::Var copy", dst, c)?;
            out.push(Instr::Xor { rd: dst, rs: c }); // dst ^= carrier  (dst was 0)
        }
        WValue::Const(k) => {
            out.push(Instr::Xori { rd: dst, imm: *k });
        }
        WValue::Rotl(inner, imm) => {
            realize_wvalue(inner, dst, carriers, work, next, peak, out)?;
            out.push(Instr::Rotli { rd: dst, imm: *imm });
        }
        WValue::Rotr(inner, imm) => {
            realize_wvalue(inner, dst, carriers, work, next, peak, out)?;
            out.push(Instr::Rotri { rd: dst, imm: *imm });
        }
        WValue::Add(x, y) | WValue::Sub(x, y) | WValue::Xor(x, y) => {
            // dst := x, then combine with y. If y is a bare carrier we combine
            // with it directly (no temp); otherwise realize y into a SCRATCH temp
            // that is uncomputed + FREED (stack discipline) so peak stays low.
            realize_wvalue(x, dst, carriers, work, next, peak, out)?;
            let (yop, y_body, mark) = operand_or_temp(y, carriers, work, next, peak, out)?;
            match val {
                WValue::Add(..) => out.push(Instr::Add { rd: dst, rs: yop }),
                WValue::Sub(..) => out.push(Instr::Sub { rd: dst, rs: yop }),
                WValue::Xor(..) => out.push(Instr::Xor { rd: dst, rs: yop }),
                _ => unreachable!(),
            }
            free_temp(&y_body, mark, next, out);
        }
        WValue::Lt(x, y) | WValue::Gt(x, y) => {
            // Realize x,y into (carrier or freed temp) operands, run the compare
            // macro into dst.bit0, then uncompute + free the temps.
            let (xop, x_body, xmark) = operand_or_temp(x, carriers, work, next, peak, out)?;
            let (yop, y_body, ymark) = operand_or_temp(y, carriers, work, next, peak, out)?;
            let cmp_temps = alloc_work_n(work, next, peak, WLT_NTEMP)?;
            match val {
                WValue::Lt(..) => emit_wlt(dst, xop, yop, &cmp_temps, out)?,
                WValue::Gt(..) => emit_wgt(dst, xop, yop, &cmp_temps, out)?,
                _ => unreachable!(),
            }
            *next -= WLT_NTEMP; // cmp_temps are net-zero → free them
            free_temp(&y_body, ymark, next, out);
            free_temp(&x_body, xmark, next, out);
        }
        WValue::LtW { x, y, width } | WValue::GtW { x, y, width } => {
            // Width-aware compare: same shape as Lt/Gt but only `wlt_ntemp(width)`
            // scratch words (the compactness key for gcd2's narrow operands).
            let n = wlt_ntemp(*width);
            let (xop, x_body, xmark) = operand_or_temp(x, carriers, work, next, peak, out)?;
            let (yop, y_body, ymark) = operand_or_temp(y, carriers, work, next, peak, out)?;
            let cmp_temps = alloc_work_n(work, next, peak, n)?;
            match val {
                WValue::LtW { .. } => emit_wlt_width(dst, xop, yop, *width, &cmp_temps, out)?,
                WValue::GtW { .. } => emit_wgt_width(dst, xop, yop, *width, &cmp_temps, out)?,
                _ => unreachable!(),
            }
            *next -= n; // cmp_temps are net-zero → free them
            free_temp(&y_body, ymark, next, out);
            free_temp(&x_body, xmark, next, out);
        }
        WValue::EqZeroW { x, width } => {
            // Width-aware is-zero: realize x into an operand, reduce its low
            // `width` bits into dst.bit0, then uncompute + free.
            let n = weqzero_ntemp(*width);
            let (xop, x_body, xmark) = operand_or_temp(x, carriers, work, next, peak, out)?;
            let eq_temps = alloc_work_n(work, next, peak, n)?;
            emit_weqzero_width(dst, xop, *width, &eq_temps, out)?;
            *next -= n; // eq_temps are net-zero → free them
            free_temp(&x_body, xmark, next, out);
        }
        WValue::AndBit { x, y } => {
            // dst.bit0 = x.bit0 ∧ y.bit0 via ONE Ccnot into the clean dst. Realize
            // x,y into (carrier or freed temp) operands, both distinct from dst;
            // then uncompute + free. No mask/one/else-temp (the Select-AND cost).
            //
            // TECHNIQUE 1 (guard/temp sequencing). A binary AND's concurrent-work peak
            // is `max(cost(first), 1 + cost(second))`: the first operand is realized
            // with nothing held, the second on top of the first's single held result
            // bit. So realize the COSTLIER operand FIRST and the cheaper one SECOND --
            // the cheaper leg's small is-zero/compare burst then lands on just one held
            // bit rather than forcing the costly leg's burst to sit atop a held bit.
            // This is a pure reordering of a commutative AND (same gates, same net-
            // zero) and never increases the peak; it strictly helps a lop-sided pair.
            let (first, second) = if factor_internal_cost(y) > factor_internal_cost(x) {
                (y.as_ref(), x.as_ref())
            } else {
                (x.as_ref(), y.as_ref())
            };
            let (fop, f_body, fmark) = operand_or_temp(first, carriers, work, next, peak, out)?;
            let (sop, s_body, smark) = operand_or_temp(second, carriers, work, next, peak, out)?;
            check_three_distinct("AndBit Ccnot", dst, fop, sop)?;
            out.push(Instr::Ccnot {
                rd: dst,
                rs: fop,
                rt: sop,
            });
            free_temp(&s_body, smark, next, out);
            free_temp(&f_body, fmark, next, out);
        }
        WValue::Masked { guard, val } => {
            // dst ^= val & broadcast(guard.bit0). Realize guard into a FRESH temp
            // (it is broadcast in place to a 0x00/0xFF byte mask), realize val into a
            // carrier/freed temp, then `dst ^= val & mask` (one Ccnot). Uncompute the
            // broadcast + free. Only ONE scratch mask word beyond val -- cheaper than
            // Select(guard,val,0). `guard` must be bit-0-only (a predicate); the
            // broadcast `~m; m+1` = `0 - guard.bit0` assumes higher bits are 0.
            let gmark = *next;
            let mask = alloc_work(work, next, peak)?; // fresh mask word (clean 0)
            let g_body = realize_wvalue(guard, mask, carriers, work, next, peak, out)?;
            // broadcast mask.bit0 → 0x00/0xFF
            out.push(Instr::Xori {
                rd: mask,
                imm: 0xFF,
            });
            out.push(Instr::Addi { rd: mask, imm: 1 });
            let (vop, v_body, vmark) = operand_or_temp(val, carriers, work, next, peak, out)?;
            check_three_distinct("Masked Ccnot", dst, vop, mask)?;
            out.push(Instr::Ccnot {
                rd: dst,
                rs: vop,
                rt: mask,
            }); // dst ^= val & mask
            free_temp(&v_body, vmark, next, out);
            // unbroadcast mask, then uncompute the guard, then free the mask word.
            out.push(Instr::Subi { rd: mask, imm: 1 });
            out.push(Instr::Xori {
                rd: mask,
                imm: 0xFF,
            });
            out.extend(invert_body(&g_body));
            *next = gmark;
        }
        WValue::Select {
            guard,
            then_,
            else_,
        } => {
            // WSelect needs dst ∉ {then_w, else_w}; a bare-carrier then/else is
            // used directly. mask/one are fresh temps (freed after).
            let (gop, g_body, gmark) = operand_or_temp(guard, carriers, work, next, peak, out)?;
            let (top, t_body, tmark) = operand_or_temp(then_, carriers, work, next, peak, out)?;
            let (eop, e_body, emark) = operand_or_temp(else_, carriers, work, next, peak, out)?;
            let mask = alloc_work(work, next, peak)?;
            let one = alloc_work(work, next, peak)?;
            emit_wselect(dst, gop, top, eop, mask, one, out)?;
            *next -= 2; // mask, one net-zero → free
            free_temp(&e_body, emark, next, out);
            free_temp(&t_body, tmark, next, out);
            free_temp(&g_body, gmark, next, out);
        }
    }
    Ok(out[start..].to_vec())
}

/// Estimate the CONCURRENT internal scratch a sub-value needs when realized into a
/// temp -- used only to ORDER the two legs of an `AndBit` (Technique 1: realize the
/// costlier leg first). A rough upper bound is sufficient; it never affects
/// correctness, only the schedule. A bare carrier/const needs 0; an is-zero/compare
/// needs its macro's temp count; a compound value takes 1 (its value temp) plus its
/// costliest child.
fn factor_internal_cost(v: &WValue) -> usize {
    match v {
        WValue::Var(_) | WValue::Const(_) => 0,
        WValue::EqZeroW { width, .. } => weqzero_ntemp(*width),
        WValue::LtW { width, .. } | WValue::GtW { width, .. } => wlt_ntemp(*width),
        WValue::Lt(..) | WValue::Gt(..) => WLT_NTEMP,
        // notp(pred) = Xor(pred, Const(1)); its cost is the predicate's.
        WValue::Xor(x, y) => factor_internal_cost(x).max(factor_internal_cost(y)),
        WValue::AndBit { x, y } => 2 + factor_internal_cost(x).max(factor_internal_cost(y)),
        WValue::Add(x, y) | WValue::Sub(x, y) => {
            1 + factor_internal_cost(x).max(factor_internal_cost(y))
        }
        WValue::Rotl(x, _) | WValue::Rotr(x, _) => factor_internal_cost(x),
        WValue::Masked { guard, val } => {
            1 + factor_internal_cost(guard).max(factor_internal_cost(val))
        }
        WValue::Select {
            guard,
            then_,
            else_,
        } => {
            2 + factor_internal_cost(guard)
                .max(factor_internal_cost(then_))
                .max(factor_internal_cost(else_))
        }
    }
}

/// Resolve a carrier-index `WValue::Var(i)` to its operand.
fn resolve_var(i: usize, carriers: &[u8]) -> Result<u8, SynthError> {
    carriers
        .get(i)
        .copied()
        .ok_or_else(|| SynthError::MalformedSpec {
            detail: format!("WValue::Var({i}) out of carrier range ({})", carriers.len()),
        })
}

/// Yield an OPERAND for a sub-value, plus its (compute-body, saved-mark) for
/// uncompute+free. A bare `WValue::Var(i)` returns the carrier operand directly
/// with an EMPTY body (nothing to uncompute, mark unchanged) -- the carrier is
/// read-only and preserved. Any other shape is realized into a freshly-allocated
/// SCRATCH temp; the returned body/mark let the caller uncompute it and restore
/// `next` (freeing the temp for reuse), keeping concurrent temp usage minimal.
fn operand_or_temp(
    val: &WValue,
    carriers: &[u8],
    work: &[u8],
    next: &mut usize,
    peak: &mut usize,
    out: &mut Vec<Instr>,
) -> Result<(u8, Vec<Instr>, usize), SynthError> {
    if let WValue::Var(i) = val {
        return Ok((resolve_var(*i, carriers)?, Vec::new(), *next));
    }
    let mark = *next;
    let t = alloc_work(work, next, peak)?;
    let body = realize_wvalue(val, t, carriers, work, next, peak, out)?;
    Ok((t, body, mark))
}

/// Uncompute a temp's compute body (net-zero) and restore the allocation mark,
/// freeing the temp (and its nested scratch) for reuse. An empty body (bare
/// carrier) is a no-op. `mark` is where `next` stood BEFORE the temp was alloc'd.
fn free_temp(body: &[Instr], mark: usize, next: &mut usize, out: &mut Vec<Instr>) {
    if body.is_empty() {
        return; // bare carrier -- nothing allocated, nothing to uncompute
    }
    out.extend(invert_body(body));
    *next = mark;
}

/// Allocate one clean work word, advancing `next` and bumping the concurrent-use
/// high-water mark `peak` (tracked because temps are FREED -- the true peak is the
/// max concurrent, not the total consumed).
fn alloc_work(work: &[u8], next: &mut usize, peak: &mut usize) -> Result<u8, SynthError> {
    let w = *work.get(*next).ok_or_else(|| SynthError::MalformedSpec {
        detail: format!(
            "work pool exhausted: needed word #{}, have {}",
            *next + 1,
            work.len()
        ),
    })?;
    *next += 1;
    *peak = (*peak).max(*next);
    Ok(w)
}

/// Allocate `n` clean work words (a contiguous slice), advancing the mark and
/// bumping `peak`.
fn alloc_work_n(
    work: &[u8],
    next: &mut usize,
    peak: &mut usize,
    n: usize,
) -> Result<Vec<u8>, SynthError> {
    if *next + n > work.len() {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "work pool exhausted: needed {n} words at #{}, have {}",
                *next,
                work.len()
            ),
        });
    }
    let slice = work[*next..*next + n].to_vec();
    *next += n;
    *peak = (*peak).max(*next);
    Ok(slice)
}

/// The inverse of one emitted instruction. Only the instructions [`realize_wvalue`]
/// and the macros emit are handled -- every one is either self-inverse or has an
/// explicit paired inverse. A gate outside that set is a bug (unreachable).
fn invert_instr(ins: Instr) -> Instr {
    match ins {
        // self-inverse
        Instr::Xor { .. }
        | Instr::Xori { .. }
        | Instr::Ccnot { .. }
        | Instr::Cswap { .. }
        | Instr::Exch { .. }
        | Instr::Xchg { .. } => ins,
        // paired inverses
        Instr::Add { rd, rs } => Instr::Sub { rd, rs },
        Instr::Sub { rd, rs } => Instr::Add { rd, rs },
        Instr::Addi { rd, imm } => Instr::Subi { rd, imm },
        Instr::Subi { rd, imm } => Instr::Addi { rd, imm },
        Instr::Rotli { rd, imm } => Instr::Rotri { rd, imm },
        Instr::Rotri { rd, imm } => Instr::Rotli { rd, imm },
        other => panic!("invert_instr: unexpected gate in a Bennett-clean body: {other:?}"),
    }
}

/// Reverse a body and invert each instruction -- the exact uncompute of a
/// compute list built only from [`realize_wvalue`]/macro gates.
fn invert_body(body: &[Instr]) -> Vec<Instr> {
    body.iter().rev().map(|&ins| invert_instr(ins)).collect()
}

/// Whether a top-level [`WValue`] realizes touching its destination ONLY via
/// XOR-accumulation (`Xor`/`Xori`/`Ccnot` into `dst`), never mutating it with
/// `Add`/`Sub`/`Rotl`/`Rotr`. Such values can be realized STRAIGHT into the clean
/// ancilla `O` (which starts at 0), skipping the value-temp -- keeping `O` a pure
/// XOR-accumulation, exactly the invariant the step-3 backward pass needs.
///
/// - `Var`/`Const` write `O` with a single `Xor`/`Xori` -- safe.
/// - `Lt`/`Gt`/`Select` realize ALL their operands into fresh temps (or bare
///   carriers) and touch `O`/`dst` only through `Ccnot`/`Xor` accumulation -- safe
///   regardless of operand shape.
/// - `Add`/`Sub`/`Rotl`/`Rotr` (and `Xor`, whose FIRST operand is realized into
///   `dst` and may itself be arithmetic) mutate `dst` non-XOR-linearly → they MUST
///   go through a value-temp then be XOR-ed into `O` (mixing raw `Add`/`Sub` into
///   `O` would break step-3 cancellation).
fn is_pure_xor_top(val: &WValue) -> bool {
    matches!(
        val,
        WValue::Var(_)
            | WValue::Const(_)
            | WValue::Lt(..)
            | WValue::Gt(..)
            | WValue::LtW { .. }
            | WValue::GtW { .. }
            | WValue::EqZeroW { .. }
            | WValue::AndBit { .. }
            | WValue::Masked { .. }
            | WValue::Select { .. }
    )
}

/// Emit one Bennett-clean "O_i ^= value" step. For a pure-XOR-top value, realize
/// directly into the clean ancilla `o` (no value-temp). Otherwise compute the
/// value into a clean work temp, XOR it into `o`, and uncompute the temp
/// (net-zero). Either way `o` is a pure XOR-accumulation so step 3 cancels it.
fn emit_clean_accumulate(
    o: u8,
    value: &WValue,
    carriers: &[u8],
    work: &[u8],
    peak: &mut usize,
    out: &mut Vec<Instr>,
) -> Result<(), SynthError> {
    // Each output is realized sequentially, so `next` restarts at 0 per output
    // (temps are reused across outputs).
    let mut next = 0usize;
    if is_pure_xor_top(value) {
        // Realize straight into the clean ancilla; nested temps are uncomputed by
        // realize_wvalue itself (net-zero). `o` keeps the XOR-accumulated value.
        realize_wvalue(value, o, carriers, work, &mut next, peak, out)?;
    } else {
        let vtmp = alloc_work(work, &mut next, peak)?;
        let compute = realize_wvalue(value, vtmp, carriers, work, &mut next, peak, out)?;
        check_two_distinct("Bennett-clean XOR into ancilla", o, vtmp)?;
        out.push(Instr::Xor { rd: o, rs: vtmp }); // O_i ^= value
        out.extend(invert_body(&compute)); // uncompute the value temp → 0
    }
    Ok(())
}

/// Word-level Bennett wrapper (W.1). See the module comment above.
///
/// `carrier_ops` are the `n` carrier word operands; `out_ancilla` the `n` clean
/// ancilla words that receive `fwd(x)` (must be ≥ the number of distinct output
/// slots referenced). `work_ops` is the shared clean scratch pool.
///
/// Returns the assemblable body, or `Err` if the specs violate a guard (carrier
/// row reading a non-carrier, illegal/duplicated/aliased operands, an ancilla
/// slot out of range, or the work pool being too small). Callers verify with the
/// symbolic oracle / whole-domain exec (this function does not claim correctness).
pub fn transcribe_word_bennett(
    fwd: &WForward,
    bwd: &WForward,
    carrier_ops: &[u8],
    out_ancilla: &[u8],
    work_ops: &[u8],
) -> Result<Vec<Instr>, SynthError> {
    transcribe_word_bennett_peak(fwd, bwd, carrier_ops, out_ancilla, work_ops).map(|(b, _)| b)
}

/// Like [`transcribe_word_bennett`] but also returns the PEAK concurrent work-word
/// count (the max scratch words live at once -- the number the widen-state / free-
/// operand budget must cover; reported at W.2 for gcd2).
pub fn transcribe_word_bennett_peak(
    fwd: &WForward,
    bwd: &WForward,
    carrier_ops: &[u8],
    out_ancilla: &[u8],
    work_ops: &[u8],
) -> Result<(Vec<Instr>, usize), SynthError> {
    transcribe_word_bennett_peak_impl(fwd, bwd, carrier_ops, out_ancilla, work_ops)
}

/// MEASUREMENT-ONLY entry -- DO NOT USE for execution / ROM emission. Identical to
/// [`transcribe_word_bennett_peak`] except the operand-legality cap is RELAXED
/// (thread-local, RAII-scoped) so indices ≥ 16 (the future widen-state scratch
/// slots and beyond) are permitted. This lets an over-budget body (e.g. word-level
/// gcd2, whose peak concurrent work far exceeds the 6 free machine work operands)
/// actually PLACE so its `body.len()` / peak can be COUNTED. The emitted body reads
/// operand indices ≥ 16 that `exec_instr` panics on -- it is a length/peak
/// measurement artifact, never a runnable program. The relaxation is scoped to this
/// call: production behavior of every other entry is unchanged.
///
/// MEASUREMENT-ONLY: gated behind the `measurement` feature.
#[cfg(feature = "measurement")]
pub fn transcribe_word_bennett_peak_measure(
    fwd: &WForward,
    bwd: &WForward,
    carrier_ops: &[u8],
    out_ancilla: &[u8],
    work_ops: &[u8],
) -> Result<(Vec<Instr>, usize), SynthError> {
    /// Relaxed operand cap for peak measurement: generous headroom (regs 0..7,
    /// scratch 10..MEASURE_OPERAND_CAP) that still rejects the decoder-reserved
    /// s0/s1 (8/9), so the HONEST peak is measured even if it far exceeds the
    /// +4 (=18-operand) widen-state target; the caller reports whether it fits 18.
    const MEASURE_OPERAND_CAP: usize = 200;
    // The guard restores the strict cap on every exit path (including `?`
    // early returns).
    let _guard = RelaxedOperandCap::new(MEASURE_OPERAND_CAP);
    transcribe_word_bennett_peak_impl(fwd, bwd, carrier_ops, out_ancilla, work_ops)
}

/// Shared implementation of the word-level Bennett wrapper. Operand legality is
/// enforced by `check_operand_legal`, which consults the thread-local operand cap
/// (strict `NUM_OPERAND_LOCS` in production; raised only on the measurement path).
fn transcribe_word_bennett_peak_impl(
    fwd: &WForward,
    bwd: &WForward,
    carrier_ops: &[u8],
    out_ancilla: &[u8],
    work_ops: &[u8],
) -> Result<(Vec<Instr>, usize), SynthError> {
    let n = carrier_ops.len();
    // ── Operand-legality + disjointness guards (mirror transcribe_bennett) ──
    for &c in carrier_ops {
        check_operand_legal("carrier", c)?;
    }
    for &a in out_ancilla {
        check_operand_legal("ancilla", a)?;
    }
    for &w in work_ops {
        check_operand_legal("work", w)?;
    }
    // No dup within each bank; carriers/ancilla/work pairwise disjoint.
    let mut seen: Vec<u8> = Vec::new();
    for (what, bank) in [
        ("carrier", carrier_ops),
        ("ancilla", out_ancilla),
        ("work", work_ops),
    ] {
        for &op in bank {
            if seen.contains(&op) {
                return Err(SynthError::MalformedSpec {
                    detail: format!("{what} operand {op} duplicates another carrier/ancilla/work"),
                });
            }
            seen.push(op);
        }
    }
    // carrier-read + slot validation: every Var read names a real carrier;
    // every output slot is in range and written at most once. Returns the ordered
    // set of ACTIVE slots (those the permutation actually writes).
    let check_slots = |wf: &WForward, which: &str| -> Result<Vec<usize>, SynthError> {
        let mut used: Vec<usize> = Vec::new();
        for (slot, val) in &wf.outputs {
            if *slot >= n {
                return Err(SynthError::MalformedSpec {
                    detail: format!("{which} output slot {slot} ≥ carrier count {n}"),
                });
            }
            if used.contains(slot) {
                return Err(SynthError::MalformedSpec {
                    detail: format!("{which} output slot {slot} written twice"),
                });
            }
            used.push(*slot);
            check_wvalue_vars(val, n, which)?;
        }
        Ok(used)
    };
    let mut fwd_slots = check_slots(fwd, "fwd")?;
    let mut bwd_slots = check_slots(bwd, "bwd")?;
    // fwd and bwd must touch the SAME carriers: an unlisted carrier is a pure
    // identity pass-through (held in place -- no ancilla, no swap). If a carrier is
    // active in one direction only, the swap/uncompute would desynchronize.
    fwd_slots.sort_unstable();
    bwd_slots.sort_unstable();
    if fwd_slots != bwd_slots {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "fwd active slots {fwd_slots:?} ≠ bwd active slots {bwd_slots:?}; \
                 fwd and bwd must write the same carriers"
            ),
        });
    }
    let active = fwd_slots; // sorted, deduped, identical to bwd
                            // ONE ancilla per ACTIVE carrier, indexed by position in the sorted active set
                            // (`out_ancilla[k]` is the clean ancilla for carrier `active[k]`). Identity
                            // carriers consume no ancilla. This is the free-operand saving that lets a
                            // permutation with many preserved guards (e.g. gcd2) fit the operand budget.
    if out_ancilla.len() < active.len() {
        return Err(SynthError::MalformedSpec {
            detail: format!(
                "need ≥ {} ancilla (one per active carrier {active:?}), have {}",
                active.len(),
                out_ancilla.len()
            ),
        });
    }
    // slot → ancilla operand.
    let ancilla_of = |slot: usize| -> u8 {
        let k = active.iter().position(|&s| s == slot).unwrap();
        out_ancilla[k]
    };
    // A carrier↔ancilla swap (for ACTIVE carriers only) needs at least one side to
    // be a register (Exch/Xchg).
    for &i in &active {
        swap_operands(carrier_ops[i], ancilla_of(i))?;
    }

    let mut out = Vec::new();
    let mut peak = 0usize;

    // Step 1: O ^= fwd(x)  (each active slot's clean ancilla XOR-accumulates the
    // forward value; UNLISTED carriers are identity -- untouched, held in place).
    for (slot, val) in &fwd.outputs {
        emit_clean_accumulate(
            ancilla_of(*slot),
            val,
            carrier_ops,
            work_ops,
            &mut peak,
            &mut out,
        )?;
    }
    // Step 2: swap ACTIVE carriers ↔ ancilla, per pair. Identity carriers stay put
    // (still hold their inputs), so bwd can read them in step 3.
    for &i in &active {
        out.push(swap_operands(carrier_ops[i], ancilla_of(i))?);
    }
    // Step 3: O ^= bwd(y).  Active carriers now hold y = fwd(x); identity carriers
    // still hold their inputs. bwd(y, identity-inputs) = x is XORed into O (which
    // holds x), zeroing every ancilla → net-zero.
    for (slot, val) in &bwd.outputs {
        emit_clean_accumulate(
            ancilla_of(*slot),
            val,
            carrier_ops,
            work_ops,
            &mut peak,
            &mut out,
        )?;
    }
    Ok((out, peak))
}

/// Reject a [`WValue`] that reads a carrier index ≥ `n` (a permutation output
/// must be a function of the carriers alone).
fn check_wvalue_vars(val: &WValue, n: usize, which: &str) -> Result<(), SynthError> {
    match val {
        WValue::Var(i) => {
            if *i >= n {
                return Err(SynthError::MalformedSpec {
                    detail: format!("{which} reads carrier index {i} ≥ carrier count {n}"),
                });
            }
            Ok(())
        }
        WValue::Const(_) => Ok(()),
        WValue::Rotl(x, _) | WValue::Rotr(x, _) => check_wvalue_vars(x, n, which),
        WValue::Add(x, y)
        | WValue::Sub(x, y)
        | WValue::Xor(x, y)
        | WValue::Lt(x, y)
        | WValue::Gt(x, y) => {
            check_wvalue_vars(x, n, which)?;
            check_wvalue_vars(y, n, which)
        }
        WValue::LtW { x, y, .. } | WValue::GtW { x, y, .. } => {
            check_wvalue_vars(x, n, which)?;
            check_wvalue_vars(y, n, which)
        }
        WValue::EqZeroW { x, .. } => check_wvalue_vars(x, n, which),
        WValue::AndBit { x, y } => {
            check_wvalue_vars(x, n, which)?;
            check_wvalue_vars(y, n, which)
        }
        WValue::Masked { guard, val } => {
            check_wvalue_vars(guard, n, which)?;
            check_wvalue_vars(val, n, which)
        }
        WValue::Select {
            guard,
            then_,
            else_,
        } => {
            check_wvalue_vars(guard, n, which)?;
            check_wvalue_vars(then_, n, which)?;
            check_wvalue_vars(else_, n, which)
        }
    }
}
