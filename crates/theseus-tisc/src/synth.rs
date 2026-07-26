//! A-6.0 — the transcriber core: deterministic `RegTransfer` → `Instr` gates
//! for the IN-PLACE, ANCILLA-FREE structured class.
//!
//! This is the automation of the hand-written transcriptions in the keystone
//! POC (`tests/bvexpr_synth_poc.rs`). It is **deterministic transcription**, a
//! direct structural walk of each register row's `BvExpr` tree into ISA gates —
//! NOT search. There is no enumeration, no beam, no brute force.
//!
//! # The in-place ancilla-free class
//!
//! Each register row of the spec is one of:
//! - **identity**: `Var(r, b)` at every bit `b` — the register is untouched →
//!   emit no gate.
//! - **affine XOR of whole registers**: `x_new = Var(x, ·) ⊕ Σ Var(y_i, ·)`,
//!   matching bit positions (bit `b` of the target is bit `b` of each source) →
//!   one `Xor { rd: x, rs: y_i }` per source term.
//! - **constant flip**: `x_new = Var(x, ·) ⊕ const` (some bits negated) →
//!   one `Xori { rd: x, imm }` with `imm` the flip mask.
//! - **byte-parallel Toffoli**: `x_new = Var(x, ·) ⊕ (Var(y, ·) ∧ Var(z, ·))` →
//!   one `Ccnot { rd: x, rs: y, rt: z }`.
//! - a **bit-0-controlled swap** across two registers (`Mux(Var(c,0), …)`) →
//!   one `Cswap { rd: c, rs: u, rt: v }`.
//!
//! Anything else — a row that is not `old ⊕ f(others)` in the ISA basis, or a
//! set of rows that cannot be ordered so every read happens before the
//! corresponding write — is **rejected** with a [`SynthError`]. Those specs are
//! A-6.1's Bennett job (zero-init ancilla + backward uncompute), not this
//! slice's.
//!
//! # Verification
//!
//! Callers verify the emission with the existing symbolic oracle:
//! `equiv(symbolic_exec(&transcribe_inplace(spec)?), spec, BITS, &vars)`. The
//! transcriber never claims correctness itself — the BDD oracle is the acceptance
//! gate (it has teeth: a wrong emission is rejected).

use crate::asm::Instr;
use crate::bveq::{BvExpr, RegTransfer, BITS};
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
    /// `x ^= Σ Var(y_i,·) ⊕ const ⊕ (Var(rs,·) ∧ Var(rt,·))` — the combined
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
    Cswap {
        ctrl: usize,
        u: usize,
        v: usize,
    },
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
    /// ordering against itself — we only list the *other* registers it reads.
    fn reads_others(&self) -> Vec<usize> {
        match self {
            Row::Identity => vec![],
            Row::Affine { xor_sources, .. } => xor_sources.clone(),
            Row::AffineCcnot {
                xor_sources, rs, rt, ..
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
/// `equiv(symbolic_exec(&result)?, spec, BITS, &operand_regs) == Ok(())` — but
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
                    detail: format!(
                        "bit {b} is not `Var({r},{b}) ⊕ f`; got {e:?}"
                    ),
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
        // MINOR-1: const is a PARITY fold, not a bool — `x ⊕ 1 ⊕ 1` is identity,
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
                                "bit {b} has more than one AND term (needs ≥2 Ccnots — Bennett)"
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
    // every bit to match (byte-parallel uniformity — one gate does all 8 bits).
    let xor_sources = classes[0].sources.clone();
    let and_term = classes[0].and_term;
    for (b, c) in classes.iter().enumerate() {
        if c.sources != xor_sources {
            return Err(SynthError::RowNotInPlace {
                reg: r,
                detail: format!(
                    "affine sources differ across bits (bit 0: {xor_sources:?}, bit {b}: {:?}) \
                     — not a whole-register byte-parallel op",
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
    // is still fine — but it is TWO gate kinds on one target. The POC's
    // `affine_nonlinear_mix` is exactly `x ^= w ⊕ (y∧z)` → [Xor, Ccnot]. So we
    // support the combination on a single target (Ccnot after the Xor/Xori,
    // both XOR-accumulate onto the target — order among them is immaterial).
    if let Some((y, z)) = and_term {
        // MINOR: the emitted `Ccnot { rd: r, rs: y, rt: z }` must have pairwise
        // DISTINCT operands (ISA-illegal / non-bijective otherwise — asm::encode
        // rejects it). The AND term touching the target (`y == r` or `z == r`, the
        // spec `x ^= x∧z` CE) is non-in-place anyway; `y == z` (`x ^= y∧y = y`)
        // is degenerate. Reject rather than emit an unassemblable body.
        if y == z || y == r || z == r {
            return Err(SynthError::RowNotInPlace {
                reg: r,
                detail: format!(
                    "Ccnot operands not pairwise-distinct (rd={r}, rs={y}, rt={z}); \
                     the AND term touches the target or repeats an operand — non-bijective"
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
/// scheduling — each output row is emitted independently, reading only sources
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

    // ── MAJOR-2 / MINOR-3: validate the DESTINATION operand map. Only rows
    // that actually emit gates matter (a spec-identity row whose dst self-maps
    // to its own src contributes nothing — MINOR-4). For every row that will
    // emit, the dst operand must be a legal operand and pairwise-distinct from
    // every OTHER emitting row's dst (aliased fresh targets clobber each other).
    let mut used_dsts: Vec<u8> = Vec::new();
    for (i, row) in spec.regs.iter().enumerate() {
        let dst = dst_ops[i];
        // MINOR-4: the skip fires ONLY on a genuine IDENTITY row that self-maps
        // (dst == src for its own reg). Any NON-identity row that happens to
        // alias a source is a caller contract violation, not a no-op — never
        // silently drop its gates.
        if is_identity_row(row, i) && dst == src_ops[i] {
            continue;
        }
        check_operand_legal("dst", dst)?;
        if used_dsts.contains(&dst) {
            return Err(SynthError::MalformedSpec {
                detail: format!(
                    "dst operand {dst} used by two rows — fresh targets must be distinct"
                ),
            });
        }
        used_dsts.push(dst);
    }

    let mut out = Vec::new();
    for (i, row) in spec.regs.iter().enumerate() {
        let dst = dst_ops[i];
        // Recognize this row as a byte-parallel `f(vars)` in the ISA basis.
        // NOTE: unlike the in-place case, there is NO self-term to peel — the
        // fresh target already holds 0 and we XOR the whole function onto it.
        let (xor_sources, const_mask, and_term) = recognize_into_row(row, i)?;
        // MINOR-4: only skip genuine identity self-maps (validated above). If we
        // reach here with a non-identity row, every gate is emitted — no source
        // may alias the dst (that would be an ISA-illegal self-XOR / self-Ccnot).
        let is_id_selfmap = is_identity_row(row, i) && dst == src_ops[i];
        if is_id_selfmap {
            continue;
        }
        for &y in &xor_sources {
            let rs = src_ops[y];
            // A copy where dst == src on a non-identity row is an ISA-illegal
            // self-XOR AND silently drops a needed term (MINOR-4 CE:
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
            // non-bijective otherwise — asm::encode rejects it).
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
/// and ≥16 would panic `operand_get`). Returns `Err(MalformedSpec)` — a
/// verification-time reject, not a ROM-time panic (MINOR-3).
fn check_operand_legal(what: &str, op: u8) -> Result<(), SynthError> {
    if (op as usize) >= NUM_OPERAND_LOCS {
        return Err(SynthError::MalformedSpec {
            detail: format!("{what} operand {op} is out of range (≥{NUM_OPERAND_LOCS})"),
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
        // MINOR-1: parity fold — even count of Const(true) is no flip.
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
                                "bit {b} has more than one AND term (needs ≥2 Ccnots — \
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
                     — not a whole-register byte-parallel op",
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

// ── A-6.1: the Bennett wrapper (general permutation via ancilla) ─────

/// Textbook Bennett reversible-in-place-permutation: realize a permutation
/// `P` (given as forward spec `fwd` and its inverse `bwd = P⁻¹`) on a set of
/// CARRIER operands, using a bank of ZERO-INIT ANCILLA operands that returns to
/// zero by construction.
///
/// Given carriers holding `x` and ancilla `O = 0`:
/// 1. `O ^= P(x)`   — [`transcribe_into`]`(fwd, dst=ancilla, src=carrier)`.
///    Now `O = P(x)`, carriers still `x`.
/// 2. swap carriers ↔ O, per carrier↔ancilla pair (`Exch` for reg↔reg,
///    `Xchg` when the ancilla is a scratch word). Now carriers `= P(x) = y`,
///    `O = x`.
/// 3. `O ^= P⁻¹(y)` — [`transcribe_into`]`(bwd, dst=ancilla, src=carrier)`.
///    Since carriers now hold `y` and `bwd = P⁻¹`, this XORs `P⁻¹(y) = x` onto
///    `O(=x)` → `O = 0`. Net-zero ancilla, guaranteed by construction.
///
/// Result: carriers hold `P(x)`, ancilla back to `0`. This is the ~2× body the
/// A-1 finding predicted — CORRECT by construction; length is whatever it is.
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
    // MINOR-3: every carrier and every USED ancilla operand must be a legal
    // operand (reject reserved s0/s1 = 8/9 and out-of-range ≥16, which would
    // otherwise panic `operand_get` at exec time rather than fail verification).
    for &c in &carrier_ops[..n] {
        check_operand_legal("carrier", c)?;
    }
    for &a in &ancilla_ops[..n] {
        check_operand_legal("ancilla", a)?;
    }

    // MAJOR-2: the operand maps must be internally disjoint with no dups:
    //   * no carrier repeated; no USED ancilla word repeated (aliased ancilla
    //     `[3,3]` defeats "ancilla returned to 0" teeth — CE);
    //   * carriers and used ancilla disjoint (Bennett requires fresh scratch).
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
    for c in &carrier_ops[..n] {
        if ancilla_ops[..n].contains(c) {
            return Err(SynthError::MalformedSpec {
                detail: format!("carrier operand {c} aliases an ancilla word (must be disjoint)"),
            });
        }
    }

    // MAJOR-1: a carrier row (0..n) may only READ carriers. `Var(y)` with
    // `y ≥ n` reads a register OUTSIDE the carrier set; the src_map padding
    // below wired every such read to a bogus operand (operand 0), silently
    // producing a wrong body + dirty ancilla. A permutation spec on `n`
    // carriers must be a function of those `n` carriers alone — reject.
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

    // Build NUM_REGS-length dst/src maps. For carrier rows 0..n, src = carrier,
    // dst = ancilla. For rows n..NUM_REGS (which must be spec-identity), point
    // both at the carrier operand so any (identity) emission is a self-noop we
    // never actually emit — but to be safe reject non-identity there.
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
    let mut src_map = vec![0u8; NUM_REGS];
    let mut dst_map = vec![0u8; NUM_REGS];
    for r in 0..NUM_REGS {
        if r < n {
            src_map[r] = carrier_ops[r];
            dst_map[r] = ancilla_ops[r];
        } else {
            // Non-carrier rows are identity; give them a harmless self-map.
            src_map[r] = carrier_ops.get(r).copied().unwrap_or(0);
            dst_map[r] = carrier_ops.get(r).copied().unwrap_or(0);
        }
    }

    let mut out = Vec::new();

    // Step 1: O ^= P(x)   (dst = ancilla, src = carriers = x).
    out.extend(transcribe_into(fwd, &dst_map, &src_map)?);

    // Step 2: swap carriers ↔ ancilla, per pair. Use Exch for reg↔reg, Xchg
    // (reg↔scratch) when exactly one side is a scratch operand.
    for i in 0..n {
        out.push(swap_operands(carrier_ops[i], ancilla_ops[i])?);
    }

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
// basis — at most one degree-2 AND term per output bit, byte-parallel. A general
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
/// **general ANF class** — XOR of arbitrary-degree AND-monomials, per bit.
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
/// `Var(y0,c0) ∧ … ∧ Var(y_{d-1},c_{d-1})` of any degree `d`; each `Var` reads
/// its OWN bit `c_k` (ANF monomials mix bit positions). Realization over the
/// 2-input-Ccnot ISA, net-zero:
///   - `Const(true)`  → `Xori { dst, 1<<b }`.
///   - degree 1       → align src bit `c0` to bit `b` (rotate, paired-undone),
///                      load `mask = 1<<b`, `Ccnot dst, src_aligned, mask`
///                      (dst bit b ^= src bit c0), unload mask, un-rotate.
///   - degree d ≥ 2   → align all factors to bit `b`; AND-tree:
///                        `Ccnot temp0, f0, f1`         (temp0 = f0∧f1)
///                        `Ccnot temp1, temp0, f2`      (temp1 = f0∧f1∧f2) …
///                      then `mask = 1<<b`, `Ccnot dst, temp_last, mask`
///                      (dst bit b ^= product bit b only), unload mask, and run
///                      the AND-tree in REVERSE to clear every temp, then
///                      un-rotate the sources. Net-zero by construction.
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
            out.push(Instr::Xori { rd: dst, imm: 1u8 << b });
            return Ok(());
        }
        BvExpr::Const(false) => return Ok(()), // XOR identity — no-op
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
        out.push(Instr::Xori { rd: helpers.mask, imm: 1u8 << b });
        out.push(Instr::Ccnot { rd: dst, rs: s, rt: helpers.mask });
        out.push(Instr::Xori { rd: helpers.mask, imm: 1u8 << b });
        rotate_by(s, -sh, out);
        return Ok(());
    }

    // degree ≥ 2: build the product with an AND-LADDER of `degree-1` clean
    // single-bit accumulators, uncomputed for net-zero. Every factor is aligned
    // to bit `b` JUST FOR its Ccnot and un-aligned right after, so two factors
    // that share a source operand (multiplicity > 1) are never rotated
    // simultaneously — the emitter is aliasing-safe without extra copy words.
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
        out.push(Instr::Ccnot { rd: acc, rs: x, rt: s });
        rotate_by(s, -sh, out);
    };

    // FORWARD LADDER (compute):
    //   temp0 := 1<<b ∧ f0    (clean single bit b = f0's bit)  — mask seeds a
    //   temp_{k-1} := temp_{k-2} ∧ f_k                          clean accumulator
    // Because temp0 is clean (0 outside bit b), every downstream AND stays clean,
    // so the final XOR onto dst needs no mask.
    out.push(Instr::Xori { rd: helpers.mask, imm: 1u8 << b }); // mask = 1<<b
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
    out.push(Instr::Xori { rd: helpers.mask, imm: 1u8 << b }); // mask back to 0
    Ok(())
}

/// Collect every register operand index READ by a `BvExpr` (walks all node
/// kinds; used by the Bennett carrier-read validation, MAJOR-1).
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
    let mut src_map = vec![0u8; NUM_REGS];
    let mut dst_map = vec![0u8; NUM_REGS];
    for r in 0..NUM_REGS {
        if r < n {
            src_map[r] = carrier_ops[r];
            dst_map[r] = out_ancilla[r];
        } else {
            src_map[r] = carrier_ops.get(r).copied().unwrap_or(0);
            dst_map[r] = carrier_ops.get(r).copied().unwrap_or(0);
        }
    }

    let mut out = Vec::new();
    // 1. O ^= P(x).
    out.extend(transcribe_into_general(fwd, &dst_map, &src_map, helpers)?);
    // 2. swap carriers ↔ output ancilla.
    for i in 0..n {
        out.push(swap_operands(carrier_ops[i], out_ancilla[i])?);
    }
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
/// is EXACT and DETERMINISTIC — no search, no enumeration of instruction
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
/// The result is ANF — length is whatever the ANF is (not minimised); A-6.4 /
/// widen-PC funds the resulting body size. Callers verify with the symbolic
/// oracle (all-register operands) and/or the exec/net-zero path.
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
            // Require Var(y,b) ∧ Var(z,b) — matching bit, two distinct-or-not regs.
            if let (
                BvExpr::Var { operand: y, bit: yb },
                BvExpr::Var { operand: z, bit: zb },
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
/// in-place: its new value is not `old ⊕ …`).
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
    if self_count % 2 == 0 {
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
            BvExpr::Var { operand: c, bit: cb },
            BvExpr::Var { operand: t, bit: _tb },
            BvExpr::Var { operand: el, bit: _eb },
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
/// row *reads* while another row *overwrites* it — a read-after-write where the
/// read must precede the write, but the write's own row might depend on the
/// reader too (a cycle ⇒ Bennett). We do a Kahn-style topological sort with the
/// edge "reader-before-writer": if row A reads register g and a *different* row
/// B writes g, then A must run before B. A cycle ⇒ `OrderingConflict`.
fn schedule(rows: &[Row]) -> Result<Vec<Row>, SynthError> {
    // writer_of[g] = index of the (unique) row that writes register g, if any.
    // A register written by two different rows is out of the simple in-place
    // class (only Cswap writes two regs, and it registers both indices to the
    // SAME row instance — but our `rows` vec has one entry per register, so a
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
    let mut queue: std::collections::VecDeque<usize> =
        (0..m).filter(|&i| indeg[i] == 0).collect();
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
