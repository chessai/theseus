//! Dual-rail lowering: typed netlist → phased cell netlist (ADR-0031).
//!
//! # Implemented lemmas (this slice)
//!
//! | node | lowering | cells |
//! |---|---|---|
//! | `Permute(×, σ)` | rail-pair rewiring | 0 |
//! | `Permute(+, σ)` | tag-line + payload rewiring (general sums) | 0 |
//! | `Prim` | verified template from the registry (identity / NOT / Fredkin / 2-CSWAP) | 0 or 1 |
//! | `Reg(n)` | `n` K0 buffer stages per bit (C2.1) | `n·width` |
//! | `Seq` / `Par` | composition / wiring | 0 |
//! | `Branch(f,g)` | CSWAP demux/merge with K1 Fredkin routing (ADR-0031) | varies |
//! | `Distrib`/`Factor` | tag-pair regrouping — pure rail reordering | 0 |
//! | `TraceLoop` | full unroll ≤ \|u\|+1 iterations (ADR-0026 C2.4) | varies |
//!
//! Every template is verified **per instance** against the prim's finite
//! `(forward)` semantics before use — exhaustive over the domain
//! (ADR-0031: "each per-instance checkable — exhaustive small").
//! Phase = depth mod P throughout (ADR-0026 C2.3).
//!
//! # Deferred (honest list, with owners)
//!
//! - TODO(B4.2): general CSWAP-network synthesis for prims beyond the
//!   built-in template table.
//! - Tick-exact balancing (C2.2) is *recorded, not repaired*:
//!   [`PhasedCellNetlist::tick_imbalances`] and `output_depths` skew are
//!   the balancer's worklist.

use core::fmt;
use std::collections::{BTreeMap, HashMap};

use theseus_core::{admit, enumerate_values, Iso, PrimDef, Type, TypeError, Value};

use crate::canonicalize::canonicalize;
use crate::ir::cell::{Cell, CellKind, LineId, PhasedCellNetlist, RailPair, PHASE_COUNT};
use crate::ir::typed::{flatten_layer, DistribDir, Monoid, NodeKind, TypedError, TypedNode};

/// Error from the dual-rail lowering.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LowerError {
    /// The source iso is ill-typed.
    Type(TypeError),
    /// The typed net is malformed.
    Typed(TypedError),
    /// Node vocabulary deferred to a later slice (see module docs).
    Deferred { node: &'static str, plan: &'static str },
    /// No dual-rail encoding for this type (`0`-typed nets are
    /// statically dead — `Absurd` pruning is a later pass).
    UnsupportedType(Type),
    /// The prim matched no verified cell template (general CSWAP-network
    /// synthesis, B4.2, deferred).
    UnsupportedPrim { name: String },
}

impl fmt::Display for LowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LowerError::Type(e) => write!(f, "ill-typed iso: {e:?}"),
            LowerError::Typed(e) => write!(f, "malformed typed net: {e}"),
            LowerError::Deferred { node, plan } => {
                write!(f, "{node} lowering deferred to a later slice ({plan})")
            }
            LowerError::UnsupportedType(ty) => {
                write!(f, "no dual-rail encoding for {ty}")
            }
            LowerError::UnsupportedPrim { name } => {
                write!(f, "prim {name:?} matched no verified cell template (B4.2 deferred)")
            }
        }
    }
}

impl From<TypeError> for LowerError {
    fn from(e: TypeError) -> Self {
        LowerError::Type(e)
    }
}

impl From<TypedError> for LowerError {
    fn from(e: TypedError) -> Self {
        LowerError::Typed(e)
    }
}

/// Number of rail pairs (logical bits) encoding a value of `ty`:
/// `1` is 0 pairs, `A + B` is `1 + max(width(A), width(B))` (1 tag pair
/// + shared payload), products concatenate.
///
/// The bit `1 + 1` is backward-compatible: `1 + max(0, 0) = 1` pair.
pub fn rail_width(ty: &Type) -> Result<usize, LowerError> {
    match ty {
        Type::One => Ok(0),
        Type::Sum(a, b) => {
            let wa = rail_width(a)?;
            let wb = rail_width(b)?;
            Ok(1 + wa.max(wb))
        }
        Type::Prod(a, b) => Ok(rail_width(a)? + rail_width(b)?),
        _ => Err(LowerError::UnsupportedType(ty.clone())),
    }
}

/// Flatten a value of an encodable type to its bit string, in rail-pair
/// order. Convention: `Left(·)` sets the tag pair TRUE (1-token),
/// `Right(·)` sets it FALSE (0-token); the payload occupies
/// `max(wA, wB)` pairs with the shorter arm padded by FALSE tokens.
///
/// For the bit `1 + 1`: `Left(Unit)` → `[true]`, `Right(Unit)` → `[false]`,
/// backward-compatible with the prior slice.
pub fn bits_of(value: &Value, ty: &Type) -> Result<Vec<bool>, LowerError> {
    match (ty, value) {
        (Type::One, Value::Unit) => Ok(vec![]),
        (Type::Sum(ta, tb), Value::Left(a)) => {
            let wa = rail_width(ta)?;
            let wb = rail_width(tb)?;
            let max_w = wa.max(wb);
            let mut bits = vec![true]; // tag = Left
            bits.extend(bits_of(a, ta)?);
            bits.extend(std::iter::repeat_n(false, max_w - wa)); // pad
            Ok(bits)
        }
        (Type::Sum(ta, tb), Value::Right(b)) => {
            let wa = rail_width(ta)?;
            let wb = rail_width(tb)?;
            let max_w = wa.max(wb);
            let mut bits = vec![false]; // tag = Right
            bits.extend(bits_of(b, tb)?);
            bits.extend(std::iter::repeat_n(false, max_w - wb)); // pad
            Ok(bits)
        }
        (Type::Prod(ta, tb), Value::Pair(a, b)) => {
            let mut bits = bits_of(a, ta)?;
            bits.extend(bits_of(b, tb)?);
            Ok(bits)
        }
        _ => Err(LowerError::UnsupportedType(ty.clone())),
    }
}

/// Rebuild a value of `ty` from its flattened bit string — the inverse
/// of [`bits_of`]. The tag bit selects the arm; only the first `wA`
/// (or `wB`) payload bits are decoded; padding bits are ignored.
///
/// # Panics
///
/// Panics if `bits.len() != rail_width(ty)` (a caller bug, not a data
/// condition).
pub fn value_from_bits(ty: &Type, bits: &[bool]) -> Result<Value, LowerError> {
    fn go(ty: &Type, bits: &mut &[bool]) -> Result<Value, LowerError> {
        match ty {
            Type::One => Ok(Value::Unit),
            Type::Sum(ta, tb) => {
                let wa = rail_width(ta)?;
                let wb = rail_width(tb)?;
                let max_w = wa.max(wb);
                let tag = bits[0];
                let payload = &bits[1..1 + max_w];
                *bits = &bits[1 + max_w..];
                if tag {
                    // Left arm: decode first wa bits, ignore padding
                    let mut sub = &payload[..wa];
                    let v = go(ta, &mut sub)?;
                    Ok(Value::left(v))
                } else {
                    // Right arm: decode first wb bits, ignore padding
                    let mut sub = &payload[..wb];
                    let v = go(tb, &mut sub)?;
                    Ok(Value::right(v))
                }
            }
            Type::Prod(a, b) => {
                let va = go(a, bits)?;
                let vb = go(b, bits)?;
                Ok(Value::pair(va, vb))
            }
            _ => Err(LowerError::UnsupportedType(ty.clone())),
        }
    }
    let w = rail_width(ty)?;
    assert_eq!(bits.len(), w, "bit string length mismatch for type");
    let mut slice = bits;
    let v = go(ty, &mut slice)?;
    assert!(slice.is_empty(), "bit string too long for type");
    Ok(v)
}

/// Build an admitted [`PrimDef`] from a bijective function on flattened
/// bits (both truth tables enumerated over the finite domain). This is
/// how demos, benches, and tests phrase "the Fredkin prim" without
/// hand-writing truth tables.
///
/// Errors if either boundary type has no dual-rail encoding.
///
/// # Panics
///
/// Panics if the resulting def fails [`admit`] — i.e. `f` is not a
/// bijection between the two types' bit strings (a caller bug).
pub fn prim_from_bits(
    name: &str,
    input_ty: Type,
    output_ty: Type,
    f: impl Fn(&[bool]) -> Vec<bool>,
) -> Result<PrimDef, LowerError> {
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in enumerate_values(&input_ty) {
        let out_bits = f(&bits_of(&v, &input_ty)?);
        let w = value_from_bits(&output_ty, &out_bits)?;
        forward.insert(v.clone(), w.clone());
        backward.insert(w, v);
    }
    let def = PrimDef {
        forward_name: name.into(),
        backward_name: format!("{name}_inv"),
        input_ty,
        output_ty,
        forward,
        backward,
    };
    assert_eq!(
        admit(&def),
        Ok(()),
        "prim_from_bits: {name:?} is not admissible (f not a bijection?)"
    );
    Ok(def)
}

/// A logical bit in flight: its rail pair and its arrival tick.
#[derive(Clone, Copy, Debug)]
struct BitAt {
    pair: RailPair,
    depth: u32,
}

impl BitAt {
    /// NOT: exchange the two token-line roles. Zero cells, zero ticks
    /// (ADR-0031 NOT-in-Permute).
    fn rail_swapped(self) -> BitAt {
        BitAt { pair: self.pair.swapped(), depth: self.depth }
    }
}

struct Builder {
    next_line: u32,
    cells: Vec<Cell>,
    /// Ancilla input pairs needed by conditional-NOT constructions.
    /// Each entry is (pair, initial_value).  These are appended to the
    /// netlist's primary inputs by `lower()`.
    ancilla_inputs: Vec<(RailPair, bool)>,
    /// Garbage output pairs produced by conditional-NOT constructions.
    /// Each entry is (pair, depth).  These are appended to the netlist's
    /// primary outputs by `lower()`.
    ancilla_outputs: Vec<(RailPair, u32)>,
}

impl Builder {
    fn fresh_pair(&mut self) -> RailPair {
        let pair = RailPair { line0: LineId(self.next_line), line1: LineId(self.next_line + 1) };
        self.next_line += 2;
        pair
    }

    /// Allocate an ancilla pair with a known initial value.  The pair
    /// is recorded so that `lower()` can add it to the netlist's primary
    /// inputs.
    fn alloc_ancilla(&mut self, value: bool) -> BitAt {
        let pair = self.fresh_pair();
        self.ancilla_inputs.push((pair, value));
        BitAt { pair, depth: 0 }
    }

    /// Record a garbage output pair produced by a conditional-NOT
    /// construction.  `lower()` adds it to the netlist's primary outputs.
    fn mark_garbage(&mut self, bit: BitAt) {
        self.ancilla_outputs.push((bit.pair, bit.depth));
    }

    /// Place one cell. It fires when its latest input arrives (C2.2
    /// equal-arrival is the deferred balancer's job; the imbalance stays
    /// visible via `tick_imbalances`), and its phase is depth mod P
    /// (C2.3).
    fn emit(&mut self, kind: CellKind, ins: &[BitAt]) -> Vec<BitAt> {
        debug_assert_eq!(ins.len(), kind.arity());
        let depth = ins.iter().map(|b| b.depth).max().expect("cell has inputs");
        let outputs: Vec<RailPair> = ins.iter().map(|_| self.fresh_pair()).collect();
        self.cells.push(Cell {
            kind,
            depth,
            phase: (depth % u32::from(PHASE_COUNT)) as u8,
            inputs: ins.iter().map(|b| b.pair).collect(),
            outputs: outputs.clone(),
        });
        outputs
            .into_iter()
            .map(|pair| BitAt { pair, depth: depth + 1 })
            .collect()
    }

    fn apply(&mut self, node: &TypedNode, ins: &[BitAt]) -> Result<Vec<BitAt>, LowerError> {
        match &node.kind {
            NodeKind::Seq(nodes) => {
                let mut cur = ins.to_vec();
                for n in nodes {
                    cur = self.apply(n, &cur)?;
                }
                Ok(cur)
            }

            NodeKind::Par(f, g) => {
                let wf = rail_width(&f.input)?;
                let (left, right) = ins.split_at(wf);
                let mut out = self.apply(f, left)?;
                out.extend(self.apply(g, right)?);
                Ok(out)
            }

            // Lemma (ADR-0031): Permute(×, σ) is rail-pair rewiring —
            // zero cells, zero ticks, both directions.
            NodeKind::Permute { monoid: Monoid::Prod, sigma } => {
                let layer = flatten_layer(Monoid::Prod, &node.input);
                let widths = layer
                    .iter()
                    .map(rail_width)
                    .collect::<Result<Vec<_>, _>>()?;
                let mut offsets = Vec::with_capacity(widths.len());
                let mut off = 0usize;
                for &w in &widths {
                    offsets.push(off);
                    off += w;
                }
                debug_assert_eq!(off, ins.len());
                let mut out = Vec::with_capacity(ins.len());
                for &src in sigma {
                    out.extend_from_slice(&ins[offsets[src]..offsets[src] + widths[src]]);
                }
                Ok(out)
            }

            // Lemma (ADR-0031): Permute(+, σ) on a binary sum acts on the
            // leading tag pair. For a 2-leaf layer: σ = id is passthrough,
            // σ = [1,0] is a tag rail swap (NOT), zero cells. The payload
            // pairs are unchanged (both arms share the same padded width).
            //
            // Multi-arm permutations that change the tree structure of the
            // sum (e.g. re-association with unequal arm widths) require
            // data-dependent payload rearrangement and are not yet
            // supported.
            NodeKind::Permute { monoid: Monoid::Sum, sigma } => {
                // Identity sigma: passthrough if the encoding layout
                // is the same on both sides (structurally equal types).
                let is_identity = sigma.iter().enumerate().all(|(i, &j)| i == j);
                if is_identity {
                    if node.input == node.output {
                        return Ok(ins.to_vec());
                    } else {
                        return Err(LowerError::UnsupportedType(node.input.clone()));
                    }
                }

                // Block-swap sigma on a binary Sum(A, B): tag rail swap,
                // payload unchanged.  SwapSum(A, B) always produces this
                // pattern: σ = [la..la+lb-1, 0..la-1] where la, lb are
                // the flattened leaf counts of A and B.
                if let Type::Sum(a, b) = &node.input {
                    let la = flatten_layer(Monoid::Sum, a).len();
                    let lb = flatten_layer(Monoid::Sum, b).len();
                    let expected: Vec<usize> = (la..la + lb).chain(0..la).collect();
                    if sigma == &expected {
                        // Tag rail swap; payload stays.
                        let mut out = Vec::with_capacity(ins.len());
                        out.push(ins[0].rail_swapped());
                        out.extend_from_slice(&ins[1..]);
                        return Ok(out);
                    }
                }

                Err(LowerError::UnsupportedType(node.input.clone()))
            }

            // Lemma (ADR-0031): Prim lowers to a CSWAP network from the
            // registry, verified per instance (B4.2 minimal form).
            NodeKind::Prim(def) => self.lower_prim(def, ins),

            // Lemma (ADR-0026 C2.1 / ADR-0031): Reg(n) is n K0 buffer
            // stages per bit.
            NodeKind::Reg(n) => Ok(ins
                .iter()
                .map(|&b| {
                    let mut cur = b;
                    for _ in 0..*n {
                        cur = self.emit(CellKind::K0, &[cur])[0];
                    }
                    cur
                })
                .collect()),

            // Lemma (ADR-0031): Branch(f, g) at type (A+B) ↔ (A'+B')
            // is controlled sequential application.  Both arms share the
            // payload and execute sequentially; each cell is promoted
            // (K0→K1, K1→K2) with the tag as an additional control so that
            // only the active arm modifies the data.  The inactive arm's
            // promoted cells pass through, preserving INV-TOKEN.
            //
            // Controlled-f is active when tag=true, controlled-g when
            // tag=false (i.e. NOT(tag) controls g's cells).
            NodeKind::Branch(f, g) => {
                self.lower_branch(f, g, ins)
            }

            // Lemma (ADR-0031): Distrib/Factor is pure rail regrouping —
            // zero cells. The tag pair moves between positions and the
            // payload rails concatenate differently, but no logic is
            // needed because both sides share the same rail pairs.
            //
            // Distrib: a × (b+c) → (a×b) + (a×c)
            //   Input  layout: [a-pairs (wa)] [tag] [inner-payload (max(wb,wc))]
            //   Output layout: [tag] [a-pairs (wa)] [inner-payload (max(wb,wc))]
            //   = move tag from position wa to position 0.
            //
            // Factor: (a×b) + (a×c) → a × (b+c)
            //   = the inverse: move tag from position 0 to position wa.
            NodeKind::Distrib(dir) => {
                // Extract the types. The product side is a × (b+c),
                // the sum side is (a×b) + (a×c).
                let (prod_side, _sum_side) = match dir {
                    DistribDir::Distrib => (&node.input, &node.output),
                    DistribDir::Factor => (&node.output, &node.input),
                };
                let Type::Prod(a, _bc) = prod_side else {
                    return Err(LowerError::UnsupportedType(prod_side.clone()));
                };
                let wa = rail_width(a)?;

                match dir {
                    DistribDir::Distrib => {
                        // Input: [a0..a(wa-1), tag, p0..p(max_w-1)]
                        // Output: [tag, a0..a(wa-1), p0..p(max_w-1)]
                        let mut out = Vec::with_capacity(ins.len());
                        out.push(ins[wa]); // tag moves to front
                        out.extend_from_slice(&ins[..wa]); // a-pairs
                        out.extend_from_slice(&ins[wa + 1..]); // inner payload
                        Ok(out)
                    }
                    DistribDir::Factor => {
                        // Input: [tag, p0..p(wa+max_w-1)]
                        // = [tag, a0..a(wa-1), inner0..inner(max_w-1)]
                        // Output: [a0..a(wa-1), tag, inner0..inner(max_w-1)]
                        let mut out = Vec::with_capacity(ins.len());
                        out.extend_from_slice(&ins[1..1 + wa]); // a-pairs
                        out.push(ins[0]); // tag moves after a
                        out.extend_from_slice(&ins[1 + wa..]); // inner payload
                        Ok(out)
                    }
                }
            }

            // TraceLoop unrolling (ADR-0026 C2.4): a total trace runs at
            // most |u|+1 body applications, so full unrolling is always
            // sound. N = |u| + 1 iterations.
            NodeKind::TraceLoop { carrier, body } => {
                self.lower_trace(carrier, body, ins)
            }
        }
    }

    /// The template registry, minimal B4.2 form: a fixed table of
    /// candidate CSWAP networks, each **verified against the prim's
    /// finite forward semantics** (exhaustive over the domain) before it
    /// is used. A prim that matches no template is rejected — never
    /// guessed at.
    fn lower_prim(&mut self, def: &PrimDef, ins: &[BitAt]) -> Result<Vec<BitAt>, LowerError> {
        let w_in = rail_width(&def.input_ty)?;
        let w_out = rail_width(&def.output_ty)?;
        debug_assert_eq!(ins.len(), w_in);

        // Wiring identity (covers reshapes packaged as prims): 0 cells.
        if w_in == w_out && prim_matches(def, |bits| bits.to_vec())? {
            return Ok(ins.to_vec());
        }
        // NOT: rail swap, 0 cells (ADR-0031 NOT-in-Permute).
        if w_in == 1 && w_out == 1 && prim_matches(def, |bits| vec![!bits[0]])? {
            return Ok(vec![ins[0].rail_swapped()]);
        }
        // Fredkin `(c, a, b)`: one K1.
        if w_in == 3 && w_out == 3 && prim_matches(def, |bits| CellKind::K1.eval(bits))? {
            return Ok(self.emit(CellKind::K1, ins));
        }
        // 2-controlled swap `(c1, c2, a, b)`: one K2.
        if w_in == 4 && w_out == 4 && prim_matches(def, |bits| CellKind::K2.eval(bits))? {
            return Ok(self.emit(CellKind::K2, ins));
        }
        // 3-controlled swap `(c1, c2, c3, a, b)`: one K3.
        if w_in == 5 && w_out == 5 && prim_matches(def, |bits| CellKind::K3.eval(bits))? {
            return Ok(self.emit(CellKind::K3, ins));
        }

        // TODO(B4.2): synthesize general CSWAP networks; until then the
        // registry is exactly the verified table above.
        Err(LowerError::UnsupportedPrim { name: def.forward_name.clone() })
    }

    /// Lower `Branch(f, g)` at type `(A+B) ↔ (A'+B')` using the
    /// controlled-sequential approach: apply controlled-f (active when
    /// tag=true) then controlled-g (active when tag=false) on the shared
    /// payload.  Each emitted cell is promoted (K0→K1, K1→K2) with the
    /// tag threaded as an extra control.
    fn lower_branch(
        &mut self,
        f: &TypedNode,
        g: &TypedNode,
        ins: &[BitAt],
    ) -> Result<Vec<BitAt>, LowerError> {
        let wa = rail_width(&f.input)?;
        let wa_out = rail_width(&f.output)?;
        let wb = rail_width(&g.input)?;
        let wb_out = rail_width(&g.output)?;
        let max_w = wa.max(wb);
        let max_w_out = wa_out.max(wb_out);

        // Same-width arms: controlled-cell promotion requires equal
        // in/out widths per arm (the payload region is shared).
        if wa != wa_out || wb != wb_out {
            return Err(LowerError::UnsupportedType(f.input.clone()));
        }
        debug_assert_eq!(max_w, max_w_out);

        let tag = ins[0];
        let payload = &ins[1..];
        debug_assert_eq!(payload.len(), max_w);

        // Step 1: controlled-f on payload[0..wa], active when tag=true.
        // After this, payload[0..wa] = f(a) when tag=true; unchanged
        // when tag=false.
        let f_input = payload[..wa].to_vec();
        let (f_result, tag_after_f) =
            self.controlled_apply(f, &f_input, tag, false)?;

        // Reassemble the full payload after step 1.
        let mut payload_after_f = f_result;
        payload_after_f.extend_from_slice(&payload[wa..]);

        // Step 2: controlled-g on payload[0..wb], active when
        // tag=false (i.e. NOT(tag) is the control).
        let g_input = payload_after_f[..wb].to_vec();
        let (g_result, tag_after_g) =
            self.controlled_apply(g, &g_input, tag_after_f, true)?;

        // Reassemble: [tag, result_payload(max_w_out)]
        let mut result = vec![tag_after_g];
        result.extend(g_result);
        result.extend_from_slice(&payload_after_f[wb..]);
        debug_assert_eq!(result.len(), 1 + max_w_out);
        Ok(result)
    }

    /// Apply `node` to `data_ins`, with each emitted cell promoted to
    /// include `tag` as an additional control.  When `invert_tag` is
    /// false, cells fire when tag=true; when true, cells fire when
    /// tag=false (the tag is rail-swapped before use as control).
    ///
    /// Returns `(data_outputs, tag_out)`.  The tag threads through
    /// every promoted cell (consumed once by the first, reproduced by
    /// each, final value returned).
    ///
    /// Cell promotion:
    /// - K0(a): functionally identity, controlled version is also
    ///   identity.  Emitted unchanged (no promotion needed).
    /// - K1(c, a, b) → K2(tag, c, a, b): swap a,b only when BOTH tag
    ///   AND c are true.
    /// - K2 → would need K3 (not in vocabulary): error.
    ///
    /// Zero-cell rewiring (Permute) is unconditional.  For identity
    /// permutations this is correct (identity regardless of control).
    /// For non-identity zero-cell rewiring (e.g. rail swap / NOT),
    /// unconditional application would be incorrect.  Instead, we
    /// synthesise conditional K1 cells: position permutations use K1
    /// directly (conditional swap), and rail swaps (NOT) use an
    /// ancilla-based splitter/selector K1 pair (see Phase 2 below).
    fn controlled_apply(
        &mut self,
        node: &TypedNode,
        data_ins: &[BitAt],
        tag: BitAt,
        invert_tag: bool,
    ) -> Result<(Vec<BitAt>, BitAt), LowerError> {
        // Lower the arm in a sub-builder to get its cells in isolation.
        let mut sub = Builder {
            next_line: self.next_line,
            cells: Vec::new(),
            ancilla_inputs: Vec::new(),
            ancilla_outputs: Vec::new(),
        };
        // Create sub-builder inputs with the same pairs but depth 0
        // for line allocation purposes — we'll fix up depths below.
        let sub_outs = sub.apply(node, data_ins)?;

        // Claim the sub-builder's line range.
        self.next_line = sub.next_line;

        // Transfer any ancilla tracking from the sub-builder.
        self.ancilla_inputs.extend_from_slice(&sub.ancilla_inputs);
        self.ancilla_outputs.extend_from_slice(&sub.ancilla_outputs);

        if sub.cells.is_empty() {
            // Zero-cell arm.  If it's pure identity, return as-is.
            let is_identity = sub_outs
                .iter()
                .zip(data_ins.iter())
                .all(|(o, i)| o.pair == i.pair);
            if is_identity {
                return Ok((sub_outs, tag));
            }

            // Non-identity zero-cell rewiring (e.g. rail swap / NOT,
            // position permutation).  We must synthesise conditional
            // cells so the transformation only fires when the tag
            // selects this arm.
            //
            // Analyse: for each output position, find which input it
            // came from and whether a rail swap (NOT) was applied.
            let n = data_ins.len();
            let mut source = vec![0usize; n];
            let mut flipped = vec![false; n];
            for i in 0..n {
                let out_pair = sub_outs[i].pair;
                let mut found = false;
                for (j, din) in data_ins.iter().enumerate() {
                    if out_pair == din.pair {
                        source[i] = j;
                        flipped[i] = false;
                        found = true;
                        break;
                    }
                    if out_pair == din.pair.swapped() {
                        source[i] = j;
                        flipped[i] = true;
                        found = true;
                        break;
                    }
                }
                assert!(found, "zero-cell output pair matches no input");
            }

            let mut cur_tag = tag;
            let mut current = data_ins.to_vec();

            // Phase 1: conditional position permutation (K1 swaps).
            // Decompose the permutation into transpositions via cycles.
            let perm = source;
            let mut visited = vec![false; n];
            for i in 0..n {
                if visited[i] || perm[i] == i {
                    continue;
                }
                let cycle_start = i;
                let mut cycle = vec![i];
                let mut j = perm[i];
                while j != cycle_start {
                    cycle.push(j);
                    visited[j] = true;
                    j = perm[j];
                }
                visited[i] = true;
                // Swap cycle_start with each subsequent element in
                // reverse order to realise the cycle.
                for &k in cycle[1..].iter().rev() {
                    let control = if invert_tag {
                        cur_tag.rail_swapped()
                    } else {
                        cur_tag
                    };
                    let outs = self.emit(CellKind::K1, &[control, current[cycle_start], current[k]]);
                    cur_tag = if invert_tag {
                        outs[0].rail_swapped()
                    } else {
                        outs[0]
                    };
                    current[cycle_start] = outs[1];
                    current[k] = outs[2];
                }
            }

            // Phase 2: conditional rail swaps (NOT) using ancilla-
            // based K1 construction.
            //
            // For each flipped bit we synthesise two K1 cells:
            //
            //   Splitter: K1(bit, TRUE_anc, FALSE_anc)
            //     → (bit_copy, NOT(bit), bit_dup)   [always, by K1 truth table]
            //
            //   Selector: K1(control, bit_copy, NOT(bit))
            //     → control active:  (ctrl, NOT(bit), bit_copy)
            //       control inactive: (ctrl, bit_copy, NOT(bit))
            //
            // The selector's output[1] is the conditional NOT.
            //
            // A K0 buffer on the tag keeps the selector's inputs
            // tick-aligned with the splitter outputs (depth d → d+1).
            for i in 0..n {
                if !flipped[i] {
                    continue;
                }
                let true_anc = self.alloc_ancilla(true);
                let false_anc = self.alloc_ancilla(false);

                // Splitter.
                let split = self.emit(CellKind::K1, &[current[i], true_anc, false_anc]);
                let bit_copy = split[0]; // = bit value on fresh pair
                let not_bit = split[1]; // = NOT(bit) on fresh pair
                self.mark_garbage(split[2]); // redundant bit copy

                // Buffer the tag so it arrives at the same tick as the
                // splitter outputs (both at depth = original + 1).
                let control_raw = if invert_tag {
                    cur_tag.rail_swapped()
                } else {
                    cur_tag
                };
                let control_buf = self.emit(CellKind::K0, &[control_raw])[0];

                // Selector.
                let sel = self.emit(CellKind::K1, &[control_buf, bit_copy, not_bit]);
                cur_tag = if invert_tag {
                    sel[0].rail_swapped()
                } else {
                    sel[0]
                };
                current[i] = sel[1]; // conditional NOT result
                self.mark_garbage(sel[2]); // garbage
            }

            return Ok((current, cur_tag));
        }

        // Check for K3 cells in the sub-builder (can't promote to K4).
        for cell in &sub.cells {
            if cell.kind == CellKind::K3 {
                return Err(LowerError::Deferred {
                    node: "Branch (K3 in arm)",
                    plan: "K4 not in vocabulary",
                });
            }
        }

        // Thread the tag through the promoted cells.  For each K1 in
        // the sub-builder, promote to K2 with the tag as an extra
        // control.  K0 cells pass through unchanged.
        //
        // We also build a mapping from old output pair → new output
        // pair + depth, so we can update the data_outs at the end.
        let mut pair_depth_map: HashMap<RailPair, (RailPair, u32)> =
            HashMap::new();
        // Seed the map: data_ins pairs map to themselves at their
        // current depth (these are primary inputs to the sub-network).
        for bit in data_ins {
            pair_depth_map.insert(bit.pair, (bit.pair, bit.depth));
        }
        // Also seed ancilla pairs (depth 0 primary inputs).
        for &(pair, _) in &sub.ancilla_inputs {
            pair_depth_map.insert(pair, (pair, 0));
        }

        let mut cur_tag = tag;
        for cell in sub.cells {
            match cell.kind {
                CellKind::K0 => {
                    // K0: no promotion.  Recompute depth from actual
                    // input arrival times.  NB: the consumer may
                    // reference the producer's pair rail-swapped (a
                    // zero-cell NOT), so look up both orientations.
                    let in_pair = cell.inputs[0];
                    let in_depth = pair_depth_map
                        .get(&in_pair)
                        .or_else(|| pair_depth_map.get(&in_pair.swapped()))
                        .map(|&(_, d)| d)
                        .unwrap_or(cell.depth);
                    let new_depth = in_depth;
                    let out_pair = cell.outputs[0];
                    let out_depth = new_depth + 1;
                    pair_depth_map.insert(out_pair, (out_pair, out_depth));
                    self.cells.push(Cell {
                        kind: CellKind::K0,
                        depth: new_depth,
                        phase: (new_depth % u32::from(PHASE_COUNT)) as u8,
                        inputs: vec![in_pair],
                        outputs: vec![out_pair],
                    });
                }
                CellKind::K1 | CellKind::K2 => {
                    // Promote K1(c, a, b) → K2(tag, c, a, b).
                    // Promote K2(c1, c2, a, b) → K3(tag, c1, c2, a, b).
                    let promoted_kind = match cell.kind {
                        CellKind::K1 => CellKind::K2,
                        CellKind::K2 => CellKind::K3,
                        _ => unreachable!(),
                    };
                    let control_tag = if invert_tag {
                        cur_tag.rail_swapped()
                    } else {
                        cur_tag
                    };

                    // Recompute depth from actual arrivals (looking up
                    // both rail orientations — a consumer may see the
                    // producer's pair swapped by a zero-cell NOT).
                    let mut latest = control_tag.depth;
                    for ip in &cell.inputs {
                        let d = pair_depth_map
                            .get(ip)
                            .or_else(|| pair_depth_map.get(&ip.swapped()))
                            .map(|&(_, d)| d);
                        if let Some(d) = d {
                            latest = latest.max(d);
                        } else {
                            latest = latest.max(cell.depth);
                        }
                    }

                    let tag_out_pair = self.fresh_pair();
                    let new_depth = latest;
                    let new_phase = (new_depth % u32::from(PHASE_COUNT)) as u8;
                    let out_depth = new_depth + 1;

                    let mut new_inputs = vec![control_tag.pair];
                    new_inputs.extend_from_slice(&cell.inputs);
                    let mut new_outputs = vec![tag_out_pair];
                    new_outputs.extend_from_slice(&cell.outputs);

                    // Record output pair → depth mapping.
                    for op in &cell.outputs {
                        pair_depth_map.insert(*op, (*op, out_depth));
                    }

                    self.cells.push(Cell {
                        kind: promoted_kind,
                        depth: new_depth,
                        phase: new_phase,
                        inputs: new_inputs,
                        outputs: new_outputs,
                    });

                    let tag_out = if invert_tag {
                        BitAt { pair: tag_out_pair.swapped(), depth: out_depth }
                    } else {
                        BitAt { pair: tag_out_pair, depth: out_depth }
                    };
                    cur_tag = tag_out;
                }
                CellKind::K3 => unreachable!("checked above"),
            }
        }

        // Reconstruct data_outs with correct depths from the map
        // (both rail orientations; keep the caller's orientation).
        let fixed_outs: Vec<BitAt> = sub_outs
            .iter()
            .map(|b| {
                if let Some(&(pair, depth)) = pair_depth_map.get(&b.pair) {
                    BitAt { pair, depth }
                } else if let Some(&(_, depth)) =
                    pair_depth_map.get(&b.pair.swapped())
                {
                    BitAt { pair: b.pair, depth }
                } else {
                    *b
                }
            })
            .collect();

        // Update ancilla output depths from the promotion map.
        for anc in &mut self.ancilla_outputs {
            if let Some(&(_, depth)) = pair_depth_map
                .get(&anc.0)
                .or_else(|| pair_depth_map.get(&anc.0.swapped()))
            {
                anc.1 = depth;
            }
        }

        Ok((fixed_outs, cur_tag))
    }

    /// Lower `TraceLoop { carrier, body }` by full unrolling (ADR-0026
    /// C2.4). The trace runs at most `|carrier| + 1` body applications.
    ///
    /// Algorithm:
    /// 1. Inject `Right(input)` into type `carrier + input_ty` (ancilla tag
    ///    + padding).
    /// 2. Apply body unconditionally (first iteration).
    /// 3. For iterations 2..=N: duplicate the tag via K1 + ancillas, then
    ///    apply body controlled by the tag copy (fires only when Left =
    ///    still looping).
    /// 4. Project: the final state is guaranteed Right; extract the output
    ///    payload and mark tag + padding as garbage.
    fn lower_trace(
        &mut self,
        carrier: &Type,
        body: &TypedNode,
        ins: &[BitAt],
    ) -> Result<Vec<BitAt>, LowerError> {
        // Verify carrier is encodable (not Zero or recursive).
        let _w_u = rail_width(carrier)?;
        // body.input = carrier + input_ty, body.output = carrier + output_ty
        let w_body_in = rail_width(&body.input)?;
        let w_body_out = rail_width(&body.output)?;

        // Controlled unrolling requires equal body I/O width (the
        // pass-through identity when inactive needs matching widths).
        if w_body_in != w_body_out {
            return Err(LowerError::UnsupportedType(body.input.clone()));
        }
        let w_body = w_body_in; // = 1 + max(w_u, w_a) = 1 + max(w_u, w_b)

        // Extract the input/output payload types from the sum structure.
        let input_ty_of_trace = match &body.input {
            Type::Sum(_u, a) => a.as_ref(),
            _ => return Err(LowerError::UnsupportedType(body.input.clone())),
        };
        let output_ty_of_trace = match &body.output {
            Type::Sum(_u, b) => b.as_ref(),
            _ => return Err(LowerError::UnsupportedType(body.output.clone())),
        };
        let w_a = rail_width(input_ty_of_trace)?;
        let w_b = rail_width(output_ty_of_trace)?;
        let max_payload = w_body - 1; // = max(w_u, w_a) = max(w_u, w_b)

        debug_assert_eq!(ins.len(), w_a);

        // N = |carrier| + 1: maximum number of body applications.
        let n_iterations = enumerate_values(carrier).len() + 1;

        // ── Step 1: Inject Right(input) into (carrier + input_ty) ────
        // Encoding: [tag=false, input_bits(w_a), padding(max_payload - w_a)]
        let mut state: Vec<BitAt> = Vec::with_capacity(w_body);
        // Tag = false (Right injection)
        state.push(self.alloc_ancilla(false));
        // Input payload
        state.extend_from_slice(ins);
        // Padding (false = zero bits for unused payload region)
        for _ in 0..(max_payload - w_a) {
            state.push(self.alloc_ancilla(false));
        }
        debug_assert_eq!(state.len(), w_body);

        // ── Step 2: First body application (unconditional) ───────────
        state = self.apply(body, &state)?;
        debug_assert_eq!(state.len(), w_body);

        // ── Steps 3..=N: Guarded body applications ───────────────────
        for _ in 1..n_iterations {
            // Duplicate the tag (state[0]) using K1 + ancillas so we
            // have one copy for body's data input and one for control.
            // K1(c,a,b): c=true→(c,b,a), c=false→(c,a,b).
            // K1(tag, TRUE, FALSE) → (tag, NOT(tag), tag_copy).
            let true_anc = self.alloc_ancilla(true);
            let false_anc = self.alloc_ancilla(false);
            let split = self.emit(CellKind::K1, &[state[0], true_anc, false_anc]);
            let state_tag = split[0]; // tag value → body's first data input
            let not_tag = split[1];   // NOT(tag) → garbage
            let tag_copy = split[2];  // tag value → control for conditional

            self.mark_garbage(not_tag);
            state[0] = state_tag;

            // Apply body controlled by tag_copy (fires when tag=true=Left).
            let (new_state, ctrl_out) =
                self.controlled_apply(body, &state, tag_copy, false)?;
            // The control value threads through and comes out unchanged;
            // it's garbage now.
            self.mark_garbage(ctrl_out);
            state = new_state;
        }

        // ── Final: Project (carrier + output_ty) → output_ty ─────────
        // State is guaranteed Right: tag=false, payload[..w_b] = output,
        // payload[w_b..] = padding (zeros from the u-region, irrelevant).
        // Mark tag and padding as garbage; return output bits.
        self.mark_garbage(state[0]); // tag (guaranteed false)
        let output = state[1..1 + w_b].to_vec();
        for &bit in &state[1 + w_b..] {
            self.mark_garbage(bit); // padding garbage
        }
        Ok(output)
    }
}

/// Does `def.forward` compute exactly `f` on flattened bits, over the
/// whole (finite) domain?
fn prim_matches(
    def: &PrimDef,
    f: impl Fn(&[bool]) -> Vec<bool>,
) -> Result<bool, LowerError> {
    for v in enumerate_values(&def.input_ty) {
        let Some(w) = def.forward.get(&v) else {
            return Ok(false); // not total ⇒ not this template (inadmissible anyway)
        };
        if f(&bits_of(&v, &def.input_ty)?) != bits_of(w, &def.output_ty)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Lower a typed net to the phased cell netlist. The result always
/// satisfies [`PhasedCellNetlist::validate`] (asserted in debug builds).
pub fn lower(net: &TypedNode) -> Result<PhasedCellNetlist, LowerError> {
    lower_inner(net).map(|(netlist, _)| netlist)
}

/// Like [`lower`], but also returns the initial values for ancilla
/// inputs (in netlist input order, after the data inputs). Needed by
/// tests that simulate the netlist with correct ancilla seeding.
pub fn lower_with_ancillas(net: &TypedNode) -> Result<(PhasedCellNetlist, Vec<bool>), LowerError> {
    lower_inner(net)
}

fn lower_inner(net: &TypedNode) -> Result<(PhasedCellNetlist, Vec<bool>), LowerError> {
    net.validate()?;
    let w_in = rail_width(&net.input)?;
    let w_out = rail_width(&net.output)?;

    let mut b = Builder {
        next_line: 0,
        cells: Vec::new(),
        ancilla_inputs: Vec::new(),
        ancilla_outputs: Vec::new(),
    };
    let ins: Vec<BitAt> = (0..w_in)
        .map(|_| BitAt { pair: b.fresh_pair(), depth: 0 })
        .collect();
    let outs = b.apply(net, &ins)?;
    debug_assert_eq!(outs.len(), w_out);

    // Include ancilla inputs/outputs so every line has exactly one
    // driver and one consumer (validate invariant).
    let mut all_inputs: Vec<RailPair> = ins.iter().map(|x| x.pair).collect();
    let ancilla_values: Vec<bool> = b.ancilla_inputs.iter().map(|&(_, v)| v).collect();
    for &(pair, _) in &b.ancilla_inputs {
        all_inputs.push(pair);
    }
    let mut all_outputs: Vec<RailPair> = outs.iter().map(|x| x.pair).collect();
    let mut all_output_depths: Vec<u32> = outs.iter().map(|x| x.depth).collect();
    for &(pair, depth) in &b.ancilla_outputs {
        all_outputs.push(pair);
        all_output_depths.push(depth);
    }

    let netlist = PhasedCellNetlist {
        input_ty: net.input.clone(),
        output_ty: net.output.clone(),
        inputs: all_inputs,
        outputs: all_outputs,
        output_depths: all_output_depths,
        cells: b.cells,
        num_lines: b.next_line,
    };
    debug_assert_eq!(netlist.validate(), Ok(()));
    Ok((netlist, ancilla_values))
}

/// Lower a Core Π iso end to end: canonicalize (ADR-0014) then lower
/// (ADR-0031).
pub fn lower_iso(iso: &Iso) -> Result<PhasedCellNetlist, LowerError> {
    let net = canonicalize(iso)?;
    lower(&net)
}

/// Like [`lower_iso`], but also returns ancilla initial values.
pub fn lower_iso_with_ancillas(iso: &Iso) -> Result<(PhasedCellNetlist, Vec<bool>), LowerError> {
    let net = canonicalize(iso)?;
    lower_with_ancillas(&net)
}

#[cfg(test)]
mod tests {
    use super::*;
    use theseus_core::type_of;

    fn bit() -> Type {
        Type::sum(Type::One, Type::One)
    }

    /// Build an admitted PrimDef from a bijective bit function.
    fn prim_from_fn(
        name: &str,
        input_ty: Type,
        output_ty: Type,
        f: impl Fn(&[bool]) -> Vec<bool>,
    ) -> PrimDef {
        prim_from_bits(name, input_ty, output_ty, f).expect("encodable test prim")
    }

    fn three_bits() -> Type {
        Type::prod(bit(), Type::prod(bit(), bit()))
    }

    fn four_bits() -> Type {
        Type::prod(Type::prod(bit(), bit()), Type::prod(bit(), bit()))
    }

    fn fredkin_prim() -> PrimDef {
        prim_from_fn("fredkin", three_bits(), three_bits(), |b| CellKind::K1.eval(b))
    }

    fn ccswap_prim() -> PrimDef {
        prim_from_fn("ccswap", four_bits(), four_bits(), |b| CellKind::K2.eval(b))
    }

    fn not_prim() -> PrimDef {
        prim_from_fn("not", bit(), bit(), |b| vec![!b[0]])
    }

    fn cnot_prim() -> PrimDef {
        prim_from_fn("cnot", Type::prod(bit(), bit()), Type::prod(bit(), bit()), |b| {
            vec![b[0], b[0] ^ b[1]]
        })
    }

    /// Untimed functional evaluation of the cell netlist: propagate token
    /// values line by line, asserting the 1-of-2 INV-TOKEN discipline at
    /// every pair read. (The *timed* phased-sim is the next slice.)
    fn eval_cells(net: &PhasedCellNetlist, input_bits: &[bool]) -> Vec<bool> {
        let mut line: Vec<Option<bool>> = vec![None; net.num_lines as usize];
        fn set_pair(line: &mut [Option<bool>], p: RailPair, v: bool) {
            line[p.line1.0 as usize] = Some(v);
            line[p.line0.0 as usize] = Some(!v);
        }
        fn get_pair(line: &[Option<bool>], p: RailPair) -> bool {
            let v1 = line[p.line1.0 as usize].expect("1-line driven");
            let v0 = line[p.line0.0 as usize].expect("0-line driven");
            assert_ne!(v0, v1, "INV-TOKEN violated: pair not 1-of-2");
            v1
        }

        assert_eq!(input_bits.len(), net.inputs.len());
        for (&p, &v) in net.inputs.iter().zip(input_bits) {
            set_pair(&mut line, p, v);
        }
        for cell in &net.cells {
            let vals: Vec<bool> = cell.inputs.iter().map(|&p| get_pair(&line, p)).collect();
            let outs = cell.kind.eval(&vals);
            for (&p, &v) in cell.outputs.iter().zip(&outs) {
                set_pair(&mut line, p, v);
            }
        }
        net.outputs.iter().map(|&p| get_pair(&line, p)).collect()
    }

    /// Differential check against the core interpreter (the semantic
    /// root, ADR-0008/0018): for every input value, the lowered netlist's
    /// untimed evaluation equals `eval(iso)`.
    fn assert_lowering_agrees(iso: &Iso) -> PhasedCellNetlist {
        let (in_ty, out_ty) = type_of(iso).expect("iso types");
        let (net, ancilla_vals) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
        assert_eq!(net.validate(), Ok(()), "lowered netlist validates");
        let n_data_out = rail_width(&out_ty).unwrap();
        for v in enumerate_values(&in_ty) {
            let mut bits = bits_of(&v, &in_ty).unwrap();
            bits.extend_from_slice(&ancilla_vals);
            let got_full = eval_cells(&net, &bits);
            let got = &got_full[..n_data_out];
            let mut fuel = 10_000u64;
            let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
                .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
            assert_eq!(
                got,
                &bits_of(&expected, &out_ty).unwrap()[..],
                "netlist disagrees with core eval on {v:?}"
            );
        }
        net
    }

    // ── Zero-cell lemmas: wiring only ────────────────────────────────

    #[test]
    fn identity_on_bit_is_pure_wiring() {
        let net = assert_lowering_agrees(&Iso::Id(bit()));
        assert!(net.cells.is_empty());
        assert_eq!(net.outputs, net.inputs);
        assert_eq!(net.output_depths, vec![0]);
        assert_eq!(net.depth(), 0);
    }

    #[test]
    fn swap_prod_is_rail_pair_rewiring() {
        // Lemma: Permute(×, σ) = rail-pair rewiring, zero cells.
        let net = assert_lowering_agrees(&Iso::SwapProd(bit(), bit()));
        assert!(net.cells.is_empty());
        assert_eq!(net.outputs, vec![net.inputs[1], net.inputs[0]]);
    }

    #[test]
    fn assoc_and_unit_prod_are_free() {
        for iso in [
            Iso::AssocRProd(bit(), bit(), bit()),
            Iso::AssocLProd(bit(), bit(), bit()),
            Iso::UniteProd(bit()),
            Iso::UnitiProd(bit()),
        ] {
            let net = assert_lowering_agrees(&iso);
            assert!(net.cells.is_empty(), "{iso:?} should lower to wiring");
        }
    }

    #[test]
    fn not_via_swap_sum_is_rail_swap() {
        // Lemma: NOT-in-Permute — rail swap, zero cells.
        let net = assert_lowering_agrees(&Iso::SwapSum(Type::One, Type::One));
        assert!(net.cells.is_empty());
        assert_eq!(net.outputs[0].line0, net.inputs[0].line1);
        assert_eq!(net.outputs[0].line1, net.inputs[0].line0);
    }

    #[test]
    fn not_prim_is_rail_swap() {
        let iso = Iso::Prim(Box::new(not_prim()));
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
        assert_eq!(net.outputs[0], net.inputs[0].swapped());
    }

    // ── Prim templates: cells from the registry ─────────────────────

    #[test]
    fn fredkin_prim_is_one_k1_at_phase_0() {
        let iso = Iso::Prim(Box::new(fredkin_prim()));
        let net = assert_lowering_agrees(&iso);
        assert_eq!(net.cells.len(), 1);
        let cell = &net.cells[0];
        assert_eq!((cell.kind, cell.depth, cell.phase), (CellKind::K1, 0, 0));
        assert_eq!(cell.inputs, net.inputs);
        assert_eq!(cell.outputs, net.outputs);
        assert_eq!(net.output_depths, vec![1, 1, 1]);
        assert_eq!(net.occupancy(), BTreeMap::from([((CellKind::K1, 0), 1)]));
        assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K1, 1)]));
    }

    #[test]
    fn ccswap_prim_is_one_k2() {
        let iso = Iso::Prim(Box::new(ccswap_prim()));
        let net = assert_lowering_agrees(&iso);
        assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K2, 1)]));
        assert_eq!(net.cells[0].phase, 0);
    }

    #[test]
    fn sym_of_fredkin_lowers_to_the_same_cell() {
        // Adjoint of a self-inverse prim: still one K1 (C2.5's syntactic
        // fallback — mirrored netlist — degenerates to the same network).
        let iso = Iso::sym(Iso::Prim(Box::new(fredkin_prim())));
        let net = assert_lowering_agrees(&iso);
        assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K1, 1)]));
    }

    // ── Phase rotation (C2.3) ────────────────────────────────────────

    #[test]
    fn seq_of_fredkins_advances_phase_mod_4() {
        let fk = || Iso::Prim(Box::new(fredkin_prim()));
        let iso = Iso::seq(fk(), Iso::seq(fk(), Iso::seq(fk(), Iso::seq(fk(), fk()))));
        let net = assert_lowering_agrees(&iso);
        assert_eq!(net.cells.len(), 5);
        let depths: Vec<u32> = net.cells.iter().map(|c| c.depth).collect();
        let phases: Vec<u8> = net.cells.iter().map(|c| c.phase).collect();
        assert_eq!(depths, vec![0, 1, 2, 3, 4]);
        assert_eq!(phases, vec![0, 1, 2, 3, 0], "phase = depth mod 4 wraps");
        assert_eq!(net.depth(), 5);
        assert_eq!(net.output_depths, vec![5, 5, 5]);
        assert!(net.tick_imbalances().unwrap().is_empty());
    }

    // ── Reg(n) → n K0 stages (C2.1) ──────────────────────────────────

    #[test]
    fn reg_lowers_to_buffer_chain() {
        let reg3 = TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(3) };
        let net = lower(&reg3).unwrap();
        assert_eq!(net.validate(), Ok(()));
        assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K0, 3)]));
        let phases: Vec<u8> = net.cells.iter().map(|c| c.phase).collect();
        assert_eq!(phases, vec![0, 1, 2]);
        assert_eq!(net.output_depths, vec![3]);
        // Functionally the identity.
        for v in [true, false] {
            assert_eq!(eval_cells(&net, &[v]), vec![v]);
        }
    }

    #[test]
    fn reg_on_two_bits_buffers_each_bit() {
        let ty = Type::prod(bit(), bit());
        let reg2 = TypedNode { input: ty.clone(), output: ty, kind: NodeKind::Reg(2) };
        let net = lower(&reg2).unwrap();
        assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K0, 4)]));
        assert_eq!(
            net.occupancy(),
            BTreeMap::from([((CellKind::K0, 0), 2), ((CellKind::K0, 1), 2)])
        );
        assert_eq!(net.output_depths, vec![2, 2]);
    }

    // ── Skew is recorded, not repaired (C2.2 deferred) ───────────────

    #[test]
    fn unbalanced_par_records_skew_and_imbalance() {
        // (Reg(1) × id × id) ; fredkin — the control arrives one tick
        // late, so the K1 fires at depth 1 and reports the imbalance the
        // future balancer must repair.
        let id_bit = TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Permute { monoid: Monoid::Prod, sigma: vec![0] },
        };
        let reg1 = TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(1) };
        let par = TypedNode {
            input: three_bits(),
            output: three_bits(),
            kind: NodeKind::Par(
                Box::new(reg1),
                Box::new(TypedNode {
                    input: Type::prod(bit(), bit()),
                    output: Type::prod(bit(), bit()),
                    kind: NodeKind::Par(Box::new(id_bit.clone()), Box::new(id_bit)),
                }),
            ),
        };
        let fredkin = TypedNode {
            input: three_bits(),
            output: three_bits(),
            kind: NodeKind::Prim(Box::new(fredkin_prim())),
        };
        let net = lower(&TypedNode {
            input: three_bits(),
            output: three_bits(),
            kind: NodeKind::Seq(vec![par, fredkin]),
        })
        .unwrap();
        assert_eq!(net.validate(), Ok(()));

        // One K0 (the reg) + one K1 firing a tick later.
        assert_eq!(
            net.cell_counts(),
            BTreeMap::from([(CellKind::K0, 1), (CellKind::K1, 1)])
        );
        let k1 = net.cells.iter().find(|c| c.kind == CellKind::K1).unwrap();
        assert_eq!((k1.depth, k1.phase), (1, 1));

        let imbalances = net.tick_imbalances().unwrap();
        assert_eq!(imbalances.len(), 1);
        assert_eq!(imbalances[0].arrivals, vec![1, 0, 0]);
    }

    #[test]
    fn output_skew_is_visible() {
        // Reg(1) × id: outputs arrive at ticks [1, 0]; the skew is the
        // balancer's future worklist, recorded faithfully today.
        let id_bit = TypedNode {
            input: bit(),
            output: bit(),
            kind: NodeKind::Permute { monoid: Monoid::Prod, sigma: vec![0] },
        };
        let reg1 = TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(1) };
        let net = lower(&TypedNode {
            input: Type::prod(bit(), bit()),
            output: Type::prod(bit(), bit()),
            kind: NodeKind::Par(Box::new(reg1), Box::new(id_bit)),
        })
        .unwrap();
        assert_eq!(net.output_depths, vec![1, 0]);
        assert!(net.tick_imbalances().unwrap().is_empty());
    }

    // ── Branch lowering ────────────────────────────────────────────────

    #[test]
    fn branch_id_id_on_bit_is_identity() {
        // Plus(Id(One), Id(One)) = Branch on Bit (tag-only, 0 payload).
        // Both arms are identity on One → zero cells.
        let iso = Iso::plus(Iso::Id(Type::One), Iso::Id(Type::One));
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
    }

    #[test]
    fn branch_fredkin_id_on_3bit_sum() {
        // Plus(Fredkin, Id(3bits)) on (3bits + 3bits):
        // controlled-Fredkin = one K2 cell.
        let fk = || Iso::Prim(Box::new(fredkin_prim()));
        let iso = Iso::plus(fk(), Iso::Id(three_bits()));
        let net = assert_lowering_agrees(&iso);
        assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K2, 1)]));
    }

    #[test]
    fn branch_id_fredkin_on_3bit_sum() {
        // Plus(Id(3bits), Fredkin): controlled-g (with inverted tag) = K2.
        let fk = || Iso::Prim(Box::new(fredkin_prim()));
        let iso = Iso::plus(Iso::Id(three_bits()), fk());
        let net = assert_lowering_agrees(&iso);
        assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K2, 1)]));
    }

    // ── Distrib / Factor lowering ─────────────────────────────────────

    #[test]
    fn distrib_is_zero_cell_rewiring() {
        // Distrib(One, One, One): 1 × (1+1) → (1×1) + (1×1).
        // = One × Bit → Bit. But rail_width(One) = 0, so
        // a × (b+c) with a=One: wa=0, input = [tag, payload(max(0,0))=0]
        // = just 1 pair (the tag of b+c).
        // This is a trivial case: tag is already at the front.
        let iso = Iso::Distrib(Type::One, Type::One, Type::One);
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
    }

    #[test]
    fn distrib_bit_one_one() {
        // Distrib(Bit, One, One): Bit × (1+1) → (Bit×1) + (Bit×1) = Bit + Bit.
        // wa=1, input layout: [bit_a, tag, payload(0)] = [bit_a, tag].
        // output layout: [tag, bit_a, payload(0)] = [tag, bit_a].
        // Pure reordering: tag moves from position 1 to position 0.
        let iso = Iso::Distrib(bit(), Type::One, Type::One);
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
    }

    #[test]
    fn factor_is_inverse_of_distrib() {
        // Factor(Bit, One, One): (Bit×1) + (Bit×1) → Bit × (1+1).
        let iso = Iso::Factor(bit(), Type::One, Type::One);
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
    }

    // ── General sum encoding ──────────────────────────────────────────

    #[test]
    fn general_sum_identity_lowers() {
        // Id(Bit + One) is now encodable with general sum encoding.
        let iso = Iso::Id(Type::sum(bit(), Type::One));
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
    }

    #[test]
    fn swap_sum_general_is_tag_swap() {
        // SwapSum(Bit, One): Bit + One → One + Bit.
        // Tag rail swap, payload unchanged.
        let iso = Iso::SwapSum(bit(), Type::One);
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
    }

    // ── Honest refusals: deferred vocabulary ─────────────────────────

    #[test]
    fn trace_trivial_one_step() {
        // Trace(One, SwapSum(One, One)): carrier=One (|u|=1), body swaps
        // Left↔Right. Body : (1+1) <-> (1+1) is just NOT on the tag.
        // Semantics: s = Right(Unit); t = body(s) = Left(Unit);
        // s = Left(Unit); t = body(s) = Right(Unit) → exit with Unit.
        // So trace(One, SwapSum(1,1)) : One <-> One = identity.
        let iso = Iso::trace(Type::One, Iso::SwapSum(Type::One, Type::One));
        assert_lowering_agrees(&iso);
    }

    #[test]
    fn unknown_prim_is_rejected_not_guessed() {
        // CNOT is bijective and admitted, but matches no verified
        // template in this slice (B4.2 synthesis deferred): refuse.
        let iso = Iso::Prim(Box::new(cnot_prim()));
        match lower_iso(&iso) {
            Err(LowerError::UnsupportedPrim { name }) => assert_eq!(name, "cnot"),
            other => panic!("expected UnsupportedPrim, got {other:?}"),
        }
    }

    // ── Encoding round-trip sanity ───────────────────────────────────

    #[test]
    fn bits_round_trip_values() {
        for ty in [
            bit(),
            three_bits(),
            four_bits(),
            Type::prod(Type::One, bit()),
            // General sums:
            Type::sum(bit(), Type::One),
            Type::sum(Type::One, bit()),
            Type::sum(bit(), bit()),
            Type::sum(bit(), Type::prod(bit(), bit())),
        ] {
            for v in enumerate_values(&ty) {
                let bits = bits_of(&v, &ty).unwrap();
                assert_eq!(bits.len(), rail_width(&ty).unwrap());
                assert_eq!(value_from_bits(&ty, &bits), Ok(v));
            }
        }
    }

    #[test]
    fn rail_width_examples() {
        assert_eq!(rail_width(&Type::One), Ok(0));
        assert_eq!(rail_width(&bit()), Ok(1));
        assert_eq!(rail_width(&three_bits()), Ok(3));
        assert_eq!(rail_width(&four_bits()), Ok(4));
        assert!(rail_width(&Type::Zero).is_err());
        // General sums are now encodable:
        assert_eq!(rail_width(&Type::sum(bit(), Type::One)), Ok(2));
        assert_eq!(rail_width(&Type::sum(Type::One, bit())), Ok(2));
        assert_eq!(rail_width(&Type::sum(bit(), bit())), Ok(2));
        assert_eq!(
            rail_width(&Type::sum(bit(), Type::prod(bit(), bit()))),
            Ok(3) // 1 + max(1, 2) = 3
        );
    }
}
