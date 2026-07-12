//! Dual-rail lowering: typed netlist → phased cell netlist (ADR-0031).
//!
//! # Implemented lemmas (this slice)
//!
//! | node | lowering | cells |
//! |---|---|---|
//! | `Permute(×, σ)` | rail-pair rewiring | 0 |
//! | `Permute(+, σ)` on the bit type | passthrough / rail swap (NOT-in-Permute) | 0 |
//! | `Prim` | verified template from the registry (identity / NOT / Fredkin / 2-CSWAP) | 0 or 1 |
//! | `Reg(n)` | `n` K0 buffer stages per bit (C2.1) | `n·width` |
//! | `Seq` / `Par` | composition / wiring | 0 |
//!
//! Every template is verified **per instance** against the prim's finite
//! `(forward)` semantics before use — exhaustive over the domain
//! (ADR-0031: "each per-instance checkable — exhaustive small").
//! Phase = depth mod P throughout (ADR-0026 C2.3).
//!
//! # Deferred (honest list, with owners)
//!
//! - TODO(Branch): CSWAP demux tree forward / merge tree adjoint; masking
//!   becomes token routing under INV-TOKEN (ADR-0031). Next slice(s).
//! - TODO(Distrib/Factor): tag-controlled payload steering, quarantined.
//! - TODO(TraceLoop): unroll per ADR-0026 C2.4 (static bound ≤ |u|).
//! - TODO(sum encoding): general sums as 1-of-N tag lines + payload rail
//!   pairs (ADR-0011 earmark). Only `1 + 1` (the bit — where 1-of-2 tag
//!   *is* the dual-rail pair) is encodable in this slice.
//! - TODO(B4.2): general CSWAP-network synthesis for prims beyond the
//!   built-in template table.
//! - Tick-exact balancing (C2.2) is *recorded, not repaired*:
//!   [`PhasedCellNetlist::tick_imbalances`] and `output_depths` skew are
//!   the balancer's worklist.

use core::fmt;
use std::collections::BTreeMap;

use theseus_core::{admit, enumerate_values, Iso, PrimDef, Type, TypeError, Value};

use crate::canonicalize::canonicalize;
use crate::ir::cell::{Cell, CellKind, LineId, PhasedCellNetlist, RailPair, PHASE_COUNT};
use crate::ir::typed::{flatten_layer, Monoid, NodeKind, TypedError, TypedNode};

/// Error from the dual-rail lowering.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LowerError {
    /// The source iso is ill-typed.
    Type(TypeError),
    /// The typed net is malformed.
    Typed(TypedError),
    /// Node vocabulary deferred to a later slice (see module docs).
    Deferred { node: &'static str, plan: &'static str },
    /// No dual-rail encoding for this type in this slice (general sums
    /// need 1-of-N tag lines + payload, ADR-0011; `0`-typed nets are
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
                write!(f, "no dual-rail encoding for {ty} yet (general sums deferred)")
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

/// Is this type the bit, `1 + 1`? The one sum type encodable in this
/// slice: its 1-of-2 tag lines coincide with the dual-rail pair.
fn is_bit(ty: &Type) -> bool {
    matches!(ty, Type::Sum(a, b) if **a == Type::One && **b == Type::One)
}

/// Number of rail pairs (logical bits) encoding a value of `ty`:
/// `1` is 0 bits, `1 + 1` is 1 bit, products concatenate. Anything else
/// has no encoding in this slice.
pub fn rail_width(ty: &Type) -> Result<usize, LowerError> {
    match ty {
        Type::One => Ok(0),
        t if is_bit(t) => Ok(1),
        Type::Prod(a, b) => Ok(rail_width(a)? + rail_width(b)?),
        _ => Err(LowerError::UnsupportedType(ty.clone())),
    }
}

/// Flatten a value of an encodable type to its bit string, in rail-pair
/// order. Convention: `Left(Unit)` is the `1` token (matches the core
/// test vocabulary's `tt`), `Right(Unit)` the `0` token.
pub fn bits_of(value: &Value, ty: &Type) -> Result<Vec<bool>, LowerError> {
    match (ty, value) {
        (Type::One, Value::Unit) => Ok(vec![]),
        (t, Value::Left(u)) if is_bit(t) && **u == Value::Unit => Ok(vec![true]),
        (t, Value::Right(u)) if is_bit(t) && **u == Value::Unit => Ok(vec![false]),
        (Type::Prod(ta, tb), Value::Pair(a, b)) => {
            let mut bits = bits_of(a, ta)?;
            bits.extend(bits_of(b, tb)?);
            Ok(bits)
        }
        _ => Err(LowerError::UnsupportedType(ty.clone())),
    }
}

/// Rebuild a value of `ty` from its flattened bit string — the inverse
/// of [`bits_of`]. Errors if `ty` has no dual-rail encoding.
///
/// # Panics
///
/// Panics if `bits.len() != rail_width(ty)` (a caller bug, not a data
/// condition).
pub fn value_from_bits(ty: &Type, bits: &[bool]) -> Result<Value, LowerError> {
    fn go(ty: &Type, bits: &mut std::slice::Iter<'_, bool>) -> Result<Value, LowerError> {
        match ty {
            Type::One => Ok(Value::Unit),
            t if is_bit(t) => {
                let b = *bits.next().expect("bit string too short for type");
                Ok(if b { Value::left(Value::Unit) } else { Value::right(Value::Unit) })
            }
            Type::Prod(a, b) => {
                let va = go(a, bits)?;
                let vb = go(b, bits)?;
                Ok(Value::pair(va, vb))
            }
            _ => Err(LowerError::UnsupportedType(ty.clone())),
        }
    }
    let mut it = bits.iter();
    let v = go(ty, &mut it)?;
    assert!(it.next().is_none(), "bit string too long for type");
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
}

impl Builder {
    fn fresh_pair(&mut self) -> RailPair {
        let pair = RailPair { line0: LineId(self.next_line), line1: LineId(self.next_line + 1) };
        self.next_line += 2;
        pair
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

            // Lemma (ADR-0031): Permute(+, σ) is tag-line rewiring. In
            // this slice only the bit (`1 + 1`) is encodable, where the
            // tag lines are the rail pair itself: σ = id is passthrough,
            // σ = [1,0] is NOT — a rail swap, zero cells.
            NodeKind::Permute { monoid: Monoid::Sum, sigma } => {
                if is_bit(&node.input) && is_bit(&node.output) {
                    match sigma.as_slice() {
                        [0, 1] => Ok(ins.to_vec()),
                        [1, 0] => Ok(vec![ins[0].rail_swapped()]),
                        _ => Err(LowerError::UnsupportedType(node.input.clone())),
                    }
                } else {
                    // TODO(sum encoding): 1-of-N tag-line rewiring for
                    // general sums (ADR-0011 earmark).
                    Err(LowerError::UnsupportedType(node.input.clone()))
                }
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

            // TODO(Branch): CSWAP demux tree forward / merge tree
            // adjoint; masking becomes token routing (ADR-0031).
            NodeKind::Branch(..) => Err(LowerError::Deferred {
                node: "Branch",
                plan: "CSWAP demux/merge trees, ADR-0031",
            }),

            // TODO(Distrib/Factor): tag-controlled payload steering,
            // quarantined per ADR-0031.
            NodeKind::Distrib(_) => Err(LowerError::Deferred {
                node: "Distrib/Factor",
                plan: "tag-controlled steering, ADR-0031",
            }),

            // TODO(TraceLoop): unroll-only in v1, bound ≤ |u| (ADR-0026
            // C2.4); per-stage phase advance.
            NodeKind::TraceLoop { .. } => Err(LowerError::Deferred {
                node: "TraceLoop",
                plan: "unroll per ADR-0026 C2.4",
            }),
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

        // TODO(B4.2): synthesize general CSWAP networks; until then the
        // registry is exactly the verified table above.
        Err(LowerError::UnsupportedPrim { name: def.forward_name.clone() })
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
    net.validate()?;
    let w_in = rail_width(&net.input)?;
    let w_out = rail_width(&net.output)?;

    let mut b = Builder { next_line: 0, cells: Vec::new() };
    let ins: Vec<BitAt> = (0..w_in)
        .map(|_| BitAt { pair: b.fresh_pair(), depth: 0 })
        .collect();
    let outs = b.apply(net, &ins)?;
    debug_assert_eq!(outs.len(), w_out);

    let netlist = PhasedCellNetlist {
        input_ty: net.input.clone(),
        output_ty: net.output.clone(),
        inputs: ins.iter().map(|x| x.pair).collect(),
        outputs: outs.iter().map(|x| x.pair).collect(),
        output_depths: outs.iter().map(|x| x.depth).collect(),
        cells: b.cells,
        num_lines: b.next_line,
    };
    debug_assert_eq!(netlist.validate(), Ok(()));
    Ok(netlist)
}

/// Lower a Core Π iso end to end: canonicalize (ADR-0014) then lower
/// (ADR-0031).
pub fn lower_iso(iso: &Iso) -> Result<PhasedCellNetlist, LowerError> {
    let net = canonicalize(iso)?;
    lower(&net)
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
        let net = lower_iso(iso).expect("lowering succeeds");
        assert_eq!(net.validate(), Ok(()), "lowered netlist validates");
        for v in enumerate_values(&in_ty) {
            let bits = bits_of(&v, &in_ty).unwrap();
            let got = eval_cells(&net, &bits);
            let mut fuel = 10_000u64;
            let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
                .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
            assert_eq!(
                got,
                bits_of(&expected, &out_ty).unwrap(),
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

    // ── Honest refusals: deferred vocabulary ─────────────────────────

    #[test]
    fn branch_is_deferred() {
        let iso = Iso::plus(Iso::Id(Type::One), Iso::Id(Type::One));
        assert!(matches!(
            lower_iso(&iso),
            Err(LowerError::Deferred { node: "Branch", .. })
        ));
    }

    #[test]
    fn trace_is_deferred() {
        let iso = Iso::trace(Type::One, Iso::SwapSum(Type::One, Type::One));
        assert!(matches!(
            lower_iso(&iso),
            Err(LowerError::Deferred { node: "TraceLoop", .. })
        ));
    }

    #[test]
    fn distrib_is_not_yet_encodable() {
        // distrib's sum-of-products boundary type needs the general
        // 1-of-N sum encoding, itself deferred.
        let iso = Iso::Distrib(Type::One, Type::One, Type::One);
        assert!(matches!(lower_iso(&iso), Err(LowerError::UnsupportedType(_))));
    }

    #[test]
    fn general_sum_types_are_not_yet_encodable() {
        let iso = Iso::Id(Type::sum(bit(), Type::One));
        assert!(matches!(lower_iso(&iso), Err(LowerError::UnsupportedType(_))));
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
        for ty in [bit(), three_bits(), four_bits(), Type::prod(Type::One, bit())] {
            for v in enumerate_values(&ty) {
                let bits = bits_of(&v, &ty).unwrap();
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
        assert!(rail_width(&Type::sum(bit(), Type::One)).is_err());
    }
}
