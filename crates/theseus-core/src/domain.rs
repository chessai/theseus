//! Flat bit-encoded domains (ADR-0051).
//!
//! Whole-domain checks over a prim (admission, template matching, gate truth
//! tables) used to walk boxed [`Value`] trees:
//! [`enumerate_values`](crate::prim::enumerate_values), then
//! `BTreeMap<Value, Value>` lookups, then `bits_of`. A `Flat(8)` value alone
//! is up to 256 boxes. Here a finite type's inhabitants are `u64` *indices*
//! and a prim's tables are a [`BitTable`] of `u32` slices, read once from the
//! maps.
//!
//! # Encoding
//!
//! An index is `theseus_netlist::bits_of`'s bit string packed little-endian:
//! bit `j` of the index is `bits_of(v)[j]`. `bits_of` stays the definition;
//! this is its packed form, property-tested against it in theseus-netlist
//! (which `theseus-core` cannot depend on). Per type:
//!
//! - `1`: no bits. `0`: no bits, no inhabitants.
//! - `a + b`: bit 0 is the tag, **1 = `Left`**, 0 = `Right`; the payload
//!   starts at bit 1 and takes `max(width(a), width(b))` bits, the narrower
//!   arm zero-padded. An uninhabited arm takes 0 payload bits.
//! - `a × b`: `a`'s bits, then `b`'s.
//! - `Flat(n)`, which has no rails (`bits_of` rejects it): the depth `k` of
//!   the thermometer `Right^k(Left(Unit))` as `n` bits, LSB first. That is
//!   the leaf byte of [`value_to_leaf_bytes`](crate::prim::value_to_leaf_bytes).
//!
//! Every other index (a set padding bit, a tag selecting an uninhabited
//! arm, a bit at or above [`bit_width`]) encodes no value.
//!
//! # Order
//!
//! [`valid_indices`] yields in
//! [`enumerate_values`](crate::prim::enumerate_values) order: a sum's
//! `Left`s before its `Right`s, a product's left component major, a `Flat` by
//! ascending `k`. On the inhabitants of one type, `Value`'s derived `Ord`
//! orders the same way (`Left < Right`, pairs lexicographically,
//! thermometers by depth), so this is also the iteration order of a
//! `BTreeMap` keyed by them. Numeric index order is different (the tag is
//! the low bit, a product's left component the low bits), so a scan that
//! must report the *first* failure walks the valid list, not `0..2^w`.

use std::collections::BTreeMap;

use crate::bvexpr::{pack_rows, unpack_rows, LANES, LANE_ZERO, OPERAND_BITS};
use crate::enumerate::cardinality_u64;
use crate::prim::{value_from_leaf_bytes, AdmitError, PrimDef, SpecTransfer, Value};
use crate::ty::Type;

/// The widest type an index can encode.
pub const MAX_INDEX_BITS: usize = 64;

/// Width in bits of the encoding of `ty` (of `bits_of(v, ty)` for any `v`).
pub fn bit_width(ty: &Type) -> usize {
    Layout::new(ty).width
}

/// `v`'s index in `ty`: `bits_of(v, ty)` packed little-endian.
///
/// # Panics
///
/// If `v` does not inhabit `ty`, or `bit_width(ty) > 64`.
pub fn index_of(v: &Value, ty: &Type) -> u64 {
    try_index_of(v, ty).unwrap_or_else(|| panic!("index_of: {v:?} does not inhabit {ty}"))
}

/// [`index_of`], or `None` if `v` does not inhabit `ty`.
///
/// # Panics
///
/// If `bit_width(ty) > 64`.
pub fn try_index_of(v: &Value, ty: &Type) -> Option<u64> {
    Layout::indexable(ty).encode(v)
}

/// The inverse of [`index_of`]: the value encoded by `i`, or `None` if `i`
/// encodes no inhabitant of `ty`.
///
/// # Panics
///
/// If `bit_width(ty) > 64`.
pub fn value_of_index(ty: &Type, i: u64) -> Option<Value> {
    let layout = Layout::indexable(ty);
    if shr(i, layout.width) != 0 {
        return None;
    }
    layout.decode(i)
}

/// The indices of the inhabitants of `ty`, in
/// [`enumerate_values`](crate::prim::enumerate_values) order (see the
/// module docs): exactly `enumerate_values(ty).map(index_of)`, built up
/// front as one `u64` per inhabitant.
///
/// # Panics
///
/// If `bit_width(ty) > 64`.
pub fn valid_indices(ty: &Type) -> impl Iterator<Item = u64> {
    Layout::indexable(ty).valid().into_iter()
}

/// The low `width` bits of `i`, LSB first (an index as a `bits_of` string).
pub fn bits_of_index(i: u64, width: usize) -> Vec<bool> {
    (0..width).map(|j| shr(i, j) & 1 == 1).collect()
}

/// Pack a bit string (LSB first) into an index. Panics past 64 bits.
pub fn index_of_bits(bits: &[bool]) -> u64 {
    assert!(
        bits.len() <= MAX_INDEX_BITS,
        "index_of_bits: {} bits",
        bits.len()
    );
    bits.iter()
        .enumerate()
        .fold(0, |acc, (j, &b)| acc | (u64::from(b) << j))
}

fn shl(x: u64, s: usize) -> u64 {
    if s >= 64 {
        0
    } else {
        x << s
    }
}

fn shr(x: u64, s: usize) -> u64 {
    if s >= 64 {
        0
    } else {
        x >> s
    }
}

fn mask(width: usize) -> u64 {
    if width >= 64 {
        u64::MAX
    } else {
        (1 << width) - 1
    }
}

/// `Right^k(Left(Unit))`.
fn thermometer(k: u64) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

/// A type compiled for index work: width, inhabitation and leaf count
/// (`count_leaves`, the leaf-byte layout) cached per node.
#[derive(Clone, Debug)]
struct Layout {
    width: usize,
    inhabited: bool,
    leaves: usize,
    shape: Shape,
}

#[derive(Clone, Debug)]
enum Shape {
    Zero,
    One,
    Flat(u8),
    Sum(Box<Layout>, Box<Layout>),
    Prod(Box<Layout>, Box<Layout>),
}

impl Layout {
    fn new(ty: &Type) -> Layout {
        match ty {
            Type::Zero => Layout {
                width: 0,
                inhabited: false,
                leaves: 0,
                shape: Shape::Zero,
            },
            Type::One => Layout {
                width: 0,
                inhabited: true,
                leaves: 0,
                shape: Shape::One,
            },
            Type::Flat(n) => Layout {
                width: *n as usize,
                inhabited: true,
                leaves: 1,
                shape: Shape::Flat(*n),
            },
            Type::Sum(a, b) => {
                let (a, b) = (Layout::new(a), Layout::new(b));
                Layout {
                    width: 1 + a.arm_width().max(b.arm_width()),
                    inhabited: a.inhabited || b.inhabited,
                    leaves: 1 + a.leaves.max(b.leaves),
                    shape: Shape::Sum(Box::new(a), Box::new(b)),
                }
            }
            Type::Prod(a, b) => {
                let (a, b) = (Layout::new(a), Layout::new(b));
                Layout {
                    width: a.width + b.width,
                    inhabited: a.inhabited && b.inhabited,
                    leaves: a.leaves + b.leaves,
                    shape: Shape::Prod(Box::new(a), Box::new(b)),
                }
            }
        }
    }

    /// [`Layout::new`] for a type whose indices fit a `u64`.
    fn indexable(ty: &Type) -> Layout {
        let layout = Layout::new(ty);
        assert!(
            layout.width <= MAX_INDEX_BITS,
            "{ty} is {} bits wide; indices hold {MAX_INDEX_BITS}",
            layout.width
        );
        layout
    }

    /// Payload bits as a sum arm: none if uninhabited (the tag never
    /// selects it).
    fn arm_width(&self) -> usize {
        if self.inhabited {
            self.width
        } else {
            0
        }
    }

    /// `v`'s index, or `None` if `v` does not inhabit this type.
    fn encode(&self, v: &Value) -> Option<u64> {
        match (&self.shape, v) {
            (Shape::One, Value::Unit) => Some(0),
            (Shape::Sum(a, _), Value::Left(x)) => Some(1 | a.encode(x)? << 1),
            (Shape::Sum(_, b), Value::Right(y)) => Some(b.encode(y)? << 1),
            (Shape::Prod(a, b), Value::Pair(x, y)) => {
                Some(a.encode(x)? | shl(b.encode(y)?, a.width))
            }
            (Shape::Flat(n), _) => {
                // Thermometer depth, refusing past the last inhabitant.
                let last = mask(*n as usize);
                let (mut k, mut cur) = (0u64, v);
                loop {
                    match cur {
                        Value::Left(u) if **u == Value::Unit => return Some(k),
                        Value::Right(rest) if k < last => {
                            k += 1;
                            cur = rest;
                        }
                        _ => return None,
                    }
                }
            }
            _ => None,
        }
    }

    /// The value at `i` (`i < 2^width`), or `None` if `i` encodes none.
    fn decode(&self, i: u64) -> Option<Value> {
        match &self.shape {
            Shape::Zero => None,
            Shape::One => Some(Value::Unit),
            Shape::Flat(_) => Some(thermometer(i)),
            Shape::Sum(a, b) => {
                let (left, payload) = (i & 1 == 1, i >> 1);
                let arm = if left { a } else { b };
                // Padding past the arm's own bits must be zero.
                if !arm.inhabited || shr(payload, arm.width) != 0 {
                    return None;
                }
                let v = arm.decode(payload)?;
                Some(if left {
                    Value::left(v)
                } else {
                    Value::right(v)
                })
            }
            Shape::Prod(a, b) => {
                let va = a.decode(i & mask(a.width))?;
                let vb = b.decode(shr(i, a.width))?;
                Some(Value::pair(va, vb))
            }
        }
    }

    /// Inhabitants' indices in enumeration order.
    fn valid(&self) -> Vec<u64> {
        match &self.shape {
            Shape::Zero => vec![],
            Shape::One => vec![0],
            Shape::Flat(n) => (0..=mask(*n as usize)).collect(),
            Shape::Sum(a, b) => {
                let mut out: Vec<u64> = a.valid().into_iter().map(|x| 1 | x << 1).collect();
                out.extend(b.valid().into_iter().map(|y| y << 1));
                out
            }
            Shape::Prod(a, b) => {
                let (va, vb) = (a.valid(), b.valid());
                let mut out = Vec::with_capacity(va.len() * vb.len());
                for &x in &va {
                    out.extend(vb.iter().map(|&y| x | shl(y, a.width)));
                }
                out
            }
        }
    }

    /// `value_to_leaf_bytes(decode(i))` into `out` (`leaves` bytes, zero on
    /// entry).
    fn write_leaf_bytes(&self, i: u64, out: &mut [u8]) {
        match &self.shape {
            Shape::Zero | Shape::One => {}
            Shape::Flat(_) => out[0] = i as u8,
            Shape::Sum(a, b) => {
                // Leaf-byte tags are the other way round: 0 = Left.
                let (arm, tag) = if i & 1 == 1 { (a, 0) } else { (b, 1) };
                out[0] = tag;
                arm.write_leaf_bytes(i >> 1, &mut out[1..1 + arm.leaves]);
            }
            Shape::Prod(a, b) => {
                let (oa, ob) = out.split_at_mut(a.leaves);
                a.write_leaf_bytes(i & mask(a.width), oa);
                b.write_leaf_bytes(shr(i, a.width), ob);
            }
        }
    }

    /// The index of `try_value_from_leaf_bytes(bytes)`, failing (`None`)
    /// exactly where it fails: the same walk, building no value.
    fn read_leaf_bytes(&self, bytes: &[u8]) -> Option<u64> {
        match &self.shape {
            Shape::Zero => None,
            Shape::One => Some(0),
            Shape::Sum(a, b) => {
                if bytes[0] == 0 {
                    Some(1 | a.read_leaf_bytes(&bytes[1..])? << 1)
                } else {
                    Some(b.read_leaf_bytes(&bytes[1..])? << 1)
                }
            }
            Shape::Prod(a, b) => {
                let ia = a.read_leaf_bytes(bytes)?;
                let ib = b.read_leaf_bytes(&bytes[a.leaves..])?;
                Some(ia | shl(ib, a.width))
            }
            Shape::Flat(n) => {
                let byte = u64::from(bytes[0]);
                (byte <= mask(*n as usize)).then_some(byte)
            }
        }
    }
}

// ── BitTable ────────────────────────────────────────────────────────

/// Sentinel: no entry (also every non-inhabitant's slot).
const NONE: u32 = u32::MAX;
/// Sentinel: the entry's value does not inhabit the other type, and the
/// reverse map sends that value back to the key -- the `Value`-level
/// round-trip check passes here (see [`BitTable::is_bijection`]).
const ILL_TYPED_BACK: u32 = u32::MAX - 1;
/// Sentinel: the entry's value does not inhabit the other type, and does
/// not round-trip.
const ILL_TYPED: u32 = u32::MAX - 2;

fn is_index(x: u32) -> bool {
    x < ILL_TYPED
}

/// Below this many bits a table side is always dense enough.
const DENSE_FLOOR_BITS: usize = 16;
/// Past [`DENSE_FLOOR_BITS`], a side may have at most this many slots per
/// inhabitant (`numeral_ty(k)` is `k` bits wide for `k` values).
const MAX_SLOTS_PER_VALUE: u64 = 16;

/// What one direction of a prim maps an index to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Entry {
    /// No entry: the map is not total there.
    Missing,
    /// The image's index.
    Index(u32),
    /// The image does not inhabit the target type.
    IllTyped,
}

/// One side (domain or codomain) of a [`BitTable`].
#[derive(Clone, Debug)]
struct Side {
    ty: Type,
    layout: Layout,
    /// Inhabitants' indices, enumeration (= `BTreeMap`) order.
    valid: Vec<u32>,
}

impl Side {
    fn new(ty: &Type) -> Option<Side> {
        let layout = Layout::new(ty);
        if layout.width > BitTable::MAX_BITS {
            return None;
        }
        let slots = 1u64 << layout.width;
        let budget = cardinality_u64(ty)
            .saturating_mul(MAX_SLOTS_PER_VALUE)
            .max(1 << DENSE_FLOOR_BITS);
        if slots > budget {
            return None;
        }
        let valid = layout.valid().into_iter().map(|i| i as u32).collect();
        Some(Side {
            ty: ty.clone(),
            layout,
            valid,
        })
    }

    fn value(&self, i: u32) -> Value {
        self.layout
            .decode(u64::from(i))
            .expect("BitTable: a valid index decodes")
    }
}

/// A prim's `(forward, backward)` maps over flat index domains:
/// `fwd[index_of(v)] = index_of(forward[v])`, `bwd` likewise, with a
/// sentinel in every slot that has no entry or is not an inhabitant.
/// Built once from a [`PrimDef`] (one pass over each map, no lookups).
///
/// Only for small, dense types: each side is at most [`Self::MAX_BITS`]
/// wide and not much sparser than its cardinality, so
/// [`from_def`](Self::from_def) is `None` for e.g. `numeral_ty(30)`;
/// callers keep their `Value` walk for those.
#[derive(Clone, Debug)]
pub struct BitTable {
    input: Side,
    output: Side,
    fwd: Vec<u32>,
    bwd: Vec<u32>,
    /// Keys of `forward` / `backward` that do not inhabit their type.
    fwd_stray_keys: usize,
    bwd_stray_keys: usize,
}

impl BitTable {
    /// Widest side a table materializes (`2^24` slots, 64 MiB).
    pub const MAX_BITS: usize = 24;

    /// Read `def`'s maps into index tables, or `None` if a side is too
    /// wide or too sparse (see the type docs).
    pub fn from_def(def: &PrimDef) -> Option<BitTable> {
        let input = Side::new(&def.input_ty)?;
        let output = Side::new(&def.output_ty)?;
        let (fwd, fwd_stray_keys) = read_map(&input, &output, &def.forward, &def.backward);
        let (bwd, bwd_stray_keys) = read_map(&output, &input, &def.backward, &def.forward);
        Some(BitTable {
            input,
            output,
            fwd,
            bwd,
            fwd_stray_keys,
            bwd_stray_keys,
        })
    }

    /// Bits of an input index ([`bit_width`] of the input type).
    pub fn input_width(&self) -> usize {
        self.input.layout.width
    }

    /// Bits of an output index.
    pub fn output_width(&self) -> usize {
        self.output.layout.width
    }

    /// The input inhabitants' indices, in enumeration (`BTreeMap`) order.
    pub fn inputs(&self) -> &[u32] {
        &self.input.valid
    }

    /// The output inhabitants' indices, in enumeration order.
    pub fn outputs(&self) -> &[u32] {
        &self.output.valid
    }

    /// What `forward` maps input index `i` to.
    pub fn forward(&self, i: u32) -> Entry {
        entry(self.fwd[i as usize])
    }

    /// What `backward` maps output index `o` to.
    pub fn backward(&self, o: u32) -> Entry {
        entry(self.bwd[o as usize])
    }

    /// The input value at a valid input index.
    pub fn input_value(&self, i: u32) -> Value {
        self.input.value(i)
    }

    /// The output value at a valid output index.
    pub fn output_value(&self, o: u32) -> Value {
        self.output.value(o)
    }

    /// `admit`'s mutual-inverse verdict: `Ok` iff `forward` and `backward`
    /// are total and `backward ∘ forward = id`, `forward ∘ backward = id`.
    ///
    /// Fast path, three slice loops: totality (every valid index has a
    /// well-typed image), injectivity/surjectivity (a bitset of hit output
    /// indices whose popcount must be the codomain's size), and backward
    /// consistency (`bwd[fwd[i]] == i`). A bijection with a left inverse
    /// is a mutual inverse, so that is the whole check when it passes.
    ///
    /// Otherwise the four `Value`-level loops are replayed in index space,
    /// in enumeration order, so the error is the one they report: same
    /// variant, same (first) offending `Value`. That includes their
    /// handling of ill-typed images, which they look up in the other map as
    /// values.
    pub fn is_bijection(&self) -> Result<(), AdmitError> {
        if self.bijective_fast() {
            return Ok(());
        }
        let (ins, outs) = (&self.input.valid, &self.output.valid);
        if let Some(&i) = ins.iter().find(|&&i| self.fwd[i as usize] == NONE) {
            return Err(AdmitError::ForwardNotTotal(self.input.value(i)));
        }
        if let Some(&o) = outs.iter().find(|&&o| self.bwd[o as usize] == NONE) {
            return Err(AdmitError::BackwardNotTotal(self.output.value(o)));
        }
        if let Some(&i) = ins.iter().find(|&&i| !round_trips(&self.fwd, &self.bwd, i)) {
            return Err(AdmitError::BackwardOfForward {
                input: self.input.value(i),
            });
        }
        if let Some(&o) = outs
            .iter()
            .find(|&&o| !round_trips(&self.bwd, &self.fwd, o))
        {
            return Err(AdmitError::ForwardOfBackward {
                input: self.output.value(o),
            });
        }
        Ok(())
    }

    fn bijective_fast(&self) -> bool {
        let (ins, outs) = (&self.input.valid, &self.output.valid);
        if ins.len() != outs.len()
            || !ins.iter().all(|&i| is_index(self.fwd[i as usize]))
            || !outs.iter().all(|&o| is_index(self.bwd[o as usize]))
        {
            return false;
        }
        // Images are indices of inhabitants, so `n` distinct ones among
        // the `n` valid outputs make `fwd` a bijection.
        let mut hit = vec![0u64; self.bwd.len().div_ceil(64)];
        for &i in ins {
            let o = self.fwd[i as usize];
            hit[(o / 64) as usize] |= 1 << (o % 64);
        }
        let distinct: usize = hit.iter().map(|w| w.count_ones() as usize).sum();
        distinct == outs.len()
            && ins
                .iter()
                .all(|&i| self.bwd[self.fwd[i as usize] as usize] == i)
    }

    /// `admit`'s spec==table check for one direction, fed from the index
    /// range: input leaf bytes come from the index, and the spec's output
    /// bytes are read back to an index and compared with the table's.
    ///
    /// The `Value` path walks the map's keys in `BTreeMap` order; this walks
    /// the valid indices in the same order, so it stands in for it only
    /// when those are the keys -- `None` if the map has keys outside its
    /// type (callers check totality first), or the spec is too wide for
    /// the lane planes. Errors are the `Value` path's: the first
    /// mismatching input, and a panic on undecodable spec output.
    pub(crate) fn spec_matches(
        &self,
        transfer: &SpecTransfer,
        backward: bool,
    ) -> Option<Result<(), AdmitError>> {
        let (from, to, map, stray) = if backward {
            (&self.output, &self.input, &self.bwd, self.bwd_stray_keys)
        } else {
            (&self.input, &self.output, &self.fwd, self.fwd_stray_keys)
        };
        if stray != 0 || transfer.width as usize > OPERAND_BITS {
            return None;
        }
        // Flattened once, evaluated per chunk; rows move through the bulk
        // transposes (ADR-0052).
        let program = transfer.lane_program();
        let (n_in, n_out) = (from.layout.leaves, transfer.leaves.len());
        let mut vars = vec![[LANE_ZERO; OPERAND_BITS]; n_in];
        let mut out = vec![[LANE_ZERO; OPERAND_BITS]; n_out];
        let mut in_rows = vec![0u8; LANES * n_in];
        let mut out_rows = vec![0u8; LANES * n_out];
        for chunk in from.valid.chunks(LANES) {
            let in_rows = &mut in_rows[..chunk.len() * n_in];
            in_rows.fill(0);
            for (lane, &i) in chunk.iter().enumerate() {
                let row = &mut in_rows[lane * n_in..(lane + 1) * n_in];
                from.layout.write_leaf_bytes(u64::from(i), row);
            }
            pack_rows(in_rows, &mut vars);
            program.eval(&vars, out.as_flattened_mut());
            let out_rows = &mut out_rows[..chunk.len() * n_out];
            unpack_rows(&out, out_rows);
            for (lane, &i) in chunk.iter().enumerate() {
                let out_bytes = &out_rows[lane * n_out..(lane + 1) * n_out];
                let got = match to.layout.read_leaf_bytes(out_bytes) {
                    Some(o) => o,
                    // Undecodable: decode anyway, to fail exactly as the
                    // `Value` path does (it panics).
                    None => to
                        .layout
                        .encode(&value_from_leaf_bytes(out_bytes, &to.ty))
                        .expect("a decoded value inhabits its type"),
                };
                // An ill-typed table value never equals a decoded one.
                if u64::from(map[i as usize]) != got {
                    return Some(Err(AdmitError::SpecTableMismatch {
                        input: from.value(i),
                    }));
                }
            }
        }
        Some(Ok(()))
    }
}

fn entry(x: u32) -> Entry {
    match x {
        NONE => Entry::Missing,
        ILL_TYPED | ILL_TYPED_BACK => Entry::IllTyped,
        o => Entry::Index(o),
    }
}

/// One pass over `map`: the slot of each key that inhabits `keys` gets its
/// value's index in `vals`, or an ill-typed sentinel. Returns the table and
/// the number of keys outside `keys`' type.
fn read_map(
    keys: &Side,
    vals: &Side,
    map: &BTreeMap<Value, Value>,
    inverse: &BTreeMap<Value, Value>,
) -> (Vec<u32>, usize) {
    let mut table = vec![NONE; 1 << keys.layout.width];
    let mut stray = 0;
    for (k, v) in map {
        let Some(i) = keys.layout.encode(k) else {
            stray += 1;
            continue;
        };
        table[i as usize] = match vals.layout.encode(v) {
            Some(o) => o as u32,
            // The `Value` check looks `v` up in the inverse map; record
            // its outcome (ill-typed images are rare -- a malformed def).
            None if inverse.get(v) == Some(k) => ILL_TYPED_BACK,
            None => ILL_TYPED,
        };
    }
    (table, stray)
}

/// The `Value` loops' pointwise check `back[there[x]] == x`, at a key that
/// has an entry.
fn round_trips(there: &[u32], back: &[u32], x: u32) -> bool {
    match there[x as usize] {
        ILL_TYPED_BACK => true,
        ILL_TYPED | NONE => false,
        y => back[y as usize] == x,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bvexpr::testgen::SplitMix64;
    use crate::prim::{
        count_leaves, enumerate_values, try_value_from_leaf_bytes, value_to_leaf_bytes,
    };

    fn bool_ty() -> Type {
        Type::sum(Type::One, Type::One)
    }

    /// A random type tree: `0`, `1`, `Bool`, `Flat(1..=3)` leaves under sums
    /// and products.
    fn random_type(rng: &mut SplitMix64, depth: usize) -> Type {
        if depth == 0 || rng.below(3) == 0 {
            return match rng.below(8) {
                0 => Type::Zero,
                1 | 2 => Type::One,
                3 | 4 => bool_ty(),
                _ => Type::Flat(1 + rng.below(3) as u8),
            };
        }
        let (a, b) = (random_type(rng, depth - 1), random_type(rng, depth - 1));
        if rng.below(2) == 0 {
            Type::sum(a, b)
        } else {
            Type::prod(a, b)
        }
    }

    /// Hand-picked shapes (sums of unequal arms, uninhabited arms, nested
    /// products, `Flat`, `One`, `Zero`), then random ones; all at most 12
    /// bits wide.
    fn test_types() -> Vec<Type> {
        let f = Type::Flat;
        let mut types = vec![
            Type::Zero,
            Type::One,
            bool_ty(),
            f(1),
            f(3),
            f(8),
            Type::sum(Type::Zero, Type::Zero),
            Type::sum(Type::Zero, Type::One),
            Type::sum(f(2), Type::One),
            Type::sum(Type::One, Type::prod(f(2), f(3))),
            Type::sum(Type::prod(f(2), Type::Zero), bool_ty()),
            Type::sum(Type::sum(Type::One, f(2)), Type::prod(f(2), Type::One)),
            Type::prod(Type::One, Type::Zero),
            Type::prod(f(2), Type::prod(bool_ty(), f(3))),
            Type::prod(Type::prod(f(2), bool_ty()), Type::sum(f(1), Type::One)),
            Type::numeral_ty(5),
            Type::list_spine_ty(bool_ty(), 3),
        ];
        let mut rng = SplitMix64(0xF1A7_D0DA);
        while types.len() < 400 {
            let ty = random_type(&mut rng, 4);
            if bit_width(&ty) <= 12 && cardinality_u64(&ty) <= 4096 {
                types.push(ty);
            }
        }
        types
    }

    /// `valid_indices` is `enumerate_values` encoded, in the same order,
    /// which is `Value` (= `BTreeMap`) order; `value_of_index` inverts
    /// `index_of` and rejects every other index below and just past
    /// `2^bit_width`.
    #[test]
    fn indices_round_trip_in_enumeration_order() {
        for ty in test_types() {
            let values = enumerate_values(&ty);
            let indices: Vec<u64> = valid_indices(&ty).collect();
            let encoded: Vec<u64> = values.iter().map(|v| index_of(v, &ty)).collect();
            assert_eq!(indices, encoded, "{ty}");
            assert!(values.windows(2).all(|p| p[0] < p[1]), "{ty}: Ord order");
            let w = bit_width(&ty);
            for (v, &i) in values.iter().zip(&indices) {
                assert!(i < 1 << w, "{ty}: {i} >= 2^{w}");
                assert_eq!(value_of_index(&ty, i).as_ref(), Some(v), "{ty}");
            }
            let valid: std::collections::BTreeSet<u64> = indices.iter().copied().collect();
            assert_eq!(valid.len(), indices.len(), "{ty}: distinct");
            for i in 0..=(1u64 << w) {
                match value_of_index(&ty, i) {
                    Some(v) => assert_eq!(index_of(&v, &ty), i, "{ty}"),
                    None => assert!(!valid.contains(&i), "{ty}: {i} rejected"),
                }
            }
        }
    }

    /// Index <-> leaf bytes agrees with `value_to_leaf_bytes` and, on any
    /// bytes, with `try_value_from_leaf_bytes` (failing exactly where it
    /// fails).
    #[test]
    fn leaf_bytes_bridge_matches_value_path() {
        let mut rng = SplitMix64(0x1EAF_B17E);
        for ty in test_types() {
            let layout = Layout::new(&ty);
            assert_eq!(layout.leaves, count_leaves(&ty), "{ty}");
            for v in enumerate_values(&ty) {
                let i = index_of(&v, &ty);
                let bytes = value_to_leaf_bytes(&v, &ty).unwrap();
                let mut out = vec![0u8; layout.leaves];
                layout.write_leaf_bytes(i, &mut out);
                assert_eq!(out, bytes, "{ty}: {v:?}");
                assert_eq!(layout.read_leaf_bytes(&bytes), Some(i), "{ty}");
            }
            for _ in 0..200 {
                let bytes: Vec<u8> = (0..layout.leaves).map(|_| rng.below(9) as u8).collect();
                let expected = try_value_from_leaf_bytes(&bytes, &ty)
                    .ok()
                    .map(|v| index_of(&v, &ty));
                assert_eq!(layout.read_leaf_bytes(&bytes), expected, "{ty}: {bytes:?}");
            }
        }
    }

    /// A `Flat(n)` leaf is its leaf byte's `n` low bits.
    #[test]
    fn flat_leaves_encode_their_leaf_byte() {
        let ty = Type::prod(Type::Flat(3), Type::prod(Type::Flat(1), Type::Flat(5)));
        for v in enumerate_values(&ty) {
            let b = value_to_leaf_bytes(&v, &ty).unwrap();
            let expected = u64::from(b[0]) | u64::from(b[1]) << 3 | u64::from(b[2]) << 4;
            assert_eq!(index_of(&v, &ty), expected);
        }
        assert_eq!(index_of(&thermometer(200), &Type::Flat(8)), 200);
        assert_eq!(try_index_of(&thermometer(8), &Type::Flat(3)), None);
    }

    /// `try_index_of` accepts exactly the inhabitants (the values of
    /// `enumerate_values`), refusing values of other types.
    #[test]
    fn try_index_of_refuses_non_inhabitants() {
        let types = test_types();
        let foreign: Vec<Value> = types
            .iter()
            .take(60)
            .flat_map(|ty| enumerate_values(ty).into_iter().take(40))
            .chain([
                Value::Unit,
                Value::pair(Value::Unit, Value::Unit),
                Value::right(Value::right(Value::pair(Value::Unit, Value::Unit))),
            ])
            .collect();
        for ty in &types {
            let inhabitants: std::collections::BTreeSet<Value> =
                enumerate_values(ty).into_iter().collect();
            for v in &foreign {
                assert_eq!(
                    try_index_of(v, ty).is_some(),
                    inhabitants.contains(v),
                    "{v:?} : {ty}"
                );
            }
        }
    }

    #[test]
    fn widths() {
        assert_eq!(bit_width(&Type::Zero), 0);
        assert_eq!(bit_width(&Type::One), 0);
        assert_eq!(bit_width(&bool_ty()), 1);
        assert_eq!(bit_width(&Type::Flat(8)), 8);
        // An uninhabited arm takes no payload bits.
        let dead_arm = Type::sum(Type::prod(Type::Flat(8), Type::Zero), Type::One);
        assert_eq!(bit_width(&dead_arm), 1);
        assert_eq!(bit_width(&Type::numeral_ty(100)), 100);
        assert_eq!(bits_of_index(0b1011, 5), [true, true, false, true, false]);
        assert_eq!(index_of_bits(&[true, true, false, true, false]), 0b1011);
    }

    #[test]
    #[should_panic(expected = "indices hold 64")]
    fn indices_refuse_types_past_64_bits() {
        let _ = valid_indices(&Type::numeral_ty(65));
    }

    fn table_def(in_ty: Type, out_ty: Type, f: impl Fn(&Value) -> Value) -> PrimDef {
        let forward: BTreeMap<Value, Value> = enumerate_values(&in_ty)
            .into_iter()
            .map(|v| (v.clone(), f(&v)))
            .collect();
        let backward = forward
            .iter()
            .map(|(v, w)| (w.clone(), v.clone()))
            .collect();
        PrimDef {
            forward_name: "t".into(),
            backward_name: "t_inv".into(),
            input_ty: in_ty,
            output_ty: out_ty,
            forward,
            backward,
            symbolic: None,
        }
    }

    #[test]
    fn bit_table_reads_both_maps() {
        // Flat(2) x Bool -> Bool x Flat(2), swapping the components.
        let a = Type::prod(Type::Flat(2), bool_ty());
        let b = Type::prod(bool_ty(), Type::Flat(2));
        let def = table_def(a.clone(), b.clone(), |v| match v {
            Value::Pair(x, y) => Value::pair(y.as_ref().clone(), x.as_ref().clone()),
            _ => unreachable!(),
        });
        let t = BitTable::from_def(&def).unwrap();
        assert_eq!((t.input_width(), t.output_width()), (3, 3));
        let expected: Vec<u32> = valid_indices(&a).map(|i| i as u32).collect();
        assert_eq!(t.inputs(), expected);
        for (v, w) in &def.forward {
            let (i, o) = (index_of(v, &a) as u32, index_of(w, &b) as u32);
            assert_eq!(t.forward(i), Entry::Index(o));
            assert_eq!(t.backward(o), Entry::Index(i));
            assert_eq!(t.input_value(i), *v);
            assert_eq!(t.output_value(o), *w);
        }
        assert_eq!(t.is_bijection(), Ok(()));
    }

    /// Too wide, or too sparse past the dense floor: no table.
    #[test]
    fn bit_table_declines_wide_or_sparse_types() {
        let succ = |n: usize| {
            let ty = Type::numeral_ty(n);
            let vals = enumerate_values(&ty);
            table_def(ty.clone(), ty, move |v| {
                let k = vals.iter().position(|x| x == v).unwrap();
                vals[(k + 1) % vals.len()].clone()
            })
        };
        assert!(BitTable::from_def(&succ(16)).is_some()); // 16 bits: under the floor
        assert!(BitTable::from_def(&succ(20)).is_none()); // 2^20 slots for 20 values
        let flat = Type::prod(Type::Flat(8), Type::prod(Type::Flat(8), Type::Flat(8)));
        let wide = PrimDef {
            forward_name: "w".into(),
            backward_name: "w".into(),
            input_ty: Type::prod(flat.clone(), Type::Flat(1)),
            output_ty: flat,
            forward: BTreeMap::new(),
            backward: BTreeMap::new(),
            symbolic: None,
        };
        assert!(BitTable::from_def(&wide).is_none()); // 25 bits
    }
}
