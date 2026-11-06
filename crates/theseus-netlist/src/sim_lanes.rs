//! Lane-parallel (bit-sliced) netlist simulation (ADR-0048).
//!
//! Every cell kind is a pure bitwise function of its ports
//! ([`CellKind::eval_lanes`]), so a token line can carry a *lane mask*
//! -- bit `l` is the line's token in independent run `l` -- instead of
//! one `bool`. One pass over the netlist then evaluates [`Lanes::WIDTH`]
//! input vectors at once. The ROM is baked into the netlist, so lanes
//! differ only in their input bits: exactly the shape of whole-domain
//! oracles and many-start ring runs.
//!
//! Two simulators, each checked lane-by-lane against its scalar
//! reference (which stays the authority):
//!
//! - [`simulate_lanes`] / [`simulate_rails_lanes`] -- the timed sim
//!   ([`crate::sim::simulate`]): same preflight, same tick schedule, same
//!   per-tick INV-TOKEN assertion, now `line0 ^ line1 == 1` on every
//!   active lane. Drivenness is lane-independent (the netlist is static),
//!   so it is one `bool` per line. A violation names the lowest failing
//!   lane and carries exactly the error the scalar sim reports on it.
//! - [`CompactBalanced::simulate_lanes`] -- the untimed topological sim
//!   ([`CompactBalanced::simulate`]) over a flat `CompactPlan` built
//!   once per netlist: K0 buffers, pass-through controls and rail-role
//!   swaps are resolved at plan time into slot aliases, so the hot loop
//!   touches only the cells that compute, and never allocates.

use std::collections::BTreeMap;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

use crate::balance::{CompactBalanced, CompactSimError};
use crate::ir::cell::{CellKind, LaneOp, PhasedCellNetlist, RailPair};
use crate::sim::{SimError, SimResult, TickSchedule};
use crate::simd::{UCell, UniformPlan};
use theseus_core::simd::{self, SimdLevel};

/// `64 * N` independent boolean lanes: bit `l % 64` of word `l / 64` is
/// lane `l`. Bitwise operators act lane-wise as plain `[u64; N]` loops: portable
/// bit-slicing (64 lanes per word op), no `unsafe` or `std::simd`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Lanes<const N: usize>(pub [u64; N]);

/// The default lane block: 256 lanes.
pub type Lane = Lanes<4>;

impl<const N: usize> Lanes<N> {
    /// Number of lanes.
    pub const WIDTH: usize = 64 * N;
    /// Every lane 0.
    pub const ZERO: Self = Lanes([0; N]);
    /// Every lane 1.
    pub const ONES: Self = Lanes([u64::MAX; N]);

    /// Every lane `v`.
    pub fn splat(v: bool) -> Self {
        if v {
            Self::ONES
        } else {
            Self::ZERO
        }
    }

    /// Lanes `0..n` set, the rest clear: the active-lane mask for `n`
    /// packed vectors.
    ///
    /// # Panics
    /// If `n > WIDTH`.
    pub fn first(n: usize) -> Self {
        assert!(
            n <= Self::WIDTH,
            "{n} lanes exceed the {} lane width",
            Self::WIDTH
        );
        let mut words = [0u64; N];
        for (i, w) in words.iter_mut().enumerate() {
            *w = match n.saturating_sub(64 * i) {
                0 => 0,
                k if k >= 64 => u64::MAX,
                k => (1u64 << k) - 1,
            };
        }
        Lanes(words)
    }

    /// Lane `lane`'s bit.
    pub fn get(&self, lane: usize) -> bool {
        (self.0[lane / 64] >> (lane % 64)) & 1 == 1
    }

    /// Set lane `lane`'s bit to `v`.
    pub fn set(&mut self, lane: usize, v: bool) {
        let bit = 1u64 << (lane % 64);
        if v {
            self.0[lane / 64] |= bit;
        } else {
            self.0[lane / 64] &= !bit;
        }
    }

    /// The lowest set lane, if any.
    pub fn lowest(&self) -> Option<usize> {
        self.0
            .iter()
            .enumerate()
            .find(|(_, &w)| w != 0)
            .map(|(i, w)| 64 * i + w.trailing_zeros() as usize)
    }

    /// Controlled swap: exchange `a` and `b` on the lanes where `s` is set.
    #[inline]
    pub fn cswap(s: Self, a: Self, b: Self) -> (Self, Self) {
        let d = (a ^ b) & s;
        (a ^ d, b ^ d)
    }

    /// XOR every word with `mask` (`0` or `u64::MAX`: identity or NOT).
    #[inline]
    fn xor_word(mut self, mask: u64) -> Self {
        for w in &mut self.0 {
            *w ^= mask;
        }
        self
    }
}

macro_rules! lane_binop {
    ($Op:ident, $op:ident, $OpAssign:ident, $op_assign:ident, $tok:tt) => {
        impl<const N: usize> $Op for Lanes<N> {
            type Output = Self;
            #[inline]
            fn $op(mut self, rhs: Self) -> Self {
                for (a, b) in self.0.iter_mut().zip(rhs.0) {
                    *a $tok b;
                }
                self
            }
        }
        impl<const N: usize> $OpAssign for Lanes<N> {
            #[inline]
            fn $op_assign(&mut self, rhs: Self) {
                for (a, b) in self.0.iter_mut().zip(rhs.0) {
                    *a $tok b;
                }
            }
        }
    };
}
lane_binop!(BitAnd, bitand, BitAndAssign, bitand_assign, &=);
lane_binop!(BitOr, bitor, BitOrAssign, bitor_assign, |=);
lane_binop!(BitXor, bitxor, BitXorAssign, bitxor_assign, ^=);

impl<const N: usize> Not for Lanes<N> {
    type Output = Self;
    #[inline]
    fn not(self) -> Self {
        self.xor_word(u64::MAX)
    }
}

/// Pack one bit vector per lane: bit `p` of `vectors[l]` becomes lane `l`
/// of word `p`. Lanes past `vectors.len()` are 0.
///
/// # Panics
/// If there are more vectors than lanes or their lengths differ.
pub fn pack_lanes<const N: usize, V: AsRef<[bool]>>(vectors: &[V]) -> Vec<Lanes<N>> {
    assert!(
        vectors.len() <= Lanes::<N>::WIDTH,
        "{} vectors exceed the {} lane width",
        vectors.len(),
        Lanes::<N>::WIDTH
    );
    let width = vectors.first().map_or(0, |v| v.as_ref().len());
    let rows: Vec<&[bool]> = vectors
        .iter()
        .enumerate()
        .map(|(lane, v)| {
            let v = v.as_ref();
            assert_eq!(v.len(), width, "lane {lane}: vector width mismatch");
            v
        })
        .collect();
    unflatten(&pack_bool_rows(&rows, width, N))
}

/// The scalar reference for [`pack_lanes`] (one bit per `set`, the
/// pre-ADR-0052 loop).
pub fn pack_lanes_scalar<const N: usize, V: AsRef<[bool]>>(vectors: &[V]) -> Vec<Lanes<N>> {
    assert!(vectors.len() <= Lanes::<N>::WIDTH);
    let width = vectors.first().map_or(0, |v| v.as_ref().len());
    let mut words = vec![Lanes::ZERO; width];
    for (lane, v) in vectors.iter().enumerate() {
        let v = v.as_ref();
        assert_eq!(v.len(), width, "lane {lane}: vector width mismatch");
        for (w, &bit) in words.iter_mut().zip(v) {
            w.set(lane, bit);
        }
    }
    words
}

/// Lane `lane` of every word, as a bit vector (the inverse of
/// [`pack_lanes`] for one lane).
pub fn unpack_lane<const N: usize>(words: &[Lanes<N>], lane: usize) -> Vec<bool> {
    words.iter().map(|w| w.get(lane)).collect()
}

/// Lanes `0..n_lanes` of `words`, one bit vector per lane: a bulk
/// [`unpack_lane`] through the SIMD bit-matrix transposes (ADR-0052).
///
/// # Panics
/// If `n_lanes` exceeds the lane width.
pub fn unpack_lanes<const N: usize>(words: &[Lanes<N>], n_lanes: usize) -> Vec<Vec<bool>> {
    if words.is_empty() {
        return vec![Vec::new(); n_lanes];
    }
    assert!(
        n_lanes <= Lanes::<N>::WIDTH,
        "{n_lanes} lanes exceed the {} lane width",
        Lanes::<N>::WIDTH
    );
    unpack_bool_rows(&flatten(words), N, words.len(), n_lanes)
}

/// Bytes per packed row of `width` bools.
fn row_bytes(width: usize) -> usize {
    width.div_ceil(8)
}

/// [`pack_lanes`] on flat words (non-generic: compiled here at this
/// crate's opt-level). A transpose of the `lanes x width` bit matrix:
///
/// 1. each row's bools pack 8 per byte (`bools_to_bits`, 32 per
///    `vpmovmskb`);
/// 2. per 64-lane word group, byte `g` of the 64 rows gathers into a
///    64-byte column block `g`;
/// 3. one `bytes_to_planes` call turns every column block into 8 words
///    (positions `8g..8g + 8`).
fn pack_bool_rows(rows: &[&[bool]], width: usize, n: usize) -> Vec<u64> {
    use theseus_core::simd::{bools_to_bits, bytes_to_planes};
    if width == 0 {
        return Vec::new();
    }
    let nb = row_bytes(width);
    let mut bits = vec![0u8; rows.len() * nb];
    for (row, out) in rows.iter().zip(bits.chunks_exact_mut(nb)) {
        bools_to_bits(row, out);
    }
    let mut words = vec![0u64; width * n];
    let mut cols = vec![0u8; nb * 64];
    let mut planes = vec![0u64; 8 * nb];
    for q in 0..n.min(rows.len().div_ceil(64)) {
        let group = &bits[64 * q * nb..(rows.len() * nb).min(64 * (q + 1) * nb)];
        cols.fill(0);
        for (i, row) in group.chunks_exact(nb).enumerate() {
            for (g, &byte) in row.iter().enumerate() {
                cols[64 * g + i] = byte;
            }
        }
        // planes[b * nb + g] = position 8g + b of lanes 64q..64q + 64.
        bytes_to_planes(&cols, &mut planes, nb, 8);
        for (p, word) in words.chunks_exact_mut(n).enumerate() {
            word[q] = planes[(p % 8) * nb + p / 8];
        }
    }
    words
}

/// [`unpack_lanes`] on flat words: [`pack_bool_rows`] run backwards
/// (`planes_to_bytes`, a byte scatter, `bits_to_bools`).
fn unpack_bool_rows(words: &[u64], n: usize, width: usize, n_lanes: usize) -> Vec<Vec<bool>> {
    use theseus_core::simd::{bits_to_bools, planes_to_bytes};
    if width == 0 {
        return vec![Vec::new(); n_lanes];
    }
    let nb = row_bytes(width);
    let mut bits = vec![0u8; n_lanes * nb];
    let mut cols = vec![0u8; nb * 64];
    let mut planes = vec![0u64; 8 * nb];
    for q in 0..n_lanes.div_ceil(64) {
        planes.fill(0);
        for (p, word) in words.chunks_exact(n).enumerate() {
            planes[(p % 8) * nb + p / 8] = word[q];
        }
        planes_to_bytes(&planes, nb, 8, &mut cols);
        let group = &mut bits[64 * q * nb..(n_lanes * nb).min(64 * (q + 1) * nb)];
        for (i, row) in group.chunks_exact_mut(nb).enumerate() {
            for (g, byte) in row.iter_mut().enumerate() {
                *byte = cols[64 * g + i];
            }
        }
    }
    let mut out = vec![vec![false; width]; n_lanes];
    for (row, src) in out.iter_mut().zip(bits.chunks_exact(nb)) {
        bits_to_bools(src, row);
    }
    out
}

// ── Timed lane simulator ────────────────────────────────────────────

/// Outcome of a successful lane-parallel timed run: [`SimResult`] with
/// one output word per port. The lane-independent fields are shared.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimResultLanes<const N: usize> {
    /// Decoded output tokens per output port, lane-wise.
    pub outputs: Vec<Lanes<N>>,
    /// As [`SimResult::ticks`].
    pub ticks: u32,
    /// As [`SimResult::occupancy`].
    pub occupancy: BTreeMap<(CellKind, u8), usize>,
    /// As [`SimResult::inv_token_checks`] (each check covers every lane).
    pub inv_token_checks: usize,
}

impl<const N: usize> SimResultLanes<N> {
    /// Lane `lane`'s run, in the scalar result shape.
    pub fn lane(&self, lane: usize) -> SimResult {
        SimResult {
            outputs: unpack_lane(&self.outputs, lane),
            ticks: self.ticks,
            occupancy: self.occupancy.clone(),
            inv_token_checks: self.inv_token_checks,
        }
    }
}

/// Why a lane-parallel timed run refused to start or aborted.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum LaneSimError {
    /// A lane-independent refusal (invalid netlist, input width, tick
    /// imbalance): every lane's scalar run reports exactly this.
    #[error(transparent)]
    Refused(#[from] SimError),
    /// A dynamic INV-TOKEN violation. `lane` is the lowest active lane
    /// failing the earliest failing check, and `error` is exactly what
    /// [`crate::sim::simulate_rails`] reports on that lane's input.
    #[error("lane {lane}: {error}")]
    Lane { lane: usize, error: SimError },
}

/// The largest cell arity ([`CellKind::arity`] of `K3`).
const MAX_ARITY: usize = 5;

/// [`crate::sim::simulate`] on [`Lanes::WIDTH`] input vectors at once:
/// `inputs[p]` holds input port `p`'s token for every lane
/// ([`pack_lanes`]). Lane `l` of the result equals `simulate` on lane
/// `l`'s inputs (`tests/sim_lanes.rs`); every lane is active.
pub fn simulate_lanes<const N: usize>(
    net: &PhasedCellNetlist,
    inputs: &[Lanes<N>],
) -> Result<SimResultLanes<N>, LaneSimError> {
    let rails: Vec<(Lanes<N>, Lanes<N>)> = inputs.iter().map(|&tok| (!tok, tok)).collect();
    simulate_rails_lanes(net, &rails, Lanes::ONES)
}

/// [`crate::sim::simulate_rails`] on many lanes: each input pair driven
/// raw as `(line0, line1)`, INV-TOKEN asserted on the `active` lanes only
/// (the rest may hold anything). The fault-injection entry point.
pub fn simulate_rails_lanes<const N: usize>(
    net: &PhasedCellNetlist,
    input_rails: &[(Lanes<N>, Lanes<N>)],
    active: Lanes<N>,
) -> Result<SimResultLanes<N>, LaneSimError> {
    const { assert!(N >= 1 && N <= MAX_WORDS, "lane blocks are 1..=8 words") };
    let plan = TimedPlan::new(net, input_rails.len())?;
    let line0: Vec<Lanes<N>> = input_rails.iter().map(|r| r.0).collect();
    let line1: Vec<Lanes<N>> = input_rails.iter().map(|r| r.1).collect();
    let outputs = plan.run_words(net, N, &flatten(&line0), &flatten(&line1), &active.0)?;
    Ok(SimResultLanes {
        outputs: unflatten(&outputs),
        ticks: plan.ticks,
        occupancy: net.occupancy(),
        inv_token_checks: plan.checks.len(),
    })
}

/// One cell of the timed plan: line indices resolved, no `Vec`s.
struct TimedCell {
    kind: CellKind,
    arity: usize,
    /// `line1` of each input pair (the decoded token, as the scalar sim
    /// reads it).
    ins: [u32; MAX_ARITY],
    outs: [RailPair; MAX_ARITY],
}

/// The [`TickSchedule`] flattened: tick `t` checks
/// `checks[check_at[t]..check_at[t + 1]]`, then fires
/// `cells[fire_at[t]..fire_at[t + 1]]`.
struct TimedPlan {
    ticks: u32,
    checks: Vec<RailPair>,
    check_at: Vec<usize>,
    cells: Vec<TimedCell>,
    fire_at: Vec<usize>,
}

impl TimedPlan {
    fn new(net: &PhasedCellNetlist, n_inputs: usize) -> Result<Self, SimError> {
        let sched = TickSchedule::new(net, n_inputs)?;
        let ticks = net.depth();
        let mut plan = TimedPlan {
            ticks,
            checks: Vec::new(),
            check_at: vec![0],
            cells: Vec::with_capacity(net.cells.len()),
            fire_at: vec![0],
        };
        for t in 0..=ticks {
            if let Some(pairs) = sched.pairs_at.get(&t) {
                plan.checks.extend_from_slice(pairs);
            }
            plan.check_at.push(plan.checks.len());
            for &ci in sched.cells_at.get(&t).into_iter().flatten() {
                let cell = &net.cells[ci];
                let mut ins = [0u32; MAX_ARITY];
                let mut outs = [cell.outputs[0]; MAX_ARITY];
                for (slot, p) in ins.iter_mut().zip(&cell.inputs) {
                    *slot = p.line1.0;
                }
                outs[..cell.outputs.len()].copy_from_slice(&cell.outputs);
                plan.cells.push(TimedCell {
                    kind: cell.kind,
                    arity: cell.inputs.len(),
                    ins,
                    outs,
                });
            }
            plan.fire_at.push(plan.cells.len());
        }
        Ok(plan)
    }

    /// The tick loop on `n`-word lane blocks, passed flat (non-generic, so
    /// it is compiled here whatever the caller's opt-level).
    fn run_words(
        &self,
        net: &PhasedCellNetlist,
        n: usize,
        line0: &[u64],
        line1: &[u64],
        active: &[u64],
    ) -> Result<Vec<u64>, LaneSimError> {
        with_words!(n, W => self.run_fixed::<W>(net, line0, line1, active))
    }

    fn run_fixed<const N: usize>(
        &self,
        net: &PhasedCellNetlist,
        line0: &[u64],
        line1: &[u64],
        active: &[u64],
    ) -> Result<Vec<u64>, LaneSimError> {
        let active = ld::<N>(active, 0);
        // val[line] = the line's lanes at its (single) valid tick,
        // meaningful once driven[line]. Drive the input pairs at tick 0.
        let n = net.num_lines as usize;
        let mut val = vec![Lanes::<N>::ZERO; n];
        let mut driven = vec![false; n];
        for (i, p) in net.inputs.iter().enumerate() {
            for (line, words) in [(p.line0, line0), (p.line1, line1)] {
                val[line.0 as usize] = ld(words, i);
                driven[line.0 as usize] = true;
            }
        }

        let mut ports = [Lanes::ZERO; MAX_ARITY];
        for t in 0..=self.ticks as usize {
            // 1. Assert INV-TOKEN on every pair valid this tick.
            for &p in &self.checks[self.check_at[t]..self.check_at[t + 1]] {
                check_pair_lanes(&val, &driven, p, t as u32, active)?;
            }
            // 2. Fire this tick's cells.
            for cell in &self.cells[self.fire_at[t]..self.fire_at[t + 1]] {
                let ports = &mut ports[..cell.arity];
                for (port, &line) in ports.iter_mut().zip(&cell.ins) {
                    *port = val[line as usize];
                }
                cell.kind.eval_lanes(ports);
                for (p, &v) in cell.outs.iter().zip(ports.iter()) {
                    val[p.line1.0 as usize] = v;
                    val[p.line0.0 as usize] = !v;
                    driven[p.line1.0 as usize] = true;
                    driven[p.line0.0 as usize] = true;
                }
            }
        }

        let outputs: Vec<Lanes<N>> = net
            .outputs
            .iter()
            .map(|p| val[p.line1.0 as usize])
            .collect();
        Ok(flatten(&outputs))
    }
}

/// The per-tick INV-TOKEN assertion for one rail pair across the active
/// lanes: both lines driven and `line0 ^ line1` set on every active lane.
fn check_pair_lanes<const N: usize>(
    val: &[Lanes<N>],
    driven: &[bool],
    pair: RailPair,
    tick: u32,
    active: Lanes<N>,
) -> Result<(), LaneSimError> {
    let (i0, i1) = (pair.line0.0 as usize, pair.line1.0 as usize);
    let (d0, d1) = (driven[i0], driven[i1]);
    let bad = if d0 && d1 {
        active & !(val[i0] ^ val[i1])
    } else {
        active
    };
    let Some(lane) = bad.lowest() else {
        return Ok(());
    };
    let at = |driven: bool, v: Lanes<N>| driven.then(|| v.get(lane));
    Err(LaneSimError::Lane {
        lane,
        error: SimError::InvToken {
            tick,
            pair,
            line0_high: at(d0, val[i0]),
            line1_high: at(d1, val[i1]),
        },
    })
}

// ── Compact (untimed) lane simulator ────────────────────────────────

/// A slot reference: index into the plan's slot file; the top bit means
/// "read negated" (the consumer names the producer's pair role-swapped).
type SlotRef = u32;
const NEG: SlotRef = 1 << 31;
const _: () = assert!(NEG == crate::simd::NEG, "one polarity bit");

#[inline]
fn read<const N: usize>(slots: &[u64], r: SlotRef) -> Lanes<N> {
    ld::<N>(slots, (r & !NEG) as usize).xor_word(0u64.wrapping_sub(u64::from(r >> 31)))
}

/// One computing cell: `controls` control refs then the target ref(s)
/// in `ins`; fresh target slots in `outs` (controls pass through as
/// aliases, so they need none).
#[derive(Clone, Debug)]
struct CompactCell {
    controls: u8,
    swap: bool,
    ins: [SlotRef; MAX_ARITY],
    outs: [u32; 2],
}

/// The untimed netlist as a flat, single-assignment slot program
/// (ADR-0048). Slots `0..n_inputs` are the input ports; every computing
/// cell writes one (`Rk`) or two (`Kk`) fresh slots. Built once per
/// netlist ([`CompactBalanced::simulate_lanes`] caches it).
#[derive(Clone, Debug)]
pub(crate) struct CompactPlan {
    n_inputs: usize,
    n_slots: usize,
    cells: Vec<CompactCell>,
    outputs: Vec<SlotRef>,
    /// The same cells in the branch-free encoding the SIMD kernels run
    /// (ADR-0052), over two extra slots: `n_slots` (all zeros; read
    /// negated it is the all-ones "no control") and `n_slots + 1` (the
    /// write-only sink for a flip's second output).
    uniform: UniformPlan,
}

impl CompactPlan {
    /// Resolve `net`'s cells, in vector (topological) order, into slots.
    ///
    /// # Panics
    /// If a cell reads a line no earlier cell or input drives -- where
    /// [`CompactBalanced::simulate`] panics too.
    pub(crate) fn new(net: &PhasedCellNetlist) -> Self {
        const UNSET: SlotRef = u32::MAX;
        // line_ref[line] = the slot holding the line's line1-polarity
        // value; NEG when this line is its pair's 0-line.
        let mut line_ref = vec![UNSET; net.num_lines as usize];
        let bind = |line_ref: &mut [SlotRef], p: &RailPair, r: SlotRef| {
            line_ref[p.line1.0 as usize] = r;
            line_ref[p.line0.0 as usize] = r ^ NEG;
        };
        for (i, p) in net.inputs.iter().enumerate() {
            bind(&mut line_ref, p, i as SlotRef);
        }
        let get = |line_ref: &[SlotRef], p: &RailPair| -> SlotRef {
            let r = line_ref[p.line1.0 as usize];
            assert_ne!(r, UNSET, "cell input line must be driven");
            r
        };

        let mut n_slots = net.inputs.len() as u32;
        let mut cells = Vec::new();
        for cell in &net.cells {
            let mut refs = [0 as SlotRef; MAX_ARITY];
            for (r, p) in refs.iter_mut().zip(&cell.inputs) {
                *r = get(&line_ref, p);
            }
            let (controls, swap) = match cell.kind.lane_op() {
                LaneOp::Pass => (None, false),
                LaneOp::Swap(k) => (Some(k), true),
                LaneOp::Flip(k) => (Some(k), false),
            };
            let mut out_refs = refs;
            if let Some(k) = controls {
                let n_targets = if swap { 2 } else { 1 };
                let mut outs = [0u32; 2];
                for (j, o) in outs.iter_mut().enumerate().take(n_targets) {
                    *o = n_slots;
                    out_refs[k + j] = n_slots;
                    n_slots += 1;
                }
                cells.push(CompactCell {
                    controls: k as u8,
                    swap,
                    ins: refs,
                    outs,
                });
            }
            for (p, &r) in cell.outputs.iter().zip(&out_refs) {
                bind(&mut line_ref, p, r);
            }
        }
        assert!(n_slots + 2 < NEG, "slot file exceeds 2^31 entries");

        let outputs = net.outputs.iter().map(|p| get(&line_ref, p)).collect();
        CompactPlan::assemble(net.inputs.len(), n_slots as usize, cells, outputs)
    }

    /// A plan from resolved cells, with its uniform re-encoding.
    fn assemble(
        n_inputs: usize,
        n_slots: usize,
        cells: Vec<CompactCell>,
        outputs: Vec<SlotRef>,
    ) -> Self {
        let zero = u32::try_from(n_slots).expect("slot file exceeds 2^31 entries");
        let uniform = cells.iter().map(|c| c.uniform(zero, zero + 1)).collect();
        CompactPlan {
            uniform: UniformPlan::new(uniform, n_slots + 2).level_order(),
            n_inputs,
            n_slots,
            cells,
            outputs,
        }
    }

    /// Cells the hot loop evaluates (every non-K0 cell).
    pub(crate) fn computing_cells(&self) -> usize {
        self.cells.len()
    }

    /// Evaluate on `n`-word lane blocks, passed flat (non-generic, so it
    /// is compiled here whatever the caller's opt-level): one block per
    /// input port in, one per output port out. `scratch` is reusable
    /// across calls: every slot is written before it is read.
    ///
    /// `kernel` picks the cell loop (ADR-0052); every choice computes the
    /// same words, and `Simd(level)` falls back to the reference loop for
    /// block widths without a kernel (`n` other than 2, 4, 8) or at
    /// `Scalar`.
    pub(crate) fn run_words(
        &self,
        n: usize,
        inputs: &[u64],
        scratch: &mut Vec<u64>,
        kernel: CompactKernel,
    ) -> Vec<u64> {
        assert_eq!(
            inputs.len(),
            self.n_inputs * n,
            "input width checked by caller"
        );
        let simd = match kernel {
            CompactKernel::Simd(level) if UniformPlan::has_kernel(level, n) => Some(level),
            _ => None,
        };
        let uniform = simd.is_some() || kernel == CompactKernel::UniformPortable;
        let n_slots = if uniform {
            self.uniform.n_slots()
        } else {
            self.n_slots
        };
        let slots = aligned_window(scratch, n_slots * n);
        slots[..inputs.len()].copy_from_slice(inputs);
        if uniform {
            // The all-zeros slot (read negated: the absent controls).
            slots[self.n_slots * n..][..n].fill(0);
        }
        match simd {
            Some(level) => {
                let ran = self.uniform.run(level, n, slots);
                debug_assert!(ran, "has_kernel({level}, {n})");
            }
            None if uniform => with_words!(n, W => self.uniform.run_portable::<W>(slots)),
            None => with_words!(n, W => self.run_fixed::<W>(slots)),
        }
        let mut out = vec![0u64; self.outputs.len() * n];
        for (block, &r) in out.chunks_exact_mut(n).zip(&self.outputs) {
            let neg = 0u64.wrapping_sub(u64::from(r >> 31));
            let slot = &slots[(r & !NEG) as usize * n..][..n];
            for (w, &s) in block.iter_mut().zip(slot) {
                *w = s ^ neg;
            }
        }
        out
    }

    /// The reference cell loop (ADR-0048): portable `[u64; N]` ops,
    /// branching per cell on the control count and swap-vs-flip.
    fn run_fixed<const N: usize>(&self, slots: &mut [u64]) {
        for c in &self.cells {
            let k = usize::from(c.controls);
            let mut s = Lanes::<N>::ONES;
            for &r in &c.ins[..k] {
                s &= read(slots, r);
            }
            if c.swap {
                let (a, b) = Lanes::cswap(s, read(slots, c.ins[k]), read(slots, c.ins[k + 1]));
                st(slots, c.outs[0] as usize, a);
                st(slots, c.outs[1] as usize, b);
            } else {
                let t = read::<N>(slots, c.ins[k]) ^ s;
                st(slots, c.outs[0] as usize, t);
            }
        }
    }
}

impl CompactCell {
    /// This cell in the branch-free uniform encoding (ADR-0052): absent
    /// controls read `zero | NEG` (all ones), and a flip `t ^= s` becomes
    /// the cswap of `t` with `!t` whose second output goes to `sink`.
    fn uniform(&self, zero: u32, sink: u32) -> UCell {
        let k = usize::from(self.controls);
        assert!(k <= 3, "a cell has at most 3 controls, got {k}");
        let mut refs = [zero | NEG; 5];
        refs[..k].copy_from_slice(&self.ins[..k]);
        refs[3] = self.ins[k];
        let outs = if self.swap {
            refs[4] = self.ins[k + 1];
            self.outs
        } else {
            refs[4] = self.ins[k] ^ NEG;
            [self.outs[0], sink]
        };
        UCell::new(refs, outs)
    }
}

/// `words` words of `scratch`, starting on a 64-byte boundary so an
/// 8-word slot is one cache line and a 4-word one never straddles two
/// (grows `scratch` as needed; the offset is recomputed per call).
fn aligned_window(scratch: &mut Vec<u64>, words: usize) -> &mut [u64] {
    if scratch.len() < words + 7 {
        scratch.resize(words + 7, 0);
    }
    let off = scratch.as_ptr().align_offset(64);
    let off = if off < 8 { off } else { 0 };
    &mut scratch[off..off + words]
}

/// Which compact-sim cell loop to run (ADR-0052). Every choice computes
/// the same result; the default ([`CompactBalanced::simulate_lanes_with`])
/// is `Simd(theseus_core::simd::level())`. Tests and benches pick one
/// explicitly.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompactKernel {
    /// The portable reference loop (ADR-0048): `[u64; N]` ops, branching
    /// per cell on the control count and swap-vs-flip.
    Reference,
    /// The branch-free uniform encoding, in portable Rust: isolates the
    /// encoding from the vector width.
    UniformPortable,
    /// The uniform encoding's explicit SSE2/AVX2 kernel at this level
    /// (capped at what the CPU has). `Scalar`, and block widths without
    /// a kernel (`N` other than 2, 4, 8), run [`Self::Reference`].
    Simd(SimdLevel),
}

/// Reusable slot file for [`CompactBalanced::simulate_lanes_with`].
#[derive(Clone, Debug, Default)]
pub struct LaneScratch(Vec<u64>);

impl CompactBalanced {
    /// [`Self::simulate_lanes_with`] with an explicit cell loop (tests and
    /// benches; ADR-0052).
    pub fn simulate_lanes_kernel<const N: usize>(
        &self,
        inputs: &[Lanes<N>],
        scratch: &mut LaneScratch,
        kernel: CompactKernel,
    ) -> Result<Vec<Lanes<N>>, CompactSimError> {
        const { assert!(N >= 1 && N <= MAX_WORDS, "lane blocks are 1..=8 words") };
        if inputs.len() != self.net().inputs.len() {
            return Err(CompactSimError::InputWidth {
                want: self.net().inputs.len(),
                got: inputs.len(),
            });
        }
        let outputs = self
            .lane_plan()
            .run_words(N, &flatten(inputs), &mut scratch.0, kernel);
        Ok(unflatten(&outputs))
    }

    /// [`CompactBalanced::simulate`] on [`Lanes::WIDTH`] input vectors at
    /// once: `inputs[p]` holds input port `p`'s token for every lane;
    /// returns one word per output port. Lane `l` equals `simulate` on
    /// lane `l`'s inputs (`tests/sim_lanes.rs`).
    pub fn simulate_lanes<const N: usize>(
        &self,
        inputs: &[Lanes<N>],
    ) -> Result<Vec<Lanes<N>>, CompactSimError> {
        self.simulate_lanes_with(inputs, &mut LaneScratch::default())
    }

    /// [`Self::simulate_lanes`] with caller-owned scratch, so a
    /// recirculating ring allocates its slot file once, not per
    /// revolution.
    pub fn simulate_lanes_with<const N: usize>(
        &self,
        inputs: &[Lanes<N>],
        scratch: &mut LaneScratch,
    ) -> Result<Vec<Lanes<N>>, CompactSimError> {
        self.simulate_lanes_kernel(inputs, scratch, CompactKernel::Simd(simd::level()))
    }

    /// Number of cells the lane simulator evaluates per pass: the active
    /// cells minus the K0 buffers it resolves to aliases.
    pub fn computing_cell_count(&self) -> usize {
        self.lane_plan().computing_cells()
    }
}

// ── Flat lane words ──────────────────────────────────────────────────
//
// The public entry points are generic over the block width `N`, but a
// generic function is compiled in the crate that instantiates it -- at
// *that* crate's opt-level (an unoptimized test binary ran the hot loop
// ~80x slower). So they cross into this crate as flat `u64` words, and
// `with_words!` instantiates the hot loops here, for every supported
// width.

/// Widest supported lane block: 8 words, 512 lanes.
const MAX_WORDS: usize = 8;

/// Run `$body` with the const `$W` bound to the runtime word count `$n`.
macro_rules! with_words {
    ($n:expr, $W:ident => $body:expr) => {
        match $n {
            1 => {
                const $W: usize = 1;
                $body
            }
            2 => {
                const $W: usize = 2;
                $body
            }
            3 => {
                const $W: usize = 3;
                $body
            }
            4 => {
                const $W: usize = 4;
                $body
            }
            5 => {
                const $W: usize = 5;
                $body
            }
            6 => {
                const $W: usize = 6;
                $body
            }
            7 => {
                const $W: usize = 7;
                $body
            }
            8 => {
                const $W: usize = 8;
                $body
            }
            n => unreachable!("{n}-word lane block (bounded at compile time)"),
        }
    };
}
use with_words;

/// Block `i` of flat `N`-word blocks.
#[inline]
fn ld<const N: usize>(words: &[u64], i: usize) -> Lanes<N> {
    Lanes(words[i * N..][..N].try_into().expect("N words"))
}

/// Store block `i` of flat `N`-word blocks.
#[inline]
fn st<const N: usize>(words: &mut [u64], i: usize, v: Lanes<N>) {
    words[i * N..][..N].copy_from_slice(&v.0);
}

fn flatten<const N: usize>(blocks: &[Lanes<N>]) -> Vec<u64> {
    blocks.iter().flat_map(|b| b.0).collect()
}

fn unflatten<const N: usize>(words: &[u64]) -> Vec<Lanes<N>> {
    (0..words.len() / N).map(|i| ld(words, i)).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::cell::LineId;

    #[test]
    fn lanes_basics() {
        let mut x = Lane::ZERO;
        assert_eq!(x.lowest(), None);
        x.set(200, true);
        x.set(70, true);
        assert!(x.get(70) && x.get(200) && !x.get(69));
        assert_eq!(x.lowest(), Some(70));
        assert_eq!((!x).lowest(), Some(0));
        x.set(70, false);
        assert_eq!(x.lowest(), Some(200));
        for n in [0, 1, 63, 64, 65, 255, 256] {
            let m = Lane::first(n);
            for l in 0..Lane::WIDTH {
                assert_eq!(m.get(l), l < n, "first({n}) lane {l}");
            }
        }
        let (a, b) = Lanes::cswap(
            Lanes::<1>([0b1100]),
            Lanes::<1>([0b1010]),
            Lanes::<1>([0b0110]),
        );
        assert_eq!((a, b), (Lanes([0b0110]), Lanes([0b1010])));
    }

    #[test]
    fn pack_unpack_round_trip() {
        let vectors: Vec<Vec<bool>> = (0..100u32)
            .map(|x| {
                (0..7)
                    .map(|i| (x.wrapping_mul(2654435761) >> i) & 1 == 1)
                    .collect()
            })
            .collect();
        let words = pack_lanes::<2, _>(&vectors);
        assert_eq!(words.len(), 7);
        assert_eq!(unpack_lanes(&words, vectors.len()), vectors);
        // Lanes past the packed vectors are 0.
        assert!(words.iter().all(|w| !w.get(127)));
    }

    // Like the scalar `inv_token_check_rejects_bad_pairs`: the check
    // itself, per lane, including the undriven case unreachable through
    // a validated netlist.
    #[test]
    fn inv_token_lane_check_names_the_lowest_bad_lane() {
        let pair = RailPair {
            line0: LineId(0),
            line1: LineId(1),
        };
        let one_hot = Lanes::<1>([0b0110]);
        let val = [!one_hot, one_hot];
        assert_eq!(
            check_pair_lanes(&val, &[true, true], pair, 3, Lanes::ONES),
            Ok(())
        );
        // Lanes 2 and 5 both high on both lines; 5 inactive -> report 2.
        let mut bad = val;
        bad[0].set(2, true);
        bad[0].set(5, true);
        let err = check_pair_lanes(&bad, &[true, true], pair, 3, Lanes::first(5));
        assert_eq!(
            err,
            Err(LaneSimError::Lane {
                lane: 2,
                error: SimError::InvToken {
                    tick: 3,
                    pair,
                    line0_high: Some(true),
                    line1_high: Some(true),
                },
            })
        );
        // Masked out entirely: fine.
        assert_eq!(
            check_pair_lanes(&bad, &[true, true], pair, 3, Lanes([!0b100100])),
            Ok(())
        );
        // Undriven fails on the first active lane.
        let err = check_pair_lanes(&val, &[false, true], pair, 0, Lanes([0b1000]));
        assert_eq!(
            err,
            Err(LaneSimError::Lane {
                lane: 3,
                error: SimError::InvToken {
                    tick: 0,
                    pair,
                    line0_high: None,
                    line1_high: Some(false),
                },
            })
        );
    }
}

/// ADR-0052: every compact-sim kernel against the reference loop, and
/// the SIMD-backed pack/unpack against the per-bit loops.
#[cfg(test)]
mod kernel_tests {
    use super::*;

    /// splitmix64 (fixed seed: failures reproduce).
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

    /// A random single-assignment plan: `cells` cells over `n_inputs`
    /// inputs, each reading earlier slots (random polarity) with 0..=3
    /// controls, swap or flip; `n_out` random output refs.
    fn random_plan(rng: &mut Rng, n_inputs: usize, cells: usize, n_out: usize) -> CompactPlan {
        let mut n_slots = n_inputs as u32;
        let mut plan_cells = Vec::with_capacity(cells);
        for i in 0..cells {
            // Cycle the shapes so every (controls, swap) pair shows up.
            let controls = (i % 4) as u8;
            let swap = (i / 4) % 2 == 0 || rng.below(3) == 0;
            let mut ins = [0; MAX_ARITY];
            for r in ins.iter_mut().take(usize::from(controls) + 2) {
                *r = rng.below(n_slots as usize) as u32 | if rng.below(2) == 0 { NEG } else { 0 };
            }
            let outs = [n_slots, n_slots + 1];
            n_slots += if swap { 2 } else { 1 };
            plan_cells.push(CompactCell {
                controls,
                swap,
                ins,
                outs,
            });
        }
        let outputs = (0..n_out)
            .map(|_| rng.below(n_slots as usize) as u32 | if rng.below(2) == 0 { NEG } else { 0 })
            .collect();
        CompactPlan::assemble(n_inputs, n_slots as usize, plan_cells, outputs)
    }

    fn kernels() -> Vec<CompactKernel> {
        let mut k = vec![CompactKernel::Reference, CompactKernel::UniformPortable];
        k.extend(SimdLevel::available().into_iter().map(CompactKernel::Simd));
        k
    }

    #[test]
    fn every_kernel_matches_the_reference() {
        let mut rng = Rng(0x51D_0052);
        let shapes = [(1, 0, 1), (3, 1, 2), (4, 4, 3), (8, 37, 5), (20, 300, 16)];
        // Miri: the small shapes only.
        let shapes = if cfg!(miri) {
            &shapes[..4]
        } else {
            &shapes[..]
        };
        for &(n_inputs, cells, n_out) in shapes {
            let plan = random_plan(&mut rng, n_inputs, cells, n_out);
            for n in 1..=MAX_WORDS {
                let inputs: Vec<u64> = (0..n_inputs * n).map(|_| rng.next()).collect();
                let want = plan.run_words(n, &inputs, &mut Vec::new(), CompactKernel::Reference);
                assert_eq!(want.len(), n_out * n);
                // One scratch across kernels: stale slots must not leak.
                let mut scratch = vec![u64::MAX; 3];
                for kernel in kernels() {
                    let got = plan.run_words(n, &inputs, &mut scratch, kernel);
                    assert_eq!(got, want, "{kernel:?}, N = {n}, {cells} cells");
                }
            }
        }
    }

    /// Exhaustive over one cell of each shape: every (controls, swap)
    /// pair and every polarity pattern of its refs, at the widest block.
    #[test]
    fn every_cell_shape_and_polarity() {
        let mut rng = Rng(0xCE11);
        for controls in 0..=3u8 {
            for swap in [false, true] {
                let n_refs = usize::from(controls) + if swap { 2 } else { 1 };
                for pol in 0..1u32 << n_refs {
                    let mut ins = [0; MAX_ARITY];
                    for (j, r) in ins.iter_mut().take(n_refs).enumerate() {
                        *r = j as u32 | if (pol >> j) & 1 == 1 { NEG } else { 0 };
                    }
                    let cell = CompactCell {
                        controls,
                        swap,
                        ins,
                        outs: [5, 6],
                    };
                    let n_slots = if swap { 7 } else { 6 };
                    let outputs = (0..n_slots).collect();
                    let plan = CompactPlan::assemble(5, n_slots as usize, vec![cell], outputs);
                    let inputs: Vec<u64> = (0..5 * MAX_WORDS).map(|_| rng.next()).collect();
                    let want = plan.run_words(
                        MAX_WORDS,
                        &inputs,
                        &mut Vec::new(),
                        CompactKernel::Reference,
                    );
                    for kernel in kernels() {
                        let got = plan.run_words(MAX_WORDS, &inputs, &mut Vec::new(), kernel);
                        assert_eq!(
                            got, want,
                            "{kernel:?}: {controls} controls, swap {swap}, pol {pol:b}"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn pack_unpack_match_the_bit_loops() {
        let mut rng = Rng(0xBACC);
        let (widths, lanes): (&[usize], &[usize]) = if cfg!(miri) {
            (&[0, 1, 9, 65], &[0, 1, 65, 130])
        } else {
            (
                &[0, 1, 7, 8, 9, 63, 64, 65, 200, 686],
                &[0, 1, 31, 64, 65, 200, 256],
            )
        };
        for &width in widths {
            for &n_lanes in lanes {
                let vectors: Vec<Vec<bool>> = (0..n_lanes)
                    .map(|_| (0..width).map(|_| rng.next() & 1 == 1).collect())
                    .collect();
                let want = pack_lanes_scalar::<4, _>(&vectors);
                assert_eq!(pack_lanes::<4, _>(&vectors), want, "{width} x {n_lanes}");
                let per_lane: Vec<Vec<bool>> =
                    (0..n_lanes).map(|l| unpack_lane(&want, l)).collect();
                assert_eq!(per_lane, vectors);
                assert_eq!(unpack_lanes(&want, n_lanes), vectors, "{width} x {n_lanes}");
                if n_lanes <= 128 {
                    let want2 = pack_lanes_scalar::<2, _>(&vectors);
                    assert_eq!(pack_lanes::<2, _>(&vectors), want2);
                    assert_eq!(unpack_lanes(&want2, n_lanes), vectors);
                }
                if n_lanes <= 64 {
                    let want1 = pack_lanes_scalar::<1, _>(&vectors);
                    assert_eq!(pack_lanes::<1, _>(&vectors), want1);
                }
                let want8 = pack_lanes_scalar::<8, _>(&vectors);
                assert_eq!(pack_lanes::<8, _>(&vectors), want8);
                assert_eq!(unpack_lanes(&want8, n_lanes), vectors);
            }
        }
    }
}
