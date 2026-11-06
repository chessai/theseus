//! TISC-1 ring runner: recirculate one compact-balanced `step` R times
//! (ADR-0033, M4-v2).
//!
//! [`step_ring`] lowers and compact-balances `step(rom)` without
//! materializing the ~1.6 M K0 buffers (uses [`balance_compact`]).
//! [`StepRing::run`] recirculates via [`CompactBalanced::simulate_lanes`].
//! The differential oracle is `eval(step)^R`.
//!
//! # Many starts at once (ADR-0048)
//!
//! The ROM is baked into the netlist, so runs from different start
//! states differ only in their input bits. Every runner here is
//! lane-parallel: `run_many` recirculates up to [`Lane::WIDTH`] start
//! states per pass, and the single-start `run` is the same code with
//! one lane. The scalar [`CompactBalanced::simulate`] remains the
//! reference the lane simulator is checked against
//! (`theseus-netlist/tests/sim_lanes.rs`), and the ancilla / bus probes
//! below still use it.
//!
//! # Ancilla restoration -- the zero-garbage ring
//!
//! True ring-machine recirculation (feeding ancilla outputs back as
//! inputs) is valid when the step's lowering restores its ancillas
//! after one revolution. [`StepRing::ancillas_restored`] checks this.
//!
//! **As of ADR-0040 phase C** (R1-based `emit_mcnot` rewrite), the
//! plain lowering IS ancilla-clean: `ancillas_restored == true` on all
//! sampled states, with ~468 ancilla lines (down from 1138 under the
//! old splitter/selector synthesis). [`StepRing::run`] is therefore the
//! **default, zero-garbage runner**: it feeds ancilla outputs back as
//! the next revolution's inputs (TRUE recirculation), so the ring
//! physically closes with **zero per-revolution boundary** -- nothing is
//! re-supplied, nothing exits. It equals `eval(step)^R`, and equals
//! [`StepRing::run_open`] (the open-loop variant that re-supplies fresh
//! inits) -- that equivalence is what certifies the closure. This is the
//! literal zero-garbage ring.
//!
//! # Closed ring (Bennett path -- fallback)
//!
//! [`closed_step_ring`] lowers the step via
//! [`lower_iso_bennett`] (compute-copy-uncompute, ADR-0033): the
//! wrap restores EVERY forward ancilla to its init, data-independently,
//! and concentrates the irreducible parity residue into a fixed
//! boundary of `2·w` slots (per data bit `j`: one slot carrying `y_j`
//! against init `1`, one carrying the regenerated `x_j` against init
//! `0`). [`ClosedStepRing::run`] therefore recirculates by feeding back
//! the State **and all forward-ancilla outputs** as the next
//! revolution's inputs, re-supplying only the fixed `(1, 0)` boundary
//! pairs -- a *physically closing* ring whose per-revolution external
//! interface is a constant-size, data-independent set of ports
//! ([`ClosedStepRing::boundary_size`], `2·w = 320` for the TISC-1
//! State). With the plain path now ancilla-clean, the Bennett
//! [`ClosedStepRing`] demotes to a **fallback** for circuits whose
//! lowering is still K-only-dirty (e.g. trace tag duplication, iter/fold
//! control chains -- genuine fan-outs that still emit K-only garbage).

use theseus_netlist::ir::cell::CellNetError;
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{
    balance_compact, bits_of, lower_iso_bennett, pack_lanes, unpack_lanes, CompactBalanced, Lane,
    LaneScratch, Lanes, PHASE_COUNT,
};

use crate::step::{step, system_step};
use crate::types::{
    decode_system_bits, encode_system, encode_system_bits, mem_state_ty, state_ty, system_state_ty,
    StateVals, COMPOSITE_BITS, MEM_WORDS, NUM_BUS_FIELDS, STATE_BITS, SYSTEM_BITS, W,
};

// ── Lane-parallel recirculation (ADR-0048) ─────────────────────────

/// How a single-body ring marshals [`StateVals`] to its carrier's flat
/// bits (the `bits_of` image of the carrier value, built directly).
struct Carrier {
    encode: fn(&StateVals) -> Vec<bool>,
    decode: fn(&[bool]) -> StateVals,
}

impl Carrier {
    /// The CPU-only `state_ty()` carrier.
    const CPU: Carrier = Carrier {
        encode: StateVals::encode_bits,
        decode: StateVals::decode_bits,
    };
    /// The `((CpuState, MemState), BusMsg)` carrier.
    const SYSTEM: Carrier = Carrier {
        encode: encode_system_bits,
        decode: decode_system_bits,
    };

    /// One lane per start state, one word per carrier bit.
    fn pack<const N: usize>(&self, starts: &[StateVals]) -> Vec<Lanes<N>> {
        let bits: Vec<Vec<bool>> = starts.iter().map(self.encode).collect();
        pack_lanes(&bits)
    }

    /// Lanes `0..n_starts` of `data` back to states (one bulk SIMD
    /// transpose, ADR-0052).
    fn unpack<const N: usize>(&self, data: &[Lanes<N>], n_starts: usize) -> Vec<StateVals> {
        unpack_lanes(data, n_starts)
            .iter()
            .map(|bits| (self.decode)(bits))
            .collect()
    }
}

/// Run `starts` through `run` in [`Lane::WIDTH`]-sized blocks.
fn in_blocks(starts: &[StateVals], run: impl Fn(&[StateVals]) -> Vec<StateVals>) -> Vec<StateVals> {
    starts.chunks(Lane::WIDTH).flat_map(run).collect()
}

/// Every lane `bits[j]`, per `j`.
fn splat_bits<const N: usize>(bits: &[bool]) -> Vec<Lanes<N>> {
    bits.iter().map(|&b| Lanes::splat(b)).collect()
}

/// Recirculate one compact body `revolutions` times over every lane of
/// `data` (one word per data port). Each revolution feeds back the data
/// outputs and the first `n_feedback` ancilla outputs (TRUE
/// recirculation); the remaining ancillas are re-supplied from
/// `ancilla_init`. Returns the final data words.
fn recirculate<const N: usize>(
    compact: &CompactBalanced,
    data: Vec<Lanes<N>>,
    ancilla_init: &[bool],
    n_feedback: usize,
    revolutions: u32,
) -> Vec<Lanes<N>> {
    let n_data = data.len();
    let mut input = data;
    input.extend(splat_bits::<N>(ancilla_init));
    let mut scratch = LaneScratch::default();
    for _ in 0..revolutions {
        let out = compact
            .simulate_lanes_with(&input, &mut scratch)
            .expect("simulate");
        input[..n_data + n_feedback].copy_from_slice(&out[..n_data + n_feedback]);
    }
    input.truncate(n_data);
    input
}

/// The lowest of lanes `0..n_starts` whose ancilla outputs, after one
/// revolution with fresh inits, differ from those inits -- with the
/// differing ancilla indices.
fn first_unrestored<const N: usize>(
    compact: &CompactBalanced,
    data: &[Lanes<N>],
    ancilla_init: &[bool],
    n_starts: usize,
) -> Option<(usize, Vec<usize>)> {
    let mut input = data.to_vec();
    input.extend(splat_bits::<N>(ancilla_init));
    let out = compact.simulate_lanes(&input).expect("simulate");
    let active = Lanes::first(n_starts);
    let diffs: Vec<Lanes<N>> = out[data.len()..]
        .iter()
        .zip(ancilla_init)
        .map(|(&o, &init)| (o ^ Lanes::splat(init)) & active)
        .collect();
    let lane = diffs.iter().fold(Lanes::ZERO, |acc, &d| acc | d).lowest()?;
    let mismatches = (0..diffs.len()).filter(|&j| diffs[j].get(lane)).collect();
    Some((lane, mismatches))
}

/// A compact-balanced TISC-1 step ready for ring recirculation.
pub struct StepRing {
    compact: CompactBalanced,
    ancilla_init: Vec<bool>,
    n_data: usize,
}

/// Build a ring-ready compact-balanced step from a ROM image.
///
/// Calls `step(rom)`, lowers with ancillas, and compact-balances --
/// O(active cells) memory, no K0 materialization.
pub fn step_ring(rom: &[u32]) -> StepRing {
    let iso = step(rom);
    let (net, ancilla_init) = lower_iso_with_ancillas(&iso).expect("step lowers");
    assert_eq!(
        net.inputs.len(),
        net.outputs.len(),
        "step netlist must have equal input/output port counts"
    );
    let n_data = net.inputs.len() - ancilla_init.len();
    let compact = balance_compact(&net).expect("balance_compact");
    StepRing {
        compact,
        ancilla_init,
        n_data,
    }
}

impl StepRing {
    /// Recirculate the step `revolutions` times starting from `start`,
    /// with the ring CLOSED -- the **default, zero-garbage** runner
    /// (ADR-0040 phase C).
    ///
    /// Each revolution feeds the ancilla *outputs* back as the next
    /// revolution's ancilla *inputs* (TRUE recirculation): nothing is
    /// re-supplied, nothing exits -- the ring physically closes with
    /// **zero per-revolution boundary**. This is valid because the
    /// R1-based lowering is ancilla-clean ([`Self::ancillas_restored`]
    /// is true), so the fed-back ancillas equal their inits every
    /// revolution; the `debug_assert!` below catches any regression that
    /// would make feed-back diverge from a fresh re-supply.
    ///
    /// For a lowering that is *not* ancilla-clean (e.g. K-only-dirty
    /// fan-outs), use [`Self::run_open`] (re-supply fresh inits) or the
    /// Bennett [`ClosedStepRing`] fallback instead.
    pub fn run(&self, start: &StateVals, revolutions: u32) -> StateVals {
        self.run_lanes::<1>(std::slice::from_ref(start), revolutions, true)
            .remove(0)
    }

    /// [`Self::run`] from many start states at once, [`Lane::WIDTH`] per
    /// pass (ADR-0048): result `i` is `run(&starts[i], revolutions)`.
    pub fn run_many(&self, starts: &[StateVals], revolutions: u32) -> Vec<StateVals> {
        in_blocks(starts, |block| {
            self.run_lanes::<4>(block, revolutions, true)
        })
    }

    /// Recirculate with the ring OPEN: each revolution discards the
    /// ancilla outputs and re-supplies fresh `ancilla_init`.
    ///
    /// Always correct (independent of ancilla cleanliness); equals
    /// [`Self::run`] exactly when the lowering is ancilla-clean, which is
    /// the equivalence that certifies the zero-garbage closure. Retained
    /// as the robust fallback for non-ancilla-clean lowerings.
    pub fn run_open(&self, start: &StateVals, revolutions: u32) -> StateVals {
        self.run_lanes::<1>(std::slice::from_ref(start), revolutions, false)
            .remove(0)
    }

    /// [`Self::run_open`] from many start states at once (ADR-0048).
    pub fn run_open_many(&self, starts: &[StateVals], revolutions: u32) -> Vec<StateVals> {
        in_blocks(starts, |block| {
            self.run_lanes::<4>(block, revolutions, false)
        })
    }

    fn run_lanes<const N: usize>(
        &self,
        starts: &[StateVals],
        revolutions: u32,
        closed: bool,
    ) -> Vec<StateVals> {
        let data = Carrier::CPU.pack::<N>(starts);
        assert_eq!(data.len(), self.n_data);
        if closed && cfg!(debug_assertions) {
            if let Some((i, mismatches)) =
                first_unrestored(&self.compact, &data, &self.ancilla_init, starts.len())
            {
                panic!(
                    "StepRing::run assumes an ancilla-clean lowering (zero-garbage \
                     ring); this lowering leaves garbage -- use run_open or \
                     ClosedStepRing. start {i} mismatches: {mismatches:?}"
                );
            }
        }
        let n_feedback = if closed { self.ancilla_init.len() } else { 0 };
        let data = recirculate(
            &self.compact,
            data,
            &self.ancilla_init,
            n_feedback,
            revolutions,
        );
        Carrier::CPU.unpack(&data, starts.len())
    }

    /// Indices of ancilla lines whose output differs from their init
    /// after one revolution from `start`.
    pub fn ancilla_mismatches(&self, start: &StateVals) -> Vec<usize> {
        let ty = state_ty();
        let data_bits = bits_of(&start.encode(), &ty).expect("bits_of");
        let mut input = data_bits;
        input.extend_from_slice(&self.ancilla_init);
        let sim = self.compact.simulate(&input).expect("simulate");
        let ancilla_out = &sim.outputs[self.n_data..];

        ancilla_out
            .iter()
            .zip(&self.ancilla_init)
            .enumerate()
            .filter(|(_, (got, expected))| got != expected)
            .map(|(i, _)| i)
            .collect()
    }

    /// True if ancillas are fully restored after one revolution from
    /// `start` -- the ring machine genuinely closes.
    pub fn ancillas_restored(&self, start: &StateVals) -> bool {
        self.ancilla_mismatches(start).is_empty()
    }

    /// The compact-balanced body.
    pub fn compact(&self) -> &CompactBalanced {
        &self.compact
    }

    /// Ancilla init values, in netlist input order after the data bits.
    pub fn ancilla_init(&self) -> &[bool] {
        &self.ancilla_init
    }

    /// Number of ancilla lines.
    pub fn ancilla_count(&self) -> usize {
        self.ancilla_init.len()
    }
}

// ── Closed ring (Bennett-wrapped step) ─────────────────────────────

/// A compact-balanced Bennett-wrapped TISC-1 step: forward ancillas
/// are restored every revolution (fed back), and only the fixed
/// `2·w`-slot boundary is re-supplied -- the physically closing ring.
pub struct ClosedStepRing {
    compact: CompactBalanced,
    /// Full ancilla init vector: forward inits ++ `[1, 0]` per data bit.
    ancilla_init: Vec<bool>,
    /// State width in rail pairs (`w`).
    n_data: usize,
    /// Forward ancilla lines (fed back each revolution).
    n_forward: usize,
}

/// Build a closed ring from a ROM image: `step(rom)` lowered via the
/// Bennett path ([`lower_iso_bennett`]), then compact-balanced.
///
/// The wrapped netlist's ancilla sections are, positionally:
/// `[0, n_forward)` the forward ancillas (restored to init after every
/// revolution, data-independently) and `[n_forward, n_forward + 2·w)`
/// the boundary pairs with fixed inits `(1, 0)` per data bit.
pub fn closed_step_ring(rom: &[u32]) -> ClosedStepRing {
    let iso = step(rom);
    let (net, ancilla_init) = lower_iso_bennett(&iso).expect("step lowers via bennett");
    assert_eq!(
        net.inputs.len(),
        net.outputs.len(),
        "wrapped step netlist must have equal input/output port counts"
    );
    let n_data = net.inputs.len() - ancilla_init.len();
    assert!(
        ancilla_init.len() >= 2 * n_data,
        "bennett wrap must append 2·w boundary slots"
    );
    let n_forward = ancilla_init.len() - 2 * n_data;
    // The boundary inits are the fixed, data-independent (1, 0) pairs.
    for j in 0..n_data {
        assert!(
            ancilla_init[n_forward + 2 * j] && !ancilla_init[n_forward + 2 * j + 1],
            "boundary pair {j} init must be (1, 0)"
        );
    }
    let compact = balance_compact(&net).expect("balance_compact");
    ClosedStepRing {
        compact,
        ancilla_init,
        n_data,
        n_forward,
    }
}

impl ClosedStepRing {
    /// Recirculate the step `revolutions` times starting from `start`,
    /// with the ring CLOSED: each revolution feeds back the State bits
    /// **and every forward-ancilla output** as the next revolution's
    /// inputs; only the fixed `2·w` boundary slots are re-supplied
    /// with their constant `(1, 0)` inits. Nothing data-dependent is
    /// injected after the start state -- the differential oracle
    /// `run(s, R) == eval(step)^R` therefore certifies that the
    /// forward ancillas really do come back clean every revolution.
    pub fn run(&self, start: &StateVals, revolutions: u32) -> StateVals {
        self.run_lanes::<1>(std::slice::from_ref(start), revolutions)
            .remove(0)
    }

    /// [`Self::run`] from many start states at once (ADR-0048).
    pub fn run_many(&self, starts: &[StateVals], revolutions: u32) -> Vec<StateVals> {
        in_blocks(starts, |block| self.run_lanes::<4>(block, revolutions))
    }

    fn run_lanes<const N: usize>(&self, starts: &[StateVals], revolutions: u32) -> Vec<StateVals> {
        let data = Carrier::CPU.pack::<N>(starts);
        assert_eq!(data.len(), self.n_data);
        // Feed the forward ancillas back -- TRUE recirculation. Boundary
        // outputs (the Bennett residue) exit the ring; fresh (1, 0) pairs
        // are re-supplied every revolution.
        let data = recirculate(
            &self.compact,
            data,
            &self.ancilla_init,
            self.n_forward,
            revolutions,
        );
        Carrier::CPU.unpack(&data, starts.len())
    }

    /// Indices (into the forward-ancilla section) of forward ancilla
    /// lines whose output differs from their init after one revolution
    /// from `start`. Empty ⇔ the ring genuinely closes at `start`
    /// (the Bennett wrap guarantees empty for EVERY state).
    pub fn forward_mismatches(&self, start: &StateVals) -> Vec<usize> {
        let sim_out = self.revolve_once(start);
        sim_out[self.n_data..self.n_data + self.n_forward]
            .iter()
            .zip(&self.ancilla_init[..self.n_forward])
            .enumerate()
            .filter(|(_, (got, expected))| got != expected)
            .map(|(i, _)| i)
            .collect()
    }

    /// The boundary outputs after one revolution from `start`: `2·w`
    /// bits, interleaved `(y_0, x_0, y_1, x_1, ...)` -- the output word
    /// and the regenerated input word, the honest Bennett residue.
    pub fn boundary_residue(&self, start: &StateVals) -> Vec<bool> {
        let sim_out = self.revolve_once(start);
        sim_out[self.n_data + self.n_forward..].to_vec()
    }

    fn revolve_once(&self, start: &StateVals) -> Vec<bool> {
        let ty = state_ty();
        let data_bits = bits_of(&start.encode(), &ty).expect("bits_of");
        let mut input = data_bits;
        input.extend_from_slice(&self.ancilla_init);
        self.compact.simulate(&input).expect("simulate").outputs
    }

    /// State width in rail pairs (`w`).
    pub fn data_width(&self) -> usize {
        self.n_data
    }

    /// Forward ancilla lines -- fed back, never re-supplied.
    pub fn forward_ancilla_count(&self) -> usize {
        self.n_forward
    }

    /// Size of the fixed boundary re-supplied each revolution:
    /// `2·w` slots with constant inits `(1, 0)` per data bit --
    /// data-independent ports, the ring's only external interface.
    pub fn boundary_size(&self) -> usize {
        2 * self.n_data
    }

    /// Total ancilla lines (forward + boundary).
    pub fn ancilla_count(&self) -> usize {
        self.ancilla_init.len()
    }

    /// The compact-balanced body.
    pub fn compact(&self) -> &CompactBalanced {
        &self.compact
    }
}

// ── System ring (system_step carrier, Stage 1b-B) ─────────────────

/// A compact-balanced TISC-1 `system_step` ready for ring recirculation.
///
/// Like [`StepRing`] but over the 3-factor `((CpuState, MemState), BusMsg)`
/// carrier, using [`encode_system`]/[`decode_system`](crate::types::decode_system) for I/O.
/// The bus-quiescence boundary condition is checked via
/// [`SystemRing::bus_quiescent`].
pub struct SystemRing {
    compact: CompactBalanced,
    ancilla_init: Vec<bool>,
    n_data: usize,
}

/// Build a ring-ready compact-balanced system_step from a ROM image.
///
/// Calls `system_step(rom)`, lowers with ancillas, and compact-balances --
/// O(active cells) memory, no K0 materialization.
pub fn system_ring(rom: &[u32]) -> SystemRing {
    let iso = system_step(rom);
    let (net, ancilla_init) = lower_iso_with_ancillas(&iso).expect("system_step lowers");
    assert_eq!(
        net.inputs.len(),
        net.outputs.len(),
        "system_step netlist must have equal input/output port counts"
    );
    let n_data = net.inputs.len() - ancilla_init.len();
    let compact = balance_compact(&net).expect("balance_compact");
    SystemRing {
        compact,
        ancilla_init,
        n_data,
    }
}

impl SystemRing {
    /// Recirculate the system_step `revolutions` times starting from
    /// `start`, with the ring CLOSED (zero-garbage).
    ///
    /// Each revolution feeds the ancilla *outputs* back as the next
    /// revolution's ancilla *inputs* (TRUE recirculation). This is
    /// valid because `system_step` lowers ancilla-clean by construction.
    ///
    /// Unlike [`StepRing::run`], this method does NOT contain a
    /// `debug_assert!(ancillas_restored)` -- the certificate test
    /// asserts restoration explicitly, and the internal assert would
    /// panic before the certificate can observe `run==run_open` /
    /// `bus_quiescent`, and it doubles per-call cost.
    pub fn run(&self, start: &StateVals, revolutions: u32) -> StateVals {
        self.run_lanes::<1>(std::slice::from_ref(start), revolutions, true)
            .remove(0)
    }

    /// [`Self::run`] from many start states at once, [`Lane::WIDTH`] per
    /// pass (ADR-0048): result `i` is `run(&starts[i], revolutions)`.
    pub fn run_many(&self, starts: &[StateVals], revolutions: u32) -> Vec<StateVals> {
        in_blocks(starts, |block| {
            self.run_lanes::<4>(block, revolutions, true)
        })
    }

    /// Recirculate with the ring OPEN: each revolution discards the
    /// ancilla outputs and re-supplies fresh `ancilla_init`.
    ///
    /// Always correct (independent of ancilla cleanliness); equals
    /// [`Self::run`] exactly when the lowering is ancilla-clean, which
    /// is the equivalence that certifies the zero-garbage closure.
    pub fn run_open(&self, start: &StateVals, revolutions: u32) -> StateVals {
        self.run_lanes::<1>(std::slice::from_ref(start), revolutions, false)
            .remove(0)
    }

    /// [`Self::run_open`] from many start states at once (ADR-0048).
    pub fn run_open_many(&self, starts: &[StateVals], revolutions: u32) -> Vec<StateVals> {
        in_blocks(starts, |block| {
            self.run_lanes::<4>(block, revolutions, false)
        })
    }

    fn run_lanes<const N: usize>(
        &self,
        starts: &[StateVals],
        revolutions: u32,
        closed: bool,
    ) -> Vec<StateVals> {
        let data = Carrier::SYSTEM.pack::<N>(starts);
        assert_eq!(data.len(), self.n_data);
        let n_feedback = if closed { self.ancilla_init.len() } else { 0 };
        let data = recirculate(
            &self.compact,
            data,
            &self.ancilla_init,
            n_feedback,
            revolutions,
        );
        Carrier::SYSTEM.unpack(&data, starts.len())
    }

    /// Indices of ancilla lines whose output differs from their init
    /// after one revolution from `start`.
    pub fn ancilla_mismatches(&self, start: &StateVals) -> Vec<usize> {
        let ty = system_state_ty();
        let data_bits = bits_of(&encode_system(start), &ty).expect("bits_of");
        let mut input = data_bits;
        input.extend_from_slice(&self.ancilla_init);
        let sim = self.compact.simulate(&input).expect("simulate");
        let ancilla_out = &sim.outputs[self.n_data..];

        ancilla_out
            .iter()
            .zip(&self.ancilla_init)
            .enumerate()
            .filter(|(_, (got, expected))| got != expected)
            .map(|(i, _)| i)
            .collect()
    }

    /// True if ancillas are fully restored after one revolution from
    /// `start` -- the ring machine genuinely closes.
    pub fn ancillas_restored(&self, start: &StateVals) -> bool {
        self.ancilla_mismatches(start).is_empty()
    }

    /// True if the BusMsg data lines are all zero after one revolution
    /// from `start` -- the bus-quiescence boundary condition.
    ///
    /// Computes the bus range symbolically as
    /// `COMPOSITE_BITS..SYSTEM_BITS` and asserts `n_data == SYSTEM_BITS`
    /// first (catches lowering-order regressions loudly).
    pub fn bus_quiescent(&self, start: &StateVals) -> bool {
        assert_eq!(
            self.n_data, SYSTEM_BITS,
            "SystemRing::bus_quiescent: n_data ({}) != SYSTEM_BITS ({}) -- \
             lowering-order regression",
            self.n_data, SYSTEM_BITS,
        );
        let ty = system_state_ty();
        let data_bits = bits_of(&encode_system(start), &ty).expect("bits_of");
        let mut input = data_bits;
        input.extend_from_slice(&self.ancilla_init);
        let sim = self.compact.simulate(&input).expect("simulate");
        let bus_out = &sim.outputs[COMPOSITE_BITS..SYSTEM_BITS];
        bus_out.iter().all(|&b| !b)
    }

    /// The compact-balanced body.
    pub fn compact(&self) -> &CompactBalanced {
        &self.compact
    }

    /// Ancilla init values, in netlist input order after the data bits.
    pub fn ancilla_init(&self) -> &[bool] {
        &self.ancilla_init
    }

    /// Number of ancilla lines.
    pub fn ancilla_count(&self) -> usize {
        self.ancilla_init.len()
    }
}

// ── Two-body ring (Stage 2a) ─────────────────────────────────────

// F4: named constants for bus-slice offsets -- a transposed splice is
// silent corruption otherwise.

/// CPU body `(CpuState, BusMsg)` data width: 160 + 24 = 184 bits.
pub const CPU_BODY_N_DATA: usize = STATE_BITS + NUM_BUS_FIELDS * W;
/// Bus bits start at offset 160 in the CPU body data vector.
pub const CPU_BUS_OFFSET: usize = STATE_BITS;
/// Number of bus bits (24).
pub const BUS_BITS: usize = NUM_BUS_FIELDS * W;

/// Mem body `(MemState, BusMsg)` data width: 32 + 24 = 56 bits.
pub const MEM_BODY_N_DATA: usize = MEM_WORDS * W + NUM_BUS_FIELDS * W;
/// Bus bits start at offset 32 in the mem body data vector.
pub const MEM_BUS_OFFSET: usize = MEM_WORDS * W;

/// A two-body ring: three separate `CompactBalanced` bodies (`cpu_pre`,
/// `mem_body`, `cpu_post`), each with its own ancilla vector, recirculated
/// sequentially with the bus threaded across (Stage 2a).
///
/// Proven equal to the single-body [`SystemRing`] and to `eval(system_step)^R`.
pub struct TwoBodyRing {
    pre: CompactBalanced,
    pre_ancilla: Vec<bool>,
    pre_n_data: usize,

    mem: CompactBalanced,
    mem_ancilla: Vec<bool>,
    mem_n_data: usize,

    post: CompactBalanced,
    post_ancilla: Vec<bool>,
    post_n_data: usize,
}

/// Build a two-body ring from a ROM image.
///
/// Lowers and compact-balances `cpu_pre`, `mem_body`, and `cpu_post` as
/// three separate bodies. Asserts data widths and input types loudly (F4).
pub fn two_body_ring(rom: &[u32]) -> TwoBodyRing {
    use crate::types::bus_state_ty;
    use theseus_core::Type;

    let cpu_bus_ty = Type::prod(state_ty(), bus_state_ty());
    let mem_bus_ty = Type::prod(mem_state_ty(), bus_state_ty());

    // --- cpu_pre ---
    let iso_pre = crate::step::cpu_pre(rom);
    let (net_pre, anc_pre) = lower_iso_with_ancillas(&iso_pre).expect("cpu_pre lowers");
    let n_data_pre = net_pre.inputs.len() - anc_pre.len();
    assert_eq!(
        n_data_pre, CPU_BODY_N_DATA,
        "cpu_pre n_data ({n_data_pre}) != CPU_BODY_N_DATA ({CPU_BODY_N_DATA})"
    );
    assert_eq!(
        net_pre.input_ty, cpu_bus_ty,
        "cpu_pre input_ty mismatch: expected (CpuState, BusMsg)"
    );
    let compact_pre = balance_compact(&net_pre).expect("balance_compact cpu_pre");

    // --- mem_body ---
    let iso_mem = crate::decoder::mem_body();
    let (net_mem, anc_mem) = lower_iso_with_ancillas(&iso_mem).expect("mem_body lowers");
    let n_data_mem = net_mem.inputs.len() - anc_mem.len();
    assert_eq!(
        n_data_mem, MEM_BODY_N_DATA,
        "mem_body n_data ({n_data_mem}) != MEM_BODY_N_DATA ({MEM_BODY_N_DATA})"
    );
    assert_eq!(
        net_mem.input_ty, mem_bus_ty,
        "mem_body input_ty mismatch: expected (MemState, BusMsg)"
    );
    let compact_mem = balance_compact(&net_mem).expect("balance_compact mem_body");

    // --- cpu_post ---
    let iso_post = crate::step::cpu_post(rom);
    let (net_post, anc_post) = lower_iso_with_ancillas(&iso_post).expect("cpu_post lowers");
    let n_data_post = net_post.inputs.len() - anc_post.len();
    assert_eq!(
        n_data_post, CPU_BODY_N_DATA,
        "cpu_post n_data ({n_data_post}) != CPU_BODY_N_DATA ({CPU_BODY_N_DATA})"
    );
    assert_eq!(
        net_post.input_ty, cpu_bus_ty,
        "cpu_post input_ty mismatch: expected (CpuState, BusMsg)"
    );
    let compact_post = balance_compact(&net_post).expect("balance_compact cpu_post");

    TwoBodyRing {
        pre: compact_pre,
        pre_ancilla: anc_pre,
        pre_n_data: n_data_pre,
        mem: compact_mem,
        mem_ancilla: anc_mem,
        mem_n_data: n_data_mem,
        post: compact_post,
        post_ancilla: anc_post,
        post_n_data: n_data_post,
    }
}

impl TwoBodyRing {
    /// Recirculate `revolutions` times starting from `start`, with each
    /// body's ancilla outputs fed back independently (closed ring).
    ///
    /// Per revolution:
    ///   (a) sim cpu_pre on Cpu[0..160)+Bus[160..184) with Bus=0
    ///   (b) relocate 24 Bus bits to offset 32, sim mem_body on Mem[0..32)+Bus[32..56)
    ///   (c) relocate Bus back to offset 160, sim cpu_post on Cpu+Bus
    ///   (d) F3: assert the 24 Bus output bits of cpu_post are ALL ZERO
    pub fn run(&self, start: &StateVals, revolutions: u32) -> StateVals {
        self.run_lanes::<1>(std::slice::from_ref(start), revolutions, true)
            .remove(0)
    }

    /// [`Self::run`] from many start states at once, [`Lane::WIDTH`] per
    /// pass (ADR-0048): result `i` is `run(&starts[i], revolutions)`.
    pub fn run_many(&self, starts: &[StateVals], revolutions: u32) -> Vec<StateVals> {
        in_blocks(starts, |block| {
            self.run_lanes::<4>(block, revolutions, true)
        })
    }

    /// Recirculate with each body's ring OPEN: each revolution re-supplies
    /// fresh ancilla inits per body. Equals [`Self::run`] when each body
    /// is ancilla-clean.
    pub fn run_open(&self, start: &StateVals, revolutions: u32) -> StateVals {
        self.run_lanes::<1>(std::slice::from_ref(start), revolutions, false)
            .remove(0)
    }

    /// [`Self::run_open`] from many start states at once (ADR-0048).
    pub fn run_open_many(&self, starts: &[StateVals], revolutions: u32) -> Vec<StateVals> {
        in_blocks(starts, |block| {
            self.run_lanes::<4>(block, revolutions, false)
        })
    }

    fn run_lanes<const N: usize>(
        &self,
        starts: &[StateVals],
        revolutions: u32,
        closed: bool,
    ) -> Vec<StateVals> {
        let active = Lanes::<N>::first(starts.len());
        let mut cpu = Carrier::CPU.pack::<N>(starts);
        let mem_bits: Vec<Vec<bool>> = starts.iter().map(Self::encode_mem_bits).collect();
        let mut mem: Vec<Lanes<N>> = pack_lanes(&mem_bits);

        // Each body's input buffer: data ++ bus ++ ancillas. The ancilla
        // tails hold the inits and, when closed, are overwritten with the
        // fed-back outputs every revolution.
        let body_input = |data_len: usize, ancilla_init: &[bool]| {
            let mut input = vec![Lanes::<N>::ZERO; data_len];
            input.extend(splat_bits::<N>(ancilla_init));
            input
        };
        let mut pre_in = body_input(self.pre_n_data, &self.pre_ancilla);
        let mut mem_in = body_input(self.mem_n_data, &self.mem_ancilla);
        let mut post_in = body_input(self.post_n_data, &self.post_ancilla);
        let mut scratch = LaneScratch::default();

        for _ in 0..revolutions {
            // (a) cpu_pre: Cpu[0..160) + Bus[160..184)=0
            pre_in[..STATE_BITS].copy_from_slice(&cpu);
            pre_in[CPU_BUS_OFFSET..self.pre_n_data].fill(Lanes::ZERO); // Bus = 0
            let pre_out = self
                .pre
                .simulate_lanes_with(&pre_in, &mut scratch)
                .expect("simulate cpu_pre");
            cpu.copy_from_slice(&pre_out[..STATE_BITS]);
            if closed {
                pre_in[self.pre_n_data..].copy_from_slice(&pre_out[self.pre_n_data..]);
            }

            // (b) mem_body: Mem[0..32) + Bus[32..56) (relocate bus)
            mem_in[..MEM_BUS_OFFSET].copy_from_slice(&mem);
            mem_in[MEM_BUS_OFFSET..MEM_BUS_OFFSET + BUS_BITS]
                .copy_from_slice(&pre_out[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS]);
            let mem_out = self
                .mem
                .simulate_lanes_with(&mem_in, &mut scratch)
                .expect("simulate mem_body");
            mem.copy_from_slice(&mem_out[..MEM_WORDS * W]);
            if closed {
                mem_in[self.mem_n_data..].copy_from_slice(&mem_out[self.mem_n_data..]);
            }

            // (c) cpu_post: Cpu[0..160) + Bus[160..184) (relocate bus back)
            post_in[..STATE_BITS].copy_from_slice(&cpu);
            post_in[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS]
                .copy_from_slice(&mem_out[MEM_BUS_OFFSET..MEM_BUS_OFFSET + BUS_BITS]);
            let post_out = self
                .post
                .simulate_lanes_with(&post_in, &mut scratch)
                .expect("simulate cpu_post");
            cpu.copy_from_slice(&post_out[..STATE_BITS]);
            if closed {
                post_in[self.post_n_data..].copy_from_slice(&post_out[self.post_n_data..]);
            }

            // (d) F3: assert bus quiescence -- the §3 lemma made runtime
            let bus_out = &post_out[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS];
            let noisy = bus_out.iter().fold(Lanes::ZERO, |acc, &w| acc | w) & active;
            if let Some(lane) = noisy.lowest() {
                let nonzero: Vec<usize> = (0..BUS_BITS).filter(|&i| bus_out[i].get(lane)).collect();
                panic!(
                    "TwoBodyRing{}: bus NOT quiescent after cpu_post -- \
                     §3 quiescence lemma violated (start {lane}, nonzero bus bits: {nonzero:?})",
                    if closed { "" } else { " (open)" },
                );
            }
        }

        // Decode back to StateVals
        let mut result = Carrier::CPU.unpack(&cpu, starts.len());
        for (s, bits) in result.iter_mut().zip(unpack_lanes(&mem, starts.len())) {
            Self::decode_mem_bits(&bits, s);
        }
        result
    }

    /// The cpu_pre compact-balanced body.
    pub fn pre(&self) -> &CompactBalanced {
        &self.pre
    }

    /// The mem_body compact-balanced body.
    pub fn mem(&self) -> &CompactBalanced {
        &self.mem
    }

    /// The cpu_post compact-balanced body.
    pub fn post(&self) -> &CompactBalanced {
        &self.post
    }

    /// Number of ancilla lines in cpu_pre.
    pub fn pre_ancilla_count(&self) -> usize {
        self.pre_ancilla.len()
    }

    /// Number of ancilla lines in mem_body.
    pub fn mem_ancilla_count(&self) -> usize {
        self.mem_ancilla.len()
    }

    /// Number of ancilla lines in cpu_post.
    pub fn post_ancilla_count(&self) -> usize {
        self.post_ancilla.len()
    }

    /// Per-body ancilla mismatch indices accumulated across `revolutions`.
    ///
    /// Threads the ring exactly as [`Self::run`] does (closed ancilla
    /// feedback), but at EACH revolution compares each body's ancilla
    /// OUTPUT bits to that body's ancilla inits and records any index
    /// that differs. Returns `(pre_mismatches, mem_mismatches,
    /// post_mismatches)`. All three empty ⇔ every body's ancillas
    /// restore every revolution -- the per-body zero-garbage certificate.
    pub fn body_ancilla_mismatches(
        &self,
        start: &StateVals,
        revolutions: u32,
    ) -> (Vec<usize>, Vec<usize>, Vec<usize>) {
        let cpu_ty = state_ty();

        let mut cpu_bits = bits_of(&start.encode(), &cpu_ty).expect("cpu bits_of");
        let mut mem_bits = Self::encode_mem_bits(start);

        let mut pre_anc = self.pre_ancilla.clone();
        let mut mem_anc = self.mem_ancilla.clone();
        let mut post_anc = self.post_ancilla.clone();

        let mut pre_mm = Vec::new();
        let mut mem_mm = Vec::new();
        let mut post_mm = Vec::new();

        for _ in 0..revolutions {
            // (a) cpu_pre
            let mut pre_input = cpu_bits.clone();
            pre_input.extend(std::iter::repeat_n(false, BUS_BITS));
            pre_input.extend_from_slice(&pre_anc);
            let pre_sim = self.pre.simulate(&pre_input).expect("simulate cpu_pre");
            cpu_bits = pre_sim.outputs[..STATE_BITS].to_vec();
            let bus_from_pre = pre_sim.outputs[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS].to_vec();
            pre_anc = pre_sim.outputs[self.pre_n_data..].to_vec();

            // Check cpu_pre ancillas
            for (i, (got, expected)) in pre_anc.iter().zip(&self.pre_ancilla).enumerate() {
                if got != expected && !pre_mm.contains(&i) {
                    pre_mm.push(i);
                }
            }

            // (b) mem_body
            let mut mem_input = mem_bits.clone();
            mem_input.extend_from_slice(&bus_from_pre);
            mem_input.extend_from_slice(&mem_anc);
            let mem_sim = self.mem.simulate(&mem_input).expect("simulate mem_body");
            mem_bits = mem_sim.outputs[..MEM_WORDS * W].to_vec();
            let bus_from_mem = mem_sim.outputs[MEM_BUS_OFFSET..MEM_BUS_OFFSET + BUS_BITS].to_vec();
            mem_anc = mem_sim.outputs[self.mem_n_data..].to_vec();

            // Check mem_body ancillas
            for (i, (got, expected)) in mem_anc.iter().zip(&self.mem_ancilla).enumerate() {
                if got != expected && !mem_mm.contains(&i) {
                    mem_mm.push(i);
                }
            }

            // (c) cpu_post
            let mut post_input = cpu_bits.clone();
            post_input.extend_from_slice(&bus_from_mem);
            post_input.extend_from_slice(&post_anc);
            let post_sim = self.post.simulate(&post_input).expect("simulate cpu_post");
            cpu_bits = post_sim.outputs[..STATE_BITS].to_vec();
            post_anc = post_sim.outputs[self.post_n_data..].to_vec();

            // Check cpu_post ancillas
            for (i, (got, expected)) in post_anc.iter().zip(&self.post_ancilla).enumerate() {
                if got != expected && !post_mm.contains(&i) {
                    post_mm.push(i);
                }
            }

            // (d) F3: bus quiescence
            let bus_out = &post_sim.outputs[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS];
            assert!(
                bus_out.iter().all(|&b| !b),
                "TwoBodyRing (ancilla check): bus NOT quiescent after cpu_post"
            );
        }

        (pre_mm, mem_mm, post_mm)
    }

    // ── Internal helpers ──────────────────────────────────────────────

    fn encode_mem_bits(s: &StateVals) -> Vec<bool> {
        let mut bits = Vec::with_capacity(MEM_WORDS * W);
        for &m in &s.mem {
            for i in 0..W {
                bits.push((m >> i) & 1 == 1);
            }
        }
        bits
    }

    fn decode_mem_bits(mem_bits: &[bool], s: &mut StateVals) {
        assert_eq!(mem_bits.len(), MEM_WORDS * W);
        for (i, m) in s.mem.iter_mut().enumerate() {
            let mut v = 0u8;
            for b in 0..W {
                if mem_bits[i * W + b] {
                    v |= 1 << b;
                }
            }
            *m = v;
        }
    }
}

// ── Two-body timing certificate (Stage 2b-lite) ─────────────────

/// Static realizability certificate for the two-body ring (Stage 2b-lite).
///
/// Proves the two-body ring (cpu_pre / mem_body / cpu_post) can be
/// clocked as two concurrent lockstep loops joined by a bus cross-edge.
/// Pure integer arithmetic over lowered-body depths -- no lowering
/// mutation, no machine-semantics change, no padder materialization.
///
/// δ₁/δ₂ are the minimal phase-aligning transits (a specific realizable
/// placement); 2b-lite proves a consistent lockstep clocking EXISTS and
/// pins it to measured depths -- the physical padded netlist (2b-full)
/// is deferred to the energy-ledger work.
#[derive(Clone, Debug)]
pub struct TwoBodyTiming {
    /// Depth of cpu_pre body (max data-output arrival).
    pub d_pre: u32,
    /// Depth of mem_body (max data-output arrival).
    pub d_mem: u32,
    /// Depth of cpu_post body (max data-output arrival).
    pub d_post: u32,
    /// Phase-aligning transit: least k≥0 with (d_pre+k) ≡ 0 (mod P).
    pub delta1: u32,
    /// Phase-aligning transit: least k≥0 with (d_pre+delta1+d_mem+k) ≡ 0 (mod P).
    pub delta2: u32,
    /// Co-arrival hold: delta1 + d_mem + delta2.
    pub delta_hold: u32,
    /// Locked period: d_pre + delta_hold + d_post, rounded up to a multiple of P.
    pub d_locked: u32,
    /// Period = d_locked.
    pub period: u32,
    /// Per-bus-line flush pads for cpu_pre: (d_pre - arrival_of_bus_output_i) for each of 24 bus lines.
    pub pre_bus_pads: Vec<u32>,
    /// Per-bus-line flush pads for mem_body: (d_mem - arrival_of_bus_output_i) for each of 24 bus lines.
    pub mem_bus_pads: Vec<u32>,
    /// Per-bus-line flush pads for cpu_post: (d_post - arrival_of_bus_output_i) for each of 24 bus lines.
    pub post_bus_pads: Vec<u32>,
}

impl TwoBodyTiming {
    /// The phase count P used in the certificate.
    pub fn period_p(&self) -> u32 {
        u32::from(PHASE_COUNT)
    }

    /// Independent re-derivation and validation of the certificate (F-teeth-2).
    ///
    /// Re-derives delta1, delta2, pad by a brute scan (a different code path
    /// than the solver) and compares to the stored fields. Enforces assertions
    /// A-G from §4b. Returns Err(descriptive string) on any mismatch.
    pub fn validate(&self) -> Result<(), String> {
        let p = self.period_p();

        // Independent re-derivation via brute scan (F-teeth-2)
        let re_delta1 = (0..p)
            .find(|&k| (self.d_pre + k).is_multiple_of(p))
            .ok_or_else(|| format!("no delta1 in [0,{p})"))?;
        let re_delta2 = (0..p)
            .find(|&k| (self.d_pre + re_delta1 + self.d_mem + k).is_multiple_of(p))
            .ok_or_else(|| format!("no delta2 in [0,{p})"))?;
        let re_delta_hold = re_delta1 + self.d_mem + re_delta2;
        let re_d_cpu = self.d_pre + re_delta_hold + self.d_post;
        let re_pad = (p - re_d_cpu % p) % p;
        let re_d_locked = re_d_cpu + re_pad;

        // Compare re-derived values against stored fields
        if self.delta1 != re_delta1 {
            return Err(format!(
                "delta1 mismatch: stored {} != re-derived {re_delta1}",
                self.delta1
            ));
        }
        if self.delta2 != re_delta2 {
            return Err(format!(
                "delta2 mismatch: stored {} != re-derived {re_delta2}",
                self.delta2
            ));
        }
        if self.delta_hold != re_delta_hold {
            return Err(format!(
                "delta_hold mismatch: stored {} != re-derived {re_delta_hold}",
                self.delta_hold
            ));
        }
        if self.d_locked != re_d_locked {
            return Err(format!(
                "d_locked mismatch: stored {} != re-derived {re_d_locked}",
                self.d_locked
            ));
        }
        if self.period != re_d_locked {
            return Err(format!(
                "period mismatch: stored {} != re-derived {re_d_locked}",
                self.period
            ));
        }

        // A. co-arrival (F1): delta_hold == delta1 + d_mem + delta2
        if self.delta_hold != self.delta1 + self.d_mem + self.delta2 {
            return Err(format!(
                "A: co-arrival violated: delta_hold {} != delta1({}) + d_mem({}) + delta2({})",
                self.delta_hold, self.delta1, self.d_mem, self.delta2
            ));
        }

        // B. no revolution skew: 0 < delta1+d_mem+delta2 < d_locked
        let bus_trip = self.delta1 + self.d_mem + self.delta2;
        if bus_trip == 0 || bus_trip >= self.d_locked {
            return Err(format!(
                "B: revolution skew: bus_trip={bus_trip} not in (0, {})",
                self.d_locked
            ));
        }

        // C. phase closure
        if !self.d_locked.is_multiple_of(p) {
            return Err(format!(
                "C: d_locked {} not divisible by P={p}",
                self.d_locked
            ));
        }
        if re_pad >= p {
            return Err(format!("C: pad {re_pad} >= P={p}"));
        }
        if !(self.d_pre + self.delta1).is_multiple_of(p) {
            return Err(format!(
                "C: (d_pre + delta1) = {} not divisible by P={p}",
                self.d_pre + self.delta1
            ));
        }
        if !(self.d_pre + self.delta1 + self.d_mem + self.delta2).is_multiple_of(p) {
            return Err(format!(
                "C: (d_pre + delta1 + d_mem + delta2) = {} not divisible by P={p}",
                self.d_pre + self.delta1 + self.d_mem + self.delta2
            ));
        }

        // D. period lock: period == d_locked
        if self.period != self.d_locked {
            return Err(format!(
                "D: period {} != d_locked {}",
                self.period, self.d_locked
            ));
        }

        // E. non-negativity + minimality (all u32 so ≥0 by type; check < P)
        if self.delta1 >= p {
            return Err(format!("E: delta1 {} >= P={p}", self.delta1));
        }
        if self.delta2 >= p {
            return Err(format!("E: delta2 {} >= P={p}", self.delta2));
        }
        if re_pad >= p {
            return Err(format!("E: pad {re_pad} >= P={p}"));
        }

        // F. line-audit (input_ty checks) -- done in the test, not here,
        // because validate() operates on the timing struct only.

        // G. per-bus-line flush pads ≥ 0 (u32 so ≥0 by type, but assert
        // the vectors have the right length = BUS_BITS)
        if self.pre_bus_pads.len() != BUS_BITS {
            return Err(format!(
                "G: pre_bus_pads length {} != BUS_BITS={BUS_BITS}",
                self.pre_bus_pads.len()
            ));
        }
        if self.mem_bus_pads.len() != BUS_BITS {
            return Err(format!(
                "G: mem_bus_pads length {} != BUS_BITS={BUS_BITS}",
                self.mem_bus_pads.len()
            ));
        }
        if self.post_bus_pads.len() != BUS_BITS {
            return Err(format!(
                "G: post_bus_pads length {} != BUS_BITS={BUS_BITS}",
                self.post_bus_pads.len()
            ));
        }
        // The pads are u32 so non-negative by construction, but we verify
        // them anyway in case the solver produced a negative value that
        // was cast to u32 wrapping (which would appear as a large value).
        // We check that d_body >= arrival, i.e. pad <= d_body.
        for (i, &pad) in self.pre_bus_pads.iter().enumerate() {
            if pad > self.d_pre {
                return Err(format!("G: pre_bus_pads[{i}]={pad} > d_pre={}", self.d_pre));
            }
        }
        for (i, &pad) in self.mem_bus_pads.iter().enumerate() {
            if pad > self.d_mem {
                return Err(format!("G: mem_bus_pads[{i}]={pad} > d_mem={}", self.d_mem));
            }
        }
        for (i, &pad) in self.post_bus_pads.iter().enumerate() {
            if pad > self.d_post {
                return Err(format!(
                    "G: post_bus_pads[{i}]={pad} > d_post={}",
                    self.d_post
                ));
            }
        }

        // Mem-merge clause (F-teeth-3): mem-loop window starts at
        // d_pre + delta1 ≡ 0 (mod P), ends at d_pre + delta1 + d_mem,
        // both within [0, d_locked). Window start already checked in C.
        let window_start = self.d_pre + self.delta1;
        if window_start >= self.d_locked {
            return Err(format!(
                "mem-merge: window_start={window_start} >= d_locked={}",
                self.d_locked
            ));
        }

        Ok(())
    }
}

/// Compute a static two-body timing realizability certificate (Stage 2b-lite).
///
/// Lowers `cpu_pre(rom)`, `mem_body()`, `cpu_post(rom)` via
/// `lower_iso_with_ancillas`, computes each body's max data-output arrival
/// via `line_arrivals()` (the authoritative balanced arrival), and solves
/// the lockstep arrival equations per §4b steps 1-6.
///
/// No lowering mutation, no machine-semantics change, no padder materialization.
pub fn two_body_timing_certificate(rom: &[u32]) -> Result<TwoBodyTiming, CellNetError> {
    let p = u32::from(PHASE_COUNT);

    // --- Lower each body and compute checked output arrivals ---

    let iso_pre = crate::step::cpu_pre(rom);
    let (net_pre, anc_pre) = lower_iso_with_ancillas(&iso_pre).expect("cpu_pre lowers");
    let n_data_pre = net_pre.inputs.len() - anc_pre.len();
    assert_eq!(
        n_data_pre, CPU_BODY_N_DATA,
        "cpu_pre n_data ({n_data_pre}) != CPU_BODY_N_DATA ({CPU_BODY_N_DATA})"
    );
    let checked_pre = net_pre.checked_output_arrivals()?;

    let iso_mem = crate::decoder::mem_body();
    let (net_mem, anc_mem) = lower_iso_with_ancillas(&iso_mem).expect("mem_body lowers");
    let n_data_mem = net_mem.inputs.len() - anc_mem.len();
    assert_eq!(
        n_data_mem, MEM_BODY_N_DATA,
        "mem_body n_data ({n_data_mem}) != MEM_BODY_N_DATA ({MEM_BODY_N_DATA})"
    );
    let checked_mem = net_mem.checked_output_arrivals()?;

    let iso_post = crate::step::cpu_post(rom);
    let (net_post, anc_post) = lower_iso_with_ancillas(&iso_post).expect("cpu_post lowers");
    let n_data_post = net_post.inputs.len() - anc_post.len();
    assert_eq!(
        n_data_post, CPU_BODY_N_DATA,
        "cpu_post n_data ({n_data_post}) != CPU_BODY_N_DATA ({CPU_BODY_N_DATA})"
    );
    let checked_post = net_post.checked_output_arrivals()?;

    // --- Step 1: D_pre/D_mem/D_post = max data-output arrival ---

    let d_pre = checked_pre[..n_data_pre]
        .iter()
        .copied()
        .max()
        .expect("body has data outputs");
    let d_mem = checked_mem[..n_data_mem]
        .iter()
        .copied()
        .max()
        .expect("body has data outputs");
    let d_post = checked_post[..n_data_post]
        .iter()
        .copied()
        .max()
        .expect("body has data outputs");

    // --- Step 2: delta1 = least k>=0 with (d_pre+k) % P == 0 ---
    let delta1 = (0..p)
        .find(|&k| (d_pre + k) % p == 0)
        .expect("delta1 must exist in [0, P)");

    // --- Step 3: delta2 = least k>=0 with (d_pre+delta1+d_mem+k) % P == 0 ---
    let delta2 = (0..p)
        .find(|&k| (d_pre + delta1 + d_mem + k) % p == 0)
        .expect("delta2 must exist in [0, P)");

    // --- Step 4: delta_hold = delta1 + d_mem + delta2 ---
    let delta_hold = delta1 + d_mem + delta2;

    // --- Step 5: d_cpu, pad, d_locked ---
    let d_cpu = d_pre + delta_hold + d_post;
    let pad = (p - d_cpu % p) % p;
    let d_locked = d_cpu + pad;

    // --- Step 6: period = d_locked ---
    let period = d_locked;

    // --- Per-bus-line flush pads (F-teeth-3) ---
    // Bus output lines: CPU bodies at [CPU_BUS_OFFSET..CPU_BUS_OFFSET+BUS_BITS),
    // mem body at [MEM_BUS_OFFSET..MEM_BUS_OFFSET+BUS_BITS).
    // Fed from the SAME checked arrivals vec -- bus range is a subset of data range.

    let pre_bus_pads: Vec<u32> = (0..BUS_BITS)
        .map(|i| d_pre - checked_pre[CPU_BUS_OFFSET + i])
        .collect();
    let mem_bus_pads: Vec<u32> = (0..BUS_BITS)
        .map(|i| d_mem - checked_mem[MEM_BUS_OFFSET + i])
        .collect();
    let post_bus_pads: Vec<u32> = (0..BUS_BITS)
        .map(|i| d_post - checked_post[CPU_BUS_OFFSET + i])
        .collect();

    Ok(TwoBodyTiming {
        d_pre,
        d_mem,
        d_post,
        delta1,
        delta2,
        delta_hold,
        d_locked,
        period,
        pre_bus_pads,
        mem_bus_pads,
        post_bus_pads,
    })
}
