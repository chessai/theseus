//! TISC-1 ring runner: recirculate one compact-balanced `step` R times
//! (ADR-0033, M4-v2).
//!
//! [`step_ring`] lowers and compact-balances `step(rom)` without
//! materializing the ~1.6 M K0 buffers (uses [`balance_compact`]).
//! [`StepRing::run`] recirculates via [`CompactBalanced::simulate`],
//! re-supplying fresh ancilla-init bits each revolution. The
//! differential oracle is `eval(step)^R`.
//!
//! # Ancilla restoration — the zero-garbage ring
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
//! physically closes with **zero per-revolution boundary** — nothing is
//! re-supplied, nothing exits. It equals `eval(step)^R`, and equals
//! [`StepRing::run_open`] (the open-loop variant that re-supplies fresh
//! inits) — that equivalence is what certifies the closure. This is the
//! literal zero-garbage ring.
//!
//! # Closed ring (Bennett path — fallback)
//!
//! [`closed_step_ring`] lowers the step via
//! [`lower_iso_bennett`] (compute–copy–uncompute, ADR-0033): the
//! wrap restores EVERY forward ancilla to its init, data-independently,
//! and concentrates the irreducible parity residue into a fixed
//! boundary of `2·w` slots (per data bit `j`: one slot carrying `y_j`
//! against init `1`, one carrying the regenerated `x_j` against init
//! `0`). [`ClosedStepRing::run`] therefore recirculates by feeding back
//! the State **and all forward-ancilla outputs** as the next
//! revolution's inputs, re-supplying only the fixed `(1, 0)` boundary
//! pairs — a *physically closing* ring whose per-revolution external
//! interface is a constant-size, data-independent set of ports
//! ([`ClosedStepRing::boundary_size`], `2·w = 320` for the TISC-1
//! State). With the plain path now ancilla-clean, the Bennett
//! [`ClosedStepRing`] demotes to a **fallback** for circuits whose
//! lowering is still K-only-dirty (e.g. trace tag duplication, iter/fold
//! control chains — genuine fan-outs that still emit K-only garbage).

use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{
    balance_compact, bits_of, lower_iso_bennett, value_from_bits, CompactBalanced,
};

use crate::step::{step, system_step};
use crate::types::{
    decode_system, encode_system, mem_state_ty, state_ty, system_state_ty, StateVals,
    COMPOSITE_BITS, MEM_WORDS, NUM_BUS_FIELDS, STATE_BITS, SYSTEM_BITS, W,
};

/// A compact-balanced TISC-1 step ready for ring recirculation.
pub struct StepRing {
    compact: CompactBalanced,
    ancilla_init: Vec<bool>,
    n_data: usize,
}

/// Build a ring-ready compact-balanced step from a ROM image.
///
/// Calls `step(rom)`, lowers with ancillas, and compact-balances —
/// O(active cells) memory, no K0 materialization.
pub fn step_ring(rom: &[u16]) -> StepRing {
    let iso = step(rom);
    let (net, ancilla_init) =
        lower_iso_with_ancillas(&iso).expect("step lowers");
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
    /// with the ring CLOSED — the **default, zero-garbage** runner
    /// (ADR-0040 phase C).
    ///
    /// Each revolution feeds the ancilla *outputs* back as the next
    /// revolution's ancilla *inputs* (TRUE recirculation): nothing is
    /// re-supplied, nothing exits — the ring physically closes with
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
        debug_assert!(
            self.ancillas_restored(start),
            "StepRing::run assumes an ancilla-clean lowering (zero-garbage \
             ring); this lowering leaves garbage — use run_open or \
             ClosedStepRing. mismatches: {:?}",
            self.ancilla_mismatches(start)
        );
        let ty = state_ty();
        let mut data_bits =
            bits_of(&start.encode(), &ty).expect("bits_of");
        let mut ancilla_bits = self.ancilla_init.clone();

        for _ in 0..revolutions {
            let mut input = data_bits.clone();
            input.extend_from_slice(&ancilla_bits);
            let sim =
                self.compact.simulate(&input).expect("simulate");
            data_bits = sim.outputs[..self.n_data].to_vec();
            ancilla_bits = sim.outputs[self.n_data..].to_vec();
        }

        let val =
            value_from_bits(&ty, &data_bits).expect("value_from_bits");
        StateVals::decode(&val)
    }

    /// Recirculate with the ring OPEN: each revolution discards the
    /// ancilla outputs and re-supplies fresh `ancilla_init`.
    ///
    /// Always correct (independent of ancilla cleanliness); equals
    /// [`Self::run`] exactly when the lowering is ancilla-clean, which is
    /// the equivalence that certifies the zero-garbage closure. Retained
    /// as the robust fallback for non-ancilla-clean lowerings.
    pub fn run_open(
        &self,
        start: &StateVals,
        revolutions: u32,
    ) -> StateVals {
        let ty = state_ty();
        let mut data_bits =
            bits_of(&start.encode(), &ty).expect("bits_of");
        assert_eq!(data_bits.len(), self.n_data);

        for _ in 0..revolutions {
            let mut input = data_bits.clone();
            input.extend_from_slice(&self.ancilla_init);
            let sim =
                self.compact.simulate(&input).expect("simulate");
            data_bits = sim.outputs[..self.n_data].to_vec();
        }

        let val =
            value_from_bits(&ty, &data_bits).expect("value_from_bits");
        StateVals::decode(&val)
    }

    /// Indices of ancilla lines whose output differs from their init
    /// after one revolution from `start`.
    pub fn ancilla_mismatches(&self, start: &StateVals) -> Vec<usize> {
        let ty = state_ty();
        let data_bits =
            bits_of(&start.encode(), &ty).expect("bits_of");
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
    /// `start` — the ring machine genuinely closes.
    pub fn ancillas_restored(&self, start: &StateVals) -> bool {
        self.ancilla_mismatches(start).is_empty()
    }

    /// The compact-balanced body.
    pub fn compact(&self) -> &CompactBalanced {
        &self.compact
    }

    /// Number of ancilla lines.
    pub fn ancilla_count(&self) -> usize {
        self.ancilla_init.len()
    }
}

// ── Closed ring (Bennett-wrapped step) ─────────────────────────────

/// A compact-balanced Bennett-wrapped TISC-1 step: forward ancillas
/// are restored every revolution (fed back), and only the fixed
/// `2·w`-slot boundary is re-supplied — the physically closing ring.
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
pub fn closed_step_ring(rom: &[u16]) -> ClosedStepRing {
    let iso = step(rom);
    let (net, ancilla_init) =
        lower_iso_bennett(&iso).expect("step lowers via bennett");
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
    /// injected after the start state — the differential oracle
    /// `run(s, R) == eval(step)^R` therefore certifies that the
    /// forward ancillas really do come back clean every revolution.
    pub fn run(&self, start: &StateVals, revolutions: u32) -> StateVals {
        let ty = state_ty();
        let mut data_bits =
            bits_of(&start.encode(), &ty).expect("bits_of");
        assert_eq!(data_bits.len(), self.n_data);
        let mut forward_bits =
            self.ancilla_init[..self.n_forward].to_vec();
        let boundary_init = &self.ancilla_init[self.n_forward..];

        for _ in 0..revolutions {
            let mut input = data_bits.clone();
            input.extend_from_slice(&forward_bits);
            input.extend_from_slice(boundary_init);
            let sim =
                self.compact.simulate(&input).expect("simulate");
            data_bits = sim.outputs[..self.n_data].to_vec();
            // Feed the forward ancillas back — TRUE recirculation.
            forward_bits = sim.outputs
                [self.n_data..self.n_data + self.n_forward]
                .to_vec();
            // Boundary outputs (the Bennett residue) exit the ring;
            // fresh (1, 0) pairs are re-supplied next revolution.
        }

        let val =
            value_from_bits(&ty, &data_bits).expect("value_from_bits");
        StateVals::decode(&val)
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
    /// bits, interleaved `(y_0, x_0, y_1, x_1, …)` — the output word
    /// and the regenerated input word, the honest Bennett residue.
    pub fn boundary_residue(&self, start: &StateVals) -> Vec<bool> {
        let sim_out = self.revolve_once(start);
        sim_out[self.n_data + self.n_forward..].to_vec()
    }

    fn revolve_once(&self, start: &StateVals) -> Vec<bool> {
        let ty = state_ty();
        let data_bits =
            bits_of(&start.encode(), &ty).expect("bits_of");
        let mut input = data_bits;
        input.extend_from_slice(&self.ancilla_init);
        self.compact.simulate(&input).expect("simulate").outputs
    }

    /// State width in rail pairs (`w`).
    pub fn data_width(&self) -> usize {
        self.n_data
    }

    /// Forward ancilla lines — fed back, never re-supplied.
    pub fn forward_ancilla_count(&self) -> usize {
        self.n_forward
    }

    /// Size of the fixed boundary re-supplied each revolution:
    /// `2·w` slots with constant inits `(1, 0)` per data bit —
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
/// carrier, using [`encode_system`]/[`decode_system`] for I/O.
/// The bus-quiescence boundary condition is checked via
/// [`SystemRing::bus_quiescent`].
pub struct SystemRing {
    compact: CompactBalanced,
    ancilla_init: Vec<bool>,
    n_data: usize,
}

/// Build a ring-ready compact-balanced system_step from a ROM image.
///
/// Calls `system_step(rom)`, lowers with ancillas, and compact-balances —
/// O(active cells) memory, no K0 materialization.
pub fn system_ring(rom: &[u16]) -> SystemRing {
    let iso = system_step(rom);
    let (net, ancilla_init) =
        lower_iso_with_ancillas(&iso).expect("system_step lowers");
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
    /// `debug_assert!(ancillas_restored)` — the certificate test
    /// asserts restoration explicitly, and the internal assert would
    /// panic before the certificate can observe `run==run_open` /
    /// `bus_quiescent`, and it doubles per-call cost.
    pub fn run(&self, start: &StateVals, revolutions: u32) -> StateVals {
        let ty = system_state_ty();
        let mut data_bits =
            bits_of(&encode_system(start), &ty).expect("bits_of");
        let mut ancilla_bits = self.ancilla_init.clone();

        for _ in 0..revolutions {
            let mut input = data_bits.clone();
            input.extend_from_slice(&ancilla_bits);
            let sim =
                self.compact.simulate(&input).expect("simulate");
            data_bits = sim.outputs[..self.n_data].to_vec();
            ancilla_bits = sim.outputs[self.n_data..].to_vec();
        }

        let val =
            value_from_bits(&ty, &data_bits).expect("value_from_bits");
        decode_system(&val)
    }

    /// Recirculate with the ring OPEN: each revolution discards the
    /// ancilla outputs and re-supplies fresh `ancilla_init`.
    ///
    /// Always correct (independent of ancilla cleanliness); equals
    /// [`Self::run`] exactly when the lowering is ancilla-clean, which
    /// is the equivalence that certifies the zero-garbage closure.
    pub fn run_open(
        &self,
        start: &StateVals,
        revolutions: u32,
    ) -> StateVals {
        let ty = system_state_ty();
        let mut data_bits =
            bits_of(&encode_system(start), &ty).expect("bits_of");
        assert_eq!(data_bits.len(), self.n_data);

        for _ in 0..revolutions {
            let mut input = data_bits.clone();
            input.extend_from_slice(&self.ancilla_init);
            let sim =
                self.compact.simulate(&input).expect("simulate");
            data_bits = sim.outputs[..self.n_data].to_vec();
        }

        let val =
            value_from_bits(&ty, &data_bits).expect("value_from_bits");
        decode_system(&val)
    }

    /// Indices of ancilla lines whose output differs from their init
    /// after one revolution from `start`.
    pub fn ancilla_mismatches(&self, start: &StateVals) -> Vec<usize> {
        let ty = system_state_ty();
        let data_bits =
            bits_of(&encode_system(start), &ty).expect("bits_of");
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
    /// `start` — the ring machine genuinely closes.
    pub fn ancillas_restored(&self, start: &StateVals) -> bool {
        self.ancilla_mismatches(start).is_empty()
    }

    /// True if the BusMsg data lines are all zero after one revolution
    /// from `start` — the bus-quiescence boundary condition.
    ///
    /// Computes the bus range symbolically as
    /// `COMPOSITE_BITS..SYSTEM_BITS` and asserts `n_data == SYSTEM_BITS`
    /// first (catches lowering-order regressions loudly).
    pub fn bus_quiescent(&self, start: &StateVals) -> bool {
        assert_eq!(
            self.n_data, SYSTEM_BITS,
            "SystemRing::bus_quiescent: n_data ({}) != SYSTEM_BITS ({}) — \
             lowering-order regression",
            self.n_data, SYSTEM_BITS,
        );
        let ty = system_state_ty();
        let data_bits =
            bits_of(&encode_system(start), &ty).expect("bits_of");
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

    /// Number of ancilla lines.
    pub fn ancilla_count(&self) -> usize {
        self.ancilla_init.len()
    }
}

// ── Two-body ring (Stage 2a) ─────────────────────────────────────

// F4: named constants for bus-slice offsets — a transposed splice is
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
pub fn two_body_ring(rom: &[u16]) -> TwoBodyRing {
    use crate::types::bus_state_ty;
    use theseus_core::Type;

    let cpu_bus_ty = Type::prod(state_ty(), bus_state_ty());
    let mem_bus_ty = Type::prod(mem_state_ty(), bus_state_ty());

    // --- cpu_pre ---
    let iso_pre = crate::step::cpu_pre(rom);
    let (net_pre, anc_pre) =
        lower_iso_with_ancillas(&iso_pre).expect("cpu_pre lowers");
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
    let (net_mem, anc_mem) =
        lower_iso_with_ancillas(&iso_mem).expect("mem_body lowers");
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
    let (net_post, anc_post) =
        lower_iso_with_ancillas(&iso_post).expect("cpu_post lowers");
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
        let cpu_ty = state_ty();

        // Encode CPU and Mem bits separately
        let mut cpu_bits = bits_of(&start.encode(), &cpu_ty).expect("cpu bits_of");
        let mut mem_bits = Self::encode_mem_bits(start);

        let mut pre_anc = self.pre_ancilla.clone();
        let mut mem_anc = self.mem_ancilla.clone();
        let mut post_anc = self.post_ancilla.clone();

        for _ in 0..revolutions {
            // (a) cpu_pre: Cpu[0..160) + Bus[160..184)=0
            let mut pre_input = cpu_bits.clone();
            pre_input.extend(std::iter::repeat_n(false, BUS_BITS)); // Bus = 0
            pre_input.extend_from_slice(&pre_anc);
            let pre_sim = self.pre.simulate(&pre_input).expect("simulate cpu_pre");
            cpu_bits = pre_sim.outputs[..STATE_BITS].to_vec();
            let bus_from_pre =
                pre_sim.outputs[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS].to_vec();
            pre_anc = pre_sim.outputs[self.pre_n_data..].to_vec();

            // (b) mem_body: Mem[0..32) + Bus[32..56) (relocate bus)
            let mut mem_input = mem_bits.clone();
            mem_input.extend_from_slice(&bus_from_pre);
            mem_input.extend_from_slice(&mem_anc);
            let mem_sim = self.mem.simulate(&mem_input).expect("simulate mem_body");
            mem_bits = mem_sim.outputs[..MEM_WORDS * W].to_vec();
            let bus_from_mem =
                mem_sim.outputs[MEM_BUS_OFFSET..MEM_BUS_OFFSET + BUS_BITS].to_vec();
            mem_anc = mem_sim.outputs[self.mem_n_data..].to_vec();

            // (c) cpu_post: Cpu[0..160) + Bus[160..184) (relocate bus back)
            let mut post_input = cpu_bits.clone();
            post_input.extend_from_slice(&bus_from_mem);
            post_input.extend_from_slice(&post_anc);
            let post_sim = self.post.simulate(&post_input).expect("simulate cpu_post");
            cpu_bits = post_sim.outputs[..STATE_BITS].to_vec();
            post_anc = post_sim.outputs[self.post_n_data..].to_vec();

            // (d) F3: assert bus quiescence — the §3 lemma made runtime
            let bus_out =
                &post_sim.outputs[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS];
            assert!(
                bus_out.iter().all(|&b| !b),
                "TwoBodyRing: bus NOT quiescent after cpu_post — \
                 §3 quiescence lemma violated (nonzero bus bits: {:?})",
                bus_out
                    .iter()
                    .enumerate()
                    .filter(|(_, &b)| b)
                    .map(|(i, _)| i)
                    .collect::<Vec<_>>()
            );
        }

        // Decode back to StateVals
        let cpu_val =
            value_from_bits(&cpu_ty, &cpu_bits).expect("cpu value_from_bits");
        let mut result = StateVals::decode(&cpu_val);
        Self::decode_mem_bits(&mem_bits, &mut result);
        result
    }

    /// Recirculate with each body's ring OPEN: each revolution re-supplies
    /// fresh ancilla inits per body. Equals [`Self::run`] when each body
    /// is ancilla-clean.
    pub fn run_open(&self, start: &StateVals, revolutions: u32) -> StateVals {
        let cpu_ty = state_ty();

        let mut cpu_bits = bits_of(&start.encode(), &cpu_ty).expect("cpu bits_of");
        let mut mem_bits = Self::encode_mem_bits(start);

        for _ in 0..revolutions {
            // (a) cpu_pre
            let mut pre_input = cpu_bits.clone();
            pre_input.extend(std::iter::repeat_n(false, BUS_BITS));
            pre_input.extend_from_slice(&self.pre_ancilla);
            let pre_sim = self.pre.simulate(&pre_input).expect("simulate cpu_pre");
            cpu_bits = pre_sim.outputs[..STATE_BITS].to_vec();
            let bus_from_pre =
                pre_sim.outputs[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS].to_vec();

            // (b) mem_body
            let mut mem_input = mem_bits.clone();
            mem_input.extend_from_slice(&bus_from_pre);
            mem_input.extend_from_slice(&self.mem_ancilla);
            let mem_sim = self.mem.simulate(&mem_input).expect("simulate mem_body");
            mem_bits = mem_sim.outputs[..MEM_WORDS * W].to_vec();
            let bus_from_mem =
                mem_sim.outputs[MEM_BUS_OFFSET..MEM_BUS_OFFSET + BUS_BITS].to_vec();

            // (c) cpu_post
            let mut post_input = cpu_bits.clone();
            post_input.extend_from_slice(&bus_from_mem);
            post_input.extend_from_slice(&self.post_ancilla);
            let post_sim = self.post.simulate(&post_input).expect("simulate cpu_post");
            cpu_bits = post_sim.outputs[..STATE_BITS].to_vec();

            // (d) F3: assert bus quiescence
            let bus_out =
                &post_sim.outputs[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS];
            assert!(
                bus_out.iter().all(|&b| !b),
                "TwoBodyRing (open): bus NOT quiescent after cpu_post"
            );
        }

        let cpu_val =
            value_from_bits(&cpu_ty, &cpu_bits).expect("cpu value_from_bits");
        let mut result = StateVals::decode(&cpu_val);
        Self::decode_mem_bits(&mem_bits, &mut result);
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
    /// restore every revolution — the per-body zero-garbage certificate.
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
            let bus_from_pre =
                pre_sim.outputs[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS].to_vec();
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
            let bus_from_mem =
                mem_sim.outputs[MEM_BUS_OFFSET..MEM_BUS_OFFSET + BUS_BITS].to_vec();
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
            let bus_out =
                &post_sim.outputs[CPU_BUS_OFFSET..CPU_BUS_OFFSET + BUS_BITS];
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
