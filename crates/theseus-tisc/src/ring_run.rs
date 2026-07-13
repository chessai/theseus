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
//! old splitter/selector synthesis). [`StepRing::run_true`] — which
//! feeds ancilla outputs back instead of re-supplying fresh inits —
//! equals `eval(step)^R` and equals [`StepRing::run`]. The ring
//! physically closes with **zero per-revolution boundary**: nothing
//! is re-supplied, nothing exits. This is the literal zero-garbage
//! ring.
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

use crate::step::step;
use crate::types::{state_ty, StateVals};

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
    /// Recirculate the step `revolutions` times starting from `start`.
    ///
    /// Each revolution: assemble input = \[data\_bits ++ ancilla\_init\],
    /// simulate, take the first `n_data` output bits as the next state,
    /// discard ancilla outputs (re-supply fresh ancilla\_init).
    pub fn run(&self, start: &StateVals, revolutions: u32) -> StateVals {
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

    /// Run with TRUE recirculation: feed ancilla outputs back as inputs
    /// (instead of re-supplying fresh ancilla\_init). Valid only when
    /// [`Self::ancillas_restored`] is true for the given start state.
    pub fn run_true(
        &self,
        start: &StateVals,
        revolutions: u32,
    ) -> StateVals {
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
