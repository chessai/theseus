//! TISC-1 ring runner: recirculate one compact-balanced `step` R times
//! (ADR-0033, M4-v2).
//!
//! [`step_ring`] lowers and compact-balances `step(rom)` without
//! materializing the ~1.6 M K0 buffers (uses [`balance_compact`]).
//! [`StepRing::run`] recirculates via [`CompactBalanced::simulate`],
//! re-supplying fresh ancilla-init bits each revolution. The
//! differential oracle is `eval(step)^R`.
//!
//! # Ancilla restoration
//!
//! True ring-machine recirculation (feeding ancilla outputs back as
//! inputs) is valid only if the step's lowering restores its ancillas
//! after one revolution. [`StepRing::ancillas_restored`] checks this.
//! If the lowering is not yet ancilla-clean, the re-supply-fresh-init
//! driver still faithfully models `step^R`; cleaning the ancillas
//! (Bennett-style uncompute) is a follow-up.

use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{balance_compact, bits_of, value_from_bits, CompactBalanced};

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
