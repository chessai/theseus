//! Ring recirculation prototype (ADR-0033, M4-v2).
//!
//! A [`RingNetlist`] is one balanced feed-forward body plus an
//! *implicit* wrap: output port i feeds input port i on the next
//! revolution. Per ADR-0033 D2 the wrap edge lives **outside** the DAG —
//! `PhasedCellNetlist` stays acyclic and every existing analysis
//! (`validate`, `line_arrivals`, `sim`) is reused on the body
//! unchanged. The closure conditions checked here:
//!
//! 1. the body validates (and is tick-balanced — the sim refuses
//!    otherwise);
//! 2. **uniform output depth** D: all output ports arrive on one tick,
//!    so the whole boundary crosses the wrap together;
//! 3. **phase closure** `D ≡ 0 (mod P)`: the 4-phase clock is periodic,
//!    so tick D drives the same trapezoids as tick 0 iff P | D — the
//!    sequencer's periodicity, not a wrapped phase assignment, is what
//!    makes the feedback legal;
//! 4. the wrap is a width-aligned identity on ports (`input_ty` =
//!    `output_ty` is the type-level face of the same condition).
//!
//! [`pad_ring`] establishes 2–3 by appending K0 chains (the materialized
//! stand-in for the compact delay-count edges of ADR-0033 D4 — fine at
//! prototype scale, exactly what `balance_compact` replaces at step
//! scale). [`RingNetlist::run`] is the value-level executor:
//! revolutions R come from the caller (the "sequencer", ADR-0033 D3),
//! and the differential oracle is iterating the body's function R
//! times.

use core::fmt;

use crate::ir::cell::{
    Cell, CellKind, CellNetError, LineId, PhasedCellNetlist, RailPair, PHASE_COUNT,
};
use crate::sim::{simulate, SimError};

/// Why a body cannot close into a ring.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RingError {
    /// The body is structurally invalid.
    Body(CellNetError),
    /// The wrap is not a width-aligned identity: output port count must
    /// equal input port count (ADR-0033 D2 condition 4).
    PortMismatch { inputs: usize, outputs: usize },
    /// Output ports arrive on different ticks — the boundary would not
    /// cross the wrap together (condition 2). Run [`pad_ring`] first.
    OutputSkew { depths: Vec<u32> },
    /// Uniform output depth D is not a multiple of P: tick D would not
    /// drive the same clock trapezoids as tick 0 (condition 3).
    PhaseClosure { depth: u32, phase_count: u8 },
}

impl fmt::Display for RingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RingError::Body(e) => write!(f, "ring: invalid body: {e}"),
            RingError::PortMismatch { inputs, outputs } => {
                write!(f, "ring: wrap is not identity-shaped: {outputs} outputs vs {inputs} inputs")
            }
            RingError::OutputSkew { depths } => {
                write!(f, "ring: output ports arrive on different ticks {depths:?}")
            }
            RingError::PhaseClosure { depth, phase_count } => {
                write!(f, "ring: body depth {depth} is not a multiple of P = {phase_count}")
            }
        }
    }
}

/// Append K0 chains so (a) every output port arrives at one uniform
/// tick D and (b) `D ≡ 0 (mod P)`, with D ≥ P. Returns a new body
/// satisfying ADR-0033 D2 conditions 2–3; inserts the minimal number of
/// buffer stages (output flush + ≤ P−1 round-up stages per rail).
///
/// This materializes the K0s — the prototype stand-in for
/// `balance_compact`'s delay-count edges (ADR-0033 D4), affordable only
/// because prototype bodies are tiny.
pub fn pad_ring(body: &PhasedCellNetlist) -> Result<PhasedCellNetlist, RingError> {
    body.validate().map_err(RingError::Body)?;

    let d_max = body.output_depths.iter().copied().max().unwrap_or(0);
    let p = u32::from(PHASE_COUNT);
    // Round up to a positive multiple of P.
    let target = (d_max.max(1)).div_ceil(p) * p;

    let mut out = body.clone();
    for (port, depth) in out.output_depths.iter_mut().enumerate() {
        while *depth < target {
            let src = out.outputs[port];
            let dst = RailPair {
                line0: LineId(out.num_lines),
                line1: LineId(out.num_lines + 1),
            };
            out.num_lines += 2;
            out.cells.push(Cell {
                kind: CellKind::K0,
                depth: *depth,
                phase: (*depth % p) as u8,
                inputs: vec![src],
                outputs: vec![dst],
            });
            out.outputs[port] = dst;
            *depth += 1;
        }
    }
    out.validate().map_err(RingError::Body)?;
    Ok(out)
}

/// One balanced step body closed into a ring (ADR-0033 D1). The wrap is
/// the identity port bijection; revolutions live with the caller.
#[derive(Clone, Debug)]
pub struct RingNetlist {
    body: PhasedCellNetlist,
    /// Uniform boundary depth D (a multiple of P).
    pub depth: u32,
}

impl RingNetlist {
    /// Close `body` into a ring, checking all four ADR-0033 D2
    /// conditions. Bodies that fail 2–3 can be repaired with
    /// [`pad_ring`].
    pub fn new(body: PhasedCellNetlist) -> Result<Self, RingError> {
        body.validate().map_err(RingError::Body)?;
        if body.inputs.len() != body.outputs.len() {
            return Err(RingError::PortMismatch {
                inputs: body.inputs.len(),
                outputs: body.outputs.len(),
            });
        }
        let depths = &body.output_depths;
        let d = depths.first().copied().unwrap_or(0);
        if depths.iter().any(|&x| x != d) {
            return Err(RingError::OutputSkew { depths: depths.clone() });
        }
        if d == 0 || d % u32::from(PHASE_COUNT) != 0 {
            return Err(RingError::PhaseClosure { depth: d, phase_count: PHASE_COUNT });
        }
        Ok(RingNetlist { body, depth: d })
    }

    /// The recirculated body — the machine's entire cell inventory,
    /// independent of how many revolutions it runs (the observable
    /// area difference vs unroll, ADR-0033).
    pub fn body(&self) -> &PhasedCellNetlist {
        &self.body
    }

    /// Run `revolutions` trips around the ring: simulate the body, feed
    /// outputs back through the identity wrap, repeat. Token-for-token,
    /// revolution r is copy r of the unrolled circuit, so the
    /// differential oracle is iterating the body's function.
    pub fn run(&self, inputs: &[bool], revolutions: u32) -> Result<Vec<bool>, SimError> {
        let mut state = inputs.to_vec();
        for _ in 0..revolutions {
            state = simulate(&self.body, &state)?.outputs;
        }
        Ok(state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use theseus_core::Type;

    fn bit() -> Type {
        Type::sum(Type::One, Type::One)
    }

    fn bits(n: usize) -> Type {
        (1..n).fold(bit(), |t, _| Type::prod(bit(), t))
    }

    /// One K1 Fredkin on ports [c, a, b], outputs at depth 1 — the
    /// tiniest nontrivial step body.
    fn fredkin_body() -> PhasedCellNetlist {
        let pair = |i: u32| RailPair { line0: LineId(2 * i), line1: LineId(2 * i + 1) };
        let ins: Vec<RailPair> = (0..3).map(pair).collect();
        let outs: Vec<RailPair> = (3..6).map(pair).collect();
        PhasedCellNetlist {
            input_ty: bits(3),
            output_ty: bits(3),
            inputs: ins.clone(),
            outputs: outs.clone(),
            output_depths: vec![1, 1, 1],
            cells: vec![Cell { kind: CellKind::K1, depth: 0, phase: 0, inputs: ins, outputs: outs }],
            num_lines: 12,
        }
    }

    #[test]
    fn unpadded_body_fails_phase_closure() {
        // D = 1 is not a multiple of P = 4: the wrap would hand tokens
        // to phase-0 cells from a phase-1 boundary.
        assert!(matches!(
            RingNetlist::new(fredkin_body()),
            Err(RingError::PhaseClosure { depth: 1, .. })
        ));
    }

    #[test]
    fn pad_ring_reaches_phase_closure_minimally() {
        let padded = pad_ring(&fredkin_body()).unwrap();
        // Flushed to D = 4 with exactly 3 K0 stages per port.
        assert_eq!(padded.output_depths, vec![4, 4, 4]);
        assert_eq!(
            padded.cells.iter().filter(|c| c.kind == CellKind::K0).count(),
            9
        );
        let ring = RingNetlist::new(padded).unwrap();
        assert_eq!(ring.depth, 4);
        assert_eq!(ring.depth % u32::from(PHASE_COUNT), 0);
    }

    #[test]
    fn ring_matches_iterated_oracle() {
        // Differential oracle (the theseus norm): R revolutions of the
        // ring == the body's function iterated R times at the value
        // level, over the whole 2^3 input domain.
        let ring = RingNetlist::new(pad_ring(&fredkin_body()).unwrap()).unwrap();
        for x in 0u32..8 {
            let input: Vec<bool> = (0..3).map(|i| (x >> i) & 1 == 1).collect();
            let mut oracle = input.clone();
            for r in 0..=5u32 {
                assert_eq!(
                    ring.run(&input, r).unwrap(),
                    oracle,
                    "input {input:?}, revolutions {r}"
                );
                oracle = CellKind::K1.eval(&oracle);
            }
        }
    }

    #[test]
    fn ring_area_is_revolution_independent() {
        // The observable difference vs unroll (ADR-0033): the cell
        // inventory is O(|body|) however many revolutions run, where
        // the unrolled circuit is O(R * |body|).
        let ring = RingNetlist::new(pad_ring(&fredkin_body()).unwrap()).unwrap();
        let area = ring.body().cells.len();
        for r in [1u32, 16, 64] {
            ring.run(&[true, true, false], r).unwrap();
            assert_eq!(ring.body().cells.len(), area);
        }
    }

    #[test]
    fn even_revolutions_restore_input() {
        // K1 is self-inverse, so every even revolution count is the
        // identity — recirculation composes adjoints on the same
        // silicon (C2.5 in miniature).
        let ring = RingNetlist::new(pad_ring(&fredkin_body()).unwrap()).unwrap();
        let input = vec![true, false, true];
        assert_eq!(ring.run(&input, 6).unwrap(), input);
    }

    #[test]
    fn port_mismatch_rejected() {
        let mut body = pad_ring(&fredkin_body()).unwrap();
        // Drop one output port: the wrap can no longer be an identity.
        body.outputs.pop();
        body.output_depths.pop();
        // (validate() will reject the dangling lines first — either
        // error is a refusal to close the ring.)
        assert!(RingNetlist::new(body).is_err());
    }
}
