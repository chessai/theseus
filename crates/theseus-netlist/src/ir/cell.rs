//! The phased cell netlist (ADR-0026 C2.7): dual-rail, cell-mapped,
//! phase-attributed — the adiabatic backend's own artifact, with its own
//! tick-level simulator (next slice).
//!
//! # Encoding
//!
//! Every logical bit is a **rail pair**: two token lines, a 0-line and a
//! 1-line; exactly one pulses per token (1-of-2 — the structural face of
//! INV-TOKEN, ADR-0031). At netlist granularity a token line is atomic;
//! its own electrical (rail, mirror) SR pair lives below this IR, inside
//! the characterized cell (`theseus-spice::cswap` module docs).
//!
//! # Cells and phases
//!
//! Cells are the k-CSWAP family (ADR-0027): `K0` buffer, `K1` Fredkin,
//! `K2` 2-controlled swap. Every cell takes one tick and *is* a register
//! (2LAL: data advances one stage per clock phase, ADR-0026). A cell at
//! `depth` d fires on tick d — consumes inputs arriving at d, produces
//! outputs arriving at d + 1 — and `phase = d mod P` (C2.3) says which
//! clock trapezoid drives it.
//!
//! # What the next slice consumes
//!
//! - the phased-sim ticks over `cells` (topological order) using
//!   [`CellKind::eval`] + [`PhasedCellNetlist::line_arrivals`];
//! - occupancy per `(kind, phase)` is [`PhasedCellNetlist::occupancy`];
//! - energy annotation (ADR-0028) is Σ over cells of
//!   `char_db.query(kind.char_db_key(), t_ramp)`;
//! - the tick-exact balancer's worklist (C2.2, deferred) is
//!   [`PhasedCellNetlist::tick_imbalances`] plus the skew visible in
//!   `output_depths`.

use core::fmt;
use std::collections::BTreeMap;

use theseus_core::Type;

/// Number of clock phases `P` in the rotation (ADR-0026 C2.3: 4 for 2LAL,
/// 8 under S2LAL). Moves into `TargetProfile` (C2.8) when profiles land.
pub const PHASE_COUNT: u8 = 4;

/// One token line. Lines are numbered densely `0..num_lines`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineId(pub u32);

/// A logical bit: a 1-of-2 pair of token lines. `line1` pulses for a `1`
/// token, `line0` for a `0` token. NOT is swapping the two roles — pure
/// rewiring, zero cells (ADR-0031 "NOT-in-Permute").
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RailPair {
    pub line0: LineId,
    pub line1: LineId,
}

impl RailPair {
    /// The pair with the two token-line roles exchanged: logical NOT,
    /// zero cells.
    pub fn swapped(self) -> RailPair {
        RailPair { line0: self.line1, line1: self.line0 }
    }
}

/// k-CSWAP cell kind (ADR-0027). All four are reversible and
/// self-inverse; a controlled swap only routes charge, so the count of
/// asserted rails is invariant per token (INV-TOKEN).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CellKind {
    /// Buffer/latch: one 2LAL stage, one logical signal.
    K0,
    /// Fredkin: `(c, a, b) → (c, c?b:a, c?a:b)`.
    K1,
    /// 2-controlled swap: swap `(a, b)` iff `c1 ∧ c2`; controls pass.
    K2,
    /// 3-controlled swap: swap `(a, b)` iff `c1 ∧ c2 ∧ c3`; controls pass.
    K3,
}

impl CellKind {
    /// Number of logical ports (rail pairs), equal on input and output:
    /// `K0` is `[a]`, `K1` is `[c, a, b]`, `K2` is `[c1, c2, a, b]` —
    /// matching `theseus-spice::cswap::cswap_signals`.
    pub fn arity(self) -> usize {
        match self {
            CellKind::K0 => 1,
            CellKind::K1 => 3,
            CellKind::K2 => 4,
            CellKind::K3 => 5,
        }
    }

    /// Characterization-database key for this cell (the `cswap-k{k}`
    /// entries built by `theseus-spice::chardb::build_char_db`). The next
    /// slice's energy annotation is Σ over cells of
    /// `char_db.query(kind.char_db_key(), t_ramp)` (ADR-0028).
    pub fn char_db_key(self) -> &'static str {
        match self {
            CellKind::K0 => "cswap-k0",
            CellKind::K1 => "cswap-k1",
            CellKind::K2 => "cswap-k2",
            CellKind::K3 => "cswap-k3",
        }
    }

    /// The cell's truth function on logical bits, port order as in
    /// [`CellKind::arity`]. Mirrors `theseus-spice::cswap::cswap_out`
    /// (cross-checked in this crate's tests). Self-inverse for all kinds.
    pub fn eval(self, bits: &[bool]) -> Vec<bool> {
        assert_eq!(bits.len(), self.arity(), "cell port-count mismatch");
        let k = match self {
            CellKind::K0 => 0,
            CellKind::K1 => 1,
            CellKind::K2 => 2,
            CellKind::K3 => 3,
        };
        let mut out = bits.to_vec();
        if k > 0 && bits[..k].iter().all(|&c| c) {
            out.swap(k, k + 1);
        }
        out
    }
}

/// One placed cell: kind + firing tick + phase + dual-rail connections.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cell {
    pub kind: CellKind,
    /// The tick this cell fires on: consumes inputs arriving at `depth`,
    /// produces outputs arriving at `depth + 1`.
    pub depth: u32,
    /// `depth % PHASE_COUNT` (ADR-0026 C2.3: phase = topological depth
    /// mod P).
    pub phase: u8,
    /// Input rail pairs, `arity(kind)` of them, port order per
    /// [`CellKind::arity`].
    pub inputs: Vec<RailPair>,
    /// Output rail pairs, same count and port order (INV-TOKEN: the cell
    /// conserves rail pairs; it only routes).
    pub outputs: Vec<RailPair>,
}

/// The phased cell netlist: cells + rail-pair wires + typed ports.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PhasedCellNetlist {
    /// Source-level boundary types (kept for decode/encode and reports;
    /// the rails below are their dual-rail image).
    pub input_ty: Type,
    pub output_ty: Type,
    /// Input port bits, in flattened type order, all arriving at tick 0.
    pub inputs: Vec<RailPair>,
    /// Output port bits, in flattened type order.
    pub outputs: Vec<RailPair>,
    /// Arrival tick of each output bit. Skew here is *recorded, not yet
    /// repaired*: the tick-exact balancer (ADR-0026 C2.2) is a later
    /// slice; until it runs, mismatched entries mean the netlist is not
    /// yet executable as a circuit.
    pub output_depths: Vec<u32>,
    /// Cells in topological (emission) order.
    pub cells: Vec<Cell>,
    /// Token lines are numbered densely `0..num_lines`.
    pub num_lines: u32,
}

/// Structural violation found by [`PhasedCellNetlist::validate`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CellNetError {
    /// A cell's port counts don't match its kind (INV-TOKEN structural:
    /// rail-pair count must be conserved through every cell).
    Arity { cell: usize, kind: CellKind, inputs: usize, outputs: usize },
    /// `phase ≠ depth mod P`.
    Phase { cell: usize, depth: u32, phase: u8 },
    /// A rail pair reuses one line for both roles (1-of-2 needs two
    /// distinct lines).
    DegeneratePair { line: LineId },
    /// A line id is out of `0..num_lines`.
    LineRange { line: LineId },
    /// A line is not driven exactly once (by a primary input or one cell
    /// output).
    LineDrivers { line: LineId, drivers: usize },
    /// A line is not consumed exactly once (by one cell input or a
    /// primary output). Fanout needs buffer trees (C2.8), a later slice;
    /// v0 netlists are linear.
    LineConsumers { line: LineId, consumers: usize },
    /// `outputs` and `output_depths` disagree in length.
    PortWidths { outputs: usize, output_depths: usize },
    /// A cell's arrival depths never resolve (cycle or dangling input).
    Unresolved { cell: usize },
    /// A cell's recorded `depth` isn't the arrival tick of its latest
    /// input.
    CellDepth { cell: usize, expected: u32, got: u32 },
    /// A recorded output depth isn't the arrival tick of that port's
    /// lines.
    OutputDepth { port: usize, expected: u32, got: u32 },
    /// The two lines of a consumed pair arrive on different ticks.
    PairSkew { line0_depth: u32, line1_depth: u32 },
}

impl fmt::Display for CellNetError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CellNetError::Arity { cell, kind, inputs, outputs } => write!(
                f,
                "cell {cell}: {kind:?} wants {} ports, has {inputs} in / {outputs} out",
                kind.arity()
            ),
            CellNetError::Phase { cell, depth, phase } => {
                write!(f, "cell {cell}: phase {phase} ≠ depth {depth} mod {PHASE_COUNT}")
            }
            CellNetError::DegeneratePair { line } => {
                write!(f, "rail pair uses line {} for both roles", line.0)
            }
            CellNetError::LineRange { line } => write!(f, "line {} out of range", line.0),
            CellNetError::LineDrivers { line, drivers } => {
                write!(f, "line {} has {drivers} drivers (want 1)", line.0)
            }
            CellNetError::LineConsumers { line, consumers } => {
                write!(f, "line {} has {consumers} consumers (want 1)", line.0)
            }
            CellNetError::PortWidths { outputs, output_depths } => {
                write!(f, "{outputs} output ports but {output_depths} output depths")
            }
            CellNetError::Unresolved { cell } => {
                write!(f, "cell {cell}: arrival depths never resolve")
            }
            CellNetError::CellDepth { cell, expected, got } => {
                write!(f, "cell {cell}: depth {got}, latest input arrives at {expected}")
            }
            CellNetError::OutputDepth { port, expected, got } => {
                write!(f, "output {port}: recorded depth {got}, lines arrive at {expected}")
            }
            CellNetError::PairSkew { line0_depth, line1_depth } => {
                write!(f, "pair lines arrive at ticks {line0_depth} vs {line1_depth}")
            }
        }
    }
}

/// A cell whose inputs arrive on different ticks — a *wrong circuit*
/// under ADR-0026 C2.2 until the tick-exact balancer (later slice)
/// inserts `K0` stages to equalize arrivals.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TickImbalance {
    /// Index into [`PhasedCellNetlist::cells`].
    pub cell: usize,
    /// Arrival tick per input port.
    pub arrivals: Vec<u32>,
}

impl PhasedCellNetlist {
    /// Per-(kind, phase) cell counts — the occupancy table the phased-sim
    /// and the energy annotation index by.
    pub fn occupancy(&self) -> BTreeMap<(CellKind, u8), usize> {
        let mut map = BTreeMap::new();
        for c in &self.cells {
            *map.entry((c.kind, c.phase)).or_insert(0) += 1;
        }
        map
    }

    /// Per-kind cell counts (the char-DB lookup multiplicities).
    pub fn cell_counts(&self) -> BTreeMap<CellKind, usize> {
        let mut map = BTreeMap::new();
        for c in &self.cells {
            *map.entry(c.kind).or_insert(0) += 1;
        }
        map
    }

    /// Total pipeline depth in ticks: the latest arrival anywhere.
    pub fn depth(&self) -> u32 {
        let outs = self.output_depths.iter().copied().max().unwrap_or(0);
        let cells = self.cells.iter().map(|c| c.depth + 1).max().unwrap_or(0);
        outs.max(cells)
    }

    /// Arrival tick of every line, computed from the graph (primary
    /// inputs arrive at 0; a cell's outputs arrive one tick after its
    /// latest input; rewiring is free). Errors if some line is never
    /// driven or some cell never resolves.
    pub fn line_arrivals(&self) -> Result<Vec<u32>, CellNetError> {
        let n = self.num_lines as usize;
        let mut arrival: Vec<Option<u32>> = vec![None; n];
        let idx = |line: LineId| -> Result<usize, CellNetError> {
            let i = line.0 as usize;
            if i < n { Ok(i) } else { Err(CellNetError::LineRange { line }) }
        };

        for p in &self.inputs {
            arrival[idx(p.line0)?] = Some(0);
            arrival[idx(p.line1)?] = Some(0);
        }

        let mut resolved = vec![false; self.cells.len()];
        let mut remaining = self.cells.len();
        while remaining > 0 {
            let mut progress = false;
            for (i, cell) in self.cells.iter().enumerate() {
                if resolved[i] {
                    continue;
                }
                let mut latest = 0u32;
                let mut ready = true;
                for p in &cell.inputs {
                    match (arrival[idx(p.line0)?], arrival[idx(p.line1)?]) {
                        (Some(d0), Some(d1)) => latest = latest.max(d0).max(d1),
                        _ => {
                            ready = false;
                            break;
                        }
                    }
                }
                if !ready {
                    continue;
                }
                for p in &cell.outputs {
                    arrival[idx(p.line0)?] = Some(latest + 1);
                    arrival[idx(p.line1)?] = Some(latest + 1);
                }
                resolved[i] = true;
                remaining -= 1;
                progress = true;
            }
            if !progress {
                let cell = resolved.iter().position(|&r| !r).unwrap_or(0);
                return Err(CellNetError::Unresolved { cell });
            }
        }

        arrival
            .into_iter()
            .enumerate()
            .map(|(i, d)| {
                d.ok_or(CellNetError::LineDrivers { line: LineId(i as u32), drivers: 0 })
            })
            .collect()
    }

    /// Cells whose input ports arrive on different ticks (per-port
    /// arrival = its lines' arrival). Non-empty means the tick-exact
    /// balancer (C2.2, later slice) still owes `K0` insertions.
    pub fn tick_imbalances(&self) -> Result<Vec<TickImbalance>, CellNetError> {
        let arrival = self.line_arrivals()?;
        let mut out = Vec::new();
        for (i, cell) in self.cells.iter().enumerate() {
            let arrivals: Vec<u32> = cell
                .inputs
                .iter()
                .map(|p| arrival[p.line1.0 as usize])
                .collect();
            if arrivals.iter().min() != arrivals.iter().max() {
                out.push(TickImbalance { cell: i, arrivals });
            }
        }
        Ok(out)
    }

    /// Structural validation — everything about INV-TOKEN and the phase
    /// discipline that is checkable without running tokens:
    ///
    /// 1. every cell conserves rail pairs (`arity` in = out) — swap cells
    ///    only route charge;
    /// 2. every rail pair is two *distinct* lines (1-of-2 needs both);
    /// 3. every line is driven exactly once and consumed exactly once
    ///    (linear netlist; fanout waits for buffer trees, C2.8);
    /// 4. `phase = depth mod P` for every cell, and every recorded depth
    ///    agrees with graph arrival times.
    ///
    /// The dynamic half of INV-TOKEN (exactly one line of each pair high
    /// per token, every tick) is the phased-sim's per-tick assertion —
    /// next slice.
    ///
    /// Note: tick-exact balance (C2.2) is *not* required here; see
    /// [`Self::tick_imbalances`].
    pub fn validate(&self) -> Result<(), CellNetError> {
        if self.outputs.len() != self.output_depths.len() {
            return Err(CellNetError::PortWidths {
                outputs: self.outputs.len(),
                output_depths: self.output_depths.len(),
            });
        }

        let n = self.num_lines as usize;
        let check_pair = |p: &RailPair| -> Result<(), CellNetError> {
            for line in [p.line0, p.line1] {
                if line.0 as usize >= n {
                    return Err(CellNetError::LineRange { line });
                }
            }
            if p.line0 == p.line1 {
                return Err(CellNetError::DegeneratePair { line: p.line0 });
            }
            Ok(())
        };

        let mut drivers = vec![0usize; n];
        let mut consumers = vec![0usize; n];
        fn tally(p: &RailPair, counts: &mut [usize]) {
            counts[p.line0.0 as usize] += 1;
            counts[p.line1.0 as usize] += 1;
        }

        for p in &self.inputs {
            check_pair(p)?;
            tally(p, &mut drivers);
        }
        for p in &self.outputs {
            check_pair(p)?;
            tally(p, &mut consumers);
        }
        for (i, cell) in self.cells.iter().enumerate() {
            let want = cell.kind.arity();
            if cell.inputs.len() != want || cell.outputs.len() != want {
                return Err(CellNetError::Arity {
                    cell: i,
                    kind: cell.kind,
                    inputs: cell.inputs.len(),
                    outputs: cell.outputs.len(),
                });
            }
            if cell.phase != (cell.depth % u32::from(PHASE_COUNT)) as u8 {
                return Err(CellNetError::Phase { cell: i, depth: cell.depth, phase: cell.phase });
            }
            for p in &cell.inputs {
                check_pair(p)?;
                tally(p, &mut consumers);
            }
            for p in &cell.outputs {
                check_pair(p)?;
                tally(p, &mut drivers);
            }
        }

        for i in 0..n {
            if drivers[i] != 1 {
                return Err(CellNetError::LineDrivers { line: LineId(i as u32), drivers: drivers[i] });
            }
            if consumers[i] != 1 {
                return Err(CellNetError::LineConsumers {
                    line: LineId(i as u32),
                    consumers: consumers[i],
                });
            }
        }

        // Depth discipline against graph arrival times.
        let arrival = self.line_arrivals()?;
        let pair_arrival = |p: &RailPair| -> Result<u32, CellNetError> {
            let d0 = arrival[p.line0.0 as usize];
            let d1 = arrival[p.line1.0 as usize];
            if d0 != d1 {
                return Err(CellNetError::PairSkew { line0_depth: d0, line1_depth: d1 });
            }
            Ok(d1)
        };
        for (i, cell) in self.cells.iter().enumerate() {
            let mut latest = 0u32;
            for p in &cell.inputs {
                latest = latest.max(pair_arrival(p)?);
            }
            if cell.depth != latest {
                return Err(CellNetError::CellDepth { cell: i, expected: latest, got: cell.depth });
            }
        }
        for (j, (p, &d)) in self.outputs.iter().zip(&self.output_depths).enumerate() {
            let got = pair_arrival(p)?;
            if got != d {
                return Err(CellNetError::OutputDepth { port: j, expected: got, got: d });
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bit() -> Type {
        Type::sum(Type::One, Type::One)
    }

    /// A hand-built single-K0 netlist on one bit.
    fn one_buffer() -> PhasedCellNetlist {
        let inp = RailPair { line0: LineId(0), line1: LineId(1) };
        let out = RailPair { line0: LineId(2), line1: LineId(3) };
        PhasedCellNetlist {
            input_ty: bit(),
            output_ty: bit(),
            inputs: vec![inp],
            outputs: vec![out],
            output_depths: vec![1],
            cells: vec![Cell {
                kind: CellKind::K0,
                depth: 0,
                phase: 0,
                inputs: vec![inp],
                outputs: vec![out],
            }],
            num_lines: 4,
        }
    }

    #[test]
    fn one_buffer_validates() {
        let net = one_buffer();
        assert_eq!(net.validate(), Ok(()));
        assert_eq!(net.depth(), 1);
        assert_eq!(net.line_arrivals().unwrap(), vec![0, 0, 1, 1]);
        assert!(net.tick_imbalances().unwrap().is_empty());
    }

    #[test]
    fn wrong_phase_rejected() {
        let mut net = one_buffer();
        net.cells[0].phase = 3;
        assert!(matches!(net.validate(), Err(CellNetError::Phase { .. })));
    }

    #[test]
    fn degenerate_pair_rejected() {
        let mut net = one_buffer();
        net.cells[0].outputs[0] = RailPair { line0: LineId(2), line1: LineId(2) };
        assert!(matches!(net.validate(), Err(CellNetError::DegeneratePair { .. })));
    }

    #[test]
    fn double_consumption_rejected() {
        let mut net = one_buffer();
        // Output port reads the *input* pair while the cell also consumes
        // it: lines 0/1 consumed twice, 2/3 never.
        net.outputs[0] = net.inputs[0];
        net.output_depths[0] = 0;
        assert!(matches!(
            net.validate(),
            Err(CellNetError::LineConsumers { .. } | CellNetError::LineDrivers { .. })
        ));
    }

    #[test]
    fn wrong_arity_rejected() {
        let mut net = one_buffer();
        net.cells[0].kind = CellKind::K1;
        assert!(matches!(net.validate(), Err(CellNetError::Arity { .. })));
    }

    #[test]
    fn wrong_depth_rejected() {
        let mut net = one_buffer();
        net.cells[0].depth = 2;
        net.cells[0].phase = 2;
        assert!(matches!(net.validate(), Err(CellNetError::CellDepth { .. })));
    }

    #[test]
    fn cell_eval_matches_characterized_truth_functions() {
        use theseus_spice::cswap::{cswap_out, cswap_truth_tokens};

        for (kind, k) in [(CellKind::K0, 0usize), (CellKind::K1, 1), (CellKind::K2, 2)] {
            assert_eq!(kind.char_db_key(), format!("cswap-k{k}"));
            for t in cswap_truth_tokens(k) {
                let bits: Vec<bool> = match kind {
                    CellKind::K0 => vec![t.a],
                    CellKind::K1 => vec![t.c1, t.a, t.b],
                    CellKind::K2 => vec![t.c1, t.c2, t.a, t.b],
                    CellKind::K3 => unreachable!("loop covers k<=2"),
                };
                let o = cswap_out(k, t);
                let want: Vec<bool> = match kind {
                    CellKind::K0 => vec![o.a],
                    CellKind::K1 => vec![o.c1, o.a, o.b],
                    CellKind::K2 => vec![o.c1, o.c2, o.a, o.b],
                    CellKind::K3 => unreachable!("loop covers k<=2"),
                };
                assert_eq!(kind.eval(&bits), want, "kind {kind:?}, token {t:?}");
            }
        }
    }

    #[test]
    fn cell_eval_self_inverse() {
        for kind in [CellKind::K0, CellKind::K1, CellKind::K2, CellKind::K3] {
            let n = kind.arity();
            for x in 0..1u32 << n {
                let bits: Vec<bool> = (0..n).map(|i| (x >> i) & 1 == 1).collect();
                assert_eq!(kind.eval(&kind.eval(&bits)), bits);
            }
        }
    }
}
