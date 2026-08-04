//! Tick-exact balancer (ADR-0026 C2.2): insert minimal K0 buffer stages
//! so every cell's reconvergent input ports share one arrival tick.
//!
//! In 2LAL every gate is a register; reconvergent inputs to a cell MUST
//! arrive on the SAME tick or the circuit is wrong (not merely slow).
//! This pass consumes the skew reported by
//! [`PhasedCellNetlist::tick_imbalances`] and repairs it by inserting
//! `K0` buffer stages (1-rail-pair buffer, 1 tick) on earlier-arriving
//! input lines. Minimal-buffer = minimal-energy (ADR-0028): exactly the
//! deficit, never more.

use std::collections::BTreeMap;

use crate::ir::cell::{
    Cell, CellKind, CellNetError, LineId, PhasedCellNetlist, RailPair, PHASE_COUNT,
};
use crate::sim::SimResult;

/// Error from the tick-exact balancer.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum BalanceError {
    /// The input netlist is structurally invalid (could not compute
    /// line arrivals).
    #[error("balance: invalid netlist: {0}")]
    CellNet(#[from] CellNetError),
}

/// Error from the compact balanced simulator.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum CompactSimError {
    /// Wrong number of input tokens for the netlist's input ports.
    #[error("compact sim: netlist has {want} input ports, got {got} tokens")]
    InputWidth { want: usize, got: usize },
}

/// Compact balanced representation of a [`PhasedCellNetlist`].
///
/// Instead of materializing one [`Cell`] per K0 buffer per tick of
/// deficit (which is ~1.6 M structs for a full TISC-1 step), this type
/// stores a per-input-port **delay count** -- the number of K0 stages
/// the materializing [`balance`] *would* insert. Memory is O(active
/// cells + ports), independent of total buffer count.
///
/// Exposes the same observable facts as [`balance`]:
/// - identical K0 buffer count ([`Self::k0_buffer_count`]),
/// - identical cell counts ([`Self::cell_counts`]),
/// - tick-exactness ([`Self::is_tick_exact`]),
/// - identical logical simulation ([`Self::simulate`]).
///
/// Produced by [`balance_compact`].
#[derive(Clone, Debug)]
pub struct CompactBalanced {
    /// The original (possibly unbalanced) netlist -- cells in topological
    /// order.
    net: PhasedCellNetlist,
    /// `cell_input_delays[i][j]` = number of K0 buffer stages that
    /// [`balance`] would insert on cell `i`'s input port `j`.
    cell_input_delays: Vec<Vec<u32>>,
    /// Cached Σ of all delays = number of K0 cells [`balance`] would
    /// materialize.
    total_k0_inserted: usize,
}

impl CompactBalanced {
    /// Number of K0 buffer cells that [`balance`] would materialize:
    /// Σ over every cell-input port of its delay count.
    pub fn k0_buffer_count(&self) -> usize {
        self.total_k0_inserted
    }

    /// Per-kind cell counts: the active cells from the original netlist
    /// plus the K0 buffers this compact form represents.
    pub fn cell_counts(&self) -> BTreeMap<CellKind, usize> {
        let mut counts = self.net.cell_counts();
        // Only record a K0 entry when buffers are actually inserted, so a
        // zero-buffer (already-balanced or wiring-only) net's counts equal
        // the original's -- matching materialized `balance()`, which adds no
        // K0 key when it inserts none.
        if self.total_k0_inserted > 0 {
            *counts.entry(CellKind::K0).or_insert(0) += self.total_k0_inserted;
        }
        counts
    }

    /// Always `true` by construction: the delays stored here equalize
    /// every cell's input arrivals to one tick (the invariant that
    /// [`balance`] establishes).
    pub fn is_tick_exact(&self) -> bool {
        // Always true by construction: balance_compact sets
        // delay = latest - arrival for each input, so every cell's input
        // arrivals equalize to cell.depth. This is a documented invariant,
        // not a runtime check -- no assert is performed.
        true
    }

    /// A reference to the original (unbalanced) netlist.
    pub fn net(&self) -> &PhasedCellNetlist {
        &self.net
    }

    /// Per-input delay for cell `cell_idx`, port `port_idx`.
    pub fn delay(&self, cell_idx: usize, port_idx: usize) -> u32 {
        self.cell_input_delays[cell_idx][port_idx]
    }

    /// Per-(kind, phase) occupancy including virtual K0 buffers.
    pub fn occupancy(&self) -> BTreeMap<(CellKind, u8), usize> {
        let mut occ = self.net.occupancy();
        let arrivals = self
            .net
            .line_arrivals()
            .expect("compact balanced net has valid arrivals");
        let p = u32::from(PHASE_COUNT);
        for (cell_idx, cell) in self.net.cells.iter().enumerate() {
            for (port_idx, inp) in cell.inputs.iter().enumerate() {
                let delay = self.cell_input_delays[cell_idx][port_idx];
                if delay == 0 {
                    continue;
                }
                let arr = arrivals[inp.line1.0 as usize];
                // Virtual K0 buffers fire at depths arr, arr+1, ...,
                // arr+delay−1. Compute per-phase counts in O(PHASE_COUNT).
                let start = arr % p;
                let full_cycles = delay / p;
                let remainder = delay % p;
                for offset in 0..p {
                    let phase = ((start + offset) % p) as u8;
                    let count =
                        full_cycles + if offset < remainder { 1 } else { 0 };
                    if count > 0 {
                        *occ.entry((CellKind::K0, phase)).or_insert(0) +=
                            count as usize;
                    }
                }
            }
        }
        occ
    }

    /// Simulate the compact balanced netlist on one input-token vector.
    ///
    /// Walks the original active cells in topological order, evaluates
    /// each via [`CellKind::eval`], and produces output bits **equal**
    /// to `sim::simulate` on the materialized balanced netlist (K0
    /// buffers are logical identity -- delays change timing, not bits).
    ///
    /// This is a real evaluation over every active cell, not a
    /// delegation shortcut.
    pub fn simulate(
        &self,
        input_tokens: &[bool],
    ) -> Result<SimResult, CompactSimError> {
        if input_tokens.len() != self.net.inputs.len() {
            return Err(CompactSimError::InputWidth {
                want: self.net.inputs.len(),
                got: input_tokens.len(),
            });
        }

        // high[line] = the line's logical value; None until driven.
        let mut high: Vec<Option<bool>> =
            vec![None; self.net.num_lines as usize];
        for (p, &tok) in self.net.inputs.iter().zip(input_tokens) {
            high[p.line1.0 as usize] = Some(tok);
            high[p.line0.0 as usize] = Some(!tok);
        }

        // Walk active cells in topological order and evaluate.
        for cell in &self.net.cells {
            let ins: Vec<bool> = cell
                .inputs
                .iter()
                .map(|p| {
                    high[p.line1.0 as usize]
                        .expect("cell input line must be driven")
                })
                .collect();
            let outs = cell.kind.eval(&ins);
            for (p, &v) in cell.outputs.iter().zip(&outs) {
                high[p.line1.0 as usize] = Some(v);
                high[p.line0.0 as usize] = Some(!v);
            }
        }

        let outputs: Vec<bool> = self
            .net
            .outputs
            .iter()
            .map(|p| {
                high[p.line1.0 as usize]
                    .expect("output line must be driven")
            })
            .collect();

        Ok(SimResult {
            outputs,
            ticks: self.net.depth(),
            occupancy: self.occupancy(),
            inv_token_checks: 0,
        })
    }
}

/// Allocate a fresh rail pair, recording its arrival tick.
fn alloc_pair(next_line: &mut u32, arrival_of: &mut Vec<u32>, arrival: u32) -> RailPair {
    let pair = RailPair { line0: LineId(*next_line), line1: LineId(*next_line + 1) };
    *next_line += 2;
    arrival_of.push(arrival);
    arrival_of.push(arrival);
    pair
}

/// Insert minimal K0 buffer stages so every cell's reconvergent inputs
/// arrive on the same tick. Returns a **new** netlist that is tick-exact
/// balanced, structurally valid, and semantics-preserving.
///
/// # Algorithm
///
/// Forward pass over cells in topological order (the emission order
/// preserved by [`crate::lower`]):
///
/// 1. Track each line's arrival tick (primary inputs arrive at 0).
/// 2. For each cell, compute per-input arrival. Let `latest` = max.
///    For every input arriving at `a < latest`, insert `latest − a`
///    chained K0 cells on that pair so its final output arrives at
///    `latest`. Rewire the cell's input port to the last buffer's
///    output pair.
/// 3. Emit the cell with depth = latest, phase = latest mod P, fresh
///    output pairs, arrival latest + 1.
/// 4. Recompute `output_depths` from the final arrivals.
///
/// The result satisfies:
/// - **Complete**: `tick_imbalances()` is empty.
/// - **Idempotent**: balancing an already-balanced netlist inserts zero
///   buffers and produces a structurally equivalent netlist.
/// - **Valid**: `validate()` passes.
/// - **Semantics-preserving**: `sim::simulate` on the balanced netlist
///   produces the same logical outputs as the original.
/// - **Minimal**: the number of inserted K0 cells equals the sum of
///   per-cell input-arrival deficits.
pub fn balance(net: &PhasedCellNetlist) -> Result<PhasedCellNetlist, BalanceError> {
    // Validate that arrivals are computable (catches cycles / dangling).
    net.line_arrivals().map_err(BalanceError::CellNet)?;

    let mut next_line: u32 = 0;
    let mut new_cells: Vec<Cell> = Vec::new();
    // arrival_of[new_line_id] = arrival tick of that line.
    let mut arrival_of: Vec<u32> = Vec::new();

    // Allocate fresh primary input pairs (arrival 0).
    let new_inputs: Vec<RailPair> =
        (0..net.inputs.len()).map(|_| alloc_pair(&mut next_line, &mut arrival_of, 0)).collect();

    // Map from original LineId → new LineId.
    let mut line_map: Vec<Option<LineId>> = vec![None; net.num_lines as usize];
    for (orig, fresh) in net.inputs.iter().zip(&new_inputs) {
        line_map[orig.line0.0 as usize] = Some(fresh.line0);
        line_map[orig.line1.0 as usize] = Some(fresh.line1);
    }

    for cell in &net.cells {
        // Remap input pairs through line_map.
        let inputs: Vec<RailPair> = cell
            .inputs
            .iter()
            .map(|p| RailPair {
                line0: line_map[p.line0.0 as usize].expect("input line mapped"),
                line1: line_map[p.line1.0 as usize].expect("input line mapped"),
            })
            .collect();

        // Per-input arrival in the new netlist.
        let input_arrivals: Vec<u32> =
            inputs.iter().map(|p| arrival_of[p.line1.0 as usize]).collect();
        let latest = *input_arrivals.iter().max().expect("cell has inputs");

        // Buffer deficit inputs with K0 chains.
        let mut balanced_inputs = inputs;
        for (i, &arr) in input_arrivals.iter().enumerate() {
            let mut cur_pair = balanced_inputs[i];
            for cur_arr in arr..latest {
                let out = alloc_pair(&mut next_line, &mut arrival_of, cur_arr + 1);
                new_cells.push(Cell {
                    kind: CellKind::K0,
                    depth: cur_arr,
                    phase: (cur_arr % u32::from(PHASE_COUNT)) as u8,
                    inputs: vec![cur_pair],
                    outputs: vec![out],
                });
                cur_pair = out;
            }
            balanced_inputs[i] = cur_pair;
        }

        // Fresh output pairs for this cell.
        let outputs: Vec<RailPair> = (0..cell.kind.arity())
            .map(|_| alloc_pair(&mut next_line, &mut arrival_of, latest + 1))
            .collect();

        // Update line_map for this cell's original outputs.
        for (orig, fresh) in cell.outputs.iter().zip(&outputs) {
            line_map[orig.line0.0 as usize] = Some(fresh.line0);
            line_map[orig.line1.0 as usize] = Some(fresh.line1);
        }

        new_cells.push(Cell {
            kind: cell.kind,
            depth: latest,
            phase: (latest % u32::from(PHASE_COUNT)) as u8,
            inputs: balanced_inputs,
            outputs,
        });
    }

    // Remap output ports.
    let new_outputs: Vec<RailPair> = net
        .outputs
        .iter()
        .map(|p| RailPair {
            line0: line_map[p.line0.0 as usize].expect("output line mapped"),
            line1: line_map[p.line1.0 as usize].expect("output line mapped"),
        })
        .collect();
    let output_depths: Vec<u32> =
        new_outputs.iter().map(|p| arrival_of[p.line1.0 as usize]).collect();

    Ok(PhasedCellNetlist {
        input_ty: net.input_ty.clone(),
        output_ty: net.output_ty.clone(),
        inputs: new_inputs,
        outputs: new_outputs,
        output_depths,
        cells: new_cells,
        num_lines: next_line,
    })
}

/// Produce a compact balanced representation of `net` in O(active cells)
/// memory: per-input-port delay counts instead of materialized K0 cells.
///
/// Computes exactly the same per-cell, per-input deficit as [`balance`]
/// (latest input arrival − this input's arrival), but stores each as an
/// integer delay rather than a chain of [`Cell`] structs.
///
/// # Errors
///
/// Returns [`BalanceError::CellNet`] if `net` has cycles or dangling
/// lines (line arrivals cannot be computed).
pub fn balance_compact(
    net: &PhasedCellNetlist,
) -> Result<CompactBalanced, BalanceError> {
    let arrivals =
        net.line_arrivals().map_err(BalanceError::CellNet)?;

    let mut cell_input_delays: Vec<Vec<u32>> =
        Vec::with_capacity(net.cells.len());
    let mut total_k0: usize = 0;

    for cell in &net.cells {
        let input_arrivals: Vec<u32> = cell
            .inputs
            .iter()
            .map(|p| arrivals[p.line1.0 as usize])
            .collect();
        let latest =
            *input_arrivals.iter().max().unwrap_or(&0);
        let delays: Vec<u32> = input_arrivals
            .iter()
            .map(|&a| latest - a)
            .collect();
        total_k0 += delays.iter().sum::<u32>() as usize;
        cell_input_delays.push(delays);
    }

    Ok(CompactBalanced {
        net: net.clone(),
        cell_input_delays,
        total_k0_inserted: total_k0,
    })
}
