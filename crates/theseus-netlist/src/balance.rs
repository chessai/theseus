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

use core::fmt;

use crate::ir::cell::{
    Cell, CellKind, CellNetError, LineId, PhasedCellNetlist, RailPair, PHASE_COUNT,
};

/// Error from the tick-exact balancer.
#[derive(Debug, Clone, PartialEq)]
pub enum BalanceError {
    /// The input netlist is structurally invalid (could not compute
    /// line arrivals).
    CellNet(CellNetError),
}

impl fmt::Display for BalanceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BalanceError::CellNet(e) => write!(f, "balance: invalid netlist: {e}"),
        }
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
