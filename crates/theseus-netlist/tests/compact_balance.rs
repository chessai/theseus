//! Tests for the compact tick-exact balancer (`balance_compact`).
//!
//! Differential oracle: on every net where the materializing `balance()`
//! is affordable, compact and materialized must agree on K0 count, cell
//! counts, and simulation outputs. A separate large-net test proves the
//! O(active-cells) memory profile by constructing a net whose
//! materialized balance would be prohibitively expensive, then asserting
//! that `balance_compact` succeeds with the correct counts.

use std::collections::BTreeMap;

use theseus_core::{enumerate_values, type_of, Iso, Type};
use theseus_netlist::ir::cell::{Cell, CellKind, LineId, PhasedCellNetlist, RailPair, PHASE_COUNT};
use theseus_netlist::ir::typed::{Monoid, NodeKind, TypedNode};
use theseus_netlist::{
    balance, balance_compact, bits_of, lower, lower_iso, prim_from_bits, simulate,
};

mod common;
use common::*;

fn fredkin_prim() -> theseus_core::PrimDef {
    prim_from_bits("fredkin", three_bits(), three_bits(), |b| {
        CellKind::K1.eval(b)
    })
    .unwrap()
}

fn id_bit() -> TypedNode {
    TypedNode {
        input: bit(),
        output: bit(),
        kind: NodeKind::Permute {
            monoid: Monoid::Prod,
            sigma: vec![0],
        },
    }
}

// ── Helper: build (Reg(n) × id × id) ; fredkin ────────────────────────

fn reconvergent_with_skew(n: u32) -> (TypedNode, Iso) {
    let reg = TypedNode {
        input: bit(),
        output: bit(),
        kind: NodeKind::Reg(n as usize),
    };
    let par = TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Par(
            Box::new(reg),
            Box::new(TypedNode {
                input: Type::prod(bit(), bit()),
                output: Type::prod(bit(), bit()),
                kind: NodeKind::Par(Box::new(id_bit()), Box::new(id_bit())),
            }),
        ),
    };
    let fredkin = TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Prim(Box::new(fredkin_prim())),
    };
    let net = TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Seq(vec![par, fredkin]),
    };
    let iso = Iso::Prim(Box::new(fredkin_prim()));
    (net, iso)
}

// ── (1) Differential oracle on small reconvergent nets ─────────────────

#[test]
fn compact_matches_materialized_reconvergent_skew_1() {
    let (typed_net, iso) = reconvergent_with_skew(1);
    let original = lower(&typed_net).unwrap();
    let balanced = balance(&original).unwrap();
    let compact = balance_compact(&original).unwrap();

    // K0 buffer count: materialized balance inserts 2 K0 buffers
    // (deficit on ports 1 and 2 of the Fredkin = 1 each).
    let materialized_k0_delta = balanced
        .cells
        .iter()
        .filter(|c| c.kind == CellKind::K0)
        .count()
        - original
            .cells
            .iter()
            .filter(|c| c.kind == CellKind::K0)
            .count();
    assert_eq!(compact.k0_buffer_count(), materialized_k0_delta);

    // Cell counts match.
    assert_eq!(compact.cell_counts(), balanced.cell_counts());

    // is_tick_exact by construction.
    assert!(compact.is_tick_exact());

    // Simulation outputs match on full domain.
    let (in_ty, out_ty) = type_of(&iso).unwrap();
    for v in enumerate_values(&in_ty) {
        let bits = bits_of(&v, &in_ty).unwrap();
        let compact_out = compact.simulate(&bits).unwrap().outputs;
        let balanced_out = simulate(&balanced, &bits).unwrap().outputs;
        assert_eq!(compact_out, balanced_out, "outputs disagree on {v:?}");
        // Also matches original (unbalanced) sim would produce if it
        // were balanced -- here via core eval as the oracle.
        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(compact_out, bits_of(&expected, &out_ty).unwrap());
    }
}

#[test]
fn compact_matches_materialized_reconvergent_skew_2() {
    let (typed_net, iso) = reconvergent_with_skew(2);
    let original = lower(&typed_net).unwrap();
    let balanced = balance(&original).unwrap();
    let compact = balance_compact(&original).unwrap();

    let materialized_k0_delta = balanced
        .cells
        .iter()
        .filter(|c| c.kind == CellKind::K0)
        .count()
        - original
            .cells
            .iter()
            .filter(|c| c.kind == CellKind::K0)
            .count();
    assert_eq!(compact.k0_buffer_count(), materialized_k0_delta);
    assert_eq!(compact.cell_counts(), balanced.cell_counts());
    assert!(compact.is_tick_exact());

    let (in_ty, _out_ty) = type_of(&iso).unwrap();
    for v in enumerate_values(&in_ty) {
        let bits = bits_of(&v, &in_ty).unwrap();
        let compact_out = compact.simulate(&bits).unwrap().outputs;
        let balanced_out = simulate(&balanced, &bits).unwrap().outputs;
        assert_eq!(compact_out, balanced_out, "outputs disagree on {v:?}");
    }
}

// ── (2) Already-balanced net: zero buffers ─────────────────────────────

#[test]
fn compact_already_balanced_inserts_zero() {
    let iso = Iso::Prim(Box::new(fredkin_prim()));
    let original = lower_iso(&iso).unwrap();
    assert!(original.tick_imbalances().unwrap().is_empty());

    let compact = balance_compact(&original).unwrap();
    assert_eq!(compact.k0_buffer_count(), 0);
    assert_eq!(compact.cell_counts(), original.cell_counts());
    assert!(compact.is_tick_exact());

    let (in_ty, _) = type_of(&iso).unwrap();
    for v in enumerate_values(&in_ty) {
        let bits = bits_of(&v, &in_ty).unwrap();
        let compact_out = compact.simulate(&bits).unwrap().outputs;
        let balanced_out = simulate(&original, &bits).unwrap().outputs;
        assert_eq!(compact_out, balanced_out);
    }
}

// ── (3) Wiring-only net: no cells ──────────────────────────────────────

#[test]
fn compact_wiring_only() {
    let iso = Iso::Id(bit());
    let original = lower_iso(&iso).unwrap();
    assert!(original.cells.is_empty());

    let compact = balance_compact(&original).unwrap();
    assert_eq!(compact.k0_buffer_count(), 0);
    assert_eq!(compact.cell_counts(), BTreeMap::new());
    assert!(compact.is_tick_exact());

    for tok in [true, false] {
        let out = compact.simulate(&[tok]).unwrap().outputs;
        assert_eq!(out, vec![tok]);
    }
}

// ── (4) Reg chain: no reconvergence, no buffers ────────────────────────

#[test]
fn compact_reg_chain_no_buffers() {
    let reg3 = TypedNode {
        input: bit(),
        output: bit(),
        kind: NodeKind::Reg(3),
    };
    let original = lower(&reg3).unwrap();
    assert!(original.tick_imbalances().unwrap().is_empty());

    let balanced = balance(&original).unwrap();
    let compact = balance_compact(&original).unwrap();

    assert_eq!(compact.k0_buffer_count(), 0);
    assert_eq!(compact.cell_counts(), balanced.cell_counts());
    assert!(compact.is_tick_exact());

    for tok in [true, false] {
        let compact_out = compact.simulate(&[tok]).unwrap().outputs;
        let balanced_out = simulate(&balanced, &[tok]).unwrap().outputs;
        assert_eq!(compact_out, balanced_out);
    }
}

// ── (5) Multi-cell: (Reg(2) × id × id) ; fredkin ; fredkin ────────────

#[test]
fn compact_matches_materialized_multi_cell() {
    let reg2 = TypedNode {
        input: bit(),
        output: bit(),
        kind: NodeKind::Reg(2),
    };
    let par = TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Par(
            Box::new(reg2),
            Box::new(TypedNode {
                input: Type::prod(bit(), bit()),
                output: Type::prod(bit(), bit()),
                kind: NodeKind::Par(Box::new(id_bit()), Box::new(id_bit())),
            }),
        ),
    };
    let fredkin_node = || TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Prim(Box::new(fredkin_prim())),
    };
    let net = TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Seq(vec![par, fredkin_node(), fredkin_node()]),
    };
    let original = lower(&net).unwrap();
    let balanced = balance(&original).unwrap();
    let compact = balance_compact(&original).unwrap();

    let materialized_k0_delta = balanced
        .cells
        .iter()
        .filter(|c| c.kind == CellKind::K0)
        .count()
        - original
            .cells
            .iter()
            .filter(|c| c.kind == CellKind::K0)
            .count();
    assert_eq!(compact.k0_buffer_count(), materialized_k0_delta);
    assert_eq!(compact.cell_counts(), balanced.cell_counts());
    assert!(compact.is_tick_exact());

    // Core oracle: two sequential Fredkins = identity (self-inverse).
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::seq(fk(), fk());
    let (in_ty, out_ty) = type_of(&iso).unwrap();
    for v in enumerate_values(&in_ty) {
        let bits = bits_of(&v, &in_ty).unwrap();
        let compact_out = compact.simulate(&bits).unwrap().outputs;
        let balanced_out = simulate(&balanced, &bits).unwrap().outputs;
        assert_eq!(compact_out, balanced_out, "outputs disagree on {v:?}");
        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(compact_out, bits_of(&expected, &out_ty).unwrap());
    }
}

// ── (6) Tick-exactness spot-check ──────────────────────────────────────

#[test]
fn compact_delays_equalize_arrivals() {
    // For each cell, verify that (arrival + delay) is the same for all
    // input ports -- i.e. the compact delays make every cell tick-exact.
    let (typed_net, _) = reconvergent_with_skew(3);
    let original = lower(&typed_net).unwrap();
    let compact = balance_compact(&original).unwrap();
    let arrivals = original.line_arrivals().unwrap();

    for (ci, cell) in compact.net().cells.iter().enumerate() {
        let balanced_arrivals: Vec<u32> = cell
            .inputs
            .iter()
            .enumerate()
            .map(|(pi, p)| arrivals[p.line1.0 as usize] + compact.delay(ci, pi))
            .collect();
        // All balanced arrivals must be the same (= cell.depth).
        let first = balanced_arrivals[0];
        for &a in &balanced_arrivals {
            assert_eq!(a, first, "cell {ci} not tick-exact: {balanced_arrivals:?}");
        }
        assert_eq!(first, cell.depth, "cell {ci} balanced arrival != depth");
    }
    assert!(compact.is_tick_exact());
}

// ── (7) MEMORY WIN: large reconvergent fan-in ──────────────────────────

/// Build a synthetic netlist with a wide reconvergent fan-in that forces
/// a large number of K0 buffers, WITHOUT calling the materializing
/// `balance()` on it.
///
/// Structure: `depth` chained K0 buffers on one bit (line A), then a
/// K1 Fredkin with inputs [A_out, B, C] where B and C are primary
/// inputs (arrival 0). The Fredkin sees arrivals [depth, 0, 0],
/// so balance would insert `depth` K0 buffers on each of ports 1 and 2
/// = 2 × depth total K0 inserts.
///
/// To scale this up, we repeat the pattern `width` times in parallel:
/// `width` independent (Reg(depth) × id × id ; fredkin) blocks.
/// Total K0 inserts = width × 2 × depth.
fn build_large_reconvergent(depth: u32, width: usize) -> PhasedCellNetlist {
    // Each block uses 3 input pairs + `depth` K0 cells + 1 K1 cell,
    // with intermediate lines. Total lines per block:
    //   3 input pairs (6 lines) + depth K0 outputs (2 each) + 1 K1 output (6 lines)
    //   = 6 + 2*depth + 6 = 12 + 2*depth
    let lines_per_block = 12 + 2 * depth;

    let mut inputs = Vec::new();
    let mut outputs = Vec::new();
    let mut output_depths = Vec::new();
    let mut cells = Vec::new();
    let num_lines = lines_per_block * (width as u32);

    for w in 0..width {
        let base = (w as u32) * lines_per_block;
        // 3 input pairs: control (line base+0,1), a (base+2,3), b (base+4,5)
        let ctrl_in = RailPair {
            line0: LineId(base),
            line1: LineId(base + 1),
        };
        let a_in = RailPair {
            line0: LineId(base + 2),
            line1: LineId(base + 3),
        };
        let b_in = RailPair {
            line0: LineId(base + 4),
            line1: LineId(base + 5),
        };
        inputs.push(ctrl_in);
        inputs.push(a_in);
        inputs.push(b_in);

        // Chain of `depth` K0 buffers on the control line.
        let mut cur_pair = ctrl_in;
        for d in 0..depth {
            let out_base = base + 6 + d * 2;
            let out_pair = RailPair {
                line0: LineId(out_base),
                line1: LineId(out_base + 1),
            };
            cells.push(Cell {
                kind: CellKind::K0,
                depth: d,
                phase: (d % u32::from(PHASE_COUNT)) as u8,
                inputs: vec![cur_pair],
                outputs: vec![out_pair],
            });
            cur_pair = out_pair;
        }

        // K1 Fredkin: inputs [ctrl_delayed, a, b], all must arrive at `depth`.
        // ctrl_delayed arrives at `depth`, a and b at 0.
        // Balanced: would insert `depth` K0 on each of a, b.
        let k1_out_base = base + 6 + depth * 2;
        let k1_outs = [
            RailPair {
                line0: LineId(k1_out_base),
                line1: LineId(k1_out_base + 1),
            },
            RailPair {
                line0: LineId(k1_out_base + 2),
                line1: LineId(k1_out_base + 3),
            },
            RailPair {
                line0: LineId(k1_out_base + 4),
                line1: LineId(k1_out_base + 5),
            },
        ];
        cells.push(Cell {
            kind: CellKind::K1,
            depth,
            phase: (depth % u32::from(PHASE_COUNT)) as u8,
            inputs: vec![cur_pair, a_in, b_in],
            outputs: k1_outs.to_vec(),
        });

        for out in &k1_outs {
            outputs.push(*out);
            output_depths.push(depth + 1);
        }
    }

    PhasedCellNetlist {
        input_ty: three_bits(), // approximate; not used by balance_compact
        output_ty: three_bits(),
        inputs,
        outputs,
        output_depths,
        cells,
        num_lines,
    }
}

#[test]
fn large_reconvergent_compact_correct_count() {
    // 500 depth × 100 width = 100,000 inserted K0 buffers.
    // The materializing balance() would create 100k Cell structs;
    // balance_compact stores only the per-input delays.
    let depth = 500u32;
    let width = 100usize;
    let net = build_large_reconvergent(depth, width);

    // Validate the hand-built netlist.
    assert_eq!(net.validate(), Ok(()));

    let compact = balance_compact(&net).unwrap();

    // Expected inserted K0: each of the `width` blocks inserts
    // 2 × depth K0 buffers (one per data input of the Fredkin).
    let expected_k0 = width * 2 * (depth as usize);
    assert_eq!(compact.k0_buffer_count(), expected_k0);

    // Cell counts: original K0 = width * depth, original K1 = width.
    let mut expected_counts = BTreeMap::new();
    expected_counts.insert(CellKind::K0, width * (depth as usize) + expected_k0);
    expected_counts.insert(CellKind::K1, width);
    assert_eq!(compact.cell_counts(), expected_counts);

    assert!(compact.is_tick_exact());
}

#[test]
fn large_reconvergent_compact_simulates_correctly() {
    // Smaller but still large enough to prove the point: 50 depth × 10
    // width = 1000 inserted K0 buffers.
    let depth = 50u32;
    let width = 10usize;
    let net = build_large_reconvergent(depth, width);
    assert_eq!(net.validate(), Ok(()));

    let compact = balance_compact(&net).unwrap();
    assert_eq!(compact.k0_buffer_count(), width * 2 * (depth as usize));

    // Simulate: each block is an independent Fredkin.
    // control=false: (c,a,b) → (c,a,b), control=true: (c,a,b) → (c,b,a).
    // Test a few input patterns across all blocks.
    let n_inputs = width * 3;
    let mut inputs_all_false = vec![false; n_inputs];
    let result = compact.simulate(&inputs_all_false).unwrap();
    assert_eq!(result.outputs, vec![false; width * 3]);

    // Set all controls to true, a=true, b=false → outputs c=true, b=false, a=true
    for w in 0..width {
        inputs_all_false[w * 3] = true; // control
        inputs_all_false[w * 3 + 1] = true; // a
        inputs_all_false[w * 3 + 2] = false; // b
    }
    let result = compact.simulate(&inputs_all_false).unwrap();
    for w in 0..width {
        assert!(result.outputs[w * 3], "control passes through");
        assert!(!result.outputs[w * 3 + 1], "a,b swapped: b");
        assert!(result.outputs[w * 3 + 2], "a,b swapped: a");
    }
}

// ── (8) Wrong input width is refused ───────────────────────────────────

#[test]
fn compact_wrong_input_width() {
    let iso = Iso::Prim(Box::new(fredkin_prim()));
    let net = lower_iso(&iso).unwrap();
    let compact = balance_compact(&net).unwrap();
    assert!(compact.simulate(&[]).is_err());
    assert!(compact.simulate(&[true]).is_err());
}

// ── (9) Synthesized adder: compact matches materialized ────────────────

#[test]
fn compact_synthesized_adder() {
    let four = Type::prod(Type::prod(bit(), bit()), Type::prod(bit(), bit()));
    let adder = prim_from_bits("add2", four.clone(), four.clone(), |bits| {
        let a = (u8::from(bits[0]) << 1) | u8::from(bits[1]);
        let b = (u8::from(bits[2]) << 1) | u8::from(bits[3]);
        let s = (a + b) & 0b11;
        vec![bits[0], bits[1], s & 0b10 != 0, s & 0b01 != 0]
    })
    .unwrap();
    let iso = Iso::Prim(Box::new(adder));
    let (net, ancilla) =
        theseus_netlist::lower::lower_iso_with_ancillas(&iso).expect("synthesis lowers");
    let balanced = balance(&net).expect("balances");
    let compact = balance_compact(&net).expect("compact balances");

    // K0 counts match.
    let materialized_k0_delta = balanced
        .cells
        .iter()
        .filter(|c| c.kind == CellKind::K0)
        .count()
        - net.cells.iter().filter(|c| c.kind == CellKind::K0).count();
    assert_eq!(compact.k0_buffer_count(), materialized_k0_delta);
    assert_eq!(compact.cell_counts(), balanced.cell_counts());
    assert!(compact.is_tick_exact());

    // Simulation outputs match on full domain.
    let n_data_out = theseus_netlist::rail_width(&four).unwrap();
    for v in enumerate_values(&four) {
        let mut bits = bits_of(&v, &four).unwrap();
        bits.extend_from_slice(&ancilla);
        let compact_out = compact.simulate(&bits).unwrap().outputs;
        let balanced_out = simulate(&balanced, &bits).unwrap().outputs;
        assert_eq!(
            &compact_out[..n_data_out],
            &balanced_out[..n_data_out],
            "compact sim disagrees with balanced sim on {v:?}"
        );
    }
}
