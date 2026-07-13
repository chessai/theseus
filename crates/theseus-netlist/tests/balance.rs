//! Tests for the tick-exact balancer (ADR-0026 C2.2).
//!
//! Pure logic tests — no ngspice, no SPICE. Validates that `balance()`
//! produces netlists that are:
//! - balanced (no tick imbalances),
//! - structurally valid (validate() Ok),
//! - semantics-preserving (sim agrees with core eval on all inputs),
//! - minimal (exact K0 count = sum of deficits),
//! - idempotent (balance(balance(net)) == balance(net)).

use std::collections::BTreeMap;

use theseus_core::{enumerate_values, type_of, Iso, Type};
use theseus_netlist::ir::typed::{Monoid, NodeKind, TypedNode};
use theseus_netlist::{
    balance, bits_of, lower, lower_iso, prim_from_bits, simulate, CellKind, PhasedCellNetlist,
};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn three_bits() -> Type {
    Type::prod(bit(), Type::prod(bit(), bit()))
}

fn fredkin_prim() -> theseus_core::PrimDef {
    prim_from_bits("fredkin", three_bits(), three_bits(), |b| CellKind::K1.eval(b)).unwrap()
}

fn id_bit() -> TypedNode {
    TypedNode {
        input: bit(),
        output: bit(),
        kind: NodeKind::Permute { monoid: Monoid::Prod, sigma: vec![0] },
    }
}

/// The differential oracle: for every input value, the timed sim of the
/// balanced netlist must agree with the core interpreter.
fn assert_balanced_agrees_with_core(iso: &Iso, balanced: &PhasedCellNetlist) {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    for v in enumerate_values(&in_ty) {
        let bits = bits_of(&v, &in_ty).unwrap();
        let res = simulate(balanced, &bits).expect("timed sim on balanced netlist");
        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        let expected_bits = bits_of(&expected, &out_ty).unwrap();
        assert_eq!(
            res.outputs, expected_bits,
            "balanced sim disagrees with core eval on {v:?}"
        );
    }
}

// ── (a) Reconvergent fixture: (Reg(1) × id × id) ; fredkin ─────────

/// Build the unbalanced netlist: `(Reg(1) × id × id) ; fredkin`.
/// The control bit passes through Reg(1) (arrives at tick 1) while the
/// data bits pass through (arrive at tick 0). The Fredkin cell sees
/// arrivals [1, 0, 0] — a tick imbalance the balancer must repair.
fn reconvergent_fixture() -> (TypedNode, Iso) {
    let reg1 = TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(1) };
    let par = TypedNode {
        input: three_bits(),
        output: three_bits(),
        kind: NodeKind::Par(
            Box::new(reg1),
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
    // Reg is logically the identity — the Iso oracle for the core
    // interpreter is just the Fredkin itself.
    let iso = Iso::Prim(Box::new(fredkin_prim()));
    (net, iso)
}

#[test]
fn reconvergent_balance_removes_imbalance() {
    let (typed_net, _) = reconvergent_fixture();
    let original = lower(&typed_net).unwrap();
    assert_eq!(original.validate(), Ok(()));

    // Original has tick imbalances.
    let imbalances = original.tick_imbalances().unwrap();
    assert!(!imbalances.is_empty(), "original must have tick imbalances");
    assert_eq!(imbalances.len(), 1);
    assert_eq!(imbalances[0].arrivals, vec![1, 0, 0]);

    // Balance.
    let balanced = balance(&original).unwrap();

    // Balanced has no imbalances.
    assert_eq!(
        balanced.tick_imbalances().unwrap(),
        vec![],
        "balanced netlist must have no tick imbalances"
    );
}

#[test]
fn reconvergent_balance_validates() {
    let (typed_net, _) = reconvergent_fixture();
    let original = lower(&typed_net).unwrap();
    let balanced = balance(&original).unwrap();
    assert_eq!(balanced.validate(), Ok(()), "balanced netlist must validate");
}

#[test]
fn reconvergent_balance_exact_k0_count() {
    let (typed_net, _) = reconvergent_fixture();
    let original = lower(&typed_net).unwrap();

    // Original: 1 K0 (from Reg(1)) + 1 K1 = 2 cells.
    assert_eq!(
        original.cell_counts(),
        BTreeMap::from([(CellKind::K0, 1), (CellKind::K1, 1)])
    );

    let balanced = balance(&original).unwrap();

    // The Fredkin's inputs arrive at [1, 0, 0]. Deficit on port 1 = 1,
    // deficit on port 2 = 1. Two K0 buffers inserted.
    // Total K0: 1 (original Reg) + 2 (inserted) = 3.
    assert_eq!(
        balanced.cell_counts(),
        BTreeMap::from([(CellKind::K0, 3), (CellKind::K1, 1)])
    );

    // Exact inserted count = 2.
    let original_k0 = original.cells.iter().filter(|c| c.kind == CellKind::K0).count();
    let balanced_k0 = balanced.cells.iter().filter(|c| c.kind == CellKind::K0).count();
    assert_eq!(balanced_k0 - original_k0, 2, "exactly 2 K0 buffers inserted");
}

#[test]
fn reconvergent_balance_preserves_semantics() {
    let (typed_net, iso) = reconvergent_fixture();
    let original = lower(&typed_net).unwrap();
    let balanced = balance(&original).unwrap();
    assert_balanced_agrees_with_core(&iso, &balanced);
}

// ── (b) Already-balanced netlist: single Fredkin ────────────────────

#[test]
fn already_balanced_fredkin_inserts_zero_buffers() {
    let iso = Iso::Prim(Box::new(fredkin_prim()));
    let original = lower_iso(&iso).unwrap();

    // Single Fredkin: all inputs at tick 0, already balanced.
    assert!(original.tick_imbalances().unwrap().is_empty());

    let balanced = balance(&original).unwrap();

    // Zero buffers inserted: same cell counts.
    assert_eq!(balanced.cell_counts(), original.cell_counts());
    assert_eq!(balanced.cells.len(), original.cells.len());

    // Still valid and balanced.
    assert_eq!(balanced.validate(), Ok(()));
    assert!(balanced.tick_imbalances().unwrap().is_empty());
}

#[test]
fn already_balanced_is_idempotent() {
    let iso = Iso::Prim(Box::new(fredkin_prim()));
    let original = lower_iso(&iso).unwrap();
    let balanced_once = balance(&original).unwrap();
    let balanced_twice = balance(&balanced_once).unwrap();

    // Same cell counts, same structure.
    assert_eq!(balanced_twice.cell_counts(), balanced_once.cell_counts());
    assert_eq!(balanced_twice.cells.len(), balanced_once.cells.len());
    assert_eq!(balanced_twice.output_depths, balanced_once.output_depths);
}

#[test]
fn reconvergent_balance_is_idempotent() {
    let (typed_net, _) = reconvergent_fixture();
    let original = lower(&typed_net).unwrap();
    let balanced_once = balance(&original).unwrap();
    let balanced_twice = balance(&balanced_once).unwrap();

    // Idempotent: second pass adds zero buffers.
    assert_eq!(balanced_twice.cell_counts(), balanced_once.cell_counts());
    assert_eq!(balanced_twice.cells.len(), balanced_once.cells.len());
    assert_eq!(balanced_twice.output_depths, balanced_once.output_depths);
}

// ── (c) Multi-cell program: chain of Fredkins with Reg skew ─────────

/// A multi-cell program: `(Reg(2) × id × id) ; fredkin ; fredkin`.
/// The first Fredkin has arrivals [2, 0, 0] (larger deficit), and
/// its output feeds the second Fredkin (already balanced since all
/// its inputs come from the first Fredkin's outputs at tick 3).
fn multi_cell_fixture() -> (TypedNode, Iso) {
    let reg2 = TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(2) };
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
    // Reg is identity; two sequential Fredkins = Fredkin ; Fredkin
    // = identity (self-inverse). The Iso oracle is just id on 3 bits.
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::seq(fk(), fk());
    (net, iso)
}

#[test]
fn multi_cell_balance_removes_all_imbalances() {
    let (typed_net, _) = multi_cell_fixture();
    let original = lower(&typed_net).unwrap();
    assert_eq!(original.validate(), Ok(()));
    assert!(!original.tick_imbalances().unwrap().is_empty());

    let balanced = balance(&original).unwrap();
    assert_eq!(balanced.validate(), Ok(()));
    assert!(balanced.tick_imbalances().unwrap().is_empty());
}

#[test]
fn multi_cell_balance_exact_k0_count() {
    let (typed_net, _) = multi_cell_fixture();
    let original = lower(&typed_net).unwrap();

    // Original: 2 K0 (from Reg(2)) + 2 K1 = 4 cells.
    assert_eq!(
        original.cell_counts(),
        BTreeMap::from([(CellKind::K0, 2), (CellKind::K1, 2)])
    );

    let balanced = balance(&original).unwrap();

    // First Fredkin: arrivals [2, 0, 0], deficit on port 1 = 2, port 2 = 2.
    // 4 K0 buffers inserted for first Fredkin.
    // Second Fredkin: all inputs from first Fredkin's outputs (tick 3),
    // no deficit. 0 K0 buffers inserted.
    // Total K0: 2 (original) + 4 (inserted) = 6.
    assert_eq!(
        balanced.cell_counts(),
        BTreeMap::from([(CellKind::K0, 6), (CellKind::K1, 2)])
    );
}

#[test]
fn multi_cell_balance_preserves_semantics_full_domain() {
    let (typed_net, iso) = multi_cell_fixture();
    let original = lower(&typed_net).unwrap();
    let balanced = balance(&original).unwrap();
    assert_balanced_agrees_with_core(&iso, &balanced);
}

#[test]
fn multi_cell_balance_is_idempotent() {
    let (typed_net, _) = multi_cell_fixture();
    let original = lower(&typed_net).unwrap();
    let balanced_once = balance(&original).unwrap();
    let balanced_twice = balance(&balanced_once).unwrap();
    assert_eq!(balanced_twice.cell_counts(), balanced_once.cell_counts());
    assert_eq!(balanced_twice.cells.len(), balanced_once.cells.len());
}

// ── Wiring-only and edge cases ──────────────────────────────────────

#[test]
fn wiring_only_netlist_is_unchanged() {
    // Identity on bit: pure wiring, no cells, nothing to balance.
    let iso = Iso::Id(bit());
    let original = lower_iso(&iso).unwrap();
    assert!(original.cells.is_empty());

    let balanced = balance(&original).unwrap();
    assert!(balanced.cells.is_empty());
    assert_eq!(balanced.validate(), Ok(()));
    assert_eq!(balanced.output_depths, original.output_depths);
}

#[test]
fn reg_chain_no_reconvergence_no_buffers() {
    // Reg(3) on one bit: 3 K0 cells in series, no reconvergence,
    // no imbalance (each K0 has exactly one input).
    let reg3 = TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(3) };
    let original = lower(&reg3).unwrap();
    assert!(original.tick_imbalances().unwrap().is_empty());

    let balanced = balance(&original).unwrap();
    assert_eq!(balanced.cell_counts(), original.cell_counts());
    assert_eq!(balanced.validate(), Ok(()));
}

#[test]
fn balance_preserves_output_skew() {
    // Reg(1) × id: outputs arrive at [1, 0]. The balancer does NOT
    // fix output skew (that's a different pass). It only balances
    // cell inputs.
    let reg1 = TypedNode { input: bit(), output: bit(), kind: NodeKind::Reg(1) };
    let net = lower(&TypedNode {
        input: Type::prod(bit(), bit()),
        output: Type::prod(bit(), bit()),
        kind: NodeKind::Par(Box::new(reg1), Box::new(id_bit())),
    })
    .unwrap();
    assert_eq!(net.output_depths, vec![1, 0]);

    let balanced = balance(&net).unwrap();
    // Output skew preserved (no cells with multiple inputs to balance).
    assert_eq!(balanced.output_depths, vec![1, 0]);
    assert_eq!(balanced.validate(), Ok(()));
}

// ── Chain of five Fredkins — semantics oracle ───────────────────────

#[test]
fn chain_of_fredkins_semantics_preserved() {
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::seq(fk(), Iso::seq(fk(), Iso::seq(fk(), Iso::seq(fk(), fk()))));
    let original = lower_iso(&iso).unwrap();
    // All inputs from previous Fredkin at same tick — already balanced.
    assert!(original.tick_imbalances().unwrap().is_empty());

    let balanced = balance(&original).unwrap();
    assert_eq!(balanced.validate(), Ok(()));
    assert_eq!(balanced.cell_counts(), original.cell_counts());
    assert_balanced_agrees_with_core(&iso, &balanced);
}

// ── B4.2 general synthesis through the timed sim ────────────────────

#[test]
fn synthesized_monolithic_adder_balances_and_sims() {
    // The monolithic preserved-input 2-bit adder lowers via general
    // CSWAP synthesis (it matches no fixed template); after balancing,
    // the TIMED phased sim — which asserts dynamic INV-TOKEN for every
    // rail pair on every tick — agrees with core eval on all 16 inputs.
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
    assert!(balanced.tick_imbalances().unwrap().is_empty());
    let n_data_out = theseus_netlist::rail_width(&four).unwrap();
    for v in enumerate_values(&four) {
        let mut bits = bits_of(&v, &four).unwrap();
        bits.extend_from_slice(&ancilla);
        let res = simulate(&balanced, &bits).expect("timed sim on balanced synthesized net");
        assert!(res.inv_token_checks > 0);
        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(
            &res.outputs[..n_data_out],
            &bits_of(&expected, &four).unwrap()[..],
            "timed sim disagrees with core eval on {v:?}"
        );
    }
}
