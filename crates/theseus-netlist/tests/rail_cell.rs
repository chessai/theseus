//! Tests for `CellKind::R1` -- the first-class rail-role-swap cell
//! (ADR-0040 follow-up slice step 1).
//!
//! Verification norm: the differential oracle vs `theseus_eval` /
//! `CellKind::eval`; this is theseus, NOT maestro's rules.

use theseus_netlist::{
    Cell, CellKind, LineId, PhasedCellNetlist, RailPair, RoleSwapKind,
};

use theseus_core::Type;

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn two_bits() -> Type {
    Type::prod(bit(), bit())
}

fn bits_of_index(x: u32, n: usize) -> Vec<bool> {
    (0..n).map(|i| (x >> i) & 1 == 1).collect()
}

// ── CellKind::R1.eval == RoleSwapKind::R1.eval over all inputs ──────

#[test]
fn cellkind_r1_eval_matches_roleswapkind_r1_eval() {
    for x in 0..4u32 {
        let bits = bits_of_index(x, 2);
        assert_eq!(
            CellKind::R1.eval(&bits),
            RoleSwapKind::R1.eval(&bits),
            "CellKind::R1 disagrees with RoleSwapKind::R1 on {bits:?}"
        );
    }
}

#[test]
fn cellkind_r1_is_cnot() {
    for x in 0..4u32 {
        let bits = bits_of_index(x, 2);
        let (c, t) = (bits[0], bits[1]);
        assert_eq!(CellKind::R1.eval(&bits), vec![c, t ^ c]);
    }
}

#[test]
fn cellkind_r1_self_inverse() {
    for x in 0..4u32 {
        let bits = bits_of_index(x, 2);
        assert_eq!(CellKind::R1.eval(&CellKind::R1.eval(&bits)), bits);
    }
}

#[test]
fn cellkind_r1_arity_is_two() {
    assert_eq!(CellKind::R1.arity(), 2);
}

#[test]
fn cellkind_r1_char_db_key() {
    assert_eq!(CellKind::R1.char_db_key(), "cswap-r1");
}

// ── Single-R1 cell in a PhasedCellNetlist ────────────────────────────

/// A hand-built single-R1 netlist on two bits: (c, t) → (c, t ⊕ c).
fn one_r1() -> PhasedCellNetlist {
    let c_in = RailPair { line0: LineId(0), line1: LineId(1) };
    let t_in = RailPair { line0: LineId(2), line1: LineId(3) };
    let c_out = RailPair { line0: LineId(4), line1: LineId(5) };
    let t_out = RailPair { line0: LineId(6), line1: LineId(7) };
    PhasedCellNetlist {
        input_ty: two_bits(),
        output_ty: two_bits(),
        inputs: vec![c_in, t_in],
        outputs: vec![c_out, t_out],
        output_depths: vec![1, 1],
        cells: vec![Cell {
            kind: CellKind::R1,
            depth: 0,
            phase: 0,
            inputs: vec![c_in, t_in],
            outputs: vec![c_out, t_out],
        }],
        num_lines: 8,
    }
}

#[test]
fn r1_netlist_validates() {
    let net = one_r1();
    assert_eq!(net.validate(), Ok(()));
    assert_eq!(net.depth(), 1);
    assert_eq!(
        net.line_arrivals().unwrap(),
        vec![0, 0, 0, 0, 1, 1, 1, 1]
    );
    assert!(net.tick_imbalances().unwrap().is_empty());
}

#[test]
fn r1_netlist_simulates_as_cnot() {
    use theseus_netlist::sim::simulate;

    let net = one_r1();
    // Differential oracle: (c, t) → (c, t ⊕ c).
    for x in 0..4u32 {
        let bits = bits_of_index(x, 2);
        let (c, t) = (bits[0], bits[1]);
        let res = simulate(&net, &bits).expect("sim should succeed");
        assert_eq!(res.outputs, vec![c, t ^ c], "CNOT oracle on {bits:?}");
        assert_eq!(res.ticks, 1);
    }
}

#[test]
fn r1_balance_accepts() {
    use theseus_netlist::balance::balance;

    let net = one_r1();
    let balanced = balance(&net).expect("balance should succeed");
    // Single-cell netlist is already balanced -- zero K0 insertions.
    assert_eq!(balanced.validate(), Ok(()));
    for x in 0..4u32 {
        let bits = bits_of_index(x, 2);
        let (c, t) = (bits[0], bits[1]);
        let res = theseus_netlist::sim::simulate(&balanced, &bits)
            .expect("sim on balanced net should succeed");
        assert_eq!(res.outputs, vec![c, t ^ c]);
    }
}

// ── Char DB key resolves in the committed DB ─────────────────────────

#[test]
fn r1_char_db_key_resolves_in_rung0_db() {
    let db = theseus_spice::chardb::rung0_measured();
    let e = db
        .query(CellKind::R1.char_db_key(), 1e-7)
        .expect("cswap-r1 must resolve in the rung0 char DB");
    assert!(e.is_finite() && e > 0.0, "energy must be positive, got {e}");
}
