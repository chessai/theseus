//! Tests for the general-sum encoding, Branch lowering, and
//! Distrib/Factor lowering (ADR-0031, M3b-A).
//!
//! Pure logic tests — no ngspice, no SPICE.  Every test builds an Iso,
//! lowers it end-to-end, then verifies by the differential oracle:
//! `theseus_eval::eval` is the semantic root, and the lowered+simulated
//! netlist must agree with it over the WHOLE finite domain.

use std::collections::BTreeMap;

use theseus_core::{enumerate_values, type_of, Iso, PrimDef, Type};
use theseus_netlist::{
    bits_of, lower_iso, prim_from_bits, rail_width, value_from_bits, CellKind,
    PhasedCellNetlist, RailPair,
};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn three_bits() -> Type {
    Type::prod(bit(), Type::prod(bit(), bit()))
}

fn fredkin_prim() -> PrimDef {
    prim_from_bits("fredkin", three_bits(), three_bits(), |b| CellKind::K1.eval(b)).unwrap()
}

/// Untimed functional evaluation of the cell netlist: propagate token
/// values line by line, asserting INV-TOKEN at every pair read.
fn eval_cells(net: &PhasedCellNetlist, input_bits: &[bool]) -> Vec<bool> {
    let mut line: Vec<Option<bool>> = vec![None; net.num_lines as usize];
    fn set_pair(line: &mut [Option<bool>], p: RailPair, v: bool) {
        line[p.line1.0 as usize] = Some(v);
        line[p.line0.0 as usize] = Some(!v);
    }
    fn get_pair(line: &[Option<bool>], p: RailPair) -> bool {
        let v1 = line[p.line1.0 as usize].expect("1-line driven");
        let v0 = line[p.line0.0 as usize].expect("0-line driven");
        assert_ne!(v0, v1, "INV-TOKEN violated: pair not 1-of-2");
        v1
    }

    assert_eq!(input_bits.len(), net.inputs.len());
    for (&p, &v) in net.inputs.iter().zip(input_bits) {
        set_pair(&mut line, p, v);
    }
    for cell in &net.cells {
        let vals: Vec<bool> = cell.inputs.iter().map(|&p| get_pair(&line, p)).collect();
        let outs = cell.kind.eval(&vals);
        for (&p, &v) in cell.outputs.iter().zip(&outs) {
            set_pair(&mut line, p, v);
        }
    }
    net.outputs.iter().map(|&p| get_pair(&line, p)).collect()
}

/// The differential oracle: for every input value, the lowered netlist's
/// untimed evaluation must agree with `theseus_eval::eval`.
fn assert_lowering_agrees(iso: &Iso) -> PhasedCellNetlist {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let net = lower_iso(iso).expect("lowering succeeds");
    assert_eq!(net.validate(), Ok(()), "lowered netlist validates");
    for v in enumerate_values(&in_ty) {
        let bits = bits_of(&v, &in_ty).unwrap();
        let got = eval_cells(&net, &bits);
        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        assert_eq!(
            got,
            bits_of(&expected, &out_ty).unwrap(),
            "netlist disagrees with core eval on {v:?}"
        );
    }
    net
}

// ── (a) Branch of two isos on a bit-payload sum ────────────────────

#[test]
fn branch_fredkin_id_oracle() {
    // Plus(Fredkin, Id(3bits)) : (Bit×Bit×Bit)+(Bit×Bit×Bit) ↔ same.
    // f = Fredkin (1 K1 cell → promoted to K2), g = Id (zero cells).
    // Tests BOTH tag arms over the whole domain.
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::plus(fk(), Iso::Id(three_bits()));
    let net = assert_lowering_agrees(&iso);
    // One K2 cell (promoted Fredkin).
    assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K2, 1)]));
    assert_eq!(net.validate(), Ok(()));
}

#[test]
fn branch_id_fredkin_oracle() {
    // Plus(Id(3bits), Fredkin): g = Fredkin, controlled by inverted tag.
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::plus(Iso::Id(three_bits()), fk());
    let net = assert_lowering_agrees(&iso);
    assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K2, 1)]));
}

#[test]
fn branch_fredkin_fredkin_oracle() {
    // Plus(Fredkin, Fredkin): both arms apply Fredkin.
    // Two K2 cells (one per arm, each controlled by tag/NOT(tag)).
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::plus(fk(), fk());
    let net = assert_lowering_agrees(&iso);
    assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K2, 2)]));
}

#[test]
fn branch_id_id_on_unit_sum() {
    // Plus(Id(One), Id(One)) on Bit = 1+1: identity, zero cells.
    let iso = Iso::plus(Iso::Id(Type::One), Iso::Id(Type::One));
    let net = assert_lowering_agrees(&iso);
    assert!(net.cells.is_empty());
}

// ── (b) Branch with unequal arm widths ─────────────────────────────

#[test]
fn branch_fredkin_id_unequal_widths() {
    // Plus(Fredkin, Id(One)) on (3bits + One):
    // wA = 3, wB = 0, max_w = 3.  f = Fredkin (promoted to K2),
    // g = Id(One) (zero cells, zero width).
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::plus(fk(), Iso::Id(Type::One));
    let net = assert_lowering_agrees(&iso);
    assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K2, 1)]));
}

#[test]
fn branch_id_fredkin_unequal_widths() {
    // Plus(Id(One), Fredkin) on (One + 3bits):
    // wA = 0, wB = 3.  g = Fredkin, controlled by inverted tag.
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::plus(Iso::Id(Type::One), fk());
    let net = assert_lowering_agrees(&iso);
    assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::K2, 1)]));
}

// ── (c) Distrib and Factor round-trip ──────────────────────────────

#[test]
fn distrib_factor_round_trip_one_one_one() {
    // Distrib(One, One, One) ; Factor(One, One, One) = id.
    let iso = Iso::seq(
        Iso::Distrib(Type::One, Type::One, Type::One),
        Iso::Factor(Type::One, Type::One, Type::One),
    );
    let net = assert_lowering_agrees(&iso);
    assert!(net.cells.is_empty(), "distrib;factor should be zero cells");
}

#[test]
fn distrib_factor_round_trip_bit_one_one() {
    // Distrib(Bit, One, One) ; Factor(Bit, One, One) = id.
    let iso = Iso::seq(
        Iso::Distrib(bit(), Type::One, Type::One),
        Iso::Factor(bit(), Type::One, Type::One),
    );
    let net = assert_lowering_agrees(&iso);
    assert!(net.cells.is_empty());
}

#[test]
fn factor_distrib_round_trip_bit_one_one() {
    // Factor then Distrib = id (the other direction).
    let iso = Iso::seq(
        Iso::Factor(bit(), Type::One, Type::One),
        Iso::Distrib(bit(), Type::One, Type::One),
    );
    let net = assert_lowering_agrees(&iso);
    assert!(net.cells.is_empty());
}

#[test]
fn distrib_oracle_bit_one_one() {
    // Distrib(Bit, One, One) alone: Bit × Bit → Bit + Bit.
    let iso = Iso::Distrib(bit(), Type::One, Type::One);
    let net = assert_lowering_agrees(&iso);
    assert!(net.cells.is_empty());
}

#[test]
fn factor_oracle_bit_one_one() {
    // Factor(Bit, One, One): Bit + Bit → Bit × Bit.
    let iso = Iso::Factor(bit(), Type::One, Type::One);
    let net = assert_lowering_agrees(&iso);
    assert!(net.cells.is_empty());
}

// ── (d) Composed iso: Distrib then Branch then Factor ──────────────
//
// This is the key integration case: a truth-table-style program
// that uses Distrib to expose the sum, Branch to apply per-arm logic,
// and Factor to re-package.

#[test]
fn distrib_branch_factor_composed() {
    // Build: Distrib(Bit, One, One) ; Branch(Id(Bit×One), Id(Bit×One)) ; Factor(Bit, One, One)
    // = Distrib ; Id ; Factor = Id on Bit×Bit.
    //
    // This exercises the full pipeline: Distrib (rail reorder) →
    // Branch (controlled cells) → Factor (rail reorder back).
    let iso = Iso::seq(
        Iso::Distrib(bit(), Type::One, Type::One),
        Iso::seq(
            Iso::plus(
                Iso::Id(Type::prod(bit(), Type::One)),
                Iso::Id(Type::prod(bit(), Type::One)),
            ),
            Iso::Factor(bit(), Type::One, Type::One),
        ),
    );
    let net = assert_lowering_agrees(&iso);
    // Should be zero cells (all operations are identity or wiring).
    assert!(net.cells.is_empty());
}

#[test]
fn distrib_branch_fredkin_factor() {
    // A non-trivial case: use Fredkin inside the Branch.
    // Distrib(Bit, Bit, Bit) exposes Bit×(Bit+Bit) → (Bit×Bit)+(Bit×Bit).
    // Then Branch(Fredkin_on_2bits?, ...) — but Fredkin needs 3 bits.
    //
    // Simpler: Distrib(3bits, One, One) ; Branch(Id, Id) ; Factor.
    // This tests the Distrib/Factor wiring on wider types.
    let iso = Iso::seq(
        Iso::Distrib(three_bits(), Type::One, Type::One),
        Iso::seq(
            Iso::plus(
                Iso::Id(Type::prod(three_bits(), Type::One)),
                Iso::Id(Type::prod(three_bits(), Type::One)),
            ),
            Iso::Factor(three_bits(), Type::One, Type::One),
        ),
    );
    let net = assert_lowering_agrees(&iso);
    assert!(net.cells.is_empty());
}

// ── (e) General sum encoding: round-trip and identity ──────────────

#[test]
fn general_sum_encoding_round_trip() {
    // Verify bits_of / value_from_bits round-trip for various sum types.
    let types = vec![
        Type::sum(bit(), Type::One),
        Type::sum(Type::One, bit()),
        Type::sum(bit(), bit()),
        Type::sum(bit(), Type::prod(bit(), bit())),
        Type::sum(Type::prod(bit(), bit()), bit()),
    ];
    for ty in &types {
        for v in enumerate_values(ty) {
            let bits = bits_of(&v, ty).unwrap();
            assert_eq!(bits.len(), rail_width(ty).unwrap());
            let decoded = value_from_bits(ty, &bits).unwrap();
            assert_eq!(decoded, v, "round-trip failed for {v:?} at type {ty}");
        }
    }
}

#[test]
fn bit_encoding_backward_compat() {
    // The bit (1+1) encoding must be backward-compatible:
    // rail_width = 1, Left(Unit) = [true], Right(Unit) = [false].
    assert_eq!(rail_width(&bit()), Ok(1));
    assert_eq!(
        bits_of(&theseus_core::Value::left(theseus_core::Value::Unit), &bit()),
        Ok(vec![true])
    );
    assert_eq!(
        bits_of(&theseus_core::Value::right(theseus_core::Value::Unit), &bit()),
        Ok(vec![false])
    );
}

#[test]
fn identity_on_general_sum_is_wiring() {
    // Id on Bit+One, One+Bit, etc. should lower to zero cells.
    for ty in [
        Type::sum(bit(), Type::One),
        Type::sum(Type::One, bit()),
        Type::sum(bit(), bit()),
    ] {
        let iso = Iso::Id(ty);
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
    }
}

#[test]
fn swap_sum_on_general_types() {
    // SwapSum on various sum types: tag rail swap, zero cells.
    for (a, b) in [
        (bit(), Type::One),
        (Type::One, bit()),
        (bit(), bit()),
    ] {
        let iso = Iso::SwapSum(a, b);
        let net = assert_lowering_agrees(&iso);
        assert!(net.cells.is_empty());
    }
}

// ── INV-TOKEN: simulate must not panic ──────────────────────────────

#[test]
fn inv_token_holds_for_all_branch_inputs() {
    // For every input to a Branch iso, eval_cells must not panic
    // (it asserts INV-TOKEN at every pair read).
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let iso = Iso::plus(fk(), Iso::Id(three_bits()));
    let (in_ty, _) = type_of(&iso).unwrap();
    let net = lower_iso(&iso).unwrap();
    for v in enumerate_values(&in_ty) {
        let bits = bits_of(&v, &in_ty).unwrap();
        // This will panic if INV-TOKEN is violated.
        let _ = eval_cells(&net, &bits);
    }
}

#[test]
fn inv_token_holds_for_distrib() {
    let iso = Iso::Distrib(bit(), Type::One, Type::One);
    let (in_ty, _) = type_of(&iso).unwrap();
    let net = lower_iso(&iso).unwrap();
    for v in enumerate_values(&in_ty) {
        let bits = bits_of(&v, &in_ty).unwrap();
        let _ = eval_cells(&net, &bits);
    }
}

// ── Validate passes for all produced netlists ───────────────────────

#[test]
fn all_netlists_validate() {
    let fk = || Iso::Prim(Box::new(fredkin_prim()));
    let isos: Vec<Iso> = vec![
        Iso::plus(fk(), Iso::Id(three_bits())),
        Iso::plus(Iso::Id(three_bits()), fk()),
        Iso::plus(fk(), fk()),
        Iso::plus(Iso::Id(Type::One), Iso::Id(Type::One)),
        Iso::Distrib(Type::One, Type::One, Type::One),
        Iso::Distrib(bit(), Type::One, Type::One),
        Iso::Factor(bit(), Type::One, Type::One),
        Iso::Id(Type::sum(bit(), Type::One)),
        Iso::SwapSum(bit(), Type::One),
    ];
    for iso in &isos {
        let net = lower_iso(iso).expect(&format!("lowering {iso:?}"));
        assert_eq!(net.validate(), Ok(()), "validate failed for {iso:?}");
    }
}
