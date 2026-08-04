//! Differential oracle tests for the `iter`/`fold`/`unfold` unroll
//! lowering (ADR-0039).
//!
//! Each test asserts that for EVERY input value in the finite domain,
//! the lowered cell netlist produces the same output as the core
//! interpreter (`theseus_eval::eval`). Additionally, every produced
//! netlist must `validate()`, `balance()` (these nets are tiny), stay
//! within the ≤ K3 cell vocabulary, and never violate INV-TOKEN in the
//! untimed simulation.

use std::collections::BTreeMap;

use theseus_core::{enumerate_values, type_of, Iso, PrimDef, Type, Value};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{bits_of, rail_width, CellKind, PhasedCellNetlist, RailPair};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn three_ty() -> Type {
    Type::sum(Type::One, bit())
}

/// Untimed functional evaluation of the cell netlist.  All input pairs
/// (data + ancilla) must be provided in `input_bits`.
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

/// Differential oracle: for every input value, the lowered netlist's
/// untimed evaluation must agree with `theseus_eval::eval`; the net
/// must validate, balance, and stay within the admitted vocabulary
/// (K0-K3 CSWAPs + the R1 rail-role swap, ADR-0040).
fn assert_lowering_agrees(iso: &Iso) -> PhasedCellNetlist {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (netlist, ancilla_values) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    assert_eq!(netlist.validate(), Ok(()), "lowered netlist validates");
    assert!(
        netlist
            .cells
            .iter()
            .all(|c| c.kind <= CellKind::K3 || c.kind == CellKind::R1),
        "no cell outside the K0-K3 + R1 vocabulary (max promotion budget)"
    );
    // These nets are tiny: the tick-exact balancer must handle them.
    let balanced = theseus_netlist::balance(&netlist).expect("netlist balances");
    assert_eq!(balanced.validate(), Ok(()), "balanced netlist validates");

    let n_data_out = rail_width(&out_ty).unwrap();
    for v in enumerate_values(&in_ty) {
        let mut full_input = bits_of(&v, &in_ty).unwrap();
        full_input.extend_from_slice(&ancilla_values);
        let got_full = eval_cells(&netlist, &full_input);
        let got = &got_full[..n_data_out];
        let mut fuel = 100_000u64;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        assert_eq!(
            got,
            &bits_of(&expected, &out_ty).unwrap()[..],
            "netlist disagrees with core eval on {v:?}"
        );
    }
    netlist
}

/// Same oracle against the TIMED phased simulation of the balanced
/// net -- asserts dynamic INV-TOKEN on every tick.
fn assert_balanced_timed_sim_agrees(iso: &Iso) {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (netlist, ancilla_values) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    let balanced = theseus_netlist::balance(&netlist).expect("netlist balances");
    let n_data_out = rail_width(&out_ty).unwrap();
    for v in enumerate_values(&in_ty) {
        let mut bits = bits_of(&v, &in_ty).unwrap();
        bits.extend_from_slice(&ancilla_values);
        let res = theseus_netlist::simulate(&balanced, &bits).expect("timed sim");
        let mut fuel = 100_000u64;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(
            &res.outputs[..n_data_out],
            &bits_of(&expected, &out_ty).unwrap()[..],
            "timed sim disagrees with core eval on {v:?}"
        );
    }
}

// ── Test bodies ─────────────────────────────────────────────────────

/// `not : Bit ↔ Bit` as a structural iso (zero-cell rail swap when
/// unconditional -- forces the conditional-NOT synthesis under gating).
fn not_iso() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// `rot3 : Three ↔ Three`, successor mod 3 -- NOT self-inverse, so
/// `iterᵏ` distinguishes every k mod 3. Lowered via B4.2 synthesis.
fn rot3() -> Iso {
    let t0 = Value::left(Value::Unit);
    let t1 = Value::right(Value::left(Value::Unit));
    let t2 = Value::right(Value::right(Value::Unit));

    let mut fwd = BTreeMap::new();
    fwd.insert(t0.clone(), t1.clone());
    fwd.insert(t1.clone(), t2.clone());
    fwd.insert(t2.clone(), t0.clone());
    let mut bwd = BTreeMap::new();
    bwd.insert(t1.clone(), t0.clone());
    bwd.insert(t2.clone(), t1);
    bwd.insert(t0, t2);

    Iso::Prim(Box::new(PrimDef {
        forward_name: "rot3".into(),
        backward_name: "rot3_inv".into(),
        input_ty: three_ty(),
        output_ty: three_ty(),
        forward: fwd,
        backward: bwd,
        symbolic: None,
    }))
}

/// `cx : Bit × Bit ↔ Bit × Bit` (CNOT, self-inverse): ⟨x, a⟩ ↦ ⟨x, a⊕x⟩.
/// Folding it accumulates the parity of the list into the accumulator.
fn cx() -> Iso {
    let ty = Type::prod(bit(), bit());
    let def = theseus_netlist::prim_from_bits("cx", ty.clone(), ty, |b| vec![b[0], b[0] ^ b[1]])
        .expect("cx prim");
    Iso::Prim(Box::new(def))
}

/// A NON-self-inverse, order-sensitive body: `cx ; swapˣ`,
/// ⟨x, a⟩ ↦ ⟨a⊕x, x⟩ -- distinguishes head-to-tail (`fold`) from
/// tail-to-head (`unfold`) threading.
fn cx_swap() -> Iso {
    Iso::seq(cx(), Iso::SwapProd(bit(), bit()))
}

// ═══════════════════════════════════════════════════════════════════
// iter
// ═══════════════════════════════════════════════════════════════════

#[test]
fn iter_not_over_bit_full_domain() {
    // iter N₄ not : ⟨k, b⟩ ↦ ⟨k, b ⊕ (k mod 2)⟩, swept over all 8
    // values. Zero-cell body → exercises the gated conditional-NOT
    // synthesis inside controlled_apply.
    assert_lowering_agrees(&Iso::iter(Type::numeral_ty(4), not_iso()));
}

#[test]
fn iter_rot3_nonselfinverse_body() {
    // iter N₄ rot3 over N₄ × Three (12 values): a synthesized prim
    // body, applied a data-dependent number of times.
    assert_lowering_agrees(&Iso::iter(Type::numeral_ty(4), rot3()));
}

#[test]
fn iter_singleton_counter_is_identity() {
    // numeral_ty(1) has the single value 0: no iterations, no cells.
    let net = assert_lowering_agrees(&Iso::iter(Type::numeral_ty(1), not_iso()));
    assert!(net.cells.is_empty(), "0-step iter lowers to pure wiring");
}

#[test]
fn iter_body_with_branch() {
    // Body = id + not : (1 + Bit) ↔ (1 + Bit) -- a Branch inside the
    // gated body: nested control promotion (Branch K1s → K2 under the
    // iteration gate).
    let body = Iso::plus(Iso::Id(Type::One), not_iso());
    assert_lowering_agrees(&Iso::iter(Type::numeral_ty(3), body));
}

#[test]
fn iter_inside_composition() {
    // swapˣ ; iter ; swapˣ -- the loop node composes with canonicalized
    // plumbing on both sides.
    let n = Type::numeral_ty(3);
    let iso = Iso::seq(
        Iso::seq(
            Iso::SwapProd(three_ty(), n.clone()),
            Iso::iter(n.clone(), rot3()),
        ),
        Iso::SwapProd(n, three_ty()),
    );
    assert_lowering_agrees(&iso);
}

#[test]
fn sym_iter_lowers_via_adjoint_body() {
    // (iter n f)† = iter n (f†): sym is eliminated syntactically, and
    // the lowered net must match the BACKWARD semantics.
    assert_lowering_agrees(&Iso::sym(Iso::iter(Type::numeral_ty(4), rot3())));
}

#[test]
fn iter_balances_and_timed_sims_with_inv_token() {
    assert_balanced_timed_sim_agrees(&Iso::iter(Type::numeral_ty(3), rot3()));
}

// ═══════════════════════════════════════════════════════════════════
// fold / unfold
// ═══════════════════════════════════════════════════════════════════

#[test]
fn fold_parity_cx_full_domain() {
    // fold L₃(Bit) cx over L₃(Bit) × Bit (14 values): parity of the
    // list accumulates; the list itself is preserved.
    assert_lowering_agrees(&Iso::fold(Type::list_spine_ty(bit(), 3), cx()));
}

#[test]
fn unfold_parity_cx_full_domain() {
    assert_lowering_agrees(&Iso::unfold(Type::list_spine_ty(bit(), 3), cx()));
}

#[test]
fn fold_order_sensitive_body() {
    // cx;swapˣ is order- and direction-sensitive: the oracle fails if
    // the accumulator is threaded in the wrong order or the rewritten
    // elements land in the wrong slots.
    assert_lowering_agrees(&Iso::fold(Type::list_spine_ty(bit(), 4), cx_swap()));
}

#[test]
fn unfold_order_sensitive_body() {
    // The tail-to-head dual: distinguishes unfold's descending gated
    // unroll from fold's ascending one.
    assert_lowering_agrees(&Iso::unfold(Type::list_spine_ty(bit(), 4), cx_swap()));
}

#[test]
fn fold_depth_one_spine_is_identity() {
    // L₁(Bit) holds only nil: no steps, no cells.
    let net = assert_lowering_agrees(&Iso::fold(Type::list_spine_ty(bit(), 1), cx()));
    assert!(net.cells.is_empty(), "0-step fold lowers to pure wiring");
}

#[test]
fn sym_fold_lowers_as_unfold_of_adjoint() {
    // (fold l f)† = unfold l (f†) -- the mutually adjoint pairing, run
    // through the lowering.
    assert_lowering_agrees(&Iso::sym(Iso::fold(
        Type::list_spine_ty(bit(), 3),
        cx_swap(),
    )));
}

#[test]
fn fold_then_unfold_of_adjoint_is_identity_net() {
    // fold(f) ; unfold(f†) = id over the whole domain, as one lowered
    // netlist (two loop nodes back to back).
    let l = Type::list_spine_ty(bit(), 3);
    let f = cx_swap();
    let iso = Iso::seq(
        Iso::fold(l.clone(), f.clone()),
        Iso::unfold(l, theseus_core::adjoint(&f)),
    );
    assert_lowering_agrees(&iso);
}

#[test]
fn fold_balances_and_timed_sims_with_inv_token() {
    assert_balanced_timed_sim_agrees(&Iso::fold(Type::list_spine_ty(bit(), 3), cx_swap()));
}

// ═══════════════════════════════════════════════════════════════════
// Encoding: numeral / spine types are one-hot tag strings
// ═══════════════════════════════════════════════════════════════════

#[test]
fn numeral_and_spine_encodings_round_trip() {
    // The 0-terminated sum types now encode; numerals are one-hot.
    let n4 = Type::numeral_ty(4);
    assert_eq!(rail_width(&n4), Ok(4));
    for (k, v) in enumerate_values(&n4).iter().enumerate() {
        let bits = bits_of(v, &n4).unwrap();
        let expected: Vec<bool> = (0..4).map(|j| j == k).collect();
        assert_eq!(bits, expected, "numeral {k} is one-hot at {k}");
        assert_eq!(theseus_netlist::value_from_bits(&n4, &bits), Ok(v.clone()));
    }
    let l3 = Type::list_spine_ty(bit(), 3);
    assert_eq!(rail_width(&l3), Ok(5)); // 3 tags + 2 element slots
    for v in enumerate_values(&l3) {
        let bits = bits_of(&v, &l3).unwrap();
        assert_eq!(theseus_netlist::value_from_bits(&l3, &bits), Ok(v));
    }
}

// ═══════════════════════════════════════════════════════════════════
// Reporting: cell counts / max kind for the headline examples
// ═══════════════════════════════════════════════════════════════════

#[test]
fn report_cell_counts() {
    for (name, iso) in [
        ("iter N4 not", Iso::iter(Type::numeral_ty(4), not_iso())),
        ("iter N4 rot3", Iso::iter(Type::numeral_ty(4), rot3())),
        (
            "fold L3(Bit) cx",
            Iso::fold(Type::list_spine_ty(bit(), 3), cx()),
        ),
        (
            "unfold L4(Bit) cx;swap",
            Iso::unfold(Type::list_spine_ty(bit(), 4), cx_swap()),
        ),
    ] {
        let net = theseus_netlist::lower_iso(&iso).expect("lowers");
        let max_kind = net.cells.iter().map(|c| c.kind).max();
        println!(
            "{name}: {} cells, counts {:?}, max kind {:?}, depth {}",
            net.cells.len(),
            net.cell_counts(),
            max_kind,
            net.depth()
        );
        assert!(net
            .cells
            .iter()
            .all(|c| c.kind <= CellKind::K3 || c.kind == CellKind::R1));
    }
}
