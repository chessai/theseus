//! Regression tests for controlled-op lowering correctness (M3b-A2).
//!
//! The bug: zero-cell arms (Id / NOT-as-rail-swap) in Branch lowering
//! are applied unconditionally, so the per-arm transformation is lost.
//! These tests exercise the differential oracle over the WHOLE finite
//! domain for elaborated controlled-NOT, Toffoli, swapMaybe, and a
//! hand-built controlled-NOT Iso.

use theseus_core::{enumerate_values, type_of, Iso, Type};
use theseus_netlist::{bits_of, lower_iso, rail_width, PhasedCellNetlist, RailPair};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// Untimed functional evaluation of the cell netlist, with support for
/// ancilla inputs.  Ancilla inputs (beyond `n_data` pairs) are
/// initialised to the alternating pattern (true, false, true, false, …)
/// matching the allocation order in the conditional-NOT construction.
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

/// Provide data bits plus ancilla values for any extra inputs.
fn extend_with_ancilla(data_bits: &[bool], net: &PhasedCellNetlist) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    let n_ancilla = net.inputs.len() - data_bits.len();
    for i in 0..n_ancilla {
        bits.push(i % 2 == 0); // alternating: true, false, true, false, …
    }
    bits
}

/// Differential oracle: for every input value, the lowered netlist's
/// untimed evaluation must agree with `theseus_eval::eval`.  Handles
/// ancilla inputs/outputs transparently.
fn assert_lowering_agrees(iso: &Iso) -> PhasedCellNetlist {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let net = lower_iso(iso).expect("lowering succeeds");
    assert_eq!(net.validate(), Ok(()), "lowered netlist validates");
    let n_data_out = rail_width(&out_ty).unwrap();
    for v in enumerate_values(&in_ty) {
        let data_bits = bits_of(&v, &in_ty).unwrap();
        let full_input = extend_with_ancilla(&data_bits, &net);
        let got_full = eval_cells(&net, &full_input);
        let got = &got_full[..n_data_out];
        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        assert_eq!(
            got,
            &bits_of(&expected, &out_ty).unwrap()[..],
            "netlist disagrees with core eval on {v:?}"
        );
    }
    net
}

// ── Helper: elaborate a Theseus source program to an Iso ────────────

fn elaborate(src: &str, iso_name: &str) -> Iso {
    let program = theseus_syntax::parse(src).expect("parse");
    let diags = theseus_check::check(&program);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = theseus_check::env::resolve(&program);
    let isos = theseus_check::elaborate::elaborate_program(&program, &env);
    isos.get(iso_name)
        .unwrap_or_else(|| panic!("iso {iso_name:?} not found in elaboration"))
        .clone()
        .unwrap_or_else(|e| panic!("elaboration of {iso_name:?} failed: {e:?}"))
}

// ── Elaborated conformance programs ─────────────────────────────────

const CNOT_SRC: &str = "\
type Bit = O | I

iso cnot : (Bit, Bit) <-> (Bit, Bit)
| (O, b) <-> (O, b)
| (I, O) <-> (I, I)
| (I, I) <-> (I, O)
";

#[test]
fn elaborated_cnot_agrees_with_eval() {
    let iso = elaborate(CNOT_SRC, "cnot");
    assert_lowering_agrees(&iso);
}

#[test]
fn elaborated_toffoli_agrees_with_eval() {
    let src = include_str!("../../../conformance/programs/0006-toffoli.thse");
    let iso = elaborate(src, "tof");
    assert_lowering_agrees(&iso);
}

#[test]
fn elaborated_swap_maybe_agrees_with_eval() {
    let src = include_str!("../../../conformance/programs/0001-swap-maybe.thse");
    let iso = elaborate(src, "swapMaybe");
    assert_lowering_agrees(&iso);
}

// ── Hand-built controlled-NOT Iso ───────────────────────────────────

/// Build a NOT iso on Bit (= SwapSum(1,1)).
fn not_iso() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// Build a NOT iso on Bit×One (unite ; NOT ; uniti).
fn not_on_bit_times_one() -> Iso {
    Iso::seq(
        Iso::UniteProd(bit()),
        Iso::seq(not_iso(), Iso::UnitiProd(bit())),
    )
}

/// Hand-built controlled-NOT: Distrib ; Plus(Id, NOT) ; Factor.
///
/// Input type: Bit × Bit.  The second Bit is the "control" (the inner
/// sum b+c in a×(b+c)).  The first Bit is the "target" (the `a` factor).
/// When control = I (Right, tag=false): NOT the target.
/// When control = O (Left, tag=true): identity on the target.
#[test]
fn hand_built_controlled_not_agrees_with_eval() {
    // Distrib(Bit, One, One) ; Plus(Id(Bit×One), NOT_on(Bit×One)) ; Factor(Bit, One, One)
    let iso = Iso::seq(
        Iso::Distrib(bit(), Type::One, Type::One),
        Iso::seq(
            Iso::plus(
                Iso::Id(Type::prod(bit(), Type::One)),
                not_on_bit_times_one(),
            ),
            Iso::Factor(bit(), Type::One, Type::One),
        ),
    );
    assert_lowering_agrees(&iso);
}

/// Same as above but with SwapProd to put control first (standard cnot
/// ordering: first bit controls, second is target).
#[test]
fn hand_built_cnot_standard_order() {
    let inner = Iso::seq(
        Iso::Distrib(bit(), Type::One, Type::One),
        Iso::seq(
            Iso::plus(
                Iso::Id(Type::prod(bit(), Type::One)),
                not_on_bit_times_one(),
            ),
            Iso::Factor(bit(), Type::One, Type::One),
        ),
    );
    let iso = Iso::seq(
        Iso::SwapProd(bit(), bit()),
        Iso::seq(inner, Iso::SwapProd(bit(), bit())),
    );
    assert_lowering_agrees(&iso);
}

// ── Branch with zero-cell NOT arms (direct Iso construction) ────────

#[test]
fn branch_not_id_on_bit_sum() {
    // Plus(NOT, Id) on Bit + Bit: when tag=true, NOT; when tag=false, Id.
    let iso = Iso::plus(not_iso(), Iso::Id(bit()));
    assert_lowering_agrees(&iso);
}

#[test]
fn branch_id_not_on_bit_sum() {
    // Plus(Id, NOT) on Bit + Bit: when tag=false, NOT; when tag=true, Id.
    let iso = Iso::plus(Iso::Id(bit()), not_iso());
    assert_lowering_agrees(&iso);
}

#[test]
fn branch_not_not_on_bit_sum() {
    // Plus(NOT, NOT) on Bit + Bit: always NOT (both arms).
    let iso = Iso::plus(not_iso(), not_iso());
    assert_lowering_agrees(&iso);
}

#[test]
fn branch_id_not_unequal_widths() {
    // Plus(Id(One), NOT) on (One + Bit): swapMaybe-like.
    // wa=0, wb=1.
    let iso = Iso::plus(Iso::Id(Type::One), not_iso());
    assert_lowering_agrees(&iso);
}

#[test]
fn branch_not_id_unequal_widths() {
    // Plus(NOT, Id(One)) on (Bit + One): NOT on left arm.
    // wa=1, wb=0.
    let iso = Iso::plus(not_iso(), Iso::Id(Type::One));
    assert_lowering_agrees(&iso);
}
