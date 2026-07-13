//! Ancilla restoration (Bennett clean-up) tests — ADR-0033 ring
//! closure, M4-v2.
//!
//! Plain lowering leaves data-dependent garbage on its ancilla outputs
//! (2 pairs per synthesized multi-controlled NOT, per Branch
//! conditional-NOT, per TraceLoop tag duplication). [`bennett_wrap`]
//! replays the whole netlist in reverse behind a fan-out copy of the
//! result, which provably restores **every forward ancilla to its
//! init** and concentrates the unavoidable residue (see the parity law
//! in `bennett.rs` module docs, checked here in
//! [`parity_obstruction_is_real`]) into exactly `2·w` boundary slots
//! carrying `(y_j, x_j)` against inits `(1, 0)`.
//!
//! Every test also re-checks the differential oracle: wrapping must
//! not change data outputs, for every input in the domain.

use std::collections::BTreeMap;

use theseus_core::{enumerate_values, type_of, Iso, PrimDef, Type};
use theseus_netlist::bennett::bennett_wrap;
use theseus_netlist::lower::{
    lower_iso_with_ancillas, lower_iso_with_ancillas_traced, prim_from_bits,
};
use theseus_netlist::{bits_of, rail_width, PhasedCellNetlist, RailPair};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn prim_from_fn(
    name: &str,
    input_ty: Type,
    output_ty: Type,
    f: impl Fn(&[bool]) -> Vec<bool>,
) -> PrimDef {
    prim_from_bits(name, input_ty, output_ty, f).expect("encodable test prim")
}

fn cnot_prim() -> PrimDef {
    prim_from_fn("cnot", Type::prod(bit(), bit()), Type::prod(bit(), bit()), |b| {
        vec![b[0], b[0] ^ b[1]]
    })
}

fn three_bits() -> Type {
    Type::prod(bit(), Type::prod(bit(), bit()))
}

fn four_bits() -> Type {
    Type::prod(Type::prod(bit(), bit()), Type::prod(bit(), bit()))
}

/// x ↦ x+1 mod 8 as one monolithic prim: several MMD gates.
fn inc3_prim() -> PrimDef {
    prim_from_fn("inc3", three_bits(), three_bits(), |b| {
        let x = (u8::from(b[0]) << 2) | (u8::from(b[1]) << 1) | u8::from(b[2]);
        let y = (x + 1) & 0b111;
        vec![y & 0b100 != 0, y & 0b010 != 0, y & 0b001 != 0]
    })
}

/// Preserved-input 2-bit adder: MMD emits gates with ≥ 3 controls, so
/// the AND-reduction chain ancillas are exercised too.
fn add2_prim() -> PrimDef {
    prim_from_fn("add2", four_bits(), four_bits(), |bits| {
        let a = (u8::from(bits[0]) << 1) | u8::from(bits[1]);
        let b = (u8::from(bits[2]) << 1) | u8::from(bits[3]);
        let s = (a + b) & 0b11;
        vec![bits[0], bits[1], s & 0b10 != 0, s & 0b01 != 0]
    })
}

/// Untimed functional evaluation of the cell netlist (cells are in
/// topological/emission order; INV-TOKEN asserted at every pair read).
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

fn eval_oracle(iso: &Iso, v: &theseus_core::Value) -> theseus_core::Value {
    let mut fuel = 10_000u64;
    theseus_eval::eval(iso, v.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"))
}

/// Count, per input value, how many ancilla output lines differ from
/// their init (the plain lowering's dirt), positionally — exactly the
/// ring driver's `ancilla_mismatches` semantics.
fn plain_dirty_max(iso: &Iso) -> (usize, usize) {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (net, inits) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    let w_out = rail_width(&out_ty).unwrap();
    let n_anc_out = net.outputs.len() - w_out;
    let mut max_dirty = 0usize;
    for v in enumerate_values(&in_ty) {
        let mut bits = bits_of(&v, &in_ty).unwrap();
        bits.extend_from_slice(&inits);
        let out = eval_cells(&net, &bits);
        let dirty = out[w_out..]
            .iter()
            .zip(&inits)
            .filter(|(got, want)| got != want)
            .count();
        max_dirty = max_dirty.max(dirty);
    }
    (n_anc_out, max_dirty)
}

/// The full Bennett contract, checked over the whole domain:
///
/// 1. oracle: wrapped data outputs == `eval(iso)`;
/// 2. every forward ancilla output == its init (data-independently);
/// 3. the fan-out slots carry exactly `(y_j, x_j)` against `(1, 0)`;
/// 4. hence dirty count == #{j : y_j = 0} + #{j : x_j = 1} ≤ 2w.
///
/// Returns the max dirty count over the domain.
fn assert_bennett_contract(iso: &Iso) -> usize {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (net, inits) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    let (wnet, winits) = bennett_wrap(&net, &inits).expect("bennett_wrap succeeds");
    assert_eq!(wnet.validate(), Ok(()), "wrapped netlist validates");

    let w = rail_width(&out_ty).unwrap();
    assert_eq!(w, rail_width(&in_ty).unwrap(), "iso shape");
    let n_f = inits.len();
    assert_eq!(winits.len(), n_f + 2 * w, "wrap adds one (1,0) pair per data bit");
    assert_eq!(wnet.outputs.len(), w + n_f + 2 * w, "wrapped port count");

    let mut max_dirty = 0usize;
    for v in enumerate_values(&in_ty) {
        let x_bits = bits_of(&v, &in_ty).unwrap();
        let mut bits = x_bits.clone();
        bits.extend_from_slice(&winits);
        let out = eval_cells(&wnet, &bits);

        // 1. Differential oracle on the data section.
        let y_bits = bits_of(&eval_oracle(iso, &v), &out_ty).unwrap();
        assert_eq!(&out[..w], &y_bits[..], "wrap broke the oracle on {v:?}");

        // 2. Forward ancillas: ALL restored to init.
        for i in 0..n_f {
            assert_eq!(
                out[w + i],
                winits[i],
                "forward ancilla {i} not restored on {v:?}"
            );
        }

        // 3. Boundary slots: exactly (y_j, x_j) against inits (1, 0).
        for j in 0..w {
            assert_eq!(out[w + n_f + 2 * j], y_bits[j], "spare slot {j} on {v:?}");
            assert_eq!(out[w + n_f + 2 * j + 1], x_bits[j], "input slot {j} on {v:?}");
        }

        // 4. Dirty count is the predicted boundary residue.
        let dirty = out[w..]
            .iter()
            .zip(&winits)
            .filter(|(got, want)| got != want)
            .count();
        let predicted = y_bits.iter().filter(|&&y| !y).count()
            + x_bits.iter().filter(|&&x| x).count();
        assert_eq!(dirty, predicted, "dirty != boundary residue on {v:?}");
        max_dirty = max_dirty.max(dirty);
    }
    max_dirty
}

// ── Synthesized prims (the mcnot splitter/selector garbage) ─────────

#[test]
fn bennett_cnot_forward_ancillas_clean() {
    let iso = Iso::Prim(Box::new(cnot_prim()));
    // Plain lowering: the single mcnot leaves 2 dirty pairs on some
    // inputs — the per-instance floor (see parity_obstruction test).
    let (n_anc, plain) = plain_dirty_max(&iso);
    assert_eq!(n_anc, 2);
    assert!(plain > 0, "plain cnot lowering has dirty ancillas");
    // Wrapped: forward ancillas clean; residue ≤ 2w = 4.
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 4);
}

#[test]
fn bennett_inc3_reduces_garbage_to_boundary() {
    let iso = Iso::Prim(Box::new(inc3_prim()));
    let (n_anc, plain) = plain_dirty_max(&iso);
    // Several MMD gates → several garbage pairs, mostly dirty.
    assert!(n_anc > 2, "inc3 synthesizes to multiple mcnots (n_anc = {n_anc})");
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 6, "residue bounded by 2w = 6, got {dirty}");
    // Honest note: on a SINGLE small prim the boundary residue can
    // exceed the plain worst case (x = 111 → y = 000 dirties all 2w
    // slots, while plain dirt here peaks at `plain`). The wrap's win is
    // that the residue is composition-independent (see
    // bennett_residue_is_composition_independent) while plain dirt
    // grows with every instance.
    let _ = plain;
}

#[test]
fn bennett_add2_cleans_and_chain_ancillas_too() {
    // MMD on 4 bits emits ≥3-control gates: exercises the AND-reduction
    // chain ancillas inside emit_mcnot as well.
    let iso = Iso::Prim(Box::new(add2_prim()));
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 8, "residue bounded by 2w = 8, got {dirty}");
}

// ── Long compositions: garbage grows, residue doesn't ───────────────

#[test]
fn bennett_residue_is_composition_independent() {
    // cnot ; cnot ; cnot ; cnot = identity on 2 bits. Plain lowering
    // accumulates 8 garbage pairs (2 per instance); the wrap's residue
    // stays ≤ 2w = 4 however long the pipeline gets.
    let cn = || Iso::Prim(Box::new(cnot_prim()));
    let iso = Iso::seq(cn(), Iso::seq(cn(), Iso::seq(cn(), cn())));
    let (n_anc, plain) = plain_dirty_max(&iso);
    assert_eq!(n_anc, 8, "2 garbage pairs per cnot instance");
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 4);
    assert!(
        plain > dirty,
        "wrap wins on compositions (plain {plain} vs wrapped {dirty})"
    );
}

// ── Branch (conditional-NOT construction) ───────────────────────────

#[test]
fn bennett_branch_cond_not_cleans() {
    // Plus(NOT, Id): the NOT arm is zero-cell rewiring, so Branch
    // synthesizes the ancilla-based conditional-NOT ("cond_not").
    let not = Iso::Prim(Box::new(prim_from_fn("not", bit(), bit(), |b| vec![!b[0]])));
    let iso = Iso::plus(not, Iso::Id(bit()));
    let (n_anc, plain) = plain_dirty_max(&iso);
    assert!(n_anc > 0, "branch cond-not allocates ancillas");
    assert!(plain > 0, "plain branch cond-not leaves dirt");
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 4, "residue ≤ 2w = 4, got {dirty}");
}

// ── TraceLoop (tag duplication garbage) ─────────────────────────────

#[test]
fn bennett_trace_trivial_is_fully_clean() {
    // trace(One, SwapSum(One,One)) : One ↔ One — w = 0, so the wrap has
    // no boundary residue at all: EVERY ancilla restored. A lowered iso
    // with zero data rails becomes genuinely ancilla-clean.
    let iso = Iso::trace(Type::One, Iso::SwapSum(Type::One, Type::One));
    let (_, plain) = plain_dirty_max(&iso);
    assert!(plain > 0, "plain trace lowering leaves dirt");
    let dirty = assert_bennett_contract(&iso);
    assert_eq!(dirty, 0, "w = 0 ⇒ no residue: fully ancilla-clean");
}

#[test]
fn bennett_trace_bit_carrier_cleans_tag_dups() {
    // trace(Bit, SwapSum(Bit, Bit)) : Bit ↔ Bit — unrolled body with
    // controlled applications and tag-duplication garbage.
    let iso = Iso::trace(bit(), Iso::SwapSum(bit(), bit()));
    let (n_anc, plain) = plain_dirty_max(&iso);
    assert!(n_anc > 2, "trace unroll allocates many ancillas (n_anc = {n_anc})");
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 2, "residue ≤ 2w = 2, got {dirty}");
    assert!(plain >= dirty);
}

// ── The honest obstruction, machine-checked ─────────────────────────

#[test]
fn parity_obstruction_is_real() {
    // Every cell kind is a value-permutation of its rail pairs, and
    // rail-role swaps are fixed per edge — so any lowering satisfies
    //   parity(data_out) ⊕ parity(anc_out)
    //     = parity(data_in) ⊕ parity(anc_in) ⊕ const.
    // (a) Machine-check the premise: K0..K3 permute their inputs.
    use theseus_netlist::CellKind;
    for kind in [CellKind::K0, CellKind::K1, CellKind::K2, CellKind::K3] {
        let n = kind.arity();
        for x in 0..(1u32 << n) {
            let ins: Vec<bool> = (0..n).map(|i| (x >> i) & 1 == 1).collect();
            let mut outs = kind.eval(&ins);
            let mut sorted_ins = ins.clone();
            sorted_ins.sort_unstable();
            outs.sort_unstable();
            assert_eq!(sorted_ins, outs, "{kind:?} must permute its values");
        }
    }

    // (b) Hence: an iso whose data-parity deviation is non-constant can
    // NEVER have all-constant ancilla outputs — some residue is
    // unavoidable, wrap or no wrap. CNOT is the smallest witness:
    let iso = Iso::Prim(Box::new(cnot_prim()));
    let (in_ty, out_ty) = type_of(&iso).expect("types");
    let deviations: std::collections::BTreeSet<bool> = enumerate_values(&in_ty)
        .into_iter()
        .map(|v| {
            let px = bits_of(&v, &in_ty).unwrap().iter().filter(|&&b| b).count() % 2;
            let y = eval_oracle(&iso, &v);
            let py = bits_of(&y, &out_ty).unwrap().iter().filter(|&&b| b).count() % 2;
            px != py
        })
        .collect();
    assert_eq!(
        deviations.len(),
        2,
        "cnot's parity deviation is non-constant ⇒ zero-garbage lowering \
         is impossible in the K-cell vocabulary; the Bennett wrap's \
         boundary residue is the honest minimum shape"
    );
}

// ── Origin attribution (diagnostics used for the step report) ───────

#[test]
fn ancilla_origins_attribute_constructs() {
    let iso = Iso::Prim(Box::new(cnot_prim()));
    let (_, inits, origins) = lower_iso_with_ancillas_traced(&iso).expect("lowers");
    assert_eq!(origins.inputs.len(), inits.len());
    assert!(origins.inputs.iter().all(|&o| o == "prim_synth"));
    assert!(origins.outputs.iter().all(|&o| o == "prim_synth"));

    let iso = Iso::trace(bit(), Iso::SwapSum(bit(), bit()));
    let (_, _, origins) = lower_iso_with_ancillas_traced(&iso).expect("lowers");
    let mut per: BTreeMap<&'static str, usize> = BTreeMap::new();
    for o in &origins.inputs {
        *per.entry(o).or_insert(0) += 1;
    }
    assert!(per.contains_key("trace"), "trace allocs attributed: {per:?}");
    assert!(per.contains_key("cond_not"), "cond-not allocs attributed: {per:?}");
}
