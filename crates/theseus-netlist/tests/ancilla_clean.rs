//! Ancilla restoration tests — ADR-0033 ring closure, updated for the
//! garbage-free R1 synthesis (ADR-0040 phase C).
//!
//! # What changed (ADR-0040)
//!
//! The K0–K3 vocabulary has a proven parity obstruction (see
//! [`parity_obstruction_is_real`], still checked and still true): every
//! K-cell permutes the values on its rail pairs, so a conditional-NOT —
//! whose parity deviation *is* its AND — could never be lowered with
//! all-constant ancilla outputs. The old splitter/selector
//! multi-controlled-NOT construction paid 2 dirty pairs per instance,
//! and the Bennett wrap ([`bennett_wrap`]) was the only way to
//! concentrate that unbounded per-construct garbage into a bounded
//! `2·w` boundary residue.
//!
//! The `R1` rail-role-swap cell is NOT a value-permutation (it flips
//! its target's value in place), so the law's premise fails for it and
//! the lowering now emits conditional-NOTs **garbage-free**: CNOT is
//! one R1 with zero ancillas; Toffoli and every k-controlled NOT are a
//! K1 AND-chain + one R1 + the replayed chain, restoring every scratch
//! pair to its init on every input. The first test section verifies
//! this zero residue over whole domains — the obstruction voided in
//! practice.
//!
//! # What the Bennett machinery still does
//!
//! The parity law remains a theorem *about the K-cells*, and some
//! constructs still emit K-only garbage (the TraceLoop tag
//! duplication, the iter/fold control chains). For those,
//! [`bennett_wrap`]'s compute–copy–uncompute discipline still turns
//! unbounded per-construct garbage into the fixed `2·w` boundary
//! residue — checked below on trace-based bodies, where the plain dirt
//! grows with composition length and the wrapped residue does not.

use std::collections::BTreeMap;

use theseus_core::{enumerate_values, type_of, Iso, PrimDef, Type};
use theseus_netlist::bennett::bennett_wrap;
use theseus_netlist::lower::{
    lower_iso_with_ancillas, lower_iso_with_ancillas_traced, prim_from_bits,
};
use theseus_netlist::{bits_of, rail_width, CellKind, PhasedCellNetlist, RailPair};

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

/// Toffoli as a monolithic prim: (c1, c2, t) ↦ (c1, c2, t ⊕ (c1 ∧ c2)).
fn toffoli_prim() -> PrimDef {
    prim_from_fn("toffoli", three_bits(), three_bits(), |b| {
        vec![b[0], b[1], b[2] ^ (b[0] && b[1])]
    })
}

/// CCC-NOT (3 controls) as a monolithic prim.
fn cccnot_prim() -> PrimDef {
    prim_from_fn("cccnot", four_bits(), four_bits(), |b| {
        vec![b[0], b[1], b[2], b[3] ^ (b[0] && b[1] && b[2])]
    })
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
/// the K1 AND-reduction chain (and its replay) is exercised too.
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
/// ring driver's `ancilla_mismatches` semantics. Also re-checks the
/// differential oracle on the data section.
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
        let y_bits = bits_of(&eval_oracle(iso, &v), &out_ty).unwrap();
        assert_eq!(&out[..w_out], &y_bits[..], "plain lowering broke the oracle on {v:?}");
        let dirty = out[w_out..]
            .iter()
            .zip(&inits)
            .filter(|(got, want)| got != want)
            .count();
        max_dirty = max_dirty.max(dirty);
    }
    (n_anc_out, max_dirty)
}

/// The garbage-free contract (ADR-0040 phase C), checked over the
/// whole domain: the plain lowering's data outputs match the oracle
/// AND every ancilla output equals its init on every input — zero
/// residue, no Bennett wrap needed. Returns the ancilla port count.
fn assert_garbage_free(iso: &Iso) -> usize {
    let (n_anc, dirty) = plain_dirty_max(iso);
    assert_eq!(
        dirty, 0,
        "expected a garbage-free lowering, found dirty ancillas"
    );
    n_anc
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

// ── R1 synthesis: garbage-free conditional-NOT (ADR-0040 phase C) ───
//
// These are the constructions the parity law used to forbid: lowered
// conditional-NOTs whose ancilla outputs are constant (= their inits)
// on EVERY input, while the data parity deviation is non-constant.
// Contrast the old splitter/selector synthesis, which was stuck at 2
// dirty pairs per multi-controlled NOT (the per-instance floor this
// file used to demonstrate).

#[test]
fn r1_cnot_lowering_is_garbage_free_zero_ancillas() {
    // CNOT = one R1 cell: no ancilla ports at all (before: 2 dirty).
    let iso = Iso::Prim(Box::new(cnot_prim()));
    let n_anc = assert_garbage_free(&iso);
    assert_eq!(n_anc, 0, "cnot lowers with zero ancilla ports");
    let (net, _) = lower_iso_with_ancillas(&iso).unwrap();
    assert_eq!(net.cell_counts(), BTreeMap::from([(CellKind::R1, 1)]));
}

#[test]
fn r1_toffoli_lowering_is_ancilla_clean() {
    // Toffoli = K1(c1, c2, 0) → R1(∧, t) → replayed K1: one scratch
    // pair in, restored to 0 on all 8 inputs (before: 2 dirty).
    let iso = Iso::Prim(Box::new(toffoli_prim()));
    let n_anc = assert_garbage_free(&iso);
    assert_eq!(n_anc, 1, "toffoli uses exactly one (clean) scratch pair");
}

#[test]
fn r1_cccnot_lowering_is_ancilla_clean() {
    // 3 controls: two-step K1 AND-chain → R1 → chain replayed. Both
    // scratch pairs restored to 0 on all 16 inputs.
    let iso = Iso::Prim(Box::new(cccnot_prim()));
    let n_anc = assert_garbage_free(&iso);
    assert_eq!(n_anc, 2, "cccnot uses exactly two (clean) chain scratch pairs");
}

#[test]
fn r1_synthesized_prims_are_ancilla_clean() {
    // Whole synthesized prims (several MMD gates each, incl. 0-control
    // and ≥ 3-control gates): every ancilla restored on every input.
    assert_garbage_free(&Iso::Prim(Box::new(inc3_prim())));
    assert_garbage_free(&Iso::Prim(Box::new(add2_prim())));
}

#[test]
fn r1_cnot_pipeline_is_composition_clean() {
    // cnot ; cnot ; cnot ; cnot: the old synthesis accumulated 2 dirty
    // pairs per instance (8 total); the R1 pipeline stays at zero.
    let cn = || Iso::Prim(Box::new(cnot_prim()));
    let iso = Iso::seq(cn(), Iso::seq(cn(), Iso::seq(cn(), cn())));
    let n_anc = assert_garbage_free(&iso);
    assert_eq!(n_anc, 0, "no ancilla ports however long the cnot pipeline");
}

#[test]
fn branch_conditional_not_is_garbage_free_via_r1() {
    // Plus(NOT, Id): the NOT arm is zero-cell rewiring, so Branch
    // synthesizes a conditional NOT — now a single R1 with the tag as
    // control (before: splitter/selector, 2 ancillas + dirt).
    let not = Iso::Prim(Box::new(prim_from_fn("not", bit(), bit(), |b| vec![!b[0]])));
    let iso = Iso::plus(not, Iso::Id(bit()));
    let n_anc = assert_garbage_free(&iso);
    assert_eq!(n_anc, 0, "branch cond-NOT needs no ancillas");
    // The wrap still composes over it (vacuously: boundary residue only).
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 4, "residue ≤ 2w = 4, got {dirty}");
}

// ── Bennett wrap on the now-clean synthesized prims ─────────────────
//
// The wrap must keep honoring its contract when the forward net is
// already clean (its residue is then just the 2·w boundary).

#[test]
fn bennett_cnot_forward_ancillas_clean() {
    let iso = Iso::Prim(Box::new(cnot_prim()));
    // Plain lowering is now itself clean (R1; contrast the pre-ADR-0040
    // state where the single mcnot left 2 dirty pairs and this test
    // demonstrated that floor).
    let (n_anc, plain) = plain_dirty_max(&iso);
    assert_eq!((n_anc, plain), (0, 0), "plain cnot lowering is garbage-free");
    // Wrapped: forward ancillas (vacuously) clean; residue ≤ 2w = 4.
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 4);
}

#[test]
fn bennett_inc3_wrap_contract_on_clean_forward() {
    let iso = Iso::Prim(Box::new(inc3_prim()));
    let (n_anc, plain) = plain_dirty_max(&iso);
    assert!(n_anc > 0, "inc3 still allocates (clean) scratch ancillas");
    assert_eq!(plain, 0, "inc3 plain lowering is garbage-free via R1");
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 6, "residue bounded by 2w = 6, got {dirty}");
}

#[test]
fn bennett_add2_cleans_and_chain_ancillas_too() {
    // MMD on 4 bits emits ≥3-control gates: exercises the K1
    // AND-reduction chain inside emit_mcnot under the wrap as well.
    let iso = Iso::Prim(Box::new(add2_prim()));
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 8, "residue bounded by 2w = 8, got {dirty}");
}

// ── Long compositions: K-only garbage grows, wrap residue doesn't ───

#[test]
fn bennett_residue_is_composition_independent() {
    // The synthesized-prim path is garbage-free now, so the growing-
    // garbage witness is a construct that still emits K-only dirt: the
    // TraceLoop tag duplication. tr = trace(Bit, SwapSum(Bit, Bit)) is
    // Bit ↔ Bit; a pipeline of 4 accumulates tag-dup garbage per
    // instance, while the wrap's residue stays ≤ 2w = 2 however long
    // the pipeline gets.
    let tr = || Iso::trace(bit(), Iso::SwapSum(bit(), bit()));
    let one_instance_dirty = {
        let (_, plain) = plain_dirty_max(&tr());
        plain
    };
    assert!(one_instance_dirty > 0, "trace tag-dup still leaves K-only dirt");
    let iso = Iso::seq(tr(), Iso::seq(tr(), Iso::seq(tr(), tr())));
    let (n_anc, plain) = plain_dirty_max(&iso);
    assert!(
        plain >= one_instance_dirty,
        "plain dirt does not shrink under composition (n_anc = {n_anc})"
    );
    let dirty = assert_bennett_contract(&iso);
    assert!(dirty <= 2, "wrap residue ≤ 2w = 2, got {dirty}");
    assert!(
        plain > dirty,
        "wrap wins on compositions (plain {plain} vs wrapped {dirty})"
    );
}

// ── TraceLoop (tag duplication garbage) ─────────────────────────────

#[test]
fn bennett_trace_trivial_is_fully_clean() {
    // trace(One, SwapSum(One,One)) : One ↔ One — w = 0, so the wrap has
    // no boundary residue at all: EVERY ancilla restored. Since the R1
    // cond-NOT rewrite even the PLAIN lowering of this one-input iso is
    // positionally clean (the tag copy exits carrying 1 against the dup
    // pair's 1-init; before ADR-0040 the splitter/selector dirt made
    // this assert plain > 0).
    let iso = Iso::trace(Type::One, Iso::SwapSum(Type::One, Type::One));
    let (n_anc, plain) = plain_dirty_max(&iso);
    assert!(n_anc > 0, "trace still allocates tag ancillas");
    assert_eq!(plain, 0, "trivial trace is clean even unwrapped");
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
    // The theorem is about the K-ONLY vocabulary and it remains true:
    // every K-cell is a value-permutation of its rail pairs, and
    // rail-role swaps are fixed per edge — so any K-only lowering
    // satisfies
    //   parity(data_out) ⊕ parity(anc_out)
    //     = parity(data_in) ⊕ parity(anc_in) ⊕ const.
    // (a) Machine-check the premise: K0..K3 permute their inputs.
    // (Deliberately NOT R1 — R1 breaks exactly this premise, which is
    // what makes the garbage-free lowerings above possible; see
    // tests/rail_role_swap.rs::role_swap_breaks_the_value_permutation_premise.)
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

    // (b) Hence: an iso whose data-parity deviation is non-constant
    // could NEVER have all-constant ancilla outputs in K-cells alone.
    // CNOT is the smallest witness — and with R1 in the vocabulary the
    // same CNOT now lowers with zero garbage
    // (r1_cnot_lowering_is_garbage_free_zero_ancillas): the law is
    // voided by extending the vocabulary, not falsified within it.
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
         is impossible in the K-cell vocabulary (R1 is the escape hatch)"
    );
}

// ── Origin attribution (diagnostics used for the step report) ───────

#[test]
fn ancilla_origins_attribute_constructs() {
    // CNOT: no ancillas at all now.
    let iso = Iso::Prim(Box::new(cnot_prim()));
    let (_, inits, origins) = lower_iso_with_ancillas_traced(&iso).expect("lowers");
    assert!(inits.is_empty(), "cnot has no ancillas");
    assert!(origins.inputs.is_empty());
    assert!(origins.outputs.is_empty());

    // CCC-NOT: the chain scratch pairs are attributed to the
    // AND-reduction chain.
    let iso = Iso::Prim(Box::new(cccnot_prim()));
    let (_, inits, origins) = lower_iso_with_ancillas_traced(&iso).expect("lowers");
    assert_eq!(origins.inputs.len(), inits.len());
    assert!(origins.inputs.iter().all(|&o| o == "prim_synth_chain"));
    assert!(origins.outputs.iter().all(|&o| o == "prim_synth_chain"));

    // Trace: tag-injection/duplication ancillas are attributed to
    // "trace"; the old "cond_not" origin is gone — conditional NOTs
    // are ancilla-free R1 cells now.
    let iso = Iso::trace(bit(), Iso::SwapSum(bit(), bit()));
    let (_, _, origins) = lower_iso_with_ancillas_traced(&iso).expect("lowers");
    let mut per: BTreeMap<&'static str, usize> = BTreeMap::new();
    for o in &origins.inputs {
        *per.entry(o).or_insert(0) += 1;
    }
    assert!(per.contains_key("trace"), "trace allocs attributed: {per:?}");
    assert!(
        !per.contains_key("cond_not"),
        "cond-NOTs no longer allocate ancillas: {per:?}"
    );
}
