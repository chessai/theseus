//! Acceptance test for the 5-wire reversible full adder through the
//! full compile pipeline (elaborate → lower → balance → simulate),
//! differential against the core evaluator over the whole 32-value
//! domain.
//!
//! History: this was the K3 acceptance test -- the adder's
//! control-nesting depth 3 forced K3 cells under the old
//! splitter/selector multi-controlled-NOT synthesis. Since the R1
//! rewrite (ADR-0040 phase C) the synthesis AND-reduces controls down
//! a K1 chain and fires one R1, so the adder no longer needs K3 at
//! all -- and its lowering is ancilla-clean (asserted below). K2/K3
//! promotion is still exercised by the Branch/iter tests.

use theseus_core::{enumerate_values, type_of, Iso};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{
    balance, bits_of, rail_width, simulate, CellKind, PhasedCellNetlist, RailPair,
};

/// Untimed functional evaluation of the cell netlist with ancilla support.
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

/// Append the lowering's real ancilla init values to the data bits
/// (positional, from `lower_iso_with_ancillas`).
fn extend_with_ancilla(data_bits: &[bool], ancillas: &[bool]) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    bits.extend_from_slice(ancillas);
    bits
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

// ── Full adder source generation ────────────────────────────────────

/// Generate the 5-wire reversible full adder source.
/// Type: (Bit,Bit,Bit,Bit,Bit) <-> (Bit,Bit,Bit,Bit,Bit)
/// Semantics: (a,b,cin,s,c) <-> (a,b,cin, s XOR (a XOR b XOR cin), c XOR maj(a,b,cin))
fn full_adder_source() -> String {
    let mut src = String::from(
        "type Bit = O | I\n\niso fa5 : (Bit, Bit, Bit, Bit, Bit) <-> (Bit, Bit, Bit, Bit, Bit)\n",
    );

    for v in 0u32..32 {
        let a = v & 1 == 1;
        let b = (v >> 1) & 1 == 1;
        let cin = (v >> 2) & 1 == 1;
        let s = (v >> 3) & 1 == 1;
        let c = (v >> 4) & 1 == 1;

        let sum_bit = a ^ b ^ cin;
        let maj_bit = (a & b) | (a & cin) | (b & cin);
        let s_out = s ^ sum_bit;
        let c_out = c ^ maj_bit;

        let bit_str = |val: bool| if val { "I" } else { "O" };
        let lhs = format!(
            "({}, {}, {}, {}, {})",
            bit_str(a),
            bit_str(b),
            bit_str(cin),
            bit_str(s),
            bit_str(c)
        );
        let rhs = format!(
            "({}, {}, {}, {}, {})",
            bit_str(a),
            bit_str(b),
            bit_str(cin),
            bit_str(s_out),
            bit_str(c_out)
        );

        src.push_str(&format!("| {lhs} <-> {rhs}\n"));
    }
    src
}

// ── Acceptance test: 5-wire full adder ──────────────────────────────

#[test]
fn full_adder_5wire_lowers_garbage_free_without_k3() {
    let src = full_adder_source();
    let iso = elaborate(&src, "fa5");

    // Lower must succeed (no Deferred).
    let (net, ancillas) = lower_iso_with_ancillas(&iso).expect("fa5 lowering succeeds");
    assert_eq!(net.validate(), Ok(()), "lowered netlist validates");

    // Balance must succeed.
    let balanced = balance(&net).expect("balance succeeds");
    assert_eq!(balanced.validate(), Ok(()), "balanced netlist validates");
    assert!(
        balanced.tick_imbalances().unwrap().is_empty(),
        "balanced netlist has no tick imbalances"
    );

    // The R1-based synthesis voids the old K3 requirement: R1 cells
    // are present, K3 is not needed anywhere.
    let counts = balanced.cell_counts();
    assert!(
        counts.get(&CellKind::R1).copied().unwrap_or(0) > 0,
        "R1 synthesis emits R1 cells; counts: {counts:?}"
    );
    assert_eq!(
        counts.get(&CellKind::K3).copied().unwrap_or(0),
        0,
        "the adder no longer needs K3; counts: {counts:?}"
    );
    eprintln!("fa5 cell counts: {counts:?}");

    // Differential oracle over all 32 inputs -- and ancilla cleanliness:
    // every ancilla output equals its init on every input (the parity
    // obstruction voided in practice, ADR-0040 D3).
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let n_data_out = rail_width(&out_ty).unwrap();

    for v in enumerate_values(&in_ty) {
        let data_bits = bits_of(&v, &in_ty).unwrap();

        // Untimed eval on unbalanced net.
        let full_input = extend_with_ancilla(&data_bits, &ancillas);
        let got_full = eval_cells(&net, &full_input);
        let got = &got_full[..n_data_out];

        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        let expected_bits = bits_of(&expected, &out_ty).unwrap();
        assert_eq!(
            got,
            &expected_bits[..],
            "untimed netlist disagrees with core eval on {v:?}"
        );

        // Ancilla residue: zero -- every ancilla restored to its init.
        let dirty = got_full[n_data_out..]
            .iter()
            .zip(&ancillas)
            .filter(|(got, want)| got != want)
            .count();
        assert_eq!(dirty, 0, "fa5 ancilla residue on {v:?}");

        // Timed sim on balanced net.
        let balanced_input = extend_with_ancilla(&data_bits, &ancillas);
        let sim_result = simulate(&balanced, &balanced_input)
            .unwrap_or_else(|e| panic!("timed sim failed on {v:?}: {e:?}"));
        let sim_data = &sim_result.outputs[..n_data_out];
        assert_eq!(
            sim_data,
            &expected_bits[..],
            "timed sim disagrees with core eval on {v:?}"
        );
    }
}

// ── Regression: 4-wire Toffoli-depth program stays within K2 + R1 ──

#[test]
fn toffoli_depth_still_lowers_within_k2_and_r1() {
    // The Toffoli (3-bit controlled-controlled-NOT) from the conformance
    // suite: since ADR-0040 phase C its conditional-NOTs are R1 cells
    // (promoted at most once), so nothing beyond K2 is ever needed.
    let src = include_str!("../../../conformance/programs/0006-toffoli.thse");
    let iso = elaborate(src, "tof");

    let (net, ancillas) = lower_iso_with_ancillas(&iso).expect("toffoli lowering succeeds");
    assert_eq!(net.validate(), Ok(()), "toffoli netlist validates");

    let counts = net.cell_counts();
    // The conditional-NOT structure shows up as R1 cells; no K3.
    assert!(
        counts.get(&CellKind::R1).copied().unwrap_or(0) > 0,
        "toffoli must use R1 cells; counts: {counts:?}"
    );
    assert_eq!(
        counts.get(&CellKind::K3).copied().unwrap_or(0),
        0,
        "toffoli must NOT use K3 cells; counts: {counts:?}"
    );

    // Differential oracle over all inputs.
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let n_data_out = rail_width(&out_ty).unwrap();
    for v in enumerate_values(&in_ty) {
        let data_bits = bits_of(&v, &in_ty).unwrap();
        let full_input = extend_with_ancilla(&data_bits, &ancillas);
        let got_full = eval_cells(&net, &full_input);
        let got = &got_full[..n_data_out];
        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        assert_eq!(
            got,
            &bits_of(&expected, &out_ty).unwrap()[..],
            "toffoli disagrees with core eval on {v:?}"
        );
    }
}
