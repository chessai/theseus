//! Acceptance test for K3 (3-control CSWAP) cell + K2→K3 promotion.
//!
//! The 5-wire reversible full adder has control-nesting depth 3, so its
//! lowering requires K3 cells. This test verifies the full compile
//! pipeline (elaborate → lower → balance → simulate) matches the core
//! evaluator over the whole 32-value domain.

use theseus_core::{enumerate_values, type_of, Iso, Type};
use theseus_netlist::{
    balance, bits_of, lower_iso, rail_width, simulate, CellKind, PhasedCellNetlist, RailPair,
};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

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

fn extend_with_ancilla(data_bits: &[bool], net: &PhasedCellNetlist) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    let n_ancilla = net.inputs.len() - data_bits.len();
    for i in 0..n_ancilla {
        bits.push(i % 2 == 0);
    }
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
    let mut src = String::from("type Bit = O | I\n\niso fa5 : (Bit, Bit, Bit, Bit, Bit) <-> (Bit, Bit, Bit, Bit, Bit)\n");

    for v in 0u32..32 {
        let a = (v >> 0) & 1 == 1;
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
            bit_str(a), bit_str(b), bit_str(cin), bit_str(s), bit_str(c)
        );
        let rhs = format!(
            "({}, {}, {}, {}, {})",
            bit_str(a), bit_str(b), bit_str(cin), bit_str(s_out), bit_str(c_out)
        );

        let sep = if v == 0 { "| " } else { "| " };
        src.push_str(&format!("{sep}{lhs} <-> {rhs}\n"));
    }
    src
}

// ── Acceptance test: 5-wire full adder ──────────────────────────────

#[test]
fn full_adder_5wire_lowers_with_k3() {
    let src = full_adder_source();
    let iso = elaborate(&src, "fa5");

    // Lower must succeed (no Deferred).
    let net = lower_iso(&iso).expect("fa5 lowering succeeds (K3 available)");
    assert_eq!(net.validate(), Ok(()), "lowered netlist validates");

    // Balance must succeed.
    let balanced = balance(&net).expect("balance succeeds");
    assert_eq!(balanced.validate(), Ok(()), "balanced netlist validates");
    assert!(
        balanced.tick_imbalances().unwrap().is_empty(),
        "balanced netlist has no tick imbalances"
    );

    // Assert at least one K3 cell exists.
    let counts = balanced.cell_counts();
    assert!(
        counts.get(&CellKind::K3).copied().unwrap_or(0) > 0,
        "netlist must contain at least one K3 cell; counts: {counts:?}"
    );
    eprintln!("fa5 cell counts: {counts:?}");

    // Differential oracle over all 32 inputs.
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let n_data_out = rail_width(&out_ty).unwrap();

    for v in enumerate_values(&in_ty) {
        let data_bits = bits_of(&v, &in_ty).unwrap();

        // Untimed eval on unbalanced net.
        let full_input = extend_with_ancilla(&data_bits, &net);
        let got_full = eval_cells(&net, &full_input);
        let got = &got_full[..n_data_out];

        let mut fuel = 10_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        let expected_bits = bits_of(&expected, &out_ty).unwrap();
        assert_eq!(
            got, &expected_bits[..],
            "untimed netlist disagrees with core eval on {v:?}"
        );

        // Timed sim on balanced net.
        let balanced_input = extend_with_ancilla(&data_bits, &balanced);
        let sim_result = simulate(&balanced, &balanced_input)
            .unwrap_or_else(|e| panic!("timed sim failed on {v:?}: {e:?}"));
        let sim_data = &sim_result.outputs[..n_data_out];
        assert_eq!(
            sim_data, &expected_bits[..],
            "timed sim disagrees with core eval on {v:?}"
        );
    }
}

// ── Regression: 4-wire Toffoli-depth program still lowers with max K2 ──

#[test]
fn toffoli_depth_still_lowers_max_k2() {
    // The Toffoli (3-bit controlled-controlled-NOT) from the conformance
    // suite lowers with K2 as the maximum cell kind (no K3 needed).
    let src = include_str!("../../../conformance/programs/0006-toffoli.thse");
    let iso = elaborate(src, "tof");

    let net = lower_iso(&iso).expect("toffoli lowering succeeds");
    assert_eq!(net.validate(), Ok(()), "toffoli netlist validates");

    let counts = net.cell_counts();
    // Must have K2 but NOT K3.
    assert!(
        counts.get(&CellKind::K2).copied().unwrap_or(0) > 0,
        "toffoli must use K2 cells"
    );
    assert_eq!(
        counts.get(&CellKind::K3).copied().unwrap_or(0),
        0,
        "toffoli must NOT use K3 cells"
    );

    // Differential oracle over all inputs.
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let n_data_out = rail_width(&out_ty).unwrap();
    for v in enumerate_values(&in_ty) {
        let data_bits = bits_of(&v, &in_ty).unwrap();
        let full_input = extend_with_ancilla(&data_bits, &net);
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
