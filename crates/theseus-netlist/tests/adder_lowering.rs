//! Differential-oracle test for the compositional reversible half-adder
//! (`conformance/programs/0024-adder.thse`).
//!
//! Proves the full surface→cells chain: the half-adder composed from prelude
//! cnot + toffoli gates parses, checks, elaborates (via the implicit prelude),
//! and lowers to a cell netlist whose untimed evaluation agrees with the
//! interpreter over the ENTIRE finite domain (2^4 = 16 inputs).

use theseus_check::prelude::with_prelude;
use theseus_core::{enumerate_values, type_of, Iso};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{bits_of, rail_width, PhasedCellNetlist, RailPair};

const ADDER_SRC: &str = include_str!("../../../conformance/programs/0024-adder.thse");

// ── Untimed cell-netlist evaluation (dual-rail, 1-of-2 per pair) ────

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

// ── Surface → core Iso (with implicit prelude) ─────────────────────

fn elaborate_with_prelude(src: &str, iso_name: &str) -> Iso {
    let program = theseus_syntax::parse(src).expect("parse");
    let merged = with_prelude(&program);
    let diags = theseus_check::check(&merged);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = theseus_check::env::resolve(&merged);
    let isos = theseus_check::elaborate::elaborate_program(&merged, &env);
    isos.get(iso_name)
        .unwrap_or_else(|| panic!("iso {iso_name:?} not found in elaboration"))
        .clone()
        .unwrap_or_else(|e| panic!("elaboration of {iso_name:?} failed: {e:?}"))
}

// ── Differential oracle over the whole finite domain ────────────────

fn assert_lowering_agrees(iso: &Iso) -> PhasedCellNetlist {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (net, ancillas) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    assert_eq!(net.validate(), Ok(()), "lowered netlist validates");
    let n_data_out = rail_width(&out_ty).unwrap();
    let mut checked = 0usize;
    for v in enumerate_values(&in_ty) {
        let mut full_input = bits_of(&v, &in_ty).unwrap();
        full_input.extend_from_slice(&ancillas);
        let got_full = eval_cells(&net, &full_input);
        let got = &got_full[..n_data_out];

        let mut fuel = 100_000u64;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
        assert_eq!(
            got,
            &bits_of(&expected, &out_ty).unwrap()[..],
            "lowered cells disagree with core eval on {v:?}"
        );
        checked += 1;
    }
    assert!(checked > 0, "domain was empty");
    eprintln!(
        "ha4: lowered cells agree with eval on all {checked} inputs; \
         {} cells, {} ancilla lines",
        net.cells.len(),
        ancillas.len()
    );
    net
}

#[test]
fn surface_ha4_lowers_and_agrees_with_eval() {
    let iso = elaborate_with_prelude(ADDER_SRC, "ha4");
    assert_lowering_agrees(&iso);
}
