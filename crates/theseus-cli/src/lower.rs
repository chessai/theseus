//! One-call surface → cell-netlist lowering + differential-oracle self-check.

use std::fmt;

use theseus_check::diagnostics::Diagnostic;
use theseus_core::{enumerate_values, type_of, Iso};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{bits_of, rail_width, PhasedCellNetlist, RailPair};
use theseus_syntax::parser::ParseError;

use crate::pipeline;

/// A successfully lowered iso: the core Iso, phased cell netlist, and ancilla
/// init values.
pub struct LoweredIso {
    pub iso: Iso,
    pub net: PhasedCellNetlist,
    pub ancillas: Vec<bool>,
}

/// Errors from the surface → cells pipeline.
#[derive(Debug)]
pub enum LowerError {
    Parse(ParseError),
    Check(Vec<Diagnostic>),
    UnknownIso(String),
    ElabFailed(String),
    Lower(theseus_netlist::LowerError),
}

impl fmt::Display for LowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LowerError::Parse(e) => write!(f, "parse error: {e:?}"),
            LowerError::Check(diags) => {
                write!(f, "check errors:")?;
                for d in diags {
                    write!(f, "\n  {d:?}")?;
                }
                Ok(())
            }
            LowerError::UnknownIso(name) => write!(f, "unknown iso: {name}"),
            LowerError::ElabFailed(name) => {
                write!(f, "elaboration of iso `{name}` failed")
            }
            LowerError::Lower(e) => write!(f, "lowering error: {e:?}"),
        }
    }
}

/// Parse, check, elaborate, and lower a named iso from surface source to a
/// phased cell netlist in one call.
pub fn surface_to_cells(src: &str, iso_name: &str) -> Result<LoweredIso, LowerError> {
    let ep = pipeline::elaborate(src).map_err(|e| match e {
        pipeline::ElabError::Parse(e) => LowerError::Parse(e),
        pipeline::ElabError::Check(diags) => LowerError::Check(diags),
    })?;

    let iso = match ep.isos.get(iso_name) {
        Some(Ok(iso)) => iso.clone(),
        Some(Err(_)) => return Err(LowerError::ElabFailed(iso_name.to_string())),
        None => return Err(LowerError::UnknownIso(iso_name.to_string())),
    };

    let (net, ancillas) = lower_iso_with_ancillas(&iso).map_err(LowerError::Lower)?;

    Ok(LoweredIso { iso, net, ancillas })
}

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

/// Differential oracle: evaluate the lowered netlist over every value in
/// the input domain and compare to `theseus_eval::eval`. Returns the
/// count of inputs checked on success, or a mismatch description on
/// failure. Only meaningful for small domains.
pub fn oracle_check(iso: &Iso, lowered: &LoweredIso) -> Result<usize, String> {
    let (in_ty, out_ty) = type_of(iso).map_err(|e| format!("type error: {e:?}"))?;
    let n_data_out = rail_width(&out_ty)
        .map_err(|e| format!("unsupported output type for rail encoding: {e:?}"))?;

    let mut checked = 0usize;
    for v in enumerate_values(&in_ty) {
        let mut full_input =
            bits_of(&v, &in_ty).map_err(|e| format!("cannot encode {v:?} as bits: {e:?}"))?;
        full_input.extend_from_slice(&lowered.ancillas);

        let got_full = eval_cells(&lowered.net, &full_input);
        let got = &got_full[..n_data_out];

        let mut fuel = 100_000u64;
        let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
            .map_err(|e| format!("core eval failed on {v:?}: {e:?}"))?;
        let expected_bits = bits_of(&expected, &out_ty)
            .map_err(|e| format!("cannot encode expected {expected:?} as bits: {e:?}"))?;

        if got != &expected_bits[..] {
            return Err(format!(
                "cells disagree with eval on {v:?}: got {got:?}, expected {expected_bits:?}"
            ));
        }
        checked += 1;
    }

    if checked == 0 {
        return Err("domain was empty".to_string());
    }

    Ok(checked)
}

/// Print a summary of a lowered netlist (for CLI output).
pub fn print_summary(lowered: &LoweredIso) {
    let net = &lowered.net;
    let counts = net.cell_counts();
    let total: usize = counts.values().sum();
    println!("cells: {total} total");
    for (kind, count) in &counts {
        println!("  {kind:?}: {count}");
    }
    println!("lines: {}", net.num_lines);
    println!("inputs: {} rail pairs", net.inputs.len());
    println!("outputs: {} rail pairs", net.outputs.len());
    println!("ancillas: {}", lowered.ancillas.len());
}
