//! Differential-oracle test for the surface Fibonacci program
//! (`conformance/programs/0022-fib.thse`).
//!
//! Proves the full surface→cells chain: the hand-written reversible
//! Fibonacci (a `swap ; cnot` step wrapped in `iter Nat`) parses, checks,
//! elaborates to a core Iso, and lowers to a k-CSWAP/R1 cell netlist
//! whose untimed evaluation agrees with the interpreter over the ENTIRE
//! finite domain (Nat × Bit × Bit at unroll depth D = 4). It also pins
//! the actual Fibonacci-mod-2 sequence the compiled loop produces.

use theseus_core::{enumerate_values, type_of, Iso, Value};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{bits_of, rail_width, PhasedCellNetlist, RailPair};

const FIB_SRC: &str = include_str!("../../../conformance/programs/0022-fib.thse");

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

// ── Surface → core Iso ──────────────────────────────────────────────

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
        "fib: lowered cells agree with eval on all {checked} inputs \
         (Nat × Bit × Bit @ D=4); {} cells, {} ancilla lines",
        net.cells.len(),
        ancillas.len()
    );
    net
}

#[test]
fn surface_fib_lowers_and_agrees_with_eval() {
    let iso = elaborate(FIB_SRC, "fib");
    assert_lowering_agrees(&iso);
}

// ── Pin the compiled Fibonacci-mod-2 sequence ───────────────────────

/// Build the Nat numeral for `k` (Zero / Succ ... at depth D = 4: k ≤ 3).
fn nat(k: u32) -> Value {
    // Nat = Zero | Succ Nat  ⇒  Sum(One, Nat); Zero = Left(unit),
    // Succ n = Right(n).
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

fn bit(b: bool) -> Value {
    if b {
        Value::right(Value::Unit)
    } else {
        Value::left(Value::Unit)
    }
}

#[test]
fn compiled_fib_matches_reference_mod2() {
    let iso = elaborate(FIB_SRC, "fib");
    // Reference: (f_k, f_{k+1}) mod 2 with f_0 = f_1 = 1.
    let mut a = 1u8;
    let mut b = 1u8;
    for k in 0..4u32 {
        // Input ⟨k, (1, 1)⟩; the count is preserved, the pair advances k steps.
        let input = Value::pair(nat(k), Value::pair(bit(true), bit(true)));
        let mut fuel = 100_000u64;
        let out = theseus_eval::eval(&iso, input, &mut fuel).expect("eval");
        // Decode ⟨k, (x, y)⟩.
        let Value::Pair(kout, pair) = out else {
            panic!("expected outer pair, got {out:?}");
        };
        assert_eq!(*kout, nat(k), "iter must preserve the count k");
        let Value::Pair(x, y) = *pair else {
            panic!("expected inner pair");
        };
        assert_eq!(*x, bit(a % 2 == 1), "fib_{k} low bit");
        assert_eq!(*y, bit(b % 2 == 1), "fib_{} low bit", k + 1);
        let next = (a + b) % 2;
        a = b;
        b = next;
    }
    eprintln!("compiled surface fib matches (f_k, f_k+1) mod 2 for k = 0..3");
}
