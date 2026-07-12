//! Reversible Fibonacci through the full backend, oracle-verified.
//!
//! The program generator (adders, routing isos, register layout) lives in
//! `tests/fib_gen/mod.rs` and is shared with the `fib_energy` example.
//!
//! Oracle: for every enumerated input value v,
//!   eval_cells(lower(iso), bits_of(v))  ==  bits_of(theseus_eval::eval(iso, v)).

mod fib_gen;

use fib_gen::{build_fib5_minwidth, build_fib_program, FibProgram};

use theseus_core::prim::Value;
use theseus_core::{enumerate_values, type_of, Iso};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{
    balance, bits_of, rail_width, simulate, CellKind, PhasedCellNetlist, RailPair,
};

// ═══════════════════════════════════════════════════════════════════════
// Pipeline + oracle helpers
// ═══════════════════════════════════════════════════════════════════════

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

/// Untimed functional evaluation of the cell netlist with INV-TOKEN checks.
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

/// Semantic wire values (false = O, true = I) → core Value of the n-Bit tuple.
fn value_of_wires(vals: &[bool]) -> Value {
    let mk = |b: bool| {
        if b {
            Value::right(Value::Unit) // I
        } else {
            Value::left(Value::Unit) // O
        }
    };
    let mut it = vals.iter().rev();
    let mut acc = mk(*it.next().expect("nonempty"));
    for &b in it {
        acc = Value::pair(mk(b), acc);
    }
    acc
}

/// Rail tag bit → semantic wire value. bits_of maps O=Left→true, I=Right→false.
fn wire_of_bit(tag: bool) -> bool {
    !tag
}

/// Assert netlist (untimed) agrees with theseus_eval on value `v`; returns
/// the netlist's semantic output wires.
fn oracle_one(
    iso: &Iso,
    net: &PhasedCellNetlist,
    ancilla_values: &[bool],
    in_ty: &theseus_core::Type,
    out_ty: &theseus_core::Type,
    v: &Value,
) -> Vec<bool> {
    let data_bits = bits_of(v, in_ty).unwrap();
    let mut full_input = data_bits.clone();
    full_input.extend_from_slice(ancilla_values);
    let got_full = eval_cells(net, &full_input);
    let n_data_out = rail_width(out_ty).unwrap();
    let got = &got_full[..n_data_out];
    let mut fuel = 1_000_000u64;
    let expected = theseus_eval::eval(iso, v.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("core eval failed on {v:?}: {e:?}"));
    let expected_bits = bits_of(&expected, out_ty).unwrap();
    assert_eq!(
        got,
        &expected_bits[..],
        "netlist disagrees with core eval on {v:?}"
    );
    got.iter().map(|&b| wire_of_bit(b)).collect()
}

/// Compile a fib program end to end; returns (iso, unbalanced net, ancillas).
fn compile(prog: &FibProgram) -> (Iso, PhasedCellNetlist, Vec<bool>) {
    let iso = elaborate(&prog.source, &prog.top_name);
    let (net, ancilla_values) = lower_iso_with_ancillas(&iso).expect("lowering succeeds");
    assert_eq!(net.validate(), Ok(()), "lowered netlist validates");
    // K3 is the largest available cell.
    let counts = net.cell_counts();
    assert!(
        !counts.keys().any(|k| *k > CellKind::K3),
        "cell kind beyond K3: {counts:?}"
    );
    (iso, net, ancilla_values)
}

/// Read register value from semantic output wires given the final layout.
fn read_reg(wires: &[bool], layout: &[String], names: &[String]) -> u64 {
    let mut v = 0u64;
    for (i, name) in names.iter().enumerate() {
        let p = layout.iter().position(|x| x == name).unwrap();
        if wires[p] {
            v |= 1 << i;
        }
    }
    v
}

// ═══════════════════════════════════════════════════════════════════════
// (a) 2-bit compositional ripple adder — exhaustive oracle (256 inputs)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn two_bit_ripple_adder_exhaustive() {
    let prog = build_fib_program(2, 1, "add2");
    eprintln!("── generated source ──\n{}", prog.source);
    let (iso, net, ancilla_values) = compile(&prog);
    let counts = net.cell_counts();
    eprintln!("add2 cell counts: {counts:?}  (lines={})", net.num_lines);

    let (in_ty, out_ty) = type_of(&iso).expect("iso types");

    // Full-domain differential oracle.
    let values = enumerate_values(&in_ty);
    assert_eq!(values.len(), 256);
    for v in &values {
        oracle_one(&iso, &net, &ancilla_values, &in_ty, &out_ty, v);
    }
    eprintln!("add2 oracle: {}/{} inputs match", values.len(), values.len());

    // Ancilla-zero slice: adder really computes x + y.
    let w = prog.width;
    let n = prog.initial_layout.len();
    let sum_reg: Vec<String> = (0..w).map(|i| format!("s1_{i}")).collect();
    let carry_out = format!("c1_{}", w - 1);
    for x in 0u64..4 {
        for y in 0u64..4 {
            let mut wires = vec![false; n];
            for i in 0..w {
                let px = prog
                    .initial_layout
                    .iter()
                    .position(|s| s == &format!("x{i}"))
                    .unwrap();
                let py = prog
                    .initial_layout
                    .iter()
                    .position(|s| s == &format!("y{i}"))
                    .unwrap();
                wires[px] = (x >> i) & 1 == 1;
                wires[py] = (y >> i) & 1 == 1;
            }
            let v = value_of_wires(&wires);
            let out = oracle_one(&iso, &net, &ancilla_values, &in_ty, &out_ty, &v);
            let s = read_reg(&out, &prog.final_layout, &sum_reg);
            let cout = read_reg(&out, &prog.final_layout, std::slice::from_ref(&carry_out));
            assert_eq!(
                s + 4 * cout,
                x + y,
                "adder wrong on x={x} y={y}: s={s} cout={cout}"
            );
            // inputs preserved
            let xr: Vec<String> = (0..w).map(|i| format!("x{i}")).collect();
            let yr: Vec<String> = (0..w).map(|i| format!("y{i}")).collect();
            assert_eq!(read_reg(&out, &prog.final_layout, &xr), x);
            assert_eq!(read_reg(&out, &prog.final_layout, &yr), y);
        }
    }

    // Timed simulation on the balanced netlist for a sample subset.
    let balanced = balance(&net).expect("balance succeeds");
    assert_eq!(balanced.validate(), Ok(()));
    assert!(balanced.tick_imbalances().unwrap().is_empty());
    assert_eq!(
        balanced.inputs.len(),
        net.inputs.len(),
        "balance must not change inputs"
    );
    let n_data_out = rail_width(&out_ty).unwrap();
    for v in values.iter().step_by(17) {
        let data_bits = bits_of(v, &in_ty).unwrap();
        let mut full = data_bits.clone();
        full.extend_from_slice(&ancilla_values);
        let sim = simulate(&balanced, &full).expect("timed sim");
        let mut fuel = 1_000_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(
            &sim.outputs[..n_data_out],
            &bits_of(&expected, &out_ty).unwrap()[..],
            "timed sim disagrees on {v:?}"
        );
    }
}

// ═══════════════════════════════════════════════════════════════════════
// (b) Fibonacci, 2-bit registers, 2 unrolled adds — exhaustive (4096)
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn fib_2bit_2adds_exhaustive_oracle() {
    let prog = build_fib_program(2, 2, "fib");
    let (iso, net, ancilla_values) = compile(&prog);
    let counts = net.cell_counts();
    eprintln!(
        "fib(2-bit, 2 adds) cell counts: {counts:?} (cells={}, lines={})",
        net.cells.len(),
        net.num_lines
    );

    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    let values = enumerate_values(&in_ty);
    assert_eq!(values.len(), 4096);
    for v in &values {
        oracle_one(&iso, &net, &ancilla_values, &in_ty, &out_ty, v);
    }
    eprintln!("fib oracle: {}/{} inputs match", values.len(), values.len());

    // Fibonacci semantics on the zero-ancilla slice: x=fib1=1, y=fib2=1.
    let n = prog.initial_layout.len();
    let mut wires = vec![false; n];
    let set = |wires: &mut Vec<bool>, name: &str| {
        let p = prog.initial_layout.iter().position(|s| s == name).unwrap();
        wires[p] = true;
    };
    set(&mut wires, "x0"); // x = 1
    set(&mut wires, "y0"); // y = 1
    let v = value_of_wires(&wires);
    let out = oracle_one(&iso, &net, &ancilla_values, &in_ty, &out_ty, &v);

    let reg = |j: usize| -> Vec<String> { (0..prog.width).map(|i| format!("s{j}_{i}")).collect() };
    let r3 = read_reg(&out, &prog.final_layout, &reg(1));
    let r4 = read_reg(&out, &prog.final_layout, &reg(2));
    let c1 = read_reg(
        &out,
        &prog.final_layout,
        std::slice::from_ref(&format!("c1_{}", prog.width - 1)),
    );
    let c2 = read_reg(
        &out,
        &prog.final_layout,
        std::slice::from_ref(&format!("c2_{}", prog.width - 1)),
    );
    eprintln!("fib registers: r3={r3} (carry {c1}), r4={r4} (carry {c2})");
    assert_eq!(r3, 2, "fib(3) = 2");
    assert_eq!(r4, 3, "fib(4) = 3");
    assert_eq!(c1, 0);
    assert_eq!(c2, 0);

    // Timed simulation spot checks on the balanced netlist.
    let balanced = balance(&net).expect("balance succeeds");
    assert_eq!(balanced.validate(), Ok(()));
    assert!(balanced.tick_imbalances().unwrap().is_empty());
    let n_data_out = rail_width(&out_ty).unwrap();
    for v in values.iter().step_by(311) {
        let data_bits = bits_of(v, &in_ty).unwrap();
        let mut full = data_bits.clone();
        full.extend_from_slice(&ancilla_values);
        let sim = simulate(&balanced, &full).expect("timed sim");
        let mut fuel = 1_000_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(
            &sim.outputs[..n_data_out],
            &bits_of(&expected, &out_ty).unwrap()[..],
            "timed sim disagrees on {v:?}"
        );
    }
}

// ═══════════════════════════════════════════════════════════════════════
// (c) Minimal-width Fibonacci to fib(5)=5 — 14 wires, EXHAUSTIVE (16384)
//
// A uniform 3-bit / 3-add machine needs 24 wires, which is intractable:
// not for lowering, but because theseus_check's coverage pass enumerates
// the full value domain of every CLAUSE-defined iso, and the routing isos
// (asc0 : 24-wire reassoc, idr22 : 22-wire identity) then require 2^24 /
// 2^22 enumerated values (measured: 0.1s @ 14 wires, 0.5s @ 16, 2.4s @ 18,
// 9.8s @ 20 — ~4x per +2 wires; at 24 the test blew the stack after 60s).
// Sizing each register to its actual fib value (1,1,2,3,4 bits) keeps the
// machine at 14 wires and makes the FULL domain enumerable.
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn fib5_minwidth_exhaustive_oracle() {
    let prog = build_fib5_minwidth("fib5");
    eprintln!("── generated source ──\n{}", prog.source);
    let (iso, net, ancilla_values) = compile(&prog);
    let counts = net.cell_counts();
    eprintln!(
        "fib5 cell counts: {counts:?} (cells={}, lines={})",
        net.cells.len(),
        net.num_lines
    );

    let (in_ty, out_ty) = type_of(&iso).expect("iso types");

    // Full-domain differential oracle: all 2^14 inputs.
    let values = enumerate_values(&in_ty);
    assert_eq!(values.len(), 16384);
    for v in &values {
        oracle_one(&iso, &net, &ancilla_values, &in_ty, &out_ty, v);
    }
    eprintln!("fib5 oracle: {}/{} inputs match", values.len(), values.len());

    // Fibonacci semantics on the zero-ancilla slice: x=1, y=1.
    let n = prog.initial_layout.len();
    let pos = |name: &str| prog.initial_layout.iter().position(|s| s == name).unwrap();
    let mut wires = vec![false; n];
    wires[pos("x0")] = true; // r1 = fib(1) = 1
    wires[pos("y0")] = true; // r2 = fib(2) = 1
    let v = value_of_wires(&wires);
    let out = oracle_one(&iso, &net, &ancilla_values, &in_ty, &out_ty, &v);

    let vals: Vec<u64> = (0..5)
        .map(|j| read_reg(&out, &prog.final_layout, &prog.regs[j]))
        .collect();
    eprintln!(
        "fib5 registers: r1={} r2={} r3={} r4={} r5={}",
        vals[0], vals[1], vals[2], vals[3], vals[4]
    );
    assert_eq!(vals, vec![1, 1, 2, 3, 5], "registers hold fib(1..=5)");

    // Timed simulation on the balanced netlist: every 33rd input + fib input.
    let balanced = balance(&net).expect("balance succeeds");
    assert_eq!(balanced.validate(), Ok(()));
    assert!(balanced.tick_imbalances().unwrap().is_empty());
    let n_data_out = rail_width(&out_ty).unwrap();
    let mut timed = 0usize;
    for v in values.iter().step_by(33).chain(std::iter::once(&v)) {
        let data_bits = bits_of(v, &in_ty).unwrap();
        let mut full = data_bits.clone();
        full.extend_from_slice(&ancilla_values);
        let sim = simulate(&balanced, &full).expect("timed sim");
        let mut fuel = 1_000_000u64;
        let expected = theseus_eval::eval(&iso, v.clone(), &mut fuel).unwrap();
        assert_eq!(
            &sim.outputs[..n_data_out],
            &bits_of(&expected, &out_ty).unwrap()[..],
            "timed sim disagrees on {v:?}"
        );
        timed += 1;
    }
    eprintln!("fib5 timed sim: {timed} inputs match on the balanced netlist");
}

// ═══════════════════════════════════════════════════════════════════════
// Regression: 1-bit 2-add chain. This is the minimal reproduction of the
// lowerer depth-bookkeeping bug (rail-swapped pair missed in
// controlled_apply's pair_depth_map fix-up → CellDepth { expected: 8,
// got: 6 }) that the fix in lower.rs resolves.
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn mini_1bit_2adds_lowers_and_oracle_matches() {
    let prog = build_fib_program(1, 2, "mini");
    let (iso, net, ancilla_values) = compile(&prog);
    let (in_ty, out_ty) = type_of(&iso).expect("iso types");
    for v in enumerate_values(&in_ty) {
        oracle_one(&iso, &net, &ancilla_values, &in_ty, &out_ty, &v);
    }
}
