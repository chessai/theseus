//! C1 ancilla-clean demonstration for the 1-of-N memory-exchange tree
//! (ADR-0046 §3.1, §4.3).
//!
//! The MEXCH instruction's decode includes a 1-of-N select tree over the
//! memory bank (the C1 tree). This test demonstrates whole-domain
//! ancilla-cleanliness at N ∈ {2, 4, 8} — the defining gate of Layer 1.
//!
//! Strategy: build a standalone iso with the SAME Fredkin-tree structure
//! as the decoder's select_mem, but using Bit-sized fields (not Word8)
//! so the domain is small enough for whole-domain enumeration:
//!   N=2: 4 bits → 16 inputs
//!   N=4: 7 bits → 128 inputs
//!   N=8: 12 bits → 4096 inputs
//!
//! This follows the ancilla_clean.rs / ADR-0040-phase-C ancilla-clean
//! test pattern: lower the iso, eval the netlist on every input, assert
//! every ancilla output equals its init.

use theseus_core::{type_of, Iso, Type};
use theseus_netlist::lower::{lower_iso_with_ancillas, prim_from_bits};
use theseus_netlist::{rail_width, CellKind, PhasedCellNetlist, RailPair};

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// Right-nested product of `n` Bits.
fn flat(n: usize) -> Type {
    assert!(n >= 1);
    if n == 1 {
        bit()
    } else {
        Type::prod(bit(), flat(n - 1))
    }
}

/// Untimed functional evaluation of the cell netlist.
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

/// Assert whole-domain ancilla-cleanliness AND data correctness of an iso.
/// Returns the ancilla count.
fn assert_ancilla_clean(name: &str, iso: &Iso, reference: impl Fn(&[bool]) -> Vec<bool>) -> usize {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (net, inits) = lower_iso_with_ancillas(iso).expect("lowering succeeds");
    let w_in = rail_width(&in_ty).unwrap();
    let w_out = rail_width(&out_ty).unwrap();
    let n_anc = inits.len();

    let domain_size = 1u64 << w_in;
    assert!(
        domain_size <= (1 << 16),
        "{name}: domain 2^{w_in} too large for whole-domain check"
    );

    for x in 0..domain_size {
        let mut input_bits: Vec<bool> = (0..w_in).map(|i| (x >> i) & 1 == 1).collect();
        let expected_data = reference(&input_bits);
        input_bits.extend_from_slice(&inits);
        let out = eval_cells(&net, &input_bits);

        // Check data correctness.
        assert_eq!(
            &out[..w_out], &expected_data[..],
            "{name}: data mismatch on input {x:#06x}"
        );

        // Check ancilla restoration.
        for (i, (got, expected)) in out[w_out..].iter().zip(&inits).enumerate() {
            assert_eq!(
                got, expected,
                "{name}: ancilla {i} dirty on input {x:#06x} \
                 (got {got}, expected {expected})"
            );
        }
    }

    eprintln!(
        "{name}: ancilla-clean on all {domain_size} inputs, {n_anc} ancilla lines"
    );
    n_anc
}

/// Build a 1-of-N addressed-exchange prim using Bit-sized fields.
///
/// Layout: (ctrl_bits, target_bit, m0, m1, ..., m_{n-1})
/// Semantics: swap target with m[addr] where addr = ctrl_bits as binary.
/// For addr >= n, identity (no swap).
///
/// This has the SAME combinatorial structure as the decoder's select_mem
/// crossbar, just with 1-bit fields instead of 8-bit Word8 fields.
fn build_mexch_prim(n: usize) -> Iso {
    let n_ctrl = match n {
        2 => 1,
        4 => 2,
        8 => 3,
        _ => panic!("unsupported N={n}"),
    };
    let total_bits = n_ctrl + 1 + n;
    let ty = flat(total_bits);

    let def = prim_from_bits(
        &format!("mexch_n{n}"),
        ty.clone(),
        ty,
        |bits| {
            let mut out = bits.to_vec();
            let mut addr = 0usize;
            for i in 0..n_ctrl {
                if bits[i] {
                    addr |= 1 << i;
                }
            }
            let tgt = n_ctrl;       // target bit index
            let mem = |k: usize| n_ctrl + 1 + k; // mem[k] bit index
            if addr < n {
                out.swap(tgt, mem(addr));
            }
            out
        },
    )
    .expect("prim creation");

    Iso::Prim(Box::new(def))
}

/// Reference function for the MEXCH prim.
fn mexch_reference(n: usize, n_ctrl: usize) -> impl Fn(&[bool]) -> Vec<bool> {
    move |bits: &[bool]| {
        let mut out = bits.to_vec();
        let mut addr = 0usize;
        for i in 0..n_ctrl {
            if bits[i] {
                addr |= 1 << i;
            }
        }
        let tgt = n_ctrl;
        let mem = |k: usize| n_ctrl + 1 + k;
        if addr < n {
            out.swap(tgt, mem(addr));
        }
        out
    }
}

// ══════════════════════════════════════════════════════════════════════
// C1 demonstration: whole-domain ancilla-clean at N ∈ {2, 4, 8}
// ══════════════════════════════════════════════════════════════════════

#[test]
fn c1_mexch_tree_ancilla_clean_n2() {
    // 1-of-2 tree: 1 ctrl + 1 target + 2 mem = 4 bits → 16 inputs.
    let iso = build_mexch_prim(2);
    let n_anc = assert_ancilla_clean("mexch_tree_n2", &iso, mexch_reference(2, 1));
    eprintln!("N=2: {n_anc} ancilla lines");
}

#[test]
fn c1_mexch_tree_ancilla_clean_n4() {
    // 1-of-4 tree: 2 ctrl + 1 target + 4 mem = 7 bits → 128 inputs.
    let iso = build_mexch_prim(4);
    let n_anc = assert_ancilla_clean("mexch_tree_n4", &iso, mexch_reference(4, 2));
    eprintln!("N=4: {n_anc} ancilla lines");
}

#[test]
fn c1_mexch_tree_ancilla_clean_n8() {
    // 1-of-8 tree: 3 ctrl + 1 target + 8 mem = 12 bits → 4096 inputs.
    let iso = build_mexch_prim(8);
    let n_anc = assert_ancilla_clean("mexch_tree_n8", &iso, mexch_reference(8, 3));
    eprintln!("N=8: {n_anc} ancilla lines");
}

// ══════════════════════════════════════════════════════════════════════
// Direct MEXCH instruction iso (instr.rs): zero ancillas
// ══════════════════════════════════════════════════════════════════════

#[test]
fn mexch_instr_iso_is_garbage_free() {
    // The instr.rs mexch(rd, addr) is a plain SwapProd lifted to State.
    // It should lower with zero ancillas (pure wire permutation).
    let iso = theseus_tisc::instr::mexch(0, 0);
    let (net, inits) = lower_iso_with_ancillas(&iso).expect("mexch lowers");
    assert_eq!(
        inits.len(), 0,
        "mexch instr iso should have zero ancillas (pure swap)"
    );
    assert_eq!(net.validate(), Ok(()));
    let counts = net.cell_counts();
    let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
    assert_eq!(max_kind, CellKind::K0, "pure swap should be K0-only");
}
