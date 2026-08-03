//! ADR-0040 machine-checked witnesses: the controlled rail-role-swap
//! cell (`RoleSwapKind`) voids the K-vocabulary parity law.
//!
//! The law (`bennett.rs` module docs; premise machine-checked in
//! `ancilla_clean.rs::parity_obstruction_is_real`) rests on every cell
//! being a *value-permutation* of its rail pairs, which forces
//! `parity(data_out) ⊕ parity(anc_out)` to be constant up to a circuit
//! constant -- so a conditional-NOT (parity deviation = its AND) can
//! never be ancilla-clean. `Rk` conserves tokens (it is a controlled
//! exchange of the two conductors of ONE pair -- pure charge routing,
//! 1-hot preserved) but does NOT permute pair-values; these tests check
//! that the premise fails and that the forbidden conclusion --
//! CNOT/Toffoli with constant ancilla outputs -- is now constructively
//! achieved.

use theseus_netlist::{CellKind, RoleSwapKind};

const ALL_KINDS: [RoleSwapKind; 3] = [RoleSwapKind::R1, RoleSwapKind::R2, RoleSwapKind::R3];

fn bits_of_index(x: u32, n: usize) -> Vec<bool> {
    (0..n).map(|i| (x >> i) & 1 == 1).collect()
}

/// R1 *is* CNOT: `(c, t) → (c, c ⊕ t)`, with ZERO ancilla ports -- the
/// K-vocabulary needs 2 garbage pairs for the same function
/// (`ancilla_clean.rs::bennett_cnot_forward_ancillas_clean`).
#[test]
fn r1_truth_table_is_cnot_with_zero_ancillas() {
    for x in 0..4u32 {
        let bits = bits_of_index(x, 2);
        let (c, t) = (bits[0], bits[1]);
        assert_eq!(RoleSwapKind::R1.eval(&bits), vec![c, c ^ t]);
    }
    // Rk generally: (controls pass, t ⊕ AND(controls)).
    for kind in ALL_KINDS {
        let n = kind.arity();
        let k = kind.controls();
        for x in 0..1u32 << n {
            let bits = bits_of_index(x, n);
            let and = bits[..k].iter().all(|&c| c);
            let mut want = bits.clone();
            want[k] ^= and;
            assert_eq!(kind.eval(&bits), want, "{kind:?} on {bits:?}");
        }
    }
}

/// Self-inverse, like every admitted cell (ADR-0027 payoff 2 / C2.5).
#[test]
fn role_swap_is_self_inverse() {
    for kind in ALL_KINDS {
        let n = kind.arity();
        for x in 0..1u32 << n {
            let bits = bits_of_index(x, n);
            assert_eq!(kind.eval(&kind.eval(&bits)), bits);
        }
    }
}

/// The line-level definition (exchange the target pair's two token
/// lines iff all controls are 1) preserves INV-TOKEN -- each pair stays
/// 1-hot, total asserted-line count is invariant -- and decodes to
/// exactly the value-level `eval`. This is the admissibility half:
/// the cell is charge routing, nothing more.
#[test]
fn line_level_swap_conserves_tokens_and_matches_value_eval() {
    for kind in ALL_KINDS {
        let n = kind.arity();
        let k = kind.controls();
        for x in 0..1u32 << n {
            let vals = bits_of_index(x, n);
            // Encode: pair i is (line0, line1) = (¬v, v). 1-hot in.
            let pairs_in: Vec<(bool, bool)> = vals.iter().map(|&v| (!v, v)).collect();
            // Line level: controls pass; target lines exchange iff AND.
            let mut pairs_out = pairs_in.clone();
            if vals[..k].iter().all(|&c| c) {
                pairs_out[k] = (pairs_in[k].1, pairs_in[k].0);
            }
            // INV-TOKEN: every pair still 1-hot; asserted-line count
            // invariant (== n, one token per pair).
            for &(l0, l1) in &pairs_out {
                assert_ne!(l0, l1, "pair not 1-hot after {kind:?}");
            }
            let high_in: usize = pairs_in.iter().map(|&(a, b)| usize::from(a) + usize::from(b)).sum();
            let high_out: usize =
                pairs_out.iter().map(|&(a, b)| usize::from(a) + usize::from(b)).sum();
            assert_eq!(high_in, high_out, "token count changed");
            // Decode == value-level eval.
            let decoded: Vec<bool> = pairs_out.iter().map(|&(_, l1)| l1).collect();
            assert_eq!(decoded, kind.eval(&vals), "{kind:?} line/value mismatch on {vals:?}");
        }
    }
}

/// The parity law's premise (a) -- "every cell permutes its values" --
/// FAILS for Rk: on all-1 controls with t = 1 the output multiset
/// {1, ..., 1, 0} differs from the input {1, ..., 1}. This is the exact
/// negation of `parity_obstruction_is_real`'s machine-checked premise,
/// so the law's telescoping derivation does not extend to a vocabulary
/// containing Rk.
#[test]
fn role_swap_breaks_the_value_permutation_premise() {
    for kind in ALL_KINDS {
        let n = kind.arity();
        let all_ones = vec![true; n];
        let mut out = kind.eval(&all_ones);
        out.sort_unstable();
        let mut sorted_in = all_ones.clone();
        sorted_in.sort_unstable();
        assert_ne!(sorted_in, out, "{kind:?} unexpectedly permutes values");
        // And its parity change is data-dependent: 0 when a control is
        // 0, 1 when all controls are 1 -- the AND itself.
        for x in 0..1u32 << n {
            let bits = bits_of_index(x, n);
            let k = kind.controls();
            let p_in = bits.iter().filter(|&&b| b).count() % 2;
            let p_out = kind.eval(&bits).iter().filter(|&&b| b).count() % 2;
            let and = bits[..k].iter().all(|&c| c);
            assert_eq!(p_in != p_out, and, "{kind:?} parity deviation ≠ AND on {bits:?}");
        }
    }
}

/// The forbidden conclusion, achieved: a Toffoli built as
/// K1-compute → R1 → K1-uncompute has a CONSTANT ancilla output (its
/// init, 0) on every input, while its data parity deviation
/// (c₁ ∧ c₂) is non-constant -- exactly the combination the parity law
/// proves impossible in the K-only vocabulary. Garbage-free
/// conditional-NOT is real with Rk in the vocabulary.
#[test]
fn toffoli_via_k1_sandwich_is_ancilla_clean() {
    let mut deviations = std::collections::BTreeSet::new();
    for x in 0..8u32 {
        let bits = bits_of_index(x, 3);
        let (c1, c2, t) = (bits[0], bits[1], bits[2]);
        let anc = false; // one scratch pair, init 0

        // compute: K1(c1, c2, anc) → third port carries c1 ∧ c2.
        let f = CellKind::K1.eval(&[c1, c2, anc]);
        // use: R1(and, t) -- reads its control, never modifies it.
        let r = RoleSwapKind::R1.eval(&[f[2], t]);
        let t_out = r[1];
        // uncompute: replay the self-inverse K1 on its own outputs
        // (untouched by R1) -- restores (c1, c2, anc).
        let u = CellKind::K1.eval(&[f[0], f[1], r[0]]);

        assert_eq!((u[0], u[1]), (c1, c2), "controls regenerated");
        assert_eq!(t_out, t ^ (c1 && c2), "Toffoli truth table");
        // THE WITNESS: the ancilla output equals its init on EVERY
        // input -- constant, data-independent, ancilla-clean.
        assert_eq!(u[2], anc, "ancilla dirty on {bits:?}");

        let p_in = bits.iter().filter(|&&b| b).count() % 2;
        let p_out = [c1, c2, t_out].iter().filter(|&&b| b).count() % 2;
        deviations.insert(p_in != p_out);
    }
    // Non-constant parity deviation + constant ancillas: the parity
    // obstruction is void, not merely relocated.
    assert_eq!(deviations.len(), 2, "Toffoli's parity deviation is non-constant");
}
