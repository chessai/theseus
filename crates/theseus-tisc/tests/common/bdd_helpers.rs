//! T2: ripple-carry BDD arithmetic shared across the BVEQ tests.
//!
//! Canonical bit-serial adders over `Vec<Bdd>` words (LSB-first), sharing the
//! single `BddVariableSet` passed in. These are the reference bit-arithmetic
//! specs the symbolic-equivalence proofs check the compiled datapath against.

#![allow(dead_code)]

use biodivine_lib_bdd::{Bdd, BddVariableSet};

/// Ripple-carry sum `a + b` (LSB-first, equal length), carry dropped at the
/// top. `vs` supplies the constant `false` seed carry.
pub fn ripple_sum(a: &[Bdd], b: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    assert_eq!(a.len(), b.len());
    let mut sums = Vec::with_capacity(a.len());
    let mut carry = vs.mk_false();
    for i in 0..a.len() {
        let axb = a[i].xor(&b[i]);
        let sum = axb.xor(&carry);
        let new_carry = a[i].and(&b[i]).or(&carry.and(&axb));
        sums.push(sum);
        carry = new_carry;
    }
    sums
}

/// `x + 1` via [`ripple_sum`] against a one-hot LSB word.
pub fn ripple_plus_one(x: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let mut one = vec![vs.mk_false(); x.len()];
    one[0] = vs.mk_true();
    ripple_sum(x, &one, vs)
}

/// Two's-complement subtraction `a - b = a + (~b) + 1`.
pub fn ripple_sub(a: &[Bdd], b: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let notb: Vec<Bdd> = b.iter().map(|x| x.not()).collect();
    ripple_plus_one(&ripple_sum(a, &notb, vs), vs)
}
