//! T1: BVEQ symbolic-state support shared across the `bveq_*` /
//! `branching_bveq` / `decoder_bveq` tests.
//!
//! These build and inspect the symbolic CpuState carrier (`SV`) under the
//! shared bit-interleaved variable map (`var_index = bit * n_fields + field`).
//! Only the helpers that are *byte-identical* across the tests that use them
//! live here; per-file variants (different signatures / dispatch) stay local.
//!
//! [`build_state_fields`] is the shared type-walk recursion **skeleton** only:
//! the per-field Word8 builder is passed in as a closure so each caller keeps
//! its own concretization strategy (pinned-zero / concrete-zero / pc-field).
//! Those strategies are intentionally different and are NOT unified here.

#![allow(dead_code)]

use biodivine_lib_bdd::{Bdd, BddVariableSet};
use theseus_core::Type;
use theseus_tisc::bveq_netlist::{extract_field, extract_word8_bdds, SV};
use theseus_tisc::types::{Layout, W};

// ── varmap (bit-interleaved: var_index = bit * n_fields + field) ────

/// Number of Word8 CpuState fields at `layout`.
pub fn n_fields(layout: &Layout) -> usize {
    layout.num_cpu_fields()
}

/// Total BDD variable count = fields * 8 bits.
pub fn n_vars(layout: &Layout) -> u16 {
    (n_fields(layout) * W) as u16
}

/// The BDD variable for `(field f, bit b)` under the interleaved map
/// (`var_index = b * nf + f`).
pub fn field_bit_var(vs: &BddVariableSet, nf: usize, f: usize, b: usize) -> Bdd {
    vs.mk_var(vs.variables()[b * nf + f])
}

/// The 8 bit-variables of field `f` (LSB-first).
pub fn field_bits(vs: &BddVariableSet, nf: usize, f: usize) -> Vec<Bdd> {
    (0..W).map(|b| field_bit_var(vs, nf, f, b)).collect()
}

// ── Word8 SV builders (right-nested, LSB-first) ─────────────────────

/// A constant `val` Word8 SV (right-nested 8 Bit leaves, LSB first).
pub fn const_word8(val: u8, vs: &BddVariableSet) -> SV {
    let mut sv = SV::Bit(if (val >> (W - 1)) & 1 == 1 {
        vs.mk_true()
    } else {
        vs.mk_false()
    });
    for b in (0..W - 1).rev() {
        let bit = if (val >> b) & 1 == 1 {
            vs.mk_true()
        } else {
            vs.mk_false()
        };
        sv = SV::pair(SV::Bit(bit), sv);
    }
    sv
}

/// A fully symbolic Word8 SV for field `f` (right-nested, LSB first).
pub fn symbolic_word8(vs: &BddVariableSet, nf: usize, f: usize) -> SV {
    let bits: Vec<SV> = (0..W)
        .map(|b| SV::Bit(field_bit_var(vs, nf, f, b)))
        .collect();
    let mut sv = bits[W - 1].clone();
    for b in (0..W - 1).rev() {
        sv = SV::pair(bits[b].clone(), sv);
    }
    sv
}

// ── Field extraction / concretization ───────────────────────────────

/// Extract field `f`'s 8 output BDDs to a `Vec` (LSB first).
pub fn out_field_bits(out: &SV, layout: &Layout, f: usize) -> Vec<Bdd> {
    let nf = n_fields(layout);
    extract_word8_bdds(extract_field(out, f, nf)).to_vec()
}

/// `(peak, sum)` BDD node counts over every Bit leaf of an SV.
pub fn sv_bdd_sizes(sv: &SV) -> (usize, usize) {
    fn walk(sv: &SV, peak: &mut usize, sum: &mut usize) {
        match sv {
            SV::Unit => {}
            SV::Bit(b) => {
                let s = b.size();
                if s > *peak {
                    *peak = s;
                }
                *sum += s;
            }
            SV::Pair(a, b) => {
                walk(a, peak, sum);
                walk(b, peak, sum);
            }
        }
    }
    let (mut peak, mut sum) = (0usize, 0usize);
    walk(sv, &mut peak, &mut sum);
    (peak, sum)
}

/// Count the Bit leaves (fields) of an SV.
pub fn sv_field_count(sv: &SV) -> usize {
    match sv {
        SV::Unit => 0,
        SV::Bit(_) => 1,
        SV::Pair(a, b) => sv_field_count(a) + sv_field_count(b),
    }
}

/// `true` iff `ty` is a Word8 (`flat(8)` = a right-nested product of 8 Bits).
pub fn is_word8(ty: &Type) -> bool {
    fn count_bits(ty: &Type) -> Option<usize> {
        match ty {
            Type::Sum(a, b) if matches!((a.as_ref(), b.as_ref()), (Type::One, Type::One)) => {
                Some(1)
            }
            Type::Prod(a, b) => Some(count_bits(a)? + count_bits(b)?),
            _ => None,
        }
    }
    count_bits(ty) == Some(W)
}

/// Read field `f` as a concrete `u32`, or `None` if any bit is symbolic.
pub fn read_concrete_field(out: &SV, layout: &Layout, f: usize) -> Option<u32> {
    let bits = out_field_bits(out, layout, f);
    let mut v = 0u32;
    for (b, bit) in bits.iter().enumerate() {
        if bit.is_true() {
            v |= 1 << b;
        } else if bit.is_false() {
        } else {
            return None; // symbolic bit ⇒ not concrete
        }
    }
    Some(v)
}

/// Read the concrete PC (lo byte, plus hi byte if a 2-field PC) from an
/// output SV.
pub fn read_concrete_pc(out: &SV, layout: &Layout) -> Option<usize> {
    let lo = read_concrete_field(out, layout, layout.pc_field())? as usize;
    if layout.pc_fields() >= 2 {
        let hi = read_concrete_field(out, layout, layout.pc_hi_field())? as usize;
        Some((hi << 8) | lo)
    } else {
        Some(lo)
    }
}

// ── State-fields type-walk skeleton (SHARED RECURSION ONLY) ─────────

/// Walk the right-nested CpuState product `field0 x (field1 x (... x fieldN))`,
/// emitting one Word8 SV per field via the caller-supplied `field_word8`
/// builder. `field_word8(vs, nf, f)` produces the SV for field `f` -- the
/// concretization policy (pinned-zero / concrete-zero / pc-field) lives in the
/// closure, NOT here. `field` is the running field index.
///
/// This is the shared recursion skeleton across the `build_state`-style
/// builders; the per-field dispatch is intentionally left to each caller.
pub fn build_state_fields(
    ty: &Type,
    vs: &BddVariableSet,
    nf: usize,
    field: &mut usize,
    field_word8: &dyn Fn(&BddVariableSet, usize, usize) -> SV,
) -> SV {
    match ty {
        // Last field: a bare Word8 (`flat(8)`), no trailing product.
        _ if is_word8(ty) => {
            let sv = field_word8(vs, nf, *field);
            *field += 1;
            sv
        }
        Type::Prod(a, rest) if is_word8(a) => {
            let head = field_word8(vs, nf, *field);
            *field += 1;
            let tail = build_state_fields(rest, vs, nf, field, field_word8);
            SV::pair(head, tail)
        }
        other => panic!("CpuState field not a Word8-headed product: {other:?}"),
    }
}
