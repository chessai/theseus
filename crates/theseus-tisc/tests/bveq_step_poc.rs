//! BVEQ-STEP-POC -- prove the REAL compiler-emitted PC-update netlist correct
//! SYMBOLICALLY (whole-domain, canonical BDD), replacing a brute-force
//! whole-domain SIMULATION sweep with a domain-size-independent BDD proof.
//!
//! ─────────────────────────────────────────────────────────────────────────
//! THE CLAIM (with numbers)
//!
//! The heavy suite establishes the ADR-0034 PC-update identity
//! `PC' = (PC + sext_{w_pc}(BR) + 1) mod 2^{w_pc}` by EVALUATING the compiled
//! iso `pc_add_br_with(&layout) ; pc_inc_with(&layout)` on EVERY (PC, BR) pair:
//!   - `adr0034_split_add_exhaustive_wpc8` (b22b_wpc8_run.rs) walks all 2^16
//!     (PC, BR) pairs -- ~387 s in the last heavy run.
//!   - The w_pc=16 analogue is 2^24 (~256× the domain → hours).
//!
//! This file proves the SAME property, over the WHOLE domain, SYMBOLICALLY in
//! ~seconds via the repo's existing BDD machinery (`bveq_netlist::eval_bdd` --
//! the symbolic Iso interpreter that also underpins `wide_add.rs`). The BDD
//! proof time is TINY and ~FLAT across w_pc=8 (2^16 domain) and w_pc=16 (2^24
//! domain), because a canonical BDD comparison is independent of domain size.
//!
//! ─────────────────────────────────────────────────────────────────────────
//! WHAT IS PROVEN (honest coverage)
//!
//! `eval_bdd` handles the COMBINATIONAL (loop-free) fragment -- Seq/Times/Swap/
//! Assoc/Unite/Prim gates -- and REJECTS Trace/Iter/Fold (loops). The PC-update
//! iso `pc_add_br_with ; pc_inc_with` is entirely combinational (a fixed cascade
//! of CNOT/Toffoli gates), so it is fully within `eval_bdd`'s fragment. This
//! proves the ONE-STEP PC-update netlist ≡ its spec over the WHOLE state domain.
//! Multi-step program correctness (the b21c composite-ring sweeps, the real-CPU
//! authority runs) then follows by induction over steps -- proving each step's
//! netlist ≡ step spec ONCE symbolically eliminates per-input, per-step
//! simulation. (Full multi-step step-phase coverage needs eval_bdd extended past
//! the pc-update fragment; the pc-update iso here needs no such extension.)

use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::{Iso, Type};

use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::compile::pc_update;
use theseus_tisc::step::{pc_add_br_with, pc_inc_with};
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH, W};

// ══════════════════════════════════════════════════════════════════════════
// Symbolic CpuState construction
//
// The iso operates on the FULL CpuState carrier `state_ty_with(&layout)` -- a
// right-nested product of `num_cpu_fields()` Word8 fields (every field is 8-bit
// at both w_pc=8 and w_pc=16, since w_ctl==w_data==8). We build an ALL-SYMBOLIC
// state SV (every field gets fresh BDD variables), which proves MORE than the
// b22b sweep: it holds regs/IR/scratch symbolic (arbitrary), not const-0.
//
// Variable ordering -- BIT-INTERLEAVED (`var = bit*num_fields + field`), the same
// interleaving `bveq::VarMap` uses and `wide_add.rs` relies on to keep
// ripple-carry LINEAR in the BDD. Sequential ordering blows the adder BDD up
// exponentially; interleaved keeps PC bit i and BR bit i adjacent so the carry
// chain stays a linear cascade.
// ══════════════════════════════════════════════════════════════════════════

/// Number of CPU fields for a layout (all 8-bit wide here).
fn n_fields(layout: &Layout) -> usize {
    layout.num_cpu_fields()
}

/// Total BDD variables for the symbolic state = num_fields * 8.
fn n_vars(layout: &Layout) -> u16 {
    (n_fields(layout) * W) as u16
}

/// The bit-interleaved BDD variable for CpuState field `f`, bit `b`.
/// Ordering: `var_index = b * num_fields + f` (VarMap convention).
fn field_bit_var(vs: &BddVariableSet, nf: usize, f: usize, b: usize) -> Bdd {
    vs.mk_var(vs.variables()[b * nf + f])
}

/// The 8 input-bit BDDs of field `f` (LSB first).
fn field_bits(vs: &BddVariableSet, nf: usize, f: usize) -> Vec<Bdd> {
    (0..W).map(|b| field_bit_var(vs, nf, f, b)).collect()
}

/// Build the CpuState input SV mirroring `state_ty_with(&layout)`.
///
/// Every field is SYMBOLIC (fresh interleaved BDD vars) EXCEPT s0 and s1, which
/// are pinned to CONSTANT 0 -- the decoder scratch convention the PC adder
/// REQUIRES and RESTORES (b22b pins s0=s1=0). The adder materializes its operand
/// INTO s0/s1 via CNOT/NOT, so a nonzero (symbolic) s0/s1 on entry would xor
/// into the operand and corrupt the sum. Pinning s0/s1=0 matches b22b exactly;
/// PC, BR, regs, IR, and s2..s7 stay symbolic (proves MORE than b22b, which held
/// regs/IR const).
fn symbolic_state_sv(layout: &Layout, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    let ty = state_ty_with(layout);
    let mut field = 0usize;
    build_symbolic(&ty, vs, nf, layout, &mut field)
}

/// Fields pinned to constant 0 (the decoder scratch operand region).
fn is_pinned_zero(layout: &Layout, f: usize) -> bool {
    f == layout.scratch_field(0) || f == layout.scratch_field(1)
}

/// Recursively build a symbolic SV from a CpuState type, consuming one 8-bit
/// field per Word8 leaf (LSB-first, bit-interleaved variables).
fn build_symbolic(
    ty: &Type,
    vs: &BddVariableSet,
    nf: usize,
    layout: &Layout,
    field: &mut usize,
) -> SV {
    // A Word8 is `flat(8)` = a right-nested product of 8 Bits. We detect a full
    // Word8 by counting Bit leaves; but simplest is: the CpuState is a
    // right-nested product of Word8 fields, so recurse on Prod and, at each
    // maximal 8-bit run, emit one field. We instead walk structurally: whenever
    // we hit a Bit, it belongs to the current field; every 8 bits advance the
    // field counter. To keep it robust we flatten each Word8 explicitly.
    match ty {
        Type::One => SV::Unit,
        // A single Bit leaf -- should not occur at top level of a Word8 carrier,
        // but handle for completeness (treats it as field `*field`, bit 0).
        Type::Sum(a, b) if matches!((a.as_ref(), b.as_ref()), (Type::One, Type::One)) => {
            let bit = field_bit_var(vs, nf, *field, 0);
            *field += 1;
            SV::Bit(bit)
        }
        Type::Prod(_, _) => {
            // Peel leading Word8 fields off the right-nested product.
            build_state_fields(ty, vs, nf, layout, field)
        }
        other => panic!("unexpected CpuState type node: {other:?}"),
    }
}

/// Walk the right-nested CpuState product, emitting one symbolic Word8 SV per
/// 8-bit field. The carrier is `field0 x (field1 x (... x fieldN))`.
fn build_state_fields(
    ty: &Type,
    vs: &BddVariableSet,
    nf: usize,
    layout: &Layout,
    field: &mut usize,
) -> SV {
    match ty {
        // Last field: a bare Word8 (`flat(8)`), no trailing product.
        _ if is_word8(ty) => {
            let sv = field_word8(vs, nf, layout, *field);
            *field += 1;
            sv
        }
        Type::Prod(a, rest) if is_word8(a) => {
            let head = field_word8(vs, nf, layout, *field);
            *field += 1;
            let tail = build_state_fields(rest, vs, nf, layout, field);
            SV::pair(head, tail)
        }
        other => panic!("CpuState field not a Word8-headed product: {other:?}"),
    }
}

/// A Word8 SV for field `f`: symbolic, unless `f` is pinned to constant 0.
fn field_word8(vs: &BddVariableSet, nf: usize, layout: &Layout, f: usize) -> SV {
    if is_pinned_zero(layout, f) {
        const_zero_word8(vs)
    } else {
        symbolic_word8(vs, nf, f)
    }
}

/// A constant-0 Word8 SV (right-nested 8 false Bits).
fn const_zero_word8(vs: &BddVariableSet) -> SV {
    let mut sv = SV::Bit(vs.mk_false());
    for _ in 0..W - 1 {
        sv = SV::pair(SV::Bit(vs.mk_false()), sv);
    }
    sv
}

/// Is `ty` exactly `flat(8)` (a right-nested product of 8 Bits)?
fn is_word8(ty: &Type) -> bool {
    fn count_bits(ty: &Type) -> Option<usize> {
        match ty {
            Type::Sum(a, b) if matches!((a.as_ref(), b.as_ref()), (Type::One, Type::One)) => {
                Some(1)
            }
            Type::Prod(a, b) => {
                let la = count_bits(a)?;
                let lb = count_bits(b)?;
                Some(la + lb)
            }
            _ => None,
        }
    }
    count_bits(ty) == Some(W)
}

/// A symbolic Word8 SV for field `f` (right-nested 8 Bits, LSB-first).
fn symbolic_word8(vs: &BddVariableSet, nf: usize, f: usize) -> SV {
    let bits: Vec<SV> = (0..W).map(|b| SV::Bit(field_bit_var(vs, nf, f, b))).collect();
    // Right-nested product, matching flat(8) = Bit x (Bit x (...)).
    let mut sv = bits[W - 1].clone();
    for b in (0..W - 1).rev() {
        sv = SV::pair(bits[b].clone(), sv);
    }
    sv
}

// ══════════════════════════════════════════════════════════════════════════
// Spec construction -- ripple-carry sum BDDs (like wide_add's spec_sum_bdds)
// ══════════════════════════════════════════════════════════════════════════

/// `sum_i = a_i ⊕ b_i ⊕ c_i`, `c_0 = 0`, `c_{i+1} = maj(a_i, b_i, c_i)`.
/// Produces `a.len()` sum-bit BDDs of `(a + b) mod 2^len`. Requires len == b.len().
fn ripple_sum(a: &[Bdd], b: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
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

/// Add a constant `1` to the `w`-bit BDD vector `x`: ripple with b = [1,0,0,..].
fn ripple_plus_one(x: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let mut one = vec![vs.mk_false(); x.len()];
    one[0] = vs.mk_true();
    ripple_sum(x, &one, vs)
}

/// The spec PC-output bit BDDs: `PC' = (PC + sext_{w_pc}(BR) + 1) mod 2^{w_pc}`.
/// `pc`: w_pc PC input bit BDDs. `br`: 8 BR input bit BDDs. Returns w_pc bits.
///
/// CRITICAL sext detail (w_pc=16): BR is 8-bit sign-extended into the w_pc-bit
/// PC before adding -- sext operand bit i = BR_bit i for i<8, BR_bit7 for i>=8.
fn spec_pc_out(pc: &[Bdd], br: &[Bdd], w_pc: usize, vs: &BddVariableSet) -> Vec<Bdd> {
    assert_eq!(pc.len(), w_pc);
    assert_eq!(br.len(), W);
    let sext: Vec<Bdd> = (0..w_pc)
        .map(|i| if i < W { br[i].clone() } else { br[W - 1].clone() })
        .collect();
    let pc_plus_br = ripple_sum(pc, &sext, vs);
    ripple_plus_one(&pc_plus_br, vs)
}

// ══════════════════════════════════════════════════════════════════════════
// Field extraction from the walked output SV
// ══════════════════════════════════════════════════════════════════════════

/// Extract the `w_pc` PC-output bit BDDs from the walked CpuState output SV.
/// PC occupies `pc_field()` (low byte) and, when `pc_fields()>=2`,
/// `pc_hi_field()` (high byte). Returns LSB-first `w_pc` bits.
fn out_pc_bits(out: &SV, layout: &Layout) -> Vec<Bdd> {
    let nf = n_fields(layout);
    let lo = extract_word8_bdds(extract_field(out, layout.pc_field(), nf));
    let mut bits: Vec<Bdd> = lo.to_vec();
    if layout.pc_fields() >= 2 {
        let hi = extract_word8_bdds(extract_field(out, layout.pc_hi_field(), nf));
        bits.extend(hi);
    }
    assert_eq!(bits.len(), layout.w_pc);
    bits
}

/// Extract the 8 output bit BDDs of field `f`.
fn out_field_bits(out: &SV, layout: &Layout, f: usize) -> Vec<Bdd> {
    let nf = n_fields(layout);
    extract_word8_bdds(extract_field(out, f, nf)).to_vec()
}

// ══════════════════════════════════════════════════════════════════════════
// The core symbolic proof, parametric in layout.
// ══════════════════════════════════════════════════════════════════════════

/// Build the iso, symbolic input, walk it, and return (out_sv, vs, elapsed_s).
fn walk_pc_update(layout: &Layout) -> (SV, BddVariableSet, f64) {
    let iso: Iso = Iso::seq(pc_add_br_with(layout), pc_inc_with(layout));
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let sv_in = symbolic_state_sv(layout, &vs);
    let t0 = Instant::now();
    let mut ctx = WalkCtx::new();
    let out = eval_bdd(&iso, sv_in, &vs, &mut ctx)
        .unwrap_or_else(|e| panic!("eval_bdd FAILED on pc_add_br;pc_inc @ w_pc={}: {e}", layout.w_pc));
    let elapsed = t0.elapsed().as_secs_f64();
    (out, vs, elapsed)
}

/// Prove `pc_add_br_with(&layout) ; pc_inc_with(&layout)` ≡ the ADR-0034 spec
/// symbolically over the WHOLE (PC, BR, regs, IR, scratch...) domain.
fn prove_pc_update(layout: &Layout) -> f64 {
    let nf = n_fields(layout);
    let (out, vs, elapsed) = walk_pc_update(layout);

    // Input bit BDDs.
    let pc_in: Vec<Bdd> = {
        let mut v = field_bits(&vs, nf, layout.pc_field());
        if layout.pc_fields() >= 2 {
            v.extend(field_bits(&vs, nf, layout.pc_hi_field()));
        }
        v
    };
    let br_in = field_bits(&vs, nf, layout.br_field());

    // ── PC output ≡ spec (PC + sext(BR) + 1) mod 2^w_pc ──
    let spec_pc = spec_pc_out(&pc_in, &br_in, layout.w_pc, &vs);
    let out_pc = out_pc_bits(&out, layout);
    for i in 0..layout.w_pc {
        assert!(
            out_pc[i] == spec_pc[i],
            "w_pc={}: PC output bit {i} != (PC + sext(BR) + 1) spec",
            layout.w_pc
        );
    }

    // ── BR preserved (read-only control) ──
    let br_out = out_field_bits(&out, layout, layout.br_field());
    for i in 0..W {
        assert!(
            br_out[i] == br_in[i],
            "w_pc={}: BR output bit {i} not preserved",
            layout.w_pc
        );
    }

    // ── s0 and s1 restored to 0 on exit (decoder scratch convention) ──
    // s0/s1 start at CONSTANT 0 (the operand-scratch region the adder threads
    // its operand through transiently). They must be RESTORED to 0 on exit --
    // the "quiescent scratch" half of ADR-0034 (b22b asserts s0==s1==0).
    let zero = vs.mk_false();
    let s0_out = out_field_bits(&out, layout, layout.scratch_field(0));
    let s1_out = out_field_bits(&out, layout, layout.scratch_field(1));
    for i in 0..W {
        assert!(
            s0_out[i] == zero,
            "w_pc={}: s0 bit {i} not restored to 0",
            layout.w_pc
        );
        assert!(
            s1_out[i] == zero,
            "w_pc={}: s1 bit {i} not restored to 0",
            layout.w_pc
        );
    }

    // ── Every OTHER field is the identity (regs, IR, remaining scratch, and --
    //    at w_pc=8 -- no PC_hi field). This proves no PC bit misroutes into a
    //    neighboring field (the exact wire-aliasing bug b22b guards against). ──
    let pc_hi = if layout.pc_fields() >= 2 {
        layout.pc_hi_field()
    } else {
        usize::MAX
    };
    let touched = [
        layout.pc_field(),
        pc_hi,
        layout.br_field(),
        layout.scratch_field(0),
        layout.scratch_field(1),
    ];
    for f in 0..nf {
        if touched.contains(&f) {
            continue;
        }
        let f_in = field_bits(&vs, nf, f);
        let f_out = out_field_bits(&out, layout, f);
        for i in 0..W {
            assert!(
                f_out[i] == f_in[i],
                "w_pc={}: field {f} bit {i} not identity (stray write)",
                layout.w_pc
            );
        }
    }

    elapsed
}

// ══════════════════════════════════════════════════════════════════════════
// pc_inc ALONE -- the isolated `+1` half (subsumes step.rs::pc_inc_exhaustive).
//
// `pc_inc_with(&layout)` ≡ `PC' = (PC + 1) mod 2^w_pc` over the WHOLE PC domain,
// with the carry crossing the PC_lo→PC_hi field boundary (w_pc=16) and s1 scratch
// restored. This gives the +1 half its OWN whole-domain authority (rather than
// only implicitly inside the composite pc_add_br;pc_inc). Negative control: a
// spec that DROPS the +1 (PC' = PC) must be detected as inequivalent.
// ══════════════════════════════════════════════════════════════════════════

/// Walk `pc_inc_with(&layout)` alone on an all-symbolic state.
fn walk_pc_inc(layout: &Layout) -> (SV, BddVariableSet, f64) {
    let iso: Iso = pc_inc_with(layout);
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let sv_in = symbolic_state_sv(layout, &vs);
    let t0 = Instant::now();
    let mut ctx = WalkCtx::new();
    let out = eval_bdd(&iso, sv_in, &vs, &mut ctx)
        .unwrap_or_else(|e| panic!("eval_bdd FAILED on pc_inc @ w_pc={}: {e}", layout.w_pc));
    let elapsed = t0.elapsed().as_secs_f64();
    (out, vs, elapsed)
}

/// Prove `pc_inc_with(&layout)` ≡ `(PC + 1) mod 2^w_pc` over the whole domain,
/// with a teeth-check negative control (drop the +1).
fn prove_pc_inc(layout: &Layout) -> f64 {
    let nf = n_fields(layout);
    let (out, vs, elapsed) = walk_pc_inc(layout);

    let pc_in: Vec<Bdd> = {
        let mut v = field_bits(&vs, nf, layout.pc_field());
        if layout.pc_fields() >= 2 {
            v.extend(field_bits(&vs, nf, layout.pc_hi_field()));
        }
        v
    };

    // PC' ≡ PC + 1.
    let spec_pc = ripple_plus_one(&pc_in, &vs);
    let out_pc = out_pc_bits(&out, layout);
    for i in 0..layout.w_pc {
        assert!(
            out_pc[i] == spec_pc[i],
            "w_pc={}: pc_inc output bit {i} != (PC + 1) spec",
            layout.w_pc
        );
    }

    // NEGATIVE CONTROL: a spec that drops the +1 (PC' = PC) must DIFFER.
    let differs = (0..layout.w_pc).any(|i| out_pc[i] != pc_in[i]);
    assert!(
        differs,
        "w_pc={}: NEG pc_inc must DIFFER from an identity (drop-+1) spec -- the oracle has TEETH",
        layout.w_pc
    );

    // BR / s0 / s1 and every non-PC field preserved (no stray write; s1 is the
    // pc_inc carry scratch and must be restored to its symbolic input).
    let touched_pc = [
        layout.pc_field(),
        if layout.pc_fields() >= 2 { layout.pc_hi_field() } else { usize::MAX },
    ];
    for f in 0..nf {
        if touched_pc.contains(&f) {
            continue;
        }
        // s0/s1 entered as CONSTANT 0 (pinned by symbolic_state_sv); the rest
        // are symbolic. Either way, pc_inc must return each field to its input.
        let f_in = if is_pinned_zero(layout, f) {
            vec![vs.mk_false(); W]
        } else {
            field_bits(&vs, nf, f)
        };
        let f_out = out_field_bits(&out, layout, f);
        for i in 0..W {
            assert!(
                f_out[i] == f_in[i],
                "w_pc={}: pc_inc field {f} bit {i} not preserved (stray write / carry not restored)",
                layout.w_pc
            );
        }
    }

    elapsed
}

#[test]
fn bveq_pc_inc_wpc8() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    assert_eq!(layout.pc_fields(), 1, "w_pc=8 must have exactly 1 PC field");
    let secs = prove_pc_inc(&layout);
    eprintln!(
        "BVEQ POC [w_pc=8]: pc_inc == (PC + 1) mod 2^8 over the FULL 2^8 PC domain \
         [regs/IR/scratch SYMBOLIC] -- proved SYMBOLICALLY in {secs:.4} s (+ TEETH: != drop-+1)."
    );
}

#[test]
fn bveq_pc_inc_wpc16() {
    let layout = Layout::default();
    assert_eq!(layout.w_pc, 16, "default layout is w_pc=16");
    let secs = prove_pc_inc(&layout);
    eprintln!(
        "BVEQ POC [w_pc=16]: pc_inc == (PC + 1) mod 2^16 over the FULL 2^16 PC domain \
         [PC_lo→PC_hi carry; s1 carry-scratch restored] -- proved SYMBOLICALLY in {secs:.4} s \
         (brute-force baseline: step.rs::pc_inc_exhaustive_all_16bit over 2^16; + TEETH: != drop-+1)."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// The headline tests.
// ══════════════════════════════════════════════════════════════════════════

#[test]
fn bveq_pc_update_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    assert_eq!(layout.pc_fields(), 1, "w_pc=8 must have exactly 1 PC field");
    let secs = prove_pc_update(&layout);
    eprintln!(
        "BVEQ POC [w_pc=8]: pc_add_br;pc_inc == (PC + sext(BR) + 1) mod 2^8 over the FULL \
         2^16 (PC,BR) domain [regs/IR/s2..s7 also SYMBOLIC; s0/s1 pinned 0 per decoder convention] -- proved SYMBOLICALLY in \
         {secs:.4} s (brute-force baseline: 387 s over 2^16). Speedup ~{:.0}×.",
        387.0 / secs.max(1e-6)
    );
}

#[test]
fn bveq_pc_update_wpc16() {
    let layout = Layout::default();
    assert_eq!(layout.w_pc, 16, "default layout is w_pc=16");
    assert_eq!(layout.pc_fields(), 2, "w_pc=16 must have 2 PC fields");
    let secs = prove_pc_update(&layout);
    eprintln!(
        "BVEQ POC [w_pc=16]: pc_add_br;pc_inc == (PC + sext(BR) + 1) mod 2^16 over the FULL \
         2^24 (PC,BR) domain [regs/IR/s2..s7 also SYMBOLIC; s0/s1 pinned 0 per decoder convention] -- proved SYMBOLICALLY in \
         {secs:.4} s (brute-force baseline: ~HOURS over 2^24). BDD time is domain-size \
         INDEPENDENT (≈ flat vs w_pc=8)."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// CONCRETE BOOTSTRAP -- the REALITY ANCHOR.
//
// The BVEQ proof above is the whole-domain authority, but it trusts `eval_bdd`
// and the ripple-carry BDD spec. This section runs a handful of representative
// + boundary (PC, BR) pairs through the REAL `theseus_eval::eval` (the concrete
// interpreter the shipped CPU runs) on the SAME `pc_add_br_with;pc_inc_with`
// iso, and asserts the concrete result equals the `pc_update` spec byte-for-byte
// -- plus BR/s0/s1 preservation and the ADJOINT recovering the input. This ties
// the symbolic model to reality: a bug in eval_bdd or the BDD spec that happened
// to make the symbolic sides agree with each other would still be caught here,
// because eval is an independent oracle. This is NOT the coverage (the BDD is);
// it is O(dozens) of inputs = sub-second. Ported from the retired b22b/step.rs
// exhaustive loops (which this file's BVEQ proof now subsumes as authority).
// ══════════════════════════════════════════════════════════════════════════

const EVAL_FUEL: u64 = 8_000_000_000;

/// A distinctive CpuState for the bootstrap: nonzero regs/IR and a nonzero
/// scratch tail (s2..) so any stray write outside the PC/BR/s0/s1 region shows
/// up as a mismatch. s0=s1=0 per the decoder scratch convention the adder needs.
fn boot_state(pc: u16, br: u8) -> StateVals {
    let mut regs = [0u8; NUM_REGS];
    regs[0] = 0x2A;
    regs[1] = 0xC3;
    regs[NUM_REGS - 1] = 0x5A;
    let mut scratch = [0u8; NUM_SCRATCH];
    scratch[2] = 0xDE;
    scratch[3] = 0xAD;
    StateVals {
        regs,
        pc,
        br: u16::from(br),
        ir: 0xDEAD_BEEF,
        scratch,
        mem: [0u8; MEM_WORDS],
    }
}

fn eval_boot(layout: &Layout, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode_with(layout), &mut fuel)
        .unwrap_or_else(|e| panic!("bootstrap eval failed: {e:?}"));
    StateVals::decode_with(layout, &ev)
}

/// Run the boundary (PC, BR) grid through the REAL eval and check against the
/// `pc_update` spec, BR/s0/s1 preservation, no stray writes, and the adjoint.
fn concrete_bootstrap(layout: &Layout, pcs: &[u16], brs: &[u8]) {
    let iso: Iso = Iso::seq(pc_add_br_with(layout), pc_inc_with(layout));
    let back = Iso::sym(iso.clone());
    for &br in brs {
        for &pc in pcs {
            let s = boot_state(pc, br);
            let got = eval_boot(layout, &iso, &s);

            // PC' == (PC + sext(BR) + 1) mod 2^w_pc, computed by the shipped
            // `pc_update` helper (masked to w_pc for the w_pc=8 layout).
            let mask: u16 = if layout.w_pc >= 16 { u16::MAX } else { (1u16 << layout.w_pc) - 1 };
            let expected_pc = pc_update(pc & mask, br) & mask;
            assert_eq!(
                got.pc, expected_pc,
                "w_pc={}: real eval PC' != pc_update spec (pc={pc:#06x} br={br:#04x})",
                layout.w_pc
            );
            // BR preserved (read-only control); s0/s1 restored to 0.
            assert_eq!(got.br, u16::from(br), "w_pc={}: BR preserved (pc={pc:#06x} br={br:#04x})", layout.w_pc);
            assert_eq!(got.scratch[0], 0, "w_pc={}: s0 restored (pc={pc:#06x} br={br:#04x})", layout.w_pc);
            assert_eq!(got.scratch[1], 0, "w_pc={}: s1 restored (pc={pc:#06x} br={br:#04x})", layout.w_pc);
            // No stray write outside PC/BR/s0/s1: regs, IR, scratch[2..] identical.
            assert_eq!(got.regs, s.regs, "w_pc={}: regs untouched (pc={pc:#06x} br={br:#04x})", layout.w_pc);
            assert_eq!(got.ir, s.ir, "w_pc={}: IR untouched (pc={pc:#06x} br={br:#04x})", layout.w_pc);
            assert_eq!(&got.scratch[2..], &s.scratch[2..], "w_pc={}: scratch tail untouched", layout.w_pc);

            // ADJOINT: sym(pc_add_br;pc_inc) subtracts (PC - sext(BR) - 1) and
            // recovers the exact input -- the reversibility half of ADR-0034.
            let rec = eval_boot(layout, &back, &got);
            assert_eq!(rec, s, "w_pc={}: adjoint must recover input (pc={pc:#06x} br={br:#04x})", layout.w_pc);
        }
    }
}

#[test]
fn bveq_pc_update_bootstrap_wpc8() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    assert_eq!(layout.pc_fields(), 1, "w_pc=8 must have exactly 1 PC field");
    // Carry / sign boundaries on the 8-bit PC and BR.
    let pcs: Vec<u16> = [0u8, 1, 0x7F, 0x80, 0xFE, 0xFF].iter().map(|&x| u16::from(x)).collect();
    let brs = [0u8, 1, 7, 8, 0x7F, 0x80, 0xF6, 0xFF];
    concrete_bootstrap(&layout, &pcs, &brs);
    eprintln!(
        "BVEQ bootstrap [w_pc=8]: real theseus_eval on pc_add_br;pc_inc == pc_update spec over \
         {} boundary (PC,BR) pairs [BR/s0/s1 preserved, adjoint recovers input] -- the reality anchor.",
        pcs.len() * brs.len()
    );
}

#[test]
fn bveq_pc_update_bootstrap_wpc16() {
    let layout = Layout::default();
    assert_eq!(layout.w_pc, 16, "default layout is w_pc=16");
    // 16-bit carry boundaries (inter-byte carry 0x00FF→0x0100, full wrap) crossed
    // with sign-boundary BR values (incl. top-bit-set = negative sext backward jump).
    let pcs = [0u16, 0x00FF, 0x0100, 0x7FFF, 0x8000, 0xFFFF, 0x1234];
    let brs = [0u8, 1, 0x7F, 0x80, 0xF6, 0xFF];
    concrete_bootstrap(&layout, &pcs, &brs);
    eprintln!(
        "BVEQ bootstrap [w_pc=16]: real theseus_eval on pc_add_br;pc_inc == pc_update spec over \
         {} boundary (PC,BR) pairs [inter-byte carry + sext backward-jump; BR/s1 preserved, adjoint \
         recovers input] -- the reality anchor.",
        pcs.len() * brs.len()
    );
}

// ══════════════════════════════════════════════════════════════════════════
// NEGATIVE CONTROLS -- the oracle must have TEETH.
//
// Each proves the BDD comparison catches a real bug in the spec: if the walked
// netlist accidentally matched a WRONG spec, the POC would be worthless.
// ══════════════════════════════════════════════════════════════════════════

/// Negative control at a given layout: three WRONG specs, each must DIFFER from
/// the true netlist output on >= 1 PC bit.
fn negative_controls(layout: &Layout) {
    let nf = n_fields(layout);
    let (out, vs, _) = walk_pc_update(layout);
    let out_pc = out_pc_bits(&out, layout);

    let pc_in: Vec<Bdd> = {
        let mut v = field_bits(&vs, nf, layout.pc_field());
        if layout.pc_fields() >= 2 {
            v.extend(field_bits(&vs, nf, layout.pc_hi_field()));
        }
        v
    };
    let br_in = field_bits(&vs, nf, layout.br_field());
    let w_pc = layout.w_pc;

    // WRONG-1: drop the `+1` (PC' = PC + sext(BR)).
    let sext: Vec<Bdd> = (0..w_pc)
        .map(|i| if i < W { br_in[i].clone() } else { br_in[W - 1].clone() })
        .collect();
    let no_plus_one = ripple_sum(&pc_in, &sext, &vs);
    let differs1 = (0..w_pc).any(|i| out_pc[i] != no_plus_one[i]);
    assert!(
        differs1,
        "w_pc={w_pc}: NEG-1 the netlist must DIFFER from a spec that drops +1"
    );

    // WRONG-2: forget the sext -- BR ZERO-extends instead of sign-extends.
    // (Only distinguishable when w_pc > 8; at w_pc=8 sext(BR)==BR==zext(BR).)
    if w_pc > W {
        let zext: Vec<Bdd> = (0..w_pc)
            .map(|i| if i < W { br_in[i].clone() } else { vs.mk_false() })
            .collect();
        let zext_spec = ripple_plus_one(&ripple_sum(&pc_in, &zext, &vs), &vs);
        let differs2 = (0..w_pc).any(|i| out_pc[i] != zext_spec[i]);
        assert!(
            differs2,
            "w_pc={w_pc}: NEG-2 the netlist must DIFFER from a zero-extend (no-sext) spec"
        );
    }

    // WRONG-3: carry-less byte split -- add PC and sext(BR)+1 per-byte with the
    // carry RESET at each 8-bit boundary (defeats cross-byte ripple). At w_pc=8
    // this collapses to the correct spec, so only meaningful at w_pc>8.
    if w_pc > W {
        let sext: Vec<Bdd> = (0..w_pc)
            .map(|i| if i < W { br_in[i].clone() } else { br_in[W - 1].clone() })
            .collect();
        // Correct low byte (PC + sext(BR) + 1) then HIGH byte with carry reset.
        let lo = ripple_plus_one(
            &ripple_sum(&pc_in[0..W], &sext[0..W], &vs),
            &vs,
        );
        let hi = ripple_sum(&pc_in[W..w_pc], &sext[W..w_pc], &vs); // no carry-in
        let mut carryless = lo;
        carryless.extend(hi);
        let differs3 = (0..w_pc).any(|i| out_pc[i] != carryless[i]);
        assert!(
            differs3,
            "w_pc={w_pc}: NEG-3 the netlist must DIFFER from a carry-less byte split"
        );
    }
}

#[test]
fn bveq_pc_update_negative_control_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    negative_controls(&layout);
    eprintln!(
        "BVEQ POC negative control [w_pc=8]: netlist != (spec without +1) -- the oracle has TEETH."
    );
}

#[test]
fn bveq_pc_update_negative_control_wpc16() {
    let layout = Layout::default();
    negative_controls(&layout);
    eprintln!(
        "BVEQ POC negative control [w_pc=16]: netlist != {{drop-+1, zero-extend-BR, \
         carry-less-byte-split}} -- the oracle has TEETH across all three wrong specs."
    );
}
