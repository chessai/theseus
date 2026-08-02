//! BVEQ-STEP-POC — prove the REAL compiler-emitted PC-update netlist correct
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
//!     (PC, BR) pairs — ~387 s in the last heavy run.
//!   - The w_pc=16 analogue is 2^24 (~256× the domain → hours).
//!
//! This file proves the SAME property, over the WHOLE domain, SYMBOLICALLY in
//! ~seconds via the repo's existing BDD machinery (`bveq_netlist::eval_bdd` —
//! the symbolic Iso interpreter that also underpins `wide_add.rs`). The BDD
//! proof time is TINY and ~FLAT across w_pc=8 (2^16 domain) and w_pc=16 (2^24
//! domain), because a canonical BDD comparison is independent of domain size.
//!
//! ─────────────────────────────────────────────────────────────────────────
//! WHAT IS PROVEN (honest coverage)
//!
//! `eval_bdd` handles the COMBINATIONAL (loop-free) fragment — Seq/Times/Swap/
//! Assoc/Unite/Prim gates — and REJECTS Trace/Iter/Fold (loops). The PC-update
//! iso `pc_add_br_with ; pc_inc_with` is entirely combinational (a fixed cascade
//! of CNOT/Toffoli gates), so it is fully within `eval_bdd`'s fragment. This
//! proves the ONE-STEP PC-update netlist ≡ its spec over the WHOLE state domain.
//! Multi-step program correctness (the b21c composite-ring sweeps, the real-CPU
//! authority runs) then follows by induction over steps — proving each step's
//! netlist ≡ step spec ONCE symbolically eliminates per-input, per-step
//! simulation. (Full multi-step step-phase coverage needs eval_bdd extended past
//! the pc-update fragment; the pc-update iso here needs no such extension.)

use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::{Iso, Type};

use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::step::{pc_add_br_with, pc_inc_with};
use theseus_tisc::types::{state_ty_with, Layout, W};

// ══════════════════════════════════════════════════════════════════════════
// Symbolic CpuState construction
//
// The iso operates on the FULL CpuState carrier `state_ty_with(&layout)` — a
// right-nested product of `num_cpu_fields()` Word8 fields (every field is 8-bit
// at both w_pc=8 and w_pc=16, since w_ctl==w_data==8). We build an ALL-SYMBOLIC
// state SV (every field gets fresh BDD variables), which proves MORE than the
// b22b sweep: it holds regs/IR/scratch symbolic (arbitrary), not const-0.
//
// Variable ordering — BIT-INTERLEAVED (`var = bit*num_fields + field`), the same
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
/// are pinned to CONSTANT 0 — the decoder scratch convention the PC adder
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
        // A single Bit leaf — should not occur at top level of a Word8 carrier,
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
// Spec construction — ripple-carry sum BDDs (like wide_add's spec_sum_bdds)
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
/// PC before adding — sext operand bit i = BR_bit i for i<8, BR_bit7 for i>=8.
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
    // its operand through transiently). They must be RESTORED to 0 on exit —
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

    // ── Every OTHER field is the identity (regs, IR, remaining scratch, and —
    //    at w_pc=8 — no PC_hi field). This proves no PC bit misroutes into a
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
         2^16 (PC,BR) domain [regs/IR/s2..s7 also SYMBOLIC; s0/s1 pinned 0 per decoder convention] — proved SYMBOLICALLY in \
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
         2^24 (PC,BR) domain [regs/IR/s2..s7 also SYMBOLIC; s0/s1 pinned 0 per decoder convention] — proved SYMBOLICALLY in \
         {secs:.4} s (brute-force baseline: ~HOURS over 2^24). BDD time is domain-size \
         INDEPENDENT (≈ flat vs w_pc=8)."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// NEGATIVE CONTROLS — the oracle must have TEETH.
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

    // WRONG-2: forget the sext — BR ZERO-extends instead of sign-extends.
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

    // WRONG-3: carry-less byte split — add PC and sext(BR)+1 per-byte with the
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
        "BVEQ POC negative control [w_pc=8]: netlist != (spec without +1) — the oracle has TEETH."
    );
}

#[test]
fn bveq_pc_update_negative_control_wpc16() {
    let layout = Layout::default();
    negative_controls(&layout);
    eprintln!(
        "BVEQ POC negative control [w_pc=16]: netlist != {{drop-+1, zero-extend-BR, \
         carry-less-byte-split}} — the oracle has TEETH across all three wrong specs."
    );
}
