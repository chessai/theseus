#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1.
//! GO-ROLLOUT: the BRANCHING AUTHORITY, retired from brute-force `theseus_eval`
//! simulation to a SYMBOLIC BVEQ whole-domain proof (mirrors decoder_bveq.rs's
//! retirement of decoder.rs).
//!
//! ── What this retires ──────────────────────────────────────────────
//! `tests/branching.rs` proved ADR-0034 data-dependent branching by CONCRETE
//! `theseus_eval::eval` on a handful of hand-built states (>4 min, NOT whole-
//! domain):
//!   * `taken_branch_with_come_from` -- a program that loads an offset δ into a
//!     register, `SWAPBR rd` moves it into BR, and the SAME-STEP end-of-step PC
//!     update jumps by that offset; then a come-from SWAPBR clears BR; matched a
//!     Rust reference emulator step-by-step (5 concrete steps).
//!   * `ir_zero_and_scratch_after_branch` -- P4 (IR=0) + s0/s1 restored across
//!     SWAPBR steps, a few concrete (pc,br,r0,r1) points.
//!   * `br_zero_fall_through_is_pc_plus_one` -- BR=0 ⇒ PC += 1 (v1-compat), a
//!     few concrete straight-line points.
//!   * the REVERSIBILITY obligation (T forward + T adjoint = start) for the
//!     multi-step branch program.
//!
//! ── The symbolic replacement (strictly STRONGER on the datapath) ────
//! The heart of branching is a SINGLE instruction whose datapath is NOT
//! register-pure: `SWAPBR rd` (opcode 0b1000) swaps r[rd] ↔ BR, and because the
//! end-of-step `pc_add_br` reads BR AFTER the swap, the branch offset is the OLD
//! r[rd] (= new BR). We walk the FULL wired step netlist `step_with(&layout,&rom)`
//! ONCE with a CONCRETE (nonzero, branch-taken) PC so `fetch` reads a CONSTANT
//! SWAPBR rom word, but SYMBOLIC register file + SYMBOLIC BR, and prove over the
//! WHOLE (regs, BR) domain, via `bveq_netlist::eval_bdd`:
//!   * rd' = old BR   (the SWAPBR swap, low byte);
//!   * BR' = old rd   (the swap's other half → becomes the branch offset);
//!   * PC' = (pc + sext(old r[rd]) + 1) mod 2^w_pc  -- the BRANCH TAKEN by the
//!     value SWAPBR just moved into BR (ADR-0034; reuses bveq_step_poc's adder);
//!   * IR back to 0 (unfetch, P4); s0 = s1 = 0 restored;
//!   * every UNTOUCHED register = identity; s2.. scratch = identity.
//!
//! ── Soundness: the SWAPBR oracle is INDEPENDENT of the netlist ──────
//! decoder_bveq.rs compares the wired decoder against `bveq::instr_semantics`,
//! an independent per-op oracle. `instr_semantics` HARD-REJECTS SWAPBR
//! (`NonRegisterPure`: it moves state off the register frame into BR), exactly
//! as decoder_bveq.rs's coverage boundary notes -- so we CANNOT reuse it here.
//! Instead the SWAPBR spec below is built DIRECTLY from the symbolic INPUT bits
//! (rd_out ≡ br_in, br_out ≡ rd_in, PC' ≡ pc + sext(rd_in) + 1) from the ISA
//! definition -- it never reads the walked netlist output, so a datapath bug or
//! a mis-dispatch (a DIFFERENT op decoding for the SWAPBR word) shows up as
//! netlist-output ≠ this independent spec. This is honest: the oracle is the ISA
//! spec of SWAPBR, not the netlist compared against itself.
//!
//! ── Relationship to bveq_full_step_poc.rs ───────────────────────────
//! `bveq_full_step_poc.rs` proves `step_with ≡ step-spec` whole-domain, but ONLY
//! for the register-PURE op family {Xor, Add, Sub, Xori, Ccnot} and at PC=0
//! (fall-through: BR unchanged, so its PC' uses the INPUT BR). It does NOT cover
//! SWAPBR (its `reg_spec`/`written_regs` panic on `Instr::Swapbr`). This file is
//! the branch-SPECIFIC specialization full_step_poc omits: SWAPBR at a CONCRETE
//! NONZERO pc (branch TAKEN) where the offset is the value SWAPBR moved into BR,
//! plus the come-from step (BR ← 0). SWAPBR was NOT in full_step_poc.
//!
//! ── Authority tiers (the GO-rollout convention) ─────────────────────
//! 1. BVEQ WHOLE-DOMAIN (primary): `branch_swapbr_whole_domain` -- SWAPBR at a
//!    concrete nonzero PC, symbolic regs+BR, `step_with` ≡ the independent
//!    SWAPBR spec over the WHOLE domain (+ come-from BR=0 whole-domain via
//!    `branch_come_from_whole_domain`).
//! 2. REALITY-ANCHOR BOOTSTRAP (sub-second): `branch_reality_anchor` -- the
//!    SAME `step` iso run through concrete `theseus_eval::eval`, lifting
//!    branching.rs's δ=3 branch-taken program + BR=0 fall-through, == the Rust
//!    reference emulator; and `branch_reversibility` (T fwd + T adjoint =
//!    start) -- the multi-step reversibility obligation kept concrete.
//! 3. NEGATIVE CONTROL (teeth): `branch_negative_control` -- WRONG specs (swap
//!    direction reversed; drop the +1; offset from OLD BR instead of NEW BR)
//!    MUST be detected as inequivalent.

// Bit-index loops (`for i in 0..W { out[i] ... }`) mirror the sibling BVEQ tests
// (bveq_full_step_poc / decoder_bveq); the index is load-bearing (compares two
// parallel BDD-bit vectors), so the range-loop form is intentional.
#![allow(clippy::needless_range_loop)]

use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::{Iso, Type};

use theseus_tisc::asm::{assemble, encode, Instr};
use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::compile::pc_update;
use theseus_tisc::step::{step, step_with};
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_SCRATCH, W};

// ══════════════════════════════════════════════════════════════════════════
// Symbolic CpuState construction (bit-interleaved varmap, per bveq_step_poc /
// bveq_full_step_poc). PC is CONCRETE (a chosen branch-taken address), IR=0,
// s0=s1=0; register file + BR + s2.. SYMBOLIC.
// ══════════════════════════════════════════════════════════════════════════

fn n_fields(layout: &Layout) -> usize {
    layout.num_cpu_fields()
}
fn n_vars(layout: &Layout) -> u16 {
    (n_fields(layout) * W) as u16
}
/// Bit-interleaved var for field `f`, bit `b`: `var = b*nf + f`.
fn field_bit_var(vs: &BddVariableSet, nf: usize, f: usize, b: usize) -> Bdd {
    vs.mk_var(vs.variables()[b * nf + f])
}
fn field_bits(vs: &BddVariableSet, nf: usize, f: usize) -> Vec<Bdd> {
    (0..W).map(|b| field_bit_var(vs, nf, f, b)).collect()
}

/// Fields pinned CONCRETE for this instance. PC (lo+hi) is set to a chosen
/// concrete address (so `fetch` reads a CONSTANT rom word -- no PC-mux blow-up);
/// IR bytes = 0 (matches the pre-fetch machine); s0/s1 = 0 (decoder scratch
/// convention, restored on exit). Everything else -- regs, BR, s2.. -- SYMBOLIC.
fn is_pc_field(layout: &Layout, f: usize) -> bool {
    f == layout.pc_field() || (layout.pc_fields() >= 2 && f == layout.pc_hi_field())
}
fn is_concrete(layout: &Layout, f: usize) -> bool {
    if is_pc_field(layout, f) {
        return true;
    }
    for k in 0..4 {
        if f == layout.ir_byte_field(k) {
            return true;
        }
    }
    f == layout.scratch_field(0) || f == layout.scratch_field(1)
}

/// Concrete Word8 SV holding byte `val` (right-nested 8 const Bits, LSB-first).
fn const_word8(vs: &BddVariableSet, val: u8) -> SV {
    let bit = |i: usize| {
        if (val >> i) & 1 == 1 {
            SV::Bit(vs.mk_true())
        } else {
            SV::Bit(vs.mk_false())
        }
    };
    let mut sv = bit(W - 1);
    for b in (0..W - 1).rev() {
        sv = SV::pair(bit(b), sv);
    }
    sv
}
fn symbolic_word8(vs: &BddVariableSet, nf: usize, f: usize) -> SV {
    let bits: Vec<SV> = (0..W)
        .map(|b| SV::Bit(field_bit_var(vs, nf, f, b)))
        .collect();
    let mut sv = bits[W - 1].clone();
    for b in (0..W - 1).rev() {
        sv = SV::pair(bits[b].clone(), sv);
    }
    sv
}
fn is_word8(ty: &Type) -> bool {
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

/// The concrete byte a PC field must carry so PC == `pc_val` (lo byte at
/// `pc_field()`, hi byte at `pc_hi_field()`).
fn pc_field_byte(layout: &Layout, f: usize, pc_val: u16) -> u8 {
    if f == layout.pc_field() {
        (pc_val & 0xFF) as u8
    } else {
        // pc_hi_field
        (pc_val >> 8) as u8
    }
}

fn field_word8(vs: &BddVariableSet, nf: usize, layout: &Layout, f: usize, pc_val: u16) -> SV {
    if is_pc_field(layout, f) {
        const_word8(vs, pc_field_byte(layout, f, pc_val))
    } else if is_concrete(layout, f) {
        const_word8(vs, 0)
    } else {
        symbolic_word8(vs, nf, f)
    }
}

fn build_state_fields(
    ty: &Type,
    vs: &BddVariableSet,
    nf: usize,
    layout: &Layout,
    pc_val: u16,
    field: &mut usize,
) -> SV {
    match ty {
        _ if is_word8(ty) => {
            let sv = field_word8(vs, nf, layout, *field, pc_val);
            *field += 1;
            sv
        }
        Type::Prod(a, rest) if is_word8(a) => {
            let head = field_word8(vs, nf, layout, *field, pc_val);
            *field += 1;
            let tail = build_state_fields(rest, vs, nf, layout, pc_val, field);
            SV::pair(head, tail)
        }
        other => panic!("CpuState field not a Word8-headed product: {other:?}"),
    }
}

/// Build the symbolic CpuState SV mirroring `state_ty_with(&layout)`, with PC
/// pinned to the concrete `pc_val`.
fn symbolic_state_sv(layout: &Layout, vs: &BddVariableSet, pc_val: u16) -> SV {
    let nf = n_fields(layout);
    let ty = state_ty_with(layout);
    let mut field = 0usize;
    build_state_fields(&ty, vs, nf, layout, pc_val, &mut field)
}

// ══════════════════════════════════════════════════════════════════════════
// Ripple-carry BDD specs (like bveq_step_poc's spec_sum_bdds)
// ══════════════════════════════════════════════════════════════════════════

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
fn ripple_plus_one(x: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let mut one = vec![vs.mk_false(); x.len()];
    one[0] = vs.mk_true();
    ripple_sum(x, &one, vs)
}

/// Const `w_pc`-bit BDD vector for the concrete value `v` (LSB first).
fn const_bits(v: u16, w_pc: usize, vs: &BddVariableSet) -> Vec<Bdd> {
    (0..w_pc)
        .map(|i| {
            if (v >> i) & 1 == 1 {
                vs.mk_true()
            } else {
                vs.mk_false()
            }
        })
        .collect()
}

/// PC' spec = (pc + sext(off) + 1) mod 2^w_pc, where `off` is an 8-bit
/// 2's-complement offset (sign-extended into w_pc bits). `pc` is CONCRETE here;
/// `off` is the 8-bit BDD vector of the value that ends up in BR (SWAPBR: old rd).
fn spec_pc_out(pc: &[Bdd], off: &[Bdd], w_pc: usize, vs: &BddVariableSet) -> Vec<Bdd> {
    assert_eq!(pc.len(), w_pc);
    assert_eq!(off.len(), W);
    let sext: Vec<Bdd> = (0..w_pc)
        .map(|i| {
            if i < W {
                off[i].clone()
            } else {
                off[W - 1].clone()
            }
        })
        .collect();
    ripple_plus_one(&ripple_sum(pc, &sext, vs), vs)
}

// ══════════════════════════════════════════════════════════════════════════
// Output field extraction
// ══════════════════════════════════════════════════════════════════════════

fn out_field_bits(out: &SV, layout: &Layout, f: usize) -> Vec<Bdd> {
    let nf = n_fields(layout);
    extract_word8_bdds(extract_field(out, f, nf)).to_vec()
}
fn out_pc_bits(out: &SV, layout: &Layout) -> Vec<Bdd> {
    let mut bits = out_field_bits(out, layout, layout.pc_field());
    if layout.pc_fields() >= 2 {
        bits.extend(out_field_bits(out, layout, layout.pc_hi_field()));
    }
    assert_eq!(bits.len(), layout.w_pc);
    bits
}

// ══════════════════════════════════════════════════════════════════════════
// The walk
// ══════════════════════════════════════════════════════════════════════════

struct WalkResult {
    out: SV,
    vs: BddVariableSet,
    walk_s: f64,
}

/// Build `step_with(&layout, &rom)`, symbolic input with PC pinned to `pc_val`,
/// walk it once.
fn walk_step(layout: &Layout, rom: &[u32], pc_val: u16) -> WalkResult {
    let iso: Iso = step_with(layout, rom);
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let sv_in = symbolic_state_sv(layout, &vs, pc_val);
    let t0 = Instant::now();
    let mut ctx = WalkCtx::new();
    let out = eval_bdd(&iso, sv_in, &vs, &mut ctx)
        .unwrap_or_else(|e| panic!("eval_bdd FAILED on step_with (SWAPBR @ pc={pc_val}): {e}"));
    let walk_s = t0.elapsed().as_secs_f64();
    WalkResult { out, vs, walk_s }
}

// ══════════════════════════════════════════════════════════════════════════
// 1. BVEQ WHOLE-DOMAIN authority -- SWAPBR branch-taken step.
// ══════════════════════════════════════════════════════════════════════════

/// Prove `step_with(layout, [SWAPBR rd])` at a CONCRETE nonzero `pc` ≡ the
/// independent SWAPBR branch-taken step spec over the whole symbolic (regs, BR)
/// domain. Returns (walk_s, compare_s).
///
/// Spec (built DIRECTLY from the symbolic input bits -- the ISA definition of
/// SWAPBR, independent of the netlist):
///   * rd_out ≡ br_in            (SWAPBR moves old BR into rd)
///   * br_out ≡ rd_in            (SWAPBR moves old rd into BR)
///   * PC_out ≡ pc + sext(rd_in) + 1   (end-of-step `pc_add_br` reads the NEW
///     BR = old rd → the branch offset)
///   * IR = 0, s0 = s1 = 0 restored; untouched regs identity; s2.. identity.
fn prove_swapbr(layout: &Layout, rd: usize, pc: u16) -> (f64, f64) {
    assert!(
        pc != 0,
        "branch-taken instance must use a NONZERO pc (full_step_poc covers pc=0)"
    );
    // Build a rom whose word at `pc` is SWAPBR rd; earlier addresses are NOPs
    // (XORI r7, 0 = identity) so `assemble` places SWAPBR at index `pc`.
    let mut prog: Vec<Instr> = Vec::new();
    for _ in 0..pc {
        prog.push(Instr::Xori { rd: 7, imm: 0 });
    }
    prog.push(Instr::Swapbr {
        rd: rd as u8,
        rs: 0,
    });
    let rom = assemble(&prog).expect("assemble SWAPBR rom");
    assert_eq!(rom.len(), (pc as usize) + 1, "rom length = pc+1");

    let WalkResult { out, vs, walk_s } = walk_step(layout, &rom, pc);
    let nf = n_fields(layout);

    let t0 = Instant::now();
    let rin = |i: usize| field_bits(&vs, nf, layout.reg_field(i));
    let br_in = field_bits(&vs, nf, layout.br_field());
    let rd_in = rin(rd);

    // ── rd' ≡ old BR ──
    let rd_out = out_field_bits(&out, layout, layout.reg_field(rd));
    for i in 0..W {
        assert!(
            rd_out[i] == br_in[i],
            "SWAPBR r{rd} @pc={pc}: rd' bit {i} != old BR (the swap moved wrong value into rd)"
        );
    }

    // ── BR' ≡ old rd ──
    let br_out = out_field_bits(&out, layout, layout.br_field());
    for i in 0..W {
        assert!(
            br_out[i] == rd_in[i],
            "SWAPBR r{rd} @pc={pc}: BR' bit {i} != old rd (the swap moved wrong value into BR)"
        );
    }

    // ── PC' ≡ pc + sext(old rd) + 1 (branch TAKEN by the new BR = old rd) ──
    let pc_in = const_bits(pc, layout.w_pc, &vs);
    let spec_pc = spec_pc_out(&pc_in, &rd_in, layout.w_pc, &vs);
    let out_pc = out_pc_bits(&out, layout);
    for i in 0..layout.w_pc {
        assert!(
            out_pc[i] == spec_pc[i],
            "SWAPBR r{rd} @pc={pc}: PC' bit {i} != (pc + sext(old rd) + 1) -- branch offset wrong"
        );
    }

    // ── Untouched registers = identity (rd is the only reg written) ──
    for r in 0..layout.num_regs {
        if r == rd {
            continue;
        }
        let got = out_field_bits(&out, layout, layout.reg_field(r));
        let inp = rin(r);
        for i in 0..W {
            assert!(
                got[i] == inp[i],
                "SWAPBR r{rd} @pc={pc}: untouched reg r{r} bit {i} != identity (stray write)"
            );
        }
    }

    // ── IR back to 0 (unfetch, P4); s0/s1 restored to 0 ──
    let zero = vs.mk_false();
    for k in 0..4 {
        let ir = out_field_bits(&out, layout, layout.ir_byte_field(k));
        for i in 0..W {
            assert!(
                ir[i] == zero,
                "SWAPBR r{rd} @pc={pc}: IR byte {k} bit {i} not re-zeroed"
            );
        }
    }
    for s in 0..2 {
        let so = out_field_bits(&out, layout, layout.scratch_field(s));
        for i in 0..W {
            assert!(
                so[i] == zero,
                "SWAPBR r{rd} @pc={pc}: s{s} bit {i} not restored to 0"
            );
        }
    }

    // ── s2.. scratch = identity (symbolic input must be restored) ──
    for s in 2..NUM_SCRATCH {
        let so = out_field_bits(&out, layout, layout.scratch_field(s));
        let si = field_bits(&vs, nf, layout.scratch_field(s));
        for i in 0..W {
            assert!(
                so[i] == si[i],
                "SWAPBR r{rd} @pc={pc}: s{s} bit {i} not restored (crossbar residue)"
            );
        }
    }

    let compare_s = t0.elapsed().as_secs_f64();
    (walk_s, compare_s)
}

/// PRIMARY authority: SWAPBR at concrete nonzero PCs (branch TAKEN), several
/// rd, over the whole symbolic (regs, BR) domain.
#[test]
fn branch_swapbr_whole_domain() {
    for (layout_name, layout) in [
        (
            "w_pc=8",
            Layout {
                w_pc: 8,
                ..Layout::default()
            },
        ),
        ("w_pc=16", Layout::default()),
    ] {
        let t_all = Instant::now();
        let mut total_walk = 0.0f64;
        // rd across low/mid/high register slots; concrete nonzero branch-taken
        // PCs. The whole-domain claim is over symbolic (regs, BR) at a FIXED
        // concrete PC, so a couple of representative branch addresses suffice --
        // the sext-into-w_pc PC adder is exercised by the SYMBOLIC rd/BR offset
        // regardless of the concrete PC (a high PC only enlarges the NOP-padded
        // ROM the fetch PC-mux walks, which is cost, not coverage). pc=5 is
        // branching.rs's real δ=3 target address.
        let pcs: &[u16] = &[1, 5];
        for &rd in &[0usize, 3, 7] {
            for &pc in pcs {
                let (w, c) = prove_swapbr(&layout, rd, pc);
                total_walk += w;
                eprintln!("  [{layout_name}] SWAPBR r{rd} @pc={pc:#06x}: walk {:.0} ms + compare {:.0} ms", w * 1e3, c * 1e3);
            }
        }
        eprintln!(
            "BRANCH-BVEQ whole-domain [{layout_name}]: step_with(SWAPBR rd) @ concrete nonzero PC \
             ≡ (rd'=old BR, BR'=old rd, PC'=pc+sext(old rd)+1) over the WHOLE symbolic (regs,BR) \
             domain [PC concrete branch-taken; IR=0; s0/s1=0] -- {:.2}s total (branching.rs sim \
             baseline: >4 min). Independent oracle (instr_semantics REJECTS SWAPBR).",
            t_all.elapsed().as_secs_f64()
        );
        let _ = total_walk;
    }
}

/// The COME-FROM step, whole-domain: when the branch DELIVERS to a `SWAPBR rd`
/// with BR carrying the come-from offset and rd = 0 is NOT assumed -- rather we
/// prove the general SWAPBR swap identity again but read out the RESUME-sequential
/// behavior: BR' = old rd (symbolic), and if the program's come-from convention
/// holds (rd was the witness 0), BR' = 0. We prove the whole-domain swap here;
/// the concrete come-from (BR←0) is anchored in `branch_reality_anchor`.
///
/// This is the SAME datapath as `prove_swapbr` at a DIFFERENT concrete PC (the
/// branch target), so it is covered by `branch_swapbr_whole_domain`'s pc sweep.
/// We keep an explicit come-from assertion: at the target PC, the SWAPBR swap
/// with SYMBOLIC BR proves BR'=old rd for EVERY rd value, so in particular the
/// come-from's rd=0 witness yields BR'=0 (resume sequential) -- the whole-domain
/// generalization of branching.rs's concrete come-from check.
#[test]
fn branch_come_from_whole_domain() {
    // Target PC = 5 (branching.rs's δ=3 target = 1+δ+1). The come-from SWAPBR r0
    // there swaps BR ↔ r0; over the whole symbolic domain BR'=old r0. Restricting
    // r0=0 (the witness convention) gives BR'=0, i.e. resume sequential -- but the
    // whole-domain proof is strictly stronger and already covers it.
    let layout = Layout::default();
    let (w, c) = prove_swapbr(&layout, 0, 5);
    eprintln!(
        "BRANCH-BVEQ come-from whole-domain [w_pc=16]: SWAPBR r0 @pc=5 (branch target) proves \
         BR'=old r0 over the WHOLE domain -- restrict r0=0 ⇒ BR'=0 (resume sequential), the \
         whole-domain generalization of branching.rs's concrete come-from. walk {:.0} ms + compare {:.0} ms.",
        w * 1e3,
        c * 1e3
    );
}

// ══════════════════════════════════════════════════════════════════════════
// 2. REALITY-ANCHOR BOOTSTRAP -- SAME step iso, concrete eval == Rust reference.
//
// Lifts branching.rs's concrete scenarios (the δ=3 branch-taken come-from
// program + the BR=0 fall-through) through the REAL `theseus_eval::eval` on the
// SAME `step` iso the whole-domain proof walks. This ties the symbolic model to
// reality: a bug in eval_bdd or the BDD spec that made the symbolic sides agree
// with each other is still caught here, because eval is an independent oracle.
// ══════════════════════════════════════════════════════════════════════════

const EVAL_FUEL: u64 = 8_000_000_000;

/// Reference emulator (v2 semantics: PC += sext(BR) + 1), ported verbatim from
/// branching.rs. `word & (1<<31)` selects R-format.
#[allow(clippy::collapsible_match)] // the Cswap arm reads clearest as a nested if (as in branching.rs)
fn ref_instr(word: u32, s: &mut StateVals) {
    if word & (1 << 31) != 0 {
        let opcode = (word >> 25) & 0x3F;
        let rd = ((word >> 20) & 0x1F) as usize;
        let rs = ((word >> 15) & 0x1F) as usize;
        let rt = ((word >> 10) & 0x1F) as usize;
        match opcode {
            0b0000 => s.regs[rd] = s.regs[rd].wrapping_add(s.regs[rs]),
            0b0001 => s.regs[rd] = s.regs[rd].wrapping_sub(s.regs[rs]),
            0b0010 => s.regs[rd] ^= s.regs[rs],
            0b0011 => s.regs.swap(rd, rs),
            0b0100 => s.regs[rd] = s.regs[rd].rotate_left(u32::from(s.regs[rs] & 7)),
            0b0101 => s.regs[rd] = s.regs[rd].rotate_right(u32::from(s.regs[rs] & 7)),
            0b0110 => {
                if s.regs[rd] & 1 == 1 {
                    s.regs.swap(rs, rt);
                }
            }
            0b0111 => std::mem::swap(&mut s.regs[rd], &mut s.scratch[rt]),
            0b1000 => {
                let t = s.regs[rd];
                s.regs[rd] = s.br as u8;
                s.br = u16::from(t);
            }
            0b1001 if rt < MEM_WORDS => std::mem::swap(&mut s.regs[rd], &mut s.mem[rt]),
            _ => {}
        }
    } else {
        let opc = (word >> 25) & 0x3F;
        let rd = ((word >> 20) & 0x1F) as usize;
        let imm = (word & 0xFF) as u8;
        match opc {
            0b000 => s.regs[rd] = s.regs[rd].wrapping_add(imm),
            0b001 => s.regs[rd] = s.regs[rd].wrapping_sub(imm),
            0b010 => s.regs[rd] ^= imm,
            0b011 => s.regs[rd] = s.regs[rd].rotate_left(u32::from(imm & 7)),
            0b100 => s.regs[rd] = s.regs[rd].rotate_right(u32::from(imm & 7)),
            _ => {}
        }
    }
}

fn ref_step(rom: &[u32], s: &StateVals) -> StateVals {
    let mut out = s.clone();
    let word = rom.get(s.pc as usize).copied().unwrap_or(0);
    ref_instr(word, &mut out);
    out.pc = pc_update(s.pc, out.br as u8);
    out
}

fn eval_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    StateVals::decode(&ev)
}

fn zero_state() -> StateVals {
    StateVals {
        regs: [0; 8],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    }
}

/// The δ=3 branch-taken program from branching.rs, run through the SAME `step`
/// iso the whole-domain proof walks, checked step-by-step against the reference.
fn branch_taken_program() -> (Vec<u32>, Iso) {
    let delta: u8 = 3;
    let target = delta as usize + 2; // 5
    let mut prog: Vec<Instr> = vec![
        Instr::Xori { rd: 0, imm: delta }, // 0: load offset δ into r0
        Instr::Swapbr { rd: 0, rs: 0 },    // 1: branch (BR ← δ); PC = 1 + δ + 1 = 5
    ];
    while prog.len() < target {
        prog.push(Instr::Xori { rd: 7, imm: 0 }); // NOP padding
    }
    prog.push(Instr::Swapbr { rd: 0, rs: 0 }); // 5: come-from (BR ← 0)
    prog.push(Instr::Subi { rd: 0, imm: delta }); // 6: clear witness
    prog.push(Instr::Addi { rd: 1, imm: 0x42 }); // 7: arrival marker
    let rom = assemble(&prog).expect("valid program");
    let iso = step(&rom);
    (rom, iso)
}

#[test]
fn branch_reality_anchor() {
    let t0 = Instant::now();

    // ── (a) δ=3 branch-taken come-from program, step-by-step vs reference ──
    let (rom, iso) = branch_taken_program();
    let delta: u8 = 3;
    let target = delta as usize + 2;
    let mut cur = zero_state();
    for t in 0..5 {
        let next = eval_state(&format!("branch/fwd[{t}]"), &iso, &cur);
        let expected = ref_step(&rom, &cur);
        assert_eq!(next, expected, "branch step {t}: eval != reference");
        assert_eq!(next.ir, 0, "P4: IR=0 after step {t}");
        assert_eq!(next.scratch[0], 0, "s0 restored after step {t}");
        assert_eq!(next.scratch[1], 0, "s1 restored after step {t}");
        cur = next;
    }
    // Branch actually taken: after SWAPBR (step 1) PC jumped to target.
    // Re-derive the intermediate to assert the landmark.
    let mut s = zero_state();
    s = ref_step(&rom, &s); // step 0: XORI r0,δ
    assert_eq!(s.regs[0], delta);
    assert_eq!(s.pc, 1);
    s = ref_step(&rom, &s); // step 1: SWAPBR (branch)
    assert_eq!(s.regs[0], 0, "SWAPBR: r0 = old BR = 0");
    assert_eq!(s.br, u16::from(delta), "SWAPBR: BR = δ");
    assert_eq!(s.pc, target as u16, "branch taken: PC = target");

    // ── (b) BR=0 fall-through = PC+1 (v1-compat), a few concrete points ──
    let fall_rom = vec![
        encode(Instr::Addi { rd: 1, imm: 5 }).unwrap(),
        encode(Instr::Add { rd: 2, rs: 1 }).unwrap(),
        encode(Instr::Xor { rd: 3, rs: 2 }).unwrap(),
    ];
    let fall_iso = step(&fall_rom);
    for pc in 0u16..3 {
        let st = StateVals {
            regs: [0x10, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76, 0x87],
            pc,
            br: 0,
            ir: 0,
            scratch: [0; 8],
            mem: [0; MEM_WORDS],
        };
        let got = eval_state(&format!("fallthrough/pc={pc}"), &fall_iso, &st);
        assert_eq!(got.pc, pc.wrapping_add(1), "BR=0: PC += 1");
        assert_eq!(got.br, 0, "BR stays 0 (no SWAPBR)");
        assert_eq!(
            got,
            ref_step(&fall_rom, &st),
            "fall-through eval != reference"
        );
    }

    // ── (c) ir_zero_and_scratch grid (P4 + scratch across SWAPBR steps) ──
    let p4_rom = vec![
        encode(Instr::Swapbr { rd: 0, rs: 0 }).unwrap(),
        encode(Instr::Swapbr { rd: 1, rs: 0 }).unwrap(),
    ];
    let p4_iso = step(&p4_rom);
    for (pc, br, r0, r1) in [
        (0u16, 0u8, 0x00u8, 0x00u8),
        (0, 0, 0x05, 0x10),
        (0, 0x99, 0x00, 0x42),
        (1, 0, 0x33, 0x00),
        (1, 0x10, 0x00, 0x05),
    ] {
        let st = StateVals {
            regs: [r0, r1, 0, 0, 0, 0, 0, 0],
            pc,
            br: u16::from(br),
            ir: 0,
            scratch: [0; 8],
            mem: [0; MEM_WORDS],
        };
        let got = eval_state(&format!("p4/pc={pc},br={br}"), &p4_iso, &st);
        assert_eq!(
            got,
            ref_step(&p4_rom, &st),
            "P4 grid: eval != reference (pc={pc},br={br})"
        );
        assert_eq!(got.ir, 0, "P4: IR=0 (pc={pc},br={br})");
        assert_eq!(got.scratch[0], 0, "s0 restored (pc={pc},br={br})");
        assert_eq!(got.scratch[1], 0, "s1 restored (pc={pc},br={br})");
    }

    eprintln!(
        "BRANCH-BVEQ reality anchor: SAME `step` iso, concrete theseus_eval == Rust reference \
         across the δ=3 branch-taken come-from program + BR=0 fall-through + P4/scratch grid, {:.2}s.",
        t0.elapsed().as_secs_f64()
    );
}

/// REVERSIBILITY (kept concrete): T forward steps then T adjoint steps return
/// exactly to the start state for the multi-step branch program. This is a
/// genuinely DIFFERENT obligation from the single-step datapath equivalence --
/// it is the whole-program bijection, cheap to check concretely, and NOT
/// expressible as a single-step BVEQ walk (it composes 5 steps + their
/// adjoints). Ported verbatim from branching.rs.
#[test]
fn branch_reversibility() {
    let (_rom, iso) = branch_taken_program();
    let start = zero_state();
    let mut cur = start.clone();
    let t_total = 5;
    for t in 0..t_total {
        cur = eval_state(&format!("rev/fwd[{t}]"), &iso, &cur);
    }
    let back = Iso::sym(iso.clone());
    let mut rev = cur;
    for k in (0..t_total).rev() {
        rev = eval_state(&format!("rev/back[{k}]"), &back, &rev);
    }
    assert_eq!(
        rev, start,
        "T forward + T adjoint must return exactly to start (branching program)"
    );
    eprintln!(
        "BRANCH-BVEQ reversibility: {t_total} forward steps (including the branch) + {t_total} \
         adjoint steps -> exact start state -- the multi-step bijection (kept concrete)."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// 3. NEGATIVE CONTROL -- wrong specs must be caught (teeth).
// ══════════════════════════════════════════════════════════════════════════

/// Walk a real SWAPBR r0 @pc=5 step and confirm the netlist output DIFFERS from
/// three WRONG specs: (1) swap direction reversed (rd'=old rd), (2) drop the +1
/// in PC, (3) branch offset from OLD BR instead of NEW BR (=old rd).
#[test]
fn branch_negative_control() {
    let layout = Layout::default();
    let pc: u16 = 5;
    let rd = 0usize;
    let mut prog: Vec<Instr> = Vec::new();
    for _ in 0..pc {
        prog.push(Instr::Xori { rd: 7, imm: 0 });
    }
    prog.push(Instr::Swapbr {
        rd: rd as u8,
        rs: 0,
    });
    let rom = assemble(&prog).unwrap();

    let WalkResult { out, vs, .. } = walk_step(&layout, &rom, pc);
    let nf = n_fields(&layout);
    let rin = |i: usize| field_bits(&vs, nf, layout.reg_field(i));
    let br_in = field_bits(&vs, nf, layout.br_field());
    let rd_in = rin(rd);

    let rd_out = out_field_bits(&out, &layout, layout.reg_field(rd));
    let out_pc = out_pc_bits(&out, &layout);
    let pc_in = const_bits(pc, layout.w_pc, &vs);

    // WRONG-1: swap direction reversed -- rd' should be OLD BR; a spec that says
    // rd' = OLD rd (no swap) must DIFFER on the domain rd != BR.
    let differs_dir = (0..W).any(|i| rd_out[i] != rd_in[i]);
    assert!(
        differs_dir,
        "NEG-1: SWAPBR rd' (=old BR) must DIFFER from a no-swap spec (rd'=old rd) -- teeth"
    );
    // Non-vacuous: the difference is satisfiable (there exist rd != BR).
    {
        let mut any = vs.mk_false();
        for i in 0..W {
            any = any.or(&rd_out[i].xor(&rd_in[i]));
        }
        assert!(
            any.sat_witness().is_some(),
            "NEG-1: swap-direction diff must be satisfiable"
        );
    }

    // WRONG-2: drop the +1 in the PC update -- PC' = pc + sext(old rd) (no +1)
    // must DIFFER.
    let sext: Vec<Bdd> = (0..layout.w_pc)
        .map(|i| {
            if i < W {
                rd_in[i].clone()
            } else {
                rd_in[W - 1].clone()
            }
        })
        .collect();
    let no_plus_one = ripple_sum(&pc_in, &sext, &vs);
    let differs_p1 = (0..layout.w_pc).any(|i| out_pc[i] != no_plus_one[i]);
    assert!(
        differs_p1,
        "NEG-2: SWAPBR PC' must DIFFER from a drop-+1 spec -- teeth"
    );

    // WRONG-3: branch offset taken from OLD BR (=br_in) instead of NEW BR
    // (=old rd). PC' = pc + sext(old BR) + 1 must DIFFER on the domain rd != BR.
    let wrong_off = spec_pc_out(&pc_in, &br_in, layout.w_pc, &vs);
    let differs_off = (0..layout.w_pc).any(|i| out_pc[i] != wrong_off[i]);
    assert!(
        differs_off,
        "NEG-3: SWAPBR PC' (offset = NEW BR = old rd) must DIFFER from offset-from-OLD-BR -- teeth"
    );

    eprintln!(
        "BRANCH-BVEQ negative control: SWAPBR step != {{no-swap rd', drop-+1 PC, offset-from-old-BR}} \
         -- the oracle has TEETH (with a satisfiable swap-direction counterexample)."
    );
}
