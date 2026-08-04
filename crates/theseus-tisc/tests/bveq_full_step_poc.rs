//! BVEQ-FULL-STEP-POC -- the PIVOTAL go/no-go experiment.
//!
//! Prove the FULL combinational CPU step netlist `step_with(&layout, &rom)`
//! ≡ its step-spec, ONCE, SYMBOLICALLY over the whole reachable state domain,
//! via the repo's BDD machinery (`bveq_netlist::eval_bdd`).
//!
//! `step_with = fetch ; decode_execute_l ; unfetch ; pc_add_br ; pc_inc` -- all
//! combinational (no Trace/Iter/Fold), so it is INSIDE `eval_bdd`'s fragment.
//! If this is tractable it is the master key: it retires the ~3.5 h decoder-class
//! brute-force sweeps AND (with induction over steps) the multi-step + ~4 h
//! real-CPU authority runs.
//!
//! ── The instance ────────────────────────────────────────────────────────
//! * PC = CONCRETE 0  → `fetch` reads a CONSTANT rom word (no PC-mux blow-up).
//! * IR = CONCRETE 0  → matches the pre-fetch machine (fetch XORs rom[0] in).
//! * register file = SYMBOLIC, BR = SYMBOLIC.
//! * s0 = s1 = CONSTANT 0 (decoder scratch convention; the PC adder + Addi/Subi
//!   immediate mask require it, and it is restored on exit).
//!
//! ── The spec (independent of the netlist) ───────────────────────────────
//! For a rom of ONE instruction at PC=0, the step must compute, over the whole
//! symbolic (regs, BR) domain:
//!   * out-regs = `instr_semantics(op)` applied to in-regs (BDD ripple specs);
//!   * PC' = (PC(0) + sext(BR) + 1) mod 2^w_pc  (ADR-0034; reuses bveq_step_poc);
//!   * BR preserved; IR back to 0 (unfetch); s0 = s1 = 0 restored;
//!   * every untouched reg = identity (no stray write / mis-route).
//!
//! ── Authority tiers ─────────────────────────────────────────────────────
//! 1. SYMBOLIC (whole-domain): the assertions below, per instruction type.
//! 2. REALITY ANCHOR: the SAME `step_with` iso run through the concrete
//!    `theseus_eval::eval` on a handful of register points equals the spec.
//! 3. NEGATIVE CONTROL: a WRONG spec (wrong op, or drop-the-+1) must DIFFER.

#![cfg(feature = "heavy-tests")]
// HEAVY tier: run under `--features heavy-tests --test-threads=1`.

use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::{Iso, Type};

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::step::step_with;
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_SCRATCH, W};

// ══════════════════════════════════════════════════════════════════════════
// Symbolic CpuState construction (bit-interleaved varmap, same as bveq_step_poc)
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

/// Fields that must enter CONCRETE for this instance:
///   PC_lo, PC_hi, IR bytes, s0, s1 → constant 0.
/// (PC concrete makes fetch a const-select; IR concrete matches the pre-fetch
/// machine; s0/s1=0 is the standing decoder scratch convention.)
fn is_concrete_zero(layout: &Layout, f: usize) -> bool {
    if f == layout.pc_field() {
        return true;
    }
    if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
        return true;
    }
    for k in 0..4 {
        if f == layout.ir_byte_field(k) {
            return true;
        }
    }
    // s0/s1 only: the decoder-reserved operand/predicate buffers. s2.. stay
    // SYMBOLIC (DATA operand space; must be restored -- asserted identity).
    f == layout.scratch_field(0) || f == layout.scratch_field(1)
}

fn const_zero_word8(vs: &BddVariableSet) -> SV {
    let mut sv = SV::Bit(vs.mk_false());
    for _ in 0..W - 1 {
        sv = SV::pair(SV::Bit(vs.mk_false()), sv);
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
fn field_word8(vs: &BddVariableSet, nf: usize, layout: &Layout, f: usize) -> SV {
    if is_concrete_zero(layout, f) {
        const_zero_word8(vs)
    } else {
        symbolic_word8(vs, nf, f)
    }
}
fn build_state_fields(
    ty: &Type,
    vs: &BddVariableSet,
    nf: usize,
    layout: &Layout,
    field: &mut usize,
) -> SV {
    match ty {
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
/// Build the symbolic CpuState SV mirroring `state_ty_with(&layout)`.
fn symbolic_state_sv(layout: &Layout, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    let ty = state_ty_with(layout);
    let mut field = 0usize;
    build_state_fields(&ty, vs, nf, layout, &mut field)
}

// ── Symbolic-PC variant: PC_lo SYMBOLIC (the PC-mux blow-up risk) ─────────
// Same as symbolic_state_sv but PC_lo is symbolic (PC_hi stays 0, so PC ∈
// [0,255]); IR=0, s0/s1=0. This exercises the fetch PC-MUX over symbolic PC.

fn is_concrete_zero_pcsym(layout: &Layout, f: usize) -> bool {
    // PC_lo now SYMBOLIC; everything else as before.
    if f == layout.pc_field() {
        return false;
    }
    is_concrete_zero(layout, f)
}
fn field_word8_pcsym(vs: &BddVariableSet, nf: usize, layout: &Layout, f: usize) -> SV {
    if is_concrete_zero_pcsym(layout, f) {
        const_zero_word8(vs)
    } else {
        symbolic_word8(vs, nf, f)
    }
}
fn build_state_fields_pcsym(
    ty: &Type,
    vs: &BddVariableSet,
    nf: usize,
    layout: &Layout,
    field: &mut usize,
) -> SV {
    match ty {
        _ if is_word8(ty) => {
            let sv = field_word8_pcsym(vs, nf, layout, *field);
            *field += 1;
            sv
        }
        Type::Prod(a, rest) if is_word8(a) => {
            let head = field_word8_pcsym(vs, nf, layout, *field);
            *field += 1;
            let tail = build_state_fields_pcsym(rest, vs, nf, layout, field);
            SV::pair(head, tail)
        }
        other => panic!("CpuState field not Word8-headed: {other:?}"),
    }
}
fn symbolic_state_sv_pcsym(layout: &Layout, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    let ty = state_ty_with(layout);
    let mut field = 0usize;
    build_state_fields_pcsym(&ty, vs, nf, layout, &mut field)
}

// ══════════════════════════════════════════════════════════════════════════
// Ripple-carry BDD specs (like bveq_step_poc / wide_add's spec_sum_bdds)
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
/// `(a - b) mod 2^len` = `a + (~b) + 1`.
fn ripple_sub(a: &[Bdd], b: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let notb: Vec<Bdd> = b.iter().map(|x| x.not()).collect();
    ripple_plus_one(&ripple_sum(a, &notb, vs), vs)
}

/// PC' spec = (PC + sext(BR) + 1) mod 2^w_pc. `pc`: w_pc bits, `br`: 8 bits.
fn spec_pc_out(pc: &[Bdd], br: &[Bdd], w_pc: usize, vs: &BddVariableSet) -> Vec<Bdd> {
    assert_eq!(pc.len(), w_pc);
    assert_eq!(br.len(), W);
    let sext: Vec<Bdd> = (0..w_pc)
        .map(|i| {
            if i < W {
                br[i].clone()
            } else {
                br[W - 1].clone()
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
// Register-output spec per instruction (BDD level). Returns, for each reg
// field, the expected output bit BDDs. `rf(i)` = input bits of reg i.
// ══════════════════════════════════════════════════════════════════════════

/// Compute the expected reg-output BDDs for `instr`, given a closure `rin`
/// returning the 8 input bit BDDs of register `i`. Only registers that CHANGE
/// are returned (as (reg_index, expected_bits)); untouched regs are asserted
/// identity separately.
fn reg_spec(
    instr: &Instr,
    rin: &dyn Fn(usize) -> Vec<Bdd>,
    vs: &BddVariableSet,
) -> Vec<(usize, Vec<Bdd>)> {
    match *instr {
        Instr::Xor { rd, rs } => {
            let a = rin(rd as usize);
            let b = rin(rs as usize);
            let out: Vec<Bdd> = (0..W).map(|i| a[i].xor(&b[i])).collect();
            vec![(rd as usize, out)]
        }
        Instr::Add { rd, rs } => {
            let out = ripple_sum(&rin(rd as usize), &rin(rs as usize), vs);
            vec![(rd as usize, out)]
        }
        Instr::Sub { rd, rs } => {
            // r[rd] -= r[rs]
            let out = ripple_sub(&rin(rd as usize), &rin(rs as usize), vs);
            vec![(rd as usize, out)]
        }
        Instr::Xori { rd, imm } => {
            let a = rin(rd as usize);
            let out: Vec<Bdd> = (0..W)
                .map(|i| {
                    if (imm >> i) & 1 == 1 {
                        a[i].not()
                    } else {
                        a[i].clone()
                    }
                })
                .collect();
            vec![(rd as usize, out)]
        }
        Instr::Ccnot { rd, rs, rt } => {
            // r[rd] ^= r[rs] & r[rt] (byte-parallel)
            let a = rin(rd as usize);
            let b = rin(rs as usize);
            let c = rin(rt as usize);
            let out: Vec<Bdd> = (0..W).map(|i| a[i].xor(&b[i].and(&c[i]))).collect();
            vec![(rd as usize, out)]
        }
        _ => panic!("reg_spec: instruction not covered: {instr:?}"),
    }
}

/// The set of register indices `instr` writes (all others must be identity).
fn written_regs(instr: &Instr) -> Vec<usize> {
    match *instr {
        Instr::Xor { rd, .. }
        | Instr::Add { rd, .. }
        | Instr::Sub { rd, .. }
        | Instr::Xori { rd, .. }
        | Instr::Ccnot { rd, .. } => vec![rd as usize],
        _ => panic!("written_regs: instruction not covered: {instr:?}"),
    }
}

// ══════════════════════════════════════════════════════════════════════════
// The walk + comparison
// ══════════════════════════════════════════════════════════════════════════

struct WalkResult {
    out: SV,
    vs: BddVariableSet,
    walk_s: f64,
}

/// Build `step_with(&layout, &rom)`, symbolic input, walk it. Reports the exact
/// eval_bdd error location if the step leaves the fragment (a critical finding).
fn walk_step(layout: &Layout, rom: &[u32]) -> Result<WalkResult, String> {
    let iso: Iso = step_with(layout, rom);
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let sv_in = symbolic_state_sv(layout, &vs);
    let t0 = Instant::now();
    let mut ctx = WalkCtx::new();
    let out = eval_bdd(&iso, sv_in, &vs, &mut ctx).map_err(|e| format!("{e}"))?;
    let walk_s = t0.elapsed().as_secs_f64();
    Ok(WalkResult { out, vs, walk_s })
}

/// Prove `step_with(layout, [instr])` at PC=0 ≡ the one-instruction step spec,
/// whole symbolic (regs, BR) domain. Returns (walk_s, compare_s).
fn prove_one_instr(layout: &Layout, instr: Instr) -> (f64, f64) {
    let rom = assemble(&[instr]).expect("assemble one instr");
    assert_eq!(rom.len(), 1, "single-instruction rom");
    let WalkResult { out, vs, walk_s } =
        walk_step(layout, &rom).unwrap_or_else(|e| panic!("eval_bdd FAILED on step_with: {e}"));
    let nf = n_fields(layout);

    let t0 = Instant::now();

    // Input reg bits closure.
    let rin = |i: usize| field_bits(&vs, nf, layout.reg_field(i));
    let br_in = field_bits(&vs, nf, layout.br_field());

    // ── Register outputs ≡ instruction spec ──
    let specs = reg_spec(&instr, &rin, &vs);
    for (r, expected) in &specs {
        let got = out_field_bits(&out, layout, layout.reg_field(*r));
        for i in 0..W {
            assert!(
                got[i] == expected[i],
                "{instr:?}: reg r{r} out bit {i} != instr spec"
            );
        }
    }

    // ── Untouched registers = identity ──
    let written = written_regs(&instr);
    for r in 0..layout.num_regs {
        if written.contains(&r) {
            continue;
        }
        let got = out_field_bits(&out, layout, layout.reg_field(r));
        let inp = rin(r);
        for i in 0..W {
            assert!(
                got[i] == inp[i],
                "{instr:?}: untouched reg r{r} out bit {i} != identity (stray write)"
            );
        }
    }

    // ── PC' ≡ (PC(0) + sext(BR) + 1) ──
    let pc_in: Vec<Bdd> = vec![vs.mk_false(); layout.w_pc]; // PC = 0 concrete
    let spec_pc = spec_pc_out(&pc_in, &br_in, layout.w_pc, &vs);
    let out_pc = out_pc_bits(&out, layout);
    for i in 0..layout.w_pc {
        assert!(
            out_pc[i] == spec_pc[i],
            "{instr:?}: PC out bit {i} != (0 + sext(BR) + 1) spec"
        );
    }

    // ── BR preserved ──
    let br_out = out_field_bits(&out, layout, layout.br_field());
    for i in 0..W {
        assert!(
            br_out[i] == br_in[i],
            "{instr:?}: BR out bit {i} not preserved"
        );
    }

    // ── IR back to 0 (unfetch), s0/s1 restored to 0 ──
    let zero = vs.mk_false();
    for k in 0..4 {
        let ir = out_field_bits(&out, layout, layout.ir_byte_field(k));
        for i in 0..W {
            assert!(
                ir[i] == zero,
                "{instr:?}: IR byte {k} bit {i} not re-zeroed (unfetch)"
            );
        }
    }
    for s in 0..2 {
        let so = out_field_bits(&out, layout, layout.scratch_field(s));
        for i in 0..W {
            assert!(so[i] == zero, "{instr:?}: s{s} bit {i} not restored to 0");
        }
    }

    // ── s2.. scratch = identity (symbolic input, must be restored) ──
    // s2.. are DATA operand space the Xchg crossbar butterfly routes through;
    // a correct reversible step must return each to its symbolic input.
    for s in 2..NUM_SCRATCH {
        let so = out_field_bits(&out, layout, layout.scratch_field(s));
        let si = field_bits(&vs, nf, layout.scratch_field(s));
        for i in 0..W {
            assert!(
                so[i] == si[i],
                "{instr:?}: s{s} bit {i} not restored (crossbar residue)"
            );
        }
    }

    let compare_s = t0.elapsed().as_secs_f64();
    (walk_s, compare_s)
}

// ══════════════════════════════════════════════════════════════════════════
// Reality anchor: SAME iso through concrete eval on a few reg points = spec.
// ══════════════════════════════════════════════════════════════════════════

const EVAL_FUEL: u64 = 8_000_000_000;

fn concrete_anchor(layout: &Layout, instr: Instr, reg_pts: &[[u8; 8]], brs: &[u8]) {
    let rom = assemble(&[instr]).expect("assemble");
    let iso = step_with(layout, &rom);
    for &br in brs {
        for regs8 in reg_pts {
            let mut regs = [0u8; theseus_tisc::types::NUM_REGS];
            let k = regs.len().min(8);
            regs[..k].copy_from_slice(&regs8[..k]);
            let s = StateVals {
                regs,
                pc: 0,
                br: u16::from(br),
                ir: 0,
                scratch: [0u8; NUM_SCRATCH],
                mem: [0u8; MEM_WORDS],
            };
            let mut fuel = EVAL_FUEL;
            let ev = theseus_eval::eval(&iso, s.encode_with(layout), &mut fuel)
                .unwrap_or_else(|e| panic!("anchor eval failed: {e:?}"));
            let got = StateVals::decode_with(layout, &ev);

            // Expected registers per op.
            let mut exp = regs;
            match instr {
                Instr::Xor { rd, rs } => exp[rd as usize] ^= exp[rs as usize],
                Instr::Add { rd, rs } => {
                    exp[rd as usize] = exp[rd as usize].wrapping_add(exp[rs as usize])
                }
                Instr::Sub { rd, rs } => {
                    exp[rd as usize] = exp[rd as usize].wrapping_sub(exp[rs as usize])
                }
                Instr::Xori { rd, imm } => exp[rd as usize] ^= imm,
                Instr::Ccnot { rd, rs, rt } => {
                    exp[rd as usize] ^= exp[rs as usize] & exp[rt as usize]
                }
                _ => panic!("anchor: op not covered"),
            }
            assert_eq!(
                got.regs, exp,
                "{instr:?}: real eval regs != spec (br={br:#04x})"
            );
            // PC' = (0 + sext(br) + 1) mod 2^w_pc.
            let mask: u16 = if layout.w_pc >= 16 {
                u16::MAX
            } else {
                (1u16 << layout.w_pc) - 1
            };
            let sext = (i16::from(br as i8)) as u16;
            let exp_pc = sext.wrapping_add(1) & mask;
            assert_eq!(
                got.pc, exp_pc,
                "{instr:?}: real eval PC' != spec (br={br:#04x})"
            );
            assert_eq!(got.br, u16::from(br), "{instr:?}: BR preserved");
            assert_eq!(got.ir, 0, "{instr:?}: IR re-zeroed");
            assert_eq!(got.scratch[0], 0, "{instr:?}: s0 restored");
            assert_eq!(got.scratch[1], 0, "{instr:?}: s1 restored");
        }
    }
}

// ══════════════════════════════════════════════════════════════════════════
// Negative control: a WRONG spec must be caught (teeth).
// ══════════════════════════════════════════════════════════════════════════

fn negative_control(layout: &Layout) {
    // Walk a real XOR r1,r0 step.
    let rom = assemble(&[Instr::Xor { rd: 1, rs: 0 }]).unwrap();
    let WalkResult { out, vs, .. } = walk_step(layout, &rom).expect("walk");
    let nf = n_fields(layout);
    let rin = |i: usize| field_bits(&vs, nf, layout.reg_field(i));
    let br_in = field_bits(&vs, nf, layout.br_field());

    // WRONG-1: wrong op (ADD instead of XOR) -- must differ on r1.
    let wrong_add = ripple_sum(&rin(1), &rin(0), &vs);
    let got_r1 = out_field_bits(&out, layout, layout.reg_field(1));
    let differs_op = (0..W).any(|i| got_r1[i] != wrong_add[i]);
    assert!(
        differs_op,
        "NEG: XOR step must DIFFER from an ADD spec (teeth)"
    );

    // WRONG-2: drop the +1 in the PC update -- must differ on PC.
    let pc_in: Vec<Bdd> = vec![vs.mk_false(); layout.w_pc];
    let sext: Vec<Bdd> = (0..layout.w_pc)
        .map(|i| {
            if i < W {
                br_in[i].clone()
            } else {
                br_in[W - 1].clone()
            }
        })
        .collect();
    let wrong_pc = ripple_sum(&pc_in, &sext, &vs); // no +1
    let got_pc = out_pc_bits(&out, layout);
    let differs_pc = (0..layout.w_pc).any(|i| got_pc[i] != wrong_pc[i]);
    assert!(
        differs_pc,
        "NEG: step PC must DIFFER from a drop-+1 spec (teeth)"
    );
}

// ══════════════════════════════════════════════════════════════════════════
// TESTS -- start minimal, measure, scale.
// ══════════════════════════════════════════════════════════════════════════

/// The MINIMAL instance: w_pc=8, one XOR. This is the go/no-go headline.
#[test]
fn full_step_xor_minimal_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let (walk_s, cmp_s) = prove_one_instr(&layout, Instr::Xor { rd: 1, rs: 0 });
    eprintln!(
        "FULL-STEP POC [w_pc=8, XOR r1,r0]: step_with ≡ step-spec over the WHOLE \
         symbolic (regs,BR) domain [PC=0,IR=0 concrete; s0/s1=0] -- eval_bdd walk {walk_s:.3}s + \
         BDD compare {cmp_s:.3}s = {:.3}s total. (decoder-class brute force baseline: ~hours.)",
        walk_s + cmp_s
    );
}

/// Scale: representative instruction types at w_pc=8.
#[test]
fn full_step_instr_family_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let cases = [
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Add { rd: 1, rs: 0 },
        Instr::Sub { rd: 1, rs: 0 },
        Instr::Xori { rd: 0, imm: 0xA3 },
        Instr::Ccnot {
            rd: 2,
            rs: 0,
            rt: 1,
        },
    ];
    for instr in cases {
        let (walk_s, cmp_s) = prove_one_instr(&layout, instr);
        eprintln!(
            "FULL-STEP POC [w_pc=8, {instr:?}]: step_with ≡ step-spec -- walk {walk_s:.3}s + \
             compare {cmp_s:.3}s = {:.3}s.",
            walk_s + cmp_s
        );
    }
}

/// Scale: the full 16-bit PC step (default layout), one XOR. Same domain-flat
/// BDD claim as bveq_step_poc -- measures the decode+execute crossbar cost at
/// the shipped PC width.
#[test]
fn full_step_xor_wpc16() {
    let layout = Layout::default();
    assert_eq!(layout.w_pc, 16);
    let (walk_s, cmp_s) = prove_one_instr(&layout, Instr::Xor { rd: 1, rs: 0 });
    eprintln!(
        "FULL-STEP POC [w_pc=16, XOR r1,r0]: step_with ≡ step-spec -- walk {walk_s:.3}s + \
         compare {cmp_s:.3}s = {:.3}s.",
        walk_s + cmp_s
    );
}

/// Reality anchor: SAME step iso through concrete eval == spec (a few points).
#[test]
fn full_step_reality_anchor_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let pts = [
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0x2A, 0xC3, 0x01, 0, 0, 0, 0, 0x5A],
        [0xFF, 0xFF, 0xFF, 0, 0, 0, 0, 0],
        [0x80, 0x7F, 0x11, 0, 0, 0, 0, 0],
    ];
    let brs = [0u8, 1, 0x7F, 0x80, 0xFF];
    for instr in [
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Add { rd: 1, rs: 0 },
        Instr::Sub { rd: 1, rs: 0 },
        Instr::Xori { rd: 0, imm: 0xA3 },
        Instr::Ccnot {
            rd: 2,
            rs: 0,
            rt: 1,
        },
    ] {
        concrete_anchor(&layout, instr, &pts, &brs);
    }
    eprintln!("FULL-STEP reality anchor [w_pc=8]: real theseus_eval == spec across the op family -- the reality anchor.");
}

/// SCALE -- SYMBOLIC PC (the PC-mux, the real blow-up risk for the inductive
/// multi-step proof). Walk the full step with PC_lo SYMBOLIC over a 2-instr rom
/// [XOR r1,r0 @0 ; ADD r1,r0 @1], regs+BR+s2.. symbolic. Then verify by
/// CO-FACTORING (BDD.restrict) the symbolic output at PC=0 and PC=1: at PC=0 the
/// step must be the XOR step, at PC=1 the ADD step, and at PC=2 (out of ROM) a
/// NOP-fetch (IR stays 0 → regs identity, PC just += sext(BR)+1). Reports peak
/// output-BDD node counts -- the tractability metric for symbolic PC.
#[test]
fn full_step_symbolic_pc_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let rom = assemble(&[Instr::Xor { rd: 1, rs: 0 }, Instr::Add { rd: 1, rs: 0 }])
        .expect("assemble 2-instr rom");
    assert_eq!(rom.len(), 2);
    let iso: Iso = step_with(&layout, &rom);

    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let sv_in = symbolic_state_sv_pcsym(&layout, &vs);

    let t0 = Instant::now();
    let mut ctx = WalkCtx::new();
    let out = eval_bdd(&iso, sv_in, &vs, &mut ctx)
        .unwrap_or_else(|e| panic!("eval_bdd FAILED on symbolic-PC step_with: {e}"));
    let walk_s = t0.elapsed().as_secs_f64();

    // Peak output BDD node counts (tractability metric).
    let out_r1 = out_field_bits(&out, &layout, layout.reg_field(1));
    let out_pc = out_pc_bits(&out, &layout);
    let peak_r1 = out_r1.iter().map(|b| b.size()).max().unwrap_or(0);
    let peak_pc = out_pc.iter().map(|b| b.size()).max().unwrap_or(0);

    // PC input variables (LSB..MSB of PC_lo).
    let pc_vars: Vec<_> = (0..W)
        .map(|b| vs.variables()[b * nf + layout.pc_field()])
        .collect();
    // Restrict PC to a concrete value.
    let pc_val = |v: u8| -> Vec<(biodivine_lib_bdd::BddVariable, bool)> {
        (0..W).map(|b| (pc_vars[b], (v >> b) & 1 == 1)).collect()
    };

    let rin = |i: usize| field_bits(&vs, nf, layout.reg_field(i));
    let br_in = field_bits(&vs, nf, layout.br_field());
    // sext(BR)+1 spec at PC=p: (p + sext(BR) + 1) mod 2^8.
    let pc_spec_at = |p: u8| -> Vec<Bdd> {
        let pcb: Vec<Bdd> = (0..W)
            .map(|b| {
                if (p >> b) & 1 == 1 {
                    vs.mk_true()
                } else {
                    vs.mk_false()
                }
            })
            .collect();
        spec_pc_out(&pcb, &br_in, layout.w_pc, &vs)
    };

    // ── PC=0: XOR step ──
    let r1_xor: Vec<Bdd> = (0..W).map(|i| rin(1)[i].xor(&rin(0)[i])).collect();
    for i in 0..W {
        assert!(
            out_r1[i].restrict(&pc_val(0)) == r1_xor[i].restrict(&pc_val(0)),
            "sym-PC @PC=0: r1 bit {i} != XOR spec"
        );
        assert!(
            out_pc[i].restrict(&pc_val(0)) == pc_spec_at(0)[i].restrict(&pc_val(0)),
            "sym-PC @PC=0: PC bit {i} != (0+sext(BR)+1)"
        );
    }
    // ── PC=1: ADD step ──
    let r1_add = ripple_sum(&rin(1), &rin(0), &vs);
    for i in 0..W {
        assert!(
            out_r1[i].restrict(&pc_val(1)) == r1_add[i].restrict(&pc_val(1)),
            "sym-PC @PC=1: r1 bit {i} != ADD spec"
        );
        assert!(
            out_pc[i].restrict(&pc_val(1)) == pc_spec_at(1)[i].restrict(&pc_val(1)),
            "sym-PC @PC=1: PC bit {i} != (1+sext(BR)+1)"
        );
    }
    // ── PC=2 (out of ROM): NOP fetch → r1 identity, PC += sext(BR)+1 ──
    for i in 0..W {
        assert!(
            out_r1[i].restrict(&pc_val(2)) == rin(1)[i].restrict(&pc_val(2)),
            "sym-PC @PC=2: r1 bit {i} != identity (NOP fetch)"
        );
        assert!(
            out_pc[i].restrict(&pc_val(2)) == pc_spec_at(2)[i].restrict(&pc_val(2)),
            "sym-PC @PC=2: PC bit {i} != (2+sext(BR)+1)"
        );
    }

    eprintln!(
        "FULL-STEP POC [w_pc=8, SYMBOLIC PC, 2-instr rom]: step_with walks the fetch PC-MUX \
         over the WHOLE 256-value PC domain in {walk_s:.3}s; verified by co-factoring @PC∈{{0,1,2}} \
         (XOR/ADD/NOP). Peak output BDD nodes: r1={peak_r1}, PC={peak_pc}. \
         → symbolic PC is TRACTABLE (no mux blow-up)."
    );
}

/// Teeth: a wrong spec (wrong op, drop-+1) must be detected as inequivalent.
#[test]
fn full_step_negative_control_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    negative_control(&layout);
    eprintln!("FULL-STEP POC negative control [w_pc=8]: step != {{wrong-op, drop-+1}} -- the oracle has TEETH.");
}
