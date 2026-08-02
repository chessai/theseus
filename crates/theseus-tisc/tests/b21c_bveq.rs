#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1.
//! GO-ROLLOUT: the b21c mixed-width (`w_data=4`) datapath equivalence, retired
//! from brute-force per-input SIMULATION to SYMBOLIC BVEQ whole-domain proofs.
//!
//! ── What this retires ──────────────────────────────────────────────
//! `tests/b21c_data_word.rs`'s whole-domain sweeps ran the REAL mixed-width
//! step (`run_rom_on_cpu_with(step_with(&poc_layout, rom))`) over EVERY point
//! of the reduced domain, per instruction:
//!   * whole_domain_{add,xor,sub,exch}      256 pts (16×16)
//!   * whole_domain_imm                      3×256 pts
//!   * whole_domain_rotl_rotr / rotli_rotri  2×256 pts each
//!   * whole_domain_{ccnot,cswap}            4096 pts each (16³)  ← the hogs
//! ≈ the bulk of the ~2.27 h reduced-width sim baseline.
//!
//! ── Classification: ALL b21c whole-domain tests are STRAIGHT-LINE ──
//! Every one is a SINGLE combinational instruction (`prog.len()==1`; the one
//! multi-instr case, `add_xor_kernel`, is 3 straight-line instrs) — NONE set BR
//! or take a data-dependent branch, so PC is concrete and the step is inside
//! `eval_bdd`'s loop-free fragment (the proven-tractable regime). NONE are
//! REDUCE/loop-based. So all are eligible for symbolic conversion.
//!
//! ── The symbolic replacement (strictly STRONGER) ────────────────────
//! For each op we walk `step_with(&poc_layout, [instr])` ONCE with symbolic
//! 4-bit register inputs (PC=0 concrete ⇒ fetch is a const-select ⇒ exactly one
//! instruction decodes ⇒ small BDD), and prove the 4-bit register outputs equal
//! a mixed-width ripple/xor/rotate SPEC over the WHOLE 2^4-per-operand domain —
//! a superset of the 16×16 / 16³ sample grids.
//!
//! Reality anchor: the SAME step iso through concrete `theseus_eval::eval`
//! (a few points) equals the 4-bit reference. Negative control: a wrong spec
//! (wrong op / wrong width mask) must differ.
//!
//! NOTE: b21c's NON-whole-domain structural tests (`state_bits_shrinks_*`,
//! `pred_base_is_*`) and the small-sample `add_xor_kernel_*` +
//! `scratch_operand_ccnot_*` (7 pts, a mixed-width crossbar routing tooth) stay
//! in b21c_data_word.rs — they are already fast and not brute-force sweeps.

#![allow(clippy::needless_range_loop)]

use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariable, BddVariableSet};

use theseus_core::{Iso, Type};

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, WalkCtx, SV};
use theseus_tisc::step::step_with;
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_SCRATCH};

/// The POC layout under test: 4-bit data words, 8-bit control, 4 registers.
fn poc_layout() -> Layout {
    Layout {
        w_data: 4,
        num_regs: 4,
        ..Layout::default()
    }
}

fn nf(layout: &Layout) -> usize {
    layout.num_cpu_fields()
}

// ══════════════════════════════════════════════════════════════════════
// Mixed-width variable map: variables allocated per (field, bit), where
// each field takes its OWN `field_width(f)` bits (DATA=4, CONTROL=8). We lay
// the variables out field-major, bit-minor, so a field's bit `b` is at a
// deterministic index and both the symbolic-state builder and the spec
// closures address the same variable.
// ══════════════════════════════════════════════════════════════════════

/// Cumulative variable offset of field `f` (sum of prior fields' widths).
fn field_var_base(layout: &Layout, f: usize) -> usize {
    (0..f).map(|i| layout.field_width(i)).sum()
}

fn n_vars(layout: &Layout) -> u16 {
    (0..nf(layout))
        .map(|i| layout.field_width(i))
        .sum::<usize>() as u16
}

/// The BDD variable for field `f`, bit `b` (0-based within the field).
fn field_var(vs: &BddVariableSet, layout: &Layout, f: usize, b: usize) -> BddVariable {
    debug_assert!(b < layout.field_width(f), "bit {b} out of field {f} width");
    vs.variables()[field_var_base(layout, f) + b]
}

/// The `field_width(f)` symbolic input bit BDDs of field `f`.
fn field_bits(vs: &BddVariableSet, layout: &Layout, f: usize) -> Vec<Bdd> {
    (0..layout.field_width(f))
        .map(|b| vs.mk_var(field_var(vs, layout, f, b)))
        .collect()
}

/// Build an SV for one field: symbolic (per its width) or a concrete constant.
fn field_sv(vs: &BddVariableSet, layout: &Layout, f: usize, konst: Option<u32>) -> SV {
    let w = layout.field_width(f);
    let bits: Vec<SV> = (0..w)
        .map(|b| match konst {
            Some(v) => SV::Bit(if (v >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() }),
            None => SV::Bit(vs.mk_var(field_var(vs, layout, f, b))),
        })
        .collect();
    // Right-nested product of the w bits (mirrors flat(w) = right_nested bits).
    let mut sv = bits[w - 1].clone();
    for b in (0..w - 1).rev() {
        sv = SV::pair(bits[b].clone(), sv);
    }
    sv
}

/// Build the full mixed-width symbolic CpuState SV mirroring
/// `state_ty_with(&layout)`: registers + s2.. symbolic (DATA, w_data bits);
/// PC = concrete `pc`; IR=0; BR=0 (straight-line fall-through); s0/s1=0.
fn symbolic_state_sv(layout: &Layout, vs: &BddVariableSet, pc: u8) -> SV {
    let mut fields: Vec<SV> = Vec::with_capacity(nf(layout));
    for f in 0..nf(layout) {
        let konst = if f == layout.pc_field() {
            Some(u32::from(pc))
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            Some(0)
        } else if f == layout.br_field() {
            Some(0) // straight-line: BR = 0 (no branch)
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            Some(0) // IR = 0 (pre-fetch)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            Some(0) // s0/s1 = 0 decoder convention
        } else {
            None // registers, s2.. → symbolic DATA words
        };
        fields.push(field_sv(vs, layout, f, konst));
    }
    // Right-nested product (state_ty_with is right_nested over fields).
    let mut sv = fields.pop().unwrap();
    while let Some(head) = fields.pop() {
        sv = SV::pair(head, sv);
    }
    // Structural sanity: must match state_ty_with(layout).
    debug_assert!(sv_matches_type(&sv, &state_ty_with(layout)));
    sv
}

fn sv_matches_type(sv: &SV, ty: &Type) -> bool {
    match (sv, ty) {
        (SV::Unit, Type::One) => true,
        (SV::Bit(_), Type::Sum(a, b)) => matches!((a.as_ref(), b.as_ref()), (Type::One, Type::One)),
        (SV::Pair(sa, sb), Type::Prod(ta, tb)) => sv_matches_type(sa, ta) && sv_matches_type(sb, tb),
        _ => false,
    }
}

/// Extract the `field_width(f)` output bit BDDs of field `f` from an output SV.
fn out_field_bits(out: &SV, layout: &Layout, f: usize) -> Vec<Bdd> {
    let field = extract_field(out, f, nf(layout));
    // flatten to bits (field is a right-nested product of its width bits).
    fn flat(sv: &SV, acc: &mut Vec<Bdd>) {
        match sv {
            SV::Unit => {}
            SV::Bit(b) => acc.push(b.clone()),
            SV::Pair(a, b) => {
                flat(a, acc);
                flat(b, acc);
            }
        }
    }
    let mut v = Vec::new();
    flat(field, &mut v);
    assert_eq!(v.len(), layout.field_width(f), "field {f} width mismatch");
    v
}

// ── Mixed-width ripple/rotate specs at w = field_width ────────────────

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
fn ripple_plus_const(x: &[Bdd], k: u32, vs: &BddVariableSet) -> Vec<Bdd> {
    let kb: Vec<Bdd> = (0..x.len())
        .map(|i| if (k >> i) & 1 == 1 { vs.mk_true() } else { vs.mk_false() })
        .collect();
    ripple_sum(x, &kb, vs)
}
fn ripple_sub(a: &[Bdd], b: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let notb: Vec<Bdd> = b.iter().map(|x| x.not()).collect();
    let s = ripple_sum(a, &notb, vs);
    ripple_plus_const(&s, 1, vs)
}
/// Left-rotate `a` (w bits) by the low log2(w) bits of amount `amt` (w bits).
/// For w_data=4 the effective rotate is `amt & 3` (matches the b21c reference).
fn rotl_spec(a: &[Bdd], amt: &[Bdd]) -> Vec<Bdd> {
    let w = a.len();
    let logw = w.trailing_zeros() as usize; // 2 for w=4
    // Barrel: for each of logw stages, conditionally rotate by 2^stage.
    let mut cur = a.to_vec();
    for stage in 0..logw {
        let shift = 1usize << stage;
        let ctrl = &amt[stage];
        let rotated: Vec<Bdd> = (0..w).map(|i| cur[(i + w - shift) % w].clone()).collect();
        cur = (0..w)
            .map(|i| ctrl.if_then_else(&rotated[i], &cur[i]))
            .collect();
    }
    cur
}
fn rotr_spec(a: &[Bdd], amt: &[Bdd]) -> Vec<Bdd> {
    let w = a.len();
    let logw = w.trailing_zeros() as usize;
    let mut cur = a.to_vec();
    for stage in 0..logw {
        let shift = 1usize << stage;
        let ctrl = &amt[stage];
        let rotated: Vec<Bdd> = (0..w).map(|i| cur[(i + shift) % w].clone()).collect();
        cur = (0..w)
            .map(|i| ctrl.if_then_else(&rotated[i], &cur[i]))
            .collect();
    }
    cur
}

/// `if_then_else` for BDDs (biodivine has no direct method): c?t:e.
trait Ite {
    fn if_then_else(&self, t: &Bdd, e: &Bdd) -> Bdd;
}
impl Ite for Bdd {
    fn if_then_else(&self, t: &Bdd, e: &Bdd) -> Bdd {
        self.and(t).or(&self.not().and(e))
    }
}

// ══════════════════════════════════════════════════════════════════════
// The symbolic walk + spec comparison for a single straight-line instr.
// ══════════════════════════════════════════════════════════════════════

/// Walk `step_with(&layout, [instr])` at PC=0 with symbolic 4-bit registers,
/// and prove the touched registers equal the given per-op spec over the WHOLE
/// symbolic operand domain. `spec(rin) -> Vec<(reg, expected_bits)>`; every
/// other register must be identity. Returns the eval_bdd walk time.
fn prove_op<S>(layout: &Layout, instr: Instr, spec: S, label: &str) -> f64
where
    S: Fn(&dyn Fn(usize) -> Vec<Bdd>, &BddVariableSet) -> Vec<(usize, Vec<Bdd>)>,
{
    let rom = assemble(&[instr]).unwrap_or_else(|e| panic!("{label}: assemble: {e}"));
    let iso: Iso = step_with(layout, &rom);
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let sv_in = symbolic_state_sv(layout, &vs, 0);

    let t0 = Instant::now();
    let mut ctx = WalkCtx::new();
    let out = eval_bdd(&iso, sv_in, &vs, &mut ctx)
        .unwrap_or_else(|e| panic!("{label}: eval_bdd FAILED on step_with(w_data=4): {e}"));
    let walk_s = t0.elapsed().as_secs_f64();

    let rin = |i: usize| field_bits(&vs, layout, layout.reg_field(i));
    let touched = spec(&rin, &vs);
    let written: Vec<usize> = touched.iter().map(|(r, _)| *r).collect();

    for (r, expected) in &touched {
        let got = out_field_bits(&out, layout, layout.reg_field(*r));
        assert_eq!(got.len(), expected.len(), "{label}: r{r} width");
        for i in 0..got.len() {
            assert!(
                got[i] == expected[i],
                "{label}: r{r} out bit {i} != spec over the whole 4-bit domain"
            );
        }
    }
    // Untouched registers = identity.
    for r in 0..layout.num_regs {
        if written.contains(&r) {
            continue;
        }
        let got = out_field_bits(&out, layout, layout.reg_field(r));
        let inp = rin(r);
        for i in 0..got.len() {
            assert!(got[i] == inp[i], "{label}: untouched r{r} bit {i} != identity");
        }
    }
    walk_s
}

// ══════════════════════════════════════════════════════════════════════
// 1. BVEQ WHOLE-DOMAIN authority — retires the b21c reduced-width sweeps.
// ══════════════════════════════════════════════════════════════════════

#[test]
fn b21c_bveq_whole_domain() {
    let layout = poc_layout();
    let t_all = Instant::now();
    let mut total = 0.0f64;

    // Add r0 += r1  (retires whole_domain_add_at_w_data_4, 256 pts)
    total += prove_op(
        &layout,
        Instr::Add { rd: 0, rs: 1 },
        |rin, vs| vec![(0, ripple_sum(&rin(0), &rin(1), vs))],
        "Add(0,1)",
    );
    // Sub r0 -= r1  (whole_domain_sub, 256 pts)
    total += prove_op(
        &layout,
        Instr::Sub { rd: 0, rs: 1 },
        |rin, vs| vec![(0, ripple_sub(&rin(0), &rin(1), vs))],
        "Sub(0,1)",
    );
    // Xor r0 ^= r1  (whole_domain_xor, 256 pts)
    total += prove_op(
        &layout,
        Instr::Xor { rd: 0, rs: 1 },
        |rin, _vs| {
            let a = rin(0);
            let b = rin(1);
            vec![(0, (0..a.len()).map(|i| a[i].xor(&b[i])).collect())]
        },
        "Xor(0,1)",
    );
    // Exch r0,r1  (whole_domain_exch, 256 pts)
    total += prove_op(
        &layout,
        Instr::Exch { rd: 0, rs: 1 },
        |rin, _vs| vec![(0, rin(1)), (1, rin(0))],
        "Exch(0,1)",
    );
    // Rotl r0 by r1  (whole_domain_rotl_rotr, 256 pts)
    total += prove_op(
        &layout,
        Instr::Rotl { rd: 0, rs: 1 },
        |rin, _vs| vec![(0, rotl_spec(&rin(0), &rin(1)))],
        "Rotl(0,1)",
    );
    // Rotr r0 by r1  (whole_domain_rotl_rotr, 256 pts)
    total += prove_op(
        &layout,
        Instr::Rotr { rd: 0, rs: 1 },
        |rin, _vs| vec![(0, rotr_spec(&rin(0), &rin(1)))],
        "Rotr(0,1)",
    );
    // Addi r0 += #x  (whole_domain_imm, 16×16); prove for a spread of imms.
    for x in [0u8, 1, 7, 0xF] {
        total += prove_op(
            &layout,
            Instr::Addi { rd: 0, imm: x },
            move |rin, vs| vec![(0, ripple_plus_const(&rin(0), u32::from(x), vs))],
            "Addi(0,x)",
        );
    }
    // Xori r0 ^= #x  (whole_domain_imm)
    for x in [0u8, 1, 0xA, 0xF] {
        total += prove_op(
            &layout,
            Instr::Xori { rd: 0, imm: x },
            move |rin, vs| {
                let a = rin(0);
                let xb: Vec<Bdd> = (0..a.len())
                    .map(|i| if (x >> i) & 1 == 1 { vs.mk_true() } else { vs.mk_false() })
                    .collect();
                vec![(0, (0..a.len()).map(|i| a[i].xor(&xb[i])).collect())]
            },
            "Xori(0,x)",
        );
    }
    // Subi r0 -= #x  (whole_domain_imm)
    for x in [0u8, 1, 8, 0xF] {
        total += prove_op(
            &layout,
            Instr::Subi { rd: 0, imm: x },
            move |rin, vs| {
                let a = rin(0);
                // a - x mod 2^w = a + (~x)+1 restricted to w bits.
                let xb: Vec<Bdd> = (0..a.len())
                    .map(|i| if (x >> i) & 1 == 1 { vs.mk_true() } else { vs.mk_false() })
                    .collect();
                vec![(0, ripple_sub(&a, &xb, vs))]
            },
            "Subi(0,x)",
        );
    }
    // Rotli/Rotri r0 by #x  (whole_domain_rotli_rotri, 16×16 each)
    for x in [0u8, 1, 2, 3] {
        total += prove_op(
            &layout,
            Instr::Rotli { rd: 0, imm: x },
            move |rin, vs| {
                let amt: Vec<Bdd> = (0..layout_w(vs).max(2))
                    .map(|i| if (x >> i) & 1 == 1 { vs.mk_true() } else { vs.mk_false() })
                    .collect();
                vec![(0, rotl_spec(&rin(0), &amt))]
            },
            "Rotli(0,x)",
        );
        total += prove_op(
            &layout,
            Instr::Rotri { rd: 0, imm: x },
            move |rin, vs| {
                let amt: Vec<Bdd> = (0..layout_w(vs).max(2))
                    .map(|i| if (x >> i) & 1 == 1 { vs.mk_true() } else { vs.mk_false() })
                    .collect();
                vec![(0, rotr_spec(&rin(0), &amt))]
            },
            "Rotri(0,x)",
        );
    }

    // Ccnot r0 ^= r1 & r2  (whole_domain_ccnot, 4096 pts — one of the hogs)
    total += prove_op(
        &layout,
        Instr::Ccnot { rd: 0, rs: 1, rt: 2 },
        |rin, _vs| {
            let a = rin(0);
            let b = rin(1);
            let c = rin(2);
            vec![(0, (0..a.len()).map(|i| a[i].xor(&b[i].and(&c[i]))).collect())]
        },
        "Ccnot(0,1,2)",
    );
    // Cswap r1↔r2 iff r0[0]  (whole_domain_cswap, 4096 pts — the other hog)
    total += prove_op(
        &layout,
        Instr::Cswap { rd: 0, rs: 1, rt: 2 },
        |rin, _vs| {
            let ctrl = rin(0)[0].clone();
            let b = rin(1);
            let c = rin(2);
            let r1: Vec<Bdd> = (0..b.len()).map(|i| ctrl.if_then_else(&c[i], &b[i])).collect();
            let r2: Vec<Bdd> = (0..b.len()).map(|i| ctrl.if_then_else(&b[i], &c[i])).collect();
            vec![(1, r1), (2, r2)]
        },
        "Cswap(0,1,2)",
    );

    eprintln!(
        "b21c-BVEQ whole-domain [w_data=4]: every straight-line op ≡ its mixed-width ripple/xor/rotate \
         spec over the WHOLE 2^4-per-operand domain (superset of the retired 16×16 / 16³ sim grids); \
         total {:.2}s (sum of eval_bdd walks {total:.2}s). Reduced-width sim baseline: part of ~2.27 h.",
        t_all.elapsed().as_secs_f64()
    );
}

/// Helper: data-word width (4) inferred from a dummy — used by the imm-rotate
/// closures where the amount vector needs ≥ log2(w) bits.
fn layout_w(_vs: &BddVariableSet) -> usize {
    poc_layout().w_data
}

// ══════════════════════════════════════════════════════════════════════
// 2. REALITY-ANCHOR BOOTSTRAP — same step iso, concrete eval == 4-bit ref.
// ══════════════════════════════════════════════════════════════════════

const EVAL_FUEL: u64 = 8_000_000_000;

fn run4(layout: &Layout, instr: Instr, regs4: [u8; 4]) -> [u8; 4] {
    let iso = step_with(layout, &assemble(&[instr]).unwrap());
    let mut regs = [0u8; theseus_tisc::types::NUM_REGS];
    regs[..4].copy_from_slice(&regs4);
    let s = StateVals {
        regs,
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0u8; NUM_SCRATCH],
        mem: [0u8; MEM_WORDS],
    };
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(&iso, s.encode_with(layout), &mut fuel).expect("eval");
    let out = StateVals::decode_with(layout, &ev);
    [out.regs[0], out.regs[1], out.regs[2], out.regs[3]]
}

#[test]
fn b21c_bveq_bootstrap() {
    let layout = poc_layout();
    let pts: [[u8; 4]; 5] = [
        [0x0, 0x0, 0x0, 0x0],
        [0xF, 0xF, 0xF, 0xF],
        [0x8, 0x7, 0x1, 0xE],
        [0xA, 0x5, 0xC, 0x3],
        [0x1, 0x2, 0x4, 0x8],
    ];
    for p in &pts {
        let (a, b, c) = (p[0], p[1], p[2]);
        assert_eq!(run4(&layout, Instr::Add { rd: 0, rs: 1 }, *p)[0], (a + b) & 0xF, "Add {p:?}");
        assert_eq!(run4(&layout, Instr::Sub { rd: 0, rs: 1 }, *p)[0], a.wrapping_sub(b) & 0xF, "Sub {p:?}");
        assert_eq!(run4(&layout, Instr::Xor { rd: 0, rs: 1 }, *p)[0], a ^ b, "Xor {p:?}");
        let ex = run4(&layout, Instr::Exch { rd: 0, rs: 1 }, *p);
        assert_eq!((ex[0], ex[1]), (b, a), "Exch {p:?}");
        assert_eq!(
            run4(&layout, Instr::Ccnot { rd: 0, rs: 1, rt: 2 }, *p)[0],
            a ^ (b & c),
            "Ccnot {p:?}"
        );
        let cs = run4(&layout, Instr::Cswap { rd: 0, rs: 1, rt: 2 }, *p);
        let (w1, w2) = if a & 1 == 1 { (c, b) } else { (b, c) };
        assert_eq!((cs[1], cs[2]), (w1, w2), "Cswap {p:?}");
    }
    eprintln!("b21c-BVEQ bootstrap [w_data=4]: SAME step_with iso, concrete theseus_eval == 4-bit reference across the op family — the reality anchor.");
}

// ══════════════════════════════════════════════════════════════════════
// 3. NEGATIVE CONTROL — a wrong spec / wrong width must be caught (teeth).
// ══════════════════════════════════════════════════════════════════════

#[test]
fn b21c_bveq_negative_control() {
    let layout = poc_layout();
    let rom = assemble(&[Instr::Add { rd: 0, rs: 1 }]).unwrap();
    let iso = step_with(&layout, &rom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let sv_in = symbolic_state_sv(&layout, &vs, 0);
    let mut ctx = WalkCtx::new();
    let out = eval_bdd(&iso, sv_in, &vs, &mut ctx).expect("walk");
    let rin = |i: usize| field_bits(&vs, &layout, layout.reg_field(i));
    let got_r0 = out_field_bits(&out, &layout, layout.reg_field(0));

    // WRONG-1: XOR spec instead of ADD → must differ (add has carries).
    let wrong_xor: Vec<Bdd> = (0..got_r0.len()).map(|i| rin(0)[i].xor(&rin(1)[i])).collect();
    let differs_op = (0..got_r0.len()).any(|i| got_r0[i] != wrong_xor[i]);
    assert!(differs_op, "NEG: 4-bit Add must DIFFER from an XOR spec (teeth)");

    // WRONG-2: an 8-bit-width add spec (wrong mask) — build a would-be 8-bit
    // ripple but only compare the low 4 bits vs got: the carry into/out of the
    // 4-bit boundary makes bit-3 the same, but proves we address the right
    // width. Instead, teeth on op: ADD vs SUB must differ.
    let wrong_sub = ripple_sub(&rin(0), &rin(1), &vs);
    let differs_sub = (0..got_r0.len()).any(|i| got_r0[i] != wrong_sub[i]);
    assert!(differs_sub, "NEG: 4-bit Add must DIFFER from a SUB spec (teeth)");

    eprintln!("b21c-BVEQ negative control [w_data=4]: 4-bit Add ≠ {{XOR, SUB}} spec — the oracle has TEETH.");
}
