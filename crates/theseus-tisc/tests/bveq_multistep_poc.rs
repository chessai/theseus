//! BVEQ-MULTISTEP-POC -- the SECOND pivotal go/no-go experiment.
//!
//! `bveq_full_step_poc.rs` proved ONE full combinational step
//! `step_with(&layout, &rom)` ≡ its step-spec symbolically over the whole
//! reachable state domain, and that SYMBOLIC PC (the fetch mux over all 256
//! PC) stays tiny. This file COMPOSES that proven step over T steps --
//! replacing the per-input Rust loop in `run_rom_on_cpu` / `run_on_cpu`
//! (`cur = eval(step_iso, cur)` × budget) with a SYMBOLIC composition
//! (`sv = eval_bdd(step_iso, sv)` × budget), feeding each output `SV` as the
//! next input `SV`.
//!
//! THE UNKNOWN THIS FILE MEASURES: **how the state BDD grows as steps
//! compose.** After each composed step we record peak BDD node count + wall
//! time. Linear/plateau = GO (retires the ~2.27 h b21c sweeps + ~4 h real-CPU
//! authority + W_BR-b's paused real-cpu proof). Blow-up = NO-GO (report where).
//!
//! Two regimes:
//!   1. STRAIGHT-LINE ROM, PC CONCRETE (advances 0→1→...). The tractability
//!      probe: symbolic register file, PC concrete each step so fetch is a
//!      const-select. After T = rom-length steps the register BDDs must equal
//!      the composed instruction semantics.
//!   2. BOUNDED LOOP, PC SYMBOLIC. The prize: a come-from loop whose trip count
//!      varies per input. PC is symbolic (the loop is realized by the symbolic
//!      PC feeding back through `pc_add_br`), the input is symbolic, and we
//!      compose `step` up to the WCET budget. This is the regime the ~4 h
//!      real-cpu authority checks.
//!
//! Reality anchor: the SAME symbolic composition, restricted (co-factored) to
//! concrete inputs, equals `theseus_eval::eval`-driven `run_rom_on_cpu`.
//! Negative control: a wrong ROM / wrong spec must be caught.

#![cfg(feature = "heavy-tests")]
// HEAVY tier: run under `--features heavy-tests --test-threads=1`.

use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariable, BddVariableSet};

use theseus_core::Iso;

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::step::step_with;
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_SCRATCH, W};

// ══════════════════════════════════════════════════════════════════════════
// Symbolic / concrete state SV construction (bit-interleaved varmap).
//
// var(field f, bit b) = b*nf + f (VarMap convention, shared with the step POC).
// ══════════════════════════════════════════════════════════════════════════

fn n_fields(layout: &Layout) -> usize {
    layout.num_cpu_fields()
}
fn n_vars(layout: &Layout) -> u16 {
    (n_fields(layout) * W) as u16
}
fn field_bit_var(vs: &BddVariableSet, nf: usize, f: usize, b: usize) -> Bdd {
    vs.mk_var(vs.variables()[b * nf + f])
}
fn field_bits(vs: &BddVariableSet, nf: usize, f: usize) -> Vec<Bdd> {
    (0..W).map(|b| field_bit_var(vs, nf, f, b)).collect()
}
fn field_var(vs: &BddVariableSet, nf: usize, f: usize, b: usize) -> BddVariable {
    vs.variables()[b * nf + f]
}

fn const_word8(val: u8, vs: &BddVariableSet) -> SV {
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

/// Build a CpuState SV field-by-field from a per-field closure returning the SV
/// for CPU field `f`. Mirrors `state_ty_with(&layout)` (right-nested product of
/// `num_cpu_fields()` Word8 fields).
fn build_state<F: Fn(usize) -> SV>(layout: &Layout, mk: F) -> SV {
    let nf = n_fields(layout);
    // Right-nested product field 0 .. nf-1.
    let mut sv = mk(nf - 1);
    for f in (0..nf - 1).rev() {
        sv = SV::pair(mk(f), sv);
    }
    // Sanity: it must structurally match state_ty_with(layout). HARD assert (a
    // carrier-shape misalignment must fault even under --release).
    assert!(
        matches_type(&sv, layout),
        "build_state: SV shape != state_ty_with(layout)"
    );
    sv
}

fn matches_type(sv: &SV, layout: &Layout) -> bool {
    // Cheap structural check: flatten length == num_cpu_fields * W.
    fn count(sv: &SV) -> usize {
        match sv {
            SV::Unit => 0,
            SV::Bit(_) => 1,
            SV::Pair(a, b) => count(a) + count(b),
        }
    }
    let _ = state_ty_with(layout); // ensure it type-checks in scope
    count(sv) == n_fields(layout) * W
}

// ── Field extraction from an output SV ─────────────────────────────────────

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

// ── BDD size metric: total unique nodes across the whole state SV ───────────
//
// We report BOTH the peak per-field BDD size (max over all output bit BDDs) and
// the SUM over all bits (the honest "state BDD" footprint -- sharing across bits
// is NOT counted, so this is an upper bound on true memory but the faithful
// growth signal).

fn sv_bdd_sizes(sv: &SV) -> (usize, usize) {
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

// ══════════════════════════════════════════════════════════════════════════
// Ripple-carry BDD specs (shared with the step POC).
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
fn ripple_sub(a: &[Bdd], b: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let notb: Vec<Bdd> = b.iter().map(|x| x.not()).collect();
    let s = ripple_sum(a, &notb, vs);
    let mut one = vec![vs.mk_false(); s.len()];
    one[0] = vs.mk_true();
    ripple_sum(&s, &one, vs)
}

// ══════════════════════════════════════════════════════════════════════════
// The multi-step composition driver.
//
// Compose `step_with(layout, rom)` `n_steps` times, feeding each output SV as
// the next input SV, EXACTLY like `run_rom_on_cpu`'s `cur = eval(step, cur)`
// loop -- but symbolic. After every step, record (peak, sum, cumulative_secs).
// ══════════════════════════════════════════════════════════════════════════

struct StepSample {
    step: usize,
    peak: usize,
    sum: usize,
    secs: f64,
}

fn compose_steps(layout: &Layout, rom: &[u32], sv0: SV, n_steps: usize) -> (SV, Vec<StepSample>) {
    let iso: Iso = step_with(layout, rom);
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let mut ctx = WalkCtx::new();
    let mut cur = sv0;
    let mut samples = Vec::with_capacity(n_steps);
    let t0 = Instant::now();
    // Safety abort: a data-dependent loop's naive step-composition blows the BDD
    // up (measured: vart_halve hit >8 GB RSS). Bail the instant the state BDD
    // crosses a safe threshold, recording WHERE -- documents the blow-up without
    // OOMing the box (maestro-gate-no-timeout lesson). Combinational/straight-line
    // runs stay far under this (peak < 500 nodes), so they are unaffected.
    const BLOWUP_ABORT_NODES: usize = 200_000;
    for step in 1..=n_steps {
        cur = eval_bdd(&iso, cur, &vs, &mut ctx)
            .unwrap_or_else(|e| panic!("eval_bdd FAILED at composed step {step}: {e}"));
        let (peak, sum) = sv_bdd_sizes(&cur);
        samples.push(StepSample {
            step,
            peak,
            sum,
            secs: t0.elapsed().as_secs_f64(),
        });
        if peak > BLOWUP_ABORT_NODES {
            eprintln!(
                "BLOW-UP ABORT: state BDD hit {peak} nodes at composed step {step} \
                 (threshold {BLOWUP_ABORT_NODES}) -- naive full-composition of a \
                 data-dependent loop is intractable; needs the inductive-invariant method."
            );
            break;
        }
    }
    (cur, samples)
}

fn print_curve(tag: &str, samples: &[StepSample]) {
    eprintln!("=== BDD-GROWTH CURVE: {tag} ===");
    eprintln!("  step | peak-node | sum-nodes | cum-secs");
    for s in samples {
        eprintln!(
            "  {:>4} | {:>9} | {:>9} | {:>7.3}",
            s.step, s.peak, s.sum, s.secs
        );
    }
    let last = samples.last().unwrap();
    let first = samples.first().unwrap();
    eprintln!(
        "  → after {} steps: peak {} → {}, sum {} → {}, total {:.3}s",
        last.step, first.peak, last.peak, first.sum, last.sum, last.secs
    );
}

// ══════════════════════════════════════════════════════════════════════════
// Restrict a symbolic-state SV's INPUT variables to a concrete StateVals, so a
// symbolic output can be co-factored to a concrete point and compared to eval.
// ══════════════════════════════════════════════════════════════════════════

/// Build the (var, bool) assignment pinning every symbolic input field to the
/// concrete value in `s` (regs, BR; PC/IR/scratch handled by the caller's SV0).
fn restrict_regs_br(
    layout: &Layout,
    vs: &BddVariableSet,
    s: &StateVals,
) -> Vec<(BddVariable, bool)> {
    let nf = n_fields(layout);
    let mut asg = Vec::new();
    for r in 0..layout.num_regs {
        let val = s.regs[r];
        for b in 0..W {
            asg.push((
                field_var(vs, nf, layout.reg_field(r), b),
                (val >> b) & 1 == 1,
            ));
        }
    }
    // BR (8-bit low field).
    let br = s.br as u8;
    for b in 0..W {
        asg.push((field_var(vs, nf, layout.br_field(), b), (br >> b) & 1 == 1));
    }
    asg
}

// ══════════════════════════════════════════════════════════════════════════
// PART 1 -- STRAIGHT-LINE, PC CONCRETE (advances). The tractability probe.
// ══════════════════════════════════════════════════════════════════════════
//
// A straight-line ROM never sets BR, so PC advances 0→1→..., and every fetch
// reads a constant ROM word. We keep PC concrete each step: the input SV to
// step k has PC = concrete k. Because PC stays concrete, the fetch PC-mux
// collapses to a const-select -- this isolates the DATAPATH (reg-file) BDD
// growth, the pure question "does composing arithmetic steps blow up the regs".

/// Symbolic regs + BR=0 + concrete PC=pc; IR=0, s0/s1=0, s2.. symbolic.
fn straightline_sv(layout: &Layout, vs: &BddVariableSet, pc: u8) -> SV {
    let nf = n_fields(layout);
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8(pc, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(0, vs)
        } else if f == layout.br_field() {
            const_word8(0, vs) // straight-line: BR = 0 (fall-through)
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            const_word8(0, vs) // IR = 0 (pre-fetch)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            const_word8(0, vs) // s0/s1 = 0 decoder convention
        } else {
            symbolic_word8(vs, nf, f)
        }
    })
}

/// Compose a straight-line ROM WITH PC RE-PINNED CONCRETE each step. Because a
/// straight-line ROM leaves BR=0, the step's PC output is exactly PC+1; we feed
/// the datapath output forward but overwrite PC with the concrete next value.
/// This is faithful (the concrete PC the datapath produces IS pc+1) and keeps
/// the fetch a const-select so the measured growth is pure datapath growth.
fn compose_straightline(
    layout: &Layout,
    rom: &[u32],
    n_steps: usize,
) -> (SV, BddVariableSet, Vec<StepSample>) {
    let iso: Iso = step_with(layout, rom);
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let nf = n_fields(layout);
    let mut ctx = WalkCtx::new();
    let mut cur = straightline_sv(layout, &vs, 0);
    let mut samples = Vec::with_capacity(n_steps);
    let t0 = Instant::now();
    for step in 1..=n_steps {
        let out = eval_bdd(&iso, cur, &vs, &mut ctx)
            .unwrap_or_else(|e| panic!("eval_bdd FAILED at straight-line step {step}: {e}"));
        // Re-pin PC (and PC_hi) to the concrete next value so the following
        // fetch is a const-select; carry everything else forward.
        let next_pc = step as u8; // pc after k steps = k (BR=0)
        cur = build_state(layout, |f| {
            if f == layout.pc_field() {
                const_word8(next_pc, &vs)
            } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
                const_word8(0, &vs)
            } else {
                // Carry the datapath output SV for field f forward.
                let bits = out_field_bits(&out, layout, f);
                let mut sv = SV::Bit(bits[W - 1].clone());
                for b in (0..W - 1).rev() {
                    sv = SV::pair(SV::Bit(bits[b].clone()), sv);
                }
                sv
            }
        });
        let _ = nf;
        let (peak, sum) = sv_bdd_sizes(&cur);
        samples.push(StepSample {
            step,
            peak,
            sum,
            secs: t0.elapsed().as_secs_f64(),
        });
    }
    (cur, vs, samples)
}

/// The straight-line headline. A 4-instruction arithmetic ROM, PC concrete,
/// symbolic regs. Compose 4 steps; measure the reg-file BDD growth; verify the
/// final register BDDs equal the composed instruction semantics.
#[test]
fn multistep_straightline_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    // r1 ^= r0 ; r2 += r1 ; r1 -= r0 ; r2 ^= r3  (touches r1, r2; reads r0, r3)
    let prog = [
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Add { rd: 2, rs: 1 },
        Instr::Sub { rd: 1, rs: 0 },
        Instr::Xor { rd: 2, rs: 3 },
    ];
    let rom = assemble(&prog).expect("assemble straight-line rom");
    let (out, vs, samples) = compose_straightline(&layout, &rom, prog.len());
    print_curve(
        "straight-line 4-instr [w_pc=8, PC concrete, regs symbolic]",
        &samples,
    );

    // ── Verify final register BDDs == composed instruction semantics ──
    let nf = n_fields(&layout);
    let r = |i: usize| field_bits(&vs, nf, layout.reg_field(i));
    // Track expected reg BDDs through the program.
    let mut exp: Vec<Vec<Bdd>> = (0..layout.num_regs).map(r).collect();
    // step0: r1 ^= r0
    exp[1] = (0..W).map(|i| exp[1][i].xor(&exp[0][i])).collect();
    // step1: r2 += r1
    exp[2] = ripple_sum(&exp[2], &exp[1], &vs);
    // step2: r1 -= r0
    exp[1] = ripple_sub(&exp[1], &exp[0], &vs);
    // step3: r2 ^= r3
    exp[2] = (0..W).map(|i| exp[2][i].xor(&exp[3][i])).collect();

    for i in 0..layout.num_regs {
        let got = out_field_bits(&out, &layout, layout.reg_field(i));
        for b in 0..W {
            assert!(
                got[b] == exp[i][b],
                "straight-line: final r{i} bit {b} != composed semantics"
            );
        }
    }
    eprintln!(
        "STRAIGHT-LINE MULTISTEP [w_pc=8]: {} composed steps ≡ composed instruction semantics \
         over the WHOLE symbolic reg domain. Final peak {} nodes, {:.3}s.",
        prog.len(),
        samples.last().unwrap().peak,
        samples.last().unwrap().secs
    );
}

/// Longer straight-line run to expose the growth trend (does the reg BDD
/// plateau or creep?). 16 XOR/ADD/SUB instrs cycling over 4 regs.
#[test]
fn multistep_straightline_long_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let mut prog = Vec::new();
    for k in 0..16u8 {
        let rd = 1 + (k % 3); // rd ∈ {1,2,3}
                              // rs must differ from rd (assembler rejects aliased operands). Pick from
                              // {0, 4, 5} so rd != rs always.
        let rs = [0u8, 4, 5][(k % 3) as usize];
        prog.push(match k % 3 {
            0 => Instr::Xor { rd, rs },
            1 => Instr::Add { rd, rs },
            _ => Instr::Sub { rd, rs },
        });
    }
    let rom = assemble(&prog).expect("assemble long straight-line rom");
    let (_out, _vs, samples) = compose_straightline(&layout, &rom, prog.len());
    print_curve("straight-line 16-instr [w_pc=8, PC concrete]", &samples);
    eprintln!(
        "STRAIGHT-LINE LONG [w_pc=8]: {} steps composed; peak-node curve = {:?}",
        prog.len(),
        samples.iter().map(|s| s.peak).collect::<Vec<_>>()
    );
}

// ══════════════════════════════════════════════════════════════════════════
// PART 1b -- STRAIGHT-LINE, PC SYMBOLIC (fetch mux LIVE across steps).
//
// The harder tractability probe: DON'T re-pin PC. PC starts concrete 0 but the
// step's own pc_add_br;pc_inc drives it, and we feed the FULL output (incl. the
// symbolic-through-datapath PC) forward. BR=0 keeps PC deterministic, but the
// fetch mux is now walked over a PC that is a function of the composition -- this
// is the mux-blow-up regime the inductive proof actually needs.
// ══════════════════════════════════════════════════════════════════════════

#[test]
fn multistep_straightline_pcfree_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let prog = [
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Add { rd: 2, rs: 1 },
        Instr::Sub { rd: 1, rs: 0 },
        Instr::Xor { rd: 2, rs: 3 },
    ];
    let rom = assemble(&prog).expect("assemble");
    // BR = 0 kept concrete; PC concrete 0 at entry then driven by the step.
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let sv0 = straightline_sv(&layout, &vs, 0);
    let (out, samples) = compose_steps(&layout, &rom, sv0, prog.len());
    print_curve(
        "straight-line 4-instr [w_pc=8, PC FREE (not re-pinned)]",
        &samples,
    );

    // PC must be concrete = prog.len() after the run (BR=0 → PC+1 each step).
    let out_pc = out_pc_bits(&out, &layout);
    let t = vs.mk_true();
    let f = vs.mk_false();
    let mut pc_val = 0u32;
    for (i, b) in out_pc.iter().enumerate() {
        assert!(*b == t || *b == f, "PC bit {i} not concrete after free run");
        if *b == t {
            pc_val |= 1 << i;
        }
    }
    assert_eq!(pc_val, prog.len() as u32, "PC must advance to rom-length");
    eprintln!(
        "STRAIGHT-LINE PC-FREE [w_pc=8]: PC stayed CONCRETE (={pc_val}) through {} composed steps \
         -- the fetch mux collapses each step because BR=0 keeps PC deterministic. Peak {} nodes.",
        prog.len(),
        samples.last().unwrap().peak
    );
}

// ══════════════════════════════════════════════════════════════════════════
// PART 2 -- BOUNDED LOOP, PC + INPUT SYMBOLIC. The prize.
//
// A count-down loop: r0 counts down to 0 by a come-from backward jump. We build
// the ROM by hand (the come-from arithmetic is what realizes the loop), keep the
// input register + PC symbolic, and compose `step` up to a WCET bound. This is
// the regime the ~4 h real-cpu authority + b21c sweeps cover.
//
// We START with a hand-built minimal loop to measure growth cheaply, THEN (if
// tractable) scale toward vart_halve K=3 (budget 646).
// ══════════════════════════════════════════════════════════════════════════

/// Fully-symbolic CpuState: regs + BR + s2.. symbolic; PC=0 concrete, IR=0,
/// s0/s1=0. (The come-from loop makes PC data-dependent as the composition
/// proceeds, so PC must be able to go symbolic -- it starts concrete 0.)
fn loop_sv0(layout: &Layout, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8(0, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(0, vs)
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            const_word8(0, vs)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            const_word8(0, vs)
        } else {
            // regs, BR, s2.. SYMBOLIC
            symbolic_word8(vs, nf, f)
        }
    })
}

/// Measure BDD growth composing a REAL compiled loop's ROM symbolically over
/// its WCET budget, WITHOUT re-pinning PC. Reports the curve; does NOT assert
/// correctness (that is the separate concrete-anchor test) -- this is the
/// tractability measurement, the whole point of the experiment.
fn measure_loop_growth(layout: &Layout, rom: &[u32], budget: usize, tag: &str) -> Vec<StepSample> {
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let sv0 = loop_sv0(layout, &vs);
    let (_out, samples) = compose_steps(layout, rom, sv0, budget);
    print_curve(tag, &samples);
    samples
}

/// Build a hand loop: `r0 -= 1 (Subi) ; if r0 != 0 jump back` -- but the CPU has
/// no data-dependent branch instruction; branching is via BR (come-from). The
/// simplest data-dependent loop expressible as a straight ROM + BR is awkward to
/// hand-mint correctly, so PART 2's REAL target is the compiled vart_halve ROM
/// (whose come-from loop IS correctly minted by the compiler). We measure THAT.
///
/// This helper instead builds a trivial FIXED-count "loop" via an unconditional
/// come-from that always jumps back a fixed span for a fixed number of physical
/// steps -- enough to observe whether PC going symbolic under a self-referential
/// fetch mux blows up. (Correctness of this synthetic ROM is not claimed; only
/// growth is measured, and eval_bdd staying in-fragment is itself a finding.)
fn measure_symbolic_pc_iteration(layout: &Layout, n_steps: usize) -> Vec<StepSample> {
    // A 3-instruction straight ROM; we feed a SYMBOLIC BR so PC becomes a
    // symbolic function of BR after step 1 -- forcing the fetch mux to stay
    // symbolic across every subsequent step. This is the exact PC-mux-under-
    // -composition stress the inductive loop proof faces.
    let prog = [
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Add { rd: 2, rs: 1 },
        Instr::Sub { rd: 3, rs: 2 },
    ];
    let rom = assemble(&prog).expect("assemble");
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    // sv0: regs + BR symbolic (BR symbolic ⇒ PC symbolic after step 1).
    let sv0 = loop_sv0(layout, &vs);
    let (_out, samples) = compose_steps(layout, &rom, sv0, n_steps);
    print_curve(
        "SYMBOLIC-PC composition [w_pc=8, BR symbolic ⇒ PC symbolic from step 2]",
        &samples,
    );
    samples
}

/// THE PC-MUX-UNDER-COMPOSITION measurement: symbolic BR makes PC symbolic, so
/// every step after the first walks the fetch mux over a symbolic PC that is
/// ITSELF a growing BDD. This is the core blow-up risk for the inductive proof.
#[test]
fn multistep_symbolic_pc_growth_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let samples = measure_symbolic_pc_iteration(&layout, 8);
    let peaks: Vec<usize> = samples.iter().map(|s| s.peak).collect();
    eprintln!(
        "SYMBOLIC-PC GROWTH [w_pc=8]: peak-node curve over 8 composed steps (PC symbolic) = {peaks:?}"
    );
    // Record the growth ratio last/first as the headline tractability number.
    let ratio = samples.last().unwrap().peak as f64 / samples.first().unwrap().peak.max(1) as f64;
    eprintln!("SYMBOLIC-PC GROWTH ratio (step8 peak / step1 peak) = {ratio:.2}");
}

/// The REAL bounded-loop prize: compile vart_halve K=3 and compose its ROM
/// symbolically over the FULL WCET budget with SYMBOLIC input. Measures whether
/// the state BDD blows up across a real data-dependent loop's 646 steps.
// FINDING (measured, 2026-08-02): naive symbolic composition of a DATA-DEPENDENT
// LOOP is INTRACTABLE. Composing vart_halve K=3's ROM (rom_len=111, WCET budget
// 646) with SYMBOLIC input+PC blows the state BDD past 8 GB RSS and OOMs -- and the
// explosion happens WITHIN a single step's `eval_bdd` walk, before any per-step
// node-count guard can fire. Root cause: once PC is data-dependent (the loop's
// come-from), the fetched IR is SYMBOLIC, so the fetch mux ranges over all 111 ROM
// words and `decode_execute` must represent EVERY opcode's effect simultaneously --
// a symbolic "any instruction on any operands" relation, which is exponential.
// (Contrast: straight-line composition stays < 500 nodes because PC is CONCRETE
// each step, so exactly one instruction is decoded per step -- see the
// `multistep_straightline_*` tests, which are green and fast.)
//
// This is the GO/NO-GO for retiring the loop-based + real-CPU-authority sweeps by
// naive step-composition: NO-GO. The path forward is the INDUCTIVE-INVARIANT
// method (prove `step` preserves a loop invariant + a progress/rank measure, both
// SINGLE-STEP properties with CONCRETE-per-arm PC -- tractable), NOT full unrolling.
//
// The test is deliberately NOT run (it is a memory bomb): it only compiles the ROM
// and reports the shape, documenting the finding without OOMing the box
// (maestro-gate-no-timeout lesson). `measure_loop_growth` is retained for a future
// SMALL-surrogate-loop demonstration under the invariant method.
#[test]
#[ignore = "documents the measured symbolic-loop-composition BLOW-UP (>8GB OOM); does not run the bomb"]
fn multistep_vart_halve_growth() {
    let (rom, budget) = build_vart_halve_rom();
    eprintln!(
        "vart_halve K=3: rom_len={}, budget(wcet)={budget}. Naive symbolic composition \
         over this data-dependent loop OOMs (>8GB) -- symbolic PC ⇒ symbolic opcode ⇒ \
         decoder represents all ops at once. NOT run; see the module FINDING comment. \
         Path forward = inductive-invariant method, not full unrolling.",
        rom.len()
    );
    assert!(
        budget > 0 && !rom.is_empty(),
        "rom compiles; the bomb itself is not run"
    );
    let _ = measure_loop_growth; // retained for the future small-surrogate demo
}

/// Compile the vart_halve K=3,HB=1 Trace and return (rom words, WCET budget).
fn build_vart_halve_rom() -> (Vec<u32>, usize) {
    use std::collections::BTreeMap;
    use theseus_core::prim::{enumerate_values, PrimDef};
    use theseus_core::Type;
    use theseus_tisc::compile::{compile_with_metadata, marshal, unmarshal};

    const K: u8 = 3;
    const HB: u8 = 1;
    fn rot_step(tag: u8, v: u8, h: u8, k: u8, hb: u8) -> (u8, u8, u8) {
        let width = (1 + hb + k) as u32;
        let w: u32 = ((tag as u32) << (hb + k)) | ((h as u32) << k) | (v as u32);
        let lsb = w & 1;
        let wp = (w >> 1) | (lsb << (width - 1));
        let vmask = (1u32 << k) - 1;
        let hmask = (1u32 << hb) - 1;
        (
            (wp >> (hb + k)) as u8,
            (wp & vmask) as u8,
            ((wp >> k) & hmask) as u8,
        )
    }
    fn state_with_regs(bytes: &[u8]) -> StateVals {
        let mut r = [0u8; theseus_tisc::types::NUM_REGS];
        r[..bytes.len()].copy_from_slice(bytes);
        StateVals {
            regs: r,
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0; NUM_SCRATCH],
            mem: [0; MEM_WORDS],
        }
    }
    let u = Type::prod(Type::flat(K), Type::flat(HB));
    let carrier = Type::sum(u.clone(), u.clone());
    let vals = enumerate_values(&carrier);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for val in &vals {
        let sv = marshal(val, &carrier);
        let (nt, nv, nh) = rot_step(sv.regs[0], sv.regs[1], sv.regs[2], K, HB);
        let out_val = unmarshal(&state_with_regs(&[nt, nv, nh]), &carrier);
        forward.insert(val.clone(), out_val.clone());
        backward.insert(out_val, val.clone());
    }
    let prim = PrimDef {
        forward_name: "v2_halve_until_odd_k3_hb1".into(),
        backward_name: "v2_halve_until_odd_k3_hb1_inv".into(),
        input_ty: carrier.clone(),
        output_ty: carrier.clone(),
        forward,
        backward,
        symbolic: None,
    };
    let iso = Iso::trace(u, Iso::Prim(Box::new(prim)));
    let lowered = compile_with_metadata(&iso).expect("vart_halve must lower");
    let rom = assemble(&lowered.instrs).expect("assemble vart_halve");
    (rom, lowered.wcet())
}

// ══════════════════════════════════════════════════════════════════════════
// REALITY ANCHOR -- symbolic composition co-factored to a concrete point equals
// the real per-step eval loop (run_rom_on_cpu).
// ══════════════════════════════════════════════════════════════════════════

const EVAL_FUEL: u64 = 8_000_000_000;

#[test]
fn multistep_reality_anchor_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let prog = [
        Instr::Xor { rd: 1, rs: 0 },
        Instr::Add { rd: 2, rs: 1 },
        Instr::Sub { rd: 1, rs: 0 },
        Instr::Xor { rd: 2, rs: 3 },
    ];
    let rom = assemble(&prog).expect("assemble");

    // Symbolic composition, PC free (starts 0, driven by the step). BR=0.
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let sv0 = straightline_sv(&layout, &vs, 0);
    let (out, _samples) = compose_steps(&layout, &rom, sv0, prog.len());

    // Concrete points to co-factor + compare against the real eval loop.
    let pts: [[u8; 8]; 4] = [
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0x2A, 0xC3, 0x01, 0x11, 0, 0, 0, 0x5A],
        [0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 0],
        [0x80, 0x7F, 0x11, 0x22, 0, 0, 0, 0],
    ];
    for regs8 in &pts {
        let mut regs = [0u8; theseus_tisc::types::NUM_REGS];
        let k = regs.len().min(8);
        regs[..k].copy_from_slice(&regs8[..k]);
        let s = StateVals {
            regs,
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0u8; NUM_SCRATCH],
            mem: [0u8; MEM_WORDS],
        };
        // Real per-step eval loop (what run_rom_on_cpu does), budget = rom len.
        let step_iso = step_with(&layout, &rom);
        let mut cur = s.encode_with(&layout);
        let mut fuel = EVAL_FUEL;
        for _ in 0..prog.len() {
            cur = theseus_eval::eval(&step_iso, cur, &mut fuel).expect("eval");
        }
        let real = StateVals::decode_with(&layout, &cur);

        // Co-factor the symbolic output to this concrete reg point.
        let asg = restrict_regs_br(&layout, &vs, &s);
        let t = vs.mk_true();
        for r in 0..layout.num_regs {
            let got = out_field_bits(&out, &layout, layout.reg_field(r));
            let mut val = 0u8;
            for b in 0..W {
                let cof = got[b].restrict(&asg);
                assert!(
                    cof == t || cof == vs.mk_false(),
                    "reality anchor: r{r} bit {b} not concrete after restrict"
                );
                if cof == t {
                    val |= 1 << b;
                }
            }
            assert_eq!(
                val, real.regs[r],
                "reality anchor: symbolic-composition r{r} != real eval loop (regs={regs8:?})"
            );
        }
    }
    eprintln!(
        "MULTISTEP REALITY ANCHOR [w_pc=8]: symbolic composition co-factored at {} concrete points \
         == real per-step theseus_eval loop (run_rom_on_cpu) -- the reality anchor.",
        pts.len()
    );
}

// ══════════════════════════════════════════════════════════════════════════
// NEGATIVE CONTROL -- a wrong composed spec must be caught.
// ══════════════════════════════════════════════════════════════════════════

#[test]
fn multistep_negative_control_wpc8() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let prog = [Instr::Xor { rd: 1, rs: 0 }, Instr::Add { rd: 2, rs: 1 }];
    let rom = assemble(&prog).expect("assemble");
    let (out, vs, _samples) = compose_straightline(&layout, &rom, prog.len());
    let nf = n_fields(&layout);
    let r = |i: usize| field_bits(&vs, nf, layout.reg_field(i));

    // TRUE composed r2 = r2 + (r1 ^ r0).
    let r1x = (0..W).map(|i| r(1)[i].xor(&r(0)[i])).collect::<Vec<_>>();
    let true_r2 = ripple_sum(&r(2), &r1x, &vs);
    let got_r2 = out_field_bits(&out, &layout, layout.reg_field(2));
    for b in 0..W {
        assert!(
            got_r2[b] == true_r2[b],
            "sanity: composed r2 must match true spec"
        );
    }

    // WRONG-1: swap the op order (add BEFORE the xor) → r2 = r2 + r1 (old r1).
    let wrong_r2 = ripple_sum(&r(2), &r(1), &vs);
    let differs = (0..W).any(|b| got_r2[b] != wrong_r2[b]);
    assert!(
        differs,
        "NEG: composition must DIFFER from the wrong op-ORDER spec (teeth)"
    );

    // WRONG-2: a DIFFERENT ROM (Add instead of Xor at step 0) must produce a
    // DIFFERENT composed output -- proves the composition tracks the ROM.
    let prog2 = [Instr::Add { rd: 1, rs: 0 }, Instr::Add { rd: 2, rs: 1 }];
    let rom2 = assemble(&prog2).expect("assemble");
    let (out2, _vs2, _s2) = compose_straightline(&layout, &rom2, prog2.len());
    let got2_r2 = out_field_bits(&out2, &layout, layout.reg_field(2));
    let differs_rom = (0..W).any(|b| got2_r2[b] != got_r2[b]);
    assert!(
        differs_rom,
        "NEG: a different ROM must yield a different composed output (teeth)"
    );

    eprintln!(
        "MULTISTEP NEGATIVE CONTROL [w_pc=8]: composition != {{wrong op-order, wrong ROM}} -- teeth."
    );
}
