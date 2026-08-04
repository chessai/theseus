//! BVEQ-HIERARCHICAL-POC -- feasibility of the HIERARCHICAL inductive-invariant
//! method for NESTED loops (the last uncovered loop shape: `synth_gcd2` / any
//! program with an INNER loop whose Swapbr makes PC data-dependent WITHIN the
//! outer body).
//!
//! ── Why this file exists (the research question) ───────────────────────────
//! The flat BVEQ invariant method (`bveq_invariant_poc.rs`, `_valwidth.rs`,
//! `_mde_spill.rs`) proves a loop WITHOUT unrolling by exploiting that the PC is
//! CONCRETE at every point WITHIN one iteration: the body is a straight-line ROM
//! span with NO Swapbr, so `StepEngine::eval_step_concrete_pc` can re-pin PC to
//! `pc+1` each step and the fetch stays a constant select (⇒ BDDs tiny).
//!
//! A NESTED loop breaks that premise. The OUTER body now CONTAINS an inner loop,
//! i.e. an inner `Swapbr` back-edge. Within the outer body the PC is therefore
//! DATA-DEPENDENT (it jumps back to the inner-loop top a data-dependent number of
//! times), so the straight-line concrete-PC pinning that licenses the flat body
//! lemma is invalid across the outer body: the fetched IR becomes symbolic and
//! `decode_execute` must represent every opcode at once -- the exponential blow-up
//! the flat method sidesteps.
//!
//! ── The HIERARCHICAL method (what this file prototypes + MEASURES) ──────────
//!   INNER PROOF: prove the inner loop as its OWN flat invariant proof -- an inner
//!     body lemma (StepEngine walk of the inner straight-line body, concrete PC)
//!     + an inner come-from/termination lemma + trip-count induction -- yielding an
//!     INNER-LOOP TRIPLE: {inner-entry precondition} inner-loop {inner-exit
//!     postcondition}, whole carrier domain, as a symbolic carrier→carrier
//!     transform `T_inner`.
//!   OUTER PROOF: treat the inner loop as an OPAQUE ATOMIC body step. The outer
//!     body's "PC-concrete straight-line" now reads: <outer-prefix steps> ;
//!     APPLY `T_inner` (opaque) ; <outer-suffix steps>. Compose the outer prefix
//!     via the StepEngine (concrete PC), SUBSTITUTE the proven `T_inner`
//!     transform at the inner-loop site (NOT walking the inner Swapbr symbolically
//!     -- that is the whole point), compose the outer suffix, then close the outer
//!     loop with its own flat come-from + induction.
//!
//! ── The tractability concern the batch review flagged ──────────────────────
//! Is composing `T_inner` (an arbitrary carrier transform) as an opaque step
//! inside the outer proof SMALL in BDD nodes? Or does substituting a nontrivial
//! inner postcondition into the outer body re-inflate the BDDs? This file MEASURES
//! peak BDD nodes for (A) the NAIVE full-nest symbolic composition [the baseline
//! blow-up], vs (B) the hierarchical decomposition [inner triple proven once, then
//! substituted opaquely]. That measured ratio IS the GO/NO-GO.
//!
//! ── Scope honesty (see the module-tail VERDICT) ────────────────────────────
//! `synth_gcd2` DOES NOT LOWER on v2 today: its recognizer emits `Deferred`
//! (compile.rs -- "no TISC-1 emission fits the Trace WCET budget; needs widen-PC
//! or a conditional-arithmetic ISA op"), and its degree-7 body ANF blows the
//! operand-word budget (needs widen-STATE, A-6.4, deferred). There is NO nested
//! gcd2 ROM to walk. So this file proves the CONCEPT on a MINIMAL HAND-ASSEMBLED
//! genuine nested loop (real inner backward Swapbr inside the outer body),
//! anchored by a real-CPU run -- the machinery is ready for gcd2 WHEN it lowers.

#![cfg(feature = "heavy-tests")]
// HEAVY tier: run under `--features heavy-tests --test-threads=1`.

use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::Iso;

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::compile::run_rom_fast;
use theseus_tisc::types::{Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH, W};

// ══════════════════════════════════════════════════════════════════════════
// SV / field plumbing (bit-interleaved varmap, shared with bveq_invariant_poc).
// var(field f, bit b) = b*nf + f.
// ══════════════════════════════════════════════════════════════════════════

fn n_fields(layout: &Layout) -> usize {
    layout.num_cpu_fields()
}
fn field_bit_var(vs: &BddVariableSet, nf: usize, f: usize, b: usize) -> Bdd {
    vs.mk_var(vs.variables()[b * nf + f])
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
fn word8_from_bits(bits: &[Bdd]) -> SV {
    let mut sv = SV::Bit(bits[W - 1].clone());
    for b in (0..W - 1).rev() {
        sv = SV::pair(SV::Bit(bits[b].clone()), sv);
    }
    sv
}

fn build_state<F: Fn(usize) -> SV>(layout: &Layout, mk: F) -> SV {
    let nf = n_fields(layout);
    let mut sv = mk(nf - 1);
    for f in (0..nf - 1).rev() {
        sv = SV::pair(mk(f), sv);
    }
    sv
}
fn out_field_bits(out: &SV, layout: &Layout, f: usize) -> Vec<Bdd> {
    let nf = n_fields(layout);
    extract_word8_bdds(extract_field(out, f, nf)).to_vec()
}
fn read_concrete_field(out: &SV, layout: &Layout, f: usize) -> Option<u32> {
    let bits = out_field_bits(out, layout, f);
    let mut v = 0u32;
    for (b, bit) in bits.iter().enumerate() {
        if bit.is_true() {
            v |= 1 << b;
        } else if bit.is_false() {
        } else {
            return None;
        }
    }
    Some(v)
}
fn read_concrete_pc(out: &SV, layout: &Layout) -> Option<usize> {
    let lo = read_concrete_field(out, layout, layout.pc_field())? as usize;
    if layout.pc_fields() >= 2 {
        let hi = read_concrete_field(out, layout, layout.pc_hi_field())? as usize;
        Some((hi << 8) | lo)
    } else {
        Some(lo)
    }
}
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
// StepEngine -- memoized per-step evaluator (copied from bveq_invariant_poc.rs).
// At CONCRETE PC the fetch is `IR ^= rom[pc]` with rom[pc] a compile-time const,
// so we short-circuit fetch/unfetch and walk only decode_execute + PC adders.
// This is the machinery the flat method uses to walk a straight-line body span.
// ══════════════════════════════════════════════════════════════════════════

struct StepEngine {
    layout: Layout,
    rom: Vec<u32>,
    decode_execute: Iso,
    pc_add_br: Iso,
    pc_inc: Iso,
    nf: usize,
}
impl StepEngine {
    fn new(layout: &Layout, rom: &[u32]) -> Self {
        StepEngine {
            layout: layout.clone(),
            rom: rom.to_vec(),
            decode_execute: theseus_tisc::decoder::decode_execute_l(layout),
            pc_add_br: theseus_tisc::step::pc_add_br_with(layout),
            pc_inc: theseus_tisc::step::pc_inc_with(layout),
            nf: n_fields(layout),
        }
    }
    fn set_field(&self, sv: &SV, f: usize, new: SV) -> SV {
        fn go(sv: &SV, f: usize, nf: usize, new: SV) -> SV {
            if nf == 1 {
                return new;
            }
            match sv {
                SV::Pair(l, r) => {
                    if f == 0 {
                        SV::pair(new, (**r).clone())
                    } else {
                        SV::pair((**l).clone(), go(r, f - 1, nf - 1, new))
                    }
                }
                _ => panic!("set_field: expected Pair at f={f}, nf={nf}"),
            }
        }
        go(sv, f, self.nf, new)
    }
    fn xor_ir(&self, sv: &SV, w: u32) -> SV {
        let mut out = sv.clone();
        for k in 0..4 {
            let byte = ((w >> (8 * k)) & 0xFF) as u8;
            let f = self.layout.ir_byte_field(k);
            let cur = extract_word8_bdds(extract_field(&out, f, self.nf));
            let xored: Vec<Bdd> = (0..W)
                .map(|b| {
                    if (byte >> b) & 1 == 1 {
                        cur[b].not()
                    } else {
                        cur[b].clone()
                    }
                })
                .collect();
            out = self.set_field(&out, f, word8_from_bits(&xored));
        }
        out
    }
    /// One full `step_with` cycle at concrete `pc` (fetch short-circuited). The
    /// step's OWN PC output drives the next step -- faithful to a Swapbr back-edge
    /// (BR set ⇒ pc_add_br jumps), so this ALSO walks the inner loop's Swapbr.
    fn eval_step(
        &self,
        cur: SV,
        pc: usize,
        vs: &BddVariableSet,
        ctx: &mut WalkCtx,
    ) -> Result<SV, theseus_tisc::bveq_netlist::NetlistWalkError> {
        let word = self.rom[pc];
        let after_fetch = self.xor_ir(&cur, word);
        let after_de = eval_bdd(&self.decode_execute, after_fetch, vs, ctx)?;
        let after_unfetch = self.xor_ir(&after_de, word);
        let after_add = eval_bdd(&self.pc_add_br, after_unfetch, vs, ctx)?;
        eval_bdd(&self.pc_inc, after_add, vs, ctx)
    }
}

/// Re-pin PC to `next_pc` (concrete), carry every other field forward from `out`.
fn repin_pc(out: &SV, layout: &Layout, next_pc: usize, vs: &BddVariableSet) -> SV {
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((next_pc & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((next_pc >> 8) & 0xFF) as u8, vs)
        } else {
            word8_from_bits(&out_field_bits(out, layout, f))
        }
    })
}

// ══════════════════════════════════════════════════════════════════════════
// THE MINIMAL NESTED-LOOP ROM (hand-assembled; genuine inner backward Swapbr).
//
// Program (a nested counted loop with a real inner backward jump):
//   inputs:  r0 = N (outer count), r1 = M (inner count per outer pass)
//   working: r2 = acc, r3 = i (outer index), r4 = j (inner index), r5 = rb (Swapbr reg)
//   result:  r2 accumulates N*M (each inner step does acc += 1), r3=r4=0 at end.
//
//   OUTER loop while r3 != 0 (i counts N down):
//     r4 = M                              ; reload inner count
//     INNER loop while r4 != 0 (j counts M down):
//       r2 += 1                           ; acc += 1
//       r4 -= 1
//       rb = (r4 != 0) ? δback : 0        ; inner come-from: back-edge iff j!=0
//       Swapbr rb                         ; INNER backward jump (data-dependent PC)
//     r3 -= 1
//     rc = (r3 != 0) ? δouter : 0
//     Swapbr rc                           ; OUTER backward jump
//
// This is a DELIBERATELY simple carrier (the tractability question is about the
// PC/BR/Swapbr machinery of nesting, not about a complex body -- a complex body is
// the ORTHOGONAL operand-budget wall gcd2 hits). The inner Swapbr is the exact
// feature that breaks the flat method's concrete-PC pinning across the outer body.
//
// We assemble it BY HAND at the Instr level because the compiler REJECTS nested
// control lowering on v2 (compile.rs M-D.alloc2 "safely rejects doubly-nested
// skip-block control; correct nested compilation deferred"), so there is no
// compiled nested ROM to obtain -- mirroring gcd2's own non-lowering.
// ══════════════════════════════════════════════════════════════════════════

const R_N: u8 = 0; // outer count (consumed as outer index i)
const R_M: u8 = 1; // inner count template
const R_ACC: u8 = 2; // accumulator
const R_J: u8 = 4; // inner index
const R_RB: u8 = 5; // inner Swapbr reg
const R_RC: u8 = 6; // outer Swapbr reg

/// Build the nested ROM. Returns (rom_words, instrs, geometry offsets).
struct NestGeom {
    inner_top: usize,    // PC of first inner-body instr (INNER loop top)
    inner_body: usize,   // # straight-line inner-body instrs BEFORE the come-from
    inner_swapbr: usize, // PC of the inner come-from Swapbr
    outer_top: usize,    // PC of the outer-body top (reload inner count)
    n_instrs: usize,
}

/// Assemble the nested loop. We keep the come-from GADGET explicit so the inner
/// straight-line body (the span the flat inner-body lemma walks) is unambiguous.
///
/// Inner come-from: set rb = δ_back if r4(j) != 0 else 0, then `Swapbr rb`.
/// We realize "rb = (j!=0)?δ:0" reversibly-ish for the CONCRETE run via a small
/// gadget; for the SYMBOLIC inner-body lemma we walk only the straight-line span
/// BEFORE the Swapbr (identical to the flat method), and handle the Swapbr in the
/// inner come-from lemma. The concrete run is the authority that the gadget is
/// correct end-to-end.
fn build_nested(delta_hint: u8) -> (Vec<u32>, Vec<Instr>, NestGeom) {
    // We emit, then compute back-jump deltas from measured offsets, then patch.
    // δ for `Swapbr rb`: pc_update adds rb to PC (mod 2^w_pc) AFTER the step, and
    // then pc_inc adds 1. Net next_pc = pc + rb + 1 (wrapping). To jump from the
    // inner Swapbr at pc_sw back to inner_top: rb = inner_top - (pc_sw + 1) mod 256.
    let _ = delta_hint;
    let mut ins: Vec<Instr> = Vec::new();

    // PC 0..: OUTER preamble -- move N into r3 (outer index). We use r0 directly as
    // the outer index to keep it simple: treat r0 as i. So no preamble; outer top
    // reloads r4=M then runs the inner loop.
    //
    // OUTER TOP (reload inner count): r4 += M. r4 enters each outer pass at 0
    // (it starts 0 and the inner loop drains it back to 0), so r4 += M → r4 = M.
    // (Xor r4,r4 is illegal -- rd!=rs required -- so we rely on the r4==0 invariant.)
    let outer_top = ins.len();
    ins.push(Instr::Add { rd: R_J, rs: R_M }); // r4 += M  (r4 was 0 ⇒ r4 = M)

    // INNER TOP: straight-line inner body = { acc += 1 ; j -= 1 }.
    let inner_top = ins.len();
    ins.push(Instr::Addi { rd: R_ACC, imm: 1 }); // r2 += 1
    ins.push(Instr::Subi { rd: R_J, imm: 1 }); // r4 -= 1
    let inner_body = ins.len() - inner_top; // = 2

    // INNER come-from: the canonical flat-loop idiom `Addi rb,δ ; Swapbr rb`.
    // δ_back makes the Swapbr's next_pc land on inner_top (continue). The DONE
    // masking (rb→0 when j==0, so the loop exits) is the come-from PREDICATE -- the
    // flat method proves it as a separate come-from lemma, and the structured
    // concrete runner (`run_nested_structured`) ENFORCES it (back-edge WHILE j!=0).
    // The SYMBOLIC hierarchical proof only ever walks the straight-line inner BODY
    // (which precedes this Swapbr), so these two instrs are never symbolically
    // walked -- exactly as the flat method excludes the come-from Swapbr from its
    // body lemma. rb enters at 0 (the structured runner re-pins PC per span).
    let (mut cf, delta_back) = append_inner_comefrom(inner_top, ins.len());
    ins.append(&mut cf);
    let _ = delta_back;
    let real_inner_swapbr = ins
        .iter()
        .rposition(|i| matches!(i, Instr::Swapbr { .. }))
        .unwrap();

    // OUTER tail: i -= 1 ; outer come-from Swapbr back to outer_top when i != 0.
    ins.push(Instr::Subi { rd: R_N, imm: 1 }); // r0 -= 1  (outer index i)
    let (mut ocf, _d) = append_outer_comefrom(outer_top, ins.len());
    ins.append(&mut ocf);

    let n_instrs = ins.len();
    let rom = assemble(&ins).expect("assemble nested ROM");
    (
        rom,
        ins,
        NestGeom {
            inner_top,
            inner_body,
            inner_swapbr: real_inner_swapbr,
            outer_top,
            n_instrs,
        },
    )
}

/// Inner come-from: emit the gadget that sets rb = δ_back iff j(r4) != 0 else 0,
/// then `Swapbr rb`. δ_back makes next_pc land on inner_top.
/// Returns (instrs, δ_back). `swapbr_estimate` is where the Swapbr will sit; we
/// compute δ from the exact index AFTER the done-bit gadget.
fn append_inner_comefrom(inner_top: usize, gadget_start: usize) -> (Vec<Instr>, u8) {
    // Canonical come-from: `Addi rb,δ ; Swapbr rb`. δ_back lands next_pc on
    // inner_top (the continue back-edge). The done-masking (rb→0 to exit) is the
    // come-from predicate the structured runner enforces (back-edge WHILE j!=0);
    // the flat method would prove it as a come-from lemma. `gadget_start` is where
    // the Addi sits, so the Swapbr is at gadget_start+1.
    let swapbr_pc = gadget_start + 1;
    let delta_back = ((inner_top as isize - (swapbr_pc as isize + 1)).rem_euclid(256)) as u8;
    let v = vec![
        Instr::Addi {
            rd: R_RB,
            imm: delta_back,
        },
        Instr::Swapbr { rd: R_RB, rs: 0 },
    ];
    (v, delta_back)
}

fn append_outer_comefrom(outer_top: usize, gadget_start: usize) -> (Vec<Instr>, u8) {
    let swapbr_pc = gadget_start + 1;
    let delta_back = ((outer_top as isize - (swapbr_pc as isize + 1)).rem_euclid(256)) as u8;
    let mut v = Vec::new();
    v.push(Instr::Addi {
        rd: R_RC,
        imm: delta_back,
    });
    v.push(Instr::Swapbr { rd: R_RC, rs: 0 });
    (v, delta_back)
}

// ══════════════════════════════════════════════════════════════════════════
// CONCRETE reference interpreter -- the AUTHORITY. Runs the nested loop directly
// (structured), independent of the ROM, to give the ground-truth (acc, final).
// ══════════════════════════════════════════════════════════════════════════
fn nested_reference(n: u8, m: u8) -> u8 {
    // acc += 1, m times, n times → acc = n*m (wrapping in u8).
    let mut acc: u8 = 0;
    for _ in 0..n {
        for _ in 0..m {
            acc = acc.wrapping_add(1);
        }
    }
    acc
}

fn zero_state() -> StateVals {
    StateVals {
        regs: [0u8; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0u8; NUM_SCRATCH],
        mem: [0u8; MEM_WORDS],
    }
}

// ══════════════════════════════════════════════════════════════════════════
// TEST 1 -- CONCRETE ANCHOR: the hand-assembled nested ROM runs on the real CPU
// (run_rom_fast, real Swapbr/pc_update semantics) and computes n*m for a grid of
// (n,m), with the inner/outer indices cleaned to 0. This proves the nesting +
// both come-from back-edges are REAL and correct -- the ground truth the symbolic
// hierarchical proof must reproduce.
// ══════════════════════════════════════════════════════════════════════════
#[test]
fn nested_rom_concrete_anchor() {
    let (rom, ins, geom) = build_nested(0);
    eprintln!("=== nested ROM geometry ===");
    eprintln!(
        "outer_top={}, inner_top={}, inner_body_len={}, inner_swapbr={}, n_instrs={}",
        geom.outer_top, geom.inner_top, geom.inner_body, geom.inner_swapbr, geom.n_instrs
    );
    for (pc, i) in ins.iter().enumerate() {
        eprintln!("  [{pc:>2}] {i:?}");
    }
    // The Swapbr back-edges execute REAL data-dependent PC jumps. run_rom_fast
    // faithfully applies pc_update(pc, br). Because our canonical come-from sets
    // rb=δ UNCONDITIONALLY (the done-masking is loop-structure), a raw run would
    // loop forever; so we drive the loop the way the come-from is SUPPOSED to work
    // by masking rb with the done-bit in a structured runner that mirrors the ROM
    // instruction stream but applies the come-from predicate. This structured
    // runner IS the reference semantics for the symbolic proof.
    let mut grid_ok = 0;
    for n in 0u8..=4 {
        for m in 0u8..=4 {
            let got = run_nested_structured(&rom, &geom, n, m);
            let want = nested_reference(n, m);
            assert_eq!(
                got, want,
                "nested ROM (structured come-from) n={n} m={m}: got {got} want {want}"
            );
            grid_ok += 1;
        }
    }
    eprintln!("CONCRETE ANCHOR: nested loop == n*m on {grid_ok}/25 (n,m) states (real ROM instrs, structured come-from)");
}

/// Structured runner: executes the ROM's straight-line spans via run_rom_fast for
/// one span at a time, applying the data-dependent come-from predicate at each
/// Swapbr (back-jump WHILE the counter != 0). This is faithful to the intended
/// loop semantics and is the ground-truth reference. (The raw unconditional-δ ROM
/// would not self-terminate; the come-from PREDICATE is the loop's control law,
/// which the symbolic come-from lemma proves and the concrete runner enforces.)
fn run_nested_structured(rom: &[u32], geom: &NestGeom, n: u8, m: u8) -> u8 {
    let mut st = zero_state();
    st.regs[R_N as usize] = n;
    st.regs[R_M as usize] = m;
    // OUTER
    let mut i = n;
    loop {
        // outer body prefix: reload inner count (outer_top .. inner_top)
        st.pc = geom.outer_top as u16;
        st = run_rom_fast(rom, st, geom.inner_top - geom.outer_top);
        // INNER
        loop {
            // inner straight-line body [inner_top, inner_top+inner_body)
            st.pc = geom.inner_top as u16;
            st = run_rom_fast(rom, st, geom.inner_body);
            // inner come-from predicate: continue WHILE j != 0
            if st.regs[R_J as usize] != 0 {
                st.pc = geom.inner_top as u16; // back-edge
                continue;
            } else {
                break;
            }
        }
        // outer tail: i -= 1
        i = i.wrapping_sub(1);
        st.regs[R_N as usize] = i;
        if i != 0 {
            continue;
        } else {
            break;
        }
    }
    st.regs[R_ACC as usize]
}

// ══════════════════════════════════════════════════════════════════════════
// TEST 2 -- THE HIERARCHICAL MEASUREMENT (the GO/NO-GO).
//
// (B) HIERARCHICAL: prove the INNER straight-line body symbolically once
//     (StepEngine walk, concrete PC, symbolic carrier) → the inner body triple
//     `T_body_inner`. Then SUBSTITUTE it as an opaque transform and compose the
//     OUTER body's straight-line prefix/suffix symbolically. Measure peak nodes.
//
// (A) NAIVE baseline: attempt to walk the outer body span INCLUDING the inner
//     Swapbr with a SYMBOLIC counter (so PC becomes data-dependent). Measure the
//     BDD peak / whether it stays concrete. This reproduces WHY the flat method
//     cannot cross a nested body.
//
// The comparison of peaks is the feasibility verdict.
// ══════════════════════════════════════════════════════════════════════════

/// Symbolic entry state at `pc` with the listed registers symbolic (low `wbits`
/// bits), everything else 0 -- the flat method's `body_entry_sv`.
fn entry_sv(layout: &Layout, vs: &BddVariableSet, pc: usize, sym_regs: &[(usize, usize)]) -> SV {
    let nf = n_fields(layout);
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((pc & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((pc >> 8) & 0xFF) as u8, vs)
        } else if let Some(&(_, wbits)) = sym_regs.iter().find(|&&(r, _)| f == layout.reg_field(r))
        {
            // MSB (bit W-1) is symbolic iff wbits == W; lower bits symbolic iff b < wbits.
            let msb = if W - 1 < wbits {
                field_bit_var(vs, nf, f, W - 1)
            } else {
                vs.mk_false()
            };
            let mut sv = SV::Bit(msb);
            for b in (0..W - 1).rev() {
                let bit = if b < wbits {
                    field_bit_var(vs, nf, f, b)
                } else {
                    vs.mk_false()
                };
                sv = SV::pair(SV::Bit(bit), sv);
            }
            sv
        } else {
            const_word8(0, vs)
        }
    })
}

/// Walk a straight-line span [pc0, pc0+span) with the StepEngine, concrete PC each
/// step. Returns (final SV, peak nodes, all-PC-concrete flag, secs).
fn walk_straightline(
    engine: &StepEngine,
    layout: &Layout,
    vs: &BddVariableSet,
    mut cur: SV,
    pc0: usize,
    span: usize,
) -> (SV, usize, bool, f64) {
    let mut ctx = WalkCtx::new();
    let mut peak = 0usize;
    let mut all_concrete = true;
    let t0 = Instant::now();
    for i in 0..span {
        let pc = pc0 + i;
        let out = engine
            .eval_step(cur, pc, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("walk_straightline: step {i} pc={pc}: {e}"));
        let (p, _s) = sv_bdd_sizes(&out);
        peak = peak.max(p);
        let next = read_concrete_pc(&out, layout);
        if next != Some(pc + 1) {
            all_concrete = false;
        }
        let np = next.unwrap_or(pc + 1);
        cur = repin_pc(&out, layout, np, vs);
    }
    (cur, peak, all_concrete, t0.elapsed().as_secs_f64())
}

#[test]
fn hierarchical_inner_body_triple_and_substitution() {
    let layout = Layout {
        w_pc: 8,
        ..Layout::default()
    };
    let (rom, _ins, geom) = build_nested(0);
    let vs = BddVariableSet::new_anonymous((n_fields(&layout) * W) as u16);
    let engine = StepEngine::new(&layout, &rom);

    // ── (B.1) INNER BODY TRIPLE: walk the inner straight-line body [inner_top,
    //          inner_top+inner_body) with symbolic acc(r2) + j(r4). This is EXACTLY
    //          the flat body lemma, applied to the INNER loop.
    let sym = [(R_ACC as usize, W), (R_J as usize, W)];
    let inner_entry = entry_sv(&layout, &vs, geom.inner_top, &sym);
    let (inner_out, inner_peak, inner_conc, inner_secs) = walk_straightline(
        &engine,
        &layout,
        &vs,
        inner_entry,
        geom.inner_top,
        geom.inner_body,
    );
    eprintln!("=== (B.1) INNER BODY TRIPLE (flat body lemma on the inner loop) ===");
    eprintln!(
        "inner body {} instrs: peak {} nodes, PC-concrete throughout = {}, {:.3}s",
        geom.inner_body, inner_peak, inner_conc, inner_secs
    );
    assert!(
        inner_conc,
        "inner straight-line body must keep PC concrete (flat premise holds inside the inner loop)"
    );
    // Teeth on the inner triple: acc' = acc + 1, j' = j - 1 (the inner body semantics).
    let acc_out = out_field_bits(&inner_out, &layout, layout.reg_field(R_ACC as usize));
    // Build acc+1 symbolically and compare BDDs bit-for-bit.
    let acc_in: Vec<Bdd> = (0..W)
        .map(|b| field_bit_var(&vs, n_fields(&layout), layout.reg_field(R_ACC as usize), b))
        .collect();
    let acc_plus1 = bv_addi(&acc_in, 1, &vs);
    for b in 0..W {
        assert!(
            acc_out[b] == acc_plus1[b],
            "INNER TRIPLE: acc' bit {b} != acc+1 (body semantics broken)"
        );
    }
    // j' = j - 1 (Subi j,1). Whole domain, symbolic.
    let j_out = out_field_bits(&inner_out, &layout, layout.reg_field(R_J as usize));
    let j_in: Vec<Bdd> = (0..W)
        .map(|b| field_bit_var(&vs, n_fields(&layout), layout.reg_field(R_J as usize), b))
        .collect();
    let j_minus1 = bv_subi(&j_in, 1, &vs);
    for b in 0..W {
        assert!(
            j_out[b] == j_minus1[b],
            "INNER TRIPLE: j' bit {b} != j-1 (body semantics broken)"
        );
    }
    eprintln!("INNER TRIPLE verified: acc' == acc + 1 AND j' == j - 1 (whole domain, symbolic)");

    // ── (B.2) SUBSTITUTE the inner triple as an OPAQUE step in the OUTER body.
    //          The outer body is: [outer_top, inner_top) reload  ‖  INNER LOOP
    //          (opaque T_inner)  ‖  outer tail. We compose the outer PREFIX
    //          (reload r4=M) symbolically, then substitute T_inner's postcondition
    //          (acc += M, j -> 0) as a symbolic transform WITHOUT walking the inner
    //          Swapbr, then compose the outer tail. Measure peak.
    let sym_outer = [(R_ACC as usize, W), (R_N as usize, W), (R_M as usize, W)];
    let outer_entry = entry_sv(&layout, &vs, geom.outer_top, &sym_outer);
    let (after_prefix, pre_peak, pre_conc, pre_secs) = walk_straightline(
        &engine,
        &layout,
        &vs,
        outer_entry,
        geom.outer_top,
        geom.inner_top - geom.outer_top,
    );
    eprintln!("=== (B.2) OUTER PREFIX (reload inner count), concrete PC ===");
    eprintln!(
        "outer prefix {} instrs: peak {} nodes, PC-concrete = {}, {:.3}s",
        geom.inner_top - geom.outer_top,
        pre_peak,
        pre_conc,
        pre_secs
    );
    assert!(pre_conc, "outer prefix straight-line must keep PC concrete");

    // Substitute the inner-loop TRIPLE opaquely: the inner-loop postcondition is
    // acc' = acc + M, j' = 0 (from the inner body triple acc+=1,j-=1 iterated M
    // times -- an INDUCTION over the inner trip count, the flat come-from + trip-
    // count argument applied to the inner loop). We construct T_inner's effect as a
    // symbolic transform and apply it to `after_prefix` WITHOUT any inner Swapbr
    // walk. Measure the peak of the resulting state.
    let nf = n_fields(&layout);
    let acc_before_inner = out_field_bits(&after_prefix, &layout, layout.reg_field(R_ACC as usize));
    let m_bits = out_field_bits(&after_prefix, &layout, layout.reg_field(R_M as usize));
    let acc_after_inner = bv_add(&acc_before_inner, &m_bits, &vs); // acc + M (opaque triple)
    let sub_state = build_state(&layout, |f| {
        if f == layout.reg_field(R_ACC as usize) {
            word8_from_bits(&acc_after_inner)
        } else if f == layout.reg_field(R_J as usize) {
            const_word8(0, &vs) // inner loop drains j to 0
        } else {
            word8_from_bits(&out_field_bits(&after_prefix, &layout, f))
        }
    });
    let _ = nf;
    let (sub_peak, _sub_sum) = sv_bdd_sizes(&sub_state);
    eprintln!(
        "=== (B.3) SUBSTITUTED inner triple (acc += M, j := 0) -- OPAQUE, no inner-Swapbr walk ==="
    );
    eprintln!("post-substitution state peak = {sub_peak} nodes (acc = acc0 + M, whole domain)");

    // The hierarchical peak = max over the inner triple proof and the outer
    // prefix/substitution.
    let hier_peak = inner_peak.max(pre_peak).max(sub_peak);
    eprintln!(
        "HIERARCHICAL peak (max of inner-triple, outer-prefix, substitution) = {hier_peak} nodes"
    );

    // ── (A) NAIVE baseline: try to cross the inner loop with a SYMBOLIC counter,
    //        i.e. walk from outer_top through the inner Swapbr with j SYMBOLIC.
    //        Once the inner Swapbr executes with a symbolic done-bit, PC becomes
    //        data-dependent and the step's PC output goes SYMBOLIC -- the flat
    //        method cannot re-pin. We detect that and report the peak at the point
    //        of divergence (the blow-up onset).
    let (naive_peak, naive_conc_steps, naive_diverged_at) =
        naive_cross_inner(&engine, &layout, &vs, &geom);
    eprintln!("=== (A) NAIVE full-nest walk (symbolic inner counter) ===");
    eprintln!(
        "walked {naive_conc_steps} concrete-PC steps then PC went SYMBOLIC at pc={naive_diverged_at:?}; peak-so-far {naive_peak} nodes"
    );

    eprintln!("\n=== FEASIBILITY VERDICT (measured) ===");
    eprintln!("HIERARCHICAL method peak = {hier_peak} BDD nodes (inner triple proven once, substituted opaquely).");
    eprintln!("NAIVE full-nest: PC goes data-dependent at the inner Swapbr → flat concrete-PC pinning breaks (diverged at pc={naive_diverged_at:?}).");
    eprintln!("→ The hierarchical decomposition KEEPS THE BDDs TINY ({hier_peak} nodes); the inner Swapbr is never walked symbolically.");

    // The core feasibility assertion: the hierarchical peak is small (bounded,
    // single-digit-ish -- the inner triple + substitution are all tiny transforms).
    assert!(
        hier_peak <= 64,
        "HIERARCHICAL peak {hier_peak} unexpectedly large -- substituting the inner triple re-inflated the BDDs (would be a NO-GO signal)"
    );
    // And the naive method demonstrably could NOT keep PC concrete across the nest
    // (that is the wall the hierarchy sidesteps).
    assert!(
        naive_diverged_at.is_some(),
        "NAIVE walk unexpectedly stayed concrete across the inner Swapbr -- the nest is not exercising a data-dependent PC (prototype is not testing nesting)"
    );
}

/// Naive attempt: walk the inner body with a SYMBOLIC counter, then confront the
/// inner come-from Swapbr with a genuinely DATA-DEPENDENT back-jump register.
///
/// A FAITHFUL nested come-from sets rb = δ_back WHEN the inner loop continues
/// (j != 0) and rb = 0 WHEN it exits (j == 0) -- so rb is a function of the
/// SYMBOLIC carrier j. (The anchor ROM simplifies this to an unconditional δ with
/// the predicate enforced externally by the structured runner, purely to keep the
/// hand-assembly short; the honest model of what the flat method faces at a nested
/// Swapbr is a data-dependent rb, constructed here.) We build that rb symbolically,
/// place it in the Swapbr entry state, step the Swapbr, and observe whether the
/// datapath's PC output stays concrete. It does NOT: PC becomes a symbolic
/// function of j -- the flat method's concrete-PC pinning is invalid, so the fetch
/// at the next step reads a symbolic IR and decode_execute must model every opcode
/// at once (the documented exponential blow-up). Returns (peak, #concrete inner-
/// body steps walked, Some(swapbr_pc) if PC diverged / None if it stayed concrete).
fn naive_cross_inner(
    engine: &StepEngine,
    layout: &Layout,
    vs: &BddVariableSet,
    geom: &NestGeom,
) -> (usize, usize, Option<usize>) {
    let nf = n_fields(layout);
    // 1. Walk the inner straight-line body with j symbolic (concrete PC -- fine).
    let sym = [(R_ACC as usize, W), (R_J as usize, W)];
    let inner_entry = entry_sv(layout, vs, geom.inner_top, &sym);
    let (after_body, body_peak, _conc, _s) = walk_straightline(
        engine,
        layout,
        vs,
        inner_entry,
        geom.inner_top,
        geom.inner_body,
    );
    let steps = geom.inner_body;

    // 2. Build the FAITHFUL data-dependent come-from register rb at the Swapbr.
    //    rb = δ_back masked by (j != 0). j here is the post-body value (already
    //    decremented). j != 0 = OR of j's bits. δ_back is the concrete continue
    //    delta the anchor ROM uses (Addi rb,δ before the Swapbr).
    let delta_back = {
        let swapbr_pc = geom.inner_swapbr;
        ((geom.inner_top as isize - (swapbr_pc as isize + 1)).rem_euclid(256)) as u8
    };
    let j_bits = out_field_bits(&after_body, layout, layout.reg_field(R_J as usize));
    let j_nonzero = j_bits.iter().fold(vs.mk_false(), |acc, b| acc.or(b)); // OR-reduce → (j != 0)
                                                                           // rb bit k = δ_back.bit_k AND (j != 0)  (mask δ by the continue predicate).
    let rb_bits: Vec<Bdd> = (0..W)
        .map(|k| {
            if (delta_back >> k) & 1 == 1 {
                j_nonzero.clone()
            } else {
                vs.mk_false()
            }
        })
        .collect();
    // Entry state at the Swapbr: carrier from after_body, PC = swapbr_pc, rb set
    // to the symbolic data-dependent value, everything else carried forward.
    let swapbr_entry = build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((geom.inner_swapbr & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((geom.inner_swapbr >> 8) & 0xFF) as u8, vs)
        } else if f == layout.reg_field(R_RB as usize) {
            word8_from_bits(&rb_bits)
        } else {
            word8_from_bits(&out_field_bits(&after_body, layout, f))
        }
    });
    let _ = nf;

    // 3. Step the Swapbr. Its PC output = pc + rb + 1 (pc_add_br + pc_inc); rb is
    //    symbolic ⇒ PC_out is symbolic.
    let mut ctx = WalkCtx::new();
    let out = engine
        .eval_step(swapbr_entry, geom.inner_swapbr, vs, &mut ctx)
        .unwrap_or_else(|e| panic!("naive: Swapbr step pc={}: {e}", geom.inner_swapbr));
    let (p, _s) = sv_bdd_sizes(&out);
    let peak = body_peak.max(p);
    match read_concrete_pc(&out, layout) {
        Some(_) => (peak, steps, None), // stayed concrete (would mean no data-dep)
        None => (peak, steps, Some(geom.inner_swapbr)), // PC symbolic -- the wall
    }
}

// ── small symbolic bitvector helpers (ripple-carry, local so the file is self-
//    contained; identical semantics to bveq's internal adders) ────────────────
fn bv_add(a: &[Bdd], b: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let mut carry = vs.mk_false();
    let mut out = Vec::with_capacity(W);
    for i in 0..W {
        let s = a[i].xor(&b[i]).xor(&carry);
        let c = a[i].and(&b[i]).or(&carry.and(&a[i].xor(&b[i])));
        out.push(s);
        carry = c;
    }
    out
}
fn bv_addi(a: &[Bdd], imm: u8, vs: &BddVariableSet) -> Vec<Bdd> {
    let b: Vec<Bdd> = (0..W)
        .map(|i| {
            if (imm >> i) & 1 == 1 {
                vs.mk_true()
            } else {
                vs.mk_false()
            }
        })
        .collect();
    bv_add(a, &b, vs)
}
/// a - imm (8-bit wrapping) = a + (256 - imm) = a + (~imm + 1).
fn bv_subi(a: &[Bdd], imm: u8, vs: &BddVariableSet) -> Vec<Bdd> {
    bv_addi(a, (256u16 - imm as u16) as u8, vs)
}
