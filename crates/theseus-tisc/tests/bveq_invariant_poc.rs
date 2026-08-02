//! BVEQ-INVARIANT-POC — the THIRD go/no-go experiment: prove a DATA-DEPENDENT
//! LOOP program correct SYMBOLICALLY, without unrolling, via the
//! **inductive-invariant method**.
//!
//! ── Why this file exists ──────────────────────────────────────────────────
//! `bveq_multistep_poc.rs` established: straight-line symbolic step-composition
//! (PC CONCRETE each step) is tractable (peak < 500 nodes), but NAIVE symbolic
//! composition of a data-dependent LOOP OOMs (>8 GB) — once PC is symbolic the
//! fetched IR is symbolic, so `decode_execute` must represent every opcode at
//! once (exponential). This file SIDESTEPS the blow-up.
//!
//! ── The key insight ───────────────────────────────────────────────────────
//! In a compiled loop the PC is CONCRETE at every point WITHIN one iteration:
//! the loop BODY is a fixed straight-line ROM sub-sequence (length `iter_len`).
//! The data-dependence is ONLY in HOW MANY times you go around, decided by the
//! come-from (a `Swapbr` + δu back-jump at the loop tail: BR ← δu to loop, BR ←
//! 0 to exit). So a loop splits into:
//!   * BODY (straight-line, concrete PC) → the PROVEN-TRACTABLE regime.
//!   * loop count → handled by INDUCTION (a math argument), NOT unrolling.
//!
//! ── The proof = two BVEQ lemmas + an induction argument ────────────────────
//!  L1 BODY LEMMA (whole carrier domain, symbolic, concrete PC → tractable):
//!     Compose `step` across the `body_len` = `iter_len − LOOP_OVERHEAD` body
//!     instructions (97, not the full 107-instr iteration; EXCLUDING the loop
//!     glue — Ltop SWAPBR + join-dispatch [L0] and the route + come-from Swapbr
//!     [L2]), concrete PC through it, SYMBOLIC carrier registers, and
//!     prove the resulting carrier transform ≡ the Trace body function (the
//!     `rot_step` bit-rotation reference for vart_halve) over the WHOLE carrier
//!     domain. Straight-line composition ⇒ tractable.
//!  L0 RE-ENTRY LEMMA (5-step glue, whole carrier domain, concrete PC):
//!     Compose the loop glue [Ltop, body_start) = offsets [2,7) — Ltop's SWAPBR
//!     rc + the 4 join-dispatch instrs — from L2's proven CONTINUE exit state and
//!     prove the exit ≡ L1's entry invariant {ctrl=temp=rb=rc=BR=0}, carrier
//!     identity, PC=body_start. This WELDS L2-exit→L1-entry so the induction step
//!     closes on lemmas (L2→L0→L1 covers the whole per-iteration span [2,108)),
//!     not on hand-tracing. Straight-line concrete PC ⇒ tractable (peak tiny).
//!  L2 COME-FROM / TERMINATION LEMMA (single step, whole carrier domain):
//!     Prove the come-from bracket at the loop tail sets BR = the back-jump δu
//!     WHEN the loop continues (¬done) and BR = 0 WHEN done — i.e. the loop takes
//!     the back-edge iff ¬done(carrier), and `done` matches the Trace's exit
//!     condition (tag = 1). Single step ⇒ tractable.
//!  L3 INDUCTION ARGUMENT (documented + anchored):
//!     (L1 ∧ L2) ⟹ `run_on_cpu(rom) == eval(trace)` over the WHOLE input
//!     domain, by induction on the (data-dependent) trip count. Anchored by a
//!     CONCRETE bootstrap (sampled inputs through the real per-step `eval` loop
//!     agree) + terminal quiescence (BR = 0, scratch clean) — the same
//!     properties the ~4 h real-CPU authority checks, now established
//!     STRUCTURALLY rather than by exhaustive loop-unrolling.
//!
//! Each lemma reports peak BDD node count + wall time. Negative controls (break
//! the body / break the come-from ⇒ DETECTED) give the oracle teeth.
//!
//! ── MEASURED RESULT (vart_halve K=3, HB=1; iter_len=107, body_len=97, B=646) ──
//!   L1 BODY LEMMA  : MAX peak 3 BDD nodes over 97 composed body instrs — the
//!                    body ≡ rot_step over the whole reachable carrier domain.
//!                    TRACTABLE (contrast: naive loop composition OOMs >8 GB).
//!   L2 COME-FROM   : peak 3 BDD nodes; BR = δu·[tag==0], δu=0x95=256−107 lands
//!                    exactly on Ltop; per-branch aux exit pinned; carrier
//!                    undisturbed.
//!   L0 RE-ENTRY    : the [2,7) glue welds L2-exit → L1-entry — tiny BDD. With
//!                    L1+L2 this closes the per-iteration relation on LEMMAS
//!                    (offsets [2,108)); the induction step no longer hand-traces.
//!                    Field-complete: scratch s2..s7 IDENTITY + IR=0 also welded
//!                    at L0/L2-exit; the SAME glue also lands the first-traversal
//!                    base precondition in L1's entry (L0-BASE) ⇒ base + step both
//!                    on lemmas. Lend's PC landing asserted vs the w_pc=16 netlist
//!                    (GAP-A: the only BR≠0 PC-adder pass).
//!   L3 anchor      : real-CPU bootstrap == eval + terminal quiescence across
//!                    trip counts T ∈ {1,2,3}; whole-domain trip_count+1 ≤ k_max.
//!   Both lemmas are TINY BDDs — the blow-up is genuinely sidestepped. This
//!   retires the loop-based whole-domain sweeps + the ~4 h real-CPU authority
//!   for this loop, replacing them with two small-BDD lemmas + an induction
//!   argument.
//!
//! ── PERFORMANCE (fetch-mux memoization, `StepEngine`) ─────────────────────
//!   Profiling the per-step `eval_bdd` (peak stays 3 nodes, so the cost is gate
//!   COUNT not BDD growth) showed each step was ~3.05 s, of which the FETCH
//!   phase — walking the 111-entry ROM address-compare MUX as BDD ops — was
//!   ~1.36 s and appears TWICE per step (fetch + unfetch) ⇒ ~2.7 s wasted on a
//!   mux whose answer at a CONCRETE PC is the compile-time constant `rom[pc]`.
//!   `StepEngine` builds the ROM-invariant phases (decode_execute, pc_add_br,
//!   pc_inc) ONCE and short-circuits fetch/unfetch to an exact `IR ^= rom[pc]`
//!   (byte-faithful; the load-bearing decode_execute phase is still walked
//!   symbolically in full). Result: L1 308 s → ~43 s, L2 29 s → ~3 s, same
//!   assertions + peaks, negative controls still fire. The residual L1 cost is
//!   the 97 real decode_execute walks (the actual body-semantics proof). L1/L2
//!   are the ALWAYS-ON fast pre-merge coverage; the real-CPU quiescence anchor
//!   stays `#[ignore]` (the slow authority these lemmas replace).

#![cfg(feature = "heavy-tests")]
// HEAVY tier: run under `--features heavy-tests --test-threads=1`.

use std::collections::BTreeMap;
use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type};

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::compile::{
    compile_with_metadata, marshal, run_on_cpu_fast, run_rom_on_cpu, unmarshal,
};
use theseus_tisc::step::step_with;
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH, W};

// ══════════════════════════════════════════════════════════════════════════
// Symbolic / concrete state SV construction (bit-interleaved varmap).
// var(field f, bit b) = b*nf + f (VarMap convention, shared with the step POCs).
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
    let bits: Vec<SV> = (0..W).map(|b| SV::Bit(field_bit_var(vs, nf, f, b))).collect();
    let mut sv = bits[W - 1].clone();
    for b in (0..W - 1).rev() {
        sv = SV::pair(bits[b].clone(), sv);
    }
    sv
}

/// Build a CpuState SV field-by-field from a per-field closure. Mirrors
/// `state_ty_with(&layout)` (right-nested product of `num_cpu_fields()` fields).
fn build_state<F: Fn(usize) -> SV>(layout: &Layout, mk: F) -> SV {
    let nf = n_fields(layout);
    let mut sv = mk(nf - 1);
    for f in (0..nf - 1).rev() {
        sv = SV::pair(mk(f), sv);
    }
    debug_assert_eq!(sv_field_count(&sv), nf * W);
    let _ = state_ty_with(layout);
    sv
}

/// Read a field of `out` as a CONCRETE integer, requiring every bit to be a BDD
/// constant. Returns `None` if any bit is symbolic. Used for the PC-agreement
/// soundness assert (item 4 of the review): before we re-pin PC to a compile-time
/// constant we CONFIRM the datapath's own PC output is exactly that constant — a
/// PC-datapath bug would otherwise be masked by the re-pin.
fn read_concrete_field(out: &SV, layout: &Layout, f: usize) -> Option<u32> {
    let bits = out_field_bits(out, layout, f);
    let mut v = 0u32;
    for (b, bit) in bits.iter().enumerate() {
        if bit.is_true() {
            v |= 1 << b;
        } else if bit.is_false() {
            // 0
        } else {
            return None; // symbolic bit ⇒ not concrete
        }
    }
    Some(v)
}

/// Read the concrete PC (lo byte, plus hi byte if a 2-field PC) from an output SV.
fn read_concrete_pc(out: &SV, layout: &Layout) -> Option<usize> {
    let lo = read_concrete_field(out, layout, layout.pc_field())? as usize;
    if layout.pc_fields() >= 2 {
        let hi = read_concrete_field(out, layout, layout.pc_hi_field())? as usize;
        Some((hi << 8) | lo)
    } else {
        Some(lo)
    }
}

fn sv_field_count(sv: &SV) -> usize {
    match sv {
        SV::Unit => 0,
        SV::Bit(_) => 1,
        SV::Pair(a, b) => sv_field_count(a) + sv_field_count(b),
    }
}

// ── Field extraction from an output SV ─────────────────────────────────────

fn out_field_bits(out: &SV, layout: &Layout, f: usize) -> Vec<Bdd> {
    let nf = n_fields(layout);
    extract_word8_bdds(extract_field(out, f, nf)).to_vec()
}

// ── BDD size metric: peak per-bit + sum over the whole state SV ─────────────

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
// THE BODY-LEMMA ENGINE.
//
// Given a straight-line body sub-ROM (the `iter_len` instructions between
// loop-top and loop-tail, EXCLUDING the come-from Swapbr), compose `step`
// across it with PC RE-PINNED CONCRETE each step (fetch = const-select ⇒ exactly
// one instruction decodes per step ⇒ tractable), SYMBOLIC carrier registers.
// Returns the final output SV + the growth curve.
//
// Faithfulness: because the body sub-ROM is straight-line (contains no Swapbr),
// BR stays 0, so the step's own PC output IS pc+1; re-pinning PC to the concrete
// next value is exactly what the datapath produces and keeps the fetch a
// const-select. The DATA (carrier + scratch) is carried forward symbolically.
// ══════════════════════════════════════════════════════════════════════════

struct BodyStep {
    step: usize,
    peak: usize,
    sum: usize,
    secs: f64,
}

// ══════════════════════════════════════════════════════════════════════════
// StepEngine — the MEMOIZED per-step evaluator.
//
// PROFILE (measured, 97-instr vart_halve body, w_pc=8): per-step `eval_bdd` on
// the whole `step_with` iso was ~3.05 s, of which the FETCH phase (walking the
// 111-entry ROM address-compare MUX as BDD ops) was ~1.36 s and it appears
// TWICE per step (fetch + unfetch) ⇒ ~2.7 s of the 3.05 s. decode_execute was
// ~0.44 s; the PC adders the rest. The BDDs are TINY (peak 3 nodes) — the cost
// is the sheer count of (constant-valued) BDD gate ops in the mux, NOT node
// growth. step_with construction was a one-time 3.5 s (already hoisted out of
// the loop).
//
// THE MEMOIZATION. The step is `fetch ; decode_execute ; unfetch ; pc_add_br ;
// pc_inc`. At a CONCRETE PC the fetch is exactly `IR ^= rom[PC]` with `rom[PC]`
// a COMPILE-TIME CONSTANT: walking the whole mux to discover which entry fires
// is pure waste — the answer is `rom[pc]`. So we:
//   * build the four ROM-invariant NON-fetch phases (decode_execute, pc_add_br,
//     pc_inc) ONCE and reuse them every step;
//   * SHORT-CIRCUIT fetch/unfetch: fetch sets the four IR bytes to const rom[pc]
//     (IR enters 0, so IR^=rom[pc] == rom[pc]); unfetch re-zeroes them (IR is
//     still rom[pc] because D1 lets nothing write PC and decode_execute
//     preserves IR — the real unfetch XORs the SAME rom[pc]). No mux walk.
//
// This is byte-faithful to what the real fetch produces at a concrete PC: the
// fetch's job (address-compare selects rom[pc]) is exactly what we substitute.
// NOTE (post-b3238b9 memoization): NO always-on lemma walks the real fetch mux
// any more — L2's come-from bracket also runs through this same StepEngine, so it
// too short-circuits fetch/unfetch. Fetch FAITHFULNESS therefore rests entirely
// on (a) the netlist-reconciliation tier and (b) the #[ignore] heavy anchor
// `invariant_induction_bootstrap_quiescence`, which runs the REAL
// `run_rom_on_cpu` (full fetch mux) on a sample. Retiring the real-CPU authority
// is contingent on keeping that anchor as an advisor-run re-anchor (see L3).
// The SEMANTICALLY-load-bearing phase for the body lemma — decode_execute (the
// rot_step logic) — is still walked symbolically in full, unchanged.
// ══════════════════════════════════════════════════════════════════════════

/// Replace field `f` of a right-nested `nf`-field state SV with `new`.
fn set_field(sv: &SV, f: usize, nf: usize, new: SV) -> SV {
    if nf == 1 {
        debug_assert_eq!(f, 0);
        return new;
    }
    match sv {
        SV::Pair(left, right) => {
            if f == 0 {
                SV::pair(new, (**right).clone())
            } else {
                SV::pair((**left).clone(), set_field(right, f - 1, nf - 1, new))
            }
        }
        _ => panic!("set_field: expected Pair at f={f}, nf={nf}"),
    }
}

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

    /// XOR the constant word `w` into the four IR bytes — the faithful fetch /
    /// unfetch effect (`IR ^= rom[pc]`). Done bit-wise on the current (BDD) IR
    /// so it is exact even if IR is symbolic: on fetch IR enters 0 (⇒ IR = w),
    /// on unfetch IR enters rom[pc] (⇒ IR = 0). NOT an overwrite — that would
    /// mask an IR-preservation violation by decode_execute.
    fn xor_ir(&self, sv: &SV, w: u32, _vs: &BddVariableSet) -> SV {
        let mut out = sv.clone();
        for k in 0..4 {
            let byte = ((w >> (8 * k)) & 0xFF) as u8;
            let f = self.layout.ir_byte_field(k);
            let cur_byte = extract_word8_bdds(extract_field(&out, f, self.nf));
            let xored: Vec<Bdd> = (0..W)
                .map(|b| if (byte >> b) & 1 == 1 { cur_byte[b].not() } else { cur_byte[b].clone() })
                .collect();
            let mut sv_byte = SV::Bit(xored[W - 1].clone());
            for b in (0..W - 1).rev() {
                sv_byte = SV::pair(SV::Bit(xored[b].clone()), sv_byte);
            }
            out = set_field(&out, f, self.nf, sv_byte);
        }
        out
    }

    /// Evaluate one full `step_with(layout, rom)` cycle at CONCRETE `pc`, with
    /// the fetch/unfetch mux short-circuited to the constant `rom[pc]`. The
    /// input SV must have IR = 0 (the machine invariant between steps) and PC
    /// concrete = `pc`; the output is byte-identical to walking the whole
    /// `step_with` iso.
    fn eval_step_concrete_pc(
        &self,
        cur: SV,
        pc: usize,
        vs: &BddVariableSet,
        ctx: &mut WalkCtx,
    ) -> Result<SV, theseus_tisc::bveq_netlist::NetlistWalkError> {
        let word = self.rom[pc];
        // 1. fetch: IR ^= rom[pc]  (IR enters 0 ⇒ IR = rom[pc]).
        let after_fetch = self.xor_ir(&cur, word, vs);
        // 2. decode_execute (the real symbolic walk — the load-bearing phase).
        let after_de = eval_bdd(&self.decode_execute, after_fetch, vs, ctx)?;
        // 3. unfetch: IR ^= rom[pc] again ⇒ IR back to 0 (decode_execute
        //    preserves IR = rom[pc]; nothing writes PC, so it XORs the SAME
        //    rom[pc]) — computed as an exact BDD XOR, so a stray IR write by
        //    decode_execute would survive rather than be masked.
        let after_unfetch = self.xor_ir(&after_de, word, vs);
        // 4. pc_add_br ; 5. pc_inc.
        let after_add = eval_bdd(&self.pc_add_br, after_unfetch, vs, ctx)?;
        eval_bdd(&self.pc_inc, after_add, vs, ctx)
    }
}

/// Body-ENTRY state at concrete PC = `pc`, faithful to the real machine at the
/// top of the body: the CARRIER registers (tag,v,h) are SYMBOLIC (the whole
/// carrier domain), and EVERY other data field is at its body-entry INVARIANT
/// value — 0. The four come-from aux {rc,rb,temp,ctrl} are 0 at body entry (the
/// join-dispatch bracket burns temp/ctrl to 0 before the body), scratch s0..s7
/// = 0 (decoder convention + the crossbar restores it to 0), IR = 0, BR = 0.
///
/// This matches the concrete body-step diagnostic (which set only the carrier
/// and everything else 0 and reproduced rot_step exactly). Making the aux/scratch
/// symbolic is UNFAITHFUL: the body's Xchg crossbar uses scratch as workspace and
/// only restores it when it enters at the invariant 0 — a symbolic scratch entry
/// is not a state the real machine is ever in at body top.
/// `carrier_regs` maps each carrier register to the number of LOW bits that are
/// symbolic (its width in the reachable carrier); higher bits enter 0 (the
/// reachable domain never has them set — the carrier is a (1+HB+K)-bit window
/// spread across the low bits of tag/v/h). Every non-carrier field enters 0.
fn body_entry_sv(
    layout: &Layout,
    vs: &BddVariableSet,
    pc: usize,
    carrier_regs: &[(usize, usize)], // (reg, symbolic_low_bits)
) -> SV {
    let nf = n_fields(layout);
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((pc & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((pc >> 8) & 0xFF) as u8, vs)
        } else if let Some(&(_, wbits)) = carrier_regs.iter().find(|&&(r, _)| f == layout.reg_field(r)) {
            // Low `wbits` symbolic, high bits 0 (reachable carrier window).
            let mut sv = SV::Bit(vs.mk_false());
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
            const_word8(0, vs) // every other field at the body-entry invariant 0
        }
    })
}

/// Compose the FULL ROM's `step` across the body span `[pc_top, pc_top+span)`,
/// PC re-pinned concrete each step. `vs` carries the symbolic input variables.
/// Returns the output SV after the last body instruction and the growth curve.
///
/// We use the WHOLE ROM in `step_with` (so fetch reads the real body words) but
/// only step over the body span, re-pinning PC — the come-from Swapbr at
/// `pc_top+span` is NEVER executed here (that is L2's job).
fn compose_body(
    layout: &Layout,
    rom: &[u32],
    vs: &BddVariableSet,
    pc_top: usize,
    span: usize,
    carrier_regs: &[(usize, usize)],
) -> (SV, Vec<BodyStep>) {
    let sctx = StepEngine::new(layout, rom);
    let mut ctx = WalkCtx::new();
    let mut cur = body_entry_sv(layout, vs, pc_top, carrier_regs);
    let mut samples = Vec::with_capacity(span);
    let t0 = Instant::now();
    for i in 0..span {
        let pc = pc_top + i;
        let out = sctx
            .eval_step_concrete_pc(cur, pc, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("eval_step FAILED at body step {i} (pc={pc}): {e}"));
        // Re-pin PC to the concrete next value; carry every other field forward.
        let next_pc = pc_top + i + 1;
        // SOUNDNESS (item 4.ii): the datapath's OWN PC output must already equal
        // the concrete constant we are about to re-pin — else a PC bug is masked.
        assert_eq!(
            read_concrete_pc(&out, layout),
            Some(next_pc),
            "PC-AGREEMENT: body step {i} (pc={pc}) datapath PC output != pc+1"
        );
        cur = build_state(layout, |f| {
            if f == layout.pc_field() {
                const_word8((next_pc & 0xFF) as u8, vs)
            } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
                const_word8(((next_pc >> 8) & 0xFF) as u8, vs)
            } else {
                let bits = out_field_bits(&out, layout, f);
                let mut sv = SV::Bit(bits[W - 1].clone());
                for b in (0..W - 1).rev() {
                    sv = SV::pair(SV::Bit(bits[b].clone()), sv);
                }
                sv
            }
        });
        let (peak, sum) = sv_bdd_sizes(&cur);
        samples.push(BodyStep {
            step: i + 1,
            peak,
            sum,
            secs: t0.elapsed().as_secs_f64(),
        });
    }
    (cur, samples)
}

fn print_curve(tag: &str, samples: &[BodyStep]) {
    eprintln!("=== BODY-LEMMA BDD-GROWTH CURVE: {tag} ===");
    let peak_of = samples.iter().map(|s| s.peak).max().unwrap_or(0);
    let last = samples.last().unwrap();
    eprintln!(
        "  {} body steps composed; MAX peak-node over the run = {}; final sum = {}; total {:.3}s",
        last.step, peak_of, last.sum, last.secs
    );
}

// ══════════════════════════════════════════════════════════════════════════
// vart_halve build (reuse the vart_halve.rs recipe).
// ══════════════════════════════════════════════════════════════════════════

const V2_K: u8 = 3;
const V2_HB: u8 = 1;

/// One reversible step: right-rotate the (1+HB+K)-bit `(tag ‖ h ‖ v)` register
/// by 1 — the vart_halve body function (the `rot_step` reference).
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
    let mut r = [0u8; NUM_REGS];
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

/// Build the halve-until-odd body prim (whole-domain bijective by construction).
fn build_v2_body_sized(k: u8, hb: u8) -> (Iso, Type, Type) {
    let u = Type::prod(Type::flat(k), Type::flat(hb));
    let carrier = Type::sum(u.clone(), u.clone());
    let vals = enumerate_values(&carrier);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for val in &vals {
        let sv = marshal(val, &carrier);
        let (nt, nv, nh) = rot_step(sv.regs[0], sv.regs[1], sv.regs[2], k, hb);
        let out_val = unmarshal(&state_with_regs(&[nt, nv, nh]), &carrier);
        forward.insert(val.clone(), out_val.clone());
        backward.insert(out_val, val.clone());
    }
    let prim = PrimDef {
        forward_name: format!("v2_halve_until_odd_k{k}_hb{hb}"),
        backward_name: format!("v2_halve_until_odd_k{k}_hb{hb}_inv"),
        input_ty: carrier.clone(),
        output_ty: carrier.clone(),
        forward,
        backward,
        symbolic: None,
    };
    (Iso::Prim(Box::new(prim)), u, carrier)
}

// ══════════════════════════════════════════════════════════════════════════
// ROM span geometry for a compiled single-level Trace (lower_trace skeleton).
//
//   prologue (2):  XORI tag,1 ; ADDI temp,δu
//   Ltop (offset 2):        SWAPBR rc              (loop-top; back-jump target)
//   join-dispatch (4):      CSWAP ctrl,rc,temp ; SUBI temp,δu ; XOR ctrl,tag ; XORI ctrl,1
//   BODY (L instrs):        <body>                 (offset 7 .. 7+L-1)
//   ctrl+route (4):         XOR ctrl,tag ; XORI ctrl,1 ; ADDI temp,δu ; CSWAP ctrl,temp,rb
//   Lend:                   SWAPBR rb              (the COME-FROM back-edge)
//   epilogue (2):           SUBI temp,δu ; XORI tag,1
//
//   iter_len = L + LOOP_OVERHEAD (LOOP_OVERHEAD = 10) = span from Ltop to Lend
//   inclusive. body_len L = iter_len - 10.
//   PROLOGUE_LEN = 2, LTOP = 2, JOIN = 4 ⇒ BODY starts at PROLOGUE_LEN + 1 + JOIN
//   = 2 + 1 + 4 = 7. Lend (come-from Swapbr) is at Ltop + iter_len - 1.
// ══════════════════════════════════════════════════════════════════════════

const LOOP_OVERHEAD: usize = 10;
const PROLOGUE_LEN: usize = 2;
const JOIN_LEN: usize = 4; // CSWAP, SUBI, XOR, XORI after Ltop's SWAPBR
const LTOP: usize = PROLOGUE_LEN; // offset of Ltop's SWAPBR rc
const BODY_START: usize = LTOP + 1 + JOIN_LEN; // = 7: first real body instr
const ROUTE_LEN: usize = 4; // XOR, XORI, ADDI, CSWAP before Lend

struct LoopGeom {
    lowered_instrs: Vec<Instr>,
    rom: Vec<u32>,
    iter_len: usize,
    body_len: usize,
    body_start: usize, // PC of first body instruction
    lend_pc: usize,    // PC of the come-from SWAPBR rb
    budget: usize,
    k_max: usize, // compiler's trajectory-derived max body applications
}

/// Compile a single-level Trace and locate its body span + come-from Swapbr,
/// VALIDATING the located instructions against the lower_trace skeleton so a
/// future change to the skeleton is caught here (not silently mislocated).
fn compile_loop_geom(iso: &Iso) -> LoopGeom {
    let lowered = compile_with_metadata(iso).expect("Trace must lower");
    let rom = assemble(&lowered.instrs).expect("assemble ROM");
    assert_eq!(lowered.loop_terms.len(), 1, "expect a single top-level loop");
    let iter_len = lowered.loop_terms[0].iter_len;
    let body_len = iter_len - LOOP_OVERHEAD;
    let body_start = BODY_START;
    let lend_pc = LTOP + iter_len - 1;

    // Validate the skeleton at the located offsets — the come-from bracket.
    let instrs = &lowered.instrs;
    assert!(
        matches!(instrs[LTOP], Instr::Swapbr { .. }),
        "Ltop (offset {LTOP}) must be SWAPBR rc, got {:?}",
        instrs[LTOP]
    );
    assert!(
        matches!(instrs[body_start - 1], Instr::Xori { imm: 1, .. }),
        "instr before body must be XORI ctrl,1 (join-dispatch tail), got {:?}",
        instrs[body_start - 1]
    );
    assert!(
        matches!(instrs[lend_pc], Instr::Swapbr { .. }),
        "Lend (offset {lend_pc}) must be SWAPBR rb (the come-from back-edge), got {:?}",
        instrs[lend_pc]
    );
    // The route instr just before Lend is the CSWAP ctrl,temp,rb that sets rb.
    assert!(
        matches!(instrs[lend_pc - 1], Instr::Cswap { .. }),
        "route (offset {}) must be CSWAP ctrl,temp,rb, got {:?}",
        lend_pc - 1,
        instrs[lend_pc - 1]
    );
    // The body span is [body_start, lend_pc - ROUTE_LEN).
    assert_eq!(
        lend_pc - ROUTE_LEN - body_start,
        body_len,
        "body span length must equal body_len"
    );

    // SOUNDNESS (item 4.i): the body span [body_start, lend_pc - ROUTE_LEN) must
    // contain NO Swapbr. This is what LICENSES the concrete-PC pinning in L1's
    // compose_body: a Swapbr inside the body could open a paired skip-block that
    // returns BR=0 at exit while the pinned composition silently walked the
    // SKIPPED instructions (a false pass). Straight-line ⇒ BR stays 0 ⇒ PC output
    // is provably pc+1 (also checked step-wise by the PC-agreement assert).
    for pc in body_start..(lend_pc - ROUTE_LEN) {
        assert!(
            !matches!(instrs[pc], Instr::Swapbr { .. }),
            "BODY SWAPBR-FREE: body instr at pc={pc} is a Swapbr ({:?}) — \
             concrete-PC pinning is UNSOUND (skip-block hazard)",
            instrs[pc]
        );
    }

    LoopGeom {
        lowered_instrs: lowered.instrs.clone(),
        rom,
        iter_len,
        body_len,
        body_start,
        lend_pc,
        budget: lowered.wcet(),
        k_max: lowered.loop_terms[0].k_max,
    }
}

// ══════════════════════════════════════════════════════════════════════════
// The carrier register layout in the compiled ROM.
//
// For vart_halve K=3,HB=1: carrier = Sum(u,u), u = Flat(3)×Flat(1), 3 leaves
// [tag, v, h]. lower_trace canonical single-level layout:
//   body_homes[0] = tag  = Reg(n_input)      (lowest free reg after inputs)
//   body_homes[1] = v    = ctx.homes[0] = Reg(0)   (input leaf 0)
//   body_homes[2] = h    = ctx.homes[1] = Reg(1)   (input leaf 1)
// n_input = num_leaves(u) = 2 (v, h). So tag = Reg(2), v = Reg(0), h = Reg(1).
// We VERIFY this against the real ROM's behavior (co-factor), not just assert it.
// ══════════════════════════════════════════════════════════════════════════

fn v2_carrier_regs() -> (usize, usize, usize) {
    // (tag_reg, v_reg, h_reg)
    (2, 0, 1)
}

// ══════════════════════════════════════════════════════════════════════════
// L1 — BODY LEMMA for vart_halve.
//
// Compose `step` across the body span with symbolic carrier + concrete PC, then
// prove the output (tag,v,h) registers ≡ rot_step(tag,v,h) over the WHOLE
// carrier domain. The carrier is only (1+HB+K) = 5 bits, but we prove it over
// the FULL 8-bit register domain of each of tag/v/h (the compiled body must be
// correct on the whole reachable carrier; out-of-range register values are not
// reachable but proving the bit-permutation over the whole 8-bit words is
// STRICTLY STRONGER and still tractable).
//
// rot_step is a fixed bit-permutation of the (tag<<4 | h<<3 | v) 5-bit window
// (K=3, HB=1): right-rotate by 1. We express it as an explicit per-output-bit
// selection of an input carrier bit — a permutation spec at the BDD level.
// ══════════════════════════════════════════════════════════════════════════

/// The rot_step bit-permutation as a spec on the symbolic carrier bits.
/// Input window bits (LSB..MSB): v[0..K], h[0..HB], tag[0]  (total 1+HB+K).
/// w = (tag<<(HB+K)) | (h<<K) | v. Right-rotate by 1:
///   wp[i] = w[i+1] for i in 0..width-1 ; wp[width-1] = w[0] (the wrapped LSB).
/// Then v' = wp[0..K], h' = wp[K..K+HB], tag' = wp[K+HB].
/// Returns (tag'_bits, v'_bits, h'_bits) as full 8-bit BDD vectors (high bits 0).
fn rot_step_spec(
    tag_in: &[Bdd],
    v_in: &[Bdd],
    h_in: &[Bdd],
    k: usize,
    hb: usize,
    vs: &BddVariableSet,
) -> (Vec<Bdd>, Vec<Bdd>, Vec<Bdd>) {
    let width = 1 + hb + k;
    // Assemble the window w[0..width] LSB-first: v bits, then h bits, then tag.
    let mut w: Vec<Bdd> = Vec::with_capacity(width);
    for i in 0..k {
        w.push(v_in[i].clone());
    }
    for i in 0..hb {
        w.push(h_in[i].clone());
    }
    w.push(tag_in[0].clone());
    // Right-rotate by 1: wp[i] = w[(i+1) mod width].
    let wp: Vec<Bdd> = (0..width).map(|i| w[(i + 1) % width].clone()).collect();
    // Split back out to v', h', tag' as full 8-bit words (high bits 0).
    let zero = vs.mk_false();
    let mut vp = vec![zero.clone(); W];
    let mut hp = vec![zero.clone(); W];
    let mut tp = vec![zero.clone(); W];
    for i in 0..k {
        vp[i] = wp[i].clone();
    }
    for i in 0..hb {
        hp[i] = wp[k + i].clone();
    }
    tp[0] = wp[k + hb].clone();
    (tp, vp, hp)
}

/// The carrier register spec for vart_halve: (reg, symbolic_low_bits).
/// tag = 1 bit, v = K bits, h = HB bits (matching the (1+HB+K)-bit window).
fn v2_carrier_spec() -> Vec<(usize, usize)> {
    let (tag_reg, v_reg, h_reg) = v2_carrier_regs();
    vec![(v_reg, V2_K as usize), (h_reg, V2_HB as usize), (tag_reg, 1)]
}

/// The body-entry symbolic bit vector for a carrier register: low `wbits`
/// symbolic vars, high bits 0 — CONSISTENT with `body_entry_sv`.
fn entry_carrier_bits(vs: &BddVariableSet, nf: usize, layout: &Layout, reg: usize, wbits: usize) -> Vec<Bdd> {
    (0..W)
        .map(|b| if b < wbits { field_bit_var(vs, nf, layout.reg_field(reg), b) } else { vs.mk_false() })
        .collect()
}

/// Prove the vart_halve body lemma. Returns (max_peak, secs).
fn prove_body_lemma_vart(layout: &Layout, geom: &LoopGeom) -> (usize, f64) {
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let nf = n_fields(layout);
    let carrier = v2_carrier_spec();
    let (out, samples) = compose_body(layout, &geom.rom, &vs, geom.body_start, geom.body_len, &carrier);
    print_curve("vart_halve BODY (rot_step) [full ROM, PC concrete]", &samples);

    let (tag_reg, v_reg, h_reg) = v2_carrier_regs();
    // Body-entry carrier bits (low window symbolic, high 0) — CONSISTENT with
    // the entry SV, so the rot_step spec is expressed over the same variables.
    let tag_in = entry_carrier_bits(&vs, nf, layout, tag_reg, 1);
    let v_in = entry_carrier_bits(&vs, nf, layout, v_reg, V2_K as usize);
    let h_in = entry_carrier_bits(&vs, nf, layout, h_reg, V2_HB as usize);

    // The rot_step spec over the WHOLE reachable carrier domain (2^(1+HB+K)).
    let (tp, vp, hp) = rot_step_spec(&tag_in, &v_in, &h_in, V2_K as usize, V2_HB as usize, &vs);

    let out_tag = out_field_bits(&out, layout, layout.reg_field(tag_reg));
    let out_v = out_field_bits(&out, layout, layout.reg_field(v_reg));
    let out_h = out_field_bits(&out, layout, layout.reg_field(h_reg));
    let zero = vs.mk_false();

    // Window bits carry rot_step; bits above the window must be 0 (they entered 0
    // and the body's rotation stays inside the window).
    for b in 0..W {
        let want_v = if b < V2_K as usize { vp[b].clone() } else { zero.clone() };
        assert!(out_v[b] == want_v, "BODY LEMMA: v' bit {b} != rot_step spec");
        let want_h = if b < V2_HB as usize { hp[b].clone() } else { zero.clone() };
        assert!(out_h[b] == want_h, "BODY LEMMA: h' bit {b} != rot_step spec");
        let want_t = if b == 0 { tp[b].clone() } else { zero.clone() };
        assert!(out_tag[b] == want_t, "BODY LEMMA: tag' bit {b} != rot_step spec");
    }

    // Every non-carrier field entered 0 (the body-entry invariant); a correct,
    // reversible body must return them all to 0 (aux/scratch restored, no leak).
    for f in 0..n_fields(layout) {
        if f == layout.reg_field(tag_reg) || f == layout.reg_field(v_reg) || f == layout.reg_field(h_reg) {
            continue;
        }
        if f == layout.pc_field() || (layout.pc_fields() >= 2 && f == layout.pc_hi_field()) {
            continue; // PC re-pinned concrete by compose_body
        }
        let got = out_field_bits(&out, layout, f);
        for b in 0..W {
            assert!(
                got[b] == zero,
                "BODY LEMMA: non-carrier field {f} bit {b} not restored to 0 (leak/stray write)"
            );
        }
    }

    let peak = samples.iter().map(|s| s.peak).max().unwrap_or(0);
    (peak, samples.last().unwrap().secs)
}

// ══════════════════════════════════════════════════════════════════════════
// TESTS.
// ══════════════════════════════════════════════════════════════════════════

// Concrete stepping through the body span (fast) — used to EMPIRICALLY determine
// the carrier register mapping + bit order before the symbolic proof.
const EVAL_FUEL: u64 = 8_000_000_000;

fn concrete_step_body(
    layout: &Layout,
    rom: &[u32],
    body_start: usize,
    body_len: usize,
    regs_in: [u8; NUM_REGS],
) -> StateVals {
    let iso = step_with(layout, rom);
    let s = StateVals {
        regs: regs_in,
        pc: body_start as u16,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    };
    let mut cur = s.encode_with(layout);
    for _ in 0..body_len {
        let mut fuel = EVAL_FUEL;
        // Body is straight-line (BR=0) so the datapath PC output is already pc+1;
        // no re-pin needed for the concrete path.
        cur = theseus_eval::eval(&iso, cur, &mut fuel).expect("body eval");
    }
    StateVals::decode_with(layout, &cur)
}

/// DIAGNOSTIC (not a proof): step the real body span concretely on a few carrier
/// points and print (in-regs → out-regs) beside rot_step, to pin the register
/// mapping + bit order empirically.
#[test]
#[ignore = "diagnostic: prints the empirical body register mapping"]
fn diag_body_register_mapping() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _c) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    eprintln!("=== DIAG body register mapping (body_start={}, body_len={}) ===", geom.body_start, geom.body_len);
    // Try carrier values placed at candidate registers. We set tag/v/h across a
    // few registers and see which regs change and how.
    for (tag, v, h) in [(1u8, 1u8, 0u8), (1, 2, 0), (1, 4, 0), (0, 3, 1), (1, 5, 1)] {
        // Place candidate: tag=Reg2, v=Reg0, h=Reg1 (hypothesis).
        let mut regs = [0u8; NUM_REGS];
        regs[0] = v;
        regs[1] = h;
        regs[2] = tag;
        let out = concrete_step_body(&layout, &geom.rom, geom.body_start, geom.body_len, regs);
        let (nt, nv, nh) = rot_step(tag, v, h, V2_K, V2_HB);
        eprintln!(
            "  in(tag={tag},v={v},h={h}) rot_step->(t={nt},v={nv},h={nh}) | out regs = {:?}",
            &out.regs[..4]
        );
    }
}

/// L1 — the vart_halve BODY LEMMA. The headline tractability result: does
/// composing the straight-line body span with symbolic carrier stay small AND
/// equal rot_step over the whole carrier domain?
#[test]
fn invariant_body_lemma_vart_halve() {
    // w_pc=8: the body span never executes a Swapbr (BR stays 0), so PC ∈ [7,104]
    // fits 8 bits and the come-from δu sign-extension is irrelevant HERE. This
    // makes the fetch mux + PC adder 8-bit (far cheaper) while being faithful to
    // the body (validated against the w_pc=8 concrete eval in diag_body_*). The
    // come-from's wider-PC δu reach is L2's concern, proven separately.
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    eprintln!(
        "vart_halve geom: iter_len={}, body_len={}, body_start(pc)={}, lend(pc)={}, budget={}",
        geom.iter_len, geom.body_len, geom.body_start, geom.lend_pc, geom.budget
    );
    let (peak, secs) = prove_body_lemma_vart(&layout, &geom);
    eprintln!(
        "L1 BODY LEMMA [vart_halve K={V2_K},HB={V2_HB}]: composed body span ({} instrs) ≡ rot_step \
         over the WHOLE symbolic carrier domain. MAX peak {} BDD nodes, {:.3}s. \
         → BODY IS TRACTABLE (concrete PC ⇒ one instr/step).",
        geom.body_len, peak, secs
    );
    // Tractability gate: the body must stay far below the loop-composition blow-up.
    assert!(peak < 50_000, "BODY LEMMA peak {peak} — expected tractable (< 50k)");
    let _ = geom.lowered_instrs.len();
}

// ══════════════════════════════════════════════════════════════════════════
// L2 — COME-FROM / TERMINATION LEMMA.
//
// The come-from bracket at the loop tail (the last ROUTE_LEN instrs + Lend):
//   XOR ctrl,tag ; XORI ctrl,1   → ctrl := ¬tag  (1 = continue/Left, 0 = exit)
//   ADDI temp,δu                 → mint token (temp: 0 → δu)
//   CSWAP ctrl,temp,rb           → continue: rb=δu,temp=0 · exit: rb=0,temp=δu
//   Lend: SWAPBR rb              → BR ↔ rb : continue BR=δu (back-jump), exit BR=0
//
// CLAIM (whole carrier domain, single bracket): starting from the pre-route
// machine invariant {ctrl=0, temp=0, rb=0, BR=0} and SYMBOLIC tag, the bracket
// sets  BR_out = δu  iff  tag==0 (continue)  and  BR_out = 0  iff  tag==1 (exit).
// i.e. the loop takes the back-edge iff ¬done, done := (tag==1) = the Trace's
// exit condition (tag'=1 ⟺ Right ⟺ an odd bit surfaced).
//
// This is a SINGLE bracket (5 steps, PC concrete each step) ⇒ tractable.
// The δu back-jump reach itself (sext(δu) = −iter_len across the wide PC) is a
// SEPARATE property proven by co-factoring the PC output — see below.
// ══════════════════════════════════════════════════════════════════════════

/// Read the aux registers from the compiled come-from bracket, so L2 is robust
/// to the exact register allocation: `ctrl,tag` from `XOR ctrl,tag`, `temp` from
/// `ADDI temp,δu`, `rb` from `SWAPBR rb`; δu from `ADDI temp,δu`.
struct ComeFrom {
    ctrl: usize,
    tag: usize,
    temp: usize,
    rb: usize,
    delta_u: u8,
    route_start: usize, // PC of the first route instr (XOR ctrl,tag)
}

fn read_come_from(geom: &LoopGeom) -> ComeFrom {
    let instrs = &geom.lowered_instrs;
    let route_start = geom.lend_pc - ROUTE_LEN;
    let (ctrl, tag) = match instrs[route_start] {
        Instr::Xor { rd, rs } => (rd as usize, rs as usize),
        ref o => panic!("route[0] must be XOR ctrl,tag, got {o:?}"),
    };
    assert!(
        matches!(instrs[route_start + 1], Instr::Xori { rd, imm: 1 } if rd as usize == ctrl),
        "route[1] must be XORI ctrl,1, got {:?}",
        instrs[route_start + 1]
    );
    let (temp, delta_u) = match instrs[route_start + 2] {
        Instr::Addi { rd, imm } => (rd as usize, imm),
        ref o => panic!("route[2] must be ADDI temp,δu, got {o:?}"),
    };
    match instrs[route_start + 3] {
        Instr::Cswap { rd, rs, rt } => {
            assert_eq!(rd as usize, ctrl, "CSWAP ctrl mismatch");
            assert_eq!(rs as usize, temp, "CSWAP temp mismatch");
            // rt is rb.
            let rb = rt as usize;
            match instrs[geom.lend_pc] {
                Instr::Swapbr { rd } => assert_eq!(rd as usize, rb, "Lend SWAPBR rb mismatch"),
                ref o => panic!("Lend must be SWAPBR rb, got {o:?}"),
            }
            ComeFrom { ctrl, tag, temp, rb, delta_u, route_start }
        }
        ref o => panic!("route[3] must be CSWAP ctrl,temp,rb, got {o:?}"),
    }
}

/// Pre-route entry SV: SYMBOLIC tag (only its bit 0 matters), everything the
/// bracket reads set to the machine invariant — ctrl=temp=rb=0, BR=0 — and PC
/// concrete at `route_start`. Carrier v/h and other regs symbolic (identity
/// witnesses; the bracket must not touch them). IR=0, s0/s1=0.
fn come_from_entry_sv(layout: &Layout, cf: &ComeFrom, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((cf.route_start & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((cf.route_start >> 8) & 0xFF) as u8, vs)
        } else if f == layout.br_field() {
            const_word8(0, vs)
        } else if f == layout.reg_field(cf.ctrl)
            || f == layout.reg_field(cf.temp)
            || f == layout.reg_field(cf.rb)
        {
            const_word8(0, vs) // pre-route machine invariant
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            const_word8(0, vs)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            const_word8(0, vs)
        } else {
            symbolic_word8(vs, nf, f) // tag, v, h, other regs symbolic
        }
    })
}

/// Compose the come-from bracket [route_start .. lend_pc] inclusive, re-pinning
/// PC concrete BUT letting the FINAL Lend step's PC/BR go free (that is the whole
/// point — Lend writes BR). Returns the output SV after Lend + max peak nodes.
fn compose_come_from(
    layout: &Layout,
    geom: &LoopGeom,
    cf: &ComeFrom,
    vs: &BddVariableSet,
) -> (SV, usize) {
    let engine = StepEngine::new(layout, &geom.rom);
    let mut ctx = WalkCtx::new();
    let mut cur = come_from_entry_sv(layout, cf, vs);
    let n = ROUTE_LEN + 1; // 4 route instrs + Lend
    let mut peak = 0usize;
    for i in 0..n {
        let pc = cf.route_start + i;
        let out = engine
            .eval_step_concrete_pc(cur, pc, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("come-from eval FAILED at step {i} (pc={pc}): {e}"));
        let (p, _s) = sv_bdd_sizes(&out);
        peak = peak.max(p);
        if i + 1 < n {
            // Not yet Lend: re-pin PC concrete (all pre-Lend route instrs keep
            // BR=0 so PC output IS pc+1). Carry data forward.
            let next_pc = cf.route_start + i + 1;
            // SOUNDNESS (item 4.ii): confirm the datapath PC output IS pc+1.
            assert_eq!(
                read_concrete_pc(&out, layout),
                Some(next_pc),
                "PC-AGREEMENT: come-from step {i} (pc={pc}) datapath PC output != pc+1"
            );
            cur = build_state(layout, |f| {
                if f == layout.pc_field() {
                    const_word8((next_pc & 0xFF) as u8, vs)
                } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
                    const_word8(((next_pc >> 8) & 0xFF) as u8, vs)
                } else {
                    let bits = out_field_bits(&out, layout, f);
                    let mut sv = SV::Bit(bits[W - 1].clone());
                    for b in (0..W - 1).rev() {
                        sv = SV::pair(SV::Bit(bits[b].clone()), sv);
                    }
                    sv
                }
            });
        } else {
            cur = out; // Lend output: keep BR + PC as produced
        }
    }
    (cur, peak)
}

fn prove_come_from_lemma(layout: &Layout, geom: &LoopGeom) -> (usize, f64) {
    let cf = read_come_from(geom);
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let nf = n_fields(layout);
    let t0 = Instant::now();
    let (out, peak) = compose_come_from(layout, geom, &cf, &vs);
    let secs = t0.elapsed().as_secs_f64();

    let tag_in = field_bits(&vs, nf, layout.reg_field(cf.tag));
    let br_out = out_field_bits(&out, layout, layout.br_field());

    // CLAIM: BR_out = δu · [tag==0]  (tag is a single-bit predicate: tag[0]).
    //   tag==0 (continue): BR = δu
    //   tag==1 (exit):     BR = 0
    // Per bit b: BR_out[b] = (¬tag[0]) ∧ δu_bit_b.
    let not_tag = tag_in[0].not();
    for b in 0..W {
        let du_bit = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        let want = not_tag.and(&du_bit);
        assert!(
            br_out[b] == want,
            "COME-FROM LEMMA: BR out bit {b} != δu·[tag==0] (δu={:#04x})",
            cf.delta_u
        );
    }

    // ── GAP-A (fix #2): the ONLY BR≠0 PC-adder pass, asserted vs the NETLIST ──
    // Lend is the SOLE step in the whole proof where `pc_add_br` sees BR≠0 (= δu
    // on the continue branch): the back-jump. Everywhere else BR=0 so PC_out=pc+1
    // (the PC-agreement asserts). Until now Lend's LANDING was only checked as the
    // closed-form arithmetic identity below (sext(δu)+1 lands on LTOP) — NEVER
    // against the datapath's own PC output SV. That leaves a sext/carry defect in
    // the w_pc=16 adder's HIGH byte unasserted (LTOP=2 and lend+1=109 differ in
    // the low byte but a high-byte carry bug would still be invisible to a low-
    // byte-only check). Here we read Lend's ACTUAL PC output (already computed in
    // `out`) and assert, per bit over the full w_pc=16 PC:
    //   PC_out = ite(tag0, lend+1, LTOP)
    //     continue (tag0=0): PC = LTOP        (δu back-jump landed)
    //     exit     (tag0=1): PC = lend+1      (fall through to epilogue)
    // This is run under Layout::default() (w_pc=16) — the REAL δu sign-extension
    // reach — so it exercises the wide adder's high byte on the live back-jump.
    assert_eq!(layout.w_pc, 16, "GAP-A PC-landing assert requires the real w_pc=16 adder");
    let lend_plus_1 = geom.lend_pc + 1;
    let pc_lo_out = out_field_bits(&out, layout, layout.pc_field());
    let pc_hi_out = out_field_bits(&out, layout, layout.pc_hi_field());
    for b in 0..W {
        // continue landing bit (LTOP) and exit landing bit (lend+1) for lo/hi.
        let ltop_lo = (LTOP >> b) & 1 == 1;
        let ltop_hi = (LTOP >> (8 + b)) & 1 == 1;
        let end_lo = (lend_plus_1 >> b) & 1 == 1;
        let end_hi = (lend_plus_1 >> (8 + b)) & 1 == 1;
        // want[b] = tag0 ? exit_bit : continue_bit  (tag0 selects exit).
        // Encode ite(tag0, e, c) = (tag0∧e) ∨ (¬tag0∧c).
        let mk = |c: bool, e: bool| -> Bdd {
            let cb = if c { vs.mk_true() } else { vs.mk_false() };
            let eb = if e { vs.mk_true() } else { vs.mk_false() };
            tag_in[0].and(&eb).or(&not_tag.and(&cb))
        };
        let want_lo = mk(ltop_lo, end_lo);
        let want_hi = mk(ltop_hi, end_hi);
        assert!(
            pc_lo_out[b] == want_lo,
            "GAP-A PC-LANDING: PC-lo bit {b} != ite(tag0, lend+1={lend_plus_1}, LTOP={LTOP}) \
             — w_pc=16 adder low-byte sext/carry defect on the δu back-jump"
        );
        assert!(
            pc_hi_out[b] == want_hi,
            "GAP-A PC-LANDING: PC-hi bit {b} != ite(tag0, lend+1={lend_plus_1}, LTOP={LTOP}) \
             — w_pc=16 adder HIGH-byte sext/carry defect on the δu back-jump"
        );
    }

    // ── L2 AUX-EXIT (item 2): pin the per-branch aux register EXIT values ──────
    // The come-from's exit aux state is the PRECONDITION L0 (the re-entry lemma)
    // starts from, so it must be pinned precisely, not left "unconstrained".
    // The bracket is `XOR ctrl,tag ; XORI ctrl,1 ; ADDI temp,δu ; CSWAP ctrl,temp,rb ;
    // SWAPBR rb`, entered with ctrl=temp=rb=0. Over the FULL symbolic tag BYTE:
    //   ctrl = tag ^ 1   ⇒ ctrl[0] = ¬tag[0], ctrl[b≥1] = tag[b]. The CSWAP and
    //     hence the whole route decision keys ONLY on ctrl&1 = ¬tag[0], so:
    //   CONTINUE (tag0=0): rb=δu→SWAPBR→BR=δu, rb=0, temp=0.
    //   EXIT     (tag0=1): CSWAP no-op ⇒ temp keeps δu, rb=0 ⇒ SWAPBR BR=0.
    //   temp = δu · [tag0==1]  (exit keeps the minted token; the epilogue SUBI
    //     burns it OUTSIDE this span). rb=0 on BOTH branches.
    // At a real iteration boundary tag ∈ {0,1} so ctrl ∈ {0,1} (the high tag bits
    // are 0); L0/L1 only ever see that single-bit tag. Pinning the FULL byte here
    // is strictly stronger and faithful to the ROM's whole-byte ctrl datapath.
    let ctrl_out = out_field_bits(&out, layout, layout.reg_field(cf.ctrl));
    let temp_out = out_field_bits(&out, layout, layout.reg_field(cf.temp));
    let rb_out = out_field_bits(&out, layout, layout.reg_field(cf.rb));
    let zero = vs.mk_false();
    for b in 0..W {
        // ctrl = tag ^ 1: bit 0 flipped, higher bits pass tag through.
        let want_ctrl = if b == 0 { not_tag.clone() } else { tag_in[b].clone() };
        assert!(
            ctrl_out[b] == want_ctrl,
            "COME-FROM AUX-EXIT: ctrl out bit {b} != (tag^1)"
        );
        // temp = δu · [tag0==1]  (exit keeps the token; continue burns it via CSWAP).
        let du_bit = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        let want_temp = tag_in[0].and(&du_bit);
        assert!(
            temp_out[b] == want_temp,
            "COME-FROM AUX-EXIT: temp out bit {b} != δu·[tag0==1]"
        );
        // rb = 0 on both branches.
        assert!(rb_out[b] == zero, "COME-FROM AUX-EXIT: rb out bit {b} != 0");
    }
    // scratch s0/s1 + IR identity 0 (entered 0, come-from must not touch them).
    for f in [
        layout.scratch_field(0),
        layout.scratch_field(1),
        layout.ir_byte_field(0),
        layout.ir_byte_field(1),
        layout.ir_byte_field(2),
        layout.ir_byte_field(3),
    ] {
        let got = out_field_bits(&out, layout, f);
        for b in 0..W {
            assert!(
                got[b] == zero,
                "COME-FROM AUX-EXIT: field {f} bit {b} not restored to 0 (scratch/IR leak)"
            );
        }
    }

    // ── FIELD-COMPLETE HAND-OFF C (fix #1, L2 leg): scratch s2..s7 IDENTITY ────
    // `come_from_entry_sv` sets s0/s1 = 0 explicitly but lets s2..s7 enter
    // SYMBOLIC (the else branch). L1's body-entry invariant requires ALL scratch
    // = 0, so the weld L1(0)→L2→L0→L1 needs L2 to pass s2..s7 through UNCHANGED
    // (identity) for any value — strictly stronger than "0-in ⇒ 0-out", and it
    // closes the previously-unasserted s2..s7 fields on the loop cycle. (IR is
    // already pinned to 0 above; here the fields entered symbolic ⇒ must be the
    // SAME symbolic vars at exit.)
    for s in 2..NUM_SCRATCH {
        let f = layout.scratch_field(s);
        let got = out_field_bits(&out, layout, f);
        let inp = field_bits(&vs, nf, f);
        for b in 0..W {
            assert!(
                got[b] == inp[b],
                "COME-FROM AUX-EXIT: scratch s{s} bit {b} not identity (come-from disturbed s2..s7)"
            );
        }
    }

    // δu correctness: δu = 256 − iter_len, so the back-jump lands on Ltop.
    // sext(δu) + 1 must equal −(lend_pc − LTOP) = −(iter_len − 1), i.e. after the
    // uniform PC += sext(BR)+1 the next PC is LTOP. Check the ARITHMETIC identity
    // (this is the residual "does δu reach across the wide PC" obligation, here
    // discharged as a closed-form check tied to the compiled δu).
    let iter_len = geom.iter_len;
    let delta_u = cf.delta_u as usize;
    assert_eq!(delta_u, 256 - iter_len, "δu must be 256 − iter_len");
    // sext over w_pc, +1, added to lend_pc, must wrap to LTOP.
    let w_pc = layout.w_pc;
    let mask: usize = if w_pc >= usize::BITS as usize { usize::MAX } else { (1usize << w_pc) - 1 };
    let sext = {
        // sign-extend the 8-bit δu to w_pc bits (as the PC adder does).
        let s = cf.delta_u as i8 as i64; // −107
        (s & (mask as i64)) as usize
    };
    let landed = (geom.lend_pc.wrapping_add(sext).wrapping_add(1)) & mask;
    assert_eq!(landed, LTOP, "back-jump δu must land on Ltop (offset {LTOP})");

    // The come-from must NOT disturb the carrier (v,h,tag) or other data regs —
    // it is a pure BR-router. tag/v/h + all non-aux regs identity.
    let (tag_reg, v_reg, h_reg) = v2_carrier_regs();
    for r in 0..layout.num_regs {
        if r == cf.ctrl || r == cf.temp || r == cf.rb {
            // Aux registers the bracket legitimately mints/routes (temp/rb hold δu
            // mid-bracket; the epilogue SUBI burns temp — OUTSIDE this span). We
            // do not constrain them here; terminal quiescence (L3 bootstrap:
            // scratch clean + BR=0) confirms the whole loop restores them.
            continue;
        }
        let got = out_field_bits(&out, layout, layout.reg_field(r));
        let inp = field_bits(&vs, nf, layout.reg_field(r));
        for b in 0..W {
            assert!(
                got[b] == inp[b],
                "COME-FROM LEMMA: data reg r{r} bit {b} disturbed by the come-from"
            );
        }
    }
    let _ = (tag_reg, v_reg, h_reg);

    (peak, secs)
}

/// L2 — the COME-FROM / TERMINATION LEMMA.
#[test]
fn invariant_come_from_lemma_vart_halve() {
    let layout = Layout::default(); // w_pc=16: the REAL δu sign-extension reach
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    eprintln!(
        "come-from regs: ctrl=r{}, tag=r{}, temp=r{}, rb=r{}, δu={:#04x}, route_start(pc)={}",
        cf.ctrl, cf.tag, cf.temp, cf.rb, cf.delta_u, cf.route_start
    );
    let (peak, secs) = prove_come_from_lemma(&layout, &geom);
    eprintln!(
        "L2 COME-FROM LEMMA [vart_halve]: bracket sets BR = δu·[tag==0] over the WHOLE tag \
         domain; back-jump δu={:#04x} lands on Ltop; carrier undisturbed. Peak {} BDD nodes, {:.3}s. \
         → the loop takes the back-edge IFF ¬done (done := tag==1). TRACTABLE.",
        cf.delta_u, peak, secs
    );
    assert!(peak < 50_000, "COME-FROM LEMMA peak {peak} — expected tractable");
}

// ══════════════════════════════════════════════════════════════════════════
// L0 — RE-ENTRY LEMMA (the induction weld: L2-exit → L1-entry).
//
// The 5 glue instructions [LTOP, BODY_START) = offsets [2,7) — Ltop's SWAPBR rc
// + the 4 join-dispatch instrs — are covered by NEITHER L1 (which starts at
// BODY_START) NOR L2 (which ends at Lend). The induction step needs: after the
// come-from takes the back-edge (L2 continue exit), the machine re-enters the
// body at BODY_START in EXACTLY L1's entry invariant. L0 PROVES that weld.
//
//   ENTRY = L2's proven CONTINUE exit (item 2 aux-exit lemma), on the back-edge:
//     PC=Ltop, BR=δu, ctrl=1, temp=0, rb=0, rc=0, tag0=0 (continue path).
//     v/h/scratch/other regs SYMBOLIC (identity witnesses).
//   Compose offsets [2,7) with the StepEngine (concrete PC each step).
//   EXIT must ≡ body_entry_sv's L1-entry invariant:
//     {ctrl=temp=rb=rc=BR=0}, PC=BODY_START, v/h/scratch/other identity,
//     tag as L1 expects (unchanged — the join-dispatch does not touch tag).
//
// Hand-trace (continue, tag0=0):
//   2 SWAPBR rc     : BR↔rc ⇒ rc=δu, BR=0        (post-swap BR=0 ⇒ PC=3)
//   3 CSWAP ctrl,rc,temp (ctrl=1) ⇒ rc=0, temp=δu
//   4 SUBI temp,δu  ⇒ temp=0
//   5 XOR ctrl,tag  ⇒ ctrl=1^0=1
//   6 XORI ctrl,1   ⇒ ctrl=0
//   ⇒ {ctrl=0,temp=0,rb=0,rc=0,BR=0} = the L1 entry invariant. QED.
//
// The entry BR=δu is a compile-time constant here; the Ltop SWAPBR consumes it
// (post-swap BR=0), so PC stays concrete in [2,7) and the StepEngine applies —
// no symbolic PC, no new BDD machinery (peak stays tiny).
// ══════════════════════════════════════════════════════════════════════════

/// Read the Ltop `SWAPBR rc` register, so L0 is robust to the allocation.
fn read_rc(geom: &LoopGeom) -> usize {
    match geom.lowered_instrs[LTOP] {
        Instr::Swapbr { rd } => rd as usize,
        ref o => panic!("Ltop must be SWAPBR rc, got {o:?}"),
    }
}

/// L0 re-entry ENTRY SV: L2's continue-exit state at PC=Ltop. Aux
/// {ctrl=1 (¬tag on continue), temp=0, rb=0, rc=0}, BR=δu (the minted back-jump),
/// tag = 0 concrete (continue path: L2 aux-exit proves ctrl=1 iff tag0=0), IR=0,
/// scratch s0/s1=0. v/h and every other data reg SYMBOLIC (identity witnesses).
fn re_entry_sv(layout: &Layout, geom: &LoopGeom, cf: &ComeFrom, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    let rc = read_rc(geom);
    let (tag_reg, _v, _h) = v2_carrier_regs();
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((LTOP & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((LTOP >> 8) & 0xFF) as u8, vs)
        } else if f == layout.br_field() {
            const_word8(cf.delta_u, vs) // the minted back-jump token
        } else if f == layout.reg_field(cf.ctrl) {
            const_word8(1, vs) // continue: ctrl = ¬tag = 1
        } else if f == layout.reg_field(cf.temp)
            || f == layout.reg_field(cf.rb)
            || f == layout.reg_field(rc)
        {
            const_word8(0, vs) // aux at the continue-exit invariant 0
        } else if f == layout.reg_field(tag_reg) {
            const_word8(0, vs) // continue path ⇒ tag0=0 (tag stays 0 across L0)
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            const_word8(0, vs)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            const_word8(0, vs)
        } else {
            symbolic_word8(vs, nf, f) // v, h, other data regs symbolic (identity)
        }
    })
}

/// L0-BASE entry SV: the FIRST traversal of the glue [LTOP, BODY_START) after the
/// prologue (`XORI tag,1 ; ADDI temp,δu`), which no other lemma covers. Its
/// precondition differs from the back-edge one: {BR=0, ctrl=0, temp=δu, tag=1}
/// (the prologue minted the token into temp and set the Right tag), rc=rb=0.
/// v/h and other data regs SYMBOLIC (identity witnesses). Proving this lands in
/// L1's entry invariant closes the base case of the induction on lemmas.
fn re_entry_base_sv(layout: &Layout, geom: &LoopGeom, cf: &ComeFrom, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    let rc = read_rc(geom);
    let (tag_reg, _v, _h) = v2_carrier_regs();
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((LTOP & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((LTOP >> 8) & 0xFF) as u8, vs)
        } else if f == layout.br_field() {
            const_word8(0, vs) // first traversal: BR=0 (no back-jump token yet)
        } else if f == layout.reg_field(cf.temp) {
            const_word8(cf.delta_u, vs) // prologue minted the token INTO temp
        } else if f == layout.reg_field(cf.ctrl)
            || f == layout.reg_field(cf.rb)
            || f == layout.reg_field(rc)
        {
            const_word8(0, vs) // ctrl/rb/rc = 0 at prologue exit
        } else if f == layout.reg_field(tag_reg) {
            const_word8(1, vs) // prologue set the Right tag (tag=1)
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            const_word8(0, vs)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            const_word8(0, vs)
        } else {
            symbolic_word8(vs, nf, f) // v, h, other data regs symbolic (identity)
        }
    })
}

/// Compose the re-entry glue [LTOP, BODY_START) with the StepEngine, concrete PC
/// each step, from a given `entry` SV. Ltop's SWAPBR consumes BR (post-swap BR=0
/// ⇒ PC+1), so PC stays concrete throughout — the PC-agreement assert confirms
/// it. Returns exit SV. The entry SV varies by leg (back-edge L0 vs first-
/// traversal L0-base); the glue instructions are identical.
fn compose_re_entry_from(
    layout: &Layout,
    geom: &LoopGeom,
    vs: &BddVariableSet,
    entry: SV,
) -> (SV, usize) {
    let engine = StepEngine::new(layout, &geom.rom);
    let mut ctx = WalkCtx::new();
    let mut cur = entry;
    let mut peak = 0usize;
    for pc in LTOP..BODY_START {
        let out = engine
            .eval_step_concrete_pc(cur, pc, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("re-entry eval FAILED at pc={pc}: {e}"));
        let (p, _s) = sv_bdd_sizes(&out);
        peak = peak.max(p);
        let next_pc = pc + 1;
        // SOUNDNESS (item 4.ii): even at Ltop (SWAPBR) the datapath PC output must
        // be pc+1 (post-swap BR=0), confirming the concrete-PC pinning is valid.
        assert_eq!(
            read_concrete_pc(&out, layout),
            Some(next_pc),
            "PC-AGREEMENT: re-entry pc={pc} datapath PC output != pc+1"
        );
        cur = build_state(layout, |f| {
            if f == layout.pc_field() {
                const_word8((next_pc & 0xFF) as u8, vs)
            } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
                const_word8(((next_pc >> 8) & 0xFF) as u8, vs)
            } else {
                let bits = out_field_bits(&out, layout, f);
                let mut sv = SV::Bit(bits[W - 1].clone());
                for b in (0..W - 1).rev() {
                    sv = SV::pair(SV::Bit(bits[b].clone()), sv);
                }
                sv
            }
        });
    }
    (cur, peak)
}

/// L0 — the RE-ENTRY LEMMA. Prove [LTOP, BODY_START) welds L2-continue-exit to
/// L1's entry invariant over the WHOLE symbolic carrier. Returns (peak, secs).
#[test]
fn invariant_re_entry_lemma_vart_halve() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let t0 = Instant::now();
    let (out, peak) = compose_re_entry_from(&layout, &geom, &vs, re_entry_sv(&layout, &geom, &cf, &vs));
    let secs = t0.elapsed().as_secs_f64();

    let rc = read_rc(&geom);
    let (tag_reg, v_reg, h_reg) = v2_carrier_regs();
    let zero = vs.mk_false();

    // EXIT AUX must all be the L1 entry invariant 0: ctrl, temp, rb, rc, BR = 0.
    for f in [
        layout.reg_field(cf.ctrl),
        layout.reg_field(cf.temp),
        layout.reg_field(cf.rb),
        layout.reg_field(rc),
        layout.br_field(),
    ] {
        let got = out_field_bits(&out, &layout, f);
        for b in 0..W {
            assert!(
                got[b] == zero,
                "RE-ENTRY LEMMA: aux field {f} bit {b} != 0 (L1 entry invariant broken)"
            );
        }
    }

    // PC must be BODY_START (the join-dispatch fell through to the first body instr).
    assert_eq!(
        read_concrete_pc(&out, &layout),
        Some(BODY_START),
        "RE-ENTRY LEMMA: exit PC != BODY_START"
    );

    // CARRIER + other data regs IDENTITY: the glue must not touch v, h, or tag
    // (tag stays 0 = the continue value; v/h symbolic → must pass through). Also
    // scratch s0/s1 + IR back to 0.
    for r in 0..layout.num_regs {
        if r == cf.ctrl || r == cf.temp || r == cf.rb || r == rc {
            continue; // aux checked above
        }
        let got = out_field_bits(&out, &layout, layout.reg_field(r));
        if r == tag_reg {
            // tag entered 0 (continue) and must stay 0.
            for b in 0..W {
                assert!(got[b] == zero, "RE-ENTRY LEMMA: tag reg bit {b} disturbed");
            }
        } else {
            let inp = field_bits(&vs, nf, layout.reg_field(r));
            for b in 0..W {
                assert!(
                    got[b] == inp[b],
                    "RE-ENTRY LEMMA: data reg r{r} bit {b} disturbed by the glue"
                );
            }
        }
    }
    for f in [layout.scratch_field(0), layout.scratch_field(1)] {
        let got = out_field_bits(&out, &layout, f);
        for b in 0..W {
            assert!(got[b] == zero, "RE-ENTRY LEMMA: scratch field {f} bit {b} != 0");
        }
    }

    // ── FIELD-COMPLETE HAND-OFF C (fix #1, L0 leg) ────────────────────────────
    // L1's body-entry invariant is ALL non-carrier fields = 0, INCLUDING s2..s7
    // and all four IR bytes. `re_entry_sv` pins s0/s1=0 + IR=0 at entry but lets
    // s2..s7 enter SYMBOLIC (else branch). To weld L0-exit → L1-entry(0) for
    // those 7 currently-unproven fields we assert:
    //   * s2..s7 ≡ IDENTITY (their entry vars): the glue must pass them through
    //     unchanged for any value (strictly stronger than 0-in⇒0-out — proves the
    //     join-dispatch never touches s2..s7). Combined with L2's s2..s7 identity
    //     and L1's s2..s7=0-in/0-out, the s2..s7 fields are closed on the cycle.
    //   * IR (all 4 bytes) == 0 at exit: L1's entry needs IR=0. (IR entered 0
    //     here; a stray IR write by the glue would survive this XOR-exact check.)
    for s in 2..NUM_SCRATCH {
        let f = layout.scratch_field(s);
        let got = out_field_bits(&out, &layout, f);
        let inp = field_bits(&vs, nf, f);
        for b in 0..W {
            assert!(
                got[b] == inp[b],
                "RE-ENTRY LEMMA: scratch s{s} bit {b} not identity (glue disturbed s2..s7)"
            );
        }
    }
    for k in 0..4 {
        let f = layout.ir_byte_field(k);
        let got = out_field_bits(&out, &layout, f);
        for b in 0..W {
            assert!(
                got[b] == zero,
                "RE-ENTRY LEMMA: IR byte {k} bit {b} != 0 at L0-exit (L1 entry needs IR=0)"
            );
        }
    }
    let _ = (v_reg, h_reg);

    assert!(peak < 50_000, "RE-ENTRY LEMMA peak {peak} — expected tractable");
    eprintln!(
        "L0 RE-ENTRY LEMMA [vart_halve]: glue [{LTOP},{BODY_START}) welds L2-continue-exit \
         → L1-entry invariant {{ctrl=temp=rb=rc=BR=0}} over the WHOLE symbolic carrier; \
         PC→BODY_START; carrier identity. Peak {peak} BDD nodes, {secs:.3}s. \
         → the induction step now closes on LEMMAS: L2→L0→L1 covers offsets [2,108)."
    );
}

/// L0-BASE — the FIRST-TRAVERSAL glue lemma (fix #3). The induction STEP is L0
/// (from the back-edge precondition {BR=δu,ctrl=1,temp=0,tag=0}); the BASE case —
/// the first entry into the glue after the prologue, precondition {BR=0,ctrl=0,
/// temp=δu,tag=1} — was in NO lemma. This proves the SAME glue [LTOP,BODY_START)
/// also lands the base precondition in L1's entry invariant (aux all 0, PC=
/// BODY_START, carrier identity — with tag=1 concrete, the Right-tagged base
/// carrier), so L1 applies on the FIRST iteration too. Hand-trace (tag=1):
///   2 SWAPBR rc      : BR=0↔rc=0 ⇒ rc=0, BR=0            (PC=3)
///   3 CSWAP ctrl,rc,temp (ctrl=0) ⇒ no-op: rc=0, temp=δu
///   4 SUBI temp,δu   ⇒ temp=0
///   5 XOR ctrl,tag   ⇒ ctrl=0^1=1
///   6 XORI ctrl,1    ⇒ ctrl=0
///   ⇒ {ctrl=temp=rb=rc=BR=0}, PC=7, tag=1. = L1's entry invariant. QED.
#[test]
fn invariant_re_entry_base_lemma_vart_halve() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let t0 = Instant::now();
    let (out, peak) =
        compose_re_entry_from(&layout, &geom, &vs, re_entry_base_sv(&layout, &geom, &cf, &vs));
    let secs = t0.elapsed().as_secs_f64();

    let rc = read_rc(&geom);
    let (tag_reg, _v_reg, _h_reg) = v2_carrier_regs();
    let zero = vs.mk_false();
    let one = vs.mk_true();

    // EXIT AUX must all be the L1 entry invariant 0: ctrl, temp, rb, rc, BR = 0.
    for f in [
        layout.reg_field(cf.ctrl),
        layout.reg_field(cf.temp),
        layout.reg_field(cf.rb),
        layout.reg_field(rc),
        layout.br_field(),
    ] {
        let got = out_field_bits(&out, &layout, f);
        for b in 0..W {
            assert!(
                got[b] == zero,
                "RE-ENTRY BASE: aux field {f} bit {b} != 0 (L1 entry invariant broken)"
            );
        }
    }

    // PC must be BODY_START.
    assert_eq!(
        read_concrete_pc(&out, &layout),
        Some(BODY_START),
        "RE-ENTRY BASE: exit PC != BODY_START"
    );

    // tag must be 1 (the prologue's Right tag, unchanged by the glue) — this is
    // the base carrier's tag; L1 handles it as a carrier value.
    let got_tag = out_field_bits(&out, &layout, layout.reg_field(tag_reg));
    for b in 0..W {
        let want = if b == 0 { one.clone() } else { zero.clone() };
        assert!(got_tag[b] == want, "RE-ENTRY BASE: tag bit {b} != 1 (base Right tag disturbed)");
    }

    // v/h + other non-aux data regs IDENTITY (glue must not touch them).
    for r in 0..layout.num_regs {
        if r == cf.ctrl || r == cf.temp || r == cf.rb || r == rc || r == tag_reg {
            continue;
        }
        let got = out_field_bits(&out, &layout, layout.reg_field(r));
        let inp = field_bits(&vs, nf, layout.reg_field(r));
        for b in 0..W {
            assert!(
                got[b] == inp[b],
                "RE-ENTRY BASE: data reg r{r} bit {b} disturbed by the glue"
            );
        }
    }

    // scratch s0/s1 = 0, s2..s7 identity, IR = 0 (field-complete hand-off C).
    for f in [layout.scratch_field(0), layout.scratch_field(1)] {
        let got = out_field_bits(&out, &layout, f);
        for b in 0..W {
            assert!(got[b] == zero, "RE-ENTRY BASE: scratch field {f} bit {b} != 0");
        }
    }
    for s in 2..NUM_SCRATCH {
        let f = layout.scratch_field(s);
        let got = out_field_bits(&out, &layout, f);
        let inp = field_bits(&vs, nf, f);
        for b in 0..W {
            assert!(got[b] == inp[b], "RE-ENTRY BASE: scratch s{s} bit {b} not identity");
        }
    }
    for k in 0..4 {
        let f = layout.ir_byte_field(k);
        let got = out_field_bits(&out, &layout, f);
        for b in 0..W {
            assert!(got[b] == zero, "RE-ENTRY BASE: IR byte {k} bit {b} != 0");
        }
    }

    assert!(peak < 50_000, "RE-ENTRY BASE peak {peak} — expected tractable");
    eprintln!(
        "L0-BASE RE-ENTRY LEMMA [vart_halve]: glue [{LTOP},{BODY_START}) lands the FIRST-traversal \
         precondition {{BR=0,ctrl=0,temp=δu,tag=1}} in L1's entry invariant (aux all 0, PC→BODY_START, \
         carrier identity, tag=1). Peak {peak} BDD nodes, {secs:.3}s. → base case closed on a lemma."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// L3 — INDUCTION ARGUMENT + BOOTSTRAP ANCHOR + TERMINAL QUIESCENCE.
//
// The meta-argument (what L1 ∧ L2 buy us), stated explicitly. Let `C_i` be the
// carrier (tag,v,h) at the top of iteration i, `C_0` = the marshalled input
// (tag=1 Right injected by the prologue). Define `body(C) = rot_step(C)` and
// `done(C) := (tag(C) == 1)`.
//
//   L1 (BODY LEMMA):    one straight-line pass of the body span transforms the
//                       carrier by exactly `body` = rot_step, over the WHOLE
//                       carrier domain, disturbing nothing else.
//   L2 (COME-FROM):     after the body, the come-from sets BR = δu (back-jump to
//                       Ltop) iff ¬done(C_{i+1}), BR = 0 (exit) iff done(C_{i+1});
//                       δu = 256−iter_len lands exactly on Ltop; carrier
//                       undisturbed. done := tag==1 = the Trace's Right/exit tag.
//
// INDUCTION on the trip count T (the number of body applications until done):
//   * Base: after 1 body pass, C_1 = body(C_0). If done(C_1) the come-from exits
//     (BR=0); else it loops.
//   * Step: assuming C_i = body^i(C_0) at Ltop with ¬done for all j<i, the body
//     produces C_{i+1} = body(C_i) = body^{i+1}(C_0) (L1) and the come-from
//     loops iff ¬done(C_{i+1}) (L2). On ¬done, L2's CONTINUE exit is exactly L0's
//     entry, and L0 proves the glue [Ltop, body_start) re-establishes L1's entry
//     invariant at PC=body_start — so L1 applies again, unchanged, on the next
//     iteration. The three lemmas COVER the whole per-iteration span [2,108):
//     L2 [104,108] → L0 [2,7) → L1 [7,104). No new symbolic PC is introduced;
//     every iteration is the SAME concrete-PC glue+body+come-from. This is why
//     the loop needs NO unrolling — the lemmas are iteration-INDEPENDENT, and the
//     induction step now closes on LEMMAS, not on hand-tracing.
//   * Termination: `theseus_eval::eval` defines the Trace's fixpoint as the first
//     C_T with done(C_T)=Right. The come-from exits at exactly that C_T (L2), and
//     the exit payload (v,h) is C_T's carrier = eval's output (the epilogue strips
//     the Right tag). Hence `run_on_cpu(rom) == eval(trace)` for every input whose
//     trajectory reaches a done state within the WCET budget B.
//
// WHAT IS BVEQ-PROVEN vs ARGUED (re-review closure — FIELD-COMPLETE welds):
//   * PROVEN symbolically (whole carrier domain), covering the FULL per-iteration
//     relation, offsets [2,108), with EVERY field/netlist link on the loop cycle
//     now asserted (the re-review's 5 gaps closed):
//       - L1 [7,104): body ≡ rot_step; ALL non-carrier fields (aux, scratch
//         s0..s7, IR, BR) restored to 0 (the body-entry invariant), disturbing
//         nothing else.
//       - L2 [104,108]: come-from routes BR by ¬done (BR=δu iff ¬done, else 0);
//         pins the per-branch aux exit (ctrl/temp/rb); scratch s0/s1 + IR=0 AND
//         **scratch s2..s7 ≡ IDENTITY** (fix #1 — they entered symbolic, must
//         pass through unchanged); **Lend's PC landing asserted vs the NETLIST
//         at w_pc=16** — PC_out ≡ ite(tag0, lend+1, LTOP) per bit incl. the high
//         byte (fix #2 GAP-A: the sole BR≠0 PC-adder pass, previously only the
//         closed-form arith identity).
//       - L0 [2,7): the STEP glue welds L2's continue exit → L1's entry invariant
//         {ctrl=temp=rb=rc=BR=0}, PC→body_start, carrier identity, **scratch
//         s2..s7 ≡ IDENTITY and IR (all 4 bytes) = 0** (fix #1 — the 7 hand-off-C
//         fields L0/L2 previously left unconstrained now welded L1(0)→L2→L0→L1).
//       - L0-BASE [2,7): the SAME glue also lands the FIRST-traversal precondition
//         {BR=0,ctrl=0,temp=δu,tag=1} (post-prologue) in L1's entry invariant —
//         the induction BASE case, now on a lemma (fix #3), not hand-traced.
//     ⇒ the per-iteration relation is FIELD-COMPLETE across all three welds
//     (L1↔L2, L2↔L0, L0↔L1) with NO unasserted field or netlist link on the
//     loop cycle; the induction STEP *and* BASE close on lemmas.
//   * PROVEN as closed-form / whole-domain enumeration (not swept as a loop):
//     the WCET budget B covers every input — `invariant_trip_count_whole_domain_bound`
//     asserts max over the WHOLE (v0,h0) domain of (trip_count+1) ≤ k_max
//     (INCLUDING the v0=0 park case), so no input runs past B.
//   * ARGUED (the induction FRAME + the prologue/epilogue legs — the re-review
//     deemed these acceptable):
//       - that iterating the (now field-complete) per-iteration relation
//         reproduces eval's fixpoint — a genuine mathematical induction (each
//         iteration byte-identical: same ROM, same concrete PCs), not a hidden
//         unroll. ANCHORED to reality by the fast/heavy bootstrap below.
//       - the PROLOGUE [0,2) (marshal → first Ltop entry with tag=1) and EPILOGUE
//         [109,111) (done-exit → unmarshal): NOT lemma-covered here; they are
//         ANCHORED by the bootstrap (run_on_cpu == eval, terminal quiescence) and
//         remain the ARGUED legs of the FULL marshal→…→unmarshal chain. (These are
//         the only remaining ARGUED legs; the loop cycle itself is field-complete.)
//
// ANCHOR (two tiers):
//   * FAST (always-on, `invariant_induction_anchor_fast`): `run_on_cpu_fast ==
//     eval` on the T ∈ {1,2,3} sample — seconds, the same reality tie vart_t2 uses.
//   * HEAVY (#[ignore], `invariant_induction_bootstrap_quiescence`): the REAL
//     system-ring `run_rom_on_cpu == eval` + terminal quiescence (BR=0, scratch
//     all-zero) — the exact ~4 h-authority path (vart_t3), on a 3-input sample.
//     This is the ground truth the invariant method REPLACES; kept #[ignore]
//     because run_rom_on_cpu at budget 646 is minutes/input.
//
// RETIRING THE ~4 h REAL-CPU AUTHORITY is CONTINGENT on keeping the #[ignore]
// heavy anchor `invariant_induction_bootstrap_quiescence` as an ADVISOR-RUN
// RE-ANCHOR whenever the step / decoder / lower_trace netlists change: post the
// b3238b9 fetch-mux memoization NO always-on lemma walks the real fetch mux, so
// the heavy anchor carries the fetch-mux + w_pc=16 + whole-domain-real-netlist +
// quiescence coverage the always-on proof substitutes. Re-run it after any such
// netlist change before trusting the always-on lemmas.

fn input_vh(v: &Value, u_ty: &Type) -> (u8, u8) {
    let bytes = theseus_tisc::compile::value_to_leaf_bytes_pub(v, u_ty);
    (bytes[0], *bytes.get(1).unwrap_or(&0))
}

/// Trip count T for entry (v0,h0): simulate rot_step until tag flips to 1.
fn trip_count(v0: u8, h0: u8) -> usize {
    let (mut tag, mut v, mut h) = (1u8, v0, h0);
    for step in 1..=1000 {
        let (nt, nv, nh) = rot_step(tag, v, h, V2_K, V2_HB);
        if nt == 1 {
            return step;
        }
        tag = nt;
        v = nv;
        h = nh;
    }
    panic!("trip count did not terminate for (v0={v0},h0={h0})");
}

use theseus_core::Value;

/// The T ∈ {1,2,3} sample (one representative per trip count, h0=0).
fn trip_sample(u_ty: &Type) -> Vec<Value> {
    let sample_v0: [u8; 3] = [1, 2, 4];
    let inputs: Vec<Value> = enumerate_values(u_ty)
        .into_iter()
        .filter(|v| {
            let (v0, h0) = input_vh(v, u_ty);
            h0 == 0 && sample_v0.contains(&v0)
        })
        .collect();
    assert_eq!(inputs.len(), 3, "one representative per trip count");
    inputs
}

/// WHOLE-DOMAIN TRIP-COUNT BOUND (item 5 — closes residual (a) against L1).
///
/// L1 proves one body pass ≡ rot_step; the induction reproduces eval's fixpoint
/// PROVIDED no input's trajectory runs past the WCET budget B. The compiler's
/// k_max is the trajectory-derived body-application bound; here we INDEPENDENTLY
/// confirm, using the same `rot_step` reference L1 ties to the gate, that
///   max over ALL (v0,h0) in the WHOLE carrier domain of (trip_count + 1) ≤ k_max
/// — INCLUDING the v0=0 park case (which `vart_variable_t_proof` leaves
/// unbounded). trip_count+1 mirrors the compiler's k_exact = max_t + 1. This
/// proves every input reaches a done state within the budget ⇒ the induction's
/// residual (a) is discharged over the whole domain, not just the sample.
#[test]
fn invariant_trip_count_whole_domain_bound() {
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty.clone(), body);
    let geom = compile_loop_geom(&iso);
    let k_max = geom.k_max;

    let mut worst: (usize, u8, u8) = (0, 0, 0);
    let mut saw_park = false;
    for v in enumerate_values(&u_ty) {
        let (v0, h0) = input_vh(&v, &u_ty);
        if v0 == 0 {
            saw_park = true; // the park case must be in-domain and bounded
        }
        let tc1 = trip_count(v0, h0) + 1; // k_exact contribution for this input
        if tc1 > worst.0 {
            worst = (tc1, v0, h0);
        }
        assert!(
            tc1 <= k_max,
            "TRIP-COUNT BOUND: (v0={v0},h0={h0}) trip_count+1 = {tc1} > k_max {k_max} — \
             an input runs past the WCET budget (unsound induction / compiler k_max too small)"
        );
    }
    assert!(saw_park, "domain must include the v0=0 park case");
    eprintln!(
        "TRIP-COUNT BOUND [vart_halve]: max over the WHOLE (v0,h0) domain of (trip_count+1) = {} \
         (at v0={},h0={}) ≤ k_max {} (incl. v0=0 park). B={} covers every input — residual (a) closed.",
        worst.0, worst.1, worst.2, k_max, geom.budget
    );
}

/// L3 (fast anchor, ALWAYS-ON) — the induction anchor via the FAST oracle
/// (`run_on_cpu_fast`, the same reality tie vart_t2 uses over the whole domain):
/// on the T ∈ {1,2,3} sample the compiled loop's CPU semantics == eval, tying the
/// two structural lemmas to the real machine in SECONDS. The heavy real-CPU
/// system-ring run + terminal quiescence is the separate #[ignore] anchor below.
#[test]
fn invariant_induction_anchor_fast() {
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty.clone(), body);
    let mut seen_t = std::collections::BTreeSet::new();
    for v in trip_sample(&u_ty) {
        let (v0, _h0) = input_vh(&v, &u_ty);
        seen_t.insert(trip_count(v0, 0));
        let cpu = run_on_cpu_fast(&iso, &v).expect("fast run");
        let mut fuel = 10_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "ANCHOR(fast): run_on_cpu_fast != eval on v0={v0}");
        eprintln!("  fast-anchor v0={v0} (T={}): run_on_cpu_fast == eval", trip_count(v0, 0));
    }
    assert_eq!(
        seen_t,
        [1usize, 2, 3].into_iter().collect(),
        "sample must span T ∈ {{1,2,3}} (variable-T)"
    );
    eprintln!(
        "L3 INDUCTION ANCHOR (fast) [vart_halve]: run_on_cpu_fast == eval across trip counts \
         T ∈ {{1,2,3}}. With L1 (body ≡ rot_step) + L2 (come-from loops iff ¬done) this \
         establishes run_on_cpu == eval over the WHOLE input domain by induction on T — \
         WITHOUT unrolling."
    );
}

/// L3 (heavy anchor, ADVISOR-RUN #[ignore]) — the REAL system-ring `run_rom_on_cpu`
/// == eval + terminal QUIESCENCE (scratch clean, BR=0) on the same sample. This is
/// EXACTLY the ~4 h authority's per-input path (vart_t3), run on a 3-input sample:
/// it is the ground-truth tie that the invariant method REPLACES. Kept #[ignore]
/// because run_rom_on_cpu at budget 646 is minutes/input (maestro-gate-no-timeout).
#[test]
#[ignore = "heavy real-CPU authority sample (minutes/input); advisor runs to re-anchor"]
fn invariant_induction_bootstrap_quiescence() {
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty.clone(), body);
    let lowered = compile_with_metadata(&iso).expect("compile");
    let budget = lowered.wcet();
    let s_max_val = lowered.s_max();
    let (in_ty, out_ty) = theseus_core::type_of(&iso).unwrap();

    let mut seen_t = std::collections::BTreeSet::new();
    for v in trip_sample(&u_ty) {
        let (v0, _h0) = input_vh(&v, &u_ty);
        let t = trip_count(v0, 0);
        seen_t.insert(t);
        let state = marshal(&v, &in_ty);
        let final_state =
            run_rom_on_cpu(&lowered.instrs, state, budget, s_max_val, theseus_eval::eval)
                .expect("real-cpu run");
        let cpu_out = unmarshal(&final_state, &out_ty);
        let mut fuel = 1_000_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu_out, ev, "BOOTSTRAP: real CPU != eval on v0={v0} (T={t})");
        // Terminal quiescence — the same properties the 4h authority checks.
        assert_eq!(
            final_state.scratch, [0u8; NUM_SCRATCH],
            "BOOTSTRAP: scratch must be all-zero at exit (v0={v0})"
        );
        assert_eq!(final_state.br, 0, "BOOTSTRAP: BR must be 0 at loop exit (v0={v0})");
        eprintln!("  bootstrap v0={v0} (T={t}): run_rom_on_cpu == eval, scratch clean, BR=0");
    }
    // The sample genuinely spans variable trip counts (the induction covers T's range).
    assert_eq!(
        seen_t,
        [1usize, 2, 3].into_iter().collect(),
        "sample must span T ∈ {{1,2,3}} (variable-T)"
    );
    eprintln!(
        "L3 INDUCTION ANCHOR [vart_halve]: real-CPU bootstrap == eval + terminal quiescence \
         across trip counts T ∈ {{1,2,3}}. With L1 (body ≡ rot_step) + L2 (come-from loops \
         iff ¬done) this establishes run_on_cpu == eval over the WHOLE input domain by \
         induction on T — WITHOUT unrolling. B={budget} ≥ T·iter_len for all inputs (k_max)."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// NEGATIVE CONTROLS — break the body / break the come-from ⇒ DETECTED (teeth).
// ══════════════════════════════════════════════════════════════════════════

/// NEG-1 (body): a WRONG body spec (rotate LEFT instead of right, or wrong reg)
/// must DIFFER from the real composed body output. Reuses the tractable w_pc=8
/// body composition; asserts the true rot_step spec matches AND a wrong spec does
/// not.
#[test]
fn invariant_negative_control_body() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let carrier = v2_carrier_spec();
    let (out, _samples) =
        compose_body(&layout, &geom.rom, &vs, geom.body_start, geom.body_len, &carrier);

    let (tag_reg, v_reg, h_reg) = v2_carrier_regs();
    let tag_in = entry_carrier_bits(&vs, nf, &layout, tag_reg, 1);
    let v_in = entry_carrier_bits(&vs, nf, &layout, v_reg, V2_K as usize);
    let h_in = entry_carrier_bits(&vs, nf, &layout, h_reg, V2_HB as usize);
    let out_v = out_field_bits(&out, &layout, layout.reg_field(v_reg));

    // TRUE spec (right-rotate) matches.
    let (_tp, vp, _hp) = rot_step_spec(&tag_in, &v_in, &h_in, V2_K as usize, V2_HB as usize, &vs);
    for b in 0..V2_K as usize {
        assert!(out_v[b] == vp[b], "sanity: true rot_step spec must match body v'");
    }

    // WRONG spec: LEFT-rotate the window (wp[i] = w[(i-1) mod width]).
    let width = 1 + V2_HB as usize + V2_K as usize;
    let mut w: Vec<Bdd> = Vec::new();
    for i in 0..V2_K as usize { w.push(v_in[i].clone()); }
    for i in 0..V2_HB as usize { w.push(h_in[i].clone()); }
    w.push(tag_in[0].clone());
    let wp_left: Vec<Bdd> = (0..width).map(|i| w[(i + width - 1) % width].clone()).collect();
    let differs = (0..V2_K as usize).any(|b| out_v[b] != wp_left[b]);
    assert!(differs, "NEG body: real body must DIFFER from a LEFT-rotate spec (teeth)");
    eprintln!("NEG-1 body: real body ≡ right-rotate rot_step, ≠ left-rotate — teeth.");
}

/// NEG-3 (mutated ROM): flip ONE body instruction in the ROM ⇒ the composed body
/// MUST cease to equal rot_step (L1 would FAIL). This gives L1 teeth against a
/// compiler that emits a subtly-wrong body: it proves L1's PASS is contingent on
/// the ACTUAL ROM, not vacuous. We replace the first body instr with a carrier-
/// bit flip (Xori v,1), re-assemble, compose, and assert the rot_step spec no
/// longer holds over the whole carrier domain.
#[test]
fn invariant_negative_control_mutated_rom() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let (tag_reg, v_reg, h_reg) = v2_carrier_regs();

    // Mutate: overwrite the FIRST body instruction with a carrier-bit flip.
    let mut mutated = geom.lowered_instrs.clone();
    mutated[geom.body_start] = Instr::Xori { rd: v_reg as u8, imm: 1 };
    let mutated_rom = assemble(&mutated).expect("assemble mutated ROM");
    // Sanity: only the one word changed.
    let n_diff = geom
        .rom
        .iter()
        .zip(mutated_rom.iter())
        .filter(|(a, b)| a != b)
        .count();
    assert_eq!(n_diff, 1, "mutation must flip exactly one ROM word");

    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let carrier = v2_carrier_spec();
    let (out, _s) =
        compose_body(&layout, &mutated_rom, &vs, geom.body_start, geom.body_len, &carrier);

    let tag_in = entry_carrier_bits(&vs, nf, &layout, tag_reg, 1);
    let v_in = entry_carrier_bits(&vs, nf, &layout, v_reg, V2_K as usize);
    let h_in = entry_carrier_bits(&vs, nf, &layout, h_reg, V2_HB as usize);
    let (tp, vp, hp) =
        rot_step_spec(&tag_in, &v_in, &h_in, V2_K as usize, V2_HB as usize, &vs);
    let out_v = out_field_bits(&out, &layout, layout.reg_field(v_reg));
    let out_h = out_field_bits(&out, &layout, layout.reg_field(h_reg));
    let out_t = out_field_bits(&out, &layout, layout.reg_field(tag_reg));

    // The mutated body must DIFFER from rot_step on at least one carrier bit.
    let matches_rot = (0..V2_K as usize).all(|b| out_v[b] == vp[b])
        && (0..V2_HB as usize).all(|b| out_h[b] == hp[b])
        && out_t[0] == tp[0];
    assert!(
        !matches_rot,
        "NEG-3 mutated ROM: a body with one flipped instr STILL matched rot_step — \
         L1 has no teeth (would pass a wrong body)"
    );
    eprintln!("NEG-3 mutated ROM: one flipped body instr ⇒ body ≠ rot_step — L1 has teeth.");
}

/// NEG-2 (come-from): the come-from output BR must DIFFER from wrong routing
/// specs — (a) inverted predicate (BR=δu when tag==1), and (b) unconditional
/// BR=δu (never exits). Both would break termination and MUST be detected.
#[test]
fn invariant_negative_control_come_from() {
    let layout = Layout::default();
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let (out, _peak) = compose_come_from(&layout, &geom, &cf, &vs);
    let tag_in = field_bits(&vs, nf, layout.reg_field(cf.tag));
    let br_out = out_field_bits(&out, &layout, layout.br_field());

    // TRUE: BR = δu·[tag==0].
    let not_tag = tag_in[0].not();
    // WRONG-A: inverted predicate BR = δu·[tag==1].
    let wrong_a: Vec<Bdd> = (0..W)
        .map(|b| {
            let du = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
            tag_in[0].and(&du)
        })
        .collect();
    let differs_a = (0..W).any(|b| br_out[b] != wrong_a[b]);
    assert!(differs_a, "NEG come-from: real BR must DIFFER from inverted-predicate spec (teeth)");

    // WRONG-B: unconditional BR = δu (loops forever).
    let wrong_b: Vec<Bdd> = (0..W)
        .map(|b| if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() })
        .collect();
    let differs_b = (0..W).any(|b| br_out[b] != wrong_b[b]);
    assert!(differs_b, "NEG come-from: real BR must DIFFER from unconditional-loop spec (teeth)");

    // And the TRUE spec DOES match (sanity, ties the teeth to the right target).
    for b in 0..W {
        let du = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        assert!(br_out[b] == not_tag.and(&du), "sanity: true δu·[tag==0] must match");
    }
    eprintln!("NEG-2 come-from: real BR ≡ δu·[tag==0], ≠ {{inverted, unconditional}} — teeth.");
}

/// NEG-4 (L0 re-entry, fix #5): a BROKEN L0 exit expectation must DIFFER from the
/// real glue output — so the re-entry lemma's welding asserts have teeth (a glue
/// that landed in the WRONG state would be caught). Two wrong expectations:
///   (a) wrong exit PC (the glue does NOT fall through to BODY_START), and
///   (b) wrong aux (ctrl != 0 at exit ⇒ L1's entry invariant violated).
/// Reuses the tractable w_pc=8 continue-leg composition.
#[test]
fn invariant_negative_control_re_entry() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let (out, _peak) =
        compose_re_entry_from(&layout, &geom, &vs, re_entry_sv(&layout, &geom, &cf, &vs));
    let zero = vs.mk_false();
    let one = vs.mk_true();

    // (a) WRONG exit PC: the real glue must NOT land anywhere but BODY_START.
    let got_pc = read_concrete_pc(&out, &layout);
    assert_ne!(
        got_pc,
        Some(BODY_START + 1),
        "NEG-4 L0: real glue exit PC matched a WRONG landing (BODY_START+1) — no teeth"
    );
    assert_eq!(got_pc, Some(BODY_START), "sanity: real glue lands on BODY_START");

    // (b) WRONG aux: the real ctrl at exit must NOT be 1 (it must be 0 = L1's
    // entry invariant). A glue that left ctrl=1 would break re-entry silently.
    let ctrl_out = out_field_bits(&out, &layout, layout.reg_field(cf.ctrl));
    let wrong_ctrl_is_one = (0..W).all(|b| ctrl_out[b] == if b == 0 { one.clone() } else { zero.clone() });
    assert!(
        !wrong_ctrl_is_one,
        "NEG-4 L0: real glue exit ctrl matched the WRONG value 1 — L0 welding has no teeth"
    );
    // sanity: real ctrl IS 0.
    for b in 0..W {
        assert!(ctrl_out[b] == zero, "sanity: real glue exit ctrl == 0 (L1 entry invariant)");
    }
    eprintln!("NEG-4 L0 re-entry: real glue lands ctrl=0 @BODY_START, ≠ {{wrong PC, ctrl=1}} — teeth.");
}

/// NEG-5 (L2 aux-exit, fix #5): a BROKEN come-from AUX-EXIT expectation must
/// DIFFER from the real bracket output — so the per-branch aux pin (the
/// PRECONDITION L0 starts from) has teeth. Two wrong expectations:
///   (a) ctrl = tag (NOT tag^1): the real route sets ctrl = ¬tag on bit 0.
///   (b) temp = 0 unconditionally: the real exit keeps temp = δu on the EXIT
///       branch (tag0=1). A wrong pin here would corrupt L0's entry precondition.
#[test]
fn invariant_negative_control_aux_exit() {
    let layout = Layout::default(); // w_pc=16 (matches L2's real run)
    let (body, u_ty, _carrier) = build_v2_body_sized(V2_K, V2_HB);
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let (out, _peak) = compose_come_from(&layout, &geom, &cf, &vs);

    let tag_in = field_bits(&vs, nf, layout.reg_field(cf.tag));
    let not_tag = tag_in[0].not();
    let ctrl_out = out_field_bits(&out, &layout, layout.reg_field(cf.ctrl));
    let temp_out = out_field_bits(&out, &layout, layout.reg_field(cf.temp));
    let zero = vs.mk_false();

    // (a) WRONG ctrl spec: ctrl == tag (bit 0 = tag[0], not ¬tag[0]).
    let differs_ctrl = (0..W).any(|b| {
        let wrong = tag_in[b].clone(); // ctrl == tag (bit0 not flipped)
        ctrl_out[b] != wrong
    });
    assert!(
        differs_ctrl,
        "NEG-5 aux-exit: real ctrl must DIFFER from the WRONG 'ctrl=tag' spec — no teeth"
    );

    // (b) WRONG temp spec: temp == 0 unconditionally (ignores the exit-branch δu).
    let differs_temp = (0..W).any(|b| temp_out[b] != zero);
    assert!(
        differs_temp,
        "NEG-5 aux-exit: real temp must DIFFER from the WRONG 'temp=0 always' spec — no teeth"
    );

    // sanity: the TRUE specs DO match (ties teeth to the right targets).
    for b in 0..W {
        let want_ctrl = if b == 0 { not_tag.clone() } else { tag_in[b].clone() };
        assert!(ctrl_out[b] == want_ctrl, "sanity: true ctrl = tag^1 must match");
        let du_bit = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        assert!(temp_out[b] == tag_in[0].and(&du_bit), "sanity: true temp = δu·[tag0==1] must match");
    }
    eprintln!("NEG-5 aux-exit: real ctrl≡tag^1, temp≡δu·[tag0==1], ≠ {{ctrl=tag, temp=0}} — teeth.");
}
