#![cfg(feature = "heavy-tests")]
// HEAVY tier: run under `--features heavy-tests --test-threads=1`.
//! BVEQ-INVARIANT-MDE-SPILL -- extend the inductive-invariant loop-proof method
//! (proven on vart_halve in `bveq_invariant_poc.rs`) to the **mde_spill** loop
//! (`mde_spill.rs`'s `mde_spill_walk` Trace), retiring its slow #[ignore]
//! real-CPU authority (`mde_spill_real_cpu`) with a fast symbolic proof.
//!
//! ── The ONE structural difference from vart_halve ─────────────────────────
//! mde_spill's carrier `Sum(N5, Bit)` has **5 leaves** (tag + 4 payload). The
//! register budget is 4, so ONE payload leaf SPILLS to Scratch(2)
//! (SCRATCH_SPILL_BASE) and lives there ACROSS the back edge. So the body-entry
//! invariant is NOT "all non-carrier = 0" (as in vart_halve): the spilled leaf
//! is loop-carried STATE resident in Scratch(2), exactly like tag/payload regs.
//!
//! Therefore the carrier for the symbolic proof = {tag reg, the 3 register-
//! resident payload leaves, AND Scratch(2)}. The body function is
//! `mde_walk_bytes` (a fixed byte map over the 5 leaf bytes). L1 proves the
//! composed body span ≡ mde_walk_bytes over the WHOLE carrier domain WITH the
//! spilled scratch treated as carrier (symbolic, asserted = the body function's
//! output for that leaf, NOT zero). L0/L2 weld the spilled-scratch value across
//! the back edge as loop-carried state (identity through the glue + come-from,
//! not restored to 0). Genuinely-transient scratch (s0/s1 decoder-reserved, and
//! the paging Xchg's net-zero bracket) stays asserted 0 at the iteration
//! boundary.
//!
//! Everything else -- StepEngine, concrete-PC body composition, the come-from /
//! re-entry glue, trip-count bound, negative controls -- is REUSED from the
//! vart_halve template, adapted to mde_spill's carrier + body function.
//!
//! ── PROVEN vs ARGUED (parity with the vart_halve template) ─────────────────
//! PROVEN symbolically (whole carrier domain, real netlist): L1 body ≡
//! mde_walk_bytes (spilled Scratch(2) as carrier), L2 come-from BR-router + aux
//! exit + GAP-A PC landing, L0/L0-BASE re-entry welds -- the FIELD-COMPLETE
//! per-iteration relation, each with a NEGATIVE CONTROL (body, spill-is-carrier,
//! mutated-ROM, come-from, re-entry, aux-exit) proving the asserts have teeth.
//! ARGUED (same legs the template leaves argued): the tag∈{0,1} canonical carrier
//! domain (case-split faithful at every real iteration boundary), the induction
//! FRAME (iterating the field-complete per-iteration relation reproduces eval's
//! fixpoint -- a math induction, not a hidden unroll), and the PROLOGUE/EPILOGUE
//! legs (marshal→first Ltop, done-exit→unmarshal) -- anchored by the fast/heavy
//! run_rom_on_cpu==eval + quiescence bootstrap, not lemma-covered here.

use std::collections::BTreeMap;
use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type};

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::compile::{
    compile_with_metadata, marshal, mde_walk_bytes, run_on_cpu_fast, run_rom_on_cpu, unmarshal,
};
use theseus_tisc::step::step_with;
use theseus_tisc::types::{state_ty_with, Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH, W};

// ══════════════════════════════════════════════════════════════════════════
// Symbolic / concrete state SV construction (bit-interleaved varmap) -- VERBATIM
// from bveq_invariant_poc.rs.  var(field f, bit b) = b*nf + f.
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

/// Build a CpuState SV field-by-field from a per-field closure.
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

fn sv_field_count(sv: &SV) -> usize {
    match sv {
        SV::Unit => 0,
        SV::Bit(_) => 1,
        SV::Pair(a, b) => sv_field_count(a) + sv_field_count(b),
    }
}

fn out_field_bits(out: &SV, layout: &Layout, f: usize) -> Vec<Bdd> {
    let nf = n_fields(layout);
    extract_word8_bdds(extract_field(out, f, nf)).to_vec()
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

struct BodyStep {
    step: usize,
    peak: usize,
    sum: usize,
    secs: f64,
}

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

// ══════════════════════════════════════════════════════════════════════════
// StepEngine -- the MEMOIZED per-step evaluator (VERBATIM from the template;
// fetch/unfetch short-circuited to `IR ^= rom[pc]`, decode_execute walked in
// full symbolically). See bveq_invariant_poc.rs for the faithfulness argument.
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

    fn eval_step_concrete_pc(
        &self,
        cur: SV,
        pc: usize,
        vs: &BddVariableSet,
        ctx: &mut WalkCtx,
    ) -> Result<SV, theseus_tisc::bveq_netlist::NetlistWalkError> {
        let word = self.rom[pc];
        let after_fetch = self.xor_ir(&cur, word, vs);
        let after_de = eval_bdd(&self.decode_execute, after_fetch, vs, ctx)?;
        let after_unfetch = self.xor_ir(&after_de, word, vs);
        let after_add = eval_bdd(&self.pc_add_br, after_unfetch, vs, ctx)?;
        eval_bdd(&self.pc_inc, after_add, vs, ctx)
    }
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
// ROM span geometry -- REUSED from the template. mde_spill's lower_trace
// skeleton is IDENTICAL to vart_halve's (prologue 2, Ltop SWAPBR rc, JOIN 4,
// body, ROUTE 4, Lend SWAPBR rb, epilogue 2), so LOOP_OVERHEAD=10.
// ══════════════════════════════════════════════════════════════════════════

const LOOP_OVERHEAD: usize = 10;
const PROLOGUE_LEN: usize = 2;
const JOIN_LEN: usize = 4;
const LTOP: usize = PROLOGUE_LEN;
const BODY_START: usize = LTOP + 1 + JOIN_LEN; // = 7
const ROUTE_LEN: usize = 4;

struct LoopGeom {
    lowered_instrs: Vec<Instr>,
    rom: Vec<u32>,
    iter_len: usize,
    body_len: usize,
    body_start: usize,
    lend_pc: usize,
    budget: usize,
    k_max: usize,
}

fn compile_loop_geom(iso: &Iso) -> LoopGeom {
    let lowered = compile_with_metadata(iso).expect("Trace must lower");
    let rom = assemble(&lowered.instrs).expect("assemble ROM");
    assert_eq!(lowered.loop_terms.len(), 1, "expect a single top-level loop");
    let iter_len = lowered.loop_terms[0].iter_len;
    let body_len = iter_len - LOOP_OVERHEAD;
    let body_start = BODY_START;
    let lend_pc = LTOP + iter_len - 1;

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
    assert!(
        matches!(instrs[lend_pc - 1], Instr::Cswap { .. }),
        "route (offset {}) must be CSWAP ctrl,temp,rb, got {:?}",
        lend_pc - 1,
        instrs[lend_pc - 1]
    );
    assert_eq!(
        lend_pc - ROUTE_LEN - body_start,
        body_len,
        "body span length must equal body_len"
    );

    // SOUNDNESS: the body span must be Swapbr-free (licenses concrete-PC pinning).
    for pc in body_start..(lend_pc - ROUTE_LEN) {
        assert!(
            !matches!(instrs[pc], Instr::Swapbr { .. }),
            "BODY SWAPBR-FREE: body instr at pc={pc} is a Swapbr ({:?}) -- unsound",
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
// mde_spill build (reuse mde_spill.rs's build_walk_body recipe).
// ══════════════════════════════════════════════════════════════════════════

fn nat_chain(n: usize) -> Type {
    assert!(n >= 2);
    let mut t = Type::sum(Type::One, Type::One);
    for _ in 2..n {
        t = Type::sum(Type::One, t);
    }
    t
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

/// The mde_spill carrier and its body prim (Sum(N5, Bit), 5 leaves).
fn build_walk_body() -> (Iso, Type, Type) {
    let n5 = nat_chain(5);
    let bit = Type::sum(Type::One, Type::One);
    let carrier = Type::sum(n5.clone(), bit.clone());
    let vals = enumerate_values(&carrier);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let sv = marshal(v, &carrier);
        let mut bytes: Vec<u8> = sv.regs[..5].to_vec();
        mde_walk_bytes(&mut bytes);
        let out_val = unmarshal(&state_with_regs(&bytes), &carrier);
        let out_re = marshal(&out_val, &carrier);
        assert_eq!(&out_re.regs[..5], &bytes[..], "walk not closed on canonical encodings");
        forward.insert(v.clone(), out_val.clone());
        assert!(
            backward.insert(out_val, v.clone()).is_none(),
            "walk is not injective on canonical values"
        );
    }

    let prim = PrimDef {
        forward_name: "mde_spill_walk".into(),
        backward_name: "mde_spill_walk_inv".into(),
        input_ty: carrier.clone(),
        output_ty: carrier.clone(),
        forward,
        backward,
        symbolic: None,
    };
    (Iso::Prim(Box::new(prim)), n5, carrier)
}

// ══════════════════════════════════════════════════════════════════════════
// DIAGNOSTIC -- determine the carrier layout empirically.
//
// The 5 leaf bytes [b0..b5] = [tag, p1, p2, p3, p4] marshal to some registers +
// one scratch (the spilled leaf). We step the body span concretely with a
// distinct value in each leaf and read back where each landed, to pin the
// (reg/scratch, leaf-index) mapping before the symbolic proof.
// ══════════════════════════════════════════════════════════════════════════

const EVAL_FUEL: u64 = 8_000_000_000;

/// Step the body span [body_start, body_start+body_len) concretely from a full
/// StateVals, returning the terminal StateVals (regs + scratch).
fn concrete_step_body_full(
    layout: &Layout,
    rom: &[u32],
    body_start: usize,
    body_len: usize,
    regs_in: [u8; NUM_REGS],
    scratch_in: [u8; NUM_SCRATCH],
) -> StateVals {
    let iso = step_with(layout, rom);
    let s = StateVals {
        regs: regs_in,
        pc: body_start as u16,
        br: 0,
        ir: 0,
        scratch: scratch_in,
        mem: [0; MEM_WORDS],
    };
    let mut cur = s.encode_with(layout);
    for _ in 0..body_len {
        let mut fuel = EVAL_FUEL;
        cur = theseus_eval::eval(&iso, cur, &mut fuel).expect("body eval");
    }
    StateVals::decode_with(layout, &cur)
}

#[test]
#[ignore = "diagnostic: prints the mde_spill body carrier layout (regs+scratch)"]
fn diag_mde_carrier_layout() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, carrier) = build_walk_body();
    eprintln!("carrier type: {carrier:?}");
    eprintln!("u_ty (input): {u_ty:?}");
    // marshal a few carrier values and show the 5 leaf bytes.
    for v in enumerate_values(&carrier).into_iter().take(6) {
        let sv = marshal(&v, &carrier);
        eprintln!("  val {v:?} -> regs[..6]={:?} (5 leaf bytes = regs[..5])", &sv.regs[..6]);
    }
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    eprintln!(
        "geom: iter_len={}, body_len={}, body_start={}, lend={}, budget={}, k_max={}",
        geom.iter_len, geom.body_len, geom.body_start, geom.lend_pc, geom.budget, geom.k_max
    );
    eprintln!("ROM ({} instrs):", geom.lowered_instrs.len());
    for (i, ins) in geom.lowered_instrs.iter().enumerate() {
        eprintln!("  [{i:2}] {ins:?}");
    }

    // PINNED HOME MAP (derived by matching the ROM body [7..22) to mde_walk_bytes
    // instruction-by-instruction, verified below over the WHOLE 32-value carrier
    // domain by concrete body stepping):
    //   leaf 0 (tag) = Reg(1)      [prologue XORI r1,1 sets the Right tag]
    //   leaf 1       = Reg(0)
    //   leaf 2       = Reg(2)
    //   leaf 3       = Reg(3)
    //   leaf 4       = Scratch(2)  [SPILLED; paged into r7 at [7], out at [21]]
    // The body Xchg r7,s2 pages the spilled leaf into r7 for the body and back to
    // s2 at the end. So at the ITERATION BOUNDARY the spilled leaf is in Scratch(2).
    let home = mde_carrier_home();
    let all_ok = enumerate_values(&carrier).iter().all(|v| {
        let s = marshal(v, &carrier);
        let leaves = [s.regs[0], s.regs[1], s.regs[2], s.regs[3], s.regs[4]];
        let mut regs = [0u8; NUM_REGS];
        let mut scratch = [0u8; NUM_SCRATCH];
        for (j, cf) in home.iter().enumerate() {
            match cf {
                CarrierHome::Reg(r) => regs[*r] = leaves[j],
                CarrierHome::Scratch(s) => scratch[*s] = leaves[j],
            }
        }
        let out = concrete_step_body_full(&layout, &geom.rom, geom.body_start, geom.body_len, regs, scratch);
        let mut want = leaves;
        mde_walk_bytes(&mut want);
        home.iter().enumerate().all(|(j, cf)| {
            let got = match cf {
                CarrierHome::Reg(r) => out.regs[*r],
                CarrierHome::Scratch(s) => out.scratch[*s],
            };
            got == want[j]
        })
    });
    eprintln!("  pinned home map {home:?} reproduces mde_walk over WHOLE carrier: {all_ok}");
    assert!(all_ok, "pinned carrier home map does not reproduce the walk");
}

// ══════════════════════════════════════════════════════════════════════════
// CARRIER LAYOUT (pinned + validated in diag_mde_carrier_layout).
//
// The 5 leaf bytes and their loop-boundary homes. leaf 4 SPILLS to Scratch(2).
// tag = leaf 0 = Reg(1). This is the ONE difference from vart_halve: the carrier
// includes a SCRATCH field, and the body-entry invariant for that field is the
// LOOP-CARRIED leaf value (not 0).
// ══════════════════════════════════════════════════════════════════════════

#[derive(Clone, Copy, Debug, PartialEq)]
enum CarrierHome {
    Reg(usize),
    Scratch(usize),
}

/// leaf index → its home at the ITERATION BOUNDARY (body-entry / body-exit).
fn mde_carrier_home() -> [CarrierHome; 5] {
    [
        CarrierHome::Reg(1),     // leaf 0 = tag
        CarrierHome::Reg(0),     // leaf 1
        CarrierHome::Reg(2),     // leaf 2
        CarrierHome::Reg(3),     // leaf 3
        CarrierHome::Scratch(2), // leaf 4 = SPILLED carrier
    ]
}

const TAG_REG: usize = 1;
const SPILL_LEAF: usize = 4;
const SPILL_SCRATCH: usize = 2;
/// The spare register the body pages the spilled leaf into (Xchg r7,s2).
const SPILL_SPARE_REG: usize = 7;

/// The field index of a carrier leaf's boundary home.
fn carrier_field(layout: &Layout, leaf: usize) -> usize {
    match mde_carrier_home()[leaf] {
        CarrierHome::Reg(r) => layout.reg_field(r),
        CarrierHome::Scratch(s) => layout.scratch_field(s),
    }
}

/// Is this field index one of the 5 carrier leaf homes?
fn is_carrier_field(layout: &Layout, f: usize) -> bool {
    (0..5).any(|leaf| carrier_field(layout, leaf) == f)
}

// ══════════════════════════════════════════════════════════════════════════
// The mde_walk_bytes SYMBOLIC spec -- mirrors compile::mde_walk_bytes on BDD
// byte-vectors. Takes the 5 carrier leaf byte-vectors (each Vec<Bdd> of W bits)
// and returns the 5 output byte-vectors. This is the body-function reference L1
// proves the composed body span equals, over the WHOLE carrier domain.
//
//   b[0]^=b[1]; b[0]^=1; b[1]^=1; b[2]^=b[3]; b[3]^=b[4]; b[0]^=1;
//   if b[0]&1 { swap(1,2); swap(2,3); swap(3,4) }
//   b[0]^=1; b[3]^=b[4]; b[2]^=b[3]; b[1]^=b[2];
// ══════════════════════════════════════════════════════════════════════════

fn xor_bytes(a: &[Bdd], b: &[Bdd]) -> Vec<Bdd> {
    (0..W).map(|i| a[i].xor(&b[i])).collect()
}
fn xor_const1(a: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    // XOR the immediate 1 into bit 0 only.
    let mut out = a.to_vec();
    out[0] = a[0].xor(&vs.mk_true());
    let _ = vs;
    out
}
/// Conditional swap on predicate p (a single BDD): if p then (x,y) else (y unchanged)
/// Returns (new_x, new_y). CSWAP r_ctrl, rs, rt keyed on ctrl&1: swaps rs,rt when set.
fn cond_swap(p: &Bdd, x: &[Bdd], y: &[Bdd]) -> (Vec<Bdd>, Vec<Bdd>) {
    // ite(p, t, e) = (p ∧ t) ∨ (¬p ∧ e).
    let np = p.not();
    let ite = |t: &Bdd, e: &Bdd| p.and(t).or(&np.and(e));
    let nx: Vec<Bdd> = (0..W).map(|i| ite(&y[i], &x[i])).collect(); // p ? y : x
    let ny: Vec<Bdd> = (0..W).map(|i| ite(&x[i], &y[i])).collect(); // p ? x : y
    (nx, ny)
}

fn mde_walk_spec(leaves_in: &[Vec<Bdd>; 5], vs: &BddVariableSet) -> [Vec<Bdd>; 5] {
    let mut b: Vec<Vec<Bdd>> = leaves_in.to_vec();
    b[0] = xor_bytes(&b[0], &b[1]); // b[0]^=b[1]
    b[0] = xor_const1(&b[0], vs); // b[0]^=1
    b[1] = xor_const1(&b[1], vs); // b[1]^=1
    b[2] = xor_bytes(&b[2], &b[3]); // b[2]^=b[3]
    b[3] = xor_bytes(&b[3], &b[4]); // b[3]^=b[4]
    b[0] = xor_const1(&b[0], vs); // b[0]^=1
    // predicate = b[0] & 1  (bit 0 of b[0]).
    let p = b[0][0].clone();
    let (n1, n2) = cond_swap(&p, &b[1], &b[2]);
    b[1] = n1;
    b[2] = n2;
    let (n2b, n3) = cond_swap(&p, &b[2], &b[3]);
    b[2] = n2b;
    b[3] = n3;
    let (n3b, n4) = cond_swap(&p, &b[3], &b[4]);
    b[3] = n3b;
    b[4] = n4;
    b[0] = xor_const1(&b[0], vs); // b[0]^=1
    b[3] = xor_bytes(&b[3], &b[4]); // b[3]^=b[4]
    b[2] = xor_bytes(&b[2], &b[3]); // b[2]^=b[3]
    b[1] = xor_bytes(&b[1], &b[2]); // b[1]^=b[2]
    [b[0].clone(), b[1].clone(), b[2].clone(), b[3].clone(), b[4].clone()]
}


// ══════════════════════════════════════════════════════════════════════════
// L1 -- BODY LEMMA (with the spilled leaf as CARRIER).
//
// Body-entry SV: the 5 carrier leaf homes are SYMBOLIC (whole carrier domain,
// including Scratch(2) = the spilled leaf), EVERY other field = 0 (the body-
// entry invariant): the paging spare r7 = 0 (the Xchg pages the leaf IN from s2),
// aux regs {rc=r4,rb=r5,temp=r6}=0, s0/s1/s3..s7=0, IR=0, BR=0.
//
// This is faithful: at body-entry the machine holds the register-resident carrier
// in r0..r3, the spilled carrier in Scratch(2), and everything else at the
// reversible-body invariant 0. The body's first instr (Xchg r7,s2) pages the
// spilled leaf into r7; the last (Xchg r7,s2) pages it back -- a net-zero bracket
// that RESTORES s2 to the *transformed* spilled leaf and r7 to 0.
// ══════════════════════════════════════════════════════════════════════════

fn body_entry_sv(layout: &Layout, vs: &BddVariableSet, pc: usize) -> SV {
    let nf = n_fields(layout);
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((pc & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((pc >> 8) & 0xFF) as u8, vs)
        } else if is_carrier_field(layout, f) {
            symbolic_word8(vs, nf, f) // carrier leaf (incl. Scratch(2)) -- whole domain
        } else {
            const_word8(0, vs) // every non-carrier field at the body-entry invariant 0
        }
    })
}

/// Compose the body span [body_start, body_start+span), PC re-pinned concrete.
fn compose_body(
    layout: &Layout,
    rom: &[u32],
    vs: &BddVariableSet,
    pc_top: usize,
    span: usize,
) -> (SV, Vec<BodyStep>) {
    let sctx = StepEngine::new(layout, rom);
    let mut ctx = WalkCtx::new();
    let mut cur = body_entry_sv(layout, vs, pc_top);
    let mut samples = Vec::with_capacity(span);
    let t0 = Instant::now();
    for i in 0..span {
        let pc = pc_top + i;
        let out = sctx
            .eval_step_concrete_pc(cur, pc, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("eval_step FAILED at body step {i} (pc={pc}): {e}"));
        let next_pc = pc_top + i + 1;
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
        samples.push(BodyStep { step: i + 1, peak, sum, secs: t0.elapsed().as_secs_f64() });
    }
    (cur, samples)
}

/// Body-entry carrier byte-vector for leaf `leaf`: fully symbolic (its home's vars).
fn entry_leaf_bits(vs: &BddVariableSet, nf: usize, layout: &Layout, leaf: usize) -> Vec<Bdd> {
    let f = carrier_field(layout, leaf);
    field_bits(vs, nf, f)
}

/// Prove the mde_spill body lemma. Returns (max_peak, secs).
fn prove_body_lemma(layout: &Layout, geom: &LoopGeom) -> (usize, f64) {
    let vs = BddVariableSet::new_anonymous(n_vars(layout));
    let nf = n_fields(layout);
    let (out, samples) = compose_body(layout, &geom.rom, &vs, geom.body_start, geom.body_len);
    print_curve("mde_spill BODY (mde_walk) [full ROM, PC concrete]", &samples);

    // Body-entry carrier leaf bit-vectors (the same vars the entry SV used).
    let leaves_in: [Vec<Bdd>; 5] = std::array::from_fn(|leaf| entry_leaf_bits(&vs, nf, layout, leaf));
    let want = mde_walk_spec(&leaves_in, &vs);

    // The 5 carrier leaf outputs must equal mde_walk_spec over the WHOLE domain,
    // INCLUDING leaf 4 read from Scratch(2) (the spilled carrier).
    for leaf in 0..5 {
        let f = carrier_field(layout, leaf);
        let got = out_field_bits(&out, layout, f);
        for b in 0..W {
            assert!(
                got[b] == want[leaf][b],
                "BODY LEMMA: carrier leaf {leaf} (field {f}) bit {b} != mde_walk_spec"
            );
        }
    }

    // Every non-carrier field must be restored to 0 (body-entry invariant): the
    // paging spare r7, aux regs, s0/s1/s3..s7, IR, BR. A paging bug leaving the
    // spilled leaf in r7 (spare) at exit, or garbage in a transient scratch, is
    // caught here.
    let zero = vs.mk_false();
    for f in 0..nf {
        if is_carrier_field(layout, f) {
            continue;
        }
        if f == layout.pc_field() || (layout.pc_fields() >= 2 && f == layout.pc_hi_field()) {
            continue;
        }
        let got = out_field_bits(&out, layout, f);
        for b in 0..W {
            assert!(
                got[b] == zero,
                "BODY LEMMA: non-carrier field {f} bit {b} not restored to 0 (leak/stray write) \
                 -- e.g. paging spare r{SPILL_SPARE_REG} left dirty"
            );
        }
    }

    let peak = samples.iter().map(|s| s.peak).max().unwrap_or(0);
    (peak, samples.last().unwrap().secs)
}

/// L1 -- the mde_spill BODY LEMMA (spill-in-carrier). Body span ≡ mde_walk over
/// the whole carrier domain (incl. Scratch(2)); everything else restored to 0.
#[test]
fn invariant_body_lemma_mde_spill() {
    // w_pc=8: the body span never executes a Swapbr (BR stays 0), PC ∈ [7,21] fits
    // 8 bits. Faithful to the body (validated vs the w_pc=8 concrete step in diag).
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    eprintln!(
        "mde_spill geom: iter_len={}, body_len={}, body_start={}, lend={}, budget={}, k_max={}",
        geom.iter_len, geom.body_len, geom.body_start, geom.lend_pc, geom.budget, geom.k_max
    );
    let (peak, secs) = prove_body_lemma(&layout, &geom);
    eprintln!(
        "L1 BODY LEMMA [mde_spill]: composed body span ({} instrs) ≡ mde_walk over the WHOLE \
         carrier domain (incl. SPILLED leaf 4 in Scratch(2)); every transient field restored to 0. \
         MAX peak {} BDD nodes, {:.3}s.",
        geom.body_len, peak, secs
    );
    assert!(peak < 50_000, "BODY LEMMA peak {peak} -- expected tractable (< 50k)");
}

// ══════════════════════════════════════════════════════════════════════════
// L2 -- COME-FROM / TERMINATION LEMMA (identical structure to vart_halve; the
// come-from is a pure BR-router that must NOT disturb the carrier -- INCLUDING
// the spilled leaf in Scratch(2), which must pass through as IDENTITY).
//
// Bracket at [route_start .. lend_pc]:
//   XOR ctrl,tag ; XORI ctrl,1 ; ADDI temp,δu ; CSWAP ctrl,temp,rb ; SWAPBR rb
// CLAIM: from {ctrl=temp=rb=BR=0} + symbolic tag, BR_out = δu·[tag==0]; carrier
// (incl. Scratch(2)) undisturbed; δu back-jump lands on Ltop.
// ══════════════════════════════════════════════════════════════════════════

struct ComeFrom {
    ctrl: usize,
    tag: usize,
    temp: usize,
    rb: usize,
    delta_u: u8,
    route_start: usize,
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
            let rb = rt as usize;
            match instrs[geom.lend_pc] {
                Instr::Swapbr { rd, .. } => assert_eq!(rd as usize, rb, "Lend SWAPBR rb mismatch"),
                ref o => panic!("Lend must be SWAPBR rb, got {o:?}"),
            }
            // The come-from tag register must be the carrier tag (Reg(TAG_REG)).
            assert_eq!(tag, TAG_REG, "come-from reads tag from r{tag}, expected r{TAG_REG}");
            ComeFrom { ctrl, tag, temp, rb, delta_u, route_start }
        }
        ref o => panic!("route[3] must be CSWAP ctrl,temp,rb, got {o:?}"),
    }
}

/// Pre-route entry SV: SYMBOLIC tag (bit 0 matters), ctrl=temp=rb=BR=0,
/// PC concrete at route_start. Carrier payload + Scratch(2) SYMBOLIC (identity
/// witnesses; the bracket must not touch them). IR=0, s0/s1=0. Other scratch
/// symbolic (identity). Non-carrier regs symbolic (identity).
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
            const_word8(0, vs)
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            const_word8(0, vs)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            const_word8(0, vs)
        } else {
            symbolic_word8(vs, nf, f) // tag, payload, Scratch(2) carrier, other scratch/regs symbolic
        }
    })
}

fn compose_come_from(layout: &Layout, geom: &LoopGeom, cf: &ComeFrom, vs: &BddVariableSet) -> (SV, usize) {
    let engine = StepEngine::new(layout, &geom.rom);
    let mut ctx = WalkCtx::new();
    let mut cur = come_from_entry_sv(layout, cf, vs);
    let n = ROUTE_LEN + 1;
    let mut peak = 0usize;
    for i in 0..n {
        let pc = cf.route_start + i;
        let out = engine
            .eval_step_concrete_pc(cur, pc, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("come-from eval FAILED at step {i} (pc={pc}): {e}"));
        let (p, _s) = sv_bdd_sizes(&out);
        peak = peak.max(p);
        if i + 1 < n {
            let next_pc = cf.route_start + i + 1;
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
            cur = out;
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
    let not_tag = tag_in[0].not();
    let zero = vs.mk_false();

    // BR = δu·[tag==0].
    for b in 0..W {
        let du_bit = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        let want = not_tag.and(&du_bit);
        assert!(br_out[b] == want, "COME-FROM: BR out bit {b} != δu·[tag==0]");
    }

    // GAP-A: Lend PC landing vs the w_pc=16 netlist -- the sole BR≠0 PC-adder pass.
    assert_eq!(layout.w_pc, 16, "GAP-A PC-landing assert requires w_pc=16 adder");
    let lend_plus_1 = geom.lend_pc + 1;
    let pc_lo_out = out_field_bits(&out, layout, layout.pc_field());
    let pc_hi_out = out_field_bits(&out, layout, layout.pc_hi_field());
    for b in 0..W {
        let ltop_lo = (LTOP >> b) & 1 == 1;
        let ltop_hi = (LTOP >> (8 + b)) & 1 == 1;
        let end_lo = (lend_plus_1 >> b) & 1 == 1;
        let end_hi = (lend_plus_1 >> (8 + b)) & 1 == 1;
        let mk = |c: bool, e: bool| -> Bdd {
            let cb = if c { vs.mk_true() } else { vs.mk_false() };
            let eb = if e { vs.mk_true() } else { vs.mk_false() };
            tag_in[0].and(&eb).or(&not_tag.and(&cb))
        };
        assert!(pc_lo_out[b] == mk(ltop_lo, end_lo), "GAP-A: PC-lo bit {b} != ite(tag0, lend+1, LTOP)");
        assert!(pc_hi_out[b] == mk(ltop_hi, end_hi), "GAP-A: PC-hi bit {b} != ite(tag0, lend+1, LTOP)");
    }

    // AUX-EXIT: ctrl=tag^1, temp=δu·[tag0==1], rb=0.
    let ctrl_out = out_field_bits(&out, layout, layout.reg_field(cf.ctrl));
    let temp_out = out_field_bits(&out, layout, layout.reg_field(cf.temp));
    let rb_out = out_field_bits(&out, layout, layout.reg_field(cf.rb));
    for b in 0..W {
        let want_ctrl = if b == 0 { not_tag.clone() } else { tag_in[b].clone() };
        assert!(ctrl_out[b] == want_ctrl, "COME-FROM AUX-EXIT: ctrl bit {b} != tag^1");
        let du_bit = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        assert!(temp_out[b] == tag_in[0].and(&du_bit), "COME-FROM AUX-EXIT: temp bit {b} != δu·[tag0==1]");
        assert!(rb_out[b] == zero, "COME-FROM AUX-EXIT: rb bit {b} != 0");
    }
    // s0/s1 + IR back to 0.
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
            assert!(got[b] == zero, "COME-FROM AUX-EXIT: field {f} bit {b} != 0 (scratch/IR leak)");
        }
    }

    // FIELD-COMPLETE HAND-OFF C (L2 leg): scratch s2..s7 IDENTITY. s2 is the
    // SPILLED CARRIER leaf -- it MUST pass through unchanged (the come-from must not
    // touch the loop-carried spilled leaf). s3..s7 identity (transient/untouched).
    for s in 2..NUM_SCRATCH {
        let f = layout.scratch_field(s);
        let got = out_field_bits(&out, layout, f);
        let inp = field_bits(&vs, nf, f);
        for b in 0..W {
            assert!(
                got[b] == inp[b],
                "COME-FROM: scratch s{s} bit {b} not identity (come-from disturbed the {} scratch)",
                if s == SPILL_SCRATCH { "SPILLED-CARRIER" } else { "transient" }
            );
        }
    }

    // δu correctness: δu = 256 − iter_len, sext+1 lands on Ltop.
    let iter_len = geom.iter_len;
    let delta_u = cf.delta_u as usize;
    assert_eq!(delta_u, 256 - iter_len, "δu must be 256 − iter_len");
    let w_pc = layout.w_pc;
    let mask: usize = if w_pc >= usize::BITS as usize { usize::MAX } else { (1usize << w_pc) - 1 };
    let sext = { let s = cf.delta_u as i8 as i64; (s & (mask as i64)) as usize };
    let landed = (geom.lend_pc.wrapping_add(sext).wrapping_add(1)) & mask;
    assert_eq!(landed, LTOP, "back-jump δu must land on Ltop");

    // Carrier + other data regs IDENTITY (come-from is a pure BR-router). The
    // register carrier leaves (tag/payload) must be untouched.
    for r in 0..layout.num_regs {
        if r == cf.ctrl || r == cf.temp || r == cf.rb {
            continue;
        }
        let got = out_field_bits(&out, layout, layout.reg_field(r));
        let inp = field_bits(&vs, nf, layout.reg_field(r));
        for b in 0..W {
            assert!(got[b] == inp[b], "COME-FROM: data reg r{r} bit {b} disturbed");
        }
    }

    (peak, secs)
}

#[test]
fn invariant_come_from_lemma_mde_spill() {
    let layout = Layout::default(); // w_pc=16
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    eprintln!(
        "come-from regs: ctrl=r{}, tag=r{}, temp=r{}, rb=r{}, δu={:#04x}, route_start={}",
        cf.ctrl, cf.tag, cf.temp, cf.rb, cf.delta_u, cf.route_start
    );
    let (peak, secs) = prove_come_from_lemma(&layout, &geom);
    eprintln!(
        "L2 COME-FROM LEMMA [mde_spill]: BR = δu·[tag==0] over the WHOLE tag domain; δu={:#04x} \
         lands on Ltop; carrier (incl. SPILLED Scratch(2)) undisturbed. Peak {} nodes, {:.3}s.",
        cf.delta_u, peak, secs
    );
    assert!(peak < 50_000, "COME-FROM LEMMA peak {peak} -- expected tractable");
}

// ══════════════════════════════════════════════════════════════════════════
// L0 -- RE-ENTRY LEMMA (the induction weld: L2-continue-exit → L1-entry).
//
// Glue [LTOP, BODY_START) = offsets [2,7): Ltop SWAPBR rc + 4 join-dispatch.
// After L2's CONTINUE exit (back-edge: BR=δu, ctrl=1, temp=0, rb=0, rc=0,
// tag0=0), the glue must re-establish L1's entry invariant: {ctrl=temp=rb=rc=BR=0},
// PC=BODY_START, carrier identity -- INCLUDING the SPILLED leaf in Scratch(2),
// which is loop-carried and must pass through as IDENTITY (NOT zeroed).
// ══════════════════════════════════════════════════════════════════════════

fn read_rc(geom: &LoopGeom) -> usize {
    match geom.lowered_instrs[LTOP] {
        Instr::Swapbr { rd, .. } => rd as usize,
        ref o => panic!("Ltop must be SWAPBR rc, got {o:?}"),
    }
}

/// L0 re-entry ENTRY SV: L2's continue-exit at PC=Ltop. ctrl=1, temp=rb=rc=0,
/// BR=δu, tag=0 (continue). Carrier payload + Scratch(2) SYMBOLIC (identity),
/// s0/s1=0, IR=0, other scratch/regs symbolic (identity).
fn re_entry_sv(layout: &Layout, geom: &LoopGeom, cf: &ComeFrom, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    let rc = read_rc(geom);
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((LTOP & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((LTOP >> 8) & 0xFF) as u8, vs)
        } else if f == layout.br_field() {
            const_word8(cf.delta_u, vs)
        } else if f == layout.reg_field(cf.ctrl) {
            const_word8(1, vs) // continue: ctrl = ¬tag = 1
        } else if f == layout.reg_field(cf.temp)
            || f == layout.reg_field(cf.rb)
            || f == layout.reg_field(rc)
        {
            const_word8(0, vs)
        } else if f == layout.reg_field(TAG_REG) {
            const_word8(0, vs) // continue path ⇒ tag0=0
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            const_word8(0, vs)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            const_word8(0, vs)
        } else {
            symbolic_word8(vs, nf, f) // payload, Scratch(2) carrier, other scratch/regs symbolic
        }
    })
}

/// L0-BASE entry SV: first-traversal precondition after the prologue
/// {BR=0, ctrl=0, temp=δu, tag=1}, rc=rb=0. Carrier payload + Scratch(2) symbolic.
fn re_entry_base_sv(layout: &Layout, geom: &LoopGeom, cf: &ComeFrom, vs: &BddVariableSet) -> SV {
    let nf = n_fields(layout);
    let rc = read_rc(geom);
    build_state(layout, |f| {
        if f == layout.pc_field() {
            const_word8((LTOP & 0xFF) as u8, vs)
        } else if layout.pc_fields() >= 2 && f == layout.pc_hi_field() {
            const_word8(((LTOP >> 8) & 0xFF) as u8, vs)
        } else if f == layout.br_field() {
            const_word8(0, vs)
        } else if f == layout.reg_field(cf.temp) {
            const_word8(cf.delta_u, vs) // prologue minted the token INTO temp
        } else if f == layout.reg_field(cf.ctrl)
            || f == layout.reg_field(cf.rb)
            || f == layout.reg_field(rc)
        {
            const_word8(0, vs)
        } else if f == layout.reg_field(TAG_REG) {
            const_word8(1, vs) // prologue set the Right tag
        } else if (0..4).any(|k| f == layout.ir_byte_field(k)) {
            const_word8(0, vs)
        } else if f == layout.scratch_field(0) || f == layout.scratch_field(1) {
            const_word8(0, vs)
        } else {
            symbolic_word8(vs, nf, f)
        }
    })
}

fn compose_re_entry_from(layout: &Layout, geom: &LoopGeom, vs: &BddVariableSet, entry: SV) -> (SV, usize) {
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

/// Shared L0 exit checks: aux all 0, PC=BODY_START, carrier identity (incl.
/// Scratch(2) spilled carrier), s0/s1=0, s2..s7 identity, IR=0. `tag_want` is
/// the expected tag byte (0 for the step leg, 1 for the base leg).
fn check_l0_exit(layout: &Layout, geom: &LoopGeom, cf: &ComeFrom, vs: &BddVariableSet, out: &SV, tag_want: u8) {
    let nf = n_fields(layout);
    let rc = read_rc(geom);
    let zero = vs.mk_false();

    for f in [
        layout.reg_field(cf.ctrl),
        layout.reg_field(cf.temp),
        layout.reg_field(cf.rb),
        layout.reg_field(rc),
        layout.br_field(),
    ] {
        let got = out_field_bits(out, layout, f);
        for b in 0..W {
            assert!(got[b] == zero, "RE-ENTRY: aux field {f} bit {b} != 0 (L1 entry invariant broken)");
        }
    }

    assert_eq!(read_concrete_pc(out, layout), Some(BODY_START), "RE-ENTRY: exit PC != BODY_START");

    // Register carrier + other data regs. tag = tag_want; payload identity.
    for r in 0..layout.num_regs {
        if r == cf.ctrl || r == cf.temp || r == cf.rb || r == rc {
            continue;
        }
        let got = out_field_bits(out, layout, layout.reg_field(r));
        if r == TAG_REG {
            for b in 0..W {
                let want = if (tag_want >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
                assert!(got[b] == want, "RE-ENTRY: tag reg bit {b} != {tag_want}");
            }
        } else {
            let inp = field_bits(vs, nf, layout.reg_field(r));
            for b in 0..W {
                assert!(got[b] == inp[b], "RE-ENTRY: data reg r{r} bit {b} disturbed by the glue");
            }
        }
    }

    // s0/s1 = 0.
    for f in [layout.scratch_field(0), layout.scratch_field(1)] {
        let got = out_field_bits(out, layout, f);
        for b in 0..W {
            assert!(got[b] == zero, "RE-ENTRY: scratch field {f} bit {b} != 0");
        }
    }
    // s2..s7 IDENTITY -- s2 is the SPILLED CARRIER leaf (must survive the glue).
    for s in 2..NUM_SCRATCH {
        let f = layout.scratch_field(s);
        let got = out_field_bits(out, layout, f);
        let inp = field_bits(vs, nf, f);
        for b in 0..W {
            assert!(
                got[b] == inp[b],
                "RE-ENTRY: scratch s{s} bit {b} not identity (glue disturbed the {} scratch)",
                if s == SPILL_SCRATCH { "SPILLED-CARRIER" } else { "transient" }
            );
        }
    }
    // IR = 0.
    for k in 0..4 {
        let f = layout.ir_byte_field(k);
        let got = out_field_bits(out, layout, f);
        for b in 0..W {
            assert!(got[b] == zero, "RE-ENTRY: IR byte {k} bit {b} != 0");
        }
    }
}

#[test]
fn invariant_re_entry_lemma_mde_spill() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let t0 = Instant::now();
    let (out, peak) = compose_re_entry_from(&layout, &geom, &vs, re_entry_sv(&layout, &geom, &cf, &vs));
    let secs = t0.elapsed().as_secs_f64();
    check_l0_exit(&layout, &geom, &cf, &vs, &out, 0);
    assert!(peak < 50_000, "RE-ENTRY LEMMA peak {peak} -- expected tractable");
    eprintln!(
        "L0 RE-ENTRY LEMMA [mde_spill]: glue [{LTOP},{BODY_START}) welds L2-continue-exit → L1-entry \
         {{ctrl=temp=rb=rc=BR=0}} over the WHOLE carrier; PC→BODY_START; carrier identity incl. \
         SPILLED Scratch(2). Peak {peak} nodes, {secs:.3}s."
    );
}

#[test]
fn invariant_re_entry_base_lemma_mde_spill() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let t0 = Instant::now();
    let (out, peak) = compose_re_entry_from(&layout, &geom, &vs, re_entry_base_sv(&layout, &geom, &cf, &vs));
    let secs = t0.elapsed().as_secs_f64();
    check_l0_exit(&layout, &geom, &cf, &vs, &out, 1);
    assert!(peak < 50_000, "RE-ENTRY BASE peak {peak} -- expected tractable");
    eprintln!(
        "L0-BASE RE-ENTRY LEMMA [mde_spill]: glue lands the FIRST-traversal precondition \
         {{BR=0,ctrl=0,temp=δu,tag=1}} in L1's entry invariant (aux 0, PC→BODY_START, carrier \
         identity incl. SPILLED Scratch(2), tag=1). Peak {peak} nodes, {secs:.3}s."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// L3 -- TRIP-COUNT BOUND + INDUCTION ANCHORS.
// ══════════════════════════════════════════════════════════════════════════

/// Eval-side trajectory length: body applications from Right(input_bit) until the
/// carrier returns to a Right (done) state. Mirrors mde_spill.rs::trajectory_len.
fn trajectory_len(body: &Iso, carrier: &Type, input_bit: u8) -> usize {
    let Iso::Prim(def) = body else { panic!() };
    let vals = enumerate_values(carrier);
    let mut cur = vals
        .iter()
        .find(|v| {
            let b = marshal(v, carrier).regs;
            b[0] == 1 && b[1] == input_bit
        })
        .expect("Right(input) canonical value")
        .clone();
    for step in 1..=1000 {
        cur = def.forward.get(&cur).expect("total table").clone();
        let b = marshal(&cur, carrier).regs;
        if b[0] == 1 {
            return step;
        }
    }
    panic!("trajectory did not terminate");
}

/// WHOLE-DOMAIN TRIP-COUNT BOUND: max over the Bit input domain of the loop's
/// come-from iteration count (= `trajectory_len`, the number of body applications
/// until the carrier returns to a done/Right state) ≤ k_max, so every input
/// reaches a done state within the WCET budget B.
///
/// NOTE (differs from vart_halve's `trip_count+1 ≤ k_max`): mde_spill's `k_max`
/// is CLAMPED by `min(k_exact, |u|+1)` where |u| = N5 inhabitants = 5, so
/// k_max = min(7, 6) = 6. The come-from loop performs exactly `trajectory_len`
/// iterations (each iteration = one body pass + one come-from decision); input 0
/// takes 6 body passes. The WCET budget B = |instrs| + (k_max−1)·iter_len =
/// 29 + 5·25 = 154 covers exactly k_max = 6 come-from iterations. So the sound
/// whole-domain bound is `trajectory_len ≤ k_max` (NOT +1): the +1 in vart_halve
/// came from its `trip_count` counting one fewer than the come-from iteration
/// count (it checked the post-step tag). Here `trajectory_len` IS the come-from
/// iteration count, so it must be ≤ k_max directly. This is cross-checked below
/// against the budget's own iteration capacity ⌊(B − |instrs|)/iter_len⌋ + 1.
#[test]
fn invariant_trip_count_whole_domain_bound_mde_spill() {
    let (body, u_ty, carrier) = build_walk_body();
    let iso = Iso::trace(u_ty.clone(), body.clone());
    let geom = compile_loop_geom(&iso);
    let k_max = geom.k_max;

    // The budget's own iteration capacity: the straight-through pass plus
    // ⌊(B − |instrs|)/iter_len⌋ back-edges. This is the number of come-from
    // iterations B actually funds -- it MUST be ≥ every input's trajectory_len.
    let n_instrs = geom.lowered_instrs.len();
    let budget_iters = (geom.budget - n_instrs) / geom.iter_len + 1;
    assert_eq!(
        budget_iters, k_max,
        "budget iteration-capacity {budget_iters} != k_max {k_max} (WCET model sanity)"
    );

    let mut worst = (0usize, 0u8);
    for input_bit in [0u8, 1u8] {
        let tc = trajectory_len(&body, &carrier, input_bit);
        if tc > worst.0 {
            worst = (tc, input_bit);
        }
        assert!(
            tc <= k_max,
            "TRIP-COUNT BOUND: input {input_bit} trajectory_len = {tc} > k_max {k_max} -- \
             an input runs past the WCET budget (unsound induction / compiler k_max too small)"
        );
        // Directly: the budget must fund this input's come-from iterations.
        assert!(
            tc <= budget_iters,
            "TRIP-COUNT BOUND: input {input_bit} trajectory_len = {tc} > budget iteration-capacity \
             {budget_iters} -- B={} does not cover this input",
            geom.budget
        );
    }
    eprintln!(
        "TRIP-COUNT BOUND [mde_spill]: max over the Bit input domain of trajectory_len = {} \
         (at input {}) ≤ k_max {} = budget iteration-capacity (B={} covers every input).",
        worst.0, worst.1, k_max, geom.budget
    );
}

/// L3 (fast anchor, ALWAYS-ON): run_on_cpu_fast == eval over the WHOLE Bit input
/// domain -- ties the structural lemmas to the real machine in seconds.
#[test]
fn invariant_induction_anchor_fast_mde_spill() {
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let bit = Type::sum(Type::One, Type::One);
    for v in enumerate_values(&bit) {
        let cpu = run_on_cpu_fast(&iso, &v).expect("fast run");
        let mut fuel = 10_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "ANCHOR(fast): run_on_cpu_fast != eval on {v:?}");
        eprintln!("  fast-anchor {v:?}: run_on_cpu_fast == eval");
    }
    eprintln!(
        "L3 INDUCTION ANCHOR (fast) [mde_spill]: run_on_cpu_fast == eval over the WHOLE Bit input \
         domain. With L1 (body ≡ mde_walk incl. spilled leaf) + L2 (come-from loops iff ¬done) + \
         L0 (weld carries the spilled Scratch(2) leaf across the back edge) this establishes \
         run_on_cpu == eval over the whole input domain by induction on the trip count."
    );
}

/// L3 (heavy anchor, ADVISOR-RUN #[ignore]): the REAL run_rom_on_cpu == eval +
/// terminal QUIESCENCE (scratch all-zero, BR=0) -- EXACTLY the mde_spill_real_cpu
/// authority's per-input path. This is the ground truth the invariant method
/// REPLACES; kept #[ignore] because run_rom_on_cpu at budget 154 is slow. The
/// scratch-all-zero terminal check confirms the SPILLED leaf's page-out bracket
/// leaves Scratch(2) clean at loop exit (the paging-leak check the authority ran).
#[test]
#[ignore = "heavy real-CPU authority (slow); advisor runs to re-anchor after netlist changes"]
fn invariant_induction_bootstrap_quiescence_mde_spill() {
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let lowered = compile_with_metadata(&iso).expect("compile");
    let budget = lowered.wcet();
    let s_max_val = lowered.s_max();
    let (in_ty, out_ty) = theseus_core::type_of(&iso).unwrap();
    let bit = Type::sum(Type::One, Type::One);

    for v in enumerate_values(&bit) {
        let state = marshal(&v, &in_ty);
        let final_state =
            run_rom_on_cpu(&lowered.instrs, state, budget, s_max_val, theseus_eval::eval)
                .expect("real-cpu run");
        let cpu_out = unmarshal(&final_state, &out_ty);
        let mut fuel = 1_000_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu_out, ev, "BOOTSTRAP: real CPU != eval on {v:?}");
        assert_eq!(
            final_state.scratch, [0u8; NUM_SCRATCH],
            "BOOTSTRAP: scratch must be all-zero at exit (spill page-out leak check) on {v:?}"
        );
        assert_eq!(final_state.br, 0, "BOOTSTRAP: BR must be 0 at loop exit on {v:?}");
        eprintln!("  bootstrap {v:?}: run_rom_on_cpu == eval, scratch clean, BR=0");
    }
    eprintln!(
        "L3 INDUCTION ANCHOR [mde_spill]: real-CPU bootstrap == eval + terminal quiescence over \
         the whole Bit input domain. This is the mde_spill_real_cpu authority the invariant \
         method replaces. B={budget}."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// NEGATIVE CONTROLS -- teeth.
// ══════════════════════════════════════════════════════════════════════════

/// NEG-1 (body): the real composed body must EQUAL mde_walk_spec AND DIFFER from
/// a wrong spec (drop the final b[1]^=b[2] mixing step). Teeth on L1's body check.
#[test]
fn invariant_negative_control_body_mde_spill() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let (out, _s) = compose_body(&layout, &geom.rom, &vs, geom.body_start, geom.body_len);

    let leaves_in: [Vec<Bdd>; 5] = std::array::from_fn(|leaf| entry_leaf_bits(&vs, nf, &layout, leaf));
    let want = mde_walk_spec(&leaves_in, &vs);

    // TRUE spec matches (leaf 1 = Reg(0)).
    let out_l1 = out_field_bits(&out, &layout, carrier_field(&layout, 1));
    for b in 0..W {
        assert!(out_l1[b] == want[1][b], "sanity: real body must match mde_walk_spec on leaf 1");
    }

    // WRONG spec: mde_walk WITHOUT the final b[1]^=b[2] mix -- leaf 1 differs.
    let mut wb: Vec<Vec<Bdd>> = leaves_in.to_vec();
    wb[0] = xor_bytes(&wb[0], &wb[1]);
    wb[0] = xor_const1(&wb[0], &vs);
    wb[1] = xor_const1(&wb[1], &vs);
    wb[2] = xor_bytes(&wb[2], &wb[3]);
    wb[3] = xor_bytes(&wb[3], &wb[4]);
    wb[0] = xor_const1(&wb[0], &vs);
    let p = wb[0][0].clone();
    let (n1, n2) = cond_swap(&p, &wb[1], &wb[2]); wb[1] = n1; wb[2] = n2;
    let (n2b, n3) = cond_swap(&p, &wb[2], &wb[3]); wb[2] = n2b; wb[3] = n3;
    let (n3b, n4) = cond_swap(&p, &wb[3], &wb[4]); wb[3] = n3b; wb[4] = n4;
    wb[0] = xor_const1(&wb[0], &vs);
    wb[3] = xor_bytes(&wb[3], &wb[4]);
    wb[2] = xor_bytes(&wb[2], &wb[3]);
    // (omit the final b[1]^=b[2])
    let differs = (0..W).any(|b| out_l1[b] != wb[1][b]);
    assert!(differs, "NEG-1 body: real body must DIFFER from the mixing-dropped spec (teeth)");
    eprintln!("NEG-1 body [mde_spill]: real body ≡ mde_walk, ≠ mixing-dropped spec -- teeth.");
}

/// NEG-SPILL (spill-in-carrier is load-bearing): if the spilled Scratch(2) leaf
/// were treated as TRANSIENT (asserted 0 at body exit like vart_halve's scratch),
/// the proof would be WRONG -- the body leaves a NON-ZERO transformed leaf in
/// Scratch(2). This control asserts the real body output for Scratch(2) is NOT
/// identically 0 (it carries the transformed spilled leaf), so the carrier
/// treatment (asserting = mde_walk_spec[4], not 0) is genuinely required.
#[test]
fn invariant_negative_control_spill_is_carrier() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let (out, _s) = compose_body(&layout, &geom.rom, &vs, geom.body_start, geom.body_len);

    let f_spill = carrier_field(&layout, SPILL_LEAF); // Scratch(2)
    let out_spill = out_field_bits(&out, &layout, f_spill);
    let zero = vs.mk_false();

    // The spilled-leaf output must NOT be identically 0 (else "transient-0" would
    // have passed and the spill-carrier treatment would be vacuous).
    let all_zero = (0..W).all(|b| out_spill[b] == zero);
    assert!(
        !all_zero,
        "NEG-SPILL: body output for Scratch(2) is identically 0 -- the spill would be transient, \
         not carrier. The spill-in-carrier treatment would be vacuous."
    );

    // And it DOES equal mde_walk_spec's leaf-4 output (the carrier claim).
    let leaves_in: [Vec<Bdd>; 5] = std::array::from_fn(|leaf| entry_leaf_bits(&vs, nf, &layout, leaf));
    let want = mde_walk_spec(&leaves_in, &vs);
    for b in 0..W {
        assert!(out_spill[b] == want[SPILL_LEAF][b], "sanity: spilled leaf ≡ mde_walk_spec[4]");
    }
    eprintln!(
        "NEG-SPILL [mde_spill]: Scratch(2) at body exit ≡ mde_walk_spec[leaf 4] and is NOT \
         identically 0 -- the spilled leaf is genuine loop-carried CARRIER, not transient. \
         Treating it as transient-0 would FAIL. The spill-in-carrier treatment is load-bearing."
    );
}

/// NEG-3 (mutated ROM): flip one body instr ⇒ L1 body ≠ mde_walk_spec.
#[test]
fn invariant_negative_control_mutated_rom_mde_spill() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);

    // Mutate: overwrite the first NON-paging body instr (body_start+1 = Xor r1,r0)
    // with a no-op-ish wrong instr (Xori on a carrier reg).
    let mut mutated = geom.lowered_instrs.clone();
    let mutate_at = geom.body_start + 1;
    mutated[mutate_at] = Instr::Xori { rd: 0, imm: 1 };
    let mutated_rom = assemble(&mutated).expect("assemble mutated ROM");
    let n_diff = geom.rom.iter().zip(mutated_rom.iter()).filter(|(a, b)| a != b).count();
    assert_eq!(n_diff, 1, "mutation must flip exactly one ROM word");

    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let (out, _s) = compose_body(&layout, &mutated_rom, &vs, geom.body_start, geom.body_len);

    let leaves_in: [Vec<Bdd>; 5] = std::array::from_fn(|leaf| entry_leaf_bits(&vs, nf, &layout, leaf));
    let want = mde_walk_spec(&leaves_in, &vs);
    let matches_all = (0..5).all(|leaf| {
        let got = out_field_bits(&out, &layout, carrier_field(&layout, leaf));
        (0..W).all(|b| got[b] == want[leaf][b])
    });
    assert!(
        !matches_all,
        "NEG-3 mutated ROM: a body with one flipped instr STILL matched mde_walk -- L1 has no teeth"
    );
    eprintln!("NEG-3 mutated ROM [mde_spill]: one flipped body instr ⇒ body ≠ mde_walk -- L1 has teeth.");
}

/// NEG-2 (come-from): real BR ≠ inverted-predicate and ≠ unconditional-loop specs.
#[test]
fn invariant_negative_control_come_from_mde_spill() {
    let layout = Layout::default();
    let (body, u_ty, _carrier) = build_walk_body();
    let iso = Iso::trace(u_ty, body);
    let geom = compile_loop_geom(&iso);
    let cf = read_come_from(&geom);
    let vs = BddVariableSet::new_anonymous(n_vars(&layout));
    let nf = n_fields(&layout);
    let (out, _peak) = compose_come_from(&layout, &geom, &cf, &vs);
    let tag_in = field_bits(&vs, nf, layout.reg_field(cf.tag));
    let br_out = out_field_bits(&out, &layout, layout.br_field());
    let not_tag = tag_in[0].not();

    let wrong_a: Vec<Bdd> = (0..W)
        .map(|b| {
            let du = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
            tag_in[0].and(&du)
        })
        .collect();
    assert!((0..W).any(|b| br_out[b] != wrong_a[b]), "NEG-2: BR must DIFFER from inverted-predicate (teeth)");

    let wrong_b: Vec<Bdd> = (0..W)
        .map(|b| if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() })
        .collect();
    assert!((0..W).any(|b| br_out[b] != wrong_b[b]), "NEG-2: BR must DIFFER from unconditional-loop (teeth)");

    for b in 0..W {
        let du = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        assert!(br_out[b] == not_tag.and(&du), "sanity: true δu·[tag==0] must match");
    }
    eprintln!("NEG-2 come-from [mde_spill]: real BR ≡ δu·[tag==0], ≠ {{inverted, unconditional}} -- teeth.");
}

/// NEG-4 (L0 re-entry, template parity): a BROKEN L0 exit expectation must DIFFER
/// from the real glue output -- so the re-entry lemma's welding asserts have teeth
/// (a glue that landed in the WRONG state would be caught). Two wrong
/// expectations: (a) wrong exit PC (the glue does NOT fall through to BODY_START),
/// and (b) wrong aux (ctrl != 0 at exit ⇒ L1's entry invariant violated). Reuses
/// the tractable w_pc=8 continue-leg composition.
#[test]
fn invariant_negative_control_re_entry_mde_spill() {
    let layout = Layout { w_pc: 8, ..Layout::default() };
    let (body, u_ty, _carrier) = build_walk_body();
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
        "NEG-4 L0: real glue exit PC matched a WRONG landing (BODY_START+1) -- no teeth"
    );
    assert_eq!(got_pc, Some(BODY_START), "sanity: real glue lands on BODY_START");

    // (b) WRONG aux: the real ctrl at exit must NOT be 1 (it must be 0 = L1's
    // entry invariant). A glue that left ctrl=1 would break re-entry silently.
    let ctrl_out = out_field_bits(&out, &layout, layout.reg_field(cf.ctrl));
    let wrong_ctrl_is_one = (0..W).all(|b| ctrl_out[b] == if b == 0 { one.clone() } else { zero.clone() });
    assert!(
        !wrong_ctrl_is_one,
        "NEG-4 L0: real glue exit ctrl matched the WRONG value 1 -- L0 welding has no teeth"
    );
    for b in 0..W {
        assert!(ctrl_out[b] == zero, "sanity: real glue exit ctrl == 0 (L1 entry invariant)");
    }
    eprintln!("NEG-4 L0 re-entry [mde_spill]: real glue lands ctrl=0 @BODY_START, ≠ {{wrong PC, ctrl=1}} -- teeth.");
}

/// NEG-5 (L2 aux-exit, template parity): a BROKEN come-from AUX-EXIT expectation
/// must DIFFER from the real bracket output -- so the per-branch aux pin (the
/// PRECONDITION L0 starts from) has teeth. Two wrong expectations:
///   (a) ctrl = tag (NOT tag^1): the real route sets ctrl = ¬tag on bit 0.
///   (b) temp = 0 unconditionally: the real exit keeps temp = δu on the EXIT
///       branch (tag0=1). A wrong pin here would corrupt L0's entry precondition.
#[test]
fn invariant_negative_control_aux_exit_mde_spill() {
    let layout = Layout::default(); // w_pc=16 (matches L2's real run)
    let (body, u_ty, _carrier) = build_walk_body();
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
    let differs_ctrl = (0..W).any(|b| ctrl_out[b] != tag_in[b]);
    assert!(
        differs_ctrl,
        "NEG-5 aux-exit: real ctrl must DIFFER from the WRONG 'ctrl=tag' spec -- no teeth"
    );

    // (b) WRONG temp spec: temp == 0 unconditionally (ignores the exit-branch δu).
    let differs_temp = (0..W).any(|b| temp_out[b] != zero);
    assert!(
        differs_temp,
        "NEG-5 aux-exit: real temp must DIFFER from the WRONG 'temp=0 always' spec -- no teeth"
    );

    // sanity: the TRUE specs DO match (ties teeth to the right targets).
    for b in 0..W {
        let want_ctrl = if b == 0 { not_tag.clone() } else { tag_in[b].clone() };
        assert!(ctrl_out[b] == want_ctrl, "sanity: true ctrl = tag^1 must match");
        let du_bit = if (cf.delta_u >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        assert!(temp_out[b] == tag_in[0].and(&du_bit), "sanity: true temp = δu·[tag0==1] must match");
    }
    eprintln!("NEG-5 aux-exit [mde_spill]: real ctrl≡tag^1, temp≡δu·[tag0==1], ≠ {{ctrl=tag, temp=0}} -- teeth.");
}
