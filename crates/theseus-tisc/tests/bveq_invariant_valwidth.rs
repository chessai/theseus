//! BVEQ-INVARIANT-VALWIDTH — extend the inductive-invariant method from the
//! Trace COME-FROM loop (vart_halve, bveq_invariant_poc.rs) to the DIFFERENT
//! loop shape used by valwidth REDUCEs: the `Iter` FORWARD-SKIP-BLOCK loop.
//!
//! ── The structural difference (from the invariant re-review) ────────────────
//! vart_halve compiles a `Trace` to a DATA-DEPENDENT BACKWARD JUMP come-from:
//! ONE physical body copy, re-entered via a Swapbr δu back-edge until `done`.
//! The proof needs INDUCTION over a data-dependent trip count (its L2 come-from
//! lemma + L0 re-entry weld).
//!
//! A valwidth reduce compiles `iter Nat_d body` via GUARDED STATIC UNROLL
//! (compile.rs `lower_data_dependent_iter`): D-1 PHYSICAL copies of the body,
//! each wrapped in a FORWARD SKIP-BLOCK (`emit_skip_block`) guarded on the i-th
//! unary counter tag register. There is NO back-jump. The skip-block is:
//!     Addi rb, body_len       ; rb = L  (skip distance)
//!     Cswap ctrl, rb, temp    ; ctrl=1 (run): rb=0,temp=L · ctrl=0 (skip): rb=L
//!     Swapbr rb               ; FORWARD skip: rb=L jumps over body, rb=0 → PC+1
//!     <body: L instrs>
//!     Swapbr rb               ; come-from (clears BR / recaptures on skip)
//!     Cswap ctrl, rb, temp    ; cleanup
//!     Subi rb, body_len       ; cleanup
//!
//! CONSEQUENCE for the proof shape (the headline finding):
//!   * The trip count is STATIC: there are ALWAYS exactly D-1 skip-blocks in
//!     ROM. The DATA (counter tags) only decides which copies RUN their body vs
//!     SKIP it. So the induction over a data-dependent count DISAPPEARS —
//!     replaced by COMPOSITION of D-1 fixed per-copy relations. `loop_terms`
//!     is EMPTY for an Iter (only Trace/Fold contribute), and wcet() == the raw
//!     instruction count — fully static.
//!   * The NEW lemma the forward-skip loop needs (the Iter analog of vart's L2
//!     come-from) is the SKIP-BLOCK LEMMA: over the whole carrier domain AND
//!     both control values of ctrl, one skip-block copy realizes
//!         ctrl_bit0 == 1  ⇒  carrier := body(carrier)   (RUN)
//!         ctrl_bit0 == 0  ⇒  carrier := carrier         (SKIP, identity)
//!     with all skip aux {rb,temp} restored to 0. Unlike vart's L2 (a single
//!     back-jump BR-router), this lemma spans the WHOLE copy AND must handle a
//!     DATA-DEPENDENT FORWARD PC JUMP inside it (the Swapbr over the body) — so
//!     concrete-PC pinning across the whole copy is NOT valid; PC branches on
//!     ctrl at the first Swapbr. That is the machinery the come-from proof did
//!     not need.
//!
//! ── The MEMORY obstacle (reported honestly) ────────────────────────────────
//! The reduce BODY uses `MexchReg` (resident-array indexed read), which operates
//! on `(CpuState, MemState)` and PANICS in `decode_execute_l` — it needs the
//! full `system_step` memory ring (CPU+Mem+Bus). vart_halve's StepEngine walks
//! only the CPU-only `decode_execute_l`. So the reduce BODY-lemma (L1 analog)
//! cannot reuse vart's CPU-only StepEngine as-is; it requires a system-ring
//! symbolic walk. This file MEASURES whether that is tractable and, crucially,
//! proves the NOVEL part — the CPU-only skip-block/counter machinery — which is
//! what actually distinguishes the Iter shape from the Trace shape.
//!
//! This is a RESEARCH file: it reports GO/NO-GO honestly and does not fake
//! soundness. See the module-tail report block.

#![cfg(feature = "heavy-tests")]

use std::collections::BTreeMap;
use std::time::Instant;

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::{assemble, Instr};
use theseus_tisc::bveq_netlist::{eval_bdd, extract_field, extract_word8_bdds, WalkCtx, SV};
use theseus_tisc::compile::{
    compile_with_metadata_resident, marshal_resident, memexch_prim_idx, run_rom_fast,
    unmarshal_resident, value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};
use theseus_tisc::step::system_step;
use theseus_tisc::types::{
    MEM_WORDS, NUM_BUS_FIELDS, NUM_CPU_FIELDS, NUM_REGS, NUM_SCRATCH, W,
};

// ══════════════════════════════════════════════════════════════════════════
// Target program: valwidth_reduce reduce_flat(w, d) = iter(Nat_d, xor_body).
// (Copied from tests/valwidth_reduce.rs so this file is self-contained.)
// ══════════════════════════════════════════════════════════════════════════

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}
fn idx_arr_ty(w: u8) -> Type {
    let flat = Type::Flat(w);
    Type::prod(bit(), Type::prod(flat.clone(), Type::prod(flat, Type::One)))
}
fn arr_ty(w: u8) -> Type {
    let flat = Type::Flat(w);
    Type::prod(flat.clone(), Type::prod(flat, Type::One))
}
fn body_ty(w: u8) -> Type {
    let flat = Type::Flat(w);
    let b = bit();
    Type::prod(
        flat.clone(),
        Type::prod(
            flat.clone(),
            Type::prod(b, Type::prod(flat.clone(), Type::prod(flat, Type::One))),
        ),
    )
}
fn carrier_ty(w: u8, d: usize) -> Type {
    Type::prod(Type::numeral_ty(d), body_ty(w))
}

fn memexch_iso(w: u8) -> Iso {
    let flat = Type::Flat(w);
    let b = bit();
    let def = memexch_prim_idx(&flat, &b, 2).expect("memexch_prim_idx(Flat(w), Bit, 2)");
    Iso::Prim(Box::new(def))
}
fn xor_prim_def(w: u8) -> PrimDef {
    let flat = Type::Flat(w);
    let ty = Type::prod(flat.clone(), flat.clone());
    let vals = enumerate_values(&ty);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let a = bytes[0];
        let b = bytes[1];
        let out_b = a ^ b;
        let (out_val, _) = value_from_leaf_bytes_pub(&vec![a, out_b], &ty);
        forward.insert(v.clone(), out_val.clone());
        backward.insert(v.clone(), out_val);
    }
    PrimDef {
        forward_name: "xorInto".into(),
        backward_name: "xorInto".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}
fn xor_prim_iso(w: u8) -> Iso {
    Iso::Prim(Box::new(xor_prim_def(w)))
}
fn reduce_step(w: u8) -> Iso {
    let flat = Type::Flat(w);
    let swap = Iso::SwapProd(flat.clone(), flat.clone());
    Iso::seq(Iso::seq(swap.clone(), xor_prim_iso(w)), swap)
}
fn succ_idx() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}
fn xor_reduce_body(w: u8) -> Iso {
    let flat = Type::Flat(w);
    let mexch = memexch_iso(w);
    let id_acc_mexch = Iso::times(Iso::Id(flat.clone()), mexch.clone());
    let assoc_l = Iso::AssocLProd(flat.clone(), flat.clone(), idx_arr_ty(w));
    let rs_times_id = Iso::times(reduce_step(w), Iso::Id(idx_arr_ty(w)));
    let assoc_r = Iso::AssocRProd(flat.clone(), flat.clone(), idx_arr_ty(w));
    let succ_part = Iso::times(
        Iso::Id(flat.clone()),
        Iso::times(
            Iso::Id(flat.clone()),
            Iso::times(succ_idx(), Iso::Id(arr_ty(w))),
        ),
    );
    Iso::seq(
        Iso::seq(
            Iso::seq(
                Iso::seq(Iso::seq(id_acc_mexch.clone(), assoc_l), rs_times_id),
                assoc_r,
            ),
            id_acc_mexch,
        ),
        succ_part,
    )
}
fn reduce_flat(w: u8, d: usize) -> Iso {
    Iso::iter(Type::numeral_ty(d), xor_reduce_body(w))
}

// ══════════════════════════════════════════════════════════════════════════
// EXPLORATION — dump the ROM geometry of the Iter skip-block loop.
// ══════════════════════════════════════════════════════════════════════════

#[test]
fn explore_iter_rom_geometry() {
    for (w, d) in [(2u8, 2usize), (2, 3), (4, 3)] {
        let iso = reduce_flat(w, d);
        let lowered = compile_with_metadata_resident(&iso, 2)
            .unwrap_or_else(|e| panic!("compile reduce_flat({w},{d}): {e:?}"));
        let rom = assemble(&lowered.instrs).expect("assemble");
        eprintln!(
            "\n=== reduce_flat(w={w}, d={d}): {} instrs, wcet={}, loop_terms={}, mem/resident ===",
            lowered.instrs.len(),
            lowered.wcet(),
            lowered.loop_terms.len(),
        );
        for (i, instr) in lowered.instrs.iter().enumerate() {
            eprintln!("  [{i:3}] {instr:?}");
        }
        let n_swapbr = lowered
            .instrs
            .iter()
            .filter(|i| matches!(i, Instr::Swapbr { .. }))
            .count();
        eprintln!(
            "  → {} Swapbr (2 per skip-block copy, D-1={} copies)",
            n_swapbr,
            d - 1
        );
        let _ = rom;
    }
}

// ══════════════════════════════════════════════════════════════════════════
// SYSTEM-STATE symbolic machinery (the NOVEL part vs the vart CPU-only POC).
//
// The reduce body uses MexchReg, which PANICS in the CPU-only decode_execute_l.
// So we walk the MONOLITHIC `system_step(rom)` Iso — an Iso over
//   ((CpuState, MemState), BusMsg)  = ((23 cpu fields, 4 mem fields), 3 bus)
// = 30 Word8 fields, structured Pair(Pair(cpu, mem), bus) — via `eval_bdd`, with
// NO fetch short-circuit (system_step is a single Iso, not a phase list).
// (NUM_CPU_FIELDS = 8 regs + PC_lo + BR + 4 IR + 8 scratch + PC_hi = 23 at the
// default layout w_pc=16/w_br=8; the types.rs "=24" docstring is stale.)
//
// system_step takes NO layout (fixed default: w_pc=16, w_br=8, 8 regs, 8 scratch,
// 4 mem, 3 bus). We use a bit-interleaved varmap over the 30 SYSTEM fields:
//   var(field f, bit b) = b*NSYS + f.
// ══════════════════════════════════════════════════════════════════════════

const NSYS: usize = NUM_CPU_FIELDS + MEM_WORDS + NUM_BUS_FIELDS; // 23+4+3 = 30

// Fixed default-layout field indices (system_step is default-layout).
const PC_LO: usize = NUM_REGS; // 8
const BR_F: usize = NUM_REGS + 1; // 9
fn ir_byte(k: usize) -> usize {
    NUM_REGS + 2 + k // 10..14
}
fn scratch_f(i: usize) -> usize {
    NUM_REGS + 6 + i // 14..22
}
const PC_HI: usize = NUM_REGS + 6 + NUM_SCRATCH; // 22
fn mem_f(i: usize) -> usize {
    NUM_CPU_FIELDS + i // 24..28
}
fn bus_f(i: usize) -> usize {
    NUM_CPU_FIELDS + MEM_WORDS + i // 28..31
}

fn n_vars_sys() -> u16 {
    (NSYS * W) as u16
}
fn field_bit_var(vs: &BddVariableSet, f: usize, b: usize) -> Bdd {
    vs.mk_var(vs.variables()[b * NSYS + f])
}
fn const_word8(val: u8, vs: &BddVariableSet) -> SV {
    let mut sv = SV::Bit(if (val >> (W - 1)) & 1 == 1 { vs.mk_true() } else { vs.mk_false() });
    for b in (0..W - 1).rev() {
        let bit = if (val >> b) & 1 == 1 { vs.mk_true() } else { vs.mk_false() };
        sv = SV::pair(SV::Bit(bit), sv);
    }
    sv
}
/// A field whose low `wbits` bits are symbolic (its varmap vars), high bits 0.
fn sym_low_word8(vs: &BddVariableSet, f: usize, wbits: usize) -> SV {
    let mut sv = SV::Bit(vs.mk_false());
    for b in (0..W - 1).rev() {
        let bit = if b < wbits { field_bit_var(vs, f, b) } else { vs.mk_false() };
        sv = SV::pair(SV::Bit(bit), sv);
    }
    sv
}
/// The symbolic entry bits for a field (low `wbits` symbolic, high 0) — the
/// reference the specs are written against.
fn entry_bits(vs: &BddVariableSet, f: usize, wbits: usize) -> Vec<Bdd> {
    (0..W)
        .map(|b| if b < wbits { field_bit_var(vs, f, b) } else { vs.mk_false() })
        .collect()
}

/// Build a system-state SV `Pair(Pair(cpu_24, mem_4), bus_3)` field-by-field via
/// a per-field closure keyed on the GLOBAL field index 0..NSYS.
fn build_system<F: Fn(usize) -> SV>(mk: F) -> SV {
    let right_nested = |lo: usize, hi: usize| -> SV {
        let mut sv = mk(hi - 1);
        for f in (lo..hi - 1).rev() {
            sv = SV::pair(mk(f), sv);
        }
        sv
    };
    let cpu = right_nested(0, NUM_CPU_FIELDS);
    let mem = right_nested(NUM_CPU_FIELDS, NUM_CPU_FIELDS + MEM_WORDS);
    let bus = right_nested(NUM_CPU_FIELDS + MEM_WORDS, NSYS);
    SV::pair(SV::pair(cpu, mem), bus)
}

/// Extract the bit BDDs of global system field `f` from an output SV shaped
/// `Pair(Pair(cpu,mem),bus)`.
fn sys_field_bits(out: &SV, f: usize) -> Vec<Bdd> {
    let SV::Pair(cpu_mem, bus) = out else { panic!("system SV: expected outer Pair") };
    let SV::Pair(cpu, mem) = &**cpu_mem else { panic!("system SV: expected (cpu,mem) Pair") };
    let inner = if f < NUM_CPU_FIELDS {
        extract_field(cpu, f, NUM_CPU_FIELDS)
    } else if f < NUM_CPU_FIELDS + MEM_WORDS {
        extract_field(mem, f - NUM_CPU_FIELDS, MEM_WORDS)
    } else {
        extract_field(bus, f - NUM_CPU_FIELDS - MEM_WORDS, NUM_BUS_FIELDS)
    };
    extract_word8_bdds(inner).to_vec()
}

/// Read a system field as a CONCRETE integer (all bits const), else None.
fn read_concrete_sys_field(out: &SV, f: usize) -> Option<u32> {
    let bits = sys_field_bits(out, f);
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
/// Read the concrete PC (lo + hi byte) from a system output SV.
fn read_concrete_pc(out: &SV) -> Option<usize> {
    let lo = read_concrete_sys_field(out, PC_LO)? as usize;
    let hi = read_concrete_sys_field(out, PC_HI)? as usize;
    Some((hi << 8) | lo)
}

/// Peak per-bit + sum node counts over the whole system SV.
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

/// A word8 SV rebuilt from a Vec<Bdd> (LSB-first, right-nested).
fn word8_from_bits(bits: &[Bdd]) -> SV {
    let mut sv = SV::Bit(bits[W - 1].clone());
    for b in (0..W - 1).rev() {
        sv = SV::pair(SV::Bit(bits[b].clone()), sv);
    }
    sv
}

// ── The SystemStepEngine: walk `system_step(rom)` at CONCRETE PC each step ──
//
// Unlike vart's memoized StepEngine, there is NO fetch short-circuit: system_step
// is one monolithic Iso, so we call `eval_bdd(&system_step, sv, vs, ctx)` per step
// (the fetch mux is walked in full). We build the Iso ONCE and reuse it. PC is
// re-pinned concrete every step (valid whenever the step's own PC output is a
// compile-time constant — asserted by the PC-agreement check at the call site).
struct SystemStepEngine {
    step: Iso,
}
impl SystemStepEngine {
    fn new(rom: &[u32]) -> Self {
        SystemStepEngine { step: system_step(rom) }
    }
    fn eval_step(
        &self,
        cur: SV,
        vs: &BddVariableSet,
        ctx: &mut WalkCtx,
    ) -> Result<SV, theseus_tisc::bveq_netlist::NetlistWalkError> {
        eval_bdd(&self.step, cur, vs, ctx)
    }
}

/// Re-pin PC to `next_pc` (concrete) and carry every other field forward from
/// `out`. Returns the new entry SV for the following step.
fn repin_pc(out: &SV, next_pc: usize, vs: &BddVariableSet) -> SV {
    build_system(|f| {
        if f == PC_LO {
            const_word8((next_pc & 0xFF) as u8, vs)
        } else if f == PC_HI {
            const_word8(((next_pc >> 8) & 0xFF) as u8, vs)
        } else {
            word8_from_bits(&sys_field_bits(out, f))
        }
    })
}

// ══════════════════════════════════════════════════════════════════════════
// The reduce carrier register/mem layout (VERIFIED empirically, not derived).
//
// From the ROM dumps + marshal_resident(resident=2):
//   d=2 (12 instrs): ctrl=r0 (tag0), tag1=r1, acc=r2, data=r3, idx=r4,
//                    arr0=mem[0], arr1=mem[1]; aux rb=r5, temp=r6.
//   d=3 (24 instrs): ctrl copy0=r0(tag0), copy1=r1(tag1), tag2=r2, acc=r3,
//                    data=r4, idx=r5, arr0=mem[0], arr1=mem[1]; aux rb=r6, temp=r7.
// `diag_reduce_layout` prints marshal_resident's actual homes to CONFIRM.
// ══════════════════════════════════════════════════════════════════════════

/// Per-d carrier register indices (acc, data, idx) and aux (rb, temp).
struct ReduceRegs {
    acc: usize,
    data: usize,
    idx: usize,
    n_tags: usize, // number of counter tag registers (= d), tag_i = r_i
    #[allow(dead_code)]
    rb: usize,
    #[allow(dead_code)]
    temp: usize,
}
fn reduce_regs(d: usize) -> ReduceRegs {
    // tags occupy r0..r_{d-1}; then acc, data, idx; aux rb, temp last.
    ReduceRegs {
        n_tags: d,
        acc: d,
        data: d + 1,
        idx: d + 2,
        rb: d + 3,
        temp: d + 4,
    }
}

/// DIAGNOSTIC: dump marshal_resident's homes for a concrete reduce input so the
/// carrier register/mem layout is CONFIRMED, not assumed.
#[test]
#[ignore = "diagnostic: prints the empirical reduce carrier layout"]
fn diag_reduce_layout() {
    for d in [2usize, 3] {
        let w = 2u8;
        let ty = carrier_ty(w, d);
        // acc=1, data=2, idx=1, arr0=3, arr1=1 ; counter k=1.
        let input = make_reduce_input(1, 1, 2, 1, 3, 1, d);
        let st = marshal_resident(&input, &ty, 2);
        eprintln!(
            "d={d}: regs={:?}  mem={:?}  (acc=1,data=2,idx=1,arr0=3,arr1=1)",
            &st.regs, &st.mem
        );
    }
}

/// Build a reduce input Value: counter k (unary), acc, data, idx, arr0, arr1.
fn make_reduce_input(k: usize, acc: u8, data: u8, idx: u8, arr0: u8, arr1: u8, _d: usize) -> Value {
    let mut counter = Value::Left(Box::new(Value::Unit));
    for _ in 0..k {
        counter = Value::Right(Box::new(counter));
    }
    let bit_val = |x: u8| {
        if x == 0 {
            Value::Left(Box::new(Value::Unit))
        } else {
            Value::Right(Box::new(Value::Unit))
        }
    };
    let flat_val = |x: u8| {
        let (v, _) = value_from_leaf_bytes_pub(&[x], &Type::Flat(2));
        v
    };
    Value::pair(
        counter,
        Value::pair(
            flat_val(acc),
            Value::pair(
                flat_val(data),
                Value::pair(
                    bit_val(idx),
                    Value::pair(flat_val(arr0), Value::pair(flat_val(arr1), Value::Unit)),
                ),
            ),
        ),
    )
}

/// Compile reduce_flat(w,d) to a ROM (resident=2).
fn compile_reduce_rom(w: u8, d: usize) -> (Vec<u32>, usize) {
    let iso = reduce_flat(w, d);
    let lowered = compile_with_metadata_resident(&iso, 2)
        .unwrap_or_else(|e| panic!("compile reduce_flat({w},{d}): {e:?}"));
    let rom = assemble(&lowered.instrs).expect("assemble");
    (rom, lowered.wcet())
}

/// Body-entry SV at concrete PC=`pc`, faithful to the machine at the top of one
/// RUN body: carrier {acc,data,idx} SYMBOLIC in regs, {arr0,arr1} SYMBOLIC in
/// mem[0]/mem[1]; EVERY other field at its body-entry invariant 0 (BR=0, IR=0,
/// scratch=0, all bus fields=0, other regs=0, mem[2],mem[3]=0). Only the low `w`
/// bits of each Flat(w) field are symbolic (idx is a single bit); the rest 0.
fn body_entry_sv(pc: usize, w: u8, rr: &ReduceRegs, vs: &BddVariableSet) -> SV {
    let wb = w as usize;
    build_system(|f| {
        if f == PC_LO {
            const_word8((pc & 0xFF) as u8, vs)
        } else if f == PC_HI {
            const_word8(((pc >> 8) & 0xFF) as u8, vs)
        } else if f == rr.acc {
            sym_low_word8(vs, f, wb)
        } else if f == rr.data {
            sym_low_word8(vs, f, wb)
        } else if f == rr.idx {
            sym_low_word8(vs, f, 1) // idx is a Bit
        } else if f == mem_f(0) || f == mem_f(1) {
            sym_low_word8(vs, f, wb) // arr0, arr1
        } else {
            const_word8(0, vs) // everything else at the body-entry invariant 0
        }
    })
}

/// SPIKE — the crux measurement. Compose the 6-instr reduce body [3,9) of
/// reduce_flat(2,2) through the SystemStepEngine at concrete PC (RUN path), with
/// symbolic carrier (acc,data,idx in regs; arr0,arr1 in mem) and everything else
/// 0. Print peak BDD nodes + secs per step. IF a step's peak explodes (>500k) or
/// takes minutes, that is the memory-ring blocker — reported LOUDLY.
#[test]
fn spike_system_ring_body_tractability() {
    let w = 2u8;
    let d = 2usize;
    let (rom, _budget) = compile_reduce_rom(w, d);
    let rr = reduce_regs(d);
    let vs = BddVariableSet::new_anonymous(n_vars_sys());
    let engine = SystemStepEngine::new(&rom);
    let mut ctx = WalkCtx::new();

    // Body span for reduce_flat(2,2): [3,9) — the 6 instrs between the two Swapbr.
    let body_start = 3usize;
    let body_len = 6usize;
    let mut cur = body_entry_sv(body_start, w, &rr, &vs);
    let mut max_peak = 0usize;
    let t_all = Instant::now();
    eprintln!("=== SPIKE: system-ring body [3,9) of reduce_flat(2,2), symbolic carrier ===");
    for i in 0..body_len {
        let pc = body_start + i;
        let t0 = Instant::now();
        let out = engine
            .eval_step(cur, &vs, &mut ctx)
            .unwrap_or_else(|e| panic!("SPIKE: system_step FAILED at pc={pc}: {e}"));
        let secs = t0.elapsed().as_secs_f64();
        let (peak, sum) = sv_bdd_sizes(&out);
        max_peak = max_peak.max(peak);
        let next_pc = pc + 1;
        let pc_out = read_concrete_pc(&out);
        eprintln!(
            "  step {i} pc={pc} ({:>10?}): peak={peak} sum={sum} {secs:.3}s  PC_out={pc_out:?} (want {next_pc})",
            rom_instr_name(&rom, pc),
        );
        // RUN body is straight-line (BR=0) so PC_out must be pc+1 — re-pin.
        assert_eq!(pc_out, Some(next_pc), "SPIKE PC-AGREEMENT: pc={pc} PC_out != pc+1");
        cur = repin_pc(&out, next_pc, &vs);
        // LOUD tractability tripwire.
        assert!(
            peak < 500_000,
            "SPIKE BLOWUP: body step {i} (pc={pc}) peak {peak} > 500k — the MEMORY RING is the blocker"
        );
    }
    eprintln!(
        "SPIKE RESULT: {body_len} system-ring body steps composed; MAX peak {max_peak} BDD nodes, \
         total {:.3}s. → the memory-ring body IS {} tractable.",
        t_all.elapsed().as_secs_f64(),
        if max_peak < 500_000 { "" } else { "NOT" }
    );
}

fn rom_instr_name(_rom: &[u32], _pc: usize) -> &'static str {
    // Cosmetic label for the spike trace; the load-bearing checks are the
    // peak/secs/PC_out values, not this string.
    "step"
}

// ══════════════════════════════════════════════════════════════════════════
// The reduce-body FUNCTION spec (BDD level), over the symbolic carrier.
//
// One RUN body copy realizes, over the whole carrier domain:
//     acc' = acc ^ (idx==0 ? arr0 : arr1)     (acc ^= arr[idx])
//     idx' = idx ^ 1
//     data' = data ; arr0'=arr0 ; arr1'=arr1  (preserved)
//   all aux/scratch/bus restored to 0, mem[2],mem[3] preserved.
//
// idx is a single bit, so arr[idx] = ite(idx0, arr1, arr0) per bit.
// ══════════════════════════════════════════════════════════════════════════

/// Per-bit spec for acc' = acc ^ arr[idx] (low `w` bits; high bits stay 0).
fn body_acc_spec(
    acc_in: &[Bdd],
    arr0_in: &[Bdd],
    arr1_in: &[Bdd],
    idx0: &Bdd,
    w: usize,
    vs: &BddVariableSet,
) -> Vec<Bdd> {
    let zero = vs.mk_false();
    (0..W)
        .map(|b| {
            if b < w {
                // arr[idx] bit b = ite(idx0, arr1, arr0)
                let sel = idx0.and(&arr1_in[b]).or(&idx0.not().and(&arr0_in[b]));
                acc_in[b].xor(&sel)
            } else {
                zero.clone()
            }
        })
        .collect()
}

/// Compose the reduce body span [body_start, body_start+body_len) at concrete PC
/// via the SystemStepEngine, symbolic carrier. Returns (output SV, max_peak, secs).
fn compose_body(
    rom: &[u32],
    w: u8,
    rr: &ReduceRegs,
    body_start: usize,
    body_len: usize,
    vs: &BddVariableSet,
) -> (SV, usize, f64) {
    let engine = SystemStepEngine::new(rom);
    let mut ctx = WalkCtx::new();
    let mut cur = body_entry_sv(body_start, w, rr, vs);
    let mut peak = 0usize;
    let t0 = Instant::now();
    for i in 0..body_len {
        let pc = body_start + i;
        let out = engine
            .eval_step(cur, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("compose_body: system_step FAILED at pc={pc}: {e}"));
        let (p, _s) = sv_bdd_sizes(&out);
        peak = peak.max(p);
        let next_pc = pc + 1;
        assert_eq!(
            read_concrete_pc(&out),
            Some(next_pc),
            "compose_body PC-AGREEMENT: pc={pc} PC_out != pc+1 (body must be straight-line BR=0)"
        );
        cur = repin_pc(&out, next_pc, vs);
    }
    (cur, peak, t0.elapsed().as_secs_f64())
}

/// Assert `out`'s carrier ≡ the reduce-body function over the whole domain, and
/// every non-carrier field restored to its entry value (aux/scratch/bus/IR = 0,
/// mem[2],mem[3] preserved). `body_start` is where the carrier entered symbolic.
/// FIELD-COMPLETE: checks all regs, all mem, all bus, all scratch, all IR, BR.
fn assert_body_semantics(out: &SV, w: u8, rr: &ReduceRegs, vs: &BddVariableSet) {
    let wb = w as usize;
    let zero = vs.mk_false();
    // Entry references (the same symbolic vars body_entry_sv installed).
    let acc_in = entry_bits(vs, rr.acc, wb);
    let data_in = entry_bits(vs, rr.data, wb);
    let idx_in = entry_bits(vs, rr.idx, 1);
    let arr0_in = entry_bits(vs, mem_f(0), wb);
    let arr1_in = entry_bits(vs, mem_f(1), wb);

    // acc' = acc ^ arr[idx].
    let acc_want = body_acc_spec(&acc_in, &arr0_in, &arr1_in, &idx_in[0], wb, vs);
    let acc_out = sys_field_bits(out, rr.acc);
    for b in 0..W {
        assert!(acc_out[b] == acc_want[b], "BODY: acc' bit {b} != acc ^ arr[idx]");
    }
    // idx' = idx ^ 1 (bit 0 flipped; higher bits 0).
    let idx_out = sys_field_bits(out, rr.idx);
    for b in 0..W {
        let want = if b == 0 { idx_in[0].not() } else { zero.clone() };
        assert!(idx_out[b] == want, "BODY: idx' bit {b} != idx ^ 1");
    }
    // data' = data (preserved).
    let data_out = sys_field_bits(out, rr.data);
    for b in 0..W {
        assert!(data_out[b] == data_in[b], "BODY: data' bit {b} != data (not preserved)");
    }
    // arr0', arr1' preserved.
    let arr0_out = sys_field_bits(out, mem_f(0));
    let arr1_out = sys_field_bits(out, mem_f(1));
    for b in 0..W {
        assert!(arr0_out[b] == arr0_in[b], "BODY: arr0 bit {b} not preserved");
        assert!(arr1_out[b] == arr1_in[b], "BODY: arr1 bit {b} not preserved");
    }
    // mem[2], mem[3] preserved at 0 (entered 0).
    for m in 2..MEM_WORDS {
        let got = sys_field_bits(out, mem_f(m));
        for b in 0..W {
            assert!(got[b] == zero, "BODY: mem[{m}] bit {b} != 0 (disturbed)");
        }
    }
    // Every OTHER field restored to 0: other regs, BR, IR(4), scratch(8), bus(3).
    for r in 0..NUM_REGS {
        if r == rr.acc || r == rr.data || r == rr.idx {
            continue;
        }
        let got = sys_field_bits(out, r);
        for b in 0..W {
            assert!(got[b] == zero, "BODY: reg r{r} bit {b} != 0 (leak/stray write)");
        }
    }
    let mut other: Vec<usize> = vec![BR_F];
    for k in 0..4 {
        other.push(ir_byte(k));
    }
    for s in 0..NUM_SCRATCH {
        other.push(scratch_f(s));
    }
    for i in 0..NUM_BUS_FIELDS {
        other.push(bus_f(i));
    }
    for f in other {
        let got = sys_field_bits(out, f);
        for b in 0..W {
            assert!(got[b] == zero, "BODY: field {f} bit {b} != 0 (aux/scratch/bus/IR/BR leak)");
        }
    }
}

/// L1-analog — the reduce BODY LEMMA (memory ring, whole carrier domain).
/// Compose body [3,9) of reduce_flat(2,2), prove carrier ≡ reduce-body function.
#[test]
fn invariant_body_lemma_reduce() {
    let w = 2u8;
    let d = 2usize;
    let (rom, _budget) = compile_reduce_rom(w, d);
    let rr = reduce_regs(d);
    let vs = BddVariableSet::new_anonymous(n_vars_sys());
    let (body_start, body_len) = (3usize, 6usize);

    let (out, peak, secs) = compose_body(&rom, w, &rr, body_start, body_len, &vs);
    assert_body_semantics(&out, w, &rr, &vs);

    // TEETH (NEG-body): a WRONG body spec — acc' = acc ^ arr[idx^1] (wrong index)
    // — must DIFFER from the real composed acc output (so the assert isn't vacuous).
    let wb = w as usize;
    let acc_in = entry_bits(&vs, rr.acc, wb);
    let arr0_in = entry_bits(&vs, mem_f(0), wb);
    let arr1_in = entry_bits(&vs, mem_f(1), wb);
    let idx_in = entry_bits(&vs, rr.idx, 1);
    let acc_out = sys_field_bits(&out, rr.acc);
    // wrong: swap idx branches (arr[idx^1]).
    let wrong = body_acc_spec(&acc_in, &arr1_in, &arr0_in, &idx_in[0], wb, &vs);
    let differs = (0..W).any(|b| acc_out[b] != wrong[b]);
    assert!(differs, "NEG-body: real acc == WRONG acc^arr[idx^1] — teeth vacuous");

    assert!(peak < 500_000, "BODY LEMMA peak {peak} — memory ring blowup");
    eprintln!(
        "L1 BODY LEMMA [reduce_flat(2,2)]: system-ring body [3,9) ≡ (acc^=arr[idx]; idx^=1; \
         data/arr preserved) over the WHOLE symbolic carrier domain; all aux/scratch/bus/IR/BR/mem \
         restored. Peak {peak} BDD nodes, {secs:.3}s. NEG-body fires. → MEMORY RING TRACTABLE."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// L2-analog — the SKIP-BLOCK LEMMA (the true Iter novelty).
//
// One skip-block copy is [copy_start, copy_start+12):
//   +0 Addi rb, 6            (rb = body_len 6 = skip distance)
//   +1 Cswap ctrl, rb, temp  (ctrl=1 RUN: rb=0,temp=6 · ctrl=0 SKIP: rb=6)
//   +2 Swapbr rb             (FORWARD skip: rb=6 → jump over body ; rb=0 → PC+1)
//   +3..+9 body (6 instrs)
//   +9 Swapbr rb             (come-from: recapture / clear BR)
//   +10 Cswap ctrl, rb, temp (cleanup)
//   +11 Subi rb, 6           (cleanup)
//
// Because Swapbr makes PC data-dependent on ctrl, we CASE-SPLIT on ctrl∈{0,1}
// (the counter tag is a concrete bit at any real iteration boundary — faithful
// and complete). We drive PC by the datapath's OWN concrete PC output each step
// (NOT pc+1), so the forward jump is followed FAITHFULLY: this is the machinery
// the come-from proof did not need.
//   RUN  (ctrl=1): rb=0 at Swapbr[+2] → BR=0 → PC=+3, body runs, come-from BR=0,
//                  exit PC=+12, carrier := body(carrier).
//   SKIP (ctrl=0): rb=6 at Swapbr[+2] → BR=6 → forward jump. The KEY assert: the
//                  datapath PC output after Swapbr[+2] MUST equal the come-from PC
//                  (copy_start+9) — PC += sext(BR)+1 = (+2)+6+1 = +9. Body [+3,+8]
//                  never executes → MexchReg never fetched. exit PC=+12, carrier
//                  IDENTITY.
// ══════════════════════════════════════════════════════════════════════════

/// Read the skip-block aux registers (ctrl, rb, temp) + body_len from a copy.
struct SkipBlock {
    copy_start: usize,
    ctrl: usize,
    rb: usize,
    #[allow(dead_code)]
    temp: usize,
    body_len_imm: u8,
}
fn read_skip_block(rom_instrs: &[Instr], copy_start: usize) -> SkipBlock {
    let (rb, body_len_imm) = match rom_instrs[copy_start] {
        Instr::Addi { rd, imm } => (rd as usize, imm),
        ref o => panic!("skip[+0] must be Addi rb,body_len, got {o:?}"),
    };
    let (ctrl, temp) = match rom_instrs[copy_start + 1] {
        Instr::Cswap { rd, rs, rt } => {
            assert_eq!(rs as usize, rb, "Cswap rs must be rb");
            (rd as usize, rt as usize)
        }
        ref o => panic!("skip[+1] must be Cswap ctrl,rb,temp, got {o:?}"),
    };
    assert!(
        matches!(rom_instrs[copy_start + 2], Instr::Swapbr { rd } if rd as usize == rb),
        "skip[+2] must be Swapbr rb, got {:?}",
        rom_instrs[copy_start + 2]
    );
    assert!(
        matches!(rom_instrs[copy_start + 9], Instr::Swapbr { rd } if rd as usize == rb),
        "skip[+9] must be Swapbr rb (come-from), got {:?}",
        rom_instrs[copy_start + 9]
    );
    assert!(
        matches!(rom_instrs[copy_start + 11], Instr::Subi { rd, .. } if rd as usize == rb),
        "skip[+11] must be Subi rb, got {:?}",
        rom_instrs[copy_start + 11]
    );
    SkipBlock { copy_start, ctrl, rb, temp, body_len_imm }
}

/// Whole-copy entry SV: PC=copy_start, ctrl register = concrete `ctrl_val` (0/1),
/// carrier {acc,data,idx,arr0,arr1} symbolic, everything else 0 (BR=0, IR=0,
/// scratch=0, bus=0, rb=temp=0, other regs=0, mem[2],mem[3]=0). The OTHER
/// counter-tag regs (besides ctrl) enter 0 (they are not read inside this copy).
fn skip_entry_sv(
    sb: &SkipBlock,
    ctrl_val: u8,
    w: u8,
    rr: &ReduceRegs,
    vs: &BddVariableSet,
) -> SV {
    let wb = w as usize;
    build_system(|f| {
        if f == PC_LO {
            const_word8((sb.copy_start & 0xFF) as u8, vs)
        } else if f == PC_HI {
            const_word8(((sb.copy_start >> 8) & 0xFF) as u8, vs)
        } else if f == sb.ctrl {
            const_word8(ctrl_val, vs)
        } else if f == rr.acc {
            sym_low_word8(vs, f, wb)
        } else if f == rr.data {
            sym_low_word8(vs, f, wb)
        } else if f == rr.idx {
            sym_low_word8(vs, f, 1)
        } else if f == mem_f(0) || f == mem_f(1) {
            sym_low_word8(vs, f, wb)
        } else {
            const_word8(0, vs)
        }
    })
}

/// Compose a whole skip-block copy [copy_start, copy_start+12), driving PC by the
/// datapath's OWN concrete PC output each step (so the forward skip is followed
/// faithfully). Returns (out SV, max_peak, secs, pc_after_first_swapbr).
/// `pc_after_first_swapbr` is the datapath PC output produced by Swapbr[+2] — the
/// KEY forward-skip control value (the Iter analog of vart's GAP-A landing assert).
fn compose_skip_block(
    rom: &[u32],
    sb: &SkipBlock,
    ctrl_val: u8,
    w: u8,
    rr: &ReduceRegs,
    vs: &BddVariableSet,
) -> (SV, usize, f64, usize) {
    let engine = SystemStepEngine::new(rom);
    let mut ctx = WalkCtx::new();
    let mut cur = skip_entry_sv(sb, ctrl_val, w, rr, vs);
    let mut peak = 0usize;
    let mut pc = sb.copy_start;
    let exit_pc = sb.copy_start + 12;
    let swapbr_pc = sb.copy_start + 2;
    let mut pc_after_first_swapbr = 0usize;
    let t0 = Instant::now();
    // Step until PC reaches exit_pc; guard against runaway.
    for _guard in 0..64 {
        if pc >= exit_pc {
            break;
        }
        let out = engine
            .eval_step(cur, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("compose_skip_block: system_step FAILED at pc={pc}: {e}"));
        let (p, _s) = sv_bdd_sizes(&out);
        peak = peak.max(p);
        // The datapath's OWN concrete PC output drives the next step (faithful to
        // the forward jump). All Swapbr's here have a CONCRETE rb (ctrl concrete
        // ⇒ Cswap output concrete ⇒ BR concrete), so PC_out is always concrete.
        let next_pc = read_concrete_pc(&out).unwrap_or_else(|| {
            panic!("compose_skip_block: PC_out SYMBOLIC at pc={pc} — ctrl case-split broken")
        });
        if pc == swapbr_pc {
            pc_after_first_swapbr = next_pc;
        }
        cur = repin_pc(&out, next_pc, vs);
        pc = next_pc;
    }
    assert_eq!(pc, exit_pc, "compose_skip_block: did not land on exit PC {exit_pc}");
    (cur, peak, t0.elapsed().as_secs_f64(), pc_after_first_swapbr)
}

/// Assert the aux {rb, temp, ctrl-preserved} + carrier + all-fields state at the
/// EXIT of a skip-block copy, for the given ctrl value. `run` selects the body
/// transform (RUN) vs identity (SKIP).
fn assert_skip_exit(
    out: &SV,
    sb: &SkipBlock,
    ctrl_val: u8,
    run: bool,
    w: u8,
    rr: &ReduceRegs,
    vs: &BddVariableSet,
) {
    let wb = w as usize;
    let zero = vs.mk_false();
    let acc_in = entry_bits(vs, rr.acc, wb);
    let data_in = entry_bits(vs, rr.data, wb);
    let idx_in = entry_bits(vs, rr.idx, 1);
    let arr0_in = entry_bits(vs, mem_f(0), wb);
    let arr1_in = entry_bits(vs, mem_f(1), wb);

    // Carrier: RUN ⇒ body(carrier), SKIP ⇒ identity.
    let acc_out = sys_field_bits(out, rr.acc);
    let idx_out = sys_field_bits(out, rr.idx);
    if run {
        let acc_want = body_acc_spec(&acc_in, &arr0_in, &arr1_in, &idx_in[0], wb, vs);
        for b in 0..W {
            assert!(acc_out[b] == acc_want[b], "SKIP-RUN: acc' bit {b} != acc^arr[idx]");
            let want_idx = if b == 0 { idx_in[0].not() } else { zero.clone() };
            assert!(idx_out[b] == want_idx, "SKIP-RUN: idx' bit {b} != idx^1");
        }
    } else {
        for b in 0..W {
            assert!(acc_out[b] == acc_in[b], "SKIP-SKIP: acc bit {b} != identity");
            assert!(idx_out[b] == idx_in[b], "SKIP-SKIP: idx bit {b} != identity");
        }
    }
    // data + arr always preserved.
    let data_out = sys_field_bits(out, rr.data);
    let arr0_out = sys_field_bits(out, mem_f(0));
    let arr1_out = sys_field_bits(out, mem_f(1));
    for b in 0..W {
        assert!(data_out[b] == data_in[b], "SKIP: data bit {b} not preserved");
        assert!(arr0_out[b] == arr0_in[b], "SKIP: arr0 bit {b} not preserved");
        assert!(arr1_out[b] == arr1_in[b], "SKIP: arr1 bit {b} not preserved");
    }
    // ctrl preserved = ctrl_val (the skip aux Cswaps cancel: mint then un-mint).
    let ctrl_out = sys_field_bits(out, sb.ctrl);
    for b in 0..W {
        let want = if (ctrl_val >> b) & 1 == 1 { vs.mk_true() } else { zero.clone() };
        assert!(ctrl_out[b] == want, "SKIP: ctrl bit {b} != preserved {ctrl_val}");
    }
    // aux rb, temp restored to 0 at exit.
    for f in [sb.rb, sb.temp] {
        let got = sys_field_bits(out, f);
        for b in 0..W {
            assert!(got[b] == zero, "SKIP: aux reg r{f} bit {b} != 0 at exit");
        }
    }
    // FIELD-COMPLETE: mem[2],mem[3] preserved 0; BR, IR(4), scratch(8), bus(3),
    // and every other reg = 0.
    for m in 2..MEM_WORDS {
        let got = sys_field_bits(out, mem_f(m));
        for b in 0..W {
            assert!(got[b] == zero, "SKIP: mem[{m}] bit {b} != 0");
        }
    }
    for r in 0..NUM_REGS {
        if r == sb.ctrl || r == rr.acc || r == rr.data || r == rr.idx || r == sb.rb || r == sb.temp {
            continue;
        }
        let got = sys_field_bits(out, r);
        for b in 0..W {
            assert!(got[b] == zero, "SKIP: reg r{r} bit {b} != 0");
        }
    }
    let mut other: Vec<usize> = vec![BR_F];
    for k in 0..4 {
        other.push(ir_byte(k));
    }
    for s in 0..NUM_SCRATCH {
        other.push(scratch_f(s));
    }
    for i in 0..NUM_BUS_FIELDS {
        other.push(bus_f(i));
    }
    for f in other {
        let got = sys_field_bits(out, f);
        for b in 0..W {
            assert!(got[b] == zero, "SKIP: field {f} bit {b} != 0 (BR/IR/scratch/bus leak)");
        }
    }
}

/// L2-analog — the SKIP-BLOCK LEMMA. Both ctrl cases over the whole carrier.
#[test]
fn invariant_skip_block_lemma_reduce() {
    let w = 2u8;
    let d = 2usize;
    let iso = reduce_flat(w, d);
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
    let rom = assemble(&lowered.instrs).expect("assemble");
    let rr = reduce_regs(d);
    let sb = read_skip_block(&lowered.instrs, 0);
    let vs = BddVariableSet::new_anonymous(n_vars_sys());

    let come_from_pc = sb.copy_start + 9;
    let exit_pc = sb.copy_start + 12;

    // ── RUN case (ctrl=1) ──────────────────────────────────────────────────
    let (out_run, peak_run, secs_run, land_run) =
        compose_skip_block(&rom, &sb, 1, w, &rr, &vs);
    // RUN: rb=0 at Swapbr[+2] ⇒ BR=0 ⇒ PC lands at body_start (+3).
    assert_eq!(
        land_run,
        sb.copy_start + 3,
        "SKIP-RUN forward-skip PC: Swapbr[+2] must land at body_start (+3) when ctrl=1"
    );
    assert_eq!(read_concrete_pc(&out_run), Some(exit_pc), "SKIP-RUN exit PC != +12");
    assert_body_run_reached_exit(&out_run, exit_pc);
    assert_skip_exit(&out_run, &sb, 1, true, w, &rr, &vs);

    // ── SKIP case (ctrl=0) ─────────────────────────────────────────────────
    let (out_skip, peak_skip, secs_skip, land_skip) =
        compose_skip_block(&rom, &sb, 0, w, &rr, &vs);
    // THE KEY FORWARD-SKIP ASSERT (Iter analog of vart's GAP-A landing): the
    // datapath PC output after Swapbr[+2] MUST equal the come-from PC (+9): body
    // [+3,+8] skipped. PC += sext(BR=6)+1 = (+2)+6+1 = +9.
    assert_eq!(
        land_skip, come_from_pc,
        "SKIP forward-skip PC: Swapbr[+2] must land on come-from (+9) when ctrl=0 (BR=body_len=6)"
    );
    assert_eq!(read_concrete_pc(&out_skip), Some(exit_pc), "SKIP exit PC != +12");
    assert_skip_exit(&out_skip, &sb, 0, false, w, &rr, &vs);

    // ── TEETH ────────────────────────────────────────────────────────────
    // NEG-skip: a WRONG landing (e.g. +8, body only partially skipped) must be
    // DETECTED — the real landing must differ from the wrong one.
    let wrong_land = sb.copy_start + 8;
    assert_ne!(land_skip, wrong_land, "NEG-skip: real landing == wrong landing (+8) — teeth vacuous");
    // NEG-run-vs-skip: the RUN carrier output must DIFFER from the SKIP output
    // (body changed acc vs identity), for at least one bit.
    let acc_run = sys_field_bits(&out_run, rr.acc);
    let acc_skip = sys_field_bits(&out_skip, rr.acc);
    let differs = (0..W).any(|b| acc_run[b] != acc_skip[b]);
    assert!(differs, "NEG-run-vs-skip: RUN acc == SKIP acc — the body did nothing / teeth vacuous");

    assert!(peak_run < 500_000 && peak_skip < 500_000, "SKIP-BLOCK peak blowup");
    // The skip-block SKIP δu forward-reach, discharged closed-form vs the netlist
    // landing above: BR=body_len ⇒ PC += body_len+1, skipping exactly the 6 body
    // instrs + landing on the come-from Swapbr that then clears BR.
    assert_eq!(sb.body_len_imm, 6, "body_len imm must be 6");
    eprintln!(
        "L2 SKIP-BLOCK LEMMA [reduce_flat(2,2)]: RUN(ctrl=1)⇒carrier:=body(carrier), Swapbr[+2]→+3, \
         peak {peak_run}, {secs_run:.3}s. SKIP(ctrl=0)⇒carrier identity, Swapbr[+2] FORWARD-JUMP →+9 \
         (come-from, body skipped), peak {peak_skip}, {secs_skip:.3}s. Both exit PC=+12; aux rb=temp=0; \
         ctrl preserved; all fields welded. NEG-skip + NEG-run-vs-skip fire. → FORWARD-SKIP LOOP PROVEN."
    );
}

/// After a RUN copy, sanity that PC reached exit (the body actually executed).
fn assert_body_run_reached_exit(out: &SV, exit_pc: usize) {
    assert_eq!(read_concrete_pc(out), Some(exit_pc), "RUN copy did not reach exit PC");
}

// ══════════════════════════════════════════════════════════════════════════
// COMPOSITION LEMMA (d=3, two copies) — the STATIC-TRIP-COUNT argument.
//
// Because loop_terms is EMPTY and there are exactly D-1 PHYSICAL copies, NO
// induction over a data-dependent count is needed: the whole program is
// copy0;copy1;...;copy_{D-2}, each a fixed skip-block guarded on its unary tag.
// We prove, for d=3, that composing the full 24-instr ROM through the
// SystemStepEngine for each concrete (tag0,tag1) prefix reproduces `iter`
// semantics: exactly k = number of leading 1-tags body applications.
//
// The unary counter tags are a PREFIX of 1s (Nat_d marshals k as k leading
// Rights): reachable (tag0,tag1) ∈ {(0,0)→k=0, (1,0)→k=1, (1,1)→k=2}.
// ══════════════════════════════════════════════════════════════════════════

/// Spec: apply the reduce body function k times to a symbolic carrier, returning
/// (acc_bits, idx_bit0) after k applications. arr fixed; data untouched.
fn body_k_spec(
    acc_in: &[Bdd],
    arr0_in: &[Bdd],
    arr1_in: &[Bdd],
    idx0_in: &Bdd,
    k: usize,
    w: usize,
    vs: &BddVariableSet,
) -> (Vec<Bdd>, Bdd) {
    let mut acc: Vec<Bdd> = acc_in.to_vec();
    let mut idx0: Bdd = idx0_in.clone();
    for _ in 0..k {
        acc = body_acc_spec(&acc, arr0_in, arr1_in, &idx0, w, vs);
        idx0 = idx0.not();
    }
    (acc, idx0)
}

/// Whole-ROM entry SV for the d=3 composition: PC=0, tag regs r0..r2 set to the
/// concrete unary counter (tags[i]), carrier {acc,data,idx,arr} symbolic, all
/// aux/scratch/bus/IR/BR/other = 0.
fn full_rom_entry_sv(tags: &[u8], w: u8, rr: &ReduceRegs, vs: &BddVariableSet) -> SV {
    let wb = w as usize;
    build_system(|f| {
        if f == PC_LO || f == PC_HI {
            const_word8(0, vs)
        } else if f < rr.n_tags {
            const_word8(tags[f], vs) // tag_f = r_f
        } else if f == rr.acc {
            sym_low_word8(vs, f, wb)
        } else if f == rr.data {
            sym_low_word8(vs, f, wb)
        } else if f == rr.idx {
            sym_low_word8(vs, f, 1)
        } else if f == mem_f(0) || f == mem_f(1) {
            sym_low_word8(vs, f, wb)
        } else {
            const_word8(0, vs)
        }
    })
}

/// Compose the whole ROM [0, n_instrs) via the SystemStepEngine, PC driven by the
/// datapath's own concrete PC output (following every forward skip faithfully).
fn compose_full_rom(
    rom: &[u32],
    n_instrs: usize,
    entry: SV,
    vs: &BddVariableSet,
) -> (SV, usize, f64) {
    let engine = SystemStepEngine::new(rom);
    let mut ctx = WalkCtx::new();
    let mut cur = entry;
    let mut peak = 0usize;
    let mut pc = 0usize;
    let t0 = Instant::now();
    for _guard in 0..(n_instrs + 8) {
        if pc >= n_instrs {
            break;
        }
        let out = engine
            .eval_step(cur, vs, &mut ctx)
            .unwrap_or_else(|e| panic!("compose_full_rom: system_step FAILED at pc={pc}: {e}"));
        let (p, _s) = sv_bdd_sizes(&out);
        peak = peak.max(p);
        let next_pc = read_concrete_pc(&out)
            .unwrap_or_else(|| panic!("compose_full_rom: PC_out SYMBOLIC at pc={pc}"));
        cur = repin_pc(&out, next_pc, vs);
        pc = next_pc;
    }
    assert_eq!(pc, n_instrs, "compose_full_rom: did not reach ROM end {n_instrs}");
    (cur, peak, t0.elapsed().as_secs_f64())
}

/// COMPOSITION LEMMA (d=3): full 24-instr ROM ≡ body^k for k = leading 1-tags,
/// over each reachable (tag0,tag1) prefix combo. Proves the static composition
/// reproduces `iter` semantics WITHOUT induction.
#[test]
fn invariant_composition_lemma_reduce_d3() {
    let w = 2u8;
    let d = 3usize;
    let iso = reduce_flat(w, d);
    let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
    let rom = assemble(&lowered.instrs).expect("assemble");
    let n_instrs = lowered.instrs.len();
    assert_eq!(n_instrs, 24, "reduce_flat(2,3) must be 24 instrs");
    // The STATIC-TRIP-COUNT premise (no induction, D-1 physical copies): an Iter
    // contributes NO loop_terms (only Trace/Fold do). If this ever becomes
    // non-empty the composition-over-copies argument (vs induction) is invalid.
    assert!(
        lowered.loop_terms.is_empty(),
        "COMPOSITION premise: Iter must yield EMPTY loop_terms (got {}) — static composition invalid",
        lowered.loop_terms.len()
    );
    let rr = reduce_regs(d);
    let wb = w as usize;
    let vs = BddVariableSet::new_anonymous(n_vars_sys());
    let zero = vs.mk_false();

    // Reachable unary-prefix combos: (tags, k). tag2 stays 0 (only copies 0,1 run).
    let combos: [([u8; 3], usize); 3] = [([0, 0, 0], 0), ([1, 0, 0], 1), ([1, 1, 0], 2)];

    let mut max_peak = 0usize;
    let mut total = 0f64;
    for (tags, k) in combos {
        let entry = full_rom_entry_sv(&tags, w, &rr, &vs);
        let (out, peak, secs) = compose_full_rom(&rom, n_instrs, entry, &vs);
        max_peak = max_peak.max(peak);
        total += secs;

        // Spec: body^k over the symbolic carrier.
        let acc_in = entry_bits(&vs, rr.acc, wb);
        let data_in = entry_bits(&vs, rr.data, wb);
        let idx_in = entry_bits(&vs, rr.idx, 1);
        let arr0_in = entry_bits(&vs, mem_f(0), wb);
        let arr1_in = entry_bits(&vs, mem_f(1), wb);
        let (acc_want, idx0_want) = body_k_spec(&acc_in, &arr0_in, &arr1_in, &idx_in[0], k, wb, &vs);

        let acc_out = sys_field_bits(&out, rr.acc);
        let idx_out = sys_field_bits(&out, rr.idx);
        for b in 0..W {
            assert!(acc_out[b] == acc_want[b], "COMPOSE k={k}: acc' bit {b} != body^{k}");
            let want_idx = if b == 0 { idx0_want.clone() } else { zero.clone() };
            assert!(idx_out[b] == want_idx, "COMPOSE k={k}: idx' bit {b} != idx^{k}");
        }
        // data + arr preserved; tags preserved; aux + scratch/bus/BR/IR clean.
        let data_out = sys_field_bits(&out, rr.data);
        for b in 0..W {
            assert!(data_out[b] == data_in[b], "COMPOSE k={k}: data bit {b} not preserved");
        }
        for (m, arr_in) in [(0usize, &arr0_in), (1, &arr1_in)] {
            let got = sys_field_bits(&out, mem_f(m));
            for b in 0..W {
                assert!(got[b] == arr_in[b], "COMPOSE k={k}: arr{m} bit {b} not preserved");
            }
        }
        for t in 0..rr.n_tags {
            let got = sys_field_bits(&out, t);
            for b in 0..W {
                let want = if (tags[t] >> b) & 1 == 1 { vs.mk_true() } else { zero.clone() };
                assert!(got[b] == want, "COMPOSE k={k}: tag r{t} bit {b} not preserved");
            }
        }
        // FIELD-COMPLETE clean-up: BR, IR, scratch, bus, aux rb/temp, mem[2,3]=0.
        let mut clean: Vec<usize> = vec![BR_F, rr.rb, rr.temp];
        for kk in 0..4 {
            clean.push(ir_byte(kk));
        }
        for s in 0..NUM_SCRATCH {
            clean.push(scratch_f(s));
        }
        for i in 0..NUM_BUS_FIELDS {
            clean.push(bus_f(i));
        }
        for m in 2..MEM_WORDS {
            clean.push(mem_f(m));
        }
        for f in clean {
            let got = sys_field_bits(&out, f);
            for b in 0..W {
                assert!(got[b] == zero, "COMPOSE k={k}: field {f} bit {b} != 0 (leak)");
            }
        }

        // TEETH: body^k must DIFFER from body^{k+1} (each run changes acc/idx), so
        // the k-count is load-bearing (not vacuous). Compare acc_want vs body^{k+1}.
        let (acc_next, _) = body_k_spec(&acc_in, &arr0_in, &arr1_in, &idx_in[0], k + 1, wb, &vs);
        let differs = (0..wb).any(|b| acc_want[b] != acc_next[b]);
        assert!(differs, "COMPOSE teeth: body^{k} == body^{} — trip count vacuous", k + 1);

        eprintln!("  COMPOSE tags={tags:?} k={k}: full ROM ≡ body^{k}, peak {peak}, {secs:.3}s");
    }

    assert!(max_peak < 500_000, "COMPOSITION peak blowup {max_peak}");
    eprintln!(
        "COMPOSITION LEMMA [reduce_flat(2,3)]: static 2-copy composition reproduces `iter` \
         (body^k, k = leading 1-tags) over k∈{{0,1,2}} whole carrier domain; loop_terms EMPTY, \
         NO induction. MAX peak {max_peak} BDD nodes, total {total:.3}s."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// MARSHAL-ENCODING ANCHOR (always-on) — the PREMISE the COMPOSITION lemma's
// case-split rests on.
//
// `invariant_composition_lemma_reduce_d3` enumerates the combos {(0,0,0),
// (1,0,0),(1,1,0)} and CLAIMS these cover all REACHABLE counter states. That is
// only true if `marshal_resident` encodes Nat_d as LEADING-ONES in r0..r_{d-1}
// (k leading 1-tags, the rest 0). Previously this was checked ONLY by the
// `#[ignore]` `diag_reduce_layout` — so a change to the marshal encoding could
// leave the composition tests green while they proved UNREACHABLE combos. This
// always-on test pins the premise: for each k, marshal_resident(input).regs[0..d]
// is exactly the k-leading-ones prefix (r0..r_{k-1}=1, r_k..r_{d-1}=0).
// ══════════════════════════════════════════════════════════════════════════
#[test]
fn invariant_marshal_encodes_leading_ones() {
    let w = 2u8;
    for d in [2usize, 3] {
        let ty = carrier_ty(w, d);
        for k in 0..d {
            let input = make_reduce_input(k, 1, 2, (k % 2) as u8, 3, 1, d);
            let st = marshal_resident(&input, &ty, 2);
            for i in 0..d {
                let want = if i < k { 1u8 } else { 0u8 };
                assert_eq!(
                    st.regs[i], want,
                    "MARSHAL-ENCODING d={d} k={k}: tag reg r{i} = {} (expected leading-ones \
                     prefix {want}); the COMPOSITION lemma's reachable-combos case-split is \
                     INVALID if this premise breaks",
                    st.regs[i]
                );
            }
        }
        // TEETH: the k-leading-ones prefix is DISTINCT across k (so the assert is
        // load-bearing, not a constant). Distinct k ⇒ distinct tag prefix.
        let ty2 = carrier_ty(w, d);
        let p0 = marshal_resident(&make_reduce_input(0, 1, 2, 0, 3, 1, d), &ty2, 2).regs[0..d].to_vec();
        let p1 = marshal_resident(&make_reduce_input(1, 1, 2, 1, 3, 1, d), &ty2, 2).regs[0..d].to_vec();
        assert_ne!(p0, p1, "MARSHAL-ENCODING d={d}: k=0 and k=1 tag prefixes collapse — teeth vacuous");
        eprintln!("  marshal-encoding d={d}: regs[0..{d}] == k-leading-ones for k=0..{}", d - 1);
    }
    eprintln!(
        "MARSHAL-ENCODING ANCHOR: Nat_d marshals as leading-ones in r0..r_{{d-1}} — the \
         premise the COMPOSITION reachable-combos case-split rests on."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// L3-analog — REALITY ANCHOR (fast, always-on). For d=2 and d=3, on a handful of
// concrete inputs spanning k=0..d-1, assert run_rom_fast == eval. Seconds. Ties
// the symbolic lemmas to the real machine's semantics.
// ══════════════════════════════════════════════════════════════════════════
#[test]
fn invariant_reality_anchor_reduce() {
    let w = 2u8;
    for d in [2usize, 3] {
        let iso = reduce_flat(w, d);
        let ty = carrier_ty(w, d);
        let lowered = compile_with_metadata_resident(&iso, 2).expect("compile");
        let rom = assemble(&lowered.instrs).expect("assemble");
        let budget = lowered.wcet();
        for k in 0..d {
            // Vary acc/idx/arr per k so each input is nontrivial.
            let input = make_reduce_input(k, 1, 2, (k % 2) as u8, 3, 1, d);
            let state = marshal_resident(&input, &ty, 2);
            let final_state = run_rom_fast(&rom, state, budget);
            let rom_out = unmarshal_resident(&final_state, &ty, 2);
            let mut fuel = 100_000_000u64;
            let eval_out = theseus_eval::eval(&iso, input.clone(), &mut fuel).expect("eval");
            assert_eq!(rom_out, eval_out, "ANCHOR d={d} k={k}: run_rom_fast != eval");
            // TEETH: a DIFFERENT input (k+1 body apps, or different arr) must give a
            // DIFFERENT eval output when k>0 (so the anchor equality is nonvacuous).
            if k > 0 {
                let other = make_reduce_input(k, 0, 2, (k % 2) as u8, 3, 1, d); // acc=0
                let mut f2 = 100_000_000u64;
                let ev2 = theseus_eval::eval(&iso, other, &mut f2).expect("eval2");
                assert_ne!(eval_out, ev2, "ANCHOR teeth d={d} k={k}: distinct inputs collapse");
            }
            eprintln!("  anchor d={d} k={k}: run_rom_fast == eval");
        }
    }
    eprintln!(
        "L3 REALITY ANCHOR [reduce]: run_rom_fast == eval for d∈{{2,3}}, k=0..d-1. \
         Ties the L1/L2/composition symbolic lemmas to the real machine."
    );
}

// ══════════════════════════════════════════════════════════════════════════
// FINDINGS (GO/NO-GO) — the inductive-invariant method EXTENDS to the Iter
// FORWARD-SKIP-BLOCK loop. HONEST research record.
//
// GO. The method extends. Measured (reduce_flat, w=2, w_pc=16 default layout):
//   * SPIKE / L1 BODY (memory ring): the 6-instr MexchReg body walked through the
//     monolithic `system_step` (CPU+Mem+Bus, 30 fields, 240 vars) with symbolic
//     carrier stays TINY — MAX peak 9 BDD nodes, ~1.6 s/step (~8-16 s total). The
//     cost is the un-short-circuited fetch-mux gate count, NOT BDD growth. The
//     MEMORY RING IS TRACTABLE — this was the crux risk and it is cleared.
//   * L2 SKIP-BLOCK RUN (ctrl=1): whole copy → carrier:=body(carrier); Swapbr[+2]
//     lands +3. peak 9, ~16 s.
//   * L2 SKIP-BLOCK SKIP (ctrl=0): whole copy → carrier IDENTITY; the FORWARD-JUMP
//     Swapbr[+2] lands on the come-from (+9), body skipped. peak 3, ~8 s.
//   * COMPOSITION d=3: full 24-instr ROM ≡ body^k, k∈{0,1,2}, peak 9, ~99 s.
//   * L3 anchor: run_rom_fast == eval, d∈{2,3}, k=0..d-1 (seconds).
//
// THE NEW LEMMA SHAPE (Iter vs Trace come-from):
//   * Trace come-from: ONE body copy + a DATA-DEPENDENT BACKWARD JUMP ⇒ needs
//     INDUCTION over a data-dependent trip count (L0 re-entry weld + L2 back-jump
//     BR-router + a math induction argument).
//   * Iter skip-block: D-1 STATIC physical copies, each a FORWARD data-dependent
//     skip. The novelty is (a) CASE-SPLIT on the concrete counter tag (ctrl∈{0,1}
//     at every real iteration boundary), (b) a FORWARD-JUMP PC-LANDING assert —
//     the SKIP path's Swapbr must land exactly on the come-from (PC+=body_len+1),
//     the sole BR≠0 pass — and (c) STATIC COMPOSITION of the D-1 fixed per-copy
//     relations INSTEAD of induction. `loop_terms` is EMPTY, wcet()==instr count.
//
// STATIC TRIP COUNT = ADVANTAGE. Yes: because the count is compile-time (D-1
// copies), there is NO data-dependent back-edge, NO induction, NO re-entry weld.
// The proof is a finite composition of per-copy relations — strictly simpler than
// the Trace come-from. Its cost is O(D) copies (the ROM is unrolled), which the
// composition lemma walks directly.
//
// FIELD-COMPLETE WELD. Every lemma asserts the FULL system state on the copy
// boundary: all 8 regs, all 4 mem words, all 3 bus fields, all 8 scratch, all 4
// IR bytes, BR, and PC. Carrier per the body/body^k spec; everything else pinned
// (aux rb/temp=0, ctrl preserved, arr/data preserved, mem[2,3]=0, bus/IR/BR/scratch=0).
//
// WHAT STAYS ARGUED vs BVEQ-PROVEN.
//   * BVEQ-PROVEN (whole carrier domain, real netlist): the body function (L1),
//     both skip cases incl. the forward-jump PC landing (L2), and the d=3 static
//     composition ≡ body^k (COMPOSITION).
//   * ARGUED: (i) the carrier is walked with only the low-w bits symbolic (the
//     reachable Flat(w) window) — like vart's carrier-width limiting; out-of-range
//     high bits are pinned 0. (ii) the counter tags are case-split on concrete
//     {0,1} (faithful: at a real iteration boundary each unary tag IS a concrete
//     bit), and only the reachable unary PREFIX combos are enumerated for d=3.
//     (iii) the PROLOGUE (marshal→r0..) and EPILOGUE (unmarshal) legs — anchored
//     by L3, not lemma-covered. (iv) the ctrl register enters concrete, so BR/PC
//     stay concrete throughout — the skip-block never introduces a symbolic PC.
//
// DOES IT REPLACE THE WHOLE-DOMAIN SWEEP? For the loop-control + body semantics:
//   YES for reduce_flat(2,d) — L1+L2+COMPOSITION cover the per-copy relation and
//   its static composition over the whole Flat(2) carrier, and the forward-skip
//   PC control is netlist-asserted. But HONESTLY: this file proves w=2 only; the
//   existing valwidth_reduce T2 (Flat(2) d=3, 1536 inputs) and T3 (Flat(4),
//   393k inputs) sweeps also exercise the MARSHAL/UNMARSHAL round-trip and larger
//   w. The symbolic lemmas retire the LOOP-CONTROL + body-semantics portion of the
//   authority for w=2; they do NOT (yet) retire the Flat(4) sweep nor the
//   marshal/unmarshal legs. A w=4 body lemma is a mechanical extension (the spike
//   shows the ring cost is gate-count, not blow-up) but is not done here.
//
// NO BLOWUP, NO FIELD-ASSERT FAILURE occurred: every peak < 10 nodes, every
// PC-landing (incl. the forward skip) matched the netlist, every field welded.
// The memory ring did NOT need new machinery beyond walking `system_step` whole.
// ══════════════════════════════════════════════════════════════════════════
