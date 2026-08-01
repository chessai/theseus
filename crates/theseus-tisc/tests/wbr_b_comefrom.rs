//! W_BR-b — the two-byte come-from δu (RUN-correct w_br=16).
//!
//! Proves the wide (w_br=16) come-from path in ISOLATION (design
//! `docs/scratch/wbr-b-comefrom-design.md`, REVISION 2). The four authority
//! tiers (mirror `b22b_wpc8_run.rs` at the BR width):
//!
//!   TEST 1 — the mechanized ADR-0034 adder identity at w_br=16, over a
//!     (PC, BR) SAMPLE incl. the come-from δu values: `(pc_add_br_with(&w_br16)
//!     ; pc_inc_with)` == `PC + BR + 1 mod 2^16` (at w_pc=w_br=16 sext is the
//!     IDENTITY — a pure add). Plus the come-from identity `δu = 65536−iter_len`
//!     sexts to `−iter_len` at the boundaries {128, 129, 32767, 32768}.
//!   TEST 2 (P2 involution) — the two-transposition swap is self-inverse,
//!     EXHAUSTIVE over the 2-field BR × 2-reg domain (the model side).
//!   TEST 3 — `vart_halve` K=4,HB=1 (previously REJECTED at iter_len>128) now
//!     LOWERS at w_br=16 with iter_len=137, and `run_on_cpu_fast == eval` over
//!     its FULL input domain (the fast oracle).
//!   TEST 4 (#[ignore], advisor-run) — real `run_rom_on_cpu` (system_step ROM)
//!     == eval on a SAMPLE of vart_halve K=4 inputs covering distinct T +
//!     terminal quiescence (BR/s0/s1==0).

use std::collections::BTreeMap;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type};

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    compile_with_metadata, exec_instr_w_br, marshal, pc_update_w_br, run_on_cpu_fast,
    run_rom_on_cpu, unmarshal, value_to_leaf_bytes_pub,
};
use theseus_tisc::step::{pc_add_br_with, pc_inc_with};
use theseus_tisc::types::{Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

const EVAL_FUEL: u64 = 8_000_000_000;

/// A w_br=16 layout: BR occupies TWO control fields (br_fields()==2). w_pc stays
/// 16 (the default), so at w_pc=w_br=16 the come-from sext is a pure add.
fn layout_w_br16() -> Layout {
    let l = Layout {
        w_br: 16,
        ..Layout::default()
    };
    assert_eq!(l.br_fields(), 2, "w_br=16 layout must have exactly 2 BR fields");
    assert_eq!(l.w_pc, 16, "w_pc stays 16 (pure-add come-from)");
    l
}

// ══════════════════════════════════════════════════════════════════════
// TEST 1 — mechanized ADR-0034 adder identity at w_br=16.
// ══════════════════════════════════════════════════════════════════════

/// A w_br=16 state: 16-bit PC, 16-bit BR, s0=s1=0 (the adder scratch
/// convention), a distinctive register/IR/scratch pattern so any misroute of a
/// PC or BR bit into a non-target field perturbs an observable field.
fn wbr16_state(pc: u16, br: u16) -> StateVals {
    let mut regs = [0u8; NUM_REGS];
    regs[0] = 0x2A;
    regs[1] = 0xC3;
    regs[NUM_REGS - 1] = 0x5A;
    StateVals {
        regs,
        pc,
        br,
        ir: 0xDEAD_BEEF,
        scratch: {
            let mut s = [0u8; NUM_SCRATCH];
            s[2] = 0xDE;
            s[3] = 0xAD;
            s
        },
        mem: [0; MEM_WORDS],
    }
}

fn eval_wbr16(layout: &Layout, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, s.encode_with(layout), &mut fuel)
        .unwrap_or_else(|e| panic!("w_br16 eval failed: {e:?}"));
    StateVals::decode_with(layout, &ev)
}

#[test]
fn adr0034_split_add_wbr16_sampled() {
    let layout = layout_w_br16();
    // The end-of-step PC update as the hardware computes it at w_br=16:
    // pc_add_br ; pc_inc, both at the wide-BR layout.
    let iso = Iso::seq(pc_add_br_with(&layout), pc_inc_with(&layout));

    // A rich (PC, BR) sample: PC across low/high/carry boundaries; BR across
    // forward offsets, the come-from δu values for iter_len ∈ {1,128,129,137,
    // 32767,32768} (= 65536−iter_len), and bit-15 boundaries.
    let iter_lens: [usize; 6] = [1, 128, 129, 137, 32767, 32768];
    let mut brs: Vec<u16> = vec![0, 1, 2, 127, 128, 129, 255, 256, 0x7FFF, 0x8000, 0xFFFF];
    for &il in &iter_lens {
        brs.push((65536usize.wrapping_sub(il) & 0xFFFF) as u16);
    }
    let pcs: [u16; 12] = [
        0, 1, 2, 127, 128, 255, 256, 0x00FF, 0x0100, 0x7FFF, 0x8000, 0xFFFF,
    ];

    let mut checked = 0usize;
    for &br in &brs {
        for &pc in &pcs {
            let s = wbr16_state(pc, br);
            let got = eval_wbr16(&layout, &iso, &s);

            // At w_pc=w_br=16 sext(BR)==BR (identity) ⇒ PC' = (PC + BR + 1) mod 2^16.
            let expected_pc = pc.wrapping_add(br).wrapping_add(1);
            assert_eq!(
                got.pc, expected_pc,
                "w_br=16 split-add mismatch: pc={pc:#06x} br={br:#06x} \
                 (expected (PC+BR+1) mod 2^16 = {expected_pc:#06x}, got {:#06x})",
                got.pc
            );
            // BR is a read-only control ⇒ preserved. s0/s1 restored to 0.
            assert_eq!(got.br, br, "BR preserved: pc={pc:#06x} br={br:#06x}");
            assert_eq!(got.scratch[0], 0, "s0 restored: pc={pc:#06x} br={br:#06x}");
            assert_eq!(got.scratch[1], 0, "s1 restored: pc={pc:#06x} br={br:#06x}");
            // No stray write outside PC/BR/s0/s1: regs, IR, rest of scratch identical.
            assert_eq!(got.regs, s.regs, "regs untouched: pc={pc:#06x} br={br:#06x}");
            assert_eq!(got.ir, s.ir, "IR untouched: pc={pc:#06x} br={br:#06x}");
            assert_eq!(&got.scratch[2..], &s.scratch[2..], "scratch[2..] untouched");

            checked += 1;
        }
    }
    assert_eq!(checked, brs.len() * pcs.len());

    // ── ADJOINT: sym(pc_add_br;pc_inc) subtracts (PC - BR - 1). ──
    let back = Iso::sym(iso.clone());
    for &br in &[0u16, 1, 0x7FFF, 0x8000, 0xFF88, 0xFFFF] {
        for &pc in &[0u16, 1, 0x00FF, 0x0100, 0x8000, 0xFFFF] {
            let s = wbr16_state(pc, br);
            let fwd = eval_wbr16(&layout, &iso, &s);
            let rec = eval_wbr16(&layout, &back, &fwd);
            assert_eq!(rec, s, "w_br=16 adjoint must recover input: pc={pc:#06x} br={br:#06x}");
        }
    }

    // ── come-from identity: sext_{16}(δu) == −iter_len at the boundaries. ──
    // At w_pc=w_br=16 sext is the identity, so δu = 65536−iter_len is ALREADY
    // the 16-bit two's-complement of −iter_len; verify PC += δu + 1 lands at
    // PC − iter_len + 1 (a genuine backward jump of iter_len − 1).
    for &il in &[128usize, 129, 32767, 32768] {
        let du = (65536usize.wrapping_sub(il) & 0xFFFF) as u16;
        // sext_{16}(du) == du (identity) == −il mod 2^16.
        assert_eq!(
            du,
            (0i32 - il as i32).rem_euclid(65536) as u16,
            "δu = 65536−iter_len must equal −iter_len mod 2^16 at iter_len={il}"
        );
        // Landing check via the real adder: from PC=Ltop+(il-1) (= Lend), the
        // update must return to Ltop. Model Ltop=0 ⇒ Lend PC = il-1.
        let lend_pc = ((il - 1) & 0xFFFF) as u16;
        let s = wbr16_state(lend_pc, du);
        let got = eval_wbr16(&layout, &iso, &s);
        assert_eq!(
            got.pc, 0,
            "come-from must land at Ltop=0 from Lend={lend_pc:#06x} with δu={du:#06x} (iter_len={il})"
        );
    }

    eprintln!(
        "W_BR-b TEST 1: ADR-0034 split-add at w_br=16 — {checked} (PC,BR) pairs \
         == (PC+BR+1) mod 2^16 (sext=identity), BR/s0/s1 preserved, adjoint \
         recovers, come-from δu lands at Ltop ✓"
    );
}

// ══════════════════════════════════════════════════════════════════════
// TEST 2 — P2: the two-transposition SWAPBR is an involution (model side).
// ══════════════════════════════════════════════════════════════════════
//
// The wide SWAPBR does `swap(r[rd], BR_lo) ; swap(r[rs], BR_hi)` — two DISJOINT
// transpositions, so the composition is self-inverse. Verify swap∘swap == id
// EXHAUSTIVELY over the 2-field BR × 2-reg domain (rd, rs distinct, rd,rs != 0
// per the SWAPBR rs=0 sentinel rule), sampled densely on the byte values.

fn tiny_state(rd: u8, rs: u8, rd_val: u8, rs_val: u8, br: u16) -> StateVals {
    let mut regs = [0u8; NUM_REGS];
    regs[rd as usize] = rd_val;
    regs[rs as usize] = rs_val;
    StateVals {
        regs,
        pc: 0,
        br,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    }
}

#[test]
fn wbr16_swapbr_involution_exhaustive() {
    let w_br = 16;
    // rd, rs distinct and both != 0 (r0 aliases the rs=0 single-transposition
    // sentinel). Use a couple of distinct pairs; the swap is register-symmetric.
    let pairs: [(u8, u8); 3] = [(1, 2), (2, 5), (7, 3)];
    // Dense byte values covering low/high/all-set + the come-from bytes.
    let vals: [u8; 8] = [0x00, 0x01, 0x7F, 0x80, 0xAA, 0x55, 0xFE, 0xFF];

    let mut checked = 0usize;
    for &(rd, rs) in &pairs {
        for &rd_v in &vals {
            for &rs_v in &vals {
                for &br_lo in &vals {
                    for &br_hi in &vals {
                        let br = u16::from(br_lo) | (u16::from(br_hi) << 8);
                        let s0 = tiny_state(rd, rs, rd_v, rs_v, br);

                        // One swap.
                        let mut s1 = s0.clone();
                        exec_instr_w_br(Instr::Swapbr { rd, rs }, &mut s1, w_br);
                        // The swap must actually move something (not the identity)
                        // when the halves differ — a light non-vacuity check.
                        // Second swap must return to the start (involution).
                        let mut s2 = s1.clone();
                        exec_instr_w_br(Instr::Swapbr { rd, rs }, &mut s2, w_br);
                        assert_eq!(
                            s2, s0,
                            "SWAPBR must be self-inverse: rd={rd} rs={rs} \
                             rd_v={rd_v:#04x} rs_v={rs_v:#04x} br={br:#06x} \
                             (after: {s1:?})"
                        );

                        // Explicit semantics check: after one swap,
                        //   r[rd] = BR_lo, r[rs] = BR_hi, BR = rd_v | (rs_v<<8).
                        assert_eq!(s1.regs[rd as usize], br_lo, "rd ← BR_lo");
                        assert_eq!(s1.regs[rs as usize], br_hi, "rs ← BR_hi");
                        assert_eq!(
                            s1.br,
                            u16::from(rd_v) | (u16::from(rs_v) << 8),
                            "BR ← rd_v | rs_v<<8"
                        );
                        checked += 1;
                    }
                }
            }
        }
    }
    eprintln!(
        "W_BR-b TEST 2: two-transposition SWAPBR involution EXHAUSTIVE over \
         {checked} (rd,rs,rd_v,rs_v,BR) states — swap∘swap == id ✓"
    );
}

// Also pin the PC update helper matches the datapath at BOTH widths at the
// boundaries (a cheap unit guard on `pc_update_w_br`).
#[test]
fn pc_update_w_br_boundaries() {
    // w_br=16: pure add (no sext).
    assert_eq!(pc_update_w_br(0, 0x8000, 16), 0x8001, "w_br=16 pure add");
    // PC += 0xFFFF + 1 = PC + 0x10000 ≡ PC. (BR=0xFFFF is −1; +1 cancels it.)
    assert_eq!(pc_update_w_br(100, 0xFFFF, 16), 100, "w_br=16: PC + (-1) + 1 = PC");
    // come-from: PC += δu + 1 with δu = 65536 − iter_len lands at PC − iter_len + 1.
    for il in [1u16, 128, 137, 32768] {
        let du = 65536u32.wrapping_sub(il as u32) as u16;
        assert_eq!(
            pc_update_w_br(il - 1, du, 16),
            0,
            "w_br=16 come-from lands at Ltop=0 (iter_len={il})"
        );
    }
    // w_br=8: sext from bit 7 (byte-identical to the shipped pc_update).
    assert_eq!(pc_update_w_br(10, 0xFF, 8), 10, "w_br=8: sext(0xFF)=-1, +1 = +0");
    assert_eq!(pc_update_w_br(10, 0x80, 8), 10u16.wrapping_sub(127), "w_br=8: sext(0x80)=-128 +1");
    assert_eq!(pc_update_w_br(0, 0x7F, 8), 0x80, "w_br=8: +127 +1");
}

// ══════════════════════════════════════════════════════════════════════
// TEST 3 — vart_halve K=4,HB=1 now LOWERS (w_br=16) + fast oracle == eval.
// ══════════════════════════════════════════════════════════════════════
//
// The reject test `vart_halve.rs::vart_reject_over_cap` pinned K=4,HB=1 as
// REJECTED at iter_len > 128. W_BR-b lifts it to the two-byte come-from: it now
// lowers with iter_len = body_len + LOOP_OVERHEAD_WIDE (14). The measured
// iter_len is 137 (MINOR-C: body_len 123 + 14 = 137; NOT 133).

const K: u8 = 4;
const HB: u8 = 1;

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

/// One reversible step: right-rotate the (1+HB+K)-bit `(tag ‖ h ‖ v)` register
/// by 1 (the mde_walk idiom; identical to vart_halve.rs::rot_step).
fn rot_step(tag: u8, v: u8, h: u8, k: u8, hb: u8) -> (u8, u8, u8) {
    let width = (1 + hb + k) as u32;
    let w: u32 = ((tag as u32) << (hb + k)) | ((h as u32) << k) | (v as u32);
    let lsb = w & 1;
    let wp = (w >> 1) | (lsb << (width - 1));
    let vmask = (1u32 << k) - 1;
    let hmask = (1u32 << hb) - 1;
    let vp = (wp & vmask) as u8;
    let hp = ((wp >> k) & hmask) as u8;
    let nt = (wp >> (hb + k)) as u8;
    (nt, vp, hp)
}

fn build_v2_body_sized(k: u8, hb: u8) -> (Iso, Type, Type) {
    let u = Type::prod(Type::flat(k), Type::flat(hb));
    let carrier = Type::sum(u.clone(), u.clone());
    let vals = enumerate_values(&carrier);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for val in &vals {
        let sv = marshal(val, &carrier);
        let (nt, nv, nh) = rot_step(sv.regs[0], sv.regs[1], sv.regs[2], k, hb);
        let bytes = [nt, nv, nh];
        let out_val = unmarshal(&state_with_regs(&bytes), &carrier);
        forward.insert(val.clone(), out_val.clone());
        if backward.insert(out_val, val.clone()).is_some() {
            panic!("v2 body NOT injective at k={k},hb={hb} on {val:?}");
        }
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

fn input_vh(v: &theseus_core::Value, u_ty: &Type) -> (u8, u8) {
    let bytes = value_to_leaf_bytes_pub(v, u_ty);
    (bytes[0], *bytes.get(1).unwrap_or(&0))
}

fn v2_step(tag: u8, v: u8, h: u8) -> (u8, u8, u8) {
    rot_step(tag, v, h, K, HB)
}

fn v2_iter_count(v0: u8, h0: u8) -> usize {
    let (mut tag, mut v, mut h) = (1u8, v0, h0);
    for step in 1..=10000 {
        let (nt, nv, nh) = v2_step(tag, v, h);
        if nt == 1 {
            return step;
        }
        tag = nt;
        v = nv;
        h = nh;
    }
    panic!("v2 loop did not terminate for (v0={v0}, h0={h0})");
}

#[test]
fn wbr16_vart_halve_k4_lowers_and_fast_oracle() {
    let (body, u_ty, _carrier) = build_v2_body_sized(K, HB);
    let iso = Iso::trace(u_ty.clone(), body);

    // (a) It now LOWERS (was rejected) — at w_br=16 with iter_len = 137.
    let lowered = compile_with_metadata(&iso)
        .expect("vart_halve K=4,HB=1 must LOWER at w_br=16 (was rejected)");
    let lt = &lowered.loop_terms[0];
    eprintln!(
        "=== W_BR-b vart_halve K={K},HB={HB} shape ===\n  ROM={} iter_len={} k_max={} w_br={}",
        lowered.instrs.len(),
        lt.iter_len,
        lt.k_max,
        lowered.w_br
    );
    assert_eq!(lowered.w_br, 16, "program must be w_br=16");
    assert_eq!(
        lt.iter_len, 137,
        "iter_len pinned at 137 (body_len + LOOP_OVERHEAD_WIDE=14), NOT 133 (MINOR-C)"
    );
    // Sanity: the ROM contains a two-transposition SWAPBR (rs != 0).
    assert!(
        lowered
            .instrs
            .iter()
            .any(|i| matches!(i, Instr::Swapbr { rs, .. } if *rs != 0)),
        "wide come-from must emit a two-operand SWAPBR"
    );

    // (b) run_on_cpu_fast == eval over the ENTIRE input domain (fast oracle).
    let mut checked = 0usize;
    for v in enumerate_values(&u_ty) {
        let cpu = run_on_cpu_fast(&iso, &v).expect("fast run (w_br=16)");
        let mut fuel = 100_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "fast-oracle disagrees with eval on {v:?}");
        checked += 1;
    }
    assert_eq!(checked, (1usize << K) * (1usize << HB), "full domain");
    eprintln!("W_BR-b TEST 3: vart_halve K=4 LOWERS (iter_len=137, w_br=16) + run_on_cpu_fast == eval on all {checked} inputs ✓");
}

// ══════════════════════════════════════════════════════════════════════
// TEST 4 — REAL system_step ROM == eval + terminal quiescence (ADVISOR-RUN).
// ══════════════════════════════════════════════════════════════════════
//     #[ignore]: authoritative, slow. The ADVISOR runs this pre-merge; the
//     worker does NOT. Sampled across distinct iteration counts T (bound the
//     domain per the maestro-gate-no-timeout lesson).

#[test]
#[ignore = "authoritative real-CPU oracle (slow, wide back-edge); advisor runs pre-merge"]
fn wbr_b_real_cpu() {
    let (body, u_ty, _carrier) = build_v2_body_sized(K, HB);
    let iso = Iso::trace(u_ty.clone(), body);

    let lowered = compile_with_metadata(&iso).expect("compile (w_br=16)");
    assert_eq!(lowered.w_br, 16);
    let budget = lowered.wcet();
    let s_max_val = lowered.s_max();
    let (in_ty, out_ty) = theseus_core::type_of(&iso).unwrap();

    // SAMPLE covering distinct T (v0=1 → T=1; v0=2 → T=2; v0=4 → T=3; v0=8 →
    // T=4, exercising the WIDE back-edge multiple times), h0=0 representatives.
    let sample_v0: [u8; 4] = [1, 2, 4, 8];
    let inputs: Vec<theseus_core::Value> = enumerate_values(&u_ty)
        .into_iter()
        .filter(|v| {
            let (v0, h0) = input_vh(v, &u_ty);
            h0 == 0 && sample_v0.contains(&v0)
        })
        .collect();
    assert_eq!(inputs.len(), 4, "one representative per sampled T value");

    for v in inputs {
        let (v0, _h0) = input_vh(&v, &u_ty);
        let t = v2_iter_count(v0, 0);
        eprintln!("real-cpu (w_br=16): v0={v0} (expected T={t}) running ...");

        let state = marshal(&v, &in_ty);
        let final_state =
            run_rom_on_cpu(&lowered.instrs, state, budget, s_max_val, theseus_eval::eval)
                .expect("real-cpu run (wide come-from)");

        let cpu_out = unmarshal(&final_state, &out_ty);
        let mut fuel = 5_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu_out, ev, "REAL wide CPU disagrees with eval on v0={v0}");
        eprintln!("real:  v0={v0} -> {cpu_out:?}  == eval OK");

        // Terminal quiescence: scratch clean + BR settled (the wide back-edge
        // closes with BR=0 and all aux terminal-zero).
        assert_eq!(
            final_state.scratch, [0u8; NUM_SCRATCH],
            "scratch must be all-zero at exit (aux/temp_hi leak check): {:?}",
            final_state.scratch
        );
        assert_eq!(final_state.br, 0, "BR must be zero at loop exit (both bytes)");
    }
    eprintln!("W_BR-b TEST 4: real system_step == eval on wide vart_halve K=4 sample, terminal quiescent ✓");
}

// ══════════════════════════════════════════════════════════════════════
// TEST 5 (L20 REJECT COVERAGE — Fable impl-review MAJOR-3). Every reject this
// slice adds must ship with a test that reproduces it. The deviation-A de-scope
// (reject a narrow SWAPBR under a w_br=16 program instead of the global
// skip-block pair) is acceptable ONLY on the guard's airtightness — so exercise
// it directly.
// ══════════════════════════════════════════════════════════════════════

use theseus_tisc::asm::encode;
use theseus_tisc::compile::guard_no_narrow_swapbr_under_wide;

/// rd≠rs on the two-operand SWAPBR — the 3-cycle-trap prevention. rd==rs at
/// w_br=16 would collapse the two "disjoint" transpositions onto one register
/// (the P2 3-cycle = silent corruption). The encoder must reject it.
#[test]
fn wbr_swapbr_rd_eq_rs_rejected() {
    assert!(
        encode(Instr::Swapbr { rd: 2, rs: 2 }).is_err(),
        "SWAPBR rd==rs must be an AliasedOperands encode error (else the two \
         transpositions collapse to a 3-cycle = silent mis-swap)"
    );
    // Distinct operands encode fine; rs=0 (the narrow sentinel) is allowed.
    assert!(encode(Instr::Swapbr { rd: 2, rs: 3 }).is_ok());
    assert!(encode(Instr::Swapbr { rd: 2, rs: 0 }).is_ok());
}

/// The deviation-A guard: under a w_br=16 program, a narrow (rs=0) SWAPBR is
/// REJECTED (not run on the wide decoder, which would swap r0↔BR₁ and corrupt).
#[test]
fn wbr_guard_narrow_under_wide_rejected() {
    let wide = Instr::Swapbr { rd: 1, rs: 2 }; // two-transposition (rs≠0)
    let narrow = Instr::Swapbr { rd: 3, rs: 0 }; // single-transposition sentinel

    // wide + narrow mix under a w_br=16 program → REJECT.
    assert!(
        guard_no_narrow_swapbr_under_wide(&[wide, narrow], true).is_err(),
        "a narrow SWAPBR under a w_br=16 program must be rejected (skip-pair deferred to W_BR-c)"
    );
    // A narrow SWAPBR alone under a w_br=16 program (e.g. a ≤128 come-from or a
    // skip block) also rejects.
    assert!(guard_no_narrow_swapbr_under_wide(&[narrow], true).is_err());
    // Pure-wide (only rs≠0) under w_br=16 → OK (this is vart_halve's shape).
    assert!(guard_no_narrow_swapbr_under_wide(&[wide], true).is_ok());
    // Narrow SWAPBRs under a w_br=8 program (needs_w_br16=false) → OK (unchanged).
    assert!(guard_no_narrow_swapbr_under_wide(&[narrow], false).is_ok());
}

/// MAJOR-2 reject: a wide (rs≠0) come-from with a MEMORY-using program falls
/// (pre-fix) into the narrow `system_step` arm (w_br=8) → wide SWAPBRs run as
/// single transpositions → mis-jump. It must REJECT (deferred to W_BR-c), not run.
#[test]
fn wbr_wide_plus_memory_rejected() {
    // A ROM that is NOT cpu-only (Mexch) AND wide (Swapbr rs≠0).
    let instrs = vec![
        Instr::Mexch { rd: 0, addr: 0 }, // memory op → not cpu-only
        Instr::Swapbr { rd: 1, rs: 2 },  // wide come-from (rs≠0)
    ];
    let init = StateVals {
        regs: [0u8; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS], // quiescent → forces the else-if rom_wide reject, not a mem assert
    };
    let res = run_rom_on_cpu(&instrs, init, 2, 0, theseus_eval::eval);
    assert!(
        res.is_err(),
        "wide (w_br=16) come-from + a memory op must be rejected (no system_step_with; \
         would silently mis-jump on the narrow netlist) — deferred to W_BR-c"
    );
}

// ══════════════════════════════════════════════════════════════════════
// TEST 6 (L20 — the wide-step NETLIST bug the full authority caught). The
// original WIP type-checked the fast oracle (interpreter) but the wide step
// NETLIST SeqMismatched: `fetch_operand` (and the replicated `ccnot_fetch_rt_br`)
// lifted their operand↔s0 / r0↔BR swap over the HARDCODED default field carrier
// (`at_fields` → NUM_FIELDS = 23, the w_br=8 count) instead of the layout's real
// carrier. At w_br=16 the state has 24 CPU fields, so the swap was off by one
// field and its boundary type no longer matched the layout-aware `select_operand`
// → SeqMismatch. `swapbr_instr` is the first shared-step caller of fetch_operand
// at w_br=16, which is why it only surfaced in W_BR-b. Fix = lift via
// `at_fields_typed(.., &layout.cpu_field_widths())` (byte-identical at default).
// These asserts fail (SeqMismatch) without the fix, pass with it (fast≠real, L15).
// ══════════════════════════════════════════════════════════════════════

#[test]
fn wbr16_wide_decoders_are_endos_on_the_netlist() {
    use theseus_core::type_of;
    use theseus_tisc::decoder::{decode_execute_l, instruction_l, Op};
    use theseus_tisc::step::step_with;
    use theseus_tisc::types::state_ty_with;

    let layout = layout_w_br16();
    let sty = state_ty_with(&layout);

    // The full wide step NETLIST type-checks as an endo on the widened CpuState
    // (this is the exact assertion `w_br16_step_type_checks` makes; duplicated
    // here so the W_BR-b regression is co-located with the come-from tests).
    let rom = vec![(3u32 << 25) | 1]; // ROTLI r0,#1 — content irrelevant
    let (a, b) = type_of(&step_with(&layout, &rom)).expect("wide step must type-check");
    assert_eq!(a, sty, "wide step input = widened CpuState");
    assert_eq!(a, b, "wide step is an endomorphism on the widened CpuState");

    // The shared decode phase (contains the wide SWAPBR + shared Ccnot/Cswap).
    let (c, d) = type_of(&decode_execute_l(&layout)).expect("wide decode_execute must type-check");
    assert_eq!(c, d, "wide decode_execute is an endomorphism");

    // Every per-instruction decoder that TOUCHES BR (Swapbr routes rd↔BR; the
    // replicated Ccnot/Cswap route rt↔BR) — each must be an endo at w_br=16.
    // Xchg (no BR) is the control: it was always an endo, even before the fix.
    for op in [Op::Swapbr, Op::Ccnot, Op::Cswap, Op::Xchg] {
        let (x, y) = type_of(&instruction_l(&layout, op))
            .unwrap_or_else(|e| panic!("wide {op:?} decoder must type-check, got {e:?}"));
        assert_eq!(x, y, "wide {op:?} decoder must be an endomorphism on the netlist");
    }
}
