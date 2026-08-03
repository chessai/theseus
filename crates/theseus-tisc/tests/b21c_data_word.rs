#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge.
//! B-2.1c -- per-program reduced DATA-WORD width (`w_data`), POC slice.
//!
//! This slice makes `state_ty_with` MIXED-width (DATA fields = `w_data`,
//! CONTROL fields = `w_ctl = 8`, per Option A) and threads a per-field width
//! vector through the field-routing machinery (`addressing::*` + `on_fields_l`
//! + the ALU-core operand/predicate offsets + the `ctrl_word_swap` reg↔s0
//! boundary). The tests here are the FAST correctness authority for the
//! narrowed `w_data=4` datapath.
//!
//! (Fast-correctness authority -- the advisor runs the full heavy suite +
//! the byte-identity `poc_step_net_bit_identical` golden separately.)

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::run_rom_on_cpu_with;
use theseus_tisc::types::{Layout, StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

/// The POC layout: 4-bit data words, 8-bit control region, 4 registers.
fn poc_layout() -> Layout {
    Layout {
        w_data: 4,
        num_regs: 4,
        ..Layout::default()
    }
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

// ══════════════════════════════════════════════════════════════════════
// 1. Mixed-width carrier is really mixed-width (state_ty + field_width)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn state_bits_shrinks_at_w_data_4() {
    let d = Layout::default();
    let poc = poc_layout();
    // Default: every field 8-bit → 24 * 8 = 192.
    assert_eq!(d.state_bits(), d.num_cpu_fields() * 8);
    // POC: 4 regs (w_data=4) + control region. num_cpu_fields shrinks with
    // num_regs; register + s2.. fields are 4-bit, control fields 8-bit.
    let ncf = poc.num_cpu_fields();
    // Recompute expected: Σ field_width.
    let expected: usize = (0..ncf).map(|i| poc.field_width(i)).sum();
    assert_eq!(poc.state_bits(), expected);
    // Registers are DATA (4-bit); s0/s1 CONTROL (8-bit); s2 DATA (4-bit).
    assert_eq!(poc.field_width(poc.reg_field(0)), 4, "r0 = w_data");
    assert_eq!(poc.field_width(poc.scratch_field(0)), 8, "s0 = w_ctl");
    assert_eq!(poc.field_width(poc.scratch_field(1)), 8, "s1 = w_ctl");
    assert_eq!(poc.field_width(poc.scratch_field(2)), 4, "s2 = w_data");
    assert_eq!(poc.field_width(poc.pc_field()), 8, "PC_lo = w_ctl");
    assert_eq!(poc.field_width(poc.br_field()), 8, "BR = w_ctl");
    assert_eq!(poc.field_width(poc.ir_byte_field(0)), 8, "IR = w_ctl");
    // The POC state is strictly smaller than the default state.
    assert!(
        poc.state_bits() < d.state_bits(),
        "w_data=4 state must be smaller: {} vs {}",
        poc.state_bits(),
        d.state_bits()
    );
    eprintln!(
        "state_bits: default={} poc(w_data=4,num_regs=4)={} (num_cpu_fields {} → {})",
        d.state_bits(),
        poc.state_bits(),
        d.num_cpu_fields(),
        ncf
    );
}

// ══════════════════════════════════════════════════════════════════════
// 2. Whole-domain per-op equivalence at w_data=4 -- RETIRED (GO-rollout)
// ══════════════════════════════════════════════════════════════════════
//
// The brute-force reduced-width sweeps here (`whole_domain_{add,xor,sub,exch,
// ccnot,cswap}`, `whole_domain_imm`, `whole_domain_rotl_rotr`,
// `whole_domain_rotli_rotri` -- every point of 16×16 / 16³ run through the REAL
// mixed-width step, the bulk of the ~2.27 h reduced-width sim baseline) are
// RETIRED into `tests/b21c_bveq.rs`. ALL were SINGLE combinational straight-line
// instructions (PC concrete, no BR / no loop → inside eval_bdd's fragment), so
// each is now proved SYMBOLICALLY: one `eval_bdd` walk of `step_with(&poc_layout,
// [instr])` with symbolic 4-bit registers ≡ its mixed-width ripple/xor/rotate
// spec over the WHOLE 2^4-per-operand domain (a superset of the sample grids),
// with a concrete-`theseus_eval` reality-anchor bootstrap + a negative control.
// The mixed-width datapath (field offsets, pred base, barrel stages) is still
// what is walked -- the retirement changes the ORACLE (symbolic whole-domain),
// not the system under test. ~2.27 h portion → ~23 s.
//
// KEPT here (fast, not brute-force sweeps): `state_bits_shrinks_at_w_data_4`,
// `pred_base_is_w_data_plus_w_ctl_not_2_w_data` (structural pins),
// `add_xor_kernel_run_on_cpu_eq_eval_at_w_data_4` (a 3-instr end-to-end
// run_on_cpu==eval on 6 points), and `scratch_operand_ccnot_at_w_data_4`
// (7-point mixed-width reg↔scratch crossbar routing tooth -- the symbolic walk
// is register-only, so this concrete scratch-routing check stays).

// ══════════════════════════════════════════════════════════════════════
// 3. run_on_cpu == eval on the add/xor kernel at w_data=4 (end-to-end)
// ══════════════════════════════════════════════════════════════════════
//
// A multi-instruction add/xor kernel over r0..r3, run on the REDUCED
// (w_data=4, num_regs=4) mixed-width ring, must agree with a 4-bit reference
// interpreter (add mod 16, xor). Includes edge nibbles (0, 0xF, high bit).

/// 4-bit reference semantics for the add/xor kernel (mod 2^4).
fn exec_ref_4bit(prog: &[Instr], init: &[u8; 4]) -> [u8; 4] {
    let mut r = *init;
    for instr in prog {
        match *instr {
            Instr::Add { rd, rs } => {
                r[rd as usize] = (r[rd as usize].wrapping_add(r[rs as usize])) & 0x0F;
            }
            Instr::Xor { rd, rs } => {
                r[rd as usize] ^= r[rs as usize];
            }
            other => panic!("kernel uses only Add/Xor, got {other:?}"),
        }
    }
    r
}

#[test]
fn add_xor_kernel_run_on_cpu_eq_eval_at_w_data_4() {
    // r0 += r1 ; r2 ^= r3 ; r0 += r2
    let prog = vec![
        Instr::Add { rd: 0, rs: 1 },
        Instr::Xor { rd: 2, rs: 3 },
        Instr::Add { rd: 0, rs: 2 },
    ];
    let layout = poc_layout();
    let budget = prog.len();

    // Edge nibbles + a spread of interior values.
    let inputs: [[u8; 4]; 6] = [
        [0x0, 0x0, 0x0, 0x0],
        [0xF, 0xF, 0xF, 0xF],
        [0x8, 0x7, 0xF, 0x1], // high-bit set / max
        [0x1, 0x2, 0x4, 0x8],
        [0xA, 0x5, 0xC, 0x3],
        [0xF, 0x1, 0x9, 0x6],
    ];

    for init4 in inputs {
        let want = exec_ref_4bit(&prog, &init4);
        let init = state_with_regs(&init4);
        let got = run_rom_on_cpu_with(&layout, &prog, init, budget, 0, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("kernel run failed for {init4:?}: {e}"));
        assert_eq!(
            &got.regs[..4],
            &want[..],
            "w_data=4 kernel run_on_cpu != 4-bit eval for init {init4:?}"
        );
        eprintln!("kernel init {init4:?}: run_on_cpu==eval regs[0..4]={:?} ✓", &got.regs[..4]);
    }
}

// ══════════════════════════════════════════════════════════════════════
// 4. Regression: the mixed-width predicate-offset (pred3 = w_data+w_ctl)
// ══════════════════════════════════════════════════════════════════════
//
// The finding the refactor fixes (design Q3/Q6.5): under the OLD uniform
// `pred3(w) = 2·w_data` the ALU core's predicate control would land at wire
// `2·w_data = 8` -- which at w_data=4 is s0's bit 0 (the OPERAND word), NOT
// s1's bit 0 (the predicate). The correct mixed-width predicate base is
// `w_data + w_ctl = 12`. A whole-domain Add that is correct for EVERY (a,b)
// can only hold if the predicate control reads the real s1-bit-0 (which the
// R-format opcode decode sets), so this asserts the specific pred-base fix:
// run Add over the domain AND confirm the byte-identity fingerprint (default)
// is unchanged (2·w_data == w_data+w_ctl at w=8). The whole-domain Add test
// above is the reproducer (it fails if pred3 stays 2·w_data at w_data=4);
// here we pin the exact mixed-width base relation.
#[test]
fn pred_base_is_w_data_plus_w_ctl_not_2_w_data() {
    let poc = poc_layout();
    // The std core addresses [r0(w_data), s0(w_ctl), s1(w_ctl)]: the predicate
    // (s1) base MUST be w_data + w_ctl, and this must differ from 2·w_data
    // whenever w_data != w_ctl -- the exact mixed-width hazard.
    let tgt0 = 0;
    let opd0 = poc.field_width(poc.reg_field(0)); // = w_data
    let pred0 = poc.field_width(poc.reg_field(0)) + poc.field_width(poc.scratch_field(0));
    assert_eq!(tgt0, 0);
    assert_eq!(opd0, poc.w_data, "operand base = w_data");
    assert_eq!(pred0, poc.w_data + poc.w_ctl, "predicate base = w_data + w_ctl");
    assert_ne!(
        pred0,
        2 * poc.w_data,
        "the OLD uniform pred3=2·w_data would MISPLACE the predicate onto s0 \
         (operand word) at w_data<w_ctl -- this is the mixed-width bug"
    );
    // At the default layout the two coincide (byte-identity).
    let d = Layout::default();
    assert_eq!(d.w_data + d.w_ctl, 2 * d.w_data, "coincide at w_data=w_ctl=8");
    eprintln!(
        "pred base: w_data({})+w_ctl({})={} ≠ 2·w_data={} (mixed-width fix pinned) ✓",
        poc.w_data,
        poc.w_ctl,
        pred0,
        2 * poc.w_data
    );
}

// RETIRED (GO-rollout): `whole_domain_imm_at_w_data_4` (Addi/Subi/Xori, 16×16
// each) → `b21c_bveq::b21c_bveq_whole_domain` (symbolic, whole 2^4 domain).

// ══════════════════════════════════════════════════════════════════════
// 5. B-2.1c-2 -- the REMAINING ops whole-domained at w_data=4 -- RETIRED
// ══════════════════════════════════════════════════════════════════════
//
// `whole_domain_{sub,exch,rotl_rotr,rotli_rotri,ccnot,cswap}_at_w_data_4`
// (Sub/Exch 256 pts, Rotl/Rotr/Rotli/Rotri 16×16, Ccnot/Cswap 16³=4096 pts
// each -- the reduced-width hogs) are RETIRED into `b21c_bveq`. Each was a single
// straight-line combinational instruction, now proved symbolically over the
// whole operand domain (Cswap: both control polarities inside one walk; Ccnot:
// the full 4-bit AND product; Rotl/Rotr: both barrel stages). The remaining
// text below documents what the retired section covered:
//
// B-2.1c-1 (the POC) proved Add/Xor/imm at w_data=4. The `shared_alu_block`
// wired-decode path ALREADY builds EVERY op at reduced w_data (`exch_core_at`,
// `rotl_core_at`, `ccnot_core_at`, `cswap_core_at`, Sub = adder adjoint), but
// only Add/Xor/imm were exercised. This section whole-domained the rest against
// the 4-bit arithmetic reference -- the teeth for the untested cores (the
// rotator's log2(w) barrel stages at w_data=4, the 3-operand controls that
// could land on an always-zero s0/s1 high bit, and Sub/Exch).

// ══════════════════════════════════════════════════════════════════════
// 6. B-2.1c-2 -- a SCRATCH operand at w_data=4 (mixed-width crossbar)
// ══════════════════════════════════════════════════════════════════════
//
// Scratch operands use ABSOLUTE indices (s2 = idx 10), decoded via
// `s[idx - num_regs]`. So a scratch operand REQUIRES the default num_regs=8
// (derive_layout pins num_regs=8 whenever a scratch operand is named, else
// idx 10 misroutes to s6). Unlike num_regs, a scratch operand does NOT need to
// pin w_data: s2..s7 are ALREADY w_data in the mixed-width state (Q1), so the
// operand crossbar is w_data-homogeneous (Q2 -- reg↔scratch swaps stay within
// one width). This test uses `w_data=4, num_regs=8` and a scratch rt (s2=idx10)
// to prove the s2 operand routes + computes correctly at the reduced datapath.

/// Layout with a narrow datapath but FULL num_regs (required for absolute
/// scratch-operand indices to route correctly).
fn scratch_op_layout() -> Layout {
    Layout {
        w_data: 4,
        num_regs: 8,
        ..Layout::default()
    }
}

#[test]
fn scratch_operand_ccnot_at_w_data_4() {
    // Ccnot r0, r1, s2  -- rt is a SCRATCH operand (s2 = idx 10), a w_data word
    // in the mixed-width state. The crossbar must route s2 into the operand
    // path and the 3-operand Ccnot core must compute r0 ^= r1 & s2 at 4 bits.
    let prog = vec![Instr::Ccnot { rd: 0, rs: 1, rt: 10 }];
    let layout = scratch_op_layout();
    // Spread of (r0, r1, s2) including edge nibbles.
    let cases: &[(u8, u8, u8)] = &[
        (0x0, 0x0, 0x0),
        (0xF, 0xF, 0xF),
        (0x5, 0xF, 0xA),
        (0xA, 0x6, 0xF),
        (0x8, 0x9, 0x1),
        (0x3, 0xC, 0x7),
        (0xF, 0x8, 0x8),
    ];
    for &(r0, r1, s2) in cases {
        let mut init = state_with_regs(&[r0, r1]);
        init.scratch[2] = s2;
        let got = run_rom_on_cpu_with(&layout, &prog, init, prog.len(), 0, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("scratch-op Ccnot w4 failed r0={r0} r1={r1} s2={s2}: {e}"));
        let want = r0 ^ (r1 & s2);
        assert_eq!(
            got.regs[0], want,
            "Ccnot r0^=r1&s2 (scratch operand) at w4: r0={r0} r1={r1} s2={s2} → got {} want {want}",
            got.regs[0]
        );
        assert_eq!(got.regs[1], r1, "scratch-op Ccnot: r1 preserved");
        assert_eq!(got.scratch[2], s2, "scratch-op Ccnot: s2 (scratch operand) preserved");
        eprintln!("scratch-op Ccnot: r0={r0} r1={r1} s2={s2} → r0={} == r0^(r1&s2) ✓", got.regs[0]);
    }
}
