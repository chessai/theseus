#![cfg(feature = "heavy-tests")] // HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! TwoBodyRing closure certificate (M-C2 Stage 2a).
//!
//! Proves the two-body value decomposition:
//!   `TwoBodyRing::run == SystemRing::run == eval(system_step)^R`
//! at N = MEM_WORDS = 4, R ∈ {1,2} (fast) / R ∈ {1,2,3} (wide), over
//! strided ≤16/≤256-state samples that satisfy F5's mandatory coverage.
//!
//! Also certifies:
//! - per-body ancilla restoration (run == run_open per body)
//! - direct per-body ancilla check (body_ancilla_mismatches == empty)
//! - structural line-audit (cpu bodies have zero MemState ports,
//!   mem body has zero CpuState ports)
//!
//! # Bounding
//!
//! Default (non-#[ignore]d) tests: ≤16 states, R ≤ 3, N = MEM_WORDS = 4.
//! Wide-strided twins are #[ignore]d (authoritative tier, run via --ignored):
//! ≤256 states, R ≤ 3.

use std::sync::OnceLock;

use theseus_core::{Iso, Type};
use theseus_tisc::ring_run::{
    system_ring, two_body_ring, SystemRing, TwoBodyRing,
    CPU_BODY_N_DATA, MEM_BODY_N_DATA,
};
use theseus_tisc::step::system_step;
use theseus_tisc::types::{
    bus_state_ty, decode_system, encode_system, mem_state_ty, state_ty,
    MEM_WORDS, StateVals,
};

const EVAL_FUEL: u64 = 8_000_000_000;

// ── Instruction encoders (spec §3) ────────────────────────────────

fn r_ins(opcode: u16, rd: u16, rs: u16, rt: u16) -> u16 {
    assert!(opcode < 16 && rd < 8 && rs < 8 && rt < 8);
    (1 << 15) | (opcode << 11) | (rd << 8) | (rs << 5) | (rt << 2)
}

fn i_ins(opc: u16, rd: u16, imm: u8) -> u16 {
    assert!(opc < 8 && rd < 8);
    (opc << 12) | (rd << 9) | (u16::from(imm) << 1)
}

// ── Eval helper ───────────────────────────────────────────────────

fn eval_system_state(name: &str, iso: &Iso, s: &StateVals) -> StateVals {
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, encode_system(s), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    decode_system(&ev)
}

// ── ROM exercising F5 coverage ────────────────────────────────────
//
// F5 mandatory sample coverage:
//   - MEXCH with rd == rt  (addr = IR.rt, rd-rt aliased)
//   - back-to-back MEXCH at R = 2
//   - nonzero reg[rd] vs nonzero Mem[addr]
//   - non-MEXCH op with nonzero IR.rt bits
//   - enable on/off, addr = each of the 4 words (0,1,2,3)
//
// ROM layout:
//   0: MEXCH r2, rt=2 (rd==rt, addr=2)        — F5: rd==rt, addr=2
//   1: MEXCH r3, rt=1 (addr=1)                — F5: back-to-back at R=2 from pc=0
//   2: MEXCH r4, rt=0 (addr=0)                — F5: addr=0
//   3: MEXCH r5, rt=3 (addr=3)                — F5: addr=3
//   4: ADDI r1, 5                             — F5: non-MEXCH (enable off)
//   5: ADD r0, r1, rt=3                       — F5: non-MEXCH with nonzero IR.rt
//   6: SWAPBR r1                              — F5: exercises BR↔reg swap + pc_add_br

fn f5_rom() -> Vec<u16> {
    vec![
        r_ins(0b1001, 2, 0, 2), // 0: MEXCH r2, rt=2 (rd==rt, addr=2)
        r_ins(0b1001, 3, 0, 1), // 1: MEXCH r3, rt=1 (addr=1)
        r_ins(0b1001, 4, 0, 0), // 2: MEXCH r4, rt=0 (addr=0)
        r_ins(0b1001, 5, 0, 3), // 3: MEXCH r5, rt=3 (addr=3)
        i_ins(0, 1, 0x05),      // 4: ADDI r1, 5 (I-format, non-MEXCH)
        r_ins(0b0000, 0, 1, 3), // 5: ADD r0, r1, rt=3 (non-MEXCH, nonzero rt)
        r_ins(0b1000, 1, 0, 0), // 6: SWAPBR r1 (swap BR ↔ r[1])
    ]
}

// ── Wide ROM (authoritative tier) ─────────────────────────────────
//
// Covers ALL 14 non-MEXCH ops at dedicated PCs + MEXCH at every addr
// 0..3 + SWAPBR. Used by the #[ignore]d wide-strided twins.
//
// ROM layout:
//   0:  ADD   r0, r1, rt=3          (R-type opcode 0b0000)
//   1:  SUB   r2, r3, rt=0          (R-type opcode 0b0001)
//   2:  XOR   r4, r5, rt=0          (R-type opcode 0b0010)
//   3:  EXCH  r0, r1                (R-type opcode 0b0011)
//   4:  ROTL  r2, r3, rt=0          (R-type opcode 0b0100)
//   5:  ROTR  r4, r5, rt=0          (R-type opcode 0b0101)
//   6:  CSWAP r0, r1, rt=2          (R-type opcode 0b0110)
//   7:  XCHG  r2, r3, rt=0          (R-type opcode 0b0111)
//   8:  SWAPBR r1                   (R-type opcode 0b1000)
//   9:  MEXCH r2, rt=2 (addr=2)     (R-type opcode 0b1001)
//  10:  MEXCH r3, rt=1 (addr=1)
//  11:  MEXCH r4, rt=0 (addr=0)
//  12:  MEXCH r5, rt=3 (addr=3)
//  13:  ADDI  r1, 5                 (I-type opc 0b000)
//  14:  SUBI  r2, 3                 (I-type opc 0b001)
//  15:  XORI  r3, 0x0F              (I-type opc 0b010)
//  16:  ROTLI r4, 2                 (I-type opc 0b011)
//  17:  ROTRI r5, 1                 (I-type opc 0b100)

fn wide_rom() -> Vec<u16> {
    vec![
        r_ins(0b0000, 0, 1, 3), //  0: ADD r0, r1, rt=3
        r_ins(0b0001, 2, 3, 0), //  1: SUB r2, r3
        r_ins(0b0010, 4, 5, 0), //  2: XOR r4, r5
        r_ins(0b0011, 0, 1, 0), //  3: EXCH r0, r1
        r_ins(0b0100, 2, 3, 0), //  4: ROTL r2, r3
        r_ins(0b0101, 4, 5, 0), //  5: ROTR r4, r5
        r_ins(0b0110, 0, 1, 2), //  6: CSWAP r0, r1, rt=2
        r_ins(0b0111, 2, 3, 0), //  7: XCHG r2, r3
        r_ins(0b1000, 1, 0, 0), //  8: SWAPBR r1
        r_ins(0b1001, 2, 0, 2), //  9: MEXCH r2, rt=2 (addr=2)
        r_ins(0b1001, 3, 0, 1), // 10: MEXCH r3, rt=1 (addr=1)
        r_ins(0b1001, 4, 0, 0), // 11: MEXCH r4, rt=0 (addr=0)
        r_ins(0b1001, 5, 0, 3), // 12: MEXCH r5, rt=3 (addr=3)
        i_ins(0, 1, 0x05),      // 13: ADDI r1, 5
        i_ins(1, 2, 0x03),      // 14: SUBI r2, 3
        i_ins(2, 3, 0x0F),      // 15: XORI r3, 0x0F
        i_ins(3, 4, 0x02),      // 16: ROTLI r4, 2
        i_ins(4, 5, 0x01),      // 17: ROTRI r5, 1
    ]
}

// ── Cached contexts ───────────────────────────────────────────────

struct Ctx {
    two: TwoBodyRing,
    sys: SystemRing,
    iso: Iso,
}

fn ctx() -> &'static Ctx {
    static CTX: OnceLock<Ctx> = OnceLock::new();
    CTX.get_or_init(|| {
        let rom = f5_rom();
        let two = two_body_ring(&rom);
        let sys = system_ring(&rom);
        let iso = system_step(&rom);
        Ctx { two, sys, iso }
    })
}

fn wide_ctx() -> &'static Ctx {
    static WIDE_CTX: OnceLock<Ctx> = OnceLock::new();
    WIDE_CTX.get_or_init(|| {
        let rom = wide_rom();
        let two = two_body_ring(&rom);
        let sys = system_ring(&rom);
        let iso = system_step(&rom);
        Ctx { two, sys, iso }
    })
}

// ── F5 sample states ─────────────────────────────────────────────
//
// Coverage (cross-product of ROM entries × initial state patterns):
//   - enable on: pc=0..3 (all MEXCH)
//   - enable off: pc=4 (ADDI), pc=5 (ADD)
//   - addr = each of 4 words: pc=0→addr2, pc=1→addr1, pc=2→addr0, pc=3→addr3
//   - nonzero reg[rd]: r2=0xAA, r3=0x55, r4=0x01, r5=0x80
//   - nonzero Mem[addr]: all mem words nonzero
//   - MEXCH with rd==rt: pc=0 has rd=rt=2
//   - back-to-back MEXCH at R=2: pc=0 → MEXCH addr=2, then pc=1 → MEXCH addr=1
//   - non-MEXCH with nonzero IR.rt bits: pc=5 (ADD rt=3)

fn f5_sample_states() -> Vec<StateVals> {
    let mut states = Vec::new();
    // Strided across PCs that exercise all ROM entries + one out-of-ROM
    for &pc in &[0u8, 1, 2, 3, 4, 5, 6, 7] {
        let mut mem = [0u8; MEM_WORDS];
        for (i, m) in mem.iter_mut().enumerate() {
            *m = 0x42u8.wrapping_add((i as u8).wrapping_mul(0x31));
        }
        states.push(StateVals {
            regs: [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
            pc,
            br: 0,
            ir: 0,
            scratch: [0; 8],
            mem,
        });
    }
    // Additional state with different reg/mem pattern for rd==rt aliasing:
    // r2=0xCC, Mem[2]=0xBB → MEXCH exchanges them
    let mut mem = [0u8; MEM_WORDS];
    mem[2] = 0xBB;
    states.push(StateVals {
        regs: [0x10, 0x20, 0xCC, 0xDD, 0x40, 0x50, 0x60, 0x70],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem,
    });
    // State where reg[rd]=0 but Mem[addr] nonzero (tests asymmetric exchange)
    let mut mem2 = [0u8; MEM_WORDS];
    mem2[1] = 0xEE;
    states.push(StateVals {
        regs: [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        pc: 1,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: mem2,
    });
    // BR != 0 at SWAPBR entry: exercises BR↔r[1] swap + pc_add_br path
    // BR=3, r[1]=0x05 → after SWAPBR: BR=5, r[1]=3; pc_add_br adds 5
    states.push(StateVals {
        regs: [0x00, 0x05, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
        pc: 6,
        br: 3,
        ir: 0,
        scratch: [0; 8],
        mem: [0x42, 0x73, 0xA4, 0xD5],
    });
    // BR != 0 at a non-SWAPBR (MEXCH) entry: exercises pc_add_br with BR preserved
    states.push(StateVals {
        regs: [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
        pc: 0,
        br: 2,
        ir: 0,
        scratch: [0; 8],
        mem: [0x11, 0x22, 0x33, 0x44],
    });
    assert!(states.len() <= 16, "F5 fast sample exceeds 16 states");
    states
}

// ── Wide-strided sample states (authoritative tier) ──────────────
//
// Genuinely wider than the fast companion: all 18 ROM PCs + out-of-ROM,
// multiple register/mem patterns, BR!=0 with SWAPBR, enable on/off.
// Count is in [64, 256].

fn wide_strided_sample_states() -> Vec<StateVals> {
    let mut states = Vec::new();

    // Base register patterns (4 distinct patterns)
    let reg_patterns: [[u8; 8]; 4] = [
        [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
        [0x10, 0x20, 0xCC, 0xDD, 0x40, 0x50, 0x60, 0x70],
        [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        [0xFF, 0x37, 0x91, 0xB2, 0xE4, 0x1C, 0x6A, 0xD8],
    ];

    // Memory patterns (4 distinct nonzero word-vectors + 1 zero)
    let mem_patterns: [[u8; MEM_WORDS]; 5] = [
        [0x42, 0x73, 0xA4, 0xD5],
        [0x11, 0x22, 0x33, 0x44],
        [0xBB, 0xCC, 0xDD, 0xEE],
        [0x01, 0x80, 0xFF, 0x55],
        [0x00, 0x00, 0x00, 0x00],
    ];

    // All 18 ROM PCs + 2 out-of-ROM PCs
    let pcs: Vec<u8> = (0..=17).chain([19u8, 23]).collect();

    // Cross: each PC × first reg pattern × first mem pattern (20 states)
    for &pc in &pcs {
        states.push(StateVals {
            regs: reg_patterns[0],
            pc,
            br: 0,
            ir: 0,
            scratch: [0; 8],
            mem: mem_patterns[0],
        });
    }

    // Cross: MEXCH PCs (9,10,11,12) × all 4 reg patterns × all 5 mem patterns
    // = 4×4×5 = 80 states (some overlap with above, but ≤256 total is fine)
    for &pc in &[9u8, 10, 11, 12] {
        for rp in &reg_patterns {
            for mp in &mem_patterns {
                states.push(StateVals {
                    regs: *rp,
                    pc,
                    br: 0,
                    ir: 0,
                    scratch: [0; 8],
                    mem: *mp,
                });
            }
        }
    }

    // Non-MEXCH PCs with varied regs/mem (enable on: pc<9; enable off: pc≥13)
    for &pc in &[0u8, 1, 2, 3, 4, 5, 6, 7, 13, 14, 15, 16, 17] {
        for (ri, rp) in reg_patterns[1..].iter().enumerate() {
            states.push(StateVals {
                regs: *rp,
                pc,
                br: 0,
                ir: 0,
                scratch: [0; 8],
                mem: mem_patterns[(ri + 1) % mem_patterns.len()],
            });
        }
    }

    // BR != 0 at SWAPBR (pc=8): exercises BR↔r[1] + pc_add_br
    for &br_val in &[1u8, 3, 7] {
        states.push(StateVals {
            regs: [0x00, 0x05, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
            pc: 8,
            br: br_val,
            ir: 0,
            scratch: [0; 8],
            mem: mem_patterns[0],
        });
    }

    // BR != 0 at non-SWAPBR PCs: exercises pc_add_br with BR preserved
    for &pc in &[0u8, 5, 13] {
        states.push(StateVals {
            regs: reg_patterns[0],
            pc,
            br: 2,
            ir: 0,
            scratch: [0; 8],
            mem: mem_patterns[1],
        });
    }

    // Deduplicate (some cross-products overlap)
    states.sort_by(|a, b| {
        a.pc.cmp(&b.pc)
            .then(a.br.cmp(&b.br))
            .then(a.regs.cmp(&b.regs))
            .then(a.mem.cmp(&b.mem))
    });
    states.dedup();

    let n = states.len();
    assert!(
        (64..=256).contains(&n),
        "wide_strided_sample_states: count {n} not in [64, 256]"
    );
    states
}

// ══════════════════════════════════════════════════════════════════════
// 1. Differential oracle: two_body == system_ring == eval(system_step)^R
// ══════════════════════════════════════════════════════════════════════

#[test]
fn two_body_matches_system_ring_and_eval_oracle() {
    let c = ctx();
    let samples = f5_sample_states();

    for (si, start) in samples.iter().enumerate() {
        let mut oracle = start.clone();
        for r in 1..=2u32 {
            oracle = eval_system_state(
                &format!("oracle/si={si}/r={r}"),
                &c.iso,
                &oracle,
            );

            let two_result = c.two.run(start, r);
            let sys_result = c.sys.run(start, r);

            assert_eq!(
                two_result, oracle,
                "TwoBodyRing.run(s, {r}) != eval^{r} at sample {si} (pc={})",
                start.pc
            );
            assert_eq!(
                sys_result, oracle,
                "SystemRing.run(s, {r}) != eval^{r} at sample {si} (pc={})",
                start.pc
            );
            assert_eq!(
                two_result, sys_result,
                "TwoBodyRing.run != SystemRing.run at sample {si}, r={r}"
            );
        }
    }
}

#[test]
#[ignore = "authoritative wide-strided oracle; run via --ignored"]
fn two_body_matches_eval_oracle_wide_strided() {
    let c = wide_ctx();
    let samples = wide_strided_sample_states();
    for (si, start) in samples.iter().enumerate() {
        let mut oracle = start.clone();
        for r in 1..=3u32 {
            oracle = eval_system_state(
                &format!("wide/si={si}/r={r}"),
                &c.iso,
                &oracle,
            );
            let two_result = c.two.run(start, r);
            let sys_result = c.sys.run(start, r);
            assert_eq!(
                two_result, oracle,
                "TwoBodyRing.run(s, {r}) != eval^{r} at sample {si} (wide strided)"
            );
            assert_eq!(
                sys_result, oracle,
                "SystemRing.run(s, {r}) != eval^{r} at sample {si} (wide strided)"
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 2. Per-body ancilla restoration: run == run_open per body
// ══════════════════════════════════════════════════════════════════════

#[test]
fn per_body_ancilla_restoration() {
    let c = ctx();

    // Assert CPU bodies allocate ancillas (kills vacuous test)
    assert!(
        c.two.pre_ancilla_count() > 0,
        "cpu_pre must allocate ancillas (vacuous test guard)"
    );
    assert!(
        c.two.post_ancilla_count() > 0,
        "cpu_post must allocate ancillas (vacuous test guard)"
    );
    // Do NOT assert on mem_body (Fredkin-only, may allocate none)

    let samples = f5_sample_states();
    for (si, start) in samples.iter().enumerate() {
        for r in 1..=2u32 {
            let closed = c.two.run(start, r);
            let open = c.two.run_open(start, r);
            assert_eq!(
                closed, open,
                "TwoBodyRing: run(s, {r}) != run_open(s, {r}) at sample {si} (pc={}) \
                 — per-body ancilla restoration failed",
                start.pc
            );
        }
    }
}

#[test]
#[ignore = "authoritative wide-strided per-body ancilla restoration; run via --ignored"]
fn per_body_ancilla_restoration_wide_strided() {
    let c = wide_ctx();
    let samples = wide_strided_sample_states();
    for (si, start) in samples.iter().enumerate() {
        for r in 1..=3u32 {
            let closed = c.two.run(start, r);
            let open = c.two.run_open(start, r);
            assert_eq!(
                closed, open,
                "TwoBodyRing (wide): run != run_open at sample {si}, r={r}"
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 2b. Direct per-body ancilla check: body_ancilla_mismatches == empty
// ══════════════════════════════════════════════════════════════════════

#[test]
fn direct_per_body_ancilla_check() {
    let c = ctx();
    let samples = f5_sample_states();
    for (si, start) in samples.iter().enumerate() {
        for r in 1..=3u32 {
            let (pre_mm, mem_mm, post_mm) =
                c.two.body_ancilla_mismatches(start, r);
            assert!(
                pre_mm.is_empty(),
                "cpu_pre ancilla mismatch at sample {si} (pc={}), r={r}: {:?}",
                start.pc, pre_mm
            );
            assert!(
                mem_mm.is_empty(),
                "mem_body ancilla mismatch at sample {si} (pc={}), r={r}: {:?}",
                start.pc, mem_mm
            );
            assert!(
                post_mm.is_empty(),
                "cpu_post ancilla mismatch at sample {si} (pc={}), r={r}: {:?}",
                start.pc, post_mm
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 3. Structural line-audit: zero MemState ports in CPU bodies,
//    zero CpuState ports in mem body
// ══════════════════════════════════════════════════════════════════════

#[test]
fn structural_line_audit() {
    let c = ctx();
    let cpu_bus_ty = Type::prod(state_ty(), bus_state_ty());
    let mem_bus_ty = Type::prod(mem_state_ty(), bus_state_ty());

    // cpu_pre: input_ty == (CpuState, BusMsg), n_data == 184
    let pre_net = c.two.pre().net();
    assert_eq!(
        pre_net.input_ty, cpu_bus_ty,
        "cpu_pre input_ty must be (CpuState, BusMsg) — zero MemState ports"
    );

    // cpu_post: input_ty == (CpuState, BusMsg), n_data == 184
    let post_net = c.two.post().net();
    assert_eq!(
        post_net.input_ty, cpu_bus_ty,
        "cpu_post input_ty must be (CpuState, BusMsg) — zero MemState ports"
    );

    // mem_body: input_ty == (MemState, BusMsg), n_data == 56
    let mem_net = c.two.mem().net();
    assert_eq!(
        mem_net.input_ty, mem_bus_ty,
        "mem_body input_ty must be (MemState, BusMsg) — zero CpuState ports"
    );

    // Verify n_data via the body data widths (already asserted at construction,
    // but the test witnesses it)
    assert_eq!(
        CPU_BODY_N_DATA, 184,
        "CPU_BODY_N_DATA must be 184 (160 CpuState + 24 BusMsg)"
    );
    assert_eq!(
        MEM_BODY_N_DATA, 56,
        "MEM_BODY_N_DATA must be 56 (32 MemState + 24 BusMsg)"
    );

    eprintln!(
        "Structural line-audit passed: cpu_pre/cpu_post have zero MemState ports \
         (input_ty = (CpuState, BusMsg), n_data = {}), mem_body has zero CpuState \
         ports (input_ty = (MemState, BusMsg), n_data = {})",
        CPU_BODY_N_DATA, MEM_BODY_N_DATA
    );
}
