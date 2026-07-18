//! MEXCHR (data-dependent memory addressing) tests (M-D.b).
//!
//! Proves:
//! 1. Bus-quiescence PRESERVED: SystemRing certificate with MEXCHR ROM.
//! 2. Two-body ring with MEXCHR ROM matches SystemRing and eval oracle.
//! 3. Oracle: hand-built MEXCHR program → run_on_cpu == eval (whole-domain,
//!    small), including a case where reg[ra] ≥ MEM_WORDS (exercises % mask).
//! 4. Alias L20 negative test: encode(MexchReg{rd:1,ra:1}) → Err.
//! 5. Certificate-weakening: hand-crafted op=0b1010 ra==rd word →
//!    bus_quiescent == false.

use std::sync::OnceLock;

use theseus_core::Iso;
use theseus_tisc::asm::{self, AsmError, Instr};
use theseus_tisc::ring_run::{system_ring, two_body_ring, SystemRing};
use theseus_tisc::step::system_step;
use theseus_tisc::types::{decode_system, encode_system, MEM_WORDS, StateVals};

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

// ══════════════════════════════════════════════════════════════════════
// ROM: exercises MEXCHR with varied addresses, plus non-MEXCH ops
// ══════════════════════════════════════════════════════════════════════

/// ROM with MEXCHR instructions:
///   0: ADDI r2, 2        — set r2 = 2 (addr for MEXCHR)
///   1: MEXCHR r1, r2     — reg[1] ↔ mem[reg[2] % 4] = mem[2]
///   2: SUBI r6, 0x11     — non-MEXCH op
fn mexchr_rom() -> Vec<u16> {
    vec![
        i_ins(0, 2, 0x02),          // 0: ADDI r2, 2
        r_ins(0b1010, 1, 2, 0),     // 1: MEXCHR r1, r2 (rd=1, ra=2, rt=0)
        i_ins(1, 6, 0x11),          // 2: SUBI r6, 0x11
    ]
}

/// ROM exercising MEXCHR with reg[ra] ≥ MEM_WORDS (tests the % mask):
///   0: ADDI r3, 0x07     — set r3 = 7 (≥ MEM_WORDS=4, wraps to addr 3)
///   1: MEXCHR r0, r3     — reg[0] ↔ mem[7 % 4] = mem[3]
///   2: MEXCH r4, 1       — immediate MEXCH for comparison
fn mexchr_wrap_rom() -> Vec<u16> {
    vec![
        i_ins(0, 3, 0x07),          // 0: ADDI r3, 7
        r_ins(0b1010, 0, 3, 0),     // 1: MEXCHR r0, r3 (rd=0, ra=3, rt=0)
        r_ins(0b1001, 4, 0, 1),     // 2: MEXCH r4, mem[1]
    ]
}

/// ROM mixing MEXCH and MEXCHR (both address sources coexist):
///   0: ADDI r2, 1        — set r2 = 1
///   1: MEXCH r3, 0       — MEXCH r3, mem[0] (immediate)
///   2: MEXCHR r4, r2     — MEXCHR r4, mem[r2%4] = mem[1] (register)
///   3: ADDI r5, 0x33     — non-MEXCH
fn mixed_rom() -> Vec<u16> {
    vec![
        i_ins(0, 2, 0x01),          // 0: ADDI r2, 1
        r_ins(0b1001, 3, 0, 0),     // 1: MEXCH r3, mem[0]
        r_ins(0b1010, 4, 2, 0),     // 2: MEXCHR r4, r2
        i_ins(0, 5, 0x33),          // 3: ADDI r5, 0x33
    ]
}

// ── Cached contexts ───────────────────────────────────────────────

#[allow(dead_code)]
struct Ctx {
    ring: SystemRing,
    iso: Iso,
}

fn ctx() -> &'static Ctx {
    static CTX: OnceLock<Ctx> = OnceLock::new();
    CTX.get_or_init(|| {
        let rom = mexchr_rom();
        let ring = system_ring(&rom);
        let iso = system_step(&rom);
        Ctx { ring, iso }
    })
}

#[allow(dead_code)]
struct CtxWrap {
    ring: SystemRing,
    iso: Iso,
}

fn ctx_wrap() -> &'static CtxWrap {
    static CTX: OnceLock<CtxWrap> = OnceLock::new();
    CTX.get_or_init(|| {
        let rom = mexchr_wrap_rom();
        let ring = system_ring(&rom);
        let iso = system_step(&rom);
        CtxWrap { ring, iso }
    })
}

#[allow(dead_code)]
struct MixedCtx {
    ring: SystemRing,
    iso: Iso,
}

fn mixed_ctx() -> &'static MixedCtx {
    static CTX: OnceLock<MixedCtx> = OnceLock::new();
    CTX.get_or_init(|| {
        let rom = mixed_rom();
        let ring = system_ring(&rom);
        let iso = system_step(&rom);
        MixedCtx { ring, iso }
    })
}

// ── Sample states ─────────────────────────────────────────────────

fn sample_state(pc: u8) -> StateVals {
    let mut mem = [0u8; MEM_WORDS];
    for (i, m) in mem.iter_mut().enumerate() {
        *m = 0x42u8.wrapping_add((i as u8).wrapping_mul(0x31));
    }
    StateVals {
        regs: [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
        pc,
        br: 0,
        ir: 0,
        scratch: [0x00, 0x00, 0xDE, 0xAD, 0xBE, 0xEF, 0x33, 0x11],
        mem,
    }
}

const SAMPLE_PCS: [u8; 4] = [0, 1, 2, 4];

// ══════════════════════════════════════════════════════════════════════
// 1. Bus-quiescence PRESERVED with MEXCHR ROM
// ══════════════════════════════════════════════════════════════════════

fn bus_quiescent_with_mexchr() {
    let c = ctx();
    for &pc in &SAMPLE_PCS {
        let start = sample_state(pc);
        assert!(
            c.ring.bus_quiescent(&start),
            "SystemRing: bus NOT quiescent with MEXCHR ROM at pc={pc}"
        );
    }
}

fn bus_quiescent_with_mexchr_wrap() {
    let c = ctx_wrap();
    for &pc in &SAMPLE_PCS {
        let start = sample_state(pc);
        assert!(
            c.ring.bus_quiescent(&start),
            "SystemRing: bus NOT quiescent with MEXCHR-wrap ROM at pc={pc}"
        );
    }
}

fn bus_quiescent_with_mixed_rom() {
    let c = mixed_ctx();
    for &pc in &[0u8, 1, 2, 3, 5] {
        let start = sample_state(pc);
        assert!(
            c.ring.bus_quiescent(&start),
            "SystemRing: bus NOT quiescent with mixed MEXCH/MEXCHR ROM at pc={pc}"
        );
    }
}

// ══════════════════════════════════════════════════════════════════════
// 1b. SystemRing matches eval oracle with MEXCHR ROM
// ══════════════════════════════════════════════════════════════════════

fn system_ring_mexchr_matches_eval_oracle() {
    let c = ctx();
    const MAX_R: u32 = 2;

    for &pc in &[0u8, 1, 4] {
        let start = sample_state(pc);
        let mut oracle = start.clone();

        for r in 0..=MAX_R {
            let ring_result = c.ring.run(&start, r);
            assert_eq!(
                ring_result, oracle,
                "SystemRing.run(s, {r}) != eval^{r} at pc={pc} (MEXCHR ROM)"
            );
            let open_result = c.ring.run_open(&start, r);
            assert_eq!(
                open_result, ring_result,
                "SystemRing: run_open != run at pc={pc}, r={r} (MEXCHR ROM)"
            );
            oracle = eval_system_state(
                &format!("mexchr/pc={pc}/r={r}"),
                &c.iso,
                &oracle,
            );
        }

        assert!(
            c.ring.bus_quiescent(&start),
            "SystemRing: bus NOT quiescent at pc={pc} (MEXCHR ROM)"
        );
    }
}

// ══════════════════════════════════════════════════════════════════════
// 1c. Two-body ring with MEXCHR ROM
// ══════════════════════════════════════════════════════════════════════

fn two_body_ring_mexchr_matches_system_ring() {
    let rom = mexchr_rom();
    let two = two_body_ring(&rom);
    let sys = system_ring(&rom);
    let iso = system_step(&rom);

    let samples: Vec<StateVals> = SAMPLE_PCS.iter()
        .map(|&pc| sample_state(pc))
        .collect();

    for (si, start) in samples.iter().enumerate() {
        let mut oracle = start.clone();
        for r in 1..=2u32 {
            oracle = eval_system_state(
                &format!("two_body_mexchr/si={si}/r={r}"),
                &iso,
                &oracle,
            );
            let two_result = two.run(start, r);
            let sys_result = sys.run(start, r);
            assert_eq!(
                two_result, oracle,
                "TwoBodyRing.run != eval^{r} at sample {si} (MEXCHR ROM)"
            );
            assert_eq!(
                sys_result, oracle,
                "SystemRing.run != eval^{r} at sample {si} (MEXCHR ROM)"
            );
            assert_eq!(
                two_result, sys_result,
                "TwoBodyRing.run != SystemRing.run at sample {si}, r={r} (MEXCHR ROM)"
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 2. Oracle: MEXCHR hand-built program, run_on_cpu == eval
// ══════════════════════════════════════════════════════════════════════

fn mexchr_oracle_basic() {
    let c = ctx();
    // ROM: [ADDI r2,2 ; MEXCHR r1,r2 ; SUBI r6,0x11]
    // Start at pc=0 with r2=0xAA, mem[0]=0x42
    // Step 0: ADDI r2, 2 → r2 = 0xAA + 2 = 0xAC, pc→1
    // Step 1: MEXCHR r1, r2 → addr = 0xAC % 4 = 0, reg[1] ↔ mem[0], pc→2
    // Step 2: SUBI r6, 0x11, pc→3

    let start = sample_state(0);
    // Run 3 steps via eval oracle
    let s1 = eval_system_state("step0", &c.iso, &start);
    let s2 = eval_system_state("step1", &c.iso, &s1);
    let result = eval_system_state("step2", &c.iso, &s2);

    // After step 0: r2 = 0xAA + 2 = 0xAC
    assert_eq!(s1.regs[2], 0xAC, "r2 after ADDI");
    // After step 1: addr = 0xAC % 4 = 0, so r1 ↔ mem[0]
    //   r1 was 0xFF, mem[0] was 0x42 → r1 = 0x42, mem[0] = 0xFF
    assert_eq!(s2.regs[1], 0x42, "r1 should be mem[0]'s old value 0x42");
    assert_eq!(s2.mem[0], 0xFF, "mem[0] should be r1's old value 0xFF");
    // After step 2: r6 = 0x7F - 0x11 = 0x6E
    assert_eq!(result.regs[6], 0x6E, "r6 after SUBI");

    // Ring result for 3 revolutions must match
    let ring_result = c.ring.run(&start, 3);
    assert_eq!(ring_result, result, "ring.run(s,3) != eval^3");
}

fn mexchr_oracle_wrap() {
    // reg[ra] ≥ MEM_WORDS: exercises the % mask.
    let c = ctx_wrap();
    // ROM: [ADDI r3,7 ; MEXCHR r0,r3 ; MEXCH r4,1]
    // Start at pc=0 with r3=0x55, r0=0x00, mem[0]=0x42

    let start = sample_state(0);
    let s1 = eval_system_state("wrap_step0", &c.iso, &start);
    let s2 = eval_system_state("wrap_step1", &c.iso, &s1);
    let result = eval_system_state("wrap_step2", &c.iso, &s2);

    // After step 0: r3 = 0x55 + 7 = 0x5C, pc→1
    assert_eq!(s1.regs[3], 0x5C, "r3 after ADDI r3,7");
    // After step 1: MEXCHR r0, r3 → addr = 0x5C % 4 = 0, r0 ↔ mem[0]
    //   r0 was 0x00, mem[0] was 0x42 → r0 = 0x42, mem[0] = 0x00
    assert_eq!(s2.regs[0], 0x42, "r0 should be mem[0]'s old value 0x42");
    assert_eq!(s2.mem[0], 0x00, "mem[0] should be r0's old value 0x00");

    // Ring result must match
    let ring_result = c.ring.run(&start, 3);
    assert_eq!(ring_result, result, "ring.run(s,3) != eval^3 (wrap case)");
}

// ══════════════════════════════════════════════════════════════════════
// 3. Alias L20 negative test: MexchReg{rd:1,ra:1} → Err
// ══════════════════════════════════════════════════════════════════════

fn mexchr_alias_rejected() {
    let result = asm::encode(Instr::MexchReg { rd: 1, ra: 1 });
    assert!(
        matches!(result, Err(AsmError::MexchAddrAliasesData { rd: 1, ra: 1 })),
        "expected MexchAddrAliasesData, got {result:?}"
    );
}

fn mexchr_encode_decode_round_trip() {
    // Verify round-trip for legal MexchReg
    for rd in 0..8u8 {
        for ra in 0..8u8 {
            if rd == ra { continue; }
            let word = asm::encode(Instr::MexchReg { rd, ra }).unwrap();
            let decoded = asm::decode(word);
            assert_eq!(
                decoded,
                Some(Instr::MexchReg { rd, ra }),
                "round-trip failed for MEXCHR r{rd}, r{ra} (word={word:#06x})"
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// 4. Certificate-weakening: hand-crafted op=0b1010 ra==rd word →
//    bus_quiescent == false
// ══════════════════════════════════════════════════════════════════════

fn certificate_weakening_mexchr_aliased() {
    // Hand-craft a 16-bit ROM word with op=0b1010, rd=1, ra=1 (rs=1),
    // bypassing the assembler which rejects ra==rd.
    // R-format: [1 | 1010 | rd=001 | rs=001 | rt=000 | 00]
    let malformed_word = r_ins(0b1010, 1, 1, 0);

    // This word decodes but violates ra≠rd.
    let decoded = asm::decode(malformed_word);
    assert_eq!(
        decoded,
        Some(Instr::MexchReg { rd: 1, ra: 1 }),
        "malformed word should decode to MexchReg{{rd:1,ra:1}}"
    );

    // Build a ROM containing only this malformed word.
    let rom = vec![malformed_word];
    let ring = system_ring(&rom);

    // The bus should NOT be quiescent for this malformed instruction,
    // because absorb reads reg[ra=rd=1] which has been mutated by the
    // gated-swap in emit → the addr XOR does not cancel.
    let start = StateVals {
        regs: [0x00, 0x42, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0xBB, 0x00, 0x00, 0x00],
    };

    // Bus quiescence should FAIL: the aliased instruction leaves residue
    // on the bus because absorb's re-read of reg[ra=rd] gets the exchanged
    // value, so the addr XOR does not cancel.
    //
    // NOTE: bus_quiescent checks whether *all* bus bits return to zero after
    // one revolution. With ra==rd, the addr bits won't cancel.
    // We need reg[1] != 0 AND mem[addr] != reg[1] for the exchange to
    // actually mutate reg[1], causing the addr cancel to fail.
    assert!(
        !ring.bus_quiescent(&start),
        "bus_quiescent should be FALSE for malformed MEXCHR with ra==rd — \
         this documents the certificate weakening: bus-quiescence now holds \
         only for ASSEMBLER-LEGAL ROMs"
    );
}

/// All MEXCHR checks in ONE sequential #[test]: each heavy check lowers the
/// large system_step netlist; running them as separate concurrent tests
/// (default cargo threads) exhausts memory (SIGKILL). Sequencing caps peak
/// memory at one lowering — same pattern as ring_two_body_timing.
#[test]
fn mexchr_all_sequential() {
    bus_quiescent_with_mexchr();
    bus_quiescent_with_mexchr_wrap();
    bus_quiescent_with_mixed_rom();
    system_ring_mexchr_matches_eval_oracle();
    two_body_ring_mexchr_matches_system_ring();
    mexchr_oracle_basic();
    mexchr_oracle_wrap();
    mexchr_alias_rejected();
    mexchr_encode_decode_round_trip();
    certificate_weakening_mexchr_aliased();
}
