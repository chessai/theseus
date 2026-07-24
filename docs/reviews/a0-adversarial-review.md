# Adversarial review — TISC-2 slice A-0 (32-bit word + scratch operand crossbar)

Fresh-context review (Fable), worktree `a0-instr-format` @ `23537c5`. Implementer: Opus 5. **Verdict:
DO NOT MERGE** — two proven BLOCKERs, both in `decode_execute` (which NO fast test exercises → the
green fast gate was insufficient; per L20 the regression tests must run *through* decode_execute).

## BLOCKER-1 — XCHG gate control is IR bit 0 = now the I-format immediate LSB → every odd-imm I-op mis-executes (EMPIRICALLY REPRODUCED)
`decoder.rs:736-765` (`xchg_instr`): the gated swap is controlled by raw IR bit 0 (wire 0 of
`IR_LO_FIELD`), load-bearing on the old invariant "IR[0] is an architecturally-zero pad bit in BOTH
formats" (`decoder.rs:59,733`). A-0 deleted the I-format pad bit — bit 0 is now the imm LSB
(`asm.rs:42`; `negative_guards.rs:63` acknowledges it) — but XCHG was never moved off it. For any
I-word with odd imm: `xchg_instr` swaps `r[rd&7] ↔ s[..]` before the real op runs.
**Proof:** `cargo test -p theseus-tisc --test decoder --features heavy-tests full_decoder_arithmetic`
FAILS: `ADDI r7,#0x01 → got r7=0x86, s0=0x87 ; expected r7=0x88, s0=0`. Blast radius: every
ADDI/SUBI/XORI/ROTLI/ROTRI with odd imm, in decode_execute + both ring paths. **fib uses Addi
imm:0xFF/0x01.** FIX needs a real design decision: there is no longer any IR bit zero in both formats,
so gate XCHG on a 2-control predicate (e.g. `fmt ∧ IR[0]`, bit 31 fmt is routing-stable) or the XCHG
opcode predicate — not a single raw bit.

## BLOCKER-2 — the scratch-addressable crossbar is NOT wired into any instruction; asm accepts scratch operands the datapath silently executes as wrong registers
`select_operand` (`decoder.rs:331-352`) has ZERO callers outside tests. Every instruction still routes
through the 8-way `select_reg`: `fetch_operand` (:570), `with_target` (:580), ccnot rt (:701), xchg
(:742), swapbr (:810), emit brackets (:1222,1268,1285). But `check_operand` (`asm.rs:186-191`) now
accepts 0..16, so: `Ccnot{rd:0,rs:1,rt:10}` (the a0_format "s2" example) **executes as
`Ccnot r0,r1,r2`** (rt&7) — silent wrong answer. `Add{rd:9,rs:1}` passes check_distinct but datapath
sees rd&7=1=rs (the non-injective aliased-ADD `encode` exists to reject). `exec_instr`
(`compile.rs:3344+`) `state.regs[rd as usize]` **panics** on idx 8..15. Three authorities disagree
(asm=legal-scratch, hardware=r(idx&7), fast-sim=crash). The design gate "an op names a scratch operand
AND executes correctly" is NOT met: `scratch_operand_program_and_fetch` only evals the orphaned
`select_operand` iso, never through decode_execute. **The headline A-0 deliverable does not exist in
the datapath.** FIX: route fetch_operand/with_target/ccnot/xchg/swapbr/emit through `select_operand`;
make exec_instr handle scratch indices; make asm/hardware/exec_instr agree.

## MAJOR-3 — "predicate tests only fmt + low 4/3 opcode bits" breaks reserved⇒identity + booby-traps A-6
Safe for CURRENT emitted ROMs (all opcodes ≤0b1011 R / ≤0b100 I → high opcode bits provably 0). BUT
the "reserved opcodes match no predicate ⇒ identity" contract (`decoder.rs:30-35`) is now false for ¾
of the opcode space (R op 16 executes as Add, 27 as Ccnot; I op 8 as Addi); `asm::decode` returns None
for these while the datapath executes them (decode/datapath divergence; worst: R op 25 matches
`r_pred(0b1001)` in the emit block :1219 → bus/mem swap on a "reserved" op). Adding op #16+ (A-6)
silently aliases unless the predicate widens — and widening is NOT trivial: fmt+6 opcode bits = 7
literals violates `pred_compute`'s `assert!((2..=5))` (:449) and collides with CSWAP's s1 bit-4 control
(:434). The s1 convention needs rework before the opcode space can grow.

## MAJOR-4 — no guard against naming s0/s1 (operand idx 8/9) as operands
Design (`a0-instruction-format.md:31`): "s0/s1 stay decoder-reserved". Only XCHG's `sx` path enforces
it (`asm.rs:270`). `check_operand` accepts rd/rs/rt=8 (s0 operand buffer) / 9 (s1 live predicate word)
for every op → once the crossbar is wired, routes the live predicate word into the datapath mid-instr.
Add an `AsmError` for it.

## MINOR
- M-5: `asm::decode` drops imm bits 8..19 (`asm.rs:333`, `&0xFF`) → non-canonical high-imm I-words
  decode to Some instead of None (asymmetric with R spare-bit rejection). Datapath reads only IR_B0, so
  the 20-bit imm is encoding-only headroom — A-2 must touch every imm core.
- M-6: reserved operand idx 16..31 ALIAS k (select_operand ignores bit 4); growing the file to 32 needs
  a 5th tree level — nothing marks it.
- M-7: stale docs now false: `types.rs:5,283` ("20 fields"), `step.rs:4` ("16-bit words"),
  `bveq_netlist.rs:28,488`, `decoder.rs:30-35,59,733`.

## Verified SOUND (attacked, held)
- RegSel per-bit mapping: rd 20..24, rs 15..19, rt 10..14, fmt 31, opcode 25..30, imm 0..7 — all match
  r_word/i_word packing incl. byte-spanning fields; `ir_bit()` correct.
- State layout / u32 completeness: NUM_CPU_FIELDS 22, scratch base 14, fetch core [PC,4×IR,s1],
  fetch_entry XORs all 32 bits, every ROM consumer u32; no leftover u16/0xFFFF; StateVals round-trips;
  step remains a bijection (the defects are semantic, not injectivity losses).

## Required before A-0 returns
Fix BLOCKER-1 + BLOCKER-2 (+ MAJOR-3/4), each with a regression test that runs THROUGH `decode_execute`
(L20); the heavy tier + the design doc's `run_on_cpu == eval` differential oracle must be green.
