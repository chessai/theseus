# M-D.b design — data-dependent memory addressing (LOAD/STORE by register)

Status: **DESIGN — pre-review.** Canonical branch `chessai/theseus-v2`, tip `a8b8fcc`
(M-D.a + alloc1 + alloc2 merged). Grounded in a full substrate read (file:line inline).

## Scope (SPLIT from the original md-design.md M-D.b)
M-D.b = **data-dependent addressing ONLY**: a memory-exchange instruction whose address
comes from a REGISTER value at runtime, not a compile-time immediate. **MEM_WORDS widening
is DEFERRED** — the substrate research shows it is separable (a `MEM_WORDS`/`MEM_WIDTH`
flip) and risks the eval-oracle OOM/timeout that pinned N=4 (`types.rs:34-36`); it belongs
with M-D.c (heap) when more memory is actually needed. This task stays at N=4.

## The key facts (from substrate research)
- The 1-of-N memory select tree (`select_mem_on_bus_n`, decoder.rs:922) is **already keyed
  on `BusMsg.addr`** (Fredkin control = `BUS_ADDR_FIELD`, pass-through, ancilla-clean for
  ANY bus-addr source). So runtime addressing of memory already works — the ONLY gap is
  how `BusMsg.addr` is populated.
- Today emit copies `BusMsg.addr ^= IR.rt` (decoder.rs:1003-1015, `n_addr =
  MEM_WIDTH.addr_bits()` bits). IR.rt is stable across the transaction (emit/mem/absorb
  don't touch IR), so `absorb = sym(emit)` re-XORs the same bits → `BusMsg.addr` returns
  to 0 → **bus-quiescence** (the Stage-1b closure).
- **The crux (reversibility):** if emit instead sources `BusMsg.addr ^= reg[addr_reg]`,
  then `absorb = sym(emit)` cancels **iff `reg[addr_reg]` holds the same value in absorb as
  in emit**. The mem phase (phase 4) touches only `(Mem,Bus)` — NOT CpuState registers — so
  a register `addr_reg` DISTINCT from the data register `rd` is stable across emit→mem→
  absorb ⇒ cancellation holds ⇒ bus-quiescence preserved. **But if `addr_reg == rd`, the
  gated-swap loads a NEW value into `reg[rd]` during mem, so at absorb `reg[addr_reg=rd]`
  no longer holds the address, the XOR does not cancel, `BusMsg.addr ≠ 0` at the seam →
  the closure certificate FAILS.**
- So the design is forced: **enforce `addr_reg ≠ rd`** (a hard alias rejection), and the
  existing `absorb=sym(emit)` + select tree give a data-dependent LOAD/STORE that preserves
  zero-garbage closure with no new closure machinery.

## The instruction
Add a register-sourced memory exchange (R-format, one free opcode — 6 R-format free,
`0b1010`..; note ADR-0046 §7's "7 R-format" is stale, MEXCH took `0b1001`):
```
MEXCHR rd, ra   :   reg[rd] ↔ mem[ reg[ra] mod MEM_WORDS ]      (require ra ≠ rd)
```
- `Instr::MexchReg { rd: u8, ra: u8 }`, encode into a free R-format opcode with `rd`, `ra`
  (in the rs field). Self-inverse (like MEXCH). asm legality: `ra ≠ rd` → new
  `AsmError::MexchAddrAliasesData`; regs < 8.
- Only the low `MEM_WIDTH.addr_bits()` bits of `reg[ra]` are the address (mod N); the select
  tree uses exactly those. (At N=4, 2 bits; a `reg[ra]` value ≥ 4 wraps — document that the
  compiler/programmer keeps the index in range, or mask explicitly.)

## The emit-phase change (the delicate part — the review must probe)
`emit_on_cpu_bus` (decoder.rs:984-1041) currently, per MEXCH, does
`pred ; selr ; [enable_cnot ; addr_cnots(IR.rt) ; gated_swap] ; sym(selr) ; sym(pred)`.
For MEXCHR the address source is `reg[ra]`, not `IR.rt`. Two obligations:
1. **Source `reg[ra]` at a point where it holds the pristine address** — i.e. the
   `addr_cnots` must read `reg[ra]` BEFORE any register permutation (`selr = select_reg`)
   or gated-swap has moved/clobbered it, and `absorb = sym(emit)` must re-read it
   identically. Since `ra ≠ rd` and mem doesn't touch CpuState, `reg[ra]` is invariant
   across the transaction, so reading it symmetrically in emit/absorb cancels. The design
   must place the `reg[ra]` addr-CNOTs so `select_reg(R_RD)` (which routes `reg[rd]`) does
   NOT move the field being read, or source before selr.
2. **Two address sources coexist** — the emit phase must support BOTH the immediate-MEXCH
   (addr from IR.rt) AND MEXCHR (addr from reg[ra]), each predicated on its opcode. The
   simplest realization: emit is a `seq` of two predicated blocks (MEXCH-pred → IR.rt
   source; MEXCHR-pred → reg[ra] source), each self-inverse, so `absorb=sym(emit)` still
   holds per block. Verify the two predicates are mutually exclusive (distinct opcodes) so
   no double-emit.

## Acceptance / obligations
- **Bus-quiescence PRESERVED:** re-run the Stage-1b `SystemRing` certificate
  (`run==run_open==eval^R ∧ bus_quiescent`, tests/ring_system.rs) at N=4 with a ROM using
  MEXCHR — `bus_quiescent` must still hold (the closure survives the new address source).
  This is the load-bearing test.
- **Oracle:** a hand-built program using MEXCHR (store reg to a computed index, load back)
  → `run_on_cpu == eval` whole-domain (small) + a bounded real-CPU sample.
- **Alias class:** `ra == rd` → `AsmError::MexchAddrAliasesData` (asm) + a compile-time
  reject; L20 negative test (`MexchReg{rd:1,ra:1}` → Err). This is not just a lint — it is
  what makes bus-quiescence hold.
- **§6 line-audit:** MEXCHR's emit must still touch zero MemState fields directly (the
  address flows through BusMsg only) — the audit that Stage 1b/2 established.

## Risks the review MUST probe
- **R1 (crux):** does sourcing `BusMsg.addr` from `reg[ra]` (ra≠rd) genuinely preserve
  `absorb=sym(emit)` bus-cancellation, given the `selr`/gated-swap register permutation
  inside emit? Trace the exact field read in emit vs absorb; confirm `reg[ra]` is at the
  same physical field at both reads. If `select_reg(R_RD)` moves `reg[ra]`, the addr-CNOT
  source differs between emit and absorb → cancellation breaks even for ra≠rd.
- **R2:** is `ra ≠ rd` SUFFICIENT for quiescence, or are there other registers the emit/mem
  transaction mutates that `ra` could collide with (e.g. the r0 slot `selr` swaps into,
  AUX_PRED, s0/s1)? Enumerate what emit/mem write and confirm `reg[ra]` (any ra≠rd) is
  untouched.
- **R3:** two coexisting address sources in emit — are the MEXCH and MEXCHR predicates
  mutually exclusive and each self-inverse, so absorb=sym(emit) still zeroes the bus for
  BOTH? Does adding a second predicated block change the ancilla-cleanliness (the Stage-1b
  `ancillas_restored`)?
- **R4:** the `reg[ra] mod N` wrap — a `reg[ra]` value ≥ N addresses a nonexistent word;
  does the select tree handle out-of-range bits gracefully (masks to low addr_bits), and is
  the eval reference consistent with the CPU on wrap?
