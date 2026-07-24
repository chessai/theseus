# Slice B: widen-PC (address space > 256) — DESIGN (draft r1)

**Status:** advisor draft; scoped by code-read during Slice A validation. HIGHEST BAR
(CPU-state layout + PC arithmetic + fetch = control-flow lowering; a bug corrupts every
compiled program). Prerequisite: **Slice A (narrow A-bit fetch) MERGED** — it is what makes
this widening cheap.

## 0. Why we need it
The 2-bit subtractive GCD (the first genuine variable-T loop; pinned recognizer
`gcd2_hist_step` / `gcd2_hist_bytes`) has k_exact=5 and a ~60-instr Bennett body ⇒
**B = 4 + 5·(60+10) = 354**, and B + S_max ≈ 362. The park-safety guard P2
(`compile.rs` ~3273) rejects `B + S_max > 256 = 2^W` because the **u8 PC would wrap**.
So the GCD is blocked ONLY by the 8-bit PC. Widen the PC to hold ≥ 362 ⇒ needs
**A = ⌈log₂ 362⌉ = 9** address bits. (Assessment: 2-bit GCD is the cheapest genuine
variable-T loop; 3-bit needs A=10, 8-bit needs A≈15. Collatz is out — needs native
multiply + arithmetic-shift-right the ISA lacks.)

## 1. The layout constraint that shapes the change
`state_ty()` (types.rs ~169) = a right-nested product of **NUM_CPU_FIELDS = 20 uniform
`word8()` fields**: r0..r7 (0..7), **PC (8)**, BR (9), IR_lo/hi (10/11), s0..s7 (12..19).
`STATE_BITS = NUM_FIELDS * W`; every field is read at bit offset `i*W`
(decode: `8*W` for PC, `9*W` BR, `(12+i)*W` scratch). Registers/words are W=8 bits and we
do NOT want to widen them (that pays the per-bit step_j tax on all register arithmetic).

⇒ Do NOT bump `W`. Instead keep every field `word8` and **add one `PC_hi` word8 field**, so
the PC becomes a 16-bit value split across two byte-fields (only the low A=9 bits are ever
non-zero, guaranteed by the same P2 bound Slice A §7 relies on). Uniform-field layout is
preserved (minimal type-system disruption); only ONE field is added.

## 2. The change (three localized pieces)
1. **State layout (types.rs):** insert `PC_hi` as a CPU field. To keep the existing
   reg/PC/BR/IR/scratch field indices stable (they are referenced widely), APPEND PC_hi as
   the last CPU field (index 20), shifting only `mem_field` base 20→21 and NUM_CPU_FIELDS
   20→21. `StateVals.pc: u8 → u16`; encode/decode push/read the extra byte. Keep
   `state_ty()` all-`word8` (now 21 fields). Re-derive every `i*W` offset that assumed 20
   fields (audit `mem_field`, STATE_BITS consumers, composite/system encode/decode).
   *(Open Q for the design-review: append vs insert-at-9 — append keeps indices stable but
   makes PC non-contiguous in the bitstream; is any code assuming PC_lo/PC_hi adjacency?)*
2. **PC arithmetic (step.rs `pc_inc`, `pc_add_br`):** currently 8-bit over one field
   (`n = 2*W`, carries `1..W`). Make them 9-bit-effective across PC_lo⊕PC_hi[bit0]: carry out
   of PC_lo bit 7 into PC_hi bit 0. The TK zero-ancilla adder extends by one bit position.
   Everything above bit 8 stays zero (P2). `pc_add_br` adds the (8-bit) BR into the 9-bit PC.
3. **Fetch (step.rs `fetch_a`, `pc_pred_a`):** ALREADY narrow (Slice A). Point the A-bit
   compare at the low A bits of the *split* PC (PC_lo bits 0..7 + PC_hi bit 0 for A=9); the
   shared high-zero gate covers PC_hi bits 1..7 (and unused PC_lo none). Per-entry fetch cost
   stays 2(A−1)=16 Toffolis — **unchanged by the widening** (the whole point). Only the shared
   high-zero term grows by the PC_hi high bits (once per step, not per entry).

## 3. Why this is "nearly free" (the Slice A payoff, realized)
Fetch predicate cost is `N·2(A−1) + shared`. A is fixed by B (=9 for the GCD), INDEPENDENT of
how many PC bits exist. Adding PC_hi grows only: the pc_inc/pc_add_br carry chain (a handful of
gates, once/step) + the shared high-zero gate's coverage (once/step). NOT the N× per-entry term.
So step_j rises by low-single-digit %, not the ~+10–15%/bit the naïve full-W-compare widen costs.
**This is the measurement the POC owes: step_j(W=8, A=9 GCD-sized ROM) vs step_j(8-bit PC) — the
delta should be a few %, dominated by the extra pc-arith bit, not the fetch.**

## 4. Soundness / adversarial-review targets
1. PC never exceeds 9 effective bits on a correct program (P2: B+S_max ≤ 2^A; the high-zero gate
   rejects any leak — reuse the Slice A §7 argument, now spanning two fields).
2. pc_inc/pc_add_br carry correctness across the PC_lo→PC_hi field boundary (the classic
   off-by-one: does the carry out of bit 7 land in PC_hi bit 0, and does add_br handle BR near
   the boundary?). Exhaustive test: for all PC in 0..2^A and all BR in 0..256, the split-field
   pc_add_br + pc_inc == the integer `(PC + BR + 1) mod 2^A`.
3. Layout audit: every `i*W` offset, `mem_field`, STATE_BITS, ring-frame width, `validate()`,
   and the golden byte-maps re-derived for 21 fields. (Energy + ring-timing goldens SHIFT — the
   state is one field wider; re-pin with the layout-change explanation.)
4. The whole-machine `run_on_cpu == eval` still green for existing programs (fib/χ/sbox/lfsr) at
   the new layout — proves the added field + wider PC didn't disturb decode/mem/bus.

## 5. Test plan (fast component proofs + one bounded integration)
- FAST exhaustive: split-field `pc_inc`/`pc_add_br` == integer PC update over all (PC<2^A, BR)
  — the analog of `fetch_equiv.rs` (fetch-iso-only, seconds).
- FAST exhaustive: `fetch_a` over the split PC still ≡ full compare at every reachable PC.
- BOUNDED integration: `run_on_cpu == eval` for fib (1–2 inputs) at the new layout + a ROM that
  actually parks above 256 (needs B>256 — a synthetic long straight-line ROM) to exercise the
  wider PC end-to-end. Energy re-pin.

## 6. Delegation note
POC-FIRST (this is an emission/layout change — the one time a hard change skipped POC it failed).
Fresh-context Fable adversarial impl-review MANDATORY before merge (fetch/decoder/layout = highest
bar). Commit-only.
