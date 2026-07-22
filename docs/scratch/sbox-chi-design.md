# TP-5 first nonlinear crypto layer: Keccak-χ, bit-sliced over Flat(8) — DESIGN (draft r1)

> **CORRECTION (post-review, see `docs/reviews/sbox-chi-design-review.md`, verdict SOUND-WITH-CHANGES + a built POC):**
> The §2 claim that the 2-register rolling scratch "returns to 0 (garbage-free)" is **FALSE** — the POC
> proves the two ancillas end holding OLD a0/a1 in 24/32 cases. The 17-instruction in-place χ is
> **garbage-free only UNDER THE ADJOINT** (the reverse run zeroes the ancillas); a forward-only single-χ
> gadget leaves 2 dirty ancilla registers. Slice-1 ships **Rev-A** (in-place χ, honest garbage-free-under-
> adjoint, dirty-ancilla assertion as the L20 regression test); true standalone garbage-free (Rev-B, +~20
> instr χ⁻¹ cleanup) is a deferred follow-up. Also corrected: χ is a *bijection but NOT an involution*
> (reversibility check is the ROM adjoint, not χ∘χ⁻¹); χ is Rust-composed and NEVER enters lowering (no
> recognizer/lower_trace change — even stronger than §3 claims); ancillas live in free registers **r5/r6,
> not Scratch**. The exact verified 17-instr schedule + POC are in the review §2/§7 and
> `docs/scratch/chi_poc.rs.ref`. Measured win: **8× bit-slicing, 5.25e-9 J/row-χ (4.20e-8 J total)**.

**Status:** advisor draft for Fable design-review. Author: advisor (overnight 2026-07-22).
Grounded on an Explore map of the current crypto building blocks (feistel/lfsr/dot_reduce/
valwidth_masked) + the ISA inventory + the BVEQ Flat(8) masked-AND capability.

## 0. Why χ, not the AES S-box (target choice)

The user's goal ("move toward the S-box, cash in BVEQ") is best served by the **smallest
genuinely-cryptographic nonlinear layer that REQUIRES the Flat(8) byte-parallel masked-AND
(`Ccnot`) that the value-width ladder + BVEQ delivered.** That is **Keccak-χ** — the nonlinear
step of SHA-3/Keccak-f:

```
A_i = a_i  XOR  ( (NOT a_{i+1}) AND a_{i+2} )      indices mod 5   (χ on a 5-lane row)
```

Rationale over the full AES S-box:
- **AES S-box** = GF(2⁸) inversion + affine, ~115 AND/XOR gates (Boyar–Peralta). That's a big
  first bite: ~230+ instructions with uncompute, a fiddly hand-composed circuit, and a design
  whose *bulk* is linear-algebra plumbing (the affine matrix) that stresses composition, not the
  BVEQ-delivered nonlinear primitive. High risk for a first delegation.
- **χ** is **one masked-AND + one NOT + one XOR per lane** (5 lanes). It is the *actual*
  nonlinear heart of SHA-3 — not a toy — and every nonlinear gate is exactly the byte-parallel
  `Ccnot` (masked-AND) BVEQ made byte-wide. It is the cleanest possible "first useful program on
  the Flat(8) nonlinear capability."
- χ's **reversibility is non-trivial and interesting** (cyclic read-before-write dependency),
  which is squarely in theseus's wheelhouse and makes a *real* reversible-computing artifact, not
  a straight-line toy.

**AES S-box is the stretch follow-up** once χ lands (it reuses the same bit-sliced lane layout +
gate→instruction mapping, just many more gates + the affine block).

### Honest BVEQ relationship (state this plainly in the review)
χ is COMPOSED from `Xori`/`Ccnot`/`Xor` gate-isos (like `feistel.rs`), so it does **not** exercise
BVEQ's *symbolic-prim recognition* (selection-by-equiv is single-instruction; χ's per-lane step
`a ⊕ (¬b ∧ c)` is a 3-instruction body, not one arm). What χ **requires** is the **Flat(8)
byte-parallel `Ccnot`** — masked-AND at full byte width — which was capped at Flat(4) before the
value-width ladder and whose Flat(8) recognition/representation BVEQ hardened. So: χ is the first
useful program that *needs* what BVEQ delivered, not a test of its recognizer. (A genuine future
cash-in of BVEQ *recognition* would be extending selection-by-equiv to short bodies so a fused
`a ⊕ (¬b ∧ c)` prim is recognized as one symbolic prim — noted as a separate follow-up, NOT in
this slice.)

## 1. Bit-sliced lane layout (where the byte width is used)

χ operates on 5 one-bit lanes `a_0..a_4`. **Bit-slicing:** put each lane in its own Flat(8)
register, where the 8 bits of that register are **8 independent Keccak rows evaluated in
parallel** (SIMD across the byte). One byte-parallel `Ccnot` then computes the same χ gate for all
8 rows at once — this is the classic bit-slicing throughput win and the concrete reason Flat(8)
(not Flat(1)) matters.

- Input:  5 registers r0..r4, each `Flat(8)` = lane i across 8 rows.  Type: `Flat(8)^5`.
- Output: χ applied lane-wise; same 5 registers (in-place bijection).
- Domain per (row, lane-tuple): 2^5 = 32 row-states; whole 5-register domain = 2^40 (too big for
  exhaustive — see §4 verification: per-row whole-domain 32 × sampled lane-packings, like the
  masked-fold test's 69,888-input strategy).

## 2. Reversibility: the cyclic read-before-write problem (the core design point)

χ reads `a_{i+1}, a_{i+2}` (OLD values) while writing `a_i`. Across the 5-cycle there is **no
overwrite order** that keeps all needed old values live (A_0 needs old a_1,a_2; … A_4 needs old
a_0,a_1) — a cyclic dependency. Options:

- **(A) Rolling 2-register scratch (chosen).** Save old `a_0, a_1` into two Scratch slots, then
  compute in ascending i using current-or-saved old values; the last two lanes (A_3 uses old
  a_4,a_0; A_4 uses old a_0,a_1) read the saved copies. This is the standard in-place χ; it uses
  **2 scratch registers**, net-zero garbage (scratch returns to 0 after). Mirrors the mde_spill /
  masked-fold garbage-free discipline (compute-into-temp, uncompute-temp-to-0).
- (B) 5 fresh output registers + uncompute inputs via χ⁻¹ — more registers, more instructions.
- (C) Ancilla-per-gate Bennett style — most garbage/uncompute, rejected.

**Chosen: (A).** Per lane the gate is: `t = ¬a_{i+1}` (Xori 0xFF on a copy), `A_i ^= t & a_{i+2}`
(Ccnot), then uncompute `t` (Xori 0xFF back) so scratch/temp returns to 0. The NOT is realized as
`Xori{imm=0xFF}` (per-bit complement — the ISA HAS this, asm.rs Xori, 0 extra cells). Need one
temp register for `t` plus the 2 rolling-save scratch slots.

**Design-review must check:** the exact save/compute/restore ordering is a reversible-Trace-class
sequencing argument — the highest-bar kind of thing to get subtly wrong (cf. L15). The design
must pin the ordering and prove scratch/temp all return to 0 (garbage-free) AND that the emitted
program computes χ on OLD values for every lane.

## 3. Gate → instruction mapping (per lane, before the in-place scheduling)

`A_i = a_i ⊕ (¬a_{i+1} ∧ a_{i+2})`:
1. `Xori{t, 0xFF}` where t holds a copy of a_{i+1}  →  t = ¬a_{i+1}   (0 cells, per-bit NOT)
2. `Ccnot{A_i, t, a_{i+2}}`                          →  A_i ^= t ∧ a_{i+2}   (byte-parallel masked-AND — THE BVEQ-delivered gate)
3. `Xori{t, 0xFF}` (+ move t back)                   →  uncompute t to 0     (garbage-free)

All three instructions are already recognized (Xori by the 1-reg immediate pattern; **Ccnot by
the byte-parallel 3-reg pattern the value-width ladder + BVEQ made Flat(8)-wide**). So χ lowers
via EXISTING recognizers — no new recognizer/prim, no lower_trace change. (This keeps the change
OFF the daemon control-flow / lowering hot path — lighter review bar than BVEQ, but still L19:
real run_on_cpu == eval authority.)

## 4. Verification plan (L19 authority)

Follow the feistel/valwidth_masked/lfsr template exactly:
- **Reference χ** in Rust (independent 3-line per-lane ref over u8 lanes, 8 rows packed) — NOT a
  re-implementation of the compiled path (avoid the L19 self-checker class).
- **Whole-domain per row:** all 2^5 = 32 row-states, packed identically into all 8 lanes, asserted
  `run_on_cpu(iso) == eval(iso)` AND `== χ_ref`.
- **Discriminating lane-packings:** distinct value per bit-lane (like masked_masked's L314
  discriminating case) so a bug that only shows when the 8 rows differ is caught — the sampled
  273-shape style, bounded (NEVER 2^40).
- **Involution/round check:** χ is a bijection; assert χ then χ⁻¹ (or two rounds' known vectors)
  round-trips, and validate()==Ok (reversible typecheck) + eval^R bus-quiescent.
- **Golden:** pin a Keccak known-answer vector (a standard χ test value) so the crypto is *real*.
- **Energy:** rom_program_energy — χ priced in joules; the bit-slicing win = 8 rows per byte-op
  (report J per row-χ vs J per byte-χ). This is the design's "perf-win POC": the byte-parallel
  Ccnot does 8 χ-gate evaluations for one instruction's energy.

## 5. Deliverable scope (ONE delegation, fast-tier gated, ~90min budget)

- `conformance/programs/00XX-chi.thse` — χ on a 5-lane Flat(8) row (the iso), OR a Rust-built iso
  in `instr.rs`/a test if .thse can't express the scratch scheduling (design-review to decide
  .thse-expressible vs Rust-composed — feistel is Rust-composed instrs; lfsr is .thse `iter`).
- Test `crates/theseus-tisc/tests/chi.rs` — the §4 battery (HEAVY-tier if it runs run_on_cpu on
  the ring; fast per-lane oracle in the fast tier).
- Reference χ + golden Keccak vector.
- Energy line (J/row-χ) — the bit-slicing win.

**Explicitly OUT of scope (follow-ups):** the full AES S-box (affine + GF(2⁸) inversion);
extending selection-by-equiv to short bodies (the genuine BVEQ-recognition cash-in); multi-round
Keccak-f (θ, ρ, π, ι).

## 6. Open questions for Fable design-review
1. Is (A) 2-register rolling scratch the right reversible schedule, or does theseus's Trace/iter
   machinery already give a cleaner in-place χ? Pin the exact instruction order + prove garbage-free.
2. .thse-expressible (via `iter`/`fold` over lanes) vs Rust-composed instrs (feistel-style)? χ's
   cyclic scratch may not fit `iter`'s uniform-body shape — likely Rust-composed like feistel.
3. Verification domain bound: confirm the per-row 32 + discriminating-packing sample has teeth for
   an 8-lane bug without going near 2^40 (gate-no-timeout).
4. Does anything here secretly need a lower_trace or recognizer change? (Expected: NO — pure
   composition of recognized instructions. If YES, it's a higher-bar slice.)
5. Perf-win POC: is "8 row-χ per byte-Ccnot, priced in J" a compelling, measurable win, or should
   the POC compare against a Flat(1) (un-bit-sliced) χ to show the 8× throughput explicitly?
