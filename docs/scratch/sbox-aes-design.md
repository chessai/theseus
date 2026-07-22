# TP-5 next: a real byte S-box (AES vs 4-bit PRESENT), bit-sliced — DESIGN (draft r1)

> **RESOLVED (see `docs/reviews/sbox-aes-design-review.md`, verdict BUILD-PRESENT-4-FIRST + a built POC):**
> **AES-8 is INFEASIBLE on the current machine.** The reviewer reconstructed the full 115-gate
> Boyar–Peralta circuit (validated against an independent GF-inversion table, 256/256) and found a
> **peak of 30 simultaneously-live signals** vs the machine's **18 addressable slots** (8 regs + 6
> usable scratch [s0/s1 decoder-reserved] + 4 mem). It's not "expensive paging" — the live set
> exceeds total storage; AES-8 needs a recomputation-bounded schedule or a wider machine (its own
> research-grade slice). **PRESENT-4** (the 4→4 PRESENT S-box) fits in **6 registers, zero paging**,
> and is **STANDALONE garbage-free** (inputs r0-r3 preserved ⇒ every temp uncomputed in the forward
> pass — stronger than χ's under-adjoint-only). POC 8/8 green (`docs/scratch/sbox_present_poc.rs.ref`):
> whole-domain==table, discriminating, goldens, bijection, eval-adjoint reversibility, system-ring L19
> authority, **8× bit-slicing at 2.82e-7 J (3.52e-8 J/S-box sliced), 47 instrs = 31 Ccnot+10 Xor+2
> Xori+4 Xchg**. Zero src/recognizer change. Slice-1 = PRESENT-4; AES-8 = documented follow-up.

**Status:** advisor draft for Fable design-review-with-POC. Builds directly on the merged
Keccak-χ slice (`012aecb`) — same bit-sliced-over-Flat(8) lane model, same Rust-composed
`Vec<Instr>` (never lowers), same run_on_cpu==eval + system-ring authority pattern. See
`docs/scratch/sbox-chi-design.md` + `docs/reviews/sbox-chi-design-review.md` for the established
approach and `docs/scratch/chi_poc.rs.ref` for the test-harness template.

## 0. The goal and the open scope question

χ (one masked-AND per lane) proved the Flat(8) byte-parallel `Ccnot` carries a real nonlinear
crypto layer. The next milestone is a **full byte substitution S-box** — a fixed nonlinear
bijection on bytes, the classic crypto primitive. Two candidate first targets; **the design-review
must recommend which, based on feasibility (esp. register pressure), and POC the recommended one:**

- **(AES-8) the AES S-box** — the canonical 8→8-bit S-box (GF(2⁸) inversion + affine). Bit-sliced,
  the smallest known circuit is Boyar–Peralta (CHES 2012): ~115 AND/XOR gates (32 AND, ~83 XOR).
  This is *the* S-box, but it is large and — the key risk — has many simultaneously-live
  intermediate signals.
- **(PRESENT-4) the PRESENT 4→4-bit S-box** — a real lightweight-crypto S-box, ~4-term ANF per
  output bit, ~14–20 gates bit-sliced. Much smaller, far fewer live temps. A genuine full S-box,
  a natural incremental step between χ and AES-8.

## 1. THE feasibility risk to resolve first: register pressure

theseus has **8 registers r0..r7** + Scratch + Mem. Bit-sliced, each of the 8 (AES) or 4 (PRESENT)
input bits is a Flat(8) register (8 parallel S-box evaluations per byte-op). A Boyar–Peralta AES
circuit has **~115 gates and dozens of intermediate signals live at once** — far more than 8
registers. So AES-8 almost certainly needs heavy Scratch paging (`Xchg` to/from the 8 Scratch
slots, cf. mde_spill) or a live-range schedule that fits 8 regs + 8 scratch. **Is that feasible
garbage-free, and at what instruction/energy cost?** This is the crux the POC must settle. PRESENT-4
uses 4 input regs + few temps → comfortably within 8 registers, likely no paging.

Concretely for the review:
1. Take the published Boyar–Peralta AES S-box gate list (32 AND + 83 XOR, a known netlist) — OR the
   PRESENT S-box ANF — and do a **live-range / register-allocation analysis**: what's the max number
   of simultaneously-live signals? Does it fit 8 regs (+ 8 scratch via Xchg)? Report the peak.
2. If AES-8 fits only with aggressive scratch paging, quantify the overhead (extra Xchg instrs,
   energy) and judge whether it's a sane FIRST S-box or whether PRESENT-4 should come first with
   AES-8 as the follow-up.

## 2. Construction (same shape as χ)

- Bit-sliced: input bits x0..x_{n-1} in registers, 8 rows in parallel per Flat(8) byte.
- Each gate → an instruction: AND = `Ccnot{tgt, a, b}` (masked-AND, but note Ccnot ACCUMULATES
  `tgt ^= a&b` — a fresh AND into a zeroed temp is `Ccnot` into a 0 reg; reuse/uncompute temps to
  stay garbage-free), XOR = `Xor{tgt, src}`, XOR-with-const / NOT = `Xori`.
- Garbage-free: like χ's Rev-A, expect garbage-free-UNDER-ADJOINT (temps/scratch cleaned by the
  reverse run); a standalone-garbage-free version (uncompute all temps to 0 in the forward pass) is
  a harder variant — the review should say which is cheaper and honest.
- Output bits y0..y_{n-1} in registers (in-place if the schedule allows, else fresh regs).

## 3. Verification (same authority as χ)
- Independent reference: the S-box TABLE (256 entries for AES, 16 for PRESENT) as a plain lookup in
  Rust — the golden. (This is the honest independent oracle; the ROM must reproduce it.)
- Fast tier: whole per-byte domain — **256 inputs** (AES) or 16 (PRESENT), each packed ×8, run the
  ROM (`run_rom_fast`) == table. Discriminating lane-packing (8 distinct inputs one-per-bit) for the
  8-lane bug. Bounded — 256 is trivially in-domain (no gate-no-timeout).
- Heavy tier (`#[ignore]`, advisor pre-merge, `--test-threads=1`): system-ring L19 authority (real
  `system_ring` == table over the 256/16 domain) + adjoint reversibility (fwd+adjoint restores input
  incl. temps→0) + energy (J per S-box eval, and the 8× bit-slicing accounting).
- Golden: a known S-box test vector (AES S-box(0x53)=0xED, etc.).

## 4. Honest capability note
Like χ, an S-box is COMPOSED from already-recognized instructions (Ccnot/Xor/Xori) — Rust-composed,
**never lowers**, no recognizer/lower/decoder change (confirm in review). It does NOT exercise BVEQ
recognition; it exercises the Flat(8) byte-parallel masked-AND capability at a larger scale than χ.
(A future genuine BVEQ-recognition cash-in remains: extend selection-by-equiv to short bodies so a
fused nonlinear step is recognized as one symbolic prim.)

## 5. Deliverable scope (ONE fast-tier delegation, ~90min)
Same as χ: promote the reviewed POC to `crates/theseus-tisc/tests/<sbox>.rs` (TEST-ONLY, allowlist
`crates/theseus-tisc/tests/**`), fast gate `--test <sbox>`, heavy `#[ignore]` tier, honest garbage-
free framing, system-ring authority. The design-review settles the target (AES-8 vs PRESENT-4), the
schedule, the register/scratch plan, and builds the POC scaffold.

## 6. Questions for the Fable design-review-with-POC
1. **Register pressure (THE question):** does a bit-sliced AES-8 S-box fit 8 regs + 8 scratch garbage-
   free? Peak live signals? If it needs heavy paging, is AES-8 a sane first S-box or should PRESENT-4
   go first? **Recommend the target and POC it.**
2. Ccnot-accumulate semantics: fresh AND needs a zeroed temp; confirm the garbage-free temp discipline
   (Ccnot into 0 → use → uncompute) works for a multi-gate circuit; watch for the L15 uncompute trap.
3. .thse vs Rust-composed? (Expect Rust-composed like χ/feistel — irregular circuit, no `iter` shape.)
4. Perf/energy: J per S-box eval + the 8× bit-slicing; for AES, is the instruction count reasonable?
5. Does anything need a src/recognizer change? (Expect NO.)
