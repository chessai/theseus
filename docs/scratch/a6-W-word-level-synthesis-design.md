# A-6 (W) — word/arithmetic-level synthesis: general AND compact prim→gates

**Decided by user (2026-07-25):** the general bit-level synthesizer (ANF `1fbb02a`, BDD `66a3ca6`)
is correct but efficiency-dead — gcd2 → 5,408 instrs (BDD, best), needing widen-PC (B=27,094) +
widen-state (9–35 words), which bloats the machine and taxes every program forever (violates the
efficiency principle). ROOT CAUSE: `BvExpr` is BIT-level, so synthesis works bit-by-bit with
alignment overhead and can't use the ISA's byte-parallel/arithmetic ops. FIX: synthesize at WORD
level. Keep BDD/ANF as the universal fallback + differential cross-check oracle.

Worktree `/tmp/a6-synth` (branch `a6-synth` @ `66a3ca6`). Builds on the verified synth core + the
verification harness (register-symbolic complete check, `assert_all_encode`, whole-domain exec).

## Why word-level is compact (the crux)
The ISA ops ARE word ops: `Xor/Add/Sub/Rotl/Rotr` (byte, reversible), `Ccnot` (byte AND-xor),
`Cswap` (bit0-ctrl swap), immediates. A word-level spec maps ~1:1 to these — e.g. gcd2's A4
`a-b` = ONE `Sub`, not ~100 aligned bit-gates. Bit-level BDD spent ~5.6 gates/node × 479 nodes;
word-level spends ~1 gate per word-op over a ~30–50-op forward circuit.

## The tractability key: word-level Bennett needs only the FORWARD function
A prim's forward function is a short NON-reversible word circuit (guards + arithmetic + muxes).
Word-level Bennett makes it reversible mechanically:
  1. compute `fwd(x)` into zero-init ancilla WORDS (not bits),
  2. swap carriers ↔ ancilla,
  3. uncompute via `bwd` (the inverse forward circuit) → ancilla net-zero.
Each step is word-ops → ~2×(forward circuit) instrs. This is why word-level is BOTH easy to author
(just the forward function — writing a reversible in-place walk is what killed A-1) AND compact.

## The word IR (WSpec)
A small SSA-ish word-level IR over registers/ancilla words. Core ops (each → 1 ISA gate):
`WXor(d,s)`, `WAdd(d,s)`, `WSub(d,s)`, `WRot{l,r}(d,imm)`, `WXorConst(d,imm)`, `WCcnot(d,s,t)`,
`WCswap(c,u,v)`. Derived macros (→ a few gates, provided by the synthesizer, verified once):
- `WSelect(dst, guard_word, then_src, else_src)` — guarded word copy `dst = guard?then:else`
  via a broadcast mask (`guard_bit → 0x00/0xFF`) + Ccnot/Xor. (guard broadcast: replicate bit0
  across the byte — e.g. `mask = 0 - guard_bit` pattern, or shifts+ors.)
- `WEqZero(dst_bit, src)` — dst_bit = (src==0): OR-reduce src's bits (via the byte structure) then
  invert; used for `a==b` (after `WSub`), `a==0`, `h==0`.
- `WLt(dst_bit, a, b)` / `WGt` — comparison via subtract + borrow/sign bit.
A WSpec = a Vec of these ops computing the forward function into designated output words, reading
carriers, using scratch/guard words. The synthesizer:
- **Direct mode:** if the WSpec is already an in-place bijection (all-reversible, no separate
  outputs), transcribe 1:1 → Instr. (For prims that are naturally reversible word programs.)
- **Bennett mode:** WSpec = forward-into-ancilla → the 3-step wrapper above (fwd; swap; bwd). This
  is gcd2's path.

## gcd2 in the word IR (the target, ~forward 30–50 word-ops)
Guards: `g_h0=WEqZero(h)`, `g_ab=WEqZero(a⊖b)`, `g_a0=WEqZero(a)`, `g_b0=WEqZero(b)`,
`g_d=g_ab'∧g_a0'∧g_b0'` (Ccnot on guard bits), `g_agtb=WGt(a,b)`. Arms as guarded word writes:
A1 (t←0,h←1 under t∧g_h0), A4 (subtract via WSub selected by g_agtb; h←2h+s via WRot/WAdd),
A2scat/Yremap (small (a,b) value permutations via WSelect over the h value), A3zE/A3exit (t flips).
Bennett → ~60–120 instrs; B ~ 350–650 (modest widen-PC); ancilla ~ 4 output + ~3 guard = 7 words
(FITS the 10 free operand words → NO widen-state, unlike BDD's 9–35). MEASURE at W.2 — this is the
efficiency payoff number.

## Verification (reuse the hardened harness)
Word-level bodies are ordinary `Instr` sequences → verify with the SAME oracle: register-ancilla
symbolic `equiv` (carriers+ancilla ≤ 8 regs = complete correctness + `bwd=fwd⁻¹` + net-zero),
`assert_all_encode`, and whole-domain 256-exec for gcd2. ADDITIONALLY differential cross-check:
the word-level gcd2 body ≡ the ANF/BDD body under `equiv` (the general path is the independent
oracle that the compact word body is correct).

## Slices (each: symbolic/exec verify → Fable review → authority → merge)
- **W.0** — the word IR types (`WOp`, `WSpec`) + the direct transcriber (`WOp → Instr`, 1:1 for
  core ops) + the derived macros (`WSelect/WEqZero/WLt/WGt`), each verified via `equiv` on small
  cases. Carry the MAJOR-1/2/reserved-operand guards.
- **W.1** — word-level Bennett wrapper (fwd WSpec → ancilla; swap; bwd WSpec) + net-zero + complete
  register-symbolic verify on a small permutation (swap, a compare-and-subtract toy).
- **W.2** — gcd2 as a WSpec (forward + inverse), synthesize, **exec-verify 256/256 + MEASURE
  body.len()/B** (expect ≪ 5,408, ancilla ≤ 10 → fits state), differential-cross-check vs the
  ANF/BDD gcd2 body. Report the number — it sizes the (now-modest) A-6.4 widen-PC.
- **W.3** — integration: a prim with a WSpec routes to word-level synthesis (the efficient default);
  BDD/ANF = fallback when no WSpec. `recognize_prim` wiring; retire vacuity guard.
Then A-6.4 (modest widen-PC, no/low widen-state) → A-6.5 (GCD runs end-to-end, run_on_cpu==eval).

## Process guardrails
Deterministic transcription/construction — delegable to Opus-5 safely. NO search/Python/beam/
sub-agents. Commit-only; never push.
