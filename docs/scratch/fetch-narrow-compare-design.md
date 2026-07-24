# Slice A: narrow the fetch address-compare (A-bit, A=⌈log₂ B⌉) — DESIGN (draft r1)

**Status:** advisor draft for Fable design-review-with-POC. HIGHEST BAR (fetch/decoder = control-flow
lowering; a bug corrupts every compiled program). Motivated by the widen-PC energy analysis
(`docs/reviews/tloops-slice1b-recognizer-poc.md` + the fork discussion): the fetch predicate's full-W
compare is what makes widen-PC expensive, AND it wastes energy on every small program today.

## 0. The problem
Fetch (`step.rs` ~127-140, `pc_pred` ~89-110) computes, for EACH live ROM entry at address `p`
(0..N−1), a full **W-bit** `PC == p` predicate = an AND of W literals = `W−1` Toffolis compute + `W−1`
uncompute = `2(W−1)` per entry. Total per-step fetch predicate cost: **`N·2(W−1)`**. This scales with
the register width W even though a program only spans `B` steps (needing `⌈log₂ B⌉ ≤ W` address bits).

## 1. The invariant that makes narrowing sound
The PC only ever holds values in `[0, B)`: it starts at 0, `pc_inc` walks it up, and it parks below B
(the P2 guard `B + S_max ≤ 2^W`, compile.rs ~3209, already bounds it). So with `A = ⌈log₂(B + S_max)⌉`
(the bits needed for every reachable PC, live + parked + forward-skip), **the high `W−A` bits of the PC
are provably always zero**, and **the low A bits uniquely identify every reachable PC value**.

⇒ An **A-bit** per-entry compare is EXACT (no false match, live or parked), because no two reachable PC
values differ only in the high (always-zero) bits.

## 2. The change (fetch predicate ONLY)
- Compiler computes `A = ⌈log₂(B + S_max)⌉` (it already knows B = wcet and S_max). Store A alongside the
  ROM / pass to `fetch`.
- Per-entry predicate: AND of the **low A bits** of PC only ⇒ `2(A−1)` Toffolis/entry (was `2(W−1)`).
- ONE SHARED safety gate: "high `W−A` bits all zero" (`2(W−A−1)` Toffolis, ONCE, not per-entry). If
  violated (should be impossible given the invariant — defensive reject-don't-corrupt), NO entry matches
  → the machine parks/no-ops (safe). (Alternatively rely purely on the invariant and drop this; the
  review decides — keeping it is cheap insurance and preserves the current safety posture.)
- `pc_inc` / `pc_add_br` UNCHANGED (full-W arithmetic). Wire layout unchanged. Only the fetch predicate
  narrows.

New per-step fetch predicate cost: **`N·2(A−1) + 2(W−A−1)`** (was `N·2(W−1)`). The per-entry term is now
A-dependent (∝ log₂ B), and W only enters the single shared term.

## 3. The double win (the point)
- **Lower step_j TODAY:** for small programs A < W, so fetch predicates shrink. fib B=24 ⇒ A=5: 4
  Toffolis/entry vs 7 (−43% on fetch predicates) at W=8. Every program's step_j drops. (Energy goldens
  DROP — a real improvement; re-pin with explanation. Ring-timing goldens may also shift — check.)
- **widen-PC nearly free:** per-entry cost ∝ log₂ B, independent of W. Bumping W (slice B) adds only the
  shared `~2(W−A)` (once) + the small `~4(W−A)` pc-arithmetic growth — a handful of gates TOTAL, not
  `N×`. This slice is the prerequisite that turns widen-PC from a ~+15-30% tax into low-single-digits.

## 4. Correctness / soundness (what the POC + adversarial review must prove)
1. **Fetch still correct:** `run_on_cpu == eval` for existing programs (fib, χ, sbox, mde_spill, lfsr,
   all Trace/iter). The A-bit compare must fetch exactly the right entry at every PC, live and parked.
2. **No false match:** for A = ⌈log₂(B+S_max)⌉, no two reachable PC values (0..B+S_max−1) collide on the
   low A bits (they're all < 2^A, distinct). Attack: an off-by-one in A (A too small) → a park address
   aliases a live entry → wrong fetch → corruption. Pin A's derivation exactly.
3. **The high-zero invariant is real:** PC < 2^A throughout (the P2 guard + pc_inc-from-0). The shared
   safety gate should never fire on a correct program; a test that it DOES fire (rejects) if the PC is
   forced high-nonzero (teeth).
4. **Park zone:** parked PC (∈ [N, B)) matches NO live entry under the A-bit compare (correct — it
   idles). Verify across a program with a park phase (fib(1): T<B).
5. **Energy:** step_j DROPS; the new value is correct (re-pin me_a3 etc. with the −X% explanation).

## 5. The MEASUREMENT the POC owes (de-risks the whole widen-PC plan)
Build the A-bit fetch, then:
(a) `run_on_cpu == eval` green for the existing programs (fetch correctness);
(b) measure step_j at W=8 WITH the opt vs the current full-W (should DROP for small programs — quantify);
(c) **the key one:** bump W to 16 WITH the opt and measure step_j — it should barely move vs W=8-with-opt
   (proves widen-PC is nearly free once fetch is narrowed). Report the numbers.

## 7. Soundness lever verified by code-read (feeds the adversarial review)
The A-derivation introduces **no new trust assumption** — it reuses the exact bound the existing
park-safety guard already depends on:
- `run_rom_on_cpu` (compile.rs ~3273) enforces **P2**: `budget + s_max_val > 256 ⇒ reject`. I.e. the
  system *already* trusts `budget + s_max` as the tight upper bound on every reachable PC (live + park +
  forward-skip) — that's precisely what P2 checks against the 256-value PC address space.
- The POC calls `fetch_addr_bits(budget, s_max_val)` with the **same `s_max_val`** P2 guards with, and
  `fetch_addr_bits = ceil_log2(budget + s_max)`. Hence `2^A ≥ budget + s_max > every reachable PC` ⇒
  **every reachable PC < 2^A**, so its high `W−A` bits are provably zero and the low-A compare is exact.
- Therefore the reviewer's headline attack — "A too small ⇒ a park address aliases a live entry" —
  reduces to "some reachable PC ≥ budget + s_max", which would mean **P2 is already unsound today** (PCs
  wrapping past 256 without Slice A). The A-bit fetch is exactly as sound as the pre-existing park model.
- Residual things the review must still break (NOT covered by the above): (i) the `ceil_log2` off-by-one
  at exact powers of two (2^A ≥ budget+s_max, verify the `.max(2)`/`clamp(1,W)` edges); (ii) the
  `high_zero` Bennett uncompute leaves *only* the flag set (intermediates + FETCH_HZ_WIRE=4W−1 restored,
  nothing leaks across fetch phases); (iii) `fetch_a(rom,W)` is byte-identical to `fetch(rom)` (the a==W
  path); (iv) confirm the a<W predicate ANDs low-a literals **plus** the shared HZ flag (not literals
  alone — that's the `_unguarded` teeth variant, which must stay test-only, never shipped in `src`).

## 6. Questions for the design-review
1. Is A = ⌈log₂(B+S_max)⌉ exactly right (covers live + park + forward-skip; off-by-one)? Where does
   S_max/max reachable PC come from (max_skip_distance + B)?
2. Keep the shared high-zero safety gate, or rely purely on the invariant (drop it)? Cost vs safety.
3. Does narrowing interact with anything else that reads the PC bits (the decoder, pc_add_br, the ring
   frame, validate())? Confirm ONLY the fetch predicate changes.
4. Energy + ring-timing goldens: which move, by how much, and are the new values correct (re-pin)?
5. The measurement (§5): the actual step_j drop today + the W=16 near-flatness (the widen-free proof).
