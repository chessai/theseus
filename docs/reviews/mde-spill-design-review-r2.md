# Adversarial design review r2 — M-D.e slice 1 (loop-carrier scratch-spill)

**Reviewer:** fresh-context Fable (adversarial), did not write the design/r1-review/spike.
**Reviewed:** `docs/scratch/mde-spill-design.md` (r2+r1) @ theseus-v2 `2ee1248`, r1 review, spike
reference `docs/scratch/mde-spike/*` — **RE-BUILT and RE-RUN in a throwaway worktree** (diff applied
clean at tip; worktree deleted; repo untouched). Every reversibility/layout claim re-verified against
`compile.rs` (`lower_trace` 2142–2427, `with_registers` 448–530, `home_of` 391, `check_pattern_kleaf`/
`mde_walk_bytes`, `run_rom_on_cpu` 2844), `step.rs` (`system_step` 259), `decoder.rs` (CSWAP 548), and
the actually-emitted 29-instruction ROM.

**Verdict: SHIP (with the DO-NOT-SHIP-UNTIL list — all test/doc hardening, no design change).** The r1
BLOCKERs (F5, F6) are genuinely resolved by the spike's construction, and the one question r1 could
not answer — does the spill close reversibly across a real back-edge — now has an **empirical yes on
the real CPU**.

## THE DEFINITIVE REAL-CPU RESULT (finding 1 — go/no-go): PASS
`spike_mde_spill_trace_real_cpu` — real `system_step` (full 8-phase fetch/decode-execute/emit/mem/
absorb/unfetch/pc_add_br/pc_inc netlist via `theseus_eval::eval`, L19), both Bit inputs:
```
real:  Left(Unit)  -> Right(Unit)  == eval OK   ← input 0: 6 iterations, 5 back-edges,
real:  Right(Unit) -> Left(Unit)   == eval OK      spilled leaf s2=1 across the iter-1→2 jump
test spike_mde_spill_trace_real_cpu ... ok       (4141.29 s, debug)
```
Input 0 enters iteration 2 in state `[0,1,1,1,1]` — spilled leaf nonzero across the backward jump —
and the CPU agrees with eval exactly. First-of-kind. Also: compile clean (~4min cold); `spike_mde_
spill_trace` (fast oracle + ROM asserts) PASS 0.03s; `spike_mde_reject_5_payload` PASS ("5 payload
leaves, max is 4"); full non-ignored theseus-tisc suite (27 binaries incl 88-test compile, ring,
decoder, energy 13/13) ALL GREEN. (One suite run OOM-killed the 84GB energy binary under 2 concurrent
real-CPU runs @6.5GB each — contention artifact, rerun alone 13/13; also a preview of finding 5a.)

## Finding 2 — Invariants I1–I5: HOLD (re-verified against the emitted ROM)
29-instr ROM: spine {0,1,27,28}, Ltop=2, Lend=26, body [7..21], paging `Xchg r7,s2` at 7 and 21 only.
- **I1 homes_in==homes_out:** homes `[tag→R1,p1→R0,p2→R2,p3→R3,p4→S2]` permanent; hard-guard (2318)
  passed; page-in/use/page-out inside the body. ✓
- **I2 aux+tag resident:** rc=r4,rb=r5,temp=r6,ctrl=r7,tag=r1 — all registers, none paged. Paging spare
  = r7=ctrl, safe per the **self-inverse exchange bracket** (F1b: net-zero for any spare value) AND ctrl
  provably 0 in the bracket window; test asserts rd≥4. ✓
- **I3 δu counts paging:** body_len=15 (13 walk+2 Xchg), iter_len=25, emitted token immediates **231=
  256−25** — paging Xchgs in the count, in the ROM. ✓
- **I4 WCET:** 4+6·25=154≤256; budget+S_max 154≤256. ✓
- **I5 k_max=|u|+1=6:** input 0's trajectory is exactly 6 iters — bound exercised at equality (stronger
  than any prior Trace test). ✓
- **No non-canonical state at the back-edge:** page-out [21] precedes ctrl-recompute [22–23]/mint/route/
  Lend; between page-out and next page-in, only s2 holds the leaf, nothing else in the skeleton touches
  scratch. ✓
- **MINOR (2a):** the design (§1/r1 F1) describes PER-INSTRUCTION paging, but the actual construction
  brackets the WHOLE 13-instr body in ONE page-in/page-out (recognizer emits all 13 in one
  `with_registers`). Better (2 Xchgs not 26) but the safety condition differs (spare dead across the
  whole body, not one instr — holds here). Design text must describe the real shape.

## Finding 3 — F5 allocation (u-wide Sum(N5,Bit)): WORKS AS BUILT
- **No live-data relocation, verified in ROM:** the spilled leaf is the `n_extra` zero-init PADDING leaf
  (no input home); marshal zeroes s2; the 4 spine slots are Xori/Addi/Subi/Xori — ZERO spine Xchgs, so
  T1 can't pass vacuously (r1 F6 gap closed by construction).
- **Guard replacement sound:** old `n_carrier+4≤8` → (a) `n_payload≤4` cap + (b) `n_input+1≤4` (tag+live
  inputs register-resident). Only newly-admitted case is n_carrier=5; live-input spill stays rejected.
  max_carrier_reg=3 → aux 4–7, ctrl=7<8; no tag repack.
- **MAJOR (3a):** the `n_input+1≤budget` guard is the ONLY barrier against r1 F5.2 silent corruption and
  is UNTESTED. Break: loosen it (e.g. a future cap-5 bump) → a Trace over a 4-leaf INPUT type homes a
  LIVE leaf in Scratch(2) with no relocation → loop reads 0 while the datum sits in a register; wrong
  answers, no gate trip. MUST ship a reject test for n_input=4 (Trace over a 4-leaf `a`) pinning this.
- Cap=4 near-ceiling regression: NONE (full suite green; Nat32 large-carrier reject still rejects; no
  green Trace test uses >1 payload leaf).
- **NIT (3b):** `home_of` maps canonical leaf 8 → Scratch(2)=SCRATCH_SPILL_BASE (unreachable at cap 4;
  add the comment/guard r1 asked for). Load-bearing neighbors to keep: M-D.d F-1 canonical/unshared-ctx
  guard (2215, guarantees scratch empty at Trace entry) + `body_occ.set(Scratch(2))` (keeps the
  zero-source displacement allocator at 646 from grabbing it).

## Finding 4 — F6 recognizer (`mde_spill_walk`): pinned is right; byte-semantics exact; genuine bijection
- Emitted-sequence ↔ `mde_walk_bytes`: EXACT, instruction-by-instruction (incl. three sequential
  `b.swap` under one `if b[0]&1` ≡ three `Cswap` on the same unmodified t; Cswap conditions on bit 0 in
  both fast sim (2949) and real decoder (548)). No marshal/recognizer split: `marshal_resident` calls
  `value_to_leaf_bytes`, same fn `check_pattern_kleaf` uses — one layout authority.
- Silent-corruption channel closed by the SEMANTIC gate: `check_pattern_kleaf` verifies the PrimDef
  forward table vs the byte map over all 7 canonical values, total table + exact leaf count, mismatch →
  Deferred (reject-don't-corrupt). Name-gate just limits blast radius.
- Bijection: genuine (composition of reversible instrs; on canonical values the verified 7-cycle
  R0→L4→L3→L2→L1→L0→R1→R0 — one orbit; test asserts closure + injectivity, both held).
- **Verdict: keep it pinned.** "k-register-linear-map" is a misnomer (Cswap → nonlinear); honest name
  "k-leaf pinned-table pattern." Generalizing to a table family multiplies the adversarial surface of
  the one fn deciding loop code, for zero machinery-proof benefit — generalize later when a real program
  needs it. **NIT (4a):** `#[doc(hidden)]` on `pub mde_walk_bytes` (scaffold, not API). **NIT (4b):**
  `check_pattern_kleaf` enumerates the whole input type — pin a comment that it's exponential in leaves.

## Finding 5 — Test strength: T1/T3 strong; four hardening items
- T1 not vacuous (2 Xchgs, both strictly inside (Ltop,Lend), same rd/sx, sx≥2, rd≥4; no spine relocation
  exists). T3 asserted (t0≥3 eval-side, actual 6; real-CPU covers input 0's 5 back-edges with s2=1 live).
- **MAJOR (5a): `_real_cpu` lacks `#[ignore]`.** House convention is `#[ignore="authoritative real-CPU
  oracle (slow)"]`. Default `cargo test` picks it up → +69 min in the mechanical gate
  ([[maestro-gate-no-timeout]]: no gate timeout) + OOM-killed the energy binary under concurrency. Gate
  it; run via `--ignored` as the L19 finish.
- **MINOR (5b): no SystemRing closure test** (design §4 T5 dropped by the spike). Every ROM-shape novelty
  since M-D ships a ring `validate()==Ok`/run==eval^R cert; first-of-kind back-edge paging deserves one
  (bus-quiescence of `Xchg` under a backward jump is uncertified elsewhere). `#[ignore]`-gated.
- **MINOR (5c): terminal check blind to a dirty spill slot.** `run_rom_on_cpu` checks BR/s0/s1 only; a
  paging bug leaving garbage in s2 at exit with correct output regs passes both oracles (unmarshal of
  Bit reads r0). Fix: slice test calls `run_rom_on_cpu` directly, asserts final `scratch[..]==0`.
- **NIT (5d):** reject test matches substring "payload leaves" — brittle; match the structured variant.

## Finding 6 — Scope / widen-PC: machinery-proof until widen-PC; one slice is right
- No meaningful spill fits the 8-bit PC: this minimal body already costs iter_len=25 → k_max≤⌊252/25⌋=10
  → |u|≤9 inhabitants; a body big enough to want a spill shrinks that further; GCD/Collatz need |u| in
  the hundreds. Same epistemic class as M-D.d (machinery + proof, not a usable program).
- Fold recognizer + spill into ONE slice: correct (recognizer exists solely to make the spill body
  expressible; splitting ships dead/untestable code; f3.5 collision gone — diff applies clean at tip).
- Honest deliverable: "a Trace can carry one scratch-homed, zero-init padding leaf of live loop-state
  across the real back-edge, net-zero, cap-by-one, live-input spill still rejected — with a real-CPU
  proof." THE remaining risk: live-input relocation spill is unbuilt/unproven; r1 F5.2 sits behind the
  `n_input+1` guard (3a) — that guard + its reject test are the contract; relocation is a SEPARATE
  future slice with its own review, not a small extension.

## SHIP / DO-NOT-SHIP
**SHIP** the r2 design + spike construction (cap logic, u-wide padding-leaf allocation, pinned
`check_pattern_kleaf` recognizer, whole-body bracket) as M-D.e slice 1, delegated with MERGE-BLOCKING:
1. **(5a)** `_real_cpu` gets `#[ignore]` (69 min, OOMs neighbors; gate must not run it).
2. **(3a)** reject test pinning `n_input+1 ≤ carrier_reg_budget` (n_input=4 case) — sole barrier vs F5.2.
3. **(5b)** SystemRing closure test on the spilled-carrier ROM (`validate()==Ok`, `#[ignore]`).
4. **(5c)** scratch-all-zero terminal assertion via `run_rom_on_cpu` in the slice test.
5. **(2a)** design/comments describe the actual whole-body bracket; **(3b)** pin the `home_of`
   leaf-8/SCRATCH_SPILL_BASE collision; **(4a)** `#[doc(hidden)]` on `mde_walk_bytes`; **(5d)** structured
   reject match.

**Empirical bottom line:** spike compiles at tip; fast oracle passes; paging `Xchg{r7,s2}` strictly
inside the body with δu=231 counting it; 5-payload cap rejects; full suite green at cap 4; and **the
real CPU carries a scratch-spilled leaf across 5 backward jumps and agrees with eval exactly** (4141s,
debug). The mechanism r1 verified on paper is verified on the machine.
