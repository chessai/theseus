# Adversarial implementation review — TP-3 LFSR (worker output)

**Reviewer:** fresh-context Fable 5; did not write the change. **Method:** read both files
in full; recomputed the LFSR math by hand (gate-level trace + orbit arithmetic +
basis-image reconstruction); re-ran the default suite, clippy, the five `theseus-check`
fixture sweeps, and the CLI conformance runner in the worker's worktree; attempted the
two `#[ignore]` tests in debug and release with timing/memory instrumentation; timed the
fib `rom_energy` example as a scaling baseline. The mechanical gate and cross-model
verifier were not trusted; every pinned number re-measured.

**Headline:** the *artifact* is correct — the `.thse` genuinely computes the pinned
x⁴+x+1 Galois LFSR (verified three independent ways), the default tests (T1/T2/T4/T5) are
real, non-tautological, and green, spec-compliance is clean (exactly 2 files, src-free,
clippy clean, full-suite sweeps green with 0026 present). But **both `#[ignore]` tests are
practically unrunnable and have never been observed green** — T3 deviates from the
design's single-trajectory shape into the hours-long whole-domain shape the design
rejected, and T5b did not complete in ≥9 min of release CPU (fib's identical pipeline:
76 s) — and **T5 silently dropped the design's "pin the measured T(k) values"
requirement**. All fixes are small test-only edits. **FIX-BEFORE-MERGE**, then ship.

## Q1 — The .thse program: CORRECT (verified 3 independent ways)
1. Gate-level hand trace: cnot01→[b0,b1⊕b0,b2,b3]; swap01→[b1⊕b0,b0,b2,b3]; swap12→
   [b1⊕b0,b2,b0,b3]; swap23→[b1⊕b0,b2,b3,b0]. Target Galois step bit0=b1⊕b0, bit1=b2,
   bit2=b3, bit3=b0 — EXACT match, no transposition/tap error.
2. Orbit arithmetic: 1→9→13→15→14→7 arithmetically correct for x⁴+x+1/mask 0b1001.
3. T4 uniquely pins the polynomial: the body is linear over F₂⁴, orbit constraints recover
   all four basis images (σ(1)=9, σ(8)=4, σ(4)=2, σ(2)=1) = exactly the x⁴+x+1 Galois
   matrix. A different-but-bijective linear map cannot pass T4.

## Q2 — Test integrity
- **T1 SOUND** (asserts compile(lfsrStep) Swapbr==0, ROM len==104, ROM Swapbr==10).
- **T2 SOUND** (full 96-input oracle, re-ran with --nocapture: "oracle agrees on all 96").
- **T4 SOUND, non-tautological** (`galois_step` is a real independent computation; the
  hardcoded [9,13,15,14,7] is derived in-test before any compiler output is consulted).
- **T5 WEAKENED** (see MAJOR-1): asserts wcet==104, monotone, exactly-affine strides,
  T(5)==B — measured T(k)=34+14k → [34,48,62,76,90,104], park taxes [67.3,53.8,40.4,26.9,
  13.5,0]% — but does NOT pin the T(k) values.
- Hardcoding audit: 104, 10, orbit are measured pins with in-test derivation; none paper
  over a discrepancy. #[ignore] placement correct (default suite: 4 passed, 2 ignored, 0
  failed) — no failing default test hidden.

## Q3 — Tractability: fast oracle trivial; keep real-CPU as a sample
D=6/N=4: 96 inputs × 104-instr ROM = ms. ROM 104 ≪ 256 assemble cap. Whole-domain
real-CPU = hours (correctly avoided / out of check_commands).

## Q4 — Demonstrates the claim (T1 rephrase): step body 0 Swapbr; ROM 10 Swapbr = 5 guard
pairs. Park tax meaningful. T5/T5b strongest part.

## Q5 — all-zero fixed point/nonzero seed correct; x⁴+x+1 primitive; scoped gate justified;
tests/lfsr.rs placement good. F9 (NIT): allocate_aux shortfall reuses TooManyLeaves →
"needs 12 leaves but maximum is 16" (repo follow-up).

## Findings
| ID | Sev | Finding |
|---|---|---|
| MAJOR-1 | MAJOR | T5 does not pin the measured T(k) values (design §3 required it); only eprintln's them. A per-copy cost regression (14→other, T(0) compensated) still passes. Fix: `assert_eq!(t_values, vec![34,48,62,76,90,104]);` |
| MAJOR-2 | MAJOR | T3 is whole-domain real-CPU (~hours, ran ~35min without completing), not the design's single k=5 seed-0b0001 trajectory. Fix: restrict to seed 0b0001 (minutes). |
| MAJOR-3 | MAJOR | T5b never observed green (≥9min release, CPU-bound not OOM; fib analog 76s). Ignore message wrong ("needs spice CharDb" — rung0_measured is a pinned RON, no ngspice; real cost is step-net lowering at ROM=104). Fix: run once in release, record runtime in the ignore msg (or annotate never-run); fix the misattribution. |
| MINOR-1 | MINOR | Design-doc park-tax off-by-one silently corrected in code with a self-contradictory comment ("k=K-1=4 (i.e. k=5)"). Truth: 13.5% at k=4, 0 at k=5. Fix the comment + design doc. |
| MINOR-2 | MINOR | Stale/contradictory T5 comment ("k=0 to k=1 might differ") above code asserting all strides equal (code is right). |
| MINOR-3 | MINOR | T2 doesn't assert domain size == 96 (an enumerate_values regression silently shrinks coverage). |
| NIT-1 | NIT | T1 doesn't pin step_instrs.len()==4 / instruction mix. |
| NIT-2 | NIT | T4 doesn't assert output counter == input k. |
| NIT-3 | NIT | Numbering gap 0024→0026 (no 0025), undocumented. |

## FIX-BEFORE-MERGE
1. MAJOR-1 — pin `t_values == [34,48,62,76,90,104]` in T5.
2. MAJOR-2 — rescope T3 to the seed-0b0001 trajectory per design.
3. MAJOR-3 — one green release run of T5b with runtime recorded (or honest never-run
   annotation); fix the "spice CharDb" misattribution.
4. MINOR-1/2 — fix the two T5 comments; patch the design doc "park-tax 0 at k=4" line.

**SHIP as-is:** the `.thse` program (polynomial-correct, verified 3 ways — do not touch),
T1, T2, T4, helper replication, #[ignore] placement, file scope/src-freedom, absence of
.expect (precedented), full-suite compatibility (theseus-check sweeps + conformance runner
verified green with 0026). MINOR-3/NITs worth folding into the same pass, not merge-blocking.

**Verifier-gap note for the maestro record:** the gate + cross-model verifier passed a
change whose two #[ignore] tests had never executed to completion and whose T5 dropped an
explicit design requirement — both invisible to check_commands by construction. "Ignored
tests must be run green at least once before merge, with runtime recorded" is worth
adopting as a standing rule for test-only milestones.
