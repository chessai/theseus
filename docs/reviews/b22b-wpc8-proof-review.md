# Adversarial review — B-2.2b (w_pc=8 run-correctness proof)

**Commit reviewed:** `eb63121` (`b22b-wpc8-proof`, off theseus-v2 `57d2500`).
**Reviewer:** fresh-context Fable (did not write it); implementer was Opus.
**Verdict:** **FIX-THEN-SHIP** — one MAJOR (a false coverage-waiver lemma), rest
MINORs. The proof body is sound: correct oracle, genuine 2^16 coverage, the two
sides of the run comparison are genuinely different layouts, and the
vacuous-pass routes were closed.

## Scope

B-2.2b is a tests-only slice (no `src/` change) that proves the just-merged
B-2.2a w_pc=8 refactor is not merely well-typed but RUN-correct — the crux
`type_of` cannot see (intra-word wire aliasing). Two tests in
`crates/theseus-tisc/tests/b22b_wpc8_run.rs`: (1) `adr0034_split_add_exhaustive_wpc8`
— `(pc_add_br_with;pc_inc_with)(&w_pc8)` == `(PC+BR+1) mod 256` over all 2^16
(PC,BR) pairs + field preservation + adjoint; (2) real CPU runs via
`run_rom_on_cpu_with(&w_pc8,...)` == the same program at the default w_pc=16
layout.

## Findings

- **MAJOR-1 — the proof waived backward-branch RUN coverage on a FALSE lemma.**
  The commit/comments claimed "a terminating hand-built backward branch is not
  constructible without a loop counter" (the come-from re-arms the token → infinite
  loop). That assumes the branch and come-from share ONE register. With TWO
  registers the token migrates and the loop terminates in exactly two passes, no
  counter. Fable exhibited a 5-instruction program; it drives the one case no
  forward-only run reaches — a real backward jump WRAPPING through the modulus
  (`3 + sext(0xFD) + 1 = 257 ≡ 1 mod 256` at w_pc=8; `= 0x10001 ≡ 1 mod 2^16` at
  w_pc=16 — the layouts agree via sext). Not a BLOCKER (TEST 1 covers the backward
  *arithmetic* exhaustively over all 128 top-bit-set BR through the same builders/
  layout the run drives), but a correctness proof must not rest a waiver on a false
  argument. **RESOLVED:** added `wpc8_run_equals_wpc16_backward_branch` (Fable's
  program: token in r2 migrates to r1, body runs twice ⇒ r3==2, terminates
  quiescent, w_pc=8 == w_pc=16 on every input) + corrected the false comment + the
  header claim. Verified: b22b suite 4/4 green.
- **MINOR-2 — doc contradiction** (header/TEST-2 preamble claimed a backward
  branch was present, contradicting the impossibility note). RESOLVED alongside
  MAJOR-1 (the backward branch is now genuinely present).
- **MINOR-3 — `s_max` passed as 0** though the programs have forward-skip distance
  2. Harmless under the 256-bound; the run-vs-run equality + quiescence guard are
  the real checks. Left as-is (the programs terminate quiescent at both widths,
  which is the property under test).
- **MINOR-4 (pre-existing src, not this commit) — the park-space guard
  (`compile.rs` ~4620) checks `budget + s_max <= pc_addr_space()` but ignores
  `init.pc`.** Case B (high-PC start) stays in-range only via the test's own
  arithmetic; a start high enough to wrap the parked PC at w_pc=8 would re-enter
  the ROM with no guard rejection. Flagged as a follow-up for B-2.2c's derivation
  (which controls init.pc + budget together); not a B-2.2b bug.
- **MINOR-5 — `assert!(got.pc < 256)` is structurally vacuous** (`decode_with` at
  pcf==1 hardcodes the hi byte to 0). Harmless; the neighbor-field preservation
  sentinels (nonzero IR/regs/scratch[2..]) are the real misroute protection and
  are good. Added an explicit `pc_fields()==1` pin in `layout_w_pc8()` per the nit.

## Confirmed sound (attacked, could not break)

- **Oracle** — cross-checked against `pc_update`/`ref_step` (compile.rs ~4700,
  tests/step.rs ~107): PC' = PC + sext(BR) + 1 mod 2^w_pc, post-execute BR; at
  w_pc=8 sext of the 8-bit BR is identity ⇒ `(PC+BR+1) mod 256`. Composition order
  matches `step_with`.
- **Not a 16-bit run in disguise** — `Layout{w_pc:8,..}` gives pc_fields()==1;
  `assert_pc_carrier` rejects the w_pc=12 aliasing case; the pcf=1 carrier genuinely
  drops the PC_hi word (a mixup would type-error at eval). `run_rom_on_cpu` baseline
  is the default (w_pc=16) net — genuinely different.
- **Not vacuous** — the runs have net register effects and different final PCs
  between a jump-walk and a no-jump-walk, so `got8==got16` (which includes `pc`)
  cannot pass with a dead w_pc=8 run; the r2/r3 branch-taken probes rule out
  fall-through. Exhaustive loop asserts `checked == 1<<16`.
- **Fetch at pcf=1 exercised** — `step_with` includes `fetch_with`; the runs decode
  real instructions (addrs 0..7) and drive the high-zero gate on the park side
  (Case B parks at PC 239..255).
- **Quiescence guard is real** (compile.rs ~4644 rejects BR/s0/s1≠0); the
  author's rejected first-draft (terminated BR=3) witnesses the guard is live.

## Process note

The B-2.2b full authority run hung for ~39h at 0% CPU on a known-good test
(`whole_domain_ccnot_at_w_data_4`) because a review agent was permitted to run
cargo in the SAME worktree/`target` as the authority run — two cargo processes on
one target dir wedged. Lesson: reviewers get a separate target dir or no cargo;
the authority run owns the box.
