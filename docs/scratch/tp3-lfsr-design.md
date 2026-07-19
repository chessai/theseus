# TP-3 — LFSR over K steps (fits-today coverage program)

**Milestone.** A Tier-1 test-program from `docs/src/test-program-roadmap.md` (TP-3): a
width-4 linear-feedback shift register run K steps via `iter`. It is the
**longest-yet-compiled `iter` with the lowest-branching body** — a clean contrast to
fib's short, variable, branching trace. **Fits today**: pure surface `.thse` + a test;
NO src / lowering / decoder changes. The genuinely-concurrent **Thread B** (disjoint
from f3.5, which edits `asm.rs`/`decoder.rs`/`recognize_prim`; TP-3 touches only a new
conformance program + a new test file).

> **Revision r2** — incorporates the fresh-context Fable design review
> (`docs/reviews/tp3-lfsr-design-review.md`), which **empirically probed** the config
> and returned verified green numbers. The originally-proposed `Nat@8×Bit⁴` does NOT
> compile (F1); the pinned config below is confirmed compiling + oracle-agreeing.

## 0. Why this fits today (and is NOT the overturned TP-1a)

- **Bijective by construction.** Every surface iso (`<->`) is a checked total bijection;
  `iter` is gated by the elaboration typing judgments (T0601 counter-is-numeral, T0602
  body-is-endo). `Iter(σ) = ⟨k,x⟩ ↦ ⟨k, σᵏx⟩` is a bijection for bijective σ.
  `run_on_cpu == eval` is well-posed.
- **Not TP-1a.** The overturned TP-1a was "orbit-length to a target" — a **halting
  `Trace`** converging on one exit state (not injective). TP-3 is a **fixed-K `iter`** of
  a fixed linear bijection — no halting, no convergence. All-zero is a fixed point but
  irrelevant to bijectivity (it maps to itself, is in the domain, T2 covers it).
- **Precedent (corrected — F3).** The real TISC-compiled precedents are
  `0004-cnot.thse`, `0022-fib.thse` (`SwapProd` + cnot-prim under `iter`, real-CPU
  oracle), and `spill_branch_iter` (11-leaf iter). (Note: `0024-adder`'s oracle is the
  *netlist* path — `crates/theseus-netlist/tests/adder_lowering.rs` — nothing in
  theseus-tisc compiles it; cite it only for the compositional *style*.) The full
  vocabulary lowers Swapbr-free: `asc`→`AssocLProd`→**0 instrs**;
  `swapBit`→`SwapProd`→`Exch`/`swap_homes` (handles Reg/Scratch/Mem);
  `cnot`→Prim→`xorInto`→`Xor`. No `toffoli` needed → f3.5's CCNOT gap is untouched.

## 1. Construction (surface `.thse`) — PINNED config

**Width N=4, polynomial x⁴+x+1** (primitive, period 15; Galois mask `0b1001`, a single
feedback `cnot`). Register `x = (b0, b1, b2, b3)` (nested `Bit` product, b0 = lsb = first
tuple component). One Galois step `x ↦ (x >> 1) ⊕ (mask · b0)` built compositionally
(0024-style routing; verified in the probe):
```
lfsrStep = cnot01 ; swap01 ; swap12 ; swap23
```
(one feedback `cnot` on (b0,b1) for the tap, then a 3-transposition rotate) — **4 TISC
instructions, 0 Swapbr** (1 Xor + 3 Exch); fast-CPU == eval == reference Galois(0b1001)
on all 16 register inputs. Build the routing from prelude gates `asc`/`swapBit`/`cnot` as
0024-adder does — no dense truth tables.

Iterate: `lfsr : Nat@6 * Reg ↔ Nat@6 * Reg = iter Nat@6 lfsrStep`.

**Hard ceiling (F1 — state in the spec):** `iter Nat@D` fails to compile for **all D≥7
today** — `lower_data_dependent_iter` excludes the D unary-counter leaves from
`allocate_aux` displacement (compile.rs:1917–1921), so at ≥8 total leaves no register is
free for the 2 loop aux. `Nat@6` (k_max = 5 executable steps) is the maximum. Domain =
`D · 2^N` = `6 · 16 = 96`. The ROM has `D−1 = 5` guarded copies. A genuinely long-K LFSR
is gated on counter-width (M-D.c) or teaching `allocate_aux` to displace counter tags —
both out of scope here.

Verified whole program: `lfsr = iter Nat@6 lfsrStep` → **ROM 104 instrs, wcet B = 104,
exactly 10 Swapbr** (= 2 guard Swapbrs × 5 copies); fast-CPU == eval on **all 96
inputs**; 5-step orbit from seed `0b0001` = `1 → 9 → 13 → 15 → 14 → 7`.

## 2. What it demonstrates (the datapoint)

- **Longest-yet iter, lowest-branching body.** k_max = 5 (vs fib's 3, array_map's 3) —
  the longest `iter` compiled to date and the widest iter payload; every step is the
  *same* straight-line linear map with a **Swapbr-free body**. The ROM's only branches
  are the 5 guard-pair skip-blocks. (Honest framing: this is "longest-yet + lowest-
  branching," not "long" in absolute terms — see the F1 ceiling.)
- **Park-tax shape.** k_max = 5 is a tight numeral, so `T(k)` is affine in the runtime
  count k and the histogram is tight (array_map's T6a precedent, B=30/T(k)=18+4k). This
  gives a third park-tax curve — a *low-branching* one — alongside map/reduce. Parked
  copies run only the ~6-instr skip spine, so park tax at k=0 is large.
- **Energy.** `rom_program_energy` prices the 104-instr ROM — a "many cheap steps"
  profile.

## 3. Test plan (L20/L21) — new `tests/lfsr.rs` (zero merge friction w/ f3.5)

- **T1 (compiles + low-branching emission)** — `lfsr` elaborates + `compile`s; assert
  BOTH: `compile(lfsrStep)` is Swapbr-free, AND the full ROM's Swapbr count == `2·(D−1)`
  = 10 (the "low branching" claim, testable on the interleaved ROM). Assert ROM length ==
  104 (measured — do NOT predict `spine + (K−1)·|step|`; payload is scratch-homed under
  the D=6 counter, ~20/copy with paging).
- **T2 (whole-domain oracle)** — `assert_oracle_fast("lfsr", &iso)`: `run_on_cpu_fast ==
  eval` over ALL 96 inputs (D·2^N). Milliseconds.
- **T3 (#[ignore], real CPU)** — `assert_oracle` on the real `system_step` for the
  **maximal k=5 trajectory from a nonzero seed** (e.g. seed `0b0001`) — no all-zeros
  blind (L19 I-2). (Full-orbit real-CPU is hours → out of `check_commands`,
  gate-no-timeout lesson.)
- **T4 (orbit correctness — pins the polynomial)** — for seed `0b0001`, assert the
  compiled output after each of the 5 steps matches the hand-computed sequence
  `1,9,13,15,14,7`. **Embed the 5-line reference Galois step in the test** (b0 = lsb =
  first tuple component); do NOT import a C-convention sequence. (Drop "returns to seed"
  — period 15 > k_max 5, F2.)
- **T5 (park-tax / WCET, DEFAULT via rom_trace)** — `wcet == 104`; `T(k)` affine and
  monotone over k∈0..5; park-tax 0 at k = 5 (= k_max; 13.5% at k=4) — measured
  T(k) = 34 + 14k → [34,48,62,76,90,104], pin these values. +
  **T5b (#[ignore])** energy via `rom_program_energy`: step_j>0, total_j == B·step_j
  (mark it the static-inventory MODEL IDENTITY, not a falsifiable check).

## 4. Probe (already discharged by the design review)

The review's throwaway probe compiled the pinned config end-to-end and returned the
numbers in §1 (ROM 104, B=104, step 4 instrs/0 Swapbr, 96/96 oracle, orbit
1→9→13→15→14→7). The worker starts from known-good numbers; if any MEASURED value
differs, REPORT it (do not hardcode over a discrepancy). If a routing composition trips
a guard, STOP and report — but this is low-risk (vocabulary verified).

## 5. Delegation shape

One maestro task (tier 0, theseus profile): the `.thse` program + `tests/lfsr.rs`.
**Test/conformance-only** — like f2/f3, NO src changes. `file_allowlist`:
`crates/theseus-tisc/**`, `conformance/programs/**`. `check_commands`: `cargo clippy` +
`timeout 1200 cargo test -p theseus-tisc --test lfsr` (scoped is fine — no ISA/decoder
change can rot unrelated suites, contrast f3.5's full suite).

## 6. Concurrency (Thread B — genuinely disjoint from f3.5)

Touches ONLY a new `conformance/programs/*.thse` + a new `tests/lfsr.rs` — **zero
overlap** with f3.5's `asm.rs`/`decoder.rs`/`compile.rs::{recognize_prim, exec_instr}`.
Truly parallel; no merge-order constraint beyond the usual FF re-gate. Merge whenever
ready (before or after f3.5 — no shared file). Spill (M-D.e) remains deferred until after
f3.5 merges (its recognizer extension collides with f3.5).
