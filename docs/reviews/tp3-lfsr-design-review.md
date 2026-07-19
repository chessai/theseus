# Adversarial design review — TP-3 LFSR

**Reviewer:** fresh-context Fable 5 (did not write the design). **Method:** recomputed
every claim against the code on `chessai/theseus-v2`; then verified empirically with a
throwaway out-of-repo probe crate that elaborated and compiled a real N=4 LFSR surface
program through today's `theseus-syntax → theseus-check → theseus-tisc` pipeline (repo
untouched; probe deleted). Bar calibrated to a fits-today, test-only program.

**Headline:** the construction is sound, genuinely bijective, fully expressible, and
Swapbr-free — compiled, and the whole-domain oracle agrees. But the design's proposed
configuration **does not compile**, and its full-orbit test requirements are
**unsatisfiable at any interesting N** under today's `iter` ceiling. Both fixable with
pinned numbers verified below. DO-NOT-SHIP-UNTIL the spec is corrected; after that, low
risk.

## F1 — BLOCKER (spec config): "N=4, K≤8" does not compile; today's `iter` ceiling is D=6 (k_max = 5 steps)

The binding constraint is **register pressure from the unary counter**, not oracle
tractability:
- `num_leaves(Nat@D) = D`; counter leaves come first, filling `Reg(0..7)`.
- `lower_data_dependent_iter` calls `allocate_aux(ctx, 2, exclude=0..n_counter)` — all
  counter leaves excluded from displacement (compile.rs:1917–1921). With ≥8 total
  leaves, no registers free → needs ≥2 register-homed payload leaves; at D≥7 at most one
  payload leaf is register-homed → `allocate_aux` fails.

**Empirically confirmed** (probe swept `iter Nat@D × Bit^N`, D=2..10, N=1..5): every
D≤6 compiles; every D≥7 fails at every N (even `iter Nat@7 * Bit` = 8 leaves). The
design's `Nat@8 × Bit⁴` fails with the misleading *"needs 12 leaves but maximum is 16"*
(F9). 0021 (`Nat@6 * Bit`, 7 leaves) sits at the edge and was never TISC-compiled (it's
eval/conformance). Largest previously *compiled* iter depth: D=4 (fib, array_map).

**Fix (verified green end-to-end):** N=4, poly x⁴+x+1 (Galois mask 0b1001, one cnot),
`iter Nat@6`, k_max=5, domain 96. Probe program (`cnot01 ; swap01 ; swap12 ; swap23`,
built 0024-style from `asc`/`swapBit`/`cnot`) produced:
- `lfsrStep`: **4 instrs, 0 Swapbr** (1 Xor + 3 Exch), fast-CPU==eval==reference
  Galois(0b1001) on all 16 inputs;
- `lfsr = iter Nat@6 lfsrStep`: **ROM 104 instrs, wcet B=104, exactly 10 Swapbr** (2
  guard × 5 copies), fast-CPU==eval on **all 96 inputs**, 5-step orbit from seed
  `0b0001` = `1→9→13→15→14→7`.

## F2 — MAJOR (test plan): T3 "K≥period" and T4 "K=period (returns to seed)" unsatisfiable

Periods: N=3→7, N=4→15, N=5→31 — all exceed k_max=5. No N≥3 full orbit fits today; N=2
(period 3) is degenerate (≈fibStep). So:
- **T3:** "the maximal k=5 trajectory from a nonzero seed" (still satisfies L19 I-2
  no-all-zeros-blind).
- **T4:** drop "returns to seed"; pin the 5-step orbit prefix vs a hand-computed sequence
  (`1,9,13,15,14,7` for mask 0b1001). Still uniquely pins the polynomial (T2's
  whole-domain step oracle already pins the exact bijection; T4's value is the legible
  sequence). Keep T4.
- §1/§2 "long iteration / period 31" is overstated: k_max=5 vs fib's 3, array_map's 3.
  It IS honestly the longest iter compiled to date + the widest iter payload — say that.
  A genuinely long-K datapoint is gated on counter-width (M-D.c) or teaching
  `allocate_aux` to displace counter tags (a src change, out of scope).

## F3 — MINOR (precedent citation): 0024's oracle is the netlist path, not the TISC path

"0024-adder already compiles + assert_oracle agrees" is wrong in the way that matters:
0024's differential oracle is in `crates/theseus-netlist/tests/adder_lowering.rs`
(surface→cells); nothing in `theseus-tisc/tests/compile.rs` compiles 0024. Real TISC
precedents: 0004-cnot, 0022-fib (`SwapProd` + cnot-prim under iter, real-CPU oracle),
`spill_branch_iter` (11-leaf iter). Vocabulary IS covered: `asc`→`AssocLProd`→0 instrs;
`swapBit`→`SwapProd`→`Exch`/`swap_homes`; `cnot`→Prim→`xorInto`→`Xor`. No toffoli needed
(f3.5's CCNOT gap untouched). Cyclic wrap needs nothing missing — the rotate IS the N−1
adjacent-transposition chain (verified: realizes `x ↦ (x>>1) ⊕ b0·mask`).

## F4 — MINOR: K vs D off-by-one
Domain = `D · 2^N`; max executable steps = D−1. Shippable: D=6, K=5, domain 96, ROM has
D−1=5 guarded copies.

## Q verdicts
- **Q1 bijectivity — SOUND, not TP-1a:** surface clause isos are checked total
  bijections; `iter` gated by T0601 (counter-is-numeral) + T0602 (body-is-endo) at
  elaboration; `Iter(σ) = ⟨k,x⟩↦⟨k,σᵏx⟩` bijective for bijective σ. No Trace/halting/
  convergence — structurally distinct from TP-1a. All-zero fixed point irrelevant
  (maps to itself; in domain; T2 covers it).
- **Q3 tractability:** D=6/N=4 → 96 inputs × 104-instr ROM, `assert_oracle_fast` =
  milliseconds; ROM 104 ≪ 256 assemble cap. Real-CPU single-seed T3 = minutes behind
  `#[ignore]`; whole-domain real-CPU = hours → correctly avoided, must stay out of
  check_commands (gate-no-timeout lesson).
- **Q4 demonstrates the claim:** low-branching REAL (step 0 Swapbr; ROM's 10 Swapbr =
  5 guard pairs). T1 assert BOTH `compile(lfsrStep)` Swapbr-free AND ROM Swapbr count ==
  `2·(D−1)`. T1 ROM formula: pin measured (ROM 104, B 104) — payload is scratch-homed
  under the D=6 counter (~20/copy with paging), do NOT predict `spine+(K−1)·|step|`.
  Park tax meaningful (tight numeral k_max; array_map T6a precedent) — a third
  low-branching curve. T5/T5b strongest part; keep.
- **Q5:** all-zero fixed point / nonzero seed (T3) correct; x⁴+x+1 primitive (period 15,
  taps={0}=Galois 0b1001, verified maximal-family); hand-compute T4 in `.thse` bit order
  (b0=lsb=first tuple component) — embed the 5-line reference step, don't import a
  C-convention sequence; scoped check_commands justified (conformance+test-only);
  `tests/lfsr.rs` slightly preferred over extending compile.rs (zero merge friction if
  f3.5 lands tests in compile.rs).
- **F9 (NIT, repo follow-up):** `allocate_aux` displaceable-shortfall reuses
  `TooManyLeaves{needed,max:16}` → self-contradictory "needs 12 leaves but maximum is
  16". Worth a diagnostics fix + negative test someday.

## Verdict
**DO-NOT-SHIP-UNTIL** (design-doc edits, then ship):
1. F1: pin N=4, x⁴+x+1 (Galois 0b1001, one cnot), `iter Nat@6`, k_max=5, domain 96;
   state the hard cap (iter fails for all D≥7 today, allocate_aux counter-exclusion).
2. F2: rewrite T3/T4 (drop "K≥period"/"returns to seed"; pin the 5-step orbit prefix);
   retitle §2 "longest-yet iter, lowest-branching body"; note the long-K gate.
3. F4: fix K/D off-by-one; domain = D·2^N, copies = D−1.
4. F3: correct the 0024 precedent (netlist path); cite 0004/0022-fib/spill_branch_iter +
   the probe numbers (step 4 instrs/0 Swapbr; ROM 104; B=104; 96/96; orbit 1→9→13→15→14→7).

**SHIP as-is:** bijectivity argument, rotate+cnot construction (verified compiling +
polynomial-correct), T2 whole-domain fast oracle, T5/T5b park-tax plan, T3-as-sample
discipline, scoped gate, delegation shape/concurrency (genuinely disjoint from f3.5),
`tests/lfsr.rs` placement. The probe discharged §4's "if it doesn't compile, STOP" risk:
it compiles at the corrected config; the worker starts from known-good numbers.
