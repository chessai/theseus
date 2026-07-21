# BVEQ slice-2b design review — symbolic netlist walk (adversarial, with PoC spike)

**Reviewer:** fresh-context adversarial agent (did not write the design). Design: `docs/scratch/bveq-slice2b-netlist-walk-design.md` r1 @ theseus-v2 `8383aa3`. **Spike:** built + run in a throwaway worktree (removed; repo untouched) — a complete `eval_bdd(&Iso, SV) -> SV` symbolic interpreter over biodivine BDDs, walked against `instruction(op)`, `decode_execute()`, and the full 8-phase `system_step(rom)` for **all 13 register-pure instructions**, compared to slice-1 `instr_semantics` through a shared extended `VarMap`, and ground-truthed against the real machine via `run_rom_on_cpu(theseus_eval::eval)`. All spike checks passed (215 s total, release).

## VERDICT: SHIP-WITH-FIXES

The construction is SOUND and the spike proves it end-to-end (including the full 8-phase `system_step`, which makes T3 vanish), but the "ms, complete, default" performance claim is wrong by ~2 orders of magnitude, the §2 initial-state and gate-builder prescriptions under-prove and over-trust respectively, and `equiv` cannot be reused as drawn.

## Spike measured results (release, 112-var universe)

| walk target | result | wall-clock/instr | max BDD nodes |
|---|---|---|---|
| A. `instruction(op)` one-arm, 13 ops | **EQUAL** (full frame) | 62–394 ms | 3–26 |
| B. `decode_execute()` all 15 arms | **EQUAL** | 2.87–3.15 s | 3–26 |
| C. `system_step([word])` full 8 phases, 13 ops | **EQUAL** (+PC=1, IR=0, bus=0, mem symbolic preserved, ancillas clean) | 3.40–3.62 s | 3–26 |
| D. bootstrap: walked C-BDDs at concrete points vs real `run_rom_on_cpu` (Xor/Add/Ccnot) | **AGREE** 3/3 | real eval 6.7–8.1 s/input | — |
| E. negative controls | all behave (E1 xor-vs-add MISMATCH; E2 Addi w/ symbolic s0 MISMATCH — s0=0 load-bearing + detected; E3 Add-arm on Xor IR IDENTITY) | | |
| F. completeness cost | Xor all 56 (rd,rs) 13 s; Xori imm sweep ×256 ≈ 18.4 s/op | | |

Max BDD nodes across everything: **26** (Add/Sub TK carry). Wall-clock is Iso-tree-interpretation bound (routing + `adjoint()` cloning under `Sym`), ~0.7 ms/gate — NOT BDD-bound.

## Q1–Q5 resolution

- **Q1 (one arm vs full):** walk the full `system_step`; ~15× the one-arm cost but eliminates T3 outright. Spike C walked the complete 8 phases — fetch predicate chains, all 15 arms, the unpredicated MEXCH `addr ^= IR.rt` emit CNOTs (which really drive BUS_ADDR non-zero mid-step and really are undone by absorb — verified), mem-phase Fredkin routing of symbolic mem on a garbage address with enable=0, unfetch, pc_add_br, pc_inc — exact equality + clean frame for all 13 ops at 3.4–3.6 s. One-arm alone is NOT sufficient as sole authority (can't see predicate-overlap or surrounding-phase regressions). Slicing: one-arm = fast default tier; full-system_step = authoritative tier.
- **Q2 (reuse equiv?):** NO as drawn — `equiv` takes `RegTransfer`(BvExpr) both sides (walk produces `Bdd`) and builds its own internal `VarMap::new(NUM_REGS)`. Use direct canonical-BDD `==` under a shared extended `VarMap` (encode `instr_semantics` through the SAME map; cex via `xor().sat_witness()`), ~30 lines — OR refactor `equiv` to expose an inner `equiv_bdds(a:&[Bdd], b:&[Bdd], map, ...)`. Netlist reconcile is w=8 → I4 vacuous, operand_regs/N5 is a RED HERRING here; full-frame is what matters (spike checks a WIDER frame than equiv: scratch/PC/IR/BR/mem/bus).
- **Q3 (BDD size):** trivially small. With IR concrete, predicate Toffoli chains + crossbar Fredkin controls are CONSTANTS → crossbar collapses to a wire permutation, predicate to 1; BDDs only see operand-register vars (max 26 nodes over 112-var universe). "≤24-30 vars" was never the risk. Cost driver = interpreting the Iso tree (~4.5k prim apps + adjoint deep-clone per Sym) → 231 ms/one-arm R-format, not "ms".
- **Q4 (which instrs):** exactly the 13 register-pure {Add,Sub,Xor,Exch,Rotl,Rotr,Cswap,Ccnot,Addi,Subi,Xori,Rotli,Rotri}, all EQUAL. Xchg/Swapbr/Mexch/MexchReg correctly excluded. NOTE: complete in operand VALUES (the BDD) is total; complete in SELECTORS+IMMEDIATES is by enumeration — 56 (rd,rs)/2-op, 336 triples/3-op, 8×256/I-format ≈ **19 min** total one-arm (8× faster than the 2.6h sweep, with total value coverage the sweep never had) — but not "ms".
- **Q5 (retire pointwise?):** retire the WIDE sweep; keep the sampled real-machine anchor; add a walk-vs-real bootstrap. The anchor is NOT optional — the walk shares a convention surface (Left=1 bit convention, VarMap interleaving, field ordering) with `instr_semantics`; a symmetric mistake is invisible to walk≡semantics. Spike D (walk-BDDs at concrete points vs run_rom_on_cpu) is the defense.

## Findings

- **MAJOR-1:** the "FAST DEFAULT (ms), complete at full Flat(8)" headline is FALSE. Measured 62–394 ms per one-arm walk PER (op, selectors, imm) combo; full selector/imm completeness ≈ 19 min; full-`system_step` variant 3.5 s/op. Re-tier (see below). Value is real (2.6h → 19min ignored with MORE coverage + 46s whole-step authority + seconds default) but the text misleads.
- **MAJOR-2:** §2 initial state ("PC/BR/scratch = constants") under-proves. s0/BR (+mem for system tier) must be SYMBOLIC — proves the strong contract (swap-fetch content-agnostic; CCNOT BR-buffer no-BR=0-assumption) at ZERO cost (max 26 nodes). EXCEPT Addi/Subi genuinely require s0=0 (unpredicated immediate mask, decoder.rs:720) — spike E2 shows symbolic-s0 correctly REJECTS Addi; pin s0=0 for I-format adds only + document why.
- **MAJOR-3:** `equiv` reuse impossible as drawn (Q2). Direct BDD `==` + own cex decode under one shared extended VarMap (regs+s0+BR+mem operands = the one TCB mapping; `instr_semantics` encoded through it), or refactor equiv to a BDD core.
- **MAJOR-4:** replace hand-written gate builders with TRUTH-TABLE SYNTHESIS (minterm expansion of each prim's admission-checked `PrimDef::forward`, ≤16 rows × ≤4 wires — the identical table `theseus_eval::eval` runs). Removes a transcription-bug class (T1 collapses), covers the two prims the design forgot (`gfredkin` K2 decoder.rs:106; NOT is `SwapSum(One,One)` structural, not a prim), generic over future prims. Cost nil.
- **MINOR-5:** stack depth — Iso trees + `adjoint()` recursion overflow a 2 MiB test-thread stack; run the shipped test in a spawned big-stack thread (spike used 1 GiB) or make the interpreter iterative.
- **MINOR-6:** `eval_bdd` must fail CLOSED (Err, not panic) outside the loop-free product fragment (Trace/Iter/Fold/Unfold/Plus/general sums) — bveq never-panic discipline. Fragment actually used: {Id, Sym, Seq, Times, SwapProd, AssocL/RProd, UniteProd, UnitiProd, SwapSum(1,1), Prim}.
- **MINOR-7:** scope precision — (a) per-instruction claim is for `rom=[word]` at PC=0 (multi-entry fetch = whole-program oracles' job); (b) keep ≥1 carry-chain + 1 rotate + 1 3-op per tier (Exch/Cswap BDDs are less informative); (c) BR must be constant 0 in the system_step tier (PC_out=BR+PC+1 else symbolic; matches terminal invariant) + document.

## Corrected test tiers (replaces §3's single default test)

- `bveq_netlist_walk_reconcile` (DEFAULT, ~5–15 s): one-arm walk, 13 ops, 2–3 selector/imm configs each (cover rs=0, high rt, a carry op, a rotate, both 3-ops), full-frame.
- `bveq_netlist_walk_system` (DEFAULT-slow or short #[ignore], ~46 s): full `system_step` walk, 13 ops, 1 config each — the authoritative whole-step theorem (discharges T3); run in CI.
- `bveq_netlist_walk_full_sweep` (#[ignore], ~19 min): one-arm, EVERY selector pair/triple + every immediate — REPLACES + RETIRES `bveq_reconciliation_lb_netlist_wide` (2.6 h) with strictly greater coverage.
- `bveq_netlist_walk_bootstrap` (short #[ignore] or default-slow, ~30–40 s): 3–5 concrete walk-vs-`run_rom_on_cpu` samples (T2 anchor). Keep existing default `bveq_reconciliation_lb_netlist_sampled`.
- T1 pins only the `bits_of`/`Left=1` convention (gate tables no longer hand-written).

## T1–T5 scorecard
- T1 gate faithfulness: discharged BY CONSTRUCTION via MAJOR-4; residual = the bit-order/Left=1 convention test.
- T2 walk-vs-machine: spike D 3/3 AGREE; keep as bootstrap (mandatory, Q5).
- T3 decode_execute-vs-system_step gap: ELIMINATED by walking `system_step` itself (spike C 13/13 EQUAL, phases verified incl. the unpredicated emit addr-CNOTs + symbolic-mem round-trip).
- T4 thermometer↔bits: the walk never touches thermometer Values — gates are flat(n) products of Bit, convention pinned by `bits_of` at one place; bootstrap is the end-to-end check.
- T5 ancilla: verified symbolically for all 13 ops, all tiers — s1–s7 exactly 0 on exit, s0/BR/mem exactly symbolic entry values, IR preserved/re-zeroed, bus quiescent.

**Bottom line:** SHIP the construction with the amendments — synthesize gate BDDs from PrimDef tables, symbolic s0/BR/mem init (s0=0 for I-adds), direct BDD== under one shared extended VarMap, big-stack thread, fail-closed interpreter, and the 4-tier test structure. The spike test source is ~90% of the slice and should be resurrected nearly verbatim. The un-ignore goal is achieved: a complete-in-values default reconcile tier (seconds) + a 46 s whole-step authority tier, with the 2.6 h pointwise sweep retired in favor of a more-complete 19 min ignored enumeration.
