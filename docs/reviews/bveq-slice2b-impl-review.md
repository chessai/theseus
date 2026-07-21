# BVEQ slice-2b implementation review — symbolic netlist walk (adversarial, fresh-context)

**Reviewer:** fresh-context adversarial agent (did not write the change). Target: worktree `01KY26R97DPH1AC08KA66JPMBB` @ `dd98b97`, diff vs `edd3b2a` = `src/bveq_netlist.rs` (570 LOC), `tests/bveq_netlist.rs` (643 LOC), `lib.rs` (+1). Binding spec: `docs/scratch/bveq-slice2b-netlist-walk-design.md` r2; proven construction: the design-review spike.

*(Advisor's faithful capture for the durable record, L21. Advisor also ran the 3 `#[ignore]` authority tiers: bootstrap (5 walk-vs-real samples agree), full_sweep (every selector/imm config, all 13 ops EQUAL), system (all 13 ops through full system_step, clean frame) — `3 passed; 0 failed`, 3063 s.)*

## VERDICT: SHIP

The walk is faithful to the proven spike, the two comparison sides are genuinely independent, the shared extended VarMap is collision-free, the compare is a canonical-BDD `==` over the full frame, and two independent mutation teeth-checks flipped the reconcile tier to FAILED. No blockers; four minor non-soundness notes.

## A. Gate synthesis (MAJOR-4) — FAITHFUL, INDEPENDENT, COMPLETE
`synthesize_gate` (bveq_netlist.rs:216-242) enumerates `def.input_ty` and reads each row from the admission-checked `PrimDef::forward` — the IDENTICAL BTreeMap `theseus_eval::eval` applies (interp.rs:269) — flattened through `bits_of` (lower.rs:159); missing key fails closed. `apply_gate` (248-268) minterm expansion `out_j = ⋁_{rows:out_j=1} ⋀_i lit(in_i)` over input BDD functions (valid for correlated inputs); arity guarded. **Independence:** `instr_semantics` (bveq.rs:736) is a hand-written BvExpr model with ZERO dependence on decoder netlists / PrimDef tables → a synthesis bug MISMATCHES (proven by teeth-mutation #2). Covers gfredkin (K2) via generic synthesis + NOT structurally as SwapSum(One,One). **Cache-poisoning attack defeated:** GateTable keyed by forward_name; every prim_from_bits construction binds one fixed table; adjoints get distinct `{name}_inv` names (prim.rs:96-105) so Sym(Prim) can't alias the forward table; GateTable stores bool rows (universe-independent).

## B. Interpreter fragment — MIRRORS eval EXACTLY; FAILS CLOSED
Side-by-side vs interp.rs:41-129: Id, Sym (adjoint then forward), Seq, Times, SwapProd, AssocL/RProd, UniteProd, UnitiProd, SwapSum(One,One)=BDD not(), Prim — all match. Every routing former demands the exact SV shape, else Err(DomainError). Out-of-fragment nodes (Plus/Trace/Iter/Fold/Unfold/AssocL/RSum/Unite/UnitiSum/Distrib/Factor) → Err(UnsupportedIso), exhaustive match (no wildcard — a new Iso variant is a compile error, not a silent pass).

## C. Shared extended VarMap — ONE MAPPING, COLLISION-FREE
`extended_varmap()` = single `VarMap::new(14)` (8 regs + s0 + BR + 4 mem, 112 vars). Walk init (`sv_word8_symbolic` → map.var) and semantics (`expr_to_bdd` → map.checked_var) use the SAME instance. Index `bit*num_operands+operand` injective for operand<14,bit<8. `instr_semantics` only emits Var{operand∈0..8} → same reg operands the walk uses. Wrong-map spurious equality structurally impossible; the pre-existing `expr_to_bdd_shifted` canary pins the discipline is load-bearing.

## D. Initial state (MAJOR-2) — SYMBOLIC WHERE THE SPEC DEMANDS
`build_cpu_init`: r0-r7 symbolic, PC=0, BR symbolic (OP_BR=9), IR=concrete word, s0 symbolic EXCEPT pinned 0 for exactly Addi|Subi (`needs_s0_pinned`, decoder.rs:720 rationale), s1-s7=0. `build_system_init`: + m0-m3 symbolic, BR=0 (PC_out=BR+PC+1 const 1), IR=0 with rom=[word], bus=0. Reconcile runs with symbolic s0/BR and asserts BR/s0 restored to their SYMBOLIC entry BDDs (strong content-agnostic contract, not a sub-fiber). `negative_addi_symbolic_s0` proves the s0=0 pin is load-bearing + detected.

## E. Compare + non-vacuity — DIRECT CANONICAL ==, FULL FRAME, TEETH VERIFIED
`assert_eq!` on canonical biodivine Bdds (== is functional equivalence) for all 8 regs × 8 bits vs independently-encoded instr_semantics + the full frame (PC, BR, IR, s0-s7). All 20 CpuState fields pinned. equiv correctly NOT reused. Reconcile covers ALL 13 ops in 23 configs (Add×3 incl rs=0, ... Ccnot×2 incl high regs, ...). Negative controls MISMATCH. **Teeth (mutations applied, run, reverted):** (1) skip every dec_cnot gate → reconcile FLIPPED FAILED (Cswap r1[0] mismatch); (2) corrupt one dec_tof truth-table row → FLIPPED FAILED at first config (Add r0[0], 0.66s) — exactly the transcription-bug class MAJOR-4 catches. Both reverted, suite green, tree clean.

## F. System tier (T3) — VERIFIED BY READING (advisor ran it: PASS)
`bveq_netlist_walk_system` walks `system_step(&[word])` (real 8-phase) from build_system_init and asserts the complete clean frame (regs≡instr_semantics, PC=1, BR=0, IR=0, s0≡entry/0, s1-s7=0, m0-m3≡symbolic entry, bus=0) for all 13 ops — spike-C verbatim, discharges T3. `full_sweep` enumerates every (rd,rs)/triple/rd×imm (strictly more than the retired 2.6h sweep). `bootstrap` anchors vs real machine on 5 samples.

## Fidelity to the spike — matched or strengthened; two immaterial drops
All r2 overrides implemented (4-tier, symbolic s0/BR/mem + Addi/Subi exception, direct BDD== under one map, truth-table synthesis, 1 GiB big-stack thread, fail-closed, BR=0+rom=[word]); bootstrap grew 3→5 samples. Dropped: (a) spike-B decode_execute standalone tier (subsumed by system tier); (b) spike-E3 Add-arm-on-Xor-IR≡identity control (implied by the system tier's frame check, untested in default gate). Neither weakens the claimed theorem.

## Test + clippy
`test result: ok. 4 passed; 0 failed; 3 ignored; finished in 11.21s` (reconcile + convention + 2 negatives; system/full_sweep/bootstrap ignored). Clippy clean. Post-revert re-run identical (11.32s).

## Minor notes (non-blocking)
1. `sv_to_u8` silently reads a non-constant BDD as 0 (bveq_netlist.rs:175-186) — unreachable in the bootstrap (constant inputs); an assert would harden it. 2. `sv_from_flat` panics on a hypothetical non-flat wide-Sum output (133/140) — no such prim today; Err would be stricter. 3. Bootstrap timing print shows the same elapsed twice (cosmetic). 4. `sv_to_u16` exported-unused; consider adding spike-E3's foreign-arm-identity control to the reconcile tier (~100ms).
