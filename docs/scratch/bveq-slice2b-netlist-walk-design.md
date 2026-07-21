# BVEQ slice-2b — the symbolic netlist walk (un-ignore the netlist reconciliation)

**Status:** design **r2** (post design-review + spike). Author: advisor. Target: a NEW netlist→BDD encoder
= new trust core → CLAUDE.md non-negotiable. Builds on slice-1 (`bveq`, merged `5154522`).

---

## r2 revision — folding the adversarial design-review + spike (`docs/reviews/bveq-slice2b-design-review.md`)

Verdict **SHIP-WITH-FIXES**. The spike walked **all 13 register-pure instructions** symbolically and got
exact equality with `instr_semantics` — one-arm, full `decode_execute`, AND the full 8-phase `system_step`
— ground-truthed against real `run_rom_on_cpu` (3/3 agree). BDDs trivially small (≤26 nodes). The r1 body
below stands EXCEPT these overrides. **The spike test source is ~90% of the slice — resurrect it.**

- **T3 ELIMINATED:** walk the full `system_step([word])` (not just one arm) as the authoritative tier —
  3.4–3.6 s/op, exact equality + clean frame (PC=1, IR=0, bus=0, mem restored, ancillas 0) for all 13.
  This proves the theorem about the REAL machine's step, so §5 Q1's assumption list is no longer assumed.
  One-arm `instruction(op)` walks (62–394 ms) are the fast DEFAULT tier but NOT sole authority (can't see
  predicate-overlap / surrounding-phase regressions).
- **MAJOR-1 (perf claim corrected — the §1/§7 "ms, complete, default" was wrong by ~2 orders).** Adopt the
  **4-tier** test structure (replaces §3's single default test):
  1. `bveq_netlist_walk_reconcile` — DEFAULT (~5–15 s): one-arm, 13 ops, 2–3 selector/imm configs each
     (cover rs=0, high rt, a carry op, a rotate, both 3-ops), full-frame. **This is the un-ignore win**:
     complete in operand VALUES, in the default gate.
  2. `bveq_netlist_walk_system` — DEFAULT-slow / short `#[ignore]` (~46 s): full `system_step`, 13 ops,
     1 config each — the whole-step authority (discharges T3); runs in CI.
  3. `bveq_netlist_walk_full_sweep` — `#[ignore]` (~19 min): one-arm, EVERY selector pair/triple + every
     immediate — **RETIRES `bveq_reconciliation_lb_netlist_wide` (2.6 h)** with strictly greater coverage.
  4. `bveq_netlist_walk_bootstrap` — short `#[ignore]` / default-slow (~30–40 s): 3–5 concrete
     walk-vs-`run_rom_on_cpu` samples (T2 real-machine anchor — mandatory, the walk shares a convention
     surface with `instr_semantics`). Keep existing default `bveq_reconciliation_lb_netlist_sampled`.
- **MAJOR-2 (init state):** regs, s0, BR (+ mem in the system tier) SYMBOLIC — proves the strong
  content-agnostic contract at zero cost (≤26 nodes). s1–s7 = 0; PC = 0; IR = concrete word (one-arm) or
  0 with `rom=[word]` (system tier); BR = 0 in the system tier (PC_out=BR+PC+1). **EXCEPT Addi/Subi
  require s0 = 0** (unpredicated immediate mask, decoder.rs:720 — spike E2: symbolic-s0 correctly REJECTS
  Addi); pin s0=0 for the two I-format adds only + document why (terminal invariant / P-checker).
- **MAJOR-3 (no equiv reuse):** `equiv` takes `RegTransfer`(BvExpr) both sides + builds its own internal
  `VarMap` → unusable. Use **direct canonical-BDD `==` under ONE shared extended `VarMap`** (regs + s0 +
  BR + mem operands = the single TCB mapping; encode `instr_semantics` through it; cex via
  `xor().sat_witness()`), OR refactor `equiv` to expose an inner `equiv_bdds(&[Bdd],&[Bdd],&VarMap)`.
  Netlist reconcile is w=8 → I4 vacuous, `operand_regs`/N5 is a RED HERRING here; full-frame is what
  matters (check the WIDER frame: regs + scratch + PC + IR + BR + mem + bus).
- **MAJOR-4 (gate builders):** do NOT hand-write gate BDDs. SYNTHESIZE each gate's BDD by minterm
  expansion of the prim's own admission-checked `PrimDef::forward` (≤16 rows × ≤4 wires — the identical
  table `theseus_eval::eval` runs). Collapses T1 to a bit-order/`Left=1` convention test, and covers the
  two the r1 gate list forgot: `gfredkin` (K2, decoder.rs:106) and NOT (= `SwapSum(One,One)`, a STRUCTURAL
  case, not a prim).
- **MINOR-5:** run the shipped test in a spawned BIG-STACK thread (~1 GiB) — Iso trees + `adjoint()`
  recursion overflow the 2 MiB cargo-test worker stack (passes on main thread, blows up under `cargo test`).
- **MINOR-6:** `eval_bdd` fails CLOSED (Err, not panic) outside the loop-free product fragment {Id, Sym,
  Seq, Times, SwapProd, AssocL/RProd, UniteProd, UnitiProd, SwapSum(1,1), Prim}.
- **MINOR-7:** scope — per-instruction claim is for `rom=[word]` at PC=0 (multi-entry fetch = whole-program
  oracles); keep ≥1 carry-chain + 1 rotate + 1 3-op per tier.

**SV representation:** `SV = Unit | Bit(Bdd) | Pair(SV,SV)` mirroring `Value`; bit convention pinned by
`theseus_netlist::bits_of` (`Left(Unit)=true=1`). The interpreter mirrors `theseus_eval::eval` structure.

---

## 1. Goal & deliverable

Prove `netlist(instr) ≡ instr_semantics(instr)` at **full `Flat(8)`** for every register-pure instruction
by **symbolically evaluating the decoder's per-instruction netlist over BDDs**, replacing the 5.56 s/input
pointwise `system_step` eval. **Deliverable: the netlist reconciliation flips from `#[ignore]` (the
2.6-hour `bveq_reconciliation_lb_netlist_wide` pointwise sweep) to a FAST DEFAULT symbolic check** (ms),
complete at full width. This is the slice the design-review (BLOCKER-1/L-c) promoted from an open question.

Slice-1's reconciliation proved `symbolic ≡ exec_instr` (the fast-sim). Slice-2b closes the last leg:
`symbolic ≡ real netlist` — completely and cheaply — so the fast-sim `exec_instr` is no longer a
sampled-only proxy for the real machine at the per-instruction level.

## 2. The construction (grounded in the scouting, docs/scratch + memory bveq-overnight-run)

- **The target is a bounded Iso.** `decoder::instruction(op)` (decoder.rs:842) is PUBLIC, ROM-independent,
  **loop-free** — a finite DAG (≤~150 gates) of `Seq`/`Times`/routing/`Prim` nodes on `CpuState` that IS
  the combinational decode+execute function for one opcode. No `Trace`/`Iter`/`Fold`.
- **A symbolic interpreter** `eval_bdd(&Iso, state) -> state` where `state: Field→[Bdd; 8]` (a BDD per
  wire bit), structurally recursing EXACTLY like `theseus_eval::eval` (interp.rs:31–280): `Seq` threads,
  `Times` fans out over product factors, the routing formers (`SwapProd`/`Assoc*`/`Unite*`/`Distrib`/
  `Factor`/`Sym`) permute the field map at **zero cost**, and `Prim` applies the gate as a BDD operation.
- **Gate BDD builders** for the 5 primitives (CNOT/Toffoli/CCC-NOT/Fredkin/NOT, decoder.rs:114–139): each
  a small hand-written per-output-bit BDD over 2–4 wire BDDs (e.g. CNOT: `out1 = in1.xor(in0)`; Toffoli:
  `out2 = in2.xor(in0.and(in1))`). Small + reviewable — the new TCB.
- **Initial state:** operand-register bits = free BDD variables (reuse slice-1's `VarMap` — the SAME
  pinned interleaved mapping, so the result compares directly against `instr_semantics` via `equiv`);
  IR = the constant opcode word (so the arm's predicate is constant-true and its gates fire); PC/BR/scratch
  = constants; mem/bus irrelevant for register-pure ops.
- **The check:** run `eval_bdd(instruction(op), init)`, extract the operand registers' output BDDs, and
  compare against `instr_semantics(op)` — ideally by feeding both into slice-1's `equiv` (reusing its
  full-frame + I4 + counterexample machinery), or a direct per-output-bit BDD `==`.

## 3. Un-ignore plan

- New DEFAULT test `bveq_netlist_walk_reconcile`: for each register-pure instruction, `eval_bdd` the
  netlist and assert `≡ instr_semantics` at full `Flat(8)`. Fast (ms/instr).
- The old `#[ignore]` `bveq_reconciliation_lb_netlist_wide` (pointwise) is SUPERSEDED. Keep a **small
  bounded bootstrap** (`bveq_netlist_walk_bootstrap`, ~8–12 samples, default-fast or a short `#[ignore]`)
  that validates the *walk itself* against the real `run_rom_on_cpu`/`system_step` on concrete inputs —
  ground-truthing the new netlist→BDD encoder against the actual machine. (The walk is a new TCB; it must
  be checked against reality, not merely against `instr_semantics`, else a shared bug in both is invisible.)

## 4. New TCB / soundness obligations (design-review targets)

- **T1 gate-builder faithfulness:** each gate BDD builder must equal the prim's forward truth table over
  its whole 2–4-wire domain (a cheap exhaustive test).
- **T2 walk faithfulness (the bootstrap, §3):** `eval_bdd(instruction(op), ·)` must agree with the real
  `system_step` on a sample — the encoder is ground-truthed against the machine.
- **T3 the decode_execute-vs-system_step gap:** the walk evaluates `instruction(op)` (one arm) or
  `system_decode_execute()` (all 15 arms). The real authority is the full 8-phase `system_step`. For
  register-pure ops the other phases (fetch/unfetch cancel; emit/mem/absorb are no-ops; pc_* are PC-only)
  should not touch the operand registers — but this is an ASSUMPTION the review must verify, not assume.
- **T4 thermometer↔bits:** the Iso's `Prim`s are defined over `Value` (thermometer `Flat(8)`); the walk
  operates at bit level. The translation (`bits_of`/`value_from_bits`, types.rs:275) must be faithful, and
  the gate builders must match the prims' BIT-level action.
- **T5 ancilla:** `instruction(op)` restores s1=0 by construction; the walk must show the scratch/predicate
  wires return to their initial constants (else the "combinational function" claim is false).

## 5. Open questions for the design-review (please BUILD a spike to resolve)

- **Q1 (the big one):** walk `instruction(op)` ALONE, or the full `system_decode_execute()`? The former is
  bounded/cheap but assumes "non-matching arms are identity + the surrounding phases don't touch operand
  regs" (T3). The latter is more faithful but a bigger BDD. Spike both; measure; pick the one that is both
  SOUND (provably equals the real per-instruction register effect) and cheap.
- **Q2:** reuse slice-1 `equiv` for the final compare (gets I4/full-frame/counterexample free), or a direct
  BDD `==`? If reusing `equiv`, the operand_regs must be derived correctly (the N5 obligation).
- **Q3:** how big do the BDDs get for the real decoder arms (predicate Toffoli-chains + crossbar Fredkin
  mux trees + core gates)? Scouting estimated ≤24–30 vars; confirm the arm's internal wires (predicate,
  carry chain, mux controls) don't blow it up. Measure per-instruction node counts + wall-clock.
- **Q4:** which instructions? The register-pure subset slice-1 already models {Add,Sub,Xor,Exch,Rotl,Rotr,
  Cswap,Ccnot,Addi,Subi,Xori,Rotli,Rotri}. MEXCH/MEXCHR are excluded (decoder.rs:814, memory phase).
- **Q5:** does this actually let us DELETE the pointwise `#[ignore]` sweep, or must a bounded pointwise
  bootstrap remain as the walk's ground truth (§3 argues the latter — confirm)?

## 6. Scope / what this is NOT

- NOT the recognizer rewire (that's slice-2). ADDITIVE + a test-tier change; no `recognize_prim` change.
- Does NOT un-ignore the WHOLE-PROGRAM real-CPU oracles (mdd/mde/ring) — those test sequencing/memory/
  timing/energy across a full ROM, outside the per-instruction combinational fragment; they stay advisor-run.
- The joule/energy model is untouched.

## 7. Success criteria

`bveq_netlist_walk_reconcile` green in the DEFAULT gate for all register-pure instructions at full
`Flat(8)`, in milliseconds; the walk ground-truthed against real `system_step` on a bounded sample; the
2.6-hour `#[ignore]` pointwise sweep retired (or demoted to the bounded bootstrap). Gate-builder + walk
faithfulness tests (T1/T2) green. Full theseus-tisc suite green (additive).
