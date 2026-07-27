# B-1 — Shared configurable ALU: design + joules baseline

**Status:** DRAFT (Phase B / B-1 prep, investigation only — no code changed).
**Worktree:** `/tmp/phaseb` (`phaseb-shared-alu`, off merged v2 `903475e`).
**Goal:** break the O(|ISA|) per-step decode tax. Route operands once, select ONE
configurable ALU by opcode, keep the single all-gated step-iso the ring proof needs.
**Deliverable of this doc:** the measured baseline, the all-gated shared-ALU design,
the quantified expected win, the slice plan, and the open questions.

All numbers below are measured live in this worktree at `T_RAMP = 1e-7` under the
lean-lower_iso `Arc<Type>` repr (which made the wide-state lowering fit RAM), with
`MALLOC_ARENA_MAX=1`.

---

## 1. The measured baseline — the O(|ISA|) tax

### 1.1 Step-level breakdown (fib's own 24-word ROM step net, the committed golden)

`me_a3_golden` (energy.rs test), the canonical fib price:

| quantity | value |
|---|---|
| `step_j` (per physical step) | **2.5259595646e-9 J** |
| `total_j` = B·step_j | 6.0623029551e-8 J |
| B (WCET physical steps) | 24 |
| T (productive steps) | 20 |
| park tax (B−T)/B | 16.7% |

Per-clause table (`print_clause_energy_table`, 2-word test ROM step net) — the
**step's cell mass**, which IS `step_j` (energy is per-cell, data-independent):

| clause | cells | active (J) | buffer (J) | total (J) |
|---|---:|---:|---:|---:|
| fetch | 6 866 | 8.64e-13 | 6.35e-12 | 7.21e-12 |
| **decode_execute** | **874 510** | 2.44e-11 | 8.28e-10 | **8.52e-10** |
| pc_inc | 4 338 | 6.21e-13 | 3.97e-12 | 4.59e-12 |
| **step (full)** | **1 291 717** | 2.74e-11 | 1.224e-9 | **1.2515e-9** |

**`decode_execute` is 874 510 / 1 291 717 = 67.7% of the step's cells, and its
8.52e-10 J is 68.1% of step energy.** (The rest: fetch, pc_inc/pc_add_br, the
crossbar/park/PC plumbing, and — in the full `system_step` — the mem body.)
Decode is where the O(|ISA|) tax lives, and it is >2/3 of every step.

Note the buffer (balance K0) energy dominates active energy ~34× — decode is a deep,
skewed netlist, so any cell we remove from decode also removes its downstream balance
buffers. Cutting decode cells is super-linear in energy.

### 1.2 The per-op cores (the O(|ISA|) factor), measured

`per_instruction_lowering_and_semantics` lowers each predicated `instruction(op)` iso
(its OWN crossbar + predicate chain + core) standalone:

| op | total cells | K1 (routing/crossbar) | core (R1/K2 = real compute) |
|---|---:|---:|---:|
| Add | 814 | 760 | 54 R1 |
| Sub | 814 | 760 | 54 R1 |
| Xor | 672 | 656 | 16 R1 |
| Exch | 656 | 648 | 8 R1 |
| Rotl | 665 | 640 | 17 K2 + 8 R1 |
| Rotr | 665 | 640 | 17 K2 + 8 R1 |
| Cswap | 1 074 | 1 056 | 8 K2 + 10 R1 |
| Ccnot | 1 104 | 1 088 | 16 R1 |
| Xchg | 354 | 336 | 8 K2 + 10 R1 |
| Swapbr | 240 | 232 | 8 R1 |
| Addi | 408 | 340 | 68 R1 |
| Subi | 408 | 340 | 68 R1 |
| Xori | 250 | 236 | 14 R1 |
| Rotli | 243 | 220 | 17 K2 + 6 R1 |
| Rotri | 243 | 220 | 17 K2 + 6 R1 |
| **Σ (15 ops, replicated)** | **≈ 8 610** | **≈ 8 172 (95%)** | **≈ 438 (5%)** |

**The load-bearing finding: ~95% of every op's cells is crossbar/routing (K1
Fredkins), only ~5% is the actual arithmetic/logic core.** The op cores are tiny
(Add's real adder = 54 cells; Xor = 16; the whole gated_tk adder is ~a hundred K1/K2
when gated). What is expensive is *routing the operands into position*, replicated per
op family.

### 1.3 How many op-cores fire (masked) per step — the O(|ISA|) factor

`decode_execute` = `decode_execute_shared()` runs, every step, ALL 15 non-MEXCH cores
gated on their opcode predicate; opcodes are mutually exclusive so **at most one is
semantically live, but all 15 cores' cells are physically instantiated and clocked
every step** (the all-gated form). Plus the two MEXCH/MEXCHR emit blocks in
`system_step`. So the masked-core factor is **|ISA| = 15 (CPU) + 2 (mem) = 17 gated
cores per step**, of which 16 do nothing on any given cycle.

A-0 already shared the crossbar *within* two groups (Group A reg-reg: one fetch_rs +
one sel_rd; Group B immediates: one sel_rd). But the residual still instantiates
**~12 operand-crossbar traversals per step**:

- Group A `with_two_regs`: 2 (fetch_rs + sel_rd)
- Ccnot: 3 (ccnot_fetch_rt_br + with_two_regs = fetch + target)
- Cswap: 3 (copy_rd0 target + with_two_regs = fetch + target)
- Xchg: 2 (select_operand rd + select_scratch rt)
- Swapbr: 1 (with_target)
- Group B: 1 (sel_rd)

Each traversal is a 16-way `select_operand` ≈ 104 K1 cells (the 8-way `select_reg` is
a pinned 56 K1; the 16-way adds the level-3 range). At ~104 cells × 12 traversals ×
(forward+adjoint) that is the bulk of decode's crossbar mass — the residual O(|ISA|)
replication A-0 did **not** remove.

### 1.4 The ISA-growth tax (measured historically)

Adding `Ccnot` to `Op::ALL` grew decode by **+9.15%** at f3.5 and **+1.91%** at the
byte-parallel redefine (MEMORY.md, "ISA-GROWTH ENERGY TAX"). Every new op adds a
crossbar + predicate + core to the all-gated decode → **`step_j` rises for EVERY
program**, even programs that never use the new op. This is the efficiency
whack-a-mole B-1 exists to kill.

---

## 2. The all-gated shared-ALU design

### 2.1 Shape

Replace the N replicated `(crossbar + predicate + core)` blocks with **one shared
datapath**:

```
  fetch/route ONCE  →  [ D-slot, A-slot, B-slot ]         (the shared 3-slot operand file)
  decode opcode ONCE →  ~6 ALU-config control lines        (O(log|ISA|) decode)
  ONE configurable ALU core, its sub-functions gated by the config lines
  route back ONCE   →  registers restored
```

**The 3 operand slots** are exactly A-0's fixed working slots: `D = r0-slot`
(`reg_field(0)`, the routed rd target), `A = s0` (`AUX_OPERAND_FIELD`, the swap-fetched
rs value), `B = BR` (`BR_FIELD`, the 2nd source used today only by Ccnot). Immediates
land in `A` via the existing IR-imm CNOT masks. The slots already exist; B-1 makes
*every* op read/write only these slots so the routing can be hoisted to a single
bracket for the whole ISA (A-0 proved this hoist is exact for the ops that fit —
§2.4).

**The configurable ALU core** is the union of the existing verified gated cores, each
gated on a decoded config line instead of its own opcode predicate:

| ALU sub-function | reuses (verbatim) | serves opcodes | config |
|---|---|---|---|
| adder (in-place TK) + `dir` | `gated_tk_gates` / `Iso::sym` | Add, Sub, Addi, Subi | `is_add ∧ dir` |
| xor-merge | `gated_xor_gates` | Xor, Xori | `is_xor` |
| barrel-rot + `dir` | `gated_rotl_gates` / `Iso::sym` | Rotl, Rotr, Rotli, Rotri | `is_rot ∧ dir` |
| word-swap (Fredkin) | `exch_core` | Exch | `is_exch` |
| controlled-swap | `cswap_instr` core | Cswap | `is_cswap` |
| and-accumulate (CCC-NOT) | `ccnot_core` | Ccnot | `is_ccnot` |

The headline sharing: **one adder serves Add/Sub/Addi/Subi** (today 4 separate
netlists — Add/Sub 814 each, Addi/Subi 408 each ≈ 2444 cells collapse toward one
adder core), and **one rotator serves Rotl/Rotr/Rotli/Rotri**. Sub/Subi = the adder's
adjoint (`dir` line selects forward vs `Iso::sym`), reusing the *exact* verified
`gated_tk_gates`; same for rot direction. Reg-vs-immediate is just which slot feeds
`A` (register swap-fetch vs IR-imm CNOT mask) — chosen once by the format bit — not a
different core.

### 2.2 The decode: opcode → ALU config (O(log|ISA|))

Today: each op runs its own `pred_compute` = a Toffoli AND-chain over 4–5 IR literals
→ its predicate bit, per op (15 predicate chains/step). B-1 computes the ALU config
lines ONCE from the opcode field:

- `is_add`, `is_xor`, `is_rot`, `is_exch`, `is_cswap`, `is_ccnot` = a small decoder
  (one Toffoli-tree per config line, or a shared 4→16 one-hot decode of the low
  opcode bits) — `O(log|ISA|)` gates producing `O(|ISA|)` one-hot lines, computed
  once, uncomputed once at step end.
- `dir` = opcode bit distinguishing Add/Sub, Rotl/Rotr (already a single opcode bit).
- `fmt` = IR bit 31, chooses register-A vs immediate-A once.

The config bits are scratch (like s1's predicate bit today) — computed by a Toffoli
tree, held for the ALU core, uncomputed by the adjoint. This replaces 15 per-op
predicate chains with one decode.

### 2.3 Preserving the single step-iso (the ring/injectivity proof constraint)

The constraint (from the synthesis, §2 Axis-4 resolution): B-1 MUST keep the
conservative **all-blocks-present-but-gated** form, so `step` remains ONE fixed iso
(the ring-closure + injectivity proof depends on a single fixed step netlist; the
active-only microcode form is deferred to B-3). B-1 therefore does NOT make unused
sub-blocks free — all ALU sub-functions' cells are still instantiated every step. The
win is removing the **×|ISA| replication of routing + predicate**, not making inactive
cores vanish.

**Why the shared form is bijective and behaviour-identical (the proof sketch):**

1. **Route-once is exact iso algebra (A-0's lemma, already proven).** For ops sharing
   the same routing bracket `B` (a self-inverse crossbar composition),
   `Σ_i B ; pred_i(core_i) ; B⁻¹ = B ; (Σ_i pred_i(core_i)) ; B⁻¹` because between
   consecutive ops `B⁻¹ ; B = id` cancels exactly. B-1 extends A-0's Group-A/B hoist
   to the *whole* ISA by making every op read/write only the fixed D/A/B slots (§2.1),
   so ONE bracket serves all ops. This is the same lemma A-0 already relies on;
   B-1's new obligation is that the extended slot discipline (B-slot for the 3-operand
   ops, immediate-into-A) holds for every op — checked per-op by whole-domain equiv.

2. **Mutual exclusivity ⇒ the gated ALU is a Branch at the value level.** The config
   lines are a one-hot decode of the opcode (mutually exclusive by construction — one
   opcode fires at most one config line). Each ALU sub-function is `Id` off its config
   line (all its gates carry the line as a control), so at most one sub-function is
   non-identity for any IR word. The whole ALU is thus a value-level Branch tree =
   bijective (each arm is a bijection; disjoint domains). This is the same
   predicate-Branch argument the current per-op dispatch uses, moved inside one core.

3. **The step iso is still ONE fixed netlist** — `decode_execute` = `route ; decode ;
   ALU ; route⁻¹ ; decode⁻¹`, a single iso, lowered once, ring-closed once. No
   per-op-active variation. The ADR-0034 total-bijection proof is width-parametric and
   structural (Seq/Times/Branch of bijections) → it survives unchanged.

**The obligation this creates (the B-1 gate):** per-opcode whole-domain equivalence
(shared-ALU output == the current `instruction(op)` output for every input, per
opcode) + mutually-exclusive control-decode (no two config lines co-fire; off-opcode
words are identity) + `run_on_cpu == eval` + ring closure + re-pinned energy goldens.

### 2.4 Ops that need care (from A-0's shared-crossbar experience)

A-0 kept Ccnot/Cswap/Xchg/Swapbr OUT of its shared groups because their routing
bracket DIFFERS (Ccnot routes rt→BR outside sel_rd; Cswap routes rt not rd + copies
rd[0]; Xchg drives a 2nd scratch crossbar; Swapbr's rs=0 aliases rd under Group A's
fetch). B-1's single-bracket goal must **absorb these into the shared bracket** or
justify keeping them separate:

- **B-slot (BR) as the universal 2nd source.** Make the shared bracket fetch rt→B for
  every 3-operand op (Ccnot, Cswap), so Cswap/Ccnot join the shared route. This is the
  hardest part — the bracket must fetch rd→D, rs→A, rt→B unconditionally, and each op
  reads the slots it needs. Cswap's rd[0]-control-copy and Xchg's scratch crossbar are
  the residual specials; they may stay as small separate blocks (each is one extra
  crossbar, not the dominant cost) — B-1 need not fold 100% to win, and the slice plan
  (§4) folds the cheap-and-safe families first.

---

## 3. Expected win + cost

### 3.1 Quantified step_j win

Decode = 67.7% of step cells / 68.1% of step_j (§1.1). Within decode, ~95% of the
per-op mass is crossbar/routing (§1.2), replicated ~12 crossbar traversals/step (§1.3).

- **Route-once across the whole ISA:** collapse ~12 crossbar traversals toward ~3
  (one D/A/B route). That removes ~9/12 = 75% of the crossbar mass. Crossbar is ~95%
  of decode cells → decode drops toward ~30–50% of its current size.
- **Core sharing:** Add/Sub/Addi/Subi → one adder (4→1); Rotl/Rotr/Rotli/Rotri → one
  rotator (4→1). Cores are only ~5% of decode, so this is a smaller absolute win but
  it is what makes op-addition free (§3.3).

**Honest estimate (matches the synthesis' ~2–4× band):** decode_execute cells
874 510 → **roughly 300k–450k** (2–3×), step_j 2.526e-9 → **~1.2e-9–1.7e-9 J**
(step ≈ decode 68% + fixed 32%; cutting decode ~2.5× cuts the step ~1.6–1.9×). Total
fib total_j 6.06e-8 → **~3.5e-8–4.5e-8 J**. This is a real, measurable win but NOT
orders of magnitude — the all-gated constraint keeps every sub-block instantiated.
The buffer-energy super-linearity (§1.1, buffers ~34× active) may push the win to the
better end since removed crossbar cells drop their downstream balance buffers too.

**This estimate MUST be validated by a real `lower_iso → balanced_energy` measurement
on the shared-ALU `step` net before commit** (per the synthesis P1/P2 caveat) — the
single-netlist gate count vs the sum-of-parts is the one unmeasured bet.

### 3.2 Cost / risk

- **A new configurable core + opcode→config decoder** — a datapath change = the
  daemon-equivalent control surface → CLAUDE.md highest bar (adversarial review + real
  authority round-trip).
- **Re-prove the single step-iso** — bijectivity of the gated ALU (Branch argument),
  mutual-exclusivity of config decode, ring closure re-run.
- **Re-pin ALL energy goldens** — `me_a3_golden` (step_j/total_j), the clause table,
  any per-op golden. step_j drops, so every golden that pins it moves. This is
  expected and is the *proof* the win landed; the re-pin is the deliverable, not a
  regression.
- **Slot-discipline risk** — the aliasing caveats A-0 documented (rd==rs, Ccnot BR
  residues, Cswap control copy) must be re-audited for the universal B-slot fetch.

### 3.3 ISA-growth tax elimination — the strategic payoff

Today a new op = a new (crossbar + predicate + core) in the all-gated decode → taxes
every program (+9.15% for Ccnot). With the shared ALU: a new op is **a new ALU
config line + (if its function isn't already present) a small sub-block**. If the new
op's function reuses an existing sub-block (e.g. any new add/xor/rot variant, or an
op expressible as an existing `BvExpr` pattern — see the synthesis §8 keystone), it
costs **one config line ≈ O(1) cells** — effectively free in step_j. Only a genuinely
new *function* adds a sub-block, and even that adds no new crossbar/predicate. **Yes:
the shared ALU makes op-addition ~free** for functions already in the ALU, and O(core)
— not O(crossbar+predicate+core) — for new functions. This is the unbounded-ISA-at-
O(1)-step-energy payoff, and it aligns the ISA with the synthesis' `BvExpr`-evaluator
endgame (the shared configurable ALU IS a hardware `BvExpr` evaluator).

---

## 4. Slice plan (smallest first, each with POC + real authority)

Each slice: build the shared sub-block, prove per-opcode whole-domain equivalence
against today's `instruction(op)`, `run_on_cpu == eval` on a real program, ring
closure, re-pin energy. Measure step_j drop each slice (it should be monotone down).

**B1.0 — measurement harness + baseline (no datapath change).** Add a test that
lowers `decode_execute()` and reports decode cells + step_j, so every subsequent slice
shows the delta. Pin the current baseline (874 510 decode cells; step_j 2.526e-9).
Cheapest, de-risks the rest. (This doc already captured the numbers.)

**B1.1 — fold Group-A arithmetic into one configurable core (Add/Sub/Xor via one
route + gated sub-blocks).** Smallest real datapath slice: Group A already shares ONE
route (fetch_rs + sel_rd). Replace its 6 per-op predicated cores with the shared ALU
core reading D/A, gated by config lines: `is_add ∧ dir` (Add/Sub share `gated_tk_gates`
± adjoint), `is_xor`, and fold Exch/Rotl/Rotr as the exch/rot sub-blocks. This is the
lowest risk (route already shared; cores are verbatim-reused; §2.3 lemma 1 already
holds for Group A) and proves the config-decode + gated-ALU pattern. POC:
`run_on_cpu == eval` on a program using add+sub+xor (e.g. the byte-XOR reduce or a
tiny add loop); whole-domain per-opcode equiv on Add/Sub/Xor/Exch/Rotl/Rotr; re-pin.

**B1.2 — fold Group B (immediates) into the SAME ALU core.** Addi/Subi reuse B1.1's
adder (immediate into A via IR-imm CNOT instead of rs fetch, chosen by `fmt`);
Xori/Rotli/Rotri reuse xor/rot. This is where the 4→1 adder and 4→1 rotator sharing
lands (the biggest core-collapse). POC: fib (uses Addi/Subi/Xori/Rotli) `run_on_cpu ==
eval` + re-pin `me_a3_golden` (the headline step_j drop shows here).

**B1.3 — fold the controlled ops (Cswap, Ccnot) via the universal B-slot.** Extend the
shared bracket to fetch rt→B; Cswap/Ccnot read D/A/B. Highest risk within B-1 (the
different-bracket ops, aliasing residues) → own adversarial review. Xchg/Swapbr may
stay as separate small blocks if folding them costs more proof than they save (each is
one crossbar). POC: a program using Ccnot/Cswap `run_on_cpu == eval` + whole-domain
equiv + re-pin.

**B1.4 — final measurement + golden re-pin + ISA-growth demonstration.** Confirm the
step_j win end-to-end; add a regression test that adding a hypothetical new op that
reuses an existing sub-block does NOT change step_j (proves the growth tax is dead).

### Highest-bar gates (per CLAUDE.md — decoder/datapath + step-iso re-proof)

- **Fresh-context adversarial review** (different model from implementer) on each
  datapath slice (B1.1, B1.3 especially): enumerate the states the shared bracket + ALU
  move between, ask what on-disk/in-iso assumption each relies on and what defeats it
  (slot aliasing, config co-fire, off-opcode identity, ring closure).
- **Real authority per slice:** `run_on_cpu == eval` on a real program through the
  shared decoder (not just unit tests of the sub-blocks) + whole-domain per-opcode
  equivalence vs `instruction(op)` + ring closure (`validate() == Ok`, bus-quiescent) +
  the energy re-pin as the win-proof.
- **Each adversarial finding ships a reproducing regression test** (L20).

---

## 5. Open questions

1. **Single-netlist gate count vs sum-of-parts (the one unmeasured bet).** The
   all-gated shared form still instantiates every sub-block; does lowering the shared
   `step` net actually hit the 2–3× decode reduction, or does the shared bracket's
   fan-out (one route feeding all cores) create balance-buffer skew that eats the win?
   Must measure a real lowered shared-ALU step net before B1.1 commits.
2. **Universal B-slot fetch vs keeping Cswap/Ccnot/Xchg/Swapbr separate.** Folding all
   4 into one bracket maximizes the crossbar collapse but multiplies the aliasing proof
   burden. Is the marginal crossbar saved (a few hundred cells) worth the review cost,
   or is B1.3 better scoped to Ccnot+Cswap only, leaving Xchg/Swapbr replicated?
3. **Config decode form:** one-hot 4→16 decode of the low opcode bits (dense, one
   shared tree) vs per-config Toffoli chains (sparse, like today's per-op predicates).
   The one-hot decode is O(log|ISA|) once but needs `2^4` output lines held in scratch;
   the scratch budget (s1 today holds pred bit 0 + chain 1..3 + CSWAP bit 4) may need a
   wider config-scratch field — a state-width change to price.
4. **Interaction with A-phase widens (if B-1 lands before all of Phase A).** Per-program
   W (B-2) and W_pc widen change slot widths; does the shared ALU need to be
   width-parametric from the start (it should, since the cores already are)?
5. **Relationship to the `BvExpr`-evaluator endgame (synthesis §8).** Should B-1's ALU
   sub-blocks be structured as `BvExpr` lowerings from day one (so B-3/A-6 synthesis
   drops in), or is that premature — build the concrete shared ALU now, unify with
   `BvExpr` later?
6. **Does folding change B (WCET) or only step_j?** It should be step_j-only (same
   instructions, same ROM, same trace) — confirm B=24/T=20 is invariant across every
   slice (a codegen-regression guard, already in `me_a3_golden`).

---

## 6. B1.1 RESULT (measured) + RE-SEQUENCING

**B1.1 shipped as a POC (`07e784a`, Group-A fold) and answered open-Q1 empirically:**

| quantity | baseline | B1.1 | delta |
|---|---|---|---|
| decode_execute (balanced) | 874 510 | 834 940 | −39 570 (1.047×) |
| Group-A portion (balanced) | 164 846 | 148 414 | −16 432 (**1.111×**) |
| step_j (fib) | 2.5259595646e-9 | **2.4672491693e-9** | **−2.324%** |
| B / T | 24 / 20 | 24 / 20 | unchanged |

**Q1 verdict: the shared single netlist SHRINKS — no balance-buffer skew catastrophe (the feared failure mode did NOT happen).** But the Group-A shrink is only **1.111×**, below the ~1.3× bar. **Why:** the design's 2–3× premise rests on collapsing replicated *routing crossbar* (§1.3/§3.1). **A-0 already shared Group A's route** (`with_two_regs` = one `fetch_rs` + one `sel_rd` across all 6 ops — decoder.rs `group_a_shared`). So B1.1 only removed the *predicate-chain* replication (6 opcode AND-chains → 1 shared `alu_config_decode`) + shared the Add/Sub adder & Rotl/Rotr rotator. There was **no routing left to capture inside Group A.** The pattern (config-decode + one gated configurable core + single fixed netlist) is PROVEN correct (6 eval-level semantics teeth pass: full_decoder_{arithmetic,logic,rotates,moves}, reserved-opcodes-identity, dispatch_exclusivity) and measurable — but its cell win is marginal because Group A was already near-optimal post-A-0.

**RE-SEQUENCING (data-grounded, holistic):** the real routing collapse lives OUTSIDE Group A — the 4 controlled ops (Ccnot 3 + Cswap 3 + Xchg 2 + Swapbr 1 = **9 of the 12 traversals**, §1.3) each carry a *private* crossbar; Group B carries 1. Non-Group-A balanced mass = 874 510 − 164 846 = **709 664 cells (81% of decode)**, all still un-shared. The design's incremental slice order (B1.1 Group-A → B1.2 immediates → B1.3 controlled) puts the biggest win LAST; the empirical finding says invert the emphasis:

- **B1.1's infra is KEPT** (config-decode + gated configurable ALU + `*_core_at(cfg)` config-wired cores) — it is the FOUNDATION the universal bracket + core-sharing reuse; a real −2.3% step_j; but NOT merged alone (a full highest-bar review cycle isn't justified by −2.3%).
- **NEXT = the UNIVERSAL 3-operand shared bracket (§2.4):** fetch rd→D / rs→A / rt→B ONCE for the whole step, route the controlled ops (Ccnot, Cswap — the 6-of-12 routing consumers) + Group A/B through it. This collapses ~12 traversals → ~3 = where the design's 2–3× decode reduction (→ ~1.6–1.9× step_j) actually comes from. Xchg (scratch crossbar) / Swapbr (rs=0 aliases rd) may stay separate small blocks if folding costs more proof than they save.
- **Merge as ONE reviewed unit** (B1.1 infra + universal bracket) with the full highest-bar cycle — Fable adversarial review (aliasing/co-fire/off-opcode-identity across the universal bracket) + real system_step authority (run_on_cpu==eval + whole-domain per-op equiv + ring closure) + energy re-pin — amortizing the expensive review over the real win.
- **Gating measurement (this doc's `poc_b1_perblock_measure`):** confirm per-block balanced mass before the big build, so the universal-bracket scope targets the heaviest blocks.

## 7. B1.2 RESULT (measured) — unified reg+imm block, group_b REMOVED

**B1.2 folds the 5 immediate ops into B1.1's shared configurable ALU and DELETES
`group_b_shared`.** rd→D always; the A-slot source is fmt-selected — R-format
swap-fetches rs into s0 (fmt-gated, INERT for I-format), I-format masks the IR
immediate into s0 (¬fmt-gated). Addi/Subi reuse the adder (`CFG_ADD_FWD/REV`),
Xori reuses the xor sub-block (`CFG_XOR`), Rotli/Rotri reuse the rotator
(`CFG_ROT_FWD/REV`) — NO new arithmetic cores. `alu_config_decode` extended to
land BOTH fmt families onto the SAME six config lines (register op and immediate
op with the same low-3 bits fire different lines because the prefix — fmt —
differs). Single fixed step-iso preserved (all sub-blocks instantiated + gated).

| quantity | baseline | B1.1 | B1.2 | Δ vs B1.1 |
|---|---|---|---|---|
| decode_execute (balanced) | 874 510 | 834 940 | **441 015** | −393 925 (**1.893×**) |
| step_j (fib me_a3) | 2.5259595646e-9 | 2.4672491693e-9 | **1.8664606459e-9** | **−24.35%** |
| total_j | 6.0623e-8 | — | **4.4795055502e-8** | — |
| B / T | 24 / 20 | 24 / 20 | 24 / 20 | unchanged |
| unified reg+imm block | (ga 148 414 + gb 107 642 = 256 056) | — | **185 864** | −70 192 (1.378×) |

**The group_b block COLLAPSED.** The unified block (185 864) is only ~25% larger
than B1.1's Group-A block alone (148 414) yet serves ALL 11 reg+imm ops — the
immediate cores fused into the shared adder/rotator (the 4→1 adder + 4→1 rotator
collapse), no separate addi/xori/rotli cores remain. Its own mass dropped 1.38×
vs group_a+group_b.

**The decode-WIDE win (−394k cells, 1.89×) FAR exceeds the unified block's own
−70k reduction — the buffer-balance super-linearity (§1.1).** Removing a whole
top-level block from `decode_execute`'s 6→5 `seq_balanced` drops not just group_b's
~107k cells but its share of the ~50% inter-block balance-buffer overhead across
the sequence. This is the same mechanism the design predicted (§3.1: "removed
crossbar cells drop their downstream balance buffers too"), here amplified by
whole-block removal rather than in-block sharing. **step_j −24.35% vs B1.1 vastly
exceeds the design's incremental-slice estimate (~6–8%)** because block removal —
not just intra-block sharing — hits the balance super-linearity hard.

**Correctness:** whole-domain per-op equivalence (unified block ==
`instruction(op)` over the full 256-value 2nd-operand domain × representative rd
set, all 11 ops: `b12_whole_domain_equiv_{arith,logic_rot}`); config
mutual-exclusion + fmt A-source gating over all R/I opcode words incl. reserved
(`b12_config_exclusion_and_fmt_gate`); the eval-level teeth
(`full_decoder_{arithmetic,logic,rotates,moves}`, `dispatch_exclusivity`, the two
BLOCKER regressions) through the unified `decode_execute`; and the fib real-CPU
oracle (`fib_data_dependent_real_cpu`: compiled 24-instr ROM using Addi/Subi/Xor/
Exch/Cswap/Swapbr on the real `step` iso == eval over all 16 inputs). NOT YET
re-pinned: `me_a3_golden` (new step_j 1.8664606459e-9 / total_j 4.4795055502e-8) —
bundled with B1.1 for one review.

## 8. B1.3 STOP (superseded by §9) — universal bracket CLEAN; config-scratch wall RESOLVED

> **RESOLVED in §9.** The wall below ("10 config-scratch bits needed, s1 holds 8")
> assumed s1 was the only usable position-stable config word. It is not: **s0 is
> also decoder-reserved and position-stable** (`select_operand` excludes positions
> 0/1 = s0/s1 from the level-3 crossbar) AND is guaranteed 0 at config-decode time.
> Relocating the 2 TRANSIENT decode-workspace bits (prefix+chain) from s1 into s0
> frees the 2 s1 bits needed for CFG_CCNOT + CFG_CSWAP. §9 records the built B1.3.
> The analysis below is kept for the historical record.

**Attempted:** fold Ccnot (R 0b1011) + Cswap (R 0b0110) into the reg+imm shared
block via a universal bracket that fetches **rd→D(r0-slot), rs→A(s0), rt→B(BR)**
once, then config-gates all cores on the fixed [D,A,B] slots (design §2.4).

### The datapath fold itself is SOUND (no aliasing hazard)

Traced rigorously (both control polarities, BR round-trip):

- **rt→B (BR) fetch is net-zero for reg+imm ops.** `ccnot_fetch_rt_br` is a
  self-inverse bracket `sel_rt ; swap(r0-slot,BR) ; sel_rt⁻¹`, sequenced OUTSIDE
  sel_rd/fetch_rs (over the un-permuted file). For reg+imm ops the cores never
  touch BR, so `fetch_rt_br ; alu ; fetch_rt_br⁻¹ = id` on BR and on every reg —
  content-agnostic, exactly the A-0 route-once lemma. (Extend the b12 inert-
  crossbar tooth to the rt→B fetch to pin it.)
- **Cswap-via-D[0] reproduces exact semantics AND restores BR** — it is CLEANER
  than today's `copy_rd0`. With rd→D, rs→A(s0), rt→B(BR): the control is D[0]
  read directly (no s1-bit-4 copy); the core swaps A↔B (= s0↔BR) gated by
  `cfg_cswap ∧ D[0]`. Trace (taken): entry BR=br0,r[rs]=x,r[rt]=y →
  fetch_rt_br: BR=y,r[rt]=br0 → fetch_rs: s0=x,r[rs]=0 → swap A↔B: s0=y,BR=x →
  unfetch_rs: r[rs]=y ✓ → unfetch_rt_br: r[rt]=x ✓, **BR=br0 restored** ✓.
  Not-taken: swap inert, all restored. Matches `r[rs]↔r[rt] iff r[rd][0]`.
- **Ccnot** lands on the identical 4 slots the current `ccnot_core` already uses
  ([r0-slot, s0, BR, s1]); `D[i]^=cfg_ccnot∧A[i]∧B[i]` is verbatim.

So the STOP is **not** a §2.4 aliasing hazard — the universal bracket composes
exactly.

### The wall: 10 config-scratch bits needed, s1 holds 8

The config lines live in **s1** (`AUX_PRED_FIELD`, the ONLY decoder-reserved word
that is position-stable — `select_operand` excludes positions 0/1 from the level-3
crossbar; s2..s7 are NOT excluded, so a scratch operand displaces them). B1.2
already uses all 8 s1 bits: 6 held one-hot lines (add_fwd/rev, xor, exch,
rot_fwd/rev) + CFG_PREFIX + CFG_CHAIN (transient decode scratch). Folding Ccnot +
Cswap needs **8 held one-hot lines** (each core gates on its own held config bit —
cswap on `cfg_cswap∧D[0]`, ccnot on `cfg_ccnot∧A∧B`; neither can be gated on raw
opcode IR bits within the prim arity: see next para). The decode still needs ≥2
transient bits (prefix+chain) held *simultaneously* with the reserved 8 one-hot
slots → **8 + 2 = 10 > 8.** Confirmed by an independent fresh-context review.

**Second, independent wall (arity).** The prim vocabulary tops out at **K3 =
3-controlled swap = 5 wires; there is NO K4** (`ir/cell.rs` CellKind; `lower.rs`
refuses "K4 not in vocabulary"). Cswap's core is a *swap* cell (gated_fredkin,
already K2 = 2 ctrl + 2 swap). Gating it on a held `cfg_cswap ∧ D[0]` = 3 ctrl +
2 swap = **K3 (OK, survivable)** — but any attempt to save a slot by adding a raw
opcode bit (e.g. op3) as an extra per-gate control to *distinguish* ccnot/cswap
on a shared line pushes cswap to 4 ctrl + 2 swap = **K4 → hard-refused.** So the
"merge onto one is_controlled line + op3-discriminate" escape is unsound for the
swap-cell (cswap); it would work only for ccnot (a role-swap that synthesizes).

**A bit-recycling construction exists but is fragile.** By opcode mutual
exclusion at most one one-hot is set at any instant, so ≥6 of the 8 slots are 0
and could host the transient prefix/chain — but its correctness rests on that
invariant holding at *every intermediate decode step for every IR word incl.
reserved/aliased ones*, exactly the subtle datapath property that "passes unit
tests but breaks on the first real word" (CLAUDE.md L15). Not shipped without a
fresh-context adversarial review of the *actual* construction.

### Recommended paths (a design fork — open-Q3 anticipated this)

1. **Reserve a 3rd scratch word (s2) as position-stable config scratch** for the
   2 transient decode bits: extend `select_operand`'s level-3 exclusion to
   positions 0/1/2 and reject s2 as a legal operand in `asm` (legal scratch
   operands shrink to s3..s7). Lower risk than a state widen; but it is an
   addressing/ISA change with its own review surface (reduces user scratch).
2. **Widen the config word (state-width change)** — cleanest for the config decode
   but re-pins every energy/ring golden and changes fetch cost.
3. **Scope B1.3 to Ccnot ONLY** (one held line, K3 CCC-NOT gate fits; needs 7
   held + 2 transient = 9 > 8 — still over by 1). Even one controlled op overruns
   s1 by one bit, so path 1 or 2 is required for *any* controlled-op fold.

### Measured potential win (contingent on the config-scratch fix)

Current per-block balanced mass (`poc_b1_perblock_measure`, this session):
reg_imm 185864, **ccnot 75520**, **cswap 63770**, xchg 10859, swapbr 9886 (SUM
345899). Folding ccnot+cswap removes their two private crossbars (~139k cells of
private routing) AND drops the top-level block count **5→3**, shedding their share
of the inter-block `seq_balanced` balance overhead — the same whole-block-removal
super-linearity that made B1.2's decode-wide win (−394k) far exceed the block's
own −70k. Expected to clear a real further step_j win, but **NOT measured** —
the fold cannot be built until the config-scratch wall (above) is resolved.

## 9. B1.3 BUILT — s0-workspace resolution + universal bracket (Ccnot + Cswap folded)

**The escape the §8 STOP missed: s0 as transient decode workspace.** The §8 wall
("10 config bits, s1 holds 8") assumed the 2 transient decode bits (prefix+chain)
had to live in s1 alongside the held one-hots. But **s0 (`AUX_OPERAND_FIELD`) is a
SECOND decoder-reserved, position-stable word** — `select_operand` excludes
positions 0/1 (= s0/s1) from its level-3 crossbar, so s0 is never displaced by a
scratch operand — AND s0 is guaranteed **0 at config-decode time**: it only
receives the A-operand LATER, when `fetch_rs`/`imm_to_a` run *inside* the decode
bracket. So:

1. **Relocated CFG_PREFIX + CFG_CHAIN into s0** (`CFG_PREFIX_S0`/`CFG_CHAIN_S0`,
   s0 bits 0/1). The config-decode carrier grew from `pred_fields()` = [IR_b0..b3,
   s1] (5 fields) to `config_decode_fields()` = [IR_b0..b3, s1, **s0**] (6 fields);
   the prefix/chain `cfg_wire`s now point at s0. The existing compute/uncompute
   logic restores prefix+chain to 0 on the decode FORWARD pass, so **s0 returns to
   0 before `fetch_rs`/`imm_to_a` write it** (the L15 imm_to_a precondition). This
   required reordering the bracket so `decode_config` is OUTERMOST (runs with
   s0 = 0), around the A-operand deposit.
2. **Freed 2 s1 bits → CFG_CCNOT + CFG_CSWAP.** s1 now holds 8 HELD one-hot lines
   (add_fwd/rev, xor, exch, rot_fwd/rev, ccnot, cswap) and **no transient bits**.
   Ccnot (R 0b1011, op3=1) decodes via a new `fmt ∧ op3` prefix branch; Cswap
   (R 0b0110, op3=0, low3=110) joins the existing `fmt ∧ ¬op3` branch (its low-3
   pattern is distinct from the six Group-A ops). Only Ccnot/Cswap fire their
   lines; no reg-op or reserved word mis-fires (config-exclusion tooth).
3. **Universal 3-operand bracket built.** `shared_alu_block`: decode_config
   (outermost) ; fetch_rt_br_gated ; fetch_rs_fmt ; sel_rd ; imm_to_a ; ALU ; …⁻¹.
   All cores (reg+imm + `ccnot_core_at` + `cswap_core_at`) gate on the [D, A=s0,
   B=BR, s1] slots. **Ccnot**: `D[i] ^= cfg_ccnot ∧ A[i] ∧ B[i]` (verbatim CCC-NOT).
   **Cswap**: swap A[i]↔B[i] gated `cfg_cswap ∧ D[0]` (control read DIRECTLY from
   D[0], K3 = 3-ctrl swap, no rd[0] copy — cleaner than the old `cswap_instr`).
   `ccnot_instr`/`cswap_instr` REMOVED from `decode_execute_shared`; block count
   **5 → 3** (shared_alu, xchg, swapbr).

**The one NEW subtlety (found + fixed, not the §8 capacity wall): the rt→B fetch
must be CONFIG-GATED.** For reg+imm ops the rt field is 0 (don't-care). An UNGATED
rt→BR fetch would route r[0]↔BR — and when rd names r0 this collides with the rd
routing and corrupts r[0]/BR (caught by `full_decoder_arithmetic`: Add r0,r1 gave
r0=0/BR−1 instead of r0=0xFF/BR unchanged). Fix: `shared_fetch_rt_br_gated` swaps
r0-slot↔BR only under `CFG_CCNOT ∨ CFG_CSWAP` (two mutually-exclusive gated
Fredkins), so it is the EXACT identity for every reg+imm word (and Swapbr) while
delivering rt→B for Ccnot/Cswap. Regression: `b13_rt_field_inert_for_reg_imm`.

### Measured result (`poc_b13_shared_alu_measure`, this session)

| quantity | baseline | B1.2 | **B1.3** | Δ vs B1.2 |
|---|---|---|---|---|
| decode_execute (balanced) | 874 510 | 441 015 | **337 459** | −103 556 (**1.307×**) |
| step_j (fib me_a3) | 2.5259595646e-9 | 1.8664606459e-9 | **1.5314006103e-9** | **−17.95%** |
| total_j | 6.0623e-8 | 4.4795055502e-8 | **3.6753614647e-8** | — |
| B / T | 24 / 20 | 24 / 20 | 24 / 20 | unchanged |
| top-level block count | 5 | 5 | **3** | −2 |
| merged universal block | (reg_imm 185864 + ccnot 75520 + cswap 63770 = 325 154) | — | **289 496** | −35 658 (1.123×) |

**decode_execute is now 2.59× smaller than the pre-B-1 baseline (874 510 →
337 459) and step_j −39.37% (2.526e-9 → 1.531e-9).** The decode-WIDE win
(−103 556 cells) again FAR exceeds the merged block's own −35 658 shrink — the
same whole-block-removal buffer-balance super-linearity B1.2 saw: dropping the two
private ccnot/cswap top-level blocks (5→3 `seq_balanced`) sheds their share of the
inter-block balance overhead, not just their own routing. The two PRIVATE crossbars
COLLAPSED into the single universal (rd→D, rs→A, rt→B) route (the ccnot/cswap
routing that was ~139k private cells is now one shared bracket). step_j −17.95% vs
B1.2 — a real further win, exactly the "biggest routing collapse lives in the
controlled ops" prediction of §6/§7.

**Correctness (heavy tier, run to completion this session — all PASS):**
- **Ccnot** whole-domain vs `instruction(Op::Ccnot)`: `b13_whole_domain_equiv_ccnot`
  (25 rd/rs/rt index+value cases) + `b13_ccnot_full_rs_sweep` (full 256 rs-values ×
  4 rd × 4 rt = 4096 lanes) PASS. **Cswap** both polarities:
  `b13_whole_domain_equiv_cswap` (25 cases incl. rd even/odd) PASS.
- **All 11 reg+imm ops** re-verified through the re-bracketed + s0-workspace route:
  `b12_whole_domain_equiv_{arith,logic_rot}` at cap 256 (the full byte datapath
  domain, now evaluated through `shared_alu_block`) PASS.
- s0-restoration tooth `b13_s0_restored_to_zero` (s0=s1=0 after decode for all 10 op
  classes) PASS. rt→B inert-for-reg+imm `b13_rt_field_inert_for_reg_imm` PASS.
- Config mutual-exclusion + fmt-gate + off-opcode identity
  `b12_config_exclusion_and_fmt_gate`, `full_decoder_reserved_opcodes_are_identity`,
  `dispatch_exclusivity`, `full_decoder_{arithmetic,logic,rotates,moves}` PASS.
- **`run_on_cpu == eval`** on a real Ccnot+Cswap+reg+imm ROM via full `system_step`
  (`b13_shared_bracket_ccnot_cswap_real_cpu`, 4 register-file cases, both Cswap
  polarities) PASS — terminal s0=s1=BR=0 enforced. **fib re-confirm**
  (`fib_data_dependent_real_cpu`, 24-instr ROM × 16 inputs) PASS.

**NOT re-pinned** (report-only, per task): `me_a3_golden` (new step_j
**1.5314006103e-9** / total_j **3.6753614647e-8**), the clause table decode cells
**337 459**. These move because the win landed; the re-pin is a follow-up after
adversarial review.

### Risks a reviewer should attack

1. **s0-restoration exactness (L15).** The whole scheme hinges on `decode_config`
   restoring s0 bits 0/1 (prefix/chain) to 0 on its FORWARD pass, before
   `fetch_rs`/`imm_to_a` write s0. `b13_s0_restored_to_zero` + the `run_rom_on_cpu`
   terminal check (s0=0) pin it, but a reviewer should re-derive that prefix+chain
   are 0 at end-of-decode-forward for EVERY IR word incl. reserved (both prefix
   branches uncompute; chain uncomputes per land_op).
2. **rt→B config-gating vs the reg+imm rt-field=0 aliasing.** The gated fetch is the
   one non-obvious fix; a reviewer should confirm the swap is EXACTLY identity for
   every non-controlled word (rt→B off) and that the two mutually-exclusive gated
   swaps compose to OR without double-firing.
3. **Ccnot/Cswap decode not mis-firing for reg-ops/reserved.** The op3 split
   (Ccnot `fmt∧op3`, Cswap `fmt∧¬op3` low3=110) must be the ONLY firing path; the
   config-exclusion tooth covers R 0..16 + I 0..8 but a reviewer should check the
   high-opcode-bit don't-cares don't alias a controlled line.
4. **Cswap-via-D[0] semantics + BR restoration.** The control reads D[0] directly;
   swap A↔B = s0↔BR; the fetch brackets must write back to r[rs]/r[rt] and restore
   BR (not-taken must be fully inert). `b13_whole_domain_equiv_cswap` (both
   polarities) + real-CPU pin it.
5. **Config co-fire / off-opcode identity / block-count-3 ring closure.** Not yet
   ring-closed via `validate()` in this session (report-only); the merged block is
   a single fixed iso (all sub-blocks gated), so the ADR-0034 structural bijection
   argument carries, but a reviewer should re-run ring closure before merge.

## Appendix — measurement provenance

- `step_j`/`total_j`/B/T: `me_a3_golden` (energy.rs), committed golden, re-verified as
  the current baseline.
- Step / decode / fetch / pc_inc cell counts + energy: `print_clause_energy_table`
  (energy.rs, `--features heavy-tests --test-threads=1`), run live this session.
- Per-op predicated cell counts: `per_instruction_lowering_and_semantics` (decoder.rs,
  heavy tier), run live this session.
- `select_reg` = 56 K1 (pinned, `select_reg_crossbar_isolated`); 16-way `select_operand`
  ≈ 104 K1 (adds the level-3 range).
- ISA-growth tax history: MEMORY.md (Ccnot +9.15% / +1.91%).
- All at `T_RAMP = 1e-7`, `MALLOC_ARENA_MAX=1`, lean-lower_iso `Arc<Type>` repr.
