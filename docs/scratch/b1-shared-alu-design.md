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
