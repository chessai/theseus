# ADR-0046: M-C2 build — the composite-ring memory bus

**Status:** Accepted — **Layer 1 implemented** (`theseus-tisc`, merged `f5db17d`);
**Stage 1a IMPLEMENTED** (carrier split + `system_step` + composite oracle, merged
`8f27ba0`); **Stage 1b-A IMPLEMENTED** (BusMsg + emit/mem/absorb non-degenerate seam
+ line-audit, value-level, merged `1f59744`); **Stage 1b-B (the SystemRing closure
certificate) is next** (design-resolved §4.5.8); Stage 2 proposed.

> **Stage 1b-A — DONE (merged `1f59744`).** The memory exchange is now
> **non-degenerate** (a real Option-B seam, not Stage-1a's degenerate splice). A
> `BusMsg = (enable, addr, word)` resident factor was added (carrier
> `((CpuState, MemState), BusMsg)`), and `mexch_composite` was replaced by the
> **emit / mem / absorb** 3-cycle: `emit` (predicated on the MEXCH opcode) copies
> `IR.rt→BusMsg.addr` and swaps `reg[rd]↔BusMsg.word`; `mem` is the C1 tree
> **re-keyed on `BusMsg.addr`** (so it touches **zero CpuState fields**); `absorb =
> sym(emit)`. Built as one iso term in the in-revolution order
> `fetch;decode_execute′;emit;mem;absorb;unfetch;pc_add_br;pc_inc` (no seam rotation,
> IR is the decode latch). **The §6 line-audit passes** (mechanical netlist check:
> mem-phase cells touch no CpuState field) — the non-degeneracy witness. 3-factor
> reshape via `view_mem_bus`/`view_cpu_bus` (Assoc/Swap); new `encode_system`/
> `decode_system`; **C1 re-verified on the re-keyed `select_mem_on_bus` at N∈{2,4,8}**.
> Verified: `run_on_cpu(system_step)==eval` (spill-15, store→reload), emit/mem/absorb
> self-inverse + correct, value-level bus-quiescence (BusMsg=0 every step boundary,
> the whole-program version `#[ignore]`d for speed — `system_step` eval is ~1.7 s/step),
> zero regression. Design of record: [docs/scratch/stage1b-design.md](../../scratch/stage1b-design.md).
> **Still value-level (no ring); the closure certificate is Stage 1b-B.**

> **Stage 1a — DONE (merged `8f27ba0`, two increments).** `MemState` is externalized
> from `State` into a separate iso factor; the carrier is `(CpuState, MemState)`
> (`CpuState` = 20 fields, `State` narrowed 24→20). **Two step isos** (Decision 1 of
> [docs/scratch/stage1a-design.md](../../scratch/stage1a-design.md)): the ring keeps
> `step:CpuState↔CpuState` (`Op::Mexch` removed → `decode_execute′`); the oracle uses
> `system_step:(CpuState,MemState)↔(CpuState,MemState)`. FIX-3 was done as
> **width-parameterized flat routing** (`route_to_head_n`/`at_fields_n`/`on_fields_n`
> at `n=20` for CpuState, `n=MEM_WORDS` for MemState) — *not* a generic `at_composite`
> — plus one isolated cross-factor primitive **`cross_ctrl_fredkin`** (1a-A, proven
> self-inverse in isolation). `StateVals` stays flat (iso-level-only split, bridged by
> `encode_composite`/`decode_composite`). **Key result: Conjecture C1 is now discharged
> on the *real* re-targeted `select_mem` lowering at N∈{2,4,8}** (Layer 1 had only the
> standalone `build_mexch_prim` proxy — the reviewer's D3 risk is retired). Verified:
> composite oracle `run_on_cpu(system_step) == eval` (spill-15, store→reload), a
> shared-ISA divergence guard (`decode_execute′` == `proj_CpuState(system_step)` on the
> non-MEXCH domain), ceiling `14+N / 12+N` unchanged, zero M-A/M-B/M-C1 regression.
> Honestly **degenerate** (Prop 5.1); the §6 line-audit / non-degeneracy is Stage 1b. Advisor-authored from the resolved scoping
([ADR-0045](0045-mc2-memory-bus.md) §0.5) and the formal proofs
([theory/reversible-memory-closure.md](../theory/reversible-memory-closure.md)),
then corrected by two adversarial reviews (§4.5.0) and a **2-agent H3 design spike
that discharged the realizability obligation** (§4.5.8): Stage 1b is buildable with
no seam rotation, no decode latch, no prologue/epilogue — one in-revolution
transaction order (§4.1) + a bus-quiescence certificate check. Implementation is
staged and each stage is gated by its differential oracle **and** its closure
certificate; it touches the ring and `step`, so it carries the CLAUDE.md
orchestration bar (adversarial review + real integration tests, per stage).

**Layer 1 implementation note (what shipped, `MEM_WORDS = 4`).** The addressed-
exchange substrate landed exactly per §3.1: `State` gains 4 resident memory words
(→ 24 fields / 192 bits); a self-inverse **`MEXCH rd, addr`** instruction
(`reg[rd] ↔ mem[addr]`, R-format opcode `0b1001`) whose decode is the **1-of-N
Fredkin exchange tree**; and `compile.rs` targets `Location::Mem(addr)` homes with
the transient-squat zero-parking (§3.1). **Conjecture C1 is DISCHARGED** — the tree
is whole-domain ancilla-clean at `N ∈ {2,4,8}` (`mem_exchange` 4/4). Verified by the
run-on-CPU oracle: `run_on_cpu` (real `step`) `== eval` on the sampled inputs for a
15-leaf spill-to-memory program, the max-ceiling program, and a store→reload
round-trip; fast-sim green whole-domain; over-ceiling programs reject cleanly. The
honest ceiling holds: **14 + N straight-line, 12 + N branching**.

**The area cost, made concrete (validates Stage 1).** Resident memory widens `State`,
which inflates the ring-lowering cost — the heavy integration tests (`ring_fib` 778s,
`feistel` 379s, `fib_cpu` ~400s at N=4) blew the full-suite gate timeout and are now
`#[ignore]`d (green pre-merge via `--ignored`, same discipline as the real-CPU
oracle tests). This is **Theorem 6.1's area cost in practice**: Option-A resident
memory does not scale in `N` because it bloats the step body the ring materializes —
exactly why **Stage 1 externalizes** memory into a shallow loop.

## 1. Decision and honest framing

We **build** the external Option-B memory bus — realized, per the proofs, as a
**composite ring**: the certified unit grows to `CPU ⊗ bus ⊗ memory`, closing under
the *same* zero-garbage invariant the ring already certifies (Theorem 3.5). We do
this knowing (Theorem 6.1) it is **not an energy win** — `N` resident words cost
`N·w` token-moves/tick in any topology. The payoff (Corollary 6.2) is architectural
and strategic: a **CPU body invariant in `N`**, a separately-characterized memory
block in the energy ledger, ~10²–10³× **area** reduction of the buffer wall, and —
decisively — **the seam behind which Option C** (static adiabatic SRAM, the only
real escape from `O(N)`-per-tick holding, ADR-0029) drops in without touching the
CPU. Per-ring closure is *not* attempted: it is provably impossible (Theorem 2.3).

## 2. The build layers (each independently gated)

> A prior draft mis-scoped the first layer as "pure `compile.rs`". An adversarial
> review corrected it: **there is no pure-compiler memory layer.** A runtime-
> effective `LOAD`/`STORE` needs the CPU to *decode* it (a new `Op` + the 1-of-N
> select tree in `decode_execute`, `decoder.rs`) and a physical *home* for the
> words (`State` is fixed at 160 bits, `types.rs:27-30`, so it must grow a bank) —
> else the real CPU no-ops the reserved opcode (`decoder.rs`) and the L19 real-CPU
> oracle cannot gate it. Compiler-scope with CPU-authority is unbuildable.

- **Layer 1 — addressed-exchange substrate + resident memory** (CPU **and**
  compiler; **orchestration-critical**). `State` gains an `N`-word memory bank;
  `decode_execute` gains `LOAD`/`STORE` = the **1-of-N reversible addressed-
  exchange tree** (§4.3, generalizing `select_fields`); `instr.rs`/`asm.rs` gain
  the encodings; `compile.rs` targets memory addresses (§3). This is honest
  **Option-A capacity** — memory resident in the carrier, deliberately *degenerate*
  (the §6 line-audit does **not** yet apply; the oracle compares the CpuState
  projection to `eval` of the surface iso — Prop 5.1 — so there is nothing
  degenerate to catch). **Conjecture C1 — the tree's whole-domain ancilla-
  cleanliness — is discharged HERE at `N ∈ {2,4,8}`: the load-bearing risk is
  *front-loaded*, not deferred.** Carries the full CLAUDE.md bar.
- **Stage 1 — externalize: single-body composite ring** (closure gate). Lift
  `MemState` out of the CPU carrier and re-place the exchange as its **own iso**
  crossing a resident `BusMsg` (Lemma 3.3), so the §6 line-audit passes; run through
  the *existing* `StepRing` (bus + tree scratch in the ancilla section) and confirm
  the composite closure certificate (Theorem 3.5). **Reused from Layer 1 verbatim:
  the tree machinery, the ISA, the compiler targeting. What changes: *where* the
  exchange sits** — spliced into `step` (Layer 1) → a separate factor (Stage 1);
  the review's honest correction to "reuse verbatim". This exercises the real
  closure question with **zero new ring machinery**.
- **Stage 2 — two-body `SystemRing`** (physical separation). Make `mem` its own
  recirculating block with staggered ports and compile-time cross-edge arrival
  equations, so `MemState` circulates a shallow loop instead of the full step body
  — where the area payoff (Corollary 6.2) materializes.

Layer 1 is the first delegation (once §3's zero-parking is spec'd and the layout is
settled). Stages 1–2 are gated on the prior stage's certificate.

## 3. Layer 1 — addressed-exchange substrate + resident memory

**CPU side** (orchestration-critical — `types.rs`, `decoder.rs`, `instr.rs`,
`asm.rs`; the reason Layer 1 is *not* compiler-only):

- **`State` memory bank.** Widen `State`/`StateVals` with an `N`-word bank
  (`types.rs:27-30`); extend `encode`/`decode` and the `NUM_FIELDS`-indexed
  field machinery every phase uses (`decoder.rs` `on_fields`/`at_fields`), and the
  `STATE_BITS`/domain-sampler assumptions in `ring_run.rs` and the oracle.
- **`LOAD`/`STORE` decode.** New `Op` variants + `load_instr`/`store_instr` in
  `decode_execute`, each the **1-of-N addressed-exchange tree** (§4.3) — this is
  the C1 gate. Encodings in `instr.rs`/`asm.rs` (free opcodes, `addr` in
  `rt`/immediate, §7).
- **Real-CPU authority.** `run_on_cpu` executes `LOAD`/`STORE` on the real `step`,
  so the oracle is authoritative (L19) — which is *only* possible because the CPU
  decodes them; a compiler-only layer would leave the real CPU no-opping the
  opcode.

**Compiler side** (`compile.rs`, extends M-C1's `Ctx`/`Location`/`with_registers`/
marshalling):

- **`Location::Mem(addr)`** homes for leaves beyond the register+scratch file;
  `LOAD`/`STORE` = the one addressed exchange (`word ↔ mem[addr]`, move discipline).
  Compile-time addressing (the compiler knows the address — no data-dependent
  addressing; that is M-D).
- **`with_registers` extends** to page a `Mem`-home leaf into a register via `LOAD`
  and back via `STORE`. The **zero-parking obligation is non-negotiable** (Lemmas
  7.1/7.2): a swap-based `LOAD` into an *occupied* register displaces the other
  leaf into `mem[addr]` (the loaded leaf's home), so a **free-zero cell** and a
  **3-cycle** (a 2-cycle cannot, Lemma 7.2) are required to park the displaced leaf
  at rest. **This blocks delegation until written** to ADR-0044 §2.3–2.4
  concreteness: name the park-cell source (a designated rest register/scratch/
  memory cell), the per-site free-zero set, and the error path.
- **The ceiling is NOT `14 + N`.** The park cell is charged, exactly as M-C1's aux
  drops the branching ceiling 14→12. **Re-derive the honest ceiling** (straight-
  line and branching) from the free-zero-cell accounting; error `TooManyLeaves`
  when no park cell exists. Asserting `14 + N` repeats the dishonesty M-C1's review
  corrected.
- **Oracle:** `run_on_cpu` (real `step`) `== eval` of the *original surface iso*
  (memory is a **compilation artifact**, never surface-visible — Proposition 5.1);
  whole-domain fast-sim + real-CPU sample per L19; **plus the C1 whole-domain
  cleanliness demonstration at `N ∈ {2,4,8}`** — Layer 1's defining gate.

**Settle before delegating.** The `MemState` word layout, address encoding, and
`BusMsg` line layout must match ADR-0045's setup (dual-rail, `word:W8`, `addr:AddrW`)
**now**, so Stage 1's externalization does not force a re-marshal of Layer 1's work.
The two settled items follow (§3.1).

## 3.1 Finalized Layer-1 spec (the two settled items)

**MemState layout.** `MEM_WORDS = N` words of `W8`, resident in `State` as fields
appended after the 8 scratch words: field order becomes
`[r0..r7, PC, BR, IR_lo, IR_hi, s0..s7, m0..m_{N-1}]`, `NUM_FIELDS = 20 + N`.
**Default `N = 8`** (3-bit address fits the `rt` field; `State` → 224 bits). The
addressed-exchange tree and marshalling are **parameterized by `N`**; C1 is
demonstrated whole-domain at `N ∈ {2,4,8}`. (If the real-`step` oracle is
intractable at N=8, the shipped default may drop to N=4 — note it explicitly; the
tree + C1 demonstration stay at {2,4,8}.)

**One instruction — `MEXCH rd, addr`** (`reg[rd] ↔ mem[addr]`, self-inverse). There
is no separate LOAD vs STORE: memory access *is* exchange (ADR-0029), so
"LOAD"/"STORE" are compiler-intent names for the one `MEXCH`. Encode **R-format,
opcode `0b1001`**, `rd` in the rd field, `addr` in the `rt` field (3 bits, N≤8;
extensible to `rs`+`rt` = 6 bits, N≤64, later). Decode = select `reg[rd]` (the
existing 1-of-8 `select_fields`) and `mem[addr]` (a **1-of-N** `select_fields`
generalization), swap, unselect — this mem-select crossbar **is the C1 tree**.

**Canonical layout + homes.** Leaf `L`: `L<8 → Reg(L)`; `8≤L<14 → Scratch(L−6)`
(`s2..s7`); `14≤L<14+N → Mem(L−14)`. `marshal`: `i<8→regs[i]`, `8≤i<14→scratch[i−6]`,
`14≤i<14+N→mem[i−14]`; `unmarshal` mirrors; `Ctx::new` inits canonical homes.

**Zero-parking — resolved (Lemma 7.2, the transient-squat option).** Paging a
`Mem`-home leaf into a register for an op uses the **`with_registers` sandwich**,
identical in discipline to M-C1's scratch paging: `MEXCH` the leaf into a spare
register (the swap transiently squats the spare's leaf in `mem[addr]`), run the op,
`MEXCH` back (self-inverse) — canonical restored at the boundary, **no free-zero
cell consumed** (the squat is undone within the sandwich, Lemma 7.2's legal case).
A free-zero cell is needed *only* for the **branch aux zero-source** (M-C1 §2.4,
Lemma 7.1), now drawn from any unused (marshalling-zeroed) scratch *or* memory word.

**Honest ceiling (Lemma 7.1 accounting).** Straight-line: **14 + N**. Branching:
**12 + N** (two free-zero cells consumed to zero the two aux registers).
`num_leaves` cap = `14 + N`; `allocate_aux` errors `TooManyLeaves` when it cannot
secure two free-zero cells. This is *not* a flat `14 + N` — the park cells are
charged, exactly as M-C1's 14→12.

**BusMsg layout (pinned for Stage 1; not built in Layer 1).**
`BusMsg = (enable:Bit, addr:⌈log₂N⌉, word:W8)`, dual-rail — so Stage 1's
externalization reuses Layer 1's `MemState` word layout and address encoding
without a re-marshal.

## 4. Stage 1 — single-body composite ring

### 4.1 The transaction order (CORRECTED — no seam rotation; §4.5.8)
> **This subsection's original "seam rotation" was WRONG** and is corrected by the
> H3 spike (§4.5.8). The rotated order `absorb ; unfetch ; pc_update ; fetch ;
> decode_execute ; emit` puts the wrap cut *between* `emit` and `absorb` — a
> **straddling transaction, which Prop 3.4 proves fatal** (the bus carries a live
> request across the wrap). It cited Prop 3.4 while implementing its counterexample.

**Correct order (Lemma 3.3's own factorization `N₂ = U∘A∘X∘E∘V`):** build
`system_step = fetch ; decode_execute′ ; emit ; mem ; absorb ; unfetch ; pc_add_br ;
pc_inc` as **one iso term** — the memory transaction (`emit ; mem ; absorb`) sits
**interior** to a single revolution. Then the wrap cut (after `pc_inc`) falls
between `absorb` and the next `fetch` — exactly where Prop 3.4 requires — the bus is
`0_b` at every wrap by construction (§4.5.8's 3-cycle trace), **H3 discharges
trivially, and no seam rotation / prologue / epilogue is needed** (rev 1 runs
correctly from IR=0, uniform body). `decode_execute′` = `decode_execute` with
`Op::Mexch` removed (its opcode now keys `emit`/`absorb`).

### 4.2 `BusMsg` as a resident composite ancilla (Lemma 3.2)
`BusMsg = (enable, addr, word)` is a **resident, marshalling-zeroed factor** of the
carrier (Proposition 4.1: it cannot be `uniti`-introduced). It is data-dependent
mid-revolution but returns to `0_b` at every boundary via the emit/exchange/absorb
conjugation (Lemma 3.3) — which, by Lemma 3.2, makes it **admissible as a composite
ancilla under the existing (ZG)**. So it joins `ι` (the ancilla init) and needs no
new certificate machinery.

### 4.3 The memory iso (the 1-of-N Fredkin exchange tree)
`mem : (MemState, BusMsg) ↔ (MemState, BusMsg')`. When `enable`: a Branch/CSWAP
tree on the `addr` bits steers `word ↔ MemState[addr]` — the ADR-0027 cell,
generalizing `decoder.rs`'s 1-of-8 `select_fields` crossbar to 1-of-N. Controls are
pass-through (the one legal fanout, ADR-0029); the tree is value-permuting with the
routing brackets self-cancelling (`Seq(sel, Seq(core, Sym(sel)))`, as `select_fields`
already is). **This is Conjecture C1** — cleanliness is argued by analogy, not
proven; Stage 1's exit criterion is to *demonstrate* it whole-domain at
`N ∈ {2,4,8}` (`ancilla_clean.rs` style). If a construct stays K-dirty, the scoped
`O(log N)` Bennett fallback (control subcircuit only, never the `2·w` full wrap)
applies.

### 4.4 The composite certificate (generalize `ring_run.rs`)
`system_step : (CpuState, MemState) ↔ (CpuState, MemState)` = `step′` with `mem`
composed and `BusMsg` pinned at rest on both sides. In Stage 1, run it through the
**existing `StepRing`** (bus + tree scratch in the ancilla section). The certificate
is the existing one, generalized (Definition 3.1, ZG-2):
`restored_ok ∧ run == run_open ∧ run == eval(system_step)^R` — **plus** the
structural anti-degeneracy gate (§6). Note Proposition 1.3: `run == run_open` is
strictly weaker than pointwise restoration, so keep the `debug_assert` on
`ancillas_restored` too.

### 4.5 Finalized Stage 1 spec (for delegation)

> **Corrected after an adversarial review (§4.5.0) — Stage 1 is split into 1a
> (delegatable now) and 1b (open obligation).** The subsections §4.5.1–§4.5.7 below
> are the *original* finalized draft; three of their claims were broken and are
> superseded by §4.5.0. Read §4.5.0 first.

#### 4.5.0 Review correction — the split, and three fixes

A fresh-context adversarial review broke the one-task framing on three grounds; all
three are corrected here.

- **FIX 1 — `MemState` is DATA, not a boundary-quiescent ancilla (a real
  contradiction).** §4.5.1/§4.5.6's "MemState pinned at its init at every revolution
  boundary" is *false*: `MemState` is a persistent spill store — a value written in
  revolution *k* must still be there at *k+j* (Lemma 2.2's `STORE…;LOAD` witness), so
  it is emphatically **not** boundary-zero mid-program. The proofs already place it
  correctly: `system_step : (CpuState, MemState) ↔ (CpuState, MemState)`, `data =
  (CpuState, MemState)` (Def 3.1, Thm 3.5). **Only `BusMsg` (+ tree scratch) is the
  boundary-quiescent ancilla** that Lemma 3.2 admits. In the ring, `(CpuState,
  MemState)` goes in the **data** section (both recirculate, persisting), `BusMsg` in
  the **ancilla** section (restored to 0 each revolution). Putting `MemState` in the
  ancilla section would make `StepRing::run` re-zero it every revolution and erase
  every spilled value. The oracle's "no domain explosion" conclusion still holds, but
  for the correct reason: `MemState` is init-0 at **program entry** (marshalling) and
  **projected away at comparison** (surface programs aren't memory-typed, Prop 5.1) —
  *not* because it is boundary-quiescent.
- **FIX 2 — the seam-rotation premise is false against the code; it is an OPEN
  obligation, not a phase reorder.** The decoder keeps **nothing decoded resident**
  (only the raw `IR` word; decode is fused into the predicated gates, `decoder.rs`).
  So "`emit` reads the live decoded instruction (`addr`, `reg[rd]`)" (§4.5.5, and
  Lemma 3.3's "fields persist in CpuState") cannot be done by reordering the existing
  5 phases — it needs **new resident decoded fields + an `emit`/`absorb`/prologue/
  epilogue construction** that does not exist and is the proofs' unproven **H3**.
  This is Stage 1b's design problem, not a delegatable increment.
- **FIX 3 — "re-target the tree by passing indices" is not real.** `at_fields`/
  `on_fields`/`route_to_head` are hardwired to `NUM_FIELDS`/`state_ty()`
  (`addressing.rs`), so operating over a `(CpuState, BusMsg, MemState)` product needs
  the **addressing layer made composite-carrier-aware** — a refactor, not an index
  change. And C1 was tested on a *standalone reconstruction* (`build_mexch_prim`,
  `tests/mem_exchange.rs`), not the real `select_mem` lowering, so **cleanliness of
  the actual re-targeted tree must be verified**, not assumed.

**The split.**

- **Stage 1a — value-level externalization (DONE, merged `8f27ba0`; see the boxed
  summary at the top of this ADR and [docs/scratch/stage1a-design.md](../../scratch/stage1a-design.md)).**
  Move `MemState` out
  of `State` into a separate factor; make the addressing layer composite-aware;
  re-target `MEXCH` to swap `reg[rd] ↔ MemState[addr]` (external), composed into
  `step` at the **value level** — **no `BusMsg`, no seam-rotation, no ring.** Prove
  `system_step : (CpuState, MemState) ↔ (CpuState, MemState)` `== eval` on
  spill-to-memory programs, under the existing full-step `run_on_cpu` loop
  (whole-domain fast-sim + real-CPU sample; re-verify C1 on the real re-targeted
  tree). This exercises FIX 1's carrier split + FIX 3's composite addressing + the
  composite-iso plumbing (theseus-core has no shared-factor combinator, so thread the
  product by explicit `Times`/`AssocProd` routing) — and *nothing else*. It is
  honestly **degenerate** (Prop 5.1; the ring/line-audit distinguish Option-B later),
  which is fine: it is a refactor-and-plumbing de-risk, not a closure claim.
- **Stage 1b — the composite ring (open; own design pass).** Introduce `BusMsg` as
  the resident port between `step` and a *separate* `mem` iso; solve the H3
  emit/absorb/decode-latch/prologue construction (FIX 2); seam-rotate; run through
  `StepRing` with `(CpuState, MemState)` in data and `BusMsg`+scratch in ancilla;
  prove the composite certificate (`restored_ok ∧ run == run_open ∧ run == eval`) +
  the line-audit. Gated on 1a green **and** the H3 construction being designed &
  reviewed. This is where non-degeneracy (Option-B) is actually established.

---

*Original finalized draft (superseded in part by §4.5.0):*

Concrete decisions, grounded in the post-Layer-1 code. **What Stage 1 reuses from
Layer 1: the exchange tree (re-targeted, §4.5.4), `StepRing` (verbatim), the
compiler's `Location::Mem` targeting. What changes: `State` (memory externalized),
`step`'s phase order (seam rotation), and where the exchange sits (a `BusMsg`-
crossing `mem` iso).**

**4.5.1 The oracle contract (resolves the "biggest obstacle").** Memory stays a
**hidden compilation artifact**, *never* a surface type (Proposition 5.1). The
oracle therefore enumerates the **same surface-input domain as Layer 1** — there is
**no domain explosion**. `run_on_cpu` compiles the surface iso to a `system_step`,
runs it (which manages `MemState` internally, marshalling-initialized to 0), and
compares the **CpuState projection** of the output to `eval(surface iso)`. `MemState`
is an internal factor pinned at its init at every revolution boundary, not a degree
of freedom the oracle ranges over. This keeps the Layer-1 oracle harness essentially
intact (it gains a MemState-projection step, not a bigger domain).

**4.5.2 Externalize `MemState` from `State`.** Move the `m0..m_{N-1}` fields out of
`State` (`types.rs`): `CpuState` returns to 16 fields (`[r0..r7,PC,BR,IR_lo,IR_hi,
s0..s7]`), and a separate `MemState` = `N` × `W8`. Split `encode`/`decode`
accordingly. The composite carrier is `(CpuState, BusMsg, MemState)` (a product);
`BusMsg = (enable:Bit, addr:⌈log₂N⌉, word:W8)` is a **resident, marshalling-zeroed
factor** (§4.2).

**4.5.3 The composite must be ONE iso term.** `StepRing` threads ancillas
*holistically* over the netlist it lowers, so `system_step` must be a **single
`Iso` expression** — `step′` (seam-rotated, §4.1) with the `mem` iso composed *in*
via `Times`/`AssocProd` plumbing (theseus-core has no shared-factor combinator, so
the `BusMsg` factor is threaded by explicit assoc/swap routing). Do **not** compose
`step` and `mem` as two runtime-separate isos — the ring would not thread their
ancillas together, and the certificate would not close.

**4.5.4 Re-target the exchange tree (already C1-clean).** C1 is **already
discharged** (Layer 1: `select_mem`/`ctrl_word_swap` are whole-domain ancilla-clean
at `N ∈ {2,4,8}`). The tree operates on **generic field indices** (`on_fields`), so
re-target it from `reg_field(0) ↔ mem_field(addr)` to `BusMsg.word ↔ MemState[addr]`
by passing the composite-product field indices. **Re-verify cleanliness after
re-targeting** (same structure, new indices — should hold; the `mem_exchange` test
extends to the composite tree). This is the corrected reading of §4.3: not "prove
C1" (done) but "re-target the proven-clean tree."

**4.5.5 Seam rotation — the PC-constancy trap.** `step′ = absorb ; unfetch ;
pc_update ; fetch ; decode_execute ; emit` (§4.1). The subtlety (`step.rs` invariant
D1): PC must stay constant across a given instruction's `fetch…unfetch`. In `step′`
that span *crosses the revolution boundary* (fetch+decode_execute+emit at end of rev
k; absorb+unfetch at start of rev k+1) — it stays correct **only because `pc_update`
is placed *after* unfetch**, so PC is unchanged from fetch through unfetch. `emit`
reads the still-live decoded instruction (`addr`, `reg[rd]`) into `BusMsg`; `absorb`
folds `BusMsg'.word` back — both require IR still holding the instruction, i.e.
unfetch after absorb. A first/last partial-revolution prologue/epilogue handles the
boundary; its semantics must be oracle-checked.

**4.5.6 The line-audit (§6) and the certificate (§4.4).** After externalization the
audit is meaningful: assert no `MemState` field index reaches a CPU-phase active cell
(mechanical netlist incidence check) — the witness that this is *not* Layer 1's
degenerate splice. Certificate at `N ∈ {2,4,8}`: `restored_ok ∧ run == run_open ∧
run == eval` on the surface domain. ~~`MemState`/`BusMsg` boundary-quiescent~~
**[STRUCK — FIX 1 landmine]:** `MemState` is DATA and is **not** boundary-quiescent
(it holds spilled values across boundaries); **only `BusMsg`** is boundary-zero, and
that requires the explicit bus-quiescence check of §4.5.8 (the existing
`ancillas_restored` does not cover it, since `BusMsg` lowers to *data* ports).

**4.5.7 Start small, prove the certificate first.** Widening the carrier with `BusMsg`
+ the composite plumbing will (again) stress the ring lowering. **Begin at `N = 2`**,
get `system_step` closing the certificate end-to-end, *then* scale to `{4,8}`. The
heavy integration tests stay `#[ignore]`d (Layer-1 precedent). If the composite will
not close the certificate at `N = 2`, **STOP and report the smallest failing state**.

#### 4.5.8 H3 spike resolution (2 Fable agents — Stage 1b is BUILDABLE)

Two independent Fable design spikes (constructive + adversarial) **converged**: the
H3 realizability obligation is discharged in design — Stage 1b is buildable, with
**no seam rotation**, **no new decode latch**, and **no prologue/epilogue**.

- **The transaction is an in-revolution insertion, not a rotation (§4.1 corrected).**
  `system_step = fetch ; decode_execute′ ; emit ; mem ; absorb ; unfetch ; pc_add_br
  ; pc_inc`, one iso term. The rotated order was Prop 3.4's forbidden straddle.
- **No decode latch (FIX 2 was overcautious).** The persistent raw IR *is* the
  latch: `emit` and `absorb` re-derive `(enable, rd, addr)` from IR via the existing
  `r_pred(0b1001)`/`select_reg(R_RD)` machinery. **`absorb = Iso::sym(emit)`** — one
  line. IR persists interior to the revolution (set by `fetch`, cleared by `unfetch`
  after `absorb`); PC is untouched by emit/absorb.
- **The construction** (all reuse existing decoder idioms): `emit` (predicated on
  `r_pred(0b1001)`) sets `enable ^= p`, `addr ^= IR.rt`, and swaps `reg[rd] ↔
  BusMsg.word`; `mem` = the **re-keyed C1 tree** (controls = `BusMsg.addr`, *not* IR
  — so it touches **zero** CpuState fields, passing the §6 line-audit) doing
  `word ↔ MemState[addr]` gated on `enable`; `absorb` = `sym(emit)`. The
  `emit→mem→absorb` **3-cycle provably returns `BusMsg` to `0_b`** (the zero `emit`
  parks in `reg[rd]` is the same zero that re-enters the bus at `absorb` — zeros
  conserved, Lemma 7.1/3.3(b)).
- **The one honest new ring machinery — a bus-quiescence check.** `BusMsg` is a
  carrier-type factor, so it lowers to **data** ports (`n_data = inputs −
  lowering_ancillas`), *not* the ancilla section. So `ancillas_restored` does **not**
  cover it. Add a `SystemRing` check: assert the `BusMsg`-designated data lines `= 0`
  at every revolution boundary — else the certificate greenwashes ZG-over-enlarged-
  data instead of ZG-2 (Prop 1.3 silent-residue risk). Data partition:
  `(CpuState, MemState)` + `BusMsg` are data (MemState persists, BusMsg checked-zero);
  lowering scratch is ancilla.
- **Residual Stage-1b work** (all bounded, gated as usual): the FIX-3 addressing
  refactor (Stage 1a); **re-verify C1 on the real re-keyed `select_mem` lowering** at
  `N ∈ {2,4,8}` (Layer 1's discharge used a standalone reconstruction); remove
  `Op::Mexch` from `decode_execute′`; the `SystemRing` bus-quiescence extension. The
  "seam rotation" is re-homed to Stage 2 as a port-placement question (and may be
  unneeded there too, §5).

## 5. Stage 2 — two-body `SystemRing`

`CompositeRing { cpu: CompactBalanced, mem: CompactBalanced }`: the CPU body's
bus-out ports exit at internal depth, the mem body enters/exits at its natural
depths, cross edges carry fixed transit delays. Generalize ADR-0033 D2's checks
with per-cross-edge arrival equations (all compile-time constants) and a 1:1
period lock (`D_mem | D_cpu`, choose equal; pad the shallow mem body with delay
counts). `MemState` circulates only the mem sub-loop — the area payoff. Same
semantics, same certificate as Stage 1; what Stage 2 adds is physical separability
and a separately-characterized memory block in the ledger.

## 6. Anti-degeneracy — the MemState-line audit (Proposition 5.2)
The value oracle is **provably blind** to Option-B-vs-degenerate-A (Proposition
5.1). Non-degeneracy is therefore a **structural, mechanical check on the netlist**:
no `MemState` port index reaches a CPU-phase *active* cell (it may ride pure K0
delay only). A builder assertion rejects any composite that violates it.

**The audit applies at Stage 1+, NOT Layer 1.** Layer 1 is *intentionally
degenerate* — `MemState` is resident in the carrier and the exchange is composed
into `step`, which the audit would (correctly) reject. That is fine: Layer 1 claims
only Option-A capacity, and its oracle (CpuState projection vs. `eval` of the
surface iso) is meaningful precisely because there is no memory-typed surface
program to be degenerate about (Prop 5.1). The audit becomes the honest witness of
externalization exactly when Stage 1 lifts the exchange into a separate factor — and
passing it is Stage 1's definition of "not Option A with indirection".

## 7. ISA
`LOAD`/`STORE` in the free opcode space (10 slots: 7 R-format, 3 I-format), `addr`
in the `rt`/immediate field per the `Xchg` template. The 8-bit immediate caps
`N ≤ 256` — adequate for compile-time-addressed spill; a wider address (multi-word
or a base register) is deferred with data-dependent addressing to M-D.

## 8. Acceptance, per stage (per L19)
- **Layer 1:** `run_on_cpu`(real `step`) `== eval`(surface iso) — pure-spill-to-
  memory program (>14 leaves) and a store+reload round-trip; whole-domain fast-sim +
  real-CPU sample; negatives error cleanly; no M-A/M-B/M-C1 regression.
- **Stage 1:** the composite certificate (§4.4) holds whole-domain at `N ∈ {2,4,8}`;
  **C1 demonstrated** (tree ancilla-clean); the line-audit (§6) passes.
- **Stage 2:** the two-body run `== system_step^R` (Stage 1); arrival equations and
  period lock validated; line-audit passes.

## 9. Energy (honest, per Theorem 6.1)
Not a win — `O(N)`-per-tick holding is topology-invariant (Axiom E1, storage is
motion). The memory block sums into `energy.rs` as its own ledger line (ADR-0028),
making ADR-0029 C3.5's wake condition **measurable** — which is exactly the meter
that will later justify Option C. Anyone reading this as cheaper joules is reading
it wrong (Corollary 6.2).

## 10. Proof obligations / risks (the review must probe these)
- **C1 (load-bearing).** The 1-of-N tree's whole-domain cleanliness is a
  *conjecture*. Stage 1 exists to discharge it at small `N` before any scaling; a
  failure is a redesign of the tree lowering, not a small fix.
- **Zero-parking (§3).** Under-specifying the free-zero-cell discipline re-opens the
  M-C1 hole (Lemmas 7.1/7.2); spec it to ADR-0044 §2.3–2.4 concreteness.
- **Seam-rotation realizability.** Reordering `step`'s phases must preserve the
  fetch/unfetch/PC invariants (ADR-0034); the prologue/epilogue for the first/last
  partial revolution must be semantics-checked.
- **Domain coverage.** The `2^(160+8N)` composite domain forces sampling for the
  real-CPU authority (as M-C1); design the sample around the exchange-boundary
  states a residue bug would hide in.
- **Degeneracy.** Without the §6 line-audit, a green oracle proves the wrong artifact
  (Proposition 5.1). The audit is not optional at Stage 1+ (it does not apply to the
  intentionally-degenerate Layer 1, §6).
- **Layer-1 scope is CPU-wide, not compiler-only** (§2 correction). It touches
  `types.rs` (State bank), `decoder.rs` (LOAD/STORE decode + the C1 tree),
  `instr.rs`/`asm.rs` (encodings), and `compile.rs` — full orchestration bar, not
  the small pure-compiler task a prior draft implied.
- **Exchange-placement rework across the Layer1→Stage1 boundary.** Layer 1 splices
  the exchange into `step`; Stage 1 re-places it as a separate factor to pass the
  audit. The tree/ISA/compiler-targeting reuse verbatim, but the *placement* is
  re-architected — plan for it, and settle the `MemState`/`BusMsg` layout up front
  (§3) so the marshaller is not redone.

## Cross-references
- [theory/reversible-memory-closure.md](../theory/reversible-memory-closure.md) —
  the formal results this build realizes (Thm 2.3, 3.5, 6.1; Lemmas 1.4, 3.2, 3.3;
  Props 3.4, 4.1, 5.1, 5.2; Conjecture C1; Lemmas 7.1/7.2).
- [ADR-0045](0045-mc2-memory-bus.md) — the scoping + spike resolution.
- [ADR-0033](0033-ring-machine.md) / [ADR-0040](0040-rail-role-swap-cell.md) — the
  ring + zero-garbage closure this generalizes.
- [ADR-0029](0029-memory.md) / [ADR-0027](0027-cell-library.md) — exchange
  discipline, Options A/B/C, the Fredkin cell.
- [ADR-0044](0044-mc-compiler-memory.md) — M-C1 spill, the Layer-1 base.
- [ADR-0008](0008-differential-testing.md) — the oracle norm (L19).
