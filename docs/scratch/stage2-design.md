# M-C2 Stage 2 — two-body `SystemRing` (build design)

Status: **DESIGN — reviewed (Fable-5 adversarial pass), fixes F1–F6 folded in.**
Verdict was SHIP-WITH-FIXES: the 2a value model is SOUND; 2b's timing certificate
needed real absolute-tick arrival equations (the mod-P draft admitted a full-period-
late bus return — the headline finding, F1). Elaborates ADR-0046 §5. Canonical branch
`chessai/theseus-v2`, tip `a833770` (code/tests `9932ddc`). Stage 1b is COMPLETE:
`system_step` over `((CpuState,MemState),BusMsg)` recirculates through a single-body
`SystemRing` that closes zero-garbage (`run == run_open == eval^R`, bus-quiescent,
§6 line-audit passes — Thm 3.5).

Stage 2 replaces the **one** body with **two** physically-separate recirculating
bodies — a CPU loop circulating `CpuState` and a memory loop circulating `MemState`
— joined by a bus cross-edge. Same semantics, same certificate as Stage 1; what
Stage 2 adds is **physical separability** (the CPU body is invariant in `N`, the
mem body is the only `O(N)` block) and a separately-characterized memory block in
the energy ledger. **Honest framing (ADR-0046 §9): this is NOT a joule win** —
`O(N)`-per-tick holding is topology-invariant. The payoff is area + a measurable,
separable memory ledger line, not cheaper energy.

## 0. The load-bearing constraint that shapes everything

`CompactBalanced::simulate(input) -> outputs` is **whole-body only**
(`crates/theseus-netlist/src/balance.rs`): it walks the entire acyclic body input→
output in one call. There is **no** mid-body injection/extraction, no per-tick step,
no interior port access. (ADR-0033 D2 deliberately keeps `PhasedCellNetlist` a single
DAG; the wrap edge lives *outside* it.) There is today **no** multi-body / cross-edge
/ coupled-loop machinery anywhere in the tree — Stage 2 is the first.

Consequence: the bus round-trips to memory **mid-revolution** (between `emit` and
`absorb`), so a single whole-body CPU netlist cannot "pause" at the bus crossing.
Therefore the **value model** of the two-body ring must split the CPU revolution at
the mem seam into two bodies — `cpu_pre` (…`emit`) and `cpu_post` (`absorb`…) — with
`mem_body` between them: **three sequential whole-body sims per revolution**, bus
wired across. This is not a limitation of the physics; it is how we *simulate* two
concurrent loops with a whole-body simulator. This split is why Stage 2 cleaves into
2a (value) and 2b (timing) — see §4.

## 1. The seam (already present in the code — no new factoring risk)

`system_step(rom)` (`step.rs:259`) is `seq_balanced` of 8 phases in in-revolution
order:

```
1 fetch            (Cpu-only, lifted ⊗ id_Mem ⊗ id_Bus)
2 system_decode_execute  (Cpu-only … ⊗ id_Bus)
3 emit             (Cpu,Bus)   — writes BusMsg from CpuState
4 mem_phase        (Mem,Bus)   — exchange keyed on BusMsg.addr; touches ZERO Cpu fields
5 absorb = sym(emit) (Cpu,Bus) — reads BusMsg back into CpuState, clears BusMsg
6 unfetch          (Cpu-only)
7 pc_add_br        (Cpu-only)
8 pc_inc           (Cpu-only)
```

Two facts, both already true in the merged code, make the split a **regrouping, not a
re-derivation**:

- **`mem_phase = view_mem_bus ; (Id(Cpu) × mem_sub) ; sym(view_mem_bus)`**
  (`decoder.rs:1061`), where `mem_sub : (MemState,BusMsg) ↔ (MemState,BusMsg)` is
  `swap ; select_mem_on_bus ; gated_swap ; sym(select) ; sym(swap)`. `mem_sub` is the
  standalone **mem body** — extract it verbatim as `pub fn mem_body() -> Iso`.
- **`emit`/`absorb` operate on `(Cpu,Bus)`** only (via `view_cpu_bus`,
  `decoder.rs:857`), never `Mem`; `absorb = sym(emit)`.

So phases {1,2,3} touch `(Cpu,Bus)` only; phase {4} touches `(Mem,Bus)` only; phases
{5,6,7,8} touch `(Cpu,Bus)` only. The Mem factor rides phases 1-3,5-8 purely as
identity, and the Cpu factor rides phase 4 purely as identity. **Dropping those
identities is exactly the physical separation.**

## 2. The two-body decomposition (target)

Build three isos on **reduced carriers** (Mem absent from the CPU bodies, Cpu absent
from the mem body — this is the area payoff and the structural line-audit witness):

```
cpu_pre(rom)  : (CpuState, BusMsg) ↔ (CpuState, BusMsg)   = fetch ; decode_execute′ ; emit
mem_body()    : (MemState, BusMsg) ↔ (MemState, BusMsg)   = mem_sub  (extracted from mem_phase)
cpu_post(rom) : (CpuState, BusMsg) ↔ (CpuState, BusMsg)   = absorb ; unfetch ; pc_add_br ; pc_inc
```

`cpu_pre`/`cpu_post` are built on the `(Cpu,Bus)` carrier — i.e. the Cpu-only phases
lifted with `id_Bus` (not `id_Mem ⊗ id_Bus`), and `emit`/`absorb` re-expressed on
`(Cpu,Bus)` (they already are, internally, via `view_cpu_bus`). No Mem lines exist in
their netlists at all.

**Value-equivalence obligation (the 2a oracle):** under the reshape that threads the
Bus factor across the three bodies and lifts back to the system carrier,

```
reshape⁻¹ ∘ (cpu_pre ; lift_mem(mem_body) ; cpu_post) ∘ reshape  ==  system_step(rom)
```

Because the three bodies are literally the same 8 phases in the same order with the
resident-identity factors removed, this is a regrouping identity; the differential
oracle discharges it empirically against the already-merged `SystemRing`/`eval`.

## 3. Bus quiescence — a TOTAL structural invariant (not merely sampled)

Stage 1b certifies `bus_quiescent` (`BusMsg == 0` at every revolution boundary) by
sampling. For Stage 2 it is stronger and load-bearing, so state it as a **lemma
provable from the code** (Fable review, promoted from bonus):

- `mem_phase` writes zero Cpu fields (`Id(Cpu) × …`, `decoder.rs:1095`) and uses
  `addr`/`enable` only as Fredkin *controls* (pass-through). So IR, s1 and the
  recomputed predicate are bit-identical at `emit` and at `absorb`.
- `absorb = sym(emit)` therefore re-XORs `enable ^= p` and `addr ^= IR.rt` with
  identical values → both return to 0 (note `addr ^= IR.rt` is **unconditional** —
  fires for every instruction, not just MEXCH — but still cancels).
- `word`: `emit` swaps `bus.word(=0) ↔ reg[rd]`; `absorb` swaps back → `bus.word`
  returns to 0, needing only `bus == 0` on entry, which `encode_system`
  (`types.rs`) establishes and each revolution re-establishes.

Induction closes for **all** CpuState/MemState, not a sample. Hence the CPU loop's
Bus factor is `0` at the start of each revolution, `emit` fills it, `mem_body`
exchanges through it, `absorb` clears it back to `0`; the two loops share state **only**
during the single bus round-trip per revolution, and across the boundary each loop
carries only its own factor. This is what makes the two-body ring a faithful refactor
rather than a new machine. (F3 turns this lemma into a runtime assertion — see §4.)

## 4. Build split

### Stage 2a — two-body value decomposition (delegate FIRST; moderate risk)

New code (all in `theseus-tisc`):
- `decoder::mem_body()` — extract `mem_sub` (pure move; `mem_phase` re-expressed as
  `view_mem_bus ; (Id(Cpu) × mem_body()) ; sym(view_mem_bus)` so nothing else moves).
- `step::cpu_pre(rom)` / `step::cpu_post(rom)` on the `(Cpu,Bus)` carrier. `emit`/
  `absorb` already have a reduced-carrier body — `decoder::emit_on_cpu_bus()`
  (`decoder.rs:986`) on `(state_ty, bus_state_ty)` — so `cpu_pre` = `fetch ⊗ id_Bus ;
  decode_execute′ ⊗ id_Bus ; emit_on_cpu_bus`, `cpu_post` = `sym(emit_on_cpu_bus) ;
  (unfetch; pc_add_br; pc_inc) ⊗ id_Bus`. No re-derivation.
- `ring_run::TwoBodyRing` — lowers+balances `cpu_pre`, `mem_body`, `cpu_post` as
  **three separate `CompactBalanced` bodies**, each with its **own ancilla vector**
  (three vectors, each fed back independently). `run(start, R)` per revolution: sim
  `cpu_pre` on `(Cpu,Bus=0)`; **relocate** the 24 Bus bits and sim `mem_body` on
  `(Mem,Bus)`; **relocate** them back and sim `cpu_post` on `(Cpu,Bus)`; feed
  `(CpuState', MemState')` into the next revolution.

**Exact data layouts (F4 — pin these as named constants, do not leave to prose).**
`STATE_BITS = 160`, `bus_state_ty = 24` bits, `mem_state_ty = 32` bits. simulate()'s
port order is data-then-ancilla, data in carrier-flattening order, so:
- CPU bodies `(Cpu,Bus)`: `n_data = 184`; Cpu at `[0..160)`, **Bus at `[160..184)`**.
- mem body `(Mem,Bus)`: `n_data = 56`; Mem at `[0..32)`, **Bus at `[32..56)`**.
`run()` must relocate the 24 Bus bits between offset 160 (CPU) and offset 32 (mem)
each crossing — assert `n_data` (184/56/184) and `input_ty` loudly on each body (use
the `SystemRing::bus_quiescent` assert at `ring_run.rs:480` as the template). A
field-transposing splice (addr↔word) survives states where those bytes coincide, so
the asserts + the F5 sample are the guard, not the happy-path run.

**F3 — do NOT silently re-zero the bus.** `SystemRing::run` feeds the bus bits back
through the wrap (`ring_run.rs:404`). `TwoBodyRing::run` must not discard them: after
`cpu_post` each revolution, **`assert!` the 24 Bus bits are all 0** (the §3 lemma made
runtime) — otherwise a quiescence regression is silently swallowed and diverges from
`SystemRing` only on unsampled states.

Certificate test (`tests/ring_two_body.rs`, all **bounded** — see §6):
- `two_body_run == SystemRing::run == eval(system_step)^R` at `N = MEM_WORDS = 4`,
  `R ∈ {1,2}`, a strided ≤256-state sample. **F5 — the sample MUST include:** MEXCH
  with `rd == rt`; back-to-back MEXCH at `R = 2`; nonzero `reg[rd]` vs nonzero
  `Mem[addr]`; and a non-MEXCH op with nonzero `IR.rt` bits (exercises the
  unconditional `addr ^= IR.rt` path, `decoder.rs:1007`). Plus enable on/off, addr =
  each word.
- per-body ancilla restoration: each of the three bodies is ancilla-clean on its own
  (`run == run_open` per body). Add `assert!(body.ancilla_count() > 0)` for the two
  **CPU** bodies to kill a vacuous test (mem_body's Fredkin-only lowering may allocate
  few/none — do not assert on it).
- **structural line-audit (now a physical fact, not a scan):** the `cpu_pre`/
  `cpu_post` netlists contain **zero** MemState ports — assert `input_ty ==
  prod(state_ty(), bus_state_ty())` and `n_data == 184` on each (`PhasedCellNetlist`
  exposes public `input_ty`/`output_ty`, reachable via `CompactBalanced::net()`,
  `balance.rs:118`) — and `mem_body` contains zero CpuState ports. MemState *cannot*
  reach a CPU-phase cell because it is not in the CPU body's netlist. Strictly stronger
  than ADR-0046 §6's reachability scan; discharges it by construction.

### Stage 2b — physical timing certificate (delegate SECOND; adversarial review)

Generalize ADR-0033 D2's single-body boundary checks to the two-body topology. There
are **three** transit edges per revolution, not two (F1 — the CPU hold path was
missing in the draft and is the whole ballgame):
- `δ₁`: `cpu_pre.emit_out → mem_body.in`;
- `δ_m`: `mem_body`'s internal exchange segment depth;
- `δ₂`: `mem_body.out → cpu_post.absorb_in`;
- `δ_hold`: the direct `CpuState` path `cpu_pre.out → cpu_post.in` (the 160 held CPU
  lines that do NOT go to memory).

**F1 — absolute-tick arrival equations (mod-P is NOT enough).** A bus return one full
period `D` late satisfies phase-boundary + period-lock + phase-closure yet makes
`cpu_post.absorb` consume the *previous* revolution's memory response — and 2a's
sequential oracle cannot see timing, so it would pass a broken machine. The certificate
must therefore pin **tick-exact co-arrival in absolute ticks** at both merge junctions:
1. each body validates as today (D2.1);
2. **CPU-hold equality:** `δ_hold = δ₁ + δ_m + δ₂` **exactly** — the held CpuState
   lines are K0-padded by the full bus round-trip so cpu_post's mixed inputs co-arrive
   tick-exact (in 2LAL a skew here is *wrong*, not merely slow — `balance.rs:4`);
3. **mem-merge equality:** the recirculating `MemState` and the incoming bus co-arrive
   tick-exact at `mem_body`'s input;
4. **same-revolution servicing (the anti-skew clause):** the equations admit **no**
   mod-`D`-late solution — memory serves revolution `r`'s request inside revolution
   `r`, never `r+1`. With (2)+(3) as absolute-tick equalities this is forced; assert it
   explicitly so a future relaxation can't reintroduce the skew;
5. **period equality** `D_cpu = D_mem` (commit to equality, **not** `D_mem | D_cpu` —
   `k > 1` would transit the exchange segment `k−1` times on an undriven bus, safe only
   because `mem_body(m,0) = (m,0)`; not worth the footgun). Pad the shallow mem body up
   with K0 delay counts;
6. **phase closure** `D ≡ 0 (mod P)` on both loops (`P = 4`, D2.3);
7. **wrap ownership (F6):** the CPU loop's 24 Bus lines are a declared constant-zero
   external boundary re-supplied `= 0` each revolution (the §3 lemma guarantees they
   exit 0), NOT wrapped as state; each loop wraps only its own factor. State this so the
   zero-garbage framing stays honest (a 24-line data-independent boundary, like
   `ClosedStepRing::boundary_size`).

**F2 — the padder is NEW machinery; derive arrivals correctly.** `CompactBalanced`
today hard-codes `output_delay == 0` (`balance.rs:402`) and `balance_compact` clones
the net **without rewriting cell depths/phases** — so `compact.net()`'s `cell.depth`/
`phase` are **stale pre-balance numbers**. The 2b validator MUST recompute balanced
arrivals as `line_arrivals + delay counts`; it must NOT read timing off `compact.net()`
cell fields (that certifies wrong constants while passing green). A compact **output
padder** (rounding outputs up to `D` via delay counts, not materialized K0 — the
materialized `pad_ring`, `ring.rs:77`, is ruled out at ~1.6M K0 step scale) is a real
new deliverable, not a config tweak — scope it as such.

Certificate test (`tests/ring_two_body_timing.rs`): all three absolute-tick equalities
hold, `D_cpu == D_mem`, phase closure holds on both loops, same-revolution clause
asserted, line-audit (from 2a) still passes.

**Honest scope of 2b:** because `simulate` is whole-body-only, 2b is a **static
arrival-arithmetic certificate** — it proves the two loops can be *physically clocked
concurrently, in lockstep, same-revolution* with a fixed bus transit and equal period.
The **dynamic** claim ("what the two-body ring computes == `eval^R`") is carried by
Stage 2a's sequential `run` — and is the *correct* oracle **only because** F1's
equations force lockstep (no pipeline skew). 2a = *what* it computes; 2b = *that the
concurrent clocking realizes exactly that, with no revolution skew*. Together =
ADR-0046 §8 Stage-2 acceptance.

## 4b. Stage 2b-lite — the finalized static realizability certificate (SCOPED)

Stage 2a is merged (`theseus-v2` 2810bab + cert-hardening 24ae386). Per the user's
scope decision, **Stage 2b is delivered as 2b-lite**: a *static realizability
certificate* in `theseus-tisc` ONLY. It computes the two loops' real depths (from the
already-public `PhasedCellNetlist::line_arrivals`, which equals the balanced arrival —
so it sidesteps the F2 stale-`compact.net()` trap WITHOUT touching the central
balancer) and proves the F1 lockstep arrival equations have a consistent solution.
This meets ADR-0046 §8's Stage-2 acceptance ("arrival equations and period lock
*validated*"). The compact output **padder** (F2) and a *materialized* padded two-body
netlist are deferred to the memory energy-ledger work (§9, M-E territory) — NOT built
here.

**Grounding (measured on `theseus-v2` 24ae386, 1-entry MEXCH ROM):** `D_pre =
depth(cpu_pre) = 2839`, `D_mem = depth(mem_body) = 35` (Fredkin-only, 0 ancillas),
`D_post = depth(cpu_post) = 152`. `P = PHASE_COUNT = 4`. A consistent lockstep solution
exists: δ₁=1, δ₂=1, δ_hold=37, `D_cpu = D_mem_loop = 3028 ≡ 0 (mod 4)`. The certificate
must recompute these from the live depths (they are NOT hard-coded — a body-depth
regression must flow through), and assert the equations below hold.

**Physical model (two concurrent lockstep loops, same-revolution servicing).** The CPU
loop circulates CpuState through `cpu_pre` (depth `D_pre`) then, after a hold of
`δ_hold` ticks, `cpu_post` (depth `D_post`); the bus exits `cpu_pre` (flushed to
`D_pre`), transits δ₁ to `mem_body` (depth `D_mem`), exits, transits δ₂ back to
`cpu_post`'s bus input. The mem loop circulates MemState through `mem_body`, idle the
rest of the period. Bus is a constant-0 boundary (2a/F6), quiescent outside the round
trip.

**The certificate algorithm (all compile-time-constant integer arithmetic):**
1. Lower `cpu_pre(rom)`, `mem_body()`, `cpu_post(rom)`; `D_pre/D_mem/D_post` = each
   body's max **data-output** arrival via `line_arrivals` (NOT `net.depth()` on a
   stale clone; NOT cell.depth fields).
2. `δ₁` = least ≥0 with `(D_pre + δ₁) ≡ 0 (mod P)` — phase-align the mem-body window.
3. `δ₂` = least ≥0 with `(D_pre + δ₁ + D_mem + δ₂) ≡ 0 (mod P)` — phase-align cpu_post.
4. `δ_hold = δ₁ + D_mem + δ₂` (the held CpuState pad, F1 co-arrival).
5. `D_cpu = D_pre + δ_hold + D_post`; `pad = (P − D_cpu % P) % P`; `D_locked = D_cpu + pad`.
6. `D_mem_loop = D_locked` (period lock — mem body padded to fill; assert equality).

**CRITICAL — the certificate must have TEETH (Fable review of this spec).** A naive
version that *solves* for δ₁/δ₂/δ_hold and then asserts the equations those very solves
satisfy is **self-fulfilling theater** — every timing assertion passes by construction,
so it detects no regression and "validated" is vacuous. The fixes below (F-teeth-1/2/3)
are MANDATORY; without them 2b-lite does not meet §8.

- **F-teeth-1 — golden pinning in the TEST (the real regression detector).** The
  *library* computes `d_pre/d_mem/d_post` live from `line_arrivals` (no hard-coding
  there). The *test* pins the full solved tuple against the measured snapshot for the
  MEXCH ROM: `d_pre==2839, d_mem==35, d_post==152, delta1==1, delta2==1,
  delta_hold==37, d_locked==3028`. A lowering/depth regression then FAILS LOUDLY and
  forces a human to re-review the timing before bumping the golden. (This is why the
  numbers live in the test, not the library.)
- **F-teeth-2 — `validate()` is an INDEPENDENT recomputation, not a restatement.**
  `validate()` re-derives `δ₁, δ₂, pad` from `(d_pre, d_mem, d_post, P)` by a *different*
  implementation than the solver — a brute scan `(0..P).find(|k| (base+k)%P==0)` — and
  compares against the stored fields. Now A/C/E are equations between two computations,
  not `x == x`; this catches solver bugs and struct-field tampering.
- **F-teeth-3 — one genuinely netlist-derived falsifiable check + minimality +
  mem-merge.** From each body's `line_arrivals`, record the per-bus-line arrival spread
  and expose the implied per-line flush pads (`D_body − arrival_i`) in `TwoBodyTiming`;
  assert every pad `≥ 0` (the contract the deferred F2 padder must satisfy — turns the
  "outputs flushed to D" premise from prose into checked data). Assert minimality
  `δ₁ < P && δ₂ < P && pad < P` (fails if a non-minimal solve is swapped in). Restore
  the **mem-merge clause** (F1 clause 3): the mem-loop places the wrapped MemState to
  co-arrive with the incoming bus at `mem_body`'s input — reduces to `D_mem_loop ==
  D_locked` placing the window at `[D_pre+δ₁ .. D_pre+δ₁+D_mem]`; assert that window
  start `≡ 0 (mod P)` and `< D_locked`.

**The certificate assertions (which have teeth, post-fix):**
- **A. co-arrival (F1):** `δ_hold == δ₁ + D_mem + δ₂`. Given F-teeth-2, the RHS is the
  independent re-derivation, so this is a real equation. (State next to A the premise it
  relies on: both the held CpuState and the bus leave `cpu_pre` flushed at `D_pre` —
  same origin — which F-teeth-3's per-line pads make checkable.)
- **B. no revolution skew (F1 same-revolution clause):** `0 < δ₁+D_mem+δ₂ < D_locked`.
  (Guards a future relaxation of the minimal solve; documents intent.)
- **C. phase closure (D2.3):** `D_locked ≡ 0 (mod P)`, `pad ≤ P−1`, `(D_pre+δ₁) ≡ 0`,
  `(D_pre+δ₁+D_mem+δ₂) ≡ 0 (mod P)` — checked via F-teeth-2's independent δ's.
- **D. period lock (F6):** `D_mem_loop == D_locked`.
- **E. non-negativity + minimality:** `δ₁,δ₂,δ_hold,pad ≥ 0` and `δ₁,δ₂,pad < P`.
- **F. line-audit (from 2a):** reuse `input_ty`/`n_data` asserts — the bus is the ONLY
  cross-loop signal; MemState absent from the CPU bodies.
- **G. per-line flush pads ≥ 0** (F-teeth-3) — the one assertion derived from the raw
  `line_arrivals` the solver does not manufacture.

**Deliverable shape:**
- `ring_run::two_body_timing_certificate(rom) -> TwoBodyTiming` — struct exposing
  `d_pre, d_mem, d_post, delta1, delta2, delta_hold, d_locked, period` (`u32`) PLUS the
  per-body per-bus-line flush pads (`Vec<u32>` each). Performs steps 1–6 over live
  `line_arrivals`. NO lowering mutation, NO machine-semantics change, NO padder
  materialization.
- `TwoBodyTiming::validate() -> Result<(), String>` — the INDEPENDENT re-derivation
  (F-teeth-2) enforcing A–G; returns a descriptive error, never silently self-passes.
- `tests/ring_two_body_timing.rs` — (i) golden-tuple pin for the MEXCH ROM (F-teeth-1);
  (ii) `validate()` Ok for a handful of ROMs (MEXCH, non-MEXCH, branch); (iii) the §F
  line-audit. Bounded (a few ROMs, one lowering each — fast; no domain enumeration).

**Honesty note in the certificate docs:** δ₁/δ₂ are the *minimal phase-aligning
transits* (a specific realizable placement); 2b-lite proves a consistent lockstep
clocking EXISTS and pins it to measured depths — the physical padded netlist (2b-full)
is deferred to the energy-ledger work. Do not overclaim a built machine.

## 5. Risks the adversarial review MUST probe

- **R1 — decomposition faithfulness.** Is dropping `id_Mem`/`id_Cpu` and threading Bus
  across three bodies *exactly* `system_step` under reshape? Confirm the Bus factor
  order (emit writes → mem reads/writes → absorb reads/clears) and that Bus=0 at the
  revolution boundary matches `system_step`'s BusMsg init. The 2a oracle is the net;
  the reviewer confirms the sample hits the states a threading/endianness bug hides in.
- **R2 — per-body ancilla cleanliness.** The one-body `system_step` is ancilla-clean;
  does each of the three *sub*-bodies restore its own ancillas independently? (emit/
  absorb = R1 CNOTs, mem_body = gated Fredkins K2 + zero-cell reshapes, fetch/pc use
  pc-pred/carry ancillas restored within-phase.) If a phase leaked an ancilla across
  the old phase boundary, the split would expose it. The per-body `run == run_open`
  test is the witness — confirm it is not vacuous (bodies actually allocate ancillas).
- **R3 — period-lock realizability.** Can `D_mem` be padded up to `D_cpu` in the
  compact delay-count form? (`output_delay` is "currently always 0" — verify the
  padder can round outputs up; the mem body is far shallower, so padding is cheap K0.)
  Are the arrival equations consistent (no negative transit, phase boundaries land)?
- **R4 — cross-edge phase alignment.** `emit_out` arrives at CPU-body depth `D_pre`;
  it must enter `mem_body` and the returned bus must re-align at `cpu_post`'s input.
  The 2a *value* is correct regardless (sequential sims); 2b must show the *phases*
  (`depth mod 4`) line up so concurrent clocking is electrically legal (D2.3 precedent).
- **R5 — did we actually separate, or just add indirection?** The structural line-audit
  (§4, 2a) is the anti-degeneracy witness (ADR-0046 §6, Prop 5.1/5.2): MemState absent
  from the CPU netlist is the definition of "not Option-A with extra wires." Confirm the
  assertion is on the netlist port types, not a comment.

## 6. Operating discipline (non-negotiable, from CLAUDE.md + memory)

- Stage 2 touches ring lowering → **fresh-context adversarial review + a real
  integration test before merge** (CLAUDE.md). This note goes to review before any
  delegation.
- **Bound every test domain** (memory `maestro-gate-no-timeout`: workers repeatedly
  ship unbounded enumerations → 3 gate-hangs). `R ∈ {1,2}`, `N = 4`, ≤256 strided
  states. Add `#[ignore]`d whole-domain twins runnable via `--ignored`, each paired
  with a fast bounded companion (oracle norm, L19 / ADR-0008).
- **Timeout-wrap the gate** (`timeout <N> cargo test`) and **scope it to the Stage-2
  test files**; the full `theseus-tisc` suite is ~13 min — run it manually before merge.
- `MEM_WORDS` is a compile-time const (`= 4`); the ring runs at the compiled width.
  Closure is N-independent; the tree's N-robustness was already discharged in 1a
  (`N ∈ {2,4,8}`). Do not ask the worker to vary `N` at the ring level.
- Driven workers **cannot** run cargo/nix (L5) — the spec must not tell them to
  self-verify; the maestro gate + the advisor verify.
