# ADR-0044: M-C -- compiler-targeted memory (scratch spill → Option-B bus)

**Status:** Accepted -- **M-C1 implemented** (`theseus-tisc::compile`, merged
`4af7862`); **M-C2 (Option-B bus) proposed** (§3, own design pass). Advisor-
authored, then revised after an adversarial design review (§2's callout box).
Refines ADR-0029's Option-A/B stratification for the **compiler**.

**Implementation note (M-C1, what shipped).** Reversible register/scratch spill
exactly per §2: a `Location`(`Reg`/`Scratch`) home per leaf, the `with_registers`
page-in/out helper routing every emission site, `allocate_aux`'s zeroed-scratch
aux source, the guarded-unroll canonical-residence `debug_assert`, and the
off-by-two marshalling -- pure `compile.rs`, no `State`/`step`/ISA change. Verified
by the run-on-CPU oracle (§4): `run_on_cpu` on the **real `step`-iso CPU** `==
eval` on the sampled inputs for pure spill (incl. the 14-leaf ceiling) and
spill+branch (controlled op + `iter`), fast-sim green over the whole domain, and
15-leaf / 13-leaf-branching programs rejected cleanly with `TooManyLeaves`. The
honest ceilings hold: **14 straight-line, 12 branching.**

## 1. Where M-C sits

M-A/M-B ([ADR-0042](0042-thse-to-tisc-compiler.md),
[ADR-0043](0043-mb-reversible-branching-codegen.md)) compile the **register-only,
bounded** fragment: every value's leaves must fit the 8-register file or
`compile.rs` returns `CompileError::TooManyLeaves`. Two facts about the current
substrate frame M-C:

- The **8 scratch words already exist in `State`** (`types.rs`; Option A of
  [ADR-0029](0029-memory.md)) but the compiler **never uses them** -- it maps
  leaf `i` → register `i` and errors above 8.
- The "exch-iso seam" ADR-0029's *free hedge* promised (v1 routing scratch access
  through named exch-style isos) was **never built**; Option B is greenfield. But
  its stated precondition -- "the ring machine exists to inform the bus protocol" --
  is **now satisfied** (M4-v2, [ADR-0033](0033-ring-machine.md)).

M-C lifts the storage ceiling, in **two deliberately separated increments**:

1. **M-C1 -- scratch spill (Option A, pure compiler).** Use the spillable in-`State`
   scratch words (`s2..s7`; `s0`/`s1` are decoder-owned, §2) as spill space via the
   existing `Xchg` (reg↔scratch) instruction. Raises the leaf ceiling **8 → 14**
   (straight-line) / **8 → 12** (branching) with **zero changes** to `State`,
   `step`, or the ring -- only `compile.rs`.
2. **M-C2 -- memory bus (Option B, architectural).**
   `step : (CpuState, BusMsg) ↔ (CpuState, BusMsg')`; memory a separately-compiled
   Fredkin-tree exchange iso ([ADR-0027](0027-cell-library.md)/0029) composed
   through the ring. Breaks the fixed ceiling toward scalable memory. Its own
   design pass.

**M-C1 first** because it is small, high-confidence, unlocks real headroom, and
**de-risks the spill/marshalling logic M-C2 also needs** -- and because Option B is
better designed *after* M-C1 has exercised spill end-to-end and against the
concrete ring interface, not blind (the exact reason ADR-0029 deferred B).

## 2. M-C1: reversible register/scratch spill

> **This section was rewritten after an adversarial design review** that broke the
> first draft's "8→16, residence persists freely" scheme. Three findings drove the
> rewrite and are load-bearing: **(i)** `s0`/`s1` are decoder-owned, not spill space
> (the assembler hard-asserts `Xchg`'s `sx ≥ 2`; `s0` is the operand/immediate
> buffer *and* `Xchg`'s own routing slot, `decoder.rs`; `s1` is the fetch/execute
> predicate + Toffoli-chain scratch, touched *every* step, `step.rs`), so only
> `s2..s7` (6 words) are spillable → the honest ceiling is **14 straight-line**, not
> 16. **(ii)** M-B skip-blocks need **two register-resident, zero-on-entry** aux
> registers held across a whole block, which needs a defined **zero-source** and
> drops the ceiling to **12 for any branching program**. **(iii)** the guarded
> unroll compiles the body **once** and re-emits a frozen instruction vector `D`
> times, so a "residence persists across ops" scheme corrupts copies 2..D (the L15
> "helpers work, the sequence doesn't" trap). The fix to all three is a **canonical
> home layout** with page-in/page-out **around each op**.

### 2.1 Canonical home layout (the invariant)
Each leaf has a **fixed home**, a pure function of its leaf index `L`:
`L < 8` → register `L`; `8 ≤ L < 14` → scratch word `s(L-6)` (i.e. leaves 8..13 →
`s2..s7`; `s0`/`s1` are reserved and never a home). **Between every instruction the
machine is in canonical layout** -- every leaf sits in its home. Residence departs
from canonical only *transiently* inside a single op (§2.3) or a single skip-block
(§2.4), and is restored before the next. This invariant is what makes the frozen
unroll body correct (§2.6) and lets every lowering site compute a leaf's location
from its index alone.

### 2.2 Paging is a swap (the reversibility point)
`Xchg r, sx` **swaps** register `r` with scratch `sx` (`sx ≥ 2`) -- already an iso
with verified step semantics, and its own inverse. It is the only new instruction
M-C1 emits; no new State, no new opcode. All paging is a sequence of `Xchg`s, so it
is intrinsically reversible and the ROM run backward un-pages exactly.

### 2.3 Op lowering: page-in → emit → page-out (`with_registers`)
Every instruction operates on **registers**. Introduce one central helper --
`with_registers(ctx, &[leaves], |regs| ...)` -- that, for an op on leaves `{a,b,c}`
(≤3, all with distinct homes):
1. for each operand whose home is scratch, `Xchg` it into a **spare register** (a
   register whose home-leaf is not an operand; ≥5 always exist since ops touch ≤3),
   which parks the spare's home-leaf in the operand's scratch home;
2. run the closure, emitting the op on the now-register operand indices;
3. **undo the same `Xchg`s in reverse**, restoring canonical layout.

**Every existing site that reads a raw register index from the leaf map must route
through `with_registers`** -- enumerated so the implementer misses none:
`recognize_prim`/the `Exch` emission, the **`SwapProd` `Exch` loop** (sharpest:
`Exch` on a scratch index is meaningless), `lower_controlled_idiom`,
`lower_bare_plus`, and the `Times`/`Seq` recursion. This centralizes the discipline
in one helper instead of five ad-hoc call sites.

### 2.4 Aux registers for branching + the zero-source
M-B's skip-block codegen (`allocate_aux`, ADR-0043 §2) needs **two** aux registers
(`rb`, `temp`) that are **register-resident and 0 on entry**, held across the whole
block. Obtain them by paging a register-home leaf **into a free, already-zero
scratch word**: `Xchg r, s_free` where `s_free` is a spill slot holding no leaf
(canonical layout keeps unused scratch at 0) moves the leaf out **and leaves `r`
holding 0** -- a zeroed aux register, for free. Two aux registers need **two free
zero scratch slots**, i.e. `6 − (spilled-leaf count) ≥ 2`. At the block's end the
aux registers are 0 again (skip-block invariant) and the two leaves page back. This
is why the **branching ceiling is 12** (8 reg + 6 scratch, minus the 2 scratch
slots consumed to zero the aux registers): a program that both spills and branches
admits at most 12 leaves.

### 2.5 Ceilings and `TooManyLeaves` (honest numbers)
- **Straight-line** (no `allocate_aux`): **14** leaves (8 reg + `s2..s7`).
- **Branching** (any skip-block / data-dependent iter / bare `Plus` / general
  controlled op): **12** leaves.

`num_leaves`' hard cap becomes **14**; `allocate_aux` errors `TooManyLeaves` when it
cannot secure two free-zero scratch slots (effectively capping branching programs
at 12). Both bounds surface as a clean **compile error, never a silent miscompile**
-- leaf count is a static property of the type, so `compile` either succeeds for all
values or fails uniformly. (The first draft's "16 / error only above 16" was wrong
on both counts.)

### 2.6 Guarded-unroll consistency (the L15 fix)
Because §2.1 holds canonical layout at every op **and** every skip-block boundary,
the body of `lower_data_dependent_iter` is in canonical layout at both its start and
end. The single compiled body instruction-vector is therefore valid for **every** of
the `D` re-emitted copies -- copy `i+1` begins in exactly the layout copy `i` left.
The implementer MUST assert this (a debug check that `Ctx` residence == canonical at
each body boundary) so a future change that lets residence leak across a body
boundary fails loudly rather than silently corrupting copies 2..D.

### 2.7 Marshalling
`marshal` currently writes leaf `i` → `regs[i]` and **panics at `regs[8]`** for any
spilled program -- it must place leaf `i<8` → `regs[i]` and leaf `8 ≤ i < 14` →
`scratch[i-6]` (the `s2..s7` homes; **note the −6, skipping `s0`/`s1`**). `unmarshal`
has the mirror bug (reads only `regs[0..n]`) and must read spilled output leaves from
`scratch[i-6]`. `Ctx::new` initializes each leaf to its canonical home (not
`mapping[i]=i`). The off-by-two (skip `s0`,`s1`) must be identical across `marshal`,
`unmarshal`, `Ctx::new`, and `with_registers`, or entry/exit desynchronizes.

### 2.8 Adjoint
`Iso::Sym` re-runs the allocator on the adjoint iso (it does not reverse a recorded
schedule); since `Xchg` is its own inverse and allocation is deterministic from leaf
indices, the paging reverses correctly.

## 3. M-C2: the Option-B memory bus (sketch -- own design pass)

When a value exceeds the register+scratch file (or when energy accounting wants
fabric-resident storage, ADR-0029 C3.5), Option A dies and Option B wakes:

- `step` becomes `(CpuState, BusMsg) ↔ (CpuState, BusMsg')` -- a swap-bus port
  (ADR-0029 §Decision).
- Memory is a **separately-compiled theseus iso**: addressed exchange = a Branch
  tree on address bits steering a controlled swap = a Fredkin tree = **literally
  the ADR-0027 cell**.
- Composed through the **ring** ([ADR-0033](0033-ring-machine.md)), whose concrete
  interface (`ring_run.rs`) now exists to inform the protocol.
- New ISA surface: `LOAD`/`STORE` = addressed `Xchg`-to-memory; the compiler spills
  leaves beyond the register+scratch file to memory addresses.

M-C2 earns its own ADR after M-C1 lands and against the real ring interface.

## 4. Acceptance: the run-on-CPU differential oracle (per L19)

Authority = `run_on_cpu` on the **real `step`-iso CPU** `== theseus_eval::eval`
over the whole finite domain ([ADR-0008](0008-differential-testing.md); L19 in
maestro `OPERATING_LESSONS.md` -- the real CPU is the authority, the byte-level
`run_on_cpu_fast` is a pinned convenience only). Required M-C1 programs:

- **(a) pure spill** -- a straight-line program on a value needing **9-14 leaves**
  (e.g. a product of ~11 `Bit`s, or a `Times` of two wide `Nat`s) compiled + run
  `== eval`. Include a **14-leaf** case (the ceiling) and confirm a **15-leaf**
  program cleanly `TooManyLeaves` (not miscompiles).
- **(b) spill + branch** -- a program that both spills and branches at up to **12
  leaves** (e.g. a general controlled op, or `iter`, over a ~10-leaf value),
  `== eval` over the whole domain; confirm a **13-leaf** branching program cleanly
  errors. This pins §2.4 (aux zero-source) and §2.6 (unroll consistency).
- **(c)** all M-A/M-B tests still green (no regression), including the authoritative
  `#[ignore]`d real-CPU oracle.

The risky parts -- the `with_registers` page-in/out bookkeeping (§2.3), the
spill↔branch aux zero-source (§2.4), and the frozen-body unroll consistency (§2.6) --
are exactly what the whole-domain oracle pins.

## 5. Open risks

- **`Xchg` traffic / ROM length.** Page-in/out **around each op** (§2.3) is
  deliberately un-optimized (correctness first); it can multiply ROM length several-
  fold for spill-heavy programs. `run_on_cpu`'s fixed budget (worst-case trace
  length) counts the paging instructions, so termination is unaffected; allocation
  minimization (keep a leaf register-resident across consecutive ops that use it) is
  an M-E concern, explicitly deferred.
- **The off-by-two is everywhere.** `s0`/`s1` reservation must be threaded
  *identically* through `Ctx::new`, `marshal`, `unmarshal`, `with_registers`, and
  `allocate_aux`'s zero-source pick. A single site that treats scratch as `s0..s7`
  desynchronizes entry/exit or lets execution clobber a spilled leaf (`s0` idle-step
  `r0 += s0`). The oracle catches it, but it is the most likely implementation bug.
- **Ceiling is bounded (14 / 12).** M-C1 is a stepping stone, not "arbitrary"; the
  Option-B bus (M-C2) and recursion/heap (M-D) remain for genuinely unbounded data.

## Cross-references
- [ADR-0029](0029-memory.md) -- memory as exchange; Option A/B stratification (M-C
  realizes the A→B path).
- [ADR-0027](0027-cell-library.md) -- the one cell (Fredkin tree = addressed
  exchange, the M-C2 memory fabric).
- [ADR-0033](0033-ring-machine.md) -- the ring machine, now existing to inform the
  Option-B bus protocol.
- [ADR-0042](0042-thse-to-tisc-compiler.md) / [ADR-0043](0043-mb-reversible-branching-codegen.md)
  -- the compiler + M-A/M-B fragment M-C extends (`Ctx`, `allocate_aux`,
  `num_leaves`, `marshal`).
- [ADR-0008](0008-differential-testing.md) -- the differential-oracle norm (real-CPU
  authority, L19).
