# ADR-0045: M-C2 -- the Option-B memory bus (external recirculating memory)

**Status:** Proposed -- scoped, adversarially reviewed (§0), and **resolved by a
2-agent research spike (§0.5).** Outcome: the external bus **is achievable** -- but
only as a **composite-ring** closure (CPU ⊗ bus ⊗ memory certified as one unit),
never as per-ring closure (which is *provably* impossible, an information-flow
theorem). It is **energy-equivalent to Option A** ("storage is motion"): it buys
area, a CPU body invariant in N, and the seam behind which Option C (adiabatic
SRAM) can later drop in -- **not** an energy win. Therefore: the **deliverable M-C2
now is a resident-memory region** (§0); the composite bus is a **documented,
de-risked future design** (§0.5) to build when the area wall binds or Option C's
wake condition (ADR-0029 C3.5) fires. The original §2-§7 sketch is retained as
record; do not delegate it as written.

## 0. Scoping finding (adversarial review) -- READ FIRST

Scoping this milestone surfaced that "full external Option-B bus" as sketched below
is **hiding a re-architecture**, on four independent grounds:

1. **The ring closure certificate forbids a bus port.** The zero-garbage ring
   closes because its wrap is an *all-or-nothing self-feeding identity* on a single
   State-width port (`run == run_open`, `ancillas_restored`; `ring_run.rs`,
   [ADR-0040](0040-rail-role-swap-cell.md) phase C). A data-dependent `BusMsg`
   exchanged between two rings *every revolution* is not "a second data-port pair
   the wrap threads" (§7's hope) -- it **defeats the exact certificate** that makes
   the ring close. This is a ring re-architecture, not a threading.
2. **§3.1 is not writable.** `introduce BusMsg (uniti)` is not a real iso -- no
   combinator manufactures a known-zero multi-bit word (non-injective; linearity
   bans it). `BusMsg`/`MemState` must be **resident, marshalling-zeroed factors of
   the compiled carrier** (exactly M-C1's zero-source, [ADR-0044](0044-mc-compiler-memory.md)
   §2.4), not introduced/eliminated.
3. **M-C2a proves the wrong thing.** At the value level, splicing `mem` into `step`
   over a shared `(CpuState, MemState)` type **is the degeneracy trap** (§1): `eval`
   cannot distinguish true Option-B from Option-A-with-indirection. So the M-C2a
   oracle would go green on a *degenerate* system while the real risk (closure)
   stays structurally invisible to it. The phasing does not de-risk; it hides.
4. **Scalability is not established.** A 1-of-N addressed-exchange tree + N-word
   resident bank is O(N) active cells + O(N) balancing buffers (the same
   ~1.6M-K0 wall, [ADR-0033](0033-ring-machine.md) D4) -- so "O(N) storage over an
   O(log N) bus" is only addressing depth; the energy profile is Option A's. The
   8-bit immediate also caps memory at 256 words.

**Deliverable reframing (the real M-C2).** A **resident-memory region**: `MemState`
as a marshalling-zeroed factor of the compiled carrier, accessed by swap
(`word ↔ mem[addr]`, exchange/move discipline) via a compile-time-addressed
`LOAD`/`STORE`, extending M-C1's `Location`/`with_registers` with a `Mem(addr)`
home and a **specified zero-parking discipline** (the review re-opened M-C1's
aux-zero-source hole in §4 -- LOADing a leaf into an occupied register displaces the
other leaf and needs a free-zero cell). Memory stays a **compilation artifact**,
invisible to `theseus_eval` (the oracle compares the CpuState projection against
`eval` of the *original* surface iso -- there is no memory-typed surface program).
This is oracle-gated and delegatable once the zero-parking is spec'd as concretely
as ADR-0044 §2.3-2.4.

**The true external two-ring bus is deferred to a research spike** -- a design
investigation against `ring_run.rs` to determine whether a cross-ring `BusMsg`
exchange can preserve the closure certificate at all -- *before* any implementation.
It is not a delegatable milestone in its current form.

## 0.5 Research-spike resolution (2 independent design-review agents)

Two independent design-review agents attacked the closure question from
opposite directions -- one **constructive** (design a closing two-ring bus), one
**foundational** (prove whether closure is preservable at all). **They converged.**

**Result 1 -- per-ring closure is *provably impossible* (a theorem, not a wall).**
If the CPU ring closes in isolation, its revolution map is a function of its own
ports alone, so a `LOAD` reply `mem[addr]` would have to be a function of
`CpuState` -- but external memory holds exactly the information *not* recoverable
from `CpuState` (the CPU swapped it out and moved on). Contradiction. So **genuine
external memory ⟺ the CPU ring is an open system**; no wrap or re-supply schedule
fixes a solo ring. This *confirms* §0 finding 1 and upgrades it: stop looking for a
per-ring fix. (The obstruction is *autonomy/information-flow*, not
reversibility/thermodynamics -- nothing is erased.)

**Result 2 -- the *composite* (CPU ⊗ bus ⊗ memory) closes, with zero
per-revolution boundary.** Both agents independently found the same mechanism:
make `BusMsg` a **resident, marshalling-zeroed factor of the carrier** that returns
to rest at *every revolution boundary* (the emit/absorb conjugation discipline --
the fetch/unfetch proof shape applied to the bus). A resident factor that is
boundary-constant but mid-revolution data-dependent **is exactly what the existing
certificate already calls an ancilla**. So the zero-garbage invariant (ZG) --
"`States × {ι}` is an invariant slice of the lowered bijection" -- generalizes
*verbatim* to the composite, with `data = (CpuState, MemState)` and `ι` extended by
the (zeroed) bus lines + memory-tree scratch. This simultaneously repairs §0
findings 1 (closure is on the composite), 2 (the resident-zeroed factor *is* the
"no `uniti`" fix), and 3 (degeneracy is caught by a **structural MemState-line
audit** -- no MemState port reaches a CPU-phase active cell -- since the value oracle
provably cannot see it).

**The minimal realization (agreed):** a **seam rotation** of `step`'s phase order
so the bus emit/absorb falls *on* the revolution boundary -- keeping the CPU body a
single ordinary netlist with ports at depth 0/D (no mid-body ports, no netlist
surgery) -- plus a `CompositeRing` runner with the generalized certificate
(`restored_ok ∧ run == run_open ∧ run == eval(system_step)^R` + the line audit), a
1:1 period-locked sequencer, and an ancilla-clean 1-of-N Fredkin exchange tree
(demonstrated whole-domain at N ∈ {2,4,8} first). Roll out in two stages
(L15-safe): **Stage 1** = single-body composite ring reusing `StepRing` verbatim
(bus + tree scratch land in the ancilla section) -- exercises the *real* closure
question end-to-end before any new machinery; **Stage 2** = two-body `SystemRing`
with staggered ports for physical separability.

**Result 3 -- energy: Option B does *not* beat Option A (confirms §0 finding 4).**
"Storage is motion" (ADR-0029 C2.1): N resident words cost `N·w` token-moves per
tick regardless of topology -- in State (A) or a mem ring (B). Topology relocates
the tokens; it cannot stop them. What the bus **does** buy: (i) **area/feasibility**
-- the D4 buffer wall shrinks ~10²-10³× (a shallow mem loop vs. N words widening the
full step body); (ii) a **CPU body invariant in N** (separate compilation, its own
energy-ledger line = ADR-0029 C3.5's wake-condition meter); (iii) **the seam behind
which Option C** (a static adiabatic SRAM that finally is *not* motion -- the only
real energy escape) can drop in **without touching the CPU**. The 8-bit immediate
also caps N ≤ 256, so the asymptotics are academic today.

**Recommendation (both agents, and adopted here).** Build the **resident-memory
region** (§0) as M-C2 *now* -- at N ≤ 256 it is oracle-gated, cheap, and
**energy-identical** to the bus. Pursue the composite-ring bus **only** when (i) the
D4 area wall actually binds (State widening makes the step body unbuildable) or
(ii) Option C's wake condition fires (fabric-resident storage dominates the ledger,
ADR-0029 C3.5) -- at which point the seam-rotation + `CompositeRing` design above is
the minimal path, with the zero-garbage certificate intact *for the composite*.
The spike's value: it turned "open research problem" into a **solved-in-principle,
deferred-on-purpose** design, and proved we are not leaving an energy win on the
table by shipping resident-memory first.

---

*Original scoping design (retained as record; superseded by §0):*

**Status (original):** Proposed (design). Realizes ADR-0029's **Option B** -- memory
as a separately-compiled reversible iso with a swap-bus interface -- now that the
ring machine ([ADR-0033](0033-ring-machine.md)) exists to inform the protocol.

## 1. Where M-C2 sits

M-C1 ([ADR-0044](0044-mc-compiler-memory.md)) spills leaves into the in-`State`
scratch words -- Option A, capped at 14 leaves, and every stored word recirculates
every tick ("storage is motion", ADR-0029 C2.1). M-C2 is the **non-degenerate**
step: memory becomes a **separate recirculating block** the CPU talks to over a
**bus port**, so storage is a shared sequential resource rather than more State
that unrolls alongside the datapath. This is what makes memory *scalable* and makes
its energy a first-class, characterized line in the report (ADR-0029 C3.5).

**The degeneracy trap (the thing this whole design must avoid).** ADR-0029's
Consequences warned: if the memory iso is *monolithically composed into `step`*
over a shared `(CpuState, MemState)` type, it simply unrolls alongside the CPU --
Option A with indirection, no benefit. The non-degenerate B keeps memory as its
**own** iso/ring, connected to the CPU **only** through a `BusMsg` port that
crosses between them once per step. Keeping `step` and `mem` as *separate isos* is
the invariant that separates B from degenerate-B.

## 2. The bus interface

### 2.1 `BusMsg` -- the port (not shared state)
```text
BusMsg = (enable : Bit, addr : AddrW, word : W8)
```
`enable` = does this step access memory; `addr` = the target cell; `word` = the
datum being exchanged. It is a *port* threaded between two isos, never a shared
memory. `AddrW` = ⌈log₂ N⌉ bits for an N-word memory.

### 2.2 Memory access is EXCHANGE, always (the linear discipline)
Per ADR-0029, a load *cannot copy* (linearity, ADR-0004) -- memory access is a
**swap**: `word ↔ mem[addr]`. So there is no separate read/write; both are the one
exchange, and values **move**, they are not duplicated. The compiler already lives
under this discipline (M-C1's `Xchg` moves a spilled leaf out and back); M-C2
extends it across the bus. A "read that preserves memory" is a move-out then
move-back, exactly as spill works today.

### 2.3 The memory iso
```text
mem : (MemState, BusMsg) ↔ (MemState, BusMsg')
```
`MemState` = the N-word bank (the actual storage). When `enable`: exchange
`BusMsg.word ↔ MemState[addr]` via an **addressed-exchange tree** -- a 1-of-N
Branch/CSWAP tree on the `addr` bits steering a swap, i.e. **literally the ADR-0027
cell** (a Fredkin/K1 demux→exchange→mux, generalizing the 1-of-8 `select_fields`
crossbar already in `decoder.rs` to 1-of-N). When `¬enable`: identity. `addr`/
`enable` are preserved (they came from the instruction); only `word` and the
selected cell move. `BusMsg'` carries the swapped-out old cell value back.

### 2.4 `step` gains the port
```text
step : (CpuState, BusMsg) ↔ (CpuState, BusMsg')
```
Only the LOAD/STORE path touches `BusMsg`: `decode_execute` on a memory
instruction sets `enable=1`, `addr` from the IR address field, and swaps the target
register's word into `BusMsg.word`; after the bus round-trip it swaps `BusMsg'.word`
back into the register. All other instructions leave `BusMsg` at `enable=0`
(identity through `mem`). Every other phase (fetch, unfetch, pc_add_br, pc_inc) is
lifted to **identity on `BusMsg`**.

## 3. Composition -- value-level first, then physical (the phasing)

The full lift has a genuinely hard part (two coupled rings) and a tractable part
(the protocol + exchange correctness). Split them:

### 3.1 M-C2a -- value-level system (correctness, oracle-gated)
Compose CPU and memory at the **value level**, keeping them as separate isos:
```text
system_step : (CpuState, MemState) ↔ (CpuState, MemState)
  = introduce BusMsg (uniti) ;
    step  on (CpuState, BusMsg) ;      -- CPU emits the request
    mem   on (MemState, BusMsg) ;      -- memory performs the exchange
    step-absorb on (CpuState, BusMsg') ;  -- CPU folds the response back
    eliminate BusMsg (unite)
```
(The request-emit and response-absorb are the two halves of `step` around the bus
round-trip; concretely the `mem`-exchange phase sits between `decode_execute` and
`unfetch`, over the product type -- but `mem` stays a *distinct* iso so 3.2 can move
it to its own ring.) The differential oracle runs `system_step^k` against
`theseus_eval::eval` of the source program (which now includes memory-typed values)
over the whole finite domain. **This proves the bus protocol and the reversible
exchange are correct** without yet solving the physical two-ring problem -- the bulk
of the risk, retired first.

### 3.2 M-C2b -- physical two-ring composition (energy/scalability)
Make `mem` a **separate recirculating block** (its own ring) sharing the CPU ring's
sequencer discipline (ADR-0033: "a second recirculating structure sharing the
ring's sequencer"), so `MemState` does **not** recirculate inside `step`'s ring --
only `BusMsg` crosses between the two rings each step. This is where energy-
scalability materializes (memory is O(N) storage accessed over an O(log N) bus, not
O(N) extra State unrolled every tick). Acceptance: the coupled two-ring run
`== system_step^R` (3.1) which `== eval`.

## 4. ISA and the compiler

- **ISA:** add `LOAD`/`STORE` (both the one exchange; naming is register↔memory
  direction) in the free opcode space (10 slots: 7 R-format, 3 I-format), addr in
  the `rt`/immediate field per the `Xchg` template. Encoding room confirmed.
- **Compiler (compile-time addressing).** The compiler always *knows* which memory
  word a spilled leaf occupies, so it emits `LOAD`/`STORE` with a **constant**
  address -- no data-dependent addressing needed (that -- arrays indexed by computed
  values -- is M-D). M-C2 extends M-C1's spill: leaves beyond register+scratch get
  memory homes; `Ctx`'s `Location` gains `Mem(addr)`; `with_registers` pages a
  memory-home leaf in via `LOAD`, out via `STORE`, canonical layout preserved. The
  ceiling rises from 14 to 14 + N (bank size).

## 5. Acceptance: the run-on-CPU differential oracle (per L19)

Authority = the real system on the real `step`/`mem` isos `== theseus_eval::eval`
over the finite domain ([ADR-0008](0008-differential-testing.md); L19). M-C2a:
`system_step^k == eval` for programs that spill to memory (whole domain where
feasible, else the representative-sample discipline M-C1 established, since domains
grow with leaf count). M-C2b: the coupled two-ring run `== system_step^R`. Required
programs: a value exceeding the register+scratch file (>14 leaves) that must use
memory; a memory round-trip that both stores and reloads; no M-A/M-B/M-C1
regression. Keep the fast-sim a pinned convenience only (L19).

## 6. Energy (why Option B at all)
The memory iso lowers to a `PhasedCellNetlist` (Fredkin/K1 addressed-exchange tree +
the N-word bank) that sums into `energy.rs` exactly like the CPU's cells, appearing
as its own line in the ADR-0028 per-iso ledger. ADR-0029 C3.5's wake condition --
"fabric-resident storage dominates the energy report" -- becomes *measurable* here:
the money plot can price a program's memory traffic against its compute.

## 7. Open risks (what the adversarial review must break)
- **Ring closure with a bus port.** M4-v2's zero-garbage ring physically closes with
  *no per-revolution boundary* (ADR-0040/0033); a `BusMsg` port is by definition a
  per-revolution boundary crossing to the memory ring. Does adding it break ancilla-
  clean closure, or is `BusMsg` simply a second data-port pair the wrap threads?
  This is the highest-risk unknown and the reason M-C2b is separated from M-C2a.
- **Two-ring sequencer coupling.** Phase/timing alignment of the CPU ring and memory
  ring, and whether one shared bus access per step suffices (or an instruction needs
  several). Under-specified until 3.2 is designed against `ring_run.rs` concretely.
- **The degeneracy trap (§1).** Any implementation that folds `mem` into `step` over
  a shared `MemState` silently reverts to Option A; the review must confirm `step`
  and `mem` stay separable isos.
- **Ancilla cleanliness of the memory iso.** The addressed-exchange tree's controls/
  intermediates must Bennett-clean (like the register crossbar `select_fields` does)
  so `mem` is ancilla-restoring and ring-composable.
- **Reversibility of the exchange under `enable=0`.** The identity path must be a
  true iso (no residue in `BusMsg'`), or non-memory steps corrupt the bus.

## Cross-references
- [ADR-0029](0029-memory.md) -- memory as exchange; Option B (this ADR realizes it).
- [ADR-0027](0027-cell-library.md) -- the Fredkin/CSWAP cell (the addressed-exchange
  fabric).
- [ADR-0033](0033-ring-machine.md) -- the ring machine + its anticipation of the
  second recirculating bus structure.
- [ADR-0034](0034-reversible-branching.md) -- SWAPBR/come-from (the exchange
  discipline generalizes it).
- [ADR-0044](0044-mc-compiler-memory.md) -- M-C1 spill (the compiler path M-C2
  extends: `Location`, `with_registers`, marshalling).
- [ADR-0028](0028-energy-annotation.md) -- the per-iso energy ledger the memory
  fabric enters.
- [ADR-0008](0008-differential-testing.md) -- the differential-oracle norm (L19).
