# lean-lower_iso — design (TISC-2 Phase-B prep)

Status: DRAFT (investigation complete; not committed). Read-only investigation
on branch `lean-lower-iso` (worktree `/tmp/leanlower`, off merged v2 `9b12f7e`).

Goal: un-defer the wide-state (STATE_BITS=184, W_PC=16) energy and ring tests
(`me_a3_golden`, the `ctx()`-based `ring_run` oracles) that OOM (SIGKILL /
SIGABRT "memory allocation failed") EVEN ALONE at ~86 GB, so we can (a) re-pin
the fib energy golden at wide state, (b) verify ring-closure at wide state, and
(c) measure Phase-B joules. This is **authority infrastructure**: the fix must
change ONLY the memory used to compute the energy/ring values, never the values
themselves.

---

## 0. TL;DR — where the memory blows (measured)

The hog is **NOT** the balancer and **NOT** the final netlist. The energy and
ring paths are already analytic/compact (they never materialize the ~1.3 M K0
balance buffers). The OOM is inside **`lower_iso` itself** — specifically the
`canonicalize(iso)` → `lower(&net)` pipeline — because the intermediate `Iso`
and `TypedNode` trees for a full `step(rom)` carry a **deep-cloned
`Box<Type>` payload of O(STATE_BITS) on every one of O(gates × routing-depth)
plumbing nodes**. That is ~O(gates × NUM_FIELDS²) `Type`-node allocations, and
it is superlinear in the state width — which is why +8 state bits (176→184)
tips a ~57 GB working set (small 2-word ROM) past ~86 GB (24-word fib ROM).

Fix (highest-leverage, smallest slice first): make `Type` cheap to clone by
structural sharing (`Rc<Type>` / interning) so the plumbing nodes stop
deep-copying O(width) payloads. This is a pure representation change that
preserves the produced netlist bit-for-bit (verified by lowering a small ROM
old-vs-new and asserting identical `PhasedCellNetlist`). Expected peak-memory
reduction: from ~86 GB to well under 40 GB (likely a few GB), because the
active netlist is only ~5,400 cells (~2 MB) and `num_lines` is modest — the
84 GB is entirely the redundant Type deep-copies.

---

## 1. Measured evidence (this investigation)

All runs: `nix develop`, `MALLOC_ARENA_MAX=1`, `--test-threads=1`, dev profile
(theseus-core / theseus-eval / theseus-netlist at opt-level 2; `debug_assert`
ON). Host: 94 GB, ~79 GB free.

| run | path exercised | result | peak RSS |
|-----|----------------|--------|----------|
| `energy::me_a1_total_is_b_times_step` (fib, 24-word ROM) | `lower_iso(step)` **only**, then analytic energy (NO `balance()`) | **SIGKILL** during "lowering step(rom)…" | **~86 GB** (90 312 448 KB) |
| `energy::step_balanced_energy_positive` (2-word `test_rom`) | `lower_iso(step)` + **analytic** `balanced_energy` (NO `balance()`) | ok (127 s) | **~57 GB** (57 353 728 KB) |
| `energy::analytic_matches_balance_pc_inc` | `lower_iso(pc_inc)` + `balance()` (materializes) | ok (2.5 s) | ~1.1 GB |

Decisive facts:

1. **me_a1 uses the analytic path** (`fib_program_energy` → `fib_step_net()` →
   `lower_iso`; energy via `analytic_buffer_count`, never `balance()`), yet it
   SIGKILLs at ~86 GB *during the `lower_iso` call itself* (printed
   "lowering step(rom)…" then died). So the OOM is inside `lower_iso`, not in
   balancing or simulation. (Note: `me_a1` is currently NOT `#[ignore]`d in
   `energy.rs` but is in fact already OOMing at the wide state — a second
   regressed test beyond the ones the prompt lists.)

2. The `step_balanced_energy_positive` breakdown shows the produced net is TINY:
   `cswap-k1: 4580`, `cswap-k2: 84`, `cswap-r1: 742` → **~5 406 active cells**
   (~2 MB of `Cell` structs). The **1 286 311** figure is the *analytic* K0
   balance-buffer COUNT (a `usize` sum, not materialized). So a 57 GB peak to
   produce a 2 MB netlist proves the memory is a transient intermediate, not the
   output.

3. Both the 2-word and 24-word ROMs share the same expensive
   `decode_execute` (shared crossbar) + `pc_add_br` + `pc_inc`; they differ only
   in the `fetch` entry count. So the dominant cost is state-width-driven, not
   ROM-length-driven.

---

## 2. The dominant allocation (struct × count × size, file:line)

**Struct:** `theseus_core::Type` — `enum Type { … Sum(Box<Type>,Box<Type>),
Prod(Box<Type>,Box<Type>) … }` (`crates/theseus-core/src/ty.rs:10-18`).
`Type` has **no structural sharing**: every `.clone()` is a deep, O(size) copy
(each `Box` is a fresh heap allocation).

**Where full-width Types get cloned per gate:**

- `at_fields_n` (`crates/theseus-tisc/src/addressing.rs:119-151`) lifts every
  register-addressed operand iso to the full `NUM_FIELDS`(=23)-field state. It
  builds:
  - `route_to_head_n(indices, n_fields)` — bubble-routing, **O(NUM_FIELDS²)**
    adjacent `swap_field_adj` isos worst case
    (`addressing.rs:59-81`), and
  - `Iso::times(op, Iso::Id(fields(rest_count)))` where `fields(rest_count)` is
    a right-nested product **Type of ~NUM_FIELDS Word8s = O(STATE_BITS) leaves**
    (`addressing.rs:142,147`), cloned per gate.
- `swap_field_adj(n,k)` (`addressing.rs:33-53`) clones `rest = fields(n-2)`
  **three times** per swap (lines 47/48/50), each an O(state-width) Type.
- `split_fields(k,r)` (`addressing.rs:89-105`) recurses, cloning `fields(k-1)`
  and `fields(r)` at every level.
- Bit-level `gate_at` (`crates/theseus-tisc/src/plumbing.rs:141-156`) does the
  same at bit granularity inside each field group: `route` = up to O(n) `swap_adj`
  isos, applied as `route ; head_k ; sym(route)`; `swap_adj`
  (`plumbing.rs:82-100`) and `head_k`/`split_bits` (`plumbing.rs:113-137`) each
  clone `flat(...)` Types of O(field-group-width) leaves.

**Then it doubles:** `canonicalize` (`crates/theseus-netlist/src/canonicalize.rs:37`)
walks the whole `Iso` tree and produces a parallel `TypedNode` tree in which
**every node stores its own `input: Type` and `output: Type`**
(`TypedNode { input, output, kind }` — e.g. Seq at `canonicalize.rs:62-81`,
Times at `:85-95`, SwapProd at `:112-127`). Each `.clone()` there is again a
deep O(width) copy. During `lower_iso`, the original `Iso` tree (built by
`step()`) and the `TypedNode` tree are **both alive simultaneously**, so peak =
Iso-tree Types + TypedNode Types.

**Scale estimate.** `decode_execute` is a sequence of ~15 instruction bodies
(`decoder::decode_execute_shared`), each with operand crossbars + arithmetic =
thousands of register/bit gates. Each gate's routing carries O(NUM_FIELDS) or
O(field-bits) full Types, and the routing chain length is itself O(NUM_FIELDS).
So total `Type`-node allocations ≈ **O(gates × NUM_FIELDS × STATE_BITS)** ≈
low-billions of small heap nodes → tens of GB. This is the 57–86 GB.

**Why +8 bits tips it over (176→184, NUM_FIELDS 22→23).** The growth is
**superlinear**: the per-node Type payload grows (`fields(rest)` gains a Word8),
AND the routing chain lengthens (`route_to_head_n` over 23 vs 22 fields), AND
`STATE_BITS` grows — the product of these is ≈ (184/176) × (23/22) × (chain
factor) on an already-quadratic base. A ~5 % linear bump on a quadratic term is
the ~10 % working-set inflation (84→86+ GB) that crosses the host-RAM cliff. The
committed cell-count delta (+~193 cells, +4 %, `step.rs:312-325`) measures the
OUTPUT growth, which is faithful for energy — but the *lowering peak* grows on
the Type-clone term, which is far steeper.

---

## 3. Is energy analytic already? (yes) — can it stream? (the net already is
     the peak, so the win is in the Type representation, not streaming)

`balanced_energy` (`crates/theseus-tisc/src/energy.rs:111-141`) is analytic:
`active_j` from `compiled_energy(net, …)` (a Σ over cells) + `buffer_j =
analytic_buffer_count(net) × k0_cost`. `analytic_buffer_count`
(`energy.rs:88-105`) computes only the *count* of K0 buffers the balancer would
insert (Σ per-input arrival deficits from `line_arrivals`), **never
materializing them** — memory O(cells + lines). Confirmed by the docstring
(`energy.rs:1-20`) and by `step_balanced_energy_positive` running the analytic
path to completion with the balance buffers left as a `usize` (1.28 M) and never
allocated as cells.

So the energy computation downstream of `lower_iso` is already lean. The peak is
**entirely** the one-time `lower_iso(step)` that produces `step_net`. The full
per-step netlist IS materialized once (it must be — `analytic_buffer_count`,
`compiled_energy`, and `balance_compact` all walk `net.cells`), but that net is
only ~5 400 cells / ~2 MB. The 84 GB is the transient Type-clone garbage created
*while building* that 2 MB net, not the net.

Because the step net is a single body (energy is `B × step_j`, exact model —
`energy.rs:376-447`), there is nothing to stream across B: the whole cost is one
`lower_iso`. The reduction therefore comes from making that one lowering cheap,
not from chunking over steps.

---

## 4. The ring path — same hog

`ring_run::step_ring` (`crates/theseus-tisc/src/ring_run.rs:68-79`) calls
`lower_iso_with_ancillas(&step(rom))` → the same `lower_inner`
(`lower.rs:1472`) → same `canonicalize`+`lower` Type-clone explosion, THEN
`balance_compact(&net)` (`balance.rs`, already O(cells) — stores per-port delay
COUNTS, never K0 cells) and `CompactBalanced::simulate` (allocates
`Vec<_>` of `num_lines`, ~MB). So the ring `ctx()` tests OOM in exactly the same
place as energy. Fixing `lower_iso` fixes both. No separate ring change needed.

---

## 5. Fix options and the choice

(a) **Structural sharing for `Type` (`Rc<Type>` / interning). ← CHOSEN.**
    Change `Sum`/`Prod` to hold `Rc<Type>` (or intern Types in a global/context
    arena and pass handles). `.clone()` becomes O(1) refcount bump instead of
    O(width) deep copy. Every plumbing node that today deep-copies `fields(rest)`
    / `flat(...)` / node `input`/`output` now shares one allocation. This
    collapses the O(gates × width²) Type-node allocations to O(distinct Types) ≈
    O(width) live Type nodes total. **Preserves semantics exactly**: `Type` is
    an immutable value type used only for shape/dispatch; sharing the same
    subtree behind an `Rc` is observationally identical (equality, `flatten_layer`,
    `rail_width`, everything reads the tree, never mutates it). This is the
    highest-leverage, smallest, lowest-risk change — it attacks the measured
    dominant allocation directly and touches no lowering logic.

(b) leaner **cell** representation (bit-pack `Cell`, small-vec inline ports).
    Rejected as primary: the cells are only ~2 MB; this saves nothing on the
    84 GB. Keep in reserve for a later constant-factor pass.

(c) chunked / streaming **balance**. Not needed — balance is already compact
    (`balance_compact`) and analytic; it is not on the OOM path.

(d) per-step template × B. Not applicable — energy is already `B × step_j` from
    ONE lowered step net; there is no per-step re-materialization to eliminate.

Secondary lever if (a) is insufficient (it should be): **avoid holding the `Iso`
tree and the `TypedNode` tree simultaneously** — e.g. lower incrementally so the
`step()` Iso subtrees can be dropped as they are canonicalized. With `Rc<Type>`
this is likely unnecessary, but it is the fallback if the non-Type structure
(the `Iso`/`TypedNode` node structs themselves) turns out to also be large.

---

## 6. Preserving EXACT energy/ring values — verification

The fix changes representation, not the produced `PhasedCellNetlist`. Verify by:

1. **Golden-net equality on a small ROM (the POC gate).** Lower `step(test_rom)`
   (2-word) with old and new `Type` representation and assert the two
   `PhasedCellNetlist`s are **equal** (`PartialEq` is derived on
   `PhasedCellNetlist`/`Cell`/`RailPair` — `ir/cell.rs:204,222`). Bit-identical
   net ⇒ identical `analytic_buffer_count`, `compiled_energy`, `balance_compact`,
   `simulate` ⇒ identical energy and ring values. This is a fast test
   (`step_balanced_energy_positive` already runs the 2-word net to completion).
2. **Same analytic energy numbers.** Re-run `analytic_matches_balance_*` (the
   analytic == materialized-`balance()` equality tests) — they must still pass
   unchanged.
3. **Wide-state fib fits.** Re-enable `me_a1`/`me_a2` and confirm they pass under
   a <40 GB cap; then RE-MEASURE and re-pin `me_a3_golden` (its old golden
   6.4930e-8 / 2.7054e-9 is stale for W_PC=16 by design — the widened
   `pc_add_br`/`pc_inc` + narrow fetch shift `step_j`; `energy.rs:320-329`).
   The re-pin is a *new* golden at the wide state, justified by the cell-count
   delta already recorded in `step.rs:312-325`.
4. **Ring closure.** Re-enable the `ctx()`-based `ring_run` oracles
   (`ring_matches_eval_oracle_fast`, and siblings at lines 99/152/185/279) and
   confirm `ring.run == eval^R` at the wide state.

---

## 7. Slices (smallest first)

- **Slice 0 (POC / measurement harness).** A test that lowers `step(test_rom)`
  and records `net.cells.len()`, `net.num_lines`, and peak RSS. Establishes the
  before/after baseline. (Read-only measurement; no logic change.)
- **Slice 1 (the fix). `Type` structural sharing.** Convert
  `Sum`/`Prod` to `Rc<Type>` (or intern), fix constructors `Type::sum`/`Type::prod`
  (`ty.rs:26-31`) and all pattern matches. Gate on Slice-0 golden-net equality
  for `step(test_rom)` (old vs new bit-identical) + all existing netlist/tisc
  tests green. **This is the highest-bar change** (touches `theseus-core::Type`,
  used everywhere) → full adversarial review + the golden-net-equality
  regression test that reproduces the OOM-fix (fails to fit / differs without
  the change, fits+identical with it).
- **Slice 2 (un-defer wide-state authority).** With Slice 1 landed: re-enable
  `me_a1/a2`, RE-MEASURE + re-pin `me_a3_golden`, re-enable the `ctx()`
  `ring_run` oracles. Each re-pin/re-enable is its own small, reviewed commit.
- **(Reserve) Slice 3.** If Slice 1 lands under target but headroom is wanted:
  the "don't hold Iso + TypedNode simultaneously" incremental-lower refactor,
  and/or `Cell` bit-packing.

Target: fib `me_a3` peak < 40 GB so it runs concurrently with siblings
(currently ~86 GB alone). With Type-sharing the expected peak is dominated by
the ~2 MB net + O(width) shared Types + the Iso/TypedNode node structs — plausibly
low single-digit GB.

---

## 8. Highest-bar note

`Type` is the single most load-bearing type in the codebase (semantics, typing,
lowering, energy, ring all read it). A representation change here is exactly the
"authority infrastructure" case: it must be adversarially reviewed by a
fresh-context agent and must ship the golden-net-equality regression test
(reproduces the finding: identical produced netlist old-vs-new on `test_rom`,
and the wide-state fib now fits). Do NOT re-pin any energy/ring golden in the
same commit as the representation change — land the memory fix first proving
bit-identical nets, THEN re-measure the wide-state goldens as separate commits,
so a golden shift can never be silently attributed to the refactor.

---

## 9. Open questions

1. `Rc<Type>` vs a global intern table (`Arc` for thread-safety across the
   `--test-threads` gate?). `Rc` is simplest and the lowering is single-threaded
   per net; the `LOWER_GATE` mutex already serializes the two heavy lowerings.
   Interning gives O(1) `Type` equality too (a bonus for `canonicalize`'s
   `fn_.output != gn.input` checks) but is a bigger change. Recommend `Rc<Type>`
   first; intern only if equality-check CPU shows up.
2. Does `unsafe_code = "deny"` (workspace lint, `Cargo.toml:41`) interact with
   any intern implementation? `Rc` is safe; a hand-rolled arena might not be.
   Prefer `Rc`.
3. Is any code relying on `Type` being `Send`/`Sync` across threads (which `Rc`
   is not)? Audit needed before Slice 1 — if a test lowers on multiple threads,
   use `Arc`. (The heavy paths serialize via `LOWER_GATE`, so likely fine.)
4. Secondary check: after Slice 1, re-measure to confirm the residual peak is the
   net (~MB) + node structs, not a second O(width²) source hiding behind the
   Type clones (e.g. the `HashMap<RailPair,…>` in `controlled_apply`
   (`lower.rs:1014`) — bounded by arm size, expected small, but verify).
