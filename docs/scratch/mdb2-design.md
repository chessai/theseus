# M-D.b2 — compiler-side MEXCHR emission (data-dependent memory addressing from `.thse`)

**Status:** design of record — **REVIEWED** (fresh-context Fable, verdict *DO-NOT-SHIP as
drawn* → **all findings folded in**; `docs/reviews/mdb2-design-review.md`). Route **(i)** chosen
for F-1 (`Idx = Bit`, `n = resident = 2`). Control-flow/lowering + marshalling change →
CLAUDE.md adversarial-review bar (design ✓ done, impl pending), reproducing tests per finding
(L20/L21).

> **Post-review deltas (what the review changed):** (F-1) f1 uses a **2-word** array indexed
> by a **`Bit`** — a single-leaf type only holds a *thermometer tag ∈{0,1}*, so a 4-value
> index is 3 leaves and cannot be "one address register" without a packing bracket (deferred).
> (F-2) the recognizer now **verifies the arr leaves are home at `[Mem(0)..Mem(n-1)]` in order**
> and rejects otherwise — this guard *is* the soundness of the whole side-parameter convention.
> (F-3) the prim is **semantically verified** over its whole domain before emission (not merely
> name-matched). (F-4) the real coherence rule is *every representable index `< n`, or
> `n == MEM_WORDS`* — not "`n == MEM_WORDS` by construction." (F-5) marshal/unmarshal/Ctx share
> **one** `home_of`. (F-6) resident/spill collision is already prevented by the occ machinery
> *provided `Ctx::new_resident` seeds `occ` from its homes* — an explicit obligation, no new
> allocator code.

## 0. The finding that motivates this (feedback-loop)

The test-program roadmap picked "iter + MEXCHR array-map" as a *fits-today* win. Grounding it
in the code first (the L20 discipline) overturned that: **the compiler emits zero `MexchReg`
instructions.** Evidence (tip `c76b6f3`):

- `grep MexchReg crates/theseus-tisc/src/`: every hit is *definition* (`asm.rs:131`),
  *encode* (`asm.rs:229`), *decode* (`asm.rs:267`), the CPU-iso `Op` enum + a panic guard
  (`decoder.rs:748,795`), and the fast-**simulator** execute arm (`compile.rs:2513`). **No
  codegen site.**
- Spill only ever emits *immediate*-addressed `Mexch { rd, addr }` at compile-time-constant
  addresses (`compile.rs:504`, via `swap_homes`/`with_registers`).

So **M-D.b delivered the *machine* capability, not the *compiler* capability.** The CPU
executes `MEXCHR rd,ra` (`reg[rd] ↔ mem[reg[ra] % MEM_WORDS]`) and the `SystemRing` closes
zero-garbage over it — verified by a *hand-assembled* oracle (`tests/mexch_reg.rs`) — but no
`.thse` program can *produce* one. M-D.b2 builds the missing front half.

The array-map is therefore a **Tier-3 customer** (sibling of TP-7 array-sum), not fits-today.
But driven by `iter` (bounded count) it dodges **both** the TH-1b sizing wall (unlike
GCD/Collatz) **and** the Trace-with-mem-body gap (unlike TP-6/7) — the **simplest** program
that isolates "compiler emits data-dependent addressing."

## 1. Goal & increments

A Core `Iso` that performs a **runtime-indexed memory exchange** compiles to a ROM that
**emits `MexchReg`** and satisfies `run_on_cpu == eval` (L19), then is priced by M-E.

- **M-D.b2.f1 (this task):** the minimal driver — a single indexed exchange `memexch` over a
  **2-word** resident array, addressed by a **`Bit`** whose value is a *runtime program input*.
  One `MexchReg` emitted; `run_on_cpu_resident == eval` over the whole (small) domain.
- **M-D.b2.f2 (follow-on):** `iter memexch` = the array map/reduce; a **second M-E energy
  artifact** (memory-touching, per-iteration bus traffic).

Deferred: the **n=4 packing bracket** (F-1 route (ii): thermometer→`ADD/SUB` pack around the
exchange); `MEM_WORDS`/PC widening (M-D.c); a typed `Type::Mem` + surface syntax (ergonomics —
would catch F-2 by construction, a point in its favor for later); Trace-bodied memory loops
(the M-D.d follow-on for TP-6/7).

## 2. What forces `MexchReg` (the honest driver)

A **compile-time-constant** address already compiles — to immediate `Mexch { rd, addr }`
(spill). `MexchReg` is *only* required when the address is a **runtime datum** the compiler
cannot constant-fold: one ROM serves every input, so the address must be sourced from a
register at run time. **f1's address is a program input** (the `Bit`), so constant-folding is
impossible → `MexchReg` is mandatory.

## 3. The three gaps (from the code survey)

- **Gap A — no resident-array marshalling.** `marshal` zero-inits `mem` and assigns leaves by
  *position only* (0–7→reg, 8–13→scratch, 14..→mem; `compile.rs:2291`, `Ctx::new` `363`). A
  resident array element must land at a leaf ≥14 — today that needs 14 junk leaves ahead of it.
- **Gap B — no `MexchReg` emission.** `recognize_prim` (`794`) has no arm producing it.
- **Gap C — no address→register wiring.** Nothing routes a runtime value into the `ra` field.

## 4. Design

### 4a. Carrier & marshalling — one shared `home_of` + a `resident` count (theseus-tisc-local)

Thread a `resident: usize` = "the last `resident` leaves of the carrier live in `mem[0..resident]`"
through the tisc marshalling + home layout. **All placement goes through ONE function** so
marshal-in, unmarshal-out, and the `Ctx` homes can never drift (F-5):

```
fn home_of(leaf: usize, n_leaves: usize, resident: usize) -> Location
    // trailing `resident` leaves  [n-resident, n)  ->  Mem(leaf - (n - resident))
    // head leaves                 [0, n-resident)  ->  Reg(0..8) then Scratch(2..8)   (existing rule)
```

- `marshal_resident`/`unmarshal_resident` write/read leaf `i` at `home_of(i, n, resident)`.
- `Ctx::new_resident(n, resident)` sets `homes[i] = home_of(i, n, resident)` **and seeds
  `occ = OccSet::from_locations(&homes)`** exactly as `Ctx::new` does (F-6 — this is what keeps
  `allocate_aux` from ever choosing a live resident word as a zero-slot; there is *no* separate
  collision detector to build). `resident == 0` reproduces today's behavior bit-for-bit.
- **Constraints (rejects, load-bearing):** `resident ≤ MEM_WORDS` **and** head leaves
  `n - resident ≤ 14`. Violation → reject (today's `TooManyLeaves` is acceptable but mislabels
  the cause — note this in the error message). With both bounds, head homes are Reg/Scratch
  only and can *never* alias `Mem(0..resident)` (F-5 aliasing half — closed).
- **Oracle:** `run_on_cpu_resident(iso, input, resident, eval_fn)` threads the parameter into
  marshal/run/unmarshal. `eval` is **unchanged and unaware** — resident-ness is purely
  *placement*, so `cpu_out == eval_out` holds iff marshal-in and unmarshal-out use the same
  `resident` (they do, via `home_of`). Assert `resident > 0 ⇒ num_leaves(in_ty) ==
  num_leaves(out_ty)` in the wrapper (F-9; trivially true for the endo `memexch`).
- A resident-aware compile entry is needed too: `compile_with_metadata_resident(iso, resident)`
  (today's `compile_with_metadata` hardwires `Ctx::new(n)`, `compile.rs:1434`) so T1 can assert
  on the emitted instr vector (F-9).

*Why not a `Type::Mem(τ,n)` core type now?* It is the honest long-term rep (and the minimal
M-D.c heap), and it would catch F-2 by construction — but it ripples through
`type_of`/`num_leaves`/eval/check/enumerate, too big for one reviewable task. f1 uses the
side-parameter **guarded by F-2's recognizer check**; the typed rep + surface syntax is the
named follow-on.

### 4b. The `memexch` primitive (a generated, semantically-verified `Prim`; no eval change)

`memexch` is a `PrimDef` (forward/backward `BTreeMap<Value,Value>`) built by
`fn memexch_prim(elem_ty, n) -> Result<PrimDef, _>`, so **eval needs no new arm** (it
interprets `Iso::Prim` by table lookup) and syntax/check are untouched (the f1 test builds the
`Iso` programmatically). For f1: `elem_ty = Bit`, `n = 2`.

```
memexch : (data:Bit) * (addr:Bit) * Arr[Bit;2]  <->  (data:Bit) * (addr:Bit) * Arr[Bit;2]
   Arr[Bit;2] = the resident factor (2 trailing leaves at mem[0],mem[1]);  addr ∈ {0,1}
   semantics:  (d, a, arr)  ↦  ( arr[a], a, arr[a ← d] )        // a is already < n = 2
```

**Coherence condition (F-4, the real rule — replaces the old "coincide by construction"):**
the generator MUST guarantee **every representable index value `a < n`** (so `a mod n = a =
a % MEM_WORDS` since `n ≤ MEM_WORDS`), **or** `n == MEM_WORDS`. f1's `|Idx| = n = 2 ≤ MEM_WORDS`
satisfies the first disjunct — both moduli collapse to the identity. The generator returns
`Err` if handed an index type with `> n` inhabitants (F-4 test).

**Bijection / self-inverse.** The op swaps the single location `data` with `arr[a]` while
leaving `addr` untouched (guaranteed by `ra ≠ rd`), so it is an **involution** → a bijection;
coverage/linearity passes and backward-table = forward-table. Set `backward_name = "memexch"`
too so `Sym(memexch)` recognizes (F-8). No Bennett witness needed (contrast GCD). With the F-4
constraint every representable `a` is in `[0,n)`, so the involution holds pointwise (F-4 folds
in the old §8.5 concern).

### 4c. Emission — semantic verification + home-layout guard (Gap B+C; F-2, F-3)

Add a `recognize_prim` arm. It does **not** trust the name; it **verifies the table** and the
**arr home layout** before emitting:

```
// 0. fast pre-filter: def.forward_name == "memexch" && def is 2-head-leaf + n resident leaves
// 1. SEMANTIC VERIFY (F-3): for every value in the enumerated input domain, decompose leaf
//    bytes as (d, a, arr[0..n]); check output == (arr[a], a, arr[a ← d]) with idx = a % MEM_WORDS
//    (the CPU's rule). Any mismatch  -> Err(UnrecognizedPrim).  (O(|table|); domain ≤ 128.)
//    This also structurally forces num_leaves(elem_ty) == 1 (a multi-leaf element can't match).
// 2. HOME-LAYOUT GUARD (F-2): assert the n arr leaves are home EXACTLY at
//    [Mem(0), Mem(1), …, Mem(n-1)] IN ORDER (element j ↔ mem[j] is what the table assumes).
//    Otherwise -> Err(ResidentMisplaced).  << this guard is the soundness of §4a >>
// 3. EMIT: with_registers pages data/addr into regs (rd, ra); reject rd == ra
//          (AliasedRegisters — reuses asm.rs MexchAddrAliasesData);  push MexchReg { rd, ra }.
```

`with_registers` pages `data`/`addr` into registers if home in scratch; the resident `arr`
leaves stay in `mem` and are addressed *implicitly* — which is exactly why step 2's guard is
mandatory: "implicit" means the *absolute* `mem[a]`, so if a composition (`Times(memexch, g)`)
shifted the arr leaves off `Mem(0..n)`, the instruction would read the wrong word (or clobber
`g`) and compile clean. Step 2 turns that silent corruption into a loud reject.

### 4d. Bus-quiescence / zero-garbage closure — reuse M-D.b

The compiled program emits exactly the `MexchReg` whose **`SystemRing` closure and
bus-quiescence M-D.b already proved** (`tests/mexch_reg.rs`, `absorb = sym(emit)`, N∈{2,4,8}).
A compiled `memexch` inherits closure — but the impl must *run* the real-CPU / `SystemRing`
oracle (T4) to demonstrate it (L19). No new closure theorem; a witness that emission preserves
it.

### 4e. WCET / M-E

f1's `memexch` is **straight-line**: `B = T`, park tax 0 (honest — not itself an interesting
energy datapoint). Its value for M-E: `rom_program_energy` prices a *memory-touching* ROM (a
`system_step` net with a live `MexchReg`, different K0-buffer inventory from fib). The park-tax
story returns in **f2** (`iter memexch`). T7 just checks `rom_program_energy` runs on a
MEXCHR-bearing ROM and prices `> 0`, `total == B·step`.

## 5. Scope of f1 (one delegation — advisor-one-task-per-delegation)

IN: `home_of` + `marshal_resident`/`unmarshal_resident`/`Ctx::new_resident` (occ-seeded) + the
`resident`/head bounds rejects; `compile_with_metadata_resident`; `memexch_prim(Bit,2)`
generator (with the F-4 index-range check); the `recognize_prim` arm with **semantic verify
(F-3) + home guard (F-2)** → `MexchReg`; `run_on_cpu_resident`; the oracle + reject tests (§7).
OUT (follow-ons): n=4 packing (F-1 (ii)); `iter memexch` map/reduce (f2) + its M-E artifact;
`Type::Mem` + surface syntax; MEM_WORDS/PC widening.

## 6. Reject-don't-corrupt boundary (each → a negative test)

- **`n > MEM_WORDS`, `resident > MEM_WORDS`, or head leaves `> 14`** → clean reject
  (`TooManyLeaves`/`ResidentOverflow`), never a silent wrong address.
- **Index type with `> n` representable values** (F-4) → generator/verifier `Err`, never a
  `mod`-mismatch corruption.
- **arr leaves not home at `Mem(0..n)` in order** (F-2) → `ResidentMisplaced`, never a shifted
  read/clobber. *This is the most important reject.*
- **`rd == ra`** (data aliases address) → `AliasedRegisters` (bus-quiescence-critical).
- **Forged "memexch" table** (F-3) → `UnrecognizedPrim`.
- **Resident/spill collision** — *not a new detector*: the occ-seeded `Ctx::new_resident`
  (F-6) makes `allocate_aux` refuse to place a spill on a live resident word; when mem is
  exhausted it errors `TooManyLeaves` (which mislabels the cause — acceptable, noted).
- **N/A for f1 (F-10):** "constant address → immediate `Mexch`" — no compiler path
  constant-folds a program input, and f1's memexch always takes the register route; keep the
  boundary statement but ship *no* test (not-tested-because: no such behavior in f1).

## 7. Acceptance (every finding → a reproducing test — L20/L21)

- **T1 (emission, F-1):** `compile_with_metadata_resident(memexch, 2)` yields a ROM
  **containing `MexchReg`** (assert on the instr vector).
- **T2 (fast oracle, whole domain, F-1):** `run_on_cpu_resident (fast) == eval` over the entire
  carrier domain (`data,addr ∈ Bit`, `arr ∈ Bit²` → 16 inputs), **including the maximal address
  `a = n-1 = 1`**.
- **T3 (real-CPU oracle, `#[ignore]`, F-1):** `run_on_cpu_resident (real system_step) == eval`
  on a sampled subset — the L19 authority; run pre-merge via `--ignored`.
- **T4 (closure):** the compiled ROM's `MexchReg` step closes on the `SystemRing`
  (`run == eval^R`, bus-quiescent) — reuse the M-D.b harness.
- **T5-F2a (reject):** `Times(memexch, id_Bit)` under a misaligning `resident` → `ResidentMisplaced`
  (without the guard the fast oracle diverges — the test's teeth).
- **T5-F2b (positive):** `Times(id_Bit, memexch)` with arr globally trailing → compiles,
  whole-domain oracle passes.
- **T5-F3 (forge):** a `PrimDef` named "memexch" whose table swaps at `(a+1) mod n` →
  `UnrecognizedPrim` (never `MexchReg`).
- **T5-F4 (over-wide index):** `memexch_prim(Bit, 2)` handed a 3-value index type → `Err`.
- **T6 (marshal↔Ctx home property, F-5):** ∀ `i`, the byte `marshal_resident` writes for leaf
  `i` lands at exactly `Ctx::new_resident(n,r).homes[i]`; plus round-trip
  `unmarshal_resident∘marshal_resident == id` for `resident > 0`.
- **T5-F6a (spill exhaustion):** `resident = MEM_WORDS` + a branching program exhausting scratch
  → `TooManyLeaves`, not corruption.
- **T5-F6b (displacement disjoint):** `resident = 2` + a program forcing one mem displacement →
  whole-domain oracle passes and the displacement `Mexch` addr is `≥ 2` (disjoint from resident).
- **T5-F7 (paging path):** a junk-padded carrier (≥8 head leaves) putting `data`/`addr` in
  scratch → sampled oracle passes, ROM contains the `Xchg` bracket around `MexchReg`.
- **T5-F8 (adjoint):** `Sym(memexch)` compiles to the same single `MexchReg`; oracle passes.
- **T7 (M-E smoke):** `rom_program_energy` prices the MEXCHR-bearing ROM `> 0`, `total == B·step`.
- OOM discipline: any test that lowers `system_step` (T3, T4) is memoized + serialized +
  `#[ignore]` for the heavy tier (the `mexch_reg` consolidation precedent, `c229920`); the
  fast-sim tiers stay in the default gate.

## 8. Residual review obligations for the IMPL reviewer

The design review closed the design-level holes; the impl review must confirm the code
*realizes* them: (1) `home_of` is the **sole** placement function (no re-implemented rule in
marshal/unmarshal/Ctx); (2) the recognizer's semantic verify + home guard actually run
*before* emit and their rejects are reachable (T5-F2a/F3 fail without them); (3) `occ` is seeded
in `Ctx::new_resident` (T5-F6a fails without it); (4) T2/T3 include `a = n-1`. Prefer a
different model from the implementer.
