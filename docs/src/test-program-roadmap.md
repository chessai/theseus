# Test-program roadmap (the compiler→CPU stress suite)

A living roster of **non-trivial reversible programs** that stress the surface→TISC-1
pipeline (`theseus-tisc::compile` → ROM → CPU → `run_on_cpu == eval` → energy). It runs as
a **concurrent feedback loop** with the [main roadmap](roadmap.md):

- **Tier 1/2 programs** *fit* the current pipeline. They are coverage + **M-E energy
  artifacts** -- especially ones whose *trace length is data-dependent*, which turn M-E's
  WCET **park-tax** story from "one number (fib)" into a *histogram*.
- **Tier 3 programs** *productively break* the pipeline. Each is the concrete **customer**
  for a main-roadmap milestone (M-D.c widening, M-D.c2 heap, M-D.d2 calling convention,
  Trace-with-a-memory-touching-body). We write the `.thse`, watch it hit a specific guard,
  and let *that* prioritise/justify the milestone -- instead of building on spec (the
  ADR-0007 "no customer" test).

**The loop:** a program that fails to compile files a finding naming the milestone it
needs; when that milestone lands, the program graduates to a Tier-1 coverage + energy
artifact and its golden energy/park-tax joins the regression suite. Every program ships
with `run_on_cpu == eval` (L19 real-CPU oracle) and, once M-E lands, an energy + park-tax
figure.

> **Reversibility is the crux.** theseus is total and reversible, so every program must be
> a **bijection**. Most classic algorithms aren't injective (GCD `(a,b)↦gcd` loses
> information), so each needs a Bennett-style formulation -- preserve inputs and/or emit the
> decision trace. For `Trace`-based programs the **come-from witness *is* that trace**, so
> the loop is reversible by construction. Finding the bijective formulation is the first
> design sub-task of each program.

## Stress axes (what there is to push on)

| Axis | Current limit | Stressed by |
|---|---|---|
| Leaf ceiling | 18 straight-line / 16 branching (N=4) | wide values → scratch+memory spill |
| 8-bit PC / ROM | ≤256 words; `B + S_max ≤ 256` | program size + WCET; deep loops/unrolls |
| Register pressure | 8 regs + s2-s7, `OccSet` | many simultaneously-live values |
| Data-dependent addressing | `MEXCHR reg[ra] % N` | memory *indexed by a computed value* (table lookup) |
| Reversible control | iter / fold / `Trace` | data-dependent iteration **count** |
| Bus-quiescence | zero-garbage closure/revolution | heavy MEXCH/MEXCHR traffic |
| WCET **park tax** (M-E) | `(B−T)/B` | inputs halting at wildly different step counts |

## Roster

| ID | Program | Stresses | Needs / drives | Status |
|---|---|---|---|---|
| TP-0 | `fib` (`iter Nat fibStep`) | iter, spill, memory | -- (baseline; M-E artifact) | ✅ compiles; **M-E priced: now 7.23e-8 J post-Ccnot (was 6.62e-8), park tax 16.67%** |
| TP-1 | GCD (subtractive Euclid) | `Trace`, sizing wall | **M-D.e + widen-PC + k_max-tighten** | ⛔ Tier-3 (customer; *overdetermined* -- TH-1b) |
| TP-2 | Bounded Collatz | `Trace`, sizing wall | **M-D.e + widen-PC + k_max-tighten** | ⛔ Tier-3 (customer) |
| ~~TP-1a~~ | ~~orbit-length (LFSR-to-target)~~ | -- | -- | ❌ **overturned** -- not a bijection (converges on one exit state) |
| TP-3 | LFSR / CRC over K steps | long `iter`, WCET | fits today | ✅ **MERGED `3b4f123`** -- width-4 Galois x⁴+x+1, `iter Nat@6` (k_max=5, the D≥7 ceiling), B=104 T=14+10k, 3rd park-tax curve; `conformance/0026-lfsr.thse` |
| TP-4 | Sorting network (3-4 elts) | leaf ceiling, alloc, branching | fits today (tight) | ▫ planned |
| TP-5 | S-box cipher round | `MEXCHR`, bus-quiescence, wide state | fits today? (see card) | ▫ planned -- may reveal a lookup-vs-swap gap |
| TP-6 | Binary search (table in mem) | `Trace`+`MEXCHR` body | **drives Trace-with-mem-body** | ⛔ breaks today (customer) |
| TP-7 | Array sum / scan | `Trace`+`MEXCHR` body | **drives Trace-with-mem-body** (simplest) | ⛔ breaks today (customer) |
| TP-8 | Shift-and-add multiply | leaf ceiling (2w product) | **drives M-D.c1 widening / M-D.c2 heap** | ⛔ breaks today (customer) |
| TP-9 | Reversible bytecode interpreter | everything | **drives M-D.c1 + M-D.d2 + Trace-mem-body** | ⛔ capstone (customer) |
| TP-10 | Reversible stack (push/pop) | memory-as-stack | **drives M-D.d2 calling convention** | ⛔ breaks today (customer) |

Status legend: ✅ done · 🎯 next · ▫ planned (fits) · ⛔ breaks today (a milestone customer)
· ❌ overturned (not a valid program).

> **First feedback-loop finding (TP-1 design + review, `docs/scratch/tp1-gcd-design.md`,
> `docs/reviews/tp1-gcd-review.md`).** Designing GCD surfaced a general **sizing law**
> (ADR-0007 **TH-1b**, trajectory disjointness): a `Trace`'s carrier must satisfy
> `|u| ≥ |input|` for any always-iterating loop, and `k_max = |u|+1` is read from *type
> cardinality*. Consequences that reshape this roadmap:
> 1. **No non-trivial variable-`T` loop fits today.** GCD/Collatz break for *any*
>    formulation (carrier ≥ input width ≫ the `MAX_LEAVES_LOOP=3` cap). My proposed
>    LFSR-showcase (TP-1a) isn't even a bijection. Today's `Trace` ceiling is a bimodal toy.
> 2. **The real milestone is a *cluster*, not just spill:** **M-D.e (loop-carrier spill) +
>    widen-PC + a `k_max`-tightening mechanism** (typed/verified iteration bound). Without
>    tightening, a raw `Trace`'s park tax is ~99.8% (dominated by the loose `|u|+1` bound) --
>    meaningless. fib's park tax (16.67%) *is* meaningful only because `iter`/`fold` carry a
>    numeral-typed (tight) iteration count.
> 3. **M-E's park-tax showcase therefore rides on typed iteration bounds** -- a genuine
>    program-level park-tax *histogram* (GCD-style) is gated on the whole cluster. The
>    honest M-E artifact today is fib and the `iter`/`fold` family (tight `k_max`).

> **Second feedback-loop finding (the iter+MEXCHR array-map, `docs/scratch/mdb2-design.md` +
> `docs/reviews/mdb2-*`).** Picked as a *fits-today* win; grounding it in the code first
> (L20) overturned that -- **the compiler emitted zero `MexchReg`.** M-D.b had shipped the
> *machine* capability (the CPU executes register-addressed memory exchange + the `SystemRing`
> closes zero-garbage) but never the *compiler front-half* that emits it. So any program that
> needs data-dependent addressing from `.thse` -- the array-map, **TP-5 S-box**, TP-6/7 --
> was a Tier-3 customer, not fits-today. The milestone that closes it is **M-D.b2** (compiler-
> side MEXCHR emission): **f1** (`da9ca8f`) emits `MexchReg` from a `memexch` prim over a 2-word
> resident array; **f2** (`52704b5`) **composes it under `iter`** -- `arrayMap` maps `g` over the
> first `k` words, **address varying per iteration** (the first real MEXCHR-under-load), priced
> into a **WCET park-tax histogram** (`B=30`, `T(k)=18+4k`, `{40...0}%`, tight numeral `k_max` --
> the histogram this roadmap wanted, via `iter`, no `Trace` needed). **f3** (`87c3f7e`) = the
> XOR/parity **REDUCE** -- `acc ⊕= arr[i]` via the borrow-swap idiom (a non-destructive indexed
> read; the f2 review's "memexch CAN reduce" -- confirmed by a probe + built), histogram `B=24,
> T(k)=12+6k, {50,25,0}%`. The **map+reduce pair** is complete. **Next: f3.5 dot-product**
> (`acc ⊕= key ∧ arr[i]`) needs a **CCNOT recognizer** (Toffoli-into-acc trips
> `has_inner_control`); an **arithmetic** sum needs single-leaf value **width** (→ M-D.c). TP-5's
> lookup-vs-swap subtlety and TP-6/7's Trace-with-mem-body gap remain separate customers.

---

## Tier 1 -- fit the pipeline, best M-E artifacts

### TP-1 -- GCD (subtractive Euclid) 🎯
**Computes (reversible):** carrier `(a, b)`; each step: if `a ≥ b` swap-so-`a<b`... actually
one clean form -- while `b ≠ 0`: `(a,b) ↦ (b, a mod b)`; the come-from witness records the
quotient/branch so the whole `Trace` is a bijection `(a,b) ↦ (gcd, witness)`. Subtractive
variant (`a -= b` when `a≥b`, else swap) keeps each step a small reversible op.
**Stresses:** `Trace` with a genuinely **data-dependent iteration count** -- GCD(13,8) runs
many steps, GCD(12,4) one. Fixed `B` (WCET), swinging `T` → **park tax ~5%-80% across
inputs**. Memory to hold the witness; wide-ish values.
**Why first:** it is the vivid M-E showcase -- the park-tax *histogram* is the headline no
per-step plot can show. Fits today's Trace + memory.
**Acceptance:** `run_on_cpu == eval` over a bounded (a,b) domain; M-E park-tax histogram;
golden energy at a pinned t_ramp.

### TP-2 -- Bounded Collatz
Body: `n even → n/2`, `n odd → 3n+1`, bounded to K steps (finitization). Come-from records
the parity branch. **Stresses** `Trace` + per-input-chaotic `T` → the most extreme park-tax
spread. Great contrast partner to GCD.

### TP-3 -- LFSR / CRC over K steps
`x ↦ (x>>1) ^ (mask · lsb)` -- a linear GF(2) bijection, run K times (`iter`). **Stresses**
long iteration depth + WCET with *minimal* branching → a "long trace, low branching"
energy datapoint (contrast GCD's variable trace). Naturally reversible, so the cleanest
Tier-1 to build.

### TP-4 -- Sorting network (3-4 elements)
A fixed compare-swap network (bitonic/odd-even) with each compare-swap = `CSWAP` gated on a
comparison (= branching). Output `(sorted, permutation-record)` to stay bijective.
**Stresses** the **leaf ceiling** (a k-element list of w-bit values → spill to
scratch+memory), register pressure (`OccSet`), and data-dependent branching -- the three at
once. Tight fit at N=4; a 4-element sort may itself want M-D.c1 widening (a soft customer).

---

## Tier 2 -- the addressing path under real load

### TP-5 -- S-box cipher round
One round: `state ⊕ key → S-box(state) → bit-permutation`. The S-box is a byte permutation
in memory, indexed by the state -- *exactly* `MEXCHR`'s purpose, and no real program uses it
as a table lookup yet.
**The subtlety (likely a finding):** `MEXCHR` **swaps** `reg[rd] ↔ mem[reg[ra]]`; a table
*lookup* `y = S[x]` is not a swap. A reversible lookup needs either (a) the inverse table
resident too, or (b) a read-only indexed-copy primitive we don't have. **TP-5 may reveal
the need for a reversible indexed-lookup construct** -- which would be a genuine new finding,
and precisely the kind of thing this suite exists to surface. **Stresses** `MEXCHR`,
bus-quiescence under repeated addressing, wide state, deep composition.

---

## Tier 3 -- break the pipeline productively (each is a milestone's customer)

### TP-6 -- Binary search over a sorted table in memory
A `Trace` loop doing one `MEXCHR` **each iteration** (read the midpoint), log-depth,
data-dependent. **Breaks today:** M-D.d v1 rejects non-Swapbr... and Trace-with-a-
memory-touching body is explicitly deferred. **Drives:** *Trace-with-a-MEXCHR-body* (the
clean M-D.d follow-on). A beautiful reversible, data-dependent program.

### TP-7 -- Array sum / scan
`Trace` accumulating `mem[i]` across `i`. The **simplest** customer for the same
Trace-with-mem-body gap -- build this *before* TP-6 to isolate the feature.

### TP-8 -- Shift-and-add multiplication
Product of two w-bit numbers is `2w` bits → **blows the leaf ceiling**. **Drives:** M-D.c1
widening (18→22) if it just fits, or M-D.c2 heap if not. Data-dependent `iter` over the
multiplier bits. The clean customer for the *data track*.

### TP-9 -- Reversible bytecode interpreter (capstone)
A fetch-decode-execute `Trace` over a program **in memory**, dispatching on opcodes, with
`MEXCHR` for program+data. "A reversible CPU inside the reversible CPU." **Drives** M-D.c1
(memory > 4 words), M-D.d2 (calling convention for op handlers), and Trace-with-mem-body at
once. If one program justifies the whole remaining roadmap, it is this -- build it last, as
the integration capstone.

### TP-10 -- Reversible stack (push/pop to memory)
Memory-as-stack with a stack pointer register. **Drives** the M-D.d2 calling convention's
stack machinery (if CALL/RET ever needs a stack rather than a link register) -- and is the
minimal probe of whether it does.

---

## Build order & feedback into the main roadmap

**Phase A (now → alongside M-E):** TP-1 GCD as the M-E showcase; TP-3 LFSR as an easy
second energy datapoint. Both fit today; both enrich M-E immediately.

**Phase B (coverage + soft customers):** TP-2 Collatz, TP-4 sort, TP-5 S-box. TP-5 likely
files the first finding (indexed-lookup vs swap); TP-4 may soft-vote for M-D.c1 widening.

**Phase C (name the next milestones):** write TP-7 (array sum) and TP-6 (binary search) →
their rejection *specifies* Trace-with-mem-body; TP-8 → specifies the widening/heap
threshold; TP-10 → tests whether M-D.d2 needs a stack. Each rejection is a committed
finding that updates the main roadmap's priority.

**Phase D (capstone):** TP-9 interpreter, once B/C's milestones land -- the end-to-end proof
that the reversible CPU runs *arbitrary* reversible programs, priced in joules.

**Feedback contract:** each Tier-3 program, when it hits a guard, appends a finding here
(`⛔ → which guard, which milestone`) and, if it changes priority, a one-line note in
[roadmap.md](roadmap.md). When the milestone lands, flip the program to ✅ and add its
golden `run_on_cpu==eval` + energy/park-tax to the regression suite. The two roadmaps
advance in lockstep: the main one adds *capability*, this one adds *proof and pressure*.
