# ADR-0042: Surface theseus → TISC-1 ROM compiler architecture

**Status:** Accepted — **M-A implemented** (`theseus-tisc::compile`,
`tests/compile.rs`): the non-recursive fragment compiles to a TISC-1 ROM that
runs on the CPU, verified `run_on_cpu == eval` over the whole domain, with
out-of-scope constructs cleanly rejected. M-B design is ADR-0043; M-C/M-D/M-E
follow.

> **Revision (advisor review, before M-A implementation).** The first
> draft drew the M-A/M-B line at "any sum type → M-B" and used
> `fibStep = swap ; cnot` as the M-A example — but a controlled operation
> does **not** stay a `Prim`: `synthesize_controlled`
> ([`elaborate.rs:668`](../../../crates/theseus-check/src/elaborate.rs))
> lowers every controlled op to `SwapProd ; Distrib ; Plus(f,g) ; Factor ;
> SwapProd`. So `cnot`/`xor`/`toffoli` — the *substance* of reversible
> computing — all elaborate to `Distrib`/`Plus`/`Factor`, which the draft
> excluded; M-A as first drawn could compile nothing but pure register
> rearrangement, not even its own example. **The correct M-A/M-B line is
> recursion, not sums.** M-A now includes the **controlled-op idiom** —
> the fixed, non-recursive, structurally-reconverging `Distrib ; Plus(f,g)
> ; Factor` shape — recognized *as a whole* and mapped to CPU
> instructions (controlled-NOT → `XOR`, controlled-swap → `CSWAP`),
> WITHOUT implementing general `Plus`/branching. Deferred to M-B+ is only
> what genuinely needs new machinery: general/unrecognized controlled-`f`
> (conditional instruction execution), **recursion** (`Trace`/`Fold`/
> `Unfold`), heap, and **data-dependent `iter` counts** (M-A does
> *constant-count* iteration via static unroll). §2, §4.1, §7 below carry
> this correction.

## 1. Goal and roadmap

**Goal.** A simulated TISC-1 CPU fetches and executes ROMs compiled from
arbitrary well-formed `.thse` programs. "Arbitrary well-formed" means
bounded at unroll depth D — theseus is already bounded (ADR-0007,
ADR-0035); every well-typed program over finite types denotes a bijection
on a finite domain, so the target is finite.

**Milestone decomposition:**

- **M-A — Register-only, non-recursive compiler (this ADR's focus).**
  Compile the product/composition fragment PLUS the **controlled-op
  idiom** (§2) and **constant-count `iter`** to TISC-1 ROMs. Values fit in
  registers; no memory bus, no recursion, no data-dependent control flow.
  Controlled ops (`cnot`/`xor`/`toffoli`) are recognized structurally and
  mapped to `XOR`/`CSWAP` — no general `Plus`/branch codegen. The
  acceptance test is the run-on-CPU differential oracle (§6).

- **M-B — Conditional execution + data-dependent control.** The cases the
  M-A idiom-recognizer rejects: a controlled op whose branch `f`/`g` is
  *not* a recognized single-instruction pattern (needs conditional
  execution of an instruction *sequence* — CSWAP-guarded predication or
  SWAPBR branching, ADR-0034 D5, with Bennett-style uncompute of the
  branch witness), and **data-dependent `iter`** counts (guard each
  unrolled body on a runtime count comparison). Bare (non-idiom) sum
  values also land here.

- **M-C — Memory subsystem.** Wakes Option-B (ADR-0029): LOAD/STORE/
  EXCH-mem via a swap-bus interface. Needed for recursive/heap values
  that exceed the register file.

- **M-D — Full algebraic codegen.** Heap value representation,
  `Fold`/`Unfold`/`Trace`, reversible calling convention. Depends on M-B
  (sum dispatch) and M-C (heap storage).

- **M-E — Polish + optional energy pricing.** Optimization passes,
  energy annotation of compiled ROMs via the char-DB fits
  (`theseus-tisc::energy`).

## 2. The M-A subset

### Included constructors

| Constructor | Source | Why in M-A |
|---|---|---|
| `Id(Type)` | `iso.rs:17` | Identity — zero instructions |
| `Sym(Box<Iso>)` | `iso.rs:19` | Adjoint — reverse + per-instruction inverse |
| `Seq(Box<Iso>, Box<Iso>)` | `iso.rs:21` | Composition — concatenation |
| `Times(Box<Iso>, Box<Iso>)` | `iso.rs:24` | Parallel product — disjoint register regions |
| `SwapProd(Type, Type)` | `iso.rs:41` | Product swap — register exchange |
| `AssocLProd(Type, Type, Type)` | `iso.rs:43` | Product re-association — register rename |
| `AssocRProd(Type, Type, Type)` | `iso.rs:45` | Product re-association — register rename |
| `UniteProd(Type)` | `iso.rs:47` | Drop unit from product — register rename |
| `UnitiProd(Type)` | `iso.rs:49` | Introduce unit into product — register rename |
| `Prim(Box<PrimDef>)` | `iso.rs:107` | Opaque bijection — instruction selection |
| `Iter(Type, Box<Iso>)` | `iso.rs:80` | Bounded iteration — static unroll |

Products nest into tuples of registers; constant-count `Iter` is
statically unrolled; `Prim` maps declared primitives to TISC-1 sequences
via a recognition table.

### Included: the controlled-op idiom

Controlled operations (`cnot`, `xor`, `toffoli`, any clause-defined
bijection where one component predicates a transform on another) do **not**
elaborate to `Prim`. `synthesize_controlled`
([`elaborate.rs:668`](../../../crates/theseus-check/src/elaborate.rs))
emits every controlled op as the fixed shape

```
SwapProd(c,t) ; Distrib(t, cₗ, cᵣ) ; Plus(f, g) ; Factor(t, cₗ, cᵣ) ; SwapProd(t,c)
```

This is **not** general sum computation — it is a *closed, non-recursive,
structurally-reconverging idiom*: `Distrib` splits the target by the
control's tag, `Plus(f,g)` applies one transform per tag, `Factor`
immediately recombines. No spine, no loop, no data-dependent PC change;
the control value is preserved. M-A therefore treats this whole
five-constructor shape as **one recognized macro** (§4.1) and maps it to a
single CPU instruction — it does **not** implement `Distrib`/`Plus`/
`Factor` as general constructors. Recognized cases for M-A:

| Idiom (control `c`, target `t`) | CPU instruction |
|---|---|
| `Plus(Id, Not)` — controlled-NOT | `XOR t, c` |
| `Plus(Id, Swap)` — controlled-swap (Fredkin) | `CSWAP c, t₀, t₁` |

An idiom whose `Plus(f,g)` branch is not one of these single-instruction
patterns is rejected by M-A (→ M-B, conditional execution of a sequence).

### Deferred constructors

| Constructor | Deferred to | Reason |
|---|---|---|
| `Plus(f,g)` outside a recognized controlled idiom, or with non-trivial `f`/`g` | M-B | Needs conditional execution of an instruction *sequence* (CSWAP-guarded predication / SWAPBR) + Bennett uncompute of the branch witness |
| bare `SwapSum`, `AssocLSum`, `AssocRSum`, `UniteSum`, `UnitiSum` (not part of an idiom) | M-B | Tag-bit routing on first-class sum values |
| `Trace` | M-B/M-D | Iteration over the sum spine `u + a ↔ u + b` — data-dependent branching (M-B) + possibly heap for `u` (M-C) |
| `Fold`, `Unfold` | M-D | Structural **recursion** over a list spine `1 + (b × …)` — needs branching (M-B) + heap for spine values exceeding registers (M-C) |
| data-dependent `Iter` (count read from data, varies per input) | M-B | Each unrolled body must be guarded on a runtime count comparison — conditional execution |

**Reasoning — the line is recursion, not sums.** The controlled-op idiom
is finite, non-recursive, and reconverges by construction, so it costs one
instruction (above). What genuinely needs new machinery is: (1)
*conditional execution of a whole instruction sequence* — a controlled-`f`
where `f` isn't a single native op — which requires CSWAP-guarded
predication or SWAPBR branching with witness uncompute (M-B); (2)
*recursion over a spine* — `Trace`/`Fold`/`Unfold` walk a `1 + (b × …)`
structure a data-dependent number of times (M-B for the branch, M-C/M-D
for heap-sized spine values); and (3) *data-dependent iteration counts*,
which are conditional execution in disguise (M-B). M-A is exactly the
*non-recursive, statically-scheduled* fragment.

## 3. Value representation (M-A)

An M-A value is a **product of word-sized leaves**. Each leaf is a
bounded word type:

- `Bit = 1 + 1` — 1 bit, but in the M-A word-subset representation, a
  Bit occupies **one 8-bit register** (zero-extended: `Left(Unit) = 0`,
  `Right(Unit) = 1`). This matches `StateVals` byte semantics
  (`types.rs:113`).
- Bounded `Nat` as an iteration count — encoded as a `u8` numeral value
  (0..=255), occupying one register.
- `u8` (8-bit word) — one register, directly.

A product `a × b` is a tuple: the leaves of `a` occupy consecutive
registers starting at some base, followed by the leaves of `b`. The
**field assignment** is type-directed: flatten the product tree into its
leaf sequence left-to-right; assign registers `r0, r1, …` in order.

**Bound:** The TISC-1 has 8 general registers (`r0`–`r7`,
`types.rs:19`) and 6 usable scratch words (`s2`–`s7`; `s0`/`s1` are
decoder-reserved, `asm.rs:136`). M-A programs are restricted to values
with **at most 8 leaf fields** (fitting in `r0`–`r7`). Scratch words
`s2`–`s7` are available for temporaries within instruction sequences but
are not part of the value representation. Programs needing >8 fields are
a compile-time error in M-A (deferred to M-C with memory).

**Marshalling.** The existing `StateVals::encode`/`decode`
(`types.rs:122–162`) convert between the byte-level `StateVals` struct
and the `Value` inhabiting `state_ty()`, using `value_from_bits` and
`bits_of` from `theseus-netlist`. For the compiler's marshal/unmarshal,
the value type's product structure maps to a subsequence of the
`StateVals` register array: leaf `i` of the flattened product writes/
reads `regs[i]`. All other state fields (PC, BR, IR, scratch) are
initialized to zero for marshalling and ignored (except PC, which is
verified to equal the instruction count after execution).

## 4. Code generation — lowering rules

The compiler translates a core `Iso` (M-A subset) into a `Vec<Instr>`
(TISC-1 instruction sequence). The translation is parameterized by a
**register allocation context** `Ctx` that tracks which registers hold
which product fields.

A context `Ctx` is a mapping from the value type's flattened leaf
positions to register indices. Initially, `Ctx` assigns leaves
left-to-right to `r0, r1, …`. Structural isos (swap, assoc, unite) may
permute this mapping without emitting instructions.

### 4.1. Per-constructor lowering

**`Id(ty)`** — Zero instructions. Context unchanged.

```
lower(Id(ty), ctx) = []
```

**`Seq(f, g)`** — Concatenate the code for `f` and `g`, threading the
context: `f` transforms `ctx` to `ctx'`, then `g` is lowered under
`ctx'`.

```
lower(Seq(f, g), ctx) = lower(f, ctx) ++ lower(g, ctx')
  where ctx' = apply_context(f, ctx)
```

**`Times(f, g)`** — `f : a ↔ b` and `g : c ↔ d`. The context splits
into two disjoint regions: the first `|a|` leaf registers for `f`, the
remaining `|c|` for `g`. Code is concatenated (order does not matter —
they touch disjoint registers).

```
lower(Times(f, g), ctx) =
  let (ctx_f, ctx_g) = split(ctx, num_leaves(a))
  lower(f, ctx_f) ++ lower(g, ctx_g)
```

**`SwapProd(a, b)`** — Exchange the registers holding the `a`-fields
with those holding the `b`-fields. If the context can absorb this as a
rename (permute the context mapping), emit zero instructions. Otherwise,
emit `EXCH` instructions to physically swap the registers.

For the common case where `a` and `b` are each single-leaf (one register
each), this is one `EXCH rd, rs` (`asm.rs:45`).

For multi-leaf products: emit a sequence of `EXCH` instructions,
pairwise swapping corresponding leaves. For `n` leaves per side, this is
`n` EXCH instructions.

```
lower(SwapProd(a, b), ctx) =
  let n = num_leaves(a)  -- == num_leaves(b)
  for i in 0..n:
    emit EXCH { rd: ctx[i], rs: ctx[n + i] }
  update ctx: swap the two field groups
```

**`AssocLProd(a, b, c)` / `AssocRProd(a, b, c)`** — Re-association of
products. This changes the nesting `a × (b × c) ↔ (a × b) × c` but
does not change the flat leaf sequence. **Zero instructions.** The
context mapping is unchanged (the same registers hold the same leaves in
the same order; only the type's tree shape changes).

```
lower(AssocLProd(a, b, c), ctx) = []   -- context type updated, mapping unchanged
lower(AssocRProd(a, b, c), ctx) = []
```

**`UniteProd(a)` / `UnitiProd(a)`** — Drop or introduce a `1` factor.
The unit `1` has zero leaf fields (no register). `UniteProd : 1 × a ↔ a`
strips the unit prefix; `UnitiProd : a ↔ 1 × a` prepends it. **Zero
instructions.** The context type changes but the register mapping is
unchanged (the unit occupies no register).

```
lower(UniteProd(a), ctx) = []   -- type: 1 × a → a; register map unchanged
lower(UnitiProd(a), ctx) = []   -- type: a → 1 × a; register map unchanged
```

**`Sym(f)`** — The adjoint (inverse). Reverse the instruction sequence
of `f` and replace each instruction with its inverse from the table
below.

```
lower(Sym(f), ctx) = reverse(lower(f, ctx')).map(invert)
  where ctx' = apply_context_inverse(f, ctx)
```

**Instruction inverse table** (TISC-1 is reversible; every instruction
has a unique inverse):

| Instruction | Inverse | Notes |
|---|---|---|
| `ADD rd, rs` | `SUB rd, rs` | `rd += rs` ↔ `rd -= rs` |
| `SUB rd, rs` | `ADD rd, rs` | |
| `XOR rd, rs` | `XOR rd, rs` | Self-inverse (XOR is involution) |
| `EXCH rd, rs` | `EXCH rd, rs` | Self-inverse (swap is involution) |
| `ROTL rd, rs` | `ROTR rd, rs` | Left-rotate ↔ right-rotate |
| `ROTR rd, rs` | `ROTL rd, rs` | |
| `CSWAP rd, rs, rt` | `CSWAP rd, rs, rt` | Self-inverse (conditional swap) |
| `XCHG rd, sx` | `XCHG rd, sx` | Self-inverse (register–scratch swap) |
| `SWAPBR rd` | `SWAPBR rd` | Self-inverse (swap is involution) |
| `ADDI rd, imm` | `SUBI rd, imm` | `rd += imm` ↔ `rd -= imm` |
| `SUBI rd, imm` | `ADDI rd, imm` | |
| `XORI rd, imm` | `XORI rd, imm` | Self-inverse |
| `ROTLI rd, imm` | `ROTRI rd, imm` | |
| `ROTRI rd, imm` | `ROTLI rd, imm` | |

**`Prim(def)`** — Instruction selection for opaque primitive bijections.
The `PrimDef` (`prim.rs:71`) carries a clause table (`forward:
BTreeMap<Value, Value>`). The compiler recognizes known patterns and maps
them to TISC-1 instruction sequences:

| Prim pattern | Recognized as | TISC-1 emission |
|---|---|---|
| `addInto : (a, b) ↦ (a, a + b)` (mod 256) | Add-into-second | `ADD { rd: reg_b, rs: reg_a }` |
| `subFrom : (a, b) ↦ (a, b - a)` (mod 256) | Sub-from-second | `SUB { rd: reg_b, rs: reg_a }` |
| `xorInto : (a, b) ↦ (a, a ⊕ b)` | XOR-into-second | `XOR { rd: reg_b, rs: reg_a }` |
| `swap : (a, b) ↦ (b, a)` | Swap | `EXCH { rd: reg_a, rs: reg_b }` |
| `cswap : (c, a, b) ↦ (c, if c&1 then (b,a) else (a,b))` | Conditional swap | `CSWAP { rd: reg_c, rs: reg_a, rt: reg_b }` |
| `rotl : (a, b) ↦ (a, rotl(b, a&7))` | Rotate left | `ROTL { rd: reg_b, rs: reg_a }` |
| `rotr : (a, b) ↦ (a, rotr(b, a&7))` | Rotate right | `ROTR { rd: reg_b, rs: reg_a }` |
| `addi_K : a ↦ a + K` (constant K) | Add immediate | `ADDI { rd: reg_a, imm: K }` |
| `subi_K : a ↦ a - K` (constant K) | Sub immediate | `SUBI { rd: reg_a, imm: K }` |
| `xori_K : a ↦ a ⊕ K` (constant K) | XOR immediate | `XORI { rd: reg_a, imm: K }` |

Recognition is by **clause-table matching**: the compiler checks whether
`def.forward` matches the input/output pattern of a known TISC-1
operation over the prim's concrete type domain (using
`enumerate_values`, `prim.rs:42`). For a `PrimDef` whose clause table
does not match any recognized pattern, the compiler emits an
"unsupported primitive in M-A" error. M-B and beyond may lower
arbitrary small-domain prims via a reversible lookup-table synthesis.

**The controlled-op idiom** — This is where `cnot`/`xor`/`toffoli` and
every other clause-defined controlled bijection actually arrive (they are
NOT `Prim`; see §2). The compiler pattern-matches the whole
`synthesize_controlled` shape as a unit:

```
match:  SwapProd(c,t) ; Distrib(t,cₗ,cᵣ) ; Plus(f,g) ; Factor(t,cₗ,cᵣ) ; SwapProd(t,c)
```

and lowers by the recognized `(f, g)` (matching is *structural on the core
Iso*, robust — not a dense clause-table enumeration):

| `(f, g)` | Meaning | Emission |
|---|---|---|
| `(Id, SwapSum(1,1))` — i.e. `Plus(Id, Not)` | controlled-NOT: `t ⊕= c` | `XOR { rd: reg_t, rs: reg_c }` |
| `(Id, SwapProd(_,_))` | controlled-swap of the two target sub-registers | `CSWAP { rd: reg_c, rs: reg_t0, rt: reg_t1 }` |

An idiom whose `(f,g)` is neither pattern (e.g. a controlled *add* or a
controlled instruction *sequence*) is rejected → **M-B** (conditional
execution of a sequence). Multi-bit controlled ops (e.g. a `u8` XOR that
elaborates to a per-bit CNOT ripple) lower to the corresponding *sequence*
of `XOR`/`CSWAP` instructions — correct, though not yet using a single
native `ADD`/`XOR` word op (that instruction-selection optimization is
M-E). Verified by the §6 oracle.

**`Iter(n_ty, body)`** — Bounded iteration. `iter n f : n × a ↔ n × a`
applies `f : a ↔ a` exactly `k` times, where `k` is the numeral in the
first component.

**M-A handles only *constant-count* iteration** — where `k` is fixed at
compile time (the same for every input, e.g. an `iter` applied at a
statically-known count, or a program whose count component is a literal).
The compiler statically unrolls: `lower(Iter, ctx) = lower(body) ++ … ++
lower(body)` (`k` copies), threading `ctx`. No guards, no branching — a
straight-line body repeated `k` times.

**Data-dependent counts are deferred to M-B.** When `k` is read from
*input data* and varies per input (the general `iter n f` semantics — e.g.
`conformance/programs/0022-fib.thse`'s `iter Nat fibStep` run at different
`k`), each unrolled body must be *guarded* on a runtime comparison `i < k`
— conditional execution of a sequence, which is M-B machinery (CSWAP
predication or a SWAPBR decrement-and-branch loop with come-from
re-convergence, ADR-0034 D5). M-A rejects data-dependent `iter`; the
first end-to-end demo is a **constant-count** program (§7: a fixed
`k`-step Fibonacci compiling to the `fib_cpu.rs` ROM).

### 4.2. Register allocation

M-A register allocation is trivial:

1. **Input layout:** Flatten the input type's product tree into leaves
   left-to-right. Assign leaf `i` to `r[i]`. All M-A programs have
   ≤ 8 leaves (compile-time check).

2. **Structural isos** (`SwapProd`, `AssocLProd`, `AssocRProd`,
   `UniteProd`, `UnitiProd`) permute the logical-to-physical mapping.
   Many are absorbed as renames; `SwapProd` emits `EXCH` only when the
   physical registers must actually swap.

3. **Scratch words** (`s2`–`s7`, via `XCHG rd, sx`, `asm.rs:53`) are
   available for multi-instruction prim sequences that need temporaries.
   In M-A's recognized-prim table (§4.1), all entries are 1–2
   instructions with no temporaries needed.

4. **No spilling.** M-A programs that exceed 8 registers are rejected.

### 4.3. Garbage-free restriction

M-A programs are restricted to the **garbage-free** fragment: every
intermediate value produced by the computation is consumed by a
subsequent operation. No Bennett uncompute is needed. This is
**not** a general property of all `Iso` programs — sum-type dispatch
produces tag-bit garbage that must be reversibly erased. M-A avoids this
by excluding sum types entirely.

The compiler does not emit uncompute sequences. Programs that would
produce garbage are rejected at M-A compile time (they necessarily
involve sum-type constructors, which are outside the M-A subset). Real
uncompute is M-B work.

## 5. Termination

TISC-1 has **no halt instruction**. The execution model for compiled
ROMs:

1. The compiler emits a **static instruction budget** `B` = the total
   number of TISC-1 instructions in the compiled ROM (the length of the
   `Vec<Instr>`).

2. The ring runner (`ring_run.rs`) executes exactly `B` steps
   (revolutions). After `B` steps, the PC has advanced past the last
   instruction.

3. Addresses `≥ rom.len()` fetch a zero word (`step.rs:128–138`): the
   fetch phase produces `IR = 0`, which decodes as `ADDI r0, #0` — the
   identity when `s0 = 0` (which holds by convention). The machine idles:
   all registers are invariant, PC increments by 1 each cycle.

4. The compiled trace length `B` is therefore an **exact budget**: the
   machine performs `B` meaningful steps, then idles. The ring is run for
   exactly `B` revolutions; the final `StateVals` contains the output.

This model requires no halt detection — the runner's revolution count IS
the halting mechanism. For programs with data-dependent iteration counts
(M-B's SWAPBR loops), the budget becomes an upper bound, and the
machine idles for the remaining cycles. The period-2 halt orbit
(ADR-0034 D5) is an alternative for programs that must signal completion,
but M-A's static-unroll programs have known-at-compile-time budgets.

## 6. Run-on-CPU differential oracle

The acceptance test for M-A — to be implemented as an integration test:

```text
compile(program_iso) -> rom: Vec<u16>         // via asm::assemble
marshal(input_value, input_ty) -> StateVals   // leaf i → regs[i], rest zero
run_ring(rom, state, B) -> StateVals          // StepRing::run or step^B via eval
unmarshal(final_state, output_ty) -> Value    // regs[i] → leaf i

FOR EVERY value v in enumerate_values(input_ty):   // prim.rs:42
  let cpu_out   = unmarshal(run_ring(rom, marshal(v), B))
  let eval_out  = theseus_eval::eval(program_iso, v)
  ASSERT cpu_out == eval_out
```

**Whole-domain agreement** over the finite input type constitutes proof
that the CPU correctly executes the compiled `.thse`. This is the same
differential-oracle methodology used throughout the project (ADR-0008):
the interpreter `theseus_eval::eval` (`crates/theseus-eval/src/interp.rs:31`)
is the reference; the compiled ROM on the CPU is the system under test.

The oracle exercises both the compiler (lowering rules + instruction
selection) and the CPU (fetch/decode/execute cycle). A single
disagreement on any input falsifies the compiler.

Implementation note: `enumerate_values` (`prim.rs:42`) enumerates all
inhabitants of a finite type. For word-sized types this is at most 256
values per leaf, and products multiply: `u8 × u8` has 65536 values —
tractable for exhaustive testing. Larger product types may require
sampling (but M-A's ≤ 8 registers × 8-bit words remain feasible for
many practical programs).

## 7. Hand-worked example: Fibonacci step → TISC-1 ROM

> **Note (per the §2 correction).** This walkthrough treats `addInto` as a
> native add prim (→ `ADD`) to show the ROM *shape* against the existing
> `fib_cpu.rs`. In the actual 1-bit `conformance/programs/0022-fib.thse`,
> `addInto = cnot`, which elaborates to the controlled-NOT **idiom**, so
> M-A emits `XOR reg_t, reg_c` (§4.1) — i.e. `fibStep` compiles to
> `EXCH r1,r2 ; XOR r2,r1`, verified below to compute `(a,b) ↦ (b, a⊕b)`.
> A `u8` add would elaborate to a per-bit CNOT ripple and compile to a
> *sequence* of `XOR`/`CSWAP` (native-`ADD` instruction selection is M-E).
> Either way the iterated program below is compiled at a **constant** `k`
> (M-A does not yet handle data-dependent counts).

### 7.1. The surface program

From `conformance/programs/0022-fib.thse`:

```theseus
iso fibStep : Bit * Bit <-> Bit * Bit = swap2 ; addInto
```

where `swap2` is the product swap and `addInto` is CNOT (XOR the first
register into the second). For M-A, we work over `u8 × u8` (8-bit
words) instead of 1-bit, giving the reversible Fibonacci step
`(a, b) ↦ (b, a + b) mod 256` that the TISC-1 already runs
(`tests/fib_cpu.rs`).

### 7.2. Core Iso elaboration

The surface `fibStep = swap2 ; addInto` elaborates to:

```
fibStep = Seq(
  SwapProd(u8, u8),                          -- swap2
  Prim(addInto_def)                          -- addInto: (a, b) ↦ (a, a+b)
)
```

where `addInto_def` is a `PrimDef` with `forward: {(a,b) ↦ (a, (a+b) mod 256)}`
over `u8 × u8`.

The full Fibonacci is `Iter(Nat_D, fibStep)` for some bound D, but for
this example we focus on one step.

### 7.3. Lowering, rule by rule

**Initial context:** `ctx = { leaf 0 → r1, leaf 1 → r2 }` (using `r1`,
`r2` to match the existing `fib_cpu.rs` convention; the general compiler
would use `r0`, `r1`).

**Step 1: lower `Seq(SwapProd, Prim(addInto))`**

Per the `Seq` rule: `lower(SwapProd) ++ lower(Prim(addInto))`, threading
context.

**Step 2: lower `SwapProd(u8, u8)` under ctx = {0→r1, 1→r2}**

Each side is one leaf. Emit:

```
EXCH r1, r2       ;; (a, b) → (b, a)
```

Context after: `{0→r2, 1→r1}` (leaves swapped). But we can also absorb
this as a rename: context becomes `{leaf_a→r2, leaf_b→r1}`, meaning "the
value that was in leaf 0 is now the second output leaf." For the prim
that follows, what matters is the physical register contents: `r1` now
holds `b`, `r2` holds `a`.

Actually, for clarity and to match the existing test, let's track
physical register contents instead:

- Before SwapProd: `r1 = a`, `r2 = b`.
- After `EXCH r1, r2`: `r1 = b`, `r2 = a`.

**Step 3: lower `Prim(addInto_def)` under the post-swap state**

The `addInto` prim has the pattern `(x, y) ↦ (x, x + y)` — recognized
as "add-into-second" from the prim recognition table (§4.1). The
first leaf (physical `r1`) is the source; the second (physical `r2`) is
the target.

Emit:

```
ADD r2, r1         ;; r2 += r1  →  r2 = a + b, r1 = b
```

Wait — the `addInto` pattern is `(x, y) ↦ (x, x + y)`. After the swap,
the pair in registers is `(r1, r2) = (b, a)`. So `addInto` applied to
`(b, a)` gives `(b, b + a) = (b, a + b)`. The target is the second
register:

```
ADD r2, r1         ;; r2 = a + r1 = a + b;  r1 = b  (unchanged)
```

Nope, let me be precise. `ADD rd, rs` means `r[rd] += r[rs]`
(`asm.rs:39`). So `ADD { rd: 2, rs: 1 }` does `r2 += r1`, i.e.,
`r2 = a + b` (since `r2 = a`, `r1 = b` post-swap). Result:
`r1 = b`, `r2 = a + b`. Correct.

### 7.4. The compiled ROM

```
Instruction 0: EXCH r1, r2     →  encode: 0x9940
Instruction 1: ADD  r2, r1     →  encode: 0x8240  (R-format, opcode 0000, rd=2, rs=1)
```

Wait, let me double-check. The existing `fib_cpu.rs:81` uses
`ADD { rd: 1, rs: 2 }` then `EXCH { rd: 1, rs: 2 }` — the *opposite*
order. Let me reconcile.

The existing fib ROM implements `(a, b) ↦ (b, a + b)` as:

```
ADD  r1, r2     ;; r1 += r2  →  r1 = a + b, r2 = b
EXCH r1, r2     ;; swap      →  r1 = b, r2 = a + b
```

Our surface program `swap ; addInto` implements the same function but
in a different decomposition:

```
EXCH r1, r2     ;; swap      →  r1 = b, r2 = a
ADD  r2, r1     ;; r2 += r1  →  r2 = a + b, r1 = b
```

Both produce `(r1, r2) = (b, a + b)` from `(a, b)`. The instruction
sequences differ but compute the same bijection — which is exactly what
the differential oracle (§6) would verify.

### 7.5. ROM encoding

| Address | Instruction | Encoding |
|---|---|---|
| 0 | `EXCH r1, r2` | `0x9940` — R-format: `1 | 0011 | 001 | 010 | 000 | 00` |
| 1 | `ADD r2, r1` | `0x8220` — R-format: `1 | 0000 | 010 | 001 | 000 | 00` |

ROM = `[0x9940, 0x8220]`. Budget `B = 2`.

### 7.6. Execution trace

Seed: `r1 = a`, `r2 = b`, all else zero. PC = 0, BR = 0.

**Cycle 0:** fetch `rom[0] = 0x9940` → decode EXCH r1, r2 → execute
`r1 ↔ r2` → unfetch → `PC += 0 + 1 = 1`.
State: `r1 = b`, `r2 = a`, PC = 1.

**Cycle 1:** fetch `rom[1] = 0x8220` → decode ADD r2, r1 → execute
`r2 += r1` → unfetch → `PC += 0 + 1 = 2`.
State: `r1 = b`, `r2 = a + b`, PC = 2.

Final: `(r1, r2) = (b, a + b)`.

### 7.7. Verification against `theseus_eval`

`theseus_eval::eval(fibStep, (a, b))` computes:
1. `eval(SwapProd, (a, b)) = (b, a)`
2. `eval(Prim(addInto), (b, a)) = (b, b + a) = (b, a + b)`

Result: `(b, a + b)`. Matches the CPU execution. QED.

For the iterated Fibonacci `iter Nat fibStep` with seed `(1, 1)` and
count `k = 10`, the compiler would unroll `fibStep` 10 times, producing
a 20-instruction ROM. This is exactly the ROM in `tests/fib_cpu.rs:79–84`
(with the instruction order `ADD; EXCH` instead of `EXCH; ADD`, but
computing the same sequence). The differential oracle would confirm
agreement on all 256 × 256 = 65536 inputs of type `u8 × u8`.

## 8. Decisions and constraints (M-A)

### Decisions

1. **Register-only.** No memory subsystem — values must fit in 8
   registers. Memory is M-C (wakes Option-B, ADR-0029).

2. **Bounded at depth D.** All types are finite; `Iter` unrolls to the
   type's cardinality bound. Theseus is already bounded (ADR-0007).

3. **Inline all iso-to-iso calls.** No reversible call stack. Every
   sub-iso is inlined at the call site. This is tractable because M-A
   programs are small (register-width values, bounded iteration).

4. **Garbage-free by construction.** M-A's fragment — products, the
   controlled-op idiom (`Distrib ; Plus(f,g) ; Factor` reconverges
   structurally), and constant-count iter — produces no residual garbage,
   so no Bennett uncompute is emitted. Rejected (→ M-B) is exactly what
   *would* leave a witness: conditional execution of an instruction
   sequence, and data-dependent branching.

5. **Constant-count Iter only.** The body is inlined `k` times where `k`
   is fixed at compile time — straight-line, no guards. Data-dependent
   counts (the general `iter` semantics) are M-B (guarded unroll). The
   differential oracle (§6) catches any disagreement.

### Open risks for M-B / M-C / M-D

- **Reversible branch re-convergence (M-B).** CSWAP-then-SWAPBR creates
  a branch; the come-from site must clear the incoming offset and unify
  divergent register states. The come-from discipline (ADR-0034 D5)
  specifies the pattern, but the codegen for clearing the witness and
  unifying post-branch register allocations is new work.

- **Bennett uncompute (M-B).** Sum dispatch consumes a tag bit to choose
  a branch; the tag is garbage that must be reversibly erased. The
  Bennett compute-copy-uncompute pattern (`theseus-netlist`'s
  `lower_iso_bennett`, used by `ring_run.rs:217`) is the model, but
  emitting it as TISC-1 instructions (rather than k-CSWAP cells) is new.

- **Reversible heap allocation (M-C).** Values exceeding register width
  require addressable storage. The Option-B swap-bus (ADR-0029) provides
  the interface (`exch : (Reg, MemCell) <-> (Reg, MemCell)`), but the
  LOAD/STORE/EXCH-mem instructions and the bus protocol are not yet
  implemented.

- **Readout (M-E).** After execution, the register state must be read
  out. In simulation this is trivial (`StateVals::decode`); on physical
  silicon it is a boundary problem (ADR-0041 §5).

## Cross-references

- ADR-0007 — totality: every well-typed term terminates
- ADR-0008 — differential testing methodology
- ADR-0029 — memory: exchange; Option-A/B/C stratification
- ADR-0034 — reversible branching: SWAPBR / come-from
- ADR-0035 — μ-types: bounded unrolling at lowering
- ADR-0036 — iter: bounded-iteration core primitive
- ADR-0041 — execution models: spatial vs temporal
- `crates/theseus-core/src/iso.rs` — the `Iso` enum (codegen source)
- `crates/theseus-core/src/prim.rs` — `Value`, `PrimDef`, `enumerate_values`
- `crates/theseus-tisc/src/asm.rs` — TISC-1 ISA: `Instr`, `encode`/`decode`
- `crates/theseus-tisc/src/types.rs` — `StateVals`, `state_ty()`, field indices
- `crates/theseus-tisc/src/step.rs` — fetch/decode/execute/PC-update cycle
- `crates/theseus-tisc/src/ring_run.rs` — `StepRing::run`, ring recirculation
- `crates/theseus-eval/src/interp.rs` — interpreter (`eval`)
- `conformance/programs/0022-fib.thse` — surface Fibonacci
- `crates/theseus-tisc/tests/fib_cpu.rs` — CPU Fibonacci test (the existing ROM)
