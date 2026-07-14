# ADR-0042: Surface theseus → TISC-1 ROM compiler architecture

**Status:** Proposed (design accepted-in-principle; M-A implementation is
a follow-on task gated by its run-on-CPU oracle)

## 1. Goal and roadmap

**Goal.** A simulated TISC-1 CPU fetches and executes ROMs compiled from
arbitrary well-formed `.thse` programs. "Arbitrary well-formed" means
bounded at unroll depth D — theseus is already bounded (ADR-0007,
ADR-0035); every well-typed program over finite types denotes a bijection
on a finite domain, so the target is finite.

**Milestone decomposition:**

- **M-A — Register-only word-subset compiler (this ADR's focus).**
  Compile the product/primitive/composition/bounded-iter fragment of core
  `Iso` to TISC-1 ROMs. Values fit in registers; no memory bus, no sum
  types, no heap. The acceptance test is the run-on-CPU differential
  oracle (§6).

- **M-B — General sum-branching + ancilla/Bennett uncompute.** Add
  `Plus`, all sum-structural isos (`SwapSum`, `AssocLSum`, `AssocRSum`,
  `UniteSum`, `UnitiSum`), `Distrib`, `Factor`. Requires reversible
  branching via CSWAP-then-SWAPBR (ADR-0034 D5) and Bennett-style
  uncomputation of garbage bits produced by branch dispatch.

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

These are exactly the constructors whose types are products of word-sized
leaves (no sums). Products nest into tuples of registers; `Iter` counts
are read from a register and statically unrolled to depth D; `Prim` maps
to TISC-1 instruction sequences via a recognition table.

### Deferred constructors

| Constructor | Deferred to | Reason |
|---|---|---|
| `Plus` | M-B | Sum-type dispatch requires reversible branching (CSWAP + SWAPBR, ADR-0034) and garbage uncompute |
| `SwapSum`, `AssocLSum`, `AssocRSum`, `UniteSum`, `UnitiSum` | M-B | Structural sum isos — same: need tag-bit routing + branching |
| `Distrib`, `Factor` | M-B | Sum–product distributivity — produces/consumes sum tags |
| `Trace` | M-D | Iteration over the sum spine via `u + a ↔ u + b` — needs sum dispatch (M-B) + potentially heap (M-C) |
| `Fold`, `Unfold` | M-D | Structural recursion over list spines — needs sum dispatch (M-B, for the `1 + (b × …)` spine) + heap (M-C, for spine values exceeding registers) |

**Reasoning per group:**
- *Sum ops (Plus, SwapSum, Assoc, Unite, Distrib, Factor):* Sum types are
  `a + b` — tagged unions. Routing on a tag bit requires conditional
  execution (CSWAP gates a swap on a predicate register, then SWAPBR
  branches). The branch produces garbage (the tag bit is consumed but the
  predicate register retains information); reversible erasure requires
  Bennett uncompute or ancilla discipline. This is genuinely new
  machinery — M-B's scope.
- *Trace:* `trace f : a ↔ b` where `f : u + a ↔ u + b` iterates `f`
  until the output exits on the `b` side. This is sum-dispatched iteration
  — needs M-B's branching machinery, and potentially heap storage for the
  loop state `u` if it exceeds register width.
- *Fold/Unfold:* These traverse a finite list spine `1 + (b × (1 + (b ×
  …)))`, applying the body at each element. The spine is a sum-type
  structure (the `1 + …` nesting), so dispatch requires M-B. The spine
  values may exceed register capacity, requiring M-C.

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

**`Iter(n_ty, body)`** — Bounded iteration. `iter n f : n × a ↔ n × a`
applies `f : a ↔ a` exactly `k` times, where `k` is read from the
first component (a unary numeral of type `n`).

**M-A strategy: static unroll to depth D.** The numeral type `n` is
finite with `|n|` inhabitants (0 through `|n|-1`). The compiler emits
code that handles each possible count value via **complete unrolling**:
the body is inlined `|n|-1` times (the maximum count), with each
application guarded by a count check.

Concretely, for `Iter` with maximum count `D = |n| - 1`:

1. The count register `r_k` holds the iteration count as a byte.
2. For each iteration `i` from `0` to `D-1`:
   - Emit a guard: `CSWAP { rd: r_k, rs: r_flag, rt: r_flag' }` where
     `r_flag` enables/disables the body. (This is M-B machinery —
     see the simplification below.)

**M-A simplification.** Since M-A restricts to the garbage-free
product/prim fragment, and `Iter` is only included for *small, known*
bounds, the M-A implementation uses **unconditional static unroll**:
the compiler unrolls the body exactly `D` times. The iteration count `k`
is carried through but the body executes all `D` times regardless. This
is correct ONLY when the program is structured so that the count is
always the maximum (i.e., the iteration bound equals the actual runtime
count for every input). Programs where `k < D` for some inputs require
conditional execution (CSWAP-based guarding, deferred to M-B).

For the word-subset programs that M-A targets — notably the Fibonacci
step `iter Nat fibStep` compiled with a fixed small `Nat` type —
the count is typically fixed or the program is written so that
every input exercises the same count. The compiler checks this
(whole-domain verification via the differential oracle, §6) and
rejects programs where unconditional unroll would disagree with the
interpreter.

**Alternative considered: SWAPBR loop.** A SWAPBR-driven loop
(ADR-0034) would emit the body once and loop `k` times using
a decrement-and-branch pattern:

```text
  LOOP: [body instructions]
        SUBI r_count, 1
        ;; if r_count != 0, branch back
        CSWAP r_count, r_offset, r_zero  ;; swap offset in if nonzero
        SWAPBR r_offset
        ;; come-from at LOOP
```

This is more efficient for large `D` but requires reversible branch
re-convergence (the come-from at the loop entry must clear the incoming
offset — ADR-0034 D5) and conditional uncompute of the loop control.
**Deferred to M-B**, where sum-branching machinery exists. For M-A's
small, fully-unrolled cases, static unroll is simpler and correct.

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

4. **Garbage-free restriction.** M-A programs are restricted to the
   product/prim/iter fragment where no garbage bits are produced. No
   Bennett uncompute is emitted. Programs involving sum types (which
   produce tag-bit garbage) are rejected.

5. **Static unroll for Iter.** The iteration body is inlined `D` times
   (unconditional). Correct only when every input exercises the maximum
   count, or when conditional guarding is not needed. The differential
   oracle (§6) catches any disagreement.

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
