# ADR-0043: M-B — reversible-branching codegen (conditional execution)

**Status:** Accepted — **M-B implemented** (`theseus-tisc::compile`, merged at
`d722481`). Authored by the advisor after two delegated drafting attempts were
write-blocked; the design below is the reviewed plan, realized as specified.

**Implementation note (what shipped).** The design here is the codegen that
landed: guarded static unroll for data-dependent `iter` (§3), the SWAPBR
skip-block conditional primitive (`emit_skip_block`, §2), tag+payload sum
representation (§4), and general controlled ops with multi-instruction arms
(§3). Verified by the run-on-CPU differential oracle (§5) over the whole finite
domain for the real `iter Nat fibStep` (`0022-fib.thse` → a 24-instruction ROM
computing Fibonacci, 16 inputs), a general controlled op, and a bare-sum
program (`swapMaybe`) — all `== theseus_eval::eval`.

**Acceptance authority (a review correction).** The authority is
`compile::run_on_cpu`, which runs the ROM on the **real `step` core-iso CPU**
we energy-account. A first cut wired the oracle to a faster hand-written
byte-level simulator (`run_on_cpu_fast`); that only proves the compiler agrees
with a second interpreter by the same author (cf. L15), not the real machine.
The fast simulator is retained as a *pinned convenience* — routine tests assert
`fast == eval`; authoritative `#[ignore]`d `*_real_cpu` tests assert
`real == eval` (run pre-merge via `--ignored`, ~30 min because each step-iso
evaluation of the 160-bit machine costs seconds). Do not treat a green
`run_on_cpu_fast` as acceptance on its own.

## 1. Where M-B sits

M-A ([ADR-0042](0042-thse-to-tisc-compiler.md)) compiles the **non-recursive,
statically-scheduled** fragment of core `Iso` to a TISC-1 ROM: products,
composition, adjoint, the controlled-op *idiom* recognized as a single
instruction (`XOR`/`CSWAP`), and **constant-count** `iter`. It cleanly *rejects*
everything that needs runtime control flow — `compile.rs` returns
`CompileError::UnsupportedSum` and `CompileError::DataDependentIter`.

**M-B is exactly those rejections** — the point where the machine must *decide at
runtime what to execute*:

1. **General controlled ops** — the `Distrib;Plus(f,g);Factor` idiom where `f`/`g`
   are arbitrary instruction *sequences*, not M-A's single-instruction patterns.
2. **Data-dependent `iter`** — the count `k` is read from data and varies per
   input (so the *real* `iter Nat fibStep`, hence `conformance/programs/0022-fib.thse`,
   compiles — the headline payoff).
3. **Bare sums** — first-class `a + b` values (tag + payload): `SwapSum`,
   `AssocL/RSum`, `UniteSum`, and `Plus`/`Distrib`/`Factor` outside a recognized
   idiom.

Still **register-only and bounded**. Recursion (`Trace`/`Fold`/`Unfold`) and
heap-sized values stay deferred to M-C (memory bus, [ADR-0029](0029-memory.md))
and M-D.

## 2. The core primitive: reversible conditional execution

Everything in M-B reduces to one primitive: **run instruction sequence `code(f)`
iff a control bit `c` is 0, `code(g)` iff `c` is 1, reversibly, and reconverge.**
TISC-1's substrate for this is the offset-BR / come-from discipline
([ADR-0034](0034-reversible-branching.md)): `SWAPBR rd` swaps `BR ↔ r[rd]`, and
every step ends with `PC += BR + 1`. So writing a nonzero offset into `BR` makes
the next step jump; a *paired* `SWAPBR` at the target (the **come-from**) swaps it
back out, clearing `BR` reversibly. A branch and its come-from are paired
addresses — that pairing is what makes the jump invertible.

### 2.1 The bootstrap: controlled constant offset

A *data-dependent* branch needs the jump distance computed from `c`. The distances
are **compile-time constants** (block lengths), so the only runtime-conditional
step is *"load constant `K` into a scratch register iff `c`"*. That is a fixed,
finite macro — call it `emit_ctrl_const(rc_bit, rscratch, K)` — synthesized once
from `Toffoli`/`CSWAP` over the known bits of `K` (for each set bit `j` of `K`,
one `Toffoli(rc_bit, 1, rscratch.bit_j)` — a controlled bit-set). It does **not**
go through the general `Distrib;Plus;Factor` path (that would be circular); it is
a primitive codegen macro. This macro is the whole bootstrap — with it, arbitrary
reversible branching follows.

### 2.2 The branch pattern

For control bit in register `rc`, arms `code(f)` (length `Lf`) and `code(g)`
(length `Lg`):

```text
        emit_ctrl_const(rc, rb, Lf+1)   ; rb = c ? (Lf+1) : 0   (skip f-block iff c=1)
        SWAPBR rb                        ; BR ← rb   (rb ← 0, the old BR)
        ; end-of-step PC += BR + 1: c=0 → fall into f; c=1 → jump to g
 f:     <code(f)>                        ; runs iff c=0
        emit_const(rb2, Lg+1) ; SWAPBR rb2   ; unconditionally skip g-block
 g:     <code(g)>                        ; runs iff c=1
 merge: <paired come-from>               ; both arms arrive; BR restored to 0
                                         ;   using the PRESERVED control (§2.3)
```

For a **single guarded op** (the common case — a controlled op whose active arm is
one instruction), prefer **CSWAP-predication** over a PC branch: route the target
through a work register conditioned on `c` (so the op sees a dummy when
unselected), apply it, route back — no PC change, no come-from. The compiler picks
CSWAP-predication for a 1-instruction arm and the SWAPBR branch for a sequence.

### 2.3 Why the merge is clean (the key correctness point)

The `Distrib;Plus(f,g);Factor` idiom **preserves the control**: `Distrib` splits
the target by `c`, `Plus` transforms each side, `Factor` recombines with `c`
intact. So the branch's "which arm did we take" witness *is* the still-live control
bit — the come-from reconverges by consuming `c` exactly as `Factor` prescribes,
with **no extra witness to erase**. This is the crucial difference from a general
irreversible branch. What *does* need cleanup is any **internal ancilla/temporary
produced inside `code(f)` or `code(g)`**: each arm computes on scratch and must
**Bennett-uncompute** (compute → use → run the arm's inverse on the scratch)
before the merge, so scratch returns to 0 on both paths. This is register/scratch-
bounded in M-B (heap uncompute is M-C).

## 3. Lowering rules (extending `compile.rs`)

M-B extends the existing recursion in `theseus-tisc::compile` at the two seams
where M-A returns errors.

**General controlled op** — replaces `match_controlled_idiom`'s "reject unless
`(f,g)` is XOR/CSWAP". When `(f,g)` are arbitrary: recursively `compile` each arm
to `code(f)`/`code(g)` (under a scratch-isolated `Ctx`), wrap them in the §2.2
branch keyed on the control leaf, insert per-arm Bennett uncompute (§2.3), emit the
come-from. Single-instruction arms take the CSWAP-predication fast path (M-A's
existing rule is the degenerate case).

**Data-dependent `iter`** (`iter n f`, count `k` in data) — **guarded static
unroll** to depth `D = |n|`: emit the body `D` times; guard body `i` on the
predicate `p_i = (i < k)` via the §2 conditional primitive (compute `p_i` from the
count register, conditionally apply `code(f)`, uncompute `p_i`). Chosen over a
SWAPBR decrement-loop (smaller code but needs reversible loop-carried BR + a
come-from at the loop head) because unroll is bounded and simpler; the loop form is
noted as an M-E size optimization. This is the rule that makes `0022-fib.thse`'s
`fib = iter Nat fibStep` compile.

**Bare sums** — a value `a + b` occupies a **tag register** (0 = `Left`, 1 =
`Right`) plus the payload in `max(|a|,|b|)` leaf registers (§4). `SwapSum` = flip
the tag (`XORI rtag, 1`). `Distrib`/`Factor` are the (tag,payload) ↔ branched-form
conversions the branch pattern already assumes; `AssocSum`/`UniteSum` are tag
renamings. Bare `Plus(f,g)` on a sum value is the §2 branch keyed on the tag.

## 4. Sum-value register representation

`a + b` → one `rtag` (a `Bit`) + the payload sharing `max(|a|,|b|)` leaf registers
(the wider arm's layout; the narrower arm zero-extends). This keeps sums
register-resident and bounded, at the cost of `|a|+|b|` not being packed — fine for
M-B's bounded programs; packing/heap is M-C/M-D. `num_leaves` in `compile.rs`
extends to `Sum(a,b) → 1 + max(num_leaves a, num_leaves b)`.

## 5. Acceptance: the run-on-CPU differential oracle

Same methodology as M-A ([ADR-0008](0008-differential-testing.md)), extended to
M-B programs. For each, over the whole finite input domain (`enumerate_values`):
`run_on_cpu(iso, v) == theseus_eval::eval(iso, v)`.

Required M-B programs: (a) the **real** `iter Nat fibStep` from `0022-fib.thse` at
*varying* `k` (the M-A blocker) → the Fibonacci sequence on the CPU; (b) a **general
controlled op** whose arm is a multi-instruction sequence (e.g. a controlled add);
(c) a **bare-sum** program (e.g. `swapMaybe`). A single disagreeing input
falsifies the codegen. The parts flagged risky above — the come-from BR-clear, the
guarded-unroll predicate, per-arm uncompute — are precisely what the whole-domain
oracle pins.

## 6. Hand-worked sketch: `iter Nat fibStep` (data-dependent)

`fib = iter Nat fibStep`, input `⟨k, (a,b)⟩` with `k` a `Nat` numeral in a register.
Guarded unroll to `D = |Nat|`: for `i = 0..D-1`, emit `p_i = (i < k)` (compare the
constant `i` against the count register → a bit in scratch), then the §2 conditional
apply of `code(fibStep)` (M-A already compiles `fibStep` to `EXCH; XOR`) guarded on
`p_i`, then uncompute `p_i`. After `D` guarded steps the registers hold
`fibStep^k(a,b) = (fib_k, fib_{k+1})` and `k` is preserved. The oracle checks this
equals `eval(fib, ⟨k,(a,b)⟩)` for every `k` in the domain — i.e. the compiled ROM
computes Fibonacci on the CPU.

## 7. Open risks for M-C / M-D

- **Come-from placement / BR arithmetic.** The offset constants must account for
  the `emit_ctrl_const` prologue lengths themselves; the implementer computes block
  lengths after arm codegen (two-pass) and the oracle catches off-by-one.
- **Nested branches / deep uncompute.** Scratch pressure grows with nesting depth;
  M-B bounds it to the register/scratch file and errors (`TooManyLeaves`-style) if
  exceeded — the honest boundary before M-C's heap.
- **Recursion (M-C/M-D).** `Trace`/`Fold`/`Unfold` add a data-dependent *number of
  branch iterations over a spine*, needing heap-resident loop state — out of scope
  here.

## Cross-references

- [ADR-0042](0042-thse-to-tisc-compiler.md) — the compiler architecture + M-A (the
  fragment M-B extends); `theseus-tisc::compile`, `CompileError`, `match_controlled_idiom`.
- [ADR-0034](0034-reversible-branching.md) — SWAPBR / offset-BR / come-from (the substrate).
- [ADR-0029](0029-memory.md) — the memory bus M-C wakes.
- [ADR-0008](0008-differential-testing.md) — the differential-oracle norm.
