# M-D design — arbitrary `.thse` compiles (heap + recursion + calling convention)

Status: **DESIGN — reviewed (Fable adversarial pass), fixes folded.** Verdict was
SHIP-WITH-FIXES: the finitization-boundedness backbone is SOUND for **Fold/Unfold**
(structural bound, body-invariant guards, adjoint-correct) but **BROKEN for Trace** —
Trace's bound is carrier *cardinality* (exponential), and guarded-static-unroll is
instruction-level broken for it (`emit_skip_block`'s CSWAP cleanup needs a body-invariant
guard; Trace's guard is the exit tag the body flips). So **Trace is removed from M-D.a and
re-homed to a reversible while-loop increment** (§4 M-D.d), and two missing prerequisites
are added (halting-model/WCET harness change; MEM_WORDS widening). See §4 (revised) and the
"Review outcome" note below.
Canonical branch `chessai/theseus-v2`, tip `c348c9a`
(Stage 2 + type-system pass done). This is the design pass for M-D (ADR-0042); it does
NOT implement. Grounded in a full read of the current compiler boundaries (file:line
cited inline). Output: the reframe, a layered decomposition into delegable increments,
the reversibility obligations, and the open questions the adversarial review must probe.

## 1. The gap (what fails to compile today)

The ROM compiler (`theseus-tisc/src/compile.rs`) defers exactly the recursion/heap
constructs:
- **`Trace` → `Deferred`** (compile.rs:1254): unbounded data-dependent fixpoint (the
  interpreter loops until the body exits on the `Right` side, `interp.rs:163-183`).
- **`Fold`/`Unfold` → `Deferred`** (compile.rs:1256-1258): structural recursion over a
  list-spine type (interp runs both, `interp.rs:207-267`).
- **non-numeral `iter` → `Deferred`** (compile.rs:1213-1215).
- **leaf ceiling** `MAX_LEAVES = 14 + MEM_WORDS` (compile.rs:180-185): values whose
  flattened leaf count exceeds this → `TooManyLeaves`. There is **no heap / pointer /
  growable value** — `Type = Zero|One|Sum|Prod` only (`theseus-core/ty.rs`).
- **compile-time-fixed addressing**: `Instr::Mexch{rd, addr:u8}` bakes `addr` into the
  ROM word (asm.rs:114-116, 208-213); no path from register data → `BUS_ADDR`. ADR-0046
  §7 defers "a base register / data-dependent addressing" to M-D.
- **no CALL/RET/stack**: ADR-0034 D5 *sketches* CALL/RET as SWAPBR macros but they are
  **unimplemented**; State has no SP/link/frame field.

## 2. The reframe — finitization makes recursion BOUNDED (the load-bearing insight)

theseus finitizes recursive types by a per-path unroll budget `D` (ADR-0035, default 4):
a `Nat`/`List`/tree becomes a finite `Sum`/`Prod` chain the compiler already sees. M-B
**already compiles bounded `iter`** by *guarded static unroll*: emit the body `|n|−1`
times, each copy `i` gated by a SWAPBR skip-block on counter-tag `i`
(`lower_data_dependent_iter`, compile.rs:1272-1312; `emit_skip_block`, compile.rs:546-622).
The real `fib` (`iter Nat fibStep`) compiles this way → a 24-instr ROM, oracle-verified
(compile.rs:209-224).

**Claim (the design's backbone), as CORRECTED by review:** after finitization,
**`Fold`/`Unfold`** are bounded by the finitized *spine depth* (a small compile-time
constant, D≈4) and compile by the SAME guarded-static-unroll machinery M-B uses for
`iter` (guard copy `i` on cons-tag `i`; tags are body-invariant so the CSWAP sandwich is
valid; the accumulator is threaded *data*, not an ancilla; `adjoint(Fold)=Unfold(f†)`,
adjoint.rs:102-103, is preserved). **`Trace` is NOT in this family** — its trip count is
carrier *cardinality* `|u|` (ADR-0007 pigeonhole: injective body ⇒ no repeat ⇒ ≤|u|
steps), which is exponential in carrier width, and — decisively — `emit_skip_block`'s
CSWAP cleanup (compile.rs:546-622) requires a **body-invariant** guard, whereas Trace's
only guard is the sum tag the body *flips* at the exit iteration, so the aux uncompute is
corrupted even at tiny `|u|`. Trace therefore needs a **reversible while-loop** (backward
SWAPBR from a bottom test, come-from paired — different machinery, kin to CALL/RET). This
splits M-D as:

- **CONTROL-a (extends M-B): `Fold`/`Unfold` bounded guarded unroll**, for list-shaped
  values fitting the `14+N` leaf ceiling — no new ISA, no heap, no dynamic addressing.
  NOTE (review R2): unlike `iter` (which compiles its body ONCE against a fixed register
  slice), `Fold`'s copy `i` acts on element `i` at a *different* leaf offset — so the
  body is lowered **per copy under a displaced `Ctx`** (an extension of
  `lower_data_dependent_iter`, NOT verbatim reuse).
- **CONTROL-loop: `Trace` (and surface labels, which elaborate to Trace —
  elaborate.rs:1065) as a reversible while-loop**, sharing come-from machinery with the
  calling convention. Requires the halting-model change (below).
- **DATA: heap value representation + data-dependent addressing** for values exceeding
  `14+N` leaves — REQUIRED for *tree*-shaped recursion (a `Tree Bit @4` is ~30 leaves,
  review R3), and needs `MEM_WORDS` widening.

**Scope honesty (review R3/R5):** M-D.a (Fold/Unfold, no heap) is non-vacuous — it ships
on **list** folds/unfolds (`List Bit @4` = 8 leaves, fold = 9; `List (Bit×Bit) @4` ≈
12-14, fits 18). It does NOT cover **trees** (M-D.c) or **labels/Trace** (M-D.d). And
inline unrolling yields **depth-4, single-level** recursion within the 256-instr PC cap;
two-level nesting `(D−1)²=9` copies (~200-350 instrs) or an `@8` depth override overflows
it — so the calling convention is NOT a general "just unroll deeper" escape and is
co-scheduled with Trace (both need the halting-model change), NOT deferred to M-E.

## Review outcome — the five folded fixes
1. Trace out of M-D.a → its own reversible-while-loop increment (M-D.d), co-scheduled
   with the calling convention (shared come-from machinery).
2. **Halting-model prerequisite (new, M-D.w):** `run_on_cpu` runs the CPU for exactly
   `rom.len()` steps (compile.rs:1438-1464) — valid only for monotone PC. Trace-as-loop
   and CALL/RET both do backward jumps → need a **WCET budget + park-at-end halt
   convention** (and the WCET-is-totality ledger, ADR-0007 TH-1, must report `|u|·|body|`
   for loop-based trace). Prerequisite for any non-monotone-PC construct.
3. **MEM_WORDS widening (new, M-D.b/c prerequisite):** `MEM_WORDS = 4` (types.rs:36) is a
   4-word heap; a depth-4 tree needs ~30 leaves. Widening touches the decoder select tree
   (power-of-two `addr_bits`), `COMPOSITE_BITS`/`SYSTEM_BITS`, and **re-establishing the
   Stage-1b bus-quiescence certificates at the new width** — a real, provable-cost item.
4. §2 corrected to its true scope (above): depth-4, single-level, list-shaped.
5. M-D.a spec: "per-copy body lowering under a displaced `Ctx`", not verbatim reuse.
Plus (R4): a new **alias obligation** for data-dependent addressing — the address-source
register must not be the data register of the same MEXCH/LOAD transaction (new alias class
in compile.rs + asm.rs).

## 3. Reversibility obligations (every increment must preserve these)

From ADR-0007 / theory/reversible-memory-closure.md / ADR-0044/0046:
- **Totality / bijection (ADR-0007):** descent + return together must be a total
  bijection on State. Acceptance = the differential-oracle norm (L19): `run_on_cpu`
  (real `step`-iso CPU) `== theseus_eval::eval` over the whole finite domain — the
  `*_real_cpu` tests, NOT `run_on_cpu_fast`.
- **Zero-conservation (Lemma 7.1/7.2):** no iso manufactures a zero; zeros originate
  only at marshalling (`marshal`, compile.rs:1391-1433). Heap alloc/dealloc must budget
  zeros from the marshalled supply — allocation depth is bounded by available zero cells.
- **Zero-parking discipline (ADR-0044 §2.3-2.4):** every scratch/aux/heap cell is 0 on
  entry and restored to 0 on exit; page-in must page-out (`with_registers`,
  compile.rs:250-314).
- **Bus-quiescence (Thm 3.5):** any runtime address must flow through the bus within one
  emit/mem/absorb transaction and leave `BusMsg == 0` at the revolution seam
  (decoder.rs:965-1105). This is what makes *data-dependent addressing* the delicate part.
- **No aliasing:** aliased operands are non-injective and rejected at both iso and asm
  levels (compile.rs `AliasedRegisters`, asm.rs:149-190). Heap references must be proven
  non-aliasing per frame.

## 4. Decomposition (delegable increments — REVISED per review)

- **M-D.a — `Fold`/`Unfold` via guarded static unroll (LIST-shaped, no heap).** Extend
  `lower_data_dependent_iter` to the spine constructs: emit the body up to the finitized
  spine depth, guard copy `i` on cons-tag `i`, threading the accumulator as data. **Body
  lowered per-copy under a displaced `Ctx`** (copy `i` acts on element `i` at a distinct
  leaf offset — NOT the single-body reuse `iter` gets). Values fit `14+N` leaves (lists,
  not trees). Acceptance: `fold`/`unfold` over small finitized *list* values compiles →
  ROM → `run_on_cpu == eval` (whole-domain); `bare_fold_rejected`/`bare_unfold` negative
  tests flip to *accepted*; `bare_trace` STAYS rejected; adjoint `fold†=unfold f†`
  checked. **This is the delegatable prerequisite** — proves recursion codegen decoupled
  from the data problem, and it is non-vacuous (list folds fit; §2 R3).
- **M-D.w — halting-model / WCET harness change (prerequisite for any backward jump).**
  Replace `run_on_cpu`'s fixed `rom.len()` step budget (compile.rs:1438-1464) with a WCET
  budget + park-at-end halt convention, so non-monotone-PC programs (Trace-loop, CALL/RET)
  can be oracle-verified. Extend the ADR-0007 WCET ledger to report `|u|·|body|` for
  loop-based trace. Small, self-contained, but blocks M-D.d.
- **M-D.b — data-dependent addressing (LOAD/STORE by register) + MEM_WORDS widening.**
  A new ISA form whose address is sourced from a *register* value routed into `BUS_ADDR`
  (vs the fixed `IR.rt` cross_cnot copies, decoder.rs:1004-1015). The select tree is
  ALREADY `BUS_ADDR`-keyed (decoder.rs:277-307, 922-952) — the new work is the address
  *marshalling* + its bus-quiescence proof + a **new alias class** (address-source reg ≠
  data reg of the same transaction, in compile.rs + asm.rs). Bundles **`MEM_WORDS`
  widening** (types.rs:36; needed for any non-trivial heap) — which re-touches the select
  tree's power-of-two `addr_bits`, `COMPOSITE_BITS`/`SYSTEM_BITS`, and **re-establishes
  the Stage-1b bus-quiescence certificate at the new width**. Acceptance: store/load at a
  computed index, oracle-verified; §6 line-audit + bus-quiescence hold at the new width.
- **M-D.c — heap value representation (trees + large values).** Finitized values
  exceeding `14+N` leaves (e.g. `Tree Bit @4` ≈ 30 leaves) as a spine/tree of memory
  cells addressed via M-D.b, with marshalling respecting zero-conservation (alloc from
  zero, dealloc back to zero; allocation depth bounded by the marshalled zero budget).
  Re-target M-D.a's `Fold`/`Unfold` to walk heap-resident spines; add tree recursion.
- **M-D.d — `Trace` (reversible while-loop) + reversible calling convention (together).**
  Trace and surface *labels* (elaborate.rs:1065) as a reversible while-loop: come-from
  paired backward SWAPBR from a bottom exit-test (NOT unroll). The SAME come-from
  machinery implements ADR-0034 D5's CALL/RET (callee `SWAPBR rl` captures the return
  offset; return recomputes the negated offset) + the pairing checker. Requires M-D.w.
  NOT M-E-adjacent — it is what makes labels + deep/nested recursion compile.

**Sequencing:** **M-D.a first** (list Fold/Unfold — low risk, proves recursion codegen,
independently shippable). Then two loosely-independent tracks: the **data track**
(M-D.b → M-D.c: addressing+widening → heap/trees) and the **control track** (M-D.w →
M-D.d: halting model → Trace/labels + calling convention). Each is one delegation with
the full discipline (design → adversarial review → scoped delegation → verify → merge)
and its own whole-domain oracle. Do NOT bundle a track into one task.

## 5. Residual risks for the per-increment reviews (design-level risks now resolved)
The backbone risks R1-R5 are resolved above. Each increment still gets its own design +
adversarial review; the load-bearing residuals to carry forward:
- **M-D.a:** the per-copy displaced-`Ctx` lowering must keep the fold/unfold *adjoint*
  exact and zero-clean across all list lengths ≤ D (sample the length-boundary states).
- **M-D.b:** the register→`BUS_ADDR` marshalling must not disturb bus-quiescence; the
  MEM_WORDS-widened Stage-1b certificate must be re-run whole-domain, and the alias class
  must be enforced (negative test per L20).
- **M-D.c:** zero-conservation for heap alloc/dealloc — prove no path manufactures a zero
  (Lemma 7.1) and every heap cell returns to 0 on dealloc.
- **M-D.d:** the come-from pairing must make the whole (call/descend + return/ascend) a
  total bijection; the WCET budget must be a proven upper bound; verify with the new
  halting-model oracle, not the monotone-PC one.

## 6. Next action
Delegate **M-D.a** (list `Fold`/`Unfold` guarded unroll, per-copy displaced-`Ctx`) as the
first increment — smallest, independently shippable, proves the recursion codegen. Its own
design-detail + adversarial review precede delegation (per the residual risks in §5). The
data track (M-D.b/c) and control track (M-D.w/d) follow, each with the full discipline.
