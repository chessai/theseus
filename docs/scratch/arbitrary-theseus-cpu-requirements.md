# Requirements: the surface a CPU must run to execute arbitrary well-formed theseus programs

Grounded by code-read (2026-07-23), all cited. This is the coverage baseline for the redesign.
Companion to `arbitrary-theseus-cpu-design-framing.md` (energy model + axes + principles).

## 1. The value universe is CLOSED and FINITE
Exactly **5 Type constructors** (`theseus-core/src/ty.rs:10-22`): `Zero` (0), `One` (1),
`Sum(a,b)`, `Prod(a,b)`, `Flat(n)` (n∈1..=8, thermometer word, 2^n). `Value` = `Unit | Left | Right
| Pair` (`prim.rs:17-26`); Flat reuses the `Right^k(Left Unit)` thermometer (no distinct byte type).
**Everything is a finite unit/sum/product/Flat tree of statically-known size.** No first-class
recursion / functions / lists / nats — `Nat`/`List` are surface sugar **μ-unrolled to a static depth
D (default 4) BEFORE checking** (`env.rs:101,300`; `Name@D` override). **The CPU always receives a
finite tree of statically-known shape.**

## 2. The Iso language is a CLOSED 22-combinator algebra (`iso.rs:13-108`)
- **Composition:** Id, Sym (inverse), Seq, Plus (⊕), Times (⊗).
- **Sum monoid:** SwapSum, AssocL/RSum, Unite/UnitiSum.
- **Prod monoid:** SwapProd, AssocL/RProd, Unite/UnitiProd.
- **Distributivity:** Distrib, Factor.
- **Loops (the hard cases):** Trace(u,f) [f:u+a↔u+b ⇒ a↔b; come-from, ≤|u|], Iter(n,f) [k off numeral
  n, ≤|n|−1], Fold/Unfold(l,f) [mapAccumL over list-spine, ≤depth−1].
- **Prim(PrimDef):** an **arbitrary admitted (forward,backward) finite-bijection table**
  (`prim.rs:104-120`) — NOT a closed opcode set. Any mutual-inverse table is legal.

## 3. Well-formedness contract (what `check` guarantees the CPU)
The CPU receives a **well-typed reversible bijection `f:a↔b` over a finite tree**, where: every Prim
is an admitted mutual inverse (both directions total, `prim.rs:181-263`); every loop count is
**statically bounded by a type cardinality** (structural totality, `lib.rs:5-7`); iter counter is a
numeral type, fold spine a uniform list-spine; sym is a total involution. An ill-typed combinator or
non-invertible prim is never emitted.

## 4. WCET verdict — YES, statically WCET-able with NO language restriction
Every well-formed core term has a computable static bound: Iter ≤|n|−1, Fold/Unfold ≤depth−1, Trace
≤|u| (finite carrier ⇒ self-terminating come-from), recursion μ-unrolled to static depth. **The
fixed-WCET reversible paradigm CAN run every well-formed program.** The bound can be large (Flat(8)
whole-domain = 256 iters; nested products multiply) but is always decidable. TISC-1's *only* WCET
problem is representational: loop ROM ≤256 & skip immediates ≤255 are u8 fields (`compile.rs:2752,
2990`).

## 5. Coverage-gap ledger — what TISC-1 CANNOT run (the redesign must close ALL)
**Fundamental (shape/algebra of the machine):**
- **Flat(n>8)** — one word = one u8 register; no word wider than a byte (`compile.rs:94-102`).
- **Non-compositional control — THE biggest class:** nested Trace (`:2573` "come-from join does not
  compose"); loop under ⊗ / `Times(Trace,g)` (`:2564`); branch-in-branch / loop-body-with-Swapbr /
  Plus-in-Plus / controlled-in-loop (the `has_inner_control` guard fires in 4 handlers: `:2431,
  2667, 3060, 1912`). **Branching cannot nest inside branching; loops cannot nest or sit under ⊗.**
- **General prims** — TISC-1 is a **RECOGNIZER** (finite pattern set + arm library: 1-op
  Addi/Subi/Xori/Rotli/Rotri, 2-op Add/Sub/Xor/Exch/Rotl/Rotr, 3-op Cswap/Ccnot; `:1425-1434`), NOT
  a general synthesizer. An arbitrary admitted bijection table has **no guaranteed lowering**
  (`:1351 UnrecognizedPrim`). **This is arguably the deepest gap for "arbitrary programs."**
- **Non-identity structural isos** (genuine AssocSum/UniteSum/Distrib/Factor tag reshuffles) deferred
  — only handled when a no-op on the leaf-byte encoding (`:2360-2368, 2384`).
- **Body register permutation** forbidden (Trace `:2674`, Fold `:2970`); controlled untaken arm must
  be identity (`:1860`); WCET/skip u8 overflow (`:2752, 2990`).

**Under-provisioning (raiseable):** MAX_LEAVES=18, MAX_LEAVES_LOOP=4, MEM_WORDS=4, NUM_REGS/SCRATCH=8;
live-input loop spill unsupported (`:2600`, only zero padding leaves spill); non-numeral Iter count.

## 6. Design implications — to run ANY well-formed program the CPU needs
(a) **word width ≥ max Flat(n)** in the program;
(b) **compositional loop/branch machinery** — nested loops, branch-in-branch, loop-under-⊗ must
   compose (this ≡ "runs arbitrary programs" — the closed 22-combinator algebra lowered so EVERY
   combinator composes with ANY sub-iso);
(c) **wider WCET / jump / skip fields** than 8-bit;
(d) **register/scratch/mem capacity** for the largest elaborated leaf count (+ the frame discipline
   from `scratch-frame-gcd-estimate.md`, generalized from one loop body to any nested context);
(e) **general prim lowering** — arbitrary admitted bijection → reversible circuit — rather than a
   finite recognizer (or a universal-gate datapath that lowers any bijection).
None require changing the LANGUAGE — well-formedness already delivers a finite, statically-bounded,
reversible bijection. The redesign is entirely on the machine/compiler side.

## 7. The reframing this suggests
"A CPU that runs arbitrary well-formed theseus programs" = **a compositional, reversible lowering of
a closed 22-combinator finite-bijection algebra over finite trees, with a statically-computed WCET.**
Because the algebra is closed and small, and every combinator is a total bijection, the natural
target may be less "flat-ROM CPU with a recognizer" and more "**reversible abstract machine that
composes the lowering of each combinator with its sub-isos**" (nesting = structural recursion with a
frame discipline; general prims = a universal reversible-gate datapath). The proposals must decide
whether the flat-ROM+PC+fetch+recognizer substrate is even the right one, or whether a
term-structured reversible machine lowers the closed algebra more naturally and more efficiently.
