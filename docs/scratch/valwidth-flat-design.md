# Value-width — `Type::Flat(n)` (a width-parameterized single-leaf word)

> **Revision r2 — incorporates the fresh-context Fable design review**
> (`docs/reviews/valwidth-flat-design-review.md`), which BUILT `Type::Flat` in a throwaway
> worktree and verified the foundation runs (bare byte-Xor: Flat(4) whole-domain +
> Flat(8) strided + real `system_step` == eval, ROM `[Xor{rd:1,rs:0}]` identical across
> widths). Changes folded:
> - **SLICE-1 = foundation + bare byte-Xor ONLY** (this doc's §1–§2, §4, T1–T4/T6/T-neg).
>   The §3 byte-XOR **reduce** is **SLICE-2 (deferred)** — F5: `memexch_prim` hardcodes
>   `idx_ty=elem_ty` and needs an explicit `idx_ty` param (keep `idx:Bit`, `elem:Flat(w)`);
>   AND the reduce **never runs at Flat(8)** (memexch table = 33.5M entries) — it ships at
>   **Flat(4) max**. Slice-2 authorizes the `memexch_prim(elem_ty, idx_ty, n)` extension.
> - **Blast radius = exactly 10 arms in 5 crates** (not "core+eval+tisc only"): the 9 the
>   design listed PLUS `theseus-netlist/src/lower.rs:142 type_is_uninhabited => Flat(_)=>false`
>   (§5 was wrong that netlist is untouched) PLUS `theseus-eval/src/value.rs:16 inhabits`
>   (a silent `_ => false` → Flat reported non-inhabitant; MUST get a Flat arm + test).
>   **theseus-check needs ZERO changes** (§1.3 was wrong — its matches are on the surface AST).
>   Exact sites: ty.rs:66 Display, prim.rs:43 enumerate_values, core enumerate.rs:9
>   cardinality_u64, eval enumerate.rs:21 cardinality, eval interp.rs:338 cardinality_u64
>   (3rd copy), netlist lower.rs:142 type_is_uninhabited, compile.rs:73 num_leaves / :114
>   value_from_leaf_bytes / :142 value_to_leaf_bytes, eval value.rs:16 inhabits.
> - **Width bound at `num_leaves`** (CompileError for n∉1..=8) — the ctor assert is bypassable
>   (`Flat(9)` wraps the u8 counter silently in release). T-neg constructs `Flat(9)` directly.
> - **Flat(8) 2-operand table cost ~4 GB / ~10 s** — the test MOVES (not clones) the PrimDef.
>   Standing rule: table-based `PrimDef` caps Flat(8) at **2 operands forever** (why Flat(n) exists).
> - **allowlist += `theseus-netlist/**`** (else `cargo build` can't go green). Add tests:
>   `inhabits` Flat arm, `value_from_leaf_bytes` range assert, direct-`Flat(9)` reject,
>   `enumerate(Flat(n)) == enumerate(numeral_ty(2^n))` pin. `Flat(1) ≠ Bit` (distinct encodings —
>   do NOT replace Bit). NOTE: 4 pre-existing `theseus-spice` reds on clean tip (unrelated).


**Milestone.** The foundation for f3.5's natural sequel — **byte-wide** computation.
Today a program value leaf holds **one bit** (`Bit = Sum(One,One)`, the only single-leaf
type, since `num_leaves(Sum)=1+max(...)` caps a leaf at 2 inhabitants). This is the
"single-leaf value width = 1 bit" wall that blocks arithmetic sum and byte-wide masked
accumulate. This milestone introduces a leaf that carries a full `n`-bit word in **one
register**, verified small (`Flat(2)/Flat(4)` whole-domain) and shipped at `Flat(8)`.

Chosen after a design consultation: **`Flat(n)` (width-parameterized) over a fixed
`Word8`**, because the correctness authority (L19: `run_on_cpu == eval` over the *whole*
domain) and the recognizer both **enumerate every value of the input type** — which
scales `(2^w)^{#operands}`. A 3-operand op at `Word8` is `256³ = 16.7M` (infeasible to
build/enumerate/verify); at `Flat(4)` it is `4096`. So width-parameterization is what
keeps the whole-domain oracle *intact* for the eventual masked-accumulate. **Discipline:
develop + whole-domain-verify at `Flat(2)/Flat(4)`; ship `Flat(8)` verified by strided
real-CPU sampling + the parametric argument.**

## 0. What `Flat(n)` is (and is NOT)

- A NEW **`Type`** constructor: `Type::Flat(n)` (n = bit-width). `cardinality = 2^n`;
  **`num_leaves = 1`** (one register). This decoupling of *cardinality* from *leaf-count*
  is the entire trick — no existing sum/product type can do it.
- **`Value` is UNCHANGED.** A `Flat(n)` value is the existing thermometer
  `Right^k(Left(Unit))` (numeral encoding — the same one `numeral_ty` produces, and the
  same one `leaf_value_to_u8` already decodes to `k`). `enumerate(Flat(n)) =
  [Left(Unit), Right(Left(Unit)), …, Right^{2^n-1}(Left(Unit))]`.
- It is **orthogonal to `MemWidth`/`W8`/`MEM_WORDS`** (those are datapath/memory *register*
  widths — already 8-bit). This is *value* width — how many inhabitants one program leaf
  carries. NOT "M-D.c" (which the roadmap mislabeled; M-D.c is MEM_WORDS widening).
- **First slice demonstrates via `Xor` only** (bitwise, no wraparound/canonicalization
  subtlety). `Add` (mod-2^n vs the mod-256 `Instr::Add`) and byte-parallel `Ccnot` (the
  decoder second-buffer, F35-1b) are explicitly **later slices**.

## 1. Core changes (`theseus-core`)

1. **`ty.rs`**: `Type::Flat(u32)` variant (bit-width; assert `1..=8` for now — `W=8`
   register ceiling). Convenience `Type::flat(n)`. `numeral_ty` etc. unchanged.
2. **`enumerate.rs`** (core) + **`theseus-eval/src/enumerate.rs`**: `cardinality(Flat(n))
   = 1u64 << n`; `enumerate(Flat(n))` = the `2^n` thermometer values `Right^k(Left)` for
   `k in 0..2^n`. (Both crates have a `cardinality`/`enumerate` — keep them in lockstep.)
3. **`typing.rs`** (theseus-check, 6 files touch `Type`): a `Flat(n)` is a well-formed
   closed type; an iso `Prim` over `Flat` types type-checks by its declared in/out types
   (prims are already opaque to the checker). **Verify** no `match ty` arm silently drops
   `Flat` (compile-error forces coverage — a feature). Most theseus-check `Type::Sum`
   sites are in elaboration/μ-unrolling that `Flat` never reaches (it has no surface
   syntax yet — §5), but every exhaustive match must gain a `Flat` arm or a documented
   `unreachable!`.

## 2. Compiler changes (`theseus-tisc/src/compile.rs`)

1. **`num_leaves`**: `Type::Flat(_) => Ok(1)`. (One register per word.)
2. **`value_to_leaf_bytes` / `value_from_leaf_bytes`** (+ the `_pub` versions): `Flat(n)`
   → emit exactly `[leaf_value_to_u8(v)]` (the byte, 0..2^n); reconstruct
   `Right^byte(Left(Unit))`. NO tag/zero-pad (unlike `Sum`). `leaf_value_to_u8` already
   returns 0..255 — no change there.
3. **`home_of`, `marshal`/`unmarshal`, `marshal_resident`**: UNCHANGED — they operate on
   leaf-count + bytes, and `Flat` is 1 leaf producing 1 byte. (A `Flat(8)` array element =
   1 leaf = 1 mem word = 1 byte — same shape f1's `Bit` array used, just a full byte.)
4. **Recognizers** (`check_pattern_2reg`/`3reg`): **UNCHANGED** — they already compute
   over full `u8` (`leaf_value_to_u8` → 0..255, `f(a_byte,b_byte)`). A byte-`Xor` prim
   `(a,b)↦(a, a^b)` over `Flat(8)` leaves recognizes to `Instr::Xor` for free.
5. **Canonicalization invariant (n<8 only):** a `Flat(n<8)` value's byte is `< 2^n`, so
   the register's upper `8-n` bits are 0. `Instr::Xor` of two such is closed (bitwise).
   `Instr::Add` is NOT (mod-256 carry dirties the upper zone) → **Add deferred to
   `Flat(8)`** where the full register is canonical. Assert/`marshal` guarantees the
   upper-bits-zero invariant on input; the recognizer's whole-table check confirms outputs
   stay canonical (this is the *same* invariant a `Bit` value already relies on — bits
   1..7 zero — so it is not new machinery, just a wider zero-zone).

## 3. The demonstrating programs

**Probe / unit (isolates the foundation):** a bare byte-`Xor` prim `(a,b) ↦ (a, a^b)`
over `Flat(w)` → compiles to `[Xor{rd,rs}]`; `run_on_cpu == eval` over the domain.

**Integration (the actual sequel — byte-wide XOR reduce):** reuse f3's `reduce_iso`
skeleton but over `Flat(w)` array elements instead of `Bit`: `acc ^= arr[i]` where
acc/arr are `Flat(w)` words. Carrier `Nat_d × (acc:Flat(w), data:Flat(w), idx, arr0:
Flat(w), arr1:Flat(w))`; body = `(id⊗memexch); assoc;(reduceStep⊗id);assoc; (id⊗memexch);
succ`, `reduceStep = SwapProd; xor_w; SwapProd`. `memexch` swaps whole words (byte-granular
→ works over `Flat(w)` unchanged). This is the byte-wide analog of f3, and the direct
stepping-stone to the masked accumulate (which only additionally needs byte-parallel
`Ccnot`, a later slice).

## 4. Verification discipline (the reason for `Flat(n)`)

- **Whole-domain (default tier):** run at `Flat(2)` and `Flat(4)`. Domains: bare Xor
  `Flat(4)` = `16²=256`; the reduce at `Flat(2)`, `d=3` ≈ a few thousand — both
  tractable for `run_on_cpu_fast == eval` over the ENTIRE domain + the recognizer's
  whole-table check.
- **`Flat(8)` (ship tier):** the whole domain (`256²` bare / `256^k` reduce) is too large
  to enumerate. Verify by (a) **strided real-CPU** `run_on_cpu == eval` sampling (L19
  authority, `#[ignore]`), and (b) the **parametric argument**: the compiled ROM is
  *identical in shape* across widths (same `Instr::Xor`, same homes) — width only changes
  the register byte values, which the byte-parallel `Xor` handles uniformly. Pin that the
  `Flat(4)` and `Flat(8)` programs compile to the *same instruction sequence*.
- **No silent width cap:** if `Flat(8)` whole-domain is skipped, `log`/comment says so
  (L19 no-blind-spot).

## 5. Explicitly DEFERRED (later slices / not this milestone)

- **Byte-parallel `Ccnot`** (the masked-accumulate AND) — needs the decoder second-buffer
  convention (F35-1b); its own design + system_step review. THIS slice makes it *possible*
  (Flat values exist) but does not build it.
- **`Add`/arithmetic at sub-byte width** — mod-2^n wraparound; clean only at `Flat(8)`.
  A `Flat(8)` `Add` reduce (real numeric sum) is a natural quick follow-on.
- **Netlist lowering + energy pricing of `Flat`** — `lower.rs` matches `(Type::Sum,
  Value::Left/Right)` to build rails; a `Flat` leaf needs an `n`-rail arm. NOT exercised by
  `run_on_cpu==eval` (that applies the fixed `system_step`; the `Flat` value is data in the
  register carrier). Energy (`rom_program_energy`) is `#[ignore]`-deferred, as in f2/f3.
- **Surface syntax** (`.thse` `Word8`/`Flat` type) — programs built programmatically as
  `Iso`/`Type` values (like f1/f2/f3). Elaborator support later.

## 6. Test plan (L20/L21)

`tests/valwidth.rs` (new):
- **T1** — `Type::Flat(n)`: `num_leaves(Flat(n))==1`, `cardinality==2^n`, `enumerate`
  yields the `2^n` thermometers; `value_to_leaf_bytes(Flat(8), Right^k(Left))==[k]` and
  round-trip `value_from_leaf_bytes`.
- **T2** — bare byte-`Xor` prim over `Flat(4)` compiles to exactly `[Xor{rd,rs}]`;
  `run_on_cpu_fast == eval` over the **whole** `16²` domain.
- **T3** — same at `Flat(8)`: compiles to the **same** `[Xor]` shape (parametric pin);
  `run_on_cpu_fast == eval` **strided** (e.g. 64 sampled pairs incl. 0/255/0xAA/0x55).
- **T4** (`#[ignore]`, L19 real CPU) — `run_on_cpu == eval` (real `system_step`) on a
  handful of `Flat(8)` Xor inputs (nonzero, dense bytes).
- **T5** — the byte-XOR **reduce** at `Flat(2)`, `d=3`: whole-domain `run_on_cpu_fast ==
  eval`; assert `acc_out = acc0 ⊕ (⊕ arr[touched])` over full words (not just bit 0).
- **T6** — canonicalization: a `Flat(4)` value's register byte is `< 16` through the whole
  reduce (upper bits stay 0); an out-of-range `Flat(4)` input is rejected/asserted.
- **T-neg** — `num_leaves` / marshalling on a `Prod(Flat,Flat)` (a 2-word value = 2
  leaves) is correct; a `Flat(9)` (>W) is rejected.

Whole-suite `cargo test -p theseus-tisc` + `theseus-core`/`theseus-eval` around merge
(new `Type` variant → any non-exhaustive match is a compile error the full build catches).

## 7. Probe (worker's first step / advisor pre-check)

Add `Type::Flat` + the core/tisc wiring, then just T1+T2 (bare Xor at Flat(4),
whole-domain `run_on_cpu_fast==eval`). Go/no-go: does a `Flat(4)` value marshal into a
register and `Xor` compute the byte function? If a non-exhaustive `match ty` elsewhere
(eval/check/netlist) blocks compilation, catalogue exactly which — that's the true blast
radius (the design predicts it is small + mechanical). If `run_on_cpu != eval`, STOP.

## 8. Delegation shape

One maestro task (tier 0, theseus profile): `Type::Flat` + core/eval/check exhaustive-arm
wiring + tisc `num_leaves`/marshalling + `tests/valwidth.rs`. `file_allowlist`:
`crates/theseus-core/**`, `crates/theseus-eval/**`, `crates/theseus-check/**`,
`crates/theseus-tisc/**`. `check_commands`: `cargo clippy` + a scoped
`cargo test -p theseus-core -p theseus-eval -p theseus-tisc --test valwidth` + a build of
the workspace (`cargo build`) to surface any non-exhaustive `match` in netlist/syntax
(fix by adding `Flat` arms — `unreachable!` where genuinely unreached, e.g. netlist until
energy). Do NOT touch the decoder / recognizer semantics / `Value` enum.

## 9. Risks / adversarial-review targets

1. **Hidden non-exhaustive matches.** The new `Type::Flat` variant must be handled (or
   `unreachable!`) at every `match ty` across 6 crates. The review should enumerate them
   and confirm each arm is correct — especially any that *silently* wildcard `_ => ...`
   and would mis-handle `Flat` without a compile error.
2. **The thermometer `Value` at `Flat(8)`** is a 255-deep nested `Value` — confirm no
   stack-overflow / quadratic blowup in eval/enumerate/marshalling at depth 255.
3. **Canonicalization (n<8):** is upper-bits-zero *actually* preserved by every op in the
   reduce (memexch, Xor, succ, the borrow-swap)? The review should trace it.
4. **`run_on_cpu` really is Flat-agnostic** — confirm the netlist/system_step path never
   inspects the program's value type (only register bytes), so no netlist `Flat` arm is
   needed for the L19 oracle.
5. **Recognizer whole-table cost at Flat(8):** the bare-Xor prim's forward table is
   `256`-entry (1-op pair enumerated = 256²? no — 2-leaf prim enumerates the input type =
   `Prod(Flat8,Flat8)` = 65536). Confirm building/checking a 65536-entry `Flat(8)` Xor
   prim is feasible (it is — 65536 is fine), and that the *reduce* stays at `Flat(2/4)` for
   whole-domain.
