# Adversarial review — B-2.1c auto-derive `w_data` (`0bd5ea9..HEAD`, branch `wt-autoderive`)

Reviewer: fresh-context adversarial reviewer (did not write the change).
Scope: `theseus-core/src/typing.rs` (`max_flat_width`), `theseus-tisc/src/compile.rs`
(`derive_full_layout`, `run_on_cpu` re-route), `theseus-tisc/src/energy.rs`
(`rom_program_energy_auto`), `tests/autoderive_wdata.rs`. Design: `docs/scratch/b21c-data-word-design.md` §Q5.

Verdict: **DO-NOT-MERGE** (two BLOCKERs, both in the `run_on_cpu` re-route, both
constructible with small real programs; the derivation function itself is sound
for *source data* but the applied invariant is wrong).

---

## BLOCKER-1 — control values live in DATA-width registers: every Trace loop
## (and any ≥16-instr skip block) misroutes at auto-derived `w_data < 8`

**One line:** `max_flat_width` correctly bounds the widest *source data* word, but
the compiler also mints **control bytes** — the backward-jump offset δu and
forward-skip distances — into **general (data-width) registers** via `Addi`, and
those need up to 8 bits regardless of the program's `Flat` width; the narrow ring
truncates them.

**Evidence chain (all read from this worktree):**

1. Trace skeleton, `compile.rs:3943-4002`:
   `delta_u = (256 - iter_len) & 0xFF` — **always ≥ 0x80** (top bit set is
   *required* for the sign-extended backward jump, guard at :3924) — is minted by
   `Instr::Addi { rd: temp, imm: delta_u }` into `temp`, a general register drawn
   by `allocate_aux_frame` ("distinct in-register slots", :3945-3953), then
   `Swapbr { rd: rc }` installs it into BR.
2. Forward-skip blocks (Plus/sum dispatch, guarded iter), `compile.rs:1042-1068`:
   `Addi { rd: temp/rb, imm: body_len }` then `Swapbr { rd: rb }` — `body_len`
   up to 127 in a general register.
3. The merged narrow datapath masks both paths to `w_data` bits:
   - `addi_core` (`decoder.rs:1303-1314`): the immediate is CNOT-masked onto s0
     over bits `0..w = layout.w_data` only → at `w_data=4`, `Addi temp, 0xF6`
     adds **6**, not 0xF6.
   - `swapbr_core` (`decoder.rs:1273-1289`): the gated swap r[rd]↔BR runs over
     `0..layout.w_data` bits only → at `w_data=4` BR's high nibble is
     **unreachable**; a backward offset (top bit set) can never be installed.
4. This slice routes every CPU-only, mem-quiescent program through
   `run_rom_on_cpu_with(&derive_full_layout(iso, instrs), …)` (`compile.rs:4647-4653`),
   and `derive_full_layout` sets `w_data = next_pow2(max_flat_width(iso))` from
   `Flat` leaves only. Swapbr is not a memory op, so `is_cpu_only` is true for
   every loop/branch program (`compile.rs:3129`).

**Concrete trigger:** any `Iso::Trace`/loop whose widest data word is `Flat(4)`
(e.g. a traced Flat(4) xor loop — exactly the "narrow-data program" this slice
targets), or any `Plus` over `Flat(≤4)` payloads with an arm ≥ 16 instructions.
`run_on_cpu` auto-selects `w_data=4`; the δu mint truncates 0xF6→0x6; the
backward jump becomes a small *forward* jump; the loop never re-enters. Outcome
is a diverged run — either a terminal-check `Err` or, if the walk happens to end
with BR=s0=s1=0, a **silently wrong value**. Both are `run_on_cpu` regressions
vs the pre-diff default-layout path, which ran these programs correctly.

**Why the shipped tests don't catch it:** the whole heavy suite exercises one
kernel — a straight-line 1-prim xor (`kernel_iso`), Swapbr-free. No loop, no
sum-dispatch, no skip block is ever run at a narrow auto-derived width. (The
B-2.1c/c-2 datapath verification was per-op whole-domain + the same straight-line
kernel; the control machinery at `w_data<8` was never the verified surface.)

**Fix direction (either restores soundness):**
- Gate the narrow route on a ROM scan: reduce `w_data` only when the instr
  stream contains **no `Swapbr`** and **no immediate ≥ 2^w_data** (both facts
  are available in `derive_full_layout(iso, instrs)`; a `has_swapbr` helper
  already exists at `compile.rs:994-999`). Conservative, one function.
- Or (the real B-2.2-shaped fix) make δu/skip staging registers control-width
  (they are architecturally control values) — a datapath change, own slice +
  own adversarial review.
- Per L20, the fix must ship with a regression test: a Flat(4) *traced* program
  run through `run_on_cpu`, failing without the fix.

## BLOCKER-2 — `derive_layout`'s `num_regs` is named-reg-based, but `marshal`
## places INPUT LEAVES by leaf index: untouched-leaf programs now panic

**One line:** `run_on_cpu` newly routes through the reduced layout, but
`derive_layout(instrs)` sizes `num_regs` from *named* registers
(`for_program_facts`: `None → num_regs=1`, `types.rs:97-115`), while
`marshal` fills `regs[i]` for every input **leaf** (`home_of`,
`compile.rs:4366-4372`); `encode_with` then asserts dropped regs are zero
(`types.rs:617-621`) → **panic** on valid programs whose instrs don't name
every input-leaf register.

**Concrete triggers (previously worked, now abort):**
- `Iso::Id(Type::prod(Flat(4), Flat(4)))` — lowers to **zero** instrs
  (`compile.rs:3192`) → `num_regs=1`; input leaf 1 marshals to `regs[1]`;
  `run_on_cpu(iso, (a, b≠0))` hits
  `encode_with: dropped reg r1 must be 0` and **panics** (pre-diff: default
  path, correct answer).
- Non-degenerate version: `Times(xor_prim_pair, Id(Flat(4)))` — "xor the first
  two components, leave the third" — prim names r0/r1 only → `num_regs=2`,
  leaf 2 in `regs[2]` → same panic. Any program with an untouched trailing
  component is in this class, at **any** width including Flat(8).

Loud (assert), not silent — but it is a `run_on_cpu` regression on
well-formed programs, and it aborts (panic, not `Err`) inside the oracle.

**Fix direction:** `derive_full_layout` has the iso — take
`num_regs ≥ next_pow2(min(num_leaves(in_ty), 8))` (and out_ty) joined with the
named-reg derivation; or fall back to the default path when
`num_leaves > named regs`. Ship with an untouched-leaf regression test.

---

## Attack #1 findings — is `max_flat_width` itself an under-reporter? NO (for source data)

Verified exhaustively; the function is sound for what it claims to measure:

- **Exhaustive match:** the `Iso` enum (`iso.rs:13-108`) has exactly the 22
  variants matched in `go()`; no `_` fallthrough; every `Type` param of every
  structural combinator, `Trace`/`Iter`/`Fold`/`Unfold` carrier/spine, and both
  `Prim` types are folded; all sub-isos recursed. A new width-bearing variant is
  a compile error here. Confirmed against the enum definition.
- **Intermediate types cannot exceed scanned types:** every type reachable by
  `type_of` is assembled from the type params of atomic sub-isos (Id/structural
  carry their own; Prim carries in/out), all of which are scanned. A "widening"
  prim `Flat(4)↔Flat(8)` is impossible to *hide* — its `output_ty` is scanned
  (and it is un-admittable anyway: 16 ≠ 256 cardinality).
- **Non-Flat leaves carry only bytes ∈ {0,1}:** leaf-byte encoding
  (`prim.rs:267-322`): `One` contributes no byte; `Sum` contributes a tag byte
  0/1; numerals are **unary tag vectors** (not binary counters), so an
  `Iter(numeral_ty(k), …)` counter never puts a byte ≥ 2 in a data register.
  Flat(n) contributes one byte < 2^n. So the *marshalled data* is genuinely
  bounded by `max_flat_width` — the under-provision is exclusively the
  compiler-minted control bytes of BLOCKER-1.
- **`SpecTransfer.width` cannot legally exceed the declared types:**
  `validate_width_vs_type` (`prim.rs:423-444`) requires `width == n` for
  **every** `Flat(n)` leaf, enforced via `PrimSpec::validate_against` in
  `admit()` (`prim.rs:181-186`), in both directions and on both types. An
  *unadmitted* hand-built PrimDef with a wider spec is constructible (the
  compiler does not re-validate — no `validate_against`/`admit` call in
  compile.rs/bveq.rs), but such a prim is ill-formed input whose output already
  fails `value_from_leaf_bytes` range checks loudly at `w_data=8`; not a new
  exposure of this slice. See MINOR-3.
- **Default 8 is the true ceiling:** `Type::flat` asserts 1..=8; `Flat(u8)`
  leaf bytes cap at 8 bits. `clamp(1,8)` covers hand-built out-of-range widths
  (see MINOR-4).

## Attack #3 — `next_pow2`: SAFE

`max_flat_width` returns 1..=8; `usize::next_power_of_two` maps 1→1, 2→2, 3→4,
5/6/7→8, 8→8 — never down, always a power of two (rotator requirement met).
But see MAJOR-1: it can *select* w ∈ {1,2}, widths the datapath was never
verified at.

## Attack #4 — energy path: SAFE, but the new API is dead + untested

- `rom_program_energy` is byte-for-byte untouched (diff is pure addition);
  me_a3/fib goldens still price against the caller-supplied default net —
  unchanged.
- `rom_program_energy_auto` has **zero callers** — not even
  `tests/autoderive_wdata.rs` uses it (the win test calls `balanced_energy`
  directly). It also inherits BLOCKER-1/2 (it prices `step_with` at the same
  broken-for-loops layout). See MAJOR-2.

## Attack #5 — byte-identity: SAFE at the netlist, CHANGED at the run path

- `step.rs`/`decoder.rs`/`types.rs` untouched by this diff → the default
  `step()`/decode netlist and the `poc_step_net_bit_identical` fixture are
  unaffected.
- But note: a Flat(8) program's `run_on_cpu` **still changes machinery** — it
  now runs `step_with(derived)` with *reduced num_regs* instead of `step(rom)`.
  Equivalent only under the "named regs ⊇ input leaves" assumption that
  BLOCKER-2 shows is false in general.

---

## MAJOR

- **MAJOR-1 — auto-selects unverified widths 1 and 2.** A `Flat(1)`/`Flat(2)`
  program derives `w_data=1/2`. B-2.1c/c-2 correctness authority (design §Q6,
  merged tests) covered w=4 and w=8 only; the rotator/ALU cores at w∈{1,2}
  (log2(w) = 0/1 shift bits) have never been equiv-checked. Floor the derivation
  at 4, or extend the per-op whole-domain equiv to the widths the derivation can
  emit.
- **MAJOR-2 — `rom_program_energy_auto` ships with no test at all** (L20: no
  finding/feature without a test that exercises it). It is also the energy-side
  carrier of BLOCKER-1 (a narrow loop program would be priced on a ring that
  cannot run it).

## MINOR

- **MINOR-1 — precondition failures are `assert!` panics, not `Err`.**
  `run_rom_on_cpu_with` aborts the process on the (now reachable, per
  BLOCKER-2) dropped-reg/width asserts instead of returning `CompileError`
  like every other guard in `run_rom_on_cpu`.
- **MINOR-2 — belt-and-braces:** `derive_full_layout` could fold
  `PrimSpec.width` (both directions) into the max for one line, closing the
  unadmitted-prim gap of Attack #1 without relying on `admit` having run.
- **MINOR-3 — compiler never validates prim specs** (pre-existing): no
  `admit`/`validate_against` call anywhere in theseus-tisc; the width-equality
  invariant is only as strong as "every prim went through admit".
- **MINOR-4 — `clamp(1,8)` silently repairs ill-formed `Flat(0)`/`Flat(>8)`**
  rather than surfacing them; constructor-guarded today, but a `debug_assert`
  would keep the invariant visible.
- **MINOR-5 — doc comment overclaims:** `derive_full_layout` says w_data "is
  never under-reported (an under-narrow w_data would misroute…)" — true for
  Flat leaves, false for the program as compiled (BLOCKER-1). The comment
  encodes the wrong invariant: the requirement is not "≥ widest Flat" but
  "≥ widest value any data-width field must hold across the run".

## Checks run (fast tier only, per constraints)

- `cargo check -p theseus-tisc` (default features): clean.
- `cargo test -p theseus-core --lib typing`: 34 passed (includes the 6 new
  `max_flat_width` pins).
- Heavy tier NOT run (box RAM in use). Heavy suite contents reviewed: it
  cannot catch either BLOCKER (straight-line kernel only, no untouched leaf).

## Verdict

**DO-NOT-MERGE.** The derivation front-end (`max_flat_width`) is correct and
well-pinned for source data widths, but applying it as-is to `run_on_cpu`
violates the actual datapath invariant: `w_data` must cover the compiler's
control-in-data-register bytes (δu ≥ 0x80 on *every* Trace; skip distances),
and `num_regs` must cover the *marshalled input leaves*, not just named
registers. Both blockers have small constructible triggers and neither is
reachable by the shipped test suite. Fix by gating the narrow/reduced route on
ROM facts (`has_swapbr` / max immediate / leaf count) or by moving control
staging to control-width fields (separate slice), each with an L20 regression
test (a traced Flat(4) program; an untouched-leaf program).

---

## Advisor resolution (post-review, applied on wt-autoderive)

All two BLOCKERs and MAJOR-1/MAJOR-2 fixed in `derive_full_layout`
(compile.rs) + regression tests (autoderive_wdata.rs), verified 9/0 heavy.

- **BLOCKER-1 (control-in-registers truncation)** — FIXED. `derive_full_layout`
  now pins `w_data = base.w_data` (8) whenever `has_inner_control(instrs)` is
  true (any `Swapbr` = a Trace come-from δu / forward-skip distance staged in a
  general register). Only straight-line programs narrow. Regression:
  `blocker1_swapbr_program_pins_full_w_data` (Flat(4) iso + Addi{0xF6}+Swapbr ROM
  → asserts w_data==8; fails at w_data==4 without the fix).
- **BLOCKER-2 (untouched-leaf num_regs under-count)** — FIXED. `num_regs` now
  `max`es the input-leaf count (`num_leaves(type_of(iso).0)`, pow2-rounded,
  clamped) so a marshalled-but-unnamed leaf gets a register. Regression:
  `blocker2_untouched_input_leaf_covered_by_num_regs` (`Id(Flat4×Flat4)` → 0
  instrs, 2 leaves; asserts num_regs>=2 + run_on_cpu==identity over the whole
  domain; panics without the fix).
- **MAJOR-1 (unverified narrow widths)** — FIXED. Narrow `w_data` floored at 4
  (`.max(4)`) — the reduced datapath is equiv-verified only at w_data ∈ {4,8}.
  Regression: `major1_narrow_flat_floors_w_data_at_4` (Flat(2) → w_data==4).
- **MAJOR-2 (`rom_program_energy_auto` untested)** — RESOLVED by adding coverage
  (kept the public convenience API): `win_flat4_step_net_shrinks_vs_default_auto`
  now also asserts `rom_program_energy_auto(iso,input) == rom_program_energy(iso,
  input, net_auto)` (the wrapper prices at the same auto-derived layout).

MINOR-1..5 noted; not blocking (the two reachable asserts are now unreachable
for auto-derived layouts because BLOCKER-2 provisions the leaf registers).
Full heavy authority re-run on the FIXED state (the pre-fix run was a
false-green — neither blocker is reachable by a straight-line kernel).
