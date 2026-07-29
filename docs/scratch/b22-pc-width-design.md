# B-2.2 · Per-program PC width (`w_pc`) — SCOPING DESIGN (read-only)

**Status:** advisor scoping draft, from a **read-only** audit of `wt-b22` (off
theseus-v2 `0bd5ea9`, post B-2.1a/b/c). No `.rs` touched; heavy tier not run.
Input to a later delegate-implemented slice; the advisor reviews + commits.

**One-line thesis.** B-2.1a/b/c already made the `Layout` struct carry `w_pc`
(default 16) and parameterized `state_ty_with`/offsets/`fetch_with`/
`pc_add_br_with`/`pc_inc_with` on it. But **no path ever passes a `w_pc ≠ 16`**,
and three builders assume PC occupies exactly two fields (`pc_field` +
`pc_hi_field`). B-2.2 (a) *derives* `w_pc` per program from the already-computed
`B + S_max`, and (b) fixes the residual "PC = 2 fields, 16 bits" assumptions so a
small-ROM program drops PC_hi and runs a byte-wide PC adder. The arithmetic is
already proved parametric (ADR-0034 on `Z/2^k`; widen-PC); this slice is *wiring +
field-count generalization*, not new proof. The win is small (PC/fetch is a few %
of the step vs decode's ~mass) — this is a **low-value, medium-risk** slice; see
Q7.

> **Efficiency model** (framing §2, energy.rs): `step_j =
> balanced_energy(step_net)`, a static cell-inventory price; `total_j = B·step_j`.
> The win of a width shrink = the cells (+ balance buffers) removed from the one
> fixed `step` netlist. Dropping PC_hi removes a state field; narrowing the adder
> from 16→8 bits roughly halves `pc_add_br`/`pc_inc` cell counts (measured deltas
> below).

---

## Q1 · What narrows, and what determines a program's minimum

**The narrowing.** PC value width `w_pc` today is the fixed 16 (`W_PC`). It drives
three things, all already `Layout`-parameterized in the source but never varied:

1. **Field count.** PC occupies `pc_fields() = w_pc / w_ctl` fields (`types.rs:124`).
   At `w_pc=16, w_ctl=8` → 2 fields (`PC_lo` at `pc_field()`, `PC_hi` appended at
   `pc_hi_field()`). At `w_pc=8` → **1 field**, and `PC_hi` disappears from
   `num_cpu_fields()` (`types.rs:141-144`: `… + (pc_fields()-1)` term goes to 0).
   Dropping PC_hi removes one 8-bit state field plumbed every step, and shrinks
   every downstream field index (`pc_hi_field`, `mem_field`, `state_bits`).

2. **PC adder width.** `pc_add_br_with`/`pc_inc_with` build a TK ripple over
   `np = layout.w_pc` bits (`step.rs:410,494`). At `np=8` the adder loops
   `1..np-1 = 1..7` (7 iterations) vs `1..15` at 16 — roughly **half** the
   Toffolis/CNOTs (measured: 8-bit baseline `pc_add_br`=74, `pc_inc`=43 vs 16-bit
   194/162 — see step.rs:347-360 standing-cost block).

3. **Fetch high-zero gate.** `pc_high_zero(layout, a)` zero-checks PC bits
   `[a..w_pc)`; at smaller `w_pc` there are fewer high bits to AND (Q3).

**Per-program minimum.** `w_pc_min = 8 · ⌈(bits needed) / 8⌉` where "bits needed"
= `⌈log₂(B + S_max + 1)⌉`, i.e. round the reachable-PC bit-count **up to a whole
`w_ctl` (=8) multiple** so PC occupies an integer number of `w_ctl`-wide fields
(`pc_fields()` is integer division — see DF-1). Concretely:

| B + S_max | bits | `w_pc` | PC fields |
|-----------|-----:|-------:|-----------|
| ≤ 255     |  ≤ 8 |      8 | 1 (no PC_hi) |
| 256…65535 | 9…16 |     16 | 2 (today) |

So **any program with `B + S_max ≤ 255` drops to `w_pc = 8`** (fib: B≈… a handful
of bits; the design's "5–6 effective bits" example → 8). Programs with B+S_max ≥
256 stay at 16 (unchanged, byte-identical — Q4). There is no sub-byte `w_pc`: the
field granularity is `w_ctl = 8`, so the only two live values are 8 and 16.
(A `w_pc=8` program still needs the `+1` in `B + S_max + 1`: PC ranges over
`[0, B + S_max]` inclusive on the parked/forward-skip zone, and `2^w_pc >
max reachable PC` is the ADR-0034 fetch-injectivity witness — see the assembler
cap `prog.len() ≤ PC_ADDR_SPACE`, asm.rs:483, and the `run_rom_on_cpu_with`
guard `budget + s_max_val ≤ layout.pc_addr_space()`, compile.rs:4555.)

**Where `B + S_max` is available (compile-time).** `compile_with_metadata` →
`Lowered` (`compile.rs:2939`) already exposes:
- `Lowered::wcet()` = **B** (`compile.rs:2961`, compositional over the cost tree),
- `Lowered::s_max()` = **S_max** = `forward_skip_total` (`compile.rs:2968`).

These are exactly the two facts `w_pc` needs, and they are *already threaded* into
`run_rom_on_cpu`/`run_rom_on_cpu_with` as the `budget`/`s_max_val` params that
feed the `pc_addr_space()` guard. So the derivation input is in hand — no new
analysis pass. (Contrast `derive_layout(instrs: &[Instr])`, compile.rs:3069, which
takes only the flat instr stream and therefore *cannot* see B+S_max — see Q6.)

---

## Q2 · The adder / sext / ADR-0034 at `w_pc = 8`

**Does `pc_add_br`/`pc_inc` collapse cleanly to the pre-widen 8-bit form?**
Structurally **yes**, with one wire-layout caveat (below):

- **sext(BR) at `w_pc=8`.** `pc_add_br_with` materializes `op[i]` for `i in 0..np`
  with `src = br(i) if i < w_ctl else br(w_ctl-1)` (step.rs:414-416). At `np = 8`
  and `w_ctl = 8`, the loop is `i in 0..8`, every `i < 8`, so `src = br(i)` for all
  bits — **sign-extension is identity** (no high bits to fill). The materialized
  operand is exactly `BR`, the pre-widen 8-bit case. Correct by construction.

- **The TK adder at `np=8`.** The loops `1..np-1`, `0..np-1`, and the
  `a(np-1)/a(np-2)` fixups (step.rs:425-445) are all written in terms of `np`, so
  at `np=8` they reproduce the shipped 8-bit ripple adder loop-for-loop. No `16`
  or `2`-field literal appears in the adder body. `pc_inc_with` is identical.

- **ADR-0034 translation-on-`Z/2^w_pc`.** ADR-0034 (0034-reversible-branching.md)
  proves `A : (PC,BR,rest) ↦ (PC + sext(BR) + 1 mod 2^k, BR, rest)` is a bijection
  (inverse `PC − sext(BR) − 1`) **for every k** — the injectivity argument (D4) is
  size-independent, re-argued for arbitrary k by widen-PC. `w_pc = 8` is just a
  specific k, so the bijection holds. The *mechanized* re-proof
  (`(pc_add_br;pc_inc)(PC,BR) == (PC+sext(BR)+1) mod 2^w_pc`, the step.rs test at
  tests/step.rs:433-434) must be **re-run at k=8** as the gate, but the argument is
  unchanged. At `w_pc=8`, sext is identity and this collapses to the original
  8-bit-PC mod-256 identity that shipped pre-widen — i.e. the *oldest* proven case.

- **THE CAVEAT — the hardcoded 2-field wire list (the real B-2.2 work).** Both
  `pc_add_br_with` (step.rs:454-464) and `pc_inc_with` (step.rs:528-537) pass a
  **fixed** field list that always includes `layout.pc_hi_field()`:
  `[pc_field, pc_hi_field, br_field, s0, s1]` (add) / `[pc_field, pc_hi_field, s0,
  s1]` (inc). At `w_pc=8`, `pc_fields()=1` and `pc_hi_field()` (types.rs:192,
  `num_regs + 6 + num_scratch`) points **one past the last CPU field** (=
  `num_cpu_fields()`) — an invalid field index (or, on the composite carrier, the
  first *memory* field). `on_fields_l` (decoder.rs:258) would address a wrong/OOB
  field. **This is the central threading gap B-2.2 must fix:** the field list and
  the wire-count `n = 5*wc`/`4*wc` must become `pc_fields()`-driven — pass the
  `pc_fields()` PC fields (1 or 2), size `n` from the actual addressed widths, and
  drop the PC_hi wire byte when `pc_fields()==1`. The adder *core* is fine; the
  *field addressing* around it assumes 2 PC fields.

  (Note: at `w_pc=8` the operand `op = 3*wc + i` spans wires `24..32` = the s0
  word only — it no longer needs s1 as the operand-high byte. So `pc_add_br_with`
  at w_pc=8 could address `[pc, br, s0, s1]` with s1 free, but s1 is still needed
  as the adder ancilla-carry region convention — the implementer must confirm the
  ancilla/carry wire against the narrowed layout, not assume the 16-bit s0‖s1
  operand split.)

**Verdict Q2:** the adder collapses cleanly *mathematically* (sext→identity, TK
loops parameterized on np, ADR-0034 holds at k=8 as the oldest proven case). The
**only** code change is generalizing the fixed 2-PC-field wire list to
`pc_fields()`.

---

## Q3 · Narrow-A fetch interaction

Fetch is **already per-program** on `A_fetch = fetch_addr_bits(rom_len)` (the
`⌈log₂ rom_len⌉` low PC bits, step.rs:194) — this predates B-2.2. B-2.2 only
changes the "rest" that the shared high-zero gate covers:

- `pc_high_zero(layout, a)` (step.rs:209) ANDs `¬PC[a..w_pc)` — at `w_pc=8` the
  hi-bit range is `[a..8)` instead of `[a..16)`, so **fewer** high bits to
  zero-check (a smaller AND chain). It already reads `fw.w_pc = layout.w_pc`
  (step.rs:210), so it composes with a reduced `w_pc` with no code change.
- The `a >= w_pc` short-circuit (`hz ≡ 1`, step.rs:213) fires when the ROM fills
  the whole address space (`A_fetch == w_pc`) — benign at either width.
- `MAX_FETCH_A = W_PC - 1` and `MAX_FETCH_ROM_LEN = 2^15` (step.rs:168-171) are
  `const`s off the *default* `W_PC=16`. At `w_pc=8` the true per-program limit is
  `2^7 = 128` (rom_len ≤ `2^(w_pc-1)`, the F-2 chain/hz-collision bound — see
  `fetch_with`'s assert at step.rs:318, which already uses `1 << (w_pc-1)` from the
  *layout*, not the const). So `fetch_with` **already rejects** correctly at
  reduced w_pc; only the two module `const`s (used for docs/asserts elsewhere) are
  16-pinned and would need a per-layout variant if any caller uses them as a cap at
  reduced w_pc. **Check:** grep shows `MAX_FETCH_ROM_LEN` is referenced only in
  asm.rs doc comments + the assembler cap is `PC_ADDR_SPACE` (default) — so at
  reduced w_pc the derivation must also ensure `rom_len ≤ 2^(w_pc-1)` (it will, since
  `w_pc ≥ ⌈log₂(B+S_max+1)⌉` and `rom_len ≤ B`).

**Verdict Q3:** fetch composes with reduced `w_pc` with **zero** builder changes
(the wire bases derive from `layout.w_pc`/`w_ctl`, step.rs:131-144); only the two
16-pinned module consts and the `A_fetch` clamp (`a.clamp(1, W_PC)`, step.rs:199 —
uses the const `W_PC`, should be `layout.w_pc`) need per-layout threading. **The
`fetch_addr_bits` clamp to `W_PC` (const 16) is a latent bug at reduced w_pc** —
it must clamp to `layout.w_pc`, else a small ROM under `w_pc=8` could compute
`A_fetch > 8`. In practice `rom_len ≤ 2^7` so `A_fetch ≤ 7 < 8`, so it never bites
— but the implementer must thread it for defense (flag as review item).

---

## Q4 · Byte-identity at `w_pc = 16`

**Is the machinery already parameterized so `w_pc=16` reproduces today?** Yes:

- `pc_fields()` = 16/8 = 2, `num_cpu_fields()` includes `(2-1)=1` PC_hi field
  (types.rs:141), `pc_hi_field()` lands where it does today. Byte-identical.
- `pc_add_br_with`/`pc_inc_with` with `np=16` reproduce the current 16-bit adder
  (the loops are literally the shipped code, parameterized). Byte-identical.
- `fetch_with`/`pc_high_zero` at `layout.w_pc=16` reproduce today's high-zero gate.
- The `poc_step_net_bit_identical_across_type_repr` golden (tests/energy.rs:107)
  lowers `step(test_rom)` = `step_with(&Layout::default(), rom)` (step.rs:571-573)
  and asserts a fixed net fingerprint. Since B-2.2 changes **nothing** on the
  `Layout::default()` (w_pc=16) path — it only makes callers *pass a smaller
  layout* — this golden stays green untouched. **This is the primary byte-identity
  guard and must remain green with no re-pin.**

**Residual 2-PC-field / 16-bit assumptions found (must be fixed or confirmed
harmless before shrinking):**

1. **`pc_byte_field(k)` (types.rs:510-516)** — a `const fn` that `panic!`s for
   `k ≥ 2` ("k out of range for W_PC=16"). If any builder uses it, it hardwires 2
   fields. *Grep: no callers in the shrink path* — but it encodes the assumption
   and should gain a `k < pc_fields()` bound or be removed.
2. **The fixed 5-/4-field wire lists in `pc_add_br_with`/`pc_inc_with`** (Q2
   caveat) — the real fix.
3. **`fetch_fields(layout)` (step.rs:175-186)** always returns 8 fields including
   `pc_hi_field()`. At `w_pc=8` this addresses a nonexistent PC_hi field. Must
   become `pc_fields()`-driven (drop pc_hi entry when 1 field).
4. **`fetch_addr_bits` clamp to const `W_PC`** (step.rs:199) and **`MAX_FETCH_A`/
   `MAX_FETCH_ROM_LEN` consts** (step.rs:168-171) — 16-pinned; thread to layout.
5. **`read_pc_bits` (types.rs:919)** reads `PC_FIELD` + `PC_HI_FIELD` (both const,
   default layout) — used by `decode`/`decode_composite` (the *default* path).
   `decode_with` (types.rs:684-688) already reads PC via `layout.pc_field()` +
   `layout.pc_hi_field()`, so at `w_pc=8` it would read a bogus hi byte. **Bug:**
   `decode_with`/`encode_with` unconditionally push/read a PC_hi byte
   (types.rs:660 `push_u8_bits(pc>>8)`; :686 read hi) — at `w_pc=8` there is no
   PC_hi field, so the marshalling must branch on `pc_fields()`.

**So byte-identity at 16 is safe (nothing on the default path changes), but the
reduced-`w_pc` path has ~5 concrete sites (fetch_fields, the two adder field
lists, encode_with/decode_with PC marshalling, the fetch_addr_bits clamp) where
"PC = exactly 2 fields" is still baked in.** These are the B-2.2 work items.

---

## Q5 · Offsets / STATE_BITS threading (interaction with B-2.1b/c)

B-2.1a/b/c made offsets **layout-derived**: `num_cpu_fields()` (types.rs:141),
`state_bits()` (:154), `pc_hi_field()` (:192), `mem_field()` (:197), and the
mixed-width `field_width()` (:216) are all functions of the `Layout`. Dropping the
PC_hi field (pc_fields 2→1) automatically:

- shrinks `num_cpu_fields()` by 1 (the `pc_fields()-1` term → 0),
- shifts `pc_hi_field()` / `mem_field()` down (they are computed, not const),
- reduces `state_bits()` by `w_ctl` (=8) bits.

**These are already threaded** — the field-index and STATE_BITS math needs **no
new work** for B-2.2; it falls out of `pc_fields()` going to 1. The gaps are
**not** in the offset math; they are in the three *builders* that hardcode a 2-PC
-field wire list (Q2/Q4) and in `encode_with`/`decode_with` PC marshalling (Q4.5),
which assume PC_hi always exists.

**Composition with B-2.1b/c.** B-2.1b (reduced `num_regs`) and B-2.1c (reduced
`w_data`) are **orthogonal** to `w_pc`: `field_width()` classifies PC bytes as
CONTROL (`w_ctl`, types.rs:231-232), so narrowing `w_data` never touches PC width,
and shrinking `num_regs` only shifts `pc_field()`/`pc_hi_field()` (already
computed). The one shared surface is `num_cpu_fields()`/`state_bits()`, which both
slices feed — but since both are computed from the `Layout`, a layout that shrinks
*both* `num_regs` and `w_pc` composes automatically. **No conflict; the offset
plumbing already handles the cross-product.** (Verify with a combined-shrink
round-trip test — Q5 gate.)

---

## Q6 · Derivation — where `w_pc` gets set

**The gap:** `derive_layout(instrs: &[Instr])` (compile.rs:3069) derives
`num_regs`/`mem_words` from the *flat instr stream* and leaves `w_pc` at the
default 16 (its doc says so: "`w_data`/`w_ctl`/`w_pc` are unchanged", :3066). It
**cannot** derive `w_pc` because B + S_max come from the *cost tree*
(`Lowered`), not the instr list — `derive_layout` never sees them.

**Two options:**
- **(6-A, recommended) A new `derive_layout_from_lowered(&Lowered)` (or extend the
  `Lowered`→layout path)** that has `wcet()` + `s_max()` in hand and sets
  `w_pc = 8 · ⌈log₂(B + S_max + 1) / 8⌉` (clamped to `[8, W_PC]`), then folds in the
  `num_regs`/`mem_words` from `derive_layout(&lowered.instrs)`. This is the natural
  home: the callers that need the reduced layout (`run_rom_on_cpu_with`,
  compile.rs:4536) already receive a `layout` argument and already compute
  `budget`/`s_max_val` — the same B+S_max. So the *call sites already have the
  inputs*; only the derivation helper is missing.
- **(6-B) Pass B+S_max into `derive_layout`** by threading `&Lowered` — bigger
  signature churn.

**Sibling in-flight (`wt-autoderive`).** Auto-derive of `w_data` is a separate
slice on `wt-autoderive`. B-2.2's `w_pc` derivation composes with it: both write
disjoint `Layout` fields (`w_pc` vs `w_data`), and both consume the same
`Lowered`. **Coordination risk:** if both slices independently introduce a
"derive the full layout from `Lowered`" entry point, they will collide on that
function. **Recommendation:** land B-2.2 to *add* `w_pc` to whatever
`derive_*_from_lowered` shape `wt-autoderive` establishes (or define a single
`derive_full_layout(&Lowered) -> Layout` that both slices extend), and rebase
whichever lands second. Flag for the advisor to sequence — do **not** let both
invent separate derivation entry points. (`derive_full_layout` is named in the
prompt but does **not** yet exist — grep confirms only `derive_layout` +
`for_program_facts`.)

---

## Q7 · Win estimate (order of magnitude — needs measurement)

**The lever is small.** From the standing-cost block (step.rs:347-360), the whole
8→16-bit widen cost `pc_add_br +120`, `pc_inc +119`, `fetch −46` = **net ≈ +193
cells on a ~4846-cell step ≈ +4.0%**. B-2.2 *reverses* this for a `w_pc=8`
program: dropping to a byte-wide PC adder + one fewer PC field recovers on the
order of **≈ −4% of step_j** (roughly halving pc_add_br/pc_inc back to ~74/43,
plus one dropped 8-bit state field, minus the fetch high-zero gate which *grows*
slightly cheaper). Order of magnitude: **a few percent** (call it 3–5%).

**Is it worth it vs w_data?** The B-2 design (Q4) estimates the **data-word `W`
halving at ≈ −25% to −30% step_j** (it scales every ALU core's gate count),
register shrink ≈ −3–6%, mem-drop ≈ free. **B-2.2's PC shrink (~−4%) is the
*smallest* width lever** — PC/fetch is a small fraction of the step; decode +
datapath dominate. So:

- B-2.2 is **low-value** in isolation.
- But it is **cheap and low-risk** (the arithmetic is the oldest proven case,
  sext→identity, and the offset math is already threaded — the work is ~5
  field-list/marshalling fixes + a derivation helper).
- It **stacks additively** with w_data/num_regs (disjoint layout axes), so on a
  fully-shrunk simple program (w_data=4, num_regs=4, w_pc=8, no mem) it contributes
  its few % to the combined ≈ 30–40% the B-2 design projects.

**Flag: order-of-magnitude only.** The real number needs `lower_iso(step_with(
&layout_w_pc8, rom))` + `balanced_energy` at the reduced layout — the same
measurement gate every B-2 slice carries. The ~4% is read straight off the
measured widen deltas (which are authoritative for the *reverse*), so it is a
tighter estimate than the w_data guess, but still needs the lowered cell count to
confirm.

**Recommendation:** land B-2.2 **after** B-2.1 (w_data) — do the big lever first —
but it is genuinely cheap and de-risked, so it is a fine "cleanup" slice once
w_data ships. Do **not** prioritize it over w_data.

---

## Slice plan (smallest-first, risk-tagged)

The infra (Layout carries `w_pc`; `pc_fields`/`pc_addr_space`; the adder/fetch are
`np`/`w_pc`-parameterized) already exists. B-2.2 is three small increments:

- **B-2.2a — generalize the 2-PC-field builders to `pc_fields()`** (no derivation
  yet; still `w_pc=16` everywhere, byte-identical). Fix: `fetch_fields`, the
  `pc_add_br_with`/`pc_inc_with` field lists + wire-count `n`, `encode_with`/
  `decode_with` PC marshalling, `pc_byte_field` bound, and the `fetch_addr_bits`
  clamp → all driven by `pc_fields()`/`layout.w_pc`. **Gate:** the
  `poc_step_net_bit_identical` golden stays green (proves w_pc=16 unchanged) + a
  **new unit test that lowers `step_with` at a hand-built `w_pc=8` layout** and
  checks it type-checks + the field count is `num_cpu_fields`−1. *Risk:* **Low-Med**
  (mechanical field-list generalization; the byte-identity golden is the exact
  guard). This is the enabling refactor; nothing derives yet.

- **B-2.2b — the mechanized ADR-0034 re-proof + a real run at `w_pc=8`.** Un-pin
  the step.rs adder test to run at k=8 (`(pc_add_br;pc_inc) == +sext(BR)+1 mod
  2^8`, exhaustive over the 8-bit PC × 8-bit BR domain — cheap, `2^16`) + a
  `run_rom_on_cpu_with(&layout_w_pc8, small_rom) == eval` round-trip on a real
  worktree (the fib-sized program the design cites). **Gate:** exhaustive re-proof
  green at k=8 + `run_on_cpu == eval` + terminal quiescence (BR/s0/s1 = 0). *Risk:*
  **Medium** (state-layout change on the step iso — the highest-bar surface per
  CLAUDE.md; needs the fresh-context adversarial review + a reproduce-the-finding
  regression test). The proof itself is the oldest case (sext=identity), so the
  *argument* is low-risk; the *field addressing* is where a bug hides.

- **B-2.2c — derive `w_pc` per program from `Lowered` (B+S_max).** Add the
  derivation helper (Q6-A) that sets `w_pc = 8·⌈log₂(B+S_max+1)/8⌉` and wire it
  into the reduced-layout run path, coordinated with `wt-autoderive`. **Gate:** a
  small-ROM program auto-derives `w_pc=8` and `run_on_cpu==eval`; a B≥256 program
  auto-derives `w_pc=16` and still runs (payoff test, widen_pc_payoff.rs
  precedent) + energy re-pin showing the ~4% drop at w_pc=8. *Risk:* **Medium**
  (composition with the sibling auto-derive slice — sequence carefully).

**Recommended order:** B-2.2a → B-2.2b → B-2.2c, and the whole thing **after
B-2.1 (w_data)** in the phase. Each is one delegable task (do not bundle).

---

## Open questions / decision-forks

**DF-1 (byte-granular vs sub-byte `w_pc`).** `pc_fields() = w_pc / w_ctl` is
integer division, so `w_pc` must be a multiple of `w_ctl` (=8) to occupy a whole
number of fields — the only live values are 8 and 16. A `w_pc=6` (fib's true bit
need) is **not representable** without a mixed-width PC field (a 6-bit `flat(6)`
PC word). **Recommendation: accept byte granularity** (w_pc ∈ {8,16}); a sub-byte
PC field is a much larger change (the PC bytes are CONTROL words at `w_ctl`, and
the fetch/adder wire math assumes byte-aligned PC fields) for a marginal extra
1–2% — not worth it. *Confirm with user.*

**DF-2 (derivation home — new helper vs extend `derive_layout`).** Q6-A (a
`Lowered`-based helper) vs Q6-B (thread `&Lowered` into `derive_layout`).
Recommendation: **Q6-A**, coordinated with `wt-autoderive` so there is **one**
`derive_full_layout(&Lowered)` both w_pc and w_data extend, not two rival entry
points. *Advisor to sequence the two in-flight slices.*

**DF-3 (`fetch_addr_bits` clamp bug).** The clamp `a.clamp(1, W_PC)` uses the
const `W_PC=16` (step.rs:199), not `layout.w_pc`. Latent (never bites since
`rom_len ≤ 2^(w_pc-1)`), but it's a 16-pin on a per-program path. **Fix it in
B-2.2a** for defense even though it can't currently misfire.

**Risks (no user decision; for the reviewer):**
- **R-1 (the PC_hi-always-exists assumption).** Five sites bake "PC = 2 fields"
  (Q4). A *missed* site keeps `pc_hi_field()` addressing a nonexistent/aliased
  field at `w_pc=8` → silent misroute onto a memory/OOB field (the widen-PC
  BLOCKER-pattern: unwidened state builder). Mitigation: the byte-identity golden
  guards w_pc=16; a **new** w_pc=8 lowering + round-trip test is the minimum bar
  (CLAUDE.md), and grep every `pc_hi_field`/`PC_HI`/`pc_byte_field`/literal-2 in
  the PC path before shrinking.
- **R-2 (adder ancilla/carry wire at narrowed layout).** At `w_pc=8` the operand
  `op` fits in s0 alone; the s1 word changes role (no longer operand-high). The
  implementer must confirm the ancilla-carry wire against the actual narrowed
  field list, not the 16-bit s0‖s1 split (Q2 note).
- **R-3 (`lower_iso` memory is *smaller* at w_pc=8, good).** Unlike widen-PC
  (grew state, OOM'd), B-2.2 shrinks state → the measurement is cheaper, not
  harder. No new OOM risk.

---

## Appendix — files read (read-only)

- `crates/theseus-tisc/src/types.rs` — `Layout` (`w_pc`, `pc_fields()` :124,
  `pc_addr_space()` :134, `num_cpu_fields()` :141, `pc_field`/`pc_hi_field`
  :172/:192, `field_width` :216, `state_bits` :154); `pc_byte_field` :510 (panics
  k≥2); `encode_with`/`decode_with` :611/:667 (unconditional PC_hi marshalling);
  `read_pc_bits` :919 (const PC_FIELD/PC_HI_FIELD).
- `crates/theseus-tisc/src/step.rs` — `pc_add_br_with` :404, `pc_inc_with` :489
  (TK ripple on `np=w_pc`; fixed 5-/4-field wire lists incl. `pc_hi_field`);
  `fetch_with`/`fetch_fields`/`fetch_addr_bits`/`pc_high_zero`/`FetchWires` :131
  (narrow-A, layout-parameterized; `W_PC` clamp :199; `MAX_FETCH_A` :168);
  `step_with` :580; standing-cost block :347-360 (measured widen deltas).
- `crates/theseus-tisc/src/compile.rs` — `Lowered::wcet()` :2961 / `s_max()` :2968
  (= B / S_max); `derive_layout` :3069 (leaves w_pc=16); `run_rom_on_cpu_with`
  :4536 (layout-threaded, `pc_addr_space()` guard :4555); `is_cpu_only` :3097.
- `crates/theseus-tisc/src/decoder.rs` — `on_fields_l` :258 / `field_bases` :275
  (mixed-width field addressing — the mechanism the PC field list feeds).
- `crates/theseus-tisc/src/asm.rs` — `PC_ADDR_SPACE` cap :483; `MAX_FETCH_ROM_LEN`
  doc :476-478.
- `crates/theseus-tisc/src/energy.rs` — dead-zone `[rom_len, PC_ADDR_SPACE)` :351.
- `crates/theseus-tisc/tests/energy.rs` :107 — `poc_step_net_bit_identical` golden.
- `docs/scratch/b2-per-program-widths-design.md` — parent B-2 design (Q1 w_pc row,
  Q4 win table, B-2.2 slice entry :281-290).
- `docs/src/adr/0034-reversible-branching.md` — the translation-on-`Z/2^k`
  bijection (any k).
