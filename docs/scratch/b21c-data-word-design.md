# B-2.1c · Per-program reduced DATA-WORD width (`w_data`) — SCOPING DESIGN

**Status:** advisor scoping draft, produced by a **read-only** code audit of
`chessai/theseus-v2` @ `4e5a4dc` (B-2.1b merged). No `.rs` touched, heavy tier
not run. This is the input to a delegate-implemented slice sequence; the advisor
reviews + commits. Companion to `docs/scratch/b2-per-program-widths-design.md`
(the B-2 design; **DF-2 = Option A / Q3-A DECIDED**: fixed 8-bit control region
`w_ctl`, registers + ALU datapath scale with `w_data`).

**One-line thesis.** B-2.1a/b already made `num_regs`/`mem_words` per-program and
threaded a `Layout` through every gate builder. B-2.1c narrows the **data word**
`w_data` — the datapath-width knob, the ~30–40% Phase-B win (stacks on B-1's
−39.4%). The whole slice reduces to two decisions and three mechanical refactors,
all gated by byte-identity at `w_data = 8`.

---

## The single structural fact everything rests on

Under Option A, the CpuState is a **mixed-width** field product. Today
`state_ty_with(layout)` (`types.rs:389`) builds **every** field as
`data_word(layout) = flat(w_data)` — uniformly. That is correct only because
`w_data == w_ctl == 8` at every layout shipped so far. **B-2.1c's foundation is
making `state_ty_with` mixed-width**: DATA fields = `flat(w_data)`, CONTROL fields
= `flat(w_ctl=8)`. Once the *carrier type* is mixed-width, every gate builder that
today computes wire indices arithmetically as `k * W` / `3 * w_data` / `pred3(w)`
must instead index into the actual (mixed) field offsets. This is the entire slice.

The good news the audit confirms: the **config machinery is already
`w_ctl`-parameterized** (`cfg_wire(wc,k) = 4*wc + k`, `config_decode_fields` lifts
over `[IR_b0..b3, s1, s0]`, `alu_config_decode_families` uses `n = 6 * w_ctl`,
`decoder.rs:1693/1706/1762`). So the *control* half already knows about `w_ctl`;
it is the **datapath half** (the ALU cores, the field-routing) that still conflates
`w_data` with the field width, plus the mismatch that the shared cores span BOTH a
`w_data` field (r0-slot) and `w_ctl` fields (s0/s1) in one gate list.

---

## Q1 — Per-field width assignment (THE FOUNDATION)

Field-by-field, under Option A. This is the exact type each field takes in the
mixed-width `state_ty_with`:

| Field(s) | Role | Width | Why |
|---|---|---|---|
| `r0..r{num_regs-1}` | general registers | **`w_data`** | the datapath; hold `Flat(w)` values. |
| `s0` (`scratch_field(0)`, `AUX_OPERAND_FIELD`) | operand-A buffer **+** transient prefix/chain workspace | **`w_ctl = 8`** | holds the fetched A-operand (a data word, zero-extended) AND the 2 transient decode bits (`CFG_PREFIX_S0`/`CFG_CHAIN_S0` at s0 bits 0/1) AND the `W_PC` operand scratch for `pc_add_br`/`pc_inc`. Its *control* uses (PC scratch, workspace) demand ≥8 bits; the data it also holds (A-operand) is only `w_data` wide and zero-extends. |
| `s1` (`scratch_field(1)`, `AUX_PRED_FIELD`) | the 8 held config one-hots + predicate | **`w_ctl = 8`** | B-1 saturated s1 at 8 one-hots (`cfg_wire(k)=32+k`, k up to 7). MUST stay 8 (design F-a). |
| `s2..s{num_scratch-1}` | operand-addressable scratch (spill / synthesizer ancilla holding DATA) | **see Q2 → `w_data`** | these are real operand-space words holding data values; they must match the register width so the operand crossbar can swap a register with a scratch word. |
| `PC_lo`, `PC_hi` | program counter | **`w_ctl = 8`** each | PC is control; `pc_fields = w_pc / w_ctl` (B-2.2 owns PC narrowing). PC arithmetic + fetch address the PC bytes and use s0/s1 as `W_PC` operand scratch, all in the control region. |
| `BR` | branch offset | **`w_ctl = 8`** | control; sign-extended into PC by `pc_add_br`. But see Q2-caveat: BR participates in the Ccnot/Cswap/xchg cores' *operand-B buffer* role. |
| `IR_b0..IR_b3` | instruction word | **`w_ctl = 8`** each | the ISA encoding is byte-oriented (opcode/operand-field bits), format-fixed, NOT data (design DF-1). |
| `m0..` (composite) | memory | `w_data` | data words. **Out of scope for B-2.1c** — a `w_data<8` program is CPU-only by construction in the first slice (mem-using programs are a later composite slice; the composite/bus path is `Layout::default()`-only today, per the B-2.1b review §5). |

**Net: exactly the `num_regs` register fields and the `s2..` scratch fields are
`w_data`; everything else (s0, s1, PC, BR, IR) is `w_ctl = 8`.** For a 4-bit
program with 4 registers, that is 4×4 (regs) + `w_ctl` control region, vs 8×8
today — a real state shrink AND every ALU core's gate count halves (loops
`0..w_data` not `0..8`).

**The load-bearing subtlety (must be got right):** s0 is the **operand-A buffer**,
and the A-operand is *data* — yet s0 is `w_ctl=8`. So the ALU cores read/write a
`w_data`-bit value living in the low `w_data` bits of an 8-bit s0 field
(zero-extended above). The cores already only touch bits `0..w_data` of the
operand slot (`gated_*_gates_w(w, ...)` loop `0..w`), so the high `w_ctl−w_data`
bits of s0 are inert — but the **field is 8 wires wide**, and the gate wire
offsets must be computed against the real field layout, not `k * w_data`. This is
the Q3 refactor.

---

## Q2 — Does the operand crossbar cross a width boundary? (THE CRUX)

`select_operand_l` (`decoder.rs:458`) routes over
`reg(0..num_regs) ∪ scratch(num_regs..num_regs+num_scratch)` and swaps whole
*fields* via `ctrl_word_swap` (a `w`-bit controlled Fredkin, `decoder.rs:300`,
`w = layout.w_data`). If a register (`w_data`) and a scratch word had **different**
widths, a reg↔scratch swap would be an ill-typed `SwapProd(flat(w_data),
flat(w_ctl))` that misaligns the state — a silent-corruption BLOCKER.

The crossbar reaches s2..s7 (positions ≥ `num_regs+2`) for legal scratch operands,
and it swaps them into the register range (r0-slot). **Therefore s2..s7 MUST be
`w_data`** (Q1), matching the registers — option (a). This is clean and correct:

- **s0/s1 (positions `num_regs`/`num_regs+1`) are already excluded from EVERY
  crossbar swap** by the B-2.1b review-F2 position-based exclusion
  (`decoder.rs:494`: `if i == n_reg || j == n_reg || i == n_reg+1 || j == n_reg+1
  { continue }`). So the crossbar never swaps the two `w_ctl` scratch words (s0/s1)
  with anything. The *only* fields the crossbar swaps are `w_data`-wide (registers
  + s2..). **The crossbar therefore lives entirely within the `w_data` operand
  space — NO width boundary is crossed.** This is the key resolution.

- Option (b) ("scratch operands pin `w_data=8`, B-2.1b precedent") is **not
  needed** and would forfeit the win for any spill/ancilla program. It IS still the
  fallback if a slice wants to defer the mixed-width s2.. (see the slice plan: the
  smallest POC uses a register-only program, so s2.. never appears and the choice
  is moot for slice-1).

**Resolution: s2..s7 are `w_data` (uniform operand space with the registers); s0/s1
are `w_ctl=8` and already crossbar-excluded, so the operand crossbar never crosses
a width boundary.** The B-2.1b F2 exclusion, shipped for a different reason
(config-word immovability), is exactly what makes B-2.1c's mixed-width crossbar
type-safe. This must be explicitly noted in the F2 invariant comment as a *second*
load-bearing consequence.

**Q2 caveat — the Ccnot/Cswap/Xchg operand-B buffer.** The 3-operand cores
(`ccnot_core_at`/`cswap_core_at`/`xchg_instr`) use **BR** and **s1** as an
operand-B buffer / control-copy slot (`shared_fetch_rt_br_gated` delivers rt→BR;
`decoder.rs:1029/1050` swap-cores use `[r0-slot, BR, s1]` / `[r0-slot, s0, BR,
s1]`). BR is `w_ctl=8` (Q1) but here holds a `w_data` DATA byte (the rt operand).
Same zero-extend situation as s0: the cores touch only bits `0..w_data`, so BR's
high bits stay 0 and the round-trip is clean — **provided the gate wire offsets are
computed against the real field widths**. The POC slice (reg+imm only, no
Ccnot/Cswap) sidesteps this; the 3-operand cores are a later sub-slice with their
own equiv (Q6).

---

## Q3 — The `addressing::fields` / `on_fields_n` refactor (how invasive?)

**This is the central mechanical refactor.** Today the field-routing machinery
assumes every field is `word8()`:

- `addressing::fields(n)` = `n × word8()` (`addressing.rs:21`).
- `swap_field_adj`, `split_fields`, `route_to_head_n`, `at_fields_n` all build/route
  `word8()` fields and `SwapProd(word8(), word8())`/`AssocL/RProd` typed on
  `word8()` (`addressing.rs:33/59/89/119`).
- `decoder::fields_ty(k)` = `k × word8()`; `on_fields_n(field_idxs, gates,
  n_fields)` sets `n = k * W` and flattens `fields_ty(k)` (`decoder.rs:218/238`).
- The ALU cores build gate lists with wire indices `3 * w_data`, `pred3(w) = 2w`,
  `opd0(w) = w`, then hand them to `on_fields_l → on_fields_n` which flattens
  `3 × word8()` = 24 wires. **At `w_data = 8` the two agree; at `w_data = 4` the
  gate list is 12 wires wide but the flatten is 24 — a hard mismatch.**

### The clean representation: thread a per-field WIDTH LIST

`on_fields_n`/`at_fields_n` already take an explicit *field index list*; they need
to additionally know **each addressed field's width**. Introduce a typed field
descriptor threaded from the layout:

- `addressing::fields_typed(widths: &[usize]) -> Type` — right-nested product of
  `flat(widths[i])` (replaces `fields(n)`; `fields(n)` becomes
  `fields_typed(&[8; n])`, byte-identical).
- `swap_field_adj`/`split_fields`/`route_to_head_n` carry the **width of each field
  slot** so their `SwapProd`/`AssocProd` are typed on the actual `flat(w_i)`, not
  `word8()`. Mechanically: pass a `&[usize]` field-width vector alongside the index
  vector; the recursion consumes it head-first (exactly mirroring the current
  `fields(n-1)` recursion but with `flat(widths[0])` as the head type).
- `on_fields_n` computes `n = Σ widths[addressed]` (not `k*W`) and lays gates at the
  running offset `Σ widths[<j]` for field-slot `j` (not `W*j`). The ALU-core gate
  builders keep computing their *local* wire layout — but that local layout must
  match the addressed fields' widths: `[r0-slot=w_data, s0=w_ctl, s1=w_ctl]`, so
  the standard core's operand base is `w_data` (`opd0`), predicate base is
  `w_data + w_ctl` (NOT `2*w_data`), i.e. `pred3` becomes
  `w_data + w_ctl`. **This is the one place the current `pred3(w)=2w` /
  `opd0(w)=w` helpers are wrong at mixed width** and must derive the operand/pred
  offset from the *field* widths of `[r0-slot, s0, s1]`.

### Where the addressed field-widths come from

`on_fields_l(layout, field_idxs, gates)` can look up each `field_idx`'s width from
the layout: a helper `layout.field_width(idx) -> usize` returning `w_data` for a
register/`s2..` field and `w_ctl` for `s0/s1/PC/BR/IR`. Then `on_fields_l` builds
the width vector `field_idxs.map(|f| layout.field_width(f))` and passes it down.
**This centralizes the entire mixed-width decision in one `field_width` method** —
the rest of the refactor is threading a `&[usize]` through the four addressing
functions.

### Byte-identity at `w_data = 8`

**Yes, exactly byte-identical.** At `w_data = w_ctl = 8`, `field_width` returns 8
for every field, `fields_typed(&[8; n]) == fields(n)`, all `SwapProd/AssocProd` are
`word8()`-typed as today, `Σ widths = k*8 = k*W`, `pred3 = 8+8 = 2*w_data`. The
whole refactor collapses to the current uniform machinery. The byte-identity gate
(`poc_step_net_bit_identical`, the netlist fingerprint) is the guard.

**Invasiveness: MEDIUM-HIGH but bounded.** Roughly the four `addressing` functions
+ `fields_ty`/`on_fields_n` in decoder + the `pred3/opd0` offset helpers + one
`Layout::field_width` method. Every ALU core keeps its structure; only its
operand/pred base offsets change from `w_data`-multiples to real field offsets.
`ctrl_word_swap` (`w = w_data`) is CORRECT once its `on_fields_l` flattens the
right widths — but note it swaps `[ctrl_field, fa, fb]`; when used by the crossbar,
`fa`/`fb` are both `w_data` (Q2), so its `3*w`/`w`-offset gates are right; when used
by `fetch_rs_fmt_gated` to swap `operand_field(0)↔s0` it swaps a `w_data` field with
a `w_ctl` field — **a mixed-width `ctrl_word_swap`** (the r0-slot data word with the
8-bit s0). This is the one `ctrl_word_swap` call that crosses the boundary; its gate
loop `0..w` must be `0..min(w_data,w_ctl)=w_data` (only the low `w_data` bits carry
the operand) and its flatten must use the real `[w_ctl_ctrl, w_data, w_ctl]` widths.
Flag this call site explicitly for the reviewer.

---

## Q4 — The `w_data`-slot rotator

`gated_rotl_gates_w` (`decoder.rs:759`) rotates via `rot_transpositions(1<<k)` for
`k in 0..3` — a **3-bit shift amount over exactly 8 slots**, and
`rot_transpositions` (`plumbing.rs:159`) hardcodes `arr = 0..8` and `%8`. This is a
fixed 8-bit barrel rotator; it ignores its `_w` parameter entirely.

**Scope:** parameterize `rot_transpositions(s, width)` to rotate `width` slots
(`arr = 0..width`, `% width`), and drive `gated_rotl_gates_w` over `k in
0..log2(w_data)` with `ctrl(k)` for each shift bit. A `w_data=4` rotate uses 2
shift bits over 4 slots. Byte-identity: `rot_transpositions(s, 8)` == today's
`rot_transpositions(s)`, and `0..log2(8)=0..3` == today's `0..3`. **LOW risk,
self-contained** — a barrel rotate on `w` slots is a permutation, a bijection for
every `w`, and the whole-domain equiv at `2^w` (Q6) proves the specific rotation.
Note: the shift-amount now needs `log2(w_data)` control bits, so a `Rotli #k`
immediate's shift field must be masked to `log2(w_data)` bits — confirm the
assembler/immediate encoding does not feed a >`log2(w_data)` shift (the POC uses
add/xor only; the rotator is a follow-up sub-slice with its own equiv).

---

## Q5 — Deriving `w_data` per-program — DEFERRED (measure at manual `w_data`)

`derive_layout(instrs: &[Instr])` (`compile.rs:3069`) works off the **compiled,
byte-oriented ROM instructions** — it sees `Instr::Add{rd,rs}`, never the source
Iso's `Flat(n)` types. The data width lives in the SOURCE `Iso`
(`Type::Flat(u8)`, `ty.rs:35`, width 1..=8) BEFORE compilation; by the time
`derive_layout` runs, the width information is gone (every value is byte-marshaled).

Deriving `w_data` automatically would require threading the source program's
max `Flat(n)` width into the compile pipeline (a `compile`-time fact, plumbable but
a separate change touching the front end). **Recommendation: B-2.1c MANUALLY
specifies `w_data` for the measurement**, exactly paralleling B-2.1b's
`Layout{num_regs:4}` manual measurement (`b21b_reduced_regs.rs`:
`Layout{num_regs:4, ..default}`). The parallel is `Layout{w_data:4, w_ctl:8,
..default}`. **Auto-derivation is an explicit follow-up slice** (B-2.1c-derive):
thread the source Iso's `Flat`-width into `derive_layout` (or a new
`derive_layout_from_iso`). This keeps B-2.1c's risk on the datapath refactor, not
the front end.

---

## Q6 — Correctness authority at reduced `w_data`

The mixed-width netlist changes the ALU datapath itself (not just state plumbing,
as B-2.1b did), so it carries the **highest-bar discipline** (the daemon-equivalent
control-surface bar from CLAUDE.md):

1. **Byte-identity at `w_data = 8`** — the `poc_step_net_bit_identical` fingerprint
   test must stay green (the refactor is behavior-preserving at default). This is
   the primary regression guard for the 299-site mechanical change (R-1).
2. **Whole-domain per-op equivalence at the narrow `w_data`** — for a `w_data=4`
   layout, evaluate each ALU core (add/sub/xor/exch, then rotl/rotr, then
   ccnot/cswap in their sub-slices) over the **full `2^{w_data}` (or `2^{2·w_data}`
   for two-operand) domain** and check the core iso == the reference semantics mod
   `2^{w_data}`. Cheaper than `w=8` (2^4 vs 2^8), not harder. This is what proves a
   `w_data=4` netlist computes `Flat(4)` arithmetic correctly.
3. **Config-decode mutual-exclusion at `w_ctl=8`** — unchanged (config region is
   still 8-bit), but re-assert it composes with the narrowed cores (the one-hots in
   s1 still gate the `w_data`-wide cores).
4. **`run_on_cpu == eval` on a real `Flat(w_data)` program** across a real
   worktree — the end-to-end authority (the L15/L20 bar: a worker→gate→retry
   round-trip is not applicable here, but the *behavioral* end-to-end
   `run_rom_on_cpu_with(Layout{w_data:4,..}) == exec_instr == default-run` on the
   named registers is). Include edge bytes (0, all-ones-of-width, high bit set).
5. **A regression test reproducing the mixed-width mismatch** — the finding the
   refactor fixes: a `w_data=4` step where the old uniform `n = k*W` flatten would
   mis-offset the operand/pred wires (fails without the mixed-width offsets, passes
   with them). Per L20, the finding ships with its reproducing test.
6. **Fresh-context adversarial review, different model** — datapath + mixed-width
   field routing is a daemon-equivalent surface. The reviewer enumerates each field
   width, each core's operand/pred offset, the mixed-width `ctrl_word_swap` call
   site (Q3), and the crossbar-boundary argument (Q2), and tries to construct a
   width-misalignment.

**Correctness-vs-testing tension (from the B-2 design DF-4):** the ring-timing /
energy goldens (`CPU_BODY_N_DATA`, `d_pre`/`period`, `step_j`) are functions of the
field widths and become per-`w_data`. Prefer computed relations (`total == B·step_j`,
`period == d_pre+d_mem+d_post`) + a small pinned set, per the design recommendation.

---

## Slice plan (smallest-first, risk-tagged)

The unifying refactor: **mixed-width `state_ty_with` + a per-field width vector
threaded through `addressing`/`on_fields_n`, all byte-identical at `w_data=8`.**

- **B-2.1c-0 — mixed-width `Layout::field_width` + `state_ty_with`, DEFAULT-only.**
  Add `Layout::field_width(idx)` (`w_data` for reg/`s2..`, `w_ctl` for
  s0/s1/PC/BR/IR) and make `state_ty_with` build each field at its `field_width`.
  At default all widths are 8 → byte-identical. Thread `fields_typed`/width-vectors
  through `addressing::{fields,swap_field_adj,split_fields,route_to_head_n,
  at_fields_n}` and `decoder::{fields_ty,on_fields_n}`; fix `opd0/pred3` to derive
  from the addressed fields' widths. **Nothing shrinks.** *Gate:* full suite +
  `poc_step_net_bit_identical` byte-identical. *Risk:* **MEDIUM** (broad mechanical
  change, exact oracle) — the enabling refactor, no width change yet.

- **B-2.1c-1 (POC, the smallest real win) — `w_data=4`, register-only add/xor
  program, no rotator, no Ccnot/Cswap, no scratch operands, CPU-only.** Manually
  specify `Layout{w_data:4, w_ctl:8, num_regs:4, ..}`. Only the TK adder + XOR +
  exch cores are exercised (all `0..w_data` loops). No s2.. (so Q2's mixed-width
  scratch is untriggered), no BR-operand-B (so Q2-caveat untriggered), no rotator
  (Q4 untriggered). *Gate:* Q6.1 (byte-id at 8) + Q6.2 (whole-domain add/xor/exch
  at `2^4`) + Q6.4 (`run_on_cpu==eval` on a real 4-bit add/xor kernel) + Q6.5
  (mismatch regression) + Q6.6 (adversarial review). This is the **highest-bar
  slice** and delivers the headline ALU-halving win. *Risk:* **HIGHEST BAR.**

- **B-2.1c-2 — the `w_data`-slot rotator (Q4).** Parameterize `rot_transpositions`
  + `gated_rotl_gates_w`; add Rotl/Rotr at `w_data=4`. *Gate:* whole-domain rotate
  equiv at `2^4` + byte-id at 8. *Risk:* **LOW-MEDIUM** (self-contained
  permutation).

- **B-2.1c-3 — 3-operand cores + `w_data` scratch (Q2/Q2-caveat).** Ccnot/Cswap/
  Xchg at reduced `w_data` (BR/s2.. as `w_data` operand-B / operand space); enable
  the mixed-width crossbar for s2.. spill/ancilla programs. *Gate:* whole-domain
  ccnot/cswap equiv (`2^{2·w_data}`) + a scratch-operand `w_data=4` program +
  the crossbar-boundary type-safety argument (Q2). *Risk:* **MEDIUM-HIGH** (the
  mixed-width crossbar + BR-as-data-buffer).

- **B-2.1c-derive (follow-up) — auto-derive `w_data` from the source Iso's
  `Flat(n)`.** Thread the max `Flat` width into `derive_layout`. *Risk:* **MEDIUM**
  (front-end plumbing, independent of the datapath refactor).

**Recommended order:** 0 → 1 → (2 ∥ 3) → derive. Do NOT bundle (one-task-per-
delegation). B-2.1c-1 is the highest-bar slice and the headline win.

---

## Open questions / decision-forks (user is OFFLINE)

- **DF-2.1c-a (s2.. width — RESOLVED as design, confirm).** Recommendation: s2..
  are `w_data` (Q2 option a), matching registers so the crossbar stays within one
  width. The POC (slice-1) uses no scratch operands so this is only load-bearing
  from slice-3. *Confirm: uniform `w_data` operand space (a), not the "scratch
  operand pins w_data=8" fallback (b).*
- **DF-2.1c-b (auto-derive vs manual `w_data`).** Recommendation: manual for
  B-2.1c measurement (parallels B-2.1b `num_regs`), auto-derive a named follow-up.
  *Confirm deferral.*
- **DF-2.1c-c (BR as operand-B data buffer).** BR is `w_ctl=8` but holds a
  `w_data` rt operand in the 3-operand cores (Q2-caveat). Recommendation: keep BR
  `w_ctl` (zero-extend the operand, cores touch only `0..w_data`), do NOT split BR
  into a separate data buffer. *Confirm — this is a slice-3 concern, POC unaffected.*
- **DF-2.1c-d (goldens: relations vs pins).** Inherits B-2 DF-4. Recommendation:
  computed relations + small pinned set. *Confirm testing philosophy.*
- **Risk R-1 (the mechanical change).** The mixed-width offset refactor is the
  widen-PC "unwidened builder" hazard class: a missed site keeps `k*W`/`2*w_data`
  and desyncs the netlist at reduced `w_data`. Mitigation: byte-identity fingerprint
  at default + centralize all width logic in `Layout::field_width` + grep every
  `* W`, `* 8`, `3 * w`, `2 * w`, `pred3`, `opd0` in the tisc crate before shrinking.

---

## Appendix — files read (read-only, @ `4e5a4dc`)

- `docs/scratch/b2-per-program-widths-design.md` — B-2 design (DF-2 = Q3-A).
- `docs/reviews/b21b-reduced-regs-review.md` — B-2.1b review (F2 s0/s1 exclusion —
  now also the Q2 crossbar-boundary guarantor).
- `crates/theseus-tisc/tests/b21b_reduced_regs.rs` — the B-2.1b test pattern
  (manual reduced `Layout`, `encode_with`/`decode_with`, `run_rom_on_cpu_with`,
  whole-domain, byte-identity).
- `crates/theseus-tisc/src/types.rs` — `Layout` (`w_data`/`w_ctl`), `state_ty_with`
  (currently UNIFORM `data_word` — the field to make mixed-width), `data_word`,
  `flat`/`word8`, the `*_field` offset helpers, `encode_with`/`decode_with`.
- `crates/theseus-tisc/src/addressing.rs` — `fields`/`swap_field_adj`/`split_fields`/
  `route_to_head_n`/`at_fields_n` (the uniform-`word8()` field machinery — the
  central Q3 refactor).
- `crates/theseus-tisc/src/decoder.rs` — `fields_ty`/`on_fields_n` (`n = k*W`),
  `ctrl_word_swap` (`w=w_data`), `select_operand_l` (F2 position exclusion),
  `operand_field_l`, the ALU cores (`gated_tk/xor/rotl_gates_w`, `add/xor/exch/
  rotl_core_at`, `std_core` on `[r0-slot,s0,s1]`, `pred3/opd0/tgt0`),
  `gated_rotl_gates_w`+`rot_transpositions` (8-slot rotator, Q4), `shared_alu_block`,
  `alu_config_decode_families`/`cfg_wire`/`config_decode_fields` (already
  `w_ctl`-parameterized), `imm_to_a`/`fetch_rs_fmt_gated` (s0 as data operand).
- `crates/theseus-tisc/src/plumbing.rs` — `flatten` (leaf-count-driven, so
  mixed-width-safe once the Types carry the widths), `rot_transpositions`, `gate_at`.
- `crates/theseus-tisc/src/compile.rs` — `derive_layout` (works off `&[Instr]`, no
  `Flat` width — Q5 deferral).
- `crates/theseus-core/src/ty.rs` — `Type::Flat(u8)` (1..=8), the per-program data
  width source (in the SOURCE Iso, gone by compile time).
