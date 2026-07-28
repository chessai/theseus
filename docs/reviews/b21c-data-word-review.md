# B-2.1c POC (per-program reduced `w_data`) — adversarial review

**Reviewer:** fresh-context adversarial review (did not write the change).
**Scope:** `4e5a4dc..HEAD` on `wt-b21c` (`621cde0` mixed-width state, `ba8bb0f`
measurement; design `docs/scratch/b21c-data-word-design.md`, DF-2 = Option A
fixed `w_ctl = 8`). Read + reason; fast-tier tests only (`--lib`,
`b2_layout`, `a0_format`, `state_instr` — all green). Heavy tier not run
(constraint); the whole-domain w4 tests and byte-identity golden are taken as
having passed in the advisor's run, and their *coverage* is assessed below.

**VERDICT: FIX-THEN-SHIP** — no BLOCKER found in the wired reduced-`w_data`
path; three MAJORs (an unguarded stale legacy path, an untested-but-shipped
imm datapath, a silent non-power-of-two rotator degeneration) should be fixed
or explicitly guarded before merge.

---

## 1. What I verified (the attack list, path by path)

### 1.1 Mixed-width field alignment — the wired reduced path is consistent

The reduced-layout step is `step_with → decode_execute_l →
decode_execute_shared = shared_alu_block + xchg_instr + swapbr_instr`
(decoder.rs:2090–2115). I enumerated every field-routing site on that path at
`w_data=4, w_ctl=8, num_regs=4` and recomputed each wire base by hand:

| Site | Addressed fields (widths) | Bases (n) | Verdict |
|---|---|---|---|
| `pred_compute` (xchg/swapbr predicates) | [IR0..3, s1] (all 8) | 0,8,16,24,32 (40) | ✓ all-`w_ctl`, `pred_wire=4·wc` correct |
| `alu_config_decode(_families)` | [IR0..3, s1, s0] (all 8) | 8·j (48) | ✓ `cfg_wire(wc,k)=4·wc+k` lands in s1 |
| `select_operand_l` crossbar | pairs of DATA fields only | per-pair via `ctrl_word_swap` | ✓ see 1.3 |
| `fetch_rs_fmt_gated` swap | [IR_b3(8), r0(4), s0(8)] | 0,8,12 (20); swaps low min(4,8)=4 | ✓ see 1.2 |
| `imm_to_a` | [s0, IR_b0, IR_b3] (all 8) | 0,8,16 (24); `fmt_local=23`; masks j∈0..4 | ✓ |
| `std_bases` (add/sub/xor/exch/rotl/rotr cores) | [r0(4), s0(8), s1(8)] | tgt 0, opd 4, pred **12 = w_data+w_ctl** (28) | ✓ config one-hots k≤7 → wires 12..20 = s1 bits 0..7 |
| `ccnot_core_at`/`cswap_core_at` | [r0(4), s0(8), BR(8), s1(8)] | 0,4,12,20 (28) | ✓ (gated off in POC, build-consistent) |
| `shared_fetch_rt_br_gated`, `swapbr_core` | [r0(4), BR(8), s1(8)] | 0,4,12 (20) | ✓ |
| `xchg_instr` pad_copy / gsw | [IR_b0,s1] / [IR_b3, IR_b0, r0(4), s0(8)] | 0,8 / 0,8,16,20 (28) | ✓ `pred_copy_wire = gb[1]` (was `w` — would be wrong at w4, fixed) |
| step.rs fetch / pc_add_br / pc_inc / park | PC, IR, s0, s1 (all `w_ctl`) | `FetchWires` from `w_pc`/`w_ctl` | ✓ control-region only, unaffected by `w_data` |

`on_fields_l` (decoder.rs:258) now flattens
`fields_typed(addressed_widths)` with `n = Σ widths` and lifts via
`at_fields_typed(core, idxs, cpu_field_widths())`; `field_bases` gives the
running offsets. `addressing::{fields_typed, swap_field_adj_typed,
route_to_head_typed, split_fields_typed, at_fields_typed}` thread the width
vector correctly — `route_to_head_typed` swaps the live width vector in
lockstep with the index array and returns the permuted widths, which
`at_fields_typed` uses for the addressed/rest split. I found no site on the
wired path that still assumes `word8()` or `k·w_data`.

`plumbing::flatten` is leaf-count-driven (`leaf_count`), so it is
mixed-width-safe by construction; `rot_transpositions(s, 8)` reproduces the
old rotator exactly (`s % 8 == s` for s ∈ {1,2,4}).

### 1.2 The r0↔s0 boundary (`fetch_rs_fmt_gated`)

- **Well-typedness:** no field-level `SwapProd(flat(4), flat(8))` is ever
  constructed. `ctrl_word_swap` builds per-bit Fredkins at real running
  offsets (`c0+ctrl_bit, a0+i, b0+i` for `i < min(wa,wb)`), inside a flatten
  of the true `[8,4,8]` product. The width boundary is crossed bit-wise, not
  field-wise — the type problem the design flagged is avoided structurally.
- **Zero-extension in:** s0 = 0 at swap time (decode_config is OUTERMOST and
  restores s0 to 0 on its forward pass before fetch_rs; bracket comment
  decoder.rs:1560–1580). After the swap, s0 = rs value in bits 0..4, bits
  4..8 = 0. Every consumer loops `0..w_data` on the operand base
  (`gated_tk/xor_gates_w`, exch, ccnot/cswap cores) and the rotator's shift
  controls are `opd0..opd0+log2(w)` ⊂ low nibble — s0's high bits are never
  read or written by any reduced-path core.
- **Restored on exit:** the bracket is a conjugation (`sel ; swap ; sel⁻¹`
  inside `sym`-matched pairs), and `run_rom_on_cpu_with` *hard-fails* if
  terminal s0/s1/BR ≠ 0 (compile.rs:4576) — the passing w4 whole-domain runs
  are real evidence the round-trip restores s0.
- **Dirty-high-bit leak:** with the swap confined to the low 4 bits, a dirty
  s0 high bit *cannot* enter a register (registers have only 4 wires) and no
  core reads it. Behavior at `w_data=8` is the old full-byte swap
  (min(8,8)=8), byte-identical.

### 1.3 s0/s1 immovability + B-1 config at `w_data=4`

- Config decode operates on the full 8-bit s1 via `[IR×4, s1, s0]`, all
  `w_ctl` fields, `n = 6·w_ctl` — no dependence on `w_data`. One-hots land at
  s1 bits 0..7 (`cfg_wire`), and the shared cores read them at
  `pred0 + k = 12 + k` (verified against `std_bases`).
- Crossbar exclusion at `num_regs=4`: s0/s1 are operand positions 4/5;
  I enumerated all four levels of `select_operand_l` at n_reg=4, n_scr=8:
  level-3 pairs (0,8)..(3,11) touch s4..s7 only; level-2 pairs (0,4),(1,5)
  are skipped by the position test, (2,6),(3,7) are r2↔s2, r3↔s3; levels
  1/0 are register-register. **Every fired crossbar swap is
  `w_data`↔`w_data`** — the operand crossbar never crosses the width
  boundary, exactly the design's Q2 resolution.
- `xchg_instr`'s scratch-side `select_fields` tree *does* move s0/s1
  (positions 0/1 of the scratch base) with mixed-width partial swaps
  (e.g. s1(8)↔s3(4) swaps low nibbles only). I traced sx=3 end-to-end: the
  operand value always travels in the low `w_data` bits along the route, the
  gsw touches only position-0 low bits, and the conjugation restores s0/s1
  exactly — same semantics as the default layout restricted to the low
  nibble. Not a bug, but this partial-swap routing is subtle and undocumented
  at the `select_fields` call site (MINOR-5).

### 1.4 Byte-identity at `w_data=8` is real

- `word8() = flat(W)` **exactly** (types.rs:396), so
  `fields_typed(&[8; n])` is node-for-node the old `fields(n)`
  (`Prod(flat8, ...)` right-nested). `swap_field_adj_typed`,
  `route_to_head_typed`, `split_fields_typed`, `at_fields_typed` produce
  identical `SwapProd/Assoc*Prod` trees at uniform 8 — I diffed the
  recursions by hand; the uniform entry points are thin `[8; n]` wrappers.
- `on_fields_l` at default: `addressed_widths = [8; k]`,
  `fields_typed == fields_ty(k)`, `n = 8k`, `cpu_field_widths() = [8; 24]`,
  `num_fields() == num_cpu_fields()` — identical construction to the old
  `on_fields_n(idxs, gates, num_fields())`.
- The guard is not sampled: `poc_step_net_bit_identical_across_type_repr`
  (energy.rs:107) FNV-hashes the `Debug` of the **entire lowered step net**
  (every cell kind/depth/phase/rails, boundary Types, num_lines) against a
  pinned golden (3290 cells / 19956 lines / fp 0x8249…), and the golden was
  **not re-pinned in this diff** — the refactor produced the identical net.

### 1.5 pred3/opd0 offsets

`tgt0/opd0/pred3` are gone; every core derives bases from
`field_bases`/`std_bases`. At w4 the predicate base is 12 = `w_data + w_ctl`
(not 8 = `2·w_data`, which would have put every config control on **s0 bit
0–3 — swept operand bits**, so the 256-pair whole-domain Add/Xor would have
failed loudly). The shipped regression test
(`pred_base_is_w_data_plus_w_ctl_not_2_w_data`) pins the arithmetic relation;
the *behavioral* reproducer is the whole-domain Add itself (fails without the
fix) — together that satisfies the L20 bar, though the pin test alone would
not.

### 1.6 StateVals encode/decode at `w_data<8`

`encode_with` **rejects** (asserts, loudly) any DATA word ≥ 2^w_data rather
than silently truncating; pushes exactly `field_width` bits per field in
field order; `decode_with` reads back at `Σ field_width(<idx)` offsets with
the same widths — exact inverse by construction (same order, same widths,
LSB-first both ways). PC/BR/IR marshal as full 8-bit control words. s0/s1
8-bit, s2.. `w_data` — matches `field_width`. ✓

---

## 2. Findings

### MAJOR-1 — the legacy per-instruction path is stale at reduced `w_data` and UNGUARDED

`instruction_l(layout, op)` (decoder.rs:1451) is `pub` and
layout-parameterized, but at `w_data<8` it builds **misaligned/ill-typed**
isos:

- `fetch_operand` (decoder.rs:842) lifts `Iso::SwapProd(word8(), word8())`
  via uniform `at_fields(..)` (= `[8; NUM_FIELDS=24]`) — wrong widths AND
  wrong field count at a reduced layout. It brackets `with_two_regs`, i.e.
  **every** reg-reg legacy instr (add/sub/xor/exch/rotl/rotr/ccnot/cswap).
- `ccnot_core` (decoder.rs:1110) still lays gates at `4·w / 3·w / 2·w / w`
  against a `[4,8,8,8]` flatten (n=16 gates in a 28-wire core).
- `addi_core` (1295: `n=4w, pred=3w, imm=2w+j`), `xori_core` (1338:
  `n=3w, pred=2w`), `rotli_core` (1364) — same class. Ironically
  `cswap_instr`'s inner cores WERE width-fixed while its `with_two_regs`
  bracket was not — a half-updated path is worse than an untouched one
  because it looks done.

**Trigger:** any call to `instruction_l`/`reg_reg`-family with a
`w_data<8` layout — e.g. the natural per-op equivalence tests of the c-2
follow-up, or a future `decode_execute_ops`-style subset harness. Today all
callers pass `Layout::default()` (verified by grep), so this is NOT reachable
in the POC — hence MAJOR, not BLOCKER. Lowering would fail loudly
(`canonicalize` re-derives types), but `eval` is structural and may not — the
failure mode is not guaranteed loud.

**Fix:** either width-fix these five sites (mechanical, same `field_bases`
pattern) or add `assert_eq!(layout.w_data, layout.w_ctl, "legacy per-instr
path is uniform-width only")` at the top of `instruction_l` and
`fetch_operand`. One line buys the invariant.

### MAJOR-2 — the I-format (imm) datapath at `w_data=4` is wired but has ZERO test coverage

`imm_to_a` was refactored (new bases, new `0..wd` mask loop) and **is live in
`shared_alu_block`** — but it is ¬fmt-gated, and both whole-domain tests and
the kernel test use only R-format Add/Xor (fmt=1 → `imm_to_a` is identity).
The POC's own header says "reg+imm add/xor"; the imm half is shipped
untested at the reduced width. My reading says the bases are correct
(all-`w_ctl` fields, 8j offsets, fmt_local=23), but per the project's L15
lesson, "correct by reading" is exactly the bar this repo rejects for
datapath changes.

**Trigger:** a mis-based `imm_to_a` (e.g. imm read from wire 8+j of the
wrong field) corrupts every `Addi/Subi/Xori` at `w_data<8` and nothing in the
suite fires. **Fix:** add `whole_domain_addi_at_w_data_4` and
`whole_domain_xori_at_w_data_4` (16 regs × 16 imms = 256 runs each, same cost
as the existing sweeps) to `b21c_data_word.rs` before merge.

### MAJOR-3 — `gated_rotl_gates_w` silently degenerates for non-power-of-two `w_data`

`shift_bits = w.trailing_zeros()` (decoder.rs:806): at `w=5` this is **0** —
the rotator emits *no gates* (Rotl/Rotli become identity); at `w=6` it is 1
(only rotate-by-1 reachable). No assert anywhere; `Layout` has no `w_data`
validation at all (only `encode_with`'s `≤ 8`). The rotator is built into
`shared_alu_block` for EVERY layout, so a `w_data=5` layout (a `Flat(5)`
program — theseus `Type::Flat(u8)` allows 1..=8, and the planned
B-2.1c-derive would derive w_data = max Flat width) ships a well-formed
netlist whose Rotl is silently wrong. Also `w_data=1` panics obscurely in
`gated_tk_gates_w` (`a(w-2)` underflow).

**Trigger:** any future layout with `w_data ∉ {2,4,8}` — most plausibly the
auto-derive follow-up hitting a `Flat(5)`/`Flat(6)` source program.
**Fix:** `assert!(w.is_power_of_two() && w >= 2)` in `gated_rotl_gates_w`
(or a `Layout::validate` enforcing `w_data ∈ {2,4,8}` and
`w_data ≤ w_ctl`), so the derive slice must consciously round up.

### MINOR-4 — whole-domain w4 coverage stops at Add/Xor

Sub (the `CFG_ADD_REV` adjoint — a *distinct* config line and gate ordering),
Exch, and the config-gated inertness of the rotator are unverified at
`w_data=4`. The 256-pair Add/Xor sweep does prove that no *other* core's
control landed on a swept operand wire (all cores physically execute every
step), but a control misplaced onto an **always-zero high bit of s0/s1**
makes a core silently never fire — indistinguishable from correctly-gated-off
for the ops the sweep doesn't check. My manual base check says they're
correct; c-2 must whole-domain Sub/Exch (and the rotator) at w4.

### MINOR-5 — composite/bus path has no reduced-width guard, and `field_width` over-promises

`cross_cnot`/`cross_toffoli`/`cross_factor_gated_swap`/`select_mem_on_bus`
and `instr.rs` remain uniformly `W`-based (fine — `system_decode_execute`
hardcodes `Layout::default()` and `run_rom_on_cpu_with` asserts CPU-only),
but nothing *in those builders* asserts default-width, and
`Layout::field_width` already answers `w_data` for mem fields — a claim no
composite code honors yet. Add a `debug_assert` (or comment) so the composite
slice can't partially inherit the mixed width. Also: the mixed-width
partial-swap behavior of `select_fields` (1.3) deserves a comment at its
definition.

### MINOR-6 — imm bits ≥ `w_data` are silently ignored; no asm-level width check

`imm_to_a` masks only bits `0..w_data`. For Add/Sub, mod-2^w arithmetic makes
`imm` and `imm mod 2^w` equivalent; for Xori the 8-bit reference semantics
(`exec_instr` on u8) would diverge from the CPU for `imm ≥ 2^w_data` — caught
only later by `encode_with`'s range assert (loud but confusing). The
assembler/derive slice should reject `imm ≥ 2^w_data` for reduced layouts.

---

## 3. What the POC leaves unverified (for c-2/c-3, as scoped)

- Rotator semantics at reduced width (built, gated off, never fires in POC;
  plus MAJOR-3's pow-2 constraint and the `Rotli` shift-field masking to
  `log2(w_data)` bits).
- Ccnot/Cswap at reduced width (cores are base-correct by reading and
  build-consistent, but no whole-domain 2^{2w} equiv; BR-as-operand-B
  zero-extension untested under fire).
- Scratch operands s2.. at `w_data` (crossbar descent levels never fire in
  the POC tests — register operands keep bits 2/3 = 0), Xchg with a scratch
  target at w4, and the s2.. encode/decode DATA-width marshaling under a
  real spill program.
- Sub/Subi/Exch at w4 (MINOR-4), the imm path (MAJOR-2 — should not wait
  for c-2).
- Ring/energy goldens at reduced `w_data` (DF-4 relations-vs-pins), and the
  `decode_execute` cell measurement's numbers are printed but not pinned
  (fine for a measurement test).

## 4. Verdict

**FIX-THEN-SHIP.** No blocker: the wired reduced-`w_data` path
(shared_alu_block + xchg + swapbr + step/fetch/park + encode/decode) is
mixed-width-consistent at every site I could enumerate, the r0↔s0 boundary is
structurally safe (bit-level Fredkins, no cross-width `SwapProd`), the s0/s1
exclusion holds at `num_regs=4`, and byte-identity at the default is
structural (`word8()==flat(8)`, wrapper-exact) *and* pinned by an unchanged
full-net fingerprint. Confidence: **medium-high** — grounded in a full
call-path enumeration plus the (advisor-run) 2^8 whole-domain sweeps and the
end-to-end w4 kernel, tempered by the untested imm path (MAJOR-2).

Before merge: (1) guard or fix the legacy `instruction_l` path (MAJOR-1);
(2) add whole-domain Addi/Xori at w4 (MAJOR-2); (3) assert power-of-two
`w_data` in the rotator or validate `Layout` (MAJOR-3). All three are small,
none disturbs the shipped netlist at default (goldens stay).

*Fast-tier verification run for this review:* `cargo test -p theseus-tisc
--lib --test b2_layout --test a0_format --test state_instr` — all green
(crossbar roundtrip identity, reduced-layout state_ty well-formedness,
default-layout per-instr semantics).
