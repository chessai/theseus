# Adversarial review — B-2.2a (per-program PC-field generalization)

**Commit reviewed:** `21bf607` (`b22a-pc-fields`, off theseus-v2 `96f40fb`).
**Reviewer:** fresh-context Fable (did not write the change); implementer was Opus.
**Verdict:** **FIX-THEN-SHIP** — one MAJOR (a one-line defensive assert), rest
doc MINORs. Byte-identity at w_pc=16 confirmed both by per-site substitution and
by a live golden run; the pcf=1 wire math is correct and alias-free.

## Scope

B-2.2a generalizes the six sites that baked in "PC = exactly 2 fields = 16 bits"
so they are driven by `layout.pc_fields()` (= `w_pc/w_ctl`; 2 at default w_pc=16,
1 at w_pc=8) and `layout.w_pc`: `FetchWires::new` (wire bases
s0/s1/n = `(pcf+4)/(pcf+5)/(pcf+6)·wc`), `fetch_fields` (`[usize;8]` → `Vec`,
drops PC_hi at pcf<2), `fetch_addr_bits_with` (clamps to a passed `w_pc`),
`pc_add_br_with`/`pc_inc_with` (field lists + wire-count `n` + `br`/`op` closures
keyed off `pcf`), `pc_byte_field` (bound off `Layout::default().pc_fields()`),
`encode_with`/`decode_with` (push/read PC_hi only when `pc_fields()>=2`). Nothing
derives w_pc=8 in the run path yet — pure enabling refactor; w_pc=8 run
correctness is B-2.2b.

## What was verified (attack surface, empirically + by wire trace)

1. **Byte-identity at pcf=2 — HOLDS.** Every changed wire-base/closure/field-list
   reduces exactly to the shipped literal at pcf=2 (FetchWires s0=6wc/s1=7wc/n=8wc;
   pc_add_br n=5wc, br=2wc, op=3wc; pc_inc n=4wc, op=2wc; identical field lists;
   fetch_addr_bits delegates at W_PC; marshalling takes the pcf≥2 branch). Live:
   `poc_step_net_bit_identical_across_type_repr` green at the exact pins
   (cells=3290, lines=19956, fp=0x8249a031c2ff5b6e) — **no re-pin**.
2. **w_pc=8 wire aliasing/OOB — CLEAN.** pc_add_br at pcf=1: PC 0..8, BR 8..16,
   op 16..24 (fits in s0; max wire 23 < n=32). pc_inc: PC 0..8, op 8..16. TK adder
   is zero-ancilla so s1's loss of the operand-high role is moot (no gate touches
   s1 at pcf=1). FetchWires at pcf=1: n=56, ir0=8, IR 8..40, s0 40..48, s1 48..56,
   pred=48, hz=55 — all live, no alias.
3. **fetch_fields/FetchWires lockstep — HOLDS** for pcf∈{1,2}; `on_fields_l`
   independently recomputes n from addressed widths, so a mismatch fails loudly at
   `type_of`. All four consumers pass `&fetch_fields(layout)` straight through; the
   `[usize;8]`→`Vec` change has no other length/indexing assumption.
4. **encode/decode round-trip — HOLDS.** PC_hi is the last CPU field, so omitting
   it at pcf=1 shifts no earlier offset; exact inverses at both pcf values.
5. **Default (non-`_with`) path — UNTOUCHED.** `encode()`/`decode()`/`read_pc_bits`/
   `PC_HI_FIELD` unchanged; `pc_byte_field`'s new assert bound = 2 for every
   (zero) caller.
6. **`pc_hi_field()` stragglers — NONE** outside the new `pcf>=2` guards / the
   default-const path.

## Findings

- **MAJOR-1 — non-byte-multiple `w_pc` silently corrupts (vs old loud panic).**
  `pc_fields()` is floor division with no Layout validation. A hand-built
  `Layout { w_pc: 12, .. }` gives pc_fields()=1 but np=12: pc_add_br target bits
  8..11 = BR wires (writes into BR); pc_inc aliases pc(8..11) with op(0..3);
  FetchWires.ir0=12 ≠ pcf·wc=8. All wire indices in-range and field widths sum to
  n, so `on_fields_l`/`type_of` PASS — silent corruption. Pre-refactor this
  panicked loudly. Not a BLOCKER (only a hand-built layout; B-2.2c derives only
  8/16), but it is the R-1 silent-misroute pattern on the highest-bar surface.
  **RESOLVED (`5738ff6`):** `assert_pc_carrier(layout)` — `w_pc ==
  pc_fields()*w_ctl && pc_fields() >= 1` — at all three PC-builder entries;
  restores loud failure. L20 regressions:
  `major1_non_byte_multiple_w_pc_panics_{add,inc}` (w_pc=12 panics),
  `valid_w_pc8_builders_do_not_panic` (guard not over-eager).
- **MINOR-2 — `pc_fields()` doc said ⌈⌉, impl is floor.** RESOLVED (`5738ff6`):
  doc corrected + points at the `assert_pc_carrier` invariant.
- **MINOR-3 — stale docs** (FetchWires struct doc, module comment, pc_inc op
  comment). FetchWires struct doc updated (`5738ff6`); the pc_inc `op` comment is
  accurate for the default (16-bit op spans s0‖s1) so left as-is.
- **MINOR-4 — at pcf=1 both adders still lift over idle s1** (zero-ancilla; op
  fits in s0). Harmless identity; DEFERRED to B-2.2b (drop s1 from pcf=1 field
  lists when the w_pc=8 net is actually measured — free cells).
- **MINOR-5 — pcf≥3 (w_pc=24) fails as an opaque type error.** The MAJOR-1 assert
  now makes it explicit (w_pc=24 is a valid multiple, pcf=3 — the assert passes;
  the carrier is genuinely 3 PC fields, which the current builders push only one
  hi field for, so it would still under-fill. B-2.2 only targets {8,16}; a >16
  w_pc is out of scope and would need its own slice. Noted.)
- **MINOR-6 (process) — the byte-identity gate is `#![cfg(feature =
  "heavy-tests")]`.** `cargo test --test energy poc_step_net_bit_identical`
  WITHOUT `--features heavy-tests` runs 0 tests (vacuous green). The mechanical
  gate's `check_command` MUST include the feature, or the primary guard proves
  nothing. Advisor authority runs always pass `--features heavy-tests`.

## Note

`w_pc8_step_type_checks` proves composition well-typedness only — `type_of`
cannot see intra-word wire aliasing (MAJOR-1 demonstrated this). Exhaustive
w_pc=8 run-correctness (`(pc_add_br;pc_inc) == +sext(BR)+1 mod 2^8`) is correctly
deferred to B-2.2b and must not slip.
