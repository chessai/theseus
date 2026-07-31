# W_BR widen (two-byte come-from δu) — design (advisor, 2026-07-31)

**Status:** advisor design from a read-only survey of theseus-v2 `fd4cfd7` (after
the variable-T `vart_halve` merge). Scopes lifting the `iter_len ≤ 128` /
`skip_distance ≤ 128` cap so wider variable-T loops and forward-skip blocks lower.
Mirrors the just-completed B-2.2 (per-program PC width) work almost exactly — BR is
to `iter_len` what PC was to `B`.

## Why (the gap, from the variable-T probes)

The come-from backward-jump offset `δu = 256 − iter_len` is minted into an **8-bit
BR** and SIGN-EXTENDED into the PC (`W_BR = 8`). A backward jump needs δu's top bit
set ⇒ `iter_len ≤ 128` (`compile.rs:4033`). The twin forward-skip guard
(`compile.rs:1020`) caps a skip block's `body_instrs.len() ≤ 128` for the same
reason (a larger forward skip sign-extends to a backward jump). So a loop body or
skip block spanning > 128 ROM instructions cannot lower. `vart_halve`'s reject test
pins this exact boundary (K=4,HB=1 → iter_len 133 > 128, rejected). Lifting it needs
BR to hold a **two-byte δu** (W_BR = 16), sign-extended from 16 bits.

NB: this caps only a SINGLE loop/skip's ROM SPAN, not total B (widen-PC already
handles B > 256). So W_BR is orthogonal to widen-PC: it widens the *offset register*,
not the PC.

## The B-2.2 parallel (this is BR's B-2.2)

`num_cpu_fields()` (types.rs:147) ALREADY has a `(br_fields() - 1)` term, and
`br_fields()` (types.rs:133) = `w_br / w_ctl` — exactly the `pc_fields()` shape that
existed before B-2.2a. So the field-count infra half-anticipates `br_fields() > 1`;
what's baked to "BR = exactly 1 field / 8 bits" is the ADDRESSING + the come-from
arithmetic. Decompose exactly like B-2.2:

### W_BR-a — enabling refactor (byte-identical at W_BR=8), LOW risk
Generalize every site that assumes BR occupies one `w_ctl` field / 8 bits to be
`br_fields()` / `layout.w_br`-driven, so a hand-built W_BR=16 layout type-checks +
lowers, while the default (W_BR=8) path stays BYTE-IDENTICAL (the
`poc_step_net_bit_identical` golden green, no re-pin). Sites (from the survey):
- **types.rs marshalling:** `decode_with` (`:724`) reads BR as one byte
  (`read_u8_bits(.., br_field())`); `encode_with` writes one byte. Must push/read
  `br_fields()` bytes (like the PC_hi branch B-2.2a added). The default-path
  `encode()`/`decode()`/`BR_FIELD` const (`:507,:759,:860`) stay 8-bit (untouched,
  like PC).
- **decoder.rs (~10 sites):** every `layout.br_field()` in the Swapbr / come-from /
  branch arms (`:1065,1096,1102,1127,1279,1284,2023,2036,2062,2075`) addresses BR as
  one field. Audit each: which need all `br_fields()` BR fields (the ones that
  read/write the FULL δu offset — the come-from) vs which only touch BR bit 0 (a
  predicate). The offset-carrying arms must lift over `br_fields()` fields.
- **step.rs `pc_add_br_with` (`:522`):** the sext of BR into PC. At W_BR=16 BR is
  already 16-bit → sext is (partly) identity; the `br(j)` operand closure must span
  `br_fields()` words (mirror the pc/op closures B-2.2a made pcf-driven). **This is
  the delicate one — it is come-from arithmetic.**
- **instr.rs `Swapbr` (`:483`):** `at_fields(swap_words_iso(), &[reg_field(rd),
  BR_FIELD])` swaps ONE reg word with ONE BR word. At W_BR=16 a Swapbr must swap the
  reg with `br_fields()` BR words (or the semantics change). Audit: does Swapbr
  swap the whole BR, or just the low byte? (The δu mint writes the offset via
  Addi+Swapbr — so Swapbr must move the FULL offset.)
- **A guard like `assert_br_carrier`** (mirror `assert_pc_carrier`, B-2.2a MAJOR-1):
  `w_br == br_fields()*w_ctl && br_fields() >= 1` at the BR-builder entries, so a
  non-byte-multiple W_BR fails loudly instead of aliasing.
**Gate:** byte-identity golden green (no re-pin) + a new unit test: a hand-built
W_BR=16 layout type-checks + `br_fields()==2` + encode/decode round-trips a 16-bit BR.
Tests-only-adjacent to src; the byte-identity golden is the guard (like B-2.2a).

### W_BR-b — prove W_BR=16 come-from is RUN-correct, HIGHEST BAR
The two-byte δu mint + sext + Swapbr-of-2-field-BR must produce a correct backward
jump for `iter_len > 128`. `δu = 2^W_BR − iter_len` (16-bit, top bit set for
iter_len ≤ 2^15), sext(δu) = −iter_len mod 2^W_PC. Prove:
- exhaustive/`ADR-0034`-style: `(pc_add_br_with(&w_br16); pc_inc_with)` implements
  `PC + sext16(BR) + 1` for the relevant BR range (mirror B-2.2b's k=8 proof at the
  BR width);
- a REAL `run_on_cpu == eval` on a variable-T program whose iter_len > 128 (e.g.
  `vart_halve` at K=4,HB=1 — currently the reject case — should now LOWER + RUN),
  terminal-quiescent. **This is a `lower_trace` + decoder + come-from change: full
  L19/L20/L21 — fresh-context Fable design review that RE-DERIVES the δu arithmetic,
  Fable impl review, reproducing real-worktree test. Do NOT rush.**
**Interaction with B-2.2:** `pc_add_br`'s BR operand is now `br_fields()`-wide AND
the PC is `pc_fields()`-wide — the two field-count generalizations compose in the
same builder. W_BR-a must not disturb the pcf logic; W_BR-b's proof runs at
(w_pc=16, w_br=16).

### W_BR-c — derive/enable W_BR=16 for loops that need it + measure
A loop/skip with span > 128 auto-selects W_BR=16 (via the derive path, mirroring
`derive_w_pc`); small loops keep W_BR=8 (byte-identical, and CHEAPER — a 2-field BR
grows the state, so W_BR=8 stays the default for the common case). Update the two
caps (`:1020`,`:4033`) to `1 << (layout.w_br - 1)`. Energy: a 2-field BR grows the
step net slightly — measure the cost so the derive keeps W_BR=8 unless the span
demands 16. Reject-boundary test (`vart_halve` K=4) flips from reject→run.

## Sequencing + discipline

Do **W_BR-a first** (low risk, byte-identity-guarded, safe to land autonomously).
**W_BR-b is the highest-bar sub-slice** (come-from arithmetic — the most dangerous
code in the compiler; a sign-extension off-by-one silently mis-jumps every wide
loop) and gets its OWN fresh design + Fable adversarial review that re-derives the
δu math + a real run==eval on a >128-span loop. W_BR-c derives + measures. Each is
one delegable slice (do not bundle). Update `vart_halve`'s reject test in W_BR-c
(its pinned boundary moves).

## Risk note

The come-from δu arithmetic is the single most safety-critical code here: it is what
makes a reversible loop actually loop. B-2.2b already proved the PC-side sext at
w_pc=8; W_BR-b is the symmetric BR-side proof at w_br=16. The byte-identity golden +
`assert_br_carrier` + the reject→run boundary test are the guardrails.
