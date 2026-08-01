# Adversarial review — W_BR-a (byte-identical BR-field refactor)

**Commit reviewed:** `fc1bc19` (`wbr-a-refactor`, off theseus-v2 `fd4cfd7`).
**Reviewer:** fresh-context Fable (NO cargo — read-only reasoning against the runtime
code). Implementer: Opus.
**Verdict:** **SHIP** — no BLOCKER/MAJOR. Four MINORs (three fixed below, one a
W_BR-b checklist item).

## Scope

W_BR-a is the byte-identical enabling refactor for widening BR (the branch/come-from
offset register) — BR's analogue of B-2.2a. Generalizes ~15 BR-addressing sites to
`br_fields()`-driven so a hand-built w_br=16 layout type-checks + lowers, while the
default (w_br=8) path stays byte-identical. The come-from δu two-byte mint, w_br=16
run-correctness, and the runtime `swapbr_core` full-offset swap are OUT of scope
(deferred to W_BR-b).

## The elevated risk (u8→u16 `StateVals.br`) — SOUND

The refactor widened `StateVals.br` u8→u16, rippling through the interpreter +
several merged reference models as `as u8` / `u16::from` casts. The byte-identity
golden covers the lowered net, NOT the interpreter, so Fable audited every BR cast:
- `StateVals.br` is only ever assigned from struct-literal `0`, `u16::from(rv: u8)`,
  or `decode*`'s single 8-bit BR field. **No production path sets `w_br`** (both
  `derive_full_layout*` inherit `..base` = 8), so `br ≤ 0xFF` is a GLOBAL invariant
  on every run path → every `state.br as u8` (compile.rs interpreter + bveq_netlist)
  is value-preserving and `pc_update`'s `as i8 as i16` sext is identical to before.
- Test-side: every touched comparison is now u16-vs-`u16::from(u8)` — full-width and
  STRICTER than before (a stray high byte would now FAIL, not be masked). No masking
  compare found in the four rewritten reference models.

## Byte-identity at br_fields()==1 — HOLDS by substitution

`pc_add_br_with` at (pcf=2, brf=1, wc=w_br=8): field list `[pc, pc_hi, br_field, s0,
s1]`, `n=40`, `br(j)=16+j`, `op(i)=24+i`, sext boundary moved `w_ctl`→`w_br` is
IDENTICAL when w_br==w_ctl — every term reduces to the shipped literal (golden
corroborates, no re-pin). `swapbr_with(default)` = `swapbr()`'s one SwapProd.
`encode_with`/`decode_with` push/read zero hi-words at brf=1. `num_cpu_fields()`
already had the `(br_fields()-1)` term at fd4cfd7 → default count unchanged; at
brf=2 BR_hi sits after PC_hi and encode/decode derive from the same helpers so
cannot skew.

## Deferred boundary — CLEAN

Runtime `swapbr_core` left low-word-only (correct — widening it without the δu mint
would corrupt the offset). Nothing on the w_br=8 path touches a 2-field BR; no
production layout reaches brf=2; `swapbr_with` has zero production callers.

## Findings

- **MINOR-1 (FIXED) — `swapbr_with` comment was a W_BR-b trap.** It claimed a
  "self-inverse chain" moving the full offset, but at br_fields()>1 the sequential
  `swap(reg,BR₀);swap(reg,BR₁)` is a **3-cycle** — NOT an involution, NOT a correct
  two-byte swap (an 8-bit reg can't hold a 16-bit offset). Pendulum SWAPBR
  self-inverseness (reverse execution relies on it) is violated for br_fields()>1.
  Harmless now (test-only), but a loaded trap. **Fixed:** rewrote the doc to mark it
  ADDRESSING-ONLY with a ⚠️ that W_BR-b must redesign the reversible two-byte move
  from scratch and never reuse this chain on a run path.
- **MINOR-2 (FIXED) — `assert_br_carrier` not called from `swapbr_with`.** Added the
  call (made `assert_br_carrier` `pub(crate)`) — defense-in-depth, mirrors
  `pc_add_br_with`; loud on a non-`w_ctl`-multiple w_br.
- **MINOR-3 (FIXED) — truncation-guard inconsistency.** `encode_with` hard-asserted
  `br>>8==0` but `encode_composite`/`encode` only `debug_assert`ed. Promoted both to
  hard `assert_eq!` (off the hot bit loop) — loud failure in release too once W_BR-b
  lets wide BR values into the model, instead of a silent `as u8` truncation.
  Validated safe: the debug-profile authority run exercised the `debug_assert` and it
  never fired (confirming br≤0xFF), so the hard assert never fires at w_br=8.
- **MINOR-4 (noted, W_BR-b checklist) — interpreter `Swapbr` (`compile.rs`)
  `operand_set(rd, state.br as u8)`** will truncate once W_BR-b admits wide BR. Already
  flagged by the in-code comment; on the W_BR-b checklist.

## Guardrails carried into W_BR-b

The come-from δu two-byte mint is the single most safety-critical code in the
compiler. W_BR-b gets its OWN fresh design + adversarial review that RE-DERIVES the
sign-extension arithmetic + a real `run_on_cpu == eval` on an iter_len>128 loop
(`vart_halve` K=4 flips reject→run). The byte-identity golden, `assert_br_carrier`,
the MINOR-3 hard asserts, and the MINOR-1 ⚠️ are the guardrails.
