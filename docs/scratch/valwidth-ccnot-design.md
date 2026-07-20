# Value-width slice-4 (final rung) — **byte-parallel `Ccnot`** + masked accumulate

> **Revision r2 — Fable design review** (`docs/reviews/valwidth-ccnot-design-review.md`) BUILT the
> change under both buffer conventions + ran the full suite. **DECISION: convention (B) BR closes**
> (validate()==Ok ×3 ROM classes, real system-ring ancilla/bus clean, PC+=BR+1 exact, dense-byte AND
> correct). (A) s2 equal-cost but B wins on hygiene; the design's "s2 reservation shrinks scratch"
> cost was WRONG (swap-fetch is content-agnostic → **zero allocator/MAX_LEAVES impact under either**);
> (C) bit-serial dropped. Revisions folded (see the spec):
> - **§2 pin (B):** fetch r[rt]→`BR` via a swap-fetch bracket sequenced OUTSIDE `with_two_regs`; core
>   on 4 fields `[r0-slot, s0, BR, s1]` = 8 predicated `cccnot_prim` gates `r0.i ^= pred ∧ s0.i ∧ BR.i`.
>   ~30 LoC replacing the bit-0 body. Cost +31 cells / +0.9% d_pre / +1.91% fib step_j (no new opcode).
> - **§3 recognizer needs a NEW target-LAST arm** — NOT just the `&1` drop: the §4 fold body is
>   `(elem,(key,acc))` so acc = leaf 2 → `Ccnot{rd:regs[2], rs:regs[0], rt:regs[1]}` for
>   `(c1,c2,t)↦(c1,c2,t^(c1&c2))`. Without it the program is UNBUILDABLE (reproduced). Keep the
>   target-first arm too (drop its `&1`). `exec_instr` drops `&1`.
> - **Redefine 0b1011** (not a new opcode): SAFE — dot_reduce + 26 targets green untouched, only
>   `t_dense_bit0_only` flips; the decoder `op_samples` become firing dense-byte cases (closes f3.5's
>   vacuous-oracle gap). `negative_guards` needs NO change (§5 was wrong).
> - **T-instr = system-ring dense-byte tier** (`run` + ancillas_restored + bus_quiescent) — genuine
>   L19, not a self-written checker. **Flat(4) masked fold** = 26 instrs / 2 Ccnots / 69,888 whole
>   domain / real-CPU incl. the bit-0-discriminating case `elems=[2],key=2`. Flat(8) program walled
>   (256³ prim); the INSTRUCTION's 8-bit behavior is proven at T-instr dense bytes.
> - **Exact re-pins** (embed — don't ship "NEEDS RE-PIN"): ring_two_body_timing d_pre 3345, delta1 3,
>   delta_hold 39, d_locked=period 3676 (d_post 289/d_mem 35/B24/T20 unchanged); energy me_a3 total_j
>   **7.3674162103e-8**, step_j **3.0697567543e-9**. `validate()==Ok` at the new values.
> - **Gate:** `cargo clippy` + `timeout 5400 cargo test -p theseus-tisc --release` (full suite = 2073s
>   release; `timeout 1800` KILLS a green run). Doc-rot: decoder.rs s1-bits-5/6 text (stale) → BR
>   swap-fetch + value-level aliasing caveat; asm.rs Instr::Ccnot doc bit0→byte; status.md joules.


**Milestone.** The last rung of the byte-wide ladder: lift `Ccnot` from **bit-0** to
**byte-parallel** — `r[rd] ^= r[rs] & r[rt]` over ALL 8 bits — completing the full masked
accumulate `acc ^= key & arr[i]` over words. This is a **`system_step` decoder change** (the
highest bar in the project): it needs a decoder-buffer convention the bit-0 version avoided
(f3.5 review F35-1, deferred to here).

## 0. Two walls, two scopes

- **The instruction (hard):** byte-parallel AND needs the target r[rd] + BOTH control bytes
  r[rs], r[rt] colocated. The decoder pins only TWO full bytes (r0-slot + `s0`); `s1` is full
  (pred/chain/copies), `s2–s7` are USER scratch. So it needs a **second reserved 8-bit
  operand buffer** — the crux (§2).
- **The program (walled at Flat(4)):** a program using byte-`Ccnot` needs a 3-leaf prim
  `(elem, key, acc) ↦ (elem, key, acc ^ (key & elem))`, whose forward table at Flat(8) is
  `256³ = 16.7M` — the 2-operands-forever wall. So: **verify the INSTRUCTION at dense bytes
  (all 8 bits); demonstrate the PROGRAM at Flat(4)** (4096-entry prim — feasible; AND is
  bitwise ⇒ canonical at every width, so Flat(4) genuinely exercises 4-bit parallel AND, which
  bit-0 `Ccnot` would get wrong). The Flat(8) masked accumulate as a whole program is not
  buildable; the instruction is what makes it *possible*.

## 1. Redefine `0b1011` to byte-parallel (no new opcode)

Change the semantics of the existing `Ccnot` (opcode `0b1011`) from `bit0` to full byte. **Safe:**
- Bit-valued programs (f3.5's `dot_reduce` reduce, all `Bit`-leaf uses) are UNAFFECTED —
  `0&0=0`, `1&1=1` are bit-0-identical to byte-parallel.
- Only f3.5's `t_dense_bit0_only` (which asserts *only* bit 0 flips on dense bytes) flips to
  assert the full-byte AND. That test's whole purpose was to pin bit-0-vs-byte; it now pins
  byte-parallel.
- No opcode bloat / no second ISA-growth energy tax. (A new opcode `0b1100` is the fallback if
  a bit-0 `Ccnot` turns out to be independently wanted — it is not.)

## 2. The decoder second-buffer (THE crux — the design-review must BUILD to decide)

The core is 8 predicated 3-control Toffolis (already expressible — `cccnot_prim`, used by ADD's
TK adder): with r[rd]@r0-slot (wires 0..8), r[rs]@s0 (8..16), r[rt]@BUF (16..24), and pred@s1.0:
```
for i in 0..8:  gate_at(3W, &[pred, W+i, 2W+i, i], cccnot_prim())   // r0.i ^= pred ∧ s0.i ∧ BUF.i
```
The open question is **where BUF is** and how r[rt] is fetched into it without disturbing
r[rs]@s0 or r[rd]@r0-slot. Three candidate conventions — the review builds a spike and picks:

- **(A) Reserve `s2` as a second operand buffer** (`BYTE_OPD_FIELD = 14`). Clean and symmetric
  with `s0` (fetch r[rt]↔s2, self-cancelling, s2 canonically 0). **Cost:** shrinks user scratch
  `s2..s7 → s3..s7`, so `SCRATCH_SPILL_BASE 2→3`, `MAX_LEAVES` drops by 1, `home_of`/the allocator
  shift — a real blast radius + many golden re-pins. Re-validate closure.
- **(B) Borrow `BR`** (field 9, canonically 0 outside branches; the uniform `PC += BR + 1` sees
  BR=0 if the fetch is balanced). **No scratch cost, no `MAX_LEAVES` change.** Risk: the crossbar
  routes r[rt] through the r0-slot waypoint, which the target r[rd] also uses — the fetch must
  sequence around the target routing (fetch r[rt]→BR *before* routing r[rd]→r0-slot, or via a
  path that doesn't transit r0-slot). The review must confirm a clean, self-cancelling route.
- **(C) Bit-serial** (8× copy-bit/cccnot/uncopy, today's machinery, no new buffer). Works, no
  closure change, but F35-1(c): ~8000+ K1 cells — a permanent step-energy tax on every program
  (the decoder grows for ALL programs). Fallback only.

**Recommendation to test first: (B) BR** (cheapest if the routing closes), then (A) if BR's route
can't be made self-cancelling. The review's spike decides empirically (build `ccnot_instr` byte-
parallel, run `validate()`, check `run_on_cpu==eval` on dense bytes).

## 3. Recognizer + fast-sim (drop the `&1`)

- Recognizer (compile.rs, the target-first ccnot arm): `|t,c1,c2| (t ^ ((c1&1)&(c2&1)), c1, c2)`
  → `|t,c1,c2| (t ^ (c1 & c2), c1, c2)` (full byte).
- Fast-sim `exec_instr`: `regs[rd] ^= (regs[rs] & regs[rt]) & 1` → `regs[rd] ^= regs[rs] & regs[rt]`.
- Both must agree with the decoder (the L19 three-way: decoder == fast-sim == eval on dense bytes).

## 4. The demonstrating program — masked accumulate at Flat(4)

`maskedFold = fold (list_spine_ty(Flat(4), D)) maskedStep`, accumulator `c = (key, acc)` both
`Flat(4)`. `maskedStep : Flat(4) × (Flat(4)×Flat(4)) ↔ same`, `(elem, (key, acc)) ↦ (elem, (key,
acc ^ (key & elem)))` — a 3-leaf prim (elem, key, acc) over Flat(4)³ = **4096 entries** (feasible),
recognizing to a single byte-parallel `Ccnot{rd:acc, rs:key, rt:elem}`. `key` rides the accumulator
(fixed mask threaded through the fold, preserved each step). Result: `acc ^= key & (⊕-of-nothing…)`
— actually `acc_out = acc0 ⊕ ⊕_i (key & elem_i)` (a keyed/masked GF(2) reduction over the list).
(Fold over a list, NOT iter+memexch — memexch walls at Flat(8) and we want the cleanest vehicle;
Flat(4) here is the *prim-table* cap, independent of memexch.)

## 5. Verification tiers

- **T-instr (dense bytes, DEFAULT — the byte-parallelism proof):** a decoder-level test (extend
  f3.5's `tests/decoder.rs` `op_samples`/`reference`, or a direct system_step gate) with dense
  controls (rs=0xAA, rt=0x55, rt=0xFF, rs=0xC3…) asserting `r[rd] ^= rs & rt` over ALL 8 bits ==
  the real `system_step` == fast-sim == the independent `&` reference. This is where all 8 bits get
  exercised (the program at Flat(4) only drives 4).
- **T-dense (update f3.5):** `t_dense_bit0_only` → assert full-byte AND (was bit-0).
- **T-prog (Flat(4) masked accumulate, whole-domain-ish):** `maskedFold` at Flat(4), D=3 — emits
  one `Ccnot` per copy; `run_on_cpu_fast == eval` + independent reference `acc0 ⊕ ⊕(key & elem_i)`
  over a tractable domain (Flat(4)³ list × key × acc — stride if needed; HOIST-COMPILE-ONCE).
- **T-real (`#[ignore]`, L19):** real `system_step == eval` on a dense-byte masked step + a Flat(4)
  masked fold sample.
- **T7 / closure:** SystemRing `validate() == Ok` with the new byte-parallel `ccnot_instr` in
  `Op::ALL` (the reversibility go/no-go).
- **Regression:** f3.5's `dot_reduce` (Bit) still green (bit-0-identical); the ring_two_body_timing
  golden RE-PINNED (decoder grew — measure + `validate()==Ok` ⇒ legit); `negative_guards`/`decoder`
  op-tables updated.

## 6. Probe / design-review mandate

The Fable design-review must **BUILD the byte-parallel `ccnot_instr`** (candidate B: BR, else A: s2),
run `validate()` + `run_on_cpu==eval` on dense bytes, and REPORT which second-buffer convention
closes reversibly + its cost (scratch/MAX_LEAVES impact, golden re-pins). This is the go/no-go: if
neither (A) nor (B) closes cleanly, fall back to (C) bit-serial (with the honest energy-tax note).

## 7. Delegation shape (post-review)

One maestro task (tier 0): the decoder arm (byte-parallel `ccnot_instr` + the chosen buffer
convention + any allocator change if (A)), recognizer/fast-sim `&1` drops, the Flat(4) masked-accum
program, T-instr/T-prog/T7 + the pre-existing test edits (t_dense, ring golden, decoder/negative_guards).
`file_allowlist`: `crates/theseus-tisc/**`. `check_commands`: `cargo clippy` + `timeout 1800 cargo
test -p theseus-tisc` (FULL suite — a decoder/system_step change rots golden tests; the f2 lesson).
FULL adversarial impl-review (system_step change — CLAUDE.md non-negotiable) + reproducing tests.

## 8. Risks / adversarial-review targets

1. **Second-buffer reversibility (THE crux)** — does the chosen BUF (BR or s2) fetch/unfetch
   self-cancel, leave BUF at its entry value, and keep `validate()==Ok`? Does BR's `PC+=BR+1`
   interaction stay correct (BR=0 at step end)?
2. **Semantic redefinition safety** — confirm every Bit-valued `Ccnot` use is bit-0-identical
   (dot_reduce reduce still green); only T-dense flips.
3. **The 3-operand Flat(8) prim wall** — the program genuinely caps at Flat(4); the instruction's
   full-byte behavior is only provable at the instruction level (T-instr dense bytes). Is that
   adequate L19 authority for the byte-parallelism, or a gap?
4. **Golden/scoped-gate rot** — the decoder grows: ring_two_body_timing + possibly energy (me_a3,
   the ISA-growth tax again — fib step_j rises further) + decoder op-tables. Full suite mandatory.
5. **Allocator blast radius (if (A) s2 reserved)** — MAX_LEAVES drop by 1 could break existing
   near-ceiling programs (the 15-leaf spill tests, the fold ceiling D≤7). Quantify.
