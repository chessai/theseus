# Adversarial review — A-0 shared operand crossbar + scratch-rd datapath (Fable)

Worktree `a0-instr-format`, restructure `99021ba` (shared decode_execute) on top of `a5150d6`
(crossbar wired). **Verdict: DO NOT MERGE.** One proven BLOCKER (empirically, through the real
evaluator). Register-only operands sound; scratch-`rd` — the core A-0 feature — corrupts.

## BLOCKER-1 — routing a scratch operand DISPLACES the operand buffer (s0) + predicate word (s1)
`select_operand`'s level-3 stage (decoder.rs:347-350) swaps `field(i) ↔ field(14+i)` for all i∈0..8
when the operand index's bit 3 is set (a scratch operand). But **field 14 = s0 (the fixed operand
buffer the cores read) and field 15 = s1 (the predicate word)** — the scratch block the crossbar
brings low *starts at s0/s1*. `with_two_regs`/`with_target` hold that permutation open across the
cores, so when `rd` names scratch (s2..s7, idx 10..15 — LEGAL per asm.rs:206):
- **(a) operand displacement:** the fetched `rs` value is swapped out of s0 before the core reads it
  (pre-existing since a5150d6 — `with_target` over the 16-way crossbar was never safe for scratch-rd).
- **(b) predicate corruption (new in 99021ba):** `group_a/b_shared` run `pred_compute` (decoder.rs:457,
  XOR-Toffoli chain into s1) INSIDE the open `sel_rd`, so its targets are r[1]'s live bits, not zeros
  → garbage predicates → wrong / multiple cores fire, including the OTHER format's cores.
Empirical (through `decode_execute()`): `Add s2,r3` → s2=0xEE (shared) vs 0xDE (replicated) vs 0x33
(correct) — **shared ≠ replicated ⇒ the equivalence claim justifying the ring/energy-path swap is
false.** `Xori s2,#0x0F` with r1=1 → SWAPBR's core fires on an I-format word, clobbers BR, leaves
s0≠0 → breaks the inter-step s0=s1=0 invariant → unbounded downstream corruption on the ring.
Affected: all 6 group-A ops, 5 immediates, Swapbr, Ccnot-rd, Cswap-rt with scratch targets.

**FIX (structural, not a one-liner):** the operand buffer + predicate must live where the
scratch-operand routing cannot reach them. Concretely: the crossbar's scratch-half must address the
LEGAL scratch operands **s2..s7 (fields 16..21) without permuting s0/s1 (fields 14/15)** — remap the
level-3 swap base from 14 to 16 (s0/s1 are never legal operands, so the routing need not include
them). Then routing any legal operand (reg 0..7 or s2..s7) leaves s0 (buffer) + s1 (predicate) stable,
and mechanisms (a)+(b) both close. Re-derive the shared/replicated equivalence with scratch explicitly
in scope. Regression (L20): the reviewer's probe `/tmp/a0-review-probe-scratch-rd.rs` — scratch-rd of
each op through `decode_execute`, asserting result + s0=s1=0 after + shared≡replicated.

## MAJOR-2 — the green suites structurally cannot see BLOCKER-1
`tests/decoder.rs` `reference` (`out.regs[rd]`) PANICS on scratch rd — no sample expresses it. The only
scratch test through decode_execute is `blocker2_ccnot` (rt, routed by the complete self-inverse
`ccnot_fetch_rt_br` — the ONE safe path). a0_format covers scratch-rd at encode/decode only. So
"decoder 12/12 + a0_format 9/9 + bveq green" proves nothing about the shipped feature. The per-op
tests exercise `instruction(op)` (replicated), not the shared cores.

## MINOR
- step_ops (replicated) vs step (shared) now use different decoders — subset tests via step_ops no
  longer certify step. Divergence trap.
- Reserved high-opcode-bit aliasing (R op16 = Add) persists (pre-existing deferred MAJOR-3).

## Verified SOUND (attacked, held)
Register-only operands: shared ≡ replicated (bracket algebra valid; sel_rd levels 0..2 touch only
fields 0..8; pred fields disjoint; empirical control passed). Exch writeback (s0 returns to rs home via
sym(fetch)). Group-B s0=0 (prior groups restore s0 for register words). Swapbr/Ccnot exclusion
rationales correct; swapbr_instr AST-identical to old. step.rs:252 phase structure matches step_ops;
system_decode_execute tensor collapse valid; op coverage 6+4+5=15=Op::ALL.

**Bottom line:** the netlist-sharing idea is sound for the register file but ships on a broken
scratch-rd datapath (the operand buffer + predicate sit inside the scratch block the crossbar permutes).
Relocate the scratch-routing base off s0/s1, adopt the scratch-rd-through-decode_execute probe as the
L20 regression, re-derive equivalence with scratch in scope — before merge.
