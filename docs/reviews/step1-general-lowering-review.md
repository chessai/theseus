# Adversarial review — A-6 STEP 1: general BDD synthesizer as prim-lowering fall-through

- **Commit under review:** `3e46244` (worktree `a6-synth` @ `24ad718`)
- **Scope:** `general_synthesize_prim` + supporting (`leaf_widths`, `table_byte_map`,
  `verify_general_body_exec`) in `crates/theseus-tisc/src/compile.rs`;
  `table_to_regtransfer` / `transcribe_bdd_bennett` / `transcribe_bdd_into` in
  `crates/theseus-tisc/src/synth.rs`; `tests/general_lowering.rs`.
- **Reviewer:** fresh-context agent (different model from the Opus-5 implementer).
  READ-ONLY review; findings only, no fixes. All probe results below were
  reproduced with real `cargo test` runs against a transient probe file
  (`tests/adv_probe.rs`, since deleted; probe sources reproduced in §Appendix).

## Verdict

**No wrong-lowering-passes-verification was found.** The one candidate channel
(output leaf wider than the input-derived width → truncated body) is caught —
somewhat by accident — by the Bennett ancilla net-zero check (Finding 3, probe
P4). For the type shapes the compiler currently feeds it (Prod-of-Flat, and
"tight" Sums whose branches have identical leaf shape), the path is trustworthy
and fail-closed.

**It is NOT yet trustworthy as the universal fall-through for ARBITRARY admitted
prims**, which is its stated purpose (and the foundation STEP 2 compositional
control wants to build on). Any prim whose input type is a "loose" Sum —
unequal-branch leaf counts or widths, i.e. every option/`1+X` shape, exactly the
shapes sum-based control produces — is either **falsely rejected with a
misleading error** (Finding 1) or **panics the compiler outright** (Finding 2),
falsifying the commit's "never a panic" contract. Fix the type-shape
precondition (and panic paths) before building compositional control on top.
The real-`system_step` attestation gap (Finding 7) also needs a deliberate
advisor run — the shipped test's prim never touches a scratch operand, so
re-running it on the real machine would not attest the novel part.

Definitions used below:
- **tight type**: every assignment of the enumerated `leaf_widths` bit-space is
  a canonical marshaling of some value (`cardinality == 2^Σwidths`). Pure
  Prod-of-Flat is tight; a Sum is tight iff both branches have identical
  leaf-shape (e.g. `Flat(2)+Flat(2)`).
- **loose type**: `cardinality < 2^Σwidths` — some enumerated byte rows are
  junk (zero-pad bytes that can be nonzero, payload bytes out of the active
  branch's range, tags ≥ 2).

---

## Finding 1 — MAJOR — loose-Sum inputs are FALSELY REJECTED with a misleading error

**Assumption broken:** "any admitted full-table bijection prim … lowers here"
(commit message; comment at compile.rs:1425). Implicit assumption in the
bwd∘fwd cross-check (compile.rs:1588–1614): every enumerated
`2^total_bits` carrier-byte row round-trips through `value_from_leaf_bytes` /
`value_to_leaf_bytes` injectively. That holds only for tight types.

**Mechanism:** for a loose Sum, junk rows (e.g. nonzero zero-pad byte) parse to
the same `Value` as their canonical twin (`value_from_leaf_bytes` silently
ignores pad bytes, compile.rs:160–165). `fwd_map(junk) == fwd_map(canonical)`,
so `bwd_map(fwd_map(junk))` returns the canonical bytes ≠ junk bytes, and the
cross-check reports **"the backward table is not the inverse"** — blaming the
prim author's backward table, which is in fact a perfect inverse.

**Counterexample (probe P1, reproduced):** the 3-cycle bijection on
`1 + Flat(2)` (admitted by `admit()`):

```
PROBE p1_sum_one_flat2: CLEAN ERROR: EmissionVerificationFailed(
  "general synth 'p1_cycle': bwd∘fwd ≠ id on carrier bytes [0, 1]
   (fwd→[1, 0], bwd→[0, 0]); the backward table is not the inverse")
```

`[0, 1]` is tag=Left with junk pad byte 1 — not a canonical value at all.

**Class:** verification-gap / coverage bug (fail-closed, but wrong diagnosis and
a hard coverage hole for every sum-shaped prim). Positive control: probe P5
(branch-swap on the *tight* Sum `Flat(2)+Flat(2)`) lowers fine (60 instrs), so
the boundary is exactly looseness, not Sums per se.

## Finding 2 — MAJOR — compile-time PANIC (not a clean `CompileError`) on admitted prims

**Assumption broken:** "a GRACEFUL fall-through … never a panic/wrong body"
(commit message). Two distinct panic paths, both in the shared marshaling
helpers the general path drives with non-canonical inputs:

**(a) Loose Sum with unequal branch widths.** The cross-check enumerates
`tag=0, payload=2` for `Flat(1)+Flat(2)`; parsing the payload against the
*active* branch `Flat(1)` trips the assert at compile.rs:174. Probe P2
(6-cycle on `Flat(1)+Flat(2)`, admitted):

```
thread panicked at crates/theseus-tisc/src/compile.rs:174:
value_from_leaf_bytes: byte 2 out of range for Flat(1)
```

**(b) Input/output leaf-width mismatch.** `widths = leaf_widths(&def.input_ty)`
(compile.rs:1558) is used for BOTH transfers; `table_to_regtransfer(&bwd_map, …,
&widths)` enumerates the OUTPUT space at INPUT widths (compile.rs:1617). If any
input leaf is wider than the corresponding output leaf's parse range, the bwd
enumeration feeds out-of-range bytes to `value_from_leaf_bytes(bytes,
&def.output_ty)`. Probe P3 (a genuine admitted bijection
`Flat(1)×Flat(3) → Flat(2)×Flat(2)`, `i = 8a+b ↦ (i>>2, i&3)`):

```
thread panicked at crates/theseus-tisc/src/compile.rs:174:
value_from_leaf_bytes: byte 4 out of range for Flat(2)
```

**Class:** correctness-of-contract / robustness bug. In the maestro pipeline a
compile panic is a process abort, not a `CompileError` the caller can route to
`NeedsProvisioning`-style handling. Note theseus-core already has the
non-panicking `try_value_from_leaf_bytes`; compile.rs re-implements the
panicking variant locally (lines 150–233) instead of using it.

## Finding 3 — MINOR (verified fail-closed, but accidental) — wide-output truncation is caught only by the ancilla net-zero check

**Assumption probed:** attack 1's false-green channel. When an output leaf
needs MORE bits than the input-derived width (loose output type, tight input),
`table_to_regtransfer` silently drops the high output bits (truth table only
records `bit < in_bit_widths[leaf]`, synth.rs:1366–1369), and
`verify_general_body_exec` **masks the carrier comparison by the same input
widths** (`want & mask`, compile.rs:1841) — so the carrier check CANNOT see the
truncation. Both "independent" checks interpret the table through the same
wrong widths, exactly the feared shared-interpretation blind spot.

What actually saves it: information lost by truncation makes the truncated
forward non-injective, so the Bennett uncompute (`O ^= P⁻¹(y′)`) cannot clear
the ancilla for at least one input, and the (unmasked) net-zero check fails.
Probe P4 (`Sum(P(F2,F2),P(F2,F2)) → Sum(Flat(4),P(F2,F2))`, `Left(p,q) ↦
Left(4p+q)`, admitted; passes the bwd∘fwd cross-check because canonical outputs
round-trip fine):

```
PROBE p4_loose_output_truncation: CLEAN ERROR: EmissionVerificationFailed(
  "general synth 'p4_loose_out': BDD body exec-verify failed:
   aux word 4 not net-zero (=0x01) after body on input [0, 1, 0]")
```

Argument that this generalizes (why I did not find a true false green): a false
green needs the truncated fwd to stay injective on the canonical domain. With a
tight input, the masked byte space has exactly `cardinality` patterns, and any
output value using bits above the input width forces a collision (pigeonhole
over the masked space; pad bytes of loose outputs are pinned to zero) → ancilla
residue → caught. With a loose input, Finding 1/2 rejects before synthesis.
This is an *argued* invariant, not a designed one — nothing in the code states
or tests it, and the failure surfaces as a baffling "exec-verify failed" that
points at the synthesizer rather than at the unsupported type shape.

**Recommended shape for the fix (covers Findings 1–3 in one precondition):**
reject up front, with a dedicated structured error, any prim where
`leaf_widths(input_ty) != leaf_widths(output_ty)` or where either type is not
tight (`cardinality != 2^Σwidths`) — or do the honest thing and derive
per-direction widths from per-direction types plus range-check tags/pads.

## Finding 4 — MINOR — the "independent differential" is not a differential, and its failure policy is inconsistent

compile.rs:1727–1763: the "differential vs the ANF path" never compares the two
bodies. It exec-verifies the ANF body against the SAME table through the SAME
`value_to_leaf_bytes`/`leaf_widths` interpretation used for the BDD body (the
comment at 1753–1755 admits this). Consequences:

- **Class of bug it cannot catch:** anything in the shared front end —
  `table_byte_map`, `leaf_widths`, marshaling, the bwd∘fwd cross-check itself,
  and the masked-comparison blind spot of Finding 3. Both paths consume the
  same `fwd`/`bwd` RegTransfers (compile.rs:1616–1617), so `table_to_regtransfer`
  bugs are only caught insofar as they make exec ≠ table — which check (a)
  already does alone. It is real coverage for `transcribe_bennett_general`
  transcription bugs and nothing else. The commit message ("independent second
  synthesis mechanism, so an agreement is strong evidence neither path is
  wrong") oversells it.
- **Inconsistent failure policy:** if `transcribe_bennett_general` cannot be
  *wired* (helper-bank shape), the differential is silently skipped
  (compile.rs:1757–1760); but if the ANF body wires and then fails exec-verify,
  the `?` at 1745–1752 **fails the whole lowering** even though the returned BDD
  body already passed its own whole-domain verification. A latent bug in the
  never-emitted ANF path can thus brick an otherwise-verified lowering.
  Deliberate fail-closed? Maybe — but then skipping on wiring failure is the
  hole in that policy. Pick one.

## Finding 5 — NOT BREAKABLE — NeedsProvisioning budget soundness (attack 3)

I could not construct an under-count, and I believe none exists:

- The budget (compile.rs:1626–1645) and the emitter (synth.rs:1875) compute
  node counts from the **same** `BvExpr`s (`fwd`/`bwd` rows), the same
  deterministic `expr_to_bdd`, the same `VarMap::new(NUM_REGS)`, and the same
  `size() - 2` formula.
- Peak concurrent node-word usage in `transcribe_bdd_bennett` IS the
  single-output-bit node count: `emit_bdd_output_bit` allocates all node words
  for one bit, uncomputes them all (reverse pass), and the next bit reuses the
  bank. Nothing holds node lines across output bits. So `max` per bit is the
  exact peak, not an approximation of an antichain.
- The budget even over-counts: it scans identity rows (var expr → 1 node) that
  `transcribe_bdd_into` skips (synth.rs:1835), and above-width `Const(false)`
  bits (0 nodes). Safe direction only.
- The `operand_get/set` panic (types.rs, idx ≥ 16) is unreachable from here:
  the free pool is structurally `{0..NUM_REGS} ∪ {NUM_REGS+s, s ∈ 2..8}` ⊂
  0..16 (compile.rs:1671–1682), and `transcribe_bdd_bennett` re-validates every
  word with `check_operand_legal` (synth.rs:2076–2087). The 7-leaf shipped test
  exercises the graceful reject.

One nit here: the `total_bits > 20` reject (compile.rs:1570–1576) reports
`required = 2^total_bits` **rows** against `available = 2^20` in a field pair
that everywhere else means **operand words** — inconsistent units in the
diagnostic. Also, the carriers-not-register-resident case (compile.rs:1655–1663)
reports `NeedsProvisioning` although it is really a paging limitation (arms use
`with_registers` to page leaves in; the general path never does), so a small
prim deep in a larger program whose leaves sit in scratch/mem gets a
misleading "provision more words" answer that widen-state will not fix.

## Finding 6 — MINOR — bijection assumption (attack 4) is safe for admitted prims, panics for un-admitted ones

`bwd∘fwd == id` is cross-checked over the whole enumerated domain; totality is
enforced by `forward.len() == backward.len() == cardinality(input_ty)`
(compile.rs:1546–1555); and `admit()` (theseus-core/src/prim.rs:181–234)
already proves totality + two-sided inverse before any prim exists. Non-injective
or inconsistent tables fail cleanly. Residual: `recognize_prim` does not itself
require admission — a table with `len == card` but one off-domain key (hence one
missing domain row) reaches `table_byte_map`'s
`panic!("no entry for input …")` (compile.rs:1499) instead of a clean error.
Only reachable by bypassing `admit`, i.e. test harnesses; worth a `Result`
anyway given Finding 2's pattern.

## Finding 7 — MAJOR (verification gap, for the advisor) — fast-vs-real: scratch-operand routing is unattested, and the shipped test cannot attest it

What `run_on_cpu_fast == eval` (and the internal exec-verify) actually attest:
`exec_instr` semantics (compile.rs:3906–3989, including scratch operands via
`operand_get/set`) plus `encode`/`decode` round-trip (`run_on_cpu_fast` does
assemble+decode, compile.rs:4007–4019). What they can NOT attest: the real
`system_step` decoder/datapath netlist — and the general path is the **first
production emitter of instructions whose operand fields name scratch words**
(node lines / temp / ancilla drawn from `s2..s7` = operands 10..15) under
`Xor`/`Xori`/`Ccnot`/`Rotli`/`Rotri`/`Exch`, plus `swap_operands`-generated
`Xchg` when a carrier↔scratch swap is needed.

Two concrete facts the advisor must weigh:

1. **No existing real-CPU test executes an instruction with a scratch operand
   index.** I grepped every test that touches `run_on_cpu`/`system_step` for
   operand indices 10..15 — zero hits. `a0_format.rs` validates the
   `select_operand` crossbar as an isolated sub-iso via `eval`, and
   encode/decode round-trips — not a full fetch-decode-execute of, say,
   `Ccnot rd=r5, rs=r0, rt=s3` on the ring.
2. **The shipped small prim (`Flat(2)×Flat(2)`, 174 instrs) never touches
   scratch.** Its free pool is consumed registers-first (compile.rs:1671–1676):
   n=2 carriers r0/r1 → ancilla r2/r3, temp r4, node lines r5… — all registers.
   So the advisor run the test file requests (bare prim, all 16 inputs, real
   `system_step`) would NOT cover the novel scratch-operand routing.

**Minimum slow-authority coverage:** (i) the bare small prim, all 16 inputs,
real `system_step` (as the test comments already demand); (ii) the `Seq(f, Id)`
composition; (iii) **a prim sized to force scratch helpers** — e.g. 4 leaves
(4 carriers + 4 ancilla + 1 temp = 9 > 8 registers, spills node/temp words into
s2..s7) — whole domain on real `system_step`, asserting scratch words return to
zero; (iv) at least one input where `rotate_by` fires with nonzero shift on
both a carrier and a node word (Flat widths > 1 with var_bit > 0 guarantee
this). Watch items: s0/s1 operand-buffer interaction with back-to-back
scratch-operand gates, and ROM length (a general body near 256 instrs meets
the 8-bit PC edge).

## Finding 8 — MINOR — vacuity-guard retirement (attack 6) is sound; two behavioral shifts to note

The retirement itself holds up: the general path requires a FULL table
(`len == card`, so the empty-table prims the `bveq_verify_emission` hard-reject
existed for still die cleanly as `UnrecognizedPrim`); `n_in == n_out ≥ 1` is
enforced; partial-table+spec prims are routed to `selection_by_equiv` before the
arm scan (compile.rs:1034–1042) and never reach the fall-through; full-table+spec
prims are lowered from the table alone, which is safe because `admit` proves
spec==table (prim.rs:236–259). I found no prim that should be rejected and now
gets a garbage lowering; loose-Sum prims that should be ACCEPTED get rejected
(Finding 1), which is the opposite failure. Two shifts worth eyes:

- Error-type shift: prims that used to return `UnrecognizedPrim` can now return
  `NeedsProvisioning` or `EmissionVerificationFailed`. No in-tree caller
  branches on `UnrecognizedPrim` (checked), but any external tooling keyed on
  that message loses its signal, and "add an arm" pressure disappears silently —
  a misnamed `memexch`-shaped prim now gets a correct but enormously more
  expensive BDD body instead of a 1-instr arm, with no warning.
- Gate-time exposure: the caps allow `total_bits` up to 20 → the cross-check
  (2^20 `BTreeMap<Value,_>` lookups on deep thermometer keys), two
  `table_to_regtransfer` sweeps (2^20 rows each + Möbius per output bit), and up
  to ~128 `expr_to_bdd` builds over huge ANF expressions — minutes of compile
  time is plausible for a legitimately admitted 16–20-bit prim, inside a maestro
  gate that has NO timeout. Bound it or document it in the spec discipline.

## Finding 9 — NIT — harness blind spots (latent, currently unreachable)

`verify_general_body_exec` zeroes every non-carrier word (compile.rs:1814–1825),
so a body that reads an UNDECLARED free word passes verification vacuously
(XOR-with-0). Today unreachable: MAJOR-1 rejects specs reading beyond the
carriers (synth.rs:2126–2141), `src_map[r≥n]` never escapes into emission for
verified specs, and runtime free words are 0 by the net-zero discipline. But it
means the verify harness proves "correct starting from the all-zero machine",
not "touches only its declared words" — worth a one-line comment or an explicit
read-set check if the general path grows. Similarly `leaf_value_to_u8`
(compile.rs:130–144) accepts `Left(non-Unit)` and arbitrary terminals where the
core version panics — cosmetic divergence between the duplicated marshaling
implementations (see Finding 2's recommendation to use core's `try_` variant and
delete the local copies).

---

## Summary table

| # | Severity | What | Kind | Evidence |
|---|----------|------|------|----------|
| 1 | MAJOR | Loose-Sum inputs (every `1+X` / unequal-branch Sum) falsely rejected as "backward table is not the inverse" | coverage bug + misdiagnosis (fail-closed) | probe P1, reproduced |
| 2 | MAJOR | Compiler PANICS (not `CompileError`) on admitted prims: unequal-width Sum branches; input/output leaf-width mismatch | contract violation ("never a panic"), robustness | probes P2, P3, reproduced |
| 3 | MINOR | Wide-output truncation invisible to the masked carrier check; caught only by accidental ancilla net-zero residue, with a misleading error | verification-gap (empirically fail-closed) | probe P4, reproduced |
| 4 | MINOR | "Differential" is body-vs-table twice, not body-vs-body; blind to shared front-end; inconsistent skip-vs-fail policy | verification-gap | code reading (compile.rs:1727–1763) |
| 5 | — | Budget under-count / operand-index panic: **not breakable** | — | code reading + shipped 7-leaf test |
| 6 | MINOR | Un-admitted len==card-but-off-domain table panics in `table_byte_map` | robustness (admit-bypass only) | code reading |
| 7 | MAJOR | Real `system_step` has never executed a scratch-operand instruction; shipped small prim uses no scratch, so re-running it doesn't attest the novel routing | verification gap (advisor authority scope) | grep of all real-CPU tests; free-pool allocation order |
| 8 | MINOR | Vacuity retirement sound; error-type shift + unbounded gate-time for 16–20-bit prims | behavioral/ops | code reading |
| 9 | NIT | Zero-state verify harness blind to undeclared-word reads; duplicated marshaling diverges cosmetically from core | latent | code reading |

## Appendix — probe definitions (reproduce by recreating `tests/adv_probe.rs`)

All probes: build the `PrimDef`, assert `admit(&def)` passes, then
`recognize_prim(&def, &Ctx::new_canonical(n))` under `catch_unwind`.

- **P1** `1 + Flat(2)`, 3-cycle over `enumerate_values` → CLEAN ERROR (false reject, "backward table is not the inverse" on junk row `[0,1]`).
- **P2** `Flat(1) + Flat(2)`, 6-cycle → PANIC `value_from_leaf_bytes: byte 2 out of range for Flat(1)` (compile.rs:174).
- **P3** `Flat(1)×Flat(3) → Flat(2)×Flat(2)`, `8a+b ↦ (i>>2, i&3)` → PANIC `byte 4 out of range for Flat(2)` (bwd enumeration at input widths).
- **P4** `Sum(P(F2,F2),P(F2,F2)) → Sum(Flat(4),P(F2,F2))`, `Left(p,q)↦Left(4p+q)`, `Right` identity → CLEAN ERROR `aux word 4 not net-zero (=0x01) after body on input [0,1,0]` (fail-closed via ancilla residue).
- **P5** (positive control) tight Sum `Flat(2)+Flat(2)`, branch swap → LOWERS OK, 60 instrs.
