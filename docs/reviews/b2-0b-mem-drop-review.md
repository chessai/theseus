# B-2.0b adversarial review — per-program Layout derivation + CPU-only MEM-drop routing

**Diff under review:** `0e1a1df..51167dc` (branch `phaseb-shared-alu`, worktree `/tmp/phaseb`)
**Reviewer:** fresh-context adversarial reviewer (did not write the change); read + reason + fast-tier tests only (heavy tier NOT run, per constraint).
**Scope of the change:** `asm.rs` (`Instr::uses_mem`, `Instr::operand_locs`), `types.rs` (`Layout::for_program_facts`), `compile.rs` (`derive_layout`, `is_cpu_only`, MEM-drop routing in `run_rom_on_cpu`), new fast-tier test suite `tests/b2_layout.rs`.

## Verdict: **SHIP** — no BLOCKER, no MAJOR. Conditional on the standard pre-merge heavy-tier authority run (see F2/F6). Four MINORs, none oracle-breaking.

---

## 1. Attack: `is_cpu_only` / `uses_mem` classification completeness

**ISA enumeration (all 17 `Instr` variants):** Add, Sub, Xor, Exch, Rotl, Rotr, Cswap, Ccnot, Xchg, Swapbr, Mexch, MexchReg, Addi, Subi, Xori, Rotli, Rotri.

At the hardware level, MemState/BusMsg are a separate tensor factor; the ONLY
phases that can net-affect them are `emit`/`mem_phase`/`absorb`
(`decoder.rs:1937–2130`), and their net effect is gated on
`pred_compute(&r_pred(0b1001))` (MEXCH) and `pred_compute(&r_pred(0b1010))`
(MEXCHR) — opcode-exact predicates. `decode_execute_shared` is
CpuState-typed and *cannot* touch mem. So the set of mem-touching
instructions is exactly `{Mexch, MexchReg}`, which is exactly what
`uses_mem()` matches. **I could not construct a program that touches mem and
slips through.**

Two subtleties checked and found safe:

- **`emit`'s unpredicated `addr ^= IR.rt` cnots** (decoder.rs:1985–1990) fire
  for *every* opcode with nonzero rt bits (e.g. `Ccnot`/`Cswap`), putting a
  transient nonzero BUS_ADDR into `mem_phase`. But `mem_body` is fully
  bracketed (`select ; gated_swap ; sym(select)`, decoder.rs:2109) and the
  gated swap is keyed on `BUS_ENABLE` (only set by the two mem predicates), so
  mem is net-unchanged; `absorb = sym(emit)` restores the bus. Net effect of
  phases 3–5 for any non-mem opcode: identity on all three factors.
- **rom/instrs divergence:** inside `run_rom_on_cpu` the executed ROM is
  `assemble(instrs)` — the classified slice IS the executed program, so the
  classification cannot be run against a different ROM than the one executed.
  All callers (`run_on_cpu`, `run_on_cpu_resident`, heavy test suites) pass
  the same `instrs` they intend to run.

**F1 (MINOR, robustness/future-proofing).** `uses_mem()` is a non-exhaustive
`matches!`. A future mem-touching variant (a hypothetical `Mexch2`) would
silently classify as CPU-only; on the `step` path its opcode is a *reserved
identity* (decode_execute doc: "Reserved opcodes (including MEXCH 0b1001 and
MEXCHR 0b1010) are the identity"), so the failure mode is a **silent no-op,
not an error** — exactly the divergence class this review is meant to prevent,
deferred to a future editor. `operand_locs()` is an exhaustive `match` and
would break compilation on a new variant; `uses_mem` should be written the
same way (exhaustive match with explicit `false` arms) so the compiler forces
the classification decision. Trigger: any future ISA growth. Fix is 5 lines.

## 2. Attack: two-carrier soundness of the `is_cpu_only && init.mem == 0` guard

- **Nonzero init.mem** (resident marshalling, or plain `marshal` of a
  program with >14 leaves — `home_of` places leaves 14..14+MEM_WORDS in mem)
  → `mem_quiescent == false` → system path. Unchanged behavior. Sound.
- **All-zero init.mem + no mem op:** under `system_step`, mem stays all-zero
  every step (per §1: phases 3–5 net-identity; nothing else types onto mem).
  So `StateVals::decode`'s fabricated `mem: [0; MEM_WORDS]` equals the true
  system-path mem, and `unmarshal`/`unmarshal_resident` reading
  `Location::Mem(a)` gets 0 on both carriers → **identical output Values**.
- **The nastiest corner — resident program, all-zero resident input, no mem
  instrs** (`marshal_resident` writes zeros into mem; `mem_quiescent` is then
  true and the program takes the step path even though its outputs are read
  from mem): still sound — no-mem-op ⇒ mem invariant ⇒ zeros preserved on the
  system path too; both carriers give mem = 0. The routing changes nothing
  observable. (Whether a zero output from an untouched mem home is *correct
  vs eval* is the compiler's problem, identically on both paths.)
- **Terminal check** (BR/s0/s1) reads fields decoded identically by
  `StateVals::decode` and `decode_system` (same bit offsets, same helpers).
- `mem_quiescent` checked on *init* only is sufficient precisely because
  `is_cpu_only` guarantees mem-invariance across steps.

**No hole found.** `StateVals::decode` never needs to represent nonzero mem on
this path, because nonzero mem is unreachable under the guard.

## 3. Attack: behavior preservation (step vs system_step on the CpuState factor)

Structural identity, phase by phase (step.rs:507–557):

| step(rom) | system_step(rom) |
|---|---|
| `fetch(rom)` | `(fetch(rom) ⊗ id_Mem) ⊗ id_Bus` |
| `decoder::decode_execute()` = `decode_execute_shared()` | `system_decode_execute() ⊗ id_Bus` = `(decode_execute_shared() ⊗ id_Mem) ⊗ id_Bus` (decoder.rs:1736–1739) |
| — | emit / mem / absorb (net-identity for non-mem opcodes, §1) |
| `fetch(rom)` (unfetch) | lifted, same |
| `pc_add_br()` ; `pc_inc()` | lifted, same |

The CpuState-factor iso is the **same object** (`decode_execute_shared()`),
not a re-implementation — so there is no per-op semantic gap to audit;
divergence would require phases 3–5 to be non-inert for a non-mem opcode,
ruled out in §1. Empirical: `cnot_is_cpu_only_and_run_on_cpu_matches_eval`
(whole input domain, routed step path) passes; full default fast tier **288
passed / 0 failed** on this worktree.

**F2 (MINOR, coverage regression — flag to advisor).** Every heavy-tier test
that used `run_rom_on_cpu` on a zero-mem, non-mem-op program as the "real
**system_step** anchor" now silently anchors against `step` instead:
`bveq.rs` L-b single-instruction reconciliation (its `state_with_regs` inits
zero mem), `bveq_netlist.rs` T2 real-machine anchors, `mde_spill.rs`
condition-4 (Xchg-only program), `widen_pc_payoff.rs`, and every CPU-only
program in `tests/compile.rs`. Truth is preserved (§1/§3), but
`system_step`'s non-mem-opcode inertness (emit/mem/absorb bracketing) loses
nearly all its *differential* coverage — after this change only mem-using
programs exercise it through the oracle. Trigger: a future regression that
breaks emit's inertness for a non-mem opcode (e.g. un-bracketing the
unpredicated rt addr-copies) would no longer be caught by any
`run_rom_on_cpu`-based test. Recommend: keep ONE heavy-tier anchor pinned to
the system path for a CPU-only instruction (simplest: an L-b case with a
nonzero init.mem word, asserting mem passes through unchanged). Also note for
the record: future re-runs of the mde_spill authority test are now
"real `step` == eval", not "real `system_step` == eval" — the advisor should
know the anchor's meaning changed.

## 4. Attack: layout derivation (`operand_locs` / reg-vs-scratch separation)

`operand_locs()` audited variant-by-variant against the ISA docs: crossbar
operands (rd/rs/rt/ra, indices 0..16 over reg∪scratch) are included; raw
non-crossbar fields excluded (`Xchg.sx` scratch index, `Mexch.addr` mem
index, all `imm`s). `derive_layout` then filters `l < NUM_REGS`, so a program
naming s5 (idx 13) **cannot** inflate `num_regs` — covered by the
`derive_reg_rounding_and_clamp` test (scratch operand idx 10 → num_regs 1).
`for_program_facts` clamp `[1, 8]` verified; `None → 1` floor is load-bearing
(the hardware routes selected operands through `reg_field(0)` in the
emit/select brackets, so r0 must always exist) — satisfied.

**F3 (MINOR, latent — next-slice notes).** `derive_layout` /
`for_program_facts` / the `num_regs` result are consumed by **nothing** in
the run/lowering/energy path in this diff (only `is_cpu_only` is wired; the
derived Layout is exercised only by tests). So no derivation bug can corrupt
the oracle *today*. For B-2.1, two recorded hazards: (a) a scratch-only ALU
op (`Add { rd: 8, rs: 9 }`) derives `num_regs = 1` while the crossbar must
still address positions 8..15 — the select-tree/state-field remapping at
`num_regs < 8` must handle scratch positions shifting; (b) the implicit
r0-routing-hub dependency is protected only by the `None → 1` floor — worth a
comment where it's consumed.

**F4 (INFO, pre-existing 0a, out of diff).** `Layout::num_cpu_fields` doc
comment says "at the default layout this is 24" but the formula yields 23
(8+1+1+4+8+(2−1)+(1−1)); `STATE_BITS` = 184, consistent with
`StateVals::decode` (PC_hi at field 22). Also `pc_fields()` is floor
division while its doc says `⌈w_pc / w_ctl⌉` — latent for non-multiple
`w_pc` (B-2.2's problem). Doc nits only; nothing consumes the wrong values.

## 5. Attack: does this move any PRICED energy?

**No.** The diff touches only `asm.rs`/`compile.rs`/`types.rs`/tests —
`energy.rs` is untouched. `rom_program_energy` / `rom_program_energy_resident`
price `balanced_energy(step_net)` where `step_net` is a caller-supplied
lowering of `step(rom)` ("CPU-only, consistent with cpu_money_plot",
energy.rs:397–402) — already mem-free before this change — and their trace
uses the fast byte sim (`rom_trace`), not `run_rom_on_cpu`. The routing
change aligns the *oracle* with the already-mem-free *pricing*; no priced
step_j or golden can move. Confirmed the design's claim.

## 6. What I ran (fast tier only, per constraint)

- `cargo test -p theseus-tisc --test b2_layout` — 6 passed, 1 ignored (heavy measurement).
- `cargo test -p theseus-tisc` (default features, full crate) — **288 passed, 0 failed**.
- NOT run (heavy-gated, constraint): `tests/compile.rs`, `tests/bveq.rs`,
  `tests/memexch.rs`, `tests/mde_spill.rs`, `tests/widen_pc_payoff.rs`.

**F6 (condition on SHIP).** Because every whole-domain `run_rom_on_cpu`
consumer except `b2_layout` is heavy-gated, the in-diff behavioral evidence
for the new routing is the cnot whole-domain test plus the structural
argument in §1/§3. The advisor's standard pre-merge
`--features heavy-tests --test-threads=1` authority run is **required** (it
now exercises the step path for all CPU-only programs and the system path
for all mem programs — both carriers get real coverage there).

## Findings summary

| ID | Severity | One-line | Trigger |
|----|----------|----------|---------|
| F1 | MINOR | `uses_mem` is a non-exhaustive `matches!`; a future mem-op variant silently classifies CPU-only and its opcode no-ops as reserved-identity on the step path (silent divergence, no error) | any future ISA growth adding a mem-touching instruction |
| F2 | MINOR | All zero-mem non-mem-op heavy anchors (bveq L-b, bveq_netlist T2, mde_spill, widen_pc_payoff, compile CPU-only cases) now anchor `step`, not `system_step`; emit/mem/absorb non-mem inertness loses differential coverage | future regression in emit bracketing for non-mem opcodes would be invisible to run_rom_on_cpu-based tests; add one pinned system-path CPU-only anchor (nonzero init.mem) |
| F3 | MINOR (latent) | derived `num_regs` is unconsumed this slice; scratch-only programs derive num_regs=1 and the r0-hub/select-tree remapping hazards are deferred to B-2.1 | consuming the derived Layout in lowering without remapping scratch positions |
| F4 | INFO | pre-existing 0a doc nits: num_cpu_fields "= 24" comment (actual 23/184 bits), pc_fields floor-vs-ceil doc | none (docs only) |
| F5 | INFO | confirmed: no priced energy moves; pricing already lowers mem-free `step(rom)` and doesn't route through `run_rom_on_cpu` | — |
| F6 | CONDITION | heavy-tier authority run required pre-merge (reviewer constrained to fast tier; most oracle consumers are heavy-gated) | — |

**No BLOCKER.** I attempted to construct a divergent program along four axes
(mis-classified mem op; nonzero/should-be-nonzero mem under the guard,
including the all-zero resident-input corner; CpuState semantic gap between
carriers; scratch-index inflation of num_regs) and failed on all four — each
is closed either structurally (shared `decode_execute_shared` object,
opcode-exact emit predicates, bracketed mem_body) or by the two-part guard.
Confidence: **high** on the routing/guard soundness (the equivalence argument
is structural, not per-op), **medium-high** on overall slice health pending
the heavy authority run (F6), which is already repo discipline.
