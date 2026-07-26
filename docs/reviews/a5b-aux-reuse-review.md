# A-5b adversarial review — aux REUSE (`Ctx.reusable_aux`) for deep loop-nesting

**Reviewer:** fresh-context agent (Fable 5), did NOT write the change. Read-only
review of `a5b-scratch-frames` @ `6525dbc` (diff vs `6b54d9d`), design doc
`docs/scratch/a5b-deep-nesting-design.md`. All probes run as real cargo tests in
a *separate* worktree (`/tmp/a5b-probe`, detached at the same commit —
`crates/theseus-tisc/tests/a5b_adversarial_probes.rs`, `a5b_rom_dump.rs`,
`a5b_real_cpu_probe.rs`); `/tmp/a5b` sources untouched.

**Verdict up front: no BLOCKER found. The attack-1 liveness premise held against
every shape I could construct and against instrumented step-by-step register
tracing. Two MINOR findings + one verification-gap finding (the shipped suite
does not contain the runtime witness the design §5.1 promised — my probes supply
it and are green). SOUND TO MERGE after the advisor's real-`system_step`
authority run (see §6 for exactly what that run must contain).**

---

## Attack 1 — the liveness premise (the crux): **PASS**

Claim under attack: the outer Trace's aux `{rc, rb, temp, ctrl}` and BR are all
ZERO across the *entire* body region, for every admitted body shape, so a nested
Trace may reuse them.

### Static argument (why it holds for ALL shapes, not just the tested ones)

1. **Only the outer skeleton's own bracket instructions ever name the frame
   registers.** The frame is drawn then occ-marked into `body_occ` *before* the
   body is lowered (`compile.rs:3565-3569`, `allocate_aux_frame` marks all four
   at `compile.rs:842-846`), so every allocator that runs while lowering the
   body — skip-block `allocate_aux` (free-per-occ only, `compile.rs:703-705`),
   the inner Trace's tag/padding ancilla draw (`free_regs` filter,
   `compile.rs:3472-3474`), prim recognition operand slots — sees them as taken.
   The ONLY consumer that can name them inside the body is a nested
   `allocate_aux_frame` via `reusable_aux` (sole read sites: `compile.rs:838`
   candidates filter, `compile.rs:3494` reservation; `allocate_aux_frame` is
   called only from `lower_trace`).
2. **Both body-entry paths leave all four aux + BR at 0.** First entry:
   prologue `ADDI temp,δu` → Ltop `SWAPBR rc` (BR=0 ⇒ rc unchanged 0) → `CSWAP
   ctrl,rc,temp` (ctrl=0 ⇒ no swap) → `SUBI temp,δu` burns the token → `XOR
   ctrl,tag; XORI ctrl,1` with tag=1 ⇒ ctrl=0. Back-edge entry: at Lend the
   token moved temp→rb→BR (temp=0, rb=0), ctrl=1; at Ltop `SWAPBR rc` absorbs
   BR (rc=δu, BR=0), `CSWAP` with ctrl=1 swaps rc↔temp (rc=0, temp=δu), `SUBI`
   burns it, ctrl uncomputed with tag=0 ⇒ 0. Either way the body region starts
   with `rc=rb=temp=ctrl=BR=0`, and nothing in the body can write them except a
   nested net-zero Trace bracket.
3. **The come-from is never register-resident across the body** — it is re-mint
   from the compile-time δu immediate (`ADDI temp,δu` *after* the body), so
   there is no outer token for the inner loop to clobber. The outer transient
   `temp=δu` lives only in the outer straight-line bracket (post-body
   mint→CSWAP→Lend and Ltop→burn), where no body instruction can interleave.

### Dynamic witness (instrumented probes — all green)

`run_trace_instrumented` steps the assembled ROM instruction-by-instruction
(same byte semantics as `run_on_cpu_fast`) and **asserts, at every crossing of
the outer body-entry (pc=7) and body-exit (pc=7+body_len) boundaries, that all
four outer aux registers AND BR are zero**, requires ≥2 outer iterations, and at
termination requires the FULL machine state (all regs, all scratch, all mem, BR)
to equal `marshal(output)` — zero residue anywhere (something
`run_on_cpu_fast==eval` alone cannot see, since unmarshal projects only output
leaves). Shapes probed, all `== eval` on the whole domain:

| probe | shape | attack angle | result |
|---|---|---|---|
| P1 | shipped depth-2 (`Seq(Plus(Id,inner),SwapSum)`) | baseline + residue | PASS (aux 0 at all 4 crossings, terminal clean) |
| P2 | inner loop in the **Left arm** | reuse region runs *after* an outer back-edge has cycled the come-from | PASS |
| P3 | inner loops in **both arms** | reuse bracket on every outer iteration | PASS |
| P4 | **two sequential** inner-loop regions in one body, carrier op between | frame torn down/rebuilt twice per iteration | PASS |
| P6 | inner `Trace(Bit,·)` with a **padding leaf spilled to scratch** inside the nest (prim body, inner iterates 3×) | reservation arithmetic + scratch traffic | PASS (terminal scratch clean too) |
| P7 | `Sym(outer)` (adjoint / reverse direction) | mirrored skeleton reuse | PASS (fast==eval whole domain) |

ROM-level non-degeneracy was also verified directly (`a5b_inner_backedge.rs`):
the shipped depth-2 ROM takes the **inner** back-edge (pc 22→12) *and* the
**outer** back-edge (pc 33→2), interleaved — two distinct Ltops exercised, so
the reuse bracket genuinely runs inside a genuinely-iterating outer loop.

Two legal-looking shapes REJECT gracefully rather than corrupt (fine, but note
the capacity reality): **P5 direct nesting** (`Trace(One, Trace(One, prim))`,
no Plus at all) rejects on `Trace WCET 336 exceeds 256` (the nested WCET fold
multiplies fast — depth-2 is usable only for small bodies); **P5b** the
combinator version rejects on the F7 AssocSum-lowering deferral.

**No corrupting nest was found, and I could not construct one even in principle:
any instruction that could make an outer aux live inside the body would have to
be emitted against a register the occ-mark hides from every body-side allocator.**

## Attack 2 — reusable_aux leak to a skip-block: **PASS** (one fragility note)

`allocate_aux` (skip-block `{rb,temp}`) filters on `ctx.occ.is_free` ONLY
(`compile.rs:703-705`); `reusable_aux` is merely *propagated* through its return
ctx (`compile.rs:786`), never consulted. Verified per call site:

- `lower_controlled_idiom` (`compile.rs:2632`): rb/temp/ctrl occ-marked into
  `occ_tgt` before lowering g (`2645-2648`) — a nested Trace inside g avoids
  them; they are never in `reusable_aux`.
- `lower_data_dependent_iter` (`3219`, R4): rb/temp occ-marked (`3233-3234`);
  additionally a Trace in an Iter body is doubly rejected (loop-terms tripwire
  `3249-3257` + non-canonical homes). Probed: rejects gracefully.
- `lower_bare_plus` (`4061`, R5): rb/temp/tag occ-marked into `occ_arm`
  (`4077-4080`) before both arms.
- `lower_fold_unfold` (`3954`): **rb/temp are NOT occ-marked before the per-copy
  body lowering** (`body_ctx.occ = dctx.occ`, `3984`). Today this is unreachable
  for the reuse path: Guard 2 (`4005-4009`) rejects any fold body whose lowering
  produced a Swapbr, so no Trace survives under fold and the A-5b propagation at
  `3987` is dead code. **Fragility note (MINOR-3, same pattern the
  compositional-control review called "protection by accident of a different
  guard"):** if fold ever admits inner control, the fold's live `{rb,temp}` are
  invisible to a nested frame draw. Not reachable now.

R4/R5/R6 occ-marks are intact in the diff (untouched lines).

## Attack 3 — propagation through split/merge/arms: **PASS**

The ONLY producer of `reusable_aux` bits is `lower_trace`'s `body_ctx`
(`ctx.reusable_aux | level_frame_mask`, `3618-3624`); `lower_trace` returns
`ctx.clone()` (`3877`) so its frame never leaks to sequential successors or
siblings. `split`/`merge`/arm ctxs copy the parent's mask unchanged — correct,
because the invariant "reusable = frames of Traces whose body strictly encloses
this point" is region-scoped: any point inside the outer body (either Times
factor, either Plus arm, any Seq position, any fold copy) is a point where those
frames are dead. The mask can never contain a carrier, sibling, tag, or
skip-block aux register: all of those are drawn from `free(occ)` while the
enclosing Trace keeps its frame occ-marked, so `reusable ∩ free(occ) = ∅` at
every draw. Sibling F-1 occupancy is untouched (frame drawn against `occ` which
already holds `Times` sibling leaves; `nested_aux.rs` Times tests green).

## Attack 4 — ancilla reservation under-reserve: **PASS**

`reusable_avail = min(popcount, 4)`, `frame_free_needed = 4 - reusable_avail`
(`3494-3496`). Worst case is a *spurious reject*, never aliasing: the ancilla
regs are drained from `free_regs` and occ-marked (`3546-3548`) before the frame
draw, and `allocate_aux_frame` still requires ≥4 candidates or rejects. When
`popcount ≥ 4` (reservation drops to 0) the frame is feasible from reusable
slots alone, so handing all free regs to tag/padding cannot strand the frame.
The union-of-ancestors mask can exceed 4 bits only when an intermediate frame
partially drew fresh free regs — those are still dead ancestor-aux at any deeper
point, so any 4 of them are safe. Padding beyond the budget spills to *free*
scratch (occ-aware), never relocating live data — probed end-to-end by P6
(terminal scratch clean). The tag/live-input guard `n_input+1 >
carrier_reg_budget` (`3433`) is untouched.

## Attack 5 — single-level byte-identity: **PASS (proven cross-revision)**

Statically: with `reusable_aux == 0` the candidates filter reduces to the old
`is_free` draw, `frame_free_needed = 4` reproduces the old reservation, and the
debug byte-identity assert is correctly re-gated on `reusable_aux == 0`
(`3592-3594`). Empirically: I compiled the same 5 lower_trace-exercising
programs (single tag-only Trace; Trace with Bit payload via prim; Trace with
u-wider-than-a padding; `Times(Trace, Id(Bit))`; `Times(Id(Bit), Trace)`) at
**both** `6b54d9d` and `6525dbc` and diffed the full instruction dumps:
**byte-identical** (`a5b_rom_dump.rs`, `/tmp/rom_old.txt == /tmp/rom_new.txt`).
`single_level_trace_rom_byte_identical` and the Times ROM-compare tests in
`nested_aux.rs` also pass.

## Attack 6 — what the real-`system_step` authority must add: see §6.

---

## Findings

**FINDING 1 (verify-gap, MAJOR against the stated bar, no code bug found):
the shipped suite lacks the runtime witness of the liveness premise that design
§5.1 explicitly promised.** `depth2_shared_aux_is_reused_not_disjoint` is an
*allocation-level* white-box check (inner frame ⊆ outer frame via a test hook);
it never runs a program and dumps state. `run_on_cpu_fast == eval` cannot see
residue in non-output registers/scratch (unmarshal projects output leaves;
fast has no terminal BR/s0/s1 check and no P2 park guard, unlike
`run_rom_on_cpu`). A reuse bug that left dirt in a reused aux while still
producing the right output leaves would be invisible to every shipped A-5b
test. My instrumented probes (P1-P4, P6: aux+BR zero at every outer-body
boundary crossing, ≥2 outer iterations, full terminal state == marshal(output))
supply exactly this witness and are all green — recommend adopting them (or the
boundary-instrumented runner) into the suite per L20 before/at merge.

**FINDING 2 (MINOR): depth-3 rejects with a nonsense diagnostic and the design's
"arbitrary depth" headline is overstated.** Depth-3 tag-only (Plus-wrapped, the
only currently-compilable nesting idiom) rejects with `program needs 1 leaves
but maximum is 16` — `allocate_aux`'s `TooManyLeaves{needed: ctx.homes.len()=1,
max: 16}` fired on its free-slot/displaceable shortfall path, a misleading
message for a register-exhaustion reject (the shipped
`depth3_nested_trace_compiles_or_rejects_gracefully` only matches it via the
broad "leaves"/"maximum" strings). Design §2.3's "arbitrary depth fits, O(1)
aux" holds for the *frame* only: each nesting level still spends 2 live
skip-block aux + 1 tag, so real depth is 2 today. Reject is graceful
(reject-don't-corrupt confirmed) — fix the message, temper the doc claim.

**FINDING 3 (MINOR, fragility): fold's per-copy body ctx does not occ-mark the
fold's own `{rb,temp}`** (`compile.rs:3984`), and the A-5b `reusable_aux`
propagation into fold bodies (`3987`) is dead code guarded only by the post-hoc
Guard-2 Swapbr reject. Unreachable today; becomes a live frame-aliasing hazard
the day fold admits inner control. Add the R4/R5-style occ-mark when that
happens (leave a comment now).

**Observation (capacity, not a defect):** the practical depth-2 envelope is
narrow — direct nesting without Plus (P5) already blows the 256 WCET budget
(`WCET 336, iter_len=90`) for a 2-operand prim body, and heavy shapes
(Times-sibling + nested Trace with payload) reject on register budget. The
tests' tag-only programs are near the *only* inhabitants of the current
envelope; fine for the machinery-proof, worth stating in status.

## Regression state

- `deep_nesting` 4/4, `nested_aux` 8/8, `nested_programs` 7/7 pass at `6525dbc`.
- Heavy tier `--features heavy-tests --test compile -- --test-threads=1`: run
  started by this review (result reported in the final message; note the bare
  `--test compile` invocation silently runs **0 tests** without the feature
  flag — do not be fooled by a green no-op).
- Adversarial probes: 10/10 pass (`a5b_adversarial_probes.rs`).
- Cross-revision ROM diff: byte-identical (attack 5).

## §6 — what the advisor's REAL `system_step` authority run must contain

Fast==eval shares `exec_instr` between the oracle and the k_max walk, so the
following are exactly the things only the real datapath can witness for the
reuse case:

1. **Real decoder/crossbar routing of the reused indices**: `SWAPBR`/`CSWAP`/
   `ADDI/SUBI/XORI/XOR` operands on the *shared* frame registers, driven through
   `select_operand`/`swapbr_core` — `exec_instr` is an independent Rust
   re-implementation and would agree with itself even if the netlist disagreed.
2. **Come-from composition across depth on the real ring**: `pc_add_br; pc_inc`
   across *interleaved* inner/outer back-edges (≥2 inner back-edges inside ≥2
   outer iterations), not the software `pc = pc + br + 1` loop.
3. **Terminal quiescence** (`BR/s0/s1 = 0`) — built into `run_rom_on_cpu`,
   absent from fast. Ideally also full-state cleanliness vs `marshal(output)`
   (Finding 1) on the real final state.
4. **P2 park-safety** (`budget + s_max ≤ 256` enforced, parked PC does not
   wrap) — fast skips the guard entirely.
5. **eval^R**: the adjoint program (`Sym(outer)`) on the real CPU, or the
   backward ring run, whichever is the house convention.

Programs (all small-domain, One/Bit carriers, no A-1 widen needed):
- the shipped depth-2 shape (`Seq(Plus(Id,inner),SwapSum)`) — whole domain;
- the **P3 both-arms** variant (reuse bracket on every outer iteration) and/or
  **P2 left-arm** variant (reuse after an outer back-edge) — strictly stronger
  than the shipped shape at zero extra register cost;
- the **P6 padding-spill** shape (real bus/scratch traffic inside the nest);
- `Sym` of the shipped shape.
A `depth2_nested_trace_real_system_step` probe (`a5b_real_cpu_probe.rs`,
`run_on_cpu` + `theseus_eval::eval`) was started by this review; its result is
in the final report. Whatever the advisor runs, per L20 the real-CPU depth-2
test should land in the heavy tier, not remain a one-off.

## Verdict

**No BLOCKER. The liveness premise (attack 1) holds** — statically for all
admissible shapes (the occ-mark hides the frame from every body-side allocator;
both entry paths zero the frame; the token is immediate-minted, never
register-carried across the body) and dynamically under instrumented tracing on
six adversarial shapes including after-back-edge reuse, double reuse regions,
and scratch-spilled padding. Skip-blocks can never draw from `reusable_aux`
(attack 2), propagation is region-correct (attack 3), the reservation can only
spuriously reject, never alias (attack 4), and single-level byte-identity is
proven across revisions (attack 5). **SOUND TO MERGE once the §6 real-authority
run is green**, with Finding 1 (adopt a runtime tear-down witness into the
suite) and Finding 2 (diagnostic + doc claim) addressed at the advisor's
discretion — Finding 1 is the one I would insist on per the repo's own L20
discipline.
