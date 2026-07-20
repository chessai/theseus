# M-D.e slice 1 — loop-carrier scratch-spill (minimal, machinery-proof)

> **Revision r2 — supersedes the r1 specifics below.** The r1 design-review
> (`docs/reviews/mde-spill-design-review.md`) verified the paging MECHANISM sound (I1-I5)
> but rejected the r1 minimal-test specifics (F5 allocation, F6 inexpressible body). Both
> are now RESOLVED and empirically de-risked by a throwaway spike (`docs/scratch/mde-spike/`
> — the recognizer/cap diff + `spike_mde.rs.ref`), built after the whole value-width ladder
> merged (so the F6 `recognize_prim` collision with f3.5 is GONE). r2 = r1's verified
> invariants + the spike's concrete construction + the review's rationale fixes:
> - **(F5, u-wide) allocation:** carrier `Sum(N5, Bit)` — u = `nat_chain(5)` (a 4-leaf, 5-
>   inhabitant nat, k_max=6), payload `Bit`; `num_leaves = 5` (tag + max(4,1)). **4 payload
>   leaves** → with `MAX_LEAVES_LOOP` bumped **3→4**, payload leaf 4 homes in **Scratch(2)**;
>   NO live-data relocation (the wide u fills the low registers canonically; the spilled leaf
>   is scratch-homed by `home_of`, marshal zeroes it). The 5-payload case still rejects
>   (cap-by-one).
> - **(F6, k-leaf recognizer):** a `check_pattern_kleaf` arm recognizing a pinned linear map
>   over k=5 carrier leaves — the spike's "thermometer walk" `R0→L4→L3→L2→L1→L0→R1→R0`,
>   emitted as a **Swapbr-free 13-instruction sequence** (Xor/Xori/Cswap) that TOUCHES ALL
>   payload leaves incl. the spilled leaf 4. Slots into `recognize_prim` cleanly now (no f3.5
>   collision). This is the F6 vocabulary extension in minimal (pinned) form — the design
>   review should decide whether to keep it pinned (test-scaffold, minimal) or make it a
>   modest k-register-linear-map generalization (more useful, bigger). It is NOT a useful
>   program (a synthetic walk) — this slice is a MACHINERY-PROOF (like M-D.d), proving Trace
>   spill closes across the back-edge; useful variable-T loops (GCD/Collatz) need the rest of
>   the M-D.e cluster (widen-PC + typed bounds).
> - **(F1b) safety rationale:** paging is reversible via the **self-inverse exchange bracket**
>   (`Xchg spare,s_k; op; Xchg spare,s_k`, net-zero for ANY spare value) — NOT because aux=0;
>   assert the paging spare is an aux index if relied on (T1).
> - **(F2) precedent:** the f3/fold d=4 spill is bracket-inside-repeated-straight-line-code and
>   does NOT exercise the Trace back-edge; the back-edge safety rests on the skeleton walk +
>   the spike's real-CPU run (first-of-kind).
> - **THE SPIKE'S RESULT (to be re-confirmed by the design review's own build):** the 4-payload
>   `Trace(N5, walk)` COMPILES; input 0 takes **6 iterations (5 back-edges)** with the spilled
>   leaf 4 live across the jump; the ROM contains the paging `Xchg{_, sx≥2}` inside the loop
>   body; `spike_mde_spill_trace` (fast oracle over the 2 Bit inputs) + `_real_cpu` (real
>   system_step, L19) + `reject_5_payload` (cap-by-one). My own re-runs timed out on the cold
>   compile under load; the design review must re-build and run the DEFINITIVE real-CPU across
>   the back-edge — that is the go/no-go this slice hinges on.
> - **Tests (F6 strength):** T1 pins the paging Xchg BETWEEN the Ltop/Lend offsets (not a spine
>   relocation); T3 asserts ≥2 iterations (input 0 → 6, exercising the back-edge with a paged
>   leaf); T-reject(5) confirms the cap.
> **First-slice scope decision (for the review):** fold the k-leaf recognizer + the spill into
> ONE slice (the recognizer exists to enable the spill body; f3.5 collision is gone). widen-PC
> and typed iteration bounds are SEPARATE later slices of the cluster.


**Milestone.** The first bite of the M-D.e cluster (loop-carrier spill + widen-PC +
typed-bounds tighten; ADR-0007 TH-1b). This slice does **only** loop-carrier spill, and
only to **scratch** (not mem), for **one** payload leaf over the register cap. It is a
*machinery-proof* slice (like M-D.d was): it does not by itself unblock GCD/Collatz
(those need the whole cluster), but it proves the reversible loop can carry more payload
than fits in registers. Concurrent **Thread B** of the f3.5 ‖ spill pair.

**This touches the reversible loop lowering (`lower_trace`) — the theseus analog of
orchestration control flow: a bug here corrupts every loop it compiles.** Full
adversarial-review + reproducing-test bar (CLAUDE.md; L19/L20/L21).

## 0. Why this is not "just bump `MAX_LEAVES_LOOP`"

Register budget in a reversible loop body (deep-dive, verified):
`lower_trace` needs **4 aux** (rc, rb, temp, ctrl — unpageable, live across the
back-edge; decoder.rs skeleton hardcodes their reg indices) + **1 tag** (also
unpageable — the join-dispatch `XOR ctrl, tag_reg` reads a fixed reg). That is 5 of 8
registers reserved, leaving **3 for payload** — hence `MAX_LEAVES_LOOP = 3`
(compile.rs:346), a hard `Deferred` reject at ~L2047.

At **4 payload** leaves, register-needs = 4 payload + 1 tag + 4 aux = **9 > 8**. Exactly
one thing must live off-register. The aux and tag are unpageable, so the **4th payload
leaf must home in scratch (s2+)** and be paged into a spare register for each body
instruction that uses it. This is a change to the loop's **register allocation**, not a
constant bump.

## 1. The mechanism this reuses (and its one subtlety)

Straight-line code already spills: `with_registers` (compile.rs:448) pages a
scratch-resident operand into a spare register (`Xchg spare, s_k`), runs the
instruction, pages back (`Xchg spare, s_k`) — net-zero, self-inverse. The question is
whether this survives **inside a loop body**, across the back-edge.

**Precedent that it does:** f3's d=4 displaced regime already pages a displaced carrier
leaf (idx → Mem(2)) inside the `iter` body, and the paging spare is an **aux register**
(rb), and f3's d=4 tests pass (run_on_cpu==eval + closure). So aux-as-paging-spare
inside a loop is already exercised and green — for **mem** paging under `iter`. This
slice extends it to **scratch** paging under `Trace`.

**Why aux-as-spare is safe (the argument the review must confirm):** at body entry the
witness is cleared — `rc=rb=temp=ctrl=0` (deep-dive; the Ltop join-dispatch zeroes them
before the body runs). The body never names an aux leaf (aux are not carrier leaves), so
they stay 0 throughout the body. `with_registers` picks a register with no leaf-home as
its spare — that will be an aux reg (=0). `Xchg aux, s_k` swaps the leaf value into aux
and 0 into s_k; the instruction runs; `Xchg aux, s_k` restores. Net: aux back to 0, s_k
back to the leaf. Reversible and layout-preserving. **The review must verify no path
leaves an aux non-zero at the back-edge** (which would corrupt the δu token / dispatch).

## 2. The invariants a correct spill MUST preserve (deep-dive §5)

| # | Invariant | What spill must do |
|---|-----------|--------------------|
| I1 | `homes_in == homes_out` (enforced compile.rs:~L2153) | The spilled leaf's home is **permanently** Scratch(k) — paging is page-in/use/page-out **within** the body, net-zero, so entry layout == exit layout. |
| I2 | 4 aux + tag **unpageable**, resident, live across the jump | Allocation must reserve their registers FIRST; only *payload* spills. `with_registers` may borrow an aux reg as a transient spare **only because aux=0 during the body** and it restores it. |
| I3 | δu token circulation exact (iter_len-dependent) | `iter_len` is computed from `body_instrs.len()` **after** lowering (compile.rs:~L2160), so the paging Xchgs ARE counted → δu stays consistent. Do NOT compute iter_len before paging is emitted. |
| I4 | WCET ≤ 256 (8-bit PC) | Paging lengthens the body → larger `k_max·iter_len`. Minimal slice keeps k_max tiny so it fits comfortably; the WCET check (compile.rs:~L2169) already accounts for the longer body. |
| I5 | `k_max = |u|+1` bound | Structural; unaffected by paging. |

The failure mode if I1 breaks (concrete, deep-dive §2): iteration 2 re-enters Ltop and
reads the tag/carrier from stale homes → wrong `ctrl` → wrong continue/exit decision →
wrong answer or PC wrap. This is why the layout-invariant guard must STAY and must pass.

## 3. The change (compile.rs only — no asm/decoder/step/recognize_prim)

1. **Allocation (`lower_trace`, the carrier-home assignment ~L2101–2125):** when
   `n_payload > 3`, assign the first 3 payload leaves to registers and the overflow
   (leaf 4 for this slice) to **Scratch(SCRATCH_SPILL_BASE=2)**, keeping tag + 4 aux in
   registers. Reuse `home_of`'s scratch placement rules; do NOT place any aux/tag in
   scratch.
2. **Guard (~L2047):** raise the cap from `MAX_LEAVES_LOOP = 3` to a new
   `MAX_LEAVES_LOOP_SPILL` = 3 + (available scratch spill slots). For THIS slice, gate
   it conservatively at **4** (one scratch leaf) and keep the reject for >4 — we grow it
   only as far as the test proves. (Document the true ceiling but do not enable
   untested widths — L19: no silent over-reach.)
3. **Body lowering (~L2221):** unchanged — the existing `lower()` → `with_registers`
   pages the Scratch-homed leaf automatically. Verify the emitted paging uses a
   genuinely-free (aux) spare and is net-zero.
4. **Layout-invariant guard (~L2153):** **keep unchanged** — it must still pass (paging
   is internal, homes net-zero). If it fires, the allocation is wrong; do not weaken it.
5. **WCET accounting (~L2160/2169):** verify `iter_len` includes the paging instructions
   (it will, if computed post-lowering). Add an assertion/comment pinning this.

## 4. Minimal testable slice (the reproducing scenario — CLAUDE.md bar)

A `Trace` whose carrier has **exactly 4 payload leaves**, whose body **actually operates
on the spilled (4th) leaf** (so paging is exercised — an identity body would page
nothing and prove nothing), that compiles, runs, and round-trips:

- Carrier: `Sum(Bit, P)` where P is 4 payload bits (tag + 4 payload = the 4-over-cap
  case). `u` small (e.g. `Bit`, |u|=2 ⇒ k_max=3) so WCET is tiny.
- Body: a reversible bijection with `homes_in==homes_out` that **touches the 4th leaf**
  — e.g. flip the spilled bit (`SwapSum` on the scratch-homed leaf), plus enough
  structure to be a legal Trace body. Exact carrier/body pinned by the probe.
- Assertions:
  - **T1** — compiles (does NOT hit the `Deferred` cap) at 4 payload; the ROM contains
    `Xchg{_, sx>=2}` paging instructions (proves the 4th leaf spilled to scratch).
  - **T2** — `run_on_cpu_fast_resident == eval` over the whole carrier domain (small).
  - **T3** (#[ignore], real CPU) — `run_on_cpu` (real `system_step`) == eval on a sample
    that **sets the spilled bit to 1** and exercises ≥2 loop iterations (k≥1), so the
    page-in/out reversibility across the back-edge is actually tested (no all-zeros
    blind — L19 I-2 lesson).
  - **T4** — the layout invariant held: assert compile succeeds AND (if a debug hook
    exists) `homes_in==homes_out`; at minimum, the round-trip in T2/T3 proves it.
  - **T5** (#[ignore], closure) — SystemRing closure `validate()==Ok` / run==eval^R on
    the spilled-carrier ROM (the aux-as-spare paging stays bus-quiescent).
  - **T-reject** — a Trace with **5** payload leaves still rejects (`Deferred`) — the
    cap moved by exactly one, no silent over-reach.

## 5. Probe (do FIRST, before delegating — go/no-go)

Spike in a throwaway branch:
1. Bump the cap to 4 and add the scratch-home assignment for the 4th payload leaf.
2. Build the minimal 4-payload Trace (§4) and try to compile it.
3. Check: does it hit the layout-invariant guard? Does `with_registers` page the scratch
   leaf using an aux spare? Is the ROM's paging net-zero (does run_on_cpu==eval)?
4. **Set the spilled bit and run ≥2 iterations on the real system_step** — the
   back-edge reversibility acid test.
De-risks: (a) whether aux-as-spare holds for scratch paging under `Trace` (not just mem
under `iter`); (b) whether the layout guard needs the allocation done differently; (c)
the measured WCET. If the closure breaks or homes_out≠homes_in, STOP — the design
changes (this is the daemon-control-flow-analog; do not ship a corrupt loop).

## 6. Delegation shape

One maestro task (tier 0, theseus profile): the allocation change + guard bump + the
minimal-slice test file. `file_allowlist`: `crates/theseus-tisc/**` (the change is
confined to `compile.rs` + one new test file; broad glob avoids scope-lockout on any
loop-lowering test that needs a tweak — [[maestro-allowlist-scope-visibility]]).
`check_commands`: `cargo clippy` + `timeout 1800 cargo test -p theseus-tisc`
(full suite — a loop-lowering change can rot any Trace/iter test).

## 7. Concurrency (Thread B of the f3.5 ‖ spill pair)

Touches **only** `compile.rs::{lower_trace, home_of, MAX_LEAVES_LOOP}` + a new test —
disjoint from f3.5's `asm.rs`/`decoder.rs`/`compile.rs::{recognize_prim, exec_instr}`.
The one shared file (`compile.rs`) is edited in **non-overlapping functions**. **Merge
order: f3.5 FIRST**, then this rebases/re-gates on the new tip
([[maestro-no-merge-under-inflight-task]]) — a loop-allocation change and a
recognizer/decoder change do not conflict textually, but the gate must diff against the
merged tip, so serialize. If a textual conflict in `compile.rs` does arise on rebase,
it is localized and mechanical (different fns).
