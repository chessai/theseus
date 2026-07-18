# M-D.c — heap value representation (widening + heap alloc)

**Status:** design of record (pre-review). Two increments: **M-D.c1 (MEM_WORDS
widening 4→8)** — bounded, mechanical-ish, the immediate deliverable — and **M-D.c2
(heap allocation model)** — design-heavy, deferred. This doc scopes both; only M-D.c1 is
delegation-ready.

**Why now / independence:** the leaf ceiling is `14 + MEM_WORDS` (compile.rs:230); at
`MEM_WORDS=4` it is 18 (straight-line) / 16 (branching). Programs with values beyond that
(bigger products, wider sums, shallow trees) don't compile. Widening to `MEM_WORDS=8`
raises it to 22/20. This does **not** risk a double-widen against M-D.d2 (the reversible
calling convention uses a **link register**, not a memory stack — ADR-0034 D5 — so it
needs no memory width). True unbounded heap (trees past any static ceiling) is M-D.c2.

---

## M-D.c1 — MEM_WORDS 4 → 8 (the widening increment)

### What changes
- `MEM_WORDS: usize = 4 → 8`, `MEM_WIDTH: MemWidth = W4 → W8` (types.rs:36,51). The
  static assert `const _: [(); MEM_WORDS] = [(); MEM_WIDTH.words()]` (types.rs:56) keeps
  them agreeing. **`W8` is the max the `MemWidth` enum admits** — this is the last
  widening available without extending that enum (a separate, larger change).
- `COMPOSITE_BITS`/`SYSTEM_BITS` (types.rs:106,119) scale automatically (`… + MEM_WORDS …`).
- `MAX_LEAVES = 14 + MEM_WORDS` (compile.rs:230) → 22; the branching ceiling likewise.
- The decoder's 1-of-N Fredkin **select tree is already width-parameterized and
  discharged at N∈{2,4,8}** (Conjecture C1, the `select_mem_on_bus_n` tests) — so N=8
  addressing is *already proven ancilla-clean*; widening just makes N=8 the shipped width.

### The real cost — re-establish the closure certificates at N=8 (OOM-prone)
The composite/ring bus-quiescence certificates (`tests/composite.rs`, `ring_system.rs`,
`ring_two_body.rs`, `ring_two_body_timing.rs`) lower `system_step` at the compiled
`MEM_WORDS`. At N=8 the carrier is wider (`SYSTEM_BITS` grows by `4·W = 32` bits), so the
lowering is bigger and **slower / more memory-hungry** — the exact OOM/SIGKILL class that
bit `ring_two_body_timing` and `mexch_reg` (memory: consolidate into ONE sequential
`#[test]`; bound domains). Obligations to re-verify at N=8:
- **Stage-1b composite closure** (`ring_system.rs`): `system_step` lowers ancilla-clean,
  `run == run_open == eval^R`, **bus-quiescent** at the revolution seam (Thm 3.5) — now at
  N=8.
- **Stage-2 two-body** (`ring_two_body.rs`): `two_body == SystemRing == eval^R` at N=8.
- **Stage-2b-lite timing** (`ring_two_body_timing.rs`): the depths change (wider mem body);
  re-derive `D_cpu`/`D_mem` and re-pin the golden tuple (the F-teeth golden values move).
- **MEXCHR** (`mexch_reg.rs`): `reg[ra] % MEM_WORDS` now mod 8; re-verify
  `system_ring_mexchr_matches_eval_oracle` and bus-quiescence at N=8.

### Acceptance (M-D.c1)
- Every `#[ignore]`d authoritative ring/composite/two-body/MEXCHR certificate is **re-run
  green at N=8** (advisor runs the `--ignored` tier; bound + serialize to dodge OOM —
  peak = one `system_step` lowering at a time).
- `MAX_LEAVES` ceiling tests updated to 22/20; a program with 19–22 leaves that failed to
  compile at N=4 now compiles → ROM → `run_on_cpu == eval` (L19 real-CPU, bounded domain).
- Golden timing tuple in `ring_two_body_timing` re-pinned to the N=8 depths (fails loudly
  on drift — the 2b-lite F-teeth discipline).
- No regression at the default (non-ignored) tier.

### Feasibility — MEASURED (throwaway N=8 worktree probe, 2026-07-18)
- **The `theseus-tisc` library compiles cleanly at MEM_WORDS=8 / MEM_WIDTH=W8** (`cargo
  build` EXIT 0) — NO compiler code changes needed; no hidden `N==4` / 2-bit-addr
  assumption in the lib. The select tree + BusMsg.addr + MEXCHR `% MEM_WORDS` all scale.
- **The Stage-1b bus-quiescence certificate HOLDS at N=8**: after padding the hardcoded
  `[_;4]` mem literals, `cargo test --test composite -- --test-threads=1` is **19 passed,
  0 failed** — the zero-garbage closure (Thm 3.5) is width-independent as designed.
- **Cost:** composite at N=8 = **163 s** (vs ~135 s at N=4) and **peak RSS 8.5 GB**
  (serial). So the OOM risk is real and quantified: certs MUST run `--test-threads=1`;
  the heaviest `system_step`-lowering certs (ring_system, ring_two_body, ring_two_body_
  timing) will exceed this and must run ONE-AT-A-TIME. This is the delegation's crux —
  the gate must be serial + scoped to the lighter tier; the heavy certs stay `#[ignore]`d
  and are **advisor-run** (workers can't run cargo, L5; and re-pinning the timing golden
  needs an actual N=8 run to read the new depths — inherently advisor work).

### Exact work-list (from the probe)
1. Flip `MEM_WORDS 4→8`, `MEM_WIDTH W4→W8` (types.rs:36,51). (Lib compiles as-is.)
2. Pad every hardcoded `[_;4]` mem literal to 8 across ALL test files. Confirmed in
   composite.rs (lines 572,588,612,1049 — `mem: [..4..]` → append `,0,0,0,0`); the
   `[0; MEM_WORDS]` forms auto-scale. The worker must grep every `tests/*.rs` for
   4-element `mem:` / `[_;4]` / `MEM_WORDS`-literal patterns.
3. Update `MAX_LEAVES`-ceiling assertions 18→22 (and the branching ceiling 16→20); add
   a real oracle test: a program with 19–22 leaves that FAILED to compile at N=4 now
   compiles → ROM → `run_on_cpu == eval` (the customer/value demonstration).
4. **Advisor-run + re-pin goldens** (OOM-heavy, serial): `ring_two_body_timing` depth
   tuple (`D_cpu`/`D_mem` move — the 2b-lite F-teeth golden), any `COMPOSITE_BITS`/
   `SYSTEM_BITS`/`MAX_LEAVES` literal expectations; re-run ring_system / ring_two_body /
   mexch_reg certs at N=8 serial and confirm green.
5. Gate for the worker task: `--test-threads=1`, scoped to build + clippy + `--test
   compile` + `--test composite` (the feasible tier); heavy ring certs advisor-run.

### Risks / open questions for the reviewer
1. **Does anything assume `MEM_WORDS ≤ 4` or a 4-bit `%` mask?** MEXCHR's `reg[ra] %
   MEM_WORDS` and the addr-bit width (`addr_bits = log2(MEM_WORDS)` = 2→3) — does the
   BusMsg.addr field carry 3 bits cleanly, and does the select tree's addr decode use the
   right bit count at N=8? (The tree is discharged at N=8, but confirm the *emit* seam's
   addr copy width.)
- **Does `W8` (8-bit) memory-word width interact with the `W=8` register width?** MemWidth
   is the number of WORDS, each still `W=8` bits — confirm `MEM_WIDTH.words()==8` and that
   no code conflates word-width with word-count.
3. **Certificate feasibility:** will `ring_system`/`two_body` at N=8 actually complete
   under the gate timeout, or must they stay `--ignored` + advisor-run? (Likely the
   latter; plan for it.)
4. **Balancer memory** (the known analytic-counting OOM, status.md open item): does a wider
   State push any *lowering* (not just tests) over the memory wall?

---

## M-D.c2 — heap allocation model (deferred, design-heavy)

Widening buys a bigger *static* ceiling, not unbounded data. True heap (recursive-type
values / trees whose size is data-dependent within the finitization bound) needs:
- A **memory layout for a finitized tree**: nodes in `mem[]`, addressed data-dependently
  via **MEXCHR** (M-D.b) — a child pointer is a register-held address.
- **Zero-conserving allocation** (Lemma 7.1/7.2, ADR-0044): no iso manufactures a zero;
  heap cells are budgeted from the marshalled zero supply; alloc depth ≤ available zero
  cells. Alloc must pair with dealloc (zero-parking) so the revolution closes.
- Bounded by the ADR-0035 finitization depth (heap is finite, so totality holds).
- Re-establish bus-quiescence for the alloc/dealloc traffic (each is a bus transaction).

This is its own design→review→delegate cycle after M-D.c1. **Open:** is a heap even needed
for the target `.thse` programs, or does the 22-leaf ceiling + M-D.a's finitized fold/
unfold already cover them? Answer that before building M-D.c2 (don't build a heap with no
customer — the ADR-0007 "no customer" test).

## Sequencing
**M-D.c1 (widening) now** — bounded, unlocks 22 leaves, re-certs at the enum-max width.
**M-D.c2 (heap)** only if real programs need past 22 leaves. Parallel/independent: M-D.d2
(calling convention, link-register, no memory).
