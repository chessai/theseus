# M-D.w DESIGN review (WCET budget + reversible park-at-end)

> Verbatim fresh-context adversarial review (model: Fable), recovered from the
> ephemeral Agent-tool transcript (agentId `a2d6107dbdab2e9b4`) and committed per maestro L21.
> Findings and their test mapping are indexed in [README.md](README.md).

---

All load-bearing claims are now verified against source, and I ran the compile test suite to get real program sizes. Compiling the review.

---

**(A) VERDICT: SHIP-WITH-FIXES** — the park mechanism itself is sound and (surprisingly) already in live use today, but the design's central safety precondition **P2 (`B ≤ 256`) is arithmetically wrong** and its own T2 test as specified would fail on the design's chosen test program (fib). Fix findings 1–3 in the design text before delegation; nothing here invalidates the overall approach.

## (B) Findings

**1. BLOCKER — P2 (`B ≤ 256`) does not prevent PC wrap; the parked PC is `B + S`, not `B`.**
Evidence: `PC += BR + 1` each step (step.rs:222-232, compile.rs:1886). Over a trace of `T` steps reaching `PC = rom.len()`, total displacement is `rom.len() = T + S`, where `S` = sum of forward skip distances taken by that input. After the full budget `B`, parked `PC = rom.len() + (B − T) = B + S`. So a program with `B ≤ 256` but any taken skips parks at `PC > B`, and wraps into live ROM whenever `B + S > 256`. Concrete construction legal under all current static checks: `emit_skip_block` allows bodies up to 255 (compile.rs:602) and `assemble` allows 256 words (asm.rs:294-296) — a Plus with a 130-instr arm in a 135-word ROM, input taking the skip: T=5, idle=130, PC walks through [135,256), wraps, and **re-executes rom[0..9] during "idle"**, silently corrupting output on both the real iso and the fast sim (both wrap mod 256 with no trap: types.rs:25, compile.rs:1886). This is a *latent bug in today's harness*, not one M-D.w introduces — current tests are safe only because programs are tiny (I ran the suite: largest compiled program is 34 instructions; see finding 4).
Fix: the precondition must bound the **parked PC**, not the step count: `B + S_max ≤ 256`, where `S_max` = static max total forward-jump distance (for current codegen: Σ of all skip-block offsets, i.e. Σ `body_len` over every `emit_skip_block`; for Plus both arms are skip blocks and every input takes exactly one). Equivalently `rom.len() + (B − T_min) ≤ 256`. The design's §8 Q1 self-answer ("first_halt_pc ≤ rom.len() ≤ B ≤ 256 bounds it") conflates step count with PC position.

**2. BLOCKER (consequence of 1) — T2 as specified fails on fib.**
T2 sweeps `k in 1..=(256 − rom.len())` extra steps and asserts stable output. But fib is data-dependent `iter` compiled by guarded unroll with skip blocks (compile.rs:1451-1457; fib = 24 instrs with `Swapbr` pairs, confirmed in test output). For an input with counter 0, parked PC after `rom.len()` steps is already `rom.len() + S`; at the top of the sweep, PC = `256 + S` → wrapped → assertion fails (or worse, passes coincidentally). T2's sweep bound must be per-input (`256 − rom.len() − S(input)`), or use a genuinely straight-line program for the full-dead-zone sweep and a skip program separately.

**3. MAJOR — hidden park precondition the design misses: `s0 = 0` at terminal.**
The design claims the idle cell is unconditional identity. On the **fast sim** it is (`Addi{rd:0,imm:0}` → `wrapping_add(0)`, compile.rs:1833-1835). On the **real iso it is not**: IR=0 fires the ADDI predicate (`i_pred(0b000)` — all-zero opcode bits match IR=0, decoder.rs:688-690), and `addi_core` adds **`s0 ⊕ imm`** into r0 (decoder.rs:43-44: "the TK adder adds `s0 ⊕ imm`"; decoder.rs:662-686 — imm=0 masks nothing, gated TK adds s0 into the r0-slot). So each real idle step executes `r0 += s0`. If `s0 ≠ 0` at terminal, the real CPU corrupts r0 every idle step while the fast sim stays frozen — a differential-oracle blind spot. In practice `s0 = 0` is an invariant for compiled programs (marshal zeroes it and spills only to s2..s7, compile.rs:1710-1712; XCHG rejects s0/s1, asm.rs:212-214; swap-fetch restores s0), so nothing is broken today — but the design must add **P3: s0 = 0 (and s1 = 0) at every terminal configuration** to §3.1, assert it in the ledger, and state the proof obligation (unreachable-by-legal-ISA).

**4. MAJOR (good news, but design should say it) — the park convention is *already live* in every skip-taking test today.**
The current `budget = rom.len()` (compile.rs:1766) already over-runs for any input that takes a forward skip: swapMaybe (7 instrs, 1-instr skip body) idles 1 step past ROM end; fib's guarded unroll idles up to `S` steps; every Plus input takes one of the two skips. All 17 `*_real_cpu` tests in tests/compile.rs are green, so forward-idle-as-identity on the real `system_step` is **already empirically proven** on today's programs. Two consequences: (a) T2's core property has silent existing coverage — good; (b) M-D.w is not "introducing" park, it's *documenting and bounding* a convention that shipped unexamined, with finding 1's wrap as its latent failure mode. The design should reframe §3 accordingly — it strengthens the case for doing M-D.w now.

**5. MINOR — `B ≥ rom.len()` should be an explicit assert.** Nothing today can make `B < rom.len()`, but M-D.d's accumulator (spine + Σk·|body|) could undercount if a body is excluded from the spine and `k_min = 0`; `B < max trace` means silent truncation (the §1 failure mode). Cheap invariant: `assert!(B >= rom.len())` plus B ≥ T for the k-bounds.

**6. MINOR — eval fuel is shared across the whole run.** `run_on_cpu` gives `fuel = 8_000_000_000` for *all* budget steps combined (compile.rs:1774-1778). A larger M-D.d budget could exhaust it — the failure is loud (`Deferred("CPU eval failed")`), but worth scaling fuel with B.

**7. MINOR — citation nits (all substance verified correct).** `decode(0)` = `Addi{rd:0,imm:0}` is asm.rs:270-278 (pad bit 0 ok, opc=000), not :277. The "rom[pc]=0 past end" claim cites step.rs:15, which is a doc comment; the code-level guarantee is `fetch()` at step.rs:127-140 — entries are built only for `p < rom.len()` with nonzero words, so out-of-range PC leaves IR=0. compile.rs:1766 (budget), :1702 (`br: 0`), :644/650 (Swapbr pair), :1880 (fast-sim zero fetch), types.rs:25 (W=8), asm.rs:294-296 (≤256) all check out exactly.

## (C) Answers to §8 questions

**Q1 — Is `B ≤ 256` right?** **No — too weak** (finding 1). `first_halt_pc + idle_span ≠ B`; parked PC = `B + S(input)`. Correct: `B + S_max ≤ 256`. Separately, `first_halt_pc` cannot overshoot `rom.len()` today: skip jumps target the come-from `Swapbr` *inside* the block (compile.rs:644-650), never past ROM end, and the last trace step always has BR=0 — so `first_halt_pc == rom.len()` exactly. M-D.d should assert all jump targets < rom.len().

**Q2 — BR=0 at terminal: guaranteed?** **Yes, for every `compile()` output.** `Swapbr` is emitted *only* by `emit_skip_block` (grep: compile.rs:644, 650 are the sole constructors), which is called from exactly four sites — controlled idiom (:1063), guarded-unroll iter (:1455), fold/unfold (:1602), Plus (:1678/1683). I traced all four skip/fall-through cases: skip-taken loads BR=body_len for one step and the come-from `Swapbr` restores BR=0 and rebanks the offset into rb, which `Subi` clears; fall-through swaps 0↔0 twice. Bodies cannot contain unbalanced control (`has_inner_control` guards at :1048, :1433, :1659). MEXCHR/Mexch/all other instructions never touch BR (`exec_instr`, compile.rs:1793-1848; only `Swapbr` at :1830). Entry BR=0 via marshal (:1702). Solid.

**Q3 — Does the real iso idle like the fast sim?** **Structurally yes, conditionally** (finding 3). Fetch has no entries for `pc ≥ rom.len()` → IR stays 0 (step.rs:127-140); all 14 decode predicates except ADDI are false on IR=0; the ADDI predicate fires and adds `s0⊕0` to r0 — identity **iff s0=0**. Emit/mem/absorb: MEXCH/MEXCHR predicates (opcodes 1001/1010) are false on IR=0 → BusMsg stays 0, bus-quiescence holds, §4's claims are correct. `pc_add_br` with BR=0 is identity, `pc_inc` advances PC. So real idle ≡ fast idle *given the s0=0/s1=0 invariants*, which hold for compiled programs — and are already exercised green by every skip-taking `*_real_cpu` test (finding 4). Not undefined, not a panic — the design's core claim survives, with P3 added.

**Q4 — `B(rom) == B(reverse_rom)`?** "By construction" is an overclaim: the reverse program is a *fresh compilation* of `sym(iso)`, and nothing structurally forces equal instruction counts. Empirically it holds today: my test run shows parity=31/sym_parity=31, shl=25/sym_shl=25 instructions. For M-D.d, `|body|` vs `|sym body|` is a genuine risk if any recognizer pattern lacks an equal-length inverse; T4 pinning it empirically is the right call — but the design should say B is computed *from each compiled ROM* and equality is asserted, not assumed.

**Q5 — Tests secretly relying on `budget == rom.len()`?** **None found.** No test passes a step count into `run_on_cpu`; ring tests (ring_run/ring_fib/ring_system/two-body timing) drive their own revolution loops on hand-built ROMs and never call `run_on_cpu`; fib_cpu.rs:163 / feistel.rs:178 assert `rom.len()` values but step their own harnesses; branching.rs steps manually. The 2b-lite timing certificate counts per-revolution cells, not budget steps. Since `B == rom.len()` for all current programs, T1 is genuinely a no-op regression check. Only the shared fuel (finding 6) couples to B.

## (D) What the design missed

1. **The parked-PC arithmetic** (finding 1) — the one real hole; also invalidates T2's sweep bound as written (finding 2).
2. **The s0=0 precondition / fast-vs-real idle divergence** (finding 3).
3. **Park is already load-bearing today** — every Plus input, every short-list fold, every small-counter iter already idles past ROM end inside the current budget (finding 4); the design presents park as new machinery when it is actually a retroactive proof of shipped behavior plus a latent-wrap fix. Framing this correctly also means T5's "hand-built terminal with BR≠0" negative test needs care: no *compiled* program can produce it, so it must be a hand-assembled ROM, which the harness-level check must therefore inspect (e.g. reject ROMs whose Swapbr usage is unpaired, or check the terminal state dynamically — the design should pick one).
4. **No wrap detection exists at runtime** — PC wraps silently mod 256 in both oracles (compile.rs:1886; the iso by construction). The "reject" in P2/T2 can only be a *static host-side* check; the design implies but never states this.
5. `B ≥ rom.len()` invariant and fuel scaling (findings 5–6) — small, but cheap to specify now.

**What's solid (verified, no action):** the injective forward-idle argument (§3) — correct, PC strictly increases through the dead zone and the fixpoint-halting obstruction is real; BR=0 terminal guarantee (Q2) across all four codegen paths; zero-conservation and bus-quiescence during idle (§4); `decode(0)` identity at the ISA level; the 256-word `assemble` cap; marshal entry state (`pc:0, br:0, ir:0`); all cited line numbers within ±8 lines of actual.
