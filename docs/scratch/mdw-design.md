# M-D.w — Halting model / static WCET budget + reversible park-at-end

**Status:** design of record — **reviewed** (fresh-context Fable adversarial review,
verdict SHIP-WITH-FIXES; all findings folded in: BLOCKER-1 parked-PC `B+S` precondition
→ §3.1 P2, MAJOR-3 `s0=0` idle condition → §3.1 P3, MAJOR-4 park-already-live reframe →
§3, MINORs `B≥rom.len()`/fuel/static-guard → §2/§1/§3.1, T2/T5 corrected → §6). The §8
questions are answered in that review. Discharges ADR-0007 ledger entry **TH-1**
("trace over finite `u` … is a total iso by construction, with a static WCET the
compiler reports; exact bound + two-direction proof still owed").

**Scope boundary (read first).** M-D.w introduces **no backward jumps** and no new
instruction. It builds and *proves correct on today's monotone-PC programs* the two
pieces of infrastructure that M-D.d (Trace/labels reversible while-loop) will need:

1. a **static WCET budget** `B(rom)` that the compiler computes and reports, and
2. a **reversible park-at-end convention** so that running the CPU for `B` steps
   yields the program's answer for *every* input, even when the input's actual
   trace is shorter than `B`.

M-D.w changes `run_on_cpu` to run for `B(rom)` steps instead of `rom.len()` steps,
and adds the ledger + the park-liveness proof. For every program that exists today
`B(rom) == rom.len()` and behavior is **bit-identical** — the value of M-D.w is the
*machinery and its proof*, handed to M-D.d with a documented precondition.

---

## 1. The problem

`run_on_cpu` (compile.rs:1766, `budget = instrs.len()`) runs `system_step` exactly
`rom.len()` times. This is correct **only for monotone PC**: with `PC += BR + 1` and
`BR ≥ 0` chosen so PC advances, `rom.len()` steps fetch `rom[0..rom.len())` exactly
once and the loop stops with `PC = rom.len()`. No idle step is ever taken inside the
budget.

Two M-D.d constructs break this:

- **Trace-as-while-loop** — a body at addresses `[a, a+L)` with a *backward* branch
  `BR = −(L+1)` re-enters address `a`. The trace length is `k·L` for `k` bounded
  iterations, which for `k·L > rom.len()` **exceeds** the fixed budget: the machine
  is stopped mid-loop and `run_on_cpu` returns a wrong (partial) answer.
- **CALL/RET** — a return jump is a backward jump; same failure.

So the oracle needs (a) a budget that is a genuine **upper bound on trace length for
all inputs**, and (b) a guarantee that once the program's *own* trace ends, the extra
steps up to the budget do not corrupt the answer.

## 2. Static WCET budget `B(rom)`

`B` is a **static, input-independent** upper bound on the number of `system_step`
applications any input can require. It is defined structurally over the compiled
program (not measured per-input):

```
B(rom) = |straight-line region|  +  Σ_{loops ℓ}  k_ℓ · |body_ℓ|
```

- **Straight-line region:** the count of instructions on the monotone spine — today
  this is all of `rom`, so `B = rom.len()`.
- **Loop term (M-D.d hook):** for each finitized `Trace`/`iter`-loop `ℓ`, `k_ℓ` is the
  *finitization bound* on its iteration count (ADR-0035; statically known — this is
  exactly what makes theseus's traces total, ADR-0007 §"Finite fragment") and
  `|body_ℓ|` its body length. M-D.w defines the accumulator and the API; M-D.d
  populates the loop term when it emits the first backward branch.

`run_on_cpu` scales its eval fuel with `B` (currently a flat `8e9`, compile.rs:1774)
so a larger budget cannot spuriously exhaust the shared per-run fuel counter (review
MINOR 6).

`B` is returned alongside the ROM. Concretely, `compile` gains a companion that
yields `CompiledProgram { instrs: Vec<Instr>, wcet: usize }` (or `compile` stays and a
`fn wcet(iso) -> usize` is added — implementer's call, see §6), and the ADR-0007
ledger entry TH-1 is reported from it. **Invariant `B ≥ rom.len()` is asserted** — a
budget below the straight-line length would silently truncate the trace (§1 failure
mode). M-D.d's loop accumulator must never undercount below `rom.len()`.

**Two-direction obligation (TH-1).** ADR-0007 requires the bound hold in *both*
directions (forward totality ⇏ backward totality). `B` is direction-symmetric by
construction: the reverse program `sym(iso)` traverses the same finitized spine and
the same loops with the same `k_ℓ·|body_ℓ|`, so `B(rom) == B(reverse_rom)`. The design
asserts this equality; the test pins it (§6, T4).

## 3. Reversible park-at-end convention

**This convention is already live and already proven green — M-D.w does not introduce
it.** The current `budget = rom.len()` (compile.rs:1766) *already over-runs* for any
input that takes a forward skip: `swapMaybe` idles 1 step past ROM end, `fib`'s
guarded unroll idles up to `S` steps, and every `Plus` input skips one of its two
arms. All 17 `*_real_cpu` oracle tests in `tests/compile.rs` are green, so
forward-idle-as-identity **on the real `system_step` iso** is already empirically
established on today's programs (Fable review finding 4). What M-D.w adds is (a) the
*proof and bound* of this shipped behavior, and (b) a fix for its **latent wrap bug**
(§3.1) that today's tiny programs merely dodge.

When a given input's trace ends before step `B`, the CPU must **idle** for the
remaining steps without changing the answer. The idle must itself be **reversible** —
`run_on_cpu` drives the real `system_step` iso, and `system_step` is a bijection.

**The idle is forward motion through identity-decoding cells, never a fixpoint.**
A PC-fixpoint self-loop (`step(s) = s`) is *not* injective — the step that *enters*
the halt state `s'↦s` plus the fixpoint `s↦s` gives `s` two preimages, breaking
reversibility (this is the reversible-computing halting obstruction). Instead the
machine keeps moving:

- The idle cell decodes to `Addi { rd: 0, imm: 0 }` (asm.rs:270-278, `decode(0)`;
  `rom[pc]` for `pc ≥ rom.len()` leaves `IR = 0` because `fetch()` builds entries only
  for `p < rom.len()`, step.rs:127-140). On the **fast sim** this is unconditional
  identity (`wrapping_add(0)`, compile.rs:1833). On the **real `system_step` iso it is
  identity *only if `s0 = 0`*:** the ADDI predicate `i_pred(0b000)` fires on `IR = 0`
  (decoder.rs:688-690) and `addi_core` adds **`s0 ⊕ imm`** into r0 (decoder.rs:43-44,
  662-686); with `imm = 0` that is `+= s0`. So idle freezes r0 **iff `s0 = 0`** — see
  P3 below. All other 13 decode predicates, and the MEXCH/MEXCHR emit/mem/absorb
  predicates (opcodes 1001/1010), are false on `IR = 0`, so BusMsg stays 0 (§4).
- With `BR = 0` at the terminal, each idle step does `PC += 0 + 1 = PC + 1`: the
  machine walks forward through the dead zone `[rom.len(), 256)`, register/scratch/mem
  state frozen, PC strictly increasing. This is injective (PC is part of the carrier
  and never repeats within the dead zone), so it is a genuine reversible idle.

The *observable* output — `unmarshal` of registers/scratch/mem, with PC and BR
projected away — is therefore **stable** from the true halt step through step `B`,
**provided the terminal preconditions P1–P3 hold**.

### 3.1 Park-safety precondition (the sharp edge — corrected per review)

PC is **`W = 8` bits and wraps mod 256** (types.rs:25); `assemble` caps the ROM at
256 words (asm.rs:294-296). The dead zone is `[rom.len(), 256)`. Forward-idle is safe
**only while PC stays in the dead zone**: if PC reaches 256 it wraps to `0` and
re-enters live ROM, re-executing instructions and corrupting the answer — **and the
wrap is silent** (`PC = PC.wrapping_add(BR).wrapping_add(1)`, compile.rs:1886; the real
iso wraps by construction). There is no runtime trap, so the guard **must be a static,
host-side check computed before the run.**

**The parked PC is `B + S`, not `B` (Fable review BLOCKER 1).** Over a trace of `T`
real steps reaching `PC = rom.len()`, `Σ(BR+1) = rom.len()`, so `T = rom.len() − S`
where `S` = total forward-skip distance that input takes. Idling to budget `B` adds
`B − T` idle steps (each `PC += 1`), so the **parked PC = `rom.len() + (B − T) =
B + S`**. A program with `B ≤ 256` but nonzero skips still wraps: the review's witness
— a 135-word ROM with a 130-instr `Plus` arm, input skipping that arm — has `T = 5`,
`S = 130`, parked `PC = B + 130 = 265` → wraps by 9 → **re-executes `rom[0..9]` during
"idle."** This is a **latent bug in today's harness** (budget `= rom.len() = 135`),
masked only because current programs are tiny (largest compiled program = 34 instrs).
M-D.w fixes it.

Three conditions make the park sound. M-D.w **checks and enforces** them
(reject-don't-corrupt if violated):

- **(P1) `BR = 0` at every terminal configuration.** Then idle advances PC by exactly
  1/step. `Swapbr` is emitted *only* by `emit_skip_block`, which restores BR to 0 (the
  `Swapbr{rb}` pair, compile.rs:644,650); no other instruction touches BR
  (`exec_instr`, only `Instr::Swapbr` at compile.rs:1830); the marshalled entry state
  has `br: 0` (compile.rs:1702). Verified across all four skip-emitting sites (review
  Q2). The ledger asserts it.
- **(P2, corrected) `B + S_max ≤ 256`**, where `S_max` is a **static upper bound on
  total forward-skip distance**. A sound, cheap over-approximation: `S_max = Σ` over
  every `emit_skip_block` in the ROM of its `body_len` (every skip taken — conservative
  because mutually-exclusive `Plus` arms can't both be skipped on one path). A program
  violating `B + S_max ≤ 256` **cannot park by idle and is rejected**
  (`CompileError::Deferred("parked PC B+S_max=… exceeds PC address space 256; …")`).
  This is a real constraint M-D.d's loops must respect (bounded `k·|body| + spine +
  S_max ≤ 256`, or spill the counter to shrink the footprint). Loud, never silent.
- **(P3) `s0 = 0` (and `s1 = 0`) at every terminal configuration (review MAJOR 3).**
  The real-iso idle cell adds `s0` into r0 (§3); idle is identity on the authoritative
  oracle only if `s0 = 0`. This is an existing invariant for compiled programs (marshal
  zeroes s0/s1 and spills only to s2..s7, compile.rs:1710-1712; XCHG rejects s0/s1,
  asm.rs:212-214; the swap-fetch restores s0), so nothing breaks today — but the park
  *depends* on it, so the ledger asserts it and the terminal-state check (§6 T5)
  enforces it dynamically. `s1` is the fetch-swap partner and is likewise 0 at rest.

> Note the honest limit this documents: with an 8-bit PC, the total *unrolled-
> equivalent* trace an oracle can cover is `B + S_max ≤ 256` steps. That is the same
> finiteness budget the whole machine already lives under (256-word ROM); M-D.w makes
> it an explicit, enforced totality bound. If M-D.d needs longer traces, that is a
> *widen-PC* decision (its own ADR), not something M-D.w should paper over.

### 3.2 Considered and rejected: halt-on-fallthrough early stop

An alternative is to stop `run_on_cpu` the first time `PC ≥ rom.len()` (the true halt
for both monotone and — after M-D.d — loop programs, which fall through past the loop),
avoiding over-run entirely. Rejected because the **`SystemRing` cannot early-stop** —
it runs a fixed `R` revolutions, so it *must* park safely regardless. Keeping
`run_on_cpu` faithful to "run to `B` then park" is what proves the property the ring
depends on; an early-stopping oracle would leave the ring's park unverified. (A
belt-and-suspenders dynamic terminal check is still added — §6 T5.)

## 4. Reversibility obligations preserved

- **Bijection / totality (ADR-0007):** `B` is the TH-1 static bound; `B(rom) ==
  B(reverse_rom)` gives the two-direction property. Park-idle is injective (§3).
- **Zero-conservation / zero-parking (ADR-0044):** the idle cell is `Addi r0,0`,
  which manufactures no zero and touches no scratch/mem — park adds no ancilla and
  restores none (it is literally identity on everything but PC).
- **Bus-quiescence (Thm 3.5):** idle steps issue no MEXCH/bus traffic (`Addi` is a
  CPU-only op); `BusMsg` stays 0 across every parked revolution, so the composite
  closure is untouched.

## 5. What M-D.w explicitly does NOT do

- No backward branch, no new instruction, no `Trace` codegen — that is M-D.d.
- No PC widening, no MEM_WORDS change.
- No change to `run_on_cpu_fast`'s *semantics* beyond adopting the same `B` budget so
  the fast and real oracles agree on step count (optional but recommended for parity).

## 6. Acceptance (every item ships with its test — L20)

- **T1 — zero regression (authoritative).** All existing `*_real_cpu` oracle tests in
  `tests/compile.rs` stay green with `run_on_cpu` now driven by `B(rom)`; since
  `B == rom.len()` for every current program, outputs are bit-identical. This is the
  real-CPU norm (L19), not the fast sim. (Eval fuel is scaled with `B` — §1 — so a
  larger budget can't spuriously exhaust the shared fuel counter.)
- **T2 — park-liveness (new, the core proof).** Two sub-cases, because the safe
  overshoot is per-input `256 − rom.len() − S(input)`, **not** `256 − rom.len()`
  (BLOCKER 1/2):
  - **T2a straight-line** (`S = 0`): a straight-line M-A iso with no skip blocks. Run
    the real CPU for `rom.len() + k` steps for every `k` in `0..=(256 − rom.len())` and
    assert the unmarshalled output equals `k = 0`. This sweeps the *entire* dead zone
    (parked `PC = rom.len() + k`, no `S` term) and is the clean forward-idle proof.
  - **T2b skip program** (`S > 0`, e.g. a `Plus`): run for `rom.len() + k`,
    `k in 0..=(256 − rom.len() − S(input))`, assert stable; and assert that at
    `parked PC = B + S`, running one step *further into a wrap* changes the output —
    documenting that the bound is tight, exactly as the cert-weakening tests do.
- **T3 — WCET ledger (new).** `wcet(iso)` reports `rom.len()` for monotone programs;
  a unit test pins the exact number for a fixed program (golden value, so a codegen
  size regression fails loudly — the 2b-lite F-teeth pattern). Also assert the
  `B ≥ rom.len()` invariant.
- **T4 — two-direction bound (new).** `wcet(iso) == wcet(sym(iso))` for a sample of
  isos (TH-1's owed two-direction proof). Note `B` is computed *from each compiled
  ROM* (a fresh compilation of `sym(iso)`), so equality is asserted, not assumed
  (review Q4).
- **T5 — precondition rejection (new, negative / L20).** No *compiled* program can
  violate P1/P3 (review Q2/MAJOR 3), so these are **hand-assembled ROMs**:
  - a ROM whose `B + S_max > 256` (the review's 135+130 witness) → `run_on_cpu`
    **rejects** it with the documented static `Deferred` error (P2), not a wrapped
    answer;
  - a hand-assembled ROM that halts with `BR ≠ 0`, and one that halts with `s0 ≠ 0`
    → the **dynamic terminal-state check** (`run_on_cpu` asserts `BR = 0 ∧ s0 = 0 ∧
    s1 = 0` at step `B` before trusting the output) fires and returns an error, not a
    corrupt value. This belt-and-suspenders check (§3.2) catches exactly the
    hand-built ROMs the static compiler-path proof cannot produce.

**Implementer's-choice, call it in the spec:** whether `B` rides on a new
`CompiledProgram` struct returned by `compile`, or a standalone `pub fn wcet(iso:
&Iso) -> Result<usize, CompileError>`. Prefer the standalone `wcet` to avoid churning
every `compile` call site; `run_on_cpu` calls it internally.

## 7. Why this is safe to delegate as one increment

It is a bounded refactor of one function (`run_on_cpu`) + one new pure function
(`wcet`) + five tests, with the hardest property (park-liveness under a wrapping PC)
reduced to a concrete, enumerable dead-zone sweep on programs that already exist. It
touches control-flow semantics, so it takes the full discipline: **this design gets a
fresh-context adversarial review before delegation, and the implementation gets one
before merge** (CLAUDE.md non-negotiable), with T1 (real-CPU oracle) as the gate.

## 8. Open questions for the design reviewer (break this)

1. **Is `B ≤ 256` the right precondition, or too weak/strong?** Could a program have
   `B ≤ 256` yet still wrap because `first_halt_pc` is already near 256 and the idle
   span pushes it over? (I claim `first_halt_pc ≤ rom.len() ≤ B ≤ 256` bounds it, but
   check the arithmetic: is `first_halt_pc + idle_span = B` exactly, or can PC
   overshoot `B` because the last real step also does `PC += BR + 1`?)
2. **BR=0 at terminal — is it actually guaranteed** for every current codegen path
   (bare sums, `iter`, fold/unfold unroll, MEXCHR), or only for skip-blocks? Find a
   path that leaves `BR ≠ 0` at `PC = rom.len()`, or prove none does.
3. **Does `system_step` idling actually match the fast-sim idling?** The fast sim
   fetches `0` for `pc ≥ rom.len()`; does the real `system_step` iso, built from
   `rom`, decode an out-of-range PC to the same identity, or does `pc_pred`/fetch do
   something else past the ROM end? (step.rs fetch is keyed on `rom.len()` addresses;
   confirm the iso's behavior for `pc ≥ rom.len()` is identity, not undefined.)
4. **Two-direction bound:** is `B(rom) == B(reverse_rom)` actually true once loops
   exist, given the reverse loop's body may differ in length after `sym`? (For M-D.w's
   monotone programs it is trivially true; flag whether the *design* of the loop term
   in §2 preserves it for M-D.d.)
5. **Does changing the budget perturb any test that secretly relied on
   `budget == rom.len()`** as a side effect (e.g. a test that counts steps)?
