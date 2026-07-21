# BVEQ recognizer design — adversarial design review + POC spike

**Reviewer:** fresh-context adversarial reviewer (did not write the design). Target:
`docs/scratch/bveq-recognizer-design.md` r1, against theseus-v2 @ 87e6e2e.
**Spike:** built and run in a throwaway worktree (`/tmp/bveq-spike`, removed after);
`biodivine-lib-bdd` 0.6.3 + path-deps on theseus-core/tisc/eval; all numbers below are
measured, release build, single-threaded.

## VERDICT: SHIP-WITH-FIXES — the BDD approach is correct and the perf win is real (POC-proven), but the tri-model reconciliation is infeasible as drawn (BLOCKER-1), the Slice-2 "Ccnot at full Flat(8)" headline is unreachable without work the design defers (BLOCKER-2), and I4 is missing its output-side half (MAJOR-1). Fix the slicing and invariants before delegation; the substrate itself is sound.

---

## Findings by severity

### BLOCKER-1 — The tri-model reconciliation (§4/G2) is arithmetically mis-costed and infeasible on the netlist leg as specified

The design claims the per-instruction reconciliation domain is "2–3 operands × 256 =
cheap". The domain is **256^k, not k×256**: 65 536 for 2-op, 16 777 216 for 3-op. The
`exec_instr` leg is fine (measured: 16.7M raw-u8 checks in **9.6 ms**). The netlist leg
is not: one `system_step` evaluation of a single-`Ccnot` ROM through `theseus_eval::eval`
measures **5.56 s** (spike, 4-sample mean, spot-check correct). Full-domain pointwise
netlist reconciliation is therefore:

- 2-op: 65 536 × 5.56 s ≈ **101 hours**
- 3-op: 16.7M × 5.56 s ≈ **26 000 hours ≈ 3 years**

Slice 1's stated deliverable ("symbolic ≡ exec_instr ≡ netlist over the whole
per-instruction Flat(8) domain") **cannot ship as written**. This is exactly the failure
mode of the [[maestro-gate-no-timeout]] lesson (a 2^24 whole-domain test ran 3 h — and
that was the *fast* path).

**Fix (concrete):** split G2 into three legs with different completeness:
1. `symbolic ≡ exec_instr` at **full** per-instruction domain (cheap — ms; do in Slice 1);
2. `exec_instr ≡ netlist` by **stratified sampling** (sample operand values *and* register
   selectors rd/rs/rt — the decoder's register-select mux trees can be wrong for one
   index only; a fixed (0,1,2) assignment tests one path through the muxes), bounded to
   minutes (≈ 20–50 evals/instr); keep L19 real-CPU as merge authority — this is Slice 1;
3. the **full-width netlist proof** = a *symbolic netlist walk*: evaluate the decoder
   netlist (it is a reversible circuit of not/cnot/toffoli gates over bit wires —
   `step.rs`/`decoder.rs`) with BDDs on the wires, operand registers symbolic, everything
   else constant. Gate count × BDD-apply on ≤24-var BDDs is well within budget given the
   spike's numbers. This is real, separate work (a symbolic evaluator for the netlist
   gate subset) and must be **its own slice** (see corrected slicing below) — it is also
   the only path to the design's actual headline (next finding).

### BLOCKER-2 — Slice 2's headline claim ("Ccnot proven equivalent at full 3-operand Flat(8)") is unreachable as sliced; the Slice-2/Slice-3 boundary is wrong (attack D confirmed)

To verify Ccnot at full Flat(8), what plays "function B"? The candidates:

- **The prim's forward table — does not and cannot exist.** Spike measurement: the
  thermometer-`Value` + `BTreeMap` representation costs 11.2 µs/entry at Flat(6)³ and
  scales superlinearly with depth (1.79 → 3.92 → 11.17 µs/entry at Flat(4/5/6)³);
  projected Flat(8)³ ≈ **750 s CPU and O(100 GB) heap** (16.7M × 2 values × ~128 boxed
  nodes each). Nothing 3-op at Flat(8) can ever *reach* `recognize_prim` until Slice 3
  exists. A Slice-2 "Flat(8)³ Ccnot recognizer arm" would be dead code with no caller.
- **The pattern closure re-expressed in `BvExpr`** — this works inside Slice 2, and the
  spike proved it in **11 µs**. But it proves *instruction-model ≡ pattern intent*. The
  thing the milestone says it retires ("its 8-bit 3-operand behavior rests on a
  structural argument + a strided real-CPU pass") is about the **real machine**, and the
  only leg connecting the model to the real machine at full width is the netlist leg —
  which BLOCKER-1 shows needs the symbolic netlist walk, deferred by the design to an
  open question (Q3).

So as drawn: Slice 1+2 deliver "Ccnot's *transcriptions* agree with the intent formula,
full width" (valuable, retires the recognizer/exec_instr divergence class) but **not**
"Ccnot the instruction is machine-verified at full width" (needs the netlist walk), and
**not** "wide prims recognizable at Flat(8)³" (needs Slice 3). The design conflates these
three under one headline. Corrected slicing:

- **Slice 1 (as designed, minus the impossible leg):** `bveq` core; reconciliation =
  symbolic ≡ exec_instr full-domain + netlist stratified-sampled; perf POC. Additive,
  self-contained — confirmed no hidden dependency on recognizer rewiring.
- **Slice 2 (rewire, honest claim):** recognizer arms via `bveq::equiv` at *currently
  reachable widths* (2-op Flat(8), 3-op ≤ Flat(4) tables — now checked against a
  *symbolic* spec instead of a hand-rolled closure loop); enumeration kept as
  cross-check; claim = "recognizer transcription divergence retired".
- **Slice 2b (NEW — the actual wall-break):** symbolic netlist walk; prove
  `netlist-decoder(Ccnot) ≡ instr_semantics(Ccnot)` at full Flat(8)³. This, composed
  with Slice 2, is the first machine-complete full-width instruction proof.
- **Slice 3 (as designed):** symbolic `PrimDef` specs; only then does a Flat(8)³ prim
  become constructible/recognizable and does I5-backward become load-bearing.

### MAJOR-1 — I4 is missing its output half: upper output bits must be proven zero, not just "inputs absent" (POC-demonstrated break)

Spike, section "I4 TRAP": on Flat(4) inputs (upper input bits fixed absent/0), the
byte-wide ADD's low 4 output bits are **BDD-identical** to the mod-16 addInto spec —
while output bit 4 is **not** identically false (it is the real carry out of bit 3). A
`equiv` that compares only the spec's w output bits would therefore **accept byte-Add as
a Flat(4) addInto**, emitting an instruction that drives a register out of the Flat(4)
canonical range — non-canonical state, broken unmarshal/bijectivity, silent corruption.
(This is exactly the failure the valwidth work dodged by making Add Flat(8)-only, where
`admit()` caught the escape as `BackwardNotTotal`. Symbolic specs have no `admit()`; the
checker itself must carry the obligation.) **Fix:** I4 must read: inputs = w free vars,
upper input bits absent; the instruction side computes all 8 output bits; `equiv`
requires (a) low w bits ≡ spec AND (b) **bits w..7 ≡ false**. Ship the byte-Add-at-Flat(4)
case as a must-reject regression test (per L20).

### MAJOR-2 — Frame-completeness: `equiv` must compare the whole transfer, not the pattern's operands

Two concrete escapes if `RegTransfer` only carries "registers the spec mentions":
(1) a candidate sequence with a net side effect on a register outside the pattern's
operand set compares equal on the operands and passes; (2) `Swapbr`/`Xchg`/`Mexch`/
`MexchReg` move state into BR/scratch/mem, which a register-only transfer cannot see.
**Fix:** `symbolic_exec` returns the transfer over the full register file (identity for
untouched regs) and hard-**rejects** (Deferred, reject-don't-corrupt) any instruction
outside the register-pure subset {Add, Sub, Xor, Exch, Rotl, Rotr, Cswap, Ccnot, Addi,
Subi, Xori, Rotli, Rotri} in Slices 1–2. The memexch arm keeps its existing bespoke
semantic verify (its resident-array domain is small); do not try to force it into
`RegTransfer`. The per-instruction netlist reconciliation must likewise compare the
**full post-state** (all regs + BR + scratch + mem + PC/IR discipline), because the
sequence-composition argument (single-instruction equivalence ⇒ sequence equivalence)
is only sound given quiescence of the non-register state — that side condition must be
part of what is checked, not assumed.

### MAJOR-3 — The shared variable-mapping is itself TCB: structural BDD equality can silently compare different denotations

`biodivine` `Bdd` equality is structural over node arrays. If the two sides assign
`(operand, bit) → BddVariable` through *different* mapping code, two **unequal**
functions can produce identical BDDs (e.g. one side interleaved, one side blocked: both
"x0 ^ x1", different meanings) — the concrete answer to attack A's "can an encoder bug
make unequal functions equal?". **Fix:** one pinned mapping object (the design's shared
`BddVariableSet` plus a single `var(operand, bit)` function) used by *both* encoders, and
a unit test that deliberately mis-maps one side and asserts the equality breaks.

### MINOR-1 — Aliasing: `instr_semantics` must be keyed on concrete register indices

`Xor rd==rs` zeroes; `exec_instr` models via indices, and `recognize_prim` can see
aliased homes (test `recognize_prim_aliased_registers`, compile.rs:3166 — but only the
Exch arm checks). `symbolic_exec` over `&[Instr]` must either model aliasing correctly
(natural if the transfer is indexed by real register numbers, per MAJOR-2) or reject
aliased operands explicitly. Add an aliased-sequence test.

### MINOR-2 — The perf narrative should be honest about *where* the enumeration cost lives

Measured: raw-u8 closure enumeration is fast (65 536 in 32 µs; 16.7M in 9.6 ms). The
actual cost of today's path is the **representation** — thermometer `Value` +
`BTreeMap`: ~2.0 s total for the 2-op Flat(8) table+check (~30 µs/entry). So "BDD is
milliseconds where enumeration is minutes" is true against the real code but not against
enumeration per se; the decisive, unfakeable win is **completeness without a table**
(16.7M-domain proof in 11 µs where the table cannot exist at any cost). Framing matters
because a cheap counter-proposal ("just byte-encode the check loop") fixes the check but
can never fix Wall 2.

### MINOR-3 — "Backward untested" (§1) overstates today's gap

`theseus-check/src/elaborate.rs:755` calls `admit()` on elaborated prims, which verifies
`backward ∘ forward = id` and `forward ∘ backward = id` pointwise. The recognizer indeed
never reads `backward`, but table prims arriving via elaboration are already
inverse-checked. I5/G4 becomes genuinely load-bearing only in Slice 3 (symbolic specs
have no table for `admit()` to walk) — schedule it there, not as Slice-1 scope creep.

### MINOR-4 — Z3 differential cross-check: keep, but strictly out of default builds

Worth having (independent decision procedure against our encoder — the one check that
catches a *shared* encoder bug that reconciliation misses if `exec_instr` was transcribed
from the same misreading). But `easy-smt` spawns a `z3` subprocess: keep it behind the
feature flag, CI-optional, never in the nix default dev shell's required closure. Not
gold-plating; cheap insurance — as long as it never touches the runtime dependency graph.

### NIT-1 — "rotates trivial" is wrong for variable rotate

`Rotl rd, rs` (amount from a register) is a 3-stage barrel mux over 11 live vars, not a
wire permutation. Measured: 184 total nodes, 30 µs — harmless, but the design text
should say "constant rotates are permutations; variable rotate is a small barrel mux".

### NIT-2 — Q2 answered: keep `Or` and `Mux`

The ripple-carry (`(a&b) | (c&(a^b))`) and the barrel mux both want `Or`/`Mux`. They cost
nothing (BDD apply has them natively). Dropping them buys no TCB reduction worth the
encoding contortions.

---

## Attack questions — explicit answers

**A. Soundness of the new TCB.** Per-output-bit ROBDD equality in one shared variable
set decides functional equality for total boolean functions — checked, holds, *given*:
(i) both sides use one pinned var mapping (MAJOR-3 — the one real way unequal functions
compare equal); (ii) output arity/frame is compared in full (MAJOR-2); (iii) Flat(w<8)
carries the upper-output-bits-zero obligation (MAJOR-1, POC-demonstrated). Reject-don't-
corrupt: preserved on the unequal path (counterexample, reject) and no-spec path
(fallback/Deferred); the encoder-panic path must be caught explicitly — `equiv` should
return `Result`, never panic across the recognizer boundary; make that a stated
invariant. A bug in a *shared* encoder path degrades soundness only against the real
machine, which is exactly what the (fixed) reconciliation + Z3 differential exist for.

**B. Tri-model reconciliation.** Constructible only after re-costing (BLOCKER-1): the
netlist per-instruction function extraction "single-instruction ROM evaluated over all
input register states" costs 5.56 s/state — 101 h (2-op) / 3 y (3-op). Not cheap; the
design's ×256 arithmetic is wrong. With the fix (full-domain vs exec_instr, sampled vs
netlist, symbolic netlist walk for full width) the divergence class is genuinely retired.
Sequence-composition gap: sound by induction only if quiescence (IR returns 0, BR/scratch
/mem untouched, PC advance uniform) is *verified per instruction*, and registers are
compared over the full file (MAJOR-2); with those, single-instruction reconciliation +
BDD sequence equivalence has no gap for straight-line register-pure code. Sampling must
stratify over register selectors, not only operand values (decoder mux trees are
per-index hardware).

**C. Ordering/blow-up.** Holds, measured: interleaved ADD = 26 nodes/output-bit max
(124 total), 40 µs build+compare; Ccnot = 56 nodes; variable rotate = 184 nodes; 40-var
`(((a+b)^c)+d)^e` = 571 nodes, 122 µs. Ordering sensitivity is real but graceful at
w=8: blocked ordering inflates ADD 124→1028 nodes and a 4-chain add to 8200 nodes/1.7 ms
— wrong ordering costs ~10×, not exponential collapse, at these widths. Pin interleaved
anyway (I3) and keep the adder-size bound test. No multiply in `Instr` (asm.rs:109–149)
and none in `BvExpr` — the Bryant bound is unreachable. Flat(w) mixing stays linear
(fewer live vars).

**D. Slicing.** Slice 1 is genuinely additive and self-contained — checked (no
`recognize_prim` control-flow change needed for any Slice-1 test; the reconciliation
needs only `exec_instr`, `run_rom_on_cpu`, both public). But Slice 2's headline needs
Slice-2b (symbolic netlist walk) for the machine-level claim, and any *recognizer-level*
Flat(8)³ claim is vacuous before Slice 3 (no such prim can be constructed — POC: ~750 s
+ O(100 GB) projected). Deferring symbolic prim specs is right; deferring the netlist
walk into an open question while claiming the full-width proof is not. See BLOCKER-2 for
the corrected slicing.

**E. R6/eqsat.** The standalone claim holds *if* `bveq` exposes pure
`BvExpr`/`&[Instr]`-level functions and the recognizer only calls `equiv` — nothing in
the design entangles them; keep `spec_to_bv` for *patterns* in compile.rs, not in bveq.
The eqsat framing is sound but slightly oversold: BDD-as-validity-oracle and
BDD-hash-as-semantic-e-class-key are correct for fixed-width BV (canonical, decidable) —
but hash-merging requires all terms interned against the same `BddVariableSet`/ordering
(same MAJOR-3 discipline), and joules-as-extraction-cost needs per-instruction energy
attribution that today exists per-ROM, not per-instruction. Right call: build the
substrate, not the e-graph.

**F. Dependency.** `biodivine-lib-bdd` 0.6.3: MIT (verified in the vendored crate),
pure Rust, no `unsafe` (the single grep hit is a doc comment), no transitive C deps;
actively maintained (biodivine/AEON group, Brno). Acceptable in the trust path given:
version-pin it, and the reconciliation + Z3 differential give independent checks on
results. The fallback story (enumeration remains) means a dep failure degrades to
today's behavior, not to corruption. Z3 cross-check: keep, test-only (MINOR-4).

## Design's open questions — explicit answers

- **Q1 (replace vs augment):** Augment. Keep the enumerated arms live as a cross-check
  for at least one release — at reachable widths they cost ≤ ~2 s worst-case (measured)
  and any bveq/enumeration disagreement is a free TCB bug report. Delete only after the
  cross-check has been silent across a full milestone.
- **Q2 (op scope):** `Xor/And/Not/Or/Mux` + `Add/Sub` ripple + rot networks. Keep `Or`
  and `Mux` (NIT-2). No `Mul`, enforced by the enum (I3).
- **Q3 (netlist extraction):** Pointwise single-instruction ROM eval is 5.56 s/input —
  usable only sampled. The symbolic netlist walk is the real answer and should be
  promoted from open question to Slice 2b (BLOCKER-1/2).
- **Q4 (counterexamples):** `sat_witness` suffices — POC decoded the no-carry-add
  witness to `a=1, b=1` (a+b=2 vs a^b=0) mechanically. Map `BddVariable → (operand, bit)`
  through the single shared mapping (MAJOR-3) and print operand bytes, not raw bits.
- **Q5 (biodivine at 24–40 vars):** Yes, with two orders of magnitude of headroom —
  40-var two-carry-chain expression: 571 nodes, 122 µs (sub-second by ~4000×). Ordering
  sensitivity observed (~10× nodes) but benign at w=8.
- **R6:** Holds as designed, with the interning caveat above.

---

## POC RESULTS (all measured, biodivine-lib-bdd 0.6.3, release, single-threaded)

Spike shape: throwaway worktree at HEAD 87e6e2e; scratch crate (`bveq-spike`) with
path-deps on theseus-core/theseus-tisc/theseus-eval; ~430 lines. Direct per-output-bit
BDD builders (interleaved ordering `var = bit·k + operand`): XOR, ripple-carry ADD,
Ccnot `t^=rs&rt`, variable-rotate barrel; counterexample via `xor().sat_witness()`;
"realistic enumeration" = faithful replica of `check_pattern_2reg/_3reg` over
`enumerate_values` thermometer `Value`s + `BTreeMap` tables; netlist leg via the real
`run_rom_on_cpu(&[Ccnot], …, theseus_eval::eval)`. Worktree removed after; main repo
untouched.

**1. Correctness (BDD vs whole-domain enumeration, 2-op Flat(8)):**
- XOR: BDD equal=true in **5–12 µs**; ADD (vs commuted spec): equal=true in **40–98 µs**;
  both agree with the 65 536-point enumeration (raw-u8: 32 µs with `black_box`).
- Deliberately-wrong candidate (carry-less "add"): **UNEQUAL** detected, witness
  `a=1, b=1` (a+b=2 ≠ a^b=0) — verified concretely correct.
- Ccnot vs wrong xor-spec: counterexample found.
- Tri-model spot check: real `system_step` on a Ccnot ROM matches the BDD model and the
  closure on all 4 sampled inputs (incl. 255/170/85).

**2. The wall-break:** Ccnot 3-operand **full Flat(8)** (2^24 = 16.7M-point domain)
proven equivalent by BDD in **11 µs** (56 nodes on the output register). The same proof
by the *real* current representation is impossible: table projected ≈ **750 s CPU +
O(100 GB) heap** (measured scaling 1.79/3.92/11.17 µs-per-entry at Flat(4/5/6)³ — the
per-entry cost itself grows with thermometer depth).

**3. Perf comparison (2-op Flat(8), 65 536 domain):**
| path | time |
|---|---|
| BDD equiv (ADD, build+compare) | **40 µs** |
| raw-u8 closure enumeration | 32 µs (honest: enumeration per se is not the bottleneck) |
| real path: `enumerate_values` + table build + `check_pattern_2reg` walk | **1.98 s** (0.38 + 1.22 + 0.38) |

→ ~**50 000×** vs the code as it exists; ~parity with an idealized byte-level
enumerator — but the byte-level enumerator can never scale to 3-op Flat(8) *tables*
(memory wall), while BDD does the 16.7M domain in 11 µs (~**10⁶×** vs the 9.6 ms raw
loop; **infinitely** better than the unbuildable table). Feasibility statement: the
asymptotic win is real precisely where it matters — completeness without construction.

**4. Netlist leg (the number that breaks the design as drawn):** one single-instruction
`system_step` eval = **5.56 s** → full-domain pointwise: 2-op ≈ **101 h**, 3-op ≈
**3 years**. G2 must be re-sliced (BLOCKER-1).

**5. Q5 stress:** 40-var `(((a+b)^c)+d)^e`: **122 µs, 571 nodes** (max 140/bit).
Variable rotate (11 vars): 30 µs, 184 nodes. Ordering sensitivity: ADD interleaved
124 nodes vs blocked 1028; 4-chain add blocked = 8200 nodes / 1.7 ms — graceful.

**6. I4 trap (new invariant obligation, demonstrated):** byte-ADD on Flat(4)-restricted
inputs: low-4-bits **equal** to the mod-16 spec, output bit 4 **not** identically zero →
a low-w-only comparison mis-recognizes byte-Add as Flat(4) addInto. The upper-output-
bits-zero check is mandatory (MAJOR-1).

---

## Summary of required fixes before delegation

1. Re-slice per BLOCKER-1/2: Slice 1 reconciliation = full-domain vs `exec_instr` +
   stratified-sampled vs netlist; new Slice 2b = symbolic netlist walk (the real
   full-width machine proof); recognizer-level Flat(8)³ claims move to Slice 3.
2. I4 gains the output-side obligation (upper output bits ≡ 0), with the
   byte-Add-at-Flat(4) must-reject regression test (MAJOR-1, L20).
3. `equiv` compares the full register-file transfer + non-register quiescence;
   `symbolic_exec` rejects non-register-pure instructions (MAJOR-2).
4. Single pinned var-mapping shared by both encoders + a mis-mapping negative test
   (MAJOR-3); `equiv` returns `Result`, never panics across the recognizer boundary.
5. Honest perf/claim wording (MINOR-2, MINOR-3, NIT-1); keep enumeration as cross-check
   (Q1); Z3 differential stays feature-gated test-only (MINOR-4).
