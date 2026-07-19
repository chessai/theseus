# f3.5 design review — `Ccnot` ISA instruction + masked-XOR reduce

**Reviewer:** fresh-context Fable (adversarial), did not write the design.
**Reviewed:** `docs/scratch/f35-dotproduct-design.md` @ theseus-v2 e95e3b3, against
`crates/theseus-tisc/src/{asm,decoder,compile}.rs`,
`tests/{xor_reduce,negative_guards,decoder,fib_cpu,ring_two_body_timing,step}.rs`,
`docs/reviews/mdb2-f3-*.md`. All claims recomputed from source.

**Summary verdict: DO-NOT-SHIP as written.** The milestone is sound and worth doing,
but the design has one construction-level hole (the decoder arm is not buildable
"like `cswap_instr()`" at byte-parallel width — §2 hand-waves the only hard part),
one false impossibility claim (§0), one missed register-budget fact that invalidates
the §5 carrier and §3 WCET expectations at d=3, and a file_allowlist that guarantees
the delegated task dead-ends on three non-allowlisted test files the new opcode
deterministically breaks.

---

## Target 1 — Decoder reversibility

### F35-1 (BLOCKER). Byte-parallel `ccnot_instr()` is not expressible by "mirroring `cswap_instr()`"; §2 defers the only genuinely hard decision in the design.

Every gated core operates on **fixed fields** colocated by routing — the target routed
to the r0 slot by one crossbar (`with_target`, decoder.rs:468), and **at most one**
full-byte operand swap-fetched into the single operand buffer s0 (`fetch_operand`
decoder.rs:458; `with_two_regs` decoder.rs:494). `cswap_instr` (decoder.rs:546) gets
away with a *third* register only because it needs **one bit** of it: `r[rd][0]` is
CNOT-copied into s1 bit 4 (`CSWAP_CTRL_WIRE_IN_S1`). Byte-parallel `Ccnot` needs
**8 bits of rs AND 8 bits of rt AND 8 bits of rd simultaneously**. There is no second
byte buffer: s1 has exactly 3 free bits (0=pred, 1–3=chain, 4=cswap copy; 5–7 free),
and sequential crossbars cannot pin two selected registers at fixed slots at once
(the second select re-permutes the first). `cross_toffoli` (decoder.rs:922), which §2
names to reuse, is a **cross-factor (Cpu,Bus) helper** for the MEXCHR emit block — the
wrong tool; and `op_gates` (§2 item 3) **does not exist** — the dispatch function is
`instruction()` (decoder.rs:784).

Three distinct options (a design decision, not something the probe "finds"):
- **(a) Redefine semantics to bit 0** — `r[rd].0 ^= r[rs].0 ∧ r[rt].0` — matching
  Cswap's existing bit-0-control convention. Then the construction is *pure existing
  machinery*: two cswap-style `with_target` copy brackets (rs[0]→s1.5, rt[0]→s1.6;
  bits 5–6 free, collide with nothing), one 4-wire gated `cccnot_prim` on [r0-slot,s1]
  (the same Tk::T shape gated ADD already emits), predicate bracket unchanged.
  **Recommended.**
- **(b) Keep byte-parallel** — requires a NEW decoder convention (self-cancelling
  swap-fetch of r[rt] into a second buffer field). Sound in principle but a new
  convention needing its own writeup, aliasing analysis, reviewer pass — not a
  mechanical mirror.
- **(c) Bit-serial byte-parallel** (8 × copy/cccnot/uncopy) — expressible today but
  ~8 crossbar-bracket-pairs ≈ thousands of extra K1 cells; step energy pays forever.

§1's "byte-parallel … costs nothing extra" is exactly backwards: byte-parallel is the
*only* thing that breaks the cswap template. Pick a route and rewrite §1/§2/§4.

### F35-2 (verified OK). Opcode `0b1011` is genuinely unclaimed; predicate structure survives.
`asm.rs::decode` R-format covers `0b0000..=0b1010`, `_ => None` (256–268). `r_pred`
instantiated for 0000–1000 + 1001/1010 emit blocks (decoder.rs:1060/1104); nothing
claims 1011. Predicates are full-literal ANDs incl. bit 15 → 1011 exclusive with every
existing opcode. `pred_compute` accepts k=5 (decoder.rs:344); 0b1011 literals routine.
Under option (a), s1 usage (pred 0, chain 1–3, cswap 4, ccnot 5–6) collision-free;
each block restores s1 to 0. `validate()`/closure re-check is real but structural —
T7 covers it.

### F35-3 (MAJOR). `Op::ALL` is `[Op; 14]`; an exhaustive match in a non-allowlisted test file breaks the build.
`Op::ALL: [Op;14]` (decoder.rs:759) + exhaustive `match op` in `instruction()`
(decoder.rs:784) must change — both allowlisted. But `tests/decoder.rs::op_samples()`
(tests/decoder.rs:113) is an **exhaustive `match op` with no catch-all**: adding a
variant is a compile error for the whole `decoder` test target, and `reference()`
(tests/decoder.rs:46) needs an 0b1011 arm or `per_instruction_lowering_and_semantics`
tests Ccnot against identity and fails. `tests/decoder.rs` not in the §8 allowlist.
(Upside: once extended, that harness gives Ccnot dense-byte coverage for free — F35-10.)

### F35-4 (MAJOR). The two-body ring timing golden stales deterministically — same class as f1, same non-allowlisted file.
`cpu_pre`/`cpu_post` contain `decode_execute()` over `Op::ALL` (step.rs:293–298).
Adding an op grows both netlists → `tests/ring_two_body_timing.rs::golden_pin_mexch_rom`
(pins d_pre=2976, d_locked/period=3304) **will fail**, needs re-pin from probe values.
Not in the allowlist. `fib_cpu` survives (pins 0x5000/I-format + pad-bit words).

## Target 2 — Leaf-order contract

### F35-5 (verified OK, one pin). Recognizer contract mechanically sound.
`with_registers` (compile.rs:448) maps `regs[i]` to home of leaf i positionally;
homes injective, spares disjoint → `regs[2]` genuinely the third leaf's location, three
regs pairwise distinct. `check_pattern_3reg` (compile.rs:1094) verifies the entire
table; no table satisfies both cswap and ccnot (c=1,a=0,b=1: cswap→(1,1,0),
ccnot→(1,0,1)) → arm order immaterial. Pin: `check_pattern_3reg` needs input type
**exactly** `Prod(B,Prod(B,B))` — a trailing `One` makes the inner destructure misread
→ falls through to `UnrecognizedPrim` (fail-loud, but tell the worker the shape).

### F35-6 (MAJOR). Deferred plumbing hides two facts derivable NOW — one invalidates §5/§3 at d=3.
1. **Register budget: f3.5 d=3 is in the displaced regime, not f3's clean regime.**
   `numeral_ty(3)` has **3** leaves. Payload now 4 (acc,key,data,idx). Head=3+4=**7** →
   Reg0..6, one free reg — but `lower_data_dependent_iter` needs **2** aux
   (compile.rs:1921), so `allocate_aux` displaces the last non-counter leaf — **idx →
   Mem(2)** — exactly f3's *d=4* regime. Every idx use pages a Mexch pair (+6 instrs/
   body), the spine gains the displacement pair, paging spare is aux rb (safe by f3
   d=4, but state it). "Expected body length 6 or 7" and the implicit clean-d=3
   histogram are wrong. Alternatives to weigh: keep key **resident** at Mem(2) (head 6,
   +2 paging around Ccnot only), accept displaced idx, or d=2 (too weak).
2. **Target-last costs Exchs the design didn't count.** Under carrier `(acc,key,data,…)`
   lining up `(key,data,acc)` is a 3-cycle = 2 Exch in + 2 out = **+4**. Carrier
   `(key,acc,data,…)` needs only f3's single swap pair = +2. A **target-FIRST** arm
   `(t,c1,c2)↦(t⊕c1∧c2,c1,c2)` emit `Ccnot{rd:regs[0],rs:regs[1],rt:regs[2]}` under
   carrier `(acc,key,data,…)` needs **zero** Exchs: body = MexchReg, Ccnot, MexchReg,
   Xori (+paging). The recognizer freedom is the design's to spend; "defer to probe"
   shouldn't obscure that §3's target-last choice is what creates the plumbing cost.

Also the F3-1 analog: with any Exch dance, `regs[2]` at the prim point is the
**data-home register temporarily holding acc** — T1 (bare prim, rd=acc home) correct
as scoped, but the integration exact-shape test must assert the *swapped* register,
like f3's T5 (tests/xor_reduce.rs:534).

## Target 3 — WCET / park-tax

### F35-7 (MINOR). Deferring numbers to the probe is right; the shape is derivable.
T(k)=(d−1)·6 + L_b·k, B=(d−1)·(6+L_b), affine/monotone, T(k_max)=B — tightness
structural, no unguarded productive instr on the spine except `allocate_aux` paging
(present: idx displacement pair). L_b ~10–14. Two corrections: park tax hits 0 at
**k=d−1** (Nat_d numerals are 0..d−1; f3 d=3 hit 0% at k=2), not "k=d"; B/T in T6a must
be re-derived after F35-1/F35-6, not before.

## Target 4 — Operand distinctness / ambiguity

### F35-8 (MINOR). Correct conclusions, wrong reasons; T-neg mis-stated.
`rs==rt` cannot arise from a 3-leaf prim (with_registers always pairwise-distinct) —
the arm's alias check is dead defensive code (fine; note cswap arm at compile.rs:954
has none). The genuine "reduces to Xor" case is a **2-leaf** prim `(c,t)↦(c,t⊕c)`
already caught by the Xor arm; rewrite T-neg. `rd==rs`/`rd==rt` Toffolis are
non-injective → never PrimDefs. No other 3-leaf pattern mis-recognized (whole-table
exact).

## Target 5 — Scope / gate rot

### F35-9 (BLOCKER for §8). Three test files the change deterministically breaks are missing from the allowlist.
**(1)** `tests/negative_guards.rs::decode_reserved_r_format_opcode_1011` asserts
`decode(0xD800)==None` (31–36) → must become the Ccnot decode assertion. **(2)**
`tests/decoder.rs` — compile error + reference model (F35-3). **(3)**
`tests/ring_two_body_timing.rs` — golden re-pin (F35-4). `check_commands: cargo test -p
theseus-tisc` (full) is right and catches all three — but the worker can't edit them
→ task fails the gate unconditionally. Add all three. Drop `src/instr.rs` (Op table
lives in decoder.rs; nothing in instr.rs enumerates opcodes). `tests/step.rs` OK
(opcode-matched catch-all; no 0xD8xx ROM).

## Target 6 — Stated-assumption checks

### F35-10 (MAJOR). §0's impossibility claim is false, and the Bit-domain test plan can't pin the chosen semantics.
- **"Xor/Add/Exch/Rotl affine over GF(2)^8" is wrong**: ADD's carry is quadratic — the
  decoder's TK adder is *built from Toffolis*. For Bit leaves, the 7-instruction,
  zero-ancilla, existing-ISA sequence `Rotli acc,1; Xor acc,key; Xor acc,data; Add
  data,key; Xor acc,data; Sub data,key; Rotri acc,1` computes `acc ^= key∧data` exactly
  on 0/1 bytes (the carry bit of key+data *is* the AND). A **recognizer-arm-only route
  — no ISA change, no decoder change, no closure re-validation, no golden re-pins** —
  exists; domain-restricted (same contract memexch's arm uses), 7 instrs/element vs 1.
  A new instruction remains a defensible **choice** (energy/element; byte-wide masked
  accumulate ahead of M-D.c; strategic "first user-facing CPU-authority instruction") —
  but §0 must be a trade-off, not a necessity.
- **Self-inverse**: true byte-parallel for pairwise-distinct operands; false under
  aliasing (rd==rs, t=1: v→0→0). Scope the claim to legal operands.
- **Semantics-pinning gap**: T1–T5 live on Bit leaves (bytes 0/1) — where byte-parallel
  and bit-0 semantics **coincide**. If decoder and `exec_instr` disagree on bits 1–7,
  nothing catches it until a future Word8 program corrupts silently. Require a
  dense-byte (0xAA/0x55) StateVals test against `reference()` — the extended
  tests/decoder.rs harness provides exactly this.

### F35-11 (NIT). Doc rot to sweep: decoder.rs:30 "Reserved opcodes R 1011–1111";
Op doc decoder.rs:735/757 "16 live"/"14 CpuState-only"; asm.rs header "14 architected"
(already stale at 16); §2 `op_gates`→`instruction()`; §Energy "k=d"→"k=d−1". Note in
status.md that `step_j` rises for **every** program once the decoder grows (published
fib/map/reduce joules drift).

---

## SHIP / DO-NOT-SHIP-UNTIL

**DO-NOT-SHIP-UNTIL (design revision, before probe/delegation):**
1. **F35-1**: §2 rewritten around an explicit choice — bit-0 semantics (recommended:
   pure existing machinery, matches Cswap) or byte-parallel with a specified
   second-buffer convention. Fix `op_gates`→`instruction()`, drop `cross_toffoli`.
   §1/§4/`exec_instr` consistent.
2. **F35-9**: allowlist += `tests/negative_guards.rs`, `tests/decoder.rs`,
   `tests/ring_two_body_timing.rs`; drop `src/instr.rs`. Tell the worker the three
   expected pre-existing-test edits (1011 decode flip, op_samples/reference arms,
   golden re-pin from probe values).
3. **F35-6**: §5 reworked for the 7-head-leaf displacement at d=3 (accept displaced
   idx, or key-resident, trade-off stated); carrier order / target-first-vs-last
   decision made explicitly; §6 T6a re-derived (numbers still pinned by probe).
4. **F35-10**: §0 rewritten as a justified trade-off vs the 7-instruction carry-trick
   route (or the milestone re-scoped to that route); add the dense-byte semantics test.

**SHIP-OK as-is (verified):** opcode 0b1011 free everywhere (F35-2); recognizer
contract + no cswap collision + distinctness (F35-5, F35-8 reword aside); probe-first
sequencing (§7); full-suite check_commands (§8); merge-f3.5-first concurrency ordering
(§9 — Thread B's `lower_trace`/`MAX_LEAVES_LOOP` does not touch the `iter`
guarded-unroll path f3.5 uses, confirmed disjoint).
