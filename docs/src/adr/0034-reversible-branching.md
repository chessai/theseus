# ADR-0034: Reversible branching — the offset-BR / come-from discipline

**Status:** Accepted (design; implementation is M4-v2, with ADR-0033)

## Context — the obstruction, restated

`step.rs` documents (and its counterexample proves) that TISC-1 v1's
SWAPBR spec semantics — *swap PC↔BR during execute, then PC += 1* —
are **not injective on the full State space**: pre-states
A = (PC=5, BR=9, X) via `rom[5] = SWAPBR` and B = (PC=9, BR=5, Y) via
fall-through collide for a suitable Y. Worse, any predicate that would
re-key the swap after IR is zeroed must read {PC, BR, ROM}, and after
*any* branch BR holds a come-from address that *is* a SWAPBR address,
so the keying convention is violated by the very next instruction. v1
therefore degenerates SWAPBR to PC-advance-only to keep `step` a total
bijection; branches don't work at the step level.

The diagnosis this ADR ratifies: **the obstruction is an artifact of
deviating from the Pendulum lineage, not a fact about reversible
branching.** v1 let an *instruction* write PC mid-step and then applied
an unconditional +1. In the actual Pendulum/PISA design, **no
instruction writes PC**; branch instructions manipulate a *branch
register*, and the machine's control unit updates PC *from* BR at the
end of every cycle. Under that structure the step is injective on the
whole state space, unconditionally — the paired-branch/come-from rule
is a *program*-level discipline that makes control flow mean what the
programmer intended, not the thing that rescues bijectivity.

## The literature, accurately

- **J. Storrs Hall,** "A Reversible Instruction Set Architecture and
  Algorithms," *PhysComp '94* — early reversible ISA with PC-relative
  paired jumps (every jump target syntactically acknowledges its
  source).
- **C. J. Vieri,** *Pendulum: A Reversible Computer Architecture*
  (M.S., MIT, 1995) and *Reversible Computer Engineering and
  Architecture* (Ph.D., MIT, 1999) — defines PISA. Control flow runs
  through a dedicated **branch register**: branch instructions modify
  it, the cycle-end PC update consumes it, and every branch target must
  contain the **paired branch** that accounts for the incoming jump so
  sequential execution resumes (the come-from).
- **M. P. Frank,** *Reversibility for Efficient Computing* (Ph.D., MIT,
  1999) — the revised PISA used by the Pendulum group; **SWAPBR is
  Frank's/Vieri's instruction and it swaps BR with a *general
  register*** (not with PC) — the primitive for computed jumps and
  call/return linkage. TISC-1 v1's "SWAPBR = swap PC↔BR" was a
  misreading of the lineage; this ADR restores the original meaning.
- **H. B. Axelsen, R. Glück, T. Yokoyama,** "Reversible Machine Code
  and Its Abstract Processor Architecture," *CSR 2007* (LNCS 4649) —
  formalizes the PISA-style machine (pc, br, and a direction bit dir as
  explicit state; PC advances by BR each cycle, negated under reverse
  direction) and proves the machine code is reversible. Their theorem
  is exactly the fiberwise-bijection argument sketched below.
- **M. K. Thomsen, H. B. Axelsen, R. Glück,** "A Reversible Processor
  Architecture and Its Reversible Logic Design," *RC 2011* (LNCS 7165)
  — BobISA; local pairing of jumps and targets carried into the gate-
  level design.
- **Janus** (C. Lutz & H. Derby, 1982/1986; T. Yokoyama & R. Glück,
  "A Reversible Programming Language and its Invertible
  Self-Interpreter," *PEPM 2007*) — the language-level analogue: every
  join point carries an entry assertion (`fi` on conditionals, `from`
  on loops) — come-from as syntax. theseus's clause coverage is the
  same idea (P3: "paired-branch assembly mirrors clause coverage").

## Decision

- **D1 — No instruction writes PC.** The execute phase's write sets are
  restricted to `{r0..r7, s0..s7, BR}` (syntactically checkable in the
  decoder's `on_fields` field lists). **SWAPBR is re-specified to its
  Pendulum meaning: `SWAPBR rd` swaps BR ↔ rd** (8 rail-pair exchanges
  gated on the opcode predicate, same shape as EXCH). It is the *only*
  control-flow instruction; conditional branching is CSWAP-then-SWAPBR,
  as the spec already anticipated.

- **D2 — BR is a PC *offset*, consumed by a uniform end-of-step PC
  update:** `PC += BR + 1 (mod 256)`, implemented as
  `pc_add_br ; pc_inc` — an 8-bit in-place ripple add of BR into PC
  (Takahashi–Kunihiro, ≤ K2, carries in s1, same datapath family as
  ADD) followed by the existing increment. BR = 0 means fall-through,
  so **every M4-v1 straight-line ROM runs bit-identically** (backward
  compatible). The step becomes:

  ```text
  step_v2(rom) = fetch ; decode_execute' ; fetch ; pc_add_br ; pc_inc
                 └────────── E ─────────┘ └──────── A ────────┘
  ```

  No direction bit is added to State: reverse execution is the adjoint
  `Iso::sym(step_v2)` (direction is modal, ADR-0026 C2.5); Axelsen et
  al.'s `dir` register is the *interpreter's* device for making one
  machine self-reversing, which theseus gets from the iso algebra.

- **D3 — P4 (IR provably zero) becomes trivial.** PC is *constant*
  across fetch → execute → unfetch (D1: nothing writes it; the update A
  runs after the unfetch). Hence the unfetch XORs the *same* ROM word
  the fetch did, for every instruction including SWAPBR, and IR returns
  to 0 unconditionally. The v1 hack — the extra SWAPBR-keyed unswap
  before the unfetch — is deleted, not repaired. There is no "(un)branch
  keying" problem left: the branch acts only on BR; fetch/unfetch key
  only on the invariant PC; the PC update keys only on the post-execute
  BR value, which the reverse step still has (BR is untouched by A).

- **D4 — Proof sketch: `step_v2` is a total bijection, injective on the
  full State space.** Write State = PC × Rest.

  1. `fetch` writes only IR and s1, keyed on PC ⇒ preserves PC; it is
     an iso by construction (self-inverse XOR mask per PC fiber).
  2. `decode_execute'` is a Seq of per-op predicated isos none of which
     has PC in its write set (D1) ⇒ preserves PC; each is an iso.
  3. Hence `E = fetch ; decode_execute' ; fetch` is a bijection with
     PC∘E = PC: it decomposes fiberwise, `E = ⊕_p E_p`, each `E_p` a
     bijection of the fiber {PC = p} onto itself.
  4. `A : (PC, BR, rest) ↦ (PC + BR + 1, BR, rest)` is a bijection —
     inverse `(PC, BR, rest) ↦ (PC − BR − 1, BR, rest)`.
  5. `step_v2 = A ∘ E` is a composition of bijections. ∎

  Note what is *not* used: reachability. Injectivity holds on all
  2^160 states, so totality (ADR-0007) needs no side condition.

  **The counterexample dissolves** rather than becoming unreachable —
  which is strictly stronger than the classical presentation. Given any
  post-state, the predecessor PC is the *function* `PC' − BR' − 1`;
  two colliding pre-states would have to agree on it. Re-running
  step.rs's pair under v2 semantics: A = (PC=5, BR=9, X) with
  `rom[5] = SWAPBR rd` yields BR' = X[rd], PC' = 6 + X[rd], so
  `PC' − BR' − 1 = 5`; B = (PC=9, BR=5, Y) via fall-through (BR
  untouched... note B's BR=5 ≠ 0 makes it a *jump* under v2, landing at
  PC' = 15, BR' = 5, `PC' − BR' − 1 = 9`). Equal post-states would
  force 5 = 9. No choice of X, Y collides.

- **D5 — The come-from / paired-branch discipline is a static,
  assembler-level well-formedness check** (`asm.rs`, "the P3 checker"),
  *not* a machine-level hazard: a program violating it still executes
  as a perfectly reversible (just unintended) computation. The
  discipline, concretely — every `SWAPBR` must belong to exactly one
  pairing pattern, with offsets checked by the assembler:

  - **JUMP L** from site `a`: load `rj ← δ` reversibly (XORI/ADDI,
    known constant δ = L − a − 1); `SWAPBR rj` (BR ← δ, rj ← 0);
    the machine lands at L. **Come-from at L:** `SWAPBR rj` (BR ← 0 —
    sequential execution resumes; rj ← δ, the come-from witness);
    `SUBI rj, δ` clears the witness. A join with several comers must
    dispatch on the witness value to clear it — *the join enumerates
    its comers*, which is P3's "paired-branch assembly mirrors clause
    coverage" made literal (and Janus's `fi` assertion in ISA clothes).
  - **CALL f / RETURN:** callee entry `SWAPBR rl` captures the incoming
    offset in the link register (BR ← 0); return computes the negated
    return offset into rl from the captured witness (reversible
    arithmetic on a known frame constant) and `SWAPBR rl` jumps back;
    the instruction after the call site is the matching come-from.
  - **Conditional (CSWAP-then-SWAPBR):** `CSWAP rc, rb, rt` moves the
    target offset into rb iff the condition bit; `SWAPBR rb`. The
    target's come-from mirrors the same pattern **under the same
    condition value**; that the condition register is preserved across
    the jump is the compiler's obligation (same epistemic status as a
    Janus `fi` assertion), while the offset arithmetic is checked
    mechanically.
  - **HALT — a closed orbit, never a fixpoint.** A total bijection has
    no *reachable* fixpoint (its unique preimage is itself), so
    reversible halting is an idle cycle. Convention: addresses `a−1`
    and `a` both hold `SWAPBR rh`, `rh` preloaded 0xFE. Entering at
    `a` with BR = 0: step 1 → (PC = a−1, BR = 0xFE, rh = 0); step 2 →
    (PC = a, BR = 0, rh = 0xFE) — period 2. Only {PC, BR, rh} move on
    the orbit; every *answer* register is invariant, so results are
    read modulo the designated halt register. ADR-0033 D3 builds the
    ring's run-length story (R is an upper bound) on exactly this
    orbit.

- **D6 — Encoding (amends tisc-1-spec §3, still under TBD-9):** SWAPBR
  keeps R-format opcode 1000 and now *uses* its rd field (`SWAPBR rd`);
  rs, rt remain reserved-zero. No State-layout change (BR exists, 8
  bits, offset-valued from now on). No new opcodes: JUMP/CALL/RETURN/
  HALT are assembler macros over SWAPBR + existing arithmetic.

## What a follow-up implementation task builds

1. `decoder.rs`: re-implement `swapbr_instr` as the BR↔rd crossbar
   exchange (EXCH-shaped, opcode-predicated); delete nothing else.
2. `plumbing.rs`/`step.rs`: add `pc_add_br()` (ripple add BR→PC, ≤K2,
   ~1.2k cells by the §5c scale — small next to the 4.8k step); build
   `step_v2 = seq_balanced [fetch, decode_execute', fetch, pc_add_br,
   pc_inc]`; remove the SWAPBR-keyed unswap phase and the module-doc
   obstruction section (superseded by this ADR, cross-reference it).
3. `asm.rs`: JUMP/CALL/RETURN/HALT macros + the P3 pairing checker
   (violation = assembly error).
4. Tests (the differential-oracle norm, vs `theseus_eval`):
   step.rs's counterexample pair mapped to distinct post-states; a
   taken jump with come-from, forward AND backward (adjoint through
   the branch); call/return round-trip; the period-2 halt orbit;
   bit-identical replay of the M4-v1 Fibonacci/Feistel straight-line
   ROMs (BR = 0 compatibility).

## Consequences

- `step` stops lying: SWAPBR is functional, injectivity is structural,
  and the ring machine (ADR-0033) gets data-dependent run length —
  together they end unroll-only.
- The v1 counterexample moves from "documented obstruction" to
  regression test.
- BR's meaning changes (come-from *address* → PC *offset*); the v1
  degenerate behavior never exposed BR to programs, so nothing breaks.

## Alternatives rejected

- **Keep PC↔BR exchange, add a re-swap predicate** — proven dead in
  step.rs: any key over {PC, BR, ROM} is violated one instruction after
  every branch.
- **Make colliding states merely unreachable (pure discipline, v1
  semantics)** — turns totality into a reachability side condition and
  the coverage checker into a whole-program analysis; D1/D2 get
  unconditional injectivity instead.
- **A direction bit in State (Axelsen et al.'s dir)** — redundant with
  the iso algebra's adjoint; direction is modal (C2.5).
- **Dedicated branch opcodes (BEZ/BNE families)** — more decoder area
  and more pairing cases; CSWAP+SWAPBR composes the same behavior from
  audited parts (P5 minimalism).
