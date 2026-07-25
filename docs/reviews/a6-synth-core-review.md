# A-6 synthesizer core — adversarial review (A-6.0 + A-6.1)

- **Scope:** `crates/theseus-tisc/src/synth.rs` (`transcribe_inplace`, `transcribe_into`,
  `transcribe_bennett`, `swap_operands`, `schedule`) + `tests/synth_inplace.rs`,
  `tests/synth_bennett.rs`, against the oracles `bveq::{symbolic_exec, equiv}` and
  `compile::exec_instr`. Branch `a6-synth` @ `3ee8254`.
- **Reviewer:** fresh-context agent, different model from the Opus-5 implementers. Read-only;
  findings confirmed by executable counterexamples (probe test run then deleted; bodies and final
  states below are the actual program output, not speculation).
- **Verdict up front:** the in-place core (A-6.0) is solid inside its class. The Bennett wrapper
  (A-6.1) has **two confirmed silent-wrong-body bugs** (input-contract holes, MAJOR-1/2), the
  **scratch-ancilla path is NOT trustworthy as-is for A-6.2** (MAJOR-3), and the recognizer basis
  **cannot express gcd2** (MAJOR-4) — A-6.2 will wall without new machinery.

---

## BLOCKER

None. No path was found where a wrong body passes the *register-symbolic* oracle (`symbolic_exec`
+ `equiv` at `w = BITS` compares all 8 regs × 8 bits over the whole domain — no vacuity found).
Every wrong-body finding below is catchable by *some* downstream check; the MAJORs are where the
check that would catch it is absent, sample-only, or not run on the path A-6.2 will actually use.

## MAJOR-1 — `transcribe_bennett` silently miswires carrier rows that READ non-carrier registers

- **Assumption broken:** "the permutation must be identity on non-carrier registers" is enforced
  only for non-carrier *rows*. Nothing requires carrier rows to *read* only carriers. The map
  padding `src_map[r] = carrier_ops.get(r).copied().unwrap_or(0)` (synth.rs:695) silently wires
  every `Var(y)` with `y ≥ n` to **operand 0**.
- **Confirmed counterexample:** `fwd = bwd` = CNOT r2→r0 (`row0 = v(0)⊕v(2)`, all other rows
  identity — passes the identity-outside-carriers check), carriers `[0,1]`, ancilla `[3,4]`.
  Wrapper returns **Ok** with body
  `[Xor{3,0}, Xor{3,0}, Xor{4,1}, Exch{0,3}, Exch{1,4}, Xor{3,0}, Xor{3,0}, Xor{4,1}]`
  — both `Var(0)` and `Var(2)` read operand 0 and cancel. From `(r0,r1,r2)=(0x11,0x22,0x44)`:
  final `r0 = 0x00` (want `0x55`) and **ancilla op3 = 0x11 (dirty)**.
- **Class:** correctness bug (silent wrong body from an `Ok` return; defeats "correct by
  construction"). The register-symbolic check catches it whole-domain; the sample-only scratch
  exec check catches it only if a sample distinguishes operand 0 from the miswired register (a
  sample with `r0 == r2` false-greens that sample).
- **Fix shape:** walk `fwd`/`bwd` carrier rows and reject any `Var { operand: y, .. }` with
  `y ≥ n` (or extend the operand maps to cover external reads and exclude them from the swap).

## MAJOR-2 — no duplicate/disjointness validation of the operand maps (net-zero can hold while carriers are WRONG)

- **Assumption broken:** "`ancilla_ops` must be … disjoint" and dst freshness are *documented*
  preconditions but unenforced beyond the single carrier-vs-ancilla containment loop. Duplicates
  *within* `carrier_ops` or *within* `ancilla_ops` pass; `transcribe_into` (a `pub` API) checks
  nothing at all about `dst_ops` (dups, aliasing with sources, reserved/out-of-range indices).
- **Confirmed counterexample:** swap spec, carriers `[0,1]`, ancilla `[3,3]` → **Ok**, body
  `[Xor{3,1}, Xor{3,0}, Exch{0,3}, Exch{1,3}, Xor{3,1}, Xor{3,0}]`. From `(0x12,0x34)`: final
  `r0 = 0x26` (want `0x34`), `r1 = 0x12`, **ancilla op3 = 0x00 — net-zero HOLDS**. This is the
  nastiest shape: it specifically defeats any "ancilla came back to zero" teeth; only the
  carrier-vs-`apply_fwd` comparison catches it.
- **Class:** correctness bug (silent wrong body). Fix: validate pairwise-distinctness of
  `carrier_ops[..n]` and `ancilla_ops[..n]` in `transcribe_bennett`; in `transcribe_into`
  validate `dst_ops` used entries pairwise-distinct and disjoint from every source they read
  (see also MINOR-4).

## MAJOR-3 — the scratch-ancilla Bennett path (what gcd2/A-6.2 needs) is NOT trustworthy as-is

This is the attack-surface-#2 verdict. The scratch path's *only* evidence today:

1. `bennett_*_netzero_scratch_ancilla` runs the body through **`exec_instr`** on **3–4 handpicked
   samples**. `exec_instr` is documented in compile.rs as "**NOT the authoritative CPU model**"
   and is *permissive*: it executes ISA-illegal instructions without complaint. Confirmed: it
   happily ran `Ccnot{0,0,2}` (non-distinct → non-bijective result `x∧¬z`-shaped, the exact thing
   a reversible CPU cannot do) and a body using **decoder-reserved s0/s1** as ancilla
   (`Xchg{sx:0/1}`, `Xor{rd:8/9}`) — that body *passes the exec net-zero check end-to-end and even
   computes the right swap*, while **every one of its instructions is rejected by
   `asm::encode`**. Passing the exec tier therefore does not imply the body can even be
   assembled, let alone that it runs on `system_step`.
2. **No synth test ever calls `encode`/`assemble` on an emitted body.** Cheap, high-value gap.
3. **No teeth on the scratch path.** The only Bennett teeth test
   (`bennett_wrong_backward_breaks_netzero`) uses *register* ancilla; nothing demonstrates that a
   wrong scratch body fails anything. There are also no teeth on `bennett_symbolic_check`.
4. Sampling: 3–4 vectors is not whole-domain, and MAJOR-2 shows a real failure class where the
   net-zero half of the check passes and only the carrier compare (on a lucky sample) fails.

**The implicit bridging argument** — "the scratch body is the same gate skeleton as the
symbolically-verified register body, modulo operand renaming and `Exch`→`Xchg`, and A-0 validated
operand routing + `Xchg` against the real CPU" — is *reasonable* but currently **unstated and
unenforced**: nothing asserts skeleton isomorphism, and MAJOR-1/2 show the skeleton itself can be
silently wrong. `zero_out` itself is fine (substituting ancilla input vars with `Const(false)` in
the post-exec transfer is exactly restriction to the zero-init subspace — sound, and matched by
`with_scratch_frame`'s zero-init guarantee; it proves nothing about dirty-scratch starts, which is
Bennett-inherent, not a bug).

**Required before A-6.2 builds on this path** (any wrong-body bug above then becomes un-hideable):

- (a) **Register-ancilla symbolic dry-run** of the same `fwd`/`bwd` through the same wrapper
  (gcd2: 4 carriers + 4 ancilla = exactly 8 regs — it fits). This is whole-domain and, per the
  attack-#3 analysis below, a *complete* `bwd = fwd⁻¹` check.
- (b) **`encode()` every emitted instruction** (catches reserved scratch, non-distinct operands,
  out-of-range) — one assert-loop.
- (c) For gcd2 specifically, **whole-domain exec** (256 states) instead of samples.
- (d) Optionally assert scratch body == register body under the operand renaming (skeleton
  isomorphism), making the bridging argument explicit.

## MAJOR-4 — the recognizer basis cannot express gcd2: A-6.2 walls here (design-reach, not a code bug)

- `recognize_into_row` requires **byte-parallel uniformity**: the same sorted source set and the
  same single AND term at *every* bit (const mask may vary). Confirmed: a row with
  `bit0 = a0⊕b0`, other bits identity → `Err(RowNotInPlace: "affine sources differ across
  bits")`. This is the *correct, honest* behavior for the shipped slices.
- But gcd2's per-bit forward/backward rows (arbitrary boolean functions of (t,a,b,h) bits, per
  the A-6.2 plan) are **not** byte-parallel-uniform and contain multi-AND/Mux terms per bit. The
  Bennett wrapper's generality is bounded by `transcribe_into`'s basis: today it synthesizes only
  {whole-register XOR copies, const masks, ONE byte-parallel Toffoli per row} + the swap step.
  **`transcribe_bennett` as shipped cannot emit gcd2.** A-6.2 needs new machinery (per-row
  multi-gate ESOP/Toffoli-network decomposition, bit-alignment via `Rotl/Rotli` — currently
  entirely unused by the synthesizer — or sequential composition of multiple specs). Flagging now
  so the wall is planned, not discovered mid-slice.

## MINOR

- **MINOR-1 — const-parity bug (both recognizers):** `has_const` is a bool, not a parity fold. A
  bit-row `v(0,b) ⊕ Const(true) ⊕ Const(true)` (semantically identity) transcribes **Ok** to
  `[Xori{0, 0xFF}]` — confirmed. Violates the documented contract "the result, when Ok, satisfies
  equiv". Caught by the register oracle (confirmed: counterexample at reg[0] bit 0) and, being a
  constant offset, by any exec sample — no false green found, but fix the parity fold.
- **MINOR-2 — Ok-with-ISA-illegal-body (Ccnot self/non-distinct):** spec `x ^= x∧z` transcribes
  **Ok** to `[Ccnot{0,0,2}]` — confirmed. `symbolic_exec` and `encode` both reject it downstream
  (`NonDistinctOperands` / `CcnotNotDistinct`), so the register path fails loudly; but
  `exec_instr` executes it *silently and non-bijectively* (confirmed `r0: 0xFF → 0xF0` with
  `z=0x0F`), so on an exec-only path it is a live hazard. The transcriber should return
  `Err(RowNotInPlace)` when the AND term touches the target (the spec is non-bijective anyway) or
  when emitted operands collide (`transcribe_into`: `src_ops[y] == dst` in an AND term).
- **MINOR-3 — reserved/out-of-range ancilla operands accepted:** ancilla `[8,9]` (decoder-reserved
  s0/s1) → wrapper **Ok**, exec check would PASS (confirmed: correct swap, s0/s1 restored to 0 in
  the fast sim), `encode` rejects every instruction. Fails safe at ROM time but long after
  "verification" said green; ops ≥ 16 would panic `operand_get` instead of returning `Err`.
  Validate operand ranges (reject 8, 9, ≥16) in `transcribe_into`/`transcribe_bennett`.
- **MINOR-4 — the `dst==src` copy-skip guard converts a contract violation into a silent gate
  drop:** for a NON-identity row with an aliased map (`row0 = v(0)⊕v(1)`, `dst_ops[0]=1`),
  `transcribe_into` returns **Ok** `[Xor{1,0}]` — the needed `x1` term is dropped, no error
  (confirmed). Inside `transcribe_bennett`'s own (checked) usage the skip only fires on the
  harmless identity self-map rows, so this is a hole in the *public* `transcribe_into` contract:
  restrict the skip to spec-identity rows, `Err` otherwise.

## NIT

- **NIT-1:** AND-term uniformity compares the `(y,z)` tuple unsorted — a spec writing
  `And(v1,v2)` at bit 0 and `And(v2,v1)` at bit 1 is falsely rejected (false negative only).
  Sort the pair like the sources.
- **NIT-2:** at `w = BITS`, `equiv`'s `operand_regs` parameter is inert (constraint is `None`,
  all bits compared) — the `vars` lists threaded through every synth test do nothing. Fail-safe,
  but misleading; a comment would prevent a future reader from believing they scope the check.

## Attack-surface answers not covered above

- **#1 vacuity/false-green (register path): not breakable.** `equiv` at `w=8` compares all
  8 regs × 8 bits whole-domain via BDD with one shared VarMap; `symbolic_exec` hard-rejects
  non-register-pure ops, out-of-range regs, and ISA-illegal aliasing (mirrors `encode`). Cross-bit
  wiring (`Var(y,b')`, `b'≠b`), `Not`, `Or`, per-bit muxes, and non-uniform rows are all rejected
  by `classify_term`/uniformity, never mis-emitted — every probe of this class produced `Err`,
  not a wrong body. `Const(false)` is dropped at flatten time (correct XOR identity);
  `classify_term`'s reject of a post-flatten `Const(false)` is dead but harmless.
- **#3 `bwd = fwd⁻¹` trust:** the wrapper does NOT check it — but the *register-symbolic* Bennett
  check is a **complete** inverse check: requiring the ancilla output rows ≡ `Const(false)` under
  BDD is exactly `∀x. x ⊕ bwd(fwd(x)) = 0` ⇔ `bwd ∘ fwd = id` (which also forces `fwd` injective,
  so non-permutation specs cannot pass either). An inconsistent pair leaves carriers = `fwd(x)`
  (still "correct") but leaks the ancilla — visible whole-domain symbolically, but only on the
  sampled states in the exec tier, so on the scratch path an inconsistency confined to un-sampled
  states passes. Hence MAJOR-3(a): the register-layout dry-run (or gcd2's 256-state whole-domain
  table cross-check, already planned in A-6.2 — make it mandatory) is the guard.
- **#4 ordering/aliasing (in-place scheduler): sound for its class.** The reader-before-writer
  edge rule is complete here because every unit writes exactly its own register(s) (Cswap dedup'd
  as one unit; its data regs are its own writes; its control is required identity, so a control
  writer cannot exist), the target-self-read is folded into the single gate, and any residual
  cycle → `Err(OrderingConflict)` (test-covered). The double-write branch is defensively
  unreachable. `peel_self_xor`'s even-parity reject is correct (`x⊕x⊕y = y` is genuinely not
  in-place). `swap_operands`' scratch↔scratch reject is correct against the ISA (no such
  instruction; `Xchg` is reg↔scratch), and its reg↔scratch index math (`sx = op − 8`) matches
  `operand_get`/`exec_instr`; the reserved-s0/s1 gap is MINOR-3.
- **#5 teeth adequacy:** in-place teeth are real (corrupted spec fails `equiv`; each `Err` class
  covered; `identity → empty` covered). Bennett teeth are register-only and net-zero-only: no
  teeth on the symbolic Bennett check, none on the scratch path at all, and MAJOR-2 exhibits a
  wrong-body class where net-zero *holds* — so "ancilla dirtied" teeth alone are the wrong signal;
  the carrier-vs-ground-truth compare must be part of every teeth assertion (it is present in
  `bennett_exec_netzero`, absent in the teeth test).

## Bottom line

- `transcribe_inplace` (A-6.0): **ship-quality within its class**; fix MINOR-1/2 opportunistically.
- `transcribe_into`/`transcribe_bennett` (A-6.1): the emission logic is right when the caller
  honors every unenforced precondition, but the wrapper's promise of "correct by construction" is
  defeated by MAJOR-1/2 — both are cheap input-validation fixes and each must land **with a
  regression test that reproduces the counterexample above** (L20).
- **The scratch-ancilla Bennett path is NOT trustworthy as-is for A-6.2.** It needs, before gcd2
  builds on it: the MAJOR-1/2 guards, encode-every-instruction, the register-layout symbolic
  dry-run (complete `bwd=fwd⁻¹` + wiring check), and whole-domain (256-state) exec for gcd2.
  With those four, the residual scratch risk reduces to A-0's already-validated operand
  routing / `Xchg` semantics — acceptable until A-6.5's real-`system_step` authority run.
- Independently: plan for MAJOR-4 now — the current basis cannot emit gcd2, and A-6.2 as
  scoped will discover that on day one.
