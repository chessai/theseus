# A-6 design — the general prim synthesizer (the keystone) + budget, so GCD ships

**Decided by user (2026-07-25):** fork option (a)+(b). GCD (and every prim) emits through ONE
mechanism — a `BvExpr` PrimSpec → reversible gates → symbolic verify — instead of per-prim hand
gadgets; AND we accept widen-PC/larger-B so the (longer, Bennett) bodies actually run. This
abandons the A-1 hand-walk (finding: `a1-gcd` `3e0e7b4`) as the wrong tool and re-validates the
original roadmap order (provisioning + synthesizer).

Worktree: `/tmp/a6-synth` (branch `a6-synth`, off merged A-0 `5f11e6b`).

## What already exists (do not rebuild)
- `BvExpr` (theseus-core/src/bvexpr.rs): {Var, Const, Not, Xor, And, Or, Mux}. NO Mul — the omission
  keeps BDDs polynomial AND makes synthesis polynomial (§8 keystone invariant).
- `PrimDef.symbolic: Option<PrimSpec>` (prim.rs:119) — the slot for a symbolic spec.
- `symbolic_exec(&[Instr]) -> RegTransfer` (per-(reg,bit) BvExpr) and `equiv(got, spec, BITS, vars)`
  (BDD equality) — bveq. The acceptance oracle: NO enumeration, any width, has teeth.
- POC `tests/bvexpr_synth_poc.rs` (3 tests): hand-written transcriptions of in-place ancilla-free
  specs, verified symbolically; wrong transcription rejected. **The POC hand-writes the gates; A-6
  AUTOMATES that emission.** POC scope note: in-place ancilla-free only; the general Bennett case
  (zero-init ancilla + backward-uncompute) composes the transcription with the frame discipline.

## Slice decomposition (each: POC/oracle → Fable review → real authority → merge)

### A-6.0 — the transcriber core (BvExpr RegTransfer → Instr gates), in-place ancilla-free case
Input: a `RegTransfer` (per-reg per-bit `BvExpr`). Output: `Vec<Instr>` + a symbolic-verify pass
(`symbolic_exec`+`equiv` == spec). Handle the structured in-place forms the ISA realizes directly:
- pure `Var(r,b)` identity rows → no gate;
- affine rows `x ^= Σ vars` → `Xor`/`Xori`;
- `x ^= (y ∧ z)` → `Ccnot`; `Mux(sel,then,else)` on bit0 → `Cswap`; `Not`/`Const` → `Xori` const.
A row is "in-place realizable" when the target reg's new value is `old ⊕ f(others)` with `f` in the
ISA's gate basis and no two rows conflict on ordering. Emit in dependency order; verify. Reject
(don't emit) specs outside this class — they go to A-6.1. Reuse the POC's `spec`/`equiv` harness as
the test bed. DETERMINISTIC transcription — NOT search (this is why A-6 is safe to delegate; the
gcd2 hand-walk was not, and its agents OOM'd the box twice on parallel beam search).

### A-6.1 — the Bennett wrapper (general permutation case, needs ancilla)
For a spec that is a permutation not realizable in place (e.g. gcd2's multi-arm map), emit the
textbook reversible-in-place-permutation:
  1. `O ^= Rule(x)` — transcribe the forward spec into zero-init frame ancilla O (reads x from regs).
  2. swap regs ↔ O (Exch).
  3. `O ^= Rule⁻¹(regs)` — transcribe the BACKWARD spec, clears O to 0 (Bennett uncompute).
Uses A-1's `with_scratch_frame` for the zero-init ancilla (net-zero guaranteed by construction).
Verify symbolically (the composed body ≡ the forward spec on the carrier, frame net-zero). This is
the ~2× body the A-1 finding predicted — CORRECT by construction, length is whatever it is (that's
what (b)/widen-PC funds).

### A-6.2 — gcd2 as a `BvExpr` PrimSpec (forward + backward, per-bit)
Express `gcd2_hist_step` as `PrimSpec { forward: RegTransfer, backward: RegTransfer }` — per-bit
boolean functions of (t,a,b,h) reproducing `gcd2_hist_bytes` and its inverse. This is the concrete
input that drives A-6.1 → the GCD body. Derive from the rule (small bijection); verify the BvExpr
spec itself matches `gcd2_hist_bytes` over the 256 states (a table cross-check) before transcribing.

### A-6.3 — recognize_prim integration
Route a prim with `symbolic: Some(spec)` to the A-6.0/A-6.1 synthesizer (in-place first, else
Bennett). Retire/replace `bveq_verify_emission`'s vacuity guard (compile.rs:888, rejects
symbolic-only) with the symbolic verify path (symbolic_exec+equiv). Keep the table recognizer for
existing table prims (no regression). The synthesizer becomes the general path; the recognizer is a
legacy peephole.

### A-6.4 — budget / widen-PC (HIGHEST BAR — the (b) provisioning)
Decouple `W_pc` from `N_rom` (review BLOCKER-3); widen pc/br/imm/pc_add_br from hardcoded 8-bit;
re-prove ADR-0034 injectivity at the new widths; re-pin energy/ring goldens. This raises B>256 so
the Bennett GCD body runs. Full L15/L19/L20/L21 + Fable review + real-worktree authority (this is
decoder/state-layout, the top bar). Can proceed in parallel with A-6.0-.3 (independent).

### A-6.5 — GCD end-to-end
With synthesizer (A-6.0-.3) + budget (A-6.4): the gcd2 `Trace` compiles + runs
`run_on_cpu == eval` across the variable-T trajectory (≤5 back-edges). GCD SHIPS — first genuine
variable-T loop, via the general mechanism. Real system_step authority + full heavy suite + golden
re-pin → merge.

## Ordering
A-6.0 → A-6.1 → A-6.2 (proves the synthesizer on gcd2 SYMBOLICALLY, no CPU run) → A-6.3 (integrate)
→ A-6.4 (budget, highest-bar, parallelizable) → A-6.5 (GCD runs end-to-end). Prove correctness
symbolically before spending on the budget.

## Process guardrails (post-OOM)
The synthesizer is deterministic transcription — delegable to Opus-5 SAFELY (no search → no parallel
Python beam OOM). If any delegate starts spawning search processes, STOP it (the walk lesson). Each
slice: POC/symbolic-oracle green → fresh Fable adversarial review (different model) → advisor real
authority for CPU-running slices → commit-only, never push.
