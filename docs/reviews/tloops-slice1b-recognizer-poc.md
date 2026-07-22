# T-loops slice-1b POC: `gcd2_hist_step` recognizer (2-bit Bennett GCD)

*(Advisor's faithful capture of the Fable POC-build report, L21. Code preserved:
`docs/scratch/gcd_poc.rs.ref` (434 lines) + `docs/scratch/gcd2-recognizer-arm.diff.ref`
(the `recognize_prim` arm + pinned `gcd2_hist_bytes` byte rule).)*

## STATUS: PARTIAL — BLOCKED-AT-EMISSION

Everything up to and including RECOGNITION is built and proven end-to-end in the real compile
pipeline; the sole unbuilt piece is the emitted instruction walk, blocked by a precisely
characterized WCET-vs-emission-cost wall. 5/5 POC tests green; fast-gate theseus-tisc suite green.

## (1) The w=2 body — PROVEN
`u = io = (Flat(2),Flat(2),Flat(3))`, carrier `Sum(u,io)` = 4 leaves `[tag,a,b,h]`, 256 canonical
states. `admit()==Ok`; `eval(Trace)==ref_gcd` on all 16 (a,b); outputs injective; eval-level T
histogram `{0:10, 2:2, 3:4}` — T varies, T_max=3, history peaks at h=7 so **Flat(3) exactly suffices**.

## (2) The recognizer — recognition PROVEN, emission not built
Wired in `recognize_prim` exactly like the `mde_spill_walk` precedent: name-gate (`gcd2_hist_step`,
n_in==n_out==4) → `check_pattern_kleaf(…, 4, gcd2_hist_bytes)` full-domain semantic check → (emission
slot). Positive: `compile_with_metadata(Trace(v4 body))` fires the arm, verifies all 256 table entries
against the pinned byte rule, returns `Deferred("… table verified … no TISC-1 emission fits the Trace
WCET budget …")`. Negative (reject-don't-corrupt): a leftover-paired body (same name, different junk)
is REJECTED → `UnrecognizedPrim`. Test-rule and shipped `gcd2_hist_bytes` asserted byte-equal on all
256 states (L20 pinning).

## (3) Numbers
- **k_exact = 5** (machine-measured max 4 applications over all 128 Right entry states + 1 safety).
  Compiled-loop T histogram over the 16 real inputs: `{2:10, 3:2, 4:4}` — variable-T signature.
- **WCET**: `B = 4 + k_max·(body_len+10)`; with k_max=5, **body ≤ 40 instrs** for B ≤ 256.
- **G5 (exact-k_max is LIVE on this carrier)**: `Trace(u_gcd2, Id)` compiles — ROM 14, loop_terms
  [(2,10)], B=24, 128/128 green. Pre-slice-1a this identical probe was the E3 wall (k_max=129, WCET
  1294). **Slice-1a fully removed the cardinality wall — only the emission wall remains.**

## (4) THE WALL — exactly where and why
The walk must be a TOTAL bijection on all 256 canonical states (not just the 42 meaningful arms).
Forced-move chain, each machine- or hand-verified:
1. **History update must be a true SHIFT, not a rotate**: any wrap-around (`rotl8`/`rot3`) makes the
   two subtract branches collide 2-to-1 over the full h-range → no reversible sequence realizes it. So
   the subtract must be gated on an h-window (reachable h∈{1,2,3}); gated-off junk states need arms.
2. **Right-slot conservation forces a scatter arm**: exits can only land in Right slots vacated by
   entering states. The byte0==1 Right region (32 slots) nets only 2 free after entries/returns, but
   the 6 h-distinct `(1,1)`-exits need 6 ⟹ ≥4 junk-entry states relocated cross-payload via a 6-way
   `payload←f(h)` permutation (mod-3 arithmetic; verified necessary by exhaustive slot accounting).
3. **Conditional-op tax**: the ISA's only conditionals are `Cswap` (bit0 control), `Ccnot` (byte-AND,
   needs a mask reg), `Rotl`-by-reg. ALL 4 registers are carrier leaves (budget exactly 4); aux regs
   4–7 hold live loop machinery (`rc`/`rb`) — touching them is the L15 trap. Every gate predicate
   (`h==0`, `a==b`, `a==0`, sign(a−b), h-window) costs a multi-instr mask-alignment gadget (~5–15 instrs
   each) + an uncompute obligation; the subtract-flag uncompute forces strict phase ordering.
4. **Sum ≈ 60+ instructions** best case vs the **≤40 cap**. (A hand-crafted gadget-free miracle isn't
   formally excluded, but the scatter arm alone has no short encoding found.)

## (5) WHAT UNBLOCKS IT (any one)
- **widen-PC** (W=8→9): B>256 ⇒ a 60–100-instr body fits. (Permanent ~+10–15% step_j PER PC bit on
  EVERY program — the ISA-growth energy tax.)
- **A conditional-arithmetic ISA op** (`Csub`/`Cadd`): one op kills ~30 gadget instrs ⇒ body ~30 ⇒
  WCET ≈ 204 < 256, fits WITHOUT widen-PC. (Adds decoder surface → energy tax, like `Ccnot` +9.2%.)
- **One spare scratch register available to Trace bodies** (frees the gadget registers). Cheapest if
  feasible, but Trace loop machinery occupies aux regs (L15-adjacent).
- (weaker) allowing Swapbr skip-blocks in loop bodies (F6 policy).

Note for the eventual w=3 target: gcd outputs vary (1..7) so the slot crunch changes shape, but
Right-slot conservation + junk-entry pressure persist — this analysis carries over.

## Bottom line for the advisor
The recognizer DESIGN is fully de-risked (semantics pinned as `gcd2_hist_bytes`, verification proven
in-pipeline, negative/reject path proven). Delegating "write the gcd2 walk" TODAY would fail (the E6
"≤53 instrs comfortable" estimate was optimistic — the real minimal walk is ~60+). The delegatable
units are instead (a) widen-PC or (b) a `Csub`-class ISA op (measure its decoder energy tax) or (c) a
spare Trace scratch reg — after which the emission slot in the already-wired arm is a mechanical
fill-in against the already-pinned `gcd2_hist_bytes` oracle.
