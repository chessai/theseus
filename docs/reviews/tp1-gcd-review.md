# TP-1 GCD-formulation review (reversibility / sizing theory)

> Verbatim fresh-context adversarial review (model: Fable), agentId `af16aff502c4e78ea`, committed per maestro L21.
> Verdict OVERTURNED-IN-PART: GCD ⛔ sound (overdetermined by the F2 trajectory-disjointness theorem); TP-1a overturned (not a bijection); the §4 law and M-D.e scope corrected.

---

All evidence is in hand. Here is the complete review.

---

# Adversarial review — TP-1 GCD design (docs/scratch/tp1-gcd-design.md) — L21

Reviewed against: `crates/theseus-tisc/src/compile.rs` (lower_trace 1763–2035, guards 1801/1821/1837/1848/1920/1927/1943, skeleton 1980–2018, aux assignment 1959–1977, constants 328–346), `crates/theseus-eval/src/interp.rs:163–183` (Trace semantics), `docs/scratch/mdd-design.md` (§1–§5), `crates/theseus-tisc/src/types.rs:28,36` (NUM_REGS=8, MEM_WORDS=4), `docs/src/test-program-roadmap.md`.

## (A) VERDICT: OVERTURNED IN PART

- The **GCD half** of the thesis (GCD ⛔ breaks today, is a Tier-3 customer) is **SOUND — and in fact overdetermined**: it survives even if the Bennett-branch argument were wrong, by a stronger formulation-independent theorem the design missed (F2).
- The **TP-1a half is OVERTURNED**: "LFSR-to-target with carrier = x alone" is **not a bijection and cannot be written as a theseus iso at all** (F1). The proposed Tier-1 M-E showcase does not exist as specified; its minimal repair (counter carrier) breaks today on *both* the leaf cap and the WCET guard. The §6 re-tiering "TP-1a → 🎯 Tier-1" is false.
- The **§4 general law is wrong in both directions** (F3), and **M-D.e as scoped will not unblock GCD** (F4) — the design names the wrong (incomplete) milestone.

## (B) Findings

### F1 — BLOCKER: TP-1a (carrier = x, 3 leaves) is not a bijection; no such iso term exists

The design (§5) claims the loop is "bijective by construction (σ is a bijection; the come-from records T)". This confuses machine-level PC-reversibility with source-level bijectivity. `Trace(u, body)` requires **body : u+a ↔ u+b to be one total bijection** (interp.rs:163–183: `s := Right(input)`, iterate `body` until `Right(y)`). For "iterate σ until x == 1" with u = a = b = Bool³:

**Deterministic-exit argument (airtight):** every nonzero input ≠ 1 walks the shared σ-orbit and reaches the *same* final carrier state `Left(1)`. The body is a function, so `body(Left(1))` is a single value `Right(y)` — at least 6 distinct inputs map to the same output y. Trace of a bijection is injective ⇒ contradiction ⇒ **no iso term denotes this program**. (Counting version: needed Left-images = 8 entry + 7 continue = 15 into 8 slots of u.)

The come-from does not rescue this: T lives in *time*, not data, and a theseus iso is a *data* bijection. The design's own listed outputs fail: "output = Unit with T read from the trace" is ill-typed (|a| = 8 ≠ |b| = 1); "output = number of steps" (x₀ ↦ orbit position — a genuine bijection) requires a **counter in the carrier**, u = (x, c) ≥ 6 leaves > MAX_LEAVES_LOOP = 3, and |u| ≥ 64 ⇒ k_max ≥ 65 ⇒ this_wcet ≥ 4 + 65·10 = 654 > 256 — rejected by the WCET guard (compile.rs:1943) even with an *empty* body. **TP-1a breaks today twice over. Re-tier it to ⛔ Tier-3.**

### F2 — MAJOR (new theorem the design needed): trajectory disjointness bounds every variable-T Trace

Body bijective ⇒ backward-deterministic ⇒ trajectories from distinct inputs are **vertex-disjoint in Left(u)** (a shared Left-state would have two predecessors walking back to two different Right entries). Consequences:

1. **Σ over inputs of (T_i − 1) ≤ |u|** — the sum-over-inputs version of mdd §1's per-trajectory `t ≤ |u|+1`, which the design never took.
2. If every input takes ≥ 2 body applications, **|u| ≥ |a|**: the carrier must be at least input-width. Bijections can't shed information mid-loop.
3. Exit carrier-states are pairwise distinct across inputs ⇒ **"iterate until x == constant" is impossible without a distinguishing witness in the carrier** — the exit-until-target test is *itself* a Bennett-taxed erasure. The design taxed GCD's internal branch but exempted TP-1a's exit test; they are the same tax.

Applied to TP-1a's advertised histogram: uniform T ∈ 1..7 over 7 inputs needs Σ(T−1) = 21 > |Bool³| = 8 — impossible with a 3-leaf carrier, independent of everything else.

Applied to GCD: nearly all 2^{2w} pairs iterate at least once ⇒ |u| ≳ 2^{2w} ⇒ **≥ 2w payload leaves for any formulation whatsoever** (6 > 3 at w=3; 4 > 3 at w=2). The ⛔ re-tiering of GCD therefore does **not** hinge on §1's branch argument at all.

### F3 — MAJOR: the §4 law is wrong in both directions

- **Too strong:** an internal data-dependent *controlled op* (CSWAP gated on a carrier bit — Fredkin) is a data-dependent branch in every meaningful sense, lowers Swapbr-free, and accumulates zero history. A `Plus` over an *existing* sum tag is likewise semantically history-free — the "branch record" is already part of the output type (the tag survives; nothing is merged). History accumulates only on an **untagged join of branch arms with overlapping images** (GCD's two subtractive arms each cover the whole quadrant — that is the real criterion). Note `Plus` in a Trace body *is* rejected today, but by the **F6 Swapbr-free-body policy** (compile.rs:1920–1924; Plus lowers via `emit_skip_block` → Swapbr, compile.rs:704–759), i.e. a compiler policy, **not** Bennett history. The law conflates the two.
- **Too weak:** branch-free bodies still fail — via the exit-until-target erasure (F1/F2.3), the leaf cap, and the WCET wall.
- **Correct law:** a Trace fits today iff (a) its needed transitions form a partial injection satisfying F2 (in particular distinct inputs have distinct exit states); (b) payload leaves ≤ 3; (c) the body lowers Swapbr-free; (d) `4 + (|u|+1)(|body|+10) ≤ 256` — where (d) already binds a *full* 3-leaf carrier to ≤ 18 body instructions (|u| = 8 ⇒ k_max = 9).

### F4 — MAJOR: M-D.e alone cannot unblock GCD; the design omits the WCET wall and the k_max problem

`k_max = |u| + 1` is computed from **type cardinality** (compile.rs:1810 `enumerate_values(u).len()`, 1816), and the guard is `4 + k_max·iter_len ≤ 256` (1936–1948). A 12-leaf GCD carrier has |u| = 2^12 ⇒ k_max = 4097 ⇒ wcet ≈ 10^5. Even a **5-bit** carrier is rejected with an empty body (33·10 = 330 > 256). So after M-D.e lands, GCD is still rejected — the §7 acceptance "when M-D.e lands, flip to ✅" is false. GCD additionally needs (i) **widen-PC** (acknowledged only in mdd §5, never here) and (ii) a **k_max-tightening mechanism** (typed fuel / verified iteration bound) — actual GCD K ≈ 6 vs pigeonhole 4097; without (ii) the M-E park-tax figure would be (B−T)/B ≈ 99.8% for every input, dominated by the loose bound, destroying the very showcase this program exists for. That mechanism is a missing milestone the design should have named.

### F5 — MODERATE: §2's reversible-GCD formulation is not well-typed as written

(i) Output `(gcd, hist)` is w+K bits vs 2w-bit input — `|a| ≠ |b|` violates Trace typing and the a/b payload-leaf guard (compile.rs:1801–1807). "`|out| = |in|`" (§2) is false for the literal types (2^9 ≠ 2^6 at w=3); a total iso needs input padding + junk arms (garbage-in-garbage-out slices), which changes the §2 arithmetic. (ii) `hist«branch` on a K-bit register drops the top bit — non-injective on the full type; fine on reachable states, but the body must be a *total* iso, so the standard partial-injection extension must be stated. Neither is fatal, but §7's "write the .thse; assert it is rejected" requires the iso to be *writable* first.

### F6 — MINOR: MAX_LEAVES_LOOP = 3 is verified but is an artifact; 4 is achievable — and nearly useless

Confirmed: `const MAX_LEAVES_LOOP: usize = 3` at compile.rs:339 (design cites :335 — minor drift), rationale comment 335–338; independently enforced by `total_regs_needed = n_carrier + 4 ≤ 8` (1868–1873); aux assigned rc/rb/temp/ctrl (1965–1969). However **rc and rb have provably disjoint nonzero ranges** (rc nonzero only Ltop-SWAPBR→join-CSWAP; rb nonzero only bottom-CSWAP→Lend-SWAPBR; the δu token conservation guarantees never both) and never co-occur as operands (top uses (rc,temp), bottom (temp,rb)) — they can merge into one register: 3 aux + tag ⇒ cap 4. Immaterial for GCD (needs ≥ 6 by F2) and of marginal value anyway: a 4-leaf carrier gives |u| = 16 ⇒ body ≤ ~4 instrs under WCET (mdd §5). If pursued, it is daemon-control-flow-adjacent and needs its own adversarial hand-trace + A2-class real-CPU test.

### F7 — MINOR: step-variant inconsistency in the K arithmetic

§1 line 16 uses swap-as-an-iteration; the collision example (line 20) uses `b -= a`. K = 6 (gcd(7,1)) holds for the b-=a variant; the swap variant makes (1,7) take 7 iterations ⇒ hist(7), 13 leaves. Immaterial (both ≫ 3) but "12 payload leaves" should name its variant.

### F8 — CONFIRMED (Q5): the come-from cannot double as a branch recorder

Skeleton compile.rs:1980–2018: `ctrl` is computed from tag at the bottom and uncomputed from the *same* tag at the top each iteration; the δu token is minted and burned once per iteration; the invariant rc = rb = temp = ctrl = 0 at every body entry is exactly what makes iterations compose (mdd §2). Fixed-width registers restored to zero each iteration cannot accumulate per-iteration bits; any recording would break the token discipline. §1's blockquote is exactly right.

### F9 — (Q6): M-D.e is coherent — no reversibility obstruction — but see F4

Paging composes: XCHG/MEXCH are involutions; a page-in prefix / page-out suffix inside the body preserves `homes_in == homes_out` (guard at 1927), so the layout invariant holds at every back-edge; the jump window (bottom-CSWAP → SWAPBR → join) touches only aux; scratch/mem are inert w.r.t. PC/BR. Capacity works: s2..s7 (6) + MEM_WORDS = 4 ⇒ 10 pageable slots ≥ the 9–10 spilled GCD leaves. **But** per F4 it is WCET-dead-on-arrival for every carrier ≥ 5 bits: M-D.e should be re-scoped as *spill + widen-PC + typed iteration bound*, or re-ordered behind them.

## (C) Q1 verdict: NO history-free reversible GCD exists — with two corrections to the design's argument

**Verified:** the 2-to-1 collision is real: (5,3) →[a>b] (2,3) and (2,5) →[a<b] (2,3). **Canonicalization does not help** (design is right to suspect this): on the a ≥ b invariant with re-sort — `if a−b ≥ b: (a−b,b) else (b, a−b)` — the output (x,y), x > y has two valid preimages (x+y, y) [no-swap] and (x+y, x) [swap]: concretely **(7,2) and (7,5) both → (5,2)**. Structurally, every subtractive step is the Stern-Brocot **parent map** — each (x,y) has exactly two children (x+y, y) and (x, x+y) and the step merges them — so *any* subtraction-descent form is 2-to-1 and the branch bit is one bit of the continued-fraction expansion. Division form loses the quotient (all of (qb+r, b) → (b, r)); binary/Stein loses the parity-case. No small-step arithmetic form escapes.

**Correction 1 (the stronger proof):** the design's per-formulation branch argument is unnecessary — F2 gives a *formulation-independent* impossibility: any iterative GCD's inputs share exit states ((g,g)/(g,0)) and take ≥ 2 body applications almost everywhere, so |u| ≥ ~|a| = 2^{2w} ⇒ ≥ 2w payload leaves ⇒ over the cap for all w ≥ 2, for **any** body whatsoever. (Loophole to state honestly: any bijection is trivially a T=1 Trace with u unused — "history-free GCD" exists as a *combinational* body computing (a,b) ↦ (gcd, rank) directly; the claim holds only for genuinely iterative small bodies, and the design should say so.)

**Correction 2 (the size claim):** the 12-leaf figure (raw K-bit shift-register hist, K ≈ 2^w−2) is a *particular naive encoding*, not a lower bound. The information floor is ~2w bits (output (g, rank-within-gcd-class) is a bijection on 2^{2w} values), and an in-place quotient/continued-fraction packing (the freed high bits of the shrinking pair absorb the CF code; Σ quotient-bit-lengths ≈ log(ab/g²)) plausibly approaches it — a far cheaper Tier-3 target once M-D.e + widen-PC exist. GCD's tiering is unaffected (6 > 3 either way), but the M-D.c "history width" pressure is overstated by the naive encoding.

## (D) TP-1a: does NOT fit, and is not even history-free — it is not a function

Overturned per F1/F2. Point-by-point on the prompt's checks: (a) the LFSR *step* is genuinely branch-free (linear GF(2) bijection) — true but irrelevant; (b) the composed body is **not** a bijection (deterministic-exit argument; 15 needed Left-images into 8 slots) — this is the fatal step the design skipped; (c) moot, though note the design also left zero margin for the x==1 test's compute/uncompute and for input x=0 (σ(0)=0 never reaches 1 — totality forces an explicit 0-exit arm the design doesn't mention); (d) the LFSR facts themselves are correct (3-bit maximal LFSR, e.g. x³+x+1, period 7, 0 excluded, orbit position ∈ 1..7); (e) the exit *test* is indeed consumable (predicate computed into the fresh tag, ancillas uncomputed from the unchanged x) — the erasure problem is not the test bit but the *convergence of all inputs onto one exit state*. **Salvage:** a contrived u = Bool³, a = Bool³ permutation with disjoint variable-length paths gives *some* park-tax variance today (Σ(T−1) ≤ 8, e.g. bimodal T ∈ {1, 9}), but never the uniform 1..7 histogram (needs 21 > 8). The honest M-E claim: meaningful data-dependent-T histograms need the counter carrier ⇒ M-D.e + widen-PC + k_max reform; today's ceiling is a bimodal toy.

## (E) The §4 law: not exactly right — wrong in both directions (F3)

Replace with: *fits today iff (needed transitions are a partial injection with per-input-distinct exit states) ∧ (payload ≤ 3) ∧ (Swapbr-free lowering) ∧ (4 + (|u|+1)(|body|+10) ≤ 256)*. "Branch-free" is neither necessary (Fredkin/controlled ops, tag-dispatched Plus are history-free; Plus is blocked only by F6 policy) nor sufficient (exit-until-target erasure; WCET).

## (F) What's SOLID

1. The 2-to-1 collision claim — verified concretely, including the ordered-invariant variant ((7,2)/(7,5) → (5,2)); canonicalization genuinely doesn't remove the branch.
2. The come-from scope statement (§1 blockquote) — exactly matches the implementation (F8); machine come-from records only the back-edge and structurally cannot accumulate.
3. MAX_LEAVES_LOOP = 3 and its derivation — real, enforced twice (compile.rs:339/1821 and 1868–1873); the GCD leaf arithmetic (12 ≫ 3; even 2-bit GCD's 7 > 3) checks out modulo F7's variant nit.
4. **GCD ⛔ breaks today** — correct and now overdetermined (leaf cap by any formulation via F2, plus the WCET guard); TP-2 Collatz ⛔ likewise correct (shared exit state at n=1 *and* overlapping arm images, e.g. 8→4←1).
5. M-D.e has no reversibility wall (F9) — it is buildable; it just isn't sufficient (F4).
6. The feedback-loop method itself: this design did exactly what the roadmap intends — the *review* of it surfaces the real milestones (widen-PC + typed iteration bounds alongside spill) and a new theorem (trajectory disjointness, F2) that should be added to the docs as the sizing law for all future Trace programs.

**Recommended actions:** re-tier TP-1a to ⛔ Tier-3 (customer for M-D.e + widen-PC + k_max reform, same as GCD); replace §4 with the F3 law; add the F2 theorem (Σ(T−1) ≤ |u|; |u| ≥ |a| for always-iterating loops; distinct-exit-states requirement) to mdd-design/ADR-0007's bound family; fix §2's typing (F5) before writing the rejection-witness .thse; re-scope or re-order M-D.e per F4; optionally file the rc/rb merge (F6) as a separate adversarially-reviewed change.
