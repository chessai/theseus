# Toward T-loops (variable-T loops): slice-1 = the first genuine variable-T program — DESIGN (draft r1)

> **REFUTED + RESOLVED (see `docs/reviews/tloops-slice1-design-review.md`, verdict NEEDS-MINIMAL-CHANGE,
> with a 7/7-green POC `docs/scratch/tloop_poc.rs.ref`):** the §0 reframing "4-bit GCD fits today, WCET
> ≈ 252, no change" is **FALSE by 10×** — four measured walls: (0) no `trace`/`while` surface syntax
> (Rust-built `Iso::trace` only); (1) the naive `(a,b)` step is **2-to-1** (Bennett — 105/256 collisions;
> honest carrier is `(a,b,hist)` with a self-delimiting sentinel history, T_max bits: w2→3,w3→7,w4→15);
> (2) **k_max = |u|+1 is carrier-cardinality, NOT a measure** — the binding wall (2-bit GCD: k_max=129,
> WCET=1294; 3-bit: k_max=8193). **widen-PC is NOT the first wall** and alone buys nothing (it would "fit"
> a 99.97%-park-tax program while k_max-looseness remains — a category error). (3) the GCD body has no
> lowering path (recognizer-only). BUT the bijective GCD **EXISTS at eval level today** (admit()==Ok,
> eval(Trace)==gcd over all 64 pairs, T varies 0..7).
> **REVISED slice-1 = "exact-k_max + 2-bit GCD":** (a) trajectory-derived k_max in `lower_trace`
> (fallback |u|+1) — SOUNDNESS-CRITICAL WCET tightening; (b) a pinned `gcd2_hist_step` k-leaf recognizer
> (mde pattern); (c) the 2-bit GCD (|u|=128, Flat(3) history) as the whole-domain `run_on_cpu==eval==gcd`
> + T/park-tax-histogram demo — the first genuine variable-T ALGORITHM, zero energy tax. Both (a),(b)
> touch lower_trace/recognize_prim → **HIGHEST BAR** (adversarial impl-review + behavioral round-trip).
> **widen-PC = the follow-on** (w=4 genuinely needs it even post-exact-k_max; ~+10–15% step_j per PC bit —
> permanent tax, must come AFTER exact-k_max). exact-k_max unlocks w=2 (body ≤53 instrs) and marginally w=3.

**Status:** advisor draft for Fable design-review-with-POC (SUPERSEDED by the review above). HIGHEST BAR if it touches lowering/decoder
(control-flow). Grounded on a thorough Explore map of the PC/WCET/loop machinery (file:line below).
Prior GCD thinking: `docs/scratch/tp1-gcd-design.md`; TP roadmap `docs/src/test-program-roadmap.md`
(TP-1 GCD, TP-2 bounded Collatz — both marked blocked by the "sizing wall").

## 0. What a T-loop is, and the reframing

A **variable-T loop** runs a *data-dependent* number of iterations T, bounded above by the static WCET
budget B; the reversible CPU always runs B steps, and the `(B−T)` surplus is "parked" (the park tax,
`(B−T)/B`). Real examples: GCD (subtractive Euclid), bounded Collatz. Today theseus has the `Trace`
(reversible while-loop) machinery and the park mechanism, but **TP-1 GCD / TP-2 Collatz are blocked by
the "sizing wall"** — WCET > 256.

### The grounding facts (Explore map, file:line)
- **PC is `u8`** (`types.rs:242`, `W=8`), max PC 255, ROM ≤ 256 (`step.rs:128` assert). PC is IN the
  decoder netlist: `pc_pred` (`step.rs:89`) = W−1 Toffolis/address; `pc_inc` (`step.rs:189`) ripple
  with 6 carries; fetch wires `4·W=32`. **Widening PC grows the decoder netlist → ISA-growth energy
  tax on EVERY program's step_j.**
- **The hard wall (explicit reject, no silent wrap):** Trace `WCET = spine + k_max·iter_len ≤ 256`
  (`compile.rs:2688`, `Deferred`); global park guard `B + S_max ≤ 256` (`compile.rs:3209`).
- **Loop bound:** `k_max = |measure|+1` (`compile.rs:2534`, ADR-0007) — the cardinality of the Trace's
  well-founded measure type. Loose measure ⇒ k_max explodes ⇒ WCET blows past 256.
- **BR backward jump:** `delta_u = (256 − iter_len) mod 256` (`compile.rs:2695`) — the **256 is the PC
  modulus**, hardcoded; a wider PC changes this.
- **Carrier leaves:** `MAX_LEAVES_LOOP = 4` (post M-D.e slice-1 `7be4a82`); GCD's carrier is `(a,b)` +
  tag ⇒ ~2–3 payload leaves, likely already within budget.

### The reframing (the crux for slice-1)
The first *genuine variable-T loop* probably does NOT need widen-PC. For a **small-domain subtractive
GCD** the WCET fits under the existing 256 cap:
- 4-bit inputs: `a+b ≤ 30` ⇒ tightest measure ≈ 31 ⇒ k_max ≈ 31; iter_len ≈ (subtract body ~6 +
  LOOP_OVERHEAD) ≈ 8 ⇒ **WCET ≈ 4 + 31·8 = 252 < 256 — FITS.**
- 8-bit inputs: `a+b ≤ 510` ⇒ k_max ≈ 511 ⇒ WCET ≫ 256 — needs widen-PC (the follow-on).

So **slice-1 = ship the first real variable-T loop (small subtractive GCD) that fits TODAY**, proving
the Trace + park machinery carries data-dependent T on the real machine, with **zero energy tax** — and
revealing exactly what blocks scaling (which motivates the widen-PC slice with a *concrete* need).

## 1. Slice-1 candidate: small subtractive GCD (the design-review resolves the exact domain)

Subtractive Euclid: `while a ≠ b { if a > b { a −= b } else { b −= a } }`, result = a (= gcd). It is a
`Trace` with a data-dependent guard (`a ≠ b`) and a well-founded measure (`a + b` strictly decreases).
Variable T: gcd(15,15) halts in 0 iters; gcd(15,1) takes 14 — T ranges widely over the domain, so the
**park tax is genuinely input-dependent** (the whole point).

**The design-review must determine:**
1. Does a small subtractive GCD `.thse` (pick the domain — 4-bit? Nat@k? Flat(4)?) **elaborate to a
   Trace and COMPILE today** (fit ≤ 256), or does it hit a wall? If a wall — is it (a) the measure's
   k_max being LOOSE (|u|+1 with u too big) → needs a k_max-tightening (typed bound) mechanism, (b) the
   carrier leaves > MAX_LEAVES_LOOP, or (c) genuinely WCET > 256 (needs widen-PC even at 4-bit)?
2. If it needs only a SMALL enabling change (e.g. a tighter measure type / k_max, or a slightly wider
   input encoding), what is the MINIMAL one? Is that change test/type-level or does it touch
   lower_trace (highest bar)?
3. POC it: a small subtractive GCD running on the **real `system_step`** (`run_on_cpu == eval`, L19)
   over its whole input domain, asserting `gcd` correct AND showing **T < B with a park-tax histogram**
   that VARIES with the input (the variable-T signature). Bound the domain (4-bit = 256 pairs — trivial).

## 2. What slice-1 is NOT (deferred, each its own slice + review)
- **widen-PC** (u8 → wider): the datapath change to raise the 256 ceiling so 8-bit GCD / longer loops
  fit. Permanent ISA-growth energy tax (quantify: step_j delta per PC bit). This is the *second* slice,
  motivated by slice-1's concrete "8-bit GCD needs WCET N > 256" finding. Blast radius (Explore §G):
  `types.rs:242` PC field, `step.rs` pc_inc/pc_pred/fetch (decoder netlist → step_j golden re-pin),
  `compile.rs:2688/3209` caps, `delta_u` modulus, dead zone, all CPU-oracle + energy goldens.
- **k_max-tightening as a general mechanism** (typed iteration bounds beyond |u|+1) — may be partly
  needed for slice-1 (if the measure is loose); the general form is its own slice.
- **bounded Collatz** (TP-2) — a second variable-T program, after GCD.

## 3. Discipline (this is the highest bar if it touches lowering)
If slice-1 ships a GCD `.thse` + test and needs NO src change (pure conformance program, like the LFSR
TP-3), it is TEST/CONFORMANCE-ONLY (lighter bar, L19 authority). If it needs a lower_trace / measure /
k_max change, it is a **control-flow/lowering change → mandatory fresh-context adversarial impl-review +
a reproducing behavioral test (real worktree + worker + gate round-trip) before merge** (CLAUDE.md).
The design-review determines which, and that sets the bar.

## 4. Questions for the Fable design-review-with-POC
1. Does a small subtractive GCD compile + run on real `system_step` TODAY? At what domain (4-bit /
   Nat@k)? Show the WCET B, and T-vs-input (the park-tax histogram — variable T is the deliverable).
2. If it hits a wall at the smallest interesting domain, what's the MINIMAL enabling change, and is it
   lowering-touching (highest bar) or test/type-only?
3. Is the measure `a+b` (or what?) — and is its k_max tight enough, or is |u|+1 loose here?
4. Carrier leaves for GCD vs MAX_LEAVES_LOOP=4 — does `(a,b)` + tag fit?
5. Confirm the honest framing: slice-1 = first variable-T loop (no widen-PC, no energy tax); widen-PC is
   the follow-on to scale. If that framing is wrong (GCD needs widen-PC even at 4-bit), say so with the
   WCET numbers and recommend widen-PC as slice-1 instead (with the energy-tax estimate).
