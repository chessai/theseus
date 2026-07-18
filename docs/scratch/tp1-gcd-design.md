# TP-1 — reversible GCD formulation (and what it reveals)

> **REVIEW OUTCOME (Fable, `docs/reviews/tp1-gcd-review.md`) — OVERTURNED IN PART.**
> - **GCD ⛔ breaks today: SOUND, and *overdetermined*.** Not just the §1 branch argument —
>   a stronger, formulation-independent theorem (**ADR-0007 TH-1b, trajectory
>   disjointness**): a bijective Trace body is backward-deterministic ⇒ distinct inputs'
>   trajectories are vertex-disjoint in `u` ⇒ `|u| ≥ |a|` for always-iterating loops ⇒ GCD's
>   ~`2^{2w}` pairs force `≥ 2w` payload leaves for *any* formulation. GCD breaks regardless
>   of the branch.
> - **§5/§6 TP-1a is OVERTURNED — it does not exist.** "Iterate σ until `x==1`, carrier=`x`"
>   is **not a bijection** (every input converges on the same exit state `Left(1)`, so the
>   body can't be injective — Trace of a bijection must be injective). Making it real needs a
>   *counter* carrier ⇒ over the leaf cap **and** the WCET wall. **There is no fitting
>   variable-`T` Trace showcase today** — only a "bimodal toy" (disjoint paths, `Σ(T−1)≤|u|`).
> - **§4 law is wrong both ways (F3);** the correct law is below. **M-D.e alone will NOT
>   unblock GCD (F4):** `k_max=|u|+1` from *type cardinality* ⇒ a 12-leaf carrier gives
>   `k_max≈4097`, WCET≈10⁵. GCD needs **M-D.e (spill) + widen-PC + a `k_max`-tightening
>   mechanism** (typed iteration bound). See ADR-0007 TH-1b.
> - **§2 GCD typing is loose (F5):** `|a|≠|b|` and `hist«branch` need padding/junk arms
>   before the rejection-witness `.thse` is writable.
>
> The sections below are the *original reasoning* (preserved — the branch argument in §1 is
> verified correct, just not the tightest proof). Read the review + ADR-0007 TH-1b for the
> corrected picture.

**Goal:** formulate GCD as a total **bijection** compilable to a TISC-1 `Trace`, as the
M-E park-tax showcase. **Outcome:** the reversible formulation is *harder* than a re-tier —
it exposes a general sizing theorem (ADR-0007 TH-1b) and a milestone *cluster* (spill +
widen-PC + typed iteration bounds), and shows no non-trivial variable-`T` loop fits today.

## 1. Why GCD is not a bijection, and the Bennett tax

`GCD : (a,b) ↦ gcd` is massively non-injective. To compile it we need a bijection
`(a,b) ↦ (gcd, witness)` where `witness` restores the lost information. Subtractive Euclid
step (the small-op form):

```
loop over (a,b), b>0:   if a > b: a -= b ;  else if a < b: swap ;  else (a==b): exit, gcd=a
```

**The body is NOT a bijection on `(a,b)` by itself.** Take the step `if a>b: a-=b ; if a<b:
b-=a`. Then `(5,3) →[a>b] (2,3)` and `(2,5) →[a<b] (2,3)` — **two inputs, one output**
(verified: an `a>b` predecessor `(x+b,b)` and an `a<b` predecessor `(a,y+a)` can collide).
So the step is 2-to-1 at points reachable both ways; to invert we **must record the branch
bit** (`a>b` vs `a<b`) each iteration. This is the **Bennett tax**: an irreversible
internal branch forces the loop to accumulate a **history** of decisions in the carrier.

> **The `Trace` come-from does NOT pay this for you.** The come-from (M-D.d) makes the
> *back-edge* reversible — it records loop entry/exit. It does **not** record a branch
> *inside* the body; the body must be a self-contained bijection. An internal data-dependent
> branch (`if a>b`) computes a bit from the state that cannot be uncomputed after the
> subtraction (re-comparing gives a different answer) → it must be stored. History.

## 2. The carrier, and why GCD breaks today

Reversible subtractive Euclid as a `Trace`:
- **Carrier `u = (a, b, hist)`** where `hist` is a `K`-bit shift register of branch bits,
  `K = max iterations` (the finitization bound).
- **Body** (a tag-branch): entry `Right(A,B) ↦ Left(a=A,b=B,hist=0)`; step `Left(a,b,hist)
  ↦` either `Left(step(a,b), hist«branch)` (continue) or `Right(gcd, hist)` (exit on a==b).
- **Output `(gcd, hist)`** — a bijection with `|out| = |in|` (the history makes up the
  cardinality gap).

**Width:** even for tiny 3-bit inputs (`a,b ≤ 7`, `w=3`), worst-case `K ≈ 6` (e.g.
`gcd(7,1)`). Carrier leaves = `a(3) + b(3) + hist(6) = 12` payload leaves.

**The wall:** the `Trace` loop carrier is capped at **`MAX_LEAVES_LOOP = 3` payload
leaves** (compile.rs:335 — 4 dedicated aux regs `rc/rb/temp/ctrl` + the tag reg use 5 of 8
registers, leaving ≤3 register-resident payload leaves; loop aux are unpageable). **GCD's
12-leaf carrier exceeds this by 4×.** *No non-trivial GCD fits* — even 2-bit GCD
(`a(2)+b(2)+hist(3) = 7` leaves) is over. GCD is firmly **⛔ breaks today**.

## 3. The two milestones GCD names (the feedback-loop payoff)

1. **Loop-carrier spill** (NEW milestone — call it **M-D.e**): a `Trace` whose carrier
   exceeds 3 leaves must **page the carrier to scratch/memory between iterations** (M-C1
   spill, but *inside* a loop body), keeping only the 4 aux regs register-resident across
   the jump. This is real, non-trivial work (paging that survives the back-edge) and is the
   gate for *every* real loop program. **GCD is its customer.**
2. **History width / heap** — even spilled, `hist` grows with `K`; large `K` needs
   MEM_WORDS widening (M-D.c1) or heap (M-D.c2). Bennett garbage is the customer for the
   *data track* too.

## 4. The general law this surfaces

**A `Trace` fits today (≤3 leaves, no spill) iff its body is a pure bijection with NO
internal data-dependent branch** — so no Bennett history accumulates. Formally:

- **Branch-free body** (the only decision is the exit tag, which the come-from records) →
  carrier stays a single small state, **no history** → **fits**.
- **Internal-branch body** (GCD, Collatz: `if even/odd`, `if a>b`) → Bennett history →
  wide carrier → **needs loop-carrier spill (M-D.e)**.

**Corollary (the coupling):** the programs with the *best* M-E park-tax (variable
iteration count) driven by *data-dependent branching* are exactly the ones that need M-D.e.
Park-tax variance ⟺ internal branching ⟺ Bennett history ⟺ wide carrier ⟺ spill.
**Unless** the variable iteration count comes from a *branch-free* body with a
data-dependent *exit* — that fits today and still varies `T`.

## 5. The M-E showcase that fits — TP-1a "orbit length" (branch-free, variable T)

Iterate a **fixed reversible permutation** `σ` (a branch-free bijection) until a target:

- **`x := lfsr(x)` until `x == 1`** (a `w`-bit maximal LFSR; orbit = all `2^w−1` nonzero
  states, period `2^w−1`, `0` excluded). Carrier = `x` (**w leaves**, e.g. 3 → fits
  `MAX_LEAVES_LOOP=3`). Body = the LFSR step (a linear GF(2) bijection, **no internal
  branch**) with the come-from exiting when `x==1`.
- **`T` = the orbit position of the start `x`** — varies `1..2^w−1` across inputs →
  genuine **park-tax variance** (`(B−T)/B` swings), *without* any Bennett history.
- Bijective by construction (`σ` is a bijection; the come-from records `T`; output = the
  number of steps, or just `Unit` with `T` read from the trace for the energy story).

This is the honest M-E park-tax showcase — a real data-dependent-iteration program that
compiles **today**. It is TP-3 (LFSR) with a **data-dependent exit** instead of a fixed
`K`; fold it in as **TP-1a**.

## 6. Re-tiering (feedback into the test-program roadmap)
- **TP-1 GCD → ⛔ Tier-3**, customer for **M-D.e (loop-carrier spill)** + history width.
  (The reversible formulation is sound — §1–§2 — it just doesn't fit until M-D.e.)
- **TP-2 Collatz → ⛔ Tier-3** (same reason: `if even/odd` is an internal branch → history).
- **TP-1a orbit-length (LFSR-to-target) → 🎯 Tier-1**, the M-E park-tax showcase that fits.
- New main-roadmap node: **M-D.e loop-carrier spill** — the gate for real loop programs;
  GCD/Collatz/binary-search all wait on it (binary search *also* needs Trace-with-mem-body).

## 7. Acceptance
- **TP-1a (build now, after M-E):** `Trace(x, lfsr-step-until-1)` compiles → ROM →
  `run_on_cpu == eval` over the whole `w`-bit domain; M-E park-tax **histogram** across
  starting `x`; golden energy at a pinned `t_ramp`. `w=3` fits `MAX_LEAVES_LOOP=3`.
- **TP-1 GCD (Tier-3 customer):** write the `.thse`/iso; assert it is **rejected** by the
  `MAX_LEAVES_LOOP` guard (compile.rs) — a committed finding proving the customer for
  M-D.e. When M-D.e lands, flip to ✅ with the real-CPU oracle + park-tax histogram.

## 8. Open questions for the design reviewer
1. Is the Bennett-history argument (§1) airtight — is there a GCD step formulation that is a
   bijection on `(a,b)` *without* stored history (e.g. keeping `a≥b` invariant so the only
   decision is a recoverable swap)? Prove none exists, or find one (which would let GCD fit).
2. Is `MAX_LEAVES_LOOP=3` the true blocker, or can the 4 aux be reduced (e.g. `temp`
   folded) to widen the register-resident carrier before spill is needed?
3. For TP-1a, does the come-from exit-on-`x==1` need any history, or is the branch-free
   body genuinely history-free (confirm the exit tag is the *only* accumulated bit and the
   come-from consumes it)? Is the whole-domain `run_on_cpu==eval` bounded enough for w=3?
4. Does M-D.e (loop-carrier spill) compose with the come-from (paging in/out across a
   backward jump while `rc/rb/temp/ctrl` stay resident) — or is that a hard reversibility
   problem of its own?
