# Reversible memory closure (M-C2)

**Claim.** This note collects and proves — or, where proof is not available,
honestly disproves or downgrades to conjecture — every result discovered during
the M-C2 memory-bus investigation: the scoping adversarial review
(`adr/0045-mc2-memory-bus.md` §0) and the 2-agent design spike (§0.5). Those
sections *state* the results; this note is the rigor. The headline theorems:
per-ring closure of a CPU with genuine external memory is **impossible** — an
autonomy/information-flow obstruction, not a thermodynamic one (§2); the
**composite** CPU ⊗ bus ⊗ memory closes under the same zero-garbage invariant
the ring already certifies, with the bus admitted as a composite ancilla (§3);
no core combinator can manufacture the zeroed bus word (§4); the value oracle is
provably blind to Option-B degeneracy (§5); and the external bus is
energy-equivalent to resident memory — it buys area and a seam, never joules
(§6). Everything is grounded in the shipped machinery:
`crates/theseus-tisc/src/ring_run.rs` (the closure certificate), ADR-0033 (the
ring), ADR-0040 (the R1 cell and ancilla-cleanliness), ADR-0029 (exchange
discipline, Options A/B/C), ADR-0044 (M-C1 zero-source). Results that rest on
analogy are **marked as conjectures with explicit proof obligations** — see the
closing ledger.

## Setup and notation

A lowered iso is a netlist whose ports split into a **data section** and an
**ancilla section**: `lower_iso_with_ancillas` returns the netlist together with
the ancilla init vector (`ring_run.rs:66-82`), and `n_data = inputs −
ancillas` (`ring_run.rs:75`). Throughout:

- `States` is the finite set of TISC-1 `State` values;
  `enc : States → B^d` is the injective bit encoding (`bits_of`,
  `ring_run.rs:110-112`), `d` the data width in lines.
- `ι ∈ B^a` is the ancilla init vector (`StepRing::ancilla_init`).
- `N : B^{d+a} → B^{d+a}` is the function computed by one pass of the
  compact-balanced body (`CompactBalanced::simulate`,
  `ring_run.rs:117-118`). Every admitted cell (K0–K3, R1) is a permutation of
  the values on its lines (ADR-0027; ADR-0040 D1–D2), the body is a DAG of such
  cells plus delay edges (ADR-0033 D2, D4), so **N is a bijection**. We take
  this as standing hypothesis (N-bij); it is the netlist-level reversibility the
  validator enforces.
- `step : States → States` is the value-level TISC-1 step; the lowering-
  correctness fact `π_data(N(enc s, ι)) = enc(step s)` is exactly what the
  differential oracle `run_open(s, R) == eval(step)^R (s)` certifies
  (`ring_run.rs:5-8`, ADR-0033 Consequences, `docs/src/adr/0033-ring-machine.md:196-198`).

A **revolution** is one application of N with the wrap feeding outputs back to
inputs (ADR-0033 D2: the wrap edge lives outside the DAG,
`0033-ring-machine.md:53-61`). The wrap-to-wrap map — N itself — is the only
object the closure predicate constrains; this locality is load-bearing in
Lemma 3.2.

## 1. The zero-garbage closure invariant (ZG)

### Definition 1.1 (ZG)

Let `A = enc(States) × {ι} ⊆ B^d × B^a` (the **rest slice**). N satisfies
**(ZG)** with respect to `(step, ι)` iff

- **(ZG-i) slice invariance:** `N(A) ⊆ A` (equivalently `N(A) = A`, since N is
  injective and A finite);
- **(ZG-ii) restriction identity:** `N|_A = (enc ∘ step ∘ enc⁻¹) × id_{{ι}}`.

(ZG) is precisely "the lowered step is a bijection whose fixed-init slice is
invariant and on which it *is* the step" — the invariant ADR-0045 §0.5 names
("`States × {ι}` is an invariant slice of the lowered bijection,"
`0045-mc2-memory-bus.md:83-85`).

### Proposition 1.2 (the pointwise certificate)

The following are equivalent:

1. N satisfies (ZG).
2. `ancilla_mismatches(s) = ∅` for every `s ∈ States` **and**
   `run_open(s, 1) = step(s)` for every `s ∈ States`.

*Proof.* `ancilla_mismatches(s)` computes `N(enc s, ι)` and lists the ancilla
coordinates that differ from ι (`ring_run.rs:160-176`); emptiness for all s says
exactly `π_anc ∘ N |_A ≡ ι`. `run_open(s, 1)` decodes `π_data(N(enc s, ι))`
(`ring_run.rs:135-156` with `revolutions = 1`), so `run_open(·,1) = step` says
exactly `π_data ∘ N|_A = enc ∘ step ∘ enc⁻¹`. Together these are (ZG-ii) plus
`N(A) ⊆ enc(States) × {ι}`: the data part lands in `enc(States)` because
`step(s) ∈ States`. That is (ZG-i). Conversely (ZG) makes both computations come
out clean by direct unfolding. ∎

`ancillas_restored` is the emptiness check (`ring_run.rs:180-182`). Note the
quantifier: the shipped check is **per start state**; the TISC-1 domain (2^160)
is sampled, not enumerated (ADR-0040 update, `0040-rail-role-swap-cell.md:29-35`).
The `debug_assert!` inside `run` (`ring_run.rs:102-107`) closes the gap
operationally: every state a real run actually starts from is checked.

### Proposition 1.3 (the trajectory certificate is implied — and strictly weaker)

If N satisfies (ZG) then `run(s, R) = run_open(s, R)` for all `s ∈ States`,
`R ≥ 0`. The converse is **false**.

*Proof.* Forward: induction on R. `run` feeds ancilla outputs back
(`ring_run.rs:114-121`); by (ZG-i) the fed-back vector equals ι after every
revolution, so the two loops compute identical sequences of inputs.

Converse — counterexample. Take `d = a = 1`, `ι = 0`,
`N(x, z) = (x, ¬z)`. N is a bijection; `run` and `run_open` both return x
unchanged for every R (the data output never reads the ancilla line), yet
`ancilla_mismatches = {0}` on every input: (ZG-i) fails. So
`run == run_open` alone does not certify (ZG). ∎

This is why `ring_run.rs` layers the certificates: `run == run_open` is the
*consequence* used as a regression oracle ("that equivalence is what certifies
the closure" is shorthand for the joint check, `ring_run.rs:22-26, 131-134`),
while the hard assertion inside `run` is the pointwise fact
(`ring_run.rs:102-107`). A garbage line that never re-enters the data path is
invisible to trajectories but is still garbage — still a line the physical wrap
would feed back dirty.

### Lemma 1.4 (compositional cleanliness — the R1 schema)

Let `N = G_k ∘ ⋯ ∘ G_1` on `B^d × B^{a_1} × ⋯ × B^{a_m}` where each gadget
`G_j` (i) acts as the identity on every ancilla block other than its own block
`b(j)`, and (ii) is **whole-domain clean** on its block: for *every* `x ∈ B^d`,
`G_j(x, ι_{b(j)}) = (g_j(x), ι_{b(j)})` for some bijection `g_j` of `B^d`. Then
N satisfies (ZG-i) with `ι = (ι_1, …, ι_m)`, and `N|_A = (g_k ∘ ⋯ ∘ g_1) × id`.

*Proof.* Induction on j. After `G_1`, block b(1) is back at init (cleanliness at
input x), all other blocks untouched (identity off-block), and the data value is
`g_1(x)`. The induction step applies `G_{j+1}` at data value
`g_j(⋯g_1(x)) ∈ B^d` — an *arbitrary* element of `B^d`, which is exactly why
hypothesis (ii) must quantify over the whole domain: cleanliness at a sampled
subset of inputs does not survive composition, because the intermediate value is
whatever the prefix produced. Chaining gives the claim. ∎

**Grounding.** This is the schema under which ADR-0040 phase C makes the step
ancilla-clean per gadget: CNOT = one `R1` with zero ancillas; Toffoli =
`K1(c₁,c₂,0) → R1 → replayed K1` with ancilla out `= 0` on **all eight inputs**
(`0040-rail-role-swap-cell.md:199-205`); `k ≥ 3` = AND-chain, `R1` on the head,
chain replayed (`0040-rail-role-swap-cell.md:7-16`). The whole-domain quantifier
in (ii) is exactly what the tests check
(`crates/theseus-netlist/tests/ancilla_clean.rs`,
`tests/rail_role_swap.rs` — "zero-residue checks over whole domains,"
`0040-rail-role-swap-cell.md:17-23`). The measured composite fact — `step`'s
468 ancilla lines all restored (`0033-ring-machine.md:163-172`,
`0040-rail-role-swap-cell.md:29-44`) — is this lemma instantiated over the real
gadget decomposition, then spot-verified pointwise by Proposition 1.2's
certificate. Where a gadget is *not* clean (trace tag duplication, iter/fold
fan-outs), the Bennett `ClosedStepRing` fallback restores forward ancillas
data-independently and exports the fixed `2·w` boundary instead
(`ring_run.rs:28-45, 195-344`; the parity law forces this floor for K-only
vocabularies, `0040-rail-role-swap-cell.md:48-64`).

## 2. Per-ring closure is impossible with external memory

The spike's Result 1 (`0045-mc2-memory-bus.md:66-74`). We prove it as stated,
and classify the obstruction.

### Definitions 2.1

Let `C` be the set of CPU states, `M` the set of memory states, and
`S : C × M → C × M` the one-revolution system map. Write `π_C`, `π_M` for the
projections.

- **(Closed)** The CPU ring is *closed* iff its revolution map is a function of
  its own ports alone: there exists `F : C → C` with
  `π_C(S(c, m)) = F(c)` for all `(c, m)`. More generally, closed *under an
  autonomous re-supply schedule* iff there exist `F_r : C → C` (r the revolution
  index) with `π_C(S applied at revolution r) = F_r(c)` — this covers every wrap
  discipline the codebase has: TRUE recirculation (inputs are functions of the
  ring's own previous outputs, `ring_run.rs:114-121`), fresh constant re-supply
  (`run_open`, `ring_run.rs:145-150`), and the Bennett boundary's fixed `(1,0)`
  pairs (`ring_run.rs:264-279`). In each, the r-fold composite is a function of
  `(c, r)` alone; nothing fed to the ring depends on `M`.
- **(Responsive)** S is *memory-responsive at* `(c; m, m')` iff `m ≠ m'` and
  `π_C(S(c, m)) ≠ π_C(S(c, m'))`.
- **(External)** Memory is *genuinely external* iff S is memory-responsive at
  some **reachable** configuration — reachable meaning both `(c, m)` and
  `(c, m')` occur in runs of the system from intended initial states.

### Lemma 2.2 (the exchange discipline manufactures the witness)

Under the exchange ISA (ADR-0029: memory access is swap, `word ↔ mem[addr]`,
`docs/src/adr/0029-memory.md:10-14`; ADR-0045 §2.2,
`0045-mc2-memory-bus.md:161-167`), any program containing
`STORE r, addr; …; LOAD r, addr` makes memory genuinely external.

*Proof.* Run the program from two initial states differing only in register r
(`v ≠ v'`), with `mem[addr] = 0` in both. The STORE swaps: after it, register r
holds 0 in *both* runs, and `mem[addr]` holds v resp. v′. If the intervening
code does not touch r or addr, the two runs reach the *same* CPU state `c` with
*different* memories `m ≠ m'` — the CPU "swapped it out and moved on"
(`0045-mc2-memory-bus.md:69-71`): the value is no longer recoverable from `c`.
The LOAD then swaps `mem[addr]` back into r, so
`π_C(S(c,m))` and `π_C(S(c,m'))` differ at register r. Responsive at a
reachable pair. ∎

### Theorem 2.3 (impossibility of per-ring closure)

If the CPU ring is closed — even under an arbitrary autonomous re-supply
schedule — then S is nowhere memory-responsive. Contrapositively: **a system
with genuinely external memory admits no closed CPU ring.** Hence
`genuine external memory ⟺ the CPU ring is an open system` (with Corollary 2.4
supplying the ⟸ reading).

*Proof.* Immediate from the definitions, which is the point: if
`π_C(S(c,m)) = F_r(c)`, then for all `m, m'`:
`π_C(S(c,m)) = F_r(c) = π_C(S(c,m'))` — no responsiveness, anywhere, at any
revolution. By Lemma 2.2, an exchange-ISA machine with a live LOAD is responsive
at a reachable pair; contradiction. No choice of wrap, re-supply schedule, or
phase discipline evades this, because all of them keep the ring's inputs
independent of `M` — that independence *is* Def 2.1, and any mechanism that
breaks it (carries `mem[addr]` onto a CPU port) is, by that very fact, an open
port. "Stop looking for a per-ring fix" (`0045-mc2-memory-bus.md:72-73`) is
therefore a theorem, not an engineering judgment. ∎

### Corollary 2.4 (degenerate converse)

If S is a bijection and nowhere memory-responsive, then `π_C ∘ S` factors as a
**bijection** `F : C → C`, and the CPU ring closes solo around F.

*Proof.* Non-responsiveness gives `S(c,m) = (F(c), h(c,m))` for some F and h.
S injective forces, for each fixed image column, injectivity in the fiber; count:
`S` maps the `|M|` elements `{c} × M` into `{F(c)} × M`, and distinct c, c′ with
`F(c) = F(c′)` would force `2|M|` distinct images inside a set of size `|M|`.
So F is injective on a finite set, hence a bijection. ∎

So the dichotomy is exact: either the memory is CPU-invisible (Option-A-with-
indirection — the degeneracy trap of ADR-0029's Consequences,
`0029-memory.md:32-43`, and ADR-0045 §1) and per-ring closure survives trivially,
or the memory is real and per-ring closure is dead.

### Remark 2.5 (classification of the obstruction)

Nothing in Theorem 2.3 uses reversibility: S was an arbitrary function; no bit
is erased; entropy export is zero (S bijective throughout the intended system).
Contrast the parity obstruction (`0040-rail-role-swap-cell.md:48-64`): that was
a *conservation law* of a cell vocabulary, voided by admitting one new cell
(R1, ADR-0040 D3). This obstruction is *definitional*: closure is an
**autonomy** property (the revolution map factors through the ring's own
ports), and external memory is precisely an information-flow edge from M into
C. No cell, wrap, or schedule voids it — the fix is to move the closure
boundary, which is §3.

## 3. Composite closure (ZG-2)

The spike's Result 2 (`0045-mc2-memory-bus.md:76-101`): the composite
CPU ⊗ bus ⊗ memory closes, with the bus as a composite ancilla.

### Setup

The composite carrier is `B^{d_C} × B^{d_M} × B^b × B^{a}`:

- `B^{d_C}`: CPU data lines; `B^{d_M}`: memory-bank lines
  (**data**, jointly encoding `(CpuState, MemState)`);
- `B^b`: the resident `BusMsg` lines — `enable : Bit`, `addr : AddrW`,
  `word : W8` (value-level 17 bits at `AddrW = 8`, dual-rail 2× lines;
  `0045-mc2-memory-bus.md:153-159`) — rest value `0_b`;
- `B^a`: CPU lowering ancillas ⊗ exchange-tree scratch, init ι.

Extended init: `ι₂ = (0_b, ι)`. Rest slice:
`A₂ = enc(C × M) × {0_b} × {ι}`. Let `N₂` be the composite revolution
bijection and `system_step : C × M → C × M` the value-level system map.

### Definition 3.1 (ZG-2)

N₂ satisfies **(ZG-2)** iff Definition 1.1 holds verbatim with data
`= (CpuState, MemState)`, init `= ι₂`: A₂ is N₂-invariant and
`N₂|_{A₂} = enc ∘ system_step ∘ enc⁻¹ × id`. This is the "generalizes verbatim"
claim of `0045-mc2-memory-bus.md:82-86`, now a definition rather than a slogan.

### Lemma 3.2 (a resident, boundary-constant, mid-revolution data-dependent factor is an admissible ancilla)

Let `N = G_k ∘ ⋯ ∘ G_1` be any factorization of a bijection N on
`X × B^b × Z` (X data, `B^b` the candidate factor, Z other ancillas), and
suppose only the *boundary* condition: for every `x ∈ X`,
`N(x, 0_b, ι_Z) ∈ X × {0_b} × {ι_Z}`. Then the factor `B^b` satisfies the
ancilla clause of (ZG) — **irrespective of the values the factor takes at the
intermediate points** `(G_j ∘ ⋯ ∘ G_1)(x, 0_b, ι_Z)`, which may depend on x
arbitrarily.

*Proof.* (ZG) (Definitions 1.1/3.1) is a predicate on the single bijection N —
the wrap-to-wrap revolution map — and quantifies only over values at the wrap
cut. The factorization `G_k ∘ ⋯ ∘ G_1` does not occur in the predicate: two
factorizations of the same N satisfy exactly the same (ZG). Hence the
hypothesis (boundary restoration) is literally the conclusion (ancilla clause),
and mid-composite behavior is unconstrained. ∎

The lemma is nearly definitional — *deliberately so*. Its content is that the
existing certificate already admits such factors, and the codebase already
exercises the permission: in the Toffoli sandwich `K1 → R1 → K1`, the scratch
pair holds `c₁ ∧ c₂` — data-dependent — strictly between the two K1s, and is 0
at both ends on all eight inputs (`0040-rail-role-swap-cell.md:199-205`); every
Bennett compute–copy–uncompute has the same shape (`ring_run.rs:28-45`). "A
resident factor that is boundary-constant but mid-revolution data-dependent is
exactly what the existing certificate already calls an ancilla"
(`0045-mc2-memory-bus.md:80-83`) — proved. What is *not* automatic is that the
bus satisfies the boundary condition, and at which cut. That is Lemma 3.3 and
Proposition 3.4.

### Lemma 3.3 (the conjugated exchange composes reversibly and restores the bus)

Specify the transaction protocol (the emit/absorb conjugation — the
fetch/unfetch proof shape, `0029-memory.md:15-21`, applied to the bus). The
revolution factors as `N₂ = U ∘ A ∘ X ∘ E ∘ V` where, writing `p(c)` for the
decoded is-memory-op predicate and `(r, addr)` for the register/address fields
of the fetched instruction (which **persists in CpuState** across the
transaction — unfetch has not yet run):

- `V` (fetch, decode/execute prefix) and `U` (unfetch, PC update): identity on
  the Mem, bus, and tree-scratch factors;
- `E` (emit), identity on Mem and tree scratch: if `p` — CNOT-copy `addr` onto
  the zeroed bus addr lines, CNOT `enable` to 1, swap `reg[r] ↔ bus.word`; if
  `¬p` — identity;
- `X` (addressed exchange), identity on Cpu: if `enable` — swap
  `bus.word ↔ mem[addr]` through the 1-of-N Fredkin tree; else identity;
- `A` (absorb), the conjugate of E keyed on the *same* persistent instruction
  fields: if `p` — swap `bus.word ↔ reg[r]`, then un-CNOT `addr` and `enable`
  against the instruction fields.

Then: **(a)** each factor is a bijection on the full composite carrier, and the
factors' active port sets are pairwise disjoint outside the shared bus lines
(port-disjointness), so `N₂` is a bijection; **(b)** on the rest slice, the
sandwich `A ∘ X ∘ E` restores the bus to `0_b` and (conditional on Conjecture
C1 below) the tree scratch to init, and effects exactly
`reg[r] ↔ mem[addr]` when `p`, identity when `¬p`.

*Proof.* (a) Controlled swaps and CNOTs are involutive bijections on the whole
space — no zero *assumption* is needed for bijectivity; the protocol merely
*uses* them on the zero slice. Compositions of bijections are bijections.
Port-disjointness (E touches Cpu ⊗ bus; X touches Mem ⊗ bus ⊗ tree; U, V touch
Cpu only) is what lets each factor be lifted by identity to the composite
carrier and is the algebraic content of "no MemState port reaches a CPU-phase
cell" (§5).

(b) Trace the two cases at boundary values (bus `= 0_b`):

*Case `p` (memory op).* After E: `bus.addr = addr`, `enable = 1`,
`bus.word = old reg[r]`, `reg[r] = 0` (the swap is against the rest-zero
`bus.word`: reg[r]'s value moves out, the bus's 0 moves in).
After X: `bus.word = old mem[addr]`, `mem[addr] = old reg[r]`. After A: the
swap puts `old mem[addr]` into `reg[r]` (currently 0) and 0 back into
`bus.word`; then un-CNOT of `addr` and `enable` against the *unchanged*
instruction fields returns those lines to 0 (CNOT against the same value is the
identity round-trip — the fetch/unfetch argument verbatim,
`0029-memory.md:15-21`). Net: `reg[r] ↔ mem[addr]`, bus `= 0_b`. ✱

*Case `¬p`.* E, X, A are each literally the identity on the bus (all three are
controlled on `p`/`enable`, which is 0 and stays 0). No residue. This
discharges ADR-0045 §7's "reversibility of the exchange under `enable = 0`"
risk (`0045-mc2-memory-bus.md:271-272`) — *for this protocol*: the resolution is
that emit/absorb are controlled operations keyed on persistent decode bits, so
the identity path is the identity by construction, not by cancellation. ∎

✱ The tree-scratch half of (b) — that X's internal 1-of-N steering ancillas
return to init on the *whole* domain — is Conjecture C1, not proven here; see
§8. At `N = 8` the same shape is the register crossbar: `select_fields`
(`crates/theseus-tisc/src/decoder.rs:187-197`) is a tree of controlled word
swaps whose data-dependent permutation is exactly undone by composing with its
adjoint (`decoder.rs:199-204`), whole-domain tested against `crossbar_model`
(`decoder.rs:213`). The analogy is strong; it is still an analogy.

### Proposition 3.4 (the quiescent cut is necessary — seam rotation)

If, for some input on the rest slice, some bus line is non-rest at the wrap cut
(a transaction straddles the wrap), then (ZG-2)(i) fails at that input;
moreover TRUE recirculation then *corrupts semantics*, not just the
certificate.

*Proof.* Failure of (ZG-2)(i) is immediate: `N₂(enc(c,m), 0_b, ι)` has a bus
coordinate `≠ 0`, so the image leaves A₂ — the composite `ancilla_mismatches`
is nonempty at that input (Proposition 1.2 transposed). For corruption: under
TRUE recirculation the fed-back bus value is the next revolution's "rest"
value; the next transaction's emit CNOTs `addr` onto now-nonzero addr lines,
producing `addr ⊕ residue` — the exchange targets the wrong cell, and
`run ≠ run_open ≠ eval(system_step)^R`. ∎

Hence the wrap cut must fall where the bus is provably at rest for *every*
input — i.e. between A and the next revolution's E. Since a ring's revolution
map from cut t is the conjugate `N_t = W ∘ N₀ ∘ W⁻¹` (W the prefix up to t),
and (ZG) is **not** conjugation-invariant (the rest slice is not W-stable in
general), choosing the cut is real design content, not bookkeeping. This is
precisely the spike's **seam rotation** — rotate `step`'s phase order so
emit/absorb sit at depth 0/D, keeping the body a single ordinary netlist with
ports only at the boundary (`0045-mc2-memory-bus.md:91-96`). That such a
rotation is *realizable* for the actual TISC-1 phase order is an engineering
claim, listed as an obligation in §8.

### Theorem 3.5 (composite closure)

Assume:

- **(H1)** the CPU-phase factors V, U are whole-domain ancilla-clean in the
  sense of Lemma 1.4 (grounded: the R1 gadget results and the measured
  `ancillas_restored` of the current step, ADR-0040 phase C,
  `0033-ring-machine.md:163-172`);
- **(H2) = Conjecture C1** the 1-of-N exchange tree's lowering is whole-domain
  ancilla-clean, N ∈ {2, 4, 8};
- **(H3)** the seam rotation of Proposition 3.4 (no transaction straddles the
  wrap; 1:1 period-locked sequencer so exactly one transaction window per
  revolution, `0045-mc2-memory-bus.md:96-97`);
- **(H4)** the transaction protocol implemented is that of Lemma 3.3.

Then N₂ satisfies **(ZG-2)**: `A₂ = enc(C × M) × {ι₂}` is invariant and
`N₂|_{A₂} = system_step × id`, with data `(CpuState, MemState)` and
`ι₂` the zeroed bus lines plus tree scratch plus CPU ancilla inits.

*Proof.* Factor `N₂ = U ∘ A ∘ X ∘ E ∘ V` (H4). Ancilla blocks: CPU ancillas
(touched by V, U and by E/A's controlled logic), bus lines (touched by E, X, A),
tree scratch (touched by X). By H1 and Lemma 1.4, the CPU-ancilla block returns
to ι across the composite for every data value entering each factor — the
whole-domain quantifier absorbs the data-dependence of intermediate values. By
Lemma 3.3(b) (using H2 for the tree block and H3 to place the cut at the rest
point), the bus and tree blocks return to `(0_b, ι_T)`. So
`N₂(A₂) ⊆ enc-image × {ι₂}`; the data action is `reg[r] ↔ mem[addr]`
composed with the CPU phases, i.e. `system_step` by construction, so the image
lies in `enc(C × M) × {ι₂}` — (ZG-2)(i) — and the restriction is
`system_step × id` — (ZG-2)(ii). Lemma 3.2 says this is all that is required of
the bus factor: its mid-revolution data-dependence (it carries `reg[r]`, then
`mem[addr]`) is invisible to the predicate. ∎

**Status: conditional theorem.** Modulo C1 (analogy, must be demonstrated
whole-domain at N ∈ {2,4,8}) and H3 (realizability). The certificate to run is
the generalized one:
`restored_ok ∧ run == run_open ∧ run == eval(system_step)^R` **plus the
structural line audit of §5** (`0045-mc2-memory-bus.md:96-98`) — the Stage-1
single-body composite ring reusing `StepRing` verbatim is exactly the
experiment that discharges it end-to-end (`0045-mc2-memory-bus.md:98-101`).

Note how Theorem 3.5 and Theorem 2.3 fit: nothing in §2 is contradicted. The
*CPU* ring remains open (the bus lines cross its boundary); it is the composite
that closes — closure moved to the boundary that encloses all the information
flow. That is the entire design.

## 4. Disproof: no combinator manufactures a zeroed BusMsg

Original §3.1 wrote `introduce BusMsg (uniti)`
(`0045-mc2-memory-bus.md:197-214`). The review called it unwritable
(`0045-mc2-memory-bus.md:27-31`). Proof:

### Proposition 4.1

There is no core term `t : a ↔ a × BusMsg` (nor any type-isomorphic variant)
whose denotation is `ζ : x ↦ (x, 0…0)`, for any inhabited multi-bit `BusMsg`
(`|BusMsg| = 2^k`, `k ≥ 1`). More generally, no core term denotes a map whose
codomain is strictly larger than its domain.

*Proof.* Every well-typed core term denotes a **total bijection** between the
denotations of its domain and codomain types — TH-4a for clause isos
(`theory/coverage-bijectivity.md`), the structural formers by induction (each
is a bijection or a bijection-preserving combinator), Trace by TH-1
(`theory/finite-trace-totality.md`). A bijection between finite sets forces
`|dom| = |cod|`. But `|a × BusMsg| = |a| · 2^k > |a|` for `k ≥ 1`. No such
term exists — regardless of combinator ingenuity. Pointwise: ζ is injective
but not surjective (its image is the measure-`2^{-k}` slice `a × {0}`); its
would-be inverse must erase k bits, which is exactly what reversibility bans
and what the linear type discipline (no weakening) refuses to type. ∎

What the algebra actually provides — and why it does not help:
`UnitiProd(a) : a ↔ 1 × a` (`crates/theseus-core/src/typing.rs:189`,
`iso.rs:51`) adjoins the **unit type**, `|1 × a| = |a|` — it introduces zero
information and, in the lowering, zero lines. `UnitiSum(a) : a ↔ 0 + a`
(`typing.rs:169`, `iso.rs:38`) adjoins the **empty summand**,
`|0 + a| = |a|` — no inhabitant of the new arm ever exists. Neither is, nor
composes to, a known-zero k-bit word.

### Corollary 4.2 (BusMsg must be resident)

The zeroed bus word cannot be introduced by any iso; therefore it must be a
**resident factor of the compiled carrier whose rest value is established by
marshalling** — outside the iso algebra, at the same place all zeros originate:
M-C1's zero-source discipline (`adr/0044-mc-compiler-memory.md:103-114`,
marshalling `0044:137-144` — "canonical layout keeps unused scratch at 0") —
and *re*-established at every revolution boundary by the conjugation discipline
of Lemma 3.3. Zeros are conserved resources under the exchange discipline: they
are budgeted and moved, never minted (see Lemma 7.1 for the same principle one
layer up). This is exactly hypothesis (H4)/(i) of Theorem 3.5, now proved
necessary, not merely convenient. ∎

## 5. The value oracle is blind to degeneracy

§0 finding 3 (`0045-mc2-memory-bus.md:32-36`): the M-C2a value-level milestone
"proves the wrong thing." Precisely:

### Proposition 5.1 (extensional blindness)

Let `t_B` be the Option-B term (memory a distinct sub-iso composed through bus
factors) and `t_A` the term obtained by inlining/splicing `mem` into `step`
over the shared `(CpuState, MemState)` type (Option-A-with-indirection,
`0029-memory.md:32-43`). Then `⟦t_B⟧ = ⟦t_A⟧`, and consequently **no experiment
of the form "evaluate and compare outputs" — in particular the differential
oracle `eval(system_step)^k == reference` — distinguishes them.**

*Proof.* `eval` is compositional: `eval(Seq(f,g)) = eval(g) ∘ eval(f)`
(`crates/theseus-eval/src/interp.rs:48-51`), and likewise structurally for
every former — so `eval` factors through denotations. Inlining is a sequence of
congruence and associativity steps, each denotation-preserving; hence
`⟦t_B⟧ = ⟦t_A⟧` as functions on `C × M`. Any oracle whose verdict is a function
of input/output pairs of `eval` is constant on denotation classes; `t_A` and
`t_B` are in the same class. ∎

Non-degeneracy — "`step` and `mem` stay separate isos; MemState does not unroll
through the CPU body" — is an **intensional** property of the artifact
(term/netlist), and Proposition 5.1 shows the class of eval-based certificates
cannot express it. It follows that *any* sound non-degeneracy certificate must
be structural. This upgrades the review's finding from "the M-C2a oracle would
go green on a degenerate system" (true, and now proven inevitable) to a
completeness statement about where the witness must live.

### Proposition 5.2 (the structural witness exists and is decidable)

Define the **MemState-line audit** on the lowered composite: compute the set of
lines reachable (forward, through active cells) from the MemState input ports;
the audit passes iff no such line is an input of an active cell scheduled in a
CPU phase (the depth intervals of V and U). Then (a) the audit is decidable —
graph reachability on a finite DAG (the body is a DAG, ADR-0033 D2,
`0033-ring-machine.md:53-61`); (b) the intended construction passes by the
port-disjointness of Lemma 3.3(a) — Mem ports feed only X's cells; (c) an
inlined lowering in which memory words ride the CPU body's rails fails it, so
the audit separates the two implementations that Proposition 5.1 proved
eval-indistinguishable. ∎

The audit certifies **separability**, not correctness — correctness remains the
oracle's job (`run == eval(system_step)^R`). The two certificates are
complementary and neither subsumes the other; the generalized acceptance
criterion of `0045-mc2-memory-bus.md:96-98` conjoins them for exactly this
reason. The audit is also what makes Corollary 6.2(b) (CPU body invariant in N)
checkable rather than aspirational.

## 6. Energy equivalence: Option B ≡ Option A

The spike's Result 3 (`0045-mc2-memory-bus.md:103-112`). This is a theorem *of
the cost model*; we state the model as an axiom so the dependence is explicit.

### Axiom E1 (storage is motion)

In the 2LAL fabric, logic **is** a shift register: every fabric-resident token
advances exactly one buffer stage per clock tick; `Reg(n)` is an n-stage buffer
chain; there is no zero-motion hold (ADR-0026 C2.1,
`adr/0026-adiabatic-backend-model.md:13-15`; restated as "storage is motion,"
`0029-memory.md:22-24`). First-order energy per stage-transfer is a constant ε
per token; energy tracks work (transfers), not area (ADR-0033,
`0033-ring-machine.md:153-157`). Dual-rail keeps exactly one token per bit
(INV-TOKEN, ADR-0031; `0040-rail-role-swap-cell.md:114-121`).

E1 is a physical modeling assumption, empirically anchored by the
characterization DB (B5 battery, ADR-0040 D2); everything below is rigorous
*relative to it*, and Option C escapes precisely by violating its premise.

### Theorem 6.1 (topology invariance of holding cost)

Let a design keep N words of w bits fabric-resident throughout a revolution of
D ticks, in any layout: recirculating inside the step body's State rails
(Option A), circulating in a separate memory loop crossed by a bus (Option B),
or any hybrid. Then the stored words' tokens undergo exactly `N·w` stage-
transfers **per tick** — `N·w·D` per revolution — in every layout, and
first-order holding energy per revolution is `ε·N·w·D`, identical across
layouts. The external bus is not an energy win.

*Proof.* E1 is a per-token, per-tick statement: each of the `N·w` resident
tokens (one per bit, INV-TOKEN) makes exactly one stage-transfer per tick,
regardless of *which* stage its path visits next. Summing over tokens and
ticks gives `N·w·D` transfers per revolution; multiplying by ε gives the
energy. The layout is quantified over and never used: topology chooses the
*route*, never the *count*. Topology relocates tokens; it cannot halt them. ∎

Second-order caveats (per-cell ε variation, ramp-slope dependence
`E ∝ 1/t_ramp`, seam-buffer differences) are explicitly outside the first-order
model, exactly as ADR-0033 prices the ring vs. unroll
(`0033-ring-machine.md:153-157`).

### Corollary 6.2 (what the bus actually buys — area ≠ energy)

1. **Area (the D4 wall).** Area counts *materialized stages*, and stages are
   layout-dependent even though transfers are not. Option A threads `N·w` rails
   through the full step body: the D2 balancing discipline buffers every rail
   at every tick of the body depth `D_cpu`, so the stored words alone
   materialize `≈ N·w·D_cpu` buffer stages — the observed ~1.6M-K0 wall, 98% of
   all cells, at just `w·|State| = 320` rails (`0033-ring-machine.md:16-24`;
   D4, `0033-ring-machine.md:123-145`). Option B holds the same tokens in a
   dedicated loop: a circulating loop holding `N·w` tokens needs `≈ N·w`
   stages (one stage per resident token per tick) plus the `O(log N)` exchange
   tree and P-padding — the buffer wall shrinks from `N·w·D_cpu` to
   `N·w·O(depth of a shallow mem loop)`, the ~10²–10³× of
   `0045-mc2-memory-bus.md:106-108`. **Precisely: energy/tick ∝ resident
   tokens (topology-invariant, Theorem 6.1); area ∝ resident tokens × loop
   depth (topology-dependent).** Conflating them is the error the spike
   existed to prevent.
2. **A CPU body invariant in N.** With the §5 audit passing, no CPU-phase cell
   depends on N; the CPU body's cell count, depth, and energy line are
   constants of the ISA, and memory becomes its own ledger line — the
   measurable meter for Option C's wake condition (ADR-0029 C3.5,
   `0029-memory.md:28-31`; `0045-mc2-memory-bus.md:108-110`).
3. **The Option-C seam.** E1 quantifies over *fabric-resident-in-motion*
   tokens. A static adiabatic SRAM macro (Option C, `0029-memory.md:28-30`)
   holds charge without per-tick stage-transfers: its bits simply do not
   satisfy E1's premise, so Theorem 6.1 does not bind them — Option C is the
   only actual escape from the Ω(N·w)-per-tick holding cost, by changing the
   physics premise rather than the topology. The factorization of Lemma 3.3 —
   X is the *only* factor touching Mem — is the seam: replace X's loop+tree
   with the SRAM Prim and the CPU (and Theorem 3.5's proof for the CPU-side
   factors) is untouched. ∎

Hence the adopted recommendation is proof-backed: at `N ≤ 256` (the 8-bit
immediate cap, `0045-mc2-memory-bus.md:110-112`) resident memory is
energy-identical to the bus, and the bus is *only* warranted when the area wall
binds or C3.5 fires (`0045-mc2-memory-bus.md:114-123`). No energy is left on
the table by shipping resident memory first — that is Theorem 6.1.

## 7. Compiler layer: the zero-parking obligations (M-C1/M-C2)

Brief, but load-bearing: these are the compiler-side facts the composite
construction's marshalling-zeroed init rests on.

### Lemma 7.1 (zeros are conserved: the aux-zero-source lemma, ADR-0044 §2.4)

Under the exchange-only discipline every emitted instruction permutes the
multiset of cell contents (registers ⊎ scratch ⊎ memory words): `Xchg` is a
transposition (`0044-mc-compiler-memory.md:81-86`), arithmetic ops are
bijections on their operand cells, nothing copies (linearity) or erases
(reversibility — the cell-level shadow of Proposition 4.1). Therefore **a
register can be brought to 0 only by swapping in a cell that already holds 0.**
M-C1's construction is the canonical instance: canonical layout keeps unused
scratch at 0 (established by marshalling, `0044:137-144`), so
`Xchg r, s_free` zeroes r *and* parks r's leaf, consuming one free-zero cell
per aux register. Branching needs two aux registers, hence two free-zero
slots, hence the honest ceiling 12 = 14 − 2
(`0044-mc-compiler-memory.md:103-114, 116-126`).

*Proof.* Induction over the instruction sequence: each instruction is a
permutation of contents, so the multiset — in particular the number of zeros —
is invariant between marshalling and unmarshalling. A zero appearing in a cell
that held nonzero must have moved from a cell that held zero. ∎

### Lemma 7.2 (the re-opened hole: an occupied-register LOAD needs a free-zero cell)

M-C2's `LOAD`/`STORE` is the addressed exchange `reg[r] ↔ mem[addr]`. If the
target register r is occupied by a leaf ℓ′ whose home is r (canonical layout,
`0044:71-80`), the bare swap sends ℓ′ *into* `mem[addr]` — the loaded leaf's
home — so either ℓ′ transiently squats in memory (legal only inside a strict
`with_registers`-style sandwich undone before the next canonical boundary,
`0044:87-101`) or ℓ′ must be **parked**, and by Lemma 7.1 parking-with-
restoration of the invariant "every non-home cell is 0" requires a designated
free-zero cell to 3-cycle through: a 2-cycle (one swap) cannot move ℓ into r
and ℓ′ to a rest cell simultaneously. This is the hole the review re-opened
(`0045-mc2-memory-bus.md:46-49`); the deliverable obligation is a **specified
zero-parking discipline** — which cells are free-zero at every LOAD/STORE
emission site, with the marshaller extending "unused cells are 0" across the
memory bank — spec'd as concretely as ADR-0044 §2.3–2.4 before delegation
(`0045-mc2-memory-bus.md:49-53`). ∎

The unifying principle across §4 and §7: **known zeros originate only at
marshalling and are thereafter conserved, budgeted resources** — at the iso
level (no uniti-manufactured bus word, Proposition 4.1), at the ISA level
(aux zero-source, Lemma 7.1), and at the ring level (the bus's rest-zero is
re-established each revolution by conjugation, never re-created, Lemma 3.3).
The frozen-unroll consistency requirement (`0044:128-135`, the L15 class) is
why these disciplines must hold at every canonical boundary, not merely at
program start.

## 8. Proof obligations remaining

Proven here, unconditionally (relative to stated definitions and (N-bij)):

- Proposition 1.2 (pointwise certificate ⇔ ZG), Proposition 1.3 (trajectory
  certificate strictly weaker, with counterexample), Lemma 1.4 (compositional
  cleanliness; whole-domain quantifier shown load-bearing).
- Theorem 2.3 / Corollary 2.4 / Lemma 2.2 (per-ring impossibility; exact
  dichotomy with degeneracy), Remark 2.5 (classification: autonomy, not
  thermodynamics).
- Lemma 3.2 (resident boundary-constant factor is an ancilla),
  Proposition 3.4 (quiescent cut necessary; straddling corrupts semantics),
  Lemma 3.3(a) and the bus half of (b) (protocol-relative).
- Proposition 4.1 / Corollary 4.2 (no zero-introduction iso; residency
  necessary).
- Propositions 5.1/5.2 (oracle blindness is inevitable; the structural audit
  is the right certificate class).
- Lemmas 7.1/7.2 (zero conservation; the occupied-register LOAD obligation).

Proven relative to an explicit model:

- Theorem 6.1 / Corollary 6.2 — rigorous given Axiom E1 (2LAL first-order cost
  model). E1's fidelity is empirical (char DB / B5); the area–energy
  distinction itself is model-independent bookkeeping.

Conjectures and open obligations (marked, per the house rule that a marked
conjecture outranks a hand-waved theorem):

- **C1 (the load-bearing one).** *Whole-domain* ancilla-cleanliness of the
  lowered 1-of-N Fredkin exchange tree, N ∈ {2, 4, 8}. Argued by analogy to
  the 1-of-8 `select_fields` crossbar (`decoder.rs:187-204`) and reducible via
  Lemma 1.4 to per-level gadget cleanliness (each level is controlled word
  swaps + address-decode AND-chains in the R1 sandwich shape) — but the
  reduction's hypotheses (disjoint scratch blocks, whole-domain per gadget)
  must be checked against the real lowering, and the spike itself demands the
  whole-domain demonstration at N ∈ {2,4,8} before any delegation
  (`0045-mc2-memory-bus.md:97-98`). Theorem 3.5 is conditional on this.
- **H3 realizability — DISCHARGED IN DESIGN** (2-agent H3 spike, ADR-0046
  §4.5.8). The obligation is *dissolved*, not merely met: there is **no seam
  rotation**. The revolution is the unrotated Lemma 3.3 factorization
  `fetch;decode_execute′;emit;mem;absorb;unfetch;pc_add_br;pc_inc` (one iso
  term); the transaction is *interior*, the cut falls after `pc_inc` (between
  `absorb` and the next `fetch` — where Prop 3.4 requires), and rev 1 runs
  correctly from IR=0 with **no prologue/epilogue**. `emit`/`absorb` re-derive
  `(enable,rd,addr)` from the persistent raw IR (no new decode latch;
  `absorb = sym(emit)`). One honest new obligation surfaced: `BusMsg` lowers to
  *data* ports (not the lowering-ancilla section), so the composite ring must add
  an explicit **bus-quiescence check** (BusMsg lines = 0 at every boundary) — the
  existing `ancillas_restored` does not cover it. Full construction: ADR-0046
  §4.1 (corrected) + §4.5.8.
- **Protocol conformance (H4).** Lemma 3.3(b)'s table is a proof about the
  *specified* protocol; the implemented emit/absorb must match it. The
  discharge is the Stage-1 composite certificate itself:
  `restored_ok ∧ run == run_open ∧ run == eval(system_step)^R` plus the §5
  line audit, run end-to-end on the single-body composite ring before any
  two-body `SystemRing` (`0045-mc2-memory-bus.md:96-101`) — the L15-safe
  ordering.
- **Domain coverage of the pointwise certificate.** `ancillas_restored` is
  checked at sampled starts plus the hard assertion on every actual run start
  (`ring_run.rs:102-107`); a whole-domain or structural (Lemma 1.4-style
  compositional) discharge for the full 2^160 State space remains open, for
  the composite as for today's ring.
- **Zero-parking spec (M-C2 deliverable gate).** Lemma 7.2 names the
  obligation; the concrete per-site discipline (ADR-0044 §2.3–2.4 fidelity)
  must be written before the resident-memory milestone is delegated
  (`0045-mc2-memory-bus.md:49-53`).

## Provenance

- **Restated (proved elsewhere, cited):** TH-4a (`theory/coverage-bijectivity.md`)
  and TH-1 (`theory/finite-trace-totality.md`) supply "every core term denotes
  a total bijection" for Proposition 4.1; the parity obstruction and its R1
  voiding are ADR-0040's (machine-checked in
  `crates/theseus-netlist/tests/ancilla_clean.rs`, `tests/rail_role_swap.rs`).
- **Formalized here, first stated in ADR-0045:** Definitions 1.1/3.1 (ZG,
  ZG-2), Theorem 2.3 (Result 1), Lemma 3.2 + Theorem 3.5 (Result 2),
  Proposition 4.1 (§0 finding 2), Proposition 5.1 (§0 finding 3),
  Theorem 6.1 (Result 3 / §0 finding 4).
- **Novel to this note:** the counterexample of Proposition 1.3 (the two
  certificates are inequivalent — why `run` hard-asserts rather than trusting
  `run == run_open`); the reachability witness of Lemma 2.2; the degenerate
  converse (Corollary 2.4) making the external/degenerate dichotomy exact; the
  conjugation-invariance failure framing of seam rotation (Proposition 3.4);
  and the zeros-are-conserved principle unifying §4 and §7.
