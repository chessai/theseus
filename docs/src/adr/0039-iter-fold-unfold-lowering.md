# ADR-0039: `iter`/`fold`/`unfold` netlist lowering -- tag-gated full unroll

**Status:** Accepted

## Context

ADR-0036 and ADR-0037 landed the three v0.2 bounded-loop core
primitives -- `iter n f : n × a ↔ n × a` and the adjoint pair
`fold`/`unfold l f : l × c ↔ l × c` -- through the interpreter only
(software dialect). The netlist backend explicitly rejected them, so
programs using structural recursion evaluated but could not be
compiled to the adiabatic cell netlist. The backend is unroll-only
(ADR-0026 C2.4: `TraceLoop` lowers by full unrolling to |u|+1
tag-controlled body applications), and all three primitives carry
their iteration bound in their *type* -- `|n| − 1` applications for
`iter`, spine depth − 1 for `fold`/`unfold` -- so the same strategy
applies. This ADR gives them a real lowering.

## Decision

Lower all three by **full unrolling with one-hot tag gating**, in the
new `NodeKind::IterLoop`/`NodeKind::FoldLoop` typed-IR nodes (the
"bounded loop nodes" ADR-0013 reserved), implemented in
`theseus-netlist/src/lower_iter.rs`.

### The one-hot observation

Under the dual-rail encoding, a numeral type `1 + (1 + (... + 0))` and a
list-spine type `1 + (b × (1 + (b × (... + 0))))` flatten to one **tag
pair per unroll layer** (interleaved with element payloads for
spines). The encoding of numeral `k` -- or of a length-`k` list's spine
-- has tags FALSE at layers `0..k`, TRUE at layer `k`, all-FALSE
padding after: **one-hot at `k`**. Hence

> "still looping at step t" ⟺ k ≥ t ⟺ ¬tag₀ ∧ ... ∧ ¬tag_{t−1},

a positive AND of rail-swapped tag reads. The chain
`c₁ = ¬tag₀`, `c_{t+1} = c_t ∧ ¬tag_t` costs one `K2` AND cell per
step (the borrowed-ancilla reduction from B4.2 synthesis:
`K2(a, b, 0, 1)` yields `a ∧ b` on its first data output, with the
tag bits passing through unchanged -- the counter/spine is preserved
on output by construction).

This required making the `0`-terminated sum types encodable at all:
an **uninhabited sum arm now occupies zero payload rails** (the tag
can never select it). `0` itself stays unencodable at top level --
such nets are statically dead.

### Constructions

- **`iter n f`** (`K = |n|` layers): emit the control chain, then
  `K − 1` applications of `f` on the payload, application `t` gated by
  `c_t` via the same `controlled_apply` promotion machinery
  `Branch`/`TraceLoop` use (K0→K0, K1→K2, K2→K3). A counter `k` makes
  exactly `c₁...c_k` true ⇒ `f` fires `k` times ⇒ `⟦iter⟧⟨k,x⟩ = ⟨k,fᵏx⟩`.
- **`fold l f`** (spine depth `d`): same chain over the spine tags;
  application `t` runs `f` on `[elem_{t−1}, acc]`, writing the
  rewritten element back into its slot and threading the accumulator --
  in **ascending** slot order (head-to-tail, reversible mapAccumL).
- **`unfold l f`**: the *same gated steps in descending order*. For a
  length-`k` list only steps `k, k−1, ..., 1` fire, so the body runs
  tail-to-head from the data-dependent last element -- no extra logic
  for the variable start position. `(fold l f)† = unfold l (f†)` holds
  at the netlist level because `sym` is eliminated syntactically
  (ADR-0014) before lowering.

Inactive steps see all-FALSE padding slots and control-off promoted
cells: pass-through, INV-TOKEN preserved.

### Gate-safe bodies

`controlled_apply` promotes an arm's **cells** by one control, but
zero-cell *rewiring* interleaved with cells (e.g. `cx ; swapˣ`) is
wire routing and cannot be gated. A body is therefore passed to
`controlled_apply` directly only when it is a single `Prim` (B4.2
lowering is always in-place or pure wiring) or lowers to pure wiring
(fully handled by the conditional-permutation synthesis). Any other
body is **tabulated**: its standalone (already per-instance verified)
lowering is evaluated over the whole finite domain and wrapped as a
monolithic `PrimDef`, which lowers in-place under the gate. This is
exhaustive-small by design; bodies wider than the B4.2 synthesis cap
(`MAX_SYNTH_WIDTH` rail pairs) are refused, never guessed at.

## Consequences

- `iter`/`fold`/`unfold` programs now compile end-to-end: lower →
  validate → balance → timed phased-sim, all ≤ K3 cells. Example
  costs: `iter N₄ rot3` = 20 K2 cells; `fold L₃(Bit) cx` = 5 K2 cells;
  `unfold L₄(Bit) (cx;swapˣ)` = 14 K2 cells.
- Cell cost is `O(steps · body)` plus one `K2` per step for the
  control chain -- the same budget class as `TraceLoop` unrolling.
- The gate-safety tabulation loses body *structure* for mixed
  cell+rewiring bodies (they re-synthesize monolithically). A later
  QoR pass could instead teach `controlled_apply` to gate residual
  rewiring directly, benefiting `Branch` arms too.
- Verification norm: differential oracle vs `theseus_eval::eval` over
  the whole finite domain (`tests/iter_lowering.rs`), including
  non-self-inverse and order-sensitive bodies that distinguish
  head-to-tail from tail-to-head threading, `sym`-composed forms, and
  the balanced timed simulation with dynamic INV-TOKEN.
