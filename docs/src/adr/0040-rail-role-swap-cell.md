# ADR-0040: The controlled rail-role-swap cell (Rk) — voiding the parity law

**Status:** Proposed (evaluation accepted-in-principle; proof-of-concept
`RoleSwapKind` shipped; fabric integration — char DB, B5 admission,
lowering rewrite — is a follow-up slice with its own gates)

## Context — the obstruction this cell exists to void

The K0–K3 vocabulary has a proven parity obstruction (bennett.rs module
docs; machine-checked in `tests/ancilla_clean.rs::parity_obstruction_is_real`):

- **Premise 1:** every K-cell *permutes the values* on its rail pairs —
  a controlled swap of whole pairs routes tokens between pairs but never
  creates or destroys a `1`-value.
- **Premise 2:** free NOTs (rail-role swaps, ADR-0031 "NOT-in-Permute")
  are *fixed per edge* — each contributes a circuit constant.
- **Law:** for any netlist in this vocabulary,
  `parity(data_out) ⊕ parity(anc_out) = parity(data_in) ⊕ parity(anc_in) ⊕ c`
  for a circuit constant `c`.
- **Consequence:** a conditional-NOT's parity deviation *is* its AND, so
  CNOT/Toffoli lowerings can never have all-constant ancilla outputs.
  Garbage-free reversible logic is provably impossible in K-cells alone;
  the Bennett wrap's `2·w` boundary residue per revolution is the honest
  floor, and the ring machine (ADR-0033) must re-supply those boundary
  pairs every revolution forever.

ADR-0033's recorded escape hatch is a cell that violates Premise 1
*without violating any physical invariant*: a controlled swap whose two
swapped conductors are **the two token lines of one dual-rail pair**.
This ADR evaluates that cell.

## D1 — Definition

**`Rk`, the k-controlled rail-role swap**, `k ∈ {1, 2, 3}`; ports are
`k` control pairs then one target pair (`arity = k + 1` rail pairs,
`2(k+1)` token lines).

Line level: all control lines pass through (as K-cell controls do). The
target's two token lines are **exchanged with each other** iff every
control pair carries a `1` token:

```text
t1_out = if ⋀cᵢ { t0_in } else { t1_in }
t0_out = if ⋀cᵢ { t1_in } else { t0_in }
```

Because exactly one of `{t0, t1}` pulses per token (1-of-2, INV-TOKEN),
exchanging the lines maps the encoded value `t` to `¬t`. Value level:

```text
Rk : (c₁, …, c_k, t) → (c₁, …, c_k, t ⊕ (c₁ ∧ … ∧ c_k))
```

`R1` *is* CNOT; `R2` *is* Toffoli — one cell, zero ancillas.

Truth table for `R1` (value level, and the line level beneath it):

| c | t | → | c | t′ |   | c1 c0 · t1 t0 | → | c1 c0 · t1′ t0′ |
|---|---|---|---|----|---|---------------|---|-----------------|
| 0 | 0 |   | 0 | 0  |   | 0 1 · 0 1     |   | 0 1 · 0 1       |
| 0 | 1 |   | 0 | 1  |   | 0 1 · 1 0     |   | 0 1 · 1 0       |
| 1 | 0 |   | 1 | 1  |   | 1 0 · 0 1     |   | 1 0 · 1 0       |
| 1 | 1 |   | 1 | 0  |   | 1 0 · 1 0     |   | 1 0 · 0 1       |

**How it differs from a k-CSWAP.** `K1(c, a, b)` swaps two *pairs*:
lines route `a.0↔b.0, a.1↔b.1`, preserving each pair's internal role
assignment — so the multiset of pair-values is permuted. `R1(c, t)`
swaps the two *lines of one pair*: `t.0↔t.1` — the token count is
identical, but the pair's value flips. Same physical act (a controlled
exchange of two conductors' charge), different wiring pattern; only the
*encoding-level* consequence differs. Note `R1` swaps **one** conductor
pair where `K1` swaps two in parallel — per routed line, `R1` is *half*
a `K1` target section, plus the same control pass-through.

## D2 — Admissibility (ADR-0027 B4.3 / ADR-0022 / ADR-0026 criteria)

**Conservative / charge-routing — yes, exactly.** A controlled exchange
of two conductors only routes charge; the count of asserted token lines
is invariant per tick. The target pair is 1-hot before the swap and
1-hot after (exchanging the two lines of a 1-hot pair is still 1-hot),
so INV-TOKEN — the *physical* invariant, "count of asserted rails is
conserved per tick" (ADR-0031) — holds bit-for-bit. Nothing is created,
destroyed, or dumped.

**Reversible and self-inverse — yes.** `Rk∘Rk = id` (the controls are
unchanged, and a conditional exchange is an involution). C2.5's
physical-adjointness story (reverse phase sequence executes the adjoint)
applies unchanged.

**Adiabatically recoverable — yes, by the same topology as K1.** The
characterized cswap fabric (`theseus-spice::cswap` module docs) builds
each output line as TG branches for a condition
`F = ⋁ᵢ(ℓᵢ₁ ∧ ℓᵢ₂ ∧ …)` over token-line literals, with per-literal
mirrored reverse chains for Bennett decomputation. `R1`'s two routed
output lines are:

```text
t1_out : F = (c₁-line ∧ t0) ∨ (c₀-line ∧ t1)
t0_out : F = (c₁-line ∧ t1) ∨ (c₀-line ∧ t0)
```

— two branches of two literals each, control literal clock-side, data
literal output-side: *the identical shape* as a `K1` target output line
(`a1_out : F = (c₁-line ∧ b1) ∨ (c₀-line ∧ a1)`). The non-monotone
control (a TG that conducts *because* `c = 0`) is gated by the `c0`
token line, which rests off — the exact sneak-path fix the 1-of-2
encoding was introduced for. The delayed-literal reverse chains are
consumer-realizable because the cell is self-inverse. Validity-window
obligations are the same class as `K1` (C2.6's deferred arithmetic; no
new internal staging). No transistor path to GND/VDD; a `0`-control
token moves no target charge at all. **There is no new physics here:
`Rk` is a rewiring of the already-characterized k-CSWAP transmission-gate
fabric, with fewer routed lines than `Kk+1`.**

**The honest crux.** What `Rk` gives up is not a physical invariant but
an encoding-level theorem. The K-family made "conserves tokens" and
"permutes pair-values" look like the same property because a whole-pair
swap has both; they are separable, and only the first is physics. Charge
does not know rail roles — roles are a compiler convention, which
ADR-0031 already exploits *statically* (free NOT = fixed rewiring). `Rk`
makes exactly one role exchange *conditional on a token*, inside one
characterized cell, while the netlist's role assignment of every edge
stays static (`t1_out` is statically the 1-line; the cell routes charge
so that the value on it is `t ⊕ ⋀cᵢ`). Downstream analyses
(`validate`, `line_arrivals`, balancing, phases) are untouched by
construction: `Rk` is one cell, one tick, `arity` in = out.

**Named obligations before fabric admission (not waived by this ADR):**

1. its own char-DB entry (`cswap-r1` etc.) and the full B5 admission
   battery — reverse-drive validation, conservation bookkeeping, energy
   curve slope (expected ≈ `K1`'s, since the topology is a subset);
2. ADR-0027 B4.1's one-cell philosophy grows a second shape. The
   justification is B4.2's own parenthetical: Bennett scratch was
   forbidden *because* "garbage-freedom means we never need it" — and
   the parity law proves the K-only vocabulary can never deliver
   garbage-freedom. B4.2's premise is false without `Rk`; admitting `Rk`
   is what makes B4.2 honest.

**Verdict: admissible.** Every ADR-0027 criterion is met by the same
arguments, and mostly the same transistors, that admitted K1.

## D3 — The parity law is void: proof

**The law's derivation fails.** The law telescopes over cells: each
K-cell changes total pair-value parity by 0 (Premise 1), each fixed
role-swapped edge by a constant (Premise 2), so the total change is a
circuit constant. `R1` changes total pair-value parity by
`(c ⊕ (c ⊕ t)) ⊕ (c ⊕ t) = c` — **data-dependent**. The telescoping sum
no longer yields a constant; the invariant does not exist in the
extended vocabulary. (Machine-checked: `R1.eval` on `(1, 1)` yields
`(1, 0)` — the output multiset differs from the input multiset, the
exact negation of `parity_obstruction_is_real`'s premise (a).)

**Constant ancilla outputs are achieved, constructively.**

- *CNOT* = one `R1`, **zero ancillas**. Ancilla-output parity is
  (vacuously) constant while the data parity deviation is `c` —
  precisely the combination the law forbade.
- *Toffoli without R2*, garbage-free with one scratch pair
  (compute–use–uncompute): `K1(c₁, c₂, anc=0)` puts `c₁∧c₂` on its
  third port; `R1(that, t)` flips `t` conditionally *without modifying
  its control*; replaying the same `K1` (self-inverse, its three
  inputs unchanged) restores `(c₁, c₂, 0)`. Ancilla out = `0` on **all
  eight inputs** — constant, data-independent, ancilla-clean. Both are
  machine-checked in `crates/theseus-netlist/tests/rail_role_swap.rs`.

**It does not merely move the problem.** The worry would be: "the AND
must still be computed, and computing it makes garbage." But the scratch
AND is uncomputed exactly (self-inverse replay, controls untouched by
`R1`), and the *effect* survives the uncompute because `R1`'s action on
the target is a role flip, not a value routing that the mirror would
undo. Before `Rk`, the residue was **unremovable by theorem**; after,
nothing obstructs constant ancillas, and the witnesses above are total
over their domains. Nor does a replacement obstruction appear:
`{R1, R2}` value-level are CNOT and Toffoli, which (with free NOT)
generate all even permutations of the encoded state space and, with one
borrowable line, all of them — there is no residual invariant of the
parity law's kind.

**Consequences for the ring (ADR-0033).** `lower.rs`'s `emit_mcnot`
splitter/selector construction — 2 dirty pairs per instance, the entire
`prim_synth` dirt of the TISC-1 step (945/1138 at pc = 0) — is exactly a
conditional-NOT worked around with value-permuting cells. With `Rk` it
becomes: AND-reduce controls (existing chain), one `R1` on the chain
head, uncompute the chain — **ancilla-clean per instance**. Plain
lowering of `step` then restores every ancilla; `ancillas_restored ==
true` becomes reachable; the Bennett wrap (2× cells, 2× depth, `2·w`
boundary pairs re-supplied per revolution) demotes from mandatory to
optional; the ring physically closes with no per-revolution boundary
traffic. That is the payoff this evaluation was asked to price.

## Decision

1. **`Rk` is admissible and voids the parity law.** Recommend admission
   to the cell library as the path to physically closing the ring.
2. This slice ships the proof-of-concept only: `RoleSwapKind` (a sibling
   enum in `ir/cell.rs`, deliberately **not** a `CellKind` variant yet —
   a variant today would ripple through every exhaustive match in
   `lower.rs`/energy/spice with no char-DB entry behind it) with its
   value-level `eval`, plus machine-checked witnesses
   (`tests/rail_role_swap.rs`): truth table = CNOT, self-inverse,
   line-level/value-level agreement + INV-TOKEN, premise violation, and
   the ancilla-clean Toffoli sandwich.
3. Follow-up slice (gates, in order): SPICE deck + `cswap-r1` char-DB
   entry + B5 battery → promote to `CellKind::R1` (+R2/R3 as measured
   need dictates, per B4.1's "measured, not doctrine") → `emit_mcnot`
   rewrite → drop the wrap from the ring path → re-run the TISC-1 ring
   with `ancillas_restored` asserted.

## Alternatives rejected

- **Status quo (K-only + Bennett wrap + boundary re-supply):** honest
  but permanently pays 2× cells, 2× depth, and `2·w` externally
  refreshed pairs per revolution — for a residue that exists only
  because of a vocabulary restriction physics does not impose.
- **Dynamic role-tracking in the compiler** (let edges' roles depend on
  data): breaks the static netlist discipline every analysis relies on;
  `Rk` gets the same effect inside one characterized cell with static
  edge roles.
- **Admitting general Bennett scratch macros (B4.2 relaxation) instead:**
  strictly weaker — still bounded by the parity law, still residue at
  the boundary.
