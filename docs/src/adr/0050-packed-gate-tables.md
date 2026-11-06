# ADR-0050: Packed gate truth tables and the Shannon BDD build

**Status:** Accepted -- **implemented** (`theseus-tisc::bveq_netlist`:
`GateTable`, `Mask`, `shannon`). Changes how the BVEQ netlist walk turns a
prim into BDDs, not what it computes: the table is still synthesized from
`PrimDef::forward` (MAJOR-4), and the output BDDs are identical.

## Context

`eval_bdd` applies every `Iso::Prim` by building one BDD per output bit
from the prim's truth table. The table was one heap row per entry
(`Vec<(Vec<bool>, Vec<bool>)>`), and each output bit was a minterm
expansion: for every row with the bit set, AND the `k` input literals, then
OR the minterm into an accumulator. That is `2^(k-1)·(k+1)` BDD applies
for a balanced column, each building an intermediate minterm BDD. It paid
the full price even for the control columns that every K/R cell passes
through unchanged. biodivine-lib-bdd has no shared node table, so each
apply is a fresh product traversal: the apply count *is* the cost.

## Decision

1. **Packed columns.** Each output column is a bitmask over input rows.
   Bit `x` is the output on the row whose input `i` is bit `i` of `x`
   (`bits_of` order, LSB first). The row comes from each value's own bits
   (`row_of`), never from its `enumerate_values` index (which walks
   `Left` = 1 first, outer field first). A gate with `≤ 6` inputs keeps one
   `u64` per column; every cell does. Wider gates use `2^(k-6)` words.
   Rows `bits_of` never produces (uneven-sum padding) are 0, as the
   minterm expansion left them. Gates over 24 inputs are rejected
   (fail closed, MINOR-6).
2. **Shannon build.** Each column is built as
   `f = ite(x₀, f|x₀=1, f|x₀=0)`, recursing on the inputs in `bits_of`
   order. Cells list their controls first, so a k-controlled swap arm is
   `k` nested `ite`s (K2: `ite(c1, ite(c2, b, a), a)`), and a
   k-controlled NOT is `k-1` `ite`s around one `xor`. Splitting
   target-first triples K2's cost.
3. **Short-circuits from the masks alone, at every level.** A constant
   column returns `true`/`false`. A column equal to input `j` returns that
   BDD, and one equal to `¬input j` returns its complement. Equal
   cofactors skip the variable. A constant input BDD keeps only one
   cofactor. A constant cofactor turns the `ite` into `and` / `and_not` /
   `imp` / `or`, and complementary cofactors turn it into `xor`. Only what
   is left goes to `Bdd::if_then_else`.

Per gate application, pass-through columns cost 0 applies. K1 costs 2,
K2 4, K3 6, R1/CNOT 1, R2/Toffoli 2 and R3/CCCNOT 3. The minterm
expansion spent 12 on a CNOT, 48 on a Toffoli and 160 on a K2.

## Verification

The minterm implementation stays under `#[cfg(test)]` as the oracle.
biodivine BDDs are canonical, so the checks use exact `==`. They cover
every prim `step(rom)` / `system_step(rom)` applies, plus its adjoint,
and the whole K/R cell family, on plain-variable inputs and on random
composite, constant and aliased inputs. They also cover every function of
`≤ 3` inputs and random columns up to 8 inputs (the multi-word path).
Tests pin the row order, the per-cell apply counts above, and the zero
cost of constant and pass-through columns.

## Consequences

- Walk outputs are unchanged BDD for BDD. Downstream node counts and
  every BVEQ verdict are unaffected.
- The gate-application share of a walk drops from exponential in the
  gate width to at most `k` applies per non-trivial column. In walks
  dominated by Iso construction and `adjoint` (the one-arm reconcile
  tier), the end-to-end gain is correspondingly small.
