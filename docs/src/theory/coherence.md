# Coherence and canonical permutations (TH-3)

Mac Lane coherence for symmetric monoidal categories: parallel structural
morphisms inducing the same leaf permutation are equal. Therefore the
canonicalizer's `Permute(monoid, σ)` is a *unique normal form* for
structural plumbing — normalization-by-evaluation for monoidal
expressions: flatten each layer to n-ary (assocs vanish), absorb units
(`1` under ×, `0` under +), read off σ (ADR-0014).

Distrib is excluded: it is the unique leaf-duplicating combinator
(leaf multiset {a,b,c} → {a,b,a,c}) and is governed by Laplaza-style rig
coherence, which is subtler — hence Distrib/Factor remain explicit nodes.

**Standing assumption:** all of the above is for the rig fragment.
Fractional/negative-type extensions (parked at L0) have genuinely
subtler coherence; Permute's canonicity claim must be re-examined before
any such extension lands. Executable form: the canonicalizer completeness
test (two structural terms canonicalize equal iff denotationally equal —
exhaustively checkable per type).
