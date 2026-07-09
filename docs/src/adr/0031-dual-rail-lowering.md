# ADR-0031: Dual-rail lowering — the lemma catalog (C1)

**Status:** Accepted (clerical: content determined by ADR-0023/0026/0027)

## Decision
The adiabatic backend's lowering, per ADR-0012's framework, third
backend column:

- **Encoding:** every logical bit = rail pair (S, S̄); well-formedness =
  exactly-one-high during validity windows; sums lower per ADR-0011's
  dual-rail earmark: 1-of-N tag rails + payload rail pairs.
- **Per-node lemmas** (each bidirectional per C2.5's modal-direction
  reading; each per-instance checkable — exhaustive small, SAT large):
  Permute(×,σ): rail-pair rewiring, zero cells. Permute(+,σ): tag-rail
  rewiring, zero cells. NOT-in-Permute: rail swap, zero cells.
  Branch: CSWAP demux tree forward / merge tree adjoint; masking
  manifests as token routing — INV-TOKEN replaces bit-masking as the
  local re-establishment mechanism. Distrib/Factor: tag-controlled
  payload steering, quarantined as before. TraceLoop: unrolled per
  C2.4; per-stage phase advance. Prim: CSWAP network from the registry
  (B4.2). Reg(n): n buffer stages.
- **New invariant INV-TOKEN:** count of asserted rails is conserved per
  tick across every cell — asserted by the phased-sim every tick;
  violations localize to a node.
- **Validity protocol obligations** attach per cell from B4.3 once
  C2.6's window arithmetic is pinned by bench data; until then the
  lemma statements carry explicit `window-TBD(bench)` holes — the one
  sanctioned incompleteness, tracked on the risk register.
