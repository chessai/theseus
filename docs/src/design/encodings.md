# Encodings and canonicity

Sums are typed in the IR; encoding is backend lowering (ADR-0011).

**FPGA default:** padded tag + shared payload (width 1 + max), constructor
order = tag order (ADR-0017). Disjoint-payload layout available as a local
heuristic for shallow sums. Dense arithmetic encoding
documented-rejected. Dual-rail / 1-of-N earmarked for the adiabatic
backend -- the native idiom of adiabatic families.

**Canonicity (ADR-0012):** `valid ⇒ well-formed`, conditional and locally
re-established -- per-node lowering lemmas (both directions, per-instance
checkable: exhaustive small, SAT large) plus defensive masking at every
merge; emitted HDL contains no `x`; boundary discharged by generated
encoders; simulators assert the invariant every cycle in test builds.

Lemma budget per backend: Permute(×), Permute(+), Distrib/Factor,
Branch, TraceLoop, Reg, plus per-prim bijectivity obligations.
