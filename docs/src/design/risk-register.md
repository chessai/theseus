# Appendix: risk register

| Risk | Severity | Mitigation | Status |
|---|---|---|---|
| Bidirectional coverage checking (ADR-0004) is lightly research territory | High | brute-force semantic oracle; adversarial corpus; prototype earliest; mutation-score gate before downstream trust | open |
| TH-1 totality bound stated informally (ADR-0007) | Medium | theory ledger owes the two-direction proof before "compiler reports exact WCET" is claimed | open |
| Rig-coherence assumption behind Permute (ADR-0014) | Low now | documented; re-examine if fractional/negative types arrive | tracked |
| Rust trades away native cosimulation (ADR-0001) | Accepted | oracle chain + owned simulators (ADR-0008/0018) | mitigated by design |
| Nightly toolchain drift (ADR-0019) | Low | stable-compatible source policy; determinism canary on pin bumps | policy |
| Verilator protocol perf | Low | libloading upgrade path recorded (ADR-0020) | deferred |
| k-CSWAP internal staging may need internal ticks (→ macro-cell) | Medium | first bench question; latency numbers change, architecture doesn't (ADR-0027) | open |
| Validity-window arithmetic unpinned until bench data (C2.6) | Medium | lemma statements carry explicit window-TBD holes; bench reference decks are the pinning path | open |
| TH-E1 additivity budget provisional until first whole-block comparisons | Medium | fallback: composite-structure characterization (DB granularity change only) | open |
| State-size energy: ~150 bits ≈ 300 rails riding buffers every tick in M4-v1 | Medium | C3 static report quantifies before build; Option-B seam ratified (ADR-0029) | open |
| Physical adjointness may fail for general logic (TH-6) | Low | mirrored-netlist fallback is cheap (syntactic adjoints, ADR-0013) | open |
| Xyce model compatibility unvalidated | Low | parked; validation gate named (ADR-0025) | parked |
