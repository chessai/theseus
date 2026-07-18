# M-D.w IMPLEMENTATION review (incomplete — Fable ended before final verdict)

> Verbatim fresh-context adversarial review (model: Fable), recovered from the
> ephemeral Agent-tool transcript (agentId `afe546cf39a3e266e`) and committed per maestro L21.
> Findings and their test mapping are indexed in [README.md](README.md).

---

fibStep = `swap2 ; addInto` → 2 instructions (Exch + Add), no Swapbr, no scratch — T2a/T3 premises confirmed. All static analysis is now complete; I'm waiting on the single-threaded release run of `mdw_t6` + `fib_data_dependent_real_cpu` to settle the last two empirical questions (T6 fuel adequacy/runtime, and no false-reject from the new terminal check on the heaviest skip program) before issuing the final verdict.
