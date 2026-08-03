# TISC-1: the theseus CPU

The CPU is a theseus program -- `iso step : State <-> State` -- compiled by
this compiler through the adiabatic backend (ADR-0022 A2). Coverage
checking *is* the machine's forward and backward determinism: left-
exhaustiveness = the machine always knows what to do (undefined opcodes
are compile errors); right-exhaustiveness/non-overlap = no two states
step to the same place -- the past is never merged away. `~step`
un-executes, with no trace stored, because the state contains its
history.

Architecture (ADR-0029/0030): exchange-based memory (addressed exchange
= Fredkin tree = the one cell); fetch-execute-unfetch with the
instruction register provably zero between steps; PC-exchange branching;
merge/exchange-only data ops; W=8, 8 regs, ~8 scratch words in State
(Option A), with the Option-B swap-bus seam present in the source as
named isos from day one.

M4-v1 is an **unrolled-T machine** (T ≈ 16-64) -- the temporal/spatial
duality made literal; the recirculating M4-v2 is gated on the ring-loop
wake condition (C2.4). Workloads: Fibonacci (legibility, run backward)
and a Feistel round (energy workload).

The substrate/software split, stated honestly: CPU-as-iso guarantees the
*machine* never pays a Landauer toll; programs running on it may still
compute garbage they must uncompute or carry -- which is the problem the
language solves one level up.
