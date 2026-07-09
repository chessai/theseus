# ADR-0001: Rust end to end

**Status:** Accepted

## Context
The original stack plan used Clash (Haskell) for the hardware layer, whose
signature value is native cosimulation: circuit-as-host-code, tested with
the same tooling as the semantics. The project owner prefers Rust.

## Decision
The entire implementation — language frontend, core, interpreter, netlist
IR, simulators, emitters, verification — is Rust. No Clash.

## Consequences
We lose native cosimulation and repay it by owning a cycle-level netlist
simulator and a differential-testing spine (ADR-0008, ADR-0018). Π compiles
to purely *structural* netlists, so behavioral synthesis is never needed;
the netlist IR we must own anyway is exactly the artifact the silicon
layer wants. One language end to end; the oracle chain replaces Clash's
"simulate in the host language" property.

## Alternatives rejected
Clash (host-language mismatch); Rust frontend + Clash backend (two
ecosystems, an FFI seam through the most delicate boundary).
