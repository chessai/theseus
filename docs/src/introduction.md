# Introduction

theseus is a full-stack reversible computing project: a mature language in
the Theseus/Π lineage, a Rust interpreter and compiler to reversible
netlists and Verilog, an FPGA bring-up with hardware time-travel debugging,
and a long-range target of adiabatic silicon.

This book is organized by **change discipline** (ADR-0021):

- **ADRs** are append-only. They record *why* — including the alternatives
  we rejected and the failure modes we found by poking. Changing course
  means a new ADR with a `Supersedes:` link.
- The **design document** is living and always-current. It records *what is
  true now*, citing ADRs for history.
- The **specification** is versioned with the language, RFC-2119 normative,
  and implementation-silent. Its executable appendix is the conformance
  suite in `/conformance`.
- The **theory notes** carry the mathematics and an honest
  proof-obligation ledger: proved / sketched / assumed.
- The **roadmap** carries everything with a *when* rather than a *what*.

Diagrams are Structurizr DSL under `docs/diagrams/` (ADR-0021, amended).

A note on provenance: ADRs 0001–0021 transcribe the original planning
dialogue and preserve its numbering (ADR-0010 fills a session-scoping
slot with the rewrite-engine decision). ADRs 0022–0032 record the
adiabatic pivot and its consequences, ratified in a second planning
phase; ADR-0022 supersedes the original roadmap's L3/L4 sequencing.
