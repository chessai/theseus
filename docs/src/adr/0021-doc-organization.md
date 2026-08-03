# ADR-0021: Documentation organization

**Status:** Accepted (amended: Structurizr instead of Mermaid)

## Context
Twenty ratifications, parked notes, a risk register, theory debts, and
roadmap material need homes with the right change disciplines.

## Decision
Five documents split **by change discipline**: ADRs (append-only; context
and rejected alternatives preserved; `Supersedes:` links; numbers are
permanent citable identifiers), design document (living, normative for
implementation, no memory -- *what is true*), language specification
(versioned with the language, RFC-2119, **implementation-silent**, error
catalog = `--explain` source of truth, conformance suite as executable
appendix), theory notes (living commentary **with a proof-obligation
ledger**: proved/sketched/assumed), roadmap (everything with a *when*).
Plus CONTRIBUTING.md carrying the definition-of-done.

Docs-as-code, one mdBook, CI-published. **Diagrams: Structurizr DSL**
(owner amendment) under `docs/diagrams/`, rendered via structurizr-cli.
Typst enters later for papers only. ADR-citation lint: every ADR cited
at least once outside the ADR directory.
