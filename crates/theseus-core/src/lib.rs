//! Core Π: the tiny categorical calculus everything else consumes.
//!
//! - v0.1 core is Π + `trace`, nothing else (ADR-0009): in the finite
//!   fragment trace is total with bound ≤ |u| (ADR-0007; proof obligation
//!   ledger entry TH-1 in the theory notes).
//! - v0.2 adds `iter` and `fold`/`unfold` as *primitives* — totality is
//!   structural, never certificate-based (ADR-0009).
//! - Adjoint is a syntactic involution; every rewrite must commute with it
//!   (T1 property, ADR-0016/ADR-0018).
//! - Core rewrites are minimal: structural (swap/assoc/unit) equalities
//!   belong to the canonicalizer, not here (ADR-0016). Fixed-order rewrite
//!   pipeline in v0.1; equality saturation is a v0.3+ candidate (ADR-0010).

pub mod ty {
    //! Types: 0, 1, sums, products. Finite in the synthesizable dialect
    //! (ADR-0006); sizes are exact and drive exhaustive-mode testing.
}

pub mod iso {
    //! Enumerated combinators (the ten structural isos + distrib + trace),
    //! sequential/parallel/choice composition, `PrimRef` opaque nodes
    //! resolved at elaboration and verified offline (ADR-0013/ADR-0016).
}

pub mod adjoint {}

pub mod rewrite {}
