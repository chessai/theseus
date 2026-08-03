//! Core Π: the tiny categorical calculus everything else consumes.
//!
//! - v0.1 core is Π + `trace`, nothing else (ADR-0009): in the finite
//!   fragment trace is total with bound ≤ |u| (ADR-0007; proof obligation
//!   ledger entry TH-1 in the theory notes).
//! - v0.2 adds `iter` and `fold`/`unfold` as *primitives* -- totality is
//!   structural, never certificate-based (ADR-0009).
//! - Adjoint is a syntactic involution; every rewrite must commute with it
//!   (T1 property, ADR-0016/ADR-0018).
//! - Core rewrites are minimal: structural (swap/assoc/unit) equalities
//!   belong to the canonicalizer, not here (ADR-0016). Fixed-order rewrite
//!   pipeline in v0.1; equality saturation is a v0.3+ candidate (ADR-0010).

pub mod ty;
pub mod prim;
pub mod bvexpr;
pub mod iso;
pub mod typing;
pub mod adjoint;
pub mod rewrite;
pub mod enumerate;

pub use adjoint::adjoint;
pub use iso::Iso;
pub use prim::{admit, count_leaves, enumerate_values, try_value_from_leaf_bytes, PrimDef, PrimSpec, SpecTransfer, Value};
pub use ty::Type;
pub use typing::{is_numeral_ty, list_spine_elem_ty, max_flat_width, type_of, TypeError};
