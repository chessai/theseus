//! Core Π iso terms (combinators).
//!
//! The enumerated combinators: the ten structural isos + distrib/factor +
//! trace, sequential/parallel/choice composition, `PrimDef` opaque nodes
//! resolved at elaboration and verified offline (ADR-0013/ADR-0016).

use crate::prim::PrimDef;
use crate::ty::Type;

/// A Core Π iso term. Every well-typed term `f` denotes a bijection
/// between finite types, written `f : a ↔ b`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Iso {
    // ── Composition ──────────────────────────────────────────────

    /// `id : a ↔ a`
    Id(Type),
    /// `sym f : b ↔ a`  (when `f : a ↔ b`)
    Sym(Box<Iso>),
    /// `f ; g : a ↔ c`  (diagram-order composition; `f : a ↔ b`, `g : b ↔ c`)
    Seq(Box<Iso>, Box<Iso>),
    /// `f + g : (a + c) ↔ (b + d)`  (when `f : a ↔ b`, `g : c ↔ d`)
    Plus(Box<Iso>, Box<Iso>),
    /// `f × g : (a × c) ↔ (b × d)`  (when `f : a ↔ b`, `g : c ↔ d`)
    Times(Box<Iso>, Box<Iso>),

    // ── Structural: sum monoid (+, 0) ────────────────────────────

    /// `swap⁺ : a + b ↔ b + a`
    SwapSum(Type, Type),
    /// `assocl⁺ : a + (b + c) ↔ (a + b) + c`
    AssocLSum(Type, Type, Type),
    /// `assocr⁺ : (a + b) + c ↔ a + (b + c)`
    AssocRSum(Type, Type, Type),
    /// `unite⁺ : 0 + a ↔ a`
    UniteSum(Type),
    /// `uniti⁺ : a ↔ 0 + a`
    UnitiSum(Type),

    // ── Structural: product monoid (×, 1) ────────────────────────

    /// `swapˣ : a × b ↔ b × a`
    SwapProd(Type, Type),
    /// `assoclˣ : a × (b × c) ↔ (a × b) × c`
    AssocLProd(Type, Type, Type),
    /// `assocrˣ : (a × b) × c ↔ a × (b × c)`
    AssocRProd(Type, Type, Type),
    /// `uniteˣ : 1 × a ↔ a`
    UniteProd(Type),
    /// `unitiˣ : a ↔ 1 × a`
    UnitiProd(Type),

    // ── Distributivity ───────────────────────────────────────────

    /// `distrib : a × (b + c) ↔ (a × b) + (a × c)`
    Distrib(Type, Type, Type),
    /// `factor : (a × b) + (a × c) ↔ a × (b + c)`
    Factor(Type, Type, Type),

    // ── Trace ────────────────────────────────────────────────────

    /// `trace f : a ↔ b`  (when `f : u + a ↔ u + b`).
    /// The [`Type`] parameter is the trace carrier `u`.
    Trace(Type, Box<Iso>),

    // ── Prim ─────────────────────────────────────────────────────

    /// `prim p : A ↔ B`  (registry-resolved, ADR-0013).
    Prim(Box<PrimDef>),
}

impl Iso {
    pub fn seq(f: Iso, g: Iso) -> Self {
        Iso::Seq(Box::new(f), Box::new(g))
    }

    pub fn sym(f: Iso) -> Self {
        Iso::Sym(Box::new(f))
    }

    pub fn plus(f: Iso, g: Iso) -> Self {
        Iso::Plus(Box::new(f), Box::new(g))
    }

    pub fn times(f: Iso, g: Iso) -> Self {
        Iso::Times(Box::new(f), Box::new(g))
    }

    pub fn trace(u: Type, f: Iso) -> Self {
        Iso::Trace(u, Box::new(f))
    }
}
