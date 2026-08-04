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

    // ── Iter (bounded iteration, ADR-0009/ADR-0036) ─────────────
    /// `iter n f : n × a ↔ n × a`  (when `f : a ↔ a` and `n` is a
    /// numeral type `1 + (1 + (... + 0))`).
    ///
    /// Denotation: `⟦iter n f⟧ ⟨k, x⟩ = ⟨k, ⟦f⟧ᵏ x⟩` where `k` is the
    /// unary numeral `inrᵏ(inl ())` inhabiting `n`. The iteration count
    /// `k` is carried in the DATA (first component) and preserved on
    /// output, so the term is symmetric by construction:
    /// `(iter n f)† = iter n (f†)`, and the backward run un-applies the
    /// body exactly `k` times. Totality is structural (ADR-0007): a
    /// value of the finite numeral type `n` encodes `k ≤ |n| − 1`, so
    /// evaluation applies the body at most `|n| − 1` times -- the bound
    /// is read off the type, no certificate needed.
    Iter(Type, Box<Iso>),

    // ── Fold/Unfold (structural recursion, ADR-0009/ADR-0037) ───
    /// `fold l f : l × c ↔ l × c`  (when `f : b × c ↔ b × c` and `l` is
    /// a list-spine type `1 + (b × (1 + (b × (... + 0))))` -- the shape
    /// ADR-0035 bounded unrolling produces for `List b`).
    ///
    /// Denotation (reversible mapAccumL, head-to-tail):
    /// `⟦fold l f⟧ ⟨[x₁ ... xₖ], c₀⟩ = ⟨[y₁ ... yₖ], cₖ⟩` where
    /// `⟨yᵢ, cᵢ⟩ = ⟦f⟧ ⟨xᵢ, cᵢ₋₁⟩`. The spine (and hence the length
    /// `k`) is preserved in the DATA, so the term is symmetric by
    /// construction: `(fold l f)† = unfold l (f†)` un-threads the
    /// accumulator tail-to-head. Totality is structural (ADR-0007):
    /// the finite spine type bounds `k` by its unroll depth − 1, so
    /// the body runs at most that many times in either direction.
    Fold(Type, Box<Iso>),

    /// `unfold l f : l × c ↔ l × c` -- the tail-to-head dual of
    /// [`Iso::Fold`] (same spine/body typing):
    /// `⟦unfold l f⟧ ⟨[x₁ ... xₖ], dₖ⟩ = ⟨[y₁ ... yₖ], d₀⟩` where
    /// `⟨yᵢ, dᵢ₋₁⟩ = ⟦f⟧ ⟨xᵢ, dᵢ⟩`. Adjoint: `(unfold l f)† = fold l (f†)`.
    Unfold(Type, Box<Iso>),

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

    pub fn iter(n: Type, f: Iso) -> Self {
        Iso::Iter(n, Box::new(f))
    }

    pub fn fold(l: Type, f: Iso) -> Self {
        Iso::Fold(l, Box::new(f))
    }

    pub fn unfold(l: Type, f: Iso) -> Self {
        Iso::Unfold(l, Box::new(f))
    }
}
