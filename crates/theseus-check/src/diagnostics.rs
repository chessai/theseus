//! Structured diagnostics shared by every static-semantics stage.
//!
//! Diagnostics are *structured values* with error codes (ADR-0020): they are
//! never eagerly formatted strings. Each stage constructs a [`Diagnostic`]
//! carrying only the payload keys relevant to its check; comparison in tests is
//! *set-based* over those keys (see [`crate::testkit::assert_diags`]).

use theseus_core::prim::Value;
use theseus_syntax::ast::Span;

/// Static-semantics error codes.
///
/// Room is intentionally left in the taxonomy for later codes (`T0401`
/// and beyond) which will be added by their own stages.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Code {
    /// Names: unresolved reference (unknown ctor/type/iso/prim).
    T0101,
    /// Typing: ill-typed clause / expression.
    T0102,
    /// Linearity: variable used a wrong number of times (T0201).
    T0201,
    /// Linearity: variable bound on one side but not the other (T0202).
    T0202,
    /// Coverage: non-exhaustive (T0301).
    T0301,
    /// Coverage: overlap / redundant clause (T0302).
    T0302,
    /// Coverage: uncovered label state (T0303).
    T0303,
    /// Combinator: `iter` counter type is not a numeral (T0601).
    T0601,
    /// Combinator: `iter` body is not an endo-iso (T0602).
    T0602,
    /// Combinator: `fold`/`unfold` spine type is not a uniform list (T0603).
    T0603,
    /// Combinator: `fold`/`unfold` body is not an endo-iso (T0604).
    T0604,
    /// Combinator: `fold`/`unfold` body type is not a product `b × c` (T0605).
    T0605,
    /// Combinator: `fold`/`unfold` body element type does not match spine element (T0606).
    T0606,
}

/// Which side of a clause (or iso signature) a diagnostic refers to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Side {
    Left,
    Right,
}

/// A structured diagnostic.
///
/// Only [`code`](Diagnostic::code) and [`span`](Diagnostic::span) are always
/// present; every other field is an optional *payload key* set by the stage
/// that produced the diagnostic. The comparison semantics used in conformance
/// tests treat these keys as a set: an expected diagnostic constrains only the
/// keys it lists (see [`crate::testkit`]).
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub code: Code,
    pub side: Option<Side>,
    /// A value witness (e.g. an uncovered input value). Compared *as a value*.
    pub witness: Option<Value>,
    /// A set of 1-indexed clause positions this diagnostic concerns.
    pub clauses: Option<Vec<usize>>,
    /// The offending variable name.
    pub variable: Option<String>,
    /// The offending identifier name (ctor/type/iso/prim).
    pub name: Option<String>,
    pub span: Span,
}

impl Diagnostic {
    /// Bare diagnostic carrying only a code and span.
    pub fn new(code: Code, span: Span) -> Self {
        Diagnostic {
            code,
            side: None,
            witness: None,
            clauses: None,
            variable: None,
            name: None,
            span,
        }
    }

    /// Builder: attach a side.
    pub fn with_side(mut self, side: Side) -> Self {
        self.side = Some(side);
        self
    }

    /// Builder: attach a value witness.
    pub fn with_witness(mut self, witness: Value) -> Self {
        self.witness = Some(witness);
        self
    }

    /// Builder: attach a set of 1-indexed clause positions.
    pub fn with_clauses(mut self, clauses: Vec<usize>) -> Self {
        self.clauses = Some(clauses);
        self
    }

    /// Builder: attach the offending variable name.
    pub fn with_variable(mut self, variable: impl Into<String>) -> Self {
        self.variable = Some(variable.into());
        self
    }

    /// Builder: attach the offending identifier name.
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }
}
