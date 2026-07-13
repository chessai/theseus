//! Source spans and the surface abstract syntax tree.
//!
//! Every AST node carries a [`Span`] covering the full extent of the syntax it
//! was parsed from. Spans are byte offsets into the original source string and
//! are half-open (`start..end`), so they can be used directly as
//! `core::ops::Range<usize>` for diagnostic rendering (that rendering lives in
//! `theseus-check`, per the test spec).

use std::ops::Range;

/// A half-open byte span `[start, end)` into a source string.
///
/// Spans MUST cover the full offending token so downstream diagnostics can
/// underline exactly the relevant source (test-spec-m1, "Parser: done means").
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    #[must_use]
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// The smallest span covering both `self` and `other`.
    #[must_use]
    pub fn merge(self, other: Span) -> Span {
        Span::new(self.start.min(other.start), self.end.max(other.end))
    }
}

impl From<Span> for Range<usize> {
    fn from(s: Span) -> Self {
        s.start..s.end
    }
}

impl From<Range<usize>> for Span {
    fn from(r: Range<usize>) -> Self {
        Span::new(r.start, r.end)
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

/// An identifier beginning with a lower-case letter: an iso-name or a variable
/// (ML-style case discipline, grammar.md normative disambiguation).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LowerName {
    pub text: String,
    pub span: Span,
}

/// An identifier beginning with an upper-case letter: a type name or a data
/// constructor (ML-style case discipline).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UpperName {
    pub text: String,
    pub span: Span,
}

/// A whole source unit: a sequence of top-level declarations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub span: Span,
}

/// A top-level declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decl {
    /// A data-type declaration: `type Name = C1 T | C2 T | ...`.
    Type(TypeDecl),
    /// An iso definition: `iso name : T <-> U = body` or a clause body.
    Iso(IsoDecl),
    /// A primitive declaration: `prim name : T <-> U = "key"`.
    Prim(PrimDecl),
}

impl Decl {
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Decl::Type(d) => d.span,
            Decl::Iso(d) => d.span,
            Decl::Prim(d) => d.span,
        }
    }
}

/// `type Name tyvar* = Ctor1 [Type] | Ctor2 [Type] | ...`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeDecl {
    pub name: UpperName,
    /// Type parameters (`lowerName`), possibly empty.
    pub params: Vec<LowerName>,
    pub ctors: Vec<Ctor>,
    pub span: Span,
}

/// A single constructor of a data type: a name and an optional payload type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ctor {
    pub name: UpperName,
    pub payload: Option<Type>,
    pub span: Span,
}

/// `iso name : Type <-> Type isobody`, where the body is either a set of
/// pattern-matching clauses or `= expr`, optionally followed by a `where`
/// block of nested isos (grammar.md `isobody`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IsoDecl {
    pub name: LowerName,
    pub lhs: Type,
    pub rhs: Type,
    pub body: IsoBody,
    /// Nested isos introduced by a `where` block (empty if none).
    pub wheres: Vec<IsoDecl>,
    pub span: Span,
}

/// The body of an [`IsoDecl`]: either clauses or an `= expr` definition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IsoBody {
    /// One or more `| pattern <-> pattern` clauses.
    Clauses(Vec<Clause>),
    /// An `= expr` combinator body.
    Expr(Expr),
}

/// A single rewrite clause `| lhs <-> rhs`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Clause {
    pub lhs: Pattern,
    pub rhs: Pattern,
    pub span: Span,
}

/// A `prim name : Type <-> Type = "key"` declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimDecl {
    pub name: LowerName,
    pub lhs: Type,
    pub rhs: Type,
    /// The registry key string literal (contents without the quotes).
    pub key: String,
    pub span: Span,
}

/// A pattern (grammar.md `pattern` / `atomPat`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern {
    /// A variable binding `lowerName`.
    Var(LowerName),
    /// A nullary constructor `Upper`, or an applied constructor `Upper p*`.
    Ctor(UpperName, Vec<Pattern>, Span),
    /// A numeric literal `Nat`.
    Nat(u128, Span),
    /// A label pattern `$name p*` (e.g. `$go`, `$go b`, `$go (b, (O, O))`).
    Label(LowerName, Vec<Pattern>, Span),
    /// An n-tuple pattern `(p1, ..., pn)` with n >= 2, nesting right.
    Tuple(Box<Pattern>, Box<Pattern>, Span),
    /// A parenthesized pattern (transparent; span covers the parens).
    Paren(Box<Pattern>, Span),
}

impl Pattern {
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Pattern::Var(n) => n.span,
            Pattern::Ctor(_, _, s)
            | Pattern::Nat(_, s)
            | Pattern::Label(_, _, s)
            | Pattern::Tuple(_, _, s)
            | Pattern::Paren(_, s) => *s,
        }
    }
}

/// A type. Types mirror the term operators structurally: `*` for products,
/// `+` for sums (grammar.md). `*` binds tighter than `+`; both are
/// left-associative.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// A named type or type constructor, e.g. `Bool`, `Nat`.
    Name(UpperName),
    /// A type parameter reference (`lowerName`), used inside a `typedecl`.
    Var(LowerName),
    /// The empty / void type `0`.
    Empty(Span),
    /// The unit type `1`.
    Unit(Span),
    /// A product type `A * B` (left-associative).
    Prod(Box<Type>, Box<Type>, Span),
    /// A sum type `A + B` (left-associative).
    Sum(Box<Type>, Box<Type>, Span),
    /// A parenthesized type (kept transparently; span covers the parens).
    Paren(Box<Type>, Span),
}

impl Type {
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Type::Name(n) => n.span,
            Type::Var(n) => n.span,
            Type::Empty(s)
            | Type::Unit(s)
            | Type::Prod(_, _, s)
            | Type::Sum(_, _, s)
            | Type::Paren(_, s) => *s,
        }
    }
}

/// The infix binary operators over expressions, in the frozen precedence
/// order (grammar.md): `*` tightest of the binaries, then `+`, then `;`
/// loosest. All are left-associative.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    /// Product / parallel composition `*`.
    Prod,
    /// Sum / choice `+`.
    Sum,
    /// Sequential composition `;`.
    Seq,
}

impl BinOp {
    /// Binding power; a larger number binds tighter. `*` > `+` > `;`.
    #[must_use]
    pub fn binding_power(self) -> u8 {
        match self {
            BinOp::Prod => 3,
            BinOp::Sum => 2,
            BinOp::Seq => 1,
        }
    }

    #[must_use]
    pub fn glyph(self) -> &'static str {
        match self {
            BinOp::Prod => "*",
            BinOp::Sum => "+",
            BinOp::Seq => ";",
        }
    }
}

/// The v0.2 keyword combinators (ADR-0038): each takes a counter/spine
/// TYPE argument and a body iso expression, and denotes the corresponding
/// core primitive (`Iso::Iter` / `Iso::Fold` / `Iso::Unfold`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CombKind {
    /// `iter n f : n * a <-> n * a` — bounded iteration (ADR-0036).
    Iter,
    /// `fold l f : l * c <-> l * c` — reversible mapAccumL (ADR-0037).
    Fold,
    /// `unfold l f : l * c <-> l * c` — the tail-to-head dual of `fold`.
    Unfold,
}

impl CombKind {
    #[must_use]
    pub fn keyword(self) -> &'static str {
        match self {
            CombKind::Iter => "iter",
            CombKind::Fold => "fold",
            CombKind::Unfold => "unfold",
        }
    }
}

/// A surface expression (an iso combinator term).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    /// A reference to an iso by name.
    Var(LowerName),
    /// A data constructor used as an iso combinator, e.g. `Cons`.
    Ctor(UpperName),
    /// The identity iso literal `id`. (Represented distinctly for clarity;
    /// it lexes as a keyword.)
    Id(Span),
    /// The adjoint / inverse of an expression: `~e` (equivalently `†e`). The
    /// `~` operator binds tightest of all (grammar.md).
    Adjoint(Box<Expr>, Span),
    /// An infix binary application `lhs OP rhs`.
    Bin(BinOp, Box<Expr>, Box<Expr>, Span),
    /// An n-tuple `(e1, e2, ..., en)` with n >= 2. Tuples nest to the right:
    /// `(a, b, c) = (a, (b, c))` (grammar.md normative disambiguation).
    Tuple(Box<Expr>, Box<Expr>, Span),
    /// The unit expression `()`.
    Unit(Span),
    /// A parenthesized expression; the span covers the parentheses.
    Paren(Box<Expr>, Span),
    /// A v0.2 combinator form `iter T e` / `fold T e` / `unfold T e`
    /// (ADR-0038): a keyword applied to a counter/spine type atom and a body
    /// expression atom. The whole form is an expression atom, so it composes
    /// with `;` `*` `+` and `~` without parentheses; a compound BODY needs
    /// them (`iter Nat (f ; g)`).
    Combinator(CombKind, Box<Type>, Box<Expr>, Span),
}

impl Expr {
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Expr::Var(n) => n.span,
            Expr::Ctor(n) => n.span,
            Expr::Id(s)
            | Expr::Adjoint(_, s)
            | Expr::Bin(_, _, _, s)
            | Expr::Tuple(_, _, s)
            | Expr::Unit(s)
            | Expr::Paren(_, s)
            | Expr::Combinator(_, _, _, s) => *s,
        }
    }
}
