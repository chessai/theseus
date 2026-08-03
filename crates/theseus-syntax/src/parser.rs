//! A hand-written recursive-descent parser producing the surface AST.
//!
//! The binary-operator precedence is the frozen table (grammar.md):
//! `~` (prefix, tightest) > `*` > `+` > `;` (loosest). All infix operators are
//! left-associative. Tuples `(a, b, c)` nest to the right.

use std::collections::BTreeMap;

use crate::ast::{
    BinOp, Clause, CombKind, Ctor, Decl, Expr, IsoBody, IsoDecl, LowerName, Pattern, PrimDecl,
    Program, Span, Type, TypeDecl, UpperName,
};
use crate::lexer::{lex, LexError, Spanned, Tok};

/// A parse error with error code `T0001` (structured rendering is
/// theseus-check's job; here we just carry the code, a message, and a span).
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("[{code}] {message} at {span:?}")]
pub struct ParseError {
    pub code: &'static str,
    pub message: String,
    pub span: Span,
    /// The underlying lexical error, when this parse error was produced by
    /// lexing (populated by `From<LexError>`). Carried as `#[source]` so the
    /// error chain reaches the original token span; `None` for parse-stage
    /// errors. Skipped by Display -- the message/span above are the surface.
    #[source]
    source: Option<LexError>,
}

impl ParseError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            code: "T0001",
            message: message.into(),
            span,
            source: None,
        }
    }
}

impl From<LexError> for ParseError {
    fn from(e: LexError) -> Self {
        ParseError {
            code: "T0001",
            message: "unrecognized token".into(),
            span: e.span,
            source: Some(e),
        }
    }
}

/// Parse a whole program from source text.
pub fn parse(src: &str) -> Result<Program, ParseError> {
    let tokens = lex(src)?;
    let mut p = Parser {
        toks: tokens,
        pos: 0,
        src_len: src.len(),
        depth_overrides: BTreeMap::new(),
    };
    p.program()
}

struct Parser {
    toks: Vec<Spanned>,
    pos: usize,
    src_len: usize,
    /// Depth annotations parsed from `Name@depth` syntax (ADR-0035 follow-up).
    depth_overrides: BTreeMap<usize, usize>,
}

impl Parser {
    fn peek(&self) -> Option<&Tok> {
        self.toks.get(self.pos).map(|s| &s.tok)
    }

    fn peek_span(&self) -> Span {
        match self.toks.get(self.pos) {
            Some(s) => s.span,
            None => Span::new(self.src_len, self.src_len),
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.toks.len()
    }

    fn bump(&mut self) -> Option<Spanned> {
        let t = self.toks.get(self.pos).cloned();
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    fn eat(&mut self, want: &Tok) -> bool {
        if self.peek() == Some(want) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn expect(&mut self, want: &Tok, what: &str) -> Result<Span, ParseError> {
        let span = self.peek_span();
        if self.eat(want) {
            Ok(span)
        } else {
            Err(ParseError::new(format!("expected {what}"), span))
        }
    }

    // ---- program ----

    fn program(&mut self) -> Result<Program, ParseError> {
        let mut decls = Vec::new();
        while !self.at_end() {
            decls.push(self.decl()?);
        }
        let span = match (decls.first(), decls.last()) {
            (Some(f), Some(l)) => f.span().merge(l.span()),
            _ => Span::new(0, self.src_len),
        };
        let depth_overrides = std::mem::take(&mut self.depth_overrides);
        Ok(Program { decls, span, depth_overrides })
    }

    fn decl(&mut self) -> Result<Decl, ParseError> {
        match self.peek() {
            Some(Tok::Type) => Ok(Decl::Type(self.type_decl()?)),
            Some(Tok::Iso) => Ok(Decl::Iso(self.iso_decl()?)),
            Some(Tok::Prim) => Ok(Decl::Prim(self.prim_decl()?)),
            _ => Err(ParseError::new(
                "expected a declaration (`type`, `iso`, or `prim`)",
                self.peek_span(),
            )),
        }
    }

    // ---- type declaration ----

    fn type_decl(&mut self) -> Result<TypeDecl, ParseError> {
        let kw = self.expect(&Tok::Type, "`type`")?;
        let name = self.upper_name()?;
        // Type parameters: zero or more `lowerName`s before the `=`.
        let mut params = Vec::new();
        while matches!(self.peek(), Some(Tok::Lower(_))) {
            params.push(self.lower_name()?);
        }
        self.expect(&Tok::Eq, "`=`")?;
        let mut ctors = Vec::new();
        ctors.push(self.ctor()?);
        while self.eat(&Tok::Bar) {
            ctors.push(self.ctor()?);
        }
        let last = ctors.last().map_or(name.span, |c| c.span);
        let span = kw.merge(last);
        Ok(TypeDecl {
            name,
            params,
            ctors,
            span,
        })
    }

    fn ctor(&mut self) -> Result<Ctor, ParseError> {
        let name = self.upper_name()?;
        // Optional payload: a type atom immediately following the constructor.
        let payload = if self.starts_type_atom() {
            Some(self.type_atom()?)
        } else {
            None
        };
        let span = match &payload {
            Some(t) => name.span.merge(t.span()),
            None => name.span,
        };
        Ok(Ctor {
            name,
            payload,
            span,
        })
    }

    // ---- iso declaration ----

    fn iso_decl(&mut self) -> Result<IsoDecl, ParseError> {
        let kw = self.expect(&Tok::Iso, "`iso`")?;
        let name = self.lower_name()?;
        self.expect(&Tok::Colon, "`:`")?;
        let lhs = self.ty()?;
        self.expect(&Tok::Arrow, "`<->`")?;
        let rhs = self.ty()?;

        // isobody ::= clauses where? | "=" expr where?
        let (body, body_end) = if self.peek() == Some(&Tok::Eq) {
            self.bump();
            let e = self.expr()?;
            let end = e.span();
            (IsoBody::Expr(e), end)
        } else if self.peek() == Some(&Tok::Bar) {
            let clauses = self.clauses()?;
            let end = clauses
                .last()
                .map_or(rhs.span(), |c| c.span);
            (IsoBody::Clauses(clauses), end)
        } else {
            return Err(ParseError::new(
                "expected `=` or a `|` clause after the iso signature",
                self.peek_span(),
            ));
        };

        // where ::= "where" isodecl+
        let mut wheres = Vec::new();
        let mut end = body_end;
        if self.eat(&Tok::Where) {
            // At least one nested isodecl.
            if self.peek() != Some(&Tok::Iso) {
                return Err(ParseError::new(
                    "expected an `iso` declaration after `where`",
                    self.peek_span(),
                ));
            }
            while self.peek() == Some(&Tok::Iso) {
                let nested = self.iso_decl()?;
                end = nested.span;
                wheres.push(nested);
            }
        }

        let span = kw.merge(end);
        Ok(IsoDecl {
            name,
            lhs,
            rhs,
            body,
            wheres,
            span,
        })
    }

    /// `clauses ::= ("|" pattern "<->" pattern)+`.
    fn clauses(&mut self) -> Result<Vec<Clause>, ParseError> {
        let mut clauses = Vec::new();
        while self.peek() == Some(&Tok::Bar) {
            let bar = self.peek_span();
            self.bump();
            let lhs = self.pattern()?;
            self.expect(&Tok::Arrow, "`<->`")?;
            let rhs = self.pattern()?;
            let span = bar.merge(rhs.span());
            clauses.push(Clause { lhs, rhs, span });
        }
        Ok(clauses)
    }

    // ---- prim declaration ----

    fn prim_decl(&mut self) -> Result<PrimDecl, ParseError> {
        let kw = self.expect(&Tok::Prim, "`prim`")?;
        let name = self.lower_name()?;
        self.expect(&Tok::Colon, "`:`")?;
        let lhs = self.ty()?;
        self.expect(&Tok::Arrow, "`<->`")?;
        let rhs = self.ty()?;
        self.expect(&Tok::Eq, "`=`")?;
        let (key, key_span) = self.string_lit()?;
        let span = kw.merge(key_span);
        Ok(PrimDecl {
            name,
            lhs,
            rhs,
            key,
            span,
        })
    }

    fn string_lit(&mut self) -> Result<(String, Span), ParseError> {
        let span = self.peek_span();
        if matches!(self.peek(), Some(Tok::Str(_))) {
            let Some(Spanned {
                tok: Tok::Str(text),
                span,
            }) = self.bump()
            else {
                unreachable!("peeked Str")
            };
            Ok((text, span))
        } else {
            Err(ParseError::new("expected a string literal", span))
        }
    }

    // ---- patterns ----

    /// `pattern ::= UpperName atomPat* | "$" lowerName atomPat* | atomPat`.
    fn pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek() {
            Some(Tok::Upper(_)) => {
                let name = self.upper_name()?;
                let mut args = Vec::new();
                while self.starts_atom_pat() {
                    args.push(self.atom_pat()?);
                }
                let span = args
                    .last()
                    .map_or(name.span, |a| name.span.merge(a.span()));
                Ok(Pattern::Ctor(name, args, span))
            }
            Some(Tok::Dollar) => {
                let dollar = self.peek_span();
                self.bump();
                let name = self.lower_name()?;
                let mut args = Vec::new();
                while self.starts_atom_pat() {
                    args.push(self.atom_pat()?);
                }
                let span = args
                    .last()
                    .map_or(dollar.merge(name.span), |a| dollar.merge(a.span()));
                Ok(Pattern::Label(name, args, span))
            }
            _ => self.atom_pat(),
        }
    }

    fn starts_atom_pat(&self) -> bool {
        matches!(
            self.peek(),
            Some(Tok::Lower(_)) | Some(Tok::Upper(_)) | Some(Tok::Nat(_)) | Some(Tok::LParen)
        )
    }

    /// `atomPat ::= lowerName | UpperName | Nat | "(" pattern ("," pattern)* ")"`.
    fn atom_pat(&mut self) -> Result<Pattern, ParseError> {
        match self.peek() {
            Some(Tok::Lower(_)) => Ok(Pattern::Var(self.lower_name()?)),
            Some(Tok::Upper(_)) => {
                let name = self.upper_name()?;
                Ok(Pattern::Ctor(name.clone(), Vec::new(), name.span))
            }
            Some(Tok::Nat(_)) => {
                let (n, span) = self.nat_lit()?;
                Ok(Pattern::Nat(n, span))
            }
            Some(Tok::LParen) => self.pattern_paren_or_tuple(),
            _ => Err(ParseError::new("expected a pattern", self.peek_span())),
        }
    }

    fn nat_lit(&mut self) -> Result<(u128, Span), ParseError> {
        let span = self.peek_span();
        if let Some(Tok::Nat(n)) = self.peek() {
            let n = *n;
            self.bump();
            Ok((n, span))
        } else {
            Err(ParseError::new("expected a numeric literal", span))
        }
    }

    /// Parse `( p )` (grouping) or `( p1 , p2 , ... )` (tuple, nesting right).
    fn pattern_paren_or_tuple(&mut self) -> Result<Pattern, ParseError> {
        let open = self.expect(&Tok::LParen, "`(`")?;
        let first = self.pattern()?;
        if self.peek() == Some(&Tok::RParen) {
            let close = self.peek_span();
            self.bump();
            return Ok(Pattern::Paren(Box::new(first), open.merge(close)));
        }
        let mut elems = vec![first];
        while self.eat(&Tok::Comma) {
            elems.push(self.pattern()?);
        }
        let close = self.expect(&Tok::RParen, "`,` or `)`")?;
        let whole = open.merge(close);
        // Fold right: (a, b, c) = (a, (b, c)).
        let mut iter = elems.into_iter().rev();
        let last = iter.next().expect("at least two tuple elements");
        let mut acc = last;
        for e in iter {
            let span = e.span().merge(acc.span());
            acc = Pattern::Tuple(Box::new(e), Box::new(acc), span);
        }
        Ok(match acc {
            Pattern::Tuple(l, r, _) => Pattern::Tuple(l, r, whole),
            other => other,
        })
    }

    // ---- types: `+` (loosest) then `*` then atom; left-assoc ----

    fn ty(&mut self) -> Result<Type, ParseError> {
        self.type_sum()
    }

    fn type_sum(&mut self) -> Result<Type, ParseError> {
        let mut lhs = self.type_prod()?;
        while self.peek() == Some(&Tok::Plus) {
            self.bump();
            let rhs = self.type_prod()?;
            let span = lhs.span().merge(rhs.span());
            lhs = Type::Sum(Box::new(lhs), Box::new(rhs), span);
        }
        Ok(lhs)
    }

    fn type_prod(&mut self) -> Result<Type, ParseError> {
        let mut lhs = self.type_atom()?;
        while self.peek() == Some(&Tok::Star) {
            self.bump();
            let rhs = self.type_atom()?;
            let span = lhs.span().merge(rhs.span());
            lhs = Type::Prod(Box::new(lhs), Box::new(rhs), span);
        }
        Ok(lhs)
    }

    fn starts_type_atom(&self) -> bool {
        matches!(
            self.peek(),
            Some(Tok::Upper(_)) | Some(Tok::Lower(_)) | Some(Tok::Nat(_)) | Some(Tok::LParen)
        )
    }

    fn type_atom(&mut self) -> Result<Type, ParseError> {
        match self.peek() {
            Some(Tok::Upper(_)) => {
                let name = self.upper_name()?;
                if self.peek() == Some(&Tok::At) {
                    let at_span = self.peek_span();
                    self.bump();
                    let (depth, _depth_span) = self.nat_lit().map_err(|_| {
                        ParseError::new("expected a depth literal after `@`", at_span)
                    })?;
                    self.depth_overrides.insert(name.span.start, depth as usize);
                }
                Ok(Type::Name(name))
            }
            Some(Tok::Lower(_)) => Ok(Type::Var(self.lower_name()?)),
            Some(Tok::Nat(n)) => {
                let s = self.peek_span();
                match n {
                    0 => {
                        self.bump();
                        Ok(Type::Empty(s))
                    }
                    1 => {
                        self.bump();
                        Ok(Type::Unit(s))
                    }
                    _ => Err(ParseError::new(
                        "expected a type (only `0` and `1` are numeric types)",
                        s,
                    )),
                }
            }
            Some(Tok::LParen) => {
                let open = self.peek_span();
                self.bump();
                let first = self.ty()?;
                if self.peek() == Some(&Tok::RParen) {
                    let close = self.peek_span();
                    self.bump();
                    return Ok(Type::Paren(Box::new(first), open.merge(close)));
                }
                // A parenthesized list of >= 2 elements is a right-nested
                // product (grammar.md: tuples nest right-associatively).
                let mut elems = vec![first];
                while self.eat(&Tok::Comma) {
                    elems.push(self.ty()?);
                }
                let close = self.expect(&Tok::RParen, "`,` or `)`")?;
                let whole = open.merge(close);
                let mut iter = elems.into_iter().rev();
                let last = iter.next().expect("at least two product elements");
                let mut acc = last;
                for e in iter {
                    let span = e.span().merge(acc.span());
                    acc = Type::Prod(Box::new(e), Box::new(acc), span);
                }
                // Wrap the whole product in a Paren carrying the paren span, so
                // the pretty-printed form (which re-parenthesizes) round-trips.
                Ok(Type::Paren(Box::new(acc), whole))
            }
            _ => Err(ParseError::new("expected a type", self.peek_span())),
        }
    }

    // ---- expressions: `;` (loosest) then `+` then `*` then `~` then atom ----

    fn expr(&mut self) -> Result<Expr, ParseError> {
        self.expr_bp(0)
    }

    /// Pratt-style precedence climbing. `min_bp` is the minimum binding power
    /// an infix operator must exceed to be consumed at this level. All infix
    /// operators are left-associative, so the right operand is parsed with
    /// `min_bp = op.binding_power()` (strictly greater to bind on the right).
    fn expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.expr_prefix()?;
        loop {
            let op = match self.peek() {
                Some(Tok::Star) => BinOp::Prod,
                Some(Tok::Plus) => BinOp::Sum,
                Some(Tok::Semi) => BinOp::Seq,
                _ => break,
            };
            let bp = op.binding_power();
            if bp <= min_bp {
                break;
            }
            self.bump();
            let rhs = self.expr_bp(bp)?;
            let span = lhs.span().merge(rhs.span());
            lhs = Expr::Bin(op, Box::new(lhs), Box::new(rhs), span);
        }
        Ok(lhs)
    }

    /// The prefix `~` / `†` adjoint operator, tighter than every infix op.
    fn expr_prefix(&mut self) -> Result<Expr, ParseError> {
        if self.peek() == Some(&Tok::Tilde) {
            let open = self.peek_span();
            self.bump();
            let inner = self.expr_prefix()?;
            let span = open.merge(inner.span());
            Ok(Expr::Adjoint(Box::new(inner), span))
        } else {
            self.expr_atom()
        }
    }

    fn expr_atom(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Some(Tok::Lower(_)) => Ok(Expr::Var(self.lower_name()?)),
            Some(Tok::Upper(_)) => Ok(Expr::Ctor(self.upper_name()?)),
            Some(Tok::Id) => {
                let s = self.peek_span();
                self.bump();
                Ok(Expr::Id(s))
            }
            Some(Tok::LParen) => self.expr_paren_or_tuple(),
            Some(Tok::Iter | Tok::Fold | Tok::Unfold) => self.combinator(),
            Some(Tok::For) => Err(ParseError::new(
                "`for` is a reserved word (grammar.md) with no production",
                self.peek_span(),
            )),
            _ => Err(ParseError::new("expected an expression", self.peek_span())),
        }
    }

    /// `comb ::= ("iter" | "fold" | "unfold") atomTy atomExpr` (ADR-0038).
    ///
    /// Both arguments are atoms: the counter/spine type is a `type_atom`
    /// (typically the μ-type's name) and the body is an `expr_atom`, so a
    /// compound body must be parenthesized (`iter Nat (f ; g)`). The whole
    /// form is itself an expression atom: it composes with the infix
    /// operators and prefix `~` without parentheses (`~iter Nat f` is the
    /// adjoint of the whole iteration).
    fn combinator(&mut self) -> Result<Expr, ParseError> {
        let kw_span = self.peek_span();
        let kind = match self.peek() {
            Some(Tok::Iter) => CombKind::Iter,
            Some(Tok::Fold) => CombKind::Fold,
            Some(Tok::Unfold) => CombKind::Unfold,
            _ => unreachable!("combinator() called off a combinator keyword"),
        };
        self.bump();
        let ty = self.type_atom()?;
        let body = self.expr_atom()?;
        let span = kw_span.merge(body.span());
        Ok(Expr::Combinator(kind, Box::new(ty), Box::new(body), span))
    }

    /// Parse `()`, `( e )`, or an n-tuple `( e1 , e2 , ... )`. Tuples nest to
    /// the right: `(a, b, c) = (a, (b, c))`.
    fn expr_paren_or_tuple(&mut self) -> Result<Expr, ParseError> {
        let open = self.expect(&Tok::LParen, "`(`")?;
        // Empty parens -> unit.
        if self.peek() == Some(&Tok::RParen) {
            let close = self.peek_span();
            self.bump();
            return Ok(Expr::Unit(open.merge(close)));
        }
        let first = self.expr()?;
        if self.peek() == Some(&Tok::RParen) {
            let close = self.peek_span();
            self.bump();
            return Ok(Expr::Paren(Box::new(first), open.merge(close)));
        }
        // At least one comma: gather the remaining elements.
        let mut elems = vec![first];
        while self.eat(&Tok::Comma) {
            elems.push(self.expr()?);
        }
        let close = self.expect(&Tok::RParen, "`,` or `)`")?;
        let whole = open.merge(close);
        // Fold right: (a, b, c) = (a, (b, c)). The outermost Tuple node carries
        // the paren-covering span; inner right-nested tuples reuse it so that
        // re-parsing the pretty-printed form reconstructs the same spans.
        let mut iter = elems.into_iter().rev();
        let last = iter.next().expect("at least two tuple elements");
        let mut acc = last;
        for e in iter {
            let span = e.span().merge(acc.span());
            acc = Expr::Tuple(Box::new(e), Box::new(acc), span);
        }
        // Replace the outermost span with the paren-covering span.
        Ok(match acc {
            Expr::Tuple(l, r, _) => Expr::Tuple(l, r, whole),
            other => other,
        })
    }

    // ---- names ----

    fn upper_name(&mut self) -> Result<UpperName, ParseError> {
        let span = self.peek_span();
        match self.peek() {
            Some(Tok::Upper(_)) => {
                let Some(Spanned {
                    tok: Tok::Upper(text),
                    span,
                }) = self.bump()
                else {
                    unreachable!("peeked Upper")
                };
                Ok(UpperName { text, span })
            }
            _ => Err(ParseError::new(
                "expected an upper-case name (type or constructor)",
                span,
            )),
        }
    }

    fn lower_name(&mut self) -> Result<LowerName, ParseError> {
        let span = self.peek_span();
        match self.peek() {
            Some(Tok::Lower(_)) => {
                let Some(Spanned {
                    tok: Tok::Lower(text),
                    span,
                }) = self.bump()
                else {
                    unreachable!("peeked Lower")
                };
                Ok(LowerName { text, span })
            }
            _ => Err(ParseError::new(
                "expected a lower-case name (iso-name or variable)",
                span,
            )),
        }
    }
}
