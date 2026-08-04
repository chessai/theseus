//! The pretty-printer: renders the surface AST back to canonical source text.
//!
//! Canonical choices (grammar.md / task):
//! * `~` is emitted for adjoint (never `†`).
//! * Parentheses are inserted only where the frozen precedence/associativity
//!   table would otherwise change the parse. Because every infix operator is
//!   left-associative, a right operand of equal precedence is parenthesized;
//!   a left operand of equal precedence is not.
//! * Tuples print flat: `(a, b, c)`.
//!
//! Round-trip note: `parse ∘ pretty ≡ id` holds up to source spans and the
//! transparent [`Expr::Paren`] / [`Type::Paren`] wrappers, which carry no
//! semantic content. Use [`normalize_program`] to erase both before comparing.

use std::collections::BTreeMap;

use crate::ast::{
    Clause, Ctor, Decl, Expr, IsoBody, IsoDecl, LowerName, Pattern, PrimDecl, Program, Span, Type,
    TypeDecl, UpperName,
};

/// Depth-override map: `span.start → depth`. Passed through printing so that
/// `Name@depth` annotations round-trip correctly (ADR-0035 follow-up).
type DepthMap = BTreeMap<usize, usize>;

/// Render a whole program to canonical surface text.
#[must_use]
pub fn pretty_program(p: &Program) -> String {
    let mut out = String::new();
    let dm = &p.depth_overrides;
    for (i, d) in p.decls.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        pretty_decl(d, dm, &mut out);
        out.push('\n');
    }
    out
}

/// Render a single expression to canonical surface text.
#[must_use]
pub fn pretty_expr(e: &Expr) -> String {
    let dm = BTreeMap::new();
    let mut out = String::new();
    print_expr(e, 0, &dm, &mut out);
    out
}

/// Render a single type to canonical surface text.
#[must_use]
pub fn pretty_type(t: &Type) -> String {
    let dm = BTreeMap::new();
    let mut out = String::new();
    print_type(t, 0, &dm, &mut out);
    out
}

fn pretty_decl(d: &Decl, dm: &DepthMap, out: &mut String) {
    match d {
        Decl::Type(t) => print_type_decl(t, dm, out),
        Decl::Iso(i) => print_iso_decl(i, 0, dm, out),
        Decl::Prim(p) => print_prim_decl(p, dm, out),
    }
}

fn print_type_decl(t: &TypeDecl, dm: &DepthMap, out: &mut String) {
    out.push_str("type ");
    out.push_str(&t.name.text);
    for p in &t.params {
        out.push(' ');
        out.push_str(&p.text);
    }
    out.push_str(" = ");
    for (i, c) in t.ctors.iter().enumerate() {
        if i > 0 {
            out.push_str(" | ");
        }
        print_ctor(c, dm, out);
    }
}

fn print_prim_decl(p: &PrimDecl, dm: &DepthMap, out: &mut String) {
    out.push_str("prim ");
    out.push_str(&p.name.text);
    out.push_str(" : ");
    print_type(&p.lhs, 0, dm, out);
    out.push_str(" <-> ");
    print_type(&p.rhs, 0, dm, out);
    out.push_str(" = \"");
    out.push_str(&p.key);
    out.push('"');
}

fn print_ctor(c: &Ctor, dm: &DepthMap, out: &mut String) {
    out.push_str(&c.name.text);
    if let Some(p) = &c.payload {
        out.push(' ');
        // A constructor payload is a type atom; parenthesize compound types.
        print_type(p, TYPE_ATOM, dm, out);
    }
}

fn print_iso_decl(i: &IsoDecl, indent: usize, dm: &DepthMap, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&pad);
    out.push_str("iso ");
    out.push_str(&i.name.text);
    out.push_str(" : ");
    print_type(&i.lhs, 0, dm, out);
    out.push_str(" <-> ");
    print_type(&i.rhs, 0, dm, out);
    match &i.body {
        IsoBody::Expr(e) => {
            out.push_str(" = ");
            print_expr(e, 0, dm, out);
        }
        IsoBody::Clauses(cs) => {
            for c in cs {
                out.push('\n');
                out.push_str(&pad);
                print_clause(c, out);
            }
        }
    }
    if !i.wheres.is_empty() {
        out.push('\n');
        out.push_str(&pad);
        out.push_str("where");
        for w in &i.wheres {
            out.push('\n');
            print_iso_decl(w, indent + 1, dm, out);
        }
    }
}

fn print_clause(c: &Clause, out: &mut String) {
    out.push_str("| ");
    print_pattern(&c.lhs, out);
    out.push_str(" <-> ");
    print_pattern(&c.rhs, out);
}

fn print_pattern(p: &Pattern, out: &mut String) {
    match p {
        Pattern::Var(n) => out.push_str(&n.text),
        Pattern::Nat(n, _) => {
            out.push_str(&n.to_string());
        }
        Pattern::Paren(inner, _) => print_pattern(inner, out),
        Pattern::Ctor(name, args, _) => {
            out.push_str(&name.text);
            for a in args {
                out.push(' ');
                print_atom_pattern(a, out);
            }
        }
        Pattern::Label(name, args, _) => {
            out.push('$');
            out.push_str(&name.text);
            for a in args {
                out.push(' ');
                print_atom_pattern(a, out);
            }
        }
        Pattern::Tuple(..) => {
            out.push('(');
            print_tuple_pattern_elems(p, out);
            out.push(')');
        }
    }
}

/// Print a pattern in `atomPat` position: applied constructors and labels need
/// parentheses so re-parsing keeps them as a single argument.
fn print_atom_pattern(p: &Pattern, out: &mut String) {
    match p {
        Pattern::Ctor(_, args, _) if !args.is_empty() => {
            out.push('(');
            print_pattern(p, out);
            out.push(')');
        }
        Pattern::Label(..) => {
            out.push('(');
            print_pattern(p, out);
            out.push(')');
        }
        _ => print_pattern(p, out),
    }
}

fn print_tuple_pattern_elems(p: &Pattern, out: &mut String) {
    match p {
        Pattern::Tuple(l, r, _) => {
            print_pattern(l, out);
            out.push_str(", ");
            print_tuple_pattern_elems(r, out);
        }
        other => print_pattern(other, out),
    }
}

// Type precedence levels: sum = 1, prod = 2, atom = 3.
const TYPE_SUM: u8 = 1;
const TYPE_PROD: u8 = 2;
const TYPE_ATOM: u8 = 3;

fn print_type(t: &Type, min: u8, dm: &DepthMap, out: &mut String) {
    match t {
        Type::Name(n) => {
            out.push_str(&n.text);
            if let Some(depth) = dm.get(&n.span.start) {
                out.push('@');
                out.push_str(&depth.to_string());
            }
        }
        Type::Var(n) => out.push_str(&n.text),
        Type::Empty(_) => out.push('0'),
        Type::Unit(_) => out.push('1'),
        Type::Paren(inner, _) => print_type(inner, min, dm, out),
        Type::Prod(l, r, _) => {
            let paren = min > TYPE_PROD;
            if paren {
                out.push('(');
            }
            // Left-assoc: left child at same level, right child one tighter.
            print_type(l, TYPE_PROD, dm, out);
            out.push_str(" * ");
            print_type(r, TYPE_PROD + 1, dm, out);
            if paren {
                out.push(')');
            }
        }
        Type::Sum(l, r, _) => {
            let paren = min > TYPE_SUM;
            if paren {
                out.push('(');
            }
            print_type(l, TYPE_SUM, dm, out);
            out.push_str(" + ");
            print_type(r, TYPE_SUM + 1, dm, out);
            if paren {
                out.push(')');
            }
        }
    }
}

// Expression precedence: `;` = 1, `+` = 2, `*` = 3, `~` = 4.
const EXPR_ADJOINT: u8 = 4;

fn print_expr(e: &Expr, min: u8, dm: &DepthMap, out: &mut String) {
    match e {
        Expr::Var(n) => out.push_str(&n.text),
        Expr::Ctor(n) => out.push_str(&n.text),
        Expr::Id(_) => out.push_str("id"),
        Expr::Unit(_) => out.push_str("()"),
        Expr::Paren(inner, _) => print_expr(inner, min, dm, out),
        Expr::Adjoint(inner, _) => {
            let paren = min > EXPR_ADJOINT;
            if paren {
                out.push('(');
            }
            out.push('~');
            // The operand binds at least as tight as `~`; prefix chains fine.
            print_expr(inner, EXPR_ADJOINT, dm, out);
            if paren {
                out.push(')');
            }
        }
        Expr::Bin(op, l, r, _) => {
            let bp = op.binding_power();
            let paren = min > bp;
            if paren {
                out.push('(');
            }
            // Left-assoc: left child at same bp, right child one tighter.
            print_expr(l, bp, dm, out);
            out.push(' ');
            out.push_str(op.glyph());
            out.push(' ');
            print_expr(r, bp + 1, dm, out);
            if paren {
                out.push(')');
            }
        }
        Expr::Tuple(..) => {
            out.push('(');
            print_tuple_elems(e, dm, out);
            out.push(')');
        }
        Expr::Combinator(kind, ty, body, _) => {
            // The whole form is an expression atom (ADR-0038): it never
            // needs surrounding parentheses, but its arguments are atoms.
            out.push_str(kind.keyword());
            out.push(' ');
            print_type(ty, TYPE_ATOM, dm, out);
            out.push(' ');
            print_expr_atom(body, dm, out);
        }
    }
}

/// Print an expression in `atomExpr` position (a combinator body argument):
/// non-atomic shapes are parenthesized so re-parsing keeps them as the single
/// body argument (grammar.md / ADR-0038).
fn print_expr_atom(e: &Expr, dm: &DepthMap, out: &mut String) {
    match e {
        Expr::Paren(inner, _) => print_expr_atom(inner, dm, out),
        Expr::Adjoint(..) | Expr::Bin(..) => {
            out.push('(');
            print_expr(e, 0, dm, out);
            out.push(')');
        }
        _ => print_expr(e, 0, dm, out),
    }
}

/// Print the comma-separated flattened elements of a right-nested tuple.
fn print_tuple_elems(e: &Expr, dm: &DepthMap, out: &mut String) {
    match e {
        Expr::Tuple(l, r, _) => {
            print_expr(l, 0, dm, out);
            out.push_str(", ");
            print_tuple_elems(r, dm, out);
        }
        other => print_expr(other, 0, dm, out),
    }
}

/// Return a copy of the program with all [`Expr::Paren`] / [`Type::Paren`]
/// wrappers removed and all spans zeroed. This is the canonical form used to
/// compare ASTs for the `parse ∘ pretty ≡ id` round-trip property, which holds
/// only up to spans, transparent parentheses, and depth annotations (which are
/// carried in the separate `depth_overrides` map, not in the AST nodes).
#[must_use]
pub fn normalize_program(p: &Program) -> Program {
    Program {
        decls: p.decls.iter().map(normalize_decl).collect(),
        span: ZERO,
        // Depth overrides are keyed by span.start, so they cannot survive
        // normalization (which zeroes all spans). Clear them so that the
        // round-trip comparison ignores annotation positions.
        depth_overrides: BTreeMap::new(),
    }
}

const ZERO: Span = Span { start: 0, end: 0 };

fn normalize_decl(d: &Decl) -> Decl {
    match d {
        Decl::Type(t) => Decl::Type(normalize_type_decl(t)),
        Decl::Iso(i) => Decl::Iso(normalize_iso_decl(i)),
        Decl::Prim(p) => Decl::Prim(PrimDecl {
            name: zero_lower(&p.name),
            lhs: normalize_type(&p.lhs),
            rhs: normalize_type(&p.rhs),
            key: p.key.clone(),
            span: ZERO,
        }),
    }
}

fn normalize_type_decl(t: &TypeDecl) -> TypeDecl {
    TypeDecl {
        name: zero_upper(&t.name),
        params: t.params.iter().map(zero_lower).collect(),
        ctors: t
            .ctors
            .iter()
            .map(|c| Ctor {
                name: zero_upper(&c.name),
                payload: c.payload.as_ref().map(normalize_type),
                span: ZERO,
            })
            .collect(),
        span: ZERO,
    }
}

fn normalize_iso_decl(i: &IsoDecl) -> IsoDecl {
    IsoDecl {
        name: zero_lower(&i.name),
        lhs: normalize_type(&i.lhs),
        rhs: normalize_type(&i.rhs),
        body: normalize_body(&i.body),
        wheres: i.wheres.iter().map(normalize_iso_decl).collect(),
        span: ZERO,
    }
}

fn normalize_body(b: &IsoBody) -> IsoBody {
    match b {
        IsoBody::Expr(e) => IsoBody::Expr(normalize_expr(e)),
        IsoBody::Clauses(cs) => IsoBody::Clauses(
            cs.iter()
                .map(|c| Clause {
                    lhs: normalize_pattern(&c.lhs),
                    rhs: normalize_pattern(&c.rhs),
                    span: ZERO,
                })
                .collect(),
        ),
    }
}

fn normalize_pattern(p: &Pattern) -> Pattern {
    match p {
        Pattern::Var(n) => Pattern::Var(zero_lower(n)),
        Pattern::Nat(n, _) => Pattern::Nat(*n, ZERO),
        Pattern::Paren(inner, _) => normalize_pattern(inner),
        Pattern::Ctor(name, args, _) => Pattern::Ctor(
            zero_upper(name),
            args.iter().map(normalize_pattern).collect(),
            ZERO,
        ),
        Pattern::Label(name, args, _) => Pattern::Label(
            zero_lower(name),
            args.iter().map(normalize_pattern).collect(),
            ZERO,
        ),
        Pattern::Tuple(l, r, _) => Pattern::Tuple(
            Box::new(normalize_pattern(l)),
            Box::new(normalize_pattern(r)),
            ZERO,
        ),
    }
}

fn zero_upper(n: &UpperName) -> UpperName {
    UpperName {
        text: n.text.clone(),
        span: ZERO,
    }
}

fn zero_lower(n: &LowerName) -> LowerName {
    LowerName {
        text: n.text.clone(),
        span: ZERO,
    }
}

fn normalize_type(t: &Type) -> Type {
    match t {
        Type::Name(n) => Type::Name(zero_upper(n)),
        Type::Var(n) => Type::Var(zero_lower(n)),
        Type::Empty(_) => Type::Empty(ZERO),
        Type::Unit(_) => Type::Unit(ZERO),
        Type::Paren(inner, _) => normalize_type(inner),
        Type::Prod(l, r, _) => Type::Prod(
            Box::new(normalize_type(l)),
            Box::new(normalize_type(r)),
            ZERO,
        ),
        Type::Sum(l, r, _) => Type::Sum(
            Box::new(normalize_type(l)),
            Box::new(normalize_type(r)),
            ZERO,
        ),
    }
}

fn normalize_expr(e: &Expr) -> Expr {
    match e {
        Expr::Var(n) => Expr::Var(zero_lower(n)),
        Expr::Ctor(n) => Expr::Ctor(zero_upper(n)),
        Expr::Id(_) => Expr::Id(ZERO),
        Expr::Unit(_) => Expr::Unit(ZERO),
        Expr::Paren(inner, _) => normalize_expr(inner),
        Expr::Adjoint(inner, _) => Expr::Adjoint(Box::new(normalize_expr(inner)), ZERO),
        Expr::Bin(op, l, r, _) => Expr::Bin(
            *op,
            Box::new(normalize_expr(l)),
            Box::new(normalize_expr(r)),
            ZERO,
        ),
        Expr::Tuple(l, r, _) => Expr::Tuple(
            Box::new(normalize_expr(l)),
            Box::new(normalize_expr(r)),
            ZERO,
        ),
        Expr::Combinator(kind, ty, body, _) => Expr::Combinator(
            *kind,
            Box::new(normalize_type(ty)),
            Box::new(normalize_expr(body)),
            ZERO,
        ),
    }
}
