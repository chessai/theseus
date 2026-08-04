//! Surface↔core value bridge.
//!
//! `.expect` rows use surface values like `O`, `I`, `(O, I)`, `Just O`.
//! This module converts between those surface forms and core [`Value`]s,
//! guided by the surface [`Type`] and resolution [`Env`].

use theseus_check::env::Env;
use theseus_core::prim::{enumerate_values, Value};
use theseus_syntax::ast::Type;

/// A parsed surface value -- either a constructor application or a tuple.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SVal {
    /// A constructor name applied to zero or more atom arguments.
    Ctor(String, Vec<SVal>),
    /// A tuple of two or more elements (parenthesised, comma-separated).
    Tuple(Vec<SVal>),
}

// ── Tokeniser ────────────────────────────────────────────────────────────

fn tokenize(s: &str) -> Vec<String> {
    let mut toks = Vec::new();
    let mut buf = String::new();
    for ch in s.chars() {
        match ch {
            '(' | ')' | ',' => {
                if !buf.is_empty() {
                    toks.push(std::mem::take(&mut buf));
                }
                toks.push(ch.to_string());
            }
            c if c.is_whitespace() => {
                if !buf.is_empty() {
                    toks.push(std::mem::take(&mut buf));
                }
            }
            c => buf.push(c),
        }
    }
    if !buf.is_empty() {
        toks.push(buf);
    }
    toks
}

// ── Parser ───────────────────────────────────────────────────────────────

/// Parse a surface value string into an [`SVal`].
///
/// Returns `Err(message)` on malformed input (unexpected end of input,
/// unbalanced parentheses, trailing tokens, ...) so CLI/REPL callers can
/// report a diagnostic instead of crashing the process.
pub fn parse_surface_value(s: &str) -> Result<SVal, String> {
    let toks = tokenize(s);
    let mut pos = 0;
    let v = parse_app(&toks, &mut pos)?;
    if pos != toks.len() {
        return Err(format!(
            "trailing tokens parsing surface value: {s:?} (at {pos}, toks={toks:?})"
        ));
    }
    Ok(v)
}

/// Parse an "application-level" value: a constructor optionally followed by
/// atom arguments, or a bare atom.
fn parse_app(toks: &[String], pos: &mut usize) -> Result<SVal, String> {
    if *pos >= toks.len() {
        return Err("unexpected end of tokens in surface value".into());
    }
    let head = &toks[*pos];
    if is_ctor_name(head) {
        let name = head.clone();
        *pos += 1;
        let mut args = Vec::new();
        while *pos < toks.len() && is_atom_start(toks, *pos) {
            args.push(parse_atom(toks, pos)?);
        }
        Ok(SVal::Ctor(name, args))
    } else {
        parse_atom(toks, pos)
    }
}

/// Parse an atom: a parenthesised tuple, a numeric literal, or a bare
/// constructor with no args.
fn parse_atom(toks: &[String], pos: &mut usize) -> Result<SVal, String> {
    if *pos >= toks.len() {
        return Err("unexpected end of tokens in surface value (atom)".into());
    }
    let head = &toks[*pos];
    if head == "(" {
        *pos += 1; // consume '('
        let first = parse_app(toks, pos)?;
        let mut elems = vec![first];
        while *pos < toks.len() && toks[*pos] == "," {
            *pos += 1; // consume ','
            elems.push(parse_app(toks, pos)?);
        }
        if !(*pos < toks.len() && toks[*pos] == ")") {
            return Err("expected ')' in tuple".into());
        }
        *pos += 1; // consume ')'
        if elems.len() == 1 {
            // Parenthesised single value -- unwrap.
            Ok(elems.into_iter().next().unwrap())
        } else {
            Ok(SVal::Tuple(elems))
        }
    } else if is_ctor_name(head) {
        // Bare constructor (no args at atom level).
        let name = head.clone();
        *pos += 1;
        Ok(SVal::Ctor(name, Vec::new()))
    } else if is_numeric(head) {
        // Numeric literal -- treat as a constructor name.
        let name = head.clone();
        *pos += 1;
        Ok(SVal::Ctor(name, Vec::new()))
    } else {
        Err(format!("unexpected token in surface value: {head:?}"))
    }
}

fn is_ctor_name(s: &str) -> bool {
    s.starts_with(|c: char| c.is_ascii_uppercase())
}

fn is_numeric(s: &str) -> bool {
    !s.is_empty() && s.bytes().all(|b| b.is_ascii_digit())
}

fn is_atom_start(toks: &[String], pos: usize) -> bool {
    let t = &toks[pos];
    t == "(" || is_ctor_name(t) || is_numeric(t)
}

/// Is `name` a builtin `U<n>` numeric type (power-of-two bit-width)?
fn is_un_type(name: &str) -> bool {
    name.strip_prefix('U')
        .and_then(|d| d.parse::<u32>().ok())
        .is_some_and(|n| n > 0 && n.is_power_of_two())
}

// ── Surface→Core conversion ──────────────────────────────────────────────

/// Convert a surface value to a core [`Value`], guided by the surface [`Type`]
/// and resolution [`Env`].
pub fn to_core(sv: &SVal, ty: &Type, env: &Env) -> Value {
    match sv {
        SVal::Tuple(elems) => tuple_to_core(elems, ty, env),
        SVal::Ctor(name, args) => ctor_to_core(name, args, ty, env),
    }
}

fn strip_paren_ty(t: &Type) -> &Type {
    match t {
        Type::Paren(inner, _) => strip_paren_ty(inner),
        _ => t,
    }
}

fn tuple_to_core(elems: &[SVal], ty: &Type, env: &Env) -> Value {
    let ty = strip_paren_ty(ty);
    match elems {
        [a, b] => {
            if let Type::Prod(ta, tb, _) = ty {
                Value::pair(to_core(a, ta, env), to_core(b, tb, env))
            } else {
                panic!("expected Prod type for 2-tuple, got {ty:?}");
            }
        }
        [a, rest @ ..] => {
            if let Type::Prod(ta, tb, _) = ty {
                Value::pair(to_core(a, ta, env), tuple_to_core(rest, tb, env))
            } else {
                panic!("expected Prod type for n-tuple, got {ty:?}");
            }
        }
        [] => panic!("empty tuple in surface value"),
    }
}

fn ctor_to_core(name: &str, args: &[SVal], ty: &Type, env: &Env) -> Value {
    // Numeric literal at a U<n> type: the k-th enumerated value.
    if is_numeric(name) && args.is_empty() {
        if let Type::Name(n) = strip_paren_ty(ty) {
            if is_un_type(&n.text) {
                let k: usize = name
                    .parse()
                    .unwrap_or_else(|_| panic!("bad numeric literal: {name}"));
                let core_ty = env.lower(ty);
                let values = enumerate_values(&core_ty);
                return values
                    .into_iter()
                    .nth(k)
                    .unwrap_or_else(|| panic!("literal {k} out of range for type {}", n.text));
            }
        }
    }

    let ci = env
        .ctor(name)
        .unwrap_or_else(|| panic!("unknown constructor in surface value: {name}"));

    // Build the payload value.
    let payload = if args.is_empty() {
        Value::Unit
    } else if args.len() == 1 {
        let payload_ty = ci
            .payload
            .as_ref()
            .unwrap_or_else(|| panic!("ctor {name} has no payload but got an arg"));
        to_core(&args[0], payload_ty, env)
    } else {
        // Multiple args → treat as a tuple payload.
        let payload_ty = ci
            .payload
            .as_ref()
            .unwrap_or_else(|| panic!("ctor {name} has no payload but got args"));
        tuple_to_core(args, payload_ty, env)
    };

    // Wrap with injection path -- innermost-first (rev).
    use theseus_check::env::Inj;
    ci.path.iter().rev().fold(payload, |v, inj| match inj {
        Inj::L => Value::left(v),
        Inj::R => Value::right(v),
    })
}

// ── Core→Surface rendering ───────────────────────────────────────────────

/// Render a core [`Value`] back to surface syntax, guided by the surface
/// [`Type`] and resolution [`Env`].
pub fn render(v: &Value, ty: &Type, env: &Env) -> String {
    let ty = strip_paren_ty(ty);
    match ty {
        Type::Unit(_) => "()".to_string(),
        Type::Empty(_) => panic!("cannot render a value of empty type"),
        Type::Prod(ta, tb, _) => {
            let (a, b) = match v {
                Value::Pair(a, b) => (a.as_ref(), b.as_ref()),
                _ => panic!("expected Pair for Prod type, got {v:?}"),
            };
            format!("({}, {})", render(a, ta, env), render(b, tb, env))
        }
        Type::Sum(ta, tb, _) => match v {
            Value::Left(inner) => format!("L({})", render(inner, ta, env)),
            Value::Right(inner) => format!("R({})", render(inner, tb, env)),
            _ => panic!("expected Left/Right for Sum type, got {v:?}"),
        },
        Type::Name(n) => render_named(v, &n.text, env),
        Type::Var(_) => format!("{v:?}"),
        Type::Paren(..) => unreachable!("strip_paren_ty removes all Paren wrappers"),
    }
}

fn render_named(v: &Value, type_name: &str, env: &Env) -> String {
    // U<n> builtin: render as the numeric index of the value in the enumeration.
    if is_un_type(type_name) {
        let ty = Type::Name(theseus_syntax::ast::UpperName {
            text: type_name.into(),
            span: theseus_syntax::ast::Span { start: 0, end: 0 },
        });
        let core_ty = env.lower(&ty);
        let values = enumerate_values(&core_ty);
        for (i, val) in values.iter().enumerate() {
            if val == v {
                return i.to_string();
            }
        }
        panic!("value {v:?} does not match any inhabitant of {type_name}");
    }

    let ti = env
        .type_info(type_name)
        .unwrap_or_else(|| panic!("unknown type in render: {type_name}"));

    // Try each constructor: follow its injection path and see if v matches.
    for ctor_name in &ti.ctors {
        let ci = env.ctor(ctor_name).unwrap();
        if let Some(payload) = try_extract_injection(v, &ci.path) {
            if let Some(payload_ty) = &ci.payload {
                let inner = render(payload, payload_ty, env);
                // Render as `Ctor payload`; the payload prints itself.
                return format!("{ctor_name} {inner}");
            } else {
                return ctor_name.clone();
            }
        }
    }
    panic!("value {v:?} does not match any ctor of type {type_name}");
}

/// Try to extract a payload by following an injection path forward through the
/// value. Returns the payload value if the path matches.
fn try_extract_injection<'a>(v: &'a Value, path: &[theseus_check::env::Inj]) -> Option<&'a Value> {
    use theseus_check::env::Inj;
    let mut current = v;
    for step in path {
        match (step, current) {
            (Inj::L, Value::Left(inner)) => current = inner,
            (Inj::R, Value::Right(inner)) => current = inner,
            _ => return None,
        }
    }
    Some(current)
}
