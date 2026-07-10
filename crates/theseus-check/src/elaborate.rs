//! Elaboration: surface [`IsoDecl`] → core [`Iso`] (M1 slice-1 structural core).
//!
//! Supported today:
//! * `= expr` bodies — recursive translation to core combinators
//!   (`;`→[`Iso::Seq`], `*`→[`Iso::Times`], `+`→[`Iso::Plus`], `~e`→[`Iso::sym`],
//!   a name → the referenced iso, elaborated).
//! * clause bodies whose denotation is a single structural combinator:
//!   - identity (every clause `p <-> p`)                → [`Iso::Id`]
//!   - a 2-constructor nullary-sum swap (e.g. `not`)    → [`Iso::SwapSum`]
//!   - a product swap `(a,b)<->(b,a)`                   → [`Iso::SwapProd`]
//!   - a product reassoc `(a,(b,c))<->((a,b),c)`        → [`Iso::AssocLProd`]
//!
//! NOT yet elaborated (returns `Err(Vec::new())` = "not elaborated", per the
//! stub convention): mixed-sum clause tables (e.g. `swapMaybe`), controlled
//! multi-clause ops (`cnot`/`toffoli` — need the general demux;perm;sym(demux)
//! synthesis), and `U<n>` literal isos. These are the deferred "full E3" work.

use std::collections::BTreeMap;

use theseus_core::iso::Iso;
use theseus_core::ty::Type as CoreType;
use theseus_syntax::ast::{BinOp, Clause, Decl, Expr, IsoBody, IsoDecl, Pattern, Program, Type as SurfType};

use crate::diagnostics::Diagnostic;
use crate::env::Env;

type ElabResult = Result<Iso, Vec<Diagnostic>>;

/// The "not elaborated (unsupported in the structural core)" sentinel — an empty
/// error vector, matching the crate convention (no `T0xxx` diagnostic applies).
fn unsupported() -> ElabResult {
    Err(Vec::new())
}

/// Elaborate every top-level iso in `program`, resolving `= expr` references
/// (both top-level siblings and `where`-bound isos) against one another. Returns
/// a name→result map; unsupported isos map to `Err(vec![])`.
pub fn elaborate_program(program: &Program, env: &Env) -> BTreeMap<String, ElabResult> {
    let mut decls: BTreeMap<String, &IsoDecl> = BTreeMap::new();
    collect_isos(&program.decls, &mut decls);
    decls
        .iter()
        .map(|(name, iso)| (name.clone(), elab_iso(iso, env, &decls)))
        .collect()
}

/// Elaborate a single iso. Resolves `= expr` references to this iso's own
/// `where`-bound isos; a reference to a top-level sibling is unresolved here
/// (use [`elaborate_program`] for cross-referencing programs).
pub fn elaborate(iso: &IsoDecl, env: &Env) -> ElabResult {
    let mut decls: BTreeMap<String, &IsoDecl> = BTreeMap::new();
    collect_where(iso, &mut decls);
    elab_iso(iso, env, &decls)
}

fn collect_isos<'a>(decls: &'a [Decl], out: &mut BTreeMap<String, &'a IsoDecl>) {
    for d in decls {
        if let Decl::Iso(iso) = d {
            out.insert(iso.name.text.clone(), iso);
            collect_where(iso, out);
        }
    }
}

fn collect_where<'a>(iso: &'a IsoDecl, out: &mut BTreeMap<String, &'a IsoDecl>) {
    for w in &iso.wheres {
        out.insert(w.name.text.clone(), w);
        collect_where(w, out);
    }
}

fn elab_iso(iso: &IsoDecl, env: &Env, isos: &BTreeMap<String, &IsoDecl>) -> ElabResult {
    match &iso.body {
        IsoBody::Expr(e) => elab_expr(e, env, isos),
        IsoBody::Clauses(cs) => elab_clauses(cs, &iso.lhs, &iso.rhs, env),
    }
}

fn elab_expr(e: &Expr, env: &Env, isos: &BTreeMap<String, &IsoDecl>) -> ElabResult {
    match e {
        Expr::Paren(inner, _) => elab_expr(inner, env, isos),
        Expr::Var(name) => match isos.get(&name.text) {
            Some(iso) => elab_iso(iso, env, isos),
            None => unsupported(),
        },
        Expr::Adjoint(inner, _) => Ok(Iso::sym(elab_expr(inner, env, isos)?)),
        Expr::Bin(op, l, r, _) => {
            let lf = elab_expr(l, env, isos)?;
            let rf = elab_expr(r, env, isos)?;
            Ok(match op {
                BinOp::Seq => Iso::seq(lf, rf),
                BinOp::Prod => Iso::times(lf, rf),
                BinOp::Sum => Iso::plus(lf, rf),
            })
        }
        // `id` needs the type at its position (inference) — deferred; `Ctor`,
        // `Tuple`, `Unit` do not appear as slice-1 combinator bodies.
        _ => unsupported(),
    }
}

fn elab_clauses(cs: &[Clause], lhs_ty: &SurfType, rhs_ty: &SurfType, env: &Env) -> ElabResult {
    // 1. Identity — every clause maps a pattern to itself.
    if !cs.is_empty() && cs.iter().all(|c| pattern_eq(&c.lhs, &c.rhs)) {
        return Ok(Iso::Id(env.lower(lhs_ty)));
    }
    // 2. Single-clause product rearrangement (swap / reassoc).
    if cs.len() == 1 {
        if let Some(iso) = elab_product_rearrange(&cs[0], lhs_ty, env) {
            return Ok(iso);
        }
    }
    // 3. Two-constructor nullary sum swap (`not`).
    if let Some(iso) = elab_sum_swap(cs, lhs_ty, rhs_ty, env) {
        return Ok(iso);
    }
    unsupported()
}

fn elab_product_rearrange(c: &Clause, lhs_ty: &SurfType, env: &Env) -> Option<Iso> {
    let l = strip_paren_pat(&c.lhs);
    let r = strip_paren_pat(&c.rhs);
    let lt = strip_paren_ty(lhs_ty);
    let (Pattern::Tuple(lx, ly, _), Pattern::Tuple(rx, ry, _)) = (l, r) else {
        return None;
    };

    // swap: (x, y) <-> (y, x)
    if let (Some(x), Some(y), Some(rxn), Some(ryn)) =
        (var_name(lx), var_name(ly), var_name(rx), var_name(ry))
    {
        if x == ryn && y == rxn && x != y {
            if let SurfType::Prod(a, b, _) = lt {
                return Some(Iso::SwapProd(env.lower(a), env.lower(b)));
            }
        }
    }

    // reassoc: (x, (y, z)) <-> ((x, y), z)
    let x = var_name(lx)?;
    let (Pattern::Tuple(ly1, ly2, _), Pattern::Tuple(rx1, rx2, _)) =
        (strip_paren_pat(ly), strip_paren_pat(rx))
    else {
        return None;
    };
    let (y, z) = (var_name(ly1)?, var_name(ly2)?);
    let (rxa, rxb, rzn) = (var_name(rx1)?, var_name(rx2)?, var_name(ry)?);
    if x == rxa && y == rxb && z == rzn {
        if let SurfType::Prod(a, bc, _) = lt {
            if let SurfType::Prod(b, cc, _) = strip_paren_ty(bc) {
                return Some(Iso::AssocLProd(env.lower(a), env.lower(b), env.lower(cc)));
            }
        }
    }
    None
}

fn elab_sum_swap(cs: &[Clause], lhs_ty: &SurfType, rhs_ty: &SurfType, env: &Env) -> Option<Iso> {
    let name = match strip_paren_ty(lhs_ty) {
        SurfType::Name(n) => n.text.as_str(),
        _ => return None,
    };
    // Same named type on both sides.
    match strip_paren_ty(rhs_ty) {
        SurfType::Name(n) if n.text == name => {}
        _ => return None,
    }
    let ti = env.type_info(name)?;
    if ti.ctors.len() != 2 || cs.len() != 2 {
        return None;
    }
    let (c0, c1) = (ti.ctors[0].as_str(), ti.ctors[1].as_str());
    if env.ctor(c0)?.payload.is_some() || env.ctor(c1)?.payload.is_some() {
        return None; // only nullary 2-ctor swaps map to swap⁺(1,1)
    }
    let mut pairs = Vec::new();
    for c in cs {
        pairs.push((nullary_ctor(&c.lhs)?, nullary_ctor(&c.rhs)?));
    }
    if pairs.contains(&(c0, c1)) && pairs.contains(&(c1, c0)) {
        Some(Iso::SwapSum(CoreType::One, CoreType::One))
    } else {
        None
    }
}

// ── small structural helpers ──────────────────────────────────────────────

fn strip_paren_pat(p: &Pattern) -> &Pattern {
    match p {
        Pattern::Paren(inner, _) => strip_paren_pat(inner),
        _ => p,
    }
}

fn strip_paren_ty(t: &SurfType) -> &SurfType {
    match t {
        SurfType::Paren(inner, _) => strip_paren_ty(inner),
        _ => t,
    }
}

fn var_name(p: &Pattern) -> Option<&str> {
    match strip_paren_pat(p) {
        Pattern::Var(n) => Some(&n.text),
        _ => None,
    }
}

fn nullary_ctor(p: &Pattern) -> Option<&str> {
    match strip_paren_pat(p) {
        Pattern::Ctor(n, args, _) if args.is_empty() => Some(&n.text),
        _ => None,
    }
}

fn pattern_eq(a: &Pattern, b: &Pattern) -> bool {
    match (strip_paren_pat(a), strip_paren_pat(b)) {
        (Pattern::Var(x), Pattern::Var(y)) => x.text == y.text,
        (Pattern::Nat(x, _), Pattern::Nat(y, _)) => x == y,
        (Pattern::Ctor(n1, a1, _), Pattern::Ctor(n2, a2, _)) => {
            n1.text == n2.text
                && a1.len() == a2.len()
                && a1.iter().zip(a2).all(|(p, q)| pattern_eq(p, q))
        }
        (Pattern::Tuple(a1, b1, _), Pattern::Tuple(a2, b2, _)) => {
            pattern_eq(a1, a2) && pattern_eq(b1, b2)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use theseus_core::prim::Value;
    use theseus_eval::interp::eval;

    use crate::testkit;

    fn o() -> Value {
        Value::left(Value::Unit)
    }
    fn i() -> Value {
        Value::right(Value::Unit)
    }
    fn run(iso: &Iso, v: Value) -> Value {
        let mut fuel = 100_000u64;
        eval(iso, v, &mut fuel).expect("eval succeeds within fuel")
    }
    fn fixture(file: &str, iso: &str) -> Iso {
        let path = testkit::repo_root().join("conformance/programs").join(file);
        let prog = testkit::load_program(&path);
        let env = crate::env::resolve(&prog);
        let map = elaborate_program(&prog, &env);
        map.get(iso)
            .unwrap_or_else(|| panic!("no iso {iso} in {file}"))
            .as_ref()
            .unwrap_or_else(|_| panic!("iso {iso} in {file} was not elaborated"))
            .clone()
    }

    #[test]
    fn id_bit_0002() {
        let f = fixture("0002-id-bit.thse", "idb");
        assert_eq!(run(&f, o()), o());
        assert_eq!(run(&f, i()), i());
    }

    #[test]
    fn not_0003() {
        let f = fixture("0003-not.thse", "notb");
        assert_eq!(run(&f, o()), i());
        assert_eq!(run(&f, i()), o());
        // round-trip via the adjoint
        assert_eq!(run(&Iso::sym(f.clone()), run(&f, o())), o());
    }

    #[test]
    fn swap_pair_0005() {
        let f = fixture("0005-swap-pair.thse", "swp");
        assert_eq!(run(&f, Value::pair(o(), i())), Value::pair(i(), o()));
        assert_eq!(run(&f, Value::pair(i(), i())), Value::pair(i(), i()));
    }

    #[test]
    fn seq_not_not_0007() {
        let f = fixture("0007-seq-not-not.thse", "nn"); // notb ; notb == id
        assert_eq!(run(&f, o()), o());
        assert_eq!(run(&f, i()), i());
    }

    #[test]
    fn par_0009() {
        let f = fixture("0009-par.thse", "flipfst"); // notb * idb
        assert_eq!(run(&f, Value::pair(o(), i())), Value::pair(i(), i()));
        assert_eq!(run(&f, Value::pair(i(), o())), Value::pair(o(), o()));
    }

    #[test]
    fn choice_0010() {
        let f = fixture("0010-choice-either.thse", "flipL"); // notb + idb
        assert_eq!(run(&f, Value::left(o())), Value::left(i())); // notb on L
        assert_eq!(run(&f, Value::right(o())), Value::right(o())); // idb on R
    }

    #[test]
    fn where_twice_0013() {
        let f = fixture("0013-where.thse", "twice"); // n ; n (n = not) == id
        assert_eq!(run(&f, o()), o());
        assert_eq!(run(&f, i()), i());
    }

    #[test]
    fn reassoc_0015() {
        let f = fixture("0015-reassoc.thse", "assoc");
        assert_eq!(
            run(&f, Value::pair(o(), Value::pair(i(), o()))),
            Value::pair(Value::pair(o(), i()), o())
        );
    }
}
