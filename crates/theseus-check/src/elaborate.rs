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
use theseus_core::prim::{enumerate_values, Value};
use theseus_core::ty::Type as CoreType;
use theseus_syntax::ast::{BinOp, Clause, Decl, Expr, IsoBody, IsoDecl, Pattern, Program, Type as SurfType};

use crate::diagnostics::Diagnostic;
use crate::env::{Env, Inj};

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
    // 4. General: oracle (match_bind/instantiate) + structural synthesis.
    if let Some(iso) = elab_clauses_general(cs, lhs_ty, rhs_ty, env) {
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

// ── General clause synthesis: oracle + structural decomposition ───────────

fn elab_clauses_general(
    cs: &[Clause],
    lhs_ty: &SurfType,
    rhs_ty: &SurfType,
    env: &Env,
) -> Option<Iso> {
    let core_lhs = env.lower(lhs_ty);
    let core_rhs = env.lower(rhs_ty);
    if core_lhs != core_rhs {
        return None;
    }
    let sigma = compute_bijection(cs, &core_lhs, env)?;
    synthesize(&sigma, &core_lhs)
}

/// Compute the value bijection σ: for each core value, match it against clauses,
/// bind pattern variables, and instantiate the RHS pattern with those bindings.
fn compute_bijection(
    cs: &[Clause],
    core_ty: &CoreType,
    env: &Env,
) -> Option<BTreeMap<Value, Value>> {
    let values = enumerate_values(core_ty);
    let mut sigma = BTreeMap::new();
    for v in &values {
        for c in cs {
            if let Some(bindings) = match_bind(v, &c.lhs, env) {
                let output = instantiate(&c.rhs, &bindings, env)?;
                sigma.insert(v.clone(), output);
                break;
            }
        }
        if !sigma.contains_key(v) {
            return None;
        }
    }
    Some(sigma)
}

// ── Oracle: match_bind + instantiate ─────────────────────────────────────

/// Pattern-match a core `Value` against a surface `Pattern`, returning variable
/// bindings on success.
fn match_bind(value: &Value, pattern: &Pattern, env: &Env) -> Option<BTreeMap<String, Value>> {
    let mut bindings = BTreeMap::new();
    if match_bind_inner(value, pattern, env, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn match_bind_inner(
    value: &Value,
    pattern: &Pattern,
    env: &Env,
    bindings: &mut BTreeMap<String, Value>,
) -> bool {
    match strip_paren_pat(pattern) {
        Pattern::Var(n) => {
            bindings.insert(n.text.clone(), value.clone());
            true
        }
        Pattern::Tuple(a, b, _) => match value {
            Value::Pair(va, vb) => {
                match_bind_inner(va, a, env, bindings)
                    && match_bind_inner(vb, b, env, bindings)
            }
            _ => false,
        },
        Pattern::Ctor(name, args, _) => {
            let Some(info) = env.ctor(&name.text) else {
                return false;
            };
            let Some(payload) = navigate_inj(value, &info.path) else {
                return false;
            };
            match_bind_args(payload, args, env, bindings)
        }
        Pattern::Paren(inner, _) => match_bind_inner(value, inner, env, bindings),
        _ => false,
    }
}

fn match_bind_args(
    value: &Value,
    args: &[Pattern],
    env: &Env,
    bindings: &mut BTreeMap<String, Value>,
) -> bool {
    match args {
        [] => true,
        [single] => match_bind_inner(value, single, env, bindings),
        [first, rest @ ..] => match value {
            Value::Pair(l, r) => {
                match_bind_inner(l, first, env, bindings)
                    && match_bind_args(r, rest, env, bindings)
            }
            _ => false,
        },
    }
}

/// Navigate a value through an injection path (`L` expects `Left`, `R` expects
/// `Right`).
fn navigate_inj<'a>(value: &'a Value, path: &[Inj]) -> Option<&'a Value> {
    let mut v = value;
    for step in path {
        match (step, v) {
            (Inj::L, Value::Left(inner)) => v = inner,
            (Inj::R, Value::Right(inner)) => v = inner,
            _ => return None,
        }
    }
    Some(v)
}

/// Instantiate a surface `Pattern` with variable bindings to produce a core
/// `Value`.
fn instantiate(
    pattern: &Pattern,
    bindings: &BTreeMap<String, Value>,
    env: &Env,
) -> Option<Value> {
    match strip_paren_pat(pattern) {
        Pattern::Var(n) => bindings.get(&n.text).cloned(),
        Pattern::Tuple(a, b, _) => {
            let va = instantiate(a, bindings, env)?;
            let vb = instantiate(b, bindings, env)?;
            Some(Value::pair(va, vb))
        }
        Pattern::Ctor(name, args, _) => {
            let info = env.ctor(&name.text)?;
            let payload = instantiate_args(args, bindings, env)?;
            Some(inject_value(payload, &info.path))
        }
        Pattern::Paren(inner, _) => instantiate(inner, bindings, env),
        _ => None,
    }
}

fn instantiate_args(
    args: &[Pattern],
    bindings: &BTreeMap<String, Value>,
    env: &Env,
) -> Option<Value> {
    match args {
        [] => Some(Value::Unit),
        [single] => instantiate(single, bindings, env),
        [first, rest @ ..] => {
            let l = instantiate(first, bindings, env)?;
            let r = instantiate_args(rest, bindings, env)?;
            Some(Value::pair(l, r))
        }
    }
}

/// Wrap a payload value with `Left`/`Right` injections according to a path,
/// building from inside out (reverse of `navigate_inj`).
fn inject_value(value: Value, path: &[Inj]) -> Value {
    let mut v = value;
    for step in path.iter().rev() {
        v = match step {
            Inj::L => Value::left(v),
            Inj::R => Value::right(v),
        };
    }
    v
}

// ── Structural synthesis: bijection → core Iso ───────────────────────────

/// Synthesize a core `Iso` that realizes the given bijection σ on a type.
fn synthesize(sigma: &BTreeMap<Value, Value>, ty: &CoreType) -> Option<Iso> {
    if sigma.iter().all(|(k, v)| k == v) {
        return Some(Iso::Id(ty.clone()));
    }
    match ty {
        CoreType::Sum(a, b) => synthesize_sum(sigma, a, b),
        CoreType::Prod(a, b) => synthesize_prod(sigma, a, b),
        _ => None,
    }
}

/// Sum decomposition: if σ preserves injection tags, decompose as `Plus(f, g)`.
/// If σ swaps all tags, decompose as `Plus(f, g) ; SwapSum`.
fn synthesize_sum(
    sigma: &BTreeMap<Value, Value>,
    a: &CoreType,
    b: &CoreType,
) -> Option<Iso> {
    // Straight: Left→Left, Right→Right.
    let mut left_sigma = BTreeMap::new();
    let mut right_sigma = BTreeMap::new();
    let mut straight = true;
    for (k, v) in sigma {
        match (k, v) {
            (Value::Left(ki), Value::Left(vo)) => {
                left_sigma.insert(ki.as_ref().clone(), vo.as_ref().clone());
            }
            (Value::Right(ki), Value::Right(vo)) => {
                right_sigma.insert(ki.as_ref().clone(), vo.as_ref().clone());
            }
            _ => {
                straight = false;
                break;
            }
        }
    }
    if straight {
        let li = synthesize(&left_sigma, a)?;
        let ri = synthesize(&right_sigma, b)?;
        return Some(Iso::plus(li, ri));
    }

    // Full swap: Left→Right, Right→Left.
    let mut l2r = BTreeMap::new();
    let mut r2l = BTreeMap::new();
    let mut swapped = true;
    for (k, v) in sigma {
        match (k, v) {
            (Value::Left(ki), Value::Right(vo)) => {
                l2r.insert(ki.as_ref().clone(), vo.as_ref().clone());
            }
            (Value::Right(ki), Value::Left(vo)) => {
                r2l.insert(ki.as_ref().clone(), vo.as_ref().clone());
            }
            _ => {
                swapped = false;
                break;
            }
        }
    }
    if swapped {
        if a == b {
            let fi = synthesize(&l2r, a)?;
            let gi = synthesize(&r2l, b)?;
            return Some(Iso::seq(
                Iso::plus(fi, gi),
                Iso::SwapSum(a.clone(), b.clone()),
            ));
        }
        // Different summand types: only simple tag-swap (payloads unchanged).
        if l2r.iter().all(|(k, v)| k == v) && r2l.iter().all(|(k, v)| k == v) {
            return Some(Iso::SwapSum(a.clone(), b.clone()));
        }
    }
    None
}

/// Product decomposition: try independent factorization, then controlled
/// decomposition when the first factor is a sum.
fn synthesize_prod(
    sigma: &BTreeMap<Value, Value>,
    a: &CoreType,
    b: &CoreType,
) -> Option<Iso> {
    if let Some(iso) = synthesize_prod_factor(sigma, a, b) {
        return Some(iso);
    }
    if let CoreType::Sum(al, ar) = a {
        return synthesize_controlled(sigma, a, al, ar, b);
    }
    None
}

/// Try to decompose σ on `a × b` as `f × g` (independent per component).
fn synthesize_prod_factor(
    sigma: &BTreeMap<Value, Value>,
    a: &CoreType,
    b: &CoreType,
) -> Option<Iso> {
    let mut a_map: BTreeMap<Value, Value> = BTreeMap::new();
    let mut b_map: BTreeMap<Value, Value> = BTreeMap::new();
    for (k, v) in sigma {
        let (Value::Pair(ka, kb), Value::Pair(va, vb)) = (k, v) else {
            return None;
        };
        if let Some(existing) = a_map.get(ka.as_ref()) {
            if existing != va.as_ref() {
                return None;
            }
        } else {
            a_map.insert(ka.as_ref().clone(), va.as_ref().clone());
        }
        if let Some(existing) = b_map.get(kb.as_ref()) {
            if existing != vb.as_ref() {
                return None;
            }
        } else {
            b_map.insert(kb.as_ref().clone(), vb.as_ref().clone());
        }
    }
    let ai = synthesize(&a_map, a)?;
    let bi = synthesize(&b_map, b)?;
    Some(Iso::times(ai, bi))
}

/// Controlled decomposition for `Sum(al, ar) × tgt`: swap to expose the sum
/// in Distrib position, split per branch, recurse, then Factor back.
fn synthesize_controlled(
    sigma: &BTreeMap<Value, Value>,
    ctrl_ty: &CoreType,
    ctrl_l: &CoreType,
    ctrl_r: &CoreType,
    tgt_ty: &CoreType,
) -> Option<Iso> {
    let mut left_sigma = BTreeMap::new();
    let mut right_sigma = BTreeMap::new();
    for (k, v) in sigma {
        let (Value::Pair(ctrl_in, tgt_in), Value::Pair(ctrl_out, tgt_out)) = (k, v) else {
            return None;
        };
        match (ctrl_in.as_ref(), ctrl_out.as_ref()) {
            (Value::Left(cl_in), Value::Left(cl_out)) => {
                left_sigma.insert(
                    Value::pair(tgt_in.as_ref().clone(), cl_in.as_ref().clone()),
                    Value::pair(tgt_out.as_ref().clone(), cl_out.as_ref().clone()),
                );
            }
            (Value::Right(cr_in), Value::Right(cr_out)) => {
                right_sigma.insert(
                    Value::pair(tgt_in.as_ref().clone(), cr_in.as_ref().clone()),
                    Value::pair(tgt_out.as_ref().clone(), cr_out.as_ref().clone()),
                );
            }
            _ => return None,
        }
    }
    let left_branch_ty = CoreType::prod(tgt_ty.clone(), ctrl_l.clone());
    let right_branch_ty = CoreType::prod(tgt_ty.clone(), ctrl_r.clone());
    let li = synthesize(&left_sigma, &left_branch_ty)?;
    let ri = synthesize(&right_sigma, &right_branch_ty)?;
    Some(Iso::seq(
        Iso::SwapProd(ctrl_ty.clone(), tgt_ty.clone()),
        Iso::seq(
            Iso::Distrib(tgt_ty.clone(), ctrl_l.clone(), ctrl_r.clone()),
            Iso::seq(
                Iso::plus(li, ri),
                Iso::seq(
                    Iso::Factor(tgt_ty.clone(), ctrl_l.clone(), ctrl_r.clone()),
                    Iso::SwapProd(tgt_ty.clone(), ctrl_ty.clone()),
                ),
            ),
        ),
    ))
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

    // ── New fixtures: mixed sum + controlled ops ─────────────────────

    fn nothing() -> Value {
        Value::left(Value::Unit)
    }
    fn just_o() -> Value {
        Value::right(Value::left(Value::Unit))
    }
    fn just_i() -> Value {
        Value::right(Value::right(Value::Unit))
    }

    #[test]
    fn swap_maybe_0001() {
        let f = fixture("0001-swap-maybe.thse", "swapMaybe");
        assert_eq!(run(&f, nothing()), nothing());
        assert_eq!(run(&f, just_o()), just_i());
        assert_eq!(run(&f, just_i()), just_o());
        // round-trip via adjoint
        let sym_f = Iso::sym(f.clone());
        assert_eq!(run(&sym_f, run(&f, nothing())), nothing());
        assert_eq!(run(&sym_f, run(&f, just_o())), just_o());
        assert_eq!(run(&sym_f, run(&f, just_i())), just_i());
    }

    #[test]
    fn cnot_0004() {
        let f = fixture("0004-cnot.thse", "cnot");
        assert_eq!(run(&f, Value::pair(o(), o())), Value::pair(o(), o()));
        assert_eq!(run(&f, Value::pair(o(), i())), Value::pair(o(), i()));
        assert_eq!(run(&f, Value::pair(i(), o())), Value::pair(i(), i()));
        assert_eq!(run(&f, Value::pair(i(), i())), Value::pair(i(), o()));
        // round-trip via adjoint
        let sym_f = Iso::sym(f.clone());
        for a in [o(), i()] {
            for b in [o(), i()] {
                let v = Value::pair(a.clone(), b.clone());
                assert_eq!(run(&sym_f, run(&f, v.clone())), v);
            }
        }
    }

    #[test]
    fn toffoli_0006() {
        let f = fixture("0006-toffoli.thse", "tof");
        // (Bit, Bit, Bit) is right-nested: (Bit, (Bit, Bit))
        let t = |a: Value, b: Value, c: Value| Value::pair(a, Value::pair(b, c));
        assert_eq!(run(&f, t(o(), o(), o())), t(o(), o(), o()));
        assert_eq!(run(&f, t(o(), o(), i())), t(o(), o(), i()));
        assert_eq!(run(&f, t(o(), i(), o())), t(o(), i(), o()));
        assert_eq!(run(&f, t(o(), i(), i())), t(o(), i(), i()));
        assert_eq!(run(&f, t(i(), o(), o())), t(i(), o(), o()));
        assert_eq!(run(&f, t(i(), o(), i())), t(i(), o(), i()));
        assert_eq!(run(&f, t(i(), i(), o())), t(i(), i(), i()));
        assert_eq!(run(&f, t(i(), i(), i())), t(i(), i(), o()));
        // round-trip via adjoint
        let sym_f = Iso::sym(f.clone());
        for a in [o(), i()] {
            for b in [o(), i()] {
                for c in [o(), i()] {
                    let v = t(a.clone(), b.clone(), c.clone());
                    assert_eq!(run(&sym_f, run(&f, v.clone())), v);
                }
            }
        }
    }
}
