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
//! Also elaborated via general clause synthesis (oracle + structural
//! decomposition): mixed-sum clause tables (e.g. `swapMaybe`), controlled
//! multi-clause ops (`cnot`/`toffoli`), and `U<n>` literal isos (e.g. `flip1`).

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
        IsoBody::Clauses(cs) => {
            let labels = env.iso_labels(&iso.name.text);
            if labels.is_empty() {
                elab_clauses(cs, &iso.lhs, &iso.rhs, env)
            } else {
                elab_clauses_label(cs, &iso.lhs, &iso.rhs, labels, env)
            }
        }
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
    let sigma = compute_bijection(cs, &core_lhs, lhs_ty, rhs_ty, env)?;
    synthesize(&sigma, &core_lhs)
}

/// Compute the value bijection σ: for each core value, match it against clauses,
/// bind pattern variables, and instantiate the RHS pattern with those bindings.
fn compute_bijection(
    cs: &[Clause],
    core_ty: &CoreType,
    lhs_ty: &SurfType,
    rhs_ty: &SurfType,
    env: &Env,
) -> Option<BTreeMap<Value, Value>> {
    let values = enumerate_values(core_ty);
    let mut sigma = BTreeMap::new();
    for v in &values {
        for c in cs {
            if let Some(bindings) = match_bind(v, &c.lhs, lhs_ty, env) {
                let output = instantiate(&c.rhs, &bindings, rhs_ty, env)?;
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
/// bindings on success. The surface type `ty` is threaded to resolve `Nat` literals.
fn match_bind(value: &Value, pattern: &Pattern, ty: &SurfType, env: &Env) -> Option<BTreeMap<String, Value>> {
    let mut bindings = BTreeMap::new();
    if match_bind_inner(value, pattern, ty, env, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn match_bind_inner(
    value: &Value,
    pattern: &Pattern,
    ty: &SurfType,
    env: &Env,
    bindings: &mut BTreeMap<String, Value>,
) -> bool {
    let ty = strip_paren_ty(ty);
    match strip_paren_pat(pattern) {
        Pattern::Var(n) => {
            bindings.insert(n.text.clone(), value.clone());
            true
        }
        Pattern::Tuple(a, b, _) => match (value, ty) {
            (Value::Pair(va, vb), SurfType::Prod(ta, tb, _)) => {
                match_bind_inner(va, a, ta, env, bindings)
                    && match_bind_inner(vb, b, tb, env, bindings)
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
            match_bind_args(payload, args, info.payload.as_ref(), env, bindings)
        }
        Pattern::Nat(k, _) => {
            let core_ty = env.lower(ty);
            let values = enumerate_values(&core_ty);
            let k = *k as usize;
            k < values.len() && *value == values[k]
        }
        Pattern::Paren(inner, _) => match_bind_inner(value, inner, ty, env, bindings),
        _ => false,
    }
}

fn match_bind_args(
    value: &Value,
    args: &[Pattern],
    payload_ty: Option<&SurfType>,
    env: &Env,
    bindings: &mut BTreeMap<String, Value>,
) -> bool {
    match args {
        [] => true,
        [single] => {
            if let Some(ty) = payload_ty {
                match_bind_inner(value, single, ty, env, bindings)
            } else {
                // Nullary — only var patterns can match here.
                if let Pattern::Var(n) = strip_paren_pat(single) {
                    bindings.insert(n.text.clone(), value.clone());
                    true
                } else {
                    false
                }
            }
        }
        [first, rest @ ..] => match value {
            Value::Pair(l, r) => {
                if let Some(ty) = payload_ty {
                    let ty = strip_paren_ty(ty);
                    if let SurfType::Prod(ta, tb, _) = ty {
                        match_bind_inner(l, first, ta, env, bindings)
                            && match_bind_args(r, rest, Some(tb), env, bindings)
                    } else {
                        false
                    }
                } else {
                    false
                }
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
/// `Value`. The surface type `ty` is threaded to resolve `Nat` literals.
fn instantiate(
    pattern: &Pattern,
    bindings: &BTreeMap<String, Value>,
    ty: &SurfType,
    env: &Env,
) -> Option<Value> {
    let ty = strip_paren_ty(ty);
    match strip_paren_pat(pattern) {
        Pattern::Var(n) => bindings.get(&n.text).cloned(),
        Pattern::Tuple(a, b, _) => {
            if let SurfType::Prod(ta, tb, _) = ty {
                let va = instantiate(a, bindings, ta, env)?;
                let vb = instantiate(b, bindings, tb, env)?;
                Some(Value::pair(va, vb))
            } else {
                None
            }
        }
        Pattern::Ctor(name, args, _) => {
            let info = env.ctor(&name.text)?;
            let payload = instantiate_args(args, bindings, info.payload.as_ref(), env)?;
            Some(inject_value(payload, &info.path))
        }
        Pattern::Nat(k, _) => {
            let core_ty = env.lower(ty);
            let values = enumerate_values(&core_ty);
            values.get(*k as usize).cloned()
        }
        Pattern::Paren(inner, _) => instantiate(inner, bindings, ty, env),
        _ => None,
    }
}

fn instantiate_args(
    args: &[Pattern],
    bindings: &BTreeMap<String, Value>,
    payload_ty: Option<&SurfType>,
    env: &Env,
) -> Option<Value> {
    match args {
        [] => Some(Value::Unit),
        [single] => {
            if let Some(ty) = payload_ty {
                instantiate(single, bindings, ty, env)
            } else {
                // No payload type — try as variable.
                if let Pattern::Var(n) = strip_paren_pat(single) {
                    bindings.get(&n.text).cloned()
                } else {
                    None
                }
            }
        }
        [first, rest @ ..] => {
            if let Some(ty) = payload_ty {
                let ty = strip_paren_ty(ty);
                if let SurfType::Prod(ta, tb, _) = ty {
                    let l = instantiate(first, bindings, ta, env)?;
                    let r = instantiate_args(rest, bindings, Some(tb), env)?;
                    Some(Value::pair(l, r))
                } else {
                    None
                }
            } else {
                None
            }
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

// ── Label elaboration (E4): label clause sets → trace(u, Φ) ────────────

/// Build the label carrier core type U = Σ_ℓ U_ℓ as a right-nested sum.
fn build_label_carrier(labels: &[(String, SurfType)], env: &Env) -> CoreType {
    let summands: Vec<CoreType> = labels.iter().map(|(_, ty)| env.lower(ty)).collect();
    match summands.as_slice() {
        [] => CoreType::Zero,
        [last] => last.clone(),
        [head, rest @ ..] => {
            let mut acc = rest.last().unwrap().clone();
            for s in rest.iter().rev().skip(1) {
                acc = CoreType::Sum(Box::new(s.clone()), Box::new(acc));
            }
            CoreType::Sum(Box::new(head.clone()), Box::new(acc))
        }
    }
}

/// Injection path for the `index`-th label in a right-nested sum of `count`
/// labels. Same encoding as `env::injection_path` for constructors.
fn label_injection_path(index: usize, count: usize) -> Vec<Inj> {
    if count <= 1 {
        return Vec::new();
    }
    let mut path = vec![Inj::R; index];
    if index < count - 1 {
        path.push(Inj::L);
    }
    path
}

/// Elaborate a label iso to `Iso::trace(u, Φ)`.
fn elab_clauses_label(
    cs: &[Clause],
    lhs_ty: &SurfType,
    rhs_ty: &SurfType,
    labels: &[(String, SurfType)],
    env: &Env,
) -> ElabResult {
    // 1. Build carrier type u = Σ_ℓ U_ℓ.
    let carrier = build_label_carrier(labels, env);

    // 2. Build extended types: ext = Sum(u, boundary).
    let boundary_lhs = env.lower(lhs_ty);
    let boundary_rhs = env.lower(rhs_ty);
    let ext_lhs = CoreType::Sum(Box::new(carrier.clone()), Box::new(boundary_lhs));
    let ext_rhs = CoreType::Sum(Box::new(carrier.clone()), Box::new(boundary_rhs));

    // 3. Only handle ext_lhs == ext_rhs (A == B) for now.
    if ext_lhs != ext_rhs {
        return unsupported();
    }

    // 4. Build the body bijection Φ over the extended type.
    let values = enumerate_values(&ext_lhs);
    let mut phi = BTreeMap::new();
    for v in &values {
        let mut matched = false;
        for c in cs {
            if let Some(bindings) = match_bind_extended(v, &c.lhs, lhs_ty, labels, env) {
                let Some(output) = instantiate_extended(&c.rhs, &bindings, rhs_ty, labels, env) else {
                    // Matched LHS but RHS instantiation failed (e.g. unbound
                    // variable). Abort immediately — do not fall through to
                    // later clauses, matching compute_bijection's behaviour.
                    return unsupported();
                };
                phi.insert(v.clone(), output);
                matched = true;
                break;
            }
        }
        if !matched {
            return unsupported();
        }
    }

    // 5. Synthesize a core iso realizing the body bijection Φ. Structural
    //    synthesis handles the shapes it knows; for an arbitrary permutation
    //    (e.g. spin's counter walk) it returns None, and we fall back to an
    //    opaque Prim carrying the (forward, backward) value tables. The Prim
    //    body is denotationally correct (eval looks it up; it is mutual-inverse
    //    by TH-4a-label) and has static bound 0, so B(trace) = |u|+1 holds.
    //    M2 owes a structural (demux, design/elaboration.md §E4) refinement of
    //    this body before hardware lowering; the Prim interim is eval-complete.
    let body = match synthesize(&phi, &ext_lhs) {
        Some(iso) => iso,
        None => {
            let backward: BTreeMap<Value, Value> =
                phi.iter().map(|(k, v)| (v.clone(), k.clone())).collect();
            // Full admissibility guard (ADR-0013): verify the (forward,
            // backward) pair are mutual inverses over the finite domains.
            // This is strictly stronger than the old len-check — it also
            // catches Φ outputs that fall outside the codomain type.
            let def = theseus_core::PrimDef {
                forward_name: "<trace-body>".into(),
                backward_name: "<trace-body-inv>".into(),
                input_ty: ext_lhs.clone(),
                output_ty: ext_rhs.clone(),
                forward: phi,
                backward,
            };
            if theseus_core::prim::admit(&def).is_err() {
                return unsupported();
            }
            Iso::Prim(Box::new(def))
        }
    };

    // 6. Wrap in trace.
    Ok(Iso::trace(carrier, body))
}

// ── Extended match/instantiate (label-aware) ────────────────────────────

/// Match a value from the extended type `Sum(u, boundary)` against a pattern.
fn match_bind_extended(
    value: &Value,
    pattern: &Pattern,
    boundary_ty: &SurfType,
    labels: &[(String, SurfType)],
    env: &Env,
) -> Option<BTreeMap<String, Value>> {
    let mut bindings = BTreeMap::new();
    if match_bind_extended_inner(value, pattern, boundary_ty, labels, env, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn match_bind_extended_inner(
    value: &Value,
    pattern: &Pattern,
    boundary_ty: &SurfType,
    labels: &[(String, SurfType)],
    env: &Env,
    bindings: &mut BTreeMap<String, Value>,
) -> bool {
    match strip_paren_pat(pattern) {
        Pattern::Label(name, args, _) => {
            // Label pattern: matches Left(carrier_val).
            let Value::Left(carrier_val) = value else {
                return false;
            };
            let Some(label_idx) = labels.iter().position(|(l, _)| l == &name.text) else {
                return false;
            };
            let path = label_injection_path(label_idx, labels.len());
            let Some(payload) = navigate_inj(carrier_val, &path) else {
                return false;
            };
            let carrier_ty = &labels[label_idx].1;
            match_bind_args(payload, args, Some(carrier_ty), env, bindings)
        }
        Pattern::Var(n) => {
            // Variable in extended context: matches boundary Right(val).
            let Value::Right(inner) = value else {
                return false;
            };
            bindings.insert(n.text.clone(), inner.as_ref().clone());
            true
        }
        plain => {
            // Other plain patterns: match boundary Right(val).
            let Value::Right(inner) = value else {
                return false;
            };
            match_bind_inner(inner, plain, boundary_ty, env, bindings)
        }
    }
}

/// Instantiate a pattern in extended context, producing a value of the
/// extended type `Sum(u, boundary)`.
fn instantiate_extended(
    pattern: &Pattern,
    bindings: &BTreeMap<String, Value>,
    boundary_ty: &SurfType,
    labels: &[(String, SurfType)],
    env: &Env,
) -> Option<Value> {
    match strip_paren_pat(pattern) {
        Pattern::Label(name, args, _) => {
            let label_idx = labels.iter().position(|(l, _)| l == &name.text)?;
            let carrier_ty = &labels[label_idx].1;
            let payload = instantiate_args(args, bindings, Some(carrier_ty), env)?;
            let path = label_injection_path(label_idx, labels.len());
            let injected = inject_value(payload, &path);
            Some(Value::left(injected))
        }
        Pattern::Var(n) => {
            let val = bindings.get(&n.text)?;
            Some(Value::right(val.clone()))
        }
        plain => {
            let val = instantiate(plain, bindings, boundary_ty, env)?;
            Some(Value::right(val))
        }
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
    fn flip1_0014() {
        let f = fixture("0014-literal-u1.thse", "flip1");
        // U1: 0 = Left(Unit), 1 = Right(Unit)
        let u0 = Value::left(Value::Unit);
        let u1 = Value::right(Value::Unit);
        assert_eq!(run(&f, u0.clone()), u1.clone());
        assert_eq!(run(&f, u1.clone()), u0.clone());
        // round-trip via adjoint
        let sym_f = Iso::sym(f.clone());
        assert_eq!(run(&sym_f, run(&f, u0.clone())), u0);
        assert_eq!(run(&sym_f, run(&f, u1.clone())), u1);
    }

    // ── Label elaboration (E4): trace tests ───────────────────────────

    #[test]
    fn spin_0011_is_trace() {
        let path = testkit::repo_root().join("conformance/programs/0011-label-loop.thse");
        let prog = testkit::load_program(&path);
        let env = crate::env::resolve(&prog);
        let map = elaborate_program(&prog, &env);
        let result = map.get("spin").expect("no iso spin");
        let iso = result.as_ref().expect("spin was not elaborated");

        // Carrier: lower((Bit, (Bit, Bit))) = Prod(Sum(1,1), Prod(Sum(1,1), Sum(1,1)))
        let bit = CoreType::Sum(Box::new(CoreType::One), Box::new(CoreType::One));
        let expected_u = CoreType::Prod(
            Box::new(bit.clone()),
            Box::new(CoreType::Prod(Box::new(bit.clone()), Box::new(bit.clone()))),
        );

        match iso {
            Iso::Trace(u, _phi) => {
                assert_eq!(*u, expected_u, "carrier type mismatch");
            }
            other => panic!("expected Trace, got {other:?}"),
        }
    }

    #[test]
    fn spin_0011_body_bijection() {
        let path = testkit::repo_root().join("conformance/programs/0011-label-loop.thse");
        let prog = testkit::load_program(&path);
        let env = crate::env::resolve(&prog);
        let map = elaborate_program(&prog, &env);
        let result = map.get("spin").expect("no iso spin");
        let iso = result.as_ref().expect("spin was not elaborated");

        let Iso::Trace(_, phi) = iso else {
            panic!("expected Trace");
        };

        // Bit encoding: O = Left(Unit), I = Right(Unit)
        // Extended value constructors: Right(v) = boundary, Left(v) = carrier state
        // Carrier (single label $go): (Bit, (Bit, Bit))
        // carrier val helper: (b, (c0, c1))
        let cv = |b: Value, c0: Value, c1: Value| Value::pair(b, Value::pair(c0, c1));

        // Clause 1: b <-> $go (b, (O, O))
        // Right(O) → Left(cv(O, O, O))
        assert_eq!(run(phi, Value::right(o())), Value::left(cv(o(), o(), o())));
        // Right(I) → Left(cv(I, O, O))
        assert_eq!(run(phi, Value::right(i())), Value::left(cv(i(), o(), o())));

        // Clause 2: $go (b, (O, O)) <-> $go (b, (O, I))
        // Left(cv(O, O, O)) → Left(cv(O, O, I))
        assert_eq!(
            run(phi, Value::left(cv(o(), o(), o()))),
            Value::left(cv(o(), o(), i()))
        );

        // Clause 5: $go (b, (I, I)) <-> b
        // Left(cv(O, I, I)) → Right(O)
        assert_eq!(
            run(phi, Value::left(cv(o(), i(), i()))),
            Value::right(o())
        );
        // Left(cv(I, I, I)) → Right(I)
        assert_eq!(
            run(phi, Value::left(cv(i(), i(), i()))),
            Value::right(i())
        );
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

    // ── FIX 1 repro: ill-typed Prim body rejected by admit() guard ───

    #[test]
    fn label_illtyped_prim_rejected() {
        // type Bit = O | I
        // iso bad : Bit <-> Bit
        //   | b <-> $go (b, b)
        //   | $go p <-> p
        //
        // $go's payload type is Bit, but clause 1's RHS `$go (b, b)` produces
        // a (Bit, Bit) payload — Φ maps boundary values into ill-typed carrier
        // entries. The admit() guard must catch this.
        let src = "type Bit = O | I\niso bad : Bit <-> Bit\n  | b <-> $go (b, b)\n  | $go p <-> p\n";
        let prog = theseus_syntax::parse(src).expect("parses");
        let env = crate::env::resolve(&prog);
        let map = elaborate_program(&prog, &env);
        let result = map.get("bad").expect("no iso bad");
        assert!(
            result.is_err(),
            "ill-typed prim body should not elaborate, got: {result:?}"
        );
    }

    // ── FIX 2 repro: instantiate failure must abort, not fall through ─

    #[test]
    fn label_instantiate_abort() {
        // type Bit = O | I
        // iso f : Bit <-> Bit
        //   | b <-> $go b
        //   | $go O <-> x       ← clause 2 RHS: `x` is unbound
        //   | $go b <-> b
        //
        // When matching carrier value $go(O), clause 2 matches but its RHS
        // instantiation fails (unbound `x`). The elaborator must abort
        // immediately rather than silently falling through to clause 3.
        let src = "type Bit = O | I\niso f : Bit <-> Bit\n  | b <-> $go b\n  | $go O <-> x\n  | $go b <-> b\n";
        let prog = theseus_syntax::parse(src).expect("parses");
        let env = crate::env::resolve(&prog);
        let map = elaborate_program(&prog, &env);
        let result = map.get("f").expect("no iso f");
        assert!(
            result.is_err(),
            "instantiate failure should abort elaboration, got: {result:?}"
        );
    }
}
