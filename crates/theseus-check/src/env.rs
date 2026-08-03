//! The resolution environment.
//!
//! [`resolve`] walks a [`Program`] and *indexes* every top-level declaration
//! into a set of lookup tables. It is **infallible**: it never reports errors.
//! Detecting *unresolved* references is the job of the Names check
//! ([`crate::names`]), which reads these tables. Resolution just builds them.
//!
//! The environment also owns the **surface → core type lowering**
//! ([`Env::lower`]) and the per-constructor **injection paths** that describe
//! where each constructor lives inside the core sum-of-products encoding of its
//! ADT. These two are the bridge that elaboration and coverage build on.

use std::collections::BTreeMap;

use theseus_core::prim::Value;
use theseus_core::ty::Type as CoreType;
use theseus_syntax::ast::{self, Ctor, Decl, Program, Span, Type as SurfType};

use crate::labels::Clash;

/// One step of an injection path into a right-nested sum.
///
/// A right-nested sum `p0 + (p1 + (p2 + ...))` selects its `i`-th summand by
/// taking [`Inj::R`] `i` times and then [`Inj::L`] once -- except the *last*
/// summand, which is reached by taking [`Inj::R`] `n-1` times (no trailing
/// `L`). See [`CtorInfo::path`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Inj {
    /// Left injection (`inl`).
    L,
    /// Right injection (`inr`).
    R,
}

/// Everything known about a constructor.
#[derive(Debug, Clone)]
pub struct CtorInfo {
    /// The type that declares this constructor.
    pub type_name: String,
    /// The 0-indexed position of this constructor within its type's ctor list.
    pub index: usize,
    /// The constructor's payload surface type, if any (`None` ⇒ nullary).
    pub payload: Option<SurfType>,
    /// The structural injection path selecting this constructor's summand in
    /// the core sum-of-products encoding of its owning type.
    ///
    /// For a type with `n` constructors `C0 | C1 | ... | C(n-1)` lowered to the
    /// right-nested sum `s0 + (s1 + ... + s(n-1))`:
    /// * ctor `i < n-1` ⇒ path = `R` repeated `i` times followed by `L`;
    /// * ctor `n-1` (last) ⇒ path = `R` repeated `n-1` times (no trailing `L`);
    /// * a type with a single ctor ⇒ empty path (the summand *is* the whole).
    pub path: Vec<Inj>,
}

/// A type declaration's essentials.
#[derive(Debug, Clone)]
pub struct TypeInfo {
    /// Constructor names in declaration order.
    pub ctors: Vec<String>,
}

/// An iso signature.
#[derive(Debug, Clone)]
pub struct IsoSig {
    pub lhs: SurfType,
    pub rhs: SurfType,
    /// Ordered label state space: `(label_name, carrier_type)` in
    /// first-occurrence order. Empty for plain (label-free) isos.
    pub labels: Vec<(String, SurfType)>,
    /// Carrier-unification clashes detected during label inference.
    pub label_clashes: Vec<Clash>,
}

/// A primitive signature.
#[derive(Debug, Clone)]
pub struct PrimSig {
    pub lhs: SurfType,
    pub rhs: SurfType,
    pub key: String,
}

/// A collision between two iso declarations that share the same bare name.
#[derive(Debug, Clone)]
pub struct IsoCollision {
    /// The duplicated iso name.
    pub name: String,
    /// Span of the second (colliding) declaration's name.
    pub span: Span,
}

/// The default bounded-unroll depth `D` for recursive (μ-) types
/// (v0.2 software dialect, increment 1 -- ADR-0035, design/mu-types.md).
///
/// A recursive nominal type is lowered by unrolling its fixpoint `D` times
/// and cutting off with `Zero` (the empty type), so the lowered type's
/// inhabitants are exactly the values of the recursive type with fewer than
/// `D` unfoldings -- e.g. `Nat` at `D = 4` lowers to `1+(1+(1+(1+0)))`, the
/// numerals `0..=3`. `4` keeps exhaustive coverage enumeration cheap even for
/// branching recursion (a binary tree at `D = 4` has 26 inhabitants) while
/// still exercising multiple levels of nesting.
pub const DEFAULT_UNROLL_DEPTH: usize = 4;

/// The resolution environment: indexed lookup tables plus type lowering.
///
/// Downstream stages consume this by reference; they must not need to rebuild
/// it. All maps are ordered ([`BTreeMap`]) so any iteration is deterministic
/// (ADR-0020).
#[derive(Debug, Clone)]
pub struct Env {
    /// type name → its declared constructors.
    types: BTreeMap<String, TypeInfo>,
    /// constructor name → owning type, payload, injection path.
    ctors: BTreeMap<String, CtorInfo>,
    /// iso name → signature.
    isos: BTreeMap<String, IsoSig>,
    /// prim name → signature.
    prims: BTreeMap<String, PrimSig>,
    /// Iso name collisions detected during resolution. The first iso wins;
    /// later isos with the same name are recorded here and NOT inserted.
    iso_collisions: Vec<IsoCollision>,
    /// Bounded-unroll depth `D` for recursive types (ADR-0035).
    unroll_depth: usize,
    /// Use-site depth annotations parsed from `Name@depth` syntax (ADR-0035
    /// follow-up). Maps `Span::start` of a type-name occurrence to the
    /// annotated depth. Populated during [`resolve`].
    depth_overrides: BTreeMap<usize, usize>,
}

impl Default for Env {
    fn default() -> Self {
        Env {
            types: BTreeMap::new(),
            ctors: BTreeMap::new(),
            isos: BTreeMap::new(),
            prims: BTreeMap::new(),
            iso_collisions: Vec::new(),
            unroll_depth: DEFAULT_UNROLL_DEPTH,
            depth_overrides: BTreeMap::new(),
        }
    }
}

impl Env {
    /// Look up a type declaration.
    pub fn type_info(&self, name: &str) -> Option<&TypeInfo> {
        self.types.get(name)
    }

    /// Is `name` a declared type?
    pub fn has_type(&self, name: &str) -> bool {
        self.types.contains_key(name) || is_builtin_type(name)
    }

    /// Look up a constructor.
    pub fn ctor(&self, name: &str) -> Option<&CtorInfo> {
        self.ctors.get(name)
    }

    /// Is `name` a declared constructor?
    pub fn has_ctor(&self, name: &str) -> bool {
        self.ctors.contains_key(name)
    }

    /// Look up an iso signature.
    pub fn iso(&self, name: &str) -> Option<&IsoSig> {
        self.isos.get(name)
    }

    /// Return the ordered `(label, carrier)` list for an iso.
    /// Empty slice for plain isos or unknown iso names.
    pub fn iso_labels(&self, iso_name: &str) -> &[(String, SurfType)] {
        self.isos
            .get(iso_name)
            .map(|sig| sig.labels.as_slice())
            .unwrap_or(&[])
    }

    /// Return the carrier-unification clashes for an iso.
    /// Empty slice for clash-free isos or unknown iso names.
    pub fn iso_label_clashes(&self, iso_name: &str) -> &[Clash] {
        self.isos
            .get(iso_name)
            .map(|sig| sig.label_clashes.as_slice())
            .unwrap_or(&[])
    }

    /// Is `name` a declared iso?
    pub fn has_iso(&self, name: &str) -> bool {
        self.isos.contains_key(name)
    }

    /// Return any iso name collisions detected during resolution.
    pub fn iso_collisions(&self) -> &[IsoCollision] {
        &self.iso_collisions
    }

    /// Look up a prim signature.
    pub fn prim(&self, name: &str) -> Option<&PrimSig> {
        self.prims.get(name)
    }

    /// Is `name` a declared prim?
    pub fn has_prim(&self, name: &str) -> bool {
        self.prims.contains_key(name)
    }

    /// Iterate declared type names in deterministic order.
    pub fn type_names(&self) -> impl Iterator<Item = &String> {
        self.types.keys()
    }

    /// Iterate declared iso names in deterministic order.
    pub fn iso_names(&self) -> impl Iterator<Item = &String> {
        self.isos.keys()
    }

    /// The bounded-unroll depth `D` used when lowering recursive types.
    pub fn unroll_depth(&self) -> usize {
        self.unroll_depth
    }

    /// Override the bounded-unroll depth `D` for recursive types.
    ///
    /// Callers (tests, future CLI flag) may tune the truncation; `D = 0`
    /// lowers every recursive type to `Zero` (no inhabitants).
    pub fn set_unroll_depth(&mut self, depth: usize) {
        self.unroll_depth = depth;
    }

    /// Register a use-site depth override for a type name at a specific
    /// source position. `span_start` is the `Span::start` of the `Name`
    /// type occurrence that carries the `@depth` annotation.
    pub fn set_depth_override(&mut self, span_start: usize, depth: usize) {
        self.depth_overrides.insert(span_start, depth);
    }

    /// Lower a surface type into its core [`CoreType`] encoding.
    ///
    /// * `Empty` ⇒ `Zero`, `Unit` ⇒ `One`.
    /// * `Prod(a, b)` ⇒ `Prod(lower a, lower b)`, `Sum(a, b)` ⇒
    ///   `Sum(lower a, lower b)`, `Paren(t)` ⇒ `lower t`.
    /// * `Var(_)` ⇒ `Zero` (type variables are not instantiable in slice 1;
    ///   this keeps lowering total).
    /// * `Name(T)` ⇒ the **sum-of-products** of `T`'s constructors: each
    ///   nullary ctor lowers to `One`, a ctor with payload `p` lowers to
    ///   `lower(p)`, and the resulting summands are combined right-nested:
    ///   `s0 + (s1 + (... + s(n-1)))`. So `Bit = O | I` ⇒ `1 + 1` and
    ///   `MaybeBit = Nothing | Just Bit` ⇒ `1 + (1 + 1)`. A type with no
    ///   constructors ⇒ `Zero`; a single-ctor type ⇒ just that summand.
    ///
    /// **Recursive (μ-) types** are lowered by **bounded unrolling**
    /// (ADR-0035): along any one path of the lowering, each named type may be
    /// entered at most `D = self.unroll_depth` times; a deeper re-entry lowers
    /// to `Zero`. The Core Π type algebra stays finite and unchanged, and the
    /// truncated type's inhabitants are exactly the recursive values with
    /// fewer than `D` unfoldings, encoded independently of `D` (a numeral `n`
    /// is `inr`ⁿ`(inl unit)` at every depth that admits it). The budget is
    /// per-path (restored on exit), so sibling occurrences of a type are
    /// unaffected and non-recursive programs lower exactly as before -- a type
    /// can only re-enter itself along a path if it is recursive.
    pub fn lower(&self, ty: &SurfType) -> CoreType {
        let mut fuel: BTreeMap<String, usize> = BTreeMap::new();
        self.lower_with(ty, &mut fuel)
    }

    /// Lowering worker: `fuel` tracks, per type name, the remaining re-entry
    /// budget along the current path (see [`Env::lower`]).
    fn lower_with(&self, ty: &SurfType, fuel: &mut BTreeMap<String, usize>) -> CoreType {
        match ty {
            SurfType::Empty(_) => CoreType::Zero,
            SurfType::Unit(_) => CoreType::One,
            SurfType::Var(_) => CoreType::Zero,
            SurfType::Paren(inner, _) => self.lower_with(inner, fuel),
            SurfType::Prod(a, b, _) => {
                CoreType::prod(self.lower_with(a, fuel), self.lower_with(b, fuel))
            }
            SurfType::Sum(a, b, _) => {
                CoreType::sum(self.lower_with(a, fuel), self.lower_with(b, fuel))
            }
            SurfType::Name(n) => {
                if let Some(&depth) = self.depth_overrides.get(&n.span.start) {
                    let old = fuel.insert(n.text.clone(), depth);
                    let result = self.lower_named(&n.text, fuel);
                    match old {
                        Some(v) => { fuel.insert(n.text.clone(), v); }
                        None => { fuel.remove(&n.text); }
                    }
                    result
                } else {
                    self.lower_named(&n.text, fuel)
                }
            }
        }
    }

    /// Lower a named ADT into its right-nested sum-of-products, spending one
    /// unit of the name's per-path unroll budget (μ-cutoff ⇒ `Zero`).
    fn lower_named(&self, name: &str, fuel: &mut BTreeMap<String, usize>) -> CoreType {
        // Builtin U<n> types are not user-declared; intercept before ADT lookup.
        if let Some(ct) = lower_un(name) {
            return ct;
        }
        let Some(info) = self.types.get(name) else {
            // Unknown type: total fallback. Names reports the real error.
            return CoreType::Zero;
        };
        let remaining = fuel
            .entry(name.to_string())
            .or_insert(self.unroll_depth);
        if *remaining == 0 {
            // μ-cutoff: a value would need ≥ D unfoldings of `name` -- outside
            // the truncated domain (empty, not junk: see ADR-0035).
            return CoreType::Zero;
        }
        *remaining -= 1;
        let summands: Vec<CoreType> = info
            .ctors
            .iter()
            .map(|c| {
                self.ctors
                    .get(c)
                    .and_then(|ci| ci.payload.as_ref())
                    .map(|p| self.lower_with(p, fuel))
                    .unwrap_or(CoreType::One)
            })
            .collect();
        // Restore the budget: it is per-path, not global (sibling occurrences
        // of `name` each get the full depth).
        *fuel.get_mut(name).expect("fuel entry inserted above") += 1;
        right_nested_sum(&summands)
    }
}

/// Combine summands into a right-nested sum. Empty ⇒ `Zero`.
pub(crate) fn right_nested_sum(summands: &[CoreType]) -> CoreType {
    match summands {
        [] => CoreType::Zero,
        [last] => last.clone(),
        [head, rest @ ..] => {
            CoreType::sum(head.clone(), right_nested_sum(rest))
        }
    }
}

/// Compute the injection path selecting the `index`-th summand of a
/// right-nested sum with `count` summands. See [`CtorInfo::path`] for the
/// encoding.
pub(crate) fn injection_path(index: usize, count: usize) -> Vec<Inj> {
    if count <= 1 {
        return Vec::new();
    }
    let mut path = vec![Inj::R; index];
    if index < count - 1 {
        path.push(Inj::L);
    }
    path
}

/// Build the label carrier core type `U = Σ_ℓ U_ℓ` as a right-nested sum
/// of `env.lower`'d carrier types.
pub(crate) fn label_carrier(labels: &[(String, SurfType)], env: &Env) -> CoreType {
    let summands: Vec<CoreType> = labels.iter().map(|(_, ty)| env.lower(ty)).collect();
    right_nested_sum(&summands)
}

/// Navigate a value through an injection path. Each [`Inj::L`] expects
/// [`Value::Left`], each [`Inj::R`] expects [`Value::Right`]. Returns the
/// payload at the end, or `None` on mismatch.
pub(crate) fn navigate<'a>(value: &'a Value, path: &[Inj]) -> Option<&'a Value> {
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

/// The blessed stdlib numeric types `U<n>` (ADR-0017): `U1`, `U2`, `U4`, `U8`,
/// `U16`, ... -- builtin, not user-declared, so name resolution treats them as
/// known.
fn is_builtin_type(name: &str) -> bool {
    parse_un_bits(name).is_some()
}

/// Parse a `U<n>` name and return the bit-width `n` if it is a valid builtin
/// numeric type (a power of two ≥ 1).
fn parse_un_bits(name: &str) -> Option<u32> {
    let digits = name.strip_prefix('U')?;
    if digits.is_empty() {
        return None;
    }
    let n: u32 = digits.parse().ok()?;
    if n == 0 || !n.is_power_of_two() {
        return None;
    }
    Some(n)
}

/// Lower a `U<n>` builtin numeric type to its core encoding (ADR-0017).
///
/// Fixed little-endian-pair representation:
/// * `U1 = 1 + 1`  (2 values -- a single bit)
/// * `U<n> = U<n/2> × U<n/2>`  for `n > 1`  (doubles the bit-width via product)
///
/// So `U2 = (1+1) × (1+1)` (4 values), `U4 = U2 × U2` (16 values), etc.
fn lower_un(name: &str) -> Option<CoreType> {
    let n = parse_un_bits(name)?;
    Some(lower_un_bits(n))
}

/// Recursive helper: build the core type for a `U<n>` with `n` bits.
fn lower_un_bits(n: u32) -> CoreType {
    if n == 1 {
        // U1 = 1 + 1
        CoreType::sum(CoreType::One, CoreType::One)
    } else {
        // U<n> = U<n/2> × U<n/2>
        let half = lower_un_bits(n / 2);
        CoreType::prod(half.clone(), half)
    }
}

/// Build the resolution environment for a program.
///
/// Resolution itself does not emit diagnostics, but it records iso-name
/// collisions (see [`Env::iso_collisions`]) for downstream stages to surface.
pub fn resolve(program: &Program) -> Env {
    let mut env = Env {
        depth_overrides: program.depth_overrides.clone(),
        ..Env::default()
    };
    for decl in &program.decls {
        match decl {
            Decl::Type(td) => {
                let type_name = td.name.text.to_string();
                let ctor_names: Vec<String> =
                    td.ctors.iter().map(|c| c.name.text.to_string()).collect();
                let count = ctor_names.len();
                for (index, Ctor { name, payload, .. }) in td.ctors.iter().enumerate() {
                    let cname = name.text.to_string();
                    env.ctors.insert(
                        cname,
                        CtorInfo {
                            type_name: type_name.clone(),
                            index,
                            payload: payload.clone(),
                            path: injection_path(index, count),
                        },
                    );
                }
                env.types.insert(type_name, TypeInfo { ctors: ctor_names });
            }
            Decl::Iso(id) => {
                insert_iso_shallow(id, &mut env);
            }
            Decl::Prim(pd) => {
                env.prims.insert(
                    pd.name.text.to_string(),
                    PrimSig {
                        lhs: pd.lhs.clone(),
                        rhs: pd.rhs.clone(),
                        key: prim_key_text(pd),
                    },
                );
            }
        }
    }
    // Second pass: label inference (needs types/ctors populated).
    for decl in &program.decls {
        if let Decl::Iso(id) = decl {
            infer_labels_recursive(id, &mut env);
        }
    }
    env
}

/// Insert an iso (and its nested `where` isos) into the env without labels.
/// If an iso with the same name already exists, the collision is recorded
/// and the duplicate is NOT inserted (the first declaration wins).
fn insert_iso_shallow(id: &ast::IsoDecl, env: &mut Env) {
    let name = id.name.text.to_string();
    match env.isos.entry(name) {
        std::collections::btree_map::Entry::Occupied(e) => {
            env.iso_collisions.push(IsoCollision {
                name: e.key().clone(),
                span: id.name.span,
            });
        }
        std::collections::btree_map::Entry::Vacant(e) => {
            e.insert(IsoSig {
                lhs: id.lhs.clone(),
                rhs: id.rhs.clone(),
                labels: Vec::new(),
                label_clashes: Vec::new(),
            });
        }
    }
    for w in &id.wheres {
        insert_iso_shallow(w, env);
    }
}

/// Run label inference for an iso and its nested `where` isos, storing
/// the results (labels AND clashes) in the env.
fn infer_labels_recursive(id: &ast::IsoDecl, env: &mut Env) {
    let (labels, clashes) = crate::labels::infer_iso_labels(id, env);
    if let Some(sig) = env.isos.get_mut(&id.name.text) {
        sig.labels = labels;
        sig.label_clashes = clashes;
    }
    for w in &id.wheres {
        infer_labels_recursive(w, env);
    }
}

/// Extract the prim key as text. `PrimDecl::key` may itself be a string-ish
/// field; adapt via `Display`-free stringification to avoid formatting policy.
fn prim_key_text(pd: &ast::PrimDecl) -> String {
    pd.key.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use theseus_core::prim::enumerate_values;

    #[test]
    fn lower_u1_has_2_values() {
        let env = Env::default();
        let ty = SurfType::Name(ast::UpperName {
            text: "U1".into(),
            span: ast::Span { start: 0, end: 0 },
        });
        let core = env.lower(&ty);
        assert_eq!(enumerate_values(&core).len(), 2);
    }

    #[test]
    fn lower_u2_has_4_values() {
        let env = Env::default();
        let ty = SurfType::Name(ast::UpperName {
            text: "U2".into(),
            span: ast::Span { start: 0, end: 0 },
        });
        let core = env.lower(&ty);
        assert_eq!(enumerate_values(&core).len(), 4);
    }

    #[test]
    fn lower_u4_has_16_values() {
        let env = Env::default();
        let ty = SurfType::Name(ast::UpperName {
            text: "U4".into(),
            span: ast::Span { start: 0, end: 0 },
        });
        let core = env.lower(&ty);
        assert_eq!(enumerate_values(&core).len(), 16);
    }

    #[test]
    fn lower_u8_has_256_values() {
        let env = Env::default();
        let ty = SurfType::Name(ast::UpperName {
            text: "U8".into(),
            span: ast::Span { start: 0, end: 0 },
        });
        let core = env.lower(&ty);
        assert_eq!(enumerate_values(&core).len(), 256);
    }

    // ── μ-types: bounded-unroll lowering (ADR-0035) ─────────────────────

    fn named(text: &str) -> SurfType {
        SurfType::Name(ast::UpperName {
            text: text.into(),
            span: ast::Span { start: 0, end: 0 },
        })
    }

    #[test]
    fn recursive_nat_lowers_to_bounded_unroll_zero_cutoff() {
        let program = theseus_syntax::parse("type Nat = Zero | Succ Nat\n").unwrap();
        let env = resolve(&program);
        let core = env.lower(&named("Nat"));
        // D = 4: Nat ⇒ 1 + (1 + (1 + (1 + 0))) -- numerals 0..=3.
        let expected = CoreType::sum(
            CoreType::One,
            CoreType::sum(
                CoreType::One,
                CoreType::sum(
                    CoreType::One,
                    CoreType::sum(CoreType::One, CoreType::Zero),
                ),
            ),
        );
        assert_eq!(core, expected);
        assert_eq!(enumerate_values(&core).len(), DEFAULT_UNROLL_DEPTH);
    }

    #[test]
    fn recursive_depth_is_configurable() {
        let program = theseus_syntax::parse("type Nat = Zero | Succ Nat\n").unwrap();
        let mut env = resolve(&program);
        for d in [0usize, 1, 2, 6] {
            env.set_unroll_depth(d);
            let core = env.lower(&named("Nat"));
            assert_eq!(
                enumerate_values(&core).len(),
                d,
                "Nat at depth {d} must have exactly {d} inhabitants"
            );
        }
    }

    #[test]
    fn recursive_value_encoding_is_depth_independent() {
        // The numeral n encodes as inrⁿ(inl unit) at EVERY depth that admits
        // it -- the truncated domains nest as prefixes. This is what lets
        // coverage/elaboration thread payload types without depth bookkeeping.
        let program = theseus_syntax::parse("type Nat = Zero | Succ Nat\n").unwrap();
        let mut env = resolve(&program);
        env.set_unroll_depth(3);
        let shallow = enumerate_values(&env.lower(&named("Nat")));
        env.set_unroll_depth(5);
        let deep = enumerate_values(&env.lower(&named("Nat")));
        assert_eq!(shallow.as_slice(), &deep[..shallow.len()]);
    }

    #[test]
    fn mutually_recursive_types_terminate() {
        // A ↔ B: each name has its own per-path budget of D, so the unroll
        // alternates until both are spent: 2·D unit summands, then the cutoff.
        let src = "type A = Za | Sa B\ntype B = Zb | Sb A\n";
        let program = theseus_syntax::parse(src).unwrap();
        let env = resolve(&program);
        let core = env.lower(&named("A"));
        assert_eq!(enumerate_values(&core).len(), 2 * DEFAULT_UNROLL_DEPTH);
    }

    #[test]
    fn branching_recursion_terminates_with_per_path_budget() {
        // Binary tree: t(d) = 1 + t(d-1)², t(0) = 0 ⇒ t(4) = 26. The budget
        // must be restored between the two subtree occurrences (per-path,
        // not global) for this count to come out right.
        let src = "type Tree = Leaf | Node (Tree * Tree)\n";
        let program = theseus_syntax::parse(src).unwrap();
        let env = resolve(&program);
        let core = env.lower(&named("Tree"));
        assert_eq!(enumerate_values(&core).len(), 26);
    }

    #[test]
    fn nonrecursive_lowering_unaffected_by_depth() {
        // Non-recursive types never re-enter themselves along a path, so even
        // depth 1 leaves them untouched.
        let src = "type Bit = O | I\ntype MaybeBit = Nothing | Just Bit\n";
        let program = theseus_syntax::parse(src).unwrap();
        let mut env = resolve(&program);
        env.set_unroll_depth(1);
        assert_eq!(enumerate_values(&env.lower(&named("MaybeBit"))).len(), 3);
        assert_eq!(enumerate_values(&env.lower(&named("Bit"))).len(), 2);
    }

    #[test]
    fn non_power_of_two_is_not_builtin() {
        assert!(parse_un_bits("U3").is_none());
        assert!(parse_un_bits("U0").is_none());
        assert!(parse_un_bits("U5").is_none());
    }

    #[test]
    fn duplicate_iso_name_detected_as_collision() {
        // Two where-isos with the same name should be detected, not silently
        // overwritten. The first declaration wins; the second is recorded as
        // a collision.
        let src = "\
type Bit = O | I

iso outer1 : Bit <-> Bit
| O <-> I
| I <-> O
where
  iso helper : Bit <-> Bit
  | O <-> O
  | I <-> I

iso outer2 : Bit <-> Bit
| O <-> I
| I <-> O
where
  iso helper : Bit <-> Bit
  | O <-> I
  | I <-> O
";
        let program = theseus_syntax::parse(src).unwrap();
        let env = resolve(&program);
        let collisions = env.iso_collisions();
        assert_eq!(
            collisions.len(),
            1,
            "expected exactly one iso-name collision, got {collisions:?}"
        );
        assert_eq!(collisions[0].name, "helper");
        // The first `helper` should still be accessible (not overwritten).
        let sig = env.iso("helper").expect("first 'helper' should survive");
        // It should have the first declaration's signature (identity iso: O<->O, I<->I).
        // Both have the same lhs/rhs (Bit <-> Bit), so just check it exists.
        assert!(matches!(&sig.lhs, SurfType::Name(n) if n.text == "Bit"));
    }

    #[test]
    fn duplicate_iso_name_emits_t0101() {
        // End-to-end: the collision surfaces as a T0101 diagnostic via typing::check.
        let src = "\
type Bit = O | I

iso outer1 : Bit <-> Bit
| O <-> I
| I <-> O
where
  iso helper : Bit <-> Bit
  | O <-> O
  | I <-> I

iso outer2 : Bit <-> Bit
| O <-> I
| I <-> O
where
  iso helper : Bit <-> Bit
  | O <-> I
  | I <-> O
";
        let program = theseus_syntax::parse(src).unwrap();
        let env = resolve(&program);
        let diags = crate::typing::check(&program, &env);
        let collision_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.code == crate::diagnostics::Code::T0101 && d.name.as_deref() == Some("helper"))
            .collect();
        assert_eq!(
            collision_diags.len(),
            1,
            "expected exactly one T0101 for duplicate iso 'helper', got {collision_diags:?}"
        );
    }
}
