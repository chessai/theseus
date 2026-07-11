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
use theseus_syntax::ast::{self, Ctor, Decl, Program, Type as SurfType};

/// One step of an injection path into a right-nested sum.
///
/// A right-nested sum `p0 + (p1 + (p2 + ...))` selects its `i`-th summand by
/// taking [`Inj::R`] `i` times and then [`Inj::L`] once — except the *last*
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
}

/// A primitive signature.
#[derive(Debug, Clone)]
pub struct PrimSig {
    pub lhs: SurfType,
    pub rhs: SurfType,
    pub key: String,
}

/// The resolution environment: indexed lookup tables plus type lowering.
///
/// Downstream stages consume this by reference; they must not need to rebuild
/// it. All maps are ordered ([`BTreeMap`]) so any iteration is deterministic
/// (ADR-0020).
#[derive(Debug, Clone, Default)]
pub struct Env {
    /// type name → its declared constructors.
    types: BTreeMap<String, TypeInfo>,
    /// constructor name → owning type, payload, injection path.
    ctors: BTreeMap<String, CtorInfo>,
    /// iso name → signature.
    isos: BTreeMap<String, IsoSig>,
    /// prim name → signature.
    prims: BTreeMap<String, PrimSig>,
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

    /// Is `name` a declared iso?
    pub fn has_iso(&self, name: &str) -> bool {
        self.isos.contains_key(name)
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
    pub fn lower(&self, ty: &SurfType) -> CoreType {
        match ty {
            SurfType::Empty(_) => CoreType::Zero,
            SurfType::Unit(_) => CoreType::One,
            SurfType::Var(_) => CoreType::Zero,
            SurfType::Paren(inner, _) => self.lower(inner),
            SurfType::Prod(a, b, _) => {
                CoreType::Prod(Box::new(self.lower(a)), Box::new(self.lower(b)))
            }
            SurfType::Sum(a, b, _) => {
                CoreType::Sum(Box::new(self.lower(a)), Box::new(self.lower(b)))
            }
            SurfType::Name(n) => self.lower_named(&n.text),
        }
    }

    /// Lower a named ADT into its right-nested sum-of-products.
    fn lower_named(&self, name: &str) -> CoreType {
        // Builtin U<n> types are not user-declared; intercept before ADT lookup.
        if let Some(ct) = lower_un(name) {
            return ct;
        }
        let Some(info) = self.types.get(name) else {
            // Unknown type: total fallback. Names reports the real error.
            return CoreType::Zero;
        };
        let summands: Vec<CoreType> = info
            .ctors
            .iter()
            .map(|c| {
                self.ctors
                    .get(c)
                    .and_then(|ci| ci.payload.as_ref())
                    .map(|p| self.lower(p))
                    .unwrap_or(CoreType::One)
            })
            .collect();
        right_nested_sum(&summands)
    }
}

/// Combine summands into a right-nested sum. Empty ⇒ `Zero`.
pub(crate) fn right_nested_sum(summands: &[CoreType]) -> CoreType {
    match summands {
        [] => CoreType::Zero,
        [last] => last.clone(),
        [head, rest @ ..] => {
            CoreType::Sum(Box::new(head.clone()), Box::new(right_nested_sum(rest)))
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
/// `U16`, … — builtin, not user-declared, so name resolution treats them as
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
/// * `U1 = 1 + 1`  (2 values — a single bit)
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
        CoreType::Sum(Box::new(CoreType::One), Box::new(CoreType::One))
    } else {
        // U<n> = U<n/2> × U<n/2>
        let half = lower_un_bits(n / 2);
        CoreType::Prod(Box::new(half.clone()), Box::new(half))
    }
}

/// Build the resolution environment for a program (infallible).
pub fn resolve(program: &Program) -> Env {
    let mut env = Env::default();
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
fn insert_iso_shallow(id: &ast::IsoDecl, env: &mut Env) {
    env.isos.insert(
        id.name.text.to_string(),
        IsoSig {
            lhs: id.lhs.clone(),
            rhs: id.rhs.clone(),
            labels: Vec::new(),
        },
    );
    for w in &id.wheres {
        insert_iso_shallow(w, env);
    }
}

/// Run label inference for an iso and its nested `where` isos, storing
/// the results in the env.
fn infer_labels_recursive(id: &ast::IsoDecl, env: &mut Env) {
    let (labels, _) = crate::labels::infer_iso_labels(id, env);
    if let Some(sig) = env.isos.get_mut(&id.name.text) {
        sig.labels = labels;
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

    #[test]
    fn non_power_of_two_is_not_builtin() {
        assert!(parse_un_bits("U3").is_none());
        assert!(parse_un_bits("U0").is_none());
        assert!(parse_un_bits("U5").is_none());
    }
}
