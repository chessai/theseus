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
fn right_nested_sum(summands: &[CoreType]) -> CoreType {
    match summands {
        [] => CoreType::Zero,
        [last] => last.clone(),
        [head, rest @ ..] => {
            CoreType::Sum(Box::new(head.clone()), Box::new(right_nested_sum(rest)))
        }
    }
}

/// Compute the injection path selecting constructor `index` of a type with
/// `count` constructors. See [`CtorInfo::path`] for the encoding.
fn injection_path(index: usize, count: usize) -> Vec<Inj> {
    if count <= 1 {
        return Vec::new();
    }
    let mut path = vec![Inj::R; index];
    if index < count - 1 {
        path.push(Inj::L);
    }
    path
}

/// The blessed stdlib numeric types `U<n>` (ADR-0017): `U1`, `U2`, `U4`, `U8`,
/// `U16`, … — builtin, not user-declared, so name resolution treats them as
/// known. (Their structural lowering is a later-task concern.)
fn is_builtin_type(name: &str) -> bool {
    name.strip_prefix('U')
        .is_some_and(|digits| !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit()))
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
                env.isos.insert(
                    id.name.text.to_string(),
                    IsoSig {
                        lhs: id.lhs.clone(),
                        rhs: id.rhs.clone(),
                    },
                );
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
    env
}

/// Extract the prim key as text. `PrimDecl::key` may itself be a string-ish
/// field; adapt via `Display`-free stringification to avoid formatting policy.
fn prim_key_text(pd: &ast::PrimDecl) -> String {
    pd.key.to_string()
}
