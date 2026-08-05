//! Prim registry: opaque `(forward, backward)` pairs with mutual-inverse
//! admission (ADR-0013, amended).
//!
//! A `prim` supplies a pair of finite functions. It is admitted **only if
//! the pair are mutual inverses**: `backward ∘ forward = id_A` and
//! `forward ∘ backward = id_B` over the finite domains -- not merely that
//! each is individually bijective.

use crate::bvexpr::BvExpr;
use crate::ty::Type;
use std::collections::BTreeMap;

/// A typed value in the Core Π value domain (initial algebra of the type
/// formers). Minimal representation for prim admission checking;
/// `theseus-eval` owns the full evaluation semantics.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Value {
    /// The sole inhabitant of `1`.
    Unit,
    /// Left injection into `a + b`.
    Left(Box<Value>),
    /// Right injection into `a + b`.
    Right(Box<Value>),
    /// Pair in `a × b`.
    Pair(Box<Value>, Box<Value>),
}

impl Value {
    pub fn left(v: Value) -> Self {
        Value::Left(Box::new(v))
    }

    pub fn right(v: Value) -> Self {
        Value::Right(Box::new(v))
    }

    pub fn pair(a: Value, b: Value) -> Self {
        Value::Pair(Box::new(a), Box::new(b))
    }
}

/// Enumerate all inhabitants of a finite type.
pub fn enumerate_values(ty: &Type) -> Vec<Value> {
    match ty {
        Type::Zero => vec![],
        Type::One => vec![Value::Unit],
        Type::Sum(a, b) => {
            let mut vals: Vec<Value> = enumerate_values(a).into_iter().map(Value::left).collect();
            vals.extend(enumerate_values(b).into_iter().map(Value::right));
            vals
        }
        Type::Prod(a, b) => {
            let bs = enumerate_values(b);
            let mut vals = Vec::new();
            for va in enumerate_values(a) {
                for vb in &bs {
                    vals.push(Value::pair(va.clone(), vb.clone()));
                }
            }
            vals
        }
        Type::Flat(n) => {
            debug_assert!(*n <= 8, "Flat({n}): width must be <= 8");
            let card = 1usize << n;
            (0..card)
                .map(|k| {
                    // Right^k(Left(Unit)) -- same thermometer encoding as numeral_ty
                    let mut v = Value::left(Value::Unit);
                    for _ in 0..k {
                        v = Value::right(v);
                    }
                    v
                })
                .collect()
        }
    }
}

/// A symbolic bit-level transfer for one direction of a prim.
///
/// `leaves[i][b]` is the `BvExpr` for bit `b` of output leaf `i`, expressed
/// over `(operand, bit)` input variables. `width` is the bit-width of each
/// leaf (1..=8, matching `Flat(width)`). Bit convention: LSB-first leaf byte
/// (identical to `value_to_leaf_bytes` / `VarMap`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SpecTransfer {
    /// Per-output-leaf, per-bit expressions. `leaves[leaf_idx][bit_idx]`.
    pub leaves: Vec<Vec<BvExpr>>,
    /// Bit-width of each leaf (1..=8).
    pub width: u8,
}

/// A symbolic prim spec: a `(forward, backward)` pair of `SpecTransfer`.
///
/// Both directions are mandatory because `adjoint()` is total and infallible --
/// a symbolic prim MUST carry a constructive inverse so adjoint can swap them.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimSpec {
    pub forward: SpecTransfer,
    pub backward: SpecTransfer,
}

/// Definition of a primitive iso: a named `(forward, backward)` pair of
/// finite functions together with their domain and codomain types.
///
/// The adjoint swaps both names and maps:
/// `def.adjoint().adjoint() == def` (involution).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimDef {
    pub forward_name: String,
    pub backward_name: String,
    pub input_ty: Type,
    pub output_ty: Type,
    pub forward: BTreeMap<Value, Value>,
    pub backward: BTreeMap<Value, Value>,
    /// Optional symbolic spec. When present, the prim can be evaluated/admitted
    /// without materializing a full table (breaks the 2^24 wall for wide ops).
    pub symbolic: Option<PrimSpec>,
}

impl PrimDef {
    /// Return the adjoint prim: swap forward/backward and input/output types.
    pub fn adjoint(&self) -> Self {
        PrimDef {
            forward_name: self.backward_name.clone(),
            backward_name: self.forward_name.clone(),
            input_ty: self.output_ty.clone(),
            output_ty: self.input_ty.clone(),
            forward: self.backward.clone(),
            backward: self.forward.clone(),
            symbolic: self.symbolic.as_ref().map(|spec| PrimSpec {
                forward: spec.backward.clone(),
                backward: spec.forward.clone(),
            }),
        }
    }
}

/// Error from prim admission.
// `Value` does not implement `Display`, so Value-carrying variants render it
// with `{:?}` (Debug); the rest use plain text.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum AdmitError {
    /// `forward` is not defined for a value in the input type's domain.
    #[error("forward is not total: undefined on input {0:?}")]
    ForwardNotTotal(Value),
    /// `backward` is not defined for a value in the output type's domain.
    #[error("backward is not total: undefined on output {0:?}")]
    BackwardNotTotal(Value),
    /// `backward(forward(v)) ≠ v`.
    #[error("backward(forward(v)) != v for input {input:?}")]
    BackwardOfForward { input: Value },
    /// `forward(backward(w)) ≠ w`.
    #[error("forward(backward(w)) != w for input {input:?}")]
    ForwardOfBackward { input: Value },
    /// The prim has a symbolic spec but no table -- core::admit cannot certify
    /// bijectivity (no BDD machinery here). The caller should use
    /// `bveq::admit_symbolic` in theseus-tisc instead.
    #[error("prim has a symbolic spec but no table: core::admit cannot certify bijectivity")]
    SymbolicOnly,
    /// The prim has both a table and a spec, but eval-via-spec disagrees with
    /// the table on at least one input.
    #[error("spec and table disagree on input {input:?}")]
    SpecTableMismatch {
        /// The input value where the spec and table disagree.
        input: Value,
    },
    /// The symbolic spec is invalid w.r.t. the prim's types (F3).
    #[error("invalid symbolic spec: {0}")]
    InvalidSpec(String),
    /// The prim has a spec AND a partial table (not covering the full domain),
    /// which would bypass the hang guard (F2).
    #[error("prim has a spec and a partial table (would bypass the hang guard)")]
    PartialTableWithSpec,
}

/// Check that a [`PrimDef`] is admissible: the `(forward, backward)` pair
/// are mutual inverses over the finite domain.
///
/// Checks both `backward ∘ forward = id_A` and `forward ∘ backward = id_B`.
///
/// **Pre-checks** (before any enumeration):
/// - F3: validates the symbolic spec against the prim's types.
/// - F2: arithmetic domain-cardinality precheck -- rejects symbolic prims
///   with an incomplete table (the partial-table+spec hang bypass).
/// - Symbolic-only (table empty, spec present) → `SymbolicOnly`.
///
/// **Both-present prims** (table AND spec) verify spec==table over the table
/// domain (forward AND backward) after the mutual-inverse check.
pub fn admit(def: &PrimDef) -> Result<(), AdmitError> {
    // F3: validate spec against the prim's types FIRST (before any enumeration).
    if let Some(spec) = &def.symbolic {
        spec.validate_against(&def.input_ty, &def.output_ty)
            .map_err(AdmitError::InvalidSpec)?;
    }

    // F2: arithmetic domain-cardinality precheck. If the prim has a spec
    // but the table is not the complete domain, reject BEFORE enumerate_values.
    // This catches the partial-table+spec bypass of the old hang guard.
    if def.symbolic.is_some() {
        let card = crate::enumerate::cardinality_u64(&def.input_ty);
        if def.forward.len() as u64 != card {
            if def.forward.is_empty() {
                return Err(AdmitError::SymbolicOnly);
            }
            return Err(AdmitError::PartialTableWithSpec);
        }
    }

    let domain = enumerate_values(&def.input_ty);
    let codomain = enumerate_values(&def.output_ty);

    // forward must be total over domain
    for v in &domain {
        if !def.forward.contains_key(v) {
            return Err(AdmitError::ForwardNotTotal(v.clone()));
        }
    }

    // backward must be total over codomain
    for w in &codomain {
        if !def.backward.contains_key(w) {
            return Err(AdmitError::BackwardNotTotal(w.clone()));
        }
    }

    // backward ∘ forward = id_A
    for v in &domain {
        let w = &def.forward[v];
        match def.backward.get(w) {
            Some(v2) if v2 == v => {}
            _ => return Err(AdmitError::BackwardOfForward { input: v.clone() }),
        }
    }

    // forward ∘ backward = id_B
    for w in &codomain {
        let v = &def.backward[w];
        match def.forward.get(v) {
            Some(w2) if w2 == w => {}
            _ => return Err(AdmitError::ForwardOfBackward { input: w.clone() }),
        }
    }

    // Both-present: verify spec==table over the table domain.
    if let Some(spec) = &def.symbolic {
        // Forward spec vs forward table.
        for (in_val, table_out) in &def.forward {
            let in_bytes =
                value_to_leaf_bytes(in_val, &def.input_ty).map_err(AdmitError::InvalidSpec)?;
            let out_bytes = spec.forward.eval_bytes(&in_bytes);
            let spec_out = value_from_leaf_bytes(&out_bytes, &def.output_ty);
            if &spec_out != table_out {
                return Err(AdmitError::SpecTableMismatch {
                    input: in_val.clone(),
                });
            }
        }
        // F4: backward spec vs backward table.
        for (in_val, table_out) in &def.backward {
            let in_bytes =
                value_to_leaf_bytes(in_val, &def.output_ty).map_err(AdmitError::InvalidSpec)?;
            let out_bytes = spec.backward.eval_bytes(&in_bytes);
            let spec_out = value_from_leaf_bytes(&out_bytes, &def.input_ty);
            if &spec_out != table_out {
                return Err(AdmitError::SpecTableMismatch {
                    input: in_val.clone(),
                });
            }
        }
    }

    Ok(())
}

/// Decompose a Value into leaf bytes (LSB-first, matching VarMap convention).
/// Core-native: no BDD dependency.
///
/// Returns `Err` if the value does not inhabit `ty` (wrong shape for a `Sum`,
/// `Prod`, or `Flat`, or a `Flat` leaf out of range) -- these are reachable
/// from user input via the evaluator, so they are errors rather than panics.
pub fn value_to_leaf_bytes(v: &Value, ty: &Type) -> Result<Vec<u8>, String> {
    match ty {
        Type::Zero => Err("value_to_leaf_bytes: Zero type".into()),
        Type::One => Ok(vec![]),
        Type::Sum(a, b) => {
            let la = count_leaves(a);
            let lb = count_leaves(b);
            let max_payload = la.max(lb);
            let total = 1 + max_payload;
            match v {
                Value::Left(inner) => {
                    let mut bytes = vec![0u8]; // tag = 0 (Left)
                    bytes.extend(value_to_leaf_bytes(inner, a)?);
                    bytes.resize(total, 0);
                    Ok(bytes)
                }
                Value::Right(inner) => {
                    let mut bytes = vec![1u8]; // tag = 1 (Right)
                    bytes.extend(value_to_leaf_bytes(inner, b)?);
                    bytes.resize(total, 0);
                    Ok(bytes)
                }
                _ => Err(format!(
                    "value_to_leaf_bytes: expected Left/Right for Sum, got {v:?}"
                )),
            }
        }
        Type::Prod(a, b) => {
            let Value::Pair(va, vb) = v else {
                return Err(format!(
                    "value_to_leaf_bytes: expected Pair for Prod, got {v:?}"
                ));
            };
            let mut bytes = value_to_leaf_bytes(va, a)?;
            bytes.extend(value_to_leaf_bytes(vb, b)?);
            Ok(bytes)
        }
        Type::Flat(n) => {
            // thermometer Right^k(Left(Unit)) → k as byte
            let mut k = 0u8;
            let mut cur = v;
            loop {
                match cur {
                    Value::Left(u) if u.as_ref() == &Value::Unit => break,
                    Value::Right(rest) => {
                        k += 1;
                        cur = rest;
                    }
                    _ => return Err(format!("value_to_leaf_bytes: not a Flat leaf: {v:?}")),
                }
            }
            if (k as u64) >= (1u64 << n) {
                return Err(format!("leaf byte {k} out of range for Flat({n})"));
            }
            Ok(vec![k])
        }
    }
}

/// Recompose leaf bytes into a Value (inverse of `value_to_leaf_bytes`).
/// Core-native: no BDD dependency.
///
/// Panics (in all build modes) if a leaf byte is out of range for its
/// `Flat(n)` type. Use [`try_value_from_leaf_bytes`] for a non-panicking
/// version that returns a `Result`.
pub fn value_from_leaf_bytes(bytes: &[u8], ty: &Type) -> Value {
    try_value_from_leaf_bytes(bytes, ty).expect("value_from_leaf_bytes: out-of-range byte")
}

/// Non-panicking version of [`value_from_leaf_bytes`]: returns `Err` if a
/// leaf byte is out of range for its `Flat(n)` type (F3 hardening).
pub fn try_value_from_leaf_bytes(bytes: &[u8], ty: &Type) -> Result<Value, String> {
    try_value_from_leaf_bytes_inner(bytes, ty).map(|(v, _)| v)
}

fn try_value_from_leaf_bytes_inner(bytes: &[u8], ty: &Type) -> Result<(Value, usize), String> {
    match ty {
        Type::Zero => Err("value_from_leaf_bytes: Zero type".into()),
        Type::One => Ok((Value::Unit, 0)),
        Type::Sum(a, b) => {
            let la = count_leaves(a);
            let lb = count_leaves(b);
            let max_payload = la.max(lb);
            let tag = bytes[0];
            if tag == 0 {
                let (v, _) = try_value_from_leaf_bytes_inner(&bytes[1..], a)?;
                Ok((Value::Left(Box::new(v)), 1 + max_payload))
            } else {
                let (v, _) = try_value_from_leaf_bytes_inner(&bytes[1..], b)?;
                Ok((Value::Right(Box::new(v)), 1 + max_payload))
            }
        }
        Type::Prod(a, b) => {
            let (va, na) = try_value_from_leaf_bytes_inner(bytes, a)?;
            let (vb, nb) = try_value_from_leaf_bytes_inner(&bytes[na..], b)?;
            Ok((Value::pair(va, vb), na + nb))
        }
        Type::Flat(n) => {
            let byte = bytes[0];
            if (byte as u64) >= (1u64 << n) {
                return Err(format!(
                    "value_from_leaf_bytes: byte {byte} out of range for Flat({n})"
                ));
            }
            let mut v = Value::left(Value::Unit);
            for _ in 0..byte {
                v = Value::right(v);
            }
            Ok((v, 1))
        }
    }
}

impl SpecTransfer {
    /// Evaluate this transfer on concrete leaf bytes (the shared
    /// decompose→eval_expr→recompose core).
    ///
    /// `in_bytes` is the leaf-byte decomposition of the input value
    /// (from `value_to_leaf_bytes`). Returns the output leaf bytes
    /// (suitable for `value_from_leaf_bytes`).
    pub fn eval_bytes(&self, in_bytes: &[u8]) -> Vec<u8> {
        let width = self.width as usize;
        let num_leaves = self.leaves.len();
        let mut out = vec![0u8; num_leaves];
        for (leaf, ob) in out.iter_mut().enumerate() {
            for b in 0..width {
                if crate::bvexpr::eval_expr(&self.leaves[leaf][b], in_bytes) {
                    *ob |= 1 << b;
                }
            }
        }
        out
    }

    /// Validate this transfer against the type it operates on (F3).
    /// Checks: width vs Flat(w), leaves.len() vs leaf count, each row len == width.
    fn validate_against_type(&self, ty: &Type, direction: &str) -> Result<(), String> {
        let expected_leaves = count_leaves(ty);
        if self.leaves.len() != expected_leaves {
            return Err(format!(
                "{direction} spec: leaves.len()={} but type has {expected_leaves} leaves",
                self.leaves.len()
            ));
        }
        let w = self.width as usize;
        // Check each leaf has exactly `width` bits.
        for (i, row) in self.leaves.iter().enumerate() {
            if row.len() != w {
                return Err(format!(
                    "{direction} spec: leaf[{i}] has {} bits but width={w}",
                    row.len()
                ));
            }
        }
        // Check width vs each Flat leaf in the type.
        validate_width_vs_type(ty, self.width, direction)?;
        Ok(())
    }
}

/// Walk the type and check that every Flat(n) leaf has n == width.
fn validate_width_vs_type(ty: &Type, width: u8, direction: &str) -> Result<(), String> {
    match ty {
        Type::Zero | Type::One => Ok(()),
        Type::Sum(a, b) => {
            validate_width_vs_type(a, width, direction)?;
            validate_width_vs_type(b, width, direction)
        }
        Type::Prod(a, b) => {
            validate_width_vs_type(a, width, direction)?;
            validate_width_vs_type(b, width, direction)
        }
        Type::Flat(n) => {
            if *n != width {
                Err(format!(
                    "{direction} spec: width={width} but type has Flat({n})"
                ))
            } else {
                Ok(())
            }
        }
    }
}

impl PrimSpec {
    /// Validate the spec against the prim's input and output types (F3).
    pub fn validate_against(&self, input_ty: &Type, output_ty: &Type) -> Result<(), String> {
        // Forward: leaves match output type, width matches both types.
        self.forward.validate_against_type(output_ty, "forward")?;
        validate_width_vs_type(input_ty, self.forward.width, "forward (input side)")?;
        // R1: validate Var indices in forward exprs (reference input leaves).
        let n_input_leaves = count_leaves(input_ty);
        let fw = self.forward.width as usize;
        for (i, row) in self.forward.leaves.iter().enumerate() {
            for (b, expr) in row.iter().enumerate() {
                validate_expr_var_indices(expr, n_input_leaves, fw)
                    .map_err(|e| format!("forward spec leaf[{i}][{b}]: {e}"))?;
            }
        }
        // Backward: leaves match input type, width matches both types.
        self.backward.validate_against_type(input_ty, "backward")?;
        validate_width_vs_type(output_ty, self.backward.width, "backward (output side)")?;
        // R1: validate Var indices in backward exprs (reference output leaves).
        let n_output_leaves = count_leaves(output_ty);
        let bw = self.backward.width as usize;
        for (i, row) in self.backward.leaves.iter().enumerate() {
            for (b, expr) in row.iter().enumerate() {
                validate_expr_var_indices(expr, n_output_leaves, bw)
                    .map_err(|e| format!("backward spec leaf[{i}][{b}]: {e}"))?;
            }
        }
        Ok(())
    }
}

/// Walk a `BvExpr` tree and validate that all `Var` indices are in range (R1).
/// Rejects `operand >= max_operand` (would index-panic in eval_expr) and
/// `bit >= max_bit` (release-masked silent wrong-bit).
fn validate_expr_var_indices(
    expr: &crate::bvexpr::BvExpr,
    max_operand: usize,
    max_bit: usize,
) -> Result<(), String> {
    use crate::bvexpr::BvExpr;
    match expr {
        BvExpr::Var { operand, bit } => {
            if *operand >= max_operand {
                return Err(format!(
                    "Var operand index {operand} >= input leaf count {max_operand}"
                ));
            }
            if *bit >= max_bit {
                return Err(format!("Var bit index {bit} >= width {max_bit}"));
            }
            Ok(())
        }
        BvExpr::Const(_) => Ok(()),
        BvExpr::Not(a) => validate_expr_var_indices(a, max_operand, max_bit),
        BvExpr::Xor(a, b) | BvExpr::And(a, b) | BvExpr::Or(a, b) => {
            validate_expr_var_indices(a, max_operand, max_bit)?;
            validate_expr_var_indices(b, max_operand, max_bit)
        }
        BvExpr::Mux { sel, then_, else_ } => {
            validate_expr_var_indices(sel, max_operand, max_bit)?;
            validate_expr_var_indices(then_, max_operand, max_bit)?;
            validate_expr_var_indices(else_, max_operand, max_bit)
        }
    }
}

// ── Builtin prim registry (ADR-0047) ────────────────────────────────
//
// A `prim name : T <-> T = "key"` declaration is opaque until its key resolves
// to a concrete `(forward, backward)` pair. Builtin keys resolve here: given the
// declared (lowered) input/output types, [`builtin_prim`] materializes the
// `PrimDef` the elaborator wraps in `Iso::Prim`. Today the only builtin is the
// reversible word add.

/// The reversible word-add prim key: `(a, b) |-> (a, a + b mod 2^w)` over a pair
/// of `W<n>` machine words (a `Flat(w)` leaf each). The addend `a` is preserved,
/// so the map is a bijection; the inverse subtracts. At `w = 8` the TISC compiler
/// recognizes it as a single reversible `Add` instruction.
pub const ADD_INTO_KEY: &str = "theseus.arith.add_into";

/// Resolve a builtin prim `key` against its declared (lowered) `input_ty` and
/// `output_ty`, returning the concrete [`PrimDef`] or `None` if the key is
/// unknown or the types do not fit the builtin's shape.
///
/// The returned def carries a symbolic [`PrimSpec`] (no materialized table). The
/// caller validates it structurally via [`PrimSpec::validate_against`]; full
/// mutual-inverse certification is the compiler's symbolic BVEQ (`admit_symbolic`
/// in theseus-tisc), since `core::admit` cannot certify a symbolic-only prim.
pub fn builtin_prim(key: &str, input_ty: &Type, output_ty: &Type) -> Option<PrimDef> {
    match key {
        ADD_INTO_KEY => {
            // add_into is an endo-prim over a homogeneous word pair.
            if input_ty != output_ty {
                return None;
            }
            let w = flat_pair_width(input_ty)?;
            Some(build_add_into(w))
        }
        _ => None,
    }
}

/// If `ty` is `Prod(Flat(w), Flat(w))` (a pair of equal-width words), return `w`.
fn flat_pair_width(ty: &Type) -> Option<u8> {
    match ty {
        Type::Prod(a, b) => match (a.as_ref(), b.as_ref()) {
            (Type::Flat(wa), Type::Flat(wb)) if wa == wb => Some(*wa),
            _ => None,
        },
        _ => None,
    }
}

/// Build the `add_into` symbolic prim at width `w` (1..=8):
/// `(a, b) |-> (a, (a + b) mod 2^w)`, inverse `(a, s) |-> (a, (s - a) mod 2^w)`.
///
/// A **symbolic** spec (bit-level ripple-carry `BvExpr`s), not a table: the
/// `Flat(w)` value domain is `2^w` deep-thermometer values, so a materialized
/// `2^w x 2^w` table is quadratically large and heap-heavy. The spec is `O(w)`
/// tiny expressions -- eval decodes each input to bytes on demand, and the TISC
/// compiler certifies + recognizes it symbolically (BDD) rather than enumerating.
fn build_add_into(w: u8) -> PrimDef {
    debug_assert!((1..=8).contains(&w), "add_into width must be 1..=8");
    let f = Type::Flat(w);
    let ty = Type::prod(f.clone(), f);
    let spec = PrimSpec {
        forward: add_transfer(w),
        backward: sub_transfer(w),
    };
    PrimDef {
        forward_name: "add_into".into(),
        backward_name: "sub_into".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: BTreeMap::new(),
        backward: BTreeMap::new(),
        symbolic: Some(spec),
    }
}

/// Forward transfer `(a, b) |-> (a, a + b mod 2^w)`. Operand 0 = `a` (preserved),
/// operand 1 = `b`; output leaf 0 = `a`, leaf 1 = the ripple-carry sum. The final
/// carry-out is dropped, giving mod-`2^w` (wrapping) semantics.
fn add_transfer(w: u8) -> SpecTransfer {
    let n = w as usize;
    let a = |i: usize| BvExpr::var(0, i);
    let b = |i: usize| BvExpr::var(1, i);
    let leaf_a: Vec<BvExpr> = (0..n).map(a).collect();
    let mut sum = Vec::with_capacity(n);
    let mut carry = BvExpr::constant(false);
    for i in 0..n {
        // s_i = a_i ^ b_i ^ carry_i
        sum.push(a(i).xor(b(i)).xor(carry.clone()));
        // carry_{i+1} = (a_i & b_i) | (carry_i & (a_i ^ b_i))
        carry = a(i).and(b(i)).or(carry.and(a(i).xor(b(i))));
    }
    SpecTransfer {
        leaves: vec![leaf_a, sum],
        width: w,
    }
}

/// Backward transfer `(a, s) |-> (a, s - a mod 2^w)`. Operand 0 = `a` (preserved),
/// operand 1 = `s`; output leaf 0 = `a`, leaf 1 = the ripple-borrow difference.
fn sub_transfer(w: u8) -> SpecTransfer {
    let n = w as usize;
    let a = |i: usize| BvExpr::var(0, i);
    let s = |i: usize| BvExpr::var(1, i);
    let leaf_a: Vec<BvExpr> = (0..n).map(a).collect();
    let mut diff = Vec::with_capacity(n);
    let mut borrow = BvExpr::constant(false);
    for i in 0..n {
        // d_i = s_i ^ a_i ^ borrow_i
        diff.push(s(i).xor(a(i)).xor(borrow.clone()));
        // borrow_{i+1} = (!s_i & a_i) | (borrow_i & !(s_i ^ a_i))
        borrow = s(i).not().and(a(i)).or(borrow.and(s(i).xor(a(i)).not()));
    }
    SpecTransfer {
        leaves: vec![leaf_a, diff],
        width: w,
    }
}

/// Count the number of leaves in a type (for Sum padding).
pub fn count_leaves(ty: &Type) -> usize {
    match ty {
        Type::Zero => 0,
        Type::One => 0,
        Type::Sum(a, b) => 1 + count_leaves(a).max(count_leaves(b)),
        Type::Prod(a, b) => count_leaves(a) + count_leaves(b),
        Type::Flat(_) => 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bool_ty() -> Type {
        Type::sum(Type::One, Type::One)
    }

    fn tt() -> Value {
        Value::left(Value::Unit)
    }

    fn ff() -> Value {
        Value::right(Value::Unit)
    }

    #[test]
    fn admit_self_inverse() {
        // `not : Bool → Bool`, self-inverse
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), ff());
        fwd.insert(ff(), tt());

        let def = PrimDef {
            forward_name: "not".into(),
            backward_name: "not".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd.clone(),
            backward: fwd,
            symbolic: None,
        };
        assert!(admit(&def).is_ok());
    }

    #[test]
    fn admit_identity() {
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), tt());
        fwd.insert(ff(), ff());

        let def = PrimDef {
            forward_name: "id".into(),
            backward_name: "id".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd.clone(),
            backward: fwd,
            symbolic: None,
        };
        assert!(admit(&def).is_ok());
    }

    #[test]
    fn reject_non_mutual_inverse() {
        // forward = identity, backward = not
        // backward(forward(tt)) = backward(tt) = ff ≠ tt
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), tt());
        fwd.insert(ff(), ff());

        let mut bwd = BTreeMap::new();
        bwd.insert(tt(), ff());
        bwd.insert(ff(), tt());

        let def = PrimDef {
            forward_name: "id".into(),
            backward_name: "not".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd,
            backward: bwd,
            symbolic: None,
        };
        assert!(admit(&def).is_err());
    }

    #[test]
    fn reject_non_total_forward() {
        // forward only defined on tt, not ff
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), tt());
        let mut bwd = BTreeMap::new();
        bwd.insert(tt(), tt());
        bwd.insert(ff(), ff());

        let def = PrimDef {
            forward_name: "partial".into(),
            backward_name: "partial".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd,
            backward: bwd,
            symbolic: None,
        };
        let err = admit(&def).unwrap_err();
        assert!(matches!(err, AdmitError::ForwardNotTotal(_)));
    }

    #[test]
    fn enumerate_zero() {
        assert!(enumerate_values(&Type::Zero).is_empty());
    }

    #[test]
    fn enumerate_one() {
        assert_eq!(enumerate_values(&Type::One), vec![Value::Unit]);
    }

    #[test]
    fn enumerate_bool() {
        let vals = enumerate_values(&bool_ty());
        assert_eq!(vals.len(), 2);
        assert_eq!(vals[0], tt());
        assert_eq!(vals[1], ff());
    }

    #[test]
    fn enumerate_product() {
        // 1 × Bool has 2 inhabitants
        let ty = Type::prod(Type::One, bool_ty());
        assert_eq!(enumerate_values(&ty).len(), 2);
    }

    #[test]
    fn enumerate_zero_product() {
        // 0 × 1 has 0 inhabitants
        let ty = Type::prod(Type::Zero, Type::One);
        assert!(enumerate_values(&ty).is_empty());
    }

    #[test]
    fn prim_def_adjoint_involution() {
        let mut fwd = BTreeMap::new();
        fwd.insert(tt(), ff());
        fwd.insert(ff(), tt());

        let def = PrimDef {
            forward_name: "not".into(),
            backward_name: "not_inv".into(),
            input_ty: bool_ty(),
            output_ty: bool_ty(),
            forward: fwd.clone(),
            backward: fwd,
            symbolic: None,
        };
        assert_eq!(def.adjoint().adjoint(), def);
    }
}
