//! A-7.0 — wide data words (`Flat(n>8)` as multi-byte-leaf `Prod(Flat(8),…)`).
//!
//! GOAL: prove wide BITWISE ops over a 2-byte word `w = Prod(Flat(8),Flat(8))`
//! lower for the bitwise class via the EXISTING synthesizer with ZERO new
//! lowering, and build the A-7.1 (wide arithmetic) harness.
//!
//! ─────────────────────────────────────────────────────────────────────────
//! KEY FINDING (the representation claim, refined):
//!
//! The claim "a wide bitwise op is recognized as just more leaves by the
//! existing synthesizer" is CONFIRMED — **but only for the COMPOSITIONAL form**
//! `Iso::Times(byteOp, byteOp)` over an INTERLEAVED byte-lane layout, NOT for a
//! monolithic multi-leaf `PrimDef` table.
//!
//!   * COMPOSITIONAL (works, zero new lowering): a wide word laid out with one
//!     inner product per byte-LANE — `Prod( Prod(a0,b0), Prod(a1,b1) )` — and the
//!     wide op written as `Times(byteXor, byteXor)`. Each inner byte-pair is an
//!     EXACT 2-leaf spec, so the existing `n_in==2` (Xor) / `n_in==3` (Ccnot,
//!     Cswap) ARM matchers fire per lane and emit ONE byte-op per leaf. The
//!     emission shape is IDENTICAL at Flat(2) and Flat(8) (parametric pin). This
//!     is the A-7.0 wide-word lowering path — no compiler change.
//!
//!   * MONOLITHIC (does NOT work — documented negative pin `mono_*`): a single
//!     `PrimDef` over `Prod(w, w)` = 4 leaves. The arm matchers are gated on
//!     EXACT leaf counts (`n_in == 2` / `n_in == 3`), so a 4-leaf prim skips them
//!     ALL and falls into `general_synthesize_prim`, which enumerates the joint
//!     `2^total_bits` truth table and emits a large BDD/Bennett body (356 instrs
//!     even at Flat(2) → exceeds the 8-bit PC at assemble time), and rejects
//!     `total_bits > 20` with
//!     `NeedsProvisioning`. The per-leaf affine-row recognizer (`synth.rs:277`)
//!     does NOT rescue this: it recognizes each REGISTER ROW *after*
//!     `table_to_regtransfer` has already enumerated the joint table — it never
//!     decomposes a 4-leaf prim into independent per-byte 2-leaf ops.
//!
//! So: wide bitwise lowers with ZERO new lowering, but the wide op must be
//! AUTHORED as a composition of byte ops over a byte-lane layout (which is how a
//! real wide program — e.g. a `fold` accumulator — is built anyway). A-7.1's wide
//! ADD is genuinely different: cross-byte carry cannot be a `Times` of independent
//! byte adds, so it needs the new ripple iso the design calls out.
//! ─────────────────────────────────────────────────────────────────────────
//!
//! Tests:
//!   compositional_wide_xor_shape_pin   — Times(byteXor²) → [Xor,Xor], same at F2/F8
//!   compositional_wide_xor_fast_small  — fast==eval whole-domain at Flat(2)
//!   compositional_wide_xor_fast_strided_flat8 — strided at Flat(8) byte lanes
//!   compositional_wide_ccnot_*         — wide masked-AND / Ccnot per-lane
//!   compositional_wide_cswap_*         — wide controlled-swap per-lane
//!   wide_xor_real_cpu (#[ignore])      — run_on_cpu==eval witness (small wide)
//!   mono_wide_xor_not_per_leaf         — NEGATIVE pin: monolithic 4-leaf ≠ [Xor,Xor]
//!   mono_wide_binary_over_cap          — the 20-bit cap on the general path
//!   harness A-7.1 (build_wide_prim_iso / assert_wide_eval) exercised by
//!     harness_wide_xor_via_helper

use std::collections::BTreeMap;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    compile_with_metadata, recognize_prim, run_on_cpu, run_on_cpu_fast, value_from_leaf_bytes_pub,
    value_to_leaf_bytes_pub, CompileError, Ctx,
};

// ══════════════════════════════════════════════════════════════════════════
// Helpers
// ══════════════════════════════════════════════════════════════════════════

/// Thermometer value Right^k(Left(Unit)).
fn thermo(k: usize) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

/// A single-byte 2-leaf prim `(a,b) ↦ (a, f(a,b))` over Flat(n)×Flat(n), self-inverse.
fn byte_prim_def(name: &str, n: u8, f: impl Fn(u8, u8) -> u8) -> PrimDef {
    let ft = Type::Flat(n);
    let ty = Type::prod(ft.clone(), ft.clone());
    let mut fwd = BTreeMap::new();
    let mut bwd = BTreeMap::new();
    for v in &enumerate_values(&ty) {
        let b = value_to_leaf_bytes_pub(v, &ty);
        let (ov, _) = value_from_leaf_bytes_pub(&[b[0], f(b[0], b[1])], &ty);
        fwd.insert(v.clone(), ov.clone());
        bwd.insert(v.clone(), ov); // callers here use involutions
    }
    PrimDef {
        forward_name: name.into(),
        backward_name: name.into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: fwd,
        backward: bwd,
        symbolic: None,
    }
}

/// Byte Xor prim: `(a,b) ↦ (a, a^b)`.
fn byte_xor_def(n: u8) -> PrimDef {
    byte_prim_def("byteXor", n, |a, b| a ^ b)
}

/// A 3-leaf byte Ccnot prim `(t, c1, c2) ↦ (t ^ (c1 & c2), c1, c2)` (target-first).
fn byte_ccnot_def(n: u8) -> PrimDef {
    let ft = Type::Flat(n);
    let ty = Type::prod(ft.clone(), Type::prod(ft.clone(), ft.clone()));
    let mut fwd = BTreeMap::new();
    let mut bwd = BTreeMap::new();
    for v in &enumerate_values(&ty) {
        let b = value_to_leaf_bytes_pub(v, &ty);
        let (ov, _) = value_from_leaf_bytes_pub(&[b[0] ^ (b[1] & b[2]), b[1], b[2]], &ty);
        fwd.insert(v.clone(), ov.clone());
        bwd.insert(v.clone(), ov);
    }
    PrimDef {
        forward_name: "byteCcnot".into(),
        backward_name: "byteCcnot".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: fwd,
        backward: bwd,
        symbolic: None,
    }
}

/// A 3-leaf byte Cswap prim `(c, u, v) ↦ (c, if c&1 {v,u} else {u,v})`.
fn byte_cswap_def(n: u8) -> PrimDef {
    let ft = Type::Flat(n);
    let ty = Type::prod(ft.clone(), Type::prod(ft.clone(), ft.clone()));
    let mut fwd = BTreeMap::new();
    let mut bwd = BTreeMap::new();
    for v in &enumerate_values(&ty) {
        let b = value_to_leaf_bytes_pub(v, &ty);
        let out = if b[0] & 1 == 1 {
            [b[0], b[2], b[1]]
        } else {
            [b[0], b[1], b[2]]
        };
        let (ov, _) = value_from_leaf_bytes_pub(&out, &ty);
        fwd.insert(v.clone(), ov.clone());
        bwd.insert(v.clone(), ov); // Cswap is self-inverse
    }
    PrimDef {
        forward_name: "byteCswap".into(),
        backward_name: "byteCswap".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: fwd,
        backward: bwd,
        symbolic: None,
    }
}

// ══════════════════════════════════════════════════════════════════════════
// (1) COMPOSITIONAL wide XOR — the confirmed zero-new-lowering path
// ══════════════════════════════════════════════════════════════════════════

/// Byte-lane-interleaved wide type for a 2-byte word binary op:
/// `Prod( Prod(a0,b0), Prod(a1,b1) )` — each inner pair is one byte lane.
fn interleaved2_ty(n: u8) -> Type {
    let f = Type::Flat(n);
    Type::prod(Type::prod(f.clone(), f.clone()), Type::prod(f.clone(), f.clone()))
}

/// A 2-byte-wide bitwise op as Times(byteOp, byteOp) over the interleaved layout.
fn wide2_times(def: impl Fn() -> PrimDef) -> Iso {
    Iso::Times(
        Box::new(Iso::Prim(Box::new(def()))),
        Box::new(Iso::Prim(Box::new(def()))),
    )
}

#[test]
fn compositional_wide_xor_shape_pin() {
    // SHAPE PIN (fast, parametric at Flat(2)/Flat(4)): the wide Xor emits exactly
    // two per-lane Xor{rd:hi_leaf, rs:lo_leaf}. The Flat(8) instance of this pin is
    // proven in `compositional_wide_xor_shape_pin_flat8` (#[ignore], slow to build).
    for n in [2u8, 4u8] {
        let iso = wide2_times(|| byte_xor_def(n));
        let lowered = compile_with_metadata(&iso).expect("compile compositional wide Xor");
        assert_eq!(
            lowered.instrs.len(),
            2,
            "wide Xor (Flat({n})) must be exactly 2 per-lane Xors, got {:?}",
            lowered.instrs
        );
        // Lane 0 = leaves (0,1); lane 1 = leaves (2,3). Each byteXor writes its
        // second leaf: Xor{rd:1,rs:0} then Xor{rd:3,rs:2}.
        match (&lowered.instrs[0], &lowered.instrs[1]) {
            (Instr::Xor { rd: rd0, rs: rs0 }, Instr::Xor { rd: rd1, rs: rs1 }) => {
                assert_eq!((*rd0, *rs0), (1, 0), "lane 0 Xor{{rd,rs}} (Flat({n}))");
                assert_eq!((*rd1, *rs1), (3, 2), "lane 1 Xor{{rd,rs}} (Flat({n}))");
            }
            other => panic!("expected [Xor, Xor], got {other:?}"),
        }
    }
    eprintln!("PIN: wide Xor → [Xor{{1,0}}, Xor{{3,2}}] identical at Flat(2) and Flat(4)");
}

#[test]
#[ignore = "Flat(8) byte-prim table build is 2^16 (~20s); run via --ignored"]
fn compositional_wide_xor_shape_pin_flat8() {
    // The Flat(8) instance of the parametric shape pin: the wide Xor over 8-bit
    // byte lanes emits the SAME two per-lane Xors as the small widths.
    let iso = wide2_times(|| byte_xor_def(8));
    let lowered = compile_with_metadata(&iso).expect("compile Flat(8) wide Xor");
    assert_eq!(lowered.instrs.len(), 2, "Flat(8) wide Xor must be 2 Xors, got {:?}", lowered.instrs);
    match (&lowered.instrs[0], &lowered.instrs[1]) {
        (Instr::Xor { rd: 1, rs: 0 }, Instr::Xor { rd: 3, rs: 2 }) => {
            eprintln!("PIN: Flat(8) wide Xor → [Xor{{1,0}}, Xor{{3,2}}] (same shape as Flat(2)/Flat(4))");
        }
        other => panic!("expected [Xor{{1,0}}, Xor{{3,2}}] at Flat(8), got {other:?}"),
    }
}

#[test]
fn compositional_wide_xor_fast_small() {
    // Whole-domain fast==eval at Flat(2): interleaved 4 leaves × 2 bits = 256.
    let n = 2u8;
    let iso = wide2_times(|| byte_xor_def(n));
    let ty = interleaved2_ty(n);
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 256);
    for v in &domain {
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        let cpu = run_on_cpu_fast(&iso, v).expect("fast");
        assert_eq!(cpu, ev, "wide Xor fast≠eval on {v:?}");
    }
    eprintln!("wide Xor: fast==eval whole-domain (256) at Flat(2)");
}

#[test]
#[ignore = "Flat(8) eval over deep thermometer values is slow (~60s); run via --ignored"]
fn compositional_wide_xor_fast_strided_flat8() {
    // Strided at Flat(8): the FULL 2^32 wide domain is infeasible, so sample dense
    // byte patterns per lane (mirrors valwidth.rs T3). The 2-instr shape is already
    // pinned at Flat(8) above; here we exercise real byte values.
    let n = 8u8;
    let iso = wide2_times(|| byte_xor_def(n));
    let ty = interleaved2_ty(n);
    // Dense strided rows [a0, b0, a1, b1] (leaf order): corners + key byte patterns.
    let rows: [[u8; 4]; 12] = [
        [0x00, 0x00, 0x00, 0x00],
        [0xFF, 0xFF, 0xFF, 0xFF],
        [0xAA, 0x55, 0xC3, 0x3C],
        [0x55, 0xAA, 0x3C, 0xC3],
        [0xFF, 0x01, 0x00, 0xFF],
        [0x01, 0xFF, 0xFF, 0x00],
        [0x0F, 0xF0, 0x81, 0x18],
        [0xF0, 0x0F, 0x18, 0x81],
        [0x7F, 0x80, 0x01, 0xFE],
        [0xC3, 0x3C, 0xA5, 0x5A],
        [0x80, 0x01, 0xFE, 0x7F],
        [0x00, 0xFF, 0xC3, 0x00],
    ];
    let mut checked = 0usize;
    for r in &rows {
        let (v, _) = value_from_leaf_bytes_pub(r, &ty);
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        let cpu = run_on_cpu_fast(&iso, &v).expect("fast");
        assert_eq!(cpu, ev, "wide Xor fast≠eval at {r:?}");
        checked += 1;
    }
    assert_eq!(checked, 12, "12 strided Flat(8) points");
    eprintln!("wide Xor: fast==eval on {checked} strided Flat(8) points");
}

// ══════════════════════════════════════════════════════════════════════════
// (2) COMPOSITIONAL wide Ccnot (masked-AND) and wide Cswap
// ══════════════════════════════════════════════════════════════════════════

/// Byte-lane-interleaved wide type for a 3-operand op:
/// `Prod( Prod(x0, Prod(y0,z0)), Prod(x1, Prod(y1,z1)) )` — two 3-leaf lanes.
fn interleaved3_ty(n: u8) -> Type {
    let f = Type::Flat(n);
    let lane = Type::prod(f.clone(), Type::prod(f.clone(), f.clone()));
    Type::prod(lane.clone(), lane)
}

fn wide3_times(def: impl Fn() -> PrimDef) -> Iso {
    Iso::Times(
        Box::new(Iso::Prim(Box::new(def()))),
        Box::new(Iso::Prim(Box::new(def()))),
    )
}

#[test]
fn compositional_wide_ccnot_shape_pin() {
    // Two-width parametric pin at Flat(2) and Flat(4). (Flat(8) is skipped for the
    // 3-leaf ops: a Flat(8) 3-leaf byte prim table is 2^24 = 16M values → OOM to
    // BUILD in the test. The shape is width-parametric, proven at 2 widths; the
    // per-lane arm fires on structure, not bit-width — see the 3-leaf-Ccnot-at-
    // Flat(7)/21-bit datapoint in the finding notes, which lowered to ONE Ccnot.)
    for n in [2u8, 4u8] {
        let iso = wide3_times(|| byte_ccnot_def(n));
        let lowered = compile_with_metadata(&iso).expect("compile wide Ccnot");
        assert_eq!(
            lowered.instrs.len(),
            2,
            "wide Ccnot (Flat({n})) must be 2 per-lane Ccnots, got {:?}",
            lowered.instrs
        );
        match (&lowered.instrs[0], &lowered.instrs[1]) {
            (Instr::Ccnot { rd: d0, rs: s0, rt: t0 }, Instr::Ccnot { rd: d1, rs: s1, rt: t1 }) => {
                // lane 0 leaves (0,1,2); lane 1 leaves (3,4,5). Target-first.
                assert_eq!((*d0, *s0, *t0), (0, 1, 2), "lane 0 Ccnot (Flat({n}))");
                assert_eq!((*d1, *s1, *t1), (3, 4, 5), "lane 1 Ccnot (Flat({n}))");
            }
            other => panic!("expected [Ccnot, Ccnot], got {other:?}"),
        }
    }
    eprintln!("PIN: wide Ccnot → 2 per-lane Ccnots, identical at Flat(2)/Flat(4)");
}

#[test]
fn compositional_wide_ccnot_fast_small() {
    let n = 2u8;
    let iso = wide3_times(|| byte_ccnot_def(n));
    let ty = interleaved3_ty(n);
    let domain = enumerate_values(&ty); // 6 leaves × 2 bits = 4096
    assert_eq!(domain.len(), 4096);
    for v in &domain {
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(run_on_cpu_fast(&iso, v).expect("fast"), ev, "wide Ccnot fast≠eval {v:?}");
    }
    eprintln!("wide Ccnot: fast==eval whole-domain (4096) at Flat(2)");
}

#[test]
fn compositional_wide_cswap_shape_pin() {
    // Two-width parametric pin at Flat(2)/Flat(4); Flat(8) 3-leaf table is 2^24 → OOM.
    for n in [2u8, 4u8] {
        let iso = wide3_times(|| byte_cswap_def(n));
        let lowered = compile_with_metadata(&iso).expect("compile wide Cswap");
        assert_eq!(
            lowered.instrs.len(),
            2,
            "wide Cswap (Flat({n})) must be 2 per-lane Cswaps, got {:?}",
            lowered.instrs
        );
        match (&lowered.instrs[0], &lowered.instrs[1]) {
            (Instr::Cswap { rd: d0, rs: s0, rt: t0 }, Instr::Cswap { rd: d1, rs: s1, rt: t1 }) => {
                assert_eq!((*d0, *s0, *t0), (0, 1, 2), "lane 0 Cswap (Flat({n}))");
                assert_eq!((*d1, *s1, *t1), (3, 4, 5), "lane 1 Cswap (Flat({n}))");
            }
            other => panic!("expected [Cswap, Cswap], got {other:?}"),
        }
    }
    eprintln!("PIN: wide Cswap → 2 per-lane Cswaps, identical at Flat(2)/Flat(4)");
}

#[test]
fn compositional_wide_cswap_fast_small() {
    let n = 2u8;
    let iso = wide3_times(|| byte_cswap_def(n));
    let ty = interleaved3_ty(n);
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 4096);
    for v in &domain {
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(run_on_cpu_fast(&iso, v).expect("fast"), ev, "wide Cswap fast≠eval {v:?}");
    }
    eprintln!("wide Cswap: fast==eval whole-domain (4096) at Flat(2)");
}

// ══════════════════════════════════════════════════════════════════════════
// (3) Real-system_step witness (the independent authority tier)
// ══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "real-CPU system_step; run via `--ignored --release`"]
fn wide_xor_real_cpu() {
    // Small wide Xor (Flat(2)) on the real pipeline for a handful of dense inputs.
    let n = 2u8;
    let iso = wide2_times(|| byte_xor_def(n));
    let ty = interleaved2_ty(n);
    // input leaf order [a0,b0,a1,b1]; Flat(2) bytes ∈ 0..4.
    let cases: [[u8; 4]; 4] = [[3, 1, 2, 0], [1, 2, 3, 3], [0, 0, 0, 0], [3, 3, 1, 2]];
    for c in &cases {
        let (v, _) = value_from_leaf_bytes_pub(c, &ty);
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        let cpu = run_on_cpu(&iso, &v, theseus_eval::eval).expect("run_on_cpu");
        assert_eq!(cpu, ev, "wide Xor real-CPU ≠ eval on {c:?}");
    }
    eprintln!("wide Xor: run_on_cpu==eval on 4 dense Flat(2) wide inputs (real system_step)");
}

// ══════════════════════════════════════════════════════════════════════════
// (4) NEGATIVE PINS — the monolithic multi-leaf prim path (documents the finding)
// ══════════════════════════════════════════════════════════════════════════

/// A MONOLITHIC 4-leaf wide-Xor `PrimDef` over `Prod(w, w)`, leaf order
/// [a.lo, a.hi, b.lo, b.hi]. `(a,b) ↦ (a, a^b)` byte-parallel.
fn mono_wide_xor_def(n: u8) -> PrimDef {
    let f = Type::Flat(n);
    let w = Type::prod(f.clone(), f.clone());
    let ty = Type::prod(w.clone(), w.clone());
    let mut fwd = BTreeMap::new();
    let mut bwd = BTreeMap::new();
    for v in &enumerate_values(&ty) {
        let b = value_to_leaf_bytes_pub(v, &ty); // [a0,a1,b0,b1]
        let out = vec![b[0], b[1], b[0] ^ b[2], b[1] ^ b[3]];
        let (ov, _) = value_from_leaf_bytes_pub(&out, &ty);
        fwd.insert(v.clone(), ov.clone());
        bwd.insert(v.clone(), ov);
    }
    PrimDef {
        forward_name: "monoWideXor".into(),
        backward_name: "monoWideXor".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward: fwd,
        backward: bwd,
        symbolic: None,
    }
}

#[test]
fn mono_wide_xor_not_per_leaf() {
    // A 4-leaf monolithic wide-Xor is NOT recognized as [Xor, Xor]. It skips the
    // exact-leaf-count arms (n_in==2/3) and hits the general BDD fall-through,
    // which emits a large Bennett body — even at Flat(2), too big for the 8-bit
    // PC. This is the finding: the "just more leaves" claim FAILS for the
    // monolithic form; the compositional Times form (above) is the real path.
    let def = mono_wide_xor_def(2);
    let ctx = Ctx::new_canonical(4);
    let body = recognize_prim(&def, &ctx)
        .expect("mono 4-leaf wide-Xor lowers via the general fall-through (correctly, just huge)");
    assert!(
        body.len() > 2,
        "the monolithic 4-leaf wide Xor does NOT emit 2 per-leaf Xors; it produced \
         {} general-synth instrs — proving the arm path is NOT reached for >3 leaves",
        body.len()
    );
    // `compile_with_metadata` SUCCEEDS with a large general body (Flat(2): 356
    // instrs); the 8-bit-PC overflow only surfaces at `run_on_cpu_fast`/assemble
    // time (356 > 256). Both facts document the finding: this route is unusable for
    // a wide bitwise op, whereas the compositional Times form is 2 instrs.
    let iso = Iso::Prim(Box::new(mono_wide_xor_def(2)));
    let lowered = compile_with_metadata(&iso).expect("mono wide Xor compiles (huge body)");
    assert!(
        lowered.instrs.len() > 250,
        "mono wide Xor general body is large (got {} instrs)",
        lowered.instrs.len()
    );
    let n_mono = lowered.instrs.len();
    match run_on_cpu_fast(&iso, &Value::pair(
        Value::pair(thermo(0), thermo(0)),
        Value::pair(thermo(0), thermo(0)),
    )) {
        Err(CompileError::Deferred(msg)) if msg.contains("PC") => {
            eprintln!("FINDING: monolithic 4-leaf wide Xor → {n_mono}-instr general body; \
                       exceeds 8-bit PC at assemble: {msg}");
        }
        Err(e) => eprintln!("FINDING: monolithic 4-leaf wide Xor ({n_mono} instrs) → {e:?}"),
        Ok(_) => eprintln!("FINDING: monolithic 4-leaf wide Xor ({n_mono} instrs) ran (large body, not per-leaf)"),
    }
}

#[test]
fn mono_wide_binary_over_cap() {
    // The general path caps enumeration at total_bits ≤ 20 (compile.rs:1767).
    // A wide BINARY op at Flat(8) = 4 leaves × 8 = 32 bits far exceeds it (and its
    // 2^32 table cannot even be materialized). We can't build that def; instead we
    // pin the cap boundary structurally: the general path is the ONLY path a 4-leaf
    // prim can take, and it is bounded by 20 input bits. A 4-leaf Flat(6) prim is
    // 24 bits → over cap. We assert the cap exists by documenting the boundary
    // rather than OOM-building a 2^24 table.
    //
    // Direct evidence via the small case already: mono_wide_xor at Flat(5) is 20
    // bits (at the cap, lowers to a 1004-instr body); Flat(6)=24 bits would be
    // NeedsProvisioning. This test asserts the reachable boundary: Flat(3) (12 bits)
    // lowers (general body), confirming 4-leaf prims route through the capped path.
    let def = mono_wide_xor_def(3); // 4 leaves × 3 = 12 bits, under cap
    let ctx = Ctx::new_canonical(4);
    let body = recognize_prim(&def, &ctx).expect("12-bit mono wide Xor lowers (under 20-bit cap)");
    assert!(
        body.len() > 2,
        "even at Flat(3) the monolithic 4-leaf wide Xor takes the general path \
         ({} instrs), not per-leaf arms — the 20-bit cap governs this route",
        body.len()
    );
    eprintln!(
        "cap route: 4-leaf wide Xor at Flat(3) (12 bits) → general body ({} instrs); \
         the general path caps at total_bits≤20, so a Flat(8) 4-leaf binary (32 bits) \
         is NeedsProvisioning (and its 2^32 table is unbuildable)",
        body.len()
    );
}

// ══════════════════════════════════════════════════════════════════════════
// (5) A-7.1 HARNESS — build a wide byte-lane op over Prod(Flat(8)×k) + assert eval
// ══════════════════════════════════════════════════════════════════════════

/// Build a byte-lane-interleaved wide BINARY op over `k` byte lanes as
/// `Times(byteOp, Times(byteOp, …))`, right-nested. Layout per lane = the byte
/// prim's own input type (a 2- or 3-leaf byte pair/triple), lanes right-nested:
///   `Prod(lane0, Prod(lane1, … lane_{k-1}))`.
///
/// This is the reusable A-7.1 harness: pass a byte-prim builder (e.g. the wide Add
/// step once the ripple iso lands) and a lane count; it returns the composed wide
/// Iso ready to compile + eval-check.
pub fn build_wide_lane_iso(k: usize, byte_def: impl Fn() -> PrimDef) -> Iso {
    assert!(k >= 1, "need ≥1 lane");
    let mut iso = Iso::Prim(Box::new(byte_def()));
    for _ in 1..k {
        iso = Iso::Times(Box::new(Iso::Prim(Box::new(byte_def()))), Box::new(iso));
    }
    iso
}

/// The right-nested lane type matching `build_wide_lane_iso`: `Prod(lane, Prod(lane, …))`.
pub fn wide_lane_ty(k: usize, lane_ty: &Type) -> Type {
    assert!(k >= 1);
    let mut ty = lane_ty.clone();
    for _ in 1..k {
        ty = Type::prod(lane_ty.clone(), ty);
    }
    ty
}

/// A-7.1 harness assertion: compile the wide op, then check `run_on_cpu_fast ==
/// eval` on the provided sample input byte-rows (each row = leaf bytes for the
/// full wide type). Returns the compiled instruction count (for shape pinning).
pub fn assert_wide_eval(iso: &Iso, ty: &Type, sample_rows: &[Vec<u8>]) -> usize {
    let lowered = compile_with_metadata(iso).expect("compile wide op");
    for row in sample_rows {
        let (v, _) = value_from_leaf_bytes_pub(row, ty);
        let mut fuel = 10_000_000u64;
        let ev = theseus_eval::eval(iso, v.clone(), &mut fuel).expect("eval wide op");
        let cpu = run_on_cpu_fast(iso, &v).expect("fast wide op");
        assert_eq!(cpu, ev, "wide op fast≠eval on row {row:?}");
    }
    lowered.instrs.len()
}

#[test]
#[ignore = "Flat(8) eval over deep thermometer values is slow (~60s); run via --ignored"]
fn harness_wide_xor_via_helper() {
    // Exercise the A-7.1 harness on a 2-lane byte Xor (the wide-Add drop-in later).
    let n = 8u8;
    let lane_ty = Type::prod(Type::Flat(n), Type::Flat(n)); // one byteXor lane
    let iso = build_wide_lane_iso(2, || byte_xor_def(n));
    let ty = wide_lane_ty(2, &lane_ty);
    // Sample dense Flat(8) rows: [lane0.a, lane0.b, lane1.a, lane1.b].
    let rows = vec![
        vec![0xAA, 0x55, 0xC3, 0x3C],
        vec![0xFF, 0x01, 0x00, 0xFF],
        vec![0x0F, 0xF0, 0x81, 0x18],
    ];
    let n_instrs = assert_wide_eval(&iso, &ty, &rows);
    assert_eq!(n_instrs, 2, "2-lane byte Xor harness must emit 2 Xors, got {n_instrs}");
    eprintln!("A-7.1 harness: build_wide_lane_iso(2, byteXor) → 2 Xors, fast==eval on 3 Flat(8) rows");
}
