#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! Value-width slice-4 (final rung): byte-parallel masked accumulate
//! `acc ^= key & arr[i]` at Flat(4) via `fold` over a Flat(4) list spine.
//!
//! T-instr: system-ring dense-byte Ccnot (byte-parallelism proof, all 8 bits).
//! T-prog: Flat(4) masked fold, whole-domain (69888 inputs), HOISTED.
//! T-prog-disc: discriminating case — elems=[2], key=2, proves 4-bit parallel AND.
//! T-real: real CPU (#[ignore]).
//! T7: SystemRing ancillas_restored + bus_quiescent (closure).

use std::collections::BTreeMap;
use std::sync::OnceLock;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type, Value};

use theseus_tisc::asm::{assemble, encode, Instr};
use theseus_tisc::compile::{
    compile_with_metadata, marshal, run_on_cpu_resident, run_rom_fast, unmarshal,
    value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};
use theseus_tisc::ring_run::{system_ring, SystemRing};
use theseus_tisc::types::{StateVals, MEM_WORDS};

// ── Type helpers ────────────────────────────────────────────────────

/// Thermometer encoding: Right^k(Left(Unit))
fn thermo(k: usize) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

/// Build a list-spine value from a slice of element bytes.
/// list_spine_ty(Flat(w), D) — elements are innermost-first:
///   nil = Left(Unit)
///   cons(h, t) = Right(Pair(thermo(h), t))
/// Includes round-trip assert via value_to/from_leaf_bytes.
fn list_spine_val(elems: &[u8], w: u8, d: usize) -> Value {
    assert!(
        elems.len() < d,
        "list_spine_val: elems.len()={} >= D={d}",
        elems.len()
    );
    let mut val = Value::left(Value::Unit); // nil
    for &e in elems.iter().rev() {
        val = Value::right(Value::pair(thermo(e as usize), val));
    }
    // Round-trip assert.
    let ty = Type::list_spine_ty(Type::Flat(w), d);
    let bytes = value_to_leaf_bytes_pub(&val, &ty);
    let (rt_val, consumed) = value_from_leaf_bytes_pub(&bytes, &ty);
    assert_eq!(
        consumed,
        bytes.len(),
        "list_spine_val round-trip: consumed mismatch"
    );
    assert_eq!(rt_val, val, "list_spine_val round-trip: value mismatch");
    val
}

// ── Masked-step prim: (elem, (key, acc)) ↦ (elem, (key, acc ^ (key & elem))) ──

/// Build the maskedStep PrimDef over Flat(w)³ = (Flat(w), (Flat(w), Flat(w))).
/// Forward: (elem, (key, acc)) ↦ (elem, (key, acc ^ (key & elem)))
/// Self-inverse (backward == forward).
fn masked_step_prim_def(w: u8) -> PrimDef {
    let flat = Type::Flat(w);
    // Carrier: elem × (key × acc)
    let ty = Type::prod(flat.clone(), Type::prod(flat.clone(), flat.clone()));
    let vals = enumerate_values(&ty);

    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();

    for v in &vals {
        let bytes = value_to_leaf_bytes_pub(v, &ty);
        let elem = bytes[0];
        let key = bytes[1];
        let acc = bytes[2];
        let new_acc = acc ^ (key & elem);
        let out_bytes = vec![elem, key, new_acc];
        let (out_val, _) = value_from_leaf_bytes_pub(&out_bytes, &ty);
        forward.insert(v.clone(), out_val.clone());
        // Self-inverse: backward is identical.
        backward.insert(v.clone(), out_val);
    }

    PrimDef {
        forward_name: "maskedStep".into(),
        backward_name: "maskedStep".into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

/// Build maskedFold iso at depth D: fold(list_spine_ty(Flat(4), D), maskedStep).
fn masked_fold_iso(d: usize) -> Iso {
    let def = masked_step_prim_def(4);
    Iso::fold(
        Type::list_spine_ty(Type::Flat(4), d),
        Iso::Prim(Box::new(def)),
    )
}

/// Full carrier type for maskedFold: list_spine_ty(Flat(4), D) × (Flat(4) × Flat(4)).
fn masked_fold_ty(d: usize) -> Type {
    Type::prod(
        Type::list_spine_ty(Type::Flat(4), d),
        Type::prod(Type::Flat(4), Type::Flat(4)),
    )
}

/// Build a full maskedFold input: Pair(list_spine_val, Pair(thermo(key), thermo(acc))).
fn masked_fold_input(elems: &[u8], key: u8, acc: u8, d: usize) -> Value {
    let list_val = list_spine_val(elems, 4, d);
    Value::pair(
        list_val,
        Value::pair(thermo(key as usize), thermo(acc as usize)),
    )
}

/// Extract the accumulator byte from a maskedFold output value.
/// The acc is the LAST leaf byte.
fn extract_acc(v: &Value, d: usize) -> u8 {
    let ty = masked_fold_ty(d);
    let bytes = value_to_leaf_bytes_pub(v, &ty);
    *bytes.last().expect("empty bytes")
}

/// Independent reference: acc_out = acc0 ⊕ ⊕ᵢ(key ∧ elemᵢ).
fn reference_masked(elems: &[u8], key: u8, acc0: u8) -> u8 {
    let mut acc = acc0;
    for &e in elems {
        acc ^= key & e;
    }
    acc
}

/// Extract list element bytes from a maskedFold input's leaf bytes.
/// Leaf-byte layout for list_spine_ty(Flat(w), D) × (Flat(w) × Flat(w)):
///   [tag, elem, tag, elem, ..., 0, key, acc]
/// Tags: 1=cons (Right), 0=nil (Left). Elements follow each 1-tag.
fn extract_list_elems_from_bytes(bytes: &[u8]) -> Vec<u8> {
    let mut elems = Vec::new();
    let mut i = 0;
    // The last two bytes are key and acc.
    let list_bytes = &bytes[..bytes.len() - 2];
    while i < list_bytes.len() {
        if list_bytes[i] == 1 {
            // cons: next byte is the element
            elems.push(list_bytes[i + 1]);
            i += 2;
        } else {
            // nil: done
            break;
        }
    }
    elems
}

// ── T-instr: system-ring dense-byte Ccnot (byte-parallelism proof) ──

/// Build a single-Ccnot ROM for system-ring testing.
fn ccnot_rom(rd: u8, rs: u8, rt: u8) -> Vec<u32> {
    vec![encode(Instr::Ccnot { rd, rs, rt }).unwrap()]
}

/// Cached system ring for the Ccnot ROM (rd=0, rs=1, rt=2).
fn ccnot_ring() -> &'static SystemRing {
    static RING: OnceLock<SystemRing> = OnceLock::new();
    RING.get_or_init(|| system_ring(&ccnot_rom(0, 1, 2)))
}

#[test]
fn t_instr_dense_byte_system_ring() {
    let ring = ccnot_ring();

    // Dense-byte test cases: (rs_val, rt_val, rd_init) → rd_out = rd_init ^ (rs_val & rt_val)
    let cases: Vec<(u8, u8, u8, &str)> = vec![
        (0xAA, 0x55, 0x00, "rs=0xAA, rt=0x55 → rd^=0x00"),
        (0xFF, 0x55, 0x00, "rs=0xFF, rt=0x55 → rd^=0x55"),
        (0xC3, 0xA5, 0x00, "rs=0xC3, rt=0xA5 → rd^=0x81"),
    ];

    for (rs_val, rt_val, rd_init, label) in &cases {
        let state = StateVals {
            regs: [*rd_init, *rs_val, *rt_val, 0x33, 0x44, 0x55, 0x66, 0x77],
            pc: 0,
            br: 0,
            ir: 0,
            scratch: [0; 8],
            mem: [0; MEM_WORDS],
        };

        let expected_rd = rd_init ^ (rs_val & rt_val);
        let out = ring.run(&state, 1);

        // Assert ancillas restored (ring closes).
        assert!(
            ring.ancillas_restored(&state),
            "ancillas NOT restored for {label}"
        );

        // Assert bus quiescent.
        assert!(ring.bus_quiescent(&state), "bus NOT quiescent for {label}");

        // Assert rd got the right value.
        assert_eq!(
            out.regs[0], expected_rd,
            "{label}: rd expected 0x{expected_rd:02X}, got 0x{:02X}",
            out.regs[0]
        );

        // Assert rs, rt unchanged.
        assert_eq!(out.regs[1], *rs_val, "{label}: rs changed");
        assert_eq!(out.regs[2], *rt_val, "{label}: rt changed");
    }
}

// ── T-prog: Flat(4) masked fold, whole-domain, HOISTED ──────────────

/// Hoisted compilation state for the masked fold at D=3.
struct HoistedMaskedFold {
    rom: Vec<u32>,
    budget: usize,
    instrs: Vec<Instr>,
}

fn hoisted_d3() -> &'static HoistedMaskedFold {
    static H: OnceLock<HoistedMaskedFold> = OnceLock::new();
    H.get_or_init(|| {
        let iso = masked_fold_iso(3);
        let lowered = compile_with_metadata(&iso).expect("compile maskedFold D=3");
        let rom = assemble(&lowered.instrs).expect("assemble");
        let budget = lowered.wcet();
        HoistedMaskedFold {
            rom,
            budget,
            instrs: lowered.instrs.clone(),
        }
    })
}

#[test]
fn t_prog_whole_domain() {
    let d = 3usize;
    let h = hoisted_d3();

    // Pin: D=3 compiles to 26 instrs with exactly 2 Ccnots.
    assert_eq!(
        h.instrs.len(),
        26,
        "D=3 maskedFold must compile to 26 instrs, got {}",
        h.instrs.len()
    );
    let ccnot_count = h
        .instrs
        .iter()
        .filter(|i| matches!(i, Instr::Ccnot { .. }))
        .count();
    assert_eq!(
        ccnot_count, 2,
        "D=3 maskedFold must contain exactly 2 Ccnots, got {ccnot_count}"
    );

    // Fresh eval iso.
    let eval_def = masked_step_prim_def(4);
    let eval_iso = Iso::fold(
        Type::list_spine_ty(Type::Flat(4), d),
        Iso::Prim(Box::new(eval_def)),
    );

    let ty = masked_fold_ty(d);
    let domain = enumerate_values(&ty);
    // Flat(4) masked fold D=3: lists ≤2 elements × key(16) × acc(16).
    // nil(1) + cons-1(16·1) + cons-2(16·16·1) = 1 + 16 + 256 = 273 list shapes
    // × 16 (key) × 16 (acc) = 273 × 256 = 69888
    assert_eq!(domain.len(), 69_888, "Flat(4) masked fold D=3 domain size");

    for v in &domain {
        let state = marshal(v, &ty);
        let final_state = run_rom_fast(&h.rom, state, h.budget);
        let rom_out = unmarshal(&final_state, &ty);

        let mut fuel = 100_000_000u64;
        let eval_out =
            theseus_eval::eval(&eval_iso, v.clone(), &mut fuel).expect("eval maskedFold D=3");

        assert_eq!(rom_out, eval_out, "maskedFold D=3: rom vs eval mismatch");

        // Independent reference: acc_out = acc0 ⊕ ⊕ᵢ(key ∧ elemᵢ).
        let in_bytes = value_to_leaf_bytes_pub(v, &ty);
        let n_bytes = in_bytes.len();
        let acc0 = in_bytes[n_bytes - 1];
        let key = in_bytes[n_bytes - 2];
        let elems = extract_list_elems_from_bytes(&in_bytes);
        let expected_acc = reference_masked(&elems, key, acc0);

        let out_acc = extract_acc(&rom_out, d);
        assert_eq!(
            out_acc, expected_acc,
            "maskedFold D=3: independent ref mismatch, elems={elems:?}, key={key}, acc0={acc0}"
        );
    }
}

// ── T-prog-disc: discriminating case ────────────────────────────────

#[test]
fn t_prog_disc_discriminating() {
    let d = 3usize;
    let h = hoisted_d3();
    let ty = masked_fold_ty(d);

    // elems=[2], key=2 at Flat(4).
    // Byte-parallel: acc ^= 2 & 2 = acc ^= 2.
    // A bit-0-only decoder would give acc ^= (2&1) & (2&1) = acc ^= 0.
    for acc0 in 0u8..16 {
        let input = masked_fold_input(&[2], 2, acc0, d);
        let state = marshal(&input, &ty);
        let final_state = run_rom_fast(&h.rom, state, h.budget);
        let rom_out = unmarshal(&final_state, &ty);
        let out_acc = extract_acc(&rom_out, d);

        let expected = acc0 ^ 2; // byte-parallel result
        assert_eq!(
            out_acc,
            expected,
            "discriminating case: acc0={acc0}, expected acc^=2={expected}, got {out_acc} \
             (a bit-0 decoder would give {}, proving byte-parallel AND)",
            acc0 // bit-0 would leave acc0 unchanged since 2&1=0
        );
    }
}

// ── T-real: real CPU (#[ignore]) ────────────────────────────────────

#[test]
#[ignore = "real-CPU system_step; run via `cargo test -p theseus-tisc --test valwidth_masked -- --ignored --release`"]
fn t_real_cpu() {
    let d = 3usize;
    let iso = masked_fold_iso(d);

    // Dense-byte masked step: single Ccnot instruction on system_step.
    // Also include Flat(4) masked-fold samples including the discriminating case.
    let samples: Vec<(Vec<u8>, u8, u8)> = vec![
        (vec![], 5, 3),       // empty list
        (vec![7], 3, 0),      // len-1
        (vec![2], 2, 0),      // discriminating case
        (vec![15, 1], 7, 10), // len-2
    ];

    let eval_def = masked_step_prim_def(4);
    let eval_iso = Iso::fold(
        Type::list_spine_ty(Type::Flat(4), d),
        Iso::Prim(Box::new(eval_def)),
    );

    for (elems, key, acc0) in &samples {
        let input = masked_fold_input(elems, *key, *acc0, d);
        let mut fuel = 100_000_000u64;
        let expected =
            theseus_eval::eval(&eval_iso, input.clone(), &mut fuel).expect("eval maskedFold D=3");

        let got = run_on_cpu_resident(&iso, &input, 0, |iso, v, fuel| {
            theseus_eval::eval(iso, v, fuel)
        })
        .expect("run_on_cpu_resident maskedFold D=3");

        assert_eq!(
            got, expected,
            "T-real: real CPU mismatch on elems={elems:?}, key={key}, acc0={acc0}"
        );
    }
}

// ── T7: SystemRing ancillas_restored + bus_quiescent (closure) ──────

#[test]
fn t7_system_ring_closure() {
    let ring = ccnot_ring();

    // The ring must close: ancillas restored + bus quiescent on a representative state.
    let state = StateVals {
        regs: [0x00, 0xFF, 0xAA, 0x55, 0x01, 0x80, 0x7F, 0xC3],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; 8],
        mem: [0; MEM_WORDS],
    };
    assert!(
        ring.ancillas_restored(&state),
        "SystemRing ancillas not restored"
    );
    assert!(ring.bus_quiescent(&state), "SystemRing bus not quiescent");
}
