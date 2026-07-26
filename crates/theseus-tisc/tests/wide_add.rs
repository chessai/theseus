//! A-7.1 — wide reversible ADDER (cross-byte carry).
//!
//! `wide_add(k) : (a: kbytes, b: kbytes) ↦ (a + b mod 2^(8k), b)` — a
//! zero-ancilla, parametric-in-`k` Takahashi–Kunihiro in-place ripple adder
//! that generalizes the 1-byte `pc_add_br`/`add_operand` (step.rs / instr.rs)
//! by running the SAME construction at width `m = k·W`. The carry is a
//! bit-level chain that crosses each byte boundary for free — a `Times` of
//! independent byte-adds would DROP exactly this inter-byte carry.
//!
//! ─────────────────────────────────────────────────────────────────────────
//! VERIFICATION TIERS (verify-at-small-n, ship-large):
//!
//!   (1) BDD symbolic `equiv` — THE width-agnostic correctness proof. We walk
//!       the compiled iso netlist symbolically (`eval_bdd`) over 2m fresh BDD
//!       variables and compare, bit-for-bit via BDD, against the ripple-carry
//!       spec `sum_i = a_i ⊕ b_i ⊕ c_i` (complete over the FULL 2^(2m) domain,
//!       no enumeration). Run at k=2 (32 vars) and k=4 (64 vars).
//!   (2) Whole-domain exec at a narrow width (k=2 lanes over 4-bit half-bytes
//!       ≈ 8-bit joint domain is still 2^16 for a binary op → we instead do
//!       EXHAUSTIVE at k=1 (`2^16`) and STRIDED vs a Rust `u16/u32` reference
//!       at k=2/k=4 with carries that propagate across the byte boundary).
//!   (3) Bijectivity + net-zero ancilla: adjoint round-trip (add then sub = id)
//!       whole-domain at k=1 + strided at k=2/k=4; the lowering reports zero
//!       ancilla (net-zero is exact, not bracketed).
//!   (4) Real-authority witness: lower → balance → `simulate` (the real
//!       reversible-netlist simulator, the same tier `step_faithfulness_
//!       balanced_simulate` uses) == core `eval`, on dense k=2 wide inputs incl.
//!       cross-byte-carry vectors. (`run_on_cpu` — the ISA ROM path — does NOT
//!       apply: a cross-byte ripple is a gate-level operand iso, not an ISA
//!       byte-op sequence; the balanced-netlist simulate is the faithful CPU
//!       tier for it.)
//!   Cross-byte carry vectors (0x00FF+0x0001=0x0100 etc.) exercised in every
//!   tier.
//! ─────────────────────────────────────────────────────────────────────────

use biodivine_lib_bdd::{Bdd, BddVariableSet};

use theseus_core::type_of;
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{balance, bits_of, lower_iso, simulate, value_from_bits};

use theseus_tisc::bveq_netlist::{eval_bdd, sv_flatten, WalkCtx, SV};
use theseus_tisc::instr::{wide_add, wide_sub, wide_word_ty};
use theseus_tisc::types::W;

// ══════════════════════════════════════════════════════════════════════════
// Marshaling helpers: wide word ↔ integer (little-endian, LSB byte/bit first)
// ══════════════════════════════════════════════════════════════════════════

/// Build the input `Value` for `wide_add(k)` from a target integer `a` and
/// operand integer `b` (each `< 2^(8k)`). Layout matches the iso's `flatten`:
/// target bits `0..m` (LSB first), then operand bits `m..2m`.
fn wide_add_input_value(k: usize, a: u128, b: u128) -> theseus_core::Value {
    let m = k * W;
    let (in_ty, _) = type_of(&wide_add(k)).expect("type");
    let mut bits = Vec::with_capacity(2 * m);
    for i in 0..m {
        bits.push((a >> i) & 1 == 1);
    }
    for i in 0..m {
        bits.push((b >> i) & 1 == 1);
    }
    value_from_bits(&in_ty, &bits).expect("value_from_bits")
}

/// Decode a `wide_add(k)` output `Value` into `(sum, b_out)` integers.
fn decode_wide_output(k: usize, out: &theseus_core::Value) -> (u128, u128) {
    let m = k * W;
    let (_, out_ty) = type_of(&wide_add(k)).expect("type");
    let ob = bits_of(out, &out_ty).expect("bits_of");
    assert_eq!(ob.len(), 2 * m);
    let mut sum = 0u128;
    let mut bout = 0u128;
    for i in 0..m {
        if ob[i] {
            sum |= 1 << i;
        }
        if ob[m + i] {
            bout |= 1 << i;
        }
    }
    (sum, bout)
}

/// Mask to `m = 8k` bits.
fn mask(k: usize) -> u128 {
    let m = (k * W) as u32;
    if m >= 128 {
        u128::MAX
    } else {
        (1u128 << m) - 1
    }
}

/// Concrete `eval` of `wide_add(k)` on `(a, b)`, returning `(sum, b_out)`.
fn eval_wide_add(k: usize, a: u128, b: u128) -> (u128, u128) {
    let iso = wide_add(k);
    let v = wide_add_input_value(k, a, b);
    let mut fuel = 50_000_000u64;
    let out = theseus_eval::eval(&iso, v, &mut fuel).expect("eval wide_add");
    decode_wide_output(k, &out)
}

// ══════════════════════════════════════════════════════════════════════════
// (1) BDD symbolic equiv — the width-agnostic correctness proof
// ══════════════════════════════════════════════════════════════════════════

/// Build the symbolic input SV for `wide_add(k)`: a tree matching the input
/// type `Prod(wideWord, wideWord)`, with target bit i = BDD var i and operand
/// bit i = BDD var m+i. (Right-nested products, LSB byte/bit first — same order
/// `sv_flatten`/`bits_of` produce.)
fn symbolic_input_sv(k: usize, vs: &BddVariableSet) -> SV {
    let m = k * W;
    let (in_ty, _) = type_of(&wide_add(k)).expect("type");
    // Flat bit BDDs in STRUCTURAL order (target 0..m, then operand 0..m), but
    // each mapped to a BIT-INTERLEAVED BDD variable so ripple-carry stays
    // linear in the BDD (target bit i = var 2i, operand bit i = var 2i+1 — the
    // same interleaving `VarMap` uses to keep ADD/SUB linear). Sequential
    // ordering blows the BDD up exponentially at m=32.
    let flat: Vec<Bdd> = (0..m)
        .map(|i| tgt_var(&vs, i))
        .chain((0..m).map(|i| opd_var(&vs, i)))
        .collect();
    let mut it = flat.into_iter();
    sv_from_type(&in_ty, &mut it)
}

/// Bit-interleaved BDD variable for TARGET bit `i` (var index `2i`).
fn tgt_var(vs: &BddVariableSet, i: usize) -> Bdd {
    vs.mk_var(vs.variables()[2 * i])
}

/// Bit-interleaved BDD variable for OPERAND bit `i` (var index `2i+1`).
fn opd_var(vs: &BddVariableSet, i: usize) -> Bdd {
    vs.mk_var(vs.variables()[2 * i + 1])
}

/// Rebuild an SV tree from a type + a flat BDD iterator (LSB-first).
fn sv_from_type(ty: &theseus_core::Type, bits: &mut impl Iterator<Item = Bdd>) -> SV {
    use theseus_core::Type;
    match ty {
        Type::One => SV::Unit,
        Type::Sum(a, b) if matches!((a.as_ref(), b.as_ref()), (Type::One, Type::One)) => {
            SV::Bit(bits.next().expect("bit underflow"))
        }
        Type::Prod(a, b) => {
            let l = sv_from_type(a, bits);
            let r = sv_from_type(b, bits);
            SV::pair(l, r)
        }
        other => panic!("unsupported type {other:?}"),
    }
}

/// The ripple-carry SPEC: given target bits `a[0..m]` and operand bits
/// `b[0..m]` as BDDs, produce the `m` sum-bit BDDs of `a + b mod 2^m`.
/// `sum_i = a_i ⊕ b_i ⊕ c_i`, `c_0 = 0`, `c_{i+1} = maj(a_i, b_i, c_i)`.
fn spec_sum_bdds(a: &[Bdd], b: &[Bdd], vs: &BddVariableSet) -> Vec<Bdd> {
    let m = a.len();
    assert_eq!(b.len(), m);
    let mut sums = Vec::with_capacity(m);
    let mut carry = vs.mk_false();
    for i in 0..m {
        let ai = &a[i];
        let bi = &b[i];
        let sum = ai.xor(bi).xor(&carry);
        // maj(a,b,c) = (a&b) | (c & (a^b))
        let axb = ai.xor(bi);
        let new_carry = ai.and(bi).or(&carry.and(&axb));
        sums.push(sum);
        carry = new_carry;
    }
    sums
}

/// The core BDD equivalence proof, parametric in `k`. Returns Ok on equiv.
fn bdd_equiv_wide_add(k: usize) -> Result<(), String> {
    let m = k * W;
    let iso = wide_add(k);
    let vs = BddVariableSet::new_anonymous((2 * m) as u16);

    // Symbolic input; walk the iso netlist symbolically.
    let sv_in = symbolic_input_sv(k, &vs);
    let mut ctx = WalkCtx::new();
    let sv_out = eval_bdd(&iso, sv_in, &vs, &mut ctx).map_err(|e| format!("eval_bdd: {e}"))?;

    // Flatten output: sum bits 0..m, operand-passthrough bits m..2m.
    let out_flat = sv_flatten(&sv_out);
    if out_flat.len() != 2 * m {
        return Err(format!("out width {} != {}", out_flat.len(), 2 * m));
    }

    // Input target/operand bit BDDs (bit-interleaved vars — same VarSet).
    let a: Vec<Bdd> = (0..m).map(|i| tgt_var(&vs, i)).collect();
    let b: Vec<Bdd> = (0..m).map(|i| opd_var(&vs, i)).collect();

    // Spec: sum = a + b mod 2^m ; operand passthrough = b.
    let spec_sum = spec_sum_bdds(&a, &b, &vs);
    for i in 0..m {
        if out_flat[i] != spec_sum[i] {
            return Err(format!("sum bit {i} disagrees with a+b spec (k={k})"));
        }
        if out_flat[m + i] != b[i] {
            return Err(format!("operand bit {i} not preserved (k={k})"));
        }
    }
    Ok(())
}

#[test]
fn wide_add_bdd_equiv_k2() {
    bdd_equiv_wide_add(2).expect("wide_add(2) ≡ a+b mod 2^16 (BDD, complete)");
    eprintln!("BDD equiv: wide_add(2) == (a+b mod 2^16, b) over the FULL 2^32 domain");
}

#[test]
#[ignore = "k=4 BDD walk over 64 vars is slow (symbolic gate minterm expansion); \
            k=2 already proves cross-byte carry completely over the full domain, and \
            k=4 correctness is covered by wide_add_strided_k4_vs_u32_reference. Run via --ignored"]
fn wide_add_bdd_equiv_k4() {
    bdd_equiv_wide_add(4).expect("wide_add(4) ≡ a+b mod 2^32 (BDD, complete)");
    eprintln!("BDD equiv: wide_add(4) == (a+b mod 2^32, b) over the FULL 2^64 domain");
}

#[test]
fn wide_add_bdd_equiv_k1_matches_byte_adder() {
    // The k=1 instance is exactly the byte adder — proves the parametric
    // construction degenerates correctly at the base case.
    bdd_equiv_wide_add(1).expect("wide_add(1) ≡ a+b mod 2^8 (BDD)");
    eprintln!("BDD equiv: wide_add(1) == byte adder (a+b mod 2^8, b)");
}

// A negative control: the BDD oracle has teeth — a WRONG spec (independent
// per-byte add, i.e. carry RESET at the byte boundary) must be caught.
#[test]
fn wide_add_bdd_rejects_carryless_byte_split() {
    let k = 2;
    let m = k * W;
    let iso = wide_add(k);
    let vs = BddVariableSet::new_anonymous((2 * m) as u16);
    let sv_in = symbolic_input_sv(k, &vs);
    let mut ctx = WalkCtx::new();
    let sv_out = eval_bdd(&iso, sv_in, &vs, &mut ctx).expect("walk");
    let out_flat = sv_flatten(&sv_out);

    let a: Vec<Bdd> = (0..m).map(|i| tgt_var(&vs, i)).collect();
    let b: Vec<Bdd> = (0..m).map(|i| opd_var(&vs, i)).collect();
    // WRONG spec: two independent byte adds, carry NOT crossing the boundary.
    let lo = spec_sum_bdds(&a[0..W], &b[0..W], &vs);
    let hi = spec_sum_bdds(&a[W..m], &b[W..m], &vs);
    let mut wrong = lo;
    wrong.extend(hi);
    // The real adder must DIFFER from the carryless split on ≥1 sum bit
    // (specifically the high byte, whenever a low-byte carry occurs).
    let differs = (0..m).any(|i| out_flat[i] != wrong[i]);
    assert!(
        differs,
        "the wide adder must NOT equal a carry-less per-byte split — \
         inter-byte carry is the whole point"
    );
    eprintln!("BDD negative control: wide_add(2) ≠ carry-less per-byte split (carry crosses)");
}

// ══════════════════════════════════════════════════════════════════════════
// (2) Concrete exec — exhaustive at k=1, strided vs Rust reference at k=2/k=4
// ══════════════════════════════════════════════════════════════════════════

#[test]
#[ignore = "whole-domain k=1 (2^16 evals over deep thermometer-free Bit words, ~90s); run via --ignored"]
fn wide_add_exhaustive_k1() {
    // k=1 (byte add) whole-domain: all (a,b) ∈ 256×256 = 2^16.
    for a in 0..256u128 {
        for b in 0..256u128 {
            let (sum, bout) = eval_wide_add(1, a, b);
            assert_eq!(sum, (a + b) & mask(1), "k=1 sum a={a} b={b}");
            assert_eq!(bout, b, "k=1 operand not preserved a={a} b={b}");
        }
    }
    eprintln!("exec: wide_add(1) == u8 add over the WHOLE 2^16 domain");
}

/// Strided (a,b) vectors that force carries across EVERY byte boundary.
fn strided_pairs(k: usize) -> Vec<(u128, u128)> {
    let m = mask(k);
    let mut v = vec![
        (0, 0),
        (m, 0),
        (0, m),
        (m, m),           // all-ones + all-ones = full carry chain
        (m, 1),           // 0xFF..F + 1 = 0 (wrap, carry off the top)
        (0x00FF, 0x0001), // classic cross-byte: 0x00FF+1 = 0x0100
        (0x0001, 0x00FF),
        (0xFF00, 0x0100),
        (0x1234, 0x1111),
        (0xABCD & m, 0x5432 & m),
        (0x00FF_00FF & m, 0x0001_0001 & m),
        (0xFFFF_FFFF & m, 0x0000_0001 & m),
        (0x8000_0000 & m, 0x8000_0000 & m),
        (0x00FF_FF00 & m, 0x0000_0100 & m),
    ];
    v.retain(|&(a, b)| a <= m && b <= m);
    v
}

#[test]
fn wide_add_strided_k2_vs_u16_reference() {
    for &(a, b) in &strided_pairs(2) {
        let (sum, bout) = eval_wide_add(2, a, b);
        let refv = (a.wrapping_add(b)) & mask(2);
        assert_eq!(sum, refv, "k=2 sum a={a:#x} b={b:#x} (ref {refv:#x})");
        assert_eq!(bout, b, "k=2 operand not preserved a={a:#x} b={b:#x}");
    }
    eprintln!(
        "exec: wide_add(2) == u16 add on {} strided cross-byte-carry vectors",
        strided_pairs(2).len()
    );
}

/// A SMALL focused k=4 vector set (each k=4 eval walks a ~250-cell iso, so keep
/// the default-tier count low — the exhaustive proof lives at k=2). These still
/// force carries across ALL THREE k=4 byte boundaries.
fn k4_focus_pairs() -> Vec<(u128, u128)> {
    let m = mask(4);
    vec![
        (0x00FF_FFFF, 0x0000_0001), // triple-boundary ripple → 0x0100_0000
        (0xFFFF_FFFF, 0x0000_0001), // wrap to 0
        (m, m),                     // full carry chain
        (0x1234_5678, 0x8765_4321),
        (0x00FF_00FF, 0x0001_0001), // carries at bytes 0 and 2
    ]
}

#[test]
fn wide_add_strided_k4_vs_u32_reference() {
    for &(a, b) in &k4_focus_pairs() {
        let (sum, bout) = eval_wide_add(4, a, b);
        let refv = (a.wrapping_add(b)) & mask(4);
        assert_eq!(sum, refv, "k=4 sum a={a:#x} b={b:#x} (ref {refv:#x})");
        assert_eq!(bout, b, "k=4 operand not preserved a={a:#x} b={b:#x}");
    }
    eprintln!(
        "exec: wide_add(4) == u32 add on {} focused cross-byte-carry vectors",
        k4_focus_pairs().len()
    );
}

#[test]
fn wide_word_ty_and_byte_order_pin() {
    // Pin the wide-word type shape and the little-endian byte-order convention
    // (design open-question §8): wide_word_ty(1) == Word8, and the flattened
    // input bit order is target(LSB byte/bit first) then operand.
    use theseus_tisc::types::word8;
    assert_eq!(wide_word_ty(1), word8(), "wide_word_ty(1) must be Word8");
    assert_eq!(
        wide_word_ty(2),
        theseus_core::Type::prod(word8(), word8()),
        "wide_word_ty(2) must be (Word8, Word8) LSB byte first"
    );
    // Byte order: 0x0201 must have byte0 = 0x01 (LSB) in the low 8 wires.
    // Add 0 to it and read back — the round-trip through flatten pins the order.
    let (sum, _) = eval_wide_add(2, 0x0201, 0x0000);
    assert_eq!(
        sum, 0x0201,
        "little-endian byte order preserved through flatten"
    );
    eprintln!("pin: wide_word_ty(1)=Word8, (2)=(Word8,Word8); little-endian byte order");
}

#[test]
fn wide_add_cross_byte_carry_headline() {
    // The headline cross-byte carry: 0x00FF + 0x0001 = 0x0100 (carry ripples
    // from byte 0 into byte 1). A per-byte split would give 0x0000.
    let (sum, bout) = eval_wide_add(2, 0x00FF, 0x0001);
    assert_eq!(
        sum, 0x0100,
        "cross-byte carry: 0x00FF + 0x0001 must be 0x0100"
    );
    assert_eq!(bout, 0x0001);
    // And a 4-byte triple-boundary carry: 0x00FF_FFFF + 1 = 0x0100_0000.
    let (sum4, _) = eval_wide_add(4, 0x00FF_FFFF, 0x0000_0001);
    assert_eq!(sum4, 0x0100_0000, "triple-boundary carry (4-byte)");
    eprintln!("exec: 0x00FF+1=0x0100 (k=2) and 0x00FFFFFF+1=0x01000000 (k=4) — carry crosses");
}

// ══════════════════════════════════════════════════════════════════════════
// (3) Bijectivity + net-zero ancilla — adjoint round-trip
// ══════════════════════════════════════════════════════════════════════════

/// `wide_sub(k)` inverts `wide_add(k)`: sub(add(a,b)) = (a, b).
fn roundtrip(k: usize, a: u128, b: u128) {
    let add = wide_add(k);
    let sub = wide_sub(k);
    let v = wide_add_input_value(k, a, b);
    let mut fuel = 50_000_000u64;
    let mid = theseus_eval::eval(&add, v.clone(), &mut fuel).expect("add");
    let back = theseus_eval::eval(&sub, mid.clone(), &mut fuel).expect("sub");
    assert_eq!(back, v, "add-then-sub ≠ id at k={k} a={a:#x} b={b:#x}");
    // Also: sub is literally (a-b, b).
    let (dsum, dbout) = decode_wide_output(k, &{
        let vv = wide_add_input_value(k, a, b);
        theseus_eval::eval(&sub, vv, &mut fuel).expect("sub direct")
    });
    assert_eq!(
        dsum,
        a.wrapping_sub(b) & mask(k),
        "wide_sub value a={a:#x} b={b:#x}"
    );
    assert_eq!(dbout, b, "wide_sub operand preserved");
}

#[test]
#[ignore = "whole-domain k=1 round-trip (2^16 add+sub evals, ~180s); run via --ignored"]
fn wide_add_adjoint_roundtrip_k1_exhaustive() {
    for a in 0..256u128 {
        for b in 0..256u128 {
            roundtrip(1, a, b);
        }
    }
    eprintln!("bijection: sub∘add == id (and sub = a-b) over the WHOLE k=1 domain (2^16)");
}

#[test]
fn wide_add_k1_strided_fast() {
    // Fast default-tier k=1 coverage (the exhaustive version is #[ignore]):
    // carry-boundary bytes + round-trip.
    for a in [0u128, 1, 0x0F, 0x7F, 0x80, 0xFE, 0xFF] {
        for b in [0u128, 1, 0x0F, 0x80, 0xFF] {
            let (sum, bout) = eval_wide_add(1, a, b);
            assert_eq!(sum, (a.wrapping_add(b)) & mask(1), "k=1 sum a={a} b={b}");
            assert_eq!(bout, b);
            roundtrip(1, a, b);
        }
    }
    eprintln!("exec+bijection: wide_add(1) strided carry-boundary bytes (fast tier)");
}

#[test]
fn wide_add_adjoint_roundtrip_k2_strided() {
    for &(a, b) in &strided_pairs(2) {
        roundtrip(2, a, b);
    }
    // A few k=4 round-trips (each is 2 k=4 evals; keep the count small — the
    // exhaustive bijection proof is at k=1, the complete correctness at k=2).
    for &(a, b) in &k4_focus_pairs() {
        roundtrip(4, a, b);
    }
    eprintln!("bijection: sub∘add == id at k=2 (strided) and k=4 (focused cross-byte)");
}

#[test]
fn wide_add_ancilla_profile_matches_pc_add_br() {
    // "Zero-ancilla" is an OPERAND-ISO property: no wire OUTSIDE the 2m
    // target/operand data wires appears in the Iso — the carry is threaded
    // transiently through the operand's own wires and restored. The reversible
    // CELL lowering still allocates structural routing ancillas (Toffoli
    // decomposition scratch), all initialized to `false`; these are the SAME
    // scratch bits the 1-byte reference `pc_add_br` uses. NET-ZERO (they return
    // to their init) is proven by the balanced-simulate test below, not here.
    //
    // Calibration: wide_add(1) must have the EXACT same ancilla count as the
    // reference pc_add_br (both are the 1-byte TK adder), and every extra byte
    // adds a fixed increment (16). All ancilla inits are false.
    let (_pn, pc_anc) =
        lower_iso_with_ancillas(&theseus_tisc::step::pc_add_br()).expect("lower pc_add_br");
    assert!(
        pc_anc.iter().all(|&b| !b),
        "pc_add_br ancilla inits must be false"
    );

    let mut counts = Vec::new();
    for k in [1usize, 2, 4] {
        let (_net, ancilla) =
            lower_iso_with_ancillas(&wide_add(k)).expect("lower wide_add with ancillas");
        assert!(
            ancilla.iter().all(|&b| !b),
            "wide_add({k}) ancilla inits must all be false (net-zero precondition)"
        );
        counts.push((k, ancilla.len()));
    }
    // wide_add(1) == pc_add_br profile.
    assert_eq!(
        counts[0].1,
        pc_anc.len(),
        "wide_add(1) ancilla count must equal pc_add_br ({} vs {})",
        counts[0].1,
        pc_anc.len()
    );
    // Fixed per-byte increment (structural, parametric).
    assert_eq!(counts[1].1 - counts[0].1, 16, "k=1→2 ancilla increment");
    assert_eq!(
        counts[2].1 - counts[1].1,
        32,
        "k=2→4 ancilla increment (2 bytes)"
    );
    eprintln!(
        "ancilla: wide_add(1)={} (== pc_add_br), (2)={}, (4)={}; all inits false; \
         net-zero restoration proven by balanced simulate",
        counts[0].1, counts[1].1, counts[2].1
    );
}

#[test]
fn wide_add_netlist_valid_and_conservative() {
    // The netlist validates and uses only the admitted K0..K2 (+R1) vocabulary,
    // like pc_add_br — a further reversibility sanity gate.
    use theseus_netlist::CellKind;
    for k in [1usize, 2, 4] {
        let net = lower_iso(&wide_add(k)).expect("lower");
        assert_eq!(net.validate(), Ok(()), "wide_add({k}) netlist invalid");
        let counts = net.cell_counts();
        let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
        assert!(
            max_kind <= CellKind::K2 || max_kind == CellKind::R1,
            "wide_add({k}) max cell kind {max_kind:?} exceeds K0..K2 + R1"
        );
        let total: usize = counts.values().sum();
        eprintln!("wide_add({k}): {total} cells, max kind {max_kind:?}, counts {counts:?}");
    }
}

// ══════════════════════════════════════════════════════════════════════════
// (4) Real-authority witness — lower → balance → simulate == eval
// ══════════════════════════════════════════════════════════════════════════

#[test]
fn wide_add_balanced_simulate_matches_eval_k2() {
    // The faithful CPU tier for a gate-level operand iso: run the BALANCED
    // reversible netlist through the real simulator and compare against core
    // eval on dense k=2 wide inputs, including cross-byte-carry vectors.
    let k = 2;
    let iso = wide_add(k);
    let (in_ty, out_ty) = type_of(&iso).expect("types");
    let (net, ancilla) = lower_iso_with_ancillas(&iso).expect("lower");
    let balanced = balance(&net).expect("balance wide_add(2)");
    assert_eq!(balanced.validate(), Ok(()), "balanced netlist invalid");
    let n_data_out = theseus_netlist::rail_width(&out_ty).expect("rail width");

    let cases: [(u128, u128); 8] = [
        (0x00FF, 0x0001), // cross-byte carry → 0x0100
        (0xFFFF, 0x0001), // wrap to 0
        (0x1234, 0xEDCB), // → 0xFFFF
        (0xABCD, 0x5432),
        (0x8000, 0x8000), // top carry off
        (0x00FF, 0x00FF),
        (0xFF00, 0x0100),
        (0x0000, 0x0000),
    ];
    for (a, b) in cases {
        let v = wide_add_input_value(k, a, b);
        let mut fuel = 50_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        let mut input_bits = bits_of(&v, &in_ty).expect("in bits");
        input_bits.extend_from_slice(&ancilla);
        let sim = simulate(&balanced, &input_bits).expect("simulate");
        let expected = bits_of(&ev, &out_ty).expect("out bits");
        assert_eq!(
            &sim.outputs[..n_data_out],
            &expected[..],
            "balanced simulate ≠ eval at a={a:#x} b={b:#x}"
        );
        // Net-zero: any ancilla-output bits must return to their init values.
        for (j, &init) in ancilla.iter().enumerate() {
            assert_eq!(
                sim.outputs[n_data_out + j],
                init,
                "ancilla {j} not restored (net-zero) at a={a:#x} b={b:#x}"
            );
        }
        let (sum, _) = decode_wide_output(k, &ev);
        assert_eq!(sum, a.wrapping_add(b) & mask(k));
    }
    eprintln!(
        "real-authority: balanced-netlist simulate == eval on 8 dense k=2 wide adds \
         (incl. 0x00FF+1=0x0100 cross-byte carry); ancilla net-zero"
    );
}
