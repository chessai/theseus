//! TISC-1 structural + immediate instruction isos (M4-L1-G1).
//!
//! EXCH / SWAPBR / XCHG — pure word-pair swaps (SwapProd, 0 cells).
//! XORI  — per-bit conditional NOT (0 cells, rail swaps).
//! ROTLI / ROTRI — bit-position rotations (structural permutation, 0 cells).
//! XOR   — 8 per-bit CNOTs via interleave-CNOT-deinterleave (K1 cells).
//!
//! Every iso is oracle-verified: lower_iso + untimed eval agrees with
//! theseus_eval::eval, and eval matches the reference semantics.

use theseus_core::{type_of, Iso, Type, Value};
use theseus_netlist::{
    bits_of, lower_iso, prim_from_bits, rail_width, CellKind, PhasedCellNetlist, RailPair,
};

// ── Type helpers ─────────────────────────────────────────────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn word8() -> Type {
    let b = bit();
    let mut ty = Type::prod(b.clone(), b.clone());
    for _ in 0..6 {
        ty = Type::prod(b.clone(), ty);
    }
    ty
}

// ── Value encode / decode ────────────────────────────────────────────

fn bit_val(v: bool) -> Value {
    if v {
        Value::left(Value::Unit)
    } else {
        Value::right(Value::Unit)
    }
}

fn bit_from_val(v: &Value) -> bool {
    matches!(v, Value::Left(_))
}

fn encode_u8(val: u8) -> Value {
    let bits: Vec<Value> = (0..8).map(|i| bit_val((val >> i) & 1 == 1)).collect();
    let mut result = bits[7].clone();
    for i in (0..7).rev() {
        result = Value::pair(bits[i].clone(), result);
    }
    result
}

fn decode_u8(val: &Value) -> u8 {
    let mut result = 0u8;
    let mut current = val;
    for i in 0..7 {
        if let Value::Pair(left, right) = current {
            if bit_from_val(left) {
                result |= 1 << i;
            }
            current = right;
        } else {
            panic!("expected Pair at bit {i}");
        }
    }
    if bit_from_val(current) {
        result |= 1 << 7;
    }
    result
}

fn encode_pair(a: u8, b: u8) -> Value {
    Value::pair(encode_u8(a), encode_u8(b))
}

fn decode_pair(v: &Value) -> (u8, u8) {
    if let Value::Pair(a, b) = v {
        (decode_u8(a), decode_u8(b))
    } else {
        panic!("expected Pair for word8 pair");
    }
}

// ── Untimed netlist evaluator (with ancilla support) ─────────────────

fn eval_cells(net: &PhasedCellNetlist, input_bits: &[bool]) -> Vec<bool> {
    let mut line: Vec<Option<bool>> = vec![None; net.num_lines as usize];
    fn set_pair(line: &mut [Option<bool>], p: RailPair, v: bool) {
        line[p.line1.0 as usize] = Some(v);
        line[p.line0.0 as usize] = Some(!v);
    }
    fn get_pair(line: &[Option<bool>], p: RailPair) -> bool {
        let v1 = line[p.line1.0 as usize].expect("1-line driven");
        let v0 = line[p.line0.0 as usize].expect("0-line driven");
        assert_ne!(v0, v1, "INV-TOKEN violated");
        v1
    }
    assert_eq!(input_bits.len(), net.inputs.len());
    for (&p, &v) in net.inputs.iter().zip(input_bits) {
        set_pair(&mut line, p, v);
    }
    for cell in &net.cells {
        let vals: Vec<bool> = cell.inputs.iter().map(|&p| get_pair(&line, p)).collect();
        let outs = cell.kind.eval(&vals);
        for (&p, &v) in cell.outputs.iter().zip(&outs) {
            set_pair(&mut line, p, v);
        }
    }
    net.outputs.iter().map(|&p| get_pair(&line, p)).collect()
}

fn extend_with_ancilla(data_bits: &[bool], net: &PhasedCellNetlist) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    let n_ancilla = net.inputs.len() - data_bits.len();
    for i in 0..n_ancilla {
        bits.push(i % 2 == 0);
    }
    bits
}

// ── Right-nested product helper ──────────────────────────────────────

fn right_nested_prod(types: &[Type]) -> Type {
    assert!(!types.is_empty());
    if types.len() == 1 {
        return types[0].clone();
    }
    Type::prod(types[0].clone(), right_nested_prod(&types[1..]))
}

// ── Instruction iso builders ─────────────────────────────────────────

/// NOT on a single Bit = SwapSum(1,1) — zero cells, pure rail swap.
fn not_iso() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// EXCH / SWAPBR / XCHG: swap two Word8 values.
/// Type: (Word8, Word8) <-> (Word8, Word8).
/// Self-inverse; 0 cells (pure wire permutation).
fn swap_words_iso() -> Iso {
    Iso::SwapProd(word8(), word8())
}

/// XORI rd, imm8: XOR rd with compile-time constant.
/// Per-bit NOT where imm bit is 1, Id where 0.
/// Type: Word8 <-> Word8. Self-inverse; 0 cells.
fn xori_iso(imm: u8) -> Iso {
    let b = bit();
    let isos: Vec<Iso> = (0..8)
        .map(|i| {
            if (imm >> i) & 1 == 1 {
                not_iso()
            } else {
                Iso::Id(b.clone())
            }
        })
        .collect();
    let mut iso = isos[7].clone();
    for i in (0..7).rev() {
        iso = Iso::times(isos[i].clone(), iso);
    }
    iso
}

/// Swap elements at positions `pos` and `pos+1` in a right-nested product.
fn swap_adjacent(types: &[Type], pos: usize) -> Iso {
    let n = types.len();
    assert!(pos + 1 < n);

    let a = types[pos].clone();
    let b = types[pos + 1].clone();

    let inner = if pos + 2 == n {
        Iso::SwapProd(a, b)
    } else {
        let rest = right_nested_prod(&types[pos + 2..]);
        Iso::seq(
            Iso::AssocLProd(a.clone(), b.clone(), rest.clone()),
            Iso::seq(
                Iso::times(Iso::SwapProd(a.clone(), b.clone()), Iso::Id(rest.clone())),
                Iso::AssocRProd(b, a, rest),
            ),
        )
    };

    let mut iso = inner;
    for i in (0..pos).rev() {
        iso = Iso::times(Iso::Id(types[i].clone()), iso);
    }
    iso
}

/// Bubble element at position `from` leftward to position `to` (from > to).
/// Used by rotl_iso to move the MSB to position 0.
fn bubble_left(types: &[Type], from: usize, to: usize) -> Iso {
    assert!(from > to);
    let mut iso = swap_adjacent(types, from - 1);
    for pos in (to..from - 1).rev() {
        iso = Iso::seq(iso, swap_adjacent(types, pos));
    }
    iso
}

/// ROTLI rd, imm8: rotate rd left by (imm8 mod 8) bit positions.
/// Rotate-left means bit i moves to position (i+1)%8 in the product.
/// Implemented by moving the last element to position 0, k times.
/// Type: Word8 <-> Word8. Adjoint is ROTRI. 0 cells.
fn rotl_iso(k: usize) -> Iso {
    let k = k % 8;
    if k == 0 {
        return Iso::Id(word8());
    }
    let bits = vec![bit(); 8];
    // Move last element to front = rotate_left(1) on the u8 value.
    let rotate_once = bubble_left(&bits, 7, 0);
    let mut iso = rotate_once.clone();
    for _ in 1..k {
        iso = Iso::seq(iso, rotate_once.clone());
    }
    iso
}

/// ROTRI rd, imm8: rotate rd right by (imm8 mod 8) = adjoint of ROTLI.
/// Type: Word8 <-> Word8. 0 cells.
fn rotr_iso(k: usize) -> Iso {
    let k = k % 8;
    if k == 0 {
        return Iso::Id(word8());
    }
    Iso::sym(rotl_iso(k))
}

/// Structural iso: (A×B) × (C×D) → (A×C) × (B×D).
/// Extracts the first element of each word into a pair.
fn extract_first_pair(a: Type, b: Type, c: Type, d: Type) -> Iso {
    let cd = Type::prod(c.clone(), d.clone());
    let bd = Type::prod(b.clone(), d.clone());

    Iso::seq(
        Iso::AssocRProd(a.clone(), b.clone(), cd),
        Iso::seq(
            Iso::times(
                Iso::Id(a.clone()),
                Iso::seq(
                    Iso::AssocLProd(b.clone(), c.clone(), d.clone()),
                    Iso::seq(
                        Iso::times(Iso::SwapProd(b.clone(), c.clone()), Iso::Id(d.clone())),
                        Iso::AssocRProd(c.clone(), b, d),
                    ),
                ),
            ),
            Iso::AssocLProd(a, c, bd),
        ),
    )
}

/// Interleave (zip) two right-nested n-element products of Bit:
///   (a0×…×a_{n-1}) × (b0×…×b_{n-1}) → (a0×b0) × ((a1×b1) × … × (a_{n-1}×b_{n-1}))
fn zip_products(n: usize) -> Iso {
    let b = bit();
    if n == 1 {
        return Iso::Id(Type::prod(b.clone(), b.clone()));
    }

    let rest_ty = right_nested_prod(&vec![b.clone(); n - 1]);
    let pair_ty = Type::prod(b.clone(), b.clone());

    let extract = extract_first_pair(b.clone(), rest_ty.clone(), b.clone(), rest_ty.clone());
    let zip_rest = zip_products(n - 1);

    Iso::seq(extract, Iso::times(Iso::Id(pair_ty), zip_rest))
}

/// CNOT prim: (target, ctrl) → (target XOR ctrl, ctrl).
fn cnot_prim() -> Iso {
    let b = bit();
    let ty = Type::prod(b.clone(), b.clone());
    let def = prim_from_bits("cnot_tc", ty.clone(), ty, |bits| {
        vec![bits[0] ^ bits[1], bits[1]]
    })
    .unwrap();
    Iso::Prim(Box::new(def))
}

/// XOR rd, rs: rd ← rd ⊕ rs, rs preserved.
/// Type: (Word8, Word8) <-> (Word8, Word8). Self-inverse; uses K1 cells.
fn xor_rd_rs_iso() -> Iso {
    let interleave = zip_products(8);
    let cnot = cnot_prim();
    let mut cnots = cnot.clone();
    for _ in 1..8 {
        cnots = Iso::times(cnot.clone(), cnots);
    }
    let deinterleave = Iso::sym(interleave.clone());
    Iso::seq(interleave, Iso::seq(cnots, deinterleave))
}

// ── Common verification ──────────────────────────────────────────────

fn verify_lowering(name: &str, iso: &Iso) -> PhasedCellNetlist {
    let net = lower_iso(iso).unwrap_or_else(|e| panic!("{name}: lower_iso failed: {e:?}"));
    assert_eq!(net.validate(), Ok(()), "{name}: netlist validation failed");
    let counts = net.cell_counts();
    for &kind in counts.keys() {
        assert!(
            kind <= CellKind::K3,
            "{name}: cell kind {kind:?} exceeds K3"
        );
    }
    eprintln!("{name}: cell counts = {counts:?}");
    net
}

fn assert_faithful(
    name: &str,
    iso: &Iso,
    net: &PhasedCellNetlist,
    input: &Value,
    in_ty: &Type,
    out_ty: &Type,
) -> Value {
    let data_bits = bits_of(input, in_ty).unwrap();
    let full_input = extend_with_ancilla(&data_bits, net);
    let got_full = eval_cells(net, &full_input);
    let n_data_out = rail_width(out_ty).unwrap();
    let got_bits = &got_full[..n_data_out];

    let mut fuel = 100_000u64;
    let eval_result = theseus_eval::eval(iso, input.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed on input: {e:?}"));
    let expected_bits = bits_of(&eval_result, out_ty).unwrap();
    assert_eq!(
        got_bits,
        &expected_bits[..],
        "{name}: netlist disagrees with eval"
    );
    eval_result
}

// ── Representative test vectors for (Word8, Word8) ops ───────────────

fn word8_pair_samples() -> Vec<(u8, u8)> {
    let mut samples = vec![
        (0, 0),
        (0xFF, 0xFF),
        (0, 0xFF),
        (0xFF, 0),
        (0xAA, 0x55),
        (0x55, 0xAA),
        (1, 0),
        (0, 1),
        (42, 42),
        (0x0F, 0xF0),
        (0x12, 0x34),
        (0x80, 0x01),
        (127, 128),
        (1, 2),
        (0xDE, 0xAD),
        (0xCA, 0xFE),
    ];
    // Add a few more distinct values
    for a in [0u8, 1, 2, 3, 7, 8, 15, 16, 31, 32, 63, 64, 127, 128, 200, 254, 255] {
        for b in [0u8, 1, 127, 128, 255] {
            samples.push((a, b));
        }
    }
    samples.sort();
    samples.dedup();
    samples
}

// ══════════════════════════════════════════════════════════════════════
// Tests: EXCH / SWAPBR / XCHG  (all same shape: swap two Word8s)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn exch_swap_words() {
    let iso = swap_words_iso();
    let net = verify_lowering("EXCH", &iso);
    let (in_ty, out_ty) = type_of(&iso).unwrap();

    for (a, b) in word8_pair_samples() {
        let input = encode_pair(a, b);
        let result = assert_faithful("EXCH", &iso, &net, &input, &in_ty, &out_ty);
        let (ra, rb) = decode_pair(&result);
        assert_eq!((ra, rb), (b, a), "EXCH({a}, {b}) should give ({b}, {a})");
    }

    // Self-inverse check
    for (a, b) in [(0u8, 0), (1, 2), (0xAA, 0x55), (0xFF, 0)] {
        let input = encode_pair(a, b);
        let mut fuel = 100_000u64;
        let once = theseus_eval::eval(&iso, input.clone(), &mut fuel).unwrap();
        let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
        assert_eq!(twice, input, "EXCH is not self-inverse for ({a}, {b})");
    }
}

#[test]
fn swapbr_swap_words() {
    let iso = swap_words_iso();
    let net = verify_lowering("SWAPBR", &iso);
    let (in_ty, out_ty) = type_of(&iso).unwrap();

    for (pc, br) in [(0u8, 0), (0xFF, 0xFF), (0x42, 0x24), (0, 0xFF)] {
        let input = encode_pair(pc, br);
        let result = assert_faithful("SWAPBR", &iso, &net, &input, &in_ty, &out_ty);
        let (rpc, rbr) = decode_pair(&result);
        assert_eq!(
            (rpc, rbr),
            (br, pc),
            "SWAPBR({pc:#x}, {br:#x}) wrong"
        );
    }
}

#[test]
fn xchg_swap_words() {
    let iso = swap_words_iso();
    let net = verify_lowering("XCHG", &iso);
    let (in_ty, out_ty) = type_of(&iso).unwrap();

    for (rd, sx) in [(0u8, 0), (0xFF, 0xFF), (0x12, 0x34), (0, 0xFF)] {
        let input = encode_pair(rd, sx);
        let result = assert_faithful("XCHG", &iso, &net, &input, &in_ty, &out_ty);
        let (rrd, rsx) = decode_pair(&result);
        assert_eq!(
            (rrd, rsx),
            (sx, rd),
            "XCHG({rd:#x}, {sx:#x}) wrong"
        );
        // Self-inverse
        let mut fuel = 100_000u64;
        let twice = theseus_eval::eval(&iso, result, &mut fuel).unwrap();
        assert_eq!(twice, input, "XCHG is not self-inverse");
    }
}

// ══════════════════════════════════════════════════════════════════════
// Tests: XORI rd, imm8
// ══════════════════════════════════════════════════════════════════════

#[test]
fn xori_all_imm_values() {
    let in_ty = word8();
    let out_ty = word8();

    for imm in [0u8, 1, 0x0F, 0xF0, 0x55, 0xAA, 0xFF, 0x80, 0x42] {
        let iso = xori_iso(imm);
        let net = verify_lowering(&format!("XORI(imm={imm:#04x})"), &iso);

        for rd in 0..=255u8 {
            let input = encode_u8(rd);
            let result = assert_faithful(
                &format!("XORI(imm={imm:#04x})"),
                &iso,
                &net,
                &input,
                &in_ty,
                &out_ty,
            );
            let got = decode_u8(&result);
            assert_eq!(got, rd ^ imm, "XORI({rd:#04x}, {imm:#04x}) = {got:#04x}, expected {:#04x}", rd ^ imm);
        }

        // Self-inverse
        for rd in [0u8, 1, 0x55, 0xFF] {
            let input = encode_u8(rd);
            let mut fuel = 100_000u64;
            let once = theseus_eval::eval(&iso, input.clone(), &mut fuel).unwrap();
            let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
            assert_eq!(twice, input, "XORI(imm={imm:#x}) not self-inverse for {rd:#x}");
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// Tests: ROTLI / ROTRI
// ══════════════════════════════════════════════════════════════════════

#[test]
fn rotli_all_shifts() {
    let in_ty = word8();
    let out_ty = word8();

    for k in 0..8u32 {
        let iso = rotl_iso(k as usize);
        let net = verify_lowering(&format!("ROTLI(k={k})"), &iso);

        for rd in 0..=255u8 {
            let input = encode_u8(rd);
            let result = assert_faithful(
                &format!("ROTLI(k={k})"),
                &iso,
                &net,
                &input,
                &in_ty,
                &out_ty,
            );
            let got = decode_u8(&result);
            let expected = rd.rotate_left(k);
            assert_eq!(got, expected, "ROTLI({rd:#04x}, {k}) = {got:#04x}, expected {expected:#04x}");
        }
    }
}

#[test]
fn rotri_all_shifts() {
    let in_ty = word8();
    let out_ty = word8();

    for k in 0..8u32 {
        let iso = rotr_iso(k as usize);
        let net = verify_lowering(&format!("ROTRI(k={k})"), &iso);

        for rd in 0..=255u8 {
            let input = encode_u8(rd);
            let result = assert_faithful(
                &format!("ROTRI(k={k})"),
                &iso,
                &net,
                &input,
                &in_ty,
                &out_ty,
            );
            let got = decode_u8(&result);
            let expected = rd.rotate_right(k);
            assert_eq!(got, expected, "ROTRI({rd:#04x}, {k}) = {got:#04x}, expected {expected:#04x}");
        }
    }
}

#[test]
fn rotli_rotri_adjoint() {
    for k in 0..8usize {
        let rotl = rotl_iso(k);
        let rotr = rotr_iso(k);

        for rd in [0u8, 1, 0x80, 0xFF, 0xAA, 0x55, 42, 137] {
            let input = encode_u8(rd);
            let mut fuel = 100_000u64;

            // rotl then rotr = identity
            let after_rotl = theseus_eval::eval(&rotl, input.clone(), &mut fuel).unwrap();
            let back = theseus_eval::eval(&rotr, after_rotl, &mut fuel).unwrap();
            assert_eq!(back, input, "ROTRI(ROTLI({rd:#x}, {k}), {k}) != identity");

            // rotr then rotl = identity
            let after_rotr = theseus_eval::eval(&rotr, input.clone(), &mut fuel).unwrap();
            let back2 = theseus_eval::eval(&rotl, after_rotr, &mut fuel).unwrap();
            assert_eq!(back2, input, "ROTLI(ROTRI({rd:#x}, {k}), {k}) != identity");
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// Tests: XOR rd, rs
// ══════════════════════════════════════════════════════════════════════

#[test]
fn xor_rd_rs_semantics() {
    let iso = xor_rd_rs_iso();
    let net = verify_lowering("XOR", &iso);
    let (in_ty, out_ty) = type_of(&iso).unwrap();

    for (rd, rs) in word8_pair_samples() {
        let input = encode_pair(rd, rs);
        let result = assert_faithful("XOR", &iso, &net, &input, &in_ty, &out_ty);
        let (got_rd, got_rs) = decode_pair(&result);
        assert_eq!(
            got_rd,
            rd ^ rs,
            "XOR rd: ({rd:#04x} ^ {rs:#04x}) = {got_rd:#04x}, expected {:#04x}",
            rd ^ rs
        );
        assert_eq!(got_rs, rs, "XOR rs: rs={rs:#04x} should be preserved, got {got_rs:#04x}");
    }
}

#[test]
fn xor_rd_rs_self_inverse() {
    let iso = xor_rd_rs_iso();

    for (rd, rs) in [(0u8, 0), (0xFF, 0xFF), (0xAA, 0x55), (1, 2), (42, 0xDE)] {
        let input = encode_pair(rd, rs);
        let mut fuel = 100_000u64;
        let once = theseus_eval::eval(&iso, input.clone(), &mut fuel).unwrap();
        let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
        assert_eq!(twice, input, "XOR not self-inverse for ({rd:#x}, {rs:#x})");
    }
}

#[test]
fn xor_rd_rs_exhaustive() {
    let iso = xor_rd_rs_iso();

    // Full 256×256 domain via core eval only (no netlist per-input — too slow).
    for rd in 0..=255u8 {
        for rs in 0..=255u8 {
            let input = encode_pair(rd, rs);
            let mut fuel = 100_000u64;
            let result = theseus_eval::eval(&iso, input, &mut fuel).unwrap();
            let (got_rd, got_rs) = decode_pair(&result);
            assert_eq!(got_rd, rd ^ rs);
            assert_eq!(got_rs, rs);
        }
    }
}
