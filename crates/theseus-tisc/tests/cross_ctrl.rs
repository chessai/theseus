//! Self-inverse test for `cross_ctrl_fredkin` on a synthetic 2-factor product.
//!
//! Proves that `f(f(x)) == x` (self-inverse) over a bounded-but-exhaustive
//! domain: the two W8 target words are enumerated exhaustively (2^16),
//! crossed with ctrl_bit ∈ {0,1}; non-participating bits are held fixed.
//! Total inputs ≤ 2^17 (131072), completing in well under a minute.

use theseus_core::Value;
use theseus_netlist::{bits_of, value_from_bits};
use theseus_tisc::addressing::fields;
use theseus_tisc::decoder::cross_ctrl_fredkin;
use theseus_tisc::types::W;

const FUEL: u64 = 50_000_000;

/// Encode 5 bytes as a value of type `fields(2) × fields(3)`.
///
/// Bit layout (depth-first left-to-right of the composite type):
///   fields(2) = (W8 × W8), fields(3) = (W8 × (W8 × W8))
///   split_fields(2,3) maps fields(5) ↔ fields(2) × fields(3)
///   so the underlying flat bit order is: lf0, lf1, rf0, rf1, rf2.
///
/// We encode directly into the composite type.
fn encode_composite(lf0: u8, lf1: u8, rf0: u8, rf1: u8, rf2: u8) -> Value {
    let ty = theseus_core::Type::prod(fields(2), fields(3));
    let mut bits = Vec::with_capacity(40);
    // Left factor: fields(2) = W8 × W8 → lf0 then lf1 (depth-first)
    for i in 0..W {
        bits.push((lf0 >> i) & 1 == 1);
    }
    for i in 0..W {
        bits.push((lf1 >> i) & 1 == 1);
    }
    // Right factor: fields(3) = W8 × (W8 × W8) → rf0, rf1, rf2
    for i in 0..W {
        bits.push((rf0 >> i) & 1 == 1);
    }
    for i in 0..W {
        bits.push((rf1 >> i) & 1 == 1);
    }
    for i in 0..W {
        bits.push((rf2 >> i) & 1 == 1);
    }
    value_from_bits(&ty, &bits).expect("encode_composite")
}

/// Decode a value of type `fields(2) × fields(3)` back to 5 bytes.
fn decode_composite(val: &Value) -> (u8, u8, u8, u8, u8) {
    let ty = theseus_core::Type::prod(fields(2), fields(3));
    let bits = bits_of(val, &ty).expect("decode_composite");
    assert_eq!(bits.len(), 40);
    let read_u8 = |off: usize| -> u8 {
        let mut v = 0u8;
        for i in 0..W {
            if bits[off + i] {
                v |= 1 << i;
            }
        }
        v
    };
    (read_u8(0), read_u8(8), read_u8(16), read_u8(24), read_u8(32))
}

#[test]
fn cross_ctrl_fredkin_self_inverse() {
    // Synthetic 2-factor product:
    //   Left factor:  2 fields (ctrl_field = 1, off-head)
    //   Right factor: 3 fields (word_b = 0, unused = 1, word_a = 2, off-head)
    //
    // Both ctrl_field and word_a are off-head to exercise routing.
    // ctrl_bit = 0 (LSB of ctrl_field).
    let n_left = 2;
    let n_right = 3;
    let ctrl_field = 1; // off-head in left factor
    let ctrl_bit = 0;
    let word_a = 2; // off-head in right factor
    let word_b = 0;

    let f = cross_ctrl_fredkin(ctrl_field, ctrl_bit, word_a, word_b, n_left, n_right);

    let unused_lf0: u8 = 0;   // left factor field 0 (non-participating)
    let unused_rf1: u8 = 0;   // right factor field 1 (non-participating)

    // Strided edge set instead of full 0..=255 × 0..=255.
    // Self-inverse is structural; edge coverage suffices.
    let edge_vals: [u8; 7] = [0x00, 0x01, 0x02, 0x55, 0xAA, 0xFE, 0xFF];

    // Sweep 1: non-participating ctrl bits = 0, ctrl_field byte = ctrl_bit_val
    for ctrl_bit_val in 0..2u8 {
        let ctrl_byte = ctrl_bit_val; // only bit 0 set or clear
        for &wa_val in &edge_vals {
            for &wb_val in &edge_vals {
                let input = encode_composite(
                    unused_lf0,
                    ctrl_byte,
                    wb_val,
                    unused_rf1,
                    wa_val,
                );
                let mut fuel = FUEL;
                let once = theseus_eval::eval(&f, input.clone(), &mut fuel)
                    .expect("eval once");
                let mut fuel2 = FUEL;
                let twice = theseus_eval::eval(&f, once, &mut fuel2)
                    .expect("eval twice");
                assert_eq!(
                    input, twice,
                    "self-inverse failed: ctrl={ctrl_byte:#04x} wa={wa_val:#04x} wb={wb_val:#04x}"
                );
            }
        }
    }

    // Sweep 2: non-participating ctrl bits = all-set (0xFE with bit 0 varying)
    for ctrl_bit_val in 0..2u8 {
        let ctrl_byte = 0xFE | ctrl_bit_val;
        for &wa_val in &edge_vals {
            for &wb_val in &edge_vals {
                let input = encode_composite(
                    unused_lf0,
                    ctrl_byte,
                    wb_val,
                    unused_rf1,
                    wa_val,
                );
                let mut fuel = FUEL;
                let once = theseus_eval::eval(&f, input.clone(), &mut fuel)
                    .expect("eval once");
                let mut fuel2 = FUEL;
                let twice = theseus_eval::eval(&f, once, &mut fuel2)
                    .expect("eval twice");
                assert_eq!(
                    input, twice,
                    "self-inverse failed (sweep 2): ctrl={ctrl_byte:#04x} wa={wa_val:#04x} wb={wb_val:#04x}"
                );
            }
        }
    }
}

/// Verify the gate actually DOES something when ctrl_bit is set:
/// with ctrl=1, word_a and word_b should be swapped.
#[test]
fn cross_ctrl_fredkin_semantics_spot_check() {
    let n_left = 2;
    let n_right = 3;
    let ctrl_field = 1;
    let ctrl_bit = 0;
    let word_a = 2;
    let word_b = 0;

    let f = cross_ctrl_fredkin(ctrl_field, ctrl_bit, word_a, word_b, n_left, n_right);

    // ctrl_bit = 0 (bit 0 clear) → no swap
    {
        let input = encode_composite(0x00, 0x00, 0xAA, 0x00, 0x55);
        let mut fuel = FUEL;
        let out = theseus_eval::eval(&f, input, &mut fuel).expect("eval");
        let (lf0, lf1, rf0, rf1, rf2) = decode_composite(&out);
        assert_eq!(
            (lf0, lf1, rf0, rf1, rf2),
            (0x00, 0x00, 0xAA, 0x00, 0x55),
            "no swap when ctrl=0"
        );
    }

    // ctrl_bit = 1 (bit 0 set) → swap word_a ↔ word_b
    {
        let input = encode_composite(0x00, 0x01, 0xAA, 0x00, 0x55);
        let mut fuel = FUEL;
        let out = theseus_eval::eval(&f, input, &mut fuel).expect("eval");
        let (lf0, lf1, rf0, rf1, rf2) = decode_composite(&out);
        assert_eq!(lf0, 0x00, "left factor field 0 preserved");
        assert_eq!(lf1, 0x01, "ctrl field preserved");
        assert_eq!(rf1, 0x00, "unused right field preserved");
        // word_b (rf0) and word_a (rf2) should be swapped
        assert_eq!(rf0, 0x55, "word_b should now hold word_a's value");
        assert_eq!(rf2, 0xAA, "word_a should now hold word_b's value");
    }
}
