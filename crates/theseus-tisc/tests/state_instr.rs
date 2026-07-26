//! Differential oracle tests for TISC-1 State-level instruction isos.
//!
//! For each instruction: build the State-level Iso, lower it, verify
//! cell kind <= K3, then for sampled StateVals check:
//!   (a) faithfulness: balanced-net simulate == core eval
//!   (b) semantics: decode(eval(iso, encode(sample))) matches reference
//!   (c) adjoint checks where applicable

use theseus_core::{type_of, Iso};
use theseus_netlist::lower::lower_iso_with_ancillas;
use theseus_netlist::{balance, bits_of, rail_width, simulate, CellKind};
use theseus_tisc::instr;
use theseus_tisc::types::{StateVals, MEM_WORDS};

const EVAL_FUEL: u64 = 50_000_000;

// ── Lowering + balance harness ─────────────────────────────────────

struct Lowered {
    balanced: theseus_netlist::PhasedCellNetlist,
    in_ty: theseus_core::Type,
    out_ty: theseus_core::Type,
    n_data_out: usize,
    /// Real ancilla init values (from `lower_iso_with_ancillas`) — R1-based
    /// lowering (ADR-0040) needs these seeded correctly, not a fabricated
    /// pattern.
    ancilla_init: Vec<bool>,
}

fn prepare(name: &str, iso: &Iso) -> Lowered {
    let (in_ty, out_ty) = type_of(iso).unwrap_or_else(|e| panic!("{name}: type_of failed: {e:?}"));
    let (net, ancilla_init) =
        lower_iso_with_ancillas(iso).unwrap_or_else(|e| panic!("{name}: lowering failed: {e}"));
    assert_eq!(net.validate(), Ok(()), "{name}: lowered netlist invalid");

    let counts = net.cell_counts();
    let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
    assert!(
        (max_kind <= CellKind::K3 || max_kind == CellKind::R1),
        "{name}: max cell kind {max_kind:?} exceeds K3"
    );
    eprintln!("{name}: cell counts {counts:?}, max kind {max_kind:?}");

    let balanced = balance(&net).unwrap_or_else(|e| panic!("{name}: balance failed: {e:?}"));
    assert_eq!(
        balanced.validate(),
        Ok(()),
        "{name}: balanced netlist invalid"
    );
    assert!(
        balanced.tick_imbalances().unwrap().is_empty(),
        "{name}: balanced netlist has tick imbalances"
    );
    eprintln!("{name}: balanced cell counts {:?}", balanced.cell_counts());

    let n_data_out = rail_width(&out_ty).unwrap();
    Lowered {
        balanced,
        in_ty,
        out_ty,
        n_data_out,
        ancilla_init,
    }
}

fn check_case(name: &str, iso: &Iso, low: &Lowered, sample: &StateVals) {
    let val = sample.encode();
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, val.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));

    // Faithfulness: balanced simulate == core eval
    let expected_bits = bits_of(&ev, &low.out_ty).unwrap();
    let data_bits = bits_of(&val, &low.in_ty).unwrap();

    let mut input_bits = data_bits;
    // Seed ancillas with their REAL init values (ADR-0040 R1 lowering needs
    // correct ancilla inits; the old fabricated `i % 2` pattern corrupts them).
    input_bits.extend_from_slice(&low.ancilla_init);
    assert_eq!(
        input_bits.len(),
        low.balanced.inputs.len(),
        "{name}: data + ancilla inputs must match the balanced net's input count"
    );

    let sim = simulate(&low.balanced, &input_bits)
        .unwrap_or_else(|e| panic!("{name}: simulate failed: {e:?}"));
    assert_eq!(
        &sim.outputs[..low.n_data_out],
        &expected_bits[..],
        "{name}: simulate disagrees with core eval"
    );
}

// ── Sample StateVals ───────────────────────────────────────────────

fn base_state() -> StateVals {
    StateVals {
        regs: [0x10, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76, 0x87],
        pc: 0x00,
        br: 0x00,
        ir: 0x0000,
        scratch: [0x00; 8],
        mem: [0; MEM_WORDS],
    }
}

fn edge_state() -> StateVals {
    StateVals {
        regs: [0x00, 0xFF, 0xAA, 0x55, 0x0F, 0xF0, 0x80, 0x01],
        pc: 0x42,
        br: 0x24,
        ir: 0x0000,
        scratch: [0x00; 8],
        mem: [0; MEM_WORDS],
    }
}

fn mixed_state() -> StateVals {
    StateVals {
        regs: [0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0x12, 0x34],
        pc: 0x80,
        br: 0xFF,
        ir: 0x0000,
        scratch: [0x00; 8],
        mem: [0; MEM_WORDS],
    }
}

fn samples() -> Vec<StateVals> {
    vec![base_state(), edge_state(), mixed_state()]
}

/// Apply a reference transformation to a StateVals clone and compare
/// with the decoded eval result.
fn check_semantics(
    name: &str,
    iso: &Iso,
    sample: &StateVals,
    reference: impl FnOnce(&mut StateVals),
) {
    let val = sample.encode();
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, val, &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: eval failed: {e:?}"));
    let got = StateVals::decode(&ev);

    let mut expected = sample.clone();
    reference(&mut expected);

    assert_eq!(
        got, expected,
        "{name}: semantics mismatch\n  got:      {got:?}\n  expected: {expected:?}"
    );
}

// ══════════════════════════════════════════════════════════════════════
// Tests
// ══════════════════════════════════════════════════════════════════════

#[test]
fn add_state_iso() {
    let rd = 3;
    let rs = 5;
    let iso = instr::add(rd, rs);
    let low = prepare("ADD(3,5)", &iso);

    for s in &samples() {
        check_case("ADD(3,5)", &iso, &low, s);
        check_semantics("ADD(3,5)", &iso, s, |st| {
            st.regs[rd] = st.regs[rd].wrapping_add(st.regs[rs]);
        });
    }
}

#[test]
fn sub_state_iso() {
    let rd = 3;
    let rs = 5;
    let iso = instr::sub(rd, rs);
    let low = prepare("SUB(3,5)", &iso);

    for s in &samples() {
        check_case("SUB(3,5)", &iso, &low, s);
        check_semantics("SUB(3,5)", &iso, s, |st| {
            st.regs[rd] = st.regs[rd].wrapping_sub(st.regs[rs]);
        });
    }
}

#[test]
fn add_sub_adjoint() {
    let rd = 2;
    let rs = 6;
    let add_iso = instr::add(rd, rs);
    let sub_iso = instr::sub(rd, rs);

    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let added = theseus_eval::eval(&add_iso, val.clone(), &mut fuel).unwrap();
        let back = theseus_eval::eval(&sub_iso, added, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&back), *s, "SUB(ADD(x)) != x");
    }
}

#[test]
fn addi_state_iso() {
    let rd = 4;
    let imm = 0x37u8;
    let iso = instr::addi(rd, imm);
    let low = prepare("ADDI(4,0x37)", &iso);

    for s in &samples() {
        check_case("ADDI(4,0x37)", &iso, &low, s);
        check_semantics("ADDI(4,0x37)", &iso, s, |st| {
            st.regs[rd] = st.regs[rd].wrapping_add(imm);
        });
    }
}

#[test]
fn subi_state_iso() {
    let rd = 4;
    let imm = 0x37u8;
    let iso = instr::subi(rd, imm);
    let low = prepare("SUBI(4,0x37)", &iso);

    for s in &samples() {
        check_case("SUBI(4,0x37)", &iso, &low, s);
        check_semantics("SUBI(4,0x37)", &iso, s, |st| {
            st.regs[rd] = st.regs[rd].wrapping_sub(imm);
        });
    }
}

#[test]
fn addi_subi_adjoint() {
    let rd = 1;
    let imm = 0xFFu8;
    let addi_iso = instr::addi(rd, imm);
    let subi_iso = instr::subi(rd, imm);

    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let added = theseus_eval::eval(&addi_iso, val.clone(), &mut fuel).unwrap();
        let back = theseus_eval::eval(&subi_iso, added, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&back), *s, "SUBI(ADDI(x)) != x");
    }
}

#[test]
fn xor_state_iso() {
    let rd = 0;
    let rs = 1;
    let iso = instr::xor(rd, rs);
    let low = prepare("XOR(0,1)", &iso);

    for s in &samples() {
        check_case("XOR(0,1)", &iso, &low, s);
        check_semantics("XOR(0,1)", &iso, s, |st| {
            st.regs[rd] ^= st.regs[rs];
        });
    }

    // Self-inverse
    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let once = theseus_eval::eval(&iso, val.clone(), &mut fuel).unwrap();
        let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&twice), *s, "XOR not self-inverse");
    }
}

#[test]
fn xori_state_iso() {
    let rd = 2;
    let imm = 0xAA;
    let iso = instr::xori(rd, imm);
    let low = prepare("XORI(2,0xAA)", &iso);

    for s in &samples() {
        check_case("XORI(2,0xAA)", &iso, &low, s);
        check_semantics("XORI(2,0xAA)", &iso, s, |st| {
            st.regs[rd] ^= imm;
        });
    }

    // Self-inverse
    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let once = theseus_eval::eval(&iso, val.clone(), &mut fuel).unwrap();
        let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&twice), *s, "XORI not self-inverse");
    }
}

#[test]
fn exch_state_iso() {
    let rd = 1;
    let rs = 6;
    let iso = instr::exch(rd, rs);
    let low = prepare("EXCH(1,6)", &iso);

    for s in &samples() {
        check_case("EXCH(1,6)", &iso, &low, s);
        check_semantics("EXCH(1,6)", &iso, s, |st| {
            st.regs.swap(rd, rs);
        });
    }

    // Self-inverse
    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let once = theseus_eval::eval(&iso, val.clone(), &mut fuel).unwrap();
        let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&twice), *s, "EXCH not self-inverse");
    }
}

#[test]
fn swapbr_state_iso() {
    let rd = 3;
    let iso = instr::swapbr(rd);
    let low = prepare("SWAPBR(r3)", &iso);

    for s in &samples() {
        check_case("SWAPBR(r3)", &iso, &low, s);
        check_semantics("SWAPBR(r3)", &iso, s, |st| {
            std::mem::swap(&mut st.regs[rd], &mut st.br);
        });
    }

    // Self-inverse
    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let once = theseus_eval::eval(&iso, val.clone(), &mut fuel).unwrap();
        let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&twice), *s, "SWAPBR not self-inverse");
    }
}

#[test]
fn xchg_state_iso() {
    let rd = 3;
    let sx = 2;
    let iso = instr::xchg(rd, sx);
    let low = prepare("XCHG(3,s2)", &iso);

    for s in &samples() {
        check_case("XCHG(3,s2)", &iso, &low, s);
        check_semantics("XCHG(3,s2)", &iso, s, |st| {
            std::mem::swap(&mut st.regs[rd], &mut st.scratch[sx]);
        });
    }

    // Self-inverse
    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let once = theseus_eval::eval(&iso, val.clone(), &mut fuel).unwrap();
        let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&twice), *s, "XCHG not self-inverse");
    }
}

#[test]
fn cswap_state_iso() {
    let rd = 0;
    let rs = 2;
    let rt = 4;
    let iso = instr::cswap(rd, rs, rt);
    let low = prepare("CSWAP(0,2,4)", &iso);

    for s in &samples() {
        check_case("CSWAP(0,2,4)", &iso, &low, s);
        check_semantics("CSWAP(0,2,4)", &iso, s, |st| {
            if st.regs[rd] & 1 == 1 {
                st.regs.swap(rs, rt);
            }
        });
    }

    // Self-inverse
    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let once = theseus_eval::eval(&iso, val.clone(), &mut fuel).unwrap();
        let twice = theseus_eval::eval(&iso, once, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&twice), *s, "CSWAP not self-inverse");
    }
}

#[test]
fn rotl_state_iso() {
    let rd = 5;
    let rs = 7;
    let iso = instr::rotl(rd, rs);
    let low = prepare("ROTL(5,7)", &iso);

    for s in &samples() {
        check_case("ROTL(5,7)", &iso, &low, s);
        check_semantics("ROTL(5,7)", &iso, s, |st| {
            st.regs[rd] = st.regs[rd].rotate_left(u32::from(st.regs[rs]) & 7);
        });
    }
}

#[test]
fn rotr_state_iso() {
    let rd = 5;
    let rs = 7;
    let iso = instr::rotr(rd, rs);
    let low = prepare("ROTR(5,7)", &iso);

    for s in &samples() {
        check_case("ROTR(5,7)", &iso, &low, s);
        check_semantics("ROTR(5,7)", &iso, s, |st| {
            st.regs[rd] = st.regs[rd].rotate_right(u32::from(st.regs[rs]) & 7);
        });
    }
}

#[test]
fn rotl_rotr_adjoint() {
    let rd = 4;
    let rs = 3;
    let rotl_iso = instr::rotl(rd, rs);
    let rotr_iso = instr::rotr(rd, rs);

    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let rotated = theseus_eval::eval(&rotl_iso, val.clone(), &mut fuel).unwrap();
        let back = theseus_eval::eval(&rotr_iso, rotated, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&back), *s, "ROTR(ROTL(x)) != x");
    }
}

#[test]
fn rotli_state_iso() {
    let rd = 6;
    let imm = 3u8;
    let iso = instr::rotli(rd, imm);
    let low = prepare("ROTLI(6,3)", &iso);

    for s in &samples() {
        check_case("ROTLI(6,3)", &iso, &low, s);
        check_semantics("ROTLI(6,3)", &iso, s, |st| {
            st.regs[rd] = st.regs[rd].rotate_left(u32::from(imm));
        });
    }
}

#[test]
fn rotri_state_iso() {
    let rd = 6;
    let imm = 3u8;
    let iso = instr::rotri(rd, imm);
    let low = prepare("ROTRI(6,3)", &iso);

    for s in &samples() {
        check_case("ROTRI(6,3)", &iso, &low, s);
        check_semantics("ROTRI(6,3)", &iso, s, |st| {
            st.regs[rd] = st.regs[rd].rotate_right(u32::from(imm));
        });
    }
}

#[test]
fn rotli_rotri_adjoint() {
    let rd = 0;
    let imm = 5u8;
    let rotli_iso = instr::rotli(rd, imm);
    let rotri_iso = instr::rotri(rd, imm);

    for s in &samples() {
        let val = s.encode();
        let mut fuel = EVAL_FUEL;
        let rotated = theseus_eval::eval(&rotli_iso, val.clone(), &mut fuel).unwrap();
        let back = theseus_eval::eval(&rotri_iso, rotated, &mut fuel).unwrap();
        assert_eq!(StateVals::decode(&back), *s, "ROTRI(ROTLI(x)) != x");
    }
}
