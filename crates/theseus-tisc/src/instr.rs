//! TISC-1 instruction isos: State <-> State.
//!
//! Each public function returns an `Iso : state_ty() <-> state_ty()`
//! parameterized by register/scratch indices and immediates.
//!
//! The operand-level constructions are lifted from the verified test files
//! (tisc_struct.rs, tisc_controlled.rs, tisc_arith.rs) and wrapped with
//! the addressing layer to produce State-level isos.

use theseus_core::{Iso, Type};
use theseus_netlist::{prim_from_bits, CellKind};
use crate::addressing::{at_fields, at_reg1, at_reg2, at_reg3};
use crate::types::{bit, flat, word8, reg_field, scratch_field, PC_FIELD, BR_FIELD, W};

// ══════════════════════════════════════════════════════════════════════
// Operand-level iso builders (lifted from verified test files)
// ══════════════════════════════════════════════════════════════════════

// ── Structural helpers ─────────────────────────────────────────────

/// NOT on a single Bit = SwapSum(1,1) — zero cells, pure rail swap.
fn not_iso() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// Number of Bit leaves in a product tree over Bit.
fn leaf_count(ty: &Type) -> usize {
    match ty {
        Type::Prod(a, b) => leaf_count(a) + leaf_count(b),
        _ => 1,
    }
}

/// `flat(a) x flat(b) <-> flat(a+b)`, pure assoc plumbing.
fn append(a: usize, b: usize) -> Iso {
    assert!(a >= 1 && b >= 1);
    if a == 1 {
        Iso::Id(flat(1 + b))
    } else {
        Iso::seq(
            Iso::AssocRProd(bit(), flat(a - 1), flat(b)),
            Iso::times(Iso::Id(bit()), append(a - 1, b)),
        )
    }
}

/// `ty <-> flat(leaf_count(ty))`, leaves in depth-first order.
fn flatten(ty: &Type) -> Iso {
    match ty {
        Type::Prod(a, b) => {
            let (na, nb) = (leaf_count(a), leaf_count(b));
            Iso::seq(Iso::times(flatten(a), flatten(b)), append(na, nb))
        }
        _ => Iso::Id(ty.clone()),
    }
}

/// Sequential composition; empty = identity.
fn seq_all(n: usize, isos: Vec<Iso>) -> Iso {
    isos.into_iter()
        .reduce(Iso::seq)
        .unwrap_or_else(|| Iso::Id(flat(n)))
}

/// Swap adjacent leaves `k` and `k+1` of `flat(n)`.
fn swap_adj(n: usize, k: usize) -> Iso {
    assert!(k + 1 < n);
    if k > 0 {
        return Iso::times(Iso::Id(bit()), swap_adj(n - 1, k - 1));
    }
    if n == 2 {
        Iso::SwapProd(bit(), bit())
    } else {
        let r = flat(n - 2);
        Iso::seq(
            Iso::seq(
                Iso::AssocLProd(bit(), bit(), r.clone()),
                Iso::times(Iso::SwapProd(bit(), bit()), Iso::Id(r.clone())),
            ),
            Iso::AssocRProd(bit(), bit(), r),
        )
    }
}

/// Apply a 1-wire iso at leaf `k` of `flat(n)`.
fn at_leaf(n: usize, k: usize, iso1: Iso) -> Iso {
    assert!(k < n);
    if k > 0 {
        Iso::times(Iso::Id(bit()), at_leaf(n - 1, k - 1, iso1))
    } else if n == 1 {
        iso1
    } else {
        Iso::times(iso1, Iso::Id(flat(n - 1)))
    }
}

// ── Structural: swap two Word8s ────────────────────────────────────

/// Swap two Word8 values: `(Word8, Word8) <-> (Word8, Word8)`.
/// Used for EXCH, SWAPBR, XCHG. Self-inverse; 0 cells.
fn swap_words_iso() -> Iso {
    Iso::SwapProd(word8(), word8())
}

// ── XORI operand iso ───────────────────────────────────────────────

/// XORI: XOR a Word8 with a compile-time constant.
/// Per-bit NOT where imm bit is 1, Id where 0. 0 cells.
fn xori_operand(imm: u8) -> Iso {
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

// ── ROTLI/ROTRI operand isos ───────────────────────────────────────

/// Bubble element at position `from` leftward to position `to` in flat(n).
fn bubble_left(n: usize, from: usize, to: usize) -> Iso {
    assert!(from > to);
    let mut iso = swap_adj(n, from - 1);
    for pos in (to..from - 1).rev() {
        iso = Iso::seq(iso, swap_adj(n, pos));
    }
    iso
}

/// ROTLI: rotate a Word8 left by `k` bit positions. 0 cells.
fn rotli_operand(k: usize) -> Iso {
    let k = k % 8;
    if k == 0 {
        return Iso::Id(word8());
    }
    let rotate_once = bubble_left(8, 7, 0);
    let mut iso = rotate_once.clone();
    for _ in 1..k {
        iso = Iso::seq(iso, rotate_once.clone());
    }
    iso
}

/// ROTRI: rotate a Word8 right by `k` = adjoint of ROTLI. 0 cells.
fn rotri_operand(k: usize) -> Iso {
    let k = k % 8;
    if k == 0 {
        return Iso::Id(word8());
    }
    Iso::sym(rotli_operand(k))
}

// ── XOR operand iso (interleave-CNOT-deinterleave) ─────────────────

/// (A x B) x (C x D) -> (A x C) x (B x D).
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

/// Zip two n-element bit products.
fn zip_products(n: usize) -> Iso {
    let b = bit();
    if n == 1 {
        return Iso::Id(Type::prod(b.clone(), b));
    }
    let rest_ty = flat(n - 1);
    let pair_ty = Type::prod(b.clone(), b.clone());
    let extract = extract_first_pair(b.clone(), rest_ty.clone(), b.clone(), rest_ty.clone());
    let zip_rest = zip_products(n - 1);
    Iso::seq(extract, Iso::times(Iso::Id(pair_ty), zip_rest))
}

/// CNOT prim: (target, ctrl) -> (target XOR ctrl, ctrl). K1.
fn cnot_prim() -> Iso {
    let b = bit();
    let ty = Type::prod(b.clone(), b);
    let def = prim_from_bits("cnot_tc", ty.clone(), ty, |bits| {
        vec![bits[0] ^ bits[1], bits[1]]
    })
    .unwrap();
    Iso::Prim(Box::new(def))
}

/// XOR rd, rs: rd <- rd XOR rs, rs preserved. Uses K1 cells.
fn xor_operand() -> Iso {
    let interleave = zip_products(8);
    let cnot = cnot_prim();
    let mut cnots = cnot.clone();
    for _ in 1..7 {
        cnots = Iso::times(cnot.clone(), cnots);
    }
    cnots = Iso::times(cnot, cnots);
    let deinterleave = Iso::sym(interleave.clone());
    Iso::seq(interleave, Iso::seq(cnots, deinterleave))
}

// ── Fredkin-based controlled isos (CSWAP, ROTL, ROTR) ──────────────

/// The 3-wide Fredkin prim. K1.
fn fredkin_prim() -> Iso {
    let t3 = flat(3);
    Iso::Prim(Box::new(
        prim_from_bits("fredkin", t3.clone(), t3, |b| CellKind::K1.eval(b))
            .expect("fredkin prim"),
    ))
}

/// Apply Fredkin prim to first three leaves of flat(n).
fn fredkin_head(n: usize) -> Iso {
    assert!(n >= 3);
    if n == 3 {
        return fredkin_prim();
    }
    let r = flat(n - 3);
    let reshape = Iso::seq(
        Iso::times(Iso::Id(bit()), Iso::AssocLProd(bit(), bit(), r.clone())),
        Iso::AssocLProd(bit(), Type::prod(bit(), bit()), r.clone()),
    );
    Iso::seq(
        Iso::seq(reshape.clone(), Iso::times(fredkin_prim(), Iso::Id(r))),
        Iso::sym(reshape),
    )
}

/// Fredkin on leaves (c, a, b) of flat(n), with bubble routing.
fn fredkin_at(n: usize, c: usize, a: usize, b: usize) -> Iso {
    assert!(c != a && c != b && a != b);
    let mut arr: Vec<usize> = (0..n).collect();
    let mut swaps: Vec<Iso> = Vec::new();
    for (slot, orig) in [(0usize, c), (1, a), (2, b)] {
        let p = arr.iter().position(|&x| x == orig).expect("leaf present");
        assert!(p >= slot, "bubble-routing invariant");
        for k in (slot..p).rev() {
            swaps.push(swap_adj(n, k));
            arr.swap(k, k + 1);
        }
    }
    let route = seq_all(n, swaps);
    Iso::seq(Iso::seq(route.clone(), fredkin_head(n)), Iso::sym(route))
}

/// CSWAP operand iso: (rd, (rs, rt)) <-> same.
/// Flat: bits 0..8 = rd, 8..16 = rs, 16..24 = rt.
fn cswap_operand() -> Iso {
    let n = 3 * W;
    let cascade: Vec<Iso> = (0..W).map(|i| fredkin_at(n, 0, W + i, 2 * W + i)).collect();
    let cswap_ty = Type::prod(word8(), Type::prod(word8(), word8()));
    let fl = flatten(&cswap_ty);
    Iso::seq(Iso::seq(fl.clone(), seq_all(n, cascade)), Iso::sym(fl))
}

/// Transpositions for fixed left-rotation-by-s of 8 rd wire slots.
fn rot_transpositions(s: usize) -> Vec<(usize, usize)> {
    let mut arr: Vec<usize> = (0..W).collect();
    let mut out = Vec::new();
    for j in 0..W {
        let want = (j + W - s) % W;
        let p = arr.iter().position(|&x| x == want).expect("bit present");
        if p != j {
            out.push((j, p));
            arr.swap(j, p);
        }
    }
    out
}

/// ROTL operand iso: (rd, rs) <-> same; rd <- rd rotl (rs mod 8).
fn rotl_operand() -> Iso {
    let n = 2 * W;
    let mut cascade: Vec<Iso> = Vec::new();
    for k in 0..3 {
        let ctrl = W + k;
        for (i, j) in rot_transpositions(1 << k) {
            cascade.push(fredkin_at(n, ctrl, i, j));
        }
    }
    let rot_ty = Type::prod(word8(), word8());
    let fl = flatten(&rot_ty);
    Iso::seq(Iso::seq(fl.clone(), seq_all(n, cascade)), Iso::sym(fl))
}

/// ROTR operand iso: adjoint of ROTL.
fn rotr_operand() -> Iso {
    Iso::sym(rotl_operand())
}

// ── Arithmetic (Takahashi-Kunihiro ripple) ─────────────────────────

#[derive(Clone, Copy)]
enum Gate {
    Cnot { c: usize, t: usize },
    Tof { c1: usize, c2: usize, t: usize },
}

/// The 46-gate zero-ancilla in-place adder on flat(16).
fn tk_add_gates() -> Vec<Gate> {
    let a = |i: usize| W + i;
    let b = |i: usize| i;
    let mut g: Vec<Gate> = Vec::new();
    for i in 1..W - 1 { g.push(Gate::Cnot { c: a(i), t: b(i) }); }
    for i in (1..W - 1).rev() { g.push(Gate::Cnot { c: a(i), t: a(i + 1) }); }
    for i in 0..W - 1 { g.push(Gate::Tof { c1: a(i), c2: b(i), t: a(i + 1) }); }
    g.push(Gate::Cnot { c: a(W - 1), t: b(W - 1) });
    g.push(Gate::Tof { c1: a(W - 2), c2: b(W - 2), t: a(W - 1) });
    for i in (1..W - 1).rev() {
        g.push(Gate::Cnot { c: a(i), t: b(i) });
        g.push(Gate::Tof { c1: a(i - 1), c2: b(i - 1), t: a(i) });
    }
    for i in 1..W - 1 { g.push(Gate::Cnot { c: a(i), t: a(i + 1) }); }
    for i in 0..W - 1 { g.push(Gate::Cnot { c: a(i), t: b(i) }); }
    g
}

/// 2-wire CNOT prim for arithmetic: (c, t) -> (c, t ^ c). K1.
fn cnot_ar_prim() -> Iso {
    let t2 = flat(2);
    let def = prim_from_bits("cnot_ar", t2.clone(), t2, |b| vec![b[0], b[1] ^ b[0]])
        .expect("cnot prim");
    Iso::Prim(Box::new(def))
}

/// 3-wire Toffoli prim: (c1, c2, t) -> (c1, c2, t ^ (c1 & c2)). K2.
fn tof_prim() -> Iso {
    let t3 = flat(3);
    let def = prim_from_bits("tof_ar", t3.clone(), t3, |b| {
        vec![b[0], b[1], b[2] ^ (b[0] & b[1])]
    })
    .expect("tof prim");
    Iso::Prim(Box::new(def))
}

/// Apply a flat(2) prim at the head of flat(n).
fn head2(n: usize, p: Iso) -> Iso {
    assert!(n >= 2);
    if n == 2 { return p; }
    let r = flat(n - 2);
    let reshape = Iso::AssocLProd(bit(), bit(), r.clone());
    Iso::seq(
        Iso::seq(reshape.clone(), Iso::times(p, Iso::Id(r))),
        Iso::sym(reshape),
    )
}

/// Apply a flat(3) prim at the head of flat(n).
fn head3(n: usize, p: Iso) -> Iso {
    assert!(n >= 3);
    if n == 3 { return p; }
    let r = flat(n - 3);
    let reshape = Iso::seq(
        Iso::times(Iso::Id(bit()), Iso::AssocLProd(bit(), bit(), r.clone())),
        Iso::AssocLProd(bit(), Type::prod(bit(), bit()), r.clone()),
    );
    Iso::seq(
        Iso::seq(reshape.clone(), Iso::times(p, Iso::Id(r))),
        Iso::sym(reshape),
    )
}

/// Apply a 2- or 3-wire prim at given leaves of flat(n).
fn gate_at(n: usize, wires: &[usize], prim: Iso) -> Iso {
    let w = wires.len();
    assert!(w == 2 || w == 3);
    let mut arr: Vec<usize> = (0..n).collect();
    let mut swaps: Vec<Iso> = Vec::new();
    for (slot, &orig) in wires.iter().enumerate() {
        let p = arr.iter().position(|&x| x == orig).expect("leaf present");
        assert!(p >= slot, "bubble-routing invariant");
        for k in (slot..p).rev() {
            swaps.push(swap_adj(n, k));
            arr.swap(k, k + 1);
        }
    }
    let route = seq_all(n, swaps);
    let headed = if w == 2 { head2(n, prim) } else { head3(n, prim) };
    Iso::seq(Iso::seq(route.clone(), headed), Iso::sym(route))
}

/// Compose a gate list into an iso on flat(n).
fn gates_iso(n: usize, gates: &[Gate]) -> Iso {
    let isos: Vec<Iso> = gates
        .iter()
        .map(|g| match *g {
            Gate::Cnot { c, t } => gate_at(n, &[c, t], cnot_ar_prim()),
            Gate::Tof { c1, c2, t } => gate_at(n, &[c1, c2, t], tof_prim()),
        })
        .collect();
    seq_all(n, isos)
}

/// ADD operand iso: (Word8, Word8) <-> (Word8, Word8); rd <- rd + rs.
fn add_operand() -> Iso {
    let pair_ty = Type::prod(word8(), word8());
    let fl = flatten(&pair_ty);
    Iso::seq(
        Iso::seq(fl.clone(), gates_iso(2 * W, &tk_add_gates())),
        Iso::sym(fl),
    )
}

/// SUB operand iso: adjoint of ADD.
fn sub_operand() -> Iso {
    Iso::sym(add_operand())
}

/// Immediate mask: XOR aux word (flat wires 8..16) with constant imm.
fn imm_mask_iso(imm: u8) -> Iso {
    let n = 2 * W;
    let nots: Vec<Iso> = (0..W)
        .filter(|i| (imm >> i) & 1 == 1)
        .map(|i| at_leaf(n, W + i, not_iso()))
        .collect();
    seq_all(n, nots)
}

/// ADDI operand iso: (Word8, Word8) <-> same; rd <- rd + imm8.
/// The second Word8 is an aux word (0 on input, 0 on output).
fn addi_operand(imm: u8) -> Iso {
    let pair_ty = Type::prod(word8(), word8());
    let fl = flatten(&pair_ty);
    let mask = imm_mask_iso(imm);
    let body = Iso::seq(
        Iso::seq(mask.clone(), gates_iso(2 * W, &tk_add_gates())),
        mask,
    );
    Iso::seq(Iso::seq(fl.clone(), body), Iso::sym(fl))
}

/// SUBI operand iso: adjoint of ADDI.
fn subi_operand(imm: u8) -> Iso {
    Iso::sym(addi_operand(imm))
}

// ══════════════════════════════════════════════════════════════════════
// Public State-level instruction isos
// ══════════════════════════════════════════════════════════════════════

/// `ADD rd, rs` : `rd <- rd + rs (mod 256)`, rs preserved.
pub fn add(rd: usize, rs: usize) -> Iso {
    at_reg2(add_operand(), reg_field(rd), reg_field(rs))
}

/// `SUB rd, rs` : `rd <- rd - rs (mod 256)`, rs preserved.
pub fn sub(rd: usize, rs: usize) -> Iso {
    at_reg2(sub_operand(), reg_field(rd), reg_field(rs))
}

/// `ADDI rd, imm8` : `rd <- rd + imm8 (mod 256)`.
///
/// Uses a scratch word (s0 by default, index 12) as the aux zero word
/// required by the parity obstruction. The scratch word must be zero on
/// entry and is restored to zero on exit.
pub fn addi(rd: usize, imm: u8) -> Iso {
    // Use scratch field 0 as the aux word
    at_fields(addi_operand(imm), &[reg_field(rd), scratch_field(0)])
}

/// `SUBI rd, imm8` : `rd <- rd - imm8 (mod 256)`.
pub fn subi(rd: usize, imm: u8) -> Iso {
    at_fields(subi_operand(imm), &[reg_field(rd), scratch_field(0)])
}

/// `XOR rd, rs` : `rd <- rd XOR rs`, rs preserved.
pub fn xor(rd: usize, rs: usize) -> Iso {
    at_reg2(xor_operand(), reg_field(rd), reg_field(rs))
}

/// `XORI rd, imm8` : `rd <- rd XOR imm8`.
pub fn xori(rd: usize, imm: u8) -> Iso {
    at_reg1(xori_operand(imm), reg_field(rd))
}

/// `EXCH rd, rs` : swap `rd` and `rs`.
pub fn exch(rd: usize, rs: usize) -> Iso {
    at_reg2(swap_words_iso(), reg_field(rd), reg_field(rs))
}

/// `SWAPBR` : swap PC and BR.
pub fn swapbr() -> Iso {
    at_fields(swap_words_iso(), &[PC_FIELD, BR_FIELD])
}

/// `XCHG rd, sx` : swap register `rd` with scratch word `sx`.
pub fn xchg(rd: usize, sx: usize) -> Iso {
    at_fields(swap_words_iso(), &[reg_field(rd), scratch_field(sx)])
}

/// `CSWAP rd, rs, rt` : if `rd[0]` then swap `rs` and `rt`, else identity.
pub fn cswap(rd: usize, rs: usize, rt: usize) -> Iso {
    at_reg3(cswap_operand(), reg_field(rd), reg_field(rs), reg_field(rt))
}

/// `ROTL rd, rs` : `rd <- rd rotl (rs mod 8)`, rs preserved.
pub fn rotl(rd: usize, rs: usize) -> Iso {
    at_reg2(rotl_operand(), reg_field(rd), reg_field(rs))
}

/// `ROTR rd, rs` : `rd <- rd rotr (rs mod 8)`, rs preserved.
pub fn rotr(rd: usize, rs: usize) -> Iso {
    at_reg2(rotr_operand(), reg_field(rd), reg_field(rs))
}

/// `ROTLI rd, imm8` : `rd <- rd rotl (imm8 mod 8)`.
pub fn rotli(rd: usize, imm: u8) -> Iso {
    at_reg1(rotli_operand(imm as usize), reg_field(rd))
}

/// `ROTRI rd, imm8` : `rd <- rd rotr (imm8 mod 8)`.
pub fn rotri(rd: usize, imm: u8) -> Iso {
    at_reg1(rotri_operand(imm as usize), reg_field(rd))
}
