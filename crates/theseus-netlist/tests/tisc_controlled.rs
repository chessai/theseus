//! TISC-1 controlled datapath instructions as reversible Core Pi isos,
//! oracle-verified (differential against `theseus_eval::eval`).
//!
//! Instructions (docs/src/design/tisc-1-spec.md section 2, W = 8, mod 256):
//!
//! * `CSWAP rd, rs, rt` — if `rd[0]` then swap the whole 8-bit `rs`/`rt`
//!   registers, else identity; `rd` preserved; self-inverse.
//! * `ROTL rd, rs` — `rd <- rd rotl (rs mod 8)`, `rs` preserved.
//! * `ROTR rd, rs` — `rd <- rd rotr (rs mod 8)`, `rs` preserved;
//!   ROTR is the adjoint of ROTL.
//!
//! Construction: everything is a CASCADE of Fredkin (K1) cells. A K1
//! cell passes its control through unchanged — `(c, a, b) -> (c,
//! c?swap)` — so one control bit drives many controlled swaps by being
//! threaded through cell after cell, with no fanout/copy primitive:
//!
//! * CSWAP: 8 Fredkins, all controlled by `rd[0]`; Fredkin `i` swaps
//!   `rs[i]`/`rt[i]`. The control emerges from Fredkin `i` and feeds
//!   Fredkin `i+1`.
//! * ROTL: a 3-stage barrel rotator. Stage `k` (k = 0, 1, 2)
//!   conditionally rotates `rd` left by `2^k` iff `rs[k]`, as a cascade
//!   of Fredkins threading `rs[k]`: the fixed rotation-by-`2^k` wire
//!   permutation is decomposed into transpositions, and each
//!   transposition becomes one Fredkin swapping those two `rd` bits.
//!   Composing the stages rotates by `rs[0]*1 + rs[1]*2 + rs[2]*4 =
//!   rs mod 8`. `rs[3..8]` are untouched (only `rs mod 8` matters).
//! * ROTR = `Iso::sym(ROTL)` (also cross-checked against the syntactic
//!   `adjoint`).
//!
//! Routing (getting the control + the two data bits adjacent to apply
//! the 3-wide Fredkin prim) is pure product-structure plumbing —
//! swap/assoc isos, which lower to zero-cell rewiring — built as
//! `route ; fredkin-at-head ; sym(route)`.

use theseus_core::{adjoint, type_of, Iso, Type, Value};
use theseus_netlist::{
    balance, bits_of, lower_iso, prim_from_bits, rail_width, simulate, value_from_bits, CellKind,
    PhasedCellNetlist, RailPair,
};

const W: usize = 8;
const EVAL_FUEL: u64 = 50_000_000;

// ── Types: Bit, flat bit products, Word8 ────────────────────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

/// Right-nested product of `n` Bits: `B x (B x (... x B))`, index 0 = LSB.
fn flat(n: usize) -> Type {
    assert!(n >= 1);
    if n == 1 {
        bit()
    } else {
        Type::prod(bit(), flat(n - 1))
    }
}

fn word8() -> Type {
    flat(W)
}

/// CSWAP operand tuple: (rd, (rs, rt)).
fn cswap_ty() -> Type {
    Type::prod(word8(), Type::prod(word8(), word8()))
}

/// ROTL/ROTR operand tuple: (rd, rs).
fn rot_ty() -> Type {
    Type::prod(word8(), word8())
}

// ── encode / decode (LSB-first) ─────────────────────────────────────

fn bytes_to_bits(bytes: &[u8]) -> Vec<bool> {
    bytes
        .iter()
        .flat_map(|&x| (0..W).map(move |i| (x >> i) & 1 == 1))
        .collect()
}

fn bits_to_bytes(bits: &[bool]) -> Vec<u8> {
    assert_eq!(bits.len() % W, 0);
    bits.chunks(W)
        .map(|c| {
            c.iter()
                .enumerate()
                .fold(0u8, |acc, (i, &b)| acc | (u8::from(b) << i))
        })
        .collect()
}

/// Encode a byte tuple as a Value of `ty` (a product tree of Word8s).
fn encode(ty: &Type, bytes: &[u8]) -> Value {
    value_from_bits(ty, &bytes_to_bits(bytes)).expect("encode")
}

/// Decode a Value of `ty` back to its byte tuple, LSB-first per word.
fn decode(ty: &Type, v: &Value) -> Vec<u8> {
    bits_to_bytes(&bits_of(v, ty).expect("decode"))
}

// ── Structural plumbing on flat bit products ────────────────────────

/// Sequential composition of `isos` on `flat(n)`; empty list = identity.
fn seq_all(n: usize, isos: Vec<Iso>) -> Iso {
    isos.into_iter()
        .reduce(Iso::seq)
        .unwrap_or_else(|| Iso::Id(flat(n)))
}

/// Swap adjacent leaves `k` and `k+1` of `flat(n)` (zero cells: lowers
/// to a rail permutation).
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

/// Number of Bit leaves in a product tree over Bit.
fn leaf_count(ty: &Type) -> usize {
    match ty {
        Type::Prod(a, b) => leaf_count(a) + leaf_count(b),
        _ => 1,
    }
}

/// `flat(a) x flat(b) <-> flat(a+b)` (append), pure assoc plumbing.
fn append(a: usize, b: usize) -> Iso {
    assert!(a >= 1 && b >= 1);
    if a == 1 {
        // (Bit x flat(b)) is literally flat(1+b).
        Iso::Id(flat(1 + b))
    } else {
        Iso::seq(
            Iso::AssocRProd(bit(), flat(a - 1), flat(b)),
            Iso::times(Iso::Id(bit()), append(a - 1, b)),
        )
    }
}

/// `ty <-> flat(leaf_count(ty))`, leaves kept in depth-first order.
fn flatten(ty: &Type) -> Iso {
    match ty {
        Type::Prod(a, b) => {
            let (na, nb) = (leaf_count(a), leaf_count(b));
            Iso::seq(Iso::times(flatten(a), flatten(b)), append(na, nb))
        }
        _ => Iso::Id(ty.clone()),
    }
}

// ── The Fredkin prim + placement at arbitrary positions ─────────────

/// The 3-wide Fredkin prim `(c, a, b) -> (c, c?b:a, c?a:b)`. Lowers via
/// the fast-path template to exactly one K1 cell.
fn fredkin_prim() -> Iso {
    let t3 = flat(3);
    Iso::Prim(Box::new(
        prim_from_bits("fredkin", t3.clone(), t3, |b| CellKind::K1.eval(b)).expect("fredkin prim"),
    ))
}

/// Apply the Fredkin prim to the first three leaves of `flat(n)`.
fn fredkin_head(n: usize) -> Iso {
    assert!(n >= 3);
    if n == 3 {
        return fredkin_prim();
    }
    let r = flat(n - 3);
    // b0 x (b1 x (b2 x R))  <->  (b0 x (b1 x b2)) x R
    let reshape = Iso::seq(
        Iso::times(Iso::Id(bit()), Iso::AssocLProd(bit(), bit(), r.clone())),
        Iso::AssocLProd(bit(), Type::prod(bit(), bit()), r.clone()),
    );
    Iso::seq(
        Iso::seq(reshape.clone(), Iso::times(fredkin_prim(), Iso::Id(r))),
        Iso::sym(reshape),
    )
}

/// Fredkin on leaves (`c`, `a`, `b`) of `flat(n)`: bubble-route the
/// three wires to slots 0/1/2 with adjacent swaps, apply the prim at
/// the head, and undo the routing (`sym`). Only the K1 itself costs a
/// cell; all routing is rewiring.
///
/// Requires the (met-by-construction) invariant that after routing the
/// earlier wires, each remaining wire sits at or right of its slot.
fn fredkin_at(n: usize, c: usize, a: usize, b: usize) -> Iso {
    assert!(c != a && c != b && a != b);
    let mut arr: Vec<usize> = (0..n).collect(); // arr[slot] = original leaf index
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

// ── The instructions ────────────────────────────────────────────────

/// CSWAP rd, rs, rt : (Word8, (Word8, Word8)) <-> same.
///
/// Flat layout: bits 0..8 = rd, 8..16 = rs, 16..24 = rt. The cascade:
/// for each i, Fredkin(control = bit 0 = rd[0], rs[i], rt[i]); the
/// control is threaded through all 8 cells (each K1 passes it through).
fn cswap_iso() -> Iso {
    let n = 3 * W;
    let cascade: Vec<Iso> = (0..W).map(|i| fredkin_at(n, 0, W + i, 2 * W + i)).collect();
    let fl = flatten(&cswap_ty());
    Iso::seq(Iso::seq(fl.clone(), seq_all(n, cascade)), Iso::sym(fl))
}

/// Transpositions realizing the fixed left-rotation-by-`s` of the 8
/// `rd` wire slots: afterwards slot `j` holds original bit `(j - s)
/// mod 8` (i.e. output bit j = input bit (j-s) mod 8 = rotl by s).
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

/// ROTL rd, rs : (Word8, Word8) <-> same; rd <- rd rotl (rs mod 8).
///
/// Flat layout: bits 0..8 = rd, 8..16 = rs. Three conditional-rotate
/// stages; stage k rotates rd left by 2^k iff rs[k], as a cascade of
/// Fredkins all threading control bit 8+k. Stage transposition counts:
/// s=1 -> 7, s=2 -> 6, s=4 -> 4 (17 K1 cells total).
fn rotl_iso() -> Iso {
    let n = 2 * W;
    let mut cascade: Vec<Iso> = Vec::new();
    for k in 0..3 {
        let ctrl = W + k;
        for (i, j) in rot_transpositions(1 << k) {
            cascade.push(fredkin_at(n, ctrl, i, j));
        }
    }
    let fl = flatten(&rot_ty());
    Iso::seq(Iso::seq(fl.clone(), seq_all(n, cascade)), Iso::sym(fl))
}

/// ROTR rd, rs = the adjoint of ROTL, per the spec table.
fn rotr_iso() -> Iso {
    Iso::sym(rotl_iso())
}

// ── Netlist evaluation helpers (same pattern as the sibling tests) ──

/// Untimed functional evaluation of the cell netlist.
fn eval_cells(net: &PhasedCellNetlist, input_bits: &[bool]) -> Vec<bool> {
    let mut line: Vec<Option<bool>> = vec![None; net.num_lines as usize];
    fn set_pair(line: &mut [Option<bool>], p: RailPair, v: bool) {
        line[p.line1.0 as usize] = Some(v);
        line[p.line0.0 as usize] = Some(!v);
    }
    fn get_pair(line: &[Option<bool>], p: RailPair) -> bool {
        let v1 = line[p.line1.0 as usize].expect("1-line driven");
        let v0 = line[p.line0.0 as usize].expect("0-line driven");
        assert_ne!(v0, v1, "INV-TOKEN violated: pair not 1-of-2");
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

// ── Oracle harness ──────────────────────────────────────────────────

struct Lowered {
    net: PhasedCellNetlist,
    balanced: PhasedCellNetlist,
    in_ty: Type,
    out_ty: Type,
    n_data_out: usize,
}

/// Lower + validate + balance; assert every cell kind is <= K3 and
/// print the cell counts.
fn prepare(name: &str, iso: &Iso) -> Lowered {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let net = lower_iso(iso).unwrap_or_else(|e| panic!("{name}: lowering failed: {e}"));
    assert_eq!(net.validate(), Ok(()), "{name}: lowered netlist validates");

    let counts = net.cell_counts();
    let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
    assert!(
        max_kind <= CellKind::K3,
        "{name}: max cell kind {max_kind:?} exceeds K3"
    );
    eprintln!(
        "{name}: cell counts {counts:?}, max kind {max_kind:?}, ancilla inputs {}",
        net.inputs.len() - rail_width(&in_ty).unwrap()
    );

    let balanced = balance(&net).unwrap_or_else(|e| panic!("{name}: balance failed: {e:?}"));
    assert_eq!(balanced.validate(), Ok(()), "{name}: balanced netlist validates");
    assert!(
        balanced.tick_imbalances().unwrap().is_empty(),
        "{name}: balanced netlist has no tick imbalances"
    );
    eprintln!("{name}: balanced cell counts {:?}", balanced.cell_counts());

    let n_data_out = rail_width(&out_ty).unwrap();
    Lowered { net, balanced, in_ty, out_ty, n_data_out }
}

/// One operand case: core eval semantics vs the Rust reference, plus
/// faithfulness of both the untimed netlist evaluation and the timed
/// (balanced) simulation against core eval.
fn check_case(name: &str, iso: &Iso, low: &Lowered, operands: &[u8], expected: &[u8]) {
    let v = encode(&low.in_ty, operands);
    let mut fuel = EVAL_FUEL;
    let ev = theseus_eval::eval(iso, v.clone(), &mut fuel)
        .unwrap_or_else(|e| panic!("{name}: core eval failed on {operands:?}: {e:?}"));

    // (3) semantics against the Rust reference.
    assert_eq!(
        decode(&low.out_ty, &ev),
        expected,
        "{name}: semantics mismatch on operands {operands:?}"
    );

    // (2) faithfulness: netlist == core eval.
    let expected_bits = bits_of(&ev, &low.out_ty).unwrap();
    let data_bits = bits_of(&v, &low.in_ty).unwrap();

    let untimed = eval_cells(&low.net, &extend_with_ancilla(&data_bits, &low.net));
    assert_eq!(
        &untimed[..low.n_data_out],
        &expected_bits[..],
        "{name}: untimed netlist disagrees with core eval on {operands:?}"
    );

    let sim = simulate(&low.balanced, &extend_with_ancilla(&data_bits, &low.balanced))
        .unwrap_or_else(|e| panic!("{name}: timed sim failed on {operands:?}: {e:?}"));
    assert_eq!(
        &sim.outputs[..low.n_data_out],
        &expected_bits[..],
        "{name}: timed sim disagrees with core eval on {operands:?}"
    );
}

/// Core-eval a byte tuple through `iso` and decode the result.
fn eval_bytes(iso: &Iso, ty: &Type, operands: &[u8]) -> Vec<u8> {
    let mut fuel = EVAL_FUEL;
    let out = theseus_eval::eval(iso, encode(ty, operands), &mut fuel).expect("core eval");
    decode(ty, &out)
}

// ── Operand samples ─────────────────────────────────────────────────

const RD_CSWAP: [u8; 8] = [0x00, 0x02, 0xAA, 0xFE, 0x01, 0x03, 0x55, 0xFF];
const REG_SAMPLES: [u8; 8] = [0x00, 0xFF, 0xAA, 0x55, 0x0F, 0xF0, 0x12, 0x34];
const RD_ROT: [u8; 8] = [0x00, 0x01, 0x80, 0xAA, 0xFF, 0x3C, 0x81, 0x5A];
const RS_ROT: [u8; 14] = [0, 1, 2, 3, 4, 5, 6, 7, 0x08, 0x0F, 0x7A, 0x80, 0xFD, 0xFF];

// ── Tests ───────────────────────────────────────────────────────────

#[test]
fn cswap_lowering_and_oracle() {
    let iso = cswap_iso();
    let low = prepare("cswap", &iso);
    let ty = cswap_ty();

    let mut cases = 0usize;
    for &rd in &RD_CSWAP {
        for &rs in &REG_SAMPLES {
            for &rt in &REG_SAMPLES {
                let (ers, ert) = if rd & 1 == 1 { (rt, rs) } else { (rs, rt) };
                check_case("cswap", &iso, &low, &[rd, rs, rt], &[rd, ers, ert]);
                // Self-inverse: applying twice restores the operands.
                let once = eval_bytes(&iso, &ty, &[rd, rs, rt]);
                assert_eq!(
                    eval_bytes(&iso, &ty, &once),
                    vec![rd, rs, rt],
                    "cswap not self-inverse on ({rd:#04x},{rs:#04x},{rt:#04x})"
                );
                cases += 1;
            }
        }
    }
    eprintln!("cswap: oracle passed on {cases} operand cases (semantics + faithfulness + self-inverse)");
}

#[test]
fn rotl_lowering_and_oracle() {
    let iso = rotl_iso();
    let low = prepare("rotl", &iso);

    let mut cases = 0usize;
    for &rd in &RD_ROT {
        for &rs in &RS_ROT {
            let expected = rd.rotate_left(u32::from(rs) & 7);
            check_case("rotl", &iso, &low, &[rd, rs], &[expected, rs]);
            cases += 1;
        }
    }
    eprintln!("rotl: oracle passed on {cases} operand cases");
}

#[test]
fn rotr_lowering_and_adjoint_oracle() {
    let rotl = rotl_iso();
    let iso = rotr_iso();
    let low = prepare("rotr", &iso);
    let ty = rot_ty();
    let syntactic_adjoint = adjoint(&rotl);

    let mut cases = 0usize;
    for &rd in &RD_ROT {
        for &rs in &RS_ROT {
            let expected = rd.rotate_right(u32::from(rs) & 7);
            check_case("rotr", &iso, &low, &[rd, rs], &[expected, rs]);

            // ROTR == adjoint(ROTL): Iso::sym agrees with the syntactic
            // adjoint pointwise, and ROTR undoes ROTL.
            assert_eq!(
                eval_bytes(&syntactic_adjoint, &ty, &[rd, rs]),
                vec![expected, rs],
                "syntactic adjoint(rotl) disagrees with rotr on ({rd:#04x},{rs:#04x})"
            );
            let rolled = eval_bytes(&rotl, &ty, &[rd, rs]);
            assert_eq!(
                eval_bytes(&iso, &ty, &rolled),
                vec![rd, rs],
                "rotr(rotl(x)) != x on ({rd:#04x},{rs:#04x})"
            );
            cases += 1;
        }
    }
    eprintln!("rotr: oracle passed on {cases} operand cases (semantics + faithfulness + adjoint)");
}
