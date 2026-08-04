//! TISC-1 ARITHMETIC datapath instructions as reversible Core Pi isos,
//! oracle-verified (differential against `theseus_eval::eval`).
//!
//! Instructions (docs/src/design/tisc-1-spec.md section 2, W = 8, mod 256):
//!
//! * `ADD rd, rs`  -- `rd <- rd + rs (mod 256)`, `rs` preserved.
//! * `SUB rd, rs`  -- `rd <- rd - rs (mod 256)`, `rs` preserved;
//!   SUB = `Iso::sym(ADD)` (the spec's adjoint pairing).
//! * `ADDI rd, imm8` -- `rd <- rd + imm8`, imm a compile-time constant.
//! * `SUBI rd, imm8` -- `rd <- rd - imm8`; SUBI = `Iso::sym(ADDI)`.
//!
//! # Construction: shallow compositional ripple, max cell kind K2
//!
//! ADD is a COMPOSITION of 46 two/three-wire gate prims (32 CNOTs +
//! 14 Toffolis) on the 16 flat wires `b_i` = rd bits (0..8) and `a_i`
//! = rs bits (8..16) -- a Takahashi-Kunihiro-style in-place ripple with
//! ZERO ancilla wires. Each prim synthesizes on its own (CNOT -> K1,
//! Toffoli -> K2), so nothing ever needs K3/K4: control nesting never
//! exceeds depth 2 by construction.
//!
//! Why not the naive "3-wire full-adder stage (rd_i, rs_i, cin) ->
//! (sum_i, rs_i, cout)": that map is NOT a bijection (e.g. inputs
//! (cin,a,b) = (1,0,0) and (0,0,1) both yield (cout,a,s) = (0,0,1)),
//! and no fix exists -- a materialized ripple carry `c_i` is an
//! UNBALANCED Boolean function of the operand bits (it is 1 on fewer
//! than half of the inputs), so it cannot be a coordinate of any
//! bijection on the 16 operand wires. The Takahashi-Kunihiro trick is
//! to keep carries only in the balanced combination `a_i XOR c_i`,
//! stored transiently on the rs wires and unwound before the end:
//!
//! * P1: `b_i ^= a_i`                     (i = 1..6)
//! * P2: `a_{i+1} ^= a_i`                 (i = 6..1)  [prefix chain]
//! * P3: `a_{i+1} ^= a_i & b_i` (Toffoli) (i = 0..6)
//!   -> invariant: `a_i` wire = `a_i ^ c_i` (balanced!)
//! * P4: `b_7 ^= a_7`  -> `b_7 = s_7` (final; P1 skipped bit 7)
//! * P5: `a_7 ^= a_6 & b_6`  [strip `c_7`]
//! * P6: `b_i ^= a_i`; `a_i ^= a_{i-1} & b_{i-1}` (i = 6..1)
//!   -> `b_i = b_i ^ c_i`, rs wires back to the prefix chain
//! * P7: `a_{i+1} ^= a_i`                 (i = 1..6)  [undo P2]
//! * P8: `b_i ^= a_i`                     (i = 0..6)  -> `b_i = s_i`
//!
//! After P8: rd = rd + rs (mod 256), rs = rs exactly. The gate list is
//! ALSO verified exhaustively (all 65536 operand pairs) against a pure
//! Rust bit model in `tk_gate_model_matches_addition_exhaustively`.
//!
//! # ADDI/SUBI and the explicit aux word
//!
//! `x -> x + imm (mod 256)` with imm ODD is a single 256-cycle, i.e.
//! an ODD permutation of the 8-wire state space -- while every cell
//! acting on fewer than 8 wires induces an EVEN permutation (it is
//! 2^(8-m) >= 2 disjoint copies of the same m-wire permutation). So
//! ADDI on exactly 8 wires is IMPOSSIBLE in this cell model, for any
//! construction. Following the spec's own datapath ("immediate
//! supplied from IR bits", section on cell budgets), ADDI carries an
//! explicit aux Word8 that is 0 on input and restored to 0 on output:
//! imprint imm onto the aux wires with 0-cell NOTs (SwapSum rail
//! swaps), run the verified ADD ripple, wipe the imm again. Semantics:
//! `(rd, 0) <-> (rd + imm mod 256, 0)`.
//!
//! Routing (getting a gate's 2-3 wires adjacent at the head) is pure
//! product-structure plumbing -- swap/assoc isos, zero-cell rewiring --
//! built as `route ; gate-at-head ; sym(route)`, exactly as in the
//! sibling `tisc_controlled.rs`.

use theseus_core::{adjoint, type_of, Iso, Type, Value};
use theseus_netlist::{
    balance, bits_of, prim_from_bits, rail_width, simulate, value_from_bits, CellKind,
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

/// ADD/SUB (and ADDI/SUBI aux) operand tuple: (rd, rs).
fn pair_ty() -> Type {
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

// ── Gate prims: CNOT (K1) and Toffoli (K2) ──────────────────────────

/// 2-wire CNOT prim `(c, t) -> (c, t ^ c)`.
fn cnot_prim() -> Iso {
    let t2 = flat(2);
    let def =
        prim_from_bits("cnot_ar", t2.clone(), t2, |b| vec![b[0], b[1] ^ b[0]]).expect("cnot prim");
    Iso::Prim(Box::new(def))
}

/// 3-wire Toffoli prim `(c1, c2, t) -> (c1, c2, t ^ (c1 & c2))`.
fn tof_prim() -> Iso {
    let t3 = flat(3);
    let def = prim_from_bits("tof_ar", t3.clone(), t3, |b| {
        vec![b[0], b[1], b[2] ^ (b[0] & b[1])]
    })
    .expect("tof prim");
    Iso::Prim(Box::new(def))
}

/// NOT on a single Bit = SwapSum(1,1) -- zero cells, pure rail swap.
fn not_iso() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// Apply a `flat(2) <-> flat(2)` prim to the first two leaves of `flat(n)`.
fn head2(n: usize, p: Iso) -> Iso {
    assert!(n >= 2);
    if n == 2 {
        return p;
    }
    let r = flat(n - 2);
    // b0 x (b1 x R) <-> (b0 x b1) x R
    let reshape = Iso::AssocLProd(bit(), bit(), r.clone());
    Iso::seq(
        Iso::seq(reshape.clone(), Iso::times(p, Iso::Id(r))),
        Iso::sym(reshape),
    )
}

/// Apply a `flat(3) <-> flat(3)` prim to the first three leaves of `flat(n)`.
fn head3(n: usize, p: Iso) -> Iso {
    assert!(n >= 3);
    if n == 3 {
        return p;
    }
    let r = flat(n - 3);
    // b0 x (b1 x (b2 x R))  <->  (b0 x (b1 x b2)) x R
    let reshape = Iso::seq(
        Iso::times(Iso::Id(bit()), Iso::AssocLProd(bit(), bit(), r.clone())),
        Iso::AssocLProd(bit(), Type::prod(bit(), bit()), r.clone()),
    );
    Iso::seq(
        Iso::seq(reshape.clone(), Iso::times(p, Iso::Id(r))),
        Iso::sym(reshape),
    )
}

/// Apply `prim` (2 or 3 wires wide) to the given leaves of `flat(n)`:
/// bubble-route the wires to the head slots with adjacent swaps, apply
/// the prim at the head, and undo the routing (`sym`). Only the prim
/// itself costs cells; all routing is rewiring.
fn gate_at(n: usize, wires: &[usize], prim: Iso) -> Iso {
    let w = wires.len();
    assert!(w == 2 || w == 3);
    let mut arr: Vec<usize> = (0..n).collect(); // arr[slot] = original leaf index
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
    let headed = if w == 2 {
        head2(n, prim)
    } else {
        head3(n, prim)
    };
    Iso::seq(Iso::seq(route.clone(), headed), Iso::sym(route))
}

// ── The Takahashi-Kunihiro-style in-place ripple gate list ──────────

#[derive(Clone, Copy, Debug)]
enum Gate {
    /// `t ^= c`
    Cnot { c: usize, t: usize },
    /// `t ^= c1 & c2`
    Tof { c1: usize, c2: usize, t: usize },
}

/// The 46-gate zero-ancilla in-place adder on flat(16):
/// wires 0..8 = rd bits (`b_i`), 8..16 = rs bits (`a_i`), LSB first.
/// Effect: `b <- b + a (mod 256)`, `a` preserved. See module docs.
fn tk_add_gates() -> Vec<Gate> {
    let a = |i: usize| W + i;
    let b = |i: usize| i;
    let mut g: Vec<Gate> = Vec::new();
    // P1: b_i ^= a_i (i = 1..=6; bit 7 deliberately skipped, see P4).
    for i in 1..W - 1 {
        g.push(Gate::Cnot { c: a(i), t: b(i) });
    }
    // P2: prefix chain a_{i+1} ^= a_i, descending.
    for i in (1..W - 1).rev() {
        g.push(Gate::Cnot {
            c: a(i),
            t: a(i + 1),
        });
    }
    // P3: carry ripple: a_{i+1} ^= a_i & b_i  ->  a_j wire = a_j ^ c_j.
    for i in 0..W - 1 {
        g.push(Gate::Tof {
            c1: a(i),
            c2: b(i),
            t: a(i + 1),
        });
    }
    // P4: top sum: b_7 ^= (a_7 ^ c_7)  ->  b_7 = s_7 (b_7 never got a_7).
    g.push(Gate::Cnot {
        c: a(W - 1),
        t: b(W - 1),
    });
    // P5: strip c_7 off the a_7 wire (reverse of P3's last Toffoli).
    g.push(Gate::Tof {
        c1: a(W - 2),
        c2: b(W - 2),
        t: a(W - 1),
    });
    // P6: descending: fold c_i into b_i, then strip c_i off a_i.
    for i in (1..W - 1).rev() {
        g.push(Gate::Cnot { c: a(i), t: b(i) });
        g.push(Gate::Tof {
            c1: a(i - 1),
            c2: b(i - 1),
            t: a(i),
        });
    }
    // P7: undo the P2 prefix chain, ascending.
    for i in 1..W - 1 {
        g.push(Gate::Cnot {
            c: a(i),
            t: a(i + 1),
        });
    }
    // P8: final sums b_i ^= a_i (i = 0..=6).
    for i in 0..W - 1 {
        g.push(Gate::Cnot { c: a(i), t: b(i) });
    }
    g
}

/// Pure bit-model execution of a gate list (ground truth for the
/// construction itself, independent of the iso/netlist pipeline).
fn model_run(gates: &[Gate], bits: &mut [bool]) {
    for g in gates {
        match *g {
            Gate::Cnot { c, t } => bits[t] ^= bits[c],
            Gate::Tof { c1, c2, t } => bits[t] ^= bits[c1] & bits[c2],
        }
    }
}

// ── The instruction isos ────────────────────────────────────────────

/// Compose a gate list into an iso on `flat(n)`.
fn gates_iso(n: usize, gates: &[Gate]) -> Iso {
    let isos: Vec<Iso> = gates
        .iter()
        .map(|g| match *g {
            Gate::Cnot { c, t } => gate_at(n, &[c, t], cnot_prim()),
            Gate::Tof { c1, c2, t } => gate_at(n, &[c1, c2, t], tof_prim()),
        })
        .collect();
    seq_all(n, isos)
}

/// ADD rd, rs : (Word8, Word8) <-> (Word8, Word8); rd <- rd + rs.
fn add_iso() -> Iso {
    let fl = flatten(&pair_ty());
    Iso::seq(
        Iso::seq(fl.clone(), gates_iso(2 * W, &tk_add_gates())),
        Iso::sym(fl),
    )
}

/// SUB rd, rs = the adjoint of ADD, per the spec table.
fn sub_iso() -> Iso {
    Iso::sym(add_iso())
}

/// XOR the aux word (flat wires 8..16) with the constant `imm`:
/// per-bit NOT (0 cells) where the imm bit is set.
fn imm_mask_iso(imm: u8) -> Iso {
    let n = 2 * W;
    let nots: Vec<Iso> = (0..W)
        .filter(|i| (imm >> i) & 1 == 1)
        .map(|i| at_leaf(n, W + i, not_iso()))
        .collect();
    seq_all(n, nots)
}

/// ADDI rd, imm8 : (Word8, Word8) <-> (Word8, Word8), aux word 0 -> 0;
/// rd <- rd + imm8. imm is baked into the iso (compile-time constant):
/// imprint imm on the zeroed aux word, run ADD, wipe imm again.
fn addi_iso(imm: u8) -> Iso {
    let fl = flatten(&pair_ty());
    let mask = imm_mask_iso(imm);
    let body = Iso::seq(
        Iso::seq(mask.clone(), gates_iso(2 * W, &tk_add_gates())),
        mask,
    );
    Iso::seq(Iso::seq(fl.clone(), body), Iso::sym(fl))
}

/// SUBI rd, imm8 = the adjoint of ADDI, per the spec table.
fn subi_iso(imm: u8) -> Iso {
    Iso::sym(addi_iso(imm))
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

/// Append the lowering's real ancilla init values to the data bits
/// (the R1-based synthesis needs its chain scratch pairs seeded 0 and
/// its constant controls seeded 1 -- positional, from
/// `lower_iso_with_ancillas`).
fn extend_with_ancilla(data_bits: &[bool], ancillas: &[bool]) -> Vec<bool> {
    let mut bits = data_bits.to_vec();
    bits.extend_from_slice(ancillas);
    bits
}

// ── Oracle harness ──────────────────────────────────────────────────

struct Lowered {
    net: PhasedCellNetlist,
    balanced: PhasedCellNetlist,
    ancillas: Vec<bool>,
    in_ty: Type,
    out_ty: Type,
    n_data_out: usize,
}

/// Lower + validate + balance; assert every cell kind is in the
/// admitted vocabulary (K0-K3 CSWAPs + the R1 rail-role swap,
/// ADR-0040) and print the cell counts.
fn prepare(name: &str, iso: &Iso) -> Lowered {
    let (in_ty, out_ty) = type_of(iso).expect("iso types");
    let (net, ancillas) = theseus_netlist::lower::lower_iso_with_ancillas(iso)
        .unwrap_or_else(|e| panic!("{name}: lowering failed: {e}"));
    assert_eq!(net.validate(), Ok(()), "{name}: lowered netlist validates");

    let counts = net.cell_counts();
    let max_kind = counts.keys().max().copied().unwrap_or(CellKind::K0);
    assert!(
        counts
            .keys()
            .all(|&k| k <= CellKind::K3 || k == CellKind::R1),
        "{name}: cell kind outside the K0-K3 + R1 vocabulary: {counts:?}"
    );
    eprintln!(
        "{name}: cell counts {counts:?}, max kind {max_kind:?}, ancilla inputs {}",
        net.inputs.len() - rail_width(&in_ty).unwrap()
    );

    let balanced = balance(&net).unwrap_or_else(|e| panic!("{name}: balance failed: {e:?}"));
    assert_eq!(
        balanced.validate(),
        Ok(()),
        "{name}: balanced netlist validates"
    );
    assert!(
        balanced.tick_imbalances().unwrap().is_empty(),
        "{name}: balanced netlist has no tick imbalances"
    );
    eprintln!("{name}: balanced cell counts {:?}", balanced.cell_counts());

    let n_data_out = rail_width(&out_ty).unwrap();
    Lowered {
        net,
        balanced,
        ancillas,
        in_ty,
        out_ty,
        n_data_out,
    }
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

    let untimed = eval_cells(&low.net, &extend_with_ancilla(&data_bits, &low.ancillas));
    assert_eq!(
        &untimed[..low.n_data_out],
        &expected_bits[..],
        "{name}: untimed netlist disagrees with core eval on {operands:?}"
    );

    let sim = simulate(
        &low.balanced,
        &extend_with_ancilla(&data_bits, &low.ancillas),
    )
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

/// 8x8 grid: 64 pairs, covering 0+0, 0xFF+1, 0xFF+0xFF (full carry
/// propagate), 0x0F+0x01, 0x80+0x80 (MSB carry out), plus a spread.
const OPS: [u8; 8] = [0x00, 0x01, 0x0F, 0x55, 0x7F, 0x80, 0xAA, 0xFF];

/// 16 extra deterministic pseudo-random pairs (xorshift-ish LCG).
fn extra_pairs() -> Vec<(u8, u8)> {
    let mut x: u32 = 0x2A2A_1234;
    let mut out = Vec::new();
    for _ in 0..16 {
        x = x.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
        out.push(((x >> 8) as u8, (x >> 20) as u8));
    }
    out
}

fn add_pairs() -> Vec<(u8, u8)> {
    let mut pairs: Vec<(u8, u8)> = Vec::new();
    for &rd in &OPS {
        for &rs in &OPS {
            pairs.push((rd, rs));
        }
    }
    pairs.extend(extra_pairs());
    pairs
}

const IMMS: [u8; 5] = [0x00, 0x01, 0x0F, 0x80, 0xFF];

// ── Tests ───────────────────────────────────────────────────────────

/// Ground-truth check of the ripple construction itself: the 46-gate
/// list computes (rd, rs) -> (rd + rs mod 256, rs) on ALL 65536 pairs.
#[test]
fn tk_gate_model_matches_addition_exhaustively() {
    let gates = tk_add_gates();
    assert_eq!(gates.len(), 46, "expected 32 CNOTs + 14 Toffolis");
    for rd in 0..=255u8 {
        for rs in 0..=255u8 {
            let mut bits = bytes_to_bits(&[rd, rs]);
            model_run(&gates, &mut bits);
            let out = bits_to_bytes(&bits);
            assert_eq!(
                out,
                vec![rd.wrapping_add(rs), rs],
                "gate model wrong on rd={rd:#04x} rs={rs:#04x}"
            );
        }
    }
    eprintln!("add gate model: exhaustive 65536/65536 cases passed");
}

#[test]
fn add_lowering_and_oracle() {
    let iso = add_iso();
    let low = prepare("add", &iso);

    let mut cases = 0usize;
    for (rd, rs) in add_pairs() {
        check_case("add", &iso, &low, &[rd, rs], &[rd.wrapping_add(rs), rs]);
        cases += 1;
    }
    eprintln!("add: oracle passed on {cases} operand cases (semantics + faithfulness)");
}

#[test]
fn sub_lowering_and_adjoint_oracle() {
    let add = add_iso();
    let iso = sub_iso();
    let low = prepare("sub", &iso);
    let ty = pair_ty();
    let syntactic_adjoint = adjoint(&add);

    let mut cases = 0usize;
    for (rd, rs) in add_pairs() {
        let expected = rd.wrapping_sub(rs);
        check_case("sub", &iso, &low, &[rd, rs], &[expected, rs]);

        // SUB == adjoint(ADD): Iso::sym agrees with the syntactic
        // adjoint pointwise, and SUB undoes ADD.
        assert_eq!(
            eval_bytes(&syntactic_adjoint, &ty, &[rd, rs]),
            vec![expected, rs],
            "syntactic adjoint(add) disagrees with sub on ({rd:#04x},{rs:#04x})"
        );
        let summed = eval_bytes(&add, &ty, &[rd, rs]);
        assert_eq!(
            eval_bytes(&iso, &ty, &summed),
            vec![rd, rs],
            "sub(add(x)) != x on ({rd:#04x},{rs:#04x})"
        );
        cases += 1;
    }
    eprintln!("sub: oracle passed on {cases} operand cases (semantics + faithfulness + adjoint)");
}

#[test]
fn addi_lowering_and_oracle() {
    for &imm in &IMMS {
        let iso = addi_iso(imm);
        let low = prepare(&format!("addi(imm={imm:#04x})"), &iso);

        let mut cases = 0usize;
        for rd in 0..=255u8 {
            check_case(
                "addi",
                &iso,
                &low,
                &[rd, 0x00],
                &[rd.wrapping_add(imm), 0x00],
            );
            cases += 1;
        }
        eprintln!("addi(imm={imm:#04x}): oracle passed on {cases} rd values (all 256)");
    }
}

#[test]
fn subi_lowering_and_adjoint_oracle() {
    let ty = pair_ty();
    for &imm in &IMMS {
        let addi = addi_iso(imm);
        let iso = subi_iso(imm);
        let low = prepare(&format!("subi(imm={imm:#04x})"), &iso);
        let syntactic_adjoint = adjoint(&addi);

        let mut cases = 0usize;
        for rd in 0..=255u8 {
            let expected = rd.wrapping_sub(imm);
            check_case("subi", &iso, &low, &[rd, 0x00], &[expected, 0x00]);

            // SUBI == adjoint(ADDI): pointwise agreement + round trip.
            assert_eq!(
                eval_bytes(&syntactic_adjoint, &ty, &[rd, 0x00]),
                vec![expected, 0x00],
                "syntactic adjoint(addi) disagrees with subi on rd={rd:#04x} imm={imm:#04x}"
            );
            let summed = eval_bytes(&addi, &ty, &[rd, 0x00]);
            assert_eq!(
                eval_bytes(&iso, &ty, &summed),
                vec![rd, 0x00],
                "subi(addi(x)) != x on rd={rd:#04x} imm={imm:#04x}"
            );
            cases += 1;
        }
        eprintln!(
            "subi(imm={imm:#04x}): oracle passed on {cases} rd values (semantics + faithfulness + adjoint)"
        );
    }
}
