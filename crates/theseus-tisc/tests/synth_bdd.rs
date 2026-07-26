//! A-6.D (D.0 + D.1) tests — the BDD→gates transcriber
//! (`transcribe_bdd_into` via `transcribe_bdd_bennett`).
//!
//! BDD synthesis is the GENERAL, structure-preserving prim→gates mechanism: it
//! walks the canonical ROBDD of each output bit and emits O(#nodes) gates. Each
//! test reports `node_count` vs `gate_count` — the compactness evidence — plus
//! two verification tiers:
//!
//!   * SYMBOLIC (`symbolic_exec` + `equiv`, all-register carriers + ancilla +
//!     helper words, total ≤ NUM_REGS): the COMPLETE whole-domain proof — carriers
//!     ≡ fwd AND every helper/ancilla row ≡ 0 (net-zero). Forces `bwd = fwd⁻¹`.
//!     Feasible only when `2n + 1(temp) + max_node_count ≤ 8`; the BDD's per-node
//!     ancilla words eat into the register file, so the deeper specs use...
//!   * NET-ZERO EXEC (`exec_instr`): run the body from concrete carrier states
//!     (helpers = 0) and assert carriers land at `P(x)` AND every helper/ancilla
//!     word returns to 0. For the n=2 nonlinear spec this is run EXHAUSTIVELY over
//!     all 2^16 carrier states (complete); helpers may live in scratch (which the
//!     symbolic model cannot represent).
//!
//! Plus `assert_all_encode` on every emitted instruction.

use theseus_tisc::asm::{encode, Instr};
use theseus_tisc::bveq::{
    equiv, expr_to_bdd, symbolic_exec, BvExpr, RegTransfer, VarMap, BITS,
};
use theseus_tisc::compile::exec_instr;
use theseus_tisc::synth::{
    transcribe_bdd_bennett, transcribe_bennett_general, BddHelpers, HelperBank,
};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

// ── Shared helpers (mirror synth_bennett.rs) ──────────────────────────

fn v(r: usize, b: usize) -> BvExpr {
    BvExpr::var(r, b)
}

fn spec(f: impl Fn(usize, usize) -> BvExpr) -> RegTransfer {
    let mut t = RegTransfer::identity();
    for r in 0..t.regs.len() {
        for b in 0..BITS {
            t.regs[r][b] = f(r, b);
        }
    }
    t
}

fn assert_all_encode(body: &[Instr]) {
    for (i, &ins) in body.iter().enumerate() {
        encode(ins).unwrap_or_else(|e| {
            panic!("emitted instr {i} ({ins:?}) fails asm::encode: {e} — body is not assemblable")
        });
    }
}

fn zero_state() -> StateVals {
    StateVals {
        regs: [0u8; NUM_REGS],
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0u8; NUM_SCRATCH],
        mem: [0u8; MEM_WORDS],
    }
}

fn zero_out(e: &BvExpr, zero_regs: &[usize]) -> BvExpr {
    match e {
        BvExpr::Var { operand, .. } if zero_regs.contains(operand) => BvExpr::Const(false),
        BvExpr::Var { .. } | BvExpr::Const(_) => e.clone(),
        BvExpr::Not(a) => zero_out(a, zero_regs).not(),
        BvExpr::Xor(a, b) => zero_out(a, zero_regs).xor(zero_out(b, zero_regs)),
        BvExpr::And(a, b) => zero_out(a, zero_regs).and(zero_out(b, zero_regs)),
        BvExpr::Or(a, b) => zero_out(a, zero_regs).or(zero_out(b, zero_regs)),
        BvExpr::Mux { sel, then_, else_ } => BvExpr::mux(
            zero_out(sel, zero_regs),
            zero_out(then_, zero_regs),
            zero_out(else_, zero_regs),
        ),
    }
}

fn transfer_zero_out(t: &RegTransfer, zero_regs: &[usize]) -> RegTransfer {
    RegTransfer {
        regs: t
            .regs
            .iter()
            .map(|row| row.iter().map(|e| zero_out(e, zero_regs)).collect())
            .collect(),
    }
}

/// MAX single-output-bit ROBDD node count over the carrier rows of `fwd` and
/// `bwd` — the node-word budget the transcriber needs and the headline node_count.
fn max_node_count(fwd: &RegTransfer, bwd: &RegTransfer, n: usize) -> usize {
    let map = VarMap::new(NUM_REGS).unwrap();
    let mut mx = 0;
    for s in [fwd, bwd] {
        for r in 0..n {
            for b in 0..BITS {
                let bdd = expr_to_bdd(&s.regs[r][b], &map).unwrap();
                mx = mx.max(bdd.size().saturating_sub(2));
            }
        }
    }
    mx
}

/// Total ROBDD node count summed across all carrier output bits of `fwd`.
fn total_fwd_nodes(fwd: &RegTransfer, n: usize) -> usize {
    let map = VarMap::new(NUM_REGS).unwrap();
    let mut total = 0;
    for r in 0..n {
        for b in 0..BITS {
            let bdd = expr_to_bdd(&fwd.regs[r][b], &map).unwrap();
            total += bdd.size().saturating_sub(2);
        }
    }
    total
}

/// COMPLETE register-symbolic dry-run. Layout in the register file:
///   carriers = 0..n, ancilla = n..2n, temp = 2n, node words = 2n+1.. .
/// Proves carriers ≡ fwd and every ancilla/helper row ≡ 0 over the WHOLE domain.
/// Returns the emitted gate count. Requires `2n + 1 + n_nodes ≤ NUM_REGS`.
fn bdd_bennett_symbolic_dryrun(
    fwd: &RegTransfer,
    bwd: &RegTransfer,
    n: usize,
    n_nodes: usize,
) -> usize {
    assert!(
        (2 * n + 1 + n_nodes) <= NUM_REGS,
        "register dry-run needs 2n+1+n_nodes ({}) ≤ NUM_REGS ({NUM_REGS})",
        2 * n + 1 + n_nodes
    );
    let carrier_ops: Vec<u8> = (0..n as u8).collect();
    let ancilla_ops: Vec<u8> = (n as u8..2 * n as u8).collect();
    let temp = (2 * n) as u8;
    let nodes: Vec<u8> = ((2 * n + 1)..(2 * n + 1 + n_nodes)).map(|x| x as u8).collect();
    let helpers = BddHelpers { temp, nodes: &nodes };
    let body = transcribe_bdd_bennett(fwd, bwd, &carrier_ops, &ancilla_ops, helpers)
        .expect("transcribe_bdd_bennett");
    assert_all_encode(&body);

    let got = symbolic_exec(&body).expect("symbolic_exec of BDD-Bennett body");
    let mut zero_regs: Vec<usize> = (n..2 * n).collect();
    zero_regs.push(temp as usize);
    zero_regs.extend(nodes.iter().map(|&w| w as usize));
    let got0 = transfer_zero_out(&got, &zero_regs);

    let expected = RegTransfer {
        regs: (0..NUM_REGS)
            .map(|r| {
                if r < n {
                    fwd.regs[r].clone()
                } else if zero_regs.contains(&r) {
                    vec![BvExpr::Const(false); BITS]
                } else {
                    (0..BITS).map(|b| v(r, b)).collect()
                }
            })
            .collect(),
    };
    let touched: Vec<usize> = zero_regs.iter().copied().chain(0..n).collect();
    equiv(&got0, &expected, BITS, &touched).expect(
        "BDD-Bennett (helpers=0) must equal fwd on carriers and 0 on every helper/ancilla \
         (complete bwd=fwd⁻¹ + net-zero)",
    );
    body.len()
}

/// Net-zero exec check over a list of carrier samples. Returns the gate count.
fn bdd_bennett_exec_netzero(
    fwd: &RegTransfer,
    bwd: &RegTransfer,
    carrier_ops: &[u8],
    ancilla_ops: &[u8],
    temp: u8,
    nodes: &[u8],
    samples: impl Iterator<Item = Vec<u8>>,
    apply_fwd: impl Fn(&[u8]) -> Vec<u8>,
) -> usize {
    let n = carrier_ops.len();
    let helpers = BddHelpers { temp, nodes };
    let body = transcribe_bdd_bennett(fwd, bwd, carrier_ops, ancilla_ops, helpers)
        .expect("transcribe_bdd_bennett");
    assert_all_encode(&body);

    let mut all_helpers: Vec<u8> = ancilla_ops.to_vec();
    all_helpers.push(temp);
    all_helpers.extend_from_slice(nodes);

    for sample in samples {
        assert_eq!(sample.len(), n, "sample must set every carrier");
        let mut st = zero_state();
        for (i, &op) in carrier_ops.iter().enumerate() {
            st.operand_set(op, sample[i]);
        }
        for &h in &all_helpers {
            assert_eq!(st.operand_get(h), 0, "helper op {h} must start 0");
        }
        for &instr in &body {
            exec_instr(instr, &mut st);
        }
        let want = apply_fwd(&sample);
        for (i, &op) in carrier_ops.iter().enumerate() {
            assert_eq!(st.operand_get(op), want[i], "carrier op {op} wrong for {sample:?}");
        }
        for &h in &all_helpers {
            assert_eq!(
                st.operand_get(h),
                0,
                "helper op {h} not restored to 0 for {sample:?} (net-zero)"
            );
        }
    }
    body.len()
}

// ── Spec (1): a 2-register swap (a,b) ↦ (b,a) ─────────────────────────

fn swap_fwd() -> RegTransfer {
    spec(|r, b| match r {
        0 => v(1, b),
        1 => v(0, b),
        _ => v(r, b),
    })
}

#[test]
fn bdd_swap_symbolic_and_report() {
    let (fwd, bwd) = (swap_fwd(), swap_fwd());
    let n = 2;
    let nc = max_node_count(&fwd, &bwd, n);
    let gates = bdd_bennett_symbolic_dryrun(&fwd, &bwd, n, nc);
    eprintln!(
        "[REPORT swap]      n={n} max_node_count={nc} total_fwd_nodes={} gate_count={gates}  \
         COMPLETE symbolic. Each output bit = a single input Var ⇒ 1-node ROBDD; ANF here is \
         degree-1 (same size) — the linear baseline.",
        total_fwd_nodes(&fwd, n)
    );
}

#[test]
fn bdd_swap_exec_netzero_scratch() {
    // carriers r0,r1 ; ancilla scratch s2,s3 (10,11) ; temp s4 (12) ; node s5 (13).
    // Swap step emits Xchg (reg↔scratch); exec_instr supports it.
    let g = bdd_bennett_exec_netzero(
        &swap_fwd(),
        &swap_fwd(),
        &[0, 1],
        &[10, 11],
        12,
        &[13],
        [vec![0x00, 0x00], vec![0xA5, 0x3C], vec![0xFF, 0x01], vec![0x12, 0x34]].into_iter(),
        |x| vec![x[1], x[0]],
    );
    eprintln!("[REPORT swap-scratch] gate_count={g} (scratch helpers, Xchg swap)");
}

// ── Spec (2): a Toffoli / CCNOT-mix permutation ───────────────────────
//   (a,b,c) ↦ (a ⊕ (b∧c), b, c) — a byte-parallel controlled-controlled-NOT.

fn toffoli_fwd() -> RegTransfer {
    spec(|r, b| match r {
        0 => v(0, b).xor(v(1, b).and(v(2, b))),
        _ => v(r, b),
    })
}

#[test]
fn bdd_toffoli_report_and_exec() {
    let (fwd, bwd) = (toffoli_fwd(), toffoli_fwd());
    let n = 3;
    let nc = max_node_count(&fwd, &bwd, n);
    // 2n+1+nc = 7+5 = 12 > 8 ⇒ the COMPLETE register-symbolic tier does not fit
    // (the ROBDD's per-node ancilla words exceed the register file). Verified via
    // net-zero exec over an exhaustive-per-bit sample instead. Report node_count.
    // Layout: carriers r0,r1,r2 ; ancilla r3,r4,r5 ; temp r6 ; node words scratch
    // s2..s6 (10..15, five words for the 5-node output bit).
    let mut samples: Vec<Vec<u8>> = Vec::new();
    // Exhaustive over one bit position's (a,b,c) triples, broadcast to full bytes,
    // plus corner bytes — the transfer is byte-parallel so per-bit coverage is
    // representative; the corners exercise all-0 / all-1 / mixed.
    for a in [0x00u8, 0xFF, 0xA5, 0x5A, 0x0F, 0xF0] {
        for b in [0x00u8, 0xFF, 0xA5, 0x33] {
            for c in [0x00u8, 0xFF, 0x0F, 0xCC] {
                samples.push(vec![a, b, c]);
            }
        }
    }
    let g = bdd_bennett_exec_netzero(
        &fwd,
        &bwd,
        &[0, 1, 2],
        &[3, 4, 5],
        6,
        &[10, 11, 12, 13, 14],
        samples.into_iter(),
        |x| vec![x[0] ^ (x[1] & x[2]), x[1], x[2]],
    );
    eprintln!(
        "[REPORT toffoli]   n={n} max_node_count={nc} total_fwd_nodes={} gate_count={g}  \
         net-zero exec (96 samples). Output bit a⊕(b∧c): degree-2 ANF (1 AND-monomial/bit), \
         ROBDD = {nc} nodes/bit under the interleaved var order; gate_count ≈ O(Σ nodes) \
         (×2 for Bennett fwd+bwd).",
        total_fwd_nodes(&fwd, n)
    );
}

// ── Spec (3): a NONLINEAR permutation whose ANF is higher-degree ──────
//
//   reg0_b_new = reg0_b ⊕ (reg1_bit0 ∧ reg1_bit1)   for every b; reg1 unchanged.
//
// The control is the conjunction of two DISTINCT input bits (bit0 ∧ bit1 of
// reg1) — a degree-2 monomial that ANF replicates into EVERY one of reg0's 8
// output bits (8 AND-monomials), while the ROBDD shares that structure. It is an
// involution (control reads only the untouched reg1), so bwd = fwd. This is
// verified EXHAUSTIVELY over all 2^16 carrier states via net-zero exec, and its
// node/gate counts are reported.

fn nonlinear_fwd() -> RegTransfer {
    spec(|r, b| match r {
        0 => v(0, b).xor(v(1, 0).and(v(1, 1))),
        _ => v(r, b),
    })
}

fn apply_nonlinear(x: &[u8]) -> Vec<u8> {
    let ctrl = (x[1] & 1) & ((x[1] >> 1) & 1);
    let flip = if ctrl == 1 { 0xFFu8 } else { 0x00 };
    vec![x[0] ^ flip, x[1]]
}

#[test]
fn bdd_nonlinear_report_and_exhaustive_exec() {
    let (fwd, bwd) = (nonlinear_fwd(), nonlinear_fwd());
    let n = 2;
    let nc = max_node_count(&fwd, &bwd, n);
    // EXHAUSTIVE 2^16 exec (complete for n=2). carriers r0,r1 ; ancilla r2,r3 ;
    // temp r4 ; node words scratch s2..s6 (10..15, five words for the 5-node bit).
    let samples = (0u32..(1 << 16)).map(|x| vec![(x & 0xFF) as u8, ((x >> 8) & 0xFF) as u8]);
    let g = bdd_bennett_exec_netzero(
        &fwd,
        &bwd,
        &[0, 1],
        &[2, 3],
        4,
        &[10, 11, 12, 13, 14],
        samples,
        apply_nonlinear,
    );
    // Direct compactness comparison: the SAME permutation through the A-6.2 ANF
    // path (`transcribe_bennett_general`). Its length is set by the ANF (a
    // degree-2 monomial in EVERY output bit) rather than the shared ROBDD.
    let anf_helpers = HelperBank { mask: 5, temps: &[6] }; // degree-2 ⇒ 1 temp
    let anf_body = transcribe_bennett_general(&fwd, &bwd, &[0, 1], &[2, 3], anf_helpers)
        .expect("transcribe_bennett_general");
    let anf_gates = anf_body.len();
    eprintln!(
        "[REPORT nonlinear] n={n} max_node_count={nc} total_fwd_nodes={} BDD_gate_count={g} \
         ANF_gate_count={anf_gates}  EXHAUSTIVE 2^16 net-zero exec. Output bit x_b ⊕ (y0∧y1): a \
         degree-2 monomial of two DISTINCT input bits, which ANF replicates across ALL 8 output \
         bits (8 AND-monomials); the ROBDD shares it ⇒ {nc} nodes/bit. BDD gates scale with the \
         node count; both are linear here (small degree), so the counts are the same ORDER — the \
         BDD win is that it stays O(nodes) as degree grows (the gcd2 degree-7 blowup is D.2).",
        total_fwd_nodes(&fwd, n)
    );
}

// ── TEETH ──────────────────────────────────────────────────────────────

/// The COMPLETE symbolic dry-run rejects a wrong backward (uses swap, which fits
/// the register budget). Identity is not swap⁻¹? swap IS an involution, so use a
/// deliberately-broken bwd = identity against a NON-involution fwd. A rotate-by-1
/// permutation reg0↦reg1, reg1↦reg0 is swap (involution). Instead corrupt via a
/// CNOT: fwd reg0 = reg0⊕reg1 (not an involution alone) — but that touches only
/// reg0; its inverse is itself for XOR... Use fwd=swap, wrong bwd=identity.
#[test]
fn bdd_wrong_backward_fails_symbolic() {
    let fwd = swap_fwd();
    let wrong = RegTransfer::identity(); // identity ≠ swap⁻¹ = swap
    let n = 2;
    let nc = max_node_count(&fwd, &wrong, n).max(1);
    let carrier_ops: Vec<u8> = (0..n as u8).collect();
    let ancilla_ops: Vec<u8> = (n as u8..2 * n as u8).collect();
    let temp = (2 * n) as u8;
    let nodes: Vec<u8> = ((2 * n + 1)..(2 * n + 1 + nc)).map(|x| x as u8).collect();
    let helpers = BddHelpers { temp, nodes: &nodes };
    let body =
        transcribe_bdd_bennett(&fwd, &wrong, &carrier_ops, &ancilla_ops, helpers).unwrap();
    let got = symbolic_exec(&body).unwrap();
    let mut zero_regs: Vec<usize> = (n..2 * n).collect();
    zero_regs.push(temp as usize);
    zero_regs.extend(nodes.iter().map(|&w| w as usize));
    let got0 = transfer_zero_out(&got, &zero_regs);
    let expected = RegTransfer {
        regs: (0..NUM_REGS)
            .map(|r| {
                if r < n {
                    fwd.regs[r].clone()
                } else if zero_regs.contains(&r) {
                    vec![BvExpr::Const(false); BITS]
                } else {
                    (0..BITS).map(|b| v(r, b)).collect()
                }
            })
            .collect(),
    };
    let touched: Vec<usize> = zero_regs.iter().copied().chain(0..n).collect();
    assert!(
        equiv(&got0, &expected, BITS, &touched).is_err(),
        "a wrong backward must FAIL the complete symbolic dry-run (teeth)"
    );
}

/// TEETH on exec net-zero: a wrong backward leaves the ancilla dirty at runtime.
#[test]
fn bdd_wrong_backward_breaks_exec_netzero() {
    let fwd = nonlinear_fwd();
    let wrong = RegTransfer::identity();
    let carrier_ops = [0u8, 1];
    let ancilla_ops = [2u8, 3];
    let temp = 4u8;
    let nodes = [10u8, 11, 12, 13, 14];
    let helpers = BddHelpers { temp, nodes: &nodes };
    let body =
        transcribe_bdd_bennett(&fwd, &wrong, &carrier_ops, &ancilla_ops, helpers).unwrap();
    let mut st = zero_state();
    st.operand_set(0, 0x0F);
    st.operand_set(1, 0x03); // control fires ⇒ carriers change
    for &instr in &body {
        exec_instr(instr, &mut st);
    }
    let dirty = [2u8, 3].iter().any(|&a| st.operand_get(a) != 0);
    assert!(dirty, "a wrong backward must leave the ancilla dirty (exec net-zero teeth)");
}

// ── Guard regressions (carry MAJOR-1/2 from transcribe_bennett) ───────

#[test]
fn bdd_bennett_carrier_reads_noncarrier_errs() {
    let cnot = spec(|r, b| match r {
        0 => v(0, b).xor(v(2, b)), // reads reg 2 (non-carrier when n=2)
        _ => v(r, b),
    });
    let nodes = [10u8, 11, 12, 13, 14];
    let helpers = BddHelpers { temp: 4, nodes: &nodes };
    let res = transcribe_bdd_bennett(&cnot, &cnot, &[0, 1], &[2, 3], helpers);
    assert!(res.is_err(), "carrier reading a non-carrier must Err, got {res:?}");
}

#[test]
fn bdd_bennett_duplicate_ancilla_errs() {
    let nodes = [10u8, 11];
    let helpers = BddHelpers { temp: 4, nodes: &nodes };
    let res = transcribe_bdd_bennett(&swap_fwd(), &swap_fwd(), &[0, 1], &[3, 3], helpers);
    assert!(res.is_err(), "duplicate ancilla must Err, got {res:?}");
}

#[test]
fn bdd_bennett_helper_aliases_carrier_errs() {
    let nodes = [10u8, 11];
    let helpers = BddHelpers { temp: 0, nodes: &nodes }; // temp aliases carrier r0
    let res = transcribe_bdd_bennett(&swap_fwd(), &swap_fwd(), &[0, 1], &[2, 3], helpers);
    assert!(res.is_err(), "helper aliasing a carrier must Err, got {res:?}");
}

#[test]
fn bdd_bennett_node_aliases_temp_errs() {
    let nodes = [4u8, 11]; // node word 4 == temp
    let helpers = BddHelpers { temp: 4, nodes: &nodes };
    let res = transcribe_bdd_bennett(&swap_fwd(), &swap_fwd(), &[0, 1], &[2, 3], helpers);
    assert!(res.is_err(), "node word aliasing temp must Err, got {res:?}");
}

#[test]
fn bdd_bennett_reserved_helper_errs() {
    let nodes = [8u8, 9]; // s0/s1 decoder-reserved
    let helpers = BddHelpers { temp: 4, nodes: &nodes };
    let res = transcribe_bdd_bennett(&swap_fwd(), &swap_fwd(), &[0, 1], &[2, 3], helpers);
    assert!(res.is_err(), "reserved helper (s0/s1) must Err, got {res:?}");
}

#[test]
fn bdd_bennett_node_bank_too_small_errs() {
    // Nonlinear needs 5 node words; give 0 → must Err at synth.
    let nodes: [u8; 0] = [];
    let helpers = BddHelpers { temp: 4, nodes: &nodes };
    let res =
        transcribe_bdd_bennett(&nonlinear_fwd(), &nonlinear_fwd(), &[0, 1], &[2, 3], helpers);
    assert!(res.is_err(), "an undersized node bank must Err, got {res:?}");
}
