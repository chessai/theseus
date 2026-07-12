//! Generator for reversible-Fibonacci Theseus programs (shared between the
//! oracle test `fib_reversible.rs` and the `fib_energy` example).
//!
//! Construction (all COMPOSITIONAL — no monolithic multi-bit truth tables):
//!
//! * `ha4`  : 4-wire preserved-input half adder
//!   (a,b,s,c) <-> (a,b, s^(a^b), c^(a&b))            [16 clauses]
//! * `fa5`  : 5-wire preserved-input full adder (proven building block)
//!   (a,b,cin,s,c) <-> (a,b,cin, s^(a^b^cin), c^maj)  [32 clauses]
//! * routing: adjacent wire transpositions `t{k}` built ONLY from the
//!   structural primitives the elaborator special-cases:
//!   swbb    : (Bit,Bit)<->(Bit,Bit)      single clause (x,y)<->(y,x)   -> SwapProd
//!   asc{k}  : (Bit,(Bit,R))<->((Bit,Bit),R)  clause (x,(y,z))<->((x,y),z) -> AssocLProd
//!   idr{r}  : R<->R                      single clause w<->w           -> Id
//!   step{k} = asc{k} ; (swbb * idr{r}) ; ~asc{k}
//!   t{k}    = idb * (idb * ... * step{k})     (k leading ids)
//! * a ripple add A+B -> fresh sum register S with fresh carry chain K:
//!   stage 0:            ha4(A0, B0, s0, k0)
//!   stage 0 < i < wa:   fa5(Ai, Bi, k_{i-1}, si, ki)
//!   stage wa <= i < wb: ha4(Bi, k_{i-1}, si, ki)
//!   Each stage's wires are bubble-routed to the tuple tail, then the adder
//!   is applied as `idb * (idb * ... * (ha4|fa5))`.
//! * Fibonacci = unrolled chain of adds, Bennett-style: every fib term goes
//!   into a FRESH register; nothing is overwritten (all adder inputs are
//!   preserved).

use std::collections::BTreeSet;

pub fn bit_tuple_ty(n: usize) -> String {
    if n == 1 {
        "Bit".to_string()
    } else {
        format!("({})", vec!["Bit"; n].join(", "))
    }
}

/// `idb * (idb * ( ... * (inner)))` with `k` leading identities.
pub fn id_prefix(k: usize, inner: &str) -> String {
    let mut s = inner.to_string();
    for _ in 0..k {
        s = format!("idb * ({s})");
    }
    s
}

/// 16-clause preserved-input half adder.
pub fn ha4_source() -> String {
    let mut src = String::from(&format!("iso ha4 : {t} <-> {t}\n", t = bit_tuple_ty(4)));
    for v in 0u32..16 {
        let a = v & 1 == 1;
        let b = (v >> 1) & 1 == 1;
        let s = (v >> 2) & 1 == 1;
        let c = (v >> 3) & 1 == 1;
        let bs = |x: bool| if x { "I" } else { "O" };
        src.push_str(&format!(
            "| ({}, {}, {}, {}) <-> ({}, {}, {}, {})\n",
            bs(a),
            bs(b),
            bs(s),
            bs(c),
            bs(a),
            bs(b),
            bs(s ^ (a ^ b)),
            bs(c ^ (a & b)),
        ));
    }
    src
}

/// 32-clause preserved-input full adder (the proven fa5 building block).
pub fn fa5_source() -> String {
    let mut src = String::from(&format!("iso fa5 : {t} <-> {t}\n", t = bit_tuple_ty(5)));
    for v in 0u32..32 {
        let a = v & 1 == 1;
        let b = (v >> 1) & 1 == 1;
        let cin = (v >> 2) & 1 == 1;
        let s = (v >> 3) & 1 == 1;
        let c = (v >> 4) & 1 == 1;
        let sum = a ^ b ^ cin;
        let maj = (a & b) | (a & cin) | (b & cin);
        let bs = |x: bool| if x { "I" } else { "O" };
        src.push_str(&format!(
            "| ({}, {}, {}, {}, {}) <-> ({}, {}, {}, {}, {})\n",
            bs(a),
            bs(b),
            bs(cin),
            bs(s),
            bs(c),
            bs(a),
            bs(b),
            bs(cin),
            bs(s ^ sum),
            bs(c ^ maj),
        ));
    }
    src
}

/// Routing plan + emitted program for a fib chain.
pub struct FibProgram {
    pub source: String,
    pub top_name: String,
    /// wire name at each position of the iso INPUT tuple
    pub initial_layout: Vec<String>,
    /// wire name at each position of the iso OUTPUT tuple
    pub final_layout: Vec<String>,
    /// wire names of each register r1, r2, r3, ... (LSB first)
    pub regs: Vec<Vec<String>>,
    pub width: usize,
}

/// Bubble-route `wanted` (in order) to the tail of `layout`, appending the
/// transposition iso names to `seq` and recording used transposition indices.
fn route_to_tail(
    layout: &mut [String],
    wanted: &[String],
    seq: &mut Vec<String>,
    used_t: &mut BTreeSet<usize>,
) {
    let n = layout.len();
    let w = wanted.len();
    for i in (0..w).rev() {
        let target = n - w + i;
        let p = layout
            .iter()
            .position(|x| x == &wanted[i])
            .unwrap_or_else(|| panic!("wire {} not in layout", wanted[i]));
        assert!(
            p <= target,
            "bubble-routing invariant violated for {}",
            wanted[i]
        );
        for k in p..target {
            seq.push(format!("t{k}"));
            used_t.insert(k);
            layout.swap(k, k + 1);
        }
    }
}

/// Emit the full theseus program given the routed op sequence.
fn emit_program(n: usize, seq: &[String], used_t: &BTreeSet<usize>, top_name: &str) -> String {
    let tn = bit_tuple_ty(n);
    let mut src = String::from("type Bit = O | I\n\n");
    src.push_str("iso idb : Bit <-> Bit\n| O <-> O\n| I <-> I\n\n");
    src.push_str("iso swbb : (Bit, Bit) <-> (Bit, Bit)\n| (x, y) <-> (y, x)\n\n");

    // Identity isos on Bit-tuple tails, per needed width r.
    let mut needed_idr: BTreeSet<usize> = BTreeSet::new();
    for &k in used_t {
        if k + 2 < n {
            needed_idr.insert(n - k - 2);
        }
    }
    for &r in &needed_idr {
        src.push_str(&format!(
            "iso idr{r} : {t} <-> {t}\n| w <-> w\n\n",
            t = bit_tuple_ty(r)
        ));
    }

    // Transpositions.
    for &k in used_t {
        if k == n - 2 {
            src.push_str(&format!(
                "iso t{k} : {tn} <-> {tn}\n= {}\n\n",
                id_prefix(k, "swbb")
            ));
        } else {
            let r = n - k - 2; // tail width after the swapped pair
            let m = n - k; // wires from position k on
            let lhs = bit_tuple_ty(m);
            let rhs = format!("((Bit, Bit), {})", vec!["Bit"; r].join(", "));
            src.push_str(&format!(
                "iso asc{k} : {lhs} <-> {rhs}\n| (x, (y, z)) <-> ((x, y), z)\n\n"
            ));
            src.push_str(&format!(
                "iso step{k} : {lhs} <-> {lhs}\n= asc{k} ; (swbb * idr{r}) ; ~asc{k}\n\n"
            ));
            src.push_str(&format!(
                "iso t{k} : {tn} <-> {tn}\n= {}\n\n",
                id_prefix(k, &format!("step{k}"))
            ));
        }
    }

    // Adders + tail applications.
    src.push_str(&ha4_source());
    src.push('\n');
    src.push_str(&fa5_source());
    src.push('\n');
    src.push_str(&format!(
        "iso appha4 : {tn} <-> {tn}\n= {}\n\n",
        id_prefix(n - 4, "ha4")
    ));
    src.push_str(&format!(
        "iso appfa5 : {tn} <-> {tn}\n= {}\n\n",
        id_prefix(n - 5, "fa5")
    ));

    // Top-level chain.
    src.push_str(&format!(
        "iso {top_name} : {tn} <-> {tn}\n= {}\n",
        seq.join("\n; ")
    ));
    src
}

/// Build the full theseus program for `n_adds` unrolled Fibonacci additions
/// over uniform `width`-bit registers.
///
/// Registers: r1 = x (input), r2 = y (input), r_{j+2} = add j's sum register
/// (wires `s{j}_{i}`), with per-add carry chain `c{j}_{i}`.
/// Add j computes r_{j+2} = r_j + r_{j+1} into the fresh sum register.
pub fn build_fib_program(width: usize, n_adds: usize, top_name: &str) -> FibProgram {
    let w = width;
    // Register wire names, LSB first.
    let mut regs: Vec<Vec<String>> = vec![
        (0..w).map(|i| format!("x{i}")).collect(),
        (0..w).map(|i| format!("y{i}")).collect(),
    ];
    for j in 1..=n_adds {
        regs.push((0..w).map(|i| format!("s{j}_{i}")).collect());
    }
    let carries: Vec<Vec<String>> = (1..=n_adds)
        .map(|j| (0..w).map(|i| format!("c{j}_{i}")).collect())
        .collect();

    // Initial layout: x bits, y bits, then per add: s/c interleaved.
    let mut layout: Vec<String> = Vec::new();
    layout.extend(regs[0].iter().cloned());
    layout.extend(regs[1].iter().cloned());
    for j in 1..=n_adds {
        for i in 0..w {
            layout.push(regs[j + 1][i].clone());
            layout.push(carries[j - 1][i].clone());
        }
    }
    let n = layout.len();
    let initial_layout = layout.clone();

    // Emit the op sequence with routing.
    let mut seq: Vec<String> = Vec::new();
    let mut used_t: BTreeSet<usize> = BTreeSet::new();
    for j in 1..=n_adds {
        // add j: A = reg[j-1], B = reg[j], S = reg[j+1], C = carries[j-1]
        for i in 0..w {
            let (a, b) = (regs[j - 1][i].clone(), regs[j][i].clone());
            let s = regs[j + 1][i].clone();
            let c = carries[j - 1][i].clone();
            if i == 0 {
                route_to_tail(&mut layout, &[a, b, s, c], &mut seq, &mut used_t);
                seq.push("appha4".to_string());
            } else {
                let cin = carries[j - 1][i - 1].clone();
                route_to_tail(&mut layout, &[a, b, cin, s, c], &mut seq, &mut used_t);
                seq.push("appfa5".to_string());
            }
        }
    }
    let final_layout = layout;
    let source = emit_program(n, &seq, &used_t, top_name);

    FibProgram {
        source,
        top_name: top_name.to_string(),
        initial_layout,
        final_layout,
        regs,
        width: w,
    }
}

/// Build the MINIMAL-WIDTH fib(5) program: each register is exactly wide
/// enough to hold any carry-complete sum of its two predecessors, so the
/// whole machine is only 14 wires and the differential oracle can cover
/// the full 2^14 domain exhaustively.
///
/// Registers (LSB first):
///   r1 = [x0]                       1 bit   (fib 1 = 1)
///   r2 = [y0]                       1 bit   (fib 2 = 1)
///   r3 = [s1_0, k1_0]               2 bits  (fib 3 = 2)   r3 = r1 + r2
///   r4 = [s2_0, s2_1, k2_1]         3 bits  (fib 4 = 3)   r4 = r2 + r3
///   r5 = [s3_0, s3_1, s3_2, k3_2]   4 bits  (fib 5 = 5)   r5 = r3 + r4
/// Garbage carries: k2_0, k3_0, k3_1.
pub fn build_fib5_minwidth(top_name: &str) -> FibProgram {
    let mut regs: Vec<Vec<String>> = vec![vec!["x0".into()], vec!["y0".into()]];

    // Per-add sum/carry wire names; register j+2 assembled from them.
    let mut layout: Vec<String> = vec!["x0".into(), "y0".into()];
    let mut adds: Vec<(Vec<String>, Vec<String>)> = Vec::new(); // (s wires, k wires)
    for j in 1..=3usize {
        let wb = regs[j].len(); // wider operand's width
        let s: Vec<String> = (0..wb).map(|i| format!("s{j}_{i}")).collect();
        let k: Vec<String> = (0..wb).map(|i| format!("k{j}_{i}")).collect();
        for i in 0..wb {
            layout.push(s[i].clone());
            layout.push(k[i].clone());
        }
        let mut reg = s.clone();
        reg.push(k[wb - 1].clone());
        regs.push(reg);
        adds.push((s, k));
    }
    let n = layout.len();
    assert_eq!(n, 14);
    let initial_layout = layout.clone();

    let mut seq: Vec<String> = Vec::new();
    let mut used_t: BTreeSet<usize> = BTreeSet::new();
    for j in 1..=3usize {
        let (s, k) = adds[j - 1].clone();
        let a = regs[j - 1].clone(); // narrower operand (wa <= wb)
        let b = regs[j].clone();
        let (wa, wb) = (a.len(), b.len());
        for i in 0..wb {
            if i == 0 {
                route_to_tail(
                    &mut layout,
                    &[a[0].clone(), b[0].clone(), s[0].clone(), k[0].clone()],
                    &mut seq,
                    &mut used_t,
                );
                seq.push("appha4".to_string());
            } else if i < wa {
                route_to_tail(
                    &mut layout,
                    &[
                        a[i].clone(),
                        b[i].clone(),
                        k[i - 1].clone(),
                        s[i].clone(),
                        k[i].clone(),
                    ],
                    &mut seq,
                    &mut used_t,
                );
                seq.push("appfa5".to_string());
            } else {
                route_to_tail(
                    &mut layout,
                    &[b[i].clone(), k[i - 1].clone(), s[i].clone(), k[i].clone()],
                    &mut seq,
                    &mut used_t,
                );
                seq.push("appha4".to_string());
            }
        }
    }
    let final_layout = layout;
    let source = emit_program(n, &seq, &used_t, top_name);

    FibProgram {
        source,
        top_name: top_name.to_string(),
        initial_layout,
        final_layout,
        regs,
        width: 0,
    }
}
