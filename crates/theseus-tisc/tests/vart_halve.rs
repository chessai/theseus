//! FIRST REAL variable-iteration-count program on the reversible CPU.
//!
//! Every prior `Trace` (reversible while-loop) test — `mde_spill.rs`,
//! `exact_kmax.rs`, the A-3/A-4/A-5 compose tests — wraps a SYNTHETIC walk body
//! (`mde_walk_bytes`, a pinned thermometer bijection) that computes nothing
//! meaningful; the loop machinery is exercised but the program is a
//! "machinery-proof, not a useful program" (docs/scratch/mde-variable-t-scoping.md).
//!
//! This is the first Trace whose body computes a genuine, named arithmetic
//! quantity: **halve-until-odd**, i.e. the **2-adic valuation** `v_2(n)` (the
//! number of trailing zero bits of the input). Its iteration count is
//! DATA-DEPENDENT — it equals the trailing-zero count — so `T` genuinely VARIES
//! with the input, not a fixed unrolled bound.
//!
//! ## The reversible encoding (why it lowers)
//!
//! A naive "while even: n >>= 1; c += 1" is NOT reversible (many inputs converge
//! to the same odd residue). We make it a `Trace` body by encoding the step as a
//! **manifestly-bijective cyclic bit-rotation** of the whole `(tag ‖ h ‖ v)`
//! carrier register (the `mde_walk` idiom: build the step from a bit-permutation,
//! not a table we hope is injective):
//!
//!   carrier = Sum(u, u),  u = Flat(K) × Flat(HB)   (leaf order [tag, v, h])
//!   step: right-rotate the (1+HB+K)-bit word `w = (tag<<(HB+K)) | (h<<K) | v` by 1.
//!
//! A cyclic rotation is a bijection unconditionally, so the full `(tag,v,h)` map
//! is injective by construction (verified whole-domain in `build_v2_body`). The
//! `tag` slot receives the bit that wrapped around = `v`'s old LSB, so the loop
//! EXITS (`tag'=1`, `Right`) exactly when an odd (LSB=1) bit surfaces and
//! CONTINUES (`tag'=0`, `Left`) while `v` is still even. Walking `v`'s low bit up
//! into the history register `h`, one rotation per even-halving, until an odd bit
//! surfaces = counting trailing zeros = the 2-adic valuation.
//!
//! ## The variable-T semantic claim (see `vart_variable_t_proof`)
//!
//! For every reachable input value `v0 ≥ 1`:
//!     T(v0) = v_2(v0) + 1
//! The `+1` is the terminal step that surfaces the odd bit and flips the tag to
//! exit. For K=3 this gives T ∈ {1,2,3} across v0 ∈ 1..8 — genuinely variable.
//! `v0 = 0` has no trailing-one bit (v_2 undefined); the rotation parks a full
//! period until the lone tag bit cycles back, an honest reversible fixed point.
//!
//! NOTE ON SEMANTICS: the *iteration count* `T` carries the v_2 meaning (the
//! number of even-halvings before an odd bit surfaces). The exit *payload*
//! (v,h) is the rotated register window, NOT `v0 >> v_2` — the rotation is
//! cyclic over the whole carrier, so bits move around the window rather than
//! literally shifting out. This test's semantic claim is about `T`, the number
//! of loop iterations, which is the observable that makes this a variable-T
//! program.
//!
//! ## Test tiers (mirrors mde_spill.rs)
//!   T1 (always-on): lowers + pinned ROM shape (B/iter_len/k_max/leaves).
//!   T2 (always-on): run_on_cpu_fast == eval over the ENTIRE input domain.
//!   variable-T proof (always-on): per-input T table; T varies AND T == v_2+1.
//!   T3 (#[ignore], advisor-run): real run_on_cpu (system_step ROM) == eval on a
//!       SAMPLE covering distinct T values + scratch-clean terminal.
//!   T-reject (always-on): a carrier one bit wider is rejected by iter_len>128.

use std::collections::BTreeMap;
use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{Iso, Type};
use theseus_tisc::compile::{
    compile_with_metadata, marshal, run_on_cpu_fast, run_rom_on_cpu, unmarshal,
    value_to_leaf_bytes_pub,
};
use theseus_tisc::types::{StateVals, MEM_WORDS, NUM_REGS, NUM_SCRATCH};

// ── Carrier size ────────────────────────────────────────────────────
// K=3 (v ∈ 0..8) gives a RICH variable-T domain T ∈ {1,2,3} while lowering with
// margin: iter_len=107 ≤ 128 (the come-from cap), node_lines=1 (linear body).
// HB=1 keeps the carrier at exactly the fit boundary (1+HB+K = 5-bit window).
const V2_K: u8 = 3;
const V2_HB: u8 = 1;

fn state_with_regs(bytes: &[u8]) -> StateVals {
    let mut r = [0u8; NUM_REGS];
    r[..bytes.len()].copy_from_slice(bytes);
    StateVals {
        regs: r,
        pc: 0,
        br: 0,
        ir: 0,
        scratch: [0; NUM_SCRATCH],
        mem: [0; MEM_WORDS],
    }
}

/// One reversible step: right-rotate the (1+HB+K)-bit `(tag ‖ h ‖ v)` register
/// by 1. A cyclic bit-permutation, hence a bijection over the whole carrier by
/// construction (parameterised so the reject test can use a wider carrier).
fn rot_step(tag: u8, v: u8, h: u8, k: u8, hb: u8) -> (u8, u8, u8) {
    let width = (1 + hb + k) as u32;
    let w: u32 = ((tag as u32) << (hb + k)) | ((h as u32) << k) | (v as u32);
    let lsb = w & 1;
    let wp = (w >> 1) | (lsb << (width - 1)); // right-rotate by 1
    let vmask = (1u32 << k) - 1;
    let hmask = (1u32 << hb) - 1;
    let vp = (wp & vmask) as u8;
    let hp = ((wp >> k) & hmask) as u8;
    let nt = (wp >> (hb + k)) as u8; // top bit → new tag (exit iff 1)
    (nt, vp, hp)
}

fn v2_step(tag: u8, v: u8, h: u8) -> (u8, u8, u8) {
    rot_step(tag, v, h, V2_K, V2_HB)
}

/// Build the halve-until-odd body as an `Iso::Prim`, verifying whole-domain
/// bijectivity + canonical closure (panics on the exact defect otherwise).
fn build_v2_body_sized(k: u8, hb: u8) -> (Iso, Type, Type) {
    let u = Type::prod(Type::flat(k), Type::flat(hb));
    let carrier = Type::sum(u.clone(), u.clone());
    let vals = enumerate_values(&carrier);
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    let mut noncanon = 0usize;
    for val in &vals {
        let sv = marshal(val, &carrier);
        let (nt, nv, nh) = rot_step(sv.regs[0], sv.regs[1], sv.regs[2], k, hb);
        let bytes = [nt, nv, nh];
        let out_val = unmarshal(&state_with_regs(&bytes), &carrier);
        // Closure: the byte output must re-marshal to the same bytes.
        let re = marshal(&out_val, &carrier);
        if re.regs[..3] != bytes[..] {
            noncanon += 1;
        }
        forward.insert(val.clone(), out_val.clone());
        if backward.insert(out_val, val.clone()).is_some() {
            panic!("v2 body NOT injective at k={k},hb={hb} on {val:?} (out {bytes:?})");
        }
    }
    assert_eq!(noncanon, 0, "v2 body must close on canonical encodings");
    assert_eq!(forward.len(), vals.len(), "v2 forward must be total");
    let prim = PrimDef {
        forward_name: format!("v2_halve_until_odd_k{k}_hb{hb}"),
        backward_name: format!("v2_halve_until_odd_k{k}_hb{hb}_inv"),
        input_ty: carrier.clone(),
        output_ty: carrier.clone(),
        forward,
        backward,
        symbolic: None,
    };
    (Iso::Prim(Box::new(prim)), u, carrier)
}

fn build_v2_body() -> (Iso, Type, Type) {
    build_v2_body_sized(V2_K, V2_HB)
}

/// v_2(n): number of trailing zero bits. None for 0 (undefined).
fn v2_of(n: u8) -> Option<u32> {
    if n == 0 {
        None
    } else {
        Some(n.trailing_zeros())
    }
}

/// Iteration count T for an entry value `(v0, h0)` — simulate the Trace loop
/// (enter `Right`, tag=1) by direct byte stepping until the tag flips back to 1.
fn v2_iter_count(v0: u8, h0: u8) -> usize {
    let (mut tag, mut v, mut h) = (1u8, v0, h0);
    for step in 1..=1000 {
        let (nt, nv, nh) = v2_step(tag, v, h);
        if nt == 1 {
            return step;
        }
        tag = nt;
        v = nv;
        h = nh;
    }
    panic!("v2 loop did not terminate for (v0={v0}, h0={h0})");
}

/// Decompose an input `u`-value into (v0, h0) leaf bytes (leaf order [v, h]).
fn input_vh(v: &theseus_core::Value, u_ty: &Type) -> (u8, u8) {
    let bytes = value_to_leaf_bytes_pub(v, u_ty);
    (bytes[0], *bytes.get(1).unwrap_or(&0))
}

// ── T1: it lowers + pinned ROM shape (always-on) ────────────────────

#[test]
fn vart_t1_rom_shape() {
    let (body, u_ty, carrier) = build_v2_body();
    let iso = Iso::trace(u_ty, body);

    let lowered = compile_with_metadata(&iso).expect("halve-until-odd Trace must lower");
    let rom_len = lowered.instrs.len();
    let b = lowered.wcet();
    let lt = &lowered.loop_terms[0];
    let iter_len = lt.iter_len;
    let k_max = lt.k_max;

    eprintln!("=== T1 ROM shape (K={V2_K}, HB={V2_HB}) ===");
    eprintln!("carrier      = {carrier}");
    eprintln!("ROM instrs   = {rom_len}");
    eprintln!("B (wcet)     = {b}");
    eprintln!("iter_len     = {iter_len}");
    eprintln!("k_max        = {k_max}");

    // Pinned ROM shape (probe-measured; document the numbers):
    //   K=3, HB=1 → carrier 3 leaves [tag,v,h], iter_len=107, B=646, k_max=6,
    //   node_lines=1 (linear body), rom_len=111.
    assert_eq!(rom_len, 111, "ROM straight-line length pinned");
    assert_eq!(b, 646, "WCET pinned");
    assert_eq!(iter_len, 107, "per-iteration come-from span pinned");
    assert_eq!(k_max, 6, "worst-case iteration count pinned");
    // Margin against the iter_len ≤ 128 come-from cap.
    assert!(iter_len <= 128, "iter_len must fit the come-from cap");

    // Carrier leaf count = 3 (tag + v + h), single-leaf Flat words.
    let carrier_leaves = value_to_leaf_bytes_pub(&enumerate_values(&carrier)[0], &carrier).len();
    assert_eq!(carrier_leaves, 3, "carrier is 3 leaves [tag, v, h]");
}

// ── T2: fast oracle over the ENTIRE input domain (always-on) ────────

#[test]
fn vart_t2_fast_oracle_full_domain() {
    let (body, u_ty, _carrier) = build_v2_body();
    let iso = Iso::trace(u_ty.clone(), body);

    let mut checked = 0usize;
    for v in enumerate_values(&u_ty) {
        let cpu = run_on_cpu_fast(&iso, &v).expect("fast run");
        let mut fuel = 10_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu, ev, "fast-oracle disagrees with eval on {v:?}");
        checked += 1;
    }
    eprintln!("=== T2 fast oracle ===");
    eprintln!("run_on_cpu_fast == eval on ALL {checked} inputs");
    assert_eq!(checked, (1usize << V2_K) * (1usize << V2_HB), "full domain");
}

// ── variable-T PROOF: T varies AND T == v_2(v0)+1 (always-on) ───────

#[test]
fn vart_variable_t_proof() {
    let (_body, u_ty, _carrier) = build_v2_body();

    eprintln!("=== variable-T proof (K={V2_K}, HB={V2_HB}) ===");
    eprintln!("  input (v0,h0) |  T  | v_2(v0)+1 | claim");
    let mut ts_reachable = Vec::new();
    for v in enumerate_values(&u_ty) {
        let (v0, h0) = input_vh(&v, &u_ty);
        let t = v2_iter_count(v0, h0);
        match v2_of(v0) {
            Some(nu) => {
                let claim = nu as usize + 1;
                eprintln!("  ({v0},{h0})         |  {t}  |    {claim}      | T == v_2+1");
                assert_eq!(
                    t, claim,
                    "T({v0},{h0}) must equal v_2({v0})+1 = {claim} (got {t})"
                );
                ts_reachable.push(t);
            }
            None => {
                // v0 == 0: no odd bit; the rotation parks a full period.
                eprintln!("  ({v0},{h0})         |  {t}  |    n/a    | v0=0 parks (v_2 undefined)");
            }
        }
    }

    // (a) T genuinely varies across the reachable (v0 ≥ 1) domain.
    let tmin = *ts_reachable.iter().min().unwrap();
    let tmax = *ts_reachable.iter().max().unwrap();
    eprintln!("reachable T range = [{tmin}, {tmax}] (variable-T iff min != max)");
    assert!(
        tmax > tmin,
        "iteration count MUST vary with input (this is what makes it variable-T, not a fixed unroll)"
    );
    // K=3 exercises v_2 ∈ {0,1,2} → T ∈ {1,2,3}.
    assert_eq!(tmin, 1, "odd inputs exit in one step (v_2=0 → T=1)");
    assert_eq!(tmax, 3, "v0=4 (v_2=2) takes three steps (T=3)");
}

// ── T3: REAL system_step ROM == eval + terminal quiescence ──────────
//     ADVISOR-RUN (#[ignore], heavy). Sampled across distinct T values; do NOT
//     run the full domain (per the maestro-gate-no-timeout lesson).

#[test]
#[ignore = "authoritative real-CPU oracle (slow); advisor runs pre-merge"]
fn vart_t3_real_cpu_sampled() {
    let (body, u_ty, _carrier) = build_v2_body();
    let iso = Iso::trace(u_ty.clone(), body);

    let lowered = compile_with_metadata(&iso).expect("compile");
    let budget = lowered.wcet();
    let s_max_val = lowered.s_max();
    let (in_ty, out_ty) = theseus_core::type_of(&iso).unwrap();

    // SAMPLE covering distinct iteration counts T ∈ {1, 2, 3}:
    //   v0=1 (odd)      → T=1
    //   v0=2 (v_2=1)    → T=2
    //   v0=4 (v_2=2)    → T=3
    // Pick the h0=0 representative for each. Small, deliberate sample: the real
    // run is seconds-to-minutes PER input.
    let sample_v0: [u8; 3] = [1, 2, 4];
    let inputs: Vec<theseus_core::Value> = enumerate_values(&u_ty)
        .into_iter()
        .filter(|v| {
            let (v0, h0) = input_vh(v, &u_ty);
            h0 == 0 && sample_v0.contains(&v0)
        })
        .collect();
    assert_eq!(inputs.len(), 3, "expected one representative per T value");

    for v in inputs {
        let (v0, _h0) = input_vh(&v, &u_ty);
        let t = v2_iter_count(v0, 0);
        eprintln!("real-cpu: v0={v0} (expected T={t}) running ...");

        // run_rom_on_cpu to inspect the FULL terminal state (quiescence checks).
        let state = marshal(&v, &in_ty);
        let final_state =
            run_rom_on_cpu(&lowered.instrs, state, budget, s_max_val, theseus_eval::eval)
                .expect("real-cpu run");

        let cpu_out = unmarshal(&final_state, &out_ty);
        let mut fuel = 1_000_000u64;
        let ev = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu_out, ev, "REAL CPU disagrees with eval on v0={v0}");
        eprintln!("real:  v0={v0} -> {cpu_out:?}  == eval OK");

        // Terminal quiescence: scratch clean + BR settled at the park convention.
        assert_eq!(
            final_state.scratch, [0u8; NUM_SCRATCH],
            "scratch must be all-zero at exit (paging/aux leak check): {:?}",
            final_state.scratch
        );
        assert_eq!(final_state.br, 0, "BR must be zero at loop exit (park convention)");
    }
}

// ── T-reject: one bit wider (K=4,HB=1) exceeds iter_len ≤ 128 ────────

#[test]
fn vart_k4_now_lowers_at_wbr16() {
    // W_BR-b: K=4,HB=1 (narrow iter_len = body+10 = 133 > 128) previously
    // REJECTED. It now LOWERS via the two-byte (w_br=16) come-from — iter_len =
    // body + LOOP_OVERHEAD_WIDE (14) = 137 (MINOR-C: NOT 133). This flips the
    // former reject→run WITHOUT weakening any guard: the narrow w_br=8 cap
    // (iter_len ≤ 128) is untouched; the loop is lifted to w_br=16 whose cap is
    // 32768. The full authority (fast oracle + real CPU) is in
    // `wbr_b_comefrom.rs`.
    let (body, u_ty, _carrier) = build_v2_body_sized(4, 1);
    let iso = Iso::trace(u_ty, body);
    let lowered =
        compile_with_metadata(&iso).expect("K=4,HB=1 must LOWER at w_br=16 (W_BR-b)");
    assert_eq!(lowered.w_br, 16, "K=4 lifts to the two-byte come-from");
    assert_eq!(
        lowered.loop_terms[0].iter_len, 137,
        "iter_len = body + LOOP_OVERHEAD_WIDE(14) = 137 (NOT 133)"
    );
    eprintln!("K=4,HB=1 now lowers at w_br=16, iter_len=137 ✓");
}
