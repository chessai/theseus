// B-2.2c — derive `w_pc` per program from the compiled `Lowered` (B + S_max +
// rom_len) and MEASURE the win of the w_pc=8 datapath vs the default w_pc=16.
//
// - `derive_w_pc(budget, s_max, rom_len)` picks the MINIMUM safe PC width that
//   satisfies BOTH runtime guards: the `run_rom_on_cpu_with` address-space guard
//   (`budget + s_max <= 1<<w_pc`) and the `fetch_with` injectivity assert
//   (`rom_len <= 1<<(w_pc-1)`), rounded up to a whole `w_ctl`(=8) multiple and
//   clamped to {8,16}.
// - `derive_full_layout_from_lowered(iso, &lowered)` folds that into the full
//   per-program layout; `run_on_cpu` / `rom_program_energy_auto` route through it.
//
// The pure `derive_w_pc` boundary tests (no lowering) are ungated. The narrowing
// run + the WIN measurement lower/run full step nets and are `heavy-tests`-gated;
// the two GB-net lowerings are serialized behind a `Mutex` (concurrent lowerings
// OOM — mirrors autoderive_wdata.rs's `LOWER_GATE`).

use theseus_tisc::compile::derive_w_pc;

// ══════════════════════════════════════════════════════════════════════
// A. PURE derive_w_pc: the two bounds + the boundary off-by-one (no lowering)
// ══════════════════════════════════════════════════════════════════════

#[test]
fn derive_w_pc_address_space_bound() {
    // (1<<w_pc) >= budget + s_max. w_pc=8 handles reach up to 256 EXACTLY.
    assert_eq!(derive_w_pc(0, 0, 0), 8, "empty program floors at 8");
    assert_eq!(derive_w_pc(10, 0, 10), 8);
    assert_eq!(derive_w_pc(200, 55, 24), 8, "B+S=255 <= 256, rom small → 8");
    assert_eq!(derive_w_pc(250, 6, 24), 8, "B+S=256 == 256 fits w_pc=8 (1<<8=256) → 8");
    // B+S = 257 needs 9 bits → 16. THIS IS THE ADDRESS-SPACE OFF-BY-ONE.
    assert_eq!(derive_w_pc(251, 6, 24), 16, "B+S=257 > 256 → must widen to 16");
    assert_eq!(derive_w_pc(300, 0, 24), 16);
    assert_eq!(derive_w_pc(65000, 0, 24), 16, "huge B stays 16");
}

#[test]
fn derive_w_pc_fetch_injectivity_bound() {
    // rom_len <= 1<<(w_pc-1). At w_pc=8 that is rom_len <= 128.
    assert_eq!(derive_w_pc(10, 0, 128), 8, "rom_len=128 == 1<<7 fits w_pc=8 → 8");
    // rom_len = 129 needs 1<<(w-1) >= 129 → w-1>=8 → w=9 → 16. THE FETCH OFF-BY-ONE.
    assert_eq!(derive_w_pc(10, 0, 129), 16, "rom_len=129 > 128 → must widen to 16");
    assert_eq!(derive_w_pc(10, 0, 200), 16);
}

#[test]
fn derive_w_pc_takes_the_max_of_both_bounds() {
    // A program can be forced to 16 by EITHER bound independently.
    // Big reach, tiny rom → reach forces 16.
    assert_eq!(derive_w_pc(257, 0, 1), 16);
    // Tiny reach, big rom → fetch forces 16. (rom_len <= B always in practice, but
    // the derivation is defensive if a caller passes them independently.)
    assert_eq!(derive_w_pc(1, 0, 129), 16);
    // Both small → 8.
    assert_eq!(derive_w_pc(120, 8, 120), 8);
}

// The correctness crux, checked purely: for EVERY (B, S_max, rom_len) triple, the
// chosen w_pc satisfies BOTH runtime guards (so a run can never be mis-narrowed
// into a Deferred error / fetch panic). Swept densely around the 256 / 128 edges.
#[test]
fn derive_w_pc_never_violates_the_runtime_guards() {
    let w_ctl = 8usize;
    for reach in 0..=520usize {
        for &rom_len in &[0usize, 1, 63, 64, 127, 128, 129, 200, 255, 256, 300] {
            // Split reach into a plausible (budget, s_max) — s_max <= budget.
            let s_max = reach / 4;
            let budget = reach - s_max;
            let w = derive_w_pc(budget, s_max, rom_len);
            // Only 8 or 16, and always a whole w_ctl multiple.
            assert!(w == 8 || w == 16, "w_pc must be in {{8,16}}, got {w}");
            assert_eq!(w % w_ctl, 0, "w_pc must be a whole w_ctl multiple");
            // GUARD 1: address space. 1<<16 can overflow usize? no (fits). Use u128.
            let addr_space = 1u128 << w;
            assert!(
                (budget as u128 + s_max as u128) <= addr_space,
                "address-space guard violated: B+S={} > 2^{w}={} (would Deferred-fail run)",
                budget + s_max,
                addr_space
            );
            // GUARD 2: fetch injectivity rom_len <= 1<<(w-1).
            let fetch_half = 1u128 << (w - 1);
            assert!(
                rom_len as u128 <= fetch_half,
                "fetch guard violated: rom_len={rom_len} > 2^{}={} (would panic fetch_with)",
                w - 1,
                fetch_half
            );
        }
    }
}

// ══════════════════════════════════════════════════════════════════════
// B. HEAVY: narrowing runs correct at w_pc=8; large stays w_pc=16 + runs; THE WIN
// ══════════════════════════════════════════════════════════════════════

#[cfg(feature = "heavy-tests")]
mod heavy {
    use std::collections::BTreeMap;
    use std::sync::Mutex;

    use theseus_core::prim::{enumerate_values, PrimDef};
    use theseus_core::{Iso, Type};

    use theseus_netlist::lower_iso;
    use theseus_spice::chardb::rung0_measured;

    use theseus_tisc::asm::assemble;
    use theseus_tisc::compile::{
        compile_with_metadata, derive_full_layout_from_lowered, derive_w_pc, run_on_cpu,
        value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
    };
    use theseus_tisc::energy::balanced_energy;
    use theseus_tisc::types::Layout;

    const T_RAMP: f64 = 1e-7;

    /// Serialize the expensive full-step-net lowerings (concurrent lowerings OOM).
    static LOWER_GATE: Mutex<()> = Mutex::new(());

    /// A byte-wise `xor` prim over `Flat(n) × Flat(n)`: `(a,b) ↦ (a, a^b)`,
    /// self-inverse. Same shape as autoderive_wdata.rs's kernel.
    fn xor_prim(n: u8) -> PrimDef {
        let flat = Type::Flat(n);
        let ty = Type::prod(flat.clone(), flat.clone());
        let mask: u8 = if n >= 8 { 0xFF } else { (1u8 << n) - 1 };
        let mut forward = BTreeMap::new();
        let mut backward = BTreeMap::new();
        for v in enumerate_values(&ty) {
            let bytes = value_to_leaf_bytes_pub(&v, &ty);
            let (a, b) = (bytes[0], bytes[1]);
            let (out, _) = value_from_leaf_bytes_pub(&vec![a, (a ^ b) & mask], &ty);
            forward.insert(v.clone(), out.clone());
            backward.insert(v, out); // xor self-inverse
        }
        PrimDef {
            forward_name: "xorInto".into(),
            backward_name: "xorInto".into(),
            input_ty: ty.clone(),
            output_ty: ty,
            forward,
            backward,
            symbolic: None,
        }
    }

    /// A single-xor kernel: small B, small rom → narrows to w_pc=8.
    fn kernel_iso(n: u8) -> Iso {
        Iso::Prim(Box::new(xor_prim(n)))
    }

    /// A chain of `k` xor prims over `Flat(4)×Flat(4)` (in=out type, so it
    /// composes with itself). Emits ~k straight-line xorInto instructions, so a
    /// large `k` gives rom_len > 128 (and B > 128) → stays w_pc=16. Net effect =
    /// k applications of xor: out = (a, a^b) if k odd, (a, b) if k even.
    ///
    /// NOTE: `Flat(4)` (256-entry prim tables), NOT `Flat(8)` (2^16-entry): a
    /// long chain of `Flat(8)` prims builds ~k·2·65536 Value trees just to
    /// CONSTRUCT the iso and OOM-kills the test. `Flat(4)` keeps the table
    /// construction cheap while still emitting ~k instructions (rom_len is a
    /// property of the ROM length, independent of the data-word width).
    fn xor_chain_iso(k: usize) -> Iso {
        assert!(k >= 1);
        let mut iso = kernel_iso(4);
        for _ in 1..k {
            iso = Iso::seq(iso, kernel_iso(4));
        }
        iso
    }

    // ── B.1 Narrowing program auto-derives w_pc=8 + runs correct ──────────
    #[test]
    fn narrowing_program_derives_wpc8_and_runs() {
        let iso = kernel_iso(8);
        let lowered = compile_with_metadata(&iso).expect("compile xor kernel");
        let layout = derive_full_layout_from_lowered(&iso, &lowered);
        assert_eq!(
            layout.w_pc, 8,
            "small program (B={} S={} rom_len={}) must derive w_pc=8",
            lowered.wcet(),
            lowered.s_max(),
            lowered.instrs.len()
        );
        assert_eq!(layout.pc_fields(), 1, "w_pc=8 → exactly one PC field (no PC_hi)");

        // run_on_cpu (which now routes through derive_full_layout_from_lowered)
        // must agree with eval over the WHOLE Flat(8)×Flat(8) domain? that is
        // 2^16 — too big. Sample the edge bytes + a stride, incl. 0/0xFF/high-bit.
        let ty = Type::prod(Type::Flat(8), Type::Flat(8));
        let key: Vec<u8> = vec![0, 1, 0x0F, 0x55, 0xAA, 0x80, 0xC3, 127, 128, 254, 255];
        let mut checked = 0usize;
        for &a in &key {
            for &b in &key {
                let (v, _) = value_from_leaf_bytes_pub(&vec![a, b], &ty);
                let cpu_out = run_on_cpu(&iso, &v, theseus_eval::eval)
                    .unwrap_or_else(|e| panic!("run_on_cpu(w_pc=8) failed on ({a},{b}): {e}"));
                let mut fuel = 1_000_000u64;
                let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel)
                    .unwrap_or_else(|e| panic!("eval failed on ({a},{b}): {e:?}"));
                assert_eq!(cpu_out, eval_out, "w_pc=8 run != eval on ({a},{b})");
                checked += 1;
            }
        }
        assert!(checked >= 100, "checked {checked}");
        eprintln!("xor kernel auto-derives w_pc=8, run_on_cpu==eval on {checked} pairs ✓");
    }

    // ── B.2 Large program stays w_pc=16 + runs correct ────────────────────
    #[test]
    fn large_program_stays_wpc16_and_runs() {
        // 130 chained xors → rom_len ~130 > 128 → fetch bound forces w_pc=16.
        let k = 130usize;
        let iso = xor_chain_iso(k);
        let lowered = compile_with_metadata(&iso).expect("compile xor chain");
        let (b, s, rl) = (lowered.wcet(), lowered.s_max(), lowered.instrs.len());
        eprintln!("xor_chain(k={k}): B={b} S_max={s} rom_len={rl}");
        assert!(
            rl > 128 || b + s > 256,
            "chain must exceed a w_pc=8 bound (rom_len={rl}, B+S={})",
            b + s
        );
        let layout = derive_full_layout_from_lowered(&iso, &lowered);
        assert_eq!(layout.w_pc, 16, "large program must stay w_pc=16 (byte-identical path)");
        assert_eq!(layout.pc_fields(), 2, "w_pc=16 → two PC fields (PC_hi present)");

        // Runs correct: k=130 even ⇒ net xor applied 130 times ⇒ identity on b.
        // Flat(4) type (matches the chain): nibble keys. FEW pairs on purpose —
        // run_on_cpu here eval's the step iso ~130 times PER pair (a 130-entry
        // ROM), so each pair is ~seconds; the run is confirmatory (derivation is
        // proven exhaustively by the pure sweep; broad run-correctness by B-2.2b
        // + the narrowing test), so 4 edge pairs suffice. Bound the domain
        // (maestro-gate-no-timeout lesson).
        let ty = Type::prod(Type::Flat(4), Type::Flat(4));
        let key: Vec<u8> = vec![0x5, 0xA];
        let mut checked = 0usize;
        for &a in &key {
            for &b_in in &key {
                let (v, _) = value_from_leaf_bytes_pub(&vec![a, b_in], &ty);
                let cpu_out = run_on_cpu(&iso, &v, theseus_eval::eval)
                    .unwrap_or_else(|e| panic!("run_on_cpu(w_pc=16 large) failed ({a},{b_in}): {e}"));
                let mut fuel = 100_000_000u64;
                let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel)
                    .unwrap_or_else(|e| panic!("eval failed ({a},{b_in}): {e:?}"));
                assert_eq!(cpu_out, eval_out, "large-program run != eval on ({a},{b_in})");
                checked += 1;
            }
        }
        assert!(checked >= 4, "checked {checked}");
        eprintln!("xor_chain(k={k}) stays w_pc=16, run_on_cpu==eval on {checked} pairs ✓");
    }

    // ── B.3 Boundary RUN test: a program right at the edge RUNS at its w_pc ──
    // Pins that the derived w_pc actually runs without the run guard erroring or
    // the fetch assert panicking. We drive both sides of the rom_len=128 edge.
    #[test]
    fn boundary_derived_wpc_runs_without_guard_error() {
        // EXACTLY ON the fetch edge (Fable B-2.2c MINOR-3): 128 xors → rom_len ==
        // 128 == 1<<(w_pc-1) at w_pc=8, the SHARPEST w_pc=8 case (rom_len 129
        // would force 16). B=128 <= 256. Must derive 8 AND run clean — a real run
        // at the exact fetch-injectivity boundary, not just under it.
        let iso = xor_chain_iso(128);
        let lowered = compile_with_metadata(&iso).expect("compile");
        let (b, s, rl) = (lowered.wcet(), lowered.s_max(), lowered.instrs.len());
        eprintln!("boundary chain(128) [rom_len==128 fetch edge]: B={b} S_max={s} rom_len={rl}");
        assert_eq!(rl, 128, "chain(128) must give rom_len==128 (the exact fetch edge)");
        let layout = derive_full_layout_from_lowered(&iso, &lowered);
        assert_eq!(layout.w_pc, 8, "rom_len==128 (== 1<<7) must derive w_pc=8, the sharp edge");
        eprintln!("  → derive_w_pc={} (direct={})", layout.w_pc, derive_w_pc(b, s, rl));
        // Whatever w_pc is derived, it MUST run — that is the crux: a mis-narrowed
        // w_pc would Deferred-fail here or panic the fetch assert.
        // Flat(4) type (matches the chain): nibble operands.
        let ty = Type::prod(Type::Flat(4), Type::Flat(4));
        let (v, _) = value_from_leaf_bytes_pub(&vec![0x3, 0xA], &ty);
        let cpu_out = run_on_cpu(&iso, &v, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("boundary run failed at w_pc={}: {e}", layout.w_pc));
        let mut fuel = 100_000_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel).expect("eval");
        assert_eq!(cpu_out, eval_out, "boundary run != eval");

        // And confirm the guard arithmetic directly: if w_pc=8 was chosen, B+S<=256
        // AND rom_len<=128; if 16, at least one edge was exceeded.
        if layout.w_pc == 8 {
            assert!(b + s <= 256 && rl <= 128, "w_pc=8 chosen but B+S={} rl={rl}", b + s);
        }
        eprintln!("boundary chain(120) ran clean at w_pc={} ✓", layout.w_pc);
    }

    // ── B.4 THE WIN: w_pc=8 net has fewer balanced cells + lower step_j ───
    #[test]
    fn win_wpc8_step_net_shrinks_vs_wpc16() {
        let _guard = LOWER_GATE.lock().unwrap();
        let iso = kernel_iso(8);
        let lowered = compile_with_metadata(&iso).expect("compile");
        let rom = assemble(&lowered.instrs).expect("assemble");

        // The auto layout (should be w_pc=8) and the SAME layout forced to w_pc=16
        // — so w_pc is the ONLY difference (isolates the PC-width win).
        let auto = derive_full_layout_from_lowered(&iso, &lowered);
        assert_eq!(auto.w_pc, 8, "kernel must auto-derive w_pc=8");
        let wpc16 = Layout { w_pc: 16, ..auto };
        assert_eq!(wpc16.w_data, auto.w_data, "only w_pc differs");
        assert_eq!(wpc16.num_regs, auto.num_regs, "only w_pc differs");

        let net8 = lower_iso(&theseus_tisc::step::step_with(&auto, &rom)).expect("lower w_pc=8");
        let net16 = lower_iso(&theseus_tisc::step::step_with(&wpc16, &rom)).expect("lower w_pc=16");

        let db = rung0_measured();
        let e8 = balanced_energy(&net8, &db, T_RAMP).expect("price w_pc=8");
        let e16 = balanced_energy(&net16, &db, T_RAMP).expect("price w_pc=16");

        let (c8, c16) = (net8.cells.len(), net16.cells.len());
        let (sj8, sj16) = (e8.total_j, e16.total_j);
        let d_cells = 100.0 * (c16 as f64 - c8 as f64) / c16 as f64;
        let d_sj = 100.0 * (sj16 - sj8) / sj16;
        eprintln!(
            "THE WIN (w_pc 8 vs 16, only w_pc differs):\n\
             cells:  w_pc8={c8} w_pc16={c16} (Δ={d_cells:.2}%)\n\
             step_j: w_pc8={sj8:.6e} w_pc16={sj16:.6e} (Δ={d_sj:.2}%)"
        );

        assert!(c8 < c16, "w_pc=8 must have fewer cells: {c8} vs {c16}");
        assert!(sj8 < sj16, "w_pc=8 step_j must be lower: {sj8:.6e} vs {sj16:.6e}");
    }
}
