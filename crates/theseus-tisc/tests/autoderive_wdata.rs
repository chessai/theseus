#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge.
//! B-2.1c auto-derive — make the reduced-`w_data` win APPLY automatically to a
//! narrow-data program, read off the SOURCE iso's `Type::Flat(n)`, with no
//! manual `Layout`.
//!
//! - `derive_full_layout(iso, instrs)` auto-derives `w_data = next_pow2(
//!   max_flat_width(iso))` (a Flat(4) program → w_data=4).
//! - THE WIN: the auto-derived step net (priced step_j / balanced decode
//!   cells) is smaller than the default (w_data=8) net — measured here.
//! - CORRECTNESS: `run_on_cpu == eval` on the Flat(4) kernel (now runs on the
//!   auto-selected w_data=4 ring); a Flat(8) program auto-derives w_data=8 and
//!   is byte-identical to the default (the `poc_step_net_bit_identical` fixture
//!   in energy.rs/b21c is the netlist guard — untouched by this front-end
//!   change).
//!
//! Run with `--test-threads=1`: lowers two full step nets (w_data=4 + w_data=8),
//! each ~GB; concurrent lowering exhausts memory.

use std::collections::BTreeMap;
use std::sync::Mutex;

use theseus_core::prim::{enumerate_values, PrimDef};
use theseus_core::{max_flat_width, Iso, Type};

use theseus_netlist::lower_iso;
use theseus_spice::chardb::rung0_measured;

use theseus_tisc::asm::assemble;
use theseus_tisc::compile::{
    compile_with_metadata, derive_full_layout, derive_full_layout_from_lowered, marshal, run_on_cpu,
    value_from_leaf_bytes_pub, value_to_leaf_bytes_pub,
};
use theseus_tisc::energy::{balanced_energy, rom_program_energy, rom_program_energy_auto};
use theseus_tisc::types::Layout;

const T_RAMP: f64 = 1e-7;

/// Serialize the expensive full-step-net lowerings (concurrent lowerings OOM).
static LOWER_GATE: Mutex<()> = Mutex::new(());

// ── Flat(n) kernel builders (a genuinely narrow-data program) ────────

/// A byte-wise binary op prim over `Flat(n) × Flat(n)`: `(a,b) ↦ (a, op(a,b))`.
/// Self-inverse iff `op` is (xor is; add is not — add's backward is sub).
fn binop_prim(n: u8, fwd: fn(u8, u8) -> u8, bwd: fn(u8, u8) -> u8, name: &str) -> PrimDef {
    let flat = Type::Flat(n);
    let ty = Type::prod(flat.clone(), flat.clone());
    let mask: u8 = if n >= 8 { 0xFF } else { (1u8 << n) - 1 };
    let mut forward = BTreeMap::new();
    let mut backward = BTreeMap::new();
    for v in enumerate_values(&ty) {
        let bytes = value_to_leaf_bytes_pub(&v, &ty);
        let (a, b) = (bytes[0], bytes[1]);
        let (fout, _) = value_from_leaf_bytes_pub(&vec![a, fwd(a, b) & mask], &ty);
        let (bout, _) = value_from_leaf_bytes_pub(&vec![a, bwd(a, b) & mask], &ty);
        forward.insert(v.clone(), fout);
        backward.insert(v, bout);
    }
    PrimDef {
        forward_name: name.into(),
        backward_name: name.into(),
        input_ty: ty.clone(),
        output_ty: ty,
        forward,
        backward,
        symbolic: None,
    }
}

/// `xor` prim: `(a,b) ↦ (a, a^b)`; self-inverse.
fn xor_prim(n: u8) -> PrimDef {
    binop_prim(n, |a, b| a ^ b, |a, b| a ^ b, "xorInto")
}

/// The Flat(n) kernel iso: a byte-wise `xor` over `Flat(n) × Flat(n)`
/// (`(a,b) ↦ (a, a^b)`). A genuinely narrow-data program exercising the xor
/// core. NOTE: `add` is deliberately NOT used — native `Add` is Flat(8)-only
/// (the Flat(w<8) `addInto` spec is rejected by the recognizer, per B-2.1c-2 /
/// MINOR-4), so a Flat(4) add falls to the over-budget general synth. The win
/// (a whole-datapath shrink) is independent of which instruction the ROM emits:
/// the step net decodes ALL ops every step, so w_data=4 shrinks the datapath
/// regardless.
fn kernel_iso(n: u8) -> Iso {
    Iso::Prim(Box::new(xor_prim(n)))
}

// ══════════════════════════════════════════════════════════════════════
// 1. AUTO-DERIVE: max_flat_width + derive_full_layout on the real kernel
// ══════════════════════════════════════════════════════════════════════

#[test]
fn kernel_flat4_auto_derives_w_data_4() {
    let iso = kernel_iso(4);
    assert_eq!(max_flat_width(&iso), 4, "Flat(4) kernel widest word = 4");
    let lowered = compile_with_metadata(&iso).expect("compile Flat(4) kernel");
    let layout = derive_full_layout(&iso, &lowered.instrs);
    assert_eq!(layout.w_data, 4, "w_data auto-derives to next_pow2(4)=4");
    assert_eq!(layout.w_ctl, 8, "w_ctl stays 8 (Option A)");
    eprintln!(
        "Flat(4) kernel: max_flat_width=4 → auto layout {{ w_data:{}, w_ctl:{}, num_regs:{} }}",
        layout.w_data, layout.w_ctl, layout.num_regs
    );
}

#[test]
fn kernel_flat8_auto_derives_w_data_8() {
    let iso = kernel_iso(8);
    assert_eq!(max_flat_width(&iso), 8, "Flat(8) kernel widest word = 8");
    let lowered = compile_with_metadata(&iso).expect("compile Flat(8) kernel");
    let layout = derive_full_layout(&iso, &lowered.instrs);
    assert_eq!(layout.w_data, 8, "w_data auto-derives to 8 (byte program)");
}

// ══════════════════════════════════════════════════════════════════════
// 2. THE WIN, APPLIED AUTOMATICALLY: priced step_j / decode cells shrink
// ══════════════════════════════════════════════════════════════════════
//
// Lower the step net at the AUTO-derived layout (w_data=4) vs the DEFAULT
// (w_data=8) and compare balanced decode cells + priced step_j. The Flat(4)
// datapath halves the ALU-core loops → fewer cells → lower step_j. This is the
// B-2.1c win now applied WITHOUT a manual Layout.

#[test]
fn win_flat4_step_net_shrinks_vs_default_auto() {
    let _guard = LOWER_GATE.lock().unwrap();
    let iso = kernel_iso(4);
    let lowered = compile_with_metadata(&iso).expect("compile Flat(4) kernel");
    let rom = assemble(&lowered.instrs).expect("assemble");

    // AUTO layout (w_data=4, num_regs auto, AND w_pc auto per B-2.2c — this small
    // kernel derives w_pc=8), and a w_data=8 variant with the SAME num_regs AND
    // w_pc so the ONLY difference is w_data — isolating the datapath-width win.
    // NOTE: use `derive_full_layout_from_lowered` (not `derive_full_layout`) so
    // this net matches what `rom_program_energy_auto` prices (it also derives
    // w_pc from the Lowered) — the MAJOR-2 equivalence assert below depends on it.
    let auto = derive_full_layout_from_lowered(&iso, &lowered);
    assert_eq!(auto.w_data, 4);
    let default_wd8 = Layout { w_data: 8, ..auto };

    let net_auto = lower_iso(&theseus_tisc::step::step_with(&auto, &rom)).expect("lower auto");
    let net_def = lower_iso(&theseus_tisc::step::step_with(&default_wd8, &rom)).expect("lower def");

    let db = rung0_measured();
    let e_auto = balanced_energy(&net_auto, &db, T_RAMP).expect("price auto");
    let e_def = balanced_energy(&net_def, &db, T_RAMP).expect("price def");

    let (cells_auto, cells_def) = (net_auto.cells.len(), net_def.cells.len());
    let (sj_auto, sj_def) = (e_auto.total_j, e_def.total_j);

    eprintln!(
        "THE WIN (auto vs default, same num_regs, w_data 4 vs 8):\n\
         cells: auto={cells_auto} default={cells_def} (Δ={:.1}%)\n\
         step_j: auto={sj_auto:.6e} default={sj_def:.6e} (Δ={:.1}%)",
        100.0 * (cells_def as f64 - cells_auto as f64) / cells_def as f64,
        100.0 * (sj_def - sj_auto) / sj_def,
    );

    assert!(
        cells_auto < cells_def,
        "auto (w_data=4) net must have fewer balanced cells than default (w_data=8): \
         {cells_auto} vs {cells_def}"
    );
    assert!(
        sj_auto < sj_def,
        "auto (w_data=4) step_j must be lower than default (w_data=8): {sj_auto:.6e} vs {sj_def:.6e}"
    );

    // Fable autoderive MAJOR-2: `rom_program_energy_auto` is the public convenience
    // that derives the per-program layout + lowers `step_with(layout, rom)` in one
    // call. Prove it prices at the SAME auto layout as the manual path lowered here
    // (`net_auto`): its `step_j` must equal `rom_program_energy(iso, .., net_auto)`.
    // This gives the wrapper real coverage (it was previously untested).
    let ty = Type::prod(Type::Flat(4), Type::Flat(4));
    let (input, _) = value_from_leaf_bytes_pub(&vec![0xA, 0x5], &ty);
    let pe_manual = rom_program_energy(&iso, &input, &db, T_RAMP, &net_auto)
        .expect("manual-auto rom_program_energy");
    let pe_conv =
        rom_program_energy_auto(&iso, &input, &db, T_RAMP).expect("rom_program_energy_auto");
    let rel = (pe_conv.step_j - pe_manual.step_j).abs() / pe_manual.step_j.abs();
    assert!(
        rel < 1e-12,
        "rom_program_energy_auto step_j ({:.6e}) must equal manual auto-layout pricing \
         ({:.6e}); rel_err={rel:.2e}",
        pe_conv.step_j,
        pe_manual.step_j
    );
    eprintln!(
        "rom_program_energy_auto == manual auto-layout pricing: step_j={:.6e} ✓",
        pe_conv.step_j
    );
}

// ══════════════════════════════════════════════════════════════════════
// 3. CORRECTNESS: run_on_cpu == eval on the Flat(4) kernel at auto-layout
// ══════════════════════════════════════════════════════════════════════
//
// run_on_cpu now auto-derives the layout, so a Flat(4) program runs on the
// w_data=4 ring. It must agree with the reference interpreter over the WHOLE
// Flat(4)×Flat(4) domain (256 inputs, incl. edge nibbles 0/0xF/high-bit).

#[test]
fn kernel_flat4_run_on_cpu_eq_eval_whole_domain() {
    let iso = kernel_iso(4);
    // Confirm run_on_cpu really routes to w_data=4.
    let lowered = compile_with_metadata(&iso).expect("compile");
    assert_eq!(derive_full_layout(&iso, &lowered.instrs).w_data, 4);

    let ty = Type::prod(Type::Flat(4), Type::Flat(4));
    let domain = enumerate_values(&ty);
    assert_eq!(domain.len(), 256, "Flat(4)² has 256 inputs");
    for v in &domain {
        let cpu_out = run_on_cpu(&iso, v, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("run_on_cpu (auto w_data=4) failed on {v:?}: {e}"));
        let mut fuel = 1_000_000u64;
        let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel)
            .unwrap_or_else(|e| panic!("eval failed on {v:?}: {e:?}"));
        assert_eq!(
            cpu_out, eval_out,
            "Flat(4) kernel run_on_cpu (auto w_data=4) != eval on {v:?}"
        );
    }
    eprintln!("Flat(4) kernel run_on_cpu==eval on all 256 inputs at AUTO w_data=4 ✓");
}

// ══════════════════════════════════════════════════════════════════════
// 4. Flat(8) program stays byte-identical: auto w_data=8, run_on_cpu==eval
// ══════════════════════════════════════════════════════════════════════
//
// A byte program auto-derives w_data=8, so run_on_cpu takes the byte ring —
// byte-identical to the default. (The netlist-fingerprint guard is
// `poc_step_net_bit_identical` in energy.rs / b21c_data_word.rs, run in the same
// heavy suite; this test confirms the run oracle still agrees with eval at 8.)

#[test]
fn kernel_flat8_run_on_cpu_eq_eval_strided() {
    let iso = kernel_iso(8);
    let lowered = compile_with_metadata(&iso).expect("compile");
    assert_eq!(
        derive_full_layout(&iso, &lowered.instrs).w_data,
        8,
        "byte program auto-derives w_data=8"
    );

    let ty = Type::prod(Type::Flat(8), Type::Flat(8));
    let key: Vec<u8> = vec![0, 1, 0x55, 0xAA, 0xC3, 127, 128, 254, 255];
    let mut checked = 0usize;
    for &a in &key {
        for &b in &key {
            let (v, _) = value_from_leaf_bytes_pub(&vec![a, b], &ty);
            let cpu_out = run_on_cpu(&iso, &v, theseus_eval::eval)
                .unwrap_or_else(|e| panic!("run_on_cpu(byte) failed on ({a},{b}): {e}"));
            let mut fuel = 1_000_000u64;
            let eval_out = theseus_eval::eval(&iso, v.clone(), &mut fuel)
                .unwrap_or_else(|e| panic!("eval failed on ({a},{b}): {e:?}"));
            assert_eq!(
                cpu_out, eval_out,
                "Flat(8) kernel run_on_cpu(auto w_data=8) != eval on ({a},{b})"
            );
            checked += 1;
        }
    }
    assert!(checked >= 36, "checked {checked} byte pairs");
    eprintln!("Flat(8) kernel run_on_cpu==eval on {checked} byte pairs at AUTO w_data=8 ✓");
}

// A sanity marshal-path pin: the auto-layout marshal of a Flat(4) value keeps
// the nibble in range (no width leak into a byte field).
#[test]
fn flat4_marshal_stays_in_nibble() {
    let iso = kernel_iso(4);
    let (in_ty, _) = theseus_core::type_of(&iso).unwrap();
    let ty = Type::prod(Type::Flat(4), Type::Flat(4));
    let (v, _) = value_from_leaf_bytes_pub(&vec![0xF, 0xF], &ty);
    let st = marshal(&v, &in_ty);
    assert!(st.regs[0] <= 0xF && st.regs[1] <= 0xF, "Flat(4) nibbles marshal < 16");
    let _ = (max_flat_width(&iso),); // keep import used across cfgs
}

// ── BLOCKER / MAJOR regression tests (Fable autoderive review) ───────

/// Fable autoderive BLOCKER-1: the compiler mints CONTROL into general
/// registers (a Trace's come-from δu ≥ 0x80, forward-skip distances, via
/// Addi+Swapbr). At w_data<8 the reduced `addi_core` masks the immediate and
/// `swapbr` swaps only w_data BR bits → the 8-bit control offset is truncated
/// and the loop/skip diverges. So a program whose ROM contains ANY Swapbr must
/// keep the full data-word (w_data=8), even with narrow SOURCE data. Reproduces
/// the misroute: without the fix, `derive_full_layout` returns w_data=4 for a
/// Flat(4) iso + Swapbr ROM.
#[test]
fn blocker1_swapbr_program_pins_full_w_data() {
    use theseus_tisc::asm::Instr;
    let iso = kernel_iso(4); // Flat(4) source data → max_flat_width = 4
    assert_eq!(max_flat_width(&iso), 4);
    // A ROM staging control in a register (big Addi immediate + Swapbr) — the
    // shape Trace / forward-skip emit.
    let control_rom = vec![
        Instr::Addi { rd: 0, imm: 0xF6 }, // δu-style control byte (≥ 0x80)
        Instr::Swapbr { rd: 0 },
        Instr::Swapbr { rd: 0 },
    ];
    let layout = derive_full_layout(&iso, &control_rom);
    assert_eq!(
        layout.w_data, 8,
        "Swapbr (control-in-registers) program must keep w_data=8, not narrow to \
         {} and truncate the 8-bit control offset",
        layout.w_data
    );
}

/// Fable autoderive BLOCKER-2: marshal fills registers by INPUT LEAF, so an
/// UNTOUCHED input leaf (an `Id`/`Times` component not named by any instr)
/// occupies an unnamed register. `num_regs` must cover the input-leaf count, or
/// `encode_with` drops (and asserts-nonzero-panic on) a live leaf. `Id(Flat4 ×
/// Flat4)` lowers to 0 instrs → the named-reg bound gives num_regs=1, but there
/// are 2 input leaves. Reproduces the panic (without the fix) on any nonzero
/// second leaf.
#[test]
fn blocker2_untouched_input_leaf_covered_by_num_regs() {
    let ty = Type::prod(Type::Flat(4), Type::Flat(4));
    let iso = Iso::Id(ty.clone());
    let lowered = compile_with_metadata(&iso).expect("compile Id(Flat4×Flat4)");
    let layout = derive_full_layout(&iso, &lowered.instrs);
    assert!(
        layout.num_regs >= 2,
        "num_regs must cover the 2 input leaves (marshal fills regs by leaf), got {}",
        layout.num_regs
    );
    // And it must RUN (identity) without panicking on a nonzero second leaf.
    for v in enumerate_values(&ty) {
        let out = run_on_cpu(&iso, &v, theseus_eval::eval)
            .unwrap_or_else(|e| panic!("run_on_cpu on Id(Flat4×Flat4) failed for {v:?}: {e}"));
        assert_eq!(out, v, "Id must return its input unchanged (auto layout)");
    }
}

/// Fable autoderive MAJOR-1: the reduced datapath is equiv-verified only at
/// w_data ∈ {4,8} (B-2.1c/c-2). A Flat(2) program must FLOOR w_data at 4 (not
/// use the never-verified w_data=2).
#[test]
fn major1_narrow_flat_floors_w_data_at_4() {
    let iso = kernel_iso(2); // Flat(2) source data → max_flat_width = 2
    assert_eq!(max_flat_width(&iso), 2);
    let lowered = compile_with_metadata(&iso).expect("compile Flat(2) kernel");
    let layout = derive_full_layout(&iso, &lowered.instrs);
    assert_eq!(
        layout.w_data, 4,
        "Flat(2) must floor w_data at 4 (widths <4 are not equiv-verified), got {}",
        layout.w_data
    );
}
