//! Energy-pricing tests for compiled iter/fold/unfold programs (M3b → v0.2).
//!
//! Constructs concrete iter and fold programs, lowers them via
//! `theseus_netlist::lower_iso`, and prices them with the existing
//! `theseus_tisc::energy` machinery (balanced_energy + rung0_measured
//! char DB at t_ramp = 1e-7). Asserts positive energy with a sensible
//! cell-kind breakdown and prints per-primitive energy tables.

use std::collections::BTreeMap;
use std::sync::OnceLock;

use theseus_core::{Iso, PrimDef, Type, Value};
use theseus_netlist::{lower_iso, CellKind};

use theseus_spice::chardb::{rung0_measured, CharDb};

use theseus_tisc::energy::{balanced_energy, EnergyBreakdown};

const T_RAMP: f64 = 1e-7;

fn char_db() -> &'static CharDb {
    static DB: OnceLock<CharDb> = OnceLock::new();
    DB.get_or_init(rung0_measured)
}

// ── Type / iso helpers (mirrored from iter_lowering.rs) ─────────────

fn bit() -> Type {
    Type::sum(Type::One, Type::One)
}

fn three_ty() -> Type {
    Type::sum(Type::One, bit())
}

/// `not : Bit ↔ Bit` — swap the two sum branches.
fn not_iso() -> Iso {
    Iso::SwapSum(Type::One, Type::One)
}

/// `rot3 : Three ↔ Three`, successor mod 3 — NOT self-inverse.
fn rot3() -> Iso {
    let t0 = Value::left(Value::Unit);
    let t1 = Value::right(Value::left(Value::Unit));
    let t2 = Value::right(Value::right(Value::Unit));

    let mut fwd = BTreeMap::new();
    fwd.insert(t0.clone(), t1.clone());
    fwd.insert(t1.clone(), t2.clone());
    fwd.insert(t2.clone(), t0.clone());
    let mut bwd = BTreeMap::new();
    bwd.insert(t1.clone(), t0.clone());
    bwd.insert(t2.clone(), t1);
    bwd.insert(t0, t2);

    Iso::Prim(Box::new(PrimDef {
        forward_name: "rot3".into(),
        backward_name: "rot3_inv".into(),
        input_ty: three_ty(),
        output_ty: three_ty(),
        forward: fwd,
        backward: bwd,
    }))
}

/// `cx : Bit × Bit ↔ Bit × Bit` (CNOT).
fn cx() -> Iso {
    let ty = Type::prod(bit(), bit());
    let def = theseus_netlist::prim_from_bits("cx", ty.clone(), ty, |b| vec![b[0], b[0] ^ b[1]])
        .expect("cx prim");
    Iso::Prim(Box::new(def))
}

/// `cx ; swap` — non-self-inverse, order-sensitive body for fold.
fn cx_swap() -> Iso {
    Iso::seq(cx(), Iso::SwapProd(bit(), bit()))
}

// ── Helpers ─────────────────────────────────────────────────────────

/// Lower an iso and price it, returning the breakdown.
fn price_iso(iso: &Iso, db: &CharDb) -> EnergyBreakdown {
    let net = lower_iso(iso).expect("lower_iso succeeds");
    balanced_energy(&net, db, T_RAMP).expect("balanced_energy succeeds")
}

/// Assert the breakdown is positive with a sensible structure.
fn assert_sane_breakdown(bd: &EnergyBreakdown, label: &str) {
    assert!(bd.total_j > 0.0, "{label}: total energy must be positive");
    assert!(bd.active_j > 0.0, "{label}: active energy must be positive");
    assert!(bd.buffer_j >= 0.0, "{label}: buffer energy must be non-negative");
    let diff = (bd.total_j - (bd.active_j + bd.buffer_j)).abs();
    assert!(
        diff < 1e-30,
        "{label}: total ({:.4e}) must equal active ({:.4e}) + buffer ({:.4e}), diff={diff:.4e}",
        bd.total_j,
        bd.active_j,
        bd.buffer_j,
    );
    assert!(!bd.by_kind.is_empty(), "{label}: breakdown must have entries");

    // Must have at least one active cell kind (K1, K2, K3, or R1 — the
    // ADR-0040 rail-role-swap cell, e.g. a NOT lowers to R1).
    let has_active = bd
        .by_kind
        .keys()
        .any(|k| (k.starts_with("cswap-k") || k.starts_with("cswap-r")) && !k.contains("balance"));
    assert!(has_active, "{label}: breakdown must contain active cell kinds");
}

// ── iter energy tests ───────────────────────────────────────────────

#[test]
fn iter_energy_positive_and_scales_with_count() {
    let db = char_db();

    eprintln!("\n═══ iter energy table (body = rot3 : Three ↔ Three) ═══");
    eprintln!(
        "{:<20} {:>10} {:>12} {:>12} {:>12} {:>14}",
        "Program", "Cells", "Active (J)", "Buffer (J)", "Total (J)", "J/iteration"
    );
    eprintln!("{:-<86}", "");

    let counts = [2, 4, 8, 16];
    let mut prev_total = 0.0;
    let mut energies = Vec::new();

    for &n in &counts {
        let iso = Iso::iter(Type::numeral_ty(n), rot3());
        let bd = price_iso(&iso, db);
        assert_sane_breakdown(&bd, &format!("iter N{n} rot3"));

        let net = lower_iso(&iso).expect("lower_iso");
        let cell_count: usize = net.cells.len();
        // Max iterations is n-1 (the numeral type has n values 0..n-1).
        let max_iters = n - 1;
        let j_per_iter = if max_iters > 0 {
            bd.total_j / max_iters as f64
        } else {
            0.0
        };

        eprintln!(
            "{:<20} {:>10} {:>12.4e} {:>12.4e} {:>12.4e} {:>14.4e}",
            format!("iter N{n} rot3"),
            cell_count,
            bd.active_j,
            bd.buffer_j,
            bd.total_j,
            j_per_iter,
        );

        // Energy must grow with iteration count (more unrolled body copies).
        if prev_total > 0.0 {
            assert!(
                bd.total_j > prev_total,
                "iter N{n} energy ({:.4e}) must exceed iter N{} energy ({:.4e})",
                bd.total_j,
                n / 2,
                prev_total,
            );
        }
        prev_total = bd.total_j;
        energies.push((n, bd));
    }

    eprintln!();
}

#[test]
fn iter_not_energy_positive() {
    let db = char_db();

    eprintln!("\n═══ iter energy table (body = not : Bit ↔ Bit) ═══");
    eprintln!(
        "{:<20} {:>10} {:>12} {:>12} {:>12} {:>14}",
        "Program", "Cells", "Active (J)", "Buffer (J)", "Total (J)", "J/iteration"
    );
    eprintln!("{:-<86}", "");

    for &n in &[2, 4, 8] {
        let iso = Iso::iter(Type::numeral_ty(n), not_iso());
        let bd = price_iso(&iso, db);
        assert_sane_breakdown(&bd, &format!("iter N{n} not"));

        let net = lower_iso(&iso).expect("lower_iso");
        let max_iters = n - 1;
        let j_per_iter = if max_iters > 0 {
            bd.total_j / max_iters as f64
        } else {
            0.0
        };

        eprintln!(
            "{:<20} {:>10} {:>12.4e} {:>12.4e} {:>12.4e} {:>14.4e}",
            format!("iter N{n} not"),
            net.cells.len(),
            bd.active_j,
            bd.buffer_j,
            bd.total_j,
            j_per_iter,
        );
    }

    eprintln!();
}

// ── fold energy tests ───────────────────────────────────────────────

#[test]
fn fold_energy_positive_and_scales_with_depth() {
    let db = char_db();

    eprintln!("\n═══ fold energy table (body = cx : Bit×Bit ↔ Bit×Bit) ═══");
    eprintln!(
        "{:<24} {:>10} {:>12} {:>12} {:>12} {:>14}",
        "Program", "Cells", "Active (J)", "Buffer (J)", "Total (J)", "J/element"
    );
    eprintln!("{:-<90}", "");

    let depths = [2, 3, 4, 5];
    let mut prev_total = 0.0;

    for &d in &depths {
        let iso = Iso::fold(Type::list_spine_ty(bit(), d), cx());
        let bd = price_iso(&iso, db);
        assert_sane_breakdown(&bd, &format!("fold L{d}(Bit) cx"));

        let net = lower_iso(&iso).expect("lower_iso");
        // Max elements is d-1 (spine of depth d has values of length 0..d-1).
        let max_elems = d - 1;
        let j_per_elem = if max_elems > 0 {
            bd.total_j / max_elems as f64
        } else {
            0.0
        };

        eprintln!(
            "{:<24} {:>10} {:>12.4e} {:>12.4e} {:>12.4e} {:>14.4e}",
            format!("fold L{d}(Bit) cx"),
            net.cells.len(),
            bd.active_j,
            bd.buffer_j,
            bd.total_j,
            j_per_elem,
        );

        if prev_total > 0.0 {
            assert!(
                bd.total_j > prev_total,
                "fold L{d} energy ({:.4e}) must exceed fold L{} energy ({:.4e})",
                bd.total_j,
                d - 1,
                prev_total,
            );
        }
        prev_total = bd.total_j;
    }

    eprintln!();
}

#[test]
fn fold_cx_swap_energy_positive() {
    let db = char_db();

    eprintln!("\n═══ fold energy table (body = cx;swap : Bit×Bit ↔ Bit×Bit) ═══");
    eprintln!(
        "{:<28} {:>10} {:>12} {:>12} {:>12} {:>14}",
        "Program", "Cells", "Active (J)", "Buffer (J)", "Total (J)", "J/element"
    );
    eprintln!("{:-<94}", "");

    for &d in &[2, 3, 4] {
        let iso = Iso::fold(Type::list_spine_ty(bit(), d), cx_swap());
        let bd = price_iso(&iso, db);
        assert_sane_breakdown(&bd, &format!("fold L{d}(Bit) cx;swap"));

        let net = lower_iso(&iso).expect("lower_iso");
        let max_elems = d - 1;
        let j_per_elem = if max_elems > 0 {
            bd.total_j / max_elems as f64
        } else {
            0.0
        };

        eprintln!(
            "{:<28} {:>10} {:>12.4e} {:>12.4e} {:>12.4e} {:>14.4e}",
            format!("fold L{d}(Bit) cx;swap"),
            net.cells.len(),
            bd.active_j,
            bd.buffer_j,
            bd.total_j,
            j_per_elem,
        );
    }

    eprintln!();
}

// ── unfold energy test ──────────────────────────────────────────────

#[test]
fn unfold_energy_positive() {
    let db = char_db();

    eprintln!("\n═══ unfold energy table (body = cx : Bit×Bit ↔ Bit×Bit) ═══");
    eprintln!(
        "{:<26} {:>10} {:>12} {:>12} {:>12}",
        "Program", "Cells", "Active (J)", "Buffer (J)", "Total (J)"
    );
    eprintln!("{:-<76}", "");

    for &d in &[2, 3, 4] {
        let iso = Iso::unfold(Type::list_spine_ty(bit(), d), cx());
        let bd = price_iso(&iso, db);
        assert_sane_breakdown(&bd, &format!("unfold L{d}(Bit) cx"));

        let net = lower_iso(&iso).expect("lower_iso");

        eprintln!(
            "{:<26} {:>10} {:>12.4e} {:>12.4e} {:>12.4e}",
            format!("unfold L{d}(Bit) cx"),
            net.cells.len(),
            bd.active_j,
            bd.buffer_j,
            bd.total_j,
        );
    }

    eprintln!();
}

// ── Combined summary table ──────────────────────────────────────────

#[test]
fn print_iter_fold_energy_summary() {
    let db = char_db();

    eprintln!("\n╔══════════════════════════════════════════════════════════════════════════════════╗");
    eprintln!("║  v0.2 iter/fold/unfold → k-CSWAP cells → joules (rung0 char DB, t_ramp=1e-7)  ║");
    eprintln!("╚══════════════════════════════════════════════════════════════════════════════════╝\n");

    let programs: Vec<(&str, Iso)> = vec![
        ("iter N2 rot3", Iso::iter(Type::numeral_ty(2), rot3())),
        ("iter N4 rot3", Iso::iter(Type::numeral_ty(4), rot3())),
        ("iter N8 rot3", Iso::iter(Type::numeral_ty(8), rot3())),
        ("iter N4 not", Iso::iter(Type::numeral_ty(4), not_iso())),
        ("fold L2(Bit) cx", Iso::fold(Type::list_spine_ty(bit(), 2), cx())),
        ("fold L3(Bit) cx", Iso::fold(Type::list_spine_ty(bit(), 3), cx())),
        ("fold L4(Bit) cx", Iso::fold(Type::list_spine_ty(bit(), 4), cx())),
        (
            "fold L3(Bit) cx;swap",
            Iso::fold(Type::list_spine_ty(bit(), 3), cx_swap()),
        ),
        ("unfold L3(Bit) cx", Iso::unfold(Type::list_spine_ty(bit(), 3), cx())),
        ("unfold L4(Bit) cx", Iso::unfold(Type::list_spine_ty(bit(), 4), cx())),
    ];

    eprintln!(
        "{:<24} {:>6} {:>6} {:>6} {:>6} {:>12} {:>12} {:>12}",
        "Program", "K0", "K1", "K2", "K3", "Active (J)", "Buffer (J)", "Total (J)"
    );
    eprintln!("{:-<100}", "");

    for (label, iso) in &programs {
        let net = lower_iso(iso).expect("lower_iso");
        let bd = price_iso(iso, db);

        let counts = net.cell_counts();
        let k0 = counts.get(&CellKind::K0).copied().unwrap_or(0);
        let k1 = counts.get(&CellKind::K1).copied().unwrap_or(0);
        let k2 = counts.get(&CellKind::K2).copied().unwrap_or(0);
        let k3 = counts.get(&CellKind::K3).copied().unwrap_or(0);

        eprintln!(
            "{:<24} {:>6} {:>6} {:>6} {:>6} {:>12.4e} {:>12.4e} {:>12.4e}",
            label, k0, k1, k2, k3, bd.active_j, bd.buffer_j, bd.total_j,
        );
    }

    eprintln!();
}
