//! Static energy annotation of a compiled cell netlist (ADR-0028).
//!
//! Energy is (almost) static in this backend: dual-rail means one rail
//! pulses per valid signal regardless of value, linearity means no data
//! fanout -- so per-run energy is data-independent to first order and the
//! primary artifact is a *compiler report*: Σ over cells of the cell's
//! DB-characterized energy at the operating ramp time (C3.2 additivity,
//! a declared assumption validated by TH-E1).
//!
//! The interpolation contract is inherited from [`CharDb::query`]
//! (C3.1): in-range ramps interpolate the fitted model; out-of-range is
//! a hard error, never an extrapolation. Attribution is per cell kind
//! (C3.5's first rung; per-clause / per-iso provenance rollup is a later
//! slice).
//!
//! Claims boundary (C3.3), stamped on the report display: logic
//! dissipation under ideal power-clocks; clock-generation / resonator
//! losses and I/O excluded.

use std::collections::BTreeMap;
use std::fmt;

use theseus_netlist::PhasedCellNetlist;

use crate::chardb::{CharDb, QueryError};

/// One cell kind's share of the report (ADR-0028 attribution).
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct KindEnergy {
    /// How many cells of this kind the netlist places.
    pub count: usize,
    /// Fitted energy per cell at the report's `t_ramp`, joules.
    pub per_cell_j: f64,
    /// `count × per_cell_j`, joules.
    pub subtotal_j: f64,
}

/// The static energy report for one compiled netlist at one operating
/// ramp time (a first-class emission, C3.6 -- serializes to RON).
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct EnergyReport {
    /// Operating ramp time the energies were interpolated at, seconds.
    pub t_ramp: f64,
    /// Σ over cells of the fitted per-cell energy, joules.
    pub total_j: f64,
    /// Per-cell-kind breakdown, keyed by char-DB key (`cswap-k{0,1,2}`).
    pub by_kind: BTreeMap<String, KindEnergy>,
}

impl fmt::Display for EnergyReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "energy report @ t_ramp = {:.3e} s", self.t_ramp)?;
        for (key, k) in &self.by_kind {
            writeln!(
                f,
                "  {key}: {} cell(s) x {:.4e} J = {:.4e} J",
                k.count, k.per_cell_j, k.subtotal_j
            )?;
        }
        writeln!(
            f,
            "  total dissipated energy per op: {:.4e} J",
            self.total_j
        )?;
        write!(
            f,
            "  [claims boundary (ADR-0028 C3.3): logic dissipation under ideal \
             power-clocks; clock-generation/resonator losses and I/O excluded]"
        )
    }
}

/// Annotate a compiled netlist with its static energy at `t_ramp`:
/// Σ over cells of `char_db.query(cell.kind.char_db_key(), t_ramp)`,
/// attributed per cell kind.
///
/// Errors propagate [`CharDb::query`]'s contract unchanged: a kind
/// missing from the DB is [`QueryError::NoSuchCell`]; a ramp outside the
/// cell's swept range is [`QueryError::OutOfRange`] (no extrapolation,
/// C3.1).
///
/// A zero-cell netlist (pure wiring -- rewiring is free in this fabric)
/// reports 0 J with an empty breakdown.
pub fn compiled_energy(
    netlist: &PhasedCellNetlist,
    char_db: &CharDb,
    t_ramp: f64,
) -> Result<EnergyReport, QueryError> {
    let mut by_kind = BTreeMap::new();
    let mut total_j = 0.0;
    for (kind, count) in netlist.cell_counts() {
        let key = kind.char_db_key();
        let per_cell_j = char_db.query(key, t_ramp)?;
        let subtotal_j = per_cell_j * count as f64;
        total_j += subtotal_j;
        by_kind.insert(
            key.to_string(),
            KindEnergy {
                count,
                per_cell_j,
                subtotal_j,
            },
        );
    }
    Ok(EnergyReport {
        t_ramp,
        total_j,
        by_kind,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chardb::{CellChar, CharPoint};
    use crate::energy::EnergyFit;

    use theseus_core::Type;
    use theseus_netlist::{Cell, CellKind, LineId, PhasedCellNetlist, RailPair};

    fn bit() -> Type {
        Type::sum(Type::One, Type::One)
    }

    /// A fixture cell whose fitted model is the constant `e` over
    /// `[t_min, t_max]` -- exact expected sums, no ngspice.
    fn const_cell(key: &str, e: f64, t_min: f64, t_max: f64) -> (String, CellChar) {
        (
            key.to_string(),
            CellChar {
                key: key.to_string(),
                points: vec![
                    CharPoint {
                        t_ramp: t_min,
                        energy: e,
                    },
                    CharPoint {
                        t_ramp: t_max,
                        energy: e,
                    },
                ],
                fit: EnergyFit {
                    k_adia: 0.0,
                    k_leak: 0.0,
                    k_res: e,
                    r_squared: 1.0,
                },
            },
        )
    }

    fn fixture_db(e_k0: f64, e_k1: f64) -> CharDb {
        CharDb {
            cells: BTreeMap::from([
                const_cell("cswap-k0", e_k0, 1e-8, 1e-5),
                const_cell("cswap-k1", e_k1, 1e-8, 1e-5),
            ]),
        }
    }

    /// A hand-built single-K0 netlist on one bit (mirrors the cell-IR
    /// test fixture).
    fn one_buffer() -> PhasedCellNetlist {
        let inp = RailPair {
            line0: LineId(0),
            line1: LineId(1),
        };
        let out = RailPair {
            line0: LineId(2),
            line1: LineId(3),
        };
        PhasedCellNetlist {
            input_ty: bit(),
            output_ty: bit(),
            inputs: vec![inp],
            outputs: vec![out],
            output_depths: vec![1],
            cells: vec![Cell {
                kind: CellKind::K0,
                depth: 0,
                phase: 0,
                inputs: vec![inp],
                outputs: vec![out],
            }],
            num_lines: 4,
        }
    }

    #[test]
    fn single_buffer_costs_one_k0() {
        let db = fixture_db(2.0e-15, 7.0e-15);
        let r = compiled_energy(&one_buffer(), &db, 1e-7).unwrap();
        assert_eq!(r.total_j, 2.0e-15);
        assert_eq!(r.by_kind.len(), 1);
        assert_eq!(
            r.by_kind["cswap-k0"],
            KindEnergy {
                count: 1,
                per_cell_j: 2.0e-15,
                subtotal_j: 2.0e-15
            }
        );
    }

    #[test]
    fn zero_cell_netlist_is_free() {
        // Pure wiring: identity on one bit, no cells.
        let inp = RailPair {
            line0: LineId(0),
            line1: LineId(1),
        };
        let net = PhasedCellNetlist {
            input_ty: bit(),
            output_ty: bit(),
            inputs: vec![inp],
            outputs: vec![inp],
            output_depths: vec![0],
            cells: vec![],
            num_lines: 2,
        };
        let db = fixture_db(2.0e-15, 7.0e-15);
        let r = compiled_energy(&net, &db, 1e-7).unwrap();
        assert_eq!(r.total_j, 0.0);
        assert!(r.by_kind.is_empty());
    }

    #[test]
    fn out_of_range_ramp_is_a_hard_error() {
        let db = fixture_db(2.0e-15, 7.0e-15);
        assert!(matches!(
            compiled_energy(&one_buffer(), &db, 1e-3),
            Err(QueryError::OutOfRange { .. })
        ));
    }

    #[test]
    fn missing_kind_is_a_hard_error() {
        let db = CharDb {
            cells: BTreeMap::new(),
        };
        assert_eq!(
            compiled_energy(&one_buffer(), &db, 1e-7),
            Err(QueryError::NoSuchCell("cswap-k0".to_string()))
        );
    }

    #[test]
    fn nonconstant_fit_is_evaluated_at_the_operating_ramp() {
        // E(t) = k_adia / t: at t = 1e-7 a k_adia of 1e-21 gives 1e-14 J.
        let cell = CellChar {
            key: "cswap-k0".to_string(),
            points: vec![
                CharPoint {
                    t_ramp: 1e-8,
                    energy: 1e-13,
                },
                CharPoint {
                    t_ramp: 1e-6,
                    energy: 1e-15,
                },
            ],
            fit: EnergyFit {
                k_adia: 1e-21,
                k_leak: 0.0,
                k_res: 0.0,
                r_squared: 1.0,
            },
        };
        let db = CharDb {
            cells: BTreeMap::from([("cswap-k0".to_string(), cell)]),
        };
        let r = compiled_energy(&one_buffer(), &db, 1e-7).unwrap();
        assert!((r.total_j - 1e-14).abs() < 1e-20, "got {}", r.total_j);
    }

    #[test]
    fn report_round_trips_through_ron() {
        let db = fixture_db(2.0e-15, 7.0e-15);
        let r = compiled_energy(&one_buffer(), &db, 1e-7).unwrap();
        let text = ron::to_string(&r).unwrap();
        let back: EnergyReport = ron::from_str(&text).unwrap();
        assert_eq!(r, back);
    }
}
