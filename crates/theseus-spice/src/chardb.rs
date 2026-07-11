//! Characterization database: per-cell energy sweeps, model fits, and
//! strict interpolation-only query (ADR-0028 C3.1).

use std::collections::BTreeMap;
use std::fmt;

use crate::energy::{self, EnergyFit};

/// A single measured (t_ramp, energy) data point.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CharPoint {
    pub t_ramp: f64,
    pub energy: f64,
}

/// Characterization record for one cell: sorted sweep points + fitted model.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CellChar {
    pub key: String,
    pub points: Vec<CharPoint>,
    pub fit: EnergyFit,
}

/// The full characterization database (deterministic key order via BTreeMap).
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CharDb {
    pub cells: BTreeMap<String, CellChar>,
}

/// Errors returned by [`CharDb::query`].
#[derive(Debug, PartialEq)]
pub enum QueryError {
    /// The requested t_ramp is outside the cell's swept range.
    OutOfRange {
        key: String,
        t_ramp: f64,
        min: f64,
        max: f64,
    },
    /// No cell with the given key exists in the database.
    NoSuchCell(String),
}

impl fmt::Display for QueryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QueryError::OutOfRange { key, t_ramp, min, max } => {
                write!(
                    f,
                    "t_ramp {t_ramp} out of range [{min}, {max}] for cell {key:?}"
                )
            }
            QueryError::NoSuchCell(key) => write!(f, "no such cell: {key:?}"),
        }
    }
}

/// Build a [`CharDb`] from raw per-cell sweep data.
///
/// Each entry is `(cell_key, unsorted_points)`. Points are sorted ascending
/// by `t_ramp` and a model is fitted for each cell.
pub fn rebuild(raw: &[(String, Vec<CharPoint>)]) -> CharDb {
    let mut cells = BTreeMap::new();
    for (key, pts) in raw {
        let mut sorted: Vec<CharPoint> = pts.clone();
        sorted.sort_by(|a, b| a.t_ramp.partial_cmp(&b.t_ramp).unwrap());

        let fit_input: Vec<(f64, f64)> = sorted.iter().map(|p| (p.t_ramp, p.energy)).collect();
        let fit = energy::fit(&fit_input);

        cells.insert(
            key.clone(),
            CellChar {
                key: key.clone(),
                points: sorted,
                fit,
            },
        );
    }
    CharDb { cells }
}

impl CharDb {
    /// Serialize to pretty RON text (deterministic).
    pub fn to_ron(&self) -> String {
        ron::ser::to_string_pretty(self, ron::ser::PrettyConfig::default())
            .expect("CharDb serialization should not fail")
    }

    /// Deserialize from RON text.
    pub fn from_ron(s: &str) -> Result<CharDb, ron::error::SpannedError> {
        ron::from_str(s)
    }

    /// Query the fitted energy for `key` at `t_ramp`.
    ///
    /// **Contract (ADR-0028 C3.1):** only values within the cell's swept
    /// `[min, max]` range are accepted — no extrapolation.
    pub fn query(&self, key: &str, t_ramp: f64) -> Result<f64, QueryError> {
        let cell = self
            .cells
            .get(key)
            .ok_or_else(|| QueryError::NoSuchCell(key.to_string()))?;

        let min = cell.points.first().unwrap().t_ramp;
        let max = cell.points.last().unwrap().t_ramp;

        if t_ramp < min || t_ramp > max {
            return Err(QueryError::OutOfRange {
                key: key.to_string(),
                t_ramp,
                min,
                max,
            });
        }

        Ok(cell.fit.eval(t_ramp))
    }
}
