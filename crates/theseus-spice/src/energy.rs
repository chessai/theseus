//! ADR-0028 energy model: E(t_ramp) = k_adia/t_ramp + k_leak*t_ramp + k_res.
//!
//! Least-squares fit in the basis [1/t, t, 1], with R² goodness-of-fit.

/// Fitted energy-model coefficients.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct EnergyFit {
    pub k_adia: f64,
    pub k_leak: f64,
    pub k_res: f64,
    pub r_squared: f64,
}

/// Fit the energy model E = k_adia/t + k_leak*t + k_res to observed `(t_ramp, energy)` points.
///
/// Solves the 3×3 normal equations for the basis [1/t, t, 1].
///
/// # Panics
///
/// Panics if `points` has fewer than 3 elements (under-determined).
pub fn fit(points: &[(f64, f64)]) -> EnergyFit {
    assert!(points.len() >= 3, "need at least 3 points for 3-parameter fit");

    // Basis functions: x0 = 1/t, x1 = t, x2 = 1.
    // Normal equations: A^T A c = A^T y  where A_ij = basis_j(t_i).
    //
    // We accumulate the 6 unique entries of the symmetric 3×3 A^T A
    // and the 3 entries of A^T y.
    let (mut s00, mut s01, mut s02) = (0.0, 0.0, 0.0);
    let (mut s11, mut s12, mut s22) = (0.0, 0.0, 0.0);
    let (mut r0, mut r1, mut r2) = (0.0, 0.0, 0.0);
    let mut y_sum = 0.0;
    let mut y_sq_sum = 0.0;

    for &(t, e) in points {
        let inv_t = 1.0 / t;
        // A^T A accumulation
        s00 += inv_t * inv_t;
        s01 += inv_t * t;  // = 1
        s02 += inv_t;
        s11 += t * t;
        s12 += t;
        s22 += 1.0;
        // A^T y accumulation
        r0 += inv_t * e;
        r1 += t * e;
        r2 += e;
        // For R²
        y_sum += e;
        y_sq_sum += e * e;
    }

    // Solve 3×3 via Cramer's rule.
    let det = s00 * (s11 * s22 - s12 * s12)
            - s01 * (s01 * s22 - s12 * s02)
            + s02 * (s01 * s12 - s11 * s02);

    let k_adia = ((s11 * s22 - s12 * s12) * r0
                + (s02 * s12 - s01 * s22) * r1
                + (s01 * s12 - s02 * s11) * r2) / det;

    let k_leak = ((s02 * s12 - s01 * s22) * r0
                + (s00 * s22 - s02 * s02) * r1
                + (s01 * s02 - s00 * s12) * r2) / det;

    let k_res  = ((s01 * s12 - s02 * s11) * r0
                + (s01 * s02 - s00 * s12) * r1
                + (s00 * s11 - s01 * s01) * r2) / det;

    // R² = 1 - SS_res / SS_tot
    let n = points.len() as f64;
    let y_mean = y_sum / n;
    let ss_tot = y_sq_sum - n * y_mean * y_mean;
    let mut ss_res = 0.0;
    for &(t, e) in points {
        let predicted = k_adia / t + k_leak * t + k_res;
        let residual = e - predicted;
        ss_res += residual * residual;
    }
    let r_squared = if ss_tot.abs() < f64::EPSILON { 1.0 } else { 1.0 - ss_res / ss_tot };

    EnergyFit { k_adia, k_leak, k_res, r_squared }
}

impl EnergyFit {
    /// Evaluate the energy model at a given ramp time.
    pub fn eval(&self, t_ramp: f64) -> f64 {
        self.k_adia / t_ramp + self.k_leak * t_ramp + self.k_res
    }

    /// Returns `true` if the fit quality meets the given R² threshold.
    pub fn adequate(&self, min_r2: f64) -> bool {
        self.r_squared >= min_r2
    }
}
