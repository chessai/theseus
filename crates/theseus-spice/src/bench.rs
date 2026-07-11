//! Energy-measurement harness: drives ngspice, integrates per-cycle energy,
//! and provides self-validation gates (RC analytic + ideal-source).

use std::env;
use std::fs;
use std::io;
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static COUNTER: AtomicU64 = AtomicU64::new(0);

/// Locate the ngspice binary: `THESEUS_NGSPICE` env var (full path), else `ngspice` on PATH.
fn ngspice_bin() -> String {
    env::var("THESEUS_NGSPICE").unwrap_or_else(|_| "ngspice".to_string())
}

/// Check whether ngspice is available.
pub fn ngspice_available() -> bool {
    Command::new(ngspice_bin())
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok()
}

/// Write `netlist` to a unique temp file, run `ngspice -b`, return stdout.
fn run_ngspice_raw(netlist: &str) -> io::Result<String> {
    let pid = std::process::id();
    let seq = COUNTER.fetch_add(1, Ordering::Relaxed);
    let filename = format!("theseus_spice_{}_{}.cir", pid, seq);
    let path = env::temp_dir().join(&filename);

    fs::write(&path, netlist)?;

    let output = Command::new(ngspice_bin())
        .arg("-b")
        .arg(&path)
        .output()?;

    // Clean up temp file regardless of outcome.
    let _ = fs::remove_file(&path);

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(io::Error::other(format!(
            "ngspice exited with {}\nstdout:\n{}\nstderr:\n{}",
            output.status, stdout, stderr
        )));
    }

    String::from_utf8(output.stdout)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Parse a `.meas` result from ngspice stdout.
///
/// ngspice prints measurement results like:
///   `esource              =  1.23456e-12`
/// or sometimes as:
///   `esource              =   1.23456e-12  from=  ...`
///
/// Returns `None` if the measurement name is not found.
pub fn parse_meas(stdout: &str, meas_name: &str) -> Option<f64> {
    let lower_name = meas_name.to_lowercase();
    for line in stdout.lines() {
        let lower_line = line.to_lowercase();
        // Match lines like "esource              =  1.23e-12"
        if let Some(eq_pos) = lower_line.find('=') {
            let before_eq = lower_line[..eq_pos].trim();
            if before_eq == lower_name {
                let after_eq = line[eq_pos + 1..].trim();
                // Take the first whitespace-delimited token after '='
                let token = after_eq.split_whitespace().next()?;
                return token.parse::<f64>().ok();
            }
        }
    }
    None
}

/// Run an ngspice netlist and extract a single `.meas` result (in joules or
/// whatever unit the measurement produces).
///
/// # Panics
/// Panics if ngspice fails or the measurement is not found.
pub fn run_energy(netlist: &str, meas_name: &str) -> f64 {
    let stdout = run_ngspice_raw(netlist).expect("ngspice invocation failed");
    parse_meas(&stdout, meas_name).unwrap_or_else(|| {
        panic!(
            "measurement '{}' not found in ngspice output:\n{}",
            meas_name, stdout
        )
    })
}

/// Sweep clock period over a parameterized netlist.
///
/// `netlist_template` must contain the literal placeholder `{PERIOD}` which
/// will be replaced with each period value (in seconds, scientific notation).
/// Returns `Vec<(period_s, energy_J)>`.
pub fn sweep_period(
    netlist_template: &str,
    meas_name: &str,
    periods: &[f64],
) -> Vec<(f64, f64)> {
    periods
        .iter()
        .map(|&t| {
            let netlist = netlist_template.replace("{PERIOD}", &format!("{:.6e}", t));
            let e = run_energy(&netlist, meas_name);
            (t, e)
        })
        .collect()
}

/// Perform a linear regression on log-log data and return the slope.
pub fn log_log_slope(data: &[(f64, f64)]) -> f64 {
    let n = data.len() as f64;
    let (mut sx, mut sy, mut sxx, mut sxy) = (0.0, 0.0, 0.0, 0.0);
    for &(x, y) in data {
        let lx = x.ln();
        let ly = y.ln();
        sx += lx;
        sy += ly;
        sxx += lx * lx;
        sxy += lx * ly;
    }
    (n * sxy - sx * sy) / (n * sxx - sx * sx)
}

// ---------------------------------------------------------------------------
// Netlist builders for self-validation gates
// ---------------------------------------------------------------------------

/// Gate 1: RC ramp netlist.
///
/// A trapezoidal voltage source linearly ramps from 0 to V over period/2,
/// then ramps back from V to 0 over period/2 (a full charge-discharge cycle).
/// The RC circuit: Vsrc -> R -> C -> GND.
///
/// We measure the total energy *delivered by the source* over one cycle via
/// `Bpwr` and `.meas tran esource INTEG`.
///
/// The closed-form dissipated energy for a linear ramp charge then discharge:
///   E_diss = 2 · C · V² · (RC / T)
/// (one charge dissipates C·V²·RC/T_half = C·V²·2RC/T; full cycle is 2×).
///
/// ΔE_stored over a full symmetric cycle = 0, so E_source = E_diss.
pub fn rc_ramp_netlist(r_ohm: f64, c_farad: f64, v_volt: f64, period_s: f64) -> String {
    let half = period_s / 2.0;
    // PWL: ramp up 0→V over half, ramp down V→0 over half
    // Use a tiny rise time at edges for numerical stability? No — the ramp IS
    // the waveform; we want a perfect triangle.
    format!(
        "\
RC ramp self-validation (T={period_s:.3e} s)
Vsrc vdd 0 PWL(0 0 {half:.15e} {v_volt} {period_s:.15e} 0)
R1 vdd cap {r_ohm}
C1 cap 0 {c_farad}
.ic V(cap)=0

* Instantaneous power delivered by the source (positive = energy out of source)
Bpwr pwr 0 V = -V(vdd)*I(Vsrc)

.control
tran {tstep:.15e} {tstop:.15e} uic
meas tran esource INTEG V(pwr) FROM=0 TO={tstop:.15e}
.endc
.end
",
        period_s = period_s,
        half = half,
        v_volt = v_volt,
        r_ohm = r_ohm,
        c_farad = c_farad,
        // Resolve BOTH the period (≥2000 steps/cycle) AND the RC dynamics
        // (≥10 steps per RC): at large T, period/2000 grows coarser than RC and
        // the trapezoidal energy integral misses the RC corners, running high.
        tstep = (period_s / 2000.0).min(r_ohm * c_farad / 10.0),
        tstop = period_s,
    )
}

/// Closed-form dissipated energy for a full triangle-ramp charge/discharge cycle.
///
/// For a linear ramp 0→V over T/2 charging through R into C:
///   E_diss(charge) = C·V²·(2RC/T)
/// (derivation: instantaneous current i(t) = CV/T_half = 2CV/T in the adiabatic
/// limit T≫RC; dissipation = ∫₀^{T/2} i²R dt = (2CV/T)²·R·T/2 = 2C²V²R/T.)
///
/// Symmetric discharge dissipates the same, so full cycle:
///   E_diss(cycle) = 2 · C · V² · (2RC/T) = 4·C²·V²·R/T
///
/// Wait — let's be careful. The exact adiabatic dissipation for a single
/// charge from 0→V with a linear ramp of duration T_half:
///   The quasi-static current is i ≈ C · dV_ramp/dt = C·V/T_half.
///   Power = i²R = C²V²R / T_half².
///   Energy = ∫₀^{T_half} i²R dt = C²V²R / T_half.
///   With T_half = T/2: E_charge = 2C²V²R / T.
///
/// Full charge+discharge cycle: E_diss = 2 × 2C²V²R / T = 4C²V²R / T.
///
/// Equivalently: E_diss = C·V² · (4RC/T) = C·V² · (2RC/T_half).
///
/// NOTE: This is the *first-order adiabatic* approximation valid when T ≫ RC.
/// At moderate T/RC ratios there are higher-order corrections; the harness
/// requirement is 5% agreement when T ≥ 100·RC.
pub fn rc_ramp_closed_form(r_ohm: f64, c_farad: f64, v_volt: f64, period_s: f64) -> f64 {
    // E_diss = 4 · C² · V² · R / T
    4.0 * c_farad * c_farad * v_volt * v_volt * r_ohm / period_s
}

/// Gate 2: Ideal source driving an open circuit — one full charge/discharge
/// cycle into a capacitor with zero resistance (ideal wires).
///
/// Over a symmetric cycle the net energy delivered should be zero (energy
/// stored during charge is returned during discharge).
pub fn ideal_source_cap_netlist(c_farad: f64, v_volt: f64, period_s: f64) -> String {
    let half = period_s / 2.0;
    format!(
        "\
Ideal-source sanity (open circuit / lossless cap)
Vsrc vdd 0 PWL(0 0 {half:.15e} {v_volt} {period_s:.15e} 0)
C1 vdd 0 {c_farad}

* Instantaneous power delivered by the source
Bpwr pwr 0 V = -V(vdd)*I(Vsrc)

.control
tran {tstep:.15e} {tstop:.15e} uic
meas tran esource INTEG V(pwr) FROM=0 TO={tstop:.15e}
.endc
.end
",
        half = half,
        v_volt = v_volt,
        period_s = period_s,
        c_farad = c_farad,
        tstep = period_s / 2000.0,
        tstop = period_s,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Skip tests if ngspice is not available rather than failing.
    macro_rules! require_ngspice {
        () => {
            if !ngspice_available() {
                eprintln!("ngspice not found — skipping test");
                return;
            }
        };
    }

    // ----- Smoke test: .meas parsing -----

    #[test]
    fn parse_meas_smoke() {
        let sample_output = "\
Circuit: test
Doing analysis at TEMPERATURE =  27.000000 and target temperature =  27.000000
esource              =  3.14159e-12
";
        let val = parse_meas(sample_output, "esource").unwrap();
        assert!((val - 3.14159e-12).abs() < 1e-20, "parsed value: {}", val);
    }

    #[test]
    fn parse_meas_missing() {
        let sample_output = "no measurements here\n";
        assert!(parse_meas(sample_output, "esource").is_none());
    }

    // ----- Gate 1: RC ramp matches closed form -----

    #[test]
    fn rc_ramp_matches_closed_form() {
        require_ngspice!();

        let r: f64 = 1e3;  // 1 kΩ
        let c: f64 = 1e-12; // 1 pF
        let v: f64 = 1.0;   // 1 V
        let rc = r * c; // 1 ns

        // Sweep T from 100·RC to 100_000·RC (3 decades), ≥5 points/decade.
        // Use 6 points/decade → 18+ points total.
        let mut periods: Vec<f64> = Vec::new();
        let t_min = 100.0 * rc;  // 100 ns
        let t_max = 100_000.0 * rc; // 100 µs
        let decades = (t_max / t_min).log10();
        let points_per_decade = 6;
        let n_points = (decades * points_per_decade as f64).ceil() as usize + 1;
        for i in 0..n_points {
            let frac = i as f64 / (n_points - 1) as f64;
            let t = t_min * 10_f64.powf(frac * decades);
            periods.push(t);
        }

        let mut results: Vec<(f64, f64)> = Vec::new();
        let mut diss_points: Vec<(f64, f64)> = Vec::new();

        for &t in &periods {
            let netlist = rc_ramp_netlist(r, c, v, t);
            let e_source = run_energy(&netlist, "esource");
            let e_analytic = rc_ramp_closed_form(r, c, v, t);

            // E_source over a full symmetric cycle ≈ E_diss (ΔE_stored = 0).
            let rel_err = (e_source - e_analytic).abs() / e_analytic;
            assert!(
                rel_err < 0.05,
                "Gate 1 FAIL at T={:.3e}: measured={:.6e}, analytic={:.6e}, rel_err={:.2}%",
                t, e_source, e_analytic, rel_err * 100.0,
            );

            results.push((t, e_source));
            diss_points.push((t, e_source));
        }

        // Check log-log slope = −1 ± 0.1.
        let slope = log_log_slope(&diss_points);
        assert!(
            (slope - (-1.0)).abs() < 0.1,
            "Gate 1 slope FAIL: slope={:.4} (expected -1 ± 0.1)",
            slope,
        );

        eprintln!("Gate 1 PASS: {} points, slope={:.4}", results.len(), slope);
    }

    // ----- Gate 2: ideal source, zero net energy -----

    #[test]
    fn ideal_source_open_circuit_zero_energy() {
        require_ngspice!();

        let c = 1e-12; // 1 pF
        let v = 1.0;   // 1 V
        let period = 1e-6; // 1 µs (arbitrary, well above any transient)

        let netlist = ideal_source_cap_netlist(c, v, period);
        let e_net = run_energy(&netlist, "esource");

        // CV² reference scale for the epsilon.
        let cv2 = c * v * v; // 1e-12 J

        // |E_net| should be within solver tolerance — tiny fraction of CV².
        let eps = 1e-18_f64.max(cv2 * 1e-4);
        assert!(
            e_net.abs() < eps,
            "Gate 2 FAIL: |E_net| = {:.6e} > eps = {:.6e}",
            e_net.abs(),
            eps,
        );

        eprintln!("Gate 2 PASS: E_net = {:.6e} J", e_net);
    }
}
