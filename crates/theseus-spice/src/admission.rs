//! Per-cell **admission checklist** (test-spec-m3a "Per-cell admission
//! checklist", ADR-0027 B4.3, made executable): the five gates that decide
//! whether a reversible cell enters the library.
//!
//! # The five gates
//!
//! 1. **Functional verdict** (B5.3) — the cell's truth table reproduced via
//!    *typed decode*: rails thresholded at 0.3·VDD / 0.7·VDD with a
//!    hysteresis dead-band between them (a rail reads high only above
//!    0.7·VDD and low only below 0.3·VDD; the band never decodes — TBD-4),
//!    sampled mid-validity (the middle 60% of the output hold tick, the
//!    harness convention), for every control/data combination. Built on
//!    [`run_cswap_functional`]. The full (ramp, load) matrix of the spec is
//!    the bench DUT matrix's scope; admission evaluates the rung's canonical
//!    functional point.
//! 2. **Conservation closure** (TBD-1, 1%) — per-cycle source energy =
//!    dissipation + ΔE_stored within 1%. What is *measured*: a short
//!    **closure orbit** of tokens (chosen per cell so every token line
//!    pulses and every branch conducts — otherwise unexercised wires float
//!    and their rung-1 leak drift reads as a spurious ΔE_stored) is
//!    repeated until settled, then (a) two consecutive orbits' source-energy
//!    integrals agree (periodicity ⇒ the device and node state returns to
//!    itself ⇒ ΔE_stored = 0 ⇒ E_source = E_dissipated), (b) the sum of the
//!    per-source energy integrals equals the total-power integral (no
//!    phantom inlet/outlet in the bookkeeping), and (c) ΔE_stored over the
//!    orbit, computed directly as Σ ½C·(V_end² − V_start²) over every
//!    *explicit* capacitor in the deck, is ≈ 0.
//!    Caveat: (c) covers the deck's explicit storage only; device-internal
//!    (BSIM) charge at rung 1 is covered indirectly by the periodicity
//!    check (a). Tolerance: 1% of the per-orbit source energy with an
//!    absolute floor of [`CLOSURE_ABS_FLOOR_J`] (solver noise).
//! 3. **Token conservation** (INV-TOKEN) — asserted-rail count invariant at
//!    every sampled valid tick: in the 1-of-2 token-line encoding exactly
//!    one line of each signal pair decodes asserted, so the total across
//!    the cell's pairs is constant (= the signal count).
//! 4. **Reverse drive** (TH-6) — the 4-phase power-clock phase sequence
//!    reversed and the *adjoint* truth table checked (the k-CSWAP cells are
//!    self-inverse, so the adjoint table is the forward table). For the
//!    single-stage cell the two clock phases it touches (φ0, φ2 = ~φ0) form
//!    a complementary pair whose global time reversal equals a one-tick
//!    shift of the same pair; the physically meaningful content of running
//!    the clock backward is therefore the exchange of the forward-port and
//!    reverse-port stimuli, which is how the reversed deck is generated
//!    (input token lines ↔ delayed-replica token lines, measurement grid
//!    unchanged). Per the spec the verdict is **recorded even on failure**
//!    (failure routes to the mirrored-netlist fallback, not rejection), so
//!    this gate does not block [`AdmissionReport::admitted`].
//! 5. **Adiabatic scaling** (ADR-0028, TBD-5) — the cell's own E(period)
//!    fits E = k_adia/t + k_leak·t + k_res with R² ≥ 0.99 over the swept
//!    range, and the leakage share is reported separately (C3.3).
//!    R² alone is necessary but **not sufficient**: a flat (CMOS) curve fits
//!    the model form vacuously with k_adia ≈ 0 and k_res = ½CV², and with 3
//!    sweep points any 3-parameter fit is exact. Admission therefore also
//!    requires ≥ 5 sweep points and that the recoverable term actually
//!    carries the curve: k_adia/t must contribute ≥ [`MIN_ADIA_SHARE`] of
//!    the model energy at both ends of the swept range. A flat CMOS curve
//!    fails (adiabatic share ≈ 0); a floored PFAL curve fails at the slow
//!    end (leakage-dominated).
//!
//! `admitted()` = gates 1 ∧ 2 ∧ 3 ∧ 5 (gate 4 recorded, non-blocking).
//!
//! # Candidate cells
//!
//! The reversible library cells — the k-CSWAP family (k ∈ {0, 1, 2, 3}) and the
//! adiabatic TG buffer primitive — plus the two baseline DUTs (static CMOS
//! inverter, PFAL buffer), which the checklist must *reject*: that the same
//! five gates admit the reversible cells and reject the baselines is the
//! discrimination test of the checklist itself. Note the raw TG buffer is
//! single-ended, so its INV-TOKEN check degenerates to "the one rail is
//! asserted at every valid tick" (stated in its verdict detail); the
//! dual-rail token discipline proper lives one level up, in the k = 0 stage.

use std::io;

use crate::adiabatic::{
    adiabatic_tg_curve_rung, cmos_baseline_curve_rung, pfal_curve_rung, DeviceRung, FetKind,
    Sky130Variant,
};
use crate::bench::{parse_meas, run_ngspice_raw};
use crate::cswap::{
    cswap_energy_curve, cswap_functional_netlist, cswap_netlist, cswap_out, cswap_signals,
    run_cswap_functional, CswapReading, CswapToken,
};
use crate::energy::{self, EnergyFit};

// ---------------------------------------------------------------------------
// Thresholds (the TBD register values this checklist executes against)
// ---------------------------------------------------------------------------

/// Typed-decode low threshold, fraction of VDD (TBD-4).
pub const DECODE_LO_FRAC: f64 = 0.3;
/// Typed-decode high threshold, fraction of VDD (TBD-4). The band between
/// the two thresholds is the hysteresis dead-zone: it never decodes.
pub const DECODE_HI_FRAC: f64 = 0.7;
/// Conservation-closure relative tolerance (TBD-1).
pub const CLOSURE_TOL: f64 = 0.01;
/// Absolute closure floor, joules: below this the comparison measures the
/// transient solver, not the circuit.
pub const CLOSURE_ABS_FLOOR_J: f64 = 1e-17;
/// Minimum energy-model fit quality (TBD-5).
pub const MIN_FIT_R2: f64 = 0.99;
/// Minimum share of the model energy the recoverable k_adia/t term must
/// carry at both ends of the swept range (see module docs: R² alone cannot
/// reject a flat curve).
pub const MIN_ADIA_SHARE: f64 = 0.5;

// ---------------------------------------------------------------------------
// Typed decode
// ---------------------------------------------------------------------------

/// The decoded state of a rail (or dual-rail pair) under the B5.3 typed
/// decode: thresholds at [`DECODE_LO_FRAC`]/[`DECODE_HI_FRAC`]·VDD with the
/// band between them as a non-decoding hysteresis zone.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RailState {
    /// At or above 0.7·VDD.
    Asserted,
    /// At or below 0.3·VDD.
    Deasserted,
    /// In the hysteresis dead-band — refuses to decode.
    Undefined,
}

/// Decode a single rail voltage.
pub fn decode_rail(v: f64, vdd: f64) -> RailState {
    if v >= DECODE_HI_FRAC * vdd {
        RailState::Asserted
    } else if v <= DECODE_LO_FRAC * vdd {
        RailState::Deasserted
    } else {
        RailState::Undefined
    }
}

/// Decode an SR-style (rail, mirror) wire pair: asserted iff the rail is
/// high AND the mirror is low; deasserted iff the exact opposite; anything
/// else (including a disagreeing pair) is undefined.
pub fn decode_pair(rail_v: f64, mirror_v: f64, vdd: f64) -> RailState {
    match (decode_rail(rail_v, vdd), decode_rail(mirror_v, vdd)) {
        (RailState::Asserted, RailState::Deasserted) => RailState::Asserted,
        (RailState::Deasserted, RailState::Asserted) => RailState::Deasserted,
        _ => RailState::Undefined,
    }
}

// ---------------------------------------------------------------------------
// Report types
// ---------------------------------------------------------------------------

/// A candidate for library admission.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CandidateCell {
    /// k = 0 CSWAP: the 2LAL buffer/latch stage.
    CswapK0,
    /// k = 1 CSWAP: the Fredkin gate.
    CswapK1,
    /// k = 2 CSWAP: the 2-controlled swap.
    CswapK2,
    /// k = 3 CSWAP: the 3-controlled swap.
    CswapK3,
    /// The adiabatic transmission-gate buffer primitive (single-ended; see
    /// module docs for the INV-TOKEN degeneracy caveat).
    AdiabaticTg,
    /// Baseline DUT: static CMOS inverter (step-driven). Expected REJECTED.
    CmosBaseline,
    /// Baseline DUT: PFAL buffer (quasi-adiabatic, floors). Expected REJECTED.
    PfalBuffer,
}

impl CandidateCell {
    /// The CSWAP order, if this is a k-CSWAP cell.
    pub fn cswap_k(self) -> Option<usize> {
        match self {
            CandidateCell::CswapK0 => Some(0),
            CandidateCell::CswapK1 => Some(1),
            CandidateCell::CswapK2 => Some(2),
            CandidateCell::CswapK3 => Some(3),
            _ => None,
        }
    }
}

/// One gate's verdict plus its human-readable evidence.
#[derive(Clone, Debug)]
pub struct GateResult {
    /// Whether the gate passed.
    pub passed: bool,
    /// What was measured and how the verdict follows from it.
    pub detail: String,
}

impl GateResult {
    fn pass(detail: String) -> Self {
        GateResult { passed: true, detail }
    }
    fn fail(detail: String) -> Self {
        GateResult { passed: false, detail }
    }
}

/// Gate-5 supporting data: the swept curve, the ADR-0028 fit, and the
/// recoverable-vs-leakage decomposition (C3.3).
#[derive(Clone, Debug)]
pub struct ScalingEvidence {
    /// `(period_s, energy_J)` points the fit was made over, ascending period.
    pub points: Vec<(f64, f64)>,
    /// The fitted E = k_adia/t + k_leak·t + k_res model.
    pub fit: EnergyFit,
    /// Share of the model energy carried by k_adia/t at the fastest period.
    pub adia_share_fast: f64,
    /// Share of the model energy carried by k_adia/t at the slowest period.
    pub adia_share_slow: f64,
    /// Share of the model energy carried by k_leak·t at the slowest period
    /// (the separately-reported leakage share, ADR-0028 C3.3).
    pub leak_share_slow: f64,
}

/// The full five-gate admission report for one cell at one device rung.
#[derive(Clone, Debug)]
pub struct AdmissionReport {
    /// The candidate evaluated.
    pub cell: CandidateCell,
    /// The device rung it was evaluated at.
    pub rung: DeviceRung,
    /// Gate 1: functional verdict via typed decode (B5.3).
    pub functional: GateResult,
    /// Gate 2: conservation closure (TBD-1).
    pub conservation: GateResult,
    /// Gate 3: INV-TOKEN asserted-rail invariance.
    pub token_invariant: GateResult,
    /// Gate 4: reverse drive (TH-6) — recorded, NON-blocking.
    pub reverse_drive: GateResult,
    /// Gate 5: adiabatic scaling fit (ADR-0028).
    pub scaling: GateResult,
    /// Gate-5 curve + fit + share decomposition.
    pub scaling_evidence: ScalingEvidence,
}

impl AdmissionReport {
    /// The admission predicate: gates 1, 2, 3, and 5 must pass. Gate 4
    /// (reverse drive) is recorded but non-blocking per the spec: a failing
    /// reverse drive routes to the mirrored-netlist fallback, not rejection.
    pub fn admitted(&self) -> bool {
        self.functional.passed
            && self.conservation.passed
            && self.token_invariant.passed
            && self.scaling.passed
    }

    /// A multi-line human-readable rendering of the whole report.
    pub fn summary(&self) -> String {
        let g = |r: &GateResult| format!("{} — {}", if r.passed { "PASS" } else { "FAIL" }, r.detail);
        format!(
            "ADMISSION {:?} @ {:?}: {}\n  [1 functional]   {}\n  [2 conservation] {}\n  [3 inv-token]    {}\n  [4 reverse]      {} (recorded, non-blocking)\n  [5 scaling]      {}",
            self.cell,
            self.rung,
            if self.admitted() { "ADMITTED" } else { "REJECTED" },
            g(&self.functional),
            g(&self.conservation),
            g(&self.token_invariant),
            g(&self.reverse_drive),
            g(&self.scaling),
        )
    }
}

/// Run all five admission gates for `cell` at `rung`.
///
/// Errors are ngspice-infrastructure failures only; a measured gate failure
/// is reported inside the [`AdmissionReport`], and a reverse-drive run that
/// fails outright is *recorded* as a gate-4 failure rather than an error.
pub fn admit(cell: CandidateCell, rung: DeviceRung) -> io::Result<AdmissionReport> {
    match cell {
        CandidateCell::CswapK0 | CandidateCell::CswapK1 | CandidateCell::CswapK2 | CandidateCell::CswapK3 => {
            admit_cswap(cell, cell.cswap_k().expect("cswap cell"), rung)
        }
        CandidateCell::AdiabaticTg => admit_tg(rung),
        CandidateCell::CmosBaseline => admit_cmos(rung),
        CandidateCell::PfalBuffer => admit_pfal(rung),
    }
}

// ---------------------------------------------------------------------------
// Shared plumbing
// ---------------------------------------------------------------------------

/// The rung's canonical functional/closure period (the same operating points
/// the existing truth-table tests use).
fn functional_period(rung: DeviceRung) -> f64 {
    match rung {
        DeviceRung::Rung0Level1 => 1e-6,
        DeviceRung::Rung1Sky130 => 1e-7,
    }
}

/// The gate-5 sweep for the adiabatic cells: 5 points across the rung's
/// adiabatic window (5 points ⇒ 2 residual degrees of freedom for the
/// 3-parameter fit; 3 points would fit exactly and prove nothing).
fn sweep_periods(rung: DeviceRung) -> Vec<f64> {
    match rung {
        DeviceRung::Rung0Level1 => vec![1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6],
        DeviceRung::Rung1Sky130 => vec![3.16e-9, 1e-8, 3.16e-8, 1e-7, 3.16e-7],
    }
}

/// Extract a required measurement, panicking with context if missing (an
/// infrastructure failure: the deck emitted it).
fn meas(out: &str, name: &str) -> f64 {
    parse_meas(out, name)
        .unwrap_or_else(|| panic!("measurement '{name}' missing from ngspice output"))
}

/// Insert extra `.meas` directives just before the deck's `.endc`.
fn inject_meas(deck: &str, extra: &[String]) -> String {
    let block = format!("\n{}\n.endc", extra.join("\n"));
    deck.replacen("\n.endc", &block, 1)
}

/// Parse a SPICE number with an optional scale suffix (`5f`, `100f`, `1p`).
fn spice_value(s: &str) -> f64 {
    let s = s.trim().to_lowercase();
    if let Ok(v) = s.parse::<f64>() {
        return v;
    }
    for (suffix, mult) in [
        ("meg", 1e6),
        ("f", 1e-15),
        ("p", 1e-12),
        ("n", 1e-9),
        ("u", 1e-6),
        ("m", 1e-3),
        ("k", 1e3),
    ] {
        if let Some(prefix) = s.strip_suffix(suffix) {
            if let Ok(v) = prefix.parse::<f64>() {
                return v * mult;
            }
        }
    }
    panic!("cannot parse SPICE value '{s}'")
}

/// Every explicit capacitor in a deck: `(node, farads)`. Grounded two-node
/// caps only (which is all these decks emit).
fn parse_caps(deck: &str) -> Vec<(String, f64)> {
    deck.lines()
        .filter_map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() == 4 && parts[0].starts_with('C') && parts[2] == "0" {
                Some((parts[1].to_string(), spice_value(parts[3])))
            } else {
                None
            }
        })
        .collect()
}

/// The shared closure verdict: settled-cycle periodicity, per-source sum
/// consistency, and explicit-storage return, all within 1% of the per-cycle
/// source energy (absolute floor [`CLOSURE_ABS_FLOOR_J`]).
fn closure_result(label: &str, e_a: f64, e_b: f64, parts_sum: f64, d_stored: f64) -> GateResult {
    let tol = (CLOSURE_TOL * e_a.abs().max(e_b.abs())).max(CLOSURE_ABS_FLOOR_J);
    let d_periodic = (e_b - e_a).abs();
    let d_sum = (parts_sum - e_a).abs();
    let d_sto = d_stored.abs();
    let passed = d_periodic <= tol && d_sum <= tol && d_sto <= tol;
    GateResult {
        passed,
        detail: format!(
            "{label}: E_src(cycle n)={e_a:.3e} J, E_src(cycle n+1)={e_b:.3e} J (Δ={d_periodic:.2e}), \
             Σ per-source={parts_sum:.3e} J (Δ={d_sum:.2e}), ΔE_stored(explicit caps)={d_stored:.2e} J; \
             tol={tol:.2e} J (1% of per-cycle source energy, floor {CLOSURE_ABS_FLOOR_J:.0e} J). \
             Settled periodicity ⇒ ΔE_stored≈0 ⇒ E_source=E_dissipated within tolerance"
        ),
    }
}

/// The shared gate-5 verdict: ADR-0028 fit quality plus recoverable-term
/// dominance at both ends of the swept range.
fn scaling_verdict(points: Vec<(f64, f64)>) -> (GateResult, ScalingEvidence) {
    assert!(points.len() >= 5, "gate 5 needs >= 5 sweep points (3 fit exactly)");
    let fit = energy::fit(&points);
    let t_min = points.first().expect("nonempty").0;
    let t_max = points.last().expect("nonempty").0;
    let e_min = fit.eval(t_min);
    let e_max = fit.eval(t_max);
    let (adia_share_fast, adia_share_slow, leak_share_slow) = if e_min > 0.0 && e_max > 0.0 {
        (
            (fit.k_adia / t_min) / e_min,
            (fit.k_adia / t_max) / e_max,
            (fit.k_leak * t_max) / e_max,
        )
    } else {
        (f64::NEG_INFINITY, f64::NEG_INFINITY, f64::INFINITY)
    };
    let fit_ok = fit.adequate(MIN_FIT_R2);
    let dominance_ok = adia_share_fast >= MIN_ADIA_SHARE && adia_share_slow >= MIN_ADIA_SHARE;
    let detail = format!(
        "E=k_adia/t+k_leak*t+k_res over [{t_min:.2e}, {t_max:.2e}] s ({} pts): R²={:.5} (min {MIN_FIT_R2}), \
         k_adia={:.3e} J·s (recoverable), k_leak={:.3e} J/s, k_res={:.3e} J; \
         adiabatic share {:.1}% (fast) / {:.1}% (slow), leakage share at slow end {:.1}% \
         [fit {}, adiabatic dominance (≥{:.0}% both ends) {}]",
        points.len(),
        fit.r_squared,
        fit.k_adia,
        fit.k_leak,
        fit.k_res,
        100.0 * adia_share_fast,
        100.0 * adia_share_slow,
        100.0 * leak_share_slow,
        if fit_ok { "adequate" } else { "INADEQUATE" },
        100.0 * MIN_ADIA_SHARE,
        if dominance_ok { "holds" } else { "FAILS" },
    );
    // A negative k_leak means the sweep window never reaches the leakage
    // knee, so k_leak is collinear with k_res and not separately resolvable
    // — the share numbers are then a fit artifact, not physics.
    let detail = if fit.k_leak < 0.0 {
        format!(
            "{detail}; NOTE: k_leak < 0 — the leakage term is not resolvable over this window \
             (collinear with k_res); use the dedicated static-leakage deck for the floor"
        )
    } else {
        detail
    };
    let evidence = ScalingEvidence {
        points,
        fit,
        adia_share_fast,
        adia_share_slow,
        leak_share_slow,
    };
    (
        GateResult { passed: fit_ok && dominance_ok, detail },
        evidence,
    )
}

// ---------------------------------------------------------------------------
// k-CSWAP admission
// ---------------------------------------------------------------------------

/// Token-line base name for (signal, polarity) — mirrors the (private)
/// convention in `cswap.rs`: `at`/`af`, `c1t`/`c1f`, ...
fn line_name(sig: &str, pol: bool) -> String {
    format!("{sig}{}", if pol { 't' } else { 'f' })
}

/// Read a signal off a token (mirrors the private `CswapToken::get`).
fn token_sig(t: CswapToken, sig: &str) -> bool {
    match sig {
        "c" | "c1" => t.c1,
        "c2" => t.c2,
        "c3" => t.c3,
        "a" => t.a,
        "b" => t.b,
        other => panic!("unknown signal {other}"),
    }
}

/// The closure orbit for the k-cell: a short token sequence that pulses
/// **every token line** and conducts **every branch** of the cell, so that
/// at rung 1 every output wire and every internal node is actively driven
/// and re-parked within each orbit instead of floating. (An all-active
/// token repeated forever leaves the 0-lines' wires permanently floating;
/// on sky130 they leak-drift for many cycles and the explicit-cap
/// ΔE_stored reads their drift, not a closure violation — measured:
/// −1.76 fJ/cycle on k0 at T = 1e-7 with the all-active token, versus
/// ≈ 0 with this orbit.)
fn closure_orbit(k: usize) -> Vec<CswapToken> {
    let t = |c1, c2, c3, a, b| CswapToken { c1, c2, c3, a, b };
    match k {
        // Both a-lines pulse.
        0 => vec![t(false, false, false, true, false), t(false, false, false, false, false)],
        // Covers c∧b, c∧¬b, c∧a, c∧¬a, ¬c∧a, ¬c∧¬a, ¬c∧b, ¬c∧¬b.
        1 => vec![
            t(true, false, false, true, false),
            t(true, false, false, false, true),
            t(false, false, false, true, false),
            t(false, false, false, false, true),
        ],
        // Covers c1∧c2∧{a,¬a,b,¬b}, ¬c1∧{a,¬a,b,¬b}, ¬c2∧{a,¬a,b,¬b}.
        2 => vec![
            t(true, true, false, true, false),
            t(true, true, false, false, true),
            t(false, true, false, true, false),
            t(false, true, false, false, true),
            t(true, false, false, true, false),
            t(true, false, false, false, true),
        ],
        // Covers c1∧c2∧c3∧{a,¬a,b,¬b}, ¬c1∧{a,¬a,b,¬b}, ¬c2∧{a,¬a,b,¬b}, ¬c3∧{a,¬a,b,¬b}.
        3 => vec![
            t(true, true, true, true, false),
            t(true, true, true, false, true),
            t(false, true, true, true, false),
            t(false, true, true, false, true),
            t(true, false, true, true, false),
            t(true, false, true, false, true),
            t(true, true, false, true, false),
            t(true, true, false, false, true),
        ],
        _ => panic!("k must be 0, 1, 2, or 3"),
    }
}

fn admit_cswap(cell: CandidateCell, k: usize, rung: DeviceRung) -> io::Result<AdmissionReport> {
    let period = functional_period(rung);
    let (readings, _ecyc) = run_cswap_functional(k, rung, period)?;
    let functional = cswap_functional_verdict(k, rung, &readings);
    let token_invariant = cswap_token_verdict(k, rung, &readings);
    let conservation = cswap_conservation_verdict(k, rung, period)?;
    let reverse_drive = cswap_reverse_verdict(k, rung, period);
    let curve = cswap_energy_curve(k, rung, &sweep_periods(rung))?;
    let (scaling, scaling_evidence) =
        scaling_verdict(curve.iter().map(|p| (p.period_s, p.etot.abs())).collect());
    Ok(AdmissionReport {
        cell,
        rung,
        functional,
        conservation,
        token_invariant,
        reverse_drive,
        scaling,
        scaling_evidence,
    })
}

/// Gate 1 for a k-CSWAP cell: typed decode of every per-token line reading
/// against the truth function.
fn cswap_functional_verdict(k: usize, rung: DeviceRung, readings: &[CswapReading]) -> GateResult {
    let vdd = rung.vdd();
    let mut mismatches: Vec<String> = Vec::new();
    for r in readings {
        let want = if r.expected_active { RailState::Asserted } else { RailState::Deasserted };
        let got = decode_pair(r.rail_v, r.mirror_v, vdd);
        if got != want {
            mismatches.push(format!(
                "token {} line {}: want {want:?}, decoded {got:?} (rail {:.4} V, mirror {:.4} V)",
                r.token_idx, r.line, r.rail_v, r.mirror_v
            ));
        }
    }
    let n_tokens = readings.iter().map(|r| r.token_idx).max().map_or(0, |m| m + 1);
    let combos = 1usize << cswap_signals(k).len();
    if mismatches.is_empty() {
        GateResult::pass(format!(
            "typed decode ({DECODE_LO_FRAC:.1}/{DECODE_HI_FRAC:.1}·VDD, hysteresis band non-decoding, \
             sampled mid-validity): all {} line readings across {n_tokens} tokens \
             (all {combos} control/data combos + pads) decode to the k{k} truth table at full dual-rail level",
            readings.len()
        ))
    } else {
        GateResult::fail(format!(
            "{} of {} line readings decode wrong: {}",
            mismatches.len(),
            readings.len(),
            mismatches.join("; ")
        ))
    }
}

/// Gate 3 for a k-CSWAP cell: at every sampled valid tick, each signal's
/// token-line pair is exactly 1-of-2 asserted, so the total asserted-line
/// count is invariant and equals the signal count.
fn cswap_token_verdict(k: usize, rung: DeviceRung, readings: &[CswapReading]) -> GateResult {
    let vdd = rung.vdd();
    let n_sig = cswap_signals(k).len();
    let mut violations: Vec<String> = Vec::new();
    let mut per_tick: std::collections::BTreeMap<usize, usize> = std::collections::BTreeMap::new();
    // Readings arrive (token, signal, pol=true) then (token, signal, pol=false):
    // consecutive pairs are one signal's two token lines at one tick.
    for pair in readings.chunks(2) {
        let [r1, r0] = pair else {
            violations.push("odd reading count — line pair split".to_string());
            continue;
        };
        debug_assert_eq!(r1.token_idx, r0.token_idx);
        let s1 = decode_pair(r1.rail_v, r1.mirror_v, vdd);
        let s0 = decode_pair(r0.rail_v, r0.mirror_v, vdd);
        let asserted = usize::from(s1 == RailState::Asserted) + usize::from(s0 == RailState::Asserted);
        if asserted != 1 || s1 == RailState::Undefined || s0 == RailState::Undefined {
            violations.push(format!(
                "tick {} pair {}/{}: states {s1:?}/{s0:?} ({} asserted, want exactly 1)",
                r1.token_idx, r1.line, r0.line, asserted
            ));
        }
        *per_tick.entry(r1.token_idx).or_insert(0) += asserted;
    }
    let count_bad: Vec<String> = per_tick
        .iter()
        .filter(|&(_, &c)| c != n_sig)
        .map(|(t, c)| format!("tick {t}: {c} asserted (want {n_sig})"))
        .collect();
    if violations.is_empty() && count_bad.is_empty() {
        GateResult::pass(format!(
            "asserted-rail count = {n_sig} (1-of-2 per signal pair) at every one of {} sampled valid ticks",
            per_tick.len()
        ))
    } else {
        GateResult::fail(format!(
            "INV-TOKEN violated: {}",
            violations.iter().chain(count_bad.iter()).cloned().collect::<Vec<_>>().join("; ")
        ))
    }
}

/// Gate 2 for a k-CSWAP cell: the closure orbit repeated; two consecutive
/// settled orbits compared (periodicity), per-source sum vs total over the
/// first measured orbit, and explicit-cap ΔE_stored over it from injected
/// FIND measurements. The measured orbit starts at the first orbit boundary
/// at or after cycle 8, so rung-1 devices get ≥ 5 leak-settling time
/// constants (τ ≈ 140 ns at T = 1e-7) before anything is asserted.
fn cswap_conservation_verdict(k: usize, rung: DeviceRung, period: f64) -> io::Result<GateResult> {
    let orbit = closure_orbit(k);
    let len = orbit.len();
    let n0 = (8usize).div_ceil(len) * len; // first measured orbit boundary
    let n_tokens = n0 + 2 * len + 1;
    let tokens: Vec<CswapToken> = orbit.iter().copied().cycle().take(n_tokens).collect();
    let deck = cswap_netlist(k, rung, period, &tokens, true);
    let caps = parse_caps(&deck);
    assert!(!caps.is_empty(), "cswap deck must declare explicit caps");
    let (t0, t1) = (n0 as f64 * period, (n0 + len) as f64 * period);
    let mut extra: Vec<String> = Vec::new();
    for (i, (node, _)) in caps.iter().enumerate() {
        extra.push(format!("meas tran sv{i} FIND V({node}) AT={t0:.9e}"));
        extra.push(format!("meas tran ev{i} FIND V({node}) AT={t1:.9e}"));
    }
    for (name, node) in
        [("cs0", "pp0"), ("cs2", "pp2"), ("csv", "ppv"), ("csi", "pint"), ("csa", "pakt")]
    {
        extra.push(format!("meas tran {name} INTEG V({node}) FROM={t0:.9e} TO={t1:.9e}"));
    }
    let out = run_ngspice_raw(&inject_meas(&deck, &extra))?;
    let e_orbit_a: f64 = (n0..n0 + len).map(|i| meas(&out, &format!("ecyc{i}"))).sum();
    let e_orbit_b: f64 = (n0 + len..n0 + 2 * len).map(|i| meas(&out, &format!("ecyc{i}"))).sum();
    let parts =
        meas(&out, "cs0") + meas(&out, "cs2") + meas(&out, "csv") + meas(&out, "csi") + meas(&out, "csa");
    let mut d_stored = 0.0;
    for (i, (_, c)) in caps.iter().enumerate() {
        let v0 = meas(&out, &format!("sv{i}"));
        let v1 = meas(&out, &format!("ev{i}"));
        d_stored += 0.5 * c * (v1 * v1 - v0 * v0);
    }
    Ok(closure_result(
        &format!(
            "k{k} closure orbit (len {len}, every line pulsed, every branch conducted), \
             T={period:.1e} s, settled orbits over cycles {n0}..{}, {} explicit caps",
            n0 + 2 * len,
            caps.len()
        ),
        e_orbit_a,
        e_orbit_b,
        parts,
        d_stored,
    ))
}

/// The PWL payload span of a source line.
fn pwl_payload(line: &str) -> &str {
    let s = line.find("PWL(").expect("PWL source line") + 4;
    let e = line.rfind(')').expect("closing paren");
    &line[s..e]
}

fn replace_pwl_payload(line: &str, payload: &str) -> String {
    let s = line.find("PWL(").expect("PWL source line") + 4;
    let e = line.rfind(')').expect("closing paren");
    format!("{}{}{}", &line[..s], payload, &line[e..])
}

/// Generate the reversed-phase deck: exchange the input token-line stimuli
/// with the delayed-replica stimuli (see module docs — for the single-stage
/// cell this IS the time-reversed phase sequence, modulo a global one-tick
/// shift absorbed by keeping the measurement grid fixed).
fn swap_input_replica_sources(deck: &str, k: usize) -> String {
    let mut lines: Vec<String> = deck.lines().map(str::to_string).collect();
    for &sig in cswap_signals(k) {
        for pol in [true, false] {
            let base = line_name(sig, pol);
            for rail in ['p', 'm'] {
                let ia_prefix = format!("V{base}{rail} ");
                let ib_prefix = format!("Vr{base}{rail} ");
                let ia = lines
                    .iter()
                    .position(|l| l.starts_with(&ia_prefix))
                    .unwrap_or_else(|| panic!("input source {ia_prefix} not found"));
                let ib = lines
                    .iter()
                    .position(|l| l.starts_with(&ib_prefix))
                    .unwrap_or_else(|| panic!("replica source {ib_prefix} not found"));
                let pa = pwl_payload(&lines[ia]).to_string();
                let pb = pwl_payload(&lines[ib]).to_string();
                lines[ia] = replace_pwl_payload(&lines[ia], &pb);
                lines[ib] = replace_pwl_payload(&lines[ib], &pa);
            }
        }
    }
    lines.join("\n") + "\n"
}

/// Gate 4 for a k-CSWAP cell. Never errors: any failure (including an
/// ngspice crash on the reversed deck) is *recorded* per the spec.
fn cswap_reverse_verdict(k: usize, rung: DeviceRung, period: f64) -> GateResult {
    let (deck, tokens) = cswap_functional_netlist(k, rung, period);
    let deck = swap_input_replica_sources(&deck, k);
    let out = match run_ngspice_raw(&deck) {
        Ok(out) => out,
        Err(e) => {
            return GateResult::fail(format!(
                "reverse-drive simulation failed outright ({e}) — recorded; routes to the \
                 mirrored-netlist fallback, not rejection"
            ))
        }
    };
    let vdd = rung.vdd();
    let mut total = 0usize;
    let mut mismatches: Vec<String> = Vec::new();
    for (j, &t) in tokens.iter().enumerate() {
        // Self-inverse cell: the adjoint truth table IS the forward table.
        let adj = cswap_out(k, t);
        for &sig in cswap_signals(k) {
            for pol in [true, false] {
                let base = format!("o{}", line_name(sig, pol));
                total += 1;
                let (Some(rail), Some(mirror)) = (
                    parse_meas(&out, &format!("v{j}{base}p")),
                    parse_meas(&out, &format!("v{j}{base}m")),
                ) else {
                    mismatches.push(format!("token {j} line {base}: measurement missing"));
                    continue;
                };
                let want = if token_sig(adj, sig) == pol {
                    RailState::Asserted
                } else {
                    RailState::Deasserted
                };
                let got = decode_pair(rail, mirror, vdd);
                if got != want {
                    mismatches.push(format!(
                        "token {j} line {base}: want {want:?}, decoded {got:?} \
                         (rail {rail:.4} V, mirror {mirror:.4} V)"
                    ));
                }
            }
        }
    }
    if mismatches.is_empty() {
        GateResult::pass(format!(
            "phase sequence reversed (forward/reverse port stimuli exchanged ≡ clock run backward \
             modulo a global tick shift): adjoint (= self-inverse) k{k} truth table reproduced over \
             all {total} line readings"
        ))
    } else {
        let shown: Vec<String> = mismatches.iter().take(4).cloned().collect();
        GateResult::fail(format!(
            "reverse drive off the adjoint table on {}/{total} readings ({}{}) — recorded; routes \
             to the mirrored-netlist fallback, not rejection",
            mismatches.len(),
            shown.join("; "),
            if mismatches.len() > shown.len() { "; ..." } else { "" }
        ))
    }
}

// ---------------------------------------------------------------------------
// Adiabatic TG buffer admission
// ---------------------------------------------------------------------------

/// Transient-step policy shared with the `adiabatic.rs` decks.
fn tran_step(rung: DeviceRung, period: f64) -> f64 {
    match rung {
        DeviceRung::Rung0Level1 => (period / 4000.0).min(5e-11),
        DeviceRung::Rung1Sky130 => (period / 4000.0).min(5e-11).max(period / 40_000.0),
    }
}

/// The TG buffer admission deck: the held-ON transmission gate charging its
/// load from a repeating trapezoidal power-clock, 4 cycles, with per-cycle
/// energy integrals, hold/rest level measurements on cycles 1..3, cycle-
/// boundary FINDs, and per-source integrals over cycle 2. `reversed`
/// time-reverses the trapezoid (rise/hold/fall/low → low/rise/hold/fall).
///
/// `variant` selects the rung-1 (Vt × body) device cards (ignored at rung 0,
/// which has one device flavor); `c_load_f` is the output load. The
/// pre-matrix behavior is `(Sky130Variant::default(), 1e-12)`.
fn tg_admission_deck(
    rung: DeviceRung,
    period: f64,
    reversed: bool,
    variant: Sky130Variant,
    c_load_f: f64,
) -> String {
    let q = period / 4.0;
    let vdd = rung.vdd();
    let step = tran_step(rung, period);
    let stop = 4.0 * period;
    let pwl = if reversed {
        format!("0 0 {q:.9e} 0 {:.9e} {vdd} {:.9e} {vdd} {period:.9e} 0", 2.0 * q, 3.0 * q)
    } else {
        format!("0 0 {q:.9e} {vdd} {:.9e} {vdd} {:.9e} 0 {period:.9e} 0", 2.0 * q, 3.0 * q)
    };
    // Hold/low window offsets within a cycle, per clock direction.
    let (hold_off, rest_off) = if reversed { (2.0 * q, 0.0) } else { (q, 3.0 * q) };
    let mut l: Vec<String> = Vec::new();
    l.push(format!(
        "adiabatic TG buffer admission deck, {rung:?} variant {} load {c_load_f:.4e} F (T={period:.6e} s{})",
        variant.key(),
        if reversed { ", reversed phase sequence" } else { "" }
    ));
    l.push(rung.model_header());
    l.push(format!("Vdd vdd 0 DC {vdd}"));
    l.push(format!("Vpc pc 0 PWL({pwl}) r=0"));
    l.push(format!("Vin ing 0 DC {vdd}"));
    l.push("Vinb inb 0 DC 0.0".into());
    match rung {
        DeviceRung::Rung0Level1 => {
            l.push(rung.fet("1", FetKind::Nmos, "pc", "ing", "out", "0", 4.0, 0.15));
            l.push(rung.fet("2", FetKind::Pmos, "pc", "inb", "out", "vdd", 8.0, 0.15));
        }
        DeviceRung::Rung1Sky130 => l.extend(variant.tg_cards("1", "2")),
    }
    l.push(format!("Cout out 0 {c_load_f:.4e}"));
    l.push("Bpc ppc 0 V = -V(pc)*I(Vpc)".into());
    l.push("Bvd pvd 0 V = -V(vdd)*I(Vdd)".into());
    l.push("Bgt pgt 0 V = -V(ing)*I(Vin)-V(inb)*I(Vinb)".into());
    l.push("Btot ptot 0 V = V(ppc)+V(pvd)+V(pgt)".into());
    l.push(".control".into());
    l.push(format!("tran {step:.9e} {stop:.9e} uic"));
    for i in 0..4 {
        let f = i as f64 * period;
        l.push(format!("meas tran ecyc{i} INTEG V(ptot) FROM={f:.9e} TO={:.9e}", f + period));
    }
    for c in 1..=3usize {
        let base = c as f64 * period;
        l.push(format!(
            "meas tran vh{c} MIN V(out) FROM={:.9e} TO={:.9e}",
            base + hold_off + 0.2 * q,
            base + hold_off + 0.8 * q
        ));
        l.push(format!(
            "meas tran vr{c} MAX V(out) FROM={:.9e} TO={:.9e}",
            base + rest_off + 0.2 * q,
            base + rest_off + 0.8 * q
        ));
    }
    let (t0, t1) = (2.0 * period, 3.0 * period);
    l.push(format!("meas tran sv0 FIND V(out) AT={t0:.9e}"));
    l.push(format!("meas tran ev0 FIND V(out) AT={t1:.9e}"));
    for (name, node) in [("cpc", "ppc"), ("cvd", "pvd"), ("cgt", "pgt")] {
        l.push(format!("meas tran {name} INTEG V({node}) FROM={t0:.9e} TO={t1:.9e}"));
    }
    l.push(".endc".into());
    l.push(".end".into());
    l.join("\n") + "\n"
}

fn admit_tg(rung: DeviceRung) -> io::Result<AdmissionReport> {
    admit_tg_impl(rung, Sky130Variant::default(), 1e-12, None)
}

/// Run the five TG-buffer admission gates for one rung-1 sky130 (Vt × body)
/// `variant` at load `c_load_f` — the per-variant admission of the M3a W5
/// bench DUT matrix.
///
/// `scaling_points` is the variant's own `(period_s, energy_J)` sweep
/// (ascending, ≥ 5 points — the gate-5 minimum), normally the very curve the
/// matrix driver just characterized into the DB, so admission re-simulates
/// only the gate-1..4 functional/closure/reverse decks and never repeats the
/// sweep. Gates and thresholds are identical to [`admit`] on
/// [`CandidateCell::AdiabaticTg`]; the default variant at 1 pF reproduces it
/// exactly.
pub fn admit_tg_sky130_variant(
    variant: Sky130Variant,
    c_load_f: f64,
    scaling_points: Vec<(f64, f64)>,
) -> io::Result<AdmissionReport> {
    admit_tg_impl(DeviceRung::Rung1Sky130, variant, c_load_f, Some(scaling_points))
}

fn admit_tg_impl(
    rung: DeviceRung,
    variant: Sky130Variant,
    c_load_f: f64,
    scaling_points: Option<Vec<(f64, f64)>>,
) -> io::Result<AdmissionReport> {
    let period = functional_period(rung);
    let vdd = rung.vdd();
    let out = run_ngspice_raw(&tg_admission_deck(rung, period, false, variant, c_load_f))?;

    // Gate 1: single-row truth table (buffer passes the clock token), typed
    // single-rail decode, sampled mid-hold and mid-rest of the settled cycle.
    let vh2 = meas(&out, "vh2");
    let vr2 = meas(&out, "vr2");
    let functional = if decode_rail(vh2, vdd) == RailState::Asserted
        && decode_rail(vr2, vdd) == RailState::Deasserted
    {
        GateResult::pass(format!(
            "typed decode ({DECODE_LO_FRAC:.1}/{DECODE_HI_FRAC:.1}·VDD): buffer truth row reproduced \
             — out {vh2:.4} V (min, hold tick) / {vr2:.4} V (max, rest tick), VDD {vdd}"
        ))
    } else {
        GateResult::fail(format!(
            "buffer output failed typed decode: hold min {vh2:.4} V, rest max {vr2:.4} V, VDD {vdd}"
        ))
    };

    // Gate 3: single-ended primitive — the invariant degenerates (see docs).
    let holds: Vec<f64> = (1..=3).map(|c| meas(&out, &format!("vh{c}"))).collect();
    let all_asserted = holds.iter().all(|&v| decode_rail(v, vdd) == RailState::Asserted);
    let token_invariant = if all_asserted {
        GateResult::pass(format!(
            "asserted-rail count = 1 at all 3 sampled valid ticks (hold mins {holds:?} V). \
             Caveat: the raw TG is single-ended, so INV-TOKEN degenerates to its one rail; \
             the dual-rail 1-of-2 discipline is checked on the k-CSWAP stages"
        ))
    } else {
        GateResult::fail(format!("output rail not asserted at every valid tick: {holds:?} V"))
    };

    // Gate 2: closure over settled cycles 2 and 3.
    let (e2, e3) = (meas(&out, "ecyc2"), meas(&out, "ecyc3"));
    let parts = meas(&out, "cpc") + meas(&out, "cvd") + meas(&out, "cgt");
    let (v0, v1) = (meas(&out, "sv0"), meas(&out, "ev0"));
    let d_stored = 0.5 * c_load_f * (v1 * v1 - v0 * v0);
    let conservation = closure_result(
        &format!(
            "TG buffer [{} C={c_load_f:.4e} F], T={period:.1e} s, settled cycle 2→3",
            variant.key()
        ),
        e2,
        e3,
        parts,
        d_stored,
    );

    // Gate 4: time-reversed trapezoid; the buffer is self-inverse (identity
    // adjoint), so the same single-row table must decode.
    let reverse_drive = match run_ngspice_raw(&tg_admission_deck(rung, period, true, variant, c_load_f)) {
        Err(e) => GateResult::fail(format!(
            "reverse-drive simulation failed outright ({e}) — recorded, non-blocking"
        )),
        Ok(rout) => {
            let rvh = meas(&rout, "vh2");
            let rvr = meas(&rout, "vr2");
            if decode_rail(rvh, vdd) == RailState::Asserted
                && decode_rail(rvr, vdd) == RailState::Deasserted
            {
                GateResult::pass(format!(
                    "time-reversed trapezoid: identity adjoint reproduced \
                     (hold min {rvh:.4} V, rest max {rvr:.4} V)"
                ))
            } else {
                GateResult::fail(format!(
                    "reversed clock failed decode (hold min {rvh:.4} V, rest max {rvr:.4} V) — \
                     recorded, non-blocking"
                ))
            }
        }
    };

    // Gate 5: the cell's own E(period) curve — the caller-supplied variant
    // sweep when given (the matrix path), else the canonical-cell sweep.
    let points = match scaling_points {
        Some(p) => p,
        None => adiabatic_tg_curve_rung(&sweep_periods(rung), rung).points,
    };
    let (scaling, scaling_evidence) = scaling_verdict(points);
    Ok(AdmissionReport {
        cell: CandidateCell::AdiabaticTg,
        rung,
        functional,
        conservation,
        token_invariant,
        reverse_drive,
        scaling,
        scaling_evidence,
    })
}

// ---------------------------------------------------------------------------
// Baseline DUTs (must be REJECTED)
// ---------------------------------------------------------------------------

/// Static CMOS inverter admission deck: square-wave input (delayed a quarter
/// period so cycle boundaries fall in a stable half), 4 cycles, per-cycle
/// energy, functional windows on the settled cycle, closure measurements.
fn cmos_admission_deck(rung: DeviceRung, period: f64) -> String {
    let vdd = rung.vdd();
    let step = tran_step(rung, period);
    let stop = 4.0 * period;
    let half = period / 2.0;
    let edge = (period / 1000.0).min(1e-10);
    let td = 0.25 * period;
    let mut l: Vec<String> = Vec::new();
    l.push(format!("static CMOS inverter admission deck, {rung:?} (T={period:.6e} s)"));
    l.push(rung.model_header());
    l.push(format!("Vdd vdd 0 DC {vdd}"));
    l.push(format!(
        "Vin in 0 PULSE(0 {vdd} {td:.9e} {edge:.9e} {edge:.9e} {half:.9e} {period:.9e})"
    ));
    l.push(rung.fet("1", FetKind::Nmos, "out", "in", "0", "0", 4.0, 0.15));
    l.push(rung.fet("2", FetKind::Pmos, "out", "in", "vdd", "vdd", 8.0, 0.15));
    l.push("Cout out 0 1p".into());
    l.push("Bvd pvd 0 V = -V(vdd)*I(Vdd)".into());
    l.push("Bin pin 0 V = -V(in)*I(Vin)".into());
    l.push("Btot ptot 0 V = V(pvd)+V(pin)".into());
    l.push(".control".into());
    l.push(format!("tran {step:.9e} {stop:.9e} uic"));
    for i in 0..4 {
        let f = i as f64 * period;
        l.push(format!("meas tran ecyc{i} INTEG V(ptot) FROM={f:.9e} TO={:.9e}", f + period));
    }
    // Input high on [2T+0.25T, 2T+0.75T] → out low; low after → out high.
    let b = 2.0 * period;
    l.push(format!(
        "meas tran vlow MAX V(out) FROM={:.9e} TO={:.9e}",
        b + 0.35 * period,
        b + 0.65 * period
    ));
    l.push(format!(
        "meas tran vhigh MIN V(out) FROM={:.9e} TO={:.9e}",
        b + 0.85 * period,
        b + 1.15 * period
    ));
    l.push(format!("meas tran sv0 FIND V(out) AT={b:.9e}"));
    l.push(format!("meas tran ev0 FIND V(out) AT={:.9e}", b + period));
    for (name, node) in [("cvd", "pvd"), ("cin", "pin")] {
        l.push(format!("meas tran {name} INTEG V({node}) FROM={b:.9e} TO={:.9e}", b + period));
    }
    l.push(".endc".into());
    l.push(".end".into());
    l.join("\n") + "\n"
}

fn admit_cmos(rung: DeviceRung) -> io::Result<AdmissionReport> {
    let period = functional_period(rung);
    let vdd = rung.vdd();
    let out = run_ngspice_raw(&cmos_admission_deck(rung, period))?;

    let (vlow, vhigh) = (meas(&out, "vlow"), meas(&out, "vhigh"));
    let functional = if decode_rail(vhigh, vdd) == RailState::Asserted
        && decode_rail(vlow, vdd) == RailState::Deasserted
    {
        GateResult::pass(format!(
            "inverter truth table reproduced via typed decode (out {vhigh:.4} V min on in=0, \
             {vlow:.4} V max on in=1) — the baseline computes; its rejection is reversibility, \
             not function"
        ))
    } else {
        GateResult::fail(format!(
            "inverter failed typed decode: out min {vhigh:.4} V (in=0), out max {vlow:.4} V (in=1)"
        ))
    };

    let (e2, e3) = (meas(&out, "ecyc2"), meas(&out, "ecyc3"));
    let parts = meas(&out, "cvd") + meas(&out, "cin");
    let (v0, v1) = (meas(&out, "sv0"), meas(&out, "ev0"));
    let d_stored = 0.5 * 1e-12 * (v1 * v1 - v0 * v0);
    let conservation = closure_result(
        &format!("CMOS inverter, T={period:.1e} s, settled cycle 2→3"),
        e2,
        e3,
        parts,
        d_stored,
    );

    let token_invariant = GateResult::fail(
        "single-rail static gate: no dual-rail token pairs exist, so the INV-TOKEN \
         asserted-rail invariant is not satisfiable — irreversible encoding"
            .to_string(),
    );
    let reverse_drive = GateResult::fail(
        "no power-clock phase sequence to reverse (rail-driven static gate)".to_string(),
    );

    let periods = vec![1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6];
    let curve = cmos_baseline_curve_rung(&periods, rung);
    let (scaling, scaling_evidence) = scaling_verdict(curve.points);
    Ok(AdmissionReport {
        cell: CandidateCell::CmosBaseline,
        rung,
        functional,
        conservation,
        token_invariant,
        reverse_drive,
        scaling,
        scaling_evidence,
    })
}

/// PFAL buffer admission deck for one DC input bit: cross-coupled PMOS latch
/// + NMOS eval pair on a repeating trapezoidal clock, 4 cycles.
fn pfal_admission_deck(rung: DeviceRung, period: f64, bit: bool) -> String {
    let q = period / 4.0;
    let vdd = rung.vdd();
    let step = tran_step(rung, period);
    let stop = 4.0 * period;
    let (vin, vinb) = if bit { (vdd, 0.0) } else { (0.0, vdd) };
    let (act, inact) = if bit { ("out", "outb") } else { ("outb", "out") };
    let mut l: Vec<String> = Vec::new();
    l.push(format!(
        "PFAL buffer admission deck, {rung:?} (T={period:.6e} s, in={})",
        u8::from(bit)
    ));
    l.push(rung.model_header());
    l.push(format!("Vdd vdd 0 DC {vdd}"));
    l.push(format!(
        "Vpc pc 0 PWL(0 0 {q:.9e} {vdd} {:.9e} {vdd} {:.9e} 0 {period:.9e} 0) r=0",
        2.0 * q,
        3.0 * q
    ));
    l.push(format!("Vin in 0 DC {vin}"));
    l.push(format!("Vinb inb 0 DC {vinb}"));
    l.push(rung.fet("P1", FetKind::Pmos, "pc", "out", "outb", "vdd", 8.0, 0.15));
    l.push(rung.fet("P2", FetKind::Pmos, "pc", "outb", "out", "vdd", 8.0, 0.15));
    l.push(rung.fet("N1", FetKind::Nmos, "pc", "in", "out", "0", 4.0, 0.15));
    l.push(rung.fet("N2", FetKind::Nmos, "pc", "inb", "outb", "0", 4.0, 0.15));
    l.push("Cout out 0 100f".into());
    l.push("Coutb outb 0 100f".into());
    l.push("Bpc ppc 0 V = -V(pc)*I(Vpc)".into());
    l.push("Bvd pvd 0 V = -V(vdd)*I(Vdd)".into());
    l.push("Bgt pgt 0 V = -V(in)*I(Vin)-V(inb)*I(Vinb)".into());
    l.push("Btot ptot 0 V = V(ppc)+V(pvd)+V(pgt)".into());
    l.push(".control".into());
    l.push(format!("tran {step:.9e} {stop:.9e} uic"));
    for i in 0..4 {
        let f = i as f64 * period;
        l.push(format!("meas tran ecyc{i} INTEG V(ptot) FROM={f:.9e} TO={:.9e}", f + period));
    }
    for c in 1..=3usize {
        let w0 = c as f64 * period + q + 0.2 * q;
        let w1 = c as f64 * period + 2.0 * q - 0.2 * q;
        l.push(format!("meas tran va{c} MIN V({act}) FROM={w0:.9e} TO={w1:.9e}"));
        l.push(format!("meas tran vi{c} MAX V({inact}) FROM={w0:.9e} TO={w1:.9e}"));
    }
    let (t0, t1) = (2.0 * period, 3.0 * period);
    l.push(format!("meas tran svo FIND V(out) AT={t0:.9e}"));
    l.push(format!("meas tran evo FIND V(out) AT={t1:.9e}"));
    l.push(format!("meas tran svb FIND V(outb) AT={t0:.9e}"));
    l.push(format!("meas tran evb FIND V(outb) AT={t1:.9e}"));
    for (name, node) in [("cpc", "ppc"), ("cvd", "pvd"), ("cgt", "pgt")] {
        l.push(format!("meas tran {name} INTEG V({node}) FROM={t0:.9e} TO={t1:.9e}"));
    }
    l.push(".endc".into());
    l.push(".end".into());
    l.join("\n") + "\n"
}

fn admit_pfal(rung: DeviceRung) -> io::Result<AdmissionReport> {
    let period = functional_period(rung);
    let vdd = rung.vdd();
    let mut functional_ok = true;
    let mut functional_notes: Vec<String> = Vec::new();
    let mut token_ok = true;
    let mut token_notes: Vec<String> = Vec::new();
    let mut closures: Vec<GateResult> = Vec::new();

    for bit in [true, false] {
        let out = run_ngspice_raw(&pfal_admission_deck(rung, period, bit))?;
        let (va, vi) = (meas(&out, "va2"), meas(&out, "vi2"));
        let row_ok = decode_pair(va, vi, vdd) == RailState::Asserted;
        functional_ok &= row_ok;
        functional_notes.push(format!(
            "in={}: active rail {va:.4} V (min), inactive {vi:.4} V (max) — {}",
            u8::from(bit),
            if row_ok { "decodes" } else { "FAILS decode" }
        ));
        for c in 1..=3usize {
            let (a, i) = (meas(&out, &format!("va{c}")), meas(&out, &format!("vi{c}")));
            if decode_pair(a, i, vdd) != RailState::Asserted {
                token_ok = false;
                token_notes.push(format!(
                    "in={} tick {c}: pair not 1-of-2 (active {a:.4} V, inactive {i:.4} V)",
                    u8::from(bit)
                ));
            }
        }
        let (e2, e3) = (meas(&out, "ecyc2"), meas(&out, "ecyc3"));
        let parts = meas(&out, "cpc") + meas(&out, "cvd") + meas(&out, "cgt");
        let (svo, evo) = (meas(&out, "svo"), meas(&out, "evo"));
        let (svb, evb) = (meas(&out, "svb"), meas(&out, "evb"));
        let d_stored = 0.5 * 100e-15 * ((evo * evo - svo * svo) + (evb * evb - svb * svb));
        closures.push(closure_result(
            &format!("PFAL in={}, T={period:.1e} s, settled cycle 2→3", u8::from(bit)),
            e2,
            e3,
            parts,
            d_stored,
        ));
    }

    let functional = GateResult {
        passed: functional_ok,
        detail: format!("clocked-buffer truth table via typed decode: {}", functional_notes.join("; ")),
    };
    let token_invariant = if token_ok {
        GateResult::pass(
            "asserted-rail count = 1 (out/outb exactly 1-of-2) at all 6 sampled valid ticks \
             across both input rows"
                .to_string(),
        )
    } else {
        GateResult::fail(format!("INV-TOKEN violated: {}", token_notes.join("; ")))
    };
    let conservation = GateResult {
        passed: closures.iter().all(|c| c.passed),
        detail: closures.iter().map(|c| c.detail.clone()).collect::<Vec<_>>().join(" | "),
    };
    let reverse_drive = GateResult::fail(
        "single-phase power-clock: reversing the phase sequence degenerates to a pure time \
         shift, so there is no adjoint witness"
            .to_string(),
    );

    // Wide sweep so the floor (and the post-floor rise) is inside the range.
    let periods = vec![1e-8, 1e-7, 1e-6, 1e-5, 1e-4];
    let curve = pfal_curve_rung(&periods, rung);
    let (scaling, scaling_evidence) = scaling_verdict(curve.points);
    Ok(AdmissionReport {
        cell: CandidateCell::PfalBuffer,
        rung,
        functional,
        conservation,
        token_invariant,
        reverse_drive,
        scaling,
        scaling_evidence,
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adiabatic::sky130_available;
    use crate::bench::ngspice_available;

    /// `admitted()` is gates 1∧2∧3∧5; gate 4 (reverse drive) is recorded but
    /// never blocks — pure predicate logic, no simulation.
    #[test]
    fn admitted_predicate_gates_1_2_3_5_not_4() {
        let pass = || GateResult::pass("p".into());
        let fail = || GateResult::fail("f".into());
        let ev = ScalingEvidence {
            points: vec![(1.0, 1.0)],
            fit: EnergyFit { k_adia: 1.0, k_leak: 0.0, k_res: 0.0, r_squared: 1.0 },
            adia_share_fast: 1.0,
            adia_share_slow: 1.0,
            leak_share_slow: 0.0,
        };
        let report = |f: GateResult, c: GateResult, t: GateResult, r: GateResult, s: GateResult| {
            AdmissionReport {
                cell: CandidateCell::CswapK0,
                rung: DeviceRung::Rung0Level1,
                functional: f,
                conservation: c,
                token_invariant: t,
                reverse_drive: r,
                scaling: s,
                scaling_evidence: ev.clone(),
            }
        };
        // Gate 4 failing must NOT block admission.
        assert!(report(pass(), pass(), pass(), fail(), pass()).admitted());
        // Each of gates 1, 2, 3, 5 failing must block.
        assert!(!report(fail(), pass(), pass(), pass(), pass()).admitted());
        assert!(!report(pass(), fail(), pass(), pass(), pass()).admitted());
        assert!(!report(pass(), pass(), fail(), pass(), pass()).admitted());
        assert!(!report(pass(), pass(), pass(), pass(), fail()).admitted());
    }

    /// The typed decode: thresholds at 0.3/0.7·VDD, dead-band non-decoding,
    /// pair decode demands full complementary agreement.
    #[test]
    fn typed_decode_thresholds_and_dead_band() {
        let vdd = 1.0;
        assert_eq!(decode_rail(0.95, vdd), RailState::Asserted);
        assert_eq!(decode_rail(0.70, vdd), RailState::Asserted);
        assert_eq!(decode_rail(0.05, vdd), RailState::Deasserted);
        assert_eq!(decode_rail(0.30, vdd), RailState::Deasserted);
        assert_eq!(decode_rail(0.50, vdd), RailState::Undefined);
        assert_eq!(decode_pair(0.95, 0.05, vdd), RailState::Asserted);
        assert_eq!(decode_pair(0.05, 0.95, vdd), RailState::Deasserted);
        // Disagreeing / mid-band pairs refuse to decode.
        assert_eq!(decode_pair(0.95, 0.95, vdd), RailState::Undefined);
        assert_eq!(decode_pair(0.5, 0.5, vdd), RailState::Undefined);
    }

    fn assert_admitted(cell: CandidateCell, rung: DeviceRung) {
        let r = admit(cell, rung).expect("ngspice failed");
        eprintln!("{}", r.summary());
        assert!(r.functional.passed, "gate 1 failed:\n{}", r.summary());
        assert!(r.conservation.passed, "gate 2 failed:\n{}", r.summary());
        assert!(r.token_invariant.passed, "gate 3 failed:\n{}", r.summary());
        assert!(r.scaling.passed, "gate 5 failed:\n{}", r.summary());
        assert!(r.admitted(), "cell must be admitted:\n{}", r.summary());
    }

    // ----- the checklist ACCEPTS the reversible library cells (rung 0) -----

    #[test]
    fn admission_accepts_cswap_k0_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        assert_admitted(CandidateCell::CswapK0, DeviceRung::Rung0Level1);
    }

    #[test]
    fn admission_accepts_cswap_k1_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        assert_admitted(CandidateCell::CswapK1, DeviceRung::Rung0Level1);
    }

    #[test]
    fn admission_accepts_cswap_k2_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        assert_admitted(CandidateCell::CswapK2, DeviceRung::Rung0Level1);
    }

    #[test]
    fn admission_accepts_cswap_k3_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        assert_admitted(CandidateCell::CswapK3, DeviceRung::Rung0Level1);
    }

    #[test]
    fn admission_accepts_adiabatic_tg_buffer_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        assert_admitted(CandidateCell::AdiabaticTg, DeviceRung::Rung0Level1);
    }

    // ----- and REJECTS the baselines (the checklist discriminates) -----

    #[test]
    fn admission_rejects_cmos_baseline_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let r = admit(CandidateCell::CmosBaseline, DeviceRung::Rung0Level1).expect("ngspice failed");
        eprintln!("{}", r.summary());
        // The inverter WORKS — rejection is about reversibility, not function.
        assert!(r.functional.passed, "the CMOS baseline should compute:\n{}", r.summary());
        // No token discipline, and a flat E(T) has no recoverable term.
        assert!(!r.token_invariant.passed, "gate 3 must fail for CMOS:\n{}", r.summary());
        assert!(!r.scaling.passed, "gate 5 must fail for CMOS (flat curve):\n{}", r.summary());
        assert!(
            r.scaling_evidence.adia_share_fast < MIN_ADIA_SHARE,
            "flat CMOS curve must have no dominant k_adia/t term: {:?}",
            r.scaling_evidence
        );
        assert!(!r.admitted(), "CMOS baseline must be rejected:\n{}", r.summary());
    }

    #[test]
    fn admission_rejects_pfal_baseline_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let r = admit(CandidateCell::PfalBuffer, DeviceRung::Rung0Level1).expect("ngspice failed");
        eprintln!("{}", r.summary());
        // PFAL computes as a clocked buffer — its failure is the energy floor.
        assert!(r.functional.passed, "the PFAL baseline should compute:\n{}", r.summary());
        assert!(
            !r.scaling.passed,
            "gate 5 must fail for PFAL (floored / leakage-dominated):\n{}",
            r.summary()
        );
        assert!(!r.admitted(), "PFAL baseline must be rejected:\n{}", r.summary());
    }

    // ----- rung 1 (sky130): the same checklist on real silicon models -----

    #[test]
    fn admission_accepts_cswap_k0_sky130() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        if !sky130_available() {
            eprintln!("[skip] sky130 PDK unavailable");
            return;
        }
        assert_admitted(CandidateCell::CswapK0, DeviceRung::Rung1Sky130);
    }
}
