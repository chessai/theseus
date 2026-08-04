//! Adiabatic-logic energy curve (M3a-2): the money-plot's slope-−1 curve.
//!
//! The canonical adiabatic result is that a circuit driven by a slow trapezoidal
//! power-clock recovers energy: dissipation per operation falls as `1/T` (log-log
//! slope −1), versus a fixed `½CV²` for a step-driven CMOS gate. This module
//! reproduces that on the 2LAL building block -- a full CMOS **transmission gate**
//! charging/recovering a load node through the power-clock -- and is measured by
//! the [`crate::bench`] harness (`∫V·I` via a behavioral-power node + `.meas
//! INTEG`, already self-validated against the closed-form RC dissipation).
//!
//! Scope: the adiabatic transmission-gate *cell* (the 2LAL primitive) plus the
//! full dual-rail 4-phase 2LAL **shift register** ([`two_lal_sr_netlist`]):
//! token propagation at full level across ≥8 stages with clock energy per
//! cycle ∝ 1/T (log-log slope ≈ −1, measured over four decades of period).
//!
//! Physics notes (learned by bench iteration):
//! * The PMOS bulk MUST tie to VDD; with a swinging clock node on the drain and
//!   the bulk at 0 the P+/N-well junction forward-biases into a huge diode
//!   current (energy grows *with* T -- the opposite of adiabatic).
//! * The transient timestep must resolve the device `R_on·C` (not just the
//!   period), or the energy integral runs high at large T (same lesson as the
//!   RC gate in [`crate::bench`]).
//! * Measure inside the adiabatic regime (`T ≫ R_on·C`) and with a load large
//!   enough to keep the tiny recovered-energy signal above the solver noise floor.

use std::io;

use crate::bench::{log_log_slope, parse_meas, run_energy, run_ngspice_raw};
use crate::deck::finalize_deck;

// ---------------------------------------------------------------------------
// Device-model rungs (ADR-0032 idealization ladder)
// ---------------------------------------------------------------------------

/// The device-model idealization rung a netlist is generated against.
///
/// Rung 0 uses generic `level=1` MOSFETs (no PDK): zero gate capacitance, no
/// junction diodes beyond the (suppressed) defaults, VDD = 1.0 V. Rung 1 uses
/// the real SkyWater sky130 foundry models (sky130A, `tt` corner, BSIM4
/// subcircuits with real gate capacitance, junction diodes, and sub-threshold
/// conduction), VDD = 1.8 V (the 01v8 core devices).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeviceRung {
    /// Ideal level-1 models -- the rung-0 physics sandbox.
    Rung0Level1,
    /// sky130A foundry models, `tt` corner -- real silicon behavior.
    Rung1Sky130,
}

/// FET polarity, used by [`DeviceRung::fet`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FetKind {
    Nmos,
    Pmos,
}

impl DeviceRung {
    /// The supply / full-swing voltage for this rung, in volts.
    pub fn vdd(self) -> f64 {
        match self {
            DeviceRung::Rung0Level1 => 1.0,
            DeviceRung::Rung1Sky130 => 1.8,
        }
    }

    /// The model-header card(s) for a deck at this rung: `.model` lines at
    /// rung 0, the pinned sky130 `.lib ... tt` include at rung 1.
    ///
    /// # Panics
    /// At rung 1, panics if `THESEUS_SKY130_LIB` is unset (the `.#char`
    /// devShell exports it). Gate callers on [`sky130_available`].
    pub fn model_header(self) -> String {
        match self {
            DeviceRung::Rung0Level1 => "\
.model NM NMOS (level=1 vto=0.4 kp=200u)
.model PM PMOS (level=1 vto=-0.4 kp=100u)"
                .to_string(),
            DeviceRung::Rung1Sky130 => {
                let lib = std::env::var("THESEUS_SKY130_LIB")
                    .expect("THESEUS_SKY130_LIB not set -- run inside the .#char devShell");
                // gmin policy (uniform across ALL rung1 sky130 decks): ngspice's
                // default gmin=1e-12 puts a ~1 pA/junction numerical shunt across
                // every junction, a fake static-leakage floor ~20× real sky130 tt
                // leakage. gmin=1e-16 cuts that artifact to ~0.2% of the real floor
                // (so cross-cell leakage comparisons measure silicon, not the
                // solver) while staying convergent. Do NOT go to 1e-18 -- it buys
                // nothing here and risks non-convergence.
                format!(".lib \"{lib}\" tt\n.options gmin=1e-16")
            }
        }
    }

    /// One transistor instance card. `w_um`/`l_um` are in microns at both
    /// rungs (the sky130 lib sets `.option scale=1.0u`, so its geometry
    /// parameters are plain micron numbers -- `W=4`, not `W=4u`).
    ///
    /// Pin order is drain gate source bulk at both rungs.
    // A transistor card irreducibly has 4 pins + polarity + W + L; a
    // params-struct would only rename the arity, not remove it.
    #[allow(clippy::too_many_arguments)]
    pub fn fet(
        self,
        name: &str,
        kind: FetKind,
        d: &str,
        g: &str,
        s: &str,
        b: &str,
        w_um: f64,
        l_um: f64,
    ) -> String {
        match self {
            DeviceRung::Rung0Level1 => {
                let model = match kind {
                    FetKind::Nmos => "NM",
                    FetKind::Pmos => "PM",
                };
                format!("M{name} {d} {g} {s} {b} {model} W={w_um}u L={l_um}u")
            }
            DeviceRung::Rung1Sky130 => {
                let model = match kind {
                    FetKind::Nmos => "sky130_fd_pr__nfet_01v8",
                    FetKind::Pmos => "sky130_fd_pr__pfet_01v8",
                };
                format!("XM{name} {d} {g} {s} {b} {model} W={w_um} L={l_um}")
            }
        }
    }
}

/// True iff the sky130 PDK is reachable: `THESEUS_SKY130_LIB` is set and the
/// file exists. Rung-1 tests skip (like `ngspice_available`) when false.
pub fn sky130_available() -> bool {
    std::env::var("THESEUS_SKY130_LIB")
        .map(|p| std::path::Path::new(&p).is_file())
        .unwrap_or(false)
}

// ---------------------------------------------------------------------------
// sky130 device-variant axes (M3a W5 bench DUT matrix; ADR-0024 B2.3)
// ---------------------------------------------------------------------------

/// Threshold-voltage flavor for rung-1 sky130 1.8 V core devices -- one of the
/// two ADR-0024 B2.3 characterization axes (Vt flavor is *measured*, not
/// pre-decided).
///
/// Discovered device mapping (sky130A, pinned PDK, `tt` section of
/// `sky130.lib.spice`):
///
/// | flavor | NMOS                          | PMOS                          |
/// |--------|-------------------------------|-------------------------------|
/// | std    | `sky130_fd_pr__nfet_01v8`     | `sky130_fd_pr__pfet_01v8`     |
/// | lvt    | `sky130_fd_pr__nfet_01v8_lvt` | `sky130_fd_pr__pfet_01v8_lvt` |
/// | hvt    | -- (see below)                 | `sky130_fd_pr__pfet_01v8_hvt` |
///
/// Two real PDK constraints, handled explicitly rather than hidden:
/// * **There is no hvt NMOS in the sky130 1.8 V core family** (the closest
///   things are 3.3/5 V native devices, unusable at VDD = 1.8 V). The `Hvt`
///   axis point therefore emits the *std* NMOS and carries the hvt flavor on
///   the PMOS only. The variant key still says `hvt`; this table is the
///   record of what that actually means on this PDK.
/// * The lvt PMOS is only binned for **L ≥ 0.35 µm** (`lmin = 3.5e-7` in
///   `sky130_fd_pr__pfet_01v8_lvt__tt.corner.spice`; ngspice hard-errors
///   "could not find a valid modelname" below it). [`Sky130Vt::fet`] clamps
///   the lvt PMOS channel length up to 0.35 µm -- the process minimum for
///   that flavor, not a tuning choice.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum Sky130Vt {
    /// Standard Vt (the rung-1 default; identical devices to [`DeviceRung::fet`]).
    #[default]
    Std,
    /// Low Vt: lower R_on (T-gates like it), higher subthreshold leakage.
    Lvt,
    /// High Vt (PMOS only on this PDK -- see the type-level table).
    Hvt,
}

impl Sky130Vt {
    /// All flavors, in axis order.
    pub const ALL: [Sky130Vt; 3] = [Sky130Vt::Std, Sky130Vt::Lvt, Sky130Vt::Hvt];

    /// Short key for DB/artifact naming.
    pub fn key(self) -> &'static str {
        match self {
            Sky130Vt::Std => "std",
            Sky130Vt::Lvt => "lvt",
            Sky130Vt::Hvt => "hvt",
        }
    }

    /// The sky130 NMOS subcircuit for this flavor. `Hvt` falls back to the
    /// std device -- no hvt NMOS exists in the 1.8 V core family (see the
    /// type-level table).
    pub fn nmos_model(self) -> &'static str {
        match self {
            Sky130Vt::Std | Sky130Vt::Hvt => "sky130_fd_pr__nfet_01v8",
            Sky130Vt::Lvt => "sky130_fd_pr__nfet_01v8_lvt",
        }
    }

    /// The sky130 PMOS subcircuit for this flavor (all three exist).
    pub fn pmos_model(self) -> &'static str {
        match self {
            Sky130Vt::Std => "sky130_fd_pr__pfet_01v8",
            Sky130Vt::Lvt => "sky130_fd_pr__pfet_01v8_lvt",
            Sky130Vt::Hvt => "sky130_fd_pr__pfet_01v8_hvt",
        }
    }

    /// One rung-1 sky130 transistor card at this Vt flavor. Same card shape
    /// and units as [`DeviceRung::fet`] at rung 1 (microns, `XM` subcircuit
    /// instance, pin order d g s b); `Std` emits byte-identical cards.
    ///
    /// The lvt PMOS channel length is clamped up to its 0.35 µm binning
    /// minimum (a hard PDK limit; ngspice errors below it).
    // Same irreducible arity as `DeviceRung::fet`.
    #[allow(clippy::too_many_arguments)]
    pub fn fet(
        self,
        name: &str,
        kind: FetKind,
        d: &str,
        g: &str,
        s: &str,
        b: &str,
        w_um: f64,
        l_um: f64,
    ) -> String {
        let (model, l_um) = match kind {
            FetKind::Nmos => (self.nmos_model(), l_um),
            FetKind::Pmos => {
                let l_min = if self == Sky130Vt::Lvt { 0.35 } else { 0.0 };
                (self.pmos_model(), l_um.max(l_min))
            }
        };
        format!("XM{name} {d} {g} {s} {b} {model} W={w_um} L={l_um}")
    }
}

/// NMOS body configuration -- the other ADR-0024 B2.3 axis.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum BodyConfig {
    /// Baseline: NMOS bulk tied to GND (and PMOS bulk to VDD) -- the shared
    /// p-substrate configuration every existing rung-1 cell uses. The NMOS
    /// pays the full body effect as its source swings above GND.
    #[default]
    BodyToRail,
    /// Deep-n-well isolated NMOS body: the NMOS sits in its own p-well,
    /// tied to the cell's load node (source-tied body -- the body-bias the
    /// DNW makes possible), inside a deep n-well tapped to VDD. The SPICE
    /// realization is (a) the NMOS bulk pin wired to the load node instead
    /// of GND, and (b) the PDK's DNW parasitic junction stack
    /// ([`dnw_parasitic_stack`]) -- the isolated p-well→DNW diode and the
    /// substrate→DNW diode -- so the isolation's capacitance and leakage are
    /// paid, not idealized away. The PMOS n-well is isolated by
    /// construction on this process and stays bulk-to-VDD.
    DeepNWell,
}

impl BodyConfig {
    /// Both configurations, in axis order.
    pub const ALL: [BodyConfig; 2] = [BodyConfig::BodyToRail, BodyConfig::DeepNWell];

    /// Short key for DB/artifact naming.
    pub fn key(self) -> &'static str {
        match self {
            BodyConfig::BodyToRail => "b2r",
            BodyConfig::DeepNWell => "dnw",
        }
    }
}

/// Assumed isolated-p-well footprint for the DNW parasitic stack, µm²
/// (a 5 µm × 8 µm well enclosing the W=4 µm NMOS; the sky130 lib's
/// `.option scale=1.0u` makes the diode `area`/`pj` instance parameters
/// plain micron numbers, like the FET geometry).
pub const DNW_WELL_AREA_UM2: f64 = 40.0;
/// Perimeter of the same well footprint, µm.
pub const DNW_WELL_PERIM_UM: f64 = 26.0;

/// The deep-n-well parasitic junction stack (PDK models, included by the
/// `tt` lib section via `all.spice`): the isolated p-well (`pw`, anode) into
/// the deep n-well (`dnw`, cathode), and the p-substrate (`psub`, anode)
/// into the same deep n-well. Both reverse-biased in normal operation with
/// the DNW tap at VDD; their junction capacitance and (tiny, tt-corner)
/// saturation current are what DNW isolation actually costs.
pub fn dnw_parasitic_stack(pw: &str, dnw: &str, psub: &str) -> String {
    format!(
        "DPW {pw} {dnw} sky130_fd_pr__model__parasitic__diode_pw2dn \
         area={DNW_WELL_AREA_UM2} pj={DNW_WELL_PERIM_UM}\n\
         DPS {psub} {dnw} sky130_fd_pr__model__parasitic__diode_ps2dn \
         area={DNW_WELL_AREA_UM2} pj={DNW_WELL_PERIM_UM}"
    )
}

/// One point on the (Vt × body) variant grid for rung-1 sky130 cells.
/// `Default` is `{Std, BodyToRail}` -- exactly the devices and wiring the
/// existing rung-1 cells emit, so all pre-variant behavior is unchanged.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Sky130Variant {
    pub vt: Sky130Vt,
    pub body: BodyConfig,
}

impl Sky130Variant {
    /// The full 3 × 2 (Vt × body) grid, in deterministic axis order.
    pub fn all() -> Vec<Sky130Variant> {
        let mut v = Vec::with_capacity(6);
        for vt in Sky130Vt::ALL {
            for body in BodyConfig::ALL {
                v.push(Sky130Variant { vt, body });
            }
        }
        v
    }

    /// Short key for DB/artifact naming, e.g. `"lvt-dnw"`.
    pub fn key(self) -> String {
        format!("{}-{}", self.vt.key(), self.body.key())
    }

    /// The canonical adiabatic-TG device cards for this variant: the held-ON
    /// transmission gate between the power-clock node `pc` and the load node
    /// `out` (gates `ing`/`inb`, W=4/8 µm N/P, L=0.15 µm -- same geometry as
    /// the single-variant cell), with the NMOS bulk wired per [`BodyConfig`]
    /// and the DNW parasitic stack appended when isolated. Shared by the
    /// energy deck and the admission deck so the two always simulate the
    /// same device under test.
    pub fn tg_cards(self, n_name: &str, p_name: &str) -> Vec<String> {
        let n_bulk = match self.body {
            BodyConfig::BodyToRail => "0",
            BodyConfig::DeepNWell => "out",
        };
        let mut cards = vec![
            self.vt
                .fet(n_name, FetKind::Nmos, "pc", "ing", "out", n_bulk, 4.0, 0.15),
            self.vt
                .fet(p_name, FetKind::Pmos, "pc", "inb", "out", "vdd", 8.0, 0.15),
        ];
        if self.body == BodyConfig::DeepNWell {
            cards.push(dnw_parasitic_stack("out", "vdd", "0"));
        }
        cards
    }
}

// ---------------------------------------------------------------------------
// Load axis (M3a spec: min / 4× / 16×)
// ---------------------------------------------------------------------------

/// The minimum load of the M3a load axis, farads. Chosen so that 16×min =
/// 1 pF -- the canonical load the existing single-variant rung-1 TG cell is
/// hardcoded to (that cell is literally the (std, body-to-rail, 16×) corner
/// of the matrix) -- while still sitting well above the TG's own parasitics
/// (a few fF of junction + gate capacitance at this geometry), so the load
/// term stays the dominant capacitance at every axis point.
pub const TG_LOAD_MIN_F: f64 = 62.5e-15;

/// The spec's three-point load axis: min, 4×min, 16×min.
pub fn tg_load_axis() -> [f64; 3] {
    [TG_LOAD_MIN_F, 4.0 * TG_LOAD_MIN_F, 16.0 * TG_LOAD_MIN_F]
}

/// Build the rung-1 sky130 adiabatic transmission-gate netlist for one
/// power-clock `period_s`, at a (Vt × body) `variant` and an explicit load.
///
/// Same physics and accounting as the rung-1 arm of
/// [`adiabatic_tg_netlist_rung`] (settled second cycle, every source
/// integrated: `etot`/`epc`/`evdd`/`egat`, plus `vmax`), with the device
/// cards swapped per [`Sky130Variant::tg_cards`] and the load parameterized.
/// With `variant = Sky130Variant::default()` and `c_load_f = 1e-12` this is
/// electrically identical to the existing cell (asserted by test).
pub fn adiabatic_tg_netlist_sky130(period_s: f64, variant: Sky130Variant, c_load_f: f64) -> String {
    let rung = DeviceRung::Rung1Sky130;
    let q = period_s / 4.0;
    let h = period_s / 2.0;
    let tq = 3.0 * period_s / 4.0;
    let vdd = rung.vdd();
    // Same timestep policy as adiabatic_tg_netlist_rung at rung 1.
    let step = (period_s / 4000.0).min(5e-11).max(period_s / 40_000.0);
    let header = rung.model_header();
    let cards = variant.tg_cards("1", "2").join("\n");
    let stop = 2.0 * period_s;
    let key = variant.key();
    format!(
        "\
adiabatic transmission-gate cell, sky130 tt, variant {key}, load {c_load_f:.4e} F (T={period_s:.6e} s)
{header}
Vdd vdd 0 DC {vdd}
Vpc pc 0 PWL(0 0 {q:.9e} {vdd} {h:.9e} {vdd} {tq:.9e} 0 {period_s:.9e} 0) r=0
Vin  ing 0 DC {vdd}
Vinb inb 0 DC 0.0
{cards}
Cout out 0 {c_load_f:.4e}
Bpc  ppc  0 V = -V(pc)*I(Vpc)
Bvdd pvdd 0 V = -V(vdd)*I(Vdd)
Bgat pgat 0 V = -V(ing)*I(Vin)-V(inb)*I(Vinb)
Btot ptot 0 V = V(ppc)+V(pvdd)+V(pgat)
.control
tran {step:.9e} {stop:.9e} uic
meas tran etot INTEG V(ptot) FROM={period_s:.9e} TO={stop:.9e}
meas tran epc  INTEG V(ppc)  FROM={period_s:.9e} TO={stop:.9e}
meas tran evdd INTEG V(pvdd) FROM={period_s:.9e} TO={stop:.9e}
meas tran egat INTEG V(pgat) FROM={period_s:.9e} TO={stop:.9e}
meas tran vmax MAX V(out) FROM={period_s:.9e} TO={stop:.9e}
.endc
.end
"
    )
}

/// One measured point of a variant sweep: the settled-cycle energy
/// decomposition (all sources, ADR-0028 C3.3 honest accounting) plus the
/// output peak proving the cell did work.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TgVariantPoint {
    /// Power-clock period, seconds.
    pub period_s: f64,
    /// Total energy from ALL sources over the settled cycle (the per-op figure).
    pub etot: f64,
    /// Power-clock share (the adiabatic/recoverable path).
    pub epc: f64,
    /// VDD-rail share: PMOS n-well bias, and under deep-n-well isolation the
    /// DNW tap -- the well/junction bookkeeping lives here.
    pub evdd: f64,
    /// Input gate-driver share.
    pub egat: f64,
    /// Output peak, volts.
    pub vmax: f64,
}

/// Simulate one (variant, load, period) point and decode every measurement.
pub fn adiabatic_tg_sky130_point(
    period_s: f64,
    variant: Sky130Variant,
    c_load_f: f64,
) -> io::Result<TgVariantPoint> {
    let out = run_ngspice_raw(&adiabatic_tg_netlist_sky130(period_s, variant, c_load_f))?;
    let get = |name: &str| {
        parse_meas(&out, name).unwrap_or_else(|| {
            panic!(
                "measurement '{name}' missing from ngspice output for variant {} @ T={period_s:e}",
                variant.key()
            )
        })
    };
    Ok(TgVariantPoint {
        period_s,
        etot: get("etot"),
        epc: get("epc"),
        evdd: get("evdd"),
        egat: get("egat"),
        vmax: get("vmax"),
    })
}

/// Sweep one (variant, load) cell of the DUT matrix over `periods`
/// (ascending). Each point carries the full per-source decomposition.
pub fn adiabatic_tg_sky130_curve(
    periods: &[f64],
    variant: Sky130Variant,
    c_load_f: f64,
) -> io::Result<Vec<TgVariantPoint>> {
    periods
        .iter()
        .map(|&t| adiabatic_tg_sky130_point(t, variant, c_load_f))
        .collect()
}

/// A money-plot curve: energy-per-op versus power-clock period, log-log.
#[derive(Clone, Debug, PartialEq)]
pub struct MoneyPlotCurve {
    /// Device-under-test identifier (e.g. `"adiabatic-tg"`).
    pub dut: String,
    /// `(period_s, energy_per_op_J)` samples, ascending in period.
    pub points: Vec<(f64, f64)>,
}

impl MoneyPlotCurve {
    /// The fitted log-log slope of energy-per-op vs period.
    pub fn slope(&self) -> f64 {
        log_log_slope(&self.points)
    }
}

/// Build the adiabatic transmission-gate netlist for one power-clock
/// `period_s` at rung 0 (ideal level-1 models). Wrapper over
/// [`adiabatic_tg_netlist_rung`]; existing callers and tests are unchanged.
pub fn adiabatic_tg_netlist(period_s: f64) -> String {
    adiabatic_tg_netlist_rung(period_s, DeviceRung::Rung0Level1)
}

/// Build the adiabatic transmission-gate netlist for one power-clock
/// `period_s` at the given [`DeviceRung`].
///
/// A full CMOS transmission gate (held ON) connects a trapezoidal power-clock
/// `pc` to an output load; the source energy delivered over one full
/// charge/recover cycle is the (adiabatic) dissipation. Devices are W=4/8 µm
/// (N/P), L=0.15 µm at both rungs.
///
/// **Rung 0** (level-1, VDD=1.0): the devices carry no state (no gate
/// capacitance, no junction charge), so cycle 0 from `uic` *is* the settled
/// cycle; one cycle is simulated and the clock-source integral `esrc` is the
/// whole story (bulk and gate sources deliver exactly 0 by construction).
///
/// **Rung 1** (sky130 tt, VDD=1.8): the BSIM4 devices have real gate/junction
/// capacitance, so cycle 0 pays a one-time charge-up of internal state. Two
/// cycles are simulated (`PWL ... r=0` repeats the clock) and all measurements
/// integrate over the settled second cycle. Honest accounting (ADR-0028
/// C3.3): EVERY source contributes to the measured total --
/// * `etot` -- total energy from all sources over the settled cycle (this is
///   the per-op figure; also aliased into `esrc` semantics for the curve),
/// * `epc`  -- the power-clock (the recoverable/adiabatic path),
/// * `evdd` -- the bulk bias (junction/well leakage),
/// * `egat` -- the input gate drivers (real gate capacitance now exists; the
///   inputs are DC here so the settled-cycle net is ≈ leakage-only, but it is
///   measured, not assumed),
/// * `vmax` -- output-rail peak, to prove the cell actually does work.
pub fn adiabatic_tg_netlist_rung(period_s: f64, rung: DeviceRung) -> String {
    let q = period_s / 4.0; // ramp-up ends
    let h = period_s / 2.0; // hold ends
    let tq = 3.0 * period_s / 4.0; // ramp-down ends
    let vdd = rung.vdd();
    // Resolve the device R_on·C (~150 ps rung0, ~660 ps rung1) as well as
    // the period: ≥4000 points/cycle, tightened to 50 ps when the period
    // allows. At rung 1 the 50 ps cap is floored at T/40000 (≤40k
    // points/cycle) -- a fixed 50 ps cap at T = 1 ms means 4×10⁸ steps and
    // ngspice never finishes; the relaxed step was cross-checked against
    // the strict one at the T = 1e-5 overlap point (etot agrees to 0.45%).
    let step = match rung {
        DeviceRung::Rung0Level1 => (period_s / 4000.0).min(5e-11),
        DeviceRung::Rung1Sky130 => (period_s / 4000.0).min(5e-11).max(period_s / 40_000.0),
    };
    let header = rung.model_header();
    let m1 = rung.fet("1", FetKind::Nmos, "pc", "ing", "out", "0", 4.0, 0.15);
    let m2 = rung.fet("2", FetKind::Pmos, "pc", "inb", "out", "vdd", 8.0, 0.15);
    match rung {
        DeviceRung::Rung0Level1 => {
            let stop = period_s;
            format!(
                "\
adiabatic transmission-gate cell (T={period_s:.6e} s)
{header}
Vdd vdd 0 DC {vdd}
Vpc pc 0 PWL(0 0 {q:.9e} {vdd} {h:.9e} {vdd} {tq:.9e} 0 {stop:.9e} 0)
Vin  ing 0 DC {vdd}
Vinb inb 0 DC 0.0
{m1}
{m2}
Cout out 0 1p
Bpwr pwr 0 V = -V(pc)*I(Vpc)
.control
tran {step:.9e} {stop:.9e} uic
meas tran esrc INTEG V(pwr) FROM=0 TO={stop:.9e}
print esrc
.endc
.end
"
            )
        }
        DeviceRung::Rung1Sky130 => {
            let stop = 2.0 * period_s;
            format!(
                "\
adiabatic transmission-gate cell, sky130 tt (T={period_s:.6e} s)
{header}
Vdd vdd 0 DC {vdd}
Vpc pc 0 PWL(0 0 {q:.9e} {vdd} {h:.9e} {vdd} {tq:.9e} 0 {period_s:.9e} 0) r=0
Vin  ing 0 DC {vdd}
Vinb inb 0 DC 0.0
{m1}
{m2}
Cout out 0 1p
Bpc  ppc  0 V = -V(pc)*I(Vpc)
Bvdd pvdd 0 V = -V(vdd)*I(Vdd)
Bgat pgat 0 V = -V(ing)*I(Vin)-V(inb)*I(Vinb)
Btot ptot 0 V = V(ppc)+V(pvdd)+V(pgat)
.control
tran {step:.9e} {stop:.9e} uic
meas tran etot INTEG V(ptot) FROM={period_s:.9e} TO={stop:.9e}
meas tran epc  INTEG V(ppc)  FROM={period_s:.9e} TO={stop:.9e}
meas tran evdd INTEG V(pvdd) FROM={period_s:.9e} TO={stop:.9e}
meas tran egat INTEG V(pgat) FROM={period_s:.9e} TO={stop:.9e}
meas tran vmax MAX V(out) FROM={period_s:.9e} TO={stop:.9e}
.endc
.end
"
            )
        }
    }
}

/// Sweep the power-clock period across the adiabatic regime and return the
/// money-plot curve (energy per op vs period). One transmission-gate op per
/// cycle, so energy-per-op is the measured per-cycle source energy.
pub fn adiabatic_tg_curve(periods: &[f64]) -> MoneyPlotCurve {
    let points = periods
        .iter()
        .map(|&t| (t, run_energy(&adiabatic_tg_netlist(t), "esrc")))
        .collect();
    MoneyPlotCurve {
        dut: "adiabatic-tg".to_string(),
        points,
    }
}

/// Rung-aware variant of [`adiabatic_tg_curve`]: sweeps energy-per-op vs
/// period at the given rung. At rung 1 the extracted energy is `etot` -- the
/// sum over ALL sources (clock + bulk + gate drivers) over the settled
/// second cycle -- so gate-drive and leakage energy are counted, not hidden.
pub fn adiabatic_tg_curve_rung(periods: &[f64], rung: DeviceRung) -> MoneyPlotCurve {
    let (dut, meas) = match rung {
        DeviceRung::Rung0Level1 => ("adiabatic-tg", "esrc"),
        DeviceRung::Rung1Sky130 => ("adiabatic-tg-sky130", "etot"),
    };
    let points = periods
        .iter()
        .map(|&t| {
            (
                t,
                run_energy(&adiabatic_tg_netlist_rung(t, rung), meas).abs(),
            )
        })
        .collect();
    MoneyPlotCurve {
        dut: dut.to_string(),
        points,
    }
}

/// Static-leakage deck for the rung-1 sky130 adiabatic TG cell (ADR-0028
/// C3.3: leakage is reported separately from switching energy).
///
/// Two copies of the cell hold the power-clock at its two rest states (high
/// = output parked at VDD, low = output parked at 0), and the total power of
/// every source attached to each copy is integrated over `window_s` after an
/// equal settling window. `run_energy(..., "eh")/window_s` and `...("el")/...`
/// give the static-power floor per state in watts; that floor times the
/// period is the energy term that caps the adiabatic 1/T slope at slow T.
pub fn adiabatic_tg_leakage_netlist(window_s: f64) -> String {
    let rung = DeviceRung::Rung1Sky130;
    let vdd = rung.vdd();
    let header = rung.model_header();
    let a1 = rung.fet("A1", FetKind::Nmos, "pch", "ing", "outh", "0", 4.0, 0.15);
    let a2 = rung.fet("A2", FetKind::Pmos, "pch", "inb", "outh", "vdd", 8.0, 0.15);
    let b1 = rung.fet("B1", FetKind::Nmos, "pcl", "ing", "outl", "0", 4.0, 0.15);
    let b2 = rung.fet("B2", FetKind::Pmos, "pcl", "inb", "outl", "vdd", 8.0, 0.15);
    let stop = 2.0 * window_s;
    let step = window_s / 1000.0;
    format!(
        "\
rung1 sky130 adiabatic TG static leakage (window={window_s:.3e} s)
{header}
Vdd vdd 0 DC {vdd}
Vpch pch 0 DC {vdd}
Vpcl pcl 0 DC 0.0
Vin  ing 0 DC {vdd}
Vinb inb 0 DC 0.0
{a1}
{a2}
Ch outh 0 1p
{b1}
{b2}
Cl outl 0 1p
Bph pwh 0 V = -V(pch)*I(Vpch)-V(vdd)*I(Vdd)-V(ing)*I(Vin)-V(inb)*I(Vinb)
Bpl pwl 0 V = -V(pcl)*I(Vpcl)
.control
tran {step:.9e} {stop:.9e} uic
meas tran eh INTEG V(pwh) FROM={window_s:.9e} TO={stop:.9e}
meas tran el INTEG V(pwl) FROM={window_s:.9e} TO={stop:.9e}
.endc
.end
"
    )
}

/// Build the non-adiabatic **CMOS baseline** netlist for one `period_s` at
/// rung 0 (ideal level-1 models). Wrapper over [`cmos_baseline_netlist_rung`];
/// existing callers and tests are unchanged.
pub fn cmos_baseline_netlist(period_s: f64) -> String {
    cmos_baseline_netlist_rung(period_s, DeviceRung::Rung0Level1)
}

/// Build the non-adiabatic **CMOS baseline** netlist for one `period_s` at the
/// given [`DeviceRung`]: a static CMOS inverter driven by a full-swing square
/// wave with FAST (non-adiabatic) edges. Each cycle dumps ~`CV²` from the rail
/// irrespective of period → a FLAT energy-vs-period curve (the money plot's
/// control). Devices are W=4/8 µm (N/P), L=0.15 µm at both rungs.
///
/// **Rung 0** (level-1, VDD=1.0): the devices carry no state (no gate
/// capacitance, no junction charge), so cycle 0 from `uic` *is* the settled
/// cycle; one cycle is simulated and the rail integral `evdd` is the whole
/// story (the input source delivers exactly 0 by construction).
///
/// **Rung 1** (sky130 tt, VDD=1.8): the BSIM4 devices have real gate/junction
/// capacitance, so cycle 0 pays a one-time charge-up of internal state. Two
/// cycles are simulated (`PULSE` repeats the square wave) and all measurements
/// integrate over the settled second cycle. Honest accounting (ADR-0028
/// C3.3): EVERY source contributes to the measured total --
/// * `etot` -- total energy from all sources over the settled cycle (the
///   per-op figure for the curve),
/// * `evdd` -- the rail (the ~½CV² switching path),
/// * `ein`  -- the input gate driver (real gate capacitance now exists; the
///   input is a square wave so it delivers the gate-charge energy, measured
///   not assumed),
/// * `vmax`/`vmin` -- output-rail extremes, to prove full-swing operation.
pub fn cmos_baseline_netlist_rung(period_s: f64, rung: DeviceRung) -> String {
    let half = period_s / 2.0;
    let edge = (period_s / 1000.0).min(1e-10); // fast edges: no energy recovery
    let vdd = rung.vdd();
    // Same timestep policy as the TG cell: resolve the device R_on·C as well
    // as the period, floored at T/40000 at rung 1 so a slow (ms) period still
    // finishes (a fixed 50 ps cap never does).
    let step = match rung {
        DeviceRung::Rung0Level1 => (period_s / 4000.0).min(5e-11),
        DeviceRung::Rung1Sky130 => (period_s / 4000.0).min(5e-11).max(period_s / 40_000.0),
    };
    let header = rung.model_header();
    let m1 = rung.fet("1", FetKind::Nmos, "out", "in", "0", "0", 4.0, 0.15);
    let m2 = rung.fet("2", FetKind::Pmos, "out", "in", "vdd", "vdd", 8.0, 0.15);
    match rung {
        DeviceRung::Rung0Level1 => {
            let stop = period_s;
            format!(
                "\
static CMOS inverter, step-driven baseline (T={period_s:.6e} s)
{header}
Vdd vdd 0 DC {vdd}
Vin in 0 PULSE(0 {vdd} 0 {edge:.9e} {edge:.9e} {half:.9e} {stop:.9e})
{m1}
{m2}
Cout out 0 1p
Bpwr pwr 0 V = V(vdd)*I(Vdd)
.control
tran {step:.9e} {stop:.9e} uic
meas tran evdd INTEG V(pwr) FROM=0 TO={stop:.9e}
print evdd
.endc
.end
"
            )
        }
        DeviceRung::Rung1Sky130 => {
            let stop = 2.0 * period_s;
            format!(
                "\
static CMOS inverter, step-driven baseline, sky130 tt (T={period_s:.6e} s)
{header}
Vdd vdd 0 DC {vdd}
Vin in 0 PULSE(0 {vdd} 0 {edge:.9e} {edge:.9e} {half:.9e} {period_s:.9e})
{m1}
{m2}
Cout out 0 1p
Bvdd pvdd 0 V = -V(vdd)*I(Vdd)
Bin  pin  0 V = -V(in)*I(Vin)
Btot ptot 0 V = V(pvdd)+V(pin)
.control
tran {step:.9e} {stop:.9e} uic
meas tran etot INTEG V(ptot) FROM={period_s:.9e} TO={stop:.9e}
meas tran evdd INTEG V(pvdd) FROM={period_s:.9e} TO={stop:.9e}
meas tran ein  INTEG V(pin)  FROM={period_s:.9e} TO={stop:.9e}
meas tran vmax MAX V(out) FROM={period_s:.9e} TO={stop:.9e}
meas tran vmin MIN V(out) FROM={period_s:.9e} TO={stop:.9e}
.endc
.end
"
            )
        }
    }
}

/// The CMOS baseline curve at rung 0 (energy per cycle vs period). Wrapper over
/// [`cmos_baseline_curve_rung`]. Energy is reported as a positive magnitude
/// (the rail-current convention makes the raw integral negative for energy
/// leaving the source).
pub fn cmos_baseline_curve(periods: &[f64]) -> MoneyPlotCurve {
    cmos_baseline_curve_rung(periods, DeviceRung::Rung0Level1)
}

/// Rung-aware CMOS baseline curve: energy per cycle vs period at the given
/// rung. At rung 1 the extracted energy is `etot` -- the sum over ALL sources
/// (rail + input gate driver) over the settled second cycle -- so the gate-
/// charge energy is counted, not hidden. Energy is a positive magnitude.
pub fn cmos_baseline_curve_rung(periods: &[f64], rung: DeviceRung) -> MoneyPlotCurve {
    let (dut, meas) = match rung {
        DeviceRung::Rung0Level1 => ("cmos-static", "evdd"),
        DeviceRung::Rung1Sky130 => ("cmos-static-sky130", "etot"),
    };
    let points = periods
        .iter()
        .map(|&t| {
            (
                t,
                run_energy(&cmos_baseline_netlist_rung(t, rung), meas).abs(),
            )
        })
        .collect();
    MoneyPlotCurve {
        dut: dut.to_string(),
        points,
    }
}

/// Build a **PFAL** (positive-feedback adiabatic logic) buffer netlist for one
/// `period_s` at rung 0 (ideal level-1 models). Wrapper over
/// [`pfal_netlist_rung`]; existing callers and tests are unchanged.
pub fn pfal_netlist(period_s: f64) -> String {
    pfal_netlist_rung(period_s, DeviceRung::Rung0Level1)
}

/// Build a **PFAL** (positive-feedback adiabatic logic) buffer netlist for one
/// `period_s` at the given [`DeviceRung`]: a cross-coupled PMOS latch powered
/// by the trapezoidal clock, with an NMOS evaluation pair. It recovers energy
/// (slope −1) at fast periods but **floors**: a static/crowbar path in the
/// resolved latch dissipates a roughly-constant power, so at slow periods
/// energy stops falling (and rises) -- the partially-adiabatic middle curve,
/// between 2LAL (keeps falling) and CMOS (flat everywhere). Devices are
/// W=8 µm (P latch) / W=4 µm (N eval), L=0.15 µm at both rungs.
///
/// **Rung 0** (level-1, VDD=1.0): the devices carry no state, so cycle 0 from
/// `uic` *is* the settled cycle; one cycle is simulated and the clock integral
/// `esrc` is the whole story.
///
/// **Rung 1** (sky130 tt, VDD=1.8): the BSIM4 devices have real gate/junction
/// capacitance, so cycle 0 pays a one-time charge-up. Two cycles are simulated
/// (`PWL ... r=0` repeats the clock) and all measurements integrate over the
/// settled second cycle. Honest accounting (ADR-0028 C3.3): EVERY source
/// contributes to the measured total --
/// * `etot` -- total energy from all sources over the settled cycle (the
///   per-op figure for the curve),
/// * `epc`  -- the power-clock (the recoverable/adiabatic path; dominant here),
/// * `evdd` -- the PMOS-latch bulk bias (junction/well leakage),
/// * `egat` -- the DC input drivers (measured, not assumed to be 0),
/// * `vmax` -- output-rail peak, to prove the latch actually resolves.
pub fn pfal_netlist_rung(period_s: f64, rung: DeviceRung) -> String {
    let q = period_s / 4.0; // ramp-up ends
    let h = period_s / 2.0; // hold ends
    let tq = 3.0 * period_s / 4.0; // ramp-down ends
    let vdd = rung.vdd();
    let step = match rung {
        DeviceRung::Rung0Level1 => (period_s / 4000.0).min(5e-11),
        DeviceRung::Rung1Sky130 => (period_s / 4000.0).min(5e-11).max(period_s / 40_000.0),
    };
    let header = rung.model_header();
    // Pin order for fet() is drain gate source bulk.
    let mp1 = rung.fet("P1", FetKind::Pmos, "pc", "out", "outb", "vdd", 8.0, 0.15);
    let mp2 = rung.fet("P2", FetKind::Pmos, "pc", "outb", "out", "vdd", 8.0, 0.15);
    let mn1 = rung.fet("N1", FetKind::Nmos, "pc", "in", "out", "0", 4.0, 0.15);
    let mn2 = rung.fet("N2", FetKind::Nmos, "pc", "inb", "outb", "0", 4.0, 0.15);
    match rung {
        DeviceRung::Rung0Level1 => {
            let stop = period_s;
            format!(
                "\
PFAL buffer (cross-coupled PMOS latch + NMOS eval), T={period_s:.6e} s
{header}
Vdd vdd 0 DC {vdd}
Vpc pc 0 PWL(0 0 {q:.9e} {vdd} {h:.9e} {vdd} {tq:.9e} 0 {stop:.9e} 0)
Vin  in  0 DC {vdd}
Vinb inb 0 DC 0.0
{mp1}
{mp2}
{mn1}
{mn2}
Cout  out  0 100f
Coutb outb 0 100f
Bpwr pwr 0 V = -V(pc)*I(Vpc)
.control
tran {step:.9e} {stop:.9e} uic
meas tran esrc INTEG V(pwr) FROM=0 TO={stop:.9e}
print esrc
.endc
.end
"
            )
        }
        DeviceRung::Rung1Sky130 => {
            let stop = 2.0 * period_s;
            format!(
                "\
PFAL buffer (cross-coupled PMOS latch + NMOS eval), sky130 tt, T={period_s:.6e} s
{header}
Vdd vdd 0 DC {vdd}
Vpc pc 0 PWL(0 0 {q:.9e} {vdd} {h:.9e} {vdd} {tq:.9e} 0 {period_s:.9e} 0) r=0
Vin  in  0 DC {vdd}
Vinb inb 0 DC 0.0
{mp1}
{mp2}
{mn1}
{mn2}
Cout  out  0 100f
Coutb outb 0 100f
Bpc  ppc  0 V = -V(pc)*I(Vpc)
Bvdd pvdd 0 V = -V(vdd)*I(Vdd)
Bgat pgat 0 V = -V(in)*I(Vin)-V(inb)*I(Vinb)
Btot ptot 0 V = V(ppc)+V(pvdd)+V(pgat)
.control
tran {step:.9e} {stop:.9e} uic
meas tran etot INTEG V(ptot) FROM={period_s:.9e} TO={stop:.9e}
meas tran epc  INTEG V(ppc)  FROM={period_s:.9e} TO={stop:.9e}
meas tran evdd INTEG V(pvdd) FROM={period_s:.9e} TO={stop:.9e}
meas tran egat INTEG V(pgat) FROM={period_s:.9e} TO={stop:.9e}
meas tran vmax MAX V(out) FROM={period_s:.9e} TO={stop:.9e}
.endc
.end
"
            )
        }
    }
}

/// The PFAL curve at rung 0 (energy per op vs period) -- falls then floors.
/// Wrapper over [`pfal_curve_rung`].
pub fn pfal_curve(periods: &[f64]) -> MoneyPlotCurve {
    pfal_curve_rung(periods, DeviceRung::Rung0Level1)
}

/// Rung-aware PFAL curve: energy per op vs period at the given rung. At rung 1
/// the extracted energy is `etot` -- the sum over ALL sources (clock + bulk +
/// gate drivers) over the settled second cycle. Falls then floors on real
/// devices: quasi-adiabatic recovery at fast periods, a non-recoverable
/// residue at slow periods. Energy is a positive magnitude.
pub fn pfal_curve_rung(periods: &[f64], rung: DeviceRung) -> MoneyPlotCurve {
    let (dut, meas) = match rung {
        DeviceRung::Rung0Level1 => ("pfal", "esrc"),
        DeviceRung::Rung1Sky130 => ("pfal-sky130", "etot"),
    };
    let points = periods
        .iter()
        .map(|&t| (t, run_energy(&pfal_netlist_rung(t, rung), meas).abs()))
        .collect();
    MoneyPlotCurve {
        dut: dut.to_string(),
        points,
    }
}

// ---------------------------------------------------------------------------
// 2LAL dual-rail shift register
// ---------------------------------------------------------------------------

/// Build the **canonical 4-phase dual-rail 2LAL shift register** netlist
/// (Athas' adiabatic amplifier + Frank's forward/reverse framework).
///
/// Measured behavior (ngspice, level-1 models): log-log slope of clock energy
/// vs period ≈ −0.99 sustained over **four decades** (10 ns ... 100 µs) with
/// every stage's true rail reaching the full 1.0 V.
///
/// # Topology
///
/// Signals are dual-rail with **level encoding**: the true rail `o{k}` rests at
/// 0 V and pulses high for a `1` token; the complement rail `o{k}b` rests at
/// VDD and pulses low. A stable dual-rail pair therefore drives a CMOS
/// transmission gate at full gate overdrive (NMOS gate ← true rail, PMOS gate
/// ← complement rail).
///
/// Four trapezoidal power-clock rails `p0..p3`, each with the canonical 2LAL
/// shape -- rise T/4, hold T/4, fall T/4, low T/4 -- staggered by T/4. With this
/// shape `~φi = φ(i+2)`, so four rails supply both polarities.
///
/// Data node `S_k` connects **only to clock rails** (there is no transistor
/// path to GND or VDD anywhere -- VDD is a bulk bias only):
///   * true rail `o{k}` ↔ `φ(k mod 4)`, complement `o{k}b` ↔ `φ(k+2 mod 4)`,
///   * each through **two parallel transmission gates**:
///     - the *forward* TG, gated by `S_{k-1}` -- charges the node while
///       `φ(k)` ramps up (tick k);
///     - the *reverse* TG, gated by `S_{k+1}` (the downstream copy) -- returns
///       the node's charge into the same clock rail while `φ(k)` ramps down
///       (tick k+2). This is the reversible-computing retraction: the input is
///       decomputed using the copy, never dumped to ground.
///
/// # Why this is fully adiabatic (slope −1)
///
/// Every TG switching event connects or isolates *equipotential* nodes:
/// `φ(k)` is low exactly during tick k−1 when the forward TG turns on, and the
/// node is charged exactly when the reverse TG turns on during tick k+1. So no
/// charge ever crosses a voltage gap, and at no instant does a conducting path
/// from a high node to ground exist -- all current is clock↔capacitor through
/// fully-enhanced TGs, dissipating only I²·R_on during the ramps (∝ 1/T).
///
/// # Numerics
///
/// The deep-adiabatic tail (≲1e-16 J/cycle) sits below two ngspice solver
/// artifacts, both suppressed here: the default `gmin` (1e-12 S junction
/// shunts → a fake ~1e-10 W static load) and the level-1 default junction
/// saturation current (`is` = 1e-14 A). Neither is device physics at rung 0.
/// The timestep must also resolve the TG `R_on·C` (~30 ps) corners or the
/// `INTEG` power integral runs high (same lesson as [`crate::bench`]).
///
/// # Boundary (data-aware terminator)
///
/// The last stage's reverse TGs are gated by a **delayed replica of the
/// actual input pattern**: the phantom successor `S_n` is, in ideal
/// operation, exactly the input waveform shifted by `(n+1)` ticks
/// (`S_k(t) = S_{-1}(t − (k+1)·T/4)`), so we synthesize it as a PWL source
/// pair replaying the injected bit stream with that shift. For the all-1s
/// stream this reduces to the periodic ack pulse at offset `(n%4)·T/4`; for
/// arbitrary data it acks a 1-token if and only if a 1-token is actually
/// retiring, so the tail decomputes correctly on 0-bits instead of being
/// driven against an all-1s assumption (which burned ~CV², period-
/// independent, per 0-bit -- flattening the real-data energy curve).
/// Both the input pair and the ack pair drive transistor **gates only** --
/// there is no channel path from them to any data or clock node -- and at
/// rung 0 the level-1 models have no gate capacitance (no `tox`/`cgso`/
/// `cgdo`), so these sources deliver exactly zero energy. We do not rely on
/// that argument alone: their delivered power is included in the measured
/// total (`ptot`) *and* integrated separately (`ein`, `eack`) so the tests
/// verify it is ~0 rather than assuming it.
///
/// This is the all-1s wrapper used by the energy sweep; see
/// [`two_lal_sr_pattern_netlist`] for the arbitrary-pattern variant.
pub fn two_lal_sr_netlist(period_s: f64, n_stages: usize) -> String {
    // Enough 1-tokens to keep the pipeline saturated through the measured
    // cycle: bit `warmup` is the last one whose plateau feeds stage 0
    // during the measurement window.
    let warmup = n_stages.div_ceil(4) + 2;
    let bits = vec![true; warmup + 1];
    build_two_lal_sr(period_s, n_stages, &bits, false)
}

/// Arbitrary-pattern variant of [`two_lal_sr_netlist`] for functional
/// verification: injects `bits` (one token per clock period, dual-rail) and
/// emits per-stage/per-bit level measurements plus per-cycle energy
/// integrals. `s{k}d{j}t` / `s{k}d{j}c` measure the true/complement rail of
/// stage `k` in the middle of the window where bit `j` must occupy it
/// (`MIN`/`MAX` chosen so the assertion is always "full level"), and
/// `ecyc{i}` integrates total delivered power over cycle `i`.
pub fn two_lal_sr_pattern_netlist(period_s: f64, n_stages: usize, bits: &[bool]) -> String {
    build_two_lal_sr(period_s, n_stages, bits, true)
}

/// Emit a dual-rail PWL pair for the token stream `bits` (one bit per period):
/// the true rail rests 0 and, for each 1-bit `j`, ramps up over
/// [j·T+off−ε, j·T+off+q−ε], holds, ramps down over [+2q+ε, +3q+ε]; the
/// complement rail is its exact mirror about VDD/2 (rests 1, dips 0). `off`
/// is the waveform delay: `3q` for the input `S_{-1}`, `3q + (n+1)q` for the
/// successor replica `S_n`.
///
/// `eps_ticks` widens each pulse by ε = `eps_ticks`·q on both sides. This is
/// a solver-robustness measure, not a physics change: it moves every PWL
/// breakpoint off the 4-phase clock corners (a gate crossing `vto` exactly
/// at a corner where all four clocks have breakpoints makes ngspice's
/// transient go `timestep = 0` at some periods). It is provably harmless:
/// the ε-early turn-on happens while the connected clock and data node are
/// already equipotential (no charge crosses a gap), and the ε-late turn-off
/// keeps a TG conducting between the same node pair the reverse/forward
/// partner TG is about to connect anyway. Distinct ε per source pair keeps
/// all breakpoint grids disjoint.
fn dual_rail_pwl(bits: &[bool], period_s: f64, off: f64, eps_ticks: f64) -> (String, String) {
    let q = period_s / 4.0;
    let eps = eps_ticks * q;
    let mut t_pts = String::from("0 0");
    let mut c_pts = String::from("0 1");
    for (j, &b) in bits.iter().enumerate() {
        if b {
            let t0 = j as f64 * period_s + off;
            t_pts.push_str(&format!(
                " {:.9e} 0 {:.9e} 1 {:.9e} 1 {:.9e} 0",
                t0 - eps,
                t0 + q - eps,
                t0 + 2.0 * q + eps,
                t0 + 3.0 * q + eps
            ));
            c_pts.push_str(&format!(
                " {:.9e} 1 {:.9e} 0 {:.9e} 0 {:.9e} 1",
                t0 - eps,
                t0 + q - eps,
                t0 + 2.0 * q + eps,
                t0 + 3.0 * q + eps
            ));
        }
    }
    (t_pts, c_pts)
}

fn build_two_lal_sr(period_s: f64, n_stages: usize, bits: &[bool], bit_meas: bool) -> String {
    assert!(n_stages >= 1, "need at least 1 stage");
    assert!(!bits.is_empty(), "need at least 1 input bit");
    let q = period_s / 4.0; // one tick = quarter period (ramp/hold duration)
    let step = (period_s / 4000.0).min(2e-11);
    // Warmup scales with pipeline fill (the token reaches stage s at tick s):
    // ceil(n/4) cycles to fill plus 2 to settle. A fixed warmup measures
    // during fill and inflates the energy ~17× at n=12.
    let warmup = n_stages.div_ceil(4) + 2;
    let meas_from = warmup as f64 * period_s;
    let meas_to = meas_from + period_s;
    // In pattern mode the run must extend until the last bit has fully
    // traversed the pipeline (bit j occupies stage n−1 until
    // (j+1)·T + (n+1)·q).
    let stop = if bit_meas {
        let last_exit = bits.len() as f64 * period_s + (n_stages + 2) as f64 * q;
        meas_to.max(last_exit)
    } else {
        meas_to
    };
    let c_load = 100e-15; // 100 fF per rail

    let mut lines: Vec<String> = Vec::new();

    lines.push(format!(
        "2LAL 4-phase dual-rail shift register (T={period_s:.6e}, {n_stages} stages)"
    ));
    // is=1e-18: suppress the level-1 default junction leakage (see doc above).
    lines.push(".model NM NMOS (level=1 vto=0.4 kp=200u is=1e-18)".into());
    lines.push(".model PM PMOS (level=1 vto=-0.4 kp=100u is=1e-18)".into());
    lines.push(".options gmin=1e-16".into());
    // VDD is a bulk bias only (PMOS n-well); it carries no channel current.
    lines.push("Vdd vdd 0 DC 1.0".into());

    // Clock rails φ0..φ3: rise tick i, high tick i+1, fall tick i+2, low
    // tick i+3. φ0/φ1 as positive PULSEs, φ2/φ3 as inverted PULSEs so that
    // the t=0 values are exact (φ2 starts falling from 1 V, φ3 is high).
    lines.push(format!(
        "Vp0 p0 0 PULSE(0 1 0 {q:.9e} {q:.9e} {q:.9e} {period_s:.9e})"
    ));
    lines.push(format!(
        "Vp1 p1 0 PULSE(0 1 {q:.9e} {q:.9e} {q:.9e} {q:.9e} {period_s:.9e})"
    ));
    lines.push(format!(
        "Vp2 p2 0 PULSE(1 0 0 {q:.9e} {q:.9e} {q:.9e} {period_s:.9e})"
    ));
    lines.push(format!(
        "Vp3 p3 0 PULSE(1 0 {q:.9e} {q:.9e} {q:.9e} {q:.9e} {period_s:.9e})"
    ));

    // Input token stream S_{-1}: dual-rail PWL of `bits`, one token per
    // period, valid one tick before stage 0 computes (offset 3q). Drives
    // gates only.
    let d_in = 3.0 * q;
    let (in_t, in_c) = dual_rail_pwl(bits, period_s, d_in, 0.01);
    lines.push(format!("Vint int 0 PWL({in_t})"));
    lines.push(format!("Vinc inc 0 PWL({in_c})"));
    // Data-aware terminator: the phantom successor S_n gating the last
    // stage's reverse TGs is the SAME bit stream delayed by (n+1) ticks --
    // exactly what a real stage n would hold -- so the tail decomputes
    // correctly for arbitrary data. Drives gates only (see doc above).
    let d_ack = d_in + (n_stages + 1) as f64 * q;
    let (ack_t, ack_c) = dual_rail_pwl(bits, period_s, d_ack, 0.015);
    lines.push(format!("Vackt ackt 0 PWL({ack_t})"));
    lines.push(format!("Vackc ackc 0 PWL({ack_c})"));

    for k in 0..n_stages {
        let pt = format!("p{}", k % 4); // true-rail clock
        let pc = format!("p{}", (k + 2) % 4); // complement-rail clock
        let (gt, gc) = if k == 0 {
            ("int".to_string(), "inc".to_string())
        } else {
            (format!("o{}", k - 1), format!("o{}b", k - 1))
        };
        let (rt, rc) = if k == n_stages - 1 {
            ("ackt".to_string(), "ackc".to_string())
        } else {
            (format!("o{}", k + 1), format!("o{}b", k + 1))
        };
        let ot = format!("o{k}");
        let oc = format!("o{k}b");

        // Forward TGs (gated by the previous stage's rails).
        lines.push(format!("MNF{k}t {pt} {gt} {ot} 0   NM W=4u L=0.15u"));
        lines.push(format!("MPF{k}t {pt} {gc} {ot} vdd PM W=8u L=0.15u"));
        lines.push(format!("MNF{k}c {pc} {gt} {oc} 0   NM W=4u L=0.15u"));
        lines.push(format!("MPF{k}c {pc} {gc} {oc} vdd PM W=8u L=0.15u"));
        // Reverse TGs (gated by the next stage's rails): energy recovery.
        lines.push(format!("MNR{k}t {pt} {rt} {ot} 0   NM W=4u L=0.15u"));
        lines.push(format!("MPR{k}t {pt} {rc} {ot} vdd PM W=8u L=0.15u"));
        lines.push(format!("MNR{k}c {pc} {rt} {oc} 0   NM W=4u L=0.15u"));
        lines.push(format!("MPR{k}c {pc} {rc} {oc} vdd PM W=8u L=0.15u"));
        // Rail loads and rest-state initial conditions.
        lines.push(format!("C{k}t {ot} 0 {c_load:.2e}"));
        lines.push(format!("C{k}c {oc} 0 {c_load:.2e}"));
        lines.push(format!(".ic V({ot})=0 V({oc})=1"));
    }

    // Honest power sum: EVERY V-source in the deck contributes to ptot --
    // the four clock rails (the real power supplies), the VDD bulk bias
    // (junction leakage only), and the gate-drive boundary sources (input
    // + ack replica; ~0 by the zero-gate-cap rung-0 idealization, but
    // included rather than assumed, and integrated separately as ein/eack
    // so the tests can verify the ~0 claim).
    for i in 0..4 {
        lines.push(format!("Bp{i} pwr{i} 0 V = -V(p{i})*I(Vp{i})"));
    }
    lines.push("Bpv pwrv 0 V = -V(vdd)*I(Vdd)".into());
    lines.push("Bpin pwrin 0 V = -V(int)*I(Vint)-V(inc)*I(Vinc)".into());
    lines.push("Bpak pwrak 0 V = -V(ackt)*I(Vackt)-V(ackc)*I(Vackc)".into());
    lines.push("Btot ptot 0 V = V(pwr0)+V(pwr1)+V(pwr2)+V(pwr3)+V(pwrv)+V(pwrin)+V(pwrak)".into());

    // Control block.
    lines.push(".control".into());
    lines.push(format!("tran {step:.9e} {stop:.9e} uic"));
    lines.push(format!(
        "meas tran etot INTEG V(ptot) FROM={meas_from:.9e} TO={meas_to:.9e}"
    ));
    lines.push(format!(
        "meas tran ein INTEG V(pwrin) FROM={meas_from:.9e} TO={meas_to:.9e}"
    ));
    lines.push(format!(
        "meas tran eack INTEG V(pwrak) FROM={meas_from:.9e} TO={meas_to:.9e}"
    ));
    for k in 0..n_stages {
        lines.push(format!(
            "meas tran vo{k}max MAX V(o{k}) FROM={meas_from:.9e} TO={meas_to:.9e}"
        ));
        lines.push(format!(
            "meas tran vo{k}bmin MIN V(o{k}b) FROM={meas_from:.9e} TO={meas_to:.9e}"
        ));
    }
    if bit_meas {
        // Per-cycle total-energy integrals (proves no per-0-bit ~CV² tail
        // burn: a broken terminator shows up as a period-independent
        // ~1.25e-13 J spike in the cycle where a 0-bit retires).
        let n_cycles = (stop / period_s).floor() as usize;
        for i in 0..n_cycles {
            let f = i as f64 * period_s;
            let t = f + period_s;
            lines.push(format!(
                "meas tran ecyc{i} INTEG V(ptot) FROM={f:.9e} TO={t:.9e}"
            ));
        }
        // Per-stage/per-bit levels: bit j occupies stage k (plateau) during
        // [(j+1)T+(k+1)q, (j+1)T+(k+2)q]. Sample the middle 60% of the
        // window. For a 1-bit assert full assertion levels (true rail MIN
        // > 0.9, complement MAX < 0.1); for a 0-bit assert full rest
        // levels (true MAX < 0.1, complement MIN > 0.9).
        for k in 0..n_stages {
            for (j, &b) in bits.iter().enumerate() {
                let w0 = (j + 1) as f64 * period_s + (k + 1) as f64 * q + 0.2 * q;
                let w1 = (j + 1) as f64 * period_s + (k + 2) as f64 * q - 0.2 * q;
                let (ft, fc) = if b { ("MIN", "MAX") } else { ("MAX", "MIN") };
                lines.push(format!(
                    "meas tran s{k}d{j}t {ft} V(o{k}) FROM={w0:.9e} TO={w1:.9e}"
                ));
                lines.push(format!(
                    "meas tran s{k}d{j}c {fc} V(o{k}b) FROM={w0:.9e} TO={w1:.9e}"
                ));
            }
        }
    }
    finalize_deck(lines)
}

/// Sweep the power-clock period and return `(period_s, energy_per_cycle_J)`
/// for each period. Energy is the total source energy (clocks + bulk +
/// gate-drive boundary sources) integrated over one settled cycle after the
/// fill-scaled warmup (`ceil(n/4)+2` cycles).
pub fn two_lal_sr_curve(periods: &[f64], n_stages: usize) -> Vec<(f64, f64)> {
    periods
        .iter()
        .map(|&t| {
            let netlist = two_lal_sr_netlist(t, n_stages);
            let e = run_energy(&netlist, "etot");
            (t, e.abs())
        })
        .collect()
}

/// The canonical three-family money plot (ADR-0023): the adiabatic
/// transmission-gate cell (slope −1, keeps falling), the PFAL buffer (falls then
/// floors), and the static-CMOS control (flat), over one shared period axis.
pub fn money_plot(periods: &[f64]) -> Vec<MoneyPlotCurve> {
    vec![
        adiabatic_tg_curve(periods),
        pfal_curve(periods),
        cmos_baseline_curve(periods),
    ]
}

/// Rung-aware three-family money plot (W6): the same three families as
/// [`money_plot`] -- adiabatic transmission-gate, PFAL, static-CMOS control --
/// but generated at the given [`DeviceRung`] via the `_rung` curve functions.
///
/// At [`DeviceRung::Rung1Sky130`] the dut names carry the `-sky130` suffix
/// (`adiabatic-tg-sky130`, `pfal-sky130`, `cmos-static-sky130`), and the
/// energies are the honest `etot` sum over all sources on the settled second
/// cycle. This does NOT modify the rung-0 [`money_plot`]; it lives alongside it.
pub fn money_plot_rung(periods: &[f64], rung: DeviceRung) -> Vec<MoneyPlotCurve> {
    vec![
        adiabatic_tg_curve_rung(periods, rung),
        pfal_curve_rung(periods, rung),
        cmos_baseline_curve_rung(periods, rung),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bench::ngspice_available;

    /// Default adiabatic-regime sweep: ~3 decades of period, ≥2 pts/decade.
    fn regime_periods() -> Vec<f64> {
        vec![1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5]
    }

    /// The money-plot self-check: energy-per-op falls as 1/period (slope −1).
    #[test]
    fn adiabatic_tg_slope_is_minus_one() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let curve = adiabatic_tg_curve(&regime_periods());
        // All points must be positive, monotone-decreasing energy with period.
        for w in curve.points.windows(2) {
            assert!(
                w[0].1 > 0.0 && w[1].1 > 0.0 && w[1].1 < w[0].1,
                "energy must be positive and fall with period: {:?}",
                curve.points
            );
        }
        let slope = curve.slope();
        assert!(
            (-1.1..=-0.9).contains(&slope),
            "adiabatic slope must be −1 ± 0.1, got {slope:.4} for {:?}",
            curve.points
        );
    }

    /// The cell must actually charge (do work): source energy is non-trivial and
    /// the load reaches near VDD -- a circuit that dissipates ~0 by doing nothing
    /// must not pass the slope check vacuously.
    #[test]
    fn adiabatic_tg_does_work() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let e = run_energy(&adiabatic_tg_netlist(1e-7), "esrc");
        assert!(
            e > 1e-16,
            "cell should dissipate a measurable amount, got {e:e}"
        );
    }

    /// The CMOS control is FLAT: ~CV² per cycle regardless of period (slope ≈ 0),
    /// and -- crucially -- at a slow period it dissipates FAR more than the
    /// adiabatic cell (the whole point of the money plot).
    #[test]
    fn cmos_baseline_is_flat_and_dominates_adiabatic() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let periods = regime_periods();
        let cmos = cmos_baseline_curve(&periods);
        let slope = cmos.slope();
        assert!(
            (-0.1..=0.1).contains(&slope),
            "CMOS baseline must be flat (slope ≈ 0), got {slope:.4} for {:?}",
            cmos.points
        );
        // At the slowest period the adiabatic cell must win by a wide margin.
        let adia = adiabatic_tg_curve(&periods);
        let (_, e_cmos_slow) = *cmos.points.last().unwrap();
        let (_, e_adia_slow) = *adia.points.last().unwrap();
        assert!(
            e_adia_slow < e_cmos_slow / 100.0,
            "adiabatic cell should beat CMOS by >100× at the slow end: adia={e_adia_slow:e}, cmos={e_cmos_slow:e}"
        );
    }

    /// Wide sweep that exposes PFAL's adiabatic regime AND its floor.
    fn wide_periods() -> Vec<f64> {
        vec![1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4]
    }

    /// PFAL is the middle family: it recovers energy (slope ≈ −1) at fast
    /// periods but FLOORS -- the energy stops falling (a static path dissipates
    /// ~constant power), so the minimum is reached at an intermediate period,
    /// unlike the pure-adiabatic cell which keeps falling.
    #[test]
    fn pfal_falls_then_floors() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let periods = wide_periods();
        let pfal = pfal_curve(&periods);
        // Fast-end (first 3 points) is adiabatic: slope ≈ −1.
        let fast_slope = log_log_slope(&pfal.points[..3]);
        assert!(
            (-1.2..=-0.8).contains(&fast_slope),
            "PFAL fast-end slope must be ≈ −1, got {fast_slope:.3} for {:?}",
            pfal.points
        );
        // It floors: the minimum energy is NOT at the slowest period (there is a
        // turnaround), whereas the pure-adiabatic cell would keep dropping.
        let min_i = pfal
            .points
            .iter()
            .enumerate()
            .min_by(|a, b| a.1 .1.partial_cmp(&b.1 .1).unwrap())
            .unwrap()
            .0;
        assert!(
            min_i < pfal.points.len() - 1,
            "PFAL must floor (min energy before the slowest period), got min at index {min_i} of {:?}",
            pfal.points
        );
    }

    /// The money plot exposes all three families over one period axis.
    #[test]
    fn money_plot_has_three_families() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let curves = money_plot(&regime_periods());
        assert_eq!(curves.len(), 3);
        for dut in ["adiabatic-tg", "pfal", "cmos-static"] {
            assert!(curves.iter().any(|c| c.dut == dut), "missing {dut}");
        }
    }

    // ----- rung 1: sky130 adiabatic TG -----

    /// Rung-1 sky130 adiabatic-TG energy falls with period over the adiabatic
    /// regime. Requires ngspice AND the sky130 PDK (the `.#char` devShell);
    /// skipped otherwise, like the ngspice-gated rung-0 tests.
    ///
    /// Measured (sky130A tt, W=4/8 µm, L=0.15 µm, 1 pF load, settled 2nd
    /// cycle, ALL sources summed): slope = −0.96 over 3.16e-8..1e-5 s --
    /// real devices, not a clean −1. The curve is bounded on both ends:
    /// at T = 1e-8 s the ramp (T/4 = 2.5 ns) approaches the TG's R_on·C ≈
    /// 0.7 ns and the decade slope shallows to −0.78 (finite-ramp knee);
    /// at slow T a static floor (junction/bulk leakage ≈ 3.3e-12 W through
    /// the bulk at ngspice's default gmin, plus a similar clock-side term)
    /// turns the curve around at T ≈ 1e-4 s (energy minimum ≈ 7.5e-16 J,
    /// rising again by 3.16e-4 s). The bound −1.15 ≤ slope ≤ −0.85 keeps
    /// margin around the measured −0.96 while still failing on any
    /// qualitative regression (a leak path or a non-recovering cell
    /// flattens the slope far above −0.85).
    #[test]
    fn sky130_adiabatic_tg_energy_falls_with_period() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        if !sky130_available() {
            eprintln!("[skip] sky130 PDK unavailable (THESEUS_SKY130_LIB unset/missing)");
            return;
        }
        // 2.5 decades, inside the adiabatic regime: above the fast-end knee
        // (at T = 1e-8 the ramp T/4 = 2.5 ns approaches the TG's R_on·C ≈
        // 0.7 ns and the decade slope shallows to ≈ −0.78) and below the
        // slow-end leakage knee (≈ 1e-4 s, where the ~3e-12 W static floor
        // crosses the shrinking adiabatic term).
        let periods = [3.16e-8, 1e-7, 1e-6, 1e-5];
        let curve = adiabatic_tg_curve_rung(&periods, DeviceRung::Rung1Sky130);
        eprintln!("rung1 sky130 TG curve: {:?}", curve.points);
        // Energy must be positive and strictly falling with period.
        for w in curve.points.windows(2) {
            assert!(
                w[0].1 > 0.0 && w[1].1 > 0.0 && w[1].1 < w[0].1,
                "rung1 energy must be positive and fall with period: {:?}",
                curve.points
            );
        }
        let slope = curve.slope();
        eprintln!("rung1 sky130 TG slope = {slope:.4}");
        assert!(
            (-1.15..=-0.85).contains(&slope),
            "rung1 adiabatic slope must be in [-1.15, -0.85], got {slope:.4} for {:?}",
            curve.points
        );
        // The cell must actually do work: full-swing output on the settled
        // cycle (vmax ≈ VDD), so the slope check cannot pass vacuously.
        let nl = adiabatic_tg_netlist_rung(1e-7, DeviceRung::Rung1Sky130);
        let vmax = run_energy(&nl, "vmax");
        assert!(
            vmax > 0.9 * DeviceRung::Rung1Sky130.vdd(),
            "rung1 TG output must reach near VDD, got {vmax:.4} V"
        );
    }

    // ----- rung 1: sky130 baselines (CMOS control + PFAL) -----

    /// Rung-1 sky130 CMOS baseline is FLAT: ~½CV² per switch, independent of
    /// period. Requires ngspice AND the sky130 PDK; skipped otherwise.
    ///
    /// Measured (sky130A tt, W=4/8 µm, L=0.15 µm, 1 pF load, settled 2nd
    /// cycle, ALL sources summed): etot ≈ 3.28e-12 J at every period from
    /// 1e-8..1e-6 s (rail evdd ≈ 3.27e-12 J dominates; input gate-drive
    /// ein ≈ 6.5e-15 J is negligible), for slope ≈ 0 -- a real static-CMOS
    /// control that dumps a fixed CV² per cycle whatever the clock does. The
    /// output swings full rail (vmax ≈ 1.8 V, vmin ≈ 0). This is the baseline
    /// the adiabatic cells must beat at slow periods; it never falls.
    #[test]
    fn sky130_cmos_baseline_is_flat() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        if !sky130_available() {
            eprintln!("[skip] sky130 PDK unavailable (THESEUS_SKY130_LIB unset/missing)");
            return;
        }
        // 2 decades is enough to expose flatness; kept modest (slope tests
        // are slow). Well inside the regime where the fixed CV² dominates.
        let periods = [1e-8, 1e-7, 1e-6];
        let curve = cmos_baseline_curve_rung(&periods, DeviceRung::Rung1Sky130);
        eprintln!("rung1 sky130 CMOS curve: {:?}", curve.points);
        // Energy must be positive at every point.
        for &(_, e) in &curve.points {
            assert!(
                e > 0.0,
                "rung1 CMOS energy must be positive: {:?}",
                curve.points
            );
        }
        let slope = curve.slope();
        eprintln!("rung1 sky130 CMOS slope = {slope:.4}");
        assert!(
            (-0.1..=0.1).contains(&slope),
            "rung1 CMOS baseline must be flat (slope ≈ 0), got {slope:.4} for {:?}",
            curve.points
        );
        // The inverter must actually switch full-rail (not pass vacuously).
        let nl = cmos_baseline_netlist_rung(1e-7, DeviceRung::Rung1Sky130);
        let vmax = run_energy(&nl, "vmax");
        assert!(
            vmax > 0.9 * DeviceRung::Rung1Sky130.vdd(),
            "rung1 CMOS output must reach near VDD, got {vmax:.4} V"
        );
    }

    /// Rung-1 sky130 PFAL FALLS THEN FLOORS: quasi-adiabatic recovery at fast
    /// periods, a non-recoverable residue at slow periods. Requires ngspice
    /// AND the sky130 PDK; skipped otherwise.
    ///
    /// Measured (sky130A tt, W=8 µm P / W=4 µm N, L=0.15 µm, 100 fF/rail,
    /// settled 2nd cycle, ALL sources summed, etot ≈ epc -- the clock is the
    /// whole story, bulk/gate ≈ 0):
    ///   T=1e-8: 2.71e-14   T=1e-7: 3.42e-15   T=1e-6: 6.19e-16 (min)
    ///   T=1e-5: 2.56e-15 (risen again)
    /// The fast half (1e-8..1e-6) falls at slope ≈ −0.82 (quasi-adiabatic,
    /// NOT a clean −1 -- real devices); the minimum is at T ≈ 1e-6 s and the
    /// curve turns UP by 1e-5 s, so the slow-end pair 1e-6→1e-5 has slope ≈
    /// +0.6, i.e. NOT still falling. The floor is the non-recoverable residue
    /// of the resolved latch (a static/crowbar path). Between CMOS (flat) and
    /// 2LAL (keeps falling), as intended.
    #[test]
    fn sky130_pfal_falls_then_floors() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        if !sky130_available() {
            eprintln!("[skip] sky130 PDK unavailable (THESEUS_SKY130_LIB unset/missing)");
            return;
        }
        // 3 decades bracketing the floor: fast end falls, slow end turns.
        let periods = [1e-8, 1e-7, 1e-6, 1e-5];
        let curve = pfal_curve_rung(&periods, DeviceRung::Rung1Sky130);
        eprintln!("rung1 sky130 PFAL curve: {:?}", curve.points);
        for &(_, e) in &curve.points {
            assert!(
                e > 0.0,
                "rung1 PFAL energy must be positive: {:?}",
                curve.points
            );
        }
        // (a) Fast-end (first 3 points) is quasi-adiabatic: falls, slope < −0.5
        // (measured ≈ −0.82; NOT asserted to be a clean −1, these are real
        // devices with an imperfect recovery).
        let fast_slope = log_log_slope(&curve.points[..3]);
        eprintln!("rung1 sky130 PFAL fast-end slope = {fast_slope:.4}");
        assert!(
            fast_slope < -0.5,
            "PFAL fast-end must fall (slope < −0.5), got {fast_slope:.3} for {:?}",
            curve.points
        );
        // (b) It floors: the minimum is NOT at the slowest period (a clean
        // adiabatic cell would keep falling), and the slow-end pair is no
        // longer falling -- slope near 0 or positive (measured ≈ +0.6).
        let min_i = curve
            .points
            .iter()
            .enumerate()
            .min_by(|a, b| a.1 .1.partial_cmp(&b.1 .1).unwrap())
            .unwrap()
            .0;
        assert!(
            min_i < curve.points.len() - 1,
            "PFAL must floor (min energy before the slowest period), got min at index {min_i} of {:?}",
            curve.points
        );
        let slow_slope = log_log_slope(&curve.points[curve.points.len() - 2..]);
        eprintln!("rung1 sky130 PFAL slow-end slope = {slow_slope:.4}");
        assert!(
            slow_slope > -0.2,
            "PFAL slow-end must have floored (slope > −0.2), got {slow_slope:.3} for {:?}",
            curve.points
        );
    }

    // ----- sky130 variant axes (pure emission -- no simulator needed) -----

    /// The default variant emits byte-identical device cards to the
    /// pre-variant rung-1 emitter, so every existing rung-1 cell is the
    /// (std, body-to-rail) corner of the matrix by construction.
    #[test]
    fn default_variant_matches_rung1_fet_cards() {
        let rung = DeviceRung::Rung1Sky130;
        let vt = Sky130Vt::default();
        assert_eq!(
            vt.fet("1", FetKind::Nmos, "pc", "ing", "out", "0", 4.0, 0.15),
            rung.fet("1", FetKind::Nmos, "pc", "ing", "out", "0", 4.0, 0.15),
        );
        assert_eq!(
            vt.fet("2", FetKind::Pmos, "pc", "inb", "out", "vdd", 8.0, 0.15),
            rung.fet("2", FetKind::Pmos, "pc", "inb", "out", "vdd", 8.0, 0.15),
        );
        // And the default-variant TG cards are exactly those two lines (no
        // DNW stack).
        let cards = Sky130Variant::default().tg_cards("1", "2");
        assert_eq!(cards.len(), 2);
        assert!(cards[0].contains("sky130_fd_pr__nfet_01v8 "));
        assert!(cards[1].contains("sky130_fd_pr__pfet_01v8 "));
    }

    /// The discovered PDK mapping is encoded honestly: lvt exists for both
    /// device types, hvt only for PMOS (NMOS falls back to std), and the
    /// lvt PMOS L is clamped to its 0.35 µm binning minimum.
    #[test]
    fn variant_device_mapping_and_lvt_pmos_l_clamp() {
        assert_eq!(Sky130Vt::Lvt.nmos_model(), "sky130_fd_pr__nfet_01v8_lvt");
        assert_eq!(Sky130Vt::Lvt.pmos_model(), "sky130_fd_pr__pfet_01v8_lvt");
        assert_eq!(Sky130Vt::Hvt.pmos_model(), "sky130_fd_pr__pfet_01v8_hvt");
        // No hvt NMOS on this PDK: documented fallback to std.
        assert_eq!(Sky130Vt::Hvt.nmos_model(), "sky130_fd_pr__nfet_01v8");
        // lvt PMOS below its binning minimum is clamped up to L=0.35.
        let card = Sky130Vt::Lvt.fet("2", FetKind::Pmos, "pc", "inb", "out", "vdd", 8.0, 0.15);
        assert!(
            card.ends_with("W=8 L=0.35"),
            "lvt PMOS L not clamped: {card}"
        );
        // NMOS and non-lvt PMOS keep the requested L.
        let n = Sky130Vt::Lvt.fet("1", FetKind::Nmos, "pc", "ing", "out", "0", 4.0, 0.15);
        assert!(n.ends_with("W=4 L=0.15"), "{n}");
        let h = Sky130Vt::Hvt.fet("2", FetKind::Pmos, "pc", "inb", "out", "vdd", 8.0, 0.15);
        assert!(h.ends_with("W=8 L=0.15"), "{h}");
    }

    /// Deep-n-well wiring: NMOS bulk to the load node (source-tied body) and
    /// the two PDK parasitic isolation diodes; the grid is the full 3 × 2.
    #[test]
    fn dnw_variant_cards_and_grid() {
        let v = Sky130Variant {
            vt: Sky130Vt::Std,
            body: BodyConfig::DeepNWell,
        };
        let cards = v.tg_cards("1", "2");
        assert_eq!(cards.len(), 3);
        assert!(
            cards[0].contains(" pc ing out out sky130_fd_pr__nfet_01v8 "),
            "NMOS bulk must tie to the load node under DNW: {}",
            cards[0]
        );
        assert!(cards[2].contains("diode_pw2dn"), "{}", cards[2]);
        assert!(cards[2].contains("diode_ps2dn"), "{}", cards[2]);
        assert_eq!(Sky130Variant::all().len(), 6);
        let keys: Vec<String> = Sky130Variant::all().iter().map(|v| v.key()).collect();
        assert_eq!(
            keys,
            ["std-b2r", "std-dnw", "lvt-b2r", "lvt-dnw", "hvt-b2r", "hvt-dnw"]
        );
        // Load axis: min / 4× / 16×, with 16× = the canonical 1 pF.
        let loads = tg_load_axis();
        assert_eq!(loads[1], 4.0 * loads[0]);
        assert_eq!(loads[2], 16.0 * loads[0]);
        assert_eq!(loads[2], 1e-12);
    }

    // ----- 2LAL shift register -----

    /// 2LAL shift register: sweep ≥2 decades (this sweep spans 3), assert
    /// slope ≈ −1 ± 0.15 (all-1s stream) and stage rails reach >0.9 V.
    ///
    /// Measured (canonical 4-phase forward/reverse-TG topology, see
    /// [`two_lal_sr_netlist`]): slope ≈ −0.99 over 1e-8..1e-5 s; all stage
    /// rails reach 1.0 V. NOTE: sustaining the 3rd decade (1e-6..1e-5 s)
    /// requires `.options gmin=1e-16` + `is=1e-18` in the deck -- ngspice's
    /// default gmin=1e-12 junction shunts inject a fake ~5e-11 W static
    /// load that collapses the 3rd decade to slope ≈ −0.13. That is honest
    /// artifact removal, not tuning: a *real* DC leak in the topology would
    /// not be cured by lowering gmin (there is no channel path to GND or
    /// VDD by construction -- the circuit is powered only by the clocks).
    #[test]
    fn two_lal_sr_slope_is_minus_one() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }

        let n_stages = 8;
        let periods = regime_periods();
        let curve = two_lal_sr_curve(&periods, n_stages);

        // (a) First and last stage rails must reach >0.9 V (functional;
        // arbitrary-pattern propagation is covered per-stage/per-bit by
        // `two_lal_sr_propagates_pattern_adiabatically`).
        let slowest = *periods.last().unwrap();
        let nl = two_lal_sr_netlist(slowest, n_stages);
        for k in [0, n_stages - 1] {
            let meas_name = format!("vo{k}max");
            let v = run_energy(&nl, &meas_name);
            assert!(v > 0.9, "stage {k} rail must reach >0.9 V, got {v:.4} V");
        }

        // (b) Log-log slope of energy vs period.
        let slope = log_log_slope(&curve);
        eprintln!(
            "2LAL SR slope = {slope:.4} over {} points: {:?}",
            curve.len(),
            curve
        );
        assert!(
            (-1.15..=-0.85).contains(&slope),
            "2LAL SR slope must be −1 ± 0.15, got {slope:.4} for {:?}",
            curve
        );
    }

    /// Functional + adiabaticity check on a NON-CONSTANT pattern.
    ///
    /// Injects 1,0,1,1,0,0,1 and asserts, for EVERY stage k (including the
    /// last) and EVERY bit j, that stage k holds bit j at full level during
    /// its occupancy window (input delayed by k+1 ticks): true rail >0.9 V
    /// for a 1 / <0.1 V for a 0, complement rail the mirror. This replaces
    /// the vacuous all-1s `vo{k}max` check, which passes on a dead chain.
    ///
    /// Also asserts the energy stays adiabatic on real data: every settled
    /// per-cycle energy -- including the cycles where 0-bits retire at the
    /// tail -- is far below the ~1.25e-13 J period-independent CV² burn
    /// that the old all-1s terminator paid per 0-bit, and the gate-only
    /// boundary sources (input + ack replica) deliver ~0 J (zero-gate-cap
    /// rung-0 models; verified, not assumed).
    #[test]
    fn two_lal_sr_propagates_pattern_adiabatically() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }

        let n_stages = 8;
        let period = 1e-6; // deep-adiabatic regime: ~1e-15 J/cycle expected
        let bits = [true, false, true, true, false, false, true];
        let nl = two_lal_sr_pattern_netlist(period, n_stages, &bits);
        let out = crate::bench::run_ngspice_raw(&nl).expect("ngspice failed");
        let get = |name: &str| -> f64 { crate::bench::require_meas(&out, name) };

        // (a) Per-stage/per-bit full-level propagation, ALL stages.
        for k in 0..n_stages {
            for (j, &b) in bits.iter().enumerate() {
                let vt = get(&format!("s{k}d{j}t"));
                let vc = get(&format!("s{k}d{j}c"));
                if b {
                    assert!(
                        vt > 0.9 && vc < 0.1,
                        "stage {k} bit {j}=1: true rail min {vt:.4} V (want >0.9), \
                         complement max {vc:.4} V (want <0.1)"
                    );
                } else {
                    assert!(
                        vt < 0.1 && vc > 0.9,
                        "stage {k} bit {j}=0: true rail max {vt:.4} V (want <0.1), \
                         complement min {vc:.4} V (want >0.9)"
                    );
                }
            }
        }

        // (b) No non-adiabatic tail burn: every settled cycle (skip cycle 0,
        // which pays the one-time uic transient) stays ≥20× below the
        // ~1.25e-13 J per-0-bit CV² burn of a broken terminator.
        // Whole cycles simulated (mirror of the generator's `stop`):
        // stop = max((warmup+1)·T, bits·T + (n+2)·q), q = T/4.
        let warmup = n_stages.div_ceil(4) + 2;
        let n_cycles = ((bits.len() * 4 + n_stages + 2) / 4).max(warmup + 1);
        let mut worst = 0.0f64;
        for i in 1..n_cycles {
            let e = get(&format!("ecyc{i}")).abs();
            eprintln!("pattern run: cycle {i} energy = {e:.3e} J");
            worst = worst.max(e);
            assert!(
                e < 6e-15,
                "cycle {i} energy {e:.3e} J is not adiabatic (CV² tail burn?)"
            );
        }
        eprintln!("pattern run: worst settled per-cycle energy = {worst:.3e} J");
        eprintln!(
            "pattern run: boundary gate-drive energies ein={:.3e} J, eack={:.3e} J",
            get("ein"),
            get("eack")
        );

        // (c) Gate-only boundary sources verified ~0 J over the etot window.
        let ein = get("ein").abs();
        let eack = get("eack").abs();
        assert!(
            ein < 1e-18 && eack < 1e-18,
            "boundary gate-drive sources must carry ~0 J, got ein={ein:.3e}, eack={eack:.3e}"
        );
    }
}
