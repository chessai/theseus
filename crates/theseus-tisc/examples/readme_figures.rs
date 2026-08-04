//! README figure generator: renders the theseus energy story to committed
//! SVG charts, with ZERO extra crate dependencies -- every SVG byte is
//! emitted by hand from the inline [`svg`] module below.
//!
//! Run: `cargo run -p theseus-tisc --example readme_figures --release`
//!
//! Emits (into `docs/img/`):
//!   * `cpu-money-plot.svg`  -- Figure 1: energy/step vs power-clock ramp,
//!     reversible curves sliding below the flat CMOS 1/2 CV^2 floor (OFFLINE).
//!   * `sky130-money-plot.svg` -- Figure 2: three device families on sky130
//!     (ADIABATIC-TG / pFAL / static CMOS). Requires ngspice + the sky130 PDK;
//!     SKIPPED with a note when unavailable.
//!   * `benchmark-energy.svg` -- Figure 3: reversible vs CMOS-equivalent
//!     energy per compiled benchmark program (OFFLINE).
//!
//! Figure 1 lowering takes a couple of minutes and some GB; figure 3 lowers
//! one step-net per benchmark. The output is deterministic (no clock/random).

use std::error::Error;
use std::fs;
use std::path::Path;

use theseus_core::Value;
use theseus_netlist::lower_iso;
use theseus_spice::chardb::rung0_measured;
use theseus_tisc::money_plot::{cmos_crossing_t_ramp, cpu_money_plot, CMOS_STATIC_E_PER_GATE_J};
use theseus_tisc::step::step;

use svg::{bar_chart, log_log_chart, Annotation, BarGroup, BarValue, Series};

/// Power-clock ramp / period axis, ~2 points/decade across the characterized
/// [1e-8, 1e-5] s range (shared by every figure for comparability).
const AXIS: [f64; 7] = [1e-8, 3.16e-8, 1e-7, 3.16e-7, 1e-6, 3.16e-6, 1e-5];

/// Fixed reference ramp for the per-benchmark bar chart (Figure 3).
const T_RAMP_REF: f64 = 1e-6;

/// Colorblind-friendly palette (ColorBrewer Dark2 / RdBu accents).
const C_REVERSIBLE: &str = "#1b9e77"; // strong teal -- full reversible step
const C_REVERSIBLE_2: &str = "#66c2a5"; // lighter teal -- active-logic subset
const C_CMOS: &str = "#d95f02"; // warm orange -- CMOS reference (dashed)
const C_PRODUCTIVE: &str = "#7570b3"; // muted violet -- productive share

const OUT_DIR: &str = "docs/img";

const FIB_SRC: &str = include_str!("../../../conformance/programs/0022-fib.thse");
const ADDER_SRC: &str = include_str!("../../../conformance/programs/0024-adder.thse");

fn main() -> Result<(), Box<dyn Error>> {
    let out = Path::new(OUT_DIR);
    fs::create_dir_all(out)?;
    let db = rung0_measured();

    figure1_cpu_money_plot(out, &db)?;
    figure2_sky130(out)?;
    figure3_benchmarks(out, &db)?;

    Ok(())
}

// --------------------------------------------------------------------------
// Figure 1 -- CPU money plot (OFFLINE): reversible energy/step slides below
// the flat CMOS 1/2 CV^2 floor as the power-clock ramp lengthens.
// --------------------------------------------------------------------------

fn figure1_cpu_money_plot(
    out: &Path,
    db: &theseus_spice::chardb::CharDb,
) -> Result<(), Box<dyn Error>> {
    // The reference ROM from `cpu_money_plot.rs`: `decode_execute` dominates the
    // step, so the exact program barely moves the machine-level energy.
    let rom: Vec<u32> = vec![
        (0b011u32 << 25) | 1,                // ROTLI r0, #1
        (0b101u32 << 25) | (1 << 20) | 0xFF, // reserved I-format opc (decodes None)
    ];

    eprintln!("[fig1] lowering the TISC-1 step (~2 min, unbalanced)...");
    let net = lower_iso(&step(&rom))?;
    let plot = cpu_money_plot(&net, db, &AXIS).map_err(|e| format!("cpu_money_plot: {e:?}"))?;

    let full: Vec<(f64, f64)> = plot
        .full_step
        .iter()
        .map(|p| (p.t_ramp, p.energy_j))
        .collect();
    let logic: Vec<(f64, f64)> = plot
        .active_logic
        .iter()
        .map(|p| (p.t_ramp, p.energy_j))
        .collect();

    // The CMOS reference is T-independent: a flat line spanning the x-range.
    let (xmin, xmax) = (AXIS[0], AXIS[AXIS.len() - 1]);
    let cmos: Vec<(f64, f64)> = vec![(xmin, plot.cmos_ref_j), (xmax, plot.cmos_ref_j)];

    let series = vec![
        Series {
            label: format!("reversible: full step ({} cells)", plot.active_cells),
            color: C_REVERSIBLE,
            points: full.clone(),
            dashed: false,
            marker: true,
        },
        Series {
            label: format!("reversible: active logic ({} R1 cells)", plot.r1_cells),
            color: C_REVERSIBLE_2,
            points: logic,
            dashed: false,
            marker: true,
        },
        Series {
            label: "CMOS reference (1/2 CV^2)".to_string(),
            color: C_CMOS,
            points: cmos,
            dashed: true,
            marker: false,
        },
    ];

    // Annotate the crossover: where the full-step curve dives below CMOS.
    let mut annotations = Vec::new();
    if let Some(tc) = cmos_crossing_t_ramp(&plot.full_step, plot.cmos_ref_j) {
        annotations.push(Annotation {
            x: tc,
            y: plot.cmos_ref_j,
            text: format!("crossover ~ {tc:.1e} s"),
            dot: true,
        });
    }

    let svg = log_log_chart(
        "TISC-1 reversible CPU: energy per step",
        "adiabatic dissipation slides below the CMOS 1/2 CV^2 floor as the clock slows",
        "power-clock ramp t_ramp (s)",
        "energy per step (J)",
        &series,
        &annotations,
    );
    write_svg(out, "cpu-money-plot.svg", &svg)
}

// --------------------------------------------------------------------------
// Figure 2 -- sky130 three-family money plot. Requires ngspice + sky130 PDK;
// SKIPPED (with a note) when either is unavailable.
// --------------------------------------------------------------------------

fn figure2_sky130(out: &Path) -> Result<(), Box<dyn Error>> {
    if !theseus_spice::bench::ngspice_available() {
        eprintln!("[fig2] SKIP: ngspice not available -- sky130 money plot needs it.");
        return Ok(());
    }
    if !theseus_spice::adiabatic::sky130_available() {
        eprintln!("[fig2] SKIP: sky130 PDK not available (run inside `.#char`).");
        return Ok(());
    }

    eprintln!("[fig2] running sky130 three-family money plot (ngspice)...");
    let curves = theseus_spice::adiabatic::money_plot_rung(
        &AXIS,
        theseus_spice::adiabatic::DeviceRung::Rung1Sky130,
    );

    // Map each known DUT to a palette colour; label with its fitted slope.
    let series: Vec<Series> = curves
        .iter()
        .map(|c| {
            let (color, dashed) = match c.dut.as_str() {
                "adiabatic-tg-sky130" => (C_REVERSIBLE, false),
                "pfal-sky130" => (C_REVERSIBLE_2, false),
                "cmos-static-sky130" => (C_CMOS, true),
                _ => (C_PRODUCTIVE, false),
            };
            Series {
                label: format!("{} (slope {:+.2})", c.dut, c.slope()),
                color,
                points: c.points.clone(),
                dashed,
                marker: true,
            }
        })
        .collect();

    if series.is_empty() {
        eprintln!("[fig2] SKIP: money_plot_rung returned no curves.");
        return Ok(());
    }

    let svg = log_log_chart(
        "sky130 device families: energy per op",
        "adiabatic-TG recovers charge (slope ~ -1); pFAL falls then floors; static CMOS is flat",
        "power-clock period (s)",
        "energy per op (J)",
        &series,
        &[],
    );
    write_svg(out, "sky130-money-plot.svg", &svg)
}

// --------------------------------------------------------------------------
// Figure 3 -- per-benchmark energy (OFFLINE): reversible total_j vs a
// CMOS-equivalent baseline, grouped bars at a fixed t_ramp.
// --------------------------------------------------------------------------

/// One benchmark's priced result at [`T_RAMP_REF`].
struct BenchResult {
    label: String,
    reversible_j: f64,
    cmos_equiv_j: f64,
}

fn figure3_benchmarks(
    out: &Path,
    db: &theseus_spice::chardb::CharDb,
) -> Result<(), Box<dyn Error>> {
    let mut results = Vec::new();

    // fib(1): the canonical loop program, marshalled exactly as in rom_energy.rs.
    let fib_input = {
        let nat1 = Value::right(Value::left(Value::Unit));
        let pair = Value::pair(Value::right(Value::Unit), Value::left(Value::Unit));
        Value::pair(nat1, pair)
    };
    match price_benchmark("fib", FIB_SRC, "fib", &fib_input, db) {
        Ok(r) => results.push(r),
        Err(e) => eprintln!("[fig3] SKIP fib: {e}"),
    }

    // adder (ha4): compositional half-adder, input (a,b,s,c) = (I,I,O,O).
    let one = || Value::right(Value::Unit); // Bit I
    let zero = || Value::left(Value::Unit); // Bit O
    let adder_input = Value::pair(one(), Value::pair(one(), Value::pair(zero(), zero())));
    match price_benchmark("adder", ADDER_SRC, "ha4", &adder_input, db) {
        Ok(r) => results.push(r),
        Err(e) => eprintln!("[fig3] SKIP adder: {e}"),
    }

    if results.is_empty() {
        eprintln!("[fig3] SKIP: no benchmark priced cleanly.");
        return Ok(());
    }

    let groups: Vec<BarGroup> = results
        .iter()
        .map(|r| BarGroup {
            label: r.label.clone(),
            bars: vec![
                BarValue {
                    label: "reversible (total)".to_string(),
                    color: C_REVERSIBLE,
                    value: r.reversible_j,
                },
                BarValue {
                    label: "CMOS-equivalent".to_string(),
                    color: C_CMOS,
                    value: r.cmos_equiv_j,
                },
            ],
        })
        .collect();

    let svg = bar_chart(
        "Per-benchmark energy: reversible vs CMOS-equivalent",
        &format!("compiled TISC-1 ROMs priced at t_ramp = {T_RAMP_REF:.0e} s (log scale)"),
        "total energy (J)",
        &groups,
        true,
    );
    write_svg(out, "benchmark-energy.svg", &svg)
}

/// Compile, lower, and price one program to `(reversible_j, cmos_equiv_j)` at
/// [`T_RAMP_REF`]. The CMOS-equivalent baseline charges every productively
/// executed step for `active_cells` static-CMOS gate switchings.
fn price_benchmark(
    name: &str,
    src: &str,
    iso_name: &str,
    input: &Value,
    db: &theseus_spice::chardb::CharDb,
) -> Result<BenchResult, Box<dyn Error>> {
    eprintln!("[fig3] pricing '{name}' ({iso_name})...");

    let program = theseus_syntax::parse(src).map_err(|e| format!("parse: {e:?}"))?;
    let env = theseus_check::env::resolve(&program);
    let isos = theseus_check::elaborate::elaborate_program(&program, &env);
    let iso = isos
        .get(iso_name)
        .ok_or_else(|| format!("iso {iso_name:?} not found"))?
        .clone()
        .map_err(|e| format!("elaborate {iso_name:?}: {e:?}"))?;

    let lowered = theseus_tisc::compile::compile_with_metadata(&iso)
        .map_err(|e| format!("compile: {e:?}"))?;
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).map_err(|e| format!("assemble: {e}"))?;

    let step_net = lower_iso(&step(&rom)).map_err(|e| format!("lower step: {e:?}"))?;

    let pe = theseus_tisc::energy::rom_program_energy(&iso, input, db, T_RAMP_REF, &step_net)
        .map_err(|e| format!("rom_program_energy: {e:?}"))?;

    // Derive the per-step CMOS-equivalent from THIS program's step-net, so the
    // baseline reflects the real active-cell inventory.
    let plot = cpu_money_plot(&step_net, db, &[T_RAMP_REF])
        .map_err(|e| format!("cpu_money_plot: {e:?}"))?;
    let cmos_step_j = plot.active_cells as f64 * CMOS_STATIC_E_PER_GATE_J;
    let cmos_equiv_j = pe.steps_t as f64 * cmos_step_j;

    eprintln!(
        "[fig3]   '{name}': {} steps (T={}), reversible {:.3e} J, CMOS-equiv {:.3e} J",
        pe.steps_b, pe.steps_t, pe.total_j, cmos_equiv_j
    );

    Ok(BenchResult {
        label: name.to_string(),
        reversible_j: pe.total_j,
        cmos_equiv_j,
    })
}

fn write_svg(out: &Path, name: &str, svg: &str) -> Result<(), Box<dyn Error>> {
    let path = out.join(name);
    fs::write(&path, svg)?;
    println!("wrote {}", path.display());
    Ok(())
}

// ==========================================================================
// svg -- a small, deterministic, hand-rolled SVG generator for publication
// quality log-log line charts and grouped bar charts. No dependencies, no
// clock/random: identical inputs produce byte-identical output.
// ==========================================================================
mod svg {
    use std::fmt::Write as _;

    // --- Canvas geometry (px). ~70px margins leave room for axis labels. ---
    const W: f64 = 880.0;
    const H: f64 = 520.0;
    const M_LEFT: f64 = 78.0;
    const M_RIGHT: f64 = 28.0;
    const M_TOP: f64 = 74.0;
    const M_BOTTOM: f64 = 64.0;

    const FONT: &str = "system-ui, -apple-system, Segoe UI, Roboto, sans-serif";
    const GRID_MAJOR: &str = "#d0d0d0";
    const GRID_MINOR: &str = "#eeeeee";
    const AXIS_COLOR: &str = "#333333";
    const SUBTITLE_COLOR: &str = "#666666";

    /// One line-chart data series in log-log space.
    pub struct Series {
        /// Legend label.
        pub label: String,
        /// Stroke / swatch colour (SVG colour string).
        pub color: &'static str,
        /// `(x, y)` data points in linear (pre-log) coordinates; must be > 0.
        pub points: Vec<(f64, f64)>,
        /// Draw the polyline dashed (used for the flat CMOS reference).
        pub dashed: bool,
        /// Draw a small filled circle at each data point.
        pub marker: bool,
    }

    /// A callout at a data coordinate, optionally with a dot marker.
    pub struct Annotation {
        /// Data-space x (linear, > 0).
        pub x: f64,
        /// Data-space y (linear, > 0).
        pub y: f64,
        /// Label text.
        pub text: String,
        /// Draw a small emphasis dot at `(x, y)`.
        pub dot: bool,
    }

    /// One bar within a grouped-bar chart.
    pub struct BarValue {
        /// Legend / series label.
        pub label: String,
        /// Fill colour.
        pub color: &'static str,
        /// Bar height in data units.
        pub value: f64,
    }

    /// One group (benchmark) of bars.
    pub struct BarGroup {
        /// Group axis label.
        pub label: String,
        /// The bars in this group (same series order across groups).
        pub bars: Vec<BarValue>,
    }

    /// Plot region bounds in px.
    struct Plot {
        x0: f64,
        y0: f64,
        x1: f64,
        y1: f64,
    }

    impl Plot {
        fn default() -> Self {
            Plot {
                x0: M_LEFT,
                y0: M_TOP,
                x1: W - M_RIGHT,
                y1: H - M_BOTTOM,
            }
        }
    }

    /// Escape the five XML-significant characters for text/attribute content.
    fn esc(s: &str) -> String {
        let mut out = String::with_capacity(s.len());
        for c in s.chars() {
            match c {
                '&' => out.push_str("&amp;"),
                '<' => out.push_str("&lt;"),
                '>' => out.push_str("&gt;"),
                '"' => out.push_str("&quot;"),
                '\'' => out.push_str("&apos;"),
                _ => out.push(c),
            }
        }
        out
    }

    /// Format a float without trailing garbage (deterministic, locale-free).
    fn fnum(v: f64) -> String {
        // 3 decimals is plenty at our pixel resolution; trim trailing zeros.
        let mut s = format!("{v:.3}");
        if s.contains('.') {
            while s.ends_with('0') {
                s.pop();
            }
            if s.ends_with('.') {
                s.pop();
            }
        }
        s
    }

    /// Decade bounds: floor/ceil of log10 across all values, padded slightly so
    /// data never touches the frame. Returns `(lo_exp, hi_exp)` as integers.
    fn decade_bounds(vals: impl Iterator<Item = f64>) -> (i32, i32) {
        let mut lo = f64::INFINITY;
        let mut hi = f64::NEG_INFINITY;
        for v in vals {
            if v > 0.0 && v.is_finite() {
                let l = v.log10();
                lo = lo.min(l);
                hi = hi.max(l);
            }
        }
        if !lo.is_finite() || !hi.is_finite() {
            return (0, 1);
        }
        let mut lo_e = lo.floor() as i32;
        let mut hi_e = hi.ceil() as i32;
        // Only pad a decade if the data sits right on the boundary, so a clean
        // decade span stays clean.
        if (lo - lo_e as f64).abs() < 1e-9 {
            lo_e -= 1;
        }
        if (hi_e as f64 - hi).abs() < 1e-9 {
            hi_e += 1;
        }
        if hi_e <= lo_e {
            hi_e = lo_e + 1;
        }
        (lo_e, hi_e)
    }

    /// A power-of-ten tick label rendered as `10^n` with a superscript tspan.
    fn pow10_label(x: f64, y: f64, exp: i32, anchor: &str) -> String {
        format!(
            "<text x=\"{}\" y=\"{}\" font-size=\"12\" fill=\"{AXIS_COLOR}\" \
             text-anchor=\"{anchor}\" font-family=\"{FONT}\">10<tspan dy=\"-6\" \
             font-size=\"9\">{exp}</tspan></text>",
            fnum(x),
            fnum(y),
        )
    }

    /// Shared SVG header + white background.
    fn header() -> String {
        format!(
            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{}\" height=\"{}\" \
             viewBox=\"0 0 {} {}\" font-family=\"{FONT}\">\n\
             <rect width=\"{}\" height=\"{}\" fill=\"#ffffff\"/>\n",
            fnum(W),
            fnum(H),
            fnum(W),
            fnum(H),
            fnum(W),
            fnum(H),
        )
    }

    /// Title (bold) + subtitle (gray), top-left of the canvas.
    fn titles(s: &mut String, title: &str, subtitle: &str) {
        let _ = writeln!(
            s,
            "<text x=\"{}\" y=\"30\" font-size=\"18\" font-weight=\"700\" \
             fill=\"#111111\" font-family=\"{FONT}\">{}</text>",
            fnum(M_LEFT),
            esc(title),
        );
        let _ = writeln!(
            s,
            "<text x=\"{}\" y=\"50\" font-size=\"12\" fill=\"{SUBTITLE_COLOR}\" \
             font-family=\"{FONT}\">{}</text>",
            fnum(M_LEFT),
            esc(subtitle),
        );
    }

    /// Legend box at the top-right of the plot region: swatch + label per entry.
    fn legend(s: &mut String, p: &Plot, entries: &[(&str, &str, bool)], left: bool) {
        // entries: (label, color, dashed)
        let box_w = 250.0;
        let row_h = 18.0;
        let pad = 8.0;
        let n = entries.len() as f64;
        let box_h = pad * 2.0 + n * row_h;
        // Anchor top-right by default; top-left when `left` (clears tall bars).
        let bx = if left {
            p.x0 + 10.0
        } else {
            p.x1 - box_w - 10.0
        };
        let by = p.y0 + 10.0;
        let _ = writeln!(
            s,
            "<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" rx=\"6\" \
             fill=\"#ffffff\" fill-opacity=\"0.96\" stroke=\"#cccccc\"/>",
            fnum(bx),
            fnum(by),
            fnum(box_w),
            fnum(box_h),
        );
        for (i, (label, color, dashed)) in entries.iter().enumerate() {
            let ry = by + pad + i as f64 * row_h + row_h / 2.0;
            let sw_x = bx + pad;
            let dash = if *dashed {
                " stroke-dasharray=\"5,3\""
            } else {
                ""
            };
            let _ = writeln!(
                s,
                "<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" \
                 stroke-width=\"3\"{}/>",
                fnum(sw_x),
                fnum(ry),
                fnum(sw_x + 20.0),
                fnum(ry),
                color,
                dash,
            );
            let _ = writeln!(
                s,
                "<text x=\"{}\" y=\"{}\" font-size=\"11.5\" fill=\"#222222\" \
                 font-family=\"{FONT}\">{}</text>",
                fnum(sw_x + 28.0),
                fnum(ry + 4.0),
                esc(label),
            );
        }
    }

    // ----------------------------------------------------------------------
    // Log-log line chart.
    // ----------------------------------------------------------------------

    /// Render `series` as a publication-quality log-log line chart, with decade
    /// gridlines (major + faint 2..9 minor), power-of-ten tick labels, a legend,
    /// and optional data-space `annotations`.
    pub fn log_log_chart(
        title: &str,
        subtitle: &str,
        x_label: &str,
        y_label: &str,
        series: &[Series],
        annotations: &[Annotation],
    ) -> String {
        let p = Plot::default();

        // Decade bounds across all series (and annotation coords).
        let xs = series
            .iter()
            .flat_map(|s| s.points.iter().map(|&(x, _)| x))
            .chain(annotations.iter().map(|a| a.x));
        let ys = series
            .iter()
            .flat_map(|s| s.points.iter().map(|&(_, y)| y))
            .chain(annotations.iter().map(|a| a.y));
        let (xlo, xhi) = decade_bounds(xs);
        let (ylo, yhi) = decade_bounds(ys);

        // Data -> pixel mappers (log10; y inverted).
        let px = |x: f64| -> f64 {
            let t = (x.log10() - xlo as f64) / (xhi - xlo) as f64;
            p.x0 + t * (p.x1 - p.x0)
        };
        let py = |y: f64| -> f64 {
            let t = (y.log10() - ylo as f64) / (yhi - ylo) as f64;
            p.y1 - t * (p.y1 - p.y0)
        };

        let mut s = header();
        titles(&mut s, title, subtitle);

        // --- Gridlines: minor (faint) then major, so major wins visually. ---
        for e in xlo..xhi {
            for m in 2..10 {
                let x = px(10f64.powi(e) * m as f64);
                grid_vline(&mut s, x, p.y0, p.y1, GRID_MINOR, 0.75);
            }
        }
        for e in ylo..yhi {
            for m in 2..10 {
                let y = py(10f64.powi(e) * m as f64);
                grid_hline(&mut s, p.x0, p.x1, y, GRID_MINOR, 0.75);
            }
        }
        for e in xlo..=xhi {
            let x = px(10f64.powi(e));
            grid_vline(&mut s, x, p.y0, p.y1, GRID_MAJOR, 1.0);
            s.push_str(&pow10_label(x, p.y1 + 22.0, e, "middle"));
        }
        for e in ylo..=yhi {
            let y = py(10f64.powi(e));
            grid_hline(&mut s, p.x0, p.x1, y, GRID_MAJOR, 1.0);
            s.push_str(&pow10_label(p.x0 - 10.0, y + 4.0, e, "end"));
        }

        // --- Frame. ---
        frame(&mut s, &p);

        // --- Data polylines. ---
        for ser in series {
            if ser.points.is_empty() {
                continue;
            }
            let mut d = String::new();
            for &(x, y) in &ser.points {
                if x <= 0.0 || y <= 0.0 {
                    continue;
                }
                let _ = write!(d, "{},{} ", fnum(px(x)), fnum(py(y)));
            }
            let dash = if ser.dashed {
                " stroke-dasharray=\"8,5\""
            } else {
                ""
            };
            let _ = writeln!(
                s,
                "<polyline points=\"{}\" fill=\"none\" stroke=\"{}\" \
                 stroke-width=\"2.5\" stroke-linejoin=\"round\" \
                 stroke-linecap=\"round\"{}/>",
                d.trim_end(),
                ser.color,
                dash,
            );
            if ser.marker {
                for &(x, y) in &ser.points {
                    if x <= 0.0 || y <= 0.0 {
                        continue;
                    }
                    let _ = writeln!(
                        s,
                        "<circle cx=\"{}\" cy=\"{}\" r=\"3.2\" fill=\"{}\"/>",
                        fnum(px(x)),
                        fnum(py(y)),
                        ser.color,
                    );
                }
            }
        }

        // --- Annotations (callouts). ---
        for a in annotations {
            if a.x <= 0.0 || a.y <= 0.0 {
                continue;
            }
            let ax = px(a.x);
            let ay = py(a.y);
            if a.dot {
                let _ = writeln!(
                    s,
                    "<circle cx=\"{}\" cy=\"{}\" r=\"4.5\" fill=\"none\" \
                     stroke=\"#111111\" stroke-width=\"1.5\"/>",
                    fnum(ax),
                    fnum(ay),
                );
            }
            let _ = writeln!(
                s,
                "<text x=\"{}\" y=\"{}\" font-size=\"11.5\" fill=\"#111111\" \
                 font-weight=\"600\" font-family=\"{FONT}\">{}</text>",
                fnum(ax + 8.0),
                fnum(ay - 8.0),
                esc(&a.text),
            );
        }

        // --- Axis titles. ---
        axis_titles(&mut s, &p, x_label, y_label);

        // --- Legend. ---
        let entries: Vec<(&str, &str, bool)> = series
            .iter()
            .map(|ser| (ser.label.as_str(), ser.color, ser.dashed))
            .collect();
        legend(&mut s, &p, &entries, false);

        s.push_str("</svg>\n");
        s
    }

    // ----------------------------------------------------------------------
    // Grouped bar chart.
    // ----------------------------------------------------------------------

    /// Render `groups` of bars. When `log_y` is set the y-axis is log10 with
    /// decade gridlines and power-of-ten labels; otherwise it is linear. Value
    /// labels sit atop each bar; a legend keys the series colours.
    pub fn bar_chart(
        title: &str,
        subtitle: &str,
        y_label: &str,
        groups: &[BarGroup],
        log_y: bool,
    ) -> String {
        let p = Plot::default();
        let mut s = header();
        titles(&mut s, title, subtitle);

        // Value bounds.
        let vals = groups.iter().flat_map(|g| g.bars.iter().map(|b| b.value));
        let max_v = vals.clone().fold(0.0f64, f64::max);
        let min_v = vals.filter(|v| *v > 0.0).fold(f64::INFINITY, f64::min);

        // Y mapper.
        let (ylo, yhi) = if log_y {
            decade_bounds(groups.iter().flat_map(|g| g.bars.iter().map(|b| b.value)))
        } else {
            (0, 0) // unused in linear branch
        };
        let lin_top = if max_v > 0.0 { max_v * 1.15 } else { 1.0 };
        let py = |v: f64| -> f64 {
            if log_y {
                let vv = if v <= 0.0 { 10f64.powi(ylo) } else { v };
                let t = (vv.log10() - ylo as f64) / (yhi - ylo) as f64;
                p.y1 - t * (p.y1 - p.y0)
            } else {
                let t = v / lin_top;
                p.y1 - t * (p.y1 - p.y0)
            }
        };

        // Y gridlines + labels.
        if log_y {
            for e in ylo..yhi {
                for m in 2..10 {
                    let y = py(10f64.powi(e) * m as f64);
                    grid_hline(&mut s, p.x0, p.x1, y, GRID_MINOR, 0.75);
                }
            }
            for e in ylo..=yhi {
                let y = py(10f64.powi(e));
                grid_hline(&mut s, p.x0, p.x1, y, GRID_MAJOR, 1.0);
                s.push_str(&pow10_label(p.x0 - 10.0, y + 4.0, e, "end"));
            }
        } else {
            for i in 0..=5 {
                let v = lin_top * i as f64 / 5.0;
                let y = py(v);
                grid_hline(&mut s, p.x0, p.x1, y, GRID_MAJOR, 1.0);
                let _ = writeln!(
                    s,
                    "<text x=\"{}\" y=\"{}\" font-size=\"11\" fill=\"{AXIS_COLOR}\" \
                     text-anchor=\"end\" font-family=\"{FONT}\">{}</text>",
                    fnum(p.x0 - 10.0),
                    fnum(y + 4.0),
                    esc(&fmt_sci(v)),
                );
            }
        }
        let _ = (min_v,); // documented as informative; bounds come from decades.

        frame(&mut s, &p);

        // Bars.
        let n_groups = groups.len().max(1) as f64;
        let group_w = (p.x1 - p.x0) / n_groups;
        let group_pad = group_w * 0.16;
        for (gi, g) in groups.iter().enumerate() {
            let gx0 = p.x0 + gi as f64 * group_w + group_pad;
            let inner_w = group_w - 2.0 * group_pad;
            let n_bars = g.bars.len().max(1) as f64;
            let bar_gap = inner_w * 0.08 / n_bars;
            let bar_w = (inner_w - (n_bars - 1.0) * bar_gap) / n_bars;
            for (bi, b) in g.bars.iter().enumerate() {
                let bx = gx0 + bi as f64 * (bar_w + bar_gap);
                let by = py(b.value);
                let bh = (p.y1 - by).max(0.0);
                let _ = writeln!(
                    s,
                    "<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"{}\" \
                     rx=\"2\"/>",
                    fnum(bx),
                    fnum(by),
                    fnum(bar_w),
                    fnum(bh),
                    b.color,
                );
                // Value label atop the bar.
                let _ = writeln!(
                    s,
                    "<text x=\"{}\" y=\"{}\" font-size=\"10.5\" fill=\"#222222\" \
                     text-anchor=\"middle\" font-family=\"{FONT}\">{}</text>",
                    fnum(bx + bar_w / 2.0),
                    fnum(by - 5.0),
                    esc(&fmt_sci(b.value)),
                );
            }
            // Group label under the axis.
            let _ = writeln!(
                s,
                "<text x=\"{}\" y=\"{}\" font-size=\"12.5\" fill=\"#222222\" \
                 text-anchor=\"middle\" font-weight=\"600\" font-family=\"{FONT}\">{}</text>",
                fnum(gx0 + inner_w / 2.0),
                fnum(p.y1 + 22.0),
                esc(&g.label),
            );
        }

        // Axis title (y only) + legend from the first group's series.
        axis_titles(&mut s, &p, "", y_label);
        if let Some(g0) = groups.first() {
            let entries: Vec<(&str, &str, bool)> = g0
                .bars
                .iter()
                .map(|b| (b.label.as_str(), b.color, false))
                .collect();
            legend(&mut s, &p, &entries, true);
        }

        s.push_str("</svg>\n");
        s
    }

    /// Compact scientific label, e.g. `1.2e-9`.
    fn fmt_sci(v: f64) -> String {
        if v == 0.0 {
            return "0".to_string();
        }
        format!("{v:.2e}")
    }

    // --- Small drawing primitives. ---

    fn grid_vline(s: &mut String, x: f64, y0: f64, y1: f64, color: &str, w: f64) {
        let _ = writeln!(
            s,
            "<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" \
             stroke-width=\"{}\"/>",
            fnum(x),
            fnum(y0),
            fnum(x),
            fnum(y1),
            color,
            fnum(w),
        );
    }

    fn grid_hline(s: &mut String, x0: f64, x1: f64, y: f64, color: &str, w: f64) {
        let _ = writeln!(
            s,
            "<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" \
             stroke-width=\"{}\"/>",
            fnum(x0),
            fnum(y),
            fnum(x1),
            fnum(y),
            color,
            fnum(w),
        );
    }

    fn frame(s: &mut String, p: &Plot) {
        let _ = writeln!(
            s,
            "<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"none\" \
             stroke=\"{AXIS_COLOR}\" stroke-width=\"1.25\"/>",
            fnum(p.x0),
            fnum(p.y0),
            fnum(p.x1 - p.x0),
            fnum(p.y1 - p.y0),
        );
    }

    fn axis_titles(s: &mut String, p: &Plot, x_label: &str, y_label: &str) {
        if !x_label.is_empty() {
            let cx = (p.x0 + p.x1) / 2.0;
            let _ = writeln!(
                s,
                "<text x=\"{}\" y=\"{}\" font-size=\"13\" fill=\"{AXIS_COLOR}\" \
                 text-anchor=\"middle\" font-family=\"{FONT}\">{}</text>",
                fnum(cx),
                fnum(H - 18.0),
                esc(x_label),
            );
        }
        if !y_label.is_empty() {
            let cy = (p.y0 + p.y1) / 2.0;
            let _ = writeln!(
                s,
                "<text x=\"22\" y=\"{}\" font-size=\"13\" fill=\"{AXIS_COLOR}\" \
                 text-anchor=\"middle\" transform=\"rotate(-90 22 {})\" \
                 font-family=\"{FONT}\">{}</text>",
                fnum(cy),
                fnum(cy),
                esc(y_label),
            );
        }
    }
}
