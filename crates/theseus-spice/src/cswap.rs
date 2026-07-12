//! k-CSWAP adiabatic cell family (ADR-0027): the k-controlled dual-rail swap,
//! k ∈ {0, 1, 2, 3}, in the verified 2LAL forward/reverse transmission-gate
//! discipline of [`crate::adiabatic::two_lal_sr_netlist`], characterized at
//! both device rungs (ADR-0032: rung 0 level-1 sandbox, rung 1 sky130 tt).
//!
//! # Cells
//!
//! * **k = 0** — the buffer/latch: a token passes through one 2LAL stage.
//! * **k = 1** — the Fredkin gate: `(c, a, b) → (c, c?b:a, c?a:b)`.
//! * **k = 2** — the 2-controlled swap: swap `(a, b)` iff `c1 ∧ c2`; both
//!   controls pass through.
//! * **k = 3** — the 3-controlled swap: swap `(a, b)` iff `c1 ∧ c2 ∧ c3`;
//!   all three controls pass through.
//!
//! All four are reversible and self-inverse; a controlled swap only routes
//! charge, so the count of active rails is invariant per token (INV-TOKEN).
//!
//! # Encoding: 1-of-2 token lines (why the SR's bare rail-pair is not enough)
//!
//! The shift register carries a signal as one rail pair with *level*
//! encoding: the true rail rests 0 and pulses high for a `1` token, the
//! mirror rail rests at VDD and dips. A `0` token is the *absence* of a
//! pulse — indistinguishable from rest. That is sufficient for the buffer,
//! whose gating is monotone, but the CSWAP routing function
//! `a_out = (c ∧ b) ∨ (¬c ∧ a)` is non-monotone in the control: it needs a
//! transmission gate that conducts *because* `c` is 0. Gating that TG by the
//! rail-swapped pair (`¬c` = mirror-as-NMOS-gate) fails, because the mirror
//! rail rests HIGH: the "¬c" TG is ON during the rest ticks, which (a) forms
//! a sneak charge path through the network while the successor replicas are
//! at rest, and (b) re-arms mid-cycle when a `c = 1` token retires,
//! reconnecting a parked internal node to a clock rail at the opposite level
//! — a per-token, period-independent CV² dump that floors the energy curve.
//!
//! The fix is the encoding ADR-0027 already implies ("NOT is a rail swap" of
//! *token lines*): every logical signal is **two token lines** — a 1-line
//! that pulses for a `1` token and a 0-line that pulses for a `0` token —
//! each line being an SR-style (rail, mirror) wire pair for full TG
//! overdrive. Every literal (`c` or `¬c`) then gates a TG from a line that
//! **rests off**, all conduction windows are confined to the token's own
//! ticks, and exactly one line per signal pulses per token (1-of-2; token
//! conservation for free).
//!
//! # Topology (per output line, per wire)
//!
//! The cell is a single 2LAL stage: output rails charge from clock phase
//! `φ0` (trapezoid: rise tick 0, hold tick 1, fall tick 2, low tick 3) and
//! mirror wires from `φ2 = ~φ0`. For an output line with branch condition
//! `F = ⋁ᵢ (ℓᵢ₁ ∧ ℓᵢ₂ ∧ …)` (each `ℓ` a token-line literal):
//!
//! * **forward chain** (per branch): series TGs gated by the input literals
//!   (controls clock-side, the data literal output-side), charging the
//!   output wire from the ramping clock during tick 0 — inputs hold exactly
//!   then (they rise tick 3⁻, hold tick 0, fall tick 1);
//! * **reverse chain**: a parallel TG per segment, **sharing the forward
//!   chain's internal nodes**, gated by the *delayed copies of the same
//!   input literals* (rise tick 1, hold tick 2, fall tick 3). A reverse
//!   branch therefore conducts exactly iff its forward branch conducted, and
//!   during the clock's down-ramp (tick 2) the charge on the output wire
//!   *and on every internal node* rides back into the clock along the exact
//!   path it arrived by: Bennett decomputation. This per-literal mirroring
//!   is load-bearing: gating the data literal by the OUTPUT-line replica
//!   instead lets an active output line switch on the reverse TG of a
//!   branch that did NOT conduct, connecting that branch's parked internal
//!   node to the active wire — a measured ~2.4 fJ/node period-independent
//!   charge-sharing dump on a≠b tokens (rung 0, 5 fF internal nodes).
//!
//!   The delayed input copies are consumer-realizable: the cell is
//!   self-inverse, so a downstream inverse stage's outputs ARE the original
//!   inputs one stage later — exactly the tick-2 window (for controls they
//!   also equal the cell's own passed-through output replicas). The bench
//!   synthesizes them as PWL boundary sources driving gates only, exactly
//!   like the SR's data-aware terminator.
//!
//! Tick 1 is the handoff: forward gates fall and reverse gates rise while
//! the clock holds and every connected node sits at its computed level, so
//! every TG switching event connects/isolates equipotential nodes. Nodes
//! park at the clock rest level between tokens.
//!
//! # Why there is no static path
//!
//! Data wires and internal nodes connect **only to the two clock rails**
//! through TGs; there is no transistor path to GND or VDD anywhere (VDD is
//! the PMOS bulk bias only). Input and replica sources drive transistor
//! gates only. During any tick, conduction exists only between nodes the
//! clock is actively ramping together; a `0` token moves no charge at all.
//!
//! # Measurement honesty (ADR-0028 C3.3)
//!
//! Every source in the deck contributes to the measured `etot` — both clock
//! rails, the bulk bias, and all input/replica gate drivers (integrated
//! separately as `ein`/`eack`: ≈ 0 at rung 0 by the zero-gate-cap
//! idealization, real and counted at rung 1). Energy is integrated over the
//! settled cycle after a fill warmup; functional levels are measured
//! per-token in the middle of the output hold window, for every line.

use std::io;

use crate::adiabatic::{DeviceRung, FetKind};
use crate::bench::{log_log_slope, parse_meas, run_ngspice_raw};

/// One k-CSWAP input token. `c1`/`c2`/`c3` are the controls (`c1` doubles as
/// the single control at k = 1; c2/c3 ignored at k ≤ 1; c3 ignored at k ≤ 2),
/// `a`/`b` the data.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CswapToken {
    pub c1: bool,
    pub c2: bool,
    pub c3: bool,
    pub a: bool,
    pub b: bool,
}

impl CswapToken {
    fn get(self, sig: &str) -> bool {
        match sig {
            "c" | "c1" => self.c1,
            "c2" => self.c2,
            "c3" => self.c3,
            "a" => self.a,
            "b" => self.b,
            other => panic!("unknown signal {other}"),
        }
    }
}

/// The k-CSWAP truth function: swap `(a, b)` iff all k controls are 1.
/// Controls pass through. Self-inverse: `cswap_out(k, cswap_out(k, t)) == t`.
pub fn cswap_out(k: usize, t: CswapToken) -> CswapToken {
    let swap = match k {
        0 => false,
        1 => t.c1,
        2 => t.c1 && t.c2,
        3 => t.c1 && t.c2 && t.c3,
        _ => panic!("k must be 0, 1, 2, or 3"),
    };
    CswapToken {
        a: if swap { t.b } else { t.a },
        b: if swap { t.a } else { t.b },
        ..t
    }
}

/// The cell's signal names (inputs and outputs coincide: every input has a
/// corresponding output line pair named `o<sig>`).
pub fn cswap_signals(k: usize) -> &'static [&'static str] {
    match k {
        0 => &["a"],
        1 => &["c", "a", "b"],
        2 => &["c1", "c2", "a", "b"],
        3 => &["c1", "c2", "c3", "a", "b"],
        _ => panic!("k must be 0, 1, 2, or 3"),
    }
}

/// All control/data combinations for the k-cell's truth-table run:
/// 2 tokens at k = 0, 8 at k = 1, 16 at k = 2.
pub fn cswap_truth_tokens(k: usize) -> Vec<CswapToken> {
    let n = cswap_signals(k).len();
    (0..1usize << n)
        .map(|j| {
            let bit = |i: usize| (j >> (n - 1 - i)) & 1 == 1;
            match k {
                0 => CswapToken { c1: false, c2: false, c3: false, a: bit(0), b: false },
                1 => CswapToken { c1: bit(0), c2: false, c3: false, a: bit(1), b: bit(2) },
                2 => CswapToken { c1: bit(0), c2: bit(1), c3: false, a: bit(2), b: bit(3) },
                3 => CswapToken { c1: bit(0), c2: bit(1), c3: bit(2), a: bit(3), b: bit(4) },
                _ => unreachable!(),
            }
        })
        .collect()
}

/// A token-line literal: (signal, polarity). Polarity `true` gates from the
/// signal's 1-line, `false` from its 0-line — both rest-off by construction.
type Lit = (&'static str, bool);

/// The OR-of-ANDs branch condition for output `out_sig`'s line of polarity
/// `pol`, over input token-line literals. Controls come first (clock-side),
/// the data literal last (output-side) — the ordering the recovery argument
/// in the module docs depends on.
fn cswap_branches(k: usize, out_sig: &str, pol: bool) -> Vec<Vec<Lit>> {
    match (k, out_sig) {
        (0, "a") => vec![vec![("a", pol)]],
        (1, "c") => vec![vec![("c", pol)]],
        (1, "a") => vec![vec![("c", true), ("b", pol)], vec![("c", false), ("a", pol)]],
        (1, "b") => vec![vec![("c", true), ("a", pol)], vec![("c", false), ("b", pol)]],
        (2, "c1") => vec![vec![("c1", pol)]],
        (2, "c2") => vec![vec![("c2", pol)]],
        (2, "a") => vec![
            vec![("c1", true), ("c2", true), ("b", pol)],
            vec![("c1", false), ("a", pol)],
            vec![("c2", false), ("a", pol)],
        ],
        (2, "b") => vec![
            vec![("c1", true), ("c2", true), ("a", pol)],
            vec![("c1", false), ("b", pol)],
            vec![("c2", false), ("b", pol)],
        ],
        (3, "c1") => vec![vec![("c1", pol)]],
        (3, "c2") => vec![vec![("c2", pol)]],
        (3, "c3") => vec![vec![("c3", pol)]],
        (3, "a") => vec![
            vec![("c1", true), ("c2", true), ("c3", true), ("b", pol)],
            vec![("c1", false), ("a", pol)],
            vec![("c2", false), ("a", pol)],
            vec![("c3", false), ("a", pol)],
        ],
        (3, "b") => vec![
            vec![("c1", true), ("c2", true), ("c3", true), ("a", pol)],
            vec![("c1", false), ("b", pol)],
            vec![("c2", false), ("b", pol)],
            vec![("c3", false), ("b", pol)],
        ],
        other => panic!("no branch table for {other:?}"),
    }
}

/// Line base name for a signal's polarity line: `at`/`af`, `c1t`/`c1f`, …
/// Wires are `<base>p` (rail: rests 0, pulses high) and `<base>m` (mirror:
/// rests VDD, dips low).
fn line(sig: &str, pol: bool) -> String {
    format!("{sig}{}", if pol { 't' } else { 'f' })
}

/// Emit the two PWL point lists (rail, mirror) for a token line that pulses
/// on the `true` entries of `pulses`, one token per period, at time offset
/// `off`. `eps_ticks` nudges breakpoints off the clock corners (distinct per
/// source pair, exactly as in the SR — solver robustness, not physics).
fn token_line_pwl(
    pulses: &[bool],
    period_s: f64,
    off: f64,
    eps_ticks: f64,
    vdd: f64,
) -> (String, String) {
    let q = period_s / 4.0;
    let eps = eps_ticks * q;
    let mut p = String::from("0 0");
    let mut m = format!("0 {vdd:.4}");
    for (j, &on) in pulses.iter().enumerate() {
        if on {
            let t0 = j as f64 * period_s + off;
            let (t1, t2, t3, t4) = (t0 - eps, t0 + q - eps, t0 + 2.0 * q + eps, t0 + 3.0 * q + eps);
            p.push_str(&format!(
                " {t1:.9e} 0 {t2:.9e} {vdd:.4} {t3:.9e} {vdd:.4} {t4:.9e} 0"
            ));
            m.push_str(&format!(
                " {t1:.9e} {vdd:.4} {t2:.9e} 0 {t3:.9e} 0 {t4:.9e} {vdd:.4}"
            ));
        }
    }
    (p, m)
}

/// Transmission-gate channel length, µm — minimum L, same geometry as the
/// verified rung-1 TG cell. Longer channels were tried (L = 0.5) to push
/// out the rung-1 OFF-gate leakage floor, and rejected on measurement: the
/// mid-period energy rises ~7× (R_on and gate charge grow) and the leak
/// gets so small that floating 0-token wires drift for many cycles, so the
/// settled-cycle energy is no longer periodic at slow T (etot even turns
/// negative). At L = 0.15 the leak-balance equilibrium is reached within a
/// cycle (τ ≈ C·V/I_leak ≈ 140 ns) and the floor is cleanly measurable.
const TG_L_UM: f64 = 0.15;

/// Fill warmup before the measured cycle (single stage: fill ≤ 1 cycle, +2
/// to settle; rung 1 additionally pays its one-time BSIM charge-up here).
const WARMUP_CYCLES: usize = 3;
/// Unmeasured lead-in tokens prepended to functional runs (rung-1 devices
/// settle internal state during them).
const PAD_TOKENS: usize = 2;

/// Build the k-CSWAP cell deck. `tokens` enter one per period; if
/// `functional` every token's output lines get level measurements plus
/// per-cycle energy integrals, otherwise the settled-cycle energy integrals
/// (`etot`, `e0`, `e2`, `evdd`, `ein`, `eack`, `vmax`) are emitted.
pub fn cswap_netlist(
    k: usize,
    rung: DeviceRung,
    period_s: f64,
    tokens: &[CswapToken],
    functional: bool,
) -> String {
    assert!(k <= 3, "k must be 0, 1, 2, or 3");
    assert!(!tokens.is_empty(), "need at least one token");
    let q = period_s / 4.0;
    let vdd = rung.vdd();
    // Resolve the TG R_on·C corners as well as the period (same policy as
    // the verified SR / rung-1 TG cell).
    let step = match rung {
        DeviceRung::Rung0Level1 => (period_s / 4000.0).min(2e-11),
        DeviceRung::Rung1Sky130 => (period_s / 4000.0).min(5e-11).max(period_s / 40_000.0),
    };
    let sigs = cswap_signals(k);
    let outs = cswap_out_tokens(k, tokens);

    let mut l: Vec<String> = Vec::new();
    l.push(format!(
        "k{k}-CSWAP adiabatic cell, {rung:?} (T={period_s:.6e} s, {} tokens)",
        tokens.len()
    ));
    match rung {
        // Rung 0 needs the SR's artifact suppression (is=1e-18 kills the
        // level-1 default junction leakage; gmin=1e-16 the solver shunts) —
        // model names match `DeviceRung::fet`'s rung-0 cards.
        DeviceRung::Rung0Level1 => {
            l.push(".model NM NMOS (level=1 vto=0.4 kp=200u is=1e-18)".into());
            l.push(".model PM PMOS (level=1 vto=-0.4 kp=100u is=1e-18)".into());
            l.push(".options gmin=1e-16".into());
        }
        // Rung 1: pinned sky130 tt lib + the uniform gmin policy.
        DeviceRung::Rung1Sky130 => l.push(rung.model_header()),
    }
    // VDD is the PMOS bulk bias only — it carries no channel current.
    l.push(format!("Vdd vdd 0 DC {vdd}"));
    // The two clock phases a single stage touches: φ0 charges the rails,
    // φ2 = ~φ0 the mirrors (φ1/φ3 belong to the neighbors).
    l.push(format!(
        "Vp0 p0 0 PULSE(0 {vdd} 0 {q:.9e} {q:.9e} {q:.9e} {period_s:.9e})"
    ));
    l.push(format!(
        "Vp2 p2 0 PULSE({vdd} 0 0 {q:.9e} {q:.9e} {q:.9e} {period_s:.9e})"
    ));

    // Input token lines (offset 3q: rise tick 3⁻, hold tick 0, fall tick 1)
    // and successor replicas of the outputs (offset 5q: rise tick 1, hold
    // tick 2, fall tick 3). All drive transistor gates only.
    let mut eps_i = 0usize;
    let mut src_wires: Vec<(String, Vec<String>)> = Vec::new(); // (group, wires)
    for &sig in sigs {
        let bits: Vec<bool> = tokens.iter().map(|t| t.get(sig)).collect();
        let mut wires = Vec::new();
        for pol in [true, false] {
            let base = line(sig, pol);
            let pulses: Vec<bool> = bits.iter().map(|&v| v == pol).collect();
            let (p, m) = token_line_pwl(&pulses, period_s, 3.0 * q, 0.01 + 0.002 * eps_i as f64, vdd);
            eps_i += 1;
            l.push(format!("V{base}p {base}p 0 PWL({p})"));
            l.push(format!("V{base}m {base}m 0 PWL({m})"));
            wires.push(base);
        }
        src_wires.push((format!("i{sig}"), wires));
    }
    for &sig in sigs {
        let bits: Vec<bool> = tokens.iter().map(|t| t.get(sig)).collect();
        let mut wires = Vec::new();
        for pol in [true, false] {
            let base = format!("r{}", line(sig, pol));
            let pulses: Vec<bool> = bits.iter().map(|&v| v == pol).collect();
            let (p, m) = token_line_pwl(&pulses, period_s, 5.0 * q, 0.01 + 0.002 * eps_i as f64, vdd);
            eps_i += 1;
            l.push(format!("V{base}p {base}p 0 PWL({p})"));
            l.push(format!("V{base}m {base}m 0 PWL({m})"));
            wires.push(base);
        }
        src_wires.push((format!("r{sig}"), wires));
    }

    // A full CMOS TG between nodes `x` and `y`, gated by token line `gate`
    // (NMOS ← rail, PMOS ← mirror; rest-off). W = 4/8 µm, L = [`TG_L_UM`].
    // Rung-1 reality this geometry buys and pays for: unlike the always-on
    // rung-1 TG cell, a logic cell holds full VDS across its OFF gates for
    // half of every cycle, and at L = 0.15 the sky130 subthreshold + DIBL
    // leakage (~nA class) sets a ~7e-10 W static floor that closes the
    // adiabatic window above T ≈ 6e-7 s (measured; see the slope tests).
    let tg = |l: &mut Vec<String>, name: &str, x: &str, gate: &str, y: &str| {
        l.push(rung.fet(&format!("{name}n"), FetKind::Nmos, x, &format!("{gate}p"), y, "0", 4.0, TG_L_UM));
        l.push(rung.fet(&format!("{name}p"), FetKind::Pmos, x, &format!("{gate}m"), y, "vdd", 8.0, TG_L_UM));
    };

    // The TG networks: per output signal, per polarity line, per wire.
    for &sig in sigs {
        for pol in [true, false] {
            let obase = format!("o{}", line(sig, pol));
            let brs = cswap_branches(k, sig, pol);
            for (clk, suffix, park) in [("p0", "p", 0.0), ("p2", "m", vdd)] {
                let wire = format!("{obase}{suffix}");
                for (bi, br) in brs.iter().enumerate() {
                    // Reverse literals: the same literal sequence, gated by
                    // the delayed input copies — so a reverse branch conducts
                    // exactly iff its forward branch conducted, and every
                    // internal node's ride-down mirrors its ride-up. (Gating
                    // the data literal by the OUTPUT-line replica instead
                    // connects the parked internal node of a NON-conducting
                    // branch to the active wire at tick 1 — a measured
                    // ~2.4 fJ/node charge-sharing dump on a≠b tokens.)
                    let rev: Vec<String> =
                        br.iter().map(|&(ls, lp)| format!("r{}", line(ls, lp))).collect();
                    let mut prev = clk.to_string();
                    for (si, &(ls, lp)) in br.iter().enumerate() {
                        let next = if si + 1 == br.len() {
                            wire.clone()
                        } else {
                            let n = format!("{wire}x{bi}{si}");
                            // Internal node: explicit cap so charge-sharing
                            // would be *seen*, parked at the clock rest level.
                            l.push(format!("C{n} {n} 0 5f"));
                            l.push(format!(".ic V({n})={park:.4}"));
                            n
                        };
                        tg(&mut l, &format!("f{wire}b{bi}s{si}"), &prev, &line(ls, lp), &next);
                        tg(&mut l, &format!("r{wire}b{bi}s{si}"), &prev, &rev[si], &next);
                        prev = next;
                    }
                }
                l.push(format!("C{wire} {wire} 0 100f"));
                l.push(format!(".ic V({wire})={park:.4}"));
            }
        }
    }

    // Honest power sum: EVERY source contributes to ptot — clocks, bulk
    // bias, and all gate-drive boundary sources (inputs + replicas,
    // integrated separately as ein/eack so tests verify instead of assume).
    l.push("Bp0 pp0 0 V = -V(p0)*I(Vp0)".into());
    l.push("Bp2 pp2 0 V = -V(p2)*I(Vp2)".into());
    l.push("Bpv ppv 0 V = -V(vdd)*I(Vdd)".into());
    let mut groups = Vec::new();
    for (group, wires) in &src_wires {
        let terms: Vec<String> = wires
            .iter()
            .map(|w| format!("-V({w}p)*I(V{w}p)-V({w}m)*I(V{w}m)"))
            .collect();
        l.push(format!("Bp{group} pw{group} 0 V = {}", terms.join("")));
        groups.push(format!("pw{group}"));
    }
    let ins: Vec<&String> = groups.iter().filter(|g| g.starts_with("pwi")).collect();
    let aks: Vec<&String> = groups.iter().filter(|g| g.starts_with("pwr")).collect();
    let sum = |gs: &[&String]| gs.iter().map(|g| format!("V({g})")).collect::<Vec<_>>().join("+");
    l.push(format!("Bin pint 0 V = {}", sum(&ins)));
    l.push(format!("Bak pakt 0 V = {}", sum(&aks)));
    l.push("Btot ptot 0 V = V(pp0)+V(pp2)+V(ppv)+V(pint)+V(pakt)".into());

    // Control block.
    let stop = if functional {
        (tokens.len() as f64 + 1.5) * period_s
    } else {
        (WARMUP_CYCLES + 1) as f64 * period_s
    };
    l.push(".control".into());
    l.push(format!("tran {step:.9e} {stop:.9e} uic"));
    let (mf, mt) = (
        WARMUP_CYCLES as f64 * period_s,
        (WARMUP_CYCLES + 1) as f64 * period_s,
    );
    if functional {
        // Per-token, per-line full-level measurements in the middle 60% of
        // the output hold window (tick 1 of cycle j+1).
        for (j, out) in outs.iter().enumerate() {
            let w0 = (j + 1) as f64 * period_s + q + 0.2 * q;
            let w1 = (j + 1) as f64 * period_s + 2.0 * q - 0.2 * q;
            for &sig in sigs {
                for pol in [true, false] {
                    let base = format!("o{}", line(sig, pol));
                    let active = out.get(sig) == pol;
                    let (fr, fm) = if active { ("MIN", "MAX") } else { ("MAX", "MIN") };
                    l.push(format!(
                        "meas tran v{j}{base}p {fr} V({base}p) FROM={w0:.9e} TO={w1:.9e}"
                    ));
                    l.push(format!(
                        "meas tran v{j}{base}m {fm} V({base}m) FROM={w0:.9e} TO={w1:.9e}"
                    ));
                }
            }
        }
        // Per-cycle total energy (catches any period-independent tail burn).
        for i in 0..(stop / period_s).floor() as usize {
            let f = i as f64 * period_s;
            l.push(format!(
                "meas tran ecyc{i} INTEG V(ptot) FROM={f:.9e} TO={:.9e}",
                f + period_s
            ));
        }
    } else {
        for (name, node) in [
            ("etot", "ptot"),
            ("e0", "pp0"),
            ("e2", "pp2"),
            ("evdd", "ppv"),
            ("ein", "pint"),
            ("eack", "pakt"),
        ] {
            l.push(format!(
                "meas tran {name} INTEG V({node}) FROM={mf:.9e} TO={mt:.9e}"
            ));
        }
        // Non-vacuity: the a-output 1-line must actually swing.
        l.push(format!(
            "meas tran vmax MAX V(oatp) FROM={mf:.9e} TO={mt:.9e}"
        ));
    }
    l.push(".endc".into());
    l.push(".end".into());
    l.join("\n") + "\n"
}

/// Expected output token per input token (the truth function, per k).
fn cswap_out_tokens(k: usize, tokens: &[CswapToken]) -> Vec<CswapToken> {
    tokens.iter().map(|&t| cswap_out(k, t)).collect()
}

/// The functional (truth-table) deck: two unmeasured lead-in tokens, then
/// every control/data combination back-to-back. Returns the deck and the
/// full token stream (pads included) so callers can decode measurements.
pub fn cswap_functional_netlist(
    k: usize,
    rung: DeviceRung,
    period_s: f64,
) -> (String, Vec<CswapToken>) {
    let truth = cswap_truth_tokens(k);
    let mut tokens = vec![truth[truth.len() - 1]; PAD_TOKENS];
    tokens.extend_from_slice(&truth);
    let deck = cswap_netlist(k, rung, period_s, &tokens, true);
    (deck, tokens)
}

/// One measured output line level for one token of a functional run.
#[derive(Clone, Debug)]
pub struct CswapReading {
    /// Index into the token stream (pads included).
    pub token_idx: usize,
    pub token: CswapToken,
    /// Output line base name (e.g. `oat` = a-output 1-line).
    pub line: String,
    /// Whether this line should pulse for this token.
    pub expected_active: bool,
    /// Worst-case rail level in the hold window (MIN if active, MAX if not).
    pub rail_v: f64,
    /// Worst-case mirror level (MAX if active, MIN if not).
    pub mirror_v: f64,
}

/// A functional run's decoded results: every per-token line reading plus
/// the per-cycle total energies (cycle index, joules).
pub type CswapFunctionalRun = (Vec<CswapReading>, Vec<(usize, f64)>);

/// Run the truth-table deck and decode every per-token line measurement,
/// plus the per-cycle energies.
pub fn run_cswap_functional(
    k: usize,
    rung: DeviceRung,
    period_s: f64,
) -> io::Result<CswapFunctionalRun> {
    let (deck, tokens) = cswap_functional_netlist(k, rung, period_s);
    let out = run_ngspice_raw(&deck)?;
    let get = |name: &str| {
        parse_meas(&out, name)
            .unwrap_or_else(|| panic!("measurement '{name}' missing from ngspice output"))
    };
    let mut readings = Vec::new();
    for (j, &t) in tokens.iter().enumerate() {
        let o = cswap_out(k, t);
        for &sig in cswap_signals(k) {
            for pol in [true, false] {
                let base = format!("o{}", line(sig, pol));
                readings.push(CswapReading {
                    token_idx: j,
                    token: t,
                    line: base.clone(),
                    expected_active: o.get(sig) == pol,
                    rail_v: get(&format!("v{j}{base}p")),
                    mirror_v: get(&format!("v{j}{base}m")),
                });
            }
        }
    }
    let n_cycles = (tokens.len() as f64 + 1.5).floor() as usize;
    let ecyc = (0..n_cycles).map(|i| (i, get(&format!("ecyc{i}")))).collect();
    Ok((readings, ecyc))
}

/// One point of the energy-vs-period sweep, with the honest breakdown.
#[derive(Clone, Debug)]
pub struct CswapEnergyPoint {
    pub period_s: f64,
    /// Total energy from ALL sources over the settled cycle (the per-op figure).
    pub etot: f64,
    /// Clock-rail share (φ0 + φ2): the adiabatic/recoverable path.
    pub epc: f64,
    /// Bulk-bias share (junction/well leakage).
    pub evdd: f64,
    /// Input gate-driver share (measured, not assumed ≈ 0).
    pub ein: f64,
    /// Replica gate-driver share.
    pub eack: f64,
    /// Peak of the a-output 1-line rail (non-vacuity: the cell does work).
    pub vmax: f64,
}

/// The all-active energy token: every control and both data high (the
/// heaviest routing case — at k ≥ 1 the swap path is exercised).
fn energy_token() -> CswapToken {
    CswapToken { c1: true, c2: true, c3: true, a: true, b: true }
}

/// The energy deck: the all-active token repeated so the measured settled
/// cycle has a token in flight; total-source energy over cycle
/// [`WARMUP_CYCLES`], `WARMUP_CYCLES`+1.
pub fn cswap_energy_netlist(k: usize, rung: DeviceRung, period_s: f64) -> String {
    cswap_netlist(k, rung, period_s, &[energy_token(); WARMUP_CYCLES + 3], false)
}

/// Run one energy point.
pub fn cswap_energy_point(k: usize, rung: DeviceRung, period_s: f64) -> io::Result<CswapEnergyPoint> {
    let out = run_ngspice_raw(&cswap_energy_netlist(k, rung, period_s))?;
    let get = |name: &str| {
        parse_meas(&out, name)
            .unwrap_or_else(|| panic!("measurement '{name}' missing from ngspice output"))
    };
    Ok(CswapEnergyPoint {
        period_s,
        etot: get("etot"),
        epc: get("e0") + get("e2"),
        evdd: get("evdd"),
        ein: get("ein"),
        eack: get("eack"),
        vmax: get("vmax"),
    })
}

/// Sweep energy-per-op vs period. Points carry the full source breakdown;
/// the log-log slope of `(period, |etot|)` is the adiabaticity figure.
pub fn cswap_energy_curve(
    k: usize,
    rung: DeviceRung,
    periods: &[f64],
) -> io::Result<Vec<CswapEnergyPoint>> {
    periods
        .iter()
        .map(|&t| cswap_energy_point(k, rung, t))
        .collect()
}

/// Log-log slope of a curve's `(period, |etot|)` points.
pub fn curve_slope(points: &[CswapEnergyPoint]) -> f64 {
    let data: Vec<(f64, f64)> = points.iter().map(|p| (p.period_s, p.etot.abs())).collect();
    log_log_slope(&data)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adiabatic::sky130_available;
    use crate::bench::ngspice_available;

    /// Assert every reading is at full level and print the measured table.
    fn assert_full_levels(k: usize, rung: DeviceRung, readings: &[CswapReading]) {
        let vdd = rung.vdd();
        let mut last_idx = usize::MAX;
        for r in readings {
            if r.token_idx != last_idx {
                last_idx = r.token_idx;
                let t = r.token;
                eprintln!(
                    "k{k} {rung:?} token {}: c1={} c2={} c3={} a={} b={} -> {:?}",
                    r.token_idx, t.c1 as u8, t.c2 as u8, t.c3 as u8, t.a as u8, t.b as u8,
                    cswap_out(k, t)
                );
            }
            eprintln!(
                "    {}: active={} rail={:.4} mirror={:.4}",
                r.line, r.expected_active, r.rail_v, r.mirror_v
            );
            if r.expected_active {
                assert!(
                    r.rail_v > 0.9 * vdd && r.mirror_v < 0.1 * vdd,
                    "k{k} {rung:?} token {} line {}: active line not full-level \
                     (rail min {:.4} V, mirror max {:.4} V, VDD {vdd})",
                    r.token_idx, r.line, r.rail_v, r.mirror_v
                );
            } else {
                assert!(
                    r.rail_v < 0.1 * vdd && r.mirror_v > 0.9 * vdd,
                    "k{k} {rung:?} token {} line {}: inactive line disturbed \
                     (rail max {:.4} V, mirror min {:.4} V, VDD {vdd})",
                    r.token_idx, r.line, r.rail_v, r.mirror_v
                );
            }
        }
    }

    fn truth_table_rung0(k: usize) {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let (readings, ecyc) = run_cswap_functional(k, DeviceRung::Rung0Level1, 1e-6)
            .expect("ngspice failed");
        assert_full_levels(k, DeviceRung::Rung0Level1, &readings);
        // No period-independent tail burn on any settled cycle: at T = 1e-6
        // even a single 5 fF internal-node charge-sharing event is ~2.4e-15 J
        // (the bug the per-literal reverse gating fixed measured 1.05e-14 J
        // on a≠b tokens); settled adiabatic cycles measure ≤ 9.6e-16 J
        // (k2 worst).
        for &(i, e) in ecyc.iter().skip(2) {
            eprintln!("k{k} rung0 cycle {i}: {e:.3e} J");
            assert!(
                e.abs() < 1.5e-15,
                "k{k} rung0 cycle {i} energy {e:.3e} J is not adiabatic \
                 (internal-node charge-sharing or tail burn)"
            );
        }
    }

    #[test]
    fn cswap_k0_truth_table_rung0() {
        truth_table_rung0(0);
    }

    #[test]
    fn cswap_k1_truth_table_rung0() {
        truth_table_rung0(1);
    }

    #[test]
    fn cswap_k2_truth_table_rung0() {
        truth_table_rung0(2);
    }

    #[test]
    fn cswap_k3_truth_table_rung0() {
        truth_table_rung0(3);
    }

    /// Rung-0 adiabatic slope for the Fredkin cell: 3 decades, slope ≈ −1.
    #[test]
    fn cswap_k1_slope_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let periods = [1e-8, 1e-7, 1e-6, 1e-5];
        let curve = cswap_energy_curve(1, DeviceRung::Rung0Level1, &periods).unwrap();
        for p in &curve {
            eprintln!(
                "k1 rung0 T={:.2e}: etot={:.3e} epc={:.3e} evdd={:.3e} ein={:.3e} eack={:.3e} vmax={:.3}",
                p.period_s, p.etot, p.epc, p.evdd, p.ein, p.eack, p.vmax
            );
            assert!(p.etot > 0.0, "energy must be positive: {p:?}");
            assert!(p.vmax > 0.9, "cell must do work (vmax {:.3} V)", p.vmax);
            // Gate-only boundary sources deliver ~0 at rung 0 (no gate caps)
            // — verified, not assumed.
            assert!(p.ein.abs() < 1e-17 && p.eack.abs() < 1e-17, "boundary sources must be ~0: {p:?}");
        }
        let slope = curve_slope(&curve);
        eprintln!("k1 rung0 slope = {slope:.4}");
        assert!(
            (-1.15..=-0.85).contains(&slope),
            "k1 rung0 slope must be ≈ −1, got {slope:.4}"
        );
    }

    /// Rung-0 adiabatic slope for the 3-control CSWAP cell.
    #[test]
    fn cswap_k3_slope_rung0() {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        let periods = [1e-8, 1e-7, 1e-6, 1e-5];
        let curve = cswap_energy_curve(3, DeviceRung::Rung0Level1, &periods).unwrap();
        for p in &curve {
            eprintln!(
                "k3 rung0 T={:.2e}: etot={:.3e} epc={:.3e} evdd={:.3e} ein={:.3e} eack={:.3e} vmax={:.3}",
                p.period_s, p.etot, p.epc, p.evdd, p.ein, p.eack, p.vmax
            );
            assert!(p.etot > 0.0, "energy must be positive: {p:?}");
            assert!(p.vmax > 0.9, "cell must do work (vmax {:.3} V)", p.vmax);
            assert!(p.ein.abs() < 1e-17 && p.eack.abs() < 1e-17, "boundary sources must be ~0: {p:?}");
        }
        let slope = curve_slope(&curve);
        eprintln!("k3 rung0 slope = {slope:.4}");
        assert!(
            (-1.15..=-0.85).contains(&slope),
            "k3 rung0 slope must be ≈ −1, got {slope:.4}"
        );
    }

    // ----- rung 1 (sky130) -----

    fn truth_table_sky130(k: usize) {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        if !sky130_available() {
            eprintln!("[skip] sky130 PDK unavailable");
            return;
        }
        let (readings, _ecyc) = run_cswap_functional(k, DeviceRung::Rung1Sky130, 1e-7)
            .expect("ngspice failed");
        assert_full_levels(k, DeviceRung::Rung1Sky130, &readings);
    }

    #[test]
    fn cswap_k0_truth_table_sky130() {
        truth_table_sky130(0);
    }

    #[test]
    fn cswap_k1_truth_table_sky130() {
        truth_table_sky130(1);
    }

    #[test]
    fn cswap_k2_truth_table_sky130() {
        truth_table_sky130(2);
    }

    #[test]
    fn cswap_k3_truth_table_sky130() {
        truth_table_sky130(3);
    }

    /// Rung-1 slope over the adiabatic regime (2.5 decades). Real silicon:
    /// expect ≈ −1 with a leakage-set shallowing at the slow end; the bound
    /// still fails on any qualitative regression (a stranded-charge dump or
    /// crowbar path flattens the curve far above it).
    fn slope_sky130(k: usize, periods: [f64; 3], lo_bound: f64, hi_bound: f64) {
        if !ngspice_available() {
            eprintln!("[skip] ngspice unavailable");
            return;
        }
        if !sky130_available() {
            eprintln!("[skip] sky130 PDK unavailable");
            return;
        }
        let curve = cswap_energy_curve(k, DeviceRung::Rung1Sky130, &periods).unwrap();
        let vdd = DeviceRung::Rung1Sky130.vdd();
        for p in &curve {
            eprintln!(
                "k{k} sky130 T={:.2e}: etot={:.3e} epc={:.3e} evdd={:.3e} ein={:.3e} eack={:.3e} vmax={:.3}",
                p.period_s, p.etot, p.epc, p.evdd, p.ein, p.eack, p.vmax
            );
            assert!(p.etot > 0.0, "energy must be positive: {p:?}");
            assert!(
                p.vmax > 0.9 * vdd,
                "cell must do work at rung 1 (vmax {:.3} V)",
                p.vmax
            );
        }
        for w in curve.windows(2) {
            assert!(
                w[1].etot < w[0].etot,
                "rung1 energy must fall with period: {:.3e} -> {:.3e}",
                w[0].etot,
                w[1].etot
            );
        }
        let slope = curve_slope(&curve);
        eprintln!("k{k} sky130 slope = {slope:.4}");
        assert!(
            (lo_bound..=hi_bound).contains(&slope),
            "k{k} sky130 slope must be in [{lo_bound}, {hi_bound}], got {slope:.4}"
        );
    }

    // Per-cell adiabatic windows, each 2 decades (measured):
    // * below the fast end the ramp approaches the branch R_on·C_load knee
    //   (k2's 3-series-TG path pushes its knee ~⅓ decade higher: at
    //   T = 3.16e-9 its output only reaches 1.62 V = 0.90·VDD);
    // * above the slow end the OFF-gate subthreshold floor (~7e-10 W for
    //   k0 at L = 0.15) takes over — the k0 curve bottoms at T ≈ 1e-6
    //   (1.09e-15 J) and RISES again by 3.16e-6 (2.13e-15 J), the textbook
    //   leakage knee. Measured slopes: k0 −0.93, k1 −0.88, k2 −0.87.

    #[test]
    fn cswap_k0_slope_sky130() {
        slope_sky130(0, [3.16e-9, 3.16e-8, 3.16e-7], -1.15, -0.7);
    }

    #[test]
    fn cswap_k1_slope_sky130() {
        slope_sky130(1, [3.16e-9, 3.16e-8, 3.16e-7], -1.15, -0.7);
    }

    #[test]
    fn cswap_k2_slope_sky130() {
        slope_sky130(2, [1e-8, 1e-7, 1e-6], -1.15, -0.7);
    }

    // k = 3: 4 series TGs in the swap path push the R_on·C knee ~½ decade
    // higher than k = 2; shift the window accordingly.

    #[test]
    fn cswap_k3_slope_sky130() {
        slope_sky130(3, [3.16e-8, 3.16e-7, 3.16e-6], -1.15, -0.7);
    }
}
