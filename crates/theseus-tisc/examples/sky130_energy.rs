//! Whole-program energy on real sky130 device models.
//!
//! ```sh
//! cargo run --release -p theseus-tisc --example sky130_energy            # tables
//! cargo run --release -p theseus-tisc --example sky130_energy -- --json  # data
//! ```
//!
//! Prices the compiled TISC step and two compiled programs from the committed
//! sky130 characterization database (`theseus_spice::chardb::sky130_measured`):
//! our own dual-rail cells, built from sky130 primitive transistors and swept
//! in ngspice. No simulator is needed to run this -- the database is committed.
//!
//! Every reversible number is `sum over cell kinds of count x E_cell(T)`, with
//! the K0 tick-alignment buffers counted analytically
//! (`theseus_tisc::energy::balanced_energy`). The irreversible reference is the
//! database's measured `cmos-static` gate (same transistors, same 100 fF wire
//! load), charged once per active reversible cell per productive step.
//!
//! The same tables are printed for the rung-0 ideal-device database, where
//! there is no leakage and energy keeps falling as `1/T`. The difference
//! between the two is the leakage floor.

use std::collections::BTreeMap;
use std::error::Error;

use theseus_core::{Iso, Value};
use theseus_netlist::{lower_iso, PhasedCellNetlist};
use theseus_spice::chardb::{rung0_measured, sky130_measured, CharDb};
use theseus_tisc::compile::{compile_with_metadata, derive_full_layout_from_lowered};
use theseus_tisc::energy::{balanced_energy, rom_program_energy};
use theseus_tisc::money_plot::{cmos_energy_per_gate_j, cpu_money_plot};
use theseus_tisc::step::{step, step_with};
use theseus_tisc::types::Layout;

const ADDER_SRC: &str = include_str!("../../../conformance/programs/0024-adder.thse");

/// Byte-wide Fibonacci steps in the `fib8` benchmark.
const FIB8_STEPS: usize = 10;

/// The periods the sky130 database was measured at (three per decade).
fn periods() -> Vec<f64> {
    (0..=9).map(|i| 10f64.powf(-8.0 + i as f64 / 3.0)).collect()
}

/// Thermometer value `Right^k(Left(Unit))` (a `W8` byte, or a unary numeral).
fn therm(k: u32) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

fn fib8_source(n_steps: usize) -> String {
    let steps = vec!["fibStep"; n_steps].join(" ; ");
    format!(
        "prim add_into : W8 * W8 <-> W8 * W8 = \"theseus.arith.add_into\"\n\
         iso swap2 : W8 * W8 <-> W8 * W8\n\
         | (a, b) <-> (b, a)\n\
         iso fibStep : W8 * W8 <-> W8 * W8 = swap2 ; add_into\n\
         iso fib8 : W8 * W8 <-> W8 * W8 = {steps}\n"
    )
}

fn elaborate(src: &str, name: &str) -> Result<Iso, Box<dyn Error>> {
    let program = theseus_syntax::parse(src).map_err(|e| format!("parse: {e:?}"))?;
    let diags = theseus_check::check(&program);
    if !diags.is_empty() {
        return Err(format!("check: {diags:?}").into());
    }
    let env = theseus_check::env::resolve(&program);
    theseus_check::elaborate::elaborate_program(&program, &env)
        .remove(name)
        .ok_or_else(|| format!("iso {name:?} not found"))?
        .map_err(|e| format!("elaborate {name:?}: {e:?}").into())
}

/// Cell inventory of a step netlist: count per characterization key, with the
/// analytic K0 balance buffers under `cswap-k0-balance`.
fn inventory(
    net: &PhasedCellNetlist,
    db: &CharDb,
) -> Result<BTreeMap<String, usize>, Box<dyn Error>> {
    let t = periods()[0];
    let bd = balanced_energy(net, db, t).map_err(|e| format!("{e:?}"))?;
    Ok(bd
        .by_kind
        .iter()
        .map(|(k, v)| (k.clone(), v.count))
        .collect())
}

/// One priced row: energies at a single period.
struct Row {
    t: f64,
    reversible_j: f64,
    active_j: f64,
    cmos_j: f64,
}

/// A step netlist priced across the period axis.
struct StepTable {
    name: String,
    inventory: BTreeMap<String, usize>,
    rows: Vec<Row>,
}

fn price_step(
    name: &str,
    net: &PhasedCellNetlist,
    db: &CharDb,
) -> Result<StepTable, Box<dyn Error>> {
    let mut rows = Vec::new();
    for t in periods() {
        let plot = cpu_money_plot(net, db, &[t]).map_err(|e| format!("{e:?}"))?;
        rows.push(Row {
            t,
            reversible_j: plot.full_step[0].energy_j,
            active_j: plot.active_logic[0].energy_j,
            cmos_j: plot.cmos_ref_j,
        });
    }
    Ok(StepTable {
        name: name.to_string(),
        inventory: inventory(net, db)?,
        rows,
    })
}

/// A compiled program priced across the period axis at its own layout.
struct ProgramTable {
    name: String,
    steps_b: usize,
    steps_t: usize,
    layout: String,
    inventory: BTreeMap<String, usize>,
    rows: Vec<Row>,
}

/// A compiled program's step netlist, lowered once at the program's own
/// auto-derived layout (the expensive part), to be priced against any database.
struct ProgramNet {
    name: &'static str,
    iso: Iso,
    input: Value,
    layout: Layout,
    net: PhasedCellNetlist,
}

fn lower_program(name: &'static str, iso: Iso, input: Value) -> Result<ProgramNet, Box<dyn Error>> {
    eprintln!("[sky130_energy] lowering the step for '{name}'...");
    let lowered = compile_with_metadata(&iso).map_err(|e| format!("compile: {e:?}"))?;
    let rom = theseus_tisc::asm::assemble(&lowered.instrs).map_err(|e| format!("{e}"))?;
    let layout = derive_full_layout_from_lowered(&iso, &lowered);
    let net = lower_iso(&step_with(&layout, &rom)).map_err(|e| format!("lower: {e:?}"))?;
    Ok(ProgramNet {
        name,
        iso,
        input,
        layout,
        net,
    })
}

fn price_program(p: &ProgramNet, db: &CharDb) -> Result<ProgramTable, Box<dyn Error>> {
    let (name, iso, input, layout, net) = (p.name, &p.iso, &p.input, &p.layout, &p.net);
    let mut rows = Vec::new();
    let (mut steps_b, mut steps_t) = (0, 0);
    for t in periods() {
        let pe = rom_program_energy(iso, input, db, t, net).map_err(|e| format!("{e:?}"))?;
        let plot = cpu_money_plot(net, db, &[t]).map_err(|e| format!("{e:?}"))?;
        steps_b = pe.steps_b;
        steps_t = pe.steps_t;
        rows.push(Row {
            t,
            reversible_j: pe.total_j,
            active_j: pe.steps_b as f64 * plot.active_logic[0].energy_j,
            // Charge the irreversible machine only for productive steps.
            cmos_j: pe.steps_t as f64 * plot.cmos_ref_j,
        });
    }
    Ok(ProgramTable {
        name: name.to_string(),
        steps_b,
        steps_t,
        layout: format!(
            "w_data={} num_regs={} w_pc={}",
            layout.w_data, layout.num_regs, layout.w_pc
        ),
        inventory: inventory(net, db)?,
        rows,
    })
}

fn print_rows(rows: &[Row]) {
    println!(
        "   period (s)   reversible (J)   active logic (J)   CMOS-equiv (J)   CMOS/reversible"
    );
    let best = rows
        .iter()
        .min_by(|a, b| a.reversible_j.total_cmp(&b.reversible_j))
        .map(|r| r.t);
    for r in rows {
        let mark = if Some(r.t) == best {
            "  <- minimum"
        } else {
            ""
        };
        println!(
            "   {:>9.3e}   {:>14.4e}   {:>16.4e}   {:>14.4e}   {:>12.3}x{mark}",
            r.t,
            r.reversible_j,
            r.active_j,
            r.cmos_j,
            r.cmos_j / r.reversible_j
        );
    }
}

fn print_inventory(inv: &BTreeMap<String, usize>) {
    let total: usize = inv.values().sum();
    let parts: Vec<String> = inv.iter().map(|(k, n)| format!("{k}: {n}")).collect();
    println!("   cells: {} ({total} total)", parts.join(", "));
}

fn json_rows(rows: &[Row]) -> String {
    let parts: Vec<String> = rows
        .iter()
        .map(|r| {
            format!(
                "{{\"t\":{:e},\"reversible_j\":{:e},\"active_j\":{:e},\"cmos_j\":{:e}}}",
                r.t, r.reversible_j, r.active_j, r.cmos_j
            )
        })
        .collect();
    format!("[{}]", parts.join(","))
}

fn json_inventory(inv: &BTreeMap<String, usize>) -> String {
    let parts: Vec<String> = inv.iter().map(|(k, n)| format!("\"{k}\":{n}")).collect();
    format!("{{{}}}", parts.join(","))
}

fn json_cells(db: &CharDb) -> String {
    let parts: Vec<String> = db
        .cells
        .iter()
        .map(|(k, c)| {
            let pts: Vec<String> = c
                .points
                .iter()
                .map(|p| format!("[{:e},{:e}]", p.t_ramp, p.energy))
                .collect();
            format!("\"{k}\":[{}]", pts.join(","))
        })
        .collect();
    format!("{{{}}}", parts.join(","))
}

fn main() -> Result<(), Box<dyn Error>> {
    let json = std::env::args().any(|a| a == "--json");

    // The two-word reference ROM of the CPU money plot: `decode_execute`
    // dominates the step, so the program barely moves the per-step energy.
    let reference_rom: Vec<u32> = vec![(0b011u32 << 25) | 1, (0b101u32 << 25) | (1 << 20) | 0xFF];
    eprintln!("[sky130_energy] lowering the default-layout step...");
    let reference_net = lower_iso(&step(&reference_rom)).map_err(|e| format!("{e:?}"))?;

    let fib8 = elaborate(&fib8_source(FIB8_STEPS), "fib8")?;
    let fib8_input = Value::pair(therm(0), therm(1));
    let adder = elaborate(ADDER_SRC, "ha4")?;
    let bit = |b: bool| {
        if b {
            Value::right(Value::Unit)
        } else {
            Value::left(Value::Unit)
        }
    };
    let adder_input = Value::pair(
        bit(true),
        Value::pair(bit(true), Value::pair(bit(false), bit(false))),
    );

    let program_nets = [
        lower_program("fib8 x10", fib8, fib8_input)?,
        lower_program("half-adder", adder, adder_input)?,
    ];

    let mut json_dbs = Vec::new();
    for (db_name, db) in [
        ("sky130", sky130_measured()),
        ("rung0-ideal", rung0_measured()),
    ] {
        let step_table = price_step("TISC step, default layout", &reference_net, &db)?;
        let programs = [
            price_program(&program_nets[0], &db)?,
            price_program(&program_nets[1], &db)?,
        ];

        if json {
            let progs: Vec<String> = programs
                .iter()
                .map(|p| {
                    format!(
                        "{{\"name\":\"{}\",\"steps_b\":{},\"steps_t\":{},\"layout\":\"{}\",\"inventory\":{},\"rows\":{}}}",
                        p.name,
                        p.steps_b,
                        p.steps_t,
                        p.layout,
                        json_inventory(&p.inventory),
                        json_rows(&p.rows)
                    )
                })
                .collect();
            json_dbs.push(format!(
                "\"{db_name}\":{{\"cmos_per_gate_j\":{:e},\"cells\":{},\"step\":{{\"inventory\":{},\"rows\":{}}},\"programs\":[{}]}}",
                cmos_energy_per_gate_j(&db, periods()[0]),
                json_cells(&db),
                json_inventory(&step_table.inventory),
                json_rows(&step_table.rows),
                progs.join(",")
            ));
            continue;
        }

        println!("================ {db_name} ================");
        println!(
            "static-CMOS reference: {:.4e} J per gate per cycle",
            cmos_energy_per_gate_j(&db, periods()[0])
        );
        println!("\n{} (energy per step)", step_table.name);
        print_inventory(&step_table.inventory);
        print_rows(&step_table.rows);
        for p in &programs {
            println!(
                "\n{} (whole program: B = {} steps, {} productive; layout {})",
                p.name, p.steps_b, p.steps_t, p.layout
            );
            print_inventory(&p.inventory);
            print_rows(&p.rows);
        }
        println!();
    }
    if json {
        println!("{{{}}}", json_dbs.join(","));
    }
    Ok(())
}
