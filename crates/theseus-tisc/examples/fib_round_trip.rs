//! Byte-wide Fibonacci, compiled from source, run forwards and then
//! backwards on the simulated reversible CPU.
//!
//! ```sh
//! cargo run --release -p theseus-tisc --example fib_round_trip            # (13, 21)
//! cargo run --release -p theseus-tisc --example fib_round_trip -- 1 1
//! cargo run --release -p theseus-tisc --example fib_round_trip -- 13 21 --json
//! ```
//!
//! Pipeline: `conformance/programs/0027-fib8.thse` → parse → check →
//! elaborate → `compile` (a ROM of `EXCH ; ADD` pairs) → `step(rom)` iterated
//! for the step budget `B` → `sym(step(rom))` iterated for `B` steps from the
//! final machine state. The backward half uses nothing but that final state.
//!
//! `--json` prints the two state traces as JSON (for plotting/animation).

use theseus_check::elaborate::elaborate_program;
use theseus_check::env::resolve;
use theseus_core::Value;
use theseus_tisc::compile::run_on_cpu_round_trip;
use theseus_tisc::types::StateVals;

const FIB8_SRC: &str = include_str!("../../../conformance/programs/0027-fib8.thse");

/// A `W8` leaf value `k` (thermometer `Right^k(Left(Unit))`).
fn w8(k: u8) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

fn row(t: usize, s: &StateVals) -> String {
    format!(
        "{t:>3}  {:>3}  {:>3}  {:>3}  {:>3}  {:#010x}",
        s.pc, s.regs[0], s.regs[1], s.br, s.ir
    )
}

fn json_states(states: &[StateVals]) -> String {
    let rows: Vec<String> = states
        .iter()
        .map(|s| {
            format!(
                "{{\"pc\":{},\"r0\":{},\"r1\":{},\"br\":{},\"ir\":{}}}",
                s.pc, s.regs[0], s.regs[1], s.br, s.ir
            )
        })
        .collect();
    format!("[{}]", rows.join(","))
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let json = args.iter().any(|a| a == "--json");
    let nums: Vec<u8> = args
        .iter()
        .filter(|a| !a.starts_with("--"))
        .map(|a| a.parse().expect("inputs are bytes (0..=255)"))
        .collect();
    let (a, b) = match nums[..] {
        [] => (13, 21),
        [a, b] => (a, b),
        _ => panic!("usage: fib_round_trip [A B] [--json]"),
    };

    let prog = theseus_syntax::parse(FIB8_SRC).expect("parse");
    let diags = theseus_check::check(&prog);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = resolve(&prog);
    let iso = elaborate_program(&prog, &env)
        .remove("fib8")
        .expect("iso fib8")
        .expect("fib8 elaborates");

    let input = Value::pair(w8(a), w8(b));
    let rt = run_on_cpu_round_trip(&iso, &input, theseus_eval::eval).expect("round trip");
    let end = &rt.forward[rt.budget];
    let start = &rt.backward[rt.budget];

    // The whole point: the backward run retraces the forward run exactly.
    for t in 0..=rt.budget {
        assert_eq!(rt.backward[t], rt.forward[rt.budget - t], "step {t}");
    }
    assert_eq!(rt.recovered_input, input);

    if json {
        let rom: Vec<String> = rt.instrs.iter().map(|i| format!("\"{i:?}\"")).collect();
        println!(
            "{{\"input\":[{a},{b}],\"output\":[{},{}],\"budget\":{},\"rom\":[{}],\"forward\":{},\"backward\":{}}}",
            end.regs[0],
            end.regs[1],
            rt.budget,
            rom.join(","),
            json_states(&rt.forward),
            json_states(&rt.backward)
        );
        return;
    }

    println!("fib8 compiled from conformance/programs/0027-fib8.thse");
    println!(
        "layout: {} data bits, {} registers, {}-bit PC; step budget B = {}",
        rt.layout.w_data, rt.layout.num_regs, rt.layout.w_pc, rt.budget
    );
    println!("\nROM");
    for (pc, i) in rt.instrs.iter().enumerate() {
        println!("{pc:>3}  {i:?}");
    }
    let header = "  t   PC   r0   r1   BR  IR";
    println!("\nforward: step(rom), {} steps", rt.budget);
    println!("{header}");
    for (t, s) in rt.forward.iter().enumerate() {
        println!("{}", row(t, s));
    }
    println!(
        "\nbackward: sym(step(rom)), {} steps from the final state",
        rt.budget
    );
    println!("{header}");
    for (t, s) in rt.backward.iter().enumerate() {
        println!("{}", row(t, s));
    }
    println!(
        "\n({a}, {b}) -> ({}, {}) -> ({}, {})",
        end.regs[0], end.regs[1], start.regs[0], start.regs[1]
    );
}
