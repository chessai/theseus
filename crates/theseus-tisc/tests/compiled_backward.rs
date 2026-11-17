//! A program **compiled from `.thse` source** runs backwards on the CPU.
//!
//! Every test here goes source → parse → check → elaborate → `compile` → ROM,
//! runs the ROM forwards on the simulated CPU for its whole step budget `B`,
//! and then runs the *adjoint machine* `sym(step(rom))` for `B` steps starting
//! from the final machine state alone. The backward run must retrace the
//! forward run state for state and land exactly on the initial machine state.
//!
//! "The CPU" here is the machine-cycle iso `step(rom) : State <-> State`
//! interpreted by `theseus_eval` (the same authority as `run_on_cpu`); reverse
//! execution is that iso's adjoint (ADR-0034 D2), not a second mechanism.
//!
//! Earlier backward runs (`tests/fib_cpu.rs`, `tests/feistel.rs`) used
//! hand-assembled ROMs. These use the compiler's output.

use theseus_check::elaborate::elaborate_program;
use theseus_check::env::resolve;
use theseus_core::prim::enumerate_values;
use theseus_core::{type_of, Iso, Value};
use theseus_tisc::asm::Instr;
use theseus_tisc::compile::{
    marshal, run_on_cpu_round_trip, run_rom_backward, unmarshal, CpuMachine, CpuRoundTrip,
};

const FIB8_SRC: &str = include_str!("../../../conformance/programs/0027-fib8.thse");
const FIB_SRC: &str = include_str!("../../../conformance/programs/0022-fib.thse");
const LFSR_SRC: &str = include_str!("../../../conformance/programs/0026-lfsr.thse");

/// Parse, check (must be clean), resolve, elaborate; return the named iso.
fn elaborate(src: &str, iso: &str) -> Iso {
    let prog = theseus_check::parse(src).expect("parse");
    let diags = theseus_check::check(&prog);
    assert!(diags.is_empty(), "check diagnostics: {diags:?}");
    let env = resolve(&prog);
    let map = elaborate_program(&prog, &env);
    map.get(iso)
        .unwrap_or_else(|| panic!("iso {iso} not in elaboration map"))
        .clone()
        .unwrap_or_else(|e| panic!("elaborate {iso}: {e:?}"))
}

/// A `W8` leaf value `k` (thermometer `Right^k(Left(Unit))`).
fn w8(k: u8) -> Value {
    let mut v = Value::left(Value::Unit);
    for _ in 0..k {
        v = Value::right(v);
    }
    v
}

fn w8_pair(a: u8, b: u8) -> Value {
    Value::pair(w8(a), w8(b))
}

/// Compile `iso`, run `input` forwards then backwards on the CPU, and check
/// everything that reversibility promises:
///
/// * the forward output equals the interpreter's `eval(iso, input)`;
/// * the backward run retraces the forward run: `backward[t] == forward[B-t]`
///   on the *entire* machine state (registers, PC, BR, IR, scratch);
/// * so `backward[B]` is the initial state and the input is recovered.
fn assert_round_trip(iso: &Iso, input: &Value) -> CpuRoundTrip {
    let rt = run_on_cpu_round_trip(iso, input, theseus_eval::eval)
        .unwrap_or_else(|e| panic!("round trip failed on {input:?}: {e}"));
    let b = rt.budget;
    assert_eq!(rt.forward.len(), b + 1);
    assert_eq!(rt.backward.len(), b + 1);

    let want = theseus_eval::eval(iso, input.clone(), &mut 1_000_000_000u64).expect("eval");
    assert_eq!(rt.output, want, "forward CPU output != eval on {input:?}");

    for t in 0..=b {
        assert_eq!(
            rt.backward[t],
            rt.forward[b - t],
            "backward step {t} does not retrace forward step {} (input {input:?})",
            b - t
        );
    }
    assert_eq!(&rt.recovered_input, input, "input not recovered");

    // The machine is clean at both ends: the fetch/unfetch pair leaves IR
    // zero between steps, and the run starts at PC = 0.
    assert_eq!(rt.forward[0].pc, 0);
    assert_eq!(rt.backward[b].pc, 0);
    for s in rt.forward.iter().chain(&rt.backward) {
        assert_eq!(s.ir, 0, "IR must be zero between steps");
    }
    rt
}

/// Byte-wide Fibonacci (`conformance/programs/0027-fib8.thse`): the compiled
/// ROM is `EXCH ; ADD` three times. Forwards it advances the pair three
/// Fibonacci steps; backwards it un-advances them.
#[test]
fn fib8_compiled_runs_forwards_then_backwards() {
    let iso = elaborate(FIB8_SRC, "fib8");
    // The fixture row: (13, 21) |-> (55, 89).
    let rt = assert_round_trip(&iso, &w8_pair(13, 21));
    assert_eq!(rt.output, w8_pair(55, 89));

    // The ROM really is the compiler's `EXCH ; ADD` datapath, and a
    // straight-line program takes exactly one step per instruction.
    assert_eq!(rt.instrs.len(), 6);
    for pair in rt.instrs.chunks(2) {
        assert!(matches!(pair[0], Instr::Exch { .. }), "{:?}", rt.instrs);
        assert!(matches!(pair[1], Instr::Add { .. }), "{:?}", rt.instrs);
    }
    assert_eq!(rt.budget, 6);
    assert!(
        matches!(rt.machine, CpuMachine::Cpu(_)),
        "fib8 needs no memory"
    );

    // Forwards the PC climbs the ROM; backwards it descends it.
    let fwd_pc: Vec<u16> = rt.forward.iter().map(|s| s.pc).collect();
    let bwd_pc: Vec<u16> = rt.backward.iter().map(|s| s.pc).collect();
    assert_eq!(fwd_pc, [0, 1, 2, 3, 4, 5, 6]);
    assert_eq!(bwd_pc, [6, 5, 4, 3, 2, 1, 0]);
}

/// More `fib8` inputs: the all-zero and all-ones corners, and (200, 100),
/// which wraps a carry on every add.
#[test]
#[ignore = "slow: each round trip interprets the full step iso 2B times; run with --ignored"]
fn fib8_compiled_runs_backwards_on_corner_inputs() {
    let iso = elaborate(FIB8_SRC, "fib8");
    for (a, b) in [(0u8, 0u8), (1, 1), (255, 255), (200, 100), (89, 144)] {
        assert_round_trip(&iso, &w8_pair(a, b));
    }
}

/// Un-computing Fibonacci from the **answer alone**. A straight-line program
/// ends at a fixed PC with nothing left behind, so its final machine state is
/// determined by its output. Build that state from `(55, 89)` -- no forward
/// run -- and drive the adjoint machine: it must produce `(13, 21)`.
#[test]
fn fib8_backwards_from_the_output_alone() {
    let iso = elaborate(FIB8_SRC, "fib8");
    let (in_ty, out_ty) = type_of(&iso).expect("typed");

    // Reference forward run, used only to learn the ROM/layout/budget and to
    // check that the final state really carries nothing but the output.
    let rt = assert_round_trip(&iso, &w8_pair(13, 21));

    let mut end = marshal(&w8_pair(55, 89), &out_ty);
    end.pc = rt.budget as u16;
    assert_eq!(
        end, rt.forward[rt.budget],
        "final state must be exactly (output registers, PC = B): no residue"
    );

    let back = run_rom_backward(&rt.machine, &rt.instrs, &end, rt.budget, theseus_eval::eval)
        .expect("backward run");
    let start = back.last().expect("states");
    assert_eq!(start.pc, 0);
    assert_eq!(unmarshal(start, &in_ty), w8_pair(13, 21));
    assert_eq!(start, &marshal(&w8_pair(13, 21), &in_ty));
}

/// Data-dependent control flow (`conformance/programs/0022-fib.thse`): the
/// loop count is a runtime value, compiled to reversible skip-blocks
/// (`SWAPBR`). Different inputs take different numbers of productive steps
/// and then park; the backward run un-parks and un-branches. Whole input
/// domain.
#[test]
#[ignore = "slow: 16 inputs x 2B interpreted steps of the full step iso; run with --ignored"]
fn data_dependent_fib_runs_backwards_on_every_input() {
    let iso = elaborate(FIB_SRC, "fib");
    let (in_ty, _) = type_of(&iso).expect("typed");
    let inputs = enumerate_values(&in_ty);
    assert_eq!(inputs.len(), 16);
    let mut saw_branch = false;
    for input in &inputs {
        let rt = assert_round_trip(&iso, input);
        assert!(
            rt.instrs.iter().any(|i| matches!(i, Instr::Swapbr { .. })),
            "the compiled loop must contain reversible branches"
        );
        // A taken branch shows up as a PC jump of more than one.
        saw_branch |= rt.forward.windows(2).any(|w| w[1].pc > w[0].pc + 1);
    }
    assert!(saw_branch, "no input exercised a taken branch");
}

/// A longer loop that uses **memory** (`conformance/programs/0026-lfsr.thse`, a
/// width-4 LFSR iterated a data-dependent number of times): a 100+ instruction
/// ROM with ten reversible branches whose state spills into memory words, so
/// it runs on the composite CPU + memory machine. Forwards then backwards, on
/// the last input of the domain (the highest iteration count).
///
/// Very slow: interpreting the composite machine costs about 13 s a step, so
/// this is roughly 45 minutes and 22 GB in a release build. (Measured as a
/// 93-minute run over both the first and the last input, which passed.)
#[test]
#[ignore = "very slow (~45 min, ~22 GB in release): composite CPU+memory machine; run with --ignored"]
fn lfsr_compiled_runs_forwards_then_backwards() {
    let iso = elaborate(LFSR_SRC, "lfsr");
    let (in_ty, _) = type_of(&iso).expect("typed");
    let inputs = enumerate_values(&in_ty);
    let input = inputs.last().expect("non-empty domain");
    let rt = assert_round_trip(&iso, input);
    assert_eq!(
        rt.machine,
        CpuMachine::System,
        "LFSR must exercise the memory machine"
    );
    assert!(
        rt.budget > 100,
        "expected a 100+ step budget, got {}",
        rt.budget
    );
}
