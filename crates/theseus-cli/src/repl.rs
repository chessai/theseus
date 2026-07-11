//! REPL with `:step` / `:unstep` (bidirectional stepping).
//!
//! The REPL reads commands from a `BufRead` source and writes output to a
//! `Write` sink, making it both interactive and scriptable/testable.

use std::io::{BufRead, Write};

use theseus_core::iso::Iso;
use theseus_core::prim::Value;
use theseus_eval::interp::{eval, EvalError};

use crate::pipeline::ElaboratedProgram;
use crate::value;

/// Fuel budget per `:step` evaluation.
const STEP_FUEL: u64 = 1_000_000;

/// REPL state: a loaded program, an optional active iso with history stack.
pub struct Repl {
    ep: ElaboratedProgram,
    /// The currently loaded iso (name, core term, lhs type, rhs type).
    active: Option<ActiveIso>,
}

struct ActiveIso {
    name: String,
    iso: Iso,
    /// Surface lhs type (iso domain — used for rendering input).
    lhs_ty: theseus_syntax::ast::Type,
    /// Surface rhs type (iso codomain — used for rendering forward-step results).
    rhs_ty: theseus_syntax::ast::Type,
    /// History stack: history[0] is the initial value; history.last() is the
    /// current value. `:step` pushes; `:unstep` pops.
    history: Vec<Value>,
}

impl ActiveIso {
    fn current(&self) -> &Value {
        self.history.last().expect("history is never empty")
    }
}

impl Repl {
    /// Create a new REPL from an elaborated program.
    pub fn new(ep: ElaboratedProgram) -> Self {
        Self { ep, active: None }
    }

    /// Run the REPL loop, reading commands from `input` and writing to `output`.
    pub fn run(&mut self, input: &mut dyn BufRead, output: &mut dyn Write) {
        let mut line = String::new();
        loop {
            line.clear();
            match input.read_line(&mut line) {
                Ok(0) => break, // EOF
                Ok(_) => {}
                Err(e) => {
                    let _ = writeln!(output, "error: {e}");
                    break;
                }
            }
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            match self.dispatch(trimmed, output) {
                ControlFlow::Continue => {}
                ControlFlow::Quit => break,
            }
        }
    }

    fn dispatch(&mut self, line: &str, out: &mut dyn Write) -> ControlFlow {
        if let Some(rest) = line.strip_prefix(":load") {
            self.cmd_load(rest.trim(), out);
            ControlFlow::Continue
        } else if line == ":step" {
            self.cmd_step(out);
            ControlFlow::Continue
        } else if line == ":unstep" {
            self.cmd_unstep(out);
            ControlFlow::Continue
        } else if line == ":quit" {
            ControlFlow::Quit
        } else {
            let _ = writeln!(out, "unknown command: {line}");
            ControlFlow::Continue
        }
    }

    fn cmd_load(&mut self, args: &str, out: &mut dyn Write) {
        // Parse: <iso_name> <value...>
        let (iso_name, val_str) = match args.split_once(char::is_whitespace) {
            Some((name, rest)) => (name.trim(), rest.trim()),
            None => {
                let _ = writeln!(out, "usage: :load <iso> <value>");
                return;
            }
        };

        // Look up iso in elaboration map.
        let iso = match self.ep.isos.get(iso_name) {
            Some(Ok(iso)) => iso.clone(),
            Some(Err(_)) => {
                let _ = writeln!(out, "error: iso `{iso_name}` could not be elaborated");
                return;
            }
            None => {
                let _ = writeln!(out, "error: unknown iso `{iso_name}`");
                return;
            }
        };

        // Look up signature for type-guided value parsing.
        let sig = match self.ep.env.iso(iso_name) {
            Some(sig) => sig.clone(),
            None => {
                let _ = writeln!(out, "error: iso `{iso_name}` not in environment");
                return;
            }
        };

        // Parse the surface value and convert to core.
        let sv = value::parse_surface_value(val_str);
        let core_val = value::to_core(&sv, &sig.lhs, &self.ep.env);

        let rendered = value::render(&core_val, &sig.lhs, &self.ep.env);
        let _ = writeln!(out, "loaded: {rendered}");

        self.active = Some(ActiveIso {
            name: iso_name.to_string(),
            iso,
            lhs_ty: sig.lhs,
            rhs_ty: sig.rhs,
            history: vec![core_val],
        });
    }

    fn cmd_step(&mut self, out: &mut dyn Write) {
        let active = match &mut self.active {
            Some(a) => a,
            None => {
                let _ = writeln!(out, "error: no iso loaded; use :load first");
                return;
            }
        };

        let current = active.current().clone();
        let mut fuel = STEP_FUEL;

        // For Iso::Trace, step ONE body application at a time instead of
        // evaluating the entire loop.
        if let Iso::Trace(_u, body) = &active.iso {
            // Guard: if the loop already exited (current is Right and we are
            // past the initial entry), refuse to step further.
            if active.history.len() > 1 && matches!(current, Value::Right(_)) {
                let _ = writeln!(out, "loop complete");
                return;
            }

            // If we are at the initial (plain) value (history has only the
            // loaded value), wrap it as Right(input) — the a-side boundary
            // entry of the extended type (u + a).
            let ext_input = if active.history.len() == 1 {
                Value::right(current)
            } else {
                // Already in extended form from a prior step.
                current
            };

            let body = body.clone();
            match eval(&body, ext_input, &mut fuel) {
                Ok(next) => {
                    let label = match &next {
                        Value::Left(_) => "label",
                        Value::Right(_) => "exit",
                        _ => "?",
                    };
                    active.history.push(next);
                    let _ = writeln!(out, "{label}: {:?}", active.history.last().unwrap());
                }
                Err(EvalError::OutOfFuel) => {
                    let _ = writeln!(out, "error: out of fuel evaluating `{}`", active.name);
                }
                Err(e) => {
                    let _ = writeln!(out, "error: eval failed: {e:?}");
                }
            }
            return;
        }

        // Plain (non-Trace) iso: evaluate the whole iso in one step.
        // History entries alternate types: even indices are lhs_ty, odd are rhs_ty.
        // After pushing, the new entry's index determines its type.
        match eval(&active.iso, current, &mut fuel) {
            Ok(next) => {
                let new_idx = active.history.len(); // index after push
                let render_ty = if new_idx % 2 == 0 { &active.lhs_ty } else { &active.rhs_ty };
                let rendered = value::render(&next, render_ty, &self.ep.env);
                active.history.push(next);
                let _ = writeln!(out, "{rendered}");
            }
            Err(EvalError::OutOfFuel) => {
                let _ = writeln!(out, "error: out of fuel evaluating `{}`", active.name);
            }
            Err(e) => {
                let _ = writeln!(out, "error: eval failed: {e:?}");
            }
        }
    }

    fn cmd_unstep(&mut self, out: &mut dyn Write) {
        let active = match &mut self.active {
            Some(a) => a,
            None => {
                let _ = writeln!(out, "error: no iso loaded; use :load first");
                return;
            }
        };

        if active.history.len() <= 1 {
            let _ = writeln!(out, "already at start");
            return;
        }

        active.history.pop();
        let current = active.current();

        // For Trace isos, render the extended-type value (unless we popped
        // back to the initial plain value at history[0]).
        if matches!(&active.iso, Iso::Trace(..)) && active.history.len() > 1 {
            let label = match current {
                Value::Left(_) => "label",
                Value::Right(_) => "exit",
                _ => "?",
            };
            let _ = writeln!(out, "{label}: {current:?}");
        } else {
            // Plain iso: even-indexed entries are lhs_ty, odd are rhs_ty.
            let idx = active.history.len() - 1;
            let render_ty = if idx % 2 == 0 { &active.lhs_ty } else { &active.rhs_ty };
            let rendered = value::render(current, render_ty, &self.ep.env);
            let _ = writeln!(out, "{rendered}");
        }
    }
}

enum ControlFlow {
    Continue,
    Quit,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipeline;

    /// Helper: run a script against a program source and return stdout lines.
    fn run_repl(program_src: &str, script: &str) -> Vec<String> {
        let ep = pipeline::elaborate(program_src).expect("program should elaborate");
        let mut repl = Repl::new(ep);
        let mut input = std::io::Cursor::new(script.as_bytes().to_vec());
        let mut output = Vec::new();
        repl.run(&mut input, &mut output);
        let text = String::from_utf8(output).expect("valid utf8");
        text.lines().map(|l| l.to_string()).collect()
    }

    const NOT_PROGRAM: &str = "\
type Bit = O | I

iso notb : Bit <-> Bit
| O <-> I
| I <-> O
";

    const SWAP_PROGRAM: &str = "\
type Bit = O | I

iso swp : (Bit, Bit) <-> (Bit, Bit)
| (a, b) <-> (b, a)
";

    #[test]
    fn step_then_unstep_returns_to_start() {
        let lines = run_repl(
            NOT_PROGRAM,
            ":load notb O\n:step\n:unstep\n:quit\n",
        );
        // :load prints "loaded: O"
        assert_eq!(lines[0], "loaded: O");
        // :step prints "I" (not(O) = I)
        assert_eq!(lines[1], "I");
        // :unstep prints "O" (back to start)
        assert_eq!(lines[2], "O");
    }

    #[test]
    fn n_steps_then_n_unsteps_returns_to_start() {
        // not(not(O)) = O, so stepping twice cycles.
        let lines = run_repl(
            NOT_PROGRAM,
            ":load notb O\n:step\n:step\n:unstep\n:unstep\n:quit\n",
        );
        assert_eq!(lines[0], "loaded: O");
        assert_eq!(lines[1], "I");   // step 1: O -> I
        assert_eq!(lines[2], "O");   // step 2: I -> O
        assert_eq!(lines[3], "I");   // unstep 1: back to I
        assert_eq!(lines[4], "O");   // unstep 2: back to O (start)
    }

    #[test]
    fn unstep_at_start_is_noop() {
        let lines = run_repl(
            NOT_PROGRAM,
            ":load notb O\n:unstep\n:quit\n",
        );
        assert_eq!(lines[0], "loaded: O");
        assert_eq!(lines[1], "already at start");
    }

    #[test]
    fn swap_pair_step_unstep() {
        let lines = run_repl(
            SWAP_PROGRAM,
            ":load swp (O, I)\n:step\n:unstep\n:quit\n",
        );
        assert_eq!(lines[0], "loaded: (O, I)");
        assert_eq!(lines[1], "(I, O)");  // step: swap
        assert_eq!(lines[2], "(O, I)");  // unstep: back to start
    }

    #[test]
    fn swap_pair_four_steps_four_unsteps() {
        let lines = run_repl(
            SWAP_PROGRAM,
            ":load swp (O, I)\n:step\n:step\n:step\n:step\n:unstep\n:unstep\n:unstep\n:unstep\n:quit\n",
        );
        assert_eq!(lines[0], "loaded: (O, I)");
        // 4 steps: (O,I) -> (I,O) -> (O,I) -> (I,O) -> (O,I)
        assert_eq!(lines[1], "(I, O)");
        assert_eq!(lines[2], "(O, I)");
        assert_eq!(lines[3], "(I, O)");
        assert_eq!(lines[4], "(O, I)");
        // 4 unsteps: (O,I) -> (I,O) -> (O,I) -> (I,O) -> (O,I) (back to start)
        assert_eq!(lines[5], "(I, O)");
        assert_eq!(lines[6], "(O, I)");
        assert_eq!(lines[7], "(I, O)");
        assert_eq!(lines[8], "(O, I)");
    }

    #[test]
    fn unknown_command() {
        let lines = run_repl(NOT_PROGRAM, ":foo\n:quit\n");
        assert_eq!(lines[0], "unknown command: :foo");
    }

    #[test]
    fn step_without_load() {
        let lines = run_repl(NOT_PROGRAM, ":step\n:quit\n");
        assert_eq!(lines[0], "error: no iso loaded; use :load first");
    }

    #[test]
    fn unstep_without_load() {
        let lines = run_repl(NOT_PROGRAM, ":unstep\n:quit\n");
        assert_eq!(lines[0], "error: no iso loaded; use :load first");
    }

    // ── Label-loop (Trace) stepping tests ────────────────────────

    const SPIN_PROGRAM: &str = "\
type Bit = O | I

iso spin : Bit <-> Bit
| b <-> $go (b, (O, O))
| $go (b, (O, O)) <-> $go (b, (O, I))
| $go (b, (O, I)) <-> $go (b, (I, O))
| $go (b, (I, O)) <-> $go (b, (I, I))
| $go (b, (I, I)) <-> b
";

    #[test]
    fn label_loop_step_walks_intermediate_states() {
        // spin with input O: 5 body applications (entry + 3 counter + exit).
        // Steps 1-4 are label (Left) states; step 5 is exit (Right).
        let script = ":load spin O\n\
                       :step\n:step\n:step\n:step\n:step\n\
                       :quit\n";
        let lines = run_repl(SPIN_PROGRAM, script);
        assert_eq!(lines[0], "loaded: O");
        // 5 steps total, proving per-body-application stepping (not whole-loop)
        assert_eq!(lines.len(), 6); // loaded + 5 steps
        // Steps 1-4 are intermediate label states
        for (i, line) in lines.iter().enumerate().take(5).skip(1) {
            assert!(
                line.starts_with("label:"),
                "step {i} should be a label state, got: {line}",
            );
        }
        // Step 5 is the exit
        assert!(
            lines[5].starts_with("exit:"),
            "step 5 should be an exit state, got: {}",
            lines[5]
        );
    }

    #[test]
    fn label_loop_step_then_unstep_returns_to_start() {
        let script = ":load spin O\n\
                       :step\n:step\n\
                       :unstep\n:unstep\n\
                       :quit\n";
        let lines = run_repl(SPIN_PROGRAM, script);
        assert_eq!(lines[0], "loaded: O");
        // Step 1: label state
        assert!(lines[1].starts_with("label:"));
        // Step 2: label state
        assert!(lines[2].starts_with("label:"));
        // Unstep 1: back to step-1 state (label)
        assert!(lines[3].starts_with("label:"));
        assert_eq!(lines[3], lines[1], "unstep should return to prior label state");
        // Unstep 2: back to initial plain value
        assert_eq!(lines[4], "O");
    }

    #[test]
    fn label_loop_all_steps_then_all_unsteps() {
        // Step through the entire loop, then unstep all the way back.
        let script = ":load spin O\n\
                       :step\n:step\n:step\n:step\n:step\n\
                       :unstep\n:unstep\n:unstep\n:unstep\n:unstep\n\
                       :quit\n";
        let lines = run_repl(SPIN_PROGRAM, script);
        assert_eq!(lines[0], "loaded: O");
        // Forward: 4 label + 1 exit = 5 steps
        for line in lines.iter().take(5).skip(1) {
            assert!(line.starts_with("label:"));
        }
        assert!(lines[5].starts_with("exit:"));
        // Backward: unstep mirrors the forward states in reverse
        for line in lines.iter().take(10).skip(6) {
            assert!(line.starts_with("label:"));
        }
        // Final unstep returns to the plain initial value
        assert_eq!(lines[10], "O");
    }

    #[test]
    fn label_loop_intermediate_states_are_distinct() {
        // Each label state should be different (the 2-bit counter advances).
        let script = ":load spin O\n\
                       :step\n:step\n:step\n:step\n\
                       :quit\n";
        let lines = run_repl(SPIN_PROGRAM, script);
        // lines[1..=4] are the 4 label states — all should be distinct.
        let label_states: Vec<&str> = lines[1..=4].iter().map(|s| s.as_str()).collect();
        for i in 0..label_states.len() {
            for j in (i + 1)..label_states.len() {
                assert_ne!(
                    label_states[i], label_states[j],
                    "label states at step {} and {} should differ",
                    i + 1,
                    j + 1
                );
            }
        }
    }

    // ── BUG 1: step past loop exit ──────────────────────────────

    #[test]
    fn label_loop_step_past_exit_prints_complete() {
        // spin with O takes 5 body steps (4 label + 1 exit).
        // Steps 6 and 7 should print "loop complete" and not push new states.
        let script = ":load spin O\n\
                       :step\n:step\n:step\n:step\n:step\n\
                       :step\n:step\n\
                       :quit\n";
        let lines = run_repl(SPIN_PROGRAM, script);
        assert_eq!(lines[0], "loaded: O");
        // Steps 1-4: label states
        for line in lines.iter().take(5).skip(1) {
            assert!(line.starts_with("label:"), "expected label, got: {line}");
        }
        // Step 5: exit
        assert!(lines[5].starts_with("exit:"), "expected exit, got: {}", lines[5]);
        // Steps 6-7: loop complete (no panic, no new state)
        assert_eq!(lines[6], "loop complete");
        assert_eq!(lines[7], "loop complete");
        // Exactly 8 output lines (loaded + 5 steps + 2 complete notices)
        assert_eq!(lines.len(), 8);
    }

    // ── BUG 2: asymmetric iso rendering ─────────────────────────

    const REASSOC_PROGRAM: &str = "\
type Bit = O | I

iso assoc : (Bit, (Bit, Bit)) <-> ((Bit, Bit), Bit)
| (a, (b, c)) <-> ((a, b), c)
";

    #[test]
    fn asymmetric_iso_step_renders_with_rhs_type() {
        // assoc maps (Bit, (Bit, Bit)) -> ((Bit, Bit), Bit).
        // Previously this panicked because the result was rendered with lhs_ty.
        let script = ":load assoc (O, (I, O))\n:step\n:quit\n";
        let lines = run_repl(REASSOC_PROGRAM, script);
        assert_eq!(lines[0], "loaded: (O, (I, O))");
        // The result should render as ((O, I), O) using rhs_ty.
        assert_eq!(lines[1], "((O, I), O)");
    }

    #[test]
    fn asymmetric_iso_step_unstep_roundtrip() {
        // Step forward then unstep back — initial value rendered with lhs_ty.
        let script = ":load assoc (O, (I, O))\n:step\n:unstep\n:quit\n";
        let lines = run_repl(REASSOC_PROGRAM, script);
        assert_eq!(lines[0], "loaded: (O, (I, O))");
        assert_eq!(lines[1], "((O, I), O)");   // rhs_ty
        assert_eq!(lines[2], "(O, (I, O))");   // back to lhs_ty
    }
}
