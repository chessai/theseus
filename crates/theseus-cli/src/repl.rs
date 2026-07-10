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
    /// Surface lhs type (iso domain — used for rendering).
    lhs_ty: theseus_syntax::ast::Type,
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
        match eval(&active.iso, current, &mut fuel) {
            Ok(next) => {
                let rendered = value::render(&next, &active.lhs_ty, &self.ep.env);
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
        let rendered = value::render(current, &active.lhs_ty, &self.ep.env);
        let _ = writeln!(out, "{rendered}");
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
}
