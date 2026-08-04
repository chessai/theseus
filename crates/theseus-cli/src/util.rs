//! Small CLI helpers shared across subcommands.

use std::fs;
use std::process;

/// Read a source file, or print the standard error and exit with code 1.
pub fn load_source(file: &str) -> String {
    fs::read_to_string(file).unwrap_or_else(|e| {
        eprintln!("error reading {file}: {e}");
        process::exit(1);
    })
}
