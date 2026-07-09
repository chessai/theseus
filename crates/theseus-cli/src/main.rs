//! theseus driver.
//!
//! - Sequential, deterministic batch pipeline (ADR-0016); every boundary
//!   dumpable via `--emit=core|typed|scheduled|flat|bits|verilog`.
//! - Invariant ledger validated statically before running passes.
//! - External tools resolve through `TargetTools` (env/Nix seam, ADR-0019),
//!   never PATH-luck.
//! - REPL: `:step` / `:unstep`.

fn main() {
    println!("theseus {} — skeleton; see docs/ for the plan", env!("CARGO_PKG_VERSION"));
}
