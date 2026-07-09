//! Fuel-based interpreter over core Π — stage one of the oracle chain.
//!
//! - Partiality per join-inverse-category semantics (ADR-0005): fuel in
//!   software, valid/done handshakes in hardware.
//! - The interpreter is the semantic root: typed-sim, bit-sim, and
//!   Verilator are all differentially tested against it (ADR-0008/0018).
//! - Powers the REPL's `:step`/`:unstep` — the software preview of the
//!   FPGA time-travel demo (workspace plan, ADR-0016 provenance).

pub mod value {}
pub mod interp {}
pub mod enumerate {
    //! Exhaustive enumeration per finite type: below the size threshold,
    //! "proven on all inputs" is an achievable test verdict (ADR-0018).
}
