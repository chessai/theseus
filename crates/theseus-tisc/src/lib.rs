//! `theseus-tisc`: TISC-1 reversible CPU datapath.
//!
//! Provides the shared State type, register-addressing layer, and
//! instruction isos for the TISC-1 ISA. Each instruction is an
//! `Iso : State <-> State` that can be lowered to a cell netlist.

pub mod types;
pub mod addressing;
pub mod decoder;
pub mod instr;
pub mod step;
