//! `theseus-tisc`: TISC-1 reversible CPU datapath.
//!
//! Provides the shared State type, register-addressing layer, and
//! instruction isos for the TISC-1 ISA. Each instruction is an
//! `Iso : State <-> State` that can be lowered to a cell netlist.

pub mod addressing;
pub mod asm;
pub mod bveq;
pub mod bveq_netlist;
pub mod compile;
pub mod decoder;
pub mod energy;
pub mod instr;
pub mod money_plot;
pub(crate) mod plumbing;
pub mod ring_run;
pub mod step;
pub mod synth;
pub mod types;
