//! Emission: flat bit-netlist → typed Verilog mini-AST → text.
//!
//! The typed middle layer is the point (ADR-0020, amended): `x`/don't-cares
//! are *unrepresentable*, not policed (ADR-0012); declared-before-used and
//! width agreement are constructor-enforced; the AST is snapshot-testable
//! independently of lowering. Structural subset only: modules, ports,
//! wires, instances, assigns, always_ff (i.e. structural SystemVerilog,
//! which Yosys reads natively — not Verilog-2005).
//!
//! FPGA lowering default: padded tag + shared payload with machine-checked
//! canonicity; dual-rail/1-of-N earmarked for the adiabatic backend;
//! dense arithmetic encoding documented-rejected (ADR-0011).

pub mod vast {}
pub mod lower {}
pub mod verilator {
    //! Harness shape (a): standalone verilated binary + framed stdio
    //! protocol — sandbox-friendly, crash-isolated, and the protocol
    //! doubles as the conformance suite's simulator interface (ADR-0020).
}
