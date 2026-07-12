//! Typed, hierarchical reversible netlist IR + its simulators.
//!
//! Node vocabulary (ADR-0013): Par / Branch / Permute(monoid, σ) /
//! Distrib / TraceLoop / Iter (reserved) / HandshakeLoop (v0.2+) /
//! Prim / Reg / ports. Sums stay *typed* in this IR; encoding is a
//! backend lowering decision (ADR-0011).
//!
//! - Canonicalization fuses all structural plumbing into `Permute` —
//!   canonical and unique by Mac Lane coherence, rig fragment only
//!   (ADR-0014; theory ledger TH-3).
//! - Canonicity invariant `valid ⇒ well-formed` is conditional and
//!   locally re-established: per-node lowering lemmas + defensive masking
//!   at merges, both directions (ADR-0012).
//! - v0.1 scheduling is static (totality ⇒ computable schedule);
//!   path-balancing runs on the typed IR (ADR-0015), parameterized by a
//!   `TargetProfile` (ADR-0016).
//!
//! # The adiabatic path (M3b, this slice)
//!
//! Core Π → [`canonicalize`] → typed netlist ([`ir::typed`]) → [`lower`]
//! (dual-rail, per the ADR-0031 lemma catalog) → **phased cell netlist**
//! ([`ir::cell`], the ADR-0026 C2.7 artifact): k-CSWAP cells (ADR-0027)
//! with phase = depth mod P (C2.3), rail-pair wires, typed ports.
//!
//! Later slices consume that artifact: the tick-level phased-sim with
//! its per-tick INV-TOKEN assertion, the tick-exact balancer (C2.2), and
//! the energy annotation over the characterization DB (ADR-0028).

pub mod canonicalize;
pub mod ir;
pub mod lower;

pub mod schedule {}
pub mod flatten {}
pub mod sim_typed {
    //! Asserts `scheduled-valid ⇒ well-formed` on every typed net, every
    //! cycle, in test builds (ADR-0012). Emits typed-decoded VCD.
}
pub mod sim_bits {}

pub use canonicalize::canonicalize;
pub use ir::cell::{Cell, CellKind, LineId, PhasedCellNetlist, RailPair, PHASE_COUNT};
pub use ir::typed::{Monoid, NodeKind, TypedNode};
pub use lower::{lower, lower_iso, LowerError};
