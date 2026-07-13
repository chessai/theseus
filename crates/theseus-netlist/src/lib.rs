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
//! That artifact now has its tick-level executor: [`sim::simulate`]
//! walks the cells in tick order, asserts the *dynamic* INV-TOKEN
//! invariant every tick (exactly one line of each rail pair high at its
//! valid tick), and returns decoded outputs + per-(kind, phase)
//! occupancy. Later slices: the tick-exact balancer (C2.2); the energy
//! annotation over the characterization DB lives in
//! `theseus-spice::annotate` (ADR-0028).

pub mod balance;
pub mod bennett;
pub mod canonicalize;
pub mod ir;
pub mod lower;
pub mod lower_iter;
pub mod ring;
pub mod sim;

pub mod schedule {}
pub mod flatten {}
pub mod sim_typed {
    //! Typed-token simulation: the tick-level phased-sim ([`crate::sim`])
    //! runs at typed-token granularity — one boolean token per rail pair
    //! per tick — asserting the dynamic INV-TOKEN protocol every tick
    //! (spirit of ADR-0012's always-on validity assertion).
    pub use crate::sim::{simulate, SimError, SimResult};
}
pub mod sim_bits {
    //! TODO(bit-level sim): the electrical (rail, mirror) SR granularity
    //! *below* the token line — per-phase trapezoid edges and C2.6
    //! validity-window arithmetic, pinned empirically in B5 against the
    //! reference decks. Deferred; the typed-token sim ([`crate::sim`])
    //! is this slice's executor.
}

pub use balance::{balance, balance_compact, BalanceError, CompactBalanced, CompactSimError};
pub use bennett::{bennett_wrap, lower_iso_bennett, BennettError, BennettWrapError};
pub use canonicalize::canonicalize;
pub use ir::cell::{Cell, CellKind, LineId, PhasedCellNetlist, RailPair, RoleSwapKind, PHASE_COUNT};
pub use ir::typed::{Monoid, NodeKind, TypedNode};
pub use lower::{
    bits_of, lower, lower_iso, prim_from_bits, rail_width, value_from_bits, LowerError,
};
pub use ring::{pad_ring, RingError, RingNetlist};
pub use sim::{simulate, SimError, SimResult};
