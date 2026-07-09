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

pub mod ir {}
pub mod canonicalize {}
pub mod schedule {}
pub mod flatten {}
pub mod sim_typed {
    //! Asserts `scheduled-valid ⇒ well-formed` on every typed net, every
    //! cycle, in test builds (ADR-0012). Emits typed-decoded VCD.
}
pub mod sim_bits {}
