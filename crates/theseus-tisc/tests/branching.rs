#![cfg(feature = "heavy-tests")]
// HEAVY tier: gated out of the fast gate; advisor runs --features heavy-tests --test-threads=1 pre-merge
//! GO-ROLLOUT: RETIRED → tests/branching_bveq.rs.
//!
//! This file's ADR-0034 branching tests proved data-dependent branching
//! (SWAPBR rd + the end-of-step `PC += sext(BR) + 1`) by CONCRETE
//! `theseus_eval::eval` on a handful of hand-built states -- slow (>4 min) and
//! NOT whole-domain. They are RETIRED into `tests/branching_bveq.rs`, which
//! proves the branch datapath SYMBOLICALLY over the WHOLE (regs, BR) domain via
//! `bveq_netlist::eval_bdd` (the GO-rollout convention decoder.rs uses to retire
//! into decoder_bveq.rs). This stub keeps no compiled tests so no >4 min copy
//! runs twice; the authority + reality anchors all live in branching_bveq.rs.
//!
//! ── Where each obligation went ──────────────────────────────────────
//! * `taken_branch_with_come_from` (a program that ACTUALLY branches: load δ
//!   into a reg, SWAPBR moves it into BR, the same-step PC update jumps by δ,
//!   come-from clears BR) — the single-step SWAPBR datapath (rd'=old BR,
//!   BR'=old rd, PC'=pc+sext(old rd)+1) is now proved WHOLE-DOMAIN in
//!   `branching_bveq::branch_swapbr_whole_domain` +
//!   `branch_come_from_whole_domain`; the SAME concrete δ=3 program is kept as a
//!   reality anchor in `branching_bveq::branch_reality_anchor`.
//! * `ir_zero_and_scratch_after_branch` (P4: IR=0 + s0/s1 restored across SWAPBR
//!   steps) — proved whole-domain inside `branch_swapbr_whole_domain` (IR=0,
//!   s0=s1=0 asserted symbolically) and kept as a concrete grid in
//!   `branch_reality_anchor`.
//! * `br_zero_fall_through_is_pc_plus_one` (BR=0 ⇒ PC += 1, v1-compat) — kept as
//!   a concrete anchor in `branch_reality_anchor`; the general PC update
//!   `PC' = PC + sext(BR) + 1` (BR=0 ⇒ +1 as a special case) is proved
//!   whole-domain by `bveq_step_poc` / `bveq_full_step_poc`.
//! * REVERSIBILITY (T forward + T adjoint = start for the multi-step branch
//!   program) — a whole-PROGRAM bijection obligation distinct from the
//!   single-step datapath equivalence, kept CONCRETE (cheap) in
//!   `branching_bveq::branch_reversibility`.
//!
//! Soundness note: `bveq::instr_semantics` HARD-REJECTS SWAPBR
//! (`NonRegisterPure` — it moves state off the register frame into BR), so
//! branching_bveq builds its SWAPBR oracle DIRECTLY from the symbolic input bits
//! (the ISA definition), independent of the netlist — see its header.
