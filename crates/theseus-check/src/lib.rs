//! `theseus-check` — the elaborator and static-semantics checker.
//!
//! The crate implements the four static-semantics stages in the order fixed by
//! `docs/src/spec/static-semantics.md` — Names (T0101), Typing (T0102),
//! Linearity (T0201/T0202), Coverage (T0301/T0302) — under **global gating**:
//! a later stage runs only if every earlier stage reported nothing. This makes
//! diagnostics from a downstream stage trustworthy (they never depend on
//! artifacts a failed upstream stage could not establish).
//!
//! # Module layout
//! * [`diagnostics`] — the structured [`Diagnostic`] shared by every stage.
//! * [`env`] — the resolution [`Env`] and surface→core type lowering.
//! * [`testkit`] — the conformance harness (public; called from stage tests).
//! * [`names`], [`typing`], [`linearity`], [`coverage`] — the four stages.
//! * [`elaborate`] — surface→core iso elaboration.

pub mod coverage;
pub mod diagnostics;
pub mod elaborate;
pub mod env;
pub mod labels;
pub mod linearity;
pub mod names;
pub mod testkit;
pub mod typing;

pub use diagnostics::{Code, Diagnostic, Side};

use theseus_syntax::ast::Program;

/// Run the full staged static-semantics judgment with global gating.
///
/// Builds the [`env::Env`] once, then runs [`names::check`]; if it reported
/// anything, those diagnostics are returned and no later stage runs. Otherwise
/// [`typing::check`] runs, then [`linearity::check`], then [`coverage::check`],
/// each gated on the previous being clean.
pub fn check(program: &Program) -> Vec<Diagnostic> {
    let env = env::resolve(program);

    let names = names::check(program, &env);
    if !names.is_empty() {
        return names;
    }

    let typing = typing::check(program, &env);
    if !typing.is_empty() {
        return typing;
    }

    let linearity = linearity::check(program, &env);
    if !linearity.is_empty() {
        return linearity;
    }

    coverage::check(program, &env)
}
