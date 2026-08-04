//! End-to-end pipeline: parse → check → elaborate → eval.

use std::collections::BTreeMap;

use theseus_check::diagnostics::Diagnostic;
use theseus_check::elaborate::elaborate_program;
use theseus_check::env::{Env, IsoSig};
use theseus_core::iso::Iso;
use theseus_core::prim::Value;
use theseus_eval::interp::{eval, EvalError};
use theseus_syntax::ast::Program;
use theseus_syntax::parser::ParseError;

/// Parse then run all static checks. Returns diagnostics (empty = well-formed).
pub fn diagnostics(src: &str) -> Result<Vec<Diagnostic>, ParseError> {
    let program = theseus_syntax::parse(src)?;
    let program = theseus_check::prelude::with_prelude(&program);
    Ok(theseus_check::check(&program))
}

/// A successfully elaborated program ready for evaluation.
pub struct ElaboratedProgram {
    pub env: Env,
    pub isos: BTreeMap<String, Result<Iso, Vec<Diagnostic>>>,
    pub program: Program,
}

/// Parse, check, and elaborate a source string.
pub fn elaborate(src: &str) -> Result<ElaboratedProgram, ElabError> {
    let program = theseus_syntax::parse(src).map_err(ElabError::Parse)?;
    let program = theseus_check::prelude::with_prelude(&program);
    let diags = theseus_check::check(&program);
    if !diags.is_empty() {
        return Err(ElabError::Check(diags));
    }
    let env = theseus_check::env::resolve(&program);
    let isos = elaborate_program(&program, &env);
    Ok(ElaboratedProgram { env, isos, program })
}

/// Errors from the elaboration pipeline.
#[derive(Debug)]
pub enum ElabError {
    Parse(ParseError),
    Check(Vec<Diagnostic>),
}

/// Evaluate an iso forward on a value with the given fuel budget.
pub fn run_iso(iso: &Iso, value: Value, fuel: &mut u64) -> Result<Value, EvalError> {
    eval(iso, value, fuel)
}

/// Look up an iso signature in the environment.
pub fn iso_sig(env: &Env, name: &str) -> Option<IsoSig> {
    env.iso(name).cloned()
}
