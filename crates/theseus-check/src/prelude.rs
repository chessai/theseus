//! Implicit prelude: a canonical set of declarations merged into every program.
//!
//! The prelude source is compiled into the binary via `include_str!` and parsed
//! once per call to [`with_prelude`]. Structural dedup ensures that if a user
//! program already declares a type or iso with the same name as a prelude
//! declaration, the user's copy wins (the prelude copy is dropped).

use std::collections::HashSet;

use theseus_syntax::ast::{Decl, Program};

/// The raw source of the standard prelude.
pub const PRELUDE_SRC: &str = include_str!("../../../stdlib/prelude.thse");

/// Return the declared name of a top-level declaration, if it has one.
fn decl_name(decl: &Decl) -> Option<&str> {
    match decl {
        Decl::Type(td) => Some(&td.name.text),
        Decl::Iso(id) => Some(&id.name.text),
        Decl::Prim(pd) => Some(&pd.name.text),
    }
}

/// Merge the implicit prelude into `program`, returning a new `Program` whose
/// decls are `[kept_prelude_decls..., program_decls...]`.
///
/// **Structural dedup**: for each prelude decl, if `program` already contains a
/// `Type` or `Iso` (or `Prim`) with the same name, the prelude copy is dropped.
/// The user's declaration wins and the existing duplicate-name machinery (in
/// `env::resolve`) judges whether the user's copy is valid.
pub fn with_prelude(program: &Program) -> Program {
    let prelude_program =
        theseus_syntax::parse(PRELUDE_SRC).expect("stdlib/prelude.thse must parse");

    // Collect names declared by the user program.
    let user_names: HashSet<&str> = program
        .decls
        .iter()
        .filter_map(decl_name)
        .collect();

    // Keep only prelude decls whose name is NOT already declared by the user.
    let mut merged_decls: Vec<Decl> = prelude_program
        .decls
        .into_iter()
        .filter(|d| {
            decl_name(d)
                .map(|n| !user_names.contains(n))
                .unwrap_or(true)
        })
        .collect();

    merged_decls.extend(program.decls.iter().cloned());

    Program {
        decls: merged_decls,
        span: program.span,
        depth_overrides: program.depth_overrides.clone(),
    }
}
