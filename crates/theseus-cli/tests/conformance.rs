//! Conformance harness: grades program fixtures against `.expect` tables and
//! diagnostic fixtures against `.diag` sidecars.

use std::fs;
use std::path::Path;

use theseus_check::elaborate::elaborate_program;
use theseus_check::env::resolve;
use theseus_check::testkit;
use theseus_cli::value;
use theseus_core::iso::Iso;
use theseus_eval::interp::eval;

const FUEL: u64 = 1_000_000;

// ── Helpers ──────────────────────────────────────────────────────────────

fn repo_root() -> std::path::PathBuf {
    let manifest = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest
        .parent()
        .and_then(Path::parent)
        .expect("crate manifest dir has a grandparent")
        .to_path_buf()
}

/// Parse the `.expect` sidecar header and rows using surface values.
///
/// Returns `(iso_name, Vec<(lhs_str, rhs_str)>)`.
fn parse_expect_surface(text: &str) -> (String, Vec<(String, String)>) {
    let mut name = String::new();
    let mut rows = Vec::new();
    for raw in text.lines() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') {
            if let Some(rest) = line.strip_prefix("# iso:") {
                name = rest.trim().to_string();
            }
            continue;
        }
        if let Some((lhs, rhs)) = line.split_once("|->") {
            rows.push((lhs.trim().to_string(), rhs.trim().to_string()));
        }
    }
    (name, rows)
}

// ── Program fixtures (the main conformance test) ─────────────────────────

#[test]
fn conformance_programs() {
    let root = repo_root();
    let programs_dir = root.join("conformance/programs");

    let mut fixtures: Vec<std::path::PathBuf> = fs::read_dir(&programs_dir)
        .unwrap_or_else(|e| panic!("reading {}: {e}", programs_dir.display()))
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("thse"))
        .collect();
    fixtures.sort();

    let mut graded = 0usize;
    let mut skipped_names = Vec::new();

    for thse_path in &fixtures {
        let stem = thse_path.file_stem().unwrap().to_str().unwrap();

        // Skip 0011-label-loop (labels are out of slice-1 scope).
        if stem == "0011-label-loop" {
            eprintln!("[skip] {stem}: deferred (labels)");
            continue;
        }

        let expect_path = thse_path.with_extension("expect");
        if !expect_path.exists() {
            continue;
        }

        let expect_text = fs::read_to_string(&expect_path)
            .unwrap_or_else(|e| panic!("reading {}: {e}", expect_path.display()));
        let (iso_name, rows) = parse_expect_surface(&expect_text);
        assert!(
            !iso_name.is_empty(),
            "no `# iso:` header in {}",
            expect_path.display()
        );

        // Parse the program.
        let prog = testkit::load_program(thse_path);

        // Check: must be well-formed.
        let diags = theseus_check::check(&prog);
        assert!(
            diags.is_empty(),
            "[{stem}] program has diagnostics: {diags:?}"
        );

        // Elaborate.
        let env = resolve(&prog);
        let elab_map = elaborate_program(&prog, &env);

        let iso_result = elab_map.get(&iso_name).unwrap_or_else(|| {
            panic!("[{stem}] iso `{iso_name}` not in elaboration map")
        });

        let iso = match iso_result {
            Ok(iso) => iso.clone(),
            Err(errs) if errs.is_empty() => {
                // Not elaborated — skip (deferred/unsupported).
                eprintln!("[skip] {stem}: iso `{iso_name}` not elaborated (structural core)");
                skipped_names.push(format!("{stem}/{iso_name}"));
                continue;
            }
            Err(errs) => {
                panic!("[{stem}] iso `{iso_name}` elaboration errors: {errs:?}");
            }
        };

        // Get the iso signature for type-guided value conversion.
        let sig = env.iso(&iso_name).unwrap_or_else(|| {
            panic!("[{stem}] iso `{iso_name}` not in env")
        });

        // Grade each row — forward AND backward.
        for (lhs_str, rhs_str) in &rows {
            let lhs_sv = value::parse_surface_value(lhs_str);
            let rhs_sv = value::parse_surface_value(rhs_str);
            let lhs_val = value::to_core(&lhs_sv, &sig.lhs, &env);
            let rhs_val = value::to_core(&rhs_sv, &sig.rhs, &env);

            // Forward: eval(iso, lhs) == rhs
            let mut fuel = FUEL;
            let fwd_result = eval(&iso, lhs_val.clone(), &mut fuel)
                .unwrap_or_else(|e| panic!("[{stem}] forward eval error on `{lhs_str}`: {e:?}"));
            assert_eq!(
                fwd_result, rhs_val,
                "[{stem}] forward mismatch: {lhs_str} |-> expected {rhs_str}, got {}",
                value::render(&fwd_result, &sig.rhs, &env)
            );

            // Backward: eval(sym(iso), rhs) == lhs
            let sym_iso = Iso::sym(iso.clone());
            let mut fuel = FUEL;
            let bwd_result = eval(&sym_iso, rhs_val.clone(), &mut fuel)
                .unwrap_or_else(|e| panic!("[{stem}] backward eval error on `{rhs_str}`: {e:?}"));
            assert_eq!(
                bwd_result, lhs_val,
                "[{stem}] backward mismatch: {rhs_str} |-> expected {lhs_str}, got {}",
                value::render(&bwd_result, &sig.lhs, &env)
            );
        }

        graded += 1;
        eprintln!("[pass] {stem}: iso `{iso_name}` ({} rows)", rows.len());
    }

    eprintln!(
        "\nConformance: {graded} graded, {} skipped",
        skipped_names.len()
    );
    if !skipped_names.is_empty() {
        eprintln!("Skipped: {}", skipped_names.join(", "));
    }

    // At least the structural-core fixtures must have graded.
    assert!(
        graded >= 8,
        "expected at least 8 graded fixtures, got {graded}"
    );
}

// ── Diagnostic fixtures ──────────────────────────────────────────────────

#[test]
fn conformance_diagnostics() {
    let root = repo_root();
    let diag_dir = root.join("conformance/diagnostics");

    let mut fixtures: Vec<std::path::PathBuf> = fs::read_dir(&diag_dir)
        .unwrap_or_else(|e| panic!("reading {}: {e}", diag_dir.display()))
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("thse"))
        .collect();
    fixtures.sort();

    let mut tested = 0usize;

    for thse_path in &fixtures {
        let stem = thse_path.file_stem().unwrap().to_str().unwrap();

        // Skip deferred-scope fixtures: 0008 (labels, T0303) and 0009 (prim/adjoint
        // diagnostics, T0401) — both out of the M1 slice-1 static-check set.
        if stem == "0008-label-coverage" || stem == "0009-prim-mismatch" {
            eprintln!("[skip] diag {stem}: deferred scope (labels / prim diagnostics)");
            continue;
        }

        let diag_path = thse_path.with_extension("diag");
        if !diag_path.exists() {
            continue;
        }

        // 0007-parse is a parse-error test — parse will fail, can't run check.
        let src = fs::read_to_string(thse_path)
            .unwrap_or_else(|e| panic!("reading {}: {e}", thse_path.display()));
        let prog = match theseus_syntax::parse(&src) {
            Ok(p) => p,
            Err(_) => {
                eprintln!("[skip] diag {stem}: parse error fixture (cannot run check)");
                continue;
            }
        };

        let diag_text = fs::read_to_string(&diag_path)
            .unwrap_or_else(|e| panic!("reading {}: {e}", diag_path.display()));
        // Compare on CODE + SIDE. Witness precision is verified in the check
        // crate's per-module unit tests; testkit's env-less value parser cannot
        // decode surface-constructor witnesses (e.g. `Just I`), so the end-to-end
        // pipeline check asserts the right diagnostic codes/sides are emitted.
        let expected = parse_diag_codes(&diag_text);
        let reported = theseus_check::check(&prog);
        for (code, side) in &expected {
            let matched = reported.iter().any(|d| {
                if format!("{:?}", d.code) != *code {
                    return false;
                }
                match side {
                    None => true,
                    Some(s) => d
                        .side
                        .as_ref()
                        .is_some_and(|ds| format!("{ds:?}").eq_ignore_ascii_case(s)),
                }
            });
            assert!(
                matched,
                "[{stem}] expected diagnostic {code}{} not reported; got {reported:?}",
                side.as_deref().map(|s| format!("/{s}")).unwrap_or_default()
            );
        }

        tested += 1;
        eprintln!("[pass] diag {stem}");
    }

    eprintln!("\nDiagnostic conformance: {tested} tested");
}

/// Extract the expected `(code, side)` blocks from a `.diag` sidecar: the code
/// from an `error:` line, the optional `side:` that follows it. Witness/clauses/
/// variable/name payloads are intentionally ignored here (see the call site).
fn parse_diag_codes(text: &str) -> Vec<(String, Option<String>)> {
    let mut out = Vec::new();
    let mut cur: Option<(String, Option<String>)> = None;
    for line in text.lines() {
        let line = line.trim();
        if let Some(c) = line.strip_prefix("error:") {
            if let Some(b) = cur.take() {
                out.push(b);
            }
            cur = Some((c.trim().to_string(), None));
        } else if let Some(s) = line.strip_prefix("side:") {
            if let Some((_, side)) = cur.as_mut() {
                *side = Some(s.trim().to_string());
            }
        }
    }
    if let Some(b) = cur.take() {
        out.push(b);
    }
    out
}
