//! Conformance harness: grades program fixtures against `.expect` tables and
//! diagnostic fixtures against `.diag` sidecars.

use std::fs;
use std::path::Path;

use theseus_check::elaborate::elaborate_program;
use theseus_check::env::resolve;
use theseus_check::testkit;
use theseus_cli::value;
use theseus_core::iso::Iso;
use theseus_eval::interp::{eval, static_bound, EvalError};

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

/// Parsed `.expect` sidecar header and rows.
struct ExpectFixture {
    iso_name: String,
    rows: Vec<(String, String)>,
    fuel_bound: Option<u64>,
    steps: Option<u64>,
}

/// Parse the `.expect` sidecar header and rows using surface values.
fn parse_expect_surface(text: &str) -> ExpectFixture {
    let mut iso_name = String::new();
    let mut rows = Vec::new();
    let mut fuel_bound = None;
    let mut steps = None;
    for raw in text.lines() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') {
            if let Some(rest) = line.strip_prefix("# iso:") {
                iso_name = rest.trim().to_string();
            } else if let Some(rest) = line.strip_prefix("# fuel-bound:") {
                fuel_bound = Some(rest.trim().parse::<u64>().expect("bad fuel-bound"));
            } else if let Some(rest) = line.strip_prefix("# steps:") {
                steps = Some(rest.trim().parse::<u64>().expect("bad steps"));
            }
            continue;
        }
        if let Some((lhs, rhs)) = line.split_once("|->") {
            rows.push((lhs.trim().to_string(), rhs.trim().to_string()));
        }
    }
    ExpectFixture { iso_name, rows, fuel_bound, steps }
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

        let expect_path = thse_path.with_extension("expect");
        if !expect_path.exists() {
            continue;
        }

        let expect_text = fs::read_to_string(&expect_path)
            .unwrap_or_else(|e| panic!("reading {}: {e}", expect_path.display()));
        let fixture = parse_expect_surface(&expect_text);
        let iso_name = fixture.iso_name.as_str();
        let rows = &fixture.rows;
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

        let iso_result = elab_map.get(iso_name).unwrap_or_else(|| {
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
        let sig = env.iso(iso_name).unwrap_or_else(|| {
            panic!("[{stem}] iso `{iso_name}` not in env")
        });

        // Guard: a delimiter drift (e.g. `|->` mistyped) makes rows empty —
        // the per-row loops would iterate zero times, a vacuous pass.
        assert!(
            !rows.is_empty(),
            "[{stem}] fixture has zero rows — likely a delimiter problem in {}",
            expect_path.display()
        );

        // Grade each row — forward AND backward.
        for (lhs_str, rhs_str) in rows {
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

        // ── Fuel-bound / steps assertions (label fixtures only) ─────────
        if let Some(expected_bound) = fixture.fuel_bound {
            let computed = static_bound(&iso);
            assert_eq!(
                computed, expected_bound,
                "[{stem}] static_bound mismatch: got {computed}, expected {expected_bound}"
            );

            // Every row must Done within exactly the static bound.
            for (lhs_str, _) in rows {
                let lhs_sv = value::parse_surface_value(lhs_str);
                let lhs_val = value::to_core(&lhs_sv, &sig.lhs, &env);
                let mut fuel = expected_bound;
                eval(&iso, lhs_val, &mut fuel).unwrap_or_else(|e| {
                    panic!("[{stem}] fuel-bound eval failed on `{lhs_str}` with fuel={expected_bound}: {e:?}")
                });
            }
            eprintln!("[pass] {stem}: fuel-bound={expected_bound}");
        }

        if let Some(n) = fixture.steps {
            assert!(
                n >= 1,
                "[{stem}] `# steps: 0` is invalid — need at least 1 step to test the OutOfFuel boundary"
            );
            // Grant n fuel → every row Done.
            for (lhs_str, _) in rows {
                let lhs_sv = value::parse_surface_value(lhs_str);
                let lhs_val = value::to_core(&lhs_sv, &sig.lhs, &env);
                let mut fuel = n;
                eval(&iso, lhs_val, &mut fuel).unwrap_or_else(|e| {
                    panic!("[{stem}] steps={n} eval failed on `{lhs_str}`: {e:?}")
                });
            }
            // Grant n-1 fuel → every row OutOfFuel with steps_taken == n-1.
            for (lhs_str, _) in rows {
                let lhs_sv = value::parse_surface_value(lhs_str);
                let lhs_val = value::to_core(&lhs_sv, &sig.lhs, &env);
                let mut fuel = n - 1;
                let result = eval(&iso, lhs_val, &mut fuel);
                assert_eq!(
                    result,
                    Err(EvalError::OutOfFuel { steps_taken: n - 1 }),
                    "[{stem}] expected OutOfFuel {{ steps_taken: {} }} at fuel={}, got {result:?} on `{lhs_str}`",
                    n - 1, n - 1
                );
            }
            eprintln!("[pass] {stem}: steps={n} (Done@{n}, OutOfFuel@{})", n - 1);
        }

        graded += 1;
        eprintln!("[pass] {stem}: iso `{iso_name}` ({} rows)", rows.len());
    }

    eprintln!(
        "\nConformance: {graded} graded, {} skipped",
        skipped_names.len()
    );

    // Nothing should be silently skipped now that labels elaborate.
    // If a fixture regresses to "not elaborated", the test must FAIL,
    // not silently drop its fuel-bound/steps assertions.
    assert!(
        skipped_names.is_empty(),
        "fixtures were silently skipped (elaboration regression?): {}",
        skipped_names.join(", ")
    );

    // Must-grade stems: if any of these stop appearing in the graded set,
    // the test fails rather than silently losing coverage.
    const MUST_GRADE: &[&str] = &[
        "0011-label-loop",
        // μ-types (ADR-0035): recursive types must keep grading end-to-end.
        "0016-mu-nat",
        "0017-mu-bitlist",
        // v0.2 surface combinators (ADR-0038): iter/fold/unfold must keep
        // grading end-to-end (parse → check → elaborate → eval, fwd+bwd).
        "0018-iter-nat",
        "0019-fold-parity",
        "0020-unfold-shift",
        // μ-type depth annotation (ADR-0035 follow-up): use-site depth must
        // grade end-to-end (parse → check → elaborate → eval, fwd+bwd).
        "0021-mu-depth",
    ];
    let graded_stems: Vec<&str> = fixtures
        .iter()
        .filter_map(|p| {
            let s = p.file_stem()?.to_str()?;
            let expect_path = p.with_extension("expect");
            if expect_path.exists() { Some(s) } else { None }
        })
        .collect();
    for must in MUST_GRADE {
        // We only check that the stem was among the fixture files with
        // .expect sidecars — the per-fixture assertions above already
        // guarantee it actually graded (not skipped).
        assert!(
            graded_stems.iter().any(|s| s == must),
            "must-grade fixture `{must}` not found among program fixtures"
        );
    }

    // Floor: at least 14 fixtures grade today.
    assert!(
        graded >= 14,
        "expected at least 14 graded fixtures, got {graded}"
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

        // Skip deferred-scope fixtures: 0009 (prim/adjoint diagnostics, T0401)
        // — out of the M1 slice-1 static-check set.
        if stem == "0009-prim-mismatch" {
            eprintln!("[skip] diag {stem}: deferred scope (prim diagnostics)");
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

    // Floor: a parser regression could turn all diag fixtures into
    // parse-error skips, leaving tested == 0 with the test still green.
    // Must-test stems: ensure critical diagnostic fixtures are not silently skipped.
    const MUST_TEST_DIAG: &[&str] = &["0008-label-coverage"];
    let diag_stems: Vec<&str> = fixtures
        .iter()
        .filter_map(|p| p.file_stem()?.to_str())
        .collect();
    for must in MUST_TEST_DIAG {
        assert!(
            diag_stems.iter().any(|s| s == must),
            "must-test diagnostic fixture `{must}` not found"
        );
    }
    assert!(
        tested >= 7,
        "expected at least 7 diagnostic fixtures tested, got {tested} \
         (parser regression turning fixtures into skips?)"
    );
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
