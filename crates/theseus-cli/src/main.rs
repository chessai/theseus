//! `theseus` — minimal CLI driver.

use std::fs;
use std::process;

use theseus_cli::lower;
use theseus_cli::pipeline;
use theseus_cli::repl::Repl;
use theseus_cli::value;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: theseus <check|run|repl|lower> ...");
        process::exit(1);
    }

    match args[1].as_str() {
        "check" => cmd_check(&args[2..]),
        "run" => cmd_run(&args[2..]),
        "repl" => cmd_repl(&args[2..]),
        "lower" => cmd_lower(&args[2..]),
        other => {
            eprintln!("unknown subcommand: {other}");
            process::exit(1);
        }
    }
}

fn cmd_check(args: &[String]) {
    if args.is_empty() {
        eprintln!("usage: theseus check <FILE>");
        process::exit(1);
    }
    let src = fs::read_to_string(&args[0]).unwrap_or_else(|e| {
        eprintln!("error reading {}: {e}", args[0]);
        process::exit(1);
    });
    match pipeline::diagnostics(&src) {
        Ok(diags) if diags.is_empty() => println!("ok"),
        Ok(diags) => {
            for d in &diags {
                println!("{d:?}");
            }
            process::exit(1);
        }
        Err(e) => {
            eprintln!("parse error: {e:?}");
            process::exit(1);
        }
    }
}

fn cmd_run(args: &[String]) {
    if args.len() < 3 {
        eprintln!("usage: theseus run <FILE> <iso> <value>");
        process::exit(1);
    }
    let src = fs::read_to_string(&args[0]).unwrap_or_else(|e| {
        eprintln!("error reading {}: {e}", args[0]);
        process::exit(1);
    });
    let iso_name = &args[1];
    let val_str = args[2..].join(" ");

    let ep = match pipeline::elaborate(&src) {
        Ok(ep) => ep,
        Err(pipeline::ElabError::Parse(e)) => {
            eprintln!("parse error: {e:?}");
            process::exit(1);
        }
        Err(pipeline::ElabError::Check(diags)) => {
            for d in &diags {
                eprintln!("{d:?}");
            }
            process::exit(1);
        }
    };

    let sig = pipeline::iso_sig(&ep.env, iso_name).unwrap_or_else(|| {
        eprintln!("unknown iso: {iso_name}");
        process::exit(1);
    });

    let iso = match ep.isos.get(iso_name) {
        Some(Ok(iso)) => iso,
        Some(Err(_)) => {
            eprintln!("iso {iso_name} could not be elaborated");
            process::exit(1);
        }
        None => {
            eprintln!("iso {iso_name} not found in elaboration map");
            process::exit(1);
        }
    };

    let sv = value::parse_surface_value(&val_str);
    let core_val = value::to_core(&sv, &sig.lhs, &ep.env);

    let mut fuel = 1_000_000u64;
    let result = pipeline::run_iso(iso, core_val, &mut fuel).unwrap_or_else(|e| {
        eprintln!("eval error: {e:?}");
        process::exit(1);
    });

    println!("{}", value::render(&result, &sig.rhs, &ep.env));
}

fn cmd_repl(args: &[String]) {
    if args.is_empty() {
        eprintln!("usage: theseus repl <FILE>");
        process::exit(1);
    }
    let src = fs::read_to_string(&args[0]).unwrap_or_else(|e| {
        eprintln!("error reading {}: {e}", args[0]);
        process::exit(1);
    });

    let ep = match pipeline::elaborate(&src) {
        Ok(ep) => ep,
        Err(pipeline::ElabError::Parse(e)) => {
            eprintln!("parse error: {e:?}");
            process::exit(1);
        }
        Err(pipeline::ElabError::Check(diags)) => {
            for d in &diags {
                eprintln!("{d:?}");
            }
            process::exit(1);
        }
    };

    let mut repl = Repl::new(ep);
    let stdin = std::io::stdin();
    let mut input = std::io::BufReader::new(stdin.lock());
    let mut output = std::io::stdout();
    repl.run(&mut input, &mut output);
}

fn cmd_lower(args: &[String]) {
    if args.len() < 2 {
        eprintln!("usage: theseus lower <FILE> <iso>");
        process::exit(1);
    }
    let src = fs::read_to_string(&args[0]).unwrap_or_else(|e| {
        eprintln!("error reading {}: {e}", args[0]);
        process::exit(1);
    });
    let iso_name = &args[1];

    let lowered = match lower::surface_to_cells(&src, iso_name) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("{e}");
            process::exit(1);
        }
    };

    lower::print_summary(&lowered);

    // Run the differential oracle if the domain is small enough.
    let domain_size = theseus_core::enumerate_values(
        &theseus_core::type_of(&lowered.iso)
            .expect("lowered iso must be well-typed")
            .0,
    )
    .len();
    if domain_size <= 4096 {
        match lower::oracle_check(&lowered.iso, &lowered) {
            Ok(n) => println!("oracle: cells agree with eval on {n} inputs"),
            Err(msg) => {
                eprintln!("oracle MISMATCH: {msg}");
                process::exit(1);
            }
        }
    } else {
        println!(
            "oracle: skipped (domain has {domain_size} values, threshold is 4096)"
        );
    }
}
