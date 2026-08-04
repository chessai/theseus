//! `theseus` -- minimal CLI driver.

use std::process;

use clap::{Parser, Subcommand};

use theseus_cli::lower;
use theseus_cli::pipeline;
use theseus_cli::repl::Repl;
use theseus_cli::util::load_source;
use theseus_cli::value;

/// theseus -- minimal CLI driver.
#[derive(Parser)]
#[command(name = "theseus", version)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Type-check a program and report diagnostics.
    Check {
        /// Path to the source file.
        file: String,
    },
    /// Run an iso on a value.
    Run {
        /// Path to the source file.
        file: String,
        /// Name of the iso to run.
        iso: String,
        /// Value to feed the iso; remaining args are joined by spaces.
        #[arg(required = true, num_args = 1..)]
        value: Vec<String>,
    },
    /// Load a program and start the bidirectional-stepping REPL.
    Repl {
        /// Path to the source file.
        file: String,
    },
    /// Lower an iso to cells and run the differential oracle.
    Lower {
        /// Path to the source file.
        file: String,
        /// Name of the iso to lower.
        iso: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Check { file } => cmd_check(&file),
        Command::Run { file, iso, value } => cmd_run(&file, &iso, &value),
        Command::Repl { file } => cmd_repl(&file),
        Command::Lower { file, iso } => cmd_lower(&file, &iso),
    }
}

fn cmd_check(file: &str) {
    let src = load_source(file);
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

fn cmd_run(file: &str, iso_name: &str, value: &[String]) {
    let src = load_source(file);
    let val_str = value.join(" ");

    let ep = pipeline::elaborate_or_exit(&src);

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

    let sv = value::parse_surface_value(&val_str).unwrap_or_else(|e| {
        eprintln!("error parsing value {val_str:?}: {e}");
        process::exit(1);
    });
    let core_val = value::to_core(&sv, &sig.lhs, &ep.env);

    let mut fuel = 1_000_000u64;
    let result = pipeline::run_iso(iso, core_val, &mut fuel).unwrap_or_else(|e| {
        eprintln!("eval error: {e:?}");
        process::exit(1);
    });

    println!("{}", value::render(&result, &sig.rhs, &ep.env));
}

fn cmd_repl(file: &str) {
    let src = load_source(file);

    let ep = pipeline::elaborate_or_exit(&src);

    let mut repl = Repl::new(ep);
    let stdin = std::io::stdin();
    let mut input = std::io::BufReader::new(stdin.lock());
    let mut output = std::io::stdout();
    repl.run(&mut input, &mut output);
}

fn cmd_lower(file: &str, iso_name: &str) {
    let src = load_source(file);

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
        println!("oracle: skipped (domain has {domain_size} values, threshold is 4096)");
    }
}
