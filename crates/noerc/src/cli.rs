use std::{path::PathBuf, process};

use crate::error::{fail, NoerResult};

#[derive(Debug)]
pub struct Cli {
    pub command: Commands,
}

#[derive(Debug)]
pub enum Commands {
    New {
        name: String,
    },
    Build {
        input: PathBuf,
        out_dir: Option<PathBuf>,
    },
    Run {
        input: PathBuf,
    },
    Check {
        input: PathBuf,
    },
    Fmt {
        input: PathBuf,
        check: bool,
    },
}

impl Cli {
    pub fn parse() -> NoerResult<Self> {
        let mut args = std::env::args().skip(1);
        let Some(cmd) = args.next() else {
            return fail(usage());
        };

        if is_help(&cmd) {
            println!("{}", usage());
            process::exit(0);
        }

        let rest: Vec<String> = args.collect();
        let command = match cmd.as_str() {
            "new" => parse_new(&rest)?,
            "build" => parse_build(&rest)?,
            "run" => parse_run(&rest)?,
            "check" => parse_check(&rest)?,
            "fmt" => parse_fmt(&rest)?,
            _ => {
                return fail(format!("unknown command `{}`\n\n{}", cmd, usage()));
            }
        };

        Ok(Self { command })
    }
}

fn parse_new(args: &[String]) -> NoerResult<Commands> {
    if args.len() != 1 || is_help(&args[0]) {
        return fail("usage: noer new <name>");
    }

    Ok(Commands::New {
        name: args[0].clone(),
    })
}

fn parse_build(args: &[String]) -> NoerResult<Commands> {
    let mut input: Option<PathBuf> = None;
    let mut out_dir: Option<PathBuf> = None;

    let mut i = 0;
    while i < args.len() {
        let arg = &args[i];
        match arg.as_str() {
            "-h" | "--help" => {
                println!("usage: noer build [input] [--out-dir <dir>]");
                process::exit(0);
            }
            "-o" | "--out-dir" => {
                i += 1;
                if i >= args.len() {
                    return fail("missing value after --out-dir");
                }
                out_dir = Some(PathBuf::from(&args[i]));
            }
            other => {
                if input.is_none() {
                    input = Some(PathBuf::from(other));
                } else {
                    return fail("too many positional arguments for build");
                }
            }
        }
        i += 1;
    }

    Ok(Commands::Build {
        input: input.unwrap_or_else(|| PathBuf::from("src/main.noer")),
        out_dir,
    })
}

fn parse_run(args: &[String]) -> NoerResult<Commands> {
    if args.iter().any(|a| is_help(a)) {
        println!("usage: noer run [input]");
        process::exit(0);
    }

    if args.len() > 1 {
        return fail("too many positional arguments for run");
    }

    Ok(Commands::Run {
        input: args
            .first()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("src/main.noer")),
    })
}

fn parse_fmt(args: &[String]) -> NoerResult<Commands> {
    let mut input: Option<PathBuf> = None;
    let mut check = false;

    for arg in args {
        match arg.as_str() {
            "-h" | "--help" => {
                println!("usage: noer fmt [input] [--check]");
                process::exit(0);
            }
            "--check" => check = true,
            other => {
                if input.is_none() {
                    input = Some(PathBuf::from(other));
                } else {
                    return fail("too many positional arguments for fmt");
                }
            }
        }
    }

    Ok(Commands::Fmt {
        input: input.unwrap_or_else(|| PathBuf::from("src/main.noer")),
        check,
    })
}

fn parse_check(args: &[String]) -> NoerResult<Commands> {
    if args.iter().any(|a| is_help(a)) {
        println!("usage: noer check [input]");
        process::exit(0);
    }

    if args.len() > 1 {
        return fail("too many positional arguments for check");
    }

    Ok(Commands::Check {
        input: args
            .first()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("src/main.noer")),
    })
}

fn is_help(value: &str) -> bool {
    value == "-h" || value == "--help" || value == "help"
}

fn usage() -> &'static str {
    "Noer compiler CLI (MVP)\n\nUsage:\n  noer new <name>\n  noer build [input] [--out-dir <dir>]\n  noer run [input]\n  noer check [input]\n  noer fmt [input] [--check]\n"
}
