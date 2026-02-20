mod ast;
mod build;
mod cli;
mod error;
mod formatter;
mod lexer;
mod parser;
mod project;
mod sema;
mod transpiler;

use crate::{
    build::{build_program, check_program, run_program},
    cli::{Cli, Commands},
    error::NoerResult,
    formatter::format_file,
    project::create_new_project,
};

fn main() -> NoerResult<()> {
    let cli = Cli::parse()?;

    match cli.command {
        Commands::New { name } => create_new_project(&name),
        Commands::Build { input, out_dir } => {
            build_program(&input, out_dir.as_deref())?;
            Ok(())
        }
        Commands::Run { input } => run_program(&input),
        Commands::Check { input } => check_program(&input),
        Commands::Fmt { input, check } => format_file(&input, check),
    }
}
