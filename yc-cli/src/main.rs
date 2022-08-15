use std::process::ExitCode;

use clap::{Command, CommandFactory, ErrorKind, Parser, Subcommand};
use colored::Colorize;
use wild;

mod check;

#[derive(Debug, Parser)]
#[clap(name = "yc", about = "Tools for YAGPDB-flavored Go templates", version = clap::crate_version!())]
#[clap(subcommand_required = true)]
#[clap(arg_required_else_help = true)]
pub struct Args {
    #[clap(subcommand)]
    pub cmd: Subcommands,
}

#[derive(Debug, Subcommand)]
pub enum Subcommands {
    /// Check files for syntax errors
    Check(check::Args),
}

fn main() -> ExitCode {
    let args = Args::parse_from(wild::args());
    let result = match args.cmd {
        Subcommands::Check(check_args) => check::run(check_args),
    };

    if let Err(err) = result {
        eprintln!("{}: {}", "error".bright_red().bold(), err);
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
