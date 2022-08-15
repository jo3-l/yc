use std::path::PathBuf;
use std::{fs, process};

use anyhow::{Context, Result};
use clap::Parser;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use colored::Colorize;
use yc_ast::token::TokenKind;
use yc_diagnostics::Severity;
use yc_parser::lex::Lexer;

#[derive(Debug, Parser)]
pub struct Args {
    /// The files to check.
    pub files: Vec<PathBuf>,
}

#[derive(Debug, Default)]
struct Stats {
    errors: usize,
    warnings: usize,
}

pub fn run(mut args: Args) -> Result<()> {
    let mut files = SimpleFiles::new();
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();

    let mut stats = Stats::default();
    args.files.sort();
    for path in args.files.iter() {
        let content = fs::read_to_string(path)
            .with_context(|| format!("could not read file `{}`", path.display()))?;
        let id = files.add(
            path.file_name().map(|name| name.to_string_lossy()).unwrap(),
            content.clone(),
        );
        let mut lexer = Lexer::new(id, &content);
        lexer
            .tokens()
            .take_while(|token| token.kind != TokenKind::Eof)
            .for_each(drop);
        for diag in lexer.finish().into_iter().map(|diag| diag.build()) {
            match diag.severity {
                Severity::Error => stats.errors += 1,
                Severity::Warning => stats.warnings += 1,
                _ => {}
            }
            term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
        }
    }

    println!(
        "{}: {} errors, {} warnings emitted across {} file(s)",
        "summary".bold(),
        stats.errors,
        stats.warnings,
        args.files.len()
    );
    if stats.errors > 0 || stats.warnings > 0 {
        process::exit(1);
    } else {
        process::exit(0);
    };
}
