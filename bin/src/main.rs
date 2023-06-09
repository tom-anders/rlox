use std::{path::PathBuf, println, io::{stdin, stdout, Write}};

use anyhow::{bail};
use clap::Parser;

use interpreter::Interpreter;


#[derive(clap::Parser)]
struct Args {
    file: Option<PathBuf>,
}

fn run_file(path: PathBuf, interpreter: &mut Interpreter) -> anyhow::Result<()> {
    run(std::fs::read_to_string(path)?, interpreter)
}

fn run_prompt(interpreter: &mut Interpreter) -> anyhow::Result<()> {
    loop {
        print!("> ");
        stdout().flush()?;
        let mut line = String::new();
        stdin().read_line(&mut line)?;
        match run(line, interpreter) {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        }
    }
}

fn run(source: String, interpreter: &mut Interpreter) -> anyhow::Result<()> {
    let tokens = scanner::scan_tokens(&source)?;

    let parser = parser::Parser::new(&tokens);

    match parser.parse() {
        Ok(stmts) => {
            interpreter.interpret(&stmts)?;
        }
        Err(errors) => {
            println!("{}", errors);
            bail!("{} error(s) in parsing", errors.0.len());
        }
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut interpreter = Interpreter::default();

    match args.file {
        Some(file) => run_file(file, &mut interpreter),
        None => run_prompt(&mut interpreter),
    }
}
