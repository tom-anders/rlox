use std::{path::PathBuf, println, io::{stdin, stdout, Write}};

use clap::Parser;
use interpreter::Interpreter;

#[derive(clap::Parser)]
struct Args {
    file: Option<PathBuf>,
}

static INTERPRETER: Interpreter = Interpreter {};

fn run_file(path: PathBuf) -> anyhow::Result<()> {
    run(std::fs::read_to_string(path)?)
}

fn run_prompt() -> anyhow::Result<()> {
    loop {
        print!("> ");
        stdout().flush()?;
        let mut line = String::new();
        stdin().read_line(&mut line)?;
        match run(line) {
            Ok(_) => (),
            Err(e) => println!("error: {}", e),
        }
    }
}

fn run(source: String) -> anyhow::Result<()> {
    let scanner = scanner::Scanner::new(&source);
    let tokens = scanner.scan_tokens()?;

    let parser = parser::Parser::new(&tokens);

    match parser.parse() {
        Ok(expr) => println!("{}", INTERPRETER.interpret(&expr)?),
        Err(errors) => {
            for error in errors.iter() {
                println!("error: {}", error);
            }
        }
    }

    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    match args.file {
        Some(file) => run_file(file),
        None => run_prompt(),
    }
}
