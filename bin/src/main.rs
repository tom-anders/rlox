use std::{path::PathBuf, fs::File, println, io::{stdin, stdout, Write, self}};

use clap::Parser;

#[derive(clap::Parser)]
struct Args {
    file: Option<PathBuf>,
}

fn run_file(path: PathBuf) -> anyhow::Result<()> {
    run(std::fs::read_to_string(path)?)
}

fn run_prompt() -> anyhow::Result<()> {
    loop {
        print!("> ");
        stdout().flush()?;
        let mut line = String::new();
        stdin().read_line(&mut line)?;
        run(line)?
    }
}

fn run(source: String) -> anyhow::Result<()> {
    let tokens = scanner::scan_tokens(source.chars())?;

    for token in tokens {
        println!("{token:?}");
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
