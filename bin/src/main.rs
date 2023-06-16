use std::{
    io::{stdin, stdout, Write},
    path::PathBuf,
    println,
};

use anyhow::bail;
use clap::Parser;

use scanner::TokenStream;
use vm::Vm;

#[derive(clap::Parser)]
struct Args {
    file: Option<PathBuf>,
}

fn run_file(path: PathBuf, vm: &mut Vm) -> anyhow::Result<()> {
    run(std::fs::read_to_string(path)?, vm)
}

fn run_prompt(vm: &mut Vm) -> anyhow::Result<()> {
    loop {
        print!("> ");
        stdout().flush()?;
        let mut line = String::new();
        stdin().read_line(&mut line)?;
        match run(line, vm) {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        }
    }
}

fn run(source: String, vm: &mut Vm) -> anyhow::Result<()> {
    vm.run_source(&source)?;
    Ok(())
}

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let args = Args::parse();

    let mut vm = Vm::new();

    match args.file {
        Some(file) => run_file(file, &mut vm),
        None => run_prompt(&mut vm),
    }
}
