use std::{
    io::{stdin, stdout, Write},
    path::PathBuf,
    println,
};

use anyhow::anyhow;
use clap::Parser;

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
    match vm.run_source(&source, &mut stdout()) {
        Ok(()) => Ok(()),
        Err(e) => match e {
            vm::InterpretError::CompileError(_) => Err(e.into()),
            vm::InterpretError::RuntimeError { .. } => Err(anyhow!("{e}\n{}", vm.stack_trace())),
        },
    }
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
