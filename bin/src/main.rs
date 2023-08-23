use std::{
    io::{stdin, stdout, Write},
    path::PathBuf,
    println,
};

use clap::Parser;

use compiler::CompilerErrors;
use vm::Vm;

#[derive(Debug, thiserror::Error)]
enum VmError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    CompilerErrors(#[from] CompilerErrors),
    #[error("{error}\n{stack_trace}")]
    RuntimeError { error: vm::InterpretError, stack_trace: String },
}

#[derive(clap::Parser)]
struct Args {
    file: Option<PathBuf>,
}

fn run_file(path: PathBuf, vm: &mut Vm) -> Result<(), VmError> {
    run(std::fs::read_to_string(path)?, vm)
}

fn run_prompt(vm: &mut Vm) -> Result<(), VmError> {
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

fn run(source: String, vm: &mut Vm) -> Result<(), VmError> {
    match vm.run_source(&source, &mut stdout()) {
        Ok(()) => Ok(()),
        Err(error) => match error {
            vm::InterpretError::CompileError(e) => Err(VmError::CompilerErrors(e)),
            vm::InterpretError::RuntimeError { .. } => {
                Err(VmError::RuntimeError { error, stack_trace: vm.stack_trace() })
            }
        },
    }
}

fn main() {
    env_logger::init();
    let args = Args::parse();

    let mut vm = Vm::new();

    let run_result = match args.file {
        Some(file) => run_file(file, &mut vm),
        None => run_prompt(&mut vm),
    };

    if let Err(e) = run_result {
        eprintln!("{}", e);

        std::process::exit(match e {
            VmError::IoError(_) => 74,
            VmError::CompilerErrors(_) => 65,
            VmError::RuntimeError { .. } => 70,
        });
    }
}
