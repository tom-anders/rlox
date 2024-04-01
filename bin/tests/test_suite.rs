use std::{collections::BTreeMap, str::Lines};

use itertools::Itertools;
use lazy_regex::regex;
use test_suite_proc_macro::generate_tests;
use vm::Vm;

use pretty_assertions::assert_eq;

pub fn test_runtime_errors_line_by_line(
    lines: Lines<'_>,
    expected_runtime_errors: &BTreeMap<usize, String>,
    expected_output: &[String],
) {
    let mut output = Vec::new();
    let mut vm = Vm::new();

    let mut actual_runtime_errors = BTreeMap::new();
    for (i, line) in lines.enumerate() {
        match vm.run_source(line, &mut output) {
            Ok(()) => (),
            Err(vm::InterpretError::RuntimeError(e)) => {
                actual_runtime_errors.insert(i + 1, e.to_string());
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }

    assert_eq!(actual_runtime_errors, *expected_runtime_errors,
        "Actual runtime errors (left) do not match expected runtime errors (right)");
    assert_eq!(
        String::from_utf8(output).unwrap().lines().collect_vec(),
        expected_output,
        "Actual output (left) does not match expected output (right)"
    );
}

pub fn lox_expect(code: &str) {
    let mut expected_compiler_errors = vec![];
    let mut expected_runtime_errors = BTreeMap::new();
    let mut expected_output = vec![];

    let compiler_error_regex = regex!(r"// (Error( at '.*')?: .*)");
    let runtime_error_regex = regex!(r"// runtime error: (.*)");
    let output_regex = regex!(r"// expect: (.*)");

    for (i, line) in code.lines().enumerate() {
        if let Some(cap) = runtime_error_regex.captures(line) {
            expected_runtime_errors.insert(i + 1, cap[1].to_string());
        } else if let Some(cap) = compiler_error_regex.captures(line) {
            expected_compiler_errors.push(format!("[line {}] {}", i + 1, &cap[1]));
        } else if let Some(cap) = output_regex.captures(line) {
            expected_output.push(cap[1].to_string());
        }
    }

    if !expected_runtime_errors.is_empty() {
        assert!(
            expected_compiler_errors.is_empty(),
            "Can't have a runtime error when there are compiler errors."
        );
    }

    if expected_runtime_errors.len() > 1 {
        test_runtime_errors_line_by_line(code.lines(), &expected_runtime_errors, &expected_output);
        return;
    }

    let mut output = Vec::new();
    match Vm::new().run_source(code, &mut output) {
        Ok(()) => {
            assert_eq!(expected_output, String::from_utf8(output).unwrap().lines().collect_vec());

            assert!(expected_runtime_errors.is_empty(), "Expected runtime error but none occurred");
            assert!(
                expected_compiler_errors.is_empty(),
                "Expected compiler errors but none occured"
            );
        }
        Err(error) => match error {
            vm::InterpretError::CompileError(e) => {
                assert!(expected_output.is_empty());
                assert!(expected_runtime_errors.is_empty());

                assert_eq!(e.to_string(), expected_compiler_errors.join("\n"));
            }
            vm::InterpretError::RuntimeError(runtime_error) => {
                assert_eq!(
                    expected_output,
                    String::from_utf8(output).unwrap().lines().collect_vec()
                );
                assert_eq!(
                    expected_runtime_errors.iter().next().unwrap().1,
                    &runtime_error.to_string()
                );

                assert!(
                    expected_compiler_errors.is_empty(),
                    "Expected compiler errors but none occured"
                );
            }
        },
    }
}

generate_tests!();
