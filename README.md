[![Build](https://github.com/tom-anders/rlox/actions/workflows/rust.yml/badge.svg?branch=main)](https://github.com/tom-anders/rlox/actions/workflows/rust.yml)

# rlox

A full implementation of the [lox programming language](craftinginterpreters.com) written in Rust. The 3rd part of the
book implements a bytecode VM in C, which the design of my Rust implementation follows rather closely. However, I tried
using idiomatic Rust code wherever possible (e.g. using Result<> for error handling, implementing the
Scanner/TokenStream as an iterator, using Rust's enums instead of tagged unions, ...)

## Building

Nothing special here, simply use `cargo build --release` to build the interpreter/REPL binary.

## Testing

Rlox currently passes all function tests of the  [official test
suite](https://github.com/munificent/craftinginterpreters/tree/master/test). There are a few tests that fail because
rlox will report different errors than clox, namely `function/body_must_be_block.lox`,
`function/missing_comma_in_parameters.lox` and `class/local_inherit_self.lox`. This is caused by slight differences in
the way I implemented parser synchronization when encountering an error.

There are also some unit tests and benchmark, they can be run via the usual `cargo test` and `cargo bench` commands.
