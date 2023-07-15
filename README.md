[![Build](https://github.com/tom-anders/rlox/actions/workflows/rust.yml/badge.svg?branch=main)](https://github.com/tom-anders/rlox/actions/workflows/rust.yml)

# rlox

An implementation of the [lox programming language](craftinginterpreters.com) written in Rust. 
The 3rd part of the book implements a bytecode VM in C, which the design of my Rust implementation follows rather closely.
However, I tried using idiomatic Rust code wherever possible (e.g. using Result<> for error handling, implementing the Scanner/TokenStream as an iterator, using Rust's enums instead of tagged unions, ...)

## Implementation Status

Currently, the implementation is complete up to section chapter 27 (Classes and Instances).
Chapter 28 is only implemented partially at the moment.

## Building

Nothing special here, simply use `cargo build --release` to build the interpreter/REPL binary.

## Testing

I did not yet integrate the [official test suite](https://github.com/munificent/craftinginterpreters/tree/master/test), but there are unit tests and also some small benchmarks.
You can run them via the usual commands `cargo test` and `cargo bench`.
