#!/bin/bash
set -e

RUST_BACKTRACE=1 ./peg examples/tests.rustpeg > examples/test_grammar.rs
rustc --test examples/tests.rs -o tests
./tests
rm tests

./peg examples/arithmetic.rustpeg > examples/arithmetic.rs
rustc examples/test_arithmetic.rs -o test_arithmetic
./test_arithmetic
rm test_arithmetic
