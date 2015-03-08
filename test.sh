#!/bin/bash
set -e

RUST_BACKTRACE=1 target/peg examples/tests.rustpeg > examples/test_grammar.rs
rustc --test examples/tests.rs
./tests
rm tests

