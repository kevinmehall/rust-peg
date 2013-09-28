#!/bin/bash
set -e

bin/peg examples/tests.rustpeg > examples/test_grammar.rs
rust test examples/tests.rs

bin/peg examples/arithmetic.rustpeg > examples/arithmetic.rs
rust run examples/test_arithmetic.rs
