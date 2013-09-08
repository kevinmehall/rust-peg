#!/bin/bash
set -e

./rustpeg.sh examples/tests.rustpeg > examples/test_grammar.rs
rust test examples/tests.rs

./rustpeg.sh examples/arithmetic.rustpeg > examples/arithmetic.rs
rust run examples/test_arithmetic.rs
