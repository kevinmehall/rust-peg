#!/bin/sh
set -e

cargo run -- peg-codegen/grammar.rustpeg > peg-codegen/grammar_new.rs

mv peg-codegen/grammar.rs peg-codegen/grammar_old.rs
cp peg-codegen/grammar_new.rs peg-codegen/grammar.rs

if cargo run -- peg-codegen/grammar.rustpeg > peg-codegen/grammar_new.rs
then
    diff -qs peg-codegen/grammar.rs peg-codegen/grammar_new.rs
else
    echo "Failed"
fi
