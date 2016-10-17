#!/bin/sh
set -e

git checkout src/grammar.rs
cargo run -- src/grammar.rustpeg > src/grammar_new.rs

mv src/grammar.rs src/grammar_old.rs
cp src/grammar_new.rs src/grammar.rs

if cargo run -- src/grammar.rustpeg > src/grammar_new.rs
then
    diff -qs src/grammar.rs src/grammar_new.rs
else
    echo "Failed"
fi
