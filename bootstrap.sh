#!/bin/sh
set -e

mkdir -p target

rustc src/peg.rs -g -o target/peg
RUST_BACKTRACE=1 target/peg src/grammar.rustpeg > src/grammar_new.rs

mv src/grammar.rs src/grammar_old.rs
mv src/grammar_new.rs src/grammar.rs

if rustc src/peg.rs -o target/peg
then
    target/peg src/grammar.rustpeg > src/grammar_new.rs
    diff -qs src/grammar.rs src/grammar_new.rs
else
    mv src/grammar_old.rs src/grammar.rs
fi
