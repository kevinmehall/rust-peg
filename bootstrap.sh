#!/bin/sh
set -e

mkdir -p bin

rustc src/peg.rs -g -o peg
RUST_BACKTRACE=1 ./peg src/grammar.rustpeg > src/grammar_new.rs

mv src/grammar.rs src/grammar_old.rs
mv src/grammar_new.rs src/grammar.rs

rustc src/peg.rs -o peg
./peg src/grammar.rustpeg > src/grammar_new.rs
diff -qs src/grammar.rs src/grammar_new.rs


