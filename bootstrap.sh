#!/bin/sh
set -e

mkdir -p bin

rustc src/peg/main.rs -o bin/peg
bin/peg src/peg/grammar.rustpeg > src/peg/grammar_new.rs

mv src/peg/grammar.rs src/peg/grammar_old.rs
mv src/peg/grammar_new.rs src/peg/grammar.rs

rustc src/peg/main.rs -o bin/peg
bin/peg src/peg/grammar.rustpeg > src/peg/grammar_new.rs
diff -qs src/peg/grammar.rs src/peg/grammar_new.rs


