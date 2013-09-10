use std::io;
use peg::*;
use codegen::RustWriter;
mod peg;
mod codegen;

fn main() {
	let grammar = include!("grammar_def.rs");
	let w = RustWriter::new(io::stdout());
	compile_grammar(&w, grammar);
}