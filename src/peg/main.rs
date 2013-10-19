#[feature(globs)];

use std::os;
use std::io;
use peg::{compile_grammar};
use codegen::RustWriter;

mod peg;
mod codegen;
mod grammar;

fn main() {
	let args = os::args();
	let filename = args[1];
	let file = io::read_whole_file_str(&Path::new(filename)).expect("Error reading file");
	let grammar_def = grammar::grammar(file).unwrap();
	let w = RustWriter::new(io::stdout());
	compile_grammar(&w, grammar_def);
}
