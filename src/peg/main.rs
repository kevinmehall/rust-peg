#[feature(globs)];
#[feature(managed_boxes)];

use std::str;
use std::io::stdout;
use std::io::fs::File;
use std::os;
use peg::{compile_grammar};
use codegen::RustWriter;

mod peg;
mod codegen;
mod grammar;

fn main() {
	let args = os::args();
	let source_utf8 = File::open(&Path::new(args[1])).read_to_end();
	let source = str::from_utf8(source_utf8);
	let grammar_def = grammar::grammar(source).unwrap();
	let w = RustWriter::new(stdout());
	compile_grammar(&w, grammar_def);
}
