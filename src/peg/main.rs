#[feature(globs)];

use std::str;
use std::rt::io;
use std::rt::io::file;
use std::rt::io::{Reader,Writer};
use std::os;
use peg::{compile_grammar};
use codegen::RustWriter;

mod peg;
mod codegen;
mod grammar;

fn main() {
	let args = os::args();
	let source = str::from_utf8(file::open(&Path::new(args[1]), io::Open, io::Read).read_to_end());
	let grammar_def = grammar::grammar(source).unwrap();
	let w = RustWriter::new(io::stdout());
	compile_grammar(&w, grammar_def);
}
