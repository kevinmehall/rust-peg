#![feature(globs)]
#![feature(managed_boxes)]

use std::str;
use std::io::{stdout,stderr};
use std::io::fs::File;
use std::os;
use peg::{compile_grammar};
use codegen::RustWriter;

mod peg;
mod codegen;
mod grammar;

fn main() {
	let args = os::args();
	let source_utf8 = File::open(&Path::new(args[1])).read_to_end().unwrap();
	let source = str::from_utf8(source_utf8).unwrap();
	let grammar_def = grammar::grammar(source);

	match grammar_def {
		Ok(grammar) => {
			let w = RustWriter::new(stdout());
			compile_grammar(&w, grammar);
		}

		Err(msg) => {
			(writeln!(&mut stderr() as &mut Writer, "Error parsing language specification: {}", msg)).unwrap();
			os::set_exit_status(1);
		}
	}
}
