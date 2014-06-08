#![feature(globs)]
#![feature(managed_boxes)]
#![feature(quote)]

extern crate syntax;

use std::str;
use std::io::{stdout,stderr};
use std::io::fs::File;
use std::os;
use peg::{compile_grammar};

mod peg;
mod grammar;
mod rustast;

fn main() {
	let args = os::args();
	let source_utf8 = File::open(&Path::new(args.get(1).as_slice())).read_to_end().unwrap();
	let source = str::from_utf8(source_utf8.as_slice()).unwrap();
	let grammar_def = grammar::grammar(source);

	match grammar_def {
		Ok(grammar) => {
			rustast::with_fake_extctxt(|e| {

				let ast = compile_grammar(e, &grammar);
				let mut out = stdout();

				peg::write_header(&mut out, &grammar).unwrap();

				for item in ast.items.iter() {
					out.write_line(rustast::item_to_str(&**item).as_slice()).unwrap();
				}
			})
		}

		Err(msg) => {
			(writeln!(&mut stderr() as &mut Writer, "Error parsing language specification: {}", msg)).unwrap();
			os::set_exit_status(1);
		}
	}
}
